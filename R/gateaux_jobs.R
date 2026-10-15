# Helpers for the v2 job API. None of these have a legacy equivalent that
# works with an API key, so they are v2 only.

#' List a report's jobs
#'
#' @param report_name The report.
#' @param JWT Authentication token.
#' @param server The gateaux server; see [gateaux_job_runner()].
#' @param status Optional vector of statuses to keep, e.g. `c("Running", "Pending")`.
#' @param tag Optional tag (as given to [gateaux_job_runner()]) to filter on.
#' @param tags Optional named list of tag filters, e.g. `list(batch = "x")`.
#' @param filter Optional free-text search over the job details.
#' @param page Zero-based page number.
#' @param perpage Results per page (max 1000).
#' @param sort `"id"`, `"received_at"`, `"started_at"` or `"finished_at"`, with a leading `-` for descending.
#' @return A data frame; see [gateaux_jobs_frame()]. The total number of matching jobs is in `attr(, "total")`.
#' @export
gateaux_jobs <- function(report_name, JWT, server = "gateaux.io", status = NULL, tag = NULL,
                         tags = NULL, filter = NULL, page = 0, perpage = 100, sort = "-received_at") {
  query <- list(page = page, perpage = perpage, sort = sort)
  if (!is.null(status)) query$status <- paste(status, collapse = ",")
  if (!is.null(filter)) query$filter <- filter
  if (!is.null(tag)) tags <- c(tags, list(tag = tag))
  for (k in names(tags)) query[[paste0("tag.", k)]] <- tags[[k]]
  res <- gateaux_request("GET", sprintf("/api/v2/reports/%s/jobs", report_name), JWT, server, query = query)
  out <- gateaux_jobs_frame(res$results)
  attr(out, "total") <- res$total
  out
}

#' Read one job
#'
#' @inheritParams gateaux_jobs
#' @param job_id The job id.
#' @param wait Optional number of seconds to hold the request open waiting for the job to change (max 60).
#' @return The job as a list (the parsed API record).
#' @export
gateaux_job <- function(report_name, job_id, JWT, server = "gateaux.io", wait = NULL) {
  query <- if (!is.null(wait)) list(wait = wait) else NULL
  gateaux_request("GET", sprintf("/api/v2/reports/%s/jobs/%s", report_name, job_id), JWT, server, query = query)
}

#' Wait for jobs to finish
#'
#' Long-polls each job until it reaches a terminal state (Success, Failed,
#' Invalid or Cancelled) or `timeout` seconds have passed.
#'
#' @inheritParams gateaux_jobs
#' @param job_ids Job ids, e.g. the `id` column from [gateaux_job_runner()].
#' @param timeout Give up after this many seconds (default: never).
#' @param poll Seconds per long-poll request (max 60).
#' @param quiet If `FALSE` (default) print each state change.
#' @return A data frame of the jobs' final states; see [gateaux_jobs_frame()].
#' @export
gateaux_wait_for_jobs <- function(report_name, job_ids, JWT, server = "gateaux.io",
                                  timeout = Inf, poll = 30, quiet = FALSE) {
  terminal <- c("Success", "Failed", "Invalid", "Cancelled")
  start <- Sys.time()
  pending <- as.list(job_ids)
  done <- list()
  last <- list()
  while (length(pending) > 0) {
    for (jid in pending) {
      job <- gateaux_job(report_name, jid, JWT, server, wait = poll)
      key <- as.character(jid)
      if (!quiet && !identical(last[[key]], job$status)) {
        message(sprintf("%s %s job %s: %s", format(Sys.time(), "%H:%M:%S"), report_name, jid, job$status))
        last[[key]] <- job$status
      }
      if (job$status %in% terminal) done[[key]] <- job
    }
    pending <- Filter(function(jid) is.null(done[[as.character(jid)]]), pending)
    if (length(pending) > 0 && as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) {
      warning(sprintf("Timed out waiting for %s", paste(unlist(pending), collapse = ", ")), call. = FALSE)
      for (jid in pending) done[[as.character(jid)]] <- gateaux_job(report_name, jid, JWT, server)
      break
    }
  }
  gateaux_jobs_frame(unname(done[as.character(job_ids)]))
}

#' Cancel a job
#'
#' Idempotent: cancelling a finished job returns its current state.
#'
#' @inheritParams gateaux_job
#' @return The job as a list.
#' @export
gateaux_cancel_job <- function(report_name, job_id, JWT, server = "gateaux.io") {
  gateaux_request("DELETE", sprintf("/api/v2/reports/%s/jobs/%s", report_name, job_id), JWT, server)
}

#' Fetch a job's log
#'
#' Returns the log so far for a finished job. For a running job the request
#' streams until the job stops, so this returns the complete log.
#'
#' @inheritParams gateaux_job
#' @return A character vector of log lines.
#' @export
gateaux_job_log <- function(report_name, job_id, JWT, server = "gateaux.io") {
  resp <- httr::GET(gateaux_url(server, sprintf("/api/v2/reports/%s/jobs/%s/log", report_name, job_id)),
                    httr::add_headers(Authorization = paste("Bearer", JWT), Accept = "text/plain"))
  gateaux_check(resp)
  text <- httr::content(resp, as = "text", encoding = "UTF-8")
  strsplit(sub("\n$", "", text), "\n", fixed = TRUE)[[1]]
}

#' List a job's output files
#'
#' @inheritParams gateaux_job
#' @return A data frame with `name`, `size`, `modified`, `mime`, `checksum` and `url` (server-relative).
#' @export
gateaux_job_outputs <- function(report_name, job_id, JWT, server = "gateaux.io") {
  res <- gateaux_request("GET", sprintf("/api/v2/reports/%s/jobs/%s/outputs", report_name, job_id), JWT, server)
  scalar <- function(x) if (is.null(x)) NA else x
  if (length(res$files) == 0) {
    return(data.frame(name = character(), size = numeric(), modified = character(), mime = character(),
                      checksum = character(), url = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(res$files, function(f) data.frame(
    name = f$name, size = as.numeric(f$size), modified = f$modified, mime = f$mime,
    checksum = scalar(f$checksum), url = f$url, stringsAsFactors = FALSE)))
}

#' Fetch the dependency graph around a job or a report
#'
#' `gateaux_job_dag()` returns the realised graph of jobs whose outputs fed
#' this job (and jobs it fed); `gateaux_report_dag()` returns the declared
#' pipeline (`requires`, `wants`, `triggers`) around a report.
#'
#' @inheritParams gateaux_job
#' @param direction `"both"` (default), `"up"` or `"down"`.
#' @param depth `"full"` (default) or a positive integer.
#' @return A list with `nodes`, `edges` and `truncated`.
#' @export
gateaux_job_dag <- function(report_name, job_id, JWT, server = "gateaux.io", direction = "both", depth = "full") {
  gateaux_request("GET", sprintf("/api/v2/reports/%s/jobs/%s/dag", report_name, job_id), JWT, server,
                  query = list(direction = direction, depth = depth))
}

#' @rdname gateaux_job_dag
#' @export
gateaux_report_dag <- function(report_name, JWT, server = "gateaux.io", direction = "both", depth = "full") {
  gateaux_request("GET", sprintf("/api/v2/reports/%s/dag", report_name), JWT, server,
                  query = list(direction = direction, depth = depth))
}

#' Stream a job's log as it is written
#'
#' Follows the log of a running (or pending) job, calling `callback` with each
#' line as it arrives, and returns when the job stops. For a finished job the
#' stored log is replayed. The default callback prints each line.
#'
#' @inheritParams gateaux_job
#' @param callback A function of one argument, called with each log line as a
#'   list with `text`, `timestamp` (milliseconds since the epoch, may be `NULL`)
#'   and `beginsSection`.
#' @return All lines received, as a character vector, invisibly.
#' @export
gateaux_stream_log <- function(report_name, job_id, JWT, server = "gateaux.io",
                               callback = function(line) cat(line$text, "\n", sep = "")) {
  url <- gateaux_url(server, sprintf("/api/v2/reports/%s/jobs/%s/log", report_name, job_id))
  seen <- character()
  buffer <- ""
  handle_event <- function(event) {
    lines <- strsplit(event, "\n", fixed = TRUE)[[1]]
    type <- sub("^event: ", "", grep("^event: ", lines, value = TRUE))
    data <- sub("^data: ", "", grep("^data: ", lines, value = TRUE))
    if (length(type) == 1 && type == "log" && length(data) == 1) {
      line <- jsonlite::fromJSON(data, simplifyVector = FALSE)
      seen <<- c(seen, line$text)
      callback(line)
    }
  }
  resp <- httr::GET(url,
    httr::add_headers(Authorization = paste("Bearer", JWT), Accept = "text/event-stream"),
    httr::write_stream(function(chunk) {
      buffer <<- paste0(buffer, rawToChar(chunk))
      # Events are separated by a blank line; keep any partial event in the buffer
      parts <- strsplit(buffer, "\n\n", fixed = TRUE)[[1]]
      complete <- if (endsWith(buffer, "\n\n")) parts else utils::head(parts, -1)
      buffer <<- if (endsWith(buffer, "\n\n")) "" else parts[length(parts)]
      for (event in complete) handle_event(event)
      TRUE
    }))
  gateaux_check(resp)
  invisible(seen)
}

#' Mint a service-account token for a report
#'
#' A service-account token acts as you, limited to one report and the given
#' scopes, and is what to put in CI rather than your personal API key. It is
#' returned once and not stored by the server; it expires after
#' `expires_in_days` and stops working if you lose access to the report.
#'
#' @inheritParams gateaux_jobs
#' @param name A label for the token (recorded in the token).
#' @param scopes Character vector from `jobs:submit`, `jobs:read`,
#'   `jobs:cancel`, `logs:read`, `outputs:read`. Default: all of them.
#' @param expires_in_days Lifetime, 1 to 365 (default 30).
#' @return The token string, with the server's record (name, scopes, expiry) in `attr(, "details")`.
#' @export
gateaux_mint_token <- function(report_name, JWT, server = "gateaux.io", name = NULL,
                               scopes = NULL, expires_in_days = NULL) {
  body <- list()
  if (!is.null(name)) body$name <- name
  if (!is.null(scopes)) body$scopes <- I(as.character(scopes))
  if (!is.null(expires_in_days)) body$expires_in_days <- as.integer(expires_in_days)
  if (length(body) == 0) body <- structure(list(), names = character())
  res <- gateaux_request("POST", sprintf("/api/v2/reports/%s/tokens", report_name), JWT, server, body = body)
  structure(res$token, details = res[setdiff(names(res), "token")])
}
