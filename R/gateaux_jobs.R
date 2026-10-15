# Helpers for the v2 job API. None of these have a legacy equivalent that
# works with an API key, so they are v2 only.

#' List jobs
#'
#' With a `report_name`, lists that report's jobs. With `report_name = NULL`,
#' lists jobs across every report you can see, which is how to find, for
#' example, everything currently running.
#'
#' @param report_name The report, or `NULL` for all reports.
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
gateaux_jobs <- function(report_name = NULL, JWT, server = "gateaux.io", status = NULL, tag = NULL,
                         tags = NULL, filter = NULL, page = 0, perpage = 100, sort = "-received_at") {
  query <- list(page = page, perpage = perpage, sort = sort)
  if (!is.null(status)) query$status <- paste(status, collapse = ",")
  if (!is.null(filter)) query$filter <- filter
  if (!is.null(tag)) tags <- c(tags, list(tag = tag))
  for (k in names(tags)) query[[paste0("tag.", k)]] <- tags[[k]]
  path <- if (is.null(report_name)) "/api/v2/jobs" else sprintf("/api/v2/reports/%s/jobs", report_name)
  res <- gateaux_request("GET", path, JWT, server, query = query)
  out <- gateaux_jobs_frame(res$results)
  attr(out, "total") <- res$total
  out
}

#' Read one job
#'
#' Job ids are global, so none of the single-job helpers need the report.
#'
#' @inheritParams gateaux_jobs
#' @param job_id The job id.
#' @param wait Optional number of seconds to hold the request open waiting for the job to change (max 60).
#' @return The job as a list (the parsed API record); its `report` field names the report.
#' @export
gateaux_job <- function(job_id, JWT, server = "gateaux.io", wait = NULL) {
  query <- if (!is.null(wait)) list(wait = wait) else NULL
  gateaux_request("GET", sprintf("/api/v2/jobs/%s", job_id), JWT, server, query = query)
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
gateaux_wait_for_jobs <- function(job_ids, JWT, server = "gateaux.io",
                                  timeout = Inf, poll = 30, quiet = FALSE) {
  terminal <- c("Success", "Failed", "Invalid", "Cancelled")
  start <- Sys.time()
  pending <- as.list(job_ids)
  done <- list()
  last <- list()
  while (length(pending) > 0) {
    for (jid in pending) {
      job <- gateaux_job(jid, JWT, server, wait = poll)
      key <- as.character(jid)
      if (!quiet && !identical(last[[key]], job$status)) {
        message(sprintf("%s %s job %s: %s", format(Sys.time(), "%H:%M:%S"), job$report, jid, job$status))
        last[[key]] <- job$status
      }
      if (job$status %in% terminal) done[[key]] <- job
    }
    pending <- Filter(function(jid) is.null(done[[as.character(jid)]]), pending)
    if (length(pending) > 0 && as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout) {
      warning(sprintf("Timed out waiting for %s", paste(unlist(pending), collapse = ", ")), call. = FALSE)
      for (jid in pending) done[[as.character(jid)]] <- gateaux_job(jid, JWT, server)
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
gateaux_cancel_job <- function(job_id, JWT, server = "gateaux.io") {
  gateaux_request("DELETE", sprintf("/api/v2/jobs/%s", job_id), JWT, server)
}

#' Fetch a job's log
#'
#' Returns the log so far for a finished job. For a running job the request
#' streams until the job stops, so this returns the complete log.
#'
#' @inheritParams gateaux_job
#' @return A character vector of log lines.
#' @export
gateaux_job_log <- function(job_id, JWT, server = "gateaux.io") {
  resp <- httr::GET(gateaux_url(server, sprintf("/api/v2/jobs/%s/log", job_id)),
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
gateaux_job_outputs <- function(job_id, JWT, server = "gateaux.io") {
  res <- gateaux_request("GET", sprintf("/api/v2/jobs/%s/outputs", job_id), JWT, server)
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
#' @param report_name The report (for `gateaux_report_dag()`).
#' @param direction `"both"` (default), `"up"` or `"down"`.
#' @param depth `"full"` (default) or a positive integer.
#' @return A list with `nodes`, `edges` and `truncated`.
#' @export
gateaux_job_dag <- function(job_id, JWT, server = "gateaux.io", direction = "both", depth = "full") {
  gateaux_request("GET", sprintf("/api/v2/jobs/%s/dag", job_id), JWT, server,
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
gateaux_stream_log <- function(job_id, JWT, server = "gateaux.io",
                               callback = function(line) cat(line$text, "\n", sep = "")) {
  url <- gateaux_url(server, sprintf("/api/v2/jobs/%s/log", job_id))
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

#' List a report's service-account tokens
#'
#' Every token minted for the report, newest first, including expired and
#' revoked ones. The tokens themselves are never shown again; use the `id`
#' with [gateaux_revoke_token()] and [gateaux_rotate_token()].
#'
#' @inheritParams gateaux_mint_token
#' @return A data frame with one row per token: `id`, `name`, `status`
#'   (`"active"`, `"expired"` or `"revoked"`), `scopes` (comma-separated),
#'   `minted_by` (GitHub login), `created_at`, `expires_at`, `revoked_at`,
#'   `revoked_by` and `replaced_by` (the id of the token a rotation minted in
#'   its place).
#' @export
gateaux_tokens <- function(report_name, JWT, server = "gateaux.io") {
  res <- gateaux_request("GET", sprintf("/api/v2/reports/%s/tokens", report_name), JWT, server)
  dplyr::bind_rows(lapply(res, function(t) dplyr::tibble(
    id = t$id, name = scalar_or_na(t$name), status = t$status,
    scopes = paste(unlist(t$scopes), collapse = ","),
    minted_by = scalar_or_na(t$minted_by$github_login),
    created_at = t$created_at, expires_at = t$expires_at,
    revoked_at = scalar_or_na(t$revoked_at),
    revoked_by = scalar_or_na(t$revoked_by$github_login),
    replaced_by = scalar_or_na(t$replaced_by))))
}

#' Revoke a service-account token
#'
#' The token stops working immediately. Revoking an already revoked token is
#' harmless and returns its current state.
#'
#' @inheritParams gateaux_mint_token
#' @param token_id The token's `id`, as returned by [gateaux_mint_token()]
#'   (in `attr(, "details")$id`) or listed by [gateaux_tokens()].
#' @return The token's record as a list (`id`, `name`, `status`, `scopes`,
#'   `revoked_at`, `revoked_by`, ...), invisibly.
#' @export
gateaux_revoke_token <- function(report_name, token_id, JWT, server = "gateaux.io") {
  invisible(gateaux_request("DELETE", sprintf("/api/v2/reports/%s/tokens/%s", report_name, token_id), JWT, server))
}

#' Rotate a service-account token
#'
#' Mints a replacement with the same name and scopes and revokes the old
#' token, in one step. The old token keeps working for `grace_seconds` so a
#' run in flight can finish while the new secret is being stored.
#'
#' @inheritParams gateaux_revoke_token
#' @param expires_in_days Lifetime of the new token in days, from now. Default:
#'   the old token's original lifetime.
#' @param grace_seconds How long the old token keeps working after rotation
#'   (default 0, maximum 3600).
#' @return The new token as a character string, shown once, with its details
#'   (`id`, `replaces`, `scopes`, `expires_at`, ...) in `attr(, "details")`,
#'   exactly as [gateaux_mint_token()] returns.
#' @export
gateaux_rotate_token <- function(report_name, token_id, JWT, server = "gateaux.io",
                                 expires_in_days = NULL, grace_seconds = NULL) {
  body <- list()
  if (!is.null(expires_in_days)) body$expires_in_days <- as.integer(expires_in_days)
  if (!is.null(grace_seconds)) body$grace_seconds <- as.integer(grace_seconds)
  if (length(body) == 0) body <- structure(list(), names = character())
  res <- gateaux_request("POST", sprintf("/api/v2/reports/%s/tokens/%s/rotate", report_name, token_id), JWT, server, body = body)
  structure(res$token, details = res[setdiff(names(res), "token")])
}

#' Who am I?
#'
#' Returns the user behind a token, what kind of token it is, and for a
#' service token its report and scopes.
#'
#' @inheritParams gateaux_jobs
#' @return A list: `user` (`id`, `github_login`, `name`), `principal` (`"user"`,
#'   `"service"` or `"report"`), `report` and `scopes` (for service and report
#'   tokens, else `NULL`), and `token` with `token_expires_at` (a fresh API key,
#'   only when called from a browser session; `NULL` otherwise).
#' @export
gateaux_session <- function(JWT, server = "gateaux.io") {
  gateaux_request("GET", "/api/v2/auth/session", JWT, server)
}

#' Set tags or metadata on a job
#'
#' `tags` replaces the job's tag map (a flat named list of strings);
#' `metadata` is any JSON-encodable list. Give one or both.
#'
#' @inheritParams gateaux_job
#' @param tags Named list or character vector of tags.
#' @param metadata A list, encoded as JSON.
#' @return The updated job as a list.
#' @export
gateaux_annotate_job <- function(job_id, JWT, server = "gateaux.io", tags = NULL, metadata = NULL) {
  body <- list()
  if (!is.null(tags)) body$tags <- as.list(tags)
  if (!is.null(metadata)) body$metadata <- metadata
  if (length(body) == 0) stop("Give tags and/or metadata")
  gateaux_request("PATCH", sprintf("/api/v2/jobs/%s", job_id), JWT, server, body = body)
}
