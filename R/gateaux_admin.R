# Runners, repositories, teams, thumbnails and streams: the supporting routes
# (v2 only; all but the streams need a user key, not a service token).

scalar_or_na <- function(x) if (is.null(x) || length(x) == 0) NA else x

#' Connected runners and their queues
#'
#' `gateaux_runners()` lists the runners currently connected to the server
#' and what each is running. `gateaux_queues()` lists the job queues: one per
#' runner key of your teams plus the default queue, with how many jobs are
#' waiting and the state of the queue's hook (the script that starts runners
#' on demand), if it has one.
#'
#' @inheritParams gateaux_jobs
#' @return `gateaux_runners()`: a data frame with one row per runner (`node`,
#'   `last_seen`, `last_used`, `cpus`, `gpus`, `memory`, `key_name`,
#'   `running`, the number of jobs) and the running jobs as a data frame in
#'   `attr(, "running")`. `gateaux_queues()`: a data frame with `name`,
#'   `length` and the hook's `started`, `exited` and `exit_code`.
#' @export
gateaux_runners <- function(JWT, server = "gateaux.io") {
  res <- gateaux_request("GET", "/api/v2/runners", JWT, server)
  runners <- dplyr::bind_rows(lapply(res, function(r) dplyr::tibble(
    node = r$node, last_seen = r$last_seen, last_used = scalar_or_na(r$last_used),
    cpus = r$cpus, gpus = r$gpus, memory = r$memory, key_name = scalar_or_na(r$key_name),
    running = length(r$running))))
  running <- dplyr::bind_rows(lapply(res, function(r) dplyr::bind_rows(lapply(r$running, function(j)
    dplyr::tibble(node = r$node, job_id = as.integer(j$job_id), cpus = j$cpus, gpus = j$gpus, memory = j$memory,
                  inputs = length(j$inputs))))))
  attr(runners, "running") <- running
  runners
}

#' @rdname gateaux_runners
#' @export
gateaux_queues <- function(JWT, server = "gateaux.io") {
  res <- gateaux_request("GET", "/api/v2/runners/queues", JWT, server)
  dplyr::bind_rows(lapply(res, function(q) dplyr::tibble(
    name = q$name, length = q$length,
    started = scalar_or_na(q$hook$started), exited = scalar_or_na(q$hook$exited),
    exit_code = scalar_or_na(q$hook$exit_code))))
}

#' Runner keys
#'
#' A runner key lets runners registered with it serve a team's reports.
#' `gateaux_runner_keys()` lists the keys of your teams; `gateaux_add_runner_key()`
#' registers a new RSA public key for one of your teams.
#'
#' @inheritParams gateaux_jobs
#' @param name A label for the key.
#' @param team The team id (see [gateaux_teams()]).
#' @param public_key The PEM-encoded RSA public key, or the path to a file holding it.
#' @return A data frame of your runner keys: `id`, `name`, `team`, `max_runners`.
#' @export
gateaux_runner_keys <- function(JWT, server = "gateaux.io") {
  keys_frame(gateaux_request("GET", "/api/v2/runners/keys", JWT, server))
}

#' @rdname gateaux_runner_keys
#' @export
gateaux_add_runner_key <- function(name, team, public_key, JWT, server = "gateaux.io") {
  if (length(public_key) == 1 && file.exists(public_key)) public_key <- paste(readLines(public_key, warn = FALSE), collapse = "\n")
  keys_frame(gateaux_request("POST", "/api/v2/runners/keys", JWT, server,
                             body = list(name = name, team = as.integer(team), public_key = public_key)))
}

keys_frame <- function(res) {
  dplyr::bind_rows(lapply(res, function(k) dplyr::tibble(
    id = k$id, name = k$name, team = k$team, max_runners = scalar_or_na(k$max_runners))))
}

#' Repositories, branches and trees
#'
#' `gateaux_repositories()` lists the GitHub repositories you are a
#' collaborator on, as of the last sync. `gateaux_branches()` and
#' `gateaux_tree()` ask GitHub live. `gateaux_sync()` re-syncs your
#' repositories, collaborators and teams from GitHub now.
#'
#' @inheritParams gateaux_jobs
#' @param repository The repository as `owner/name`.
#' @param branch The branch; defaults to the repository's branch.
#' @return `gateaux_repositories()`: a data frame (`name`, `branch`,
#'   `description`, `hook_token`, `hook_seen`, `data_access`) with the sync
#'   time in `attr(, "synced_at")`. `gateaux_branches()`: `name` and `commit`.
#'   `gateaux_tree()`: `type`, `sha`, `path`, `url`, with the tree's sha in
#'   `attr(, "sha")`. `gateaux_sync()`: `TRUE`.
#' @export
gateaux_repositories <- function(JWT, server = "gateaux.io") {
  res <- gateaux_request("GET", "/api/v2/repositories", JWT, server)
  out <- dplyr::bind_rows(lapply(res$repositories, function(r) dplyr::tibble(
    name = r$name, branch = scalar_or_na(r$branch), description = scalar_or_na(r$description),
    hook_token = r$hook_token, hook_seen = scalar_or_na(r$hook_seen),
    data_access = list(unlist(r$data_access)))))
  attr(out, "synced_at") <- res$synced_at
  out
}

#' @rdname gateaux_repositories
#' @export
gateaux_branches <- function(repository, JWT, server = "gateaux.io") {
  res <- gateaux_request("GET", sprintf("/api/v2/repositories/%s/branches", repository), JWT, server)
  dplyr::bind_rows(lapply(res, function(b) dplyr::tibble(name = b$name, commit = b$commit)))
}

#' @rdname gateaux_repositories
#' @export
gateaux_tree <- function(repository, JWT, server = "gateaux.io", branch = NULL) {
  query <- if (!is.null(branch)) list(branch = branch) else NULL
  res <- gateaux_request("GET", sprintf("/api/v2/repositories/%s/tree", repository), JWT, server, query = query)
  out <- dplyr::bind_rows(lapply(res$entries, function(e) dplyr::tibble(
    type = e$type, sha = e$sha, path = e$path, url = scalar_or_na(e$url))))
  attr(out, "sha") <- res$sha
  out
}

#' @rdname gateaux_repositories
#' @export
gateaux_sync <- function(JWT, server = "gateaux.io") {
  isTRUE(gateaux_request("POST", "/api/v2/sync", JWT, server)$synced)
}

#' Teams
#'
#' Your GitHub teams, each with its members who are Gateaux users, as of
#' the last sync.
#'
#' @inheritParams gateaux_jobs
#' @return A data frame with one row per team (`id`, `org`, `name`, `slug`,
#'   `members`: the number of members) and the members as a data frame in
#'   `attr(, "members")` (`team_id`, `team_name`, `id`, `github_login`, `name`),
#'   with the sync time in `attr(, "synced_at")`.
#' @export
gateaux_teams <- function(JWT, server = "gateaux.io") {
  res <- gateaux_request("GET", "/api/v2/teams", JWT, server)
  out <- dplyr::bind_rows(lapply(res$teams, function(t) dplyr::tibble(
    id = t$id, org = t$org, name = t$name, slug = t$slug, members = length(t$members))))
  attr(out, "members") <- dplyr::bind_rows(lapply(res$teams, function(t) dplyr::bind_rows(lapply(t$members, function(m)
    dplyr::tibble(team_id = t$id, team_name = t$name, id = m$id, github_login = scalar_or_na(m$github_login), name = scalar_or_na(m$name))))))
  attr(out, "synced_at") <- res$synced_at
  out
}

#' Report thumbnails
#'
#' Set a report's thumbnail from a JPEG or PNG file, or remove it. Needs
#' write access and a user key.
#'
#' @inheritParams gateaux_report
#' @param file Path to a JPEG or PNG image.
#' @return The report as a list (its `thumbnail_url` reflects the change).
#' @export
gateaux_set_thumbnail <- function(report_name, file, JWT, server = "gateaux.io") {
  type <- if (grepl("\\.png$", file, ignore.case = TRUE)) "image/png" else "image/jpeg"
  resp <- httr::PUT(gateaux_url(server, sprintf("/api/v2/reports/%s/thumbnail", report_name)),
                    httr::add_headers(Authorization = paste("Bearer", JWT)),
                    body = httr::upload_file(file, type = type))
  gateaux_check(resp)
  jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' @rdname gateaux_set_thumbnail
#' @export
gateaux_delete_thumbnail <- function(report_name, JWT, server = "gateaux.io") {
  gateaux_request("DELETE", sprintf("/api/v2/reports/%s/thumbnail", report_name), JWT, server)
}

#' Download all of a job's outputs as one zip
#'
#' @inheritParams gateaux_job
#' @param dest Where to write the zip (default: `<job_id>.zip` in the working directory).
#' @return The path written, invisibly.
#' @export
gateaux_download_outputs_zip <- function(job_id, JWT, server = "gateaux.io", dest = NULL) {
  dest <- dest %||% paste0(job_id, ".zip")
  resp <- gateaux_request("GET", sprintf("/api/v2/jobs/%s/outputs.zip", job_id), JWT, server, parse = FALSE)
  writeBin(httr::content(resp, as = "raw"), dest)
  invisible(dest)
}

#' Follow job state changes as they happen
#'
#' `gateaux_stream_events()` follows every job state change across the
#' reports you can see; `gateaux_stream_job()` follows one job until it
#' finishes. Both call `callback` with each job summary as it arrives
#' (a list with `id`, `report`, `status`, `message`, `tags` and timestamps)
#' and return the summaries received.
#'
#' @inheritParams gateaux_jobs
#' @param callback Called with each job summary. The default prints a line.
#' @param until For `gateaux_stream_events()`: a function of the job summary
#'   that returns `TRUE` to stop following (default: never stop).
#' @param timeout Give up after this many seconds (default: never).
#' @return The job summaries received, as a list, invisibly.
#' @export
gateaux_stream_events <- function(JWT, server = "gateaux.io",
                                  callback = function(job) cat(sprintf("%s %s %s\n", job$report, job$id, job$status)),
                                  until = function(job) FALSE, timeout = Inf) {
  sse_stream(gateaux_url(server, "/api/v2/events"), JWT, c("job"), callback, until, timeout)
}

#' @rdname gateaux_stream_events
#' @inheritParams gateaux_job
#' @export
gateaux_stream_job <- function(job_id, JWT, server = "gateaux.io",
                               callback = function(job) cat(sprintf("%s %s %s\n", job$report, job$id, job$status)),
                               timeout = Inf) {
  sse_stream(gateaux_url(server, sprintf("/api/v2/jobs/%s/events", job_id)), JWT, c("status"), callback,
             function(job) FALSE, timeout)
}

# Read a Server-Sent Events stream, calling `callback` with the parsed data of
# events whose name is in `events`. Stops at the server's `end` event, when
# `until` returns TRUE, or after `timeout` seconds.
sse_stream <- function(url, JWT, events, callback, until, timeout) {
  seen <- list()
  buffer <- ""
  done <- function() stop(structure(class = c("gateaux_stream_done", "condition"), list(message = "done", call = NULL)))
  handle_event <- function(event) {
    lines <- strsplit(event, "\n", fixed = TRUE)[[1]]
    type <- sub("^event: ", "", grep("^event: ", lines, value = TRUE))
    data <- sub("^data: ", "", grep("^data: ", lines, value = TRUE))
    if (length(type) != 1) return(invisible())
    if (type == "end") done()
    if (type %in% events && length(data) == 1) {
      job <- jsonlite::fromJSON(data, simplifyVector = FALSE)
      seen[[length(seen) + 1]] <<- job
      callback(job)
      if (isTRUE(until(job))) done()
    }
  }
  args <- list(url, httr::add_headers(Authorization = paste("Bearer", JWT), Accept = "text/event-stream"),
               httr::write_stream(function(chunk) {
                 buffer <<- paste0(buffer, rawToChar(chunk))
                 parts <- strsplit(buffer, "\n\n", fixed = TRUE)[[1]]
                 complete <- if (endsWith(buffer, "\n\n")) parts else utils::head(parts, -1)
                 buffer <<- if (endsWith(buffer, "\n\n")) "" else parts[length(parts)]
                 for (event in complete) handle_event(event)
                 TRUE
               }))
  if (is.finite(timeout)) args <- c(args, list(httr::timeout(timeout)))
  resp <- tryCatch(do.call(httr::GET, args),
                   gateaux_stream_done = function(c) NULL,
                   error = function(e) if (grepl("Timeout|timed out", conditionMessage(e), ignore.case = TRUE)) NULL else stop(e))
  if (!is.null(resp)) gateaux_check(resp)
  invisible(seen)
}
