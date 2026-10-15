# Report and secret management (v2 only).

#' List the reports you can see
#'
#' @inheritParams gateaux_jobs
#' @param filter Text to match in code, description, repository, branch, directory, owner or runner key.
#' @param page Zero-based page number.
#' @param perpage Results per page.
#' @param sort `"activity"` (default), `"code"`, `"status"` or `"author"`.
#' @return A data frame with one row per report (code, description, repository,
#'   branch, directory, owner, access type, latest job id and status). The
#'   total number of matching reports is in `attr(, "total")`.
#' @export
gateaux_reports <- function(JWT, server = "gateaux.io", filter = NULL, page = 0, perpage = 30, sort = "activity") {
  query <- list(page = page, perpage = perpage, sort = sort)
  if (!is.null(filter)) query$filter <- filter
  res <- gateaux_request("GET", "/api/v2/reports", JWT, server, query = query)
  scalar <- function(x) if (is.null(x)) NA else x
  rows <- lapply(res$results, function(item) {
    r <- item$report; j <- item$latest_job
    dplyr::tibble(code = r$code, description = scalar(r$description), repository = r$repository,
                  branch = r$branch, directory = r$directory, owner = scalar(r$owner$github_login),
                  access = scalar(r$access),
                  latest_job_id = if (is.null(j)) NA_integer_ else as.integer(j$id),
                  latest_job_status = if (is.null(j)) NA_character_ else j$status)
  })
  out <- dplyr::bind_rows(rows)
  attr(out, "total") <- res$total
  out
}

#' Read, create, update or archive a report
#'
#' `gateaux_create_report()` needs `code`, `repository`, `branch` and
#' `directory`; you must be a collaborator on the repository.
#' `gateaux_update_report()` changes only the fields given; a new `code`
#' moves the report's jobs with it. `gateaux_delete_report()` archives
#' (soft-deletes) the report. All need write access except reading.
#'
#' @inheritParams gateaux_jobs
#' @param code Report code.
#' @param repository GitHub repository, `owner/name`.
#' @param branch Branch to run.
#' @param directory Directory within the repository holding the report.
#' @param description Free text.
#' @param runner_key Id of the runner key to pin the report to, if any.
#' @return The report as a list (the parsed API record); `gateaux_delete_report()` returns the archived code.
#' @export
gateaux_report <- function(report_name, JWT, server = "gateaux.io") {
  gateaux_request("GET", sprintf("/api/v2/reports/%s", report_name), JWT, server)
}

#' @rdname gateaux_report
#' @export
gateaux_create_report <- function(code, repository, branch, directory, JWT, server = "gateaux.io",
                                  description = NULL, runner_key = NULL) {
  body <- list(code = code, repository = repository, branch = branch, directory = directory)
  if (!is.null(description)) body$description <- description
  if (!is.null(runner_key)) body$runner_key <- runner_key
  gateaux_request("POST", "/api/v2/reports", JWT, server, body = body)
}

#' @rdname gateaux_report
#' @param ... Fields to change: any of `code`, `description`, `repository`, `branch`, `directory`, `runner_key`.
#' @export
gateaux_update_report <- function(report_name, JWT, server = "gateaux.io", ...) {
  body <- list(...)
  allowed <- c("code", "description", "repository", "branch", "directory", "runner_key")
  bad <- setdiff(names(body), allowed)
  if (length(bad) > 0) stop("Unknown report fields: ", paste(bad, collapse = ", "))
  if (length(body) == 0) stop("Nothing to change")
  gateaux_request("PATCH", sprintf("/api/v2/reports/%s", report_name), JWT, server, body = body)
}

#' @rdname gateaux_report
#' @export
gateaux_delete_report <- function(report_name, JWT, server = "gateaux.io") {
  gateaux_request("DELETE", sprintf("/api/v2/reports/%s", report_name), JWT, server)$deleted
}

#' Manage a report's secrets
#'
#' Secrets are environment variables injected into the report's jobs. Values
#' are never returned by the API: `gateaux_secrets()` lists names only.
#' Setting and removing need write access and a user key, not a service token.
#'
#' @inheritParams gateaux_jobs
#' @param name The secret's name (an environment variable name).
#' @param value The value.
#' @return A character vector of the report's secret names.
#' @export
gateaux_secrets <- function(report_name, JWT, server = "gateaux.io") {
  as.character(unlist(gateaux_request("GET", sprintf("/api/v2/reports/%s/secrets", report_name), JWT, server)$secrets))
}

#' @rdname gateaux_secrets
#' @export
gateaux_set_secret <- function(report_name, name, value, JWT, server = "gateaux.io") {
  as.character(unlist(gateaux_request("PUT", sprintf("/api/v2/reports/%s/secrets/%s", report_name, name), JWT, server,
                                      body = list(value = value))$secrets))
}

#' @rdname gateaux_secrets
#' @export
gateaux_delete_secret <- function(report_name, name, JWT, server = "gateaux.io") {
  as.character(unlist(gateaux_request("DELETE", sprintf("/api/v2/reports/%s/secrets/%s", report_name, name), JWT, server)$secrets))
}
