#' Which Gateaux API to talk to
#'
#' bakeR talks to the v2 API (`/api/v2/...`) by default. The legacy API can be
#' selected per call with `api = "v1"`, or for a whole session with
#' `options(bakeR.api = "v1")`.
#'
#' @param api `"v2"`, `"v1"` or `NULL` (use `getOption("bakeR.api", "v2")`).
#' @return `"v2"` or `"v1"`.
#' @export
gateaux_api <- function(api = NULL) {
  api <- api %||% getOption("bakeR.api", "v2")
  match.arg(api, c("v2", "v1"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Build a Gateaux URL
#'
#' @param server Host name (`"gateaux.io"`) or a full base URL
#'   (`"http://127.0.0.1:7444"`). A bare host name gets `https://`.
#' @param path Path beginning with `/`.
#' @return The full URL.
#' @keywords internal
gateaux_url <- function(server, path) {
  base <- if (grepl("^https?://", server)) server else paste0("https://", server)
  paste0(sub("/+$", "", base), path)
}

#' Make one request to Gateaux
#'
#' Sends the bearer token, encodes `body` as JSON, and turns error responses
#' into R errors. v2 errors carry a JSON envelope `{"error": {code, message}}`
#' which is reported as `gateaux <code>: <message>`.
#'
#' @param method `"GET"`, `"POST"`, `"PUT"`, `"PATCH"` or `"DELETE"`.
#' @param path Request path.
#' @param JWT Bearer token: your user API key (from the API guide page of the
#'   web UI), a service token or a report token.
#' @param server See [gateaux_url()].
#' @param body List to send as JSON, or `NULL`.
#' @param query Named list of query parameters, or `NULL`.
#' @param headers Named character vector of extra headers.
#' @param parse If `TRUE` (default) return the parsed JSON body (with
#'   `simplifyVector = FALSE`); otherwise return the `httr` response.
#' @return Parsed JSON (a list) or the response.
#' @keywords internal
gateaux_request <- function(method, path, JWT, server = "gateaux.io", body = NULL,
                            query = NULL, headers = character(), parse = TRUE) {
  if (missing(JWT) || is.null(JWT) || !nzchar(JWT)) {
    stop("A JWT is required: get your API key from the API guide page of the Gateaux web UI")
  }
  url <- gateaux_url(server, path)
  hdrs <- httr::add_headers(Authorization = paste("Bearer", JWT), .headers = headers)
  args <- list(url = url, hdrs, httr::accept_json())
  if (!is.null(query)) args$query <- query
  if (!is.null(body)) {
    args$body <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
    args <- c(args, list(httr::content_type_json()))
  }
  fn <- switch(method, GET = httr::GET, POST = httr::POST, PUT = httr::PUT, PATCH = httr::PATCH,
               DELETE = httr::DELETE, stop("Unsupported method: ", method))
  # a failed connection is reported by httr with the URL; never let a token through
  resp <- tryCatch(do.call(fn, args), error = function(e) stop(.redact(conditionMessage(e)), call. = FALSE))
  gateaux_check(resp)
  if (!parse) return(resp)
  text <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!nzchar(text)) return(NULL)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' Stop with a useful message if a response is an error
#' @keywords internal
gateaux_check <- function(resp) {
  status <- httr::status_code(resp)
  if (status < 400) return(invisible(resp))
  text <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(parsed$error$code)) {
    stop(sprintf("gateaux %s (HTTP %s): %s", parsed$error$code, status, .redact(parsed$error$message)), call. = FALSE)
  }
  stop(sprintf("gateaux HTTP %s: %s", status, .redact(if (nzchar(text)) text else httr::http_status(resp)$message)), call. = FALSE)
}

#' Turn job records into a data frame
#'
#' One row per job, with the job's top-level fields, its `tag`, one column per
#' environment variable (`env.<NAME>`) and one per job tag (`tags.<key>`).
#' Works for both API versions.
#'
#' @param jobs A list of job records as returned by the API.
#' @return A data frame (tibble).
#' @export
gateaux_jobs_frame <- function(jobs) {
  if (length(jobs) == 0) {
    return(dplyr::tibble(id = integer(), status = character(), tag = character()))
  }
  scalar <- function(x) if (is.null(x) || length(x) == 0) NA else x
  rows <- lapply(jobs, function(j) {
    env <- j$parameters$env %||% list()
    tag <- j$tags$tag %||% env$TAG
    base <- dplyr::tibble(
      id           = as.integer(j$id),
      report       = scalar(j$report),
      status       = scalar(j$status),
      tag          = scalar(tag),
      triggered_by = scalar(if (is.list(j$triggered_by)) j$triggered_by$github_login else j$triggered_by),
      started_at   = scalar(j$started_at),
      finished_at  = scalar(j$finished_at),
      message      = scalar(j$message),
      runner       = scalar(j$runner %||% j$apparatchik),
      commit       = scalar(j$commit),
      cpus         = scalar(j$parameters$cpus),
      memory       = scalar(j$parameters$memory)
    )
    if (length(env) > 0) {
      env <- lapply(env, scalar)
      names(env) <- paste0("env.", names(env))
      base <- dplyr::bind_cols(base, dplyr::as_tibble(env))
    }
    tags <- j$tags %||% list()
    if (length(tags) > 0) {
      tags <- lapply(tags, function(v) scalar(as.character(v)))
      names(tags) <- paste0("tags.", names(tags))
      base <- dplyr::bind_cols(base, dplyr::as_tibble(tags))
    }
    base
  })
  dplyr::bind_rows(rows)
}
