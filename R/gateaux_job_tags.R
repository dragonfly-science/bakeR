# coerce a list to one jsonlite serialises as a JSON object ({}), even when empty
.as_json_object <- function(x) {
  x <- as.list(x)
  if (length(x) == 0) x <- structure(list(), names = character(0))
  x
}

#' List gateaux/kahawai jobs for a report
#'
#' Lists jobs for a report, most recent first. Optionally filter by free-text
#' terms in the job details (\code{filter}) and/or by attached job tags
#' (\code{tags}, see \code{\link{gateaux_annotate_job}}). When \code{tags} are
#' supplied the call is sent as a POST request (as required by the API to match
#' tags), otherwise a GET request is used.
#'
#' @param report_name The report code to list jobs for.
#' @param JWT String: Authentication token.
#' @param server The server url to use. Defaults to flexion.larva.kahawai.net.nz
#' @param page Integer (or vector of integers): result page(s) to fetch. Results
#' from multiple pages are combined. Defaults to 1.
#' @param filter Optional string: limit jobs to those whose details contain this word.
#' @param tags Optional named list/vector of tag key-value pairs to match,
#' e.g. \code{list(author = "bob")}. Triggers a POST request.
#' @author Dragonfly bakery
#' @return A data.frame of jobs (one row per job), most recent first. Job tags
#' and metadata are flattened into columns where present.
#' @examples
#' \dontrun{
#' # all jobs for a report
#' gateaux_list_jobs(report_name = "MyReport", JWT = JWT)
#'
#' # jobs tagged author=bob
#' gateaux_list_jobs(report_name = "MyReport", JWT = JWT, tags = list(author = "bob"))
#' }
#' @importFrom magrittr %>%
#' @export

gateaux_list_jobs <- function(report_name,
                              JWT,
                              server = 'flexion.larva.kahawai.net.nz',
                              page = 1,
                              filter = NULL,
                              tags = NULL) {

  # the list endpoint requires a trailing slash on the report code, otherwise
  # the route 404s ("File not found"); query string is appended after the slash
  base_url <- sprintf('https://%s/api/jobs/%s/', server, report_name)

  results <- lapply(page, function(pg) {

    query <- c()
    if (!is.null(pg))     query <- c(query, sprintf('page=%s', pg))
    if (!is.null(filter)) query <- c(query, sprintf('filter=%s', utils::URLencode(filter, reserved = TRUE)))
    call_url <- base_url
    if (length(query) > 0) call_url <- paste0(call_url, '?', paste(query, collapse = '&'))

    if (!is.null(tags)) {
      # tag matching requires a POST with the tag key-value pairs in the body
      body <- jsonlite::toJSON(.as_json_object(tags), auto_unbox = TRUE)
      call <- sprintf('curl -sL -X POST -H "Authorization: Bearer %s" -H "Content-Type: application/json" -d \'%s\' "%s"',
                      JWT, body, call_url)
    } else {
      call <- sprintf('curl -sL -H "Authorization: Bearer %s" -H "Content-Type: application/json" "%s"',
                      JWT, call_url)
    }

    raw <- paste(system(call, intern = TRUE), collapse = "\n")
    # give a useful error when the server returns a non-JSON body (e.g. a 404
    # "File not found" page) rather than the opaque jsonlite lexer error
    if (!grepl('^\\s*[\\[{]', raw))
      stop(sprintf("Unexpected (non-JSON) response from %s:\n%s", call_url, raw), call. = FALSE)
    json <- jsonlite::fromJSON(raw, flatten = TRUE)
    json$results
  })

  # drop empty pages, then row-bind into a single tidy data.frame
  results <- results[vapply(results, function(x) !is.null(x) && NROW(x) > 0, logical(1))]
  if (length(results) == 0) return(data.frame())
  dplyr::bind_rows(results)
}


#' Annotate a gateaux/kahawai job with tags and/or metadata
#'
#' Attaches tags (searchable key-value pairs, see \code{\link{gateaux_list_jobs}})
#' and/or metadata (arbitrary json) to a job. Tags and metadata are returned in
#' the job list API response. Passing an empty value (e.g. \code{tags = list()})
#' clears the corresponding field for the job.
#'
#' @param report_name The report code the job belongs to.
#' @param job_id The job ID to annotate.
#' @param JWT String: Authentication token.
#' @param server The server url to use. Defaults to flexion.larva.kahawai.net.nz
#' @param tags Optional named list/vector of searchable key-value pairs,
#' e.g. \code{list(state = "unreviewed", series = "XYZ123")}. Pass \code{list()}
#' to clear existing tags.
#' @param metadata Optional R list (or pre-formed json string) of arbitrary
#' metadata. Pass \code{list()} to clear existing metadata.
#' @author Dragonfly bakery
#' @return The parsed JSON API response (invisibly).
#' @examples
#' \dontrun{
#' gateaux_annotate_job(report_name = "MyReport",
#'                      job_id = "123456789",
#'                      JWT = JWT,
#'                      tags = list(state = "unreviewed", series = "XYZ123"),
#'                      metadata = list())
#' }
#' @importFrom magrittr %>%
#' @export

gateaux_annotate_job <- function(report_name,
                                 job_id,
                                 JWT,
                                 server = 'flexion.larva.kahawai.net.nz',
                                 tags = NULL,
                                 metadata = NULL) {

  if (is.null(tags) && is.null(metadata))
    stop("Provide at least one of 'tags' or 'metadata' to annotate a job.")

  body <- list()
  if (!is.null(tags))     body$tags     <- .as_json_object(tags)
  # allow metadata to be passed as a pre-formed json string or an R list
  if (!is.null(metadata)) body$metadata <- if (is.character(metadata)) jsonlite::fromJSON(metadata) else .as_json_object(metadata)

  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

  call_url <- sprintf('https://%s/api/job/annotate/%s/%s', server, report_name, job_id)
  call <- sprintf('curl -s -X POST -H "Authorization: Bearer %s" -H "Content-Type: application/json" -d \'%s\' "%s"',
                  JWT, body_json, call_url)

  ret <- system(call, intern = TRUE)
  invisible(jsonlite::fromJSON(ret))
}
