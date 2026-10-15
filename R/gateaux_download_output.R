#' Download gateaux job outputs from R
#'
#' With the v2 API (default) each output file of each job is downloaded into
#' `<dest>/<report>-<job id>/`, keeping file names, and checked against the
#' MD5 checksum the server reports. With `api = "v1"` the legacy behaviour is
#' kept: the job list is paged through and each job's `files_url` is fetched
#' as a ZIP into `<dest>/<n>_<report>.zip`.
#'
#' @param report_name The name of the report
#' @param report_id The job ID(s)
#' @param JWT String: Authentication token.
#' @param page_no (v1 only) The job-list pages to search for the jobs. Defaults to pages 0 and 1.
#' @param server The gateaux server to use; see [gateaux_job_runner()].
#' @param dest Directory to download into (default `output`).
#' @param api `"v2"` (default) or `"v1"`; see [gateaux_api()].
#' @author Dragonfly bakery
#' @return A data frame of the downloaded files (job id, name, local path), invisibly.
#' @examples
#' \dontrun{
#' gateaux_download_output(report_name = "bakeR-testreport",
#'                         report_id = "101010",
#'                         JWT = JWT)
#' }
#' @importFrom magrittr %>%
#' @export

gateaux_download_output <- function(report_name,
                                    report_id,
                                    JWT,
                                    page_no = c(0:1),
                                    server = 'gateaux.io',
                                    dest = 'output',
                                    api = NULL) {
  api <- gateaux_api(api)
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)

  if (api == "v2") {
    rows <- lapply(report_id, function(jid) {
      files <- gateaux_job_outputs(jid, JWT, server)
      if (nrow(files) == 0) {
        message(sprintf("%s job %s has no output files", report_name, jid))
        return(NULL)
      }
      jobdir <- file.path(dest, paste0(report_name, "-", jid))
      if (!dir.exists(jobdir)) dir.create(jobdir, recursive = TRUE)
      files$path <- vapply(seq_len(nrow(files)), function(i) {
        path <- file.path(jobdir, files$name[i])
        if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
        message(sprintf("[%s/%s] %s job %s: %s", i, nrow(files), report_name, jid, files$name[i]))
        resp <- gateaux_request("GET", files$url[i], JWT, server, parse = FALSE)
        writeBin(httr::content(resp, as = "raw"), path)
        sum <- files$checksum[i]
        if (!is.na(sum) && startsWith(sum, "md5:") && tolower(sub("md5:", "", sum)) != tolower(unname(tools::md5sum(path)))) {
          warning(sprintf("Checksum mismatch for %s", path), call. = FALSE)
        }
        path
      }, character(1))
      files$job_id <- as.integer(jid)
      files
    })
    return(invisible(dplyr::bind_rows(rows)))
  }

  # v1: find each job's files_url in the paged job list and fetch the ZIP
  downloadURL <- lapply(page_no, function(pn) {
    res <- gateaux_request("GET", sprintf("/api/jobs/%s", report_name), JWT, server, query = list(page = pn))
    data.frame(jobID = vapply(res$results, function(i) as.integer(i[["id"]]), integer(1)),
               fURL  = vapply(res$results, function(i) i[["files_url"]], character(1)),
               stringsAsFactors = FALSE)
  })
  downloadURL <- do.call(rbind, downloadURL)
  downloadURL <- downloadURL[downloadURL$jobID %in% report_id, , drop = FALSE]
  if (nrow(downloadURL) == 0) stop("None of the requested jobs were found in the listed pages")

  message('Downloading ...')
  paths <- vapply(seq_len(nrow(downloadURL)), function(f) {
    path <- file.path(dest, paste0(f, '_', report_name, '.zip'))
    message(sprintf('[%s] %s -- %s', f, report_name, downloadURL$jobID[f]))
    # files_url carries its own report token as ?jwt=; a user API key in the
    # Authorization header would be rejected by the output server, so send none.
    resp <- httr::GET(downloadURL$fURL[f])
    gateaux_check(resp)
    writeBin(httr::content(resp, as = "raw"), path)
    message('Stored here: ', normalizePath(path))
    path
  }, character(1))
  invisible(data.frame(job_id = downloadURL$jobID, name = basename(paths), path = paths, stringsAsFactors = FALSE))
}
