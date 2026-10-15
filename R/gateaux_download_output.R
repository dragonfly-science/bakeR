#' Download gateaux job outputs from R
#'
#' Downloads the output files of one or more jobs, each as a zip archive, into
#' an \code{output/} directory under the working directory. The jobs are found
#' by listing the report's jobs page by page, so a job older than the pages
#' requested will not be found.
#'
#' @param report_name The report code (as shown in the Gateaux web UI).
#' @param report_id The job id(s) whose outputs to download.
#' @param JWT String: your API key, from the API guide page of the web UI.
#' @param page_no The result page(s) of the report's job list to search for
#' the jobs. Pages are zero-based; the default searches the first two.
#' @param server The gateaux server to use. Defaults to gateaux.io; use
#' \code{"kahawai.io"} for Kahawai.
#' @author Dragonfly bakery
#' @return Nothing useful; called for its side effect of writing
#' \code{output/<n>_<report_name>.zip} for each job found.
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
                                    server = 'gateaux.io') {
  # require(rjson)
  # require(dplyr)

  downloadURL <- list()
  for(pn in 1:length(page_no)) {
    print(pn)
    call_getURL <- sprintf('curl -H "Authorization: Bearer %s" -H "Content-Type: application/json" https://%s/api/jobs/%s?page=%s', JWT, server, report_name, page_no[pn])
    json_getURL <- rjson::fromJSON(.system(call_getURL, intern = TRUE))

    downloadURL[[pn]] <- data.frame(jobID = sapply(json_getURL$results, function(i)i[["id"]]),
                                    fURL = sapply(json_getURL$results, function(i)i[["files_url"]]))
  }

  downloadURL <- do.call(rbind, downloadURL) %>% dplyr::filter(jobID %in% report_id)

  print('Downloading ...')
  for(f in 1:nrow(downloadURL)) {
    if(!dir.exists('output/')) dir.create('output/')
    message(sprintf('[%s] %s -- %s', f, report_name, downloadURL$jobID[f]))
    # call_download <- sprintf("curl -o output/%s '%s'", paste0(f,'_',report_name, '.zip'), downloadURL$fURL[f])
    call_download <- sprintf("curl '%s' > output/%s", downloadURL$fURL[f], paste0(f,'_',report_name, '.zip'))
    print(.redact(call_download))
    .system(call_download, intern = FALSE)
    print(paste('Stored here: ', file.path(getwd(), 'output', paste0(f,'_',report_name, '.zip'))))
  }
}
