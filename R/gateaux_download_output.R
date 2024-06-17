#' Download gateaux jobs outputs from R
#'
#' @param report_name The name of the job
#' @param report_id The job ID
#' @param JWT String: Authentication token.
#' @param server The gateaux server url to use. defaults to gateaux.io
#' @param page_no The pages where job output are located. defaults to the first and second pages, which is index 0 and 1.
#' @author Dragonfly bakery
#' @return JSON API return
#' @examples
#' gateaux_download_output(report_name = "bakeR-testreport",
#'                         report_id = "101010",
#'                         JWT = JWT)
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
    json_getURL <- rjson::fromJSON(system(call_getURL, intern = T))

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
    print(call_download)
    system(call_download)
    print(paste('Stored here: ', file.path(getwd(), paste0(f,'_',report_name, '.zip'))))
  }
}
