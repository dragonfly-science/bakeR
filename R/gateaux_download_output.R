#' Download gateaux jobs outputs from R
#'
#' @param report_name The name of the job
#' @param report_id The job ID
#' @param JWT String: Authentication token.
#' @param server The gateaux server url to use. Defaults to gateaux.io
#' @param page_no The pages where job output are located. Defaults to the first and second pages (index 0 and 1).
#' @author Dragonfly bakery
#' @return JSON API return
#' @examples
#' gateaux_download_output(
#'   report_name = "bakeR-testreport",
#'   report_id = "101010",
#'   JWT = JWT
#' )
#' @importFrom magrittr %>%
#' @export


gateaux_download_output <- function(report_name,
                                    report_id,
                                    JWT,
                                    page_no = c(0:1),
                                    server = "gateaux.io") {
  download_url <- list()
  for (pn in seq_along(page_no)) {
    print(pn)
    call_get_url <- sprintf(
      'curl -H "Authorization: Bearer %s" -H "Content-Type: application/json" https://%s/api/jobs/%s?page=%s',
      JWT,
      server,
      report_name,
      page_no[pn]
    )
    json_get_url <- rjson::fromJSON(system(call_get_url, intern = TRUE))

    download_url[[pn]] <- data.frame(
      job_id = sapply(json_get_url$results, function(i) i[["id"]]),
      f_url = sapply(json_get_url$results, function(i) i[["files_url"]])
    )
  }

  download_url <- do.call(rbind, download_url) %>% dplyr::filter(jobID %in% report_id)

  print("Downloading ...")
  for (f in seq_len(download_url)) {
    if (!dir.exists("output/")) dir.create("output/")
    message(sprintf("[%s] %s -- %s", f, report_name, download_url$job_id[f]))
    call_download <- sprintf("curl '%s' > output/%s", download_url$f_url[f], paste0(f, "_", report_name, ".zip"))
    print(call_download)
    system(call_download)
    print(paste("Stored here: ", file.path(getwd(), paste0(f, "_", report_name, ".zip"))))
  }
}
