#' Call arbitrary gateaux jobs from R
#'
#' Submits one job per entry of `pars_list`. By default this uses the v2 API
#' (`POST /api/v2/reports/<report>/jobs`), which returns the created job(s)
#' directly. With `api = "v1"` (or `options(bakeR.api = "v1")`) it uses the
#' legacy `POST /job/<report>` route exactly as earlier versions of bakeR did.
#'
#' @param pars_list A named list of named lists of environment variables and job requirements - one list per job.
#' Each named list defines a job tag (name), with up to three sub-lists:
#'     *  pars (named environment variables passed to the job),
#'     *  wants (named optional job dependencies),
#'     *  requires (named required job dependencies).
#' @param report_name The name of the report to run
#' @param server The gateaux server to use: a host name (defaults to gateaux.io)
#'   or a full base URL such as `http://127.0.0.1:7444`.
#' @param JWT String: Authentication token (a user API key from `/auth/key`).
#' @param log_jobs Export job parameters to logfile?
#' @param prefix for the list, can include path
#' @param append append to existing file?
#' @param cpus cpus for runner, will override any gateaux.yaml settings
#' @param memory memory for runner, will override any gateaux.yaml settings
#' @param branch Branch to run instead of the report's default (v2 only).
#' @param api `"v2"` (default) or `"v1"` for the legacy API; see [gateaux_api()].
#' @author Dragonfly bakery
#' @return A data frame with one row per submitted job (see [gateaux_jobs_frame()]),
#'   returned invisibly. With `api = "v1"` the legacy route returns the jobs too.
#' @examples
#' \dontrun{
#' pars = list(Run1 = list(pars = list(Env1 = "This_env", Env2 = "That_env"), wants = list(Upstream_job = 1234), requires = list(Upstream_job2 = 4321)),
#'             Run2 = list(pars = list(Env1 = "That_env", Env2 = "This_env"), wants = list(Upstream_job = 2222), requires = list(Upstream_job2 = 4321)))
#' gateaux_job_runner(pars,
#'                    report_name = "bakeR-testreport",
#'                    JWT = JWT)
#' }
#' @importFrom magrittr %>%
#' @export

gateaux_job_runner <- function(pars_list = NULL,
                               report_name,
                               server = 'gateaux.io',
                               JWT,
                               log_jobs = T,
                               prefix = 'jobs',
                               append = TRUE,
                               cpus = '',
                               memory = '',
                               branch = NULL,
                               api = NULL){

  api <- gateaux_api(api)
  if (length(pars_list) == 0) stop("pars_list is empty")
  if (is.null(names(pars_list)) || any(!nzchar(names(pars_list)))) stop("Every entry of pars_list needs a name (the job tag)")

  submitted <- lapply(names(pars_list), function(tag) {
    body <- gateaux_job_body(tag, pars_list[[tag]], cpus = cpus, memory = memory, branch = branch, api = api)
    jobs <- if (api == "v2") {
      res <- gateaux_request("POST", sprintf("/api/v2/reports/%s/jobs", report_name), JWT, server, body = body)
      res$results
    } else {
      gateaux_request("POST", sprintf("/job/%s", report_name), JWT, server, body = body)
    }
    gateaux_jobs_frame(jobs)
  })
  submitted <- dplyr::bind_rows(submitted)

  if (log_jobs) {
    readr::write_csv(submitted, file = paste0(prefix, '-joblist.csv'), append = append && file.exists(paste0(prefix, '-joblist.csv')))
  }
  invisible(submitted)
}

#' Build the request body for one job
#'
#' @param tag The job tag (the entry's name in `pars_list`).
#' @param spec One entry of `pars_list`: a list with optional `pars`, `wants`, `requires`.
#' @inheritParams gateaux_job_runner
#' @return A list ready to be sent as JSON.
#' @keywords internal
gateaux_job_body <- function(tag, spec, cpus = '', memory = '', branch = NULL, api = "v2") {
  env <- c(list(TAG = tag), lapply(spec$pars %||% list(), function(v) as.character(v)))

  refs <- function(deps) {
    if (length(deps) == 0) return(NULL)
    unlist(lapply(names(deps), function(report) {
      ids <- deps[[report]]
      if (length(ids) == 0 || all(is.na(ids))) report else sprintf("%s:%s", report, as.character(ids))
    }))
  }

  body <- list(env = env)
  if (!identical(cpus, '')) body$cpus <- as.numeric(cpus)
  if (!identical(memory, '')) body$memory <- as.numeric(memory)
  wants <- refs(spec$wants); if (!is.null(wants)) body$wants <- I(wants)
  requires <- refs(spec$requires); if (!is.null(requires)) body$requires <- I(requires)

  if (api == "v2") {
    body$tags <- list(tag = tag)
    if (!is.null(branch)) body$branch <- branch
  } else if (!is.null(branch)) {
    stop("branch is only supported by the v2 API")
  }
  body
}


#' Modify par list to run all jobs listed with a certain env variable -
#' useful to temporarily try or alter a parameter setup without modifying the original setup
#'
#' @param pars_list A named list of named lists of environment variables and job requirements - one list per job.
#' Each named list defines a job tag (name), with up to three sub-lists:
#'     *  pars (named environment variables passed to the job),
#'     *  wants (named optional job dependencies),
#'     *  requires (named required job dependencies).
#' @param var env variable to create or alter.
#' @param value env variable value to assign. Can be a single value or a vector or the same length as pars_list.
#' @param set Which sub-list to modify (default `"pars"`).
#' @param rm_vals If `TRUE`, remove the sub-list named by `set` instead.
#' @author Dragonfly bakery
#' @return Altered parameter list to run with gateaux_job_runner
#' @export

run_all_with <- function(pars_list, var, value, set="pars",rm_vals=F){

  if(!rm_vals){
    lapply(pars_list, function(l) {for(s in 1:length(var)) {l[[set]][var[s]] <- value[s]};l})
  } else {
    lapply(pars_list, function(l) {l[[set]]<- NULL;l})
  }

}
