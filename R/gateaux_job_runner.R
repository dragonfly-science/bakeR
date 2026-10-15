#' Launch gateaux jobs from R
#'
#' Submits one job per element of \code{pars_list} to a report, each with its
#' own environment variables and (optionally) job dependencies. The element's
#' name is attached to the job as the \code{TAG} environment variable.
#'
#' @param pars_list A named list of named lists, one per job. Each inner list
#' may have up to three elements:
#'     *  \code{pars}: named environment variables passed to the job,
#'     *  \code{wants}: named optional job dependencies (job ids),
#'     *  \code{requires}: named required job dependencies (job ids).
#' @param report_name The report code (as shown in the Gateaux web UI).
#' @param JWT String: your API key, from the API guide page of the web UI.
#' @param server The gateaux server to use. Defaults to gateaux.io; use
#' \code{"kahawai.io"} for Kahawai.
#' @param log_jobs If TRUE (the default), append the details of each submitted
#' job to a CSV file (see \code{prefix}) as well as returning them.
#' @param prefix Prefix of the CSV file written when \code{log_jobs} is TRUE;
#' may include a path. The file is \code{<prefix>-joblist.csv}.
#' @param append Append to the CSV file if it exists (TRUE) or overwrite it.
#' @param cpus CPUs for the job, overriding the report's gateaux.yaml.
#' @param memory Memory for the job in gigabytes, overriding the report's gateaux.yaml.
#' @author Dragonfly bakery
#' @return A list with one element per job: the job as returned by the API
#' (a list), or, when \code{log_jobs} is TRUE, the flattened data frame that was
#' written to the CSV file.
#' @examples
#' \dontrun{
#' pars <- list(Run1 = list(pars = list(Env1 = "This_env", Env2 = "That_env"),
#'                          wants = list(Upstream_job = 1234),
#'                          requires = list(Upstream_job2 = 4321)),
#'              Run2 = list(pars = list(Env1 = "That_env", Env2 = "This_env"),
#'                          wants = list(Upstream_job = 2222),
#'                          requires = list(Upstream_job2 = 4321)))
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
                               memory = ''){

    # the job routes live under /api/ (as the other functions already use)
    call_url <- paste0("https://",server,"/api/job/",report_name)
    if(cpus != '') cpus <- sprintf('"cpus":%s, ',  cpus)
    if(memory != '') memory <- sprintf('"memory":%s, ', memory)

  ret <- lapply(1:length(pars_list),function(l){

    tag = sprintf('"%s":"%s"',"TAG",names(pars_list[l]))
    if(length(pars_list[[l]]$pars)>0){
      envs = lapply(1:length(pars_list[[l]]$pars),
                    function(ll) sprintf('"%s":"%s"',names(pars_list[[l]]$pars)[ll],pars_list[[l]]$pars[ll]))
      envs = paste(unlist(envs), collapse = ',')
      envs = paste(tag, envs, sep=',')
    } else {envs = tag}

    if(length(pars_list[[l]]$wants)>0){
      wants = lapply(1:length(pars_list[[l]]$wants),
                     function(ll) sprintf('"%s:%s"',names(pars_list[[l]]$wants)[ll],as.character(pars_list[[l]]$wants[ll][[1]])))
      wants = paste(unlist(wants), collapse = ',')
      wants = paste0(', "wants":[',wants,']')
    } else {wants = ''}

    if(length(pars_list[[l]]$requires)>0){
      requires = lapply(1:length(pars_list[[l]]$requires),
                        function(ll) sprintf('"%s:%s"',names(pars_list[[l]]$requires)[ll],as.character(pars_list[[l]]$requires[ll][[1]])))
      requires = paste(unlist(requires), collapse = ',')
      requires = paste0(', "requires":[',requires,']')
    } else {requires = ''}

    call <- sprintf('curl -X POST -H "Authorization: Bearer %s" -H "Content-Type: application/json" -d \'{%s%s"env":{%s} %s %s}\' %s',
                    JWT,
                    cpus,
                    memory,
                    envs,
                    wants,
                    requires,
                    call_url
                    )

      print(.redact(call))

    ret <- .system(call, intern = TRUE)
    if(log_jobs){

      rr <- jsonlite::fromJSON(ret)
      rr <- dplyr::bind_cols(rr %>% dplyr::select(-parameters),rr$parameters)
      rr <- dplyr::bind_cols(rr %>% dplyr::select(-env),rr$env)
      readr::write_csv(rr %>% dplyr::select(-variant), file = paste0(prefix,'-joblist.csv'), append = append)
    } else {
      jsonlite::fromJSON(ret)
      }
  }
  )

}


#' Modify a parameter list to run all jobs with a certain setting
#'
#' Useful to try or alter a setting across every job without editing the
#' original list.
#'
#' @param pars_list A named list of named lists, one per job, as for
#' \code{\link{gateaux_job_runner}}.
#' @param var Name(s) of the environment variable (or dependency) to create or alter.
#' @param value Value(s) to assign, one per element of \code{var}.
#' @param set Which part of each job to change: \code{"pars"} (the default),
#' \code{"wants"} or \code{"requires"}.
#' @param rm_vals If TRUE, remove the whole \code{set} element from every job
#' instead of setting values.
#' @author Dragonfly bakery
#' @return The altered parameter list, ready for \code{\link{gateaux_job_runner}}.
#' @examples
#' pars <- list(Run1 = list(pars = list(Env1 = "a")), Run2 = list(pars = list(Env1 = "b")))
#' run_all_with(pars, "DEBUG", "yes")
#' @export

run_all_with <- function(pars_list, var, value, set="pars",rm_vals=F){

  if(!rm_vals){
    lapply(pars_list, function(l) {for(s in 1:length(var)) {l[[set]][var[s]] <- value[s]};l})
  } else {
    lapply(pars_list, function(l) {l[[set]]<- NULL;l})
  }

}


