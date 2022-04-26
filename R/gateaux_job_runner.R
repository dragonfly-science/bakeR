#' Call arbitrary gateaux jobs from R
#'
#' @param pars_list A named list of named lists of environment variables and job requirements - one list per job.
#' Each named list defines a job tag (name), with up to three sub-lists:
#'     *  pars (named environment variables passed to the job),
#'     *  wants (named optional job dependencies),
#'     *  requires (named required job dependencies).
#' @param report_name The name of the job
#' @param JWT String: Authentication token.
#' @param Server The gateaux server url to use. defaults to gorbachev.io
#' @param log_jobs Export job parameters to logfile?
#' @param prefix for the list, can include path
#' @param append append to existing file?
#' @param cpus cpus for apparatchik, will override any gorbachev.yaml settings
#' @param memory memory for apparatchik, will override any gorbachev.yaml settings#'
#' @author Dragonfly bakery
#' @return JSON API return
#' @examples
#' pars = list(Run1 = list(pars = list(Env1 = "This_env", Env2 = "That_env"), wants = list(Upstream_job = 1234), requires = list(Upstream_job2 = 4321)),
#'             Run2 = list(pars = list(Env1 = "That_env", Env2 = "This_env"), wants = list(Upstream_job = 2222), requires = list(Upstream_job2 = 4321)))
#' gateaux_job_runner(pars,
#'                    report_name = "bakeR-testreport",
#'                    JWT = JWT)
#' @importFrom magrittr %>%
#' @export

gateaux_job_runner <- function(pars_list = NULL,
                               report_name,
                               server = 'gorbachev.io',
                               JWT,
                               log_jobs = T,
                               prefix = 'jobs',
                               append = TRUE,
                               cpus = '',
                               memory = ''){

    call_url <- paste0("https://",server,"/job/",report_name)
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

      print(call)

    ret <- system(call,intern=T)
    if(log_jobs){

      rr <- jsonlite::fromJSON(ret)
      rr <- dplyr::bind_cols(rr %>% select(-parameters),rr$parameters)    # added 'dplyr::' to solve a error message: could not find function "bind_cols"
      rr <- dplyr::bind_cols(rr %>% select(-env),rr$env)                  # added 'dplyr::' to solve a error message: could not find function "bind_cols"
      readr::write_csv(rr%>% select(-variant),path = paste0(prefix,'-joblist.csv'),append = append)
    } else {
      jsonlite::fromJSON(ret)
      }
  }
  )

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


