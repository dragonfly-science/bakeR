# bakeR
R package for calling gateaux API from R

## Install

```{R}
remotes::install_github("dragonfly-science/bakeR")
```


## Example use

After loading your private JWT token in your R session (```JWT="my_token_goes_here"```), define the parameter list for each run, and launch!

```{R}

pars = list(Run1 = list(pars = list(Env1 = "This_env", 
                                    Env2 = "That_env"), 
                        wants = list(Upstram_job = 1234), 
                        requires = list(Upstream_job2 = 4321)),
            Run2 = list(pars = list(Env1 = "That_env", 
                                    Env2 = "This_env"), 
                        wants = list(Upstram_job = 2222), 
                        requires = list(Upstream_job2 = 4321)))
                        
gateaux_job_runner(pars, 
                   report_name = "bakeR-testreport",
                   JWT = JWT)
```
