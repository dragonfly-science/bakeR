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
                                    Env2 = "That_env")),
            Run2 = list(pars = list(Env1 = "That_env", 
                                    Env2 = "This_env")))
                        
gateaux_job_runner(pars, 
                   report_name = "bakeR-testreport",
                   JWT = JWT)
```

wants and requires arguments can be added like so (where numbers refer to job ids):

```{R}

pars = list(Run1 = list(pars = list(Env1 = "This_env", 
                                    Env2 = "That_env"), 
                        wants = list(Upstram_job = 1234), 
                        requires = list(Upstream_job2 = 4321)),
            Run2 = list(pars = list(Env1 = "That_env", 
                                    Env2 = "This_env"), 
                        wants = list(Upstram_job = c(2222, 5555, 6666)), 
                        requires = list(Upstream_job2 = c(3333, 2244, 111))))

```

## Listing jobs

List the jobs for a report (most recent first) as a tidy data.frame:

```{R}

jobs <- gateaux_list_jobs(report_name = "MyReport", JWT = JWT)
```

Page through results, or filter by a word in the job details:

```{R}

# fetch the first three result pages
jobs <- gateaux_list_jobs(report_name = "MyReport", JWT = JWT, page = 1:3)

# only jobs whose details mention "rerun"
jobs <- gateaux_list_jobs(report_name = "MyReport", JWT = JWT, filter = "rerun")
```

To match jobs by attached tags (see "Annotating jobs" below), pass a named list
of tag key-value pairs. This is sent as a POST request, as the API requires for
tag matching:

```{R}

jobs <- gateaux_list_jobs(report_name = "MyReport", JWT = JWT,
                          tags = list(author = "bob"))
```

## Annotating jobs

Attach searchable tags (key-value pairs) and/or arbitrary metadata to a job:

```{R}

gateaux_annotate_job(report_name = "MyReport",
                     job_id = "123456789",
                     JWT = JWT,
                     tags = list(state = "unreviewed", series = "XYZ123"))
```

Tags and metadata are returned in the job list response, so you can annotate a
job and then find it again with `gateaux_list_jobs(..., tags = ...)`. Pass an
empty list to clear a field — e.g. clear any existing metadata while setting tags:

```{R}

gateaux_annotate_job(report_name = "MyReport",
                     job_id = "123456789",
                     JWT = JWT,
                     tags = list(state = "reviewed"),
                     metadata = list())
```

