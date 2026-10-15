# bakeR
R package for calling gateaux API from R

## Install

```r
remotes::install_github("dragonfly-science/bakeR")
```


## Your API key

Every call takes your API key as `JWT`. Get it from the **API guide** page of
the Gateaux web UI (the user menu, top right), and keep it out of version
control; for example put `GATEAUX_KEY=...` in a `.Renviron` file and read it
with `JWT <- Sys.getenv("GATEAUX_KEY")`. The key is valid for 30 days.

Every function also takes a `server` argument, which defaults to `gateaux.io`.
Pass `server = "kahawai.io"` to talk to Kahawai.

## Launching jobs

Define the parameter list for each run, and launch! Each element's name is
attached to its job as the `TAG` environment variable.

```r
pars <- list(Run1 = list(pars = list(Env1 = "This_env",
                                     Env2 = "That_env")),
             Run2 = list(pars = list(Env1 = "That_env",
                                     Env2 = "This_env")))

gateaux_job_runner(pars,
                   report_name = "bakeR-testreport",
                   JWT = JWT)
```

By default the details of each submitted job are also appended to
`jobs-joblist.csv` in the working directory (`log_jobs = FALSE` turns that
off), and `cpus` and `memory` override the report's `gateaux.yaml`.

`wants` and `requires` dependencies can be added like so, where the numbers
are job ids:

```r
pars <- list(Run1 = list(pars = list(Env1 = "This_env",
                                     Env2 = "That_env"),
                         wants = list(Upstream_job = 1234),
                         requires = list(Upstream_job2 = 4321)),
             Run2 = list(pars = list(Env1 = "That_env",
                                     Env2 = "This_env"),
                         wants = list(Upstream_job = c(2222, 5555, 6666)),
                         requires = list(Upstream_job2 = c(3333, 2244, 111))))
```

To run every job in a list with an extra or changed setting, without editing
the list by hand:

```r
pars_debug <- run_all_with(pars, "DEBUG", "yes")
```

## Listing jobs

List the jobs for a report (most recent first) as a tidy data.frame:

```r
jobs <- gateaux_list_jobs(report_name = "MyReport", JWT = JWT)
```

Page through results (pages are zero-based), or filter by a word in the job
details:

```r
# fetch the first three result pages
jobs <- gateaux_list_jobs(report_name = "MyReport", JWT = JWT, page = 0:2)

# only jobs whose details mention "rerun"
jobs <- gateaux_list_jobs(report_name = "MyReport", JWT = JWT, filter = "rerun")
```

To match jobs by attached tags (see "Annotating jobs" below), pass a named list
of tag key-value pairs. This is sent as a POST request, as the API requires for
tag matching:

```r
jobs <- gateaux_list_jobs(report_name = "MyReport", JWT = JWT,
                          tags = list(author = "bob"))
```

## Annotating jobs

Attach searchable tags (key-value pairs) and/or arbitrary metadata to a job:

```r
gateaux_annotate_job(report_name = "MyReport",
                     job_id = "123456789",
                     JWT = JWT,
                     tags = list(state = "unreviewed", series = "XYZ123"))
```

Tags and metadata are returned in the job list response, so you can annotate a
job and then find it again with `gateaux_list_jobs(..., tags = ...)`. Pass an
empty list to clear a field, for example to clear any existing metadata while
setting tags:

```r
gateaux_annotate_job(report_name = "MyReport",
                     job_id = "123456789",
                     JWT = JWT,
                     tags = list(state = "reviewed"),
                     metadata = list())
```

## Downloading outputs

Download the output files of one or more jobs, each as a zip archive, into an
`output/` directory under the working directory:

```r
gateaux_download_output(report_name = "MyReport",
                        report_id = c(123456789, 123456790),
                        JWT = JWT)
```

The jobs are looked up in the report's job list, so for older jobs widen the
pages searched with `page_no`, for example `page_no = 0:5`.

## Development

The tests run offline (`devtools::test()`): every call to curl is replaced
with a stand-in, so they check the requests the package builds and how it
reads the answers, not the servers. A pre-commit hook regenerates the
documentation and runs them; enable it once per clone with

```sh
git config core.hooksPath hooks
```
