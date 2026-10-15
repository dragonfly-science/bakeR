# bakeR

R package for driving [Gateaux](https://gateaux.io) jobs: submit, list, wait
for, cancel, and fetch logs and outputs.

## Install

```r
remotes::install_github("dragonfly-science/bakeR")
```

## Authentication

Every call takes a `JWT`. Log in to the Gateaux web UI and visit
`https://gateaux.io/auth/key` to get a user API key (valid 30 days), then

```r
JWT <- "my_token_goes_here"
```

For CI, mint a token that acts as you but is limited to one report and a
set of scopes, and put that in the pipeline instead of your personal key:

```r
ci_token <- gateaux_mint_token("bakeR-testreport", JWT, name = "ci",
                               scopes = c("jobs:submit", "jobs:read", "logs:read", "outputs:read"),
                               expires_in_days = 90)
```

## Submitting jobs

Define one named list per job (the name becomes the job's tag), and launch:

```r
pars <- list(Run1 = list(pars = list(Env1 = "This_env", Env2 = "That_env")),
             Run2 = list(pars = list(Env1 = "That_env", Env2 = "This_env")))

jobs <- gateaux_job_runner(pars, report_name = "bakeR-testreport", JWT = JWT)
jobs$id
```

`wants` and `requires` name upstream reports, optionally pinned to job ids:

```r
pars <- list(Run1 = list(pars = list(Env1 = "This_env"),
                         wants = list(upstream_report = 1234),
                         requires = list(other_report = c(3333, 2244))))
```

The tag is sent both as the `TAG` environment variable (as before) and as the
job's `tag` annotation, so `gateaux_jobs(..., tag = "Run1")` finds it.

## Following jobs

```r
gateaux_wait_for_jobs("bakeR-testreport", jobs$id, JWT)      # long-polls until finished
gateaux_jobs("bakeR-testreport", JWT, status = "Running")    # server-side filters
gateaux_job_log("bakeR-testreport", jobs$id[1], JWT)          # character vector of lines
gateaux_stream_log("bakeR-testreport", jobs$id[1], JWT)       # prints lines live until the job stops
gateaux_job_outputs("bakeR-testreport", jobs$id[1], JWT)      # names, sizes, checksums
gateaux_download_output("bakeR-testreport", jobs$id, JWT)     # into output/<report>-<id>/
gateaux_cancel_job("bakeR-testreport", jobs$id[1], JWT)
gateaux_job_dag("bakeR-testreport", jobs$id[1], JWT)          # upstream/downstream jobs
gateaux_report_dag("bakeR-testreport", JWT)                   # declared pipeline
```

## Reports and secrets

```r
gateaux_reports(JWT, filter = "psc")                                  # what you can see
gateaux_create_report("my-report", "dragonfly-science/repo", "main", "reports/my", JWT)
gateaux_update_report("my-report", JWT, description = "Nightly run")
gateaux_set_secret("my-report", "API_TOKEN", token, JWT)             # names only ever come back
gateaux_secrets("my-report", JWT)
gateaux_delete_report("my-report", JWT)                               # archives it
```

## Which API

bakeR uses the Gateaux v2 API (`/api/v2`, documented at
`https://gateaux.io/api/v2/swagger-ui`) by default. The two original
functions, `gateaux_job_runner()` and `gateaux_download_output()`, can still
use the legacy routes:

```r
gateaux_job_runner(pars, "bakeR-testreport", JWT = JWT, api = "v1")
options(bakeR.api = "v1")   # for a whole session
```

The other helpers are v2 only, since the legacy API has no API-key access to
logs, cancellation or outputs.

To talk to a local development server pass a full base URL as `server`:

```r
gateaux_jobs("parent", JWT, server = "http://127.0.0.1:7444")
```
