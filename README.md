# bakeR

R package for driving [Gateaux](https://gateaux.io) jobs: submit, list, wait
for, cancel, and fetch logs and outputs.

## Install

```r
remotes::install_github("dragonfly-science/bakeR")
```

## Authentication

Every call takes your API key as `JWT`. Get it from the **API guide** page of
the Gateaux web UI (the user menu, top right); it is valid for 30 days. Keep it
out of version control, for example in a `.Renviron` file:

```r
JWT <- Sys.getenv("GATEAUX_KEY")
```

Every function also takes a `server` argument, which defaults to `gateaux.io`.
Pass `server = "kahawai.io"` to talk to Kahawai.

For CI, mint a token that acts as you but is limited to one report and a
set of scopes, and put that in the pipeline instead of your personal key:

```r
ci_token <- gateaux_mint_token("bakeR-testreport", JWT, name = "ci",
                               scopes = c("jobs:submit", "jobs:read", "logs:read", "outputs:read"),
                               expires_in_days = 90)
```

Tokens can be listed, rotated and revoked. Rotation mints a replacement
with the same name and scopes and retires the old one, optionally after a
grace period so a pipeline run in flight can finish:

```r
gateaux_tokens("bakeR-testreport", JWT)                       # id, name, status, scopes, ...
new_token <- gateaux_rotate_token("bakeR-testreport", attr(ci_token, "details")$id, JWT,
                                  grace_seconds = 600)
gateaux_revoke_token("bakeR-testreport", attr(new_token, "details")$id, JWT)
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
gateaux_wait_for_jobs(jobs$id, JWT)                           # long-polls until finished
gateaux_jobs("bakeR-testreport", JWT, status = "Running")    # server-side filters
gateaux_jobs(NULL, JWT, status = c("Running", "Pending"))     # across every report you can see
gateaux_job_log(jobs$id[1], JWT)                              # character vector of lines
gateaux_stream_log(jobs$id[1], JWT)                           # prints lines live until the job stops
gateaux_job_outputs(jobs$id[1], JWT)                          # names, sizes, checksums
gateaux_download_output("bakeR-testreport", jobs$id, JWT)     # into output/<report>-<id>/
gateaux_cancel_job(jobs$id[1], JWT)
gateaux_annotate_job("bakeR-testreport", jobs$id[1], JWT, tags = list(reviewed = "yes"))  # tags and metadata
gateaux_list_jobs("bakeR-testreport", JWT, tags = list(reviewed = "yes"))   # find them again
gateaux_job_dag(jobs$id[1], JWT)                              # upstream/downstream jobs
gateaux_report_dag("bakeR-testreport", JWT)                   # declared pipeline
```

Job ids are global, so the single-job helpers take just the id;
`gateaux_annotate_job()` keeps the report as its first argument, as in bakeR 0.2.

## Reports and secrets

```r
gateaux_reports(JWT, filter = "psc")                                  # what you can see
gateaux_create_report("my-report", "dragonfly-science/repo", "main", "reports/my", JWT)
gateaux_update_report("my-report", JWT, description = "Nightly run")
gateaux_set_secret("my-report", "API_TOKEN", token, JWT)             # names only ever come back
gateaux_secrets("my-report", JWT)
gateaux_delete_report("my-report", JWT)                               # archives it
```

## Everything else the web UI can do

```r
gateaux_runners(JWT); gateaux_queues(JWT)                   # connected runners and their queues
gateaux_runner_keys(JWT); gateaux_add_runner_key("ci", team_id, "key.pem", JWT)
gateaux_repositories(JWT); gateaux_branches("dragonfly-science/repo", JWT); gateaux_tree("dragonfly-science/repo", JWT)
gateaux_sync(JWT)                                             # re-sync from GitHub now
gateaux_teams(JWT)                                            # teams, members in attr(, "members")
gateaux_set_thumbnail("my-report", "cover.png", JWT); gateaux_delete_thumbnail("my-report", JWT)
gateaux_download_outputs_zip(jobs$id[1], JWT)
gateaux_stream_events(JWT, until = function(job) job$status == "Failed")   # follow every job change
gateaux_stream_job(jobs$id[1], JWT)                           # follow one job to the end
```

## Upgrading from bakeR 0.2

Everything in 0.2 still works with the same arguments: `gateaux_job_runner()`,
`gateaux_download_output()`, `gateaux_list_jobs()` (with `page`, `filter` and
`tags`), `gateaux_annotate_job()` and `run_all_with()`. Two things differ:

- Job data frames have the v2 shape described under [gateaux_jobs_frame()]:
  `id`, `report`, `status`, `tag`, `triggered_by`, timings, then `env.<NAME>`
  and `tags.<key>` columns. The legacy `files_url` column, which carried a
  download token, is gone; use `gateaux_download_output()`.
- `gateaux_job_runner()` returns a data frame of the submitted jobs rather than
  the raw JSON.

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

## Development

The unit tests run offline (`devtools::test()`): the request function is
replaced with a stand-in, so they check what the package sends and how it
reads the answers. The live tests in `tests/testthat/test-live.R` run against
a real server when `GATEAUX_TEST_SERVER`, `GATEAUX_TEST_JWT` and
`GATEAUX_TEST_REPORT` are set, and submit jobs when `GATEAUX_TEST_SUBMIT=1`
as well. A pre-commit hook regenerates the documentation and runs the unit
tests (not the live ones); enable it once per clone with

```sh
git config core.hooksPath hooks
```
