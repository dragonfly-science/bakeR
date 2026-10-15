# Live tests against a running Gateaux server. Skipped unless
# GATEAUX_TEST_SERVER, GATEAUX_TEST_JWT and GATEAUX_TEST_REPORT are set, e.g.
#   GATEAUX_TEST_SERVER=http://127.0.0.1:7444 GATEAUX_TEST_JWT=... GATEAUX_TEST_REPORT=parent
# GATEAUX_TEST_SUBMIT=1 additionally submits a job and waits for it.

live <- function() {
  vars <- Sys.getenv(c("GATEAUX_TEST_SERVER", "GATEAUX_TEST_JWT", "GATEAUX_TEST_REPORT"))
  if (any(!nzchar(vars))) skip("GATEAUX_TEST_* not set")
  as.list(vars)
}

test_that("jobs can be listed and read", {
  l <- live()
  jobs <- gateaux_jobs(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, perpage = 5)
  expect_s3_class(jobs, "data.frame")
  expect_true(is.numeric(attr(jobs, "total")))
  if (nrow(jobs) > 0) {
    job <- gateaux_job(l$GATEAUX_TEST_REPORT, jobs$id[1], l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
    expect_equal(as.integer(job$id), jobs$id[1])
    expect_true(is.list(job$links))
    outputs <- gateaux_job_outputs(l$GATEAUX_TEST_REPORT, jobs$id[1], l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
    expect_s3_class(outputs, "data.frame")
  }
  dag <- gateaux_report_dag(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true(any(vapply(dag$nodes, function(n) n$code == l$GATEAUX_TEST_REPORT, logical(1))))
})

test_that("errors arrive as R errors with the server's code", {
  l <- live()
  expect_error(gateaux_job(l$GATEAUX_TEST_REPORT, 1, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), "NOT_FOUND")
  expect_error(gateaux_jobs(l$GATEAUX_TEST_REPORT, "not-a-token", l$GATEAUX_TEST_SERVER), "UNAUTHENTICATED")
})

test_that("a job can be submitted, waited for, logged and downloaded", {
  l <- live()
  if (!nzchar(Sys.getenv("GATEAUX_TEST_SUBMIT"))) skip("GATEAUX_TEST_SUBMIT not set")
  tag <- paste0("bakeR-test-", format(Sys.time(), "%H%M%S"))
  pars <- setNames(list(list(pars = list(BAKER_TEST = "1"))), tag)
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  submitted <- gateaux_job_runner(pars, l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_SERVER, l$GATEAUX_TEST_JWT, prefix = "test")
  expect_equal(nrow(submitted), 1)
  expect_equal(submitted$tag, tag)
  expect_true(file.exists("test-joblist.csv"))
  final <- gateaux_wait_for_jobs(l$GATEAUX_TEST_REPORT, submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, timeout = 300, quiet = TRUE)
  expect_equal(final$status, "Success")
  log <- gateaux_job_log(l$GATEAUX_TEST_REPORT, submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true(any(grepl("Starting job", log)))
  streamed <- gateaux_stream_log(l$GATEAUX_TEST_REPORT, submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, callback = function(line) NULL)
  expect_true(any(grepl("Starting job", streamed)))
  listed <- gateaux_jobs(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, tag = tag)
  expect_equal(listed$id, submitted$id)
  files <- gateaux_download_output(l$GATEAUX_TEST_REPORT, submitted$id, l$GATEAUX_TEST_JWT, server = l$GATEAUX_TEST_SERVER, dest = "out")
  expect_true(all(file.exists(files$path)))
  cancelled <- gateaux_cancel_job(l$GATEAUX_TEST_REPORT, submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_equal(cancelled$status, "Success")
})

test_that("a scoped service token is limited to its report and scopes", {
  l <- live()
  tok <- gateaux_mint_token(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER,
                            name = "bakeR-test", scopes = c("jobs:read", "logs:read"), expires_in_days = 1)
  expect_type(tok, "character")
  expect_equal(sort(unlist(attr(tok, "details")$scopes)), c("jobs:read", "logs:read"))
  jobs <- gateaux_jobs(l$GATEAUX_TEST_REPORT, tok, l$GATEAUX_TEST_SERVER, perpage = 1)
  expect_s3_class(jobs, "data.frame")
  expect_error(gateaux_jobs("some-other-report", tok, l$GATEAUX_TEST_SERVER), "FORBIDDEN")
  expect_error(gateaux_job_runner(list(t = list()), l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_SERVER, tok, log_jobs = FALSE), "INSUFFICIENT_SCOPE")
  expect_error(gateaux_mint_token(l$GATEAUX_TEST_REPORT, tok, l$GATEAUX_TEST_SERVER), "FORBIDDEN")
})
