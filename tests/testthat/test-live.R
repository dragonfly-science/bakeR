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
    job <- gateaux_job(jobs$id[1], l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
    expect_equal(as.integer(job$id), jobs$id[1])
    expect_true(is.list(job$links))
    outputs <- gateaux_job_outputs(jobs$id[1], l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
    expect_s3_class(outputs, "data.frame")
  }
  dag <- gateaux_report_dag(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true(any(vapply(dag$nodes, function(n) n$code == l$GATEAUX_TEST_REPORT, logical(1))))
})

test_that("jobs can be listed across reports and the session read", {
  l <- live()
  all <- gateaux_jobs(NULL, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, perpage = 5)
  expect_true("report" %in% names(all))
  running <- gateaux_jobs(NULL, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, status = c("Running", "Pending"))
  expect_true(all(running$status %in% c("Running", "Pending")))
  me <- gateaux_session(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_equal(me$principal, "user")
  expect_true(is.numeric(me$user$id))
})

test_that("errors arrive as R errors with the server's code", {
  l <- live()
  expect_error(gateaux_job(1, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), "NOT_FOUND")
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
  final <- gateaux_wait_for_jobs(submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, timeout = 300, quiet = TRUE)
  expect_equal(final$status, "Success")
  log <- gateaux_job_log(submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true(any(grepl("Starting job", log)))
  streamed <- gateaux_stream_log(submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, callback = function(line) NULL)
  expect_true(any(grepl("Starting job", streamed)))
  listed <- gateaux_jobs(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, tag = tag)
  expect_equal(listed$id, submitted$id)
  files <- gateaux_download_output(l$GATEAUX_TEST_REPORT, submitted$id, l$GATEAUX_TEST_JWT, server = l$GATEAUX_TEST_SERVER, dest = "out")
  expect_true(all(file.exists(files$path)))
  cancelled <- gateaux_cancel_job(submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_equal(cancelled$status, "Success")
  annotated <- gateaux_annotate_job(l$GATEAUX_TEST_REPORT, submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER,
                                    tags = list(tag = tag, reviewed = "yes"), metadata = list(note = "bakeR"))
  expect_equal(annotated$tags$reviewed, "yes")
  expect_equal(annotated$metadata$note, "bakeR")
  expect_equal(annotated$triggered_by$id, gateaux_session(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)$user$id)
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
  expect_error(gateaux_tokens(l$GATEAUX_TEST_REPORT, tok, l$GATEAUX_TEST_SERVER), "FORBIDDEN")
})

test_that("service tokens can be listed, rotated and revoked", {
  l <- live()
  tok <- gateaux_mint_token(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER,
                            name = "bakeR-rotate", scopes = "jobs:read", expires_in_days = 1)
  id <- attr(tok, "details")$id
  expect_type(id, "character")
  listed <- gateaux_tokens(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_s3_class(listed, "data.frame")
  expect_equal(listed$status[listed$id == id], "active")
  expect_equal(listed$scopes[listed$id == id], "jobs:read")

  new <- gateaux_rotate_token(l$GATEAUX_TEST_REPORT, id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_equal(attr(new, "details")$replaces, id)
  expect_equal(unlist(attr(new, "details")$scopes), "jobs:read")
  expect_error(gateaux_jobs(l$GATEAUX_TEST_REPORT, tok, l$GATEAUX_TEST_SERVER, perpage = 1), "TOKEN_REVOKED")
  expect_s3_class(gateaux_jobs(l$GATEAUX_TEST_REPORT, new, l$GATEAUX_TEST_SERVER, perpage = 1), "data.frame")
  expect_error(gateaux_rotate_token(l$GATEAUX_TEST_REPORT, id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), "TOKEN_REVOKED")

  new_id <- attr(new, "details")$id
  revoked <- gateaux_revoke_token(l$GATEAUX_TEST_REPORT, new_id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_equal(revoked$status, "revoked")
  expect_error(gateaux_jobs(l$GATEAUX_TEST_REPORT, new, l$GATEAUX_TEST_SERVER, perpage = 1), "TOKEN_REVOKED")
  listed <- gateaux_tokens(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_equal(listed$replaced_by[listed$id == id], new_id)
  expect_true(all(listed$status[listed$id %in% c(id, new_id)] == "revoked"))
  expect_error(gateaux_revoke_token(l$GATEAUX_TEST_REPORT, "no-such-token", l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), "NOT_FOUND")
})

test_that("reports can be listed, created, changed, given secrets and archived", {
  l <- live()
  if (!nzchar(Sys.getenv("GATEAUX_TEST_SUBMIT"))) skip("GATEAUX_TEST_SUBMIT not set (this test creates a report)")
  reports <- gateaux_reports(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, perpage = 5)
  expect_s3_class(reports, "data.frame")
  expect_true(is.numeric(attr(reports, "total")))
  base <- gateaux_report(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  code <- paste0("bakeR-crud-", format(Sys.time(), "%H%M%S"))
  created <- gateaux_create_report(code, base$repository, base$branch, base$directory,
                                   l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, description = "bakeR test")
  expect_equal(created$code, code)
  updated <- gateaux_update_report(code, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, description = "changed")
  expect_equal(updated$description, "changed")
  expect_equal(updated$branch, base$branch)
  expect_equal(gateaux_secrets(code, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), character(0))
  expect_equal(gateaux_set_secret(code, "TEST_SECRET", "value", l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), "TEST_SECRET")
  expect_false("value" %in% unlist(gateaux_report(code, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)$secrets))
  expect_equal(gateaux_delete_secret(code, "TEST_SECRET", l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), character(0))
  expect_equal(gateaux_delete_report(code, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), code)
  expect_error(gateaux_report(code, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER), "NOT_FOUND")
})

test_that("runners, repositories and teams can be read", {
  l <- live()
  runners <- gateaux_runners(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_s3_class(runners, "data.frame")
  expect_s3_class(attr(runners, "running"), "data.frame")
  queues <- gateaux_queues(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true("default" %in% queues$name)
  keys <- gateaux_runner_keys(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_s3_class(keys, "data.frame")
  repos <- gateaux_repositories(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true(nrow(repos) > 0)
  expect_true(nzchar(attr(repos, "synced_at")))
  base <- gateaux_report(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  branches <- gateaux_branches(base$repository, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true(base$branch %in% branches$name)
  tree <- gateaux_tree(base$repository, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, branch = base$branch)
  expect_true(any(startsWith(tree$path, base$directory)))
  expect_true(gateaux_sync(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER))
  teams <- gateaux_teams(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true(all(c("id", "org", "name", "slug", "members") %in% names(teams)))
  members <- attr(teams, "members")
  expect_true(all(c("team_id", "github_login") %in% names(members)))
})

test_that("thumbnails, zipped outputs and event streams work", {
  l <- live()
  if (!nzchar(Sys.getenv("GATEAUX_TEST_SUBMIT"))) skip("GATEAUX_TEST_SUBMIT not set")
  dir <- withr::local_tempdir()
  png <- file.path(dir, "t.png")
  # an 8x8 orange PNG
  writeBin(jsonlite::base64_dec("iVBORw0KGgoAAAANSUhEUgAAAAgAAAAICAIAAABLbSncAAAAEklEQVR4nGP4n8KAFWEXHbQSAO2GWMEZtBE8AAAAAElFTkSuQmCC"), png)
  report <- gateaux_set_thumbnail(l$GATEAUX_TEST_REPORT, png, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_true(nzchar(report$thumbnail_url))
  cleared <- gateaux_delete_thumbnail(l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER)
  expect_null(cleared$thumbnail_url)

  tag <- paste0("bakeR-stream-", format(Sys.time(), "%H%M%S"))
  submitted <- gateaux_job_runner(setNames(list(list()), tag), l$GATEAUX_TEST_REPORT, l$GATEAUX_TEST_SERVER, l$GATEAUX_TEST_JWT, log_jobs = FALSE)
  # a quick job can finish before the stream is connected, so the stream may
  # or may not carry its events; the job's final state is what must hold
  seen <- gateaux_stream_events(l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, callback = function(job) NULL,
                                until = function(job) job$id == submitted$id && job$status %in% c("Success", "Failed", "Invalid", "Cancelled"),
                                timeout = 30)
  expect_true(is.list(seen))
  expect_true(all(vapply(seen, function(j) is.numeric(j$id) && is.character(j$status), logical(1))))
  expect_equal(gateaux_wait_for_jobs(submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, quiet = TRUE)$status, "Success")
  final <- gateaux_stream_job(submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, callback = function(job) NULL, timeout = 60)
  expect_equal(final[[length(final)]]$status, "Success")
  zip <- gateaux_download_outputs_zip(submitted$id, l$GATEAUX_TEST_JWT, l$GATEAUX_TEST_SERVER, dest = file.path(dir, "out.zip"))
  expect_true(file.exists(zip) && file.info(zip)$size > 0)
})
