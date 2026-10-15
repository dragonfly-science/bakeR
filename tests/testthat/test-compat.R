# Offline tests of what the package sends: gateaux_request() is replaced with
# a stand-in that records each call and answers with canned records. They
# pin down the bakeR 0.2 argument conventions (gateaux_list_jobs, the report
# first in gateaux_annotate_job, run_all_with) and the legacy route.

job_record <- function(id = 101, report = "MyReport", tags = list(), env = list(TAG = "Run1")) {
  list(id = id, report = report, status = "Success", tags = tags, metadata = list(),
       parameters = list(env = env, cpus = 2), triggered_by = list(github_login = "alice"))
}

# Run `code` with gateaux_request() mocked; `reply` maps (method, path) to a response.
with_requests <- function(reply, code) {
  calls <- list()
  local_mocked_bindings(gateaux_request = function(method, path, JWT, server = "gateaux.io", body = NULL,
                                                   query = NULL, headers = character(), parse = TRUE) {
    calls[[length(calls) + 1]] <<- list(method = method, path = path, server = server, body = body, query = query)
    reply(method, path, body)
  }, .package = "bakeR")
  result <- code()
  list(calls = calls, result = result)
}

test_that("gateaux_list_jobs pages, filters and matches tags through the v2 listing", {
  run <- with_requests(function(method, path, body) list(results = list(job_record(102, tags = list(series = "XYZ")), job_record(101)), total = 2),
    function() gateaux_list_jobs("MyReport", JWT = "tok", page = 0:1, filter = "re run", tags = list(series = "XYZ")))
  expect_length(run$calls, 2)
  expect_equal(run$calls[[1]]$method, "GET")
  expect_equal(run$calls[[1]]$path, "/api/v2/reports/MyReport/jobs")
  expect_equal(run$calls[[1]]$query$page, 0)
  expect_equal(run$calls[[2]]$query$page, 1)
  expect_equal(run$calls[[1]]$query$filter, "re run")
  expect_equal(run$calls[[1]]$query[["tag.series"]], "XYZ")
  expect_s3_class(run$result, "data.frame")
  expect_equal(nrow(run$result), 4)
  expect_equal(run$result$id[1:2], c(102L, 101L))
  expect_equal(run$result$tags.series[1], "XYZ")
  expect_true(is.na(run$result$tags.series[2]))
  expect_equal(run$result$env.TAG[1], "Run1")
})

test_that("gateaux_list_jobs with nothing found is an empty data frame", {
  run <- with_requests(function(...) list(results = list(), total = 0),
    function() gateaux_list_jobs("MyReport", JWT = "tok"))
  expect_s3_class(run$result, "data.frame")
  expect_equal(nrow(run$result), 0)
})

test_that("gateaux_annotate_job takes the report first, checks it, and clears with empty objects", {
  reply <- function(method, path, body) {
    if (method == "GET") return(job_record(101, report = "MyReport"))
    rec <- job_record(101)
    rec$tags <- body$tags
    rec$metadata <- body$metadata
    rec
  }
  run <- with_requests(reply, function() list(
    set   = gateaux_annotate_job("MyReport", 101, JWT = "tok", tags = list(state = "reviewed"), metadata = list(rmse = 0.4)),
    clear = gateaux_annotate_job("MyReport", 101, JWT = "tok", tags = list(), metadata = list()),
    str   = gateaux_annotate_job("MyReport", 101, JWT = "tok", metadata = '{"a": 1}')
  ))
  patches <- Filter(function(c) c$method == "PATCH", run$calls)
  expect_length(patches, 3)
  expect_equal(patches[[1]]$path, "/api/v2/jobs/101")
  expect_equal(patches[[1]]$body, list(tags = list(state = "reviewed"), metadata = list(rmse = 0.4)))
  # empty lists must encode as {} not []
  expect_equal(as.character(jsonlite::toJSON(patches[[2]]$body, auto_unbox = TRUE)), '{"tags":{},"metadata":{}}')
  expect_equal(patches[[3]]$body, list(metadata = list(a = 1L)))
  expect_equal(run$result$set$tags$state, "reviewed")

  expect_error(gateaux_annotate_job("MyReport", 101, JWT = "tok"), "at least one")
  local_mocked_bindings(gateaux_request = function(method, path, JWT, server = "gateaux.io", body = NULL,
                                                   query = NULL, headers = character(), parse = TRUE) {
    job_record(101, report = "OtherReport")
  })
  expect_error(gateaux_annotate_job("MyReport", 101, JWT = "tok", tags = list(a = "b")), "belongs to report OtherReport")
})

test_that("gateaux_job_runner posts to the report's v2 jobs route, or the legacy /api/job route", {
  pars <- list(Run1 = list(pars = list(Env1 = "a"), wants = list(up = c(1, 2)), requires = list(must = 3)))
  v2 <- with_requests(function(method, path, body) list(results = list(job_record())),
    function() gateaux_job_runner(pars, "MyReport", JWT = "tok", log_jobs = FALSE, cpus = 4, memory = 8))
  expect_equal(v2$calls[[1]]$method, "POST")
  expect_equal(v2$calls[[1]]$path, "/api/v2/reports/MyReport/jobs")
  expect_equal(v2$calls[[1]]$body$env, list(TAG = "Run1", Env1 = "a"))
  expect_equal(v2$calls[[1]]$body$cpus, 4)
  expect_equal(v2$calls[[1]]$body$memory, 8)
  expect_equal(v2$result$id, 101L)

  v1 <- with_requests(function(method, path, body) list(job_record()),
    function() gateaux_job_runner(pars, "MyReport", JWT = "tok", server = "kahawai.io", log_jobs = FALSE, api = "v1"))
  expect_equal(v1$calls[[1]]$path, "/api/job/MyReport")
  expect_equal(v1$calls[[1]]$server, "kahawai.io")
  expect_equal(v1$result$id, 101L)
})

test_that("run_all_with sets, vectorises and removes", {
  pars <- list(Run1 = list(pars = list(Env1 = "a")), Run2 = list(pars = list(Env1 = "b")))
  set <- run_all_with(pars, "DEBUG", "yes")
  expect_equal(set$Run1$pars$DEBUG, "yes")
  expect_equal(set$Run2$pars$Env1, "b")
  two <- run_all_with(pars, c("X", "Y"), c("1", "2"))
  expect_equal(two$Run1$pars$Y, "2")
  wants <- run_all_with(pars, "Up", 123, set = "wants")
  expect_equal(wants$Run1$wants[["Up"]], 123)
  gone <- run_all_with(pars, "Env1", NULL, rm_vals = TRUE)
  expect_null(gone$Run1$pars)
})

test_that("tokens are masked in anything that could be printed", {
  expect_equal(bakeR:::.redact('curl -H "Authorization: Bearer abc.def" https://x/?jwt=zzz&p=1'),
               'curl -H "Authorization: Bearer ***" https://x/?jwt=***&p=1')
  # a server error whose message quotes a token (httr's accessors are stood in for)
  local_mocked_bindings(status_code = function(x) 400L,
                        content = function(x, ...) '{"error":{"code":"BAD","message":"got Bearer abc"}}',
                        .package = "httr")
  expect_error(bakeR:::gateaux_check(list()), "Bearer \\*\\*\\*")
  expect_error(bakeR:::gateaux_check(list()), "gateaux BAD \\(HTTP 400\\)")
  # a connection failure on the legacy download link (mock before changing
  # directory: testthat finds the package from the working directory)
  local_mocked_bindings(gateaux_request = function(method, path, JWT, server = "gateaux.io", body = NULL,
                                                   query = NULL, headers = character(), parse = TRUE) {
    list(results = list(list(id = 7, files_url = "http://127.0.0.1:1/file/x?jwt=SECRET")))
  }, .package = "bakeR")
  withr::local_dir(withr::local_tempdir())
  err <- tryCatch(suppressMessages(gateaux_download_output("MyReport", 7, JWT = "tok", page_no = 0, api = "v1")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "connect", ignore.case = TRUE)
  expect_no_match(err, "SECRET", fixed = TRUE)
})
