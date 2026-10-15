# Offline tests: every function shells out to curl through .system(), which
# is replaced here with a stand-in that records the command and answers with
# canned JSON. Nothing touches the network.

job_object <- function(id = 101, tags = list(state = "unreviewed"), metadata = list()) {
  list(
    id = id, status = "Pending", report = "MyReport",
    files_url = sprintf("https://gateaux.io/file/MyReport/%s?jwt=SECRET", id),
    parameters = list(env = list(TAG = "Run1", Env1 = "a"), variant = list(), cpus = 2),
    tags = tags, metadata = metadata
  )
}

# the submit route answers with a list of jobs, one per variant; a new job
# has no tags or metadata yet (JSON null)
canned_jobs <- function(id = 101) {
  jsonlite::toJSON(list(job_object(id, tags = NA, metadata = NA)), auto_unbox = TRUE, na = "null")
}

# the annotate route answers with the one job
canned_job <- function(id = 101, tags = list(state = "unreviewed")) {
  jsonlite::toJSON(job_object(id, tags), auto_unbox = TRUE)
}

canned_list <- function(ids = c(102, 101)) {
  jsonlite::toJSON(list(
    results = lapply(ids, function(id) list(
      id = id, status = "Success", files_url = sprintf("https://gateaux.io/file/MyReport/%s?jwt=SECRET", id),
      tags = list(series = "XYZ"), metadata = list(n = 1)
    )),
    total = length(ids), page = 0
  ), auto_unbox = TRUE)
}

# Record the curl commands and answer with `reply` (a function of the command).
with_curl <- function(reply, code) {
  calls <- character()
  local_mocked_bindings(.system = function(command, intern = TRUE) {
    calls <<- c(calls, command)
    if (!intern) return(system(command))
    out <- reply(command)
    attr(out, "status") <- 0L
    out
  }, .env = rlang::caller_env())
  printed <- capture.output(result <- code())
  list(calls = calls, result = result, printed = printed)
}

# the JSON body of a curl command (-d '...'), parsed
body_of <- function(command) {
  m <- regmatches(command, regexpr("-d '.*' ", command))
  jsonlite::fromJSON(substr(m, 5, nchar(m) - 2), simplifyVector = FALSE)
}

test_that("gateaux_job_runner posts to /api/job/<report> on gateaux.io by default", {
  run <- with_curl(function(cmd) canned_jobs(), function() {
    gateaux_job_runner(list(Run1 = list(pars = list(Env1 = "a", Env2 = "b"))),
                       report_name = "MyReport", JWT = "tok", log_jobs = FALSE)
  })
  expect_length(run$calls, 1)
  expect_match(run$calls, "curl -X POST", fixed = TRUE)
  expect_match(run$calls, "https://gateaux.io/api/job/MyReport", fixed = TRUE)
  expect_match(run$calls, 'Authorization: Bearer tok', fixed = TRUE)
  body <- body_of(run$calls)
  expect_equal(body$env, list(TAG = "Run1", Env1 = "a", Env2 = "b"))
  expect_equal(run$result[[1]]$id, 101)
})

test_that("gateaux_job_runner carries wants, requires, cpus, memory and the server", {
  pars <- list(Run1 = list(pars = list(Env1 = "a"),
                           wants = list(Up = c(1, 2)),
                           requires = list(Must = 3)),
               Run2 = list(pars = list()))
  run <- with_curl(function(cmd) canned_jobs(), function() {
    gateaux_job_runner(pars, report_name = "MyReport", JWT = "tok", server = "kahawai.io",
                       log_jobs = FALSE, cpus = 4, memory = 8)
  })
  expect_length(run$calls, 2)
  expect_match(run$calls[1], "https://kahawai.io/api/job/MyReport", fixed = TRUE)
  b1 <- body_of(run$calls[1])
  expect_equal(b1$cpus, 4)
  expect_equal(b1$memory, 8)
  expect_equal(unlist(b1$wants), c("Up:1", "Up:2"))
  expect_equal(unlist(b1$requires), "Must:3")
  b2 <- body_of(run$calls[2])
  expect_equal(b2$env, list(TAG = "Run2"))
  expect_null(b2$wants)
})

test_that("gateaux_job_runner writes the job list CSV when asked", {
  withr::local_dir(withr::local_tempdir())
  run <- with_curl(function(cmd) canned_jobs(), function() {
    gateaux_job_runner(list(Run1 = list(pars = list(Env1 = "a"))),
                       report_name = "MyReport", JWT = "tok", prefix = "jobs", append = FALSE)
  })
  expect_true(file.exists("jobs-joblist.csv"))
  csv <- read.csv("jobs-joblist.csv")
  expect_equal(csv$id, 101)
  expect_equal(csv$Env1, "a")
  expect_false("variant" %in% names(csv))
})

test_that("gateaux_list_jobs uses GET with a trailing slash, and POST when tags are given", {
  run <- with_curl(function(cmd) canned_list(), function() {
    list(
      plain = gateaux_list_jobs("MyReport", JWT = "tok"),
      paged = gateaux_list_jobs("MyReport", JWT = "tok", page = 0:1, filter = "re run"),
      tagged = gateaux_list_jobs("MyReport", JWT = "tok", tags = list(series = "XYZ"), server = "kahawai.io")
    )
  })
  expect_length(run$calls, 4)
  expect_match(run$calls[1], '"https://gateaux.io/api/jobs/MyReport/?page=0"', fixed = TRUE)
  expect_no_match(run$calls[1], "-X POST", fixed = TRUE)
  expect_match(run$calls[2], "?page=0&filter=re%20run", fixed = TRUE)
  expect_match(run$calls[3], "?page=1&filter=re%20run", fixed = TRUE)
  expect_match(run$calls[4], "-X POST", fixed = TRUE)
  expect_match(run$calls[4], "https://kahawai.io/api/jobs/MyReport/?page=0", fixed = TRUE)
  expect_equal(body_of(run$calls[4]), list(series = "XYZ"))

  expect_s3_class(run$result$plain, "data.frame")
  expect_equal(run$result$plain$id, c(102, 101))
  expect_equal(run$result$plain$tags.series, c("XYZ", "XYZ"))
  expect_equal(nrow(run$result$paged), 4)
})

test_that("gateaux_list_jobs explains a non-JSON answer and handles an empty one", {
  expect_error(
    with_curl(function(cmd) "<html>File not found</html>", function() gateaux_list_jobs("MyReport", JWT = "tok")),
    "Unexpected \\(non-JSON\\) response"
  )
  run <- with_curl(function(cmd) '{"results": [], "total": 0}', function() gateaux_list_jobs("MyReport", JWT = "tok"))
  expect_equal(nrow(run$result), 0)
})

test_that("gateaux_annotate_job posts tags and metadata, and clears with empty lists", {
  run <- with_curl(function(cmd) canned_job(tags = list(state = "reviewed")), function() {
    list(
      set = gateaux_annotate_job("MyReport", 101, JWT = "tok", tags = list(state = "reviewed"), metadata = list(rmse = 0.4)),
      clear = gateaux_annotate_job("MyReport", 101, JWT = "tok", tags = list(), metadata = list()),
      str = gateaux_annotate_job("MyReport", 101, JWT = "tok", metadata = '{"a": 1}')
    )
  })
  expect_match(run$calls[1], "https://gateaux.io/api/job/annotate/MyReport/101", fixed = TRUE)
  expect_equal(body_of(run$calls[1]), list(tags = list(state = "reviewed"), metadata = list(rmse = 0.4)))
  expect_match(run$calls[2], '"tags":{},"metadata":{}', fixed = TRUE)
  expect_equal(body_of(run$calls[3]), list(metadata = list(a = 1)))
  expect_equal(run$result$set$tags$state, "reviewed")
  expect_error(gateaux_annotate_job("MyReport", 101, JWT = "tok"), "at least one")
})

test_that("gateaux_download_output finds jobs in the listing and fetches their files_url", {
  withr::local_dir(withr::local_tempdir())
  payload <- withr::local_tempfile(fileext = ".zip")
  writeLines("not really a zip", payload)
  listing <- jsonlite::toJSON(list(results = list(
    list(id = 7, files_url = paste0("file://", payload, "?jwt=SECRET")),
    list(id = 8, files_url = "file:///nope")
  )), auto_unbox = TRUE)
  run <- with_curl(function(cmd) listing, function() {
    gateaux_download_output("MyReport", report_id = 7, JWT = "tok", page_no = 0)
  })
  expect_match(run$calls[1], "https://gateaux.io/api/jobs/MyReport?page=0", fixed = TRUE)
  expect_length(run$calls, 2)
  expect_match(run$calls[2], paste0("'file://", payload, "?jwt=SECRET' > output/1_MyReport.zip"), fixed = TRUE)
  expect_true(file.exists("output/1_MyReport.zip"))
  expect_equal(readLines("output/1_MyReport.zip"), "not really a zip")
  # the printed download command carries the link with its token redacted
  expect_no_match(paste(run$printed, collapse = "\n"), "SECRET", fixed = TRUE)
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

test_that("tokens never appear in warnings or printed commands", {
  expect_equal(bakeR:::.redact('curl -H "Authorization: Bearer abc.def" https://x/?jwt=zzz&p=1'),
               'curl -H "Authorization: Bearer ***" https://x/?jwt=***&p=1')
  expect_warning(
    bakeR:::.system("curl -s -H 'Authorization: Bearer abc' http://127.0.0.1:1/ 2>/dev/null", intern = TRUE),
    "Bearer \\*\\*\\*"
  )
  run <- with_curl(function(cmd) canned_jobs(), function() {
    gateaux_job_runner(list(Run1 = list(pars = list())), report_name = "MyReport", JWT = "s3cret", log_jobs = FALSE)
  })
  expect_match(run$calls, "Bearer s3cret", fixed = TRUE)
  expect_no_match(paste(run$printed, collapse = "\n"), "s3cret", fixed = TRUE)
})
