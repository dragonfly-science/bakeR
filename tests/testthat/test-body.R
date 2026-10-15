test_that("v2 job body carries env, tags, overrides and dependency refs", {
  body <- bakeR:::gateaux_job_body(
    "Run1",
    list(pars = list(Env1 = "a", Env2 = 2), wants = list(up = c(1, 2)), requires = list(up2 = 3)),
    cpus = 4, memory = "8", branch = "dev", api = "v2")
  expect_equal(body$env, list(TAG = "Run1", Env1 = "a", Env2 = "2"))
  expect_equal(body$tags, list(tag = "Run1"))
  expect_equal(body$cpus, 4)
  expect_equal(body$memory, 8)
  expect_equal(as.character(body$wants), c("up:1", "up:2"))
  expect_equal(as.character(body$requires), "up2:3")
  expect_equal(body$branch, "dev")
  json <- jsonlite::fromJSON(jsonlite::toJSON(body, auto_unbox = TRUE), simplifyVector = FALSE)
  expect_equal(json$env$TAG, "Run1")
  expect_equal(unlist(json$wants), c("up:1", "up:2"))
})

test_that("v1 job body has no tags and rejects branch", {
  body <- bakeR:::gateaux_job_body("Run1", list(pars = list(X = "y")), api = "v1")
  expect_null(body$tags)
  expect_equal(body$env, list(TAG = "Run1", X = "y"))
  expect_error(bakeR:::gateaux_job_body("Run1", list(), branch = "dev", api = "v1"), "v2")
})

test_that("a dependency without job ids names the report only", {
  body <- bakeR:::gateaux_job_body("t", list(requires = list(upstream = NULL)), api = "v2")
  expect_equal(as.character(body$requires), "upstream")
})

test_that("api selection honours the argument and the option", {
  expect_equal(gateaux_api("v1"), "v1")
  expect_equal(gateaux_api(), "v2")
  withr::with_options(list(bakeR.api = "v1"), expect_equal(gateaux_api(), "v1"))
  expect_error(gateaux_api("v3"))
})

test_that("gateaux_url handles host names and base URLs", {
  expect_equal(bakeR:::gateaux_url("gateaux.io", "/api/v2/x"), "https://gateaux.io/api/v2/x")
  expect_equal(bakeR:::gateaux_url("http://127.0.0.1:7444/", "/api/v2/x"), "http://127.0.0.1:7444/api/v2/x")
})

test_that("gateaux_jobs_frame flattens jobs and env", {
  jobs <- list(list(id = 1, status = "Success", tags = list(tag = "a"), parameters = list(env = list(TAG = "a", X = "1"), cpus = 2),
                    triggered_by = list(id = 11, github_login = "ed", name = "Ed")),
               list(id = 2, status = "Pending", parameters = list(env = list(TAG = "b")), triggered_by = "legacy name"))
  f <- gateaux_jobs_frame(jobs)
  expect_equal(f$id, c(1L, 2L))
  expect_equal(f$tag, c("a", "b"))
  expect_equal(f$triggered_by, c("ed", "legacy name"))
  expect_equal(f$env.X, c("1", NA))
  expect_equal(nrow(gateaux_jobs_frame(list())), 0)
})
