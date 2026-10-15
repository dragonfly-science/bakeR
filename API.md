# Kahawai / Gateaux job-runner API

Two Dragonfly job-runner systems share the same underlying server software
(the Gateaux server — Haskell + Servant):

- **gateaux.io** — one install of the Gateaux server
- **gateaux.io** — a more recent install of the same software; its API surface is
  closer to current dev. 

The notes below are from probing the **live gateaux.io API** during
psc-estimation work. The gateaux shapes come from `bakeR`
(`R/gateaux_job_runner.R`, `R/gateaux_download_output.R`).

---

## Two auth models

| Token type | Where it lives | What it can do |
|---|---|---|
| **Per-report trigger token** | `.jwt` in the project (one per kahawai report) | Submit jobs and list jobs for *that* report. Cannot fetch logs/outputs. |
| **User-scoped Bearer JWT** | Personal; from the kahawai web UI (browser dev-tools → network → look for `Authorization: Bearer …`) | Everything: submit, list, logs, outputs, cancel — across any report you have access to. |

Per-report tokens go in the URL path. User JWTs go in the `Authorization` header
(or `?jwt=` query for some download routes).

---

## Submit a job

### Kahawai (per-report trigger token)

```
PUT https://gateaux.io/job/<report_name>/<token>
Content-Type: application/json

{"TAG": "memorable-name", "PREDICTOR_TAG": "bycatch-dfffs", "SAMPLE_PRIOR": "only"}
```

The body is a **flat top-level JSON object**: each key becomes a separate
`-e KEY=VALUE` env var on the container. Response: `{"result": "Processed"}`
with HTTP 200. **The response does not contain the new job id** — fetch the
joblist (next section) and filter by TAG to recover it.

#### Gotcha — don't wrap env

```jsonc
// WRONG — kahawai stores the whole map as a single env var named `env`
{"env": {"PREDICTOR_TAG": "x"}}
// WRONG — same problem; whole string ends up in one env var
{"env": "PREDICTOR_TAG=x SAMPLE_PRIOR=only"}
// RIGHT
{"PREDICTOR_TAG": "x", "SAMPLE_PRIOR": "only"}
```

The wrong shapes return HTTP 200 and the job runs — but `${PREDICTOR_TAG}` in
your shell script will be empty, so the failure mode is silent.

#### curl example

```bash
TOKEN=$(cat .jwt)
curl -X PUT -H "Content-Type: application/json" \
     -d '{"TAG":"silver-swallow","PREDICTOR_TAG":"bycatch-dfffs"}' \
     "https://gateaux.io/job/DAT2023-04C-models/$TOKEN"
```

#### R example

```r
body <- jsonlite::toJSON(
  list(TAG = "silver-swallow", PREDICTOR_TAG = "bycatch-dfffs"),
  auto_unbox = TRUE
)
url <- sprintf("https://gateaux.io/job/%s/%s", report, token)
system(sprintf('curl -X PUT -H "Content-Type: application/json" -d %s %s',
               shQuote(body), shQuote(url)))
```

### Gateaux (user Bearer JWT)

```
POST https://gateaux.io/job/<report_name>
Authorization: Bearer <user_jwt>
Content-Type: application/json

{
  "cpus":    4,
  "memory":  8,
  "env":     {"TAG": "Run1", "PREDICTOR_TAG": "bycatch-dfffs"},
  "wants":   ["Upstream_job:1234"],
  "requires":["Upstream_job2:4321"]
}
```

Note the **opposite** shape — `env` is a nested object here, and the body
supports `cpus`, `memory`, `wants`, `requires`. Use the `bakeR::gateaux_job_runner`
helper rather than rolling your own.

---

## List jobs

### Kahawai

```
GET https://gateaux.io/jobs/<report>/<token>
```

Returns `{"results": [job, …]}`. Each `job` has:

```jsonc
{
  "id": 151044,
  "status": "Success",                  // Pending | Running | Success | Failed | Cancelled
  "started_at":   "2026-05-28T11:30:00Z",
  "finished_at":  "2026-05-28T11:35:34Z",
  "message":      "",
  "parameters": {
    "env":     {"TAG": "arctic-dragonfly", "PREDICTOR_TAG": "bycatch-dfffs"},
    "cpus":    4,
    "memory":  8,
    "docker":  "ubuntu/psc-estimation",
    "requires":["DAT2023-04C-psc-estimates-db"]
  }
}
```

To recover a just-submitted job id: GET the joblist, filter by your `TAG`,
take the max `id`.

### Gateaux

```
GET https://gateaux.io/api/jobs/<report>?page=<N>
Authorization: Bearer <user_jwt>
```

Each result has `id` and a `files_url` that can be GETed directly to download
the output ZIP.

---

## Logs and outputs

### Kahawai with the per-report token

**Not supported.** `GET /files/<report>/<job_id>` returns 401 — that route
requires a user-scoped JWT or session cookie. Other path/header/query variants
of the token all return 404 or 401.

### Kahawai with a user JWT

```
# JSON metadata listing output files
GET /api/files/<report>/<job_id>
Authorization: Bearer <user_jwt>

# Plain-text job log
GET /api/download/<report>/<job_id>/log
Authorization: Bearer <user_jwt>

# Individual file by name
GET /api/download/<report>/<job_id>/<filename>
Authorization: Bearer <user_jwt>

# Or via the jwt-in-query route
GET /file/<report>/<job_id>/<filename>?jwt=<user_jwt>
```

### Gateaux

Pull `files_url` from the joblist (see above) and GET it with the Bearer
header. The `bakeR::gateaux_download_output` helper does the lookup + download
loop.

---

## End-to-end submit + poll (R)

This is the pattern used by `baker.r` and `joblist.r` in this directory:

```r
suppressMessages({library(jsonlite); library(data.table)})

report <- "DAT2023-04C-models"
token  <- readLines(".jwt", warn = FALSE, n = 1)

submit_kahawai <- function(env_vars, tag = NULL) {
  env_vars$TAG <- tag %||% env_vars$TAG
  body <- jsonlite::toJSON(env_vars, auto_unbox = TRUE)
  url  <- sprintf("https://gateaux.io/job/%s/%s", report, token)
  system(sprintf(
    'curl -s -X PUT -H "Content-Type: application/json" -d %s %s',
    shQuote(body), shQuote(url)
  ), intern = TRUE)
}

fetch_jobs <- function(tag = NULL) {
  url <- sprintf("https://gateaux.io/jobs/%s/%s", report, token)
  raw <- system(sprintf("curl -s %s", shQuote(url)), intern = TRUE)
  jl  <- jsonlite::fromJSON(paste(raw, collapse = ""), simplifyVector = FALSE)
  rows <- lapply(jl$results, function(j) {
    e <- j$parameters$env %||% list()
    data.table(id = j$id, status = j$status,
               tag = e$TAG %||% NA, predictor = e$PREDICTOR_TAG %||% NA)
  })
  dt <- rbindlist(rows, fill = TRUE)
  if (!is.null(tag)) dt <- dt[tag == ..tag]
  dt
}

submit_kahawai(list(PREDICTOR_TAG = "bycatch-dfffs",
                    SAMPLE_PRIOR  = "only",
                    ITER          = "400"),
               tag = "silver-swallow")

Sys.sleep(2)
fetch_jobs(tag = "silver-swallow")
```

---

## Open questions

These weren't pinned down during this session:

- **Cancel endpoint.** The job model has a `cancel_initiated_at` field but the
  cancel HTTP route hasn't been probed. Read the Gateaux server source for
  the route, or check the web UI's network calls when you cancel manually.
- **Per-report token + logs.** A clean way to enable headless log retrieval
  from CI would be to patch the Gateaux server to also accept the per-report token on
  `/files/<report>/<job_id>/log`. Until that's done, log retrieval needs a
  user JWT in some shape.
