#!/usr/bin/env Rscript
# Deploy must refuse a missing app_25.0/.Renviron unless explicitly overridden.
# Prevents Cloud deploys from wiping live Google/Email/Neon wiring again.

root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
if (!file.exists(file.path(root, "scripts", "deploy_app_21.R"))) {
  root <- normalizePath("../..", mustWork = FALSE)
}
deploy_src <- paste(
  readLines(file.path(root, "scripts", "deploy_app_21.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

n_ok <- 0L
n_fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    n_ok <<- n_ok + 1L
    message("PASS: ", label)
  } else {
    n_fail <<- n_fail + 1L
    message("FAIL: ", label)
  }
}

check("deploy guard mentions .Renviron", grepl("\\.Renviron", deploy_src, fixed = FALSE))
check("deploy guard can refuse missing file", grepl("Refusing deploy: missing", deploy_src, fixed = TRUE))
check("deploy guard has override", grepl("YNOW_ALLOW_MISSING_RENVIRON", deploy_src, fixed = TRUE))
check("deploy warns when Neon missing", grepl("no Neon YNOW_DB_", deploy_src, fixed = TRUE))
check("deploy warns when OAuth missing", grepl("neither Supabase nor Google", deploy_src, fixed = TRUE))

message("")
message("Results: ", n_ok, " PASS / ", n_fail, " FAIL")
if (n_fail > 0L) quit(status = 1L)
