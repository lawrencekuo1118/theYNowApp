#!/usr/bin/env Rscript
# Deploy must refuse a missing / incomplete app_25.0/.Renviron unless overridden.
# Prevents Cloud deploys from wiping live Google/Email/Neon wiring again.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_deploy_renviron_guard.R", mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)

deploy_src <- paste(
  readLines(file.path(root, "scripts", "deploy_app_21.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
restore_path <- file.path(root, "scripts", "restore_renviron_from_shinyapps.R")
restore_src <- if (file.exists(restore_path)) {
  paste(readLines(restore_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  ""
}

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
check("deploy guard has renviron override", grepl("YNOW_ALLOW_MISSING_RENVIRON", deploy_src, fixed = TRUE))
check("deploy refuses missing Neon by default", grepl("Refusing deploy: .Renviron has no Neon", deploy_src, fixed = TRUE))
check("deploy has Neon override", grepl("YNOW_ALLOW_MISSING_NEON", deploy_src, fixed = TRUE))
check("deploy auto-restores from live bundle", grepl("restore_renviron_from_shinyapps", deploy_src, fixed = TRUE))
check("deploy refuses missing OAuth", grepl("neither Supabase nor Google OAuth", deploy_src, fixed = TRUE))
check("restore script exists", file.exists(restore_path))
check("restore script never prints values", {
  grepl("Never prints secret values", restore_src, fixed = TRUE) &&
    !grepl("message\\(.*password", restore_src, ignore.case = TRUE)
})

message("")
message("Results: ", n_ok, " PASS / ", n_fail, " FAIL")
if (n_fail > 0L) quit(status = 1L)
