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
check("deploy refuses missing account DB by default", {
  grepl("Refusing deploy: .Renviron has no account Postgres", deploy_src, fixed = TRUE) ||
    grepl("Refusing deploy: .Renviron has no Neon", deploy_src, fixed = TRUE)
})
check("deploy has account-DB override", grepl("YNOW_ALLOW_MISSING_NEON", deploy_src, fixed = TRUE))
check("deploy auto-restores from live bundle", grepl("restore_renviron_from_shinyapps", deploy_src, fixed = TRUE))
check("deploy injects secrets from process env", grepl("inject_renviron_from_env", deploy_src, fixed = TRUE))
check("deploy refuses missing OAuth", grepl("neither Supabase nor Google OAuth", deploy_src, fixed = TRUE))
check("restore script exists", file.exists(restore_path))
check("restore script never prints values", {
  grepl("Never prints secret values", restore_src, fixed = TRUE) &&
    !grepl("message\\(.*password", restore_src, ignore.case = TRUE)
})
inject_path <- file.path(root, "scripts", "inject_renviron_from_env.R")
inject_src <- if (file.exists(inject_path)) {
  paste(readLines(inject_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  ""
}
check("inject script exists", file.exists(inject_path))
check("inject script allow-lists Neon keys", {
  grepl("YNOW_DB_HOST", inject_src, fixed = TRUE) &&
    grepl("YNOW_DATABASE_URL", inject_src, fixed = TRUE) &&
    grepl("Never prints secret values", inject_src, fixed = TRUE)
})

# Functional: env inject writes Neon keys without echoing values.
tmp_app <- tempfile("ynow_inject_")
dir.create(file.path(tmp_app, "app_25.0"), recursive = TRUE)
# Point inject script at temp tree by running from tmp with a shim root layout.
# Reuse the real script logic via env + temporary dest by chdir into a fake root.
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
# Copy inject script into a mini root that mirrors repo layout.
dir.create(file.path(tmp_app, "scripts"), recursive = TRUE)
file.copy(inject_path, file.path(tmp_app, "scripts", "inject_renviron_from_env.R"))
setwd(tmp_app)
Sys.setenv(
  YNOW_DB_HOST = "ep-test-pooler.example.neon.tech",
  YNOW_DB_USER = "neondb_owner",
  YNOW_DB_PASSWORD = "test-secret-not-for-prod",
  YNOW_DB_NAME = "neondb"
)
out <- system2(
  "Rscript",
  file.path("scripts", "inject_renviron_from_env.R"),
  stdout = TRUE,
  stderr = TRUE
)
ren_path <- file.path("app_25.0", ".Renviron")
ren_txt <- if (file.exists(ren_path)) {
  paste(readLines(ren_path, warn = FALSE), collapse = "\n")
} else {
  ""
}
check("inject creates .Renviron with Neon host", grepl("YNOW_DB_HOST=ep-test-pooler", ren_txt, fixed = TRUE))
check("inject stdout does not echo password value", {
  !any(grepl("test-secret-not-for-prod", out, fixed = TRUE))
})
Sys.unsetenv(c("YNOW_DB_HOST", "YNOW_DB_USER", "YNOW_DB_PASSWORD", "YNOW_DB_NAME"))

message("")
message("Results: ", n_ok, " PASS / ", n_fail, " FAIL")
if (n_fail > 0L) quit(status = 1L)
