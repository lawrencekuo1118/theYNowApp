#!/usr/bin/env Rscript
# Deploy the live app directory (app_25.0) → shinyapps.io TheYNowApp
# YNOW_VERSION_BUMP_MAJOR=1 ships the next whole version (v24.25 -> v25.00).
#
# Credentials (either):
#   SHINYAPPS_ACCOUNT / SHINYAPPS_TOKEN / SHINYAPPS_SECRET
#   or a locally configured rsconnect account (rsconnect::accounts())
# Optional:
#   SHINYAPPS_APP_NAME (default TheYNowApp)
#   SHINYAPPS_APP_ID   (default 10907657)
#   YNOW_SKIP_VERSION_BUMP=1 or --no-bump
#       ship the tree as-is (retry). Default: +0.01 display version, then upload.
#   YNOW_VERSION_BUMP_NO_COMMIT=1
#       rewrite version files but do not git commit them.

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

acct <- Sys.getenv("SHINYAPPS_ACCOUNT", "")
tok  <- Sys.getenv("SHINYAPPS_TOKEN", "")
sec  <- Sys.getenv("SHINYAPPS_SECRET", "")
use_env_creds <- nzchar(acct) && nzchar(tok) && nzchar(sec)

if (!use_env_creds) {
  acc <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
  if (!is.null(acc) && nrow(acc) > 0) {
    acct <- as.character(acc$name[[1]])
    message("Using configured rsconnect account: ", acct, " @ ", acc$server[[1]])
  } else {
    message(paste(
      "Missing shinyapps credentials.",
      "Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, SHINYAPPS_SECRET",
      "(from https://www.shinyapps.io/admin/#/tokens) then re-run:",
      "  Rscript scripts/deploy_app_21.R",
      sep = "\n"
    ))
    quit(status = 2)
  }
}

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
if (length(file_arg) == 1L && nzchar(file_arg)) {
  root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
bump_path <- file.path(root, "scripts", "bump_display_version.R")
if (!file.exists(bump_path)) stop("Missing ", bump_path)
source(bump_path, local = FALSE)
app_dir <- ynow_live_app_dir(root)
if (!file.exists(file.path(app_dir, "app.R"))) {
  stop("live app.R not found under ", root)
}

# Guard: shinyapps has no Secrets UI — auth/DB keys must ride in gitignored app/.Renviron.
# Cloud deploys that omit this file wipe live Google/Email login and empty the points admin list.
renviron_path <- file.path(app_dir, ".Renviron")
allow_missing_renviron <- identical(Sys.getenv("YNOW_ALLOW_MISSING_RENVIRON"), "1") ||
  "--allow-missing-renviron" %in% commandArgs(trailingOnly = TRUE)
if (!file.exists(renviron_path) && !isTRUE(allow_missing_renviron)) {
  stop(paste(
    "Refusing deploy: missing ", renviron_path, ".",
    "Copy .Renviron.example → .Renviron, fill YNOW_SUPABASE_* (or Google client) and YNOW_DB_*,",
    "then re-run. Override only with YNOW_ALLOW_MISSING_RENVIRON=1 (breaks live login).",
    sep = "\n"
  ))
}
if (file.exists(renviron_path)) {
  # Presence-only check (never print values). Warn when OAuth or Neon blocks look unset.
  ren_lines <- readLines(renviron_path, warn = FALSE, encoding = "UTF-8")
  ren_keys <- sub("=.*", "", ren_lines[grepl("^[A-Za-z_][A-Za-z0-9_]*=.", ren_lines)])
  has_supabase <- all(c("YNOW_SUPABASE_URL", "YNOW_SUPABASE_ANON_KEY") %in% ren_keys)
  has_google <- all(c("YNOW_GOOGLE_CLIENT_ID", "YNOW_GOOGLE_CLIENT_SECRET") %in% ren_keys)
  has_neon <- all(c("YNOW_DB_HOST", "YNOW_DB_USER", "YNOW_DB_PASSWORD") %in% ren_keys) ||
    ("YNOW_DATABASE_URL" %in% ren_keys)
  if (!has_supabase && !has_google) {
    message("WARNING: .Renviron has neither Supabase nor Google OAuth keys — social login will fail.")
  }
  if (!has_neon) {
    message("WARNING: .Renviron has no Neon YNOW_DB_* / YNOW_DATABASE_URL — Email login and points admin stay empty.")
  }
  message("Deploy bundle will include .Renviron (gitignored; not committed).")
}

user_args <- commandArgs(trailingOnly = TRUE)
skip_bump <- "--no-bump" %in% user_args || identical(Sys.getenv("YNOW_SKIP_VERSION_BUMP"), "1")
if (!skip_bump) {
  ver <- ynow_prepare_deploy_version(root, commit = TRUE)
  message("Shipping display version ", ver$version)
} else {
  message("YNOW_SKIP_VERSION_BUMP: shipping display version without +0.01")
}

app_name <- Sys.getenv("SHINYAPPS_APP_NAME", "TheYNowApp")
app_id <- suppressWarnings(as.integer(Sys.getenv("SHINYAPPS_APP_ID", "10907657")))

if (use_env_creds) {
  message("Configuring account from env: ", acct)
  rsconnect::setAccountInfo(name = acct, token = tok, secret = sec)
} else {
  message("Deploying with saved rsconnect account: ", acct)
}

# Preflight: Neon account store packages must be installed locally or rsconnect
# skips them in the bundle → live falls back to empty in-memory points/users.
.ynow_extra_lib <- file.path(root, ".R", "library")
if (dir.exists(.ynow_extra_lib)) {
  .libPaths(c(normalizePath(.ynow_extra_lib, mustWork = FALSE), .libPaths()))
}
need_pkgs <- c("RPostgres", "pool", "sodium", "future", "DBI")
missing_pkgs <- need_pkgs[!vapply(
  need_pkgs,
  function(p) requireNamespace(p, quietly = TRUE),
  logical(1)
)]
if (length(missing_pkgs)) {
  stop(
    "Deploy preflight failed — install these packages before shipping ",
    "(otherwise Testing admin / sign-in Neon backend will be empty on shinyapps):\n  ",
    paste(missing_pkgs, collapse = ", "),
    "\nExample:\n  Rscript -e 'install.packages(c(\"RPostgres\",\"pool\",\"sodium\",\"future\"))'",
    call. = FALSE
  )
}
message("Deploy preflight OK: ", paste(need_pkgs, collapse = ", "))

message("Deploying ", app_dir, " → ", app_name, " (appId=", app_id, ")")
res <- rsconnect::deployApp(
  appDir = app_dir,
  appName = app_name,
  appId = if (is.finite(app_id)) app_id else NULL,
  account = acct,
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  lint = FALSE
)

message("Deploy finished.")
print(res)
if (!skip_bump && exists("ver") && nzchar(ver$version)) {
  ynow_record_shipped_version(root, ver$version)
  message(
    "Deploy complete. Display version is ", ver$version,
    " (+0.01 from the previous shipped version when a bump was applied)."
  )
}
invisible(res)
