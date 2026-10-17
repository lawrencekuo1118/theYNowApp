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
user_args <- commandArgs(trailingOnly = TRUE)
allow_missing_renviron <- identical(Sys.getenv("YNOW_ALLOW_MISSING_RENVIRON"), "1") ||
  "--allow-missing-renviron" %in% user_args
allow_missing_neon <- identical(Sys.getenv("YNOW_ALLOW_MISSING_NEON"), "1") ||
  "--allow-missing-neon" %in% user_args
# Default ON: pull live bundle .Renviron before ship so redeploys cannot drop secrets.
auto_restore <- !identical(Sys.getenv("YNOW_SKIP_RENVIRON_RESTORE"), "1") &&
  !("--skip-renviron-restore" %in% user_args)
# Default ON: merge Cursor/CI process-env secrets into .Renviron (Neon first bind).
auto_env_inject <- !identical(Sys.getenv("YNOW_SKIP_RENVIRON_ENV_INJECT"), "1") &&
  !("--skip-renviron-env-inject" %in% user_args)
restore_script <- file.path(root, "scripts", "restore_renviron_from_shinyapps.R")
inject_script <- file.path(root, "scripts", "inject_renviron_from_env.R")

.ynow_renviron_key_flags <- function(path) {
  if (!file.exists(path)) {
    return(list(
      keys = character(), has_supabase = FALSE, has_google = FALSE,
      has_oauth = FALSE, has_neon = FALSE
    ))
  }
  ren_lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  ren_keys <- sub("=.*", "", ren_lines[grepl("^[A-Za-z_][A-Za-z0-9_]*=.", ren_lines)])
  has_supabase <- all(c("YNOW_SUPABASE_URL", "YNOW_SUPABASE_ANON_KEY") %in% ren_keys)
  has_google <- all(c("YNOW_GOOGLE_CLIENT_ID", "YNOW_GOOGLE_CLIENT_SECRET") %in% ren_keys)
  has_neon <- all(c("YNOW_DB_HOST", "YNOW_DB_USER", "YNOW_DB_PASSWORD") %in% ren_keys) ||
    ("YNOW_DATABASE_URL" %in% ren_keys)
  list(
    keys = ren_keys,
    has_supabase = has_supabase,
    has_google = has_google,
    has_oauth = isTRUE(has_supabase) || isTRUE(has_google),
    has_neon = isTRUE(has_neon)
  )
}

flags <- .ynow_renviron_key_flags(renviron_path)
need_restore <- !file.exists(renviron_path) || !isTRUE(flags$has_oauth) || !isTRUE(flags$has_neon)
if (isTRUE(auto_restore) && isTRUE(need_restore) && file.exists(restore_script)) {
  message(
    "Auto-restoring .Renviron from live shinyapps bundle ",
    "(merge keeps any newer local keys)…"
  )
  # Merge when a local file already exists so freshly pasted Neon is not wiped.
  status <- system2(
    "Rscript",
    c(restore_script, if (file.exists(renviron_path)) "--merge" else character()),
    stdout = TRUE,
    stderr = TRUE
  )
  writeLines(status)
  if (!is.null(attr(status, "status")) && !identical(attr(status, "status"), 0L)) {
    message("WARNING: live .Renviron restore failed — continuing with local file checks.")
  }
  flags <- .ynow_renviron_key_flags(renviron_path)
}

# After live restore: overlay process-env secrets (first-time Neon bind from Cloud secrets).
# Env values win so a newly pasted Neon is not discarded when live still lacks it.
if (isTRUE(auto_env_inject) && file.exists(inject_script)) {
  message("Merging allow-listed YNOW_* secrets from process env into .Renviron…")
  inj_status <- system2("Rscript", inject_script, stdout = TRUE, stderr = TRUE)
  writeLines(inj_status)
  if (!is.null(attr(inj_status, "status")) && !identical(attr(inj_status, "status"), 0L)) {
    message("WARNING: env → .Renviron inject failed — continuing with file checks.")
  }
  flags <- .ynow_renviron_key_flags(renviron_path)
}

if (!file.exists(renviron_path) && !isTRUE(allow_missing_renviron)) {
  stop(paste(
    "Refusing deploy: missing ", renviron_path, ".",
    "Copy .Renviron.example → .Renviron, fill YNOW_SUPABASE_* (or Google client) and YNOW_DB_*,",
    "or run: Rscript scripts/restore_renviron_from_shinyapps.R",
    "Override only with YNOW_ALLOW_MISSING_RENVIRON=1 (breaks live login).",
    sep = "\n"
  ), call. = FALSE)
}

if (file.exists(renviron_path)) {
  flags <- .ynow_renviron_key_flags(renviron_path)
  if (!isTRUE(flags$has_oauth) && !isTRUE(allow_missing_renviron)) {
    stop(paste(
      "Refusing deploy: .Renviron has neither Supabase nor Google OAuth keys.",
      "Restore from live: Rscript scripts/restore_renviron_from_shinyapps.R",
      "or set YNOW_SUPABASE_URL + YNOW_SUPABASE_ANON_KEY.",
      sep = "\n"
    ), call. = FALSE)
  }
  if (!isTRUE(flags$has_neon) && !isTRUE(allow_missing_neon)) {
    stop(paste(
      "Refusing deploy: .Renviron has no account Postgres YNOW_DB_* / YNOW_DATABASE_URL.",
      "Without an account DB (Supabase pooler, Neon, or other Postgres), live falls back to",
      "memory points (wiped every cold start) and Testing admin / Email login stay empty.",
      "Fix (any one):",
      "  1) Paste pooled YNOW_DB_* (or YNOW_DATABASE_URL) into app_25.0/.Renviron (gitignored), or",
      "  2) Set those keys in the environment / Cloud Agent secrets, then re-run deploy",
      "     (scripts/inject_renviron_from_env.R merges them into .Renviron), or",
      "  3) Restore a prior live bundle that already has the account DB keys.",
      "Emergency override only: YNOW_ALLOW_MISSING_NEON=1",
      sep = "\n"
    ), call. = FALSE)
  }
  if (!isTRUE(flags$has_neon) && isTRUE(allow_missing_neon)) {
    message("WARNING: shipping WITHOUT account DB (YNOW_ALLOW_MISSING_NEON=1) — points stay memory-only.")
  }
  message(
    "Deploy .Renviron OK: oauth=", flags$has_oauth,
    " account_db=", flags$has_neon,
    " (gitignored; not committed)."
  )
}

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
