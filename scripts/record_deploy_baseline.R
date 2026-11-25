#!/usr/bin/env Rscript
# After a successful tip-ship, rewrite scripts/DEPLOY_BASELINE.txt with live
# bundleId / status plus the git commit that was shipped.
#
# Used by .github/workflows/tip-ship.yml and optional Mac follow-up.
# Never prints SHINYAPPS_* / YNOW_* secret values.
#
# Env:
#   SHINYAPPS_ACCOUNT / SHINYAPPS_TOKEN / SHINYAPPS_SECRET — or saved rsconnect
#   SHINYAPPS_APP_NAME (default TheYNowApp)
#   SHINYAPPS_APP_ID   (default 10907657)
#   YNOW_BASELINE_NO_COMMIT=1 — write file only, skip git commit

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("rsconnect required", call. = FALSE)
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(file_arg) == 1L && nzchar(file_arg)) {
  root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

bump_path <- file.path(root, "scripts", "bump_display_version.R")
if (!file.exists(bump_path)) stop("Missing ", bump_path, call. = FALSE)
source(bump_path, local = FALSE)

acct <- Sys.getenv("SHINYAPPS_ACCOUNT", "")
tok <- Sys.getenv("SHINYAPPS_TOKEN", "")
sec <- Sys.getenv("SHINYAPPS_SECRET", "")
use_env <- nzchar(acct) && nzchar(tok) && nzchar(sec)
if (use_env) {
  rsconnect::setAccountInfo(name = acct, token = tok, secret = sec)
} else {
  acc <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
  if (is.null(acc) || !nrow(acc)) {
    stop("No shinyapps credentials (SHINYAPPS_* or rsconnect::accounts())", call. = FALSE)
  }
  acct <- as.character(acc$name[[1]])
}

info <- tryCatch(
  rsconnect::accountInfo(name = acct, server = "shinyapps.io"),
  error = function(e) NULL
)
if (is.null(info)) stop("Could not load rsconnect accountInfo for ", acct, call. = FALSE)

authInfo <- list(token = info$token, secret = info$secret)
GET <- getFromNamespace("GET", "rsconnect")
service <- list(
  protocol = "https", host = "api.shinyapps.io", port = 443,
  account_name = info$name
)
app_id <- suppressWarnings(as.integer(Sys.getenv("SHINYAPPS_APP_ID", "10907657")))
app_name <- Sys.getenv("SHINYAPPS_APP_NAME", unset = "")
if (!nzchar(app_name)) app_name <- "TheYNowApp"

app <- GET(service, authInfo, paste0("/v1/applications/", app_id))
bid <- app$deployment$image$bundle_id
status <- app$status
url <- app$url
if (is.null(bid) || !nzchar(as.character(bid))) {
  stop("Could not resolve live bundleId for appId=", app_id, call. = FALSE)
}

app_dir <- ynow_live_app_dir(root)
display_version <- tryCatch(ynow_read_display_version(app_dir), error = function(e) "")
if (!nzchar(display_version)) {
  display_version <- ynow_read_baseline_version(root)
}
if (is.na(display_version) || !nzchar(display_version)) {
  stop("Could not resolve display_version", call. = FALSE)
}

sha <- system2("git", c("-C", root, "rev-parse", "HEAD"), stdout = TRUE)
short <- system2("git", c("-C", root, "rev-parse", "--short", "HEAD"), stdout = TRUE)
msg <- system2(
  "git", c("-C", root, "log", "-1", "--pretty=%s"),
  stdout = TRUE
)
captured_at <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
if (!nzchar(url)) {
  url <- paste0("https://", acct, ".shinyapps.io/", app_name, "/")
}

note <- paste0(
  "Shipped ", display_version, " via deploy_app_21.R (cloud tip-ship / Actions). ",
  "Live status=", status, "; rsconnect bundleId=", bid, "."
)

path <- file.path(root, "scripts", "DEPLOY_BASELINE.txt")
lines <- c(
  paste0("source=shinyapps.io ", app_name),
  paste0("bundleId=", bid),
  paste0("appId=", app_id),
  paste0("url=", url, "  # pragma: allowlist secret"),
  paste0("aligned_git=", sha[[1]]),
  paste0("commit=", sha[[1]]),
  paste0("short=", short[[1]]),
  paste0("message=", msg[[1]]),
  paste0("captured_at=", captured_at),
  paste0("display_version=", display_version),
  paste0("note=", note)
)
ynow_write_lines(path, lines)
message("Wrote ", path, " bundleId=", bid, " display_version=", display_version)

if (!identical(Sys.getenv("YNOW_BASELINE_NO_COMMIT"), "1")) {
  ynow_git_commit_paths(
    root, path,
    paste0("chore: record shinyapps baseline bundle ", bid, " (app_28.0)"),
    must = FALSE
  )
}

invisible(list(bundleId = bid, display_version = display_version, status = status))
