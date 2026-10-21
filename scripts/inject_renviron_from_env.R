#!/usr/bin/env Rscript
# Merge selected secrets from process env into gitignored app_26.0/.Renviron.
# Used by deploy preflight so Cursor Cloud / CI secrets can bind Neon + OAuth
# even when the live shinyapps bundle never had those keys.
#
# Never prints secret values. Never commits .Renviron.
#
# Usage (repo root):
#   Rscript scripts/inject_renviron_from_env.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(file_arg) == 1L && nzchar(file_arg)) {
  root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

app_dir <- if (dir.exists(file.path(root, "app_26.0"))) {
  file.path(root, "app_26.0")
} else {
  stop("app_26.0 not found under ", root, call. = FALSE)
}
dest <- file.path(app_dir, ".Renviron")

# Only allow-listed keys — never dump arbitrary env into the bundle.
secret_keys <- c(
  "YNOW_DB_HOST", "YNOW_DB_NAME", "YNOW_DB_USER", "YNOW_DB_PASSWORD",
  "YNOW_DB_PORT", "YNOW_DB_SSLMODE", "YNOW_DATABASE_URL",
  "YNOW_ENABLE_ACCOUNT_LOGIN",
  "YNOW_SUPABASE_URL", "YNOW_SUPABASE_ANON_KEY", "YNOW_SUPABASE_SERVICE_ROLE",
  "YNOW_GOOGLE_CLIENT_ID", "YNOW_GOOGLE_CLIENT_SECRET",
  "YNOW_FACEBOOK_APP_ID", "YNOW_FACEBOOK_APP_SECRET",
  "YNOW_OAUTH_REDIRECT_URI", "YNOW_OAUTH_STATE_SECRET",
  "YNOW_POINTS_ENABLED", "YNOW_POINTS_LOGIN_GRANT",
  "YNOW_POINTS_SIGNUP_BONUS", "YNOW_POINTS_DAILY_CHECKIN",
  "YNOW_AUTH_SEED_EMAIL", "YNOW_AUTH_SEED_PASSWORD"
)

.parse_map <- function(path) {
  out <- list()
  if (!file.exists(path)) return(out)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  for (ln in lines) {
    if (!grepl("^[A-Za-z_][A-Za-z0-9_]*=", ln)) next
    k <- sub("=.*", "", ln)
    out[[k]] <- ln
  }
  out
}

.escape_renviron_value <- function(val) {
  # .Renviron: quote when value has whitespace, #, or quotes.
  if (!grepl("[[:space:]#'\"]", val)) return(val)
  paste0("\"", gsub("\"", "\\\"", val, fixed = TRUE), "\"")
}

existing <- .parse_map(dest)
injected <- character()
for (k in secret_keys) {
  v <- Sys.getenv(k, unset = "")
  if (!nzchar(v)) next
  existing[[k]] <- paste0(k, "=", .escape_renviron_value(v))
  injected <- c(injected, k)
}

if (!length(injected)) {
  message("No allow-listed YNOW_* secrets found in process env — .Renviron unchanged.")
  quit(status = 0)
}

prefer <- secret_keys
keys <- c(prefer[prefer %in% names(existing)], setdiff(names(existing), prefer))
out_lines <- vapply(keys, function(k) existing[[k]], character(1))
writeLines(c(
  "# Merged by scripts/inject_renviron_from_env.R — gitignored; never commit.",
  out_lines
), dest, useBytes = TRUE)
Sys.chmod(dest, mode = "0600")
message(
  "Injected ", length(injected), " key(s) from process env into .Renviron: ",
  paste(injected, collapse = ", ")
)
invisible(list(path = dest, injected = injected, keys = names(existing)))
