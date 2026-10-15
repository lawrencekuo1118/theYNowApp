#!/usr/bin/env Rscript
# Create ynow_users on Neon and optionally seed YOUR maintenance admin.
# Seed is NOT a whitelist — end users self-register via Google / Facebook / Email.
# Usage (from repo root, with app_25.0/.Renviron filled):
#   Rscript scripts/neon_migrate.R

app_dir <- if (dir.exists("app_25.0")) {
  normalizePath("app_25.0")
} else {
  normalizePath(".")
}
renv <- file.path(app_dir, ".Renviron")
if (file.exists(renv)) readRenviron(renv)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(pool)
  library(openssl)
})
if (requireNamespace("sodium", quietly = TRUE)) {
  suppressPackageStartupMessages(library(sodium))
}

source(file.path(app_dir, "auth_account.R"), local = TRUE, encoding = "UTF-8")
source(file.path(app_dir, "auth_db.R"), local = TRUE, encoding = "UTF-8")

if (!isTRUE(ynow_db_ready())) {
  stop(
    "Neon DB env not set. Copy app_25.0/.Renviron.example → .Renviron and fill ",
    "YNOW_DB_* or YNOW_DATABASE_URL."
  )
}

ok <- ynow_db_ensure_schema()
if (!isTRUE(ok)) stop("Schema create failed (check SSL / credentials / network).")
cat("OK schema ynow_users\n")

seeded <- ynow_db_seed_admin_from_env()
if (isTRUE(seeded)) {
  cat("OK seed admin from YNOW_AUTH_SEED_EMAIL (created or already present)\n")
} else {
  cat("SKIP seed (set YNOW_AUTH_SEED_EMAIL + YNOW_AUTH_SEED_PASSWORD to create admin)\n")
}

cat("Done.\n")
