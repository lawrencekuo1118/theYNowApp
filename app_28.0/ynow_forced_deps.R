# Force rsconnect / renv to snapshot Postgres account-store + async packages that pacman::p_load
# alone can miss when those packages are not installed in the deploy VM.
# Sourced from global.R; keep as plain library() calls (side-effect free if already loaded).
# pagedown is intentionally omitted here: PDF uses pagedown::chrome_print lazily in
# setup_report.R (rsconnect still snapshots via `::`). Never attach or find Chrome at boot.
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(pool)
  library(sodium)
  library(openssl)
  library(future)
  library(promises)
  library(later)
  library(callr)
})
