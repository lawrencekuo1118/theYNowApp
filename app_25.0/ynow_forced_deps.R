# Force rsconnect / renv to snapshot Neon + async packages that pacman::p_load
# alone can miss when those packages are not installed in the deploy VM.
# Sourced from global.R; keep as plain library() calls (side-effect free if already loaded).
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
