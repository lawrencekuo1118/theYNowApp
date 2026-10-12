#!/usr/bin/env Rscript
# Compatibility shim: live line is app_23.0 — forwards to deploy_app_21.R.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
status <- system2(
  "Rscript",
  c(file.path(script_dir, "deploy_app_21.R"), commandArgs(trailingOnly = TRUE))
)
quit(status = if (is.numeric(status)) as.integer(status)[[1]] else 1L)
