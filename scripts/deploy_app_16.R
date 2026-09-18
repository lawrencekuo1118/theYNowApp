#!/usr/bin/env Rscript
# Compatibility shim: live line is app_17.0 — forwards to deploy_app_17.R
message("Note: live line is app_17.0; forwarding to scripts/deploy_app_17.R")
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
root <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
status <- system2("Rscript", file.path(root, "scripts", "deploy_app_17.R"))
quit(status = if (is.na(status)) 1L else as.integer(status))
