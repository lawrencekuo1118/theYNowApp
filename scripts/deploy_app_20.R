#!/usr/bin/env Rscript
# Compatibility shim: live line is app_21.0 — forwards to deploy_app_21.R
message("Note: live line is app_21.0; forwarding to scripts/deploy_app_21.R")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
user_args <- commandArgs(trailingOnly = TRUE)
status <- system2("Rscript", c(file.path(root, "scripts", "deploy_app_21.R"), user_args))
quit(status = status)
