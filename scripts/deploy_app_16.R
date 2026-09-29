#!/usr/bin/env Rscript
# Compatibility shim: live line is app_18.0 — forwards to deploy_app_18.R
message("Note: live line is app_18.0; forwarding to scripts/deploy_app_18.R")
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
if (length(file_arg) == 1L && nzchar(file_arg)) {
  root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
user_args <- commandArgs(trailingOnly = TRUE)
status <- system2("Rscript", c(file.path(root, "scripts", "deploy_app_18.R"), user_args))
quit(status = if (is.null(status)) 0L else status)
