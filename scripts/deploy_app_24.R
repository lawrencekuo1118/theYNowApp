# Live line app_24.0 — forwards to deploy_app_21.R (ynow_live_app_dir resolves app_24.0).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
source(file.path(root, "scripts", "deploy_app_21.R"), local = FALSE)
