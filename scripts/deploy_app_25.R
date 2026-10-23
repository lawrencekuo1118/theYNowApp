# Live line app_27.0. Resolves the directory inside deploy_app_21.R.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg, mustWork = TRUE))
} else {
  normalizePath("scripts", mustWork = TRUE)
}
status <- system2(
  "Rscript",
  c(file.path(script_dir, "deploy_app_21.R"), commandArgs(trailingOnly = TRUE))
)
quit(status = status)
