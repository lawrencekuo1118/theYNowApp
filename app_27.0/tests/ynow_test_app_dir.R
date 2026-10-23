# Resolve the live-line app root for unit tests under app_27.0/tests.
# Prefer: parent of tests/ (dirname), then cwd if it is the app, then app_27.0
# from the repo root. Do not fall back to historical app_18/21/22/24 trees.
#
# Load via --file=/cwd candidates (tests already inline this bootstrap), then:
#   root <- ynow_test_app_dir("global.R"); setwd(root)
# Preferred run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_….R
# Also works: Rscript app_27.0/tests/test_….R from repo root when bootstrap is used.

ynow_load_test_helper <- function() {
  if (exists("ynow_test_app_dir", mode = "function", inherits = TRUE)) {
    return(invisible(TRUE))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_args <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(file_args) == 1L && nzchar(file_args[[1L]])) {
    cands <- c(
      cands,
      file.path(
        dirname(normalizePath(file_args[[1L]], winslash = "/", mustWork = FALSE)),
        "ynow_test_app_dir.R"
      )
    )
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_27.0", "tests", "ynow_test_app_dir.R"),
    file.path(dirname(wd), "tests", "ynow_test_app_dir.R"),
    file.path(dirname(wd), "app_27.0", "tests", "ynow_test_app_dir.R")
  )
  cands <- unique(cands[nzchar(cands) & !is.na(cands) & file.exists(cands)])
  if (!length(cands)) {
    stop("Cannot locate tests/ynow_test_app_dir.R", call. = FALSE)
  }
  sys.source(cands[[1L]], envir = globalenv())
  invisible(TRUE)
}

ynow_test_app_dir <- function(marker = "global.R", must_work = TRUE) {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  args <- commandArgs(trailingOnly = FALSE)
  file_args <- sub("^--file=", "", args[grepl("^--file=", args)])
  from_script <- character()
  if (length(file_args) == 1L && nzchar(file_args[[1L]])) {
    script_dir <- tryCatch(
      dirname(normalizePath(file_args[[1L]], winslash = "/", mustWork = TRUE)),
      error = function(e) NA_character_
    )
    if (is.character(script_dir) && length(script_dir) == 1L &&
        nzchar(script_dir) && !is.na(script_dir) && basename(script_dir) == "tests") {
      from_script <- dirname(script_dir)
    }
  }
  candidates <- c(
    from_script,
    if (basename(wd) == "tests") dirname(wd) else character(),
    normalizePath("..", winslash = "/", mustWork = FALSE),
    wd,
    file.path(wd, "app_27.0"),
    file.path(dirname(wd), "app_27.0")
  )
  candidates <- unique(candidates[nzchar(candidates) & !is.na(candidates)])
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, marker))]
  if (length(hit)) {
    return(hit[[1L]])
  }
  if (isTRUE(must_work)) {
    stop(
      sprintf(
        "Cannot locate app_27.0 (marker %s). Run from app_27.0/tests or repo root.",
        marker
      ),
      call. = FALSE
    )
  }
  NA_character_
}
