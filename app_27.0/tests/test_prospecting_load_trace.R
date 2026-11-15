# Shared JSONL path: session and multisession workers must append to one file.
# Default OFF stays off. Run: cd app_27.0/tests && Rscript test_prospecting_load_trace.R
pass <- 0L
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_27.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("prospecting_load_trace.R")
cat("root=", root, "\n", sep = "")

old_trace <- Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE", unset = NA)
old_path <- Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE_PATH", unset = NA)
old_run <- Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE_RUN", unset = NA)
on.exit({
  if (is.na(old_trace)) Sys.unsetenv("YNOW_PROSPECTING_LOAD_TRACE") else Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE = old_trace)
  if (is.na(old_path)) Sys.unsetenv("YNOW_PROSPECTING_LOAD_TRACE_PATH") else Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE_PATH = old_path)
  if (is.na(old_run)) Sys.unsetenv("YNOW_PROSPECTING_LOAD_TRACE_RUN") else Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE_RUN = old_run)
  tryCatch(future::plan(future::sequential), error = function(e) NULL)
}, add = TRUE)

Sys.unsetenv("YNOW_PROSPECTING_LOAD_TRACE")
Sys.unsetenv("YNOW_PROSPECTING_LOAD_TRACE_PATH")
source(file.path(root, "prospecting_load_trace.R"), local = FALSE)
check("trace default off", !isTRUE(ynow_pload_enabled()))
check("default off does not write", !isTRUE(ynow_pload("n0", "mark")))

# Re-source so the enabled cache starts empty under the flag.
Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE = "1")
Sys.unsetenv("YNOW_PROSPECTING_LOAD_TRACE_PATH")
Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE_RUN = "trace-test")
source(file.path(root, "prospecting_load_trace.R"), local = FALSE)
path <- ynow_pload_path()
check("path is absolute", grepl("^/", path))
check("path is stable", identical(path, ynow_pload_path()))
check("path is not this process tempdir", !startsWith(normalizePath(path, mustWork = FALSE), normalizePath(tempdir(), mustWork = FALSE)))
check("path env is pinned", identical(Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE_PATH"), path))
if (file.exists(path)) unlink(path)
check("session line writes", isTRUE(ynow_pload("session_probe", "mark", list(pid_role = "parent"))))

# Simulate a worker that booted with tracing off, then receives the session path.
Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE = "0")
.ynow_pload_enabled_cache <- FALSE
ynow_pload_bind(path, "trace-test", TRUE)
check("bind turns tracing on", isTRUE(ynow_pload_enabled()))
check("bind keeps the session path", identical(ynow_pload_path(), path))

async_src <- paste(readLines(file.path(root, "async_jobs.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("extended task passes explicit globals", grepl("globals = list(", async_src, fixed = TRUE))
check("extended task does not capture job_fn", !grepl("jf <- job_fn", async_src, fixed = TRUE))
check("extended task binds the trace path", grepl("ynow_pload_bind(trace_path, trace_run, TRUE)", async_src, fixed = TRUE))

worker_line <- FALSE
if (requireNamespace("future", quietly = TRUE)) {
  app <- gsub("'", "", normalizePath(root, winslash = "/", mustWork = FALSE), fixed = TRUE)
  startup <- paste(c(
    sprintf("setwd('%s')", app),
    "env <- new.env(parent = .GlobalEnv)",
    "sys.source('prospecting_load_trace.R', envir = env)",
    "if (!('ynow:worker' %in% search())) attach(env, name = 'ynow:worker', warn.conflicts = FALSE)"
  ), collapse = "\n")
  future::plan(future::multisession, workers = 2, rscript_startup = startup)
  worker_path <- path
  f <- future::future({
    ynow_pload_bind(trace_path, "trace-test", TRUE)
    ynow_pload("worker_probe", "mark", list(pid_role = "worker"))
    c(pid = as.character(Sys.getpid()), path = ynow_pload_path())
  }, globals = list(trace_path = worker_path), seed = TRUE)
  got <- tryCatch(future::value(f), error = function(e) c(pid = NA_character_, path = conditionMessage(e)))
  check("worker path matches session", identical(unname(got[["path"]]), path))
  check("worker is a different pid", !identical(unname(got[["pid"]]), as.character(Sys.getpid())))
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
  worker_line <- any(grepl("worker_probe", lines, fixed = TRUE)) &&
    any(grepl("session_probe", lines, fixed = TRUE))
  future::plan(future::sequential)
}
check("worker and session share one JSONL", isTRUE(worker_line))

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
