#!/usr/bin/env Rscript
# Catalog ExtendedTask must resolve on sequential (hosted / workers=1)
# and on multisession workers that preloaded the app.
# Run: cd app_27.0/tests && Rscript test_async_catalog_job.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

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

if (!requireNamespace("future", quietly = TRUE) ||
    !requireNamespace("promises", quietly = TRUE) ||
    !requireNamespace("later", quietly = TRUE) ||
    !requireNamespace("shiny", quietly = TRUE)) {
  cat("SKIP missing future/promises/later/shiny\n")
  quit(status = 0L)
}

# Source into a child env, like app.R local=TRUE. Do not leak job fns to .GlobalEnv.
app_env <- new.env(parent = globalenv())
sys.source(file.path(app_dir, "async_jobs.R"), envir = app_env)
app_env$lab_im_run_catalog_job <- function(market_mode = "US") {
  list(ok = TRUE, market = as.character(market_mode)[1], via = "closure")
}

collect_promise <- function(p, timeout_s = 8) {
  box <- new.env(parent = emptyenv())
  promises::then(
    p,
    onFulfilled = function(v) box$val <- v,
    onRejected = function(e) box$err <- conditionMessage(e)
  )
  t0 <- Sys.time()
  repeat {
    later::run_now(timeoutSecs = 0.05)
    if (!is.null(box$val) || !is.null(box$err)) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_s) {
      box$err <- "timeout"
      break
    }
  }
  box
}

# --- sequential: name lookup in .GlobalEnv misses; closure must run ---
future::plan(future::sequential)
check("sequential backend detected", isTRUE(app_env$.ynow_async_backend_sequential()))
check("catalog job absent from .GlobalEnv",
      !exists("lab_im_run_catalog_job", envir = globalenv(), inherits = FALSE))

fn <- app_env$lab_im_run_catalog_job
attr(fn, "ynow_job_name") <- "lab_im_run_catalog_job"
# Production path: register in this process, then future_promise name lookup.
app_env$.ynow_async_job_promise_for_test <- function(job_fn, job_name, args) {
  .ynow_async_register_job(job_name, job_fn)
  jn <- job_name
  promises::future_promise(
    {
      fn <- NULL
      if (nzchar(jn) && exists(jn, mode = "function", inherits = TRUE)) {
        fn <- get(jn, mode = "function", inherits = TRUE)
      }
      if (!is.function(fn) &&
          exists(".ynow_async_jobs", envir = globalenv(), inherits = FALSE)) {
        box <- get(".ynow_async_jobs", envir = globalenv(), inherits = FALSE)
        if (exists(jn, envir = box, inherits = FALSE, mode = "function")) {
          fn <- get(jn, envir = box, inherits = FALSE, mode = "function")
        }
      }
      if (!is.function(fn)) {
        stop("async job is not loaded in this process: ", jn, call. = FALSE)
      }
      do.call(fn, args)
    },
    globals = list(jn = jn, args = args),
    seed = TRUE
  )
}
environment(app_env$.ynow_async_job_promise_for_test) <- app_env

p_seq <- app_env$.ynow_async_job_promise_for_test(
  fn, "lab_im_run_catalog_job", list(market_mode = "TW")
)
got <- collect_promise(p_seq)
check("sequential catalog job resolves via closure", {
  is.null(got$err) && is.list(got$val) &&
    isTRUE(got$val$ok) && identical(got$val$market, "TW") &&
    identical(got$val$via, "closure")
})

# Hosted flag forces in-process even if a caller left a multisession plan.
Sys.setenv(YNOW_ASYNC_SEQUENTIAL = "1")
check("hosted sequential flag forces in-process",
      isTRUE(app_env$.ynow_async_backend_sequential()))
Sys.unsetenv("YNOW_ASYNC_SEQUENTIAL")

# --- multisession workers: startup attaches the job; name lookup works ---
startup_dir <- tempfile("ynow-cat-job-")
dir.create(startup_dir)
writeLines(
  "lab_im_run_catalog_job <- function(market_mode = 'US') list(ok = TRUE, market = as.character(market_mode)[1], via = 'worker')\n",
  file.path(startup_dir, "global.R")
)
startup <- sprintf(paste(c(
  "Sys.setenv(YNOW_FUTURE_WORKER='1')",
  "setwd('%s')",
  "env <- new.env(parent = .GlobalEnv)",
  "sys.source('global.R', envir = env)",
  "if (!('ynow:worker' %%in%% search())) attach(env, name = 'ynow:worker', warn.conflicts = FALSE)"
), collapse = "\n"), gsub("'", "", normalizePath(startup_dir, winslash = "/"), fixed = TRUE))

future::plan(future::multisession, workers = 2L, rscript_startup = startup)
check("two-worker plan is not sequential", !isTRUE(app_env$.ynow_async_backend_sequential()))
p_ms <- app_env$.ynow_async_job_promise_for_test(
  fn, "lab_im_run_catalog_job", list(market_mode = "US")
)
got_ms <- collect_promise(p_ms, timeout_s = 15)
check("multisession catalog job resolves from worker preload", {
  is.null(got_ms$err) && is.list(got_ms$val) &&
    isTRUE(got_ms$val$ok) && identical(got_ms$val$via, "worker")
})

# future>=1.82: multisession workers=1 is SequentialFutureBackend (no startup).
future::plan(future::multisession, workers = 1L, rscript_startup = startup)
check("workers=1 multisession collapses to sequential",
      isTRUE(app_env$.ynow_async_backend_sequential()))
p_w1 <- app_env$.ynow_async_job_promise_for_test(
  fn, "lab_im_run_catalog_job", list(market_mode = "US")
)
got_w1 <- collect_promise(p_w1)
check("workers=1 catalog job still resolves via closure", {
  is.null(got_w1$err) && is.list(got_w1$val) && identical(got_w1$val$via, "closure")
})

future::plan(future::sequential)
unlink(startup_dir, recursive = TRUE)

# The production ExtendedTask must branch on the sequential backend.
async_src <- paste(readLines(file.path(app_dir, "async_jobs.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("ExtendedTask registers jobs for sequential lookup",
      grepl(".ynow_async_register_job", async_src, fixed = TRUE) &&
        grepl(".ynow_async_jobs", async_src, fixed = TRUE) &&
        grepl("lab_im_run_catalog_job", async_src, fixed = TRUE))

cat("\nPassed:", pass, " Failed:", fail, "\n")
if (fail > 0L) quit(status = 1L)
