# Sequential ExtendedTask job lookup: Shiny app env is not .GlobalEnv.
# Hosted shinyapps uses future::sequential; jobs must resolve by name.
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
root <- ynow_test_app_dir("async_jobs.R")
check("resolves live-line app_27.0", basename(root) == "app_27.0")

async_src <- paste(
  readLines(file.path(root, "async_jobs.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check(
  "defines attach helper",
  grepl(".ynow_async_attach_job_env", async_src, fixed = TRUE)
)
check(
  "hosted sequential calls attach",
  grepl("hosted_memory_safe", async_src, fixed = TRUE) &&
    grepl(".ynow_async_attach_job_env()", async_src, fixed = TRUE)
)

if (!requireNamespace("future", quietly = TRUE) ||
    !requireNamespace("promises", quietly = TRUE)) {
  cat("SKIP runtime (future/promises missing)\n")
  cat("\nPassed:", pass, " Failed:", fail, "\n")
  if (fail > 0L) quit(status = 1L)
  quit(status = 0L)
}

# Isolate: force sequential, no multisession preload.
Sys.setenv(
  YNOW_ASYNC_SEQUENTIAL = "1",
  YNOW_ASYNC_SKIP_WORKER_PRELOAD = "1",
  YNOW_ASYNC_FORCE_PARALLEL = ""
)
# Detach leftover attach from prior tests.
if ("ynow:worker" %in% search()) {
  try(detach("ynow:worker", unload = FALSE, character.only = TRUE), silent = TRUE)
}

# Simulate Shiny: source into a private app env (not .GlobalEnv).
app_env <- new.env(parent = .GlobalEnv)
sys.source(file.path(root, "async_jobs.R"), envir = app_env)
# Minimal job stubs (same names ExtendedTask resolves).
app_env$lab_im_run_catalog_job <- function(market_mode = "US",
                                           boards = character(),
                                           force = FALSE,
                                           key = "") {
  list(ok = TRUE, key = key, market_mode = market_mode)
}
app_env$lab_im_run_eval_job <- function(...) list(ok = TRUE)
app_env$.ynow_async_plan_ready <- FALSE

ok_init <- tryCatch(
  eval(quote(.ynow_async_init()), envir = app_env),
  error = function(e) {
    cat("init err: ", conditionMessage(e), "\n", sep = "")
    FALSE
  }
)
check("async init ok under sequential", isTRUE(ok_init))
check(
  "attach put app env on search()",
  "ynow:worker" %in% search() ||
    identical(environment(app_env$lab_im_run_catalog_job), .GlobalEnv)
)

# Name-only future (mirrors .ynow_make_extended_task) must resolve catalog job.
jn <- "lab_im_run_catalog_job"
resolved <- tryCatch(
  {
    future::value(future::future(
      {
        exists(jn, mode = "function", inherits = TRUE) ||
          ("ynow:worker" %in% search() &&
             exists(jn, where = "ynow:worker", mode = "function"))
      },
      globals = list(jn = jn),
      seed = TRUE
    ))
  },
  error = function(e) {
    cat("future err: ", conditionMessage(e), "\n", sep = "")
    FALSE
  }
)
check("sequential future sees lab_im_run_catalog_job", isTRUE(resolved))

# End-to-end: ExtendedTask-style do.call by name.
ran <- tryCatch(
  {
    future::value(future::future(
      {
        fn <- get(jn, mode = "function", inherits = TRUE)
        do.call(fn, list(market_mode = "US", key = "k1"))
      },
      globals = list(jn = jn),
      seed = TRUE
    ))
  },
  error = function(e) {
    cat("do.call err: ", conditionMessage(e), "\n", sep = "")
    NULL
  }
)
check(
  "sequential future runs catalog job by name",
  is.list(ran) && isTRUE(ran$ok) && identical(ran$key, "k1")
)

# Cleanup attach so later tests are not polluted.
if ("ynow:worker" %in% search()) {
  try(detach("ynow:worker", unload = FALSE, character.only = TRUE), silent = TRUE)
}
tryCatch(future::plan(future::sequential), error = function(e) NULL)

cat("\nPassed:", pass, " Failed:", fail, "\n")
if (fail > 0L) quit(status = 1L)
