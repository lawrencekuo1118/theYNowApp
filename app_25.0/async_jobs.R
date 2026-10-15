# ==========================================
# async_jobs.R — non-blocking feature runners (ExtendedTask + future)
# ==========================================
# Shiny's main R loop is single-threaded: synchronous withProgress blocks every
# other observeEvent in the session. Long Lab / HFV jobs therefore run via
# shiny::ExtendedTask + promises::future_promise so at least two features can
# compute concurrently (e.g. Blue Chip + Clustering, or Blue Chip + HFV).

.ynow_async_plan_ready <- FALSE

#' Initialise a 2-worker future plan (multicore when safe, else multisession).
.ynow_async_init <- function() {
  if (isTRUE(.ynow_async_plan_ready)) return(invisible(TRUE))
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("promises", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  workers <- tryCatch(
    {
      n <- as.integer(future::availableCores())
      if (!is.finite(n) || n < 1L) n <- 1L
      max(1L, min(2L, n))
    },
    error = function(e) 2L
  )
  # Default to multisession (socket workers). Multicore/fork is unsafe once
  # reticulate/Python has started (common in Blue Chip / Yahoo scrape paths).
  # Set YNOW_ASYNC_MULTICORE=1 to force fork when you know Python is unused.
  force_mc <- identical(Sys.getenv("YNOW_ASYNC_MULTICORE"), "1")
  ok <- tryCatch(
    {
      if (isTRUE(force_mc) && isTRUE(future::supportsMulticore())) {
        future::plan(future::multicore, workers = workers)
      } else {
        future::plan(future::multisession, workers = workers)
      }
      TRUE
    },
    error = function(e) {
      tryCatch(
        {
          future::plan(future::sequential)
          TRUE
        },
        error = function(e2) FALSE
      )
    }
  )
  .ynow_async_plan_ready <<- isTRUE(ok)
  invisible(isTRUE(ok))
}

#' Whether async runners are available this process.
.ynow_async_available <- function() {
  requireNamespace("future", quietly = TRUE) &&
    requireNamespace("promises", quietly = TRUE) &&
    exists("ExtendedTask", envir = asNamespace("shiny"), inherits = FALSE)
}

#' Wrap a pure job in a future promise (call from ExtendedTask$new(func)).
#' @param expr Expression or braced block evaluated in a background worker.
.ynow_async_promise <- function(expr) {
  .ynow_async_init()
  expr <- substitute(expr)
  promises::future_promise(
    expr = expr,
    envir = parent.frame(),
    seed = TRUE
  )
}

#' Build an ExtendedTask that runs `job_fn(...)` off the session thread.
#' `job_fn` must be a named function (not a closure over reactives).
.ynow_make_extended_task <- function(job_fn) {
  if (!is.function(job_fn)) stop("job_fn must be a function")
  # Capture by name so multisession workers re-resolve from global env when possible.
  job_name <- NULL
  if (is.character(attr(job_fn, "ynow_job_name"))) {
    job_name <- attr(job_fn, "ynow_job_name")[1]
  }
  shiny::ExtendedTask$new(function(...) {
    args <- list(...)
    jn <- job_name
    jf <- job_fn
    promises::future_promise(
      {
        fn <- jf
        if (!is.null(jn) && nzchar(jn) && exists(jn, mode = "function", inherits = TRUE)) {
          fn <- get(jn, mode = "function", inherits = TRUE)
        }
        do.call(fn, args)
      },
      seed = TRUE
    )
  })
}
