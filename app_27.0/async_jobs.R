# ==========================================
# async_jobs.R — non-blocking feature runners (ExtendedTask + future)
# ==========================================
# Shiny's main R loop is single-threaded: synchronous withProgress blocks every
# other observeEvent in the session. Long Lab / HFV jobs therefore run via
# shiny::ExtendedTask + promises::future_promise so at least two features can
# compute concurrently (e.g. Blue Chip + Clustering, or Blue Chip + HFV).

.ynow_async_plan_ready <- FALSE

#' Whether this process should avoid spawning extra future workers (memory).
#' Hosted shinyapps workers already multiplex many browser sessions; each
#' multisession child can double/triple RSS and OOM the instance under load.
.ynow_async_hosted_memory_safe <- function() {
  if (identical(Sys.getenv("YNOW_ASYNC_FORCE_PARALLEL"), "1")) return(FALSE)
  if (identical(Sys.getenv("YNOW_ASYNC_SEQUENTIAL"), "1")) return(TRUE)
  if (exists(".ynow_is_hosted_connect", mode = "function")) {
    return(isTRUE(.ynow_is_hosted_connect()))
  }
  nzchar(Sys.getenv("SHINY_SERVER_VERSION")) ||
    grepl("shinyapps", Sys.getenv("HOSTNAME"), ignore.case = TRUE) ||
    grepl("/srv/connect/apps", tryCatch(getwd(), error = function(e) ""), fixed = TRUE)
}

#' App directory that contains global.R, or "" when this process is not the app.
.ynow_async_app_dir <- function() {
  env <- Sys.getenv("YNOW_APP_DIR", "")
  if (nzchar(env) && file.exists(file.path(env, "global.R"))) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  wd <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = FALSE), error = function(e) getwd())
  parent <- tryCatch(normalizePath(file.path(wd, ".."), winslash = "/", mustWork = FALSE), error = function(e) "")
  cands <- c(wd, file.path(wd, "app_27.0"), parent)
  for (cand in cands) {
    if (nzchar(cand) &&
        file.exists(file.path(cand, "global.R")) &&
        file.exists(file.path(cand, "async_jobs.R"))) {
      return(normalizePath(cand, winslash = "/", mustWork = FALSE))
    }
  }
  ""
}

#' rscript_startup script: load the app onto the worker search path once.
#' future() wipes .GlobalEnv before each task, so an attached env is what survives.
#' Parent of that env is .GlobalEnv so lexical lookup still reaches library() packages.
.ynow_async_worker_startup <- function(app_dir) {
  app_dir <- gsub("'", "", normalizePath(app_dir, winslash = "/", mustWork = FALSE), fixed = TRUE)
  paste(c(
    "Sys.setenv(YNOW_FUTURE_WORKER='1')",
    sprintf("Sys.setenv(YNOW_APP_DIR='%s')", app_dir),
    sprintf("setwd('%s')", app_dir),
    "env <- new.env(parent = .GlobalEnv)",
    "sys.source('global.R', envir = env)",
    "if (!('ynow:worker' %in% search())) attach(env, name = 'ynow:worker', warn.conflicts = FALSE)"
  ), collapse = "\n")
}

#' Initialise a future plan (local: up to 2 workers; hosted: sequential).
.ynow_async_init <- function() {
  if (identical(Sys.getenv("YNOW_FUTURE_WORKER"), "1")) return(invisible(FALSE))
  if (isTRUE(.ynow_async_plan_ready)) return(invisible(TRUE))
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("promises", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  # Pin one shared trace file before workers are spawned so they inherit it.
  if (exists("ynow_pload_enabled", mode = "function") &&
      exists("ynow_pload_path", mode = "function") &&
      isTRUE(ynow_pload_enabled())) {
    tryCatch(ynow_pload_path(), error = function(e) NULL)
  }
  # shinyapps / Connect: sequential only — ExtendedTask still yields the session
  # loop, but we do not fork extra R processes that starve co-tenants.
  if (isTRUE(.ynow_async_hosted_memory_safe())) {
    ok <- tryCatch(
      {
        future::plan(future::sequential)
        TRUE
      },
      error = function(e) FALSE
    )
    .ynow_async_plan_ready <<- isTRUE(ok)
    return(invisible(isTRUE(ok)))
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
  app_dir <- .ynow_async_app_dir()
  # Unit tests that source only this file have no job functions yet. Skip the
  # full app preload so those futures stay cheap. The running app has already
  # sourced lab_im_run_eval_job before this init.
  preload <- nzchar(app_dir) &&
    exists("lab_im_run_eval_job", mode = "function", inherits = TRUE) &&
    !identical(Sys.getenv("YNOW_ASYNC_SKIP_WORKER_PRELOAD"), "1")
  ok <- tryCatch(
    {
      if (isTRUE(force_mc) && isTRUE(future::supportsMulticore())) {
        future::plan(future::multicore, workers = workers)
      } else if (isTRUE(preload)) {
        future::plan(
          future::multisession,
          workers = workers,
          rscript_startup = .ynow_async_worker_startup(app_dir)
        )
        ready <- tryCatch(
          future::value(future::future({
            exists("lab_im_run_eval_job", mode = "function", inherits = TRUE) &&
              exists("lab_im_run_catalog_job", mode = "function", inherits = TRUE) &&
              exists("lab_cluster_run_job", mode = "function", inherits = TRUE) &&
              exists("lab_im_run_ind_mcap_job", mode = "function", inherits = TRUE) &&
              exists("lab_im_catalog_memo_key", mode = "function", inherits = TRUE)
          }, globals = FALSE, seed = TRUE)),
          error = function(e) FALSE
        )
        if (!isTRUE(ready)) {
          stop("worker preload did not expose job functions", call. = FALSE)
        }
      } else {
        future::plan(future::multisession, workers = workers)
      }
      TRUE
    },
    error = function(e) {
      message("ynow async multisession failed: ", conditionMessage(e))
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

#' TRUE when futures run in this process (hosted sequential, or future>=1.82
#' collapsing multisession workers=1 onto SequentialFutureBackend).
#' Job functions then live in the Shiny app env, not .GlobalEnv, so a
#' name lookup inside future() cannot see them unless they were registered.
.ynow_async_backend_sequential <- function() {
  if (isTRUE(.ynow_async_hosted_memory_safe())) return(TRUE)
  if (!requireNamespace("future", quietly = TRUE)) return(TRUE)
  backend <- tryCatch(future::plan("backend"), error = function(e) NULL)
  if (is.null(backend)) return(TRUE)
  inherits(backend, "SequentialFutureBackend")
}

#' Remember a job function in this process. Sequential futures eval in
#' .GlobalEnv and cannot see the Shiny app environment. The registry is a
#' binding only — it is not placed in future globals (that would serialize
#' the app environment and freeze the session).
.ynow_async_register_job <- function(job_name, job_fn) {
  if (!exists(".ynow_async_jobs", envir = globalenv(), inherits = FALSE)) {
    assign(".ynow_async_jobs", new.env(parent = emptyenv()), envir = globalenv())
  }
  box <- get(".ynow_async_jobs", envir = globalenv(), inherits = FALSE)
  assign(as.character(job_name)[1], job_fn, envir = box)
  invisible(TRUE)
}

#' Resolve a job by name: worker search path first, then this process's registry.
.ynow_async_resolve_job <- function(job_name) {
  jn <- as.character(job_name %||% "")[1]
  if (nzchar(jn) && exists(jn, mode = "function", inherits = TRUE)) {
    return(get(jn, mode = "function", inherits = TRUE))
  }
  if (nzchar(jn) && exists(".ynow_async_jobs", envir = globalenv(), inherits = FALSE)) {
    box <- get(".ynow_async_jobs", envir = globalenv(), inherits = FALSE)
    if (exists(jn, envir = box, inherits = FALSE, mode = "function")) {
      return(get(jn, envir = box, inherits = FALSE, mode = "function"))
    }
  }
  stop("async job is not loaded in this process: ", jn, call. = FALSE)
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
#' The future expression must not capture `job_fn`: future()'s automatic globals
#' walk freezes the Shiny session for many seconds.
#' Multisession workers resolve the name from the preloaded search path.
#' Sequential / hosted futures eval in .GlobalEnv, so the job is also
#' registered there (see `.ynow_async_register_job`).
.ynow_make_extended_task <- function(job_fn) {
  if (!is.function(job_fn)) stop("job_fn must be a function")
  job_name <- NULL
  if (is.character(attr(job_fn, "ynow_job_name"))) {
    job_name <- attr(job_fn, "ynow_job_name")[1]
  }
  if (is.null(job_name) || !nzchar(job_name)) {
    stop("job_fn must set attr ynow_job_name", call. = FALSE)
  }
  .ynow_async_register_job(job_name, job_fn)
  shiny::ExtendedTask$new(function(...) {
    args <- list(...)
    jn <- job_name
    # Re-register in case this process forked or the registry was cleared.
    if (is.function(job_fn)) .ynow_async_register_job(jn, job_fn)
    trace_on <- exists("ynow_pload_enabled", mode = "function", inherits = TRUE) &&
      isTRUE(ynow_pload_enabled())
    trace_path <- if (isTRUE(trace_on) &&
                      exists("ynow_pload_path", mode = "function", inherits = TRUE)) {
      ynow_pload_path()
    } else {
      ""
    }
    trace_run <- if (isTRUE(trace_on)) {
      Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE_RUN", "")
    } else {
      ""
    }
    promises::future_promise(
      {
        if (isTRUE(trace_on) &&
            exists("ynow_pload_bind", mode = "function", inherits = TRUE)) {
          ynow_pload_bind(trace_path, trace_run, TRUE)
        }
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
      globals = list(
        jn = jn,
        args = args,
        trace_on = trace_on,
        trace_path = trace_path,
        trace_run = trace_run
      ),
      seed = TRUE
    )
  })
}

# later callbacks run outside the originating observer. Keep both the session
# domain (notifications, currency context) and an isolated reactive read scope.
ynow_session_callback <- function(session, callback) {
  force(session); force(callback)
  function(...) {
    args <- list(...)
    if (isTRUE(session$isClosed())) return(invisible(NULL))
    shiny::withReactiveDomain(session, shiny::isolate(do.call(callback, args)))
  }
}
