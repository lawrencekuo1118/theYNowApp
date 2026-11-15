# Prospecting first-load trace (diagnostic only).
# Default OFF. Set YNOW_PROSPECTING_LOAD_TRACE=1 before the R process starts.
# Path: YNOW_PROSPECTING_LOAD_TRACE_PATH. If unset, the loading process publishes
# one absolute file (not tempdir(): each multisession worker has its own).
# Futures also call ynow_pload_bind() so a worker writes that file even when
# it was spawned before the env was set.
# Opt-in experiment gates (also default OFF, no effect unless set):
#   YNOW_PROSPECTING_SKIP_AUTO_EVAL=1
#   YNOW_PROSPECTING_SKIP_IND_MCAP=1
#   YNOW_PROSPECTING_DEFER_CATALOG=1
#   YNOW_PROSPECTING_PREWARM_CATALOG=1

.ynow_pload_enabled_cache <- NULL

ynow_pload_enabled <- function() {
  if (!is.null(.ynow_pload_enabled_cache)) return(isTRUE(.ynow_pload_enabled_cache))
  v <- tolower(trimws(Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE", "")))
  .ynow_pload_enabled_cache <<- v %in% c("1", "true", "yes")
  isTRUE(.ynow_pload_enabled_cache)
}

#' Absolute JSONL path shared by the session and its future workers.
#' Pins YNOW_PROSPECTING_LOAD_TRACE_PATH in this process when it was empty.
ynow_pload_path <- function() {
  path <- Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE_PATH", "")
  if (!nzchar(path)) {
    base <- if (dir.exists("/tmp")) {
      "/tmp"
    } else {
      ad <- Sys.getenv("YNOW_APP_DIR", "")
      if (nzchar(ad)) ad else getwd()
    }
    path <- file.path(base, sprintf("ynow-prospecting-load-%s.jsonl", Sys.getpid()))
  }
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE_PATH = path)
  path
}

#' Point this process at the session JSONL and drop the enabled-flag cache.
#' Call from a future before ynow_pload(); workers do not see a later setenv.
ynow_pload_bind <- function(path = "", run = "", on = FALSE) {
  if (isTRUE(on)) Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE = "1")
  path <- if (is.null(path) || !length(path)) "" else as.character(path)[1]
  run <- if (is.null(run) || !length(run)) "" else as.character(run)[1]
  if (is.na(path)) path <- ""
  if (is.na(run)) run <- ""
  if (nzchar(path)) Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE_PATH = path)
  if (nzchar(run)) Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE_RUN = run)
  .ynow_pload_enabled_cache <<- NULL
  invisible(TRUE)
}

ynow_prospecting_diag <- function(name) {
  v <- tolower(trimws(Sys.getenv(as.character(name)[1], "")))
  v %in% c("1", "true", "yes")
}

ynow_pload <- function(node, phase = "mark", extra = NULL) {
  if (!isTRUE(ynow_pload_enabled())) return(invisible(FALSE))
  path <- ynow_pload_path()
  rec <- list(
    t_unix = round(as.numeric(Sys.time()), 4),
    t_proc = round(unname(proc.time()[["elapsed"]]), 4),
    pid = Sys.getpid(),
    node = as.character(node)[1],
    phase = as.character(phase)[1],
    run = Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE_RUN", "")
  )
  if (is.list(extra)) {
    for (nm in names(extra)) {
      val <- extra[[nm]]
      if (length(val) == 1L && !is.null(val) &&
          (is.numeric(val) || is.character(val) || is.logical(val))) {
        rec[[nm]] <- if (is.numeric(val)) round(as.numeric(val), 4) else val
      }
    }
  }
  line <- tryCatch(
    paste0(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null"), "\n"),
    error = function(e) NULL
  )
  if (is.null(line)) return(invisible(FALSE))
  tryCatch(cat(line, file = path, append = TRUE), error = function(e) NULL)
  invisible(TRUE)
}

#' Time `expr` on the caller thread when tracing is on. No-op wrapper otherwise.
ynow_pload_time <- function(node, expr) {
  if (!isTRUE(ynow_pload_enabled())) return(expr)
  t0 <- unname(proc.time()[["elapsed"]])
  ynow_pload(node, "start")
  on.exit(
    ynow_pload(node, "end", list(elapsed_s = unname(proc.time()[["elapsed"]]) - t0)),
    add = TRUE
  )
  force(expr)
}

# Publish the path before multisession workers are planned (global.R sources
# this file, then calls .ynow_async_init). No-op when the trace flag is off.
if (isTRUE(ynow_pload_enabled())) {
  tryCatch(ynow_pload_path(), error = function(e) NULL)
}
