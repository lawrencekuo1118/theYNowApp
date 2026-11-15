# Prospecting first-load trace (diagnostic only).
# Default OFF. Set YNOW_PROSPECTING_LOAD_TRACE=1 before the R process starts.
# Path: YNOW_PROSPECTING_LOAD_TRACE_PATH. If unset, the loading process publishes
# one absolute /tmp file into the environment before future workers spawn.
# Do not use tempdir(): each multisession worker has its own temp dir, so
# catalog / F-Score lines would miss the session JSONL.
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

ynow_prospecting_diag <- function(name) {
  v <- tolower(trimws(Sys.getenv(as.character(name)[1], "")))
  v %in% c("1", "true", "yes")
}

# One shared JSONL for the session and its future workers. Resolved once at
# load (see the call below, which runs before .ynow_async_init) and stored in
# the process environment so workers inherit the path instead of tempdir().
ynow_pload_path <- function() {
  path <- trimws(Sys.getenv("YNOW_PROSPECTING_LOAD_TRACE_PATH", ""))
  if (!nzchar(path)) {
    path <- sprintf("/tmp/ynow-prospecting-load-%s.jsonl", Sys.getpid())
    Sys.setenv(YNOW_PROSPECTING_LOAD_TRACE_PATH = path)
  }
  path
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
