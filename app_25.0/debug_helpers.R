# ==========================================
# debug_helpers.R — runtime log gate only
# ==========================================
# Sourced by global.R. Does not load debug_lab.R.
# Verbose logs run only when YNOW_DEBUG=1 (or TRUE/yes).
# YNOW_DEBUG_SKIP_PY=1 still skips Python init (local parse checks).

.ynow_debug_on <- function() {
  v <- tolower(trimws(Sys.getenv("YNOW_DEBUG", "")))
  v %in% c("1", "true", "yes", "on")
}

.ynow_log <- function(...) {
  if (.ynow_debug_on()) message(...)
  invisible(NULL)
}

# Temporary agent NDJSON debug (sidebar-dim / admin-empty). Always writes; no secrets.
.ynow_agent_dbg <- function(hypothesisId, location, message, data = list()) {
  # #region agent log
  tryCatch({
    path <- "/opt/cursor/logs/debug.log"
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    payload <- list(
      id = paste0("log_", as.integer(Sys.time()), "_", sample.int(1e6, 1)),
      timestamp = as.numeric(Sys.time()) * 1000,
      location = as.character(location)[1],
      message = as.character(message)[1],
      data = if (is.list(data)) data else list(value = data),
      hypothesisId = as.character(hypothesisId)[1]
    )
    line <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", force = TRUE)
    cat(as.character(line), "\n", file = path, append = TRUE, sep = "")
  }, error = function(e) invisible(NULL))
  # #endregion
  invisible(NULL)
}
