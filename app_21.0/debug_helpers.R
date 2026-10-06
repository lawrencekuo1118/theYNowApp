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

# #region agent log
#' NDJSON debug logger for Multiples / composite / DCF investigation (TSM).
.ynow_dbg_mult <- function(hypothesisId, location, message, data = list(),
                           runId = "pre-fix") {
  payload <- list(
    id = paste0("log_", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, trim = TRUE)),
    timestamp = as.numeric(Sys.time()) * 1000,
    location = as.character(location)[1],
    message = as.character(message)[1],
    data = data,
    hypothesisId = as.character(hypothesisId)[1],
    sessionId = "multiples-tsm",
    runId = as.character(runId)[1]
  )
  line <- tryCatch({
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = NA)
    } else {
      sprintf(
        '{"hypothesisId":"%s","location":"%s","message":"%s","timestamp":%s,"data":{}}',
        hypothesisId, location, gsub("\"", "'", message),
        format(as.numeric(Sys.time()) * 1000, scientific = FALSE, trim = TRUE)
      )
    }
  }, error = function(e) NULL)
  if (!is.null(line) && nzchar(line)) {
    try(cat(line, "\n", file = "/workspace/.cursor/debug-multiples-tsm.ndjson", append = TRUE), silent = TRUE)
    try(cat(line, "\n", file = "/opt/cursor/logs/debug.log", append = TRUE), silent = TRUE)
  }
  invisible(NULL)
}
# #endregion
