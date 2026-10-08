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
.ynow_dbg_radar_focus <- function(hypothesisId, location, message, data = list()) {
  .coalesce <- function(x, d = "") {
    if (is.null(x) || length(x) < 1L || (is.atomic(x) && is.na(x[1]))) d else x[1]
  }
  payload <- list(
    id = paste0("log_", as.integer(as.numeric(Sys.time()) * 1000) %% 1e9, "_", sample.int(1e6, 1)),
    timestamp = as.numeric(Sys.time()) * 1000,
    hypothesisId = as.character(.coalesce(hypothesisId, "")),
    location = as.character(.coalesce(location, "")),
    message = as.character(.coalesce(message, "")),
    data = data,
    runId = "radar-focus"
  )
  line <- tryCatch(
    paste0(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"), "\n"),
    error = function(e) {
      paste0(
        '{"hypothesisId":"', as.character(hypothesisId)[1],
        '","location":"', as.character(location)[1],
        '","message":"json_fail","timestamp":', as.numeric(Sys.time()) * 1000, "}\n"
      )
    }
  )
  paths <- c(
    "/workspace/.cursor/debug-radar-focus.ndjson",
    "/opt/cursor/logs/debug.log"
  )
  for (p in paths) {
    tryCatch({
      dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
      cat(line, file = p, append = TRUE)
    }, error = function(e) invisible(NULL))
  }
  invisible(NULL)
}
# #endregion
