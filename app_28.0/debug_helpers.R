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
