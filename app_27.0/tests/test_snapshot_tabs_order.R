# Snapshot sub-tabs: Current App Parameter Snapshot (snap_current) before Manual adjustments.
# Run: Rscript app_27.0/tests/test_snapshot_tabs_order.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_snapshot_tabs_order.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    bad <<- bad + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
  invisible(isTRUE(cond))
}

source(file.path(app_dir, "tests", "ynow_test_sources.R"))
src <- ynow_test_ui_sources(app_dir)
ui <- src$ui
shipped <- src$shipped

snap_start <- regexpr('id = "snapshot_report"', ui, fixed = TRUE)[1]
check("snapshot_report tabBox found", snap_start > 0)

snap_chunk <- if (snap_start > 0) {
  substr(ui, snap_start, min(nchar(ui), snap_start + 12000L))
} else {
  ""
}

pos_audit <- regexpr('value = "snap_audit"', snap_chunk, fixed = TRUE)[1]
pos_current <- regexpr('value = "snap_current"', snap_chunk, fixed = TRUE)[1]
pos_defaults <- regexpr('value = "snap_defaults"', snap_chunk, fixed = TRUE)[1]

check("snap_audit panel present", pos_audit > 0)
check("snap_current panel present", pos_current > 0)
check("snap_defaults panel present", pos_defaults > 0)
check(
  "LTR order: snap_current before snap_audit",
  pos_current > 0 && pos_audit > 0 && pos_current < pos_audit
)
check(
  "LTR order: snap_audit before snap_defaults",
  pos_audit > 0 && pos_defaults > 0 && pos_audit < pos_defaults
)
check(
  "tabBox selected defaults to snap_current",
  grepl('selected = "snap_current"', snap_chunk, fixed = TRUE)
)

# Lite / leave-sysadmin fallbacks prefer Current snapshot first (leftmost free tab).
# Selectors are unescaped in ynow_app.js.
lite_fn <- regexpr("function ensureLiteSnapshotFreeTab", shipped, fixed = TRUE)[1]
lite_chunk <- if (lite_fn > 0) substr(shipped, lite_fn, min(nchar(shipped), lite_fn + 900L)) else ""
audit_fb <- regexpr('data-value="snap_audit"', lite_chunk, fixed = TRUE)[1]
current_fb <- regexpr('data-value="snap_current"', lite_chunk, fixed = TRUE)[1]
check(
  "ensureLiteSnapshotFreeTab prefers snap_current",
  grepl("ensureLiteSnapshotFreeTab", shipped, fixed = TRUE) &&
    audit_fb > 0 && current_fb > 0 && current_fb < audit_fb
)

sys_leave <- regexpr("Leaving sysadmin: leave APP_DEFAULTS", shipped, fixed = TRUE)[1]
sys_chunk <- if (sys_leave > 0) substr(shipped, sys_leave, min(nchar(shipped), sys_leave + 700L)) else ""
sys_audit <- regexpr('data-value="snap_audit"', sys_chunk, fixed = TRUE)[1]
sys_current <- regexpr('data-value="snap_current"', sys_chunk, fixed = TRUE)[1]
check(
  "leave-sysadmin fallback prefers snap_current",
  sys_audit > 0 && sys_current > 0 && sys_current < sys_audit
)

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
invisible(NULL)
