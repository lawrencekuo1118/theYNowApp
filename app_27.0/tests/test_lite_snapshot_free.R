# Lite Snapshot stays free: keep snap_audit + snap_current; only hide snap_defaults.
# Run: Rscript app_27.0/tests/test_lite_snapshot_free.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_lite_snapshot_free.R", mustWork = FALSE)
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

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "Lite CSS hides snap_defaults",
  grepl(
    'body.ynow-lite #snapshot_report > li:has(> a[data-value="snap_defaults"])',
    ui,
    fixed = TRUE
  ) || grepl(
    "body.ynow-lite #snapshot_report > li:has(> a[data-value=\"snap_defaults\"])",
    ui,
    fixed = TRUE
  )
)
check(
  "Lite CSS does not hide snap_audit",
  !grepl(
    'body.ynow-lite #snapshot_report > li:has(> a[data-value="snap_audit"])',
    ui,
    fixed = TRUE
  ) && !grepl(
    "body.ynow-lite[\\s\\S]{0,120}data-value=\"snap_audit\"[\\s\\S]{0,80}display:\\s*none",
    ui,
    perl = TRUE
  )
)
check(
  "Lite CSS does not hide snap_current",
  !grepl(
    'body.ynow-lite #snapshot_report > li:has(> a[data-value="snap_current"])',
    ui,
    fixed = TRUE
  )
)
check(
  "JS ensureLiteSnapshotFreeTab prefers snap_current",
  grepl("ensureLiteSnapshotFreeTab", ui, fixed = TRUE) &&
    grepl('data-value=\\"snap_current\\"', ui, fixed = TRUE) &&
    grepl("Never land on locked APP_DEFAULTS", ui, fixed = TRUE)
)
check(
  "Foot Snapshot entry uses free-tab helper",
  grepl("ensureLiteSnapshotFreeTab", ui, fixed = TRUE) &&
    grepl("ynowShowSidebarFootTab", ui, fixed = TRUE)
)
check(
  "Lite click gate still skips proprietary download gate",
  grepl("if (document.body.classList.contains('ynow-lite')) return;", ui, fixed = TRUE) &&
    grepl("'download_snapshot'", ui, fixed = TRUE) &&
    grepl("ids.map(function (id) { return '#' + id; })", ui, fixed = TRUE)
)
check(
  "Full still shows snap_defaults lock until sysadmin",
  grepl(
    "body:not(.ynow-lite):not(.ynow-sysadmin-on) #snapshot_report a[data-value=\"snap_defaults\"]",
    ui,
    fixed = TRUE
  ) || grepl("ynow-snap-tab-lock", ui, fixed = TRUE)
)

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
invisible(NULL)
