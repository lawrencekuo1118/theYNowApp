# Scenario check (every engine) lives on Snapshot → Current params, not Model Dashboard SGR.
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "ynow_ui.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Extract SGR box region (.dcf_core_params_box) vs Snapshot snap_current panel
sgr_start <- regexpr("\\.dcf_core_params_box\\s*<-\\s*function", ui, perl = TRUE)[1]
sgr_end <- regexpr("\\.dcf_two_stage_params_box\\s*<-\\s*function", ui, perl = TRUE)[1]
check("SGR box region found", sgr_start > 0 && sgr_end > sgr_start)
sgr_chunk <- if (sgr_start > 0 && sgr_end > sgr_start) {
  substr(ui, sgr_start, sgr_end - 1L)
} else {
  ""
}
check("SGR box no longer hosts txt_scenario_audit", !grepl("txt_scenario_audit", sgr_chunk, fixed = TRUE))

snap_start <- regexpr('value = \"snap_current\"', ui, perl = TRUE)[1]
snap_end <- regexpr('value = \"snap_defaults\"', ui, perl = TRUE)[1]
check("snap_current region found", snap_start > 0 && snap_end > snap_start)
snap_chunk <- if (snap_start > 0 && snap_end > snap_start) {
  substr(ui, snap_start, snap_end - 1L)
} else {
  ""
}
check("snap_current hosts txt_scenario_audit", grepl("txt_scenario_audit", snap_chunk, fixed = TRUE))
check(
  "scenario audit above snapshot_table",
  {
    a <- regexpr("txt_scenario_audit", snap_chunk, fixed = TRUE)[1]
    b <- regexpr("snapshot_table", snap_chunk, fixed = TRUE)[1]
    a > 0 && b > a
  }
)
check("server still renders txt_scenario_audit", grepl("output\\$txt_scenario_audit", srv, perl = TRUE))
check("en scenario help key", grepl("sgr_scenario_help", loc, fixed = TRUE))
check("zh scenario title", grepl("情境檢核（每個模型）", loc, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
