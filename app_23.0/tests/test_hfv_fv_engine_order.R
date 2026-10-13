#!/usr/bin/env Rscript
# HFV Chart overlay / Replay model pickers follow sidebar Fair Value order.
# Run: cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_hfv_fv_engine_order.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("setup.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "market_profile.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

want <- c("nav", "dcf", "ddm", "ri", "pb")
check("YNOW_FV_ENGINE_ORDER sidebar-aligned", identical(YNOW_FV_ENGINE_ORDER, want))
ch <- ynow_fv_engine_choices()
check("choices values order", identical(unname(ch), want))
check("choices labels", identical(names(ch), c("NAV", "DCF", "DDM", "RI", "P/B")))
check(
  "order helper shuffles",
  identical(ynow_order_fv_engines(c("pb", "nav", "ri", "dcf")), c("nav", "dcf", "ri", "pb"))
)
check("order helper drops multiples/sotp", identical(ynow_order_fv_engines(c("multiples", "sotp", "dcf")), "dcf"))

ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("overlay uses ynow_fv_engine_choices", grepl("bt_fv_models", ui_txt, fixed = TRUE) &&
        grepl("ynow_fv_engine_choices()", ui_txt, fixed = TRUE))
check("replay uses ynow_fv_engine_choices", grepl("bt_fv_replay_model", ui_txt, fixed = TRUE) &&
        grepl("ynow_fv_engine_choices()", ui_txt, fixed = TRUE))
# No hard-coded DCF-first choice block for these inputs
dcf_first_block <- grepl(
  'bt_fv_models[\\s\\S]{0,400}\"DCF\"\\s*=\\s*\"dcf\"[\\s\\S]{0,80}\"DDM\"',
  ui_txt,
  perl = TRUE
)
check("overlay not hard-coded DCF-first", !isTRUE(dcf_first_block))

srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("server orders via ynow_order_fv_engines", grepl("ynow_order_fv_engines", srv_txt, fixed = TRUE))
check(
  "specs list starts with nav",
  grepl("\\.bt_fv_model_specs[\\s\\S]{0,200}nav\\s*=\\s*list", srv_txt, perl = TRUE)
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASS\n")
