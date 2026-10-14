#!/usr/bin/env Rscript
# Audit closeout — structural checks for remaining P1-D / Phase 2 residuals.
# Run: cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_audit_closeout.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("ynow_server.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("PASS ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
setup <- paste(readLines("setup.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# P1D-DCF-TAB-NA fix: tab visit fires DCF when ready and FV missing
check(
  "DCF tab-visit auto-calc observer present",
  grepl("dcf_calculator", srv, fixed = TRUE) &&
    grepl("stock_price_estimate_val", srv, fixed = TRUE) &&
    grepl("\\.fire_auto_calc_primary\\(\"dcf\"\\)", srv) &&
    grepl("P1-D closeout", srv, fixed = TRUE)
)
check(
  "Companion DDM/RI/MULT tab-visit fire present",
  grepl("ddm_calculator", srv, fixed = TRUE) &&
    grepl("ri_calculator", srv, fixed = TRUE) &&
    grepl("rel_multiples_calculator", srv, fixed = TRUE) &&
    grepl("\\.fire_auto_calc_primary\\(key\\)", srv)
)

# Rf fallback UI surfacing
check(
  "capm_rf_fallback_note in en locale",
  grepl("capm_rf_fallback_note\\s*=\\s*\"Using engineering fallback Rf", loc)
)
check(
  "capm_rf_fallback_note in zh-TW locale",
  grepl("capm_rf_fallback_note\\s*=\\s*\"目前使用工程 fallback Rf", loc)
)
check(
  "Rf note UI checks is_fallback",
  grepl("is_fallback", srv, fixed = TRUE) &&
    grepl("capm_rf_fallback_note", srv, fixed = TRUE)
)

# Normalize fix still present (Phase 2)
check(
  "reorder_financial_columns uses integer indices",
  grepl("period_idx\\s*<-\\s*seq\\.int", setup) &&
    grepl("make\\.unique", setup)
)

# Locale key parity for new string
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
check(
  "capm_rf_fallback_note exists in en and zh-TW",
  !is.null(.UI_STRINGS$en$capm_rf_fallback_note) &&
    !is.null(.UI_STRINGS$`zh-TW`$capm_rf_fallback_note) &&
    nzchar(.UI_STRINGS$en$capm_rf_fallback_note) &&
    nzchar(.UI_STRINGS$`zh-TW`$capm_rf_fallback_note)
)

cat("\n== Audit closeout summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
cat("ALL CLOSEOUT CHECKS PASSED\n")
quit(status = 0)
