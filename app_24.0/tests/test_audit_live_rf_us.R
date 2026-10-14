#!/usr/bin/env Rscript
# Acceptance — US live Rf path (Yahoo ^TNX via quantmod when Python absent).
# Network required. Exit 0 only if source is live or last_known (not fallback).
#
# Run: cd app_24.0 && YNOW_DEBUG_SKIP_PY=0 Rscript tests/test_audit_live_rf_us.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("web_crawler.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "0")

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

source("web_crawler.R", local = TRUE)

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

check(
  "quantmod US Rf helper exists",
  exists(".fetch_rf_live_us_quantmod", mode = "function")
)

rf_qm <- tryCatch(.fetch_rf_live_us_quantmod(), error = function(e) e)
check(
  "quantmod ^TNX returns finite positive Rf%",
  !inherits(rf_qm, "error") && is.finite(rf_qm) && rf_qm > 0 && rf_qm < 40
)
cat("INFO quantmod ^TNX rf=", if (inherits(rf_qm, "error")) conditionMessage(rf_qm) else rf_qm, "\n")

# Clear memo so detail path re-fetches
tryCatch(memoise::forget(.cached_fetch_rf_live_us), error = function(e) NULL)
.rf_live_last[["US"]] <- NULL

det <- tryCatch(cached_get_risk_free_rate_detail("US"), error = function(e) e)
check("detail call succeeds", !inherits(det, "error") && is.list(det))
if (!inherits(det, "error")) {
  cat(
    "INFO detail source=", det$source %||% "?",
    " is_fallback=", det$is_fallback %||% NA,
    " rf=", det$rf_pct %||% NA, "\n",
    sep = ""
  )
  check(
    "US Rf is live or last_known (not engineering fallback)",
    isFALSE(isTRUE(det$is_fallback)) &&
      as.character(det$source %||% "")[1] %in% c("live", "last_known")
  )
  check(
    "US Rf% finite positive",
    is.finite(suppressWarnings(as.numeric(det$rf_pct)[1])) &&
      as.numeric(det$rf_pct)[1] > 0
  )
}

# Regression: explicit TW must not collapse to US when normalize_market_mode
# is unavailable (web_crawler sourced alone in probes).
check(
  ".rf_market_mode('TW') is TW without normalize helper",
  identical(.rf_market_mode("TW"), "TW")
)
check(
  ".rf_market_mode('US') is US",
  identical(.rf_market_mode("US"), "US")
)

# TW without Python should fail soft to documented TW fallback — not US ^TNX
tryCatch(memoise::forget(.cached_fetch_rf_live_tw), error = function(e) NULL)
.rf_live_last[["TW"]] <- NULL
det_tw <- tryCatch(cached_get_risk_free_rate_detail("TW"), error = function(e) e)
if (!inherits(det_tw, "error")) {
  cat(
    "INFO TW detail source=", det_tw$source %||% "?",
    " is_fallback=", det_tw$is_fallback %||% NA,
    " rf=", det_tw$rf_pct %||% NA,
    " label=", det_tw$label %||% "?", "\n",
    sep = ""
  )
  check(
    "TW detail label is TW curve (not US ^TNX) when mode honored",
    grepl("台灣|TPEx|TPEX", as.character(det_tw$label %||% ""), ignore.case = TRUE) ||
      grepl("TPEX", as.character(det_tw$symbol %||% ""), ignore.case = TRUE)
  )
  # If Python unavailable: expect engineering fallback ~1.8, not US live ~5.2
  if (isTRUE(det_tw$is_fallback)) {
    check(
      "TW engineering fallback is TW profile (~1.8), not US 5%",
      is.finite(as.numeric(det_tw$rf_pct)[1]) && as.numeric(det_tw$rf_pct)[1] < 3
    )
  }
}

cat("\n== Live US Rf summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
cat("ALL LIVE US RF CHECKS PASSED\n")
quit(status = 0)
