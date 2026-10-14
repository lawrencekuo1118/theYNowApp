#!/usr/bin/env Rscript
# Acceptance — live Rf paths (US ^TNX + TW TPEx when Python venv available).
# Prefers app_23.0/.ynow_venv via RETICULATE_PYTHON when present.
#
# Run: cd app_23.0 && YNOW_DEBUG_SKIP_PY=0 Rscript tests/test_audit_live_rf_paths.R
# Env:  YNOW_ACCEPTANCE_REQUIRE_TW_LIVE=1 to fail if TW is not live

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

venv_py <- file.path(app_dir, ".ynow_venv", "bin", "python")
if (!nzchar(Sys.getenv("RETICULATE_PYTHON")) && file.exists(venv_py)) {
  Sys.setenv(RETICULATE_PYTHON = venv_py)
}
if (nzchar(Sys.getenv("RETICULATE_PYTHON")) && requireNamespace("reticulate", quietly = TRUE)) {
  tryCatch(
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE),
    error = function(e) NULL
  )
}

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

check(".rf_market_mode TW/US", identical(.rf_market_mode("TW"), "TW") &&
        identical(.rf_market_mode("US"), "US"))

tryCatch(memoise::forget(.cached_fetch_rf_live_us), error = function(e) NULL)
tryCatch(memoise::forget(.cached_fetch_rf_live_tw), error = function(e) NULL)
.rf_live_last[["US"]] <- NULL
.rf_live_last[["TW"]] <- NULL

# US (quantmod and/or Python)
rf_qm <- tryCatch(.fetch_rf_live_us_quantmod(), error = function(e) e)
check(
  "US quantmod ^TNX finite",
  !inherits(rf_qm, "error") && is.finite(rf_qm) && rf_qm > 0 && rf_qm < 40
)
cat("INFO quantmod ^TNX rf=", if (inherits(rf_qm, "error")) conditionMessage(rf_qm) else rf_qm, "\n")

det_us <- tryCatch(cached_get_risk_free_rate_detail("US"), error = function(e) e)
check("US detail ok", !inherits(det_us, "error") && is.list(det_us))
if (!inherits(det_us, "error")) {
  cat("INFO US source=", det_us$source, " fb=", det_us$is_fallback, " rf=", det_us$rf_pct, "\n", sep = "")
  check(
    "US live or last_known",
    isFALSE(isTRUE(det_us$is_fallback)) &&
      as.character(det_us$source %||% "")[1] %in% c("live", "last_known")
  )
}

# TW
det_tw <- tryCatch(cached_get_risk_free_rate_detail("TW"), error = function(e) e)
check("TW detail ok", !inherits(det_tw, "error") && is.list(det_tw))
tw_live <- FALSE
if (!inherits(det_tw, "error")) {
  cat(
    "INFO TW source=", det_tw$source, " fb=", det_tw$is_fallback,
    " rf=", det_tw$rf_pct, " label=", det_tw$label, "\n",
    sep = ""
  )
  check(
    "TW label/symbol is TPEx family",
    grepl("台灣|TPEx|TPEX", as.character(det_tw$label %||% ""), ignore.case = TRUE) ||
      grepl("TPEX", as.character(det_tw$symbol %||% ""), ignore.case = TRUE)
  )
  tw_live <- isFALSE(isTRUE(det_tw$is_fallback)) &&
    as.character(det_tw$source %||% "")[1] %in% c("live", "last_known")
  if (isTRUE(tw_live)) {
    check(
      "TW live Rf% in plausible band (0.5–5)",
      is.finite(as.numeric(det_tw$rf_pct)[1]) &&
        as.numeric(det_tw$rf_pct)[1] > 0.5 &&
        as.numeric(det_tw$rf_pct)[1] < 5
    )
    check(
      "TW live Rf differs from US live Rf (markets not collapsed)",
      !inherits(det_us, "error") &&
        is.finite(as.numeric(det_us$rf_pct)[1]) &&
        abs(as.numeric(det_tw$rf_pct)[1] - as.numeric(det_us$rf_pct)[1]) > 0.2
    )
  } else if (isTRUE(det_tw$is_fallback)) {
    check(
      "TW engineering fallback ~1.8 when live unavailable",
      is.finite(as.numeric(det_tw$rf_pct)[1]) && as.numeric(det_tw$rf_pct)[1] < 3
    )
  }
}

has_venv <- file.exists(venv_py)
cat("INFO has_ynow_venv=", has_venv, " tw_live=", tw_live, "\n", sep = "")
if (identical(Sys.getenv("YNOW_ACCEPTANCE_REQUIRE_TW_LIVE", "0"), "1") ||
    (isTRUE(has_venv) && identical(Sys.getenv("YNOW_ACCEPTANCE_REQUIRE_TW_LIVE", "auto"), "auto"))) {
  check("TW live required and observed", isTRUE(tw_live))
} else if (!isTRUE(tw_live)) {
  cat("INFO TW live not required this run (set YNOW_ACCEPTANCE_REQUIRE_TW_LIVE=1 to enforce)\n")
}

cat("\n== Live Rf paths summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
cat("ALL LIVE RF PATH CHECKS PASSED\n")
quit(status = 0)
