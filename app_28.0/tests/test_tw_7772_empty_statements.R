#!/usr/bin/env Rscript
# Offline: TW OTC 7772 empty Yahoo shell must not crash Search integrity path.
# Bug: ynow_source_frame used `%||%` on 0×1 statement dfs →
#   "missing value where TRUE/FALSE needed" → notif_fetch_fail.
# Board: 7772.TWO = 上櫃 (TPEX). Controls: 2330 listed, 6488 OTC.
# Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_tw_7772_empty_statements.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
  }
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

source("setup.R", local = TRUE, encoding = "UTF-8")
source("market_profile.R", local = TRUE, encoding = "UTF-8")
source("lab_tw_universe.R", local = TRUE, encoding = "UTF-8")

# Yahoo-shaped empty shell (live 7772.TWO when annuals missing).
.empty_stmt <- function() {
  shell <- data.frame(Breakdown = character(0), stringsAsFactors = FALSE)
  list(collapsed = shell, expanded = shell)
}
.empty_yahoo <- function() {
  list(
    "Income Statement" = .empty_stmt(),
    "Balance Sheet" = .empty_stmt(),
    "Cash Flow" = .empty_stmt()
  )
}

# --- Reproduce the failing `%||%` shape (pre-fix oracle) ---
na_crash <- tryCatch(
  {
    d <- data.frame(Breakdown = character(0), stringsAsFactors = FALSE)
    invisible(d %||% "fallback")
    FALSE
  },
  error = function(e) {
    grepl("missing value where TRUE/FALSE needed", conditionMessage(e), fixed = TRUE)
  }
)
check("empty 0x1 df still trips setup %||% (oracle)", isTRUE(na_crash))

# --- Universe / board controls ---
uni_path <- file.path(app_dir, "data", "tw_universe.csv")
check("tw_universe.csv present", file.exists(uni_path))
if (file.exists(uni_path)) {
  u <- utils::read.csv(uni_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  lab_get_tw_universe <- function(...) u

  check(
    "7772 bare → 7772.TWO (上櫃)",
    identical(normalize_ticker_for_market("7772", "TW"), "7772.TWO")
  )
  check(
    "7772.TWO exact exchange TPEX",
    identical(lookup_tw_universe_exchange_exact("7772.TWO"), "TPEX")
  )
  check(
    "7772 not ESB",
    isFALSE(isTRUE(is_tw_esb_ticker("7772.TWO")))
  )
  check(
    "control 2330 → .TW listed",
    identical(normalize_ticker_for_market("2330", "TW"), "2330.TW") &&
      identical(lookup_tw_universe_exchange_exact("2330.TW"), "TWSE")
  )
  check(
    "control 6488 → .TWO OTC",
    identical(normalize_ticker_for_market("6488", "TW"), "6488.TWO") &&
      identical(lookup_tw_universe_exchange_exact("6488.TWO"), "TPEX")
  )
}

# --- Integrity path: empty Yahoo shell must not throw ---
res <- .empty_yahoo()
src <- ynow_statement_source(res, "current_cache", "7772.TWO", "TWD")
frame_ok <- tryCatch({
  d <- ynow_source_frame(src, "Income Statement")
  is.data.frame(d) && identical(names(d), "Breakdown") && nrow(d) == 0L
}, error = function(e) {
  cat("  ynow_source_frame err:", conditionMessage(e), "\n")
  FALSE
})
check("ynow_source_frame returns empty 0x1 IS", isTRUE(frame_ok))

periods_res <- tryCatch(
  ynow_fiscal_periods(list(src)),
  error = function(e) e
)
check(
  "ynow_fiscal_periods empty shell → character(0)",
  is.character(periods_res) && length(periods_res) == 0L
)

recovery <- tryCatch(
  ynow_recover_model_inputs(
    list(src),
    periods_res,
    list(),
    context = list(
      ticker = "7772.TWO",
      financial_currency = "TWD",
      quote_currency = "TWD",
      allow_book_claim_proxy = TRUE
    )
  ),
  error = function(e) e
)
check(
  "ynow_recover_model_inputs empty shell does not throw",
  !inherits(recovery, "error") && is.list(recovery)
)
check(
  "empty shell flagged all-empty",
  isTRUE(financials_is_bs_cf_all_empty(res))
)

# Non-empty control still resolves periods (synthetic, not live Yahoo).
ok_is <- data.frame(
  Breakdown = "Total Revenue",
  `12/31/2025` = 100,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
ok_src <- ynow_statement_source(
  list(
    "Income Statement" = list(expanded = ok_is, collapsed = ok_is),
    "Balance Sheet" = .empty_stmt(),
    "Cash Flow" = .empty_stmt()
  ),
  "cache", "2330.TW", "TWD"
)
ok_periods <- tryCatch(ynow_fiscal_periods(list(ok_src)), error = function(e) e)
check(
  "non-empty IS still yields fiscal period",
  is.character(ok_periods) && "2025-12-31" %in% ok_periods
)

if (fail > 0L) {
  cat("FAILED:", fail, "checks\n")
  quit(status = 1L)
}
cat("PASS: TW 7772 empty-statement integrity + board controls\n")
