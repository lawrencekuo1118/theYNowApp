# test_tpex_financial_summary.R — 櫃買財務資料簡報 fallback（fixture + optional live smoke）
# Run: Rscript app_15.0/tests/test_tpex_financial_summary.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")  # avoid shiny/reticulate init side effects for R helpers
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  normalizePath("tests/test_tpex_financial_summary.R")
}
root <- dirname(dirname(script_path))
setwd(root)

fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    cat("PASS:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
  }
}

# --- Pure R helpers (minimal stubs) -----------------------------------------
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

coerce_financial_df <- function(df) {
  if (is.null(df)) return(NULL)
  if (is.data.frame(df)) return(df)
  if (is.list(df) && !is.null(df$columns) && !is.null(df$data)) {
    cols <- as.character(unlist(df$columns, use.names = FALSE))
    rows <- df$data
    if (is.null(rows) || length(rows) == 0) {
      out <- as.data.frame(matrix(nrow = 0, ncol = length(cols)), stringsAsFactors = FALSE)
      names(out) <- cols
      return(out)
    }
    mat <- do.call(rbind, lapply(rows, function(r) {
      r <- as.character(unlist(r, use.names = FALSE))
      length(r) <- length(cols)
      r
    }))
    out <- as.data.frame(mat, stringsAsFactors = FALSE)
    names(out) <- cols
    return(out)
  }
  NULL
}

financial_df_is_empty <- function(df) {
  df <- coerce_financial_df(df)
  is.null(df) || !is.data.frame(df) || nrow(df) < 1L || ncol(df) < 2L
}

financials_is_bs_cf_all_empty <- function(res) {
  if (is.null(res)) return(TRUE)
  .stmt_empty <- function(key) {
    stmt <- tryCatch(res[[key]], error = function(e) NULL)
    if (is.null(stmt)) return(TRUE)
    exp <- stmt$expanded
    if (is.null(exp)) exp <- stmt$collapsed
    financial_df_is_empty(coerce_financial_df(exp))
  }
  .stmt_empty("Income Statement") &&
    .stmt_empty("Balance Sheet") &&
    .stmt_empty("Cash Flow")
}

source("tpex_financial.R", local = TRUE, encoding = "UTF-8")

check(
  "should_try only for .TWO",
  isTRUE(should_try_tpex_financial_fallback("7772.TWO", NULL)) &&
    isFALSE(should_try_tpex_financial_fallback("2330.TW", NULL)) &&
    isFALSE(should_try_tpex_financial_fallback("AMZN", NULL))
)

empty_yahoo <- list(
  "Income Statement" = list(expanded = data.frame(Breakdown = character(0))),
  "Balance Sheet" = list(expanded = data.frame(Breakdown = character(0))),
  "Cash Flow" = list(expanded = data.frame(Breakdown = character(0)))
)
check(
  "should_try when yahoo empty .TWO",
  isTRUE(should_try_tpex_financial_fallback("6488.TWO", empty_yahoo))
)

# Prefer system Python when available (avoid reticulate uv downloading a second CPython).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "")
if (nzchar(Sys.which("python3"))) {
  tryCatch(reticulate::use_python(Sys.which("python3"), required = FALSE), error = function(e) NULL)
}

py_ok <- FALSE
tryCatch({
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("no reticulate")
  reticulate::py_run_string("import xlrd, requests")
  reticulate::source_python("tpex_financial_summary.py")
  py_ok <- TRUE
}, error = function(e) {
  cat("NOTE: reticulate/python skip:", conditionMessage(e), "\n")
  cat("      Run: python3 -c 'from tpex_financial_summary import *' from app_15.0/\n")
})

if (isTRUE(py_ok)) {
  fixture <- jsonlite::fromJSON("tests/data/tpex_financial_summary_fixture.json", simplifyVector = FALSE)
  # Flatten all file rows
  all_rows <- list()
  for (nm in names(fixture$files)) {
    for (row in fixture$files[[nm]]) all_rows[[length(all_rows) + 1L]] <- row
  }

  otc <- statements_from_fixture_rows(all_rows, "6488")
  check("fixture OTC meta ok", isTRUE(otc[["_meta"]]$ok))
  is_df <- coerce_financial_df(otc[["Income Statement"]]$expanded)
  bs_df <- coerce_financial_df(otc[["Balance Sheet"]]$expanded)
  cf_df <- coerce_financial_df(otc[["Cash Flow"]]$expanded)
  check("fixture OTC IS has revenue", any(grepl("Total Revenue", is_df[[1]])))
  check("fixture OTC IS has NI", any(grepl("^Net Income$", is_df[[1]])))
  check("fixture OTC BS has equity", any(grepl("Stockholders Equity", bs_df[[1]])))
  check("fixture OTC BS has BVPS", any(grepl("Book Value Per Share", bs_df[[1]])))
  check("fixture OTC CF empty (honest)", financial_df_is_empty(cf_df))
  check(
    "fixture OTC not all-empty",
    isFALSE(financials_is_bs_cf_all_empty(list(
      "Income Statement" = list(expanded = is_df),
      "Balance Sheet" = list(expanded = bs_df),
      "Cash Flow" = list(expanded = cf_df)
    )))
  )

  esb <- statements_from_fixture_rows(all_rows, "7772")
  check("fixture ESB 7772 meta ok", isTRUE(esb[["_meta"]]$ok))
  esb_is <- coerce_financial_df(esb[["Income Statement"]]$expanded)
  check("fixture ESB has revenue", any(grepl("Total Revenue", esb_is[[1]])))
  # 223895 千元 → 223895000
  rev_row <- esb_is[grepl("Total Revenue", esb_is[[1]]), , drop = FALSE]
  rev_val <- suppressWarnings(as.numeric(rev_row[[2]][1]))
  check("fixture ESB revenue scaled from 千元", isTRUE(abs(rev_val - 223895000) < 1))

  # Unmapped documented
  check(
    "unmapped lists CapEx",
    any(grepl("CapEx", unlist(otc[["_meta"]]$unmapped), fixed = TRUE))
  )
} else {
  cat("SKIP python fixture checks\n")
}

# --- Optional live smoke (network) ------------------------------------------
live <- identical(Sys.getenv("YNOW_TPEX_LIVE_SMOKE"), "1")
if (isTRUE(live) && isTRUE(py_ok)) {
  cat("LIVE smoke: 6488 OTC + 7772 ESB...\n")
  live_otc <- tryCatch(scrape_tpex_financials_fallback("6488.TWO", "OTC"), error = function(e) e)
  if (!inherits(live_otc, "error") && isTRUE(live_otc[["_meta"]]$ok)) {
    check("live OTC 6488 ok", TRUE)
  } else {
    check("live OTC 6488 ok", FALSE)
    cat("  detail:", if (inherits(live_otc, "error")) conditionMessage(live_otc) else live_otc[["_meta"]]$error, "\n")
  }
  live_esb <- tryCatch(scrape_tpex_financials_fallback("7772.TWO", "ESB"), error = function(e) e)
  if (!inherits(live_esb, "error") && isTRUE(live_esb[["_meta"]]$ok)) {
    check("live ESB 7772 ok", TRUE)
  } else {
    check("live ESB 7772 ok", FALSE)
    cat("  detail:", if (inherits(live_esb, "error")) conditionMessage(live_esb) else live_esb[["_meta"]]$error, "\n")
  }
} else {
  cat("SKIP live smoke (set YNOW_TPEX_LIVE_SMOKE=1 to enable)\n")
}

if (fail > 0L) {
  cat("\n", fail, " check(s) failed\n", sep = "")
  quit(status = 1)
}
cat("\nAll checks passed.\n")
