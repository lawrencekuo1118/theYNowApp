#!/usr/bin/env Rscript
# Phase 2 — Data Integrity Audit (Completeness + Accuracy)
# Offline / mock-driven; no live Yahoo / TPEx network required.
# Run: cd app_26.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_data_integrity.R

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

suppressPackageStartupMessages({
  if (requireNamespace("jsonlite", quietly = TRUE)) library(jsonlite)
})

source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_tw_universe.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

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
approx_eq <- function(a, b, tol = 1e-9) {
  a <- suppressWarnings(as.numeric(a)[1])
  b <- suppressWarnings(as.numeric(b)[1])
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}
is_blank <- function(x) {
  x <- as.character(x)
  is.na(x) | !nzchar(trimws(x)) | toupper(trimws(x)) %in% c("NA", "NULL", "N/A")
}

# =============================================================================
# A. Completeness — offline snapshot / universe CSVs (data-flow entry points)
# =============================================================================
cat("\n== A. Completeness: offline CSV schemas & key fields ==\n")

.csv_path <- function(...) {
  p <- file.path(app_dir, "data", ...)
  if (file.exists(p)) normalizePath(p) else NA_character_
}

.read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
}

.assert_cols <- function(df, need, label) {
  check(paste0(label, ": required columns"), all(need %in% names(df)))
}

.assert_no_blank <- function(df, col, label) {
  if (!col %in% names(df) || !nrow(df)) {
    check(paste0(label, ": ", col, " present & non-blank"), FALSE)
    return(invisible(NULL))
  }
  check(
    paste0(label, ": ", col, " non-blank (", nrow(df), " rows)"),
    !any(is_blank(df[[col]]))
  )
}

# --- TW universe ---
tw_path <- .csv_path("tw_universe.csv")
check("tw_universe.csv exists", isTRUE(file.exists(tw_path)))
if (file.exists(tw_path)) {
  tw <- .read_csv(tw_path)
  .assert_cols(tw, c("ticker", "name", "exchange", "industry_raw", "industry_key", "fetched_at", "source"), "tw_universe")
  check("tw_universe row count > 100", nrow(tw) > 100L)
  .assert_no_blank(tw, "ticker", "tw_universe")
  .assert_no_blank(tw, "name", "tw_universe")
  check(
    "tw_universe tickers look Yahoo-suffixed",
    mean(grepl("\\.(TW|TWO)$", tw$ticker, ignore.case = TRUE)) > 0.95
  )
}

# --- US universe ---
us_path <- .csv_path("us_universe.csv")
check("us_universe.csv exists", isTRUE(file.exists(us_path)))
if (file.exists(us_path)) {
  us <- .read_csv(us_path)
  .assert_cols(us, c("ticker", "name", "exchange", "industry_raw", "industry_key", "fetched_at", "source"), "us_universe")
  check("us_universe row count > 100", nrow(us) > 100L)
  .assert_no_blank(us, "ticker", "us_universe")
  .assert_no_blank(us, "name", "us_universe")
}

# --- S&P 500 universe ---
sp_path <- .csv_path("sp500_universe.csv")
check("sp500_universe.csv exists", isTRUE(file.exists(sp_path)))
if (file.exists(sp_path)) {
  sp <- .read_csv(sp_path)
  .assert_cols(sp, c("ticker", "name", "exchange", "gics_sector", "gics_sub_industry", "fetched_at", "source"), "sp500")
  check("sp500 row count roughly 500", nrow(sp) >= 450L && nrow(sp) <= 520L)
  .assert_no_blank(sp, "ticker", "sp500")
}

# --- Universe metrics snapshot ---
um_path <- .csv_path("universe_metrics_snapshot.csv")
check("universe_metrics_snapshot.csv exists", isTRUE(file.exists(um_path)))
if (file.exists(um_path)) {
  um <- .read_csv(um_path)
  .assert_cols(um, c("ticker", "market_cap", "ret_1y", "paid_in_capital", "snapshot_at", "source"), "universe_metrics")
  check("universe_metrics row count > 100", nrow(um) > 100L)
  .assert_no_blank(um, "ticker", "universe_metrics")
  # Completeness: market_cap should be finite for a large majority (snapshot may leave gaps)
  mc <- suppressWarnings(as.numeric(um$market_cap))
  check(
    "universe_metrics market_cap finite rate > 80%",
    mean(is.finite(mc)) > 0.80
  )
  # Type: market_cap / ret_1y must coerce to numeric (no residual non-numeric strings)
  check(
    "universe_metrics market_cap numeric-coercible",
    isTRUE(is.numeric(mc) || all(is.na(mc) | grepl("^-?[0-9.eE+]+$", trimws(as.character(um$market_cap)))))
  )
}

# --- Cluster features snapshot ---
cf_path <- .csv_path("cluster_features_snapshot.csv")
check("cluster_features_snapshot.csv exists", isTRUE(file.exists(cf_path)))
if (file.exists(cf_path)) {
  cf <- .read_csv(cf_path)
  need_cf <- c(
    "ticker", "name", "market_cap", "ROE", "Operating_Margin", "Rev_YoY",
    "OpInc_YoY", "Debt_Ratio", "PE_Ratio", "PB_Ratio", "snapshot_at"
  )
  .assert_cols(cf, need_cf, "cluster_features")
  .assert_no_blank(cf, "ticker", "cluster_features")
  check("cluster_features rows > 50", nrow(cf) > 50L)
}

# --- Macro Buffett series (time continuity) ---
for (mode_file in c("macro_buffett_us.csv", "macro_buffett_tw.csv")) {
  mp <- .csv_path(mode_file)
  check(paste0(mode_file, " exists"), isTRUE(file.exists(mp)))
  if (!file.exists(mp)) next
  mb <- .read_csv(mp)
  .assert_cols(mb, c("date", "ratio_pct", "source"), mode_file)
  dts <- as.Date(mb$date)
  ratio <- suppressWarnings(as.numeric(mb$ratio_pct))
  check(paste0(mode_file, ": date parseable"), all(!is.na(dts)))
  check(paste0(mode_file, ": ratio_pct finite"), all(is.finite(ratio)))
  check(paste0(mode_file, ": sorted ascending"), isTRUE(all(diff(dts) >= 0)))
  # Annual-ish series: gaps should be mostly ~365d (allow leap / missing year ≤ 400d median)
  if (length(dts) >= 3L) {
    gaps <- as.numeric(diff(sort(unique(dts))))
    check(
      paste0(mode_file, ": median gap ≤ 400 days (no large missing intervals)"),
      isTRUE(stats::median(gaps) <= 400)
    )
    check(
      paste0(mode_file, ": no gap > 800 days"),
      isTRUE(max(gaps) <= 800)
    )
  }
}

mb_abs <- .csv_path("macro_buffett_tw_abs.csv")
check("macro_buffett_tw_abs.csv exists", isTRUE(file.exists(mb_abs)))
if (file.exists(mb_abs)) {
  mba <- .read_csv(mb_abs)
  .assert_cols(mba, c("date", "market_cap_usd", "gdp_usd", "source"), "macro_buffett_tw_abs")
  check(
    "macro_buffett_tw_abs: levels finite",
    all(is.finite(suppressWarnings(as.numeric(mba$market_cap_usd)))) &&
      all(is.finite(suppressWarnings(as.numeric(mba$gdp_usd))))
  )
}

# =============================================================================
# B. Accuracy — mock statement payload → coerce / normalize (100% expected)
# =============================================================================
cat("\n== B. Accuracy: mock financial payload → normalize_all_financials ==\n")

# Known-correct mock: Python-shaped {columns, data}; values with commas / B suffix
mock_is_payload <- list(
  columns = c("Breakdown", "12/31/2022", "12/31/2024", "12/31/2023", "TTM"),
  data = list(
    c("Total Revenue", "80,000,000", "100000000", "90B", "105M"),
    c("Net Income", "8,000,000", "12,000,000", "10,000,000", "11,000,000"),
    c("Basic EPS", "1.10", "1.50", "1.30", "1.40")
  )
)
mock_bs_payload <- list(
  columns = c("Breakdown", "12/31/2024", "12/31/2023"),
  data = list(
    c("Total Assets", "500000000", "450000000"),
    c("Stockholders Equity", "200000000", "180000000"),
    c("Total Debt", "50,000,000", "60,000,000")
  )
)
mock_cf_payload <- list(
  columns = c("Breakdown", "12/31/2024", "12/31/2023"),
  data = list(
    c("Operating Cash Flow", "40,000,000", "35,000,000"),
    c("Capital Expenditure", "-15,000,000", "-12,000,000"),
    c("Free Cash Flow", "25,000,000", "23,000,000")
  )
)
mock_raw <- list(
  "Income Statement" = list(collapsed = mock_is_payload, expanded = mock_is_payload),
  "Balance Sheet" = list(collapsed = mock_bs_payload, expanded = mock_bs_payload),
  "Cash Flow" = list(collapsed = mock_cf_payload, expanded = mock_cf_payload),
  "_meta" = list(currency = "USD", financialCurrency = "USD")
)

norm <- normalize_all_financials(mock_raw)
check("normalize returns IS/BS/CF keys", all(c("Income Statement", "Balance Sheet", "Cash Flow") %in% names(norm)))
check("normalize stamps quote currency USD", identical(attr(norm, "currency"), "USD"))
check("normalize stamps financialCurrency USD", identical(attr(norm, "financialCurrency"), "USD"))
check("normalize not all-empty", isFALSE(financials_is_bs_cf_all_empty(norm)))

is_df <- norm[["Income Statement"]]$expanded
check("IS is data.frame after coerce", is.data.frame(is_df) && nrow(is_df) == 3L)
check(
  "IS columns reordered: Breakdown | TTM | newest FY → oldest",
  identical(colnames(is_df), c("Breakdown", "TTM", "12/31/2024", "12/31/2023", "12/31/2022"))
)

# Metric extraction must match hand-computed expected values 100%
rev <- select_clean_metric_row(is_df, "Total Revenue", include_ttm = TRUE)
check("Total Revenue vector length = 4 periods", length(rev) == 4L)
check("Total Revenue TTM == 105e6 (105M)", approx_eq(rev[1], 105e6))
check("Total Revenue FY2024 == 1e8", approx_eq(rev[2], 1e8))
check("Total Revenue FY2023 == 90e9 (90B)", approx_eq(rev[3], 90e9))
check("Total Revenue FY2022 == 8e7", approx_eq(rev[4], 8e7))
check("Total Revenue no NA", all(is.finite(rev)))

ni <- select_clean_metric_row(is_df, "Net Income", include_ttm = TRUE)
check("Net Income FY2024 == 12e6", approx_eq(ni[2], 12e6))
check("Net Income no NA", all(is.finite(ni)))

bs_df <- norm[["Balance Sheet"]]$expanded
eq <- select_clean_metric_row(bs_df, "Stockholders Equity", include_ttm = FALSE)
check("Equity FY2024 == 2e8", approx_eq(eq[1], 2e8))
check("Equity no NA", all(is.finite(eq)))

cf_df <- norm[["Cash Flow"]]$expanded
fcf <- select_clean_metric_row(cf_df, "Free Cash Flow", include_ttm = FALSE)
check("FCF FY2024 == 25e6", approx_eq(fcf[1], 25e6))
check("FCF no NA", all(is.finite(fcf)))

# Empty payload completeness helper
empty_payload <- list(
  "Income Statement" = list(
    collapsed = list(columns = "Breakdown", data = list()),
    expanded = list(columns = "Breakdown", data = list())
  ),
  "Balance Sheet" = list(
    collapsed = list(columns = "Breakdown", data = list()),
    expanded = list(columns = "Breakdown", data = list())
  ),
  "Cash Flow" = list(
    collapsed = list(columns = "Breakdown", data = list()),
    expanded = list(columns = "Breakdown", data = list())
  )
)
check("empty Python payload → all-empty TRUE", isTRUE(financials_is_bs_cf_all_empty(normalize_all_financials(empty_payload))))

# =============================================================================
# C. Accuracy — parse_financial_number type / suffix matrix
# =============================================================================
cat("\n== C. Accuracy: parse_financial_number type coercion ==\n")

check("parse 1.5B → 1.5e9", approx_eq(parse_financial_number("1.5B"), 1.5e9))
check("parse 2.0M → 2e6", approx_eq(parse_financial_number("2.0M"), 2e6))
check("parse 3K → 3000", approx_eq(parse_financial_number("3K"), 3000))
check("parse $1,234.50 → 1234.5", approx_eq(parse_financial_number("$1,234.50"), 1234.5))
check("parse 12.5% strips percent mark", approx_eq(parse_financial_number("12.5%"), 12.5))
check("parse '-' → NA", isTRUE(is.na(parse_financial_number("-")[1])))
check("parse 'N/A' → NA", isTRUE(is.na(parse_financial_number("N/A")[1])))
check("parse '--' → NA", isTRUE(is.na(parse_financial_number("--")[1])))
check(
  "parse refuses alpha garbage → NA (no silent string math)",
  isTRUE(is.na(parse_financial_number("abc")[1]))
)
# Vectorized: mix of good + blank must not crash and preserve positions
pv <- parse_financial_number(c("10", "-", "2M", "N/A"))
check("parse vector length preserved", length(pv) == 4L)
check("parse vector positions", approx_eq(pv[1], 10) && is.na(pv[2]) && approx_eq(pv[3], 2e6) && is.na(pv[4]))

# =============================================================================
# D. Accuracy — mock Summary quote → extract_quote_price_mcap
# =============================================================================
cat("\n== D. Accuracy: mock Summary quote → price / market_cap ==\n")

summary_mock <- data.frame(
  Item = c(
    "Market Price", "Previous Close", "Market Cap (intraday)",
    "Shares Outstanding", "EPS (TTM)", "Beta (5Y Monthly)"
  ),
  Value = c("172.50", "170.00", "2.70T", "5.20B", "6.45", "1.12"),
  stringsAsFactors = FALSE
)
qm <- extract_quote_price_mcap(summary_mock)
check("summary price == 172.50", approx_eq(qm$price, 172.50))
check("summary market_cap == 2.70e12", approx_eq(qm$market_cap, 2.70e12))
check("summary EPS via extract_summary_number", approx_eq(extract_summary_number(summary_mock, "^EPS"), 6.45))
check("summary shares outstanding", approx_eq(extract_summary_number(summary_mock, "Shares Outstanding"), 5.20e9))

# Completeness: critical quote fields must not be NA after parse
check("summary Market Price not NA", is.finite(qm$price))
check("summary Market Cap not NA", is.finite(qm$market_cap))

# Bad schema: missing Item/Value → NA (no crash)
bad_sum <- data.frame(x = 1, y = 2)
qm_bad <- extract_quote_price_mcap(bad_sum)
check("bad summary schema → NA price/mcap (no crash)", is.na(qm_bad$price) && is.na(qm_bad$market_cap))

# =============================================================================
# E. Accuracy — mock TW universe rows → lab_finalize_tw_list (100% expected)
# =============================================================================
cat("\n== E. Accuracy: mock TW listing rows → lab_finalize_tw_list ==\n")

mock_tw_rows <- data.frame(
  公司代號 = c("2330", "6488", "0050"),
  公司名稱 = c("台積電", "環球晶圓", "元大台灣50"),
  產業別 = c("24", "24", "00"),
  stringsAsFactors = FALSE
)
tw_fin <- lab_finalize_tw_list(
  mock_tw_rows,
  exchange = "TWSE",
  source = "audit_mock",
  fetched_at = "2026-10-12T00:00:00Z"
)
# 0050 is ETF-like and should be excluded by lab_is_excluded_tw_ticker when applicable
check("finalize returns data.frame", is.data.frame(tw_fin))
check("finalize keeps equity 2330.TW", "2330.TW" %in% tw_fin$ticker)
check("finalize ticker non-blank", !any(is_blank(tw_fin$ticker)))
check("finalize name non-blank", !any(is_blank(tw_fin$name)))
check(
  "finalize 2330 name is 台積電",
  identical(as.character(tw_fin$name[tw_fin$ticker == "2330.TW"][1]), "台積電")
)
check(
  "finalize industry_key mapped for 半導體 code 24",
  {
    k <- as.character(tw_fin$industry_key[tw_fin$ticker == "2330.TW"][1])
    nzchar(k) && !identical(k, "NA")
  }
)
check(
  "finalize schema matches lab_empty_tw",
  identical(sort(names(tw_fin)), sort(names(lab_empty_tw())))
)

# =============================================================================
# F. Accuracy — mock metrics snapshot loader (deterministic)
# =============================================================================
cat("\n== F. Accuracy: mock universe_metrics_snapshot loader ==\n")

fixture_dir <- tempfile("ynow_audit_metrics_")
dir.create(fixture_dir, recursive = TRUE)
fixture_csv <- file.path(fixture_dir, "universe_metrics_snapshot.csv")
utils::write.csv(
  data.frame(
    ticker = c("AAA", "bbb", "1101.TW"),
    market_cap = c("1000000000", "5.0e9", "2000000000"),
    ret_1y = c("0.10", "-0.05", "0.20"),
    paid_in_capital = c(NA_real_, NA_real_, 7.7e10),
    snapshot_at = "2026-10-12T00:00:00Z",
    source = "audit_mock",
    stringsAsFactors = FALSE
  ),
  file = fixture_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
options(ynow.universe_metrics_snapshot = normalizePath(fixture_csv, winslash = "/"))
if (exists(".lab_universe_metrics_cache", inherits = TRUE)) {
  rm(list = ls(envir = .lab_universe_metrics_cache), envir = .lab_universe_metrics_cache)
}
snap <- lab_load_universe_metrics_snapshot(force = TRUE)
check("mock snapshot 3 rows", nrow(snap) == 3L)
check("mock snapshot uppercases ticker", identical(sort(snap$ticker), c("1101.TW", "AAA", "BBB")))
check("mock snapshot AAA mcap 1e9", approx_eq(snap$market_cap[snap$ticker == "AAA"], 1e9))
check("mock snapshot BBB mcap 5e9 (scientific string)", approx_eq(snap$market_cap[snap$ticker == "BBB"], 5e9))
check("mock snapshot ret AAA 0.10", approx_eq(snap$ret_1y[snap$ticker == "AAA"], 0.10))
check("mock snapshot TW paid_in finite", is.finite(snap$paid_in_capital[snap$ticker == "1101.TW"]))
# Completeness of key numeric fields for mock equities with known mcap
check(
  "mock snapshot market_cap non-NA for all fixtures",
  all(is.finite(snap$market_cap))
)

caps <- lab_fetch_market_caps_usd(c("AAA", "MISSING"))
check("fetch mcap AAA from snapshot", approx_eq(caps[["AAA"]], 1e9))
check("fetch mcap MISSING stays NA", !is.finite(caps[["MISSING"]]))

# Restore default snapshot option
options(ynow.universe_metrics_snapshot = NULL)
unlink(fixture_dir, recursive = TRUE)

# =============================================================================
# G. Completeness — TPEx fixture JSON (tests/data) shape if present
# =============================================================================
cat("\n== G. Completeness: TPEx financial fixture (if present) ==\n")

tpex_fix <- file.path(app_dir, "tests", "data", "tpex_financial_summary_fixture.json")
check("tpex fixture json exists", isTRUE(file.exists(tpex_fix)))
if (file.exists(tpex_fix) && requireNamespace("jsonlite", quietly = TRUE)) {
  fx <- tryCatch(jsonlite::fromJSON(tpex_fix, simplifyVector = FALSE), error = function(e) NULL)
  check("tpex fixture parses", !is.null(fx))
  if (!is.null(fx)) {
    # Shipped shape: { files: { "<board_period>": [ { sheet, year, quarter, board, row: [...] }, ... ] } }
    files <- fx$files
    check("tpex fixture has files object", is.list(files) && length(files) >= 1L)
    first_key <- names(files)[1]
    entries <- if (!is.null(first_key)) files[[first_key]] else NULL
    check("tpex fixture period has ≥1 entry", is.list(entries) && length(entries) >= 1L)
    if (is.list(entries) && length(entries) >= 1L) {
      e0 <- entries[[1]]
      check(
        "tpex fixture entry has sheet/year/quarter/board/row",
        all(c("sheet", "year", "quarter", "board", "row") %in% names(e0))
      )
      row0 <- e0$row
      check("tpex fixture row length ≥ 3 (code, name, revenue…)", is.list(row0) && length(row0) >= 3L)
      if (is.list(row0) && length(row0) >= 3L) {
        code <- as.character(row0[[1]] %||% "")[1]
        rev <- suppressWarnings(as.numeric(row0[[3]])[1])
        check("tpex fixture code non-blank", !is_blank(code))
        check("tpex fixture revenue (col3) finite / non-NA", is.finite(rev))
      }
    }
  }
}

# =============================================================================
# Summary
# =============================================================================
cat("\n== Data Integrity Audit summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) {
  quit(status = 1L)
}
cat("ALL CONTROLS PASSED\n")
