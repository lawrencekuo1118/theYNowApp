#!/usr/bin/env Rscript
# Behavior: TW OTC／興櫃 with empty Yahoo annuals + TPEx YTD fill must expose
# Income Statement / Crossover KPI numerics via allow_interim history frames,
# while default (valuation) history stays blank for non-annual IS.
# Cash Flow remains empty when TPEx has no CF (honest N/A).
# Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_tw_otc_statement_kpi.R
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
source("setup_fs.R", local = TRUE, encoding = "UTF-8")
source("model_integrity.R", local = TRUE, encoding = "UTF-8")
source("tpex_financial.R", local = TRUE, encoding = "UTF-8")

.empty_stmt <- function() {
  shell <- data.frame(Breakdown = character(0), stringsAsFactors = FALSE)
  list(collapsed = shell, expanded = shell)
}

# Synthetic TPEx-shaped YTD summary (same labels as tpex_financial_summary.py)
tpex_is <- data.frame(
  Breakdown = c(
    "Total Revenue", "Operating Income", "Pretax Income",
    "Net Income", "Net Income Common Stockholders", "Basic EPS", "Diluted EPS"
  ),
  `09/30/2026` = c(293243000, 9649000, 13943000, 13413000, 13413000, 0.53, 0.53),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
tpex_bs <- data.frame(
  Breakdown = c(
    "Common Stock", "Ordinary Shares Number", "Stockholders Equity",
    "Common Stock Equity", "Total Assets", "Book Value Per Share"
  ),
  `09/30/2026` = c(264986000, 26498600, 1024965848, 1024965848, 1366621130.67, 38.68),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

res <- list(
  "Income Statement" = list(expanded = tpex_is, collapsed = tpex_is),
  "Balance Sheet" = list(expanded = tpex_bs, collapsed = tpex_bs),
  "Cash Flow" = .empty_stmt()
)
attr(res, "tpex_filled") <- c("Income Statement", "Balance Sheet")
attr(res, "financialCurrency") <- "TWD"
attr(res, "currency") <- "TWD"

src <- ynow_statement_source(res, "current_cache", "7772.TWO", "TWD")
check("TPEx-filled IS marked non-annual", isFALSE(ynow_source_is_annual(src, "Income Statement")))
check("TPEx-filled BS marked non-annual", isFALSE(ynow_source_is_annual(src, "Balance Sheet")))

ctx <- list(ticker = "7772.TWO", financial_currency = "TWD", quote_currency = "TWD")
sources <- list(src)

# Valuation default: IS history blank / NA shells (must not treat YTD as annual)
val_is <- ynow_fiscal_history_frame(sources, "Income Statement", ctx, allow_interim = FALSE)
rev_val <- tryCatch(
  get_avg(select_clean_metric_row(val_is, "Total Revenue", include_ttm = FALSE)),
  error = function(e) NA_real_
)
ni_val <- tryCatch(
  get_avg(select_clean_metric_row_any(val_is, NET_INCOME_PATTERNS, include_ttm = FALSE)),
  error = function(e) NA_real_
)
check("valuation IS Total Revenue stays NA (no interim)", !is.finite(rev_val))
check("valuation IS Net Income stays NA (no interim)", !is.finite(ni_val))

# KPI display path: allow_interim surfaces YTD numbers
kpi_is <- ynow_fiscal_history_frame(sources, "Income Statement", ctx, allow_interim = TRUE)
kpi_bs <- ynow_fiscal_history_frame(sources, "Balance Sheet", ctx, allow_interim = TRUE)
kpi_cf <- ynow_fiscal_history_frame(sources, "Cash Flow", ctx, allow_interim = TRUE)

rev_kpi <- get_avg(select_clean_metric_row(kpi_is, "Total Revenue", include_ttm = FALSE))
ni_kpi <- get_avg(select_clean_metric_row_any(kpi_is, NET_INCOME_PATTERNS, include_ttm = FALSE))
asset_kpi <- get_avg(select_clean_metric_row(kpi_bs, "Total Assets", include_ttm = FALSE))
equity_kpi <- get_avg(select_clean_metric_row_any(kpi_bs, EQUITY_PATTERNS, include_ttm = FALSE))

check("KPI IS Total Revenue finite (TPEx YTD)", is.finite(rev_kpi) && abs(rev_kpi - 293243000) < 1)
check("KPI IS Net Income finite (TPEx YTD)", is.finite(ni_kpi) && abs(ni_kpi - 13413000) < 1)
check("KPI BS Total Assets finite", is.finite(asset_kpi) && asset_kpi > 0)
check("KPI BS Equity finite", is.finite(equity_kpi) && equity_kpi > 0)

# Crossover-style ratios (same formulas as kpi_module)
npm <- ni_kpi / rev_kpi * 100
roa <- ni_kpi / asset_kpi * 100
roe <- ni_kpi / equity_kpi * 100
ato <- rev_kpi / asset_kpi
check("Net Profit Margin computable", is.finite(npm) && npm > 0)
check("ROA computable", is.finite(roa) && roa > 0)
check("ROE computable", is.finite(roe) && roe > 0)
check("Asset Turnover computable", is.finite(ato) && ato > 0)

# CF still empty — Cash Flow KPIs and OCF/NI stay honest N/A
ocf <- tryCatch(
  get_avg(cashflow_total_row(kpi_cf, "ocf", include_ttm = FALSE)),
  error = function(e) NA_real_
)
check("KPI CF Operating Cash Flow still NA (TPEx never invents CF)", !is.finite(ocf))
check(
  "KPI CF history empty or all-NA rows",
  !is.data.frame(kpi_cf) || nrow(kpi_cf) == 0L ||
    all(!is.finite(suppressWarnings(as.numeric(unlist(kpi_cf[-1])))))
)

# Annual Yahoo control: allow_interim must not change annual IS
ann_is <- data.frame(
  Breakdown = c("Total Revenue", "Net Income Common Stockholders"),
  `12/31/2025` = c(1000, 100),
  `12/31/2024` = c(900, 90),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
ann_res <- list(
  "Income Statement" = list(expanded = ann_is, collapsed = ann_is),
  "Balance Sheet" = .empty_stmt(),
  "Cash Flow" = .empty_stmt()
)
ann_src <- ynow_statement_source(ann_res, "cache", "6488.TWO", "TWD")
check("Yahoo annual IS remains annual flag", isTRUE(ynow_source_is_annual(ann_src, "Income Statement")))
a0 <- ynow_fiscal_history_frame(list(ann_src), "Income Statement", ctx, allow_interim = FALSE)
a1 <- ynow_fiscal_history_frame(list(ann_src), "Income Statement", ctx, allow_interim = TRUE)
check(
  "annual Yahoo IS identical with/without allow_interim",
  identical(
    as.numeric(select_clean_metric_row(a0, "Total Revenue", include_ttm = FALSE)),
    as.numeric(select_clean_metric_row(a1, "Total Revenue", include_ttm = FALSE))
  )
)

# Server wiring: KPI module must consume *_kpi reactives
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "kpi_module_server wired to d_*_kpi reactives",
  grepl(
    "kpi_module_server\\([\\s\\S]*d_income_statement_kpi[\\s\\S]*d_balance_sheet_kpi[\\s\\S]*d_cash_flow_kpi",
    srv,
    perl = TRUE
  )
)
check(
  "d_income_statement_kpi uses allow_interim",
  grepl("d_income_statement_kpi\\s*<-\\s*reactive", srv) &&
    grepl("allow_interim\\s*=\\s*\\.kpi_allow_interim\\(\\)", srv)
)

# TPEx timeout default raised for cold discovery (behavior contract)
py <- paste(readLines("tpex_financial_summary.py", warn = FALSE), collapse = "\n")
check(
  "TPEx scrape default timeout >= 75s",
  grepl('YNOW_TPEX_TIMEOUT_SEC",\\s*"75"', py) ||
    grepl("YNOW_TPEX_TIMEOUT_SEC\", \"75\"", py, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED:", fail, "checks\n")
  quit(status = 1L)
}
cat("PASS: TW OTC／興櫃 statement KPI interim display + valuation gate\n")
