#!/usr/bin/env Rscript
# TSM ADR + FX: share counts must not be FX-scaled; wrong shares → ~5× FV error.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}
approx_eq <- function(a, b, tol = 1e-6) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

check("skip Ordinary Shares FX", isFALSE(fs_row_is_fx_money("Ordinary Shares Number")))
check("skip Share Issued FX", isFALSE(fs_row_is_fx_money("Share Issued")))
check("skip Tax Rate FX", isFALSE(fs_row_is_fx_money("Tax Rate For Calcs")))
check("skip EPS FX", isFALSE(fs_row_is_fx_money("Basic EPS")))
check("money Total Revenue FX", isTRUE(fs_row_is_fx_money("Total Revenue")))
check("money Cash FX", isTRUE(fs_row_is_fx_money("Cash And Cash Equivalents")))

# TSM-like: TWD statements, Ordinary Shares must stay at common count after FX
df <- data.frame(
  Breakdown = c("Total Revenue", "Ordinary Shares Number", "Cash And Cash Equivalents"),
  `2025` = c("3809054300000", "25932524521", "2767856400000"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
fx <- 32
out <- scale_financial_df_money(df, "TWD", "USD", usd_twd = fx)
check("money_scaled attr", isTRUE(attr(out, "money_scaled")))
rev <- parse_financial_number(out[["2025"]][1])[1]
sh <- parse_financial_number(out[["2025"]][2])[1]
cash <- parse_financial_number(out[["2025"]][3])[1]
check("revenue FX /32", approx_eq(rev, 3809054300000 / fx, tol = 1))
check("shares NOT FX-scaled", approx_eq(sh, 25932524521, tol = 0.5))
check("cash FX /32", approx_eq(cash, 2767856400000 / fx, tol = 1))

# ADR resolve: common vs mcap/price ≈ 1/5
price <- 420
mcap <- 2.18e12
adr <- mcap / price
sh_res <- resolve_shares_for_price(
  sh, price = price, market_cap = mcap, ticker = "TSM",
  quote_currency = "USD", financial_currency = "TWD"
)
check("method market_cap_per_price", identical(sh_res$method, "market_cap_per_price"))
check("ADR shares ~ mcap/price", approx_eq(sh_res$shares, adr, tol = 1))
check("ratio ~ 0.2", is.finite(sh_res$ratio) && abs(sh_res$ratio - adr / sh) < 1e-9)

# Per-share sanity: Equity_USD / ADR ≫ Equity_USD / common by ~5×
equity_usd <- 6.9e11
fv_adr <- equity_usd / sh_res$shares
fv_common <- equity_usd / sh
check("ADR FV ~5× common FV", abs(fv_adr / fv_common - sh / sh_res$shares) < 1e-9)
check("common undervalues vs ADR by ~5×", abs(sh / sh_res$shares - 5) < 0.15)

# Regression: old bug scaled shares by FX → ratio ~6.4 and wrong path risk
sh_bug <- sh / fx
ratio_bug <- adr / sh_bug
check("FX-scaled shares ratio ~ FX/5", abs(ratio_bug - fx / 5) < 0.05)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("All TSM ADR/FX DCF share checks passed.\n")
