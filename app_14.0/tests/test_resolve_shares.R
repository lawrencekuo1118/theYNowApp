#!/usr/bin/env Rscript
# Unit checks for ADR / share-class resolve (no network).
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

# US common: ratio ~1 → balance_sheet
r <- resolve_shares_for_price(1e10, price = 100, market_cap = 1e12, ticker = "AAPL")
check("AAPL balance_sheet", identical(r$method, "balance_sheet") && abs(r$shares - 1e10) < 1)

# ADR 5:1 (TSM-like): implied/bs = 0.2 → market_cap_per_price
r <- resolve_shares_for_price(
  25e9, price = 400, market_cap = 2e12, ticker = "TSM",
  quote_currency = "USD", financial_currency = "TWD"
)
check("TSM ADR auto", identical(r$method, "market_cap_per_price") && abs(r$shares - 5e9) < 1)

# ADR 8:1 (BABA-like)
r <- resolve_shares_for_price(
  18e9, price = 100, market_cap = 2.25e11, ticker = "BABA",
  quote_currency = "USD", financial_currency = "CNY"
)
check("BABA ADR auto", identical(r$method, "market_cap_per_price") && abs(r$shares - 2.25e9) < 1)

# Near 1:1 dual-listed (ASML-like): no convert
r <- resolve_shares_for_price(
  4e8, price = 800, market_cap = 3.2e11, ticker = "ASML",
  quote_currency = "USD", financial_currency = "EUR"
)
check("ASML 1:1 keep BS", identical(r$method, "balance_sheet"))

# BRK-B
r <- resolve_shares_for_price(1.4e6, price = 500, market_cap = 1e12, ticker = "BRK-B")
check("BRK-B implied", identical(r$method, "market_cap_per_price"))

# extract_quote_price_mcap
sum_df <- data.frame(
  Item = c("Previous Close", "Market Cap (intraday)"),
  Value = c("123.45", "1.5B"),
  stringsAsFactors = FALSE
)
qm <- extract_quote_price_mcap(sum_df)
check("extract price", abs(qm$price - 123.45) < 1e-6)
check("extract mcap", abs(qm$market_cap - 1.5e9) < 1)

check("auto_adjust helper", isTRUE(shares_auto_adjust_method("market_cap_per_price")))
check("auto_adjust neg", !isTRUE(shares_auto_adjust_method("balance_sheet")))

if (fail > 0L) {
  cat("\n", fail, " failure(s)\n", sep = "")
  quit(status = 1)
}
cat("\nAll resolve_shares checks passed.\n")
