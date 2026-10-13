# Shared cash / debt / shares / revenue source for DCF, WACC, Multiples, SOTP.
# Run: cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_shared_bridge.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
app_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath("..", mustWork = TRUE)
}
setwd(app_dir)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
source("setup.R", local = FALSE, encoding = "UTF-8")

fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) cat("OK ", label, "\n", sep = "")
  else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

bs <- data.frame(
  Breakdown = c("Cash And Cash Equivalents", "Total Debt", "Ordinary Shares Number"),
  `2025` = c(20, 80, 10),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
cd <- statement_cash_debt(bs)
check("cash and debt from the same rows", identical(cd$cash, 20) && identical(cd$debt, 80))

bs_split <- data.frame(
  Breakdown = c("Cash And Cash Equivalents", "Current Debt", "Long Term Debt"),
  `2025` = c(5, 3, 7),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
cd2 <- statement_cash_debt(bs_split)
check("debt sums short and long when Total Debt is absent", identical(cd2$debt, 10))

empty <- statement_cash_debt(NULL)
check("missing statements are zero cash and debt", identical(empty$cash, 0) && identical(empty$debt, 0))

is_df <- data.frame(
  Breakdown = c("Total Revenue", "Operating Revenue"),
  `2025` = c(100, 90),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
check("total revenue prefers Total Revenue", identical(statement_total_revenue(is_df), 100))

check("bridge pick uses the first finite override", identical(bridge_pick_number(NA, 4, 9), 4))
check("bridge pick falls through to the statement", identical(bridge_pick_number(NA, NA, 9), 9))

adr <- adopt_valuation_shares(
  list(shares = 2, shares_bs = 200, method = "market_cap_per_price", note = "adr", ratio = 100),
  statement_ccy = "TWD", quote_ccy = "USD"
)
check("ADR shares are the quote-class count", identical(adr$shares, 2) && identical(adr$method, "market_cap_per_price"))

plain <- adopt_valuation_shares(
  list(shares = 10, shares_bs = 10, method = "balance_sheet", note = NULL, ratio = NA),
  statement_ccy = "USD", quote_ccy = "USD"
)
check("same-currency shares stay on the balance sheet", identical(plain$shares, 10))

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
