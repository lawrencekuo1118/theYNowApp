# TPEx capitalization index history (Yahoo IX0043.TWO is thin; official API fallback)
# Run: cd app_21.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_tpex_index_history.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
root <- if (file.exists("backtest_module.R")) {
  normalizePath(".")
} else if (file.exists("../backtest_module.R")) {
  normalizePath("..")
} else {
  stop("Cannot locate backtest_module.R")
}
setwd(root)

source("backtest_module.R", local = TRUE, encoding = "UTF-8")

check <- function(label, ok) {
  if (!isTRUE(ok)) stop("FAIL: ", label, call. = FALSE)
  cat("OK ", label, "\n", sep = "")
}

check("ROC date parse", identical(.ynow_parse_tw_slash_date("113/10/01"), as.Date("2024-10-01")))
check("Gregorian date parse", identical(.ynow_parse_tw_slash_date("2024/10/01"), as.Date("2024-10-01")))
check("symbol detector", isTRUE(.is_tpex_index_symbol("IX0043.TWO")) && isTRUE(.is_tpex_index_symbol("ix0043.two")))
check("period months 1y", identical(.tpex_index_period_months("1y"), 12L))
check("period months 5d", identical(.tpex_index_period_months("5d"), 1L))

# Fixture-shaped month row → Close column index 5
toy_row <- list("113/10/01", "887,960", "97,634,980", "731,030", 270.28, 1.83)
check("fixture close is numeric", is.finite(as.numeric(toy_row[[5]])))

live <- identical(Sys.getenv("YNOW_TPEX_INDEX_LIVE"), "0")
if (isTRUE(live)) {
  cat("SKIP live TPEx index fetch (YNOW_TPEX_INDEX_LIVE=0)\n")
} else {
  one <- tryCatch(.tpex_index_fetch_month(as.Date("2024-10-01")), error = function(e) e)
  check(
    "live month fetch October 2024",
    is.data.frame(one) && nrow(one) >= 15L &&
      all(c("Date", "Close") %in% names(one)) &&
      sum(is.finite(one$Close)) >= 15L
  )
  hist <- tryCatch(fetch_tpex_index_history_df("3mo"), error = function(e) e)
  check(
    "live 3mo history has tape",
    is.data.frame(hist) && nrow(hist) >= 40L &&
      sum(is.finite(hist$Close)) >= 40L
  )
  # Simulate Yahoo-thin path: direct official fetch used by price helper.
  via <- tryCatch(fetch_tpex_index_history_df("1mo"), error = function(e) NULL)
  check("1mo enough for line chart", is.data.frame(via) && nrow(via) >= 2L)
}

cat("PASS test_tpex_index_history\n")
