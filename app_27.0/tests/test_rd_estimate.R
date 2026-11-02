#!/usr/bin/env Rscript
# Pre-tax Rd = Interest Expense / Interest-bearing Debt (WACC then applies Rd×(1−T))
# Reference case aligned with rich01.com WACC article numbers (interest 52, debt 1200).

args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
app <- if (nzchar(script)) {
  dirname(dirname(normalizePath(script)))
} else {
  normalizePath("..")
}
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app, "setup_fs.R"), local = FALSE)
source(file.path(app, "setup.R"), local = FALSE)

.fail <- function(msg) stop(msg, call. = FALSE)
.check <- function(cond, msg) if (!isTRUE(cond)) .fail(msg)

interest <- 52
debt <- 1200
tax <- 0.35
rd_pct <- 100 * interest / debt
.check(abs(rd_pct - (52 / 12)) < 1e-9, "Rd % = interest/debt*100")
rd_after_tax <- (rd_pct / 100) * (1 - tax)
.check(abs(rd_after_tax - 0.0281666667) < 1e-8, "after-tax Rd in WACC")

# Clamp behavior
lo <- 0; hi <- 40
rd_hi <- 100 * 600 / 1000  # 60%
clamped <- max(lo, min(rd_hi, hi))
.check(identical(clamped, 40), "Rd clamp to ceiling")

# Flow selector must prefer finite TTM, but fall back to newest FY when TTM is blank.
# Yahoo often leaves Interest Expense TTM empty while annual columns are populated;
# returning TTM NA blocked auto-feed into rd_interest_expense / 估算 rᵈ.
is_ttm_na <- data.frame(
  Breakdown = c("Interest Expense", "Interest Expense Non Operating"),
  TTM = c(NA_real_, NA_real_),
  `09/27/2025` = c(52, NA_real_),
  `09/28/2024` = c(48, 40),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
is_ttm_na <- reorder_financial_columns(is_ttm_na)
.check(
  identical(
    as.numeric(select_current_metric_any(is_ttm_na, INTEREST_EXPENSE_PATTERNS, "flow")),
    52
  ),
  "flow Interest Expense skips blank TTM and uses newest FY"
)
.check(
  identical(
    as.numeric(select_current_metric(is_ttm_na, "^Interest Expense$", "flow")),
    52
  ),
  "select_current_metric same TTM-NA fallback"
)

is_ttm_ok <- is_ttm_na
is_ttm_ok$TTM[is_ttm_ok$Breakdown == "Interest Expense"] <- 55
.check(
  identical(
    as.numeric(select_current_metric_any(is_ttm_ok, INTEREST_EXPENSE_PATTERNS, "flow")),
    55
  ),
  "finite TTM still preferred over FY"
)

# Production Rd numerator helper (same body as ynow_server.R)
parsed <- parse(file.path(app, "ynow_server.R"), keep.source = FALSE)
assignments <- list()
walk <- function(x) {
  if (missing(x) || (!is.call(x) && !is.expression(x))) return()
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]])) {
    assignments[[as.character(x[[2]])]] <<- x[[3]]
  }
  for (j in seq_along(x)) walk(x[[j]])
}
walk(parsed)
.stopifnot_has <- !is.null(assignments[[".scraped_interest_expense"]])
.check(.stopifnot_has, ".scraped_interest_expense extractable from ynow_server.R")

interest_env <- new.env(parent = globalenv())
interest_env$d_income_statement <- function() is_ttm_na
interest_env$select_current_metric_any <- select_current_metric_any
interest_env$INTEREST_EXPENSE_PATTERNS <- INTEREST_EXPENSE_PATTERNS
scraped_fn <- eval(assignments[[".scraped_interest_expense"]], interest_env)
scraped_val <- scraped_fn()
.check(identical(as.numeric(scraped_val), 52), "scraped Interest Expense feeds Rd numerator when TTM blank")
.check(
  identical(attr(scraped_val, "source"), "income_interest_expense"),
  "Rd numerator source remains gross accrual Interest Expense"
)

interest_env$d_income_statement <- function() is_ttm_ok
.check(
  identical(as.numeric(scraped_fn()), 55),
  "scraped Interest Expense prefers finite TTM"
)

rd_env <- new.env(parent = globalenv())
rd_env$input <- list(
  rd_interest_expense = round(as.numeric(scraped_val), 2),
  rd_interest_bearing_debt = 1200,
  wacc_rd_min = 0,
  wacc_rd_max = 40
)
rd_env$reactive <- shiny::reactive
rd_env$.rd_clamp_bounds <- eval(assignments[[".rd_clamp_bounds"]], rd_env)
rd_fn <- eval(assignments[["scraped_rd_pct"]], rd_env)
rd_session <- shiny::MockShinySession$new()
rd_from_inputs <- shiny::withReactiveDomain(rd_session, shiny::isolate(rd_fn()))
rd_session$close()
.check(
  abs(rd_from_inputs - (100 * 52 / 1200)) < 1e-9,
  "rd pct = 100*52/1200 from fed interest and debt input"
)

# Net Interest alone must stay unavailable (parameter-source audit #574)
interest_env$d_income_statement <- function() {
  data.frame(
    Breakdown = "Net Interest Expense",
    TTM = 9,
    `09/27/2025` = 9,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
net_only <- scraped_fn()
.check(length(net_only) == 1L && is.na(net_only), "Rd numerator refuses Net Interest Expense alone")

cat("test_rd_estimate.R: OK\n")
