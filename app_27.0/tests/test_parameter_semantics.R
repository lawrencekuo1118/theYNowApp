#!/usr/bin/env Rscript
# Adversarial accounting fixtures: related financial rows intentionally differ.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
app <- dirname(dirname(normalizePath(script)))
source(file.path(app, "tests", "test_parameter_source_modes.R"))
source(file.path(app, "market_profile.R"))
source(file.path(app, "relative_multiples_module.R"))
source(file.path(app, "ri_module.R"))
walk(parse(file.path(app, "fcf_projection_module.R")))
mk <- function(labels, newest, prior = newest) {
  data.frame(Breakdown = labels, `2025` = newest, `2024` = prior,
             check.names = FALSE, stringsAsFactors = FALSE)
}
checks <- list()
record <- function(mode, name, actual, expected) {
  ok <- if (is.na(expected)) length(actual) == 1L && is.na(actual) else
    length(actual) == 1L && is.finite(actual) && abs(actual - expected) < 1e-10
  checks[[length(checks) + 1L]] <<- cbind(mode, check = name, actual = actual,
                                        expected = expected, pass = ok)
}
for (i in seq_len(nrow(modes))) {
  mode <- modes[i, ]
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    set_market_mode(mode$market)
    native <- if (mode$market == "TW") "TWD" else "USD"
    set_ynow_currency_context(session_ccy = mode$session_ccy, quote_ccy = native,
                              statement_ccy = native, fx_usd_twd = 32)
    is <- mk(c("Total Revenue", "Operating Income", "Pretax Income", "Tax Provision",
               "Net Income", "Net Income Common Stockholders", "Diluted Average Shares"),
             c(200, 100, 50, 10, 73, 24, 8), c(100, 80, 40, 8, 60, 20, 10))
    bs <- mk(c("Common Stock Equity", "Stockholders Equity", "Total Equity Gross Minority Interest",
               "Ordinary Shares Number", "Total Assets", "Current Assets", "Current Liabilities",
               "Current Debt", "Cash And Cash Equivalents"),
             c(100, 150, 180, 2, 500, 100, 40, 10, 20))
    cf <- mk(c("Depreciation And Amortization", "Change In Working Capital", "Capital Expenditure"),
             c(10, -5, -50), c(20, -3, -40))
    record(mode, "NOPAT uses operating earnings, not Net Income", statement_nopat(is), 80)
    missing_ebit <- is[is$Breakdown != "Operating Income", ]
    record(mode, "missing EBIT does not relabel Net Income as NOPAT", statement_nopat(missing_ebit), NA_real_)
    missing_tax <- is[!is$Breakdown %in% c("Tax Provision", "Pretax Income"), ]
    record(mode, "market-specific statutory tax fallback", statement_nopat(missing_tax),
           if (mode$market == "TW") 80 else 79)
    record(mode, "common earnings precede consolidated earnings",
           select_current_metric_any(is, NET_INCOME_PATTERNS, "flow"), 24)
    record(mode, "common equity precedes preferred/minority-inclusive equity",
           select_current_metric_any(bs, EQUITY_PATTERNS, "stock"), 100)
    record(mode, "EPS uses weighted-average shares, never ending BS shares",
           .rel_trailing_eps_from_statements(NULL, is, bs), 3)
    record(mode, "report and model use the same EPS resolver",
           .report_eps_bvps(NULL, is, bs)$eps, 3)
    no_weight <- is[is$Breakdown != "Diluted Average Shares", ]
    record(mode, "missing weighted shares do not trigger ending-share substitution",
           .rel_trailing_eps_from_statements(NULL, no_weight, bs), NA_real_)
    parent_only <- bs[bs$Breakdown != "Common Stock Equity", ]
    record(mode, "unknown preferred claim is not common book equity", common_stock_equity(parent_only), NA_real_)
    preferred <- rbind(parent_only, mk("Preferred Stock Equity", 50))
    record(mode, "common book equity subtracts preferred claim", common_stock_equity(preferred), 100)
    total_income <- is[is$Breakdown != "Net Income Common Stockholders", ]
    record(mode, "unknown preferred earnings claim is not common NI", common_net_income(total_income), NA_real_)
    total_income <- rbind(total_income, mk("Preferred Stock Dividends", 10))
    record(mode, "common NI excludes preferred dividends", common_net_income(total_income), 63)
    paid <- mk(c("Cash Dividends Paid", "Preferred Stock Dividend Paid"), c(-30, -10))
    record(mode, "DDM and common payout exclude preferred dividends", common_cash_dividends(paid), 20)
    record(mode, "unknown dividend attribution is unavailable",
           common_cash_dividends(paid[paid$Breakdown == "Cash Dividends Paid", ]), NA_real_)
    reported_eps <- rbind(is, mk("Diluted EPS", 2.5))
    record(mode, "reported diluted EPS wins over computed and Summary alternatives",
           valuation_trailing_eps(data.frame(Item = "EPS (TTM)", Value = "999"), reported_eps), 2.5)
    dividend_summary <- data.frame(Item = c("Forward Annual Dividend Rate", "Dividend Yield", "Trailing Annual Dividend Rate"), Value = c("999", "55%", "2"))
    record(mode, "DDM D0 is trailing DPS, never forward dividend or yield",
           valuation_trailing_dps(dividend_summary), 2)
    record(mode, "missing trailing DPS cannot become a forward rate",
           valuation_trailing_dps(dividend_summary[1:2, ]), NA_real_)
    cf_unlever <- mk(c("Operating Cash Flow", "Capital Expenditure", "Free Cash Flow"), c(100, -20, 999))
    is_unlever <- mk(c("Interest Expense", "Pretax Income", "Tax Provision"), c(10, 100, 20))
    record(mode, "EV/FCF uses reconstructed FCFF, not levered/raw Yahoo FCF",
           .rel_latest_fcff(cf_unlever, is_unlever), 88)
    record(mode, "unknown interest cannot become a zero FCFF adjustment",
           .rel_latest_fcff(cf_unlever, is_unlever[is_unlever$Breakdown != "Interest Expense", ]), NA_real_)
    env <- new.env(parent = globalenv())
    env$d_income_statement <- function() missing_ebit
    env$d_balance_sheet <- function() bs
    env$d_cash_flow <- function() cf
    env$shared_bridge <- NULL
    env$session <- NULL
    env$req <- function(...) invisible(TRUE)
    updates <- new.env(parent = emptyenv())
    env$updateNumericInput <- function(session, id, value) updates[[id]] <- value
    env$.capex_spike_settings <- function() list(enabled = FALSE, mult = 1.35, avg_n = 3, prior_n = 2)
    sync <- eval(assignments[["do_sync_financials"]], env)
    sync()
    record(mode, "production FCF sync refuses Net Income as NOPAT", updates$fcf_nopat, NA_real_)
    hist <- eval(assignments[["extract_dcf_parameters"]], env)(is, cf, bs)
    record(mode, "historical tax accepts Tax Provision alias", hist$tax_rate, .2)
    record(mode, "each year's CapEx uses that year's D&A", hist$net_capex_margin, .2)
    interest_env <- new.env(parent = globalenv())
    interest_env$d_income_statement <- function() mk("Net Interest Expense", 4)
    interest_env$d_cash_flow <- function() mk("Interest Paid", 9)
    interest <- eval(assignments[[".scraped_interest_expense"]], interest_env)
    record(mode, "Rd numerator cannot substitute cash or net interest", interest(), NA_real_)
    interest_env$d_income_statement <- function() mk(c("Net Interest Expense", "Interest Expense"), c(4, 7))
    record(mode, "Rd numerator uses gross accrual interest expense", as.numeric(interest()), 7)
    for (method in c("constant", "linear", "industry", "custom")) {
      ri_env <- new.env(parent = globalenv())
      ri_env$input <- list(`mod_ri-b0` = 10, `mod_ri-ri_ke` = 9, `mod_ri-ri_g` = 2,
                          `mod_ri-ri_years` = 5, `mod_ri-ri_payout` = 30,
                          `mod_ri-ri_roe` = 20, `mod_ri-roe_method` = method,
                          `mod_ri-roe_terminal` = 10, `mod_ri-roe_industry` = 12,
                          `mod_ri-roe_custom_txt` = "30,25,20,15,10")
      price <- eval(assignments[[".ri_price_at"]], ri_env)()
      path <- switch(method, constant = rep(.2, 5), linear = c(.2, .175, .15, .125, .1),
                     industry = c(.2, .18, .16, .14, .12), custom = c(.3, .25, .2, .15, .1))
      expected <- compute_ri_valuation(10, .09, .02, 5, .3, path)$intrinsic
      record(mode, paste("RI scenario honors selected ROE path", method), price, expected)
    }
  })
  session$close()
}
# Real reactive domains verify connection isolation, beyond mode-label permutations.
first <- shiny::MockShinySession$new()
second <- shiny::MockShinySession$new()
shiny::withReactiveDomain(first, {
  set_market_mode("TW")
  set_ynow_currency_context("TWD", 32, "TWD", "TWD")
})
shiny::withReactiveDomain(second, {
  set_market_mode("US")
  set_ynow_currency_context("USD", 40, "USD", "USD")
})
shiny::withReactiveDomain(first, {
  stopifnot(identical(get_market_mode(), "TW"), money_prefix() == "NT$",
            fx_factor("USD", "TWD") == 32)
})
shiny::withReactiveDomain(second, {
  stopifnot(identical(get_market_mode(), "US"), money_prefix() == "$",
            fx_factor("USD", "TWD") == 40)
})
first$close(); second$close()
cat("PASS: independent reactive-domain market/currency/FX contexts\n")
checks <- do.call(rbind, checks)
output <- Sys.getenv("YNOW_PARAMETER_SEMANTICS_CSV", "")
if (nzchar(output)) write.csv(checks, output, row.names = FALSE)
cat("Accounting parameter semantic checks:", nrow(checks), " Failures:", sum(!checks$pass), "\n")
if (any(!checks$pass)) {
  print(unique(checks[!checks$pass, c("market", "check", "actual", "expected")]))
  quit(status = 1L)
}
cat("ALL ACCOUNTING PARAMETER SEMANTICS CHECKS PASSED\n")
