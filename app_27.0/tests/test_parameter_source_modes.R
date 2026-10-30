#!/usr/bin/env Rscript
# Exercise the production WACC resolver rather than a copy of its arithmetic.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
app <- dirname(dirname(normalizePath(script)))
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app, "setup.R"))
source(file.path(app, "industry_standards.R"))
source(file.path(app, "default_config.R"))
parsed <- parse(file.path(app, "ynow_server.R"))
assignments <- list()
walk <- function(x) {
  if (missing(x) || (!is.call(x) && !is.expression(x))) return()
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]])) {
    assignments[[as.character(x[[2]])]] <<- x[[3]]
  }
  for (j in seq_along(x)) walk(x[[j]])
}
walk(parsed)
stopifnot(is.call(assignments[[".auto_recalc_capm_wacc"]]))
cell <- function(initial = NULL) {
  value <- initial
  function(x) { if (!missing(x)) value <<- x; value }
}

# Market-specific native data; never compare TW values with US values.
# Identity and language must not select a separate financial parameter source.
modes <- expand.grid(
  market = c("US", "TW"), locale = c("en", "zh-TW"),
  edition = c("Full", "Lite"),
  identity = c("guest", "member", "ops_admin", "sysadmin",
               "sysadmin_guest", "sysadmin_signed_in", "sysadmin_omniscient"),
  session_ccy = c("USD", "TWD"), stringsAsFactors = FALSE
)
failures <- character()
records <- list()
for (i in seq_len(nrow(modes))) {
  mode <- modes[i, ]
  env <- new.env(parent = globalenv())
  native <- if (mode$market == "TW") "TWD" else "USD"
  fx <- 32
  market_cap_native <- if (native == "TWD") 96000 else 2000
  debt_native <- if (native == "TWD") 32000 else 500
  tax <- if (native == "TWD") 20 else 21
  # All monetary statements are already normalized by d_balance_sheet().
  debt_session <- money_to_session(debt_native, native, mode$session_ccy, fx)
  env$input <- list(capm_rf = 4, capm_beta = 1.2, capm_rm = 9,
                    use_estimated_re = TRUE, wacc_re = 10, wacc_rd = 5,
                    wacc_tax = tax, dcf_mode = "gordon",
                    ynow_lite_mode = mode$edition == "Lite")
  env$ui_locale <- function() mode$locale
  env$market_mode <- function() mode$market
  env$session_currency <- function() mode$session_ccy
  env$quote_currency <- function() native
  env$statement_currency <- function() native
  env$fx_usd_twd <- function() fx
  env$current_ticker <- function() if (native == "TWD") "2330.TW" else "AAPL"
  env$d_balance_sheet <- function() data.frame(Breakdown = "Total Debt", `2025` = debt_session)
  env$summary_data <- function() data.frame(Item = "Market Cap", Value = market_cap_native)
  # Isolate only the provider call: downstream production source selection is real.
  env$resolve_valuation_shares <- function(...) {
    list(shares = 100, price = market_cap_native / 100,
         market_cap = market_cap_native, method = "balance_sheet")
  }
  env$.shared_bridge <- function() list(debt = debt_session, other_claims = 0)
  env$estimated_re <- cell()
  env$calculated_wacc <- cell()
  env$session <- NULL
  env$updateNumericInput <- function(...) invisible(NULL)
  env$APP_DEFAULTS <- APP_DEFAULTS
  fn <- eval(assignments[[".auto_recalc_capm_wacc"]], env)
  fn()
  actual <- env$calculated_wacc()
  ke <- .04 + 1.2 * (.09 - .04)
  expected <- (market_cap_native * ke + debt_native * .05 * (1 - tax / 100)) /
    (market_cap_native + debt_native)
  ok <- length(actual) == 1 && is.finite(actual) && abs(actual - expected) < 1e-12
  label <- paste(unlist(mode), collapse = "/")
  records[[i]] <- cbind(mode, expected = expected, actual = actual, pass = ok)
  if (!ok) failures <- c(failures, label)
}
output <- Sys.getenv("YNOW_PARAMETER_SOURCE_AUDIT_CSV", "")
if (nzchar(output)) write.csv(do.call(rbind, records), output, row.names = FALSE)
cat("WACC parameter-source contexts:", nrow(modes), " Failures:", length(failures), "\n")
all_failures <- failures

# Run DCF and its scenario/sensitivity resolver must read the same effective
# scenario, including while a hidden Lite radio is waiting to echo an update.
dcf_records <- list()
for (i in seq_len(nrow(modes))) for (delayed in c(FALSE, TRUE)) {
  mode <- modes[i, ]
  env <- new.env(parent = globalenv())
  lite <- mode$edition == "Lite"
  env$input <- list(years = 5, dcf_mode = if (delayed) "gordon" else "two_stage",
                    dcf_claim = "fcff", sgr = 2, wacc_gordon = 12,
                    wacc_stage1 = 8, wacc_stage2 = 10, yr_stage1 = 2,
                    g_stage1 = 10, use_estimated_re = FALSE, wacc_re = 11,
                    wacc_rd = 5, wacc_tax = if (mode$market == "TW") 20 else 21)
  env$current_ticker <- function() if (mode$market == "TW") "2330.TW" else "AAPL"
  env$lite_mode <- function() lite
  env$.effective_dcf_scenario <- eval(assignments[[".effective_dcf_scenario"]], env)
  env$lite_dcf_silent <- function() lite
  env$lite_desired_des <- function() if (lite) list(two_stage = TRUE, claim = "fcff") else NULL
  env$fcf_results <- list(df_fcf = function() data.frame(FCFF = c(10, 12, 14, 16, 18)))
  env$.shared_bridge <- function() list(cash = 20, debt = 10, shares = 10, other_claims = 0)
  env$.valuation_shares <- function() list(shares = 10, method = "balance_sheet")
  env$.dcf_per_share <- function(eq, sh) eq / sh$shares
  env$calculated_wacc <- function() .1
  env$estimated_re <- function() .11
  env$stock_price_estimate_val <- cell()
  env$dcf_value_result <- cell()
  env$dcf_audit_snap <- cell()
  env$lite_dcf_block <- cell()
  env$isolate <- function(x) x
  env$req <- function(...) invisible(TRUE)
  env$updateNumericInput <- function(...) invisible(NULL)
  env$.notify_g_discount_and_guide <- function(...) stop("unexpected invalid fixture")
  env$APP_DEFAULTS <- APP_DEFAULTS
  env$.execute_dcf_calc <- eval(assignments[[".execute_dcf_calc"]], env)
  env$.dcf_valuation_bundle <- eval(assignments[[".dcf_valuation_bundle"]], env)
  env$.execute_dcf_calc(notify = FALSE)
  run <- env$stock_price_estimate_val()
  scenario <- env$.dcf_valuation_bundle()$price
  ok <- is.finite(run) && is.finite(scenario) && abs(run - scenario) < 1e-12
  dcf_records[[length(dcf_records) + 1L]] <- cbind(mode, delayed = delayed,
                                                run = run, scenario = scenario, pass = ok)
  if (!ok) all_failures <- c(all_failures, paste("DCF", paste(unlist(mode), collapse = "/"), delayed))
}
dcf_records <- do.call(rbind, dcf_records)
if (nzchar(output)) write.csv(dcf_records, sub("\\.csv$", "-dcf.csv", output), row.names = FALSE)
cat("DCF run/scenario contexts:", nrow(dcf_records), " Failures:", sum(!dcf_records$pass), "\n")

# Rf must use this session's market even if another session changes the legacy
# process-wide context. Providers are deterministic; routing functions are real.
provider <- new.env(parent = globalenv())
sys.source(file.path(app, "market_profile.R"), provider)
walk(parse(file.path(app, "web_crawler.R")))
for (name in c(".rf_market_mode", ".rf_profile_bits", ".rf_documented_fallback",
               "get_risk_free_rate_detail")) {
  assign(name, eval(assignments[[name]], provider), provider)
}
provider$.rf_live_last <- new.env(parent = emptyenv())
provider$.ynow_log <- function(...) invisible(NULL)
provider$.cached_fetch_rf_live_us <- function() 4.2
provider$.cached_fetch_rf_live_tw <- function() 1.6
provider$cached_get_risk_free_rate_detail <- provider$get_risk_free_rate_detail
rf_records <- list()
for (i in seq_len(nrow(modes))) for (other_market in c("US", "TW")) {
  mode <- modes[i, ]
  env <- new.env(parent = provider)
  env$market_mode <- function() mode$market
  env$input <- list(capm_rf = if (mode$market == "TW") 1.6 else 4.2)
  env$APP_DEFAULTS <- APP_DEFAULTS
  provider$set_market_mode(other_market)
  fn <- eval(assignments[[".current_rf_detail"]], env)
  actual <- fn()
  expected_symbol <- if (mode$market == "TW") "TPEX_CURVE_10Y" else "^TNX"
  ok <- identical(actual$symbol, expected_symbol)
  rf_records[[length(rf_records) + 1L]] <- cbind(mode, other_market = other_market,
                                               expected = expected_symbol,
                                               actual = actual$symbol, pass = ok)
  if (!ok) all_failures <- c(all_failures, paste("Rf cross-session", paste(unlist(mode), collapse = "/"), other_market))
}
rf_records <- do.call(rbind, rf_records)
if (nzchar(output)) write.csv(rf_records, sub("\\.csv$", "-rf.csv", output), row.names = FALSE)
cat("Rf session-isolation contexts:", nrow(rf_records), " Failures:", sum(!rf_records$pass), "\n")
if (length(all_failures)) {
  cat(paste(head(all_failures, 8), collapse = "\n"), "\n")
  quit(status = 1L)
}
cat("ALL PARAMETER SOURCE MODE CHECKS PASSED\n")
