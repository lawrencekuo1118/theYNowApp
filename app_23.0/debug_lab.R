# ==========================================
# debug_lab.R — local probes only (NOT sourced by global.R / app.R)
# ==========================================
# Usage (interactive):
#   Sys.setenv(YNOW_DEBUG = "1")
#   source("backtest_module.R"); source("backtest_validation.R")
#   source("debug_lab.R")
#
# Requires: .bv_safe_num, run_company_backtest (from backtest_* files).

#' Perturb WACC / SGR / n_years and compare Mode A FV-index terminal vs baseline.
run_parameter_plateau <- function(ticker, d_is, d_bs, d_cf, params, model_params,
                                  bench_ticker = "SPY", years = 5,
                                  mos = NA_real_, verbose = FALSE) {
  if (!exists("run_company_backtest", mode = "function")) {
    stop("run_company_backtest not available; source backtest_module.R first")
  }
  .fv_terminal <- function(res) {
    if (is.null(res) || is.null(res$equity_df)) return(NA_real_)
    eq <- as.numeric(res$equity_df$Model_A)
    eq <- eq[is.finite(eq) & eq > 0]
    if (length(eq) < 2) return(NA_real_)
    eq[length(eq)]
  }
  baseline <- tryCatch(
    run_company_backtest(ticker, d_is, d_bs, d_cf,
                         params = params, model_params = model_params,
                         mos = mos, bench_ticker = bench_ticker, years = years),
    error = function(e) {
      if (isTRUE(verbose) || (exists(".ynow_debug_on") && isTRUE(.ynow_debug_on()))) {
        message("baseline failed: ", e$message)
      }
      NULL
    }
  )
  if (is.null(baseline)) {
    return(list(status = "資料不足", reason = "無法建立基準回測", details = NULL))
  }
  base_fv <- .bv_safe_num(.fv_terminal(baseline), NA_real_)

  scenarios <- list(
    list(name = "wacc +1pp",  mp = modifyList(model_params, list(wacc = .bv_safe_num(model_params$wacc, 0.09) + 0.01)), p = params),
    list(name = "wacc -1pp",  mp = modifyList(model_params, list(wacc = .bv_safe_num(model_params$wacc, 0.09) - 0.01)), p = params),
    list(name = "sgr +1pp",   mp = modifyList(model_params, list(sgr  = .bv_safe_num(model_params$sgr,  0.025) + 0.01)), p = params),
    list(name = "sgr -1pp",   mp = modifyList(model_params, list(sgr  = .bv_safe_num(model_params$sgr,  0.025) - 0.01)), p = params),
    list(name = "n_years +1", mp = modifyList(model_params, list(n_years = as.integer(.bv_safe_num(model_params$n_years, 5)) + 1L)), p = params),
    list(name = "n_years -1", mp = modifyList(model_params, list(n_years = max(1L, as.integer(.bv_safe_num(model_params$n_years, 5)) - 1L))), p = params)
  )

  rows <- lapply(scenarios, function(sc) {
    res <- tryCatch(
      run_company_backtest(ticker, d_is, d_bs, d_cf,
                           params = sc$p, model_params = sc$mp,
                           mos = mos, bench_ticker = bench_ticker, years = years),
      error = function(e) { if (verbose) message(sc$name, ": ", e$message); NULL }
    )
    fv <- .bv_safe_num(.fv_terminal(res), NA_real_)
    d_rel <- if (is.finite(fv) && is.finite(base_fv) && abs(base_fv) > 1e-9) {
      (fv - base_fv) / base_fv
    } else {
      NA_real_
    }
    data.frame(
      scenario = sc$name,
      model_a_end = fv,
      d_rel = d_rel,
      stringsAsFactors = FALSE
    )
  })
  details <- do.call(rbind, rows)

  d_abs <- abs(details$d_rel)
  d_abs <- d_abs[is.finite(d_abs)]
  worst <- if (length(d_abs) == 0) NA_real_ else max(d_abs)
  status <- if (!is.finite(worst)) "資料不足"
            else if (worst < 0.05) "穩定 (Stable)"
            else if (worst < 0.15) "中等 (Moderate)"
            else "敏感 (Sensitive)"

  reason <- if (nrow(details) > 0 && any(is.finite(details$d_rel))) {
    ix <- which.max(abs(ifelse(is.finite(details$d_rel), details$d_rel, 0)))
    sprintf("最大 Mode A 終值變動來自「%s」(dRel=%+.1f%%, 基準終值=%.3f)",
            details$scenario[ix], 100 * details$d_rel[ix], base_fv)
  } else {
    "無有效情境"
  }

  list(status = status, reason = reason, baseline_model_a = base_fv, details = details)
}
