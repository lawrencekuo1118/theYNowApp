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

#' Render a valuation_df row into a readable multiline explanation (console / lab).
build_signal_explain <- function(row) {
  if (is.null(row)) return(list(text = "資料不足 / no data", lines = character(0)))
  g <- function(x) tryCatch(row[[x]], error = function(e) NA)
  fmt <- function(x, digits = 2, pct = FALSE, na = "N/A") {
    x <- .bv_safe_num(x, NA_real_)
    if (!is.finite(x)) return(na)
    if (pct) sprintf(paste0("%.", digits, "f%%"), x * 100)
    else sprintf(paste0("%.", digits, "f"), x)
  }
  lines <- c(
    sprintf("再平衡日: %s (財年 %s)",
            as.character(g("Date")), as.character(g("fund_year"))),
    sprintf("市價: %s | 策略合理價（勾選平均）: %s",
            fmt(g("hist_price")), fmt(g("fair_value"))),
    sprintf("分項模型 - DCF: %s | DDM: %s | RI: %s | P/B: %s",
            fmt(g("fv_dcf")), fmt(g("fv_ddm")),
            fmt(g("fv_ri")), fmt(g("fv_pb"))),
    sprintf("MOS: %s | 訊號: %s | 估值分數: %s / 100",
            fmt(g("mos"), 1, pct = TRUE),
            as.character(g("signal")),
            fmt(g("valuation_score"), 0)),
    sprintf("Rolling β: %s | Rf: %s | Rm: %s (%s) | We: %s | Ke: %s | WACC: %s",
            fmt(g("rolling_beta"), 2),
            fmt(g("rf_pit"), 1, pct = TRUE),
            fmt(g("rm_pit"), 1, pct = TRUE),
            {
              w <- as.character(g("rm_window"))[1]
              if (is.null(w) || !nzchar(w) || is.na(w)) "—" else w
            },
            fmt(g("we_pit"), 0, pct = TRUE),
            fmt(g("ke_pit"), 1, pct = TRUE),
            fmt(g("wacc_pit"), 1, pct = TRUE)),
    sprintf("Great Filter: %s (%s)",
            if (isTRUE(as.logical(g("filter_pass")))) "PASS" else "FAIL",
            as.character(g("filter_path"))),
    sprintf("目標曝險 - 基本面策略 Exp_A: %s | 情緒策略 Exp_B: %s",
            fmt(g("exp_a"), 2), fmt(g("exp_b"), 2))
  )
  ex <- as.character(g("explain"))
  if (is.character(ex) && length(ex) > 0 && nzchar(ex) && !is.na(ex)) {
    lines <- c(lines, paste0("備註: ", ex))
  }
  list(text = paste(lines, collapse = "\n"), lines = lines)
}

#' Console dump of DCF forecast sync (was renderPrint in ynow_server).
debug_print_fcf_sync <- function(df, claim = "fcff", interest_after_tax = 0,
                                 debt0 = 0, g_path = 0, dcf_mode = NA) {
  if (is.null(df)) {
    message("尚未匯入財報資料，或正在等待計算...")
    return(invisible(NULL))
  }
  tag <- if (exists("dcf_cf_tag", mode = "function")) dcf_cf_tag(claim) else claim
  cfs <- if (exists("extract_dcf_claim_series", mode = "function")) {
    extract_dcf_claim_series(
      df, claim,
      interest_after_tax = interest_after_tax,
      debt0 = debt0,
      g_path = g_path
    )
  } else {
    numeric(0)
  }
  message(tag, " 預測資料已同步")
  if (length(cfs) > 0) {
    message("第 1 年預測現金流: ", round(cfs[1], 2))
    message("第 ", length(cfs), " 年預測現金流: ", round(tail(cfs, 1), 2))
  }
  if (!is.na(dcf_mode)) message("DCF 模式: ", dcf_mode)
  invisible(cfs)
}
