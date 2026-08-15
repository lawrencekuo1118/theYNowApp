# ==========================================
# backtest_validation.R -- The YNow App V12.0
# --------------------------------------------------------------
# Pure (non-Shiny) diagnostic and validation helpers for the
# v12 backtest engine.
#
# Exports:
#   analyze_bh_gap(equity_df, valuation_df)
#   compute_alpha_dashboard(equity_df, rf_annual = 0.04)
#   validate_mos_effectiveness(valuation_df, price_df)
#   validate_fair_value_edge(valuation_df, price_df)
#   run_parameter_plateau(ticker, d_is, d_bs, d_cf, params, model_params, ...)
#   build_signal_explain(row)
# ==========================================

# ---------- small helpers (local, avoid clashing with module) ----------

.bv_safe_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (length(x) < 1 || is.na(x) || !is.finite(x)) default else x
}

.bv_clip <- function(x, lo, hi) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- (lo + hi) / 2
  pmin(pmax(x, lo), hi)
}

.bv_terminal_return <- function(eq) {
  eq <- as.numeric(eq)
  eq <- eq[is.finite(eq)]
  if (length(eq) < 2) return(NA_real_)
  eq[length(eq)] / eq[1] - 1
}

.bv_daily_returns <- function(eq) {
  eq <- as.numeric(eq)
  r <- diff(eq) / head(eq, -1)
  r[!is.finite(r)] <- 0
  r
}

# ==========================================
# 1) Buy-and-Hold attribution
# ==========================================

#' Decompose Strategy A vs Buy&Hold using up-day additive (1−Exp_A)×r.
#'
#' On B&H up-days only, `cash_gap = (1 - Exp_A) * r` is partitioned into:
#' - overvaluation_reduction: signal 價值高估 or MOS < 0
#' - early_exit: Exp_A stepped down vs prior 20-session peak (and not overval)
#' - cash_drag: remaining up-day underinvestment (includes structural max_exp<1
#'   and untagged zeros)
#' These three sum to the additive up-day drag. They are **not** a log-wealth
#' identity. Residual (missed_trend) = compounded terminal shortfall − that sum.
#'
#' @param max_exp Strategy max exposure from this run (shown on the panel)
analyze_bh_gap <- function(equity_df, valuation_df, max_exp = 0.90) {
  stopifnot(is.data.frame(equity_df))
  if (!("Model_B" %in% colnames(equity_df)) && ("Trade_B" %in% colnames(equity_df))) {
    equity_df$Model_B <- equity_df$Trade_B
  }
  req <- c("Date", "Model_B", "BuyHold", "Exp_A", "Exp_B")
  missing <- setdiff(req, colnames(equity_df))
  if (length(missing) > 0) stop("equity_df missing columns: ", paste(missing, collapse = ", "))
  if (!("Trade_A" %in% colnames(equity_df))) stop("equity_df missing Trade_A")
  eq_a <- equity_df$Trade_A
  eq_b <- if ("Trade_B" %in% colnames(equity_df)) equity_df$Trade_B else equity_df$Model_B

  max_exp <- .bv_safe_num(max_exp, 0.90)
  max_exp <- max(0.5, min(1, max_exp))
  max_exp_pct <- as.integer(round(100 * max_exp))

  bh_term <- .bv_terminal_return(equity_df$BuyHold)
  a_term  <- .bv_terminal_return(eq_a)
  b_term  <- .bv_terminal_return(eq_b)
  shortfall_a <- .bv_safe_num(bh_term, 0) - .bv_safe_num(a_term, 0)
  shortfall_b <- .bv_safe_num(bh_term, 0) - .bv_safe_num(b_term, 0)

  n <- nrow(equity_df)
  bh_ret <- .bv_daily_returns(equity_df$BuyHold)
  exp_a <- equity_df$Exp_A[-1]
  exp_b <- equity_df$Exp_B[-1]

  roll_max_prev <- rep(0, n - 1)
  win <- 20L
  for (k in seq_len(n - 1)) {
    lo <- max(1L, k - win + 1L)
    hi <- k
    if (hi >= lo) {
      roll_max_prev[k] <- max(equity_df$Exp_A[lo:hi], na.rm = TRUE)
      if (!is.finite(roll_max_prev[k])) roll_max_prev[k] <- 0
    }
  }
  early_exit_flag <- (exp_a < (roll_max_prev - 0.05))

  state <- data.frame(
    Date = equity_df$Date,
    signal = NA_character_,
    mos = NA_real_,
    filter_pass = NA,
    stringsAsFactors = FALSE
  )
  n_rebal <- 0L
  n_filter_fail <- 0L
  if (!is.null(valuation_df) && is.data.frame(valuation_df) && nrow(valuation_df) > 0) {
    vd <- valuation_df[order(valuation_df$Date), , drop = FALSE]
    n_rebal <- nrow(vd)
    if ("filter_pass" %in% names(vd)) {
      n_filter_fail <- as.integer(sum(vd$filter_pass %in% FALSE, na.rm = TRUE))
    }
    j <- 1L
    cur_sig <- NA_character_; cur_mos <- NA_real_; cur_fp <- NA
    for (i in seq_len(nrow(state))) {
      while (j <= nrow(vd) && vd$Date[j] <= state$Date[i]) {
        cur_sig <- vd$signal[j]
        cur_mos <- vd$mos[j]
        cur_fp <- if ("filter_pass" %in% names(vd)) isTRUE(vd$filter_pass[j]) else NA
        j <- j + 1L
      }
      state$signal[i] <- cur_sig
      state$mos[i]    <- cur_mos
      state$filter_pass[i] <- cur_fp
    }
  }
  sig <- state$signal[-1]
  mos <- state$mos[-1]
  fp  <- state$filter_pass[-1]

  up <- bh_ret > 0
  cash_gap <- (1 - exp_a) * bh_ret * up

  overval_mask <- up & (
    (!is.na(sig) & sig == "價值高估") | (!is.na(mos) & mos < 0)
  )
  overval_contrib <- sum(cash_gap[overval_mask], na.rm = TRUE)

  early_mask <- up & early_exit_flag & !overval_mask
  early_contrib <- sum(cash_gap[early_mask], na.rm = TRUE)

  rest_mask <- up & !overval_mask & !early_mask
  cash_drag_contrib <- sum(cash_gap[rest_mask], na.rm = TRUE)

  sent_gap <- pmax(exp_a - exp_b, 0) * bh_ret * up
  sent_contrib <- sum(sent_gap, na.rm = TRUE)
  sent_boost <- pmax(exp_b - exp_a, 0) * bh_ret * up
  sent_boost_contrib <- sum(sent_boost, na.rm = TRUE)

  additive_up <- overval_contrib + early_contrib + cash_drag_contrib
  missed_trend <- shortfall_a - additive_up

  frac <- function(x) {
    if (!is.finite(shortfall_a) || abs(shortfall_a) <= 1e-6) return(NA_real_)
    x / shortfall_a
  }

  avg_exp_a <- mean(exp_a, na.rm = TRUE)
  avg_exp_b <- mean(exp_b, na.rm = TRUE)
  cash_share_a <- 1 - .bv_safe_num(avg_exp_a, 0)
  n_ret <- length(exp_a)
  n_up <- sum(up, na.rm = TRUE)
  n_zero_a <- sum(exp_a <= 1e-9, na.rm = TRUE)
  pct_zero_a <- if (n_ret > 0) n_zero_a / n_ret else NA_real_
  n_days_filter_fail <- sum(isFALSE(fp), na.rm = TRUE)
  pct_days_filter_fail <- if (n_ret > 0) n_days_filter_fail / n_ret else NA_real_
  pct_rebal_fail <- if (n_rebal > 0) n_filter_fail / n_rebal else NA_real_

  pct0 <- function(x) sprintf("%.0f%%", 100 * .bv_safe_num(x, 0))

  if (shortfall_a > 0.001) {
    headline <- "基本面策略淨值落後 Buy & Hold"
    outcome <- "behind"
  } else if (shortfall_a < -0.001) {
    headline <- "基本面策略淨值領先 Buy & Hold"
    outcome <- "ahead"
  } else {
    headline <- "基本面策略淨值與 Buy & Hold 大致持平"
    outcome <- "flat"
  }

  bullets <- character(0)
  bullets <- c(bullets, sprintf(
    "終值（從 1 起算）：基本面策略 %+0.1f%%，B&H %+0.1f%%，差額 %+0.1f pp（複利）。情緒策略 %+0.1f%%（vs B&H %+0.1f pp）。",
    100 * .bv_safe_num(a_term, 0), 100 * .bv_safe_num(bh_term, 0),
    100 * shortfall_a,
    100 * .bv_safe_num(b_term, 0), 100 * shortfall_b
  ))

  pos_bits <- sprintf(
    "本次設定最大持股 %d%%；平均 Exp_A %s（現金部位報酬＝0）。空手日 %d／%d（%s）。",
    max_exp_pct, pct0(avg_exp_a), n_zero_a, n_ret, pct0(pct_zero_a)
  )
  if (is.finite(n_rebal) && n_rebal > 0 && is.finite(n_filter_fail) && n_filter_fail > 0) {
    pos_bits <- paste0(
      pos_bits,
      sprintf(
        " 持倉條件未過：%d／%d 次再平衡（%s），對應 %d 日（%s）Exp_A＝0。",
        n_filter_fail, n_rebal, pct0(pct_rebal_fail),
        n_days_filter_fail, pct0(pct_days_filter_fail)
      )
    )
  } else if (is.finite(n_rebal) && n_rebal > 0) {
    pos_bits <- paste0(
      pos_bits,
      sprintf(" 持倉條件：%d 次再平衡皆通過（本頁未顯示 Filter 空手）。", n_rebal)
    )
  }
  bullets <- c(bullets, pos_bits)

  bullets <- c(bullets, sprintf(
    "B&H 上漲日 %d／%d：把 (1−Exp_A)×r 加總得 %+0.1f pp（現金拖累 %+0.1f、提前出場 %+0.1f、高估減碼 %+0.1f）。此加總 ≠ 複利終值差 %+0.1f pp；殘差 %+0.1f pp＝終值差−加總（近似，非對數財富恆等）。",
    n_up, n_ret, 100 * additive_up,
    100 * cash_drag_contrib, 100 * early_contrib, 100 * overval_contrib,
    100 * shortfall_a, 100 * missed_trend
  ))

  if (identical(outcome, "behind") && is.finite(bh_term) && bh_term > 0.02) {
    why <- sprintf("本次 B&H 終值為正（%+0.1f%%）。", 100 * bh_term)
    if (max_exp < 0.999) {
      why <- paste0(why, sprintf(" 最大持股 %d%%，即使滿檔也不到 100%%。", max_exp_pct))
    }
    if (n_filter_fail > 0) {
      why <- paste0(why, " Filter 未過日為空手，上漲日會計入現金拖累。")
    }
    bullets <- c(bullets, why)
  }

  narrative_a <- paste(bullets, collapse = " ")
  narrative_b <- sprintf(
    "情緒策略平均 Exp_B %s。相對基本面、僅上漲日：(Exp_A−Exp_B)×r 減碼 %+0.1f pp；加碼 %+0.1f pp。",
    pct0(avg_exp_b), 100 * sent_contrib, 100 * sent_boost_contrib
  )

  list(
    headline = headline,
    outcome = outcome,
    bullets = bullets,
    terminal = list(bh = bh_term, a = a_term, b = b_term,
                    shortfall_a = shortfall_a, shortfall_b = shortfall_b),
    components_a = list(
      cash_drag              = cash_drag_contrib,
      early_exit             = early_contrib,
      overvaluation_reduction = overval_contrib,
      additive_up            = additive_up,
      missed_trend           = missed_trend
    ),
    fractions_a = list(
      cash_drag              = frac(cash_drag_contrib),
      early_exit             = frac(early_contrib),
      overvaluation_reduction = frac(overval_contrib),
      missed_trend           = frac(missed_trend)
    ),
    avg_exp_a = avg_exp_a,
    avg_exp_b = avg_exp_b,
    cash_share_a = cash_share_a,
    max_exp = max_exp,
    n_days = n_ret,
    n_up_days = n_up,
    n_zero_a = n_zero_a,
    pct_zero_a = pct_zero_a,
    n_rebal = n_rebal,
    n_filter_fail = n_filter_fail,
    pct_rebal_fail = pct_rebal_fail,
    n_days_filter_fail = n_days_filter_fail,
    pct_days_filter_fail = pct_days_filter_fail,
    sentiment_reduction_b = sent_contrib,
    sentiment_boost_b = sent_boost_contrib,
    beat_bh_a = shortfall_a <= 0.001,
    narrative_a = narrative_a,
    narrative_b = narrative_b
  )
}

# ==========================================
# 2) Alpha dashboard (CAGR / Sharpe / MDD / Jensen alpha)
# ==========================================

.bv_perf <- function(dates, eq, rf_daily) {
  eq <- as.numeric(eq)
  ok <- is.finite(eq)
  eq <- eq[ok]; dates <- dates[ok]
  if (length(eq) < 20) {
    return(list(cagr = NA_real_, sharpe = NA_real_, mdd = NA_real_))
  }
  rets <- diff(eq) / head(eq, -1)
  rets <- rets[is.finite(rets)]
  mu <- mean(rets - rf_daily)
  sdv <- stats::sd(rets)
  sharpe <- if (isTRUE(sdv > 0)) (mu / sdv) * sqrt(252) else NA_real_
  peak <- cummax(eq); dd <- eq / peak - 1; mdd <- min(dd, na.rm = TRUE)
  yrs <- as.numeric(difftime(dates[length(dates)], dates[1], units = "days")) / 365.25
  cagr <- if (isTRUE(yrs > 0)) eq[length(eq)] ^ (1 / yrs) - 1 else NA_real_
  list(cagr = cagr, sharpe = sharpe, mdd = mdd)
}

#' Compute CAGR / Sharpe / MDD / ExcessReturn(vs BH) / JensenAlpha(vs Benchmark)
#' for BH, Strategy A, Strategy B.
compute_alpha_dashboard <- function(equity_df, rf_annual = 0.04) {
  stopifnot(is.data.frame(equity_df))
  if (!("Model_B" %in% colnames(equity_df)) && ("Trade_B" %in% colnames(equity_df))) {
    equity_df$Model_B <- equity_df$Trade_B
  }
  req <- c("Date", "Model_B", "BuyHold", "Benchmark")
  missing <- setdiff(req, colnames(equity_df))
  if (length(missing) > 0) stop("equity_df missing columns: ", paste(missing, collapse = ", "))
  # Mode A / Mode B strategy NAVs — never FV index.
  if (!("Trade_A" %in% colnames(equity_df))) stop("equity_df missing Trade_A")
  eq_a <- equity_df$Trade_A
  eq_b <- if ("Trade_B" %in% colnames(equity_df)) equity_df$Trade_B else equity_df$Model_B

  rf_d <- (1 + .bv_safe_num(rf_annual, 0.04)) ^ (1 / 252) - 1
  perf_bh <- .bv_perf(equity_df$Date, equity_df$BuyHold, rf_d)
  perf_a  <- .bv_perf(equity_df$Date, eq_a, rf_d)
  perf_b  <- .bv_perf(equity_df$Date, eq_b, rf_d)

  bh_term <- .bv_terminal_return(equity_df$BuyHold)
  a_term  <- .bv_terminal_return(eq_a)
  b_term  <- .bv_terminal_return(eq_b)

  # Jensen's alpha via CAPM on daily excess returns vs Benchmark.
  bench_ret <- .bv_daily_returns(equity_df$Benchmark) - rf_d
  jensen <- function(eq) {
    r <- .bv_daily_returns(eq) - rf_d
    n <- min(length(r), length(bench_ret))
    if (n < 40) return(NA_real_)
    r <- r[seq_len(n)]; b <- bench_ret[seq_len(n)]
    df <- data.frame(y = r, x = b)
    fit <- tryCatch(lm(y ~ x, data = df), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    alpha_d <- unname(stats::coef(fit)["(Intercept)"])
    (1 + alpha_d) ^ 252 - 1
  }
  alpha_bh <- jensen(equity_df$BuyHold)
  alpha_a  <- jensen(eq_a)
  alpha_b  <- jensen(eq_b)

  data.frame(
    Series = c("BuyHold", "StrategyA", "StrategyB"),
    CAGR   = c(perf_bh$cagr, perf_a$cagr, perf_b$cagr),
    Sharpe = c(perf_bh$sharpe, perf_a$sharpe, perf_b$sharpe),
    MaxDD  = c(perf_bh$mdd, perf_a$mdd, perf_b$mdd),
    ExcessReturn = c(0, .bv_safe_num(a_term, NA) - .bv_safe_num(bh_term, NA),
                     .bv_safe_num(b_term, NA) - .bv_safe_num(bh_term, NA)),
    JensenAlpha = c(alpha_bh, alpha_a, alpha_b),
    stringsAsFactors = FALSE
  )
}

# ==========================================
# 3) MOS effectiveness (bucketed forward returns)
# ==========================================

.bv_forward_return <- function(price_df, from_date, horizon_days) {
  price_df <- price_df[is.finite(price_df$Close) & !is.na(price_df$Date), , drop = FALSE]
  price_df <- price_df[order(price_df$Date), , drop = FALSE]
  if (nrow(price_df) < 2) return(NA_real_)
  price_df$Date <- as.Date(price_df$Date)
  from_date <- as.Date(from_date)
  idx0 <- which(price_df$Date >= from_date)[1]
  if (is.na(idx0)) return(NA_real_)
  target <- from_date + as.integer(horizon_days)
  idx1 <- which(price_df$Date >= target)[1]
  if (is.na(idx1)) return(NA_real_)
  p0 <- price_df$Close[idx0]; p1 <- price_df$Close[idx1]
  if (!is.finite(p0) || p0 <= 0 || !is.finite(p1)) return(NA_real_)
  p1 / p0 - 1
}

#' Bucket rebalance rows by MOS and compute forward 1Y / 3Y / 5Y returns.
validate_mos_effectiveness <- function(valuation_df, price_df) {
  if (is.null(valuation_df) || nrow(valuation_df) == 0) {
    return(data.frame(bucket = character(0), n = integer(0),
                      ret_1y = numeric(0), ret_3y = numeric(0), ret_5y = numeric(0),
                      stringsAsFactors = FALSE))
  }
  vd <- valuation_df[is.finite(valuation_df$mos), , drop = FALSE]
  if (nrow(vd) == 0) {
    return(data.frame(bucket = character(0), n = integer(0),
                      ret_1y = numeric(0), ret_3y = numeric(0), ret_5y = numeric(0),
                      stringsAsFactors = FALSE))
  }
  bucket <- ifelse(vd$mos > 0.50, ">50%",
             ifelse(vd$mos > 0.30, "30-50%",
              ifelse(vd$mos > 0.10, "10-30%", "<10%")))
  bucket <- factor(bucket, levels = c(">50%", "30-50%", "10-30%", "<10%"))

  ret1 <- vapply(vd$Date, function(d) .bv_forward_return(price_df, d, 252),  numeric(1))
  ret3 <- vapply(vd$Date, function(d) .bv_forward_return(price_df, d, 252*3), numeric(1))
  ret5 <- vapply(vd$Date, function(d) .bv_forward_return(price_df, d, 252*5), numeric(1))

  agg <- function(v) tapply(v, bucket, function(x) mean(x, na.rm = TRUE))
  n_per <- as.integer(tapply(rep(1L, nrow(vd)), bucket, sum))

  out <- data.frame(
    bucket = levels(bucket),
    n = ifelse(is.na(n_per), 0L, n_per),
    ret_1y = as.numeric(agg(ret1)),
    ret_3y = as.numeric(agg(ret3)),
    ret_5y = as.numeric(agg(ret5)),
    stringsAsFactors = FALSE
  )
  out
}

# ==========================================
# 4) Fair-value edge (undervalued vs overvalued)
# ==========================================

#' Compare forward returns for CHEAP (fair_value edge) vs EXPENSIVE rebalances.
#' Undervalued: signal == "策略低估" OR mos > 0.1.
validate_fair_value_edge <- function(valuation_df, price_df) {
  if (is.null(valuation_df) || nrow(valuation_df) == 0) {
    return(list(
      table = data.frame(group = character(0), n = integer(0),
                         ret_1y = numeric(0), ret_3y = numeric(0), ret_5y = numeric(0),
                         stringsAsFactors = FALSE),
      answer = "資料不足 / insufficient data",
      edge_1y = NA_real_, edge_3y = NA_real_, edge_5y = NA_real_
    ))
  }
  vd <- valuation_df
  # mos = (FV − price) / FV：正值＝模型價高於市價（相對便宜／有安全邊際）
  under <- is.finite(vd$mos) & vd$mos > 0.1
  grp <- ifelse(under, "undervalued_mos>10%", "not_undervalued")
  ret1 <- vapply(vd$Date, function(d) .bv_forward_return(price_df, d, 252),  numeric(1))
  ret3 <- vapply(vd$Date, function(d) .bv_forward_return(price_df, d, 252*3), numeric(1))
  ret5 <- vapply(vd$Date, function(d) .bv_forward_return(price_df, d, 252*5), numeric(1))

  agg <- function(v, g) tapply(v, g, function(x) mean(x, na.rm = TRUE))
  grp_f <- factor(grp, levels = c("undervalued_mos>10%", "not_undervalued"))
  n_per <- as.integer(tapply(rep(1L, nrow(vd)), grp_f, sum))

  tab <- data.frame(
    group = levels(grp_f),
    n = ifelse(is.na(n_per), 0L, n_per),
    ret_1y = as.numeric(agg(ret1, grp_f)),
    ret_3y = as.numeric(agg(ret3, grp_f)),
    ret_5y = as.numeric(agg(ret5, grp_f)),
    stringsAsFactors = FALSE
  )
  edge_1y <- tab$ret_1y[tab$group == "undervalued_mos>10%"] - tab$ret_1y[tab$group == "not_undervalued"]
  edge_3y <- tab$ret_3y[tab$group == "undervalued_mos>10%"] - tab$ret_3y[tab$group == "not_undervalued"]
  edge_5y <- tab$ret_5y[tab$group == "undervalued_mos>10%"] - tab$ret_5y[tab$group == "not_undervalued"]
  edge_1y <- if (length(edge_1y) == 0) NA_real_ else edge_1y
  edge_3y <- if (length(edge_3y) == 0) NA_real_ else edge_3y
  edge_5y <- if (length(edge_5y) == 0) NA_real_ else edge_5y

  ans <- if (is.finite(edge_1y) && edge_1y > 0) {
    sprintf("是。MOS>10%%（模型價高於市價）組 forward 1Y 平均高出 %.1fpp。", 100 * edge_1y)
  } else if (is.finite(edge_1y)) {
    sprintf("否。MOS>10%% 組 1Y 未能勝出，差距 %.1fpp。", 100 * edge_1y)
  } else "資料不足以判斷。"

  list(table = tab, answer = ans,
       edge_1y = edge_1y, edge_3y = edge_3y, edge_5y = edge_5y)
}

# ==========================================
# 5) Parameter plateau (robustness) probe
# ==========================================

#' Perturb WACC / SGR / n_years and compare Mode A FV-index terminal vs baseline.
#' (Mode A is valuation trial — not exposure Sharpe. bt_w_vg is excluded.)
#' Classification on max |relative change| of terminal Model_A:
#'   Stable   : < 5%
#'   Moderate : < 15%
#'   Sensitive: otherwise
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
    error = function(e) { message("baseline failed: ", e$message); NULL }
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
  } else "無有效擾動結果"

  list(status = status, reason = reason, baseline_model_a = base_fv, details = details)
}

# ==========================================
# 6) Signal explainability for UI
# ==========================================

#' Render a valuation_df row (or list) into a readable multiline explanation.
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
    sprintf("市價: %s | 策略公允（勾選平均）: %s",
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
