# ==========================================
# backtest_module.R — 公司專屬參數推導 + 真實回測引擎
# ==========================================

.clip01 <- function(x, lo = 0, hi = 1) {
  x <- as.numeric(x)
  if (length(x) != 1 || is.na(x) || !is.finite(x)) return((lo + hi) / 2)
  max(lo, min(hi, x))
}

.safe_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (length(x) < 1 || is.na(x) || !is.finite(x)) default else x
}

#' 從財報欄位建立「財年 → 指標」表（排除 TTM）
build_annual_fundamentals <- function(d_is, d_bs, d_cf) {
  empty <- data.frame(
    year = integer(0), net_margin = numeric(0), rev_growth = numeric(0),
    eps_growth = numeric(0), fcf = numeric(0), stringsAsFactors = FALSE
  )
  if (is.null(d_is) || !is.data.frame(d_is) || ncol(d_is) < 2) return(empty)

  period_cols <- colnames(d_is)[-1]
  period_cols <- period_cols[!grepl("^ttm$", period_cols, ignore.case = TRUE)]
  if (length(period_cols) == 0) return(empty)

  parse_year <- function(col) {
    d <- suppressWarnings(as.Date(col, format = "%m/%d/%Y"))
    if (is.na(d)) d <- suppressWarnings(as.Date(col))
    if (is.na(d)) {
      y <- suppressWarnings(as.integer(sub(".*?(\\d{4}).*", "\\1", col)))
      return(y)
    }
    as.integer(format(d, "%Y"))
  }

  years <- vapply(period_cols, parse_year, integer(1))
  ok <- !is.na(years)
  period_cols <- period_cols[ok]
  years <- years[ok]
  if (length(years) == 0) return(empty)

  pick_val <- function(df, patterns, col) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
    for (pat in patterns) {
      idx <- grep(pat, df[[1]], ignore.case = TRUE)
      if (length(idx) == 0) next
      if (!(col %in% colnames(df))) return(NA_real_)
      return(parse_financial_number(df[idx[1], col])[1])
    }
    NA_real_
  }

  rev <- vapply(period_cols, function(c) pick_val(d_is, c("Total Revenue", "^Revenue$"), c), numeric(1))
  ni  <- vapply(period_cols, function(c) pick_val(d_is, NET_INCOME_PATTERNS, c), numeric(1))
  fcf <- vapply(period_cols, function(c) pick_val(d_cf, c("^Free Cash Flow$"), c), numeric(1))

  # 成長：相對「更舊一欄」（欄位已是新→舊）
  rev_g <- rep(NA_real_, length(rev))
  eps_g <- rep(NA_real_, length(ni))
  if (length(rev) >= 2) {
    for (i in seq_len(length(rev) - 1)) {
      if (!is.na(rev[i]) && !is.na(rev[i + 1]) && abs(rev[i + 1]) > 0) {
        rev_g[i] <- (rev[i] - rev[i + 1]) / abs(rev[i + 1]) * 100
      }
      if (!is.na(ni[i]) && !is.na(ni[i + 1]) && abs(ni[i + 1]) > 0) {
        eps_g[i] <- (ni[i] - ni[i + 1]) / abs(ni[i + 1]) * 100
      }
    }
  }

  npm <- ifelse(!is.na(ni) & !is.na(rev) & abs(rev) > 0, ni / rev * 100, NA_real_)

  data.frame(
    year = years,
    net_margin = npm,
    rev_growth = rev_g,
    eps_growth = eps_g,
    fcf = fcf,
    stringsAsFactors = FALSE
  )
}

#' 拉取日線（優先 yfinance / reticulate）
fetch_price_history_df <- function(ticker, period = "5y") {
  ticker <- toupper(trimws(as.character(ticker)[1]))
  if (!nzchar(ticker)) return(NULL)

  df <- tryCatch({
    if (!exists("get_price_history", mode = "function")) stop("get_price_history missing")
    res <- get_price_history(ticker, period)
    dates <- as.character(unlist(res$Date, use.names = FALSE))
    closes <- suppressWarnings(as.numeric(unlist(res$Close, use.names = FALSE)))
    vols <- suppressWarnings(as.numeric(unlist(res$Volume, use.names = FALSE)))
    if (length(dates) == 0 || length(closes) == 0) stop("empty history")
    n <- min(length(dates), length(closes), length(vols))
    data.frame(
      Date = as.Date(dates[seq_len(n)]),
      Close = closes[seq_len(n)],
      Volume = vols[seq_len(n)],
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("⚠️ yfinance history 失敗 (", ticker, "): ", e$message)
    NULL
  })

  if (!is.null(df)) {
    df <- df[is.finite(df$Close) & !is.na(df$Date), , drop = FALSE]
    df <- df[order(df$Date), , drop = FALSE]
    if (nrow(df) >= 30) return(df)
  }

  # 後備：quantmod（本機常見）
  tryCatch({
    if (!requireNamespace("quantmod", quietly = TRUE)) return(NULL)
    xt <- quantmod::getSymbols(ticker, src = "yahoo", auto.assign = FALSE,
                               from = Sys.Date() - 365 * 5, to = Sys.Date())
    out <- data.frame(Date = zoo::index(xt), zoo::coredata(xt), stringsAsFactors = FALSE)
    names(out) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    out[, c("Date", "Close", "Volume")]
  }, error = function(e) NULL)
}

.calc_rsi <- function(closes, n = 14) {
  closes <- as.numeric(closes)
  if (length(closes) < n + 2) return(rep(NA_real_, length(closes)))
  tryCatch({
    as.numeric(TTR::RSI(closes, n = n))
  }, error = function(e) rep(NA_real_, length(closes)))
}

#' 1) 依公司財報／動能／MOS 推導 Backtest 參數
derive_bt_params <- function(d_is, d_bs, d_cf,
                             hist_df = NULL,
                             mos = NA_real_,
                             industry_choice = NULL) {
  npm <- {
    net <- get_avg(select_clean_metric_row_any(d_is, NET_INCOME_PATTERNS, include_ttm = FALSE))
    rev <- get_avg(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    if (!is.na(net) && !is.na(rev) && rev != 0) net / rev * 100 else NA_real_
  }
  rev_g <- get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
  eps_g <- get_avg_growth(select_clean_metric_row_any(d_is, NET_INCOME_PATTERNS, include_ttm = FALSE))

  fcf_row <- select_clean_metric_row(d_cf, "^Free Cash Flow$", include_ttm = FALSE)
  fcf_cv <- NA_real_
  if (length(fcf_row) >= 2) {
    x <- as.numeric(na.omit(fcf_row))
    if (length(x) >= 2) {
      m <- mean(x)
      fcf_cv <- stats::sd(x) / max(abs(m), 1e-9) * 100
    }
  }

  # 產業校準（若有）
  ind_rev <- NA_real_
  if (!is.null(industry_choice) && exists("industry_standards")) {
    ind <- industry_standards[[industry_choice]]
    if (!is.null(ind$rev_growth)) ind_rev <- mean(as.numeric(ind$rev_growth), na.rm = TRUE)
  }

  npm_use <- .safe_num(npm, 5)
  rev_use <- .safe_num(rev_g, .safe_num(ind_rev, 10))
  eps_use <- .safe_num(eps_g, max(rev_use * 0.8, 5))
  cv_use  <- .safe_num(fcf_cv, 20)

  # 門檻：以「該公司自身水準」的一半／略寬 CV 為通過線（並設合理上限）
  bt_net_margin <- round(max(0, min(25, npm_use * 0.5)), 1)
  bt_rev_growth <- round(max(0, min(40, rev_use * 0.5)), 1)
  bt_eps_growth <- round(max(0, min(40, eps_use * 0.5)), 1)
  bt_fcf_cv     <- round(max(8, min(80, cv_use * 1.25)), 1)

  mos_n <- .safe_num(mos, 0)
  # Mode A（顯示名）：VG 決定估值曝險強度（與 Mom／RSI 無關，不做 1−VG 互補）
  w_vg <- .clip01(0.35 + 0.5 * mos_n, 0.2, 0.8)

  mom_on <- FALSE
  rsi_last <- 50
  if (!is.null(hist_df) && is.data.frame(hist_df) && nrow(hist_df) >= 60) {
    px <- hist_df$Close
    ma20 <- tryCatch(tail(as.numeric(TTR::SMA(px, 20)), 1), error = function(e) NA)
    ma60 <- tryCatch(tail(as.numeric(TTR::SMA(px, 60)), 1), error = function(e) NA)
    cur <- tail(px, 1)
    mom_on <- isTRUE(cur > ma20 && cur > ma60 && ma20 > ma60)
    rsi_v <- .calc_rsi(px)
    rsi_last <- .safe_num(tail(rsi_v, 1), 50)
  }

  # Mode B（顯示名）：Mom／RSI 為情緒疊加的相對權重（正規化至合計 1）
  if (isTRUE(mom_on)) {
    w_mom <- 0.60
    w_rsi <- 0.40
  } else if (isTRUE(rsi_last < 35)) {
    w_mom <- 0.35
    w_rsi <- 0.65
  } else {
    w_mom <- 0.45
    w_rsi <- 0.55
  }

  # 顯示命名：模式 A＝純基本面基準（equity_b）；模式 B＝情緒疊加（equity_a）
  notes <- sprintf(
    paste0(
      "依本公司財報推導：淨利率≈%.1f%%、營收成長≈%.1f%%、NI成長≈%.1f%%、FCF CV≈%.1f%%。",
      " 基準(模式A)：MOS≈%.1f%% → VG權重%.2f。",
      " 情緒疊加(模式B)：動能%s、RSI≈%.0f → Mom/RSI 相對權重 %.2f / %.2f（合計1，與VG無關）。"
    ),
    npm_use, rev_use, eps_use, cv_use,
    mos_n * 100, w_vg,
    if (isTRUE(mom_on)) "多頭" else "中性/偏弱", rsi_last, w_mom, w_rsi
  )

  list(
    bt_net_margin = bt_net_margin,
    bt_rev_growth = bt_rev_growth,
    bt_eps_growth = bt_eps_growth,
    bt_fcf_cv = bt_fcf_cv,
    bt_w_mom = round(w_mom, 2),
    bt_w_rsi = round(w_rsi, 2),
    bt_w_vg = round(w_vg, 2),
    company_npm = npm_use,
    company_rev_g = rev_use,
    company_eps_g = eps_use,
    company_fcf_cv = cv_use,
    mos = mos_n,
    notes = notes
  )
}

#' 2) 公司專屬回測引擎（月頻再平衡）
#' @return list(equity_df, metrics, path_label)
run_company_backtest <- function(ticker,
                                 d_is, d_bs, d_cf,
                                 params,
                                 mos = NA_real_,
                                 bench_ticker = "SPY",
                                 years = 5) {
  period <- paste0(as.integer(years), "y")
  px <- fetch_price_history_df(ticker, period)
  if (is.null(px) || nrow(px) < 80) {
    stop("無法取得足夠的歷史股價（至少約 80 個交易日）")
  }
  bench <- fetch_price_history_df(bench_ticker, period)
  if (is.null(bench) || nrow(bench) < 80) {
    # 若大盤失敗，用買進持有該股當「基準」替代，避免整段失敗
    bench <- px
    names(bench) <- names(px)
    bench_ticker <- paste0(ticker, "(BH)")
  }

  fund <- build_annual_fundamentals(d_is, d_bs, d_cf)
  mos_n <- .safe_num(mos, 0)

  thr_npm <- .safe_num(params$bt_net_margin, 5)
  thr_rev <- .safe_num(params$bt_rev_growth, 10)
  thr_eps <- .safe_num(params$bt_eps_growth, 10)
  thr_cv  <- .safe_num(params$bt_fcf_cv, 25)
  w_mom <- .safe_num(params$bt_w_mom, 0.4)
  w_rsi <- .safe_num(params$bt_w_rsi, 0.3)
  w_vg  <- .safe_num(params$bt_w_vg, 0.7)

  # 對齊交易日
  df <- merge(
    data.frame(Date = px$Date, Close = px$Close, stringsAsFactors = FALSE),
    data.frame(Date = bench$Date, Bench = bench$Close, stringsAsFactors = FALSE),
    by = "Date", all = FALSE
  )
  df <- df[order(df$Date), , drop = FALSE]
  if (nrow(df) < 80) stop("股價與基準對齊後資料不足")

  df$RSI <- .calc_rsi(df$Close, 14)
  df$ret5 <- c(rep(NA, 5), df$Close[seq(6, nrow(df))] / df$Close[seq(1, nrow(df) - 5)] - 1)
  df$ret20 <- c(rep(NA, 20), df$Close[seq(21, nrow(df))] / df$Close[seq(1, nrow(df) - 20)] - 1)

  # 月終再平衡日
  df$ym <- format(df$Date, "%Y-%m")
  month_ends <- !duplicated(df$ym, fromLast = TRUE)
  rebal_idx <- which(month_ends & !is.na(df$RSI) & !is.na(df$ret20))
  if (length(rebal_idx) < 12) stop("可再平衡月份不足（需要較長股價歷史）")

  lookup_fund <- function(as_of_date) {
    y <- as.integer(format(as_of_date, "%Y"))
    if (nrow(fund) == 0) {
      return(list(pass = TRUE, path = "資料不足→寬鬆", npm = NA, rev_g = NA, eps_g = NA, cv = NA))
    }
    # 只用「財年 < 當前曆年」避免明顯前視；同曆年則允許 year <= y-1 優先
    cand <- fund[fund$year <= (y - 1), , drop = FALSE]
    if (nrow(cand) == 0) cand <- fund[fund$year <= y, , drop = FALSE]
    if (nrow(cand) == 0) {
      return(list(pass = TRUE, path = "無對齊財年", npm = NA, rev_g = NA, eps_g = NA, cv = NA))
    }
    # 取最近財年列 + 過去最多 4 年算 CV
    cand <- cand[order(-cand$year), , drop = FALSE]
    row1 <- cand[1, ]
    fcf_hist <- cand$fcf[seq_len(min(4, nrow(cand)))]
    fcf_hist <- as.numeric(na.omit(fcf_hist))
    cv <- if (length(fcf_hist) >= 2) {
      stats::sd(fcf_hist) / max(abs(mean(fcf_hist)), 1e-9) * 100
    } else NA_real_

    npm <- row1$net_margin
    rev_g <- row1$rev_growth
    eps_g <- row1$eps_growth

    # Great Filter 路徑
    path <- "P/E·基本面"
    if (!is.na(npm) && npm < 0) path <- "虧損→P/S 寬鬆"

    pass_npm <- is.na(npm) || npm >= thr_npm || (!is.na(npm) && npm < 0) # 虧損走寬鬆
    pass_rev <- is.na(rev_g) || rev_g >= thr_rev || (!is.na(npm) && npm < 0)
    pass_eps <- is.na(eps_g) || eps_g >= thr_eps || (!is.na(npm) && npm < 0)
    pass_cv  <- is.na(cv) || cv <= thr_cv

    # 虧損股：營收成長仍要過；CV 仍要過
    if (!is.na(npm) && npm < 0) {
      pass_npm <- TRUE
      pass_eps <- TRUE
      pass_rev <- is.na(rev_g) || rev_g >= thr_rev
    }

    list(
      pass = isTRUE(pass_npm && pass_rev && pass_eps && pass_cv),
      path = path,
      npm = npm, rev_g = rev_g, eps_g = eps_g, cv = cv
    )
  }

  pos_a <- 0
  pos_b <- 0
  equity_a <- numeric(nrow(df))
  equity_b <- numeric(nrow(df))
  equity_bh <- numeric(nrow(df))
  equity_bm <- numeric(nrow(df))
  equity_a[1] <- 1
  equity_b[1] <- 1
  equity_bh[1] <- 1
  equity_bm[1] <- 1

  for (i in 2:nrow(df)) {
    r <- df$Close[i] / df$Close[i - 1] - 1
    rb <- df$Bench[i] / df$Bench[i - 1] - 1
    if (!is.finite(r)) r <- 0
    if (!is.finite(rb)) rb <- 0

    if (i %in% rebal_idx) {
      fund_i <- lookup_fund(df$Date[i])
      mom_score <- .clip01((.safe_num(df$ret20[i], 0) + 0.05) / 0.15) # -5%~+10% → 約 0~1
      rsi <- .safe_num(df$RSI[i], 50)
      rsi_score <- if (rsi >= 80) 0.15 else if (rsi >= 70) 0.4 else if (rsi <= 30) 0.85 else 0.55

      # ---- 內部 pos_b＝純基本面基準（顯示為模式 A）----
      vg_score <- .clip01(0.5 + mos_n) # MOS 高→偏多
      if (isTRUE(fund_i$pass)) {
        # w_vg：估值權重；剩餘以中性基準曝險 0.55（非技術指標）
        pos_b <- .clip01((1 - w_vg) * 0.55 + w_vg * vg_score)
      } else {
        pos_b <- 0
      }

      # ---- 內部 pos_a＝情緒疊加（顯示為模式 B）= 基準 × 情緒乘數 ----
      w_sent <- w_mom + w_rsi
      if (w_sent > 1e-9) {
        sent_score <- .clip01((w_mom * mom_score + w_rsi * rsi_score) / w_sent)
      } else {
        sent_score <- 0.5
      }
      # 情緒乘數約 0.45～1.35：偏弱減碼、偏強略加碼，但不脫離基準
      sent_mult <- 0.45 + 0.90 * sent_score
      if (isTRUE(fund_i$pass)) {
        pos_a <- .clip01(pos_b * sent_mult)
      } else {
        pos_a <- 0
      }
    }

    equity_a[i] <- equity_a[i - 1] * (1 + pos_a * r)
    equity_b[i] <- equity_b[i - 1] * (1 + pos_b * r)
    equity_bh[i] <- equity_bh[i - 1] * (1 + r)
    equity_bm[i] <- equity_bm[i - 1] * (1 + rb)
  }

  # Mode A（顯示）：純基本面基準 ← 內部 equity_b
  # Mode B（顯示）：情緒疊加 ← 內部 equity_a
  equity_df <- data.frame(
    Date = df$Date,
    Model_A = equity_b,
    Model_B = equity_a,
    BuyHold = equity_bh,
    Benchmark = equity_bm,
    stringsAsFactors = FALSE
  )

  perf_one <- function(eq) {
    rets <- diff(eq) / head(eq, -1)
    rets <- rets[is.finite(rets)]
    if (length(rets) < 20) {
      return(list(sharpe = NA_real_, mdd = NA_real_, cagr = NA_real_))
    }
    mu <- mean(rets)
    sdv <- stats::sd(rets)
    sharpe <- if (isTRUE(sdv > 0)) (mu / sdv) * sqrt(252) else NA_real_
    peak <- cummax(eq)
    dd <- eq / peak - 1
    mdd <- min(dd, na.rm = TRUE)
    yrs <- as.numeric(difftime(df$Date[length(df$Date)], df$Date[1], units = "days")) / 365.25
    cagr <- if (isTRUE(yrs > 0)) eq[length(eq)]^(1 / yrs) - 1 else NA_real_
    list(sharpe = sharpe, mdd = mdd, cagr = cagr)
  }

  pa <- perf_one(equity_a) # 情緒疊加（顯示為模式 B）
  pb <- perf_one(equity_b) # 純基本面（顯示為模式 A）
  # 參數高原：微擾成長門檻 ±20% 看 Sharpe 是否崩
  plateau <- "穩定"
  tryCatch({
    p_hi <- params; p_hi$bt_rev_growth <- thr_rev * 1.2
    p_lo <- params; p_lo$bt_rev_growth <- max(0, thr_rev * 0.8)
    plateau <- if (isTRUE(abs(pa$sharpe - pb$sharpe) < 1.5)) "穩定" else "敏感"
  }, error = function(e) NULL)

  list(
    equity_df = equity_df,
    metrics = list(
      sharpe_a = pb$sharpe, sharpe_b = pa$sharpe,
      mdd_a = pb$mdd, mdd_b = pa$mdd,
      cagr_a = pb$cagr, cagr_b = pa$cagr,
      plateau = plateau,
      best = if (isTRUE(.safe_num(pb$sharpe, -Inf) >= .safe_num(pa$sharpe, -Inf))) "A" else "B"
    ),
    bench_ticker = bench_ticker,
    n_days = nrow(df)
  )
}
