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

.parse_period_year <- function(col) {
  d <- suppressWarnings(as.Date(col, format = "%m/%d/%Y"))
  if (is.na(d)) d <- suppressWarnings(as.Date(col))
  if (is.na(d)) {
    y <- suppressWarnings(as.integer(sub(".*?(\\d{4}).*", "\\1", col)))
    return(y)
  }
  as.integer(format(d, "%Y"))
}

.pick_statement_val <- function(df, patterns, col) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
  for (pat in patterns) {
    idx <- grep(pat, df[[1]], ignore.case = TRUE)
    if (length(idx) == 0) next
    if (!(col %in% colnames(df))) return(NA_real_)
    return(parse_financial_number(df[idx[1], col])[1])
  }
  NA_real_
}

#' 在財報表中找「指定財年」欄位（同名優先，否則同年／最近較舊年）
.find_col_for_year <- function(df, year, prefer_cols = NULL) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) < 2 || is.na(year)) return(NA_character_)
  cols <- colnames(df)[-1]
  cols <- cols[!grepl("^ttm$", cols, ignore.case = TRUE)]
  if (length(cols) == 0) return(NA_character_)
  if (!is.null(prefer_cols)) {
    hit <- prefer_cols[prefer_cols %in% cols]
    if (length(hit) > 0) {
      ys <- vapply(hit, .parse_period_year, integer(1))
      exact <- hit[!is.na(ys) & ys == year]
      if (length(exact) > 0) return(exact[1])
    }
  }
  ys <- vapply(cols, .parse_period_year, integer(1))
  exact <- cols[!is.na(ys) & ys == year]
  if (length(exact) > 0) return(exact[1])
  prior <- cols[!is.na(ys) & ys <= year]
  if (length(prior) == 0) return(NA_character_)
  prior[which.max(ys[!is.na(ys) & ys <= year])]
}

#' 以「此刻 DCF 參數」+ 歷史單期 FCF／BS 估算每股策略估值
#' @param fcf0 歷史自由現金流（與財報同單位）
#' @param wacc,sgr,g_explicit 小數（非百分比）
estimate_hist_fair_value <- function(fcf0, cash, debt, shares,
                                     wacc, sgr, n_years = 5, g_explicit = NULL) {
  fcf0 <- .safe_num(fcf0, NA_real_)
  shares <- .safe_num(shares, NA_real_)
  wacc <- .safe_num(wacc, NA_real_)
  sgr <- .safe_num(sgr, NA_real_)
  n_years <- as.integer(.safe_num(n_years, 5))
  if (is.null(g_explicit) || !is.finite(.safe_num(g_explicit, NA_real_))) {
    g_explicit <- sgr
  } else {
    g_explicit <- .safe_num(g_explicit, sgr)
  }
  cash <- .safe_num(cash, 0)
  debt <- .safe_num(debt, 0)

  if (is.na(fcf0) || is.na(shares) || shares <= 1 || is.na(wacc) || wacc <= 0) {
    return(NA_real_)
  }
  if (n_years < 1L) n_years <- 5L
  if (is.na(sgr)) sgr <- max(0, wacc - 0.03)
  if (sgr >= wacc) sgr <- max(0, wacc - 0.005)
  if (!is.finite(g_explicit)) g_explicit <- sgr

  fcfs <- fcf0 * (1 + g_explicit)^seq_len(n_years)
  dfs <- cumprod(rep(1 + wacc, n_years))
  pv_fcf <- sum(fcfs / dfs)
  tv <- fcfs[n_years] * (1 + sgr) / (wacc - sgr)
  pv_tv <- tv / dfs[n_years]
  ev <- pv_fcf + pv_tv
  equity <- ev + cash - debt
  fv <- equity / shares
  if (!is.finite(fv) || fv <= 0) return(NA_real_)
  fv
}

#' 估值訊號：策略估值 vs 歷史市價
#' - 策略結果 < 歷史市價 → 策略低估
#' - 策略結果 > 歷史市價 → 價值高估
valuation_signal_label <- function(fv, price) {
  fv <- .safe_num(fv, NA_real_)
  price <- .safe_num(price, NA_real_)
  if (is.na(fv) || is.na(price) || price <= 0) return("資料不足")
  if (fv < price) return("策略低估")
  if (fv > price) return("價值高估")
  "合理"
}

#' 從財報欄位建立「財年 → 指標」表（排除 TTM；含估值所需 FCF／現金／負債／股數）
build_annual_fundamentals <- function(d_is, d_bs, d_cf) {
  empty <- data.frame(
    year = integer(0), net_margin = numeric(0), rev_growth = numeric(0),
    eps_growth = numeric(0), fcf = numeric(0),
    cash = numeric(0), debt = numeric(0), shares = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(d_is) || !is.data.frame(d_is) || ncol(d_is) < 2) return(empty)

  period_cols <- colnames(d_is)[-1]
  period_cols <- period_cols[!grepl("^ttm$", period_cols, ignore.case = TRUE)]
  if (length(period_cols) == 0) return(empty)

  years <- vapply(period_cols, .parse_period_year, integer(1))
  ok <- !is.na(years)
  period_cols <- period_cols[ok]
  years <- years[ok]
  if (length(years) == 0) return(empty)

  rev <- vapply(period_cols, function(c) .pick_statement_val(d_is, c("Total Revenue", "^Revenue$"), c), numeric(1))
  ni  <- vapply(period_cols, function(c) .pick_statement_val(d_is, NET_INCOME_PATTERNS, c), numeric(1))

  fcf <- vapply(seq_along(period_cols), function(i) {
    col <- .find_col_for_year(d_cf, years[i], prefer_cols = period_cols[i])
    if (is.na(col)) return(NA_real_)
    .pick_statement_val(d_cf, c("^Free Cash Flow$"), col)
  }, numeric(1))

  cash <- vapply(seq_along(period_cols), function(i) {
    col <- .find_col_for_year(d_bs, years[i], prefer_cols = period_cols[i])
    if (is.na(col)) return(NA_real_)
    v <- .pick_statement_val(
      d_bs,
      c("Cash.*Equivalents.*Investments", "Cash And Cash Equivalents", "^Total Cash$"),
      col
    )
    if (is.na(v)) 0 else v
  }, numeric(1))

  debt <- vapply(seq_along(period_cols), function(i) {
    col <- .find_col_for_year(d_bs, years[i], prefer_cols = period_cols[i])
    if (is.na(col)) return(NA_real_)
    v <- .pick_statement_val(d_bs, c("^Total Debt$"), col)
    if (!is.na(v)) return(v)
    st <- .pick_statement_val(d_bs, c("Current Debt", "Short Term Debt"), col)
    lt <- .pick_statement_val(d_bs, c("Long Term Debt"), col)
    sum(c(if (is.na(st)) 0 else st, if (is.na(lt)) 0 else lt), na.rm = TRUE)
  }, numeric(1))

  shares <- vapply(seq_along(period_cols), function(i) {
    col <- .find_col_for_year(d_bs, years[i], prefer_cols = period_cols[i])
    if (is.na(col)) return(NA_real_)
    .pick_statement_val(
      d_bs,
      c("Ordinary Shares Number", "Share Issued", "Total Shares Outstanding", "Basic Average Shares"),
      col
    )
  }, numeric(1))

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
    cash = cash,
    debt = debt,
    shares = shares,
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
#' @param dcf_params list(wacc, sgr, n_years, g_explicit) 皆為小數；用「此刻」模型參數驗證歷史
#' @return list(equity_df, metrics, valuation_df, ...)
run_company_backtest <- function(ticker,
                                 d_is, d_bs, d_cf,
                                 params,
                                 mos = NA_real_,
                                 dcf_params = NULL,
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
  mos_fallback <- .safe_num(mos, 0)

  # 此刻 DCF 參數（歷史財報 × 當前假設 → 策略估值）
  dcf_wacc <- .safe_num(dcf_params$wacc, NA_real_)
  dcf_sgr <- .safe_num(dcf_params$sgr, NA_real_)
  dcf_n <- as.integer(.safe_num(dcf_params$n_years, 5))
  dcf_g_exp <- .safe_num(dcf_params$g_explicit, dcf_sgr)
  use_hist_fv <- is.finite(dcf_wacc) && dcf_wacc > 0 && nrow(fund) > 0

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
    empty_row <- list(
      pass = TRUE, path = "資料不足→寬鬆",
      npm = NA, rev_g = NA, eps_g = NA, cv = NA,
      fcf = NA, cash = 0, debt = 0, shares = NA, fund_year = NA
    )
    if (nrow(fund) == 0) return(empty_row)
    # 只用「財年 < 當前曆年」避免明顯前視；同曆年則允許 year <= y-1 優先
    cand <- fund[fund$year <= (y - 1), , drop = FALSE]
    if (nrow(cand) == 0) cand <- fund[fund$year <= y, , drop = FALSE]
    if (nrow(cand) == 0) {
      empty_row$path <- "無對齊財年"
      return(empty_row)
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
      npm = npm, rev_g = rev_g, eps_g = eps_g, cv = cv,
      fcf = row1$fcf, cash = row1$cash, debt = row1$debt,
      shares = row1$shares, fund_year = row1$year
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

  val_rows <- list()

  for (i in 2:nrow(df)) {
    r <- df$Close[i] / df$Close[i - 1] - 1
    rb <- df$Bench[i] / df$Bench[i - 1] - 1
    if (!is.finite(r)) r <- 0
    if (!is.finite(rb)) rb <- 0

    if (i %in% rebal_idx) {
      fund_i <- lookup_fund(df$Date[i])
      price_i <- .safe_num(df$Close[i], NA_real_)
      mom_score <- .clip01((.safe_num(df$ret20[i], 0) + 0.05) / 0.15) # -5%~+10% → 約 0~1
      rsi <- .safe_num(df$RSI[i], 50)
      rsi_score <- if (rsi >= 80) 0.15 else if (rsi >= 70) 0.4 else if (rsi <= 30) 0.85 else 0.55

      # ---- 歷史財報 × 此刻參數 → 策略估值，再對照當時市價 ----
      fv_i <- NA_real_
      if (isTRUE(use_hist_fv)) {
        fv_i <- estimate_hist_fair_value(
          fcf0 = fund_i$fcf,
          cash = fund_i$cash,
          debt = fund_i$debt,
          shares = fund_i$shares,
          wacc = dcf_wacc,
          sgr = dcf_sgr,
          n_years = dcf_n,
          g_explicit = dcf_g_exp
        )
      }
      signal_i <- valuation_signal_label(fv_i, price_i)
      # MOS = (策略估值 − 歷史市價) / 策略估值
      # 策略低估 (fv < price, mos < 0) → 降低曝險；策略估值 > 市價 (mos > 0) → 提高曝險
      if (is.finite(fv_i) && is.finite(price_i) && fv_i > 0) {
        mos_i <- (fv_i - price_i) / fv_i
      } else {
        mos_i <- mos_fallback
      }
      vg_score <- .clip01(0.5 + mos_i)

      # ---- 內部 pos_b＝純基本面基準（顯示為模式 A）----
      if (isTRUE(fund_i$pass)) {
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
      sent_mult <- 0.45 + 0.90 * sent_score
      if (isTRUE(fund_i$pass)) {
        pos_a <- .clip01(pos_b * sent_mult)
      } else {
        pos_a <- 0
      }

      val_rows[[length(val_rows) + 1L]] <- data.frame(
        Date = df$Date[i],
        fund_year = fund_i$fund_year,
        hist_price = price_i,
        strategy_fv = fv_i,
        mos = mos_i,
        signal = signal_i,
        filter_pass = isTRUE(fund_i$pass),
        pos_fundamental = pos_b,
        stringsAsFactors = FALSE
      )
    }

    equity_a[i] <- equity_a[i - 1] * (1 + pos_a * r)
    equity_b[i] <- equity_b[i - 1] * (1 + pos_b * r)
    equity_bh[i] <- equity_bh[i - 1] * (1 + r)
    equity_bm[i] <- equity_bm[i - 1] * (1 + rb)
  }

  valuation_df <- if (length(val_rows) > 0) {
    do.call(rbind, val_rows)
  } else {
    data.frame(
      Date = as.Date(character()), fund_year = integer(),
      hist_price = numeric(), strategy_fv = numeric(), mos = numeric(),
      signal = character(), filter_pass = logical(), pos_fundamental = numeric(),
      stringsAsFactors = FALSE
    )
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
  plateau <- "穩定"
  tryCatch({
    plateau <- if (isTRUE(abs(pa$sharpe - pb$sharpe) < 1.5)) "穩定" else "敏感"
  }, error = function(e) NULL)

  sig <- valuation_df$signal
  n_sig <- sum(sig %in% c("策略低估", "價值高估", "合理"), na.rm = TRUE)
  pct_under <- if (n_sig > 0) sum(sig == "策略低估", na.rm = TRUE) / n_sig else NA_real_
  pct_over <- if (n_sig > 0) sum(sig == "價值高估", na.rm = TRUE) / n_sig else NA_real_
  mean_mos <- if (nrow(valuation_df) > 0) {
    mean(valuation_df$mos[is.finite(valuation_df$mos)], na.rm = TRUE)
  } else {
    NA_real_
  }
  last_signal <- if (nrow(valuation_df) > 0) tail(valuation_df$signal, 1) else "資料不足"

  list(
    equity_df = equity_df,
    valuation_df = valuation_df,
    metrics = list(
      sharpe_a = pb$sharpe, sharpe_b = pa$sharpe,
      mdd_a = pb$mdd, mdd_b = pa$mdd,
      cagr_a = pb$cagr, cagr_b = pa$cagr,
      plateau = plateau,
      best = if (isTRUE(.safe_num(pb$sharpe, -Inf) >= .safe_num(pa$sharpe, -Inf))) "A" else "B",
      pct_strategy_under = pct_under,
      pct_value_over = pct_over,
      mean_hist_mos = mean_mos,
      last_signal = last_signal,
      use_hist_fv = use_hist_fv
    ),
    bench_ticker = bench_ticker,
    n_days = nrow(df),
    dcf_params_used = list(
      wacc = dcf_wacc, sgr = dcf_sgr, n_years = dcf_n, g_explicit = dcf_g_exp
    )
  )
}
