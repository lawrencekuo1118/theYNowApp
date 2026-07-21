# ==========================================
# backtest_module.R -- The YNow App V12.0
# --------------------------------------------------------------
# Dynamic session-only PIT (point-in-time) backtest engine.
# - No warehouse: every rebalance date reconstructs fair values
#   from annual financials whose fiscal year <= calendar_year - 1.
#   Growth / n / P/B come from CURRENT session; Ke/WACC use Rolling Beta.
# - Multi-model composite fair value: DCF + DDM + RI + P/B, then
#   mean of available models.
# - Model_A: normalized PIT fair-value INDEX (參數高原／內部用；不是淨值圖曲線).
# - Trade_A (模式「純基本面價值」策略淨值): Exp_A × 日報酬；Exp_A 來自 MOS＋Great Filter.
# - Trade_B / Model_B (模式「情緒波動價值」策略淨值): Exp_B × 日報酬；
#   Exp_B = clip(Exp_A × sentiment, 0.75×A … 1.25×A)；Exp_A=0 → Exp_B=0.
# 淨值圖只畫 Trade_A／Trade_B vs BuyHold／Benchmark；合理價看 HFV Timeline.
# ==========================================

# ---------- small helpers ----------

.clip01 <- function(x, lo = 0, hi = 1) {
  x <- as.numeric(x)
  if (length(x) != 1 || is.na(x) || !is.finite(x)) return((lo + hi) / 2)
  max(lo, min(hi, x))
}

.safe_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (length(x) < 1 || is.na(x) || !is.finite(x)) default else x
}

.coalesce <- function(a, b) if (is.null(a)) b else a

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) < 1 || (length(x) == 1 && is.na(x))) y else x
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
    v <- tryCatch(parse_financial_number(df[idx[1], col])[1],
                  error = function(e) suppressWarnings(as.numeric(df[idx[1], col])[1]))
    return(v)
  }
  NA_real_
}

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

# Setup fallbacks (in case setup.R helpers are unavailable)
.NET_INCOME_PATTERNS <- c(
  "Net Income From Continuing (And|&) Discontinued Operation",
  "Net Income Common Stockholders",
  "^Net Income$"
)
.EQUITY_PATTERNS <- c(
  "Common Stock Equity",
  "Stockholders Equity",
  "Total Equity Gross Minority Interest"
)
.SHARE_PATTERNS <- c(
  "Ordinary Shares Number",
  "Total Shares Outstanding",
  "Share Issued",
  "Basic Average Shares"
)
.DIVIDEND_PATTERNS <- c(
  "Cash Dividends Paid",
  "^Dividends Paid$",
  "Common Stock Dividend Paid"
)

.get_pattern <- function(name, fallback) {
  if (exists(name, mode = "character", envir = .GlobalEnv, inherits = TRUE)) {
    v <- get(name, envir = .GlobalEnv, inherits = TRUE)
    if (is.character(v) && length(v) > 0) return(v)
  }
  fallback
}

# ---------- multi-model fair-value helpers ----------

#' Historical / PIT DCF (Gordon multi-year FCFF).
#' Uses fcf0 and current session WACC / SGR / n_years / g_explicit.
estimate_hist_dcf <- function(fcf0, cash, debt, shares,
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
  if (is.na(fcf0) || is.na(shares) || shares <= 1 || is.na(wacc) || wacc <= 0) return(NA_real_)
  if (n_years < 1L) n_years <- 5L
  if (is.na(sgr)) sgr <- max(0, wacc - 0.03)
  if (sgr >= wacc) sgr <- max(0, wacc - 0.005)
  if (!is.finite(g_explicit)) g_explicit <- sgr

  fcfs <- fcf0 * (1 + g_explicit) ^ seq_len(n_years)
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

# Back-compat alias (kept from v11 API surface).
estimate_hist_fair_value <- function(fcf0, cash, debt, shares,
                                     wacc, sgr, n_years = 5, g_explicit = NULL) {
  estimate_hist_dcf(fcf0, cash, debt, shares, wacc, sgr, n_years, g_explicit)
}

#' Gordon DDM: P0 = D0 * (1 + g) / (Ke - g).
#' d0 is DIVIDEND-PER-SHARE (already normalized by shares).
estimate_hist_ddm <- function(d0, ke, g) {
  d0 <- .safe_num(d0, NA_real_)
  ke <- .safe_num(ke, NA_real_)
  g  <- .safe_num(g,  NA_real_)
  if (!is.finite(d0) || d0 <= 0 || !is.finite(ke) || !is.finite(g)) return(NA_real_)
  if (ke <= g) return(NA_real_)
  p0 <- d0 * (1 + g) / (ke - g)
  if (!is.finite(p0) || p0 <= 0) return(NA_real_)
  p0
}

#' Simplified Residual Income model (per share).
#' b0 = BVPS; roe & ke small decimals; g terminal growth of RI;
#' n = explicit forecast horizon; payout implied from dividends (fallback 0.5 retention).
estimate_hist_ri <- function(b0, roe, ke, g, n = 5, payout = NA_real_) {
  b0 <- .safe_num(b0, NA_real_)
  roe <- .safe_num(roe, NA_real_)
  ke <- .safe_num(ke, NA_real_)
  g  <- .safe_num(g,  NA_real_)
  n <- as.integer(.safe_num(n, 5))
  payout <- .safe_num(payout, NA_real_)
  if (!is.finite(b0) || b0 <= 0 || !is.finite(roe) || !is.finite(ke) || ke <= 0) return(NA_real_)
  if (n < 1L) n <- 5L
  if (!is.finite(payout) || payout < 0) payout <- 0.5
  payout <- min(max(payout, 0), 1)
  retention <- 1 - payout

  B <- numeric(n + 1)
  B[1] <- b0
  RI <- numeric(n)
  book_g <- roe * retention
  for (t in seq_len(n)) {
    RI[t]  <- (roe - ke) * B[t]
    B[t + 1] <- B[t] * (1 + book_g)
  }
  dfs <- cumprod(rep(1 + ke, n))
  pv_ri <- sum(RI / dfs)

  pv_tv <- 0
  if (is.finite(g) && ke > g) {
    ri_next <- (roe - ke) * B[n + 1]
    tv <- ri_next / (ke - g)
    pv_tv <- tv / dfs[n]
  }
  v <- b0 + pv_ri + pv_tv
  if (!is.finite(v) || v <= 0) return(NA_real_)
  v
}

#' Book-value multiple: BVPS * P/B target.
estimate_hist_pb <- function(bvps, pb_mid) {
  bvps <- .safe_num(bvps, NA_real_)
  pb_mid <- .safe_num(pb_mid, NA_real_)
  if (!is.finite(bvps) || bvps <= 0 || !is.finite(pb_mid) || pb_mid <= 0) return(NA_real_)
  v <- bvps * pb_mid
  if (!is.finite(v) || v <= 0) return(NA_real_)
  v
}

#' Signal label. Convention (per v11 hist-fv PR):
#'   fair_value < price -> "策略低估" (strategy says price is CHEAP,
#'                                       i.e. FV under the market -> lower conviction)
#'   fair_value > price -> "價值高估" (FV over price -> undervalued)
#' NOTE: This is the labelling convention shipped in v11 and the app UI
#'   relies on it. Do NOT flip.
valuation_signal_label <- function(fv, price) {
  fv <- .safe_num(fv, NA_real_)
  price <- .safe_num(price, NA_real_)
  if (is.na(fv) || is.na(price) || price <= 0) return("資料不足")
  if (fv < price) return("策略低估")
  if (fv > price) return("價值高估")
  "合理"
}

#' Market under/over metrics from PIT rebalance rows.
#' Under = actual price below model fair value (price < FV).
.compute_market_pricing_metrics <- function(valuation_df) {
  empty <- list(
    pct_market_under = NA_real_,
    pct_market_over = NA_real_,
    market_pricing_bias = "資料不足",
    market_pricing_dominant_pct = NA_real_,
    pct_strategy_under = NA_real_,
    pct_value_over = NA_real_,
    mean_hist_mos = NA_real_,
    last_signal = "資料不足"
  )
  if (is.null(valuation_df) || !is.data.frame(valuation_df) || nrow(valuation_df) == 0) {
    return(empty)
  }
  price <- valuation_df$hist_price
  fv <- valuation_df$fair_value
  valid <- is.finite(price) & is.finite(fv) & price > 0 & fv > 0
  n_valid <- sum(valid)
  if (n_valid == 0) return(empty)

  p <- price[valid]
  f <- fv[valid]
  pct_under <- sum(p < f) / n_valid
  pct_over <- sum(p > f) / n_valid
  bias <- if (pct_under > pct_over + 0.05) {
    "低估為主"
  } else if (pct_over > pct_under + 0.05) {
    "高估為主"
  } else {
    "大致均衡"
  }
  dom_pct <- if (identical(bias, "低估為主")) {
    pct_under
  } else if (identical(bias, "高估為主")) {
    pct_over
  } else {
    NA_real_
  }
  mean_mos <- mean(valuation_df$mos[is.finite(valuation_df$mos)], na.rm = TRUE)
  last_signal <- if ("signal" %in% names(valuation_df)) tail(valuation_df$signal, 1) else "資料不足"

  list(
    pct_market_under = pct_under,
    pct_market_over = pct_over,
    market_pricing_bias = bias,
    market_pricing_dominant_pct = dom_pct,
    pct_strategy_under = pct_under,
    pct_value_over = pct_over,
    mean_hist_mos = mean_mos,
    last_signal = last_signal
  )
}

#' Point-in-time fair-value reconstruction for a single fundamentals row.
#' @param fund_row list-like with fcf, cash, debt, shares, ni, equity_book,
#'   dividends_paid.
#' @param price historical closing price at rebalance date.
#' @param model_params list(wacc, ke, sgr, g_explicit, n_years, pb_mid, ddm_g).
#' @return list(fv_dcf, fv_ddm, fv_ri, fv_pb, fair_value, mos, signal,
#'   valuation_score, bvps, roe, dps, payout).
reconstruct_fair_value_pit <- function(fund_row, price, model_params) {
  price <- .safe_num(price, NA_real_)
  wacc <- .safe_num(model_params$wacc, NA_real_)
  ke   <- .safe_num(model_params$ke, wacc)
  sgr  <- .safe_num(model_params$sgr, NA_real_)
  n_yr <- as.integer(.safe_num(model_params$n_years, 5))
  g_ex <- .safe_num(model_params$g_explicit, sgr)
  pb_mid <- .safe_num(model_params$pb_mid, NA_real_)
  ddm_g <- .safe_num(model_params$ddm_g, sgr)

  shares <- .safe_num(fund_row$shares, NA_real_)
  fcf    <- .safe_num(fund_row$fcf, NA_real_)
  cash   <- .safe_num(fund_row$cash, 0)
  debt   <- .safe_num(fund_row$debt, 0)
  ni     <- .safe_num(fund_row$ni, NA_real_)
  eqbook <- .safe_num(fund_row$equity_book, NA_real_)
  divp   <- .safe_num(fund_row$dividends_paid, NA_real_)

  bvps <- if (is.finite(eqbook) && is.finite(shares) && shares > 1) eqbook / shares else NA_real_
  roe  <- if (is.finite(ni) && is.finite(eqbook) && eqbook > 0) ni / eqbook else NA_real_
  dps  <- if (is.finite(divp) && is.finite(shares) && shares > 1) abs(divp) / shares else NA_real_
  payout <- if (is.finite(dps) && is.finite(ni) && ni > 0 && is.finite(shares) && shares > 1) {
    min(max(abs(divp) / ni, 0), 1)
  } else NA_real_

  fv_dcf <- estimate_hist_dcf(fcf, cash, debt, shares, wacc, sgr, n_yr, g_ex)
  fv_ddm <- if (is.finite(dps) && dps > 0) estimate_hist_ddm(dps, ke, ddm_g) else NA_real_
  fv_ri  <- estimate_hist_ri(bvps, roe, ke, g_ex, n = n_yr, payout = payout)
  fv_pb  <- estimate_hist_pb(bvps, pb_mid)

  # Mode A uses the user-selected valuation model (not always the blend).
  fv_model <- tolower(as.character(model_params$fv_model %||% "composite")[1])
  pick_one <- function(x) if (is.finite(x) && x > 0) x else NA_real_
  fair_value <- switch(
    fv_model,
    "dcf" = pick_one(fv_dcf),
    "ddm" = pick_one(fv_ddm),
    "ri"  = pick_one(fv_ri),
    "pb"  = pick_one(fv_pb),
    {
      cand <- c(fv_dcf, fv_ddm, fv_ri, fv_pb)
      cand <- cand[is.finite(cand) & cand > 0]
      if (length(cand) > 0) mean(cand) else NA_real_
    }
  )
  # Fallback if the selected model is unavailable at this PIT date.
  if (!is.finite(fair_value) || fair_value <= 0) {
    cand <- c(fv_dcf, fv_ddm, fv_ri, fv_pb)
    cand <- cand[is.finite(cand) & cand > 0]
    fair_value <- if (length(cand) > 0) mean(cand) else NA_real_
  }

  mos <- if (is.finite(fair_value) && fair_value > 0 && is.finite(price) && price > 0) {
    (fair_value - price) / fair_value
  } else NA_real_
  signal <- valuation_signal_label(fair_value, price)
  # Map MOS in [-0.2, +0.5] to [0, 100], clipped.
  score <- if (is.finite(mos)) .clip01((mos + 0.2) / 0.7, 0, 1) * 100 else NA_real_

  list(
    fv_dcf = fv_dcf, fv_ddm = fv_ddm, fv_ri = fv_ri, fv_pb = fv_pb,
    fair_value = fair_value, mos = mos, signal = signal,
    valuation_score = score,
    bvps = bvps, roe = roe, dps = dps, payout = payout
  )
}

# ---------- expanded annual fundamentals table ----------

build_annual_fundamentals <- function(d_is, d_bs, d_cf) {
  empty <- data.frame(
    year = integer(0),
    net_margin = numeric(0), rev_growth = numeric(0), eps_growth = numeric(0),
    fcf = numeric(0), cash = numeric(0), debt = numeric(0), shares = numeric(0),
    dividends_paid = numeric(0), equity_book = numeric(0), ni = numeric(0),
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

  ni_pat <- .get_pattern("NET_INCOME_PATTERNS", .NET_INCOME_PATTERNS)
  eq_pat <- .get_pattern("EQUITY_PATTERNS",     .EQUITY_PATTERNS)
  sh_pat <- .get_pattern("SHARE_PATTERNS",      .SHARE_PATTERNS)

  rev <- vapply(period_cols, function(c) .pick_statement_val(d_is, c("Total Revenue", "^Revenue$"), c), numeric(1))
  ni  <- vapply(period_cols, function(c) .pick_statement_val(d_is, ni_pat, c), numeric(1))

  fcf <- vapply(seq_along(period_cols), function(i) {
    col <- .find_col_for_year(d_cf, years[i], prefer_cols = period_cols[i])
    if (is.na(col)) return(NA_real_)
    .pick_statement_val(d_cf, c("^Free Cash Flow$"), col)
  }, numeric(1))

  divp <- vapply(seq_along(period_cols), function(i) {
    col <- .find_col_for_year(d_cf, years[i], prefer_cols = period_cols[i])
    if (is.na(col)) return(NA_real_)
    .pick_statement_val(d_cf, .DIVIDEND_PATTERNS, col)
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
    .pick_statement_val(d_bs, sh_pat, col)
  }, numeric(1))

  eqbook <- vapply(seq_along(period_cols), function(i) {
    col <- .find_col_for_year(d_bs, years[i], prefer_cols = period_cols[i])
    if (is.na(col)) return(NA_real_)
    .pick_statement_val(d_bs, eq_pat, col)
  }, numeric(1))

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
    dividends_paid = divp,
    equity_book = eqbook,
    ni = ni,
    stringsAsFactors = FALSE
  )
}

# ---------- price fetching ----------

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
    message("yfinance history failed (", ticker, "): ", e$message)
    NULL
  })

  if (!is.null(df)) {
    df <- df[is.finite(df$Close) & !is.na(df$Date), , drop = FALSE]
    df <- df[order(df$Date), , drop = FALSE]
    if (nrow(df) >= 30) return(df)
  }

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

#' Rolling beta as-of a date from daily prices (Yahoo-style 5Y monthly when possible).
#' Uses month-end returns ending at/before as_of; falls back to weekly if months scarce.
#' @return numeric beta or NA_real_
estimate_rolling_beta <- function(stock_close, bench_close, dates, as_of,
                                  lookback_months = 60L, min_obs = 24L) {
  dates <- as.Date(dates)
  as_of <- as.Date(as_of)[1]
  stock_close <- as.numeric(stock_close)
  bench_close <- as.numeric(bench_close)
  ok <- is.finite(stock_close) & is.finite(bench_close) & !is.na(dates) & dates <= as_of
  if (sum(ok, na.rm = TRUE) < 40L) return(NA_real_)
  d <- data.frame(Date = dates[ok], S = stock_close[ok], M = bench_close[ok])
  d <- d[order(d$Date), , drop = FALSE]

  .beta_from_prices <- function(px, min_n) {
    if (nrow(px) < min_n + 1L) return(NA_real_)
    rs <- diff(px$S) / head(px$S, -1)
    rm <- diff(px$M) / head(px$M, -1)
    fine <- is.finite(rs) & is.finite(rm)
    rs <- rs[fine]; rm <- rm[fine]
    if (length(rs) < min_n) return(NA_real_)
    v <- stats::var(rm)
    if (!is.finite(v) || v <= 1e-12) return(NA_real_)
    b <- stats::cov(rs, rm) / v
    if (!is.finite(b)) return(NA_real_)
    max(min(b, 3.5), -0.5)
  }

  # Prefer month-end series (aligns with Yahoo "5Y Monthly" beta).
  ym <- format(d$Date, "%Y-%m")
  mth <- d[!duplicated(ym, fromLast = TRUE), , drop = FALSE]
  mth <- tail(mth, as.integer(lookback_months) + 1L)
  beta <- .beta_from_prices(mth, min_obs)
  if (is.finite(beta)) return(beta)

  # Weekly fallback when history is short.
  yw <- format(d$Date, "%Y-%W")
  wk <- d[!duplicated(yw, fromLast = TRUE), , drop = FALSE]
  wk <- tail(wk, max(as.integer(lookback_months) * 4L, 52L) + 1L)
  .beta_from_prices(wk, max(min_obs, 36L))
}

#' Build point-in-time Ke/WACC from rolling beta + session CAPM structure.
#' Falls back to session ke/wacc when beta cannot be estimated.
pit_discount_params <- function(model_params, stock_close, bench_close, dates, as_of) {
  mp <- model_params
  beta_i <- estimate_rolling_beta(
    stock_close, bench_close, dates, as_of,
    lookback_months = as.integer(.safe_num(model_params$beta_lookback_months, 60)),
    min_obs = as.integer(.safe_num(model_params$beta_min_months, 24))
  )
  if (!is.finite(beta_i)) {
    beta_i <- .safe_num(model_params$beta_fallback, NA_real_)
  }
  rf <- .safe_num(model_params$rf, NA_real_)
  rm <- .safe_num(model_params$rm, NA_real_)
  ke0 <- .safe_num(model_params$ke, NA_real_)
  wacc0 <- .safe_num(model_params$wacc, NA_real_)

  ke_i <- ke0
  wacc_i <- wacc0
  if (is.finite(beta_i) && is.finite(rf) && is.finite(rm)) {
    ke_try <- rf + beta_i * (rm - rf)
    if (is.finite(ke_try) && ke_try > 0.01) {
      ke_i <- ke_try
      we <- .safe_num(model_params$we, NA_real_)
      wd <- .safe_num(model_params$wd, NA_real_)
      rd <- .safe_num(model_params$rd, 0.05)
      tax <- .safe_num(model_params$tax, 0.21)
      if (is.finite(we) && is.finite(wd) && (we + wd) > 0) {
        wacc_try <- we * ke_i + wd * rd * (1 - tax)
        if (is.finite(wacc_try) && wacc_try > 0.01) wacc_i <- wacc_try
      } else if (is.finite(ke0) && ke0 > 0 && is.finite(wacc0) && wacc0 > 0) {
        wacc_i <- wacc0 * (ke_i / ke0)
      }
    }
  }
  if (!is.finite(ke_i) || ke_i <= 0) ke_i <- ke0
  if (!is.finite(wacc_i) || wacc_i <= 0) wacc_i <- wacc0
  # Keep terminal g < WACC
  sgr <- .safe_num(mp$sgr, 0.025)
  if (is.finite(wacc_i) && is.finite(sgr) && sgr >= wacc_i) {
    mp$sgr <- max(0, wacc_i - 0.005)
  }
  mp$ke <- ke_i
  mp$wacc <- wacc_i
  list(model_params = mp, beta = beta_i, ke = ke_i, wacc = wacc_i)
}

# ---------- parameter derivation ----------

#' Derive company-specific backtest thresholds & weights.
#' v12 notes:
#'   - Great Filter thresholds still driven by company's own history.
#'   - bt_w_vg now scales how STRONGLY the MOS hysteresis map is
#'     applied (blend with neutral 0.40 exposure).
#'   - Sentiment weights only feed Strategy B multiplier scaling.
derive_bt_params <- function(d_is, d_bs, d_cf,
                             hist_df = NULL,
                             mos = NA_real_,
                             industry_choice = NULL) {
  npm <- {
    if (exists("select_clean_metric_row_any", mode = "function")) {
      net <- get_avg(select_clean_metric_row_any(d_is, .get_pattern("NET_INCOME_PATTERNS", .NET_INCOME_PATTERNS), include_ttm = FALSE))
    } else {
      net <- NA_real_
    }
    rev <- if (exists("select_clean_metric_row", mode = "function")) {
      get_avg(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    } else NA_real_
    if (!is.na(net) && !is.na(rev) && rev != 0) net / rev * 100 else NA_real_
  }
  rev_g <- if (exists("select_clean_metric_row", mode = "function"))
    get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE)) else NA_real_
  eps_g <- if (exists("select_clean_metric_row_any", mode = "function"))
    get_avg_growth(select_clean_metric_row_any(d_is, .get_pattern("NET_INCOME_PATTERNS", .NET_INCOME_PATTERNS), include_ttm = FALSE)) else NA_real_

  fcf_row <- if (exists("select_clean_metric_row", mode = "function"))
    select_clean_metric_row(d_cf, "^Free Cash Flow$", include_ttm = FALSE) else NA
  fcf_cv <- NA_real_
  if (length(fcf_row) >= 2) {
    x <- as.numeric(na.omit(fcf_row))
    if (length(x) >= 2) {
      m <- mean(x)
      fcf_cv <- stats::sd(x) / max(abs(m), 1e-9) * 100
    }
  }

  ind_rev <- NA_real_
  if (!is.null(industry_choice) && exists("industry_standards")) {
    ind <- industry_standards[[industry_choice]]
    if (!is.null(ind$rev_growth)) ind_rev <- mean(as.numeric(ind$rev_growth), na.rm = TRUE)
  }

  npm_use <- .safe_num(npm, 5)
  rev_use <- .safe_num(rev_g, .safe_num(ind_rev, 10))
  eps_use <- .safe_num(eps_g, max(rev_use * 0.8, 5))
  cv_use  <- .safe_num(fcf_cv, 20)

  bt_net_margin <- round(max(0, min(25, npm_use * 0.5)), 1)
  bt_rev_growth <- round(max(0, min(40, rev_use * 0.5)), 1)
  bt_eps_growth <- round(max(0, min(40, eps_use * 0.5)), 1)
  bt_fcf_cv     <- round(max(8, min(80, cv_use * 1.25)), 1)

  mos_n <- .safe_num(mos, 0)
  # bt_w_vg: how strongly MOS hysteresis drives A. 0 -> flat 0.40,
  # 1 -> pure hysteresis map. Default anchored around 0.65-0.75.
  w_vg <- .clip01(0.55 + 0.4 * mos_n, 0.3, 0.9)

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

  if (isTRUE(mom_on)) {
    w_mom <- 0.60; w_rsi <- 0.40
  } else if (isTRUE(rsi_last < 35)) {
    w_mom <- 0.35; w_rsi <- 0.65
  } else {
    w_mom <- 0.45; w_rsi <- 0.55
  }

  notes <- sprintf(
    paste0(
      "v12 季頻 PIT 多模型：依本公司財報推導 淨利率≈%.1f%%、營收成長≈%.1f%%、NI成長≈%.1f%%、FCF CV≈%.1f%%。",
      " 純基本面價值：MOS≈%.1f%% → w_vg=%.2f（越大越依 MOS 分級；持股上限見「最大持股」滑桿，預設 90%%）。",
      " 情緒波動價值：動能%s、RSI≈%.0f → Mom/RSI 相對權重 %.2f / %.2f（僅微調基準權重，範圍 0.75~1.25×）。"
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

# ---------- MOS hysteresis & sentiment mapping ----------

#' MOS -> Strategy A target exposure (hysteresis map).
#' @param max_exp ceiling for deep-undervalued bucket (default 0.90; set 1 to fit BH)
mos_hysteresis_target <- function(mos, w_vg = 0.7, max_exp = 0.90) {
  max_exp <- .clip01(.safe_num(max_exp, 0.90), 0.5, 1)
  if (!is.finite(mos)) return(min(0.40, max_exp))
  # Scale legacy map (old max 0.90) to user max_exp.
  base <- if (mos >= 0.30) max_exp
          else if (mos >= 0.10) max_exp * (0.65 / 0.90)
          else if (mos >= 0.00) max_exp * (0.40 / 0.90)
          else if (mos >= -0.10) max_exp * (0.15 / 0.90)
          else 0.00
  w <- .clip01(w_vg, 0, 1)
  flat <- min(0.40, max_exp)
  target <- (1 - w) * flat + w * base
  .clip01(target, 0, max_exp)
}

#' Sentiment multiplier in [0.75, 1.25] from mom/RSI features.
sentiment_multiplier <- function(mom_score, rsi_score, w_mom = 0.5, w_rsi = 0.5) {
  mom_score <- .clip01(.safe_num(mom_score, 0.5), 0, 1)
  rsi_score <- .clip01(.safe_num(rsi_score, 0.5), 0, 1)
  w_sum <- .safe_num(w_mom, 0.5) + .safe_num(w_rsi, 0.5)
  if (!is.finite(w_sum) || w_sum <= 1e-9) {
    sent <- 0.5 * mom_score + 0.5 * rsi_score
  } else {
    sent <- (.safe_num(w_mom, 0.5) * mom_score + .safe_num(w_rsi, 0.5) * rsi_score) / w_sum
  }
  0.75 + 0.5 * .clip01(sent, 0, 1)   # -> [0.75, 1.25]
}

# ---------- great filter (fundamental gate) ----------

.great_filter_pass <- function(fund_row, thr_npm, thr_rev, thr_eps, thr_cv, cv_hist) {
  if (is.null(fund_row)) return(FALSE)
  npm <- fund_row$net_margin
  rev_g <- fund_row$rev_growth
  eps_g <- fund_row$eps_growth
  cv <- cv_hist

  path <- "P/E·基本面"
  if (!is.na(npm) && npm < 0) path <- "虧損→P/S 寬鬆"

  pass_npm <- is.na(npm) || npm >= thr_npm || (!is.na(npm) && npm < 0)
  pass_rev <- is.na(rev_g) || rev_g >= thr_rev || (!is.na(npm) && npm < 0)
  pass_eps <- is.na(eps_g) || eps_g >= thr_eps || (!is.na(npm) && npm < 0)
  pass_cv  <- is.na(cv) || cv <= thr_cv
  if (!is.na(npm) && npm < 0) {
    pass_npm <- TRUE
    pass_eps <- TRUE
    pass_rev <- is.na(rev_g) || rev_g >= thr_rev
  }
  list(
    pass = isTRUE(pass_npm && pass_rev && pass_eps && pass_cv),
    path = path
  )
}

#' Company metrics for Dashboard「回測濾鏡」(percent units, same as derive_bt_params).
compute_dashboard_filter_metrics <- function(d_is, d_cf) {
  npm <- {
    net <- if (exists("select_clean_metric_row_any", mode = "function")) {
      get_avg(select_clean_metric_row_any(
        d_is, .get_pattern("NET_INCOME_PATTERNS", .NET_INCOME_PATTERNS), include_ttm = FALSE
      ))
    } else NA_real_
    rev <- if (exists("select_clean_metric_row", mode = "function")) {
      get_avg(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    } else NA_real_
    if (!is.na(net) && !is.na(rev) && rev != 0) net / rev * 100 else NA_real_
  }
  rev_g <- if (exists("select_clean_metric_row", mode = "function") &&
              exists("get_avg_growth", mode = "function")) {
    get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
  } else NA_real_
  eps_g <- if (exists("select_clean_metric_row_any", mode = "function") &&
              exists("get_avg_growth", mode = "function")) {
    get_avg_growth(select_clean_metric_row_any(
      d_is, .get_pattern("NET_INCOME_PATTERNS", .NET_INCOME_PATTERNS), include_ttm = FALSE
    ))
  } else NA_real_
  fcf_cv <- NA_real_
  if (exists("select_clean_metric_row", mode = "function")) {
    fcf_row <- select_clean_metric_row(d_cf, "^Free Cash Flow$", include_ttm = FALSE)
    if (length(fcf_row) >= 2) {
      x <- as.numeric(na.omit(fcf_row))
      if (length(x) >= 2) {
        m <- mean(x)
        fcf_cv <- stats::sd(x) / max(abs(m), 1e-9) * 100
      }
    }
  }
  list(net_margin = npm, rev_growth = rev_g, eps_growth = eps_g, fcf_cv = fcf_cv)
}

#' Evaluate current-company metrics vs holding thresholds (Dashboard 回測濾鏡).
#' Metrics / thresholds in percent units matching bt_* inputs.
evaluate_holding_filter <- function(metrics, thresholds) {
  npm <- .safe_num(metrics$net_margin, NA_real_)
  rev <- .safe_num(metrics$rev_growth, NA_real_)
  eps <- .safe_num(metrics$eps_growth, NA_real_)
  cv  <- .safe_num(metrics$fcf_cv, NA_real_)
  thr_npm <- .safe_num(thresholds$bt_net_margin, 5)
  thr_rev <- .safe_num(thresholds$bt_rev_growth, 10)
  thr_eps <- .safe_num(thresholds$bt_eps_growth, 10)
  thr_cv  <- .safe_num(thresholds$bt_fcf_cv, 25)
  fund_row <- list(net_margin = npm, rev_growth = rev, eps_growth = eps)
  gf <- .great_filter_pass(fund_row, thr_npm, thr_rev, thr_eps, thr_cv, cv)
  rows <- list(
    list(id = "npm", label = "淨利率", actual = npm, threshold = thr_npm, op = "≥",
         pass = is.na(npm) || npm >= thr_npm || (is.finite(npm) && npm < 0)),
    list(id = "rev", label = "營收成長", actual = rev, threshold = thr_rev, op = "≥",
         pass = is.na(rev) || rev >= thr_rev || (is.finite(npm) && npm < 0 && (is.na(rev) || rev >= thr_rev))),
    list(id = "eps", label = "EPS／NI 成長", actual = eps, threshold = thr_eps, op = "≥",
         pass = is.na(eps) || eps >= thr_eps || (is.finite(npm) && npm < 0)),
    list(id = "cv", label = "FCF CV", actual = cv, threshold = thr_cv, op = "≤",
         pass = is.na(cv) || cv <= thr_cv)
  )
  # Align with .great_filter_pass loss-loose branch exactly
  if (is.finite(npm) && npm < 0) {
    rows[[1]]$pass <- TRUE
    rows[[3]]$pass <- TRUE
    rows[[2]]$pass <- is.na(rev) || rev >= thr_rev
  }
  list(overall = isTRUE(gf$pass), rows = rows, path = gf$path)
}

# ---------- fundamentals lookup for a given trading date ----------

.lookup_fund_at <- function(fund, as_of_date) {
  y <- as.integer(format(as_of_date, "%Y"))
  empty_row <- list(
    fund_year = NA_integer_,
    net_margin = NA_real_, rev_growth = NA_real_, eps_growth = NA_real_,
    fcf = NA_real_, cash = 0, debt = 0, shares = NA_real_,
    dividends_paid = NA_real_, equity_book = NA_real_, ni = NA_real_,
    cv_fcf = NA_real_
  )
  if (is.null(fund) || nrow(fund) == 0) return(empty_row)
  cand <- fund[fund$year <= (y - 1), , drop = FALSE]
  if (nrow(cand) == 0) cand <- fund[fund$year <= y, , drop = FALSE]
  if (nrow(cand) == 0) return(empty_row)
  cand <- cand[order(-cand$year), , drop = FALSE]
  row1 <- cand[1, ]
  fcf_hist <- as.numeric(na.omit(cand$fcf[seq_len(min(4, nrow(cand)))]))
  cv <- if (length(fcf_hist) >= 2) {
    stats::sd(fcf_hist) / max(abs(mean(fcf_hist)), 1e-9) * 100
  } else NA_real_
  list(
    fund_year = row1$year,
    net_margin = row1$net_margin,
    rev_growth = row1$rev_growth,
    eps_growth = row1$eps_growth,
    fcf = row1$fcf, cash = row1$cash, debt = row1$debt, shares = row1$shares,
    dividends_paid = row1$dividends_paid,
    equity_book = row1$equity_book,
    ni = row1$ni,
    cv_fcf = cv
  )
}

# ---------- internal daily backtest core ----------

#' Given aligned daily df (Date, Close, Bench, RSI, ret20), fundamentals
#' and params, simulate quarterly rebalance and return
#' equity_df / valuation_df / exposure summary / explain_last.
.run_backtest_core <- function(df, fund, params, model_params, mos_fallback = 0,
                               beta_df = NULL, fv_only = FALSE) {
  thr_npm <- .safe_num(params$bt_net_margin, 5)
  thr_rev <- .safe_num(params$bt_rev_growth, 10)
  thr_eps <- .safe_num(params$bt_eps_growth, 10)
  thr_cv  <- .safe_num(params$bt_fcf_cv, 25)
  w_mom <- .safe_num(params$bt_w_mom, 0.5)
  w_rsi <- .safe_num(params$bt_w_rsi, 0.5)
  w_vg  <- .safe_num(params$bt_w_vg, 0.7)
  max_exp <- .clip01(.safe_num(params$bt_max_exp, 0.90), 0.5, 1)
  min_exp_pass <- .clip01(.safe_num(params$bt_min_exp_pass, 0), 0, 0.4)

  # Full history for rolling β (may be longer than the simulation window).
  if (is.null(beta_df) || !is.data.frame(beta_df) ||
      !all(c("Date", "Close", "Bench") %in% names(beta_df))) {
    beta_df <- df[, c("Date", "Close", "Bench"), drop = FALSE]
  }

  # Quarter-end rebalance: last available trading day per (year, quarter).
  qkey <- sprintf("%d-Q%d",
                  as.integer(format(df$Date, "%Y")),
                  ((as.integer(format(df$Date, "%m")) - 1) %/% 3) + 1)
  quarter_ends <- !duplicated(qkey, fromLast = TRUE)
  rebal_idx <- which(quarter_ends & !is.na(df$RSI) & !is.na(df$ret20))
  if (length(rebal_idx) < 4) {
    stop("可再平衡季數不足（需要較長股價歷史）")
  }

  n <- nrow(df)
  pos_a <- 0
  pos_b <- 0
  equity_a <- numeric(n); equity_a[1] <- 1   # Trade_A: exposure sim (diagnostic)
  equity_b <- numeric(n); equity_b[1] <- 1
  equity_bh <- numeric(n); equity_bh[1] <- 1
  equity_bm <- numeric(n); equity_bm[1] <- 1
  exp_a_daily <- numeric(n)
  exp_b_daily <- numeric(n)
  # Mode A chart: selected-model PIT fair value, grown by SGR between
  # rebalances (avoids multi-quarter flat LOCF steps when fund year is unchanged).
  fv_daily <- rep(NA_real_, n)
  fv_anchor <- NA_real_
  fv_anchor_date <- as.Date(NA)
  g_carry <- .safe_num(model_params$sgr, 0.025)
  if (!is.finite(g_carry)) g_carry <- 0.025
  g_carry <- max(min(g_carry, 0.12), -0.05)

  val_rows <- list()
  explain_last <- NULL

  for (i in 2:n) {
    r  <- df$Close[i] / df$Close[i - 1] - 1
    rb <- df$Bench[i] / df$Bench[i - 1] - 1
    if (!is.finite(r)) r <- 0
    if (!is.finite(rb)) rb <- 0

    if (i %in% rebal_idx) {
      fund_i <- .lookup_fund_at(fund, df$Date[i])
      price_i <- .safe_num(df$Close[i], NA_real_)

      # Momentum + RSI features for sentiment overlay only.
      mom_score <- .clip01((.safe_num(df$ret20[i], 0) + 0.05) / 0.15, 0, 1)
      rsi <- .safe_num(df$RSI[i], 50)
      rsi_score <- if (rsi >= 80) 0.15 else if (rsi >= 70) 0.4
                   else if (rsi <= 30) 0.85 else 0.55

      # Rolling β → Ke/WACC at this rebalance (not session fixed β).
      disc <- pit_discount_params(
        model_params,
        stock_close = beta_df$Close,
        bench_close = beta_df$Bench,
        dates = beta_df$Date,
        as_of = df$Date[i]
      )
      mp_i <- disc$model_params

      pit <- reconstruct_fair_value_pit(fund_i, price_i, mp_i)
      mos_i <- if (is.finite(pit$mos)) pit$mos else mos_fallback
      signal_i <- pit$signal
      if (is.finite(pit$fair_value) && pit$fair_value > 0) {
        fv_anchor <- pit$fair_value
        fv_anchor_date <- df$Date[i]
      }

      if (isTRUE(fv_only)) {
        pos_a <- 0
        pos_b <- 0
        sent_mult <- 1
        gf <- list(pass = NA, path = "fv_only")
        explain_txt <- sprintf(
          "%s | 公允 %.2f (dcf %.2f / ddm %.2f / ri %.2f / pb %.2f) vs 市價 %.2f, MOS %.1f%%, score %.0f/100. Rollingβ=%.2f Ke=%.1f%% WACC=%.1f%%.",
          signal_i,
          .safe_num(pit$fair_value, NA_real_),
          .safe_num(pit$fv_dcf, NA_real_), .safe_num(pit$fv_ddm, NA_real_),
          .safe_num(pit$fv_ri, NA_real_),  .safe_num(pit$fv_pb, NA_real_),
          .safe_num(price_i, NA_real_),
          100 * .safe_num(mos_i, NA_real_),
          .safe_num(pit$valuation_score, NA_real_),
          .safe_num(disc$beta, NA_real_),
          100 * .safe_num(disc$ke, NA_real_),
          100 * .safe_num(disc$wacc, NA_real_)
        )
      } else {
        gf <- .great_filter_pass(fund_i, thr_npm, thr_rev, thr_eps, thr_cv, fund_i$cv_fcf)

        # ---- Mode A exposure (Trade_A); Mode B nests on Exp_A ----
        pos_a_target <- mos_hysteresis_target(mos_i, w_vg, max_exp = max_exp)
        if (!isTRUE(gf$pass)) {
          pos_a_target <- 0
        } else if (is.finite(mos_i) && mos_i >= -0.10 && min_exp_pass > 0) {
          pos_a_target <- max(pos_a_target, min_exp_pass)
        }
        pos_a <- .clip01(min(pos_a_target, max_exp), 0, 1)

        # ---- Strategy B: sentiment overlay ONLY scales Exp_A weight ----
        sent_mult <- sentiment_multiplier(mom_score, rsi_score, w_mom, w_rsi)
        if (pos_a <= 0) {
          pos_b <- 0
        } else {
          pos_b_raw <- pos_a * sent_mult
          pos_b <- .clip01(min(max(pos_b_raw, pos_a * 0.75), min(1, pos_a * 1.25)), 0, 1)
        }

        explain_txt <- sprintf(
          "%s | 公允 %.2f (dcf %.2f / ddm %.2f / ri %.2f / pb %.2f) vs 市價 %.2f, MOS %.1f%%, score %.0f/100. Rollingβ=%.2f Ke=%.1f%% WACC=%.1f%%. 過濾:%s(%s). Exp_A=%.2f, Exp_B=%.2f (Sent x%.2f).",
          signal_i,
          .safe_num(pit$fair_value, NA_real_),
          .safe_num(pit$fv_dcf, NA_real_), .safe_num(pit$fv_ddm, NA_real_),
          .safe_num(pit$fv_ri, NA_real_),  .safe_num(pit$fv_pb, NA_real_),
          .safe_num(price_i, NA_real_),
          100 * .safe_num(mos_i, NA_real_),
          .safe_num(pit$valuation_score, NA_real_),
          .safe_num(disc$beta, NA_real_),
          100 * .safe_num(disc$ke, NA_real_),
          100 * .safe_num(disc$wacc, NA_real_),
          if (isTRUE(gf$pass)) "PASS" else "FAIL",
          gf$path, pos_a, pos_b, sent_mult
        )
      }

      val_rows[[length(val_rows) + 1L]] <- data.frame(
        Date = df$Date[i],
        fund_year = fund_i$fund_year,
        hist_price = price_i,
        bench_price = .safe_num(df$Bench[i], NA_real_),
        fv_dcf = .safe_num(pit$fv_dcf, NA_real_),
        fv_ddm = .safe_num(pit$fv_ddm, NA_real_),
        fv_ri  = .safe_num(pit$fv_ri,  NA_real_),
        fv_pb  = .safe_num(pit$fv_pb,  NA_real_),
        fair_value = .safe_num(pit$fair_value, NA_real_),
        strategy_fv = .safe_num(pit$fair_value, NA_real_),  # back-compat alias
        mos = mos_i,
        signal = signal_i,
        valuation_score = .safe_num(pit$valuation_score, NA_real_),
        rolling_beta = .safe_num(disc$beta, NA_real_),
        ke_pit = .safe_num(disc$ke, NA_real_),
        wacc_pit = .safe_num(disc$wacc, NA_real_),
        exp_a = pos_a,
        exp_b = pos_b,
        filter_pass = isTRUE(gf$pass),
        filter_path = gf$path,
        pos_fundamental = pos_a,  # back-compat alias
        explain = explain_txt,
        stringsAsFactors = FALSE
      )

      explain_last <- list(
        Date = df$Date[i],
        fund_year = fund_i$fund_year,
        price = price_i,
        fair_value = pit$fair_value,
        fv_dcf = pit$fv_dcf, fv_ddm = pit$fv_ddm,
        fv_ri = pit$fv_ri,  fv_pb = pit$fv_pb,
        mos = mos_i, signal = signal_i,
        valuation_score = pit$valuation_score,
        rolling_beta = disc$beta, ke_pit = disc$ke, wacc_pit = disc$wacc,
        filter_pass = isTRUE(gf$pass),
        filter_path = gf$path,
        exp_a = pos_a, exp_b = pos_b,
        sentiment_mult = sent_mult,
        bvps = pit$bvps, roe = pit$roe, dps = pit$dps, payout = pit$payout
      )
    }

    if (is.finite(fv_anchor) && fv_anchor > 0 && !is.na(fv_anchor_date)) {
      dt_yrs <- as.numeric(difftime(df$Date[i], fv_anchor_date, units = "days")) / 365.25
      if (!is.finite(dt_yrs) || dt_yrs < 0) dt_yrs <- 0
      fv_daily[i] <- fv_anchor * (1 + g_carry)^dt_yrs
    } else {
      fv_daily[i] <- NA_real_
    }
    exp_a_daily[i] <- pos_a
    exp_b_daily[i] <- pos_b
    if (!isTRUE(fv_only)) {
      equity_a[i]  <- equity_a[i - 1]  * (1 + pos_a * r)
      equity_b[i]  <- equity_b[i - 1]  * (1 + pos_b * r)
      equity_bh[i] <- equity_bh[i - 1] * (1 + r)
      equity_bm[i] <- equity_bm[i - 1] * (1 + rb)
    }
  }

  # Backfill FV before first rebalance; normalize Mode A to start at 1.
  first_fv <- which(is.finite(fv_daily) & fv_daily > 0)[1]
  if (is.finite(first_fv)) {
    if (first_fv > 1L) fv_daily[seq_len(first_fv - 1L)] <- fv_daily[first_fv]
    model_a <- fv_daily / fv_daily[1]
    model_a[!is.finite(model_a)] <- NA_real_
  } else {
    model_a <- rep(NA_real_, n)
  }

  valuation_df <- if (length(val_rows) > 0) do.call(rbind, val_rows) else {
    data.frame(
      Date = as.Date(character()), fund_year = integer(),
      hist_price = numeric(), bench_price = numeric(),
      fv_dcf = numeric(), fv_ddm = numeric(), fv_ri = numeric(), fv_pb = numeric(),
      fair_value = numeric(), strategy_fv = numeric(),
      mos = numeric(), signal = character(),
      valuation_score = numeric(),
      rolling_beta = numeric(), ke_pit = numeric(), wacc_pit = numeric(),
      exp_a = numeric(), exp_b = numeric(),
      filter_pass = logical(), filter_path = character(),
      pos_fundamental = numeric(), explain = character(),
      stringsAsFactors = FALSE
    )
  }

  equity_df <- data.frame(
    Date = df$Date,
    Close = df$Close,
    Bench = df$Bench,
    # Daily PIT fair value (carried between rebalances) for 折現比較圖
    FairValue = fv_daily,
    # Model_A: FV index for plateau (NOT equity-chart Mode A).
    Model_A = model_a,
    # Two backtest modes → two strategy NAVs on the equity chart.
    Trade_A = equity_a,   # 純基本面價值
    Trade_B = equity_b,   # 情緒波動價值
    Model_B = equity_b,   # back-compat alias of Trade_B
    BuyHold = equity_bh,
    Benchmark = equity_bm,
    Exp_A = exp_a_daily,
    Exp_B = exp_b_daily,
    stringsAsFactors = FALSE
  )

  # Align comparison window at first quarterly decision so strategies
  # (cash until first rebalance) do not give Buy&Hold a free head-start.
  if (!isTRUE(fv_only)) {
    i0 <- rebal_idx[1L]
    if (is.finite(i0) && i0 > 1L && i0 <= n) {
      equity_df <- equity_df[i0:n, , drop = FALSE]
      for (col in c("Model_A", "Trade_A", "Trade_B", "Model_B", "BuyHold", "Benchmark")) {
        base <- equity_df[[col]][1]
        if (is.finite(base) && base > 0) {
          equity_df[[col]] <- equity_df[[col]] / base
        }
      }
      rownames(equity_df) <- NULL
    }
  }

  mkt <- .compute_market_pricing_metrics(valuation_df)

  if (isTRUE(fv_only)) {
    return(list(
      equity_df = equity_df[, c("Date", "Close", "Bench", "FairValue"), drop = FALSE],
      valuation_df = valuation_df,
      exposure = NULL,
      metrics = list(
        sharpe_a = NA_real_, sharpe_b = NA_real_,
        mdd_a = NA_real_, mdd_b = NA_real_,
        cagr_a = NA_real_, cagr_b = NA_real_,
        plateau = "待驗證",
        best = "A",
        pct_market_under = mkt$pct_market_under,
        pct_market_over = mkt$pct_market_over,
        market_pricing_bias = mkt$market_pricing_bias,
        market_pricing_dominant_pct = mkt$market_pricing_dominant_pct,
        pct_strategy_under = mkt$pct_strategy_under,
        pct_value_over = mkt$pct_value_over,
        mean_hist_mos = mkt$mean_hist_mos,
        last_signal = mkt$last_signal
      ),
      explain_last = explain_last
    ))
  }

  # exposure summary (post-alignment window)
  ea <- equity_df$Exp_A[-1]
  eb <- equity_df$Exp_B[-1]
  exposure <- list(
    avg_a = mean(ea), max_a = max(ea), min_a = min(ea), cash_avg_a = 1 - mean(ea),
    avg_b = mean(eb), max_b = max(eb), min_b = min(eb), cash_avg_b = 1 - mean(eb)
  )

  # perf metrics
  perf_one <- function(eq) {
    rets <- diff(eq) / head(eq, -1)
    rets <- rets[is.finite(rets)]
    if (length(rets) < 20) return(list(sharpe = NA_real_, mdd = NA_real_, cagr = NA_real_))
    mu <- mean(rets); sdv <- stats::sd(rets)
    sharpe <- if (isTRUE(sdv > 0)) (mu / sdv) * sqrt(252) else NA_real_
    peak <- cummax(eq); dd <- eq / peak - 1; mdd <- min(dd, na.rm = TRUE)
    yrs <- as.numeric(difftime(equity_df$Date[length(equity_df$Date)], equity_df$Date[1], units = "days")) / 365.25
    cagr <- if (isTRUE(yrs > 0)) eq[length(eq)] ^ (1 / yrs) - 1 else NA_real_
    list(sharpe = sharpe, mdd = mdd, cagr = cagr)
  }
  # Trading metrics for the two modes — never the FV index.
  pa <- perf_one(equity_df$Trade_A)
  pb <- perf_one(equity_df$Trade_B)

  list(
    equity_df = equity_df,
    valuation_df = valuation_df,
    exposure = exposure,
    metrics = list(
      sharpe_a = pa$sharpe, sharpe_b = pb$sharpe,
      mdd_a = pa$mdd, mdd_b = pb$mdd,
      cagr_a = pa$cagr, cagr_b = pb$cagr,
      plateau = "待驗證",   # filled by run_parameter_plateau
      best = if (isTRUE(.safe_num(pa$sharpe, -Inf) >= .safe_num(pb$sharpe, -Inf))) "A" else "B",
      pct_market_under = mkt$pct_market_under,
      pct_market_over = mkt$pct_market_over,
      market_pricing_bias = mkt$market_pricing_bias,
      market_pricing_dominant_pct = mkt$market_pricing_dominant_pct,
      pct_strategy_under = mkt$pct_strategy_under,
      pct_value_over = mkt$pct_value_over,
      mean_hist_mos = mkt$mean_hist_mos,
      last_signal = mkt$last_signal
    ),
    explain_last = explain_last
  )
}

#' Recompute fair-value series after valuation-model change (no strategy re-sim).
refresh_backtest_fair_value <- function(res, fund, model_params) {
  if (is.null(res) || is.null(res$equity_df) || is.null(res$valuation_df)) {
    stop("尚無回測結果可更新")
  }
  equity_df <- res$equity_df
  vd <- res$valuation_df
  if (nrow(vd) == 0) stop("尚無再平衡估值紀錄")

  mp_base <- model_params
  if (is.null(mp_base)) mp_base <- res$model_params_used
  if (is.null(mp_base)) stop("缺少模型參數")

  g_carry <- .safe_num(mp_base$sgr, 0.025)
  if (!is.finite(g_carry)) g_carry <- 0.025
  g_carry <- max(min(g_carry, 0.12), -0.05)

  n <- nrow(equity_df)
  fv_daily <- rep(NA_real_, n)
  rebal_idx <- match(vd$Date, equity_df$Date)

  for (j in seq_len(nrow(vd))) {
    i <- rebal_idx[j]
    if (is.na(i)) next
    fund_i <- .lookup_fund_at(fund, vd$Date[j])
    price_i <- .safe_num(vd$hist_price[j], equity_df$Close[i])

    mp_i <- mp_base
    if ("wacc_pit" %in% names(vd) && is.finite(vd$wacc_pit[j])) mp_i$wacc <- vd$wacc_pit[j]
    if ("ke_pit" %in% names(vd) && is.finite(vd$ke_pit[j])) mp_i$ke <- vd$ke_pit[j]

    pit <- reconstruct_fair_value_pit(fund_i, price_i, mp_i)
    vd$fv_dcf[j] <- .safe_num(pit$fv_dcf, NA_real_)
    vd$fv_ddm[j] <- .safe_num(pit$fv_ddm, NA_real_)
    vd$fv_ri[j]  <- .safe_num(pit$fv_ri, NA_real_)
    vd$fv_pb[j]  <- .safe_num(pit$fv_pb, NA_real_)
    vd$fair_value[j] <- .safe_num(pit$fair_value, NA_real_)
    vd$strategy_fv[j] <- vd$fair_value[j]
    vd$mos[j] <- pit$mos
    vd$signal[j] <- pit$signal
    vd$valuation_score[j] <- .safe_num(pit$valuation_score, NA_real_)
  }

  current_anchor <- NA_real_
  current_date <- as.Date(NA)
  rebal_set <- rebal_idx[!is.na(rebal_idx)]
  for (i in seq_len(n)) {
    if (i %in% rebal_set) {
      j <- which(rebal_idx == i)[1]
      if (is.finite(vd$fair_value[j]) && vd$fair_value[j] > 0) {
        current_anchor <- vd$fair_value[j]
        current_date <- equity_df$Date[i]
      }
    }
    if (is.finite(current_anchor) && current_anchor > 0 && !is.na(current_date)) {
      dt_yrs <- as.numeric(difftime(equity_df$Date[i], current_date, units = "days")) / 365.25
      if (!is.finite(dt_yrs) || dt_yrs < 0) dt_yrs <- 0
      fv_daily[i] <- current_anchor * (1 + g_carry)^dt_yrs
    }
  }

  first_fv <- which(is.finite(fv_daily) & fv_daily > 0)[1]
  if (is.finite(first_fv)) {
    if (first_fv > 1L) fv_daily[seq_len(first_fv - 1L)] <- fv_daily[first_fv]
    model_a <- fv_daily / fv_daily[1]
    model_a[!is.finite(model_a)] <- NA_real_
  } else {
    model_a <- rep(NA_real_, n)
  }

  equity_df$FairValue <- fv_daily
  equity_df$Model_A <- model_a

  metrics <- res$metrics
  if (is.null(metrics)) metrics <- list()
  mkt <- .compute_market_pricing_metrics(vd)
  metrics <- utils::modifyList(metrics, mkt)

  explain_last <- res$explain_last
  if (nrow(vd) > 0) {
    last <- vd[nrow(vd), , drop = FALSE]
    explain_last <- utils::modifyList(
      if (is.list(explain_last)) explain_last else list(),
      list(
        fair_value = last$fair_value,
        fv_dcf = last$fv_dcf, fv_ddm = last$fv_ddm,
        fv_ri = last$fv_ri, fv_pb = last$fv_pb,
        mos = last$mos, signal = last$signal,
        valuation_score = last$valuation_score
      )
    )
  }

  mp_out <- mp_base
  if (!is.null(res$model_params_used)) {
    mp_out <- utils::modifyList(res$model_params_used, mp_base)
  }

  list(
    equity_df = equity_df,
    valuation_df = vd,
    exposure = res$exposure,
    metrics = metrics,
    explain_last = explain_last,
    bench_ticker = res$bench_ticker,
    n_days = res$n_days,
    model_params_used = mp_out,
    dcf_params_used = mp_out
  )
}

# ---------- prepare aligned daily frame ----------

.prepare_daily_df <- function(px, bench) {
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
  df
}

# ---------- public API ----------

#' Lightweight price frame for the HFV discount chart (no fundamentals).
fetch_hfv_price_frame <- function(ticker, bench_ticker = "SPY", years = 5) {
  sim_years <- max(1L, as.integer(years))
  fetch_years <- max(sim_years + 2L, 5L)
  period <- paste0(fetch_years, "y")
  px <- fetch_price_history_df(ticker, period)
  if (is.null(px) || nrow(px) < 30) {
    stop("無法取得足夠的歷史股價")
  }
  bench <- fetch_price_history_df(bench_ticker, period)
  if (is.null(bench) || nrow(bench) < 30) {
    bench <- px
  }
  df <- merge(
    data.frame(Date = px$Date, Close = px$Close, stringsAsFactors = FALSE),
    data.frame(Date = bench$Date, Bench = bench$Close, stringsAsFactors = FALSE),
    by = "Date", all = FALSE
  )
  df <- df[order(df$Date), , drop = FALSE]
  cutoff <- max(df$Date) - as.difftime(round(sim_years * 365.25), units = "days")
  df <- df[df$Date >= cutoff, , drop = FALSE]
  if (nrow(df) < 30) stop("股價與基準對齊後資料不足")
  df[, c("Date", "Close", "Bench"), drop = FALSE]
}

#' PIT fair-value timeline only (for HFV chart refresh; no strategy simulation).
compute_fair_value_timeline <- function(ticker,
                                        d_is, d_bs, d_cf,
                                        model_params = NULL,
                                        mos = NA_real_,
                                        bench_ticker = "SPY",
                                        years = 5) {
  if (is.null(model_params)) model_params <- list()
  if (is.null(model_params$ke) || !is.finite(.safe_num(model_params$ke, NA_real_))) {
    model_params$ke <- .safe_num(model_params$wacc, 0.09)
  }
  if (is.null(model_params$ddm_g) || !is.finite(.safe_num(model_params$ddm_g, NA_real_))) {
    model_params$ddm_g <- .safe_num(model_params$sgr, 0.025)
  }
  if (is.null(model_params$pb_mid) || !is.finite(.safe_num(model_params$pb_mid, NA_real_))) {
    model_params$pb_mid <- 2.5
  }
  if (is.null(model_params$n_years) || !is.finite(.safe_num(model_params$n_years, NA_real_))) {
    model_params$n_years <- 5
  }
  if (is.null(model_params$fv_model) || !nzchar(as.character(model_params$fv_model)[1])) {
    model_params$fv_model <- "dcf"
  }
  if (is.null(model_params$beta_lookback_months) ||
      !is.finite(.safe_num(model_params$beta_lookback_months, NA_real_))) {
    model_params$beta_lookback_months <- 60L
  }

  sim_years <- max(1L, as.integer(years))
  fetch_years <- max(sim_years + 5L, 10L)
  period <- paste0(fetch_years, "y")
  px <- fetch_price_history_df(ticker, period)
  if (is.null(px) || nrow(px) < 80) {
    stop("無法取得足夠的歷史股價（至少約 80 個交易日）")
  }
  bench <- fetch_price_history_df(bench_ticker, period)
  if (is.null(bench) || nrow(bench) < 80) {
    bench <- px
    bench_ticker <- paste0(ticker, "(BH)")
  }
  df_full <- .prepare_daily_df(px, bench)
  beta_df <- df_full[, c("Date", "Close", "Bench"), drop = FALSE]
  cutoff <- max(df_full$Date) - as.difftime(round(sim_years * 365.25), units = "days")
  df <- df_full[df_full$Date >= cutoff, , drop = FALSE]
  if (nrow(df) < 80) df <- df_full

  fund <- build_annual_fundamentals(d_is, d_bs, d_cf)
  mos_fallback <- .safe_num(mos, 0)
  dummy_params <- list(
    bt_net_margin = 0, bt_rev_growth = 0, bt_eps_growth = 0, bt_fcf_cv = 999,
    bt_w_mom = 0.5, bt_w_rsi = 0.5, bt_w_vg = 0.7,
    bt_max_exp = 0.9, bt_min_exp_pass = 0
  )
  core <- .run_backtest_core(
    df, fund, dummy_params, model_params, mos_fallback,
    beta_df = beta_df, fv_only = TRUE
  )
  list(
    equity_df = core$equity_df,
    valuation_df = core$valuation_df,
    metrics = core$metrics,
    explain_last = core$explain_last,
    bench_ticker = bench_ticker,
    n_days = nrow(df),
    model_params_used = model_params
  )
}

#' Run one company backtest (v12).
#' @param model_params list(wacc, ke, sgr, g_explicit, n_years, pb_mid, ddm_g,
#'   fv_model, rf, rm, rd, tax, we, wd, beta_fallback).
#'   `dcf_params` accepted as a legacy alias (server.R still passes it).
run_company_backtest <- function(ticker,
                                 d_is, d_bs, d_cf,
                                 params,
                                 model_params = NULL,
                                 mos = NA_real_,
                                 dcf_params = NULL,
                                 bench_ticker = "SPY",
                                 years = 5) {
  if (is.null(model_params)) model_params <- dcf_params
  if (is.null(model_params)) model_params <- list()

  # Fill sensible defaults for optional model params.
  if (is.null(model_params$ke) || !is.finite(.safe_num(model_params$ke, NA_real_))) {
    model_params$ke <- .safe_num(model_params$wacc, 0.09)
  }
  if (is.null(model_params$ddm_g) || !is.finite(.safe_num(model_params$ddm_g, NA_real_))) {
    model_params$ddm_g <- .safe_num(model_params$sgr, 0.025)
  }
  if (is.null(model_params$pb_mid) || !is.finite(.safe_num(model_params$pb_mid, NA_real_))) {
    model_params$pb_mid <- 2.5  # neutral default; UI can override
  }
  if (is.null(model_params$n_years) || !is.finite(.safe_num(model_params$n_years, NA_real_))) {
    model_params$n_years <- 5
  }
  if (is.null(model_params$fv_model) || !nzchar(as.character(model_params$fv_model)[1])) {
    model_params$fv_model <- "dcf"
  }
  if (is.null(model_params$beta_lookback_months) ||
      !is.finite(.safe_num(model_params$beta_lookback_months, NA_real_))) {
    model_params$beta_lookback_months <- 60L
  }

  # Fetch extra history so early rebalances still have ~5Y monthly β lookback.
  sim_years <- max(1L, as.integer(years))
  fetch_years <- max(sim_years + 5L, 10L)
  period <- paste0(fetch_years, "y")
  px <- fetch_price_history_df(ticker, period)
  if (is.null(px) || nrow(px) < 80) {
    stop("無法取得足夠的歷史股價（至少約 80 個交易日）")
  }
  bench <- fetch_price_history_df(bench_ticker, period)
  if (is.null(bench) || nrow(bench) < 80) {
    bench <- px
    names(bench) <- names(px)
    bench_ticker <- paste0(ticker, "(BH)")
  }
  df_full <- .prepare_daily_df(px, bench)
  beta_df <- df_full[, c("Date", "Close", "Bench"), drop = FALSE]

  # Simulation window = last `sim_years` (equity / Mode A chart).
  cutoff <- max(df_full$Date) - as.difftime(round(sim_years * 365.25), units = "days")
  df <- df_full[df_full$Date >= cutoff, , drop = FALSE]
  if (nrow(df) < 80) df <- df_full

  fund <- build_annual_fundamentals(d_is, d_bs, d_cf)
  mos_fallback <- .safe_num(mos, 0)

  core <- .run_backtest_core(df, fund, params, model_params, mos_fallback,
                             beta_df = beta_df)

  list(
    equity_df    = core$equity_df,
    valuation_df = core$valuation_df,
    exposure     = core$exposure,
    metrics      = core$metrics,
    explain_last = core$explain_last,
    bench_ticker = bench_ticker,
    n_days       = nrow(df),
    model_params_used = model_params,
    dcf_params_used   = model_params  # back-compat alias for v11 UI
  )
}

# ---------- Backtest methodology notes (UI + download) ----------

#' Build plain-text / Markdown notes on backtest data sources & calc process.
#' @param meta optional named list with session fields for a filled-in appendix
build_bt_methodology_doc <- function(meta = NULL) {
  meta <- if (is.null(meta) || !is.list(meta)) list() else meta
  g <- function(key, default = "（執行回測後填入）") {
    v <- meta[[key]]
    if (is.null(v) || length(v) < 1 || (length(v) == 1 && is.na(v))) return(default)
    as.character(v[[1]])
  }

  paste0(
    "# The YNow App — 回測數據來源與計算過程說明\n",
    "\n",
    "版本：app_12.0｜產出日期：", format(Sys.Date(), "%Y-%m-%d"), "\n",
    "\n",
    "本文件說明 Backtest Zone「純基本面價值／情緒波動價值」回測所使用的資料來源、",
    "時點對齊規則、曝險與淨值計算流程。結果僅存於當次 Session，不寫入資料庫。\n",
    "\n",
    "---\n",
    "\n",
    "## 1. 數據來源\n",
    "\n",
    "| 項目 | 來源 | 說明 |\n",
    "|------|------|------|\n",
    "| 標的日收盤價 | Yahoo Finance（`yfinance`，`auto_adjust=True`） | 含拆股／股息調整後 Close；失敗時後備 quantmod/Yahoo |\n",
    "| 基準指數 | SPY（同上） | 無法取得時以標的自身代替並註記 |\n",
    "| 年度財報 | 本次 Session 已搜尋載入之 IS／BS／CF | 來自 yfinance 財報表；欄位標準化後使用 |\n",
    "| 無風險利率 Rf | `^TNX`（10 年期美債殖利率） | 供 Alpha／Sharpe 等驗證用；失敗時預設約 4% |\n",
    "| 評價假設 | Get Started／Dashboard 目前 Session 參數 | SGR、年數、P/B 等；Ke／WACC 於再平衡日以 Rolling β 重估 |\n",
    "\n",
    "價格抓取期間：模擬視窗約最近 ", g("sim_years", "5"), " 年；",
    "為 Rolling β 另多抓約 5 年歷史（合計常 ≥10 年）。\n",
    "\n",
    "## 2. Point-in-Time（避免前瞻偏差）\n",
    "\n",
    "1. 僅在**季末再平衡日**重建合理價與曝險。\n",
    "2. 財報對齊規則：使用財政年度 `fund_year ≤ 回測日曆年 − 1` 的已公告年度資料",
    "（近似「只用當時已可知資訊」）。\n",
    "3. 折現率：各再平衡日以標的 vs SPY 約 **60 個月月報酬** 估計 Rolling β → CAPM Ke →",
    "結合 Session 資本結構得到當期 WACC（非全樣本固定 β）。\n",
    "4. 合理價模型：依執行面板「回測用評價模型」選擇 DCF／DDM／RI／P/B／綜合均值；",
    "成長／預測年數等來自當下 Session。\n",
    "\n",
    "## 3. 序列定義（淨值圖）\n",
    "\n",
    "- **該股買進持有 (Buy&Hold)**：每日 `E_t = E_{t-1} × (1 + r_t)`，全程 100% 持股；現金部位為 0。\n",
    "- **純基本面價值 (Trade_A)**：策略淨值 `E_t = E_{t-1} × (1 + Exp_A × r_t)`。",
    "`Exp_A` 由持倉回測條件 + MOS 滯後曝險決定（上限見 `bt_max_exp`，預設 90%；可調至 100%）。這是淨值圖上的「模式 A」。\n",
    "- **情緒波動價值 (Trade_B)**：策略淨值，在 `Exp_A` 上乘情緒乘數（動能／RSI），",
    "並夾在 `[0.75×Exp_A, min(1, 1.25×Exp_A)]`；`Exp_A=0` 時必須空手。這是淨值圖上的「模式 B」，嵌套於 A 而非獨立宇宙。\n",
    "- **大盤基準**：SPY 全日報酬累積指數。\n",
    "- **合理價指數 (Model_A)**：僅供參數高原等診斷；**不畫在淨值圖**。",
    "價格 vs 合理價請看 Historical Fair Value Timeline。\n",
    "\n",
    "比較視窗自**首次有效季再平衡日**對齊起點（避免策略暖身期空手讓 Buy&Hold 佔先）。\n",
    "\n",
    "## 4. 曝險計算過程（季頻）\n",
    "\n",
    "```\n",
    "每日收盤報酬 r_t = Close_t / Close_{t-1} - 1\n",
    "若為季再平衡日：\n",
    "  1) PIT 重建合理價 → MOS = (FV - Price) / Price\n",
    "  2) 持倉回測條件（淨利率／營收成長／EPS成長／FCF CV 門檻）\n",
    "     → 未通過則 Exp_A = Exp_B = 0\n",
    "  3) 通過則 Exp_A = MOS 滯後映射（與 w_vg 混合；MOS≥30%→bt_max_exp；\n",
    "     可選 bt_min_exp_pass 地板，當 MOS≥−10%）\n",
    "  4) Exp_B = clip(Exp_A × sentiment_mult, 0.75×Exp_A … 1.25×Exp_A)\n",
    "非再平衡日：沿用上一季 Exp_A／Exp_B\n",
    "```\n",
    "\n",
    "**刻意設計（非錯誤）：** 現金部位報酬視為 0；未建模交易成本／稅負／滑價。",
    "牛市中 Buy&Hold 常因「滿倉」勝過減碼策略，屬風控取捨。\n",
    "\n",
    "## 5. 驗證指標（回測後）\n",
    "\n",
    "- **Alpha／Sharpe／MDD／Excess vs BH／Jensen α**：以 Trade_A、Trade_B、BuyHold、Benchmark 計算。\n",
    "- **MOS／FV 前瞻報酬**：依再平衡列分組，對齊日曆 1Y／3Y／5Y 前瞻報酬。\n",
    "- **參數高原**：微擾 WACC／SGR／年數等，觀察合理價指數 (Model_A) 終值相對變動（不是策略淨值）。\n",
    "- **為何輸給 Buy&Hold**：拆解 Cash Drag／Early Exit／高估減碼／情緒減碼等。\n",
    "\n",
    "## 6. 本次 Session 參數摘要\n",
    "\n",
    "- 標的：", g("ticker"), "\n",
    "- 基準：", g("bench", "SPY"), "\n",
    "- 模擬年數：", g("sim_years", "5"), "\n",
    "- 回測用評價模型：", g("fv_model"), "\n",
    "- 持倉回測條件門檻（淨利率／營收成長／EPS成長／FCF CV %）：",
    g("filters"), "\n",
    "- 純基本面 Fit：最大持股／通過後最低持股：", g("fit_exp", "0.90 / 0.00"), "\n",
    "- 權重 w_vg／w_mom／w_rsi：", g("weights"), "\n",
    "- Session SGR／n_years：", g("sgr_n"), "\n",
    "- 回測日數（對齊後）：", g("n_days"), "\n",
    "\n",
    "---\n",
    "\n",
    "**免責：** 本回測僅供研究與教育，不構成投資建議。資料依賴第三方公開來源，",
    "可能有缺漏或延遲；PIT 規則為實務近似，非交易所級時間戳對齊。\n"
  )
}

