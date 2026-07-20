# ==========================================
# backtest_module.R -- The YNow App V12.0
# --------------------------------------------------------------
# Dynamic session-only PIT (point-in-time) backtest engine.
# - No warehouse: every rebalance date reconstructs fair values
#   from annual financials whose fiscal year <= calendar_year - 1
#   using CURRENT session model parameters (WACC / Ke / g / n / P/B).
# - Multi-model composite fair value: DCF + DDM + RI + P/B, then
#   mean of available models.
# - Mode A (chart Model_A): session params × historical financials
#   → PIT composite fair-value path (LOCF, normalized). Independent
#   of position / exposure assumptions.
# - Exposure / Trade_A: MOS hysteresis + Great Filter (diagnostic;
#   also the base weight for Strategy B).
# - Strategy B: sentiment overlay ONLY scales Exp_A in
#   [0.75 * A, min(1, 1.25 * A)]; cannot override A == 0.
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
  if (is.na(fcf0) || is.na(shares) || shares <= 0 || is.na(wacc) || wacc <= 0) return(NA_real_)
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

#' Point-in-time fair-value reconstruction for a single fundamentals row.
#' @param fund_row list-like with fcf, cash, debt, shares, ni, equity_book,
#'   dividends_paid.
#' @param price historical closing price at rebalance date.
#' @param model_params list(wacc, ke, sgr, g_explicit, n_years, pb_mid, ddm_g,
#'   ri_g).
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
  ri_g <- .safe_num(model_params$ri_g, sgr)

  shares <- .safe_num(fund_row$shares, NA_real_)
  fcf    <- .safe_num(fund_row$fcf, NA_real_)
  cash   <- .safe_num(fund_row$cash, 0)
  debt   <- .safe_num(fund_row$debt, 0)
  ni     <- .safe_num(fund_row$ni, NA_real_)
  eqbook <- .safe_num(fund_row$equity_book, NA_real_)
  divp   <- .safe_num(fund_row$dividends_paid, NA_real_)

  bvps <- if (is.finite(eqbook) && is.finite(shares) && shares > 0) eqbook / shares else NA_real_
  roe  <- if (is.finite(ni) && is.finite(eqbook) && eqbook > 0) ni / eqbook else NA_real_
  dps  <- if (is.finite(divp) && is.finite(shares) && shares > 0) abs(divp) / shares else NA_real_
  payout <- if (is.finite(dps) && is.finite(ni) && ni > 0 && is.finite(shares) && shares > 0) {
    min(max(abs(divp) / ni, 0), 1)
  } else NA_real_

  fv_dcf <- estimate_hist_dcf(fcf, cash, debt, shares, wacc, sgr, n_yr, g_ex)
  fv_ddm <- if (is.finite(dps) && dps > 0) estimate_hist_ddm(dps, ke, ddm_g) else NA_real_
  fv_ri  <- estimate_hist_ri(bvps, roe, ke, ri_g, n = n_yr, payout = payout)
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
      " Strategy A（純基本面、長抱）：MOS≈%.1f%% → w_vg=%.2f（越大越依 MOS 分級）。",
      " Strategy B（情緒疊加）：動能%s、RSI≈%.0f → Mom/RSI 相對權重 %.2f / %.2f（僅微調 A 的權重，範圍 0.75~1.25×）。"
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
mos_hysteresis_target <- function(mos, w_vg = 0.7) {
  if (!is.finite(mos)) return(0.40)
  base <- if (mos >= 0.30) 0.90
          else if (mos >= 0.10) 0.65
          else if (mos >= 0.00) 0.40
          else if (mos >= -0.10) 0.15
          else 0.00
  w <- .clip01(w_vg, 0, 1)
  target <- (1 - w) * 0.40 + w * base
  .clip01(target, 0, 1)
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
.run_backtest_core <- function(df, fund, params, model_params, mos_fallback = 0) {
  thr_npm <- .safe_num(params$bt_net_margin, 5)
  thr_rev <- .safe_num(params$bt_rev_growth, 10)
  thr_eps <- .safe_num(params$bt_eps_growth, 10)
  thr_cv  <- .safe_num(params$bt_fcf_cv, 25)
  w_mom <- .safe_num(params$bt_w_mom, 0.5)
  w_rsi <- .safe_num(params$bt_w_rsi, 0.5)
  w_vg  <- .safe_num(params$bt_w_vg, 0.7)

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

      pit <- reconstruct_fair_value_pit(fund_i, price_i, model_params)
      mos_i <- if (is.finite(pit$mos)) pit$mos else mos_fallback
      signal_i <- pit$signal
      if (is.finite(pit$fair_value) && pit$fair_value > 0) {
        fv_anchor <- pit$fair_value
        fv_anchor_date <- df$Date[i]
      }

      gf <- .great_filter_pass(fund_i, thr_npm, thr_rev, thr_eps, thr_cv, fund_i$cv_fcf)

      # ---- Exposure base (diagnostic + Mode B weight); NOT chart Mode A ----
      pos_a_target <- mos_hysteresis_target(mos_i, w_vg)
      if (!isTRUE(gf$pass)) pos_a_target <- 0
      pos_a <- .clip01(pos_a_target, 0, 1)

      # ---- Strategy B: sentiment overlay ONLY scales Exp_A weight ----
      sent_mult <- sentiment_multiplier(mom_score, rsi_score, w_mom, w_rsi)
      if (pos_a <= 0) {
        pos_b <- 0
      } else {
        pos_b_raw <- pos_a * sent_mult
        pos_b <- .clip01(min(max(pos_b_raw, pos_a * 0.75), min(1, pos_a * 1.25)), 0, 1)
      }

      # explainability text
      explain_txt <- sprintf(
        "%s | 公允 %.2f (dcf %.2f / ddm %.2f / ri %.2f / pb %.2f) vs 市價 %.2f, MOS %.1f%%, score %.0f/100. 過濾:%s(%s). Exp_A=%.2f, Exp_B=%.2f (Sent x%.2f).",
        signal_i,
        .safe_num(pit$fair_value, NA_real_),
        .safe_num(pit$fv_dcf, NA_real_), .safe_num(pit$fv_ddm, NA_real_),
        .safe_num(pit$fv_ri, NA_real_),  .safe_num(pit$fv_pb, NA_real_),
        .safe_num(price_i, NA_real_),
        100 * .safe_num(mos_i, NA_real_),
        .safe_num(pit$valuation_score, NA_real_),
        if (isTRUE(gf$pass)) "PASS" else "FAIL",
        gf$path, pos_a, pos_b, sent_mult
      )

      val_rows[[length(val_rows) + 1L]] <- data.frame(
        Date = df$Date[i],
        fund_year = fund_i$fund_year,
        hist_price = price_i,
        fv_dcf = .safe_num(pit$fv_dcf, NA_real_),
        fv_ddm = .safe_num(pit$fv_ddm, NA_real_),
        fv_ri  = .safe_num(pit$fv_ri,  NA_real_),
        fv_pb  = .safe_num(pit$fv_pb,  NA_real_),
        fair_value = .safe_num(pit$fair_value, NA_real_),
        strategy_fv = .safe_num(pit$fair_value, NA_real_),  # back-compat alias
        mos = mos_i,
        signal = signal_i,
        valuation_score = .safe_num(pit$valuation_score, NA_real_),
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
    equity_a[i]  <- equity_a[i - 1]  * (1 + pos_a * r)
    equity_b[i]  <- equity_b[i - 1]  * (1 + pos_b * r)
    equity_bh[i] <- equity_bh[i - 1] * (1 + r)
    equity_bm[i] <- equity_bm[i - 1] * (1 + rb)
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
      hist_price = numeric(),
      fv_dcf = numeric(), fv_ddm = numeric(), fv_ri = numeric(), fv_pb = numeric(),
      fair_value = numeric(), strategy_fv = numeric(),
      mos = numeric(), signal = character(),
      valuation_score = numeric(), exp_a = numeric(), exp_b = numeric(),
      filter_pass = logical(), filter_path = character(),
      pos_fundamental = numeric(), explain = character(),
      stringsAsFactors = FALSE
    )
  }

  equity_df <- data.frame(
    Date = df$Date,
    Close = df$Close,
    # Chart Mode A: params × hist financials FV path (no position sizing).
    Model_A = model_a,
    # Exposure-weighted sim kept for gap / alpha / plateau diagnostics.
    Trade_A = equity_a,
    Model_B = equity_b,   # Strategy B (sentiment-adjusted Exp_A)
    BuyHold = equity_bh,
    Benchmark = equity_bm,
    Exp_A = exp_a_daily,
    Exp_B = exp_b_daily,
    stringsAsFactors = FALSE
  )

  # exposure summary
  ea <- exp_a_daily[-1]
  eb <- exp_b_daily[-1]
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
    yrs <- as.numeric(difftime(df$Date[length(df$Date)], df$Date[1], units = "days")) / 365.25
    cagr <- if (isTRUE(yrs > 0)) eq[length(eq)] ^ (1 / yrs) - 1 else NA_real_
    list(sharpe = sharpe, mdd = mdd, cagr = cagr)
  }
  # Trading metrics: Trade_A (exposure sim) + Strategy B — not FV index.
  pa <- perf_one(equity_a)
  pb <- perf_one(equity_b)

  # signal composition stats
  sig <- valuation_df$signal
  n_sig <- sum(sig %in% c("策略低估", "價值高估", "合理"), na.rm = TRUE)
  pct_under <- if (n_sig > 0) sum(sig == "策略低估", na.rm = TRUE) / n_sig else NA_real_
  pct_over  <- if (n_sig > 0) sum(sig == "價值高估", na.rm = TRUE) / n_sig else NA_real_
  mean_mos <- if (nrow(valuation_df) > 0) mean(valuation_df$mos[is.finite(valuation_df$mos)], na.rm = TRUE) else NA_real_
  last_signal <- if (nrow(valuation_df) > 0) tail(valuation_df$signal, 1) else "資料不足"

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
      pct_strategy_under = pct_under,
      pct_value_over = pct_over,
      mean_hist_mos = mean_mos,
      last_signal = last_signal
    ),
    explain_last = explain_last
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

#' Run one company backtest (v12).
#' @param model_params list(wacc, ke, sgr, g_explicit, n_years, pb_mid, ddm_g,
#'   ri_g).
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

  period <- paste0(as.integer(years), "y")
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
  df <- .prepare_daily_df(px, bench)
  fund <- build_annual_fundamentals(d_is, d_bs, d_cf)
  mos_fallback <- .safe_num(mos, 0)

  core <- .run_backtest_core(df, fund, params, model_params, mos_fallback)

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
