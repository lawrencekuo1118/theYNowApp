# HTCDI scoring — Human Tech Civilization Destruction Index.
# Oil index + critical-tech nodes from statements and market vs history.
# Reuses setup.R FX / ADR classifiers. Missing optional data is excluded.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

HTCDI_ERROR_CODES <- c(
  "STATEMENT_CURRENCY_UNAVAILABLE", "REQUIRED_FX_RATE_MISSING",
  "REQUIRED_FX_RATE_INVALID", "FX_DATE_MISMATCH",
  "APPLICABLE_ADR_RATIO_MISSING", "APPLICABLE_ADR_RATIO_INVALID",
  "REQUIRED_PER_SHARE_VALUE_NON_FINITE", "BENCHMARK_DATA_MISSING",
  "INSUFFICIENT_ROLLING_WINDOW", "STALE_SOURCE_DATA", "DUPLICATE_ECONOMIC_ISSUER",
  "SOURCE_HISTORY_UNAVAILABLE"
)
HTCDI_ALERT_LEVELS <- c("Expanding", "Steady", "Cooling", "Contracting", "Unavailable")
.HTCDI_CALC_LABELS <- c(
  per_share = "per-share alignment", returns = "return / volume",
  vol = "realized volatility", drawdown = "max drawdown", beta = "rolling beta",
  health = "Statement Development", stress = "Trajectory vs History",
  fragility = "Influence vs Market",
  market = "Market vs Benchmark", composite = "HTCDI composite"
)

.finite1 <- function(x) {
  num <- suppressWarnings(as.numeric(x)[1])
  length(num) == 1L && is.finite(num)
}
.htcdi_clip <- function(x, lo, hi) {
  if (!.finite1(x)) return(NA_real_)
  min(max(as.numeric(x)[1], lo), hi)
}
.htcdi_mean_excl_na <- function(vals, wts = NULL) {
  v <- suppressWarnings(as.numeric(vals))
  w <- if (is.null(wts)) rep(1, length(v)) else suppressWarnings(as.numeric(wts))
  if (length(w) != length(v)) w <- rep(1, length(v))
  ok <- is.finite(v) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(v[ok] * w[ok]) / sum(w[ok])
}
.htcdi_pillar <- function(value) if (.finite1(value)) .htcdi_clip(value, 0, 100) else NA_real_
.htcdi_from_signed <- function(x, half) {
  if (!.finite1(x) || !.finite1(half) || as.numeric(half)[1] <= 0) return(NA_real_)
  .htcdi_clip(50 + as.numeric(x)[1] / as.numeric(half)[1] * 50, 0, 100)
}
# Optional numeric: missing stays NA (never fill 0/50/100/72). Explicit 0 is kept.
.htcdi_opt <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  suppressWarnings(as.numeric(x)[1])
}

htcdi_classify_alignment <- function(source_currency, target_currency,
                                     instrument_type = "ordinary",
                                     underlying_share_comparison_required = FALSE,
                                     usd_twd = NULL, adr_ratio = NULL, shares = NULL,
                                     required_per_share = NULL,
                                     statement_currency = source_currency,
                                     share_method = NULL, fx_date = NULL, quote_date = NULL) {
  need_adr <- identical(toupper(as.character(instrument_type %||% "")[1]), "ADR") &&
    isTRUE(underlying_share_comparison_required)
  method <- if (!is.null(share_method) && nzchar(as.character(share_method)[1])) {
    share_method
  } else if (isTRUE(need_adr)) "market_cap_per_price" else "balance_sheet"
  if (!exists("classify_per_share_alignment_failure", mode = "function")) {
    src <- toupper(trimws(as.character(source_currency %||% "")[1]))
    tgt <- toupper(trimws(as.character(target_currency %||% "")[1]))
    st <- toupper(trimws(as.character(statement_currency %||% "")[1]))
    if (isTRUE(need_adr) && (!nzchar(st) || st %in% c("NA", "N/A"))) return("STATEMENT_CURRENCY_UNAVAILABLE")
    if (nzchar(src) && nzchar(tgt) && !identical(src, tgt)) {
      fx <- suppressWarnings(as.numeric(usd_twd)[1])
      if (!is.finite(fx) || fx <= 0) return("REQUIRED_FX_RATE_MISSING")
    }
    return(NULL)
  }
  code <- classify_per_share_alignment_failure(
    statement_ccy = statement_currency, quote_ccy = target_currency,
    usd_twd = usd_twd, share_method = method, equity_ccy = source_currency,
    shares = shares, adr_ratio = adr_ratio, required_per_share = required_per_share
  )
  if (!is.null(code)) return(code)
  if (!is.null(fx_date) && !is.null(quote_date)) {
    fd <- tryCatch(as.Date(fx_date)[1], error = function(e) as.Date(NA))
    qd <- tryCatch(as.Date(quote_date)[1], error = function(e) as.Date(NA))
    if (!is.na(fd) && !is.na(qd) && abs(as.numeric(fd - qd)) > 3) return("FX_DATE_MISMATCH")
  }
  NULL
}

htcdi_per_share <- function(equity, shares, equity_ccy, to_ccy, usd_twd = NULL,
                            instrument_type = "ordinary",
                            underlying_share_comparison_required = FALSE,
                            statement_ccy = equity_ccy, share_method = NULL, adr_ratio = NULL) {
  need_adr <- identical(toupper(as.character(instrument_type %||% "")[1]), "ADR") &&
    isTRUE(underlying_share_comparison_required)
  method <- share_method %||% if (isTRUE(need_adr)) "market_cap_per_price" else "balance_sheet"
  code <- htcdi_classify_alignment(
    source_currency = equity_ccy, target_currency = to_ccy,
    instrument_type = instrument_type,
    underlying_share_comparison_required = underlying_share_comparison_required,
    usd_twd = usd_twd, adr_ratio = adr_ratio, shares = shares,
    statement_currency = statement_ccy, share_method = method
  )
  px <- if (exists("per_share_in_quote", mode = "function")) {
    per_share_in_quote(equity, shares, equity_ccy = equity_ccy, to_ccy = to_ccy,
                       usd_twd = usd_twd, share_method = method, statement_ccy = statement_ccy)
  } else NA_real_
  if (!is.null(code)) { attr(px, "alignment_failure") <- code; return(px) }
  if (!is.finite(px)) attr(px, "alignment_failure") <- "REQUIRED_PER_SHARE_VALUE_NON_FINITE"
  px
}

htcdi_toast_key <- function(code) {
  cde <- as.character(code %||% "")[1]
  if (!nzchar(cde) || is.na(cde)) return(NA_character_)
  switch(cde,
    STATEMENT_CURRENCY_UNAVAILABLE = "notif_htcdi_statement_ccy",
    REQUIRED_FX_RATE_MISSING = "notif_htcdi_fx_missing",
    REQUIRED_FX_RATE_INVALID = "notif_htcdi_fx_invalid",
    FX_DATE_MISMATCH = "notif_htcdi_fx_date",
    APPLICABLE_ADR_RATIO_MISSING = "notif_htcdi_adr_missing",
    APPLICABLE_ADR_RATIO_INVALID = "notif_htcdi_adr_invalid",
    REQUIRED_PER_SHARE_VALUE_NON_FINITE = "notif_htcdi_per_share_bad",
    BENCHMARK_DATA_MISSING = "notif_htcdi_bench_missing",
    INSUFFICIENT_ROLLING_WINDOW = "notif_htcdi_window",
    STALE_SOURCE_DATA = "notif_htcdi_stale",
    DUPLICATE_ECONOMIC_ISSUER = "notif_htcdi_dup_issuer",
    SOURCE_HISTORY_UNAVAILABLE = "notif_htcdi_history_missing",
    NA_character_)
}

htcdi_failure_toast <- function(code, blocked_calc = "per_share",
                                available_calcs = c("returns", "vol"), locale = "en") {
  cde <- as.character(code %||% "")[1]
  if (!nzchar(cde) || is.na(cde)) return(NA_character_)
  key <- htcdi_toast_key(cde)
  blocked <- .HTCDI_CALC_LABELS[[blocked_calc]] %||% blocked_calc
  remain <- paste(vapply(available_calcs, function(k) .HTCDI_CALC_LABELS[[k]] %||% k, character(1)), collapse = "; ")
  if (exists("ui_str", mode = "function") && !is.na(key)) {
    base <- tryCatch(ui_str(key, locale), error = function(e) key)
    why <- tryCatch(ui_str("notif_htcdi_why_required", locale), error = function(e) "")
    blk <- gsub("{calc}", blocked, tryCatch(ui_str("notif_htcdi_blocked", locale), error = function(e) "Blocked: {calc}."), fixed = TRUE)
    av <- gsub("{calcs}", remain, tryCatch(ui_str("notif_htcdi_available", locale), error = function(e) "Still available: {calcs}."), fixed = TRUE)
    return(paste(base, why, blk, av))
  }
  paste0(cde, " — required only for the blocked calculation. Blocked: ", blocked, ". Still available: ", remain, ".")
}

htcdi_period_return <- function(closes, n = NULL) {
  x <- suppressWarnings(as.numeric(closes)); x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  if (!is.null(n) && is.finite(n) && n >= 1L) {
    x <- tail(x, min(length(x), as.integer(n) + 1L))
    if (length(x) < 2L) return(NA_real_)
  }
  a <- x[1]; b <- x[length(x)]
  if (!is.finite(a) || !is.finite(b) || a == 0) return(NA_real_)
  b / a - 1
}

htcdi_realized_vol <- function(closes, window = 20L) {
  x <- suppressWarnings(as.numeric(closes)); x <- x[is.finite(x)]
  if (length(x) < window + 1L) return(list(vol = NA_real_, code = "INSUFFICIENT_ROLLING_WINDOW"))
  r <- diff(tail(x, window + 1L)) / head(tail(x, window + 1L), -1)
  r <- r[is.finite(r)]
  if (length(r) < max(5L, floor(window / 2))) return(list(vol = NA_real_, code = "INSUFFICIENT_ROLLING_WINDOW"))
  list(vol = stats::sd(r) * sqrt(252), code = NULL)
}

htcdi_max_drawdown <- function(closes) {
  x <- suppressWarnings(as.numeric(closes)); x <- x[is.finite(x)]
  if (length(x) < 2L) return(list(max_dd = NA_real_, duration = NA_integer_, code = "INSUFFICIENT_ROLLING_WINDOW"))
  peak <- x[1]; max_dd <- 0; dur <- 0L; run <- 0L
  for (i in seq_along(x)) {
    if (x[i] > peak) { peak <- x[i]; run <- 0L
    } else if (peak > 0) {
      dd <- x[i] / peak - 1; run <- run + 1L
      if (dd < max_dd) { max_dd <- dd; dur <- run }
    }
  }
  list(max_dd = max_dd, duration = dur, code = NULL)
}

htcdi_rolling_beta <- function(stock_close, bench_close, window = 60L) {
  s <- suppressWarnings(as.numeric(stock_close)); b <- suppressWarnings(as.numeric(bench_close))
  n <- min(length(s), length(b))
  if (n < window + 2L) return(list(beta = NA_real_, instability = NA_real_, code = "INSUFFICIENT_ROLLING_WINDOW"))
  s <- tail(s[seq_len(n)], window + 1L); b <- tail(b[seq_len(n)], window + 1L)
  rs <- diff(s) / head(s, -1); rb <- diff(b) / head(b, -1)
  fine <- is.finite(rs) & is.finite(rb); rs <- rs[fine]; rb <- rb[fine]
  if (length(rs) < max(10L, floor(window / 3))) return(list(beta = NA_real_, instability = NA_real_, code = "INSUFFICIENT_ROLLING_WINDOW"))
  v <- stats::var(rb)
  if (!is.finite(v) || v <= 1e-12) return(list(beta = NA_real_, instability = NA_real_, code = "BENCHMARK_DATA_MISSING"))
  beta <- stats::cov(rs, rb) / v
  if (!is.finite(beta)) return(list(beta = NA_real_, instability = NA_real_, code = "BENCHMARK_DATA_MISSING"))
  mid <- max(8L, floor(length(rs) / 2))
  b1 <- if (mid >= 8L) {
    v1 <- stats::var(rb[seq_len(mid)])
    if (is.finite(v1) && v1 > 1e-12) stats::cov(rs[seq_len(mid)], rb[seq_len(mid)]) / v1 else NA_real_
  } else NA_real_
  list(beta = beta, instability = if (is.finite(b1)) abs(beta - b1) else NA_real_, code = NULL)
}

htcdi_volume_anomaly <- function(volume, window = 20L) {
  v <- suppressWarnings(as.numeric(volume)); v <- v[is.finite(v)]
  if (length(v) < window + 1L) return(NA_real_)
  base <- head(tail(v, window + 1L), window); last <- tail(v, 1)
  mu <- mean(base); sdv <- stats::sd(base)
  if (!is.finite(mu) || mu <= 0 || !is.finite(sdv) || sdv <= 0) return(NA_real_)
  (last - mu) / sdv
}

htcdi_stale_code <- function(dates, as_of = NULL, max_age_days = 7L) {
  if (is.null(dates) || !length(dates)) return("STALE_SOURCE_DATA")
  d <- tryCatch(as.Date(dates), error = function(e) as.Date(character(0))); d <- d[!is.na(d)]
  if (!length(d)) return("STALE_SOURCE_DATA")
  ref <- if (is.null(as_of)) max(d) else tryCatch(as.Date(as_of)[1], error = function(e) max(d))
  if (is.na(ref)) ref <- max(d)
  if (as.numeric(ref - max(d)) > max_age_days) "STALE_SOURCE_DATA" else NULL
}

htcdi_market_signals <- function(price_df = NULL, bench_df = NULL, as_of = NULL) {
  empty <- list(ret_1d = NA_real_, ret_1m = NA_real_, ret_ytd = NA_real_, ret_1y = NA_real_,
                vol_20d = NA_real_, vol_60d = NA_real_, max_dd = NA_real_,
                dd_duration = NA_integer_, beta_60d = NA_real_, beta_252d = NA_real_,
                beta_instability = NA_real_, abnormal_return = NA_real_,
                excess_1y = NA_real_, price_vs_hist = NA_real_,
                volume_z = NA_real_, failures = list())
  if (is.null(price_df) || !is.data.frame(price_df) || nrow(price_df) < 2L || !("Close" %in% names(price_df))) return(empty)
  df <- price_df[order(as.Date(price_df$Date)), , drop = FALSE]
  stale <- htcdi_stale_code(df$Date, as_of)
  if (!is.null(stale)) empty$failures$stale <- stale
  px <- df$Close
  empty$ret_1d <- htcdi_period_return(px, 1L)
  empty$ret_1m <- htcdi_period_return(px, 21L)
  empty$ret_1y <- htcdi_period_return(px, 252L)
  empty$price_vs_hist <- htcdi_price_vs_history(px)
  if ("Date" %in% names(df)) {
    dts <- as.Date(df$Date)
    y0 <- as.Date(paste0(format(max(dts, na.rm = TRUE), "%Y"), "-01-01"))
    first <- which(dts >= y0)[1]
    if (is.finite(first)) empty$ret_ytd <- htcdi_period_return(px[seq.int(first, length(px))])
  }
  v20 <- htcdi_realized_vol(px, 20L); v60 <- htcdi_realized_vol(px, 60L)
  empty$vol_20d <- v20$vol; empty$vol_60d <- v60$vol
  if (!is.null(v20$code)) empty$failures$vol_20d <- v20$code
  if (!is.null(v60$code)) empty$failures$vol_60d <- v60$code
  dd <- htcdi_max_drawdown(px); empty$max_dd <- dd$max_dd; empty$dd_duration <- dd$duration
  if ("Volume" %in% names(df)) empty$volume_z <- htcdi_volume_anomaly(df$Volume)
  if (is.null(bench_df) || !is.data.frame(bench_df) || nrow(bench_df) < 2L || !("Close" %in% names(bench_df))) {
    empty$failures$beta <- "BENCHMARK_DATA_MISSING"; empty$failures$abnormal <- "BENCHMARK_DATA_MISSING"
    return(empty)
  }
  bench <- bench_df[order(as.Date(bench_df$Date)), , drop = FALSE]
  common <- merge(data.frame(Date = as.Date(df$Date), S = df$Close),
                  data.frame(Date = as.Date(bench$Date), M = bench$Close), by = "Date")
  if (nrow(common) < 5L) {
    empty$failures$beta <- "BENCHMARK_DATA_MISSING"; empty$failures$abnormal <- "BENCHMARK_DATA_MISSING"
    return(empty)
  }
  b60 <- htcdi_rolling_beta(common$S, common$M, 60L)
  b252 <- htcdi_rolling_beta(common$S, common$M, 252L)
  empty$beta_60d <- b60$beta; empty$beta_instability <- b60$instability; empty$beta_252d <- b252$beta
  if (!is.null(b60$code)) empty$failures$beta <- b60$code
  sr <- htcdi_period_return(common$S, 21L); br <- htcdi_period_return(common$M, 21L)
  if (is.finite(sr) && is.finite(br)) empty$abnormal_return <- sr - br
  sr1 <- htcdi_period_return(common$S, 252L); br1 <- htcdi_period_return(common$M, 252L)
  if (is.finite(sr1) && is.finite(br1)) empty$excess_1y <- sr1 - br1
  empty
}

htcdi_price_vs_history <- function(closes) {
  x <- suppressWarnings(as.numeric(closes)); x <- x[is.finite(x)]
  if (length(x) < 60L) return(NA_real_)
  lag <- min(length(x) - 1L, max(60L, floor(length(x) * 0.6)))
  old <- x[length(x) - lag]; cur <- x[length(x)]
  if (!is.finite(old) || !is.finite(cur) || old == 0) return(NA_real_)
  cur / old - 1
}

.htcdi_fin_df <- function(fs, key) {
  if (is.null(fs) || !is.list(fs)) return(NULL)
  stmt <- fs[[key]]
  if (is.null(stmt)) return(NULL)
  df <- if (is.data.frame(stmt)) stmt else (stmt$expanded %||% stmt$collapsed)
  if (exists("coerce_financial_df", mode = "function")) {
    df <- tryCatch(coerce_financial_df(df), error = function(e) df)
  }
  if (is.data.frame(df) && nrow(df) >= 1L && ncol(df) >= 2L) df else NULL
}

.htcdi_metric_row <- function(df, names) {
  if (is.null(df) || !is.data.frame(df)) return(numeric(0))
  if (exists("select_clean_metric_row_any", mode = "function")) {
    v <- tryCatch(select_clean_metric_row_any(df, names, include_ttm = FALSE), error = function(e) NA)
    v <- suppressWarnings(as.numeric(v))
    return(v[is.finite(v)])
  }
  numeric(0)
}

.htcdi_latest_yoy <- function(vals) {
  v <- suppressWarnings(as.numeric(vals)); v <- v[is.finite(v)]
  if (length(v) < 2L || abs(v[2]) < 1e-12) return(NA_real_)
  (v[1] - v[2]) / abs(v[2])
}

.htcdi_older_yoy_mean <- function(vals) {
  v <- suppressWarnings(as.numeric(vals)); v <- v[is.finite(v)]
  if (length(v) < 3L) return(NA_real_)
  rates <- (v[seq_len(length(v) - 1L)] - v[-1]) / abs(v[-1])
  rates <- rates[-1]
  rates <- rates[is.finite(rates)]
  if (!length(rates)) return(NA_real_)
  mean(rates)
}

htcdi_statement_metrics <- function(fs) {
  out <- list(rev_yoy = NA_real_, gm_latest = NA_real_, gm_delta = NA_real_,
              capex_rev = NA_real_, capex_vs_own = NA_real_, rev_yoy_vs_own = NA_real_)
  is_df <- .htcdi_fin_df(fs, "Income Statement")
  cf_df <- .htcdi_fin_df(fs, "Cash Flow")
  rev <- .htcdi_metric_row(is_df, c("^Total Revenue$", "Total Revenue", "Operating Revenue", "^Revenue$"))
  gp <- .htcdi_metric_row(is_df, c("^Gross Profit$", "Gross Profit"))
  capex <- .htcdi_metric_row(cf_df, if (exists("CAPEX_PATTERNS")) CAPEX_PATTERNS else c("^Capital Expenditure$", "Capital Expenditure"))
  out$rev_yoy <- .htcdi_latest_yoy(rev)
  older <- .htcdi_older_yoy_mean(rev)
  if (.finite1(out$rev_yoy) && .finite1(older)) out$rev_yoy_vs_own <- out$rev_yoy - older
  gm <- if (length(rev) >= 1L && length(gp) >= 1L) {
    n <- min(length(rev), length(gp))
    ifelse(is.finite(rev[seq_len(n)]) & abs(rev[seq_len(n)]) > 1e-12, gp[seq_len(n)] / rev[seq_len(n)], NA_real_)
  } else numeric(0)
  gm <- gm[is.finite(gm)]
  if (length(gm) >= 1L) out$gm_latest <- gm[1]
  if (length(gm) >= 2L) out$gm_delta <- gm[1] - gm[2]
  cr <- if (length(rev) >= 1L && length(capex) >= 1L) {
    n <- min(length(rev), length(capex))
    ifelse(is.finite(rev[seq_len(n)]) & abs(rev[seq_len(n)]) > 1e-12,
           abs(capex[seq_len(n)]) / abs(rev[seq_len(n)]), NA_real_)
  } else numeric(0)
  cr <- cr[is.finite(cr)]
  if (length(cr) >= 1L) out$capex_rev <- cr[1]
  if (length(cr) >= 2L) out$capex_vs_own <- cr[1] - mean(cr[-1])
  out
}

htcdi_raw_weights <- function(cfg = NULL, liquidity_adj = NULL) {
  cfg <- htcdi_load_config(cfg)
  issuers <- htcdi_issuers(cfg)
  ids <- vapply(issuers, function(x) x$id, character(1))
  pri <- vapply(issuers, function(x) as.numeric(x$criticality_prior)[1], numeric(1))
  pri[!is.finite(pri) | pri < 0] <- NA_real_
  # Missing priors are omitted (weight 0), never mean-filled or equal-weighted as a live score.
  if (!any(is.finite(pri)) || sum(pri, na.rm = TRUE) <= 0) {
    raw <- rep(NA_real_, length(ids)); names(raw) <- ids; return(raw)
  }
  pri[!is.finite(pri)] <- 0
  raw <- pri / sum(pri)
  names(raw) <- ids
  cap <- cfg$weighting$liquidity_adj_max %||% 0.20
  if (!is.null(liquidity_adj)) {
    adj <- suppressWarnings(as.numeric(liquidity_adj[ids])); names(adj) <- ids
    adj[!is.finite(adj)] <- 0; adj <- pmin(pmax(adj, -cap), cap)
    raw <- raw * (1 + adj); raw <- raw / sum(raw)
  }
  raw
}

htcdi_constrain_weights <- function(raw, cfg = NULL) {
  cfg <- htcdi_load_config(cfg)
  raw <- suppressWarnings(as.numeric(raw))
  ids <- names(raw)
  if (is.null(ids) || !length(ids)) { ids <- htcdi_issuer_ids(cfg); raw <- htcdi_raw_weights(cfg) }
  names(raw) <- ids
  raw[!is.finite(raw) | raw < 0] <- 0
  if (sum(raw) <= 0) {
    w <- raw; w[] <- NA_real_
    issuers0 <- htcdi_issuers(cfg)
    layer_of0 <- stats::setNames(vapply(issuers0, function(x) as.character(x$layer)[1], character(1)),
                                 vapply(issuers0, function(x) x$id, character(1)))
    return(list(raw = raw, constrained = w, layer = layer_of0[ids]))
  }
  raw <- raw / sum(raw)
  issuer_cap <- cfg$weighting$issuer_cap %||% 0.12
  exceptions <- cfg$weighting$issuer_cap_exceptions %||% numeric(0)
  layer_cap <- cfg$weighting$layer_cap %||% 0.25
  issuers <- htcdi_issuers(cfg)
  layer_of <- stats::setNames(vapply(issuers, function(x) as.character(x$layer)[1], character(1)),
                              vapply(issuers, function(x) x$id, character(1)))
  .issuer_cap <- function(id) {
    if (id %in% names(exceptions) && is.finite(exceptions[[id]])) as.numeric(exceptions[[id]])[1] else issuer_cap
  }
  w <- raw
  for (iter in seq_len(24L)) {
    changed <- FALSE
    for (id in ids) if (w[[id]] > .issuer_cap(id) + 1e-12) { w[[id]] <- .issuer_cap(id); changed <- TRUE }
    for (ly in unique(unname(layer_of[ids]))) {
      mem <- ids[layer_of[ids] == ly]; s <- sum(w[mem])
      if (s > layer_cap + 1e-12) { w[mem] <- w[mem] * (layer_cap / s); changed <- TRUE }
    }
    leftover <- 1 - sum(w)
    if (leftover > 1e-10) {
      room <- vapply(ids, function(id) max(0, .issuer_cap(id) - w[[id]]), numeric(1))
      for (ly in unique(unname(layer_of[ids]))) {
        mem <- ids[layer_of[ids] == ly]
        ly_room <- max(0, layer_cap - sum(w[mem]))
        if (ly_room <= 0) room[mem] <- 0 else room[mem] <- room[mem] * min(1, ly_room / max(sum(room[mem]), 1e-12))
      }
      if (sum(room) > 1e-12) w <- w + leftover * room / sum(room) else w <- w / sum(w)
    } else if (sum(w) > 0) w <- w / sum(w)
    if (!changed && abs(sum(w) - 1) < 1e-9) break
  }
  list(raw = raw, constrained = w / sum(w), layer = layer_of[ids])
}

htcdi_statement_development <- function(issuer_rows, cfg = NULL) {
  cfg <- htcdi_load_config(cfg); sc <- cfg$score_scales; w <- cfg$statement_weights
  scores <- vapply(issuer_rows, function(r) {
    pillars <- c(rev_yoy = .htcdi_from_signed(r$rev_yoy, sc$rev_yoy),
                 gm_delta = .htcdi_from_signed(r$gm_delta, sc$gm_delta),
                 capex_vs_own = .htcdi_from_signed(r$capex_vs_own, sc$capex_vs_own))
    .htcdi_mean_excl_na(pillars, w[names(pillars)])
  }, numeric(1))
  wt <- vapply(issuer_rows, function(r) as.numeric(r$weight)[1], numeric(1))
  list(issuer = scores, index = .htcdi_mean_excl_na(scores, wt))
}

htcdi_market_vs_benchmark <- function(issuer_rows, cfg = NULL) {
  cfg <- htcdi_load_config(cfg); sc <- cfg$score_scales; w <- cfg$market_weights
  scores <- vapply(issuer_rows, function(r) {
    ex <- r$excess_1y; if (!.finite1(ex)) ex <- r$abnormal_return
    absr <- r$ret_1y; if (!.finite1(absr)) absr <- r$ret_1m
    pillars <- c(excess = .htcdi_from_signed(ex, sc$excess),
                 absolute = .htcdi_from_signed(absr, sc$absolute))
    .htcdi_mean_excl_na(pillars, w[names(pillars)])
  }, numeric(1))
  wt <- vapply(issuer_rows, function(r) as.numeric(r$weight)[1], numeric(1))
  list(issuer = scores, index = .htcdi_mean_excl_na(scores, wt))
}

htcdi_influence_vs_market <- function(issuer_rows, cfg = NULL) {
  cfg <- htcdi_load_config(cfg); sc <- cfg$score_scales; w <- cfg$influence_weights
  scores <- vapply(issuer_rows, function(r) {
    b <- r$beta_60d
    beta_s <- if (.finite1(b)) .htcdi_clip(50 + (as.numeric(b)[1] - 1) * 50, 0, 100) else NA_real_
    ex <- r$excess_1y; if (!.finite1(ex)) ex <- r$abnormal_return
    pillars <- c(beta = beta_s, excess = .htcdi_from_signed(ex, sc$excess))
    .htcdi_mean_excl_na(pillars, w[names(pillars)])
  }, numeric(1))
  wt <- vapply(issuer_rows, function(r) as.numeric(r$weight)[1], numeric(1))
  list(issuer = scores, index = .htcdi_mean_excl_na(scores, wt))
}

htcdi_trajectory_vs_history <- function(issuer_rows, cfg = NULL) {
  cfg <- htcdi_load_config(cfg); sc <- cfg$score_scales; w <- cfg$trajectory_weights
  scores <- vapply(issuer_rows, function(r) {
    pillars <- c(price_vs_hist = .htcdi_from_signed(r$price_vs_hist, sc$price_vs_hist),
                 rev_yoy_vs_own = .htcdi_from_signed(r$rev_yoy_vs_own, sc$rev_yoy_vs_own))
    .htcdi_mean_excl_na(pillars, w[names(pillars)])
  }, numeric(1))
  wt <- vapply(issuer_rows, function(r) as.numeric(r$weight)[1], numeric(1))
  list(issuer = scores, index = .htcdi_mean_excl_na(scores, wt))
}

htcdi_composite <- function(stmt, mkt, inf, traj, cfg = NULL) {
  cfg <- htcdi_load_config(cfg); w <- cfg$composite
  vals <- c(
    statement_development = stmt,
    market_vs_benchmark = mkt,
    influence_vs_market = inf,
    trajectory_vs_history = traj
  )
  dropped <- names(vals)[!is.finite(vals)]
  used <- names(vals)[is.finite(vals)]
  ww <- if (length(used)) w[used] else numeric(0)
  if (length(ww) && sum(ww, na.rm = TRUE) > 0) ww <- ww / sum(ww)
  out <- .htcdi_mean_excl_na(vals, w[names(vals)])
  attr(out, "dropped") <- dropped
  attr(out, "used") <- used
  attr(out, "used_weights") <- ww
  attr(out, "terms") <- vals
  out
}

.htcdi_formula_text <- function(comp) {
  base <- "HTCDI = 0.30·Stmt + 0.25·Mkt + 0.25·Inf + 0.20·Traj. Stmt = statements; Mkt = vs benchmark; Inf = β and excess vs market; Traj = vs own history."
  dropped <- attr(comp, "dropped")
  used_w <- attr(comp, "used_weights")
  extra <- " Unscored terms are omitted and leftover weights scaled up; no 0/50/100 fill."
  if (length(dropped)) {
    bits <- paste(dropped, collapse = ", ")
    wtxt <- if (length(used_w)) paste(sprintf("%s=%.3f", names(used_w), as.numeric(used_w)), collapse = ", ") else ""
    paste0(base, extra, " Dropped: ", bits, if (nzchar(wtxt)) paste0("; leftover weights ", wtxt) else "", ".")
  } else paste0(base, extra, " Statement, Market, Influence, and Trajectory can all be scored.")
}

.htcdi_has_live_observation <- function(rows) {
  if (is.null(rows) || !length(rows)) return(FALSE)
  any(vapply(rows, function(r) {
    .finite1(r$rev_yoy) || .finite1(r$gm_delta) || .finite1(r$capex_vs_own) ||
      .finite1(r$ret_1d) || .finite1(r$ret_1m) || .finite1(r$ret_1y) || .finite1(r$ret_ytd) ||
      .finite1(r$excess_1y) || .finite1(r$abnormal_return) || .finite1(r$beta_60d) ||
      .finite1(r$price_vs_hist) || .finite1(r$rev_yoy_vs_own) ||
      .finite1(r$vol_20d) || .finite1(r$max_dd) ||
      .finite1(r$financial_resilience) || .finite1(r$data_confidence)
  }, logical(1)))
}

.htcdi_issuer_combined <- function(row) {
  .htcdi_mean_excl_na(c(
    .htcdi_pillar(row$stmt_score), .htcdi_pillar(row$mkt_score),
    .htcdi_pillar(row$inf_score), .htcdi_pillar(row$traj_score)
  ))
}

htcdi_issuer_cooling <- function(row, cfg = NULL) {
  cfg <- htcdi_load_config(cfg)
  thr <- cfg$alerts$cooling_score %||% 45
  s <- .htcdi_issuer_combined(row)
  if (!.finite1(s)) {
    s <- .htcdi_mean_excl_na(c(
      .htcdi_from_signed(if (.finite1(row$excess_1y)) row$excess_1y else row$abnormal_return, 0.30),
      .htcdi_from_signed(row$price_vs_hist, 0.50),
      .htcdi_from_signed(row$rev_yoy, 0.20)
    ))
  }
  is.finite(s) && s < thr
}

htcdi_contagion <- function(issuer_rows, cfg = NULL) {
  cfg <- htcdi_load_config(cfg)
  rows <- issuer_rows
  names(rows) <- vapply(rows, function(r) r$id, character(1))
  cooling <- vapply(rows, function(r) isTRUE(htcdi_issuer_cooling(r, cfg)), logical(1))
  evaluable <- vapply(rows, function(r) {
    is.finite(.htcdi_issuer_combined(r)) || .finite1(r$excess_1y) || .finite1(r$abnormal_return) ||
      .finite1(r$rev_yoy) || .finite1(r$price_vs_hist) || .finite1(r$ret_1m)
  }, logical(1))
  if (!any(evaluable)) {
    return(list(penalty = NA_real_, channels = character(0), persistent_issuers = character(0),
                system_critical_ok = FALSE, unevaluated = TRUE))
  }
  channels_hit <- character(0); penalty <- 0
  for (ch in cfg$contagion_channels) {
    ids <- ch$issuers
    if (is.null(ids) && !is.null(ch$layers)) ids <- unique(unlist(cfg$layers[ch$layers], use.names = FALSE))
    ids <- intersect(as.character(ids), names(rows))
    min_n <- as.integer(ch$min_stressed %||% 2L)
    if (length(ids) >= min_n && sum(cooling[ids], na.rm = TRUE) >= min_n) {
      channels_hit <- c(channels_hit, ch$id); penalty <- penalty + 8
    }
  }
  list(penalty = .htcdi_clip(penalty, 0, 100), channels = unique(channels_hit),
       persistent_issuers = names(cooling)[cooling], system_critical_ok = length(channels_hit) > 0,
       unevaluated = FALSE)
}

htcdi_alert_level <- function(composite, contagion = NULL, cfg = NULL) {
  cfg <- htcdi_load_config(cfg)
  cscore <- suppressWarnings(as.numeric(composite)[1])
  if (!is.finite(cscore)) return("Unavailable")
  if (cscore < (cfg$alerts$cooling_min %||% 40)) return("Contracting")
  if (cscore < (cfg$alerts$steady_min %||% 55)) return("Cooling")
  if (cscore < (cfg$alerts$expanding_min %||% 70)) return("Steady")
  "Expanding"
}

# Issuer row: structural config fields (criticality, replacement, buffer) stay;
# live statement/market pillars are NA until an input or derived statistic exists.
.htcdi_issuer_row <- function(iss, weight, input = NULL) {
  inp <- input %||% list()
  fin <- .htcdi_opt(inp$financial_resilience)
  ops <- .htcdi_opt(inp$operational_continuity)
  if (isTRUE(inp$ops_incident_persistent)) {
    ops <- if (.finite1(ops)) min(as.numeric(ops)[1], 35) else 35
  } else if (isTRUE(inp$ops_incident)) {
    ops <- if (.finite1(ops)) min(as.numeric(ops)[1], 55) else 55
  }
  supply_h <- .htcdi_opt(inp$supply_chain_resilience)
  vol_s <- .htcdi_opt(inp$abnormal_vol)
  if (!.finite1(vol_s) && .finite1(inp$vol_20d)) {
    vol_s <- .htcdi_clip((as.numeric(inp$vol_20d)[1] / 0.35) * 50, 0, 100)
  }
  dd_s <- .htcdi_opt(inp$drawdown_stress)
  if (!.finite1(dd_s) && .finite1(inp$max_dd)) {
    dd_s <- .htcdi_clip(abs(as.numeric(inp$max_dd)[1]) * 200, 0, 100)
  }
  ops_s <- .htcdi_opt(inp$ops_incident_stress)
  if (!.finite1(ops_s)) {
    if (isTRUE(inp$ops_incident_persistent)) ops_s <- 80
    else if (isTRUE(inp$ops_incident)) ops_s <- 45
  }
  fin_d <- .htcdi_opt(inp$financial_deterioration)
  if (!.finite1(fin_d) && .finite1(fin)) fin_d <- .htcdi_clip(100 - as.numeric(fin)[1], 0, 100)
  sup_d <- .htcdi_opt(inp$supply_disruption)
  if (!.finite1(sup_d) && .finite1(supply_h)) sup_d <- .htcdi_clip(100 - as.numeric(supply_h)[1], 0, 100)
  sub_s <- .htcdi_opt(inp$substitute_scarcity)
  if (!.finite1(sub_s)) sub_s <- .htcdi_opt(iss$switching_difficulty)
  list(
    id = iss$id, tickers = iss$tickers, economic_issuer = iss$economic_issuer,
    function_id = iss$function_id, layer = iss$layer, criticality_prior = iss$criticality_prior,
    weight_raw = NA_real_, weight = weight, substitutes = iss$substitutes,
    replacement_time_years = iss$replacement_time_years, switching_difficulty = iss$switching_difficulty,
    buffer = iss$buffer, financial_resilience = fin, operational_continuity = ops,
    supply_chain_resilience = supply_h, market_stability = .htcdi_opt(inp$market_stability),
    data_confidence = .htcdi_opt(inp$data_confidence), abnormal_vol = vol_s, drawdown_stress = dd_s,
    ops_incident_stress = ops_s, ops_incident = isTRUE(inp$ops_incident),
    ops_incident_persistent = isTRUE(inp$ops_incident_persistent),
    financial_deterioration = fin_d, supply_disruption = sup_d,
    substitute_scarcity = sub_s,
    ret_1d = .htcdi_opt(inp$ret_1d), ret_1m = .htcdi_opt(inp$ret_1m), ret_ytd = .htcdi_opt(inp$ret_ytd),
    vol_20d = .htcdi_opt(inp$vol_20d), vol_60d = .htcdi_opt(inp$vol_60d),
    max_dd = .htcdi_opt(inp$max_dd), dd_duration = {
      d <- suppressWarnings(as.integer(inp$dd_duration)[1])
      if (length(d) != 1L || !is.finite(d)) NA_integer_ else d
    },
    beta_60d = .htcdi_opt(inp$beta_60d), beta_252d = .htcdi_opt(inp$beta_252d),
    beta_instability = .htcdi_opt(inp$beta_instability), abnormal_return = .htcdi_opt(inp$abnormal_return),
    ret_1y = .htcdi_opt(inp$ret_1y), excess_1y = .htcdi_opt(inp$excess_1y),
    price_vs_hist = .htcdi_opt(inp$price_vs_hist),
    rev_yoy = .htcdi_opt(inp$rev_yoy), gm_latest = .htcdi_opt(inp$gm_latest),
    gm_delta = .htcdi_opt(inp$gm_delta), capex_rev = .htcdi_opt(inp$capex_rev),
    capex_vs_own = .htcdi_opt(inp$capex_vs_own), rev_yoy_vs_own = .htcdi_opt(inp$rev_yoy_vs_own),
    stmt_score = NA_real_, mkt_score = NA_real_, inf_score = NA_real_, traj_score = NA_real_,
    volume_z = .htcdi_opt(inp$volume_z), stress_duration_days = .htcdi_opt(inp$stress_duration_days),
    issuer_stress = .htcdi_opt(inp$issuer_stress), failures = inp$failures %||% list(),
    quote_currency = iss$quote_currency, statement_currency = iss$statement_currency,
    instrument_type = iss$instrument_type, alignment_code = inp$alignment_code %||% NULL
  )
}

htcdi_score <- function(issuer_inputs = NULL, cfg = NULL, liquidity_adj = NULL) {
  cfg <- htcdi_load_config(cfg)
  issuers <- htcdi_issuers(cfg)
  ids <- vapply(issuers, function(x) x$id, character(1))
  raw <- htcdi_raw_weights(cfg, liquidity_adj)
  wt <- htcdi_constrain_weights(raw, cfg)
  rows <- vector("list", length(issuers)); failures <- list()
  for (i in seq_along(issuers)) {
    iss <- issuers[[i]]
    inp <- if (!is.null(issuer_inputs)) issuer_inputs[[iss$id]] else NULL
    if (is.null(inp) && !is.null(issuer_inputs)) {
      for (tk in iss$tickers) if (!is.null(issuer_inputs[[tk]])) { inp <- issuer_inputs[[tk]]; break }
    }
    row <- .htcdi_issuer_row(iss, wt$constrained[[iss$id]], inp)
    row$weight_raw <- wt$raw[[iss$id]]
    if (!is.null(inp$price_df) || !is.null(inp$closes)) {
      pdf <- inp$price_df
      if (is.null(pdf) && !is.null(inp$closes)) {
        n <- length(inp$closes)
        pdf <- data.frame(Date = inp$dates %||% seq.Date(as.Date("2024-01-01"), by = "day", length.out = n),
                          Close = inp$closes, Volume = inp$volume %||% rep(NA_real_, n))
      }
      sig <- htcdi_market_signals(pdf, inp$bench_df %||% NULL, inp$as_of)
      for (nm in names(sig)) {
        if (identical(nm, "failures")) row$failures <- c(row$failures, sig$failures)
        else if (is.null(inp[[nm]]) || (is.atomic(inp[[nm]]) && all(is.na(inp[[nm]])))) row[[nm]] <- sig[[nm]]
      }
      if (!.finite1(row$abnormal_vol) && .finite1(sig$vol_20d)) row$abnormal_vol <- .htcdi_clip((sig$vol_20d / 0.35) * 50, 0, 100)
      if (!.finite1(row$drawdown_stress) && .finite1(sig$max_dd)) row$drawdown_stress <- .htcdi_clip(abs(sig$max_dd) * 200, 0, 100)
    }
    fs <- inp$financials %||% NULL
    if (!is.null(fs)) {
      st <- htcdi_statement_metrics(fs)
      for (nm in names(st)) {
        if (!.finite1(row[[nm]]) && .finite1(st[[nm]])) row[[nm]] <- st[[nm]]
      }
    }
    if (!is.null(row$alignment_code)) failures[[paste0(iss$id, ".align")]] <- row$alignment_code
    if (length(row$failures)) failures[[iss$id]] <- row$failures
    rows[[i]] <- row
  }
  names(rows) <- ids
  stmt <- htcdi_statement_development(rows, cfg)
  mkt <- htcdi_market_vs_benchmark(rows, cfg)
  inf <- htcdi_influence_vs_market(rows, cfg)
  traj <- htcdi_trajectory_vs_history(rows, cfg)
  for (i in seq_along(ids)) {
    id <- ids[[i]]
    rows[[id]]$stmt_score <- stmt$issuer[[i]]
    rows[[id]]$mkt_score <- mkt$issuer[[i]]
    rows[[id]]$inf_score <- inf$issuer[[i]]
    rows[[id]]$traj_score <- traj$issuer[[i]]
    comb <- .htcdi_issuer_combined(rows[[id]])
    if (!.finite1(rows[[id]]$issuer_stress)) rows[[id]]$issuer_stress <- comb
  }
  contagion <- htcdi_contagion(rows, cfg)
  comp <- htcdi_composite(stmt$index, mkt$index, inf$index, traj$index, cfg)
  if (.finite1(comp) && .finite1(contagion$penalty) && contagion$penalty > 0) {
    attrs <- attributes(comp)
    comp <- .htcdi_clip(as.numeric(comp)[1] - as.numeric(contagion$penalty)[1], 0, 100)
    attributes(comp) <- attrs
  }
  live_ok <- isTRUE(.htcdi_has_live_observation(rows)) &&
    any(is.finite(c(stmt$index, mkt$index, inf$index, traj$index)))
  if (!isTRUE(live_ok)) {
    failures$composite <- "SOURCE_HISTORY_UNAVAILABLE"
    dropped <- unique(c(attr(comp, "dropped"), c("statement_development", "market_vs_benchmark",
                                                 "influence_vs_market", "trajectory_vs_history")))
    used_w <- attr(comp, "used_weights")
    comp <- NA_real_
    attr(comp, "dropped") <- dropped
    attr(comp, "used") <- character(0)
    attr(comp, "used_weights") <- numeric(0)
    attr(comp, "reason") <- "SOURCE_HISTORY_UNAVAILABLE"
    attr(comp, "used_weights_pre_gate") <- used_w
  }
  alert <- htcdi_alert_level(comp, contagion, cfg)
  layer_dev <- list()
  for (ly in names(cfg$layers)) {
    mem <- intersect(as.character(cfg$layers[[ly]]), ids)
    if (!length(mem)) next
    layer_dev[[ly]] <- .htcdi_mean_excl_na(
      vapply(mem, function(id) .htcdi_issuer_combined(rows[[id]]), numeric(1)),
      vapply(mem, function(id) rows[[id]]$weight, numeric(1)))
  }
  weakest_layer <- if (length(layer_dev) && any(is.finite(unlist(layer_dev)))) {
    names(layer_dev)[[which.min(replace(unlist(layer_dev), !is.finite(unlist(layer_dev)), Inf))]]
  } else NA_character_
  contrib <- vapply(ids, function(id) {
    w <- rows[[id]]$weight; s <- .htcdi_issuer_combined(rows[[id]])
    if (!.finite1(w) || !.finite1(s)) return(NA_real_)
    as.numeric(w) * as.numeric(s)
  }, numeric(1))
  names(contrib) <- ids
  top <- names(sort(contrib, decreasing = TRUE, na.last = TRUE))
  list(
    composite = unname(as.numeric(comp)[1]), alert = alert,
    availability = if (isTRUE(live_ok)) "ok" else "unavailable",
    dropped_terms = attr(comp, "dropped") %||% character(0),
    used_weights = attr(comp, "used_weights") %||% numeric(0),
    indices = list(
      statement_development = stmt$index, market_vs_benchmark = mkt$index,
      influence_vs_market = inf$index, trajectory_vs_history = traj$index,
      systems_health = stmt$index, systemic_stress = traj$index,
      concentration_fragility = inf$index, market_observation = mkt$index
    ),
    index_weights = cfg$composite, weights = wt, issuers = rows,
    issuer_health = stats::setNames(stmt$issuer, ids),
    issuer_stress = stats::setNames(traj$issuer, ids),
    issuer_fragility = stats::setNames(inf$issuer, ids),
    issuer_market = stats::setNames(mkt$issuer, ids),
    contagion = contagion, highest_risk_layer = weakest_layer, layer_stress = layer_dev,
    top_contributors = top[seq_len(min(5L, length(top)))], failures = failures,
    formula = .htcdi_formula_text(comp),
    role = as.character(cfg$meta$role %||% "civilization_destruction_reading")[1]
  )
}

htcdi_live_inputs_from_prices <- function(price_map = NULL, bench_df = NULL, cfg = NULL, fs_map = NULL) {
  cfg <- htcdi_load_config(cfg); out <- list()
  for (iss in htcdi_issuers(cfg)) {
    pdf <- NULL
    for (tk in iss$tickers) if (!is.null(price_map[[tk]])) { pdf <- price_map[[tk]]; break }
    inp <- list(bench_df = bench_df)
    if (!is.null(pdf) && is.data.frame(pdf) && nrow(pdf) >= 2L && ("Close" %in% names(pdf))) {
      inp$price_df <- pdf
      ok <- mean(is.finite(suppressWarnings(as.numeric(pdf$Close))))
      if (is.finite(ok)) inp$data_confidence <- .htcdi_clip(100 * ok, 0, 100)
    } else {
      inp$failures <- list(history = "SOURCE_HISTORY_UNAVAILABLE")
    }
    fs <- NULL
    if (!is.null(fs_map)) {
      fs <- fs_map[[iss$id]]
      if (is.null(fs)) for (tk in iss$tickers) if (!is.null(fs_map[[tk]])) { fs <- fs_map[[tk]]; break }
    }
    if (!is.null(fs)) inp$financials <- fs
    out[[iss$id]] <- inp
  }
  out
}
