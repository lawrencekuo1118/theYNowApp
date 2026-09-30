# HCCSI scoring — observation tool. Price is never treated as health.
# Reuses setup.R FX / ADR classifiers. Missing optional data is excluded.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

HCCSI_ERROR_CODES <- c(
  "STATEMENT_CURRENCY_UNAVAILABLE", "REQUIRED_FX_RATE_MISSING",
  "REQUIRED_FX_RATE_INVALID", "FX_DATE_MISMATCH",
  "APPLICABLE_ADR_RATIO_MISSING", "APPLICABLE_ADR_RATIO_INVALID",
  "REQUIRED_PER_SHARE_VALUE_NON_FINITE", "BENCHMARK_DATA_MISSING",
  "INSUFFICIENT_ROLLING_WINDOW", "STALE_SOURCE_DATA", "DUPLICATE_ECONOMIC_ISSUER"
)
HCCSI_ALERT_LEVELS <- c("Normal", "Watch", "Warning", "Critical")
.HCCSI_CALC_LABELS <- c(
  per_share = "per-share alignment", returns = "return / volume",
  vol = "realized volatility", drawdown = "max drawdown", beta = "rolling beta",
  health = "Systems Health Index", stress = "Systemic Stress Index",
  fragility = "Concentration and Fragility Index",
  market = "Market Observation Index", composite = "HCCSI composite"
)

.finite1 <- function(x) {
  num <- suppressWarnings(as.numeric(x)[1])
  length(num) == 1L && is.finite(num)
}
.hccsi_clip <- function(x, lo, hi) {
  if (!.finite1(x)) return(NA_real_)
  min(max(as.numeric(x)[1], lo), hi)
}
.hccsi_mean_excl_na <- function(vals, wts = NULL) {
  v <- suppressWarnings(as.numeric(vals))
  w <- if (is.null(wts)) rep(1, length(v)) else suppressWarnings(as.numeric(wts))
  if (length(w) != length(v)) w <- rep(1, length(v))
  ok <- is.finite(v) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(v[ok] * w[ok]) / sum(w[ok])
}
.hccsi_pillar <- function(value) if (.finite1(value)) .hccsi_clip(value, 0, 100) else NA_real_

hccsi_classify_alignment <- function(source_currency, target_currency,
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

hccsi_per_share <- function(equity, shares, equity_ccy, to_ccy, usd_twd = NULL,
                            instrument_type = "ordinary",
                            underlying_share_comparison_required = FALSE,
                            statement_ccy = equity_ccy, share_method = NULL, adr_ratio = NULL) {
  need_adr <- identical(toupper(as.character(instrument_type %||% "")[1]), "ADR") &&
    isTRUE(underlying_share_comparison_required)
  method <- share_method %||% if (isTRUE(need_adr)) "market_cap_per_price" else "balance_sheet"
  code <- hccsi_classify_alignment(
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

hccsi_toast_key <- function(code) {
  cde <- as.character(code %||% "")[1]
  if (!nzchar(cde) || is.na(cde)) return(NA_character_)
  switch(cde,
    STATEMENT_CURRENCY_UNAVAILABLE = "notif_hccsi_statement_ccy",
    REQUIRED_FX_RATE_MISSING = "notif_hccsi_fx_missing",
    REQUIRED_FX_RATE_INVALID = "notif_hccsi_fx_invalid",
    FX_DATE_MISMATCH = "notif_hccsi_fx_date",
    APPLICABLE_ADR_RATIO_MISSING = "notif_hccsi_adr_missing",
    APPLICABLE_ADR_RATIO_INVALID = "notif_hccsi_adr_invalid",
    REQUIRED_PER_SHARE_VALUE_NON_FINITE = "notif_hccsi_per_share_bad",
    BENCHMARK_DATA_MISSING = "notif_hccsi_bench_missing",
    INSUFFICIENT_ROLLING_WINDOW = "notif_hccsi_window",
    STALE_SOURCE_DATA = "notif_hccsi_stale",
    DUPLICATE_ECONOMIC_ISSUER = "notif_hccsi_dup_issuer",
    NA_character_)
}

hccsi_failure_toast <- function(code, blocked_calc = "per_share",
                                available_calcs = c("returns", "vol"), locale = "en") {
  cde <- as.character(code %||% "")[1]
  if (!nzchar(cde) || is.na(cde)) return(NA_character_)
  key <- hccsi_toast_key(cde)
  blocked <- .HCCSI_CALC_LABELS[[blocked_calc]] %||% blocked_calc
  remain <- paste(vapply(available_calcs, function(k) .HCCSI_CALC_LABELS[[k]] %||% k, character(1)), collapse = "; ")
  if (exists("ui_str", mode = "function") && !is.na(key)) {
    base <- tryCatch(ui_str(key, locale), error = function(e) key)
    why <- tryCatch(ui_str("notif_hccsi_why_required", locale), error = function(e) "")
    blk <- gsub("{calc}", blocked, tryCatch(ui_str("notif_hccsi_blocked", locale), error = function(e) "Blocked: {calc}."), fixed = TRUE)
    av <- gsub("{calcs}", remain, tryCatch(ui_str("notif_hccsi_available", locale), error = function(e) "Still available: {calcs}."), fixed = TRUE)
    return(paste(base, why, blk, av))
  }
  paste0(cde, " — required only for the blocked calculation. Blocked: ", blocked, ". Still available: ", remain, ".")
}

hccsi_period_return <- function(closes, n = NULL) {
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

hccsi_realized_vol <- function(closes, window = 20L) {
  x <- suppressWarnings(as.numeric(closes)); x <- x[is.finite(x)]
  if (length(x) < window + 1L) return(list(vol = NA_real_, code = "INSUFFICIENT_ROLLING_WINDOW"))
  r <- diff(tail(x, window + 1L)) / head(tail(x, window + 1L), -1)
  r <- r[is.finite(r)]
  if (length(r) < max(5L, floor(window / 2))) return(list(vol = NA_real_, code = "INSUFFICIENT_ROLLING_WINDOW"))
  list(vol = stats::sd(r) * sqrt(252), code = NULL)
}

hccsi_max_drawdown <- function(closes) {
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

hccsi_rolling_beta <- function(stock_close, bench_close, window = 60L) {
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

hccsi_volume_anomaly <- function(volume, window = 20L) {
  v <- suppressWarnings(as.numeric(volume)); v <- v[is.finite(v)]
  if (length(v) < window + 1L) return(NA_real_)
  base <- head(tail(v, window + 1L), window); last <- tail(v, 1)
  mu <- mean(base); sdv <- stats::sd(base)
  if (!is.finite(mu) || mu <= 0 || !is.finite(sdv) || sdv <= 0) return(NA_real_)
  (last - mu) / sdv
}

hccsi_stale_code <- function(dates, as_of = NULL, max_age_days = 7L) {
  if (is.null(dates) || !length(dates)) return("STALE_SOURCE_DATA")
  d <- tryCatch(as.Date(dates), error = function(e) as.Date(character(0))); d <- d[!is.na(d)]
  if (!length(d)) return("STALE_SOURCE_DATA")
  ref <- if (is.null(as_of)) max(d) else tryCatch(as.Date(as_of)[1], error = function(e) max(d))
  if (is.na(ref)) ref <- max(d)
  if (as.numeric(ref - max(d)) > max_age_days) "STALE_SOURCE_DATA" else NULL
}

hccsi_market_signals <- function(price_df = NULL, bench_df = NULL, as_of = NULL) {
  empty <- list(ret_1d = NA_real_, ret_1m = NA_real_, ret_ytd = NA_real_,
                vol_20d = NA_real_, vol_60d = NA_real_, max_dd = NA_real_,
                dd_duration = NA_integer_, beta_60d = NA_real_, beta_252d = NA_real_,
                beta_instability = NA_real_, abnormal_return = NA_real_,
                volume_z = NA_real_, failures = list())
  if (is.null(price_df) || !is.data.frame(price_df) || nrow(price_df) < 2L || !("Close" %in% names(price_df))) return(empty)
  df <- price_df[order(as.Date(price_df$Date)), , drop = FALSE]
  stale <- hccsi_stale_code(df$Date, as_of)
  if (!is.null(stale)) empty$failures$stale <- stale
  px <- df$Close
  empty$ret_1d <- hccsi_period_return(px, 1L)
  empty$ret_1m <- hccsi_period_return(px, 21L)
  if ("Date" %in% names(df)) {
    dts <- as.Date(df$Date)
    y0 <- as.Date(paste0(format(max(dts, na.rm = TRUE), "%Y"), "-01-01"))
    first <- which(dts >= y0)[1]
    if (is.finite(first)) empty$ret_ytd <- hccsi_period_return(px[seq.int(first, length(px))])
  }
  v20 <- hccsi_realized_vol(px, 20L); v60 <- hccsi_realized_vol(px, 60L)
  empty$vol_20d <- v20$vol; empty$vol_60d <- v60$vol
  if (!is.null(v20$code)) empty$failures$vol_20d <- v20$code
  if (!is.null(v60$code)) empty$failures$vol_60d <- v60$code
  dd <- hccsi_max_drawdown(px); empty$max_dd <- dd$max_dd; empty$dd_duration <- dd$duration
  if ("Volume" %in% names(df)) empty$volume_z <- hccsi_volume_anomaly(df$Volume)
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
  b60 <- hccsi_rolling_beta(common$S, common$M, 60L)
  b252 <- hccsi_rolling_beta(common$S, common$M, 252L)
  empty$beta_60d <- b60$beta; empty$beta_instability <- b60$instability; empty$beta_252d <- b252$beta
  if (!is.null(b60$code)) empty$failures$beta <- b60$code
  sr <- hccsi_period_return(common$S, 21L); br <- hccsi_period_return(common$M, 21L)
  if (is.finite(sr) && is.finite(br)) empty$abnormal_return <- sr - br
  empty
}

hccsi_raw_weights <- function(cfg = NULL, liquidity_adj = NULL) {
  cfg <- hccsi_load_config(cfg)
  issuers <- hccsi_issuers(cfg)
  ids <- vapply(issuers, function(x) x$id, character(1))
  pri <- vapply(issuers, function(x) as.numeric(x$criticality_prior)[1], numeric(1))
  pri[!is.finite(pri) | pri < 0] <- NA_real_
  raw <- if (!any(is.finite(pri))) rep(1 / length(ids), length(ids)) else {
    pri[!is.finite(pri)] <- mean(pri, na.rm = TRUE); pri / sum(pri)
  }
  names(raw) <- ids
  cap <- cfg$weighting$liquidity_adj_max %||% 0.20
  if (!is.null(liquidity_adj)) {
    adj <- suppressWarnings(as.numeric(liquidity_adj[ids])); names(adj) <- ids
    adj[!is.finite(adj)] <- 0; adj <- pmin(pmax(adj, -cap), cap)
    raw <- raw * (1 + adj); raw <- raw / sum(raw)
  }
  raw
}

hccsi_constrain_weights <- function(raw, cfg = NULL) {
  cfg <- hccsi_load_config(cfg)
  raw <- suppressWarnings(as.numeric(raw))
  ids <- names(raw)
  if (is.null(ids) || !length(ids)) { ids <- hccsi_issuer_ids(cfg); raw <- hccsi_raw_weights(cfg) }
  names(raw) <- ids
  raw[!is.finite(raw) | raw < 0] <- 0
  if (sum(raw) <= 0) raw[] <- 1 / length(raw)
  raw <- raw / sum(raw)
  issuer_cap <- cfg$weighting$issuer_cap %||% 0.12
  exceptions <- cfg$weighting$issuer_cap_exceptions %||% numeric(0)
  layer_cap <- cfg$weighting$layer_cap %||% 0.25
  issuers <- hccsi_issuers(cfg)
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

hccsi_systems_health <- function(issuer_rows, cfg = NULL) {
  cfg <- hccsi_load_config(cfg); w <- cfg$systems_health_weights
  scores <- vapply(issuer_rows, function(r) {
    pillars <- c(financial_resilience = .hccsi_pillar(r$financial_resilience),
                 operational_continuity = .hccsi_pillar(r$operational_continuity),
                 supply_chain_resilience = .hccsi_pillar(r$supply_chain_resilience),
                 market_stability = .hccsi_pillar(r$market_stability),
                 data_confidence = .hccsi_pillar(r$data_confidence))
    .hccsi_mean_excl_na(pillars, w[names(pillars)])
  }, numeric(1))
  wt <- vapply(issuer_rows, function(r) as.numeric(r$weight)[1], numeric(1))
  list(issuer = scores, index = .hccsi_mean_excl_na(scores, wt))
}

hccsi_systemic_stress <- function(issuer_rows, contagion = 0, cfg = NULL) {
  cfg <- hccsi_load_config(cfg); w <- cfg$systemic_stress_weights
  scores <- vapply(issuer_rows, function(r) {
    pillars <- c(abnormal_vol = .hccsi_pillar(r$abnormal_vol),
                 drawdowns = .hccsi_pillar(r$drawdown_stress),
                 operational_incidents = .hccsi_pillar(r$ops_incident_stress),
                 financial_deterioration = .hccsi_pillar(r$financial_deterioration),
                 supply_chain_disruption = .hccsi_pillar(r$supply_disruption),
                 contagion = .hccsi_pillar(contagion))
    .hccsi_mean_excl_na(pillars, w[names(pillars)])
  }, numeric(1))
  wt <- vapply(issuer_rows, function(r) as.numeric(r$weight)[1], numeric(1))
  list(issuer = scores, index = .hccsi_mean_excl_na(scores, wt))
}

hccsi_concentration_fragility <- function(issuer_rows, cfg = NULL) {
  scores <- vapply(issuer_rows, function(r) {
    repl <- r$replacement_time_years
    repl_s <- if (.finite1(repl)) .hccsi_clip(as.numeric(repl) / 8 * 100, 0, 100) else NA_real_
    inv_buf <- if (.finite1(r$buffer)) .hccsi_clip(100 - as.numeric(r$buffer), 0, 100) else NA_real_
    conc <- if (.finite1(r$weight)) .hccsi_clip(as.numeric(r$weight) / 0.15 * 60, 0, 100) else NA_real_
    .hccsi_mean_excl_na(c(.hccsi_pillar(r$substitute_scarcity), .hccsi_pillar(r$switching_difficulty),
                          repl_s, inv_buf, conc))
  }, numeric(1))
  wt <- vapply(issuer_rows, function(r) as.numeric(r$weight)[1], numeric(1))
  list(issuer = scores, index = .hccsi_mean_excl_na(scores, wt))
}

hccsi_market_observation <- function(issuer_rows) {
  scores <- vapply(issuer_rows, function(r) {
    ret <- r$ret_1m; if (!.finite1(ret)) ret <- r$ret_ytd
    if (!.finite1(ret)) return(NA_real_)
    .hccsi_clip(50 + as.numeric(ret)[1] * 200, 0, 100)
  }, numeric(1))
  wt <- vapply(issuer_rows, function(r) as.numeric(r$weight)[1], numeric(1))
  list(issuer = scores, index = .hccsi_mean_excl_na(scores, wt))
}

hccsi_market_neutral <- function(market_observation) {
  m <- suppressWarnings(as.numeric(market_observation)[1])
  if (!is.finite(m)) return(NA_real_)
  .hccsi_clip(100 - 2 * abs(m - 50), 0, 100)
}

hccsi_composite <- function(health, stress, fragility, market, cfg = NULL) {
  cfg <- hccsi_load_config(cfg); w <- cfg$composite
  vals <- c(
    systems_health = health,
    systemic_stress_inverted = if (.finite1(stress)) 100 - as.numeric(stress)[1] else NA_real_,
    concentration_fragility_inverted = if (.finite1(fragility)) 100 - as.numeric(fragility)[1] else NA_real_,
    market_observation_neutral = hccsi_market_neutral(market)
  )
  .hccsi_mean_excl_na(vals, w[names(vals)])
}

hccsi_issuer_persistent <- function(row, cfg = NULL) {
  cfg <- hccsi_load_config(cfg)
  thr <- cfg$alerts$persist_stress_threshold %||% 65
  days_need <- cfg$alerts$persist_days %||% 20
  stress <- row$issuer_stress
  if (!.finite1(stress)) {
    stress <- .hccsi_mean_excl_na(c(.hccsi_pillar(row$ops_incident_stress),
                                   .hccsi_pillar(row$supply_disruption),
                                   .hccsi_pillar(row$financial_deterioration)))
  }
  dur <- suppressWarnings(as.numeric(row$stress_duration_days)[1]); if (!is.finite(dur)) dur <- 0
  is.finite(stress) && stress >= thr && dur >= days_need
}

hccsi_contagion <- function(issuer_rows, cfg = NULL) {
  cfg <- hccsi_load_config(cfg)
  rows <- issuer_rows
  names(rows) <- vapply(rows, function(r) r$id, character(1))
  persist <- vapply(rows, function(r) isTRUE(hccsi_issuer_persistent(r, cfg)), logical(1))
  channels_hit <- character(0); penalty <- 0
  for (ch in cfg$contagion_channels) {
    ids <- ch$issuers
    if (is.null(ids) && !is.null(ch$layers)) ids <- unique(unlist(cfg$layers[ch$layers], use.names = FALSE))
    ids <- intersect(as.character(ids), names(rows))
    min_n <- as.integer(ch$min_stressed %||% 2L)
    if (length(ids) >= min_n && sum(persist[ids], na.rm = TRUE) >= min_n) {
      channels_hit <- c(channels_hit, ch$id); penalty <- penalty + 18
    }
  }
  list(penalty = .hccsi_clip(penalty, 0, 100), channels = unique(channels_hit),
       persistent_issuers = names(persist)[persist], system_critical_ok = length(channels_hit) > 0)
}

hccsi_alert_level <- function(composite, contagion, cfg = NULL) {
  cfg <- hccsi_load_config(cfg)
  cscore <- suppressWarnings(as.numeric(composite)[1])
  if (!is.finite(cscore)) return("Watch")
  crit_ok <- isTRUE(contagion$system_critical_ok)
  if (cscore < (cfg$alerts$warning_min %||% 40) && isTRUE(crit_ok)) return("Critical")
  if (cscore < (cfg$alerts$warning_min %||% 40)) return("Warning")
  if (cscore < (cfg$alerts$watch_min %||% 55)) return("Warning")
  if (cscore < (cfg$alerts$normal_min %||% 70)) return("Watch")
  "Normal"
}

.hccsi_default_row <- function(iss, weight, input = NULL) {
  inp <- input %||% list()
  fin <- inp$financial_resilience %||% 72; ops <- inp$operational_continuity %||% 72
  if (isTRUE(inp$ops_incident) && !isTRUE(inp$ops_incident_persistent)) ops <- min(as.numeric(ops)[1], 55)
  if (isTRUE(inp$ops_incident_persistent)) ops <- min(as.numeric(ops)[1], 35)
  supply_h <- inp$supply_chain_resilience %||% 70
  vol_s <- inp$abnormal_vol
  if (is.null(vol_s) && .finite1(inp$vol_20d)) vol_s <- .hccsi_clip((as.numeric(inp$vol_20d)[1] / 0.35) * 50, 0, 100)
  dd_s <- inp$drawdown_stress
  if (is.null(dd_s) && .finite1(inp$max_dd)) dd_s <- .hccsi_clip(abs(as.numeric(inp$max_dd)[1]) * 200, 0, 100)
  ops_s <- inp$ops_incident_stress
  if (is.null(ops_s)) ops_s <- if (isTRUE(inp$ops_incident_persistent)) 80 else if (isTRUE(inp$ops_incident)) 45 else 10
  fin_d <- inp$financial_deterioration
  if (is.null(fin_d) && .finite1(inp$financial_resilience)) fin_d <- .hccsi_clip(100 - as.numeric(inp$financial_resilience)[1], 0, 100)
  if (is.null(fin_d)) fin_d <- 20
  sup_d <- inp$supply_disruption %||% if (.finite1(supply_h)) 100 - as.numeric(supply_h)[1] else 20
  list(
    id = iss$id, tickers = iss$tickers, economic_issuer = iss$economic_issuer,
    function_id = iss$function_id, layer = iss$layer, criticality_prior = iss$criticality_prior,
    weight_raw = NA_real_, weight = weight, substitutes = iss$substitutes,
    replacement_time_years = iss$replacement_time_years, switching_difficulty = iss$switching_difficulty,
    buffer = iss$buffer, financial_resilience = fin, operational_continuity = ops,
    supply_chain_resilience = supply_h, market_stability = inp$market_stability %||% 70,
    data_confidence = inp$data_confidence %||% 60, abnormal_vol = vol_s, drawdown_stress = dd_s,
    ops_incident_stress = ops_s, ops_incident = isTRUE(inp$ops_incident),
    ops_incident_persistent = isTRUE(inp$ops_incident_persistent),
    financial_deterioration = fin_d, supply_disruption = sup_d,
    substitute_scarcity = inp$substitute_scarcity %||% (iss$switching_difficulty %||% 70),
    ret_1d = inp$ret_1d %||% NA_real_, ret_1m = inp$ret_1m %||% NA_real_, ret_ytd = inp$ret_ytd %||% NA_real_,
    vol_20d = inp$vol_20d %||% NA_real_, vol_60d = inp$vol_60d %||% NA_real_,
    max_dd = inp$max_dd %||% NA_real_, dd_duration = inp$dd_duration %||% NA_integer_,
    beta_60d = inp$beta_60d %||% NA_real_, beta_252d = inp$beta_252d %||% NA_real_,
    beta_instability = inp$beta_instability %||% NA_real_, abnormal_return = inp$abnormal_return %||% NA_real_,
    volume_z = inp$volume_z %||% NA_real_, stress_duration_days = inp$stress_duration_days %||% 0,
    issuer_stress = inp$issuer_stress %||% NA_real_, failures = inp$failures %||% list(),
    quote_currency = iss$quote_currency, statement_currency = iss$statement_currency,
    instrument_type = iss$instrument_type, alignment_code = inp$alignment_code %||% NULL
  )
}

hccsi_score <- function(issuer_inputs = NULL, cfg = NULL, liquidity_adj = NULL) {
  cfg <- hccsi_load_config(cfg)
  issuers <- hccsi_issuers(cfg)
  ids <- vapply(issuers, function(x) x$id, character(1))
  raw <- hccsi_raw_weights(cfg, liquidity_adj)
  wt <- hccsi_constrain_weights(raw, cfg)
  rows <- vector("list", length(issuers)); failures <- list()
  for (i in seq_along(issuers)) {
    iss <- issuers[[i]]
    inp <- if (!is.null(issuer_inputs)) issuer_inputs[[iss$id]] else NULL
    if (is.null(inp) && !is.null(issuer_inputs)) {
      for (tk in iss$tickers) if (!is.null(issuer_inputs[[tk]])) { inp <- issuer_inputs[[tk]]; break }
    }
    row <- .hccsi_default_row(iss, wt$constrained[[iss$id]], inp)
    row$weight_raw <- wt$raw[[iss$id]]
    if (!is.null(inp$price_df) || !is.null(inp$closes)) {
      pdf <- inp$price_df
      if (is.null(pdf) && !is.null(inp$closes)) {
        n <- length(inp$closes)
        pdf <- data.frame(Date = inp$dates %||% seq.Date(as.Date("2024-01-01"), by = "day", length.out = n),
                          Close = inp$closes, Volume = inp$volume %||% rep(NA_real_, n))
      }
      sig <- hccsi_market_signals(pdf, inp$bench_df %||% NULL, inp$as_of)
      for (nm in names(sig)) {
        if (identical(nm, "failures")) row$failures <- c(row$failures, sig$failures)
        else if (is.null(inp[[nm]]) || (is.atomic(inp[[nm]]) && all(is.na(inp[[nm]])))) row[[nm]] <- sig[[nm]]
      }
      if (!.finite1(row$abnormal_vol) && .finite1(sig$vol_20d)) row$abnormal_vol <- .hccsi_clip((sig$vol_20d / 0.35) * 50, 0, 100)
      if (!.finite1(row$drawdown_stress) && .finite1(sig$max_dd)) row$drawdown_stress <- .hccsi_clip(abs(sig$max_dd) * 200, 0, 100)
    }
    if (!is.null(row$alignment_code)) failures[[paste0(iss$id, ".align")]] <- row$alignment_code
    if (length(row$failures)) failures[[iss$id]] <- row$failures
    rows[[i]] <- row
  }
  names(rows) <- ids
  for (id in ids) {
    r <- rows[[id]]
    loc <- .hccsi_mean_excl_na(c(.hccsi_pillar(r$ops_incident_stress), .hccsi_pillar(r$supply_disruption),
                                 .hccsi_pillar(r$financial_deterioration), .hccsi_pillar(r$abnormal_vol),
                                 .hccsi_pillar(r$drawdown_stress)))
    if (!.finite1(r$issuer_stress)) rows[[id]]$issuer_stress <- loc
  }
  contagion <- hccsi_contagion(rows, cfg)
  health <- hccsi_systems_health(rows, cfg)
  stress <- hccsi_systemic_stress(rows, contagion$penalty, cfg)
  frag <- hccsi_concentration_fragility(rows, cfg)
  mkt <- hccsi_market_observation(rows)
  comp <- hccsi_composite(health$index, stress$index, frag$index, mkt$index, cfg)
  alert <- hccsi_alert_level(comp, contagion, cfg)
  layer_stress <- list()
  for (ly in names(cfg$layers)) {
    mem <- intersect(as.character(cfg$layers[[ly]]), ids)
    if (!length(mem)) next
    layer_stress[[ly]] <- .hccsi_mean_excl_na(
      vapply(mem, function(id) rows[[id]]$issuer_stress, numeric(1)),
      vapply(mem, function(id) rows[[id]]$weight, numeric(1)))
  }
  highest_layer <- if (length(layer_stress) && any(is.finite(unlist(layer_stress)))) {
    names(layer_stress)[[which.max(replace(unlist(layer_stress), !is.finite(unlist(layer_stress)), -Inf))]]
  } else NA_character_
  contrib <- vapply(ids, function(id) {
    w <- rows[[id]]$weight; h <- health$issuer[[which(ids == id)]]
    if (!.finite1(w) || !.finite1(h)) return(NA_real_)
    as.numeric(w) * (100 - as.numeric(h))
  }, numeric(1))
  names(contrib) <- ids
  top <- names(sort(contrib, decreasing = TRUE, na.last = TRUE))
  list(
    composite = comp, alert = alert,
    indices = list(systems_health = health$index, systemic_stress = stress$index,
                   concentration_fragility = frag$index, market_observation = mkt$index,
                   market_observation_neutral = hccsi_market_neutral(mkt$index)),
    index_weights = cfg$composite, weights = wt, issuers = rows,
    issuer_health = stats::setNames(health$issuer, ids),
    issuer_stress = stats::setNames(stress$issuer, ids),
    issuer_fragility = stats::setNames(frag$issuer, ids),
    issuer_market = stats::setNames(mkt$issuer, ids),
    contagion = contagion, highest_risk_layer = highest_layer, layer_stress = layer_stress,
    top_contributors = top[seq_len(min(5L, length(top)))], failures = failures,
    formula = "HCCSI = 0.40·H + 0.30·(100−S) + 0.20·(100−F) + 0.10·M*, M* = 100 − 2·|M − 50| (price is not health)",
    role = "systemic_risk_observation"
  )
}

hccsi_live_inputs_from_prices <- function(price_map = NULL, bench_df = NULL, cfg = NULL) {
  cfg <- hccsi_load_config(cfg); out <- list()
  for (iss in hccsi_issuers(cfg)) {
    pdf <- NULL
    for (tk in iss$tickers) if (!is.null(price_map[[tk]])) { pdf <- price_map[[tk]]; break }
    inp <- list(bench_df = bench_df, data_confidence = if (is.null(pdf)) 35 else 70)
    if (!is.null(pdf)) inp$price_df <- pdf
    out[[iss$id]] <- inp
  }
  out
}
