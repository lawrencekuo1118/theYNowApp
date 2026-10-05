# ==========================================
# macro_bubble_indicators.R — 動態產業泡沫與權重集中度
#
# 獨立於 CAPM／Ke／WACC／個股估值引擎。
# 訂閱全域 market_mode 與 Macro 共用的 industry_key／concept_key；不寫回任何折現率輸入。
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.MACRO_BUBBLE_GICS_SECTOR <- c(
  gics_xlk = "Information Technology",
  gics_xlv = "Health Care",
  gics_xlf = "Financials",
  gics_xle = "Energy",
  gics_xli = "Industrials",
  gics_xly = "Consumer Discretionary",
  gics_xlp = "Consumer Staples",
  gics_xlu = "Utilities",
  gics_xlb = "Materials",
  gics_xlre = "Real Estate",
  gics_xlc = "Communication Services"
)

.MACRO_BUBBLE_MAX_POOL <- 40L
.MACRO_BUBBLE_ENV <- new.env(parent = emptyenv())

macro_bubble_buffett_paths <- function(mode = "US") {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  rel <- if (identical(mode, "TW")) {
    file.path("data", "macro_buffett_tw.csv")
  } else {
    file.path("data", "macro_buffett_us.csv")
  }
  unique(c(rel, file.path(getwd(), rel), file.path("app_18.0", rel)))
}

#' Absolute market-cap / GDP CSV paths (TW fallback when World Bank / DGBAS unavailable).
macro_bubble_buffett_abs_paths <- function(mode = "US") {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  rel <- if (identical(mode, "TW")) {
    file.path("data", "macro_buffett_tw_abs.csv")
  } else {
    file.path("data", "macro_buffett_us_abs.csv")
  }
  unique(c(rel, file.path(getwd(), rel), file.path("app_21.0", rel)))
}

# DGBAS NSTAT (行政院主計總處總體統計資料庫) — Taiwan macro Buffett legs.
# Portal: https://nstatdb.dgbas.gov.tw/dgbasAll/webMain.aspx?sys=100&funid=dgmaind
# SDMX filter uses 1-based field indices + empty 複分類 slots (trailing "..."), then A/M,
# with startTime/endTime as YYYY-00 (annual) or YYYY-MM (monthly, zero-padded).
.MACRO_DGBAS_BASE <- "https://nstatdb.dgbas.gov.tw/dgbasAll/webMain.aspx"
# A110101010 公開發行公司股票發行概況 — field 4 = 上市公司-上市公司市值(十億元)
.MACRO_DGBAS_MCAP_FUN <- "A110101010"
.MACRO_DGBAS_MCAP_FLD <- 4L
# A018101010 國民所得統計常用資料 — 2=平均匯率(元/美元), 5=GDP名目值(百萬美元)
.MACRO_DGBAS_NI_FUN <- "A018101010"
.MACRO_DGBAS_FX_FLD <- 2L
.MACRO_DGBAS_GDP_USD_FLD <- 5L

#' HTTP GET text (httr preferred).
.macro_dgbas_http_get <- function(url, timeout_sec = 25) {
  url <- as.character(url %||% "")[1]
  if (!nzchar(url)) return(NULL)
  tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(url, httr::timeout(timeout_sec))
      if (httr::status_code(resp) >= 400L) return(NULL)
      httr::content(resp, as = "text", encoding = "UTF-8")
    } else {
      con <- url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      paste(readLines(con, warn = FALSE), collapse = "\n")
    }
  }, error = function(e) NULL)
}

#' Parse first non-empty SDMX series observations → named numeric by 0-based index.
.macro_dgbas_obs_vec <- function(parsed) {
  if (!is.list(parsed)) return(NULL)
  sets <- tryCatch(parsed$data$dataSets, error = function(e) NULL)
  if (!is.list(sets) || !length(sets)) return(NULL)
  ser <- sets[[1]]$series
  if (!is.list(ser) || !length(ser)) return(NULL)
  for (nm in names(ser)) {
    obs <- ser[[nm]]$observations
    if (!is.list(obs) || !length(obs)) next
    idx <- suppressWarnings(as.integer(names(obs)))
    vals <- vapply(obs, function(x) {
      if (is.list(x) && length(x) >= 1L) suppressWarnings(as.numeric(x[[1]])[1])
      else suppressWarnings(as.numeric(x)[1])
    }, numeric(1))
    ok <- is.finite(idx) & is.finite(vals)
    if (!any(ok)) next
    out <- vals[ok]
    names(out) <- as.character(idx[ok])
    return(out)
  }
  NULL
}

#' Fetch one DGBAS annual series (field index) as date / value.
macro_dgbas_fetch_annual_field <- function(funid,
                                           field,
                                           start_year = 1995L,
                                           end_year = NA_integer_,
                                           timeout_sec = 25) {
  funid <- toupper(trimws(as.character(funid %||% "")[1]))
  field <- suppressWarnings(as.integer(field)[1])
  sy <- suppressWarnings(as.integer(start_year)[1])
  ey <- suppressWarnings(as.integer(end_year)[1])
  if (!nzchar(funid) || !is.finite(field) || field < 1L || !is.finite(sy)) return(NULL)
  if (!is.finite(ey)) ey <- as.integer(format(Sys.Date(), "%Y"))
  if (ey < sy) return(NULL)
  cache_key <- sprintf("dgbas_%s_%d_%d_%d", funid, field, sy, ey)
  if (is.data.frame(.MACRO_BUBBLE_ENV[[cache_key]])) {
    return(.MACRO_BUBBLE_ENV[[cache_key]])
  }
  # Three dots after field: empty 複分類 slots required by the portal's SDMX builder.
  q <- sprintf(
    "sdmx/%s/%d...A&startTime=%d-00&endTime=%d-00",
    funid, field, sy, ey
  )
  url <- paste0(.MACRO_DGBAS_BASE, "?", q)
  raw <- .macro_dgbas_http_get(url, timeout_sec = timeout_sec)
  if (!nzchar(raw %||% "") || !startsWith(trimws(raw), "{")) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  obs <- .macro_dgbas_obs_vec(parsed)
  if (is.null(obs) || !length(obs)) return(NULL)
  idx <- suppressWarnings(as.integer(names(obs)))
  ord <- order(idx)
  idx <- idx[ord]
  val <- as.numeric(obs[ord])
  # Observation 0 = start_year when structure is omitted (portal quick/API-JSON habit).
  years <- sy + idx
  df <- data.frame(
    date = as.Date(sprintf("%d-12-31", years)),
    value = val,
    source = "dgbas",
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$value) & !is.na(df$date), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  .MACRO_BUBBLE_ENV[[cache_key]] <- df
  df
}

#' Taiwan Buffett abs + ratio from DGBAS NSTAT (listed market cap / GDP).
#' Market cap: A110101010 field 4 (NT$ 十億元); GDP: A018101010 field 5 (US$ million);
#' FX: A018101010 field 2 (TWD per USD) to convert market cap into USD.
macro_bubble_fetch_buffett_dgbas <- function(timeout_sec = 25) {
  cache_key <- "dgbas_buffett_tw_abs"
  if (is.data.frame(.MACRO_BUBBLE_ENV[[cache_key]])) {
    return(.MACRO_BUBBLE_ENV[[cache_key]])
  }
  ey <- as.integer(format(Sys.Date(), "%Y"))
  mcap <- macro_dgbas_fetch_annual_field(
    .MACRO_DGBAS_MCAP_FUN, .MACRO_DGBAS_MCAP_FLD,
    start_year = 1995L, end_year = ey, timeout_sec = timeout_sec
  )
  gdp <- macro_dgbas_fetch_annual_field(
    .MACRO_DGBAS_NI_FUN, .MACRO_DGBAS_GDP_USD_FLD,
    start_year = 1995L, end_year = ey, timeout_sec = timeout_sec
  )
  fx <- macro_dgbas_fetch_annual_field(
    .MACRO_DGBAS_NI_FUN, .MACRO_DGBAS_FX_FLD,
    start_year = 1995L, end_year = ey, timeout_sec = timeout_sec
  )
  if (!is.data.frame(mcap) || !nrow(mcap) || !is.data.frame(gdp) || !nrow(gdp)) {
    return(NULL)
  }
  dates <- sort(unique(c(mcap$date, gdp$date)))
  mc_nt_bn <- mcap$value[match(dates, mcap$date)]
  gd_mil <- gdp$value[match(dates, gdp$date)]
  fx_v <- if (is.data.frame(fx) && nrow(fx)) fx$value[match(dates, fx$date)] else rep(NA_real_, length(dates))
  # Forward/back fill FX gaps lightly (annual series is dense).
  if (any(!is.finite(fx_v)) && any(is.finite(fx_v))) {
    for (i in seq_along(fx_v)) {
      if (!is.finite(fx_v[[i]]) && i > 1L && is.finite(fx_v[[i - 1L]])) fx_v[[i]] <- fx_v[[i - 1L]]
    }
    for (i in rev(seq_along(fx_v))) {
      if (!is.finite(fx_v[[i]]) && i < length(fx_v) && is.finite(fx_v[[i + 1L]])) fx_v[[i]] <- fx_v[[i + 1L]]
    }
  }
  gdp_usd <- gd_mil * 1e6
  mcap_usd <- ifelse(is.finite(mc_nt_bn) & is.finite(fx_v) & fx_v > 0, mc_nt_bn * 1e9 / fx_v, NA_real_)
  ratio_pct <- ifelse(is.finite(mcap_usd) & is.finite(gdp_usd) & gdp_usd > 0,
                      100 * mcap_usd / gdp_usd, NA_real_)
  df <- data.frame(
    date = dates,
    market_cap_usd = as.numeric(mcap_usd),
    gdp_usd = as.numeric(gdp_usd),
    ratio_pct = as.numeric(ratio_pct),
    source = "dgbas",
    stringsAsFactors = FALSE
  )
  # Drop placeholder / unpublished years (API may pad with 0).
  keep <- (is.finite(df$market_cap_usd) & df$market_cap_usd > 0) |
    (is.finite(df$gdp_usd) & df$gdp_usd > 0)
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  .MACRO_BUBBLE_ENV[[cache_key]] <- df
  df
}

macro_bubble_read_buffett_abs_csv <- function(mode = "US") {
  paths <- macro_bubble_buffett_abs_paths(mode)
  hit <- paths[file.exists(paths)]
  if (!length(hit)) return(NULL)
  d <- tryCatch(
    utils::read.csv(hit[[1]], stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d)) return(NULL)
  need <- c("date", "market_cap_usd", "gdp_usd")
  if (!all(need %in% names(d))) return(NULL)
  d$date <- as.Date(d$date)
  d$market_cap_usd <- suppressWarnings(as.numeric(d$market_cap_usd))
  d$gdp_usd <- suppressWarnings(as.numeric(d$gdp_usd))
  d <- d[(!is.na(d$date)) & (is.finite(d$market_cap_usd) | is.finite(d$gdp_usd)), , drop = FALSE]
  d <- d[order(d$date), , drop = FALSE]
  if (!("source" %in% names(d))) d$source <- "csv"
  if (!nrow(d)) return(NULL)
  d[, c("date", "market_cap_usd", "gdp_usd", "source"), drop = FALSE]
}

#' Resolve theme_key → ticker universe for concentration/attribution.
macro_bubble_resolve_universe <- function(theme_key, mode = get_market_mode()) {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  key <- as.character(theme_key %||% "")[1]
  if (!nzchar(key)) {
    return(list(tickers = character(0), source = "empty", label = ""))
  }

  # Concept / TW baskets already expand via macro_theme_tickers
  if (exists("macro_theme_tickers", mode = "function") &&
      (startsWith(key, "concept_") || startsWith(key, "tw_"))) {
    tks <- macro_theme_tickers(key, mode)
    return(list(
      tickers = unique(as.character(tks)),
      source = "concept",
      label = key
    ))
  }

  # GICS ETF → S&P 500 sector constituents
  if (startsWith(key, "gics_") && key %in% names(.MACRO_BUBBLE_GICS_SECTOR)) {
    sector <- unname(.MACRO_BUBBLE_GICS_SECTOR[[key]])
    sp <- tryCatch({
      if (exists("lab_get_sp500_universe", mode = "function")) {
        lab_get_sp500_universe(FALSE)
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (is.data.frame(sp) && nrow(sp) > 0L &&
        all(c("ticker", "gics_sector") %in% names(sp))) {
      keep <- toupper(trimws(as.character(sp$gics_sector))) == toupper(sector)
      tks <- unique(as.character(sp$ticker[keep]))
      tks <- tks[nzchar(tks)]
      return(list(tickers = tks, source = "gics_sp500", label = sector))
    }
    # Fallback: single ETF ticker
    if (exists("macro_theme_tickers", mode = "function")) {
      tks <- macro_theme_tickers(key, mode)
      return(list(tickers = unique(as.character(tks)), source = "gics_etf", label = sector))
    }
  }

  if (exists("macro_theme_tickers", mode = "function")) {
    tks <- macro_theme_tickers(key, mode)
    return(list(tickers = unique(as.character(tks)), source = "theme", label = key))
  }
  list(tickers = character(0), source = "empty", label = key)
}

#' Attach market caps; keep top max_n by mcap.
macro_bubble_attach_caps <- function(tickers, max_n = .MACRO_BUBBLE_MAX_POOL) {
  tks <- unique(as.character(tickers))
  tks <- tks[nzchar(tks)]
  if (!length(tks)) {
    return(data.frame(
      ticker = character(0), market_cap = numeric(0), weight = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  caps <- rep(NA_real_, length(tks))
  names(caps) <- tks
  if (exists("lab_fetch_market_caps_usd", mode = "function")) {
    got <- tryCatch(lab_fetch_market_caps_usd(tks), error = function(e) NULL)
    if (is.numeric(got) && length(got)) {
      nm <- toupper(trimws(names(got)))
      idx <- match(toupper(trimws(tks)), nm)
      hit <- which(!is.na(idx))
      if (length(hit)) caps[hit] <- as.numeric(got[idx[hit]])
    } else if (is.data.frame(got) && all(c("ticker", "market_cap") %in% names(got))) {
      idx <- match(toupper(trimws(tks)), toupper(trimws(as.character(got$ticker))))
      hit <- which(!is.na(idx))
      if (length(hit)) caps[hit] <- suppressWarnings(as.numeric(got$market_cap[idx[hit]]))
    }
  }
  if (!any(is.finite(caps)) && exists("lab_load_universe_metrics_snapshot", mode = "function")) {
    snap <- tryCatch(lab_load_universe_metrics_snapshot(), error = function(e) NULL)
    if (is.data.frame(snap) && all(c("ticker", "market_cap") %in% names(snap))) {
      idx <- match(toupper(trimws(tks)), toupper(trimws(as.character(snap$ticker))))
      hit <- which(!is.na(idx))
      if (length(hit)) caps[hit] <- suppressWarnings(as.numeric(snap$market_cap[idx[hit]]))
    }
  }
  # Fallback: snapshot CSV direct read
  if (!any(is.finite(caps))) {
    p <- c(
      file.path("data", "universe_metrics_snapshot.csv"),
      file.path(getwd(), "data", "universe_metrics_snapshot.csv")
    )
    p <- p[file.exists(p)][1]
    if (!is.na(p) && nzchar(p)) {
      snap <- tryCatch(
        utils::read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8"),
        error = function(e) NULL
      )
      if (is.data.frame(snap) && all(c("ticker", "market_cap") %in% names(snap))) {
        idx <- match(toupper(trimws(tks)), toupper(trimws(as.character(snap$ticker))))
        hit <- which(!is.na(idx))
        if (length(hit)) caps[hit] <- suppressWarnings(as.numeric(snap$market_cap[idx[hit]]))
      }
    }
  }

  df <- data.frame(
    ticker = tks,
    market_cap = as.numeric(caps),
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$market_cap) & df$market_cap > 0, , drop = FALSE]
  if (!nrow(df)) return(df)
  df <- df[order(-df$market_cap, df$ticker), , drop = FALSE]
  max_n <- max(5L, as.integer(max_n)[1])
  df <- utils::head(df, max_n)
  tot <- sum(df$market_cap)
  df$weight <- if (is.finite(tot) && tot > 0) df$market_cap / tot else NA_real_
  df
}

#' Period total return from price history (fraction).
macro_bubble_ticker_return <- function(ticker, period = "1y") {
  period <- as.character(period %||% "1y")[1]
  df <- tryCatch({
    if (exists("fetch_price_history_df", mode = "function")) {
      fetch_price_history_df(ticker, period)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 2L) return(NA_real_)
  if (!all(c("Date", "Close") %in% names(df))) return(NA_real_)
  df <- df[order(df$Date), , drop = FALSE]
  px <- df$Close[is.finite(df$Close)]
  if (length(px) < 2L) return(NA_real_)
  as.numeric(tail(px, 1) / px[[1]] - 1)
}

#' Fetch Close series for each ticker in a market-cap pool (named list).
macro_bubble_fetch_pool_prices <- function(pool, period = "1y", fetch_px = NULL) {
  period <- as.character(period %||% "1y")[1]
  out <- list()
  if (!is.data.frame(pool) || !nrow(pool) || !"ticker" %in% names(pool)) return(out)
  if (is.null(fetch_px)) {
    fetch_px <- if (exists("fetch_price_history_df", mode = "function")) {
      fetch_price_history_df
    } else {
      function(...) NULL
    }
  }
  for (tk in as.character(pool$ticker)) {
    if (!nzchar(tk)) next
    df <- tryCatch(fetch_px(tk, period), error = function(e) NULL)
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 2L) next
    if (!all(c("Date", "Close") %in% names(df))) next
    df <- df[order(as.Date(df$Date)), , drop = FALSE]
    df <- df[is.finite(df$Close) & df$Close > 0, , drop = FALSE]
    if (nrow(df) < 2L) next
    out[[tk]] <- data.frame(
      date = as.Date(df$Date),
      close = as.numeric(df$Close),
      stringsAsFactors = FALSE
    )
  }
  out
}

#' Total return (fraction) from a Close series data.frame.
macro_bubble_series_return <- function(ser) {
  if (is.null(ser) || !is.data.frame(ser) || nrow(ser) < 2L) return(NA_real_)
  if (!"close" %in% names(ser)) return(NA_real_)
  px <- ser$close[is.finite(ser$close) & ser$close > 0]
  if (length(px) < 2L) return(NA_real_)
  as.numeric(tail(px, 1) / px[[1]] - 1)
}

#' Empty concentration history frame.
.macro_bubble_conc_hist_empty <- function() {
  data.frame(
    date = as.Date(character(0)),
    top_n_share = numeric(0),
    top1_share = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Proxy Top-N / Top-1 market-cap share path for a fixed basket pool.
#' Historical weights use current market cap × (Close_t / Close_last); Top-N is
#' re-ranked each sample date within the snapshot pool (research proxy).
macro_bubble_concentration_history <- function(pool,
                                              top_n = 5L,
                                              period = "1y",
                                              prices = NULL,
                                              fetch_px = NULL) {
  top_n <- max(1L, min(10L, as.integer(top_n)[1]))
  period <- as.character(period %||% "1y")[1]
  empty <- .macro_bubble_conc_hist_empty()
  if (!is.data.frame(pool) || !nrow(pool)) return(empty)
  if (!all(c("ticker", "market_cap") %in% names(pool))) return(empty)
  if (is.null(prices)) {
    prices <- macro_bubble_fetch_pool_prices(pool, period = period, fetch_px = fetch_px)
  }
  if (!length(prices)) return(empty)

  tks <- intersect(as.character(pool$ticker), names(prices))
  if (!length(tks)) return(empty)
  caps0 <- suppressWarnings(as.numeric(pool$market_cap[match(tks, pool$ticker)]))
  names(caps0) <- tks
  last_px <- vapply(tks, function(tk) {
    x <- prices[[tk]]
    as.numeric(tail(x$close, 1))
  }, numeric(1))

  all_dates <- sort(unique(do.call(c, lapply(prices[tks], function(x) x$date))))
  if (!length(all_dates)) return(empty)
  if (identical(period, "1mo")) {
    step <- max(1L, as.integer(floor(length(all_dates) / 12)))
    samp <- all_dates[seq(1L, length(all_dates), by = step)]
  } else if (identical(period, "3mo")) {
    step <- max(1L, as.integer(floor(length(all_dates) / 16)))
    samp <- all_dates[seq(1L, length(all_dates), by = step)]
  } else {
    ym <- format(all_dates, "%Y-%m")
    samp <- all_dates[!duplicated(ym, fromLast = TRUE)]
  }
  samp <- sort(unique(c(samp, max(all_dates))))

  rows <- lapply(samp, function(d) {
    mcaps <- vapply(tks, function(tk) {
      x <- prices[[tk]]
      hit <- x$close[x$date <= d]
      if (!length(hit)) return(NA_real_)
      px <- as.numeric(tail(hit, 1))
      lp <- last_px[[tk]]
      cap <- caps0[[tk]]
      if (!is.finite(px) || px <= 0 || !is.finite(lp) || lp <= 0 || !is.finite(cap) || cap <= 0) {
        return(NA_real_)
      }
      cap * (px / lp)
    }, numeric(1))
    ok <- is.finite(mcaps) & mcaps > 0
    if (sum(ok) < 1L) return(NULL)
    w <- sort(mcaps[ok] / sum(mcaps[ok]), decreasing = TRUE)
    data.frame(
      date = d,
      top_n_share = sum(utils::head(w, top_n)),
      top1_share = w[[1]],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out) || !nrow(out)) return(empty)
  rownames(out) <- NULL
  out
}

#' Attach display names for Top-N table rows.
macro_bubble_top_names <- function(tickers, mode = "US") {
  tks <- as.character(tickers)
  out <- stats::setNames(rep("", length(tks)), tks)
  if (!length(tks)) return(out)
  if (exists("ynow_index_lookup_names", mode = "function")) {
    got <- tryCatch(ynow_index_lookup_names(tks, mode), error = function(e) NULL)
    if (is.character(got) && length(got)) {
      nm <- names(got)
      if (!is.null(nm) && length(nm) == length(got)) {
        idx <- match(toupper(trimws(tks)), toupper(trimws(nm)))
        hit <- which(!is.na(idx))
        if (length(hit)) out[hit] <- as.character(got[idx[hit]])
      } else if (length(got) == length(tks)) {
        out[] <- as.character(got)
      }
    }
  }
  out
}

#' Concentration + return attribution for a theme universe.
#' @return list with weights table, alerts, attribution, history
macro_bubble_concentration <- function(theme_key,
                                       mode = get_market_mode(),
                                       top_n = 5L,
                                       attr_period = "1y") {
  top_n <- max(1L, min(10L, as.integer(top_n)[1]))
  attr_period <- as.character(attr_period %||% "1y")[1]
  if (!attr_period %in% c("1mo", "3mo", "1y", "2y")) attr_period <- "1y"

  uni <- macro_bubble_resolve_universe(theme_key, mode)
  pool <- macro_bubble_attach_caps(uni$tickers, max_n = .MACRO_BUBBLE_MAX_POOL)
  empty_hist <- .macro_bubble_conc_hist_empty()
  empty <- list(
    universe = uni,
    pool = pool,
    top = pool[0, , drop = FALSE],
    top_n = top_n,
    top_share = NA_real_,
    top1_weight = NA_real_,
    alerts = character(0),
    history = empty_hist,
    attribution = list(
      period = attr_period,
      basket_ret = NA_real_,
      top_contrib = NA_real_,
      rest_contrib = NA_real_,
      top_share_of_upside = NA_real_,
      breadth_warn = FALSE
    )
  )
  if (!nrow(pool)) return(empty)

  top <- utils::head(pool, top_n)
  top_share <- sum(top$weight)
  top1 <- pool$weight[[1]]
  alerts <- character(0)
  if (is.finite(top1) && top1 >= 0.50) alerts <- c(alerts, "top1_gt_50")
  if (is.finite(top_share) && top_share >= 0.70) alerts <- c(alerts, "topn_gt_70")

  # One price fetch for attribution returns + concentration path
  prices <- macro_bubble_fetch_pool_prices(pool, period = attr_period)
  rets <- vapply(pool$ticker, function(tk) {
    macro_bubble_series_return(prices[[as.character(tk)]])
  }, numeric(1))
  pool$ret <- as.numeric(rets)
  ok <- is.finite(pool$weight) & is.finite(pool$ret)
  basket_ret <- if (any(ok)) sum(pool$weight[ok] * pool$ret[ok]) else NA_real_
  top_idx <- seq_len(min(top_n, nrow(pool)))
  top_ok <- ok[top_idx]
  top_contrib <- if (any(top_ok)) {
    sum(pool$weight[top_idx][top_ok] * pool$ret[top_idx][top_ok])
  } else {
    NA_real_
  }
  rest_contrib <- if (is.finite(basket_ret) && is.finite(top_contrib)) {
    basket_ret - top_contrib
  } else {
    NA_real_
  }

  breadth_warn <- FALSE
  top_share_up <- NA_real_
  if (is.finite(basket_ret) && basket_ret > 0 && is.finite(top_contrib)) {
    # Share of positive basket return coming from Top-N
    pos_top <- max(top_contrib, 0)
    top_share_up <- pos_top / basket_ret
    if (is.finite(top_share_up) && top_share_up >= 0.80) {
      breadth_warn <- TRUE
      alerts <- c(alerts, "narrow_breadth")
    }
  }

  history <- macro_bubble_concentration_history(
    pool = pool,
    top_n = top_n,
    period = attr_period,
    prices = prices
  )

  list(
    universe = uni,
    pool = pool,
    top = top,
    top_n = top_n,
    top_share = top_share,
    top1_weight = top1,
    alerts = unique(alerts),
    history = history,
    attribution = list(
      period = attr_period,
      basket_ret = basket_ret,
      top_contrib = top_contrib,
      rest_contrib = rest_contrib,
      top_share_of_upside = top_share_up,
      breadth_warn = breadth_warn
    )
  )
}

macro_bubble_read_buffett_csv <- function(mode = "US") {
  paths <- macro_bubble_buffett_paths(mode)
  hit <- paths[file.exists(paths)]
  if (!length(hit)) return(NULL)
  d <- tryCatch(
    utils::read.csv(hit[[1]], stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d)) return(NULL)
  if (!("date" %in% names(d)) || !("ratio_pct" %in% names(d))) return(NULL)
  d$date <- as.Date(d$date)
  d$ratio_pct <- suppressWarnings(as.numeric(d$ratio_pct))
  d <- d[is.finite(d$ratio_pct) & !is.na(d$date), , drop = FALSE]
  d <- d[order(d$date), , drop = FALSE]
  if (!("source" %in% names(d))) d$source <- "csv"
  d
}

.macro_bubble_wb_country <- function(mode = "US") {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  if (identical(mode, "TW")) "TWN" else "USA"
}

#' Format World Bank current-USD levels (market cap / GDP) for KPI display.
macro_bubble_fmt_usd_level <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return("—")
  ax <- abs(x)
  if (ax >= 1e12) return(sprintf("$%.2fT", x / 1e12))
  if (ax >= 1e9) return(sprintf("$%.1fB", x / 1e9))
  if (ax >= 1e6) return(sprintf("$%.1fM", x / 1e6))
  sprintf("$%.0f", x)
}

#' World Bank indicator series → date / value (current USD or %).
macro_bubble_fetch_wb_indicator <- function(mode = "US",
                                            indicator = "CM.MKT.LCAP.GD.ZS",
                                            timeout_sec = 20) {
  code <- .macro_bubble_wb_country(mode)
  ind <- as.character(indicator %||% "")[1]
  if (!nzchar(ind)) return(NULL)
  cache_key <- paste0("wb_", code, "_", ind)
  if (is.data.frame(.MACRO_BUBBLE_ENV[[cache_key]])) {
    return(.MACRO_BUBBLE_ENV[[cache_key]])
  }
  url <- sprintf(
    "https://api.worldbank.org/v2/country/%s/indicator/%s?format=json&per_page=120",
    code, ind
  )
  raw <- tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(url, httr::timeout(timeout_sec))
      if (httr::status_code(resp) >= 400L) return(NULL)
      httr::content(resp, as = "text", encoding = "UTF-8")
    } else {
      con <- url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      paste(readLines(con, warn = FALSE), collapse = "\n")
    }
  }, error = function(e) NULL)
  if (!nzchar(raw %||% "")) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(parsed) || length(parsed) < 2L) return(NULL)
  rows <- parsed[[2]]
  if (!is.list(rows) || !length(rows)) return(NULL)
  out <- lapply(rows, function(r) {
    if (!is.list(r)) return(NULL)
    yr <- suppressWarnings(as.integer(r$date %||% NA))
    val <- suppressWarnings(as.numeric(r$value %||% NA))
    if (!is.finite(yr) || !is.finite(val)) return(NULL)
    data.frame(
      date = as.Date(sprintf("%d-12-31", yr)),
      value = val,
      source = "worldbank",
      stringsAsFactors = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(NULL)
  df <- do.call(rbind, out)
  df <- df[order(df$date), , drop = FALSE]
  .MACRO_BUBBLE_ENV[[cache_key]] <- df
  df
}

#' Try World Bank CM.MKT.LCAP.GD.ZS; fall back to shipped CSV.
macro_bubble_fetch_buffett_worldbank <- function(mode = "US", timeout_sec = 20) {
  ser <- macro_bubble_fetch_wb_indicator(
    mode, "CM.MKT.LCAP.GD.ZS", timeout_sec = timeout_sec
  )
  if (!is.data.frame(ser) || !nrow(ser)) return(NULL)
  data.frame(
    date = ser$date,
    ratio_pct = ser$value,
    source = ser$source,
    stringsAsFactors = FALSE
  )
}

#' Absolute market-cap and GDP (current USD) for Buffett KPI companions.
#' TW: DGBAS NSTAT first, then shipped CSV. US: World Bank first, then CSV.
macro_bubble_buffett_abs_series <- function(mode = get_market_mode(), timeout_sec = 20) {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  if (identical(mode, "TW")) {
    dg <- tryCatch(macro_bubble_fetch_buffett_dgbas(timeout_sec = timeout_sec), error = function(e) NULL)
    if (is.data.frame(dg) && nrow(dg) > 0L) {
      return(dg[, c("date", "market_cap_usd", "gdp_usd", "source"), drop = FALSE])
    }
    csv <- tryCatch(macro_bubble_read_buffett_abs_csv(mode), error = function(e) NULL)
    if (is.data.frame(csv) && nrow(csv) > 0L) return(csv)
    return(NULL)
  }
  mcap <- tryCatch(
    macro_bubble_fetch_wb_indicator(mode, "CM.MKT.LCAP.CD", timeout_sec = timeout_sec),
    error = function(e) NULL
  )
  gdp <- tryCatch(
    macro_bubble_fetch_wb_indicator(mode, "NY.GDP.MKTP.CD", timeout_sec = timeout_sec),
    error = function(e) NULL
  )
  ratio <- tryCatch(
    macro_bubble_fetch_wb_indicator(mode, "CM.MKT.LCAP.GD.ZS", timeout_sec = timeout_sec),
    error = function(e) NULL
  )
  if (!is.data.frame(mcap) || !nrow(mcap)) mcap <- NULL
  if (!is.data.frame(gdp) || !nrow(gdp)) gdp <- NULL
  if (!is.data.frame(ratio) || !nrow(ratio)) ratio <- NULL
  if (is.null(mcap) && is.null(gdp)) {
    csv <- tryCatch(macro_bubble_read_buffett_abs_csv(mode), error = function(e) NULL)
    if (is.data.frame(csv) && nrow(csv) > 0L) return(csv)
    return(NULL)
  }
  dates <- sort(unique(c(
    if (!is.null(mcap)) mcap$date else as.Date(character(0)),
    if (!is.null(gdp)) gdp$date else as.Date(character(0)),
    if (!is.null(ratio)) ratio$date else as.Date(character(0))
  )))
  if (!length(dates)) return(NULL)
  mc <- if (!is.null(mcap)) mcap$value[match(dates, mcap$date)] else rep(NA_real_, length(dates))
  gd <- if (!is.null(gdp)) gdp$value[match(dates, gdp$date)] else rep(NA_real_, length(dates))
  rt <- if (!is.null(ratio)) ratio$value[match(dates, ratio$date)] else rep(NA_real_, length(dates))
  # Fill gaps: Market cap ≈ GDP × (market cap / GDP).
  miss_mc <- !is.finite(mc) & is.finite(gd) & is.finite(rt) & rt > 0
  if (any(miss_mc)) mc[miss_mc] <- gd[miss_mc] * (rt[miss_mc] / 100)
  data.frame(
    date = dates,
    market_cap_usd = as.numeric(mc),
    gdp_usd = as.numeric(gd),
    source = "worldbank",
    stringsAsFactors = FALSE
  )
}

#' Latest (as-of filtered) absolute levels for KPI boxes.
macro_bubble_buffett_abs_asof <- function(series, asof_year = NA_integer_) {
  empty <- list(
    market_cap_usd = NA_real_,
    gdp_usd = NA_real_,
    as_of = as.Date(NA),
    ok = FALSE
  )
  if (!is.data.frame(series) || !nrow(series)) return(empty)
  ser <- series
  yr <- suppressWarnings(as.integer(asof_year)[1])
  if (is.finite(yr)) {
    ser <- ser[as.integer(format(ser$date, "%Y")) <= yr, , drop = FALSE]
  }
  if (!nrow(ser)) return(empty)
  # Prefer the newest row with a positive level (ignore 0 / placeholder pads).
  for (i in rev(seq_len(nrow(ser)))) {
    mc <- suppressWarnings(as.numeric(ser$market_cap_usd[[i]])[1])
    gd <- suppressWarnings(as.numeric(ser$gdp_usd[[i]])[1])
    mc_ok <- is.finite(mc) && mc > 0
    gd_ok <- is.finite(gd) && gd > 0
    if (mc_ok || gd_ok) {
      return(list(
        market_cap_usd = if (mc_ok) mc else NA_real_,
        gdp_usd = if (gd_ok) gd else NA_real_,
        as_of = as.Date(ser$date[[i]]),
        ok = TRUE
      ))
    }
  }
  empty
}

macro_bubble_buffett_series <- function(mode = get_market_mode()) {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    toupper(as.character(mode)[1])
  }
  if (identical(mode, "TW")) {
    dg <- tryCatch(macro_bubble_fetch_buffett_dgbas(), error = function(e) NULL)
    if (is.data.frame(dg) && nrow(dg) >= 8L && any(is.finite(dg$ratio_pct))) {
      out <- data.frame(
        date = dg$date,
        ratio_pct = dg$ratio_pct,
        source = dg$source,
        stringsAsFactors = FALSE
      )
      out <- out[is.finite(out$ratio_pct), , drop = FALSE]
      if (nrow(out) >= 8L) return(out)
    }
  }
  live <- tryCatch(macro_bubble_fetch_buffett_worldbank(mode), error = function(e) NULL)
  if (is.data.frame(live) && nrow(live) >= 8L) return(live)
  csv <- macro_bubble_read_buffett_csv(mode)
  if (is.data.frame(csv) && nrow(csv) >= 1L) return(csv)
  live
}

#' Traffic light from series mean ± 0.75·sd
macro_bubble_buffett_light <- function(series) {
  if (is.null(series) || !is.data.frame(series) || nrow(series) < 3L) {
    return(list(
      level = "unknown",
      label_key = "macro_bubble_buffett_unknown",
      color = "#95a5a6",
      current = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      as_of = as.Date(NA)
    ))
  }
  x <- series$ratio_pct[is.finite(series$ratio_pct)]
  mu <- mean(x)
  sdv <- stats::sd(x)
  if (!is.finite(sdv) || sdv <= 0) sdv <- abs(mu) * 0.05 + 1
  cur_row <- series[nrow(series), , drop = FALSE]
  cur <- as.numeric(cur_row$ratio_pct[[1]])
  lo <- mu - 0.75 * sdv
  hi <- mu + 0.75 * sdv
  level <- if (!is.finite(cur)) {
    "unknown"
  } else if (cur >= hi) {
    "overvalued"
  } else if (cur <= lo) {
    "undervalued"
  } else {
    "fair"
  }
  label_key <- switch(
    level,
    overvalued = "macro_bubble_buffett_over",
    undervalued = "macro_bubble_buffett_under",
    fair = "macro_bubble_buffett_fair",
    "macro_bubble_buffett_unknown"
  )
  color <- switch(
    level,
    overvalued = "#d9534f",
    undervalued = "#00a65a",
    fair = "#f39c12",
    "#95a5a6"
  )
  list(
    level = level,
    label_key = label_key,
    color = color,
    current = cur,
    mean = mu,
    sd = sdv,
    lo = lo,
    hi = hi,
    as_of = as.Date(cur_row$date[[1]])
  )
}

#' UI body: bubble & concentration (mounted at the bottom of Macro & Market Trends).
#' Page supplies visible title + lead; KPIs sit above charts.
macro_bubble_chapter_ui <- function(ns) {
  tags$div(
    class = "ynow-macro-bubble ynow-funnel-bubble-body",
    # Basket comes from shared Industry / Concept picks in Relative performance (no duplicate theme menu).
    uiOutput(ns("bubble_shared_pick_status")),
    tags$div(
      class = "ynow-funnel-toolbar",
      role = "group",
      `aria-label` = "Bubble and concentration controls",
      fluidRow(
        column(
          width = 4,
          class = "col-xs-12 col-sm-4 col-md-4",
          selectInput(
            ns("bubble_top_n"),
            label = tags$span(id = "ynow_macro_bubble_topn_label", "Top N by market cap"),
            choices = c("Top 3" = "3", "Top 5" = "5"),
            selected = "5"
          )
        ),
        column(
          width = 4,
          class = "col-xs-12 col-sm-4 col-md-4",
          selectInput(
            ns("bubble_attr_period"),
            label = tags$span(id = "ynow_macro_bubble_attr_label", "Analysis window"),
            choices = c("1M" = "1mo", "3M" = "3mo", "1Y" = "1y", "2Y" = "2y"),
            selected = "1y"
          )
        ),
        column(
          width = 4,
          class = "col-xs-12 col-sm-4 col-md-4",
          tags$div(
            style = "margin-top: 24px;",
            actionButton(
              ns("bubble_refresh"),
              label = tags$span(id = "ynow_macro_bubble_refresh", "Refresh"),
              icon = icon("sync"),
              class = "btn-default"
            )
          )
        )
      )
    ),
    uiOutput(ns("bubble_alert_box")),
    fluidRow(
      class = "ynow-macro-chart-row ynow-bubble-pair-row",
      column(
        width = 6,
        class = "col-xs-12 col-sm-6 col-md-6 ynow-bubble-pair-col ynow-bubble-conc-col",
        tags$h4(id = "ynow_macro_bubble_conc_title", "Market-cap concentration"),
        uiOutput(ns("bubble_conc_kpi")),
        tags$div(
          class = "ynow-bubble-conc-stack",
          plotlyOutput(ns("bubble_conc_plot"), height = "280px") %>%
            shinycssloaders::withSpinner(),
          tags$h5(
            id = "ynow_macro_bubble_top_list_title",
            style = "margin-top: 12px;",
            "Top N by market cap"
          ),
          uiOutput(ns("bubble_conc_table")),
          tags$p(
            id = "ynow_macro_bubble_conc_note",
            class = "ynow-macro-hint",
            paste0(
              "History uses current market cap × relative Close (research proxy); ",
              "Top-N membership is re-ranked within today’s basket pool."
            )
          )
        )
      ),
      column(
        width = 6,
        class = "col-xs-12 col-sm-6 col-md-6 ynow-bubble-pair-col ynow-bubble-attr-col",
        tags$h4(id = "ynow_macro_bubble_attr_title", "Return attribution"),
        uiOutput(ns("bubble_attr_kpi")),
        tags$div(
          class = "ynow-bubble-attr-plot-host",
          plotlyOutput(ns("bubble_attr_plot"), height = "520px", width = "100%") %>%
            shinycssloaders::withSpinner()
        )
      )
    ),
    htmltools::tags$script(htmltools::HTML("
      (function() {
        function measureConcStack() {
          var stack = document.querySelector('.ynow-bubble-conc-stack');
          if (!stack) return 0;
          var plot = stack.querySelector('.js-plotly-plot, .html-widget');
          var table = stack.querySelector('.table-responsive, table');
          var title = stack.querySelector('#ynow_macro_bubble_top_list_title, h5');
          var h = 0;
          if (plot) h += plot.offsetHeight || 0;
          if (title) {
            var cs = window.getComputedStyle(title);
            h += title.offsetHeight || 0;
            h += (parseFloat(cs.marginTop) || 0) + (parseFloat(cs.marginBottom) || 0);
          }
          if (table) h += table.offsetHeight || 0;
          return h;
        }
        function syncAttrPieHeight() {
          var host = document.querySelector('.ynow-bubble-attr-plot-host');
          if (!host) return;
          var h = measureConcStack();
          if (!(h > 200)) return;
          host.style.height = h + 'px';
          host.style.minHeight = h + 'px';
          var gd = host.querySelector('.js-plotly-plot');
          if (gd) {
            gd.style.height = h + 'px';
            if (window.Plotly && Plotly.Plots && Plotly.Plots.resize) {
              try { Plotly.Plots.resize(gd); } catch (e) {}
            }
          }
        }
        function boot() {
          syncAttrPieHeight();
          var stack = document.querySelector('.ynow-bubble-conc-stack');
          if (stack && !stack.__ynowBubbleRo) {
            stack.__ynowBubbleRo = true;
            if (window.ResizeObserver) {
              var ro = new ResizeObserver(function() { syncAttrPieHeight(); });
              ro.observe(stack);
            }
          }
        }
        if (window.jQuery) {
          $(document).on('shiny:value shiny:visualchange plotly_afterplot', function() {
            boot();
          });
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', boot);
        } else {
          boot();
        }
        window.addEventListener('resize', syncAttrPieHeight);
      })();
    ")),
    tags$hr(),
    tags$h4(id = "ynow_macro_bubble_buffett_title", "Buffett Indicator (market cap / GDP)"),
    fluidRow(
      class = "ynow-macro-kpi-row ynow-funnel-kpi-row",
      column(
        width = 4,
        class = "col-xs-12 col-sm-4 col-md-4",
        uiOutput(ns("bubble_buffett_light"))
      ),
      column(
        width = 4,
        class = "col-xs-12 col-sm-4 col-md-4",
        uiOutput(ns("bubble_buffett_mcap"))
      ),
      column(
        width = 4,
        class = "col-xs-12 col-sm-4 col-md-4",
        uiOutput(ns("bubble_buffett_gdp"))
      )
    ),
    tags$div(
      class = "ynow-bubble-buffett-plot-wrap",
      plotlyOutput(ns("bubble_buffett_plot"), height = "320px", width = "100%") %>%
        shinycssloaders::withSpinner()
    ),
    ynow_notes_block(
      tags$p(
        id = "ynow_macro_bubble_buffett_note",
        class = "ynow-macro-hint",
        paste0(
          "Series: World Bank market capitalization of listed domestic companies (% of GDP) when reachable; ",
          "companion boxes use World Bank total market cap (CM.MKT.LCAP.CD) and GDP (NY.GDP.MKTP.CD) in current USD. ",
          "Otherwise the bundled CSV snapshot (ratio only). Traffic light uses each market’s own mean ± 0.75·sd."
        )
      )
    )
  )
}
