# ==========================================
# global_prediction_module.R — Global Prediction (standalone)
# Boards: Economic / Finance / Company. Kalshi + Polymarket public APIs.
#
# Crowd-implied probabilities from Kalshi + Polymarket public APIs.
# Theme registry joins two independent crowds for side-by-side comparison.
# Not Fair Value, not a buy signal, not blended "truth" unless labeled.
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

.GP_KALSHI_COLOR <- "#00D66C"
.GP_POLYMARKET_COLOR <- "#2E5CFF"
# Warm copper/ochre — distinct from Kalshi green / Polymarket blue; fits ATX khaki–brown chrome.
.GP_FEEX_COLOR <- "#C17A2E"
.GP_AGREE_PP <- 0.10
.GP_DIVERGE_PP <- 0.25
.GP_UA <- "theYNowApp"

# ---- Freshness / crawl policy (時效性重掃) ---------------------------------
# Quote layer: live Yes probs — short TTL on fast cache (minutes, not 24h).
# Discovery layer: theme→contract classification — slightly longer, but hard
# triggers always invalidate before rescan.
.GP_QUOTE_TTL_SEC <- 3L * 60L          # 3 min — live quotes
.GP_DISCOVERY_TTL_SEC <- 15L * 60L     # 15 min — theme→contract discovery
.GP_PERIODIC_RESCAN_SEC <- 3L * 60L    # while ATX / Global Prediction visible
.GP_MIN_FETCH_GAP_SEC <- 30L           # soft rate limit between network bursts
.GP_DROP_STATUS <- c(
  "finalized", "closed", "settled", "determined", "inactive", "expired"
)
.GP_HARD_TRIGGERS <- c(
  "tab_open", "tab_focus", "mode_switch", "manual_refresh"
)
# Line-chart history (Yes probability time series). Never invent points.
# One plotly line chart per market (never overlay markets on one theme plot).
.GP_HISTORY_LOOKBACK_DAYS <- 90L
.GP_HISTORY_PER_VENUE <- 6L  # match theme quote cap; each market → own chart
.GP_HISTORY_MIN_POINTS <- 2L
.GP_HISTORY_PERIOD_MIN <- 1440L  # Kalshi daily candlesticks
.GP_POLY_CLOB <- "https://clob.polymarket.com"
.GP_KALSHI_API <- "https://api.elections.kalshi.com/trade-api/v2"

#' Documented crawl / freshness rules for Global Prediction venues.
#' @return named list of TTLs, triggers, venues, and fail-closed flags.
gp_crawl_policy <- function() {
  list(
    quote_ttl_sec = as.integer(.GP_QUOTE_TTL_SEC),
    discovery_ttl_sec = as.integer(.GP_DISCOVERY_TTL_SEC),
    periodic_rescan_sec = as.integer(.GP_PERIODIC_RESCAN_SEC),
    min_fetch_gap_sec = as.integer(.GP_MIN_FETCH_GAP_SEC),
    hard_triggers = as.character(.GP_HARD_TRIGGERS),
    soft_triggers = c("periodic_visible"),
    venues = c("kalshi", "polymarket", "feex"),
    cache_tier = "fast",
    fail_closed = TRUE,
    invent_probs = FALSE,
    breadth_order = c("macro", "market_index", "sector_theme", "single_stock"),
    note = paste(
      "Hard triggers invalidate gp:* keys then rescan.",
      "Periodic uses quote TTL; discovery TTL gates series/query re-probe.",
      "Boards Economic/Finance/Company; Eco/Finance use monthly+ Kalshi frequency."
    )
  )
}

gp_is_hard_trigger <- function(trigger) {
  .gp_chr(trigger) %in% .GP_HARD_TRIGGERS
}

gp_brand_colors <- function() {
  list(
    kalshi = .GP_KALSHI_COLOR,
    polymarket = .GP_POLYMARKET_COLOR,
    feex = .GP_FEEX_COLOR
  )
}

.gp_chr <- function(x, default = "") {
  x <- tryCatch(as.character(x)[1], error = function(e) NA_character_)
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) default else x
}

.gp_norm_market_mode <- function(mode = "US") {
  m <- toupper(.gp_chr(mode, "US"))
  if (m %in% c("TW", "TWN", "TAIWAN")) "TW" else "US"
}

#' 未來事件交易所 (xFuture / NCCU): no live public feed — honest empty.
gp_fetch_feex_theme <- function(theme_id, topic = NULL, use_cache = TRUE,
                                layer = "discovery") {
  theme_id <- .gp_chr(theme_id)
  cache_key <- .gp_cache_key("feex", theme_id)
  ttl <- if (identical(layer, "quote")) .GP_QUOTE_TTL_SEC else .GP_DISCOVERY_TTL_SEC
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get_fresh(cache_key, ttl)
    if (is.list(hit)) return(hit)
  }
  # DNS for xfuture.org / legacy FEEX hosts does not resolve (2026 check).
  # Academic archives require application; inventing prices is forbidden.
  out <- list(
    ok = FALSE,
    quotes = .gp_empty_quote(),
    error = "feex_no_public_feed",
    status = "defunct"
  )
  if (isTRUE(use_cache)) .gp_cache_set_fresh(cache_key, out, layer = layer)
  out
}

# Dollar strings are already [0, 1]. Only a whole number above 1 is a legacy cent quote.
# Anything outside that is missing. It is not rescaled into a probability.
.gp_num01 <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  if (is.character(x)) {
    raw <- trimws(as.character(x)[1])
    if (!nzchar(raw) || is.na(raw)) return(NA_real_)
    v <- suppressWarnings(as.numeric(raw))
    if (length(v) != 1L || is.na(v) || !is.finite(v) || v < 0 || v > 1) return(NA_real_)
    return(v)
  }
  v <- suppressWarnings(as.numeric(x)[1])
  if (length(v) != 1L || is.na(v) || !is.finite(v)) return(NA_real_)
  if (abs(v - round(v)) < 1e-9 && v > 1 && v <= 100) v <- v / 100
  if (v < 0 || v > 1) return(NA_real_)
  v
}

.gp_label <- function(key, locale = "en") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    "en"
  }
  if (exists("ui_str", mode = "function")) {
    out <- tryCatch(ui_str(key, loc), error = function(e) key)
    if (is.null(out) || !nzchar(.gp_chr(out))) key else .gp_chr(out)
  } else {
    key
  }
}

.gp_http_get_json <- function(url, timeout_sec = 12) {
  url <- .gp_chr(url)
  if (!nzchar(url)) {
    return(list(ok = FALSE, status_code = NA_integer_, data = NULL, error = "empty_url"))
  }
  resp <- tryCatch(
    {
      if (requireNamespace("httr", quietly = TRUE)) {
        r <- httr::GET(
          url,
          httr::user_agent(.GP_UA),
          httr::timeout(timeout_sec),
          httr::add_headers(Accept = "application/json")
        )
        code <- httr::status_code(r)
        body <- httr::content(r, as = "text", encoding = "UTF-8")
        list(code = code, body = body)
      } else {
        con <- url(url, open = "rb")
        on.exit(close(con), add = TRUE)
        raw <- readBin(con, what = "raw", n = 5e6)
        list(code = 200L, body = rawToChar(raw))
      }
    },
    error = function(e) list(code = NA_integer_, body = NULL, err = conditionMessage(e))
  )
  if (is.null(resp$body) || !nzchar(.gp_chr(resp$body))) {
    return(list(
      ok = FALSE,
      status_code = resp$code %||% NA_integer_,
      data = NULL,
      error = resp$err %||% "empty_body"
    ))
  }
  code <- as.integer(resp$code %||% NA_integer_)
  if (is.finite(code) && code >= 400L) {
    return(list(ok = FALSE, status_code = code, data = NULL, error = paste0("http_", code)))
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(resp$body, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed)) {
    return(list(ok = FALSE, status_code = code, data = NULL, error = "json_parse"))
  }
  list(ok = TRUE, status_code = code, data = parsed, error = NULL)
}

# cachem keys: lowercase letters + digits only (no ":" / "|" / ",").
.gp_cache_key <- function(...) {
  raw <- paste(vapply(list(...), function(x) {
    paste(as.character(unlist(x, use.names = FALSE)), collapse = "")
  }, character(1)), collapse = "")
  key <- tolower(gsub("[^a-zA-Z0-9]+", "", raw))
  if (!nzchar(key)) key <- "empty"
  # Stable "gp" prefix so invalidate can find all Global Prediction entries.
  paste0("gp", key)
}

.gp_cache_get <- function(key) {
  if (!exists(".ynow_fast_cache", inherits = TRUE)) return(NULL)
  key <- .gp_chr(key)
  if (!nzchar(key)) return(NULL)
  tryCatch(.ynow_fast_cache$get(key), error = function(e) NULL)
}

.gp_cache_set <- function(key, value) {
  if (!exists(".ynow_fast_cache", inherits = TRUE)) return(invisible(FALSE))
  key <- .gp_chr(key)
  if (!nzchar(key)) return(invisible(FALSE))
  tryCatch({
    .ynow_fast_cache$set(key, value)
    TRUE
  }, error = function(e) FALSE)
}

.gp_cache_remove <- function(key) {
  if (!exists(".ynow_fast_cache", inherits = TRUE)) return(invisible(FALSE))
  key <- .gp_chr(key)
  if (!nzchar(key)) return(invisible(FALSE))
  tryCatch({
    .ynow_fast_cache$remove(key)
    TRUE
  }, error = function(e) FALSE)
}

#' Read a stamped GP cache entry; miss if older than ttl_sec or unstamped legacy.
.gp_cache_get_fresh <- function(key, ttl_sec) {
  hit <- .gp_cache_get(key)
  if (is.null(hit)) return(NULL)
  # Legacy unstamped payloads (pre-freshness) are treated as expired.
  if (!is.list(hit) || is.null(hit$cached_at) || is.null(hit$value)) return(NULL)
  age <- suppressWarnings(as.numeric(difftime(Sys.time(), hit$cached_at, units = "secs")))
  if (!is.finite(age) || age < 0 || age > as.numeric(ttl_sec)) return(NULL)
  hit$value
}

.gp_cache_set_fresh <- function(key, value, layer = "quote") {
  .gp_cache_set(
    key,
    list(
      cached_at = Sys.time(),
      layer = .gp_chr(layer, "quote"),
      value = value
    )
  )
}

#' Drop all Global Prediction fast-cache keys (quote + discovery).
#' Call on hard rescan triggers so classification is never sticky forever.
gp_invalidate_crawl_cache <- function(market_mode = NULL) {
  if (!exists(".ynow_fast_cache", inherits = TRUE)) return(invisible(0L))
  keys <- tryCatch(.ynow_fast_cache$keys(), error = function(e) character(0))
  if (!length(keys)) return(invisible(0L))
  mode <- if (is.null(market_mode)) NULL else .gp_norm_market_mode(market_mode)
  mode_token <- if (is.null(mode)) NULL else tolower(paste0("mode", mode))
  n <- 0L
  for (k in keys) {
    k <- as.character(k)[1]
    if (!nzchar(k) || !startsWith(k, "gp")) next
    if (!is.null(mode_token) && grepl("mode", k, fixed = TRUE) &&
          !grepl(mode_token, k, fixed = TRUE)) {
      next
    }
    if (isTRUE(.gp_cache_remove(k))) n <- n + 1L
  }
  invisible(n)
}

.gp_parse_time <- function(x) {
  x <- .gp_chr(x)
  if (!nzchar(x)) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  formats <- c(
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d"
  )
  for (fmt in formats) {
    t <- suppressWarnings(as.POSIXct(x, tz = "UTC", format = fmt))
    if (length(t) == 1L && !is.na(t)) return(t)
  }
  t <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (length(t) == 1L && !is.na(t)) t else as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
}

.gp_empty_quote <- function() {
  data.frame(
    venue = character(0),
    theme_id = character(0),
    market_id = character(0),
    title = character(0),
    yes_prob = numeric(0),
    updated_at = character(0),
    expires_at = character(0),
    url = character(0),
    raw_status = character(0),
    breadth_tier = integer(0),
    series_ticker = character(0),
    history_token = character(0),
    stringsAsFactors = FALSE
  )
}

.gp_quote_row <- function(venue, theme_id, market_id, title, yes_prob,
                          updated_at = NA_character_, expires_at = NA_character_,
                          url = NA_character_, raw_status = NA_character_,
                          breadth_tier = NA_integer_,
                          series_ticker = NA_character_,
                          history_token = NA_character_) {
  data.frame(
    venue = .gp_chr(venue),
    theme_id = .gp_chr(theme_id),
    market_id = .gp_chr(market_id),
    title = .gp_chr(title),
    yes_prob = .gp_num01(yes_prob),
    updated_at = .gp_chr(updated_at, NA_character_),
    expires_at = .gp_chr(expires_at, NA_character_),
    url = .gp_chr(url, NA_character_),
    raw_status = .gp_chr(raw_status, NA_character_),
    breadth_tier = as.integer(breadth_tier)[1],
    series_ticker = .gp_chr(series_ticker, NA_character_),
    history_token = .gp_chr(history_token, NA_character_),
    stringsAsFactors = FALSE
  )
}

.gp_empty_series <- function() {
  data.frame(
    venue = character(0),
    theme_id = character(0),
    market_id = character(0),
    title = character(0),
    ts = as.POSIXct(character(0), tz = "UTC"),
    yes_prob = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Derive Kalshi series ticker when the board fetch already knows it.
.gp_kalshi_series_ticker <- function(market_ticker, series_hint = NULL) {
  hint <- .gp_chr(series_hint)
  if (nzchar(hint)) return(hint)
  t <- .gp_chr(market_ticker)
  if (!nzchar(t)) return(NA_character_)
  parts <- strsplit(t, "-", fixed = TRUE)[[1]]
  if (length(parts) >= 2L) parts[[1]] else t
}

#' True when status or resolution/close time says the contract is dead.
gp_quote_is_expired <- function(row, now = Sys.time()) {
  if (!is.data.frame(row) || nrow(row) < 1L) return(TRUE)
  status <- tolower(.gp_chr(row$raw_status[[1]]))
  if (nzchar(status) && status %in% .GP_DROP_STATUS) return(TRUE)
  exp_raw <- if ("expires_at" %in% names(row)) row$expires_at[[1]] else NA_character_
  exp <- .gp_parse_time(exp_raw)
  if (!is.na(exp) && is.finite(as.numeric(exp)) && exp < now) return(TRUE)
  FALSE
}

#' Drop closed/expired rows; dedupe by venue+market_id. Never invents probs.
gp_filter_live_quotes <- function(quotes, now = Sys.time()) {
  if (!is.data.frame(quotes) || nrow(quotes) == 0L) return(.gp_empty_quote())
  keep <- !vapply(seq_len(nrow(quotes)), function(i) {
    gp_quote_is_expired(quotes[i, , drop = FALSE], now = now)
  }, logical(1))
  out <- quotes[keep, , drop = FALSE]
  if (nrow(out) == 0L) return(.gp_empty_quote())
  key <- paste(out$venue, out$market_id, sep = "\t")
  out <- out[!duplicated(key), , drop = FALSE]
  # Align columns for rbind safety across call sites.
  need <- names(.gp_empty_quote())
  for (nm in need) {
    if (!(nm %in% names(out))) {
      if (nm == "breadth_tier") out[[nm]] <- NA_integer_
      else if (nm == "yes_prob") out[[nm]] <- NA_real_
      else out[[nm]] <- NA_character_
    }
  }
  out[, need, drop = FALSE]
}

.gp_series_downsample <- function(df, max_n = 400L) {
  if (exists(".ynow_downsample_df", mode = "function")) {
    return(.ynow_downsample_df(df, max_n = max_n))
  }
  if (is.null(df) || !is.data.frame(df)) return(df)
  n <- nrow(df)
  max_n <- as.integer(max_n)[1]
  if (!is.finite(n) || !is.finite(max_n) || n <= max_n || max_n < 3L) return(df)
  idx <- unique(c(1L, as.integer(round(seq(1, n, length.out = max_n))), n))
  df[idx, , drop = FALSE]
}

#' Score a title against theme keywords; higher = better fit.
.gp_theme_keyword_score <- function(title, keywords) {
  if (is.null(keywords) || length(keywords) == 0L) return(0L)
  hay <- tolower(.gp_chr(title))
  if (!nzchar(hay)) return(0L)
  sum(vapply(keywords, function(k) {
    k <- tolower(.gp_chr(k))
    as.integer(nzchar(k) && grepl(k, hay, fixed = TRUE))
  }, integer(1)))
}

#' Re-home live quotes onto the best-matching theme in the mode registry.
#' Promotes newly relevant markets into the correct breadth_tier; orphans with
#' zero keyword hits stay on their fetch theme_id (registry order still applies).
gp_reclassify_quotes <- function(quotes, market_mode = "US", now = Sys.time()) {
  quotes <- gp_filter_live_quotes(quotes, now = now)
  if (nrow(quotes) == 0L) return(quotes)
  mode <- .gp_norm_market_mode(market_mode)
  reg <- gp_theme_registry(mode)
  tier_map <- setNames(as.integer(reg$breadth_tier), as.character(reg$theme_id))
  out_rows <- vector("list", nrow(quotes))
  for (i in seq_len(nrow(quotes))) {
    row <- quotes[i, , drop = FALSE]
    title <- .gp_chr(row$title[[1]])
    best_id <- .gp_chr(row$theme_id[[1]])
    best_score <- -1L
    for (j in seq_len(nrow(reg))) {
      kw <- if ("keywords" %in% names(reg)) reg$keywords[[j]] else NULL
      sc <- .gp_theme_keyword_score(title, kw)
      if (sc > best_score) {
        best_score <- sc
        best_id <- as.character(reg$theme_id[[j]])
      }
    }
    if (best_score <= 0L) {
      # Keep fetch theme; still stamp breadth from registry when known.
      best_id <- .gp_chr(row$theme_id[[1]])
    }
    row$theme_id[[1]] <- best_id
    row$breadth_tier[[1]] <- unname(tier_map[[best_id]] %||% NA_integer_)
    out_rows[[i]] <- row
  }
  out <- do.call(rbind, out_rows)
  # Stable: breadth_tier then original order.
  ord <- order(
    as.integer(out$breadth_tier %||% NA_integer_),
    seq_len(nrow(out)),
    na.last = TRUE
  )
  out[ord, , drop = FALSE]
}

.gp_book_yes <- function(last, bid, ask) {
  if (is.finite(last)) return(last)
  if (is.finite(bid) && is.finite(ask)) return((bid + ask) / 2)
  if (is.finite(bid)) return(bid)
  if (is.finite(ask)) return(ask)
  NA_real_
}

.gp_kalshi_yes <- function(m) {
  has_dollars <- any(vapply(
    c("last_price_dollars", "yes_bid_dollars", "yes_ask_dollars"),
    function(k) nzchar(.gp_chr(m[[k]])),
    logical(1)
  ))
  if (has_dollars) {
    return(.gp_book_yes(
      .gp_num01(m$last_price_dollars),
      .gp_num01(m$yes_bid_dollars),
      .gp_num01(m$yes_ask_dollars)
    ))
  }
  .gp_book_yes(.gp_num01(m$last_price), .gp_num01(m$yes_bid), .gp_num01(m$yes_ask))
}

.gp_as_payload <- function(payload) {
  if (is.character(payload)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(payload[[1]], simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.list(parsed)) return(NULL)
    return(parsed)
  }
  if (is.list(payload)) return(payload)
  NULL
}

#' Parse Kalshi /markets JSON list into MarketQuote rows.
gp_parse_kalshi_markets <- function(payload, theme_id, limit = 3L,
                                    series_ticker = NULL) {
  theme_id <- .gp_chr(theme_id)
  series_hint <- .gp_chr(series_ticker)
  payload <- .gp_as_payload(payload)
  if (is.null(payload)) return(.gp_empty_quote())
  markets <- payload
  if (is.list(payload) && !is.null(payload$markets)) markets <- payload$markets
  if (!is.list(markets) || length(markets) == 0L) return(.gp_empty_quote())
  rows <- list()
  n <- 0L
  for (m in markets) {
    if (!is.list(m)) next
    status <- tolower(.gp_chr(m$status))
    if (status %in% .GP_DROP_STATUS) next
    ticker <- .gp_chr(m$ticker %||% m$market_ticker)
    if (!nzchar(ticker)) next
    title <- .gp_chr(m$title %||% m$yes_sub_title %||% m$subtitle %||% ticker)
    yes <- .gp_kalshi_yes(m)
    if (!is.finite(yes)) next
    exp_at <- .gp_chr(m$expected_expiration_time %||% m$close_time)
    # Time-bound: skip if resolution/close already past.
    if (nzchar(exp_at)) {
      exp_t <- .gp_parse_time(exp_at)
      if (!is.na(exp_t) && exp_t < Sys.time()) next
    }
    ser <- .gp_kalshi_series_ticker(
      ticker,
      series_hint %||% .gp_chr(m$series_ticker)
    )
    n <- n + 1L
    rows[[n]] <- .gp_quote_row(
      venue = "kalshi",
      theme_id = theme_id,
      market_id = ticker,
      title = title,
      yes_prob = yes,
      updated_at = .gp_chr(m$close_time %||% m$expected_expiration_time),
      expires_at = exp_at,
      url = paste0("https://kalshi.com/markets/", ticker),
      raw_status = .gp_chr(m$status, "open"),
      series_ticker = ser
    )
    if (n >= as.integer(limit)) break
  }
  if (n == 0L) return(.gp_empty_quote())
  gp_filter_live_quotes(do.call(rbind, rows))
}

.gp_poly_parse_json_vec <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) || is.numeric(x)) return(x)
  if (is.character(x) && length(x) == 1L && grepl("^\\[", x)) {
    return(tryCatch(jsonlite::fromJSON(x), error = function(e) NULL))
  }
  if (is.character(x) && length(x) > 1L) return(x)
  NULL
}

# Yes is the outcome named Yes. The first price is not assumed to be Yes.
.gp_poly_yes_index <- function(market) {
  if (!is.list(market)) return(NA_integer_)
  outcomes <- .gp_poly_parse_json_vec(market$outcomes)
  if (is.null(outcomes)) return(NA_integer_)
  labs <- tolower(trimws(as.character(unlist(outcomes, use.names = FALSE))))
  yes_i <- which(labs %in% c("yes", "y"))
  if (length(yes_i) != 1L) return(NA_integer_)
  as.integer(yes_i[[1]])
}

.gp_poly_yes_prob <- function(market) {
  if (!is.list(market)) return(NA_real_)
  op <- .gp_poly_parse_json_vec(market$outcomePrices)
  outcomes <- .gp_poly_parse_json_vec(market$outcomes)
  if (is.null(op) || is.null(outcomes)) return(NA_real_)
  labs <- tolower(trimws(as.character(unlist(outcomes, use.names = FALSE))))
  prices <- unlist(op, use.names = FALSE)
  if (length(labs) != length(prices)) return(NA_real_)
  yes_i <- .gp_poly_yes_index(market)
  if (!is.finite(yes_i)) return(NA_real_)
  .gp_num01(prices[[yes_i]])
}

#' Polymarket CLOB Yes token for prices-history (never invents a token).
.gp_poly_yes_token <- function(market) {
  if (!is.list(market)) return(NA_character_)
  tokens <- .gp_poly_parse_json_vec(market$clobTokenIds %||% market$clob_token_ids)
  if (is.null(tokens)) return(NA_character_)
  toks <- as.character(unlist(tokens, use.names = FALSE))
  yes_i <- .gp_poly_yes_index(market)
  if (!is.finite(yes_i) || yes_i < 1L || yes_i > length(toks)) return(NA_character_)
  tok <- .gp_chr(toks[[yes_i]])
  if (!nzchar(tok)) NA_character_ else tok
}

.gp_keyword_hit <- function(text, keywords) {
  if (is.null(keywords) || length(keywords) == 0L) return(TRUE)
  hay <- tolower(paste(as.character(text), collapse = " "))
  any(vapply(keywords, function(k) {
    k <- tolower(.gp_chr(k))
    nzchar(k) && grepl(k, hay, fixed = TRUE)
  }, logical(1)))
}

#' Parse Polymarket public-search / events payload into MarketQuote rows.
gp_parse_polymarket_events <- function(payload, theme_id, limit = 3L, keywords = NULL) {
  theme_id <- .gp_chr(theme_id)
  payload <- .gp_as_payload(payload)
  if (is.null(payload)) return(.gp_empty_quote())
  events <- payload
  if (is.list(payload) && !is.null(payload$events)) events <- payload$events
  if (!is.list(events) || length(events) == 0L) return(.gp_empty_quote())
  rows <- list()
  n <- 0L
  for (ev in events) {
    if (!is.list(ev) || isTRUE(ev$closed)) next
    markets <- ev$markets %||% list()
    if (!is.list(markets)) markets <- list()
    if (length(markets) == 0L) markets <- list(ev)
    for (m in markets) {
      if (!is.list(m)) next
      if (isTRUE(m$closed) || identical(m$active, FALSE)) next
      mid <- .gp_chr(m$id %||% m$conditionId %||% m$slug %||% ev$id %||% ev$slug)
      title <- .gp_chr(m$question %||% m$title %||% ev$title %||% mid)
      if (!.gp_keyword_hit(c(ev$title, title), keywords)) next
      yes <- .gp_poly_yes_prob(m)
      if (!is.finite(yes)) next
      slug <- .gp_chr(ev$slug %||% m$slug)
      url <- if (nzchar(slug)) paste0("https://polymarket.com/event/", slug) else NA_character_
      exp_at <- .gp_chr(m$endDate %||% ev$endDate)
      if (nzchar(exp_at)) {
        exp_t <- .gp_parse_time(exp_at)
        if (!is.na(exp_t) && exp_t < Sys.time()) next
      }
      n <- n + 1L
      rows[[n]] <- .gp_quote_row(
        venue = "polymarket",
        theme_id = theme_id,
        market_id = mid,
        title = title,
        yes_prob = yes,
        updated_at = .gp_chr(m$updatedAt %||% ev$endDate %||% ev$creationDate),
        expires_at = exp_at,
        url = url,
        raw_status = if (isTRUE(ev$closed) || isTRUE(m$closed)) "closed" else "open",
        history_token = .gp_poly_yes_token(m)
      )
      if (n >= as.integer(limit)) break
    }
    if (n >= as.integer(limit)) break
  }
  if (n == 0L) return(.gp_empty_quote())
  gp_filter_live_quotes(do.call(rbind, rows))
}

#' Parse Kalshi candlesticks payload into Yes-prob time series (close only).
gp_parse_kalshi_candlesticks <- function(payload, theme_id, market_id, title,
                                         venue = "kalshi") {
  theme_id <- .gp_chr(theme_id)
  market_id <- .gp_chr(market_id)
  title <- .gp_chr(title, market_id)
  venue <- .gp_chr(venue, "kalshi")
  payload <- .gp_as_payload(payload)
  if (is.null(payload)) return(.gp_empty_series())
  candles <- NULL
  if (is.list(payload) && !is.null(payload$candlesticks)) {
    candles <- payload$candlesticks
  } else if (is.list(payload) && !is.null(payload$markets) && length(payload$markets) > 0L) {
    # Batch shape: { markets: [ { ticker?, candlesticks: [...] } ] }
    m0 <- payload$markets[[1]]
    if (is.list(m0)) candles <- m0$candlesticks
  }
  if (!is.list(candles) || length(candles) == 0L) return(.gp_empty_series())
  rows <- list()
  n <- 0L
  for (cndl in candles) {
    if (!is.list(cndl)) next
    ts_raw <- cndl$end_period_ts %||% cndl$ts %||% cndl$end_ts
    ts_num <- suppressWarnings(as.numeric(ts_raw)[1])
    if (!is.finite(ts_num) || ts_num <= 0) next
    price <- cndl$price %||% list()
    close <- .gp_num01(
      if (is.list(price)) price$close_dollars %||% price$close else cndl$close_dollars
    )
    if (!is.finite(close)) next
    n <- n + 1L
    rows[[n]] <- data.frame(
      venue = venue,
      theme_id = theme_id,
      market_id = market_id,
      title = title,
      ts = as.POSIXct(ts_num, origin = "1970-01-01", tz = "UTC"),
      yes_prob = close,
      stringsAsFactors = FALSE
    )
  }
  if (n == 0L) return(.gp_empty_series())
  out <- do.call(rbind, rows)
  out[order(out$ts), , drop = FALSE]
}

#' Parse Polymarket CLOB prices-history into Yes-prob time series.
gp_parse_polymarket_history <- function(payload, theme_id, market_id, title,
                                        venue = "polymarket") {
  theme_id <- .gp_chr(theme_id)
  market_id <- .gp_chr(market_id)
  title <- .gp_chr(title, market_id)
  venue <- .gp_chr(venue, "polymarket")
  payload <- .gp_as_payload(payload)
  if (is.null(payload)) return(.gp_empty_series())
  hist <- payload
  if (is.list(payload) && !is.null(payload$history)) hist <- payload$history
  if (!is.list(hist) || length(hist) == 0L) return(.gp_empty_series())
  rows <- list()
  n <- 0L
  for (pt in hist) {
    if (!is.list(pt)) next
    ts_num <- suppressWarnings(as.numeric(pt$t %||% pt$timestamp %||% pt$ts)[1])
    if (!is.finite(ts_num) || ts_num <= 0) next
    # Polymarket history can be seconds or ms.
    if (ts_num > 1e12) ts_num <- ts_num / 1000
    p <- .gp_num01(pt$p %||% pt$price %||% pt$yes_prob)
    if (!is.finite(p)) next
    n <- n + 1L
    rows[[n]] <- data.frame(
      venue = venue,
      theme_id = theme_id,
      market_id = market_id,
      title = title,
      ts = as.POSIXct(ts_num, origin = "1970-01-01", tz = "UTC"),
      yes_prob = p,
      stringsAsFactors = FALSE
    )
  }
  if (n == 0L) return(.gp_empty_series())
  out <- do.call(rbind, rows)
  out[order(out$ts), , drop = FALSE]
}

gp_fetch_kalshi_history <- function(market_id, theme_id = "", title = "",
                                    series_ticker = NULL, use_cache = TRUE,
                                    lookback_days = .GP_HISTORY_LOOKBACK_DAYS) {
  market_id <- .gp_chr(market_id)
  if (!nzchar(market_id)) return(.gp_empty_series())
  lookback_days <- max(7L, as.integer(lookback_days)[1])
  end_ts <- as.integer(Sys.time())
  start_ts <- end_ts - as.integer(lookback_days) * 86400L
  cache_key <- .gp_cache_key("khist", market_id, lookback_days, .GP_HISTORY_PERIOD_MIN)
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get_fresh(cache_key, .GP_DISCOVERY_TTL_SEC)
    if (is.data.frame(hit)) return(hit)
  }
  # Batch candlesticks does not require series_ticker.
  url <- sprintf(
    "%s/markets/candlesticks?market_tickers=%s&start_ts=%d&end_ts=%d&period_interval=%d",
    .GP_KALSHI_API,
    utils::URLencode(market_id, reserved = TRUE),
    as.integer(start_ts),
    as.integer(end_ts),
    as.integer(.GP_HISTORY_PERIOD_MIN)
  )
  res <- .gp_http_get_json(url, timeout_sec = 15)
  if (!isTRUE(res$ok)) return(.gp_empty_series())
  out <- gp_parse_kalshi_candlesticks(
    res$data,
    theme_id = theme_id,
    market_id = market_id,
    title = title
  )
  if (isTRUE(use_cache) && nrow(out) > 0L) {
    .gp_cache_set_fresh(cache_key, out, layer = "discovery")
  }
  out
}

gp_fetch_polymarket_history <- function(history_token, theme_id = "",
                                        market_id = "", title = "",
                                        use_cache = TRUE) {
  token <- .gp_chr(history_token)
  if (!nzchar(token)) return(.gp_empty_series())
  cache_key <- .gp_cache_key("phist", token)
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get_fresh(cache_key, .GP_DISCOVERY_TTL_SEC)
    if (is.data.frame(hit)) return(hit)
  }
  url <- paste0(
    .GP_POLY_CLOB,
    "/prices-history?market=",
    utils::URLencode(token, reserved = TRUE),
    "&interval=1d&fidelity=60"
  )
  res <- .gp_http_get_json(url, timeout_sec = 15)
  if (!isTRUE(res$ok)) return(.gp_empty_series())
  out <- gp_parse_polymarket_history(
    res$data,
    theme_id = theme_id,
    market_id = .gp_chr(market_id, token),
    title = title
  )
  if (isTRUE(use_cache) && nrow(out) > 0L) {
    .gp_cache_set_fresh(cache_key, out, layer = "discovery")
  }
  out
}

#' Attach venue Yes-prob histories for a theme pair. Fail closed / honest empty.
gp_attach_theme_series <- function(pair, use_cache = TRUE,
                                   per_venue = .GP_HISTORY_PER_VENUE) {
  if (!is.list(pair)) {
    return(list(series = .gp_empty_series()))
  }
  per_venue <- max(1L, as.integer(per_venue)[1])
  parts <- list()
  kq <- if (is.data.frame(pair$kalshi)) pair$kalshi else .gp_empty_quote()
  pq <- if (is.data.frame(pair$polymarket)) pair$polymarket else .gp_empty_quote()
  if (nrow(kq) > 0L) {
    n <- min(nrow(kq), per_venue)
    for (i in seq_len(n)) {
      row <- kq[i, , drop = FALSE]
      ser <- gp_fetch_kalshi_history(
        market_id = row$market_id[[1]],
        theme_id = pair$theme_id %||% row$theme_id[[1]],
        title = row$title[[1]],
        series_ticker = if ("series_ticker" %in% names(row)) row$series_ticker[[1]] else NULL,
        use_cache = use_cache
      )
      if (nrow(ser) >= as.integer(.GP_HISTORY_MIN_POINTS)) parts[[length(parts) + 1L]] <- ser
    }
  }
  if (nrow(pq) > 0L) {
    n <- min(nrow(pq), per_venue)
    for (i in seq_len(n)) {
      row <- pq[i, , drop = FALSE]
      tok <- if ("history_token" %in% names(row)) .gp_chr(row$history_token[[1]]) else ""
      if (!nzchar(tok)) next
      ser <- gp_fetch_polymarket_history(
        history_token = tok,
        theme_id = pair$theme_id %||% row$theme_id[[1]],
        market_id = row$market_id[[1]],
        title = row$title[[1]],
        use_cache = use_cache
      )
      if (nrow(ser) >= as.integer(.GP_HISTORY_MIN_POINTS)) parts[[length(parts) + 1L]] <- ser
    }
  }
  series <- if (length(parts)) do.call(rbind, parts) else .gp_empty_series()
  pair$series <- series
  pair
}

.gp_series_market_keys <- function(series) {
  if (!is.data.frame(series) || nrow(series) == 0L) return(character(0))
  unique(paste(series$venue, series$market_id, sep = "\t"))
}

#' Split attached theme series into one data.frame per market (venue + market_id).
gp_split_series_by_market <- function(series) {
  keys <- .gp_series_market_keys(series)
  if (!length(keys)) return(list())
  out <- vector("list", length(keys))
  names(out) <- keys
  for (k in keys) {
    sub <- series[paste(series$venue, series$market_id, sep = "\t") == k, , drop = FALSE]
    sub <- sub[is.finite(sub$yes_prob) & !is.na(sub$ts), , drop = FALSE]
    if (nrow(sub) < as.integer(.GP_HISTORY_MIN_POINTS)) next
    out[[k]] <- sub[order(sub$ts), , drop = FALSE]
  }
  out[!vapply(out, is.null, logical(1))]
}

#' Venue chart chrome cues (Kalshi / Polymarket live market pages).
#' One series per chart — never invents points; style only.
gp_venue_chart_theme <- function(venue = "kalshi") {
  colors <- gp_brand_colors()
  v <- tolower(.gp_chr(venue, "kalshi"))
  if (identical(v, "polymarket")) {
    return(list(
      venue = "polymarket",
      venue_lab = "Polymarket",
      line = colors$polymarket,
      # Soft Yes-area under a single outcome line (readable; multi-outcome refs are line-only).
      fill = "rgba(46, 92, 255, 0.10)",
      use_fill = TRUE,
      line_width = 2.2,
      paper_bg = "#FFFFFF",
      plot_bg = "#FFFFFF",
      grid = "#E5E7EB",
      axis_font = "#9CA3AF",
      source_key = "gp_chart_source_poly"
    ))
  }
  list(
    venue = "kalshi",
    venue_lab = "Kalshi",
    line = colors$kalshi,
    fill = "rgba(0, 214, 108, 0.08)",
    use_fill = TRUE,
    line_width = 2.6,
    paper_bg = "#FFFFFF",
    plot_bg = "#FFFFFF",
    grid = "#E5E7EB",
    axis_font = "#9CA3AF",
    source_key = "gp_chart_source_kalshi"
  )
}

.gp_chart_header_ui <- function(title, last_pct, theme, locale = "en") {
  pct_txt <- if (is.finite(last_pct)) sprintf("%.0f%%", round(last_pct)) else "—"
  htmltools::tags$div(
    class = "ynow-gp-chart-head",
    htmltools::tags$div(
      class = "ynow-gp-chart-head-main",
      htmltools::tags$span(
        class = "ynow-gp-chart-dot",
        style = sprintf("background:%s;", theme$line)
      ),
      htmltools::tags$span(class = "ynow-gp-chart-title", title),
      htmltools::tags$span(
        class = "ynow-gp-chart-chance",
        style = sprintf("color:%s;", theme$line),
        pct_txt
      )
    ),
    htmltools::tags$div(
      class = "ynow-gp-chart-head-meta",
      htmltools::tags$span(class = "ynow-gp-chart-chance-label", .gp_label("gp_chart_chance", locale)),
      htmltools::tags$span(class = "ynow-gp-chart-venue-mark", theme$venue_lab)
    )
  )
}

#' Native plotly line chart for **one** prediction market (no ggplotly, no overlay).
#' Visual chrome leans Kalshi / Polymarket live market pages (right Yes% axis,
#' horizontal-only grid, brand line + soft fill, header Chance %).
#' If `series` contains multiple markets, only the first market is plotted — callers
#' should prefer `gp_build_theme_market_charts()` to stack one chart per market.
gp_build_line_chart <- function(series, locale = "en", height = 240) {
  locale <- .gp_chr(locale, "en")
  empty_node <- function() {
    htmltools::tags$p(
      class = "ynow-gp-empty ynow-gp-chart-empty",
      .gp_label("gp_chart_empty", locale)
    )
  }
  if (!is.data.frame(series) || nrow(series) < as.integer(.GP_HISTORY_MIN_POINTS)) {
    return(empty_node())
  }
  if (!requireNamespace("plotly", quietly = TRUE)) return(empty_node())
  keys <- .gp_series_market_keys(series)
  if (!length(keys)) return(empty_node())
  # Single-market only: never consolidate Kalshi + Polymarket (or multi-market) overlays.
  sub <- series[paste(series$venue, series$market_id, sep = "\t") == keys[[1]], , drop = FALSE]
  sub <- sub[is.finite(sub$yes_prob) & !is.na(sub$ts), , drop = FALSE]
  if (nrow(sub) < as.integer(.GP_HISTORY_MIN_POINTS)) return(empty_node())
  sub <- sub[order(sub$ts), , drop = FALSE]
  sub <- .gp_series_downsample(sub, max_n = 400L)
  venue <- .gp_chr(sub$venue[[1]], "kalshi")
  theme <- gp_venue_chart_theme(venue)
  title <- .gp_chr(sub$title[[1]], sub$market_id[[1]])
  lab <- title
  if (nchar(lab) > 72L) lab <- paste0(substr(lab, 1L, 69L), "…")
  y_pct <- 100 * as.numeric(sub$yes_prob)
  last_pct <- utils::tail(y_pct[is.finite(y_pct)], 1)
  if (!length(last_pct)) last_pct <- NA_real_
  fig <- plotly::plot_ly(height = as.integer(height))
  trace_args <- list(
    p = fig,
    x = sub$ts,
    y = y_pct,
    type = "scatter",
    mode = "lines",
    name = lab,
    line = list(color = theme$line, width = theme$line_width, shape = "linear"),
    hovertemplate = paste0(
      "<b>", lab, "</b><br>",
      "%{x|%b %d, %Y}<br>",
      "Yes %{y:.1f}%",
      "<extra></extra>"
    ),
    showlegend = FALSE
  )
  if (isTRUE(theme$use_fill)) {
    trace_args$fill <- "tozeroy"
    trace_args$fillcolor <- theme$fill
  }
  fig <- do.call(plotly::add_trace, trace_args)
  fig <- plotly::layout(
    fig,
    # Title lives in HTML header (venue-page legend + Chance %), not plotly title.
    title = NULL,
    xaxis = list(
      title = "",
      showgrid = FALSE,
      zeroline = FALSE,
      showline = FALSE,
      ticks = "",
      tickfont = list(size = 11, color = theme$axis_font, family = "Inter, system-ui, sans-serif"),
      automargin = TRUE
    ),
    yaxis = list(
      title = "",
      side = "right",
      range = c(0, 100),
      dtick = 25,
      tick0 = 0,
      ticksuffix = "%",
      tickfont = list(size = 11, color = theme$axis_font, family = "Inter, system-ui, sans-serif"),
      showgrid = TRUE,
      gridcolor = theme$grid,
      gridwidth = 1,
      zeroline = FALSE,
      showline = FALSE,
      ticks = "",
      automargin = TRUE,
      fixedrange = TRUE
    ),
    margin = list(l = 8, r = 44, t = 8, b = 28),
    paper_bgcolor = theme$paper_bg,
    plot_bgcolor = theme$plot_bg,
    hovermode = "x",
    hoverlabel = list(
      bgcolor = "#FFFFFF",
      bordercolor = "#E5E7EB",
      font = list(size = 12, color = "#111827", family = "Inter, system-ui, sans-serif")
    ),
    showlegend = FALSE,
    annotations = list(
      list(
        text = .gp_label(theme$source_key, locale),
        xref = "paper", yref = "paper",
        x = 0, y = -0.12,
        xanchor = "left", yanchor = "top",
        showarrow = FALSE,
        font = list(size = 10, color = "#9CA3AF", family = "Inter, system-ui, sans-serif")
      )
    )
  )
  fig <- plotly::config(fig, displayModeBar = FALSE, responsive = TRUE, displaylogo = FALSE)
  htmltools::tags$div(
    class = paste(
      "ynow-gp-chart-widget ynow-gp-market-chart-widget",
      paste0("ynow-gp-venue-", theme$venue)
    ),
    `data-venue` = theme$venue,
    `data-venue-style` = theme$venue,
    `data-market-id` = .gp_chr(sub$market_id[[1]]),
    .gp_chart_header_ui(lab, last_pct, theme, locale),
    fig
  )
}

#' One line chart per prediction market under a theme (stacked; never merged).
gp_build_theme_market_charts <- function(series, locale = "en", height = 240) {
  locale <- .gp_chr(locale, "en")
  parts <- gp_split_series_by_market(series)
  if (!length(parts)) {
    return(htmltools::tags$p(
      class = "ynow-gp-empty ynow-gp-chart-empty",
      .gp_label("gp_chart_empty", locale)
    ))
  }
  charts <- lapply(names(parts), function(k) {
    htmltools::tags$div(
      class = "ynow-gp-market-chart",
      `data-market-key` = k,
      gp_build_line_chart(parts[[k]], locale = locale, height = height)
    )
  })
  htmltools::tags$div(class = "ynow-gp-charts", charts)
}

gp_fetch_kalshi_theme <- function(theme_id, series_tickers, limit = 3L, use_cache = TRUE,
                                  layer = "quote") {
  theme_id <- .gp_chr(theme_id)
  series_tickers <- unique(as.character(unlist(series_tickers, use.names = FALSE)))
  series_tickers <- series_tickers[nzchar(series_tickers)]
  if (length(series_tickers) == 0L) {
    return(list(ok = FALSE, quotes = .gp_empty_quote(), error = "no_series"))
  }
  cache_key <- .gp_cache_key("kalshi", theme_id, series_tickers)
  ttl <- if (identical(layer, "discovery")) .GP_DISCOVERY_TTL_SEC else .GP_QUOTE_TTL_SEC
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get_fresh(cache_key, ttl)
    if (is.list(hit) && isTRUE(hit$ok)) {
      hit$quotes <- gp_filter_live_quotes(hit$quotes)
      return(hit)
    }
  }
  collected <- list()
  last_err <- NULL
  for (ser in series_tickers) {
    url <- sprintf(
      "https://external-api.kalshi.com/trade-api/v2/markets?status=open&limit=%d&series_ticker=%s",
      max(5L, as.integer(limit)),
      utils::URLencode(ser, reserved = TRUE)
    )
    res <- .gp_http_get_json(url)
    if (!isTRUE(res$ok)) {
      last_err <- res$error %||% "fetch_failed"
      next
    }
    q <- gp_parse_kalshi_markets(res$data, theme_id = theme_id, limit = limit)
    if (nrow(q) > 0L) collected[[length(collected) + 1L]] <- q
    if (sum(vapply(collected, nrow, 1L)) >= as.integer(limit)) break
  }
  if (length(collected) == 0L) {
    out <- list(ok = FALSE, quotes = .gp_empty_quote(), error = last_err %||% "empty")
    return(out)
  }
  quotes <- gp_filter_live_quotes(do.call(rbind, collected))
  quotes <- quotes[!duplicated(quotes$market_id), , drop = FALSE]
  if (nrow(quotes) > as.integer(limit)) {
    quotes <- quotes[seq_len(as.integer(limit)), , drop = FALSE]
  }
  out <- list(ok = TRUE, quotes = quotes, error = NULL)
  if (isTRUE(use_cache)) .gp_cache_set_fresh(cache_key, out, layer = layer)
  out
}

gp_fetch_polymarket_theme <- function(theme_id, queries, limit = 3L, use_cache = TRUE,
                                      keywords = NULL, layer = "quote") {
  theme_id <- .gp_chr(theme_id)
  queries <- unique(as.character(unlist(queries, use.names = FALSE)))
  queries <- queries[nzchar(queries)]
  if (length(queries) == 0L) {
    return(list(ok = FALSE, quotes = .gp_empty_quote(), error = "no_query"))
  }
  cache_key <- .gp_cache_key("poly", theme_id, queries)
  ttl <- if (identical(layer, "discovery")) .GP_DISCOVERY_TTL_SEC else .GP_QUOTE_TTL_SEC
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get_fresh(cache_key, ttl)
    if (is.list(hit) && isTRUE(hit$ok)) {
      hit$quotes <- gp_filter_live_quotes(hit$quotes)
      return(hit)
    }
  }
  collected <- list()
  last_err <- NULL
  for (q in queries) {
    url <- paste0(
      "https://gamma-api.polymarket.com/public-search?q=",
      utils::URLencode(q, reserved = TRUE)
    )
    res <- .gp_http_get_json(url)
    if (!isTRUE(res$ok)) {
      last_err <- res$error %||% "fetch_failed"
      next
    }
    parsed <- gp_parse_polymarket_events(
      res$data, theme_id = theme_id, limit = limit, keywords = keywords
    )
    if (nrow(parsed) > 0L) collected[[length(collected) + 1L]] <- parsed
    if (sum(vapply(collected, nrow, 1L)) >= as.integer(limit)) break
  }
  if (length(collected) == 0L) {
    return(list(ok = FALSE, quotes = .gp_empty_quote(), error = last_err %||% "empty"))
  }
  quotes <- gp_filter_live_quotes(do.call(rbind, collected))
  quotes <- quotes[!duplicated(quotes$market_id), , drop = FALSE]
  if (nrow(quotes) > as.integer(limit)) {
    quotes <- quotes[seq_len(as.integer(limit)), , drop = FALSE]
  }
  out <- list(ok = TRUE, quotes = quotes, error = NULL)
  if (isTRUE(use_cache)) .gp_cache_set_fresh(cache_key, out, layer = layer)
  out
}

#' Crowd agreement between two venue mid probs. Not a fused truth.
gp_crowd_agreement <- function(kalshi_probs, polymarket_probs) {
  k <- suppressWarnings(as.numeric(kalshi_probs))
  p <- suppressWarnings(as.numeric(polymarket_probs))
  k <- k[is.finite(k)]
  p <- p[is.finite(p)]
  if (length(k) == 0L || length(p) == 0L) {
    return(list(
      status = "insufficient",
      gap = NA_real_,
      kalshi_mid = if (length(k)) mean(k) else NA_real_,
      polymarket_mid = if (length(p)) mean(p) else NA_real_,
      simple_mean = NA_real_
    ))
  }
  km <- mean(k)
  pm <- mean(p)
  gap <- abs(km - pm)
  status <- if (gap <= .GP_AGREE_PP) {
    "agree"
  } else if (gap >= .GP_DIVERGE_PP) {
    "diverge"
  } else {
    "mixed"
  }
  list(
    status = status,
    gap = gap,
    kalshi_mid = km,
    polymarket_mid = pm,
    simple_mean = mean(c(km, pm))
  )
}

#' Fetch Snapshot for all themes. Fail closed per venue. Mode selects theme set.
#' @param trigger crawl trigger id (see gp_crawl_policy); hard triggers invalidate first.

.gp_fmt_pct <- function(p) {
  p <- .gp_num01(p)
  if (!is.finite(p)) return("—")
  sprintf("%.0f%%", round(100 * p))
}

#' Compact last-print list (secondary). Primary board view is the line chart.
.gp_quote_cards <- function(quotes, venue_color, locale = "en") {
  if (!is.data.frame(quotes) || nrow(quotes) == 0L) {
    return(htmltools::tags$p(
      class = "ynow-gp-empty",
      .gp_label("gp_empty_venue", locale)
    ))
  }
  cards <- lapply(seq_len(nrow(quotes)), function(i) {
    row <- quotes[i, , drop = FALSE]
    title_node <- if (nzchar(.gp_chr(row$url[[1]]))) {
      htmltools::tags$a(
        href = row$url[[1]],
        target = "_blank",
        rel = "noopener noreferrer",
        .gp_chr(row$title[[1]])
      )
    } else {
      htmltools::tags$span(.gp_chr(row$title[[1]]))
    }
    htmltools::tags$div(
      class = "ynow-gp-quote",
      style = sprintf("border-left-color:%s;", venue_color),
      htmltools::tags$div(class = "ynow-gp-quote-title", title_node),
      htmltools::tags$div(
        class = "ynow-gp-quote-prob",
        style = sprintf("color:%s;", venue_color),
        .gp_fmt_pct(row$yes_prob[[1]])
      )
    )
  })
  htmltools::tagList(cards)
}

.gp_theme_row_ui <- function(pair, locale = "en") {
  colors <- gp_brand_colors()
  agree <- pair$agreement %||% gp_crowd_agreement(
    if (is.data.frame(pair$kalshi)) pair$kalshi$yes_prob else numeric(0),
    if (is.data.frame(pair$polymarket)) pair$polymarket$yes_prob else numeric(0)
  )
  status <- .gp_chr(agree$status, "insufficient")
  status_key <- paste0("gp_crowd_", status)
  gap_txt <- if (is.finite(agree$gap %||% NA_real_)) {
    sprintf("%s %s", .gp_label("gp_crowd_gap", locale), .gp_fmt_pct(agree$gap))
  } else {
    ""
  }
  mean_txt <- if (is.finite(agree$simple_mean %||% NA_real_)) {
    sprintf(
      "%s %s",
      .gp_label("gp_crowd_simple_mean", locale),
      .gp_fmt_pct(agree$simple_mean)
    )
  } else {
    ""
  }
  series <- if (is.data.frame(pair$series)) pair$series else .gp_empty_series()
  htmltools::tags$div(
    class = "ynow-gp-theme",
    `data-theme` = .gp_chr(pair$theme_id),
    htmltools::tags$div(
      class = "ynow-gp-theme-head",
      htmltools::tags$h4(
        class = "ynow-gp-theme-title",
        id = paste0("ynow_gp_theme_", .gp_chr(pair$theme_id)),
        .gp_label(pair$label_key %||% "gp_theme_fed_rates", locale)
      ),
      htmltools::tags$div(
        class = "ynow-gp-agree",
        htmltools::tags$span(
          class = paste0("ynow-gp-agree-badge ynow-gp-agree-", status),
          .gp_label(status_key, locale)
        ),
        if (nzchar(gap_txt)) htmltools::tags$span(class = "ynow-gp-agree-meta", gap_txt),
        if (nzchar(mean_txt)) htmltools::tags$span(class = "ynow-gp-agree-meta", mean_txt)
      )
    ),
    # One plotly line chart per market (stacked under theme; never a merged overlay).
    htmltools::tags$div(
      class = "ynow-gp-chart",
      gp_build_theme_market_charts(series, locale = locale, height = 220)
    ),
    htmltools::tags$p(
      class = "ynow-gp-last-print-label",
      .gp_label("gp_last_print", locale)
    ),
    htmltools::tags$div(
      class = "ynow-gp-cols",
      htmltools::tags$div(
        class = "ynow-gp-col ynow-gp-col-kalshi",
        htmltools::tags$div(
          class = "ynow-gp-venue-label",
          style = sprintf("color:%s;border-bottom-color:%s;", colors$kalshi, colors$kalshi),
          "Kalshi"
        ),
        .gp_quote_cards(pair$kalshi, colors$kalshi, locale)
      ),
      htmltools::tags$div(
        class = "ynow-gp-col ynow-gp-col-polymarket",
        htmltools::tags$div(
          class = "ynow-gp-venue-label",
          style = sprintf(
            "color:%s;border-bottom-color:%s;",
            colors$polymarket,
            colors$polymarket
          ),
          "Polymarket"
        ),
        .gp_quote_cards(pair$polymarket, colors$polymarket, locale)
      )
    )
  )
}


# ---- Boards: Economic / Finance / Company ---------------------------------
# Standalone Global Prediction page. Sources map to public Kalshi series
# categories and Polymarket tag/search feeds (monthly+ for Eco/Finance).

.GP_FREQ_MONTHLY_PLUS <- c("monthly", "quarterly", "annual", "yearly")
# Company events are often custom / one_off on Kalshi.
.GP_FREQ_COMPANY <- c("monthly", "quarterly", "annual", "yearly", "custom", "one_off")

gp_board_ids <- function() c("economic", "finance", "company")

gp_norm_board <- function(board = "economic") {
  b <- tolower(.gp_chr(board, "economic"))
  if (b %in% c("econ", "economics", "macro")) return("economic")
  if (b %in% c("fin", "financials", "financial")) return("finance")
  if (b %in% c("co", "companies", "equity", "equities", "stock", "stocks")) return("company")
  if (b %in% gp_board_ids()) b else "economic"
}

#' Source matrix for each board (public APIs mirroring the listed venue pages).
gp_board_sources <- function(board = "economic") {
  board <- gp_norm_board(board)
  if (identical(board, "finance")) {
    return(list(
      board = board,
      # Kalshi Financials monthly + annual (user: /financials/frequency/monthly|annual)
      kalshi_categories = c("Financials"),
      kalshi_freq = .GP_FREQ_MONTHLY_PLUS,
      # Polymarket /finance/monthly + /finance/indicies (site slug typo kept)
      poly_tags = c(
        "monthly", "finance", "indices", "indicies",
        "spx", "nasdaq", "equities", "stocks"
      ),
      poly_queries = c(
        "S&P 500 monthly", "Nasdaq monthly", "Treasury yield",
        "stock index", "finance monthly", "VIX", "credit spread",
        "IPO", "dollar index"
      ),
      company_filter = FALSE,
      theme_buckets = list(
        list(id = "rates_credit", label_key = "gp_theme_rates_credit",
             kw = c("treasury", "yield", "rate", "fed funds", "credit", "bond",
                    "spread", "high yield")),
        list(id = "equity_index", label_key = "gp_theme_equity_index",
             kw = c("s&p", "spx", "nasdaq", "russell", "dow", "index", "indices",
                    "indicies", "taiex", "hang seng")),
        list(id = "vol_credit", label_key = "gp_theme_vol_credit",
             kw = c("vix", "volatility", "vol ", "stress", "default", "cds")),
        list(id = "fx_commod_fin", label_key = "gp_theme_fx_markets",
             kw = c("fx", "dollar", "dxy", "euro", "yen", "currency", "forex")),
        list(id = "ipo_ma", label_key = "gp_theme_ipo_ma",
             kw = c("ipo", "merger", "acquire", "m&a", "spac", "takeover")),
        list(id = "fin_other", label_key = "gp_theme_fin_other", kw = character(0))
      )
    ))
  }
  if (identical(board, "company")) {
    return(list(
      board = board,
      # Kalshi company-reports + Companies category; keep Financials for equity single-names
      kalshi_categories = c("Companies", "Financials"),
      kalshi_freq = .GP_FREQ_COMPANY,
      # Polymarket /tech + /finance/stocks + /finance/earnings
      poly_tags = c("tech", "stocks", "earnings", "equities"),
      poly_queries = c(
        "earnings", "TSMC", "Apple", "NVIDIA", "Microsoft", "Meta",
        "Taiwan Semiconductor", "stock price", "2330", "Hon Hai",
        "Amazon", "Tesla", "Broadcom"
      ),
      company_filter = TRUE,
      theme_buckets = list(
        list(id = "us_mega", label_key = "gp_theme_us_mega",
             kw = c("apple", "aapl", "microsoft", "msft", "amazon", "amzn",
                    "google", "alphabet", "googl", "meta", "facebook",
                    "tesla", "tsla", "netflix", "nflx", "costco", "walmart")),
        list(id = "semis", label_key = "gp_theme_semis",
             kw = c("nvidia", "nvda", "broadcom", "avgo", "amd", "intel", "intc",
                    "tsmc", "taiwan semiconductor", "2330", "umc", "2303",
                    "mediatek", "2454", "asml", "semiconductor", "chip")),
        list(id = "tw_listed", label_key = "gp_theme_tw_listed",
             kw = c("hon hai", "foxconn", "2317", "taiex", "taiwan", ".tw",
                    "quanta", "2382", "pegatron", "4938", "largan", "3008",
                    "chunghwa", "2412", "fubon", "cathay")),
        list(id = "earnings", label_key = "gp_theme_earnings",
             kw = c("earn", "eps", "revenue", "quarter", "beat", "miss",
                    "guidance", "print")),
        list(id = "co_other", label_key = "gp_theme_co_other", kw = character(0))
      )
    ))
  }
  # economic — Kalshi Economics + commodities; Polymarket /economy + /finance/commodities
  list(
    board = "economic",
    kalshi_categories = c("Economics", "Commodities"),
    kalshi_freq = .GP_FREQ_MONTHLY_PLUS,
    poly_tags = c("economy", "commodities", "economics"),
    poly_queries = c(
      "US economy", "CPI inflation", "Fed rate", "recession",
      "GDP", "unemployment", "housing", "tariff", "oil", "gold", "commodities"
    ),
    company_filter = FALSE,
    theme_buckets = list(
      list(id = "fed_rates", label_key = "gp_theme_fed_rates",
           kw = c("fed", "fomc", "rate cut", "rate hike", "federal reserve",
                  "central bank", "interest rate")),
      list(id = "inflation_cpi", label_key = "gp_theme_inflation_cpi",
           kw = c("cpi", "inflation", "ppi", "pce", "consumer price", "deflation")),
      list(id = "growth_jobs", label_key = "gp_theme_growth_jobs",
           kw = c("gdp", "growth", "jobs", "nfp", "payroll", "jolts", "employment")),
      list(id = "recession_labor", label_key = "gp_theme_recession_labor",
           kw = c("recession", "unemployment", "layoff", "soft landing", "hard landing")),
      list(id = "housing", label_key = "gp_theme_housing",
           kw = c("housing", "home price", "mortgage", "hpi", "rent", "case-shiller")),
      list(id = "energy_commod", label_key = "gp_theme_energy_commod",
           kw = c("oil", "wti", "brent", "gas", "gold", "copper", "silver",
                  "commodity", "commodities", "crude")),
      list(id = "trade_policy", label_key = "gp_theme_trade_policy",
           kw = c("tariff", "trade war", "import", "export", "customs", "sanction")),
      list(id = "econ_other", label_key = "gp_theme_econ_other", kw = character(0))
    )
  )
}

#' Legacy theme registry (tests / reclassify). Maps onto Economic board buckets.
gp_theme_registry <- function(market_mode = "US") {
  src <- gp_board_sources("economic")
  buckets <- src$theme_buckets
  n <- length(buckets)
  data.frame(
    theme_id = vapply(buckets, function(b) b$id, character(1)),
    label_key = vapply(buckets, function(b) b$label_key, character(1)),
    breadth_tier = as.integer(seq_len(n)),
    breadth_ord = as.integer(seq_len(n)),
    kalshi_series = I(replicate(n, character(0), simplify = FALSE)),
    polymarket_queries = I(replicate(n, character(0), simplify = FALSE)),
    keywords = I(lapply(buckets, function(b) b$kw)),
    feex_topics = I(as.list(vapply(buckets, function(b) b$id, character(1)))),
    stringsAsFactors = FALSE
  )
}

.gp_freq_ok <- function(freq, allow) {
  f <- tolower(.gp_chr(freq))
  if (!nzchar(f)) return(FALSE)
  f %in% tolower(as.character(allow))
}

#' TW/US listed-company heuristic for Company board (titles / tickers).
gp_is_tw_us_listed_title <- function(title) {
  t <- tolower(.gp_chr(title))
  if (!nzchar(t)) return(FALSE)
  # Explicit exchange / ADR / TWSE cues
  if (grepl("\\.(tw|two)\\b", t) || grepl("\\btwse\\b|\\btaiex\\b|\\botc\\b", t)) return(TRUE)
  if (grepl("\\bnyse\\b|\\bnasdaq\\b|\\bamex\\b", t)) return(TRUE)
  # Parenthetical ticker (AAPL) / spaced ticker tokens
  if (grepl("\\(([A-Z]{1,5})\\)", .gp_chr(title))) return(TRUE)
  us <- c(
    "apple", "aapl", "microsoft", "msft", "nvidia", "nvda", "amazon", "amzn",
    "alphabet", "google", "googl", "meta", "facebook", "tesla", "tsla",
    "broadcom", "avgo", "netflix", "nflx", "amd", "intel", "intc", "costco",
    "jpmorgan", "jpm", "goldman", "gs ", "berkshire", "exxon", "xom",
    "unitedhealth", "unh", "visa", "mastercard", "walmart", "wmt",
    "asml", "adobe", "salesforce", "crm", "oracle", "orcl", "cisco", "csco",
    "pfizer", "coca-cola", "pepsico", "disney", "nike"
  )
  tw <- c(
    "tsmc", "taiwan semiconductor", "2330", "hon hai", "foxconn", "2317",
    "mediatek", "2454", "umc", "2303", "ase", "3711", "quanta", "2382",
    "pegatron", "4938", "largan", "3008", "realtek", "2379", "novatek",
    "3034", "chunghwa telecom", "2412", "fubon", "cathay", "taiwan",
    "delta electronics", "2308", "taiwan mobile", "3045", "cht "
  )
  if (any(vapply(us, function(k) grepl(k, t, fixed = TRUE), logical(1)))) return(TRUE)
  if (any(vapply(tw, function(k) grepl(k, t, fixed = TRUE), logical(1)))) return(TRUE)
  FALSE
}

.gp_bucket_theme_id <- function(title, buckets) {
  best_id <- buckets[[length(buckets)]]$id
  best <- 0L
  for (b in buckets) {
    kw <- b$kw
    if (!length(kw)) next
    sc <- .gp_theme_keyword_score(title, kw)
    if (sc > best) {
      best <- sc
      best_id <- b$id
    }
  }
  best_id
}

gp_fetch_kalshi_series_list <- function(category, use_cache = TRUE) {
  category <- .gp_chr(category)
  cache_key <- .gp_cache_key("kseries", category)
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get_fresh(cache_key, .GP_DISCOVERY_TTL_SEC)
    if (is.list(hit) && isTRUE(hit$ok)) return(hit)
  }
  url <- sprintf(
    "%s/series?limit=200&category=%s",
    .GP_KALSHI_API,
    utils::URLencode(category, reserved = TRUE)
  )
  res <- .gp_http_get_json(url, timeout_sec = 20)
  if (!isTRUE(res$ok)) {
    return(list(ok = FALSE, series = list(), error = res$error %||% "fetch_failed"))
  }
  series <- res$data$series %||% list()
  if (!is.list(series)) series <- list()
  out <- list(ok = TRUE, series = series, error = NULL)
  if (isTRUE(use_cache)) .gp_cache_set_fresh(cache_key, out, layer = "discovery")
  out
}

gp_fetch_kalshi_board <- function(board = "economic", limit_quotes = 24L, use_cache = TRUE) {
  src <- gp_board_sources(board)
  cache_key <- .gp_cache_key("kboard", src$board, paste(src$kalshi_categories, collapse = ""))
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get_fresh(cache_key, .GP_QUOTE_TTL_SEC)
    if (is.list(hit) && isTRUE(hit$ok)) {
      hit$quotes <- gp_filter_live_quotes(hit$quotes)
      return(hit)
    }
  }
  collected <- list()
  last_err <- NULL
  for (cat in src$kalshi_categories) {
    sl <- gp_fetch_kalshi_series_list(cat, use_cache = use_cache)
    if (!isTRUE(sl$ok)) {
      last_err <- sl$error %||% "series_failed"
      next
    }
    series <- sl$series
    # Prefer monthly+; keep order from API
    keep <- list()
    for (s in series) {
      if (!is.list(s)) next
      if (!.gp_freq_ok(s$frequency, src$kalshi_freq)) next
      keep[[length(keep) + 1L]] <- s
    }
    # Cap series probes per category to control rate
    if (length(keep) > 36L) keep <- keep[seq_len(36L)]
    for (s in keep) {
      ticker <- .gp_chr(s$ticker)
      if (!nzchar(ticker)) next
      theme_guess <- .gp_bucket_theme_id(
        paste(s$title %||% "", ticker),
        src$theme_buckets
      )
      url <- sprintf(
        "%s/markets?status=open&limit=5&series_ticker=%s&mve_filter=exclude",
        .GP_KALSHI_API,
        utils::URLencode(ticker, reserved = TRUE)
      )
      res <- .gp_http_get_json(url, timeout_sec = 12)
      if (!isTRUE(res$ok)) {
        last_err <- res$error %||% "markets_failed"
        next
      }
      q <- gp_parse_kalshi_markets(
        res$data, theme_id = theme_guess, limit = 3L, series_ticker = ticker
      )
      if (nrow(q) == 0L) next
      if (isTRUE(src$company_filter)) {
        ok_rows <- vapply(seq_len(nrow(q)), function(i) {
          gp_is_tw_us_listed_title(q$title[[i]])
        }, logical(1))
        q <- q[ok_rows, , drop = FALSE]
      }
      if (nrow(q) > 0L) collected[[length(collected) + 1L]] <- q
      n_have <- sum(vapply(collected, nrow, 1L))
      if (n_have >= as.integer(limit_quotes)) break
    }
    if (sum(vapply(collected, nrow, 1L)) >= as.integer(limit_quotes)) break
  }
  if (!length(collected)) {
    return(list(ok = FALSE, quotes = .gp_empty_quote(), error = last_err %||% "empty"))
  }
  quotes <- gp_filter_live_quotes(do.call(rbind, collected))
  quotes <- quotes[!duplicated(quotes$market_id), , drop = FALSE]
  if (nrow(quotes) > as.integer(limit_quotes)) {
    quotes <- quotes[seq_len(as.integer(limit_quotes)), , drop = FALSE]
  }
  out <- list(ok = TRUE, quotes = quotes, error = NULL)
  if (isTRUE(use_cache)) .gp_cache_set_fresh(cache_key, out, layer = "quote")
  out
}

gp_fetch_polymarket_board <- function(board = "economic", limit_quotes = 24L, use_cache = TRUE) {
  src <- gp_board_sources(board)
  cache_key <- .gp_cache_key("pboard", src$board, paste(c(src$poly_tags, src$poly_queries), collapse = ""))
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get_fresh(cache_key, .GP_QUOTE_TTL_SEC)
    if (is.list(hit) && isTRUE(hit$ok)) {
      hit$quotes <- gp_filter_live_quotes(hit$quotes)
      return(hit)
    }
  }
  collected <- list()
  last_err <- NULL
  # Tag feeds first (mirror /economy, /finance/monthly, /tech, …)
  for (tag in src$poly_tags) {
    url <- sprintf(
      "https://gamma-api.polymarket.com/events?tag_slug=%s&active=true&closed=false&limit=12",
      utils::URLencode(tag, reserved = TRUE)
    )
    res <- .gp_http_get_json(url, timeout_sec = 12)
    if (!isTRUE(res$ok)) {
      last_err <- res$error %||% "tag_failed"
      next
    }
    payload <- res$data
    if (is.list(payload) && is.null(payload$events) && !is.null(payload[[1]])) {
      payload <- list(events = payload)
    }
    theme_guess <- .gp_bucket_theme_id(tag, src$theme_buckets)
    q <- gp_parse_polymarket_events(payload, theme_id = theme_guess, limit = 8L)
    if (nrow(q) > 0L) collected[[length(collected) + 1L]] <- q
    if (sum(vapply(collected, nrow, 1L)) >= as.integer(limit_quotes)) break
  }
  # Search fallback for sparse tags (e.g. indices)
  if (sum(vapply(collected, nrow, 1L)) < 6L) {
    for (qstr in src$poly_queries) {
      url <- paste0(
        "https://gamma-api.polymarket.com/public-search?q=",
        utils::URLencode(qstr, reserved = TRUE)
      )
      res <- .gp_http_get_json(url, timeout_sec = 12)
      if (!isTRUE(res$ok)) {
        last_err <- res$error %||% "search_failed"
        next
      }
      theme_guess <- .gp_bucket_theme_id(qstr, src$theme_buckets)
      parsed <- gp_parse_polymarket_events(res$data, theme_id = theme_guess, limit = 6L)
      if (nrow(parsed) > 0L) collected[[length(collected) + 1L]] <- parsed
      if (sum(vapply(collected, nrow, 1L)) >= as.integer(limit_quotes)) break
    }
  }
  if (!length(collected)) {
    return(list(ok = FALSE, quotes = .gp_empty_quote(), error = last_err %||% "empty"))
  }
  quotes <- gp_filter_live_quotes(do.call(rbind, collected))
  if (isTRUE(src$company_filter) && nrow(quotes) > 0L) {
    ok_rows <- vapply(seq_len(nrow(quotes)), function(i) {
      gp_is_tw_us_listed_title(quotes$title[[i]])
    }, logical(1))
    quotes <- quotes[ok_rows, , drop = FALSE]
  }
  quotes <- quotes[!duplicated(quotes$market_id), , drop = FALSE]
  # Re-bucket titles into board themes
  if (nrow(quotes) > 0L) {
    for (i in seq_len(nrow(quotes))) {
      quotes$theme_id[[i]] <- .gp_bucket_theme_id(quotes$title[[i]], src$theme_buckets)
    }
  }
  if (nrow(quotes) > as.integer(limit_quotes)) {
    quotes <- quotes[seq_len(as.integer(limit_quotes)), , drop = FALSE]
  }
  if (nrow(quotes) == 0L) {
    return(list(ok = FALSE, quotes = .gp_empty_quote(), error = last_err %||% "empty"))
  }
  out <- list(ok = TRUE, quotes = quotes, error = NULL)
  if (isTRUE(use_cache)) .gp_cache_set_fresh(cache_key, out, layer = "quote")
  out
}

#' Fetch one board snapshot (Economic / Finance / Company).
gp_fetch_snapshot <- function(market_mode = "US", use_cache = TRUE, limit_per_venue = 24L,
                              trigger = "periodic_visible", layer = "quote",
                              board = "economic") {
  mode <- .gp_norm_market_mode(market_mode)
  board <- gp_norm_board(board)
  trigger <- .gp_chr(trigger, "periodic_visible")
  layer <- if (.gp_chr(layer) %in% c("quote", "discovery")) .gp_chr(layer) else "quote"
  if (gp_is_hard_trigger(trigger) || identical(layer, "discovery")) {
    gp_invalidate_crawl_cache(NULL)
    layer <- "discovery"
    use_cache <- TRUE
  }
  src <- gp_board_sources(board)
  k <- gp_fetch_kalshi_board(board, limit_quotes = limit_per_venue, use_cache = use_cache)
  p <- gp_fetch_polymarket_board(board, limit_quotes = limit_per_venue, use_cache = use_cache)
  f <- gp_fetch_feex_theme(board, topic = board, use_cache = use_cache, layer = layer)
  kq <- if (is.data.frame(k$quotes)) k$quotes else .gp_empty_quote()
  pq <- if (is.data.frame(p$quotes)) p$quotes else .gp_empty_quote()
  # Bucket into theme pairs for UI
  pairs <- lapply(src$theme_buckets, function(b) {
    tid <- b$id
    k_part <- if (nrow(kq)) kq[kq$theme_id == tid, , drop = FALSE] else .gp_empty_quote()
    p_part <- if (nrow(pq)) pq[pq$theme_id == tid, , drop = FALSE] else .gp_empty_quote()
    # Orphans with empty catch-all bucket
    if (identical(tid, src$theme_buckets[[length(src$theme_buckets)]]$id)) {
      known <- vapply(src$theme_buckets, function(x) x$id, character(1))
      if (nrow(kq)) {
        extra_k <- kq[!(kq$theme_id %in% known[known != tid]), , drop = FALSE]
        if (nrow(extra_k)) k_part <- rbind(k_part, extra_k)
      }
      if (nrow(pq)) {
        extra_p <- pq[!(pq$theme_id %in% known[known != tid]), , drop = FALSE]
        if (nrow(extra_p)) p_part <- rbind(p_part, extra_p)
      }
    }
    if (nrow(k_part) > 6L) k_part <- k_part[seq_len(6L), , drop = FALSE]
    if (nrow(p_part) > 6L) p_part <- p_part[seq_len(6L), , drop = FALSE]
    list(
      theme_id = tid,
      label_key = b$label_key,
      breadth_tier = NA_integer_,
      kalshi = k_part,
      polymarket = p_part,
      feex = .gp_empty_quote(),
      feex_status = f$status %||% "defunct",
      agreement = gp_crowd_agreement(k_part$yes_prob, p_part$yes_prob),
      series = .gp_empty_series()
    )
  })
  # Drop empty theme cards except when entire board empty (keep one honest empty)
  nonempty <- vapply(pairs, function(pr) {
    nrow(pr$kalshi) + nrow(pr$polymarket) > 0L
  }, logical(1))
  if (any(nonempty)) pairs <- pairs[nonempty]
  # Primary board view: Yes-prob line charts from venue history (honest empty if none).
  pairs <- lapply(pairs, function(pr) {
    gp_attach_theme_series(pr, use_cache = use_cache, per_venue = .GP_HISTORY_PER_VENUE)
  })
  list(
    fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
    market_mode = mode,
    board = board,
    trigger = trigger,
    layer = layer,
    policy = gp_crawl_policy(),
    pairs = pairs,
    venue_errors = list(
      kalshi = if (isTRUE(k$ok)) NULL else k$error,
      polymarket = if (isTRUE(p$ok)) NULL else p$error,
      feex = "feex_no_public_feed"
    )
  )
}

global_prediction_ui <- function(id, locale = "en") {
  ns <- shiny::NS(id)
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    "en"
  }
  colors <- gp_brand_colors()
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(sprintf("
      .ynow-gp { margin-top: 4px; }
      .ynow-gp-lead { font-size: 13px; line-height: 1.5; color: #3f3a33; margin: 0 0 10px; }
      .ynow-gp-context { font-size: 12px; line-height: 1.45; color: #5c5346; margin: 0 0 12px;
        padding: 8px 10px; background: #f4f0e6; border: 1px solid #e4dccb; border-radius: 6px; }
      .ynow-gp-caution { font-size: 12px; line-height: 1.45; color: #5c5346; margin: 0 0 12px; }
      .ynow-gp-participation-inline { font-size: 11px; line-height: 1.4; color: #7a7368;
        margin: 6px 0 0; width: 100%%; flex-basis: 100%%; }
      .ynow-gp-meta { font-size: 12px; color: #5c5346; margin: 0 0 14px; display: flex;
        flex-wrap: wrap; gap: 8px 16px; align-items: center; }
      .ynow-gp-boards { display: flex; flex-wrap: wrap; gap: 8px; margin: 0 0 14px; }
      .ynow-gp-board-btn { border: 1px solid #cfc6b4; background: #fff; color: #3f3a33;
        border-radius: 6px; padding: 6px 12px; font-size: 13px; font-weight: 600; }
      .ynow-gp-board-btn.active { background: #1c1915; color: #f7f3ea; border-color: #1c1915; }
      .ynow-gp-swatch { display: inline-flex; align-items: center; gap: 6px; }
      .ynow-gp-dot { width: 10px; height: 10px; border-radius: 2px; display: inline-block; }
      .ynow-gp-theme { margin: 0 0 16px; padding: 12px; background: #fff;
        border: 1px solid #e4dccb; border-radius: 8px; }
      .ynow-gp-theme-head { display: flex; flex-wrap: wrap; gap: 8px 14px;
        align-items: baseline; justify-content: space-between; margin-bottom: 10px; }
      .ynow-gp-theme-title { margin: 0; font-size: 15px; color: #1c1915; }
      .ynow-gp-agree { display: flex; flex-wrap: wrap; gap: 6px 10px; align-items: center;
        font-size: 12px; color: #5c5346; }
      .ynow-gp-agree-badge { font-weight: 700; padding: 2px 8px; border-radius: 4px;
        background: #efe9dc; color: #3f3a33; }
      .ynow-gp-agree-agree { background: rgba(0, 214, 108, 0.18); }
      .ynow-gp-agree-diverge { background: rgba(196, 86, 52, 0.16); }
      .ynow-gp-agree-mixed { background: rgba(193, 122, 46, 0.16); }
      .ynow-gp-agree-meta { color: #7a7368; }
      .ynow-gp-cols { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
      .ynow-gp-col { border-radius: 8px; padding: 8px 10px; }
      .ynow-gp-col-kalshi { background: rgba(0, 214, 108, 0.12); }
      .ynow-gp-col-polymarket { background: rgba(46, 92, 255, 0.10); }
      @media (max-width: 767px) { .ynow-gp-cols { grid-template-columns: 1fr; } }
      .ynow-gp-venue-label { font-size: 13px; font-weight: 700; letter-spacing: 0.02em;
        border-bottom: 2px solid; padding-bottom: 4px; margin-bottom: 8px; }
      .ynow-gp-chart { margin: 0 0 10px; }
      .ynow-gp-charts { display: flex; flex-direction: column; gap: 14px; }
      .ynow-gp-market-chart { margin: 0; padding: 12px 12px 8px; background: #fff;
        border: 1px solid #e5e7eb; border-radius: 8px; }
      .ynow-gp-chart-widget { width: 100%%; }
      .ynow-gp-chart-head { display: flex; flex-wrap: wrap; align-items: baseline;
        justify-content: space-between; gap: 6px 12px; margin: 0 0 6px; }
      .ynow-gp-chart-head-main { display: flex; flex-wrap: wrap; align-items: center;
        gap: 8px; min-width: 0; }
      .ynow-gp-chart-dot { width: 8px; height: 8px; border-radius: 999px; flex: 0 0 auto; }
      .ynow-gp-chart-title { font-size: 13px; font-weight: 600; color: #111827;
        line-height: 1.35; }
      .ynow-gp-chart-chance { font-size: 22px; font-weight: 700; font-variant-numeric: tabular-nums;
        letter-spacing: -0.02em; }
      .ynow-gp-chart-head-meta { display: flex; gap: 10px; align-items: center;
        font-size: 11px; color: #9ca3af; text-transform: uppercase; letter-spacing: 0.04em; }
      .ynow-gp-chart-venue-mark { font-weight: 600; }
      .ynow-gp-chart-empty { padding: 18px 8px; text-align: center; }
      .ynow-gp-last-print-label { font-size: 11px; color: #7a7368; margin: 0 0 6px; }
      .ynow-gp-quote { border-left: 3px solid; padding: 6px 0 8px 10px; margin-bottom: 8px; }
      .ynow-gp-quote-title { font-size: 13px; line-height: 1.35; color: #1c1915; }
      .ynow-gp-quote-title a { color: inherit; text-decoration: underline; text-underline-offset: 2px; }
      .ynow-gp-quote-prob { font-size: 18px; font-weight: 700; margin: 4px 0; font-variant-numeric: tabular-nums; }
      .ynow-gp-empty { font-size: 12px; color: #7a7368; margin: 0; }
      .ynow-gp-dot-kalshi { background: %s; }
      .ynow-gp-dot-poly { background: %s; }
      .ynow-gp-dot-feex { background: %s; }
    ", colors$kalshi, colors$polymarket, colors$feex))),
    htmltools::tags$div(
      class = "ynow-gp",
      htmltools::tags$p(id = "ynow_gp_lead", class = "ynow-gp-lead", .gp_label("gp_lead", loc)),
      htmltools::tags$p(id = "ynow_gp_context", class = "ynow-gp-context", .gp_label("gp_context", loc)),
      htmltools::tags$p(id = "ynow_gp_caution", class = "ynow-gp-caution", .gp_label("gp_caution", loc)),
      htmltools::tags$p(
        id = "ynow_gp_participation",
        class = "ynow-gp-caution",
        .gp_label("gp_participation_note", loc)
      ),
      htmltools::tags$div(
        class = "ynow-gp-boards",
        shiny::actionButton(ns("board_economic"), label = .gp_label("gp_board_economic", loc),
                            class = "ynow-gp-board-btn active"),
        shiny::actionButton(ns("board_finance"), label = .gp_label("gp_board_finance", loc),
                            class = "ynow-gp-board-btn"),
        shiny::actionButton(ns("board_company"), label = .gp_label("gp_board_company", loc),
                            class = "ynow-gp-board-btn")
      ),
      htmltools::tags$div(
        class = "ynow-gp-meta",
        htmltools::tags$span(
          class = "ynow-gp-swatch",
          htmltools::tags$span(class = "ynow-gp-dot ynow-gp-dot-kalshi"), "Kalshi"
        ),
        htmltools::tags$span(
          class = "ynow-gp-swatch",
          htmltools::tags$span(class = "ynow-gp-dot ynow-gp-dot-poly"), "Polymarket"
        ),
        shiny::uiOutput(ns("board_badge")),
        shiny::actionButton(
          ns("refresh"),
          label = .gp_label("gp_refresh", loc),
          class = "btn-sm btn-default",
          style = "margin-left:auto;"
        ),
        shiny::uiOutput(ns("updated"))
      ),
      shiny::uiOutput(ns("body")),
      htmltools::tags$script(htmltools::HTML(sprintf("
        (function () {
          var inputId = '%s';
          var ping = function () {
            if (document.hidden) return;
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue(inputId, Date.now(), {priority: 'event'});
            }
          };
          document.addEventListener('visibilitychange', ping);
          window.addEventListener('focus', ping);
        })();
      ", ns("page_focus"))))
    )
  )
}

global_prediction_server <- function(id,
                                     ui_locale_rv = reactive("en"),
                                     market_mode_rv = reactive("US"),
                                     tab_active_rv = reactive(FALSE),
                                     subtab_active_rv = reactive(TRUE),
                                     board_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    snap_rv <- shiny::reactiveVal(NULL)
    busy_rv <- shiny::reactiveVal(FALSE)
    gen_rv <- shiny::reactiveVal(0L)
    last_fetch_at <- shiny::reactiveVal(as.POSIXct(NA, tz = "UTC"))
    saw_active_rv <- shiny::reactiveVal(FALSE)
    board_state <- shiny::reactiveVal("economic")

    loc <- shiny::reactive({
      if (is.reactive(ui_locale_rv)) ui_locale_rv() else "en"
    })
    mode <- shiny::reactive({
      if (is.reactive(market_mode_rv)) .gp_norm_market_mode(market_mode_rv()) else "US"
    })
    panel_active <- shiny::reactive({
      isTRUE(tab_active_rv()) && isTRUE(subtab_active_rv())
    })

    set_board <- function(b) {
      b <- gp_norm_board(b)
      board_state(b)
      invisible(b)
    }

    
    shiny::observe({
      b <- board_state()
      if (requireNamespace("shinyjs", quietly = TRUE)) {
        shinyjs::runjs(sprintf(
          "(function(){var map={economic:'%s',finance:'%s',company:'%s'};var want=map[%s];document.querySelectorAll('.ynow-gp-board-btn').forEach(function(el){el.classList.toggle('active', el.id===want);});})();",
          session$ns("board_economic"),
          session$ns("board_finance"),
          session$ns("board_company"),
          jsonlite::toJSON(b, auto_unbox = TRUE)
        ))
      }
    })

shiny::observeEvent(input$board_economic, {
      set_board("economic")
      do_fetch("manual_refresh", force = TRUE)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$board_finance, {
      set_board("finance")
      do_fetch("manual_refresh", force = TRUE)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$board_company, {
      set_board("company")
      do_fetch("manual_refresh", force = TRUE)
    }, ignoreInit = TRUE)

    do_fetch <- function(trigger = "periodic_visible", force = FALSE) {
      trigger <- .gp_chr(trigger, "periodic_visible")
      hard <- isTRUE(force) || gp_is_hard_trigger(trigger)
      gated <- trigger %in% c("periodic_visible", "tab_focus")
      if (gated) {
        prev <- shiny::isolate(last_fetch_at())
        if (!is.na(prev)) {
          gap <- as.numeric(difftime(Sys.time(), prev, units = "secs"))
          if (is.finite(gap) && gap < as.numeric(.GP_MIN_FETCH_GAP_SEC)) {
            return(invisible(NULL))
          }
        }
      }
      g <- shiny::isolate(as.integer(gen_rv()) %||% 0L) + 1L
      gen_rv(g)
      busy_rv(TRUE)
      mm <- shiny::isolate(mode())
      bb <- shiny::isolate(board_state())
      layer <- if (hard) "discovery" else "quote"
      snap <- tryCatch(
        gp_fetch_snapshot(
          market_mode = mm,
          use_cache = TRUE,
          limit_per_venue = 24L,
          trigger = trigger,
          layer = layer,
          board = bb
        ),
        error = function(e) {
          list(
            fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
            market_mode = mm,
            board = bb,
            trigger = trigger,
            layer = layer,
            pairs = list(),
            venue_errors = list(
              kalshi = conditionMessage(e),
              polymarket = conditionMessage(e),
              feex = "feex_no_public_feed"
            )
          )
        }
      )
      if (identical(shiny::isolate(as.integer(gen_rv())), g)) {
        snap_rv(snap)
        busy_rv(FALSE)
        last_fetch_at(Sys.time())
      }
      invisible(snap)
    }

    shiny::observeEvent(
      list(panel_active(), board_state()),
      {
        if (!isTRUE(panel_active())) {
          saw_active_rv(FALSE)
          return()
        }
        first <- !isTRUE(shiny::isolate(saw_active_rv()))
        saw_active_rv(TRUE)
        do_fetch(if (first) "tab_open" else "manual_refresh", force = TRUE)
      },
      ignoreInit = FALSE
    )

    shiny::observeEvent(input$page_focus, {
      if (!isTRUE(panel_active())) return()
      do_fetch("tab_focus", force = TRUE)
    }, ignoreInit = TRUE)

    shiny::observe({
      shiny::req(isTRUE(panel_active()))
      shiny::invalidateLater(as.integer(.GP_PERIODIC_RESCAN_SEC) * 1000L, session)
      prev <- shiny::isolate(last_fetch_at())
      if (is.na(prev)) return()
      do_fetch("periodic_visible", force = FALSE)
    })

    output$board_badge <- shiny::renderUI({
      b <- board_state()
      key <- paste0("gp_board_", b)
      htmltools::tags$span(
        id = "ynow_gp_board_badge",
        class = "ynow-gp-swatch",
        .gp_label(key, loc())
      )
    })

    shiny::observeEvent(input$refresh, {
      do_fetch("manual_refresh", force = TRUE)
    }, ignoreInit = TRUE)

    shiny::observe({
      shiny::req(is.reactive(ui_locale_rv))
      loc()
      shiny::updateActionButton(session, "refresh", label = .gp_label("gp_refresh", loc()))
      shiny::updateActionButton(session, "board_economic", label = .gp_label("gp_board_economic", loc()))
      shiny::updateActionButton(session, "board_finance", label = .gp_label("gp_board_finance", loc()))
      shiny::updateActionButton(session, "board_company", label = .gp_label("gp_board_company", loc()))
    })

    output$updated <- shiny::renderUI({
      snap <- snap_rv()
      locale <- loc()
      if (isTRUE(busy_rv())) {
        return(htmltools::tags$span(id = "ynow_gp_updated", .gp_label("gp_loading", locale)))
      }
      stamp <- if (is.list(snap)) .gp_chr(snap$fetched_at) else ""
      htmltools::tags$span(
        id = "ynow_gp_updated",
        if (nzchar(stamp)) {
          sub("{stamp}", stamp, .gp_label("gp_updated", locale), fixed = TRUE)
        } else {
          ""
        }
      )
    })

    output$body <- shiny::renderUI({
      snap <- snap_rv()
      locale <- loc()
      if (isTRUE(busy_rv()) && is.null(snap)) {
        return(htmltools::tags$p(class = "ynow-gp-empty", .gp_label("gp_loading", locale)))
      }
      if (!is.list(snap) || length(snap$pairs) == 0L) {
        return(htmltools::tags$p(class = "ynow-gp-empty", .gp_label("gp_error_venue", locale)))
      }
      errs <- snap$venue_errors %||% list()
      venue_down <- !is.null(errs$kalshi) || !is.null(errs$polymarket)
      htmltools::tagList(
        if (isTRUE(venue_down)) {
          htmltools::tags$p(class = "ynow-gp-caution", .gp_label("gp_error_venue", locale))
        },
        htmltools::tags$p(class = "ynow-gp-caution", .gp_label("gp_attribution", locale)),
        lapply(snap$pairs, function(pair) .gp_theme_row_ui(pair, locale))
      )
    })
  })
}
