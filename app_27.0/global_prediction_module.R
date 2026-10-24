# ==========================================
# global_prediction_module.R — Global Prediction under ATX
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

gp_brand_colors <- function() {
  list(
    kalshi = .GP_KALSHI_COLOR,
    polymarket = .GP_POLYMARKET_COLOR,
    feex = .GP_FEEX_COLOR
  )
}

# breadth_tier: 1 macro → 2 market/index → 3 sector/theme → 4 single-stock.
# Rows are stored already sorted; gp_theme_registry() re-orders defensively.
.gp_sort_theme_registry <- function(reg) {
  if (!is.data.frame(reg) || nrow(reg) == 0L) return(reg)
  if (!("breadth_tier" %in% names(reg))) {
    reg$breadth_tier <- 1L
  }
  if (!("breadth_ord" %in% names(reg))) {
    reg$breadth_ord <- seq_len(nrow(reg))
  }
  reg[order(as.integer(reg$breadth_tier), as.integer(reg$breadth_ord), as.character(reg$theme_id)), , drop = FALSE]
}

#' Theme registry by App equity mode (US vs TW). Overlap themes (Fed/CPI) are intentional.
#' Order is always 總體 → 市場／指數 → 題材 → 個股 (never lead with single-stock).
gp_theme_registry <- function(market_mode = "US") {
  mode <- .gp_norm_market_mode(market_mode)
  if (identical(mode, "TW")) {
    return(.gp_sort_theme_registry(data.frame(
      theme_id = c(
        "fed_rates",
        "inflation_cpi",
        "tw_macro",
        "tw_equity",
        "tw_housing"
      ),
      label_key = c(
        "gp_theme_fed_rates",
        "gp_theme_inflation_cpi",
        "gp_theme_tw_macro",
        "gp_theme_tw_equity",
        "gp_theme_tw_housing"
      ),
      breadth_tier = c(1L, 1L, 1L, 2L, 3L),
      breadth_ord = c(1L, 2L, 3L, 1L, 1L),
      kalshi_series = I(list(
        c("KXRATECUT", "KXFEDDECISION"),
        c("KXCPI", "KXCPICOREYOY"),
        character(0),
        character(0),
        character(0)
      )),
      polymarket_queries = I(list(
        "Fed rate",
        "CPI inflation",
        c("Taiwan GDP", "Taiwan economy", "Taiwan election"),
        c("Taiwan stock", "TAIEX", "Taiwan equity"),
        c("Taiwan housing", "Taipei housing", "Taiwan property")
      )),
      keywords = I(list(
        c("fed", "fomc", "rate cut", "federal reserve"),
        c("cpi", "inflation"),
        c("taiwan", "gdp", "election"),
        c("taiwan", "taiex", "twse"),
        c("taiwan", "housing", "property", "real estate")
      )),
      feex_topics = I(list(
        "fed_rates",
        "inflation",
        "tw_macro",
        "tw_equity",
        "tw_housing"
      )),
      stringsAsFactors = FALSE
    )))
  }
  .gp_sort_theme_registry(data.frame(
    theme_id = c(
      "fed_rates",
      "inflation_cpi",
      "recession",
      "treasury_yields",
      "equities_index"
    ),
    label_key = c(
      "gp_theme_fed_rates",
      "gp_theme_inflation_cpi",
      "gp_theme_recession",
      "gp_theme_treasury_yields",
      "gp_theme_equities_index"
    ),
    breadth_tier = c(1L, 1L, 1L, 2L, 2L),
    breadth_ord = c(1L, 2L, 3L, 1L, 2L),
    kalshi_series = I(list(
      c("KXRATECUT", "KXFEDDECISION", "KXRATECUTCOUNT", "KXDOTPLOT"),
      c("KXCPI", "KXCPICOREYOY", "KXLCPIMAXYOY"),
      c("KXNBERRECESSQ", "KXU3MAX"),
      c("KX10YR550PCT", "TNOTEW"),
      c("KXNASDAQ100M", "KXNASDAQ100MD")
    )),
    polymarket_queries = I(list(
      c("Fed rate cuts 2026", "Federal Reserve rate", "FOMC"),
      c("US CPI", "core CPI inflation"),
      c("US recession", "recession 2026"),
      c("Treasury yield", "10-year Treasury"),
      c("S&P 500", "Nasdaq-100")
    )),
    keywords = I(list(
      c("fed", "fomc", "rate cut", "rate hike", "federal reserve"),
      c("cpi", "inflation", "consumer price"),
      c("recession", "nber"),
      c("treasury", "yield", "t-note", "tnote"),
      c("nasdaq", "s&p", "spx")
    )),
    feex_topics = I(list(
      "fed_rates",
      "inflation",
      "recession",
      "treasury",
      "equities"
    )),
    stringsAsFactors = FALSE
  ))
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
gp_fetch_feex_theme <- function(theme_id, topic = NULL, use_cache = TRUE) {
  theme_id <- .gp_chr(theme_id)
  cache_key <- paste0("gp:feex:", theme_id)
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get(cache_key)
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
  if (isTRUE(use_cache)) .gp_cache_set(cache_key, out)
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

.gp_cache_get <- function(key) {
  if (!exists(".ynow_fast_cache", inherits = TRUE)) return(NULL)
  tryCatch(.ynow_fast_cache$get(key), error = function(e) NULL)
}

.gp_cache_set <- function(key, value) {
  if (!exists(".ynow_fast_cache", inherits = TRUE)) return(invisible(FALSE))
  tryCatch({
    .ynow_fast_cache$set(key, value)
    TRUE
  }, error = function(e) FALSE)
}

.gp_empty_quote <- function() {
  data.frame(
    venue = character(0),
    theme_id = character(0),
    market_id = character(0),
    title = character(0),
    yes_prob = numeric(0),
    updated_at = character(0),
    url = character(0),
    raw_status = character(0),
    stringsAsFactors = FALSE
  )
}

.gp_quote_row <- function(venue, theme_id, market_id, title, yes_prob,
                          updated_at = NA_character_, url = NA_character_,
                          raw_status = NA_character_) {
  data.frame(
    venue = .gp_chr(venue),
    theme_id = .gp_chr(theme_id),
    market_id = .gp_chr(market_id),
    title = .gp_chr(title),
    yes_prob = .gp_num01(yes_prob),
    updated_at = .gp_chr(updated_at, NA_character_),
    url = .gp_chr(url, NA_character_),
    raw_status = .gp_chr(raw_status, NA_character_),
    stringsAsFactors = FALSE
  )
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
gp_parse_kalshi_markets <- function(payload, theme_id, limit = 3L) {
  theme_id <- .gp_chr(theme_id)
  payload <- .gp_as_payload(payload)
  if (is.null(payload)) return(.gp_empty_quote())
  markets <- payload
  if (is.list(payload) && !is.null(payload$markets)) markets <- payload$markets
  if (!is.list(markets) || length(markets) == 0L) return(.gp_empty_quote())
  drop_status <- c("finalized", "closed", "settled", "determined", "inactive", "expired")
  rows <- list()
  n <- 0L
  for (m in markets) {
    if (!is.list(m)) next
    status <- tolower(.gp_chr(m$status))
    if (status %in% drop_status) next
    ticker <- .gp_chr(m$ticker %||% m$market_ticker)
    if (!nzchar(ticker)) next
    title <- .gp_chr(m$title %||% m$yes_sub_title %||% m$subtitle %||% ticker)
    yes <- .gp_kalshi_yes(m)
    if (!is.finite(yes)) next
    n <- n + 1L
    rows[[n]] <- .gp_quote_row(
      venue = "kalshi",
      theme_id = theme_id,
      market_id = ticker,
      title = title,
      yes_prob = yes,
      updated_at = .gp_chr(m$close_time %||% m$expected_expiration_time),
      url = paste0("https://kalshi.com/markets/", ticker),
      raw_status = .gp_chr(m$status, "open")
    )
    if (n >= as.integer(limit)) break
  }
  if (n == 0L) return(.gp_empty_quote())
  do.call(rbind, rows)
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
.gp_poly_yes_prob <- function(market) {
  if (!is.list(market)) return(NA_real_)
  op <- .gp_poly_parse_json_vec(market$outcomePrices)
  outcomes <- .gp_poly_parse_json_vec(market$outcomes)
  if (is.null(op) || is.null(outcomes)) return(NA_real_)
  labs <- tolower(trimws(as.character(unlist(outcomes, use.names = FALSE))))
  prices <- unlist(op, use.names = FALSE)
  if (length(labs) != length(prices)) return(NA_real_)
  yes_i <- which(labs %in% c("yes", "y"))
  if (length(yes_i) != 1L) return(NA_real_)
  .gp_num01(prices[[yes_i[[1]]]])
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
      n <- n + 1L
      rows[[n]] <- .gp_quote_row(
        venue = "polymarket",
        theme_id = theme_id,
        market_id = mid,
        title = title,
        yes_prob = yes,
        updated_at = .gp_chr(m$updatedAt %||% ev$endDate %||% ev$creationDate),
        url = url,
        raw_status = if (isTRUE(ev$closed) || isTRUE(m$closed)) "closed" else "open"
      )
      if (n >= as.integer(limit)) break
    }
    if (n >= as.integer(limit)) break
  }
  if (n == 0L) return(.gp_empty_quote())
  do.call(rbind, rows)
}

gp_fetch_kalshi_theme <- function(theme_id, series_tickers, limit = 3L, use_cache = TRUE) {
  theme_id <- .gp_chr(theme_id)
  series_tickers <- unique(as.character(unlist(series_tickers, use.names = FALSE)))
  series_tickers <- series_tickers[nzchar(series_tickers)]
  if (length(series_tickers) == 0L) {
    return(list(ok = FALSE, quotes = .gp_empty_quote(), error = "no_series"))
  }
  cache_key <- paste0("gp:kalshi:", theme_id, ":", paste(series_tickers, collapse = ","))
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get(cache_key)
    if (is.list(hit) && isTRUE(hit$ok)) return(hit)
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
  quotes <- do.call(rbind, collected)
  quotes <- quotes[!duplicated(quotes$market_id), , drop = FALSE]
  if (nrow(quotes) > as.integer(limit)) {
    quotes <- quotes[seq_len(as.integer(limit)), , drop = FALSE]
  }
  out <- list(ok = TRUE, quotes = quotes, error = NULL)
  if (isTRUE(use_cache)) .gp_cache_set(cache_key, out)
  out
}

gp_fetch_polymarket_theme <- function(theme_id, queries, limit = 3L, use_cache = TRUE, keywords = NULL) {
  theme_id <- .gp_chr(theme_id)
  queries <- unique(as.character(unlist(queries, use.names = FALSE)))
  queries <- queries[nzchar(queries)]
  if (length(queries) == 0L) {
    return(list(ok = FALSE, quotes = .gp_empty_quote(), error = "no_query"))
  }
  cache_key <- paste0("gp:poly:", theme_id, ":", paste(queries, collapse = "|"))
  if (isTRUE(use_cache)) {
    hit <- .gp_cache_get(cache_key)
    if (is.list(hit) && isTRUE(hit$ok)) return(hit)
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
  quotes <- do.call(rbind, collected)
  quotes <- quotes[!duplicated(quotes$market_id), , drop = FALSE]
  if (nrow(quotes) > as.integer(limit)) {
    quotes <- quotes[seq_len(as.integer(limit)), , drop = FALSE]
  }
  out <- list(ok = TRUE, quotes = quotes, error = NULL)
  if (isTRUE(use_cache)) .gp_cache_set(cache_key, out)
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
gp_fetch_snapshot <- function(market_mode = "US", use_cache = TRUE, limit_per_venue = 3L) {
  mode <- .gp_norm_market_mode(market_mode)
  reg <- gp_theme_registry(mode)
  pairs <- vector("list", nrow(reg))
  k_ok <- FALSE
  p_ok <- FALSE
  k_err <- NULL
  p_err <- NULL
  feex_err <- "feex_no_public_feed"
  for (i in seq_len(nrow(reg))) {
    tid <- reg$theme_id[[i]]
    series <- reg$kalshi_series[[i]]
    if (is.null(series)) series <- character(0)
    k <- if (length(unlist(series)) == 0L) {
      list(ok = FALSE, quotes = .gp_empty_quote(), error = "empty")
    } else {
      gp_fetch_kalshi_theme(
        tid,
        series,
        limit = limit_per_venue,
        use_cache = use_cache
      )
    }
    kw <- if ("keywords" %in% names(reg)) reg$keywords[[i]] else NULL
    p <- gp_fetch_polymarket_theme(
      tid,
      reg$polymarket_queries[[i]],
      limit = limit_per_venue,
      use_cache = use_cache,
      keywords = kw
    )
    feex_topic <- if ("feex_topics" %in% names(reg)) reg$feex_topics[[i]] else tid
    f <- gp_fetch_feex_theme(tid, topic = feex_topic, use_cache = use_cache)
    if (isTRUE(k$ok)) k_ok <- TRUE
    else if (!identical(k$error, "empty") && is.null(k_err)) k_err <- k$error
    if (isTRUE(p$ok)) p_ok <- TRUE
    else if (!identical(p$error, "empty") && is.null(p_err)) p_err <- p$error
    kq <- if (is.data.frame(k$quotes)) k$quotes else .gp_empty_quote()
    pq <- if (is.data.frame(p$quotes)) p$quotes else .gp_empty_quote()
    fq <- if (is.data.frame(f$quotes)) f$quotes else .gp_empty_quote()
    agree <- gp_crowd_agreement(kq$yes_prob, pq$yes_prob)
    pairs[[i]] <- list(
      theme_id = tid,
      label_key = reg$label_key[[i]],
      kalshi = kq,
      polymarket = pq,
      feex = fq,
      feex_status = f$status %||% f$error %||% "empty",
      agreement = agree
    )
  }
  list(
    fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
    market_mode = mode,
    pairs = pairs,
    venue_errors = list(
      kalshi = if (k_ok) NULL else k_err,
      polymarket = if (p_ok) NULL else p_err,
      feex = feex_err
    )
  )
}

.gp_fmt_pct <- function(p) {
  p <- .gp_num01(p)
  if (!is.finite(p)) return("—")
  sprintf("%.0f%%", round(100 * p))
}

.gp_quote_cards <- function(quotes, venue_color, locale = "en") {
  if (!is.data.frame(quotes) || nrow(quotes) == 0L) {
    return(htmltools::tags$p(
      class = "ynow-gp-empty",
      .gp_label("gp_empty_venue", locale)
    ))
  }
  cards <- lapply(seq_len(nrow(quotes)), function(i) {
    row <- quotes[i, , drop = FALSE]
    bar_w <- if (is.finite(row$yes_prob[[1]])) {
      sprintf("%.1f%%", 100 * row$yes_prob[[1]])
    } else {
      "0%"
    }
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
      ),
      htmltools::tags$div(
        class = "ynow-gp-bar-track",
        htmltools::tags$div(
          class = "ynow-gp-bar-fill",
          style = sprintf("width:%s;background:%s;", bar_w, venue_color)
        )
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
      htmltools::tags$p(
        class = "ynow-gp-participation-inline",
        .gp_label("gp_participation_note", locale)
      )
    ),
    htmltools::tags$div(
      class = "ynow-gp-cols ynow-gp-cols-3",
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
        margin: 6px 0 0; width: 100%; flex-basis: 100%; }
      .ynow-gp-meta { font-size: 12px; color: #5c5346; margin: 0 0 14px; display: flex;
        flex-wrap: wrap; gap: 8px 16px; align-items: center; }
      .ynow-gp-swatch { display: inline-flex; align-items: center; gap: 6px; }
      .ynow-gp-dot { width: 10px; height: 10px; border-radius: 2px; display: inline-block; }
      .ynow-gp-theme { margin: 0 0 16px; padding: 12px; background: #fff;
        border: 1px solid #e4dccb; border-radius: 8px; }
      .ynow-gp-theme-head { display: flex; flex-wrap: wrap; gap: 8px 14px;
        align-items: baseline; justify-content: space-between; margin-bottom: 10px; }
      .ynow-gp-theme-title { margin: 0; font-size: 15px; color: #1c1915; }
      .ynow-gp-crowd-badge { font-size: 12px; display: flex; flex-wrap: wrap; gap: 6px 12px;
        align-items: center; }
      .ynow-gp-crowd-agree { color: #1e7a46; font-weight: 600; }
      .ynow-gp-crowd-diverge { color: #a33b2b; font-weight: 600; }
      .ynow-gp-crowd-mixed { color: #6b5b3e; font-weight: 600; }
      .ynow-gp-crowd-insufficient { color: #7a7368; }
      .ynow-gp-crowd-meta { color: #5c5346; font-weight: 400; }
      .ynow-gp-cols { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
      .ynow-gp-cols-3 { grid-template-columns: 1fr 1fr 1fr; }
      .ynow-gp-col { border-radius: 8px; padding: 8px 10px; }
      .ynow-gp-col-kalshi { background: rgba(0, 214, 108, 0.12); }
      .ynow-gp-col-polymarket { background: rgba(46, 92, 255, 0.10); }
      .ynow-gp-col-feex { background: rgba(193, 122, 46, 0.12); }
      @media (max-width: 991px) {
        .ynow-gp-cols-3 { grid-template-columns: 1fr; }
      }
      @media (max-width: 767px) {
        .ynow-gp-cols { grid-template-columns: 1fr; }
      }
      .ynow-gp-venue-label { font-size: 13px; font-weight: 700; letter-spacing: 0.02em;
        border-bottom: 2px solid; padding-bottom: 4px; margin-bottom: 8px; }
      .ynow-gp-quote { border-left: 3px solid; padding: 6px 0 8px 10px; margin-bottom: 8px; }
      .ynow-gp-quote-title { font-size: 13px; line-height: 1.35; color: #1c1915; }
      .ynow-gp-quote-title a { color: inherit; text-decoration: underline; text-underline-offset: 2px; }
      .ynow-gp-quote-prob { font-size: 20px; font-weight: 700; margin: 4px 0; font-variant-numeric: tabular-nums; }
      .ynow-gp-bar-track { height: 6px; background: #efe9dc; border-radius: 999px; overflow: hidden; }
      .ynow-gp-bar-fill { height: 100%; border-radius: 999px; }
      .ynow-gp-empty { font-size: 12px; color: #7a7368; margin: 0; }
      .ynow-gp-dot-kalshi { background: %s; }
      .ynow-gp-dot-poly { background: %s; }
      .ynow-gp-dot-feex { background: %s; }
    ", colors$kalshi, colors$polymarket, colors$feex))),
    htmltools::tags$div(
      class = "ynow-gp",
      htmltools::tags$p(
        id = "ynow_gp_lead",
        class = "ynow-gp-lead",
        .gp_label("gp_lead", loc)
      ),
      htmltools::tags$p(
        id = "ynow_gp_context",
        class = "ynow-gp-context",
        .gp_label("gp_context", loc)
      ),
      htmltools::tags$p(
        id = "ynow_gp_caution",
        class = "ynow-gp-caution",
        .gp_label("gp_caution", loc)
      ),
      htmltools::tags$p(
        id = "ynow_gp_participation",
        class = "ynow-gp-caution",
        .gp_label("gp_participation_note", loc)
      ),
      htmltools::tags$div(
        class = "ynow-gp-meta",
        htmltools::tags$span(
          class = "ynow-gp-swatch",
          htmltools::tags$span(class = "ynow-gp-dot ynow-gp-dot-kalshi"),
          "Kalshi"
        ),
        htmltools::tags$span(
          class = "ynow-gp-swatch",
          htmltools::tags$span(class = "ynow-gp-dot ynow-gp-dot-poly"),
          "Polymarket"
        ),
        shiny::uiOutput(ns("mode_badge")),
        shiny::actionButton(
          ns("refresh"),
          label = .gp_label("gp_refresh", loc),
          class = "btn-sm btn-default",
          style = "margin-left:auto;"
        ),
        shiny::uiOutput(ns("updated"))
      ),
      shiny::uiOutput(ns("body"))
    )
  )
}

global_prediction_server <- function(id,
                                     ui_locale_rv = reactive("en"),
                                     market_mode_rv = reactive("US"),
                                     tab_active_rv = reactive(FALSE),
                                     subtab_active_rv = reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    snap_rv <- shiny::reactiveVal(NULL)
    busy_rv <- shiny::reactiveVal(FALSE)
    gen_rv <- shiny::reactiveVal(0L)

    loc <- shiny::reactive({
      if (is.reactive(ui_locale_rv)) ui_locale_rv() else "en"
    })
    mode <- shiny::reactive({
      if (is.reactive(market_mode_rv)) .gp_norm_market_mode(market_mode_rv()) else "US"
    })

    do_fetch <- function() {
      g <- shiny::isolate(as.integer(gen_rv()) %||% 0L) + 1L
      gen_rv(g)
      busy_rv(TRUE)
      mm <- shiny::isolate(mode())
      snap <- tryCatch(
        gp_fetch_snapshot(market_mode = mm, use_cache = TRUE, limit_per_venue = 3L),
        error = function(e) {
          list(
            fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
            market_mode = mm,
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
      }
      invisible(snap)
    }

    shiny::observeEvent(
      list(tab_active_rv(), subtab_active_rv(), mode()),
      {
        if (!isTRUE(tab_active_rv()) || !isTRUE(subtab_active_rv())) return()
        do_fetch()
      },
      ignoreInit = FALSE
    )

    output$mode_badge <- shiny::renderUI({
      key <- if (identical(mode(), "TW")) "gp_mode_tw" else "gp_mode_us"
      htmltools::tags$span(
        id = "ynow_gp_mode_badge",
        class = "ynow-gp-swatch",
        .gp_label(key, loc())
      )
    })

    shiny::observeEvent(input$refresh, {
      do_fetch()
    }, ignoreInit = TRUE)

    shiny::observe({
      shiny::req(is.reactive(ui_locale_rv))
      loc()
      shiny::updateActionButton(session, "refresh", label = .gp_label("gp_refresh", loc()))
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
        htmltools::tags$p(
          class = "ynow-gp-caution",
          .gp_label("gp_attribution", locale)
        ),
        lapply(snap$pairs, function(pair) .gp_theme_row_ui(pair, locale))
      )
    })
  })
}
