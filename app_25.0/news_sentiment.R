# Headline news sentiment for the live ticker.
# Score is a finance-headline lexicon in [-1, 1] over every Yahoo headline
# published in the last 24 hours. The bundle is cached one day.
# It is not FinBERT. Historical backtests must not reuse today's score on past dates.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

.ynow_news_neg <- c(
  "fraud", "investigation", "lawsuit", "sec probe", "sec investigation",
  "restatement", "accounting irregular", "resigns", "steps down",
  "downgrade", "bankruptcy", "default", "misses estimates", "profit warning",
  "layoff", "recall", "probe", "class action", "sued"
)
.ynow_news_pos <- c(
  "beat estimates", "beats estimates", "upgrade", "buyback", "share repurchase",
  "special dividend", "expansion", "raises guidance", "record revenue",
  "partnership", "capacity expansion"
)
.ynow_news_flags <- list(
  management = c("resigns", "steps down", "management change", "ceo depart", "cfo depart"),
  sec = c("sec investigation", "sec probe", "regulatory investigation", "regulator"),
  lawsuit = c("lawsuit", "sued", "litigation", "class action")
)
.ynow_news_nudge_capex <- c("expansion", "capacity expansion", "new plant", "capital expenditure", "capex")
.ynow_news_nudge_div <- c("special dividend", "buyback", "share repurchase", "treasury stock")

ynow_news_score_headline <- function(title) {
  t <- tolower(as.character(title %||% "")[1])
  if (!nzchar(t)) return(0)
  neg <- sum(vapply(.ynow_news_neg, function(w) grepl(w, t, fixed = TRUE), logical(1)))
  pos <- sum(vapply(.ynow_news_pos, function(w) grepl(w, t, fixed = TRUE), logical(1)))
  if (neg + pos == 0L) return(0)
  max(-1, min(1, (pos - neg) / (pos + neg)))
}

ynow_news_flags_for_title <- function(title) {
  t <- tolower(as.character(title %||% "")[1])
  names(Filter(function(words) {
    any(vapply(words, function(w) grepl(w, t, fixed = TRUE), logical(1)))
  }, .ynow_news_flags))
}

ynow_news_nudges_for_titles <- function(titles) {
  blob <- tolower(paste(as.character(titles %||% ""), collapse = " "))
  out <- character()
  if (any(vapply(.ynow_news_nudge_capex, function(w) grepl(w, blob, fixed = TRUE), logical(1)))) {
    out <- c(out, "capex")
  }
  if (any(vapply(.ynow_news_nudge_div, function(w) grepl(w, blob, fixed = TRUE), logical(1)))) {
    out <- c(out, "dividend")
  }
  out
}

ynow_news_score_bundle <- function(titles) {
  titles <- as.character(titles %||% character())
  titles <- titles[nzchar(titles)]
  if (!length(titles)) {
    return(list(
      score = NA_real_, n = 0L, flags = character(), nudges = character(),
      rows = data.frame(title = character(), score = numeric(), stringsAsFactors = FALSE)
    ))
  }
  scores <- vapply(titles, ynow_news_score_headline, numeric(1))
  flags <- unique(unlist(lapply(titles, ynow_news_flags_for_title), use.names = FALSE))
  list(
    score = mean(scores),
    n = length(titles),
    flags = flags,
    nudges = ynow_news_nudges_for_titles(titles),
    rows = data.frame(title = titles, score = as.numeric(scores), stringsAsFactors = FALSE)
  )
}

.ynow_news_xml_field <- function(chunk, tag) {
  m <- regexpr(
    paste0("<", tag, "\\b[^>]*>.*?</", tag, ">"),
    chunk, perl = TRUE, ignore.case = TRUE
  )
  if (is.na(m) || m < 0L) return("")
  raw <- regmatches(chunk, m)
  text <- gsub("<!\\[CDATA\\[|\\]\\]>", "", raw)
  text <- gsub("<[^>]+>", "", text)
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  text <- gsub("&quot;", "\"", text, fixed = TRUE)
  trimws(text)
}

.ynow_news_parse_pubdate <- function(pub) {
  pub <- trimws(as.character(pub %||% "")[1])
  if (!nzchar(pub)) return(as.POSIXct(NA_real_, tz = "UTC"))
  pub <- sub("\\s+GMT$", " +0000", pub)
  pub <- sub("\\s+UT$", " +0000", pub)
  parsed <- suppressWarnings(as.POSIXct(pub, format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC"))
  if (length(parsed) != 1L || !is.finite(parsed)) as.POSIXct(NA_real_, tz = "UTC") else parsed
}

ynow_news_parse_rss <- function(xml_text, now = Sys.time(), window_sec = 86400) {
  xml <- paste(as.character(xml_text %||% ""), collapse = "\n")
  empty <- data.frame(title = character(), published = as.POSIXct(character(), tz = "UTC"), stringsAsFactors = FALSE)
  if (!nzchar(xml)) return(empty)
  m <- gregexpr("<item\\b[^>]*>.*?</item>", xml, perl = TRUE, ignore.case = TRUE)
  items <- regmatches(xml, m)[[1]]
  if (!length(items)) return(empty)
  titles <- vapply(items, .ynow_news_xml_field, character(1), tag = "title")
  published <- vapply(items, function(item) {
    as.numeric(.ynow_news_parse_pubdate(.ynow_news_xml_field(item, "pubDate")))
  }, numeric(1))
  published <- as.POSIXct(published, origin = "1970-01-01", tz = "UTC")
  now_utc <- as.POSIXct(as.numeric(now), origin = "1970-01-01", tz = "UTC")
  window_sec <- suppressWarnings(as.numeric(window_sec)[1])
  if (!is.finite(window_sec) || window_sec <= 0) window_sec <- 86400
  age <- as.numeric(now_utc) - as.numeric(published)
  keep <- nzchar(titles) & !grepl("^Yahoo", titles) & is.finite(age) & age >= -900 & age <= window_sec
  if (!any(keep)) return(empty)
  data.frame(title = titles[keep], published = published[keep], stringsAsFactors = FALSE)
}

ynow_news_fetch_rss <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  empty <- data.frame(title = character(), stringsAsFactors = FALSE)
  if (!nzchar(tk)) return(empty)
  url <- paste0(
    "https://feeds.finance.yahoo.com/rss/2.0/headline?s=",
    utils::URLencode(tk, reserved = TRUE),
    "&region=US&lang=en-US"
  )
  xml <- tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(url, httr::timeout(8), httr::user_agent("theYNowApp/news"))
      if (httr::status_code(resp) >= 400) return("")
      httr::content(resp, as = "text", encoding = "UTF-8")
    } else {
      paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }
  }, error = function(e) "")
  ynow_news_parse_rss(xml)
}

ynow_news_bundle <- function(ticker, fetch = NULL) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  key <- paste0("news:", tk)
  if (nzchar(tk) && exists("ynow_market_cache_get", mode = "function") &&
      exists("ynow_market_cache_ttl", mode = "function")) {
    hit <- tryCatch(
      ynow_market_cache_get(key, ynow_market_cache_ttl("news")),
      error = function(e) NULL
    )
    if (is.list(hit) && is.finite(hit$score %||% NA_real_)) return(hit)
  }
  headlines <- if (is.function(fetch)) fetch(tk) else ynow_news_fetch_rss(tk)
  scored <- ynow_news_score_bundle(headlines$title %||% character())
  bundle <- list(
    ok = scored$n > 0L,
    ticker = tk,
    score = scored$score,
    n = scored$n,
    flags = scored$flags,
    nudges = scored$nudges,
    rows = scored$rows,
    method = "lexicon",
    source = "yahoo_rss"
  )
  if (isTRUE(bundle$ok) && exists("ynow_market_cache_put", mode = "function")) {
    tryCatch(ynow_market_cache_put(key, "news", tk, bundle), error = function(e) NULL)
  }
  bundle
}

#' Latest-rebalance only. Does not rewrite earlier Exp_B (no look-ahead).
ynow_news_apply_veto <- function(pos_a, pos_b, rsi_score, news_score, enabled = TRUE) {
  pos_b <- suppressWarnings(as.numeric(pos_b)[1])
  pos_a <- suppressWarnings(as.numeric(pos_a)[1])
  if (!isTRUE(enabled) || !is.finite(news_score) || !is.finite(pos_a) || !is.finite(pos_b)) {
    return(list(pos_b = pos_b, veto = "off"))
  }
  rsi_score <- suppressWarnings(as.numeric(rsi_score)[1])
  if (news_score <= -0.55 && is.finite(rsi_score) && rsi_score >= 0.80) {
    return(list(pos_b = min(pos_b, pos_a), veto = "block_oversold"))
  }
  if (news_score >= 0.55 && is.finite(rsi_score) && rsi_score <= 0.40) {
    return(list(pos_b = max(pos_b, pos_a), veto = "soften_overbought"))
  }
  list(pos_b = pos_b, veto = "none")
}
