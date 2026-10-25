# ==========================================
# Seeking Alpha earnings-call HTML — last-resort fallback only
# ==========================================
# Prefer IR / SEC / licensed API (US) and AlphaMemo index (TW) first.
# SA HTML scrape runs only when those paths return empty/unusable.
# Credibility: last_resort (third-party HTML — not official IR / filing).
# Fail closed: honest empty if blocked (403/captcha), parse miss, or no facts.
# Rate-limited; never invent backlog / bookings / CapEx amounts.
# Secrets never committed — no SA cookies/API keys in repo.

.ynow_sa_ec_chr <- function(x, default = "") {
  v <- as.character(x %||% default)[1]
  if (!nzchar(v) || is.na(v)) default else v
}

.ynow_sa_ec_env <- new.env(parent = emptyenv())
.ynow_sa_ec_env$last_fetch_at <- 0

.ynow_sa_ec_min_interval_sec <- function() {
  v <- suppressWarnings(as.numeric(Sys.getenv("YNOW_SA_SCRAPE_MIN_INTERVAL_SEC", unset = "2.5")))
  if (!is.finite(v) || v < 0.5) 2.5 else min(v, 30)
}

.ynow_sa_ec_enabled <- function() {
  # Default ON for product last-resort; tests may set YNOW_SA_SCRAPE=0 to force skip.
  flag <- tolower(trimws(Sys.getenv("YNOW_SA_SCRAPE", unset = "1")))
  !flag %in% c("0", "false", "no", "off")
}

#' Normalize ticker for SA symbol pages (US bare; TW keep numeric bare).
ynow_sa_earnings_call_symbol <- function(ticker) {
  tk <- toupper(trimws(.ynow_sa_ec_chr(ticker)))
  tk <- sub("\\.(US)$", "", tk, ignore.case = TRUE)
  tk <- sub("\\.(TW|TWO)$", "", tk, ignore.case = TRUE)
  tk
}

ynow_sa_earnings_call_index_url <- function(ticker) {
  tk <- ynow_sa_earnings_call_symbol(ticker)
  if (!nzchar(tk)) return("")
  paste0("https://seekingalpha.com/symbol/", utils::URLencode(tk, reserved = TRUE), "/earnings/transcripts")
}

.ynow_sa_ec_strip_tags <- function(html) {
  txt <- gsub("<script[\\s\\S]*?</script>", " ", html %||% "", perl = TRUE, ignore.case = TRUE)
  txt <- gsub("<style[\\s\\S]*?</style>", " ", txt, perl = TRUE, ignore.case = TRUE)
  txt <- gsub("<[^>]+>", " ", txt, perl = TRUE)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&lt;", "<", txt, fixed = TRUE)
  txt <- gsub("&gt;", ">", txt, fixed = TRUE)
  txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
  txt <- gsub("&#39;", "'", txt, fixed = TRUE)
  gsub("\\s+", " ", txt)
}

#' HTTP GET with browser-like headers + inter-request throttle.
.ynow_sa_ec_http_get <- function(url, timeout_sec = 14) {
  url <- .ynow_sa_ec_chr(url)
  if (!nzchar(url)) return(NULL)
  # Simple process-wide rate limit.
  elapsed <- as.numeric(Sys.time()) - (.ynow_sa_ec_env$last_fetch_at %||% 0)
  wait <- .ynow_sa_ec_min_interval_sec() - elapsed
  if (is.finite(wait) && wait > 0) Sys.sleep(wait)
  .ynow_sa_ec_env$last_fetch_at <- as.numeric(Sys.time())

  ua <- paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  )
  headers <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9",
    "Cache-Control" = "no-cache",
    "Upgrade-Insecure-Requests" = "1"
  )
  tryCatch(
    {
      if (requireNamespace("httr", quietly = TRUE)) {
        resp <- httr::GET(
          url,
          httr::user_agent(ua),
          httr::add_headers(.headers = headers),
          httr::timeout(timeout_sec)
        )
        code <- httr::status_code(resp)
        if (code >= 400L) return(list(ok = FALSE, status_code = code, body = ""))
        list(
          ok = TRUE,
          status_code = code,
          body = httr::content(resp, as = "text", encoding = "UTF-8")
        )
      } else if (requireNamespace("httr2", quietly = TRUE)) {
        req <- httr2::request(url) |>
          httr2::req_timeout(timeout_sec) |>
          httr2::req_user_agent(ua) |>
          httr2::req_headers(!!!as.list(headers))
        resp <- httr2::req_perform(req)
        code <- httr2::resp_status(resp)
        if (code >= 400L) return(list(ok = FALSE, status_code = code, body = ""))
        list(ok = TRUE, status_code = code, body = httr2::resp_body_string(resp))
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )
}

#' Parse first transcript article URL from an SA symbol transcripts index HTML.
.ynow_sa_ec_parse_index_links <- function(html) {
  html <- as.character(html %||% "")[1]
  if (!nzchar(html)) return(character(0))
  # Bot wall / captcha → empty.
  if (grepl("(?i)access to this page has been denied|px-captcha|cf-challenge", html, perl = TRUE)) {
    return(character(0))
  }
  pats <- c(
    'href="(/article/[0-9]+-[^"?#]+(?:earnings[-_]call[-_]transcript|transcript)[^"?#]*)"',
    "href='(/article/[0-9]+-[^'?#]+(?:earnings[-_]call[-_]transcript|transcript)[^'?#]*)'",
    'href="(https://seekingalpha\\.com/article/[0-9]+-[^"?#]+(?:earnings[-_]call[-_]transcript|transcript)[^"?#]*)"',
    'href="(/article/[0-9]+-[^"?#]+)"'
  )
  hits <- character(0)
  for (pat in pats) {
    m <- gregexpr(pat, html, ignore.case = TRUE, perl = TRUE)
    if (m[[1]][1] < 0) next
    caps <- regmatches(html, m)[[1]]
    # Extract group 1 via sub
    for (cap in caps) {
      u <- sub(pat, "\\1", cap, ignore.case = TRUE, perl = TRUE)
      if (!nzchar(u)) next
      if (!grepl("^https?://", u)) u <- paste0("https://seekingalpha.com", u)
      # Prefer transcript-titled paths.
      hits <- c(hits, u)
    }
    if (length(hits)) break
  }
  unique(hits)
}

#' Extract title + plain text from an SA article HTML body.
.ynow_sa_ec_parse_article <- function(html) {
  html <- as.character(html %||% "")[1]
  out <- list(title = "", text = "", blocked = FALSE)
  if (!nzchar(html)) return(out)
  if (grepl("(?i)access to this page has been denied|px-captcha|cf-challenge", html, perl = TRUE)) {
    out$blocked <- TRUE
    return(out)
  }
  # Title candidates
  title <- ""
  for (pat in c(
    'data-test-id="post-title"[^>]*>([^<]+)<',
    "<title>([^<]+)</title>",
    'property="og:title"\\s+content="([^"]+)"',
    "property='og:title'\\s+content='([^']+)'"
  )) {
    m <- regexpr(pat, html, ignore.case = TRUE, perl = TRUE)
    if (m[1] < 0) next
    title <- sub(pat, "\\1", regmatches(html, m)[1], ignore.case = TRUE, perl = TRUE)
    title <- trimws(.ynow_sa_ec_strip_tags(title))
    if (nzchar(title)) break
  }
  out$title <- title
  # Body candidates (prefer article-content / a-body / article-section).
  body_html <- ""
  for (pat in c(
    'data-test-id="article-content"[^>]*>([\\s\\S]*?)</div>',
    'data-test-id="article-section"[^>]*>([\\s\\S]*?)</div>',
    'id="a-body"[^>]*>([\\s\\S]*?)</div>',
    '<article[^>]*>([\\s\\S]*?)</article>'
  )) {
    m <- regexpr(pat, html, ignore.case = TRUE, perl = TRUE)
    if (m[1] < 0) next
    body_html <- sub(pat, "\\1", regmatches(html, m)[1], ignore.case = TRUE, perl = TRUE)
    if (nzchar(body_html) && nchar(body_html) > 200L) break
  }
  if (!nzchar(body_html)) {
    # Soft fallback: strip whole page (still fail closed later if no facts).
    body_html <- html
  }
  text <- trimws(.ynow_sa_ec_strip_tags(body_html))
  # Drop obvious chrome noise.
  text <- sub("(?i)^.*?Earnings Call Transcripts?", "", text, perl = TRUE)
  out$text <- text
  out
}

#' Last-resort SA HTML fetch → facts via shared extractors when available.
#' @param extract_fun optional function(text) → list(backlog, bookings, capex, excerpt, themes)
ynow_sa_earnings_call_fetch_last_resort <- function(ticker, extract_fun = NULL) {
  if (!isTRUE(.ynow_sa_ec_enabled())) return(NULL)
  tk <- ynow_sa_earnings_call_symbol(ticker)
  if (!nzchar(tk)) return(NULL)

  # Allow offline fixture injection for tests (never invent live content).
  fixture <- Sys.getenv("YNOW_SA_SCRAPE_FIXTURE_HTML", unset = "")
  index_html <- ""
  article_url <- ""
  article_html <- ""
  if (nzchar(fixture) && file.exists(fixture)) {
    article_html <- paste(readLines(fixture, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    article_url <- paste0("fixture://", basename(fixture))
  } else {
    index_url <- ynow_sa_earnings_call_index_url(tk)
    idx <- .ynow_sa_ec_http_get(index_url)
    if (!is.list(idx) || !isTRUE(idx$ok)) return(NULL)
    index_html <- idx$body %||% ""
    links <- .ynow_sa_ec_parse_index_links(index_html)
    if (!length(links)) return(NULL)
    # Prefer paths that mention transcript.
    prefer <- links[grepl("transcript", links, ignore.case = TRUE)]
    article_url <- if (length(prefer)) prefer[[1]] else links[[1]]
    art <- .ynow_sa_ec_http_get(article_url)
    if (!is.list(art) || !isTRUE(art$ok)) return(NULL)
    article_html <- art$body %||% ""
  }

  parsed <- .ynow_sa_ec_parse_article(article_html)
  if (isTRUE(parsed$blocked) || !nzchar(parsed$text)) return(NULL)

  if (!is.function(extract_fun) && exists(".ynow_us_ec_extract_facts", mode = "function")) {
    extract_fun <- .ynow_us_ec_extract_facts
  }
  facts <- if (is.function(extract_fun)) {
    tryCatch(extract_fun(parsed$text), error = function(e) NULL)
  } else {
    list(
      backlog = NA_real_, bookings = NA_real_, capex = NA_real_,
      excerpt = substr(gsub("\\s+", " ", parsed$text), 1L, 420L),
      themes = character(0)
    )
  }
  if (!is.list(facts)) return(NULL)
  excerpt <- .ynow_sa_ec_chr(facts$excerpt)
  if (!nzchar(excerpt)) {
    excerpt <- substr(gsub("\\s+", " ", parsed$text), 1L, 420L)
  }
  usable <- is.finite(facts$backlog %||% NA_real_) ||
    is.finite(facts$bookings %||% NA_real_) ||
    is.finite(facts$capex %||% NA_real_) ||
    length(facts$suppliers %||% list()) > 0L ||
    length(facts$customers %||% list()) > 0L ||
    length(facts$order_lines %||% list()) > 0L ||
    nzchar(excerpt)
  if (!isTRUE(usable)) return(NULL)

  out <- list(
    ok = TRUE,
    status = "ok",
    source_id = "seeking_alpha_html",
    credibility = "last_resort",
    title = .ynow_sa_ec_chr(parsed$title, paste(tk, "Seeking Alpha transcript")),
    url = article_url,
    published = "",
    backlog = facts$backlog %||% NA_real_,
    bookings = facts$bookings %||% NA_real_,
    capex = facts$capex %||% NA_real_,
    themes = facts$themes %||% character(0),
    suppliers = facts$suppliers %||% list(),
    customers = facts$customers %||% list(),
    order_lines = facts$order_lines %||% list(),
    excerpt = excerpt,
    sa_url = ynow_sa_earnings_call_index_url(tk)
  )
  if (exists(".ynow_ec_finalize_with_keywords", mode = "function")) {
    out <- .ynow_ec_finalize_with_keywords(out, text = parsed$text)
  }
  out
}

#' Whether a preferred-path result is usable enough to skip SA.
ynow_ec_result_usable <- function(res) {
  if (!is.list(res) || !isTRUE(res$ok)) return(FALSE)
  if (is.finite(suppressWarnings(as.numeric(res$backlog)))) return(TRUE)
  if (is.finite(suppressWarnings(as.numeric(res$bookings)))) return(TRUE)
  if (is.finite(suppressWarnings(as.numeric(res$capex)))) return(TRUE)
  if (length(res$suppliers %||% list()) > 0L) return(TRUE)
  if (length(res$customers %||% list()) > 0L) return(TRUE)
  if (length(res$order_lines %||% list()) > 0L) return(TRUE)
  # Status-only notes from FinmoConf presentation PDF (orders / supply chain / outlook).
  if (length(res$order_notes %||% character(0)) > 0L) return(TRUE)
  if (length(res$supply_chain_notes %||% character(0)) > 0L) return(TRUE)
  if (length(res$outlook_notes %||% character(0)) > 0L) return(TRUE)
  if (nzchar(.ynow_sa_ec_chr(res$excerpt)) && identical(.ynow_sa_ec_chr(res$status), "ok")) {
    return(TRUE)
  }
  FALSE
}
