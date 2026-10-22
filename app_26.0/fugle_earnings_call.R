# ==========================================
# Fugle 法說會備忘錄 — free TW memo HTML (Estimate)
# ==========================================
# Topic: https://blog.fugle.tw/topic/earnings-call-memo
# Posts: /post/earnings-call-{ticker}-{YYYY-MM-DD}
# robots.txt: Allow / ; Disallow /api/ and /search only.
# Blog memo / ASR summary — not a full official transcript.
# Credibility: estimate. Never invent amounts. Fail closed.
# AlphaMemo remains human-outbound only (do not scrape AlphaMemo here).

.ynow_fugle_ec_chr <- function(x, default = "") {
  v <- as.character(x %||% default)[1]
  if (!nzchar(v) || is.na(v)) default else v
}

.ynow_fugle_ec_env <- new.env(parent = emptyenv())
.ynow_fugle_ec_env$last_fetch_at <- 0

.ynow_fugle_ec_min_interval_sec <- function() {
  v <- suppressWarnings(as.numeric(Sys.getenv("YNOW_FUGLE_SCRAPE_MIN_INTERVAL_SEC", unset = "1.5")))
  if (!is.finite(v) || v < 0.5) 1.5 else min(v, 30)
}

.ynow_fugle_ec_enabled <- function() {
  flag <- tolower(trimws(Sys.getenv("YNOW_FUGLE_SCRAPE", unset = "1")))
  !flag %in% c("0", "false", "no", "off")
}

ynow_fugle_earnings_call_topic_url <- function() {
  "https://blog.fugle.tw/topic/earnings-call-memo"
}

.ynow_fugle_ec_blocked <- function(html) {
  grepl(
    "(?i)access to this page has been denied|px-captcha|cf-challenge|just a moment",
    as.character(html %||% "")[1],
    perl = TRUE
  )
}

.ynow_fugle_ec_http_get <- function(url, timeout_sec = 8) {
  url <- .ynow_fugle_ec_chr(url)
  if (!nzchar(url)) return(NULL)
  elapsed <- as.numeric(Sys.time()) - (.ynow_fugle_ec_env$last_fetch_at %||% 0)
  wait <- .ynow_fugle_ec_min_interval_sec() - elapsed
  if (is.finite(wait) && wait > 0) Sys.sleep(wait)
  .ynow_fugle_ec_env$last_fetch_at <- as.numeric(Sys.time())

  ua <- "TheYNowApp/1.0 (earnings-memo research; +https://theynow.app)"
  headers <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "zh-TW,zh;q=0.9,en-US;q=0.8,en;q=0.7"
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

.ynow_fugle_ec_strip_tags <- function(html) {
  if (exists(".ynow_sa_ec_strip_tags", mode = "function")) {
    return(.ynow_sa_ec_strip_tags(html))
  }
  txt <- gsub("<script[\\s\\S]*?</script>", " ", html %||% "", perl = TRUE, ignore.case = TRUE)
  txt <- gsub("<style[\\s\\S]*?</style>", " ", txt, perl = TRUE, ignore.case = TRUE)
  txt <- gsub("<[^>]+>", " ", txt, perl = TRUE)
  gsub("\\s+", " ", txt)
}

#' Newest matching post path for a TW bare ticker from topic HTML.
#' Date lives in the path (`...-YYYY-MM-DD`), so a string sort is a date sort.
.ynow_fugle_ec_parse_topic_links <- function(html, bare_ticker) {
  html <- as.character(html %||% "")[1]
  bare <- gsub("[^0-9]", "", .ynow_fugle_ec_chr(bare_ticker))
  if (!nzchar(html) || !nzchar(bare) || isTRUE(.ynow_fugle_ec_blocked(html))) {
    return(character(0))
  }
  pat <- paste0("/post/earnings-call-", bare, "-[0-9]{4}-[0-9]{2}-[0-9]{2}")
  m <- gregexpr(pat, html, perl = TRUE)
  if (m[[1]][1] < 0) return(character(0))
  hits <- unique(regmatches(html, m)[[1]])
  if (!length(hits)) return(character(0))
  hits[order(hits, decreasing = TRUE)]
}

.ynow_fugle_ec_published <- function(url) {
  m <- regexec("earnings-call-[0-9]+-([0-9]{4}-[0-9]{2}-[0-9]{2})", as.character(url %||% "")[1], perl = TRUE)
  parts <- regmatches(as.character(url %||% "")[1], m)[[1]]
  if (length(parts) >= 2L) parts[[2]] else ""
}

#' Prefer the memo body over nav chrome. Fall back to the full document.
.ynow_fugle_ec_main_html <- function(html) {
  html <- as.character(html %||% "")[1]
  if (!nzchar(html)) return("")
  for (pat in c(
    'data-post-content="true"[^>]*>([\\s\\S]*?)</section>',
    "<article[^>]*>([\\s\\S]*?)</article>"
  )) {
    m <- regexpr(pat, html, ignore.case = TRUE, perl = TRUE)
    if (m[1] < 0) next
    body <- sub(pat, "\\1", regmatches(html, m)[1], ignore.case = TRUE, perl = TRUE)
    if (nzchar(body) && nchar(body) > 40L) return(body)
  }
  html
}

.ynow_fugle_ec_parse_title <- function(html) {
  html <- as.character(html %||% "")[1]
  m <- regexpr("<meta[^>]+property=\"og:title\"[^>]+content=\"([^\"]+)\"", html, perl = TRUE, ignore.case = TRUE)
  if (m[1] > 0) {
    hit <- regmatches(html, m)[1]
    tit <- sub('.*content="([^"]+)".*', "\\1", hit, perl = TRUE)
    if (nzchar(tit)) return(tit)
  }
  m2 <- regexpr("<title>([^<]+)</title>", html, perl = TRUE, ignore.case = TRUE)
  if (m2[1] > 0) {
    return(trimws(sub(".*<title>([^<]+)</title>.*", "\\1", regmatches(html, m2)[1], ignore.case = TRUE, perl = TRUE)))
  }
  ""
}

#' Fetch Fugle 法說會備忘錄 for a TW ticker → facts via extract_fun.
ynow_fugle_earnings_call_fetch <- function(ticker, extract_fun = NULL) {
  if (!isTRUE(.ynow_fugle_ec_enabled())) return(NULL)
  tk <- toupper(trimws(.ynow_fugle_ec_chr(ticker)))
  bare <- sub("\\.(TW|TWO)$", "", tk, ignore.case = TRUE)
  if (!nzchar(bare) || !grepl("^[0-9]{4,6}[A-Z]?$", bare)) return(NULL)

  topic_url <- ynow_fugle_earnings_call_topic_url()
  article_html <- ""
  article_url <- ""
  fixture <- Sys.getenv("YNOW_FUGLE_SCRAPE_FIXTURE_HTML", unset = "")
  if (nzchar(fixture) && file.exists(fixture)) {
    article_html <- paste(readLines(fixture, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    links <- .ynow_fugle_ec_parse_topic_links(article_html, bare)
    has_posts <- grepl("/post/earnings-call-[0-9]+-[0-9]{4}-", article_html, perl = TRUE)
    if (isTRUE(has_posts) && !length(links)) return(NULL)
    article_url <- if (length(links)) {
      paste0("https://blog.fugle.tw", links[[1]])
    } else {
      paste0("fixture://", basename(fixture))
    }
  } else {
    idx <- .ynow_fugle_ec_http_get(topic_url)
    if (!is.list(idx) || !isTRUE(idx$ok) || isTRUE(.ynow_fugle_ec_blocked(idx$body))) return(NULL)
    links <- .ynow_fugle_ec_parse_topic_links(idx$body %||% "", bare)
    if (!length(links)) return(NULL)
    article_url <- paste0("https://blog.fugle.tw", links[[1]])
    art <- .ynow_fugle_ec_http_get(article_url)
    if (!is.list(art) || !isTRUE(art$ok) || isTRUE(.ynow_fugle_ec_blocked(art$body))) return(NULL)
    article_html <- art$body %||% ""
  }
  if (!nzchar(article_html) || isTRUE(.ynow_fugle_ec_blocked(article_html))) return(NULL)

  text <- .ynow_fugle_ec_strip_tags(.ynow_fugle_ec_main_html(article_html))
  text <- sub("^\\s*文章內容僅供參考[^。]*。\\s*", "", text)
  if (!nzchar(trimws(text))) return(NULL)
  title <- .ynow_fugle_ec_parse_title(article_html)
  if (!nzchar(title)) title <- paste(bare, "Fugle 法說會備忘錄")

  if (!is.function(extract_fun) && exists(".ynow_us_ec_extract_facts", mode = "function")) {
    extract_fun <- .ynow_us_ec_extract_facts
  }
  facts <- if (is.function(extract_fun)) {
    tryCatch(extract_fun(text), error = function(e) NULL)
  } else {
    list(
      backlog = NA_real_, bookings = NA_real_, capex = NA_real_,
      excerpt = substr(gsub("\\s+", " ", text), 1L, 420L),
      themes = character(0),
      suppliers = list(), customers = list(), order_lines = list()
    )
  }
  if (!is.list(facts)) return(NULL)
  excerpt <- .ynow_fugle_ec_chr(facts$excerpt)
  if (!nzchar(excerpt)) {
    excerpt <- substr(gsub("\\s+", " ", text), 1L, 420L)
  }
  usable <- is.finite(facts$backlog %||% NA_real_) ||
    is.finite(facts$bookings %||% NA_real_) ||
    is.finite(facts$capex %||% NA_real_) ||
    length(facts$suppliers %||% list()) > 0L ||
    length(facts$customers %||% list()) > 0L ||
    length(facts$order_lines %||% list()) > 0L ||
    nzchar(excerpt)
  if (!isTRUE(usable)) return(NULL)

  list(
    ok = TRUE,
    status = "ok",
    source_id = "fugle_earnings_memo",
    credibility = "estimate",
    title = title,
    url = article_url,
    published = .ynow_fugle_ec_published(article_url),
    backlog = facts$backlog %||% NA_real_,
    bookings = facts$bookings %||% NA_real_,
    capex = facts$capex %||% NA_real_,
    themes = facts$themes %||% character(0),
    suppliers = facts$suppliers %||% list(),
    customers = facts$customers %||% list(),
    order_lines = facts$order_lines %||% list(),
    excerpt = excerpt,
    fugle_url = topic_url
  )
}
