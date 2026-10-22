# ==========================================
# US earnings-call / transcript ingest (product path)
# ==========================================
# Methodology inspiration (財報狗 Statementdog blog — reading framework only,
# not a scrape target of their site): CEO outlook / CFO near-term guidance /
# CapEx & capital plan / analyst Q&A themes. Product sources (ranked):
# licensed transcript API (env key) → SEC EDGAR 8-K/6-K body (Item 2.02) →
# company IR link-out → Seeking Alpha HTML scrape as **last-resort** only
# (credibility = last_resort; third-party HTML — not official IR).
# Never invent backlog / bookings / CapEx amounts. Honest empty when all fail.

.ynow_us_ec_chr <- function(x, default = "") {
  v <- as.character(x %||% default)[1]
  if (!nzchar(v) || is.na(v)) default else v
}

.ynow_us_ec_num <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1L || !is.finite(v)) NA_real_ else v
}

#' Seeking Alpha transcripts index URL (human outbound; scrape is last-resort only).
ynow_us_earnings_call_sa_url <- function(ticker) {
  tk <- toupper(trimws(.ynow_us_ec_chr(ticker)))
  tk <- sub("\\.(US)$", "", tk, ignore.case = TRUE)
  if (!nzchar(tk) || grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)) return("")
  if (exists("ynow_sa_earnings_call_index_url", mode = "function")) {
    return(ynow_sa_earnings_call_index_url(tk))
  }
  paste0("https://seekingalpha.com/symbol/", utils::URLencode(tk, reserved = TRUE), "/earnings/transcripts")
}

#' SEC EDGAR full-text search for recent 8-K filings (human outbound).
ynow_us_earnings_call_sec_search_url <- function(ticker) {
  tk <- toupper(trimws(.ynow_us_ec_chr(ticker)))
  tk <- sub("\\.(US)$", "", tk, ignore.case = TRUE)
  if (!nzchar(tk) || grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)) return("")
  paste0(
    "https://www.sec.gov/edgar/search/#/category=custom&forms=8-K%252C8-K%252FA%252C6-K%252C6-K%252FA&entityName=",
    utils::URLencode(tk, reserved = TRUE)
  )
}

#' Company IR / investor-relations Google query (human outbound; no scrape).
ynow_us_earnings_call_ir_search_url <- function(ticker) {
  tk <- toupper(trimws(.ynow_us_ec_chr(ticker)))
  tk <- sub("\\.(US)$", "", tk, ignore.case = TRUE)
  if (!nzchar(tk)) return("")
  q <- paste(tk, "investor relations earnings call transcript")
  paste0("https://www.google.com/search?q=", utils::URLencode(q, reserved = TRUE))
}

.ynow_us_ec_api_key <- function() {
  keys <- c(
    Sys.getenv("YNOW_FMP_API_KEY", unset = ""),
    Sys.getenv("FMP_API_KEY", unset = ""),
    Sys.getenv("YNOW_TRANSCRIPT_API_KEY", unset = "")
  )
  keys <- trimws(keys)
  keys <- keys[nzchar(keys)]
  if (!length(keys)) "" else keys[[1]]
}

.ynow_us_ec_money_to_num <- function(piece) {
  piece <- as.character(piece %||% "")[1]
  if (!nzchar(piece)) return(NA_real_)
  # zh-TW: 8.54億元 / 1,404 百萬 / 7,043 萬元 (absolute TWD when unit is 台幣-scale).
  zh <- .ynow_us_ec_money_to_num_zh(piece)
  if (is.finite(zh)) return(zh)
  m <- regexpr(
    "([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b",
    piece,
    ignore.case = TRUE,
    perl = TRUE
  )
  if (m[1] < 0) return(NA_real_)
  hit <- regmatches(piece, m)[1]
  n <- suppressWarnings(as.numeric(sub(
    "^([0-9.]+).*", "\\1", hit, perl = TRUE
  )))
  unit <- tolower(sub(
    "^[0-9.]+\\s*(billion|million|bn|mm|m)\\b.*$",
    "\\1",
    hit,
    ignore.case = TRUE,
    perl = TRUE
  ))
  if (!is.finite(n)) return(NA_real_)
  if (unit %in% c("billion", "bn")) return(n * 1e9)
  if (unit %in% c("million", "mm", "m")) return(n * 1e6)
  NA_real_
}

#' Parse zh-TW money phrases (億／百萬／萬). Returns absolute currency units.
.ynow_us_ec_money_to_num_zh <- function(piece) {
  piece <- as.character(piece %||% "")[1]
  if (!nzchar(piece)) return(NA_real_)
  piece <- gsub(",", "", piece, fixed = TRUE)
  # 1 億 = 1e8, including 億美元. Do not scale it as an English billion (1e9).
  m_usd <- regexpr(
    "([0-9]+(?:\\.[0-9]+)?)\\s*億\\s*美元",
    piece,
    perl = TRUE
  )
  if (m_usd[1] > 0) {
    n <- suppressWarnings(as.numeric(sub(
      "^.*?([0-9]+(?:\\.[0-9]+)?)\\s*億\\s*美元.*$", "\\1",
      regmatches(piece, m_usd)[1], perl = TRUE
    )))
    if (is.finite(n)) return(n * 1e8)
  }
  m_yi <- regexpr("([0-9]+(?:\\.[0-9]+)?)\\s*億\\s*元?", piece, perl = TRUE)
  if (m_yi[1] > 0) {
    n <- suppressWarnings(as.numeric(sub(
      "^.*?([0-9]+(?:\\.[0-9]+)?)\\s*億.*$", "\\1",
      regmatches(piece, m_yi)[1], perl = TRUE
    )))
    if (is.finite(n)) return(n * 1e8)
  }
  m_bai <- regexpr("([0-9]+(?:\\.[0-9]+)?)\\s*百萬", piece, perl = TRUE)
  if (m_bai[1] > 0) {
    n <- suppressWarnings(as.numeric(sub(
      "^.*?([0-9]+(?:\\.[0-9]+)?)\\s*百萬.*$", "\\1",
      regmatches(piece, m_bai)[1], perl = TRUE
    )))
    if (is.finite(n)) return(n * 1e6)
  }
  # 萬元 only. Bare 萬 also matches 萬張 (share lots) and is not a currency amount.
  m_wan <- regexpr("([0-9]+(?:\\.[0-9]+)?)\\s*萬\\s*元", piece, perl = TRUE)
  if (m_wan[1] > 0) {
    n <- suppressWarnings(as.numeric(sub(
      "^.*?([0-9]+(?:\\.[0-9]+)?)\\s*萬.*$", "\\1",
      regmatches(piece, m_wan)[1], perl = TRUE
    )))
    if (is.finite(n)) return(n * 1e4)
  }
  NA_real_
}

#' Conservative party rows for 業務拆解 order research (name + share or amount only).
#' Never invent names or figures; skip vague "a large customer" without identity.
.ynow_us_ec_extract_parties <- function(text) {
  txt <- as.character(text %||% "")[1]
  empty <- list(suppliers = list(), customers = list(), order_lines = list())
  if (!nzchar(txt)) return(empty)
  money_re <- "([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b"
  pct_re <- "([0-9]+(?:\\.[0-9]+)?)\\s*%"
  # Capitalised English name tokens (Apple, TSMC, Foxconn Technology).
  name_re <- "([A-Z][A-Za-z0-9&.'\\-]{1,40}(?:\\s+[A-Z][A-Za-z0-9&.'\\-]{1,40}){0,3})"
  stop_names <- c(
    "The", "Our", "A", "An", "This", "That", "Customer", "Supplier",
    "Major", "Largest", "Key", "Revenue", "Sales", "Net", "Total"
  )

  push_rows <- function(patterns, kind) {
    rows <- list()
    seen <- character(0)
    for (pat in patterns) {
      m <- gregexpr(pat, txt, perl = TRUE)
      if (m[[1]][1] < 0) next
      starts <- as.integer(m[[1]])
      lens <- attr(m[[1]], "match.length")
      for (i in seq_along(starts)) {
        hit <- substr(txt, starts[[i]], starts[[i]] + lens[[i]] - 1L)
        nm_m <- regexpr(name_re, hit, perl = TRUE)
        if (nm_m[1] < 0) next
        nm <- regmatches(hit, nm_m)[1]
        if (!nzchar(nm) || nm %in% seen || nm %in% stop_names) next
        share <- NA_real_
        amt <- NA_real_
        pm <- regexpr(pct_re, hit, ignore.case = TRUE, perl = TRUE)
        if (pm[1] > 0) {
          share <- suppressWarnings(as.numeric(sub(
            "^([0-9.]+).*", "\\1", regmatches(hit, pm)[1], perl = TRUE
          ))) / 100
        }
        mm <- regexpr(money_re, hit, ignore.case = TRUE, perl = TRUE)
        if (mm[1] > 0) amt <- .ynow_us_ec_money_to_num(regmatches(hit, mm)[1])
        if (!is.finite(share) && !is.finite(amt)) next
        seen <- c(seen, nm)
        rows[[length(rows) + 1L]] <- list(
          name = nm,
          share = if (is.finite(share)) share else NA_real_,
          amount = if (is.finite(amt)) amt else NA_real_,
          kind = kind
        )
        if (length(rows) >= 5L) return(rows)
      }
    }
    rows
  }

  customers <- push_rows(c(
    paste0(name_re, "[^.]{0,80}(?:accounted for|contributed|represented|comprised)[^.]{0,50}", pct_re, "[^.]{0,40}(?:revenue|sales)"),
    paste0("(?:[Ll]argest|[Mm]ajor|[Kk]ey) customer[, ]+", name_re, "[^.]{0,60}", pct_re),
    paste0("(?:[Ll]argest|[Mm]ajor|[Kk]ey) customer[, ]+", name_re, "[^.]{0,60}", money_re),
    paste0("[Cc]ustomer ", name_re, "[^.]{0,60}(?:accounted for|contributed)[^.]{0,40}", pct_re)
  ), "customer")

  suppliers <- push_rows(c(
    paste0("(?:[Ll]argest|[Mm]ajor|[Kk]ey) supplier[, ]+", name_re, "[^.]{0,60}", pct_re),
    paste0("(?:[Ll]argest|[Mm]ajor|[Kk]ey) supplier[, ]+", name_re, "[^.]{0,60}", money_re),
    paste0("[Ss]upplier ", name_re, "[^.]{0,60}(?:accounted for|represented)[^.]{0,40}", pct_re)
  ), "supplier")

  order_lines <- push_rows(c(
    paste0("(?:[Oo]rders?|[Bb]ookings?) from ", name_re, "[^.]{0,60}", money_re),
    paste0(name_re, "[^.]{0,40}(?:orders?|bookings?)[^.]{0,40}", money_re)
  ), "order")

  list(suppliers = suppliers, customers = customers, order_lines = order_lines)
}

#' Pull numbers / theme flags / party rows only when the transcript text states them.
#' End goal: feed 業務拆解 order-research buckets (orders / suppliers / customers).
#' Theme flags follow Statementdog reading axes (status-only; no invented amounts).
.ynow_us_ec_extract_facts <- function(text) {
  txt <- as.character(text %||% "")[1]
  out <- list(
    backlog = NA_real_,
    bookings = NA_real_,
    capex = NA_real_,
    excerpt = "",
    themes = character(0),
    suppliers = list(),
    customers = list(),
    order_lines = list()
  )
  if (!nzchar(txt)) return(out)
  # Keep a short excerpt for UI (not a full republished transcript).
  clean <- gsub("\\s+", " ", txt)
  out$excerpt <- substr(clean, 1L, 420L)
  # Capture "$X.X billion/million" near backlog / RPO / bookings / CapEx — no invented amounts.
  grab <- function(patterns) {
    for (pat in patterns) {
      m <- regexpr(pat, txt, ignore.case = TRUE, perl = TRUE)
      if (m[1] < 0) next
      hit <- regmatches(txt, m)[1]
      num_m <- regexpr(
        "([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b",
        hit,
        ignore.case = TRUE,
        perl = TRUE
      )
      if (num_m[1] < 0) next
      piece <- regmatches(hit, num_m)[1]
      n <- .ynow_us_ec_money_to_num(piece)
      if (is.finite(n)) return(n)
    }
    NA_real_
  }
  out$backlog <- grab(c(
    "(?i)(?:remaining performance obligations?|\\bRPO\\b|backlog)[^.]{0,80}(?:of|=|:)?[^.]{0,40}([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b",
    "(?i)([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b[^.]{0,40}(?:remaining performance obligations?|\\bRPO\\b|backlog)"
  ))
  out$bookings <- grab(c(
    "(?i)(?:bookings|net new bookings)[^.]{0,80}(?:of|=|:)?[^.]{0,40}([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b",
    "(?i)([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b[^.]{0,40}(?:bookings|net new bookings)"
  ))
  out$capex <- grab(c(
    "(?i)(?:capital expenditures?|\\bCapEx\\b|capex)[^.]{0,80}(?:of|=|:)?[^.]{0,40}([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b",
    "(?i)([0-9]+(?:\\.[0-9]+)?)\\s*(billion|million|bn|mm|m)\\b[^.]{0,40}(?:capital expenditures?|\\bCapEx\\b|capex)"
  ))
  # zh-TW amount patterns (Fugle memo / TW IR text). Only when stated near keywords.
  grab_zh <- function(patterns) {
    for (pat in patterns) {
      m <- regexpr(pat, txt, perl = TRUE)
      if (m[1] < 0) next
      hit <- regmatches(txt, m)[1]
      n <- .ynow_us_ec_money_to_num_zh(hit)
      if (is.finite(n)) return(n)
    }
    NA_real_
  }
  zh_amt <- "(?:億\\s*元?|百萬|萬\\s*元)"
  if (!is.finite(out$backlog)) {
    out$backlog <- grab_zh(c(
      paste0("在手訂單[^。]{0,40}([0-9,\\.]+)\\s*", zh_amt),
      paste0("([0-9,\\.]+)\\s*", zh_amt, "[^。]{0,20}在手訂單")
    ))
  }
  if (!is.finite(out$bookings)) {
    # (?<!在手) keeps 在手訂單 on backlog. Bare 訂單 still counts when stated alone.
    out$bookings <- grab_zh(c(
      paste0("(?<!在手)(?:新訂單|接單|訂單金額|訂單)[^。]{0,40}([0-9,\\.]+)\\s*", zh_amt),
      paste0("([0-9,\\.]+)\\s*", zh_amt, "[^。]{0,16}(?<!在手)(?:新訂單|接單|訂單金額)")
    ))
  }
  if (!is.finite(out$capex)) {
    out$capex <- grab_zh(c(
      paste0("(?:資本支出|CapEx|capex)[^。]{0,40}([0-9,\\.]+)\\s*", zh_amt),
      paste0("([0-9,\\.]+)\\s*", zh_amt, "[^。]{0,20}(?:資本支出|CapEx|capex)")
    ))
  }
  themes <- character(0)
  if (grepl("(?i)\\b(guidance|outlook|expect(?:s|ed|ing)?\\s+(?:revenue|EPS|margin)|next\\s+quarter)", txt, perl = TRUE) ||
        grepl("展望", txt, fixed = TRUE)) {
    themes <- c(themes, "cfo_guidance")
  }
  if (grepl("(?i)\\b(capital expenditures?|\\bCapEx\\b|capex|capacity\\s+investment|fab\\s+build)", txt, perl = TRUE) ||
        grepl("資本支出", txt, fixed = TRUE)) {
    themes <- c(themes, "capex_plan")
  }
  if (grepl("(?i)\\b(question[- ]and[- ]answer|\\bQ\\s*&\\s*A\\b|analyst\\s+question)", txt, perl = TRUE) ||
        grepl("問答", txt, fixed = TRUE)) {
    themes <- c(themes, "analyst_qa")
  }
  if (grepl("(?i)\\b(long[- ]term|multi[- ]year|secular|competitive\\s+advantage)\\b", txt, perl = TRUE) ||
        grepl("展望", txt, fixed = TRUE)) {
    themes <- c(themes, "ceo_outlook")
  }
  out$themes <- unique(themes)
  parties <- .ynow_us_ec_extract_parties(txt)
  # zh-TW party hints (name + share/% only when both appear nearby).
  zh_parties <- .ynow_us_ec_extract_parties_zh(txt)
  if (length(zh_parties$customers)) {
    parties$customers <- c(parties$customers, zh_parties$customers)
  }
  if (length(zh_parties$suppliers)) {
    parties$suppliers <- c(parties$suppliers, zh_parties$suppliers)
  }
  if (length(zh_parties$order_lines)) {
    parties$order_lines <- c(parties$order_lines, zh_parties$order_lines)
  }
  out$suppliers <- parties$suppliers
  out$customers <- parties$customers
  out$order_lines <- parties$order_lines
  out
}

#' Conservative zh-TW customer/supplier/order rows (share or amount required).
.ynow_us_ec_extract_parties_zh <- function(text) {
  txt <- as.character(text %||% "")[1]
  empty <- list(suppliers = list(), customers = list(), order_lines = list())
  if (!nzchar(txt)) return(empty)
  name_re <- "([\u4e00-\u9fffA-Za-z0-9&]{2,16})"
  stop_names <- c(
    "主要", "最大", "單一", "前大", "營收", "公司", "客戶", "供應商",
    "占營收", "佔營收", "我們", "本期", "其中", "訂單", "展望"
  )
  pct_re <- "([0-9]+(?:\\.[0-9]+)?)\\s*%"
  money_re <- "([0-9,\\.]+)\\s*(?:億\\s*元?|百萬|萬\\s*元)"
  push <- function(patterns, kind) {
    rows <- list()
    seen <- character(0)
    for (pat in patterns) {
      m <- gregexpr(pat, txt, perl = TRUE)
      if (m[[1]][1] < 0) next
      starts <- as.integer(m[[1]])
      lens <- attr(m[[1]], "match.length")
      for (i in seq_along(starts)) {
        hit <- substr(txt, starts[[i]], starts[[i]] + lens[[i]] - 1L)
        nm_m <- regexec(
          "(?:客戶|供應商|來自)(?:為|是|包括)?\\s*([\u4e00-\u9fffA-Za-z0-9&]{2,16})",
          hit,
          perl = TRUE
        )
        parts <- regmatches(hit, nm_m)[[1]]
        if (length(parts) < 2L) next
        nm <- parts[[2]]
        if (!nzchar(nm) || nm %in% seen || nm %in% stop_names) next
        share <- NA_real_
        amt <- NA_real_
        pm <- regexpr(pct_re, hit, perl = TRUE)
        if (pm[1] > 0) {
          share <- suppressWarnings(as.numeric(sub(
            "^([0-9.]+).*", "\\1", regmatches(hit, pm)[1], perl = TRUE
          ))) / 100
        }
        amt <- .ynow_us_ec_money_to_num_zh(hit)
        if (!is.finite(share) && !is.finite(amt)) next
        seen <- c(seen, nm)
        rows[[length(rows) + 1L]] <- list(
          name = nm,
          share = if (is.finite(share)) share else NA_real_,
          amount = if (is.finite(amt)) amt else NA_real_,
          kind = kind
        )
        if (length(rows) >= 5L) return(rows)
      }
    }
    rows
  }
  customers <- push(c(
    paste0("(?:主要|重大|關鍵)?客戶(?:為|是|包括)?\\s*", name_re, "[^。]{0,24}(?:佔|占)[^。]{0,12}", pct_re),
    paste0("(?:主要|重大|關鍵)?客戶(?:為|是|包括)?\\s*", name_re, "[^。]{0,24}", money_re)
  ), "customer")
  suppliers <- push(c(
    paste0("(?:主要|重大|關鍵)?供應商(?:為|是|包括)?\\s*", name_re, "[^。]{0,30}", money_re),
    paste0("(?:主要|重大|關鍵)?供應商(?:為|是|包括)?\\s*", name_re, "[^。]{0,24}(?:佔|占)[^。]{0,12}", pct_re)
  ), "supplier")
  order_lines <- push(c(
    paste0("來自", name_re, "(?:的)?訂單[^。]{0,24}", money_re)
  ), "order")
  list(suppliers = suppliers, customers = customers, order_lines = order_lines)
}

.ynow_us_ec_from_fmp <- function(ticker, api_key) {
  tk <- toupper(trimws(.ynow_us_ec_chr(ticker)))
  tk <- sub("\\.(US)$", "", tk, ignore.case = TRUE)
  if (!nzchar(tk) || !nzchar(api_key)) return(NULL)
  if (!requireNamespace("httr", quietly = TRUE) && !requireNamespace("httr2", quietly = TRUE)) {
    return(NULL)
  }
  # Prefer year/quarter-less latest transcript endpoints when licensed key exists.
  urls <- c(
    sprintf(
      "https://financialmodelingprep.com/stable/earning-call-transcript?symbol=%s&apikey=%s",
      utils::URLencode(tk, reserved = TRUE),
      utils::URLencode(api_key, reserved = TRUE)
    ),
    sprintf(
      "https://financialmodelingprep.com/api/v4/earning_call_transcript?symbol=%s&apikey=%s",
      utils::URLencode(tk, reserved = TRUE),
      utils::URLencode(api_key, reserved = TRUE)
    )
  )
  body <- NULL
  used <- ""
  for (u in urls) {
    raw <- tryCatch(
      {
        if (requireNamespace("httr", quietly = TRUE)) {
          resp <- httr::GET(u, httr::timeout(12))
          if (httr::status_code(resp) >= 400L) return(NULL)
          httr::content(resp, as = "text", encoding = "UTF-8")
        } else {
          req <- httr2::request(u) |> httr2::req_timeout(12)
          resp <- httr2::req_perform(req)
          httr2::resp_body_string(resp)
        }
      },
      error = function(e) NULL
    )
    if (!nzchar(raw %||% "")) next
    parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) next
    # Shape: list of transcripts or single object.
    row <- if (is.list(parsed) && !is.null(parsed$content)) {
      parsed
    } else if (is.list(parsed) && length(parsed) >= 1L && is.list(parsed[[1]])) {
      parsed[[1]]
    } else {
      NULL
    }
    if (is.null(row)) next
    content <- .ynow_us_ec_chr(row$content %||% row$transcript %||% row$text %||% "")
    if (!nzchar(content)) next
    body <- row
    body$`_content` <- content
    used <- u
    break
  }
  if (is.null(body)) return(NULL)
  facts <- .ynow_us_ec_extract_facts(body$`_content`)
  list(
    ok = TRUE,
    status = if (is.finite(facts$backlog) || is.finite(facts$bookings) ||
                 is.finite(facts$capex) || nzchar(facts$excerpt) ||
                 length(facts$suppliers) || length(facts$customers) ||
                 length(facts$order_lines)) {
      "ok"
    } else {
      "empty"
    },
    source_id = "licensed_api",
    credibility = "management",
    title = .ynow_us_ec_chr(
      body$title %||% body$symbol %||% paste(tk, "earnings call transcript"),
      paste(tk, "earnings call transcript")
    ),
    url = .ynow_us_ec_chr(body$url %||% body$link %||% ""),
    published = .ynow_us_ec_chr(body$date %||% body$publishedDate %||% ""),
    backlog = facts$backlog,
    bookings = facts$bookings,
    capex = facts$capex,
    themes = facts$themes,
    suppliers = facts$suppliers %||% list(),
    customers = facts$customers %||% list(),
    order_lines = facts$order_lines %||% list(),
    excerpt = facts$excerpt,
    api_endpoint_host = tryCatch(httr::parse_url(used)$hostname, error = function(e) "financialmodelingprep.com")
  )
}

.ynow_us_ec_from_sec_8k <- function(ticker) {
  tk <- toupper(trimws(.ynow_us_ec_chr(ticker)))
  tk <- sub("\\.(US)$", "", tk, ignore.case = TRUE)
  if (!nzchar(tk)) return(NULL)
  if (!exists("cached_fetch_sec_report_notes", mode = "function") &&
        !exists("fetch_sec_report_notes", mode = "function")) {
    return(NULL)
  }
  fetch_fn <- if (exists("cached_fetch_sec_report_notes", mode = "function")) {
    cached_fetch_sec_report_notes
  } else {
    fetch_sec_report_notes
  }
  res <- tryCatch(fetch_fn(tk, "8-K", max_chars = 4000L), error = function(e) NULL)
  if (!is.list(res) || !isTRUE(res$ok %||% FALSE)) return(NULL)
  # Python sec_report_notes returns parallel lists: short_names / urls /
  # excerpts / full_texts (see deep_scraper._sec_empty_result). Older probes
  # looked for title/text and always got empty → skipped free SEC path.
  titles <- as.character(unlist(res$short_names %||% character(0)))
  urls <- as.character(unlist(res$urls %||% character(0)))
  texts_full <- as.character(unlist(res$full_texts %||% character(0)))
  texts_ex <- as.character(unlist(res$excerpts %||% character(0)))
  texts <- if (length(texts_full)) texts_full else texts_ex
  # Legacy / alternate shapes (keep as fallback only).
  if (!length(titles)) {
    titles <- as.character(res$title %||% res$note_title %||% character(0))
  }
  if (!length(texts)) {
    texts <- as.character(res$text %||% res$note_text %||% character(0))
  }
  if (!length(urls)) {
    urls <- as.character(res$url %||% res$note_url %||% character(0))
  }
  if (!length(texts) && nzchar(.ynow_us_ec_chr(res$primary_text %||% ""))) {
    texts <- .ynow_us_ec_chr(res$primary_text)
    titles <- .ynow_us_ec_chr(res$form %||% "8-K")
    urls <- .ynow_us_ec_chr(res$primary_doc_url %||% "")
  }
  pick <- 1L
  if (length(titles)) {
    hit <- which(grepl(
      "2\\.02|Results of Operations|earnings|transcript|Exhibit\\s*99",
      titles,
      ignore.case = TRUE
    ))
    if (length(hit)) pick <- hit[[1]]
  }
  text <- if (length(texts) >= pick) texts[[pick]] else paste(texts, collapse = "\n")
  if (!nzchar(text) && length(res$text)) {
    text <- paste(as.character(res$text), collapse = "\n")
  }
  if (!nzchar(text) && is.list(res) && !is.null(res$note_text)) {
    text <- paste(as.character(unlist(res$note_text)), collapse = "\n")
  }
  if (!nzchar(text)) {
    for (nm in c("text", "body", "content", "excerpts", "full_texts")) {
      if (!is.null(res[[nm]])) {
        text <- paste(as.character(unlist(res[[nm]])), collapse = "\n")
        if (nzchar(text)) break
      }
    }
  }
  # Cap extractor input (full_texts can be long HTML-stripped bodies).
  if (nzchar(text) && nchar(text) > 24000L) {
    text <- substr(text, 1L, 24000L)
  }
  facts <- .ynow_us_ec_extract_facts(text)
  list(
    ok = TRUE,
    status = if (is.finite(facts$backlog) || is.finite(facts$bookings) ||
                 is.finite(facts$capex) || nzchar(facts$excerpt) ||
                 length(facts$suppliers) || length(facts$customers) ||
                 length(facts$order_lines)) {
      "ok"
    } else {
      "empty"
    },
    source_id = "sec_8k",
    credibility = "filing",
    title = .ynow_us_ec_chr(
      if (length(titles) >= pick) titles[[pick]] else res$form,
      paste(tk, "SEC 8-K / 6-K")
    ),
    url = .ynow_us_ec_chr(
      if (length(urls) >= pick) urls[[pick]] else res$primary_doc_url,
      ""
    ),
    published = .ynow_us_ec_chr(res$filing_date %||% res$report_date %||% ""),
    backlog = facts$backlog,
    bookings = facts$bookings,
    capex = facts$capex,
    themes = facts$themes,
    suppliers = facts$suppliers %||% list(),
    customers = facts$customers %||% list(),
    order_lines = facts$order_lines %||% list(),
    excerpt = facts$excerpt
  )
}

.ynow_us_ec_empty <- function(ticker, reason = "undisclosed") {
  tk <- toupper(trimws(.ynow_us_ec_chr(ticker)))
  list(
    ok = FALSE,
    status = "empty",
    source_id = "empty",
    credibility = "management",
    title = "",
    url = "",
    published = "",
    backlog = NA_real_,
    bookings = NA_real_,
    capex = NA_real_,
    themes = character(0),
    suppliers = list(),
    customers = list(),
    order_lines = list(),
    excerpt = "",
    empty_reason = reason
  )
}

#' Fetch US earnings-call disclosure for BBLab order research.
#' @return list with facts fields (backlog/bookings/…) plus meta links.
ynow_us_earnings_call_fetch <- function(ticker) {
  tk <- toupper(trimws(.ynow_us_ec_chr(ticker)))
  tk <- sub("\\.(US)$", "", tk, ignore.case = TRUE)
  meta_links <- list(
    sa_url = ynow_us_earnings_call_sa_url(tk),
    sec_url = ynow_us_earnings_call_sec_search_url(tk),
    ir_url = ynow_us_earnings_call_ir_search_url(tk)
  )
  if (!nzchar(tk) || grepl("\\.(TW|TWO)$", ticker, ignore.case = TRUE)) {
    return(c(.ynow_us_ec_empty(tk, "tw_leave_as_is"), meta_links))
  }
  usable <- if (exists("ynow_ec_result_usable", mode = "function")) {
    ynow_ec_result_usable
  } else {
    function(res) is.list(res) && isTRUE(res$ok) && identical(res$status %||% "", "ok")
  }
  # 1) Licensed API when key present (env only — never committed).
  api_key <- .ynow_us_ec_api_key()
  if (nzchar(api_key)) {
    licensed <- tryCatch(.ynow_us_ec_from_fmp(tk, api_key), error = function(e) NULL)
    if (isTRUE(usable(licensed))) {
      return(c(licensed, meta_links))
    }
  }
  # 2) SEC 8-K / 6-K primary body (Item 2.02 when titled).
  sec <- tryCatch(.ynow_us_ec_from_sec_8k(tk), error = function(e) NULL)
  if (isTRUE(usable(sec))) {
    return(c(sec, meta_links))
  }
  # 3) Seeking Alpha HTML — last-resort only (third-party; not official IR).
  if (exists("ynow_sa_earnings_call_fetch_last_resort", mode = "function")) {
    sa <- tryCatch(
      ynow_sa_earnings_call_fetch_last_resort(tk, extract_fun = .ynow_us_ec_extract_facts),
      error = function(e) NULL
    )
    if (isTRUE(usable(sa))) {
      return(c(sa, meta_links))
    }
  }
  # 4) Honest empty + outbound IR / SEC / SA links for humans.
  c(.ynow_us_ec_empty(tk, "no_transcript_or_8k_or_sa"), meta_links)
}

#' Shape for payload$order_disclosures$earnings_call → 業務拆解 order research.
ynow_us_earnings_call_to_disclosure <- function(fetched) {
  if (!is.list(fetched)) return(list())
  themes <- fetched$themes %||% character(0)
  if (!is.character(themes)) themes <- as.character(unlist(themes))
  out <- list(
    backlog = .ynow_us_ec_num(fetched$backlog),
    bookings = .ynow_us_ec_num(fetched$bookings),
    capex = .ynow_us_ec_num(fetched$capex),
    themes = themes,
    suppliers = fetched$suppliers %||% list(),
    customers = fetched$customers %||% list(),
    order_lines = fetched$order_lines %||% list(),
    excerpt = .ynow_us_ec_chr(fetched$excerpt),
    title = .ynow_us_ec_chr(fetched$title),
    url = .ynow_us_ec_chr(fetched$url),
    published = .ynow_us_ec_chr(fetched$published),
    source_id = .ynow_us_ec_chr(fetched$source_id, "empty"),
    credibility = .ynow_us_ec_chr(fetched$credibility, "management"),
    status = .ynow_us_ec_chr(fetched$status, "empty"),
    sa_url = .ynow_us_ec_chr(fetched$sa_url),
    sec_url = .ynow_us_ec_chr(fetched$sec_url),
    ir_url = .ynow_us_ec_chr(fetched$ir_url)
  )
  out
}
