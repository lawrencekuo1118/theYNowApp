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

#' Pull numbers / theme flags only when the transcript/filing text states them.
#' Theme flags follow Statementdog reading axes (status-only; no invented amounts).
.ynow_us_ec_extract_facts <- function(text) {
  txt <- as.character(text %||% "")[1]
  out <- list(
    backlog = NA_real_,
    bookings = NA_real_,
    capex = NA_real_,
    excerpt = "",
    themes = character(0)
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
      n <- suppressWarnings(as.numeric(sub("^([0-9.]+).*", "\\1", piece, perl = TRUE)))
      unit <- tolower(sub(".*?\\s*(billion|million|bn|mm|m)\\b.*", "\\1", piece, perl = TRUE, ignore.case = TRUE))
      if (!is.finite(n)) next
      mult <- if (unit %in% c("billion", "bn")) 1e9 else 1e6
      return(n * mult)
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
  themes <- character(0)
  if (grepl("(?i)\\b(guidance|outlook|expect(?:s|ed|ing)?\\s+(?:revenue|EPS|margin)|next\\s+quarter)", txt, perl = TRUE)) {
    themes <- c(themes, "cfo_guidance")
  }
  if (grepl("(?i)\\b(capital expenditures?|\\bCapEx\\b|capex|capacity\\s+investment|fab\\s+build)", txt, perl = TRUE)) {
    themes <- c(themes, "capex_plan")
  }
  if (grepl("(?i)\\b(question[- ]and[- ]answer|\\bQ\\s*&\\s*A\\b|analyst\\s+question)", txt, perl = TRUE)) {
    themes <- c(themes, "analyst_qa")
  }
  if (grepl("(?i)\\b(long[- ]term|multi[- ]year|secular|competitive\\s+advantage)\\b", txt, perl = TRUE)) {
    themes <- c(themes, "ceo_outlook")
  }
  out$themes <- unique(themes)
  out
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
                 is.finite(facts$capex) || nzchar(facts$excerpt)) {
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
  # Prefer Item 2.02 when present in note titles; else use primary body text.
  titles <- as.character(res$title %||% res$note_title %||% character(0))
  texts <- as.character(res$text %||% res$note_text %||% character(0))
  urls <- as.character(res$url %||% res$note_url %||% character(0))
  if (!length(texts) && nzchar(.ynow_us_ec_chr(res$primary_text %||% ""))) {
    texts <- .ynow_us_ec_chr(res$primary_text)
    titles <- .ynow_us_ec_chr(res$form %||% "8-K")
    urls <- .ynow_us_ec_chr(res$primary_doc_url %||% "")
  }
  # Parallel-list shape from Python.
  if (is.list(res$items) && length(res$items)) {
    # ignore
  }
  pick <- 1L
  if (length(titles)) {
    hit <- which(grepl("2\\.02|Results of Operations|earnings", titles, ignore.case = TRUE))
    if (length(hit)) pick <- hit[[1]]
  }
  text <- if (length(texts) >= pick) texts[[pick]] else paste(texts, collapse = "\n")
  if (!nzchar(text) && length(res$text)) {
    text <- paste(as.character(res$text), collapse = "\n")
  }
  # Flatten common reticulate list-of-columns shape.
  if (!nzchar(text) && is.list(res) && !is.null(res$note_text)) {
    text <- paste(as.character(unlist(res$note_text)), collapse = "\n")
  }
  if (!nzchar(text)) {
    # Try named columns from parallel lists
    for (nm in c("text", "body", "content")) {
      if (!is.null(res[[nm]])) {
        text <- paste(as.character(unlist(res[[nm]])), collapse = "\n")
        if (nzchar(text)) break
      }
    }
  }
  facts <- .ynow_us_ec_extract_facts(text)
  list(
    ok = TRUE,
    status = if (is.finite(facts$backlog) || is.finite(facts$bookings) ||
                 is.finite(facts$capex) || nzchar(facts$excerpt)) {
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

#' Shape for payload$order_disclosures$earnings_call (numeric facts only).
ynow_us_earnings_call_to_disclosure <- function(fetched) {
  if (!is.list(fetched)) return(list())
  themes <- fetched$themes %||% character(0)
  if (!is.character(themes)) themes <- as.character(unlist(themes))
  out <- list(
    backlog = .ynow_us_ec_num(fetched$backlog),
    bookings = .ynow_us_ec_num(fetched$bookings),
    capex = .ynow_us_ec_num(fetched$capex),
    themes = themes,
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
