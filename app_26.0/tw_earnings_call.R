# ==========================================
# TW earnings-call / 法說會結論 ingest (product path)
# ==========================================
# Preferred automatable: Fugle 法說會備忘錄 HTML (free; Estimate; robots Allow /).
# AlphaMemo free-transcripts index:
#   https://www.alphamemo.ai/free-transcripts
# AlphaMemo robots.txt disallows /api/; Terms forbid bots/crawlers without
# written permission → outbound index link only (no AlphaMemo HTML/API scrape).
# Last-resort (user override): Seeking Alpha HTML scrape when preferred paths
# cannot supply automatable facts. Credibility = last_resort (third-party HTML).
# Fail closed with honest empty if Fugle + SA fail. Never invent amounts.

.ynow_tw_ec_chr <- function(x, default = "") {
  v <- as.character(x %||% default)[1]
  if (!nzchar(v) || is.na(v)) default else v
}

.ynow_tw_ec_num <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1L || !is.finite(v)) NA_real_ else v
}

#' Canonical AlphaMemo public free-transcripts index (human outbound).
ynow_tw_earnings_call_alphamemo_index_url <- function() {
  "https://www.alphamemo.ai/free-transcripts"
}

#' Optional Google query that helps a human find a ticker on AlphaMemo (no scrape).
ynow_tw_earnings_call_alphamemo_search_url <- function(ticker) {
  tk <- toupper(trimws(.ynow_tw_ec_chr(ticker)))
  tk <- sub("\\.(TW|TWO)$", "", tk, ignore.case = TRUE)
  if (!nzchar(tk)) return("")
  q <- paste("site:alphamemo.ai/free-transcripts", tk, "法說會")
  paste0("https://www.google.com/search?q=", utils::URLencode(q, reserved = TRUE))
}

.ynow_tw_ec_empty <- function(ticker, reason = "human_browsable_only") {
  list(
    ok = FALSE,
    status = "empty",
    source_id = "alphamemo_index",
    credibility = "estimate",
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

#' Fetch TW earnings-call disclosure for BBLab order research.
#' Fugle memo HTML (Estimate) → SA HTML last-resort → AlphaMemo outbound empty.
ynow_tw_earnings_call_fetch <- function(ticker) {
  tk <- toupper(trimws(.ynow_tw_ec_chr(ticker)))
  bare <- sub("\\.(TW|TWO)$", "", tk, ignore.case = TRUE)
  meta_links <- list(
    alphamemo_url = ynow_tw_earnings_call_alphamemo_index_url(),
    alphamemo_search_url = ynow_tw_earnings_call_alphamemo_search_url(bare),
    fugle_url = if (exists("ynow_fugle_earnings_call_topic_url", mode = "function")) {
      ynow_fugle_earnings_call_topic_url()
    } else {
      "https://blog.fugle.tw/topic/earnings-call-memo"
    },
    sa_url = if (exists("ynow_sa_earnings_call_index_url", mode = "function")) {
      ynow_sa_earnings_call_index_url(bare)
    } else {
      ""
    }
  )
  if (!nzchar(tk)) {
    return(c(.ynow_tw_ec_empty("", "blank_ticker"), meta_links))
  }
  # US tickers leave TW path alone.
  if (!grepl("\\.(TW|TWO)$", ticker, ignore.case = TRUE) &&
        !grepl("^[0-9]{4}$", bare)) {
    return(c(.ynow_tw_ec_empty(tk, "us_leave_as_is"), meta_links))
  }

  usable <- if (exists("ynow_ec_result_usable", mode = "function")) {
    ynow_ec_result_usable
  } else {
    function(res) is.list(res) && isTRUE(res$ok) && identical(res$status %||% "", "ok")
  }
  extract_fun <- if (exists(".ynow_us_ec_extract_facts", mode = "function")) {
    .ynow_us_ec_extract_facts
  } else {
    NULL
  }

  # Named list order is the waterfall. AlphaMemo is meta_links only.
  steps <- list(
    fugle_earnings_memo = function() {
      if (!exists("ynow_fugle_earnings_call_fetch", mode = "function")) return(NULL)
      tryCatch(
        ynow_fugle_earnings_call_fetch(bare, extract_fun = extract_fun),
        error = function(e) NULL
      )
    },
    seeking_alpha_html = function() {
      if (!exists("ynow_sa_earnings_call_fetch_last_resort", mode = "function")) return(NULL)
      tryCatch(
        ynow_sa_earnings_call_fetch_last_resort(bare, extract_fun = extract_fun),
        error = function(e) NULL
      )
    }
  )
  for (step in steps) {
    got <- step()
    if (isTRUE(usable(got))) return(c(got, meta_links))
  }
  c(.ynow_tw_ec_empty(bare, "fugle_empty_alphamemo_human_only_sa_empty"), meta_links)
}

#' Shape for payload$order_disclosures$earnings_call (numeric facts only when stated).
ynow_tw_earnings_call_to_disclosure <- function(fetched) {
  if (!is.list(fetched)) return(list())
  themes <- fetched$themes %||% character(0)
  if (!is.character(themes)) themes <- as.character(unlist(themes))
  list(
    backlog = .ynow_tw_ec_num(fetched$backlog),
    bookings = .ynow_tw_ec_num(fetched$bookings),
    capex = .ynow_tw_ec_num(fetched$capex),
    themes = themes,
    suppliers = fetched$suppliers %||% list(),
    customers = fetched$customers %||% list(),
    order_lines = fetched$order_lines %||% list(),
    excerpt = .ynow_tw_ec_chr(fetched$excerpt),
    title = .ynow_tw_ec_chr(fetched$title),
    url = .ynow_tw_ec_chr(fetched$url),
    published = .ynow_tw_ec_chr(fetched$published),
    source_id = .ynow_tw_ec_chr(fetched$source_id, "alphamemo_index"),
    credibility = .ynow_tw_ec_chr(fetched$credibility, "estimate"),
    status = .ynow_tw_ec_chr(fetched$status, "empty"),
    alphamemo_url = .ynow_tw_ec_chr(fetched$alphamemo_url),
    alphamemo_search_url = .ynow_tw_ec_chr(fetched$alphamemo_search_url),
    fugle_url = .ynow_tw_ec_chr(fetched$fugle_url),
    sa_url = .ynow_tw_ec_chr(fetched$sa_url),
    empty_reason = .ynow_tw_ec_chr(fetched$empty_reason, "human_browsable_only")
  )
}
