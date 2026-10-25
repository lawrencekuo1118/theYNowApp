# ==========================================
# TW earnings-call / 法說會結論 ingest (product path)
# ==========================================
# Preferred automatable:
#   1) FinmoConf company page → latest MOPS STR presentation PDF (zh then en)
#      (Estimate; FinmoConf robots Allow / / Disallow /api/; no /api/ calls)
#   2) Fugle 法說會備忘錄 HTML (Estimate; robots Allow /)
# AlphaMemo free-transcripts index:
#   https://www.alphamemo.ai/free-transcripts
# AlphaMemo robots.txt disallows /api/; Terms forbid bots/crawlers without
# written permission → outbound index link only (no AlphaMemo HTML/API scrape).
# Last-resort (user override): Seeking Alpha HTML scrape when preferred paths
# cannot supply automatable facts. Credibility = last_resort (third-party HTML).
# Fail closed with honest empty if FinmoConf + Fugle + SA fail. Never invent amounts.

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
    order_notes = character(0),
    supply_chain_notes = character(0),
    outlook_notes = character(0),
    empty_reason = reason
  )
}

.ynow_tw_ec_chr_vec <- function(x) {
  if (is.null(x)) return(character(0))
  v <- as.character(unlist(x))
  v <- trimws(v)
  v[nzchar(v) & !is.na(v)]
}

#' Merge two usable TW EC results: prefer first non-NA amounts; union notes/parties.
.ynow_tw_ec_merge <- function(primary, secondary) {
  if (!is.list(primary)) return(secondary)
  if (!is.list(secondary)) return(primary)
  pick_num <- function(a, b) {
    if (is.finite(.ynow_tw_ec_num(a))) .ynow_tw_ec_num(a) else .ynow_tw_ec_num(b)
  }
  themes <- unique(c(
    as.character(primary$themes %||% character(0)),
    as.character(secondary$themes %||% character(0))
  ))
  themes <- themes[nzchar(themes)]
  list(
    ok = TRUE,
    status = "ok",
    source_id = .ynow_tw_ec_chr(primary$source_id, .ynow_tw_ec_chr(secondary$source_id)),
    credibility = .ynow_tw_ec_chr(primary$credibility, "estimate"),
    title = {
      t1 <- .ynow_tw_ec_chr(primary$title)
      if (nzchar(t1)) t1 else .ynow_tw_ec_chr(secondary$title)
    },
    url = {
      u1 <- .ynow_tw_ec_chr(primary$url)
      if (nzchar(u1)) u1 else .ynow_tw_ec_chr(secondary$url)
    },
    published = {
      p1 <- .ynow_tw_ec_chr(primary$published)
      if (nzchar(p1)) p1 else .ynow_tw_ec_chr(secondary$published)
    },
    lang = .ynow_tw_ec_chr(primary$lang, .ynow_tw_ec_chr(secondary$lang)),
    backlog = pick_num(primary$backlog, secondary$backlog),
    bookings = pick_num(primary$bookings, secondary$bookings),
    capex = pick_num(primary$capex, secondary$capex),
    themes = themes,
    suppliers = c(primary$suppliers %||% list(), secondary$suppliers %||% list()),
    customers = c(primary$customers %||% list(), secondary$customers %||% list()),
    order_lines = c(primary$order_lines %||% list(), secondary$order_lines %||% list()),
    order_notes = unique(c(
      .ynow_tw_ec_chr_vec(primary$order_notes),
      .ynow_tw_ec_chr_vec(secondary$order_notes)
    )),
    supply_chain_notes = unique(c(
      .ynow_tw_ec_chr_vec(primary$supply_chain_notes),
      .ynow_tw_ec_chr_vec(secondary$supply_chain_notes)
    )),
    outlook_notes = unique(c(
      .ynow_tw_ec_chr_vec(primary$outlook_notes),
      .ynow_tw_ec_chr_vec(secondary$outlook_notes)
    )),
    excerpt = {
      e1 <- .ynow_tw_ec_chr(primary$excerpt)
      if (nzchar(e1)) e1 else .ynow_tw_ec_chr(secondary$excerpt)
    },
    finmoconf_url = .ynow_tw_ec_chr(primary$finmoconf_url, .ynow_tw_ec_chr(secondary$finmoconf_url)),
    finmoconf_zh_url = .ynow_tw_ec_chr(primary$finmoconf_zh_url, .ynow_tw_ec_chr(secondary$finmoconf_zh_url)),
    finmoconf_en_url = .ynow_tw_ec_chr(primary$finmoconf_en_url, .ynow_tw_ec_chr(secondary$finmoconf_en_url)),
    fugle_url = .ynow_tw_ec_chr(primary$fugle_url, .ynow_tw_ec_chr(secondary$fugle_url)),
    sa_url = .ynow_tw_ec_chr(primary$sa_url, .ynow_tw_ec_chr(secondary$sa_url)),
    pdf_bytes = {
      b1 <- suppressWarnings(as.integer(primary$pdf_bytes %||% NA_integer_)[1])
      if (is.finite(b1) && b1 > 0L) b1 else suppressWarnings(as.integer(secondary$pdf_bytes %||% NA_integer_)[1])
    },
    # Keyword framework: prefer primary (FinmoConf deck) when present.
    keyword_analysis = primary$keyword_analysis %||% secondary$keyword_analysis,
    keyword_tone_score = {
      v1 <- suppressWarnings(as.integer(primary$keyword_tone_score %||% NA_integer_)[1])
      if (is.finite(v1)) v1 else suppressWarnings(as.integer(secondary$keyword_tone_score %||% NA_integer_)[1])
    },
    keyword_guidance_update = {
      g1 <- .ynow_tw_ec_chr(primary$keyword_guidance_update)
      if (nzchar(g1)) g1 else .ynow_tw_ec_chr(secondary$keyword_guidance_update)
    },
    keyword_top_positives = unique(c(
      .ynow_tw_ec_chr_vec(primary$keyword_top_positives),
      .ynow_tw_ec_chr_vec(secondary$keyword_top_positives)
    )),
    keyword_top_risks = unique(c(
      .ynow_tw_ec_chr_vec(primary$keyword_top_risks),
      .ynow_tw_ec_chr_vec(secondary$keyword_top_risks)
    )),
    keyword_trap_flags = unique(c(
      .ynow_tw_ec_chr_vec(primary$keyword_trap_flags),
      .ynow_tw_ec_chr_vec(secondary$keyword_trap_flags)
    )),
    keyword_bridging_notes = unique(c(
      .ynow_tw_ec_chr_vec(primary$keyword_bridging_notes),
      .ynow_tw_ec_chr_vec(secondary$keyword_bridging_notes)
    )),
    keyword_capex_notes = unique(c(
      .ynow_tw_ec_chr_vec(primary$keyword_capex_notes),
      .ynow_tw_ec_chr_vec(secondary$keyword_capex_notes)
    ))
  )
}

#' Fetch TW earnings-call disclosure for BBLab order research.
#' FinmoConf presentation PDF (Estimate) ∪ Fugle memo → SA HTML last-resort → AlphaMemo outbound empty.
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
    finmoconf_url = if (exists("ynow_finmoconf_company_url", mode = "function")) {
      ynow_finmoconf_company_url(bare)
    } else {
      "https://finmoconf.diveinvest.net/"
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
    function(res) {
      is.list(res) && isTRUE(res$ok) && identical(res$status %||% "", "ok")
    }
  }
  extract_fun <- if (exists(".ynow_us_ec_extract_facts", mode = "function")) {
    .ynow_us_ec_extract_facts
  } else {
    NULL
  }

  # Preferred paths may both contribute (PDF deck + Fugle memo). Merge when both usable.
  preferred <- list()
  if (exists("ynow_finmoconf_earnings_call_fetch", mode = "function")) {
    got_f <- tryCatch(
      ynow_finmoconf_earnings_call_fetch(bare, extract_fun = extract_fun),
      error = function(e) NULL
    )
    if (isTRUE(usable(got_f))) preferred[[length(preferred) + 1L]] <- got_f
  }
  if (exists("ynow_fugle_earnings_call_fetch", mode = "function")) {
    got_g <- tryCatch(
      ynow_fugle_earnings_call_fetch(bare, extract_fun = extract_fun),
      error = function(e) NULL
    )
    if (isTRUE(usable(got_g))) preferred[[length(preferred) + 1L]] <- got_g
  }
  if (length(preferred) == 1L) {
    return(c(preferred[[1]], meta_links))
  }
  if (length(preferred) >= 2L) {
    # FinmoConf first in list → primary source_id when it contributed.
    merged <- preferred[[1]]
    for (i in seq_along(preferred)[-1]) {
      merged <- .ynow_tw_ec_merge(merged, preferred[[i]])
    }
    return(c(merged, meta_links))
  }

  # Last-resort SA only when preferred paths empty.
  if (exists("ynow_sa_earnings_call_fetch_last_resort", mode = "function")) {
    got_sa <- tryCatch(
      ynow_sa_earnings_call_fetch_last_resort(bare, extract_fun = extract_fun),
      error = function(e) NULL
    )
    if (isTRUE(usable(got_sa))) return(c(got_sa, meta_links))
  }
  c(.ynow_tw_ec_empty(bare, "finmoconf_fugle_empty_alphamemo_human_only_sa_empty"), meta_links)
}

#' Shape for payload$order_disclosures$earnings_call (numeric facts only when stated).
ynow_tw_earnings_call_to_disclosure <- function(fetched) {
  if (!is.list(fetched)) return(list())
  themes <- fetched$themes %||% character(0)
  if (!is.character(themes)) themes <- as.character(unlist(themes))
  out <- list(
    backlog = .ynow_tw_ec_num(fetched$backlog),
    bookings = .ynow_tw_ec_num(fetched$bookings),
    capex = .ynow_tw_ec_num(fetched$capex),
    themes = themes,
    suppliers = fetched$suppliers %||% list(),
    customers = fetched$customers %||% list(),
    order_lines = fetched$order_lines %||% list(),
    order_notes = .ynow_tw_ec_chr_vec(fetched$order_notes),
    supply_chain_notes = .ynow_tw_ec_chr_vec(fetched$supply_chain_notes),
    outlook_notes = .ynow_tw_ec_chr_vec(fetched$outlook_notes),
    excerpt = .ynow_tw_ec_chr(fetched$excerpt),
    title = .ynow_tw_ec_chr(fetched$title),
    url = .ynow_tw_ec_chr(fetched$url),
    published = .ynow_tw_ec_chr(fetched$published),
    lang = .ynow_tw_ec_chr(fetched$lang),
    source_id = .ynow_tw_ec_chr(fetched$source_id, "alphamemo_index"),
    credibility = .ynow_tw_ec_chr(fetched$credibility, "estimate"),
    status = .ynow_tw_ec_chr(fetched$status, "empty"),
    alphamemo_url = .ynow_tw_ec_chr(fetched$alphamemo_url),
    alphamemo_search_url = .ynow_tw_ec_chr(fetched$alphamemo_search_url),
    fugle_url = .ynow_tw_ec_chr(fetched$fugle_url),
    finmoconf_url = .ynow_tw_ec_chr(fetched$finmoconf_url),
    finmoconf_zh_url = .ynow_tw_ec_chr(fetched$finmoconf_zh_url),
    finmoconf_en_url = .ynow_tw_ec_chr(fetched$finmoconf_en_url),
    sa_url = .ynow_tw_ec_chr(fetched$sa_url),
    empty_reason = .ynow_tw_ec_chr(fetched$empty_reason, "human_browsable_only"),
    keyword_tone_score = {
      v <- suppressWarnings(as.integer(fetched$keyword_tone_score %||% NA_integer_)[1])
      if (is.finite(v)) v else NA_integer_
    },
    keyword_guidance_update = .ynow_tw_ec_chr(fetched$keyword_guidance_update),
    keyword_top_positives = .ynow_tw_ec_chr_vec(fetched$keyword_top_positives),
    keyword_top_risks = .ynow_tw_ec_chr_vec(fetched$keyword_top_risks),
    keyword_trap_flags = .ynow_tw_ec_chr_vec(fetched$keyword_trap_flags),
    keyword_bridging_notes = .ynow_tw_ec_chr_vec(fetched$keyword_bridging_notes),
    keyword_capex_notes = .ynow_tw_ec_chr_vec(fetched$keyword_capex_notes)
  )
  out
}
