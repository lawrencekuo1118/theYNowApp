# ==========================================
# FinmoConf 台股法說會簡報 PDF (Estimate)
# ==========================================
# Discovery: https://finmoconf.diveinvest.net/company/{ticker}
# robots.txt: Allow / ; Disallow /api/ and /admin/ → never call /api/.
# PDF bytes: official TWSE STR files via doc.twse.com.tw (same family as
# tw_annual_report_orders). Prefer zh (M*) then en (E*). Latest card by date.
# Credibility: estimate (PDF text extract limits). Status-only facts; never invent.
# Env:
#   YNOW_FINMOCONF_SCRAPE=0|1 (default 1)
#   YNOW_FINMOCONF_PDF=0|1 (default 1; 0 = discovery links only)
#   YNOW_FINMOCONF_SCRAPE_FIXTURE_HTML / YNOW_FINMOCONF_PDF_FIXTURE_TEXT
#   YNOW_FINMOCONF_SCRAPE_MIN_INTERVAL_SEC (default 1.5)

.ynow_finmo_ec_chr <- function(x, default = "") {
  v <- as.character(x %||% default)[1]
  if (!nzchar(v) || is.na(v)) default else v
}

.ynow_finmo_ec_env <- new.env(parent = emptyenv())
.ynow_finmo_ec_env$last_fetch_at <- 0
.ynow_finmo_ec_env$py_ready <- FALSE

.ynow_finmo_ec_min_interval_sec <- function() {
  v <- suppressWarnings(as.numeric(Sys.getenv("YNOW_FINMOCONF_SCRAPE_MIN_INTERVAL_SEC", unset = "1.5")))
  if (!is.finite(v) || v < 0.5) 1.5 else min(v, 30)
}

.ynow_finmo_ec_enabled <- function() {
  flag <- tolower(trimws(Sys.getenv("YNOW_FINMOCONF_SCRAPE", unset = "1")))
  !flag %in% c("0", "false", "no", "off")
}

.ynow_finmo_ec_pdf_enabled <- function() {
  flag <- tolower(trimws(Sys.getenv("YNOW_FINMOCONF_PDF", unset = "1")))
  !flag %in% c("0", "false", "no", "off")
}

ynow_finmoconf_company_url <- function(ticker) {
  bare <- gsub("[^0-9A-Za-z]", "", sub("\\.(TW|TWO)$", "", toupper(trimws(.ynow_finmo_ec_chr(ticker))), ignore.case = TRUE))
  if (!nzchar(bare)) return("https://finmoconf.diveinvest.net/")
  paste0("https://finmoconf.diveinvest.net/company/", bare)
}

ynow_finmoconf_index_url <- function() {
  "https://finmoconf.diveinvest.net/"
}

.ynow_finmo_ec_rate_limit <- function() {
  elapsed <- as.numeric(Sys.time()) - (.ynow_finmo_ec_env$last_fetch_at %||% 0)
  wait <- .ynow_finmo_ec_min_interval_sec() - elapsed
  if (is.finite(wait) && wait > 0) Sys.sleep(wait)
  .ynow_finmo_ec_env$last_fetch_at <- as.numeric(Sys.time())
}

.ynow_finmo_ec_ensure_py <- function() {
  if (isTRUE(.ynow_finmo_ec_env$py_ready) &&
        exists("fetch_finmoconf_latest_presentation", envir = .ynow_finmo_ec_env, inherits = FALSE)) {
    return(TRUE)
  }
  if (identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1") &&
        !nzchar(Sys.getenv("YNOW_FINMOCONF_PDF_FIXTURE_TEXT", unset = "")) &&
        !nzchar(Sys.getenv("YNOW_FINMOCONF_SCRAPE_FIXTURE_HTML", unset = ""))) {
    # Fixture-only unit tests may still source Python for pure parsers.
  }
  ok <- FALSE
  tryCatch({
    if (exists(".ynow_ensure_python", mode = "function")) {
      tryCatch(.ynow_ensure_python(), error = function(e) FALSE)
    }
    if (exists(".ensure_python_scraper", mode = "function")) {
      tryCatch(.ensure_python_scraper(), error = function(e) FALSE)
    }
    py_file <- "finmoconf_earnings_pdf.py"
    if (!file.exists(py_file)) return(FALSE)
    if (!requireNamespace("reticulate", quietly = TRUE)) return(FALSE)
    if (!isTRUE(reticulate::py_available(initialize = FALSE))) {
      suppressMessages(reticulate::py_config())
    }
    reticulate::source_python(py_file, envir = .ynow_finmo_ec_env)
    .ynow_finmo_ec_env$py_ready <- TRUE
    ok <- TRUE
  }, error = function(e) {
    if (exists(".ynow_log", mode = "function")) {
      .ynow_log("⚠️ FinmoConf earnings PDF module load failed: ", e$message)
    }
    ok <<- FALSE
  })
  isTRUE(ok) && exists("fetch_finmoconf_latest_presentation", envir = .ynow_finmo_ec_env, inherits = FALSE)
}

#' Status-only sentence harvest for 訂單／供應鏈／今後發展 (no invented amounts).
.ynow_finmo_ec_status_notes <- function(text, max_n = 6L) {
  txt <- as.character(text %||% "")[1]
  empty <- list(
    order_notes = character(0),
    supply_chain_notes = character(0),
    outlook_notes = character(0)
  )
  if (!nzchar(trimws(txt))) return(empty)
  # Split on Chinese / English sentence boundaries and slide breaks.
  parts <- unlist(strsplit(txt, "[。！？\n\r]+", perl = TRUE))
  parts <- trimws(gsub("\\s+", " ", parts))
  parts <- parts[nzchar(parts) & nchar(parts) >= 8L & nchar(parts) <= 280L]
  if (!length(parts)) return(empty)

  pick <- function(patterns) {
    hit <- parts[vapply(parts, function(s) {
      any(vapply(patterns, function(p) grepl(p, s, perl = TRUE, ignore.case = TRUE), logical(1)))
    }, logical(1))]
    unique(utils::head(hit, max_n))
  }
  list(
    order_notes = pick(c(
      "在手訂單", "新訂單", "接單", "訂單能見度", "訂單展望", "bookings?", "\\bbacklog\\b",
      "book[- ]to[- ]bill", "可見度"
    )),
    supply_chain_notes = pick(c(
      "供應鏈", "供應商", "供貨", "缺料", "長約", "產能", "採購", "supplier", "supply[- ]chain"
    )),
    outlook_notes = pick(c(
      "今後", "展望", "前瞻", "指引", "明年", "下半年", "下季", "outlook", "guidance",
      "產業趨勢", "未來"
    ))
  )
}

.ynow_finmo_ec_cache_get <- function(key) {
  if (!exists(".ynow_slow_cache", inherits = TRUE)) return(NULL)
  tryCatch(.ynow_slow_cache$get(key), error = function(e) NULL)
}

.ynow_finmo_ec_cache_set <- function(key, value) {
  if (!exists(".ynow_slow_cache", inherits = TRUE)) return(invisible(FALSE))
  tryCatch({
    .ynow_slow_cache$set(key, value)
    TRUE
  }, error = function(e) FALSE)
}

#' Offline HTML STR-link parse (fixtures / no Python). Newest ymd; zh (M) preferred.
.ynow_finmo_ec_parse_html_offline <- function(html, bare) {
  html <- gsub("\\\\/", "/", as.character(html %||% "")[1])
  bare <- toupper(.ynow_finmo_ec_chr(bare))
  if (!nzchar(html) || !nzchar(bare)) return(NULL)
  pat <- paste0(
    "https?://(?:mopsov\\.twse\\.com\\.tw|doc\\.twse\\.com\\.tw)/nas/STR/",
    bare,
    "(20[0-9]{6})([ME])([0-9]{3})\\.pdf"
  )
  m <- gregexpr(pat, html, perl = TRUE, ignore.case = TRUE)
  if (m[[1]][1] < 0) return(NULL)
  hits <- unique(regmatches(html, m)[[1]])
  if (!length(hits)) return(NULL)
  parsed <- lapply(hits, function(u) {
    mm <- regexec(pat, u, perl = TRUE, ignore.case = TRUE)
    parts <- regmatches(u, mm)[[1]]
    if (length(parts) < 4L) return(NULL)
    list(
      ymd = parts[[2]],
      lang = if (toupper(parts[[3]]) == "M") "zh" else "en",
      url = sub("https?://mopsov\\.twse\\.com\\.tw", "https://doc.twse.com.tw", u, ignore.case = TRUE)
    )
  })
  parsed <- Filter(Negate(is.null), parsed)
  if (!length(parsed)) return(NULL)
  ymds <- vapply(parsed, function(x) x$ymd, character(1))
  latest <- max(ymds)
  bucket <- parsed[ymds == latest]
  langs <- vapply(bucket, function(x) x$lang, character(1))
  pick <- bucket[[match("zh", langs, nomatch = match("en", langs))]]
  zh_url <- ""
  en_url <- ""
  if ("zh" %in% langs) zh_url <- bucket[[match("zh", langs)]]$url
  if ("en" %in% langs) en_url <- bucket[[match("en", langs)]]$url
  list(
    ok = TRUE,
    status = "ok",
    title = paste(bare, "法說會簡報 (", substr(latest, 1, 4), "-", substr(latest, 5, 6), "-", substr(latest, 7, 8), ", ", pick$lang, ")", sep = ""),
    url = pick$url,
    published = paste0(substr(latest, 1, 4), "-", substr(latest, 5, 6), "-", substr(latest, 7, 8)),
    lang = pick$lang,
    zh_url = zh_url,
    en_url = en_url,
    text = "",
    pdf_bytes = 0L
  )
}

#' Fetch latest FinmoConf-indexed 法說會 presentation → facts via extract_fun.
ynow_finmoconf_earnings_call_fetch <- function(ticker, extract_fun = NULL) {
  if (!isTRUE(.ynow_finmo_ec_enabled())) return(NULL)
  tk <- toupper(trimws(.ynow_finmo_ec_chr(ticker)))
  bare <- sub("\\.(TW|TWO)$", "", tk, ignore.case = TRUE)
  bare <- gsub("[^0-9A-Za-z]", "", bare)
  if (!nzchar(bare) || !grepl("^[0-9]{4,6}[A-Z]?$", bare)) return(NULL)

  company_url <- ynow_finmoconf_company_url(bare)
  cache_key <- paste0("finmoconf_ec_v1_", bare)
  cached <- .ynow_finmo_ec_cache_get(cache_key)
  # Fixtures bypass cache so unit tests stay deterministic.
  html_fix <- Sys.getenv("YNOW_FINMOCONF_SCRAPE_FIXTURE_HTML", unset = "")
  text_fix <- Sys.getenv("YNOW_FINMOCONF_PDF_FIXTURE_TEXT", unset = "")
  use_cache <- !nzchar(html_fix) && !nzchar(text_fix)
  if (isTRUE(use_cache) && is.list(cached) && isTRUE(cached$ok)) {
    return(cached)
  }

  html_body <- ""
  if (nzchar(html_fix) && file.exists(html_fix)) {
    html_body <- paste(readLines(html_fix, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  text_body <- ""
  if (nzchar(text_fix) && file.exists(text_fix)) {
    text_body <- paste(readLines(text_fix, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }

  raw <- NULL
  # Fully offline fixture path (no reticulate / no network).
  if (nzchar(html_body) && nzchar(text_body)) {
    raw <- .ynow_finmo_ec_parse_html_offline(html_body, bare)
    if (is.list(raw)) {
      raw$text <- text_body
      raw$ok <- TRUE
      raw$status <- "ok"
    }
  } else {
    if (!isTRUE(.ynow_finmo_ec_ensure_py())) return(NULL)
    if (!nzchar(html_body) && !nzchar(text_body)) {
      .ynow_finmo_ec_rate_limit()
    }
    skip_pdf <- !isTRUE(.ynow_finmo_ec_pdf_enabled()) && !nzchar(text_body)
    raw <- tryCatch(
      .ynow_finmo_ec_env$fetch_finmoconf_latest_presentation(
        bare,
        prefer_lang = "zh",
        html_fixture = html_body,
        text_fixture = text_body,
        skip_pdf = isTRUE(skip_pdf)
      ),
      error = function(e) NULL
    )
  }
  if (!is.list(raw) || !isTRUE(raw$ok)) return(NULL)

  text <- .ynow_finmo_ec_chr(raw$text)
  if (!nzchar(trimws(text))) return(NULL)

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

  notes <- .ynow_finmo_ec_status_notes(text)
  themes <- facts$themes %||% character(0)
  if (!is.character(themes)) themes <- as.character(unlist(themes))
  if (length(notes$order_notes)) themes <- c(themes, "order_status")
  if (length(notes$supply_chain_notes)) themes <- c(themes, "supply_chain_status")
  if (length(notes$outlook_notes)) themes <- c(themes, "outlook_status", "ceo_outlook")
  themes <- unique(themes)

  excerpt <- .ynow_finmo_ec_chr(facts$excerpt)
  if (!nzchar(excerpt)) {
    # Prefer outlook / supply / order status as the UI excerpt when present.
    prefer <- c(notes$order_notes, notes$supply_chain_notes, notes$outlook_notes)
    if (length(prefer)) {
      excerpt <- substr(paste(utils::head(prefer, 3L), collapse = "；"), 1L, 420L)
    } else {
      excerpt <- substr(gsub("\\s+", " ", text), 1L, 420L)
    }
  }

  usable <- is.finite(facts$backlog %||% NA_real_) ||
    is.finite(facts$bookings %||% NA_real_) ||
    is.finite(facts$capex %||% NA_real_) ||
    length(facts$suppliers %||% list()) > 0L ||
    length(facts$customers %||% list()) > 0L ||
    length(facts$order_lines %||% list()) > 0L ||
    length(notes$order_notes) > 0L ||
    length(notes$supply_chain_notes) > 0L ||
    length(notes$outlook_notes) > 0L ||
    nzchar(excerpt)
  if (!isTRUE(usable)) return(NULL)

  out <- list(
    ok = TRUE,
    status = "ok",
    source_id = "finmoconf_presentation_pdf",
    credibility = "estimate",
    title = .ynow_finmo_ec_chr(raw$title, paste(bare, "FinmoConf 法說會簡報")),
    url = .ynow_finmo_ec_chr(raw$url),
    published = .ynow_finmo_ec_chr(raw$published),
    lang = .ynow_finmo_ec_chr(raw$lang),
    backlog = facts$backlog %||% NA_real_,
    bookings = facts$bookings %||% NA_real_,
    capex = facts$capex %||% NA_real_,
    themes = themes,
    suppliers = facts$suppliers %||% list(),
    customers = facts$customers %||% list(),
    order_lines = facts$order_lines %||% list(),
    order_notes = notes$order_notes,
    supply_chain_notes = notes$supply_chain_notes,
    outlook_notes = notes$outlook_notes,
    excerpt = excerpt,
    finmoconf_url = company_url,
    finmoconf_zh_url = .ynow_finmo_ec_chr(raw$zh_url),
    finmoconf_en_url = .ynow_finmo_ec_chr(raw$en_url),
    pdf_bytes = suppressWarnings(as.integer(raw$pdf_bytes %||% 0L))[1]
  )
  if (isTRUE(use_cache)) .ynow_finmo_ec_cache_set(cache_key, out)
  out
}
