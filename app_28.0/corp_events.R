# ==========================================
# corp_events.R — 個股重要日期／董監改選／可轉債稀釋
#
# Public sources (no paid APIs):
#   Key dates (US + TW): Yahoo Finance via yfinance (calendar / earnings call)
#   董監改選 (TW): MOPS open CSV t187ap41_*（股東會＋是否改選）、t187ap34_*（候選人）
#   可轉債 (TW): 櫃買 OpenAPI bond_ISSBD5_data（國內轉／交換債）、bond_ISSBD6_data（海外轉換債）
#   US board / CB: no free open-data parity → SEC EDGAR links + soft diluted-share gap
# ==========================================

YNOW_MOPS_BOARD_MEET_L <- "https://mopsfin.twse.com.tw/opendata/t187ap41_L.csv"
YNOW_MOPS_BOARD_MEET_O <- "https://mopsfin.twse.com.tw/opendata/t187ap41_O.csv"
YNOW_MOPS_BOARD_ELECT_L <- "https://mopsfin.twse.com.tw/opendata/t187ap34_L.csv"
YNOW_MOPS_BOARD_ELECT_O <- "https://mopsfin.twse.com.tw/opendata/t187ap34_O.csv"
# TWSE OpenAPI JSON (often more SSL-reliable than mopsfin CSV from some hosts)
YNOW_TWSE_BOARD_MEET_L_JSON <- "https://openapi.twse.com.tw/v1/opendata/t187ap41_L"
YNOW_TWSE_BOARD_ELECT_L_JSON <- "https://openapi.twse.com.tw/v1/opendata/t187ap34_L"
YNOW_TPEX_BOARD_MEET_O_JSON <- "https://www.tpex.org.tw/openapi/v1/mopsfin_t187ap41_O"
YNOW_TPEX_BOARD_ELECT_O_JSON <- "https://www.tpex.org.tw/openapi/v1/mopsfin_t187ap34_O"
YNOW_TPEX_CB_DOMESTIC_URL <- "https://www.tpex.org.tw/openapi/v1/bond_ISSBD5_data"
YNOW_TPEX_CB_OVERSEAS_URL <- "https://www.tpex.org.tw/openapi/v1/bond_ISSBD6_data"

#' Strip Yahoo suffix → TW issuer / company code (keep leading zeros).
.tw_issuer_code <- function(ticker) {
  bare <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(bare)) return("")
  bare <- sub("\\.(TW|TWO)$", "", bare)
  gsub("[^0-9A-Z]", "", bare)
}

#' ROC packed date (e.g. 1150528) or YYYYMMDD → ISO YYYY-MM-DD.
.tw_packed_date_to_iso <- function(x) {
  s <- gsub("[^0-9]", "", as.character(x %||% "")[1])
  if (!nzchar(s)) return("")
  if (nchar(s) == 7L) {
    # ROC yyyMMdd (3-digit year)
    y <- suppressWarnings(as.integer(substr(s, 1L, 3L)))
    m <- substr(s, 4L, 5L)
    d <- substr(s, 6L, 7L)
    if (!is.finite(y)) return("")
    return(sprintf("%04d-%s-%s", y + 1911L, m, d))
  }
  if (nchar(s) == 8L) {
    y <- substr(s, 1L, 4L)
    m <- substr(s, 5L, 6L)
    d <- substr(s, 7L, 8L)
    return(sprintf("%s-%s-%s", y, m, d))
  }
  ""
}

.tw_http_get_text <- function(url, timeout_sec = 45) {
  url <- as.character(url %||% "")[1]
  if (!nzchar(url)) return(NULL)
  tmp <- tempfile(fileext = ".dat")
  on.exit(unlink(tmp), add = TRUE)
  opts <- options(timeout = max(as.integer(timeout_sec), getOption("timeout", 60L)))
  on.exit(options(opts), add = TRUE)
  ok <- FALSE
  for (method in c("libcurl", "curl", "auto")) {
    ok <- tryCatch({
      utils::download.file(url, destfile = tmp, quiet = TRUE, mode = "wb", method = method)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(ok) && file.exists(tmp) && isTRUE(file.info(tmp)$size >= 10)) break
    ok <- FALSE
  }
  # Shell curl fallback (cloud hosts sometimes break R SSL to mopsfin)
  if (!isTRUE(ok) && nzchar(Sys.which("curl"))) {
    ok <- tryCatch({
      status <- system2(
        "curl",
        c("-fsSL", "-A", "theYNowApp/corp-events", "--max-time", as.character(timeout_sec),
          "-o", tmp, url),
        stdout = FALSE, stderr = FALSE
      )
      identical(as.integer(status), 0L) && file.exists(tmp) && isTRUE(file.info(tmp)$size >= 10)
    }, error = function(e) FALSE)
  }
  if (!isTRUE(ok) || !file.exists(tmp) || isTRUE(file.info(tmp)$size < 10)) {
    return(NULL)
  }
  raw <- tryCatch(readBin(tmp, what = "raw", n = file.info(tmp)$size), error = function(e) raw(0))
  if (!length(raw)) return(NULL)
  # Prefer UTF-8; MOPS CSVs are usually UTF-8 with BOM. Fall back to CP950/BIG5.
  for (enc in c("UTF-8", "CP950", "BIG5")) {
    txt <- tryCatch(
      iconv(rawToChar(raw, multiple = FALSE), from = enc, to = "UTF-8"),
      error = function(e) NA_character_
    )
    if (is.character(txt) && length(txt) == 1L && !is.na(txt) && nchar(txt) > 0) {
      return(sub("^\ufeff", "", txt))
    }
  }
  tryCatch(sub("^\ufeff", "", rawToChar(raw, multiple = FALSE)), error = function(e) NULL)
}

.tw_read_mops_csv <- function(url) {
  txt <- .tw_http_get_text(url)
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  if (grepl("<html", txt, ignore.case = TRUE)) return(NULL)
  df <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || !nrow(df)) return(NULL)
  names(df) <- trimws(names(df))
  df
}

.tw_json_get <- function(url, timeout_sec = 45) {
  txt <- .tw_http_get_text(url, timeout_sec = timeout_sec)
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  if (grepl("^\\s*<", txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE), error = function(e) NULL)
}

# ---- Key dates (Yahoo / yfinance) ----------------------------------------

#' @return list(earnings_date, earnings_call, guidance_date, ex_dividend, source, is_estimate)
fetch_company_key_dates <- function(ticker) {
  empty <- list(
    earnings_date = "",
    earnings_call = "",
    guidance_date = "",
    guidance_soft = FALSE,
    ex_dividend = "",
    source = "Yahoo Finance",
    is_estimate = FALSE,
    shares_outstanding = NA_real_,
    shares_diluted = NA_real_
  )
  tk <- trimws(as.character(ticker %||% "")[1])
  if (!nzchar(tk)) return(empty)
  if (!isTRUE(.ensure_python_scraper()) ||
      !exists("get_company_key_dates", envir = .py_scraper_env, inherits = FALSE, mode = "function")) {
    return(empty)
  }
  res <- tryCatch(
    get("get_company_key_dates", envir = .py_scraper_env, inherits = FALSE)(tk),
    error = function(e) {
      .ynow_log("⚠️ key dates failed for ", tk, ": ", e$message)
      NULL
    }
  )
  if (is.null(res) || !is.list(res)) return(empty)
  .chr <- function(x) {
    s <- trimws(as.character(x %||% "")[1])
    if (!nzchar(s) || s %in% c("None", "null", "NA")) "" else s
  }
  .num <- function(x) {
    suppressWarnings(as.numeric(.chr(x)))
  }
  list(
    earnings_date = .chr(res$earnings_date),
    earnings_call = .chr(res$earnings_call),
    guidance_date = .chr(res$guidance_date),
    guidance_soft = isTRUE(res$guidance_soft) || identical(.chr(res$guidance_soft), "TRUE"),
    ex_dividend = .chr(res$ex_dividend),
    source = .chr(res$source %||% "Yahoo Finance"),
    is_estimate = isTRUE(res$is_estimate) || identical(.chr(res$is_estimate), "TRUE"),
    shares_outstanding = .num(res$shares_outstanding),
    shares_diluted = .num(res$shares_diluted)
  )
}

cached_fetch_company_key_dates <- memoise::memoise(
  fetch_company_key_dates,
  cache = if (exists(".ynow_slow_cache")) .ynow_slow_cache else cachem::cache_mem(max_age = 24 * 3600)
)

# ---- TW board election ---------------------------------------------------

.tw_df_filter_code <- function(df, code) {
  if (is.null(df) || !NROW(df)) return(NULL)
  if (!is.data.frame(df)) df <- tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !("公司代號" %in% names(df))) return(NULL)
  hit <- df[trimws(as.character(df[["公司代號"]])) == code, , drop = FALSE]
  if (!nrow(hit)) return(NULL)
  hit
}

.fetch_tw_board_meeting_row <- function(code) {
  code <- as.character(code %||% "")[1]
  if (!nzchar(code)) return(NULL)
  # Prefer OpenAPI JSON, then MOPS CSV
  for (url in c(
    YNOW_TWSE_BOARD_MEET_L_JSON,
    YNOW_TPEX_BOARD_MEET_O_JSON,
    YNOW_MOPS_BOARD_MEET_L,
    YNOW_MOPS_BOARD_MEET_O
  )) {
    df <- if (grepl("\\.csv$", url, ignore.case = TRUE)) {
      .tw_read_mops_csv(url)
    } else {
      raw <- .tw_json_get(url)
      if (is.null(raw)) NULL else tryCatch(as.data.frame(raw, stringsAsFactors = FALSE), error = function(e) NULL)
    }
    hit <- .tw_df_filter_code(df, code)
    if (!is.null(hit) && nrow(hit)) return(hit[1, , drop = FALSE])
  }
  NULL
}

.fetch_tw_board_election_rows <- function(code) {
  code <- as.character(code %||% "")[1]
  if (!nzchar(code)) return(NULL)
  # Prefer MOPS CSV first: TWSE OpenAPI t187ap34 JSON is often truncated mid-transfer.
  # Return the first board hit (L or O) — do not rbind across sources (column sets differ).
  for (url in c(
    YNOW_MOPS_BOARD_ELECT_L,
    YNOW_MOPS_BOARD_ELECT_O,
    YNOW_TWSE_BOARD_ELECT_L_JSON,
    YNOW_TPEX_BOARD_ELECT_O_JSON
  )) {
    df <- if (grepl("\\.csv$", url, ignore.case = TRUE)) {
      .tw_read_mops_csv(url)
    } else {
      raw <- .tw_json_get(url)
      if (is.null(raw)) NULL else tryCatch(as.data.frame(raw, stringsAsFactors = FALSE), error = function(e) NULL)
    }
    hit <- .tw_df_filter_code(df, code)
    if (!is.null(hit) && nrow(hit)) return(hit)
  }
  NULL
}

#' @return list with meeting / election summary for TW ticker
fetch_tw_board_election <- function(ticker) {
  code <- .tw_issuer_code(ticker)
  empty <- list(
    market = "TW",
    company_code = code,
    has_data = FALSE,
    meeting_type = "",
    meeting_date = "",
    electing = FALSE,
    elect_method = "",
    nomination = FALSE,
    nomination_window = "",
    n_director = 0L,
    n_independent = 0L,
    n_supervisor = 0L,
    candidates = data.frame(),
    source = "MOPS t187ap41 / t187ap34",
    source_urls = c(YNOW_MOPS_BOARD_MEET_L, YNOW_MOPS_BOARD_ELECT_L)
  )
  if (!nzchar(code)) return(empty)

  meet <- tryCatch(.fetch_tw_board_meeting_row(code), error = function(e) NULL)
  elect <- tryCatch(.fetch_tw_board_election_rows(code), error = function(e) NULL)

  if (is.null(meet) && (is.null(elect) || !nrow(elect))) return(empty)

  meeting_type <- if (!is.null(meet)) trimws(as.character(meet[["股東常(臨時)會"]][1] %||% "")) else ""
  meeting_date_raw <- if (!is.null(meet)) {
    trimws(as.character(meet[["開會日期"]][1] %||% ""))
  } else if (!is.null(elect) && nrow(elect)) {
    trimws(as.character(elect[["股東常(臨時)會日期-日期"]][1] %||% ""))
  } else {
    ""
  }
  electing_flag <- if (!is.null(meet)) {
    identical(trimws(as.character(meet[["是否改選董監"]][1] %||% "")), "是")
  } else if (!is.null(elect) && nrow(elect)) {
    any(trimws(as.character(elect[["是否選任董監事"]])) == "是", na.rm = TRUE)
  } else {
    FALSE
  }

  elect_method <- ""
  nomination <- FALSE
  nomination_window <- ""
  n_dir <- 0L
  n_ind <- 0L
  n_sup <- 0L
  cand_df <- data.frame()

  if (!is.null(elect) && nrow(elect)) {
    elect_yes <- elect[trimws(as.character(elect[["是否選任董監事"]])) == "是", , drop = FALSE]
    if (!nrow(elect_yes)) elect_yes <- elect
    elect_method <- trimws(as.character(elect_yes[["董監事選任方式"]][1] %||% ""))
    nomination <- identical(trimws(as.character(elect_yes[["是否採候選人提名制選任董監事"]][1] %||% "")), "是")
    nomination_window <- trimws(as.character(elect_yes[["採候選人提名制選任董監事相關公告-提名受理期間"]][1] %||% ""))
    roles <- trimws(as.character(elect_yes[["候選人類別"]]))
    names_v <- trimws(as.character(elect_yes[["候選人姓名"]]))
    won <- trimws(as.character(elect_yes[["是否當選"]]))
    keep <- nzchar(names_v)
    if (any(keep)) {
      cand_df <- data.frame(
        role = roles[keep],
        name = names_v[keep],
        elected = won[keep],
        stringsAsFactors = FALSE
      )
      n_dir <- sum(cand_df$role == "董事", na.rm = TRUE)
      n_ind <- sum(cand_df$role == "獨立董事", na.rm = TRUE)
      n_sup <- sum(cand_df$role %in% c("監察人", "監事"), na.rm = TRUE)
    }
    if (!nzchar(meeting_type)) {
      meeting_type <- trimws(as.character(elect_yes[["股東常(臨時)會日期-常或臨時"]][1] %||% ""))
    }
  }

  list(
    market = "TW",
    company_code = code,
    has_data = TRUE,
    meeting_type = meeting_type,
    meeting_date = .tw_packed_date_to_iso(meeting_date_raw),
    electing = isTRUE(electing_flag),
    elect_method = elect_method,
    nomination = isTRUE(nomination),
    nomination_window = nomination_window,
    n_director = as.integer(n_dir),
    n_independent = as.integer(n_ind),
    n_supervisor = as.integer(n_sup),
    candidates = cand_df,
    source = "MOPS open data (t187ap41 / t187ap34)",
    source_urls = c(YNOW_MOPS_BOARD_MEET_L, YNOW_MOPS_BOARD_ELECT_L)
  )
}

cached_fetch_tw_board_election <- memoise::memoise(
  fetch_tw_board_election,
  cache = if (exists(".ynow_slow_cache")) .ynow_slow_cache else cachem::cache_mem(max_age = 24 * 3600)
)

# ---- TW convertible bonds ------------------------------------------------

.tw_cb_num <- function(x) {
  s <- gsub(",", "", as.character(x %||% "")[1])
  suppressWarnings(as.numeric(s))
}

.tw_normalize_cb_rows <- function(rows, scope = "domestic") {
  if (is.null(rows) || !NROW(rows)) {
    return(data.frame(
      scope = character(0), bond_code = character(0), short_name = character(0),
      issue_amount = numeric(0), outstanding = numeric(0), conv_price = numeric(0),
      currency = character(0), period_start = character(0), period_end = character(0),
      maturity = character(0), coupon = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(rows)) rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  # JSON keys may use slash
  getc <- function(nm) {
    if (nm %in% names(rows)) return(rows[[nm]])
    alt <- gsub("/", ".", nm, fixed = TRUE)
    if (alt %in% names(rows)) return(rows[[alt]])
    rep(NA, nrow(rows))
  }
  data.frame(
    scope = scope,
    bond_code = trimws(as.character(getc("BondCode"))),
    short_name = trimws(as.character(getc("ShortName"))),
    issue_amount = vapply(getc("IssueAmount"), .tw_cb_num, numeric(1)),
    outstanding = vapply(getc("OutstandingAmount"), .tw_cb_num, numeric(1)),
    conv_price = vapply(getc("Conversion/ExchangePriceAtIssuance"), .tw_cb_num, numeric(1)),
    currency = trimws(as.character(getc("Currency"))),
    period_start = vapply(getc("Conversion/ExchangePeriodStartDate"), .tw_packed_date_to_iso, character(1)),
    period_end = vapply(getc("Conversion/ExchangePeriodEndDate"), .tw_packed_date_to_iso, character(1)),
    maturity = vapply(getc("MaturityDate"), .tw_packed_date_to_iso, character(1)),
    coupon = vapply(getc("CouponRate"), .tw_cb_num, numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Potential conversion shares + dilution vs current shares.
#' Domestic CB: face TWD / conv price.
#' Overseas CB (Currency=2): face USD × FX / conv price (TWD).
.compute_cb_dilution <- function(cb_df, shares_out, fx_usd_twd = NA_real_) {
  if (is.null(cb_df) || !nrow(cb_df)) {
    return(list(
      rows = cb_df, potential_shares = 0, dilution_pct = NA_real_,
      shares_out = shares_out, note = "no_cb"
    ))
  }
  fx <- suppressWarnings(as.numeric(fx_usd_twd)[1])
  pot <- rep(NA_real_, nrow(cb_df))
  for (i in seq_len(nrow(cb_df))) {
    out_amt <- cb_df$outstanding[[i]]
    px <- cb_df$conv_price[[i]]
    if (!is.finite(out_amt) || out_amt <= 0 || !is.finite(px) || px <= 0) next
    cur <- as.character(cb_df$currency[[i]] %||% "1")
    if (identical(cur, "2") || identical(toupper(cur), "USD")) {
      if (!is.finite(fx) || fx <= 0) next
      pot[[i]] <- (out_amt * fx) / px
    } else {
      pot[[i]] <- out_amt / px
    }
  }
  cb_df$potential_shares <- pot
  total_pot <- sum(pot, na.rm = TRUE)
  sh <- suppressWarnings(as.numeric(shares_out)[1])
  dil <- if (is.finite(sh) && sh > 0 && is.finite(total_pot)) 100 * total_pot / sh else NA_real_
  list(
    rows = cb_df,
    potential_shares = total_pot,
    dilution_pct = dil,
    shares_out = sh,
    note = if (!is.finite(sh) || sh <= 0) "shares_missing" else "ok"
  )
}

.fetch_tpex_cb_for_issuer <- function(code) {
  code <- as.character(code %||% "")[1]
  if (!nzchar(code)) return(NULL)
  bind_hit <- function(url, scope) {
    raw <- .tw_json_get(url)
    if (is.null(raw)) return(NULL)
    df <- tryCatch(as.data.frame(raw, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || !nrow(df) || !("IssuerCode" %in% names(df))) return(NULL)
    hit <- df[trimws(as.character(df$IssuerCode)) == code, , drop = FALSE]
    if (!nrow(hit)) return(NULL)
    # keep outstanding > 0
    out_amt <- vapply(hit$OutstandingAmount, .tw_cb_num, numeric(1))
    hit <- hit[is.finite(out_amt) & out_amt > 0, , drop = FALSE]
    if (!nrow(hit)) return(NULL)
    .tw_normalize_cb_rows(hit, scope = scope)
  }
  dom <- tryCatch(bind_hit(YNOW_TPEX_CB_DOMESTIC_URL, "domestic"), error = function(e) NULL)
  ovs <- tryCatch(bind_hit(YNOW_TPEX_CB_OVERSEAS_URL, "overseas"), error = function(e) NULL)
  if (is.null(dom) && is.null(ovs)) return(NULL)
  if (is.null(dom)) return(ovs)
  if (is.null(ovs)) return(dom)
  rbind(dom, ovs)
}

#' @param shares_out current diluted / bridge shares
#' @param fx_usd_twd USD→TWD for overseas CB face
fetch_tw_convertible_bonds <- function(ticker, shares_out = NA_real_, fx_usd_twd = NA_real_) {
  code <- .tw_issuer_code(ticker)
  empty <- list(
    market = "TW",
    company_code = code,
    has_data = FALSE,
    n_issues = 0L,
    potential_shares = 0,
    dilution_pct = NA_real_,
    shares_out = shares_out,
    rows = data.frame(),
    source = "TPEx OpenAPI bond_ISSBD5 / bond_ISSBD6",
    source_urls = c(YNOW_TPEX_CB_DOMESTIC_URL, YNOW_TPEX_CB_OVERSEAS_URL),
    note = "none"
  )
  if (!nzchar(code)) return(empty)
  cb <- tryCatch(.fetch_tpex_cb_for_issuer(code), error = function(e) NULL)
  if (is.null(cb) || !nrow(cb)) return(empty)
  dil <- .compute_cb_dilution(cb, shares_out = shares_out, fx_usd_twd = fx_usd_twd)
  list(
    market = "TW",
    company_code = code,
    has_data = TRUE,
    n_issues = nrow(dil$rows),
    potential_shares = dil$potential_shares,
    dilution_pct = dil$dilution_pct,
    shares_out = dil$shares_out,
    rows = dil$rows,
    source = "TPEx OpenAPI bond_ISSBD5 (domestic CB) / bond_ISSBD6 (overseas CB)",
    source_urls = c(YNOW_TPEX_CB_DOMESTIC_URL, YNOW_TPEX_CB_OVERSEAS_URL),
    note = dil$note
  )
}

# Cache CB universe fetches are heavy — memoise per ticker+shares+fx via wrapper
.fetch_tw_convertible_bonds_cached_core <- function(ticker) {
  fetch_tw_convertible_bonds(ticker, shares_out = NA_real_, fx_usd_twd = NA_real_)
}
cached_fetch_tw_convertible_bonds_core <- memoise::memoise(
  .fetch_tw_convertible_bonds_cached_core,
  cache = if (exists(".ynow_slow_cache")) .ynow_slow_cache else cachem::cache_mem(max_age = 24 * 3600)
)

fetch_tw_convertible_bonds_live <- function(ticker, shares_out = NA_real_, fx_usd_twd = NA_real_) {
  core <- cached_fetch_tw_convertible_bonds_core(ticker)
  if (!isTRUE(core$has_data) || is.null(core$rows) || !nrow(core$rows)) return(core)
  dil <- .compute_cb_dilution(core$rows, shares_out = shares_out, fx_usd_twd = fx_usd_twd)
  core$potential_shares <- dil$potential_shares
  core$dilution_pct <- dil$dilution_pct
  core$shares_out <- dil$shares_out
  core$rows <- dil$rows
  core$note <- dil$note
  core
}

# ---- US soft stubs -------------------------------------------------------

fetch_us_board_election_stub <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  list(
    market = "US",
    company_code = tk,
    has_data = FALSE,
    meeting_type = "",
    meeting_date = "",
    electing = FALSE,
    elect_method = "",
    nomination = FALSE,
    nomination_window = "",
    n_director = 0L,
    n_independent = 0L,
    n_supervisor = 0L,
    candidates = data.frame(),
    source = "SEC EDGAR DEF 14A (proxy)",
    source_urls = c(
      paste0(
        "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",
        utils::URLencode(tk, reserved = TRUE),
        "&type=DEF+14A&owner=exclude&count=10"
      )
    ),
    note = "us_proxy"
  )
}

fetch_us_cb_soft <- function(ticker, shares_out = NA_real_, shares_diluted = NA_real_) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  sh <- suppressWarnings(as.numeric(shares_out)[1])
  dil_sh <- suppressWarnings(as.numeric(shares_diluted)[1])
  gap <- if (is.finite(sh) && sh > 0 && is.finite(dil_sh) && dil_sh > sh) dil_sh - sh else NA_real_
  gap_pct <- if (is.finite(gap) && is.finite(sh) && sh > 0) 100 * gap / sh else NA_real_
  list(
    market = "US",
    company_code = tk,
    has_data = is.finite(gap_pct),
    n_issues = 0L,
    potential_shares = if (is.finite(gap)) gap else 0,
    dilution_pct = gap_pct,
    shares_out = sh,
    shares_diluted = dil_sh,
    rows = data.frame(),
    source = "Yahoo diluted vs basic shares (soft proxy); CB detail → SEC 10-K notes",
    source_urls = c(
      paste0(
        "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",
        utils::URLencode(tk, reserved = TRUE),
        "&type=10-K&owner=exclude&count=5"
      )
    ),
    note = if (is.finite(gap_pct)) "soft_gap" else "us_no_cb_feed"
  )
}

# ---- UI helpers ----------------------------------------------------------

#' Data-limited–style key-date chips (inline; not a separate block).
.ynow_key_date_chips_ui <- function(dates, market_mode = "US", loc = "en") {
  if (is.null(dates) || !is.list(dates)) return(NULL)
  loc <- if (exists("normalize_ui_locale", mode = "function")) normalize_ui_locale(loc) else loc
  .lab <- function(key, fallback) {
    if (exists("ui_str", mode = "function")) {
      s <- tryCatch(ui_str(key, loc), error = function(e) "")
      if (nzchar(s)) return(s)
    }
    fallback
  }
  chip <- function(k, v, estimate = FALSE) {
    v <- trimws(as.character(v %||% "")[1])
    if (!nzchar(v)) return(NULL)
    tags$span(
      class = "ynow-key-date-chip",
      title = if (isTRUE(estimate)) .lab("key_date_estimate_tip", "Date may be an estimate") else k,
      tags$span(class = "k", k),
      tags$span(class = "v", v)
    )
  }
  items <- list(
    chip(.lab("key_date_fs_release", "Next FS release"), dates$earnings_date, dates$is_estimate),
    chip(.lab("key_date_earnings_call", "Next earnings call"), dates$earnings_call, FALSE)
  )
  mode_chip <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(market_mode)
  } else {
    toupper(as.character(market_mode %||% "US")[1])
  }
  if (identical(mode_chip, "US")) {
    g_lab <- .lab("key_date_guidance", "Next guidance")
    if (isTRUE(dates$guidance_soft)) {
      g_lab <- .lab("key_date_guidance_soft", "Guidance (w/ earnings)")
    }
    items[[length(items) + 1L]] <- chip(g_lab, dates$guidance_date, isTRUE(dates$guidance_soft))
  }
  items <- Filter(Negate(is.null), items)
  if (!length(items)) return(NULL)
  tags$span(class = "ynow-key-date-chips", items)
}

.fmt_shares_short <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return("—")
  if (abs(x) >= 1e9) return(sprintf("%.2fB", x / 1e9))
  if (abs(x) >= 1e6) return(sprintf("%.2fM", x / 1e6))
  if (abs(x) >= 1e3) return(sprintf("%.1fK", x / 1e3))
  format(round(x), big.mark = ",", scientific = FALSE)
}

.fmt_pct_1 <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return("—")
  sprintf("%.1f%%", x)
}

#' Board election + CB dilution panel for Company FINANCIAL REPORT tab.
.ynow_corp_events_panel_ui <- function(board, cb, market_mode = "US", loc = "en") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) normalize_ui_locale(loc) else loc
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(market_mode)
  } else {
    as.character(market_mode)[1]
  }
  .lab <- function(key, fallback) {
    if (exists("ui_str", mode = "function")) {
      s <- tryCatch(ui_str(key, loc), error = function(e) "")
      if (nzchar(s)) return(s)
    }
    fallback
  }

  # --- Board block ---
  board_body <- if (identical(mode, "TW") && isTRUE(board$has_data)) {
    elect_txt <- if (isTRUE(board$electing)) {
      .lab("corp_board_elect_yes", "Board election at this AGM: Yes")
    } else {
      .lab("corp_board_elect_no", "Board election at this AGM: No")
    }
    cand_note <- if (isTRUE(board$electing) && (board$n_director + board$n_independent + board$n_supervisor) > 0) {
      sprintf(
        .lab("corp_board_cand_counts", "Candidates — directors %d / independent %d / supervisors %d"),
        board$n_director, board$n_independent, board$n_supervisor
      )
    } else {
      NULL
    }
    tagList(
      tags$p(
        tags$b(.lab("corp_board_meeting", "Shareholder meeting")),
        ": ",
        if (nzchar(board$meeting_type)) paste0(board$meeting_type, " · ") else "",
        if (nzchar(board$meeting_date)) board$meeting_date else "—"
      ),
      tags$p(elect_txt),
      if (nzchar(board$elect_method)) {
        tags$p(
          tags$b(.lab("corp_board_method", "Election method")),
          ": ", board$elect_method,
          if (isTRUE(board$nomination)) {
            paste0(" · ", .lab("corp_board_nomination", "nomination system"))
          } else {
            NULL
          }
        )
      },
      if (nzchar(board$nomination_window)) {
        tags$p(
          tags$b(.lab("corp_board_nomination_window", "Nomination window")),
          ": ", board$nomination_window
        )
      },
      if (!is.null(cand_note)) tags$p(cand_note),
      if (!is.null(board$candidates) && nrow(board$candidates) > 0 && isTRUE(board$electing)) {
        show_n <- min(12L, nrow(board$candidates))
        tags$div(
          class = "ynow-corp-events-cand",
          tags$p(
            style = "margin:6px 0 4px 0; font-size:12px; color:#666;",
            .lab("corp_board_cand_list", "Candidate list (open data; may be prior AGM)")
          ),
          tags$ul(
            lapply(seq_len(show_n), function(i) {
              r <- board$candidates[i, ]
              tags$li(sprintf("%s — %s%s", r$role, r$name,
                              if (identical(r$elected, "是")) " ✓" else ""))
            })
          )
        )
      },
      tags$p(
        class = "ynow-corp-events-source",
        .lab("corp_board_source", "Source"), ": ", board$source
      )
    )
  } else if (identical(mode, "TW")) {
    tags$p(
      class = "ynow-corp-events-waiting",
      .lab("corp_board_empty_tw", "No MOPS board-election open data for this ticker yet.")
    )
  } else {
    url <- if (!is.null(board$source_urls) && length(board$source_urls)) board$source_urls[[1]] else ""
    tagList(
      tags$p(.lab(
        "corp_board_us_note",
        "US board elections are disclosed in DEF 14A proxy statements (no free open-data feed matching TW MOPS)."
      )),
      if (nzchar(url)) {
        tags$p(
          tags$a(href = url, target = "_blank", rel = "noopener noreferrer",
                 .lab("corp_board_us_sec_link", "Open SEC EDGAR DEF 14A search"))
        )
      }
    )
  }

  # --- CB block ---
  cb_body <- if (identical(mode, "TW") && isTRUE(cb$has_data)) {
    rows <- cb$rows
    tbl <- if (!is.null(rows) && nrow(rows)) {
      show <- head(rows, 8L)
      tags$table(
        class = "table table-condensed ynow-corp-cb-table",
        tags$thead(tags$tr(
          tags$th(.lab("corp_cb_col_code", "CB code")),
          tags$th(.lab("corp_cb_col_name", "Short name")),
          tags$th(.lab("corp_cb_col_out", "Outstanding")),
          tags$th(.lab("corp_cb_col_px", "Conv. price")),
          tags$th(.lab("corp_cb_col_pot", "Potential shares")),
          tags$th(.lab("corp_cb_col_end", "Conv. end"))
        )),
        tags$tbody(lapply(seq_len(nrow(show)), function(i) {
          r <- show[i, ]
          tags$tr(
            tags$td(r$bond_code),
            tags$td(r$short_name),
            tags$td(.fmt_shares_short(r$outstanding)),
            tags$td(if (is.finite(r$conv_price)) sprintf("%.2f", r$conv_price) else "—"),
            tags$td(.fmt_shares_short(r$potential_shares)),
            tags$td(if (nzchar(r$period_end)) r$period_end else r$maturity)
          )
        }))
      )
    } else {
      NULL
    }
    tagList(
      tags$p(
        tags$b(.lab("corp_cb_issues", "Outstanding CB issues")),
        ": ", as.integer(cb$n_issues),
        " · ",
        tags$b(.lab("corp_cb_potential", "Potential conversion shares")),
        ": ", .fmt_shares_short(cb$potential_shares),
        " · ",
        tags$b(.lab("corp_cb_dilution", "Implied dilution")),
        ": ", .fmt_pct_1(cb$dilution_pct)
      ),
      tags$p(
        style = "font-size:12px; color:#666; margin-top:-4px;",
        .lab(
          "corp_cb_dilution_help",
          "Dilution ≈ Σ(outstanding face ÷ conversion price) ÷ current shares. Overseas CB (USD face) uses USD/TWD FX. Engineering heuristic — verify vs MOPS / prospectus."
        )
      ),
      if (identical(cb$note, "shares_missing")) {
        tags$p(
          style = "font-size:12px; color:#a67c00;",
          .lab("corp_cb_shares_missing", "Current shares unavailable — dilution % withheld.")
        )
      },
      tbl,
      tags$p(
        class = "ynow-corp-events-source",
        .lab("corp_cb_source", "Source"), ": ", cb$source
      )
    )
  } else if (identical(mode, "TW")) {
    tags$p(
      class = "ynow-corp-events-waiting",
      .lab("corp_cb_empty_tw", "No outstanding convertible / exchangeable bonds in TPEx open data for this issuer.")
    )
  } else {
    url <- if (!is.null(cb$source_urls) && length(cb$source_urls)) cb$source_urls[[1]] else ""
    tagList(
      tags$p(.lab(
        "corp_cb_us_note",
        "US convertible notes lack a free issuance open-data feed comparable to TPEx ISSBD. Soft proxy: Yahoo diluted vs basic share gap when available."
      )),
      if (isTRUE(cb$has_data)) {
        tags$p(
          tags$b(.lab("corp_cb_soft_gap", "Diluted − basic shares")),
          ": ", .fmt_shares_short(cb$potential_shares),
          " (", .fmt_pct_1(cb$dilution_pct), ")"
        )
      },
      if (nzchar(url)) {
        tags$p(
          tags$a(href = url, target = "_blank", rel = "noopener noreferrer",
                 .lab("corp_cb_us_sec_link", "Open SEC EDGAR 10-K search"))
        )
      }
    )
  }

  tags$div(
    class = "ynow-corp-events",
    tags$div(
      class = "ynow-corp-events-block",
      tags$h4(class = "ynow-corp-events-h", .lab("corp_board_title", "Board election")),
      board_body
    ),
    tags$hr(),
    tags$div(
      class = "ynow-corp-events-block",
      tags$h4(class = "ynow-corp-events-h", .lab("corp_cb_title", "Convertible bonds — dilution")),
      cb_body
    )
  )
}
