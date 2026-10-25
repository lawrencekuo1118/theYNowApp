# ==========================================
# ownership_structure.R — Company 股權結構分析
#
# Sources (no invented holdings):
#   US: Yahoo quoteSummary (majorHoldersBreakdown / institutionOwnership /
#       fundOwnership / insiderHolders / insiderTransactions) via
#       deep_scraper.get_ownership_quote — free-float–relevant top-10 when present
#   TW: Prefer Yahoo institution/fund (local or HTCDI ADR) as free-float top-10;
#       else MOPS t187ap11 (董監／內部人). Structure: TWSE MI_QFIIS + T86.
#   Volume: existing fetch_price_history_df (Yahoo); TWSE STOCK_DAY fallback
#   Institution drill-down / cross-hold: name→public ticker map + second fetch only
# ==========================================

YNOW_TWSE_MI_QFIIS_URL <- "https://www.twse.com.tw/rwd/zh/fund/MI_QFIIS"
YNOW_TWSE_T86_URL <- "https://www.twse.com.tw/rwd/zh/fund/T86"
YNOW_TWSE_STOCK_DAY_URL <- "https://www.twse.com.tw/rwd/zh/afterTrading/STOCK_DAY"
YNOW_MOPS_INSIDER_L_JSON <- "https://openapi.twse.com.tw/v1/opendata/t187ap11_L"
YNOW_MOPS_INSIDER_O_JSON <- "https://www.tpex.org.tw/openapi/v1/mopsfin_t187ap11_O"
YNOW_MOPS_INSIDER_L_CSV <- "https://mopsfin.twse.com.tw/opendata/t187ap11_L.csv"
YNOW_MOPS_INSIDER_O_CSV <- "https://mopsfin.twse.com.tw/opendata/t187ap11_O.csv"

.ownership_empty_holders_df <- function() {
  data.frame(
    rank = integer(),
    name = character(),
    shares = numeric(),
    pct = numeric(),
    holder_type = character(),
    report_date = character(),
    source = character(),
    credibility = character(),
    clickable = logical(),
    stringsAsFactors = FALSE
  )
}

.ownership_empty_snapshot <- function(ticker = "", market = "US") {
  list(
    ticker = as.character(ticker %||% "")[1],
    market = as.character(market %||% "US")[1],
    as_of = "",
    structure = list(),
    flow = list(
      net_shares = NA_real_,
      buy_shares = NA_real_,
      sell_shares = NA_real_,
      label = "",
      as_of = "",
      source = "",
      credibility = "unavailable"
    ),
    volume = data.frame(Date = as.Date(character()), Volume = numeric(), stringsAsFactors = FALSE),
    holders = .ownership_empty_holders_df(),
    holders_kind = "empty",
    holders_source_ticker = "",
    detail = NULL,
    sources = character(),
    notes = character(),
    ok = FALSE
  )
}

.ownership_parse_num <- function(x) {
  s <- gsub(",", "", as.character(x %||% "")[1])
  s <- gsub("%", "", s)
  s <- trimws(s)
  if (!nzchar(s) || s %in% c("-", "--", "—", "NA", "null")) return(NA_real_)
  suppressWarnings(as.numeric(s))
}

.ownership_issuer_code <- function(ticker) {
  if (exists(".tw_issuer_code", mode = "function")) {
    return(.tw_issuer_code(ticker))
  }
  bare <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(bare)) return("")
  bare <- sub("\\.(TW|TWO)$", "", bare)
  gsub("[^0-9A-Z]", "", bare)
}

.ownership_http_json <- function(url, timeout_sec = 45) {
  if (exists(".tw_json_get", mode = "function")) {
    return(.tw_json_get(url, timeout_sec = timeout_sec))
  }
  txt <- NULL
  if (exists(".tw_http_get_text", mode = "function")) {
    txt <- .tw_http_get_text(url, timeout_sec = timeout_sec)
  } else if (nzchar(Sys.which("curl"))) {
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    st <- tryCatch(
      system2(
        "curl",
        c("-fsSL", "-A", "theYNowApp/ownership", "--max-time", as.character(timeout_sec),
          "-o", tmp, url),
        stdout = FALSE, stderr = FALSE
      ),
      error = function(e) 1L
    )
    if (identical(as.integer(st), 0L) && file.exists(tmp)) {
      txt <- tryCatch(rawToChar(readBin(tmp, "raw", file.info(tmp)$size)), error = function(e) NULL)
    }
  }
  if (is.null(txt) || !nzchar(txt) || grepl("^\\s*<", txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE), error = function(e) NULL)
}

.ownership_is_institution_name <- function(name, holder_type = "") {
  ht <- tolower(trimws(as.character(holder_type %||% "")[1]))
  if (ht %in% c("institution", "法人", "fund")) return(TRUE)
  if (ht %in% c("individual", "insider", "自然人")) return(FALSE)
  nm <- as.character(name %||% "")[1]
  if (!nzchar(nm)) return(FALSE)
  # TW / EN institutional cues (conservative; individuals stay non-clickable)
  grepl(
    paste0(
      "公司|銀行|證券|投信|基金|保險|人壽|商銀|金控|投資|法人|",
      "Inc\\.?$|Corp\\.?$|LLC|LP\\b|Ltd\\.?$|Limited|Bank|Fund|",
      "Capital|Asset|Management|Holdings|Trust|Partners|Group"
    ),
    nm,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.ownership_holders_has_institution <- function(holders) {
  is.data.frame(holders) && nrow(holders) > 0 &&
    any(holders$holder_type %in% c("institution", "fund", "法人"))
}

.ownership_holders_kind_from_df <- function(holders) {
  if (!is.data.frame(holders) || !nrow(holders)) return("empty")
  if (.ownership_holders_has_institution(holders)) return("free_float")
  "insider"
}

#' Normalize Yahoo get_ownership_quote list → structure slices + holders.
.ownership_from_yahoo <- function(raw, ticker = "") {
  structure <- list()
  holders <- .ownership_empty_holders_df()
  flow <- .ownership_empty_snapshot()$flow
  sources <- character()
  notes <- character()
  as_of <- ""
  float_shares <- NA_real_
  shares_out <- NA_real_

  if (is.null(raw) || !is.list(raw)) {
    return(list(
      structure = structure, holders = holders, flow = flow,
      sources = sources, notes = c(notes, "yahoo_unavailable"), as_of = as_of,
      holders_kind = "empty", float_shares = float_shares,
      shares_outstanding = shares_out, ok = FALSE
    ))
  }
  if (!isTRUE(raw$ok)) {
    err <- as.character(raw$error %||% "")[1]
    if (nzchar(err)) notes <- c(notes, paste0("yahoo:", err))
    return(list(
      structure = structure, holders = holders, flow = flow,
      sources = sources, notes = notes, as_of = as_of,
      holders_kind = "empty", float_shares = float_shares,
      shares_outstanding = shares_out, ok = FALSE
    ))
  }
  sources <- c(sources, "Yahoo Finance quoteSummary")
  inst <- suppressWarnings(as.numeric(raw$institutions_pct)[1])
  ins <- suppressWarnings(as.numeric(raw$insiders_pct)[1])
  inst_float <- suppressWarnings(as.numeric(raw$institutions_float_pct)[1])
  float_shares <- suppressWarnings(as.numeric(raw$float_shares)[1])
  shares_out <- suppressWarnings(as.numeric(raw$shares_outstanding)[1])
  # Yahoo fractions are usually 0–1
  if (is.finite(inst) && inst > 1.5) inst <- inst / 100
  if (is.finite(ins) && ins > 1.5) ins <- ins / 100
  if (is.finite(inst_float) && inst_float > 1.5) inst_float <- inst_float / 100
  if (is.finite(inst)) {
    structure[[length(structure) + 1L]] <- list(
      key = "institutions", pct = inst, credibility = "yahoo",
      source = "Yahoo majorHoldersBreakdown"
    )
  }
  # Prefer float-relative institution % when Yahoo discloses it (free-float path)
  if (is.finite(inst_float) && (!is.finite(inst) || abs(inst_float - inst) > 0.005)) {
    structure[[length(structure) + 1L]] <- list(
      key = "institutions_float", pct = inst_float, credibility = "yahoo",
      source = "Yahoo institutionsFloatPercentHeld"
    )
  }
  if (is.finite(ins)) {
    structure[[length(structure) + 1L]] <- list(
      key = "insiders", pct = ins, credibility = "yahoo",
      source = "Yahoo majorHoldersBreakdown"
    )
  }
  if (is.finite(inst) && is.finite(ins)) {
    other <- max(0, 1 - inst - ins)
    if (other > 0.001) {
      structure[[length(structure) + 1L]] <- list(
        key = "other", pct = other, credibility = "estimate",
        source = "residual (1 − institutions − insiders)"
      )
    }
  }

  names_v <- as.character(unlist(raw$holder_name %||% list(), use.names = FALSE))
  pct_v <- suppressWarnings(as.numeric(unlist(raw$holder_pct %||% list(), use.names = FALSE)))
  sh_v <- suppressWarnings(as.numeric(unlist(raw$holder_shares %||% list(), use.names = FALSE)))
  rd_v <- as.character(unlist(raw$holder_report_date %||% list(), use.names = FALSE))
  kind_v <- as.character(unlist(raw$holder_kind %||% list(), use.names = FALSE))
  n <- min(length(names_v), 10L)
  if (n > 0L) {
    pct_v <- if (length(pct_v) >= n) pct_v[seq_len(n)] else rep(NA_real_, n)
    sh_v <- if (length(sh_v) >= n) sh_v[seq_len(n)] else rep(NA_real_, n)
    rd_v <- if (length(rd_v) >= n) rd_v[seq_len(n)] else rep("", n)
    kind_v <- if (length(kind_v) >= n) kind_v[seq_len(n)] else rep("", n)
    # Normalize pct > 1.5 as percent points
    pct_v <- ifelse(is.finite(pct_v) & pct_v > 1.5, pct_v / 100, pct_v)
    htype <- ifelse(
      grepl("insider", kind_v, ignore.case = TRUE),
      "insider",
      "institution"
    )
    click <- vapply(
      seq_len(n),
      function(i) .ownership_is_institution_name(names_v[i], htype[i]),
      logical(1)
    )
    holders <- data.frame(
      rank = seq_len(n),
      name = names_v[seq_len(n)],
      shares = sh_v,
      pct = pct_v,
      holder_type = htype,
      report_date = rd_v,
      source = rep("Yahoo Finance", n),
      credibility = rep("yahoo", n),
      clickable = click,
      stringsAsFactors = FALSE
    )
    as_of <- rd_v[nzchar(rd_v)][1] %||% ""
  }

  net <- suppressWarnings(as.numeric(raw$insider_net_shares)[1])
  n_txn <- suppressWarnings(as.integer(raw$insider_txn_count)[1])
  if (is.finite(net) && is.finite(n_txn) && n_txn > 0L) {
    flow <- list(
      net_shares = net,
      buy_shares = if (net > 0) net else NA_real_,
      sell_shares = if (net < 0) abs(net) else NA_real_,
      label = "insider_transactions",
      as_of = "",
      source = "Yahoo insiderTransactions",
      credibility = "yahoo"
    )
  }

  list(
    structure = structure, holders = holders, flow = flow,
    sources = sources, notes = notes, as_of = as_of,
    holders_kind = .ownership_holders_kind_from_df(holders),
    float_shares = float_shares, shares_outstanding = shares_out, ok = TRUE
  )
}

.ownership_tw_yyyymmdd_recent <- function(n_back = 12L) {
  # TWSE session dates: walk back calendar days until T86/QFIIS OK
  out <- character()
  day <- Sys.Date()
  for (i in seq_len(as.integer(n_back))) {
    d <- day - (i - 1L)
    if (as.integer(format(d, "%u")) >= 6L) next
    out <- c(out, format(d, "%Y%m%d"))
  }
  unique(out)
}

.ownership_twse_rows <- function(j) {
  fields <- as.character(j$fields %||% character())
  data <- j$data
  if (is.null(data) || !length(fields)) return(NULL)
  rows <- NULL
  if (is.data.frame(data)) {
    rows <- data
  } else if (is.matrix(data)) {
    rows <- as.data.frame(data, stringsAsFactors = FALSE)
  } else if (is.list(data)) {
    rows <- tryCatch(
      as.data.frame(do.call(rbind, data), stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(rows) || !nrow(rows)) return(NULL)
  if (ncol(rows) == length(fields)) {
    names(rows) <- fields
  } else if (ncol(rows) > 0 && length(fields) > 0) {
    # Partial name align when column count drifts
    n <- min(ncol(rows), length(fields))
    names(rows)[seq_len(n)] <- fields[seq_len(n)]
  }
  rows
}

.ownership_tw_qfiis_row <- function(code) {
  code <- as.character(code %||% "")[1]
  if (!nzchar(code)) return(NULL)
  for (d in .ownership_tw_yyyymmdd_recent(14L)) {
    url <- sprintf(
      "%s?response=json&date=%s&selectType=ALLBUT0999",
      YNOW_TWSE_MI_QFIIS_URL, d
    )
    j <- .ownership_http_json(url, timeout_sec = 30)
    if (is.null(j) || !identical(as.character(j$stat %||% "")[1], "OK")) next
    rows <- .ownership_twse_rows(j)
    if (is.null(rows)) next
    code_col <- names(rows)[1]
    hit <- rows[trimws(as.character(rows[[code_col]])) == code, , drop = FALSE]
    if (!nrow(hit)) next
    pct <- .ownership_parse_num(hit[["全體外資及陸資持股比率"]][1] %||% hit[[8]][1])
    shares <- .ownership_parse_num(hit[["全體外資及陸資持有股數"]][1] %||% hit[[6]][1])
    issued <- .ownership_parse_num(hit[["發行股數"]][1] %||% hit[[4]][1])
    return(list(
      as_of = as.character(j$date %||% d)[1],
      foreign_pct = if (is.finite(pct) && pct > 1.5) pct / 100 else pct,
      foreign_shares = shares,
      issued_shares = issued,
      source = "TWSE MI_QFIIS",
      credibility = "official"
    ))
  }
  NULL
}

.ownership_tw_t86_row <- function(code) {
  code <- as.character(code %||% "")[1]
  if (!nzchar(code)) return(NULL)
  for (d in .ownership_tw_yyyymmdd_recent(14L)) {
    url <- sprintf(
      "%s?response=json&date=%s&selectType=ALLBUT0999",
      YNOW_TWSE_T86_URL, d
    )
    j <- .ownership_http_json(url, timeout_sec = 30)
    if (is.null(j) || !identical(as.character(j$stat %||% "")[1], "OK")) next
    rows <- .ownership_twse_rows(j)
    if (is.null(rows)) next
    hit <- rows[trimws(as.character(rows[[names(rows)[1]]])) == code, , drop = FALSE]
    if (!nrow(hit)) next
    net <- .ownership_parse_num(hit[["三大法人買賣超股數"]][1])
    buy_f <- .ownership_parse_num(hit[["外陸資買進股數(不含外資自營商)"]][1])
    sell_f <- .ownership_parse_num(hit[["外陸資賣出股數(不含外資自營商)"]][1])
    return(list(
      as_of = as.character(j$date %||% d)[1],
      net_shares = net,
      buy_shares = buy_f,
      sell_shares = sell_f,
      source = "TWSE T86",
      credibility = "official",
      label = "tw_institutional_net"
    ))
  }
  NULL
}

.ownership_tw_insiders <- function(code) {
  code <- as.character(code %||% "")[1]
  if (!nzchar(code)) return(NULL)
  fetch_one <- function(url) {
    j <- .ownership_http_json(url, timeout_sec = 60)
    if (is.null(j)) return(NULL)
    if (is.data.frame(j)) return(j)
    if (is.list(j) && length(j) && is.list(j[[1]])) {
      return(tryCatch({
        rows <- lapply(j, function(row) {
          if (is.data.frame(row)) return(row)
          as.data.frame(row, stringsAsFactors = FALSE)
        })
        # Align columns
        cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
        rows <- lapply(rows, function(r) {
          miss <- setdiff(cols, names(r))
          for (m in miss) r[[m]] <- NA
          r[cols]
        })
        do.call(rbind, rows)
      }, error = function(e) NULL))
    }
    NULL
  }
  df <- NULL
  for (url in c(YNOW_MOPS_INSIDER_L_JSON, YNOW_MOPS_INSIDER_O_JSON)) {
    df <- fetch_one(url)
    if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
      hit <- df[trimws(as.character(df[["公司代號"]] %||% "")) == code, , drop = FALSE]
      if (nrow(hit) > 0) {
        df <- hit
        break
      }
    }
    df <- NULL
  }
  if (is.null(df) || !nrow(df)) return(NULL)
  # Drop pure representative rows with 0 shares when a 法人 principal exists
  shares <- vapply(df[["目前持股"]], .ownership_parse_num, numeric(1))
  names_v <- trimws(as.character(df[["姓名"]] %||% ""))
  titles <- trimws(as.character(df[["職稱"]] %||% ""))
  keep <- is.finite(shares) & shares > 0 & nzchar(names_v)
  # Prefer 本人 / 法人 over 代表人 duplicates
  is_rep <- grepl("代表人", titles)
  keep <- keep & !is_rep
  if (!any(keep)) {
    keep <- is.finite(shares) & shares > 0 & nzchar(names_v)
  }
  df <- df[keep, , drop = FALSE]
  shares <- shares[keep]
  names_v <- names_v[keep]
  titles <- titles[keep]
  if (!length(names_v)) return(NULL)
  # Aggregate same name
  agg <- tapply(shares, names_v, sum, na.rm = TRUE)
  nm <- names(agg)
  sh <- as.numeric(agg)
  ord <- order(sh, decreasing = TRUE)
  nm <- nm[ord]
  sh <- sh[ord]
  n <- min(length(nm), 10L)
  htype <- ifelse(
    vapply(nm[seq_len(n)], function(x) .ownership_is_institution_name(x, ""), logical(1)),
    "institution",
    "insider"
  )
  data.frame(
    rank = seq_len(n),
    name = nm[seq_len(n)],
    shares = sh[seq_len(n)],
    pct = rep(NA_real_, n),
    holder_type = htype,
    report_date = rep(as.character(df[["資料年月"]][1] %||% ""), n),
    source = rep("MOPS t187ap11", n),
    credibility = rep("official", n),
    clickable = htype == "institution",
    stringsAsFactors = FALSE
  )
}

.ownership_roc_slash_date <- function(x) {
  # TWSE STOCK_DAY uses ROC dates like 115/10/01
  s <- trimws(as.character(x %||% "")[1])
  if (!nzchar(s)) return(as.Date(NA))
  m <- regmatches(s, regexec("^([0-9]{2,3})/([0-9]{1,2})/([0-9]{1,2})$", s))[[1]]
  if (length(m) < 4L) return(as.Date(NA))
  y <- suppressWarnings(as.integer(m[2]))
  mo <- suppressWarnings(as.integer(m[3]))
  d <- suppressWarnings(as.integer(m[4]))
  if (!is.finite(y) || !is.finite(mo) || !is.finite(d)) return(as.Date(NA))
  tryCatch(
    as.Date(sprintf("%04d-%02d-%02d", y + 1911L, mo, d)),
    error = function(e) as.Date(NA)
  )
}

#' TWSE STOCK_DAY volume series (official 成交股數). Up to `months` calendar months.
.ownership_tw_stock_day_volume <- function(code, months = 24L) {
  empty <- data.frame(Date = as.Date(character()), Volume = numeric(), stringsAsFactors = FALSE)
  code <- as.character(code %||% "")[1]
  months <- max(1L, min(as.integer(months)[1], 60L))
  if (!nzchar(code)) return(empty)
  out <- list()
  # Anchor on first of current month; walk back
  anchor <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  for (i in seq_len(months) - 1L) {
    ym <- if (identical(i, 0L)) {
      anchor
    } else {
      tryCatch(
        seq(anchor, by = paste0("-", i, " month"), length.out = 2L)[2],
        error = function(e) NA
      )
    }
    if (is.na(ym)) next
    url <- sprintf(
      "%s?response=json&date=%s&stockNo=%s",
      YNOW_TWSE_STOCK_DAY_URL,
      format(ym, "%Y%m%d"),
      utils::URLencode(code, reserved = TRUE)
    )
    j <- .ownership_http_json(url, timeout_sec = 25)
    if (is.null(j) || !identical(as.character(j$stat %||% "")[1], "OK")) next
    data <- j$data
    if (is.null(data) || !length(data)) next
    if (is.matrix(data)) {
      rows <- as.data.frame(data, stringsAsFactors = FALSE)
    } else if (is.data.frame(data)) {
      rows <- data
    } else if (is.list(data)) {
      rows <- tryCatch(
        as.data.frame(do.call(rbind, data), stringsAsFactors = FALSE),
        error = function(e) NULL
      )
    } else {
      next
    }
    if (is.null(rows) || !nrow(rows) || ncol(rows) < 2L) next
    fields <- as.character(j$fields %||% character())
    if (length(fields) >= ncol(rows)) {
      names(rows)[seq_len(ncol(rows))] <- fields[seq_len(ncol(rows))]
    }
    date_col <- if ("日期" %in% names(rows)) "日期" else names(rows)[1]
    vol_col <- if ("成交股數" %in% names(rows)) "成交股數" else names(rows)[2]
    # lapply (not vapply): Date FUN.VALUE is stored as numeric under vapply
    dates <- as.Date(vapply(
      as.character(rows[[date_col]]),
      function(x) as.character(.ownership_roc_slash_date(x)),
      character(1)
    ))
    vols <- vapply(rows[[vol_col]], .ownership_parse_num, numeric(1))
    keep <- !is.na(dates) & is.finite(vols)
    if (!any(keep)) next
    out[[length(out) + 1L]] <- data.frame(
      Date = dates[keep],
      Volume = vols[keep],
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) return(empty)
  df <- do.call(rbind, out)
  df <- df[!duplicated(df$Date), , drop = FALSE]
  df[order(df$Date), , drop = FALSE]
}

.ownership_fetch_volume <- function(ticker, period = "5y", market = NULL) {
  empty <- data.frame(
    Date = as.Date(character()),
    Volume = numeric(),
    source = character(),
    stringsAsFactors = FALSE
  )
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  mode <- if (!is.null(market) && nzchar(as.character(market)[1])) {
    toupper(as.character(market)[1])
  } else if (grepl("\\.(TW|TWO)$", tk)) {
    "TW"
  } else {
    "US"
  }
  if (exists("fetch_price_history_df", mode = "function")) {
    df <- tryCatch(fetch_price_history_df(tk, period), error = function(e) NULL)
    if (is.data.frame(df) && nrow(df) > 0 && all(c("Date", "Volume") %in% names(df))) {
      out <- df[, c("Date", "Volume"), drop = FALSE]
      out <- out[is.finite(out$Volume) & !is.na(out$Date), , drop = FALSE]
      if (nrow(out) > 0) {
        out$source <- "Yahoo price history"
        return(out[order(out$Date), , drop = FALSE])
      }
    }
  }
  # TW official fallback when Yahoo history is empty / rate-limited
  if (identical(mode, "TW")) {
    code <- .ownership_issuer_code(tk)
    tw <- tryCatch(.ownership_tw_stock_day_volume(code, months = 24L), error = function(e) NULL)
    if (is.data.frame(tw) && nrow(tw) > 0) {
      tw$source <- "TWSE STOCK_DAY"
      return(tw)
    }
  }
  empty
}

.ownership_yahoo_live <- function(ticker) {
  if (!exists("get_ownership_quote", mode = "function")) return(NULL)
  tryCatch(get_ownership_quote(ticker), error = function(e) NULL)
}

#' HTCDI ADR ticker for a TW ordinary listing (free-float Yahoo fallback only).
.ownership_adr_ticker_for_tw <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk) || !grepl("\\.(TW|TWO)$", tk)) return("")
  if (!exists("htcdi_find_issuer", mode = "function")) return("")
  iss <- tryCatch(htcdi_find_issuer(tk), error = function(e) NULL)
  if (is.null(iss)) return("")
  itype <- toupper(as.character(iss$instrument_type %||% "")[1])
  # Issuer may be keyed by ADR id with local_ordinary_ticker = this TW code
  loc <- toupper(trimws(as.character(iss$local_ordinary_ticker %||% "")[1]))
  adr <- toupper(trimws(as.character(iss$tickers[[1]] %||% iss$id %||% "")[1]))
  if (!nzchar(adr) || identical(adr, tk)) return("")
  if (nzchar(loc) && identical(loc, tk)) return(adr)
  if (identical(itype, "ADR") && nzchar(loc) && identical(loc, tk)) return(adr)
  # Also accept when find_issuer matched via local_ordinary and primary ticker is ADR
  if (nzchar(loc) && identical(loc, tk) && !grepl("\\.(TW|TWO)$", adr)) return(adr)
  ""
}

#' Free-float–relevant Yahoo holders for a ticker; optional ADR retry for TW.
.ownership_yahoo_float_holders <- function(ticker, market = NULL) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  empty <- list(
    holders = .ownership_empty_holders_df(),
    holders_kind = "empty",
    source_ticker = "",
    as_of = "",
    sources = character(),
    notes = character(),
    structure_extra = list(),
    ok = FALSE
  )
  if (!nzchar(tk)) return(empty)

  try_one <- function(sym, note_tag = NULL) {
    raw <- .ownership_yahoo_live(sym)
    y <- .ownership_from_yahoo(raw, sym)
    if (!isTRUE(y$ok) || !is.data.frame(y$holders) || !nrow(y$holders)) {
      return(NULL)
    }
    if (!.ownership_holders_has_institution(y$holders)) {
      return(NULL)
    }
    list(
      holders = y$holders,
      holders_kind = "free_float",
      source_ticker = sym,
      as_of = y$as_of %||% "",
      sources = y$sources %||% character(),
      notes = c(
        if (!is.null(note_tag)) note_tag else "holders_free_float_yahoo",
        y$notes %||% character()
      ),
      structure_extra = {
        # Keep institutions_float slice only
        Filter(function(s) identical(s$key, "institutions_float"), y$structure %||% list())
      },
      ok = TRUE
    )
  }

  hit <- try_one(tk, "holders_free_float_yahoo")
  if (!is.null(hit)) return(hit)

  mode <- if (!is.null(market) && nzchar(as.character(market)[1])) {
    toupper(as.character(market)[1])
  } else if (grepl("\\.(TW|TWO)$", tk)) {
    "TW"
  } else {
    "US"
  }
  if (identical(mode, "TW")) {
    adr <- .ownership_adr_ticker_for_tw(tk)
    if (nzchar(adr) && !identical(adr, tk)) {
      hit <- try_one(adr, "holders_free_float_yahoo_adr")
      if (!is.null(hit)) return(hit)
    }
  }
  empty
}

#' Curated institution / 法人 name → public ticker (disclosure path only).
#' Private managers (Vanguard, Fidelity Management) stay unmapped — honest empty top-5.
.ownership_institution_ticker_map <- function() {
  list(
    list(pattern = "(?i)blackrock", ticker = "BLK", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)state\\s*street", ticker = "STT", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)t\\.?\\s*rowe", ticker = "TROW", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)goldman\\s*sachs", ticker = "GS", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)morgan\\s*stanley", ticker = "MS", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)j\\.?p\\.?\\s*morgan|jpmorgan", ticker = "JPM", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)bank\\s*of\\s*america|bofa\\b", ticker = "BAC", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)charles\\s*schwab", ticker = "SCHW", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)northern\\s*trust", ticker = "NTRS", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)\\binvesco\\b", ticker = "IVZ", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)franklin\\s*(resources|templeton)", ticker = "BEN", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)ameriprise", ticker = "AMP", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)\\bubs\\b", ticker = "UBS", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)deutsche\\s*bank", ticker = "DB", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)berkshire\\s*hathaway", ticker = "BRK-B", intro_key = "own_detail_intro_mapped"),
    list(pattern = "(?i)\\bhsbc\\b", ticker = "HSBC", intro_key = "own_detail_intro_mapped"),
    # TW listed 金控 / banks (common MOPS 法人 names)
    list(pattern = "國泰(金融|金控|人壽|世華|證)", ticker = "2882.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "富邦(金融|金控|人壽|銀行|證)", ticker = "2881.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "中(國)?信託|中信金", ticker = "2891.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "兆豐(金|國際|商銀|銀行)", ticker = "2886.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "第一(金|銀行|金控)", ticker = "2892.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "合作金庫|合庫金", ticker = "5880.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "華南(金|銀行|金控)", ticker = "2880.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "元大(金|證券|投信|銀行)", ticker = "2885.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "永豐(金|銀行|金控)", ticker = "2890.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "玉山(金|銀行)", ticker = "2884.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "台新(金|銀行)", ticker = "2887.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "新光(金|人壽)", ticker = "2888.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "開發金|凱基(證|銀行|人壽)", ticker = "2883.TW", intro_key = "own_detail_intro_tw_mapped"),
    list(pattern = "中華郵政|郵政公司", ticker = "", intro_key = "own_detail_intro_tw_gov"),
    list(pattern = "勞工退休|勞退基金|勞保局", ticker = "", intro_key = "own_detail_intro_tw_gov"),
    list(pattern = "行政院國家發展基金|國發基金", ticker = "", intro_key = "own_detail_intro_tw_ndf"),
    list(pattern = "(?i)\\bvanguard\\b", ticker = "", intro_key = "own_detail_intro_private_mgr"),
    list(pattern = "(?i)fidelity\\s*(management|investments)?", ticker = "", intro_key = "own_detail_intro_private_mgr")
  )
}

.ownership_resolve_institution_ticker <- function(holder_name) {
  nm <- trimws(as.character(holder_name %||% "")[1])
  if (!nzchar(nm)) {
    return(list(ticker = "", intro_key = "own_detail_intro_generic", matched = FALSE))
  }
  for (row in .ownership_institution_ticker_map()) {
    if (grepl(row$pattern, nm, perl = TRUE)) {
      return(list(
        ticker = as.character(row$ticker %||% "")[1],
        intro_key = as.character(row$intro_key %||% "own_detail_intro_generic")[1],
        matched = TRUE
      ))
    }
  }
  # Rare: name is already a ticker token
  maybe_tk <- toupper(gsub("[^A-Z0-9.\\-]", "", nm))
  if (nzchar(maybe_tk) && nchar(maybe_tk) <= 8L &&
      grepl("^[A-Z]{1,5}(-[A-Z])?$", maybe_tk)) {
    return(list(ticker = maybe_tk, intro_key = "own_detail_intro_yahoo", matched = TRUE))
  }
  list(ticker = "", intro_key = "own_detail_intro_generic", matched = FALSE)
}

.ownership_cross_hold_match <- function(holder_names, subject_ticker,
                                        subject_aliases = character()) {
  names_v <- trimws(as.character(holder_names %||% character()))
  if (!length(names_v)) {
    return(list(status = "no", evidence = "empty_top5", credibility = "unavailable"))
  }
  tk <- toupper(trimws(as.character(subject_ticker %||% "")[1]))
  bare <- gsub("\\.(TW|TWO)$", "", tk)
  aliases <- unique(c(
    bare,
    tk,
    trimws(as.character(subject_aliases %||% character()))
  ))
  aliases <- aliases[nzchar(aliases)]
  names_l <- tolower(names_v)
  for (a in aliases) {
    # Avoid ultra-short numeric false positives (e.g. "2") — require ≥3 chars
    if (nchar(a) < 3L) next
    hit <- grepl(tolower(a), names_l, fixed = TRUE)
    if (any(hit)) {
      return(list(
        status = "yes",
        evidence = names_v[which(hit)[1]],
        credibility = "yahoo"
      ))
    }
  }
  list(status = "no", evidence = "not_in_top5", credibility = "yahoo")
}

.ownership_top5_from_holders <- function(holders) {
  empty <- data.frame(
    rank = integer(), name = character(), pct = numeric(), shares = numeric(),
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(holders) || !nrow(holders)) return(empty)
  top <- utils::head(holders, 5)
  data.frame(
    rank = seq_len(nrow(top)),
    name = top$name,
    pct = top$pct,
    shares = top$shares,
    stringsAsFactors = FALSE
  )
}

#' Build institution detail panel; honest empty when undisclosed.
#' @param holder_name selected holder
#' @param subject_ticker currently searched company
#' @param subject_holders top holders of subject (for reverse cross-hold hint only when names match)
ownership_institution_detail <- function(holder_name, subject_ticker = "",
                                         subject_holders = NULL,
                                         market = "US",
                                         subject_aliases = character()) {
  nm <- trimws(as.character(holder_name %||% "")[1])
  tk <- toupper(trimws(as.character(subject_ticker %||% "")[1]))
  detail <- list(
    name = nm,
    intro = "",
    intro_key = "own_detail_intro_generic",
    mapped_ticker = "",
    top5 = data.frame(
      rank = integer(), name = character(), pct = numeric(), shares = numeric(),
      stringsAsFactors = FALSE
    ),
    cross_hold = list(
      status = "unknown",
      evidence = "",
      credibility = "unavailable"
    ),
    source = "",
    credibility = "unavailable",
    ok = FALSE
  )

  if (!nzchar(nm)) return(detail)

  detail$intro_key <- "own_detail_intro_generic"
  detail$source <- "disclosure_gap"
  detail$credibility <- "unavailable"

  # Reverse presence: subject lists this institution (not true cross-hold alone)
  if (is.data.frame(subject_holders) && nrow(subject_holders) > 0 &&
      "name" %in% names(subject_holders)) {
    if (any(trimws(as.character(subject_holders$name)) == nm)) {
      detail$cross_hold$status <- "unknown"
      detail$cross_hold$evidence <- "listed_as_holder_only"
      detail$cross_hold$credibility <- "estimate"
    }
  }

  resolved <- .ownership_resolve_institution_ticker(nm)
  if (isTRUE(resolved$matched)) {
    detail$intro_key <- resolved$intro_key
    detail$mapped_ticker <- resolved$ticker %||% ""
    if (!nzchar(detail$mapped_ticker)) {
      # Known private / gov fund — intro only
      detail$source <- "name_map_no_public_ticker"
      detail$credibility <- if (grepl("tw_", detail$intro_key)) "official" else "estimate"
      detail$ok <- TRUE
      return(detail)
    }
  } else if (identical(toupper(as.character(market)[1]), "TW") || grepl("\\.(TW|TWO)$", tk)) {
    if (grepl("銀行|投信|保險|人壽|金控|證券", nm)) {
      detail$intro_key <- "own_detail_intro_tw_institution"
      detail$source <- "MOPS t187ap11"
      detail$credibility <- "official"
      detail$ok <- TRUE
    }
  }

  fetch_tk <- detail$mapped_ticker
  if (!nzchar(fetch_tk)) {
    maybe_tk <- toupper(gsub("[^A-Z0-9.\\-]", "", nm))
    if (nzchar(maybe_tk) && nchar(maybe_tk) <= 8L &&
        grepl("^[A-Z]{1,5}(-[A-Z])?$", maybe_tk) &&
        !identical(maybe_tk, gsub("\\.(TW|TWO)$", "", tk))) {
      fetch_tk <- maybe_tk
    }
  }

  if (nzchar(fetch_tk) && !identical(toupper(fetch_tk), tk)) {
    raw <- .ownership_yahoo_live(fetch_tk)
    y <- .ownership_from_yahoo(raw, fetch_tk)
    if (isTRUE(y$ok) && is.data.frame(y$holders) && nrow(y$holders) > 0) {
      detail$top5 <- .ownership_top5_from_holders(y$holders)
      detail$ok <- TRUE
      detail$source <- paste0("Yahoo Finance quoteSummary (", fetch_tk, ")")
      detail$credibility <- "yahoo"
      if (!nzchar(detail$intro_key) ||
          identical(detail$intro_key, "own_detail_intro_generic") ||
          identical(detail$intro_key, "own_detail_intro_tw_institution")) {
        detail$intro_key <- if (grepl("\\.TW", fetch_tk)) {
          "own_detail_intro_tw_mapped"
        } else {
          "own_detail_intro_mapped"
        }
      }
      detail$cross_hold <- .ownership_cross_hold_match(
        detail$top5$name, tk, subject_aliases = subject_aliases
      )
      return(detail)
    }
    # TW listed 法人: MOPS insider list of the mapped company as official top-5 proxy
    if (grepl("\\.(TW|TWO)$", fetch_tk)) {
      code <- .ownership_issuer_code(fetch_tk)
      ins <- tryCatch(.ownership_tw_insiders(code), error = function(e) NULL)
      if (is.data.frame(ins) && nrow(ins) > 0) {
        detail$top5 <- .ownership_top5_from_holders(ins)
        detail$ok <- TRUE
        detail$source <- paste0("MOPS t187ap11 (", fetch_tk, ")")
        detail$credibility <- "official"
        detail$intro_key <- "own_detail_intro_tw_mapped"
        detail$cross_hold <- .ownership_cross_hold_match(
          detail$top5$name, tk, subject_aliases = subject_aliases
        )
        # Official list may not cover free-float mutual stakes — mark unknown if no hit
        if (identical(detail$cross_hold$status, "no")) {
          detail$cross_hold <- list(
            status = "unknown",
            evidence = "mops_insiders_only_no_match",
            credibility = "official"
          )
        }
        return(detail)
      }
    }
  }

  detail
}

#' Fetch OwnershipSnapshot for a ticker (TW or US).
fetch_ownership_structure <- function(ticker, market = NULL) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  snap <- .ownership_empty_snapshot(tk, market %||% "US")
  if (!nzchar(tk)) return(snap)

  mode <- if (!is.null(market) && nzchar(as.character(market)[1])) {
    toupper(as.character(market)[1])
  } else if (grepl("\\.(TW|TWO)$", tk)) {
    "TW"
  } else {
    "US"
  }
  snap$market <- mode

  yahoo <- .ownership_from_yahoo(.ownership_yahoo_live(tk), tk)
  snap$structure <- yahoo$structure
  snap$holders <- yahoo$holders
  snap$holders_kind <- yahoo$holders_kind %||% .ownership_holders_kind_from_df(yahoo$holders)
  snap$holders_source_ticker <- if (.ownership_holders_has_institution(yahoo$holders)) tk else ""
  snap$flow <- yahoo$flow
  snap$sources <- yahoo$sources
  snap$notes <- yahoo$notes
  snap$as_of <- yahoo$as_of
  snap$ok <- isTRUE(yahoo$ok)
  if (identical(snap$holders_kind, "free_float")) {
    snap$notes <- c(snap$notes, "holders_free_float_yahoo")
  }

  if (identical(mode, "TW")) {
    code <- .ownership_issuer_code(tk)
    q <- tryCatch(.ownership_tw_qfiis_row(code), error = function(e) NULL)
    if (!is.null(q) && is.finite(q$foreign_pct)) {
      snap$structure <- list(
        list(
          key = "foreign", pct = q$foreign_pct, credibility = "official",
          source = q$source
        )
      )
      # Keep Yahoo float-institution slice as supplemental when disclosed
      for (sl in yahoo$structure %||% list()) {
        if (identical(sl$key, "institutions_float")) {
          snap$structure[[length(snap$structure) + 1L]] <- sl
        }
      }
      if (isTRUE(yahoo$ok) && length(yahoo$structure)) {
        snap$notes <- c(snap$notes, "yahoo_structure_suppressed_for_tw_official")
      }
      snap$as_of <- q$as_of
      snap$sources <- unique(c(snap$sources, q$source))
      snap$ok <- TRUE
    }
    t86 <- tryCatch(.ownership_tw_t86_row(code), error = function(e) NULL)
    if (!is.null(t86) && is.finite(t86$net_shares)) {
      snap$flow <- list(
        net_shares = t86$net_shares,
        buy_shares = t86$buy_shares,
        sell_shares = t86$sell_shares,
        label = t86$label,
        as_of = t86$as_of,
        source = t86$source,
        credibility = t86$credibility
      )
      snap$sources <- unique(c(snap$sources, t86$source))
      snap$ok <- TRUE
    }

    # Free-float–relevant top-10: prefer Yahoo institution/fund (local or ADR)
    float_hit <- NULL
    if (.ownership_holders_has_institution(snap$holders)) {
      float_hit <- list(
        holders = snap$holders,
        holders_kind = "free_float",
        source_ticker = tk,
        sources = snap$sources,
        notes = "holders_free_float_yahoo",
        ok = TRUE
      )
    } else {
      float_hit <- tryCatch(
        .ownership_yahoo_float_holders(tk, market = "TW"),
        error = function(e) NULL
      )
    }

    ins <- tryCatch(.ownership_tw_insiders(code), error = function(e) NULL)
    if (!is.null(float_hit) && isTRUE(float_hit$ok) &&
        is.data.frame(float_hit$holders) && nrow(float_hit$holders) > 0) {
      snap$holders <- float_hit$holders
      snap$holders_kind <- "free_float"
      snap$holders_source_ticker <- as.character(float_hit$source_ticker %||% tk)[1]
      snap$sources <- unique(c(snap$sources, float_hit$sources %||% character()))
      snap$notes <- unique(c(
        setdiff(snap$notes, "holders_free_float_yahoo"),
        float_hit$notes %||% "holders_free_float_yahoo"
      ))
      # Drop stale "insiders only" note if we upgraded to free-float
      snap$notes <- setdiff(snap$notes, "tw_holders_are_insiders_not_free_float_top10")
      snap$ok <- TRUE
      # Still fold MOPS insider aggregate into structure when issued shares known
      if (is.data.frame(ins) && nrow(ins) > 0 &&
          !is.null(q) && is.finite(q$issued_shares) && q$issued_shares > 0) {
        insider_pct <- sum(ins$shares, na.rm = TRUE) / q$issued_shares
        if (is.finite(insider_pct) && insider_pct > 0) {
          snap$structure[[length(snap$structure) + 1L]] <- list(
            key = "insiders",
            pct = min(insider_pct, 1),
            credibility = "official",
            source = "MOPS t187ap11 ÷ MI_QFIIS issued shares"
          )
          foreign_pct <- if (!is.null(q) && is.finite(q$foreign_pct)) q$foreign_pct else NA_real_
          if (is.finite(foreign_pct)) {
            other <- max(0, 1 - foreign_pct - insider_pct)
            if (other > 0.001) {
              snap$structure[[length(snap$structure) + 1L]] <- list(
                key = "other",
                pct = other,
                credibility = "estimate",
                source = "residual (1 − foreign − disclosed insiders)"
              )
            }
          }
        }
        snap$sources <- unique(c(snap$sources, "MOPS t187ap11"))
        snap$notes <- c(snap$notes, "tw_insiders_used_for_structure_only")
      }
    } else if (is.data.frame(ins) && nrow(ins) > 0) {
      # Honest fallback: MOPS 董監／內部人 — not free-float top-10
      if (!is.null(q) && is.finite(q$issued_shares) && q$issued_shares > 0) {
        ins$pct <- ins$shares / q$issued_shares
        insider_pct <- sum(ins$shares, na.rm = TRUE) / q$issued_shares
        if (is.finite(insider_pct) && insider_pct > 0) {
          snap$structure[[length(snap$structure) + 1L]] <- list(
            key = "insiders",
            pct = min(insider_pct, 1),
            credibility = "official",
            source = "MOPS t187ap11 ÷ MI_QFIIS issued shares"
          )
          foreign_pct <- if (!is.null(q) && is.finite(q$foreign_pct)) q$foreign_pct else NA_real_
          if (is.finite(foreign_pct)) {
            other <- max(0, 1 - foreign_pct - insider_pct)
            if (other > 0.001) {
              snap$structure[[length(snap$structure) + 1L]] <- list(
                key = "other",
                pct = other,
                credibility = "estimate",
                source = "residual (1 − foreign − disclosed insiders)"
              )
            }
          }
        }
      }
      snap$holders <- ins
      snap$holders_kind <- "insider"
      snap$holders_source_ticker <- ""
      snap$sources <- unique(c(snap$sources, "MOPS t187ap11"))
      snap$notes <- c(snap$notes, "tw_holders_are_insiders_not_free_float_top10")
      snap$ok <- TRUE
    } else if (!isTRUE(yahoo$ok) || !nrow(snap$holders)) {
      snap$notes <- c(snap$notes, "tw_insider_holders_unavailable")
      snap$holders_kind <- "empty"
    }
  }

  vol <- .ownership_fetch_volume(tk, "5y", market = mode)
  snap$volume <- if (is.data.frame(vol) && "Date" %in% names(vol) && "Volume" %in% names(vol)) {
    vol[, c("Date", "Volume"), drop = FALSE]
  } else {
    data.frame(Date = as.Date(character()), Volume = numeric(), stringsAsFactors = FALSE)
  }
  if (nrow(snap$volume) > 0) {
    vol_src <- if ("source" %in% names(vol) && nzchar(as.character(vol$source[1] %||% "")[1])) {
      as.character(vol$source[1])
    } else {
      "Yahoo price history"
    }
    snap$sources <- unique(c(snap$sources, vol_src))
    if (identical(vol_src, "TWSE STOCK_DAY")) {
      snap$notes <- c(snap$notes, "volume_from_twse_stock_day")
    }
    snap$ok <- TRUE
  } else {
    snap$notes <- c(snap$notes, "volume_history_unavailable")
  }

  snap
}

#' Fast-tier memoized ownership fetch (T86/QFIIS change daily; ≤15m reuse is fine).
cached_fetch_ownership_structure <- if (
  requireNamespace("memoise", quietly = TRUE) &&
    exists(".ynow_fast_cache", inherits = TRUE)
) {
  tryCatch(
    memoise::memoise(fetch_ownership_structure, cache = .ynow_fast_cache),
    error = function(e) fetch_ownership_structure
  )
} else {
  fetch_ownership_structure
}

.ownership_fmt_shares <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return("—")
  ax <- abs(x)
  if (ax >= 1e9) return(sprintf("%.2fB", x / 1e9))
  if (ax >= 1e6) return(sprintf("%.2fM", x / 1e6))
  if (ax >= 1e3) return(sprintf("%.1fK", x / 1e3))
  format(round(x), big.mark = ",")
}

.ownership_fmt_pct <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return("—")
  if (x > 1.5) return(sprintf("%.1f%%", x))
  sprintf("%.1f%%", 100 * x)
}

.ownership_structure_label <- function(key, loc = "en") {
  k <- switch(
    as.character(key %||% "")[1],
    "institutions" = "own_slice_institutions",
    "institutions_float" = "own_slice_institutions_float",
    "insiders" = "own_slice_insiders",
    "foreign" = "own_slice_foreign",
    "other" = "own_slice_other",
    "own_slice_other"
  )
  tryCatch(ui_str(k, loc), error = function(e) as.character(key))
}

.ynow_ownership_waiting_ui <- function(loc = "en") {
  tags$div(
    class = "ynow-own",
    tags$p(class = "ynow-own-waiting", ui_str("own_waiting", loc))
  )
}

#' Top KPIs + intro (volume chart is a static plotlyOutput sibling).
.ynow_ownership_top_ui <- function(snap, loc = "en") {
  loc <- tryCatch(normalize_ui_locale(loc), error = function(e) "en")
  if (is.null(snap) || !is.list(snap) || !nzchar(snap$ticker %||% "")) {
    return(.ynow_ownership_waiting_ui(loc))
  }

  struct_cells <- lapply(snap$structure %||% list(), function(sl) {
    tags$div(
      class = "ynow-own-kpi-cell",
      tags$div(class = "ynow-own-kpi-label", .ownership_structure_label(sl$key, loc)),
      tags$div(class = "ynow-own-kpi-value", .ownership_fmt_pct(sl$pct)),
      tags$div(
        class = "ynow-own-kpi-cred",
        sprintf("%s · %s", sl$credibility %||% "", sl$source %||% "")
      )
    )
  })
  if (!length(struct_cells)) {
    struct_cells <- list(tags$div(
      class = "ynow-own-kpi-cell ynow-own-empty",
      tags$div(class = "ynow-own-kpi-label", ui_str("own_kpi_structure", loc)),
      tags$div(class = "ynow-own-kpi-value", "—"),
      tags$div(class = "ynow-own-kpi-cred", ui_str("own_cred_unavailable", loc))
    ))
  }

  flow <- snap$flow %||% list()
  net <- suppressWarnings(as.numeric(flow$net_shares)[1])
  flow_val <- if (is.finite(net)) {
    paste0(if (net > 0) "+" else "", .ownership_fmt_shares(net))
  } else {
    "—"
  }
  flow_cred <- if (identical(flow$credibility, "unavailable") || !is.finite(net)) {
    ui_str("own_cred_unavailable", loc)
  } else {
    sprintf(
      "%s · %s%s",
      flow$credibility %||% "",
      flow$source %||% "",
      if (nzchar(flow$as_of %||% "")) paste0(" @ ", flow$as_of) else ""
    )
  }

  tagList(
    tags$p(
      style = "margin:0 0 12px 0; color:#666; font-size:13px; line-height:1.5;",
      ui_str("own_intro", loc)
    ),
    fluidRow(
      column(
        width = 8,
        tags$div(
          class = "ynow-own-kpi-row",
          tags$div(
            class = "ynow-own-kpi-block",
            tags$h4(ui_str("own_kpi_structure", loc)),
            tags$div(class = "ynow-own-kpi-grid", struct_cells)
          )
        )
      ),
      column(
        width = 4,
        tags$div(
          class = "ynow-own-kpi-block",
          tags$h4(ui_str("own_kpi_flow", loc)),
          tags$div(class = "ynow-own-kpi-value ynow-own-flow-value", flow_val),
          tags$div(class = "ynow-own-kpi-cred", flow_cred),
          tags$p(
            class = "ynow-own-muted",
            style = "margin-top:6px;font-size:12px;",
            ui_str("own_kpi_flow_help", loc)
          )
        )
      )
    ),
    tags$hr(),
    tags$h4(ui_str("own_volume_title", loc))
  )
}

#' Holders table + institution detail (right panel).
#' Default: collapsible box starts collapsed; pass collapsed = FALSE to keep open
#' across institution detail re-renders. Ticker / search resets should pass TRUE.
.ynow_ownership_holders_ui <- function(snap, detail = NULL, loc = "en",
                                       selected_name = "",
                                       collapsed = TRUE) {
  loc <- tryCatch(normalize_ui_locale(loc), error = function(e) "en")
  if (is.null(snap) || !is.list(snap) || !nzchar(snap$ticker %||% "")) {
    return(NULL)
  }

  holders <- snap$holders
  holder_rows <- list()
  if (is.data.frame(holders) && nrow(holders) > 0) {
    for (i in seq_len(min(nrow(holders), 10L))) {
      nm <- holders$name[i]
      clickable <- isTRUE(holders$clickable[i])
      selected <- identical(nm, selected_name)
      label <- nm
      if (clickable) {
        label <- tags$a(
          href = "#",
          class = paste(
            "ynow-own-holder-link",
            if (selected) "ynow-own-holder-selected" else ""
          ),
          `data-own-holder` = nm,
          onclick = sprintf(
            "Shiny.setInputValue('own_holder_click', {name: %s, nonce: Date.now()}, {priority: 'event'}); return false;",
            jsonlite::toJSON(nm, auto_unbox = TRUE)
          ),
          nm
        )
      }
      holder_rows[[length(holder_rows) + 1L]] <- tags$tr(
        tags$td(as.character(holders$rank[i])),
        tags$td(label),
        tags$td(.ownership_fmt_shares(holders$shares[i])),
        tags$td(.ownership_fmt_pct(holders$pct[i])),
        tags$td(holders$holder_type[i]),
        tags$td(sprintf("%s / %s", holders$credibility[i], holders$source[i]))
      )
    }
  }

  if (!is.null(detail) && nzchar(detail$name %||% "")) {
    top5 <- detail$top5
    top5_ui <- if (is.data.frame(top5) && nrow(top5) > 0) {
      tags$table(
        class = "table table-condensed ynow-own-top5",
        tags$thead(tags$tr(
          tags$th("#"), tags$th(ui_str("own_col_name", loc)),
          tags$th(ui_str("own_col_pct", loc)), tags$th(ui_str("own_col_shares", loc))
        )),
        tags$tbody(lapply(seq_len(nrow(top5)), function(i) {
          tags$tr(
            tags$td(as.character(top5$rank[i])),
            tags$td(top5$name[i]),
            tags$td(.ownership_fmt_pct(top5$pct[i])),
            tags$td(.ownership_fmt_shares(top5$shares[i]))
          )
        }))
      )
    } else {
      tags$p(class = "ynow-own-muted", ui_str("own_detail_top5_empty", loc))
    }
    ch <- detail$cross_hold %||% list()
    ch_status <- as.character(ch$status %||% "unknown")[1]
    ch_txt <- switch(
      ch_status,
      "yes" = ui_str("own_cross_yes", loc),
      "no" = ui_str("own_cross_no", loc),
      ui_str("own_cross_unknown", loc)
    )
    intro_txt <- tryCatch(
      ui_str(detail$intro_key %||% "own_detail_intro_generic", loc),
      error = function(e) ui_str("own_detail_intro_generic", loc)
    )
    detail_ui <- tags$div(
      class = "ynow-own-detail",
      tags$h4(ui_str("own_detail_title", loc)),
      tags$p(tags$b(detail$name)),
      tags$p(class = "ynow-own-muted", intro_txt),
      tags$h5(ui_str("own_detail_top5_title", loc)),
      top5_ui,
      tags$h5(ui_str("own_cross_title", loc)),
      tags$p(ch_txt),
      tags$p(
        class = "ynow-own-kpi-cred",
        sprintf("%s · %s", detail$credibility %||% "", detail$source %||% "")
      )
    )
  } else {
    detail_ui <- tags$div(
      class = "ynow-own-detail ynow-own-detail-idle",
      tags$p(class = "ynow-own-muted", ui_str("own_detail_idle", loc))
    )
  }

  note_ui <- NULL
  if (length(snap$notes)) {
    note_keys_seen <- character()
    note_items <- list()
    for (n in snap$notes) {
      key <- switch(
        as.character(n)[1],
        "tw_holders_are_insiders_not_free_float_top10" = "own_note_tw_insiders",
        "holders_free_float_yahoo" = "own_note_float_yahoo",
        "holders_free_float_yahoo_adr" = "own_note_float_yahoo_adr",
        "tw_insiders_used_for_structure_only" = "own_note_tw_insiders_structure",
        "volume_history_unavailable" = "own_note_volume_empty",
        "volume_from_twse_stock_day" = "own_note_volume_twse",
        "tw_insider_holders_unavailable" = "own_note_holders_empty",
        NULL
      )
      if (is.null(key)) {
        # Skip internal/noisy notes (yahoo_structure_suppressed…, yahoo:rate…)
        if (grepl("^(yahoo_structure_suppressed|yahoo:)", as.character(n)[1])) next
        note_items[[length(note_items) + 1L]] <- tags$li(as.character(n))
      } else if (!(key %in% note_keys_seen)) {
        note_keys_seen <- c(note_keys_seen, key)
        note_items[[length(note_items) + 1L]] <- tags$li(ui_str(key, loc))
      }
    }
    if (length(note_items)) {
      note_ui <- tags$ul(class = "ynow-own-notes", note_items)
    }
  }

  holders_title_key <- if (identical(snap$holders_kind %||% "", "free_float")) {
    "own_holders_title_float"
  } else if (identical(snap$holders_kind %||% "", "insider")) {
    "own_holders_title_insider"
  } else {
    "own_holders_title"
  }

  body <- tagList(
    fluidRow(
      column(
        width = 7,
        if (length(holder_rows)) {
          tags$div(
            class = "ynow-own-holders-wrap",
            tags$table(
              class = "table table-striped table-condensed ynow-own-holders",
              tags$thead(tags$tr(
                tags$th("#"),
                tags$th(ui_str("own_col_name", loc)),
                tags$th(ui_str("own_col_shares", loc)),
                tags$th(ui_str("own_col_pct", loc)),
                tags$th(ui_str("own_col_type", loc)),
                tags$th(ui_str("own_col_source", loc))
              )),
              tags$tbody(holder_rows)
            )
          )
        } else {
          tags$p(class = "ynow-own-muted", ui_str("own_holders_empty", loc))
        },
        note_ui
      ),
      column(width = 5, detail_ui)
    ),
    tags$p(
      class = "ynow-own-muted",
      style = "margin-top:10px;font-size:11px;",
      paste(ui_str("own_sources_label", loc), paste(unique(snap$sources), collapse = " · "))
    )
  )

  # Collapsible AdminLTE box: default collapsed on first paint / ticker change.
  # Callers pass collapsed = FALSE after the user expands so institution clicks
  # do not remount the box shut. Prefer shinydashboard::box — never graphics::box.
  box_fn <- if (requireNamespace("shinydashboard", quietly = TRUE)) {
    shinydashboard::box
  } else {
    NULL
  }
  if (is.null(box_fn)) {
    return(tagList(
      tags$hr(),
      tags$h4(ui_str(holders_title_key, loc)),
      body
    ))
  }
  # width = NULL — never width = 12. A col-sm-12 wrapper floats inside the
  # Board / CB tab-pane (no parent .row) and leaks into .nav-tabs-custom, which
  # parks the Board / CB tab chrome at the bottom when the box is collapsed.
  bx <- box_fn(
    title = ui_str(holders_title_key, loc),
    width = NULL,
    status = "primary",
    solidHeader = FALSE,
    collapsible = TRUE,
    collapsed = isTRUE(collapsed),
    body
  )
  # shinydashboard::box(width=NULL) → outer wrapper div > .box; tag .box for JS.
  box_node <- bx
  if (inherits(bx, "shiny.tag") && length(bx$children) >= 1L &&
      inherits(bx$children[[1]], "shiny.tag") &&
      grepl("\\bbox\\b", as.character(bx$children[[1]]$attribs$class %||% ""))) {
    box_node <- bx$children[[1]]
  }
  if (inherits(box_node, "shiny.tag")) {
    cls <- as.character(box_node$attribs$class %||% "")
    if (!grepl("ynow-own-holders-box", cls, fixed = TRUE)) {
      cls <- paste(trimws(cls), "ynow-own-holders-box")
    }
    box_node$attribs$id <- "own_holders_box"
    box_node$attribs$class <- cls
    if (identical(box_node, bx)) {
      bx <- box_node
    } else {
      bx$children[[1]] <- box_node
    }
  }
  tagList(tags$hr(), bx)
}

#' Combined panel helper (tests / one-shot render).
.ynow_ownership_panel_ui <- function(snap, detail = NULL, loc = "en",
                                     selected_name = "",
                                     collapsed = TRUE) {
  tagList(
    .ynow_ownership_top_ui(snap, loc = loc),
    .ynow_ownership_holders_ui(
      snap, detail = detail, loc = loc, selected_name = selected_name,
      collapsed = collapsed
    )
  )
}
