# ==========================================
# Earnings-call / 法說會 transcript catalog
# ==========================================
# Past-call index + fetch-by-id for BBLab Company orders & market concentration.
# Reuses Fugle / FinmoConf / FMP / SA / SEC harvest; does not invent amounts.
# Catalog rows are metadata only (no full transcript bodies in the UI).

.ynow_ec_cat_chr <- function(x, default = "") {
  v <- as.character(x %||% default)[1]
  if (!nzchar(v) || is.na(v)) default else v
}

.ynow_ec_cat_empty <- function() {
  list(
    market = "",
    ticker = "",
    rows = list(),
    default_id = "",
    source_note = "empty"
  )
}

#' Stable catalog row id (source + published + url hash).
ynow_ec_catalog_row_id <- function(source_id, published = "", url = "",
                                   year = NULL, quarter = NULL) {
  src <- .ynow_ec_cat_chr(source_id, "unknown")
  pub <- .ynow_ec_cat_chr(published)
  u <- .ynow_ec_cat_chr(url)
  yq <- ""
  if (!is.null(year) && is.finite(suppressWarnings(as.numeric(year)))) {
    yq <- paste0("y", as.integer(year))
    if (!is.null(quarter) && is.finite(suppressWarnings(as.numeric(quarter)))) {
      yq <- paste0(yq, "q", as.integer(quarter))
    }
  }
  key <- paste(c(src, pub, yq, u), collapse = "|")
  # Stable short id without requiring the digest package.
  h <- abs(sum(utf8ToInt(enc2utf8(key)) * seq_along(utf8ToInt(enc2utf8(key))))) %% 1e8
  tag <- sprintf("%08d", as.integer(h))
  paste(c(src, if (nzchar(pub)) pub else if (nzchar(yq)) yq else "x", tag), collapse = "::")
}

#' Normalize one catalog row.
ynow_ec_catalog_row <- function(source_id,
                                published = "",
                                title = "",
                                url = "",
                                label = NULL,
                                year = NULL,
                                quarter = NULL,
                                lang = "",
                                credibility = "",
                                id = NULL) {
  src <- .ynow_ec_cat_chr(source_id, "unknown")
  pub <- .ynow_ec_cat_chr(published)
  u <- .ynow_ec_cat_chr(url)
  tit <- .ynow_ec_cat_chr(title)
  if (!nzchar(tit)) {
    tit <- if (nzchar(pub)) paste(pub, src) else src
  }
  lab <- .ynow_ec_cat_chr(label)
  if (!nzchar(lab)) {
    lab <- if (nzchar(pub)) pub else tit
  }
  rid <- .ynow_ec_cat_chr(id)
  if (!nzchar(rid)) {
    rid <- ynow_ec_catalog_row_id(src, pub, u, year = year, quarter = quarter)
  }
  list(
    id = rid,
    source_id = src,
    published = pub,
    title = tit,
    label = lab,
    url = u,
    year = if (is.null(year)) NA_integer_ else suppressWarnings(as.integer(year)[1]),
    quarter = if (is.null(quarter)) NA_integer_ else suppressWarnings(as.integer(quarter)[1]),
    lang = .ynow_ec_cat_chr(lang),
    credibility = .ynow_ec_cat_chr(credibility)
  )
}

.ynow_ec_catalog_sort <- function(rows) {
  if (!length(rows)) return(list())
  pubs <- vapply(rows, function(r) .ynow_ec_cat_chr(r$published), character(1))
  # ISO dates / yyyy-mm-dd sort correctly as strings; blanks sink.
  ord <- order(pubs, decreasing = TRUE, na.last = TRUE)
  # Prefer blank-published last among ties already handled; force empty dates last.
  empty <- !nzchar(pubs)
  if (any(empty) && !all(empty)) {
    non <- which(!empty)
    emp <- which(empty)
    non_ord <- order(pubs[non], decreasing = TRUE)
    ord <- c(non[non_ord], emp)
  }
  rows[ord]
}

.ynow_ec_catalog_dedupe <- function(rows) {
  if (!length(rows)) return(list())
  seen <- character(0)
  out <- list()
  for (r in rows) {
    key <- .ynow_ec_cat_chr(r$id)
    if (!nzchar(key) || key %in% seen) next
    seen <- c(seen, key)
    out[[length(out) + 1L]] <- r
  }
  out
}

#' TW catalog: Fugle topic posts + FinmoConf presentation dates (newest first).
ynow_tw_earnings_call_catalog <- function(ticker, max_rows = 12L) {
  tk <- toupper(trimws(.ynow_ec_cat_chr(ticker)))
  bare <- sub("\\.(TW|TWO)$", "", tk, ignore.case = TRUE)
  bare <- gsub("[^0-9A-Za-z]", "", bare)
  out <- .ynow_ec_cat_empty()
  out$market <- "TW"
  out$ticker <- if (nzchar(tk)) tk else bare
  if (!nzchar(bare)) return(out)

  rows <- list()
  max_n <- max(1L, as.integer(max_rows)[1])

  # 1) Fugle topic index — many posts for the bare ticker.
  if (exists("ynow_fugle_earnings_call_catalog_rows", mode = "function")) {
    fr <- tryCatch(
      ynow_fugle_earnings_call_catalog_rows(bare, max_rows = max_n),
      error = function(e) list()
    )
    if (length(fr)) rows <- c(rows, fr)
  }

  # 2) FinmoConf company page — one row per presentation date (zh preferred).
  if (exists("ynow_finmoconf_earnings_call_catalog_rows", mode = "function")) {
    mr <- tryCatch(
      ynow_finmoconf_earnings_call_catalog_rows(bare, max_rows = max_n),
      error = function(e) list()
    )
    if (length(mr)) rows <- c(rows, mr)
  }

  rows <- .ynow_ec_catalog_dedupe(.ynow_ec_catalog_sort(rows))
  if (length(rows) > max_n) rows <- rows[seq_len(max_n)]
  out$rows <- rows
  out$default_id <- if (length(rows)) .ynow_ec_cat_chr(rows[[1]]$id) else ""
  out$source_note <- if (length(rows)) "fugle_finmoconf" else "empty"
  out
}

#' US catalog: FMP transcript dates (when keyed) then Seeking Alpha index links.
ynow_us_earnings_call_catalog <- function(ticker, max_rows = 12L) {
  tk <- toupper(trimws(.ynow_ec_cat_chr(ticker)))
  tk <- sub("\\.(US)$", "", tk, ignore.case = TRUE)
  out <- .ynow_ec_cat_empty()
  out$market <- "US"
  out$ticker <- tk
  if (!nzchar(tk) || grepl("\\.(TW|TWO)$", ticker, ignore.case = TRUE)) {
    out$source_note <- "tw_leave_as_is"
    return(out)
  }

  rows <- list()
  max_n <- max(1L, as.integer(max_rows)[1])

  if (exists("ynow_us_ec_fmp_catalog_rows", mode = "function")) {
    fr <- tryCatch(
      ynow_us_ec_fmp_catalog_rows(tk, max_rows = max_n),
      error = function(e) list()
    )
    if (length(fr)) rows <- c(rows, fr)
  }

  if (length(rows) < max_n && exists("ynow_sa_earnings_call_catalog_rows", mode = "function")) {
    sr <- tryCatch(
      ynow_sa_earnings_call_catalog_rows(tk, max_rows = max_n),
      error = function(e) list()
    )
    if (length(sr)) rows <- c(rows, sr)
  }

  # Lightweight SEC placeholder when nothing else listed (single latest 8-K path).
  if (!length(rows) && exists("ynow_us_ec_sec_catalog_row", mode = "function")) {
    sec <- tryCatch(ynow_us_ec_sec_catalog_row(tk), error = function(e) NULL)
    if (is.list(sec) && nzchar(.ynow_ec_cat_chr(sec$id))) rows <- list(sec)
  }

  rows <- .ynow_ec_catalog_dedupe(.ynow_ec_catalog_sort(rows))
  if (length(rows) > max_n) rows <- rows[seq_len(max_n)]
  out$rows <- rows
  out$default_id <- if (length(rows)) .ynow_ec_cat_chr(rows[[1]]$id) else ""
  out$source_note <- if (length(rows)) {
    unique(vapply(rows, function(r) .ynow_ec_cat_chr(r$source_id), character(1)))[1]
  } else {
    "empty"
  }
  out
}

#' Market-aware catalog for a ticker.
ynow_earnings_call_catalog <- function(ticker, max_rows = 12L) {
  tk <- .ynow_ec_cat_chr(ticker)
  if (grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE) || grepl("^[0-9]{4}$", tk)) {
    ynow_tw_earnings_call_catalog(tk, max_rows = max_rows)
  } else {
    ynow_us_earnings_call_catalog(tk, max_rows = max_rows)
  }
}

#' Pick catalog rows by id; empty selection → latest (default_id / first row).
ynow_ec_catalog_select_rows <- function(catalog, selected_ids = NULL) {
  if (!is.list(catalog)) return(list())
  rows <- catalog$rows %||% list()
  if (!length(rows)) return(list())
  ids <- as.character(unlist(selected_ids %||% character(0)))
  ids <- trimws(ids)
  ids <- ids[nzchar(ids)]
  if (!length(ids)) {
    def <- .ynow_ec_cat_chr(catalog$default_id)
    if (!nzchar(def)) def <- .ynow_ec_cat_chr(rows[[1]]$id)
    ids <- def
  }
  picked <- Filter(function(r) .ynow_ec_cat_chr(r$id) %in% ids, rows)
  if (!length(picked)) picked <- list(rows[[1]])
  .ynow_ec_catalog_sort(picked)
}

#' N-way merge of earnings-call result lists (newest primary for numbers).
ynow_ec_merge_results <- function(results) {
  results <- Filter(function(x) is.list(x) && isTRUE(x$ok %||% TRUE), results)
  if (!length(results)) return(NULL)
  if (length(results) == 1L) return(results[[1]])
  ranked <- if (exists("ynow_ec_rank_by_event_date", mode = "function")) {
    ynow_ec_rank_by_event_date(results)
  } else {
    results
  }
  merge_fn <- if (exists(".ynow_tw_ec_merge", mode = "function")) {
    .ynow_tw_ec_merge
  } else {
    function(a, b) a
  }
  merged <- ranked[[1]]
  for (i in seq_along(ranked)[-1]) {
    merged <- merge_fn(merged, ranked[[i]])
  }
  if (exists("ynow_ec_stamp_info_updated", mode = "function")) {
    merged <- ynow_ec_stamp_info_updated(merged)
  }
  merged$analysis_axis <- "latest_earnings_call"
  merged$analysis_axis_source_id <- .ynow_ec_cat_chr(ranked[[1]]$source_id)
  merged$analysis_axis_published <- .ynow_ec_cat_chr(ranked[[1]]$published)
  merged$selected_call_count <- length(ranked)
  merged$selected_call_ids <- vapply(
    ranked,
    function(r) .ynow_ec_cat_chr(r$catalog_id %||% r$id),
    character(1)
  )
  merged
}

#' Fetch one catalog row → facts list (or NULL).
ynow_ec_fetch_catalog_row <- function(ticker, row, extract_fun = NULL) {
  if (!is.list(row)) return(NULL)
  src <- .ynow_ec_cat_chr(row$source_id)
  tk <- .ynow_ec_cat_chr(ticker)
  if (!is.function(extract_fun) && exists(".ynow_us_ec_extract_facts", mode = "function")) {
    extract_fun <- .ynow_us_ec_extract_facts
  }
  got <- NULL
  if (identical(src, "fugle_earnings_memo") &&
        exists("ynow_fugle_earnings_call_fetch", mode = "function")) {
    got <- tryCatch(
      ynow_fugle_earnings_call_fetch(
        tk,
        extract_fun = extract_fun,
        article_url = .ynow_ec_cat_chr(row$url)
      ),
      error = function(e) NULL
    )
  } else if (identical(src, "finmoconf_presentation_pdf") &&
               exists("ynow_finmoconf_earnings_call_fetch", mode = "function")) {
    got <- tryCatch(
      ynow_finmoconf_earnings_call_fetch(
        tk,
        extract_fun = extract_fun,
        pdf_url = .ynow_ec_cat_chr(row$url),
        published = .ynow_ec_cat_chr(row$published),
        lang = .ynow_ec_cat_chr(row$lang)
      ),
      error = function(e) NULL
    )
  } else if (identical(src, "licensed_api") &&
               exists("ynow_us_ec_from_fmp_row", mode = "function")) {
    got <- tryCatch(
      ynow_us_ec_from_fmp_row(tk, row),
      error = function(e) NULL
    )
  } else if (identical(src, "seeking_alpha_html") &&
               exists("ynow_sa_earnings_call_fetch_url", mode = "function")) {
    got <- tryCatch(
      ynow_sa_earnings_call_fetch_url(
        tk,
        .ynow_ec_cat_chr(row$url),
        extract_fun = extract_fun,
        published = .ynow_ec_cat_chr(row$published),
        title = .ynow_ec_cat_chr(row$title)
      ),
      error = function(e) NULL
    )
  } else if (identical(src, "sec_8k") &&
               exists(".ynow_us_ec_from_sec_8k", mode = "function")) {
    got <- tryCatch(.ynow_us_ec_from_sec_8k(tk), error = function(e) NULL)
  }
  if (!is.list(got)) return(NULL)
  got$catalog_id <- .ynow_ec_cat_chr(row$id)
  got
}

#' Fetch + merge selected catalog rows into one disclosure-ready result.
ynow_ec_fetch_selected <- function(ticker, catalog, selected_ids = NULL,
                                   extract_fun = NULL) {
  picked <- ynow_ec_catalog_select_rows(catalog, selected_ids)
  if (!length(picked)) return(NULL)
  results <- lapply(picked, function(row) {
    ynow_ec_fetch_catalog_row(ticker, row, extract_fun = extract_fun)
  })
  results <- Filter(Negate(is.null), results)
  ynow_ec_merge_results(results)
}

#' Disclosure shape for order research from a (possibly merged) fetch result.
ynow_ec_to_disclosure <- function(fetched, market = "") {
  mkt <- toupper(.ynow_ec_cat_chr(market))
  if (!nzchar(mkt) && is.list(fetched)) {
    sid <- .ynow_ec_cat_chr(fetched$source_id)
    if (sid %in% c("fugle_earnings_memo", "finmoconf_presentation_pdf", "alphamemo_index")) {
      mkt <- "TW"
    } else {
      mkt <- "US"
    }
  }
  if (identical(mkt, "TW") && exists("ynow_tw_earnings_call_to_disclosure", mode = "function")) {
    return(ynow_tw_earnings_call_to_disclosure(fetched))
  }
  if (exists("ynow_us_earnings_call_to_disclosure", mode = "function")) {
    return(ynow_us_earnings_call_to_disclosure(fetched))
  }
  list()
}

#' UI helper: choice named vector for checkboxGroupInput / button labels.
ynow_ec_catalog_choice_labels <- function(catalog, locale = "en") {
  rows <- if (is.list(catalog)) catalog$rows %||% list() else list()
  if (!length(rows)) return(character(0))
  ids <- vapply(rows, function(r) .ynow_ec_cat_chr(r$id), character(1))
  labs <- vapply(rows, function(r) {
    pub <- .ynow_ec_cat_chr(r$published)
    src <- .ynow_ec_cat_chr(r$source_id)
    src_lab <- switch(
      src,
      fugle_earnings_memo = if (identical(locale, "zh-TW")) "Fugle 備忘錄" else "Fugle memo",
      finmoconf_presentation_pdf = if (identical(locale, "zh-TW")) "FinmoConf 簡報" else "FinmoConf PDF",
      licensed_api = "FMP",
      seeking_alpha_html = "Seeking Alpha",
      sec_8k = "SEC 8-K",
      src
    )
    if (nzchar(pub)) paste0(pub, " · ", src_lab) else src_lab
  }, character(1))
  stats::setNames(ids, labs)
}
