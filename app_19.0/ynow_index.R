# YNOW (US) and TYNOW (TW) — equal-weight 10
#
# Rank primary listings by market cap. Keep a name only when Piotroski F-Score
# is 8 or higher and Schilit screening has zero 警示. Take the first 10.
# Equal weight. Reconstitute once per calendar month (Asia/Taipei).
# No industry filter. Scan caps are engineering limits, not sector rules.
# US symbol YNOW. TW (上市／上櫃, no ETFs, no 興櫃) symbol TYNOW.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

YNOW_INDEX_N <- 10L
YNOW_INDEX_MIN_FSCORE <- 8
YNOW_INDEX_SCAN_CAP <- 80L
YNOW_INDEX_SCAN_CAP_TW <- 200L
YNOW_INDEX_SYMBOL <- "YNOW"
TYNOW_INDEX_SYMBOL <- "TYNOW"

.ynow_index_mem <- new.env(parent = emptyenv())

ynow_index_month_key <- function(as_of = Sys.time()) {
  if (inherits(as_of, "Date") && !inherits(as_of, "POSIXt")) {
    as_of <- as.POSIXct(paste(as.character(as_of)[1], "12:00:00"), tz = "Asia/Taipei")
  }
  format(as.POSIXlt(as_of, tz = "Asia/Taipei"), "%Y-%m")
}

ynow_index_passes <- function(f_score, n_alert, min_f = YNOW_INDEX_MIN_FSCORE) {
  fs <- suppressWarnings(as.numeric(f_score)[1])
  na_n <- suppressWarnings(as.numeric(n_alert)[1])
  floor_f <- suppressWarnings(as.numeric(min_f)[1])
  if (!is.finite(floor_f)) floor_f <- 8
  is.finite(fs) && fs >= floor_f - 1e-8 && is.finite(na_n) && na_n == 0
}

ynow_index_normalize_market <- function(market = "US") {
  m <- toupper(trimws(as.character(market %||% "US")[1]))
  if (m %in% c("TW", "TYNOW")) "TW" else "US"
}

ynow_index_symbol <- function(market = "US") {
  if (identical(ynow_index_normalize_market(market), "TW")) TYNOW_INDEX_SYMBOL else YNOW_INDEX_SYMBOL
}

ynow_index_scan_cap <- function(market = "US") {
  if (identical(ynow_index_normalize_market(market), "TW")) {
    YNOW_INDEX_SCAN_CAP_TW
  } else {
    YNOW_INDEX_SCAN_CAP
  }
}

ynow_index_screen_statements <- function(d_is, d_bs, d_cf, industry_key = NULL) {
  fs <- if (exists("compute_report_f_score", mode = "function")) {
    tryCatch(compute_report_f_score(d_is, d_bs, d_cf), error = function(e) NULL)
  } else {
    NULL
  }
  total <- if (is.null(fs)) NA_real_ else suppressWarnings(as.numeric(fs$total)[1])
  ev <- if (exists("evaluate_shenanigans", mode = "function")) {
    tryCatch(
      evaluate_shenanigans(d_is, d_bs, d_cf, industry_key = industry_key),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  n_alert <- if (is.null(ev) || !isTRUE(ev$ok)) NA_real_ else suppressWarnings(as.numeric(ev$n_alert)[1])
  list(
    pass = ynow_index_passes(total, n_alert),
    f_score = total,
    n_alert = n_alert
  )
}

#' Walk market-cap order. `screen_fn(ticker)` returns pass / f_score / n_alert.
ynow_index_select <- function(tickers, screen_fn, mcap = NULL, n = YNOW_INDEX_N,
                              scan_cap = YNOW_INDEX_SCAN_CAP) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  n <- max(1L, as.integer(n)[1])
  scan_cap <- max(n, as.integer(scan_cap)[1])
  tks <- utils::head(tks, scan_cap)
  picked <- list()
  scanned <- 0L
  for (tk in tks) {
    scanned <- scanned + 1L
    res <- tryCatch(screen_fn(tk), error = function(e) list(pass = FALSE, f_score = NA_real_, n_alert = NA_real_))
    if (!isTRUE(res$pass)) next
    cap <- NA_real_
    if (!is.null(mcap)) {
      cap <- suppressWarnings(as.numeric(mcap[[tk]])[1])
    }
    if (!is.finite(cap) && !is.null(res$market_cap)) {
      cap <- suppressWarnings(as.numeric(res$market_cap)[1])
    }
    picked[[length(picked) + 1L]] <- list(
      ticker = tk,
      market_cap = cap,
      f_score = suppressWarnings(as.numeric(res$f_score)[1]),
      n_alert = 0
    )
    if (length(picked) >= n) break
  }
  w <- if (length(picked)) rep(1 / length(picked), length(picked)) else numeric(0)
  for (i in seq_along(picked)) picked[[i]]$weight <- w[[i]]
  list(
    members = picked,
    scanned = scanned,
    n = length(picked),
    complete = length(picked) >= n,
    hit_scan_cap = scanned >= length(tks) && length(picked) < n && length(tks) >= scan_cap
  )
}

ynow_index_bh_equal <- function(price_map) {
  if (is.null(price_map) || !length(price_map)) return(NULL)
  dfs <- list()
  for (nm in names(price_map)) {
    d <- price_map[[nm]]
    if (!is.data.frame(d) || !all(c("Date", "Close") %in% names(d)) || nrow(d) < 2L) next
    dd <- data.frame(
      Date = as.Date(d$Date),
      Close = suppressWarnings(as.numeric(d$Close)),
      stringsAsFactors = FALSE
    )
    dd <- dd[is.finite(dd$Close) & !is.na(dd$Date), , drop = FALSE]
    if (nrow(dd) >= 2L) dfs[[nm]] <- dd
  }
  if (!length(dfs)) return(NULL)
  dates <- as.Date(dfs[[1]]$Date)
  for (d in dfs) dates <- intersect(dates, as.Date(d$Date))
  dates <- sort(unique(dates))
  if (length(dates) < 2L) return(NULL)
  mat <- vapply(dfs, function(d) {
    as.numeric(d$Close[match(dates, as.Date(d$Date))])
  }, numeric(length(dates)))
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1L)
  p0 <- mat[1, ]
  ok <- is.finite(p0) & p0 > 0
  if (!any(ok)) return(NULL)
  mat <- mat[, ok, drop = FALSE]
  p0 <- p0[ok]
  shares <- (1 / ncol(mat)) / p0
  level <- as.numeric(mat %*% shares) * 100
  data.frame(Date = dates, Close = level, stringsAsFactors = FALSE)
}

ynow_index_snapshot_path <- function() {
  cands <- c(
    file.path("data", "universe_metrics_snapshot.csv"),
    file.path("app_19.0", "data", "universe_metrics_snapshot.csv")
  )
  hit <- cands[file.exists(cands)]
  if (length(hit)) hit[[1]] else cands[[1]]
}

ynow_index_cache_path <- function(market = "US") {
  snap <- ynow_index_snapshot_path()
  fn <- if (identical(ynow_index_normalize_market(market), "TW")) {
    "tynow_index_basket.csv"
  } else {
    "ynow_index_basket.csv"
  }
  file.path(dirname(snap), fn)
}

#' 上市／上櫃普通股。排除 ETF（00 開頭）與興櫃。
ynow_index_filter_tw_rows <- function(uni) {
  if (!is.data.frame(uni) || !nrow(uni) || !"ticker" %in% names(uni)) {
    return(uni[0, , drop = FALSE])
  }
  out <- uni
  out$ticker <- toupper(trimws(as.character(out$ticker)))
  out <- out[nzchar(out$ticker), , drop = FALSE]
  if ("exchange" %in% names(out)) {
    ex <- toupper(trimws(as.character(out$exchange)))
    out <- out[!ex %in% c("ESB", "EMERGING", "TPEX_ESB"), , drop = FALSE]
  }
  code <- sub("\\.(TW|TWO)$", "", out$ticker)
  out[!grepl("^00", code), , drop = FALSE]
}

ynow_index_us_ranked <- function(scan_cap = YNOW_INDEX_SCAN_CAP) {
  path <- ynow_index_snapshot_path()
  if (!file.exists(path)) {
    return(data.frame(ticker = character(0), market_cap = numeric(0), stringsAsFactors = FALSE))
  }
  snap <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("ticker", "market_cap") %in% names(snap))) {
    return(data.frame(ticker = character(0), market_cap = numeric(0), stringsAsFactors = FALSE))
  }
  snap$ticker <- toupper(gsub("/", "-", trimws(as.character(snap$ticker))))
  snap$market_cap <- suppressWarnings(as.numeric(snap$market_cap))
  snap <- snap[nzchar(snap$ticker) & is.finite(snap$market_cap) & snap$market_cap > 0, , drop = FALSE]
  if (exists("lab_get_us_universe", mode = "function")) {
    uni <- tryCatch(lab_get_us_universe(), error = function(e) NULL)
    if (is.data.frame(uni) && "ticker" %in% names(uni) && nrow(uni)) {
      us <- toupper(gsub("/", "-", trimws(as.character(uni$ticker))))
      snap <- snap[snap$ticker %in% us, , drop = FALSE]
    }
  }
  snap <- snap[order(-snap$market_cap, snap$ticker), , drop = FALSE]
  snap <- snap[!duplicated(snap$ticker), , drop = FALSE]
  utils::head(snap, max(1L, as.integer(scan_cap)[1]))
}

ynow_index_tw_ranked <- function(scan_cap = YNOW_INDEX_SCAN_CAP_TW) {
  path <- ynow_index_snapshot_path()
  empty <- data.frame(ticker = character(0), market_cap = numeric(0), stringsAsFactors = FALSE)
  if (!file.exists(path)) return(empty)
  snap <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("ticker", "market_cap") %in% names(snap))) return(empty)
  snap$ticker <- toupper(gsub("/", "-", trimws(as.character(snap$ticker))))
  snap$market_cap <- suppressWarnings(as.numeric(snap$market_cap))
  snap <- snap[nzchar(snap$ticker) & is.finite(snap$market_cap) & snap$market_cap > 0, , drop = FALSE]
  uni <- NULL
  if (exists("lab_get_tw_universe", mode = "function")) {
    uni <- tryCatch(lab_get_tw_universe(FALSE), error = function(e) NULL)
  }
  if (!is.data.frame(uni) || !nrow(uni) || !"ticker" %in% names(uni)) {
    csv <- file.path(dirname(path), "tw_universe.csv")
    if (file.exists(csv)) {
      uni <- tryCatch(utils::read.csv(csv, stringsAsFactors = FALSE), error = function(e) NULL)
    }
  }
  uni <- ynow_index_filter_tw_rows(uni)
  if (!is.data.frame(uni) || !nrow(uni)) return(empty)
  snap <- snap[snap$ticker %in% uni$ticker, , drop = FALSE]
  snap <- snap[order(-snap$market_cap, snap$ticker), , drop = FALSE]
  snap <- snap[!duplicated(snap$ticker), , drop = FALSE]
  utils::head(snap, max(1L, as.integer(scan_cap)[1]))
}

ynow_index_read_cache <- function(month, path = ynow_index_cache_path()) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || !nrow(df) || !("month" %in% names(df))) return(NULL)
  df <- df[as.character(df$month) == as.character(month), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  if (!nzchar(as.character(df$ticker[1]))) {
    return(list(
      members = list(),
      scanned = suppressWarnings(as.integer(df$scanned[1])),
      n = 0L,
      complete = FALSE,
      hit_scan_cap = isTRUE(df$hit_scan_cap[1] %in% c(TRUE, "TRUE", "true", 1)),
      month = as.character(month),
      built_at = as.character(df$built_at[1])
    ))
  }
  members <- lapply(seq_len(nrow(df)), function(i) {
    list(
      ticker = toupper(as.character(df$ticker[i])),
      market_cap = suppressWarnings(as.numeric(df$market_cap[i])),
      f_score = suppressWarnings(as.numeric(df$f_score[i])),
      n_alert = 0,
      weight = suppressWarnings(as.numeric(df$weight[i]))
    )
  })
  list(
    members = members,
    scanned = suppressWarnings(as.integer(df$scanned[1])),
    n = length(members),
    complete = isTRUE(length(members) >= YNOW_INDEX_N),
    hit_scan_cap = isTRUE(df$hit_scan_cap[1] %in% c(TRUE, "TRUE", "true", 1)),
    month = as.character(month),
    built_at = as.character(df$built_at[1])
  )
}

ynow_index_write_cache <- function(sel, path = ynow_index_cache_path()) {
  mem <- sel$members %||% list()
  df <- if (length(mem)) {
    do.call(rbind, lapply(mem, function(m) {
      data.frame(
        month = sel$month %||% "",
        ticker = m$ticker,
        market_cap = m$market_cap %||% NA_real_,
        f_score = m$f_score %||% NA_real_,
        weight = m$weight %||% NA_real_,
        scanned = sel$scanned %||% NA_integer_,
        hit_scan_cap = isTRUE(sel$hit_scan_cap),
        built_at = sel$built_at %||% "",
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      month = sel$month %||% "",
      ticker = "",
      market_cap = NA_real_,
      f_score = NA_real_,
      weight = NA_real_,
      scanned = sel$scanned %||% NA_integer_,
      hit_scan_cap = isTRUE(sel$hit_scan_cap),
      built_at = sel$built_at %||% "",
      stringsAsFactors = FALSE
    )
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ok <- tryCatch({
    utils::write.csv(df, path, row.names = FALSE)
    TRUE
  }, error = function(e) FALSE)
  invisible(ok)
}

ynow_index_live_screen <- function(ticker) {
  empty <- list(pass = FALSE, f_score = NA_real_, n_alert = NA_real_)
  if (!exists("cached_scrape_financials", mode = "function")) return(empty)
  res <- tryCatch(cached_scrape_financials(ticker), error = function(e) NULL)
  if (is.null(res) || !is.list(res)) return(empty)
  pull <- function(nm) {
    node <- res[[nm]]
    df <- if (is.list(node) && !is.null(node$expanded)) node$expanded else node
    if (exists("coerce_financial_df", mode = "function")) {
      df <- tryCatch(coerce_financial_df(df), error = function(e) df)
    }
    df
  }
  ind <- ynow_index_industry_key(ticker)
  ynow_index_screen_statements(pull("Income Statement"), pull("Balance Sheet"), pull("Cash Flow"), ind)
}

ynow_index_industry_key <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  tw <- grepl("\\.(TW|TWO)$", tk)
  getter <- if (tw) "lab_get_tw_universe" else "lab_get_us_universe"
  if (!exists(getter, mode = "function")) return(NULL)
  uni <- tryCatch(get(getter)(), error = function(e) NULL)
  if (tw) uni <- ynow_index_filter_tw_rows(uni)
  if (!is.data.frame(uni) || !all(c("ticker", "industry_key") %in% names(uni))) return(NULL)
  hit <- uni$industry_key[toupper(uni$ticker) == tk]
  if (!length(hit)) return(NULL)
  as.character(hit[1])
}

ynow_index_current <- function(as_of = Sys.time(), rebuild = FALSE,
                               screen_fn = NULL, ranked = NULL,
                               cache_path = NULL, market = "US") {
  market <- ynow_index_normalize_market(market)
  key <- ynow_index_month_key(as_of)
  path <- cache_path %||% ynow_index_cache_path(market)
  mem_key <- paste0(key, "|", path)
  if (!isTRUE(rebuild) && exists(mem_key, envir = .ynow_index_mem, inherits = FALSE)) {
    return(get(mem_key, envir = .ynow_index_mem, inherits = FALSE))
  }
  if (!isTRUE(rebuild)) {
    disk <- ynow_index_read_cache(key, path)
    if (!is.null(disk)) {
      assign(mem_key, disk, envir = .ynow_index_mem)
      return(disk)
    }
  }
  if (is.null(ranked)) {
    ranked <- if (identical(market, "TW")) ynow_index_tw_ranked() else ynow_index_us_ranked()
  }
  if (is.data.frame(ranked) && all(c("ticker", "market_cap") %in% names(ranked))) {
    ranked$market_cap <- suppressWarnings(as.numeric(ranked$market_cap))
    ranked <- ranked[order(-ranked$market_cap, ranked$ticker), , drop = FALSE]
  }
  tks <- if (is.data.frame(ranked)) ranked$ticker else as.character(ranked)
  caps <- NULL
  if (is.data.frame(ranked) && "market_cap" %in% names(ranked)) {
    caps <- stats::setNames(suppressWarnings(as.numeric(ranked$market_cap)), toupper(ranked$ticker))
  }
  fn <- screen_fn %||% ynow_index_live_screen
  sel <- ynow_index_select(tks, fn, mcap = caps, scan_cap = ynow_index_scan_cap(market))
  sel$month <- key
  sel$market <- market
  sel$symbol <- ynow_index_symbol(market)
  sel$built_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ynow_index_write_cache(sel, path)
  assign(mem_key, sel, envir = .ynow_index_mem)
  sel
}

ynow_index_series <- function(tickers, period = "1y", fetch_px = NULL) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks)]
  if (!length(tks)) return(NULL)
  if (is.null(fetch_px)) fetch_px <- if (exists("fetch_price_history_df", mode = "function")) {
    fetch_price_history_df
  } else {
    function(...) NULL
  }
  px <- list()
  for (tk in tks) {
    px[[tk]] <- tryCatch(fetch_px(tk, period), error = function(e) NULL)
  }
  ynow_index_bh_equal(px)
}
