# ==========================================
# lab_us_universe.R — Lab 宇宙：美股主要上市（Nasdaq／NYSE／NYSE American）
# 來源：SEC company_tickers_exchange.json（過濾 primary listing；排除 OTC／CBOE／None）
# 產業鍵：優先套用 S&P 500 GICS 對應（lab_sp500_universe.R）；其餘 → lab.Unmapped
# 快取 data/us_universe.csv；過期約 7 天可背景更新。
# Blue Chip／Search 用此宇宙；評估仍以 N＋候選截斷，不全掃數千檔 Yahoo。
# ==========================================

LAB_US_STALE_DAYS <- 7
LAB_US_CACHE_REL <- file.path("data", "us_universe.csv")
LAB_US_SEC_EXCHANGE_URL <- "https://www.sec.gov/files/company_tickers_exchange.json"
# 評估前若候選過大，先預篩至此再做市值／漲幅截斷（避免一次抓數千檔市值）
LAB_US_EVAL_PRESCREEN <- 800L
LAB_US_INDEX_ETFS <- c("SPY", "QQQ", "DIA", "IWM", "VOO", "IVV", "VTI", "QQQM")

.lab_us_env <- new.env(parent = emptyenv())

lab_empty_us <- function() {
  data.frame(
    ticker = character(0),
    name = character(0),
    exchange = character(0),
    industry_raw = character(0),
    industry_key = character(0),
    fetched_at = character(0),
    source = character(0),
    stringsAsFactors = FALSE
  )
}

.lab_us_http_download <- function(url, dest, timeout_sec = 60) {
  old <- getOption("timeout")
  on.exit(options(timeout = old), add = TRUE)
  options(timeout = max(12L, as.integer(timeout_sec)[1]))
  ua <- "theYNowApp/17.0 (lab US universe; lawrencekuo1118@gmail.com)"
  ok <- tryCatch({
    utils::download.file(
      url, destfile = dest, quiet = TRUE, mode = "wb",
      headers = c("User-Agent" = ua)
    )
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok) || !file.exists(dest) || isTRUE(file.info(dest)$size < 80)) {
    ok <- tryCatch({
      status <- system2(
        "curl",
        c("-fsSL", "-A", ua, "-o", dest, "--max-time", as.character(as.integer(timeout_sec)[1]), url),
        stdout = FALSE, stderr = FALSE
      )
      identical(as.integer(status)[1], 0L)
    }, error = function(e) FALSE)
  }
  isTRUE(ok) && file.exists(dest) && isTRUE(file.info(dest)$size > 80)
}

lab_us_cache_paths <- function() {
  unique(c(
    LAB_US_CACHE_REL,
    file.path(getwd(), LAB_US_CACHE_REL),
    file.path("app_17.0", LAB_US_CACHE_REL)
  ))
}

lab_us_existing_cache <- function() {
  hits <- lab_us_cache_paths()
  hits <- hits[file.exists(hits)]
  if (!length(hits)) return(NA_character_)
  normalizePath(hits[[1]], mustWork = FALSE)
}

#' Drop obvious ETF / fund name patterns from SEC equity listing rows
lab_us_looks_like_etf_or_fund <- function(name) {
  nm <- toupper(trimws(as.character(name %||% "")[1]))
  if (!nzchar(nm)) return(FALSE)
  grepl(
    "\\bETF\\b|\\bETN\\b|\\bEXCHANGE[- ]TRADED\\b|\\bTRUST\\b.*\\bFUND\\b|\\bINDEX FUND\\b|\\bMUTUAL FUND\\b",
    nm,
    perl = TRUE
  )
}

#' Apply lab_ticker_industry_overrides to Unmapped US rows (ADR / non-S&P)
lab_us_overlay_ticker_industry_overrides <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(df)
  if (!("ticker" %in% names(df)) || !("industry_key" %in% names(df))) return(df)
  if (!exists("lab_ticker_industry_overrides", mode = "function")) return(df)
  ov <- lab_ticker_industry_overrides()
  if (!length(ov)) return(df)
  unmapped <- if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) LAB_UNMAPPED_KEY else "lab.Unmapped"
  tks <- toupper(trimws(as.character(df$ticker)))
  keys <- as.character(df$industry_key)
  need <- which(
    tks %in% names(ov) &
      (is.na(keys) | !nzchar(keys) | keys == unmapped)
  )
  # #region agent log
  tryCatch({
    probe <- c("TSM", "SKHY", "MU", "NVDA")
    pre <- setNames(keys[match(probe, tks)], probe)
    applied <- character(0)
    if (length(need)) {
      applied <- unique(tks[need])
    }
    payload <- sprintf(
      paste0(
        '{"hypothesisId":"fix","runId":"post-fix","location":"lab_us_universe.R:lab_us_overlay_ticker_industry_overrides",',
        '"message":"ticker override overlay","data":{"n_need":%d,"applied":[%s],"pre_keys":{%s}},',
        '"timestamp":%s}\n'
      ),
      length(need),
      paste(sprintf('"%s"', applied), collapse = ","),
      paste(sprintf('"%s":"%s"', names(pre), gsub('"', "", as.character(pre))), collapse = ","),
      format(as.numeric(Sys.time()) * 1000, scientific = FALSE)
    )
    cat(payload, file = "/opt/cursor/logs/debug.log", append = TRUE)
  }, error = function(e) invisible(NULL))
  # #endregion
  if (!length(need)) return(df)
  df$industry_key[need] <- unname(ov[tks[need]])
  # #region agent log
  tryCatch({
    probe <- c("TSM", "SKHY", "MU", "NVDA")
    post <- setNames(as.character(df$industry_key[match(probe, tks)]), probe)
    payload <- sprintf(
      paste0(
        '{"hypothesisId":"fix","runId":"post-fix","location":"lab_us_universe.R:lab_us_overlay_ticker_industry_overrides:after",',
        '"message":"keys after ticker override overlay","data":{"post_keys":{%s}},"timestamp":%s}\n'
      ),
      paste(sprintf('"%s":"%s"', names(post), gsub('"', "", as.character(post))), collapse = ","),
      format(as.numeric(Sys.time()) * 1000, scientific = FALSE)
    )
    cat(payload, file = "/opt/cursor/logs/debug.log", append = TRUE)
  }, error = function(e) invisible(NULL))
  # #endregion
  df
}

#' Overlay S&P 500 industry_key / industry_raw when ticker matches
lab_us_overlay_sp500_industry <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(df)
  sp <- tryCatch({
    if (exists("lab_get_sp500_universe", mode = "function")) lab_get_sp500_universe(FALSE) else NULL
  }, error = function(e) NULL)
  if (!is.null(sp) && is.data.frame(sp) && nrow(sp) >= 1L &&
      all(c("ticker", "industry_key") %in% names(sp))) {
    sp$ticker <- toupper(trimws(as.character(sp$ticker)))
    idx <- match(toupper(trimws(as.character(df$ticker))), sp$ticker)
    hit <- which(!is.na(idx))
    # #region agent log
    tryCatch({
      probe <- c("TSM", "SKHY", "MU", "NVDA")
      df_tk <- toupper(trimws(as.character(df$ticker)))
      pre_keys <- setNames(as.character(df$industry_key[match(probe, df_tk)]), probe)
      sp_hit <- setNames(!is.na(match(probe, sp$ticker)), probe)
      payload <- sprintf(
        paste0(
          '{"hypothesisId":"B","location":"lab_us_universe.R:lab_us_overlay_sp500_industry",',
          '"message":"SP500 overlay probe","data":{"n_df":%d,"n_sp":%d,"n_hit":%d,',
          '"pre_keys":{%s},"in_sp500":{%s}},"timestamp":%s}\n'
        ),
        nrow(df), nrow(sp), length(hit),
        paste(sprintf('"%s":"%s"', names(pre_keys), gsub('"', "", as.character(pre_keys))), collapse = ","),
        paste(sprintf('"%s":%s', names(sp_hit), ifelse(sp_hit, "true", "false")), collapse = ","),
        format(as.numeric(Sys.time()) * 1000, scientific = FALSE)
      )
      cat(payload, file = "/opt/cursor/logs/debug.log", append = TRUE)
    }, error = function(e) invisible(NULL))
    # #endregion
    if (length(hit)) {
      df$industry_key[hit] <- as.character(sp$industry_key[idx[hit]])
      if ("industry_raw" %in% names(sp)) {
        if (!("industry_raw" %in% names(df))) df$industry_raw <- NA_character_
        df$industry_raw[hit] <- as.character(sp$industry_raw[idx[hit]])
      }
    }
    # #region agent log
    tryCatch({
      probe <- c("TSM", "SKHY", "MU", "NVDA")
      df_tk <- toupper(trimws(as.character(df$ticker)))
      post_keys <- setNames(as.character(df$industry_key[match(probe, df_tk)]), probe)
      payload <- sprintf(
        paste0(
          '{"hypothesisId":"A","location":"lab_us_universe.R:lab_us_overlay_sp500_industry:after",',
          '"message":"keys after SP500 overlay","data":{"post_keys":{%s}},"timestamp":%s}\n'
        ),
        paste(sprintf('"%s":"%s"', names(post_keys), gsub('"', "", as.character(post_keys))), collapse = ","),
        format(as.numeric(Sys.time()) * 1000, scientific = FALSE)
      )
      cat(payload, file = "/opt/cursor/logs/debug.log", append = TRUE)
    }, error = function(e) invisible(NULL))
    # #endregion
  }
  # ADR／非 S&P：套用個股覆寫（TSM→Foundry、SKHY→Memory 等）
  lab_us_overlay_ticker_industry_overrides(df)
}

lab_finalize_us_from_sec <- function(raw, fetched_at = NULL) {
  empty <- lab_empty_us()
  if (is.null(raw) || !is.list(raw) || is.null(raw$data)) return(empty)
  unmapped <- if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) LAB_UNMAPPED_KEY else "lab.Unmapped"
  ts <- as.character(fetched_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))[1]
  yahoo_sym <- if (exists("lab_yahoo_symbol", mode = "function")) {
    lab_yahoo_symbol
  } else {
    function(sym) {
      tk <- toupper(trimws(as.character(sym %||% "")[1]))
      if (!nzchar(tk) || identical(tk, "NA")) return(NA_character_)
      gsub("\\.", "-", gsub("/", "-", tk))
    }
  }
  rows <- list()
  seen <- character(0)
  for (row in raw$data) {
    if (!is.list(row) || length(row) < 4L) next
    tk <- yahoo_sym(row[[3]])
    if (is.na(tk) || !nzchar(tk)) next
    if (tk %in% seen) next
    if (tk %in% LAB_US_INDEX_ETFS) next
    nm <- trimws(as.character(row[[2]] %||% ""))
    if (lab_us_looks_like_etf_or_fund(nm)) next
    exch_raw <- as.character(row[[4]] %||% "")
    ex_norm <- if (exists("normalize_us_listing_exchange", mode = "function")) {
      normalize_us_listing_exchange(exch_raw)
    } else {
      NA_character_
    }
    # Keep primary Nasdaq / NYSE (incl. American via normalize)
    keep <- if (exists("is_us_primary_listing_exchange", mode = "function")) {
      isTRUE(is_us_primary_listing_exchange(exch_raw)) ||
        (!is.na(ex_norm) && nzchar(ex_norm))
    } else {
      !is.na(ex_norm) && nzchar(ex_norm)
    }
    if (!isTRUE(keep) || is.na(ex_norm) || !nzchar(ex_norm)) next
    seen <- c(seen, tk)
    rows[[length(rows) + 1L]] <- data.frame(
      ticker = tk,
      name = nm,
      exchange = ex_norm,
      industry_raw = NA_character_,
      industry_key = unmapped,
      fetched_at = ts,
      source = "sec-exchange",
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty)
  df <- do.call(rbind, rows)
  # #region agent log
  tryCatch({
    probe <- c("TSM", "SKHY")
    df_tk <- toupper(trimws(as.character(df$ticker)))
    pre <- setNames(as.character(df$industry_key[match(probe, df_tk)]), probe)
    names_p <- setNames(as.character(df$name[match(probe, df_tk)]), probe)
    payload <- sprintf(
      paste0(
        '{"hypothesisId":"A","location":"lab_us_universe.R:lab_finalize_us_from_sec",',
        '"message":"SEC default keys before overlay","data":{"default_unmapped":"%s",',
        '"pre_keys":{%s},"names":{%s}},"timestamp":%s}\n'
      ),
      unmapped,
      paste(sprintf('"%s":"%s"', names(pre), gsub('"', "", as.character(pre))), collapse = ","),
      paste(sprintf('"%s":"%s"', names(names_p), gsub('["\\\\]', "", as.character(names_p))), collapse = ","),
      as.integer(as.numeric(Sys.time()) * 1000)
    )
    cat(payload, file = "/opt/cursor/logs/debug.log", append = TRUE)
  }, error = function(e) invisible(NULL))
  # #endregion
  lab_us_overlay_sp500_industry(df)
}

lab_fetch_us_sec_universe <- function(timeout_sec = 60) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  if (!isTRUE(.lab_us_http_download(LAB_US_SEC_EXCHANGE_URL, tmp, timeout_sec))) {
    stop("SEC company_tickers_exchange.json download failed")
  }
  raw <- tryCatch(
    jsonlite::fromJSON(tmp, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw)) stop("SEC JSON parse failed")
  lab_finalize_us_from_sec(
    raw,
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

lab_read_us_csv <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) return(NULL)
  d <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d) || !("ticker" %in% names(d))) return(NULL)
  need <- c("ticker", "name", "exchange", "industry_raw", "industry_key", "fetched_at", "source")
  for (col in need) {
    if (!(col %in% names(d))) d[[col]] <- NA_character_
  }
  d$ticker <- toupper(trimws(as.character(d$ticker)))
  d <- d[nzchar(d$ticker) & !is.na(d$ticker), , drop = FALSE]
  d <- d[!duplicated(d$ticker), , drop = FALSE]
  # #region agent log
  tryCatch({
    probe <- c("TSM", "SKHY", "MU", "NVDA")
    pre <- setNames(as.character(d$industry_key[match(probe, d$ticker)]), probe)
    payload <- sprintf(
      paste0(
        '{"hypothesisId":"A","location":"lab_us_universe.R:lab_read_us_csv",',
        '"message":"CSV keys before overlay","data":{"path":"%s","n":%d,"pre_keys":{%s}},',
        '"timestamp":%s}\n'
      ),
      gsub('"', "", as.character(path)[1]),
      nrow(d),
      paste(sprintf('"%s":"%s"', names(pre), gsub('"', "", as.character(pre))), collapse = ","),
      as.integer(as.numeric(Sys.time()) * 1000)
    )
    cat(payload, file = "/opt/cursor/logs/debug.log", append = TRUE)
  }, error = function(e) invisible(NULL))
  # #endregion
  lab_us_overlay_sp500_industry(d)
}

lab_load_us_cache <- function() {
  p <- lab_us_existing_cache()
  if (is.na(p)) return(NULL)
  lab_read_us_csv(p)
}

lab_save_us_cache <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(invisible(FALSE))
  paths <- lab_us_cache_paths()
  for (p in paths) {
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    ok <- tryCatch({
      utils::write.csv(df, p, row.names = FALSE, fileEncoding = "UTF-8")
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok) && file.exists(p)) {
      .lab_us_env$cache_path <- normalizePath(p, mustWork = FALSE)
      return(invisible(TRUE))
    }
  }
  tp <- file.path(tempdir(), "us_universe.csv")
  utils::write.csv(df, tp, row.names = FALSE, fileEncoding = "UTF-8")
  .lab_us_env$cache_path <- tp
  invisible(TRUE)
}

lab_us_cache_age_days <- function(df) {
  ts <- if (is.data.frame(df) && "fetched_at" %in% names(df) && nrow(df) > 0) {
    as.character(df$fetched_at[[1]])
  } else {
    NA_character_
  }
  t <- suppressWarnings(as.POSIXct(ts, tz = "UTC"))
  if (is.na(t)) return(Inf)
  as.numeric(difftime(Sys.time(), t, units = "days"))
}

lab_us_is_stale <- function(df = NULL) {
  df <- df %||% .lab_us_env$universe
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(TRUE)
  age <- lab_us_cache_age_days(df)
  !is.finite(age) || age >= LAB_US_STALE_DAYS
}

lab_get_us_universe <- function(force_refresh = FALSE) {
  if (!isTRUE(force_refresh) && is.data.frame(.lab_us_env$universe) &&
      nrow(.lab_us_env$universe) > 0) {
    return(.lab_us_env$universe)
  }
  loaded <- lab_load_us_cache()
  if (is.data.frame(loaded) && nrow(loaded) > 0) {
    .lab_us_env$universe <- loaded
    if (!isTRUE(force_refresh) && !lab_us_is_stale(loaded)) return(loaded)
  }
  if (isTRUE(force_refresh) || is.null(loaded) || nrow(loaded) < 1L || lab_us_is_stale(loaded)) {
    fetched <- tryCatch(lab_fetch_us_sec_universe(), error = function(e) NULL)
    if (is.data.frame(fetched) && nrow(fetched) >= 1000L) {
      lab_save_us_cache(fetched)
      .lab_us_env$universe <- fetched
      return(fetched)
    }
  }
  if (is.data.frame(loaded) && nrow(loaded) > 0) {
    .lab_us_env$universe <- loaded
    return(loaded)
  }
  empty <- lab_empty_us()
  .lab_us_env$universe <- empty
  empty
}

lab_refresh_us_universe <- function() {
  fetched <- lab_fetch_us_sec_universe()
  if (is.null(fetched) || !is.data.frame(fetched) || nrow(fetched) < 1L) {
    stop("US universe refresh returned empty")
  }
  lab_save_us_cache(fetched)
  .lab_us_env$universe <- fetched
  fetched
}

lab_us_universe_meta <- function() {
  u <- tryCatch(lab_get_us_universe(FALSE), error = function(e) lab_empty_us())
  unmapped <- if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) LAB_UNMAPPED_KEY else "lab.Unmapped"
  ex <- if ("exchange" %in% names(u)) toupper(trimws(as.character(u$exchange))) else character(0)
  list(
    n = nrow(u),
    n_nasdaq = if (!length(ex)) 0L else sum(ex == "NASDAQ", na.rm = TRUE),
    n_nyse = if (!length(ex)) 0L else sum(ex == "NYSE", na.rm = TRUE),
    n_unmapped = if (!nrow(u) || !("industry_key" %in% names(u))) 0L else
      sum(u$industry_key == unmapped, na.rm = TRUE),
    fetched_at = if (nrow(u) && "fetched_at" %in% names(u)) as.character(u$fetched_at[[1]]) else NA_character_,
    source = if (nrow(u) && "source" %in% names(u)) as.character(u$source[[1]]) else NA_character_,
    stale = lab_us_is_stale(u)
  )
}

lab_us_company_name <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk)) return(NA_character_)
  u <- .lab_us_env$universe
  if (is.null(u) || !is.data.frame(u) || nrow(u) < 1L) {
    u <- tryCatch(lab_get_us_universe(FALSE), error = function(e) NULL)
  }
  if (is.null(u) || !nrow(u)) return(NA_character_)
  hit <- match(tk, toupper(trimws(as.character(u$ticker))))
  if (is.na(hit)) return(NA_character_)
  nm <- trimws(as.character(u$name[[hit]]))
  if (!nzchar(nm)) NA_character_ else nm
}

#' When eval pool is huge, prefer S&P names then random fill before mcap/ret truncate
lab_us_prescreen_eval_pool <- function(pool, max_n = LAB_US_EVAL_PRESCREEN,
                                       prefer_tickers = NULL) {
  pool <- if (exists("lab_dedupe_eval_pool", mode = "function")) {
    lab_dedupe_eval_pool(pool)
  } else {
    pool
  }
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) < 1L) return(pool)
  max_n <- suppressWarnings(as.integer(max_n)[1])
  if (!is.finite(max_n) || max_n < 50L) max_n <- LAB_US_EVAL_PRESCREEN
  if (nrow(pool) <= max_n) return(pool)
  tks <- toupper(trimws(as.character(pool$ticker)))
  prefer <- unique(toupper(trimws(as.character(prefer_tickers %||% character(0)))))
  if (!length(prefer) && exists("lab_get_sp500_universe", mode = "function")) {
    sp <- tryCatch(lab_get_sp500_universe(FALSE), error = function(e) NULL)
    if (is.data.frame(sp) && nrow(sp) > 0 && "ticker" %in% names(sp)) {
      prefer <- unique(toupper(trimws(as.character(sp$ticker))))
    }
  }
  in_pref <- which(tks %in% prefer)
  out_pref <- which(!tks %in% prefer)
  keep_idx <- integer(0)
  if (length(in_pref)) {
    keep_idx <- c(keep_idx, utils::head(in_pref, max_n))
  }
  need <- max_n - length(keep_idx)
  if (need > 0L && length(out_pref)) {
    set.seed(as.integer(Sys.time()) %% 1e6L)
    take <- if (length(out_pref) <= need) out_pref else sample(out_pref, need)
    keep_idx <- c(keep_idx, take)
  }
  pool[sort(unique(keep_idx)), , drop = FALSE]
}

# Warm cache on source (bundled CSV preferred; network refresh only when stale)
tryCatch(lab_get_us_universe(force_refresh = FALSE), error = function(e) NULL)
