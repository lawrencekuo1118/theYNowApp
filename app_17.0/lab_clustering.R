# ==========================================
# lab_clustering.R — Blue Chip Lab：基本面 K-Means 分群
#
# 特徵僅用比率／成長率（禁止金額進模型），排除規模干擾。
# 分群為研究／教育工具，非下單或買進訊號。
# ==========================================

LAB_CLUSTER_FEATURES <- c(
  "ROE",
  "Operating_Margin",
  "Rev_YoY",
  "OpInc_YoY",
  "Debt_Ratio",
  "PE_Ratio",
  "PB_Ratio"
)

LAB_CLUSTER_AXIS_CHOICES <- c(
  "ROE" = "ROE",
  "Operating Margin" = "Operating_Margin",
  "Rev YoY" = "Rev_YoY",
  "OpInc YoY" = "OpInc_YoY",
  "Debt Ratio" = "Debt_Ratio",
  "Trailing P/E" = "PE_Ratio",
  "P/B" = "PB_Ratio"
)

#' Winsorize numeric vector to [lo, hi] empirical quantiles
lab_cluster_winsorize <- function(x, lo = 0.01, hi = 0.99) {
  x <- suppressWarnings(as.numeric(x))
  ok <- is.finite(x)
  if (!any(ok)) return(x)
  qs <- stats::quantile(x[ok], probs = c(lo, hi), na.rm = TRUE, names = FALSE, type = 7)
  x[ok & x < qs[1]] <- qs[1]
  x[ok & x > qs[2]] <- qs[2]
  x
}

#' Convert Python / list-of-dicts feature rows into a tidy data.frame
lab_cluster_features_to_df <- function(rows) {
  empty <- data.frame(
    ticker = character(0),
    name = character(0),
    market_cap = numeric(0),
    ROE = numeric(0),
    Operating_Margin = numeric(0),
    Rev_YoY = numeric(0),
    OpInc_YoY = numeric(0),
    Debt_Ratio = numeric(0),
    PE_Ratio = numeric(0),
    PB_Ratio = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(rows)) return(empty)
  if (is.data.frame(rows)) {
    df <- rows
  } else if (is.list(rows)) {
    if (length(rows) == 0L) return(empty)
    if (!is.null(names(rows)) && all(c("ticker", "ROE") %in% names(rows))) {
      df <- as.data.frame(rows, stringsAsFactors = FALSE)
    } else {
      df <- tryCatch(
        dplyr::bind_rows(lapply(rows, function(r) {
          if (is.null(r)) return(NULL)
          as.data.frame(as.list(r), stringsAsFactors = FALSE)
        })),
        error = function(e) empty
      )
    }
  } else {
    return(empty)
  }
  if (!is.data.frame(df) || nrow(df) == 0L) return(empty)
  rename_map <- c(
    Ticker = "ticker", Symbol = "ticker", symbol = "ticker",
    Name = "name", shortName = "name",
    marketCap = "market_cap", Market_Cap = "market_cap",
    OperatingMargin = "Operating_Margin", operatingMargins = "Operating_Margin",
    RevYoY = "Rev_YoY", revenueGrowth = "Rev_YoY",
    OpIncYoY = "OpInc_YoY",
    DebtRatio = "Debt_Ratio", debtToEquity = "Debt_Ratio",
    PE = "PE_Ratio", trailingPE = "PE_Ratio",
    PB = "PB_Ratio", priceToBook = "PB_Ratio"
  )
  for (nm in names(rename_map)) {
    if (nm %in% names(df) && !rename_map[[nm]] %in% names(df)) {
      names(df)[names(df) == nm] <- rename_map[[nm]]
    }
  }
  need <- names(empty)
  for (nm in need) {
    if (!nm %in% names(df)) {
      df[[nm]] <- if (nm %in% c("ticker", "name")) NA_character_ else NA_real_
    }
  }
  df$ticker <- toupper(trimws(as.character(df$ticker)))
  blank <- is.na(df$ticker) | !nzchar(df$ticker)
  if (any(blank) && !is.null(rownames(df))) {
    df$ticker[blank] <- toupper(trimws(rownames(df)[blank]))
  }
  df$name <- as.character(df$name)
  df$name[is.na(df$name) | !nzchar(df$name)] <- df$ticker[is.na(df$name) | !nzchar(df$name)]
  for (f in LAB_CLUSTER_FEATURES) {
    df[[f]] <- suppressWarnings(as.numeric(df[[f]]))
  }
  df$market_cap <- suppressWarnings(as.numeric(df$market_cap))
  df <- df[!is.na(df$ticker) & nzchar(df$ticker), need, drop = FALSE]
  rownames(df) <- NULL
  df
}

.lab_cluster_yahoo_raw <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.list(x) && !is.null(x$raw)) x <- x$raw
  suppressWarnings(as.numeric(x)[1])
}

.lab_cluster_pct_points <- function(x) {
  v <- .lab_cluster_yahoo_raw(x)
  if (!is.finite(v)) return(NA_real_)
  if (abs(v) <= 1.5) v * 100 else v
}

.lab_cluster_debt_pct <- function(x) {
  v <- .lab_cluster_yahoo_raw(x)
  if (!is.finite(v)) return(NA_real_)
  if (abs(v) < 5) v * 100 else v
}

#' Count rows with usable ratio features (same criteria as missing-data keep rule)
lab_cluster_usable_feature_rows <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(0L)
  feats <- intersect(LAB_CLUSTER_FEATURES, names(df))
  if (!length(feats)) return(0L)
  mat <- do.call(
    cbind,
    lapply(feats, function(f) is.finite(suppressWarnings(as.numeric(df[[f]]))))
  )
  n_finite <- as.integer(rowSums(mat, na.rm = TRUE))
  as.integer(sum(n_finite >= 2L, na.rm = TRUE))
}

#' Count finite clustering ratios for one ticker (0 if absent)
lab_cluster_n_finite_for_ticker <- function(df, ticker) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(0L)
  matched <- lab_cluster_match_ticker(df$ticker, ticker)
  if (is.na(matched) || !nzchar(matched)) return(0L)
  feats <- intersect(LAB_CLUSTER_FEATURES, names(df))
  if (!length(feats)) return(0L)
  row <- df[toupper(trimws(as.character(df$ticker))) == matched, , drop = FALSE][1, , drop = FALSE]
  as.integer(sum(vapply(
    feats,
    function(f) is.finite(suppressWarnings(as.numeric(row[[f]][[1]]))),
    logical(1)
  )))
}

#' Tickers that still need ≥2 finite ratios (for R gap-fill)
lab_cluster_sparse_tickers <- function(df, tickers) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  if (!length(tks)) return(character(0))
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(tks)
  feats <- intersect(LAB_CLUSTER_FEATURES, names(df))
  if (!length(feats)) return(tks)
  sparse <- logical(length(tks))
  for (i in seq_along(tks)) {
    matched <- lab_cluster_match_ticker(df$ticker, tks[[i]])
    if (is.na(matched) || !nzchar(matched)) {
      sparse[[i]] <- TRUE
      next
    }
    sparse[[i]] <- lab_cluster_n_finite_for_ticker(df, matched) < 2L
  }
  tks[sparse]
}

#' Merge feature frames by ticker (prefer finite values from primary, then secondary)
lab_cluster_merge_feature_dfs <- function(primary, secondary) {
  empty <- lab_cluster_features_to_df(NULL)
  a <- if (is.null(primary)) empty else lab_cluster_features_to_df(primary)
  b <- if (is.null(secondary)) empty else lab_cluster_features_to_df(secondary)
  if (nrow(a) == 0L) return(b)
  if (nrow(b) == 0L) return(a)
  all_tks <- unique(c(as.character(a$ticker), as.character(b$ticker)))
  all_tks <- all_tks[nzchar(all_tks) & !is.na(all_tks)]
  feats <- LAB_CLUSTER_FEATURES
  rows <- lapply(all_tks, function(sym) {
    ra <- a[match(sym, a$ticker), , drop = FALSE]
    rb <- b[match(sym, b$ticker), , drop = FALSE]
    has_a <- nrow(ra) == 1L && !all(is.na(ra$ticker))
    has_b <- nrow(rb) == 1L && !all(is.na(rb$ticker))
    out <- list(
      ticker = sym,
      name = NA_character_,
      market_cap = NA_real_
    )
    for (f in feats) out[[f]] <- NA_real_
    if (has_a) {
      nm <- as.character(ra$name[[1]])
      if (nzchar(nm) && !is.na(nm)) out$name <- nm
      mc <- suppressWarnings(as.numeric(ra$market_cap[[1]]))
      if (is.finite(mc)) out$market_cap <- mc
      for (f in feats) {
        v <- suppressWarnings(as.numeric(ra[[f]][[1]]))
        if (is.finite(v)) out[[f]] <- v
      }
    }
    if (has_b) {
      if (is.na(out$name) || !nzchar(out$name)) {
        nm <- as.character(rb$name[[1]])
        if (nzchar(nm) && !is.na(nm)) out$name <- nm
      }
      if (!is.finite(out$market_cap)) {
        mc <- suppressWarnings(as.numeric(rb$market_cap[[1]]))
        if (is.finite(mc)) out$market_cap <- mc
      }
      for (f in feats) {
        if (!is.finite(out[[f]])) {
          v <- suppressWarnings(as.numeric(rb[[f]][[1]]))
          if (is.finite(v)) out[[f]] <- v
        }
      }
    }
    out
  })
  lab_cluster_features_to_df(rows)
}

#' Mint Yahoo crumb + cookie handle (required since quoteSummary returns 401 without it)
.lab_yahoo_crumb_session <- function(timeout_sec = 12, max_attempts = 3L) {
  if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
  ua <- paste0(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/122.0.0.0 Safari/537.36"
  )
  last_err <- "no_crumb"
  max_attempts <- as.integer(max_attempts)[1]
  if (!is.finite(max_attempts) || max_attempts < 1L) max_attempts <- 3L
  for (attempt in seq_len(max_attempts)) {
    h <- httr::handle("https://finance.yahoo.com")
    to <- as.numeric(timeout_sec)[1]
    tryCatch(
      httr::GET(
        "https://fc.yahoo.com",
        handle = h,
        httr::add_headers(`User-Agent` = ua),
        httr::timeout(to)
      ),
      error = function(e) NULL
    )
    tryCatch(
      httr::GET(
        "https://finance.yahoo.com/",
        handle = h,
        httr::add_headers(`User-Agent` = ua),
        httr::timeout(to)
      ),
      error = function(e) NULL
    )
    crumb <- NULL
    rate_hit <- FALSE
    for (crumb_url in c(
      "https://query2.finance.yahoo.com/v1/test/getcrumb",
      "https://query1.finance.yahoo.com/v1/test/getcrumb"
    )) {
      res <- tryCatch(
        httr::GET(
          crumb_url,
          handle = h,
          httr::add_headers(`User-Agent` = ua, Accept = "text/plain"),
          httr::timeout(to)
        ),
        error = function(e) NULL
      )
      if (is.null(res)) next
      sc <- httr::status_code(res)
      txt <- tryCatch(
        httr::content(res, as = "text", encoding = "UTF-8"),
        error = function(e) ""
      )
      if (identical(sc, 429L) || grepl("Too Many Requests", txt, fixed = TRUE)) {
        rate_hit <- TRUE
        last_err <- "rate_limited"
        break
      }
      if (sc >= 400L) next
      if (!nzchar(txt) || grepl("<html>|Unauthorized|Invalid", txt, ignore.case = TRUE)) {
        next
      }
      crumb <- trimws(txt)
      if (nzchar(crumb)) {
        return(list(handle = h, crumb = crumb, ua = ua, error = NULL))
      }
    }
    if (isTRUE(rate_hit) && attempt < max_attempts) {
      Sys.sleep(1.2 * attempt)
      next
    }
    if (!is.null(crumb) && nzchar(crumb)) {
      return(list(handle = h, crumb = crumb, ua = ua, error = NULL))
    }
  }
  list(handle = httr::handle("https://finance.yahoo.com"), crumb = NULL, ua = ua, error = last_err)
}

#' R-only Yahoo quoteSummary fallback (crumb+cookie; no Python / reticulate)
lab_fetch_cluster_features_r <- function(tickers, timeout_sec = 12) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  empty <- lab_cluster_features_to_df(NULL)
  if (!length(tks)) return(empty)
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("httr/jsonlite required for R Yahoo feature fallback.")
  }

  sess <- .lab_yahoo_crumb_session(timeout_sec = timeout_sec)
  if (is.null(sess) || is.null(sess$crumb) || !nzchar(sess$crumb)) {
    err <- if (!is.null(sess)) sess$error else "session_failed"
    # Empty shells must NOT be treated as coverage — attach reason for caller
    out <- empty
    attr(out, "yahoo_r_error") <- err %||% "no_crumb"
    return(out)
  }

  empty_row <- function(sym) {
    list(
      ticker = sym,
      name = NA_character_,
      market_cap = NA_real_,
      ROE = NA_real_,
      Operating_Margin = NA_real_,
      Rev_YoY = NA_real_,
      OpInc_YoY = NA_real_,
      Debt_Ratio = NA_real_,
      PE_Ratio = NA_real_,
      PB_Ratio = NA_real_
    )
  }

  refresh_crumb <- function() {
    sess <<- .lab_yahoo_crumb_session(timeout_sec = timeout_sec)
    !is.null(sess) && !is.null(sess$crumb) && nzchar(sess$crumb)
  }

  fetch_one <- function(sym) {
    row <- empty_row(sym)
    hosts <- c(
      "https://query2.finance.yahoo.com",
      "https://query1.finance.yahoo.com"
    )
    for (attempt in 1:2) {
      auth_retry <- FALSE
      for (host in hosts) {
        url <- sprintf(
          "%s/v10/finance/quoteSummary/%s",
          host,
          utils::URLencode(sym, reserved = TRUE)
        )
        res <- tryCatch(
          httr::GET(
            url,
            handle = sess$handle,
            query = list(
              modules = "defaultKeyStatistics,financialData,summaryDetail,price",
              crumb = sess$crumb
            ),
            httr::add_headers(
              `User-Agent` = sess$ua,
              Accept = "application/json"
            ),
            httr::timeout(as.numeric(timeout_sec)[1])
          ),
          error = function(e) NULL
        )
        if (is.null(res)) next
        sc <- httr::status_code(res)
        if (identical(sc, 429L)) {
          # Back off once then retry same symbol (Yahoo bursts after N≈25–100)
          Sys.sleep(1.5)
          res2 <- tryCatch(
            httr::GET(
              url,
              handle = sess$handle,
              query = list(
                modules = "defaultKeyStatistics,financialData,summaryDetail,price",
                crumb = sess$crumb
              ),
              httr::add_headers(
                `User-Agent` = sess$ua,
                Accept = "application/json"
              ),
              httr::timeout(as.numeric(timeout_sec)[1])
            ),
            error = function(e) NULL
          )
          if (!is.null(res2) && identical(httr::status_code(res2), 200L)) {
            res <- res2
            sc <- 200L
          } else {
            attr(row, "yahoo_r_error") <- "rate_limited"
            return(row)
          }
        }
        if (identical(sc, 401L) || identical(sc, 403L)) {
          if (attempt == 1L && isTRUE(refresh_crumb())) {
            auth_retry <- TRUE
            break
          }
          next
        }
        if (sc >= 400L) next
        body <- tryCatch(
          httr::content(res, as = "parsed", type = "application/json"),
          error = function(e) NULL
        )
        result <- tryCatch(body$quoteSummary$result[[1]], error = function(e) NULL)
        if (is.null(result)) next
        fd <- result$financialData %||% list()
        ks <- result$defaultKeyStatistics %||% list()
        sd <- result$summaryDetail %||% list()
        pr <- result$price %||% list()
        nm <- pr$shortName %||% pr$longName %||% NA_character_
        if (!is.null(nm) && !is.na(nm)) row$name <- as.character(nm)[1]
        row$market_cap <- .lab_cluster_yahoo_raw(pr$marketCap %||% ks$enterpriseValue)
        row$ROE <- .lab_cluster_pct_points(fd$returnOnEquity)
        row$Operating_Margin <- .lab_cluster_pct_points(fd$operatingMargins)
        row$Rev_YoY <- .lab_cluster_pct_points(fd$revenueGrowth)
        row$OpInc_YoY <- .lab_cluster_pct_points(
          fd$earningsGrowth %||% ks$earningsQuarterlyGrowth
        )
        row$Debt_Ratio <- .lab_cluster_debt_pct(fd$debtToEquity)
        pe <- .lab_cluster_yahoo_raw(sd$trailingPE)
        if (!is.finite(pe)) pe <- .lab_cluster_yahoo_raw(ks$forwardPE)
        pb <- .lab_cluster_yahoo_raw(sd$priceToBook %||% ks$priceToBook)
        if (is.finite(pe) && pe > 0) row$PE_Ratio <- pe
        if (is.finite(pb) && pb > 0) row$PB_Ratio <- pb
        return(row)
      }
      if (!isTRUE(auth_retry)) break
    }
    row
  }

  rows <- vector("list", length(tks))
  for (i in seq_along(tks)) {
    if (i > 1L) Sys.sleep(0.18)
    rows[[i]] <- fetch_one(tks[[i]])
  }

  out <- lab_cluster_features_to_df(rows)
  n_ok_out <- lab_cluster_usable_feature_rows(out)
  if (n_ok_out < 1L) {
    # Prefer first per-row rate_limited / auth signal over generic empty
    row_errs <- vapply(rows, function(r) {
      ae <- attr(r, "yahoo_r_error")
      if (is.null(ae)) "" else as.character(ae)[1]
    }, character(1))
    row_errs <- row_errs[nzchar(row_errs)]
    attr(out, "yahoo_r_error") <- if (length(row_errs)) {
      row_errs[[1]]
    } else {
      sess$error %||% "empty_quoteSummary"
    }
  }
  out
}


#' Path to bundled offline Clustering feature snapshot (CSV)
lab_cluster_features_snapshot_path <- function() {
  candidates <- c(
    file.path("data", "cluster_features_snapshot.csv"),
    file.path("app_17.0", "data", "cluster_features_snapshot.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

#' Load bundled offline ratio features (durable when Yahoo crumb is blocked)
lab_load_cluster_features_snapshot <- function(tickers = NULL) {
  path <- lab_cluster_features_snapshot_path()
  empty <- lab_cluster_features_to_df(NULL)
  if (!nzchar(path) || is.na(path) || !file.exists(path)) {
    attr(empty, "snapshot_source") <- "missing"
    return(empty)
  }
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0L) {
    attr(empty, "snapshot_source") <- "empty"
    return(empty)
  }
  df <- lab_cluster_features_to_df(raw)
  if (!is.null(tickers) && length(tickers)) {
    want <- unique(toupper(trimws(as.character(tickers))))
    want <- want[nzchar(want) & !is.na(want)]
    df <- df[df$ticker %in% want, , drop = FALSE]
  }
  snap_at <- if ("snapshot_at" %in% names(raw)) {
    as.character(raw$snapshot_at[[1]])
  } else {
    NA_character_
  }
  attr(df, "snapshot_source") <- path
  attr(df, "snapshot_at") <- snap_at
  rownames(df) <- NULL
  df
}

#' Fetch Clustering features: live Yahoo overlay + durable offline snapshot fallback
#' Chunks large universes to reduce Yahoo 429s (N=100 is especially fragile).
lab_fetch_cluster_features <- function(tickers, chunk_size = 20L) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  # Keep TW/TWO suffixes intact (do not strip dots)
  if (!length(tks)) return(lab_cluster_features_to_df(NULL))

  `%||%` <- function(a, b) if (is.null(a)) b else a
  py_err <- NULL
  r_err <- NULL
  snap_err <- NULL
  df <- NULL
  used_snapshot <- FALSE
  chunk_size <- as.integer(chunk_size)[1]
  if (!is.finite(chunk_size) || chunk_size < 5L) chunk_size <- 20L
  if (chunk_size > 40L) chunk_size <- 40L

  # Durable base layer: bundled snapshot (works when Yahoo datacenter blocks crumb)
  snap <- tryCatch(
    lab_load_cluster_features_snapshot(tks),
    error = function(e) {
      snap_err <<- conditionMessage(e)
      NULL
    }
  )
  n_snap <- lab_cluster_usable_feature_rows(snap)
  if (!is.null(snap) && nrow(snap) > 0L && n_snap > 0L) {
    df <- snap
    used_snapshot <- TRUE
  }

  .py_fetch_chunk <- function(chunk) {
    fn <- NULL
    if (exists(".py_scraper_env") &&
        exists("get_cluster_features_batch", envir = .py_scraper_env, inherits = FALSE, mode = "function")) {
      fn <- get("get_cluster_features_batch", envir = .py_scraper_env, inherits = FALSE)
    } else if (exists("get_cluster_features_batch", mode = "function")) {
      fn <- get_cluster_features_batch
    }
    if (!is.function(fn)) {
      py_err <<- "get_cluster_features_batch not found"
      return(NULL)
    }
    tryCatch({
      raw <- fn(as.list(chunk))
      if (!is.data.frame(raw) && !is.list(raw) &&
          requireNamespace("reticulate", quietly = TRUE)) {
        raw <- tryCatch(reticulate::py_to_r(raw), error = function(e) raw)
      }
      lab_cluster_features_to_df(raw)
    }, error = function(e) {
      py_err <<- conditionMessage(e)
      NULL
    })
  }

  ready <- exists(".ensure_python_scraper", mode = "function") &&
    isTRUE(tryCatch(.ensure_python_scraper(), error = function(e) FALSE))
  if (isTRUE(ready)) {
    chunks <- split(tks, ceiling(seq_along(tks) / chunk_size))
    for (ci in seq_along(chunks)) {
      if (ci > 1L) Sys.sleep(0.55)
      part <- .py_fetch_chunk(chunks[[ci]])
      if (!is.null(part) && nrow(part) > 0L) {
        # Live Yahoo preferred over snapshot when finite
        df <- lab_cluster_merge_feature_dfs(part, df)
      }
    }
  } else {
    py_err <- "Python scraper unavailable"
  }

  n_ok <- lab_cluster_usable_feature_rows(df)
  # Gap-fill sparse / missing tickers via R crumb path (also covers cold Python)
  need_r <- lab_cluster_sparse_tickers(df, tks)
  # Prefer filling a usable core first when universe is huge (N=100 rate-limit)
  if (length(need_r) > 40L && n_ok < 8L) {
    need_r <- need_r[seq_len(40L)]
  }
  if (length(need_r) > 0L) {
    r_chunks <- split(need_r, ceiling(seq_along(need_r) / chunk_size))
    for (ri in seq_along(r_chunks)) {
      if (ri > 1L) Sys.sleep(0.7)
      df_r <- tryCatch(
        lab_fetch_cluster_features_r(r_chunks[[ri]]),
        error = function(e) {
          r_err <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(df_r)) {
        r_attr <- attr(df_r, "yahoo_r_error")
        if (!is.null(r_attr) && nzchar(as.character(r_attr)[1])) {
          r_err <- as.character(r_attr)[1]
        }
        n_ok_r <- lab_cluster_usable_feature_rows(df_r)
        if (n_ok_r > 0L) {
          df <- lab_cluster_merge_feature_dfs(df_r, df)
          n_ok <- lab_cluster_usable_feature_rows(df)
        }
      }
      # Do not abandon remaining sparse names while any requested ticker is still empty
      still_sparse <- lab_cluster_sparse_tickers(df, tks)
      if (!length(still_sparse)) break
    }
  }

  # If live Yahoo failed, re-merge full snapshot for requested tickers
  if (n_ok < 2L) {
    snap2 <- tryCatch(
      lab_load_cluster_features_snapshot(tks),
      error = function(e) NULL
    )
    if (!is.null(snap2) && lab_cluster_usable_feature_rows(snap2) > 0L) {
      df <- lab_cluster_merge_feature_dfs(df, snap2)
      n_ok <- lab_cluster_usable_feature_rows(df)
      used_snapshot <- TRUE
    }
  }

  if (is.null(df) || nrow(df) == 0L || n_ok < 2L) {
    detail <- paste0(
      "python: ", py_err %||% "n/a",
      if (!is.null(r_err)) paste0("; R: ", r_err) else "",
      if (!is.null(snap_err)) paste0("; snapshot: ", snap_err) else "",
      "; snapshot_usable=", n_snap
    )
    stop(sprintf(
      paste0(
        "Ratio features unavailable for clustering ",
        "(%d/%d tickers with ≥2 finite ratios; %s). ",
        "Offline snapshot missing or incomplete for this universe — ",
        "retry later or lower Universe size (N)."
      ),
      n_ok, length(tks), detail
    ))
  }
  attr(df, "used_snapshot") <- used_snapshot
  attr(df, "snapshot_at") <- if (!is.null(snap)) attr(snap, "snapshot_at") else NA_character_
  attr(df, "n_live_ok") <- n_ok
  df
}

#' Match a session/focus ticker against a ticker vector (TW suffix / BRK.B↔BRK-B)
lab_cluster_match_ticker <- function(tickers, focus) {
  tks <- toupper(trimws(as.character(tickers)))
  foc <- toupper(trimws(as.character(focus %||% "")[1]))
  if (!nzchar(foc) || !length(tks)) return(NA_character_)
  if (foc %in% tks) return(foc)
  bare <- function(x) sub("\\.(TW|TWO)$", "", x, ignore.case = TRUE)
  hit <- which(bare(tks) == bare(foc))
  if (length(hit)) return(tks[[hit[[1]]]])
  foc2 <- gsub("\\.", "-", foc)
  tks2 <- gsub("\\.", "-", tks)
  hit <- which(tks2 == foc2)
  if (length(hit)) return(tks[[hit[[1]]]])
  NA_character_
}

#' Drop excess rows while keeping focus, using the same truncate priority as
#' lab_select_eval_pool (lowest mcap / lowest 1Y return / trailing random slots).
lab_cluster_drop_excess_keeping_focus <- function(pool, keep_focus, max_n,
                                                 rank_mode = "mcap") {
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0L) return(pool)
  max_n <- lab_resolve_im_max_n(max_n, custom = NULL, lo = 1L, hi = 500L)
  if (!is.finite(max_n) || nrow(pool) <= max_n) return(pool)
  keep_focus <- as.integer(keep_focus)[1]
  if (!is.finite(keep_focus) || keep_focus < 1L || keep_focus > nrow(pool)) {
    keep_focus <- NA_integer_
  }
  drop_cand <- setdiff(seq_len(nrow(pool)), keep_focus)
  n_drop <- nrow(pool) - as.integer(max_n)
  if (!length(drop_cand) || n_drop <= 0L) return(pool)
  mode <- lab_normalize_pool_rank_mode(rank_mode)
  tk <- as.character(pool$ticker)
  if (identical(mode, "ret_1y") && "ret_1y" %in% names(pool)) {
    ret <- suppressWarnings(as.numeric(pool$ret_1y))
    ret[!is.finite(ret)] <- -Inf
    ord <- drop_cand[order(ret[drop_cand], tk[drop_cand], na.last = FALSE)]
  } else if (identical(mode, "random")) {
    # Prefer dropping trailing slots (preserve earlier random sample order)
    ord <- rev(drop_cand)
  } else {
    # mcap / concept (mcap within): drop lowest market cap first; missing last
    mcap <- suppressWarnings(as.numeric(pool$market_cap))
    mcap[!is.finite(mcap)] <- Inf
    ord <- drop_cand[order(mcap[drop_cand], tk[drop_cand])]
  }
  pool[-ord[seq_len(min(n_drop, length(ord)))], , drop = FALSE]
}

#' Force-include ensure_ticker in a capped pool (drop by truncate mode if over N).
#' If the ticker is absent from catalog, inject a minimal row so Search stock still enters N.
lab_cluster_ensure_ticker_in_pool <- function(pool, catalog, ensure_ticker, max_n,
                                              rank_mode = "mcap") {
  ensure_raw <- toupper(trimws(as.character(ensure_ticker %||% "")[1]))
  if (!nzchar(ensure_raw) || is.null(pool) || !is.data.frame(pool)) return(pool)
  mode <- lab_normalize_pool_rank_mode(rank_mode)
  matched <- lab_cluster_match_ticker(pool$ticker, ensure_raw)
  if (!is.na(matched)) {
    # Already in pool — still enforce max_n without dropping the focus
    max_n <- lab_resolve_im_max_n(max_n, custom = NULL, lo = 1L, hi = 500L)
    if (is.finite(max_n) && nrow(pool) > max_n) {
      keep_focus <- which(toupper(trimws(as.character(pool$ticker))) == matched)[1]
      if (!is.finite(keep_focus)) {
        keep_focus <- which(!is.na(lab_cluster_match_ticker(pool$ticker, ensure_raw)))[1]
      }
      pool <- lab_cluster_drop_excess_keeping_focus(
        pool, keep_focus, max_n, rank_mode = mode
      )
    }
    return(pool)
  }
  # Prefer unfiltered catalog so the session ticker can enter even if filters excluded it
  src <- catalog
  if (is.null(src) || !is.data.frame(src) || !"ticker" %in% names(src)) {
    src <- pool
  }
  if (!is.null(src) && is.data.frame(src) && "ticker" %in% names(src) && nrow(src) > 0L) {
    src$ticker <- toupper(trimws(as.character(src$ticker)))
    matched <- lab_cluster_match_ticker(src$ticker, ensure_raw)
  } else {
    matched <- NA_character_
  }
  if (is.na(matched)) {
    # Not in Blue Chip catalog — still inject Search ticker as a minimal row
    matched <- ensure_raw
    row <- data.frame(
      ticker = matched,
      industry_key = NA_character_,
      industry_label = NA_character_,
      market_cap = NA_real_,
      stringsAsFactors = FALSE
    )
  } else {
    row <- src[src$ticker == matched, , drop = FALSE][1, , drop = FALSE]
  }
  need_cols <- unique(c(
    names(pool), "ticker", "industry_key", "industry_label", "market_cap", "ret_1y"
  ))
  for (nm in setdiff(need_cols, names(row))) row[[nm]] <- NA
  for (nm in setdiff(need_cols, names(pool))) {
    pool[[nm]] <- if (nm == "ticker") character(nrow(pool)) else NA
  }
  pool <- rbind(pool[, need_cols, drop = FALSE], row[, need_cols, drop = FALSE])
  pool <- lab_dedupe_eval_pool(pool)
  max_n <- lab_resolve_im_max_n(max_n, custom = NULL, lo = 1L, hi = 500L)
  if (is.finite(max_n) && nrow(pool) > max_n) {
    keep_focus <- which(toupper(trimws(as.character(pool$ticker))) == matched)[1]
    if (!is.finite(keep_focus)) {
      keep_focus <- which(!is.na(vapply(
        pool$ticker, function(t) lab_cluster_match_ticker(t, ensure_raw), character(1)
      )))[1]
    }
    pool <- lab_cluster_drop_excess_keeping_focus(
      pool, keep_focus, max_n, rank_mode = mode
    )
  }
  pool
}

#' Ensure Search / session ticker has a feature row (shell OK; clustering may impute).
lab_cluster_ensure_ticker_in_features <- function(feats, ensure_ticker) {
  ensure_raw <- toupper(trimws(as.character(ensure_ticker %||% "")[1]))
  if (!nzchar(ensure_raw)) {
    return(if (is.null(feats)) lab_cluster_features_to_df(NULL) else feats)
  }
  if (is.null(feats) || !is.data.frame(feats)) {
    feats <- lab_cluster_features_to_df(NULL)
  }
  matched <- lab_cluster_match_ticker(feats$ticker, ensure_raw)
  if (!is.na(matched) && nzchar(matched)) return(feats)
  row <- list(
    ticker = ensure_raw,
    name = ensure_raw,
    market_cap = NA_real_
  )
  for (f in LAB_CLUSTER_FEATURES) row[[f]] <- NA_real_
  lab_cluster_merge_feature_dfs(feats, lab_cluster_features_to_df(list(row)))
}

#' Solo re-fetch for Search / radar-focus when still <2 finite ratios.
#' Tries Python batch → R crumb → offline snapshot (fuzzy match).
lab_cluster_priority_refill_features <- function(feats, ticker) {
  ensure_raw <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(ensure_raw)) {
    return(if (is.null(feats)) lab_cluster_features_to_df(NULL) else feats)
  }
  if (is.null(feats) || !is.data.frame(feats)) {
    feats <- lab_cluster_features_to_df(NULL)
  }
  n0 <- lab_cluster_n_finite_for_ticker(feats, ensure_raw)
  if (n0 >= 2L) return(feats)

  # 1) Python solo
  py_part <- NULL
  ready <- exists(".ensure_python_scraper", mode = "function") &&
    isTRUE(tryCatch(.ensure_python_scraper(), error = function(e) FALSE))
  if (isTRUE(ready)) {
    fn <- NULL
    if (exists(".py_scraper_env") &&
        exists("get_cluster_features_batch", envir = .py_scraper_env, inherits = FALSE, mode = "function")) {
      fn <- get("get_cluster_features_batch", envir = .py_scraper_env, inherits = FALSE)
    } else if (exists("get_cluster_features_batch", mode = "function")) {
      fn <- get_cluster_features_batch
    }
    if (is.function(fn)) {
      py_part <- tryCatch({
        raw <- fn(as.list(ensure_raw))
        if (!is.data.frame(raw) && !is.list(raw) &&
            requireNamespace("reticulate", quietly = TRUE)) {
          raw <- tryCatch(reticulate::py_to_r(raw), error = function(e) raw)
        }
        lab_cluster_features_to_df(raw)
      }, error = function(e) NULL)
    }
  }
  if (!is.null(py_part) && nrow(py_part) > 0L) {
    feats <- lab_cluster_merge_feature_dfs(py_part, feats)
  }
  n1 <- lab_cluster_n_finite_for_ticker(feats, ensure_raw)
  if (n1 >= 2L) {
    return(feats)
  }

  # 2) R crumb solo
  r_part <- tryCatch(
    lab_fetch_cluster_features_r(ensure_raw),
    error = function(e) NULL
  )
  if (!is.null(r_part) && nrow(r_part) > 0L &&
      lab_cluster_usable_feature_rows(r_part) > 0L) {
    feats <- lab_cluster_merge_feature_dfs(r_part, feats)
  }
  n2 <- lab_cluster_n_finite_for_ticker(feats, ensure_raw)
  if (n2 >= 2L) {
    return(feats)
  }

  # 3) Offline snapshot (exact + bare TW / BRK variants)
  snap <- tryCatch(
    lab_load_cluster_features_snapshot(NULL),
    error = function(e) NULL
  )
  if (!is.null(snap) && nrow(snap) > 0L) {
    hit <- lab_cluster_match_ticker(snap$ticker, ensure_raw)
    if (!is.na(hit) && nzchar(hit)) {
      feats <- lab_cluster_merge_feature_dfs(
        snap[toupper(trimws(as.character(snap$ticker))) == hit, , drop = FALSE],
        feats
      )
    }
  }
  feats
}

#' Build evaluation pool for clustering from Blue Chip catalog filters
lab_cluster_build_pool <- function(catalog, industry_filter = NULL, method_filter = NULL,
                                   max_n = 50L, ensure_ticker = NULL,
                                   rank_mode = "mcap", concept_keys = NULL,
                                   market_mode = "US") {
  empty <- data.frame(
    ticker = character(0),
    industry_key = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(catalog) || !is.data.frame(catalog) || nrow(catalog) == 0L) {
    # Still seed Universe with the Search ticker when catalog is empty
    seeded <- lab_cluster_ensure_ticker_in_pool(empty, catalog, ensure_ticker, max_n = 1L)
    if (is.null(seeded) || nrow(seeded) == 0L) return(empty)
    cols <- intersect(c("ticker", "industry_key", "industry_label", "market_cap"), names(seeded))
    return(seeded[, cols, drop = FALSE])
  }
  pool <- lab_merge_catalog_scores(
    catalog,
    scores = NULL,
    method_filter = method_filter,
    industry_filter = industry_filter,
    eq_only = FALSE,
    gate_only = FALSE
  )
  pool <- lab_dedupe_eval_pool(pool)
  if (is.null(pool) || nrow(pool) == 0L) {
    # Filters emptied the pool — still try to seed with the session ticker alone
    pool <- lab_cluster_ensure_ticker_in_pool(empty, catalog, ensure_ticker, max_n = 1L)
    if (is.null(pool) || nrow(pool) == 0L) return(empty)
    cols <- intersect(c("ticker", "industry_key", "industry_label", "market_cap"), names(pool))
    return(pool[, cols, drop = FALSE])
  }
  max_n <- lab_resolve_im_max_n(max_n, custom = NULL, lo = 1L, hi = 500L)
  mode <- lab_normalize_pool_rank_mode(rank_mode)
  if (is.finite(max_n) && nrow(pool) > max_n &&
      mode %in% c("mcap", "concept")) {
    pool <- lab_attach_market_caps(pool)
  }
  pool <- lab_select_eval_pool(
    pool,
    max_n = max_n,
    mode = mode,
    concept_keys = concept_keys,
    market_mode = market_mode,
    seed = as.integer(Sys.time())
  )
  mode_used <- as.character(attr(pool, "pool_rank_mode") %||% mode)[1]
  note_used <- as.character(attr(pool, "pool_rank_note") %||% "")[1]
  n_filtered <- as.integer(attr(pool, "n_filtered") %||% nrow(pool))[1]
  used_mcap <- isTRUE(attr(pool, "used_market_cap"))
  pool <- lab_cluster_ensure_ticker_in_pool(
    pool, catalog, ensure_ticker, max_n = max_n, rank_mode = mode_used
  )
  cols <- intersect(
    c("ticker", "industry_key", "industry_label", "market_cap", "ret_1y", "primary"),
    names(pool)
  )
  out <- pool[, cols, drop = FALSE]
  attr(out, "pool_rank_mode") <- mode_used
  attr(out, "pool_rank_note") <- note_used
  attr(out, "n_filtered") <- n_filtered
  attr(out, "used_market_cap") <- used_mcap
  attr(out, "max_n") <- max_n
  out
}

#' Semantic labels from cluster-center means (research labels, not buy signals)
lab_cluster_semantic_labels <- function(centers_df, locale = "en") {
  loc_raw <- as.character(if (is.null(locale) || !length(locale)) "en" else locale)[1]
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(loc_raw)
  } else {
    loc_raw
  }
  is_zh <- grepl("^zh", tolower(as.character(loc)), perl = TRUE)
  labels <- character(nrow(centers_df))
  for (i in seq_len(nrow(centers_df))) {
    row <- centers_df[i, , drop = FALSE]
    roe <- suppressWarnings(as.numeric(row[["ROE"]])[1])
    rev <- suppressWarnings(as.numeric(row[["Rev_YoY"]])[1])
    pe <- suppressWarnings(as.numeric(row[["PE_Ratio"]])[1])
    debt <- suppressWarnings(as.numeric(row[["Debt_Ratio"]])[1])
    if (is.finite(roe) && is.finite(rev) && roe > 20 && rev > 15) {
      labels[i] <- if (is_zh) "高能成長明星" else "High-energy growth"
    } else if (is.finite(roe) && is.finite(pe) && roe > 10 && pe < 12) {
      labels[i] <- if (is_zh) "成熟穩健價值" else "Mature value"
    } else if (is.finite(debt) && is.finite(roe) && debt > 70 && roe < 5) {
      labels[i] <- if (is_zh) "高槓桿風險股" else "High-leverage risk"
    } else {
      labels[i] <- if (is_zh) "中性均衡防禦" else "Balanced defensive"
    }
  }
  if (any(duplicated(labels))) {
    for (lab in unique(labels)) {
      idx <- which(labels == lab)
      if (length(idx) > 1L) {
        labels[idx] <- paste0(lab, " #", seq_along(idx))
      }
    }
  }
  labels
}

#' Run Winsorize → median impute → scale → K-Means clustering
#' @param ensure_ticker optional Search ticker that must remain after missing-data filter
lab_run_stock_clustering <- function(df_financials, k_clusters = 3L,
                                     features = LAB_CLUSTER_FEATURES,
                                     locale = "en", seed = 42L,
                                     ensure_ticker = NULL) {
  k_clusters <- as.integer(k_clusters)[1]
  if (!is.finite(k_clusters) || k_clusters < 2L) k_clusters <- 2L
  if (k_clusters > 8L) k_clusters <- 8L

  df <- as.data.frame(df_financials, stringsAsFactors = FALSE)
  if (nrow(df) < k_clusters) {
    stop(sprintf("Need at least %d stocks with features; got %d.", k_clusters, nrow(df)))
  }
  feats <- intersect(as.character(features), names(df))
  if (length(feats) < 3L) stop("Too few clustering features available.")

  X <- df[, feats, drop = FALSE]
  for (f in feats) {
    X[[f]] <- lab_cluster_winsorize(X[[f]])
  }
  for (f in feats) {
    col <- X[[f]]
    med <- stats::median(col[is.finite(col)], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    col[!is.finite(col)] <- med
    X[[f]] <- col
  }

  n_finite_orig <- as.integer(rowSums(do.call(
    cbind,
    lapply(feats, function(f) is.finite(suppressWarnings(as.numeric(df[[f]]))))
  ), na.rm = TRUE))
  keep <- n_finite_orig >= 2L
  # Search ticker must stay in Universe / radar even if Yahoo ratios are sparse
  ensure_hit <- lab_cluster_match_ticker(df$ticker, ensure_ticker)
  if (!is.na(ensure_hit) && nzchar(ensure_hit)) {
    keep[toupper(trimws(as.character(df$ticker))) == ensure_hit] <- TRUE
  }
  if (sum(keep) < k_clusters) {
    stop(sprintf(
      paste0(
        "After missing-data filter, only %d stocks remain (need ≥ %d). ",
        "Each name needs ≥2 finite Yahoo ratios among: ",
        paste(feats, collapse = ", "), "."
      ),
      sum(keep), k_clusters
    ))
  }
  df <- df[keep, , drop = FALSE]
  X <- X[keep, , drop = FALSE]
  n_finite_kept <- as.integer(n_finite_orig[keep])
  rownames(df) <- NULL
  rownames(X) <- NULL

  Xs <- scale(as.matrix(X))
  Xs[!is.finite(Xs)] <- 0

  set.seed(as.integer(seed)[1])
  km <- stats::kmeans(Xs, centers = k_clusters, nstart = 25L, iter.max = 100L)
  df$Cluster_ID <- as.integer(km$cluster)
  # Pre-impute coverage (usable-row rule uses ≥2 finite Yahoo ratios)
  df$n_finite <- n_finite_kept

  # Write back winsorized+imputed ratios so radar / assignments show the
  # values used for clustering (raw all-NA Search shells otherwise plot as r=0).
  for (f in feats) {
    df[[f]] <- as.numeric(X[[f]])
  }

  centers <- as.data.frame(
    stats::aggregate(X, by = list(Cluster_ID = df$Cluster_ID), FUN = mean)
  )
  lab_vec <- lab_cluster_semantic_labels(centers, locale = locale)
  lab_map <- stats::setNames(lab_vec, as.character(centers$Cluster_ID))
  df$Cluster_Label <- unname(lab_map[as.character(df$Cluster_ID)])

  list(
    data = df,
    centers = centers,
    features = feats,
    k = k_clusters,
    n = nrow(df),
    tot_withinss = km$tot.withinss,
    betweenss = km$betweenss,
    labels = lab_map
  )
}

#' Pick same-cluster peers for radar (exclude focus; prefer closest in feature space)
lab_cluster_radar_peers <- function(result, focus_ticker, n_peers = 3L) {
  df <- result$data
  focus <- lab_cluster_match_ticker(df$ticker, focus_ticker)
  if (is.na(focus) || !nzchar(focus)) return(character(0))
  cid <- df$Cluster_ID[match(focus, df$ticker)]
  peers <- df[df$Cluster_ID == cid & df$ticker != focus, , drop = FALSE]
  if (nrow(peers) == 0L) return(character(0))
  feats <- result$features
  focus_row <- as.numeric(df[df$ticker == focus, feats, drop = FALSE][1, ])
  peer_mat <- as.matrix(peers[, feats, drop = FALSE])
  d <- sqrt(rowSums(
    (peer_mat - matrix(focus_row, nrow = nrow(peer_mat), ncol = length(focus_row), byrow = TRUE))^2
  ))
  d[!is.finite(d)] <- Inf
  o <- order(d, peers$ticker)
  utils::head(peers$ticker[o], as.integer(n_peers)[1])
}

#' Plotly 2D cluster scatter
#' @param focus_ticker optional Radar focus; drawn as a red marker on top
lab_cluster_scatter_plotly <- function(result, x_feat = "ROE", y_feat = "PE_Ratio",
                                       locale = "en", focus_ticker = NULL) {
  df <- result$data
  empty_msg <- function(msg) {
    plotly::plotly_empty() %>%
      plotly::layout(annotations = list(list(text = msg, showarrow = FALSE, font = list(size = 14))))
  }
  if (is.null(df) || nrow(df) == 0L) return(empty_msg("No cluster data"))
  if (!x_feat %in% names(df)) x_feat <- result$features[[1]]
  if (!y_feat %in% names(df)) y_feat <- result$features[[min(2L, length(result$features))]]
  loc <- if (exists("normalize_ui_locale", mode = "function")) normalize_ui_locale(locale) else as.character(locale)[1]
  is_zh <- grepl("^zh", tolower(as.character(loc)), perl = TRUE)
  xlab <- names(LAB_CLUSTER_AXIS_CHOICES)[match(x_feat, LAB_CLUSTER_AXIS_CHOICES)]
  ylab <- names(LAB_CLUSTER_AXIS_CHOICES)[match(y_feat, LAB_CLUSTER_AXIS_CHOICES)]
  if (is.na(xlab)) xlab <- x_feat
  if (is.na(ylab)) ylab <- y_feat
  title <- if (is_zh) {
    "基本面分群星團圖（研究用）"
  } else {
    "Fundamental Cluster Map (research)"
  }
  df$hover <- paste0(
    df$ticker, " · ", df$name, "<br>",
    df$Cluster_Label, "<br>",
    xlab, ": ", signif(df[[x_feat]], 4), "<br>",
    ylab, ": ", signif(df[[y_feat]], 4)
  )
  df$._x <- df[[x_feat]]
  df$._y <- df[[y_feat]]
  focus <- lab_cluster_match_ticker(df$ticker, focus_ticker)
  is_focus_row <- !is.na(focus) & nzchar(focus) &
    toupper(trimws(as.character(df$ticker))) == focus
  df_rest <- df[!is_focus_row, , drop = FALSE]
  df_focus <- df[is_focus_row, , drop = FALSE]

  p <- NULL
  if (nrow(df_rest) > 0L) {
    p <- plotly::plot_ly(
      data = df_rest,
      x = ~._x,
      y = ~._y,
      color = ~Cluster_Label,
      text = ~hover,
      type = "scatter",
      mode = "markers",
      hoverinfo = "text",
      marker = list(size = 11, opacity = 0.85),
      showlegend = TRUE
    )
  }
  if (nrow(df_focus) > 0L) {
    focus_name <- paste0(df_focus$ticker[[1]], " ★")
    focus_args <- list(
      data = df_focus,
      x = ~._x,
      y = ~._y,
      text = ~hover,
      name = focus_name,
      type = "scatter",
      mode = "markers",
      hoverinfo = "text",
      marker = list(
        size = 16,
        color = "#E53935",
        opacity = 1,
        line = list(color = "#B71C1C", width = 1.5)
      ),
      showlegend = TRUE
    )
    if (is.null(p)) {
      p <- do.call(plotly::plot_ly, focus_args)
    } else {
      p <- do.call(plotly::add_trace, c(list(p), focus_args))
    }
  }
  if (is.null(p)) return(empty_msg("No cluster data"))
  p %>%
    plotly::layout(
      title = list(text = title, font = list(size = 14)),
      xaxis = list(title = xlab, zeroline = FALSE),
      yaxis = list(title = ylab, zeroline = FALSE),
      legend = list(orientation = "h", y = -0.2),
      margin = list(t = 40, b = 80)
    ) %>%
    plotly::config(displayModeBar = TRUE, responsive = TRUE)
}

#' Plotly radar: focus + same-cluster peers (scaled 0–1 within the plotted set)
lab_cluster_radar_plotly <- function(result, focus_ticker, peer_tickers = NULL,
                                     locale = "en") {
  df <- result$data
  feats <- result$features
  focus_raw <- toupper(trimws(as.character(focus_ticker)[1]))
  focus <- lab_cluster_match_ticker(df$ticker, focus_raw)
  empty_msg <- function(msg) {
    plotly::plotly_empty() %>%
      plotly::layout(annotations = list(list(text = msg, showarrow = FALSE)))
  }
  if (is.na(focus) || !nzchar(focus)) return(empty_msg("Select a focus ticker"))
  if (is.null(peer_tickers) || !length(peer_tickers)) {
    peer_tickers <- lab_cluster_radar_peers(result, focus, n_peers = 3L)
  }
  tickers <- unique(c(focus, toupper(as.character(peer_tickers))))
  # Keep focus first so its trace is always drawn / named with ★
  sub <- df[match(tickers, df$ticker), , drop = FALSE]
  sub <- sub[!is.na(sub$ticker), , drop = FALSE]
  if (nrow(sub) < 1L) return(empty_msg("No peers"))
  mat <- as.matrix(sub[, feats, drop = FALSE])
  rng <- apply(mat, 2, function(col) {
    lo <- min(col, na.rm = TRUE)
    hi <- max(col, na.rm = TRUE)
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) c(0, 1) else c(lo, hi)
  })
  scaled <- sweep(mat, 2, rng[1, ], "-")
  scaled <- sweep(scaled, 2, pmax(rng[2, ] - rng[1, ], 1e-9), "/")
  scaled[!is.finite(scaled)] <- 0
  loc <- if (exists("normalize_ui_locale", mode = "function")) normalize_ui_locale(locale) else as.character(locale)[1]
  is_zh <- grepl("^zh", tolower(as.character(loc)), perl = TRUE)
  title <- if (is_zh) {
    paste0("同群雷達圖：", focus, "（研究用，非買進訊號）")
  } else {
    paste0("Same-cluster radar: ", focus, " (research, not a buy signal)")
  }
  axis_labs <- feats
  for (i in seq_along(feats)) {
    nm <- names(LAB_CLUSTER_AXIS_CHOICES)[match(feats[i], LAB_CLUSTER_AXIS_CHOICES)]
    if (!is.na(nm)) axis_labs[i] <- nm
  }
  p <- NULL
  for (i in seq_len(nrow(sub))) {
    vals <- as.numeric(scaled[i, ])
    is_focus <- identical(as.character(sub$ticker[i]), focus)
    args <- list(
      r = c(vals, vals[1]),
      theta = c(axis_labs, axis_labs[1]),
      name = paste0(sub$ticker[i], if (is_focus) " ★" else ""),
      type = "scatterpolar",
      mode = "lines",
      fill = "toself",
      line = if (is_focus) list(width = 3) else list(width = 1.5),
      opacity = if (is_focus) 1 else 0.75
    )
    if (is.null(p)) {
      p <- do.call(plotly::plot_ly, args)
    } else {
      p <- do.call(plotly::add_trace, c(list(p), args))
    }
  }
  if (is.null(p)) return(empty_msg("No peers"))
  p %>%
    plotly::layout(
      title = list(text = title, font = list(size = 14)),
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 1))),
      legend = list(orientation = "h", y = -0.15),
      margin = list(t = 40, b = 60)
    ) %>%
    plotly::config(displayModeBar = TRUE, responsive = TRUE)
}

#' TRUE when Clustering Lab has a non-empty result worth rendering.
lab_cluster_has_result <- function(res) {
  !is.null(res) &&
    is.list(res) &&
    !is.null(res$data) &&
    is.data.frame(res$data) &&
    nrow(res$data) > 0L
}

#' Coverage label for Clustering assignments (Data-limited vs OK)
#' @param n_finite integer vector of pre-impute finite feature counts
#' @param tickers character vector of tickers (same length)
#' @param search_ticker optional Search ticker — if fund_profile_fallback, force Data-limited
#' @param search_is_fallback TRUE when Search fundamental profile is fallback
#' @param locale en | zh-TW
lab_cluster_coverage_labels <- function(n_finite, tickers,
                                        search_ticker = NULL,
                                        search_is_fallback = FALSE,
                                        locale = "en") {
  loc <- as.character(locale %||% "en")[1]
  if (exists("normalize_ui_locale", mode = "function")) {
    loc <- tryCatch(normalize_ui_locale(loc), error = function(e) loc)
  }
  use_zh <- identical(loc, "zh-TW") || grepl("^zh", loc, ignore.case = TRUE)
  lab_ok <- if (isTRUE(use_zh)) "充足" else "OK"
  lab_dl <- if (isTRUE(use_zh)) "資料受限" else "Data-limited"
  nf <- suppressWarnings(as.integer(n_finite))
  tks <- toupper(trimws(as.character(tickers %||% character(0))))
  out <- ifelse(is.finite(nf) & nf >= 2L, lab_ok, lab_dl)
  out[!is.finite(nf)] <- lab_dl
  foc <- toupper(trimws(as.character(search_ticker %||% "")[1]))
  if (isTRUE(search_is_fallback) && nzchar(foc) && length(tks)) {
    matched <- lab_cluster_match_ticker(tks, foc)
    if (!is.na(matched) && nzchar(matched)) {
      out[toupper(trimws(tks)) == matched] <- lab_dl
    }
  }
  as.character(out)
}

#' Order cluster assignment rows: Radar focus / Search ticker first, then
#' truncate-logic sort (pool order when provided; else mcap / ret_1y / ticker).
lab_cluster_order_by_truncate <- function(df, rank_mode = "mcap",
                                          pin_ticker = NULL,
                                          pool_ticker_order = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)
  mode <- lab_normalize_pool_rank_mode(rank_mode)
  pin <- lab_cluster_match_ticker(df$ticker, pin_ticker)
  is_pin <- !is.na(pin) & nzchar(pin) &
    toupper(trimws(as.character(df$ticker))) == pin
  pin_df <- df[is_pin, , drop = FALSE]
  rest <- df[!is_pin, , drop = FALSE]

  if (nrow(rest) > 0L) {
    if (!is.null(pool_ticker_order) && length(pool_ticker_order)) {
      po <- toupper(trimws(as.character(pool_ticker_order)))
      po <- po[nzchar(po) & !is.na(po)]
      idx <- match(toupper(trimws(as.character(rest$ticker))), po)
      miss <- which(!is.finite(idx))
      if (length(miss)) {
        idx[miss] <- length(po) + seq_along(miss)
      }
      rest <- rest[order(idx, rest$ticker), , drop = FALSE]
    } else if (identical(mode, "ret_1y") && "ret_1y" %in% names(rest)) {
      ret <- suppressWarnings(as.numeric(rest$ret_1y))
      missing <- is.na(ret) | !is.finite(ret)
      o <- order(missing, -ifelse(missing, 0, ret), rest$ticker, na.last = TRUE)
      rest <- rest[o, , drop = FALSE]
    } else if (!identical(mode, "random") && "market_cap" %in% names(rest)) {
      mcap <- suppressWarnings(as.numeric(rest$market_cap))
      missing <- is.na(mcap) | !is.finite(mcap) | mcap <= 0
      o <- order(missing, -ifelse(missing, 0, mcap), rest$ticker, na.last = TRUE)
      rest <- rest[o, , drop = FALSE]
    } else {
      rest <- rest[order(rest$ticker), , drop = FALSE]
    }
  }

  out <- rbind(pin_df, rest)
  rownames(out) <- NULL
  attr(out, "pool_rank_mode") <- mode
  out
}

#' Round numeric columns in Cluster assignments table to 2 decimal places
#' (Cluster_ID and non-numeric identity columns left unchanged).
lab_cluster_format_assignments_df <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !ncol(df)) return(df)
  out <- df
  skip <- c(
    "ticker", "name", "Cluster_ID", "Cluster_Label", "industry_key", "industry_label",
    "n_finite", "Coverage", "資料覆蓋", "coverage"
  )
  for (nm in names(out)) {
    if (nm %in% skip) next
    if (is.numeric(out[[nm]])) {
      out[[nm]] <- round(as.numeric(out[[nm]]), 2)
    }
  }
  out
}

#' Idle placeholder HTML (used when lab_cluster_result is NULL so Plotly/DT are destroyed).
lab_cluster_idle_placeholder <- function(msg, min_height = "420px") {
  txt <- if (is.null(msg) || length(msg) < 1L || is.na(msg[[1]])) "" else as.character(msg[[1]])
  htmltools::tags$div(
    class = "ynow-lab-cluster-idle",
    style = sprintf(
      paste0(
        "min-height:%s;display:flex;align-items:center;justify-content:center;",
        "color:#666;padding:12px;text-align:center;"
      ),
      min_height
    ),
    txt
  )
}

#' Decide Clustering panel body: idle placeholder vs live output id marker.
#' Returns a list(mode=..., msg=...) for tests; UI builders use the same predicate.
lab_cluster_panel_mode <- function(res) {
  if (lab_cluster_has_result(res)) "live" else "idle"
}

#' Synthetic feature frame for unit tests (no network)
lab_cluster_synthetic_features <- function(n = 24L, seed = 1L) {
  set.seed(as.integer(seed)[1])
  n <- as.integer(n)[1]
  groups <- rep(1:4, length.out = n)
  data.frame(
    ticker = paste0("T", seq_len(n)),
    name = paste0("Synthetic ", seq_len(n)),
    market_cap = 1e9 * (n:1),
    ROE = c(28, 18, 4, 12)[groups] + stats::rnorm(n, 0, 2),
    Operating_Margin = c(22, 14, 2, 10)[groups] + stats::rnorm(n, 0, 1.5),
    Rev_YoY = c(25, 6, 1, 8)[groups] + stats::rnorm(n, 0, 3),
    OpInc_YoY = c(20, 5, -2, 6)[groups] + stats::rnorm(n, 0, 3),
    Debt_Ratio = c(35, 45, 95, 55)[groups] + stats::rnorm(n, 0, 5),
    PE_Ratio = c(32, 11, 8, 18)[groups] + stats::rnorm(n, 0, 2),
    PB_Ratio = c(8, 1.4, 0.8, 2.5)[groups] + stats::rnorm(n, 0, 0.3),
    stringsAsFactors = FALSE
  )
}
