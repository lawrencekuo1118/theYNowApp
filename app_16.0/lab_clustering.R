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

#' R-only Yahoo quoteSummary fallback (no Python / reticulate)
lab_fetch_cluster_features_r <- function(tickers, timeout_sec = 12) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  empty <- lab_cluster_features_to_df(NULL)
  if (!length(tks)) return(empty)
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("httr/jsonlite required for R Yahoo feature fallback.")
  }

  rows <- lapply(tks, function(sym) {
    url <- sprintf(
      "https://query2.finance.yahoo.com/v10/finance/quoteSummary/%s",
      utils::URLencode(sym, reserved = TRUE)
    )
    res <- tryCatch(
      httr::GET(
        url,
        query = list(modules = "defaultKeyStatistics,financialData,summaryDetail,price"),
        httr::add_headers(
          `User-Agent` = "Mozilla/5.0 (compatible; TheYNowApp/16.0; +https://github.com/lawrencekuo1118/theYNowApp)",
          Accept = "application/json"
        ),
        httr::timeout(as.numeric(timeout_sec)[1])
      ),
      error = function(e) NULL
    )
    row <- list(
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
    if (is.null(res) || httr::status_code(res) >= 400) return(row)
    body <- tryCatch(
      httr::content(res, as = "parsed", type = "application/json"),
      error = function(e) NULL
    )
    result <- tryCatch(body$quoteSummary$result[[1]], error = function(e) NULL)
    if (is.null(result)) return(row)
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
    row$OpInc_YoY <- .lab_cluster_pct_points(fd$earningsGrowth %||% ks$earningsQuarterlyGrowth)
    row$Debt_Ratio <- .lab_cluster_debt_pct(fd$debtToEquity)
    pe <- .lab_cluster_yahoo_raw(sd$trailingPE %||% ks$forwardPE %||% fd$currentPrice)
    # Prefer trailingPE from summaryDetail / defaultKeyStatistics
    pe <- .lab_cluster_yahoo_raw(sd$trailingPE)
    if (!is.finite(pe)) pe <- .lab_cluster_yahoo_raw(ks$forwardPE)
    pb <- .lab_cluster_yahoo_raw(sd$priceToBook %||% ks$priceToBook)
    if (is.finite(pe) && pe > 0) row$PE_Ratio <- pe
    if (is.finite(pb) && pb > 0) row$PB_Ratio <- pb
    row
  })

  lab_cluster_features_to_df(rows)
}

#' Fetch Yahoo ratio features (Python first, R quoteSummary fallback)
#' R-only Yahoo quoteSummary fallback (works when reticulate Python is unavailable)
#' Fetch Yahoo ratio features (Python first; R quoteSummary fallback for shinyapps)
lab_fetch_cluster_features <- function(tickers) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  if (!length(tks)) return(lab_cluster_features_to_df(NULL))

  `%||%` <- function(a, b) if (is.null(a)) b else a
  py_err <- NULL
  df <- NULL
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
      df <- tryCatch({
        raw <- fn(as.list(tks))
        lab_cluster_features_to_df(raw)
      }, error = function(e) {
        py_err <<- conditionMessage(e)
        NULL
      })
    } else {
      py_err <- "get_cluster_features_batch not found"
    }
  } else {
    py_err <- "Python scraper unavailable"
  }

  n_ok <- if (is.null(df) || nrow(df) == 0L) {
    0L
  } else {
    sum(is.finite(df$ROE) | is.finite(df$PE_Ratio) | is.finite(df$Operating_Margin), na.rm = TRUE)
  }
  if (is.null(df) || nrow(df) == 0L || n_ok < 2L) {
    df_r <- tryCatch(
      lab_fetch_cluster_features_r(tks),
      error = function(e) {
        stop(sprintf(
          "Feature fetch failed (python: %s; R fallback: %s)",
          py_err %||% "n/a", conditionMessage(e)
        ))
      }
    )
    if (!is.null(df_r) && nrow(df_r) > 0L) return(df_r)
    stop(sprintf(
      "Feature fetch returned 0 usable rows for %d tickers (python: %s).",
      length(tks), py_err %||% "n/a"
    ))
  }
  df
}

#' Build evaluation pool for clustering from Blue Chip catalog filters
lab_cluster_build_pool <- function(catalog, industry_filter = NULL, method_filter = NULL,
                                   max_n = 50L) {
  empty <- data.frame(
    ticker = character(0),
    industry_key = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(catalog) || !is.data.frame(catalog) || nrow(catalog) == 0L) {
    return(empty)
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
  if (is.null(pool) || nrow(pool) == 0L) return(empty)
  max_n <- lab_resolve_im_max_n(max_n, custom = NULL, lo = 1L, hi = 500L)
  if (is.finite(max_n) && nrow(pool) > max_n) {
    pool <- lab_attach_market_caps(pool)
  }
  pool <- lab_rank_and_cap_eval_pool(pool, max_n = max_n)
  cols <- intersect(c("ticker", "industry_key", "industry_label", "market_cap"), names(pool))
  pool[, cols, drop = FALSE]
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
lab_run_stock_clustering <- function(df_financials, k_clusters = 4L,
                                     features = LAB_CLUSTER_FEATURES,
                                     locale = "en", seed = 42L) {
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

  n_finite_orig <- rowSums(vapply(
    feats,
    function(f) is.finite(suppressWarnings(as.numeric(df[[f]]))),
    logical(nrow(df))
  ))
  keep <- n_finite_orig >= 2L
  if (sum(keep) < k_clusters) {
    stop(sprintf(
      "After missing-data filter, only %d stocks remain (need ≥ %d).",
      sum(keep), k_clusters
    ))
  }
  df <- df[keep, , drop = FALSE]
  X <- X[keep, , drop = FALSE]
  rownames(df) <- NULL
  rownames(X) <- NULL

  Xs <- scale(as.matrix(X))
  Xs[!is.finite(Xs)] <- 0

  set.seed(as.integer(seed)[1])
  km <- stats::kmeans(Xs, centers = k_clusters, nstart = 25L, iter.max = 100L)
  df$Cluster_ID <- as.integer(km$cluster)

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
  focus <- toupper(trimws(as.character(focus_ticker)[1]))
  if (!nzchar(focus) || !focus %in% df$ticker) return(character(0))
  cid <- df$Cluster_ID[match(focus, df$ticker)]
  peers <- df[df$Cluster_ID == cid & df$ticker != focus, , drop = FALSE]
  if (nrow(peers) == 0L) return(character(0))
  feats <- result$features
  focus_row <- as.numeric(df[df$ticker == focus, feats, drop = FALSE][1, ])
  peer_mat <- as.matrix(peers[, feats, drop = FALSE])
  d <- sqrt(rowSums(
    (peer_mat - matrix(focus_row, nrow = nrow(peer_mat), ncol = length(focus_row), byrow = TRUE))^2
  ))
  o <- order(d, peers$ticker)
  utils::head(peers$ticker[o], as.integer(n_peers)[1])
}

#' Plotly 2D cluster scatter
lab_cluster_scatter_plotly <- function(result, x_feat = "ROE", y_feat = "PE_Ratio",
                                       locale = "en") {
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
  plotly::plot_ly(
    df,
    x = ~._x,
    y = ~._y,
    color = ~Cluster_Label,
    text = ~hover,
    type = "scatter",
    mode = "markers",
    hoverinfo = "text",
    marker = list(size = 11, opacity = 0.85)
  ) %>%
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
  focus <- toupper(trimws(as.character(focus_ticker)[1]))
  empty_msg <- function(msg) {
    plotly::plotly_empty() %>%
      plotly::layout(annotations = list(list(text = msg, showarrow = FALSE)))
  }
  if (!nzchar(focus) || !focus %in% df$ticker) return(empty_msg("Select a focus ticker"))
  if (is.null(peer_tickers) || !length(peer_tickers)) {
    peer_tickers <- lab_cluster_radar_peers(result, focus, n_peers = 3L)
  }
  tickers <- unique(c(focus, toupper(as.character(peer_tickers))))
  sub <- df[df$ticker %in% tickers, , drop = FALSE]
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
  p <- plotly::plot_ly(type = "scatterpolar", mode = "lines", fill = "toself")
  for (i in seq_len(nrow(sub))) {
    vals <- as.numeric(scaled[i, ])
    p <- plotly::add_trace(
      p,
      r = c(vals, vals[1]),
      theta = c(axis_labs, axis_labs[1]),
      name = paste0(sub$ticker[i], if (identical(sub$ticker[i], focus)) " ★" else ""),
      mode = "lines"
    )
  }
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

#' Round numeric columns in Cluster assignments table to 2 decimal places
#' (Cluster_ID and non-numeric identity columns left unchanged).
lab_cluster_format_assignments_df <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !ncol(df)) return(df)
  out <- df
  skip <- c("ticker", "name", "Cluster_ID", "Cluster_Label", "industry_key", "industry_label")
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
