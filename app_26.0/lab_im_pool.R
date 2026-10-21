# ==========================================
# lab_im_pool.R — extracted from lab_industry_method.R (C-STRUCT-03)
# Behavior-preserving structural split; sourced by lab_industry_method.R.
# ==========================================

#' App 預設預測年數（僅作缺財報／缺生命週期時的後備）
lab_model_horizon_years <- function() {
  n <- tryCatch(
    suppressWarnings(as.integer(APP_DEFAULTS$years %||% 5L)[1]),
    error = function(e) 5L
  )
  if (!is.finite(n) || n < 1L) 5L else as.integer(n)
}

#' 總潛在漲幅 % → 依 n 年換算年化 %
lab_annualized_upside_pct <- function(fv, price, n_years = NULL) {
  fv <- suppressWarnings(as.numeric(fv)[1])
  px <- suppressWarnings(as.numeric(price)[1])
  n <- suppressWarnings(as.numeric(n_years %||% lab_model_horizon_years())[1])
  if (!is.finite(fv) || !is.finite(px) || px <= 0 || fv <= 0) return(NA_real_)
  if (!is.finite(n) || n < 1) n <- 5
  ((fv / px)^(1 / n) - 1) * 100
}

#' 自 Yahoo Summary 取市值、現價、β 與公司全稱
lab_fetch_summary_metrics <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  out <- list(
    market_cap = NA_real_, price = NA_real_, beta = NA_real_,
    company_name = NA_character_
  )
  if (!nzchar(tk)) return(out)
  # Prefer fast-tier memoised summary (15m); fall back to uncached only if missing.
  sum_df <- tryCatch({
    if (exists("cached_get_summary_data", mode = "function")) {
      cached_get_summary_data(tk)
    } else {
      get_summary_data(tk)
    }
  }, error = function(e) NULL)
  if (is.null(sum_df) || !is.data.frame(sum_df) || nrow(sum_df) == 0) {
    return(out)
  }
  idx_m <- grep("^Market Cap \\(intraday\\)$|^Market Cap$", sum_df$Item, ignore.case = TRUE)
  if (length(idx_m) > 0) {
    out$market_cap <- parse_financial_number(sum_df$Value[idx_m[1]])[1]
  }
  idx_p <- grep("^Previous Close$|^Market Price$", sum_df$Item, ignore.case = TRUE)
  if (length(idx_p) > 0) {
    out$price <- parse_financial_number(sum_df$Value[idx_p[1]])[1]
  }
  idx_b <- grep("^Beta \\(5Y Monthly\\)$|^Beta$", sum_df$Item, ignore.case = TRUE)
  if (length(idx_b) > 0) {
    out$beta <- parse_financial_number(sum_df$Value[idx_b[1]])[1]
  }
  cname <- tryCatch(attr(sum_df, "company_name"), error = function(e) NULL)
  cname <- trimws(as.character(cname %||% "")[1])
  if (nzchar(cname)) out$company_name <- cname
  out
}

#' 依個股最新財報／β／生命週期推算實驗區 n、Ke、終值 g、近端 g1（勿一體適用 App 預設）
#' @return list(n_years, ke_pct, g_pct, g1_pct, beta, beta_source, rf_pct, rm_pct,
#'   lifecycle_stage, fund_sgr_pct, rev_cagr_pct)
lab_resolve_ticker_params <- function(d_is, d_bs, d_cf,
                                      industry_key = "",
                                      beta = NA_real_,
                                      rf_pct = NA_real_,
                                      rm_pct = NA_real_,
                                      ticker = "") {
  key <- as.character(industry_key %||% "")[1]
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  fallback_n <- lab_model_horizon_years()
  fallback_ke <- tryCatch(lab_industry_ke_pct(key), error = function(e) 9)
  if (!is.finite(fallback_ke) || fallback_ke <= 0) fallback_ke <- 9
  fallback_g <- suppressWarnings(as.numeric(APP_DEFAULTS$sgr %||% 4)[1])
  if (!is.finite(fallback_g)) fallback_g <- 4
  fallback_g1 <- suppressWarnings(as.numeric(APP_DEFAULTS$custom_g %||% fallback_g)[1])
  if (!is.finite(fallback_g1)) fallback_g1 <- fallback_g

  rf <- suppressWarnings(as.numeric(rf_pct)[1])
  if (!is.finite(rf)) {
    rf <- tryCatch({
      if (exists("cached_get_risk_free_rate", mode = "function")) {
        suppressWarnings(as.numeric(cached_get_risk_free_rate())[1])
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
  }
  if (!is.finite(rf)) {
    rf <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_rf %||% 4)[1])
  }
  if (!is.finite(rf)) rf <- 4

  b <- suppressWarnings(as.numeric(beta)[1])
  beta_src <- "ticker"
  if (!is.finite(b) || b <= 0) {
    ind <- tryCatch(industry_standards[[key]], error = function(e) NULL)
    b <- if (!is.null(ind) && !is.null(ind$beta_avg)) {
      suppressWarnings(as.numeric(ind$beta_avg)[1])
    } else {
      NA_real_
    }
    beta_src <- "industry"
  }
  if (!is.finite(b) || b <= 0) {
    b <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_beta %||% 1)[1])
    beta_src <- "default"
  }
  if (!is.finite(b) || b <= 0) {
    b <- 1
    beta_src <- "default"
  }

  rm <- suppressWarnings(as.numeric(rm_pct)[1])
  if (!is.finite(rm)) {
    ind <- tryCatch(industry_standards[[key]], error = function(e) NULL)
    rm <- if (!is.null(ind) && !is.null(ind$rm_avg)) {
      suppressWarnings(as.numeric(ind$rm_avg)[1])
    } else {
      NA_real_
    }
  }
  if (!is.finite(rm) || rm < rf + 2) {
    rm <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_rm %||% (rf + 5))[1])
  }
  if (!is.finite(rm) || rm < rf + 2) rm <- rf + 5

  ke_pct <- rf + b * (rm - rf)
  if (!is.finite(ke_pct) || ke_pct <= 0) ke_pct <- fallback_ke

  life <- tryCatch({
    if (exists("classify_lifecycle_result", mode = "function")) {
      classify_lifecycle_result(
        industry_key = key,
        ticker = tk,
        d_is = d_is,
        d_bs = d_bs,
        d_cf = d_cf,
        wacc_pct = ke_pct,
        selected_stage = "auto",
        locale = "zh-TW"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)
  stage <- if (!is.null(life)) {
    as.character(life$autoDetectedStage %||% NA_character_)[1]
  } else {
    NA_character_
  }
  n <- if (!is.null(life) && is.finite(suppressWarnings(as.numeric(life$recommendedForecastYears)[1]))) {
    as.integer(life$recommendedForecastYears)[1]
  } else if (exists("recommended_forecast_years_for_stage", mode = "function")) {
    suppressWarnings(as.integer(recommended_forecast_years_for_stage(stage))[1])
  } else {
    NA_integer_
  }
  if (!is.finite(n) || n < 1L) n <- fallback_n
  n <- as.integer(max(3L, min(15L, n)))

  fund_sgr <- tryCatch({
    if (exists("calc_fundamental_sgr_pct", mode = "function")) {
      calc_fundamental_sgr_pct(d_is, d_bs, d_cf)
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  long_cap <- tryCatch({
    if (exists("LIFECYCLE_CONFIG", inherits = TRUE)) {
      suppressWarnings(as.numeric(LIFECYCLE_CONFIG$terminal$long_run_nominal_g_cap_pct %||% 4.5)[1])
    } else {
      4.5
    }
  }, error = function(e) 4.5)
  if (!is.finite(long_cap) || long_cap <= 0) long_cap <- 4.5
  g_pct <- if (is.finite(fund_sgr)) fund_sgr else fallback_g
  g_ceil <- min(long_cap, ke_pct - 1.5)
  if (!is.finite(g_ceil) || g_ceil < 0.5) g_ceil <- 0.5
  g_pct <- max(0.5, min(g_pct, g_ceil))
  if (!is.finite(g_pct)) g_pct <- min(fallback_g, g_ceil)

  rev_cagr <- tryCatch({
    if (exists("get_avg_growth", mode = "function") &&
        exists("select_clean_metric_row", mode = "function") &&
        !is.null(d_is) && is.data.frame(d_is)) {
      get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  g1_pct <- if (is.finite(rev_cagr)) rev_cagr else fallback_g1
  g1_pct <- max(1, min(g1_pct, 15))
  if (!is.finite(g1_pct)) g1_pct <- fallback_g1

  list(
    n_years = as.integer(n),
    ke_pct = round(as.numeric(ke_pct)[1], 2),
    g_pct = round(as.numeric(g_pct)[1], 2),
    g1_pct = round(as.numeric(g1_pct)[1], 2),
    beta = round(as.numeric(b)[1], 3),
    beta_source = as.character(beta_src)[1],
    rf_pct = round(as.numeric(rf)[1], 2),
    rm_pct = round(as.numeric(rm)[1], 2),
    lifecycle_stage = if (is.na(stage) || !nzchar(stage)) NA_character_ else stage,
    fund_sgr_pct = if (is.finite(fund_sgr)) round(fund_sgr, 2) else NA_real_,
    rev_cagr_pct = if (is.finite(rev_cagr)) round(rev_cagr, 2) else NA_real_
  )
}

#' 宇宙檔數 N："all"／"全部" → 不設上限；空／自訂未填 → 預設 25
#' 已解析的數值（含 Inf）原樣保留，避免二次 clamp 把 Inf 誤判成預設 25。
lab_parse_im_max_n <- function(x, default_n = 25L) {
  default_n <- max(1L, as.integer(default_n)[1])
  if (is.null(x) || !length(x) || (length(x) == 1L && is.na(x))) {
    return(list(n = default_n, label = as.character(default_n), unlimited = FALSE))
  }
  # Already-resolved numeric from a prior lab_resolve / lab_clamp call
  if (is.numeric(x) && !is.factor(x)) {
    n0 <- suppressWarnings(as.numeric(x)[1])
    if (length(n0) == 1L && !is.na(n0) && !is.finite(n0)) {
      return(list(n = Inf, label = "全部", unlimited = TRUE))
    }
    if (is.finite(n0) && n0 >= 1) {
      n_i <- as.integer(round(n0))
      return(list(n = n_i, label = as.character(n_i), unlimited = FALSE))
    }
  }
  s <- trimws(as.character(x)[1])
  if (!nzchar(s) || identical(s, "custom") || identical(s, "自訂")) {
    return(list(n = default_n, label = as.character(default_n), unlimited = FALSE))
  }
  if (identical(s, "all") || identical(s, "全部") ||
      identical(s, "Inf") || identical(tolower(s), "inf")) {
    return(list(n = Inf, label = "全部", unlimited = TRUE))
  }
  n <- suppressWarnings(as.integer(s)[1])
  if (!is.finite(n) || n < 1L) {
    return(list(n = default_n, label = as.character(default_n), unlimited = FALSE))
  }
  list(n = as.integer(n), label = as.character(n), unlimited = FALSE)
}

lab_im_max_n_label <- function(x) {
  lab_parse_im_max_n(x)$label
}

#' 相容舊呼叫：回傳整數上限，或 Inf 表示全部
lab_clamp_im_max_n <- function(x, default = 25L, lo = 1L, hi = NULL) {
  def_n <- if (is.finite(default)) as.integer(default)[1] else 25L
  parsed <- lab_parse_im_max_n(x, default_n = def_n)
  n <- parsed$n
  if (!is.finite(n)) return(Inf)
  n <- max(as.integer(lo)[1], n)
  if (!is.null(hi) && is.finite(hi)) n <- min(as.integer(hi)[1], n)
  as.integer(n)
}

#' Shared Evaluation count / Universe size (N) choices for Detail + Clustering
lab_im_max_n_select_choices <- function(locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  if (identical(loc, "en")) {
    c(
      "25 names" = "25",
      "50 names" = "50",
      "100 names" = "100",
      "200 names" = "200",
      "500 names" = "500",
      "All" = "all",
      "Custom…" = "custom"
    )
  } else {
    c(
      "25 檔" = "25",
      "50 檔" = "50",
      "100 檔" = "100",
      "200 檔" = "200",
      "500 檔" = "500",
      "全部" = "all",
      "自訂…" = "custom"
    )
  }
}

#' Resolve select + optional custom numeric into clampable max_n
lab_resolve_im_max_n <- function(sel, custom = NULL, lo = 1L, hi = 500L) {
  s <- if (is.null(sel) || !length(sel)) "" else trimws(as.character(sel)[1])
  if (identical(s, "custom") || identical(s, "自訂")) {
    return(lab_clamp_im_max_n(custom, default = 25L, lo = lo, hi = hi))
  }
  lab_clamp_im_max_n(sel, default = 25L, lo = lo, hi = hi)
}

#' Coerce display/eval N for lab_im_run_eval_job — preserve Inf ("全部").
#'
#' `as.integer(Inf)` is NA in R; that used to collapse universe N=全部 to the
#' default 25 via lab_clamp_im_max_n(NA). Keep Inf, coerce finite values to int.
lab_coerce_im_job_n <- function(x) {
  if (is.null(x) || !length(x)) return(NA_real_)
  if (is.numeric(x) && !is.factor(x)) {
    n0 <- suppressWarnings(as.numeric(x)[1])
    if (length(n0) == 1L && !is.na(n0) && !is.finite(n0)) return(Inf)
    if (is.finite(n0) && n0 >= 1) return(as.integer(round(n0)))
  }
  lab_clamp_im_max_n(x)
}

#' Filter a ticker data.frame to selected concept-group members (display path).
#'
#' Ranking / detail views call this when pool_rank == concept so the concept
#' picker stays linked even if scores were produced under a different truncate rule.
lab_filter_df_by_concept_keys <- function(df, concept_keys, market_mode = "US") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)
  if (!"ticker" %in% names(df)) return(df)
  ckeys <- if (exists("lab_normalize_multi_filter", mode = "function")) {
    lab_normalize_multi_filter(concept_keys)
  } else {
    unique(as.character(unlist(concept_keys, use.names = FALSE)))
  }
  ckeys <- ckeys[!is.na(ckeys) & nzchar(ckeys)]
  if (!length(ckeys)) return(df)
  if (!exists("lab_concept_tickers", mode = "function")) return(df)
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market_mode)
    } else {
      m <- toupper(trimws(as.character(market_mode)[1]))
      if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
    }
  }, error = function(e) "US")
  ctks <- lab_concept_tickers(ckeys, market = mm)
  if (!length(ctks)) return(df[FALSE, , drop = FALSE])
  tk <- toupper(trimws(as.character(df$ticker)))
  ctks_u <- unique(toupper(trimws(ctks)))
  bare <- function(x) sub("\\.(TW|TWO)$", "", x, ignore.case = TRUE)
  keep <- tk %in% ctks_u |
    bare(tk) %in% bare(ctks_u) |
    gsub("\\.", "-", tk) %in% gsub("\\.", "-", ctks_u)
  df[keep, , drop = FALSE]
}

lab_resolve_im_max_n_label <- function(sel, custom = NULL) {
  s <- if (is.null(sel) || !length(sel)) "" else trimws(as.character(sel)[1])
  if (identical(s, "custom") || identical(s, "自訂")) {
    n <- lab_resolve_im_max_n(sel, custom)
    return(if (is.finite(n)) as.character(n) else "全部")
  }
  lab_im_max_n_label(sel)
}

#' Resolve Yahoo evaluation pool size from display N
#'
#' Universe size (N) is the **post-analysis display cap** (qualified rows), not
#' the fetch/eval count. Evaluation uses a larger ordered pool so enough names
#' may pass quality gates; detail/leaderboard then show at most N without padding.
#'
#' @param display_n finite integer or Inf ("全部")
#' @return Inf when display_n is unlimited; otherwise max(display_n, min(hi, max(display_n * 4, 100)))
lab_resolve_im_eval_n <- function(display_n, lo = 1L, hi = 500L) {
  if (is.null(display_n) || length(display_n) < 1L) return(100L)
  d <- suppressWarnings(as.numeric(display_n)[1])
  if (!is.finite(d)) return(Inf)
  d <- as.integer(max(as.integer(lo)[1], d))
  hi_i <- as.integer(hi)[1]
  if (!is.finite(hi_i) || hi_i < 1L) hi_i <- 500L
  eval_n <- max(d * 4L, 100L)
  eval_n <- max(d, min(hi_i, eval_n))
  as.integer(eval_n)
}

#' Cap detail rows from the qualified pool — never pad to fill display_n
#'
#' Qualified = lab_leaderboard_pool (finite annualized upside; optional F≥7 / EQ / no FS alerts).
#' Returns at most display_n rows; if fewer qualify, returns that shorter set.
lab_cap_detail_display <- function(merged_df, display_n,
                                   eq_only = FALSE, gate_only = TRUE,
                                   no_alert = FALSE) {
  pool <- lab_leaderboard_pool(
    merged_df,
    eq_only = eq_only,
    gate_only = gate_only,
    no_alert = no_alert
  )
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0L) {
    return(pool)
  }
  d <- suppressWarnings(as.numeric(display_n)[1])
  if (!is.finite(d) || d < 1) return(pool)
  utils::head(pool, as.integer(d))
}

lab_dedupe_eval_pool <- function(pool) {
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0L) {
    return(pool)
  }
  keep <- !is.na(pool$ticker) & nzchar(as.character(pool$ticker))
  pool <- pool[keep, , drop = FALSE]
  if (nrow(pool) == 0L) return(pool)
  pool$ticker <- toupper(trimws(as.character(pool$ticker)))
  pool[!duplicated(pool$ticker), , drop = FALSE]
}

.lab_mcap_cache <- new.env(parent = emptyenv())
.lab_ret1y_cache <- new.env(parent = emptyenv())
.lab_universe_metrics_cache <- new.env(parent = emptyenv())

#' Path to bundled offline universe metrics (market_cap / ret_1y / paid_in_capital)
lab_universe_metrics_snapshot_path <- function() {
  opt <- getOption("ynow.universe_metrics_snapshot", default = NULL)
  if (!is.null(opt) && length(opt) >= 1L) {
    p <- as.character(opt)[1]
    if (nzchar(p) && file.exists(p)) {
      return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  candidates <- c(
    file.path("data", "universe_metrics_snapshot.csv"),
    file.path("app_26.0", "data", "universe_metrics_snapshot.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

#' Load offline universe metrics snapshot once per session
lab_load_universe_metrics_snapshot <- function(force = FALSE) {
  if (!isTRUE(force) &&
      exists("df", envir = .lab_universe_metrics_cache, inherits = FALSE)) {
    return(get("df", envir = .lab_universe_metrics_cache, inherits = FALSE))
  }
  path <- lab_universe_metrics_snapshot_path()
  empty <- data.frame(
    ticker = character(0),
    market_cap = numeric(0),
    ret_1y = numeric(0),
    paid_in_capital = numeric(0),
    stringsAsFactors = FALSE
  )
  if (!nzchar(path) || is.na(path) || !file.exists(path)) {
    assign("df", empty, envir = .lab_universe_metrics_cache)
    attr(empty, "snapshot_source") <- "missing"
    return(empty)
  }
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0L || !"ticker" %in% names(raw)) {
    assign("df", empty, envir = .lab_universe_metrics_cache)
    attr(empty, "snapshot_source") <- "empty"
    return(empty)
  }
  df <- data.frame(
    ticker = toupper(trimws(as.character(raw$ticker))),
    market_cap = if ("market_cap" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$market_cap))
    } else {
      NA_real_
    },
    ret_1y = if ("ret_1y" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$ret_1y))
    } else {
      NA_real_
    },
    paid_in_capital = if ("paid_in_capital" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$paid_in_capital))
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
  df <- df[nzchar(df$ticker) & !is.na(df$ticker), , drop = FALSE]
  df <- df[!duplicated(df$ticker), , drop = FALSE]
  rownames(df) <- NULL
  attr(df, "snapshot_source") <- path
  if ("snapshot_at" %in% names(raw)) {
    attr(df, "snapshot_at") <- as.character(raw$snapshot_at[[1]])
  }
  assign("df", df, envir = .lab_universe_metrics_cache)
  df
}

.lab_lookup_metric_vec <- function(tickers, col) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  out <- stats::setNames(rep(NA_real_, length(tks)), tks)
  if (!length(tks)) return(out)
  snap <- tryCatch(lab_load_universe_metrics_snapshot(), error = function(e) NULL)
  if (is.null(snap) || !is.data.frame(snap) || nrow(snap) == 0L || !col %in% names(snap)) {
    return(out)
  }
  idx <- match(tks, snap$ticker)
  hit <- which(is.finite(idx))
  if (!length(hit)) return(out)
  vals <- suppressWarnings(as.numeric(snap[[col]][idx[hit]]))
  out[tks[hit]] <- vals
  # BRK.B / BRK-B style alts
  miss <- tks[is.na(out) | !is.finite(out)]
  if (length(miss)) {
    alts <- gsub("\\.", "-", miss)
    idx2 <- match(alts, gsub("\\.", "-", snap$ticker))
    hit2 <- which(is.finite(idx2))
    if (length(hit2)) {
      out[miss[hit2]] <- suppressWarnings(as.numeric(snap[[col]][idx2[hit2]]))
    }
  }
  out
}

#' 批次市值：離線快照優先，缺口再打 Yahoo；仍缺則 NA（呼叫端維持原宇宙順序）
lab_fetch_market_caps_usd <- function(tickers) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  out <- stats::setNames(rep(NA_real_, length(tks)), tks)
  if (!length(tks)) return(out)

  # 1) session cache
  need <- character(0)
  for (tk in tks) {
    if (exists(tk, envir = .lab_mcap_cache, inherits = FALSE)) {
      out[[tk]] <- suppressWarnings(as.numeric(get(tk, envir = .lab_mcap_cache, inherits = FALSE))[1])
    } else {
      need <- c(need, tk)
    }
  }

  # 2) bundled offline snapshot (full US/TW universe when shipped)
  if (length(need)) {
    snap_caps <- .lab_lookup_metric_vec(need, "market_cap")
    for (tk in need) {
      v <- suppressWarnings(as.numeric(snap_caps[[tk]])[1])
      if (is.finite(v) && v > 0) {
        out[[tk]] <- v
        assign(tk, v, envir = .lab_mcap_cache)
      }
    }
    need <- need[!(is.finite(out[need]) & out[need] > 0)]
  }

  if (!length(need)) return(out)

  # 3) live Yahoo batch for remaining gaps
  fetched <- NULL
  if (exists(".ensure_python_scraper", mode = "function") &&
      isTRUE(.ensure_python_scraper()) &&
      exists("get_market_caps_batch", mode = "function")) {
    fetched <- tryCatch(get_market_caps_batch(need), error = function(e) NULL)
  }
  if (!is.null(fetched) && (is.list(fetched) || is.numeric(fetched))) {
    nms <- toupper(trimws(as.character(names(fetched))))
    vals <- suppressWarnings(as.numeric(unlist(fetched, use.names = FALSE)))
    if (length(nms) == length(vals) && length(nms) > 0) {
      for (i in seq_along(nms)) {
        key <- gsub("/", "-", nms[[i]])
        v <- vals[[i]]
        if (!nzchar(key)) next
        if (is.finite(v) && v > 0) {
          out[[key]] <- v
          assign(key, v, envir = .lab_mcap_cache)
          alt <- gsub("\\.", "-", key)
          if (!identical(alt, key) && alt %in% names(out) && !is.finite(out[[alt]])) {
            out[[alt]] <- v
            assign(alt, v, envir = .lab_mcap_cache)
          }
        }
      }
    }
  }
  out
}

#' 掛上市值，供候選 > N 時依市值降序截斷評估池（非規模篩選）
lab_attach_market_caps <- function(pool) {
  pool <- lab_dedupe_eval_pool(pool)
  if (is.null(pool) || nrow(pool) == 0L) return(pool)
  caps <- lab_fetch_market_caps_usd(pool$ticker)
  pool$market_cap <- unname(caps[pool$ticker])
  # TW 實收資本額（資本額）；不作為市值排序代理
  paid <- .lab_lookup_metric_vec(pool$ticker, "paid_in_capital")
  pool$paid_in_capital <- unname(paid[pool$ticker])
  pool
}

#' Pool truncate / rank modes when candidates > N
lab_im_pool_rank_choices <- function(locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  is_zh <- grepl("^zh", tolower(loc), perl = TRUE)
  if (is_zh) {
    c(
      "依市值（由大到小）" = "mcap",
      "所選概念股全部" = "concept",
      "依近一年股價漲幅" = "ret_1y",
      "系統隨機" = "random"
    )
  } else {
    c(
      "By market cap (largest first)" = "mcap",
      "All selected concept groups" = "concept",
      "By 1Y price return" = "ret_1y",
      "System random" = "random"
    )
  }
}

lab_normalize_pool_rank_mode <- function(x, default = "mcap") {
  s <- tolower(trimws(as.character(x %||% default)[1]))
  if (!nzchar(s)) s <- default
  if (s %in% c("mcap", "market_cap", "cap", "市值")) return("mcap")
  if (s %in% c("concept", "concepts", "theme", "概念", "概念股")) return("concept")
  if (s %in% c("ret_1y", "return_1y", "1y", "漲幅", "近一年")) return("ret_1y")
  if (s %in% c("random", "rand", "隨機")) return("random")
  default
}

#' Batch 1Y total return (fraction): offline snapshot first, then Yahoo
lab_fetch_returns_1y <- function(tickers) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  out <- stats::setNames(rep(NA_real_, length(tks)), tks)
  if (!length(tks)) return(out)

  need <- character(0)
  for (tk in tks) {
    if (exists(tk, envir = .lab_ret1y_cache, inherits = FALSE)) {
      out[[tk]] <- suppressWarnings(as.numeric(get(tk, envir = .lab_ret1y_cache, inherits = FALSE))[1])
    } else {
      need <- c(need, tk)
    }
  }

  if (length(need)) {
    snap_rets <- .lab_lookup_metric_vec(need, "ret_1y")
    for (tk in need) {
      v <- suppressWarnings(as.numeric(snap_rets[[tk]])[1])
      if (is.finite(v)) {
        out[[tk]] <- v
        assign(tk, v, envir = .lab_ret1y_cache)
      }
    }
    need <- need[!(is.finite(out[need]))]
  }

  if (!length(need)) return(out)

  fetched <- NULL
  if (exists(".ensure_python_scraper", mode = "function") &&
      isTRUE(.ensure_python_scraper()) &&
      exists("get_returns_1y_batch", mode = "function")) {
    fetched <- tryCatch(get_returns_1y_batch(need), error = function(e) NULL)
  }
  if (!is.null(fetched) && (is.list(fetched) || is.numeric(fetched))) {
    nms <- toupper(trimws(as.character(names(fetched))))
    vals <- suppressWarnings(as.numeric(unlist(fetched, use.names = FALSE)))
    if (length(nms) == length(vals) && length(nms) > 0) {
      for (i in seq_along(nms)) {
        key <- gsub("/", "-", nms[[i]])
        v <- vals[[i]]
        if (!nzchar(key) || !is.finite(v)) next
        out[[key]] <- v
        assign(key, v, envir = .lab_ret1y_cache)
        alt <- gsub("\\.", "-", key)
        if (!identical(alt, key) && alt %in% names(out) && !is.finite(out[[alt]])) {
          out[[alt]] <- v
          assign(alt, v, envir = .lab_ret1y_cache)
        }
      }
    }
  }
  out
}

lab_attach_returns_1y <- function(pool) {
  pool <- lab_dedupe_eval_pool(pool)
  if (is.null(pool) || nrow(pool) == 0L) return(pool)
  rets <- lab_fetch_returns_1y(pool$ticker)
  pool$ret_1y <- unname(rets[pool$ticker])
  pool
}

#' 依市值降序取最多 max_n 檔（無市值置後，再依代碼）
#' 顯示檔數 N：截斷評估池用 eval_n；明細／排行自合格池取至多 N，不足不湊滿。
#' 不做市值分級／規模篩選。
lab_rank_and_cap_eval_pool <- function(pool, max_n = 25L) {
  lab_select_eval_pool(pool, max_n = max_n, mode = "mcap")
}

#' Select / truncate evaluation pool by user mode
#'
#' Semantic order (always):
#' 1. Apply Candidate truncate rule on the full filtered universe pool
#'    (sort by market cap / filter to selected concept groups / sort by 1Y return / random).
#' 2. Then take the first evaluation-count (eval_n) rows — typically larger than
#'    display N so enough names may pass quality gates after Yahoo scoring.
#'
#' Modes:
#' - mcap: sort entire pool by market cap (largest first), then take eval_n
#' - concept: keep selected concept-group tickers first; if still > eval_n, mcap within then take eval_n
#' - ret_1y: sort by 1Y price return (highest first), then take eval_n
#' - random: uniform random sample of eval_n
lab_select_eval_pool <- function(pool, max_n = 25L, mode = "mcap",
                                 concept_keys = NULL,
                                 market_mode = "US",
                                 seed = NULL) {
  pool <- lab_dedupe_eval_pool(pool)
  mode <- lab_normalize_pool_rank_mode(mode)
  if (is.null(pool) || !is.data.frame(pool)) {
    empty <- data.frame(ticker = character(0), stringsAsFactors = FALSE)
    attr(empty, "n_filtered") <- 0L
    attr(empty, "max_n") <- lab_clamp_im_max_n(max_n)
    attr(empty, "used_market_cap") <- FALSE
    attr(empty, "pool_rank_mode") <- mode
    attr(empty, "pool_rank_note") <- "empty"
    return(empty)
  }
  max_n <- lab_clamp_im_max_n(max_n)
  n_filtered <- nrow(pool)
  note <- mode
  used_mcap <- FALSE
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market_mode)
    } else {
      m <- toupper(trimws(as.character(market_mode)[1]))
      if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
    }
  }, error = function(e) "US")

  .pool_has_usable_mcap <- function(df) {
    if (is.null(df) || !is.data.frame(df) || !"market_cap" %in% names(df)) return(FALSE)
    caps <- suppressWarnings(as.numeric(df$market_cap))
    isTRUE(sum(is.finite(caps) & caps > 0, na.rm = TRUE) > 0L)
  }

  # --- Step 1a: concept filter on the full pool (before any N cut) ---
  if (identical(mode, "concept")) {
    if (!exists("lab_concept_tickers", mode = "function")) {
      mode <- "mcap"
      note <- "concept_unavailable_fallback_mcap"
    } else {
      ckeys <- lab_normalize_multi_filter(concept_keys)
      ctks <- lab_concept_tickers(ckeys, market = mm)
      if (!length(ckeys) || !length(ctks)) {
        mode <- "mcap"
        note <- "concept_none_selected_fallback_mcap"
      } else {
        pool$ticker <- toupper(trimws(as.character(pool$ticker)))
        ctks_u <- unique(toupper(trimws(ctks)))
        bare <- function(x) sub("\\.(TW|TWO)$", "", x, ignore.case = TRUE)
        keep <- pool$ticker %in% ctks_u |
          bare(pool$ticker) %in% bare(ctks_u) |
          gsub("\\.", "-", pool$ticker) %in% gsub("\\.", "-", ctks_u)
        pool_c <- pool[keep, , drop = FALSE]
        if (nrow(pool_c) == 0L) {
          mode <- "mcap"
          note <- "concept_no_overlap_fallback_mcap"
        } else {
          pool <- pool_c
          note <- sprintf("concept_n=%d", nrow(pool))
        }
      }
    }
  }

  # Soft Yahoo-cost guard for 1Y returns only. Never pre-cut a pool that already
  # has market_cap (would break "sort all by mcap, then take N").
  if (identical(mode, "ret_1y") &&
      exists("lab_us_prescreen_eval_pool", mode = "function")) {
    pre_n <- if (exists("LAB_US_EVAL_PRESCREEN", inherits = TRUE)) {
      as.integer(LAB_US_EVAL_PRESCREEN)[1]
    } else {
      800L
    }
    if (is.finite(pre_n) && nrow(pool) > pre_n) {
      pool <- lab_us_prescreen_eval_pool(pool, max_n = pre_n)
      note <- paste0(as.character(note)[1], ";prescreen=", nrow(pool))
    }
  }

  if (identical(mode, "random")) {
    if (is.finite(max_n) && nrow(pool) > max_n) {
      if (!is.null(seed) && is.finite(as.numeric(seed)[1])) {
        set.seed(as.integer(seed)[1])
      }
      pool <- pool[sample.int(nrow(pool), size = as.integer(max_n)), , drop = FALSE]
      pool <- pool[order(pool$ticker), , drop = FALSE]
    }
    attr(pool, "n_filtered") <- as.integer(n_filtered)
    attr(pool, "max_n") <- max_n
    attr(pool, "used_market_cap") <- FALSE
    attr(pool, "pool_rank_mode") <- "random"
    attr(pool, "pool_rank_note") <- note
    return(pool)
  }

  if (identical(mode, "ret_1y")) {
    # Step 1b: rank entire (post-filter) pool by 1Y return
    pool <- lab_attach_returns_1y(pool)
    ret <- suppressWarnings(as.numeric(pool$ret_1y))
    missing <- is.na(ret) | !is.finite(ret)
    o <- order(missing, -ifelse(missing, 0, ret), pool$ticker, na.last = TRUE)
    pool <- pool[o, , drop = FALSE]
    # Step 2: take first N
    if (is.finite(max_n) && nrow(pool) > max_n) pool <- utils::head(pool, max_n)
    attr(pool, "n_filtered") <- as.integer(n_filtered)
    attr(pool, "max_n") <- max_n
    attr(pool, "used_market_cap") <- FALSE
    attr(pool, "pool_rank_mode") <- "ret_1y"
    attr(pool, "pool_rank_note") <- note
    return(pool)
  }

  # Default / concept-fallback / mcap-within-concept: market cap
  # Step 1b: attach caps to the FULL remaining pool, then sort all (no S&P pre-cut)
  if (!.pool_has_usable_mcap(pool)) {
    if (is.finite(max_n) && nrow(pool) > max_n) {
      pool <- lab_attach_market_caps(pool)
    } else if (!is.finite(max_n) || nrow(pool) <= max_n) {
      # Still attach when useful for display / stable ordering
      if (nrow(pool) > 0L && nrow(pool) <= 800L) {
        pool <- lab_attach_market_caps(pool)
      }
    }
  }
  if (!"market_cap" %in% names(pool)) pool$market_cap <- NA_real_
  mcap <- suppressWarnings(as.numeric(pool$market_cap))
  missing <- is.na(mcap) | !is.finite(mcap) | mcap <= 0
  used_mcap <- isTRUE(sum(!missing) > 0L)
  o <- order(missing, -ifelse(missing, 0, mcap), pool$ticker, na.last = TRUE)
  pool <- pool[o, , drop = FALSE]
  # Step 2: take first N from the sorted pool
  if (is.finite(max_n) && nrow(pool) > max_n) pool <- utils::head(pool, max_n)
  attr(pool, "n_filtered") <- as.integer(n_filtered)
  attr(pool, "max_n") <- max_n
  attr(pool, "used_market_cap") <- used_mcap
  attr(pool, "pool_rank_mode") <- "mcap"
  attr(pool, "pool_rank_note") <- note
  pool
}

#' 正規化複選篩選：NULL／空／含 "all" → character(0) 表示不過濾
lab_normalize_multi_filter <- function(x) {
  v <- unique(as.character(unlist(x, use.names = FALSE)))
  v <- v[nzchar(v) & !is.na(v) & v != "all"]
  v
}
