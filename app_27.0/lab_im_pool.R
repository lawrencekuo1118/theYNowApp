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

# Ranking screen: one v7 quote batch fills these before the per-ticker loop.
.lab_ranking_summary_cache <- new.env(parent = emptyenv())

.lab_ranking_summary_empty <- function() {
  list(
    market_cap = NA_real_, price = NA_real_, beta = NA_real_,
    company_name = NA_character_,
    currency = NA_character_, financial_currency = NA_character_
  )
}

.lab_ranking_summary_clear <- function() {
  rm(list = ls(envir = .lab_ranking_summary_cache, all.names = TRUE),
     envir = .lab_ranking_summary_cache)
  invisible(TRUE)
}

.lab_ranking_summary_put <- function(ticker, metrics) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  if (!nzchar(tk) || !is.list(metrics)) return(invisible(FALSE))
  assign(tk, metrics, envir = .lab_ranking_summary_cache)
  invisible(TRUE)
}

.lab_ranking_summary_get <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  if (!nzchar(tk)) return(NULL)
  if (!exists(tk, envir = .lab_ranking_summary_cache, inherits = FALSE)) return(NULL)
  get(tk, envir = .lab_ranking_summary_cache, inherits = FALSE)
}

.lab_ranking_summary_from_row <- function(row) {
  out <- .lab_ranking_summary_empty()
  if (is.null(row)) return(out)
  if (is.list(row)) {
    out$market_cap <- suppressWarnings(as.numeric(row$market_cap %||% row[["market_cap"]])[1])
    out$price <- suppressWarnings(as.numeric(row$price %||% row[["price"]])[1])
    out$beta <- suppressWarnings(as.numeric(row$beta %||% row[["beta"]])[1])
    cn <- trimws(as.character(row$company_name %||% row[["company_name"]] %||% "")[1])
    if (nzchar(cn) && !identical(cn, "None")) out$company_name <- cn
    ccy <- toupper(trimws(as.character(row$currency %||% row[["currency"]] %||% "")[1]))
    if (nzchar(ccy) && !identical(ccy, "NONE")) out$currency <- ccy
    fcy <- toupper(trimws(as.character(
      row$financial_currency %||% row[["financial_currency"]] %||% ""
    )[1]))
    if (nzchar(fcy) && !identical(fcy, "NONE")) out$financial_currency <- fcy
  }
  out
}

# Batch is only "complete" when Ranking has a usable price. Name/beta alone
# must not skip the per-ticker Summary fallback (Yahoo often returns a name
# with no marketCap / thin quote fields).
.lab_ranking_summary_complete <- function(sm) {
  is.list(sm) && is.finite(suppressWarnings(as.numeric(sm$price)[1])) &&
    suppressWarnings(as.numeric(sm$price)[1]) > 0
}

#' Prefetch Ranking summary fields via one Yahoo v7 quote batch (no stock.info).
lab_prefetch_ranking_summaries <- function(tickers) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  if (!length(tks)) return(invisible(0L))
  t0 <- unname(proc.time()[["elapsed"]])
  fetched <- NULL
  fn <- NULL
  if (exists(".py_scraper_env") &&
      exists("get_ranking_summary_batch", envir = .py_scraper_env,
             inherits = FALSE, mode = "function")) {
    fn <- get("get_ranking_summary_batch", envir = .py_scraper_env, inherits = FALSE)
  } else if (exists("get_ranking_summary_batch", mode = "function")) {
    fn <- get_ranking_summary_batch
  }
  if (is.function(fn) &&
      exists(".ensure_python_scraper", mode = "function") &&
      isTRUE(.ensure_python_scraper())) {
    fetched <- tryCatch(fn(as.list(tks)), error = function(e) NULL)
    if (!is.null(fetched) && !is.list(fetched) &&
        requireNamespace("reticulate", quietly = TRUE)) {
      fetched <- tryCatch(reticulate::py_to_r(fetched), error = function(e) fetched)
    }
  }
  n_hit <- 0L
  if (!is.null(fetched) && is.list(fetched) && length(names(fetched))) {
    nms <- toupper(trimws(as.character(names(fetched))))
    for (i in seq_along(nms)) {
      key <- gsub("/", "-", nms[[i]])
      if (!nzchar(key)) next
      el <- fetched[[i]]
      if (is.null(el) || !(is.list(el) || is.numeric(el))) next
      sm <- .lab_ranking_summary_from_row(el)
      # Store partial rows (name/ccy hints) but only count complete price hits.
      has_any <- is.finite(sm$market_cap) || is.finite(sm$price) ||
        is.finite(sm$beta) || (is.character(sm$company_name) && nzchar(sm$company_name)) ||
        (is.character(sm$currency) && nzchar(sm$currency))
      if (!has_any) next
      .lab_ranking_summary_put(key, sm)
      if (.lab_ranking_summary_complete(sm)) n_hit <- n_hit + 1L
      alt <- gsub("\\.", "-", key)
      if (!identical(alt, key)) .lab_ranking_summary_put(alt, sm)
    }
  }
  if (exists("ynow_pload", mode = "function")) {
    ynow_pload("fscore_summary_batch", "end", list(
      n_req = length(tks),
      n_hit = n_hit,
      elapsed_s = unname(proc.time()[["elapsed"]]) - t0
    ))
  }
  invisible(n_hit)
}

#' 自 Yahoo Summary 取市值、現價、β 與公司全稱
lab_fetch_summary_metrics <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  out <- .lab_ranking_summary_empty()
  if (!nzchar(tk)) return(out)
  batch <- NULL
  # Only during Ranking screen: prefer a complete v7 batch hit (requires price).
  # Outside the screen the batch cache must not outlive Ranking / shadow the
  # 15-minute Summary memo (clustering beta fill, Search helpers, etc.).
  if (isTRUE(getOption("ynow.ranking_screen", FALSE))) {
    batch <- .lab_ranking_summary_get(tk)
    if (is.null(batch)) batch <- .lab_ranking_summary_get(gsub("\\.", "-", tk))
    if (is.list(batch) && .lab_ranking_summary_complete(batch)) {
      out$market_cap <- batch$market_cap
      out$price <- batch$price
      out$beta <- batch$beta
      out$company_name <- batch$company_name
      out$currency <- batch$currency
      out$financial_currency <- batch$financial_currency
      return(out)
    }
  }
  # Prefer fast-tier memoised summary (15m); fall back to uncached only if missing.
  sum_df <- tryCatch({
    if (exists("cached_get_summary_data", mode = "function")) {
      cached_get_summary_data(tk)
    } else {
      get_summary_data(tk)
    }
  }, error = function(e) NULL)
  if (is.null(sum_df) || !is.data.frame(sum_df) || nrow(sum_df) == 0) {
    # Partial batch (name/ccy only) may still help when Summary is empty.
    if (is.list(batch)) {
      if (is.character(batch$company_name) && nzchar(batch$company_name)) {
        out$company_name <- batch$company_name
      }
      if (is.character(batch$currency) && nzchar(batch$currency)) {
        out$currency <- batch$currency
      }
      if (is.character(batch$financial_currency) && nzchar(batch$financial_currency)) {
        out$financial_currency <- batch$financial_currency
      }
      if (is.finite(batch$beta)) out$beta <- batch$beta
      if (is.finite(batch$market_cap)) out$market_cap <- batch$market_cap
    }
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
  # Merge batch currency hints when Summary has no ccy attrs.
  if (is.list(batch)) {
    if ((!is.character(out$currency) || !nzchar(out$currency)) &&
        is.character(batch$currency) && nzchar(batch$currency)) {
      out$currency <- batch$currency
    }
    if ((!is.character(out$financial_currency) || !nzchar(out$financial_currency)) &&
        is.character(batch$financial_currency) && nzchar(batch$financial_currency)) {
      out$financial_currency <- batch$financial_currency
    }
  }
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
    file.path("app_27.0", "data", "universe_metrics_snapshot.csv")
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

# Shared with other processes when YNOW_CACHE_ROOT is set (local multisession
# workers do not inherit this process's .lab_mcap_cache).
# US and TW are separate files. Entries are Yahoo gap-fills only (the bundled
# snapshot is re-read each call) and expire after YNOW_MCAP_CACHE_TTL_H (default
# 24h, slow tier — not the 15m quote cache). A lock dir serializes one fill per
# market so a prefetch worker and the Ranking worker do not both hit Yahoo.
.lab_mcap_ttl_sec <- function() {
  h <- suppressWarnings(as.numeric(Sys.getenv("YNOW_MCAP_CACHE_TTL_H", "24"))[1])
  if (!is.finite(h) || h < 0) h <- 24
  h * 3600
}

.lab_mcap_market_of <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  if (is.na(tk) || !nzchar(tk)) return("US")
  if (grepl("\\.(TW|TWO)$", tk)) "TW" else "US"
}

.lab_mcap_mem_key <- function(market, ticker) {
  paste0(if (identical(market, "TW")) "TW" else "US", "|", toupper(trimws(as.character(ticker)[1])))
}

.lab_mcap_root <- function() {
  root <- Sys.getenv("YNOW_CACHE_ROOT", "")
  if (!nzchar(root)) return(NA_character_)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root
}

.lab_mcap_disk_path <- function(market = "US") {
  root <- .lab_mcap_root()
  if (is.na(root)) return(NA_character_)
  m <- if (identical(market, "TW")) "TW" else "US"
  file.path(root, sprintf("lab_mcap_cache_%s.rds", m))
}

.lab_mcap_mem_get <- function(market, ticker, now = NULL) {
  key <- .lab_mcap_mem_key(market, ticker)
  if (!exists(key, envir = .lab_mcap_cache, inherits = FALSE)) return(NA_real_)
  entry <- get(key, envir = .lab_mcap_cache, inherits = FALSE)
  if (!is.list(entry)) return(NA_real_)
  v <- suppressWarnings(as.numeric(entry$v)[1])
  at <- suppressWarnings(as.numeric(entry$at)[1])
  if (!is.finite(v) || v <= 0 || !is.finite(at)) {
    rm(list = key, envir = .lab_mcap_cache)
    return(NA_real_)
  }
  if (is.null(now)) now <- as.numeric(Sys.time())
  if ((as.numeric(now) - at) > .lab_mcap_ttl_sec()) {
    rm(list = key, envir = .lab_mcap_cache)
    return(NA_real_)
  }
  v
}

.lab_mcap_mem_put <- function(market, ticker, value, src = "yahoo", at = NULL) {
  v <- suppressWarnings(as.numeric(value)[1])
  if (!is.finite(v) || v <= 0) return(invisible(FALSE))
  if (is.null(at) || !is.finite(suppressWarnings(as.numeric(at)[1]))) {
    at <- as.numeric(Sys.time())
  }
  assign(
    .lab_mcap_mem_key(market, ticker),
    list(v = v, at = as.numeric(at)[1], src = as.character(src)[1]),
    envir = .lab_mcap_cache
  )
  invisible(TRUE)
}

.lab_mcap_disk_load <- function(market, force = FALSE) {
  market <- if (identical(market, "TW")) "TW" else "US"
  flag <- paste0(".disk_loaded_", market)
  if (!isTRUE(force) && isTRUE(get0(flag, envir = .lab_mcap_cache, inherits = FALSE))) {
    return(invisible(FALSE))
  }
  assign(flag, TRUE, envir = .lab_mcap_cache)
  path <- .lab_mcap_disk_path(market)
  saved <- NULL
  stamp_path <- NA_character_
  from_legacy <- FALSE
  if (!is.na(path) && file.exists(path)) {
    saved <- tryCatch(readRDS(path), error = function(e) NULL)
    stamp_path <- path
  } else {
    root <- .lab_mcap_root()
    legacy <- if (!is.na(root)) file.path(root, "lab_mcap_cache.rds") else NA_character_
    if (!is.na(legacy) && file.exists(legacy)) {
      saved <- tryCatch(readRDS(legacy), error = function(e) NULL)
      stamp_path <- legacy
      from_legacy <- TRUE
    }
  }
  if (is.null(saved)) return(invisible(FALSE))
  caps <- NULL
  ats <- NULL
  if (is.list(saved) && !is.null(saved$caps) && is.numeric(saved$caps)) {
    file_m <- as.character(saved$market %||% "")[1]
    if (nzchar(file_m) && !identical(file_m, market)) return(invisible(FALSE))
    caps <- saved$caps
    ats <- saved$at
  } else if (is.numeric(saved) && length(names(saved))) {
    caps <- saved
  } else {
    return(invisible(FALSE))
  }
  nms <- names(caps)
  if (!length(nms)) return(invisible(FALSE))
  file_stamp <- NA_real_
  if (!is.na(stamp_path) && file.exists(stamp_path)) {
    file_stamp <- suppressWarnings(as.numeric(file.info(stamp_path)$mtime)[1])
  }
  if (!is.finite(file_stamp)) file_stamp <- as.numeric(Sys.time())
  now <- as.numeric(Sys.time())
  ttl <- .lab_mcap_ttl_sec()
  for (i in seq_along(caps)) {
    key <- toupper(trimws(nms[[i]]))
    if (!nzchar(key) || grepl("^\\.", key)) next
    if (!identical(.lab_mcap_market_of(key), market)) next
    v <- suppressWarnings(as.numeric(caps[[i]])[1])
    at <- NA_real_
    if (is.numeric(ats) && length(ats)) {
      if (key %in% names(ats)) at <- suppressWarnings(as.numeric(ats[[key]])[1])
      if (!is.finite(at) && nms[[i]] %in% names(ats)) {
        at <- suppressWarnings(as.numeric(ats[[nms[[i]]]])[1])
      }
    }
    if (!is.finite(at)) at <- if (isTRUE(from_legacy) || is.numeric(saved)) file_stamp else NA_real_
    if (!is.finite(v) || v <= 0 || !is.finite(at)) next
    if ((now - at) > ttl) next
    memk <- .lab_mcap_mem_key(market, key)
    if (!exists(memk, envir = .lab_mcap_cache, inherits = FALSE)) {
      assign(memk, list(v = v, at = at, src = "yahoo"), envir = .lab_mcap_cache)
    }
  }
  invisible(TRUE)
}

.lab_mcap_disk_read_payload <- function(market) {
  market <- if (identical(market, "TW")) "TW" else "US"
  path <- .lab_mcap_disk_path(market)
  if (is.na(path) || !file.exists(path)) {
    return(list(caps = numeric(0), at = numeric(0)))
  }
  saved <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(saved)) return(list(caps = numeric(0), at = numeric(0)))
  caps <- NULL
  ats <- NULL
  if (is.list(saved) && !is.null(saved$caps) && is.numeric(saved$caps)) {
    file_m <- as.character(saved$market %||% "")[1]
    if (nzchar(file_m) && !identical(file_m, market)) {
      return(list(caps = numeric(0), at = numeric(0)))
    }
    caps <- saved$caps
    ats <- saved$at
  } else if (is.numeric(saved) && length(names(saved))) {
    caps <- saved
    stamp <- suppressWarnings(as.numeric(file.info(path)$mtime)[1])
    if (!is.finite(stamp)) stamp <- as.numeric(Sys.time())
    ats <- stats::setNames(rep(stamp, length(caps)), names(caps))
  } else {
    return(list(caps = numeric(0), at = numeric(0)))
  }
  nms <- names(caps)
  if (!length(nms)) return(list(caps = numeric(0), at = numeric(0)))
  keep_tk <- character(0)
  keep_v <- numeric(0)
  keep_at <- numeric(0)
  now <- as.numeric(Sys.time())
  ttl <- .lab_mcap_ttl_sec()
  for (i in seq_along(caps)) {
    key <- toupper(trimws(nms[[i]]))
    if (!nzchar(key) || grepl("^\\.", key)) next
    if (!identical(.lab_mcap_market_of(key), market)) next
    v <- suppressWarnings(as.numeric(caps[[i]])[1])
    at <- NA_real_
    if (is.numeric(ats) && length(ats)) {
      if (key %in% names(ats)) at <- suppressWarnings(as.numeric(ats[[key]])[1])
      if (!is.finite(at) && nms[[i]] %in% names(ats)) {
        at <- suppressWarnings(as.numeric(ats[[nms[[i]]]])[1])
      }
    }
    if (!is.finite(at)) next
    if (!is.finite(v) || v <= 0) next
    if ((now - at) > ttl) next
    keep_tk <- c(keep_tk, key)
    keep_v <- c(keep_v, v)
    keep_at <- c(keep_at, at)
  }
  list(
    caps = stats::setNames(keep_v, keep_tk),
    at = stats::setNames(keep_at, keep_tk)
  )
}

.lab_mcap_disk_save <- function(market) {
  market <- if (identical(market, "TW")) "TW" else "US"
  path <- .lab_mcap_disk_path(market)
  if (is.na(path)) return(invisible(FALSE))
  # Always re-read disk before replace. A Ranking worker may have
  # .disk_loaded_* set from an earlier empty load while ind-mcap (or another
  # worker) wrote a larger shared file; memory-only save would wipe those hits.
  disk <- .lab_mcap_disk_read_payload(market)
  by_tk <- new.env(parent = emptyenv())
  if (length(disk$caps)) {
    for (tk in names(disk$caps)) {
      assign(
        tk,
        list(v = as.numeric(disk$caps[[tk]])[1], at = as.numeric(disk$at[[tk]])[1]),
        envir = by_tk
      )
    }
  }
  prefix <- paste0(market, "|")
  nms <- ls(envir = .lab_mcap_cache, all.names = FALSE)
  nms <- nms[startsWith(nms, prefix)]
  now <- as.numeric(Sys.time())
  ttl <- .lab_mcap_ttl_sec()
  for (memk in nms) {
    entry <- get(memk, envir = .lab_mcap_cache, inherits = FALSE)
    if (!is.list(entry) || !identical(as.character(entry$src %||% "")[1], "yahoo")) next
    v <- suppressWarnings(as.numeric(entry$v)[1])
    at <- suppressWarnings(as.numeric(entry$at)[1])
    if (!is.finite(v) || v <= 0 || !is.finite(at)) next
    if ((now - at) > ttl) next
    tk <- substring(memk, nchar(prefix) + 1L)
    if (!nzchar(tk) || !identical(.lab_mcap_market_of(tk), market)) next
    if (exists(tk, envir = by_tk, inherits = FALSE)) {
      prev <- get(tk, envir = by_tk, inherits = FALSE)
      prev_at <- suppressWarnings(as.numeric(prev$at)[1])
      if (is.finite(prev_at) && prev_at > at) next
    }
    assign(tk, list(v = v, at = at), envir = by_tk)
  }
  keys <- ls(envir = by_tk, all.names = FALSE)
  if (!length(keys)) return(invisible(FALSE))
  keys <- sort(keys)
  vals <- numeric(length(keys))
  ats <- numeric(length(keys))
  for (i in seq_along(keys)) {
    e <- get(keys[[i]], envir = by_tk, inherits = FALSE)
    vals[[i]] <- e$v
    ats[[i]] <- e$at
    memk <- .lab_mcap_mem_key(market, keys[[i]])
    if (!exists(memk, envir = .lab_mcap_cache, inherits = FALSE)) {
      assign(
        memk,
        list(v = e$v, at = e$at, src = "yahoo"),
        envir = .lab_mcap_cache
      )
    }
  }
  payload <- list(
    version = 2L,
    market = market,
    caps = stats::setNames(vals, keys),
    at = stats::setNames(ats, keys)
  )
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  ok <- tryCatch(
    {
      saveRDS(payload, tmp)
      isTRUE(file.rename(tmp, path))
    },
    error = function(e) FALSE
  )
  if (file.exists(tmp)) unlink(tmp)
  invisible(isTRUE(ok))
}

.lab_mcap_lock_dir <- function(market) {
  root <- .lab_mcap_root()
  if (is.na(root)) return(NA_character_)
  m <- if (identical(market, "TW")) "TW" else "US"
  file.path(root, sprintf("lab_mcap_fill_%s.lock.d", m))
}

.lab_mcap_lock_acquire <- function(market) {
  d <- .lab_mcap_lock_dir(market)
  if (is.na(d)) return(FALSE)
  if (dir.exists(d)) {
    age <- suppressWarnings(as.numeric(difftime(Sys.time(), file.info(d)$mtime, units = "secs"))[1])
    if (is.finite(age) && age > 180) unlink(d, recursive = TRUE)
  }
  ok <- isTRUE(dir.create(d, recursive = FALSE, showWarnings = FALSE))
  if (!ok) return(FALSE)
  tryCatch(writeLines(as.character(Sys.getpid()), file.path(d, "pid")), error = function(e) NULL)
  TRUE
}

.lab_mcap_lock_release <- function(market) {
  d <- .lab_mcap_lock_dir(market)
  if (!is.na(d) && dir.exists(d)) unlink(d, recursive = TRUE)
  invisible(TRUE)
}

.lab_mcap_cool_path <- function(market) {
  root <- .lab_mcap_root()
  if (is.na(root)) return(NA_character_)
  m <- if (identical(market, "TW")) "TW" else "US"
  file.path(root, sprintf("lab_mcap_yahoo_cool_%s.rds", m))
}

.lab_mcap_yahoo_cooled <- function(market) {
  path <- .lab_mcap_cool_path(market)
  if (is.na(path) || !file.exists(path)) return(FALSE)
  until <- tryCatch(readRDS(path), error = function(e) NA_real_)
  until <- suppressWarnings(as.numeric(until)[1])
  is.finite(until) && until > as.numeric(Sys.time())
}

.lab_mcap_yahoo_cool_set <- function(market, sec = 900) {
  path <- .lab_mcap_cool_path(market)
  if (is.na(path)) return(invisible(FALSE))
  sec <- suppressWarnings(as.numeric(sec)[1])
  if (!is.finite(sec) || sec < 30) sec <- 900
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  ok <- tryCatch(
    {
      saveRDS(as.numeric(Sys.time()) + sec, tmp)
      isTRUE(file.rename(tmp, path))
    },
    error = function(e) FALSE
  )
  if (file.exists(tmp)) unlink(tmp)
  invisible(isTRUE(ok))
}

.lab_mcap_lock_wait <- function(market, timeout = 90) {
  d <- .lab_mcap_lock_dir(market)
  if (is.na(d)) return(invisible(FALSE))
  t0 <- Sys.time()
  while (dir.exists(d)) {
    age <- suppressWarnings(as.numeric(difftime(Sys.time(), file.info(d)$mtime, units = "secs"))[1])
    if (!is.finite(age) || age > 180) {
      unlink(d, recursive = TRUE)
      break
    }
    waited <- suppressWarnings(as.numeric(difftime(Sys.time(), t0, units = "secs"))[1])
    if (!is.finite(waited) || waited > timeout) break
    Sys.sleep(0.25)
  }
  invisible(TRUE)
}

.lab_fetch_mcap_apply_yahoo <- function(out, market, fetched) {
  n_hit <- 0L
  if (is.null(fetched) || !(is.list(fetched) || is.numeric(fetched))) {
    return(list(out = out, n_hit = n_hit))
  }
  nms <- names(fetched)
  if (is.null(nms) || !length(nms)) return(list(out = out, n_hit = n_hit))
  nms <- toupper(trimws(as.character(nms)))
  # Element-wise parse. unlist() drops Python None / R NULL and then
  # length(nms) != length(vals) discards a partial-success Yahoo dict.
  for (i in seq_along(nms)) {
    key <- gsub("/", "-", nms[[i]])
    if (!nzchar(key)) next
    el <- fetched[[i]]
    if (is.null(el) || length(el) == 0L) next
    v <- suppressWarnings(as.numeric(el)[1])
    if (!is.finite(v) || v <= 0) next
    if (!identical(.lab_mcap_market_of(key), market)) next
    if (key %in% names(out)) out[[key]] <- v
    .lab_mcap_mem_put(market, key, v, src = "yahoo")
    n_hit <- n_hit + 1L
    alt <- gsub("\\.", "-", key)
    if (!identical(alt, key) && alt %in% names(out) && !is.finite(out[[alt]])) {
      out[[alt]] <- v
      .lab_mcap_mem_put(market, alt, v, src = "yahoo")
    }
  }
  list(out = out, n_hit = n_hit)
}

.lab_fetch_mcap_market <- function(tks, market) {
  market <- if (identical(market, "TW")) "TW" else "US"
  t0 <- unname(proc.time()[["elapsed"]])
  out <- stats::setNames(rep(NA_real_, length(tks)), tks)
  if (!length(tks)) return(out)
  .lab_mcap_disk_load(market, force = FALSE)
  need <- character(0)
  n_mem <- 0L
  for (tk in tks) {
    v <- .lab_mcap_mem_get(market, tk)
    if (is.finite(v) && v > 0) {
      out[[tk]] <- v
      n_mem <- n_mem + 1L
    } else {
      need <- c(need, tk)
    }
  }
  n_snap <- 0L
  if (length(need)) {
    snap_caps <- .lab_lookup_metric_vec(need, "market_cap")
    still <- character(0)
    for (tk in need) {
      v <- suppressWarnings(as.numeric(snap_caps[[tk]])[1])
      if (is.finite(v) && v > 0) {
        out[[tk]] <- v
        .lab_mcap_mem_put(market, tk, v, src = "snap")
        n_snap <- n_snap + 1L
      } else {
        still <- c(still, tk)
      }
    }
    need <- still
  }
  waited <- FALSE
  lock_dir <- .lab_mcap_lock_dir(market)
  if (length(need) && !is.na(lock_dir) && dir.exists(lock_dir)) {
    .lab_mcap_lock_wait(market)
    waited <- TRUE
    .lab_mcap_disk_load(market, force = TRUE)
    still <- character(0)
    for (tk in need) {
      v <- .lab_mcap_mem_get(market, tk)
      if (is.finite(v) && v > 0) out[[tk]] <- v else still <- c(still, tk)
    }
    need <- still
  }
  held <- FALSE
  if (length(need)) {
    held <- .lab_mcap_lock_acquire(market)
    if (!held && !is.na(lock_dir)) {
      .lab_mcap_lock_wait(market)
      waited <- TRUE
      .lab_mcap_disk_load(market, force = TRUE)
      still <- character(0)
      for (tk in need) {
        v <- .lab_mcap_mem_get(market, tk)
        if (is.finite(v) && v > 0) out[[tk]] <- v else still <- c(still, tk)
      }
      need <- still
      if (length(need)) held <- .lab_mcap_lock_acquire(market)
    }
  }
  if (isTRUE(held)) on.exit(.lab_mcap_lock_release(market), add = TRUE)
  n_yahoo <- 0L
  n_yahoo_hit <- 0L
  cooled <- FALSE
  if (length(need) && .lab_mcap_yahoo_cooled(market)) {
    cooled <- TRUE
    need <- character(0)
  }
  if (length(need) &&
      exists(".ensure_python_scraper", mode = "function") &&
      isTRUE(.ensure_python_scraper()) &&
      exists("get_market_caps_batch", mode = "function")) {
    n_yahoo <- length(need)
    fetched <- tryCatch(get_market_caps_batch(need), error = function(e) NULL)
    applied <- .lab_fetch_mcap_apply_yahoo(out, market, fetched)
    out <- applied$out
    n_yahoo_hit <- applied$n_hit
    if (n_yahoo_hit > 0L) {
      .lab_mcap_disk_save(market)
    } else if (n_yahoo >= 40L) {
      # Empty pass (rate limit or quotes with no marketCap). Don't repeat it
      # from ind-mcap in this host for 15 minutes. Snapshot caps still apply.
      .lab_mcap_yahoo_cool_set(market, 900)
    }
  }
  if (exists("ynow_pload", mode = "function")) {
    ynow_pload("n7_mcap_gap", "end", list(
      market = market,
      n_req = length(tks),
      n_mem = n_mem,
      n_snap = n_snap,
      n_yahoo = n_yahoo,
      n_yahoo_hit = n_yahoo_hit,
      cooled = isTRUE(cooled),
      waited = isTRUE(waited),
      elapsed_s = unname(proc.time()[["elapsed"]]) - t0
    ))
  }
  out
}

#' 批次市值：記憶體與磁碟快取、離線快照，缺口再打 Yahoo。仍缺則 NA。
#' `market` 只決定預設分組；`.TW` / `.TWO` 一律進 TW 檔，不會寫進 US 快取。
lab_fetch_market_caps_usd <- function(tickers, market = NULL) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  out <- stats::setNames(rep(NA_real_, length(tks)), tks)
  if (!length(tks)) return(out)
  # Suffix chooses the file. `market` is accepted so callers can state the
  # session market; it must not pull the other market's cache into this one.
  markets <- vapply(tks, .lab_mcap_market_of, character(1))
  if (!is.null(market)) {
    hint <- toupper(trimws(as.character(market)[1]))
    if (identical(hint, "TW") || identical(hint, "US")) {
      markets <- ifelse(grepl("\\.(TW|TWO)$", tks), "TW", hint)
    }
  }
  for (m in unique(markets)) {
    sub <- tks[markets == m]
    got <- .lab_fetch_mcap_market(sub, m)
    if (length(got)) out[names(got)] <- got
  }
  out
}

#' 掛上市值，供候選 > N 時依市值降序截斷評估池（非規模篩選）
lab_attach_market_caps <- function(pool) {
  pool <- lab_dedupe_eval_pool(pool)
  if (is.null(pool) || nrow(pool) == 0L) return(pool)
  mm <- NULL
  if (exists("get_market_mode", mode = "function")) {
    mm <- tryCatch(get_market_mode(), error = function(e) NULL)
  }
  caps <- lab_fetch_market_caps_usd(pool$ticker, market = mm)
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
