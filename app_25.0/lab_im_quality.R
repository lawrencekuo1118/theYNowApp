# ==========================================
# lab_im_quality.R — extracted from lab_industry_method.R (C-STRUCT-03)
# Behavior-preserving structural split; sourced by lab_industry_method.R.
# ==========================================

LAB_QUALITY_SNAPSHOT_TTL_SEC <- 24 * 60 * 60

lab_quality_snapshot_empty <- function() {
  data.frame(
    ticker = character(0),
    market = character(0),
    updated_at = as.POSIXct(character(0)),
    f_score = numeric(0),
    quality_flag = numeric(0),
    n_fs_alerts = integer(0),
    ok = logical(0),
    error = character(0),
    stringsAsFactors = FALSE
  )
}

lab_quality_snapshot_dir <- function() {
  override <- Sys.getenv("YNOW_SLOW_CACHE_DIR", unset = "")
  d <- if (nzchar(override)) {
    override
  } else if (exists(".ynow_slow_cache_dir", mode = "function")) {
    .ynow_slow_cache_dir()
  } else {
    file.path(tempdir(), "ynow_cache", "slow")
  }
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

lab_quality_snapshot_path <- function(market = "US") {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  file.path(lab_quality_snapshot_dir(), sprintf("lab_quality_snapshot_%s.rds", mm))
}

lab_load_quality_snapshot <- function(market = "US") {
  path <- lab_quality_snapshot_path(market)
  if (!file.exists(path)) return(lab_quality_snapshot_empty())
  snap <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(snap) || !nrow(snap) || !"ticker" %in% names(snap)) {
    return(lab_quality_snapshot_empty())
  }
  base <- lab_quality_snapshot_empty()
  for (nm in names(base)) {
    if (!nm %in% names(snap)) snap[[nm]] <- base[[nm]][0]
  }
  snap <- snap[, names(base), drop = FALSE]
  snap$ticker <- toupper(trimws(as.character(snap$ticker)))
  snap <- snap[!is.na(snap$ticker) & nzchar(snap$ticker), , drop = FALSE]
  snap <- snap[!duplicated(snap$ticker), , drop = FALSE]
  snap$updated_at <- as.POSIXct(snap$updated_at, origin = "1970-01-01", tz = "UTC")
  snap$f_score <- suppressWarnings(as.numeric(snap$f_score))
  snap$quality_flag <- suppressWarnings(as.numeric(snap$quality_flag))
  snap$n_fs_alerts <- suppressWarnings(as.integer(snap$n_fs_alerts))
  snap$ok <- as.logical(snap$ok)
  snap$ok[is.na(snap$ok)] <- FALSE
  rownames(snap) <- NULL
  snap
}

lab_save_quality_snapshot <- function(snapshot, market = "US") {
  path <- lab_quality_snapshot_path(market)
  snap <- if (is.data.frame(snapshot)) snapshot else lab_quality_snapshot_empty()
  if (!nrow(snap)) snap <- lab_quality_snapshot_empty()
  tmp <- paste0(path, ".tmp")
  saveRDS(snap, tmp)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

#' Statement-only quality fields (no price / fair value).
lab_compute_quality_metrics <- function(ticker, industry_key = NULL) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  out <- list(
    ticker = tk,
    f_score = NA_real_,
    quality_flag = NA_real_,
    n_fs_alerts = NA_integer_,
    ok = FALSE,
    error = NA_character_
  )
  if (!nzchar(tk)) {
    out$error <- "empty ticker"
    return(out)
  }
  if (!exists("cached_scrape_financials", mode = "function")) {
    out$error <- "financials unavailable"
    return(out)
  }
  res <- tryCatch(cached_scrape_financials(tk), error = function(e) e)
  if (inherits(res, "error") || is.null(res) || !is.list(res)) {
    out$error <- if (inherits(res, "error")) conditionMessage(res) else "no statements"
    return(out)
  }
  d_is <- tryCatch(res[["Income Statement"]]$expanded, error = function(e) NULL)
  d_bs <- tryCatch(res[["Balance Sheet"]]$expanded, error = function(e) NULL)
  d_cf <- tryCatch(res[["Cash Flow"]]$expanded, error = function(e) NULL)
  fs <- if (exists("compute_report_f_score", mode = "function")) {
    tryCatch(
      compute_report_f_score(d_is, d_bs, d_cf),
      error = function(e) list(total = NA_real_, quality_flag = NA_real_)
    )
  } else {
    list(total = NA_real_, quality_flag = NA_real_)
  }
  out$f_score <- suppressWarnings(as.numeric(fs$total)[1])
  out$quality_flag <- suppressWarnings(as.numeric(fs$quality_flag)[1])
  ind_key <- as.character(industry_key %||% "")[1]
  if (exists("collect_fraud_warnings", mode = "function")) {
    warns <- tryCatch(
      collect_fraud_warnings(d_cf, d_is, d_bs, industry_key = ind_key),
      error = function(e) character(0)
    )
    out$n_fs_alerts <- as.integer(length(warns))
  }
  out$ok <- is.finite(out$f_score) || is.finite(out$quality_flag) ||
    is.finite(out$n_fs_alerts)
  if (!isTRUE(out$ok) && is.na(out$error)) out$error <- "quality metrics unavailable"
  out
}

lab_quality_row_is_fresh <- function(updated_at, ok, now = Sys.time(),
                                     ttl_sec = LAB_QUALITY_SNAPSHOT_TTL_SEC) {
  isTRUE(ok) && !is.na(updated_at) &&
    is.finite(as.numeric(difftime(now, updated_at, units = "secs"))) &&
    as.numeric(difftime(now, updated_at, units = "secs")) <= ttl_sec
}

#' Upsert quality rows. `rows` needs ticker plus f_score / quality_flag / n_fs_alerts.
lab_upsert_quality_rows <- function(rows, market = "US", now = Sys.time()) {
  mm <- tryCatch(normalize_market_mode(market), error = function(e) "US")
  snap <- lab_load_quality_snapshot(mm)
  if (!is.data.frame(rows) || !nrow(rows) || !"ticker" %in% names(rows)) {
    return(snap)
  }
  add <- lab_quality_snapshot_empty()
  add <- add[seq_len(nrow(rows)), , drop = FALSE]
  add$ticker <- toupper(trimws(as.character(rows$ticker)))
  add$market <- mm
  add$updated_at <- as.POSIXct(now, tz = "UTC")
  add$f_score <- suppressWarnings(as.numeric(rows$f_score))
  add$quality_flag <- suppressWarnings(as.numeric(rows$quality_flag))
  add$n_fs_alerts <- suppressWarnings(as.integer(rows$n_fs_alerts))
  add$ok <- is.finite(add$f_score) | is.finite(add$quality_flag) | is.finite(add$n_fs_alerts)
  add$error <- NA_character_
  add <- add[!is.na(add$ticker) & nzchar(add$ticker), , drop = FALSE]
  add <- add[!duplicated(add$ticker), , drop = FALSE]
  if (!nrow(add)) return(snap)
  if (nrow(snap)) snap <- snap[!snap$ticker %in% add$ticker, , drop = FALSE]
  snap <- rbind(snap, add)
  lab_save_quality_snapshot(snap, mm)
  snap
}

#' Refresh quality snapshot for the structural pool. Fresh rows (≤ 24h) are kept.
lab_refresh_quality_snapshot <- function(pool, market = "US", force = FALSE,
                                         progress_cb = NULL, compute_fn = NULL,
                                         now = Sys.time()) {
  mm <- tryCatch(normalize_market_mode(market), error = function(e) "US")
  if (is.character(pool)) {
    pool <- data.frame(
      ticker = pool, industry_key = NA_character_, stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(pool) || !nrow(pool) || !"ticker" %in% names(pool)) {
    return(list(
      snapshot = lab_load_quality_snapshot(mm),
      n_run = 0L, n_skip = 0L, n_pool = 0L
    ))
  }
  pool$ticker <- toupper(trimws(as.character(pool$ticker)))
  if (!"industry_key" %in% names(pool)) pool$industry_key <- NA_character_
  pool <- pool[!is.na(pool$ticker) & nzchar(pool$ticker), , drop = FALSE]
  pool <- pool[!duplicated(pool$ticker), , drop = FALSE]
  snap <- lab_load_quality_snapshot(mm)
  n <- nrow(pool)
  n_run <- 0L
  n_skip <- 0L
  fn <- if (is.function(compute_fn)) compute_fn else lab_compute_quality_metrics
  for (i in seq_len(n)) {
    tk <- pool$ticker[[i]]
    if (is.function(progress_cb)) {
      tryCatch(progress_cb(i, n, tk), error = function(e) NULL)
    }
    hit <- if (nrow(snap)) match(tk, snap$ticker) else NA_integer_
    if (!isTRUE(force) && !is.na(hit) &&
        lab_quality_row_is_fresh(snap$updated_at[[hit]], snap$ok[[hit]], now = now)) {
      n_skip <- n_skip + 1L
      next
    }
    ev <- tryCatch(
      fn(tk, pool$industry_key[[i]]),
      error = function(e) list(
        ticker = tk, f_score = NA_real_, quality_flag = NA_real_,
        n_fs_alerts = NA_integer_, ok = FALSE, error = conditionMessage(e)
      )
    )
    row <- lab_quality_snapshot_empty()
    row <- row[1, , drop = FALSE]
    row$ticker <- tk
    row$market <- mm
    row$updated_at <- as.POSIXct(now, tz = "UTC")
    row$f_score <- suppressWarnings(as.numeric(ev$f_score)[1])
    row$quality_flag <- suppressWarnings(as.numeric(ev$quality_flag)[1])
    row$n_fs_alerts <- suppressWarnings(as.integer(ev$n_fs_alerts)[1])
    row$ok <- isTRUE(ev$ok) || is.finite(row$f_score) || is.finite(row$quality_flag) ||
      is.finite(row$n_fs_alerts)
    row$error <- as.character(ev$error %||% NA_character_)[1]
    if (!is.na(hit)) {
      snap[hit, ] <- row
    } else {
      snap <- rbind(snap, row)
    }
    n_run <- n_run + 1L
  }
  lab_save_quality_snapshot(snap, mm)
  list(snapshot = snap, n_run = n_run, n_skip = n_skip, n_pool = n)
}

#' Pool size after quality gates. Missing snapshot rows are excluded only while
#' a gate is checked. With every gate off, the structural pool size is returned.
#' If a gate is checked but the snapshot covers none of the structural pool
#' (common on TW before Search / refresh), keep the structural count — do not
#' report 0 as if every name failed the gate.
lab_apply_quality_snapshot <- function(tickers, snapshot,
                                       eq_only = FALSE, gate_only = FALSE,
                                       no_alert = FALSE, now = Sys.time()) {
  tickers <- unique(as.character(tickers))
  tickers <- tickers[!is.na(tickers) & nzchar(tickers)]
  n_pool <- length(tickers)
  snap <- if (is.data.frame(snapshot)) snapshot else lab_quality_snapshot_empty()
  idx <- if (n_pool && nrow(snap)) match(tickers, as.character(snap$ticker)) else rep(NA_integer_, n_pool)
  covered <- rep(FALSE, n_pool)
  if (n_pool && any(!is.na(idx))) {
    hit <- which(!is.na(idx))
    covered[hit] <- isTRUE(nrow(snap) > 0) & as.logical(snap$ok[idx[hit]])
    covered[is.na(covered)] <- FALSE
  }
  n_covered <- sum(covered)
  updated_at <- as.POSIXct(NA)
  if (n_covered > 0L) {
    updated_at <- max(snap$updated_at[idx[covered]], na.rm = TRUE)
    if (!is.finite(as.numeric(updated_at))) updated_at <- as.POSIXct(NA)
  }
  gates_on <- isTRUE(eq_only) || isTRUE(gate_only) || isTRUE(no_alert)
  n_show <- n_pool
  gates_applied <- FALSE
  if (gates_on && n_covered < 1L) {
    # No usable quality rows for this pool — defer gates; keep structural N.
    n_show <- n_pool
    gates_applied <- FALSE
  } else if (gates_on) {
    pass <- rep(FALSE, n_pool)
    sub_i <- which(covered)
    rows <- snap[idx[sub_i], , drop = FALSE]
    p <- rep(TRUE, length(sub_i))
    if (isTRUE(eq_only)) {
      qf <- suppressWarnings(as.numeric(rows$quality_flag))
      p <- p & !is.na(qf) & qf %in% 1
    }
    if (isTRUE(gate_only)) {
      fs <- suppressWarnings(as.numeric(rows$f_score))
      p <- p & is.finite(fs) & fs >= 7
    }
    if (isTRUE(no_alert)) {
      na_n <- suppressWarnings(as.numeric(rows$n_fs_alerts))
      p <- p & is.finite(na_n) & na_n <= 0
    }
    pass[sub_i] <- p
    n_show <- sum(pass)
    gates_applied <- TRUE
  }
  age_sec <- if (!is.na(updated_at)) {
    as.numeric(difftime(now, updated_at, units = "secs"))
  } else {
    NA_real_
  }
  list(
    n_show = as.integer(n_show),
    n_pool = as.integer(n_pool),
    n_covered = as.integer(n_covered),
    n_missing = as.integer(n_pool - n_covered),
    updated_at = updated_at,
    stale = is.finite(age_sec) && age_sec > LAB_QUALITY_SNAPSHOT_TTL_SEC,
    gates_on = gates_on,
    gates_applied = gates_applied
  )
}

#' 排行榜候選池：有年化漲幅；可選 F-Score≥7／無財報警訊；同一代碼只留最高 CAGR 一列
#' @param gate_only 若 TRUE（預設），只保留 F-Score≥7；FALSE＝不設 F 門檻（仍須有年化漲幅）
#' @param no_alert 若 TRUE，只保留財報警訊數＝0（Schilit＋盈餘品質紅旗）
lab_leaderboard_pool <- function(merged_df, eq_only = FALSE, gate_only = TRUE,
                                 no_alert = FALSE) {
  empty <- merged_df[0, , drop = FALSE]
  if (is.null(merged_df) || !is.data.frame(merged_df) || nrow(merged_df) == 0) {
    return(empty)
  }
  df <- merged_df
  if (isTRUE(eq_only) && "quality_flag" %in% names(df)) {
    df <- df[!is.na(df$quality_flag) & df$quality_flag %in% 1, , drop = FALSE]
  }
  fs <- suppressWarnings(as.numeric(df$f_score))
  keep <- is.finite(df$upside_cagr_pct)
  if (isTRUE(gate_only)) {
    keep <- keep & is.finite(fs) & fs >= 7
  }
  if (isTRUE(no_alert) && "n_fs_alerts" %in% names(df)) {
    na_n <- suppressWarnings(as.numeric(df$n_fs_alerts))
    keep <- keep & is.finite(na_n) & na_n <= 0
  }
  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0) return(df)
  df <- df[order(-df$upside_cagr_pct, df$ticker), , drop = FALSE]
  df[!duplicated(df$ticker), , drop = FALSE]
}

#' 排行產業下拉：全部 + 目前排行候選池內的產業標籤
#' @return named character vector（顯示名 = 值；`__all__` 為全部）
lab_leaderboard_industry_choices <- function(merged_df, eq_only = FALSE,
                                            gate_only = TRUE,
                                            no_alert = FALSE,
                                            all_label = "全部產業") {
  pool <- lab_leaderboard_pool(
    merged_df, eq_only = eq_only, gate_only = gate_only, no_alert = no_alert
  )
  labs <- if (nrow(pool) > 0 && "industry_label" %in% names(pool)) {
    unique(as.character(pool$industry_label))
  } else {
    character(0)
  }
  labs <- labs[nzchar(labs) & !is.na(labs)]
  labs <- sort(unique(labs))
  out <- c(stats::setNames("__all__", all_label))
  if (length(labs)) {
    out <- c(out, stats::setNames(labs, labs))
  }
  out
}

#' 績優排行榜：同一評估／明細集合中，依年化漲幅降序取 Top-K（預設再套 F-Score≥7）
#' （只截斷顯示，不另抽樣；輸入應已是本次評估的 N 檔。）
#' @param eq_only 若 TRUE，再只保留盈餘品質通過者（與 Piotroski 高門檻獨立）
#' @param gate_only 若 TRUE，只列 F-Score≥7；FALSE＝不設 F 門檻
#' @param no_alert 若 TRUE，只列財報警訊數＝0者
#' @param scope `"overall"`＝整體前十（跨產業、含產業欄；依年化估值漲幅）；
#'   `"by_industry"`＝選定產業內前十（單一 Top-K；產業內排名自該產業第 1 名起算）；
#'   `"industry_avg"`＝產業市值加權年化估值漲幅排名（每元市值加權平均）；
#'   `"undervalued"`＝價值低估前十（跨產業；依推薦主模型總潛在漲幅＝FV vs 市價差距，非年化）
#' @param industry_filter 選定產業：`industry_key` 或 `industry_label` 字串向量；
#'   `NULL`／空／`"__all__"`＝不另限產業（由呼叫端決定是否已篩過）
lab_quality_leaderboard <- function(merged_df, top_n = 10L, eq_only = FALSE,
                                    gate_only = TRUE, no_alert = FALSE,
                                    scope = c("overall", "by_industry", "industry_avg",
                                              "undervalued"),
                                    industry_filter = NULL) {
  scope <- match.arg(scope)
  if (identical(scope, "industry_avg")) {
    return(lab_industry_mcap_upside_leaderboard(
      merged_df, top_n = top_n, eq_only = eq_only, gate_only = gate_only,
      no_alert = no_alert, industry_filter = industry_filter
    ))
  }
  # Overall / undervalued: Rank, Ticker, Company, Industry, Model, upsides, F-Score.
  # Selected-industry Top 10 also inserts Industry, then Industry rank, after Rank.
  lb_cols_cross <- c(
    "排名", "代號", "公司名稱", "產業", "估值方法",
    "年化估值漲幅", "總潛在漲幅", "F-Score"
  )
  lb_cols_by_ind <- c(
    "排名", "產業", "產業內排名", "代號", "公司名稱", "估值方法",
    "年化估值漲幅", "總潛在漲幅", "F-Score"
  )
  empty <- data.frame(
    排名 = integer(0), 代號 = character(0), 公司名稱 = character(0), 產業 = character(0),
    估值方法 = character(0),
    年化估值漲幅 = character(0),
    總潛在漲幅 = character(0),
    `F-Score` = numeric(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (identical(scope, "by_industry")) {
    empty$產業內排名 <- integer(0)
    empty <- empty[, lb_cols_by_ind, drop = FALSE]
  } else {
    empty <- empty[, lb_cols_cross, drop = FALSE]
  }
  df <- lab_leaderboard_pool(
    merged_df, eq_only = eq_only, gate_only = gate_only, no_alert = no_alert
  )
  if (nrow(df) == 0) return(empty)

  indf <- as.character(industry_filter %||% character(0))
  indf <- indf[!is.na(indf) & nzchar(indf)]
  indf <- indf[!indf %in% c("__all__", "NULL", "null")]
  if (length(indf) > 0L) {
    ind_lab <- if ("industry_label" %in% names(df)) as.character(df$industry_label) else rep("", nrow(df))
    ind_key <- if ("industry_key" %in% names(df)) as.character(df$industry_key) else rep("", nrow(df))
    keep <- (ind_key %in% indf) | (ind_lab %in% indf)
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0) return(empty)

  top_n <- max(1L, as.integer(top_n)[1])
  .fmt_lb_rows <- function(sub, ind_rank_values = NULL) {
    if (nrow(sub) == 0) return(NULL)
    meth <- as.character(sub$method_used)
    prim <- as.character(sub$primary)
    miss <- is.na(meth) | !nzchar(meth)
    meth[miss] <- prim[miss]
    meth <- toupper(meth)
    meth[is.na(meth) | !nzchar(meth)] <- "—"
    yahoo_nm <- if ("company_name" %in% names(sub)) sub$company_name else NA_character_
    names_out <- vapply(seq_len(nrow(sub)), function(i) {
      nm <- lab_company_display_name(sub$ticker[[i]], yahoo_nm[[i]])
      if (!nzchar(nm) || identical(nm, "—")) as.character(sub$ticker[[i]]) else nm
    }, character(1))
    ind_lab <- if ("industry_label" %in% names(sub)) {
      as.character(sub$industry_label)
    } else {
      rep("—", nrow(sub))
    }
    ind_lab[is.na(ind_lab) | !nzchar(ind_lab)] <- "—"
    out <- data.frame(
      排名 = seq_len(nrow(sub)),
      代號 = if (exists("display_tickers_for_market", mode = "function")) {
        display_tickers_for_market(sub$ticker, tryCatch(get_market_mode(), error = function(e) "US"))
      } else {
        sub$ticker
      },
      公司名稱 = names_out,
      產業 = ind_lab,
      估值方法 = meth,
      年化估值漲幅 = sprintf("%+.1f%%", sub$upside_cagr_pct),
      總潛在漲幅 = ifelse(
        is.na(sub$upside_total_pct), "—", sprintf("%+.1f%%", sub$upside_total_pct)
      ),
      `F-Score` = sub$f_score,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cols <- lb_cols_cross
    if (!is.null(ind_rank_values) && length(ind_rank_values) == nrow(sub)) {
      out$產業內排名 <- as.integer(ind_rank_values)
      cols <- lb_cols_by_ind
    }
    out[, cols, drop = FALSE]
  }

  # Within-industry rank: sort by industry then CAGR so each industry starts at 1
  ind_key_vec <- if ("industry_key" %in% names(df)) {
    as.character(df$industry_key)
  } else if ("industry_label" %in% names(df)) {
    as.character(df$industry_label)
  } else {
    rep("", nrow(df))
  }
  ind_key_vec[is.na(ind_key_vec) | !nzchar(ind_key_vec)] <- "—"
  o_ind <- order(
    is.na(df$upside_cagr_pct), ind_key_vec, -df$upside_cagr_pct, df$ticker,
    na.last = TRUE
  )
  df_ind <- df[o_ind, , drop = FALSE]
  ind_key_sorted <- ind_key_vec[o_ind]
  df_ind$.ind_rank <- ave(seq_len(nrow(df_ind)), ind_key_sorted, FUN = seq_along)

  if (identical(scope, "undervalued")) {
    # 價值低估前十：推薦主模型總潛在漲幅（FV vs 市價，非年化）由大到小；只列仍被低估者
    tot <- suppressWarnings(as.numeric(df_ind$upside_total_pct))
    keep_uv <- is.finite(tot) & tot > 0
    df_uv <- df_ind[keep_uv, , drop = FALSE]
    tot <- tot[keep_uv]
    if (nrow(df_uv) == 0L) return(empty)
    o_uv <- order(-tot, df_uv$ticker, na.last = TRUE)
    df_uv <- df_uv[o_uv, , drop = FALSE]
    df_uv <- head(df_uv, top_n)
    out <- .fmt_lb_rows(df_uv)
    if (is.null(out) || nrow(out) == 0) return(empty)
    return(out)
  }

  # Cross-industry Top-K still by global CAGR (single list)
  o_glob <- order(
    is.na(df_ind$upside_cagr_pct), -df_ind$upside_cagr_pct, df_ind$ticker,
    na.last = TRUE
  )
  df_glob <- df_ind[o_glob, , drop = FALSE]
  df_glob <- head(df_glob, top_n)

  if (identical(scope, "by_industry")) {
    # 產業內排名 = that name's rank within its industry (1 = best in industry),
    # not its place in the cross-industry Top 10.
    out <- .fmt_lb_rows(df_glob, ind_rank_values = df_glob$.ind_rank)
    if (is.null(out) || nrow(out) == 0) return(empty)
    return(out)
  }

  out <- .fmt_lb_rows(df_glob)
  if (is.null(out) || nrow(out) == 0) return(empty)
  out
}

#' Industry ranking by market-cap-weighted average annualized valuation upside
#' (每元市值加權：Σ(mcap × upside) / Σ(mcap)；缺市值列不計入權重)
lab_industry_mcap_upside_leaderboard <- function(merged_df, top_n = 10L,
                                                eq_only = FALSE, gate_only = TRUE,
                                                no_alert = FALSE,
                                                industry_filter = NULL,
                                                locale = "zh-TW") {
  empty <- data.frame(
    排名 = integer(0),
    產業 = character(0),
    市值加權年化估值漲幅 = character(0),
    總市值 = character(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  df <- lab_leaderboard_pool(
    merged_df, eq_only = eq_only, gate_only = gate_only, no_alert = no_alert
  )
  if (nrow(df) == 0) return(empty)

  indf <- as.character(industry_filter %||% character(0))
  indf <- indf[!is.na(indf) & nzchar(indf)]
  indf <- indf[!indf %in% c("__all__", "NULL", "null")]
  if (length(indf) > 0L) {
    ind_lab <- if ("industry_label" %in% names(df)) as.character(df$industry_label) else rep("", nrow(df))
    ind_key <- if ("industry_key" %in% names(df)) as.character(df$industry_key) else rep("", nrow(df))
    keep <- (ind_key %in% indf) | (ind_lab %in% indf)
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0) return(empty)

  # Ensure market_cap when missing (best-effort; offline snapshot / Yahoo)
  if (!"market_cap" %in% names(df) ||
      !any(is.finite(suppressWarnings(as.numeric(df$market_cap))) &
             suppressWarnings(as.numeric(df$market_cap)) > 0, na.rm = TRUE)) {
    if (exists("lab_attach_market_caps", mode = "function")) {
      df <- tryCatch(lab_attach_market_caps(df), error = function(e) df)
    }
  }
  mcap <- suppressWarnings(as.numeric(df$market_cap))
  ups <- suppressWarnings(as.numeric(df$upside_cagr_pct))
  ok <- is.finite(mcap) & mcap > 0 & is.finite(ups)
  df <- df[ok, , drop = FALSE]
  mcap <- mcap[ok]
  ups <- ups[ok]
  if (nrow(df) == 0) return(empty)

  ind_lab <- if ("industry_label" %in% names(df)) {
    as.character(df$industry_label)
  } else if ("industry_key" %in% names(df)) {
    as.character(df$industry_key)
  } else {
    rep("—", nrow(df))
  }
  ind_lab[is.na(ind_lab) | !nzchar(ind_lab)] <- "—"
  ind_key <- if ("industry_key" %in% names(df)) {
    as.character(df$industry_key)
  } else {
    ind_lab
  }
  ind_key[is.na(ind_key) | !nzchar(ind_key)] <- ind_lab[is.na(ind_key) | !nzchar(ind_key)]

  keys <- unique(ind_key)
  rows <- lapply(keys, function(k) {
    hit <- which(ind_key == k)
    w <- mcap[hit]
    u <- ups[hit]
    wavg <- sum(w * u) / sum(w)
    lab <- ind_lab[hit][[1]]
    data.frame(
      industry_key = k,
      industry_label = lab,
      wavg_cagr = wavg,
      total_mcap = sum(w),
      stringsAsFactors = FALSE
    )
  })
  agg <- do.call(rbind, rows)
  agg <- agg[order(-agg$wavg_cagr, agg$industry_label), , drop = FALSE]
  top_n <- max(1L, as.integer(top_n)[1])
  agg <- head(agg, top_n)
  if (nrow(agg) == 0) return(empty)

  .fmt_mcap <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x) || x <= 0) return("—")
    if (x >= 1e12) sprintf("%.2fT", x / 1e12)
    else if (x >= 1e9) sprintf("%.2fB", x / 1e9)
    else if (x >= 1e6) sprintf("%.2fM", x / 1e6)
    else sprintf("%.0f", x)
  }
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  ind_disp <- vapply(seq_len(nrow(agg)), function(i) {
    k <- as.character(agg$industry_key[[i]] %||% "")[1]
    if (nzchar(k) && exists("industry_label", mode = "function")) {
      lab <- tryCatch(industry_label(k, locale = loc), error = function(e) NA_character_)
      if (is.character(lab) && length(lab) && nzchar(lab[[1]]) && !is.na(lab[[1]])) {
        return(as.character(lab[[1]]))
      }
    }
    as.character(agg$industry_label[[i]] %||% k)[1]
  }, character(1))
  lab_localize_im_colnames(
    data.frame(
      排名 = seq_len(nrow(agg)),
      產業 = ind_disp,
      市值加權年化估值漲幅 = sprintf("%+.1f%%", agg$wavg_cagr),
      總市值 = vapply(agg$total_mcap, .fmt_mcap, character(1)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    loc
  )
}
