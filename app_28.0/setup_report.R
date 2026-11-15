# ==========================================
# setup_report.R — Investment report helpers (PDF / KPI / rating)
# Extracted from setup.R (C-STRUCT-04). Sourced by setup.R after SGR.
# ==========================================

# =========================================================
# 📄 投資意見報告書輔助函數（券商研究報告格式）
# =========================================================

# 蒐集財報警訊／體質警訊（Schilit + 盈餘品質紅旗；只列「警示」）
collect_fraud_warnings <- function(d_cf, d_is, d_bs, industry_key = NULL) {
  out <- character(0)
  if (exists("evaluate_shenanigans", mode = "function")) {
    ev <- tryCatch(
      evaluate_shenanigans(d_is, d_bs, d_cf, industry_key = industry_key),
      error = function(e) NULL
    )
    if (!is.null(ev) && isTRUE(ev$ok) && !is.null(ev$items) && nrow(ev$items) >= 1L) {
      al <- ev$items[ev$items$status == "警示", , drop = FALSE]
      if (nrow(al) >= 1L) {
        out <- c(out, paste0(al$code, " ", al$name, "：", al$reason))
      }
    }
  }
  if (exists("collect_earnings_quality_warnings", mode = "function")) {
    eq_flags <- tryCatch(
      collect_earnings_quality_warnings(d_is, d_bs, d_cf),
      error = function(e) character(0)
    )
    if (length(eq_flags) > 0L) out <- c(out, eq_flags)
  }
  unique(out)
}

# Piotroski F-Score（與決策看板 checklist 一致；供 PDF 報告）
compute_report_f_score <- function(d_is, d_bs, d_cf) {
  empty <- list(
    total = NA_real_,
    quality_flag = NA_real_,
    checklist = data.frame(
      `檢驗維度` = character(0),
      `得分` = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  if (is.null(d_is) || is.null(d_bs) || is.null(d_cf) ||
      !is.data.frame(d_is) || !is.data.frame(d_bs) || !is.data.frame(d_cf) ||
      nrow(d_is) == 0 || nrow(d_bs) == 0 || nrow(d_cf) == 0) {
    return(empty)
  }
  get_row2 <- function(df, label) {
    res <- select_clean_metric_row(df, label, include_ttm = FALSE)
    if (length(res) < 2) return(c(NA_real_, NA_real_))
    suppressWarnings(as.numeric(res[1:2]))
  }
  # Piotroski 流動比率用流動資產合計／流動負債合計。Yahoo HTML 列名為
  # Total Current *；yfinance 同一列是 Current Assets／Current Liabilities。
  # 先取 Total*，再取未加 Total 的合計列。錨定比對，避免 Other Current Assets 搶先。
  get_row2_any <- function(df, labels) {
    res <- select_clean_metric_row_any(df, labels, include_ttm = FALSE)
    if (length(res) < 2) return(c(NA_real_, NA_real_))
    suppressWarnings(as.numeric(res[1:2]))
  }
  tryCatch({
    net_inc  <- get_row2(d_is, "Net Income Common Stockholders|Net Income$")
    revenue  <- get_row2(d_is, "Total Revenue")
    gp       <- get_row2(d_is, "Gross Profit")
    assets   <- get_row2(d_bs, "Total Assets")
    lt_debt  <- get_row2(d_bs, "Long Term Debt|Total Non Current Liabilities")
    cur_ast  <- get_row2_any(d_bs, c("^Total Current Assets$", "^Current Assets$"))
    cur_liab <- get_row2_any(d_bs, c("^Total Current Liabilities$", "^Current Liabilities$"))
    shares   <- get_row2(d_bs, "Ordinary Shares Number")
    ocf      <- get_row2_any(d_cf, CASHFLOW_OCF_PATTERNS)
    oe_row   <- get_operating_earnings_row(d_is, include_ttm = FALSE)
    op_earn  <- {
      v <- suppressWarnings(as.numeric(oe_row))
      if (length(v) < 1L || all(is.na(v))) {
        c(NA_real_, NA_real_)
      } else {
        c(v[1], if (length(v) >= 2L) v[2] else NA_real_)
      }
    }

    p1 <- ifelse(!is.na(net_inc[1]) && !is.na(assets[1]) && assets[1] != 0 && (net_inc[1] / assets[1]) > 0, 1, 0)
    p2 <- ifelse(!is.na(ocf[1]) && ocf[1] > 0, 1, 0)
    p3 <- ifelse(all(!is.na(net_inc[1:2]), !is.na(assets[1:2])) &&
                   assets[1] != 0 && assets[2] != 0 &&
                   (net_inc[1] / assets[1]) > (net_inc[2] / assets[2]), 1, 0)
    # 盈餘品質：OCF > 營運利潤（淨利 − 投資證券未實現損益）
    p4 <- ifelse(!is.na(ocf[1]) && !is.na(op_earn[1]) && ocf[1] > op_earn[1], 1, 0)
    p5 <- ifelse(all(!is.na(lt_debt[1:2]), !is.na(assets[1:2])) &&
                   assets[1] != 0 && assets[2] != 0 &&
                   (lt_debt[1] / assets[1]) <= (lt_debt[2] / assets[2]), 1, 0)
    p6 <- ifelse(all(!is.na(cur_ast[1:2]), !is.na(cur_liab[1:2])) &&
                   cur_liab[1] != 0 && cur_liab[2] != 0 &&
                   (cur_ast[1] / cur_liab[1]) > (cur_ast[2] / cur_liab[2]), 1, 0)
    p7 <- ifelse(!is.na(shares[1]) && !is.na(shares[2]) && shares[1] <= (shares[2] * 1.02), 1, 0)
    p8 <- ifelse(all(!is.na(gp[1:2]), !is.na(revenue[1:2])) &&
                   revenue[1] != 0 && revenue[2] != 0 &&
                   (gp[1] / revenue[1]) > (gp[2] / revenue[2]), 1, 0)
    p9 <- ifelse(all(!is.na(revenue[1:2]), !is.na(assets[1:2])) &&
                   assets[1] != 0 && assets[2] != 0 &&
                   (revenue[1] / assets[1]) > (revenue[2] / assets[2]), 1, 0)
    scores <- c(p1, p2, p3, p4, p5, p6, p7, p8, p9)
    list(
      total = sum(scores),
      quality_flag = p4,
      checklist = data.frame(
        `檢驗維度` = c(
          "獲利性 (ROA > 0)", "獲利性 (OCF > 0)", "獲利性 (ROA 成長)",
          "獲利性 (盈餘品質：OCF > 營業利益)",
          "安全性 (槓桿下降)", "安全性 (流動比提升)", "安全性 (未大幅增資)",
          "效率 (毛利率提升)", "效率 (資產週轉率提升)"
        ),
        `得分` = ifelse(scores == 1, "通過", "未達標"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }, error = function(e) empty)
}

# 建立 KPI 摘要表（對應研究報告「關鍵財務指標」區塊）
build_report_kpi_df <- function(d_is, d_bs, d_cf) {
  pct <- function(x) if (is.na(x)) "N/A" else paste0(sprintf("%.1f", x), "%")
  num <- function(x) if (is.na(x)) "N/A" else format_dollar_abbr(x)
  
  gm_m <- tryCatch(
    kpi_gross_margin_box_metrics(d_is),
    error = function(e) list(mode = "unavailable", margin_pct = NA_real_)
  )
  rev <- get_avg(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
  net <- get_avg(select_clean_metric_row_any(d_is, NET_INCOME_PATTERNS, include_ttm = FALSE))
  ocf <- get_avg(select_clean_metric_row(d_cf, "Operating Cash Flow", include_ttm = FALSE))
  fcf <- get_avg(select_clean_metric_row(d_cf, "Free Cash Flow", include_ttm = FALSE))
  assets <- get_avg(select_clean_metric_row(d_bs, "Total Assets", include_ttm = FALSE))
  equity <- get_avg(select_clean_metric_row_any(d_bs, EQUITY_PATTERNS, include_ttm = FALSE))
  
  rev_g <- get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
  roa <- if (!is.na(net) && !is.na(assets) && assets != 0) net / assets * 100 else NA
  roe <- if (!is.na(net) && !is.na(equity) && equity != 0) net / equity * 100 else NA
  fcf_margin <- if (!is.na(fcf) && !is.na(rev) && rev != 0) fcf / rev * 100 else NA
  gm_pct <- suppressWarnings(as.numeric(gm_m$margin_pct)[1])
  gm_label <- if (identical(as.character(gm_m$mode %||% "")[1], "bank_nii")) {
    "淨利息收入／營收（銀行啟發式）"
  } else {
    "毛利率"
  }
  
  data.frame(
    指標 = c(gm_label, "淨利率", "營收成長率 (年均)", "ROA", "ROE", "FCF 利潤率", "營業現金流 (均)", "自由現金流 (均)"),
  數值 = c(
      pct(gm_pct),
      pct(if (!is.na(net) && !is.na(rev) && rev != 0) net / rev * 100 else NA),
      pct(rev_g),
      pct(roa), pct(roe), pct(fcf_margin),
      num(ocf), num(fcf)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# 報告用：只列 MOS／價差，不作券商 Buy／Reduce 門檻（非 Graham／CFA 約束）
derive_investment_rating <- function(current_price, target_price) {
  cur <- suppressWarnings(as.numeric(current_price))
  tgt <- suppressWarnings(as.numeric(target_price))
  if (length(cur) != 1 || length(tgt) != 1 || is.na(cur) || is.na(tgt) || tgt <= 0) {
    return(list(
      rating = "待評估", rating_en = "NR",
      rating_color = "#6c757d", upside_pct = NA, margin_of_safety = NA
    ))
  }
  upside <- (tgt - cur) / cur * 100
  mos <- (tgt - cur) / tgt * 100
  list(
    rating = "未評等（僅列 MOS／價差）",
    rating_en = "NR",
    rating_color = "#6c757d",
    upside_pct = upside,
    margin_of_safety = mos
  )
}

# 推薦估值方法（v13：分類 → 主／副模型）
derive_valuation_method <- function(d_cf, industry_text = "", d_is = NULL, d_bs = NULL,
                                    industry_choice = NULL) {
  rec <- recommend_valuation_models(
    d_cf, industry_text, d_is = d_is, d_bs = d_bs, industry_choice = industry_choice
  )
  list(method = rec$summary_method, rationale = rec$reason,
       primary = rec$primary, secondary = rec$secondary, company_type = rec$company_type)
}

#' 模型選擇器（v16：含獨立 NAV）
#' @return list with company_type, primary, secondary, ddm/dcf/pb/ri/nav flags,
#'   tags, summary_method, reason, suggest_two_stage, confidence_inputs
recommend_valuation_models <- function(d_cf, industry_text = "", d_is = NULL, d_bs = NULL,
                                       industry_choice = NULL,
                                       wacc_pct = NA_real_,
                                       lifecycle_result = NULL,
                                       selected_stage = "auto",
                                       expected_rev_cagr_pct = NA_real_,
                                       locale = "zh-TW") {
  if (is.null(d_cf) || !is.data.frame(d_cf) || nrow(d_cf) == 0) {
    return(assemble_model_recommendation(data_missing = TRUE))
  }

  rec_fcff <- tryCatch(
    reconstruct_hist_fcff(d_cf, d_is = d_is),
    error = function(e) NULL
  )
  fcf_seq <- if (!is.null(rec_fcff)) rec_fcff$fcff else NULL
  if (is.null(fcf_seq) || !length(fcf_seq) || all(!is.finite(fcf_seq))) {
    fcf_seq <- tryCatch(
      select_clean_metric_row(d_cf, "Free Cash Flow", include_ttm = FALSE),
      error = function(e) NULL
    )
  }
  div_seq <- tryCatch(
    select_clean_metric_row(d_cf, "Cash Dividends Paid", include_ttm = FALSE),
    error = function(e) NULL
  )
  is_fcf_pos <- length(fcf_seq) > 0 && !all(is.na(fcf_seq)) &&
    isTRUE(mean(fcf_seq, na.rm = TRUE) > 0)
  fcf_vals <- suppressWarnings(as.numeric(na.omit(fcf_seq)))
  fcf_cv <- if (length(fcf_vals) >= 2) {
    stats::sd(fcf_vals) / max(abs(mean(fcf_vals)), 1e-9)
  } else {
    NA_real_
  }
  is_fcf_stable <- isTRUE(is_fcf_pos) && (is.na(fcf_cv) || fcf_cv <= 0.75)

  div_current <- tryCatch(
    select_current_metric(d_cf, "Cash Dividends Paid", "flow"),
    error = function(e) NA_real_
  )
  div_assess <- if (exists("ynow_assess_dividends", mode = "function")) {
    ynow_assess_dividends(
      div_seq,
      div_current = div_current,
      div_cv_max = 0.50,
      min_years = 2L
    )
  } else {
    div_vals <- abs(suppressWarnings(as.numeric(na.omit(div_seq))))
    div_vals <- div_vals[is.finite(div_vals) & div_vals > 0]
    list(
      is_div = length(div_vals) > 0L,
      is_div_stable = FALSE,
      div_cv = NA_real_,
      has_current_div = is.finite(div_current) && abs(div_current) > 0
    )
  }
  is_div <- isTRUE(div_assess$is_div)
  div_cv <- div_assess$div_cv
  is_div_stable <- isTRUE(div_assess$is_div_stable)

  ind_txt <- as.character(industry_text %||% "")
  ind_key <- as.character(industry_choice %||% "")

  rev_g <- tryCatch(
    get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE)),
    error = function(e) NA_real_
  )
  ni <- tryCatch(common_net_income(d_is), error = function(e) NA_real_)
  equity <- tryCatch(common_stock_equity(d_bs), error = function(e) NA_real_)
  roe <- if (!is.na(ni) && !is.na(equity) && equity > 0) ni / equity * 100 else NA_real_
  last_fcff <- NA_real_
  if (length(fcf_seq)) {
    hit <- which(is.finite(suppressWarnings(as.numeric(fcf_seq))))
    if (length(hit)) last_fcff <- suppressWarnings(as.numeric(fcf_seq)[hit[1]])
  }

  conf_in <- list(
    fcf_cv = fcf_cv,
    div_cv = div_cv,
    has_fcf = isTRUE(is_fcf_pos),
    has_div = isTRUE(is_div),
    has_roe = isTRUE(is.finite(roe)),
    data_complete = isTRUE(!is.null(d_is) && !is.null(d_bs) && isTRUE(is_fcf_pos || is_div))
  )

  goodwill <- tryCatch(select_current_metric(d_bs, "^Goodwill$", "stock"), error = function(e) NA_real_)
  intang <- tryCatch(
    select_current_metric(d_bs, "Goodwill And Other Intangible Assets|Other Intangible Assets", "stock"),
    error = function(e) NA_real_
  )
  assets <- tryCatch(select_current_metric(d_bs, "^Total Assets$", "stock"), error = function(e) NA_real_)
  rd_exp <- tryCatch(
    select_current_metric(d_is, "Research And Development|Research & Development", "flow"),
    error = function(e) NA_real_
  )
  revenue_now <- tryCatch(
    select_current_metric(d_is, "^Total Revenue$", "flow"),
    error = function(e) NA_real_
  )
  ebit_now <- tryCatch(
    select_current_metric(d_is, "Operating Income|^EBIT$", "flow"),
    error = function(e) NA_real_
  )
  ebitda_now <- tryCatch(
    select_current_metric(d_is, "^EBITDA$|Normalized EBITDA", "flow"),
    error = function(e) NA_real_
  )
  da_now <- tryCatch(
    select_current_metric(d_cf, "Depreciation And Amortization|Depreciation & Amortization|Reconciled Depreciation", "flow"),
    error = function(e) NA_real_
  )
  debt_now <- tryCatch(
    select_current_metric(d_bs, "^Total Debt$|Long Term Debt", "stock"),
    error = function(e) NA_real_
  )
  n_fcff <- sum(is.finite(suppressWarnings(as.numeric(fcf_seq))))
  if (is.null(lifecycle_result) && exists("classify_lifecycle_result", mode = "function")) {
    lifecycle_result <- classify_lifecycle_result(
      industry_text = ind_txt,
      industry_key = ind_key,
      d_is = d_is,
      d_bs = d_bs,
      d_cf = d_cf,
      wacc_pct = wacc_pct,
      expected_rev_cagr_pct = if (is.finite(expected_rev_cagr_pct)) expected_rev_cagr_pct else rev_g,
      selected_stage = selected_stage,
      locale = locale
    )
  }
  assemble_model_recommendation(
    data_missing = FALSE,
    conf_in = conf_in,
    is_fcf_pos = is_fcf_pos,
    is_fcf_stable = is_fcf_stable,
    fcf_cv = fcf_cv,
    is_div_stable = is_div_stable,
    ind_txt = ind_txt,
    ind_key = ind_key,
    rev_g = rev_g,
    ni = ni,
    equity = equity,
    roe = roe,
    last_fcff = last_fcff,
    fcff_source = if (!is.null(rec_fcff)) rec_fcff$source else "unknown",
    n_fcff = n_fcff,
    goodwill = goodwill,
    intangibles = intang,
    assets = assets,
    rd_expense = rd_exp,
    revenue = revenue_now,
    ebitda = ebitda_now,
    ebit = ebit_now,
    da = da_now,
    debt = debt_now,
    lifecycle_result = lifecycle_result
  )
}

#' Justified / industry / history P/B targets (v13+)
#' @param roe_pct ROE in percent (e.g. 15)
#' @param ke_pct Cost of equity in percent
#' @param g_pct Perpetual growth in percent (SGR / terminal g; Justified only)
#' @param industry_band list(low, mid, high) or numeric length 2–3
#' @param hist_pb numeric vector of trailing P/B observations (optional)
#' @param include_justified if FALSE, skip Justified (ROE−g)/(Ke−g) — no SGR needed
#' @param include_industry if FALSE, ignore industry_band even when provided
#' @param include_history if FALSE, ignore hist_pb
derive_pb_targets <- function(roe_pct = NA_real_, ke_pct = NA_real_, g_pct = NA_real_,
                              industry_band = NULL, hist_pb = NULL,
                              clamp = c(0.3, 6),
                              include_justified = TRUE,
                              include_industry = TRUE,
                              include_history = TRUE,
                              book_equity = NA_real_,
                              industry_key_valid = TRUE,
                              peer_n = NA_real_,
                              locale = "zh-TW") {
  # `clamp` is accepted for compatibility and is not applied.
  num <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x)) NA_real_ else x
  }
  en <- identical(normalize_ui_locale_safe(locale), "en")
  diags <- list()
  eq <- num(book_equity)
  blocked <- is.finite(eq) && eq <= 0
  if (blocked) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_neg_equity", "equity", current_value = eq,
      expected_rule = "Negative common equity does not support an ordinary P/B",
      impact = "No blended P/B fair-value multiple",
      auto_corrected = FALSE, adopted_value = NA
    )
  }

  ind_lo <- ind_mid <- ind_hi <- NA_real_
  industry_usable <- isTRUE(include_industry) && isTRUE(industry_key_valid) && !blocked
  peers <- num(peer_n)
  if (is.finite(peers) && peers < 3) {
    industry_usable <- FALSE
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_thin_peers", "peer_n", current_value = peers,
      expected_rule = "Fewer than 3 peers cannot support a high-confidence industry P/B",
      impact = "Industry multiple is shown separately and excluded from the blend",
      auto_corrected = FALSE, adopted_value = NA
    )
  }
  if (!isTRUE(industry_key_valid) && isTRUE(include_industry)) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_industry_pb_blocked", "industry_key",
      current_value = NA,
      expected_rule = "Industry P/B requires a validated taxonomy key",
      impact = "Passed industry band is ignored",
      auto_corrected = FALSE, adopted_value = NA
    )
  }
  if (isTRUE(industry_usable) && !is.null(industry_band)) {
    if (is.list(industry_band)) {
      ind_lo <- num(industry_band$low %||% industry_band[[1]])
      ind_hi <- num(industry_band$high %||% industry_band[[2]])
      ind_mid <- num(industry_band$mid %||% (if (length(industry_band) >= 3) industry_band[[3]] else mean(c(ind_lo, ind_hi))))
    } else {
      b <- suppressWarnings(as.numeric(industry_band))
      if (length(b) >= 2) {
        ind_lo <- b[1]; ind_hi <- b[2]
        ind_mid <- if (length(b) >= 3 && is.finite(b[3])) b[3] else mean(c(ind_lo, ind_hi))
      }
    }
  }

  just <- NA_real_
  just_usable <- FALSE
  if (isTRUE(include_justified) && !blocked) {
    roe <- num(roe_pct) / 100
    ke <- num(ke_pct) / 100
    g <- num(g_pct) / 100
    if (is.finite(ke) && is.finite(g) && ke <= g) {
      diags[[length(diags) + 1L]] <- valuation_diagnostic(
        "fatal", "val_diag_wacc_gt_g", "Ke-g", current_value = ke - g,
        expected_rule = "Justified P/B requires Ke > g",
        impact = "Justified multiple is NA",
        auto_corrected = FALSE, adopted_value = NA
      )
    } else if (is.finite(roe) && is.finite(ke) && is.finite(g)) {
      just <- (roe - g) / (ke - g)
      just_usable <- is.finite(just) && just > 0 && roe >= 0
      if (!just_usable) {
        diags[[length(diags) + 1L]] <- valuation_diagnostic(
          "material", "val_diag_justified_pb", "justified_pb", current_value = just,
          expected_rule = "Negative ROE or a non-positive justified P/B is disclosed, not clipped to a market-like multiple",
          impact = "Justified P/B is excluded from the blend",
          auto_corrected = FALSE, adopted_value = just
        )
      }
    }
  }

  hist_lo <- hist_mid <- hist_hi <- NA_real_
  if (isTRUE(include_history) && !blocked) {
    hp <- suppressWarnings(as.numeric(hist_pb))
    hp <- hp[is.finite(hp) & hp > 0]
    if (length(hp) >= 4) {
      qs <- stats::quantile(hp, probs = c(0.25, 0.50, 0.75), names = FALSE, na.rm = TRUE)
      hist_lo <- qs[1]; hist_mid <- qs[2]; hist_hi <- qs[3]
    }
  }

  sources <- list()
  quality <- c()
  if (isTRUE(just_usable)) {
    sources$justified <- just
    quality <- c(quality, justified = 0.70)
  }
  if (is.finite(ind_mid)) {
    sources$industry <- ind_mid
    q_ind <- if (is.finite(peers) && peers >= 8) 0.80 else 0.50
    quality <- c(quality, industry = q_ind)
  }
  if (is.finite(hist_mid)) {
    sources$history <- hist_mid
    quality <- c(quality, history = 0.40)
  }

  note_parts <- character(0)
  blended <- FALSE
  base <- NA_real_
  ww <- numeric(0)
  if (!length(sources)) {
    note_parts <- if (en) {
      "No usable justified, industry, or history P/B. No generic multiple is substituted."
    } else {
      "沒有可用的 Justified、產業或歷史 P/B，不代入通用倍數。"
    }
  } else if (length(sources) == 1L) {
    base <- unname(unlist(sources)[1])
    ww <- stats::setNames(1, names(sources))
    blended <- FALSE
    note_parts <- if (en) {
      paste0("Single source ", names(sources)[1], " = ", sprintf("%.2f", base), ". Not a blend.")
    } else {
      paste0("單一來源 ", names(sources)[1], "＝", sprintf("%.2f", base), "，未與其他方法平均。")
    }
  } else {
    vals <- unlist(sources)
    rel <- (max(vals) - min(vals)) / max(abs(vals))
    if (is.finite(rel) && rel > 0.50) {
      note_parts <- if (en) {
        "Justified and industry or history P/B differ by more than half the larger multiple. They stay separate and are not averaged."
      } else {
        "Justified 與產業或歷史 P/B 差距過大，分開列示，不平均。"
      }
      diags[[length(diags) + 1L]] <- valuation_diagnostic(
        "material", "val_diag_pb_gap", "pb_sources", current_value = rel,
        expected_rule = "Do not average P/B methods when the relative gap exceeds 0.50",
        impact = "Blended mid is NA", auto_corrected = FALSE, adopted_value = NA
      )
    } else {
      ww <- quality[names(sources)]
      ww <- ww / sum(ww)
      base <- sum(vals * ww[names(vals)])
      blended <- TRUE
      note_parts <- if (en) {
        paste0("Blend weights follow source quality: ", paste(sprintf("%s=%.0f%%", names(ww), ww * 100), collapse = ", "), ".")
      } else {
        paste0("權重依來源品質：", paste(sprintf("%s=%.0f%%", names(ww), ww * 100), collapse = ", "), "。")
      }
    }
  }

  lows <- c(if (just_usable) just else NA_real_, ind_lo, hist_lo)
  highs <- c(if (just_usable) just else NA_real_, ind_hi, hist_hi)
  lows <- lows[is.finite(lows)]
  highs <- highs[is.finite(highs)]
  bear <- if (is.finite(base) && length(lows)) min(c(base, lows)) else if (length(lows) && length(sources) == 1L) min(lows) else NA_real_
  bull <- if (is.finite(base) && length(highs)) max(c(base, highs)) else if (length(highs) && length(sources) == 1L) max(highs) else NA_real_
  if (is.finite(base)) {
    if (is.finite(bear)) bear <- min(bear, base)
    if (is.finite(bull)) bull <- max(bull, base)
  }

  list(
    low = if (blocked) NA_real_ else bear,
    mid = if (blocked) NA_real_ else base,
    high = if (blocked) NA_real_ else bull,
    justified = just,
    justified_usable = just_usable,
    industry_low = ind_lo, industry_mid = ind_mid, industry_high = ind_hi,
    industry_usable = is.finite(ind_mid),
    history_low = hist_lo, history_mid = hist_mid, history_high = hist_hi,
    weights_used = if (!length(ww)) character(0) else paste(sprintf("%s=%.0f%%", names(ww), ww * 100), collapse = ", "),
    source_note = paste(note_parts, collapse = " "),
    blended = blended,
    clamp_applied = FALSE,
    pb_blocked = blocked,
    diagnostics = diags
  )
}

normalize_ui_locale_safe <- function(locale) {
  if (exists("normalize_ui_locale", mode = "function")) return(normalize_ui_locale(locale))
  loc <- tolower(trimws(as.character(locale %||% "en")[1]))
  if (loc %in% c("zh", "zh-tw", "zhtw", "tw", "taiwan")) return("zh-TW")
  "en"
}

#' Valuation confidence label from fundamentals quality signals (v13)
#' Pick model-recommendation reason text for the active UI locale.
#' Prefer reason_en when locale is en; fall back to reason (zh) if EN missing.
#' @param rec list with reason / reason_en (from recommend_valuation_models / empties)
#' @param locale "en" / "zh-TW" (or aliases normalized via normalize_ui_locale_safe)
#' @return character(1)
.pick_rec_reason <- function(rec, locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale_safe", mode = "function")) {
    normalize_ui_locale_safe(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  zh <- as.character(rec$reason %||% "")[1]
  en <- as.character(rec$reason_en %||% "")[1]
  if (identical(loc, "en") && nzchar(en)) return(en)
  if (nzchar(zh)) return(zh)
  en
}

#' @return list(level, score, reasons) — level/reasons follow locale (en-US / zh-TW)
score_valuation_confidence <- function(confidence_inputs = list(),
                                       f_score = NA_real_,
                                       primary_base = NA_real_,
                                       secondary_point = NA_real_,
                                       tv_weight = NA_real_,
                                       locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale_safe", mode = "function")) {
    normalize_ui_locale_safe(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  .cs <- function(key, fallback) {
    if (exists("ui_str", mode = "function")) {
      out <- tryCatch(ui_str(key, loc), error = function(e) NULL)
      if (is.character(out) && nzchar(out[1])) return(as.character(out)[1])
    }
    fallback
  }
  score <- 40
  reasons <- character(0)
  ci <- confidence_inputs %||% list()

  if (isTRUE(ci$data_complete)) {
    score <- score + 15
    reasons <- c(reasons, .cs("conf_fs_complete", "財報輸入完整"))
  } else {
    reasons <- c(reasons, .cs("conf_fs_incomplete", "財報輸入不完整"))
  }
  if (isTRUE(ci$has_fcf)) score <- score + 8
  if (isTRUE(ci$has_div)) score <- score + 5
  if (isTRUE(ci$has_roe)) score <- score + 5

  fcf_cv <- suppressWarnings(as.numeric(ci$fcf_cv)[1])
  if (is.finite(fcf_cv)) {
    if (fcf_cv <= 0.5) {
      score <- score + 12
      reasons <- c(reasons, .cs("conf_fcf_stable", "FCF 波動可控"))
    } else if (fcf_cv > 1) {
      score <- score - 10
      reasons <- c(reasons, .cs("conf_fcf_volatile", "FCF 波動偏高"))
    }
  }

  fs <- suppressWarnings(as.numeric(f_score)[1])
  if (is.finite(fs)) {
    if (fs >= 7) {
      score <- score + 12
      reasons <- c(reasons, .cs("conf_fscore_strong", "F-Score 偏強（品質檢核）"))
    } else if (fs < 4) {
      score <- score - 12
      reasons <- c(reasons, .cs("conf_fscore_weak", "F-Score 偏弱（品質檢核）"))
    }
  }

  pb <- suppressWarnings(as.numeric(primary_base)[1])
  sp <- suppressWarnings(as.numeric(secondary_point)[1])
  if (is.finite(pb) && is.finite(sp) && pb > 0) {
    gap <- abs(sp - pb) / pb
    if (gap <= 0.15) {
      score <- score + 10
      reasons <- c(reasons, .cs("conf_models_close", "主副模型接近"))
    } else if (gap > 0.35) {
      score <- score - 8
      reasons <- c(reasons, .cs("conf_models_diverge", "主副模型分歧大"))
    }
  }

  tvw <- suppressWarnings(as.numeric(tv_weight)[1])
  if (is.finite(tvw) && tvw > 0.75) {
    score <- score - 10
    reasons <- c(reasons, .cs("conf_tv_weight_high", "終值權重偏高"))
  }

  score <- max(0, min(100, round(score)))
  level <- if (score >= 70) {
    .cs("conf_level_high", "高")
  } else if (score >= 45) {
    .cs("conf_level_mid", "中")
  } else {
    .cs("conf_level_low", "低")
  }
  list(level = level, score = score, reasons = reasons, locale = loc)
}

#' Map model key to display label
.model_label <- function(key) {
  switch(
    as.character(key %||% ""),
    "dcf" = "DCF",
    "ddm" = "DDM",
    "pb" = "Justified P/B",
    "ri" = "RI",
    "nav" = "NAV",
    "multiples" = "Multiples",
    "sotp" = "SOTP",
    as.character(key %||% "N/A")
  )
}

#' Build Bear/Base/Bull scalars for discount models (documented small stresses)
scenario_stress_factors <- function(model = c("dcf", "ddm", "ri")) {
  model <- match.arg(model)
  # deltas in percentage points for rates; near-term growth haircut as multiplier
  switch(
    model,
    dcf = list(
      bear = list(wacc_pp = +1.0, g_pp = -0.5, near_g_mult = 0.85),
      base = list(wacc_pp = 0, g_pp = 0, near_g_mult = 1),
      bull = list(wacc_pp = -1.0, g_pp = +0.5, near_g_mult = 1.10)
    ),
    ddm = list(
      bear = list(ke_pp = +1.0, g_pp = -0.5),
      base = list(ke_pp = 0, g_pp = 0),
      bull = list(ke_pp = -1.0, g_pp = +0.5)
    ),
    ri = list(
      bear = list(ke_pp = +1.0, g_pp = -0.5, roe_pp = -2),
      base = list(ke_pp = 0, g_pp = 0, roe_pp = 0),
      bull = list(ke_pp = -1.0, g_pp = +0.5, roe_pp = +2)
    )
  )
}


# 從 Summary 表萃取單一欄位
extract_summary_item <- function(summary_df, pattern, default = "N/A") {
  if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) == 0) return(default)
  idx <- grep(pattern, summary_df$Item, ignore.case = TRUE)
  if (length(idx) == 0) return(default)
  val <- summary_df$Value[idx[1]]
  if (is.na(val) || val == "") default else as.character(val)
}

# 附錄財報表格裁切（左側 TTM / 最新期優先）
trim_report_table <- function(df, max_rows = 18, max_cols = 7) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  df <- coerce_financial_df(df)
  df <- reorder_financial_columns(df)
  if (ncol(df) > max_cols) df <- df[, seq_len(max_cols), drop = FALSE]
  if (nrow(df) > max_rows) df <- df[seq_len(max_rows), , drop = FALSE]
  df
}

# 報告用：NULL / 空字串 → NA_real_
.report_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  n <- suppressWarnings(as.numeric(x[[1]]))
  if (length(n) != 1 || !is.finite(n)) NA_real_ else n
}

# A report uses the same Fair Value primary as the screen. Never substitute an
# unrelated DCF price when the primary is unavailable or only an implied price.
.report_primary_target <- function(primary, band = NULL, model_point = NULL) {
  key <- as.character(primary %||% "")[1]
  if (!key %in% c("dcf", "ddm", "pb", "ri", "nav")) return(NA_real_)
  base <- .report_num(if (is.list(band)) band$base else NULL)
  if (is.finite(base)) return(base)
  if (!is.function(model_point)) return(NA_real_)
  .report_num(tryCatch(model_point(key), error = function(e) NA_real_))
}

# FCF is optional: thin statements and Shiny req() cancellations must not abort
# a P/B, RI, NAV or Lite report. Evaluate the reactive once and isolate graphics.
.report_save_fcf_plot <- function(plot_fn, path, enabled = TRUE) {
  if (!isTRUE(enabled) || !is.function(plot_fn)) return(NA_character_)
  tryCatch({
    p <- plot_fn()
    if (is.null(p)) return(NA_character_)
    ggplot2::ggsave(path, plot = p, width = 9, height = 5.5, dpi = 200)
    path
  }, error = function(e) {
    unlink(path)
    NA_character_
  })
}

# DCF 模式顯示名稱
.report_dcf_mode_label <- function(mode, locale = "zh-TW") {
  m <- if (is.null(mode) || length(mode) == 0 || is.na(mode[1])) "" else as.character(mode[1])
  en <- identical(as.character(locale)[1], "en")
  if (identical(m, "gordon")) {
    return(if (en) "Explicit forecast + Gordon Terminal Value" else "明確預測期 + Gordon 終值")
  }
  if (identical(m, "two_stage")) {
    return(if (en) "Two-Stage DCF" else "兩階段成長（Two-Stage DCF）")
  }
  if (!nzchar(m)) return("N/A")
  m
}

# 報告用：自財報推估每股 EPS／每股淨值（BVPS）
.report_eps_bvps <- function(summary_df, d_is = NULL, d_bs = NULL) {
  eps <- valuation_trailing_eps(summary_df, d_is)
  bvps <- NA_real_
  bv_src <- extract_summary_item(
    summary_df, "Book Value Per Share|Book Value/Share|每股淨值", default = NA_character_
  )
  if (!is.na(bv_src) && !identical(bv_src, "N/A")) {
    bvps <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(bv_src))))
  }
  if (!is.finite(bvps) && !is.null(d_bs) && is.data.frame(d_bs) && nrow(d_bs) > 0) {
    eq <- tryCatch(
      common_stock_equity(d_bs),
      error = function(e) NA_real_
    )
    sh <- tryCatch(
      select_current_metric(
        d_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock"
      ),
      error = function(e) NA_real_
    )
    if (is.finite(eq) && is.finite(sh) && sh > 0) bvps <- eq / sh
  }
  list(eps = .report_num(eps), bvps = .report_num(bvps))
}

# CapEx／營收比與歷史均值（工程啟發式填表，非學術標準）
.report_capex_revenue_ratio <- function(d_is, d_cf, lookback = 5L) {
  empty <- list(ratio_pct = NA_real_, avg_pct = NA_real_, n_years = 0L)
  if (is.null(d_is) || is.null(d_cf) || !is.data.frame(d_is) || !is.data.frame(d_cf)) {
    return(empty)
  }
  rev <- tryCatch(
    select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE),
    error = function(e) NULL
  )
  capex <- tryCatch(
    select_clean_metric_row(
      d_cf, "Capital Expenditure|Purchase Of PPE|Net PPE Purchase And Sale",
      include_ttm = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(rev) || is.null(capex) || length(rev) < 1L || length(capex) < 1L) {
    return(empty)
  }
  n <- min(as.integer(lookback), length(rev), length(capex))
  if (n < 1L) return(empty)
  ratios <- abs(suppressWarnings(as.numeric(capex[seq_len(n)]))) /
    abs(suppressWarnings(as.numeric(rev[seq_len(n)])))
  ratios <- ratios[is.finite(ratios) & ratios > 0]
  if (length(ratios) < 1L) return(empty)
  list(
    ratio_pct = ratios[1] * 100,
    avg_pct = mean(ratios) * 100,
    n_years = length(ratios)
  )
}

# 敏感度矩陣 → 可 kable 的 data.frame（列名保留）
.report_sensitivity_df <- function(mat) {
  if (is.null(mat) || !is.matrix(mat) || length(mat) < 1L) return(NULL)
  df <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
  rn <- rownames(mat)
  if (!is.null(rn) && length(rn) == nrow(df)) {
    df <- cbind(` ` = rn, df, stringsAsFactors = FALSE)
  }
  for (j in seq_along(df)) {
    if (is.numeric(df[[j]])) df[[j]] <- round(df[[j]], 2)
  }
  df
}

#' 組裝個股 PDF 報告公式欄與章節文案（僅標的本身；不含同業排名／Lab 宇宙）
build_ticker_report_copy <- function(
    locale = "zh-TW",
    stock_code = NA,
    company_name = NA,
    sector = NA,
    industry = NA,
    current_price = NA,
    target_price = NA,
    primary_method = NA,
    method_rationale = NA,
    margin_of_safety = NA,
    upside_pct = NA,
    dcf_price = NA,
    ddm_value = NA,
    pb_value = NA,
    ri_value = NA,
    primary_bear = NA,
    primary_base = NA,
    primary_bull = NA,
    secondary_point = NA,
    secondary_label = NA,
    confidence_level = NA,
    confidence_score = NA,
    wacc = NA,
    terminal_growth = NA,
    forecast_years = NA,
    dcf_mode = NA,
    eps = NA,
    bvps = NA,
    pe_ratio = NA,
    market_cap = NA,
    beta = NA,
    dividend_yield = NA,
    roe_pct = NA,
    rev_growth_pct = NA,
    capex_rev_pct = NA,
    capex_avg_pct = NA,
    capex_n_years = NA,
    fscore_total = NA,
    money_prefix = "$",
    market_mode = "US",
    report_condensed = FALSE
) {
  en <- identical(as.character(locale)[1], "en")
  mm <- toupper(trimws(as.character(market_mode)[1]))
  if (!mm %in% c("US", "TW")) mm <- "US"
  condensed <- isTRUE(report_condensed)
  px <- function(x) {
    n <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(n)) return("N/A")
    paste0(money_prefix, format(round(n, 2), nsmall = 2, big.mark = ","))
  }
  pct <- function(x, signed = FALSE) {
    n <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(n)) return("N/A")
    paste0(if (signed && n > 0) "+" else "", sprintf("%.1f", n), "%")
  }
  sc <- function(x, default = "N/A") {
    if (is.null(x) || length(x) < 1L || (length(x) == 1L && is.na(x))) return(default)
    s <- trimws(as.character(x[[1]]))
    if (!nzchar(s) || identical(s, "NA")) default else s
  }

  tgt <- suppressWarnings(as.numeric(target_price)[1])
  cur <- suppressWarnings(as.numeric(current_price)[1])
  bv <- suppressWarnings(as.numeric(bvps)[1])
  ep <- suppressWarnings(as.numeric(eps)[1])
  implied_pb <- if (is.finite(tgt) && is.finite(bv) && bv > 0) tgt / bv else NA_real_
  implied_pe <- if (is.finite(tgt) && is.finite(ep) && ep != 0) tgt / ep else NA_real_
  spot_pb <- if (is.finite(cur) && is.finite(bv) && bv > 0) cur / bv else NA_real_
  spot_pe <- if (is.finite(cur) && is.finite(ep) && ep != 0) cur / ep else NA_real_

  if (en) {
    target_formula <- sprintf(
      "Target (primary Base) = %s  via %s",
      px(tgt), sc(primary_method)
    )
    if (is.finite(implied_pb)) {
      target_formula <- paste0(
        target_formula,
        sprintf("  ·  implied P/B ≈ %.2f× on BVPS %s", implied_pb, px(bv))
      )
    }
    if (is.finite(implied_pe)) {
      target_formula <- paste0(
        target_formula,
        sprintf("  ·  implied P/E ≈ %.2f× on EPS %s", implied_pe, px(ep))
      )
    }
    ref_formula <- sprintf("Reference (last) = %s", px(cur))
    if (is.finite(spot_pb)) {
      ref_formula <- paste0(ref_formula, sprintf("  ·  %.2f× BVPS", spot_pb))
    }
    if (is.finite(spot_pe)) {
      ref_formula <- paste0(ref_formula, sprintf("  ·  %.2f× EPS", spot_pe))
    }
    mos_formula <- sprintf(
      "MOS = (FV − Price) / FV = (%s − %s) / %s = %s",
      px(tgt), px(cur), px(tgt), pct(margin_of_safety)
    )
    upside_formula <- sprintf(
      "Upside = (FV − Price) / Price = %s",
      pct(upside_pct, signed = TRUE)
    )
  } else {
    target_formula <- sprintf(
      "目標價（主模型 Base）＝ %s｜方法：%s",
      px(tgt), sc(primary_method)
    )
    if (is.finite(implied_pb)) {
      target_formula <- paste0(
        target_formula,
        sprintf("｜隱含 P/B ≈ %.2f×（BVPS %s）", implied_pb, px(bv))
      )
    }
    if (is.finite(implied_pe)) {
      target_formula <- paste0(
        target_formula,
        sprintf("｜隱含 P/E ≈ %.2f×（EPS %s）", implied_pe, px(ep))
      )
    }
    ref_formula <- sprintf("參考價（現價）＝ %s", px(cur))
    if (is.finite(spot_pb)) {
      ref_formula <- paste0(ref_formula, sprintf("｜%.2f× BVPS", spot_pb))
    }
    if (is.finite(spot_pe)) {
      ref_formula <- paste0(ref_formula, sprintf("｜%.2f× EPS", spot_pe))
    }
    mos_formula <- sprintf(
      "安全邊際 MOS＝(FV−Price)/FV＝(%s−%s)/%s＝%s",
      px(tgt), px(cur), px(tgt), pct(margin_of_safety)
    )
    upside_formula <- sprintf(
      "潛在報酬＝(FV−Price)/Price＝%s",
      pct(upside_pct, signed = TRUE)
    )
  }

  bullets <- character(0)
  bullets <- c(bullets, mos_formula, upside_formula)
  if (is.finite(suppressWarnings(as.numeric(dcf_price)[1]))) {
    bullets <- c(bullets, if (en) {
      sprintf("DCF fair value / share = %s (WACC %s; terminal g %s; %s; horizon %s yrs).",
              px(dcf_price), sc(wacc), sc(terminal_growth), sc(dcf_mode), sc(forecast_years))
    } else {
      sprintf("DCF 每股合理價＝%s（WACC %s；終值 g %s；%s；預測 %s 年）。",
              px(dcf_price), sc(wacc), sc(terminal_growth), sc(dcf_mode), sc(forecast_years))
    })
  }
  if (is.finite(suppressWarnings(as.numeric(ddm_value)[1]))) {
    bullets <- c(bullets, if (en) sprintf("DDM fair value / share = %s.", px(ddm_value))
                 else sprintf("DDM 每股合理價＝%s。", px(ddm_value)))
  }
  if (is.finite(suppressWarnings(as.numeric(ri_value)[1]))) {
    bullets <- c(bullets, if (en) sprintf("RI fair value / share = %s.", px(ri_value))
                 else sprintf("RI 每股合理價＝%s。", px(ri_value)))
  }
  if (is.finite(suppressWarnings(as.numeric(pb_value)[1]))) {
    bullets <- c(bullets, if (en) sprintf("P/B fair value / share = %s.", px(pb_value))
                 else sprintf("P/B 每股合理價＝%s。", px(pb_value)))
  }
  bear <- suppressWarnings(as.numeric(primary_bear)[1])
  base <- suppressWarnings(as.numeric(primary_base)[1])
  bull <- suppressWarnings(as.numeric(primary_bull)[1])
  if (is.finite(bear) && is.finite(base) && is.finite(bull)) {
    bullets <- c(bullets, if (en) {
      sprintf("Primary scenario band Bear / Base / Bull = %s / %s / %s.",
              px(bear), px(base), px(bull))
    } else {
      sprintf("主模型情境區間 Bear／Base／Bull＝%s／%s／%s。",
              px(bear), px(base), px(bull))
    })
  }
  sec <- suppressWarnings(as.numeric(secondary_point)[1])
  if (is.finite(sec)) {
    bullets <- c(bullets, if (en) {
      sprintf("Secondary check (%s) = %s.", sc(secondary_label, "secondary"), px(sec))
    } else {
      sprintf("副模型檢核（%s）＝%s。", sc(secondary_label, "副模型"), px(sec))
    })
  }
  conf_lv <- sc(confidence_level, "")
  if (nzchar(conf_lv) && !identical(conf_lv, "N/A")) {
    sc_n <- suppressWarnings(as.numeric(confidence_score)[1])
    bullets <- c(bullets, if (en) {
      if (is.finite(sc_n)) sprintf("Valuation confidence = %s (%s).", conf_lv, sc_n)
      else sprintf("Valuation confidence = %s.", conf_lv)
    } else {
      if (is.finite(sc_n)) sprintf("估值可信度＝%s（%s）。", conf_lv, sc_n)
      else sprintf("估值可信度＝%s。", conf_lv)
    })
  }
  rat <- sc(method_rationale, "")
  if (nzchar(rat) && !identical(rat, "N/A")) {
    bullets <- c(bullets, rat)
  }
  fs <- suppressWarnings(as.numeric(fscore_total)[1])
  if (is.finite(fs)) {
    bullets <- c(bullets, if (en) {
      sprintf("Piotroski F-Score = %d / 9 (quality screen only; not a buy signal).", as.integer(fs))
    } else {
      sprintf("Piotroski F-Score＝%d／9（個股品質檢核；非買進訊號）。", as.integer(fs))
    })
  }

  growth_bullets <- character(0)
  if (is.finite(suppressWarnings(as.numeric(roe_pct)[1]))) {
    growth_bullets <- c(growth_bullets, if (en) {
      sprintf("ROE (recent avg) ≈ %s.", pct(roe_pct))
    } else {
      sprintf("股東權益報酬率 ROE（近期均）≈ %s。", pct(roe_pct))
    })
  }
  if (is.finite(suppressWarnings(as.numeric(rev_growth_pct)[1]))) {
    growth_bullets <- c(growth_bullets, if (en) {
      sprintf("Revenue growth (avg YoY) ≈ %s.", pct(rev_growth_pct, signed = TRUE))
    } else {
      sprintf("營收成長率（年均 YoY）≈ %s。", pct(rev_growth_pct, signed = TRUE))
    })
  }
  if (is.finite(suppressWarnings(as.numeric(capex_avg_pct)[1]))) {
    n_cx <- suppressWarnings(as.integer(capex_n_years)[1])
    n_cx_txt <- if (is.finite(n_cx) && n_cx > 0L) as.character(n_cx) else "—"
    growth_bullets <- c(growth_bullets, if (en) {
      sprintf(
        "CapEx / Revenue ≈ %s (latest) · avg %s over %s yrs (engineering heuristic).",
        pct(capex_rev_pct), pct(capex_avg_pct), n_cx_txt
      )
    } else {
      sprintf(
        "資本支出 CapEx／營收 ≈ %s（最新）· 均值 %s（%s 年；工程啟發式）。",
        pct(capex_rev_pct), pct(capex_avg_pct), n_cx_txt
      )
    })
  }
  growth_bullets <- c(growth_bullets, if (en) {
    sprintf(
      "Discount / Terminal: WACC %s · terminal g %s · mode %s · years %s.",
      sc(wacc), sc(terminal_growth), sc(dcf_mode), sc(forecast_years)
    )
  } else {
    sprintf(
      "折現／終值假設：WACC %s · 永續成長率 g %s · 模式 %s · 預測年數 %s。",
      sc(wacc), sc(terminal_growth), sc(dcf_mode), sc(forecast_years)
    )
  })

  # Broker-style numbered 分析說明 paragraphs (investment view / methodology)
  analysis_paragraphs <- list()
  if (en) {
    analysis_paragraphs <- list(
      list(
        title = "Stance & target",
        body = sprintf(
          paste0(
            "Using primary Base fair value %s via %s versus last price %s, ",
            "MOS is %s and upside is %s. The in-app stance label reflects MOS / price gap only — ",
            "it is not a broker Buy/Hold/Reduce gate and must not be read as a buy signal."
          ),
          px(tgt), sc(primary_method), px(cur), pct(margin_of_safety), pct(upside_pct, signed = TRUE)
        )
      ),
      list(
        title = "Primary methodology",
        body = {
          rat <- sc(method_rationale, "")
          meth <- sprintf(
            "The ticker report anchors on %s (primary). Target price equals primary Base; Bear / Bull bands frame scenario risk.",
            sc(primary_method)
          )
          if (nzchar(rat) && !identical(rat, "N/A")) paste0(meth, " Rationale: ", rat) else meth
        }
      ),
      list(
        title = "Multi-model cross-check",
        body = paste(
          sprintf("DCF / share %s", px(dcf_price)),
          sprintf("DDM / share %s", px(ddm_value)),
          sprintf("RI / share %s", px(ri_value)),
          sprintf("P/B / share %s", px(pb_value)),
          if (is.finite(sec)) sprintf("Secondary check (%s) %s", sc(secondary_label, "secondary"), px(sec)) else NULL,
          if (nzchar(conf_lv) && !identical(conf_lv, "N/A")) {
            sc_n <- suppressWarnings(as.numeric(confidence_score)[1])
            if (is.finite(sc_n)) sprintf("confidence %s (%s)", conf_lv, sc_n) else sprintf("confidence %s", conf_lv)
          } else NULL,
          sep = "; "
        )
      ),
      list(
        title = "Discount, terminal g & CapEx",
        body = paste(
          sprintf(
            "Discount / Terminal assumptions: WACC %s · terminal g %s · mode %s · forecast %s yrs.",
            sc(wacc), sc(terminal_growth), sc(dcf_mode), sc(forecast_years)
          ),
          if (is.finite(suppressWarnings(as.numeric(capex_avg_pct)[1]))) {
            n_cx <- suppressWarnings(as.integer(capex_n_years)[1])
            sprintf(
              " CapEx / Revenue ≈ %s latest · avg %s over %s yrs (engineering heuristic, user-adjustable in-app).",
              pct(capex_rev_pct), pct(capex_avg_pct),
              if (is.finite(n_cx) && n_cx > 0L) as.character(n_cx) else "—"
            )
          } else "",
          sep = ""
        )
      ),
      list(
        title = "Scenario & sensitivity",
        body = {
          if (is.finite(bear) && is.finite(base) && is.finite(bull)) {
            sprintf(
              "Primary scenario band Bear / Base / Bull = %s / %s / %s. Review the WACC × terminal g sensitivity table when a DCF／DDM path is active.",
              px(bear), px(base), px(bull)
            )
          } else {
            "Scenario band incomplete — run the primary valuation path, then re-download for Bear / Base / Bull and sensitivity."
          }
        }
      ),
      list(
        title = "Quality screen & scope",
        body = {
          fs_txt <- if (is.finite(fs)) {
            sprintf("Piotroski F-Score = %d / 9 (quality screen only; never a buy signal). ", as.integer(fs))
          } else ""
          paste0(
            fs_txt,
            "Scope is ticker-only: peer rankings, industry screens, and lab-universe narratives are excluded. ",
            "For research / education only — not investment advice."
          )
        }
      )
    )
  } else {
    analysis_paragraphs <- list(
      list(
        title = "投資立場與目標價",
        body = sprintf(
          paste0(
            "依主模型「%s」Base 公允價值 %s 對照現價 %s，安全邊際 MOS 為 %s、潛在報酬為 %s。",
            "App 內評等標籤僅反映 MOS／價差，不作券商買進／持有／減持門檻，亦非買進訊號。"
          ),
          sc(primary_method), px(tgt), px(cur), pct(margin_of_safety), pct(upside_pct, signed = TRUE)
        )
      ),
      list(
        title = "主模型方法論",
        body = {
          rat <- sc(method_rationale, "")
          meth <- sprintf(
            "本報告以「%s」為主模型錨點；目標價取自主模型 Base，Bear／Bull 區間用於情境風險框架。",
            sc(primary_method)
          )
          if (nzchar(rat) && !identical(rat, "N/A")) paste0(meth, "方法說明：", rat) else meth
        }
      ),
      list(
        title = "多模型交叉檢核",
        body = paste(
          sprintf("DCF 每股合理價 %s", px(dcf_price)),
          sprintf("DDM 每股合理價 %s", px(ddm_value)),
          sprintf("RI 每股合理價 %s", px(ri_value)),
          sprintf("P/B 每股合理價 %s", px(pb_value)),
          if (is.finite(sec)) sprintf("副模型檢核（%s）%s", sc(secondary_label, "副模型"), px(sec)) else NULL,
          if (nzchar(conf_lv) && !identical(conf_lv, "N/A")) {
            sc_n <- suppressWarnings(as.numeric(confidence_score)[1])
            if (is.finite(sc_n)) sprintf("估值可信度 %s（%s）", conf_lv, sc_n) else sprintf("估值可信度 %s", conf_lv)
          } else NULL,
          sep = "；"
        )
      ),
      list(
        title = "折現、終值 g 與 CapEx",
        body = paste(
          sprintf(
            "折現／終值假設：WACC %s · 永續成長率 g %s · 模式 %s · 預測 %s 年。",
            sc(wacc), sc(terminal_growth), sc(dcf_mode), sc(forecast_years)
          ),
          if (is.finite(suppressWarnings(as.numeric(capex_avg_pct)[1]))) {
            n_cx <- suppressWarnings(as.integer(capex_n_years)[1])
            sprintf(
              "資本支出 CapEx／營收 ≈ %s（最新）· 均值 %s（%s 年；工程啟發式，可於 App 調整）。",
              pct(capex_rev_pct), pct(capex_avg_pct),
              if (is.finite(n_cx) && n_cx > 0L) as.character(n_cx) else "—"
            )
          } else "",
          sep = ""
        )
      ),
      list(
        title = "情境區間與敏感度",
        body = {
          if (is.finite(bear) && is.finite(base) && is.finite(bull)) {
            sprintf(
              "主模型情境區間 Bear／Base／Bull＝%s／%s／%s。若已跑出 DCF／DDM 路徑，請一併參閱 WACC × 終值 g 敏感度表。",
              px(bear), px(base), px(bull)
            )
          } else {
            "情境區間尚未齊備——請先完成主模型試算後再下載，以帶出 Bear／Base／Bull 與敏感度。"
          }
        }
      ),
      list(
        title = "品質檢核與報告範圍",
        body = {
          fs_txt <- if (is.finite(fs)) {
            sprintf("Piotroski F-Score＝%d／9（僅作品質檢核閘門，絕非買進訊號）。", as.integer(fs))
          } else ""
          paste0(
            fs_txt,
            "本報告範圍僅限搜尋標的之個股分析：已排除同業排名、產業宇宙篩選與 Lab 敘事。",
            "僅供研究／教育參考，不構成投資建議或要約。"
          )
        }
      )
    )
  }

  titles <- if (en) {
    list(
      doc_kicker = if (identical(mm, "TW")) "TW Company Report" else "US Company Report",
      doc_title = if (condensed) "Ticker Investment Report (Lite)" else "Ticker Investment Report",
      rating_label = "Stance (MOS / gap only)",
      target_label = "Target price",
      ref_label = "Reference price",
      section1 = "I. Investment view",
      section2 = "II. Company snapshot",
      section3 = "III. Earnings & growth drivers",
      section4 = "IV. Scenario & risk",
      appendix = "Appendix — Financial statements (ticker)",
      company_intro = "Company profile",
      ops_outlook = "Operating & valuation snapshot",
      growth_est = "Growth / CapEx / discount assumptions",
      scenario = if (condensed) "Bear / Base / Bull" else "Bear / Base / Bull & sensitivity",
      fscore = "F-Score quality screen",
      warnings = "Financial-statement alerts",
      analysis_heading = "Analysis notes (numbered)",
      lite_appendix_note = "Lite condensed PDF omits statement extracts — open Full mode for the appendix.",
      lite_fcf_note = "Lite omits the FCFF path chart — open Full mode for the plot.",
      lite_sens_note = "Lite omits the WACC × g sensitivity matrix — open Full mode for the grid.",
      disclaimer = paste0(
        "This report is auto-generated by The YNow App for research and education only. ",
        "It is not investment advice. You alone bear all investment decisions and losses. ",
        "Ratings show MOS / price gap only (not broker Buy/Sell gates). ",
        "Peer rankings, industry screens, and lab-universe narratives are excluded — ticker-only analysis. ",
        "See the in-app About page for privacy and intellectual-property notices."
      ),
      no_kpi = "Insufficient financials for KPI table. Search a ticker first.",
      no_fcf = "No FCFF path chart yet — run DCF on the valuation tab, then download again.",
      no_sens = "No WACC × g sensitivity matrix yet (needs an active DCF / DDM path).",
      no_warn = "No material statement alerts detected (still review footnotes / news).",
      warn_title = "Detected statement alerts:",
      basic_info = "Basic information",
      multi_model = "Multi-model fair values"
    )
  } else {
    list(
      doc_kicker = if (identical(mm, "TW")) "台股個股報告" else "美股個股報告",
      doc_title = if (condensed) "投資意見報告（Lite 精簡）" else "投資意見報告",
      rating_label = "投資立場（僅列 MOS／價差）",
      target_label = "目標價位",
      ref_label = "參考價位",
      section1 = "壹、投資建議",
      section2 = "貳、公司概況",
      section3 = "參、獲利及成長動能預估",
      section4 = "肆、財務情境評估",
      appendix = "附錄：財務簡表（個股）",
      company_intro = "一、公司簡介",
      ops_outlook = "二、營運與評價快覽",
      growth_est = "一、成長／CapEx／折現假設",
      scenario = if (condensed) "一、Bear／Base／Bull" else "一、Bear／Base／Bull 與敏感度",
      fscore = "二、F-Score 品質檢核",
      warnings = "三、財報警訊",
      analysis_heading = "分析說明（編號段落）",
      lite_appendix_note = "Lite 精簡版省略財報附錄；完整附錄請改用 Full 模式下載。",
      lite_fcf_note = "Lite 精簡版省略 FCFF 路徑圖；完整圖表請改用 Full 模式。",
      lite_sens_note = "Lite 精簡版省略 WACC × g 敏感度表；完整矩陣請改用 Full 模式。",
      disclaimer = paste0(
        "本報告由 The YNow App 自動產出，僅供研究與教育參考，不構成任何投資建議或要約。",
        "投資決策與損益一律由使用者自行承擔。",
        "評等僅列安全邊際 MOS／價差，不作券商買進／減持門檻。",
        "已排除同業排名、產業宇宙篩選與 Lab 敘事——僅保留搜尋標的之個股分析。",
        "隱私權與智慧財產權聲明請見 App 內「關於」分頁。"
      ),
      no_kpi = "尚無足夠財報資料計算 KPI。請先搜尋股票並載入財報。",
      no_fcf = "尚無 FCFF 預測圖。請於 DCF 分頁完成試算後再下載報告。",
      no_sens = "尚無 WACC × g 敏感度矩陣（需已跑出 DCF／DDM 路徑）。",
      no_warn = "系統未偵測到明顯財報警訊（仍請自行查核附註與新聞）。",
      warn_title = "檢測到以下財報警訊：",
      basic_info = "公司基本資訊",
      multi_model = "多模型每股合理價"
    )
  }

  # Lite: keep the first four analysis notes for a shorter desk brief.
  if (condensed && length(analysis_paragraphs) > 4L) {
    analysis_paragraphs <- analysis_paragraphs[seq_len(4L)]
  }

  list(
    target_formula = target_formula,
    ref_formula = ref_formula,
    mos_formula = mos_formula,
    upside_formula = upside_formula,
    investment_bullets = bullets,
    growth_bullets = growth_bullets,
    analysis_paragraphs = analysis_paragraphs,
    titles = titles,
    implied_pb = implied_pb,
    implied_pe = implied_pe,
    spot_pb = spot_pb,
    spot_pe = spot_pe,
    market_mode = mm,
    report_condensed = condensed
  )
}

# HTML → PDF（shinyapps／本機容器常用 --no-sandbox）
render_report_pdf <- function(html_path, pdf_path) {

  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop("需要 pagedown 套件以產出 PDF")
  }
  pagedown::chrome_print(
    input = html_path,
    output = pdf_path,
    wait = 2,
    timeout = 120,
    verbose = 0,
    extra_args = c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage")
  )
  invisible(pdf_path)
}
