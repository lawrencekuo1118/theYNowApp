# ==========================================
# lab_im_bluechip_eval.R — extracted from lab_industry_method.R (C-STRUCT-03)
# Behavior-preserving structural split; sourced by lab_industry_method.R.
# ==========================================

lab_industry_ke_pct <- function(industry_key) {
  ind <- tryCatch(industry_standards[[as.character(industry_key)[1]]], error = function(e) NULL)
  rf <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_rf %||% 4)[1])
  if (!is.finite(rf)) rf <- 4
  beta <- if (!is.null(ind$beta_avg)) suppressWarnings(as.numeric(ind$beta_avg)[1]) else
    suppressWarnings(as.numeric(APP_DEFAULTS$capm_beta %||% 1)[1])
  if (!is.finite(beta)) beta <- 1
  rm <- if (!is.null(ind$rm_avg)) suppressWarnings(as.numeric(ind$rm_avg)[1]) else
    suppressWarnings(as.numeric(APP_DEFAULTS$capm_rm %||% (rf + 5))[1])
  if (!is.finite(rm) || rm < rf + 2) rm <- rf + 5
  rf + beta * (rm - rf)
}

#' 實驗區簡化合理價／股（對齊產業主方法；非完整 UI 估值引擎）
#' n／Ke／g／g1 預設由個股財報＋β＋生命週期推算（lab_resolve_ticker_params）；
#' 僅在缺財報時退回 App／產業預設。可傳 n_years／ke_pct／g_pct／g1_pct／beta 覆寫。
lab_estimate_fv_per_share <- function(method, industry_key, d_is, d_bs, d_cf,
                                      price = NA_real_, market_cap = NA_real_,
                                      beta = NA_real_, ticker = "",
                                      n_years = NULL, ke_pct = NULL,
                                      g_pct = NULL, g1_pct = NULL) {
  method <- as.character(method %||% "pb")[1]
  key <- as.character(industry_key %||% "")[1]
  params <- tryCatch(
    lab_resolve_ticker_params(
      d_is, d_bs, d_cf,
      industry_key = key,
      beta = beta,
      ticker = ticker
    ),
    error = function(e) NULL
  )
  n <- suppressWarnings(as.integer(n_years %||% params$n_years %||% lab_model_horizon_years())[1])
  if (!is.finite(n) || n < 1L) n <- lab_model_horizon_years()
  ke_pct <- suppressWarnings(as.numeric(ke_pct %||% params$ke_pct %||% lab_industry_ke_pct(key))[1])
  if (!is.finite(ke_pct) || ke_pct <= 0) ke_pct <- lab_industry_ke_pct(key)
  ke <- ke_pct / 100
  g_pct <- suppressWarnings(as.numeric(g_pct %||% params$g_pct %||% APP_DEFAULTS$sgr %||% 4)[1])
  if (!is.finite(g_pct)) g_pct <- 4
  g <- max(0.005, min(g_pct / 100, ke - 0.015))
  g1_pct <- suppressWarnings(as.numeric(g1_pct %||% params$g1_pct %||% APP_DEFAULTS$custom_g %||% g_pct)[1])
  if (!is.finite(g1_pct)) g1_pct <- g_pct
  g1 <- max(0.01, min(g1_pct / 100, 0.15))

  shares <- tryCatch(
    select_current_metric(d_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock"),
    error = function(e) NA_real_
  )
  shares <- suppressWarnings(as.numeric(shares)[1])
  sh_res <- tryCatch(
    resolve_shares_for_price(
      shares,
      price = price,
      market_cap = market_cap,
      ticker = ""
    ),
    error = function(e) NULL
  )
  if (!is.null(sh_res) && is.finite(sh_res$shares) && sh_res$shares > 0) {
    shares <- sh_res$shares
  }
  if (!is.finite(shares) || shares <= 0) {
    px <- suppressWarnings(as.numeric(price)[1])
    mc <- suppressWarnings(as.numeric(market_cap)[1])
    if (is.finite(px) && px > 0 && is.finite(mc) && mc > 0) shares <- mc / px
  }
  if (!is.finite(shares) || shares <= 0) {
    return(list(fv = NA_real_, method = method, n_years = n, note = "無股數"))
  }

  equity <- tryCatch(
    select_current_metric_any(d_bs, EQUITY_PATTERNS, "stock"),
    error = function(e) NA_real_
  )
  equity <- suppressWarnings(as.numeric(equity)[1])
  bvps <- if (is.finite(equity) && equity > 0) equity / shares else NA_real_

  ni <- tryCatch(
    select_current_metric_any(d_is, NET_INCOME_PATTERNS, "flow"),
    error = function(e) NA_real_
  )
  ni <- suppressWarnings(as.numeric(ni)[1])
  roe <- if (is.finite(ni) && is.finite(equity) && equity > 0) ni / equity else NA_real_

  .pb_mid <- function() {
    ind <- tryCatch(industry_standards[[key]], error = function(e) NULL)
    band <- ind$pb_band
    if (!is.null(band) && length(band) >= 2) {
      if (length(band) >= 3) return(suppressWarnings(as.numeric(band[3])[1]))
      return(mean(suppressWarnings(as.numeric(band[1:2]))))
    }
    1.4
  }

  fv <- NA_real_
  note <- method

  if (identical(method, "pb")) {
    pb_m <- .pb_mid()
    # 可用 Justified 時與產業 mid 平均
    just <- NA_real_
    if (is.finite(roe) && is.finite(ke) && ke > g) {
      just <- max(0.3, min(8, (roe - g) / (ke - g)))
    }
    mult <- if (is.finite(just)) (0.55 * just + 0.45 * pb_m) else pb_m
    fv <- if (is.finite(bvps)) bvps * mult else NA_real_
    note <- "P/B×BVPS"
  } else if (identical(method, "ddm")) {
    div_tot <- tryCatch(
      abs(select_current_metric(d_cf, "Cash Dividends Paid", "flow")),
      error = function(e) NA_real_
    )
    div_tot <- suppressWarnings(as.numeric(div_tot)[1])
    dps <- if (is.finite(div_tot) && div_tot > 0) div_tot / shares else NA_real_
    if (is.finite(dps) && dps > 0 && ke > g) {
      fv <- dps * (1 + g) / (ke - g)
      note <- "Gordon DDM"
    } else {
      # 無股利 → 退回 P/B
      fv <- if (is.finite(bvps)) bvps * .pb_mid() else NA_real_
      note <- "DDM缺股利→P/B"
    }
  } else if (identical(method, "ri")) {
    if (is.finite(bvps) && is.finite(roe) && ke > g) {
      # 簡化：B0 + PV of perpetual residual income
      fv <- bvps + (roe - ke) * bvps / (ke - g)
      if (fv < 0) fv <- bvps
      note <- "簡化 RI"
    } else if (is.finite(bvps)) {
      fv <- bvps * .pb_mid()
      note <- "RI缺ROE→P/B"
    }
  } else if (identical(method, "nav")) {
    disc <- suppressWarnings(as.numeric(APP_DEFAULTS$nav_holdco_discount %||% 0)[1])
    if (!is.finite(disc)) disc <- 0
    disc <- max(0, min(0.5, disc))
    navc <- tryCatch(
      extract_nav_components(d_bs, holdco_discount = disc),
      error = function(e) NULL
    )
    nav_tot <- if (!is.null(navc)) suppressWarnings(as.numeric(navc$nav)[1]) else NA_real_
    navps <- if (is.finite(nav_tot) && nav_tot > 0) nav_tot / shares else NA_real_
    if (!is.finite(navps) && is.finite(bvps) && bvps > 0) navps <- bvps
    mult <- suppressWarnings(as.numeric(APP_DEFAULTS$nav_mid %||% 1)[1])
    if (!is.finite(mult) || mult <= 0) mult <- 1
    if (is.finite(navps) && navps > 0) {
      fv <- navps * mult
      note <- if (!is.null(navc) && nzchar(as.character(navc$note %||% "")[1])) {
        paste0("簡化NAV×", sprintf("%.2f", mult), "（", as.character(navc$note)[1], "）")
      } else {
        paste0("簡化NAV×", sprintf("%.2f", mult))
      }
    } else if (is.finite(bvps)) {
      fv <- bvps * .pb_mid()
      note <- "NAV缺權益→P/B"
    }
  } else if (identical(method, "multiples")) {
    # Simplified trading multiple: Trailing EPS × selected/default P/E (Implied Price)
    eps <- tryCatch(
      select_current_metric(d_is, "Diluted EPS|Basic EPS|^EPS$", "flow"),
      error = function(e) NA_real_
    )
    eps <- suppressWarnings(as.numeric(eps)[1])
    if (!is.finite(eps) && is.finite(ni) && is.finite(shares) && shares > 0) eps <- ni / shares
    pe_m <- suppressWarnings(as.numeric(APP_DEFAULTS$rel_pe_multiple %||% 18)[1])
    if (!is.finite(pe_m) || pe_m <= 0) pe_m <- 18
    if (is.finite(eps) && eps > 0) {
      fv <- eps * pe_m
      note <- paste0("Implied P/E×", sprintf("%.1f", pe_m))
    } else {
      rev <- tryCatch(
        select_current_metric(d_is, "Total Revenue|^Revenue$|Operating Revenue", "flow"),
        error = function(e) NA_real_
      )
      rev <- suppressWarnings(as.numeric(rev)[1])
      ps_m <- suppressWarnings(as.numeric(APP_DEFAULTS$rel_ps_multiple %||% 3)[1])
      if (!is.finite(ps_m) || ps_m <= 0) ps_m <- 3
      if (is.finite(rev) && rev > 0 && is.finite(shares) && shares > 0) {
        fv <- (rev * ps_m) / shares
        note <- paste0("Implied P/S×", sprintf("%.1f", ps_m))
      } else if (is.finite(bvps)) {
        fv <- bvps * .pb_mid()
        note <- "Multiples缺EPS→P/B"
      }
    }
  } else if (identical(method, "sotp")) {
    # Lab simplified SOTP proxy: total revenue × EV/Sales − net debt (not true multi-segment)
    rev <- tryCatch(
      select_current_metric(d_is, "Total Revenue|^Revenue$|Operating Revenue", "flow"),
      error = function(e) NA_real_
    )
    rev <- suppressWarnings(as.numeric(rev)[1])
    evs <- suppressWarnings(as.numeric(APP_DEFAULTS$rel_ev_sales_multiple %||% 3)[1])
    if (!is.finite(evs) || evs <= 0) evs <- 3
    debt <- tryCatch(
      select_current_metric(d_bs, "^Total Debt$|Long Term Debt", "stock"),
      error = function(e) NA_real_
    )
    cash <- tryCatch(
      select_current_metric(d_bs, "Cash And Cash Equivalents|Cash Cash Equivalents And Short Term Investments", "stock"),
      error = function(e) NA_real_
    )
    debt <- suppressWarnings(as.numeric(debt)[1])
    cash <- suppressWarnings(as.numeric(cash)[1])
    if (is.finite(rev) && rev > 0 && is.finite(shares) && shares > 0) {
      ev <- rev * evs
      eq <- ev + (if (is.finite(cash)) cash else 0) - (if (is.finite(debt)) debt else 0)
      fv <- eq / shares
      note <- paste0("SOTP proxy Rev×EV/Sales ", sprintf("%.1f", evs), "×")
    } else if (is.finite(bvps)) {
      fv <- bvps * .pb_mid()
      note <- "SOTP缺營收→P/B"
    }
  } else {
    # DCF：n 年顯式 FCF 成長＋終值（用 Ke 近似折現；實驗區簡化）
    fcf <- tryCatch(
      latest_hist_fcff(d_cf, d_is = d_is),
      error = function(e) NA_real_
    )
    fcf <- suppressWarnings(as.numeric(fcf)[1])
    r <- max(ke, g + 0.02)
    if (is.finite(fcf) && fcf > 0) {
      fcfs <- fcf * (1 + g1)^seq_len(n)
      dfs <- cumprod(rep(1 + r, n))
      pv_fcf <- sum(fcfs / dfs)
      tv <- fcfs[n] * (1 + g) / (r - g)
      enterprise <- pv_fcf + tv / dfs[n]
      debt <- tryCatch(
        select_current_metric(d_bs, "^Total Debt$|Long Term Debt", "stock"),
        error = function(e) NA_real_
      )
      cash <- tryCatch(
        select_current_metric(d_bs, "Cash And Cash Equivalents|Cash Cash Equivalents And Short Term Investments", "stock"),
        error = function(e) NA_real_
      )
      debt <- suppressWarnings(as.numeric(debt)[1])
      cash <- suppressWarnings(as.numeric(cash)[1])
      net_debt <- if (is.finite(debt)) debt - (if (is.finite(cash)) cash else 0) else 0
      equity_val <- enterprise - net_debt
      fv <- equity_val / shares
      note <- paste0("簡化DCF(", n, "年)")
    } else if (is.finite(bvps)) {
      fv <- bvps * .pb_mid()
      note <- "DCF缺FCF→P/B"
    }
  }

  list(
    fv = suppressWarnings(as.numeric(fv)[1]),
    method = method,
    n_years = as.integer(n),
    note = note,
    ke_pct = ke_pct,
    g_pct = g_pct,
    g1_pct = g1_pct,
    beta = if (!is.null(params)) params$beta else NA_real_,
    beta_source = if (!is.null(params)) params$beta_source else NA_character_,
    lifecycle_stage = if (!is.null(params)) params$lifecycle_stage else NA_character_,
    fund_sgr_pct = if (!is.null(params)) params$fund_sgr_pct else NA_real_,
    rev_cagr_pct = if (!is.null(params)) params$rev_cagr_pct else NA_real_
  )
}

#' 產業預設主／副評價方法（無個股財報時的規則映射）
#' @return data.frame: industry_key, primary, secondary, suggest_two_stage, rationale
lab_industry_method_defaults <- function() {
  rows <- list(
    list("tech.Optoelectronics", "dcf", "pb", FALSE, "光電／顯示循環 → DCF"),
    list("tech.Electronics_Distribution", "dcf", "pb", FALSE, "電子通路低毛利周轉 → DCF"),
    list("tech.IT_Services", "dcf", "pb", FALSE, "資訊服務 → DCF"),
    list("cons.Restaurants", "dcf", "ddm", FALSE, "餐飲現金流／成熟配息 → DCF"),
    list("cons.Home_Living", "dcf", "pb", FALSE, "居家生活消費 → DCF"),
    list("cons.Sports_Leisure", "dcf", "pb", FALSE, "運動休閒 → DCF"),
    list("ind.Conglomerate", "nav", "sotp", FALSE, "綜合／其他 → 純 NAV；SOTP 結構交叉"),
    list("mat.Textiles", "dcf", "pb", FALSE, "紡織纖維循環 → DCF"),
    list("mat.Paper_Packaging", "dcf", "pb", FALSE, "造紙包裝 → DCF"),
    list("mat.Glass_Ceramics", "dcf", "pb", FALSE, "玻璃陶瓷 → DCF"),
    list("en.Environmental", "dcf", "pb", TRUE, "綠能環保服務成長 → DCF"),
    list("ag.Agriculture", "dcf", "pb", FALSE, "農業科技 → DCF"),
    list("media.Advertising", "dcf", "pb", FALSE, "廣告行銷 → DCF"),
    list("bus.Professional_Services", "dcf", "pb", FALSE, "專業服務 → DCF"),

    # 金融／控股／REIT／公用 → P/B（+ RI）；控股主模型純 NAV
    list("fn.Banking", "pb", "ri", FALSE, "金融簿價驅動 → P/B；ROE 可時交叉 RI"),
    list("fn.Investment_Banking", "pb", "ri", FALSE, "金融簿價驅動 → P/B"),
    list("fn.Insurance", "pb", "ri", FALSE, "保險／帳面導向 → P/B"),
    list("fn.Asset_Management", "pb", "nav", FALSE, "資產管理偏帳面 → P/B；副選純 NAV"),
    list("fn.Fintech", "dcf", "pb", TRUE, "成長型金融科技 → 兩階段 DCF"),
    list("fn.Conglomerate_Holding", "nav", "sotp", FALSE, "控股／綜合企業 → 純 NAV；SOTP 結構交叉"),
    list("re.REIT", "pb", "nav", FALSE, "REIT 簿價／殖利率 → P/B；副選純 NAV"),
    list("en.Utilities", "pb", "ddm", FALSE, "公用事業簿價／管制資產 → P/B；高配息輔 DDM"),

    # 高成長科技／生技／EV → DCF two-stage
    # Multiples is a universal Reference (not a secondary candidate) — pair FV secondaries instead.
    list("saas.SaaS_Cloud", "dcf", "ri", TRUE, "SaaS／雲端成長 → 兩階段 DCF；副選 RI；Multiples 為參照"),
    list("tech.Internet_Platform", "dcf", "ri", TRUE, "網路平台成長 → 兩階段 DCF；副選 RI；Multiples 為參照"),
    list("tech.Software", "dcf", "ri", TRUE, "軟體成長／穩健 FCF → DCF；副選 RI；Multiples 為參照"),
    list("hc.Biotech", "dcf", "pb", TRUE, "生技早期成長 → 兩階段 DCF（風險高）"),
    list("auto.Automotive_EV", "dcf", "pb", TRUE, "電動車成長 → 兩階段 DCF"),
    list("auto.EV_Startups", "dcf", "pb", TRUE, "新創 EV → 兩階段 DCF"),
    list("en.Renewables", "dcf", "pb", TRUE, "再生能源成長 → 兩階段 DCF"),

    # 半導體／硬體
    list("sc.IC_Design", "dcf", "pb", TRUE, "IC 設計常具成長與 FCF → DCF"),
    list("sc.Foundry", "dcf", "pb", FALSE, "晶圓代工資本密集／景氣循環 → DCF；P/B 為副模型"),
    list("sc.Packaging", "dcf", "pb", FALSE, "封測循環／FCF → DCF"),
    list("sc.Memory", "dcf", "pb", FALSE, "記憶體循環股 → DCF（波動大）"),
    list("sc.Equipment", "dcf", "pb", TRUE, "設備材料成長循環 → DCF"),
    list("tech.Hardware", "dcf", "ddm", FALSE, "硬體成熟 FCF／配息 → DCF（穩健者可輔 DDM）"),
    list("ec.Hardware", "dcf", "pb", FALSE, "電子零組件 → DCF"),

    # 消費成熟配息傾向
    list("fmcg.Food_Beverages", "ddm", "dcf", FALSE, "食品飲料穩定配息 → DDM；FCF 穩則輔 DCF"),
    list("fmcg.Household_Personal", "ddm", "dcf", FALSE, "家用品／個護防禦配息 → DDM"),
    list("fmcg.Health_Beauty", "dcf", "ddm", FALSE, "健康美容穩健 FCF → DCF"),
    list("lxg.Luxury_Fashion", "dcf", "pb", FALSE, "精品現金流品質 → DCF"),
    list("ecr.Ecommerce_Retail", "dcf", "pb", TRUE, "電商成長 → DCF"),
    list("retail.Brick_Mortar", "dcf", "ddm", FALSE, "實體零售 → DCF／成熟配息者 DDM"),
    list("cons.Discretionary", "dcf", "pb", FALSE, "非必需消費 → DCF"),

    # 汽車／醫療成熟
    list("auto.Vehicle_Manufacturing", "dcf", "pb", FALSE, "整車製造循環 → DCF"),
    list("auto.Parts_Suppliers", "dcf", "pb", FALSE, "汽車零組件 → DCF"),
    list("hc.Healthcare_Services", "dcf", "pb", FALSE, "醫療服務穩定現金流 → DCF"),
    list("hc.Pharma", "dcf", "ddm", FALSE, "製藥成熟 FCF／配息 → DCF（可輔 DDM）"),
    list("hc.Medtech", "dcf", "pb", FALSE, "醫材 → DCF"),

    # 工業／原物料／能源／通訊／運輸／媒體
    list("ind.Machinery", "dcf", "pb", FALSE, "機械設備 → DCF"),
    list("ind.Aerospace_Defense", "dcf", "pb", FALSE, "航太國防 → DCF"),
    list("ind.Construction", "dcf", "pb", FALSE, "營建工程 → DCF"),
    list("mat.Chemicals", "dcf", "pb", FALSE, "化學循環 → DCF"),
    list("mat.Metals_Mining", "dcf", "pb", FALSE, "金屬礦業循環 → DCF"),
    list("en.Energy_OilGas", "dcf", "pb", FALSE, "油氣循環／FCF → DCF；P/B 為副模型"),
    list("tel.Telecom", "ddm", "dcf", FALSE, "電信高配息 → DDM；穩健 FCF 輔 DCF"),
    list("tr.Logistics_Shipping", "dcf", "pb", FALSE, "物流運輸 → DCF"),
    list("tr.Airlines", "dcf", "pb", FALSE, "航空循環／資本密集 → DCF"),
    list("media.Entertainment", "dcf", "pb", TRUE, "娛樂內容成長／轉型 → DCF"),
    list("media.Gaming", "dcf", "pb", TRUE, "遊戲成長 → DCF"),
    list("hosp.Hotels_Travel", "dcf", "pb", FALSE, "旅宿循環 → DCF")
  )

  df <- do.call(rbind, lapply(rows, function(r) {
    data.frame(
      industry_key = r[[1]],
      primary = r[[2]],
      secondary = r[[3]],
      suggest_two_stage = isTRUE(r[[4]]),
      rationale = r[[5]],
      stringsAsFactors = FALSE
    )
  }))

  # 覆蓋產業標籤；缺漏的產業鍵用 DCF 後備
  all_keys <- names(industry_labels)
  miss <- setdiff(all_keys, df$industry_key)
  if (length(miss) > 0) {
    df <- rbind(
      df,
      data.frame(
        industry_key = miss,
        primary = "dcf",
        secondary = "pb",
        suggest_two_stage = FALSE,
        rationale = "未特化產業規則 → 預設 DCF",
        stringsAsFactors = FALSE
      )
    )
  }
  if (exists("LAB_UNMAPPED_KEY", inherits = TRUE) &&
      !LAB_UNMAPPED_KEY %in% df$industry_key) {
    df <- rbind(
      df,
      data.frame(
        industry_key = LAB_UNMAPPED_KEY,
        primary = "dcf",
        secondary = "pb",
        suggest_two_stage = FALSE,
        rationale = "GICS 未對應 App 產業 → 預設 DCF",
        stringsAsFactors = FALSE
      )
    )
  }
  # Industry-catalog prior only (no statements yet). Per-stock recommend_valuation_models
  # ranks from financial-statement attributes; a cyclical label does not force P/B.
  if (exists("is_cyclical_industry", mode = "function") &&
      exists("cyclical_pb_slot", mode = "function")) {
    for (i in seq_len(nrow(df))) {
      if (!isTRUE(is_cyclical_industry(df$industry_key[[i]], ""))) next
      slot <- cyclical_pb_slot(df$primary[[i]], df$secondary[[i]], pb_ok = TRUE)
      if (isTRUE(slot$changed)) {
        df$secondary[[i]] <- slot$secondary
        df$rationale[[i]] <- paste0(df$rationale[[i]], "；景氣循環 → P/B 為主或副模型")
      }
    }
  }
  df$industry_label <- ifelse(
    exists("LAB_UNMAPPED_KEY", inherits = TRUE) & df$industry_key == LAB_UNMAPPED_KEY,
    LAB_UNMAPPED_LABEL,
    as.character(industry_labels[df$industry_key])
  )
  df$rel_family <- vapply(seq_len(nrow(df)), function(i) {
    lab_default_rel_family(df$industry_key[[i]], df$primary[[i]])
  }, character(1))
  df$primary_label <- as.character(LAB_METHOD_LABELS[df$primary])
  df$secondary_label <- ifelse(
    is.na(df$secondary) | !nzchar(df$secondary),
    "",
    as.character(LAB_METHOD_LABELS[df$secondary])
  )
  df[order(df$primary, df$industry_label), , drop = FALSE]
}

#' 各產業美股績優候選（US primary listings 宇宙；一檔對一產業，不重抓財報）
#' @return named list: industry_key → character vector of US tickers
lab_us_quality_candidates <- function() {
  u <- tryCatch(lab_get_us_universe(FALSE), error = function(e) NULL)
  if (is.null(u) || !is.data.frame(u) || nrow(u) == 0L) {
    u <- tryCatch(lab_get_sp500_universe(FALSE), error = function(e) NULL)
  }
  if (is.null(u) || !is.data.frame(u) || nrow(u) == 0L) return(list())
  tks <- as.character(u$ticker)
  keys <- as.character(u$industry_key)
  keep <- nzchar(tks) & !is.na(tks) & nzchar(keys) & !is.na(keys)
  tks <- tks[keep]
  keys <- keys[keep]
  drop <- grepl("\\.TW$|\\.TWO$|-TW$|-TWO$", tks, ignore.case = TRUE) |
    tks %in% c("SPY", "QQQ", "DIA", "IWM", "VOO", "IVV", "VTI", "QQQM")
  tks <- tks[!drop]
  keys <- keys[!drop]
  if (!length(tks)) return(list())
  split(tks, keys)
}

#' 依市場模式回傳績優候選（美股主要上市／台股依 boards）
#' @param boards TW only：TWSE／TPEX／ESB（預設上市＋上櫃）
lab_quality_candidates_for_market <- function(mode = get_market_mode(),
                                              boards = c("TWSE", "TPEX")) {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    m <- toupper(trimws(as.character(mode)[1]))
    if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
  }
  if (identical(mode, "TW") && exists("lab_tw_quality_candidates", mode = "function")) {
    return(lab_tw_quality_candidates(boards = boards))
  }
  lab_us_quality_candidates()
}

#' Attach TW exchange／board columns onto a ticker frame (no-op for US)
lab_attach_tw_exchange <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || !"ticker" %in% names(df)) {
    return(df)
  }
  if (!exists("lookup_tw_universe_exchange", mode = "function")) return(df)
  if (!"exchange" %in% names(df)) df$exchange <- NA_character_
  miss <- is.na(df$exchange) | !nzchar(trimws(as.character(df$exchange)))
  if (any(miss)) {
    df$exchange[miss] <- vapply(
      as.character(df$ticker[miss]),
      function(tk) {
        ex <- tryCatch(lookup_tw_universe_exchange(tk), error = function(e) "")
        if (!nzchar(ex)) "" else ex
      },
      character(1)
    )
  }
  if (exists("lab_tw_exchange_to_board", mode = "function")) {
    df$board <- vapply(
      as.character(df$exchange),
      function(ex) lab_tw_exchange_to_board(ex),
      character(1)
    )
  }
  df
}

#' Filter a ticker frame by TW boards (TWSE／TPEX／ESB). Empty boards → empty frame.
lab_filter_df_by_tw_boards <- function(df, boards = c("TWSE", "TPEX")) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)
  if (!exists("lab_normalize_tw_boards", mode = "function")) return(df)
  boards <- lab_normalize_tw_boards(boards)
  if (!length(boards)) return(df[0, , drop = FALSE])
  df <- lab_attach_tw_exchange(df)
  if (!"exchange" %in% names(df) && !"board" %in% names(df)) return(df)
  keep <- if ("board" %in% names(df)) {
    as.character(df$board) %in% boards
  } else {
    vapply(
      as.character(df$exchange),
      function(ex) isTRUE(lab_tw_exchange_in_boards(ex, boards)),
      logical(1)
    )
  }
  df[keep, , drop = FALSE]
}

#' 展開為一列一檔的產業×方法×候選表
#' @param market_mode "US" | "TW"（預設讀取 get_market_mode）
#' @param boards TW only：TWSE／TPEX／ESB（預設上市＋上櫃）
lab_build_industry_method_catalog <- function(market_mode = NULL,
                                             boards = c("TWSE", "TPEX")) {
  mode <- if (!is.null(market_mode)) {
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market_mode)
    } else {
      m <- toupper(trimws(as.character(market_mode)[1]))
      if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
    }
  } else if (exists("get_market_mode", mode = "function")) {
    get_market_mode()
  } else {
    "US"
  }
  defaults <- lab_industry_method_defaults()
  cands <- lab_quality_candidates_for_market(mode, boards = boards)
  rows <- list()
  for (i in seq_len(nrow(defaults))) {
    key <- defaults$industry_key[[i]]
    tickers <- unique(as.character(cands[[key]] %||% character(0)))
    tickers <- tickers[nzchar(tickers)]
    if (identical(mode, "TW")) {
      tickers <- tickers[grepl("\\.(TW|TWO)$", tickers, ignore.case = TRUE)]
      tickers <- setdiff(tickers, c("0050.TW", "0056.TW", "^TWII"))
    } else {
      # 排除台股與常見指數 ETF
      tickers <- tickers[!grepl("\\.TW$|\\.TWO$", tickers, ignore.case = TRUE)]
      tickers <- setdiff(tickers, c("SPY", "QQQ", "DIA", "IWM"))
    }
    if (length(tickers) == 0L) {
      rows[[length(rows) + 1L]] <- data.frame(
        industry_key = key,
        industry_label = defaults$industry_label[[i]],
        primary = defaults$primary[[i]],
        primary_label = defaults$primary_label[[i]],
        secondary = defaults$secondary[[i]],
        secondary_label = defaults$secondary_label[[i]],
        suggest_two_stage = defaults$suggest_two_stage[[i]],
        rationale = defaults$rationale[[i]],
        rel_family = defaults$rel_family[[i]],
        ticker = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      for (tk in tickers) {
        rows[[length(rows) + 1L]] <- data.frame(
          industry_key = key,
          industry_label = defaults$industry_label[[i]],
          primary = defaults$primary[[i]],
          primary_label = defaults$primary_label[[i]],
          secondary = defaults$secondary[[i]],
          secondary_label = defaults$secondary_label[[i]],
          suggest_two_stage = defaults$suggest_two_stage[[i]],
          rationale = defaults$rationale[[i]],
          rel_family = defaults$rel_family[[i]],
          ticker = tk,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  # Attach ADR flag from US universe (TW rows are never US ADR)
  if (identical(mode, "US") && exists("lab_is_us_adr", mode = "function") &&
      is.data.frame(out) && nrow(out) > 0L && "ticker" %in% names(out)) {
    u <- tryCatch(lab_get_us_universe(FALSE), error = function(e) NULL)
    if (!is.null(u) && is.data.frame(u) && "is_adr" %in% names(u)) {
      idx <- match(
        toupper(trimws(as.character(out$ticker))),
        toupper(trimws(as.character(u$ticker)))
      )
      out$is_adr <- FALSE
      hit <- which(is.finite(idx))
      if (length(hit)) {
        out$is_adr[hit] <- vapply(
          idx[hit],
          function(j) isTRUE(as.logical(u$is_adr[[j]])),
          logical(1)
        )
      }
    } else {
      out$is_adr <- vapply(
        as.character(out$ticker),
        function(tk) isTRUE(lab_is_us_adr(tk, NULL)),
        logical(1)
      )
    }
  } else if (is.data.frame(out) && nrow(out) > 0L) {
    out$is_adr <- FALSE
  }
  # TW: attach exchange／board for 上市／上櫃／興櫃 filters
  if (identical(mode, "TW") && is.data.frame(out) && nrow(out) > 0L) {
    out <- lab_attach_tw_exchange(out)
  }
  out
}

#' 是否通過舊版綜合品質門檻（F-Score≥7 且盈餘品質通過；保留供相容）
lab_is_quality_stock <- function(f_score, quality_flag, min_score = 7L) {
  fs <- suppressWarnings(as.numeric(f_score)[1])
  qf <- suppressWarnings(as.numeric(quality_flag)[1])
  isTRUE(is.finite(fs) && fs >= as.numeric(min_score) && isTRUE(qf == 1))
}

#' 評估單一標的：F-Score 門檻＋產業主方法簡化合理價＋個股 n 年年化漲幅
lab_evaluate_ticker_fscore <- function(ticker, industry_key = NULL, method = NULL) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  n_yrs <- lab_model_horizon_years()
  out <- list(
    ticker = tk,
    ok = FALSE,
    f_score = NA_real_,
    quality_flag = NA_real_,
    n_fs_alerts = NA_integer_,
    is_quality = FALSE,
    market_cap = NA_real_,
    price = NA_real_,
    beta = NA_real_,
    fv = NA_real_,
    upside_total_pct = NA_real_,
    upside_cagr_pct = NA_real_,
    n_years = n_yrs,
    ke_pct = NA_real_,
    g_pct = NA_real_,
    g1_pct = NA_real_,
    lifecycle_stage = NA_character_,
    fv_note = NA_character_,
    company_type = NA_character_,
    company_name = NA_character_,
    primary_live = NA_character_,
    method_used = as.character(method %||% NA_character_)[1],
    is_quality_upside = FALSE,
    error = NULL
  )
  if (!nzchar(tk)) {
    out$error <- "空代號"
    return(out)
  }

  sm <- tryCatch(lab_fetch_summary_metrics(tk), error = function(e) {
    list(market_cap = NA_real_, price = NA_real_, beta = NA_real_, company_name = NA_character_)
  })
  out$market_cap <- suppressWarnings(as.numeric(sm$market_cap)[1])
  out$price <- suppressWarnings(as.numeric(sm$price)[1])
  out$beta <- suppressWarnings(as.numeric(sm$beta)[1])
  out$company_name <- lab_company_display_name(tk, sm$company_name)

  res <- tryCatch(cached_scrape_financials(tk), error = function(e) e)
  if (inherits(res, "error")) {
    out$error <- conditionMessage(res)
    return(out)
  }
  if (is.null(res) || !is.list(res)) {
    out$error <- "無法取得財報"
    return(out)
  }
  d_is <- tryCatch(res[["Income Statement"]]$expanded, error = function(e) NULL)
  d_bs <- tryCatch(res[["Balance Sheet"]]$expanded, error = function(e) NULL)
  d_cf <- tryCatch(res[["Cash Flow"]]$expanded, error = function(e) NULL)
  fs <- tryCatch(
    compute_report_f_score(d_is, d_bs, d_cf),
    error = function(e) list(total = NA_real_, quality_flag = NA_real_)
  )
  out$f_score <- suppressWarnings(as.numeric(fs$total)[1])
  out$quality_flag <- suppressWarnings(as.numeric(fs$quality_flag)[1])
  out$is_quality <- lab_is_quality_stock(out$f_score, out$quality_flag)

  ind_key <- as.character(industry_key %||% "")[1]
  # 財報警訊：與 YNOW 漏斗相同定義（Schilit 警示 + 盈餘品質紅旗）
  if (exists("collect_fraud_warnings", mode = "function")) {
    warns <- tryCatch(
      collect_fraud_warnings(d_cf, d_is, d_bs, industry_key = ind_key),
      error = function(e) character(0)
    )
    out$n_fs_alerts <- as.integer(length(warns))
  } else {
    out$n_fs_alerts <- NA_integer_
  }
  meth <- as.character(method %||% "")[1]
  if (!nzchar(meth)) meth <- "dcf"

  rec <- tryCatch(
    recommend_valuation_models(
      d_cf, industry_text = "", d_is = d_is, d_bs = d_bs, industry_choice = ind_key
    ),
    error = function(e) NULL
  )
  if (!is.null(rec)) {
    out$company_type <- as.character(rec$company_type %||% NA_character_)
    out$primary_live <- as.character(rec$primary %||% NA_character_)
  }

  # 估值用實際建議主方法（有財報時），否則用產業預設
  meth_fv <- if (!is.null(rec) && nzchar(as.character(rec$primary %||% ""))) {
    as.character(rec$primary)[1]
  } else {
    meth
  }

  fv_res <- tryCatch(
    lab_estimate_fv_per_share(
      meth_fv, ind_key, d_is, d_bs, d_cf,
      price = out$price, market_cap = out$market_cap,
      beta = out$beta, ticker = tk
    ),
    error = function(e) list(fv = NA_real_, n_years = n_yrs, note = e$message)
  )
  out$fv <- suppressWarnings(as.numeric(fv_res$fv)[1])
  out$n_years <- suppressWarnings(as.integer(fv_res$n_years %||% n_yrs)[1])
  out$ke_pct <- suppressWarnings(as.numeric(fv_res$ke_pct)[1])
  out$g_pct <- suppressWarnings(as.numeric(fv_res$g_pct)[1])
  out$g1_pct <- suppressWarnings(as.numeric(fv_res$g1_pct)[1])
  out$lifecycle_stage <- as.character(fv_res$lifecycle_stage %||% NA_character_)[1]
  out$fv_note <- as.character(fv_res$note %||% "")[1]
  out$method_used <- meth_fv
  if (is.finite(out$fv) && is.finite(out$price) && out$price > 0) {
    out$upside_total_pct <- (out$fv / out$price - 1) * 100
    out$upside_cagr_pct <- lab_annualized_upside_pct(out$fv, out$price, out$n_years)
  }

  # 績優候選：通過門檻且能算出年化漲幅
  out$is_quality_upside <- isTRUE(out$is_quality) && is.finite(out$upside_cagr_pct)

  out$ok <- isTRUE(is.finite(out$f_score))
  if (!out$ok && is.null(out$error)) out$error <- "F-Score 無法計算"
  out
}

#' 批次評估；tickers_df 可含 ticker / industry_key / primary 欄
#' 呼叫端應已用 lab_rank_and_cap_eval_pool 取好評估池；max_n=Inf 時不再截斷。
lab_screen_tickers_fscore <- function(tickers, progress_cb = NULL, max_n = Inf,
                                      industry_keys = NULL, methods = NULL) {
  # 接受字元向量或 data.frame
  if (is.data.frame(tickers)) {
    tks <- as.character(tickers$ticker)
    ind_keys <- as.character(tickers$industry_key %||% rep(NA_character_, nrow(tickers)))
    meths <- as.character(tickers$primary %||% rep(NA_character_, nrow(tickers)))
  } else {
    tks <- as.character(tickers)
    ind_keys <- if (is.null(industry_keys)) rep(NA_character_, length(tks)) else as.character(industry_keys)
    meths <- if (is.null(methods)) rep(NA_character_, length(tks)) else as.character(methods)
  }
  keep <- nzchar(toupper(trimws(tks))) & !is.na(tks)
  tks <- toupper(trimws(tks[keep]))
  ind_keys <- ind_keys[keep]
  meths <- meths[keep]
  mode <- if (exists("get_market_mode", mode = "function")) get_market_mode() else "US"
  if (identical(mode, "TW")) {
    keep_mkt <- grepl("\\.(TW|TWO)$", tks) | grepl("^\\^", tks)
    tks <- tks[keep_mkt]; ind_keys <- ind_keys[keep_mkt]; meths <- meths[keep_mkt]
  } else {
    drop_tw <- grepl("\\.TW$|\\.TWO$", tks)
    tks <- tks[!drop_tw]; ind_keys <- ind_keys[!drop_tw]; meths <- meths[!drop_tw]
  }

  # 同一代碼只評估一次（取第一筆產業／方法）
  ord <- !duplicated(tks)
  tks <- tks[ord]; ind_keys <- ind_keys[ord]; meths <- meths[ord]

  if (is.finite(max_n) && length(tks) > as.integer(max_n)) {
    tks <- head(tks, as.integer(max_n))
    ind_keys <- head(ind_keys, as.integer(max_n))
    meths <- head(meths, as.integer(max_n))
  }
  n <- length(tks)
  rows <- vector("list", n)
  for (i in seq_along(tks)) {
    if (is.function(progress_cb)) {
      tryCatch(progress_cb(i, n, tks[[i]]), error = function(e) NULL)
    }
    ev <- lab_evaluate_ticker_fscore(tks[[i]], industry_key = ind_keys[[i]], method = meths[[i]])
    rows[[i]] <- data.frame(
      ticker = ev$ticker,
      ok = isTRUE(ev$ok),
      f_score = ev$f_score,
      quality_flag = ev$quality_flag,
      n_fs_alerts = as.integer(ev$n_fs_alerts %||% NA_integer_),
      is_quality = isTRUE(ev$is_quality),
      is_quality_upside = isTRUE(ev$is_quality_upside),
      market_cap = ev$market_cap,
      price = ev$price,
      fv = ev$fv,
      upside_total_pct = ev$upside_total_pct,
      upside_cagr_pct = ev$upside_cagr_pct,
      n_years = ev$n_years,
      fv_note = ev$fv_note %||% NA_character_,
      method_used = ev$method_used %||% NA_character_,
      primary_live = ev$primary_live %||% NA_character_,
      company_type = ev$company_type %||% NA_character_,
      company_name = ev$company_name %||% NA_character_,
      error = ev$error %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
  empty <- data.frame(
    ticker = character(0), ok = logical(0), f_score = numeric(0),
    quality_flag = numeric(0), n_fs_alerts = integer(0),
    is_quality = logical(0),
    is_quality_upside = logical(0),
    market_cap = numeric(0),
    price = numeric(0), fv = numeric(0),
    upside_total_pct = numeric(0), upside_cagr_pct = numeric(0),
    n_years = integer(0), fv_note = character(0), method_used = character(0),
    primary_live = character(0), company_type = character(0),
    company_name = character(0),
    error = character(0),
    stringsAsFactors = FALSE
  )
  if (n == 0L) return(empty)
  do.call(rbind, rows)
}

#' Pure Blue Chip evaluation job for ExtendedTask / future workers.
#' All inputs are plain R objects (no reactives / session). Returns a list with
#' `scores`, optional `pool_note` (character), and meta fields for UI copy.
lab_im_run_eval_job <- function(catlg,
                                method_filter = NULL,
                                industry_filter = NULL,
                                include_adr = TRUE,
                                display_n = 25L,
                                eval_n = 25L,
                                rank_mode = "mcap",
                                concept_keys = NULL,
                                market_mode = "US",
                                locale = "zh-TW") {
  mm <- tryCatch(normalize_market_mode(market_mode), error = function(e) "US")
  tryCatch(set_market_mode(mm), error = function(e) NULL)
  loc <- tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
  if (!is.data.frame(catlg) || nrow(catlg) < 1L) {
    return(list(ok = FALSE, reason = "empty_catalog", scores = NULL))
  }
  pool <- lab_merge_catalog_scores(
    catlg,
    scores = NULL,
    method_filter = method_filter,
    industry_filter = industry_filter,
    eq_only = FALSE,
    gate_only = FALSE
  )
  pool <- lab_dedupe_eval_pool(pool)
  if (!identical(mm, "TW") && exists("lab_filter_pool_adr", mode = "function")) {
    pool <- lab_filter_pool_adr(pool, include_adr = isTRUE(include_adr))
  }
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) < 1L) {
    return(list(ok = FALSE, reason = "no_candidates", scores = NULL))
  }
  display_n <- lab_coerce_im_job_n(display_n)
  eval_n <- lab_coerce_im_job_n(eval_n)
  if (!is.finite(eval_n) || (is.finite(eval_n) && eval_n < 1L)) eval_n <- display_n
  rank_mode <- lab_normalize_pool_rank_mode(rank_mode %||% "mcap")
  n_raw <- nrow(pool)
  if (is.finite(eval_n) && n_raw > eval_n) {
    if (identical(rank_mode, "mcap") || identical(rank_mode, "concept")) {
      pool <- lab_attach_market_caps(pool)
    }
  }
  pool <- lab_select_eval_pool(
    pool,
    max_n = eval_n,
    mode = rank_mode,
    concept_keys = concept_keys,
    market_mode = mm,
    seed = as.integer(Sys.time())
  )
  n_filtered <- as.integer(attr(pool, "n_filtered") %||% nrow(pool))
  used_mcap <- isTRUE(attr(pool, "used_market_cap"))
  mode_used <- as.character(attr(pool, "pool_rank_mode") %||% rank_mode)[1]
  note <- as.character(attr(pool, "pool_rank_note") %||% "")[1]
  if (nrow(pool) < 1L) {
    return(list(ok = FALSE, reason = "no_candidates_ind_model", scores = NULL))
  }
  pool_note <- NULL
  if (is.finite(eval_n) && n_filtered > nrow(pool)) {
    how <- switch(
      mode_used,
      ret_1y = tryCatch(ui_str("lab_im_rank_how_ret_1y", loc), error = function(e) "by 1Y return"),
      random = tryCatch(ui_str("lab_im_rank_how_random", loc), error = function(e) "random"),
      concept = tryCatch(ui_str("lab_im_rank_how_concept", loc), error = function(e) "by concept"),
      if (used_mcap) {
        tryCatch(ui_str("lab_im_rank_how_mcap", loc), error = function(e) "by market cap")
      } else {
        tryCatch(ui_str("lab_im_rank_how_mcap_fallback", loc), error = function(e) "by market cap")
      }
    )
    if (grepl("fallback_mcap", note, fixed = TRUE)) {
      how <- paste0(
        how,
        tryCatch(ui_str("lab_im_rank_how_concept_fallback", loc), error = function(e) "")
      )
    }
    adr_note <- if (!identical(mm, "TW") && !isTRUE(include_adr)) {
      tryCatch(ui_str("lab_im_rank_adr_excluded", loc), error = function(e) "")
    } else {
      ""
    }
    disp_note <- if (is.finite(display_n) && is.finite(eval_n) && eval_n > display_n) {
      tryCatch(
        .ynow_fmt_str(ui_str("lab_im_rank_disp_note", loc), n = display_n),
        error = function(e) ""
      )
    } else {
      ""
    }
    pool_note <- tryCatch(
      .ynow_fmt_str(
        ui_str("lab_im_rank_pool_note", loc),
        n_filtered = n_filtered, how = how, n_eval = nrow(pool),
        adr = adr_note, disp = disp_note
      ),
      error = function(e) NULL
    )
  }
  cols <- intersect(c("ticker", "industry_key", "primary"), names(pool))
  scores <- lab_screen_tickers_fscore(
    pool[, cols, drop = FALSE],
    max_n = eval_n,
    progress_cb = NULL
  )
  list(
    ok = is.data.frame(scores) && nrow(scores) > 0L,
    reason = if (is.data.frame(scores) && nrow(scores) > 0L) "ok" else "empty_scores",
    scores = scores,
    pool_note = pool_note,
    display_n = display_n,
    market_mode = mm,
    locale = loc
  )
}

#' Query-filter keys → row hit.
#' Absolute `pb` = Justified P/B Fair Value (primary/secondary).
#' Relative `book` = Multiples reference family book (industry / history).
#' Enterprise / earnings / revenue match the industry Multiples reference family.
#' Legacy `multiples` still matches any relative family.
lab_method_filter_hits <- function(df, method_filter) {
  n <- if (is.data.frame(df)) nrow(df) else 0L
  if (n < 1L) return(logical(0))
  mf <- lab_normalize_multi_filter(method_filter)
  if (!length(mf)) return(rep(TRUE, n))
  legacy_mult <- "multiples" %in% mf
  mf <- setdiff(mf, "multiples")
  fv <- intersect(mf, c("nav", "dcf", "ddm", "ri", "pb", "sotp"))
  fam <- intersect(mf, c("enterprise", "earnings", "revenue", "book"))
  hit <- rep(FALSE, n)
  if (length(fv)) {
    hit <- hit | as.character(df$primary) %in% fv | as.character(df$secondary) %in% fv
  }
  if ("rel_family" %in% names(df)) {
    rel <- as.character(df$rel_family)
    if (length(fam)) hit <- hit | rel %in% fam
    if (legacy_mult) {
      hit <- hit | rel %in% c("enterprise", "earnings", "revenue", "book")
    }
  }
  hit
}

#' 合併目錄與評估結果，複選篩選後依年化估值漲幅排序
#' @param evaluated_only TRUE：只保留已評估代碼（明細／排行榜與評估檔數 N 對齊）
lab_merge_catalog_scores <- function(catalog, scores = NULL,
                                     method_filter = character(0),
                                     industry_filter = character(0),
                                     eq_only = FALSE,
                                     gate_only = FALSE,
                                     no_alert = FALSE,
                                     quality_only = FALSE,
                                     evaluated_only = FALSE) {
  # quality_only：舊「只看通過」別名 → Piotroski 高門檻（F-Score≥7；不含盈餘品質）
  if (isTRUE(quality_only)) gate_only <- TRUE
  df <- catalog
  score_cols_na <- function(d) {
    d$ok <- NA
    d$f_score <- NA_real_
    d$quality_flag <- NA_real_
    d$n_fs_alerts <- NA_integer_
    d$is_quality <- NA
    d$is_quality_upside <- NA
    d$market_cap <- NA_real_
    d$price <- NA_real_
    d$fv <- NA_real_
    d$upside_total_pct <- NA_real_
    d$upside_cagr_pct <- NA_real_
    d$n_years <- NA_integer_
    d$fv_note <- NA_character_
    d$method_used <- NA_character_
    d$primary_live <- NA_character_
    d$company_type <- NA_character_
    d$company_name <- NA_character_
    d$error <- NA_character_
    d
  }
  has_scores <- !is.null(scores) && is.data.frame(scores) && nrow(scores) > 0
  .coalesce_merge_mcap <- function(d) {
    if ("market_cap" %in% names(d)) return(d)
    if ("market_cap.y" %in% names(d) || "market_cap.x" %in% names(d)) {
      my <- if ("market_cap.y" %in% names(d)) {
        suppressWarnings(as.numeric(d$market_cap.y))
      } else {
        rep(NA_real_, nrow(d))
      }
      mx <- if ("market_cap.x" %in% names(d)) {
        suppressWarnings(as.numeric(d$market_cap.x))
      } else {
        rep(NA_real_, nrow(d))
      }
      d$market_cap <- ifelse(is.finite(my) & my > 0, my, mx)
      drop <- intersect(c("market_cap.x", "market_cap.y"), names(d))
      if (length(drop)) d[drop] <- NULL
    }
    d
  }
  if (isTRUE(evaluated_only)) {
    if (!has_scores) {
      df <- score_cols_na(df)
      return(df[0, , drop = FALSE])
    }
    df <- merge(df, scores, by = "ticker", all.x = FALSE, all.y = FALSE, sort = FALSE)
    df <- df[!is.na(df$ticker) & nzchar(as.character(df$ticker)), , drop = FALSE]
    df <- df[!duplicated(df$ticker), , drop = FALSE]
    df <- .coalesce_merge_mcap(df)
  } else if (has_scores) {
    df <- merge(df, scores, by = "ticker", all.x = TRUE, sort = FALSE)
    df <- .coalesce_merge_mcap(df)
  } else {
    df <- score_cols_na(df)
  }

  mf <- lab_normalize_multi_filter(method_filter)
  if (length(mf) > 0) {
    df <- df[lab_method_filter_hits(df, mf), , drop = FALSE]
  }
  indf <- lab_normalize_multi_filter(industry_filter)
  if (length(indf) > 0) {
    df <- df[df$industry_key %in% indf, , drop = FALSE]
  }
  if (isTRUE(eq_only)) {
    df <- df[!is.na(df$quality_flag) & df$quality_flag %in% 1, , drop = FALSE]
  }
  if (isTRUE(gate_only)) {
    # Piotroski 高門檻：僅 F-Score≥7（與盈餘品質無關）
    fs <- suppressWarnings(as.numeric(df$f_score))
    df <- df[is.finite(fs) & fs >= 7, , drop = FALSE]
  }
  if (isTRUE(no_alert) && "n_fs_alerts" %in% names(df)) {
    # 無財報警訊：Schilit／盈餘品質紅旗合計為 0（與 YNOW 財報警訊一致）
    na_n <- suppressWarnings(as.numeric(df$n_fs_alerts))
    df <- df[is.finite(na_n) & na_n <= 0, , drop = FALSE]
  }
  if (nrow(df) == 0L) return(df)
  # 年化估值漲幅由大到小（NA 置後）— 即「未來期間漲幅最大者」優先
  ind_lab <- if ("industry_label" %in% names(df)) df$industry_label else rep("", nrow(df))
  o <- order(is.na(df$upside_cagr_pct), -df$upside_cagr_pct, ind_lab, df$ticker,
             na.last = TRUE)
  df[o, , drop = FALSE]
}

#' Tickers in the Blue Chip query-filter pool (pre-truncate), one row per ticker.
#' Industry × model filters, then optional US ADR exclude. Board filter is
#' assumed already applied when building `catalog`.
lab_structural_filter_pool <- function(catalog,
                                       method_filter = character(0),
                                       industry_filter = character(0),
                                       include_adr = TRUE,
                                       market_mode = "US") {
  empty <- data.frame(
    ticker = character(0),
    industry_key = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(catalog) || !is.data.frame(catalog) || nrow(catalog) < 1L) {
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
  mm <- tryCatch(
    normalize_market_mode(market_mode),
    error = function(e) as.character(market_mode %||% "US")[1]
  )
  if (!identical(mm, "TW") &&
      exists("lab_filter_pool_adr", mode = "function") &&
      !isTRUE(include_adr)) {
    pool <- lab_filter_pool_adr(pool, include_adr = FALSE)
  }
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) < 1L ||
      !"ticker" %in% names(pool)) {
    return(empty)
  }
  pool$ticker <- as.character(pool$ticker)
  pool <- pool[!is.na(pool$ticker) & nzchar(pool$ticker), , drop = FALSE]
  if (!nrow(pool)) return(empty)
  if (!"industry_key" %in% names(pool)) pool$industry_key <- NA_character_
  pool <- pool[!duplicated(pool$ticker), c("ticker", "industry_key"), drop = FALSE]
  rownames(pool) <- NULL
  pool
}

#' Count unique tickers in the Blue Chip query-filter pool (pre-truncate).
#' @return integer count (≥ 0)
lab_count_filter_pool_tickers <- function(catalog,
                                          method_filter = character(0),
                                          industry_filter = character(0),
                                          include_adr = TRUE,
                                          market_mode = "US") {
  as.integer(nrow(lab_structural_filter_pool(
    catalog,
    method_filter = method_filter,
    industry_filter = industry_filter,
    include_adr = include_adr,
    market_mode = market_mode
  )))
}

