# ==========================================
# lab_industry_method.R — 實驗區：產業 × 評價方法 × 美股績優候選
#
# 績優原則：在 F-Score＋盈餘品質過門檻後，選「模型合理價相對現價」、
# 並依 App 預設預測年數 n（APP_DEFAULTS$years）換算年化漲幅最大者。
# 產業建議方法對齊 recommend_valuation_models 的產業層規則（簡化估值）。
# ==========================================

LAB_METHOD_LABELS <- c(
  dcf = "DCF（現金流折現）",
  ddm = "DDM（股利折現）",
  pb  = "P/B（帳面價值／資產）",
  ri  = "RI（剩餘收益）"
)

# 市值分級（USD）：大型 ≥100B／中型 10–100B／小型 ＜10B
LAB_SIZE_LABELS <- c(
  large = "大型（≥100B）",
  mid   = "中型（10–100B）",
  small = "小型（＜10B）"
)

# 目錄代碼 → 公司全稱（評估前即可顯示；Yahoo 名稱可覆寫）
LAB_TICKER_NAMES <- c(
  AVGO = "Broadcom Inc.", AMD = "Advanced Micro Devices, Inc.",
  QCOM = "QUALCOMM Incorporated", NVDA = "NVIDIA Corporation",
  TSM = "Taiwan Semiconductor Manufacturing Company Limited",
  AMKR = "Amkor Technology, Inc.", MU = "Micron Technology, Inc.",
  AMAT = "Applied Materials, Inc.", LRCX = "Lam Research Corporation",
  KLAC = "KLA Corporation", ASML = "ASML Holding N.V.",
  MSFT = "Microsoft Corporation", ORCL = "Oracle Corporation",
  ADBE = "Adobe Inc.", CRM = "Salesforce, Inc.",
  NOW = "ServiceNow, Inc.", SNOW = "Snowflake Inc.",
  GOOGL = "Alphabet Inc.", META = "Meta Platforms, Inc.",
  AMZN = "Amazon.com, Inc.", AAPL = "Apple Inc.",
  TXN = "Texas Instruments Incorporated", ADI = "Analog Devices, Inc.",
  JPM = "JPMorgan Chase & Co.", BAC = "Bank of America Corporation",
  WFC = "Wells Fargo & Company", GS = "The Goldman Sachs Group, Inc.",
  MS = "Morgan Stanley", PGR = "The Progressive Corporation",
  CB = "Chubb Limited", AIG = "American International Group, Inc.",
  BLK = "BlackRock, Inc.", BX = "Blackstone Inc.",
  V = "Visa Inc.", MA = "Mastercard Incorporated",
  SQ = "Block, Inc.", `BRK-B` = "Berkshire Hathaway Inc.",
  SHOP = "Shopify Inc.", COST = "Costco Wholesale Corporation",
  WMT = "Walmart Inc.", HD = "The Home Depot, Inc.",
  PEP = "PepsiCo, Inc.", KO = "The Coca-Cola Company",
  PG = "The Procter & Gamble Company", CL = "Colgate-Palmolive Company",
  EL = "The Estée Lauder Companies Inc.", LULU = "lululemon athletica inc.",
  TPR = "Tapestry, Inc.", NKE = "NIKE, Inc.",
  SBUX = "Starbucks Corporation", GM = "General Motors Company",
  F = "Ford Motor Company", TSLA = "Tesla, Inc.",
  APTV = "Aptiv PLC", BWA = "BorgWarner Inc.",
  RIVN = "Rivian Automotive, Inc.", LCID = "Lucid Group, Inc.",
  UNH = "UnitedHealth Group Incorporated", CI = "The Cigna Group",
  LLY = "Eli Lilly and Company", JNJ = "Johnson & Johnson",
  MRK = "Merck & Co., Inc.", PFE = "Pfizer Inc.",
  ABT = "Abbott Laboratories", MDT = "Medtronic plc",
  ISRG = "Intuitive Surgical, Inc.", VRTX = "Vertex Pharmaceuticals Incorporated",
  REGN = "Regeneron Pharmaceuticals, Inc.", AMGN = "Amgen Inc.",
  CAT = "Caterpillar Inc.", DE = "Deere & Company",
  RTX = "RTX Corporation", LMT = "Lockheed Martin Corporation",
  BA = "The Boeing Company", VMC = "Vulcan Materials Company",
  MLM = "Martin Marietta Materials, Inc.", LIN = "Linde plc",
  APD = "Air Products and Chemicals, Inc.", FCX = "Freeport-McMoRan Inc.",
  NEM = "Newmont Corporation", XOM = "Exxon Mobil Corporation",
  CVX = "Chevron Corporation", NEE = "NextEra Energy, Inc.",
  DUK = "Duke Energy Corporation", ENPH = "Enphase Energy, Inc.",
  FSLR = "First Solar, Inc.", VZ = "Verizon Communications Inc.",
  T = "AT&T Inc.", UNP = "Union Pacific Corporation",
  UPS = "United Parcel Service, Inc.", FDX = "FedEx Corporation",
  DAL = "Delta Air Lines, Inc.", UAL = "United Airlines Holdings, Inc.",
  PLD = "Prologis, Inc.", AMT = "American Tower Corporation",
  O = "Realty Income Corporation", DIS = "The Walt Disney Company",
  NFLX = "Netflix, Inc.", RBLX = "Roblox Corporation",
  EA = "Electronic Arts Inc.", MAR = "Marriott International, Inc.",
  BKNG = "Booking Holdings Inc.", ABNB = "Airbnb, Inc."
)

#' 公司全稱：Yahoo 名稱優先，否則目錄對照
lab_company_display_name <- function(ticker, yahoo_name = NULL) {
  tk <- toupper(gsub("\\.", "-", trimws(as.character(ticker %||% "")[1])))
  yn <- trimws(as.character(yahoo_name %||% "")[1])
  if (is.na(yn)) yn <- ""
  if (nzchar(yn) && !identical(toupper(yn), tk)) return(yn)
  nm <- unname(LAB_TICKER_NAMES[tk])
  if (!is.na(nm) && nzchar(nm)) return(as.character(nm)[1])
  if (nzchar(yn)) return(yn)
  "—"
}

#' 依市值（USD）分級；無法判定則 NA
lab_classify_market_cap <- function(mcap_usd) {
  x <- suppressWarnings(as.numeric(mcap_usd)[1])
  if (!is.finite(x) || x <= 0) return(NA_character_)
  if (x >= 1e11) return("large")
  if (x >= 1e10) return("mid")
  "small"
}

#' 模型預測年數（與 App DCF／預設 n 對齊）
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

#' 自 Yahoo Summary 取市值、現價與公司全稱
lab_fetch_summary_metrics <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  out <- list(market_cap = NA_real_, price = NA_real_, company_name = NA_character_)
  if (!nzchar(tk)) return(out)
  sum_df <- tryCatch(get_summary_data(tk), error = function(e) NULL)
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
  cname <- tryCatch(attr(sum_df, "company_name"), error = function(e) NULL)
  cname <- trimws(as.character(cname %||% "")[1])
  if (nzchar(cname)) out$company_name <- cname
  out
}

#' 相容舊呼叫
lab_fetch_market_cap_usd <- function(ticker) {
  lab_fetch_summary_metrics(ticker)$market_cap
}

#' 正規化複選篩選：NULL／空／含 "all" → character(0) 表示不過濾
lab_normalize_multi_filter <- function(x) {
  v <- unique(as.character(unlist(x, use.names = FALSE)))
  v <- v[nzchar(v) & !is.na(v) & v != "all"]
  v
}

#' 規模複選 → size_band keys（相容誤傳中文標籤）
lab_normalize_size_filter <- function(x) {
  v <- lab_normalize_multi_filter(x)
  if (length(v) == 0L) return(character(0))
  # 已是 key
  keys <- names(LAB_SIZE_LABELS)
  labels <- unname(LAB_SIZE_LABELS)
  out <- character(0)
  for (item in v) {
    if (item %in% keys) {
      out <- c(out, item)
    } else if (item %in% labels) {
      out <- c(out, keys[match(item, labels)])
    }
  }
  unique(out[!is.na(out) & nzchar(out)])
}

#' picker 用：顯示中文、回傳 large/mid/small
lab_size_picker_choices <- function() {
  stats::setNames(names(LAB_SIZE_LABELS), unname(LAB_SIZE_LABELS))
}

#' 產業 CAPM Ke（%）
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
lab_estimate_fv_per_share <- function(method, industry_key, d_is, d_bs, d_cf,
                                      price = NA_real_, market_cap = NA_real_) {
  method <- as.character(method %||% "pb")[1]
  key <- as.character(industry_key %||% "")[1]
  n <- lab_model_horizon_years()
  ke_pct <- lab_industry_ke_pct(key)
  ke <- ke_pct / 100
  g_pct <- suppressWarnings(as.numeric(APP_DEFAULTS$sgr %||% 4)[1])
  if (!is.finite(g_pct)) g_pct <- 4
  g <- max(0.005, min(g_pct / 100, ke - 0.015))
  g1_pct <- suppressWarnings(as.numeric(APP_DEFAULTS$custom_g %||% g_pct)[1])
  if (!is.finite(g1_pct)) g1_pct <- g_pct
  g1 <- max(0.01, min(g1_pct / 100, 0.15))

  shares <- tryCatch(
    select_current_metric(d_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock"),
    error = function(e) NA_real_
  )
  shares <- suppressWarnings(as.numeric(shares)[1])
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
  } else {
    # DCF：n 年顯式 FCF 成長＋終值（用 Ke 近似折現；實驗區簡化）
    fcf <- tryCatch(
      select_current_metric(d_cf, "Free Cash Flow", "flow"),
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
    n_years = n,
    note = note,
    ke_pct = ke_pct
  )
}

#' 產業預設主／副評價方法（無個股財報時的規則映射）
#' @return data.frame: industry_key, primary, secondary, suggest_two_stage, rationale
lab_industry_method_defaults <- function() {
  rows <- list(
    # 金融／控股／REIT／公用 → P/B（+ RI）
    list("fn.Banking", "pb", "ri", FALSE, "金融簿價驅動 → P/B；ROE 可時交叉 RI"),
    list("fn.Investment_Banking", "pb", "ri", FALSE, "金融簿價驅動 → P/B"),
    list("fn.Insurance", "pb", "ri", FALSE, "保險／帳面導向 → P/B"),
    list("fn.Asset_Management", "pb", "ri", FALSE, "資產管理偏帳面／AUM → P/B"),
    list("fn.Fintech", "dcf", "pb", TRUE, "成長型金融科技 → 兩階段 DCF"),
    list("fn.Conglomerate_Holding", "pb", "ri", FALSE, "控股／綜合企業 → P/B（+ RI）"),
    list("re.REIT", "pb", "ddm", FALSE, "REIT 簿價／殖利率 → P/B；穩定股利可輔 DDM"),
    list("en.Utilities", "pb", "ddm", FALSE, "公用事業簿價／管制資產 → P/B；高配息輔 DDM"),

    # 高成長科技／生技／EV → DCF two-stage
    list("saas.SaaS_Cloud", "dcf", "pb", TRUE, "SaaS／雲端成長 → 兩階段 DCF"),
    list("tech.Internet_Platform", "dcf", "pb", TRUE, "網路平台成長 → 兩階段 DCF"),
    list("tech.Software", "dcf", "ri", TRUE, "軟體成長／穩健 FCF → DCF"),
    list("hc.Biotech", "dcf", "pb", TRUE, "生技早期成長 → 兩階段 DCF（風險高）"),
    list("auto.Automotive_EV", "dcf", "pb", TRUE, "電動車成長 → 兩階段 DCF"),
    list("auto.EV_Startups", "dcf", "pb", TRUE, "新創 EV → 兩階段 DCF"),
    list("en.Renewables", "dcf", "pb", TRUE, "再生能源成長 → 兩階段 DCF"),

    # 半導體／硬體
    list("sc.IC_Design", "dcf", "pb", TRUE, "IC 設計常具成長與 FCF → DCF"),
    list("sc.Foundry", "dcf", "ri", FALSE, "晶圓代工成熟資本密集 → DCF"),
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
    list("en.Energy_OilGas", "dcf", "ddm", FALSE, "油氣巨頭 FCF／配息 → DCF"),
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
  df$industry_label <- as.character(industry_labels[df$industry_key])
  df$primary_label <- as.character(LAB_METHOD_LABELS[df$primary])
  df$secondary_label <- ifelse(
    is.na(df$secondary) | !nzchar(df$secondary),
    "",
    as.character(LAB_METHOD_LABELS[df$secondary])
  )
  df[order(df$primary, df$industry_label), , drop = FALSE]
}

#' 各產業美股績優候選（人工 curated；含 App presets 美股＋常見藍籌）
#' @return named list: industry_key → character vector of US tickers
lab_us_quality_candidates <- function() {
  list(
    "sc.IC_Design" = c("AVGO", "AMD", "QCOM", "NVDA"),
    "sc.Foundry" = c("TSM"),
    "sc.Packaging" = c("AMKR"),
    "sc.Memory" = c("MU"),
    "sc.Equipment" = c("AMAT", "LRCX", "KLAC", "ASML"),
    "tech.Software" = c("MSFT", "ORCL", "ADBE"),
    "saas.SaaS_Cloud" = c("MSFT", "CRM", "NOW", "SNOW"),
    "tech.Internet_Platform" = c("GOOGL", "META", "AMZN"),
    "tech.Hardware" = c("AAPL"),
    "ec.Hardware" = c("TXN", "ADI"),
    "fn.Banking" = c("JPM", "BAC", "WFC"),
    "fn.Investment_Banking" = c("GS", "MS"),
    "fn.Insurance" = c("PGR", "CB", "AIG"),
    "fn.Asset_Management" = c("BLK", "BX"),
    "fn.Fintech" = c("V", "MA", "SQ"),
    "fn.Conglomerate_Holding" = c("BRK-B"),
    "ecr.Ecommerce_Retail" = c("AMZN", "SHOP"),
    "retail.Brick_Mortar" = c("COST", "WMT", "HD"),
    "fmcg.Food_Beverages" = c("PEP", "KO", "COST"),
    "fmcg.Household_Personal" = c("PG", "CL"),
    "fmcg.Health_Beauty" = c("EL", "PG"),
    "lxg.Luxury_Fashion" = c("LULU", "TPR"),
    "cons.Discretionary" = c("NKE", "SBUX"),
    "auto.Vehicle_Manufacturing" = c("GM", "F"),
    "auto.Automotive_EV" = c("TSLA"),
    "auto.Parts_Suppliers" = c("APTV", "BWA"),
    "auto.EV_Startups" = c("RIVN", "LCID"),
    "hc.Healthcare_Services" = c("UNH", "CI"),
    "hc.Pharma" = c("LLY", "JNJ", "MRK", "PFE"),
    "hc.Medtech" = c("ABT", "MDT", "ISRG"),
    "hc.Biotech" = c("VRTX", "REGN", "AMGN"),
    "ind.Machinery" = c("CAT", "DE"),
    "ind.Aerospace_Defense" = c("RTX", "LMT", "BA"),
    "ind.Construction" = c("VMC", "MLM"),
    "mat.Chemicals" = c("LIN", "APD"),
    "mat.Metals_Mining" = c("FCX", "NEM"),
    "en.Energy_OilGas" = c("XOM", "CVX"),
    "en.Utilities" = c("NEE", "DUK"),
    "en.Renewables" = c("ENPH", "FSLR"),
    "tel.Telecom" = c("VZ", "T"),
    "tr.Logistics_Shipping" = c("UNP", "UPS", "FDX"),
    "tr.Airlines" = c("DAL", "UAL"),
    "re.REIT" = c("PLD", "AMT", "O"),
    "media.Entertainment" = c("DIS", "NFLX"),
    "media.Gaming" = c("RBLX", "EA"),
    "hosp.Hotels_Travel" = c("MAR", "BKNG", "ABNB")
  )
}

#' 展開為一列一檔的產業×方法×候選表
lab_build_industry_method_catalog <- function() {
  defaults <- lab_industry_method_defaults()
  cands <- lab_us_quality_candidates()
  rows <- list()
  for (i in seq_len(nrow(defaults))) {
    key <- defaults$industry_key[[i]]
    tickers <- unique(as.character(cands[[key]] %||% character(0)))
    tickers <- tickers[nzchar(tickers)]
    # 排除台股與常見指數 ETF
    tickers <- tickers[!grepl("\\.TW$|\\.TWO$", tickers, ignore.case = TRUE)]
    tickers <- setdiff(tickers, c("SPY", "QQQ", "DIA", "IWM"))
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
          ticker = tk,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}

#' 是否通過 F-Score 品質門檻（績優候選前置條件，非最終排序鍵）
lab_is_quality_stock <- function(f_score, quality_flag, min_score = 7L) {
  fs <- suppressWarnings(as.numeric(f_score)[1])
  qf <- suppressWarnings(as.numeric(quality_flag)[1])
  isTRUE(is.finite(fs) && fs >= as.numeric(min_score) && isTRUE(qf == 1))
}

#' 評估單一美股：F-Score 門檻＋產業主方法簡化合理價＋n 年年化漲幅
lab_evaluate_ticker_fscore <- function(ticker, industry_key = NULL, method = NULL) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  n_yrs <- lab_model_horizon_years()
  out <- list(
    ticker = tk,
    ok = FALSE,
    f_score = NA_real_,
    quality_flag = NA_real_,
    is_quality = FALSE,
    market_cap = NA_real_,
    size_band = NA_character_,
    price = NA_real_,
    fv = NA_real_,
    upside_total_pct = NA_real_,
    upside_cagr_pct = NA_real_,
    n_years = n_yrs,
    fv_note = NA_character_,
    company_type = NA_character_,
    company_name = NA_character_,
    primary_live = NA_character_,
    method_used = as.character(method %||% NA_character_)[1],
    is_quality_upside = FALSE,
    error = NULL
  )
  if (!nzchar(tk)) {
    out$error <- "空代碼"
    return(out)
  }

  sm <- tryCatch(lab_fetch_summary_metrics(tk), error = function(e) {
    list(market_cap = NA_real_, price = NA_real_, company_name = NA_character_)
  })
  out$market_cap <- suppressWarnings(as.numeric(sm$market_cap)[1])
  out$price <- suppressWarnings(as.numeric(sm$price)[1])
  out$company_name <- lab_company_display_name(tk, sm$company_name)
  out$size_band <- lab_classify_market_cap(out$market_cap)

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
      price = out$price, market_cap = out$market_cap
    ),
    error = function(e) list(fv = NA_real_, n_years = n_yrs, note = e$message)
  )
  out$fv <- suppressWarnings(as.numeric(fv_res$fv)[1])
  out$n_years <- suppressWarnings(as.integer(fv_res$n_years %||% n_yrs)[1])
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
lab_screen_tickers_fscore <- function(tickers, progress_cb = NULL, max_n = 40L,
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
  drop_tw <- grepl("\\.TW$|\\.TWO$", tks)
  tks <- tks[!drop_tw]; ind_keys <- ind_keys[!drop_tw]; meths <- meths[!drop_tw]

  # 同一代碼只評估一次（取第一筆產業／方法）
  ord <- !duplicated(tks)
  tks <- tks[ord]; ind_keys <- ind_keys[ord]; meths <- meths[ord]

  if (length(tks) > as.integer(max_n)) {
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
      is_quality = isTRUE(ev$is_quality),
      is_quality_upside = isTRUE(ev$is_quality_upside),
      market_cap = ev$market_cap,
      size_band = ev$size_band %||% NA_character_,
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
    quality_flag = numeric(0), is_quality = logical(0),
    is_quality_upside = logical(0),
    market_cap = numeric(0), size_band = character(0),
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

#' 合併目錄與評估結果，複選篩選後依年化估值漲幅排序
lab_merge_catalog_scores <- function(catalog, scores = NULL,
                                     method_filter = character(0),
                                     industry_filter = character(0),
                                     size_filter = character(0),
                                     quality_only = FALSE) {
  df <- catalog
  score_cols_na <- function(d) {
    d$ok <- NA
    d$f_score <- NA_real_
    d$quality_flag <- NA_real_
    d$is_quality <- NA
    d$is_quality_upside <- NA
    d$market_cap <- NA_real_
    d$size_band <- NA_character_
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
  if (!is.null(scores) && is.data.frame(scores) && nrow(scores) > 0) {
    df <- merge(df, scores, by = "ticker", all.x = TRUE, sort = FALSE)
  } else {
    df <- score_cols_na(df)
  }

  mf <- lab_normalize_multi_filter(method_filter)
  if (length(mf) > 0) {
    df <- df[df$primary %in% mf | df$secondary %in% mf, , drop = FALSE]
  }
  indf <- lab_normalize_multi_filter(industry_filter)
  if (length(indf) > 0) {
    df <- df[df$industry_key %in% indf, , drop = FALSE]
  }
  sf <- lab_normalize_size_filter(size_filter)
  if (length(sf) > 0) {
    df <- df[!is.na(df$size_band) & df$size_band %in% sf, , drop = FALSE]
  }
  if (isTRUE(quality_only)) {
    # 績優顯示池：F-Score 門檻通過（再以年化漲幅決選排序）
    df <- df[!is.na(df$is_quality) & df$is_quality %in% TRUE, , drop = FALSE]
  }
  # 年化估值漲幅由大到小（NA 置後）— 即「未來期間漲幅最大者」優先
  o <- order(is.na(df$upside_cagr_pct), -df$upside_cagr_pct, df$industry_label, df$ticker,
             na.last = TRUE)
  df[o, , drop = FALSE]
}

#' 績優排行榜：門檻通過且有年化漲幅，一代碼一列，依 CAGR 降序
lab_quality_leaderboard <- function(merged_df, top_n = 10L) {
  if (is.null(merged_df) || !is.data.frame(merged_df) || nrow(merged_df) == 0) {
    return(data.frame(
      排名 = integer(0), 代碼 = character(0), 年化估值漲幅 = character(0),
      總潛在漲幅 = character(0),
      估值方法 = character(0), `F-Score` = numeric(0),
      規模 = character(0), 產業 = character(0),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  df <- merged_df
  keep <- !is.na(df$is_quality) & df$is_quality %in% TRUE &
    is.finite(df$upside_cagr_pct)
  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0) {
    return(data.frame(
      排名 = integer(0), 代碼 = character(0), 年化估值漲幅 = character(0),
      總潛在漲幅 = character(0),
      估值方法 = character(0), `F-Score` = numeric(0),
      規模 = character(0), 產業 = character(0),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  # 同一代碼保留年化漲幅最高的一列
  df <- df[order(-df$upside_cagr_pct, df$ticker), , drop = FALSE]
  df <- df[!duplicated(df$ticker), , drop = FALSE]
  top_n <- max(1L, as.integer(top_n)[1])
  df <- head(df, top_n)
  size_lab <- unname(LAB_SIZE_LABELS[df$size_band])
  size_lab[is.na(size_lab)] <- "—"
  meth <- as.character(df$method_used)
  prim <- as.character(df$primary)
  miss <- is.na(meth) | !nzchar(meth)
  meth[miss] <- prim[miss]
  meth <- toupper(meth)
  meth[is.na(meth) | !nzchar(meth)] <- "—"
  data.frame(
    排名 = seq_len(nrow(df)),
    代碼 = df$ticker,
    年化估值漲幅 = sprintf("%+.1f%%", df$upside_cagr_pct),
    總潛在漲幅 = ifelse(
      is.na(df$upside_total_pct), "—", sprintf("%+.1f%%", df$upside_total_pct)
    ),
    估值方法 = meth,
    `F-Score` = df$f_score,
    規模 = size_lab,
    產業 = df$industry_label,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' 顯示用摘要：依主方法分組的產業數／候選檔數
lab_method_group_summary <- function(catalog) {
  if (is.null(catalog) || !nrow(catalog)) {
    return(data.frame(
      建議評價方法 = character(0),
      產業數 = integer(0),
      候選檔數 = integer(0),
      method_key = character(0),
      stringsAsFactors = FALSE
    ))
  }
  methods <- unique(as.character(catalog$primary))
  do.call(rbind, lapply(methods, function(m) {
    sub <- catalog[catalog$primary == m, , drop = FALSE]
    data.frame(
      建議評價方法 = as.character(LAB_METHOD_LABELS[[m]] %||% m),
      產業數 = length(unique(sub$industry_key)),
      候選檔數 = length(unique(stats::na.omit(sub$ticker))),
      method_key = m,
      stringsAsFactors = FALSE
    )
  }))
}

#' 明細表：F-Score／盈餘品質／門檻 分開著色
lab_html_fscore_pill <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(v),
    '<span class="ynow-lab-pill ynow-lab-pill-muted">—</span>',
    sprintf('<span class="ynow-lab-pill ynow-lab-pill-fs">%s</span>', as.integer(round(v)))
  )
}

lab_html_eq_pill <- function(quality_flag) {
  qf <- suppressWarnings(as.numeric(quality_flag))
  ifelse(
    is.na(qf),
    '<span class="ynow-lab-pill ynow-lab-pill-muted">未評</span>',
    ifelse(
      qf == 1,
      '<span class="ynow-lab-pill ynow-lab-pill-eq-ok">盈餘通過</span>',
      '<span class="ynow-lab-pill ynow-lab-pill-eq-no">盈餘未過</span>'
    )
  )
}

lab_html_gate_pill <- function(is_quality) {
  ifelse(
    is.na(is_quality),
    '<span class="ynow-lab-pill ynow-lab-pill-muted">未評估</span>',
    ifelse(
      is_quality %in% TRUE,
      '<span class="ynow-lab-pill ynow-lab-pill-gate-ok">門檻通過</span>',
      '<span class="ynow-lab-pill ynow-lab-pill-gate-no">未達門檻</span>'
    )
  )
}
