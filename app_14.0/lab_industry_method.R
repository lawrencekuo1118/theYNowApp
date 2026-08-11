# ==========================================
# lab_industry_method.R — 實驗區：產業 × 評價方法 × 美股績優候選
# 產業建議方法對齊 setup.R::recommend_valuation_models 的產業層規則；
# 個股仍可能因 FCF／股利／成長而不同，故標為「產業預設」。
# 績優判定對齊 compute_report_f_score（F-Score + 盈餘品質）。
# ==========================================

LAB_METHOD_LABELS <- c(
  dcf = "DCF（現金流折現）",
  ddm = "DDM（股利折現）",
  pb  = "P/B（帳面價值／資產）",
  ri  = "RI（剩餘收益）"
)

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

#' 是否視為 F-Score 風格「績優」
#' 對齊決策漏斗：需通過盈餘品質（quality_flag）；分數採偏嚴門檻。
lab_is_quality_stock <- function(f_score, quality_flag, min_score = 7L) {
  fs <- suppressWarnings(as.numeric(f_score)[1])
  qf <- suppressWarnings(as.numeric(quality_flag)[1])
  isTRUE(is.finite(fs) && fs >= as.numeric(min_score) && isTRUE(qf == 1))
}

#' 針對單一美股抓財報並算 F-Score（沿用 cached_scrape_financials）
lab_evaluate_ticker_fscore <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  out <- list(
    ticker = tk,
    ok = FALSE,
    f_score = NA_real_,
    quality_flag = NA_real_,
    is_quality = FALSE,
    company_type = NA_character_,
    primary_live = NA_character_,
    error = NULL
  )
  if (!nzchar(tk)) {
    out$error <- "空代碼"
    return(out)
  }
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

  # 可選：用個股財報驗證「即時建議方法」（不覆寫產業預設，僅供參考）
  rec <- tryCatch(
    recommend_valuation_models(d_cf, industry_text = "", d_is = d_is, d_bs = d_bs),
    error = function(e) NULL
  )
  if (!is.null(rec)) {
    out$company_type <- as.character(rec$company_type %||% NA_character_)
    out$primary_live <- as.character(rec$primary %||% NA_character_)
  }
  out$ok <- isTRUE(is.finite(out$f_score))
  if (!out$ok && is.null(out$error)) out$error <- "F-Score 無法計算"
  out
}

#' 批次評估；progress_cb(i, n, ticker) 可選
lab_screen_tickers_fscore <- function(tickers, progress_cb = NULL, max_n = 40L) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  tks <- tks[!grepl("\\.TW$|\\.TWO$", tks)]
  if (length(tks) > as.integer(max_n)) {
    tks <- head(tks, as.integer(max_n))
  }
  n <- length(tks)
  rows <- vector("list", n)
  for (i in seq_along(tks)) {
    if (is.function(progress_cb)) {
      tryCatch(progress_cb(i, n, tks[[i]]), error = function(e) NULL)
    }
    ev <- lab_evaluate_ticker_fscore(tks[[i]])
    rows[[i]] <- data.frame(
      ticker = ev$ticker,
      ok = isTRUE(ev$ok),
      f_score = ev$f_score,
      quality_flag = ev$quality_flag,
      is_quality = isTRUE(ev$is_quality),
      primary_live = ev$primary_live %||% NA_character_,
      company_type = ev$company_type %||% NA_character_,
      error = ev$error %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (n == 0L) {
    return(data.frame(
      ticker = character(0), ok = logical(0), f_score = numeric(0),
      quality_flag = numeric(0), is_quality = logical(0),
      primary_live = character(0), company_type = character(0),
      error = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' 合併目錄與 F-Score 結果，組 UI 顯示表
lab_merge_catalog_scores <- function(catalog, scores = NULL,
                                     method_filter = "all",
                                     quality_only = FALSE) {
  df <- catalog
  if (!is.null(scores) && is.data.frame(scores) && nrow(scores) > 0) {
    df <- merge(
      df, scores,
      by = "ticker", all.x = TRUE, sort = FALSE
    )
  } else {
    df$ok <- NA
    df$f_score <- NA_real_
    df$quality_flag <- NA_real_
    df$is_quality <- NA
    df$primary_live <- NA_character_
    df$company_type <- NA_character_
    df$error <- NA_character_
  }
  if (!identical(method_filter, "all") && nzchar(as.character(method_filter))) {
    mf <- as.character(method_filter)[1]
    df <- df[df$primary == mf | df$secondary == mf, , drop = FALSE]
  }
  if (isTRUE(quality_only)) {
    df <- df[isTRUE(df$is_quality) | (!is.na(df$is_quality) & df$is_quality), , drop = FALSE]
    # R logical NA handling
    df <- df[!is.na(df$is_quality) & df$is_quality, , drop = FALSE]
  }
  df <- df[order(df$primary, df$industry_label, df$ticker), , drop = FALSE]
  df
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
