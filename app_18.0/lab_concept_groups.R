# ==========================================
# lab_concept_groups.R — 常見概念股群（研究用；非投資建議）
#
# US／TW 各一組；代號與宇宙對齊（台股用 .TW／.TWO）。
# 「概念股」模式：所選群聯集 ∩ 目前篩選池；若仍 > N 再依市值截斷。
# ==========================================

#' Concept-group catalog (labels are locale-neutral keys; UI maps via ui_locale)
LAB_CONCEPT_GROUPS <- list(
  US = list(
    mag7 = c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA", "TSLA"),
    semis = c(
      "NVDA", "AVGO", "AMD", "QCOM", "TXN", "ADI", "INTC", "MU", "AMAT", "LRCX",
      "KLAC", "ASML", "TSM", "AMKR", "MRVL", "NXPI", "ON", "SWKS", "MCHP"
    ),
    ai_infra = c(
      "NVDA", "AMD", "AVGO", "MSFT", "GOOGL", "AMZN", "META", "ORCL", "CRM",
      "NOW", "SNOW", "PLTR", "SMCI", "DELL", "ANET", "CSCO"
    ),
    cloud_saas = c(
      "MSFT", "AMZN", "GOOGL", "ORCL", "CRM", "NOW", "ADBE", "INTU", "SNOW",
      "DDOG", "NET", "CRWD", "PANW", "ZS", "WDAY", "TEAM"
    ),
    ev_clean = c(
      "TSLA", "F", "GM", "RIVN", "LCID", "NIO", "ENPH", "FSLR", "SEDG", "NEE",
      "PLUG", "BE", "QS"
    ),
    biotech = c(
      "LLY", "JNJ", "MRK", "PFE", "ABBV", "AMGN", "GILD", "VRTX", "REGN", "BIIB",
      "MRNA", "BNTX", "ISRG", "MDT", "ABT", "UNH", "CI"
    ),
    banks_fin = c(
      "JPM", "BAC", "WFC", "C", "GS", "MS", "BLK", "BX", "SCHW", "AXP", "V", "MA",
      "PYPL", "COF", "USB", "PNC"
    ),
    consumer_staples = c(
      "PG", "KO", "PEP", "COST", "WMT", "CL", "KMB", "MDLZ", "GIS", "KHC", "PM", "MO"
    ),
    energy = c(
      "XOM", "CVX", "COP", "SLB", "EOG", "MPC", "PSX", "VLO", "OXY", "HAL", "BKR"
    ),
    defense = c("LMT", "RTX", "NOC", "GD", "BA", "LHX", "HII", "TDG"),
    retail_ecom = c(
      "AMZN", "WMT", "COST", "TGT", "HD", "LOW", "NKE", "SBUX", "MCD", "BKNG",
      "ABNB", "EBAY", "ETSY", "SHOP"
    ),
    reits = c("PLD", "AMT", "EQIX", "CCI", "O", "SPG", "DLR", "PSA", "WELL", "VICI"),
    media_ent = c("DIS", "NFLX", "CMCSA", "WBD", "PARA", "EA", "TTWO", "RBLX", "SPOT")
  ),
  TW = list(
    # AI／先進製程／晶圓代工
    ai_foundry = c(
      "2330.TW", "2303.TW", "3711.TW", "2454.TW", "3034.TW", "2379.TW", "3443.TW",
      "3661.TW", "3529.TW", "5274.TW", "4966.TW", "6488.TWO", "8299.TW"
    ),
    # IC 設計
    ic_design = c(
      "2454.TW", "2379.TW", "3034.TW", "3443.TW", "3661.TW", "2408.TW", "4966.TW",
      "5274.TW", "3529.TW", "6488.TWO", "3227.TW", "6415.TW", "8299.TW", "4919.TW"
    ),
    # 伺服器／散熱／電源
    server_thermal = c(
      "6669.TW", "3017.TW", "2383.TW", "2356.TW", "2357.TW", "3231.TW", "2376.TW",
      "3653.TW", "3324.TW", "6239.TW", "2301.TW", "2404.TW", "6285.TW", "2393.TW"
    ),
    # PCB／載板
    pcb = c(
      "3037.TW", "8046.TW", "3189.TW", "2368.TW", "2313.TW", "5469.TW", "6274.TW",
      "4958.TW", "8210.TW", "2355.TW"
    ),
    # 記憶體／封測
    memory_osat = c(
      "2303.TW", "2408.TW", "3711.TW", "6239.TW", "2329.TW", "2449.TW", "3374.TWO",
      "6488.TWO", "3532.TW"
    ),
    # 電動車／車用電子
    ev_auto = c(
      "2308.TW", "2236.TW", "2239.TW", "2231.TW", "1513.TW", "1319.TW", "1338.TW",
      "6278.TW", "2049.TW", "3653.TW", "2392.TW", "2207.TW"
    ),
    # 綠能／重電／儲能
    green_power = c(
      "1605.TW", "1513.TW", "1519.TW", "1504.TW", "1514.TW", "6443.TW", "3576.TW",
      "6244.TWO", "6869.TW", "6781.TW"
    ),
    # 金融股
    banks_fin = c(
      "2881.TW", "2882.TW", "2884.TW", "2885.TW", "2886.TW", "2887.TW", "2890.TW",
      "2891.TW", "2892.TW", "2880.TW", "2883.TW", "2801.TW", "2834.TW", "5880.TW"
    ),
    # 高股息／電信／傳產防禦
    high_div_defensive = c(
      "2412.TW", "3045.TW", "4904.TW", "1301.TW", "1303.TW", "1326.TW", "1101.TW",
      "1102.TW", "1216.TW", "2912.TW", "9904.TW", "2105.TW", "2002.TW"
    ),
    # 生技醫療
    biotech = c(
      "4743.TWO", "6541.TWO", "4162.TWO", "1786.TW", "1795.TW", "6491.TWO",
      "4105.TWO", "4137.TW", "6467.TWO", "6709.TWO"
    ),
    # 航運
    shipping = c(
      "2603.TW", "2609.TW", "2615.TW", "2618.TW", "2637.TW", "2605.TW", "2610.TW"
    ),
    # 鋼鐵／水泥／塑化
    materials = c(
      "2002.TW", "2027.TW", "2014.TW", "1101.TW", "1102.TW", "1301.TW", "1303.TW",
      "1326.TW", "6505.TW", "1717.TW"
    ),
    # 面板／光電
    display_opto = c(
      "2409.TW", "3481.TW", "6116.TW", "4938.TW", "3406.TW", "3008.TW", "3535.TW",
      "2340.TW", "3450.TW"
    ),
    # 觀光／餐旅／零售
    tourism_retail = c(
      "2707.TW", "2727.TW", "2723.TW", "2731.TW", "2912.TW", "5905.TWO", "8454.TW",
      "9941.TW", "2603.TW"
    )
  )
)

#' Labels for concept keys (zh-TW / en-US)
lab_concept_group_label <- function(key, market = c("US", "TW"), locale = "zh-TW") {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  market <- match.arg(toupper(as.character(market)[1]), c("US", "TW"))
  key <- as.character(key %||% "")[1]
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale)[1]
  }
  is_zh <- grepl("^zh", tolower(loc), perl = TRUE)
  labels_zh <- list(
    US = c(
      mag7 = "美股七巨頭（Mag 7）",
      semis = "半導體",
      ai_infra = "AI 基礎設施／雲端算力",
      cloud_saas = "雲端／SaaS 資安",
      ev_clean = "電動車／綠能",
      biotech = "生技／醫療",
      banks_fin = "銀行／金融",
      consumer_staples = "民生消費防衛",
      energy = "能源／油氣",
      defense = "國防航太",
      retail_ecom = "零售／電商",
      reits = "REITs",
      media_ent = "媒體／娛樂"
    ),
    TW = c(
      ai_foundry = "AI／先進製程／晶圓代工",
      ic_design = "IC 設計",
      server_thermal = "伺服器／散熱／電源",
      pcb = "PCB／載板",
      memory_osat = "記憶體／封測",
      ev_auto = "電動車／車用電子",
      green_power = "綠能／重電／儲能",
      banks_fin = "金融股",
      high_div_defensive = "高股息／電信／傳產防衛",
      biotech = "生技醫療",
      shipping = "航運",
      materials = "鋼鐵／水泥／塑化",
      display_opto = "面板／光電",
      tourism_retail = "觀光／餐旅／零售"
    )
  )
  labels_en <- list(
    US = c(
      mag7 = "Magnificent 7",
      semis = "Semiconductors",
      ai_infra = "AI infra / cloud compute",
      cloud_saas = "Cloud / SaaS / cyber",
      ev_clean = "EV / clean energy",
      biotech = "Biotech / health care",
      banks_fin = "Banks / financials",
      consumer_staples = "Consumer staples",
      energy = "Energy / oil & gas",
      defense = "Defense / aerospace",
      retail_ecom = "Retail / e-commerce",
      reits = "REITs",
      media_ent = "Media / entertainment"
    ),
    TW = c(
      ai_foundry = "AI / advanced foundry",
      ic_design = "IC design",
      server_thermal = "Servers / thermal / PSU",
      pcb = "PCB / substrates",
      memory_osat = "Memory / OSAT",
      ev_auto = "EV / auto electronics",
      green_power = "Green power / heavy electric",
      banks_fin = "Banks / financials",
      high_div_defensive = "High-dividend / telecom / defensive",
      biotech = "Biotech / health care",
      shipping = "Shipping",
      materials = "Steel / cement / petrochem",
      display_opto = "Display / optoelectronics",
      tourism_retail = "Tourism / F&B / retail"
    )
  )
  tab <- if (is_zh) labels_zh[[market]] else labels_en[[market]]
  if (is.null(tab) || !key %in% names(tab)) return(key)
  unname(tab[[key]])
}

#' Named choices for selectInput / checkboxGroup (values = keys)
lab_concept_group_choices <- function(market = c("US", "TW"), locale = "zh-TW") {
  market <- match.arg(toupper(as.character(market)[1]), c("US", "TW"))
  groups <- LAB_CONCEPT_GROUPS[[market]]
  if (is.null(groups) || !length(groups)) return(character(0))
  keys <- names(groups)
  labs <- vapply(keys, function(k) lab_concept_group_label(k, market, locale), character(1))
  stats::setNames(keys, labs)
}

#' Tickers for one or more concept keys (market-aware)
lab_concept_tickers <- function(concept_keys, market = c("US", "TW")) {
  market <- match.arg(toupper(as.character(market)[1]), c("US", "TW"))
  keys <- unique(as.character(unlist(concept_keys, use.names = FALSE)))
  keys <- keys[nzchar(keys) & !is.na(keys)]
  groups <- LAB_CONCEPT_GROUPS[[market]]
  if (is.null(groups) || !length(keys)) return(character(0))
  out <- character(0)
  for (k in keys) {
    if (!k %in% names(groups)) next
    out <- c(out, as.character(groups[[k]]))
  }
  unique(toupper(trimws(out)))
}
