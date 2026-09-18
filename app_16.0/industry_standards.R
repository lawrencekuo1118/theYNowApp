# ==========================================
# industry_standards.R - 產業 KPI 基準與顯示標籤
#
# Methodology / 口徑（請誠實解讀，勿當成精確公開統計）：
# - 區間為工程啟發式 peer bands（% 或倍數），供 KPI 色碼與「目前產業標準快覽」對照，
#   非經審核之官方產業平均、亦非個股公式特例。
# - 參考取向：公開市場常見同業區間、Damodaran 風格產業表數量級、以及本 App
#   既有半導體／軟體／金融等經驗區間；beta_avg／rm_avg／debt_ratio_avg／pb_band
#   同為啟發式點估計或區間。
# - 單位：rev_growth／gpm／npm／opex／roa／roe 為 %；eqt_multiplier 為 ×；
#   debt_ratio_avg 為小數（0–1）；pb_band = c(low, high, mid)。
# - 產業建議僅經本表與 mapping helpers；禁止 ticker-specific 估值公式分支。
# ==========================================

# 產業顯示名稱（zh-TW；UI picker／快覽預設）
industry_labels <- c(
  # 半導體
  "sc.IC_Design"              = "半導體｜IC 設計",
  "sc.Foundry"                = "半導體｜晶圓代工",
  "sc.Packaging"              = "半導體｜封測",
  "sc.Memory"                 = "半導體｜記憶體",
  "sc.Equipment"              = "半導體｜設備材料",
  # 科技／軟體／網路
  "tech.Software"             = "科技｜套裝軟體",
  "saas.SaaS_Cloud"           = "科技｜SaaS／雲端",
  "tech.Internet_Platform"    = "科技｜網路平台",
  "tech.Hardware"             = "科技｜消費電子硬體",
  "ec.Hardware"               = "科技｜電子零組件",
  "tech.Optoelectronics"      = "科技｜光電／顯示",
  "tech.Electronics_Distribution" = "科技｜電子通路",
  "tech.IT_Services"          = "科技｜資訊服務",
  # 金融
  "fn.Banking"                = "金融｜銀行",
  "fn.Investment_Banking"     = "金融｜投行／證券",
  "fn.Insurance"              = "金融｜保險",
  "fn.Asset_Management"       = "金融｜資產管理",
  "fn.Fintech"                = "金融｜金融科技",
  "fn.Conglomerate_Holding"   = "金融｜控股／綜合企業",
  # 消費
  "ecr.Ecommerce_Retail"      = "消費｜電商零售",
  "retail.Brick_Mortar"       = "消費｜實體零售",
  "fmcg.Food_Beverages"       = "消費｜食品飲料",
  "fmcg.Household_Personal"   = "消費｜家用品／個護",
  "fmcg.Health_Beauty"        = "消費｜健康美容",
  "lxg.Luxury_Fashion"        = "消費｜精品時尚",
  "cons.Discretionary"        = "消費｜非必需消費",
  "cons.Restaurants"          = "消費｜餐飲",
  "cons.Home_Living"          = "消費｜居家生活",
  "cons.Sports_Leisure"       = "消費｜運動休閒",
  # 汽車
  "auto.Vehicle_Manufacturing"= "汽車｜整車製造",
  "auto.Automotive_EV"        = "汽車｜電動車",
  "auto.Parts_Suppliers"      = "汽車｜零組件",
  "auto.EV_Startups"          = "汽車｜新創 EV",
  # 醫療
  "hc.Healthcare_Services"    = "醫療｜醫療服務",
  "hc.Pharma"                 = "醫療｜製藥",
  "hc.Medtech"                = "醫療｜醫材",
  "hc.Biotech"                = "醫療｜生技",
  # 工業／原物料／能源
  "ind.Machinery"             = "工業｜機械設備",
  "ind.Aerospace_Defense"     = "工業｜航太國防",
  "ind.Construction"          = "工業｜營建工程",
  "ind.Conglomerate"          = "工業｜綜合／其他",
  "mat.Chemicals"             = "原物料｜化學",
  "mat.Metals_Mining"         = "原物料｜金屬礦業",
  "mat.Textiles"              = "原物料｜紡織纖維",
  "mat.Paper_Packaging"       = "原物料｜造紙包裝",
  "mat.Glass_Ceramics"        = "原物料｜玻璃陶瓷",
  "en.Energy_OilGas"          = "能源｜石油天然氣",
  "en.Utilities"              = "能源｜公用事業",
  "en.Renewables"             = "能源｜再生能源",
  "en.Environmental"          = "能源｜綠能環保服務",
  # 農業
  "ag.Agriculture"            = "農業｜農業科技",
  # 通訊／運輸／地產／媒體／服務
  "tel.Telecom"               = "通訊｜電信營運",
  "tr.Logistics_Shipping"     = "運輸｜物流海運",
  "tr.Airlines"               = "運輸｜航空",
  "re.REIT"                   = "地產｜REIT／不動產",
  "media.Entertainment"       = "媒體｜娛樂內容",
  "media.Gaming"              = "媒體｜遊戲",
  "media.Advertising"         = "媒體｜廣告行銷",
  "bus.Professional_Services" = "服務｜專業服務",
  "hosp.Hotels_Travel"        = "服務｜旅宿旅遊"
)

# 產業顯示名稱（en-US）
industry_labels_en <- c(
  "sc.IC_Design"              = "Semiconductors | IC Design",
  "sc.Foundry"                = "Semiconductors | Foundry",
  "sc.Packaging"              = "Semiconductors | Packaging & Testing",
  "sc.Memory"                 = "Semiconductors | Memory",
  "sc.Equipment"              = "Semiconductors | Equipment & Materials",
  "tech.Software"             = "Tech | Packaged Software",
  "saas.SaaS_Cloud"           = "Tech | SaaS / Cloud",
  "tech.Internet_Platform"    = "Tech | Internet Platforms",
  "tech.Hardware"             = "Tech | Consumer Electronics Hardware",
  "ec.Hardware"               = "Tech | Electronic Components",
  "tech.Optoelectronics"      = "Tech | Optoelectronics / Display",
  "tech.Electronics_Distribution" = "Tech | Electronics Distribution",
  "tech.IT_Services"          = "Tech | IT Services",
  "fn.Banking"                = "Financials | Banking",
  "fn.Investment_Banking"     = "Financials | Investment Banking / Brokerage",
  "fn.Insurance"              = "Financials | Insurance",
  "fn.Asset_Management"       = "Financials | Asset Management",
  "fn.Fintech"                = "Financials | Fintech",
  "fn.Conglomerate_Holding"   = "Financials | Holding / Conglomerate",
  "ecr.Ecommerce_Retail"      = "Consumer | E-commerce Retail",
  "retail.Brick_Mortar"       = "Consumer | Brick-and-Mortar Retail",
  "fmcg.Food_Beverages"       = "Consumer | Food & Beverages",
  "fmcg.Household_Personal"   = "Consumer | Household / Personal Care",
  "fmcg.Health_Beauty"        = "Consumer | Health & Beauty",
  "lxg.Luxury_Fashion"        = "Consumer | Luxury & Fashion",
  "cons.Discretionary"        = "Consumer | Discretionary",
  "cons.Restaurants"          = "Consumer | Restaurants",
  "cons.Home_Living"          = "Consumer | Home Living",
  "cons.Sports_Leisure"       = "Consumer | Sports & Leisure",
  "auto.Vehicle_Manufacturing"= "Auto | Vehicle Manufacturing",
  "auto.Automotive_EV"        = "Auto | Electric Vehicles",
  "auto.Parts_Suppliers"      = "Auto | Parts Suppliers",
  "auto.EV_Startups"          = "Auto | EV Startups",
  "hc.Healthcare_Services"    = "Healthcare | Services",
  "hc.Pharma"                 = "Healthcare | Pharmaceuticals",
  "hc.Medtech"                = "Healthcare | Medtech",
  "hc.Biotech"                = "Healthcare | Biotech",
  "ind.Machinery"             = "Industrials | Machinery",
  "ind.Aerospace_Defense"     = "Industrials | Aerospace & Defense",
  "ind.Construction"          = "Industrials | Construction",
  "ind.Conglomerate"          = "Industrials | Conglomerate / Other",
  "mat.Chemicals"             = "Materials | Chemicals",
  "mat.Metals_Mining"         = "Materials | Metals & Mining",
  "mat.Textiles"              = "Materials | Textiles",
  "mat.Paper_Packaging"       = "Materials | Paper & Packaging",
  "mat.Glass_Ceramics"        = "Materials | Glass & Ceramics",
  "en.Energy_OilGas"          = "Energy | Oil & Gas",
  "en.Utilities"              = "Energy | Utilities",
  "en.Renewables"             = "Energy | Renewables",
  "en.Environmental"          = "Energy | Environmental Services",
  "ag.Agriculture"            = "Agriculture | AgTech",
  "tel.Telecom"               = "Telecom | Operators",
  "tr.Logistics_Shipping"     = "Transport | Logistics & Shipping",
  "tr.Airlines"               = "Transport | Airlines",
  "re.REIT"                   = "Real Estate | REIT / Property",
  "media.Entertainment"       = "Media | Entertainment",
  "media.Gaming"              = "Media | Gaming",
  "media.Advertising"         = "Media | Advertising",
  "bus.Professional_Services" = "Services | Professional Services",
  "hosp.Hotels_Travel"        = "Services | Hotels & Travel"
)

#' 依 locale 回傳產業顯示名稱（en | zh-TW）
industry_label <- function(key, locale = "zh-TW") {
  k <- as.character(key %||% "")[1]
  if (!nzchar(k)) return("")
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    loc0 <- tolower(as.character(locale %||% "zh-TW")[1])
    if (identical(loc0, "en") || startsWith(loc0, "en")) "en" else "zh-TW"
  }
  labs <- if (identical(loc, "en")) industry_labels_en else industry_labels
  lab <- unname(labs[k])
  if (is.null(lab) || length(lab) < 1 || is.na(lab) || !nzchar(as.character(lab)[1])) {
    lab <- unname(industry_labels[k])
  }
  if (is.null(lab) || length(lab) < 1 || is.na(lab) || !nzchar(as.character(lab)[1])) k else as.character(lab)[1]
}

# 供 pickerInput 使用：顯示名 = 代碼
industry_picker_choices <- function(locale = "zh-TW") {
  keys <- names(industry_standards)
  labs <- vapply(keys, function(k) industry_label(k, locale = locale), character(1))
  missing <- is.na(labs) | labs == ""
  labs[missing] <- keys[missing]
  ord <- order(unname(labs))
  stats::setNames(keys[ord], labs[ord])
}

# 內部：組裝產業基準（缺省欄位給合理 NA／預設）
.ind <- function(eqt = NULL, rev_g, gpm = NULL, npm = NULL, opex = NULL,
                 roa = NULL, roe = NULL, beta, rm, debt = NULL, pb = NULL) {
  out <- list(
    rev_growth = rev_g,
    beta_avg   = beta,
    rm_avg     = rm
  )
  if (!is.null(eqt))  out$eqt_multiplier <- eqt
  if (!is.null(gpm))  out$gross_profit_margin <- gpm
  if (!is.null(npm))  out$net_profit_margin <- npm
  if (!is.null(opex)) out$opex_ratio <- opex
  if (!is.null(roa))  out$roa <- roa
  if (!is.null(roe))  out$roe <- roe
  if (!is.null(debt)) out$debt_ratio_avg <- debt
  if (!is.null(pb))   out$pb_band <- pb  # c(low, high, mid)
  out
}

industry_standards <- list(
  # ---------- 半導體 ----------
  sc.IC_Design = .ind(
    eqt = c(1.5, 2.5), rev_g = c(0, 15), gpm = c(40, 70), opex = c(30, 50),
    roa = c(10, 20), roe = c(15, 30), beta = 1.35, rm = 8.5, debt = 0.10,
    pb = c(2.5, 6.0, 4.0)
  ),
  sc.Foundry = .ind(
    eqt = c(1.5, 3.5), rev_g = c(0, 12), gpm = c(30, 55), opex = c(10, 25),
    roa = c(5, 12), roe = c(10, 20), beta = 1.25, rm = 8.2, debt = 0.25,
    pb = c(2.0, 5.0, 3.2)
  ),
  sc.Packaging = .ind(
    eqt = c(1.5, 2.5), rev_g = c(0, 10), gpm = c(15, 30), opex = c(20, 35),
    roa = c(5, 10), roe = c(8, 18), beta = 1.15, rm = 8.0, debt = 0.20,
    pb = c(1.5, 3.5, 2.2)
  ),
  sc.Memory = .ind(
    eqt = c(2.0, 4.0), rev_g = c(-5, 15), gpm = c(20, 50), opex = c(15, 25),
    roa = c(-5, 15), roe = c(-10, 25), beta = 1.40, rm = 8.8, debt = 0.30,
    pb = c(1.2, 3.5, 2.0)
  ),
  sc.Equipment = .ind(
    eqt = c(1.5, 2.8), rev_g = c(0, 15), gpm = c(40, 55), opex = c(20, 35),
    roa = c(8, 18), roe = c(15, 30), beta = 1.30, rm = 8.5, debt = 0.15,
    pb = c(3.0, 7.0, 4.5)
  ),

  # ---------- 科技／軟體／網路 ----------
  tech.Software = .ind(
    eqt = c(1.5, 3.0), rev_g = c(5, 15), gpm = c(65, 85), opex = c(35, 55),
    npm = c(15, 30), roa = c(8, 18), roe = c(15, 35), beta = 1.10, rm = 8.8,
    debt = 0.15, pb = c(4.0, 12.0, 7.0)
  ),
  saas.SaaS_Cloud = .ind(
    eqt = c(2.0, 5.0), rev_g = c(10, 25), gpm = c(70, 85), opex = c(40, 60),
    roa = c(5, 15), roe = c(15, 35), beta = 1.20, rm = 9.0, debt = 0.20,
    pb = c(5.0, 15.0, 9.0)
  ),
  tech.Internet_Platform = .ind(
    eqt = c(1.5, 3.5), rev_g = c(8, 20), gpm = c(45, 70), opex = c(25, 45),
    npm = c(10, 25), roa = c(6, 15), roe = c(12, 30), beta = 1.15, rm = 8.8,
    debt = 0.15, pb = c(3.0, 10.0, 5.5)
  ),
  tech.Hardware = .ind(
    eqt = c(2.0, 4.0), rev_g = c(0, 10), gpm = c(30, 45), opex = c(10, 20),
    npm = c(10, 25), roa = c(10, 20), roe = c(25, 50), beta = 1.15, rm = 8.5,
    debt = 0.25, pb = c(4.0, 12.0, 7.0)
  ),
  ec.Hardware = .ind(
    eqt = c(2.5, 6.0), rev_g = c(2, 12), gpm = c(25, 45), opex = c(10, 25),
    roa = c(8, 18), roe = c(25, 50), beta = 1.10, rm = 8.0, debt = 0.20,
    pb = c(2.0, 5.0, 3.0)
  ),
  tech.Optoelectronics = .ind(
    eqt = c(1.8, 4.0), rev_g = c(-5, 12), gpm = c(15, 35), npm = c(2, 12),
    opex = c(10, 25), roa = c(3, 10), roe = c(8, 20), beta = 1.25, rm = 8.5,
    debt = 0.30, pb = c(1.2, 3.5, 2.0)
  ),
  tech.Electronics_Distribution = .ind(
    eqt = c(2.0, 5.0), rev_g = c(0, 10), gpm = c(5, 15), npm = c(1, 5),
    opex = c(3, 10), roa = c(3, 8), roe = c(10, 20), beta = 1.05, rm = 8.0,
    debt = 0.40, pb = c(0.8, 2.0, 1.3)
  ),
  tech.IT_Services = .ind(
    eqt = c(1.5, 3.0), rev_g = c(3, 15), gpm = c(25, 45), npm = c(5, 15),
    opex = c(20, 40), roa = c(5, 12), roe = c(12, 25), beta = 1.05, rm = 8.2,
    debt = 0.20, pb = c(2.0, 5.0, 3.2)
  ),

  # ---------- 金融 ----------
  fn.Banking = .ind(
    eqt = c(8, 18), rev_g = c(0, 8), npm = c(15, 30), opex = c(45, 60),
    roa = c(0.8, 1.5), roe = c(8, 15), beta = 1.05, rm = 7.8, debt = 0.85,
    pb = c(0.8, 1.5, 1.15)
  ),
  fn.Investment_Banking = .ind(
    eqt = c(10, 25), rev_g = c(-5, 12), npm = c(10, 25), opex = c(50, 70),
    roa = c(0.5, 2.0), roe = c(8, 18), beta = 1.40, rm = 8.8, debt = 0.80,
    pb = c(0.8, 1.6, 1.15)
  ),
  fn.Insurance = .ind(
    eqt = c(4, 12), rev_g = c(0, 8), npm = c(5, 15), opex = c(50, 65),
    roa = c(0.5, 2.0), roe = c(8, 15), beta = 0.90, rm = 7.5, debt = 0.40,
    pb = c(1.0, 1.7, 1.35)
  ),
  fn.Asset_Management = .ind(
    eqt = c(1.5, 3.0), rev_g = c(0, 10), npm = c(15, 35), opex = c(40, 60),
    roa = c(5, 15), roe = c(15, 30), beta = 1.15, rm = 8.2, debt = 0.20,
    pb = c(1.5, 4.0, 2.5)
  ),
  fn.Fintech = .ind(
    eqt = c(1.5, 4.0), rev_g = c(10, 30), gpm = c(40, 70), opex = c(40, 70),
    npm = c(-5, 20), roa = c(-2, 10), roe = c(-5, 20), beta = 1.35, rm = 9.0,
    debt = 0.25, pb = c(2.0, 8.0, 4.0)
  ),
  fn.Conglomerate_Holding = .ind(
    eqt = c(1.5, 3.0), rev_g = c(0, 8), npm = c(8, 20), opex = c(20, 40),
    roa = c(3, 8), roe = c(8, 15), beta = 0.85, rm = 7.5, debt = 0.25,
    pb = c(1.1, 1.8, 1.40)
  ),

  # ---------- 消費 ----------
  ecr.Ecommerce_Retail = .ind(
    eqt = c(1.8, 3.5), rev_g = c(5, 18), gpm = c(20, 45), opex = c(15, 35),
    npm = c(2, 10), roa = c(5, 12), roe = c(12, 25), beta = 1.15, rm = 8.5,
    debt = 0.25, pb = c(2.0, 6.0, 3.5)
  ),
  retail.Brick_Mortar = .ind(
    eqt = c(1.8, 3.5), rev_g = c(-2, 6), gpm = c(25, 40), opex = c(20, 35),
    npm = c(2, 8), roa = c(4, 10), roe = c(10, 20), beta = 1.00, rm = 8.0,
    debt = 0.35, pb = c(1.2, 3.0, 1.8)
  ),
  fmcg.Food_Beverages = .ind(
    eqt = c(1.5, 2.8), rev_g = c(2, 6), gpm = c(35, 50), opex = c(25, 35),
    npm = c(8, 14), roa = c(5, 12), roe = c(12, 22), beta = 0.70, rm = 7.2,
    debt = 0.35, pb = c(2.5, 5.0, 3.5)
  ),
  fmcg.Household_Personal = .ind(
    eqt = c(1.8, 3.0), rev_g = c(2, 8), gpm = c(45, 60), opex = c(28, 38),
    npm = c(10, 16), roa = c(6, 14), roe = c(15, 28), beta = 0.75, rm = 7.3,
    debt = 0.30, pb = c(3.0, 6.0, 4.0)
  ),
  fmcg.Health_Beauty = .ind(
    eqt = c(1.5, 2.5), rev_g = c(4, 12), gpm = c(50, 70), opex = c(30, 45),
    npm = c(12, 20), roa = c(8, 16), roe = c(15, 30), beta = 0.85, rm = 7.6,
    debt = 0.25, pb = c(3.0, 7.0, 4.5)
  ),
  lxg.Luxury_Fashion = .ind(
    eqt = c(1.5, 2.5), rev_g = c(4, 12), gpm = c(60, 75), opex = c(30, 45),
    roa = c(10, 18), roe = c(20, 35), beta = 1.05, rm = 8.0, debt = 0.25,
    pb = c(3.0, 8.0, 5.0)
  ),
  cons.Discretionary = .ind(
    eqt = c(1.8, 3.5), rev_g = c(0, 10), gpm = c(30, 50), opex = c(20, 35),
    npm = c(5, 12), roa = c(4, 12), roe = c(10, 22), beta = 1.15, rm = 8.3,
    debt = 0.30, pb = c(1.5, 4.0, 2.5)
  ),
  cons.Restaurants = .ind(
    eqt = c(2.0, 5.0), rev_g = c(0, 10), gpm = c(25, 40), npm = c(4, 12),
    opex = c(25, 40), roa = c(4, 12), roe = c(12, 28), beta = 1.05, rm = 8.0,
    debt = 0.45, pb = c(2.0, 6.0, 3.5)
  ),
  cons.Home_Living = .ind(
    eqt = c(1.8, 3.5), rev_g = c(0, 8), gpm = c(25, 45), npm = c(3, 10),
    opex = c(18, 35), roa = c(4, 10), roe = c(10, 20), beta = 1.00, rm = 8.0,
    debt = 0.35, pb = c(1.2, 3.5, 2.0)
  ),
  cons.Sports_Leisure = .ind(
    eqt = c(1.8, 3.5), rev_g = c(0, 12), gpm = c(30, 50), npm = c(4, 12),
    opex = c(20, 40), roa = c(4, 12), roe = c(10, 22), beta = 1.15, rm = 8.3,
    debt = 0.30, pb = c(1.5, 4.5, 2.8)
  ),

  # ---------- 汽車 ----------
  auto.Vehicle_Manufacturing = .ind(
    eqt = c(2.5, 5.0), rev_g = c(-3, 8), gpm = c(10, 20), npm = c(3, 8),
    opex = c(10, 20), roa = c(2, 8), roe = c(8, 18), beta = 1.20, rm = 8.3,
    debt = 0.45, pb = c(0.8, 2.0, 1.2)
  ),
  auto.Automotive_EV = .ind(
    eqt = c(2.5, 6.0), rev_g = c(5, 20), gpm = c(12, 25), opex = c(10, 20),
    roa = c(2, 8), roe = c(8, 18), beta = 1.35, rm = 9.0, debt = 0.35,
    pb = c(2.0, 8.0, 4.0)
  ),
  auto.Parts_Suppliers = .ind(
    eqt = c(2.0, 4.0), rev_g = c(-3, 10), gpm = c(12, 25), npm = c(4, 10),
    opex = c(12, 25), roa = c(3, 9), roe = c(8, 18), beta = 1.15, rm = 8.0,
    debt = 0.35, pb = c(1.0, 2.5, 1.5)
  ),
  auto.EV_Startups = .ind(
    eqt = c(1.2, 3.0), rev_g = c(10, 40), gpm = c(5, 20), npm = c(-25, 5),
    opex = c(30, 60), roa = c(-15, 5), roe = c(-30, 10), beta = 1.60, rm = 9.5,
    debt = 0.40, pb = c(1.5, 6.0, 3.0)
  ),

  # ---------- 醫療 ----------
  hc.Healthcare_Services = .ind(
    eqt = c(1.8, 3.5), rev_g = c(2, 10), gpm = c(30, 50), opex = c(40, 60),
    npm = c(5, 12), roa = c(4, 10), roe = c(8, 18), beta = 0.85, rm = 7.5,
    debt = 0.40, pb = c(1.5, 3.5, 2.2)
  ),
  hc.Pharma = .ind(
    eqt = c(1.5, 3.0), rev_g = c(2, 10), gpm = c(60, 80), opex = c(30, 50),
    npm = c(12, 25), roa = c(8, 15), roe = c(12, 25), beta = 0.90, rm = 7.6,
    debt = 0.30, pb = c(2.5, 6.0, 4.0)
  ),
  hc.Medtech = .ind(
    eqt = c(1.5, 3.0), rev_g = c(3, 12), gpm = c(55, 75), opex = c(25, 40),
    npm = c(10, 22), roa = c(8, 18), roe = c(12, 28), beta = 1.00, rm = 7.8,
    debt = 0.25, pb = c(3.0, 7.0, 4.5)
  ),
  hc.Biotech = .ind(
    eqt = c(1.2, 2.5), rev_g = c(-10, 25), gpm = c(50, 90), opex = c(50, 120),
    npm = c(-80, 10), roa = c(-20, 8), roe = c(-40, 15), beta = 1.45, rm = 9.0,
    debt = 0.20, pb = c(2.0, 10.0, 5.0)
  ),

  # ---------- 工業／原物料／能源 ----------
  ind.Machinery = .ind(
    eqt = c(1.8, 3.5), rev_g = c(0, 8), gpm = c(25, 40), npm = c(6, 12),
    opex = c(15, 30), roa = c(4, 10), roe = c(10, 20), beta = 1.15, rm = 8.2,
    debt = 0.35, pb = c(1.5, 3.5, 2.2)
  ),
  ind.Aerospace_Defense = .ind(
    eqt = c(2.0, 5.0), rev_g = c(2, 10), gpm = c(15, 30), npm = c(5, 12),
    opex = c(10, 25), roa = c(3, 9), roe = c(10, 20), beta = 1.05, rm = 8.0,
    debt = 0.45, pb = c(2.0, 5.0, 3.2)
  ),
  ind.Construction = .ind(
    eqt = c(2.0, 4.5), rev_g = c(-2, 8), gpm = c(10, 20), npm = c(2, 6),
    opex = c(8, 18), roa = c(2, 7), roe = c(6, 15), beta = 1.25, rm = 8.5,
    debt = 0.40, pb = c(0.8, 2.0, 1.3)
  ),
  ind.Conglomerate = .ind(
    eqt = c(1.8, 3.5), rev_g = c(-2, 8), gpm = c(15, 35), npm = c(3, 10),
    opex = c(12, 30), roa = c(3, 8), roe = c(8, 16), beta = 1.05, rm = 8.0,
    debt = 0.35, pb = c(0.9, 2.2, 1.4)
  ),
  mat.Chemicals = .ind(
    eqt = c(1.8, 3.5), rev_g = c(-3, 8), gpm = c(20, 35), npm = c(5, 12),
    opex = c(10, 20), roa = c(3, 10), roe = c(8, 18), beta = 1.15, rm = 8.3,
    debt = 0.40, pb = c(1.2, 3.0, 1.8)
  ),
  mat.Metals_Mining = .ind(
    eqt = c(1.5, 3.0), rev_g = c(-8, 15), gpm = c(15, 40), npm = c(5, 20),
    opex = c(8, 20), roa = c(2, 12), roe = c(5, 20), beta = 1.30, rm = 8.8,
    debt = 0.35, pb = c(0.8, 2.2, 1.3)
  ),
  mat.Textiles = .ind(
    eqt = c(1.8, 3.5), rev_g = c(-3, 8), gpm = c(15, 30), npm = c(2, 8),
    opex = c(10, 22), roa = c(2, 8), roe = c(6, 15), beta = 1.10, rm = 8.2,
    debt = 0.35, pb = c(0.8, 2.0, 1.3)
  ),
  mat.Paper_Packaging = .ind(
    eqt = c(1.8, 3.5), rev_g = c(-2, 6), gpm = c(15, 30), npm = c(3, 10),
    opex = c(10, 22), roa = c(3, 8), roe = c(8, 16), beta = 1.05, rm = 8.0,
    debt = 0.40, pb = c(0.9, 2.2, 1.4)
  ),
  mat.Glass_Ceramics = .ind(
    eqt = c(1.8, 3.5), rev_g = c(-3, 8), gpm = c(18, 35), npm = c(3, 10),
    opex = c(10, 22), roa = c(3, 9), roe = c(8, 16), beta = 1.10, rm = 8.2,
    debt = 0.35, pb = c(0.9, 2.3, 1.5)
  ),
  en.Energy_OilGas = .ind(
    eqt = c(1.8, 3.0), rev_g = c(-5, 12), gpm = c(20, 40), opex = c(5, 15),
    roa = c(5, 12), roe = c(10, 22), beta = 1.10, rm = 8.5, debt = 0.40,
    pb = c(1.0, 2.2, 1.5)
  ),
  en.Utilities = .ind(
    eqt = c(2.5, 4.5), rev_g = c(1, 5), gpm = c(25, 40), npm = c(8, 15),
    opex = c(15, 30), roa = c(2, 6), roe = c(8, 14), beta = 0.65, rm = 7.0,
    debt = 0.55, pb = c(1.2, 2.2, 1.6)
  ),
  en.Renewables = .ind(
    eqt = c(2.0, 5.0), rev_g = c(5, 20), gpm = c(30, 55), npm = c(0, 15),
    opex = c(15, 35), roa = c(1, 8), roe = c(4, 15), beta = 1.20, rm = 8.5,
    debt = 0.50, pb = c(1.5, 4.0, 2.5)
  ),
  en.Environmental = .ind(
    eqt = c(2.0, 4.5), rev_g = c(2, 15), gpm = c(20, 40), npm = c(2, 12),
    opex = c(15, 35), roa = c(2, 8), roe = c(6, 16), beta = 1.10, rm = 8.3,
    debt = 0.45, pb = c(1.2, 3.5, 2.2)
  ),

  # ---------- 農業 ----------
  ag.Agriculture = .ind(
    eqt = c(1.8, 3.5), rev_g = c(0, 12), gpm = c(20, 40), npm = c(2, 12),
    opex = c(15, 30), roa = c(3, 10), roe = c(8, 18), beta = 1.05, rm = 8.2,
    debt = 0.35, pb = c(1.2, 3.5, 2.0)
  ),

  # ---------- 通訊／運輸／地產／媒體／服務 ----------
  tel.Telecom = .ind(
    eqt = c(2.0, 4.0), rev_g = c(0, 5), gpm = c(45, 60), npm = c(8, 15),
    opex = c(25, 40), roa = c(3, 8), roe = c(8, 16), beta = 0.75, rm = 7.3,
    debt = 0.50, pb = c(1.0, 2.5, 1.6)
  ),
  tr.Logistics_Shipping = .ind(
    eqt = c(2.0, 4.0), rev_g = c(-5, 12), gpm = c(15, 35), npm = c(3, 12),
    opex = c(10, 25), roa = c(3, 10), roe = c(8, 18), beta = 1.20, rm = 8.3,
    debt = 0.40, pb = c(0.8, 2.0, 1.3)
  ),
  tr.Airlines = .ind(
    eqt = c(3.0, 8.0), rev_g = c(-5, 12), gpm = c(15, 30), npm = c(-5, 8),
    opex = c(15, 30), roa = c(-2, 6), roe = c(-5, 15), beta = 1.40, rm = 8.8,
    debt = 0.60, pb = c(0.8, 2.5, 1.4)
  ),
  re.REIT = .ind(
    eqt = c(1.5, 3.0), rev_g = c(1, 6), npm = c(20, 50), opex = c(20, 40),
    roa = c(2, 6), roe = c(6, 12), beta = 0.80, rm = 7.2, debt = 0.50,
    pb = c(0.8, 1.5, 1.1)
  ),
  media.Entertainment = .ind(
    eqt = c(1.5, 3.5), rev_g = c(0, 12), gpm = c(30, 55), npm = c(5, 18),
    opex = c(30, 50), roa = c(3, 10), roe = c(8, 20), beta = 1.10, rm = 8.3,
    debt = 0.35, pb = c(1.5, 5.0, 2.8)
  ),
  media.Gaming = .ind(
    eqt = c(1.2, 2.5), rev_g = c(5, 18), gpm = c(55, 80), npm = c(10, 30),
    opex = c(30, 50), roa = c(8, 18), roe = c(15, 30), beta = 1.05, rm = 8.5,
    debt = 0.15, pb = c(2.5, 8.0, 4.5)
  ),
  media.Advertising = .ind(
    eqt = c(1.5, 3.0), rev_g = c(0, 10), gpm = c(25, 45), npm = c(5, 15),
    opex = c(25, 45), roa = c(4, 12), roe = c(10, 22), beta = 1.10, rm = 8.3,
    debt = 0.25, pb = c(1.5, 4.5, 2.8)
  ),
  bus.Professional_Services = .ind(
    eqt = c(1.5, 3.0), rev_g = c(2, 12), gpm = c(30, 50), npm = c(6, 15),
    opex = c(25, 45), roa = c(5, 12), roe = c(12, 25), beta = 1.00, rm = 8.0,
    debt = 0.25, pb = c(2.0, 5.0, 3.2)
  ),
  hosp.Hotels_Travel = .ind(
    eqt = c(2.0, 5.0), rev_g = c(-5, 12), gpm = c(25, 45), npm = c(2, 12),
    opex = c(25, 45), roa = c(2, 8), roe = c(6, 18), beta = 1.25, rm = 8.5,
    debt = 0.45, pb = c(1.5, 4.0, 2.5)
  )
)

# ---------------------------------------------------------------------------
# External string → industry_standards key（Yahoo / 關鍵字模糊；未知回傳 ""）
# ---------------------------------------------------------------------------

#' Yahoo／自由文字 Sector+Industry → App 產業鍵（啟發式；未知回傳 ""）
map_yahoo_industry_to_key <- function(sector = "", industry = "") {
  sec <- trimws(as.character(sector %||% "")[1])
  ind <- trimws(as.character(industry %||% "")[1])
  blob <- tolower(paste(sec, ind, sep = " | "))
  if (!nzchar(gsub("[\\s|NA/na]", "", blob))) return("")

  # Exact-ish GICS sub-industry names if Lab map is loaded
  if (exists("lab_gics_subindustry_map", mode = "function") && nzchar(ind)) {
    smap <- lab_gics_subindustry_map()
    if (ind %in% names(smap)) {
      key <- unname(smap[[ind]])
      if (key %in% names(industry_standards)) return(key)
    }
  }

  rules <- list(
    "sc.IC_Design" = c("semiconductor(?!.*(equip|material|memory|foundry))", "ic design", "fabless"),
    "sc.Foundry" = c("foundry", "晶圓代工"),
    "sc.Memory" = c("memory", "dram", "nand"),
    "sc.Equipment" = c("semiconductor.*(equip|material)", "wafer equip"),
    "sc.Packaging" = c("packag.*(test|assembl)", "osat", "封測"),
    "saas.SaaS_Cloud" = c("software.*application", "saas", "cloud computing", "internet services & infrastructure", "數位雲端"),
    "tech.Software" = c("systems software", "software—infrastructure", "packaged software"),
    "tech.Internet_Platform" = c("internet content", "interactive media", "social media", "網路平台"),
    "tech.Hardware" = c("computer hardware", "consumer electronics", "technology hardware"),
    "tech.Optoelectronics" = c("optoelectronic", "display panel", "led", "光電"),
    "tech.Electronics_Distribution" = c("technology distributor", "electronics distribution", "電子通路"),
    "tech.IT_Services" = c("it consulting", "information technology services", "資訊服務"),
    "ec.Hardware" = c("electronic component", "electronic equipment", "通訊設備", "電子零組件"),
    "fn.Banking" = c("\\bbanks?\\b", "diversified bank", "regional bank", "銀行"),
    "fn.Investment_Banking" = c("investment banking", "brokerage", "capital markets", "證券"),
    "fn.Insurance" = c("insurance", "保險"),
    "fn.Asset_Management" = c("asset management", "capital markets.*asset", "資產管理"),
    "fn.Fintech" = c("consumer finance", "payment", "fintech", "transaction.*process"),
    "fn.Conglomerate_Holding" = c("conglomerate", "multi-sector holdings", "holding compan"),
    "ecr.Ecommerce_Retail" = c("internet retail", "broadline retail", "e-?commerce"),
    "retail.Brick_Mortar" = c("specialty retail", "apparel retail", "food retail", "department store"),
    "cons.Restaurants" = c("restaurant", "餐飲"),
    "cons.Home_Living" = c("homefurnishing", "home improvement", "居家生活"),
    "cons.Sports_Leisure" = c("leisure products", "sporting", "運動休閒"),
    "fmcg.Food_Beverages" = c("packaged foods", "beverages", "soft drinks", "brewers", "食品"),
    "fmcg.Household_Personal" = c("household products", "personal products"),
    "fmcg.Health_Beauty" = c("personal care", "cosmetics", "beauty"),
    "lxg.Luxury_Fashion" = c("apparel.*luxury", "footwear", "luxury goods"),
    "cons.Discretionary" = c("consumer discretionary", "specialty consumer"),
    "auto.Automotive_EV" = c("electric vehicle", "ev manufacturer", "電動車"),
    "auto.Vehicle_Manufacturing" = c("auto manufacturers", "automobile manufacturer", "整車"),
    "auto.Parts_Suppliers" = c("auto parts", "automotive parts", "汽車工業"),
    "hc.Biotech" = c("biotechnology", "生技"),
    "hc.Pharma" = c("pharmaceutical", "drug manufacturer", "製藥"),
    "hc.Medtech" = c("health care equipment", "medical device", "life sciences tools", "醫材"),
    "hc.Healthcare_Services" = c("health care (facilities|services|providers|managed)", "醫療服務"),
    "ind.Aerospace_Defense" = c("aerospace", "defense"),
    "ind.Construction" = c("construction", "engineering", "homebuilding", "cement", "營建", "水泥"),
    "ind.Machinery" = c("industrial machinery", "machinery", "electrical equipment", "電機機械"),
    "ind.Conglomerate" = c("industrial conglomerate", "diversified industrials"),
    "mat.Chemicals" = c("chemical", "specialty chemical", "fertilizer", "化學", "塑膠", "橡膠"),
    "mat.Metals_Mining" = c("steel", "copper", "gold", "mining", "metal", "鋼鐵"),
    "mat.Textiles" = c("textile", "紡織"),
    "mat.Paper_Packaging" = c("paper", "packaging", "container", "造紙"),
    "mat.Glass_Ceramics" = c("glass", "ceramic", "玻璃", "陶瓷"),
    "en.Energy_OilGas" = c("oil (&|and) gas", "integrated oil", "exploration", "refining", "石油"),
    "en.Utilities" = c("electric utilities", "multi-utilities", "gas utilities", "water utilities", "公用"),
    "en.Renewables" = c("renewable", "solar", "wind", "再生能源"),
    "en.Environmental" = c("environmental", "waste management", "綠能環保"),
    "ag.Agriculture" = c("agricultural", "farm", "農業"),
    "tel.Telecom" = c("telecommunication", "wireless", "telecom", "電信", "通信網路"),
    "tr.Airlines" = c("airline", "航空"),
    "tr.Logistics_Shipping" = c("air freight", "logistics", "railroad", "shipping", "marine", "航運", "物流"),
    "re.REIT" = c("\\breit\\b", "real estate", "不動產"),
    "media.Gaming" = c("interactive home entertainment", "gaming", "casino", "遊戲"),
    "media.Advertising" = c("advertising", "廣告"),
    "media.Entertainment" = c("movies", "entertainment", "broadcasting", "publishing", "娛樂", "文化創意"),
    "bus.Professional_Services" = c("research & consulting", "professional services", "human resource"),
    "hosp.Hotels_Travel" = c("hotels?", "resort", "cruise", "travel", "leisure facilities", "觀光", "旅宿")
  )

  for (key in names(rules)) {
    for (pat in rules[[key]]) {
      if (grepl(pat, blob, ignore.case = TRUE, perl = TRUE)) {
        if (key %in% names(industry_standards)) return(key)
      }
    }
  }

  if (exists("lab_gics_sector_fallback", mode = "function") && nzchar(sec)) {
    fmap <- lab_gics_sector_fallback()
    if (sec %in% names(fmap)) {
      key <- unname(fmap[[sec]])
      if (key %in% names(industry_standards)) return(key)
    }
  }
  ""
}

#' 解析「Sector: X | Industry: Y」顯示字串
parse_yahoo_industry_display <- function(display_text) {
  s <- as.character(display_text %||% "")[1]
  sec <- sub("(?is)^.*Sector:\\s*([^|]*).*$", "\\1", s, perl = TRUE)
  ind <- sub("(?is)^.*Industry:\\s*(.*)$", "\\1", s, perl = TRUE)
  if (identical(sec, s)) sec <- ""
  if (identical(ind, s)) ind <- ""
  list(sector = trimws(sec), industry = trimws(ind))
}

#' 由 Yahoo display／sector／industry 解析產業鍵；未知回傳 ""（呼叫端保留原選擇）
resolve_industry_key_from_yahoo <- function(display_text = "", sector = "", industry = "") {
  if ((!nzchar(as.character(sector %||% "")[1]) || !nzchar(as.character(industry %||% "")[1])) &&
      nzchar(as.character(display_text %||% "")[1])) {
    parsed <- parse_yahoo_industry_display(display_text)
    if (!nzchar(as.character(sector %||% "")[1])) sector <- parsed$sector
    if (!nzchar(as.character(industry %||% "")[1])) industry <- parsed$industry
  }
  map_yahoo_industry_to_key(sector, industry)
}

# 🎨 KPI 顏色判定（只取區間前兩碼 low/high；與 industry_standards 一致）
# 回傳 AdminLTE color：black／red／blue，或缺區間／N/A 時回傳 "none"（白；由 kpi_band_value_box 渲染）
get_box_color <- function(industry_choice, metric_name, val) {
  # In-band=black; worse/alert=red; better=blue; missing/no band=none (white).
  if (is.null(industry_choice) || length(industry_choice) == 0 || industry_choice == "") return("none")
  if (is.null(metric_name) || length(metric_name) == 0) return("none")
  if (is.na(val) || is.null(val)) return("none")
  if (!(industry_choice %in% names(industry_standards))) return("none")

  std <- industry_standards[[industry_choice]][[metric_name]]
  if (is.null(std) || length(std) < 2) return("none")
  lo <- suppressWarnings(as.numeric(std[1])[1])
  hi <- suppressWarnings(as.numeric(std[2])[1])
  if (!is.finite(lo) || !is.finite(hi)) return("none")

  # 費用／槓桿類：越高通常越差 → 反向著色
  lower_is_better <- metric_name %in% c("opex_ratio", "eqt_multiplier")

  if (val >= lo && val <= hi) {
    return("black")
  } else if (isTRUE(lower_is_better)) {
    if (val < lo) return("blue") else return("red")
  } else {
    if (val < lo) return("red") else return("blue")
  }
}

#' KPI valueBox：僅允許黑／白／紅／藍（AdminLTE: black, red, blue；白＝none）
#' @param mark_focus 若 TRUE，在數值旁加琥珀色「屬性重視」小圓點（與同業紅藍語意分離）
#' @param focus_title 圓點 title／aria-label
kpi_band_value_box <- function(value, subtitle, color, icon = NULL, width = 4,
                               mark_focus = FALSE, focus_title = NULL) {
  color <- as.character(color %||% "none")[1]
  allowed <- c("black", "red", "blue", "none", "white")
  if (!nzchar(color) || is.na(color) || !(color %in% allowed)) color <- "none"
  mark <- if (isTRUE(mark_focus) && exists(".ynow_focus_metric_mark", mode = "function")) {
    .ynow_focus_metric_mark(focus_title)
  } else {
    NULL
  }
  value_ui <- if (!is.null(mark)) {
    tagList(value, mark)
  } else {
    value
  }
  if (identical(color, "none") || identical(color, "white")) {
    box_content <- div(
      class = "small-box ynow-kpi-na",
      div(class = "inner", h3(value_ui), p(subtitle)),
      if (!is.null(icon)) div(class = "icon-large", icon)
    )
    return(div(class = if (!is.null(width)) paste0("col-sm-", width), box_content))
  }
  valueBox(value = value_ui, subtitle = subtitle, icon = icon, color = color, width = width)
}

#' 產業標準欄位 → 顯示標籤／單位（單一來源，供快覽／Annotation／色碼共用）
INDUSTRY_METRIC_META <- list(
  rev_growth          = list(label = "營收成長", unit = "%"),
  gross_profit_margin = list(label = "毛利率", unit = "%"),
  roe                 = list(label = "ROE", unit = "%"),
  roa                 = list(label = "ROA", unit = "%"),
  opex_ratio          = list(label = "營運費用比", unit = "%"),
  eqt_multiplier      = list(label = "財務槓桿", unit = "x"),
  net_profit_margin   = list(label = "淨利率", unit = "%")
)

#' 「目前產業標準快覽」晶片固定順序（僅這六項；有定義才顯示）
INDUSTRY_SNAPSHOT_METRIC_ORDER <- c(
  "rev_growth",
  "gross_profit_margin",
  "roe",
  "roa",
  "opex_ratio",
  "eqt_multiplier"
)

#' 格式化單一數值：整數保留整數寫法，對齊 industry_standards.R 字面量
industry_format_number <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return(NA_character_)
  if (abs(x - round(x)) < 1e-9) {
    return(format(as.integer(round(x)), scientific = FALSE, trim = TRUE))
  }
  format(round(x, digits), nsmall = 1, scientific = FALSE, trim = TRUE)
}

#' 格式化產業標準區間（直接讀 industry_standards[[key]][[metric]]）
annotation_format_band <- function(industry_key, metric_name, unit = NULL) {
  key <- as.character(industry_key %||% "")[1]
  metric_name <- as.character(metric_name %||% "")[1]
  if (!nzchar(key) || !(key %in% names(industry_standards))) return("—")
  if (!nzchar(metric_name)) return("—")
  std <- industry_standards[[key]][[metric_name]]
  if (is.null(std) || length(std) < 2) return("—（本產業無此區間）")
  lo <- suppressWarnings(as.numeric(std[1])[1])
  hi <- suppressWarnings(as.numeric(std[2])[1])
  if (!is.finite(lo) || !is.finite(hi)) return("—")
  if (is.null(unit) || !nzchar(as.character(unit)[1])) {
    meta_u <- INDUSTRY_METRIC_META[[metric_name]]
    unit <- if (!is.null(meta_u) && !is.null(meta_u$unit)) meta_u$unit else "%"
  }
  lo_s <- industry_format_number(lo)
  hi_s <- industry_format_number(hi)
  if (identical(as.character(unit)[1], "x")) {
    paste0(lo_s, "–", hi_s, "×")
  } else {
    paste0(lo_s, "–", hi_s, "%")
  }
}

#' 該產業在 industry_standards 中實際定義的 KPI 區間表
industry_standard_bands_df <- function(industry_key) {
  key <- as.character(industry_key %||% "")[1]
  if (!nzchar(key) || !(key %in% names(industry_standards))) {
    return(data.frame(
      metric = character(0), label = character(0),
      band = character(0), unit = character(0),
      stringsAsFactors = FALSE
    ))
  }
  inds <- industry_standards[[key]]
  # 快覽固定順序：營收成長 → 毛利率 → ROE → ROA → 營運費用比 → 財務槓桿
  metrics <- INDUSTRY_SNAPSHOT_METRIC_ORDER[
    INDUSTRY_SNAPSHOT_METRIC_ORDER %in% names(inds) &
      INDUSTRY_SNAPSHOT_METRIC_ORDER %in% names(INDUSTRY_METRIC_META)
  ]
  do.call(rbind, lapply(metrics, function(m) {
    meta <- INDUSTRY_METRIC_META[[m]]
    data.frame(
      metric = m,
      label = meta$label,
      band = annotation_format_band(key, m, unit = meta$unit),
      unit = meta$unit,
      stringsAsFactors = FALSE
    )
  }))
}

#' Annotation：Dashboard KPI 解讀表（可帶入目前產業區間與財報屬性重視）
annotation_kpi_guide_df <- function(industry_key = NULL, profile_id = NULL) {
  key <- as.character(industry_key %||% "")[1]
  prof <- as.character(profile_id %||% "")[1]
  rows <- list(
    list("毛利率", "Gross Profit / Revenue", "越高越好", "gross_profit_margin", "%",
         "技術／品牌定價力；著色對照產業毛利率區間"),
    list("淨利率", "Net Income / Revenue", "越高越好", "net_profit_margin", "%",
         "獲利兌現能力；注意業外與一次性項目"),
    list("營運費用比", "Operating Expense / Revenue", "越低越好", "opex_ratio", "%",
         "管銷效率；低於區間下限＝藍（更好）"),
    list("營收成長率", "年均 YoY(Total Revenue)", "越高越好", "rev_growth", "%",
         "成長動能；年報序列排除 TTM"),
    list("毛利成長率", "年均 YoY(Gross Profit)", "越高越好", "rev_growth", "%",
         "與營收成長共用產業「成長」區間著色"),
    list("財務槓桿", "Total Assets / Equity", "適中／偏低較穩", "eqt_multiplier", "x",
         "權益乘數；高於區間＝紅（槓桿偏高）"),
    list("營業現金成長", "年均 YoY(Operating CF)", "越高越好", "rev_growth", "%",
         "現金動能；共用成長區間著色"),
    list("投資現金成長", "年均 YoY(Investing CF)", "視策略", "rev_growth", "%",
         "負值常見（擴產／投資流出）；著色仍對成長區間，需搭配商業邏輯解讀"),
    list("籌資現金成長", "年均 YoY(Financing CF)", "視資本結構", "rev_growth", "%",
         "借款／配息／庫藏股買回會造成正負波動"),
    list("ROA", "Net Income / Assets", "越高越好", "roa", "%",
         "資產運用效率"),
    list("ROE", "Net Income / Equity", "越高越好", "roe", "%",
         "股東報酬；過高需檢查是否槓桿推升"),
    list("資產週轉率", "Revenue / Assets", "越高越好（無區間）", NA_character_, "x",
         "目前不套產業著色（固定黑），僅供交叉閱讀"),
    list("OCF／淨利", "Operating CF / Net Income", "≥1 較佳（無區間）", NA_character_, "x",
         "盈餘品質；與 F-Score「OCF＞營業利益」相互參照，不套產業色")
  )
  do.call(rbind, lapply(rows, function(r) {
    band <- if (!is.na(r[[4]]) && nzchar(key)) {
      annotation_format_band(key, r[[4]], unit = r[[5]])
    } else {
      "—"
    }
    focus_id <- if (exists(".annotation_row_focus_id", mode = "function")) {
      .annotation_row_focus_id(r[[4]], r[[1]])
    } else {
      r[[4]]
    }
    focus_mark <- if (nzchar(prof) && exists("is_profile_focus_metric", mode = "function") &&
                      isTRUE(is_profile_focus_metric(prof, focus_id))) {
      "★"
    } else {
      "—"
    }
    data.frame(
      指標 = r[[1]],
      計算 = r[[2]],
      解讀方向 = r[[3]],
      產業標準區間 = band,
      `屬性重視` = focus_mark,
      說明 = r[[6]],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
}

#' Annotation：指標「可讀性／穩定性」參考（對齊實際 Dashboard KPI）
annotation_stability_df <- function() {
  data.frame(
    指標群組 = c(
      "獲利結構", "費用效率", "成長動能", "槓桿體質",
      "現金品質", "交叉報酬", "週轉效率"
    ),
    `代表 KPI` = c(
      "毛利率、淨利率",
      "營運費用比",
      "營收／毛利／OCF 成長",
      "財務槓桿（權益乘數）",
      "OCF／淨利、F-Score 盈餘品質",
      "ROA、ROE",
      "資產週轉率"
    ),
    解讀穩定度 = c("高", "高", "中", "中高", "高", "中高", "中"),
    使用提示 = c(
      "產業區間可比性高，適合同業色碼判斷",
      "越低越好；著色方向與獲利類相反",
      "波動大；需看多期而非單年",
      "金融／REIT 等產業基準不同，勿跨產業硬比",
      "價值陷阱預警關鍵；優於單一淨利",
      "ROE 受槓桿影響，宜與 ROA 並讀",
      "目前無同業色碼，作輔助觀察"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' 目前產業標準快覽 UI（Dashboard／Annotation 共用）
#' 數值一律即時讀自 industry_standards[[key]]，不另硬編碼區間。
#' @param industry_key 產業鍵（如 sc.Foundry）
#' @param yahoo_text 可選 Yahoo Sector/Industry 字串
#' @param show_chips 是否顯示該產業「已定義」的 KPI 區間 chips
#' @param show_title 是否顯示「目前產業標準快覽」標題
#' @param empty_message 未選產業時提示
#' @param profile_id 財報屬性 id（顯示於 Yahoo Sector/Industry 列右側）
#' @param profile_label 財報屬性顯示名
#' @param profile_title 財報屬性 hover 說明
industry_standard_snapshot_ui <- function(industry_key,
                                          yahoo_text = NULL,
                                          show_chips = TRUE,
                                          show_title = TRUE,
                                          empty_message = "尚未選擇比較產業（請至基礎設定 → Industry Standard）",
                                          profile_id = NULL,
                                          profile_label = NULL,
                                          profile_title = NULL) {
  key <- as.character(industry_key %||% "")[1]
  if (!nzchar(key) || !(key %in% names(industry_standards))) {
    return(tags$div(
      style = "margin: 0 0 12px 0; padding: 10px 12px; background: #f7f7f7; border-left: 4px solid #999; border-radius: 4px;",
      tags$span(style = "color:#666; font-size:13px;", empty_message)
    ))
  }
  lab <- industry_label(key)
  if (!nzchar(as.character(lab %||% "")[1])) lab <- key
  inds <- industry_standards[[key]]

  meta <- character(0)
  if (!is.null(inds$beta_avg) && is.finite(suppressWarnings(as.numeric(inds$beta_avg)[1]))) {
    beta <- suppressWarnings(as.numeric(inds$beta_avg)[1])
    meta <- c(meta, paste0("β≈", industry_format_number(beta, digits = 2)))
  }
  if (!is.null(inds$rm_avg) && is.finite(suppressWarnings(as.numeric(inds$rm_avg)[1]))) {
    rm <- suppressWarnings(as.numeric(inds$rm_avg)[1])
    meta <- c(meta, paste0("Rm≈", industry_format_number(rm, digits = 1), "%"))
  }
  if (!is.null(inds$debt_ratio_avg) && is.finite(suppressWarnings(as.numeric(inds$debt_ratio_avg)[1]))) {
    debt_pct <- 100 * suppressWarnings(as.numeric(inds$debt_ratio_avg)[1])
    meta <- c(meta, paste0("Debt≈", industry_format_number(debt_pct, digits = 1), "%"))
  }
  if (!is.null(inds$pb_band) && length(inds$pb_band) >= 2) {
    pb <- suppressWarnings(as.numeric(inds$pb_band))
    mid <- if (length(pb) >= 3 && is.finite(pb[3])) pb[3] else mean(pb[1:2])
    meta <- c(meta, sprintf(
      "P/B %s–%s (mid %s)",
      industry_format_number(pb[1], digits = 2),
      industry_format_number(pb[2], digits = 2),
      industry_format_number(mid, digits = 2)
    ))
  }

  chips <- NULL
  if (isTRUE(show_chips)) {
    bands <- industry_standard_bands_df(key)
    if (nrow(bands) > 0) {
      chips <- lapply(seq_len(nrow(bands)), function(i) {
        tags$div(
          class = "ynow-ann-chip ynow-ind-snapshot-chip",
          tags$span(style = "font-size:11px; color:#888;", bands$label[[i]]),
          tags$span(style = "font-weight:700; color:#222222;", bands$band[[i]])
        )
      })
    }
  }

  yahoo <- trimws(as.character(yahoo_text %||% "")[1])
  pid <- as.character(profile_id %||% "")[1]
  plab <- as.character(profile_label %||% "")[1]
  show_badge <- nzchar(pid) || nzchar(plab)
  badge <- if (isTRUE(show_badge) && exists(".ynow_fund_profile_badge_ui", mode = "function")) {
    .ynow_fund_profile_badge_ui(
      profile_id = if (nzchar(pid)) pid else "fallback",
      profile_label = plab,
      title = profile_title
    )
  } else {
    NULL
  }

  tags$div(
    style = "margin: 0 0 12px 0; padding: 12px 14px; background: #f5f5f5; border-left: 4px solid #222222; border-radius: 4px;",
    if (isTRUE(show_title)) {
      tags$div(
        style = "font-size: 13px; font-weight: 700; color: #222222; margin-bottom: 6px;",
        "目前產業標準快覽"
      )
    },
    tags$div(
      style = "font-size: 13px; color: #222; line-height: 1.45;",
      tags$b(lab),
      tags$span(style = "color:#888; font-size:12px; margin-left:6px;", paste0("(", key, ")")),
      if (length(meta)) {
        tags$span(
          style = "margin-left:10px; color:#555; font-size:12px;",
          paste(meta, collapse = " · ")
        )
      }
    ),
    if (nzchar(yahoo) || !is.null(badge)) {
      tags$div(
        class = "ynow-ind-yahoo-row",
        tags$div(
          class = "ynow-ind-yahoo-text",
          if (nzchar(yahoo)) {
            tagList(
              tags$span(style = "font-weight:600;", "Yahoo："),
              yahoo
            )
          }
        ),
        if (!is.null(badge)) badge
      )
    },
    if (!is.null(chips)) {
      tags$div(
        class = "ynow-ann-legend ynow-ind-snapshot-chips",
        style = "margin-top:10px; margin-bottom:0;",
        chips
      )
    }
  )
}

