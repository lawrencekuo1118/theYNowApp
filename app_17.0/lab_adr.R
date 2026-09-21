# ==========================================
# lab_adr.R — US-listed ADR / foreign-issuer helpers
# 用於：產業覆寫（避免非 S&P ADR 落入「未對應產業」）、Blue Chip「含 ADR」篩選
# ==========================================

LAB_ADR_UNMAPPED_KEY <- "lab.Unmapped"

#' Major US-listed ADRs / foreign issuers → App industry_key
#' Curated for Blue Chip Lab; extend as needed (no valuation formula branches).
lab_adr_industry_map <- function() {
  c(
    # Semiconductors / tech hardware
    TSM = "sc.Foundry",
    UMC = "sc.Foundry",
    ASX = "sc.Foundry",
    SKHY = "sc.Memory",
    ASML = "sc.Equipment",
    ARM = "sc.IC_Design",
    STM = "sc.IC_Design",
    NOK = "tel.Telecom",
    ERIC = "tel.Telecom",
    # China / HK tech & consumer platforms
    BABA = "ecr.Ecommerce_Retail",
    PDD = "ecr.Ecommerce_Retail",
    JD = "ecr.Ecommerce_Retail",
    BIDU = "tech.IT_Services",
    BILI = "media.Entertainment",
    TME = "media.Entertainment",
    IQ = "media.Entertainment",
    VIPS = "ecr.Ecommerce_Retail",
    NIO = "auto.EV_Startups",
    XPEV = "auto.EV_Startups",
    LI = "auto.EV_Startups",
    BZ = "saas.SaaS_Cloud",
    YUMC = "cons.Restaurants",
    HTHT = "hosp.Hotels_Travel",
    TCOM = "hosp.Hotels_Travel",
    # Europe / UK / Nordics
    SAP = "saas.SaaS_Cloud",
    NVO = "hc.Pharma",
    NVS = "hc.Pharma",
    SNY = "hc.Pharma",
    AZN = "hc.Pharma",
    GSK = "hc.Pharma",
    RHHBY = "hc.Pharma",
    TAK = "hc.Pharma",
    UL = "fmcg.Household_Personal",
    DEO = "fmcg.Food_Beverages",
    BUD = "fmcg.Food_Beverages",
    CCEP = "fmcg.Food_Beverages",
    SHEL = "en.Energy_OilGas",
    TTE = "en.Energy_OilGas",
    BP = "en.Energy_OilGas",
    EQNR = "en.Energy_OilGas",
    ENB = "en.Utilities",
    # Autos / industrials
    TM = "auto.Automotive_EV",
    HMC = "auto.Automotive_EV",
    SONY = "tech.Hardware",
    SNE = "tech.Hardware",
    RACE = "auto.Automotive_EV",
    STLA = "auto.Automotive_EV",
    VWAGY = "auto.Automotive_EV",
    # Financials
    HSBC = "fn.Banking",
    BCS = "fn.Banking",
    LYG = "fn.Banking",
    DB = "fn.Banking",
    ING = "fn.Banking",
    SAN = "fn.Banking",
    BBVA = "fn.Banking",
    ITUB = "fn.Banking",
    BBD = "fn.Banking",
    PBR = "en.Energy_OilGas",
    VALE = "mat.Metals_Mining",
    RIO = "mat.Metals_Mining",
    BHP = "mat.Metals_Mining",
    # LatAm / other
    MELI = "ecr.Ecommerce_Retail",
    NU = "fn.Banking",
    XP = "fn.Investment_Banking",
    ABEV = "fmcg.Food_Beverages",
    # Korea / Japan / Taiwan extras
    KB = "fn.Banking",
    SHG = "fn.Banking",
    WF = "fn.Banking",
    PKX = "mat.Metals_Mining",
    LPL = "tech.Optoelectronics",
    KT = "tel.Telecom",
    SKM = "tel.Telecom",
    INFY = "tech.IT_Services",
    WIT = "tech.IT_Services",
    HDB = "fn.Banking",
    IBN = "fn.Banking",
    # More Europe
    RELX = "media.Advertising",
    PHG = "hc.Medtech",
    ORAN = "tel.Telecom",
    VOD = "tel.Telecom",
    TEF = "tel.Telecom",
    AMX = "tel.Telecom",
    SPOT = "media.Entertainment",
    SE = "ecr.Ecommerce_Retail",
    GRAB = "tr.Logistics_Shipping",
    CPNG = "ecr.Ecommerce_Retail",
    # Misc liquid ADRs
    NTES = "media.Entertainment",
    EDU = "bus.Professional_Services",
    TAL = "bus.Professional_Services",
    GDS = "saas.SaaS_Cloud",
    BEKE = "tech.Internet_Platform",
    FUTU = "fn.Fintech",
    TIGR = "fn.Fintech",
    ZTO = "tr.Logistics_Shipping",
    YMM = "tr.Logistics_Shipping",
    BGNE = "hc.Biotech",
    LEGN = "hc.Biotech"
  )
}

#' Strong foreign-issuer name patterns (US CORP/INC alone are NOT enough)
lab_adr_name_looks_foreign <- function(name) {
  nm <- as.character(name %||% "")[1]
  if (!nzchar(nm) || is.na(nm)) return(FALSE)
  grepl(
    paste0(
      "\\bPLC\\b|\\bN\\.?V\\.?\\b|\\bS\\.?A\\.?\\b|\\bA/?S\\b|\\bAG\\b|",
      "\\bSE\\b|\\bS\\.?p\\.?A\\.?\\b|/UK\\b|/DE\\b|/NL\\b|",
      "TAIWAN|KOREA|JAPAN|CHINA|NETHERLANDS|SWITZERLAND|SWEDEN|",
      "DENMARK|NORWAY|BRAZIL|MEXICO|INDIA|ISRAEL|LUXEMBOURG|",
      "HOLDINGS PLC|GROUP PLC|GROUP LTD"
    ),
    nm,
    ignore.case = TRUE,
    perl = TRUE
  )
}

#' US primary-listing tickers that are known domestic (avoid false ADR from "SE"/"AG" noise)
lab_adr_known_domestic <- function() {
  c(
    "AAPL", "MSFT", "NVDA", "AMZN", "GOOGL", "GOOG", "META", "TSLA", "AVGO",
    "JPM", "V", "MA", "UNH", "XOM", "JNJ", "WMT", "PG", "HD", "CVX", "MRK",
    "ABBV", "KO", "PEP", "COST", "LLY", "ORCL", "AMD", "QCOM", "INTC", "MU",
    "AMAT", "LRCX", "KLAC", "ADI", "TXN", "CSCO", "IBM", "CRM", "NOW", "ADBE",
    "NFLX", "DIS", "CMCSA", "T", "VZ", "BA", "CAT", "GE", "HON", "UPS", "FDX",
    "GS", "MS", "C", "BAC", "WFC", "BLK", "SCHW", "AXP", "COF", "USB",
    "ENPH", "FSLR", "RIVN", "LCID"
  )
}

#' Is this US-universe row an ADR / foreign issuer for Blue Chip filter?
lab_is_us_adr <- function(ticker, name = NULL) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk) || is.na(tk)) return(FALSE)
  if (tk %in% lab_adr_known_domestic()) return(FALSE)
  amap <- lab_adr_industry_map()
  if (tk %in% names(amap)) return(TRUE)
  lab_adr_name_looks_foreign(name)
}

#' Apply ADR industry map to still-Unmapped rows; set is_adr column
lab_us_overlay_adr_industry <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(df)
  if (!("ticker" %in% names(df))) return(df)
  unmapped <- if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) {
    LAB_UNMAPPED_KEY
  } else {
    LAB_ADR_UNMAPPED_KEY
  }
  tks <- toupper(trimws(as.character(df$ticker)))
  nms <- if ("name" %in% names(df)) as.character(df$name) else rep("", length(tks))
  amap <- lab_adr_industry_map()
  if (!("industry_key" %in% names(df))) df$industry_key <- unmapped
  keys <- as.character(df$industry_key)
  need <- which(
    tks %in% names(amap) &
      (is.na(keys) | !nzchar(keys) | keys == unmapped)
  )
  if (length(need)) {
    df$industry_key[need] <- unname(amap[tks[need]])
  }
  # Optional: bundled snapshot for extra ADR industries
  snap <- tryCatch(lab_load_adr_industry_snapshot(), error = function(e) NULL)
  if (!is.null(snap) && is.data.frame(snap) && nrow(snap) > 0L &&
      all(c("ticker", "industry_key") %in% names(snap))) {
    keys <- as.character(df$industry_key)
    idx <- match(tks, toupper(trimws(as.character(snap$ticker))))
    hit <- which(
      is.finite(idx) &
        (is.na(keys) | !nzchar(keys) | keys == unmapped) &
        nzchar(as.character(snap$industry_key[idx])) &
        !is.na(snap$industry_key[idx]) &
        as.character(snap$industry_key[idx]) != unmapped
    )
    if (length(hit)) {
      df$industry_key[hit] <- as.character(snap$industry_key[idx[hit]])
    }
  }
  df$is_adr <- vapply(
    seq_along(tks),
    function(i) isTRUE(lab_is_us_adr(tks[[i]], nms[[i]])),
    logical(1)
  )
  df
}

lab_adr_industry_snapshot_path <- function() {
  candidates <- c(
    file.path("data", "us_adr_industry_snapshot.csv"),
    file.path("app_17.0", "data", "us_adr_industry_snapshot.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

.lab_adr_snap_cache <- new.env(parent = emptyenv())

lab_load_adr_industry_snapshot <- function(force = FALSE) {
  if (!isTRUE(force) && exists("df", envir = .lab_adr_snap_cache, inherits = FALSE)) {
    return(get("df", envir = .lab_adr_snap_cache, inherits = FALSE))
  }
  path <- lab_adr_industry_snapshot_path()
  empty <- data.frame(
    ticker = character(0),
    industry_key = character(0),
    stringsAsFactors = FALSE
  )
  if (!nzchar(path) || is.na(path) || !file.exists(path)) {
    assign("df", empty, envir = .lab_adr_snap_cache)
    return(empty)
  }
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || !"ticker" %in% names(raw)) {
    assign("df", empty, envir = .lab_adr_snap_cache)
    return(empty)
  }
  df <- data.frame(
    ticker = toupper(trimws(as.character(raw$ticker))),
    industry_key = if ("industry_key" %in% names(raw)) {
      as.character(raw$industry_key)
    } else {
      rep(NA_character_, nrow(raw))
    },
    stringsAsFactors = FALSE
  )
  df <- df[nzchar(df$ticker) & !is.na(df$ticker), , drop = FALSE]
  df <- df[!duplicated(df$ticker), , drop = FALSE]
  assign("df", df, envir = .lab_adr_snap_cache)
  df
}

#' Drop ADR rows when include_adr is FALSE
lab_filter_pool_adr <- function(pool, include_adr = TRUE) {
  if (isTRUE(include_adr)) return(pool)
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0L) return(pool)
  if ("is_adr" %in% names(pool)) {
    keep <- !as.logical(pool$is_adr)
    keep[is.na(keep)] <- TRUE
    return(pool[keep, , drop = FALSE])
  }
  nms <- if ("name" %in% names(pool)) as.character(pool$name) else rep("", nrow(pool))
  tks <- as.character(pool$ticker)
  is_a <- vapply(
    seq_len(nrow(pool)),
    function(i) isTRUE(lab_is_us_adr(tks[[i]], nms[[i]])),
    logical(1)
  )
  pool[!is_a, , drop = FALSE]
}
