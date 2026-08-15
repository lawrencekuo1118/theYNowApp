# ==========================================
# lab_sp500_universe.R — Lab 宇宙：可更新 S&P 500 成分股
# 來源優先 Wikipedia 成分表，失敗則 GitHub datasets CSV。
# 快取 data/sp500_universe.csv；過期約 7 天可背景更新，不阻擋首屏。
# ==========================================

LAB_UNMAPPED_KEY <- "lab.Unmapped"
LAB_UNMAPPED_LABEL <- "未對應產業"
LAB_SP500_STALE_DAYS <- 7
LAB_SP500_CACHE_REL <- file.path("data", "sp500_universe.csv")
LAB_SP500_WIKI_URL <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
LAB_SP500_GITHUB_URL <- "https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv"
LAB_INDEX_ETFS <- c("SPY", "QQQ", "DIA", "IWM")

.lab_sp500_env <- new.env(parent = emptyenv())

#' Yahoo 代碼：Wikipedia 的 BRK.B → BRK-B
lab_yahoo_symbol <- function(sym) {
  tk <- toupper(trimws(as.character(sym %||% "")[1]))
  if (!nzchar(tk) || identical(tk, "NA")) return(NA_character_)
  gsub("\\.", "-", gsub("/", "-", tk))
}

#' GICS sub-industry → App industry_standards 鍵（全表對應，避免默默丟檔）
lab_gics_subindustry_map <- function() {
  c(
    "Advertising" = "media.Entertainment",
    "Aerospace & Defense" = "ind.Aerospace_Defense",
    "Agricultural & Farm Machinery" = "ind.Machinery",
    "Agricultural Products & Services" = "fmcg.Food_Beverages",
    "Air Freight & Logistics" = "tr.Logistics_Shipping",
    "Apparel Retail" = "retail.Brick_Mortar",
    "Apparel, Accessories & Luxury Goods" = "lxg.Luxury_Fashion",
    "Application Software" = "saas.SaaS_Cloud",
    "Asset Management & Custody Banks" = "fn.Asset_Management",
    "Automobile Manufacturers" = "auto.Vehicle_Manufacturing",
    "Automotive Parts & Equipment" = "auto.Parts_Suppliers",
    "Automotive Retail" = "retail.Brick_Mortar",
    "Biotechnology" = "hc.Biotech",
    "Brewers" = "fmcg.Food_Beverages",
    "Broadcasting" = "media.Entertainment",
    "Broadline Retail" = "ecr.Ecommerce_Retail",
    "Building Products" = "ind.Construction",
    "Cable & Satellite" = "tel.Telecom",
    "Cargo Ground Transportation" = "tr.Logistics_Shipping",
    "Casinos & Gaming" = "media.Gaming",
    "Commodity Chemicals" = "mat.Chemicals",
    "Communications Equipment" = "ec.Hardware",
    "Computer & Electronics Retail" = "retail.Brick_Mortar",
    "Construction & Engineering" = "ind.Construction",
    "Construction Machinery & Heavy Transportation Equipment" = "ind.Machinery",
    "Construction Materials" = "ind.Construction",
    "Consumer Electronics" = "tech.Hardware",
    "Consumer Finance" = "fn.Fintech",
    "Consumer Staples Merchandise Retail" = "retail.Brick_Mortar",
    "Copper" = "mat.Metals_Mining",
    "Data Center REITs" = "re.REIT",
    "Data Processing & Outsourced Services" = "fn.Fintech",
    "Distillers & Vintners" = "fmcg.Food_Beverages",
    "Distributors" = "cons.Discretionary",
    "Diversified Banks" = "fn.Banking",
    "Diversified Support Services" = "ind.Machinery",
    "Electric Utilities" = "en.Utilities",
    "Electrical Components & Equipment" = "ec.Hardware",
    "Electronic Components" = "ec.Hardware",
    "Electronic Equipment & Instruments" = "ec.Hardware",
    "Electronic Manufacturing Services" = "ec.Hardware",
    "Environmental & Facilities Services" = "ind.Construction",
    "Fertilizers & Agricultural Chemicals" = "mat.Chemicals",
    "Financial Exchanges & Data" = "fn.Asset_Management",
    "Food Distributors" = "fmcg.Food_Beverages",
    "Food Retail" = "retail.Brick_Mortar",
    "Footwear" = "lxg.Luxury_Fashion",
    "Gas Utilities" = "en.Utilities",
    "Gold" = "mat.Metals_Mining",
    "Health Care Distributors" = "hc.Healthcare_Services",
    "Health Care Equipment" = "hc.Medtech",
    "Health Care Facilities" = "hc.Healthcare_Services",
    "Health Care REITs" = "re.REIT",
    "Health Care Services" = "hc.Healthcare_Services",
    "Health Care Supplies" = "hc.Medtech",
    "Health Care Technology" = "hc.Healthcare_Services",
    "Heavy Electrical Equipment" = "ind.Machinery",
    "Home Improvement Retail" = "retail.Brick_Mortar",
    "Homebuilding" = "ind.Construction",
    "Homefurnishing Retail" = "retail.Brick_Mortar",
    "Hotel & Resort REITs" = "re.REIT",
    "Hotels, Resorts & Cruise Lines" = "hosp.Hotels_Travel",
    "Household Products" = "fmcg.Household_Personal",
    "Human Resource & Employment Services" = "cons.Discretionary",
    "Independent Power Producers & Energy Traders" = "en.Utilities",
    "Industrial Conglomerates" = "ind.Machinery",
    "Industrial Gases" = "mat.Chemicals",
    "Industrial Machinery & Supplies & Components" = "ind.Machinery",
    "Industrial REITs" = "re.REIT",
    "Insurance Brokers" = "fn.Insurance",
    "Integrated Oil & Gas" = "en.Energy_OilGas",
    "Integrated Telecommunication Services" = "tel.Telecom",
    "Interactive Home Entertainment" = "media.Gaming",
    "Interactive Media & Services" = "tech.Internet_Platform",
    "Internet Services & Infrastructure" = "saas.SaaS_Cloud",
    "Investment Banking & Brokerage" = "fn.Investment_Banking",
    "IT Consulting & Other Services" = "tech.Software",
    "Leisure Products" = "cons.Discretionary",
    "Life & Health Insurance" = "fn.Insurance",
    "Life Sciences Tools & Services" = "hc.Medtech",
    "Managed Health Care" = "hc.Healthcare_Services",
    "Metal, Glass & Plastic Containers" = "mat.Chemicals",
    "Movies & Entertainment" = "media.Entertainment",
    "Multi-Family Residential REITs" = "re.REIT",
    "Multi-line Insurance" = "fn.Insurance",
    "Multi-Sector Holdings" = "fn.Conglomerate_Holding",
    "Multi-Utilities" = "en.Utilities",
    "Office REITs" = "re.REIT",
    "Oil & Gas Equipment & Services" = "en.Energy_OilGas",
    "Oil & Gas Exploration & Production" = "en.Energy_OilGas",
    "Oil & Gas Refining & Marketing" = "en.Energy_OilGas",
    "Oil & Gas Storage & Transportation" = "en.Energy_OilGas",
    "Other Specialized REITs" = "re.REIT",
    "Other Specialty Retail" = "retail.Brick_Mortar",
    "Packaged Foods & Meats" = "fmcg.Food_Beverages",
    "Paper & Plastic Packaging Products & Materials" = "mat.Chemicals",
    "Passenger Airlines" = "tr.Airlines",
    "Passenger Ground Transportation" = "tr.Logistics_Shipping",
    "Personal Care Products" = "fmcg.Health_Beauty",
    "Pharmaceuticals" = "hc.Pharma",
    "Property & Casualty Insurance" = "fn.Insurance",
    "Publishing" = "media.Entertainment",
    "Rail Transportation" = "tr.Logistics_Shipping",
    "Real Estate Services" = "re.REIT",
    "Regional Banks" = "fn.Banking",
    "Reinsurance" = "fn.Insurance",
    "Research & Consulting Services" = "ind.Machinery",
    "Restaurants" = "cons.Discretionary",
    "Retail REITs" = "re.REIT",
    "Self-Storage REITs" = "re.REIT",
    "Semiconductor Materials & Equipment" = "sc.Equipment",
    "Semiconductors" = "sc.IC_Design",
    "Single-Family Residential REITs" = "re.REIT",
    "Soft Drinks & Non-alcoholic Beverages" = "fmcg.Food_Beverages",
    "Specialized Consumer Services" = "cons.Discretionary",
    "Specialty Chemicals" = "mat.Chemicals",
    "Steel" = "mat.Metals_Mining",
    "Systems Software" = "tech.Software",
    "Technology Distributors" = "ec.Hardware",
    "Technology Hardware, Storage & Peripherals" = "tech.Hardware",
    "Telecom Tower REITs" = "re.REIT",
    "Timber REITs" = "re.REIT",
    "Tobacco" = "fmcg.Food_Beverages",
    "Trading Companies & Distributors" = "ind.Machinery",
    "Transaction & Payment Processing Services" = "fn.Fintech",
    "Water Utilities" = "en.Utilities",
    "Wireless Telecommunication Services" = "tel.Telecom"
  )
}

lab_gics_sector_fallback <- function() {
  c(
    "Communication Services" = "media.Entertainment",
    "Consumer Discretionary" = "cons.Discretionary",
    "Consumer Staples" = "fmcg.Food_Beverages",
    "Energy" = "en.Energy_OilGas",
    "Financials" = "fn.Banking",
    "Health Care" = "hc.Healthcare_Services",
    "Industrials" = "ind.Machinery",
    "Information Technology" = "tech.Software",
    "Materials" = "mat.Chemicals",
    "Real Estate" = "re.REIT",
    "Utilities" = "en.Utilities"
  )
}

#' 比 GICS 更細的個股覆寫（僅 S&P 內、且 App 有對應鍵時）
lab_ticker_industry_overrides <- function() {
  c(
    TSLA = "auto.Automotive_EV",
    RIVN = "auto.EV_Startups",
    LCID = "auto.EV_Startups",
    MU = "sc.Memory",
    ENPH = "en.Renewables",
    FSLR = "en.Renewables"
  )
}

lab_map_gics_to_industry_key <- function(sector, sub_industry, ticker = "") {
  tk <- lab_yahoo_symbol(ticker)
  ov <- lab_ticker_industry_overrides()
  if (!is.na(tk) && nzchar(tk) && tk %in% names(ov)) return(unname(ov[[tk]]))
  sub <- trimws(as.character(sub_industry %||% "")[1])
  smap <- lab_gics_subindustry_map()
  if (nzchar(sub) && sub %in% names(smap)) return(unname(smap[[sub]]))
  sec <- trimws(as.character(sector %||% "")[1])
  fmap <- lab_gics_sector_fallback()
  if (nzchar(sec) && sec %in% names(fmap)) return(unname(fmap[[sec]]))
  LAB_UNMAPPED_KEY
}

lab_sp500_cache_paths <- function() {
  unique(c(
    LAB_SP500_CACHE_REL,
    file.path(getwd(), LAB_SP500_CACHE_REL),
    file.path("app_14.0", LAB_SP500_CACHE_REL)
  ))
}

lab_sp500_existing_cache <- function() {
  hits <- lab_sp500_cache_paths()
  hits <- hits[file.exists(hits)]
  if (!length(hits)) return(NA_character_)
  normalizePath(hits[[1]], mustWork = FALSE)
}

.lab_pick_col <- function(df, patterns) {
  nms <- names(df)
  for (p in patterns) {
    hit <- grep(p, nms, ignore.case = TRUE)
    if (length(hit) > 0) return(df[[hit[1]]])
  }
  NULL
}

.lab_http_download <- function(url, dest, timeout_sec = 12) {
  old <- getOption("timeout")
  on.exit(options(timeout = old), add = TRUE)
  options(timeout = timeout_sec)
  ua <- "theYNowApp/14.0 (lab S&P 500 universe)"
  ok <- tryCatch({
    utils::download.file(
      url, destfile = dest, quiet = TRUE, mode = "wb",
      headers = c("User-Agent" = ua)
    )
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    ok <- tryCatch({
      utils::download.file(url, destfile = dest, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) FALSE)
  }
  isTRUE(ok) && file.exists(dest) && isTRUE(file.info(dest)$size > 80)
}

lab_empty_sp500 <- function() {
  data.frame(
    ticker = character(0),
    name = character(0),
    gics_sector = character(0),
    gics_sub_industry = character(0),
    industry_key = character(0),
    fetched_at = character(0),
    source = character(0),
    stringsAsFactors = FALSE
  )
}

lab_is_excluded_lab_ticker <- function(raw_sym, yahoo_sym) {
  raw <- toupper(trimws(as.character(raw_sym %||% "")[1]))
  y <- toupper(trimws(as.character(yahoo_sym %||% "")[1]))
  if (grepl("\\.TW$|\\.TWO$", raw, ignore.case = TRUE)) return(TRUE)
  if (grepl("-TW$|-TWO$", y, ignore.case = TRUE)) return(TRUE)
  y %in% LAB_INDEX_ETFS
}

#' 正規化成分表並套產業對應（每次載入重算 mapping，不必重抓）
lab_finalize_sp500 <- function(raw_df, source, fetched_at = NULL) {
  empty <- lab_empty_sp500()
  if (is.null(raw_df) || !is.data.frame(raw_df) || nrow(raw_df) == 0) return(empty)
  sym <- .lab_pick_col(raw_df, c("^Symbol$", "^Ticker$", "^Ticker.symbol$"))
  nm <- .lab_pick_col(raw_df, c("^Security$", "^Name$", "^Company$"))
  sec <- .lab_pick_col(raw_df, c("GICS.?Sector", "^Sector$"))
  sub <- .lab_pick_col(raw_df, c("GICS.?Sub", "Sub.?Industry"))
  if (is.null(sym)) return(empty)
  n <- length(sym)
  if (is.null(nm)) nm <- rep(NA_character_, n)
  if (is.null(sec)) sec <- rep(NA_character_, n)
  if (is.null(sub)) sub <- rep(NA_character_, n)
  ts <- as.character(fetched_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))[1]
  rows <- vector("list", n)
  seen <- character(0)
  for (i in seq_len(n)) {
    y <- lab_yahoo_symbol(sym[[i]])
    if (is.na(y) || !nzchar(y)) next
    if (lab_is_excluded_lab_ticker(sym[[i]], y)) next
    if (y %in% seen) next
    seen <- c(seen, y)
    key <- lab_map_gics_to_industry_key(sec[[i]], sub[[i]], y)
    cname <- trimws(as.character(nm[[i]] %||% "")[1])
    if (is.na(cname)) cname <- ""
    rows[[length(rows) + 1L]] <- data.frame(
      ticker = y,
      name = cname,
      gics_sector = trimws(as.character(sec[[i]] %||% "")[1]),
      gics_sub_industry = trimws(as.character(sub[[i]] %||% "")[1]),
      industry_key = key,
      fetched_at = ts,
      source = as.character(source)[1],
      stringsAsFactors = FALSE
    )
  }
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

lab_fetch_sp500_wikipedia <- function(timeout_sec = 12) {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  if (!isTRUE(.lab_http_download(LAB_SP500_WIKI_URL, tmp, timeout_sec))) {
    stop("Wikipedia download failed")
  }
  pg <- xml2::read_html(tmp)
  tabs <- rvest::html_table(pg, fill = TRUE)
  if (!length(tabs)) stop("Wikipedia has no tables")
  d <- as.data.frame(tabs[[1]], stringsAsFactors = FALSE)
  lab_finalize_sp500(d, source = "wikipedia", fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

lab_fetch_sp500_github <- function(timeout_sec = 12) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  if (!isTRUE(.lab_http_download(LAB_SP500_GITHUB_URL, tmp, timeout_sec))) {
    stop("GitHub constituents download failed")
  }
  d <- utils::read.csv(tmp, stringsAsFactors = FALSE, encoding = "UTF-8")
  lab_finalize_sp500(d, source = "github-datasets", fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

lab_try_fetch_sp500 <- function(timeout_sec = 12) {
  wiki <- tryCatch(lab_fetch_sp500_wikipedia(timeout_sec), error = function(e) NULL)
  if (is.data.frame(wiki) && nrow(wiki) >= 400L) return(wiki)
  gh <- tryCatch(lab_fetch_sp500_github(timeout_sec), error = function(e) NULL)
  if (is.data.frame(gh) && nrow(gh) >= 400L) return(gh)
  if (is.data.frame(wiki) && nrow(wiki) > 0) return(wiki)
  if (is.data.frame(gh) && nrow(gh) > 0) return(gh)
  NULL
}

lab_read_sp500_csv <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) return(NULL)
  d <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d) || !("ticker" %in% names(d))) return(NULL)
  src <- if ("source" %in% names(d)) as.character(d$source[1]) else "bundled"
  ts <- if ("fetched_at" %in% names(d)) as.character(d$fetched_at[1]) else NA_character_
  # 重套 mapping，讓產業規則更新不必重抓
  lab_finalize_sp500(d, source = src, fetched_at = ts)
}

lab_load_sp500_cache <- function() {
  p <- lab_sp500_existing_cache()
  if (is.na(p)) return(NULL)
  lab_read_sp500_csv(p)
}

lab_save_sp500_cache <- function(df) {
  if (is.null(df) || !nrow(df)) return(invisible(FALSE))
  dests <- unique(c(
    LAB_SP500_CACHE_REL,
    file.path(getwd(), LAB_SP500_CACHE_REL)
  ))
  keep <- df[, c("ticker", "name", "gics_sector", "gics_sub_industry",
                 "fetched_at", "source"), drop = FALSE]
  for (p in dests) {
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    ok <- tryCatch({
      utils::write.csv(keep, p, row.names = FALSE, fileEncoding = "UTF-8")
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok) && file.exists(p)) {
      .lab_sp500_env$cache_path <- normalizePath(p, mustWork = FALSE)
      return(invisible(TRUE))
    }
  }
  tp <- file.path(tempdir(), "sp500_universe.csv")
  tryCatch({
    utils::write.csv(keep, tp, row.names = FALSE, fileEncoding = "UTF-8")
    .lab_sp500_env$cache_path <- tp
    TRUE
  }, error = function(e) FALSE)
}

lab_sp500_cache_age_days <- function(df) {
  if (is.null(df) || !nrow(df) || !("fetched_at" %in% names(df))) return(Inf)
  ts <- as.character(df$fetched_at[1])
  t <- suppressWarnings(as.POSIXct(ts, tz = "UTC"))
  if (is.na(t)) t <- suppressWarnings(as.POSIXct(substr(ts, 1, 10), tz = "UTC"))
  if (is.na(t)) return(Inf)
  as.numeric(difftime(Sys.time(), t, units = "days"))
}

lab_sp500_is_stale <- function(df = NULL) {
  df <- df %||% .lab_sp500_env$universe
  if (is.null(df) || !nrow(df)) return(TRUE)
  age <- lab_sp500_cache_age_days(df)
  !is.finite(age) || age > LAB_SP500_STALE_DAYS
}

#' 讀取宇宙：預設只用快取／內建快照，不打網路
lab_get_sp500_universe <- function(force_refresh = FALSE) {
  if (!isTRUE(force_refresh) && is.data.frame(.lab_sp500_env$universe) &&
      nrow(.lab_sp500_env$universe) > 0) {
    return(.lab_sp500_env$universe)
  }
  loaded <- lab_load_sp500_cache()
  if (!isTRUE(force_refresh) && is.data.frame(loaded) && nrow(loaded) > 0) {
    .lab_sp500_env$universe <- loaded
    return(loaded)
  }
  if (isTRUE(force_refresh)) {
    fetched <- lab_try_fetch_sp500()
    if (is.data.frame(fetched) && nrow(fetched) > 0) {
      lab_save_sp500_cache(fetched)
      .lab_sp500_env$universe <- fetched
      return(fetched)
    }
  }
  if (is.data.frame(loaded) && nrow(loaded) > 0) {
    .lab_sp500_env$universe <- loaded
    return(loaded)
  }
  empty <- lab_empty_sp500()
  .lab_sp500_env$universe <- empty
  empty
}

lab_refresh_sp500_universe <- function() {
  fetched <- lab_try_fetch_sp500()
  if (!is.data.frame(fetched) || nrow(fetched) < 1) return(NULL)
  lab_save_sp500_cache(fetched)
  .lab_sp500_env$universe <- fetched
  fetched
}

lab_sp500_universe_meta <- function() {
  u <- tryCatch(lab_get_sp500_universe(FALSE), error = function(e) lab_empty_sp500())
  n <- if (is.null(u) || !nrow(u)) 0L else length(unique(u$ticker[nzchar(u$ticker)]))
  n_unmap <- if (is.null(u) || !nrow(u)) 0L else
    sum(u$industry_key == LAB_UNMAPPED_KEY, na.rm = TRUE)
  fetched_at <- if (!is.null(u) && nrow(u)) as.character(u$fetched_at[1]) else NA_character_
  src <- if (!is.null(u) && nrow(u) && "source" %in% names(u)) as.character(u$source[1]) else NA_character_
  list(
    n = as.integer(n),
    n_unmapped = as.integer(n_unmap),
    fetched_at = fetched_at,
    source = src,
    stale = lab_sp500_is_stale(u)
  )
}

lab_sp500_company_name <- function(ticker) {
  tk <- lab_yahoo_symbol(ticker)
  u <- .lab_sp500_env$universe
  if (!is.data.frame(u) || !nrow(u) || is.na(tk)) return(NA_character_)
  hit <- u$name[match(tk, u$ticker)]
  hit <- trimws(as.character(hit %||% "")[1])
  if (!nzchar(hit) || identical(hit, "NA")) return(NA_character_)
  hit
}

lab_format_fetched_at <- function(x) {
  s <- as.character(x %||% "")[1]
  if (!nzchar(s) || identical(s, "NA")) return("—")
  t <- suppressWarnings(as.POSIXct(s, tz = "UTC"))
  if (is.na(t)) t <- suppressWarnings(as.POSIXct(substr(s, 1, 10), tz = "UTC"))
  if (is.na(t)) return(s)
  format(t, "%Y-%m-%d")
}

#' Lab 產業 picker：App 產業＋未對應桶（不污染 Get Started）
lab_industry_picker_choices <- function() {
  base <- industry_picker_choices()
  extra <- stats::setNames(LAB_UNMAPPED_KEY, LAB_UNMAPPED_LABEL)
  c(base, extra)
}

# 啟動只讀快取，不打網路
tryCatch(lab_get_sp500_universe(force_refresh = FALSE), error = function(e) NULL)
