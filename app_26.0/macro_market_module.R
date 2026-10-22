# ==========================================
# macro_market_module.R — 總體經濟與大盤趨勢
#
# 訂閱全域 market_mode（US／TW），不另建市場開關。
# 指數／板塊歷史序列維持 Yahoo 原始報價幣別，不做歷史 FX 換算。
# Theme vs benchmark overlay 與泡沫／集中度僅供研究顯示，絕不可寫入 CAPM／Ke／WACC。
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ---- Catalogs (no FX; Yahoo native quotes) ----

.MACRO_US_INDICES <- c(
  "^GSPC" = "S&P 500",
  "^IXIC" = "Nasdaq",
  "^DJI" = "Dow Jones",
  "^SOX" = "SOX (semis)"
)

# Taiwan boards (Yahoo native TWD): TAIEX, TPEx, and Yuanta Taiwan 50 (0050.TW).
# TPEx cap-weighted index is IX0043.TWO. Yahoo chart history for that symbol is
# effectively empty (often a single live bar; ^TWOII is delisted). Macro / price
# fetch falls back to TPEx official tradingIndex (櫃買指數) via fetch_tpex_index_history_df().
# Industry sub-indices (Electronics / Finance) are not Macro 大盤指標.
.MACRO_TW_INDICES <- c(
  "^TWII" = "TAIEX",
  "IX0043.TWO" = "TPEx",
  "0050.TW" = "0050"
)

# GICS-ish US sector ETFs (11 SPDR sectors)
.MACRO_US_GICS <- c(
  gics_xlk = "XLK",
  gics_xlv = "XLV",
  gics_xlf = "XLF",
  gics_xle = "XLE",
  gics_xli = "XLI",
  gics_xly = "XLY",
  gics_xlp = "XLP",
  gics_xlu = "XLU",
  gics_xlb = "XLB",
  gics_xlre = "XLRE",
  gics_xlc = "XLC"
)

.MACRO_US_GICS_LABELS_EN <- c(
  gics_xlk = "Technology (XLK)",
  gics_xlv = "Health Care (XLV)",
  gics_xlf = "Financials (XLF)",
  gics_xle = "Energy (XLE)",
  gics_xli = "Industrials (XLI)",
  gics_xly = "Consumer Discretionary (XLY)",
  gics_xlp = "Consumer Staples (XLP)",
  gics_xlu = "Utilities (XLU)",
  gics_xlb = "Materials (XLB)",
  gics_xlre = "Real Estate (XLRE)",
  gics_xlc = "Communication Services (XLC)"
)

.MACRO_US_GICS_LABELS_ZH <- c(
  gics_xlk = "科技（XLK）",
  gics_xlv = "醫療保健（XLV）",
  gics_xlf = "金融（XLF）",
  gics_xle = "能源（XLE）",
  gics_xli = "工業（XLI）",
  gics_xly = "非必需消費（XLY）",
  gics_xlp = "必需消費（XLP）",
  gics_xlu = "公用事業（XLU）",
  gics_xlb = "原物料（XLB）",
  gics_xlre = "不動產（XLRE）",
  gics_xlc = "通訊服務（XLC）"
)

macro_bench_ticker <- function(mode = get_market_mode()) {
  if (identical(normalize_market_mode(mode), "TW")) "^TWII" else "^GSPC"
}

macro_index_specs <- function(mode = get_market_mode()) {
  if (identical(normalize_market_mode(mode), "TW")) .MACRO_TW_INDICES else .MACRO_US_INDICES
}

# Click-to-chart KPI strip: US four or TW three (TAIEX / TPEx / 0050), by market mode.
.MACRO_CLICK_INDEX_BOX_IDS <- c(
  "^GSPC" = "ynow_macro_idx_gspc",
  "^IXIC" = "ynow_macro_idx_ixic",
  "^DJI" = "ynow_macro_idx_dji",
  "^SOX" = "ynow_macro_idx_sox",
  "^TWII" = "ynow_macro_idx_twii",
  "IX0043.TWO" = "ynow_macro_idx_twoii",
  "0050.TW" = "ynow_macro_idx_0050"
)
.MACRO_CLICK_INDEX_NAME_KEYS <- c(
  "^GSPC" = "macro_index_name_gspc",
  "^IXIC" = "macro_index_name_ixic",
  "^DJI" = "macro_index_name_dji",
  "^SOX" = "macro_index_name_sox",
  "^TWII" = "macro_index_name_twii",
  "IX0043.TWO" = "macro_index_name_twoii",
  "0050.TW" = "macro_index_name_0050"
)

macro_click_index_specs <- function(mode = get_market_mode()) {
  macro_index_specs(mode)
}

.macro_named_pick <- function(dict, symbol) {
  sym <- as.character(symbol %||% "")[1]
  if (length(sym) != 1L || is.na(sym) || !nzchar(sym) || !sym %in% names(dict)) {
    return(NA_character_)
  }
  unname(dict[[sym]])
}

macro_index_box_id <- function(symbol) {
  sym <- as.character(symbol %||% "")[1]
  id <- .macro_named_pick(.MACRO_CLICK_INDEX_BOX_IDS, sym)
  if (length(id) && !is.na(id) && nzchar(id)) return(id)
  if (!nzchar(sym) || is.na(sym)) return("ynow_macro_idx_empty")
  paste0("ynow_macro_idx_", gsub("[^A-Za-z0-9]+", "", tolower(sym)))
}

macro_index_name_key <- function(symbol) {
  key <- .macro_named_pick(.MACRO_CLICK_INDEX_NAME_KEYS, symbol)
  if (length(key) && !is.na(key) && nzchar(key)) return(key)
  "macro_index_chart_empty"
}

# Overlay line colors for major boards on YNOW / TYNOW charts (research display).
.MACRO_OWN_OVERLAY_COLORS <- c(
  "^GSPC" = "#c0392b",
  "^IXIC" = "#8e44ad",
  "^DJI" = "#16a085",
  "^SOX" = "#d35400",
  "^TWII" = "#c0392b",
  "IX0043.TWO" = "#8e44ad",
  "0050.TW" = "#16a085"
)

macro_own_overlay_color <- function(symbol) {
  col <- .macro_named_pick(.MACRO_OWN_OVERLAY_COLORS, symbol)
  if (length(col) && !is.na(col) && nzchar(col)) return(col)
  "#7f8c8d"
}

#' Align Date rows across series and rebase each Close to 100 at the common start.
#' Used so YNOW/TYNOW can overlay major boards on a comparable scale (no FX).
macro_align_rebase_100 <- function(named_dfs) {
  if (is.null(named_dfs) || !length(named_dfs)) return(NULL)
  cleaned <- list()
  for (nm in names(named_dfs)) {
    d <- named_dfs[[nm]]
    if (!is.data.frame(d) || !all(c("Date", "Close") %in% names(d)) || nrow(d) < 2L) next
    dd <- data.frame(
      Date = as.Date(d$Date),
      Close = suppressWarnings(as.numeric(d$Close)),
      stringsAsFactors = FALSE
    )
    dd <- dd[is.finite(dd$Close) & !is.na(dd$Date), , drop = FALSE]
    if (nrow(dd) >= 2L) cleaned[[nm]] <- dd
  }
  if (!length(cleaned)) return(NULL)
  dates <- as.Date(cleaned[[1]]$Date)
  for (d in cleaned[-1]) dates <- intersect(dates, as.Date(d$Date))
  dates <- sort(unique(dates))
  if (length(dates) < 2L) return(NULL)
  out <- list()
  for (nm in names(cleaned)) {
    d <- cleaned[[nm]]
    y <- as.numeric(d$Close[match(dates, as.Date(d$Date))])
    base <- y[is.finite(y)][1]
    if (!is.finite(base) || base == 0) next
    out[[nm]] <- data.frame(
      Date = dates,
      Close = 100 * y / base,
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) NULL else out
}

#' Localized choice vector for YNOW/TYNOW major-index overlays (current market boards).
macro_own_overlay_choices <- function(mode = get_market_mode(), locale = "en") {
  specs <- macro_index_specs(mode)
  if (!length(specs)) return(character(0))
  labs <- vapply(names(specs), function(sym) {
    key <- macro_index_name_key(sym)
    lab <- if (exists("ui_str", mode = "function")) {
      tryCatch(ui_str(key, locale), error = function(e) unname(specs[[sym]]))
    } else {
      unname(specs[[sym]])
    }
    if (!nzchar(as.character(lab)[1]) || identical(lab, key)) {
      lab <- unname(specs[[sym]])
    }
    as.character(lab)[1]
  }, character(1))
  stats::setNames(names(specs), labs)
}

macro_none_choice <- function(locale = "en") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale)[1]
  }
  lab <- if (exists("ui_str", mode = "function")) {
    tryCatch(ui_str("macro_none_option", loc), error = function(e) "—")
  } else {
    "—"
  }
  if (!nzchar(as.character(lab)[1])) lab <- "—"
  stats::setNames("", as.character(lab)[1])
}

macro_choices_with_none <- function(choices, locale = "en") {
  c(macro_none_choice(locale), choices)
}

#' Industry picker: US GICS sector ETFs, or the same industry-standard
#' snapshot list as Dashboard「目前產業標準快覽」.
macro_industry_choices <- function(mode = get_market_mode(), locale = "en") {
  mode <- normalize_market_mode(mode)
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale)[1]
  }
  is_zh <- grepl("^zh", tolower(loc), perl = TRUE)
  if (identical(mode, "TW")) {
    if (!exists("industry_picker_choices", mode = "function")) return(character(0))
    return(industry_picker_choices(if (is_zh) "zh-TW" else "en"))
  }
  if (!identical(mode, "US")) return(character(0))
  labs <- if (is_zh) .MACRO_US_GICS_LABELS_ZH else .MACRO_US_GICS_LABELS_EN
  stats::setNames(names(.MACRO_US_GICS), as.character(labs[names(.MACRO_US_GICS)]))
}

#' Industry-vs-benchmark default selection: blank until linked to a ticker industry.
macro_industry_default_key <- function(mode = get_market_mode()) {
  ""
}

#' Map Company「目前產業標準快覽」key → Macro Industry-vs-benchmark picker value.
#' TW: same industry_standards keys. US: map to GICS sector ETF keys (gics_*).
macro_resolve_linked_industry_key <- function(industry_key, mode = get_market_mode()) {
  key <- as.character(industry_key %||% "")[1]
  if (is.na(key) || !nzchar(key)) return("")
  mode <- normalize_market_mode(mode)
  if (identical(mode, "TW")) {
    ch <- tryCatch(macro_industry_choices("TW", "en"), error = function(e) character(0))
    if (key %in% unname(ch)) return(key)
    # Picker may not be bound yet on first paint — still accept catalog keys.
    if (exists("industry_standards") && key %in% names(industry_standards)) return(key)
    return("")
  }
  if (startsWith(key, "gics_") && key %in% names(.MACRO_US_GICS)) return(key)
  # Exact reverse of lab_gics_sector_fallback() industry keys → GICS ETF.
  exact <- c(
    "media.Entertainment" = "gics_xlc",
    "cons.Discretionary" = "gics_xly",
    "fmcg.Food_Beverages" = "gics_xlp",
    "en.Energy_OilGas" = "gics_xle",
    "fn.Banking" = "gics_xlf",
    "hc.Healthcare_Services" = "gics_xlv",
    "ind.Machinery" = "gics_xli",
    "tech.Software" = "gics_xlk",
    "mat.Chemicals" = "gics_xlb",
    "re.REIT" = "gics_xlre",
    "en.Utilities" = "gics_xlu"
  )
  if (key %in% names(exact)) return(unname(exact[[key]]))
  prefix <- sub("\\..*$", "", key)
  by_prefix <- c(
    sc = "gics_xlk", tech = "gics_xlk", saas = "gics_xlk", ec = "gics_xlk",
    fn = "gics_xlf",
    hc = "gics_xlv",
    en = "gics_xle",
    ind = "gics_xli", tr = "gics_xli", bus = "gics_xli",
    mat = "gics_xlb", ag = "gics_xlb",
    re = "gics_xlre",
    media = "gics_xlc", tel = "gics_xlc",
    cons = "gics_xly", auto = "gics_xly", retail = "gics_xly",
    ecr = "gics_xly", lxg = "gics_xly", hosp = "gics_xly",
    fmcg = "gics_xlp"
  )
  if (identical(key, "en.Utilities") || startsWith(key, "en.Util")) return("gics_xlu")
  hit <- unname(by_prefix[[prefix]] %||% "")
  if (nzchar(hit) && hit %in% names(.MACRO_US_GICS)) return(hit)
  ""
}

#' TW constituents for one industry-standard key.
#' Prefer the TWSE industry code (industry_raw). The stored industry_key
#' column leaves many listings, including 2330, as Unmapped.
macro_tw_industry_tickers <- function(industry_key) {
  key <- as.character(industry_key %||% "")[1]
  if (!nzchar(key) || !exists("lab_get_tw_universe", mode = "function")) return(character(0))
  u <- tryCatch(lab_get_tw_universe(FALSE), error = function(e) NULL)
  if (is.null(u) || !is.data.frame(u) || !nrow(u)) return(character(0))
  need <- c("ticker", "industry_raw", "exchange")
  if (!all(need %in% names(u))) return(character(0))
  ex <- toupper(as.character(u$exchange))
  u <- u[ex %in% c("TWSE", "TSE", "LISTED", "TPEX", "TWO", "OTC", "ROTC"), , drop = FALSE]
  if (!nrow(u)) return(character(0))
  mapped <- if (exists("lab_map_tw_industry_to_key", mode = "function")) {
    vapply(u$industry_raw, lab_map_tw_industry_to_key, character(1))
  } else if ("industry_key" %in% names(u)) {
    as.character(u$industry_key)
  } else {
    return(character(0))
  }
  u <- u[mapped == key, , drop = FALSE]
  if (!nrow(u)) return(character(0))
  ex <- toupper(as.character(u$exchange))
  listed <- ex %in% c("TWSE", "TSE", "LISTED")
  u <- u[order(!listed, as.character(u$ticker)), , drop = FALSE]
  unique(as.character(u$ticker[nzchar(u$ticker)]))
}

#' Concept-stock picker: lab_concept_groups keys (US / TW).
macro_concept_choices <- function(mode = get_market_mode(), locale = "en") {
  mode <- normalize_market_mode(mode)
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale)[1]
  }
  out <- character(0)
  if (exists("LAB_CONCEPT_GROUPS", inherits = TRUE) &&
      !is.null(LAB_CONCEPT_GROUPS[[mode]])) {
    keys <- names(LAB_CONCEPT_GROUPS[[mode]])
    for (k in keys) {
      lab <- if (exists("lab_concept_group_label", mode = "function")) {
        lab_concept_group_label(k, market = mode, locale = loc)
      } else {
        k
      }
      prefix <- if (identical(mode, "US")) "concept_" else "tw_"
      out <- c(out, stats::setNames(paste0(prefix, k), lab))
    }
  }
  out
}

#' Constituent count for one concept theme key (concept_* / tw_*).
macro_concept_group_n <- function(theme_key, mode = get_market_mode()) {
  key <- as.character(theme_key %||% "")[1]
  if (!nzchar(key)) return(0L)
  mode <- normalize_market_mode(mode)
  bare <- sub("^(concept_|tw_)", "", key)
  grp <- NULL
  if (exists("LAB_CONCEPT_GROUPS", inherits = TRUE) &&
      !is.null(LAB_CONCEPT_GROUPS[[mode]])) {
    grp <- LAB_CONCEPT_GROUPS[[mode]][[bare]]
  }
  if (is.null(grp)) return(0L)
  length(unique(as.character(grp[nzchar(as.character(grp))])))
}

#' Font sizes (px) for concept word-cloud chips, scaled by basket size.
macro_concept_cloud_sizes <- function(choices, mode = get_market_mode(),
                                      min_px = 12, max_px = 22) {
  vals <- unname(as.character(choices %||% character(0)))
  if (!length(vals)) return(numeric(0))
  ns <- vapply(vals, function(k) macro_concept_group_n(k, mode), integer(1))
  ns[!is.finite(ns) | ns < 1L] <- 1L
  lo <- min(ns)
  hi <- max(ns)
  if (!is.finite(hi) || hi <= lo) {
    return(rep((min_px + max_px) / 2, length(ns)))
  }
  min_px + (max_px - min_px) * (ns - lo) / (hi - lo)
}

.MACRO_CONCEPT_OVERLAY_COLORS <- c(
  "#2980b9", "#8e44ad", "#16a085", "#c0392b",
  "#d35400", "#2c3e50", "#27ae60", "#e67e22",
  "#1abc9c", "#9b59b6"
)

macro_concept_overlay_color <- function(i) {
  i <- as.integer(i)[1]
  if (!is.finite(i) || i < 1L) i <- 1L
  cols <- .MACRO_CONCEPT_OVERLAY_COLORS
  cols[((i - 1L) %% length(cols)) + 1L]
}

#' Normalize multi-select concept keys from the word cloud.
macro_selected_concept_keys <- function(raw) {
  keys <- unique(as.character(raw %||% character(0)))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  keys
}

#' Word-cloud checkbox group for concept multi-select overlays.
macro_concept_cloud_input <- function(inputId, choices, selected = character(0),
                                      mode = get_market_mode()) {
  vals <- unname(as.character(choices %||% character(0)))
  labs <- names(choices)
  if (is.null(labs) || length(labs) != length(vals)) {
    labs <- vals
  }
  if (!length(vals)) {
    return(htmltools::tags$p(
      class = "ynow-macro-hint",
      id = "ynow_macro_concept_cloud_empty",
      "—"
    ))
  }
  sizes <- macro_concept_cloud_sizes(choices, mode = mode)
  choice_names <- lapply(seq_along(vals), function(i) {
    htmltools::tags$span(
      class = "ynow-macro-cloud-word",
      style = sprintf("font-size:%spx;", round(sizes[[i]])),
      labs[[i]]
    )
  })
  sel <- intersect(as.character(selected %||% character(0)), vals)
  htmltools::tags$div(
    class = "ynow-macro-concept-cloud",
    shiny::checkboxGroupInput(
      inputId = inputId,
      label = NULL,
      choiceNames = choice_names,
      choiceValues = vals,
      selected = sel,
      inline = TRUE
    )
  )
}

#' Combined catalog (bubble / backward compat): industry then concept.
macro_theme_choices <- function(mode = get_market_mode(), locale = "en") {
  c(macro_industry_choices(mode, locale), macro_concept_choices(mode, locale))
}

#' Resolve theme key → Yahoo tickers (equal-weight basket or single ETF).
macro_theme_tickers <- function(theme_key, mode = get_market_mode()) {
  mode <- normalize_market_mode(mode)
  key <- as.character(theme_key %||% "")[1]
  if (!nzchar(key)) return(character(0))

  if (startsWith(key, "gics_") && key %in% names(.MACRO_US_GICS)) {
    return(as.character(.MACRO_US_GICS[[key]]))
  }
  if (identical(mode, "TW") && exists("industry_standards", inherits = TRUE) &&
      key %in% names(industry_standards)) {
    tks <- macro_tw_industry_tickers(key)
    return(tks)
  }
  cg_key <- sub("^(concept_|tw_)", "", key)
  if (exists("LAB_CONCEPT_GROUPS", inherits = TRUE) &&
      !is.null(LAB_CONCEPT_GROUPS[[mode]]) &&
      cg_key %in% names(LAB_CONCEPT_GROUPS[[mode]])) {
    return(as.character(LAB_CONCEPT_GROUPS[[mode]][[cg_key]]))
  }
  character(0)
}

#' Equal-weight rebased series (start = 100). No FX conversion.
macro_equal_weight_rebased <- function(tickers, period = "1y", max_n = 10L) {
  tickers <- unique(as.character(tickers))
  tickers <- tickers[nzchar(tickers)]
  if (!length(tickers)) return(NULL)
  tickers <- head(tickers, as.integer(max_n))

  series_list <- list()
  for (tk in tickers) {
    df <- tryCatch(fetch_price_history_df(tk, period), error = function(e) NULL)
    if (is.null(df) || nrow(df) < 20L) next
    df <- df[order(df$Date), , drop = FALSE]
    base <- df$Close[which(is.finite(df$Close))[1]]
    if (!is.finite(base) || base <= 0) next
    df$rebased <- 100 * df$Close / base
    series_list[[tk]] <- df[, c("Date", "rebased")]
  }
  if (!length(series_list)) return(NULL)

  # Outer join on Date, rowMeans of available rebased legs
  all_dates <- sort(unique(do.call(c, lapply(series_list, function(x) x$Date))))
  mat <- matrix(NA_real_, nrow = length(all_dates), ncol = length(series_list))
  colnames(mat) <- names(series_list)
  for (i in seq_along(series_list)) {
    s <- series_list[[i]]
    idx <- match(s$Date, all_dates)
    mat[idx, i] <- s$rebased
  }
  avg <- apply(mat, 1L, function(r) {
    r <- r[is.finite(r)]
    if (!length(r)) return(NA_real_)
    mean(r)
  })
  out <- data.frame(Date = all_dates, Theme = as.numeric(avg), stringsAsFactors = FALSE)
  out <- out[is.finite(out$Theme), , drop = FALSE]
  if (nrow(out) < 10L) return(NULL)
  out
}

#' Rebased overlay series for one industry or concept key. NULL if empty / unloadable.
macro_rebased_theme_df <- function(theme_key, mode = get_market_mode(), period = "1y") {
  key <- as.character(theme_key %||% "")[1]
  if (!nzchar(key)) return(NULL)
  tickers <- macro_theme_tickers(key, mode)
  if (!length(tickers)) return(NULL)
  if (length(tickers) == 1L) {
    raw <- tryCatch(fetch_price_history_df(tickers[[1]], period), error = function(e) NULL)
    if (is.null(raw) || nrow(raw) < 20L) return(NULL)
    raw <- raw[order(raw$Date), , drop = FALSE]
    base <- raw$Close[which(is.finite(raw$Close))[1]]
    if (!is.finite(base) || base <= 0) return(NULL)
    return(data.frame(
      Date = raw$Date,
      Theme = 100 * raw$Close / base,
      Close = raw$Close,
      stringsAsFactors = FALSE
    ))
  }
  ew <- macro_equal_weight_rebased(tickers, period = period, max_n = 10L)
  if (is.null(ew)) return(NULL)
  data.frame(
    Date = ew$Date, Theme = ew$Theme, Close = ew$Theme,
    stringsAsFactors = FALSE
  )
}

#' Rolling β path (month-end) for theme vs bench. Display-only; never writes CAPM.
macro_rolling_beta_path <- function(theme_df, bench_df, lookback_months = 36L) {
  if (is.null(theme_df) || is.null(bench_df)) return(NULL)
  if (!all(c("Date", "Close") %in% names(theme_df))) {
    if ("Theme" %in% names(theme_df)) {
      theme_df <- data.frame(Date = theme_df$Date, Close = theme_df$Theme)
    } else {
      return(NULL)
    }
  }
  if (!all(c("Date", "Close") %in% names(bench_df))) return(NULL)

  merged <- merge(
    theme_df[, c("Date", "Close")],
    bench_df[, c("Date", "Close")],
    by = "Date",
    suffixes = c("_s", "_m")
  )
  names(merged) <- c("Date", "S", "M")
  merged <- merged[is.finite(merged$S) & is.finite(merged$M), , drop = FALSE]
  if (nrow(merged) < 60L) return(NULL)
  merged <- merged[order(merged$Date), , drop = FALSE]

  ym <- format(merged$Date, "%Y-%m")
  ends <- merged[!duplicated(ym, fromLast = TRUE), , drop = FALSE]
  if (nrow(ends) < 18L) return(NULL)

  betas <- rep(NA_real_, nrow(ends))
  for (i in seq_len(nrow(ends))) {
    as_of <- ends$Date[i]
    betas[i] <- estimate_rolling_beta(
      merged$S, merged$M, merged$Date, as_of,
      lookback_months = lookback_months, min_obs = 18L
    )
  }
  data.frame(Date = ends$Date, Beta = betas, stringsAsFactors = FALSE)
}

# ---- Fast index quotes (KPI strip) ----

#' Map Yahoo chart range → approximate calendar lookback (quantmod fallback).
macro_quote_window_days <- function(period = "5d") {
  p <- tolower(trimws(as.character(period %||% "5d")[1]))
  days <- switch(
    p,
    "1d" = 5L,
    "5d" = 12L,
    "1wk" = 14L,
    "1mo" = 45L,
    "3mo" = 120L,
    "6mo" = 200L,
    "1y" = 400L,
    "2y" = 800L,
    "5y" = 365L * 5L,
    "max" = 365L * 20L,
    365L * 5L
  )
  as.integer(days)
}

#' Short Yahoo chart history for Macro KPI last/chg (fast tier; not statements).
#' Avoids `fetch_price_history_df(..., "5d")` when that path returns a full 5y tape.
macro_yahoo_chart_df <- function(symbol, range = "5d", interval = "1d") {
  sym <- as.character(symbol %||% "")[1]
  if (!nzchar(sym) || is.na(sym)) return(NULL)
  if (exists(".ynow_yahoo_gate_acquire", mode = "function")) {
    tryCatch(.ynow_yahoo_gate_acquire(timeout_sec = 10), error = function(e) NULL)
  }
  enc <- utils::URLencode(sym, reserved = TRUE)
  url <- sprintf(
    "https://query1.finance.yahoo.com/v8/finance/chart/%s?range=%s&interval=%s",
    enc,
    as.character(range %||% "5d")[1],
    as.character(interval %||% "1d")[1]
  )
  if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
  resp <- tryCatch(
    httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (compatible; YNowMacro/1.0)"),
      httr::timeout(15)
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200L) return(NULL)
  body <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
  if (!nzchar(body) || !requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
  res <- tryCatch(j$chart$result[[1]], error = function(e) NULL)
  ts <- tryCatch(unlist(res$timestamp, use.names = FALSE), error = function(e) NULL)
  closes <- tryCatch(
    unlist(res$indicators$quote[[1]]$close, use.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(ts) || is.null(closes) || !length(ts) || !length(closes)) return(NULL)
  n <- min(length(ts), length(closes))
  close_n <- suppressWarnings(as.numeric(closes[seq_len(n)]))
  dates <- as.Date(as.POSIXct(as.numeric(ts[seq_len(n)]), origin = "1970-01-01", tz = "UTC"))
  ok <- is.finite(close_n) & !is.na(dates)
  if (!any(ok)) return(NULL)
  data.frame(Date = dates[ok], Close = close_n[ok], stringsAsFactors = FALSE)
}

#' Last + day-chg for Macro index KPI symbols (native Yahoo currency; no FX).
#' Prefers short chart windows; falls back to history helper only when chart fails.
macro_fetch_last_quotes <- function(symbols) {
  syms <- unique(as.character(symbols %||% character(0)))
  syms <- syms[nzchar(syms) & !is.na(syms)]
  if (!length(syms)) return(list())

  # Optional Python batch helper (deep_scraper.get_last_quotes) when already loaded.
  if (exists("get_last_quotes", mode = "function")) {
    raw <- tryCatch(get_last_quotes(as.list(syms)), error = function(e) NULL)
    if (is.list(raw) && !is.null(raw$Symbol)) {
      out <- list()
      n <- length(raw$Symbol)
      for (i in seq_len(n)) {
        sym <- as.character(raw$Symbol[[i]] %||% "")[1]
        if (!nzchar(sym)) next
        out[[length(out) + 1L]] <- list(
          symbol = sym,
          last = suppressWarnings(as.numeric(raw$Last[[i]] %||% NA_real_)[1]),
          chg_pct = suppressWarnings(as.numeric(raw$ChangePct[[i]] %||% NA_real_)[1])
        )
      }
      if (length(out)) return(out)
    }
  }

  lapply(syms, function(sym) {
    df <- tryCatch(macro_yahoo_chart_df(sym, "5d"), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) {
      df <- tryCatch({
        if (exists("fetch_price_history_df", mode = "function")) {
          fetch_price_history_df(sym, "5d")
        } else {
          NULL
        }
      }, error = function(e) NULL)
    }
    last <- NA_real_
    chg <- NA_real_
    if (is.data.frame(df) && nrow(df) && "Close" %in% names(df)) {
      cc <- df$Close[is.finite(df$Close)]
      if (length(cc)) last <- tail(cc, 1)
      if (length(cc) >= 2L) chg <- 100 * (tail(cc, 1) / cc[length(cc) - 1L] - 1)
    }
    list(symbol = sym, last = last, chg_pct = chg)
  })
}

# ---- UI ----

#' Static first-paint index KPI skeleton (no server round-trip).
#' Nested `uiOutput("index_kpi_row")` can stay empty for many seconds while the
#' R loop is busy; bake dash cards into the lazy HTML so the row is never blank.
macro_index_kpi_skeleton_ui <- function(mode = "US", locale = "en") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "en")[1]
  }
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(mode)
    } else {
      toupper(as.character(mode %||% "US")[1])
    }
  }, error = function(e) "US")
  if (!mm %in% c("US", "TW")) mm <- "US"
  specs <- tryCatch(macro_click_index_specs(mm), error = function(e) character(0))
  if (!length(specs)) return(NULL)
  n_idx <- length(specs)
  idx_col_w <- if (identical(n_idx, 3L)) 4L else 3L
  idx_col_cls <- if (identical(n_idx, 3L)) {
    "col-xs-12 col-sm-4 col-md-4"
  } else {
    "col-xs-6 col-sm-6 col-md-3"
  }
  cols <- lapply(names(specs), function(sym) {
    name_key <- macro_index_name_key(sym)
    lab <- if (exists("ui_str", mode = "function")) {
      tryCatch({
        v <- ui_str(name_key, loc)
        if (nzchar(v) && !identical(v, name_key)) v else unname(specs[[sym]])
      }, error = function(e) unname(specs[[sym]]))
    } else {
      unname(specs[[sym]])
    }
    column(
      width = idx_col_w,
      class = idx_col_cls,
      tags$div(
        # No stable id — live uiOutput owns ynow_macro_idx_* once it flushes.
        class = "ynow-macro-kpi ynow-macro-kpi--skeleton",
        `data-macro-index` = sym,
        `aria-hidden` = "true",
        tags$div(class = "ynow-macro-kpi__label", lab),
        tags$div(class = "ynow-macro-kpi__value", "—"),
        tags$div(class = "ynow-macro-kpi__chg", "—"),
        tags$div(class = "ynow-macro-kpi__sym", sym)
      )
    )
  })
  do.call(fluidRow, c(list(class = "ynow-macro-kpi-row ynow-macro-kpi-row--skeleton"), cols))
}

#' Macro & Market Trends page body.
#' @param locale first-paint locale (`en` / `zh-TW`); language switches still
#'   update via `applyUiLocale`. Baking locale avoids English flash while lazy
#'   mount + sync quote fetches block the R event loop.
#' @param market_mode Active market so industry choices bake US ≠ TW at mount.
macro_market_ui <- function(id = "macro", locale = "en", market_mode = "US") {
  ns <- NS(id)
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "en")[1]
  }
  if (!nzchar(loc)) loc <- "en"
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market_mode)
    } else {
      toupper(as.character(market_mode %||% "US")[1])
    }
  }, error = function(e) "US")
  if (!mm %in% c("US", "TW")) mm <- "US"
  .s <- function(key) {
    if (exists("ui_str", mode = "function")) {
      tryCatch(ui_str(key, loc), error = function(e) key)
    } else {
      key
    }
  }
  tags$div(
    class = "ynow-macro-report",
    tags$div(
      class = "ynow-macro-report__masthead",
      h2(tags$b(id = "ynow_macro_page_title", .s("macro_page_title"))),
      p(
        id = "ynow_macro_page_sub",
        class = "ynow-macro-report__lead",
        .s("macro_page_sub")
      )
    ),
    tags$div(
      class = "ynow-macro-kpi-host",
      # Static skeleton paints with the lazy page HTML; hidden once live uiOutput
      # has children (see ynow_ui.R CSS :has() rule).
      tags$div(
        class = "ynow-macro-kpi-skeleton",
        macro_index_kpi_skeleton_ui(mm, loc)
      ),
      uiOutput(ns("index_kpi_row"))
    ),
    tags$p(
      id = "ynow_macro_index_hint",
      class = "ynow-macro-hint ynow-full-only",
      .s("macro_index_chart_hint")
    ),
    tags$p(
      id = "ynow_macro_ynow_index_hint",
      class = "ynow-macro-hint ynow-lite-only",
      .s("macro_ynow_index_chart_hint")
    ),
    uiOutput(ns("rf_signal_row")),
    # Shared expand slot for ^GSPC / boards and YNOW／TYNOW (same card style).
    # Visible in Lite when YNOW／TYNOW is selected (board KPIs stay Full-only).
    tags$div(
      id = "ynow_macro_index_hist",
      class = "ynow-macro-index-hist",
      uiOutput(ns("index_hist_panel"))
    ),
    # HTCDI expand panel: available in Lite and Full (not Full-gated).
    tags$div(
      id = "ynow_macro_htcdi_expand",
      class = "ynow-macro-htcdi-expand",
      uiOutput(ns("htcdi_expand_panel"))
    ),
    tags$section(
      class = "ynow-macro-chapter",
      # Refresh re-fetches index quotes, overlay series, HTCDI, and bubble data —
      # keep it; place on the same row as the chapter title (right-aligned).
      tags$div(
        class = "ynow-macro-theme-title-row",
        tags$h3(id = "ynow_macro_theme_title", .s("macro_theme_title")),
        actionButton(
          ns("refresh"),
          .s("btn_macro_refresh"),
          icon = icon("sync"),
          class = "btn-default ynow-macro-refresh-in-title"
        )
      ),
      # Always-visible block chrome — not Notes / 附註. Currency-lock footnote stays below.
      tags$p(
        id = "ynow_macro_theme_help",
        class = "ynow-macro-hint ynow-macro-chapter__lead",
        .s("macro_theme_help")
      ),
      fluidRow(
        column(
          width = 6,
          selectInput(
            ns("industry_key"),
            label = tags$span(id = "ynow_macro_industry_label", .s("macro_industry_label")),
            choices = tryCatch(
              macro_choices_with_none(macro_industry_choices(mm, loc), loc),
              error = function(e) c("—" = "")
            ),
            selected = ""
          )
        ),
        column(
          width = 6,
          selectInput(
            ns("hist_period"),
            label = tags$span(id = "ynow_macro_period_label", .s("macro_period_label")),
            choices = c("6M" = "6mo", "1Y" = "1y", "3Y" = "3y", "5Y" = "5y"),
            selected = "1y"
          )
        )
      ),
      tags$div(
        class = "ynow-macro-concept-cloud-wrap",
        tags$label(
          id = "ynow_macro_concept_label",
          class = "control-label",
          .s("macro_concept_label")
        ),
        tags$p(
          id = "ynow_macro_concept_cloud_hint",
          class = "ynow-macro-hint",
          .s("macro_concept_cloud_hint")
        ),
        uiOutput(ns("concept_cloud_ui"))
      ),
      tags$div(
        class = "ynow-macro-overlay-plot-wrap",
        shinycssloaders::withSpinner(
          plotlyOutput(ns("overlay_plot"), height = "380px", width = "100%")
        )
      ),
      ynow_notes_block(
        tags$p(
          id = "ynow_macro_fx_lock",
          class = "ynow-macro-hint",
          .s("macro_fx_lock")
        )
      )
    ),
    tags$div(
      id = "ynow_own_index",
      class = "ynow-macro-own-index",
      uiOutput(ns("own_index_block"))
    ),
    tags$script(HTML("
      (function () {
        if (document.documentElement.getAttribute('data-ynow-idx-tap') === '1') return;
        document.documentElement.setAttribute('data-ynow-idx-tap', '1');
        // Index-card tap logging removed (localhost ingest was debug-only).
      })();
    ")),
    tags$section(
      class = "ynow-macro-chapter ynow-macro-bubble-chapter",
      tags$h3(
        id = "ynow_macro_bubble_title",
        .s("macro_bubble_title")
      ),
      tags$p(
        id = "ynow_macro_bubble_sub",
        class = "ynow-macro-hint ynow-macro-chapter__lead",
        .s("macro_bubble_sub")
      ),
      macro_bubble_chapter_ui(ns)
    )
  )
}

# ---- Server ----

#' @param market_mode_rv reactiveVal / reactive returning "US" | "TW"
#' @param ui_locale_rv reactive returning locale id
#' @param company_industry_rv reactive returning Company industry_choice key (optional)
macro_market_server <- function(id = "macro",
                                market_mode_rv,
                                ui_locale_rv = reactive("en"),
                                lite_mode_rv = NULL,
                                company_industry_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a)) b else a

    .loc <- function() {
      tryCatch(normalize_ui_locale(ui_locale_rv()), error = function(e) "en")
    }
    .mode <- function() {
      tryCatch(normalize_market_mode(market_mode_rv()), error = function(e) "US")
    }
    .linked_industry <- function() {
      if (is.null(company_industry_rv)) return("")
      raw <- tryCatch(as.character(company_industry_rv())[1], error = function(e) "")
      macro_resolve_linked_industry_key(raw, .mode())
    }
    .is_lite <- function() {
      if (!is.null(lite_mode_rv)) {
        return(isTRUE(tryCatch(lite_mode_rv(), error = function(e) FALSE)))
      }
      tryCatch(isTRUE(session$rootScope()$input$ynow_lite_mode), error = function(e) FALSE)
    }
    .ui <- function(key, ...) {
      if (exists("ui_str", mode = "function")) {
        tryCatch(ui_str(key, .loc(), ...), error = function(e) key)
      } else {
        key
      }
    }
    .own_index_symbol <- function() {
      if (!exists("ynow_index_symbol", mode = "function")) return("")
      mode <- toupper(as.character(.mode() %||% "")[1])
      if (!mode %in% c("US", "TW")) return("")
      ynow_index_symbol(mode)
    }
    .index_ui_key <- function(suffix) {
      prefix <- if (identical(toupper(as.character(.mode() %||% "")[1]), "TW")) {
        "tynow_index_"
      } else {
        "ynow_index_"
      }
      paste0(prefix, suffix)
    }
    .px_last_chg <- function(df) {
      last <- NA_real_
      chg <- NA_real_
      if (is.data.frame(df) && nrow(df) && "Close" %in% names(df)) {
        cc <- df$Close[is.finite(df$Close)]
        if (length(cc)) last <- tail(cc, 1)
        if (length(cc) >= 2L) chg <- 100 * (tail(cc, 1) / cc[length(cc) - 1L] - 1)
      }
      list(last = last, chg = chg)
    }

    refresh_token <- reactiveVal(0L)
    selected_index <- reactiveVal("")
    htcdi_expanded <- reactiveVal(FALSE)
    # YNOW / TYNOW chart overlays — default none (no major-index overlay).
    own_index_overlays <- reactiveVal(character(0))
    # Until the user changes Industry vs benchmark, follow Company ticker industry (else blank).
    industry_touched <- reactiveVal(FALSE)
    industry_programmatic <- reactiveVal(FALSE)
    observeEvent(input$refresh, {
      refresh_token(isolate(refresh_token()) + 1L)
    }, ignoreInit = TRUE)
    observeEvent(input$htcdi_click, {
      htcdi_expanded(!isolate(htcdi_expanded()))
    }, ignoreInit = TRUE)
    observeEvent(input$index_click, {
      sym <- as.character(input$index_click %||% "")[1]
      specs <- macro_click_index_specs(.mode())
      own <- .own_index_symbol()
      # Lite: only YNOW / TYNOW expands; board KPIs stay Full-only.
      accepted <- if (.is_lite()) {
        nzchar(sym) && identical(sym, own)
      } else {
        nzchar(sym) && (sym %in% names(specs) || identical(sym, own))
      }
      if (!accepted) return()
      # Second click on the same board collapses the expand panel (same pattern as HTCDI).
      cur <- isolate(as.character(selected_index() %||% "")[1])
      if (identical(cur, sym)) {
        selected_index("")
      } else {
        selected_index(sym)
      }
    }, ignoreInit = TRUE)
    observeEvent({
      .mode()
    }, {
      specs <- macro_click_index_specs(.mode())
      own <- .own_index_symbol()
      sel <- isolate(as.character(selected_index() %||% "")[1])
      if (nzchar(sel) && !(sel %in% names(specs)) && !identical(sel, own)) {
        selected_index("")
      }
      # Drop overlays that are not in the new market's board list.
      cur_ov <- isolate(as.character(own_index_overlays() %||% character(0)))
      own_index_overlays(intersect(cur_ov, names(specs)))
    }, ignoreInit = TRUE)
    observeEvent(input$own_index_overlay, {
      specs <- macro_click_index_specs(.mode())
      sel <- as.character(input$own_index_overlay %||% character(0))
      sel <- sel[nzchar(sel) & sel %in% names(specs)]
      own_index_overlays(sel)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    # Keep overlay choice labels in sync with market + UI locale.
    observe({
      mode <- .mode()
      loc <- .loc()
      own <- .own_index_symbol()
      sel_idx <- as.character(selected_index() %||% "")[1]
      if (!nzchar(own) || !identical(sel_idx, own)) return()
      specs <- macro_index_specs(mode)
      cur <- intersect(as.character(isolate(own_index_overlays()) %||% character(0)), names(specs))
      tryCatch(
        updateCheckboxGroupInput(
          session,
          "own_index_overlay",
          label = .ui("macro_own_index_overlay_label"),
          choices = macro_own_overlay_choices(mode, loc),
          selected = cur,
          inline = TRUE
        ),
        error = function(e) NULL
      )
    })
    observeEvent(input$bubble_refresh, {
      refresh_token(isolate(refresh_token()) + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$industry_key, {
      if (isTRUE(isolate(industry_programmatic()))) {
        industry_programmatic(FALSE)
        return()
      }
      cur <- as.character(input$industry_key %||% "")[1]
      if (is.na(cur)) cur <- ""
      linked <- isolate(.linked_industry())
      # Choice-list rebuilds (locale / market) often re-fire selectInput with the
      # same blank or linked value — that is not a user edit.
      if (identical(cur, linked)) return()
      industry_touched(TRUE)
    }, ignoreInit = TRUE)

    .apply_linked_industry <- function(force_blank_if_unlinked = FALSE) {
      mode <- .mode()
      loc <- .loc()
      linked <- .linked_industry()
      ind_ch <- macro_choices_with_none(macro_industry_choices(mode, loc), loc)
      isel <- isolate(as.character(input$industry_key %||% "")[1])
      if (is.na(isel)) isel <- ""
      in_menu <- isel %in% unname(ind_ch)
      if (!isTRUE(isolate(industry_touched())) || !in_menu) {
        isel <- if (nzchar(linked) && linked %in% unname(ind_ch)) linked else ""
      } else if (force_blank_if_unlinked && !nzchar(linked) && !nzchar(isel)) {
        isel <- ""
      }
      # Always mark programmatic: Shiny may re-fire industry_key when only the
      # choice labels change, which would otherwise sticky-touch and block link.
      industry_programmatic(TRUE)
      updateSelectInput(session, "industry_key", choices = ind_ch, selected = isel)
    }

    # Shared Industry menu + Concept word cloud (Relative performance + Dynamic bubble).
    # Default blank; until user edits the picker, follow Company industry after a ticker load
    # (mapped to GICS on US / industry_standards on TW).
    # Lazy-tab race: gate Industry updates on hist_period after inputs bind.
    observe({
      hist_ready <- input$hist_period
      req(!is.null(hist_ready))
      .linked_industry()
      .mode()
      .loc()
      .apply_linked_industry()
    })

    # New ticker / Company industry resolve: re-link Macro Industry (clears prior
    # sticky touch from menu rebuilds). Manual Macro override holds only until
    # the next Company industry change.
    if (!is.null(company_industry_rv)) {
      observeEvent(
        company_industry_rv(),
        {
          if (is.null(isolate(input$hist_period))) return()
          industry_touched(FALSE)
          .apply_linked_industry()
        },
        ignoreInit = TRUE
      )
    }

    concept_cloud_catalog <- reactive({
      list(
        mode = .mode(),
        loc = .loc(),
        choices = macro_concept_choices(.mode(), .loc()),
        hist_ready = !is.null(input$hist_period)
      )
    })

    output$concept_cloud_ui <- renderUI({
      cat <- concept_cloud_catalog()
      csel <- isolate(
        intersect(
          macro_selected_concept_keys(input$concept_keys),
          unname(cat$choices)
        )
      )
      tryCatch(
        macro_concept_cloud_input(
          session$ns("concept_keys"),
          choices = cat$choices,
          selected = csel,
          mode = cat$mode
        ),
        error = function(e) {
          htmltools::tags$p(
            class = "ynow-macro-hint",
            id = "ynow_macro_concept_cloud_error",
            paste("Concept cloud unavailable:", conditionMessage(e))
          )
        }
      )
    })

    .selected_concepts <- function() {
      keys <- macro_selected_concept_keys(input$concept_keys)
      mode <- .mode()
      valid <- unname(macro_concept_choices(mode, .loc()))
      intersect(keys, valid)
    }

    .bubble_theme_key <- function() {
      ind <- as.character(input$industry_key %||% "")[1]
      if (is.na(ind)) ind <- ""
      if (nzchar(ind)) return(ind)
      cons <- .selected_concepts()
      if (length(cons)) return(cons[[1]])
      ""
    }

    .choice_label <- function(choices, key) {
      key <- as.character(key %||% "")[1]
      if (!nzchar(key)) return("")
      labs <- names(choices)
      vals <- unname(choices)
      if (is.null(labs) || !length(vals)) return(key)
      hit <- match(key, vals)
      if (!is.finite(hit)) return(key)
      lab <- as.character(labs[[hit]] %||% "")[1]
      if (!nzchar(lab)) key else lab
    }

    ynow_basket <- reactive({
      refresh_token()
      mode <- toupper(as.character(.mode() %||% "")[1])
      if (!mode %in% c("US", "TW")) return(NULL)
      if (!exists("ynow_index_read_cache", mode = "function")) return(NULL)
      key <- if (exists("ynow_index_month_key", mode = "function")) ynow_index_month_key() else ""
      path <- if (exists("ynow_index_cache_path", mode = "function")) ynow_index_cache_path(mode) else NULL
      tryCatch(ynow_index_read_cache(key, path), error = function(e) NULL)
    })

    # Progressive / non-blocking quotes: paint skeleton KPIs immediately so lazy
    # Macro mount does not hold the R event loop (which also delayed locale push).
    .index_quote_skeleton <- function(mode) {
      specs <- tryCatch(macro_click_index_specs(mode), error = function(e) character(0))
      if (!length(specs)) return(list())
      lapply(names(specs), function(sym) {
        list(
          symbol = sym,
          label = unname(specs[[sym]]),
          last = NA_real_,
          chg_pct = NA_real_
        )
      })
    }
    # Seed US skeleton at register time so the first index_kpi_row paint is never 0 cards.
    index_quotes_rv <- reactiveVal(.index_quote_skeleton("US"))
    index_quotes_gen <- reactiveVal(0L)
    index_quotes_ready_gen <- reactiveVal(0L)
    observeEvent(
      list(refresh_token(), .mode()),
      {
        mode <- .mode()
        skeleton <- .index_quote_skeleton(mode)
        syms <- vapply(skeleton, function(r) as.character(r$symbol)[1], character(1))
        if (!length(syms)) {
          index_quotes_rv(list())
          return(invisible(NULL))
        }
        gen <- isolate(as.integer(index_quotes_gen()) %||% 0L) + 1L
        index_quotes_gen(gen)
        index_quotes_rv(skeleton)
        # Short-window chart quotes (or get_last_quotes). Finish the whole strip
        # before HTCDI so Yahoo-gate Sys.sleep inside HTCDI cannot starve DJI/SOX.
        .fetch_one <- function(i, acc) {
          if (!identical(isolate(as.integer(index_quotes_gen()) %||% 0L), gen)) {
            return(invisible(NULL))
          }
          if (i > length(syms)) {
            tryCatch(index_quotes_rv(acc), error = function(e) NULL)
            # Signal completion; HTCDI observer below starts only after this gen.
            index_quotes_ready_gen(gen)
            return(invisible(NULL))
          }
          sym <- syms[[i]]
          qrow <- tryCatch(macro_fetch_last_quotes(sym)[[1]], error = function(e) NULL)
          acc[[i]] <- list(
            symbol = sym,
            label = acc[[i]]$label %||% sym,
            last = suppressWarnings(as.numeric(qrow$last %||% NA_real_)[1]),
            chg_pct = suppressWarnings(as.numeric(qrow$chg_pct %||% NA_real_)[1])
          )
          tryCatch(index_quotes_rv(acc), error = function(e) NULL)
          if (requireNamespace("later", quietly = TRUE)) {
            later::later(function() {
              tryCatch(.fetch_one(i + 1L, acc), error = function(e) NULL)
            }, delay = 0)
          } else {
            .fetch_one(i + 1L, acc)
          }
          invisible(NULL)
        }
        if (requireNamespace("later", quietly = TRUE)) {
          # Yield past after_mount locale push (0.12s).
          later::later(function() {
            tryCatch(.fetch_one(1L, skeleton), error = function(e) NULL)
          }, delay = 0.15)
        } else {
          .fetch_one(1L, skeleton)
        }
        invisible(NULL)
      },
      ignoreNULL = FALSE
    )
    index_quotes <- reactive({
      rows <- index_quotes_rv()
      if (length(rows)) return(rows)
      .index_quote_skeleton(.mode())
    })

    .macro_wrap_alertable <- function(ui, metric, ticker, value, label,
                                      ready = FALSE, default_kind = "price_gte") {
      if (!exists("ynow_alertable", mode = "function")) return(ui)
      ynow_alertable(
        ui,
        metric = metric,
        ticker = ticker,
        value = value,
        label = label,
        ready = ready,
        default_kind = default_kind
      )
    }

    .macro_publish_alert_metrics <- function(entries) {
      if (!is.list(entries) || !length(entries)) return(invisible(NULL))
      cur <- tryCatch(session$userData$value_alert_metrics, error = function(e) NULL)
      if (!is.list(cur)) cur <- list()
      for (nm in names(entries)) {
        mets <- entries[[nm]]
        if (!is.list(mets) || !length(mets)) next
        tk <- toupper(trimws(as.character(nm)[1]))
        if (!nzchar(tk)) next
        cur[[tk]] <- modifyList(as.list(cur[[tk]] %||% list()), mets)
      }
      session$userData$value_alert_metrics <- cur
      invisible(NULL)
    }

    output$index_kpi_row <- renderUI({
      rows <- index_quotes()
      lite <- .is_lite()
      sel <- as.character(selected_index() %||% "")[1]
      n_idx <- length(rows)
      # TW three boards = col-4 / 1:1:1; US four boards stay col-3. No empty fourth slot.
      idx_col_w <- if (identical(n_idx, 3L)) 4L else 3L
      idx_col_cls <- if (identical(n_idx, 3L)) {
        "col-xs-12 col-sm-4 col-md-4"
      } else {
        "col-xs-6 col-sm-6 col-md-3"
      }
      built <- lapply(rows, function(r) {
        last_txt <- if (is.finite(r$last)) format(round(r$last, 2), big.mark = ",") else "—"
        chg_txt <- if (is.finite(r$chg_pct)) sprintf("%+.2f%%", r$chg_pct) else "—"
        col_cls <- if (is.finite(r$chg_pct) && r$chg_pct >= 0) "ynow-macro-up" else "ynow-macro-down"
        box_id <- macro_index_box_id(r$symbol)
        name_key <- macro_index_name_key(r$symbol)
        loc_lab <- .ui(name_key)
        lab <- if (nzchar(loc_lab) && !identical(loc_lab, name_key)) loc_lab else r$label
        kpi_cls <- "ynow-macro-kpi"
        extra <- NULL
        if (!isTRUE(lite)) {
          kpi_cls <- paste(kpi_cls, "ynow-macro-kpi--clickable")
          if (nzchar(sel) && identical(sel, r$symbol)) {
            kpi_cls <- paste(kpi_cls, "ynow-macro-kpi--selected")
          }
          extra <- list(
            role = "button",
            tabindex = "0",
            onclick = sprintf(
              paste0(
                "if (document.body && document.body.classList.contains('ynow-lite')) return; ",
                "if (window.Shiny && Shiny.setInputValue) {",
                " Shiny.setInputValue('%s', '%s', {priority: 'event'}); }"
              ),
              ns("index_click"),
              r$symbol
            )
          )
        }
        kpi <- do.call(
          tags$div,
          c(
            list(
              id = box_id,
              class = kpi_cls,
              `data-macro-index` = r$symbol,
              tags$div(class = "ynow-macro-kpi__label", lab),
              tags$div(class = "ynow-macro-kpi__value", last_txt),
              tags$div(class = paste("ynow-macro-kpi__chg", col_cls), chg_txt),
              tags$div(class = "ynow-macro-kpi__sym", r$symbol)
            ),
            extra
          )
        )
        kpi <- .macro_wrap_alertable(
          kpi,
          metric = "last_price",
          ticker = r$symbol,
          value = r$last,
          label = lab,
          ready = is.finite(r$last),
          default_kind = "price_gte"
        )
        list(
          col = column(width = idx_col_w, class = idx_col_cls, kpi),
          sym = as.character(r$symbol)[1],
          last = r$last
        )
      })
      alert_entries <- list()
      for (b in built) {
        if (is.finite(b$last) && nzchar(b$sym %||% "")) {
          alert_entries[[b$sym]] <- list(last_price = b$last)
        }
      }
      if (length(alert_entries)) .macro_publish_alert_metrics(alert_entries)
      cols <- lapply(built, function(b) b$col)
      do.call(fluidRow, c(list(class = "ynow-macro-kpi-row"), cols))
    })

    index_hist_data <- reactive({
      refresh_token()
      sym <- as.character(selected_index() %||% "")[1]
      if (!nzchar(sym)) return(NULL)
      own <- .own_index_symbol()
      # Lite: only own-index (YNOW / TYNOW) series; board charts stay Full-only.
      if (.is_lite() && !identical(sym, own)) return(NULL)
      period <- as.character(input$hist_period %||% "1y")[1]
      if (!nzchar(period) || is.na(period)) period <- "1y"
      if (identical(sym, own)) {
        b <- tryCatch(ynow_basket(), error = function(e) NULL)
        mem <- b$members %||% list()
        if (!length(mem) || !exists("ynow_index_series", mode = "function")) return(NULL)
        tks <- vapply(mem, function(m) as.character(m$ticker)[1], character(1))
        return(tryCatch(ynow_index_series(tks, period), error = function(e) NULL))
      }
      tryCatch(fetch_price_history_df(sym, period), error = function(e) NULL)
    })

    own_member_quotes <- reactive({
      refresh_token()
      sym <- as.character(selected_index() %||% "")[1]
      if (!nzchar(sym) || !identical(sym, .own_index_symbol())) return(list())
      b <- tryCatch(ynow_basket(), error = function(e) NULL)
      mem <- b$members %||% list()
      if (!length(mem)) return(list())
      lapply(mem, function(m) {
        tk <- as.character(m$ticker)[1]
        df <- tryCatch(fetch_price_history_df(tk, "5d"), error = function(e) NULL)
        q <- .px_last_chg(df)
        list(
          ticker = tk,
          weight = m$weight,
          market_cap = suppressWarnings(as.numeric(m$market_cap)[1]),
          last = q$last,
          chg = q$chg
        )
      })
    })

    .own_index_constituents_ui <- function() {
      own <- .own_index_symbol()
      if (!nzchar(own)) return(NULL)
      quotes <- own_member_quotes()
      mode <- if (identical(own, "TYNOW")) "TW" else "US"
      mcap_ccy <- if (identical(mode, "TW")) "TWD" else "USD"
      names_v <- if (exists("ynow_index_lookup_names", mode = "function") && length(quotes)) {
        ynow_index_lookup_names(vapply(quotes, function(m) m$ticker, character(1)), mode)
      } else {
        character(0)
      }
      rows <- lapply(quotes, function(m) {
        w <- suppressWarnings(as.numeric(m$weight)[1])
        w_txt <- if (is.finite(w)) sprintf("%.2f%%", 100 * w) else "—"
        mcap <- suppressWarnings(as.numeric(m$market_cap)[1])
        mcap_txt <- if (is.finite(mcap) && mcap > 0 && exists("format_money_abbr", mode = "function")) {
          format_money_abbr(mcap, mcap_ccy)
        } else if (is.finite(mcap) && mcap > 0) {
          format(mcap, big.mark = ",", scientific = FALSE)
        } else {
          "—"
        }
        px_txt <- if (is.finite(m$last)) format(round(m$last, 2), big.mark = ",", nsmall = 2) else "—"
        px_chg <- if (is.finite(m$chg)) sprintf("%+.2f%%", m$chg) else "—"
        px_cls <- if (is.finite(m$chg) && m$chg >= 0) "ynow-macro-up" else "ynow-macro-down"
        nm <- if (length(names_v) && nzchar(names_v[[m$ticker]] %||% "")) names_v[[m$ticker]] else "—"
        tags$tr(
          tags$td(m$ticker),
          tags$td(nm),
          tags$td(mcap_txt),
          tags$td(w_txt),
          tags$td(px_txt),
          tags$td(class = px_cls, px_chg)
        )
      })
      table <- if (length(rows)) {
        tags$table(
          class = "table table-condensed ynow-htcdi-table",
          tags$thead(tags$tr(
            tags$th(id = "ynow_index_col_ticker", `data-i18n` = "ynow_index_col_ticker", .ui("ynow_index_col_ticker")),
            tags$th(id = "ynow_index_col_name", `data-i18n` = "ynow_index_col_name", .ui("ynow_index_col_name")),
            tags$th(id = "ynow_index_col_mcap", `data-i18n` = "ynow_index_col_mcap", .ui("ynow_index_col_mcap")),
            tags$th(id = "ynow_index_col_weight", `data-i18n` = "ynow_index_col_weight", .ui("ynow_index_col_weight")),
            tags$th(id = "ynow_index_col_last", `data-i18n` = "ynow_index_col_last", .ui("ynow_index_col_last")),
            tags$th(id = "ynow_index_col_chg", `data-i18n` = "ynow_index_col_chg", .ui("ynow_index_col_chg"))
          )),
          tags$tbody(rows)
        )
      } else {
        tags$p(class = "ynow-macro-hint", .ui(.index_ui_key("waiting")))
      }
      tags$div(
        class = "ynow-index-detail-ready",
        tags$h4(
          id = "ynow_index_constituents",
          `data-i18n` = "ynow_index_constituents",
          .ui("ynow_index_constituents")
        ),
        table,
        tags$p(
          id = "ynow_index_chart_note",
          `data-i18n` = "ynow_index_chart_note",
          class = "ynow-macro-hint",
          .ui("ynow_index_chart_note")
        )
      )
    }

    output$index_hist_panel <- renderUI({
      sym <- as.character(selected_index() %||% "")[1]
      if (!nzchar(sym)) return(NULL)
      # YNOW / TYNOW: same expand card as ^GSPC — rule above chart, constituents under it.
      own <- identical(sym, .own_index_symbol())
      # Lite: only own-index expand; board expand stays Full-only.
      if (.is_lite() && !own) return(NULL)
      dat <- index_hist_data()
      title <- if (own) .ui(.index_ui_key("title")) else .ui(macro_index_name_key(sym))
      rule_key <- .index_ui_key("rule")
      overlay_ctrl <- if (own) {
        ov_choices <- macro_own_overlay_choices(.mode(), .loc())
        ov_sel <- intersect(
          as.character(isolate(own_index_overlays()) %||% character(0)),
          unname(ov_choices)
        )
        tags$div(
          class = "ynow-own-index-overlay",
          checkboxGroupInput(
            ns("own_index_overlay"),
            label = tags$span(
              id = "ynow_own_index_overlay_label",
              `data-i18n` = "macro_own_index_overlay_label",
              .ui("macro_own_index_overlay_label")
            ),
            choices = ov_choices,
            selected = ov_sel,
            inline = TRUE
          ),
          tags$p(
            id = "ynow_own_index_overlay_hint",
            `data-i18n` = "macro_own_index_overlay_hint",
            class = "ynow-macro-hint",
            .ui("macro_own_index_overlay_hint")
          )
        )
      } else {
        NULL
      }
      body <- if (is.null(dat)) {
        tags$p(
          id = "ynow_macro_index_empty",
          class = "ynow-macro-hint",
          if (own) .ui(.index_ui_key("empty")) else .ui("macro_index_chart_error")
        )
      } else if (!is.data.frame(dat) || nrow(dat) < 2L) {
        tags$p(
          id = "ynow_macro_index_empty",
          class = "ynow-macro-hint",
          if (own) .ui(.index_ui_key("none")) else .ui("macro_index_chart_empty")
        )
      } else {
        plotlyOutput(ns("index_hist_plot"), height = "320px", width = "100%")
      }
      tags$div(
        class = "ynow-macro-card ynow-macro-index-hist__card",
        tags$h4(id = "ynow_macro_index_hist_title", title),
        if (own) {
          tags$p(
            id = "ynow_index_rule",
            `data-i18n` = rule_key,
            class = "ynow-macro-hint ynow-macro-chapter__lead",
            .ui(rule_key)
          )
        } else {
          NULL
        },
        overlay_ctrl,
        body,
        if (own) .own_index_constituents_ui() else NULL
      )
    })

    output$index_hist_plot <- plotly::renderPlotly({
      dat <- index_hist_data()
      shiny::validate(shiny::need(
        is.data.frame(dat) && nrow(dat) >= 2L,
        .ui("macro_index_chart_empty")
      ))
      sym <- as.character(selected_index() %||% "")[1]
      own <- identical(sym, .own_index_symbol())
      if (.is_lite() && !own) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines"))
      }
      title <- if (own) .ui(.index_ui_key("title")) else .ui(macro_index_name_key(sym))
      overlays <- if (own) {
        as.character(own_index_overlays() %||% character(0))
      } else {
        character(0)
      }
      overlays <- overlays[nzchar(overlays)]
      period <- as.character(input$hist_period %||% "1y")[1]
      if (!nzchar(period) || is.na(period)) period <- "1y"

      plot_df <- dat
      overlay_series <- list()
      if (own && length(overlays)) {
        named <- list(own = dat)
        for (ov in overlays) {
          raw <- tryCatch(fetch_price_history_df(ov, period), error = function(e) NULL)
          if (is.data.frame(raw) && nrow(raw) >= 2L) named[[ov]] <- raw
        }
        aligned <- macro_align_rebase_100(named)
        if (!is.null(aligned) && !is.null(aligned$own)) {
          plot_df <- aligned$own
          overlay_series <- aligned[setdiff(names(aligned), "own")]
        }
      }

      ylab <- if (own && length(overlay_series)) {
        .ui("macro_own_index_overlay_yaxis")
      } else if (own) {
        .ui("ynow_index_level")
      } else {
        title
      }
      fig <- plotly::plot_ly(
        plot_df, x = ~Date, y = ~Close,
        type = "scatter", mode = "lines",
        name = title,
        line = list(color = "#0c5484", width = 2.5)
      )
      if (length(overlay_series)) {
        for (ov in names(overlay_series)) {
          od <- overlay_series[[ov]]
          ov_lab <- .ui(macro_index_name_key(ov))
          if (!nzchar(ov_lab) || identical(ov_lab, "macro_index_chart_empty")) {
            ov_lab <- ov
          }
          fig <- plotly::add_trace(
            fig,
            data = od, x = ~Date, y = ~Close,
            type = "scatter", mode = "lines",
            name = ov_lab,
            line = list(color = macro_own_overlay_color(ov), width = 1.6, dash = "dot")
          )
        }
      }
      plotly::layout(
        fig,
        title = list(text = title, font = list(size = 14)),
        xaxis = list(title = ""),
        yaxis = list(title = ylab, showgrid = TRUE),
        margin = list(l = 50, r = 20, t = 50, b = 40),
        hovermode = "x unified",
        showlegend = length(overlay_series) > 0L,
        legend = list(orientation = "h", y = 1.12)
      )
    })

    own_level <- reactive({
      refresh_token()
      b <- tryCatch(ynow_basket(), error = function(e) NULL)
      mem <- b$members %||% list()
      last <- NA_real_
      chg <- NA_real_
      if (length(mem) && exists("ynow_index_series", mode = "function")) {
        tks <- vapply(mem, function(m) as.character(m$ticker)[1], character(1))
        ser <- tryCatch(ynow_index_series(tks, "5d"), error = function(e) NULL)
        q <- .px_last_chg(ser)
        last <- q$last
        chg <- q$chg
      }
      list(members = mem, last = last, chg = chg)
    })

    # YNOW / TYNOW KPI card (sits on Rf row at 2:1:1 with HTCDI).
    .own_index_kpi_card <- function() {
      own <- .own_index_symbol()
      if (!nzchar(own)) return(NULL)
      sel <- as.character(selected_index() %||% "")[1]
      selected_own <- identical(sel, own)
      lvl <- tryCatch(own_level(), error = function(e) list(last = NA_real_, chg = NA_real_))
      last <- lvl$last
      chg <- lvl$chg
      last_txt <- if (is.finite(last)) format(round(last, 2), big.mark = ",", nsmall = 2) else "—"
      chg_txt <- if (is.finite(chg)) sprintf("%+.2f%%", chg) else "—"
      chg_cls <- if (is.finite(chg) && chg >= 0) "ynow-macro-up" else "ynow-macro-down"
      title_key <- .index_ui_key("title")
      lab <- .ui(title_key)
      # Clickable in Full and Lite (Lite expands YNOW／TYNOW only).
      kpi_cls <- "ynow-macro-kpi ynow-macro-kpi--ynow ynow-macro-kpi--clickable"
      if (selected_own) kpi_cls <- paste(kpi_cls, "ynow-macro-kpi--selected")
      extra <- list(
        role = "button",
        tabindex = "0",
        onclick = sprintf(
          paste0(
            "if (window.Shiny && Shiny.setInputValue) {",
            " Shiny.setInputValue('%s', '%s', {priority: 'event'}); }"
          ),
          ns("index_click"),
          own
        )
      )
      kpi <- do.call(
        tags$div,
        c(
          list(
            id = macro_index_box_id(own),
            class = kpi_cls,
            `data-macro-index` = own,
            tags$div(
              class = "ynow-macro-kpi__label",
              id = "ynow_index_title",
              `data-i18n` = title_key,
              lab
            ),
            tags$div(class = "ynow-macro-kpi__value ynow-htcdi-flow", last_txt),
            tags$div(class = paste("ynow-macro-kpi__chg", chg_cls), chg_txt),
            tags$div(class = "ynow-macro-kpi__sym", own)
          ),
          extra
        )
      )
      if (is.finite(last)) {
        mets <- list()
        mets$macro_ynow_level <- last
        .macro_publish_alert_metrics(setNames(list(mets), own))
      }
      .macro_wrap_alertable(
        kpi,
        metric = "macro_ynow_level",
        ticker = own,
        value = last,
        label = lab,
        ready = is.finite(last),
        default_kind = "value_gte"
      )
    }

    # Rule + chart + constituents all live in the shared expand card (index_hist_panel).
    # Keep this output as NULL so the old mid-page YNOW chapter does not reappear.
    output$own_index_block <- renderUI({
      NULL
    })

    # rf_box is inlined inside rf_signal_row (stable id for locale / tests).

    # HTCDI: 5y prices (+ optional statements). Must not start until index KPI
    # quotes finish — Yahoo-gate Sys.sleep inside a sync HTCDI loop starved DJI/SOX.
    # Phase A = prices only (publish score). Phase B = statement scrape enrich.
    htcdi_val <- reactiveVal(NULL)
    htcdi_job <- reactiveVal(0L)
    .htcdi_score_from_maps <- function(price_map, bench_df, cfg, fs_map = list()) {
      inputs <- if (exists("htcdi_live_inputs_from_prices", mode = "function")) {
        htcdi_live_inputs_from_prices(price_map, bench_df, cfg, fs_map)
      } else {
        NULL
      }
      if (exists("htcdi_score", mode = "function")) {
        tryCatch(htcdi_score(inputs, cfg), error = function(e) NULL)
      } else {
        NULL
      }
    }
    .htcdi_enrich_fs <- function(job_id, cfg0, issuers0, price_map0, bench_df0) {
      if (!identical(isolate(htcdi_job()), job_id)) return()
      if (!exists("cached_scrape_financials", mode = "function")) return()
      fs_map <- list()
      for (iss in issuers0) {
        if (!identical(isolate(htcdi_job()), job_id)) return()
        tk <- as.character(iss$tickers[[1]] %||% "")[1]
        if (!nzchar(tk)) next
        itype <- tolower(as.character(iss$instrument_type %||% "")[1])
        if (identical(itype, "etf") || identical(itype, "commodity")) next
        fs <- tryCatch(cached_scrape_financials(tk), error = function(e) NULL)
        if (!is.null(fs)) fs_map[[tk]] <- fs
      }
      if (!length(fs_map)) return()
      res2 <- tryCatch(
        .htcdi_score_from_maps(price_map0, bench_df0, cfg0, fs_map),
        error = function(e) NULL
      )
      if (!identical(isolate(htcdi_job()), job_id)) return()
      if (!is.null(res2)) htcdi_val(res2)
    }
    .queue_htcdi <- function() {
      mode <- isolate(.mode())
      job <- isolate(htcdi_job()) + 1L
      htcdi_job(job)
      htcdi_val(list(availability = "loading"))
      cfg <- if (exists("htcdi_load_config", mode = "function")) {
        tryCatch(htcdi_load_config(), error = function(e) NULL)
      } else {
        NULL
      }
      issuers <- if (exists("htcdi_issuers", mode = "function")) {
        tryCatch(htcdi_issuers(cfg), error = function(e) list())
      } else {
        list()
      }
      if (!is.list(issuers)) issuers <- list()
      state <- new.env(parent = emptyenv())
      state$price_map <- list()
      state$bench_df <- NULL
      bench_tk <- tryCatch(macro_bench_ticker(mode), error = function(e) "^GSPC")

      .fetch_price_i <- function(i) {
        if (!identical(isolate(htcdi_job()), job)) return()
        if (i > length(issuers)) {
          if (exists("fetch_price_history_df", mode = "function")) {
            state$bench_df <- tryCatch(
              fetch_price_history_df(bench_tk, "5y"),
              error = function(e) NULL
            )
          }
          res <- tryCatch(
            .htcdi_score_from_maps(state$price_map, state$bench_df, cfg, list()),
            error = function(e) NULL
          )
          if (!identical(isolate(htcdi_job()), job)) return()
          htcdi_val(res %||% list(availability = "unavailable"))
          # Phase B: slow-tier statements; re-score when done (honest empty if fail).
          if (requireNamespace("later", quietly = TRUE)) {
            later::later(function() {
              tryCatch(
                .htcdi_enrich_fs(job, cfg, issuers, state$price_map, state$bench_df),
                error = function(e) NULL
              )
            }, delay = 0.25)
          }
          return()
        }
        iss <- issuers[[i]]
        tk <- as.character(iss$tickers[[1]] %||% "")[1]
        if (nzchar(tk) && exists("fetch_price_history_df", mode = "function")) {
          pdf <- tryCatch(fetch_price_history_df(tk, "5y"), error = function(e) NULL)
          if (!is.null(pdf) && is.data.frame(pdf) && nrow(pdf) >= 2L) {
            state$price_map[[tk]] <- pdf
          }
        }
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(function() {
            tryCatch(.fetch_price_i(i + 1L), error = function(e) NULL)
          }, delay = 0)
        } else {
          .fetch_price_i(i + 1L)
        }
      }

      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() {
          tryCatch(.fetch_price_i(1L), error = function(e) {
            if (identical(isolate(htcdi_job()), job)) {
              htcdi_val(list(availability = "unavailable"))
            }
          })
        }, delay = 0)
      } else {
        .fetch_price_i(1L)
      }
    }
    # Start HTCDI only after index KPI quotes complete (same gen).
    observeEvent(index_quotes_ready_gen(), {
      gen <- as.integer(index_quotes_ready_gen() %||% 0L)[1]
      if (!is.finite(gen) || gen < 1L) return()
      if (!identical(gen, isolate(as.integer(index_quotes_gen()) %||% 0L))) return()
      .queue_htcdi()
    }, ignoreInit = TRUE)
    observeEvent(refresh_token(), {
      htcdi_val(NULL)
      # Quotes observer also listens to refresh_token and will re-signal ready.
    }, ignoreInit = TRUE)
    htcdi_result <- reactive(htcdi_val())

    output$htcdi_expand_panel <- renderUI({
      if (!isTRUE(htcdi_expanded())) return(NULL)
      res <- htcdi_result()
      if (exists("htcdi_expand_ui", mode = "function")) htcdi_expand_ui(res, .loc()) else NULL
    })

    # Rf : YNOW/TYNOW : HTCDI = 2:1:1 (widths 6 / 3 / 3). TW NDC on the next row.
    # HTCDI four-index expand works in Lite and Full, below this row.
    output$rf_signal_row <- renderUI({
      .loc()
      mode <- .mode()
      lite <- .is_lite()
      is_tw <- identical(mode, "TW")
      res <- tryCatch(htcdi_result(), error = function(e) NULL)
      if (is.null(res)) res <- list(availability = "loading")
      ynow_card <- .own_index_kpi_card()
      det <- tryCatch({
        if (exists("cached_get_risk_free_rate_detail", mode = "function")) {
          cached_get_risk_free_rate_detail(mode)
        } else {
          list(rf_pct = cached_get_risk_free_rate(mode), source = "")
        }
      }, error = function(e) list(rf_pct = NA_real_, source = e$message))
      rf <- suppressWarnings(as.numeric(det$rf_pct %||% NA_real_)[1])
      rf_src <- as.character(det$source %||% "")[1]
      rf_src_lab <- switch(
        rf_src,
        live = .ui("macro_rf_src_live"),
        last_known = .ui("macro_rf_src_last"),
        fallback = .ui("macro_rf_src_fallback"),
        .ui("macro_rf_source_fallback")
      )
      rf_det_lab <- as.character(det$label %||% "")[1]
      rf_tk <- if (is_tw) "RF_TW" else "RF_US"
      rf_lab <- .ui("macro_rf_title")
      # Inline Rf value (no nested uiOutput). HTCDI updates used to rebuild this
      # row and orphan nested rf_box, leaving a blank black card.
      rf_kpi <- tags$div(
        class = "ynow-macro-kpi ynow-macro-kpi--rf",
        tags$div(class = "ynow-macro-kpi__label", id = "ynow_macro_rf_title", rf_lab),
        tags$div(
          id = ns("rf_box"),
          tags$div(
            class = "ynow-macro-kpi__value ynow-macro-rf__value",
            if (is.finite(rf)) sprintf("%.2f%%", rf) else "—"
          ),
          tags$div(
            class = "ynow-macro-hint",
            paste(rf_det_lab, "·", rf_src_lab)
          )
        )
      )
      if (is.finite(rf)) {
        .macro_publish_alert_metrics(setNames(list(list(macro_rf_pct = rf)), rf_tk))
      }
      rf_kpi <- .macro_wrap_alertable(
        rf_kpi,
        metric = "macro_rf_pct",
        ticker = rf_tk,
        value = rf,
        label = rf_lab,
        ready = is.finite(rf),
        default_kind = "value_gte"
      )
      rf_col <- column(
        width = 6, class = "col-xs-12 col-sm-6 col-md-6",
        rf_kpi
      )
      ynow_col <- column(
        width = 3, class = "col-xs-12 col-sm-3 col-md-3",
        if (!is.null(ynow_card)) ynow_card else tags$div(class = "ynow-macro-kpi ynow-macro-kpi--ynow", "—")
      )
      htcdi_score <- suppressWarnings(as.numeric(res$composite %||% NA_real_)[1])
      if (is.finite(htcdi_score)) {
        .macro_publish_alert_metrics(list(HTCDI = list(macro_htcdi = htcdi_score)))
      }
      htcdi_col <- column(
        width = 3, class = "col-xs-12 col-sm-3 col-md-3",
        if (exists("htcdi_kpi_box", mode = "function")) {
          htcdi_kpi_box(res, lite = lite, locale = .loc(), ns = ns,
                        selected = isTRUE(htcdi_expanded()))
        } else {
          tags$div(class = "ynow-macro-kpi ynow-macro-kpi--htcdi", "HTCDI")
        }
      )
      main_row <- do.call(
        fluidRow,
        list(class = "ynow-macro-rf-row ynow-macro-kpi-row", rf_col, ynow_col, htcdi_col)
      )
      if (!is_tw) return(main_row)
      ndc_row <- fluidRow(
        class = "ynow-macro-rf-row ynow-macro-ndc-row",
        column(
          width = 12, class = "col-xs-12 col-sm-12 col-md-12",
          tags$div(
            class = "ynow-macro-card",
            tags$h4(id = "ynow_macro_tw_signal_title", .ui("macro_tw_signal_title")),
            tags$div(
              class = "ynow-macro-hint",
              tags$p(.ui("macro_tw_signal_body")),
              tags$p(tags$a(
                href = "https://index.ndc.gov.tw/", target = "_blank",
                rel = "noopener noreferrer", .ui("macro_tw_signal_link")
              ))
            )
          )
        )
      )
      tagList(main_row, ndc_row)
    })

    overlay_data <- reactive({
      refresh_token()
      mode <- .mode()
      period <- as.character(input$hist_period %||% "1y")[1]
      industry_key <- as.character(input$industry_key %||% "")[1]
      concept_keys <- .selected_concepts()
      if (is.na(industry_key)) industry_key <- ""

      industry <- NULL
      concepts <- list()
      if (nzchar(industry_key)) {
        industry <- tryCatch(
          macro_rebased_theme_df(industry_key, mode, period),
          error = function(e) NULL
        )
      }
      if (length(concept_keys)) {
        for (ck in concept_keys) {
          df <- tryCatch(
            macro_rebased_theme_df(ck, mode, period),
            error = function(e) NULL
          )
          if (!is.null(df) && is.data.frame(df) && nrow(df) >= 2L) {
            concepts[[ck]] <- df
          }
        }
      }

      bench_tk <- macro_bench_ticker(mode)
      bench <- NULL
      if (nzchar(industry_key) || length(concept_keys)) {
        bench_raw <- tryCatch(fetch_price_history_df(bench_tk, period), error = function(e) NULL)
        if (!is.null(bench_raw) && nrow(bench_raw) >= 20L) {
          bench_raw <- bench_raw[order(bench_raw$Date), , drop = FALSE]
          base_b <- bench_raw$Close[which(is.finite(bench_raw$Close))[1]]
          if (is.finite(base_b) && base_b > 0) {
            bench <- data.frame(
              Date = bench_raw$Date,
              Bench = 100 * bench_raw$Close / base_b,
              Close = bench_raw$Close,
              stringsAsFactors = FALSE
            )
          }
        }
      }
      list(
        industry = industry,
        concepts = concepts,
        concept_keys = concept_keys,
        bench = bench,
        bench_ticker = bench_tk,
        industry_key = industry_key
      )
    })

    output$overlay_plot <- plotly::renderPlotly({
      od <- overlay_data()
      has_ind_key <- nzchar(od$industry_key %||% "")
      has_con_key <- length(od$concept_keys %||% character(0)) > 0L
      shiny::validate(shiny::need(
        has_ind_key || has_con_key,
        .ui("macro_plot_need_pick")
      ))
      shiny::validate(shiny::need(
        !is.null(od$industry) || length(od$concepts) > 0L,
        .ui("macro_plot_need_theme")
      ))
      shiny::validate(shiny::need(!is.null(od$bench), .ui("macro_plot_need_bench")))

      bh <- od$bench
      common <- bh$Date
      if (!is.null(od$industry)) common <- intersect(common, od$industry$Date)
      for (ck in names(od$concepts)) {
        common <- intersect(common, od$concepts[[ck]]$Date)
      }
      shiny::validate(shiny::need(length(common) >= 5L, .ui("macro_plot_need_theme")))
      bh <- bh[bh$Date %in% common, , drop = FALSE]

      industry_lab <- .ui("macro_series_industry")
      con_ch <- macro_concept_choices(.mode(), .loc())
      n_con <- length(od$concepts)
      left_lab <- if (!is.null(od$industry) && n_con > 0L) {
        .ui("macro_series_rebased")
      } else if (!is.null(od$industry)) {
        industry_lab
      } else if (n_con > 1L) {
        .ui("macro_series_concepts")
      } else {
        .ui("macro_series_concept")
      }
      bench_lab <- paste0(.ui("macro_series_bench"), " (", od$bench_ticker, ")")

      fig <- plotly::plot_ly()
      if (!is.null(od$industry)) {
        th <- od$industry[od$industry$Date %in% common, , drop = FALSE]
        fig <- plotly::add_trace(
          fig,
          x = th$Date, y = th$Theme,
          type = "scatter", mode = "lines",
          name = industry_lab,
          line = list(color = "#e67e22", width = 2),
          yaxis = "y"
        )
      }
      if (n_con > 0L) {
        for (i in seq_along(od$concepts)) {
          ck <- names(od$concepts)[[i]]
          th <- od$concepts[[ck]]
          th <- th[th$Date %in% common, , drop = FALSE]
          lab <- .choice_label(con_ch, ck)
          if (!nzchar(lab)) lab <- ck
          fig <- plotly::add_trace(
            fig,
            x = th$Date, y = th$Theme,
            type = "scatter", mode = "lines",
            name = lab,
            line = list(color = macro_concept_overlay_color(i), width = 2),
            yaxis = "y"
          )
        }
      }
      fig <- plotly::add_trace(
        fig,
        x = bh$Date, y = bh$Bench,
        type = "scatter", mode = "lines",
        name = bench_lab,
        line = list(color = "#888888", width = 1.5, dash = "dash"),
        yaxis = "y2"
      )
      plotly::layout(
        fig,
        autosize = TRUE,
        title = list(text = .ui("macro_overlay_title"), font = list(size = 14)),
        xaxis = list(title = ""),
        yaxis = list(title = left_lab, side = "left", showgrid = TRUE),
        yaxis2 = list(
          title = bench_lab,
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        legend = list(orientation = "h", y = 1.12),
        margin = list(l = 50, r = 60, t = 50, b = 40),
        hovermode = "x unified"
      ) %>%
        plotly::config(responsive = TRUE, displayModeBar = FALSE)
    })

    # Macro-page theme-vs-benchmark β chart is no longer rendered.
    # Keep macro_rolling_beta_path() + estimate_rolling_beta() for CAPM / HFV / backtest.

    # ---- Bubble & concentration (display-only; shares Industry/Concept picks) ----
    output$bubble_shared_pick_status <- renderUI({
      mode <- .mode()
      loc <- .loc()
      ind <- as.character(input$industry_key %||% "")[1]
      cons <- .selected_concepts()
      if (is.na(ind)) ind <- ""
      ind_ch <- macro_choices_with_none(macro_industry_choices(mode, loc), loc)
      con_ch <- macro_concept_choices(mode, loc)
      if (!nzchar(ind) && !length(cons)) {
        return(tags$p(
          id = "ynow_macro_bubble_shared_status",
          class = "ynow-macro-hint",
          .ui("macro_bubble_shared_need_pick")
        ))
      }
      con_lab <- if (length(cons)) {
        paste(vapply(cons, function(k) .choice_label(con_ch, k), character(1)), collapse = " · ")
      } else {
        ""
      }
      if (nzchar(ind) && length(cons)) {
        msg <- gsub(
          "{industry}", .choice_label(ind_ch, ind),
          .ui("macro_bubble_shared_using_industry_both"),
          fixed = TRUE
        )
        msg <- gsub("{concept}", con_lab, msg, fixed = TRUE)
      } else if (nzchar(ind)) {
        msg <- gsub(
          "{industry}", .choice_label(ind_ch, ind),
          .ui("macro_bubble_shared_using_industry"),
          fixed = TRUE
        )
      } else {
        msg <- gsub(
          "{concept}", con_lab,
          .ui("macro_bubble_shared_using_concept"),
          fixed = TRUE
        )
      }
      tags$p(id = "ynow_macro_bubble_shared_status", class = "ynow-macro-hint", msg)
    })

    bubble_data <- reactive({
      refresh_token()
      theme_key <- .bubble_theme_key()
      req(nzchar(theme_key))
      top_n <- suppressWarnings(as.integer(input$bubble_top_n %||% 5L)[1])
      if (!is.finite(top_n)) top_n <- 5L
      attr_p <- as.character(input$bubble_attr_period %||% "1y")[1]
      macro_bubble_concentration(
        theme_key = theme_key,
        mode = .mode(),
        top_n = top_n,
        attr_period = attr_p
      )
    })

    output$bubble_alert_box <- renderUI({
      bd <- tryCatch(bubble_data(), error = function(e) NULL)
      if (is.null(bd) || !length(bd$alerts)) return(NULL)
      msgs <- character(0)
      if ("top1_gt_50" %in% bd$alerts) msgs <- c(msgs, .ui("macro_bubble_alert_top1"))
      if ("topn_gt_70" %in% bd$alerts) msgs <- c(msgs, .ui("macro_bubble_alert_topn"))
      if ("narrow_breadth" %in% bd$alerts) msgs <- c(msgs, .ui("macro_bubble_alert_breadth"))
      if (!length(msgs)) return(NULL)
      tags$div(
        class = "ynow-macro-callout ynow-macro-callout--warn",
        tags$b(.ui("macro_bubble_alert_title")),
        tags$ul(lapply(msgs, tags$li))
      )
    })

    output$bubble_conc_kpi <- renderUI({
      bd <- tryCatch(bubble_data(), error = function(e) NULL)
      if (is.null(bd) || !nrow(bd$pool)) {
        return(tags$p(class = "ynow-macro-hint", .ui("macro_bubble_need_theme")))
      }
      tags$p(
        class = "ynow-macro-hint",
        sprintf(
          .ui("macro_bubble_conc_kpi"),
          as.integer(bd$top_n),
          if (is.finite(bd$top_share)) 100 * bd$top_share else NA_real_,
          if (is.finite(bd$top1_weight)) 100 * bd$top1_weight else NA_real_,
          nrow(bd$pool)
        )
      )
    })

    output$bubble_conc_plot <- plotly::renderPlotly({
      bd <- bubble_data()
      shiny::validate(shiny::need(nrow(bd$pool) >= 1L, .ui("macro_bubble_need_theme")))
      hist <- bd$history
      shiny::validate(shiny::need(
        is.data.frame(hist) && nrow(hist) >= 2L,
        .ui("macro_bubble_conc_need_hist")
      ))
      top_lab <- sprintf(.ui("macro_bubble_conc_topn_series"), as.integer(bd$top_n))
      top1_lab <- .ui("macro_bubble_conc_top1_series")
      fig <- plotly::plot_ly()
      fig <- plotly::add_trace(
        fig,
        x = hist$date,
        y = 100 * hist$top_n_share,
        type = "scatter",
        mode = "lines",
        name = top_lab,
        line = list(color = "#e67e22", width = 2.2)
      )
      fig <- plotly::add_trace(
        fig,
        x = hist$date,
        y = 100 * hist$top1_share,
        type = "scatter",
        mode = "lines",
        name = top1_lab,
        line = list(color = "#2980b9", width = 1.6, dash = "dot")
      )
      plotly::layout(
        fig,
        xaxis = list(title = ""),
        yaxis = list(
          title = .ui("macro_bubble_conc_axis"),
          ticksuffix = "%",
          rangemode = "tozero"
        ),
        legend = list(orientation = "h", y = -0.2, x = 0),
        margin = list(l = 48, r = 12, t = 8, b = 56),
        hovermode = "x unified"
      )
    })

    output$bubble_conc_table <- renderUI({
      bd <- tryCatch(bubble_data(), error = function(e) NULL)
      if (is.null(bd) || !is.data.frame(bd$top) || !nrow(bd$top)) return(NULL)
      top <- bd$top
      names_v <- macro_bubble_top_names(top$ticker, .mode())
      rows <- lapply(seq_len(nrow(top)), function(i) {
        tk <- as.character(top$ticker[[i]])
        nm <- if (length(names_v) && nzchar(names_v[[tk]] %||% "")) {
          names_v[[tk]]
        } else {
          "—"
        }
        mcap <- suppressWarnings(as.numeric(top$market_cap[[i]]))
        w <- suppressWarnings(as.numeric(top$weight[[i]]))
        tags$tr(
          tags$td(as.character(i)),
          tags$td(tk),
          tags$td(nm),
          tags$td(macro_bubble_fmt_usd_level(mcap)),
          tags$td(if (is.finite(w)) sprintf("%.1f%%", 100 * w) else "—")
        )
      })
      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-condensed ynow-htcdi-table",
          tags$thead(tags$tr(
            tags$th("#"),
            tags$th(.ui("macro_bubble_col_ticker")),
            tags$th(.ui("macro_bubble_col_name")),
            tags$th(.ui("macro_bubble_col_mcap")),
            tags$th(.ui("macro_bubble_col_weight"))
          )),
          tags$tbody(rows)
        )
      )
    })

    output$bubble_attr_kpi <- renderUI({
      bd <- tryCatch(bubble_data(), error = function(e) NULL)
      if (is.null(bd)) return(NULL)
      att <- bd$attribution
      tags$p(
        class = "ynow-macro-hint",
        sprintf(
          .ui("macro_bubble_attr_kpi"),
          att$period,
          if (is.finite(att$basket_ret)) 100 * att$basket_ret else NA_real_,
          if (is.finite(att$top_contrib)) 100 * att$top_contrib else NA_real_,
          if (is.finite(att$rest_contrib)) 100 * att$rest_contrib else NA_real_
        )
      )
    })

    output$bubble_attr_plot <- plotly::renderPlotly({
      bd <- bubble_data()
      shiny::validate(shiny::need(nrow(bd$pool) >= 1L, .ui("macro_bubble_need_theme")))
      att <- bd$attribution
      pool <- bd$pool
      top_n <- as.integer(bd$top_n)[1]
      top_idx <- seq_len(min(top_n, nrow(pool)))
      labels <- character(0)
      contribs <- numeric(0)
      colors <- character(0)
      pal <- c("#e67e22", "#2980b9", "#27ae60", "#8e44ad", "#c0392b", "#16a085", "#d35400")
      for (i in top_idx) {
        w <- suppressWarnings(as.numeric(pool$weight[[i]]))
        r <- suppressWarnings(as.numeric(pool$ret[[i]]))
        if (!is.finite(w) || !is.finite(r)) next
        labels <- c(labels, as.character(pool$ticker[[i]]))
        contribs <- c(contribs, w * r)
        colors <- c(colors, pal[((i - 1L) %% length(pal)) + 1L])
      }
      rest_c <- if (is.finite(att$rest_contrib)) att$rest_contrib else 0
      labels <- c(labels, .ui("macro_bubble_rest"))
      contribs <- c(contribs, rest_c)
      colors <- c(colors, "#7f8c8d")
      shiny::validate(shiny::need(length(contribs) >= 1L, .ui("macro_bubble_need_theme")))
      # Pie slices need non-negative magnitudes; signed pp stay in text/hover.
      mag <- abs(contribs)
      if (!any(mag > 0)) mag <- rep(1, length(mag))
      text_lbl <- sprintf("%s<br>%+.1f pp", labels, contribs * 100)
      fig <- plotly::plot_ly(
        labels = labels,
        values = mag,
        type = "pie",
        text = text_lbl,
        textinfo = "label+percent",
        textposition = "inside",
        insidetextorientation = "horizontal",
        hovertemplate = "%{text}<extra></extra>",
        marker = list(colors = colors, line = list(color = "#ffffff", width = 1)),
        sort = FALSE,
        direction = "clockwise"
      )
      plotly::layout(
        fig,
        autosize = TRUE,
        margin = list(l = 8, r = 8, t = 8, b = 8),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
        plotly::config(responsive = TRUE, displayModeBar = FALSE)
    })

    buffett_series <- reactive({
      refresh_token()
      macro_bubble_buffett_series(.mode())
    })

    buffett_abs_series <- reactive({
      refresh_token()
      tryCatch(macro_bubble_buffett_abs_series(.mode()), error = function(e) NULL)
    })

    output$bubble_buffett_light <- renderUI({
      ser <- buffett_series()
      lt <- macro_bubble_buffett_light(ser)
      tags$div(
        class = "ynow-macro-kpi",
        style = sprintf(
          "border-left: 6px solid %s; padding-left: 10px;",
          lt$color
        ),
        tags$div(
          class = "ynow-macro-kpi__label",
          .ui("macro_bubble_buffett_level")
        ),
        tags$div(
          class = "ynow-macro-kpi__value",
          style = sprintf("color: %s;", lt$color),
          .ui(lt$label_key)
        ),
        tags$div(
          class = "ynow-macro-hint",
          if (is.finite(lt$current)) {
            sprintf(.ui("macro_bubble_buffett_kpi"), lt$current, as.character(lt$as_of))
          } else {
            .ui("macro_bubble_buffett_unknown")
          }
        )
      )
    })

    .buffett_abs_kpi_box <- function(label_key, value_txt, as_of, hint_key) {
      asof_txt <- if (inherits(as_of, "Date") && !is.na(as_of)) {
        as.character(as_of)
      } else {
        "—"
      }
      tags$div(
        class = "ynow-macro-kpi",
        tags$div(class = "ynow-macro-kpi__label", .ui(label_key)),
        tags$div(class = "ynow-macro-kpi__value", value_txt),
        tags$div(
          class = "ynow-macro-hint",
          sprintf(.ui(hint_key), asof_txt)
        )
      )
    }

    output$bubble_buffett_mcap <- renderUI({
      .loc()
      mode_now <- .mode()
      abs_ser <- buffett_abs_series()
      got <- macro_bubble_buffett_abs_asof(abs_ser, NA_integer_)
      val <- if (isTRUE(got$ok) && is.finite(got$market_cap_usd)) {
        macro_bubble_fmt_usd_level(got$market_cap_usd)
      } else {
        "—"
      }
      .buffett_abs_kpi_box(
        "macro_bubble_buffett_mcap_label",
        val,
        got$as_of,
        if (identical(mode_now, "TW")) {
          "macro_bubble_buffett_mcap_hint_tw"
        } else {
          "macro_bubble_buffett_mcap_hint"
        }
      )
    })

    output$bubble_buffett_gdp <- renderUI({
      .loc()
      mode_now <- .mode()
      abs_ser <- buffett_abs_series()
      got <- macro_bubble_buffett_abs_asof(abs_ser, NA_integer_)
      val <- if (isTRUE(got$ok) && is.finite(got$gdp_usd)) {
        macro_bubble_fmt_usd_level(got$gdp_usd)
      } else {
        "—"
      }
      .buffett_abs_kpi_box(
        "macro_bubble_buffett_gdp_label",
        val,
        got$as_of,
        if (identical(mode_now, "TW")) {
          "macro_bubble_buffett_gdp_hint_tw"
        } else {
          "macro_bubble_buffett_gdp_hint"
        }
      )
    })

    output$bubble_buffett_plot <- plotly::renderPlotly({
      ser <- buffett_series()
      shiny::validate(shiny::need(is.data.frame(ser) && nrow(ser) >= 3L, .ui("macro_bubble_buffett_need")))
      shiny::validate(shiny::need(nrow(ser) >= 2L, .ui("macro_bubble_buffett_need")))
      lt <- macro_bubble_buffett_light(ser)
      fig <- plotly::plot_ly(
        ser, x = ~date, y = ~ratio_pct,
        type = "scatter", mode = "lines+markers",
        name = .ui("macro_bubble_buffett_series"),
        line = list(color = "#2c3e50", width = 2),
        marker = list(size = 5)
      )
      if (is.finite(lt$mean)) {
        fig <- plotly::add_trace(
          fig,
          x = ser$date,
          y = rep(lt$mean, nrow(ser)),
          type = "scatter", mode = "lines",
          name = .ui("macro_bubble_buffett_mean"),
          line = list(color = "#7f8c8d", dash = "dot", width = 1),
          inherit = FALSE
        )
      }
      if (is.finite(lt$lo) && is.finite(lt$hi)) {
        fig <- plotly::add_trace(
          fig,
          x = ser$date,
          y = rep(lt$hi, nrow(ser)),
          type = "scatter", mode = "lines",
          name = "+0.75σ",
          line = list(color = "#d9534f", dash = "dash", width = 1),
          inherit = FALSE
        )
        fig <- plotly::add_trace(
          fig,
          x = ser$date,
          y = rep(lt$lo, nrow(ser)),
          type = "scatter", mode = "lines",
          name = "−0.75σ",
          line = list(color = "#00a65a", dash = "dash", width = 1),
          inherit = FALSE
        )
      }
      plotly::layout(
        fig,
        autosize = TRUE,
        title = list(text = .ui("macro_bubble_buffett_chart"), font = list(size = 13)),
        xaxis = list(title = ""),
        yaxis = list(title = .ui("macro_bubble_buffett_axis")),
        legend = list(orientation = "h", y = 1.12),
        margin = list(l = 50, r = 20, t = 50, b = 40),
        hovermode = "x unified"
      ) %>%
        plotly::config(responsive = TRUE, displayModeBar = FALSE)
    })
  })
}
