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
# TPEx cap-weighted index is IX0043.TWO. ^TWOII is delisted on Yahoo (404, empty history).
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

#' Industry-vs-benchmark default: US Technology (XLK).
#' TW uses the snapshot default (半導體｜晶圓代工 / sc.Foundry).
macro_industry_default_key <- function(mode = get_market_mode()) {
  if (!identical(normalize_market_mode(mode), "TW")) return("gics_xlk")
  key <- "sc.Foundry"
  if (exists("APP_DEFAULTS", inherits = TRUE)) {
    k <- as.character(APP_DEFAULTS$industry_choice %||% "")[1]
    if (nzchar(k) && !is.na(k)) key <- k
  }
  key
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
    # #region agent log
    try(cat(paste0(
      "{\"sessionId\":\"ef0f33\",\"runId\":\"tw-ind-menu\",\"hypothesisId\":\"MENU\",",
      "\"location\":\"macro_market_module.R:macro_theme_tickers\",\"message\":\"tw industry basket\",",
      "\"data\":{\"key\":\"", gsub("\"", "", key), "\",\"n\":", length(tks),
      ",\"has_2330\":", if ("2330.TW" %in% tks) "true" else "false", "},",
      "\"timestamp\":", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, trim = TRUE), "}\n"
    ), file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-ef0f33.log", append = TRUE), silent = TRUE)
    # #endregion
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

# ---- UI ----

macro_market_ui <- function(id = "macro") {
  ns <- NS(id)
  tags$div(
    class = "ynow-macro-report",
    tags$div(
      class = "ynow-macro-report__masthead",
      h2(tags$b(id = "ynow_macro_page_title", "Macro & Market Trends")),
      p(
        id = "ynow_macro_page_sub",
        class = "ynow-macro-report__lead",
        paste0(
          "Follows the global US / TW market toggle. Index and theme price series stay in Yahoo’s native quote currency—",
          "no historical FX conversion."
        )
      )
    ),
    fluidRow(
      column(width = 12, uiOutput(ns("index_kpi_row")))
    ),
    tags$p(
      id = "ynow_macro_index_hint",
      class = "ynow-macro-hint ynow-full-only",
      "Click an index box to show its historical line chart."
    ),
    uiOutput(ns("rf_signal_row")),
    # Shared expand slot for ^GSPC / boards and YNOW／TYNOW (same card style).
    tags$div(
      id = "ynow_macro_index_hist",
      class = "ynow-macro-index-hist ynow-full-only",
      uiOutput(ns("index_hist_panel"))
    ),
    tags$div(
      id = "ynow_macro_hccsi_expand",
      class = "ynow-macro-hccsi-expand ynow-full-only",
      uiOutput(ns("hccsi_expand_panel"))
    ),
    tags$section(
      class = "ynow-macro-chapter",
      tags$h3(id = "ynow_macro_theme_title", "Relative performance vs benchmark"),
      # Always-visible block chrome — not Notes / 附註. Currency-lock footnote stays below.
      tags$p(
        id = "ynow_macro_theme_help",
        class = "ynow-macro-hint ynow-macro-chapter__lead",
        paste0(
          "Pick Industry and Concept independently (either, both, or neither). ",
          "The same picks drive Relative performance and Dynamic industry bubble below. ",
          "US industry uses GICS sector ETFs; Taiwan industry uses the industry-standard snapshot. ",
          "Concept uses the concept-stock universe. ",
          "Benchmark is gray dashed on the right axis (rebased = 100 at window start; native currency, no FX)."
        )
      ),
      fluidRow(
        column(
          width = 3,
          selectInput(
            ns("industry_key"),
            label = tags$span(id = "ynow_macro_industry_label", "Industry vs benchmark"),
            choices = c("—" = ""),
            selected = "gics_xlk"
          )
        ),
        column(
          width = 3,
          selectInput(
            ns("concept_key"),
            label = tags$span(id = "ynow_macro_concept_label", "Concept vs benchmark"),
            choices = c("—" = ""),
            selected = ""
          )
        ),
        column(
          width = 2,
          selectInput(
            ns("hist_period"),
            label = tags$span(id = "ynow_macro_period_label", "Window"),
            choices = c("6M" = "6mo", "1Y" = "1y", "3Y" = "3y", "5Y" = "5y"),
            selected = "1y"
          )
        ),
        column(
          width = 4,
          tags$div(
            style = "margin-top: 24px;",
            actionButton(
              ns("refresh"),
              "Refresh",
              icon = icon("sync"),
              class = "btn-default"
            )
          )
        )
      ),
      plotlyOutput(ns("overlay_plot"), height = "380px") %>% shinycssloaders::withSpinner(),
      ynow_notes_block(
        tags$p(
          id = "ynow_macro_fx_lock",
          class = "ynow-macro-hint",
          "Currency lock: historical index / theme series are never converted by the session USD⇄TWD toggle."
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
        function ynowIdxLog(hid, message, ev) {
          var t = ev.target;
          var card = t && t.closest ? t.closest('[data-macro-index]') : null;
          var cs = t ? getComputedStyle(t) : null;
          var rect = card ? card.getBoundingClientRect() : null;
          var mid = rect ? document.elementFromPoint(rect.left + rect.width / 2, rect.top + rect.height / 2) : null;
          fetch('http://127.0.0.1:7302/ingest/e3a0dcdf-71e1-4bba-855e-f942118bd315',{method:'POST',headers:{'Content-Type':'application/json','X-Debug-Session-Id':'ef0f33'},body:JSON.stringify({sessionId:'ef0f33',runId:'pre-fix',hypothesisId:hid,location:'macro_market_module.R:index-tap',message:message,data:{index:card?card.getAttribute('data-macro-index'):'',targetCls:t&&t.className?String(t.className).slice(0,120):'',clip:cs?cs.webkitBackgroundClip:'',pe:cs?cs.pointerEvents:'',vw:window.innerWidth,lite:!!(document.body&&document.body.classList.contains('ynow-lite')),midCls:mid&&mid.className?String(mid.className).slice(0,120):'',cardW:rect?Math.round(rect.width):0},timestamp:Date.now()})}).catch(function(){});
        }
        document.addEventListener('touchend', function (ev) {
          if (ev.target && ev.target.closest && ev.target.closest('[data-macro-index]')) ynowIdxLog('A', 'touchend on index card', ev);
        }, true);
        document.addEventListener('click', function (ev) {
          if (ev.target && ev.target.closest && ev.target.closest('[data-macro-index]')) ynowIdxLog('A', 'click on index card', ev);
        }, true);
      })();
    ")),
    tags$section(
      class = "ynow-macro-chapter ynow-macro-bubble-chapter",
      tags$h3(
        id = "ynow_macro_bubble_title",
        "Dynamic industry bubble & weight concentration"
      ),
      tags$p(
        id = "ynow_macro_bubble_sub",
        class = "ynow-macro-hint ynow-macro-chapter__lead",
        paste0(
          "Uses the shared Industry vs benchmark and Concept vs benchmark picks above. ",
          "Concentration uses market-cap weights on that basket (Industry when both are set; ",
          "GICS maps to S&P 500 sector peers). Buffett Indicator is market-level market-cap / GDP ",
          "(research display only — never feeds CAPM / Ke / WACC)."
        )
      ),
      macro_bubble_chapter_ui(ns)
    )
  )
}

# ---- Server ----

#' @param market_mode_rv reactiveVal / reactive returning "US" | "TW"
#' @param ui_locale_rv reactive returning locale id
macro_market_server <- function(id = "macro",
                                market_mode_rv,
                                ui_locale_rv = reactive("en"),
                                lite_mode_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a)) b else a

    .loc <- function() {
      tryCatch(normalize_ui_locale(ui_locale_rv()), error = function(e) "en")
    }
    .mode <- function() {
      tryCatch(normalize_market_mode(market_mode_rv()), error = function(e) "US")
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
    hccsi_expanded <- reactiveVal(FALSE)
    # YNOW / TYNOW chart overlays — default none (no major-index overlay).
    own_index_overlays <- reactiveVal(character(0))
    # Until the user changes Industry vs benchmark, US opens on Technology (XLK).
    industry_touched <- reactiveVal(FALSE)
    industry_programmatic <- reactiveVal(FALSE)
    observeEvent(input$refresh, {
      refresh_token(isolate(refresh_token()) + 1L)
    }, ignoreInit = TRUE)
    observeEvent(input$hccsi_click, {
      if (.is_lite()) return()
      hccsi_expanded(!isolate(hccsi_expanded()))
    }, ignoreInit = TRUE)
    observeEvent(input$index_click, {
      if (.is_lite()) return()
      sym <- as.character(input$index_click %||% "")[1]
      specs <- macro_click_index_specs(.mode())
      own <- .own_index_symbol()
      accepted <- nzchar(sym) && (sym %in% names(specs) || identical(sym, own))
      # #region agent log
      if (exists(".ynow_dbg_ef0f33", mode = "function")) {
        .ynow_dbg_ef0f33("D", "macro_market_module.R:index_click", "index click received", list(
          sym = sym, own = own, accepted = accepted, lite = .is_lite()
        ))
      }
      # #endregion
      if (accepted) {
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
          selected = cur
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
      industry_touched(TRUE)
    }, ignoreInit = TRUE)

    # Shared Industry + Concept menus (Relative performance + Dynamic bubble).
    # Default is Technology (XLK) on US, and the snapshot default (sc.Foundry) on TW.
    # An explicit None stays None.
    observe({
      mode <- .mode()
      loc <- .loc()
      ind_ch <- macro_choices_with_none(macro_industry_choices(mode, loc), loc)
      con_ch <- macro_choices_with_none(macro_concept_choices(mode, loc), loc)

      isel <- isolate(as.character(input$industry_key %||% "")[1])
      in_menu <- isel %in% unname(ind_ch)
      def_ind <- macro_industry_default_key(mode)
      if (!isTRUE(isolate(industry_touched())) || !in_menu) {
        isel <- if (def_ind %in% unname(ind_ch)) def_ind else ""
      }
      csel <- isolate(as.character(input$concept_key %||% "")[1])
      if (is.null(csel) || !nzchar(csel) || !(csel %in% unname(con_ch))) {
        csel <- ""
      }
      cur_ind <- isolate(as.character(input$industry_key %||% "")[1])
      if (!identical(cur_ind, isel)) industry_programmatic(TRUE)
      updateSelectInput(session, "industry_key", choices = ind_ch, selected = isel)
      updateSelectInput(session, "concept_key", choices = con_ch, selected = csel)
    })

    # Bubble concentration basket: Industry when set, else Concept (shared picks).
    .bubble_theme_key <- function() {
      ind <- as.character(input$industry_key %||% "")[1]
      con <- as.character(input$concept_key %||% "")[1]
      if (is.na(ind)) ind <- ""
      if (is.na(con)) con <- ""
      if (nzchar(ind)) return(ind)
      if (nzchar(con)) return(con)
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

    index_quotes <- reactive({
      refresh_token()
      specs <- macro_click_index_specs(.mode())
      lapply(names(specs), function(sym) {
        df <- tryCatch(fetch_price_history_df(sym, "5d"), error = function(e) NULL)
        q <- .px_last_chg(df)
        # #region agent log
        if (exists(".ynow_dbg_ef0f33", mode = "function")) {
          .ynow_dbg_ef0f33("E", "macro_market_module.R:index_quotes", "kpi quote", list(
            symbol = sym, nrow = if (is.data.frame(df)) nrow(df) else 0L,
            last = q$last, chg = q$chg, finite_last = is.finite(q$last)
          ))
        }
        # #endregion
        list(symbol = sym, label = unname(specs[[sym]]), last = q$last, chg_pct = q$chg)
      })
    })

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
      cols <- lapply(rows, function(r) {
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
        column(
          width = idx_col_w,
          class = idx_col_cls,
          do.call(
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
        )
      })
      do.call(fluidRow, c(list(class = "ynow-macro-kpi-row"), cols))
    })

    index_hist_data <- reactive({
      refresh_token()
      if (.is_lite()) return(NULL)
      sym <- as.character(selected_index() %||% "")[1]
      if (!nzchar(sym)) return(NULL)
      period <- as.character(input$hist_period %||% "1y")[1]
      if (!nzchar(period) || is.na(period)) period <- "1y"
      if (identical(sym, .own_index_symbol())) {
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
      if (.is_lite() || !nzchar(sym) || !identical(sym, .own_index_symbol())) return(list())
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
          class = "table table-condensed ynow-hccsi-table",
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
      if (.is_lite()) return(NULL)
      sym <- as.character(selected_index() %||% "")[1]
      if (!nzchar(sym)) return(NULL)
      # YNOW / TYNOW: same expand card as ^GSPC — rule above chart, constituents under it.
      own <- identical(sym, .own_index_symbol())
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
      if (.is_lite()) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines"))
      }
      dat <- index_hist_data()
      shiny::validate(shiny::need(
        is.data.frame(dat) && nrow(dat) >= 2L,
        .ui("macro_index_chart_empty")
      ))
      sym <- as.character(selected_index() %||% "")[1]
      own <- identical(sym, .own_index_symbol())
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

    # YNOW / TYNOW KPI card (sits on Rf row at 2:1:1 with HCCSI).
    .own_index_kpi_card <- function() {
      own <- .own_index_symbol()
      if (!nzchar(own)) return(NULL)
      lite <- .is_lite()
      sel <- as.character(selected_index() %||% "")[1]
      selected_own <- !isTRUE(lite) && identical(sel, own)
      lvl <- tryCatch(own_level(), error = function(e) list(last = NA_real_, chg = NA_real_))
      last <- lvl$last
      chg <- lvl$chg
      last_txt <- if (is.finite(last)) format(round(last, 2), big.mark = ",", nsmall = 2) else "—"
      chg_txt <- if (is.finite(chg)) sprintf("%+.2f%%", chg) else "—"
      chg_cls <- if (is.finite(chg) && chg >= 0) "ynow-macro-up" else "ynow-macro-down"
      title_key <- .index_ui_key("title")
      kpi_cls <- "ynow-macro-kpi ynow-macro-kpi--ynow"
      extra <- NULL
      if (!isTRUE(lite)) {
        kpi_cls <- paste(kpi_cls, "ynow-macro-kpi--clickable")
        if (selected_own) kpi_cls <- paste(kpi_cls, "ynow-macro-kpi--selected")
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
            own
          )
        )
      }
      do.call(
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
              .ui(title_key)
            ),
            tags$div(class = "ynow-macro-kpi__value ynow-hccsi-flow", last_txt),
            tags$div(class = paste("ynow-macro-kpi__chg", chg_cls), chg_txt),
            tags$div(class = "ynow-macro-kpi__sym", own)
          ),
          extra
        )
      )
    }

    # Rule + chart + constituents all live in the shared expand card (index_hist_panel).
    # Keep this output as NULL so the old mid-page YNOW chapter does not reappear.
    output$own_index_block <- renderUI({
      NULL
    })

    output$rf_box <- renderUI({
      refresh_token()
      mode <- .mode()
      det <- tryCatch({
        if (exists("cached_get_risk_free_rate_detail", mode = "function")) {
          cached_get_risk_free_rate_detail(mode)
        } else {
          list(rf_pct = cached_get_risk_free_rate(mode), source = "")
        }
      }, error = function(e) list(rf_pct = NA_real_, source = e$message))
      rf <- suppressWarnings(as.numeric(det$rf_pct %||% NA_real_)[1])
      src <- as.character(det$source %||% "")[1]
      lab <- as.character(det$label %||% "")[1]
      src_lab <- switch(
        src,
        live = .ui("macro_rf_src_live"),
        last_known = .ui("macro_rf_src_last"),
        fallback = .ui("macro_rf_src_fallback"),
        .ui("macro_rf_source_fallback")
      )
      tags$div(
        tags$div(
          class = "ynow-macro-kpi__value ynow-macro-rf__value",
          if (is.finite(rf)) sprintf("%.2f%%", rf) else "—"
        ),
        tags$div(
          class = "ynow-macro-hint",
          paste(lab, "·", src_lab)
        )
      )
    })

    # HCCSI pulls 5y prices and statements for every issuer. That work used to
    # run inside the first paint. Queue it after the session flushes so the
    # index cards and the rest of the page can appear first.
    hccsi_val <- reactiveVal(NULL)
    hccsi_job <- reactiveVal(0L)
    .hccsi_compute <- function(mode) {
      # Always attempt live Yahoo/history and statements. Never score from config placeholders.
      cfg <- if (exists("hccsi_load_config", mode = "function")) hccsi_load_config() else NULL
      price_map <- list(); bench_df <- NULL
      n_iss <- 0L
      if (exists("fetch_price_history_df", mode = "function") &&
          exists("hccsi_issuers", mode = "function")) {
        issuers <- hccsi_issuers(cfg)
        n_iss <- length(issuers)
        for (iss in issuers) {
          tk <- as.character(iss$tickers[[1]] %||% "")[1]
          if (!nzchar(tk)) next
          pdf <- tryCatch(fetch_price_history_df(tk, "5y"), error = function(e) NULL)
          if (!is.null(pdf) && is.data.frame(pdf) && nrow(pdf) >= 2L) price_map[[tk]] <- pdf
        }
        bench_tk <- tryCatch(macro_bench_ticker(mode), error = function(e) "^GSPC")
        bench_df <- tryCatch(fetch_price_history_df(bench_tk, "5y"), error = function(e) NULL)
      }
      fs_map <- list()
      if (exists("cached_scrape_financials", mode = "function") &&
          exists("hccsi_issuers", mode = "function")) {
        for (iss in hccsi_issuers(cfg)) {
          tk <- as.character(iss$tickers[[1]] %||% "")[1]
          if (!nzchar(tk)) next
          fs <- tryCatch(cached_scrape_financials(tk), error = function(e) NULL)
          if (!is.null(fs)) fs_map[[tk]] <- fs
        }
      }
      inputs <- if (exists("hccsi_live_inputs_from_prices", mode = "function")) {
        hccsi_live_inputs_from_prices(price_map, bench_df, cfg, fs_map)
      } else NULL
      res <- if (exists("hccsi_score", mode = "function")) {
        tryCatch(hccsi_score(inputs, cfg), error = function(e) NULL)
      } else {
        NULL
      }
      list(res = res, n_iss = n_iss, n_px = length(price_map), n_fs = length(fs_map))
    }
    .queue_hccsi <- function() {
      mode <- isolate(.mode())
      job <- isolate(hccsi_job()) + 1L
      hccsi_job(job)
      # #region agent log
      try(cat(paste0(
        "{\"sessionId\":\"ef0f33\",\"runId\":\"load\",\"hypothesisId\":\"HCCSI\",",
        "\"location\":\"macro_market_module.R:queue_hccsi\",\"message\":\"hccsi queued after flush\",",
        "\"data\":{\"job\":", job, ",\"mode\":\"", gsub("\"", "", as.character(mode)[1]), "\"},",
        "\"timestamp\":", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, trim = TRUE), "}\n"
      ), file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-ef0f33.log", append = TRUE), silent = TRUE)
      # #endregion
      run_job <- function() {
        if (!identical(isolate(hccsi_job()), job)) return()
        t0 <- proc.time()[["elapsed"]]
        out <- tryCatch(.hccsi_compute(mode), error = function(e) list(res = NULL, n_iss = 0L, n_px = 0L, n_fs = 0L))
        if (!identical(isolate(hccsi_job()), job)) return()
        # #region agent log
        try(cat(paste0(
          "{\"sessionId\":\"ef0f33\",\"runId\":\"load\",\"hypothesisId\":\"HCCSI\",",
          "\"location\":\"macro_market_module.R:queue_hccsi\",\"message\":\"hccsi finished\",",
          "\"data\":{\"job\":", job,
          ",\"elapsed_s\":", format(proc.time()[["elapsed"]] - t0, scientific = FALSE, trim = TRUE, digits = 4),
          ",\"n_iss\":", out$n_iss %||% 0L, ",\"n_px\":", out$n_px %||% 0L, ",\"n_fs\":", out$n_fs %||% 0L, "},",
          "\"timestamp\":", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, trim = TRUE), "}\n"
        ), file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-ef0f33.log", append = TRUE), silent = TRUE)
        # #endregion
        hccsi_val(out$res %||% list(availability = "unavailable"))
      }
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(run_job, delay = 0)
      } else {
        run_job()
      }
    }
    session$onFlushed(function() .queue_hccsi(), once = TRUE)
    observeEvent(refresh_token(), {
      hccsi_val(NULL)
      .queue_hccsi()
    }, ignoreInit = TRUE)
    hccsi_result <- reactive(hccsi_val())

    output$hccsi_expand_panel <- renderUI({
      if (.is_lite() || !isTRUE(hccsi_expanded())) return(NULL)
      res <- hccsi_result()
      if (exists("hccsi_expand_ui", mode = "function")) hccsi_expand_ui(res, .loc()) else NULL
    })

    # Rf : YNOW/TYNOW : HCCSI = 2:1:1 (widths 6 / 3 / 3). TW NDC on the next row.
    # HCCSI four-index expand stays Full-only, below this row.
    output$rf_signal_row <- renderUI({
      .loc()
      mode <- .mode()
      lite <- .is_lite()
      is_tw <- identical(mode, "TW")
      res <- tryCatch(hccsi_result(), error = function(e) NULL)
      if (is.null(res)) res <- list(availability = "loading")
      ynow_card <- .own_index_kpi_card()
      rf_col <- column(
        width = 6, class = "col-xs-12 col-sm-6 col-md-6",
        tags$div(
          class = "ynow-macro-kpi ynow-macro-kpi--rf",
          tags$div(class = "ynow-macro-kpi__label", id = "ynow_macro_rf_title", .ui("macro_rf_title")),
          uiOutput(ns("rf_box"))
        )
      )
      ynow_col <- column(
        width = 3, class = "col-xs-12 col-sm-3 col-md-3",
        if (!is.null(ynow_card)) ynow_card else tags$div(class = "ynow-macro-kpi ynow-macro-kpi--ynow", "—")
      )
      hccsi_col <- column(
        width = 3, class = "col-xs-12 col-sm-3 col-md-3",
        if (exists("hccsi_kpi_box", mode = "function")) {
          hccsi_kpi_box(res, lite = lite, locale = .loc(), ns = ns,
                        selected = isTRUE(hccsi_expanded()) && !isTRUE(lite))
        } else {
          tags$div(class = "ynow-macro-kpi ynow-macro-kpi--hccsi", "HCCSI")
        }
      )
      main_row <- do.call(
        fluidRow,
        list(class = "ynow-macro-rf-row ynow-macro-kpi-row", rf_col, ynow_col, hccsi_col)
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
      concept_key <- as.character(input$concept_key %||% "")[1]
      if (is.na(industry_key)) industry_key <- ""
      if (is.na(concept_key)) concept_key <- ""

      industry <- NULL
      concept <- NULL
      if (nzchar(industry_key)) {
        industry <- tryCatch(
          macro_rebased_theme_df(industry_key, mode, period),
          error = function(e) NULL
        )
      }
      if (nzchar(concept_key)) {
        concept <- tryCatch(
          macro_rebased_theme_df(concept_key, mode, period),
          error = function(e) NULL
        )
      }

      bench_tk <- macro_bench_ticker(mode)
      bench <- NULL
      if (nzchar(industry_key) || nzchar(concept_key)) {
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
        concept = concept,
        bench = bench,
        bench_ticker = bench_tk,
        industry_key = industry_key,
        concept_key = concept_key
      )
    })

    output$overlay_plot <- plotly::renderPlotly({
      od <- overlay_data()
      has_ind_key <- nzchar(od$industry_key %||% "")
      has_con_key <- nzchar(od$concept_key %||% "")
      shiny::validate(shiny::need(
        has_ind_key || has_con_key,
        .ui("macro_plot_need_pick")
      ))
      shiny::validate(shiny::need(
        !is.null(od$industry) || !is.null(od$concept),
        .ui("macro_plot_need_theme")
      ))
      shiny::validate(shiny::need(!is.null(od$bench), .ui("macro_plot_need_bench")))

      bh <- od$bench
      common <- bh$Date
      if (!is.null(od$industry)) common <- intersect(common, od$industry$Date)
      if (!is.null(od$concept)) common <- intersect(common, od$concept$Date)
      shiny::validate(shiny::need(length(common) >= 5L, .ui("macro_plot_need_theme")))
      bh <- bh[bh$Date %in% common, , drop = FALSE]

      industry_lab <- .ui("macro_series_industry")
      concept_lab <- .ui("macro_series_concept")
      left_lab <- if (!is.null(od$industry) && !is.null(od$concept)) {
        .ui("macro_series_rebased")
      } else if (!is.null(od$industry)) {
        industry_lab
      } else {
        concept_lab
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
      if (!is.null(od$concept)) {
        th <- od$concept[od$concept$Date %in% common, , drop = FALSE]
        fig <- plotly::add_trace(
          fig,
          x = th$Date, y = th$Theme,
          type = "scatter", mode = "lines",
          name = concept_lab,
          line = list(color = "#2980b9", width = 2),
          yaxis = "y"
        )
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
      )
    })

    # Macro-page theme-vs-benchmark β chart is no longer rendered.
    # Keep macro_rolling_beta_path() + estimate_rolling_beta() for CAPM / HFV / backtest.

    # ---- Bubble & concentration (display-only; shares Industry/Concept picks) ----
    output$bubble_shared_pick_status <- renderUI({
      mode <- .mode()
      loc <- .loc()
      ind <- as.character(input$industry_key %||% "")[1]
      con <- as.character(input$concept_key %||% "")[1]
      if (is.na(ind)) ind <- ""
      if (is.na(con)) con <- ""
      ind_ch <- macro_choices_with_none(macro_industry_choices(mode, loc), loc)
      con_ch <- macro_choices_with_none(macro_concept_choices(mode, loc), loc)
      if (!nzchar(ind) && !nzchar(con)) {
        return(tags$p(
          id = "ynow_macro_bubble_shared_status",
          class = "ynow-macro-hint",
          .ui("macro_bubble_shared_need_pick")
        ))
      }
      if (nzchar(ind) && nzchar(con)) {
        msg <- gsub(
          "{industry}", .choice_label(ind_ch, ind),
          .ui("macro_bubble_shared_using_industry_both"),
          fixed = TRUE
        )
        msg <- gsub("{concept}", .choice_label(con_ch, con), msg, fixed = TRUE)
      } else if (nzchar(ind)) {
        msg <- gsub(
          "{industry}", .choice_label(ind_ch, ind),
          .ui("macro_bubble_shared_using_industry"),
          fixed = TRUE
        )
      } else {
        msg <- gsub(
          "{concept}", .choice_label(con_ch, con),
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
        legend = list(orientation = "h", y = 1.12),
        margin = list(l = 50, r = 20, t = 30, b = 40),
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
          class = "table table-condensed ynow-hccsi-table",
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
      top_c <- if (is.finite(att$top_contrib)) att$top_contrib else 0
      rest_c <- if (is.finite(att$rest_contrib)) att$rest_contrib else 0
      df <- data.frame(
        part = c(sprintf("Top %d", bd$top_n), .ui("macro_bubble_rest")),
        contrib = c(top_c, rest_c) * 100,
        stringsAsFactors = FALSE
      )
      fig <- plotly::plot_ly(
        df,
        x = ~contrib,
        y = ~part,
        type = "bar",
        orientation = "h",
        marker = list(color = c("#e67e22", "#7f8c8d"))
      )
      plotly::layout(
        fig,
        xaxis = list(title = .ui("macro_bubble_attr_axis")),
        yaxis = list(title = ""),
        margin = list(l = 80, r = 20, t = 10, b = 40),
        showlegend = FALSE
      )
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
        "macro_bubble_buffett_mcap_hint"
      )
    })

    output$bubble_buffett_gdp <- renderUI({
      .loc()
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
        "macro_bubble_buffett_gdp_hint"
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
        title = list(text = .ui("macro_bubble_buffett_chart"), font = list(size = 13)),
        xaxis = list(title = ""),
        yaxis = list(title = .ui("macro_bubble_buffett_axis")),
        legend = list(orientation = "h", y = 1.12),
        margin = list(l = 50, r = 20, t = 50, b = 40),
        hovermode = "x unified"
      )
    })
  })
}
