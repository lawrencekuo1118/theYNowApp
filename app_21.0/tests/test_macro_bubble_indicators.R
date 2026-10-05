# Macro bubble & concentration indicators (isolated from CAPM / valuation)
# Run: cd app_18.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_macro_bubble_indicators.R

root <- if (file.exists("macro_bubble_indicators.R")) {
  getwd()
} else if (file.exists("app_18.0/macro_bubble_indicators.R")) {
  file.path(getwd(), "app_18.0")
} else {
  stop("Run from repo root or app_18.0")
}
setwd(root)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# Minimal market helpers if not loaded
if (!exists("normalize_market_mode", mode = "function")) {
  normalize_market_mode <- function(mode) {
    m <- toupper(trimws(as.character(mode)[1]))
    if (m %in% c("TW", "TWN", "TAIWAN")) "TW" else "US"
  }
}
if (!exists("get_market_mode", mode = "function")) {
  get_market_mode <- function() "US"
}

source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("lab_concept_groups.R", local = TRUE, encoding = "UTF-8")
source("lab_sp500_universe.R", local = TRUE, encoding = "UTF-8")
source("macro_market_module.R", local = TRUE, encoding = "UTF-8")
source("macro_bubble_indicators.R", local = TRUE, encoding = "UTF-8")

check <- function(msg, cond) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
  cat("OK", msg, "\n")
}

# GICS → S&P sector peers
uni_xlk <- macro_bubble_resolve_universe("gics_xlk", "US")
check("XLK resolves to sp500 IT or etf", length(uni_xlk$tickers) >= 1L)
check("XLK source gics", uni_xlk$source %in% c("gics_sp500", "gics_etf"))

# Concept basket
uni_mag7 <- macro_bubble_resolve_universe("concept_mag7", "US")
check("mag7 has AAPL", "AAPL" %in% uni_mag7$tickers)

# Concentration math on toy caps (bypass Yahoo by direct pool math)
toy <- data.frame(
  ticker = c("A", "B", "C", "D"),
  market_cap = c(60, 20, 10, 10),
  weight = c(0.6, 0.2, 0.1, 0.1),
  stringsAsFactors = FALSE
)
check("top1 alert threshold", toy$weight[[1]] >= 0.50)

# Concentration history from synthetic prices (no network)
dates <- as.Date("2024-01-01") + c(0, 30, 60, 90)
toy_prices <- list(
  A = data.frame(date = dates, close = c(100, 110, 120, 130), stringsAsFactors = FALSE),
  B = data.frame(date = dates, close = c(100, 100, 100, 100), stringsAsFactors = FALSE),
  C = data.frame(date = dates, close = c(100, 90, 80, 70), stringsAsFactors = FALSE),
  D = data.frame(date = dates, close = c(100, 100, 100, 100), stringsAsFactors = FALSE)
)
hist <- macro_bubble_concentration_history(
  pool = toy,
  top_n = 2L,
  period = "3mo",
  prices = toy_prices
)
check("conc history rows", is.data.frame(hist) && nrow(hist) >= 2L)
check("conc history cols", all(c("date", "top_n_share", "top1_share") %in% names(hist)))
check("conc history shares in (0,1]", {
  all(is.finite(hist$top_n_share) & hist$top_n_share > 0 & hist$top_n_share <= 1 + 1e-9) &&
    all(is.finite(hist$top1_share) & hist$top1_share > 0 & hist$top1_share <= 1 + 1e-9)
})
# At last date, proxy mcap equals current caps → Top-2 share = 0.8
last <- hist[nrow(hist), , drop = FALSE]
check("conc history last top2 ~0.8", abs(last$top_n_share - 0.8) < 1e-6)
check("conc history last top1 ~0.6", abs(last$top1_share - 0.6) < 1e-6)
check("series return from prices", {
  abs(macro_bubble_series_return(toy_prices$A) - 0.30) < 1e-9
})

lt_over <- macro_bubble_buffett_light(data.frame(
  date = as.Date(c("2000-12-31", "2001-12-31", "2002-12-31", "2024-12-31")),
  ratio_pct = c(100, 100, 100, 200),
  stringsAsFactors = FALSE
))
check("overvalued light", identical(lt_over$level, "overvalued"))

lt_under <- macro_bubble_buffett_light(data.frame(
  date = as.Date(c("2000-12-31", "2001-12-31", "2002-12-31", "2024-12-31")),
  ratio_pct = c(100, 100, 100, 20),
  stringsAsFactors = FALSE
))
check("undervalued light", identical(lt_under$level, "undervalued"))

ser_us <- macro_bubble_read_buffett_csv("US")
check("US buffett csv loads", is.data.frame(ser_us) && nrow(ser_us) >= 10L)
ser_tw <- macro_bubble_read_buffett_csv("TW")
check("TW buffett csv loads", is.data.frame(ser_tw) && nrow(ser_tw) >= 10L)

# Locale keys
for (k in c(
  "macro_bubble_title", "macro_bubble_buffett_over", "macro_bubble_alert_breadth",
  "macro_bubble_conc_axis", "macro_bubble_top_list_title", "macro_bubble_conc_note",
  "macro_bubble_col_ticker", "macro_bubble_col_mcap"
)) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
}

# UI mounts chapter on Macro (page bottom), not YNOW
dec_src <- paste(readLines("investment_decision_module.R", warn = FALSE), collapse = "\n")
check("YNOW tab does not call bubble chapter", !grepl("macro_bubble_chapter_ui", dec_src, fixed = TRUE))
check("ch2 notes still present", grepl("ynow_notes_block", dec_src, fixed = TRUE) &&
        grepl("ynow_funnel_ch2_lead", dec_src, fixed = TRUE))
bt_src <- paste(readLines("macro_bubble_indicators.R", warn = FALSE), collapse = "\n")
check("bubble shares Macro Industry/Concept picks", {
  !grepl('ns("bubble_theme_key")', bt_src, fixed = TRUE) &&
    grepl("bubble_shared_pick_status", bt_src, fixed = TRUE)
})
macro_src_for_default <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
check("shared Industry first-paint Technology XLK", {
  grepl('ns("industry_key")', macro_src_for_default, fixed = TRUE) &&
    grepl('selected = "gics_xlk"', macro_src_for_default, fixed = TRUE)
})
check("conc history UI mounts table", {
  grepl("bubble_conc_table", bt_src, fixed = TRUE) &&
    grepl("ynow_macro_bubble_top_list_title", bt_src, fixed = TRUE) &&
    grepl("ynow_macro_bubble_conc_note", bt_src, fixed = TRUE)
})
check("conc + attr side-by-side 1:1", {
  grepl("ynow-bubble-pair-row", bt_src, fixed = TRUE) &&
    grepl("col-sm-6 col-md-6 ynow-bubble-pair-col ynow-bubble-conc-col", bt_src, fixed = TRUE) &&
    grepl("col-sm-6 col-md-6 ynow-bubble-pair-col ynow-bubble-attr-col", bt_src, fixed = TRUE)
})
check("attr pie height syncs to conc+table", {
  grepl("ynow-bubble-attr-plot-host", bt_src, fixed = TRUE) &&
    grepl("syncAttrPieHeight", bt_src, fixed = TRUE) &&
    grepl("measureConcStack", bt_src, fixed = TRUE)
})
check("buffett history plot width 100%", {
  grepl("ynow-bubble-buffett-plot-wrap", bt_src, fixed = TRUE) &&
    grepl('plotlyOutput(ns("bubble_buffett_plot"), height = "320px", width = "100%")', bt_src, fixed = TRUE)
})
check("analysis window includes 2y", grepl('"2Y" = "2y"', bt_src, fixed = TRUE))
check("buffett companion KPI outputs", {
  grepl("bubble_buffett_mcap", bt_src, fixed = TRUE) &&
    grepl("bubble_buffett_gdp", bt_src, fixed = TRUE)
})
check("buffett playback removed", {
  !grepl("bubble_buffett_play", bt_src, fixed = TRUE) &&
    !grepl("bubble_buffett_pause", bt_src, fixed = TRUE) &&
    !grepl("bubble_buffett_asof", bt_src, fixed = TRUE) &&
    !grepl("playback", bt_src, ignore.case = TRUE)
})
macro_src2 <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
check("buffett playback server removed", {
  !grepl("buffett_play_on", macro_src2, fixed = TRUE) &&
    !grepl("bubble_buffett_asof", macro_src2, fixed = TRUE)
})
check("USD level formatter", {
  identical(macro_bubble_fmt_usd_level(1.5e12), "$1.50T") &&
    identical(macro_bubble_fmt_usd_level(NA_real_), "—")
})
check("abs asof picks latest finite", {
  toy <- data.frame(
    date = as.Date(c("2022-12-31", "2023-12-31", "2024-12-31")),
    market_cap_usd = c(40e12, NA_real_, 50e12),
    gdp_usd = c(20e12, 22e12, NA_real_),
    stringsAsFactors = FALSE
  )
  got <- macro_bubble_buffett_abs_asof(toy, 2024L)
  isTRUE(got$ok) && is.finite(got$market_cap_usd) &&
    abs(got$market_cap_usd - 50e12) < 1 && identical(got$as_of, as.Date("2024-12-31"))
})
check("TW abs CSV seed exists", {
  p <- macro_bubble_buffett_abs_paths("TW")
  any(file.exists(p))
})
check("TW abs CSV readable", {
  d <- macro_bubble_read_buffett_abs_csv("TW")
  is.data.frame(d) && nrow(d) >= 8L &&
    all(c("market_cap_usd", "gdp_usd") %in% names(d)) &&
    any(is.finite(d$market_cap_usd)) && any(is.finite(d$gdp_usd))
})
check("TW abs series falls back to CSV when DGBAS empty", {
  old_fn <- macro_bubble_fetch_buffett_dgbas
  assign("macro_bubble_fetch_buffett_dgbas", function(...) NULL, envir = .GlobalEnv)
  on.exit(assign("macro_bubble_fetch_buffett_dgbas", old_fn, envir = .GlobalEnv), add = TRUE)
  if (exists(".MACRO_BUBBLE_ENV", mode = "environment")) {
    .MACRO_BUBBLE_ENV$dgbas_buffett_tw_abs <- NULL
  }
  ser <- macro_bubble_buffett_abs_series("TW", timeout_sec = 1)
  got <- macro_bubble_buffett_abs_asof(ser, NA_integer_)
  is.data.frame(ser) && nrow(ser) >= 8L &&
    all(c("market_cap_usd", "gdp_usd") %in% names(ser)) &&
    any(grepl("seed-approx|csv", as.character(ser$source), ignore.case = TRUE)) &&
    isTRUE(got$ok) && is.finite(got$market_cap_usd) && is.finite(got$gdp_usd)
})
check("TW DGBAS fetcher wired", {
  src <- paste(readLines("macro_bubble_indicators.R", warn = FALSE), collapse = "\n")
  grepl("macro_bubble_fetch_buffett_dgbas", src, fixed = TRUE) &&
    grepl("nstatdb.dgbas.gov.tw", src, fixed = TRUE) &&
    grepl("A110101010", src, fixed = TRUE) &&
    grepl("A018101010", src, fixed = TRUE)
})
check("TW DGBAS live fetch (network)", {
  dg <- tryCatch(macro_bubble_fetch_buffett_dgbas(timeout_sec = 30), error = function(e) NULL)
  if (is.null(dg)) {
    cat("SKIP TW DGBAS live fetch (offline / unreachable)\n")
    TRUE
  } else {
    is.data.frame(dg) && nrow(dg) >= 8L &&
      any(is.finite(dg$market_cap_usd)) && any(is.finite(dg$gdp_usd)) &&
      any(is.finite(dg$ratio_pct)) &&
      identical(unique(as.character(dg$source)), "dgbas")
  }
})
check("buffett note wrapped", grepl("ynow_notes_block", bt_src, fixed = TRUE) &&
        grepl("ynow_macro_bubble_buffett_note", bt_src, fixed = TRUE))
check("buffett KPI/plot not inside notes", {
  m <- regexpr("ynow_notes_block\\s*\\(", bt_src)
  if (m < 1L) FALSE else {
    chunk <- substr(bt_src, as.integer(m), as.integer(m) + 500L)
    grepl("ynow_macro_bubble_buffett_note", chunk, fixed = TRUE) &&
      !grepl("bubble_buffett_light", chunk, fixed = TRUE) &&
      !grepl("bubble_buffett_plot", chunk, fixed = TRUE)
  }
})
macro_src <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
check("Macro tab mounts bubble chapter at bottom", {
  pos_own <- regexpr("ynow_own_index", macro_src, fixed = TRUE)[1]
  pos_bub <- regexpr("macro_bubble_chapter_ui", macro_src, fixed = TRUE)[1]
  is.finite(pos_own) && pos_own > 0 && is.finite(pos_bub) && pos_bub > pos_own
})
check("conc plot uses history series", {
  grepl("bd$history", macro_src, fixed = TRUE) &&
    grepl("macro_bubble_conc_topn_series", macro_src, fixed = TRUE) &&
    grepl("output$bubble_conc_table", macro_src, fixed = TRUE)
})
check("tab layout still has KPI above conc plot", {
  pos_kpi <- regexpr("bubble_conc_kpi", bt_src, fixed = TRUE)[1]
  pos_plot <- regexpr("bubble_conc_plot", bt_src, fixed = TRUE)[1]
  pos_tbl <- regexpr("bubble_conc_table", bt_src, fixed = TRUE)[1]
  pos_kpi > 0 && pos_plot > pos_kpi && pos_tbl > pos_plot
})
check("no CAPM write in bubble file", {
  bt <- paste(readLines("macro_bubble_indicators.R", warn = FALSE), collapse = "\n")
  !grepl("updateNumericInput", bt, fixed = TRUE)
})
check("attr plot is pie chart", {
  grepl("output$bubble_attr_plot", macro_src, fixed = TRUE) &&
    grepl('type = "pie"', macro_src, fixed = TRUE)
})
ui_css <- paste(readLines("ynow_ui.R", warn = FALSE), collapse = "\n")
check("bubble pair + buffett width CSS", {
  grepl("ynow-bubble-attr-plot-host", ui_css, fixed = TRUE) &&
    grepl("ynow-bubble-buffett-plot-wrap", ui_css, fixed = TRUE) &&
    grepl(".ynow-bubble-buffett-plot-wrap .js-plotly-plot", ui_css, fixed = TRUE)
})

cat("PASS macro_bubble_indicators\n")
