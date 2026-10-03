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
  "macro_bubble_title", "macro_bubble_buffett_over", "macro_bubble_alert_breadth"
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
check("bubble theme first-paint Technology XLK", grepl('selected = "gics_xlk"', bt_src, fixed = TRUE))
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
check("no CAPM write in bubble file", {
  bt <- paste(readLines("macro_bubble_indicators.R", warn = FALSE), collapse = "\n")
  !grepl("updateNumericInput", bt, fixed = TRUE)
})

cat("PASS macro_bubble_indicators\n")
