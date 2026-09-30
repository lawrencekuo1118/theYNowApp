# Macro & Market Trends — catalog / isolation checks (no network)
# Run: cd app_19.0 && Rscript tests/test_macro_market_module.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
root <- if (file.exists("macro_market_module.R")) {
  normalizePath(".")
} else if (file.exists("../macro_market_module.R")) {
  normalizePath("..")
} else if (dir.exists("app_19.0") && file.exists("app_19.0/macro_market_module.R")) {
  normalizePath("app_19.0")
} else if (dir.exists("app_18.0") && file.exists("app_18.0/macro_market_module.R")) {
  normalizePath("app_18.0")
} else {
  stop("Cannot locate macro_market_module.R")
}
setwd(root)

source("debug_helpers.R", local = TRUE, encoding = "UTF-8")
source("market_profile.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("lab_concept_groups.R", local = TRUE, encoding = "UTF-8")
fetch_price_history_df <- function(...) NULL
estimate_rolling_beta <- function(...) NA_real_
source("macro_market_module.R", local = TRUE, encoding = "UTF-8")

check <- function(label, ok) {
  if (!isTRUE(ok)) stop("FAIL: ", label)
  cat("OK ", label, "\n", sep = "")
}

check("US bench", identical(macro_bench_ticker("US"), "^GSPC"))
check("TW bench", identical(macro_bench_ticker("TW"), "^TWII"))

ch_us <- macro_theme_choices("US", "en")
ch_tw <- macro_theme_choices("TW", "zh-TW")
ind_us <- macro_industry_choices("US", "en")
ind_tw <- macro_industry_choices("TW", "zh-TW")
con_us <- macro_concept_choices("US", "en")
con_tw <- macro_concept_choices("TW", "zh-TW")
check("US themes nonempty", length(ch_us) > 10)
check("TW themes nonempty", length(ch_tw) > 5)
check("US industry is GICS only", length(ind_us) >= 11L && all(startsWith(unname(ind_us), "gics_")))
check("US industry has no concept keys", !any(startsWith(unname(ind_us), "concept_")))
check("TW industry empty (no sector-ETF catalog)", length(ind_tw) == 0L)
check("US concept has mag7", "concept_mag7" %in% unname(con_us))
check("US concept has no GICS keys", !any(startsWith(unname(con_us), "gics_")))
check("TW concept has shipping", "tw_shipping" %in% unname(con_tw))
check("US has XLK gics", "gics_xlk" %in% unname(ch_us) && "gics_xlk" %in% unname(ind_us))
check("XLK ticker", identical(macro_theme_tickers("gics_xlk", "US"), "XLK"))
check("TW shipping has 2603", "2603.TW" %in% macro_theme_tickers("tw_shipping", "TW"))
check("empty key no tickers", identical(macro_theme_tickers("", "US"), character(0)))
check("empty rebased is NULL", is.null(macro_rebased_theme_df("", "US", "1y")))
check("unknown rebased is NULL", is.null(macro_rebased_theme_df("gics_xlk", "US", "1y")))

none_en <- macro_none_choice("en")
none_zh <- macro_none_choice("zh-TW")
check("none en empty value", identical(unname(none_en), ""))
check("none zh empty value", identical(unname(none_zh), ""))
check("none en label", identical(names(none_en), ui_str("macro_none_option", "en")))
check("none zh label", identical(names(none_zh), ui_str("macro_none_option", "zh-TW")))
with_none <- macro_choices_with_none(ind_us, "en")
check("none prepended", identical(unname(with_none)[1], "") && "gics_xlk" %in% unname(with_none))

for (k in c(
  "menu_macro_market", "macro_fx_lock", "macro_page_title", "macro_theme_title",
  "macro_industry_label", "macro_concept_label", "macro_none_option",
  "macro_plot_need_pick", "macro_series_industry", "macro_series_concept",
  "macro_series_rebased", "macro_bubble_theme_label"
)) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
  check(paste("zh key present", k), !is.null(.UI_STRINGS$`zh-TW`[[k]]))
}
check("en industry label", identical(ui_str("macro_industry_label", "en"), "Industry vs benchmark"))
check("zh industry label", identical(ui_str("macro_industry_label", "zh-TW"), "產業別 vs 大盤"))
check("en concept label", identical(ui_str("macro_concept_label", "en"), "Concept vs benchmark"))
check("zh concept label", identical(ui_str("macro_concept_label", "zh-TW"), "概念股別 vs 大盤"))
check("en title not combined", !grepl("Industry / concept vs benchmark", ui_str("macro_theme_title", "en"), fixed = TRUE))
check("zh title not combined", !grepl("產業／概念股 vs 大盤", ui_str("macro_theme_title", "zh-TW"), fixed = TRUE))
check("zh none is 不選", identical(ui_str("macro_none_option", "zh-TW"), "不選"))

for (k in c(
  "macro_beta_title", "macro_beta_warn_title", "macro_beta_warn_body",
  "macro_beta_latest", "macro_beta_kpi_hint", "macro_beta_need_data",
  "macro_beta_chart_title"
)) {
  check(paste("en orphan", k), is.null(.UI_STRINGS$en[[k]]))
  check(paste("zh orphan", k), is.null(.UI_STRINGS$`zh-TW`[[k]]))
}

txt <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
check("no CAPM writeback", !grepl("updateNumericInput", txt, fixed = TRUE))
check("warn copy present", grepl("CAPM", txt, fixed = TRUE))
check("theme rolling beta title gone", !grepl("Theme Rolling β vs benchmark", txt, fixed = TRUE))
check("ynow_macro_beta_title gone", !grepl("ynow_macro_beta_title", txt, fixed = TRUE))
check("beta_plot output gone", !grepl("beta_plot", txt, fixed = TRUE))
check("beta_kpi output gone", !grepl("beta_kpi", txt, fixed = TRUE))
check("rolling beta engine kept", grepl("macro_rolling_beta_path", txt, fixed = TRUE))
check("estimate_rolling_beta still used", grepl("estimate_rolling_beta", txt, fixed = TRUE))

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE), collapse = "\n")
check("sidebar first macro", grepl('tabName = "macro_market"', ui_src, fixed = TRUE))
# macro menuItem appears before Dashboard menuItem
pos_macro <- regexpr('tabName = "macro_market"', ui_src, fixed = TRUE)[1]
pos_dash <- regexpr('tabName = "dashboard"', ui_src, fixed = TRUE)[1]
check("macro before dashboard in UI", is.finite(pos_macro) && pos_macro > 0 && pos_macro < pos_dash)
check(
  "hide ticker chrome on macro",
  grepl("input.sidebar_tabs != 'about' && input.sidebar_tabs != 'macro_market'", ui_src, fixed = TRUE)
)
check("locale wiring dropped beta title", !grepl("ynow_macro_beta_title", ui_src, fixed = TRUE))
check("locale wiring dropped beta warn", !grepl("macro_beta_warn_title", ui_src, fixed = TRUE))
check("locale wires industry label", grepl("ynow_macro_industry_label", ui_src, fixed = TRUE))
check("locale wires concept label", grepl("ynow_macro_concept_label", ui_src, fixed = TRUE))
check("locale dropped combined theme label", !grepl("ynow_macro_theme_label", ui_src, fixed = TRUE))

check("overlay industry input", grepl('ns("industry_key")', txt, fixed = TRUE))
check("overlay concept input", grepl('ns("concept_key")', txt, fixed = TRUE))
check("combined overlay theme_key gone", !grepl('ns("theme_key")', txt, fixed = TRUE))
check("combined title gone", !grepl("Industry / concept vs benchmark", txt, fixed = TRUE))
check("industry vs benchmark first paint", grepl("Industry vs benchmark", txt, fixed = TRUE))
check("concept vs benchmark first paint", grepl("Concept vs benchmark", txt, fixed = TRUE))
check("empty none default", grepl("industry_key", txt, fixed = TRUE) && grepl('selected = ""', txt, fixed = TRUE))
check("no Lite-only combined menu", !grepl("ynow-lite-only", txt, fixed = TRUE))

cat("All macro market module checks passed.\n")
