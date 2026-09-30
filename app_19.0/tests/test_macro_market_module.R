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

click_us <- macro_click_index_specs("US")
click_tw <- macro_click_index_specs("TW")
check("click specs same in US and TW", identical(click_us, click_tw))
check(
  "click specs four Yahoo symbols",
  identical(names(click_us), c("^GSPC", "^IXIC", "^DJI", "^SOX"))
)
check(
  "click specs four labels",
  identical(unname(click_us), c("S&P 500", "Nasdaq", "Dow Jones", "SOX (semis)"))
)
check("box id GSPC", identical(macro_index_box_id("^GSPC"), "ynow_macro_idx_gspc"))
check("box id IXIC", identical(macro_index_box_id("^IXIC"), "ynow_macro_idx_ixic"))
check("box id DJI", identical(macro_index_box_id("^DJI"), "ynow_macro_idx_dji"))
check("box id SOX", identical(macro_index_box_id("^SOX"), "ynow_macro_idx_sox"))
check("name key GSPC", identical(macro_index_name_key("^GSPC"), "macro_index_name_gspc"))

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
  "macro_series_rebased", "macro_bubble_theme_label",
  "macro_index_name_gspc", "macro_index_name_ixic", "macro_index_name_dji",
  "macro_index_name_sox", "macro_index_chart_hint", "macro_index_chart_empty",
  "macro_index_chart_error"
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
check("en index names stay English", identical(ui_str("macro_index_name_gspc", "en"), "S&P 500"))
check("zh index names stay English", identical(ui_str("macro_index_name_gspc", "zh-TW"), "S&P 500"))
check("en SOX name", identical(ui_str("macro_index_name_sox", "en"), "SOX (semis)"))
check("zh SOX name", identical(ui_str("macro_index_name_sox", "zh-TW"), "SOX (semis)"))
check("en chart hint", grepl("Click an index box", ui_str("macro_index_chart_hint", "en"), fixed = TRUE))
check("zh chart hint", grepl("點選指數方塊", ui_str("macro_index_chart_hint", "zh-TW"), fixed = TRUE))

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

check("leftover theme help is notes", grepl("ynow_notes_block", txt, fixed = TRUE) &&
        grepl("ynow_macro_theme_help", txt, fixed = TRUE))
check("leftover fx lock is notes", grepl("ynow_macro_fx_lock", txt, fixed = TRUE))
check("overlay plot not inside leftover notes", {
  m <- regexpr("ynow_notes_block\\s*\\(", txt)
  if (m < 1L) FALSE else {
    chunk <- substr(txt, as.integer(m), as.integer(m) + 420L)
    grepl("ynow_macro_theme_help", chunk, fixed = TRUE) &&
      !grepl("overlay_plot", chunk, fixed = TRUE)
  }
})
check("overlay industry input", grepl('ns("industry_key")', txt, fixed = TRUE))
check("overlay concept input", grepl('ns("concept_key")', txt, fixed = TRUE))
check("combined overlay theme_key gone", !grepl('ns("theme_key")', txt, fixed = TRUE))
check("combined title gone", !grepl("Industry / concept vs benchmark", txt, fixed = TRUE))
check("industry vs benchmark first paint", grepl("Industry vs benchmark", txt, fixed = TRUE))
check("concept vs benchmark first paint", grepl("Concept vs benchmark", txt, fixed = TRUE))
check("empty none default", grepl("industry_key", txt, fixed = TRUE) && grepl('selected = ""', txt, fixed = TRUE))
check("no Lite-only combined menu", !grepl("ynow-lite-only", txt, fixed = TRUE))
check("click specs helper", grepl("macro_click_index_specs", txt, fixed = TRUE))
check("KPI uses click specs not TW catalog", grepl("macro_click_index_specs(.mode())", txt, fixed = TRUE))
check("clickable box id GSPC", grepl("ynow_macro_idx_gspc", txt, fixed = TRUE))
check("clickable box id IXIC", grepl("ynow_macro_idx_ixic", txt, fixed = TRUE))
check("clickable box id DJI", grepl("ynow_macro_idx_dji", txt, fixed = TRUE))
check("clickable box id SOX", grepl("ynow_macro_idx_sox", txt, fixed = TRUE))
check("index click input", grepl("index_click", txt, fixed = TRUE))
check("index hist plot output", grepl("index_hist_plot", txt, fixed = TRUE))
check("index hist panel above Rf", {
  pos_hist <- regexpr("ynow_macro_index_hist", txt, fixed = TRUE)[1]
  pos_rf <- regexpr("rf_signal_row", txt, fixed = TRUE)[1]
  is.finite(pos_hist) && pos_hist > 0 && is.finite(pos_rf) && pos_hist < pos_rf
})
check("hint above Rf", {
  pos_hint <- regexpr("ynow_macro_index_hint", txt, fixed = TRUE)[1]
  pos_rf <- regexpr("rf_signal_row", txt, fixed = TRUE)[1]
  is.finite(pos_hint) && pos_hint > 0 && pos_hint < pos_rf
})
check("chart slot is full-only", grepl("ynow-macro-index-hist ynow-full-only", txt, fixed = TRUE))
check("hint is full-only", grepl("ynow-macro-hint ynow-full-only", txt, fixed = TRUE))
check("lite gate on click", grepl(".is_lite", txt, fixed = TRUE) && grepl("lite_mode_rv", txt, fixed = TRUE))
check("keeps showing on same box", !grepl("selected_index(\"\")", txt, fixed = TRUE))
check("no Theme Rolling beta return", !grepl("Theme Rolling β vs benchmark", txt, fixed = TRUE))

ui_css <- ui_src
check("lite CSS hides chart slot", grepl("body.ynow-lite #ynow_macro_index_hist", ui_css, fixed = TRUE))
check("lite CSS disables clickable boxes", grepl("body.ynow-lite .ynow-macro-kpi--clickable", ui_css, fixed = TRUE))
check("locale wires index hint", grepl("ynow_macro_index_hint", ui_css, fixed = TRUE))

cat("All macro market module checks passed.\n")
