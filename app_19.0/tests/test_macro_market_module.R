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
check("US themes nonempty", length(ch_us) > 10)
check("TW themes nonempty", length(ch_tw) > 5)
check("US has XLK gics", "gics_xlk" %in% unname(ch_us))
check("XLK ticker", identical(macro_theme_tickers("gics_xlk", "US"), "XLK"))
check("TW shipping has 2603", "2603.TW" %in% macro_theme_tickers("tw_shipping", "TW"))

for (k in c("menu_macro_market", "macro_fx_lock", "macro_page_title", "macro_theme_title")) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
}

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

cat("All macro market module checks passed.\n")
