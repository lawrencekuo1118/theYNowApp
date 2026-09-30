#!/usr/bin/env Rscript
# YNOW tab: title, three-block order, conclusion cards above tables, Lite=Full
# Run: cd app_19.0 && Rscript tests/test_ynow_tab_layout.R

root <- if (file.exists("investment_decision_module.R")) {
  normalizePath(".")
} else if (file.exists("../investment_decision_module.R")) {
  normalizePath("..")
} else if (dir.exists("app_19.0") && file.exists("app_19.0/investment_decision_module.R")) {
  normalizePath("app_19.0")
} else {
  stop("Cannot locate app_19.0")
}
setwd(root)

source("ui_locale.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check("en title YNOW", identical(.UI_STRINGS$en$funnel_page_title, "YNOW"))
check("zh title YNOW", identical(.UI_STRINGS$`zh-TW`$funnel_page_title, "YNOW"))
check("en ch1 Quality screen (F-Score)", identical(.UI_STRINGS$en$funnel_ch1_title, "Quality screen (F-Score)"))
check("zh ch1 品質檢核（F-Score）", identical(.UI_STRINGS$`zh-TW`$funnel_ch1_title, "品質檢核（F-Score）"))
check("en ch2 alerts", identical(.UI_STRINGS$en$funnel_ch2_title, "Statement alerts"))
check("zh ch2 財報警訊", identical(.UI_STRINGS$`zh-TW`$funnel_ch2_title, "財報警訊"))
check(
  "en ch3 bubble",
  identical(.UI_STRINGS$en$funnel_ch3_title, "Dynamic industry bubble & weight concentration")
)
check(
  "zh ch3 泡沫集中度",
  identical(.UI_STRINGS$`zh-TW`$funnel_ch3_title, "動態產業泡沫與權重集中度")
)
check("ch1_lead en", nzchar(ui_str("funnel_ch1_lead", "en")))
check("ch1_lead zh", nzchar(ui_str("funnel_ch1_lead", "zh-TW")))
check("no Decision Funnel title", !grepl("Decision Funnel", .UI_STRINGS$en$funnel_page_title, fixed = TRUE))
check("no 決策漏斗 title", !grepl("決策漏斗", .UI_STRINGS$`zh-TW`$funnel_page_title, fixed = TRUE))

dec <- paste(readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pos_q <- regexpr('`data-ynow-block` = "quality"', dec, fixed = TRUE)[1]
pos_a <- regexpr('`data-ynow-block` = "alerts"', dec, fixed = TRUE)[1]
pos_b <- regexpr('`data-ynow-block` = "bubble"', dec, fixed = TRUE)[1]
check("quality block present", is.finite(pos_q) && pos_q > 0)
check("order 體質 → 警訊", is.finite(pos_a) && pos_a > pos_q)
check("order 警訊 → 泡沫", is.finite(pos_b) && pos_b > pos_a)

pos_fs <- regexpr("vbox_fscore", dec, fixed = TRUE)[1]
pos_tbl <- regexpr("table_checklist", dec, fixed = TRUE)[1]
check("F-Score box above table", pos_fs > pos_q && pos_fs < pos_tbl && pos_tbl < pos_a)

pos_fraud <- regexpr("vbox_fraud", dec, fixed = TRUE)[1]
pos_shen <- regexpr("shenanigans_panel", dec, fixed = TRUE)[1]
check("alert box above shenanigans", pos_fraud > pos_a && pos_fraud < pos_shen && pos_shen < pos_b)

bt <- paste(readLines("macro_bubble_indicators.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pos_conc <- regexpr("bubble_conc_kpi", bt, fixed = TRUE)[1]
pos_plot <- regexpr("bubble_conc_plot", bt, fixed = TRUE)[1]
check("bubble KPI above conc plot", pos_conc > 0 && pos_plot > pos_conc)

check("bubble mounted on YNOW", grepl("macro_bubble_chapter_ui", dec, fixed = TRUE))
check("no lite-only YNOW markup", !grepl("ynow-lite-only", dec, fixed = TRUE))
check("no full-only YNOW markup", !grepl("ynow-full-only", dec, fixed = TRUE))

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "sensitivity tab mounts decision_ui",
  grepl('tabName = "sensitivity"', ui, fixed = TRUE) &&
    grepl('decision_ui("main_decision")', ui, fixed = TRUE)
)
check("applyUiLocale ch1 lead", grepl("ynow_funnel_ch1_lead", ui, fixed = TRUE))
check("applyUiLocale page title", grepl("ynow_funnel_page_title", ui, fixed = TRUE))

macro <- paste(readLines("macro_market_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("bubble left Macro", !grepl("macro_bubble_chapter_ui", macro, fixed = TRUE))

if (fail > 0L) {
  stop(sprintf("%d YNOW layout check(s) failed", fail), call. = FALSE)
}
cat("PASS ynow_tab_layout\n")
