#!/usr/bin/env Rscript
# YNOW tab: title, KPI jump row above Section I, two-block order, Lite=Full
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

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
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
check("en ch1 Statement quality", identical(.UI_STRINGS$en$funnel_ch1_title, "Statement quality"))
check("zh ch1 財報體質", identical(.UI_STRINGS$`zh-TW`$funnel_ch1_title, "財報體質"))
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
check("notes_title en", identical(ui_str("notes_title", "en"), "Notes"))
check("notes_title zh", identical(ui_str("notes_title", "zh-TW"), "附註"))
check("notes_title not 备注", !grepl("备注", ui_str("notes_title", "zh-TW"), fixed = TRUE))
check("notes_title not 注释", !grepl("注释", ui_str("notes_title", "zh-TW"), fixed = TRUE))
check("notes_toggle_aria en", nzchar(ui_str("notes_toggle_aria", "en")))
check("notes_toggle_aria zh", nzchar(ui_str("notes_toggle_aria", "zh-TW")))
check("notes helper exists", exists("ynow_notes_block", mode = "function"))
check("no Decision Funnel title", !grepl("Decision Funnel", .UI_STRINGS$en$funnel_page_title, fixed = TRUE))
check("no 決策漏斗 title", !grepl("決策漏斗", .UI_STRINGS$`zh-TW`$funnel_page_title, fixed = TRUE))
check("page_sub click MOS", grepl("Click MOS / Reliability", .UI_STRINGS$en$funnel_page_sub, fixed = TRUE))
check("page_sub zh 點選", grepl("點選 MOS／Reliability", .UI_STRINGS$`zh-TW`$funnel_page_sub, fixed = TRUE))
check(
  "jump aria keys",
  nzchar(ui_str("funnel_kpi_jump_mos_aria", "en")) &&
    nzchar(ui_str("funnel_kpi_jump_fscore_aria", "en")) &&
    nzchar(ui_str("funnel_kpi_jump_alerts_aria", "en")) &&
    nzchar(ui_str("funnel_kpi_jump_mos_aria", "zh-TW")) &&
    nzchar(ui_str("funnel_kpi_jump_fscore_aria", "zh-TW")) &&
    nzchar(ui_str("funnel_kpi_jump_alerts_aria", "zh-TW"))
)
check("en aria keeps F-Score", grepl("F-Score", ui_str("funnel_kpi_jump_fscore_aria", "en"), fixed = TRUE))
check("zh aria keeps F-Score", grepl("F-Score", ui_str("funnel_kpi_jump_fscore_aria", "zh-TW"), fixed = TRUE))
check("zh aria keeps MOS", grepl("MOS", ui_str("funnel_kpi_jump_mos_aria", "zh-TW"), fixed = TRUE))

dec <- paste(readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui_start <- regexpr("decision_ui <- function", dec, fixed = TRUE)[1]
ui_end <- regexpr("decision_momentum_panel_ui", dec, fixed = TRUE)[1]
check("decision_ui slice", is.finite(ui_start) && ui_start > 0 && ui_end > ui_start)
ui_fn <- if (ui_start > 0 && ui_end > ui_start) substr(dec, ui_start, ui_end) else dec

pos_q <- regexpr('`data-ynow-block` = "quality"', ui_fn, fixed = TRUE)[1]
pos_a <- regexpr('`data-ynow-block` = "alerts"', ui_fn, fixed = TRUE)[1]
pos_b <- regexpr('`data-ynow-block` = "bubble"', ui_fn, fixed = TRUE)[1]
check("quality block present", is.finite(pos_q) && pos_q > 0)
check("order 體質 → 警訊", is.finite(pos_a) && pos_a > pos_q)
check("bubble not on YNOW", !is.finite(pos_b) || pos_b < 1)

pos_ch1_id <- regexpr('id = "ynow_funnel_ch1"', ui_fn, fixed = TRUE)[1]
pos_ch1_title <- regexpr("ynow_funnel_ch1_title", ui_fn, fixed = TRUE)[1]
pos_mos <- regexpr("vbox_mos", ui_fn, fixed = TRUE)[1]
pos_fs <- regexpr("vbox_fscore", ui_fn, fixed = TRUE)[1]
pos_fraud <- regexpr("vbox_fraud", ui_fn, fixed = TRUE)[1]
pos_tbl <- regexpr("fscore_panel", ui_fn, fixed = TRUE)[1]
pos_shen <- regexpr("shenanigans_panel", ui_fn, fixed = TRUE)[1]
check("MOS card before F-Score", pos_mos > 0 && pos_fs > pos_mos)
check("F-Score card before alerts", pos_fs > 0 && pos_fraud > pos_fs)
check("KPI row before Section I heading", pos_fraud > 0 && pos_ch1_title > pos_fraud && pos_ch1_id > pos_fraud)
check("Section I heading not in KPI row", pos_ch1_title > pos_ch1_id && pos_ch1_id > pos_fraud)
check("F-Score boxes still in Section I", pos_tbl > pos_q && pos_tbl < pos_a)
check("shenanigans still in Section II", pos_shen > pos_a)

ch1_body <- if (pos_q > 0 && pos_a > pos_q) substr(ui_fn, pos_q, pos_a) else ""
ch2_body <- if (pos_a > 0) substr(ui_fn, pos_a, nchar(ui_fn)) else ""
check("no vbox_mos inside ch1", !grepl("vbox_mos", ch1_body, fixed = TRUE))
check("no vbox_fscore inside ch1", !grepl("vbox_fscore", ch1_body, fixed = TRUE))
check("no vbox_fraud inside ch1", !grepl("vbox_fraud", ch1_body, fixed = TRUE))
check("no vbox_fraud inside ch2", !grepl("vbox_fraud", ch2_body, fixed = TRUE))
check("no vbox_fscore inside ch2", !grepl("vbox_fscore", ch2_body, fixed = TRUE))
check("href MOS → ch1", grepl('href = "#ynow_funnel_ch1"', ui_fn, fixed = TRUE))
check("href F-Score → fscore", grepl('href = "#ynow_funnel_fscore"', ui_fn, fixed = TRUE))
check("href alerts → ch2", grepl('href = "#ynow_funnel_ch2"', ui_fn, fixed = TRUE))
check("jump id MOS", grepl("ynow_kpi_jump_mos", ui_fn, fixed = TRUE))
check("jump id F-Score", grepl("ynow_kpi_jump_fscore", ui_fn, fixed = TRUE))
check("jump id alerts", grepl("ynow_kpi_jump_alerts", ui_fn, fixed = TRUE))
check("role=button on jump cards", grepl('role = "button"', dec, fixed = TRUE))
check("fscore table id", grepl('id = "ynow_funnel_fscore"', ui_fn, fixed = TRUE))
.notes_calls <- function(txt) {
  calls <- character(0)
  remaining <- txt
  repeat {
    m <- regexpr("ynow_notes_block\\s*\\(", remaining)
    if (m < 1L) break
    start <- as.integer(m)
    depth <- 0L
    end <- nchar(remaining)
    for (i in seq.int(start, nchar(remaining))) {
      ch <- substr(remaining, i, i)
      if (identical(ch, "(")) depth <- depth + 1L
      if (identical(ch, ")")) {
        depth <- depth - 1L
        if (depth <= 0L) {
          end <- i
          break
        }
      }
    }
    calls <- c(calls, substr(remaining, start, end))
    remaining <- substr(remaining, end + 1L, nchar(remaining))
  }
  calls
}
notes_calls <- .notes_calls(ui_fn)
check("YNOW chapter notes remain", length(notes_calls) >= 2L)
check("page sub present", grepl("ynow_funnel_page_sub", ui_fn, fixed = TRUE))
check("page sub outside notes", !any(grepl("ynow_funnel_page_sub", notes_calls, fixed = TRUE)))
pos_mh <- regexpr("ynow-funnel-report__masthead", ui_fn, fixed = TRUE)[1]
pos_kpi <- regexpr("ynow-funnel-kpi-jump-row", ui_fn, fixed = TRUE)[1]
masthead <- if (pos_mh > 0 && pos_kpi > pos_mh) substr(ui_fn, pos_mh, pos_kpi) else ""
check("page sub in masthead chrome", grepl("ynow_funnel_page_sub", masthead, fixed = TRUE))
check("masthead has no notes wrapper", !grepl("ynow_notes_block", masthead, fixed = TRUE))
check("YNOW notes default collapsed", !grepl("ynow_notes_block\\([^)]*open\\s*=\\s*TRUE", ui_fn))
check("ch1 lead wrapped", grepl("ynow_notes_block", ch1_body, fixed = TRUE) &&
        grepl("ynow_funnel_ch1_lead", ch1_body, fixed = TRUE))
check("ch2 lead wrapped", grepl("ynow_notes_block", ch2_body, fixed = TRUE) &&
        grepl("ynow_funnel_ch2_lead", ch2_body, fixed = TRUE))
.notes_call <- function(txt) {
  m <- regexpr("ynow_notes_block\\s*\\(", txt)
  if (m < 1L) return("")
  start <- as.integer(m)
  depth <- 0L
  for (i in seq.int(start, nchar(txt))) {
    ch <- substr(txt, i, i)
    if (identical(ch, "(")) depth <- depth + 1L
    if (identical(ch, ")")) {
      depth <- depth - 1L
      if (depth <= 0L) return(substr(txt, start, i))
    }
  }
  substr(txt, start, nchar(txt))
}
ch1_notes <- .notes_call(ch1_body)
ch2_notes <- .notes_call(ch2_body)
check("F-Score boxes not inside ch1 notes call", grepl("ynow_funnel_ch1_lead", ch1_notes, fixed = TRUE) &&
        !grepl("fscore_panel", ch1_notes, fixed = TRUE) &&
        !grepl("table_checklist", ch1_notes, fixed = TRUE))
check("F-Score panel output in ch1", grepl("fscore_panel", ch1_body, fixed = TRUE))
check("alerts panel not inside ch2 notes call", grepl("ynow_funnel_ch2_lead", ch2_notes, fixed = TRUE) &&
        !grepl("shenanigans_panel", ch2_notes, fixed = TRUE))

bt <- paste(readLines("macro_bubble_indicators.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pos_conc <- regexpr("bubble_conc_kpi", bt, fixed = TRUE)[1]
pos_plot <- regexpr("bubble_conc_plot", bt, fixed = TRUE)[1]
check("bubble KPI above conc plot", pos_conc > 0 && pos_plot > pos_conc)

check("no lite-only YNOW markup", !grepl("ynow-lite-only", dec, fixed = TRUE))
check("no full-only YNOW markup", !grepl("ynow-full-only", dec, fixed = TRUE))

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "sensitivity tab mounts decision_ui",
  grepl('tabName = "sensitivity"', ui, fixed = TRUE) &&
    grepl('decision_ui("main_decision")', ui, fixed = TRUE)
)
check("applyUiLocale ch1 lead", grepl("ynow_funnel_ch1_lead", ui, fixed = TRUE))
check("applyUiLocale notes titles", grepl("ynow-notes__title", ui, fixed = TRUE) &&
        grepl("s.notes_title", ui, fixed = TRUE))
check("notes CSS details", grepl(".ynow-notes", ui, fixed = TRUE) &&
        grepl(".ynow-notes__summary", ui, fixed = TRUE))
check("F-Score card CSS", grepl(".ynow-fscore-card", ui, fixed = TRUE) &&
        grepl(".ynow-fscore-wrap", ui, fixed = TRUE) &&
        grepl(".ynow-fscore-grid", ui, fixed = TRUE))
check("applyUiLocale page title", grepl("ynow_funnel_page_title", ui, fixed = TRUE))
check("applyUiLocale kpi jump aria", grepl("ynow_kpi_jump_mos", ui, fixed = TRUE) &&
        grepl("funnel_kpi_jump_mos_aria", ui, fixed = TRUE))
check("click handler scrollIntoView", grepl("ynowOnFunnelKpiJump", ui, fixed = TRUE) &&
        grepl("ynowScrollFunnelAnchor", ui, fixed = TRUE) &&
        grepl("scrollIntoView", ui, fixed = TRUE))
check("Lite About YNOW keeps F-Score and 財報警訊", grepl("財報體質（F-Score）與財報警訊", ui, fixed = TRUE))
check("Lite About EN uses statement quality", grepl("statement quality (F-Score) and statement alerts", ui, fixed = TRUE))
check("first-paint F-Score list title zh", grepl("F-Score 品質檢核清單", dec, fixed = TRUE))

macro <- paste(readLines("macro_market_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("bubble at Macro bottom", {
  pos_own <- regexpr("ynow_own_index", macro, fixed = TRUE)[1]
  pos_bub <- regexpr("macro_bubble_chapter_ui", macro, fixed = TRUE)[1]
  is.finite(pos_own) && pos_own > 0 && is.finite(pos_bub) && pos_bub > pos_own
})
check("split industry picker", grepl('ns("industry_key")', macro, fixed = TRUE))
check("split concept picker", grepl('ns("concept_key")', macro, fixed = TRUE))
check("Macro first-paint bubble title", grepl("Dynamic industry bubble", macro, fixed = TRUE))

if (fail > 0L) {
  stop(sprintf("%d YNOW layout check(s) failed", fail), call. = FALSE)
}
cat("PASS ynow_tab_layout\n")
