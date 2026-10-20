#!/usr/bin/env Rscript
# YNOW tab: masthead → KPI → verdict → risk matrix → Ch.1 quality → Ch.2 alerts
# Run: cd app_25.0/tests && Rscript test_ynow_tab_layout.R

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_25.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("investment_decision_module.R")
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
check("page_sub click QoE", grepl("Quality of Earnings (QoE)", .UI_STRINGS$en$funnel_page_sub, fixed = TRUE))
check("page_sub zh 點選 QoE", grepl("點選盈餘品質 (QoE)", .UI_STRINGS$`zh-TW`$funnel_page_sub, fixed = TRUE))
check("page_sub independent (no unlock gate)", grepl("no Decision Checklist", .UI_STRINGS$en$funnel_page_sub, fixed = TRUE))
check("page_sub zh 不需先完成", grepl("不需先完成", .UI_STRINGS$`zh-TW`$funnel_page_sub, fixed = TRUE))
check("page_sub may still name Decision SOP as optional", grepl("Decision SOP", .UI_STRINGS$en$funnel_page_sub, fixed = TRUE))
check("page_sub zh Decision SOP optional", grepl("Decision SOP", .UI_STRINGS$`zh-TW`$funnel_page_sub, fixed = TRUE))
check("page_sub statement-only focus", grepl("statements only", .UI_STRINGS$en$funnel_page_sub, fixed = TRUE))
check("page_sub zh 財報數字", grepl("財報數字", .UI_STRINGS$`zh-TW`$funnel_page_sub, fixed = TRUE))
check("verdict locale keys", nzchar(ui_str("funnel_verdict_title", "en")) &&
        nzchar(ui_str("funnel_verdict_title", "zh-TW")))
check("zh verdict title 財報結論", identical(ui_str("funnel_verdict_title", "zh-TW"), "YNOW 財報結論"))
check("risk locale keys", nzchar(ui_str("funnel_risk_title", "en")) &&
        identical(ui_str("funnel_risk_title", "zh-TW"), "自動化風險矩陣"))
check(
  "jump aria keys",
  nzchar(ui_str("funnel_kpi_jump_qoe_aria", "en")) &&
    nzchar(ui_str("funnel_kpi_jump_fscore_aria", "en")) &&
    nzchar(ui_str("funnel_kpi_jump_alerts_aria", "en")) &&
    nzchar(ui_str("funnel_kpi_jump_qoe_aria", "zh-TW")) &&
    nzchar(ui_str("funnel_kpi_jump_fscore_aria", "zh-TW")) &&
    nzchar(ui_str("funnel_kpi_jump_alerts_aria", "zh-TW"))
)
check("en aria keeps F-Score", grepl("F-Score", ui_str("funnel_kpi_jump_fscore_aria", "en"), fixed = TRUE))
check("zh aria keeps F-Score", grepl("F-Score", ui_str("funnel_kpi_jump_fscore_aria", "zh-TW"), fixed = TRUE))
check("zh aria QoE → verdict", grepl("財報結論", ui_str("funnel_kpi_jump_qoe_aria", "zh-TW"), fixed = TRUE))
check("en aria QoE → verdict", grepl("verdict", ui_str("funnel_kpi_jump_qoe_aria", "en"), ignore.case = TRUE))
check("alerts aria mentions M-Score", grepl("M-Score", ui_str("funnel_kpi_jump_alerts_aria", "en"), fixed = TRUE))
check("fraud KPI label mentions M-Score", grepl("M-Score", ui_str("funnel_vbox_fraud", "en"), fixed = TRUE))
check("verdict lead no MOS", !grepl("MOS", ui_str("funnel_verdict_lead", "en"), fixed = TRUE) ||
        grepl("no MOS", ui_str("funnel_verdict_lead", "en"), fixed = TRUE))
check("verdict lead zh excludes MOS from inputs", grepl("不含 MOS", ui_str("funnel_verdict_lead", "zh-TW"), fixed = TRUE))

dec <- paste(readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui_start <- regexpr("decision_ui <- function", dec, fixed = TRUE)[1]
ui_end <- regexpr("decision_momentum_panel_ui", dec, fixed = TRUE)[1]
check("decision_ui slice", is.finite(ui_start) && ui_start > 0 && ui_end > ui_start)
ui_fn <- if (ui_start > 0 && ui_end > ui_start) substr(dec, ui_start, ui_end) else dec

pos_v <- regexpr('`data-ynow-block` = "verdict"', ui_fn, fixed = TRUE)[1]
pos_r <- regexpr('`data-ynow-block` = "risk_matrix"', ui_fn, fixed = TRUE)[1]
pos_q <- regexpr('`data-ynow-block` = "quality"', ui_fn, fixed = TRUE)[1]
pos_a <- regexpr('`data-ynow-block` = "alerts"', ui_fn, fixed = TRUE)[1]
pos_b <- regexpr('`data-ynow-block` = "bubble"', ui_fn, fixed = TRUE)[1]
check("verdict block present", is.finite(pos_v) && pos_v > 0)
check("risk matrix block present", is.finite(pos_r) && pos_r > 0)
check("quality block present", is.finite(pos_q) && pos_q > 0)
check("order verdict → risk → 體質 → 警訊",
      pos_v > 0 && pos_r > pos_v && pos_q > pos_r && pos_a > pos_q)
check("bubble not on YNOW", !is.finite(pos_b) || pos_b < 1)

pos_ch1_id <- regexpr('id = "ynow_funnel_ch1"', ui_fn, fixed = TRUE)[1]
pos_ch1_title <- regexpr("ynow_funnel_ch1_title", ui_fn, fixed = TRUE)[1]
pos_qoe_kpi <- regexpr("vbox_qoe", ui_fn, fixed = TRUE)[1]
pos_fs <- regexpr("vbox_fscore", ui_fn, fixed = TRUE)[1]
pos_fraud <- regexpr("vbox_fraud", ui_fn, fixed = TRUE)[1]
pos_tbl <- regexpr("fscore_panel", ui_fn, fixed = TRUE)[1]
pos_shen <- regexpr("shenanigans_panel", ui_fn, fixed = TRUE)[1]
pos_qoe <- regexpr("qoe_dashboard", ui_fn, fixed = TRUE)[1]
pos_risk <- regexpr("risk_matrix_panel", ui_fn, fixed = TRUE)[1]
pos_mscore <- regexpr("mscore_widget", ui_fn, fixed = TRUE)[1]
pos_rec <- regexpr("ui_recommendation", ui_fn, fixed = TRUE)[1]
check("QoE KPI before F-Score", pos_qoe_kpi > 0 && pos_fs > pos_qoe_kpi)
check("F-Score card before alerts", pos_fs > 0 && pos_fraud > pos_fs)
check("KPI row before verdict", pos_fraud > 0 && pos_v > pos_fraud)
check("recommendation in verdict block", pos_rec > pos_v && pos_rec < pos_r)
check("risk panel in risk block", pos_risk > pos_r && pos_risk < pos_q)
check("QoE dashboard before F-Score panel", pos_qoe > pos_q && pos_qoe < pos_tbl)
check("F-Score boxes still in Section I", pos_tbl > pos_q && pos_tbl < pos_a)
check("M-Score in Section II (alerts)", pos_mscore > pos_a && pos_mscore < pos_shen)
check("shenanigans still in Section II", pos_shen > pos_a)
check("verdict notes after recommendation", {
  pos_v_lead <- regexpr("ynow_funnel_verdict_lead", ui_fn, fixed = TRUE)[1]
  pos_v_lead > pos_rec && pos_v_lead < pos_r
})
check("risk notes after risk panel", {
  pos_r_lead <- regexpr("ynow_funnel_risk_lead", ui_fn, fixed = TRUE)[1]
  pos_r_lead > pos_risk && pos_r_lead < pos_q
})
check("ch1 notes after F-Score panel", {
  pos_ch1_lead <- regexpr("ynow_funnel_ch1_lead", ui_fn, fixed = TRUE)[1]
  pos_ch1_lead > pos_tbl && pos_ch1_lead < pos_a
})
check("ch2 notes after shenanigans", {
  pos_ch2_lead <- regexpr("ynow_funnel_ch2_lead", ui_fn, fixed = TRUE)[1]
  pos_ch2_lead > pos_shen
})

ch1_body <- if (pos_q > 0 && pos_a > pos_q) substr(ui_fn, pos_q, pos_a) else ""
ch2_body <- if (pos_a > 0) substr(ui_fn, pos_a, nchar(ui_fn)) else ""
risk_body <- if (pos_r > 0 && pos_q > pos_r) substr(ui_fn, pos_r, pos_q) else ""
check("no vbox_qoe inside ch1", !grepl("vbox_qoe", ch1_body, fixed = TRUE))
check("no vbox_mos on YNOW page", !grepl("vbox_mos", ui_fn, fixed = TRUE))
check("no vbox_fscore inside ch1", !grepl("vbox_fscore", ch1_body, fixed = TRUE))
check("no vbox_fraud inside ch1", !grepl("vbox_fraud", ch1_body, fixed = TRUE))
check("no vbox_fraud inside ch2", !grepl("vbox_fraud", ch2_body, fixed = TRUE))
check("no vbox_fscore inside ch2", !grepl("vbox_fscore", ch2_body, fixed = TRUE))
check("no mscore in ch1", !grepl("mscore_widget", ch1_body, fixed = TRUE))
check("no risk matrix in ch2", !grepl("risk_matrix_panel", ch2_body, fixed = TRUE))
check("risk matrix not in ch1", !grepl("risk_matrix_panel", ch1_body, fixed = TRUE))
check("risk body has matrix", grepl("risk_matrix_panel", risk_body, fixed = TRUE))
check("href QoE → verdict", grepl('href = "#ynow_funnel_verdict"', ui_fn, fixed = TRUE))
check("href F-Score → fscore", grepl('href = "#ynow_funnel_fscore"', ui_fn, fixed = TRUE))
check("href alerts → ch2", grepl('href = "#ynow_funnel_ch2"', ui_fn, fixed = TRUE))
check("no QoE href to ch1", !grepl('ynow_kpi_jump_qoe"[\\s\\S]*?#ynow_funnel_ch1', ui_fn, perl = TRUE))
check("jump id QoE", grepl("ynow_kpi_jump_qoe", ui_fn, fixed = TRUE))
check("jump id F-Score", grepl("ynow_kpi_jump_fscore", ui_fn, fixed = TRUE))
check("jump id alerts", grepl("ynow_kpi_jump_alerts", ui_fn, fixed = TRUE))
check("verdict uses statement helper", grepl("ynow_statement_verdict", dec, fixed = TRUE))
check("verdict ignores mos_calc", !grepl("mos_calc\\(\\)", {
  rec_start <- regexpr("final_recommendation <- reactive", dec, fixed = TRUE)[1]
  next_start <- regexpr("\\.alert_tk <- function", dec, fixed = TRUE)[1]
  if (is.finite(rec_start) && rec_start > 0 && is.finite(next_start) && next_start > rec_start) {
    substr(dec, rec_start, next_start - 1L)
  } else {
    ""
  }
}))
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
check(
  "YNOW sidebar uses clock icon",
  grepl('menuItem("YNOW", tabName = "sensitivity", icon = icon("clock"))', ui, fixed = TRUE)
)
check(
  "atx menu first-paint name",
  grepl('ynow_menu_asset_transmission", "Asset Transmission Root"', ui, fixed = TRUE)
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
check("applyUiLocale kpi jump aria", grepl("ynow_kpi_jump_qoe", ui, fixed = TRUE) &&
        grepl("funnel_kpi_jump_qoe_aria", ui, fixed = TRUE))
check("click handler scrollIntoView", grepl("ynowOnFunnelKpiJump", ui, fixed = TRUE) &&
        grepl("ynowScrollFunnelAnchor", ui, fixed = TRUE) &&
        grepl("scrollIntoView", ui, fixed = TRUE))
check("Lite About YNOW keeps QoE and 財報警訊", grepl("盈餘品質、F-Score 品質檢核、風險矩陣與財報警訊", ui, fixed = TRUE))
check("Lite About EN uses Quality of Earnings", grepl("Quality of Earnings, F-Score screen, risk matrix, and statement alerts", ui, fixed = TRUE))
check("first-paint F-Score list title zh", grepl("F-Score 品質檢核清單", dec, fixed = TRUE))
check("KPI valueBox numbers stay one line", {
  grepl("small-box .inner h3", ui, fixed = TRUE) &&
    grepl("white-space: nowrap !important", ui, fixed = TRUE) &&
    grepl("never orphan a trailing digit", ui, fixed = TRUE)
})
check("KPI valueBox equal row height", {
  grepl("row:has(> [class*='col-'] .small-box)", ui, fixed = TRUE) &&
    grepl("align-items: stretch", ui, fixed = TRUE)
})
check("stock KPI grid centers value text", {
  grepl(".ynow-kpi-grid .small-box .inner", ui, fixed = TRUE) &&
    grepl("padding-right: 9px !important", ui, fixed = TRUE) &&
    grepl("justify-content: center !important", ui, fixed = TRUE) &&
    grepl("align-items: center !important", ui, fixed = TRUE) &&
    grepl("text-align: center !important", ui, fixed = TRUE)
})

macro <- paste(readLines("macro_market_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("bubble at Macro bottom", {
  pos_own <- regexpr("ynow_own_index", macro, fixed = TRUE)[1]
  pos_bub <- regexpr("macro_bubble_chapter_ui", macro, fixed = TRUE)[1]
  is.finite(pos_own) && pos_own > 0 && is.finite(pos_bub) && pos_bub > pos_own
})
check("split industry picker", grepl('ns("industry_key")', macro, fixed = TRUE))
check("concept word cloud multi-select", {
  grepl("concept_keys", macro, fixed = TRUE) &&
    grepl("ynow-macro-concept-cloud", macro, fixed = TRUE) &&
    !grepl('ns("concept_key")', macro, fixed = TRUE)
})
check("Macro first-paint bubble title", grepl("Dynamic industry bubble", macro, fixed = TRUE))

# Composite valuation: model notes sit under chart footer note (same block)
cmp_start <- regexpr("output\\$ui_valuation_compare", dec)[1]
cmp_src <- if (is.finite(cmp_start) && cmp_start > 0) substr(dec, cmp_start, nchar(dec)) else ""
pos_footer <- regexpr("ynow-composite-footer-note", cmp_src, fixed = TRUE)[1]
pos_model_notes <- regexpr("ynow-composite-model-notes", cmp_src, fixed = TRUE)[1]
pos_chart_notes <- regexpr("ynow-composite-chart-notes", cmp_src, fixed = TRUE)[1]
pos_axis <- regexpr("ynow-composite-model-mark", cmp_src, fixed = TRUE)[1]
check("composite chart-notes wrapper", is.finite(pos_chart_notes) && pos_chart_notes > 0)
check("composite footer before model notes", {
  is.finite(pos_footer) && pos_footer > 0 &&
    is.finite(pos_model_notes) && pos_model_notes > pos_footer
})
check("composite axis before chart notes", {
  is.finite(pos_axis) && pos_axis > 0 &&
    is.finite(pos_chart_notes) && pos_chart_notes > pos_axis
})
check("composite model notes not above Bear cards", {
  # Bear band card is built before chart-notes HTML (notes no longer lead the card)
  pos_bear <- regexpr('.band_card("Bear"', cmp_src, fixed = TRUE)[1]
  is.finite(pos_bear) && pos_bear > 0 && pos_bear < pos_chart_notes
})

if (fail > 0L) {
  stop(sprintf("%d YNOW layout check(s) failed", fail), call. = FALSE)
}
cat("PASS ynow_tab_layout\n")
