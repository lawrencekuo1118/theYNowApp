# Lite mode: UI chrome keys, locale strings, and Smart Analysis wiring
# (no new scrapers — reuses recommend_valuation_models / auto_calc)

suppressPackageStartupMessages({
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("testthat required")
  }
})

testthat::local_edition(3)

testthat::test_that("Lite locale keys exist in en and zh-TW", {
  source(file.path("..", "ui_locale.R"), local = TRUE)
  keys <- c(
    "menu_smart_analysis", "smart_page_title", "smart_page_sub",
    "smart_chart_title", "smart_primary_kicker", "smart_secondary_kicker",
    "smart_price_kicker", "smart_mos_kicker", "smart_waiting",
    "smart_calc_pending", "smart_reason_title",
    "smart_scenario_title", "smart_scenario_two_stage", "smart_scenario_gordon",
    "smart_scenario_sgr", "smart_scenario_claim",
    "ddm_formula_gordon", "ddm_d0_label", "ddm_r_ke_label",
    "lab_im_eq_label", "lab_im_eq_hint",
    "lab_im_eq_explain_title", "lab_im_eq_explain_body",
    "lab_im_include_adr_label", "lab_im_include_adr_hint",
    "lite_toggle_title", "lite_toggle_aria",
    "snapshot_page_help_lite", "snapshot_defaults_help_lite",
    "test_link", "testing_page_title", "testing_page_sub",
    "testing_box_title", "testing_box_body",
    "hfv_chart_overlay_aria", "hfv_chart_models_label",
    "lab_im_gate_label", "funnel_ch1_title", "dc_label_fscore",
    "notes_title", "notes_toggle_aria",
    "macro_index_name_gspc", "macro_index_name_ixic", "macro_index_name_dji",
    "macro_index_name_sox", "macro_index_chart_hint", "macro_index_chart_empty",
    "macro_index_chart_error"
  )
  for (k in keys) {
    en <- .UI_STRINGS$en[[k]]
    zh <- .UI_STRINGS$`zh-TW`[[k]]
    testthat::expect_true(is.character(en) && nzchar(en[1]), info = paste("en", k))
    testthat::expect_true(is.character(zh) && nzchar(zh[1]), info = paste("zh-TW", k))
  }
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$menu_smart_analysis, "智慧分析")
  testthat::expect_identical(.UI_STRINGS$en$menu_smart_analysis, "Smart Analysis")
  testthat::expect_true(grepl("參數情境", .UI_STRINGS$`zh-TW`$smart_page_sub, fixed = TRUE))
  testthat::expect_true(grepl("parameter scenario", .UI_STRINGS$en$smart_page_sub, fixed = TRUE))
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$smart_scenario_title, "已套用參數情境：")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$lab_im_eq_label, "盈餘品質")
  testthat::expect_identical(.UI_STRINGS$en$lab_im_eq_label, "Earnings quality")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$lab_im_eq_explain_title, "盈餘品質：")
  testthat::expect_true(grepl("OCF", .UI_STRINGS$`zh-TW`$lab_im_eq_explain_body, fixed = TRUE))
})

testthat::test_that("ynow_ui wires Lite toggle, Smart Analysis tab, and CSS hooks", {
  ui_path <- file.path("..", "ynow_ui.R")
  txt <- paste(readLines(ui_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("ynow_lite_toggle", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-lite-badge", txt, fixed = TRUE))
  testthat::expect_true(grepl("body.ynow-lite", txt, fixed = TRUE))
  testthat::expect_true(grepl('tabName = "macro_market"', txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_menu_macro", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-macro-report", txt, fixed = TRUE))
  # Theme Rolling β vs benchmark is removed from Macro (Full and Lite)
  testthat::expect_false(grepl("ynow_macro_beta_title", txt, fixed = TRUE))
  testthat::expect_false(grepl("macro_beta_title", txt, fixed = TRUE))
  testthat::expect_false(grepl("macro_beta_warn_title", txt, fixed = TRUE))
  macro_path <- file.path("..", "macro_market_module.R")
  macro_txt <- paste(readLines(macro_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_false(grepl("Theme Rolling β vs benchmark", macro_txt, fixed = TRUE))
  testthat::expect_false(grepl("ynow_macro_beta_title", macro_txt, fixed = TRUE))
  testthat::expect_false(grepl("output$beta_plot", macro_txt, fixed = TRUE))
  testthat::expect_false(grepl("output$beta_kpi", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("macro_rolling_beta_path", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl('ns("industry_key")', macro_txt, fixed = TRUE))
  testthat::expect_true(grepl('ns("concept_key")', macro_txt, fixed = TRUE))
  testthat::expect_false(grepl('ns("theme_key")', macro_txt, fixed = TRUE))
  testthat::expect_false(grepl("Industry / concept vs benchmark", macro_txt, fixed = TRUE))
  testthat::expect_false(grepl("ynow-lite-only", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_macro_idx_gspc", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_macro_idx_ixic", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_macro_idx_dji", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_macro_idx_sox", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_macro_index_hist", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("index_hist_plot", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-full-only", macro_txt, fixed = TRUE))
  testthat::expect_true(
    regexpr("ynow_macro_index_hist", macro_txt, fixed = TRUE)[1] <
      regexpr("rf_signal_row", macro_txt, fixed = TRUE)[1]
  )
  testthat::expect_true(grepl("macro_click_index_specs", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("lite_mode_rv", macro_txt, fixed = TRUE))
  testthat::expect_true(grepl("body.ynow-lite #ynow_macro_index_hist", txt, fixed = TRUE))
  testthat::expect_true(grepl("body.ynow-lite .ynow-macro-kpi--clickable", txt, fixed = TRUE))
  testthat::expect_true(grepl("body.ynow-lite #ynow_macro_index_hint", txt, fixed = TRUE))
  # Macro tab must appear before Dashboard in sidebar markup
  testthat::expect_true(
    regexpr('tabName = "macro_market"', txt, fixed = TRUE)[1] <
      regexpr('tabName = "dashboard"', txt, fixed = TRUE)[1]
  )
  testthat::expect_true(grepl('tabName = "smart_analysis"', txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-full-only", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_lab_im_detail_tab", txt, fixed = TRUE))
  testthat::expect_true(grepl("The YNow App v19.10", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-hfv-report", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-hfv-toolbar", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-hfv-chapter", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_hfv_ch1_title", txt, fixed = TRUE))
  # Chart overlay models sit in Section I body immediately above the timeline chart
  testthat::expect_true(grepl("ynow_hfv_chart_overlay_controls", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-hfv-toolbar--above-chart", txt, fixed = TRUE))
  pos_overlay <- regexpr("ynow_hfv_chart_overlay_controls", txt, fixed = TRUE)[1]
  pos_chart <- regexpr('plotlyOutput\\(\"bt_hfv_timeline\"', txt, perl = TRUE)[1]
  testthat::expect_true(is.finite(pos_overlay) && pos_overlay > 0)
  testthat::expect_true(is.finite(pos_chart) && pos_chart > 0)
  testthat::expect_true(pos_overlay < pos_chart)
  # Overlay controls are no longer in the main report toolbar block after Section I
  toolbar_block <- regmatches(
    txt,
    regexpr(
      'id = \"ynow_hfv_toolbar\"[\\s\\S]*?id = \"ynow_hfv_ch2_kicker\"',
      txt,
      perl = TRUE
    )
  )
  testthat::expect_true(length(toolbar_block) == 1L && nzchar(toolbar_block))
  testthat::expect_false(grepl("bt_fv_models", toolbar_block, fixed = TRUE))
  testthat::expect_true(grepl("ynow-backtest-report", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-backtest-toolbar", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-backtest-zone-band", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-backtest-chapter", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_bt_ch1_title", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_bt_nav_controls", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-backtest-toolbar--above-chart", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_bt_ch2_title", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_menu_backtest", txt, fixed = TRUE))
  testthat::expect_true(grepl('tabName = "lab_notes"', txt, fixed = TRUE))
  testthat::expect_true(grepl("about_lite_intro_ui", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-lite-only", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-sidebar-test-link", txt, fixed = TRUE))
  testthat::expect_true(grepl('tabName = "testing"', txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_sidebar_test_btn", txt, fixed = TRUE))
  testthat::expect_true(grepl("ensureLiteSnapshotDefaultsTab", txt, fixed = TRUE))
  testthat::expect_true(grepl('value = "snap_defaults"', txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_snapshot_page_help_lite", txt, fixed = TRUE))
  # Lite must hide Quant Backtest Lab sidebar and Testing foot link (Full-only)
  testthat::expect_true(grepl(
    "body\\.ynow-lite[\\s\\S]*data-value=\"lab_notes\"",
    txt,
    perl = TRUE
  ))
  testthat::expect_true(grepl(
    "body\\.ynow-lite[\\s\\S]*ynow-sidebar-test-link",
    txt,
    perl = TRUE
  ))

  testthat::expect_true(grepl(
    "body\\.ynow-lite[\\s\\S]*snap_audit",
    txt,
    perl = TRUE
  ))
  # Header quote KPIs are Dashboard-only (not Smart Analysis)
  testthat::expect_true(grepl(
    "Header KPIs: Dashboard only",
    txt,
    fixed = TRUE
  ))
  testthat::expect_true(grepl(
    'condition = "input.sidebar_tabs == \'dashboard\'"',
    txt,
    fixed = TRUE
  ))
  testthat::expect_false(grepl(
    "input.sidebar_tabs == 'dashboard' || input.sidebar_tabs == 'smart_analysis'",
    txt,
    fixed = TRUE
  ))
  # Lite About must not mount methodology outside ynow-full-only
  testthat::expect_true(grepl(
    "ynow-full-only[\\s\\S]*valuation_methodology_section_ui",
    txt,
    perl = TRUE
  ))
  testthat::expect_true(grepl("ynow-lab-im-eq-adr-row", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_lab_im_eq_explain", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-lab-im-eq-explain ynow-lite-only", txt, fixed = TRUE))
})

testthat::test_that("Lite Smart Analysis blurb sits above composite status without a page heading", {
  ui_path <- file.path("..", "ynow_ui.R")
  txt <- paste(readLines(ui_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  pos_sub <- regexpr('id = "ynow_smart_page_sub"', txt, fixed = TRUE)[1]
  pos_comp <- regexpr('class = "ynow-header-composite-row"', txt, fixed = TRUE)[1]
  pos_compare <- regexpr('decision_valuation_compare_ui("main_decision")', txt, fixed = TRUE)[1]
  testthat::expect_true(is.finite(pos_sub) && pos_sub > 0)
  testthat::expect_true(is.finite(pos_comp) && pos_comp > pos_sub)
  testthat::expect_true(is.finite(pos_compare) && pos_compare > pos_comp)
  # Blurb is the last Lite-only node immediately before the composite status box
  between <- substr(txt, pos_sub, pos_comp)
  testthat::expect_false(grepl("smart_analysis_summary", between, fixed = TRUE))
  testthat::expect_false(grepl("smart_analysis_chart", between, fixed = TRUE))
  testthat::expect_false(grepl("ynow-header-years-suggest-row", between, fixed = TRUE))
  wrap <- substr(txt, max(1L, pos_sub - 500L), pos_comp)
  testthat::expect_true(grepl("ynow-lite-only", wrap, fixed = TRUE))
  testthat::expect_true(grepl("ynow-smart-lite-blurb", wrap, fixed = TRUE))
  testthat::expect_true(grepl(
    'condition = "input.sidebar_tabs == \'smart_analysis\'"',
    wrap,
    fixed = TRUE
  ))

  # Heading element removed; locale key / JS lookup may remain
  testthat::expect_false(grepl(
    'id = "ynow_smart_page_title", "Smart Analysis"',
    txt,
    fixed = TRUE
  ))
  testthat::expect_false(grepl(
    "h2(tags$b(id = \"ynow_smart_page_title\"",
    txt,
    fixed = TRUE
  ))

  smart_tab <- regmatches(
    txt,
    regexpr(
      'tabItem\\(\\s*tabName = "smart_analysis",[\\s\\S]*?tabItem\\(tabName = "ddm_calculator"',
      txt,
      perl = TRUE
    )
  )
  testthat::expect_true(length(smart_tab) == 1L && nzchar(smart_tab))
  testthat::expect_false(grepl("ynow_smart_page_title", smart_tab, fixed = TRUE))
  testthat::expect_false(grepl("ynow_smart_page_sub", smart_tab, fixed = TRUE))
  testthat::expect_false(grepl("h2(", smart_tab, fixed = TRUE))

  # Full model pages still share the same composite header; blurb is Lite / Smart Analysis only
  testthat::expect_true(grepl(
    "input.sidebar_tabs == 'dcf_calculator' ||",
    txt,
    fixed = TRUE
  ))
  testthat::expect_true(grepl(
    "input.sidebar_tabs == 'nav_calculator'",
    txt,
    fixed = TRUE
  ))
  years_cond <- regmatches(
    txt,
    regexpr(
      "# Lite Smart Analysis：不顯示手動模型參數[\\s\\S]*?ynow-header-years-suggest-row",
      txt,
      perl = TRUE
    )
  )
  testthat::expect_true(length(years_cond) == 1L && nzchar(years_cond))
  testthat::expect_false(grepl("smart_analysis", years_cond, fixed = TRUE))
})

testthat::test_that("ynow_server wires Lite scenario apply before auto-calc", {
  srv_path <- file.path("..", "ynow_server.R")
  txt <- paste(readLines(srv_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl(".apply_lite_recommended_scenario", txt, fixed = TRUE))
  testthat::expect_true(grepl(".lite_desired_scenario", txt, fixed = TRUE))
  testthat::expect_true(grepl(".clamp_g_below_rate", txt, fixed = TRUE))
  testthat::expect_true(grepl(".lite_resync_discount_consistency", txt, fixed = TRUE))
  testthat::expect_true(grepl("lite_dcf_silent", txt, fixed = TRUE))
  testthat::expect_true(grepl(".heal_g_vs_rate", txt, fixed = TRUE))
  testthat::expect_true(grepl(".auto_calc_shares_ready", txt, fixed = TRUE))
  testthat::expect_true(grepl("per_share_bridge_ready", txt, fixed = TRUE))
  testthat::expect_true(grepl("infer_statement_currency", txt, fixed = TRUE))
  testthat::expect_true(grepl("classify_per_share_alignment_failure", txt, fixed = TRUE))
  testthat::expect_true(grepl(".execute_dcf_calc(notify = FALSE)", txt, fixed = TRUE))
  testthat::expect_true(grepl("notif_dcf_neg_equity", txt, fixed = TRUE))
  testthat::expect_true(grepl("lite_scenario_applied_sig", txt, fixed = TRUE))
  testthat::expect_true(grepl("calculated_wacc()", txt, fixed = TRUE))
  testthat::expect_true(grepl("Do not block on .lite_scenario_matches_ui", txt, fixed = TRUE))
  testthat::expect_true(grepl("Lite Smart Analysis must not hang", txt, fixed = TRUE))
})

testthat::test_that("ynow_server wires Lite auto-calc and Smart Analysis outputs", {
  srv_path <- file.path("..", "ynow_server.R")
  txt <- paste(readLines(srv_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("lite_mode <- reactive", txt, fixed = TRUE))
  testthat::expect_true(grepl("lite_default_keys", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_lite_mode", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_summary", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_chart", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_reason", txt, fixed = TRUE))
  # Lite defaults allowlist = UI-configurable only (not Smart Analysis engines)
  testthat::expect_true(grepl('"lab_im_eq_only"', txt, fixed = TRUE))
  testthat::expect_true(grepl('"lab_im_pool_rank"', txt, fixed = TRUE))
  testthat::expect_true(grepl('"lab_cluster_k"', txt, fixed = TRUE))
  # Extract lite_default_keys vector body and assert engines are excluded
  m <- regmatches(txt, regexpr(
    "lite_default_keys <- c\\([\\s\\S]*?\\)\\n\\s*# Lite has no module extras",
    txt,
    perl = TRUE
  ))
  testthat::expect_true(length(m) == 1L && nzchar(m))
  testthat::expect_false(grepl('"years"', m, fixed = TRUE))
  testthat::expect_false(grepl('"dcf_mode"', m, fixed = TRUE))
  testthat::expect_false(grepl('"wacc_gordon"', m, fixed = TRUE))
  testthat::expect_false(grepl('"apply_capex_spike_smooth"', m, fixed = TRUE))
})

testthat::test_that("Lite defaults help copy mentions configurable-only filter", {
  source(file.path("..", "ui_locale.R"), local = TRUE)
  testthat::expect_true(grepl(
    "parameters Lite can set",
    .UI_STRINGS$en$snapshot_defaults_help_lite,
    fixed = TRUE
  ))
  testthat::expect_true(grepl(
    "簡化版介面可設定",
    .UI_STRINGS$`zh-TW`$snapshot_defaults_help_lite,
    fixed = TRUE
  ))
  testthat::expect_false(grepl(
    "Smart Analysis engines, Dashboard",
    .UI_STRINGS$en$snapshot_defaults_help_lite,
    fixed = TRUE
  ))
})

testthat::test_that("clamp_g_below_rate keeps g strictly below discount", {
  # Mirror of server helper (pure math) for regression without Shiny session
  clamp_g_below_rate <- function(g_pct, rate_pct, margin = 0.5) {
    g_pct <- suppressWarnings(as.numeric(g_pct)[1])
    rate_pct <- suppressWarnings(as.numeric(rate_pct)[1])
    if (!is.finite(g_pct)) return(NA_real_)
    if (!is.finite(rate_pct) || rate_pct <= 0) return(g_pct)
    if (g_pct < rate_pct - 1e-6) return(g_pct)
    max(rate_pct - margin, 0)
  }
  testthat::expect_equal(clamp_g_below_rate(3, 8), 3)
  testthat::expect_equal(clamp_g_below_rate(8, 8), 7.5)
  testthat::expect_equal(clamp_g_below_rate(12, 9), 8.5)
  testthat::expect_true(clamp_g_below_rate(10, 8) < 8)
})

testthat::test_that("YNOW page title and three-block order are shared by Lite and Full", {
  source(file.path("..", "ui_locale.R"), local = TRUE)
  testthat::expect_identical(.UI_STRINGS$en$funnel_page_title, "YNOW")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$funnel_page_title, "YNOW")
  testthat::expect_identical(.UI_STRINGS$en$funnel_ch1_title, "Statement quality")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$funnel_ch1_title, "財報體質")
  testthat::expect_identical(.UI_STRINGS$en$funnel_ch2_title, "Statement alerts")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$funnel_ch2_title, "財報警訊")
  testthat::expect_identical(
    .UI_STRINGS$en$funnel_ch3_title,
    "Dynamic industry bubble & weight concentration"
  )
  testthat::expect_identical(
    .UI_STRINGS$`zh-TW`$funnel_ch3_title,
    "動態產業泡沫與權重集中度"
  )
  testthat::expect_false(grepl("Decision Funnel", .UI_STRINGS$en$funnel_page_title, fixed = TRUE))
  testthat::expect_false(grepl("決策漏斗", .UI_STRINGS$`zh-TW`$funnel_page_title, fixed = TRUE))

  dec <- paste(
    readLines(file.path("..", "investment_decision_module.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  ui_start <- regexpr("decision_ui <- function", dec, fixed = TRUE)[1]
  ui_end <- regexpr("decision_momentum_panel_ui", dec, fixed = TRUE)[1]
  testthat::expect_true(is.finite(ui_start) && ui_start > 0 && ui_end > ui_start)
  ui_fn <- substr(dec, ui_start, ui_end)
  pos_q <- regexpr('`data-ynow-block` = "quality"', ui_fn, fixed = TRUE)[1]
  pos_a <- regexpr('`data-ynow-block` = "alerts"', ui_fn, fixed = TRUE)[1]
  pos_b <- regexpr('`data-ynow-block` = "bubble"', ui_fn, fixed = TRUE)[1]
  testthat::expect_true(is.finite(pos_q) && pos_q > 0)
  testthat::expect_true(is.finite(pos_a) && pos_a > pos_q)
  testthat::expect_true(is.finite(pos_b) && pos_b > pos_a)

  pos_mos <- regexpr("vbox_mos", ui_fn, fixed = TRUE)[1]
  pos_fs <- regexpr("vbox_fscore", ui_fn, fixed = TRUE)[1]
  pos_fraud <- regexpr("vbox_fraud", ui_fn, fixed = TRUE)[1]
  pos_ch1_title <- regexpr("ynow_funnel_ch1_title", ui_fn, fixed = TRUE)[1]
  pos_tbl <- regexpr("table_checklist", ui_fn, fixed = TRUE)[1]
  pos_shen <- regexpr("shenanigans_panel", ui_fn, fixed = TRUE)[1]
  testthat::expect_true(pos_mos > 0 && pos_fs > pos_mos && pos_fraud > pos_fs)
  testthat::expect_true(pos_ch1_title > pos_fraud)
  testthat::expect_true(pos_tbl > pos_q && pos_tbl < pos_a)
  testthat::expect_true(pos_shen > pos_a && pos_shen < pos_b)
  ch1_body <- substr(ui_fn, pos_q, pos_a)
  ch2_body <- substr(ui_fn, pos_a, pos_b)
  testthat::expect_false(grepl("vbox_fscore", ch1_body, fixed = TRUE))
  testthat::expect_false(grepl("vbox_mos", ch1_body, fixed = TRUE))
  testthat::expect_false(grepl("vbox_fraud", ch2_body, fixed = TRUE))
  testthat::expect_true(grepl('href = "#ynow_funnel_ch1"', ui_fn, fixed = TRUE))
  testthat::expect_true(grepl('href = "#ynow_funnel_ch2"', ui_fn, fixed = TRUE))
  testthat::expect_true(grepl("ynow-funnel-kpi-jump-row", ui_fn, fixed = TRUE))
  testthat::expect_identical(.UI_STRINGS$en$notes_title, "Notes")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$notes_title, "附註")
  testthat::expect_true(length(gregexpr("ynow_notes_block(", ui_fn, fixed = TRUE)[[1]]) >= 4L)
  testthat::expect_false(grepl("ynow_notes_block\\([^)]*open\\s*=\\s*TRUE", ui_fn))

  testthat::expect_true(grepl("macro_bubble_chapter_ui", dec, fixed = TRUE))
  testthat::expect_false(grepl("ynow-lite-only", dec, fixed = TRUE))
  testthat::expect_false(grepl("ynow-full-only", dec, fixed = TRUE))

  ui <- paste(
    readLines(file.path("..", "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  testthat::expect_true(grepl('tabName = "sensitivity"', ui, fixed = TRUE))
  testthat::expect_true(grepl('decision_ui("main_decision")', ui, fixed = TRUE))
  testthat::expect_true(grepl("財報體質（F-Score）、財報警訊、動態產業泡沫與權重集中度", ui, fixed = TRUE))
  testthat::expect_true(grepl("statement quality (F-Score), statement alerts", ui, fixed = TRUE))
  testthat::expect_false(grepl("三區塊版面：品質檢核（F-Score）", ui, fixed = TRUE))

  macro <- paste(
    readLines(file.path("..", "macro_market_module.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  testthat::expect_false(grepl("macro_bubble_chapter_ui", macro, fixed = TRUE))
})
