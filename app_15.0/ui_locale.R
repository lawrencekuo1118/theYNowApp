# ==========================================
# ui_locale.R — 主介面文案（en ｜ zh-TW）
# 台股模式預設 zh-TW；美股模式恢復 en。
# 財務專有名詞維持英文（WACC、FCFF、DCF…）。
# ==========================================

normalize_ui_locale <- function(locale) {
  loc <- tolower(trimws(as.character(locale %||% "en")[1]))
  loc <- gsub("_", "-", loc)
  if (loc %in% c("zh", "zh-tw", "zhtw", "tw", "taiwan")) return("zh-TW")
  "en"
}

#' 市場模式 → 預設 UI locale
locale_for_market <- function(mode = get_market_mode()) {
  if (identical(normalize_market_mode(mode), "TW")) "zh-TW" else "en"
}

.UI_STRINGS <- list(
  en = list(
    recent_search = "Recent Search:",
    menu_dashboard = "Dashboard",
    menu_get_started = "Get Started",
    menu_dcf = "DCF-Model",
    menu_ddm = "DDM",
    menu_pb = "P/B-Asset",
    menu_ri = "RI-Model",
    menu_ynow = "YNOW",
    menu_bluechip = "Blue Chip",
    menu_backtest = "Backtest Zone",
    menu_about = "About",
    ticker_label = "Ticker / Stock Code",
    data_source_title = "Data Source:",
    data_source_body = paste0(
      "This application integrates real-time financial data via web parsing ",
      "and API resources, applying comprehensive models for valuation."
    ),
    download_report = "Download full analysis report (PDF)",
    snapshot_link = " Snapshot",
    test_link = " Testing",
    feedback_link = " Feedback",
    market_hint = "Market",
    industry_standard = "Industry Standard",
    btn_expand_all = "Expand All",
    btn_compress_all = "Compress (switch to summary)",
    # --- tabBox headers ---
    box_financial_report = "FINANCIAL REPORT",
    box_performance = "PERFORMANCE",
    box_dividend_discount = "DIVIDEND DISCOUNT",
    box_discounted_cf = "DISCOUNTED CASH FLOW",
    box_sensitivity = "SENSITIVITY",
    box_residual_income = "RESIDUAL INCOME",
    box_pb_asset = "P/B & ASSET VALUE",
    box_blue_chip = "BLUE CHIP",
    box_beta = "BETA",
    box_model_guide = "Model Selection Guide",
    box_bt_params = "Strategy Parameters",
    # --- nested / small tabs (keyed for JS; data-value match) ---
    tab_finance_summary = "Finance Summary",
    tab_income_statement = "Income Statement",
    tab_balance_sheet = "Balance Sheet",
    tab_cash_flow = "Cash Flow",
    tab_sec_notes = "SEC Notes",
    tab_kpi_by_sheet = "KPI by Sheet",
    tab_crossover_kpis = "Crossover KPIs",
    tab_annotation = "Annotation",
    tab_ddm_overview = "DDM Overview",
    tab_ddm_calc_details = "DDM Calculation Details",
    tab_overview = "Overview",
    tab_d0 = "D0",
    tab_ke = "Ke",
    tab_beta = "Beta (β)",
    tab_dcf_overview = "DCF Overview",
    tab_dcf_calc_details = "DCF Calculation Details",
    tab_wacc = "WACC",
    tab_ri_overview = "RI Overview",
    tab_ri_settings = "RI Settings",
    tab_sensitivity_analysis = "Sensitivity Analysis",
    tab_pb_overview = "P/B Overview",
    tab_pb_settings = "P/B Settings",
    tab_target_pb = "Target P/B",
    tab_beta_overview = "Beta Overview",
    tab_peer_unlever = "Peer Unlever",
    tab_rolling_beta = "Rolling β",
    tab_decision_matrix = "Decision Matrix",
    tab_about_ddm = "Dividend Discount Model (DDM)",
    tab_about_dcf = "Discounted Cash Flow (DCF)",
    tab_about_ri = "Residual Income (RI)",
    tab_about_pb = "Price-to-Book (P/B)",
    tab_im_filters = "Filters",
    tab_im_detail = "Detail",
    tab_bt_fundamental = "Fundamental Strategy",
    tab_bt_sentiment = "Sentiment Strategy"
  ),
  `zh-TW` = list(
    recent_search = "最近搜尋：",
    menu_dashboard = "總覽 Dashboard",
    menu_get_started = "開始設定",
    menu_dcf = "DCF 模型",
    menu_ddm = "DDM",
    menu_pb = "P/B 資產",
    menu_ri = "RI 模型",
    menu_ynow = "YNOW",
    menu_bluechip = "Blue Chip 績優",
    menu_backtest = "回測專區",
    menu_about = "關於",
    ticker_label = "Ticker／股票代號",
    data_source_title = "資料來源：",
    data_source_body = paste0(
      "本應用程式整合即時財務資料（網頁解析與 API），",
      "並套用完整估值模型。"
    ),
    download_report = "下載完整分析報告 (PDF)",
    snapshot_link = " 快照",
    test_link = " 測試",
    feedback_link = " 意見區",
    market_hint = "市場",
    industry_standard = "產業標準",
    btn_expand_all = "全部展開",
    btn_compress_all = "壓縮（切換回精簡版）",
    # --- tabBox headers ---
    box_financial_report = "財務報表",
    box_performance = "績效指標",
    box_dividend_discount = "股利折現 DDM",
    box_discounted_cf = "折現現金流 DCF",
    box_sensitivity = "敏感度分析",
    box_residual_income = "剩餘收益 RI",
    box_pb_asset = "P/B 與資產價值",
    box_blue_chip = "Blue Chip 績優",
    box_beta = "Beta β",
    box_model_guide = "模型選擇決策指南",
    box_bt_params = "策略參數設定",
    # --- nested / small tabs ---
    tab_finance_summary = "財務摘要",
    tab_income_statement = "損益表",
    tab_balance_sheet = "資產負債表",
    tab_cash_flow = "現金流量表",
    tab_sec_notes = "財報附註 (SEC)",
    tab_kpi_by_sheet = "分表 KPI",
    tab_crossover_kpis = "交叉 KPI",
    tab_annotation = "註解說明",
    tab_ddm_overview = "DDM 總覽",
    tab_ddm_calc_details = "DDM 計算明細",
    tab_overview = "總覽",
    tab_d0 = "D0",
    tab_ke = "Ke",
    tab_beta = "Beta (β)",
    tab_dcf_overview = "DCF 總覽",
    tab_dcf_calc_details = "DCF 計算明細",
    tab_wacc = "WACC",
    tab_ri_overview = "RI 總覽",
    tab_ri_settings = "RI 設定",
    tab_sensitivity_analysis = "敏感度分析",
    tab_pb_overview = "P/B 總覽",
    tab_pb_settings = "P/B 設定",
    tab_target_pb = "目標本淨比",
    tab_beta_overview = "Beta 總覽",
    tab_peer_unlever = "同業去槓桿",
    tab_rolling_beta = "Rolling β",
    tab_decision_matrix = "決策矩陣",
    tab_about_ddm = "股利折現模型 (DDM)",
    tab_about_dcf = "折現現金流模型 (DCF)",
    tab_about_ri = "剩餘收益模型 (RI)",
    tab_about_pb = "本淨比 (P/B)",
    tab_im_filters = "篩選條件",
    tab_im_detail = "明細",
    tab_bt_fundamental = "基本面策略",
    tab_bt_sentiment = "情緒策略"
  )
)

# data-value attribute (as rendered) → ui_str key
.UI_TAB_DATA_VALUE_KEYS <- c(
  "Finance Summary" = "tab_finance_summary",
  "Income Statement" = "tab_income_statement",
  "Balance Sheet" = "tab_balance_sheet",
  "Cash Flow" = "tab_cash_flow",
  "sec_notes" = "tab_sec_notes",
  "KPI by Sheet" = "tab_kpi_by_sheet",
  "Crossover KPIs" = "tab_crossover_kpis",
  "Annotation" = "tab_annotation",
  "DDM Overview" = "tab_ddm_overview",
  "DDM Calculation Details" = "tab_ddm_calc_details",
  "Overview" = "tab_overview",
  "D0" = "tab_d0",
  "Ke" = "tab_ke",
  "Beta (β)" = "tab_beta",
  "DCF Overview" = "tab_dcf_overview",
  "DCF Calculation Details" = "tab_dcf_calc_details",
  "WACC" = "tab_wacc",
  "RI Overview" = "tab_ri_overview",
  "RI Settings" = "tab_ri_settings",
  "Sensitivity Analysis" = "tab_sensitivity_analysis",
  "P/B Overview" = "tab_pb_overview",
  "P/B Settings" = "tab_pb_settings",
  "target_pb" = "tab_target_pb",
  "Beta Overview" = "tab_beta_overview",
  "peer_unlever" = "tab_peer_unlever",
  "Rolling β" = "tab_rolling_beta",
  "Decision Matrix" = "tab_decision_matrix",
  "Dividend Discount Model (DDM)" = "tab_about_ddm",
  "Discounted Cash Flow (DCF)" = "tab_about_dcf",
  "Residual Income (RI)" = "tab_about_ri",
  "Price-to-Book (P/B)" = "tab_about_pb",
  "im_filters" = "tab_im_filters",
  "im_detail" = "tab_im_detail",
  "bt_fundamental" = "tab_bt_fundamental",
  "bt_sentiment" = "tab_bt_sentiment",
  # legacy Chinese data-value (pre-stable-value tabs) — still match if present
  "同業去槓桿" = "tab_peer_unlever",
  "目標本淨比" = "tab_target_pb",
  "篩選條件" = "tab_im_filters",
  "明細" = "tab_im_detail",
  "財報附註 (SEC)" = "tab_sec_notes"
)

# English (canonical) header text → ui_str key; aliases include both locales for rematch
.UI_BOX_HEADER_KEYS <- c(
  "FINANCIAL REPORT" = "box_financial_report",
  "PERFORMANCE" = "box_performance",
  "DIVIDEND DISCOUNT" = "box_dividend_discount",
  "DISCOUNTED CASH FLOW" = "box_discounted_cf",
  "SENSITIVITY" = "box_sensitivity",
  "RESIDUAL INCOME" = "box_residual_income",
  "P/B & ASSET VALUE" = "box_pb_asset",
  "BLUE CHIP" = "box_blue_chip",
  "BETA" = "box_beta",
  "模型選擇決策指南" = "box_model_guide",
  "Model Selection Guide" = "box_model_guide",
  "策略參數設定" = "box_bt_params",
  "Strategy Parameters" = "box_bt_params"
)

#' 取單一 UI 字串
ui_str <- function(key, locale = "en") {
  loc <- normalize_ui_locale(locale)
  bucket <- .UI_STRINGS[[loc]]
  if (is.null(bucket)) bucket <- .UI_STRINGS$en
  val <- bucket[[key]]
  if (is.null(val) || !nzchar(as.character(val)[1])) {
    val <- .UI_STRINGS$en[[key]]
  }
  as.character(val %||% key)[1]
}

#' 回傳目前 locale 的完整字串 map（給 JS chrome 更新）
ui_locale_payload <- function(locale = "en") {
  loc <- normalize_ui_locale(locale)
  keys <- names(.UI_STRINGS$en)
  stats::setNames(lapply(keys, function(k) ui_str(k, loc)), keys)
}

#' data-value → 目前 locale 標籤（小頁籤）
ui_tab_label_map <- function(locale = "en") {
  loc <- normalize_ui_locale(locale)
  dvs <- names(.UI_TAB_DATA_VALUE_KEYS)
  stats::setNames(
    lapply(dvs, function(dv) ui_str(.UI_TAB_DATA_VALUE_KEYS[[dv]], loc)),
    dvs
  )
}

#' tabBox 標題：每組 aliases（en+zh）→ 目前 locale 標籤
ui_box_header_specs <- function(locale = "en") {
  loc <- normalize_ui_locale(locale)
  keys <- unique(unname(.UI_BOX_HEADER_KEYS))
  lapply(keys, function(k) {
    aliases <- names(.UI_BOX_HEADER_KEYS)[.UI_BOX_HEADER_KEYS == k]
    aliases <- unique(c(aliases, ui_str(k, "en"), ui_str(k, "zh-TW")))
    list(match = as.character(aliases), label = ui_str(k, loc))
  })
}
