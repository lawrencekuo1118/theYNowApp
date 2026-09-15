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
    menu_get_started = "Basic Setup",
    menu_dcf = "DCF-Model",
    menu_ddm = "DDM",
    menu_pb = "P/B",
    menu_ri = "RI-Model",
    menu_nav = "NAV",
    menu_cat_asset = "Asset-Based Approach",
    menu_cat_income = "Income / Cash Flow Approach",
    menu_cat_relative = "Relative Valuation",
    menu_ynow = "YNOW",
    menu_bluechip = "Blue Chip Ranking",
    menu_hfv = "Hist. FV Validation",
    menu_about = "About",
    ticker_label = "Ticker / Stock Code",
    data_source_title = "Data Source:",
    data_source_body = paste0(
      "This application integrates real-time financial data via web parsing ",
      "and API resources, applying comprehensive models for valuation."
    ),
    download_report = "Download Report (PDF)",
    snapshot_link = " Snapshot",
    test_link = " Testing",
    feedback_link = " Feedback",
    market_hint = "Market",
    industry_standard = "Industry Standard",
    kpi_legend_blue = "Blue · Better",
    kpi_legend_red = "Red · Worse",
    kpi_legend_black = "Black · In band",
    kpi_legend_white = "White · N/A",
    btn_run_bt = "Run Backtest",
    bt_analysis_freq = "Analysis frequency (valuation date Date_t)",
    bt_freq_monthly = "Monthly",
    bt_freq_quarterly = "Quarterly",
    bt_freq_yearly = "Yearly",
    bt_freq_hint = "Only frequencies supported by available data are shown. Monthly appears only when a full monthly series exists (or can be produced). US and TW use the same rule.",
    bt_freq_insufficient = "Price history is too short for analysis-frequency options.",
    hfv_page_title = "Historical Fundamental Validation",
    hfv_page_sub = paste0(
      "Next-period price up/down odds R=(P_next−P)/P, plus position vs theoretical FV. ",
      "This is not a trading-strategy backtest; quantitative backtest lives under Testing (sidebar foot)."
    ),
    lab_notes_title = "Testing — Quantitative Backtest",
    lab_notes_sub = paste0(
      "Strategy NAV, performance, parameters, and holding filters live here. ",
      "Historical Fundamental Validation (theoretical FV vs actual market) is under the Hist. FV Validation sidebar. ",
      "US blue-chip screening is under Blue Chip Ranking. SEC notes: Dashboard → FINANCIAL REPORT → SEC Notes."
    ),
    bt_zone_title = "Backtest Zone",
    box_hfv_discount = "FV vs Market Price",
    box_hfv_validation = "Historical Fundamental Validation: Next-Period Up/Down & vs FV",
    box_hfv_param_inventory = "US Valuation Replay Inventory (Live vs Hist PIT)",
    hfv_fb_title = "Default / fallback notice",
    hfv_fb_item_fmt = "%s — %s (~%d valuation points)",
    hfv_sum_title = "Validation results",
    hfv_sum_price_block = "Q1 · Next-period price move",
    hfv_sum_price_formula = "R = (P_next − P) / P",
    hfv_sum_mos_block = "MOS group outlook (same-ticker history)",
    hfv_sum_fv_block = "Q2 · Position vs theoretical FV",
    hfv_sum_fv_formula = "(P_next − FV) / FV — not the same as up/down",
    hfv_sum_empty = "After Search and model selection, next-period up/down odds and vs-FV stats appear here (not a strategy backtest).",
    hfv_sum_notes = "Result notes",
    hfv_sec_method = "How to read this panel",
    hfv_sec_settings = "Settings",
    hfv_sec_results = "Results",
    hfv_chart_gap = "Magnitude (P_next − FV) / FV",
    hfv_table_detail = "Period detail",

    # --- tabBox headers ---
    box_financial_report = "FINANCIAL REPORT",
    box_performance = "PERFORMANCE",
    box_dividend_discount = "DIVIDEND DISCOUNT",
    box_discounted_cf = "DISCOUNTED CASH FLOW",
    box_sensitivity = "SENSITIVITY",
    box_residual_income = "RESIDUAL INCOME",
    box_pb_asset = "P/B & ASSET VALUE",
    box_nav = "NAV (BOOK HOLDCO)",
    box_blue_chip = "BLUE CHIP RANKING",
    box_beta = "BETA",
    box_sgr = "SUSTAINABLE GROWTH RATE",
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
    tab_sgr = "SGR",
    tab_dcf_overview = "DCF Overview",
    tab_dcf_calc_details = "DCF Calculation Details",
    tab_wacc = "WACC",
    tab_ri_overview = "RI Overview",
    tab_ri_settings = "RI Settings",
    tab_sensitivity_analysis = "Sensitivity Analysis",
    tab_pb_overview = "P/B Overview",
    tab_pb_settings = "P/B Settings",
    tab_target_pb = "Target P/B",
    tab_nav_overview = "NAV Overview",
    tab_nav_settings = "NAV Settings",
    tab_beta_overview = "Beta Overview",
    tab_peer_unlever = "Peer Unlever",
    tab_rolling_beta = "Rolling β",
    tab_decision_matrix = "Decision Matrix",
    tab_about_ddm = "Dividend Discount Model (DDM)",
    tab_about_dcf = "Discounted Cash Flow (DCF)",
    tab_about_ri = "Residual Income (RI)",
    tab_about_pb = "Price-to-Book (P/B)",
    tab_about_nav = "Net Asset Value (NAV)",
    tab_im_filters = "Filters",
    tab_im_detail = "Detail",
    tab_bt_fundamental = "Fundamental Strategy",
    tab_bt_sentiment = "Sentiment Strategy"
  ),
  `zh-TW` = list(
    recent_search = "最近搜尋：",
    menu_dashboard = "總覽 Dashboard",
    menu_get_started = "基礎設定",
    menu_dcf = "DCF 模型",
    menu_ddm = "DDM",
    menu_pb = "P/B",
    menu_ri = "RI 模型",
    menu_nav = "NAV",
    menu_cat_asset = "資產基礎法",
    menu_cat_income = "收益與現金流折現法",
    menu_cat_relative = "相對估值法",
    menu_ynow = "YNOW",
    menu_bluechip = "績優股排行",
    menu_hfv = "歷史基本面驗證",
    menu_about = "關於",
    ticker_label = "Ticker／股票代號",
    data_source_title = "資料來源：",
    data_source_body = paste0(
      "本應用程式整合即時財務資料（網頁解析與 API），",
      "並套用完整估值模型。"
    ),
    download_report = "下載報告 (PDF)",
    snapshot_link = " 快照",
    test_link = " 測試",
    feedback_link = " 意見區",
    market_hint = "市場",
    industry_standard = "產業標準",
    kpi_legend_blue = "藍 · 優於同業 (Better)",
    kpi_legend_red = "紅 · 劣於同業 (Worse)",
    kpi_legend_black = "黑 · 與同業一致 (In band)",
    kpi_legend_white = "白 · 無法比較／N/A",
    btn_run_bt = "執行回測",
    bt_analysis_freq = "分析頻率（估值日 Date_t）",
    bt_freq_monthly = "每月",
    bt_freq_quarterly = "每季",
    bt_freq_yearly = "每年",
    bt_freq_hint = "僅顯示資料可支持之頻率：有完整每月序列才顯示「每月」；僅有季頻則只顯示「每季」。美股／台股相同。",
    bt_freq_insufficient = "股價歷史不足，尚無可用分析頻率。",
    hfv_page_title = "歷史基本面驗證",
    hfv_page_sub = paste0(
      "市價下期漲跌機率 R=(P下一期−P)/P，以及相對理論 FV 的位置／幅度。",
      "這不是交易策略回測；量化回測請至側邊底部「測試」。"
    ),
    lab_notes_title = "測試 — Testing（量化回測）",
    lab_notes_sub = paste0(
      "量化回測（策略淨值／績效／參數）與持倉閘門在此。",
      "歷史基本面驗證（理論估值 vs 實際市值）請至側邊「歷史基本面驗證」。",
      "美股績優篩選請至側邊「績優股排行」。SEC 財報附註在 Dashboard → FINANCIAL REPORT →「財報附註 (SEC)」。"
    ),
    bt_zone_title = "量化回測實驗室 (Backtest Zone)",
    box_hfv_discount = "折現比較（合理價 vs 實際股價）",
    box_hfv_validation = "歷史基本面驗證：市價下期漲跌與相對 FV",
    box_hfv_param_inventory = "美股估值復盤參數盤點（Live vs Hist PIT）",
    hfv_fb_title = "預設／fallback 提醒",
    hfv_fb_item_fmt = "%s — %s（約 %d 個估值點）",
    hfv_sum_title = "驗證結果",
    hfv_sum_price_block = "問題一・市價下期漲跌",
    hfv_sum_price_formula = "R = (P下一期 − P) / P",
    hfv_sum_mos_block = "安全邊際（MOS）分組展望（該股自身歷史）",
    hfv_sum_fv_block = "問題二・相對理論 FV",
    hfv_sum_fv_formula = "(P下一期 − FV) / FV — 與漲跌不同口徑",
    hfv_sum_empty = "載入標的並勾選評價模型後，將顯示市價下期漲跌機率與相對 FV 統計（非策略回測）。",
    hfv_sum_notes = "結果附註",
    hfv_sec_method = "說明",
    hfv_sec_settings = "設定",
    hfv_sec_results = "結果",
    hfv_chart_gap = "幅度 (P下一期 − FV) / FV",
    hfv_table_detail = "逐期明細",

    # --- tabBox headers ---
    box_financial_report = "財務報表",
    box_performance = "績效指標",
    box_dividend_discount = "股利折現 DDM",
    box_discounted_cf = "折現現金流 DCF",
    box_sensitivity = "敏感度分析",
    box_residual_income = "剩餘收益 RI",
    box_pb_asset = "P/B 與相對估值",
    box_nav = "NAV（帳面控股）",
    box_blue_chip = "績優股排行",
    box_beta = "Beta β",
    box_sgr = "SUSTAINABLE GROWTH RATE",
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
    tab_sgr = "SGR",
    tab_dcf_overview = "DCF 總覽",
    tab_dcf_calc_details = "DCF 計算明細",
    tab_wacc = "WACC",
    tab_ri_overview = "RI 總覽",
    tab_ri_settings = "RI 設定",
    tab_sensitivity_analysis = "敏感度分析",
    tab_pb_overview = "P/B 總覽",
    tab_pb_settings = "P/B 設定",
    tab_target_pb = "目標本淨比",
    tab_nav_overview = "NAV 總覽",
    tab_nav_settings = "NAV 設定",
    tab_beta_overview = "Beta 總覽",
    tab_peer_unlever = "同業去槓桿",
    tab_rolling_beta = "Rolling β",
    tab_decision_matrix = "決策矩陣",
    tab_about_ddm = "股利折現模型 (DDM)",
    tab_about_dcf = "折現現金流模型 (DCF)",
    tab_about_ri = "剩餘收益模型 (RI)",
    tab_about_pb = "本淨比 (P/B)",
    tab_about_nav = "淨資產價值 (NAV)",
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
  "NAV Overview" = "tab_nav_overview",
  "NAV Settings" = "tab_nav_settings",
  "Beta Overview" = "tab_beta_overview",
  "SGR" = "tab_sgr",
  "peer_unlever" = "tab_peer_unlever",
  "Rolling β" = "tab_rolling_beta",
  "Decision Matrix" = "tab_decision_matrix",
  "Dividend Discount Model (DDM)" = "tab_about_ddm",
  "Discounted Cash Flow (DCF)" = "tab_about_dcf",
  "Residual Income (RI)" = "tab_about_ri",
  "Price-to-Book (P/B)" = "tab_about_pb",
  "Net Asset Value (NAV)" = "tab_about_nav",
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
  "NAV (BOOK HOLDCO)" = "box_nav",
  "BLUE CHIP" = "box_blue_chip",
  "BLUE CHIP RANKING" = "box_blue_chip",
  "Blue Chip 績優" = "box_blue_chip",
  "績優股排行" = "box_blue_chip",
  "BETA" = "box_beta",
  "SUSTAINABLE GROWTH RATE" = "box_sgr",
  "模型選擇決策指南" = "box_model_guide",
  "Model Selection Guide" = "box_model_guide",
  "策略參數設定" = "box_bt_params",
  "Strategy Parameters" = "box_bt_params",
  "折現比較（合理價 vs 實際股價）" = "box_hfv_discount",
  "FV vs Market Price" = "box_hfv_discount",
  "歷史基本面驗證：市價下期漲跌與相對 FV" = "box_hfv_validation",
  "Historical Fundamental Validation: Next-Period Up/Down & vs FV" = "box_hfv_validation",
  "歷史基本面驗證：理論估值 vs 實際市值（漲跌機率／幅度）" = "box_hfv_validation",
  "Historical Fundamental Validation: Theoretical FV vs Actual Market (odds & magnitude)" = "box_hfv_validation",
  "美股估值復盤參數盤點（Live vs Hist PIT）" = "box_hfv_param_inventory",
  "US Valuation Replay Inventory (Live vs Hist PIT)" = "box_hfv_param_inventory"
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
