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
    market_hint = "Market"
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
    market_hint = "市場"
  )
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
