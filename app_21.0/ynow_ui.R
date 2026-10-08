# ==========================================
# ui.R - 前端介面設計
# ==========================================

#' Display / deploy build id from www/ynow_build.json (baked at UI source time).
.ynow_read_build_version <- function() {
  paths <- c(
    file.path("www", "ynow_build.json"),
    "ynow_build.json"
  )
  for (p in paths) {
    if (!file.exists(p)) next
    raw <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    m <- regmatches(raw, regexpr('"version"\\s*:\\s*"[^"]+"', raw))
    if (length(m) == 1L && nzchar(m[[1]])) {
      return(sub('^"version"\\s*:\\s*"([^"]+)".*$', "\\1", m[[1]]))
    }
  }
  "v21.44"
}
.YNOW_BUILD_VERSION <- .ynow_read_build_version()

# Backtest Zone：欄位下方小字說明
.bt_hint <- function(text) {
  tags$p(
    style = "margin: -6px 0 14px 0; font-size: 11.5px; line-height: 1.45; color: #777;",
    text
  )
}

.bt_section_intro <- function(text) {
  tags$p(style = "margin: 0 0 12px 0; font-size: 12.5px; color: #555; line-height: 1.5;", text)
}

#' Privacy / IP / investment-risk notices (About: bilingual zh-TW | en-US)
.legal_notices_ui <- function() {
  en <- tryCatch(.UI_STRINGS$en, error = function(e) NULL)
  zh <- tryCatch(.UI_STRINGS$`zh-TW`, error = function(e) NULL)
  if (is.null(en) || is.null(zh)) return(NULL)
  .legal_pair <- function(title_zh, body_zh, title_en, body_en) {
    fluidRow(
      class = "ynow-legal-row",
      column(
        width = 6,
        class = "ynow-about-col ynow-about-col--zh",
        tags$h4(class = "ynow-legal-h", title_zh),
        tags$p(class = "ynow-legal-body", body_zh)
      ),
      column(
        width = 6,
        class = "ynow-about-col ynow-about-col--en",
        tags$h4(class = "ynow-legal-h", title_en),
        tags$p(class = "ynow-legal-body", body_en)
      )
    )
  }
  tags$div(
    class = "ynow-legal-notices",
    tags$h3(
      class = "ynow-about-section-title",
      paste0(zh$legal_section_title, " / ", en$legal_section_title)
    ),
    .legal_pair(
      zh$legal_privacy_title, zh$legal_privacy_body,
      en$legal_privacy_title, en$legal_privacy_body
    ),
    .legal_pair(
      zh$legal_ip_title, zh$legal_ip_body,
      en$legal_ip_title, en$legal_ip_body
    ),
    .legal_pair(
      zh$legal_risk_title, zh$legal_risk_body,
      en$legal_risk_title, en$legal_risk_body
    )
  )
}

#' About 分頁：中英左右對照專案簡介
.about_bilingual_intro_ui <- function() {
  brand <- tags$div(
    class = "ynow-about-brand",
    tags$img(
      class = "ynow-about-logo-full",
      src = "ynow-logo-full-480.png",
      alt = "YNow — WH.Y VALUE NOW",
      width = 240,
      height = 240
    )
  )
  zh_features <- tags$ul(
    class = "ynow-about-feat",
    tags$li(
      tags$b("自動化資料與防雷機制："),
      "即時擷取三大報表，並內建「財報警訊」（三表對照），交叉比對現金流與獲利品質，自動偵測潛在地雷股與價值陷阱；支援美股與台股市場切換。"
    ),
    tags$li(
      tags$b("估值引擎："),
      "內建 Fair Value 引擎（DCF：FCFF／WACC 或 FCFE／Ke、DDM：Gordon／SPM／二階段、P/B＋NAV、RI）與 Implied Price 相對引擎（Multiples、SOTP）；依產業屬性動態推薦主／副路徑，並以 Composite valuation 並陳各模型相對現價位置。"
    ),
    tags$li(
      tags$b("智慧決策與量化回測："),
      "結合 Piotroski F-Score（品質檢核）、安全邊際 (MOS)、決策檢核（Decision Checklist）、Historical Fundamental Validation (HFV)，以及 Point-in-Time (PIT) 回測引擎，提供貼近實戰的策略驗證。"
    ),
    tags$li(
      tags$b("Blue Chip Lab："),
      "產業×方法評估池、Clustering（比率特徵／離線快照備援）、候選截斷邏輯（市值／概念股／近一年漲幅／隨機），輔助同業比較與研究分群（非買進訊號）。"
    ),
    tags$li(
      tags$b("一次產出投資報告："),
      "自動彙整估值圖表、KPI 與分析結果，產出可下載的專業 PDF 投資意見報告。"
    )
  )
  en_features <- tags$ul(
    class = "ynow-about-feat",
    tags$li(
      tags$b("Automated Data & Fraud Detection: "),
      "Fetches the three financial statements in real time and flags earnings-quality issues by cross-checking cash flow against reported profits, helping surface potential value traps. Supports US and TW market modes."
    ),
    tags$li(
      tags$b("Valuation Engines: "),
      "Fair Value engines (DCF: FCFF/WACC or FCFE/Ke; DDM: Gordon / SPM / two-stage; P/B with holding NAV; Residual Income) plus Implied Price relative engines (Multiples, SOTP). The app recommends a primary/secondary path by industry attributes and overlays model values versus the current price on the Composite valuation axis."
    ),
    tags$li(
      tags$b("Smart Decision Matrix & Backtesting: "),
      "Combines the Piotroski F-Score (quality screen), Margin of Safety (MOS), Decision Checklist, Historical Fundamental Validation (HFV), and an institutional-grade Point-in-Time (PIT) backtesting engine for real-world strategy checks."
    ),
    tags$li(
      tags$b("Blue Chip Lab: "),
      "Industry × method evaluation pools, Clustering (ratio features with offline snapshot fallback), and candidate truncate rules (market cap / concept groups / 1Y return / random) for peer research—not buy signals."
    ),
    tags$li(
      tags$b("One-Click Investment Reports: "),
      "Compiles ticker-only valuation (DCF／DDM／RI／P/B), KPIs, MOS, F-Score, and WACC×g sensitivity into a broker-style PDF — no peer ranking or lab-universe narrative."
    )
  )

  github_url <- "https://github.com/lawrencekuo1118/theYNowApp"

  tagList(
    brand,
    fluidRow(
      class = "ynow-about-bilingual",
      column(
        width = 6,
        class = "ynow-about-col ynow-about-col--zh",
        tags$h2(class = "ynow-about-title", tags$b("關於 The YNow App")),
        tags$p(
          class = "ynow-about-lead",
          "The YNow App (v21.44) 是一套專為專業投資人與分析師打造的「全方位量化財務與估值決策系統」。本系統整合即時財報擷取、多模型估值、決策檢核、Blue Chip Lab 與動態回測，將繁雜的市場資料轉化為可執行的投資決策架構。"
        ),
        tags$p(
          class = "ynow-about-method",
          "我們的核心方法論為：",
          tags$b("「先分類，再選模型；先推導，再校正；先給區間，再給單點。」"),
          " 完整「Valuation Methodology｜評價方法論」矩陣與公式說明見 ",
          tags$b("Model Dashboard"),
          " 分頁最下方。"
        ),
        tags$h4(class = "ynow-about-feat-h", tags$b("核心功能亮點：")),
        zh_features
      ),
      column(
        width = 6,
        class = "ynow-about-col ynow-about-col--en",
        tags$h2(class = "ynow-about-title", tags$b("About The YNow App")),
        tags$p(
          class = "ynow-about-lead",
          "The YNow App (v21.44) is a comprehensive quantitative financial analysis and valuation decision system for professional investors and analysts. It integrates real-time financials, multi-model valuation, Decision Checklist, Blue Chip Lab, and dynamic backtesting to turn complex market data into a disciplined decision framework."
        ),
        tags$p(
          class = "ynow-about-method",
          "Our core methodology is: ",
          tags$b("\"Classify before selecting models; derive before calibrating; provide valuation ranges before absolute price targets.\""),
          " The full Valuation Methodology matrix and formulas sit at the bottom of ",
          tags$b("Model Dashboard"),
          "."
        ),
        tags$h4(class = "ynow-about-feat-h", tags$b("Core Features:")),
        en_features
      )
    ),
    tags$p(
      class = "ynow-about-github",
      tags$span("GitHub："),
      tags$a(
        href = github_url,
        target = "_blank",
        rel = "noopener noreferrer",
        github_url
      )
    )
  )
}

#' About 分頁（Lite）：對齊簡化版可見功能，不含完整方法論手冊
.about_lite_intro_ui <- function() {
  brand <- tags$div(
    class = "ynow-about-brand",
    tags$img(
      class = "ynow-about-logo-full",
      src = "ynow-logo-full-480.png",
      alt = "YNow — WH.Y VALUE NOW",
      width = 240,
      height = 240
    )
  )
  zh_features <- tags$ul(
    class = "ynow-about-feat",
    tags$li(
      tags$b("簡化版切換："),
      "點選首頁或側邊欄底部 logo 即可在 Lite／完整版之間切換；Lite 時兩處 logo 右下角都顯示 LITE 角標。"
    ),
    tags$li(
      tags$b("個股："),
      "輸入股票代號、檢視產業標準快覽與 KPI 色碼框格，並使用市場／語言／幣別等切換 UI；精簡財報明細與模型參數頁。"
    ),
    tags$li(
      tags$b("智慧分析："),
      "依股票性質自動判別主／副估值模型（DCF、DDM、RI、P/B、NAV；Multiples／SOTP 僅作 Implied Price 交叉檢核），",
      "辨識參數情境（Two-Stage／Gordon、SGR 法、claim）並套用最合理預設後試算，",
      "顯示合理價比較圖與 MOS；不開放手動模型設定。"
    ),
    tags$li(
      tags$b("YNOW："),
      "與完整版相同的盈餘品質、F-Score 品質檢核、風險矩陣與財報警訊；動態產業泡沫與權重集中度在「總體經濟與大盤趨勢」分頁最下方。"
    ),
    tags$li(
      tags$b("績優股排行榜（Blue Chip）："),
      "保留候選截斷邏輯與宇宙檔數（N）、盈餘品質與 ADR 篩選、排行頁底部明細表，以及分群研究；隱藏其餘進階查詢條件。"
    )
  )
  en_features <- tags$ul(
    class = "ynow-about-feat",
    tags$li(
      tags$b("Lite toggle: "),
      "Click the home logo or the sidebar bottom logo to switch between Lite and Full. In Lite, a LITE badge appears at both logo corners."
    ),
    tags$li(
      tags$b("Company: "),
      "Enter a ticker, review the industry standard snapshot and KPI color boxes, and use market / language / currency switches. Statement detail tabs and manual model pages are hidden."
    ),
    tags$li(
      tags$b("Smart Analysis: "),
      "Auto-selects primary and secondary valuation models (DCF, DDM, RI, P/B, NAV; Multiples / SOTP as Implied Price cross-checks only) from the ticker profile, ",
      "detects the best parameter scenario (Two-Stage vs Gordon, SGR method, claim), applies those defaults, ",
      "and shows fair-value comparison charts plus MOS—no manual model settings."
    ),
    tags$li(
      tags$b("YNOW: "),
      "Same Quality of Earnings, F-Score screen, risk matrix, and statement alerts as Full. Dynamic industry bubble & weight concentration sit at the bottom of Macro & Market Trends."
    ),
    tags$li(
      tags$b("Blue Chip Leaderboard: "),
      "Keeps Candidate truncate rules and Universe size (N), Earnings Quality and ADR filters, the detail table at the bottom of Rankings, plus Clustering. Hides other advanced Ranking filters."
    )
  )

  github_url <- "https://github.com/lawrencekuo1118/theYNowApp"

  tagList(
    brand,
    fluidRow(
      class = "ynow-about-bilingual",
      column(
        width = 6,
        class = "ynow-about-col ynow-about-col--zh",
        tags$h2(class = "ynow-about-title", tags$b("關於 The YNow App（簡化版）")),
        tags$p(
          class = "ynow-about-lead",
          "The YNow App Lite（v21.44）是完整版的精簡工作流：共用同一套財報資料與估值公式，",
          "以自動判別主／副模型與預設參數完成試算，讓使用者先看到合理價區間與產業 KPI，再決定是否回到完整版深入調整。"
        ),
        tags$p(
          class = "ynow-about-method",
          "簡化版原則：",
          tags$b("「先看結果，再開參數；共用引擎，不另開爬蟲。」")
        ),
        tags$h4(class = "ynow-about-feat-h", tags$b("簡化版可見功能：")),
        zh_features
      ),
      column(
        width = 6,
        class = "ynow-about-col ynow-about-col--en",
        tags$h2(class = "ynow-about-title", tags$b("About The YNow App (Lite)")),
        tags$p(
          class = "ynow-about-lead",
          "The YNow App Lite (v21.44) is a streamlined workflow of the Full app. It reuses the same financial data and valuation formulas, ",
          "auto-selects primary/secondary models with App defaults, and surfaces fair-value ranges plus industry KPIs before you open Full for deeper calibration."
        ),
        tags$p(
          class = "ynow-about-method",
          "Lite principle: ",
          tags$b("\"Results first, parameters later; shared engines—no separate scrapers.\"")
        ),
        tags$h4(class = "ynow-about-feat-h", tags$b("What Lite shows:")),
        en_features
      )
    ),
    tags$p(
      class = "ynow-about-github",
      tags$span("GitHub："),
      tags$a(
        href = github_url,
        target = "_blank",
        rel = "noopener noreferrer",
        github_url
      )
    )
  )
}

#' 品牌首頁：一句用途 + 四個主要入口
.home_brand_ui <- function() {
  tagList(
    tags$div(
      class = "ynow-home",
      tags$div(
        class = "ynow-home-logo-wrap ynow-lite-toggle",
        id = "ynow_home_lite_toggle",
        role = "button",
        tabindex = "0",
        `aria-pressed` = "false",
        `aria-label` = "Toggle Lite mode",
        title = "Click to switch Lite / Full",
        tags$img(
          class = "ynow-home-logo",
          src = "ynow-logo-full-480.png",
          alt = "YNow — WH.Y VALUE NOW",
          width = 132,
          draggable = "false"
        ),
        tags$span(
          class = "ynow-lite-badge",
          id = "ynow_home_lite_badge",
          "LITE"
        )
      ),
      tags$h2(id = "ynow_home_title", class = "ynow-home-title", "The YNow App"),
      tags$p(
        id = "ynow_home_lead",
        class = "ynow-home-lead",
        "A valuation desk for US and Taiwan equities."
      ),
      tags$p(
        id = "ynow_home_method",
        class = "ynow-home-method ynow-full-only",
        "Classify the business, choose the model, then decide."
      ),
      tags$p(
        id = "ynow_home_method_lite",
        class = "ynow-home-method ynow-lite-only",
        "Classify the business, then read fair value and YNOW."
      ),
      tags$div(
        class = "ynow-home-grid",
        role = "group",
        `aria-labelledby` = "ynow_home_title",
        # Order mirrors sidebar after Home: Macro → Blue Chip → Company → YNOW → Value → Action
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "macro_market",
          tags$p(id = "ynow_home_market_k", class = "ynow-home-card-k", "Market"),
          tags$p(
            id = "ynow_home_market_d",
            class = "ynow-home-card-d",
            "Index levels, and industry performance versus the benchmark."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "bluechip",
          tags$p(id = "ynow_home_bluechip_k", class = "ynow-home-card-k", "Blue Chip"),
          tags$p(
            id = "ynow_home_bluechip_d",
            class = "ynow-home-card-d",
            "Peer ranking pools, truncate rules, and clustering for research."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "dashboard",
          tags$p(id = "ynow_home_company_k", class = "ynow-home-card-k", "Company"),
          tags$p(
            id = "ynow_home_company_d",
            class = "ynow-home-card-d",
            "Industry-standard snapshot, KPIs, and financial statements."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "sensitivity",
          tags$p(id = "ynow_home_ynow_k", class = "ynow-home-card-k", "YNOW"),
          tags$p(
            id = "ynow_home_ynow_d",
            class = "ynow-home-card-d",
            "F-Score and statement alerts. A quality screen, not a buy signal."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card",
          `data-tab` = "get_started",
          `data-tab-lite` = "smart_analysis",
          tags$p(id = "ynow_home_value_k", class = "ynow-home-card-k ynow-full-only", "Fundamental Value"),
          tags$p(id = "ynow_home_value_k_lite", class = "ynow-home-card-k ynow-lite-only", "Smart Analysis"),
          tags$p(
            id = "ynow_home_value_d",
            class = "ynow-home-card-d ynow-full-only",
            "Fair value and MOS across DCF, DDM, RI, and P/B."
          ),
          tags$p(
            id = "ynow_home_value_d_lite",
            class = "ynow-home-card-d ynow-lite-only",
            "Fair value and MOS from the recommended model."
          )
        ),
        tags$button(
          type = "button",
          class = "ynow-home-card ynow-full-only",
          `data-tab` = "decision_checklist",
          tags$p(id = "ynow_home_decide_k", class = "ynow-home-card-k", "Action"),
          tags$p(
            id = "ynow_home_decide_d",
            class = "ynow-home-card-d",
            "F-Score, statement alerts, and the Decision Checklist."
          )
        )
      ),
      tags$div(
        class = "ynow-home-legal",
        tags$p(
          id = "ynow_home_legal",
          class = "ynow-home-legal-text",
          paste0(
            "Research and education only — not investment advice. ",
            "You alone bear all investment decisions and losses."
          )
        ),
        tags$button(
          type = "button",
          id = "ynow_home_legal_about",
          class = "ynow-home-legal-link",
          `data-tab` = "about",
          "About · Privacy, IP & risk"
        )
      )
    ),
    tags$script(HTML("
      (function () {
        if (document.documentElement.getAttribute('data-ynow-home') === '1') return;
        document.documentElement.setAttribute('data-ynow-home', '1');
        document.addEventListener('click', function (ev) {
          var card = ev.target && ev.target.closest
            ? ev.target.closest('.ynow-home-card, .ynow-home-legal-link')
            : null;
          if (!card) return;
          ev.preventDefault();
          var lite = document.body && document.body.classList.contains('ynow-lite');
          var tab = card.getAttribute('data-tab');
          if (lite) tab = card.getAttribute('data-tab-lite') || tab;
          if (!tab) return;
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
          }
          var a = document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
          if (a) { try { a.click(); } catch (e) {} }
        });
      })();
    "))
  )
}

#' KPI 色碼圖例：藍→紅→黑→白；金色屬性重視改一行文字註解（非 chip 框）
.kpi_band_color_legend_ui <- function() {
  tags$div(
    class = "ynow-kpi-legend-wrap",
    tags$div(
      class = "ynow-ann-legend ynow-kpi-legend-chips",
      tags$div(
        class = "ynow-ann-chip ynow-kpi-legend-chip",
        tags$span(class = "ynow-ann-swatch", style = "background:#3c8dbc;"),
        tags$span(id = "ynow_kpi_legend_blue", "藍 · 優於同業 (Better)")
      ),
      tags$div(
        class = "ynow-ann-chip ynow-kpi-legend-chip",
        tags$span(class = "ynow-ann-swatch", style = "background:#dd4b39;"),
        tags$span(id = "ynow_kpi_legend_red", "紅 · 劣於同業 (Worse)")
      ),
      tags$div(
        class = "ynow-ann-chip ynow-kpi-legend-chip",
        tags$span(class = "ynow-ann-swatch", style = "background:#222;"),
        tags$span(id = "ynow_kpi_legend_black", "黑 · 與同業一致 (In band)")
      ),
      tags$div(
        class = "ynow-ann-chip ynow-kpi-legend-chip",
        tags$span(class = "ynow-ann-swatch",
                  style = "background:#fff; border:1px solid #999;"),
        tags$span(id = "ynow_kpi_legend_white", "白 · 無法比較／N/A")
      )
    ),
    tags$p(
      class = "ynow-focus-metric-note",
      tags$span(
        class = "ynow-focus-metric-dot ynow-focus-metric-dot--legend",
        `aria-hidden` = "true"
      ),
      tags$span(id = "ynow_kpi_legend_focus", "金 · 本財報屬性關鍵指標")
    )
  )
}

# Shared gray「回復預設」／試算 helpers live in setup.R (ynow_reset_defaults_btn, ynow_calc_btn).

#' Shared CAPM / Beta settings block (canonical IDs on DCF → WACC).
#' DDM → Ke uses the same layout with `id_prefix = "ddm_"`; server keeps mirrors in sync.
#' @param calc_id actionButton id
#' @param result_id htmlOutput id for CAPM result text
#' @param width shinydashboard box width (1–12)
#' @param id_prefix optional prefix for input ids (e.g. `"ddm_"` → `ddm_capm_rf`)
#' @param box_title_id DOM id for locale title
#' @param sync_label_id DOM id for sync-checkbox label
#' @param rf_note_id uiOutput id for Rf source note
#' @param btn_label actionButton label (locale may overwrite)
capm_beta_settings_ui <- function(title = "CAPM Estimate rₑ",
                                  calc_id = "calc_capm",
                                  result_id = "capm_result",
                                  width = 6,
                                  id_prefix = "",
                                  box_title_id = "ynow_capm_box_title",
                                  sync_label_id = "ynow_sync_gs_beta_label",
                                  rf_note_id = "capm_rf_source_note",
                                  btn_label = "Estimate rₑ (CAPM)") {
  pid <- function(x) {
    if (nzchar(as.character(id_prefix %||% "")[1])) paste0(id_prefix, x) else x
  }
  box(
    width = width,
    h4(title, id = box_title_id),
    numericInput(pid("capm_rf"), "Risk-free rate Rf (%)", value = APP_DEFAULTS$capm_rf, step = 0.01),
    uiOutput(rf_note_id),
    numericInput(pid("capm_rm"), "Market return Rm (%)", value = APP_DEFAULTS$capm_rm, step = 0.01),
    numericInput(pid("capm_beta"), "Beta (β)", value = APP_DEFAULTS$capm_beta, step = 0.01),
    checkboxInput(
      pid("sync_gs_beta"),
      tags$span(id = sync_label_id, style = "font-weight: bold;", "Sync with Basic Setup"),
      value = isTRUE(APP_DEFAULTS$sync_gs_beta)
    ),
    actionButton(calc_id, btn_label, class = "btn-primary ynow-btn-calc-capm"),
    tags$br(), htmlOutput(result_id)
  )
}

#' Left companion on DDM → Ke (mirrors WACC tab’s rᵈ column role: context beside CAPM).
ddm_ke_bridge_settings_ui <- function(width = 6) {
  box(
    width = width,
    h4("Ke and central rₑ", id = "ynow_ddm_ke_bridge_title"),
    tags$p(
      id = "ynow_ddm_ke_bridge_help",
      style = "margin:0 0 10px 0;color:#666;font-size:12px;line-height:1.55;",
      paste0(
        "DDM discounts at equity cost Ke, the same central Ke as DCF→WACC rₑ. ",
        "When \"Use estimated Ke\" is checked, Ke follows CAPM; uncheck to override and keep bidirectional sync with WACC rₑ. ",
        "Choose β source on this model's Beta (β) tab."
      )
    ),
    tags$div(
      style = "padding:8px;border-left:4px solid #222222;background:#f5f5f5;font-size:13px;margin-bottom:10px;",
      tags$b("CAPM："), " Ke = Rf + β × (Rm − Rf)",
      tags$br(),
      tags$span(style = "color:#666;font-size:12px;", "ERP = Rm − Rf (equity risk premium)")
    ),
    htmlOutput("ddm_ke_bridge_status")
  )
}

#' rᵈ estimation block: Interest Expense ÷ Interest-bearing Debt (pre-tax rᵈ).
#' Layout companion to CAPM on the WACC tab (left 50%).
rd_estimate_settings_ui <- function(width = 6) {
  box(
    width = width,
    h4("估算 rᵈ", id = "ynow_rd_box_title"),
    numericInput(
      "rd_interest_expense", "利息費用",
      value = NA_real_, min = 0, step = 1
    ),
    uiOutput("rd_interest_source_note"),
    numericInput(
      "rd_interest_bearing_debt", "有息負債",
      value = NA_real_, min = 0, step = 1
    ),
    uiOutput("rd_debt_source_note"),
    fluidRow(
      column(6, numericInput(
        "wacc_rd_min", "估算 rᵈ 下限 (%)",
        value = APP_DEFAULTS$wacc_rd_min, min = 0, step = 0.1
      )),
      column(6, numericInput(
        "wacc_rd_max", "估算 rᵈ 上限 (%)",
        value = APP_DEFAULTS$wacc_rd_max, min = 0, step = 0.1
      ))
    ),
    checkboxInput(
      "use_estimated_rd",
      tags$span(id = "ynow_use_estimated_rd_label", "採用估算 rᵈ（利息／有息負債）"),
      value = isTRUE(APP_DEFAULTS$use_est_rd)
    ),
    actionButton("calc_rd", "估算 rᵈ", class = "btn-primary"),
    tags$br(), htmlOutput("rd_result")
  )
}

#' Canonical + mirrored β-source radio IDs (bidirectional sync in server).
#' Basic Setup ↔ DCF ↔ DDM ↔ RI. P/B / NAV do not host Beta tabs.
.BETA_U_APPLY_SOURCE_IDS <- c(
  "beta_u_apply_source",
  "dcf_beta_u_apply_source",
  "ddm_beta_u_apply_source",
  "ri_beta_u_apply_source"
)

#' Matching "sync selected β" buttons (same order as .BETA_U_APPLY_SOURCE_IDS).
.BETA_U_APPLY_BTN_IDS <- c(
  "apply_beta_u_selected",
  "dcf_apply_beta_u_selected",
  "ddm_apply_beta_u_selected",
  "ri_apply_beta_u_selected"
)

#' Shared β source picker (Yahoo / industry / Bottom-Up / Hamada / manual).
#' Unique input_id per placement; server keeps all IDs in sync.
#' @param input_id radioButtons id
#' @param apply_btn_id actionButton id (sync selected β into CAPM)
#' @param include_hidden_purpose keep legacy beta_purpose input (Basic Setup only)
#' @param include_crosscheck render beta_crosscheck_panel under the picker
beta_source_picker_ui <- function(input_id,
                                  apply_btn_id,
                                  include_hidden_purpose = FALSE,
                                  include_crosscheck = FALSE,
                                  extra = NULL) {
  default_selected <- tryCatch(
    APP_DEFAULTS$beta_u_apply_source,
    error = function(e) "summary"
  )
  if (!nzchar(as.character(default_selected %||% "")[1])) default_selected <- "summary"

  tagList(
    tags$p(
      class = "ynow-beta-source-heading",
      style = "font-weight:600; margin:0 0 10px 0;",
      "Beta source (default writes into CAPM)"
    ),
    # choiceNames／choiceValues 由 server 動態覆寫（數字粗體 + 各選項旁說明）
    radioButtons(
      input_id,
      label = NULL,
      choiceNames = list(
        HTML("Yahoo Finance Summary β <b>n/a</b> <span style='color:#666;font-size:12px;'>— Yahoo Finance Summary \"Beta (5Y Monthly)\"; default writes into CAPM.</span>"),
        HTML("Industry default β <b>n/a</b> <span style='color:#666;font-size:12px;'>— Structural β for the selected industry.</span>"),
        HTML("Peer-average Bottom-Up (βᵤ→βe) <b>n/a</b> <span style='color:#666;font-size:12px;'>— Unlevered mean / median βᵤ of peer companies.</span>"),
        HTML("Unlevered βᵤ <b>n/a</b> <span style='color:#666;font-size:12px;'>— Hamada βᵤ = β_L / (1+(1−T)·D/E).</span>"),
        HTML("Manual βe <b>n/a</b>")
      ),
      choiceValues = list("summary", "industry", "bottomup", "unlever_firm", "manual"),
      selected = default_selected,
      inline = FALSE
    ),
    if (isTRUE(include_hidden_purpose)) {
      tags$div(
        style = "display:none;",
        radioButtons(
          "beta_purpose",
          NULL,
          choices = c("valuation" = "valuation"),
          selected = "valuation"
        )
      )
    },
    tags$p(
      class = "ynow-beta-rolling-help help-block",
      style = "margin-top:0;",
      "Rolling estimates are for cross-check only and are not written into CAPM (hence omitted above)."
    ),
    actionButton(
      apply_btn_id,
      "Sync selected β now",
      class = "btn-success ynow-btn-sync-selected-beta",
      icon = icon("check")
    ),
    extra,
    if (isTRUE(include_crosscheck)) {
      tagList(tags$br(), tags$br(), uiOutput("beta_crosscheck_panel"))
    }
  )
}

#' Model Beta tab: shared picker + note (replaces old "moved to Basic Setup" stub).
#' No outer box/well chrome — controls sit flush in the tab.
.beta_model_source_section_ui <- function(input_id, apply_btn_id, extra = NULL) {
  fluidRow(
    column(
      width = 12,
      beta_source_picker_ui(
        input_id = input_id,
        apply_btn_id = apply_btn_id,
        extra = extra
      )
    )
  )
}

#' Beta Overview：來源寫入 CAPM + Rolling 僅對照
beta_overview_section_ui <- function() {
  tagList(
    fluidRow(
      valueBoxOutput("vbx_beta_summary", width = 6),
      valueBoxOutput("vbx_beta_industry", width = 6)
    ),
    fluidRow(
      valueBoxOutput("vbx_beta_unlever_bottomup", width = 4),
      valueBoxOutput("vbx_beta_unlever_firm", width = 4),
      valueBoxOutput("vbx_beta_estimated", width = 4)
    ),
    fluidRow(
      box(
        width = 12, status = "success", solidHeader = FALSE,
        beta_source_picker_ui(
          input_id = "beta_u_apply_source",
          apply_btn_id = "apply_beta_u_selected",
          include_hidden_purpose = TRUE,
          include_crosscheck = TRUE
        )
      )
    )
  )
}

#' Peer unlever: Bottom-Up peer average (left) + firm Hamada / manual βe (right)
beta_unlever_section_ui <- function() {
  tagList(
    fluidRow(
      box(
        width = 7, status = "warning", solidHeader = FALSE,
        tags$h5(id = "ynow_beta_bottomup_heading", style = "margin-top:0;", "Bottom-Up peer average (primary valuation estimate)"),
        selectizeInput(
          "beta_peers",
          "Peer / competitor tickers (multi-select or type)",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            create = TRUE,
            placeholder = "e.g. INTC, AMD, AVGO …",
            plugins = list("remove_button"),
            maxItems = 15
          )
        ),
        radioButtons(
          "beta_bottomup_agg",
          "βᵤ aggregation",
          choices = c("Mean" = "mean", "Median" = "median"),
          selected = APP_DEFAULTS$beta_bottomup_agg,
          inline = TRUE
        ),
        helpText(
          id = "ynow_beta_bottomup_help",
          paste0(
            "Flow: peer equity β → unlever → mean / median βᵤ. ",
            "If peers are empty, fall back to industry β and industry leverage (data-limited proxy)."
          )
        ),
        actionButton(
          "calc_beta_bottomup", "Calculate Bottom-Up βᵤ",
          class = "btn-primary ynow-btn-calc-beta-bottomup", icon = icon("calculator")
        ),
        tags$br(), tags$br(),
        htmlOutput("beta_bottomup_result"),
        tags$br(),
        tableOutput("beta_bottomup_peers_table")
      ),
      box(
        width = 5, status = "warning", solidHeader = FALSE,
        tags$h5(id = "ynow_beta_unlever_heading", style = "margin-top:0;", "Unlevered βᵤ (Hamada)"),
        tags$div(
          style = "display:none;",
          radioButtons(
            "beta_bl_source", "Levered Beta (β_L) source",
            choices = c(
              "Finance Summary (Yahoo 5Y Monthly)" = "summary",
              "Rolling estimate (run Rolling tab first)" = "rolling",
              "Auto (Summary → Rolling)" = "auto"
            ),
            selected = APP_DEFAULTS$beta_bl_source,
            inline = FALSE
          ),
          # 隱藏保留：舊版再槓桿設定已移除，僅維持 input ID 相容
          radioButtons(
            "beta_relever_de_mode", NULL,
            choices = c("current" = "current"),
            selected = "current"
          ),
          numericInput("beta_target_de", NULL, value = NA, min = 0, max = 10, step = 0.01)
        ),
        helpText(
          id = "ynow_beta_unlever_help",
          paste0(
            "Hamada (debt β≈0): βᵤ = β_L / (1+(1−T)·D/E). ",
            "β_L defaults to Yahoo Finance Summary \"Beta (5Y Monthly)\"; T from WACC; D/E = Total Debt ÷ equity market value. ",
            "Pick \"Unlevered βᵤ\" as β source to write into CAPM; levered β_L itself is not written into CAPM."
          )
        ),
        htmlOutput("beta_unlever_firm_result"),
        tags$hr(),
        tags$h5(id = "ynow_beta_manual_heading", "Manual βe"),
        numericInput(
          "beta_u_manual",
          NULL,
          value = APP_DEFAULTS$beta_u_manual,
          min = 0, max = 5, step = 0.01
        ),
        helpText(
          id = "ynow_beta_manual_help",
          paste0(
            "After selecting \"Manual βe\" as β source, this value writes into CAPM; ",
            "editing here also switches the source to manual and syncs."
          )
        )
      )
    )
  )
}

#' Rolling β estimate (Basic Setup)
#' Cross-check only: compare vs valuation β; do not write into CAPM.
beta_rolling_section_ui <- function() {
  tagList(
    fluidRow(
      box(
        width = 5, status = "primary", solidHeader = TRUE,
        title = tagList(
          icon("sliders-h"),
          tags$span(id = "ynow_beta_rolling_settings_title", "Estimate settings (cross-check only)")
        ),
        selectizeInput(
          "beta_bench", "Benchmark index",
          choices = market_profile("US")$beta_bench_choices,
          selected = APP_DEFAULTS$beta_bench,
          options = list(
            create = TRUE,
            placeholder = "Pick a common index, or enter a ticker…",
            maxItems = 1
          )
        ),
        selectInput(
          "beta_lookback_months", "Lookback window (cross-check)",
          choices = c(
            "1Y (12 months)" = 12,
            "2Y (24 months)" = 24,
            "5Y (60 months, Yahoo-aligned)" = 60
          ),
          selected = as.character(APP_DEFAULTS$beta_lookback_months)
        ),
        # 技術門檻：固定預設，不另開冷門參數
        tags$div(
          style = "display:none;",
          numericInput(
            "beta_min_obs", "Min observation months",
            value = APP_DEFAULTS$beta_min_obs,
            min = 12, max = 60, step = 1
          )
        ),
        helpText(
          id = "ynow_beta_rolling_help_body",
          paste0(
            "Rolling β measures recent equity sensitivity to the market and often embeds sentiment. ",
            "Use it only to cross-check valuation β — it is not written into CAPM / Ke / WACC. ",
            "β = Cov(Rᵢ, Rₘ) / Var(Rₘ); compare 1Y / 2Y / 5Y windows."
          )
        ),
        actionButton(
          "calc_beta_est", "Estimate Rolling β (cross-check)",
          class = "btn-primary ynow-btn-calc-beta-est", icon = icon("calculator")
        ),
        tags$br(), tags$br(),
        htmlOutput("beta_est_result")
      ),
      box(
        width = 7, status = "info", solidHeader = TRUE,
        title = tagList(
          icon("exchange-alt"),
          tags$span(id = "ynow_beta_rolling_compare_title", "Window comparison")
        ),
        tableOutput("beta_window_table"),
        tags$hr(),
        plotOutput("plt_beta_scatter", height = "280px")
      )
    )
  )
}

#' Consideration-dimensions comparison table (Model Selector annotation + shared HTML).
.consideration_dimensions_table <- function() {
  tags$div(
    style = "overflow-x: auto;",
    HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #2C3E50; color: white;'>
                                   <tr>
                                     <th>考慮維度</th>
                                     <th>DDM</th>
                                     <th>DCF</th>
                                     <th>RI</th>
                                     <th>P/B</th>
                                     <th>NAV</th>
                                     <th>Multiples</th>
                                     <th>SOTP</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>主要資料來源</b></td>
                                     <td>現金流量表（股利）</td>
                                     <td>現金流量表（CFO／CapEx）</td>
                                     <td>損益表＋資產負債表</td>
                                     <td>資產負債表（權益／有形淨值）</td>
                                     <td>資產負債表（權益／投資科目）</td>
                                     <td>損益／現金流／營收；ARR 手動</td>
                                     <td>多部門營收（BB Lab）＋Cash／Debt</td>
                                   </tr>
                                   <tr>
                                     <td><b>投資人觀點</b></td>
                                     <td>小股東（配息請求權）</td>
                                     <td>控股／併購（造血能力）</td>
                                     <td>皆可（尤其負 FCF）</td>
                                     <td>金融／保險／帳面錨</td>
                                     <td>控股／綜合企業</td>
                                     <td>成長／無形資產交叉</td>
                                     <td>多事業部結構交叉</td>
                                   </tr>
                                   <tr>
                                     <td><b>典型適用</b></td>
                                     <td>成熟穩健配息</td>
                                     <td>成長／擴張、FCF 為正</td>
                                     <td>資產密集、FCF 不穩</td>
                                     <td>銀行、保險、REIT</td>
                                     <td>控股／集團帳面</td>
                                     <td>SaaS／平台／高成長</td>
                                     <td>控股／綜合多部門</td>
                                   </tr>
                                   <tr>
                                     <td><b>對配息依賴</b></td>
                                     <td><span class='label label-danger'>極高</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                     <td><span class='label label-success'>極低</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                     <td><span class='label label-success'>低</span></td>
                                   </tr>
                                   <tr>
                                     <td><b>典型限制</b></td>
                                     <td>不配息／配息波動大時失效</td>
                                     <td>FCF 長期為負或高度循環時難估</td>
                                     <td>帳面／ROE 失真時偏誤</td>
                                     <td>無形資產主導時失準</td>
                                     <td>非市場法 SOTP；無投資科目時＝權益</td>
                                     <td>倍數隨市場情緒；非 Fair Value</td>
                                     <td>需 ≥2 正值部門營收；非 EBIT SOTP</td>
                                   </tr>
                                 </tbody>
                               </table>")
  )
}

#' Collapsible Model Selector footnote: consideration dimensions (default closed).
#' Reuses `.ynow-notes` chrome; title class is NOT `.ynow-notes__title` so
#' applyUiLocale does not overwrite it with the generic Notes / 附註 string.
.model_selector_dimensions_annotation_ui <- function() {
  tags$details(
    id = "ynow_ms_dims_details",
    class = "ynow-notes ynow-ms-dims",
    tags$summary(
      class = "ynow-notes__summary",
      id = "ynow_ms_dims_summary",
      title = "Show or hide consideration dimensions",
      `aria-label` = "Show or hide consideration dimensions",
      tags$span(
        class = "ynow-ms-dims__title",
        id = "ynow_ms_dims_title",
        "考慮維度"
      )
    ),
    tags$div(
      class = "ynow-notes__body",
      .consideration_dimensions_table()
    )
  )
}

#' Valuation methodology guide (Decision Matrix + model tabs).
#' Mounted at the bottom of Basic Setup (Full only; Lite hides Basic Setup).
#' Title/lead use column(12); tabBox keeps width=12 (shinydashboard always emits col-sm-N).
.valuation_methodology_section_ui <- function(collapsible = TRUE, collapsed = FALSE) {
  # collapsible/collapsed kept for call-site compatibility (no outer box to collapse)
  tagList(
    fluidRow(
      column(
        width = 12,
        class = "ynow-about-section",
        h3(
          class = "ynow-about-section-title",
          tags$b(id = "ynow_method_section_title", "Valuation Methodology｜評價方法論")
        ),
        withMathJax(),
        p(
          class = "ynow-about-section-lead",
          id = "ynow_method_lead_zh",
          tags$b("先分類，再選模型；先推導，再校正；先給區間，再給單點。"),
          " Fair Value 引擎：DCF（FCFF／WACC、FCFE／Ke）、DDM（Gordon／SPM／二階段）、RI、P/B、NAV。",
          " Implied Price 相對引擎（僅交叉檢核、不作主模型）：Multiples、SOTP。",
          " 選對路徑與算對數字同等重要；以下說明流程紀律、適用場景與核心公式。"
        ),
        p(
          class = "ynow-about-section-lead",
          id = "ynow_method_lead_en",
          style = "margin-top: -6px;",
          tags$b("Classify before models; derive before calibrating; ranges before point targets. "),
          "Fair Value: DCF (FCFF/WACC, FCFE/Ke), DDM (Gordon / SPM / two-stage), RI, P/B, NAV. ",
          "Implied Price cross-checks only (never Fair Value primary): Multiples, SOTP. ",
          "Path selection matters as much as the arithmetic—process, fit, and formulas below."
        )
      )
    ),
    fluidRow(
      tabBox(
        title = tags$span(id = "ynow_method_tabbox_title", "Model Selection Guide"),
        width = 12,
        side = "left",

        # Tab: 方法論比較矩陣
        tabPanel(
          title = tags$span(id = "ynow_method_tab_matrix", "Decision Matrix"),
          icon = icon("table"),
          tags$p(
            id = "ynow_method_matrix_fv_note",
            class = "help-block",
            style = "margin:0 0 10px 0;",
            "Fair Value engines (Intrinsic Value / Fair Value primaries)."
          ),
          tags$div(
            style = "overflow-x: auto; margin-bottom: 18px;",
            HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #2C3E50; color: white;'>
                                   <tr>
                                     <th>對照項目</th>
                                     <th>DDM</th>
                                     <th>DCF</th>
                                     <th>RI</th>
                                     <th>P/B</th>
                                     <th>NAV</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>現金流／錨定</b></td>
                                     <td>每股股利 D（股權請求權）</td>
                                     <td>FCFF（企業）或 FCFE（股權）</td>
                                     <td>帳面淨值 + 超額盈餘</td>
                                     <td>BVPS／TBVPS／控股 NAVPS × 目標 P/B</td>
                                     <td>帳面控股淨資產 NAVPS × 倍數</td>
                                   </tr>
                                   <tr>
                                     <td><b>折現率／倍數</b></td>
                                     <td>Ke（CAPM）</td>
                                     <td>FCFF：WACC；FCFE：Ke</td>
                                     <td>Ke（CAPM）</td>
                                     <td>目標倍數（Justified／產業／歷史）</td>
                                     <td>折價／溢價倍數（非 Justified）</td>
                                   </tr>
                                   <tr>
                                     <td><b>成長率 g／年數</b></td>
                                     <td>股利永續 g；SPM 盈餘 g；二階段 g1／n1</td>
                                     <td>明確預測年數 n；終值 SGR（相對 WACC／Ke）</td>
                                     <td>RI 預測期；終值 g（相對 Ke）</td>
                                     <td>Justified 需 g；產業／歷史倍數不強制 SGR</td>
                                     <td>不涉及預測年數 n</td>
                                   </tr>
                                   <tr>
                                     <td><b>核心公式</b></td>
                                     <td>Gordon：P₀=D₁/(Ke−g)；SPM；二階段 ΣPV(D)+PV(TV)</td>
                                     <td>EV＝ΣPV(FCFF)+PV(TV) 或 Equity＝ΣPV(FCFE)+PV(TV)</td>
                                     <td>V₀=B₀+ΣPV(RI)+PV(TV_RI)</td>
                                     <td>P=(BVPS／TBVPS／NAVPS)×Target P/B</td>
                                     <td>P=NAVPS×NAV multiple</td>
                                   </tr>
                                   <tr>
                                     <td><b>輸出角色</b></td>
                                     <td>Fair Value 每股合理價</td>
                                     <td>Fair Value（FCFF 先 EV 再橋接）</td>
                                     <td>Fair Value 每股內在價值</td>
                                     <td>Fair Value 區間（Bear／Base／Bull）</td>
                                     <td>Fair Value（控股／資產錨）</td>
                                   </tr>
                                 </tbody>
                               </table>")
          ),
          tags$p(
            id = "ynow_method_matrix_rel_note",
            class = "help-block",
            style = "margin:0 0 10px 0;",
            "Implied Price relative engines — cross-check only; never Fair Value primary; not HFV overlays."
          ),
          tags$div(
            style = "overflow-x: auto; margin-bottom: 18px;",
            HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                 <thead style='background-color: #34495e; color: white;'>
                                   <tr>
                                     <th>對照項目</th>
                                     <th>Multiples（市場倍數）</th>
                                     <th>SOTP（分部加總）</th>
                                   </tr>
                                 </thead>
                                 <tbody>
                                   <tr>
                                     <td><b>錨定</b></td>
                                     <td>EPS／EBIT／EBITDA／FCFF／Revenue／ARR × 交易倍數</td>
                                     <td>≥2 部門營收 × 各部門 EV/Sales（＋非營業資產）</td>
                                   </tr>
                                   <tr>
                                     <td><b>橋接</b></td>
                                     <td>Enterprise：EV→Equity＝EV+Cash−Debt；P/S：股權面、無負債橋接</td>
                                     <td>Implied EV→Equity＝EV+Cash−Debt（同 DCF 橋接）</td>
                                   </tr>
                                   <tr>
                                     <td><b>成長／年數</b></td>
                                     <td>不涉及預測年數 n；PEG 用成長％作相對指標</td>
                                     <td>不涉及預測年數 n</td>
                                   </tr>
                                   <tr>
                                     <td><b>核心公式</b></td>
                                     <td>Implied Price＝EPS×P/E；或 Equity＝(Metric×Multiple+Cash−Debt)÷Shares</td>
                                     <td>Implied EV＝Σ(Seg Rev×EV/Sales)+Non-op；Price＝Equity÷Shares</td>
                                   </tr>
                                   <tr>
                                     <td><b>輸出角色</b></td>
                                     <td>Implied Price 交叉檢核（非 Intrinsic Value）</td>
                                     <td>結構型 Implied Price 交叉檢核（控股／綜合常配 NAV）</td>
                                   </tr>
                                 </tbody>
                               </table>")
          )
          # Consideration dimensions table lives under Model Selector as a
          # collapsed annotation (see .model_selector_dimensions_annotation_ui).
        ),

        # Tab: DDM 模型解說
        tabPanel(
          "Dividend Discount Model (DDM)",
          icon = icon("hand-holding-usd"),
          h4(tags$b("股利折現模型（Gordon／SPM／二階段）")),
          p("DDM 將普通股價值視為未來現金股利的現值。現金流是股利、折現率是 Ke，與以 FCFF／WACC 為核心的 DCF 屬不同層級。本 App 的永續成長家族含 Gordon Growth Model (GGM) 與 Sum of Perpetuities Method (SPM)。"),
          tags$ul(
            tags$li(
              tags$b("Gordon (GGM)："),
              tags$b("$$P_0 = \\frac{D_1}{K_e - g} = \\frac{D_0 \\times (1 + g)}{K_e - g}$$"),
              " 假設現金發放率維持固定比例，股利隨盈餘以 g 成長。約束：g < Ke。"
            ),
            tags$li(
              tags$b("SPM（永續和）："),
              tags$b("$$P_0 = \\frac{E \\times g}{K_e^2} + \\frac{D}{K_e}$$"),
              " Brown & Abraham (2012)：將「定額股利永續」與「保留盈餘創造之成長」分開折現；對輸入較不敏感。當 ROE = Ke 且 g = ROE × 保留率時，與前向股利版 GGM 等價。EPS 取 D0 分頁之預估／最新 EPS；D 取今年股利。"
            ),
            tags$li(tags$b("二階段："), "前 n₁ 年股利以 g₁ 成長，之後以永續 g₂ 做 Gordon 終值。約束：g₂ < Ke。"),
            tags$li(tags$b("$$D_t = D_0(1+g_1)^t,\\quad TV = \\frac{D_{n_1}(1+g_2)}{K_e-g_2},\\quad P_0 = \\sum_{t=1}^{n_1}\\frac{D_t}{(1+K_e)^t} + \\frac{TV}{(1+K_e)^{n_1}}$$"))
          ),
          p("股利成長率 g（二階段的 g₂；SPM 的盈餘成長 g）可與中央終值 SGR 同步，亦可在 DDM 分頁單獨覆寫。基本面法可參考 $$g = ROE \\times Retention\\ Ratio$$（或 ROA × RR），但不宜與 FCFF 終值 g 強制畫上等號。選擇 GGM 或 SPM 時，宜先觀察公司股利政策較接近「固定配息率」或「固定股利金額」。")
        ),

        # Tab: DCF 模型解說
        tabPanel(
          "Discounted Cash Flow (DCF)",
          icon = icon("money-bill-wave"),
          h4(tags$b("自由現金流折現模型 (FCFF／FCFE)")),
          p("DCF 關注企業造血能力。預設以 FCFF 用 WACC 折現得到企業價值（EV），再加現金、減負債橋接至股權價值後才除以流通股數。歷史現金流以 CFO＋稅後利息−|CapEx| 重建（Yahoo Free Cash Flow 為稅後、近 FCFE，不可直接配 WACC）。亦可切換 FCFE：將 FCFF 轉成股權現金流後以 Ke 折現，不再加減淨現金／負債。本 app 的「Gordon」模式為明確預測期加上 Gordon 終值，而非單期 EV = FCF₁/(WACC−g)。若末期 FCFF 為負則略過永續終值，並建議改用 RI。CapEx 暴衝平滑為可選工程啟發式（可調倍數閾值／均值年數）。"),
          tags$ul(
            tags$li(tags$b("$$FCFF = CFO + Interest\\times(1-T) - CapEx$$")),
            tags$li(tags$b("$$FCFF = NOPAT + D\\&A - \\Delta NWC - CapEx$$")),
            tags$li(tags$b("$$FCFE = FCFF - Interest\\times(1-T) + Net\\ Borrowing$$")),
            tags$li(tags$b("FCFF："), tags$b("$$Enterprise\\ Value = \\sum \\frac{FCFF_t}{(1+WACC)^t} + \\frac{TV}{(1+WACC)^n}$$")),
            tags$li(tags$b("FCFE："), tags$b("$$Equity = \\sum \\frac{FCFE_t}{(1+K_e)^t} + \\frac{TV}{(1+K_e)^n}$$")),
            tags$li(tags$b("$$Terminal\\ Value = \\frac{CF_n \\times (1 + g)}{r - g}$$"))
          ),
          p("Live 預測表以 NOPAT（EBIT×(1−T)）為錨，不是淨利 NI。FCFE 的淨舉債採固定槓桿近似：各年淨舉債＝g×期初負債。兩階段模式在高速成長期後將終值成長率收斂至 SGR；FCFF 約束 g < WACC，FCFE 約束 g < Ke。")
        ),

        # Tab: RI 模型解說
        tabPanel(
          "Residual Income (RI)",
          icon = icon("gem"),
          h4(tags$b("剩餘收益模型 (Residual Income)")),
          p("RI 以帳面淨值為起點，將「超過股權成本的盈餘」折現加總。適合 FCF 為負、但淨值與 ROE 具參考性的企業；折現率使用 Ke（與 DDM 同屬股權層級）。"),
          tags$ul(
            tags$li(tags$b("$$RI_t = (ROE_t - K_e) \\times B_{t-1}$$")),
            tags$li(tags$b("$$B_t = B_{t-1} + NI_t \\times (1 - Payout)$$")),
            tags$li(tags$b("$$V_0 = B_0 + \\sum_{t=1}^{n} \\frac{RI_t}{(1+K_e)^t} + \\frac{TV_{RI}}{(1+K_e)^n}$$")),
            tags$li(tags$b("$$TV_{RI} = \\frac{RI_n \\times (1 + g)}{K_e - g}$$"))
          ),
          p("本 app 可設定固定 ROE、線性淡化或產業 ROE；終值成長 g 須滿足 g < Ke。當 ROE < Ke 時，剩餘收益為負，代表價值減損。")
        ),

        # Tab: P/B 模型解說
        tabPanel(
          "Price-to-Book (P/B)",
          icon = icon("landmark"),
          h4(tags$b("本淨比／相對估值 (P/B)")),
          p("以每股帳面淨值、有形淨值或控股 NAVPS 乘上目標本淨比。目標倍數可來自產業／歷史（無需 SGR），或 Justified P/B（需 ROE、Ke、SGR／g）。純帳面 NAV（無倍數法 Justified）請用獨立「NAV」模型——兩者同屬資產／帳面家族，但路徑分開。"),
          tags$ul(
            tags$li(tags$b("$$BVPS = \\frac{Common\\ Equity}{Shares}$$")),
            tags$li(tags$b("$$TBVPS = \\frac{Common\\ Equity - Goodwill - Intangibles}{Shares}$$")),
            tags$li(tags$b("$$P = (BVPS\\ /\\ TBVPS\\ /\\ NAVPS) \\times Target\\ P/B$$")),
            tags$li(tags$b("Justified\\ P/B \\approx \\frac{ROE - g}{K_e - g}"))
          ),
          p("雙重股權／ADR 等「報價股數 ≠ 財報股數」時，與 DCF／RI／回測相同，一律自動約當股數（市值÷股價）。")
        ),
        tabPanel(
          "Net Asset Value (NAV)",
          icon = icon("sitemap"),
          h4(tags$b("純 NAV（帳面控股淨資產）")),
          p("評估「現在家底（存量）」：合理價 = NAVPS × 折價／溢價倍數。控股折價只套用在已辨識投資科目；無投資科目時 NAV＝帳面權益。此為帳面拆解，不是市場法分部 SOTP，亦不需要 Justified／SGR。"),
          tags$ul(
            tags$li(tags$b("$$NAV = Equity - Holdco\\ Discount \\times Identified\\ Investments$$")),
            tags$li(tags$b("$$P = NAVPS \\times NAV\\ Multiple$$"))
          )
        ),

        tabPanel(
          "Multiples",
          icon = icon("percentage"),
          h4(tags$b("市場倍數法（Implied Price）")),
          p("相對估值依資本請求權分為 Equity／Enterprise 兩族（非 Intrinsic Value／Fair Value 主模型）。不可與 DCF／DDM／RI／P/B／NAV 等權平均，亦不可作為 HFV 疊加。"),
          tags$ul(
            tags$li(tags$b("Equity："), tags$b("$$Implied\\ Price = EPS \\times P/E$$"), "；", tags$b("$$Implied\\ Equity = Revenue \\times P/S$$"), "；PEG = P/E ÷ g(%)（相對指標）。"),
            tags$li(tags$b("Enterprise："), tags$b("$$Implied\\ EV = Metric \\times Multiple$$"), "；", tags$b("$$Equity = EV + Cash - Debt$$"), "；Price = Equity ÷ Shares（與 SOTP 共用橋接；EV/FCF 用 FCFF）。"),
            tags$li(tags$b("SOTP（側欄）："), "Enterprise-structural — 部門營收 × EV/Sales 加總後再用同一 Cash−Debt 橋接。")
          ),
          p("共用假設：Shares；Enterprise／SOTP 另共用 Cash、Debt、EV/Sales 預設層級。獨特假設見 Multiples Overview 參數矩陣。")
        ),

        tabPanel(
          "SOTP",
          icon = icon("puzzle-piece"),
          h4(tags$b("分部加總 SOTP（Enterprise-structural Implied Price）")),
          p("側欄獨立引擎，與 Multiples→Enterprise 同屬企業價值請求權：拆解 ≥2 筆部門營收 × 各段 EV/Sales，加總後共用 Cash−Debt×Shares 橋接。控股／綜合常作 NAV 副模型；非 Equity 的 P/E／P/S，亦非部門 EBIT SOTP。"),
          tags$ul(
            tags$li(tags$b("$$Segment\\ EV = Segment\\ Revenue \\times EV/Sales$$")),
            tags$li(tags$b("$$Implied\\ EV = \\sum Segment\\ EV + Non\\text{-}operating$$")),
            tags$li(tags$b("$$Equity = Implied\\ EV + Cash - Debt$$"), "；", tags$b("$$Price = Equity \\div Shares$$"))
          ),
          p("共用：Cash／Debt／Shares、Default EV/Sales。獨特：部門營收、各段 EV/Sales、Non-operating assets。")
        )
      )
    )
  )
}

.dcf_core_params_box <- function() {
  # Chrome／欄寬對齊下方 BETA tabBox：左側單一頁籤 + 右側大標英文；同為 col-sm-12
  tabBox(
    title = "SUSTAINABLE GROWTH RATE",
    width = 12,
    tabPanel(
      "SGR",
      icon = icon("seedling"),
      # 與 BETA Overview 同款 valueBox 列：各 50% 並排
      fluidRow(
        valueBoxOutput("vbx_sgr_pct", width = 6),
        valueBoxOutput("vbx_session_g", width = 6)
      ),
      fluidRow(
        column(
          width = 6,
          tags$h5(tags$b(id = "ynow_sgr_method_title", "終值永續成長率 (SGR) 評價方法")),
          selectInput(
            "perpetual_g_method",
            NULL,
            choices = c(
              "總體經濟錨定（Macro）" = "macro",
              "基本面公式（Fundamental／SGR）" = "fundamental",
              "產業生命週期（Lifecycle）" = "lifecycle"
            ),
            selected = APP_DEFAULTS$perpetual_g_method
          )
        ),
        column(
          width = 6,
          tags$h5(tags$b(id = "ynow_lifecycle_stage_title", "生命週期檔位")),
          selectInput(
            "lifecycle_stage",
            NULL,
            choices = c(
              "自動偵測" = "auto",
              "High Growth" = "HIGH_GROWTH",
              "Growth-to-Mature" = "GROWTH_TO_MATURE",
              "Mature Growth" = "MATURE_GROWTH",
              "Mature Stable" = "MATURE_STABLE",
              "Declining / Finite Life" = "DECLINING_OR_FINITE_LIFE",
              "Regulated Utility" = "REGULATED_UTILITY",
              "Financial Institution" = "FINANCIAL_INSTITUTION"
            ),
            selected = APP_DEFAULTS$lifecycle_stage
          ),
          helpText(
            id = "ynow_lifecycle_stage_help",
            "Auto-detect 依多因子評分；手動選擇不會被覆寫。選項不含固定終值 g。"
          )
        )
      ),
      helpText(
        id = "ynow_sgr_method_help",
        "Macro：採用即時擷取的市場 10 年期公債 Rf（美股 Yahoo ^TNX；台股櫃買 TPEx 公債殖利率曲線 10 年期；失敗則最近成功值，再失敗才工程 fallback 並標明）。",
        "Fundamental：Retention×ROE（僅適合成熟穩健企業）。",
        "Lifecycle：以經濟錨定估計終值 g，生命週期檔位不寫入固定百分比。"
      ),
      uiOutput("txt_perpetual_g_method_suggest"),
      uiOutput("txt_lifecycle_classification"),
      tags$h5(tags$b("估計依據")),
      uiOutput("txt_perpetual_g_reason"),
      tags$hr(style = "margin: 12px 0;"),
      numericInput(
        "sgr",
        "自訂 SGR (%)",
        value = APP_DEFAULTS$sgr
      )
    )
  )
}

.dcf_two_stage_params_box <- function() {
  conditionalPanel(
    condition = "input.dcf_mode == 'two_stage'",
    box(
      title = tagList(icon("layer-group"), "兩階段成長假設（DCF）"),
      width = 12, status = "warning", solidHeader = TRUE,
      tags$p(style = "margin: 0 0 6px 0; font-size: 12.5px; color: #555;", tags$b("第一階段｜高速成長")),
      numericInput("yr_stage1", "年數", value = APP_DEFAULTS$yr_stage1),
      numericInput("g_stage1", "成長率 g1 (%)", value = APP_DEFAULTS$g_stage1),
      helpText(
        id = "ynow_g_stage1_help",
        "預設帶入「預估營收成長率」；可手動覆寫。終值成長率仍用基礎設定的 SGR。"
      ),
      conditionalPanel(
        condition = "input.dcf_claim != 'fcfe'",
        numericInput("wacc_stage1", "折現率 WACC1 (%)", value = APP_DEFAULTS$wacc_stage1, step = 0.01)
      ),
      tags$p(style = "margin: 10px 0 6px 0; font-size: 12.5px; color: #555;", tags$b("第二階段｜永續成長")),
      helpText("第二階段成長率採用基礎設定的 SGR；以下設定折現率。"),
      conditionalPanel(
        condition = "input.dcf_claim != 'fcfe'",
        numericInput("wacc_stage2", "折現率 WACC2 (%)", value = APP_DEFAULTS$wacc_stage2, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.dcf_claim == 'fcfe'",
        helpText("FCFE 模式以 Ke（WACC 分頁之 rₑ／CAPM）折現，不再分 WACC1／WACC2。兩階段成長率仍適用；終值約束 g < Ke。")
      )
    )
  )
}

.ddm_two_stage_params_box <- function() {
  conditionalPanel(
    condition = "input['mod_ddm-ddm_mode'] == 'two_stage'",
    box(
      title = tagList(icon("layer-group"), "兩階段成長假設（DDM）"),
      width = 12, status = "warning", solidHeader = TRUE,
      tags$p(style = "margin: 0 0 6px 0; font-size: 12.5px; color: #555;", tags$b("第一階段｜高速成長")),
      numericInput(
        "mod_ddm-yr_stage1", "年數 n",
        value = APP_DEFAULTS$ddm_yr_stage1, min = 1, max = 30, step = 1
      ),
      numericInput(
        "mod_ddm-g_stage1", "成長率 g1 (%)",
        value = APP_DEFAULTS$ddm_g_stage1, step = 0.1
      ),
      tags$p(style = "margin: 10px 0 6px 0; font-size: 12.5px; color: #555;", tags$b("第二階段｜永續成長")),
      helpText(id = "ynow_ddm_two_stage_help", "第二階段股利成長率採用 Overview 的永續 g（g₂）；折現率 r＝Ke（CAPM）。終值約束：g₂ < r。")
    )
  )
}

.ddm_formula_banner <- function() {
  tagList(
    conditionalPanel(
      condition = "input['mod_ddm-ddm_mode'] == 'gordon' || input['mod_ddm-ddm_mode'] == null || input['mod_ddm-ddm_mode'] == ''",
      div(
        id = "ynow_ddm_formula_gordon",
        "V₀ = D₁ / (r − g)　｜　D₁ = D₀ × (1 + g)　｜　r = Ke (CAPM)",
        style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
      )
    ),
    conditionalPanel(
      condition = "input['mod_ddm-ddm_mode'] == 'spm'",
      div(
        id = "ynow_ddm_formula_spm",
        "V₀ = (E × g) / r² + D / r　｜　r = Ke (CAPM)　｜　SPM",
        style = "font-size: 16px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
      )
    ),
    conditionalPanel(
      condition = "input['mod_ddm-ddm_mode'] == 'two_stage'",
      div(
        id = "ynow_ddm_formula_two_stage",
        "V₀ = Σ Dₜ / (1+r)ᵗ + Pₙ / (1+r)ⁿ　｜　Pₙ = Dₙ × (1+g₂) / (r − g₂)",
        style = "font-size: 16px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
      )
    )
  )
}

#' Shared bottom block: formula-param contribution / relative ±1% elasticity.
.model_param_sensitivity_box <- function(title, table_id) {
  fluidRow(
    box(
      title = tagList(icon("percentage"), title),
      width = 12, status = "info", solidHeader = TRUE,
      collapsible = TRUE, collapsed = TRUE,
      tags$div(
        style = "overflow-x:auto;",
        tableOutput(table_id)
      ),
      tags$div(
        class = "help-block",
        style = "margin:12px 0 0 0; font-size:12px; line-height:1.55; color:#737373;",
        tags$p(style = "margin:0 0 4px 0; font-weight:600; color:#555;", "註解"),
        tags$ul(
          style = "margin:0; padding-left:1.25em;",
          tags$li("「｜ε｜」由估值公式在目前模型參數（WACC／Ke、g、n、We／Wd、配息、FCF 佔比等）上評估，與個股價格、股數、現金／負債規模無關；兩檔股票只要公式參數相同，｜ε｜與排序即相同。"),
          tags$li("g 與折現率的彈性本身取決於 (r−g)，故改參數才會改｜ε｜。"),
          tags$li("一次只變動一個參數（相對 ±1%）；整數年數 n 以 ±1 年換算成 1% 等價彈性。"),
          tags$li("「估值Δ%」≈ 該參數變動 1% 時公式價值的變動幅度；「｜ε｜」= 兩側 |Δ估值%| 平均。"),
          tags$li("水準參數（FCFF／FCFE／D0／B0／BVPS／目標 P/B）在公式為線性時｜ε｜≈1。")
        )
      )
    )
  )
}

# ---- Lazy page bodies (mounted once via uiOutput hosts) ----

#' Lazy page body for tab `macro_market` (mounted once per session).
.ynow_page_ui_macro_market <- function() {
  tagList(
  macro_market_ui("macro")
  )
}

#' Lazy page body for tab `bluechip` (mounted once per session).
.ynow_page_ui_bluechip <- function() {
  tagList(
  # Shared pool controls above BLUE CHIP (semantic: truncate rule → then take N)
          # Concept groups appear beside truncate when mode is concept.
          fluidRow(
            column(
              width = 12,
              class = "ynow-lab-im-pool-controls",
              style = "margin: 0 0 14px 0;",
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    "lab_im_pool_rank",
                    tags$span(id = "ynow_lab_im_pool_rank_label", "候選截斷邏輯"),
                    choices = lab_im_pool_rank_choices(),
                    selected = APP_DEFAULTS$lab_im_pool_rank,
                    width = "100%"
                  )
                ),
                conditionalPanel(
                  condition = "input.lab_im_pool_rank == 'concept'",
                  class = "col-sm-4",
                  selectizeInput(
                    "lab_im_concepts",
                    tags$span(id = "ynow_lab_im_concepts_label", "概念股群"),
                    choices = lab_concept_group_choices("US", "zh-TW"),
                    selected = APP_DEFAULTS$lab_im_concepts,
                    multiple = TRUE,
                    options = list(
                      placeholder = "選擇一或多個概念股群…",
                      plugins = list("remove_button")
                    ),
                    width = "100%"
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    "lab_im_max_n",
                    tags$span(id = "ynow_lab_im_max_n_label", "宇宙檔數（N）"),
                    choices = lab_im_max_n_select_choices(),
                    selected = APP_DEFAULTS$lab_im_max_n,
                    width = "100%"
                  ),
                  conditionalPanel(
                    condition = "input.lab_im_max_n == 'custom'",
                    numericInput(
                      "lab_im_max_n_custom",
                      tags$span(id = "ynow_lab_im_max_n_custom_label", "自訂檔數"),
                      value = 25,
                      min = 1,
                      max = 500,
                      step = 1,
                      width = "100%"
                    )
                  )
                )
              )
            )
          ),
          tabBox(
            title = "BLUE CHIP",
            id = "bluechip_im_report",
            width = "auto",

            tabPanel(
              title = "排行",
              value = "im_filters",
              icon = icon("trophy"),
              uiOutput("lab_im_bluechip_blurb"),
              tags$hr(),
              fluidRow(
                column(
                  width = 12,
                  tags$div(
                    class = "ynow-lab-im-lb-scope",
                    style = "margin:0 0 10px 0;",
                    radioButtons(
                      "lab_im_lb_mode",
                      tags$span(id = "ynow_lab_im_lb_mode_label", "排行視角"),
                      choiceNames = list(
                        tags$span(id = "ynow_lab_im_lb_mode_overall", "整體前十名"),
                        tags$span(id = "ynow_lab_im_lb_mode_by_ind", "選定產業前十名"),
                        tags$span(id = "ynow_lab_im_lb_mode_ind_avg", "產業市值加權漲幅")
                      ),
                      choiceValues = list("overall", "by_industry", "industry_avg"),
                      selected = APP_DEFAULTS$lab_im_lb_mode,
                      inline = TRUE
                    ),
                    tags$div(
                      id = "ynow_lab_im_lb_scope_help",
                      style = "color:#888; font-size:12px; line-height:1.45; margin:-4px 0 8px 0;",
                      paste0(
                        "整體前十名：跨本次已評估產業依年化估值漲幅取 Top 10，並顯示產業欄。",
                        "選定產業前十名：所選產業內單一 Top 10；產業內排名自該產業第 1 名起算（非跨產業流水號）。",
                        "產業市值加權漲幅：各產業以 Σ(市值×年化估值漲幅)/Σ(市值) 排名（每元市值加權）。",
                        "前十名只從合格者取最多 10 檔；合格不足 10 時不會湊滿。"
                      )
                    )
                  ),
                  uiOutput("lab_im_leader_note"),
                  uiOutput("lab_im_leaderboard_status"),
                  tableOutput("lab_im_leaderboard")
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = "查詢條件",
                  tags$div(
                    class = "ynow-lab-im-universe-row",
                    uiOutput("lab_im_universe_meta"),
                    actionButton(
                      "lab_im_refresh_universe", "更新名單",
                      icon = icon("sync"),
                      class = "btn-default btn-sm",
                      title = "重新擷取目前市場的成分／上市／上櫃／興櫃名單"
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      class = "ynow-full-only",
                      shinyWidgets::pickerInput(
                        "lab_im_industries", "產業",
                        choices = lab_industry_picker_choices(),
                        selected = unname(lab_industry_picker_choices()),
                        multiple = TRUE,
                        width = "100%",
                        options = shinyWidgets::pickerOptions(
                          actionsBox = TRUE,
                          liveSearch = TRUE,
                          selectedTextFormat = "count > 2",
                          noneSelectedText = "全部產業",
                          size = 10,
                          container = "body",
                          dropupAuto = TRUE
                        )
                      ),
                      tags$div(
                        class = "ynow-lab-im-boards ynow-tw-only",
                        style = "margin:8px 0 4px 0;",
                        checkboxGroupInput(
                          "lab_im_boards",
                          tags$span(id = "ynow_lab_im_boards_label", "板別"),
                          choiceNames = list(
                            tags$span(id = "ynow_lab_im_board_twse", "上市"),
                            tags$span(id = "ynow_lab_im_board_tpex", "上櫃"),
                            tags$span(id = "ynow_lab_im_board_esb", "興櫃")
                          ),
                          choiceValues = list("TWSE", "TPEX", "ESB"),
                          selected = APP_DEFAULTS$lab_im_boards,
                          inline = TRUE
                        ),
                        tags$span(
                          id = "ynow_lab_im_boards_hint",
                          class = "ynow-lab-im-quality-hint ynow-full-only",
                          style = "display:block; color:#888; font-size:12px; margin-top:-4px;",
                          "複選上市／上櫃／興櫃；預設上市＋上櫃。興櫃資料覆蓋較不穩，勾選後才納入績優評估池。"
                        )
                      ),
                      tags$div(
                        class = "ynow-lab-im-methods",
                        checkboxGroupInput(
                          "lab_im_methods",
                          tags$span(id = "ynow_lab_im_methods_label", "模型"),
                          # Same top→bottom order as sidebar valuation menus
                          choices = c(
                            "NAV" = "nav",
                            "DCF" = "dcf",
                            "DDM" = "ddm",
                            "RI" = "ri",
                            "P/B" = "pb",
                            "Multiples" = "multiples",
                            "SOTP" = "sotp"
                          ),
                          selected = APP_DEFAULTS$lab_im_methods,
                          inline = TRUE
                        )
                      )
                    ),
                    column(
                      width = 6,
                      class = "ynow-lab-im-filter-col",
                      # 盈餘品質｜含 ADR 同列並排（Lite／Full 皆同；含 ADR 僅美股）
                      tags$div(
                        class = "row ynow-lab-im-eq-adr-row",
                        column(
                          width = 6,
                          class = "ynow-lab-im-eq-col",
                          tags$div(
                            class = "ynow-lab-im-quality",
                            checkboxInput(
                              "lab_im_eq_only",
                              tags$span(id = "ynow_lab_im_eq_label", "盈餘品質"),
                              value = isTRUE(APP_DEFAULTS$lab_im_eq_only)
                            ),
                            tags$span(
                              id = "ynow_lab_im_eq_hint",
                              class = "ynow-lab-im-quality-hint ynow-full-only",
                              paste0(
                                "預設勾選：排行榜／明細只列盈餘品質通過者；",
                                "取消勾選則不過濾。合格不足 N 時不湊滿。"
                              )
                            )
                          )
                        ),
                        column(
                          width = 6,
                          class = "ynow-lab-im-include-adr ynow-us-only",
                          tags$div(
                            class = "ynow-lab-im-quality",
                            checkboxInput(
                              "lab_im_include_adr",
                              tags$span(id = "ynow_lab_im_include_adr_label", "含 ADR"),
                              value = isTRUE(APP_DEFAULTS$lab_im_include_adr)
                            ),
                            tags$span(
                              id = "ynow_lab_im_include_adr_hint",
                              class = "ynow-lab-im-quality-hint ynow-full-only",
                              paste0(
                                "預設勾選：評估池含美股上市 ADR／外國發行人；",
                                "取消勾選則排除 ADR 後再套用候選截斷與宇宙檔數 N。"
                              )
                            )
                          )
                        )
                      ),
                      tags$div(
                        class = "ynow-lab-im-quality ynow-full-only",
                        style = "margin-top:12px;",
                        checkboxInput(
                          "lab_im_gate_only",
                          tags$span(id = "ynow_lab_im_gate_label", "Piotroski 高門檻"),
                          value = isTRUE(APP_DEFAULTS$lab_im_gate_only)
                        ),
                        tags$span(
                          id = "ynow_lab_im_gate_hint",
                          class = "ynow-lab-im-quality-hint",
                          "預設勾選：前十名與明細只列 Piotroski F-Score≥7（品質檢核）者；取消勾選則不設 F-Score 門檻。合格不足 N 或不足 10 時不會湊滿。"
                        )
                      )
                    )
                  ),
                  tags$div(
                    class = "ynow-lab-im-method-summary ynow-full-only",
                    style = "margin: 0 0 12px 0;",
                    tableOutput("lab_im_summary")
                  ),
                  tags$div(
                    class = "ynow-lab-im-actions",
                    actionButton(
                      "lab_im_run_fscore", "搜尋績優股",
                      icon = icon("chart-line"),
                      class = "btn-success",
                      title = "Piotroski 高門檻（F-Score≥7）＋年化估值漲幅排序"
                    ),
                    downloadButton(
                      "lab_im_download_report", "下載本頁報告",
                      icon = icon("download"),
                      class = "btn btn-default",
                      title = "下載目前篩選、摘要與明細（不必重新評估）"
                    )
                  ),
                  tags$p(
                    style = "margin-top:10px; color:#888; font-size:12px;",
                    "提示：評估需逐檔抓 Yahoo 財報與估值，檔數愈多愈久。"
                  )
                )
              ),
              # Lite：查詢條件區塊外正下方 — 盈餘品質說明
              tags$div(
                id = "ynow_lab_im_eq_explain",
                class = "ynow-lab-im-eq-explain ynow-lite-only",
                tags$b(id = "ynow_lab_im_eq_explain_title", "盈餘品質："),
                tags$span(
                  id = "ynow_lab_im_eq_explain_body",
                  paste0(
                    "預設勾選時，排行榜只列通過盈餘品質檢核者（OCF 與營運獲利交叉比對，",
                    "協助排除現金流與帳面獲利落差過大的標的）；取消勾選則不過濾。",
                    "合格不足 N 時不湊滿。"
                  )
                )
              ),
              # Detail table at bottom of Rankings (Detail sub-tab removed)
              tags$hr(style = "margin: 18px 0 12px 0; border-color: #e5e8eb;"),
              p(
                id = "ynow_lab_im_detail_intro",
                "本次已評估檔的明細（按年化估值漲幅排序）。宇宙檔數（N）＝分析後最終顯示上限：候選截斷與評分後，最多顯示 N 檔合格列；條件不足時不湊滿。"
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = tags$span(
                    id = "ynow_lab_im_detail_box_title",
                    "明細（按年化估值漲幅排序）"
                  ),
                  DT::dataTableOutput("lab_im_table") %>% shinycssloaders::withSpinner()
                )
              )
            ),

            tabPanel(
              title = "分群",
              value = "im_cluster",
              icon = icon("project-diagram"),
              tags$p(
                id = "ynow_lab_cluster_blurb",
                paste0(
                  "研究用分群 Lab：僅以比率／成長率做 K-Means（不把金額放入模型），",
                  "降低公司規模對距離的干擾。語意標籤為描述性啟發式，非買進／賣出訊號。"
                )
              ),
              tags$p(
                id = "ynow_lab_cluster_disclaimer",
                style = "color:#a94442; font-size:12px; margin-top:-6px;",
                "僅供研究／教育，非投資建議，亦非買進訊號。"
              ),
              tags$hr(),
              fluidRow(
                column(
                  width = 2,
                  numericInput(
                    "lab_cluster_k",
                    tags$span(id = "ynow_lab_cluster_k_label", "群數（k）"),
                    value = APP_DEFAULTS$lab_cluster_k, min = 2, max = 8, step = 1, width = "100%"
                  )
                ),
                column(
                  width = 2,
                  selectInput(
                    "lab_cluster_x",
                    tags$span(id = "ynow_lab_cluster_x_label", "散點 X"),
                    choices = c(
                      "ROE" = "ROE",
                      "Operating Margin" = "Operating_Margin",
                      "Rev YoY" = "Rev_YoY",
                      "OpInc YoY" = "OpInc_YoY",
                      "Debt Ratio" = "Debt_Ratio",
                      "Trailing P/E" = "PE_Ratio",
                      "P/B" = "PB_Ratio"
                    ),
                    selected = APP_DEFAULTS$lab_cluster_x,
                    width = "100%"
                  )
                ),
                column(
                  width = 2,
                  selectInput(
                    "lab_cluster_y",
                    tags$span(id = "ynow_lab_cluster_y_label", "散點 Y"),
                    choices = c(
                      "ROE" = "ROE",
                      "Operating Margin" = "Operating_Margin",
                      "Rev YoY" = "Rev_YoY",
                      "OpInc YoY" = "OpInc_YoY",
                      "Debt Ratio" = "Debt_Ratio",
                      "Trailing P/E" = "PE_Ratio",
                      "P/B" = "PB_Ratio"
                    ),
                    selected = APP_DEFAULTS$lab_cluster_y,
                    width = "100%"
                  )
                ),
                column(
                  width = 3,
                  tags$div(
                    class = "ynow-sc-wrap ynow-ticker-typeahead",
                    style = "width: 100%; max-width: none;",
                    textInput(
                      "lab_cluster_focus",
                      tags$span(id = "ynow_lab_cluster_focus_label", "Radar focus ticker"),
                      value = "",
                      width = "100%",
                      placeholder = "e.g. AAPL / 2330"
                    ),
                    uiOutput("lab_cluster_focus_suggest_ui")
                  )
                ),
                column(
                  width = 3,
                  tags$div(
                    style = "margin-top: 24px;",
                    actionButton(
                      "lab_cluster_run",
                      tags$span(id = "ynow_lab_cluster_run_label", "執行分群"),
                      icon = icon("object-ungroup"),
                      class = "btn-success"
                    )
                  )
                )
              ),
              tags$p(
                id = "ynow_lab_cluster_hint",
                style = "color:#888; font-size:12px;",
                paste0(
                  "沿用「排行」頁目前的產業／模型篩選（若有）。",
                  "流程：宇宙池先依「候選截斷邏輯」全市排序／篩選（市值／概念股／近一年漲幅／隨機），",
                  "再依所選「宇宙檔數（N）」作分群分析（非固定預設檔數）。",
                  "Search 後的代號一律強制納入宇宙（N），並作為雷達焦點預設。",
                  "擷取 Yahoo 比率特徵；若 Yahoo 受限則改用內建離線快照。"
                )
              ),
              fluidRow(
                box(
                  width = 7, status = "primary", solidHeader = TRUE,
                  title = tags$span(id = "ynow_lab_cluster_map_title", "分群星團圖"),
                  # renderUI destroys plotlyOutput when idle (empty plotly leaves stale widgets)
                  uiOutput("lab_cluster_scatter_ui")
                ),
                box(
                  width = 5, status = "info", solidHeader = TRUE,
                  title = tags$span(id = "ynow_lab_cluster_radar_title", "同群雷達圖"),
                  uiOutput("lab_cluster_radar_ui")
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tags$span(id = "ynow_lab_cluster_table_title", "分群結果"),
                  uiOutput("lab_cluster_table_ui")
                )
              )
            )
          )
  )
}

#' Lazy page body for tab `hfv` (mounted once per session).
.ynow_page_ui_hfv <- function() {
  tagList(
  withMathJax(),
          tags$div(
            class = "ynow-hfv-report",

            # --- Masthead ---
            tags$div(
              class = "ynow-hfv-report__masthead",
              h2(tags$b(id = "ynow_hfv_page_title", "Historical Fundamental Validation")),
              p(
                id = "ynow_hfv_page_sub",
                class = "ynow-hfv-report__lead",
                paste0(
                  "A point-in-time review of theoretical fair value versus market price: ",
                  "next-period odds, position vs FV, and historical scenario taxonomy. ",
                  "This is a validation report—not a trading backtest (see Quant Backtest Lab)."
                )
              )
            ),

            # --- How to read (top of page, collapsed by default) ---
            box(
              title = tagList(
                icon("book-open"),
                tags$span(id = "ynow_hfv_sec_method", "How to read this report")
              ),
              width = NULL,
              status = "primary",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              tags$div(
                class = "ynow-hfv-method",
                tags$p(
                  id = "ynow_hfv_method_body",
                  style = "font-size:12.5px;color:#444;line-height:1.55;margin:0 0 8px 0;",
                  paste0(
                    "Validation report — not a trading backtest. ",
                    "Chart Overlay Models (multi) drive chart FV lines and scenario A–D ",
                    "(average FV when several; show only with an A–D conclusion). ",
                    "Replay Model (single) drives odds, tip P(up), MOS, gap-to-FV, and the pair table."
                  )
                ),
                tags$p(
                  id = "ynow_hfv_sum_scenario_matrix",
                  style = "font-size:12px;color:#555;line-height:1.5;margin:0 0 6px 0;",
                  paste0(
                    "A Golden pit: FV↑, Price↓, Price ≪ FV · ",
                    "B Davis double: FV↑, Price↑, Price ≈ FV · ",
                    "C Value trap: FV↓, Price↓, Price < FV · ",
                    "D Bubble hype: FV≤flat, Price strong↑, Price ≫ FV · ",
                    "other = unmatched."
                  )
                ),
                tags$p(
                  id = "ynow_hfv_scenario_thresh_note",
                  style = "font-size:11.5px;color:#666;line-height:1.45;margin:0 0 4px 0;",
                  paste0(
                    "Scenario bands (engineering defaults): flat |Δ|/prev ≤ 2%; |MOS| ≤ 10% ≈ FV; ",
                    "MOS ≥ 20% ≪ FV; MOS ≤ −20% ≫ FV; D also needs price momentum ≥ +5%."
                  )
                ),
                tags$p(
                  style = "font-size:11.5px;color:#888;line-height:1.45;margin:0;",
                  id = "ynow_hfv_method_data_note",
                  paste0(
                    "Data: Yahoo annuals may be restated; PIT = period_end + ~90d (no soft bypass). ",
                    "g ≠ terminal SGR; missing CapEx/ΔNWC not invented as 0. ",
                    "TW OTC/ESB may use TPEx IS/BS fill (CF never invented). n<5 illustrative only."
                  )
                )
              )
            ),

            # Page-level common control (under How-to-read): analysis frequency
            tags$div(
              class = "ynow-hfv-page-controls",
              role = "group",
              `aria-label` = "HFV page controls",
              id = "ynow_hfv_page_controls",
              tags$div(
                class = "ynow-hfv-page-controls__freq",
                uiOutput("bt_fv_analysis_freq_ui")
              )
            ),

            # --- Chapter I: FV vs market ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch1_kicker", "Section I"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch1_title",
                  "Fair value vs market price"
                )
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                uiOutput("bt_valuation_summary"),
                # Chart overlay controls sit directly above the FV vs price chart
                tags$div(
                  class = "ynow-hfv-toolbar ynow-hfv-toolbar--above-chart",
                  role = "group",
                  `aria-label` = "HFV chart overlay controls",
                  id = "ynow_hfv_chart_overlay_controls",
                  tags$div(
                    class = "ynow-hfv-toolbar__group ynow-hfv-toolbar__group--wide",
                    checkboxGroupInput(
                      "bt_fv_models",
                      "Chart overlay models",
                      inline = TRUE,
                      choices = c(
                        "DCF" = "dcf",
                        "DDM" = "ddm",
                        "RI" = "ri",
                        "P/B" = "pb",
                        "NAV" = "nav"
                      ),
                      selected = APP_DEFAULTS$bt_fv_models
                    ),
                    tags$p(
                      id = "ynow_hfv_rel_models_note",
                      class = "help-block",
                      style = "margin:4px 0 0 0; font-size:12px;",
                      "Multiples / SOTP are Implied Price engines (not historical Fair Value) — they are not available as HFV chart overlays."
                    ),
                    checkboxInput(
                      "bt_hfv_show_bench",
                      tags$span(id = "ynow_hfv_show_bench_label", "Show benchmark"),
                      value = isTRUE(APP_DEFAULTS$bt_hfv_show_bench)
                    ),
                    tags$p(
                      id = "ynow_hfv_overlay_vs_replay_note",
                      class = "help-block ynow-hfv-overlay-replay-note",
                      paste0(
                        "Overlay (multi): chart FV lines + scenario A–D (average FV when several). ",
                        "Replay (Section II, single): odds / tip P(up) / MOS / gap / pair table."
                      )
                    )
                  )
                ),
                plotlyOutput("bt_hfv_timeline", height = "420px") %>% withSpinner(),
                # Scenarios under chart; Overlay-only; hidden until A–D conclusion
                uiOutput("bt_hfv_scenario_findings"),
                uiOutput("bt_session_params")
              )
            ),

            # --- Chapter II: Investor summary ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch2_kicker", "Section II"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch2_title",
                  "Investor summary"
                )
              ),
              tags$p(
                class = "ynow-hfv-chapter__lead",
                id = "ynow_hfv_ch2_lead",
                "Set Replay model and sample scope, then read the sample snapshot and tip market-price odds."
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                tags$div(
                  class = "ynow-hfv-toolbar ynow-hfv-toolbar--in-ch2",
                  role = "group",
                  `aria-label` = "HFV validation controls",
                  id = "ynow_hfv_toolbar",
                  tags$div(
                    class = "ynow-hfv-toolbar__group",
                    radioButtons(
                      "bt_fv_replay_model",
                      "Replay model",
                      inline = TRUE,
                      choices = c(
                        "DCF" = "dcf",
                        "DDM" = "ddm",
                        "RI" = "ri",
                        "P/B" = "pb",
                        "NAV" = "nav"
                      ),
                      selected = APP_DEFAULTS$bt_fv_replay_model
                    )
                  ),
                  tags$div(
                    class = "ynow-hfv-toolbar__group",
                    radioButtons(
                      "bt_fv_conv_window",
                      "Sample window",
                      inline = TRUE,
                      choices = c(
                        "All" = "all",
                        "1Y" = "1y",
                        "3Y" = "3y",
                        "5Y" = "5y",
                        "Custom" = "custom"
                      ),
                      selected = APP_DEFAULTS$bt_fv_conv_window
                    ),
                    conditionalPanel(
                      condition = "input.bt_fv_conv_window == 'custom'",
                      dateRangeInput(
                        "bt_fv_conv_custom",
                        NULL,
                        start = Sys.Date() - 365 * 3,
                        end = Sys.Date(),
                        language = "zh-TW"
                      )
                    )
                  ),
                  tags$div(
                    class = "ynow-hfv-toolbar__group ynow-hfv-toolbar__group--wide",
                    radioButtons(
                      "bt_fv_oos_mode",
                      "Validation sample scope",
                      inline = TRUE,
                      choices = c(
                        "Realized next period only (default)" = "realized",
                        "Expanding-window OOS hit rates" = "expanding",
                        "Include unrealized next period (in-sample)" = "insample"
                      ),
                      selected = APP_DEFAULTS$bt_fv_oos_mode
                    )
                  )
                ),
                tags$div(
                  class = "ynow-hfv-findings-block",
                  # Locale target kept for applyUiLocale; chapter II title already names this block.
                  tags$span(id = "ynow_hfv_sec_results", style = "display:none;", "Sample snapshot"),
                  uiOutput("bt_hfv_investor_summary")
                )
              )
            ),

            # --- Chapter III: Market price & MOS ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch3_kicker", "Section III"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch3_title",
                  "Next-period market price & MOS"
                )
              ),
              tags$p(
                class = "ynow-hfv-chapter__lead",
                id = "ynow_hfv_ch3_lead",
                "Did market price rise or fall between valuation dates? MOS outlook conditions tip odds (causal expanding-window)."
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                uiOutput("bt_hfv_price_findings")
              )
            ),

            # --- Chapter IV: Gap to FV ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch4_kicker", "Section IV"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch4_title",
                  "Gap to theoretical FV"
                )
              ),
              tags$p(
                class = "ynow-hfv-chapter__lead",
                id = "ynow_hfv_ch4_lead",
                "Did |P−FV_t| shrink or expand when P_next arrived? Chart = landing (P_next − FV) / FV."
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                uiOutput("bt_hfv_fv_findings"),
                tags$div(
                  class = "ynow-hfv-findings-block",
                  style = "margin-top:14px;",
                  # Locale target; chapter IV lead already names the chart.
                  tags$span(id = "ynow_hfv_chart_gap", style = "display:none;", "Landing magnitude (P_next − FV) / FV"),
                  plotlyOutput("bt_fv_conv_plot", height = "280px") %>% withSpinner()
                )
              )
            ),

            # --- Chapter V: Period detail appendix (scenarios live under Ch1 chart) ---
            tags$section(
              class = "ynow-hfv-chapter",
              tags$div(
                class = "ynow-hfv-chapter__head",
                tags$span(class = "ynow-hfv-chapter__kicker", id = "ynow_hfv_ch5_kicker", "Section V"),
                tags$h3(
                  class = "ynow-hfv-chapter__title",
                  id = "ynow_hfv_ch5_title",
                  "Period detail (appendix)"
                )
              ),
              tags$p(
                class = "ynow-hfv-chapter__lead",
                id = "ynow_hfv_ch5_lead",
                "Pair-by-pair outcomes for audit — not a buy/sell score."
              ),
              tags$div(
                class = "ynow-hfv-chapter__body",
                tags$div(
                  class = "ynow-hfv-findings-block",
                  tags$span(id = "ynow_hfv_table_detail", style = "display:none;", "Pair table"),
                  tags$div(
                    style = "overflow-x:auto; width:100%;",
                    tags$style(HTML("#bt_fv_conv_table table { width: 100% !important; }")),
                    tableOutput("bt_fv_conv_table")
                  )
                )
              )
            ),

            # --- Appendix: param inventory ---
            box(
              title = tagList(
                icon("table"),
                tags$span(id = "ynow_hfv_param_inv_title", "US Valuation Replay Inventory (Live vs Hist PIT)")
              ),
              width = NULL,
              status = "primary",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              tags$p(
                id = "ynow_hfv_param_inv_help",
                style = "font-size:12.5px;color:#444;line-height:1.55;",
                "Historical theoretical values are rebuilt from then-available data; ",
                tags$b("hist DCF prefers NOPAT / D&A / CapEx / ΔNWC margin path"),
                ", else falls back to Gordon geometry ", tags$code("FCF0×(1+g)^t"), "; ",
                tags$b("not"),
                " the Live DCF page revenue→NOPAT / CapEx / ΔNWC forecast table."
              ),
              tags$div(style = "overflow-x:auto;", tableOutput("bt_param_inventory"))
            )
          )
  )
}

#' Lazy page body for tab `lab_notes` (mounted once per session).
.ynow_page_ui_lab_notes <- function() {
  tagList(
  withMathJax(),
          tags$div(
            class = "ynow-backtest-report",

            # --- Masthead ---
            tags$div(
              class = "ynow-backtest-report__masthead",
              h2(tags$b(id = "ynow_lab_notes_title", "Quantitative Backtest Lab"))
            ),

            # --- Intro + Backtest Zone run (4:1, vertically centered, light-gray band) ---
            tags$div(
              class = "ynow-backtest-zone-band",
              role = "group",
              `aria-label` = "Backtest run controls",
              id = "ynow_bt_toolbar",
              tags$div(
                class = "ynow-backtest-zone-band__lead",
                p(
                  id = "ynow_lab_notes_sub",
                  class = "ynow-backtest-report__lead",
                  paste0(
                    "Point-in-time strategy NAV and holding gates: read performance and the wealth-index chart first, ",
                    "then exposure versus buy-and-hold. Related controls sit under each chapter (collapsed by default). ",
                    "This is a quantitative backtest report—not Historical Fundamental Validation (see Hist. FV Validation)."
                  )
                )
              ),
              tags$div(
                class = "ynow-backtest-zone-band__run",
                tags$span(
                  class = "ynow-backtest-toolbar__label",
                  id = "ynow_bt_zone_title",
                  "Backtest Zone"
                ),
                actionButton(
                  "run_bt", "Run Backtest",
                  icon = icon("play"),
                  class = "btn-warning",
                  style = "margin:0; white-space:nowrap; font-weight:600;"
                ),
                uiOutput("bt_run_status")
              )
            ),

            # --- Chapter I: Performance KPIs ---
            tags$section(
              class = "ynow-backtest-chapter",
              tags$div(
                class = "ynow-backtest-chapter__head",
                tags$span(
                  class = "ynow-backtest-chapter__kicker",
                  id = "ynow_bt_ch1_kicker",
                  "Section I"
                ),
                tags$h3(
                  class = "ynow-backtest-chapter__title",
                  id = "ynow_bt_ch1_title",
                  "Performance summary"
                )
              ),
              tags$div(
                class = "ynow-backtest-chapter__body",
                uiOutput("perf_metrics")
              )
            ),

            # --- Ch I controls: parameter sync (default collapsed); Run lives in zone band above ---
            box(
              title = tagList(
                icon("play-circle"),
                tags$span(id = "ynow_bt_sec_run_controls", "Run controls & parameter sync")
              ),
              width = NULL,
              status = "warning",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              class = "ynow-bt-run-panel",
              checkboxInput(
                "bt_param_auto",
                "Auto-sync parameters (derive from statements on ticker change)",
                value = isTRUE(APP_DEFAULTS$bt_param_auto)
              ),
              tags$p(
                id = "ynow_bt_param_auto_hint",
                class = "ynow-backtest-inline-hint",
                paste0(
                  "When on, searching / loading a new company overwrites holding thresholds, exposure / sentiment weights, ",
                  "and aligns the HFV recommended valuation model. Manual edits turn this off."
                )
              ),
              actionButton(
                "bt_refresh_params", "Recompute once for current company",
                icon = icon("sync"), class = "btn-default btn-block",
                style = "margin-bottom: 10px;"
              ),
              tags$p(
                id = "ynow_bt_refresh_params_hint",
                class = "ynow-backtest-inline-hint",
                "One-shot: recompute thresholds / weights from current statements (use after turning auto-sync off)."
              )
            ),

            # --- Chapter II: Strategy NAV ---
            tags$section(
              class = "ynow-backtest-chapter",
              tags$div(
                class = "ynow-backtest-chapter__head",
                tags$span(
                  class = "ynow-backtest-chapter__kicker",
                  id = "ynow_bt_ch2_kicker",
                  "Section II"
                ),
                tags$h3(
                  class = "ynow-backtest-chapter__title",
                  id = "ynow_bt_ch2_title",
                  "Strategy NAV (wealth index, start = 1)"
                )
              ),
              tags$div(
                class = "ynow-backtest-chapter__body",
                tags$p(
                  id = "ynow_bt_ch2_lead",
                  class = "ynow-backtest-chapter__lead",
                  paste0(
                    "This is a wealth index, not a share price. Both strategies share the holding gate; ",
                    "paths differ—Fundamental NAV = Exp_A × daily returns; Sentiment NAV = Exp_B × daily returns ",
                    "(Exp_A mixed with momentum / RSI). Actual price on the HFV discount chart is not comparable here."
                  )
                ),
                # NAV window controls sit directly above the wealth-index chart
                tags$div(
                  class = "ynow-backtest-toolbar ynow-backtest-toolbar--chapter ynow-backtest-toolbar--above-chart",
                  role = "group",
                  `aria-label` = "NAV window controls",
                  id = "ynow_bt_nav_controls",
                  tags$div(
                    class = "ynow-backtest-toolbar__group ynow-backtest-toolbar__group--wide",
                    radioButtons(
                      "bt_nav_window",
                      "NAV window (each series resets to 1 at the window start)",
                      inline = TRUE,
                      choices = c(
                        "All" = "all",
                        "1Y" = "1y",
                        "3Y" = "3y",
                        "5Y" = "5y",
                        "Custom" = "custom"
                      ),
                      selected = APP_DEFAULTS$bt_nav_window
                    ),
                    conditionalPanel(
                      condition = "input.bt_nav_window == 'custom'",
                      dateRangeInput(
                        "bt_nav_custom",
                        NULL,
                        start = Sys.Date() - 365,
                        end = Sys.Date(),
                        language = "zh-TW"
                      )
                    )
                  )
                ),
                plotlyOutput("bt_equity_plot", height = "400px") %>% withSpinner(),
                tags$ul(
                  id = "ynow_bt_equity_legend",
                  class = "ynow-backtest-legend",
                  tags$li(
                    tags$b(id = "ynow_bt_leg_fund_label", "Fundamental NAV"),
                    tags$span(
                      id = "ynow_bt_leg_fund_body",
                      " (orange) = holding gate + MOS sizing × daily returns, from 1."
                    )
                  ),
                  tags$li(
                    tags$b(id = "ynow_bt_leg_sent_label", "Sentiment NAV"),
                    tags$span(
                      id = "ynow_bt_leg_sent_body",
                      " (blue) = Exp_A mixed with momentum / RSI; see Sentiment Parameters under Chapter IV."
                    )
                  ),
                  tags$li(
                    tags$b(id = "ynow_bt_leg_bh_label", "Stock buy-and-hold"),
                    tags$span(
                      id = "ynow_bt_leg_bh_body",
                      " (green) = 100% wealth index; "
                    ),
                    tags$b(id = "ynow_bt_leg_bench_label", "Benchmark"),
                    tags$span(
                      id = "ynow_bt_leg_bench_body",
                      " (gray dashed) = SPY or 0050.TW wealth index by market mode."
                    )
                  ),
                  tags$li(
                    id = "ynow_bt_leg_hfv_note",
                    "Per-share FV vs actual price: Hist. FV Validation sidebar — do not mix with this chart."
                  )
                )
              )
            ),

            # --- Chapter III: Exposure & vs B&H ---
            tags$section(
              class = "ynow-backtest-chapter",
              tags$div(
                class = "ynow-backtest-chapter__head",
                tags$span(
                  class = "ynow-backtest-chapter__kicker",
                  id = "ynow_bt_ch3_kicker",
                  "Section III"
                ),
                tags$h3(
                  class = "ynow-backtest-chapter__title",
                  id = "ynow_bt_ch3_title",
                  "Exposure paths & vs buy-and-hold"
                )
              ),
              tags$div(
                class = "ynow-backtest-chapter__body",
                fluidRow(
                  column(
                    width = 6,
                    tags$h5(
                      class = "ynow-backtest-subhead",
                      id = "ynow_bt_exposure_title",
                      "Two-mode exposure paths"
                    ),
                    uiOutput("bt_exposure_stats"),
                    plotlyOutput("bt_exposure_plot", height = "260px") %>% withSpinner()
                  ),
                  column(
                    width = 6,
                    tags$h5(
                      class = "ynow-backtest-subhead",
                      id = "ynow_bt_bh_gap_title",
                      "Relative to buy-and-hold"
                    ),
                    uiOutput("bt_bh_gap")
                  )
                )
              )
            ),

            # --- Ch III controls: Holding gate (default collapsed) ---
            box(
              title = tagList(
                icon("filter"),
                tags$span(id = "ynow_bt_sec_hold_gate", "Holding gate: position filters")
              ),
              width = NULL,
              status = "warning",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              tags$p(
                id = "ynow_bt_hold_gate_intro",
                class = "ynow-backtest-chapter__lead",
                paste0(
                  "On each rebalance date (monthly / quarterly / yearly by analysis frequency), all four filters must pass ",
                  "to allow a position; otherwise both Fundamental and Sentiment strategies stay flat (Exp_A = Exp_B = 0). ",
                  "Thresholds are shared with the backtest engine and the KPI filter."
                )
              ),
              fluidRow(
                column(3, tipify(numericInput("bt_net_margin", "Net margin threshold (%)", APP_DEFAULTS$bt_net_margin),
                                 "Auto mode uses about half of the company's historical net margin.", placement = "top")),
                column(3, tipify(numericInput("bt_rev_growth", "Revenue growth threshold (%)", APP_DEFAULTS$bt_rev_growth),
                                 "Auto mode uses about half of historical revenue growth.", placement = "top")),
                column(3, tipify(numericInput("bt_eps_growth", "EPS / net income growth threshold (%)", APP_DEFAULTS$bt_eps_growth),
                                 "Auto mode uses about half of net income growth.", placement = "top")),
                column(3, tipify(numericInput("bt_fcf_cv", "FCF CV ceiling (%)", APP_DEFAULTS$bt_fcf_cv),
                                 "Auto mode uses FCF CV × 1.25.", placement = "top"))
              ),
              tags$hr(),
              tags$div(
                style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap; margin-bottom:8px;",
                tags$span(
                  style = "font-size:13px; font-weight:600;",
                  id = "ynow_bt_kpi_filter_label",
                  "KPI filter"
                ),
                actionButton(
                  "bt_kpi_filter", "Match current company",
                  icon = icon("filter"),
                  class = "btn-sm",
                  style = "background-color: #222222; color: #ffffff; border: 1px solid #111111; font-size: 12px; padding: 6px 14px; border-radius: 4px; font-weight: 600;"
                ),
                uiOutput("bt_filter_badge")
              ),
              tags$p(
                id = "ynow_bt_kpi_filter_hint",
                style = "margin: 0 0 8px 0; font-size: 12px; color: #666;",
                "Compare Dashboard-loaded company KPIs to the thresholds above (same Great Filter as the backtest)."
              ),
              uiOutput("bt_filter_detail")
            ),

            # --- Chapter IV: MOS / FV signal validation ---
            tags$section(
              class = "ynow-backtest-chapter",
              tags$div(
                class = "ynow-backtest-chapter__head",
                tags$span(
                  class = "ynow-backtest-chapter__kicker",
                  id = "ynow_bt_ch4_kicker",
                  "Section IV"
                ),
                tags$h3(
                  class = "ynow-backtest-chapter__title",
                  id = "ynow_bt_ch4_title",
                  "Signal validation: MOS & Fair Value"
                )
              ),
              tags$div(
                class = "ynow-backtest-chapter__body",
                tags$p(
                  id = "ynow_bt_ch4_lead",
                  class = "ynow-backtest-chapter__lead",
                  paste0(
                    "Checks whether undervaluation coincides with better forward returns—the core test of whether ",
                    "the backtest signal can stand. Parameter sensitivity belongs on the YNOW tab (WACC×g matrix)."
                  )
                ),
                tags$div(
                  class = "ynow-bt-validate",
                  fluidRow(
                    column(
                      6,
                      tags$div(
                        class = "ynow-bt-validate-col",
                        tags$div(
                          class = "ynow-bt-validate-panel",
                          tags$h5(tags$b(id = "ynow_bt_mos_eff_title", "MOS effectiveness")),
                          tags$p(
                            id = "ynow_bt_mos_eff_hint",
                            class = "ynow-backtest-inline-hint",
                            "Forward 1Y / 3Y / 5Y returns by MOS bucket: do higher MOS buckets earn more?"
                          ),
                          tags$div(style = "overflow-x:auto;", tableOutput("bt_mos_table"))
                        )
                      )
                    ),
                    column(
                      6,
                      tags$div(
                        class = "ynow-bt-validate-col",
                        tags$div(
                          class = "ynow-bt-validate-panel",
                          tags$h5(tags$b(id = "ynow_bt_fv_edge_title", "Fair Value predictive edge")),
                          uiOutput("bt_fv_edge"),
                          tags$div(style = "overflow-x:auto;", tableOutput("bt_fv_table"))
                        )
                      )
                    )
                  )
                )
              )
            ),

            # --- Ch IV controls: Strategy parameters (default collapsed) ---
            box(
              title = tagList(
                icon("sliders-h"),
                tags$span(id = "ynow_bt_sec_strategy_params", "Strategy parameters")
              ),
              width = NULL,
              status = "primary",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              uiOutput("bt_param_notes"),
              tags$p(
                id = "ynow_bt_params_gate_note",
                class = "ynow-backtest-chapter__lead",
                paste0(
                  "Holding gate (net margin / revenue growth / EPS growth / FCF volatility — fail → Exp_A = 0) ",
                  "is under Chapter III. Here adjust sizing and sentiment weights only."
                )
              ),
              tags$div(
                class = "ynow-bt-params",
                tabBox(
                  title = NULL,
                  width = 12,
                  tabPanel(
                    title = tagList(
                      icon("balance-scale"),
                      tags$span(id = "ynow_bt_tab_fundamental", "Fundamental")
                    ),
                    value = "bt_fundamental",
                    tags$p(
                      id = "ynow_bt_fund_intro",
                      class = "ynow-backtest-chapter__lead",
                      "Mode A: Exp_A → orange NAV line. MOS buckets set sizing; MOS uses HFV Replay-model fair value."
                    ),
                    fluidRow(
                      column(
                        6,
                        sliderInput("bt_w_vg", "MOS / Value Gap weight (exposure)", 0, 1, APP_DEFAULTS$bt_w_vg, step = 0.01),
                        tags$p(
                          id = "ynow_bt_w_vg_hint",
                          class = "ynow-backtest-inline-hint",
                          "Higher = more MOS-bucket de-risking; lower ≈ fixed neutral size."
                        )
                      ),
                      column(
                        6,
                        tags$div(
                          class = "ynow-backtest-callout",
                          tags$b(id = "ynow_bt_mos_ladder_title", "MOS lag exposure (baseline map)"),
                          tags$br(),
                          tags$span(
                            id = "ynow_bt_mos_ladder_body",
                            paste0(
                              "MOS≥30%→near max holding; ≥10%→~72%×cap; ≥0%→~44%×cap; ≥−10%→~17%×cap; else flat. ",
                              "(Max / floor holding and \"Closer to buy-and-hold\" are under Sentiment.)"
                            )
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    title = tagList(
                      icon("bolt"),
                      tags$span(id = "ynow_bt_tab_sentiment", "Sentiment")
                    ),
                    value = "bt_sentiment",
                    tags$div(
                      class = "ynow-bt-mode-b",
                      tags$p(
                        id = "ynow_bt_sent_intro",
                        class = "ynow-backtest-chapter__lead",
                        paste0(
                          "Mode B: mix momentum / RSI onto Exp_A (hot→fuller, cold→conservative). ",
                          "Blue line = Sentiment NAV from 1. Unrelated to HFV actual price."
                        )
                      ),
                      tags$div(
                        class = "ynow-bt-mode-b-grid",
                        fluidRow(
                          column(
                            3,
                            sliderInput("bt_w_mom", "Momentum relative weight", 0, 1, APP_DEFAULTS$bt_w_mom, step = 0.01),
                            tags$p(
                              id = "ynow_bt_w_mom_hint",
                              class = "ynow-backtest-inline-hint",
                              "With RSI forms the sentiment score, then mixes with Exp_A."
                            )
                          ),
                          column(
                            3,
                            sliderInput("bt_w_rsi", "RSI relative weight", 0, 1, APP_DEFAULTS$bt_w_rsi, step = 0.01),
                            tags$p(
                              id = "ynow_bt_w_rsi_hint",
                              class = "ynow-backtest-inline-hint",
                              "Overbought lowers the sentiment target; oversold raises it."
                            )
                          ),
                          column(
                            3,
                            sliderInput("bt_max_exp", "Max holding cap", 0.5, 1, APP_DEFAULTS$bt_max_exp, step = 0.01),
                            tags$p(
                              id = "ynow_bt_max_exp_hint",
                              class = "ynow-backtest-inline-hint",
                              "Set to 1.00 to remove structural underweight vs buy-and-hold."
                            )
                          ),
                          column(
                            3,
                            sliderInput("bt_min_exp_pass", "Min holding after gate pass", 0, 0.4, APP_DEFAULTS$bt_min_exp_pass, step = 0.01),
                            tags$p(
                              id = "ynow_bt_min_exp_hint",
                              class = "ynow-backtest-inline-hint",
                              "Floor size when the gate passes and valuation is not extremely rich."
                            )
                          )
                        )
                      ),
                      tags$div(
                        class = "ynow-bt-fit-row",
                        fluidRow(
                          column(
                            12,
                            tags$div(
                              class = "ynow-bt-fit-panel",
                              actionButton(
                                "bt_fit_bh_preset", "Closer to buy-and-hold",
                                icon = icon("chart-line"),
                                class = "btn-success",
                                style = "font-weight:600;"
                              ),
                              tags$div(
                                id = "ynow_bt_fit_bh_hint",
                                style = "margin-top:8px;font-size:11px;color:#666;",
                                "One-click: max=100%, min=40%, w_vg=0.35 (weaker de-risking). Turns auto-sync off."
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),

            # --- Appendix: trend momentum (timing aid) ---
            decision_momentum_panel_ui("main_decision"),

            # --- Appendix: methodology ---
            box(
              title = tagList(
                icon("book-open"),
                tags$span(id = "ynow_bt_sec_methodology", "Data sources & methodology notes")
              ),
              width = NULL,
              status = "primary",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              uiOutput("bt_methodology_notes")
            )
          )
  )
}

#' Lazy page body for tab `asset_transmission` (mounted once per session).
#' @param locale first-paint locale (`en` / `zh-TW`); language switches still update via applyUiLocale.
.ynow_page_ui_asset_transmission <- function(locale = "en") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    "en"
  }
  .atx_s <- function(key) {
    if (exists("ui_str", mode = "function")) ui_str(key, loc) else key
  }
  tagList(
    fluidRow(
      column(
        width = 12,
        h2(tags$b(id = "ynow_atx_page_title", .atx_s("atx_page_title"))),
        p(
          id = "ynow_atx_page_sub",
          .atx_s("atx_page_sub")
        ),
        tags$hr()
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "box box-solid box-primary ynow-atx-panel",
          tags$div(
            class = "box-header",
            tags$h3(
              class = "box-title",
              tagList(icon("project-diagram"), tags$span(id = "ynow_atx_box_title", .atx_s("atx_box_title")))
            )
          ),
          tags$div(
            class = "box-body",
            tags$p(
              id = "ynow_atx_box_body",
              style = "color:#555; line-height:1.5; margin:0 0 10px 0;",
              .atx_s("atx_box_body")
            ),
            asset_transmission_ui("atx")
          )
        )
      )
    )
  )
}

#' Lazy page body for tab `testing` (Full-only sandbox; Asset transmission moved out).
.ynow_page_ui_testing <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        h2(tags$b(id = "ynow_testing_page_title", "Testing")),
        p(
          id = "ynow_testing_page_sub",
          paste0(
            "Full-only sandbox for experiments. Lite mode hides this entry. ",
            "Asset transmission root map now lives in its own sidebar tab (after Macro & Market Trends)."
          )
        ),
        tags$hr(),
        tags$p(
          id = "ynow_testing_box_body",
          style = "color:#555; line-height:1.5;",
          paste0(
            "Use the sidebar entry Asset transmission root map for the inflation → rates → liquidity → asset map. ",
            "This Testing page remains a Full-only placeholder for future lab hooks."
          )
        )
      )
    )
  )
}

ui <- dashboardPage(
  skin = "black",
  # Browser tab. Header logo stays the HTML progress title; do not let that markup become <title>.
  title = "The YNow App",

  dashboardHeader(
                title = HTML(paste0(
                  '<span class="ynow-app-title" id="ynow_app_title" ',
                  'data-ynow-build="', .YNOW_BUILD_VERSION, '" ',
                  'role="progressbar" aria-valuemin="0" aria-valuemax="100" aria-valuenow="0" ',
                  'aria-label="The YNow App loading">',
                  '<span class="ynow-app-title-base" aria-hidden="true">The YNow App v21.44</span>',
                  '<span class="ynow-app-title-fill" aria-hidden="true">',
                  '<span class="ynow-app-title-fill-inner">The YNow App v21.44</span>',
                  '</span></span>'
                )),
    titleWidth = 250,
    tags$li(
      id = "ynow-market-header",
      class = "dropdown ynow-market-header",
      tags$div(
        class = "ynow-market-stack",
        role = "group",
        `aria-label` = "Market",
        tags$button(
          type = "button",
          class = "ynow-mkt-btn active",
          id = "ynow_mkt_btn_us",
          `data-value` = "US",
          "US"
        ),
        tags$button(
          type = "button",
          class = "ynow-mkt-btn",
          id = "ynow_mkt_btn_tw",
          `data-value` = "TW",
          "TW"
        )
      )
    ),
    # Language (zh-TW ↔ en-US) — custom buttons (same stability pattern as US/TW;
    # avoid shinyWidgets radioGroupButtons re-render that displaces the control on mobile)
    tags$li(
      class = "dropdown ynow-lang-header ynow-hdr-toggle",
      tags$div(
        class = "ynow-lang-stack",
        id = "ynow_lang_stack",
        role = "group",
        `aria-label` = "Language",
        tags$button(
          type = "button",
          class = "ynow-lang-btn",
          id = "ynow_lang_btn_zh",
          `data-value` = "zh-TW",
          "繁中"
        ),
        tags$button(
          type = "button",
          class = "ynow-lang-btn active",
          id = "ynow_lang_btn_en",
          `data-value` = "en",
          "EN"
        )
      )
    ),
    # Logo + USD／TWD under logo (modest gap below black header)
    tags$li(
      id = "ynow-header-logo",
      class = "dropdown ynow-header-logo",
      tags$img(
        id = "ynow_header_logo_home",
        class = "ynow-header-logo-mark",
        src = "ynow-logo-mark-40.png",
        width = 36,
        height = 36,
        alt = "YNow",
        role = "button",
        tabindex = "0",
        title = "Go to Home",
        `aria-label` = "Go to Home"
      ),
      tags$div(
        class = "ynow-ccy-float",
        role = "group",
        `aria-label` = "Currency",
        shinyWidgets::radioGroupButtons(
          inputId = "session_ccy_pick",
          label = NULL,
          choices = c("USD", "TWD"),
          selected = APP_DEFAULTS$session_ccy,
          status = "default",
          size = "xs",
          individual = TRUE
        ),
        tags$div(
          class = "ynow-hdr-ccy-status",
          textOutput("hdr_ccy_status", inline = TRUE)
        )
      )
    )
  ),
  
  dashboardSidebar(
    width = 250,
    collapsed = TRUE,
    column(width = 12,
           column(width = 12, textOutput("today"),
                  hr()
           )
    ),
    
    # Must stay inside column(width=12): bare sidebarMenu after Bootstrap
    # floated cols collapses to width:0 / left:250 (menu invisible).
    # 「推薦」badges are patched in-place (no renderMenu remount).
    column(width = 12,
           sidebarMenu(
             id = "sidebar_tabs",
             menuItem(
               text = tags$span(id = "ynow_menu_home", "Home"),
               tabName = "home",
               icon = icon("home"),
               selected = TRUE
             ),
             menuItem(
               text = tags$span(id = "ynow_menu_macro", "Macro & Market Trends"),
               tabName = "macro_market",
               icon = icon("globe-asia")
             ),
             menuItem(
               text = tags$span(id = "ynow_menu_asset_transmission", "Asset transmission root map"),
               tabName = "asset_transmission",
               icon = icon("project-diagram")
             ),
             menuItem("Blue Chip Leaderboard", tabName = "bluechip", icon = icon("star")),
             menuItem("Company", tabName = "dashboard", icon = icon("chart-line")),
             menuItem(
               text = tags$span(id = "ynow_menu_company_advance", "Business Breakdown"),
               tabName = "company_advance",
               icon = icon("layer-group")
             ),
             menuItem("YNOW", tabName = "sensitivity", icon = icon("clock")),
             menuItem(
               text = tags$span(id = "ynow_menu_smart", "Smart Analysis"),
               tabName = "smart_analysis",
               icon = icon("magic")
             ),
             menuItem("Model Dashboard", tabName = "get_started", icon = icon("play-circle")),
             menuItem(
               text = tags$span(id = "ynow_menu_cat_asset", "Appr. Asset-based"),
               icon = icon("building"),
               startExpanded = FALSE,
               menuSubItem(
                 text = tags$span(id = "ynow_menu_nav", "NAV"),
                 tabName = "nav_calculator",
                 icon = icon("sitemap")
               )
             ),
             menuItem(
               text = tags$span(id = "ynow_menu_cat_income", "Appr. Income / Cashflow"),
               icon = icon("chart-area"),
               startExpanded = FALSE,
               menuSubItem("DCF-Model", tabName = "dcf_calculator", icon = icon("calculator")),
               menuSubItem("DDM", tabName = "ddm_calculator", icon = icon("hand-holding-usd")),
               menuSubItem("RI-Model", tabName = "ri_calculator", icon = icon("gem"))
             ),
             menuItem(
               text = tags$span(id = "ynow_menu_cat_relative", "Appr. Relative Valuation"),
               icon = icon("percentage"),
               startExpanded = FALSE,
               menuSubItem("P/B", tabName = "pb_calculator", icon = icon("landmark")),
               menuSubItem(
                 text = tags$span(id = "ynow_menu_rel_multiples", "Multiples"),
                 tabName = "rel_multiples_calculator",
                 icon = icon("percentage")
               ),
               menuSubItem(
                 text = tags$span(id = "ynow_menu_sotp", "SOTP"),
                 tabName = "sotp_calculator",
                 icon = icon("puzzle-piece")
               )
             ),
             # 歷史基本面驗證（HFV）：理論估值 vs 實際市值 — 非策略回測
             menuItem("Hist. FV Validation", tabName = "hfv", icon = icon("balance-scale")),
             # 量化回測實驗室：主選單（完整版）；Lite 以 CSS 隱藏，不進簡化版
             menuItem(
               text = tags$span(id = "ynow_menu_backtest", "Quant Backtest Lab"),
               tabName = "lab_notes",
               icon = icon("flask")
             ),
             menuItem("Decision Checklist", tabName = "decision_checklist", icon = icon("clipboard-check")),
             menuItem("About", tabName = "about", icon = icon("info-circle"))
             # Snapshot 不放主選單（避免巢狀 li 被瀏覽器抬出隱藏）；改由底部捷徑切換
           ),
           hr()
    ),
    
    column(width = 12,
           h5(id = "ynow_recent_search_label", "Recent Search:"),
           textOutput("recentsearch"),
           hr()
    ),
    
    column(width = 12,
           tags$div(
             class = "ynow-sidebar-brand ynow-lite-toggle",
             id = "ynow_lite_toggle",
             role = "button",
             tabindex = "0",
             `aria-pressed` = "false",
             `aria-label` = "Toggle Lite mode",
             title = "Click to switch Lite / Full",
             tags$div(
               class = "ynow-sidebar-brand-inner",
               tags$img(
                 class = "ynow-sidebar-logo-full",
                 src = "ynow-logo-full-480.png",
                 alt = "YNow — WH.Y VALUE NOW",
                 draggable = "false"
               ),
               tags$span(
                 class = "ynow-lite-badge",
                 id = "ynow_lite_badge",
                 "LITE"
               )
             )
           ),
           div(class = "ynow-sidebar-download-wrap",
               downloadButton("download_report", "Download Report (PDF)",
                              class = "ynow-sidebar-download-btn",
                              style = "background-color: #1a1a1a; color: #ffffff; border: 1px solid #000000; box-shadow: none; text-shadow: none;")
           )
    ),
    
    column(width = 12,
           div(
             id = "ynow_data_source_block",
             style = "padding: 15px; border-radius: 5px; border-left: 4px",
             tags$b(id = "ynow_data_source_title", "Data Source:"), tags$br(),
             tags$span(
               id = "ynow_data_source_body",
               "This application integrates real-time financial data via web parsing and API resources, applying comprehensive models for valuation."
             )
           )
    ),

    # Snapshot：側邊欄內容流最底部低調捷徑（勿用 absolute，會跑到搜尋框）
    column(
      width = 12,
      class = "ynow-sidebar-snapshot-foot",
      tags$a(
        href = "#shiny-tab-snapshot",
        `data-toggle` = "tab",
        `data-value` = "snapshot",
        class = "ynow-sidebar-snapshot-link",
        onclick = "Shiny.setInputValue('sidebar_tabs', 'snapshot', {priority: 'event'}); setTimeout(function(){ try { if (window.ensureLiteSnapshotDefaultsTab) window.ensureLiteSnapshotDefaultsTab(); } catch (e) {} }, 0); return false;",
        icon("camera", class = "fa-fw"),
        tags$span(id = "ynow_snapshot_link_label", " Snapshot")
      ),
      # Testing：側邊欄小按鈕（完整版）；供後續實驗／測試功能入口；Lite 隱藏
      tags$a(
        id = "ynow_sidebar_test_btn",
        href = "#shiny-tab-testing",
        `data-toggle` = "tab",
        `data-value` = "testing",
        class = "ynow-sidebar-snapshot-link ynow-sidebar-test-link ynow-full-only",
        onclick = "Shiny.setInputValue('sidebar_tabs', 'testing', {priority: 'event'}); Shiny.setInputValue('sidebar_test_click', (window.__ynowTestClicks=(window.__ynowTestClicks||0)+1), {priority: 'event'}); return false;",
        icon("flask", class = "fa-fw"),
        tags$span(id = "ynow_test_link_label", " Testing")
      ),
      # 意見區：收集使用者回饋，系統性開 GitHub Issue
      tags$a(
        id = "ynow_sidebar_feedback_btn",
        href = "#shiny-tab-feedback",
        `data-toggle` = "tab",
        `data-value` = "feedback",
        class = "ynow-sidebar-snapshot-link ynow-sidebar-feedback-link",
        onclick = "Shiny.setInputValue('sidebar_tabs', 'feedback', {priority: 'event'}); Shiny.setInputValue('sidebar_feedback_click', (window.__ynowFeedbackClicks=(window.__ynowFeedbackClicks||0)+1), {priority: 'event'}); return false;",
        icon("comment-dots", class = "fa-fw"),
        tags$span(id = "ynow_feedback_link_label", " 意見區")
      )
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    # MathJax: load on pages that need it (Basic Setup methodology, HFV, Backtest Lab)
    # rather than on every first paint of Home.

    tags$head(
      tags$style(HTML('
        /* YNOW monochrome chrome: black/white (keep KPI bg-blue / Schilit red-green) */
        :root {
          --ynow-ink: #1a1a1a;
          --ynow-ink-soft: #333333;
          --ynow-line: #d0d0d0;
          --ynow-wash: #f5f5f5;
          /* 亮面金屬金（非土黃／ochre）；fallback 給不支援 clip 的環境） */
          --ynow-gold: #F5C518;
          /* 白底標題用深金（對比足夠）；圖標可用亮金 */
          --ynow-gold-ink: #856404;
          --ynow-gold-deep: #C9A227;
          /* logo 圖檔主色（藍／綠）— 現金流序列等資料色 */
          --ynow-logo-blue: #0C5484;
          --ynow-logo-green: #249C60;
          --ynow-logo-cyan: #1AA8B8;
          /* Shared content column — same as Macro & Market Trends (.ynow-macro-report) */
          --ynow-page-max: 1200px;
          --ynow-logo-flow-gradient: linear-gradient(
            105deg,
            #0C5484 0%,
            #1578A0 16%,
            #1AA8B8 32%,
            #249C60 50%,
            #1AA8B8 68%,
            #0C5484 100%
          );
          /* HTCDI numerals only — flame flow (not shared with YNOW / Data-limited logo fill) */
          --ynow-htcdi-flame-gradient: linear-gradient(
            105deg,
            #4A0E0E 0%,
            #8B1A1A 12%,
            #C0392B 24%,
            #E67E22 38%,
            #F39C12 50%,
            #F7CA18 62%,
            #FFE66D 74%,
            #E67E22 88%,
            #922B21 100%
          );
          /* Model Selector｜估值模型推薦 色系（各模型頁主題；與小卡 icon 色一致） */
          --ynow-model-nav: #d81b60;
          --ynow-model-dcf: #00a65a;
          --ynow-model-ddm: #f39c12;
          --ynow-model-ri: #605ca8;
          --ynow-model-pb: #3c8dbc;
          --ynow-model-multiples: #17a2b8;
          --ynow-model-sotp: #343a40;
          --ynow-gold-gradient: linear-gradient(
            105deg,
            #FFF6C8 0%,
            #FFE566 18%,
            #F5C518 36%,
            #C9A227 50%,
            #FFE566 68%,
            #FFF8D0 100%
          );
        }

        @keyframes ynow-gold-shine {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }
        @keyframes ynow-logo-flow {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }
        @keyframes ynow-htcdi-flame-flow {
          0%, 100% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
        }

        /* Shiny toasts: keep above AdminLTE chrome; mobile must not clip / bury them */
        #shiny-notification-panel {
          position: fixed !important;
          z-index: 20000 !important;
          pointer-events: none;
        }
        #shiny-notification-panel .shiny-notification {
          pointer-events: auto;
          box-shadow: 0 4px 16px rgba(0, 0, 0, 0.18);
        }
        @media (max-width: 767px) {
          #shiny-notification-panel {
            top: auto !important;
            bottom: 12px !important;
            right: 8px !important;
            left: 8px !important;
            width: auto !important;
            max-width: none !important;
          }
          #shiny-notification-panel .shiny-notification {
            width: 100% !important;
            max-width: none !important;
            font-size: 13px;
            line-height: 1.4;
            margin: 0 0 8px 0 !important;
          }
        }

        /* 標題＝載入進度條：底層淡金軌道 + 金色填滿層（隨 --ynow-load-pct） */
        .main-header .logo,
        .main-header .logo:hover {
          position: relative;
          z-index: 1;
          font-weight: bold;
          color: #FFD700 !important;
          background-color: var(--ynow-ink) !important;
          background-image: none !important;
          filter: none !important;
        }
        .main-header .logo .ynow-app-title {
          --ynow-load-pct: 0%;
          position: relative;
          display: inline-block;
          font-weight: bold;
          color: transparent !important;
          background-image: none !important;
          -webkit-text-fill-color: transparent;
          animation: none;
          filter: none !important;
          line-height: 1.15;
          vertical-align: middle;
        }
        /* Update-available: cursor only here; flame fill overrides come after gold title rules */
        body.ynow-update-available .main-header .logo,
        body.ynow-update-available .main-header .logo .ynow-app-title {
          cursor: pointer;
        }
        .main-header .logo .ynow-app-title-base {
          display: inline-block;
          font-weight: bold;
          white-space: nowrap;
          color: rgba(245, 197, 24, 0.32) !important;
          -webkit-text-fill-color: rgba(245, 197, 24, 0.32);
          text-shadow: none;
        }
        .main-header .logo .ynow-app-title-fill {
          position: absolute;
          left: 0;
          top: 0;
          bottom: 0;
          width: var(--ynow-load-pct, 0%);
          max-width: 100%;
          overflow: hidden;
          white-space: nowrap;
          pointer-events: none;
          transition: width 0.28s ease-out;
        }
        .main-header .logo .ynow-app-title-fill-inner {
          display: inline-block;
          font-weight: bold;
          white-space: nowrap;
          color: #FFD700 !important;
          background-color: transparent !important;
          background-image: var(--ynow-gold-gradient) !important;
          background-size: 220% 100%;
          background-repeat: no-repeat;
          -webkit-background-clip: text;
          background-clip: text;
          -webkit-text-fill-color: transparent;
          filter: none !important;
        }
        .main-header .logo .ynow-app-title.is-loading .ynow-app-title-fill-inner {
          animation: none;
        }
        .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill {
          width: 100%;
          transition: width 0.2s ease-out;
        }
        .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
          animation: ynow-gold-shine 2.6s ease-in-out infinite;
        }
        @supports not ((-webkit-background-clip: text) or (background-clip: text)) {
          .main-header .logo .ynow-app-title-fill-inner {
            -webkit-text-fill-color: #FFD700 !important;
            color: #FFD700 !important;
            background-image: none !important;
            text-shadow:
              0 0 6px rgba(255, 230, 120, 0.85),
              0 0 14px rgba(255, 200, 40, 0.45),
              0 1px 2px rgba(0, 0, 0, 0.85);
          }
          .main-header .logo .ynow-app-title-base {
            color: rgba(255, 215, 0, 0.35) !important;
            -webkit-text-fill-color: rgba(255, 215, 0, 0.35);
          }
        }
        /* Pending deploy: flame title must override gold fill/shine (place after gold rules) */
        body.ynow-update-available .main-header .logo .ynow-app-title-base {
          color: rgba(192, 57, 43, 0.38) !important;
          -webkit-text-fill-color: rgba(192, 57, 43, 0.38) !important;
          animation: none !important;
        }
        body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill,
        body.ynow-update-available .main-header .logo .ynow-app-title-fill {
          width: 100% !important;
        }
        body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner,
        body.ynow-update-available .main-header .logo .ynow-app-title-fill-inner {
          color: transparent !important;
          background-color: transparent !important;
          background-image: var(--ynow-htcdi-flame-gradient) !important;
          background-size: 220% 100% !important;
          background-repeat: no-repeat !important;
          -webkit-background-clip: text !important;
          background-clip: text !important;
          -webkit-text-fill-color: transparent !important;
          animation: ynow-htcdi-flame-flow 2.4s ease-in-out infinite !important;
          text-decoration: none !important;
          filter: none !important;
        }
        @supports not ((-webkit-background-clip: text) or (background-clip: text)) {
          body.ynow-update-available .main-header .logo .ynow-app-title-fill-inner,
          body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
            -webkit-text-fill-color: #E67E22 !important;
            color: #E67E22 !important;
            background-image: none !important;
            animation: none !important;
            text-shadow:
              0 0 6px rgba(255, 120, 40, 0.85),
              0 0 14px rgba(192, 57, 43, 0.55),
              0 1px 2px rgba(0, 0, 0, 0.85);
          }
          body.ynow-update-available .main-header .logo .ynow-app-title-base {
            color: rgba(192, 57, 43, 0.45) !important;
            -webkit-text-fill-color: rgba(192, 57, 43, 0.45) !important;
          }
        }
        @media (prefers-reduced-motion: reduce) {
          body.ynow-update-available .main-header .logo .ynow-app-title-fill-inner,
          body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner {
            animation: none !important;
            background-position: 0% 50% !important;
          }
        }
        /* 美股：logo 區塊黑底（容器層，不影響內層文字漸層） */
        .skin-black .main-header .logo::before {
          content: "";
          position: absolute;
          inset: 0;
          z-index: -1;
          pointer-events: none;
          background-color: var(--ynow-ink);
        }

        .content-wrapper, .right-side { background-color: #f7f7f7; }
        /* 網頁／手機共用：黑色頁首橫列置頂固定，捲動不滑掉 */
        .main-header {
          position: fixed !important;
          top: 0 !important;
          left: 0 !important;
          right: 0 !important;
          width: 100% !important;
          z-index: 1030 !important;
        }
        .content-wrapper,
        .right-side {
          margin-top: 50px !important;
        }
        /* Reserve band under fixed header for credit (left) + USD/TWD (right) */
        .content-wrapper > .content {
          padding-top: 52px;
        }
        /* All sidebar tabs share Macro & Market Trends content width + center */
        .content-wrapper > .content > .tab-content {
          width: 100%;
          max-width: 100%;
        }
        .content-wrapper > .content > .tab-content > .tab-pane {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin-left: auto;
          margin-right: auto;
          box-sizing: border-box;
        }
        .content-wrapper > .content > .tab-content > .tab-pane .box,
        .content-wrapper > .content > .tab-content > .tab-pane .nav-tabs-custom {
          max-width: 100%;
        }
        /* Keep Plotly / DT / images fluid inside the shared column */
        .content-wrapper > .content > .tab-content > .tab-pane .plotly,
        .content-wrapper > .content > .tab-content > .tab-pane .html-widget,
        .content-wrapper > .content > .tab-content > .tab-pane .dataTables_wrapper,
        .content-wrapper > .content > .tab-content > .tab-pane img {
          max-width: 100%;
        }
        @media (max-width: 767px) {
          .content-wrapper > .content > .tab-content > .tab-pane {
            max-width: 100%;
            padding-left: 0;
            padding-right: 0;
          }
        }
        .skin-black .main-header .navbar {
          background-color: var(--ynow-ink) !important;
          position: relative;
        }
        /* 側欄漢堡：僅三條白線，無外框／無 FA 字型邊框；整顆按鈕與線條皆垂直置中 */
        .main-header .navbar > .sidebar-toggle,
        .skin-black .main-header .navbar .sidebar-toggle {
          color: transparent !important;
          border: none !important;
          border-right: none !important;
          box-shadow: none !important;
          outline: none !important;
          background-color: transparent !important;
          background-image: none !important;
          position: relative !important;
          box-sizing: border-box !important;
          float: left;
          width: 44px;
          min-width: 44px;
          height: 50px !important;
          min-height: 50px !important;
          max-height: 50px !important;
          padding: 0 !important;
          margin: 0 !important;
          display: inline-flex !important;
          align-items: center !important;
          justify-content: center !important;
          line-height: 0 !important;
        }
        .main-header .navbar > .sidebar-toggle:before,
        .skin-black .main-header .navbar .sidebar-toggle:before {
          content: "" !important;
          display: block !important;
          width: 18px;
          height: 14px;
          margin: 0;
          padding: 0;
          border: none !important;
          border-radius: 0 !important;
          /* 14px 高的三線 icon，flex 可正確垂直置中（勿用 2px+box-shadow） */
          background-color: transparent !important;
          background-image: linear-gradient(
            to bottom,
            #fff 0px, #fff 2px,
            transparent 2px, transparent 6px,
            #fff 6px, #fff 8px,
            transparent 8px, transparent 12px,
            #fff 12px, #fff 14px
          ) !important;
          box-shadow: none !important;
          position: relative !important;
          left: auto !important;
          top: auto !important;
          transform: none !important;
          font-family: none !important;
          font-weight: normal !important;
          line-height: 0 !important;
        }
        .main-header .navbar > .sidebar-toggle .icon-bar,
        .skin-black .main-header .navbar .sidebar-toggle .icon-bar {
          display: none !important;
        }
        .main-header .navbar > .sidebar-toggle .fa,
        .main-header .navbar > .sidebar-toggle .fas,
        .skin-black .main-header .navbar .sidebar-toggle .fa,
        .skin-black .main-header .navbar .sidebar-toggle .fas {
          display: none !important;
        }
        .skin-black .main-header .navbar .sidebar-toggle:hover,
        .skin-black .main-header .navbar .sidebar-toggle:focus,
        .skin-black .main-header .navbar .sidebar-toggle:active {
          background-color: transparent !important;
          color: transparent !important;
          border: none !important;
          box-shadow: none !important;
        }
        .skin-black .main-header .navbar .sidebar-toggle:hover:before,
        .skin-black .main-header .navbar .sidebar-toggle:focus:before {
          background-color: transparent !important;
          background-image: linear-gradient(
            to bottom,
            #fff 0px, #fff 2px,
            transparent 2px, transparent 6px,
            #fff 6px, #fff 8px,
            transparent 8px, transparent 12px,
            #fff 12px, #fff 14px
          ) !important;
          box-shadow: none !important;
        }

        /* 美股／台股：釘在三線 icon 右側（navbar 座標；脫離右欄 flex；無空隙） */
        .main-header .navbar {
          position: relative !important;
        }
        .main-header .navbar > .sidebar-toggle,
        .skin-black .main-header .navbar .sidebar-toggle {
          padding-right: 0 !important;
          margin-right: 0 !important;
        }
        .main-header .navbar #ynow-market-header.ynow-market-header,
        .main-header .navbar-custom-menu #ynow-market-header.ynow-market-header,
        .main-header .navbar-custom-menu .navbar-nav > li#ynow-market-header.ynow-market-header {
          position: absolute !important;
          left: 44px !important; /* fallback；JS 依 toggle.offsetWidth 覆寫 */
          right: auto !important;
          top: 0 !important;
          float: none !important;
          width: 40px !important;
          height: 50px !important;
          margin: 0 !important;
          padding: 0 !important;
          list-style: none !important;
          display: block !important;
          visibility: visible !important;
          z-index: 1040;
        }
        #ynow-market-header .ynow-market-stack {
          display: flex;
          flex-direction: column;
          width: 40px;
          height: 50px;
          margin: 0;
          padding: 0;
        }
        #ynow-market-header .ynow-mkt-btn {
          box-sizing: border-box;
          flex: 1 1 50%;
          width: 100%;
          height: 25px;
          min-height: 25px;
          max-height: 25px;
          margin: 0;
          padding: 0;
          border: 1px solid rgba(255,255,255,0.35);
          border-radius: 0;
          background: rgba(255,255,255,0.12);
          color: #fff;
          font-size: 11px;
          font-weight: 700;
          line-height: 23px;
          cursor: pointer;
        }
        #ynow-market-header .ynow-mkt-btn + .ynow-mkt-btn {
          border-top-width: 0;
        }
        #ynow-market-header .ynow-mkt-btn.active {
          background: #fff;
          color: #222;
          border-color: #fff;
        }
        #ynow-market-header .ynow-mkt-btn:focus {
          outline: none;
        }

        /* 台股（桌面）：dashboardHeader 以中華民國國旗填滿；美股維持全黑。
           手機改回黑底（見下方 max-width: 767px 覆寫）。 */
        body.ynow-market-tw .main-header {
          position: relative;
          background-color: transparent !important;
        }
        body.ynow-market-tw .main-header::before {
          content: "";
          position: absolute;
          inset: 0;
          z-index: 0;
          pointer-events: none;
          background-image: url("roc_flag.svg");
          background-size: cover;
          background-position: center center;
          background-repeat: no-repeat;
        }
        body.ynow-market-tw .main-header::after {
          content: "";
          position: absolute;
          inset: 0;
          z-index: 0;
          pointer-events: none;
          background: rgba(0, 0, 0, 0.42);
        }
        body.ynow-market-tw .main-header > .logo,
        body.ynow-market-tw .main-header > .navbar {
          position: relative;
          z-index: 1;
        }
        body.ynow-market-tw .skin-black .main-header .navbar {
          background-color: transparent !important;
          background-image: none !important;
        }
        /* 台股 logo：容器透明讓國旗透出；金色仍在內層 .ynow-app-title */
        body.ynow-market-tw .skin-black .main-header .logo,
        body.ynow-market-tw .skin-black .main-header .logo:hover {
          background-color: transparent !important;
          background-image: none !important;
        }
        body.ynow-market-tw .skin-black .main-header .logo::before {
          display: none;
        }
        body.ynow-market-tw .main-header .navbar > .sidebar-toggle,
        body.ynow-market-tw .skin-black .main-header .navbar .sidebar-toggle {
          color: transparent !important;
          border: none !important;
          background-color: transparent !important;
        }
        body.ynow-market-tw .main-header .navbar > .sidebar-toggle:before,
        body.ynow-market-tw .skin-black .main-header .navbar .sidebar-toggle:before {
          background-color: transparent !important;
          background-image: linear-gradient(
            to bottom,
            #fff 0px, #fff 2px,
            transparent 2px, transparent 6px,
            #fff 6px, #fff 8px,
            transparent 8px, transparent 12px,
            #fff 12px, #fff 14px
          ) !important;
          box-shadow: none !important;
        }
        body.ynow-market-tw .main-header .navbar > .sidebar-toggle .icon-bar,
        body.ynow-market-tw .skin-black .main-header .navbar .sidebar-toggle .icon-bar {
          display: none !important;
        }
        body.ynow-market-tw .skin-black .main-header .navbar .sidebar-toggle:hover {
          background-color: transparent !important;
          color: transparent !important;
        }
        body.ynow-market-tw .main-header .logo,
        body.ynow-market-tw .main-header .logo:hover {
          filter: none !important;
        }
        body.ynow-market-tw .main-header .logo .ynow-app-title,
        body.ynow-market-tw .main-header .logo .ynow-app-title-fill-inner {
          filter: none !important;
        }
        body.ynow-market-tw .main-header .logo .ynow-app-title-fill-inner {
          text-shadow: 0 1px 3px rgba(0, 0, 0, 0.65);
        }
        body.ynow-market-tw .main-header .navbar .nav > li > a,
        body.ynow-market-tw .ynow-market-header,
        body.ynow-market-tw .ynow-lang-header {
          color: #fff !important;
          text-shadow: 0 1px 3px rgba(0, 0, 0, 0.85);
        }
        .skin-black .wrapper,
        .skin-black .main-sidebar,
        .skin-black .left-side {
          background-color: var(--ynow-ink) !important;
        }
        .skin-black .sidebar-menu > li:hover > a,
        .skin-black .sidebar-menu > li.active > a {
          background: #000 !important;
          border-left-color: #fff !important;
        }
        .box.box-primary { border-top-color: var(--ynow-ink) !important; }
        .box.box-solid.box-primary > .box-header {
          color: #fff !important;
          background: var(--ynow-ink) !important;
          background-color: var(--ynow-ink) !important;
        }
        .box.box-solid.box-primary {
          border: 1px solid var(--ynow-ink) !important;
        }
        /* Asset transmission: no outer frame around the Testing panel. */
        .box.box-solid.box-primary.ynow-atx-panel,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary.ynow-atx-panel,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary.ynow-atx-panel,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary.ynow-atx-panel {
          border: none !important;
          box-shadow: none !important;
        }
        .box.box-solid.box-primary > .box-header a,
        .box.box-solid.box-primary > .box-header .btn {
          color: #fff !important;
        }
        .box.box-info { border-top-color: var(--ynow-ink-soft) !important; }
        .box.box-solid.box-info > .box-header {
          color: #fff !important;
          background: var(--ynow-ink-soft) !important;
          background-color: var(--ynow-ink-soft) !important;
        }
        .box.box-solid.box-info {
          border: 1px solid var(--ynow-ink-soft) !important;
        }
        .box.box-solid.box-info > .box-header a,
        .box.box-solid.box-info > .box-header .btn {
          color: #fff !important;
        }
        .btn-primary {
          background-color: var(--ynow-ink) !important;
          border-color: #000 !important;
          color: #fff !important;
        }
        .btn-primary:hover,
        .btn-primary:focus,
        .btn-primary:active,
        .btn-primary.active,
        .open > .dropdown-toggle.btn-primary {
          background-color: #000 !important;
          border-color: #000 !important;
          color: #fff !important;
        }
        .btn-info {
          background-color: var(--ynow-ink-soft) !important;
          border-color: #222 !important;
          color: #fff !important;
        }
        .btn-info:hover,
        .btn-info:focus,
        .btn-info:active,
        .btn-info.active {
          background-color: #111 !important;
          border-color: #000 !important;
          color: #fff !important;
        }
        /* Valuation「試算」：logo 綠 #249C60、白字（DCF／DDM／RI／P/B／NAV） */
        .btn.ynow-btn-calc,
        .btn.ynow-btn-calc:focus,
        .btn.ynow-btn-calc:active,
        .btn.ynow-btn-calc.active,
        .btn.ynow-btn-calc:visited {
          background-color: var(--ynow-logo-green) !important;
          background-image: none !important;
          border-color: #1e7a4c !important;
          color: #ffffff !important;
          text-shadow: none !important;
        }
        .btn.ynow-btn-calc:hover {
          background-color: #1e8552 !important;
          background-image: none !important;
          border-color: #196b42 !important;
          color: #ffffff !important;
        }
        .btn.ynow-btn-calc .fa,
        .btn.ynow-btn-calc .fas {
          color: #ffffff !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--ynow-ink) !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active > a,
        .nav-tabs-custom > .nav-tabs > li.active:hover > a {
          border-top-color: transparent;
          color: var(--ynow-ink) !important;
        }

        /* --- 估值模型頁色系（對齊 Model Selector 七卡；不含 試算／回復預設 按鈕）--- */
        body.ynow-theme-nav { --ynow-model-accent: var(--ynow-model-nav); }
        body.ynow-theme-dcf { --ynow-model-accent: var(--ynow-model-dcf); }
        body.ynow-theme-ddm { --ynow-model-accent: var(--ynow-model-ddm); }
        body.ynow-theme-ri  { --ynow-model-accent: var(--ynow-model-ri); }
        body.ynow-theme-pb  { --ynow-model-accent: var(--ynow-model-pb); }
        body.ynow-theme-multiples { --ynow-model-accent: var(--ynow-model-multiples); }
        body.ynow-theme-sotp { --ynow-model-accent: var(--ynow-model-sotp); }

        /* Model Selector cards — same accent tokens as page themes */
        .ynow-model-card--nav { --ynow-card-accent: var(--ynow-model-nav); }
        .ynow-model-card--dcf { --ynow-card-accent: var(--ynow-model-dcf); }
        .ynow-model-card--ddm { --ynow-card-accent: var(--ynow-model-ddm); }
        .ynow-model-card--ri { --ynow-card-accent: var(--ynow-model-ri); }
        .ynow-model-card--pb { --ynow-card-accent: var(--ynow-model-pb); }
        .ynow-model-card--multiples { --ynow-card-accent: var(--ynow-model-multiples); }
        .ynow-model-card--sotp { --ynow-card-accent: var(--ynow-model-sotp); }
        .ynow-model-card__icon { color: var(--ynow-card-accent, #333); }
        .ynow-model-card--primary {
          border-color: var(--ynow-card-accent) !important;
          background: #fffaf2 !important;
        }
        .ynow-model-card--primary .ynow-model-card__badge {
          background: var(--ynow-card-accent) !important;
        }

        body.ynow-theme-model .content-wrapper .box.box-primary,
        body.ynow-theme-model .content-wrapper .box.box-info,
        body.ynow-theme-model .content-wrapper .box.box-success,
        body.ynow-theme-model .content-wrapper .box.box-warning,
        body.ynow-theme-model .content-wrapper .box.box-danger {
          border-top-color: var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-info,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-success,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-warning,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-danger {
          border: 1px solid var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary > .box-header,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-info > .box-header,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-success > .box-header,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-warning > .box-header,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-danger > .box-header {
          color: #fff !important;
          background: var(--ynow-model-accent) !important;
          background-color: var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-primary > .box-header .btn,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-info > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-info > .box-header .btn,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-success > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-success > .box-header .btn,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-warning > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-warning > .box-header .btn,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-danger > .box-header a,
        body.ynow-theme-model .content-wrapper .box.box-solid.box-danger > .box-header .btn {
          color: #fff !important;
        }
        body.ynow-theme-model .content-wrapper .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--ynow-model-accent) !important;
        }
        body.ynow-theme-model .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a,
        body.ynow-theme-model .content-wrapper .nav-tabs-custom > .nav-tabs > li.active:hover > a {
          color: var(--ynow-model-accent) !important;
        }
        /* Model KPI palette: result = full accent; siblings = same family, distinct mixes */
        body.ynow-theme-model {
          --ynow-model-kpi-result: var(--ynow-model-accent);
          --ynow-model-kpi-s2: color-mix(in srgb, var(--ynow-model-accent) 72%, #000000);
          --ynow-model-kpi-s3: color-mix(in srgb, var(--ynow-model-accent) 58%, #ffffff);
          --ynow-model-kpi-s4: color-mix(in srgb, var(--ynow-model-accent) 82%, #4a5568);
          --ynow-model-kpi-muted: color-mix(in srgb, var(--ynow-model-accent) 64%, #2c3e50);
        }
        body.ynow-theme-model .content-wrapper .small-box,
        body.ynow-theme-model .tab-content .small-box {
          background-color: var(--ynow-model-kpi-muted) !important;
        }
        body.ynow-theme-model .content-wrapper .info-box.bg-aqua,
        body.ynow-theme-model .content-wrapper .info-box.bg-green,
        body.ynow-theme-model .content-wrapper .info-box.bg-yellow,
        body.ynow-theme-model .content-wrapper .info-box.bg-red,
        body.ynow-theme-model .content-wrapper .info-box.bg-maroon,
        body.ynow-theme-model .content-wrapper .info-box.bg-purple,
        body.ynow-theme-model .content-wrapper .info-box.bg-navy,
        body.ynow-theme-model .content-wrapper .info-box.bg-teal,
        body.ynow-theme-model .content-wrapper .info-box.bg-olive,
        body.ynow-theme-model .content-wrapper .info-box.bg-orange,
        body.ynow-theme-model .content-wrapper .info-box.bg-light-blue,
        body.ynow-theme-model .content-wrapper .info-box.bg-black,
        body.ynow-theme-model .content-wrapper .info-box.bg-blue {
          background-color: var(--ynow-model-kpi-muted) !important;
        }
        body.ynow-theme-model .content-wrapper .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-muted) !important;
        }
        body.ynow-theme-model .ynow-model-kpi-result .small-box,
        body.ynow-theme-model .ynow-model-kpi-result .info-box,
        body.ynow-theme-model .ynow-model-kpi-result .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-result) !important;
        }
        body.ynow-theme-model .ynow-model-kpi-tone-2 .small-box,
        body.ynow-theme-model .ynow-model-kpi-tone-2 .info-box,
        body.ynow-theme-model .ynow-model-kpi-tone-2 .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-s2) !important;
        }
        body.ynow-theme-model .ynow-model-kpi-tone-3 .small-box,
        body.ynow-theme-model .ynow-model-kpi-tone-3 .info-box,
        body.ynow-theme-model .ynow-model-kpi-tone-3 .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-s3) !important;
        }
        body.ynow-theme-model .ynow-model-kpi-tone-4 .small-box,
        body.ynow-theme-model .ynow-model-kpi-tone-4 .info-box,
        body.ynow-theme-model .ynow-model-kpi-tone-4 .info-box .info-box-icon {
          background-color: var(--ynow-model-kpi-s4) !important;
        }
        body.ynow-theme-model .ynow-model-kpi-result-num.ynow-htcdi-flow {
          background-image: var(--ynow-logo-flow-gradient, linear-gradient(105deg, #0C5484 0%, #1AA8B8 50%, #249C60 100%)) !important;
          -webkit-background-clip: text !important;
          background-clip: text !important;
          -webkit-text-fill-color: transparent !important;
          color: transparent !important;
          animation: ynow-logo-flow 2.6s ease-in-out infinite;
          font-weight: 800;
        }
        body.ynow-theme-model .small-box .inner h3 .ynow-model-kpi-result-num.ynow-htcdi-flow,
        body.ynow-theme-model .info-box .info-box-number .ynow-model-kpi-result-num.ynow-htcdi-flow {
          color: transparent !important;
          -webkit-text-fill-color: transparent !important;
        }
        /* Sidebar active rail matches the open model tab */
        body.ynow-theme-model.skin-black .main-sidebar .sidebar-menu > li.active > a,
        body.ynow-theme-model.skin-black .main-sidebar .sidebar-menu > li.menu-open > a,
        body.ynow-theme-model.skin-black .main-sidebar .sidebar-menu .treeview-menu > li.active > a {
          border-left-color: var(--ynow-model-accent) !important;
        }
        /* 共用 Composite／years header：左側色條提示目前模型 */
        body.ynow-theme-model .ynow-header-composite-row,
        body.ynow-theme-model .ynow-dcf-mode-row,
        body.ynow-theme-model .ynow-header-years-suggest-row {
          border-left: 4px solid var(--ynow-model-accent);
          padding-left: 6px;
          margin-left: 0;
        }
        /* Waiting for valuation…：底色跟隨目前評價模型分頁主題色 */
        body.ynow-theme-model .ynow-header-composite-row .ynow-waiting-val.alert {
          background-color: var(--ynow-model-accent) !important;
          border-color: var(--ynow-model-accent) !important;
          color: #ffffff !important;
        }
        /* 明確排除：試算／回復預設按鈕色不變 */
        body.ynow-theme-model .btn.ynow-btn-calc,
        body.ynow-theme-model .btn.ynow-btn-calc:hover,
        body.ynow-theme-model .btn.ynow-btn-calc:focus,
        body.ynow-theme-model .btn.ynow-btn-calc:active,
        body.ynow-theme-model .btn.ynow-btn-calc.active {
          background-color: var(--ynow-logo-green) !important;
          border-color: #1e7a4c !important;
          color: #ffffff !important;
        }
        body.ynow-theme-model .btn.ynow-btn-reset,
        body.ynow-theme-model .btn.ynow-btn-reset:hover,
        body.ynow-theme-model .btn.ynow-btn-reset:focus,
        body.ynow-theme-model .btn.ynow-btn-reset:active {
          background-color: #7f8c8d !important;
          border-color: #6c757d !important;
          color: #ffffff !important;
        }

        /* --- Blue Chip：藍海色系（TW／US 共用，不隨市場模式變色）---
           Palette: #0F5A90 → #0A72AE → #33B1D2 → #65AEC7 → #8CD0EB */
        body.ynow-theme-bluechip {
          --ynow-bluechip-deep: #0F5A90;
          --ynow-bluechip-ocean: #0A72AE;
          --ynow-bluechip-cyan: #33B1D2;
          --ynow-bluechip-sky: #65AEC7;
          --ynow-bluechip-light: #8CD0EB;
          --ynow-bluechip-accent: var(--ynow-bluechip-ocean);
          --ynow-bluechip-accent-deep: var(--ynow-bluechip-deep);
          --ynow-bluechip-gradient: linear-gradient(
            105deg,
            #0F5A90 0%,
            #0A72AE 28%,
            #33B1D2 55%,
            #65AEC7 78%,
            #8CD0EB 100%
          );
          --ynow-bluechip-wash: linear-gradient(
            180deg,
            rgba(15, 90, 144, 0.09) 0%,
            rgba(51, 177, 210, 0.06) 42%,
            rgba(140, 208, 235, 0.04) 100%
          );
          accent-color: var(--ynow-bluechip-ocean);
        }
        body.ynow-theme-bluechip .content-wrapper {
          background-color: #f3f8fc !important;
          background-image: var(--ynow-bluechip-wash) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .box.box-primary,
        body.ynow-theme-bluechip .content-wrapper .box.box-info,
        body.ynow-theme-bluechip .content-wrapper .box.box-success,
        body.ynow-theme-bluechip .content-wrapper .box.box-warning {
          border-top-color: var(--ynow-bluechip-accent) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-info,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-success,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-warning {
          border: 1px solid var(--ynow-bluechip-accent) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary > .box-header,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-info > .box-header,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-success > .box-header,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-warning > .box-header {
          color: #fff !important;
          background: var(--ynow-bluechip-gradient) !important;
          background-color: var(--ynow-bluechip-accent) !important;
          border-bottom-color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary > .box-header a,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-primary > .box-header .btn,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-info > .box-header a,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-info > .box-header .btn,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-success > .box-header a,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-success > .box-header .btn,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-warning > .box-header a,
        body.ynow-theme-bluechip .content-wrapper .box.box-solid.box-warning > .box-header .btn {
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--ynow-bluechip-cyan) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a,
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active:hover > a {
          color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li > a > .fa,
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li > a > .fas {
          color: var(--ynow-bluechip-ocean);
        }
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a > .fa,
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a > .fas {
          color: var(--ynow-bluechip-deep);
        }
        /* tabBox title strip */
        body.ynow-theme-bluechip .content-wrapper .nav-tabs-custom > .nav-tabs > li.header {
          color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .ynow-lab-im-pool-controls {
          border-left: 4px solid var(--ynow-bluechip-cyan);
          padding: 10px 12px;
          margin-bottom: 14px;
          border-radius: 0 6px 6px 0;
          background: linear-gradient(
            90deg,
            rgba(15, 90, 144, 0.08) 0%,
            rgba(140, 208, 235, 0.12) 100%
          );
        }
        /* Primary actions — ocean (incl. former green「搜尋績優股」) */
        body.ynow-theme-bluechip .content-wrapper .btn-success,
        body.ynow-theme-bluechip .content-wrapper .btn-success:focus,
        body.ynow-theme-bluechip .content-wrapper .btn-success:active,
        body.ynow-theme-bluechip .content-wrapper .btn-success.active,
        body.ynow-theme-bluechip .content-wrapper #lab_im_run_fscore,
        body.ynow-theme-bluechip .content-wrapper #lab_im_run_fscore:focus,
        body.ynow-theme-bluechip .content-wrapper #lab_im_run_fscore:active {
          background-color: var(--ynow-bluechip-ocean) !important;
          border-color: var(--ynow-bluechip-deep) !important;
          color: #fff !important;
          background-image: none !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-success:hover,
        body.ynow-theme-bluechip .content-wrapper #lab_im_run_fscore:hover {
          background-color: var(--ynow-bluechip-deep) !important;
          border-color: var(--ynow-bluechip-deep) !important;
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-primary,
        body.ynow-theme-bluechip .content-wrapper .btn-info {
          background-color: var(--ynow-bluechip-ocean) !important;
          border-color: var(--ynow-bluechip-deep) !important;
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-primary:hover,
        body.ynow-theme-bluechip .content-wrapper .btn-info:hover {
          background-color: var(--ynow-bluechip-deep) !important;
          border-color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-default {
          border-color: var(--ynow-bluechip-sky) !important;
          color: var(--ynow-bluechip-deep) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .btn-default:hover {
          background-color: rgba(140, 208, 235, 0.25) !important;
          border-color: var(--ynow-bluechip-cyan) !important;
          color: var(--ynow-bluechip-deep) !important;
        }
        /* Form focus / selectize / radio */
        body.ynow-theme-bluechip .content-wrapper .form-control:focus,
        body.ynow-theme-bluechip .content-wrapper .selectize-input.focus {
          border-color: var(--ynow-bluechip-cyan) !important;
          box-shadow: 0 0 0 2px rgba(51, 177, 210, 0.25) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .radio input[type="radio"],
        body.ynow-theme-bluechip .content-wrapper .checkbox input[type="checkbox"] {
          accent-color: var(--ynow-bluechip-ocean);
        }
        body.ynow-theme-bluechip .content-wrapper a:not(.btn) {
          color: var(--ynow-bluechip-ocean);
        }
        body.ynow-theme-bluechip .content-wrapper a:not(.btn):hover {
          color: var(--ynow-bluechip-deep);
        }
        body.ynow-theme-bluechip .content-wrapper .ynow-lab-im-eq-explain {
          background: rgba(140, 208, 235, 0.18);
          border-color: var(--ynow-bluechip-sky);
          color: #2a4a5c;
        }
        body.ynow-theme-bluechip .content-wrapper .ynow-lab-im-eq-explain b {
          color: var(--ynow-bluechip-deep);
        }
        body.ynow-theme-bluechip .content-wrapper .label-primary,
        body.ynow-theme-bluechip .content-wrapper .badge-primary,
        body.ynow-theme-bluechip .content-wrapper .label-info,
        body.ynow-theme-bluechip .content-wrapper .badge-info {
          background-color: var(--ynow-bluechip-ocean) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .progress-bar,
        body.ynow-theme-bluechip .content-wrapper .progress-bar-success,
        body.ynow-theme-bluechip .content-wrapper .progress-bar-info {
          background-color: var(--ynow-bluechip-cyan) !important;
        }
        body.ynow-theme-bluechip .content-wrapper .small-box,
        body.ynow-theme-bluechip .content-wrapper .small-box.bg-aqua,
        body.ynow-theme-bluechip .content-wrapper .small-box.bg-blue,
        body.ynow-theme-bluechip .content-wrapper .small-box.bg-green {
          background-color: var(--ynow-bluechip-ocean) !important;
        }
        /* Sidebar active rail stays ocean on Blue Chip (both markets) */
        body.ynow-theme-bluechip.skin-black .main-sidebar .sidebar-menu > li.active > a,
        body.ynow-theme-bluechip.skin-black .main-sidebar .sidebar-menu > li.menu-open > a {
          border-left-color: var(--ynow-bluechip-cyan) !important;
        }
        /* DT / tables light ocean header tint */
        body.ynow-theme-bluechip .content-wrapper .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        body.ynow-theme-bluechip .content-wrapper .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          background: var(--ynow-bluechip-ocean) !important;
          border-color: var(--ynow-bluechip-deep) !important;
          color: #fff !important;
        }
        body.ynow-theme-bluechip .content-wrapper table.table > thead > tr > th {
          border-bottom-color: var(--ynow-bluechip-sky) !important;
          color: var(--ynow-bluechip-deep);
        }
        /* Spinner / loaders */
        body.ynow-theme-bluechip .content-wrapper .load-container .loader,
        body.ynow-theme-bluechip .content-wrapper .shiny-spinner-output-container .load-container {
          color: var(--ynow-bluechip-cyan);
        }

        /* --- Testing / Asset transmission：森林色系（TW／US 共用；圖表綠漲紅跌不變）---
           Palette: #464704 → #9CA889 → #F3F5E7 → #B7A78C → #423926 */
        body.ynow-theme-atx {
          --ynow-atx-moss: #464704;
          --ynow-atx-sage: #9CA889;
          --ynow-atx-ivory: #F3F5E7;
          --ynow-atx-khaki: #B7A78C;
          --ynow-atx-brown: #423926;
          --ynow-atx-accent: var(--ynow-atx-moss);
          --ynow-atx-accent-deep: var(--ynow-atx-brown);
          --ynow-atx-gradient: linear-gradient(
            105deg,
            #423926 0%,
            #464704 28%,
            #6e7340 55%,
            #9CA889 78%,
            #B7A78C 100%
          );
          --ynow-atx-wash: linear-gradient(
            180deg,
            rgba(70, 71, 4, 0.08) 0%,
            rgba(156, 168, 137, 0.10) 42%,
            rgba(243, 245, 231, 0.55) 100%
          );
          accent-color: var(--ynow-atx-moss);
        }
        body.ynow-theme-atx .content-wrapper {
          background-color: var(--ynow-atx-ivory) !important;
          background-image: var(--ynow-atx-wash) !important;
        }
        body.ynow-theme-atx .content-wrapper h2,
        body.ynow-theme-atx .content-wrapper h2 b,
        body.ynow-theme-atx .content-wrapper #ynow_atx_page_title {
          color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper #ynow_atx_page_sub,
        body.ynow-theme-atx .content-wrapper #ynow_atx_box_body {
          color: #5c5346 !important;
        }
        body.ynow-theme-atx .content-wrapper hr {
          border-top-color: var(--ynow-atx-khaki) !important;
        }
        body.ynow-theme-atx .content-wrapper .box.box-primary,
        body.ynow-theme-atx .content-wrapper .box.box-info,
        body.ynow-theme-atx .content-wrapper .box.box-success,
        body.ynow-theme-atx .content-wrapper .box.box-warning {
          border-top-color: var(--ynow-atx-accent) !important;
        }
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-info,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-success,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-warning {
          border: 1px solid var(--ynow-atx-khaki) !important;
        }
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary > .box-header,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-info > .box-header,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-success > .box-header,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-warning > .box-header {
          color: #fff !important;
          background: var(--ynow-atx-gradient) !important;
          background-color: var(--ynow-atx-accent) !important;
          border-bottom-color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary > .box-header a,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-primary > .box-header .btn,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-info > .box-header a,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-info > .box-header .btn,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-success > .box-header a,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-success > .box-header .btn,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-warning > .box-header a,
        body.ynow-theme-atx .content-wrapper .box.box-solid.box-warning > .box-header .btn {
          color: #fff !important;
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--ynow-atx-sage) !important;
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a,
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active:hover > a {
          color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li > a > .fa,
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li > a > .fas {
          color: var(--ynow-atx-moss);
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a > .fa,
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.active > a > .fas {
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .nav-tabs-custom > .nav-tabs > li.header {
          color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper .btn-success,
        body.ynow-theme-atx .content-wrapper .btn-success:focus,
        body.ynow-theme-atx .content-wrapper .btn-success:active,
        body.ynow-theme-atx .content-wrapper .btn-success.active,
        body.ynow-theme-atx .content-wrapper .btn-primary,
        body.ynow-theme-atx .content-wrapper .btn-info {
          background-color: var(--ynow-atx-moss) !important;
          border-color: var(--ynow-atx-brown) !important;
          color: #fff !important;
          background-image: none !important;
        }
        body.ynow-theme-atx .content-wrapper .btn-success:hover,
        body.ynow-theme-atx .content-wrapper .btn-primary:hover,
        body.ynow-theme-atx .content-wrapper .btn-info:hover {
          background-color: var(--ynow-atx-brown) !important;
          border-color: var(--ynow-atx-brown) !important;
          color: #fff !important;
        }
        body.ynow-theme-atx .content-wrapper .btn-default {
          border-color: var(--ynow-atx-khaki) !important;
          color: var(--ynow-atx-brown) !important;
          background-color: rgba(243, 245, 231, 0.85) !important;
        }
        body.ynow-theme-atx .content-wrapper .btn-default:hover {
          background-color: rgba(156, 168, 137, 0.28) !important;
          border-color: var(--ynow-atx-sage) !important;
          color: var(--ynow-atx-brown) !important;
        }
        body.ynow-theme-atx .content-wrapper .form-control:focus,
        body.ynow-theme-atx .content-wrapper .selectize-input.focus {
          border-color: var(--ynow-atx-sage) !important;
          box-shadow: 0 0 0 2px rgba(156, 168, 137, 0.35) !important;
        }
        body.ynow-theme-atx .content-wrapper .radio input[type="radio"],
        body.ynow-theme-atx .content-wrapper .checkbox input[type="checkbox"] {
          accent-color: var(--ynow-atx-moss);
        }
        body.ynow-theme-atx .content-wrapper a:not(.btn) {
          color: var(--ynow-atx-moss);
        }
        body.ynow-theme-atx .content-wrapper a:not(.btn):hover {
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .label-primary,
        body.ynow-theme-atx .content-wrapper .badge-primary,
        body.ynow-theme-atx .content-wrapper .label-info,
        body.ynow-theme-atx .content-wrapper .badge-info {
          background-color: var(--ynow-atx-moss) !important;
        }
        body.ynow-theme-atx .content-wrapper .progress-bar,
        body.ynow-theme-atx .content-wrapper .progress-bar-success,
        body.ynow-theme-atx .content-wrapper .progress-bar-info {
          background-color: var(--ynow-atx-sage) !important;
        }
        body.ynow-theme-atx .content-wrapper .small-box,
        body.ynow-theme-atx .content-wrapper .small-box.bg-aqua,
        body.ynow-theme-atx .content-wrapper .small-box.bg-blue,
        body.ynow-theme-atx .content-wrapper .small-box.bg-green {
          background-color: var(--ynow-atx-moss) !important;
        }
        body.ynow-theme-atx.skin-black .main-sidebar .sidebar-menu > li.active > a,
        body.ynow-theme-atx.skin-black .main-sidebar .sidebar-menu > li.menu-open > a {
          border-left-color: var(--ynow-atx-sage) !important;
        }
        body.ynow-theme-atx .content-wrapper .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        body.ynow-theme-atx .content-wrapper .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          background: var(--ynow-atx-moss) !important;
          border-color: var(--ynow-atx-brown) !important;
          color: #fff !important;
        }
        body.ynow-theme-atx .content-wrapper table.table > thead > tr > th {
          border-bottom-color: var(--ynow-atx-khaki) !important;
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .load-container .loader,
        body.ynow-theme-atx .content-wrapper .shiny-spinner-output-container .load-container {
          color: var(--ynow-atx-sage);
        }
        /* ATX chrome follows forest palette; map canvas + move colors stay as designed. */
        body.ynow-theme-atx .content-wrapper .ynow-atx-summary,
        body.ynow-theme-atx .content-wrapper .ynow-atx h4 {
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-legend,
        body.ynow-theme-atx .content-wrapper .ynow-atx-note,
        body.ynow-theme-atx .content-wrapper .ynow-atx-status {
          color: #5c5346;
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-regime,
        body.ynow-theme-atx .content-wrapper .ynow-atx-path {
          background: rgba(243, 245, 231, 0.92);
          border-color: var(--ynow-atx-khaki);
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-regime h4,
        body.ynow-theme-atx .content-wrapper .ynow-atx-path h4 {
          color: var(--ynow-atx-brown);
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-load-bar {
          background: linear-gradient(
            90deg,
            var(--ynow-atx-moss) 0%,
            var(--ynow-atx-sage) 55%,
            var(--ynow-atx-khaki) 100%
          );
        }
        body.ynow-theme-atx .content-wrapper .ynow-atx-load-track {
          background: rgba(243, 245, 231, 0.22);
          box-shadow: inset 0 0 0 1px rgba(183, 167, 140, 0.35);
        }

        /* Dashboard 損益表／現金流量表：圖標與選取頂條對齊 logo 金 */
        #dashboard_fin_report > .nav-tabs > li > a[data-value="Income Statement"] > .fa,
        #dashboard_fin_report > .nav-tabs > li > a[data-value="Income Statement"] > .fas,
        #dashboard_fin_report > .nav-tabs > li > a[data-value="Cash Flow"] > .fa,
        #dashboard_fin_report > .nav-tabs > li > a[data-value="Cash Flow"] > .fas {
          color: var(--ynow-gold-deep) !important;
        }
        #dashboard_fin_report > .nav-tabs > li.active > a[data-value="Income Statement"] > .fa,
        #dashboard_fin_report > .nav-tabs > li.active > a[data-value="Income Statement"] > .fas,
        #dashboard_fin_report > .nav-tabs > li.active > a[data-value="Cash Flow"] > .fa,
        #dashboard_fin_report > .nav-tabs > li.active > a[data-value="Cash Flow"] > .fas {
          color: var(--ynow-gold) !important;
        }
        #dashboard_fin_report > .nav-tabs > li.active:has(> a[data-value="Income Statement"]),
        #dashboard_fin_report > .nav-tabs > li.active:has(> a[data-value="Cash Flow"]) {
          border-top-color: var(--ynow-gold) !important;
        }
        @media (max-width: 767px) {
          #dashboard_fin_report > .nav-tabs > li > a[data-value="Income Statement"] > .fa,
          #dashboard_fin_report > .nav-tabs > li > a[data-value="Income Statement"] > .fas,
          #dashboard_fin_report > .nav-tabs > li > a[data-value="Cash Flow"] > .fa,
          #dashboard_fin_report > .nav-tabs > li > a[data-value="Cash Flow"] > .fas {
            font-size: 13px;
          }
        }
        /* 損益表／現金流量表圖表區：金色頂條 chrome（對齊 logo） */
        .ynow-fs-chart {
          border-top: 3px solid var(--ynow-gold);
          margin-top: 6px;
          padding-top: 10px;
          background: linear-gradient(180deg, rgba(245, 197, 24, 0.06) 0%, rgba(255, 255, 255, 0) 28px);
        }
        .ynow-fs-chart-heading {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0 0 8px 0;
          font-size: 13px;
          font-weight: 700;
          color: var(--ynow-gold-ink);
          letter-spacing: 0.01em;
        }
        .ynow-fs-chart-heading > .fa,
        .ynow-fs-chart-heading > .fas {
          color: var(--ynow-gold-deep);
          font-size: 14px;
          width: 1.2em;
          text-align: center;
        }
        @media (max-width: 767px) {
          .ynow-fs-chart {
            border-top-width: 2px;
            padding-top: 8px;
          }
          .ynow-fs-chart-heading {
            font-size: 12px;
          }
        }
        .nav-pills > li.active > a,
        .nav-pills > li.active > a:hover,
        .nav-pills > li.active > a:focus {
          background-color: var(--ynow-ink) !important;
        }
        .pagination > .active > a,
        .pagination > .active > span,
        .pagination > .active > a:hover,
        .pagination > .active > span:hover {
          background-color: var(--ynow-ink) !important;
          border-color: var(--ynow-ink) !important;
        }
        a { color: var(--ynow-ink-soft); }
        a:hover, a:focus { color: #000; }
        .label-primary, .badge-primary {
          background-color: var(--ynow-ink) !important;
        }
        /* Sidebar 推薦／備選 badges — overflow:visible so flex cannot clip the chip */
        .sidebar-menu > li > a,
        .sidebar-menu .treeview-menu > li > a {
          display: flex !important;
          align-items: center !important;
          flex-wrap: nowrap !important;
          overflow: visible !important;
        }
        .sidebar-menu > li > a > span:not(.pull-right-container):not(.ynow-rec-slot),
        .sidebar-menu .treeview-menu > li > a > span:not(.pull-right-container):not(.ynow-rec-slot),
        .sidebar-menu > li > a > .ynow-menu-label,
        .sidebar-menu .treeview-menu > li > a > .ynow-menu-label {
          flex: 1 1 auto;
          min-width: 0;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .sidebar-menu > li > a > .pull-right-container,
        .sidebar-menu .treeview-menu > li > a > .pull-right-container {
          float: none !important;
          position: static !important;
          margin-left: 4px !important;
          margin-right: 0 !important;
          display: inline-flex !important;
          align-items: center !important;
          flex: 0 0 auto;
          gap: 4px;
        }
        .sidebar-menu > li > a > .pull-right-container > .fa.pull-right,
        .sidebar-menu > li > a > .pull-right-container > .fas.pull-right,
        .sidebar-menu > li > a > .fa.pull-right,
        .sidebar-menu > li > a > .fas.pull-right,
        .sidebar-menu > li > a > .fa-angle-left,
        .sidebar-menu > li > a > .fas.fa-angle-left {
          float: none !important;
          position: static !important;
          top: auto !important;
          right: auto !important;
          margin: 0 0 0 4px !important;
          flex: 0 0 auto;
          transform: none !important;
        }
        .sidebar-menu .ynow-rec-slot {
          display: inline-flex !important;
          align-items: center !important;
          flex: 0 0 auto !important;
          margin-left: 6px !important;
          max-width: none !important;
          overflow: visible !important;
        }
        .sidebar-menu small.ynow-sidebar-badge {
          display: inline-block !important;
          visibility: visible !important;
          opacity: 1 !important;
          float: none !important;
          position: static !important;
          flex: 0 0 auto !important;
          align-self: center;
          margin: 0 !important;
          padding: 2px 6px !important;
          white-space: nowrap !important;
          line-height: 1.2 !important;
          font-size: 10px !important;
          font-weight: 700 !important;
          border-radius: 3px !important;
          max-width: none !important;
          transform: none !important;
          z-index: 5;
        }
        .sidebar-menu small.ynow-sidebar-badge.ynow-rec-primary {
          background-color: #dd4b39 !important;
          color: #ffffff !important;
        }
        .sidebar-menu small.ynow-sidebar-badge.ynow-rec-secondary {
          background-color: #6c757d !important;
          color: #ffffff !important;
        }
        .sidebar-menu .treeview-menu {
          overflow: visible !important;
        }
        .progress-bar-primary {
          background-color: var(--ynow-ink) !important;
        }
        .text-primary { color: var(--ynow-ink) !important; }
        .bg-primary { background-color: var(--ynow-ink) !important; }
        /* Do not remap .bg-blue — KPI band 「優於區間」uses AdminLTE blue */

        #shiny-tab-get_started > h2 { font-weight: 800 !important; }
        /* 側邊欄：僅目前選取頁面粗體（勿固定加粗基礎設定） */
        .sidebar-menu > li > a {
          font-weight: 400 !important;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li.menu-open > a {
          font-weight: 700 !important;
        }
        /* 側邊欄圖示固定寬度欄：英文標籤左緣對齊（父層＋子分頁；勿動 pull-right 展開箭頭） */
        .sidebar-menu > li > a > .fa:not(.pull-right),
        .sidebar-menu > li > a > .fas:not(.pull-right),
        .sidebar-menu > li > a > .far:not(.pull-right),
        .sidebar-menu > li > a > .glyphicon:not(.pull-right),
        .sidebar-menu > li > a > .ion:not(.pull-right),
        .sidebar-menu .treeview-menu > li > a > .fa:not(.pull-right),
        .sidebar-menu .treeview-menu > li > a > .fas:not(.pull-right),
        .sidebar-menu .treeview-menu > li > a > .far:not(.pull-right),
        .sidebar-menu .treeview-menu > li > a > .glyphicon:not(.pull-right),
        .sidebar-menu .treeview-menu > li > a > .ion:not(.pull-right) {
          display: inline-block;
          flex: 0 0 1.35em;
          width: 1.35em;
          min-width: 1.35em;
          margin-right: 8px;
          text-align: center;
          vertical-align: middle;
        }
        /* 估值分類子分頁標籤：黑底白字（僅 treeview-menu，不改父層分類標籤） */
        .skin-black .sidebar-menu .treeview-menu {
          background-color: #000 !important;
          padding-top: 0 !important;
          padding-bottom: 0 !important;
        }
        .skin-black .sidebar-menu .treeview-menu > li > a {
          background-color: #000 !important;
          color: #fff !important;
          border-left: 3px solid transparent;
        }
        .skin-black .sidebar-menu .treeview-menu > li > a > .fa,
        .skin-black .sidebar-menu .treeview-menu > li > a > .glyphicon,
        .skin-black .sidebar-menu .treeview-menu > li > a > .ion {
          color: #fff !important;
        }
        .skin-black .sidebar-menu .treeview-menu > li > a:hover {
          background-color: #1a1a1a !important;
          color: #fff !important;
          border-left-color: rgba(255, 255, 255, 0.55);
        }
        .skin-black .sidebar-menu .treeview-menu > li.active > a,
        .skin-black .sidebar-menu .treeview-menu > li.active > a:hover {
          background-color: #000 !important;
          color: #fff !important;
          font-weight: 700 !important;
          border-left-color: #fff;
        }
        /* Snapshot：側邊欄內容底部低調捷徑（正常文件流，避免 absolute 跑位） */
        .ynow-sidebar-snapshot-foot {
          margin-top: 8px !important;
          margin-bottom: 10px !important;
          padding: 0 10px !important;
          text-align: left !important;
        }
        .ynow-sidebar-snapshot-link {
          display: inline-block;
          font-size: 11px !important;
          font-weight: 400 !important;
          color: rgba(255,255,255,0.45) !important;
          text-decoration: none !important;
          padding: 2px 4px;
          border-radius: 3px;
          opacity: 0.75;
          line-height: 1.2;
        }
        .ynow-sidebar-snapshot-link:hover {
          color: rgba(255,255,255,0.85) !important;
          opacity: 1;
          background: rgba(255,255,255,0.06);
        }
        .ynow-sidebar-feedback-link { margin-left: 8px; }
        .ynow-sidebar-feedback-link { margin-left: 8px; }
        .ynow-sidebar-test-link { margin-left: 8px; }
        /* 側欄完整 LOGO：Recent Search 橫線下、Download Report 正上方；等比例放大 */
        .ynow-sidebar-brand {
          display: flex;
          justify-content: center;
          align-items: center;
          padding: 8px 10px 6px 10px;
          cursor: pointer;
          user-select: none;
          outline: none;
        }
        .ynow-sidebar-brand:hover .ynow-sidebar-logo-full {
          opacity: 1;
        }
        .ynow-sidebar-brand:focus-visible {
          box-shadow: inset 0 0 0 2px rgba(255,255,255,0.35);
          border-radius: 4px;
        }
        .ynow-sidebar-brand-inner {
          position: relative;
          display: inline-flex;
          justify-content: center;
          align-items: center;
          width: min(200px, 92%);
          max-width: 100%;
        }
        .ynow-sidebar-logo-full {
          width: 100%;
          height: auto;
          max-width: 100%;
          object-fit: contain;
          display: block;
          opacity: 0.98;
        }
        .ynow-lite-badge {
          display: none;
          position: absolute;
          right: 2px;
          bottom: 2px;
          font-size: 9px;
          font-weight: 700;
          letter-spacing: 0.06em;
          line-height: 1;
          padding: 2px 4px;
          color: #1a1a1a;
          background: #f0c14b;
          border-radius: 2px;
          box-shadow: 0 0 0 1px rgba(0,0,0,0.25);
          pointer-events: none;
        }
        body.ynow-lite .ynow-lite-badge {
          display: inline-block;
        }
        .ynow-bblab--report {
          max-width: var(--ynow-page-max, 1200px);
          margin: 0 auto 36px;
          padding: 8px 4px 24px;
          width: 100%;
          box-sizing: border-box;
        }
        .ynow-bblab-report__cover {
          margin: 0 0 18px 0;
          padding: 8px 0 18px 0;
          border-bottom: 2px solid #0C5484;
        }
        .ynow-bblab-report__kicker {
          margin: 0 0 6px 0;
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: #0C5484;
        }
        .ynow-bblab-report__title {
          margin: 0 0 10px 0;
          font-size: 28px;
          font-weight: 700;
          line-height: 1.2;
          color: #12263a;
        }
        .ynow-bblab-report__deck {
          margin: 0 0 14px 0;
          max-width: 46em;
          font-size: 14px;
          line-height: 1.55;
          color: #4a5560;
        }
        .ynow-bblab-report__meta {
          display: flex;
          flex-wrap: wrap;
          gap: 16px 28px;
          margin-top: 14px;
        }
        .ynow-bblab-report__meta-item { min-width: 140px; }
        .ynow-bblab-report__meta-item--grow { flex: 1 1 220px; }
        .ynow-bblab-report__meta-lab {
          display: block;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #6a7680;
          margin-bottom: 2px;
        }
        .ynow-bblab-report__meta-val {
          font-size: 15px;
          font-weight: 600;
          color: #12263a;
        }
        .ynow-bblab-report__toolbar {
          margin: 0 0 22px 0;
          padding: 12px 14px 6px;
          background: #f6f8fa;
          border: 1px solid #d9dee3;
          border-radius: 4px;
        }
        .ynow-bblab-report__toolbar-check { padding-top: 22px; }
        /* Shiny defaults .shiny-input-container to 300px — too narrow for the GM fallback label. */
        .ynow-bblab-report__toolbar-check .shiny-input-container,
        .ynow-bblab-report__toolbar-check .checkbox,
        .ynow-bblab-report__toolbar-check label {
          width: auto !important;
          max-width: 100% !important;
        }
        .ynow-bblab-report__toolbar-check label,
        .ynow-bblab-report__toolbar-check #ynow_bblab_fallback_gm_label {
          white-space: nowrap;
        }
        .ynow-bblab-report__hint {
          margin: 0 0 6px 0;
          font-size: 12px;
          color: #6a7680;
        }
        .ynow-bblab-report__section {
          margin: 0 0 28px 0;
          padding: 0 0 8px 0;
        }
        .ynow-bblab-report__section--muted {
          padding: 12px 14px;
          background: #fafbfc;
          border: 1px solid #e4e8ec;
          border-radius: 4px;
        }
        .ynow-bblab-report__section-head {
          display: flex;
          align-items: baseline;
          gap: 10px;
          margin: 0 0 8px 0;
          padding-bottom: 6px;
          border-bottom: 1px solid #d9dee3;
        }
        .ynow-bblab-report__section-num {
          font-size: 13px;
          font-weight: 700;
          color: #0C5484;
          min-width: 1.6em;
        }
        .ynow-bblab-report__section-title {
          margin: 0;
          font-size: 18px;
          font-weight: 700;
          color: #12263a;
        }
        .ynow-bblab-report__section-help {
          margin: 0 0 12px 0;
          font-size: 13px;
          line-height: 1.5;
          color: #5a6570;
        }
        .ynow-bblab-report__section-body { margin-top: 4px; }
        .ynow-bblab__listed-notice {
          margin: 0 0 14px 0;
          padding: 8px 12px;
          font-size: 13px;
          font-weight: 600;
          line-height: 1.45;
          color: #1b3a4b;
          background: #eef6f8;
          border: 1px solid #1AA8B8;
          border-left-width: 4px;
          border-radius: 3px;
        }
        .ynow-bblab__listed-scope {
          margin: 8px 0 0 0;
          font-weight: 600;
          color: #1b3a4b;
        }
        .ynow-bblab-card {
          border: 1px solid #d9dee3;
          border-radius: 6px;
          padding: 10px 12px;
          margin-bottom: 12px;
          background: #fff;
          min-height: 160px;
        }
        .ynow-bblab-card--focus { outline: 2px solid #1AA8B8; }
        .ynow-bblab-card--neutral { background: #f4f6f7; border-color: #c5ccd1; }
        .ynow-bblab-card__metrics { padding-left: 1.1em; margin: 8px 0 0 0; }
        .ynow-bblab-tag {
          display: inline-block;
          margin-left: 6px;
          font-size: 10px;
          padding: 1px 6px;
          border-radius: 2px;
          background: #eef2f4;
          color: #555;
        }
        .ynow-bblab-recon--pass { color: #1e7a46; font-weight: 700; }
        .ynow-bblab-recon--fail { color: #b33b3b; font-weight: 700; }
        .ynow-bblab-chapter__num {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 1.55em;
          height: 1.55em;
          margin-right: 8px;
          font-size: 12px;
          font-weight: 700;
          color: #fff;
          background: #0C5484;
          border-radius: 50%;
          vertical-align: middle;
        }
        .ynow-bblab-subhead {
          margin: 18px 0 6px 0;
          font-size: 15px;
          font-weight: 700;
          color: #0C5484;
        }
        .ynow-bblab-kpis {
          display: flex;
          flex-wrap: wrap;
          gap: 10px 14px;
          margin: 8px 0 4px 0;
        }
        .ynow-bblab-kpi {
          min-width: 140px;
          padding: 8px 10px;
          background: #f7fafb;
          border: 1px solid #d5dee3;
          border-radius: 4px;
        }
        .ynow-bblab-kpi__lab {
          display: block;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.02em;
          color: #4a5b66;
          margin-bottom: 2px;
        }
        .ynow-bblab-kpi__val {
          font-size: 16px;
          font-weight: 700;
          color: #1a1a1a;
        }
        .ynow-bblab-kpi__unit {
          font-size: 11px;
          color: #666;
          margin-left: 4px;
        }
        .ynow-bblab-is {
          margin: 8px 0 4px 0;
          max-width: 720px;
        }
        .ynow-bblab-is__meta {
          display: flex;
          flex-wrap: wrap;
          gap: 12px 28px;
          margin: 0 0 10px 0;
          color: #4a5b66;
          font-size: 13px;
        }
        .ynow-bblab-is__meta-lab {
          font-weight: 700;
          margin-right: 6px;
        }
        .ynow-bblab-is__table {
          width: 100%;
          border-collapse: collapse;
          font-variant-numeric: tabular-nums;
        }
        .ynow-bblab-is__row td {
          padding: 5px 8px;
          vertical-align: baseline;
        }
        .ynow-bblab-is__op {
          width: 1.6em;
          color: #0C5484;
          font-weight: 700;
          text-align: center;
        }
        .ynow-bblab-is__lab {
          color: #1a1a1a;
        }
        .ynow-bblab-is__amt {
          text-align: right;
          font-weight: 700;
          white-space: nowrap;
        }
        .ynow-bblab-is__fml {
          padding-left: 14px;
          color: #5b6b75;
          font-size: 12px;
          white-space: nowrap;
        }
        .ynow-bblab-is__row--total td {
          border-top: 1px solid #0C5484;
          padding-top: 8px;
        }
        .ynow-bblab-is__row--ratio td {
          color: #4a5b66;
          font-weight: 500;
        }
        .ynow-bblab-is__row--ni td {
          border-top: 1px dotted #c5d0d6;
          padding-top: 10px;
        }
        .ynow-bblab-split {
          margin: 0;
          padding-left: 1.2em;
        }
        .ynow-bblab-split li { margin-bottom: 4px; }
        /* ---- Lite mode: hide Full-only chrome; show Smart Analysis ---- */
        .sidebar-menu a[data-value="smart_analysis"],
        .sidebar-menu li:has(> a[data-value="smart_analysis"]) {
          display: none !important;
        }
        body.ynow-lite .sidebar-menu a[data-value="smart_analysis"],
        body.ynow-lite .sidebar-menu li:has(> a[data-value="smart_analysis"]) {
          display: block !important;
        }
        body.ynow-lite .sidebar-menu a[data-value="get_started"],
        body.ynow-lite .sidebar-menu li:has(> a[data-value="get_started"]),
        body.ynow-lite .sidebar-menu li:has(a[data-value="nav_calculator"]),
        body.ynow-lite .sidebar-menu li:has(a[data-value="dcf_calculator"]),
        body.ynow-lite .sidebar-menu li:has(a[data-value="pb_calculator"]),
        body.ynow-lite .sidebar-menu li:has(a[data-value="rel_multiples_calculator"]),
        body.ynow-lite .sidebar-menu li:has(a[data-value="sotp_calculator"]),
        body.ynow-lite .sidebar-menu a[data-value="hfv"],
        body.ynow-lite .sidebar-menu li:has(> a[data-value="hfv"]),
        body.ynow-lite .sidebar-menu a[data-value="decision_checklist"],
        body.ynow-lite .sidebar-menu li:has(> a[data-value="decision_checklist"]) {
          display: none !important;
        }
        body.ynow-lite .ynow-full-only {
          display: none !important;
        }
        /* Lite: board KPIs stay non-clickable; YNOW／TYNOW keeps expand/collapse. */
        body.ynow-lite .ynow-macro-kpi--clickable:not(.ynow-macro-kpi--ynow) {
          cursor: default !important;
          pointer-events: none;
        }
        body.ynow-lite #ynow_macro_index_hint,
        body.ynow-lite #ynow_macro_htcdi_expand {
          display: none !important;
        }
        /* Prefer data-value: locale applyTabLabels may strip title span ids */
        body.ynow-lite #dashboard_fin_report .nav-tabs > li:has(#ynow_dash_annotation_tab),
        body.ynow-lite #dashboard_fin_report .nav-tabs > li:has(> a[data-value="Annotation"]) {
          display: none !important;
        }
        /* Lite: under Data Source keep Snapshot + Feedback only;
           Quant Backtest Lab + Testing foot link stay Full-only.
           Business Breakdown (tab company_advance) is Full-only. */
        body.ynow-lite .sidebar-menu a[data-value="lab_notes"],
        body.ynow-lite .sidebar-menu li:has(> a[data-value="lab_notes"]),
        body.ynow-lite .sidebar-menu a[data-value="company_advance"],
        body.ynow-lite .sidebar-menu li:has(> a[data-value="company_advance"]) {
          display: none !important;
        }
        body.ynow-lite .ynow-sidebar-test-link,
        body.ynow-lite .ynow-company-advance-bblab,
        body.ynow-lite .ynow-bblab {
          display: none !important;
        }
        /* Lite Snapshot: only System defaults tab */
        body.ynow-lite #snapshot_report > .nav-tabs > li:has(> a[data-value="snap_audit"]),
        body.ynow-lite #snapshot_report > .nav-tabs > li:has(> a[data-value="snap_current"]) {
          display: none !important;
        }
        .ynow-lite-only {
          display: none !important;
        }
        body.ynow-lite .ynow-lite-only {
          display: block !important;
        }
        /* Brand home: sidebar-aligned doors (Macro → Blue Chip → Company → YNOW → Value → Action) */
        .ynow-home {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 12px auto 28px;
          padding: 28px 8px 8px;
          background: transparent;
          color: #222;
          border-radius: 0;
          text-align: center;
          box-sizing: border-box;
        }
        .ynow-home-logo-wrap {
          position: relative;
          display: inline-block;
          margin: 0 0 18px;
          padding: 0;
          border: 0;
          background: transparent;
          cursor: pointer;
          user-select: none;
          outline: none;
        }
        .ynow-home-logo-wrap:focus-visible {
          outline: 2px solid #2f9e6b;
          outline-offset: 3px;
          border-radius: 8px;
        }
        .ynow-home-logo {
          width: 132px;
          height: auto;
          display: block;
          margin: 0;
        }
        .ynow-home-logo-wrap .ynow-lite-badge {
          right: -30px;
          bottom: 2px;
        }
        .ynow-home-title {
          margin: 0;
          font-size: 26px;
          font-weight: 600;
          letter-spacing: -0.02em;
          color: #1a1a1a;
        }
        .ynow-home-lead {
          margin: 10px 0 0;
          font-size: 16px;
          line-height: 1.45;
          color: #333;
        }
        .ynow-home-method {
          margin: 6px 0 22px;
          font-size: 13px;
          line-height: 1.45;
          color: #666;
        }
        .ynow-home-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 10px;
          text-align: left;
        }
        .ynow-home-card {
          display: block;
          width: 100%;
          margin: 0;
          padding: 14px 14px 15px;
          text-align: left;
          color: #222;
          background: transparent;
          border: 1px solid #d0d5da;
          border-radius: 8px;
          cursor: pointer;
        }
        .ynow-home-card:hover,
        .ynow-home-card:focus-visible {
          border-color: #2f9e6b;
          outline: none;
        }
        .ynow-home-card-k {
          margin: 0 0 4px;
          font-size: 14px;
          font-weight: 650;
          color: #111;
        }
        .ynow-home-card-d {
          margin: 0;
          font-size: 12.5px;
          line-height: 1.45;
          color: #555;
        }
        @media (max-width: 720px) {
          .ynow-home { padding: 24px 12px 18px; max-width: 100%; }
          .ynow-home-grid { grid-template-columns: 1fr; }
        }
        @media (min-width: 721px) and (max-width: 991px) {
          .ynow-home-grid { grid-template-columns: 1fr 1fr; }
        }
        @media (min-width: 992px) {
          .ynow-home-grid { grid-template-columns: 1fr 1fr 1fr; }
        }
        .ynow-home-legal {
          margin: 18px 0 0 0;
          padding-top: 14px;
          border-top: 1px solid #e5e8eb;
          text-align: center;
        }
        .ynow-home-legal-text {
          margin: 0 0 8px 0;
          font-size: 11.5px;
          line-height: 1.5;
          color: #666;
        }
        .ynow-home-legal-link {
          display: inline-block;
          margin: 0;
          padding: 0;
          border: none;
          background: transparent;
          color: #0b57d0;
          font-size: 12px;
          line-height: 1.4;
          cursor: pointer;
          text-decoration: underline;
          text-underline-offset: 2px;
        }
        .ynow-home-legal-link:hover,
        .ynow-home-legal-link:focus-visible {
          color: #0842a0;
          outline: none;
        }
        /* Lite Smart Analysis: lead blurb above composite status (no page heading) */
        .ynow-smart-lite-blurb-wrap {
          margin: 0 0 10px 0;
        }
        .ynow-smart-lite-blurb {
          margin: 0;
          font-size: 14px;
          line-height: 1.55;
          color: #444;
        }
        .ynow-smart-card-row {
          display: flex;
          flex-wrap: wrap;
          gap: 12px;
          margin: 0 0 14px 0;
        }
        .ynow-smart-card {
          flex: 1 1 220px;
          border: 1px solid #ddd;
          border-radius: 6px;
          padding: 12px 14px;
          background: #fff;
          min-height: 96px;
        }
        .ynow-smart-card-kicker {
          font-size: 11px;
          color: #777;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          margin: 0 0 4px 0;
        }
        .ynow-smart-card-title {
          font-size: 16px;
          font-weight: 700;
          margin: 0 0 6px 0;
          color: #222;
        }
        .ynow-smart-card-value {
          font-size: 22px;
          font-weight: 700;
          color: #111;
          margin: 0;
        }
        .ynow-smart-card-meta {
          font-size: 12px;
          color: #666;
          margin: 6px 0 0 0;
          line-height: 1.4;
        }
        /* 側邊欄 PDF 下載：換行／縮放，避免長標籤撐破側欄 */
        .ynow-sidebar-download-wrap {
          padding: 6px 10px 10px 10px;
          text-align: center;
          margin-top: 4px;
          max-width: 100%;
          box-sizing: border-box;
        }
        .ynow-sidebar-download-wrap #download_report,
        .ynow-sidebar-download-btn {
          display: inline-block;
          width: 100%;
          max-width: 100%;
          box-sizing: border-box;
          white-space: normal !important;
          word-break: break-word;
          overflow-wrap: anywhere;
          line-height: 1.25;
          padding: 8px 10px;
          font-size: 13px;
          font-weight: bold;
          text-align: center;
        }
        /* 主選單不應再出現 Snapshot */
        .sidebar-menu a[data-value="snapshot"] { display: none !important; }
        .sidebar-menu li:has(> a[data-value="snapshot"]) { display: none !important; }
        .sidebar-menu a[data-value="feedback"] { display: none !important; }
        .sidebar-menu li:has(> a[data-value="feedback"]) { display: none !important; }
        /* 公司全稱：允許換行，避免被切掉；台股中英雙語直向堆疊 */
        .ynow-corpname {
          font-weight: bold;
          color: #333333;
          margin: 0;
          line-height: 1.25;
          white-space: normal;
          overflow-wrap: anywhere;
          word-break: break-word;
        }
        .ynow-corpname .ynow-corpname-stack {
          display: inline-flex;
          flex-direction: column;
          align-items: stretch;
          width: fit-content;
          max-width: 100%;
        }
        .ynow-corpname .ynow-corpname-zh {
          display: block;
          text-align: left;
        }
        .ynow-corpname .ynow-corpname-en {
          display: block;
          text-align: right;
          align-self: stretch;
          font-size: 0.82em;
          font-weight: 600;
          color: #555555;
          margin-top: 2px;
          line-height: 1.2;
        }
        .ynow-corpname .ynow-corpname-single {
          display: inline;
        }
        /* Header Language toggle (in black bar); Currency floats under logo */
        /* 繁中／EN 與小 logo 在頁首列垂直置中對齊 */
        .main-header .navbar-custom-menu {
          height: 50px !important;
          display: flex !important;
          align-items: center !important;
        }
        .main-header .navbar-custom-menu > .navbar-nav {
          display: flex !important;
          flex-direction: row;
          align-items: center !important;
          flex-wrap: nowrap !important;
          height: 50px !important;
          margin: 0 !important;
        }
        .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header,
        .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo {
          float: none !important;
          height: 50px !important;
          min-height: 50px !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          margin: 0 !important;
          padding-top: 0 !important;
          padding-bottom: 0 !important;
          flex-shrink: 0 !important;
        }
        .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header {
          padding: 0 10px 0 4px !important;
          list-style: none !important;
          overflow: visible !important;
        }
        .ynow-lang-stack {
          display: inline-flex !important;
          flex-direction: row !important;
          flex-wrap: nowrap !important;
          align-items: center !important;
          justify-content: center !important;
          height: 28px;
          margin: 0;
          padding: 0;
          gap: 0;
          flex-shrink: 0 !important;
          white-space: nowrap !important;
        }
        .ynow-lang-btn {
          box-sizing: border-box;
          flex: 0 0 auto;
          min-width: 42px;
          height: 28px;
          margin: 0;
          padding: 0 8px;
          border: 1px solid rgba(255,255,255,0.35);
          border-radius: 0;
          background: rgba(255,255,255,0.12);
          color: #fff !important;
          font-size: 12px;
          font-weight: 700;
          line-height: 26px;
          cursor: pointer;
          white-space: nowrap !important;
          text-shadow: none;
        }
        .ynow-lang-btn + .ynow-lang-btn {
          border-left-width: 0;
        }
        .ynow-lang-btn.active {
          background: #fff !important;
          color: #222 !important;
          border-color: #fff !important;
        }
        .ynow-lang-btn:focus {
          outline: none;
        }

        /* USD／TWD: under logo with modest gap below black header */
        .main-header,
        .main-header .navbar,
        .main-header .navbar-custom-menu,
        .main-header .navbar-custom-menu > .navbar-nav {
          overflow: visible !important;
        }
        .ynow-ccy-float {
          position: absolute;
          top: 100%;
          right: 14px;
          margin: 8px 0 0 0;
          padding: 0;
          z-index: 1035;
          display: flex;
          flex-direction: column;
          align-items: flex-end;
          gap: 4px;
          line-height: 1.15;
          pointer-events: auto;
          max-width: min(320px, calc(100vw - 16px));
        }
        .ynow-ccy-float .form-group,
        .ynow-ccy-float .shiny-input-container {
          margin: 0 !important;
        }
        .ynow-ccy-float .btn-group-xs > .btn,
        .ynow-ccy-float .btn-xs {
          background: #ffffff !important;
          border: 1px solid var(--ynow-ink) !important;
          color: var(--ynow-ink) !important;
          font-weight: 700 !important;
          min-width: 42px;
          box-shadow: 0 1px 2px rgba(26, 26, 26, 0.12);
        }
        .ynow-ccy-float .btn-group-xs > .btn.active,
        .ynow-ccy-float .btn-xs.active {
          background: var(--ynow-ink) !important;
          color: #fff !important;
          border-color: var(--ynow-ink) !important;
          box-shadow: none !important;
        }
        .ynow-ccy-float .radiobtn { margin: 0 !important; }
        .ynow-ccy-float .ynow-hdr-ccy-status,
        .ynow-ccy-float .ynow-hdr-ccy-status .shiny-text-output {
          display: block;
          font-size: 10px;
          color: rgba(26, 26, 26, 0.62);
          white-space: normal;
          overflow-wrap: anywhere;
          word-break: break-word;
          max-width: min(280px, calc(100vw - 24px));
          text-align: right;
          line-height: 1.3;
          margin: 0;
        }
        /* Credit: document-flow subband (left), same row as USD/TWD — not fixed/sticky */
        .ynow-hdr-subband {
          position: relative;
          display: flex;
          align-items: center;
          min-height: 36px;
          margin: -44px 0 10px 0;
          padding: 0;
          max-width: calc(100% - 140px);
          pointer-events: none;
        }
        .ynow-hdr-subband .ynow-credit-flow {
          pointer-events: auto;
          position: static;
          float: none;
          display: flex;
          align-items: center;
          margin: 0;
          padding: 0;
          max-width: 100%;
        }
        .ynow-hdr-subband .ynow-credit-text {
          margin: 0;
          padding: 0;
          font-size: 13px;
          font-weight: 600;
          line-height: 1.2;
          color: #666666;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        @media (max-width: 767px) {
          .ynow-hdr-subband {
            max-width: calc(100% - 120px);
            /* Do not pull into fixed black header (was -40px → covered when header stacked to 100px) */
            margin-top: 4px;
            margin-bottom: 8px;
          }
          .ynow-hdr-subband .ynow-credit-text {
            font-size: 11px;
          }
          .content-wrapper > .content {
            padding-top: 40px;
          }
        }
        /* Compact composite valuation status (replaces KPI row on model pages) */
        .ynow-val-status-header {
          background: #ffffff;
          padding: 14px 16px;
          border-radius: 8px;
          box-shadow: 0 1px 2px rgba(26, 26, 26, 0.08);
          margin-bottom: 4px;
        }
        .ynow-val-status-header h4 {
          margin: 0;
          font-weight: 700;
          font-size: 16px;
          line-height: 1.35;
        }
        .ynow-ind-overview-block {
          margin: 0 0 12px 0;
          padding: 12px 14px;
          background: #f5f5f5;
          border-left: 4px solid #222222;
          border-radius: 4px;
        }
        .ynow-ind-overview-block .ynow-ind-overview-title {
          font-size: 13px;
          font-weight: 700;
          color: #222222;
          margin: 0 0 8px 0;
        }
        .ynow-ind-overview-row {
          display: flex;
          flex-direction: row;
          align-items: flex-start;
          gap: 16px;
          flex-wrap: wrap;
        }
        .ynow-ind-overview-picker {
          flex: 0 1 280px;
          min-width: 200px;
          max-width: 360px;
        }
        .ynow-ind-overview-summary {
          flex: 1 1 260px;
          min-width: 0;
        }
        .ynow-ind-overview-block .form-group {
          margin-bottom: 0;
        }
        @media (max-width: 767px) {
          .ynow-ind-overview-block {
            padding: 10px 12px;
          }
          .ynow-ind-overview-block .ynow-ind-overview-title {
            margin: 0 0 4px 0;
          }
          .ynow-ind-overview-row {
            flex-direction: column;
            gap: 4px;
            align-items: stretch;
          }
          .ynow-ind-overview-picker {
            max-width: none;
            width: 100%;
            flex: 0 0 auto;
            /* Keep closed picker to toggle height; menu is portaled via container=body */
            position: relative;
            z-index: 5;
          }
          .ynow-ind-overview-picker .form-group,
          .ynow-ind-overview-picker .shiny-input-container {
            margin-bottom: 0 !important;
          }
          .ynow-ind-overview-picker .bootstrap-select,
          .ynow-ind-overview-picker .btn-group.bootstrap-select {
            margin-bottom: 0 !important;
            width: 100% !important;
            position: relative;
            /* Do not let an in-flow leftover menu stretch this cell (~size×row ≈ huge white gap) */
            height: auto !important;
            max-height: none;
          }
          /* Closed menu must never consume vertical layout (size=12 ≈ 1/3 viewport) */
          .ynow-ind-overview-picker .bootstrap-select:not(.open):not(.show) > .dropdown-menu,
          .ynow-ind-overview-picker .bootstrap-select:not(.open):not(.show) .dropdown-menu {
            display: none !important;
            height: 0 !important;
            max-height: 0 !important;
            min-height: 0 !important;
            overflow: hidden !important;
            margin: 0 !important;
            padding: 0 !important;
            border: 0 !important;
            visibility: hidden !important;
            pointer-events: none !important;
          }
          /* If a menu remains inside the picker (no bs-container), keep it absolute */
          .ynow-ind-overview-picker .bootstrap-select > .dropdown-menu {
            position: absolute !important;
            top: 100% !important;
            left: 0 !important;
            right: auto !important;
            z-index: 1060 !important;
          }
          .ynow-ind-overview-picker select.selectpicker,
          .ynow-ind-overview-picker select.bs-select-hidden {
            display: none !important;
            height: 0 !important;
            max-height: 0 !important;
            margin: 0 !important;
            padding: 0 !important;
            border: 0 !important;
            position: absolute !important;
            clip: rect(0, 0, 0, 0) !important;
          }
          .ynow-ind-overview-summary {
            margin: 0;
            flex: 0 0 auto;
            min-height: 0 !important;
          }
          .ynow-ind-overview-block .shiny-html-output,
          .ynow-ind-overview-block .shiny-bound-output {
            min-height: 0 !important;
            height: auto !important;
          }
          .ynow-ind-overview-block .ynow-ind-yahoo-row {
            margin-top: 2px;
          }
          .ynow-ind-overview-metrics {
            margin-top: 6px;
          }
          .ynow-ind-snapshot-chips,
          .ynow-kpi-legend-chips {
            gap: 6px;
            row-gap: 6px;
            column-gap: 6px;
          }
        }
        /* DCF shared header：上列「選擇模型／採用現金流」與下列「預測年數 n／建議」同寬靠左對齊 */
        #ibx_EPS { margin-bottom: 8px; }
        .ynow-dcf-mode-row,
        .ynow-header-years-suggest-row {
          display: flex;
          flex-wrap: wrap;
          align-items: flex-start;
          margin-left: 0;
          margin-right: 0;
          padding: 0;
        }
        .ynow-dcf-mode-row > [class*="col-"],
        .ynow-header-years-suggest-row > [class*="col-"] {
          float: none;
          padding-left: 10px;
          padding-right: 10px;
        }
        .ynow-dcf-mode-row .form-group { margin-bottom: 8px; }
        .ynow-dcf-mode-col,
        .ynow-header-years {
          padding: 0 0 8px 0;
          margin-top: 0;
          width: 100%;
        }
        .ynow-header-years .form-group { margin-bottom: 0; }
        .ynow-dcf-mode-col > .form-group > label.control-label,
        .ynow-header-years label.control-label {
          font-size: 13px;
          font-weight: 700;
          color: #333;
          margin-bottom: 5px;
          min-height: 18px;
          line-height: 1.35;
          display: block;
          text-align: left;
        }
        .ynow-dcf-claim-col {
          padding: 0 0 8px 0;
          width: 100%;
        }
        .ynow-dcf-claim-suggest-wrap {
          padding: 0 0 8px 0;
          width: 100%;
        }
        .ynow-dcf-claim-suggest-wrap .ynow-dcf-claim-suggest {
          margin: 0;
          padding: 8px 10px;
          background: #f5f5f5;
          border-left: 4px solid #222;
          font-size: 12px;
          color: #444;
          line-height: 1.5;
          box-sizing: border-box;
          min-height: 34px;
          text-align: left;
        }
        /* Header KPI：Last Price／Market Cap／EPS — logo 綠圖示；無金框 */
        .ynow-header-kpi-row .info-box {
          background: #ffffff;
          border: 1px solid rgba(26, 26, 26, 0.10);
          border-top: none;
          box-shadow: 0 1px 2px rgba(26, 26, 26, 0.06);
          background-image: none;
        }
        .ynow-header-kpi-row .info-box .info-box-icon,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-purple,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-aqua,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-green,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-yellow,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-blue,
        .ynow-header-kpi-row .info-box .info-box-icon.bg-teal {
          background-color: var(--ynow-logo-green) !important;
          color: #ffffff !important;
        }
        .ynow-header-kpi-row .info-box .info-box-icon > .fa,
        .ynow-header-kpi-row .info-box .info-box-icon > .fas {
          color: #ffffff !important;
        }
        .ynow-header-kpi-row .info-box .info-box-text {
          color: var(--ynow-gold-ink) !important;
          font-weight: 700;
          letter-spacing: 0.02em;
        }
        .ynow-header-kpi-row .info-box .info-box-number,
        .ynow-header-kpi-row .info-box .info-box-number h3 {
          color: var(--ynow-ink) !important;
        }
        /* 手機：PREVIOUS CLOSE / MARKET CAP / EPS 直向堆疊、各佔 100% */
        @media (max-width: 767px) {
          .content-wrapper .ynow-header-kpi-row > .col-xs-12 {
            width: 100% !important;
            float: none !important;
            display: block;
            clear: both;
          }
          .content-wrapper .ynow-header-kpi-row .info-box {
            margin-bottom: 10px;
          }
        }

        /* HFV report shell: narrative chapters + responsive control toolbar */
        .ynow-hfv-report {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 0 auto 24px auto;
          box-sizing: border-box;
        }
        .ynow-hfv-report__masthead {
          margin: 0 0 16px 0;
          padding: 4px 2px 8px 2px;
          /* No rule under the page lead — keep masthead flush into how-to-read / chapters */
          border-bottom: none;
        }
        .ynow-hfv-report__masthead h2 {
          margin: 0 0 8px 0;
          font-size: clamp(20px, 2.6vw, 28px);
          font-weight: 700;
          letter-spacing: -0.01em;
          color: #1a1a1a;
        }
        .ynow-hfv-report__lead {
          margin: 0;
          max-width: 58em;
          font-size: 13.5px;
          line-height: 1.55;
          color: #555;
        }
        .ynow-hfv-chapter__lead {
          margin: 0;
          padding: 10px 16px 0 16px;
          max-width: none;
          font-size: 13px;
          line-height: 1.5;
          color: #555;
        }
        .ynow-hfv-toolbar {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
          gap: 12px 14px;
          width: 100%;
          margin: 0 0 18px 0;
          padding: 14px 16px;
          background: #fafafa;
          border: 1px solid #e4e4e4;
          border-radius: 8px;
          box-sizing: border-box;
        }
        .ynow-hfv-toolbar--above-chart {
          margin: 8px 0 12px 0;
          grid-template-columns: 1fr;
        }
        /* Page-level common controls (under How-to-read): compact analysis frequency */
        .ynow-hfv-page-controls {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px 16px;
          width: 100%;
          margin: 0 0 16px 0;
          padding: 8px 12px;
          background: #f7f8fa;
          border: 1px solid #e6e8eb;
          border-radius: 6px;
          box-sizing: border-box;
        }
        .ynow-hfv-page-controls__freq {
          flex: 1 1 auto;
          min-width: 0;
        }
        .ynow-hfv-page-controls .form-group {
          margin: 0 !important;
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px 12px;
        }
        .ynow-hfv-page-controls .shiny-input-radiogroup > label.control-label {
          display: inline-block;
          margin: 0;
          padding: 0;
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.02em;
          text-transform: none;
          color: #555;
          line-height: 1.3;
          white-space: nowrap;
        }
        .ynow-hfv-page-controls .shiny-options-group {
          display: inline-flex !important;
          flex-wrap: wrap !important;
          align-items: center;
          gap: 6px;
          margin: 0 !important;
          padding: 0 !important;
          clear: none;
        }
        .ynow-hfv-page-controls .radio,
        .ynow-hfv-page-controls .radio-inline {
          float: none !important;
          display: inline-flex !important;
          align-items: center;
          margin: 0 !important;
          padding: 0 !important;
        }
        .ynow-hfv-page-controls .radio > label,
        .ynow-hfv-page-controls .radio-inline,
        .ynow-hfv-page-controls .radio-inline > label {
          display: inline-flex !important;
          align-items: center;
          justify-content: center;
          margin: 0 !important;
          padding: 4px 10px !important;
          min-height: 28px;
          font-size: 12px;
          font-weight: 600;
          line-height: 1.2;
          color: #444;
          background: #fff;
          border: 1px solid #d0d5dd;
          border-radius: 999px;
          cursor: pointer;
          white-space: nowrap;
        }
        .ynow-hfv-page-controls .shiny-options-group input[type="radio"] {
          position: absolute !important;
          opacity: 0 !important;
          width: 0 !important;
          height: 0 !important;
          margin: 0 !important;
          pointer-events: none;
        }
        .ynow-hfv-page-controls .shiny-options-group .radio:has(input:checked) > label,
        .ynow-hfv-page-controls .shiny-options-group .radio-inline:has(input:checked) {
          color: #0c5484;
          background: #e8f2f8;
          border-color: #0c5484;
        }
        .ynow-hfv-page-controls .ynow-hfv-page-controls__hint {
          margin: 0;
          font-size: 11px;
          line-height: 1.35;
          color: #888;
        }
        /* Section II: two equal columns + full-width OOS row; options fill each group */
        .ynow-hfv-toolbar--in-ch2 {
          margin: 0 0 16px 0;
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .ynow-hfv-toolbar__group {
          min-width: 0;
          width: 100%;
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .ynow-hfv-toolbar__group--wide {
          grid-column: 1 / -1;
        }
        .ynow-hfv-toolbar__group .form-group {
          margin: 0;
          width: 100%;
        }
        .ynow-hfv-toolbar__group > label.control-label,
        .ynow-hfv-toolbar .shiny-input-radiogroup > label.control-label,
        .ynow-hfv-toolbar .shiny-input-checkboxgroup > label.control-label {
          display: block;
          margin: 0 0 6px 0;
          padding: 0;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #666;
          line-height: 1.35;
        }
        .ynow-hfv-toolbar .shiny-options-group {
          display: flex !important;
          flex-wrap: wrap !important;
          align-items: stretch;
          gap: 6px;
          margin: 0;
          padding: 0;
          clear: none;
          width: 100%;
        }
        /* Ch2: equal cells, at most two rows, fill group width (keep pill chips) */
        .ynow-hfv-toolbar--in-ch2 #bt_fv_replay_model .shiny-options-group,
        .ynow-hfv-toolbar--in-ch2 #bt_fv_conv_window .shiny-options-group {
          display: grid !important;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 6px 8px;
        }
        .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .shiny-options-group {
          display: grid !important;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 6px 8px;
        }
        .ynow-hfv-toolbar--above-chart #bt_fv_models .shiny-options-group {
          display: grid !important;
          grid-template-columns: repeat(5, minmax(0, 1fr));
          gap: 6px 8px;
        }
        .ynow-hfv-toolbar .radio,
        .ynow-hfv-toolbar .radio-inline,
        .ynow-hfv-toolbar .checkbox,
        .ynow-hfv-toolbar .checkbox-inline {
          float: none !important;
          display: inline-flex !important;
          align-items: center;
          margin: 0 !important;
          padding: 0 !important;
          min-height: 0;
        }
        .ynow-hfv-toolbar--in-ch2 .radio,
        .ynow-hfv-toolbar--in-ch2 .radio-inline,
        .ynow-hfv-toolbar--in-ch2 .checkbox,
        .ynow-hfv-toolbar--in-ch2 .checkbox-inline,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox-inline {
          width: 100%;
          display: flex !important;
        }
        .ynow-hfv-toolbar .radio > label,
        .ynow-hfv-toolbar .radio-inline,
        .ynow-hfv-toolbar .radio-inline > label,
        .ynow-hfv-toolbar .checkbox > label,
        .ynow-hfv-toolbar .checkbox-inline,
        .ynow-hfv-toolbar .checkbox-inline > label {
          margin: 0 !important;
          padding: 7px 12px 7px 28px !important;
          border: 1px solid #d0d0d0;
          border-radius: 999px;
          background: #fff;
          font-size: 12.5px;
          font-weight: 500;
          line-height: 1.25;
          color: #222;
          white-space: nowrap;
          cursor: pointer;
          transition: border-color .12s ease, background .12s ease, box-shadow .12s ease;
        }
        .ynow-hfv-toolbar--in-ch2 .radio > label,
        .ynow-hfv-toolbar--in-ch2 .radio-inline,
        .ynow-hfv-toolbar--in-ch2 .radio-inline > label,
        .ynow-hfv-toolbar--in-ch2 .checkbox > label,
        .ynow-hfv-toolbar--in-ch2 .checkbox-inline,
        .ynow-hfv-toolbar--in-ch2 .checkbox-inline > label,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox > label,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox-inline,
        .ynow-hfv-toolbar--above-chart #bt_fv_models .checkbox-inline > label {
          width: 100%;
          box-sizing: border-box;
          justify-content: center;
          text-align: center;
          /* No left gutter for native radio/checkbox — those are visually hidden below */
          padding: 8px 12px !important;
        }
        .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .radio > label,
        .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .radio-inline,
        .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .radio-inline > label {
          white-space: normal;
          min-height: 40px;
          align-items: center;
        }
        /* Default: radio/checkbox sit in the label left padding */
        .ynow-hfv-toolbar .radio input[type="radio"],
        .ynow-hfv-toolbar .radio-inline input[type="radio"],
        .ynow-hfv-toolbar .checkbox input[type="checkbox"],
        .ynow-hfv-toolbar .checkbox-inline input[type="checkbox"] {
          position: absolute;
          margin-left: -20px;
          margin-top: 1px;
        }
        /* Chip option groups: hide native dots so they never cover centered label text.
           Selection is shown by the dark filled chip (input:checked). */
        .ynow-hfv-toolbar .shiny-options-group input[type="radio"],
        .ynow-hfv-toolbar .shiny-options-group input[type="checkbox"] {
          position: absolute !important;
          left: 0 !important;
          top: 0 !important;
          width: 1px !important;
          height: 1px !important;
          margin: 0 !important;
          padding: 0 !important;
          opacity: 0 !important;
          clip: rect(0, 0, 0, 0);
          pointer-events: none;
        }
        .ynow-hfv-toolbar .shiny-options-group .radio > label:hover,
        .ynow-hfv-toolbar .shiny-options-group .radio-inline:hover,
        .ynow-hfv-toolbar .shiny-options-group .checkbox > label:hover,
        .ynow-hfv-toolbar .shiny-options-group .checkbox-inline:hover {
          border-color: #999;
          background: #fff;
        }
        .ynow-hfv-toolbar .shiny-options-group .radio:has(input:checked) > label,
        .ynow-hfv-toolbar .shiny-options-group .radio-inline:has(input:checked),
        .ynow-hfv-toolbar .shiny-options-group .checkbox:has(input:checked) > label,
        .ynow-hfv-toolbar .shiny-options-group .checkbox-inline:has(input:checked) {
          border-color: #1a1a1a;
          background: #1a1a1a;
          color: #fff;
          box-shadow: 0 1px 2px rgba(0,0,0,0.12);
        }
        /* Standalone checkbox (Show benchmark): plain flex row, never a filled chip */
        .ynow-hfv-toolbar .checkbox:not(.checkbox-inline) > label {
          display: inline-flex !important;
          align-items: center !important;
          gap: 8px;
          border: none !important;
          background: transparent !important;
          box-shadow: none !important;
          border-radius: 0 !important;
          padding: 2px 0 !important;
          color: #333 !important;
          width: auto !important;
          justify-content: flex-start !important;
          text-align: left !important;
          white-space: normal !important;
          min-height: 0 !important;
        }
        .ynow-hfv-toolbar .checkbox:not(.checkbox-inline) input[type="checkbox"] {
          position: static !important;
          margin: 0 !important;
          opacity: 1 !important;
          width: auto !important;
          height: auto !important;
          clip: auto !important;
          pointer-events: auto !important;
          flex: 0 0 auto;
        }
        .ynow-hfv-toolbar__hint {
          margin: 2px 0 0 0;
          font-size: 11px;
          line-height: 1.45;
          color: #888;
        }
        .ynow-hfv-chapter {
          margin: 0 0 18px 0;
          padding: 0;
          background: #fff;
          border: 1px solid #e6e6e6;
          border-radius: 8px;
          overflow: hidden;
        }
        .ynow-hfv-chapter__head {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 6px 12px;
          padding: 12px 16px;
          background: #fff;
          border-bottom: 1px solid #ececec;
        }
        .ynow-hfv-chapter__kicker {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #888;
        }
        .ynow-hfv-chapter__title {
          margin: 0;
          font-size: 16px;
          font-weight: 700;
          color: #1a1a1a;
          line-height: 1.35;
        }
        .ynow-hfv-chapter__body {
          padding: 12px 16px 16px 16px;
        }
        .ynow-hfv-chapter--appendix .ynow-hfv-chapter__body {
          background: #fafafa;
        }
        .ynow-hfv-report > .box {
          margin-bottom: 14px;
          border-radius: 8px;
          box-shadow: none;
          border: 1px solid #e6e6e6;
        }
        .ynow-hfv-report > .box > .box-header {
          padding: 10px 14px;
        }
        .ynow-hfv-report > .box > .box-body {
          padding: 12px 14px 14px 14px;
        }
        .ynow-hfv-findings-block:last-child {
          margin-bottom: 0;
        }
        .ynow-hfv-findings-block > h5 {
          margin: 0 0 10px 0;
          font-size: 13px;
          font-weight: 700;
          color: #333;
        }
        .ynow-hfv-kpi-grid {
          display: grid;
          grid-template-columns: repeat(4, minmax(0, 1fr));
          gap: 10px;
          align-items: stretch;
          width: 100%;
          margin: 0 0 10px 0;
          box-sizing: border-box;
        }
        .ynow-hfv-kpi-grid--3 {
          grid-template-columns: repeat(3, minmax(0, 1fr));
        }
        .ynow-hfv-kpi-cell {
          display: flex;
          flex-direction: column;
          gap: 4px;
          min-height: 88px;
          height: 100%;
          padding: 10px 12px;
          box-sizing: border-box;
          border: 1px solid #e8e8e8;
          border-radius: 4px;
          background: #fff;
        }
        /* HFV bullish/bearish tones: US green-up / red-down; TW inverts via body.ynow-market-tw */
        .ynow-hfv-bull { color: #00a65a !important; }
        .ynow-hfv-bear { color: #d9534f !important; }
        .ynow-hfv-kpi-cell--bull {
          background: #f7fbf8;
          border-left: 4px solid #00a65a;
        }
        .ynow-hfv-kpi-cell--bear {
          background: #fdf7f7;
          border-left: 4px solid #d9534f;
        }
        .ynow-hfv-kpi-cell--pair {
          background: #fafafa;
          border-left: 4px solid #adb5bd;
        }
        .ynow-hfv-kpi-pair-vals {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 2px 4px;
          line-height: 1.2;
        }
        .ynow-hfv-kpi-pair-sep {
          color: #999 !important;
          font-weight: 500;
        }
        body.ynow-market-tw .ynow-hfv-bull { color: #c0392b !important; }
        body.ynow-market-tw .ynow-hfv-bear { color: #00a65a !important; }
        body.ynow-market-tw .ynow-hfv-kpi-cell--bull {
          background: #fdf7f7;
          border-left-color: #c0392b;
        }
        body.ynow-market-tw .ynow-hfv-kpi-cell--bear {
          background: #f7fbf8;
          border-left-color: #00a65a;
        }
        .ynow-hfv-overlay-replay-note {
          margin: 8px 0 0 0 !important;
          font-size: 11.5px !important;
          line-height: 1.45;
          color: #666;
        }
        .ynow-hfv-scenario-under-chart {
          margin-top: 14px;
        }
        @media (max-width: 991px) {
          .ynow-hfv-kpi-grid,
          .ynow-hfv-kpi-grid--3 {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
        }
        @media (max-width: 767px) {
          .ynow-hfv-toolbar,
          .ynow-hfv-toolbar--in-ch2 {
            grid-template-columns: 1fr;
            padding: 12px;
            gap: 14px;
          }
          .ynow-hfv-toolbar__group--wide {
            grid-column: auto;
          }
          .ynow-hfv-toolbar--in-ch2 #bt_fv_replay_model .shiny-options-group,
          .ynow-hfv-toolbar--in-ch2 #bt_fv_conv_window .shiny-options-group,
          .ynow-hfv-toolbar--in-ch2 #bt_fv_oos_mode .shiny-options-group,
          .ynow-hfv-toolbar--above-chart #bt_fv_models .shiny-options-group {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
          .ynow-hfv-page-controls .form-group {
            flex-direction: column;
            align-items: flex-start;
          }
          .ynow-hfv-toolbar .radio > label,
          .ynow-hfv-toolbar .radio-inline,
          .ynow-hfv-toolbar .checkbox > label,
          .ynow-hfv-toolbar .checkbox-inline {
            white-space: normal;
            max-width: 100%;
          }
          .ynow-hfv-kpi-grid,
          .ynow-hfv-kpi-grid--3 {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
          .ynow-hfv-chapter__body {
            padding: 12px;
          }
          .ynow-hfv-chapter__head {
            padding: 10px 12px;
          }
          .ynow-hfv-chapter__lead {
            padding: 8px 12px 0 12px;
          }
          .ynow-hfv-report__masthead {
            padding-bottom: 12px;
          }
        }

        /* HFV 設定：統計期間／驗證樣本範圍共用 label→選項間距與區塊節奏 */
        .ynow-hfv-settings .shiny-input-radiogroup {
          margin-top: 0;
          margin-bottom: 12px;
        }
        .ynow-hfv-settings .shiny-input-radiogroup > label.control-label {
          display: block;
          margin-top: 0;
          margin-bottom: 6px;
          padding: 0;
          line-height: 1.4;
        }
        .ynow-hfv-settings .shiny-input-radiogroup .shiny-options-group {
          margin-top: 0;
          margin-bottom: 0;
          padding-top: 0;
          padding-left: 0;
        }
        /* 驗證樣本範圍：工具列內改為橫向 chip；舊直排規則僅保留於非 toolbar */
        .ynow-hfv-settings #bt_fv_oos_mode .shiny-options-group {
          display: flex !important;
          flex-direction: column !important;
          flex-wrap: nowrap !important;
          align-items: flex-start !important;
          column-gap: 0 !important;
          row-gap: 4px;
          text-align: left;
        }
        .ynow-hfv-settings #bt_fv_oos_mode .radio {
          display: block !important;
          float: none !important;
          width: auto;
          max-width: 100%;
          margin-top: 0 !important;
          margin-bottom: 0 !important;
          min-height: 0;
          padding-left: 0;
          text-align: left;
        }
        .ynow-hfv-settings #bt_fv_oos_mode .radio > label {
          display: inline-block;
          text-align: left;
          white-space: normal;
          font-weight: normal;
          margin: 0;
          padding-left: 20px;
        }
      ')),
      tags$script(HTML("
        (function () {
          var TAB_PARENT = {
            nav_calculator: 'ynow_menu_cat_asset',
            dcf_calculator: 'ynow_menu_cat_income',
            ddm_calculator: 'ynow_menu_cat_income',
            ri_calculator: 'ynow_menu_cat_income',
            pb_calculator: 'ynow_menu_cat_relative',
            rel_multiples_calculator: 'ynow_menu_cat_relative',
            sotp_calculator: 'ynow_menu_cat_relative'
          };
          var LAST_BADGE_MAP = null;

          function clearRecBadges(a) {
            if (!a) return;
            a.querySelectorAll('small.ynow-sidebar-badge, small.ynow-rec-badge, .ynow-rec-slot').forEach(function (b) {
              b.remove();
            });
          }

          /* Wrap bare text so flex can shrink the label; otherwise badges clip */
          function ensureShrinkableLabel(a) {
            if (!a) return;
            var nodes = Array.prototype.slice.call(a.childNodes);
            for (var i = 0; i < nodes.length; i++) {
              var n = nodes[i];
              if (n.nodeType !== 3) continue;
              var t = String(n.textContent || '').replace(/\\s+/g, ' ').trim();
              if (!t) continue;
              var span = document.createElement('span');
              span.className = 'ynow-menu-label';
              span.textContent = ' ' + t + ' ';
              a.replaceChild(span, n);
            }
          }

          function findChevron(a) {
            if (!a) return null;
            var kids = a.children;
            for (var i = 0; i < kids.length; i++) {
              var el = kids[i];
              if (!el.classList) continue;
              if (el.classList.contains('pull-right-container')) return el;
              if (el.classList.contains('pull-right') &&
                  (el.classList.contains('fa') || el.classList.contains('fas') ||
                   el.classList.contains('far') || el.classList.contains('glyphicon'))) {
                return el;
              }
              if (el.classList.contains('fa-angle-left') || el.classList.contains('fa-angle-down')) {
                return el;
              }
            }
            return null;
          }

          function setRecBadgeOnAnchor(a, role, labels) {
            if (!a) return;
            clearRecBadges(a);
            if (!role) return;
            ensureShrinkableLabel(a);
            var lab = labels || {};
            var isPrimary = role === 'primary';
            var badge = document.createElement('small');
            badge.className = 'ynow-sidebar-badge ' +
              (isPrimary ? 'ynow-rec-primary' : 'ynow-rec-secondary');
            badge.textContent = isPrimary
              ? (lab.primary || 'Recommend')
              : (lab.secondary || 'Secondary');
            var slot = document.createElement('span');
            slot.className = 'ynow-rec-slot';
            slot.appendChild(badge);
            var chev = findChevron(a);
            if (chev) a.insertBefore(slot, chev);
            else a.appendChild(slot);
          }

          function setTabBadge(tab, role, labels) {
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
            setRecBadgeOnAnchor(a, role || '', labels);
          }

          function setParentBadges(map, labels) {
            var rolesByParent = {
              ynow_menu_cat_asset: [],
              ynow_menu_cat_income: [],
              ynow_menu_cat_relative: []
            };
            Object.keys(TAB_PARENT).forEach(function (tab) {
              var role = (map && map[tab] && map[tab].role) ? String(map[tab].role) : '';
              var pid = TAB_PARENT[tab];
              if (role && rolesByParent[pid]) rolesByParent[pid].push(role);
            });
            Object.keys(rolesByParent).forEach(function (pid) {
              var span = document.getElementById(pid);
              if (!span) return;
              var a = span.closest ? span.closest('a') : null;
              if (!a) a = span.parentElement;
              var roles = rolesByParent[pid] || [];
              var parentRole = '';
              if (roles.indexOf('primary') >= 0) parentRole = 'primary';
              else if (roles.indexOf('secondary') >= 0) parentRole = 'secondary';
              setRecBadgeOnAnchor(a, parentRole, labels);
            });
          }

          function applySidebarBadges(map) {
            LAST_BADGE_MAP = map || null;
            var labels = (map && map.labels) || {};
            var tabs = ['dcf_calculator', 'ddm_calculator', 'pb_calculator', 'rel_multiples_calculator', 'sotp_calculator', 'ri_calculator', 'nav_calculator'];
            tabs.forEach(function (t) {
              var role = (map && map[t] && map[t].role) ? String(map[t].role) : '';
              if (!role && map && map[t] && map[t].on) role = 'primary';
              setTabBadge(t, role, labels);
            });
            setParentBadges(map, labels);
          }

          function registerBadgeHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerBadgeHandler, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowSidebarBadges', applySidebarBadges);
            function pingReady() {
              if (!(window.Shiny && Shiny.setInputValue)) {
                setTimeout(pingReady, 50);
                return;
              }
              Shiny.setInputValue('ynow_sidebar_badges_ready', Date.now(), {priority: 'event'});
            }
            pingReady();
            if (window.jQuery) {
              jQuery(document).on('shiny:connected shiny:sessioninitialized', function () {
                setTimeout(pingReady, 0);
                setTimeout(pingReady, 400);
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              });
            }
          }
          registerBadgeHandler();

          /* 美股／台股：搬到三線 toggle 正後方；left = toggle 右緣（無空隙） */
          function placeMarketHeaderByToggle() {
            var navbar = document.querySelector('.main-header .navbar');
            var market = document.getElementById('ynow-market-header');
            if (!navbar || !market) return;
            var toggle = null;
            var kids = navbar.children;
            for (var i = 0; i < kids.length; i++) {
              if (kids[i].classList && kids[i].classList.contains('sidebar-toggle')) {
                toggle = kids[i];
                break;
              }
            }
            if (!toggle) toggle = navbar.querySelector('.sidebar-toggle');
            if (!toggle) return;
            if (!(market.parentNode === navbar && market.previousElementSibling === toggle)) {
              navbar.insertBefore(market, toggle.nextSibling);
            }
            var leftPx = Math.round(toggle.offsetLeft + toggle.offsetWidth);
            if (!(leftPx > 0)) leftPx = Math.round(toggle.offsetWidth);
            market.style.left = leftPx + 'px';
            market.style.marginLeft = '0';
            market.style.paddingLeft = '0';
            placeMobileAppTitle();
          }
          /* Mobile: app title viewport-centered in the gap; scale font to fit — never cover chrome. */
          function placeMobileAppTitle() {
            var logo = document.querySelector('.main-header .logo');
            if (!logo) return;
            var title = logo.querySelector('.ynow-app-title');
            var base = title ? title.querySelector('.ynow-app-title-base') : null;
            var fillInner = title ? title.querySelector('.ynow-app-title-fill-inner') : null;
            if (window.innerWidth > 767) {
              logo.style.left = '';
              logo.style.right = '';
              if (title) {
                title.style.maxWidth = '';
                title.style.fontSize = '';
              }
              if (base) base.style.fontSize = '';
              if (fillInner) fillInner.style.fontSize = '';
              return;
            }
            logo.style.left = '0px';
            logo.style.right = '0px';
            if (!title || !base) return;
            var mkt = document.getElementById('ynow-market-header');
            var custom = document.querySelector('.main-header .navbar-custom-menu');
            var gap = 10;
            var leftClear = mkt ? Math.round(mkt.getBoundingClientRect().right) : 90;
            var rightClear = custom
              ? Math.round(window.innerWidth - custom.getBoundingClientRect().left)
              : 120;
            var maxW = Math.max(64, window.innerWidth - leftClear - rightClear - gap * 2);
            title.style.maxWidth = maxW + 'px';
            title.style.overflow = 'visible';
            title.style.textOverflow = 'clip';
            /* Prefer readable size; shrink proportionally so full version string stays visible */
            var fs = 16;
            var minFs = 9;
            base.style.fontSize = fs + 'px';
            if (fillInner) fillInner.style.fontSize = fs + 'px';
            title.style.fontSize = fs + 'px';
            var guard = 0;
            while (base.scrollWidth > maxW + 1 && fs > minFs && guard < 24) {
              fs -= 0.5;
              base.style.fontSize = fs + 'px';
              if (fillInner) fillInner.style.fontSize = fs + 'px';
              title.style.fontSize = fs + 'px';
              guard++;
            }
          }
          window.placeMobileAppTitle = placeMobileAppTitle;

          /* Header title = load progress bar (fills with boot / Shiny busy / withProgress) */
          document.title = 'The YNow App';
          (function initYnowTitleLoadBar() {
            var pct = 0;
            var firstIdleDone = false;
            var busyTimer = null;
            var completeTimer = null;
            function titleEl() {
              return document.getElementById('ynow_app_title') ||
                document.querySelector('.main-header .logo .ynow-app-title');
            }
            function setPct(n, opts) {
              opts = opts || {};
              n = Math.max(0, Math.min(100, Number(n) || 0));
              if (!opts.force && n < pct && pct >= 100 && firstIdleDone && !opts.busy) {
                return;
              }
              if (!opts.force && n < pct && !opts.busy) {
                /* allow only forward during boot; busy may reset */
                if (!(opts.allowDrop)) return;
              }
              pct = n;
              var el = titleEl();
              if (!el) return;
              el.style.setProperty('--ynow-load-pct', pct + '%');
              el.setAttribute('aria-valuenow', String(Math.round(pct)));
              if (pct < 100) {
                el.classList.add('is-loading');
                el.classList.remove('is-complete');
              }
            }
            function markComplete() {
              setPct(100, { force: true });
              var el = titleEl();
              if (!el) return;
              el.classList.remove('is-loading');
              el.classList.add('is-complete');
              el.setAttribute('aria-valuenow', '100');
              firstIdleDone = true;
            }
            function readShinyProgressPct() {
              var bar = document.querySelector('.shiny-progress .progress-bar');
              if (!bar) return null;
              var styleW = bar.style.width || '';
              var m = String(styleW).match(/([0-9.]+)[ ]*%/);
              if (m) return Math.max(0, Math.min(100, parseFloat(m[1])));
              try {
                var parent = bar.parentElement;
                if (parent && parent.clientWidth > 0) {
                  return Math.max(0, Math.min(100, (bar.clientWidth / parent.clientWidth) * 100));
                }
              } catch (e) {}
              return null;
            }
            function onBusy() {
              if (completeTimer) {
                clearTimeout(completeTimer);
                completeTimer = null;
              }
              var el = titleEl();
              if (el) {
                el.classList.add('is-loading');
                el.classList.remove('is-complete');
              }
              var sp = readShinyProgressPct();
              if (sp != null) {
                setPct(Math.max(8, sp), { force: true, busy: true, allowDrop: true });
              } else {
                setPct(Math.max(pct < 100 ? pct : 12, 12), { force: true, busy: true, allowDrop: true });
              }
              if (busyTimer) clearInterval(busyTimer);
              busyTimer = setInterval(function () {
                var p = readShinyProgressPct();
                if (p != null) {
                  setPct(Math.max(8, Math.min(98, p)), { force: true, busy: true, allowDrop: true });
                  return;
                }
                if (pct < 90) setPct(pct + 1.2, { force: true, busy: true });
              }, 180);
            }
            function onIdle() {
              if (busyTimer) {
                clearInterval(busyTimer);
                busyTimer = null;
              }
              setPct(100, { force: true });
              if (completeTimer) clearTimeout(completeTimer);
              completeTimer = setTimeout(markComplete, 320);
            }
            setPct(6, { force: true });
            function onDomReady() { setPct(28); }
            function onWinLoad() { setPct(48); }
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', onDomReady);
            } else {
              onDomReady();
            }
            if (document.readyState === 'complete') {
              onWinLoad();
            } else {
              window.addEventListener('load', onWinLoad);
            }
            function bindShinyLoad() {
              if (!window.jQuery) {
                setTimeout(bindShinyLoad, 40);
                return;
              }
              var $doc = jQuery(document);
              $doc.on('shiny:connected', function () { setPct(66); });
              $doc.on('shiny:sessioninitialized', function () { setPct(82); });
              $doc.on('shiny:busy', onBusy);
              $doc.on('shiny:idle', onIdle);
              /* If already idle after connect, complete */
              setTimeout(function () {
                if (!firstIdleDone && pct >= 66) onIdle();
              }, 1800);
            }
            bindShinyLoad();
            if (typeof MutationObserver !== 'undefined') {
              var obs = new MutationObserver(function () {
                var p = readShinyProgressPct();
                if (p == null) return;
                var el = titleEl();
                if (el && el.classList.contains('is-loading')) {
                  setPct(Math.max(8, Math.min(99, p)), { force: true, busy: true, allowDrop: true });
                }
              });
              obs.observe(document.documentElement, {
                childList: true,
                subtree: true,
                attributes: true,
                attributeFilter: ['style', 'class']
              });
            }
          })();

          /* Deploy refresh: poll ynow_build.json; never auto-reload.
             When a newer version is live, user must click The YNow App title to refresh. */
          (function initYnowDeployRefresh() {
            var POLL_MS = 45000;
            var sessionBuild = null;
            var remoteBuild = null;
            var pollTimer = null;
            var LAST_UPDATE_STRINGS = null;

            function titleEl() {
              return document.getElementById('ynow_app_title') ||
                document.querySelector('.main-header .logo .ynow-app-title');
            }
            function logoEl() {
              return document.querySelector('.main-header .logo');
            }
            function readSessionBuild() {
              if (sessionBuild) return sessionBuild;
              var el = titleEl();
              var fromDom = el && el.getAttribute('data-ynow-build');
              if (fromDom) {
                sessionBuild = String(fromDom).trim();
                return sessionBuild;
              }
              sessionBuild = '';
              return sessionBuild;
            }
            function updateTipText() {
              var s = LAST_UPDATE_STRINGS || {};
              return s.update_available_click ||
                'New version available — click The YNow App to refresh';
            }
            function applyUpdateChrome() {
              var pending = !!(remoteBuild && sessionBuild && remoteBuild !== sessionBuild);
              document.body.classList.toggle('ynow-update-available', pending);
              var tip = updateTipText();
              var title = titleEl();
              var logo = logoEl();
              if (title) {
                if (pending) {
                  title.setAttribute('title', tip);
                  title.setAttribute('aria-label', tip);
                  title.setAttribute('role', 'button');
                  title.setAttribute('tabindex', '0');
                } else {
                  title.removeAttribute('title');
                  if (title.getAttribute('role') === 'button') {
                    title.setAttribute('role', 'progressbar');
                  }
                  title.removeAttribute('tabindex');
                }
              }
              if (logo) {
                if (pending) logo.setAttribute('title', tip);
                else logo.removeAttribute('title');
              }
            }
            window.ynowSetUpdateLocaleStrings = function (strings) {
              LAST_UPDATE_STRINGS = strings || null;
              applyUpdateChrome();
            };
            function markRemote(version) {
              var v = String(version || '').trim();
              if (!v) return;
              remoteBuild = v;
              applyUpdateChrome();
            }
            function fetchBuild() {
              var url = 'ynow_build.json?_=' + Date.now();
              return fetch(url, { cache: 'no-store', credentials: 'same-origin' })
                .then(function (res) {
                  if (!res || !res.ok) throw new Error('build fetch failed');
                  return res.json();
                })
                .then(function (data) {
                  if (data && data.version) markRemote(data.version);
                })
                .catch(function () { /* keep last known; no auto-reload */ });
            }
            function reloadForUpdate() {
              /* Hard navigation so the browser loads the new Shiny bundle / assets. */
              try {
                var u = new URL(window.location.href);
                u.searchParams.set('_ynow_refresh', String(Date.now()));
                window.location.replace(u.toString());
              } catch (eReload) {
                window.location.reload();
              }
            }
            function onTitleActivate(ev) {
              if (!document.body.classList.contains('ynow-update-available')) return;
              var hit = ev.target && ev.target.closest
                ? ev.target.closest('#ynow_app_title, .main-header .logo')
                : null;
              if (!hit) return;
              /* Do not steal clicks from market/lang chrome inside the navbar. */
              if (ev.target.closest && ev.target.closest(
                '.ynow-market-header, .ynow-lang-header, .navbar-custom-menu, .sidebar-toggle'
              )) return;
              ev.preventDefault();
              ev.stopPropagation();
              reloadForUpdate();
            }
            function bindClick() {
              if (document.documentElement.getAttribute('data-ynow-deploy-refresh') === '1') return;
              document.documentElement.setAttribute('data-ynow-deploy-refresh', '1');
              document.addEventListener('click', onTitleActivate, true);
              document.addEventListener('keydown', function (ev) {
                if (!document.body.classList.contains('ynow-update-available')) return;
                var t = ev.target;
                if (!t || t.id !== 'ynow_app_title') return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                reloadForUpdate();
              }, true);
            }
            function startPolling() {
              readSessionBuild();
              fetchBuild();
              setTimeout(fetchBuild, 3000);
              setTimeout(fetchBuild, 10000);
              if (pollTimer) clearInterval(pollTimer);
              pollTimer = setInterval(fetchBuild, POLL_MS);
            }
            bindClick();
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', startPolling);
            } else {
              startPolling();
            }
            document.addEventListener('visibilitychange', function () {
              if (!document.hidden) fetchBuild();
            });
            window.addEventListener('focus', function () { fetchBuild(); });
            function bindShinyHooks() {
              if (!window.jQuery) {
                setTimeout(bindShinyHooks, 50);
                return;
              }
              /* After reconnect (e.g. post-deploy), re-check build — still no auto-reload. */
              jQuery(document).on('shiny:connected shiny:reconnected', function () {
                fetchBuild();
              });
            }
            bindShinyHooks();
          })();

          function bindMarketModeButtons() {
            placeMarketHeaderByToggle();
            var stack = document.querySelector('#ynow-market-header .ynow-market-stack');
            if (!stack) return;
            if (stack.getAttribute('data-ynow-bound') === '1') return;
            stack.setAttribute('data-ynow-bound', '1');
            stack.addEventListener('click', function (ev) {
              var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-mkt-btn') : null;
              if (!btn || !stack.contains(btn)) return;
              var val = btn.getAttribute('data-value');
              if (!val) return;
              stack.querySelectorAll('.ynow-mkt-btn').forEach(function (b) {
                b.classList.toggle('active', b === btn);
              });
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('market_mode_pick', val, {priority: 'event'});
                // Force Clustering Lab clear even if mode was already applied client-side
                Shiny.setInputValue('lab_cluster_clear_tick', Date.now(), {priority: 'event'});
              }
            });
            function pushInitial() {
              if (!(window.Shiny && Shiny.setInputValue)) {
                setTimeout(pushInitial, 50);
                return;
              }
              var active = stack.querySelector('.ynow-mkt-btn.active');
              var val = active ? active.getAttribute('data-value') : 'US';
              Shiny.setInputValue('market_mode_pick', val || 'US', {priority: 'event'});
            }
            pushInitial();
          }
          /* Language: stable custom buttons (no shinyWidgets re-render / mobile drift).
             Do not push an initial value — server + device_ui_locale own first paint;
             a premature EN push can race and lock out device zh-TW. */
          function bindLangModeButtons() {
            var stack = document.getElementById('ynow_lang_stack') ||
              document.querySelector('.ynow-lang-header .ynow-lang-stack');
            if (!stack) return;
            if (stack.getAttribute('data-ynow-bound') === '1') return;
            stack.setAttribute('data-ynow-bound', '1');
            stack.addEventListener('click', function (ev) {
              var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-lang-btn') : null;
              if (!btn || !stack.contains(btn)) return;
              var val = btn.getAttribute('data-value');
              if (!val) return;
              stack.querySelectorAll('.ynow-lang-btn').forEach(function (b) {
                b.classList.toggle('active', b === btn);
              });
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ui_locale_pick', val, {priority: 'event'});
              }
            });
          }
          function armMarketHeaderPlacement() {
            placeMarketHeaderByToggle();
            bindMarketModeButtons();
            bindLangModeButtons();
            if (typeof MutationObserver !== 'undefined') {
              var hdrNav = document.querySelector('.main-header .navbar');
              if (hdrNav && !hdrNav.getAttribute('data-ynow-mkt-obs')) {
                hdrNav.setAttribute('data-ynow-mkt-obs', '1');
                var mktObs = new MutationObserver(function () {
                  placeMarketHeaderByToggle();
                });
                /* Only react to direct navbar child moves (market relocate), not lang label text. */
                mktObs.observe(hdrNav, { childList: true, subtree: false });
              }
            }
          }
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', armMarketHeaderPlacement);
          } else {
            armMarketHeaderPlacement();
          }
          [0, 250, 1000, 2500].forEach(function (ms) {
            setTimeout(placeMarketHeaderByToggle, ms);
          });
          window.addEventListener('resize', function () {
            placeMarketHeaderByToggle();
            placeMobileAppTitle();
          });
          if (window.jQuery) {
            jQuery(document).on('shiny:connected shiny:sessioninitialized', placeMarketHeaderByToggle);
          }
          document.addEventListener('click', function (ev) {
            var t = ev.target;
            if (t && t.closest && t.closest('.sidebar-menu .treeview > a')) {
              setTimeout(function () {
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              }, 0);
              setTimeout(function () {
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              }, 200);
            }
          });

          /* ---- UI locale (en / zh-TW) in-place chrome labels ---- */
          function setMenuLabel(tab, label) {
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
            if (!a || !label) return;
            var icon = a.querySelector('i.fa:not(.pull-right), i.fas:not(.pull-right), i.far:not(.pull-right), i.glyphicon:not(.pull-right), i.ion:not(.pull-right)');
            var prc = a.querySelector('.pull-right-container');
            var angle = a.querySelector('i.fa.pull-right, i.fas.pull-right, i.far.pull-right, i.glyphicon.pull-right, i.fa-angle-left, i.fas.fa-angle-left');
            var slots = a.querySelectorAll('.ynow-rec-slot');
            var badges = a.querySelectorAll('small.ynow-sidebar-badge, small.ynow-rec-badge');
            var iconClone = icon ? icon.cloneNode(true) : null;
            var prcClone = prc ? prc.cloneNode(true) : null;
            var angleClone = (!prc && angle) ? angle.cloneNode(true) : null;
            var slotClones = [];
            if (slots.length) {
              slots.forEach(function (s) { slotClones.push(s.cloneNode(true)); });
            } else {
              badges.forEach(function (b) {
                var slot = document.createElement('span');
                slot.className = 'ynow-rec-slot';
                slot.appendChild(b.cloneNode(true));
                slotClones.push(slot);
              });
            }
            a.innerHTML = '';
            if (iconClone) a.appendChild(iconClone);
            var lab = document.createElement('span');
            lab.className = 'ynow-menu-label';
            lab.textContent = ' ' + label + ' ';
            a.appendChild(lab);
            slotClones.forEach(function (sc) { a.appendChild(sc); });
            if (prcClone) a.appendChild(prcClone);
            else if (angleClone) a.appendChild(angleClone);
          }

          function setNavLinkLabel(a, label) {
            if (!a || !label) return;
            /* 勿覆寫含 Shiny 動態輸出的頁籤（如 FCFF／FCFE） */
            if (a.querySelector('.shiny-text-output, .shiny-html-output, .shiny-bound-output')) return;
            var icon = a.querySelector('i');
            var iconClone = icon ? icon.cloneNode(true) : null;
            a.innerHTML = '';
            if (iconClone) {
              a.appendChild(iconClone);
              a.appendChild(document.createTextNode(' '));
            }
            a.appendChild(document.createTextNode(label));
          }

          function applyTabLabels(tabMap) {
            if (!tabMap) return;
            document.querySelectorAll('.nav-tabs a[data-value], .navbar-nav a[data-value]').forEach(function (a) {
              var dv = a.getAttribute('data-value');
              if (dv && tabMap[dv]) setNavLinkLabel(a, tabMap[dv]);
            });
          }

          function applyBoxHeaders(specs) {
            if (!specs || !specs.length) return;
            document.querySelectorAll('.nav-tabs-custom > .nav-tabs > li.header').forEach(function (li) {
              var text = (li.textContent || '').replace(/\\s+/g, ' ').trim();
              if (!text) return;
              for (var i = 0; i < specs.length; i++) {
                var sp = specs[i] || {};
                var aliases = sp.match || [];
                var hit = false;
                for (var j = 0; j < aliases.length; j++) {
                  var al = String(aliases[j] || '').replace(/\\s+/g, ' ').trim();
                  /* Exact match only — substring hits remapped unrelated titles (e.g. Snapshot). */
                  if (al && text === al) { hit = true; break; }
                }
                if (!hit || !sp.label) continue;
                var icon = li.querySelector('i');
                var iconClone = icon ? icon.cloneNode(true) : null;
                li.innerHTML = '';
                if (iconClone) {
                  li.appendChild(iconClone);
                  li.appendChild(document.createTextNode(' '));
                }
                li.appendChild(document.createTextNode(sp.label));
                break;
              }
            });
          }

          function applyUiLocale(payload) {
            var s = (payload && payload.strings) || {};
            if (typeof window.ynowSetUpdateLocaleStrings === 'function') {
              window.ynowSetUpdateLocaleStrings(s);
            }
            var menu = {
              home: s.menu_home,
              macro_market: s.menu_macro_market,
              asset_transmission: s.menu_asset_transmission,
              dashboard: s.menu_dashboard,
              company_advance: s.menu_company_advance,
              smart_analysis: s.menu_smart_analysis,
              get_started: s.menu_get_started,
              dcf_calculator: s.menu_dcf,
              ddm_calculator: s.menu_ddm,
              pb_calculator: s.menu_pb,
              rel_multiples_calculator: s.menu_rel_multiples,
              sotp_calculator: s.menu_sotp,
              ri_calculator: s.menu_ri,
              nav_calculator: s.menu_nav,
              sensitivity: s.menu_ynow,
              lab_notes: s.menu_backtest,
              bluechip: s.menu_bluechip,
              hfv: s.menu_hfv,
              decision_checklist: s.menu_decision_checklist,
              about: s.menu_about
            };
            Object.keys(menu).forEach(function (k) {
              if (menu[k]) setMenuLabel(k, menu[k]);
            });
            var menuMacro = document.getElementById('ynow_menu_macro');
            if (menuMacro && s.menu_macro_market) menuMacro.textContent = s.menu_macro_market;
            var menuAtx = document.getElementById('ynow_menu_asset_transmission');
            if (menuAtx && s.menu_asset_transmission) menuAtx.textContent = s.menu_asset_transmission;
            var menuCompanyAdvance = document.getElementById('ynow_menu_company_advance');
            if (menuCompanyAdvance && s.menu_company_advance) {
              menuCompanyAdvance.textContent = s.menu_company_advance;
            }
            var headerLogoHome = document.getElementById('ynow_header_logo_home');
            if (headerLogoHome && s.header_logo_home_title) {
              headerLogoHome.setAttribute('title', s.header_logo_home_title);
              headerLogoHome.setAttribute('aria-label', s.header_logo_home_title);
            }
            var smartLab = document.getElementById('ynow_menu_smart');
            if (smartLab && s.menu_smart_analysis) smartLab.textContent = s.menu_smart_analysis;
            var menuBt = document.getElementById('ynow_menu_backtest');
            if (menuBt && s.menu_backtest) menuBt.textContent = s.menu_backtest;
            setBtText('ynow_bblab_page_title', 'bblab_page_title');
            setBtText('ynow_bblab_report_kicker', 'bblab_report_kicker');
            setBtText('ynow_bblab_page_sub', 'bblab_page_sub');
            setBtText('ynow_bblab_listed_only_notice', 'bblab_listed_only_notice');
            setBtText('ynow_bblab_listed_only_scope', 'bblab_listed_only_scope');
            setBtText('ynow_bblab_shared_ticker_hint', 'bblab_shared_ticker_hint');
            setBtText('ynow_bblab_company_label', 'bblab_company_label');
            setBtText('ynow_bblab_period_label', 'bblab_period_label');
            setBtText('ynow_bblab_statement_ccy_label', 'bblab_statement_ccy_label');
            setBtText('ynow_bblab_ch1_title', 'bblab_ch1_title');
            setBtText('ynow_bblab_ch1_help', 'bblab_ch1_help');
            setBtText('ynow_bblab_ch2_title', 'bblab_ch2_title');
            setBtText('ynow_bblab_ch2_help', 'bblab_ch2_help');
            setBtText('ynow_bblab_struct_title', 'bblab_struct_title');
            setBtText('ynow_bblab_struct_help', 'bblab_struct_help');
            setBtText('ynow_bblab_struct_biz_h', 'bblab_struct_biz_h');
            setBtText('ynow_bblab_struct_adj_h', 'bblab_struct_adj_h');
            setBtText('ynow_bblab_struct_conc_h', 'bblab_struct_conc_h');
            setBtText('ynow_bblab_ch3_title', 'bblab_ch3_title');
            setBtText('ynow_bblab_ch3_help', 'bblab_ch3_help');
            setBtText('ynow_bblab_ch3_current_label', 'bblab_ch3_current_label');
            setBtText('ynow_bblab_ch4_title', 'bblab_ch4_title');
            setBtText('ynow_bblab_ch4_help', 'bblab_ch4_help');
            setBtText('ynow_bblab_ch4_limited', 'bblab_ch4_limited');
            setBtText('ynow_bblab_ch5_title', 'bblab_ch5_title');
            setBtText('ynow_bblab_ch5_help', 'bblab_ch5_help');
            setBtText('ynow_bblab_ch6_title', 'bblab_ch6_title');
            setBtText('ynow_bblab_ch7_title', 'bblab_ch7_title');
            setBtText('ynow_bblab_summary_title', 'bblab_ch2_title');
            setBtText('ynow_bblab_chart_title', 'bblab_ch3_title');
            setBtText('ynow_bblab_cards_title', 'bblab_ch5_title');
            setBtText('ynow_bblab_recon_title', 'bblab_ch6_title');
            setBtText('ynow_bblab_sources_title', 'bblab_ch7_title');
            setBtText('ynow_bblab_sources_chrome', 'bblab_sources_chrome');
            setBtText('ynow_bblab_shared_title', 'bblab_shared_title');
            setBtText('ynow_bblab_expand_other', 'bblab_expand_other');
            setBtText('ynow_bblab_fallback_gm_label', 'bblab_fallback_gm_label');
            setBtText('ynow_bblab_geo_veto_why', 'bblab_geo_veto_why');
            setBtText('ynow_bblab_overlap_why', 'bblab_overlap_why');
            setBtText('ynow_macro_page_title', 'macro_page_title');
            setBtText('ynow_macro_page_sub', 'macro_page_sub');
            setBtText('ynow_macro_rf_title', 'macro_rf_title');
            setBtText('ynow_macro_htcdi_title', 'htcdi_title');
            var htcdiBox = document.getElementById('ynow_macro_htcdi_box');
            var htcdiAlert = htcdiBox ? String(htcdiBox.getAttribute('data-htcdi-alert') || '') : '';
            if (htcdiAlert.toLowerCase() === 'unavailable') {
              setBtText('ynow_macro_htcdi_hint', 'htcdi_unavailable');
            } else if (document.body && document.body.classList.contains('ynow-lite')) {
              setBtText('ynow_macro_htcdi_hint', 'htcdi_disclosure_short');
            } else {
              setBtText('ynow_macro_htcdi_hint', 'htcdi_click_hint');
            }
            setBtText('ynow_macro_htcdi_disclosure', 'htcdi_disclosure');
            setBtText('ynow_macro_htcdi_overview_title', 'htcdi_overview_title');
            setBtText('ynow_macro_htcdi_formula', 'htcdi_formula_eq');
            setBtText('ynow_macro_htcdi_formula_parts', 'htcdi_formula_parts');
            setBtText('ynow_macro_htcdi_layer_title', 'htcdi_layer_title');
            setBtText('ynow_macro_htcdi_network_title', 'htcdi_network_title');
            setBtText('ynow_macro_htcdi_contagion_paths', 'htcdi_contagion_paths');
            setBtText('ynow_macro_htcdi_network_note', 'htcdi_network_note');
            setBtText('ynow_macro_htcdi_term_bridge', 'htcdi_term_bridge');
            setBtText('ynow_macro_htcdi_map_stmt', 'htcdi_term_map_stmt');
            setBtText('ynow_macro_htcdi_map_mkt', 'htcdi_term_map_mkt');
            setBtText('ynow_macro_htcdi_map_inf', 'htcdi_term_map_inf');
            setBtText('ynow_macro_htcdi_map_traj', 'htcdi_term_map_traj');
            setBtText('ynow_macro_htcdi_in_title', 'htcdi_in_composite_title');
            setBtText('ynow_macro_htcdi_in_help', 'htcdi_in_composite_help');
            setBtText('ynow_macro_htcdi_method_title', 'htcdi_method_title');
            setBtText('ynow_macro_index_hint', 'macro_index_chart_hint');
            setBtText('ynow_macro_ynow_index_hint', 'macro_ynow_index_chart_hint');
            setBtText('ynow_own_index_overlay_label', 'macro_own_index_overlay_label');
            setBtText('ynow_own_index_overlay_hint', 'macro_own_index_overlay_hint');
            ['ynow_index_title', 'ynow_index_chapter_title', 'ynow_index_rule',
             'ynow_index_chart_note', 'ynow_index_constituents',
             'ynow_index_col_ticker', 'ynow_index_col_name', 'ynow_index_col_mcap',
             'ynow_index_col_weight', 'ynow_index_col_last', 'ynow_index_col_chg'].forEach(function(id) {
              var el = document.getElementById(id);
              if (!el) return;
              var key = el.getAttribute('data-i18n') || id;
              setBtText(id, key);
            });
            setBtText('ynow_macro_tw_signal_title', 'macro_tw_signal_title');
            setBtText('ynow_macro_theme_title', 'macro_theme_title');
            setBtText('ynow_macro_theme_help', 'macro_theme_help');
            setBtText('ynow_macro_industry_label', 'macro_industry_label');
            setBtText('ynow_macro_concept_label', 'macro_concept_label');
            setBtText('ynow_macro_concept_cloud_hint', 'macro_concept_cloud_hint');
            setBtText('ynow_macro_period_label', 'macro_period_label');
            setBtText('ynow_macro_fx_lock', 'macro_fx_lock');
            setBtText('ynow_macro_bubble_title', 'macro_bubble_title');
            setBtText('ynow_macro_bubble_sub', 'macro_bubble_sub');
            setBtText('ynow_macro_bubble_topn_label', 'macro_bubble_topn_label');
            setBtText('ynow_macro_bubble_attr_label', 'macro_bubble_attr_label');
            setBtText('ynow_macro_bubble_conc_title', 'macro_bubble_conc_title');
            setBtText('ynow_macro_bubble_top_list_title', 'macro_bubble_top_list_title');
            setBtText('ynow_macro_bubble_conc_note', 'macro_bubble_conc_note');
            setBtText('ynow_macro_bubble_attr_title', 'macro_bubble_attr_title');
            setBtText('ynow_macro_bubble_buffett_title', 'macro_bubble_buffett_title');
            setBtText('ynow_macro_bubble_buffett_note', 'macro_bubble_buffett_note');
            var macroRefresh = document.getElementById('macro-refresh');
            if (macroRefresh && s.btn_macro_refresh) {
              var ri = macroRefresh.querySelector('i');
              var runIconHtml = ri ? ri.outerHTML + ' ' : '';
              macroRefresh.innerHTML = runIconHtml + s.btn_macro_refresh;
            }
            var catAsset = document.getElementById('ynow_menu_cat_asset');
            if (catAsset && s.menu_cat_asset) catAsset.textContent = s.menu_cat_asset;
            var catIncome = document.getElementById('ynow_menu_cat_income');
            if (catIncome && s.menu_cat_income) catIncome.textContent = s.menu_cat_income;
            var catRel = document.getElementById('ynow_menu_cat_relative');
            if (catRel && s.menu_cat_relative) catRel.textContent = s.menu_cat_relative;
            var menuRel = document.getElementById('ynow_menu_rel_multiples');
            if (menuRel && s.menu_rel_multiples) menuRel.textContent = s.menu_rel_multiples;
            var menuSotp = document.getElementById('ynow_menu_sotp');
            if (menuSotp && s.menu_sotp) menuSotp.textContent = s.menu_sotp;
            setBtText('ynow_rel_multiples_lead_title', 'rel_multiples_lead_title');
            setBtText('ynow_rel_multiples_lead_body', 'rel_multiples_lead_body');
            setBtText('ynow_rel_multiples_box_title', 'rel_multiples_box_title');
            setBtText('ynow_rel_multiples_tab_overview', 'rel_multiples_tab_overview');
            setBtText('ynow_rel_multiples_tab_equity', 'rel_multiples_tab_equity');
            setBtText('ynow_rel_multiples_tab_enterprise', 'rel_multiples_tab_enterprise');
            setBtText('ynow_rel_multiples_tab_bridge', 'rel_multiples_tab_bridge');
            setBtText('ynow_rel_param_matrix_title', 'rel_param_matrix_title');
            setBtText('ynow_rel_formula_equity', 'rel_formula_equity');
            setBtText('ynow_rel_multiples_btn_calc', 'rel_multiples_btn_calc');
            setBtText('ynow_rel_multiples_btn_sync', 'rel_multiples_btn_sync');
            setBtText('ynow_rel_mode_label', 'rel_mode_label');
            setBtText('ynow_rel_mode_help', 'rel_mode_help');
            setBtText('ynow_rel_formula_earnings', 'rel_formula_earnings');
            setBtText('ynow_rel_formula_enterprise', 'rel_formula_enterprise');
            setBtText('ynow_rel_formula_ps', 'rel_formula_ps');
            setBtText('ynow_rel_settings_pe_note', 'rel_settings_pe_note');
            setBtText('ynow_rel_settings_peg_note', 'rel_settings_peg_note');
            setBtText('ynow_rel_settings_ev_note', 'rel_settings_ev_note');
            setBtText('ynow_rel_settings_ps_note', 'rel_settings_ps_note');
            setBtText('ynow_rel_settings_bridge_note', 'rel_settings_bridge_note');
            setBtText('ynow_rel_multiples_bridge_heading', 'rel_multiples_bridge_heading');
            setBtText('ynow_rel_multiples_ps_rev_note', 'rel_multiples_ps_rev_note');
            setBtText('ynow_rel_multiples_pe_heading', 'rel_multiples_pe_heading');
            setBtText('ynow_rel_multiples_peg_heading', 'rel_multiples_peg_heading');
            setBtText('ynow_rel_multiples_ev_heading', 'rel_multiples_ev_heading');
            setBtText('ynow_rel_multiples_ps_heading', 'rel_multiples_ps_heading');
            setBtText('ynow_rel_multiples_pe_help', 'rel_multiples_pe_help');
            setBtText('ynow_rel_multiples_peg_help', 'rel_multiples_peg_help');
            setBtText('ynow_rel_multiples_ev_help', 'rel_multiples_ev_help');
            setBtText('ynow_rel_multiples_ps_help', 'rel_multiples_ps_help');
            setBtText('ynow_rel_multiples_arr_help', 'rel_multiples_arr_help');
            setBtText('ynow_rel_multiples_bridge_help', 'rel_multiples_bridge_help');
            setBtText('ynow_sotp_lead_title', 'sotp_lead_title');
            setBtText('ynow_sotp_lead_body', 'sotp_lead_body');
            setBtText('ynow_sotp_box_title', 'sotp_box_title');
            setBtText('ynow_sotp_tab_overview', 'sotp_tab_overview');
            setBtText('ynow_sotp_tab_segments', 'sotp_tab_segments');
            setBtText('ynow_sotp_tab_bridge', 'sotp_tab_bridge');
            setBtText('ynow_sotp_formula_banner', 'sotp_formula_banner');
            setBtText('ynow_sotp_settings_seg_note', 'sotp_settings_seg_note');
            setBtText('ynow_sotp_settings_bridge_note', 'sotp_settings_bridge_note');
            setBtText('ynow_sotp_bridge_heading', 'sotp_bridge_heading');
            setBtText('ynow_sotp_nonop_help', 'sotp_nonop_help');
            setBtText('ynow_sotp_btn_calc', 'sotp_btn_calc');
            setBtText('ynow_sotp_btn_sync', 'sotp_btn_sync');
            setBtText('ynow_sotp_btn_apply_mult', 'sotp_btn_apply_mult');
            setBtText('ynow_sotp_help', 'sotp_help');
            setBtText('ynow_sotp_bridge_help', 'sotp_bridge_help');
            var smartTitle = document.getElementById('ynow_smart_page_title');
            if (smartTitle && s.smart_page_title) smartTitle.textContent = s.smart_page_title;
            var smartSub = document.getElementById('ynow_smart_page_sub');
            if (smartSub && s.smart_page_sub) smartSub.textContent = s.smart_page_sub;
            var smartChart = document.getElementById('ynow_smart_chart_title');
            if (smartChart && s.smart_chart_title) smartChart.textContent = s.smart_chart_title;
            document.querySelectorAll('.ynow-lite-toggle').forEach(function (liteToggle) {
              if (s.lite_toggle_title) liteToggle.setAttribute('title', s.lite_toggle_title);
              if (s.lite_toggle_aria) liteToggle.setAttribute('aria-label', s.lite_toggle_aria);
            });
            if (LAST_BADGE_MAP) {
              LAST_BADGE_MAP.labels = {
                primary: s.menu_badge_primary || (LAST_BADGE_MAP.labels && LAST_BADGE_MAP.labels.primary) || '推薦',
                secondary: s.menu_badge_secondary || (LAST_BADGE_MAP.labels && LAST_BADGE_MAP.labels.secondary) || '備選'
              };
              applySidebarBadges(LAST_BADGE_MAP);
            }
            applyTabLabels((payload && payload.tabs) || {});
            applyBoxHeaders((payload && payload.boxes) || []);
            var hfvTitle = document.getElementById('ynow_hfv_page_title');
            if (hfvTitle && s.hfv_page_title) hfvTitle.textContent = s.hfv_page_title;
            var hfvSub = document.getElementById('ynow_hfv_page_sub');
            if (hfvSub && s.hfv_page_sub) hfvSub.textContent = s.hfv_page_sub;
            var dcTitle = document.getElementById('ynow_dc_page_title');
            if (dcTitle && s.dc_page_title) dcTitle.textContent = s.dc_page_title;
            var dcSub = document.getElementById('ynow_dc_page_sub');
            if (dcSub && s.dc_page_sub) dcSub.textContent = s.dc_page_sub;
            var dcBoxChecks = document.getElementById('ynow_dc_box_checks');
            if (dcBoxChecks && s.dc_box_checks) dcBoxChecks.textContent = s.dc_box_checks;
            var dcBoxSum = document.getElementById('ynow_dc_box_summary');
            if (dcBoxSum && s.dc_box_summary) dcBoxSum.textContent = s.dc_box_summary;
            var dcBoxSop = document.getElementById('ynow_dc_box_sop');
            if (dcBoxSop && s.dc_box_sop) dcBoxSop.textContent = s.dc_box_sop;
            var dcSopIntro = document.getElementById('ynow_dc_sop_intro');
            if (dcSopIntro && s.dc_sop_intro) dcSopIntro.textContent = s.dc_sop_intro;
            var hfvMethod = document.getElementById('ynow_hfv_sec_method');
            if (hfvMethod && s.hfv_sec_method) hfvMethod.textContent = s.hfv_sec_method;
            var hfvResults = document.getElementById('ynow_hfv_sec_results');
            if (hfvResults && s.hfv_sec_results) hfvResults.textContent = s.hfv_sec_results;
            var hfvCh1K = document.getElementById('ynow_hfv_ch1_kicker');
            if (hfvCh1K && s.hfv_ch1_kicker) hfvCh1K.textContent = s.hfv_ch1_kicker;
            var hfvCh1T = document.getElementById('ynow_hfv_ch1_title');
            if (hfvCh1T && s.hfv_ch1_title) hfvCh1T.textContent = s.hfv_ch1_title;
            var hfvCh2K = document.getElementById('ynow_hfv_ch2_kicker');
            if (hfvCh2K && s.hfv_ch2_kicker) hfvCh2K.textContent = s.hfv_ch2_kicker;
            var hfvCh2T = document.getElementById('ynow_hfv_ch2_title');
            if (hfvCh2T && s.hfv_ch2_title) hfvCh2T.textContent = s.hfv_ch2_title;
            var hfvCh2Lead = document.getElementById('ynow_hfv_ch2_lead');
            if (hfvCh2Lead && s.hfv_ch2_lead) hfvCh2Lead.textContent = s.hfv_ch2_lead;
            var hfvCh3K = document.getElementById('ynow_hfv_ch3_kicker');
            if (hfvCh3K && s.hfv_ch3_kicker) hfvCh3K.textContent = s.hfv_ch3_kicker;
            var hfvCh3T = document.getElementById('ynow_hfv_ch3_title');
            if (hfvCh3T && s.hfv_ch3_title) hfvCh3T.textContent = s.hfv_ch3_title;
            var hfvCh3Lead = document.getElementById('ynow_hfv_ch3_lead');
            if (hfvCh3Lead && s.hfv_ch3_lead) hfvCh3Lead.textContent = s.hfv_ch3_lead;
            var hfvCh4K = document.getElementById('ynow_hfv_ch4_kicker');
            if (hfvCh4K && s.hfv_ch4_kicker) hfvCh4K.textContent = s.hfv_ch4_kicker;
            var hfvCh4T = document.getElementById('ynow_hfv_ch4_title');
            if (hfvCh4T && s.hfv_ch4_title) hfvCh4T.textContent = s.hfv_ch4_title;
            var hfvCh4Lead = document.getElementById('ynow_hfv_ch4_lead');
            if (hfvCh4Lead && s.hfv_ch4_lead) hfvCh4Lead.textContent = s.hfv_ch4_lead;
            var hfvCh5K = document.getElementById('ynow_hfv_ch5_kicker');
            if (hfvCh5K && s.hfv_ch5_kicker) hfvCh5K.textContent = s.hfv_ch5_kicker;
            var hfvCh5T = document.getElementById('ynow_hfv_ch5_title');
            if (hfvCh5T && s.hfv_ch5_title) hfvCh5T.textContent = s.hfv_ch5_title;
            var hfvCh5Lead = document.getElementById('ynow_hfv_ch5_lead');
            if (hfvCh5Lead && s.hfv_ch5_lead) hfvCh5Lead.textContent = s.hfv_ch5_lead;
            var hfvOvNote = document.getElementById('ynow_hfv_overlay_vs_replay_note');
            if (hfvOvNote && s.hfv_overlay_vs_replay_note) hfvOvNote.textContent = s.hfv_overlay_vs_replay_note;
            var hfvParamInv = document.getElementById('ynow_hfv_param_inv_title');
            if (hfvParamInv && (s.hfv_param_inv_title || s.box_hfv_param_inventory)) {
              hfvParamInv.textContent = s.hfv_param_inv_title || s.box_hfv_param_inventory;
            }
            var hfvParamHelp = document.getElementById('ynow_hfv_param_inv_help');
            if (hfvParamHelp && s.hfv_param_inv_help) hfvParamHelp.textContent = s.hfv_param_inv_help;
            var hfvChart = document.getElementById('ynow_hfv_chart_gap');
            if (hfvChart && s.hfv_chart_gap) hfvChart.textContent = s.hfv_chart_gap;
            var hfvTable = document.getElementById('ynow_hfv_table_detail');
            if (hfvTable && s.hfv_table_detail) hfvTable.textContent = s.hfv_table_detail;
            var hfvDataNote = document.getElementById('ynow_hfv_method_data_note');
            if (hfvDataNote && s.hfv_method_data_note) hfvDataNote.textContent = s.hfv_method_data_note;
            var hfvMethodBody = document.getElementById('ynow_hfv_method_body');
            if (hfvMethodBody && s.hfv_method_body) hfvMethodBody.textContent = s.hfv_method_body;
            var hfvScMatrix = document.getElementById('ynow_hfv_sum_scenario_matrix');
            if (hfvScMatrix && s.hfv_sum_scenario_matrix) hfvScMatrix.textContent = s.hfv_sum_scenario_matrix;
            var hfvScThresh = document.getElementById('ynow_hfv_scenario_thresh_note');
            if (hfvScThresh && s.hfv_scenario_thresh_note) hfvScThresh.textContent = s.hfv_scenario_thresh_note;
            var fsIsHead = document.getElementById('ynow_fs_is_chart_heading');
            if (fsIsHead && s.tab_income_statement) fsIsHead.textContent = s.tab_income_statement;
            var fsCfHead = document.getElementById('ynow_fs_cf_chart_heading');
            if (fsCfHead && s.tab_cash_flow) fsCfHead.textContent = s.tab_cash_flow;
            var hfvSessTitle = document.getElementById('ynow_hfv_session_params_title');
            if (hfvSessTitle && s.hfv_session_params_title) hfvSessTitle.textContent = s.hfv_session_params_title;
            var hfvToolbar = document.getElementById('ynow_hfv_toolbar') || document.querySelector('.ynow-hfv-toolbar');
            if (hfvToolbar && s.hfv_toolbar_aria) hfvToolbar.setAttribute('aria-label', s.hfv_toolbar_aria);
            var hfvPageCtl = document.getElementById('ynow_hfv_page_controls');
            if (hfvPageCtl && s.hfv_page_controls_aria) hfvPageCtl.setAttribute('aria-label', s.hfv_page_controls_aria);
            var hfvOverlayCtl = document.getElementById('ynow_hfv_chart_overlay_controls');
            if (hfvOverlayCtl && s.hfv_chart_overlay_aria) hfvOverlayCtl.setAttribute('aria-label', s.hfv_chart_overlay_aria);
            var chartModelsLab = document.querySelector('label[for=\"bt_fv_models\"]');
            if (chartModelsLab && s.hfv_chart_models_label) chartModelsLab.textContent = s.hfv_chart_models_label;
            var replayLab = document.querySelector('label[for=\"bt_fv_replay_model\"]');
            if (replayLab && s.hfv_replay_model_label) replayLab.textContent = s.hfv_replay_model_label;
            var oosLab = document.querySelector('label[for=\"bt_fv_oos_mode\"]');
            if (oosLab && s.hfv_oos_mode_label) oosLab.textContent = s.hfv_oos_mode_label;
            var freqLab = document.querySelector('label[for=\"bt_fv_analysis_freq\"]');
            if (freqLab && s.hfv_analysis_freq_label) freqLab.textContent = s.hfv_analysis_freq_label;
            var labTitle = document.getElementById('ynow_lab_notes_title');
            if (labTitle && s.lab_notes_title) labTitle.textContent = s.lab_notes_title;
            var labSub = document.getElementById('ynow_lab_notes_sub');
            if (labSub && s.lab_notes_sub) labSub.textContent = s.lab_notes_sub;
            var btZone = document.getElementById('ynow_bt_zone_title');
            if (btZone && s.bt_zone_title) btZone.textContent = s.bt_zone_title;
            var btToolbar = document.getElementById('ynow_bt_toolbar') || document.querySelector('.ynow-backtest-toolbar');
            if (btToolbar && s.bt_toolbar_aria) btToolbar.setAttribute('aria-label', s.bt_toolbar_aria);
            var btNavCtl = document.getElementById('ynow_bt_nav_controls');
            if (btNavCtl && s.bt_nav_controls_aria) btNavCtl.setAttribute('aria-label', s.bt_nav_controls_aria);
            var navWinLab = document.querySelector('label[for=\"bt_nav_window\"]');
            if (navWinLab && s.bt_nav_window_label) navWinLab.textContent = s.bt_nav_window_label;
            function setBtText(id, key) {
              var el = document.getElementById(id);
              if (el && s[key]) el.textContent = s[key];
            }
            ['ynow_home_title', 'ynow_home_lead', 'ynow_home_method', 'ynow_home_method_lite',
             'ynow_home_market_k', 'ynow_home_market_d',
             'ynow_home_bluechip_k', 'ynow_home_bluechip_d',
             'ynow_home_company_k', 'ynow_home_company_d',
             'ynow_home_ynow_k', 'ynow_home_ynow_d',
             'ynow_home_value_k', 'ynow_home_value_d',
             'ynow_home_value_k_lite', 'ynow_home_value_d_lite',
             'ynow_home_decide_k', 'ynow_home_decide_d',
             'ynow_home_legal', 'ynow_home_legal_about'].forEach(function (id) {
              var el = document.getElementById(id);
              if (!el) return;
              var key = id.replace(/^ynow_/, '');
              setBtText(id, key);
            });
            setBtText('ynow_bt_ch1_kicker', 'bt_ch1_kicker');
            setBtText('ynow_bt_ch1_title', 'bt_ch1_title');
            setBtText('ynow_bt_ch2_kicker', 'bt_ch2_kicker');
            setBtText('ynow_bt_ch2_title', 'bt_ch2_title');
            setBtText('ynow_bt_ch2_lead', 'bt_ch2_lead');
            setBtText('ynow_bt_ch3_kicker', 'bt_ch3_kicker');
            setBtText('ynow_bt_ch3_title', 'bt_ch3_title');
            setBtText('ynow_bt_ch4_kicker', 'bt_ch4_kicker');
            setBtText('ynow_bt_ch4_title', 'bt_ch4_title');
            setBtText('ynow_bt_ch4_lead', 'bt_ch4_lead');
            setBtText('ynow_bt_exposure_title', 'bt_exposure_title');
            setBtText('ynow_bt_bh_gap_title', 'bt_bh_gap_title');
            setBtText('ynow_bt_mos_eff_title', 'bt_mos_eff_title');
            setBtText('ynow_bt_mos_eff_hint', 'bt_mos_eff_hint');
            setBtText('ynow_bt_fv_edge_title', 'bt_fv_edge_title');
            setBtText('ynow_bt_leg_fund_label', 'bt_leg_fund_label');
            setBtText('ynow_bt_leg_fund_body', 'bt_leg_fund_body');
            setBtText('ynow_bt_leg_sent_label', 'bt_leg_sent_label');
            setBtText('ynow_bt_leg_sent_body', 'bt_leg_sent_body');
            setBtText('ynow_bt_leg_bh_label', 'bt_leg_bh_label');
            setBtText('ynow_bt_leg_bh_body', 'bt_leg_bh_body');
            setBtText('ynow_bt_leg_bench_label', 'bt_leg_bench_label');
            setBtText('ynow_bt_leg_bench_body', 'bt_leg_bench_body');
            setBtText('ynow_bt_leg_hfv_note', 'bt_leg_hfv_note');
            setBtText('ynow_bt_sec_hold_gate', 'bt_sec_hold_gate');
            setBtText('ynow_bt_hold_gate_intro', 'bt_hold_gate_intro');
            setBtText('ynow_bt_kpi_filter_label', 'bt_kpi_filter_label');
            setBtText('ynow_bt_kpi_filter_hint', 'bt_kpi_filter_hint');
            setBtText('ynow_bt_sec_run_controls', 'bt_sec_run_controls');
            setBtText('ynow_bt_param_auto_hint', 'bt_param_auto_hint');
            setBtText('ynow_bt_refresh_params_hint', 'bt_refresh_params_hint');
            setBtText('ynow_bt_sec_strategy_params', 'bt_sec_strategy_params');
            setBtText('ynow_bt_params_gate_note', 'bt_params_gate_note');
            setBtText('ynow_bt_tab_fundamental', 'bt_tab_fundamental');
            setBtText('ynow_bt_tab_sentiment', 'bt_tab_sentiment');
            setBtText('ynow_bt_fund_intro', 'bt_fund_intro');
            setBtText('ynow_bt_w_vg_hint', 'bt_w_vg_hint');
            setBtText('ynow_bt_mos_ladder_title', 'bt_mos_ladder_title');
            setBtText('ynow_bt_mos_ladder_body', 'bt_mos_ladder_body');
            setBtText('ynow_bt_sent_intro', 'bt_sent_intro');
            setBtText('ynow_bt_w_mom_hint', 'bt_w_mom_hint');
            setBtText('ynow_bt_w_rsi_hint', 'bt_w_rsi_hint');
            setBtText('ynow_bt_max_exp_hint', 'bt_max_exp_hint');
            setBtText('ynow_bt_min_exp_hint', 'bt_min_exp_hint');
            setBtText('ynow_bt_fit_bh_hint', 'bt_fit_bh_hint');
            setBtText('ynow_bt_sec_methodology', 'bt_sec_methodology');
            var recent = document.getElementById('ynow_recent_search_label');
            if (recent && s.recent_search) recent.textContent = s.recent_search;
            var scLab = document.querySelector('label[for=\"sc\"]');
            if (scLab && s.ticker_label) scLab.textContent = s.ticker_label;
            var yahooIndLab = document.getElementById('ynow_industry_info_yahoo');
            if (yahooIndLab && s.industry_info_yahoo) yahooIndLab.textContent = s.industry_info_yahoo;
            var indLab = document.querySelector('label[for=\"industry_choice\"]');
            if (indLab) {
              indLab.style.display = 'none';
              indLab.textContent = '';
            }
            var sgrMethodTitle = document.getElementById('ynow_sgr_method_title');
            if (sgrMethodTitle && s.sgr_method_title) sgrMethodTitle.textContent = s.sgr_method_title;
            var lifeTitle = document.getElementById('ynow_lifecycle_stage_title');
            if (lifeTitle && s.lifecycle_stage_title) lifeTitle.textContent = s.lifecycle_stage_title;
            var lifeHelp = document.getElementById('ynow_lifecycle_stage_help');
            if (lifeHelp && s.lifecycle_stage_help) lifeHelp.textContent = s.lifecycle_stage_help;
            var sgrMethodHelp = document.getElementById('ynow_sgr_method_help');
            if (sgrMethodHelp && s.sgr_method_help) sgrMethodHelp.textContent = s.sgr_method_help;
            var sgrCustomLab = document.querySelector('label[for=\"sgr\"]');
            if (sgrCustomLab && s.sgr_custom_label) sgrCustomLab.textContent = s.sgr_custom_label;
            var indOverviewTitle = document.getElementById('ynow_ind_overview_title');
            if (indOverviewTitle && s.industry_overview_title) indOverviewTitle.textContent = s.industry_overview_title;
            var valStatusPrefix = document.getElementById('ynow_val_status_prefix');
            if (valStatusPrefix && s.composite_status_prefix) valStatusPrefix.textContent = s.composite_status_prefix;
            var g1Help = document.getElementById('ynow_g_stage1_help');
            if (g1Help && s.g_stage1_help) g1Help.textContent = s.g_stage1_help;
            var ddmModeHelp = document.getElementById('ynow_ddm_mode_help');
            if (ddmModeHelp && s.ddm_mode_help) ddmModeHelp.textContent = s.ddm_mode_help;
            var ddmD0Help = document.getElementById('ynow_ddm_d0_help');
            if (ddmD0Help && s.ddm_d0_help) ddmD0Help.textContent = s.ddm_d0_help;
            var ddmSpmTip = document.getElementById('ynow_ddm_spm_tip');
            if (ddmSpmTip && s.ddm_spm_tip) ddmSpmTip.textContent = s.ddm_spm_tip;
            var ddmGSyncHelp = document.getElementById('ynow_ddm_g_sync_help');
            if (ddmGSyncHelp && s.ddm_g_sync_help) ddmGSyncHelp.textContent = s.ddm_g_sync_help;
            var ddmSpmGNote = document.getElementById('ynow_ddm_spm_g_note');
            if (ddmSpmGNote && s.ddm_spm_g_note) ddmSpmGNote.textContent = s.ddm_spm_g_note;
            var ddmFormulaGordon = document.getElementById('ynow_ddm_formula_gordon');
            if (ddmFormulaGordon && s.ddm_formula_gordon) ddmFormulaGordon.textContent = s.ddm_formula_gordon;
            var ddmFormulaSpm = document.getElementById('ynow_ddm_formula_spm');
            if (ddmFormulaSpm && s.ddm_formula_spm) ddmFormulaSpm.textContent = s.ddm_formula_spm;
            var ddmFormulaTwo = document.getElementById('ynow_ddm_formula_two_stage');
            if (ddmFormulaTwo && s.ddm_formula_two_stage) ddmFormulaTwo.textContent = s.ddm_formula_two_stage;
            var ddmOverviewHint = document.getElementById('ynow_ddm_overview_hint');
            if (ddmOverviewHint && s.ddm_overview_hint) ddmOverviewHint.textContent = s.ddm_overview_hint;
            var ddmD0Banner = document.getElementById('ynow_ddm_d0_banner');
            if (ddmD0Banner && s.ddm_d0_banner) ddmD0Banner.textContent = s.ddm_d0_banner;
            var ddmTwoStageHelp = document.getElementById('ynow_ddm_two_stage_help');
            if (ddmTwoStageHelp && s.ddm_two_stage_help) ddmTwoStageHelp.textContent = s.ddm_two_stage_help;
            var waccBoxTitle = document.getElementById('ynow_wacc_box_title');
            if (waccBoxTitle && s.wacc_box_title) waccBoxTitle.textContent = s.wacc_box_title;
            var waccHelp = document.getElementById('ynow_wacc_help');
            if (waccHelp && s.wacc_help) waccHelp.textContent = s.wacc_help;
            var rdBoxTitle = document.getElementById('ynow_rd_box_title');
            if (rdBoxTitle && s.rd_box_title) rdBoxTitle.textContent = s.rd_box_title;
            var rdIntLab = document.querySelector('label[for=\"rd_interest_expense\"]');
            if (rdIntLab && s.rd_interest_label) rdIntLab.textContent = s.rd_interest_label;
            var rdDebtLab = document.querySelector('label[for=\"rd_interest_bearing_debt\"]');
            if (rdDebtLab && s.rd_debt_label) rdDebtLab.textContent = s.rd_debt_label;
            var rdMinLab = document.querySelector('label[for=\"wacc_rd_min\"]');
            if (rdMinLab && s.rd_min_label) rdMinLab.textContent = s.rd_min_label;
            var rdMaxLab = document.querySelector('label[for=\"wacc_rd_max\"]');
            if (rdMaxLab && s.rd_max_label) rdMaxLab.textContent = s.rd_max_label;
            var useRdLab = document.getElementById('ynow_use_estimated_rd_label');
            if (useRdLab && s.use_estimated_rd_label) useRdLab.textContent = s.use_estimated_rd_label;
            var btnCalcRd = document.getElementById('calc_rd');
            if (btnCalcRd && s.btn_calc_rd) {
              var rdIcon = btnCalcRd.querySelector('i');
              var rdIconHtml = rdIcon ? rdIcon.outerHTML + ' ' : '';
              btnCalcRd.innerHTML = rdIconHtml + s.btn_calc_rd;
            }
            var btnCalcWacc = document.getElementById('calc_wacc');
            if (btnCalcWacc && s.btn_calc_wacc) {
              var wIcon = btnCalcWacc.querySelector('i');
              var wIconHtml = wIcon ? wIcon.outerHTML + ' ' : '';
              btnCalcWacc.innerHTML = wIconHtml + s.btn_calc_wacc;
            }
            function setBtnLabel(id, label) {
              if (!id || !label) return;
              var el = document.getElementById(id);
              if (!el) return;
              var ic = el.querySelector('i');
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + label;
            }
            setBtnLabel('calc', s.btn_calc_dcf);
            setBtnLabel('mod_ddm-btn_calc_ddm', s.btn_calc_ddm);
            setBtnLabel('mod_ri-btn_calc_ri', s.btn_calc_ri);
            setBtnLabel('mod_pb-btn_calc_pb', s.btn_calc_pb);
            setBtnLabel('mod_nav-btn_calc_nav', s.btn_calc_nav);
            if (s.btn_reset_defaults) {
              document.querySelectorAll('.ynow-btn-reset').forEach(function (el) {
                var ic = el.querySelector('i');
                el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + s.btn_reset_defaults;
              });
            }
            var riCalcHint = document.getElementById('mod_ri-ynow_ri_calc_hint');
            if (riCalcHint && s.ri_calc_hint) riCalcHint.textContent = s.ri_calc_hint;
            var riSettingsHint = document.getElementById('mod_ri-ynow_ri_settings_recalc_hint');
            if (riSettingsHint && s.ri_settings_recalc_hint) riSettingsHint.textContent = s.ri_settings_recalc_hint;
            var pbSettingsHint = document.getElementById('mod_pb-ynow_pb_settings_reset_hint');
            if (pbSettingsHint && s.pb_settings_reset_hint) pbSettingsHint.textContent = s.pb_settings_reset_hint;
            var navSettingsHint = document.getElementById('mod_nav-ynow_nav_settings_reset_hint');
            if (navSettingsHint && s.nav_settings_reset_hint) navSettingsHint.textContent = s.nav_settings_reset_hint;
            var capmBoxTitle = document.getElementById('ynow_capm_box_title');
            if (capmBoxTitle && s.capm_box_title) capmBoxTitle.textContent = s.capm_box_title;
            var ddmCapmBoxTitle = document.getElementById('ynow_ddm_capm_box_title');
            if (ddmCapmBoxTitle && s.ddm_capm_box_title) ddmCapmBoxTitle.textContent = s.ddm_capm_box_title;
            var ddmKeBoxTitle = document.getElementById('ynow_ddm_ke_box_title');
            if (ddmKeBoxTitle && s.ddm_ke_box_title) ddmKeBoxTitle.textContent = s.ddm_ke_box_title;
            var ddmKeHelp = document.getElementById('ynow_ddm_ke_help');
            if (ddmKeHelp && s.ddm_ke_help) ddmKeHelp.textContent = s.ddm_ke_help;
            var ddmKeBridgeTitle = document.getElementById('ynow_ddm_ke_bridge_title');
            if (ddmKeBridgeTitle && s.ddm_ke_bridge_title) ddmKeBridgeTitle.textContent = s.ddm_ke_bridge_title;
            var ddmKeBridgeHelp = document.getElementById('ynow_ddm_ke_bridge_help');
            if (ddmKeBridgeHelp && s.ddm_ke_bridge_help) ddmKeBridgeHelp.textContent = s.ddm_ke_bridge_help;
            var ddmUseEstKe = document.getElementById('ynow_ddm_use_estimated_ke_label');
            if (ddmUseEstKe && s.ddm_use_estimated_ke_label) ddmUseEstKe.textContent = s.ddm_use_estimated_ke_label;
            setBtnLabel('calc_ddm_ke', s.btn_calc_ddm_ke);
            setBtnLabel('calc_ddm_capm', s.btn_calc_ddm_capm);
            var syncGsLab = document.getElementById('ynow_sync_gs_beta_label');
            if (syncGsLab && s.sync_gs_beta_label) syncGsLab.textContent = s.sync_gs_beta_label;
            var ddmSyncGsLab = document.getElementById('ynow_ddm_sync_gs_beta_label');
            if (ddmSyncGsLab && s.sync_gs_beta_label) ddmSyncGsLab.textContent = s.sync_gs_beta_label;
            document.querySelectorAll('.ynow-beta-source-heading').forEach(function (el) {
              if (s.beta_source_heading) el.textContent = s.beta_source_heading;
            });
            document.querySelectorAll('.ynow-beta-rolling-help').forEach(function (el) {
              if (s.beta_rolling_help) el.textContent = s.beta_rolling_help;
            });
            document.querySelectorAll('.ynow-btn-sync-selected-beta').forEach(function (el) {
              if (!s.btn_sync_selected_beta) return;
              var ic = el.querySelector('i');
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + s.btn_sync_selected_beta;
            });
            setBtnLabel('calc_capm', s.btn_calc_capm);
            setBtnLabel('calc_ddm_capm', s.btn_calc_ddm_capm || s.btn_calc_capm);
            document.querySelectorAll('.ynow-btn-calc-capm').forEach(function (el) {
              if (!s.btn_calc_capm) return;
              var ic = el.querySelector('i');
              /* Keep DDM CAPM button on its own key when present */
              var lab = (el.id === 'calc_ddm_capm' && s.btn_calc_ddm_capm) ? s.btn_calc_ddm_capm : s.btn_calc_capm;
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + lab;
            });
            var rfLab = document.querySelector('label[for=\"capm_rf\"]');
            if (rfLab && s.capm_rf_label) rfLab.textContent = s.capm_rf_label;
            var rmLab = document.querySelector('label[for=\"capm_rm\"]');
            if (rmLab && s.capm_rm_label) rmLab.textContent = s.capm_rm_label;
            var ddmRfLab = document.querySelector('label[for=\"ddm_capm_rf\"]');
            if (ddmRfLab && s.capm_rf_label) ddmRfLab.textContent = s.capm_rf_label;
            var ddmRmLab = document.querySelector('label[for=\"ddm_capm_rm\"]');
            if (ddmRmLab && s.capm_rm_label) ddmRmLab.textContent = s.capm_rm_label;
            var useEstRe = document.getElementById('ynow_use_estimated_re_label');
            if (useEstRe && s.use_estimated_re_label) useEstRe.textContent = s.use_estimated_re_label;
            var gsModelSel = document.getElementById('ynow_gs_model_selector_title');
            if (gsModelSel && s.gs_model_selector_title) gsModelSel.textContent = s.gs_model_selector_title;
            var gsBetaOv = document.getElementById('ynow_gs_beta_overview_help');
            if (gsBetaOv && s.gs_beta_overview_help) gsBetaOv.textContent = s.gs_beta_overview_help;
            var gsPeer = document.getElementById('ynow_gs_peer_unlever_help');
            if (gsPeer && s.gs_peer_unlever_help) gsPeer.textContent = s.gs_peer_unlever_help;
            var gsRoll = document.getElementById('ynow_gs_rolling_help');
            if (gsRoll && s.gs_rolling_help) gsRoll.textContent = s.gs_rolling_help;
            var rollSetTitle = document.getElementById('ynow_beta_rolling_settings_title');
            if (rollSetTitle && s.beta_rolling_settings_title) rollSetTitle.textContent = s.beta_rolling_settings_title;
            var rollCmpTitle = document.getElementById('ynow_beta_rolling_compare_title');
            if (rollCmpTitle && s.beta_rolling_compare_title) rollCmpTitle.textContent = s.beta_rolling_compare_title;
            var rollHelpBody = document.getElementById('ynow_beta_rolling_help_body');
            if (rollHelpBody && s.beta_rolling_help_body) rollHelpBody.textContent = s.beta_rolling_help_body;
            var benchLab = document.querySelector('label[for=\"beta_bench\"]');
            if (benchLab && s.beta_bench_label) benchLab.textContent = s.beta_bench_label;
            var lookLab = document.querySelector('label[for=\"beta_lookback_months\"]');
            if (lookLab && s.beta_lookback_label) lookLab.textContent = s.beta_lookback_label;
            setBtnLabel('calc_beta_est', s.btn_calc_beta_est);
            var buHead = document.getElementById('ynow_beta_bottomup_heading');
            if (buHead && s.beta_bottomup_heading) buHead.textContent = s.beta_bottomup_heading;
            var peersLab = document.querySelector('label[for=\"beta_peers\"]');
            if (peersLab && s.beta_peers_label) peersLab.textContent = s.beta_peers_label;
            var buHelp = document.getElementById('ynow_beta_bottomup_help');
            if (buHelp && s.beta_bottomup_help) buHelp.textContent = s.beta_bottomup_help;
            setBtnLabel('calc_beta_bottomup', s.btn_calc_beta_bottomup);
            var unlHead = document.getElementById('ynow_beta_unlever_heading');
            if (unlHead && s.beta_unlever_heading) unlHead.textContent = s.beta_unlever_heading;
            var unlHelp = document.getElementById('ynow_beta_unlever_help');
            if (unlHelp && s.beta_unlever_help) unlHelp.textContent = s.beta_unlever_help;
            var manHead = document.getElementById('ynow_beta_manual_heading');
            if (manHead && s.beta_manual_heading) manHead.textContent = s.beta_manual_heading;
            var manHelp = document.getElementById('ynow_beta_manual_help');
            if (manHelp && s.beta_manual_help) manHelp.textContent = s.beta_manual_help;
            var funnelPageTitle = document.getElementById('ynow_funnel_page_title');
            if (funnelPageTitle && s.funnel_page_title) funnelPageTitle.textContent = s.funnel_page_title;
            var funnelPageSub = document.getElementById('ynow_funnel_page_sub');
            if (funnelPageSub && s.funnel_page_sub) funnelPageSub.textContent = s.funnel_page_sub;
            var funnelCh1K = document.getElementById('ynow_funnel_ch1_kicker');
            if (funnelCh1K && s.funnel_ch1_kicker) funnelCh1K.textContent = s.funnel_ch1_kicker;
            var funnelCh1T = document.getElementById('ynow_funnel_ch1_title');
            if (funnelCh1T && s.funnel_ch1_title) funnelCh1T.textContent = s.funnel_ch1_title;
            var funnelCh1Lead = document.getElementById('ynow_funnel_ch1_lead');
            if (funnelCh1Lead && s.funnel_ch1_lead) funnelCh1Lead.textContent = s.funnel_ch1_lead;
            var funnelCh2K = document.getElementById('ynow_funnel_ch2_kicker');
            if (funnelCh2K && s.funnel_ch2_kicker) funnelCh2K.textContent = s.funnel_ch2_kicker;
            var funnelCh2T = document.getElementById('ynow_funnel_ch2_title');
            if (funnelCh2T && s.funnel_ch2_title) funnelCh2T.textContent = s.funnel_ch2_title;
            var funnelCh2Lead = document.getElementById('ynow_funnel_ch2_lead');
            if (funnelCh2Lead && s.funnel_ch2_lead) funnelCh2Lead.textContent = s.funnel_ch2_lead;
            var funnelCh3K = document.getElementById('ynow_funnel_ch3_kicker');
            if (funnelCh3K && s.funnel_ch3_kicker) funnelCh3K.textContent = s.funnel_ch3_kicker;
            var funnelCh3T = document.getElementById('ynow_funnel_ch3_title');
            if (funnelCh3T && s.funnel_ch3_title) funnelCh3T.textContent = s.funnel_ch3_title;
            var funnelCh3Lead = document.getElementById('ynow_funnel_ch3_lead');
            if (funnelCh3Lead && s.funnel_ch3_lead) funnelCh3Lead.textContent = s.funnel_ch3_lead;
            if (s.notes_title) {
              document.querySelectorAll('.ynow-notes__title').forEach(function (el) {
                el.textContent = s.notes_title;
              });
            }
            if (s.notes_toggle_aria) {
              document.querySelectorAll('.ynow-notes__summary').forEach(function (el) {
                if (el.id === 'ynow_ms_dims_summary') return;
                el.setAttribute('title', s.notes_toggle_aria);
                el.setAttribute('aria-label', s.notes_toggle_aria);
              });
            }
            var msDimsTitle = document.getElementById('ynow_ms_dims_title');
            if (msDimsTitle && s.gs_ms_dims_title) msDimsTitle.textContent = s.gs_ms_dims_title;
            var msDimsSummary = document.getElementById('ynow_ms_dims_summary');
            if (msDimsSummary && s.gs_ms_dims_toggle_aria) {
              msDimsSummary.setAttribute('title', s.gs_ms_dims_toggle_aria);
              msDimsSummary.setAttribute('aria-label', s.gs_ms_dims_toggle_aria);
            }
            var kpiJumpMos = document.getElementById('ynow_kpi_jump_mos');
            if (kpiJumpMos && s.funnel_kpi_jump_mos_aria) kpiJumpMos.setAttribute('aria-label', s.funnel_kpi_jump_mos_aria);
            var kpiJumpFs = document.getElementById('ynow_kpi_jump_fscore');
            if (kpiJumpFs && s.funnel_kpi_jump_fscore_aria) kpiJumpFs.setAttribute('aria-label', s.funnel_kpi_jump_fscore_aria);
            var kpiJumpAl = document.getElementById('ynow_kpi_jump_alerts');
            if (kpiJumpAl && s.funnel_kpi_jump_alerts_aria) kpiJumpAl.setAttribute('aria-label', s.funnel_kpi_jump_alerts_aria);
            var funnelSecMethod = document.getElementById('ynow_funnel_sec_method');
            if (funnelSecMethod && s.funnel_sec_method) funnelSecMethod.textContent = s.funnel_sec_method;
            var funnelMethodBody = document.getElementById('ynow_funnel_method_body');
            if (funnelMethodBody && s.funnel_method_body) funnelMethodBody.textContent = s.funnel_method_body;
            var funnelMethodCaveat = document.getElementById('ynow_funnel_method_caveat');
            if (funnelMethodCaveat && s.funnel_method_caveat) funnelMethodCaveat.textContent = s.funnel_method_caveat;
            var funnelFs = document.getElementById('ynow_funnel_fscore_list_title');
            if (funnelFs && s.funnel_fscore_list_title) funnelFs.textContent = s.funnel_fscore_list_title;
            var funnelMomTitle = document.getElementById('ynow_funnel_mom_box_title');
            if (funnelMomTitle && s.funnel_mom_box_title) funnelMomTitle.textContent = s.funnel_mom_box_title;
            var funnelMomIntro = document.getElementById('ynow_funnel_mom_intro');
            if (funnelMomIntro && s.funnel_mom_intro) funnelMomIntro.textContent = s.funnel_mom_intro;
            var funnelMomLogic = document.getElementById('ynow_funnel_mom_logic_title');
            if (funnelMomLogic && s.funnel_mom_logic_title) funnelMomLogic.textContent = s.funnel_mom_logic_title;
            var funnelMomCond1 = document.getElementById('ynow_funnel_mom_cond1');
            if (funnelMomCond1 && s.funnel_mom_cond1) funnelMomCond1.textContent = s.funnel_mom_cond1;
            var funnelMomCond1Lab = document.getElementById('ynow_funnel_mom_cond1_label');
            if (funnelMomCond1Lab && s.funnel_mom_cond1_label) funnelMomCond1Lab.textContent = s.funnel_mom_cond1_label;
            var funnelMomCond2 = document.getElementById('ynow_funnel_mom_cond2');
            if (funnelMomCond2 && s.funnel_mom_cond2) funnelMomCond2.textContent = s.funnel_mom_cond2;
            var funnelMomCond2Lab = document.getElementById('ynow_funnel_mom_cond2_label');
            if (funnelMomCond2Lab && s.funnel_mom_cond2_label) funnelMomCond2Lab.textContent = s.funnel_mom_cond2_label;
            var funnelMomBull = document.getElementById('ynow_funnel_mom_bull_rule');
            if (funnelMomBull && s.funnel_mom_bull_rule) funnelMomBull.textContent = s.funnel_mom_bull_rule;
            var funnelMomData = document.getElementById('ynow_funnel_mom_data_title');
            if (funnelMomData && s.funnel_mom_data_title) funnelMomData.textContent = s.funnel_mom_data_title;
            var funnelMomD1 = document.getElementById('ynow_funnel_mom_data_1');
            if (funnelMomD1 && s.funnel_mom_data_1) funnelMomD1.textContent = s.funnel_mom_data_1;
            var funnelMomD2 = document.getElementById('ynow_funnel_mom_data_2');
            if (funnelMomD2 && s.funnel_mom_data_2) funnelMomD2.textContent = s.funnel_mom_data_2;
            var funnelMomD3 = document.getElementById('ynow_funnel_mom_data_3');
            if (funnelMomD3 && s.funnel_mom_data_3) funnelMomD3.textContent = s.funnel_mom_data_3;
            var kpiBlue = document.getElementById('ynow_kpi_legend_blue');
            if (kpiBlue && s.kpi_legend_blue) kpiBlue.textContent = s.kpi_legend_blue;
            var kpiRed = document.getElementById('ynow_kpi_legend_red');
            if (kpiRed && s.kpi_legend_red) kpiRed.textContent = s.kpi_legend_red;
            var kpiBlack = document.getElementById('ynow_kpi_legend_black');
            if (kpiBlack && s.kpi_legend_black) kpiBlack.textContent = s.kpi_legend_black;
            var kpiWhite = document.getElementById('ynow_kpi_legend_white');
            if (kpiWhite && s.kpi_legend_white) kpiWhite.textContent = s.kpi_legend_white;
            var kpiFocus = document.getElementById('ynow_kpi_legend_focus');
            if (kpiFocus && s.kpi_legend_focus_metric) kpiFocus.textContent = s.kpi_legend_focus_metric;
            var fundProfLab = document.getElementById('ynow_fund_profile_label');
            if (fundProfLab && s.fund_profile_label) fundProfLab.textContent = s.fund_profile_label + '：';
            var fundBadge = document.getElementById('ynow_fund_profile_badge_text');
            if (fundBadge) {
              var pid = fundBadge.getAttribute('data-profile-id') || '';
              var pkey = pid ? ('fund_profile_' + pid) : '';
              if (pkey && s[pkey]) fundBadge.textContent = s[pkey];
            }
            var dsTitle = document.getElementById('ynow_data_source_title');
            if (dsTitle && s.data_source_title) dsTitle.textContent = s.data_source_title;
            var dsBody = document.getElementById('ynow_data_source_body');
            if (dsBody && s.data_source_body) dsBody.textContent = s.data_source_body;
            var dl = document.getElementById('download_report');
            if (dl && s.download_report) {
              var spans = dl.querySelectorAll('span, .shiny-download-link');
              /* downloadButton text is in the button itself after icon */
              var icon = dl.querySelector('i');
              var iconHtml = icon ? icon.outerHTML + ' ' : '';
              dl.innerHTML = iconHtml + s.download_report;
            }
            var snap = document.getElementById('ynow_snapshot_link_label');
            if (snap && s.snapshot_link) snap.textContent = s.snapshot_link;
            var snapTitle = document.getElementById('ynow_snapshot_page_title');
            if (snapTitle && s.snapshot_page_title) snapTitle.textContent = s.snapshot_page_title;
            var snapHelp = document.getElementById('ynow_snapshot_page_help');
            if (snapHelp && s.snapshot_page_help) snapHelp.textContent = s.snapshot_page_help;
            var snapHelpLite = document.getElementById('ynow_snapshot_page_help_lite');
            if (snapHelpLite && s.snapshot_page_help_lite) snapHelpLite.textContent = s.snapshot_page_help_lite;
            var snapDefaultsHelpLite = document.getElementById('ynow_snapshot_defaults_help_lite');
            if (snapDefaultsHelpLite && s.snapshot_defaults_help_lite) {
              snapDefaultsHelpLite.textContent = s.snapshot_defaults_help_lite;
            }
            var snapTabAudit = document.getElementById('ynow_snapshot_tab_audit');
            if (snapTabAudit && (s.snapshot_tab_audit || s.param_audit_title)) {
              snapTabAudit.textContent = s.snapshot_tab_audit || s.param_audit_title;
            }
            var snapTabCurrent = document.getElementById('ynow_snapshot_tab_current');
            if (snapTabCurrent && s.snapshot_tab_current) snapTabCurrent.textContent = s.snapshot_tab_current;
            var snapTabDefaults = document.getElementById('ynow_snapshot_tab_defaults');
            if (snapTabDefaults && s.snapshot_tab_defaults) snapTabDefaults.textContent = s.snapshot_tab_defaults;
            var snapDefaultsHelp = document.getElementById('ynow_snapshot_defaults_help');
            if (snapDefaultsHelp && s.snapshot_defaults_help) snapDefaultsHelp.textContent = s.snapshot_defaults_help;
            var dlSnapBtn = document.getElementById('ynow_download_snapshot_btn');
            if (dlSnapBtn && s.download_snapshot_btn) dlSnapBtn.textContent = s.download_snapshot_btn;
            var dlRestoreBtn = document.getElementById('ynow_download_param_restore_btn');
            if (dlRestoreBtn && s.download_param_restore_btn) dlRestoreBtn.textContent = s.download_param_restore_btn;
            var restoreTitle = document.getElementById('ynow_param_restore_title');
            if (restoreTitle && s.param_restore_title) restoreTitle.textContent = s.param_restore_title;
            var restoreHelp = document.getElementById('ynow_param_restore_help');
            if (restoreHelp && s.param_restore_help) restoreHelp.textContent = s.param_restore_help;
            var restoreFileLab = document.getElementById('ynow_param_restore_file_label');
            if (restoreFileLab && s.param_restore_file_label) restoreFileLab.textContent = s.param_restore_file_label;
            var restoreBtn = document.getElementById('ynow_param_restore_btn');
            if (restoreBtn && s.param_restore_btn) restoreBtn.textContent = s.param_restore_btn;
            var dlDefaultsBtn = document.getElementById('ynow_download_defaults_btn');
            if (dlDefaultsBtn && s.download_defaults_btn) dlDefaultsBtn.textContent = s.download_defaults_btn;
            var restoreBrowseRoot = document.getElementById('param_restore_file');
            if (restoreBrowseRoot) {
              var restoreBrowse = restoreBrowseRoot.closest('.form-group, .shiny-input-container');
              if (restoreBrowse) {
                var browseBtn = restoreBrowse.querySelector('.btn-file, .input-group-btn label, .input-group-btn .btn');
                if (browseBtn && s.param_restore_browse_btn) {
                  var onlyText = true;
                  browseBtn.childNodes.forEach(function (n) {
                    if (n.nodeType === 3 && String(n.textContent || '').trim()) {
                      n.textContent = ' ' + s.param_restore_browse_btn + ' ';
                      onlyText = false;
                    }
                  });
                  if (onlyText && !browseBtn.querySelector('i')) browseBtn.textContent = s.param_restore_browse_btn;
                }
                var ph = restoreBrowse.querySelector('input[type=\"text\"]');
                if (ph && s.param_restore_placeholder) {
                  ph.setAttribute('placeholder', s.param_restore_placeholder);
                  ph.placeholder = s.param_restore_placeholder;
                }
              }
            }
            var mktUs = document.getElementById('ynow_mkt_btn_us');
            if (mktUs && s.market_us) mktUs.textContent = s.market_us;
            var mktTw = document.getElementById('ynow_mkt_btn_tw');
            if (mktTw && s.market_tw) mktTw.textContent = s.market_tw;
            var mktStack = document.querySelector('#ynow-market-header .ynow-market-stack');
            if (mktStack && s.market_hint) mktStack.setAttribute('aria-label', s.market_hint);
            var langStack = document.getElementById('ynow_lang_stack') ||
              document.querySelector('.ynow-lang-header .ynow-lang-stack');
            if (langStack && s.hdr_lang_label) langStack.setAttribute('aria-label', s.hdr_lang_label);
            var langZh = document.getElementById('ynow_lang_btn_zh');
            var langEn = document.getElementById('ynow_lang_btn_en');
            if (langZh && s.hdr_lang_zh) langZh.textContent = s.hdr_lang_zh;
            if (langEn && s.hdr_lang_en) langEn.textContent = s.hdr_lang_en;
            var loc = (payload && payload.locale) ? String(payload.locale) : 'en';
            document.querySelectorAll('.ynow-lang-stack .ynow-lang-btn').forEach(function (b) {
              b.classList.toggle('active', b.getAttribute('data-value') === loc);
            });
            var ccyFloat = document.querySelector('.ynow-ccy-float');
            if (ccyFloat && s.hdr_ccy_label) ccyFloat.setAttribute('aria-label', s.hdr_ccy_label);
            setBtText('ynow_hfv_rel_models_note', 'hfv_rel_models_note');
            setBtText('ynow_lab_im_methods_label', 'lab_im_methods_label');
            var hfvBenchLab = document.getElementById('ynow_hfv_show_bench_label');
            if (hfvBenchLab && s.hfv_show_bench) hfvBenchLab.textContent = s.hfv_show_bench;
            var convLab = document.querySelector('label[for=\"bt_fv_conv_window\"]');
            if (convLab && s.hfv_conv_window_label) convLab.textContent = s.hfv_conv_window_label;
            var paHelp = document.getElementById('ynow_param_audit_help');
            if (paHelp && s.param_audit_help) paHelp.textContent = s.param_audit_help;
            var paPdfTitle = document.getElementById('ynow_param_audit_pdf_title');
            if (paPdfTitle && s.param_audit_pdf_title) paPdfTitle.textContent = s.param_audit_pdf_title;
            var paPdfHelp = document.getElementById('ynow_param_audit_pdf_help');
            if (paPdfHelp && s.param_audit_pdf_help) paPdfHelp.textContent = s.param_audit_pdf_help;
            var paPdfPages = document.getElementById('ynow_param_audit_pdf_pages_label');
            if (paPdfPages && s.param_audit_pdf_pages_label) paPdfPages.textContent = s.param_audit_pdf_pages_label;
            var paPdfBtn = document.getElementById('ynow_param_audit_pdf_btn');
            if (paPdfBtn && s.param_audit_pdf_btn) paPdfBtn.textContent = s.param_audit_pdf_btn;
            document.querySelectorAll('.ynow-pdf-page-lab').forEach(function (el) {
              var k = el.getAttribute('data-key');
              if (k && s[k]) el.textContent = s[k];
            });
            var testL = document.getElementById('ynow_test_link_label');
            if (testL && s.test_link) testL.textContent = s.test_link;
            var fb = document.getElementById('ynow_feedback_link_label');
            if (fb && s.feedback_link) fb.textContent = s.feedback_link;
            var testTitle = document.getElementById('ynow_testing_page_title');
            if (testTitle && s.testing_page_title) testTitle.textContent = s.testing_page_title;
            var testSub = document.getElementById('ynow_testing_page_sub');
            if (testSub && s.testing_page_sub) testSub.textContent = s.testing_page_sub;
            var testBoxBody = document.getElementById('ynow_testing_box_body');
            if (testBoxBody && s.testing_box_body) testBoxBody.textContent = s.testing_box_body;
            var atxTitle = document.getElementById('ynow_atx_page_title');
            if (atxTitle && s.atx_page_title) atxTitle.textContent = s.atx_page_title;
            var atxSub = document.getElementById('ynow_atx_page_sub');
            if (atxSub && s.atx_page_sub) atxSub.textContent = s.atx_page_sub;
            var atxBoxTitle = document.getElementById('ynow_atx_box_title');
            if (atxBoxTitle && s.atx_box_title) atxBoxTitle.textContent = s.atx_box_title;
            var atxBoxBody = document.getElementById('ynow_atx_box_body');
            if (atxBoxBody && s.atx_box_body) atxBoxBody.textContent = s.atx_box_body;
            var runBt = document.getElementById('run_bt');
            if (runBt && s.btn_run_bt) {
              var runIcon = runBt.querySelector('i');
              var runIconHtml = runIcon ? runIcon.outerHTML + ' ' : '';
              runBt.innerHTML = runIconHtml + s.btn_run_bt;
            }
            setBtnLabel('bt_kpi_filter', s.btn_bt_kpi_filter);
            setBtnLabel('bt_refresh_params', s.btn_bt_refresh_params);
            setBtnLabel('bt_fit_bh_preset', s.btn_bt_fit_bh);
            var labImRun = document.getElementById('lab_im_run_fscore');
            if (labImRun && s.btn_lab_im_run) {
              var labIcon = labImRun.querySelector('i');
              var labIconHtml = labIcon ? labIcon.outerHTML + ' ' : '';
              labImRun.innerHTML = labIconHtml + s.btn_lab_im_run;
            }
            var labMaxNLabel = document.getElementById('ynow_lab_im_max_n_label');
            if (labMaxNLabel && s.lab_im_max_n_label) labMaxNLabel.textContent = s.lab_im_max_n_label;
            var labMaxNCustom = document.getElementById('ynow_lab_im_max_n_custom_label');
            if (labMaxNCustom && s.lab_im_max_n_custom_label) labMaxNCustom.textContent = s.lab_im_max_n_custom_label;
            var labPoolRank = document.getElementById('ynow_lab_im_pool_rank_label');
            if (labPoolRank && s.lab_im_pool_rank_label) labPoolRank.textContent = s.lab_im_pool_rank_label;
            var labConcepts = document.getElementById('ynow_lab_im_concepts_label');
            if (labConcepts && s.lab_im_concepts_label) labConcepts.textContent = s.lab_im_concepts_label;
            var labDetailIntro = document.getElementById('ynow_lab_im_detail_intro');
            if (labDetailIntro && s.lab_im_detail_intro) labDetailIntro.textContent = s.lab_im_detail_intro;
            setBtText('ynow_lab_im_detail_box_title', 'lab_im_detail_box_title');
            var labLbMode = document.getElementById('ynow_lab_im_lb_mode_label');
            if (labLbMode && s.lab_im_lb_mode_label) labLbMode.textContent = s.lab_im_lb_mode_label;
            var labLbOverall = document.getElementById('ynow_lab_im_lb_mode_overall');
            if (labLbOverall && s.lab_im_lb_mode_overall) labLbOverall.textContent = s.lab_im_lb_mode_overall;
            var labLbByInd = document.getElementById('ynow_lab_im_lb_mode_by_ind');
            if (labLbByInd && s.lab_im_lb_mode_by_industry) labLbByInd.textContent = s.lab_im_lb_mode_by_industry;
            var labLbIndAvg = document.getElementById('ynow_lab_im_lb_mode_ind_avg');
            if (labLbIndAvg && s.lab_im_lb_mode_industry_avg) labLbIndAvg.textContent = s.lab_im_lb_mode_industry_avg;
            var labBoardsLabel = document.getElementById('ynow_lab_im_boards_label');
            if (labBoardsLabel && s.lab_im_boards_label) labBoardsLabel.textContent = s.lab_im_boards_label;
            var labBoardTwse = document.getElementById('ynow_lab_im_board_twse');
            if (labBoardTwse && s.lab_im_board_twse) labBoardTwse.textContent = s.lab_im_board_twse;
            var labBoardTpex = document.getElementById('ynow_lab_im_board_tpex');
            if (labBoardTpex && s.lab_im_board_tpex) labBoardTpex.textContent = s.lab_im_board_tpex;
            var labBoardEsb = document.getElementById('ynow_lab_im_board_esb');
            if (labBoardEsb && s.lab_im_board_esb) labBoardEsb.textContent = s.lab_im_board_esb;
            var labBoardsHint = document.getElementById('ynow_lab_im_boards_hint');
            if (labBoardsHint && s.lab_im_boards_hint) labBoardsHint.textContent = s.lab_im_boards_hint;
            var labLbHelp = document.getElementById('ynow_lab_im_lb_scope_help');
            if (labLbHelp && s.lab_im_lb_scope_help) labLbHelp.textContent = s.lab_im_lb_scope_help;
            var labGateLabel = document.getElementById('ynow_lab_im_gate_label');
            if (labGateLabel && s.lab_im_gate_label) labGateLabel.textContent = s.lab_im_gate_label;
            var labGateHint = document.getElementById('ynow_lab_im_gate_hint');
            if (labGateHint && s.lab_im_gate_hint) labGateHint.textContent = s.lab_im_gate_hint;
            var labImRunTitle = document.getElementById('lab_im_run_fscore');
            if (labImRunTitle && s.lab_im_run_title) labImRunTitle.setAttribute('title', s.lab_im_run_title);
            var annFs = document.getElementById('ynow_ann_fscore_crossref');
            if (annFs && s.ann_fscore_crossref) annFs.textContent = s.ann_fscore_crossref;
            var labAdrLabel = document.getElementById('ynow_lab_im_include_adr_label');
            if (labAdrLabel && s.lab_im_include_adr_label) labAdrLabel.textContent = s.lab_im_include_adr_label;
            var labAdrHint = document.getElementById('ynow_lab_im_include_adr_hint');
            if (labAdrHint && s.lab_im_include_adr_hint) labAdrHint.textContent = s.lab_im_include_adr_hint;
            var labEqLabel = document.getElementById('ynow_lab_im_eq_label');
            if (labEqLabel && s.lab_im_eq_label) labEqLabel.textContent = s.lab_im_eq_label;
            var labEqHint = document.getElementById('ynow_lab_im_eq_hint');
            if (labEqHint && s.lab_im_eq_hint) labEqHint.textContent = s.lab_im_eq_hint;
            var labEqExplainTitle = document.getElementById('ynow_lab_im_eq_explain_title');
            if (labEqExplainTitle && s.lab_im_eq_explain_title) labEqExplainTitle.textContent = s.lab_im_eq_explain_title;
            var labEqExplainBody = document.getElementById('ynow_lab_im_eq_explain_body');
            if (labEqExplainBody && s.lab_im_eq_explain_body) labEqExplainBody.textContent = s.lab_im_eq_explain_body;
            var clusterBlurb = document.getElementById('ynow_lab_cluster_blurb');
            if (clusterBlurb && s.lab_cluster_blurb) clusterBlurb.textContent = s.lab_cluster_blurb;
            var clusterDisc = document.getElementById('ynow_lab_cluster_disclaimer');
            if (clusterDisc && s.lab_cluster_disclaimer) clusterDisc.textContent = s.lab_cluster_disclaimer;
            var clusterHint = document.getElementById('ynow_lab_cluster_hint');
            if (clusterHint && s.lab_cluster_hint) clusterHint.textContent = s.lab_cluster_hint;
            var clusterK = document.getElementById('ynow_lab_cluster_k_label');
            if (clusterK && s.lab_cluster_k_label) clusterK.textContent = s.lab_cluster_k_label;
            var clusterX = document.getElementById('ynow_lab_cluster_x_label');
            if (clusterX && s.lab_cluster_x_label) clusterX.textContent = s.lab_cluster_x_label;
            var clusterY = document.getElementById('ynow_lab_cluster_y_label');
            if (clusterY && s.lab_cluster_y_label) clusterY.textContent = s.lab_cluster_y_label;
            var clusterFocus = document.getElementById('ynow_lab_cluster_focus_label');
            if (clusterFocus && s.lab_cluster_focus_label) clusterFocus.textContent = s.lab_cluster_focus_label;
            var clusterRun = document.getElementById('ynow_lab_cluster_run_label');
            if (clusterRun && s.btn_lab_cluster_run) clusterRun.textContent = s.btn_lab_cluster_run;
            var clusterMap = document.getElementById('ynow_lab_cluster_map_title');
            if (clusterMap && s.lab_cluster_map_title) clusterMap.textContent = s.lab_cluster_map_title;
            var clusterRadar = document.getElementById('ynow_lab_cluster_radar_title');
            if (clusterRadar && s.lab_cluster_radar_title) clusterRadar.textContent = s.lab_cluster_radar_title;
            var clusterTable = document.getElementById('ynow_lab_cluster_table_title');
            if (clusterTable && s.lab_cluster_table_title) clusterTable.textContent = s.lab_cluster_table_title;
            document.documentElement.setAttribute('lang', (payload && payload.locale) || 'en');
            var mkt = (payload && payload.market) ? String(payload.market) : 'US';
            document.body.classList.toggle('ynow-market-tw', mkt === 'TW');
            document.querySelectorAll('#ynow-market-header .ynow-mkt-btn').forEach(function (b) {
              b.classList.toggle('active', b.getAttribute('data-value') === mkt);
            });
          }

          function applyDcLocale(payload) {
            if (!payload) return;
            var hint = document.getElementById('ynow_dc_panel_hint');
            if (hint && payload.panel_hint) hint.textContent = payload.panel_hint;
            (payload.sections || []).forEach(function (sec) {
              var st = document.getElementById('ynow_dc_section_' + sec.id);
              if (st && sec.title) st.textContent = sec.title;
            });
            var items = payload.items || [];
            items.forEach(function (it) {
              var lab = document.getElementById('ynow_dc_label_' + it.id);
              if (lab && it.label) lab.textContent = it.label;
              var h = document.getElementById('ynow_dc_hint_' + it.id);
              if (h && it.hint) h.textContent = it.hint;
              var badge = document.getElementById('ynow_dc_badge_' + it.id);
              if (badge && payload.badge) badge.textContent = payload.badge;
              (it.conds || []).forEach(function (c) {
                var el = document.querySelector('label[for=\"' + c.input_id + '\"]');
                if (el && c.label) el.textContent = c.label;
                var ch = c.hint_id ? document.getElementById(c.hint_id) : null;
                if (ch && c.hint) ch.textContent = c.hint;
              });
            });
          }
          function registerLocaleHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerLocaleHandler, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowUiLocale', applyUiLocale);
            Shiny.addCustomMessageHandler('ynowDcLocale', applyDcLocale);
            Shiny.addCustomMessageHandler('ynowDcSopLocale', function (payload) {
              if (!payload) return;
              var set = function (id, val) {
                var el = document.getElementById(id);
                if (el && val) el.textContent = val;
              };
              set('ynow_dc_sop_intro', payload.intro);
              set('ynow_dc_box_sop', payload.box_sop);
              set('ynow_dc_sop_step1_title', payload.step1);
              set('ynow_dc_sop_step2_title', payload.step2);
              set('ynow_dc_sop_step3_title', payload.step3);
              set('ynow_dc_sop_ack_sgr_label', payload.ack_sgr);
              set('ynow_dc_sop_ack_sens_label', payload.ack_sens);
              set('ynow_dc_sop_ack_qual_label', payload.ack_qual);
            });
            Shiny.addCustomMessageHandler('ynowModelTheme', function (payload) {
              var tab = (payload && payload.tab) ? String(payload.tab) : '';
              /* Keys match Model Selector card accents (NAV/DCF/DDM/RI/P/B/Multiples/SOTP). */
              var map = {
                nav_calculator: 'nav',
                dcf_calculator: 'dcf',
                ddm_calculator: 'ddm',
                ri_calculator: 'ri',
                pb_calculator: 'pb',
                rel_multiples_calculator: 'multiples',
                sotp_calculator: 'sotp'
              };
              var key = map[tab] || null;
              ['nav', 'dcf', 'ddm', 'ri', 'pb', 'multiples', 'sotp'].forEach(function (k) {
                document.body.classList.toggle('ynow-theme-' + k, k === key);
              });
              document.body.classList.toggle('ynow-theme-model', !!key);
              document.body.classList.toggle('ynow-theme-bluechip', tab === 'bluechip');
              document.body.classList.toggle('ynow-theme-atx', tab === 'asset_transmission');
            });
          }
          registerLocaleHandler();

          /* Device / browser language → initial UI locale (mapping done in R). */
          function pushDeviceUiLocale() {
            if (!(window.Shiny && Shiny.setInputValue)) {
              setTimeout(pushDeviceUiLocale, 50);
              return;
            }
            if (window.__ynowDeviceLocalePushed) return;
            window.__ynowDeviceLocalePushed = true;
            var tags = [];
            try {
              if (navigator.languages && navigator.languages.length) {
                for (var i = 0; i < navigator.languages.length; i++) {
                  var t = String(navigator.languages[i] || '').trim();
                  if (t) tags.push(t);
                }
              } else if (navigator.language) {
                var one = String(navigator.language || '').trim();
                if (one) tags.push(one);
              }
            } catch (eLang) {}
            Shiny.setInputValue('device_ui_locale', tags.join(','), {priority: 'event'});
          }
          pushDeviceUiLocale();

          /* ---- YNOW KPI cards: click / keyboard scroll to chapter anchors ---- */
          function ynowScrollFunnelAnchor(id) {
            if (!id) return false;
            var el = document.getElementById(id);
            if (!el) return false;
            try { el.scrollIntoView({ behavior: 'smooth', block: 'start' }); }
            catch (e) { try { el.scrollIntoView(true); } catch (e2) {} }
            return true;
          }
          function ynowOnFunnelKpiJump(ev) {
            var a = ev.target && ev.target.closest ? ev.target.closest('a.ynow-funnel-kpi-jump') : null;
            if (!a) return;
            var href = a.getAttribute('href') || '';
            if (href.charAt(0) !== '#') return;
            var id = href.slice(1);
            if (!ynowScrollFunnelAnchor(id)) return;
            ev.preventDefault();
            if (window.history && history.replaceState) {
              try { history.replaceState(null, '', href); } catch (e3) {}
            }
          }
          document.addEventListener('click', ynowOnFunnelKpiJump);
          document.addEventListener('keydown', function (ev) {
            if (ev.key !== ' ' && ev.key !== 'Spacebar') return;
            var a = ev.target && ev.target.closest ? ev.target.closest('a.ynow-funnel-kpi-jump') : null;
            if (!a) return;
            ev.preventDefault();
            a.click();
          });

          /* ---- Lite mode: sidebar logo click toggles body.ynow-lite + LITE badge ---- */
          (function () {
            var FULL_ONLY_TABS = [
              'get_started', 'nav_calculator', 'dcf_calculator', 'ddm_calculator',
              'ri_calculator', 'pb_calculator', 'rel_multiples_calculator', 'sotp_calculator', 'hfv',
              'decision_checklist', 'lab_notes', 'testing', 'company_advance'
            ];
            function remapLegacyTab(tab) {
              tab = String(tab || '');
              if (tab === 'business_breakdown_lab') return 'company_advance';
              return tab;
            }
            window.ynowRemapLegacyTab = remapLegacyTab;
            function ynowTabAnchor(tab) {
              return document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]') ||
                     document.querySelector('a[data-value=\"' + tab + '\"]');
            }
            function currentSidebarTab() {
              var active = document.querySelector('.sidebar-menu li.active > a[data-value]');
              if (active) return remapLegacyTab(active.getAttribute('data-value') || '');
              var pane = document.querySelector('.tab-content > .tab-pane.active');
              if (pane) {
                var id = pane.id || '';
                if (id.indexOf('shiny-tab-') === 0) {
                  return remapLegacyTab(id.slice('shiny-tab-'.length));
                }
                return remapLegacyTab(pane.getAttribute('data-value') || '');
              }
              return '';
            }
            function gotoTab(tab) {
              tab = remapLegacyTab(tab);
              if (!tab) return;
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
              }
              var a = ynowTabAnchor(tab);
              if (a) {
                try { a.click(); } catch (e) {}
              }
            }
            window.ynowGotoTab = gotoTab;
            function bindModelSelectorJump() {
              if (document.documentElement.getAttribute('data-ynow-ms-jump') === '1') return;
              document.documentElement.setAttribute('data-ynow-ms-jump', '1');
              document.addEventListener('click', function (ev) {
                var card = ev.target && ev.target.closest
                  ? ev.target.closest('.ynow-model-card[data-tab]')
                  : null;
                if (!card) return;
                ev.preventDefault();
                var tab = card.getAttribute('data-tab');
                if (tab) gotoTab(tab);
              });
              document.addEventListener('keydown', function (ev) {
                var card = ev.target && ev.target.closest
                  ? ev.target.closest('.ynow-model-card[data-tab]')
                  : null;
                if (!card) return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                var tab = card.getAttribute('data-tab');
                if (tab) gotoTab(tab);
              });
            }
            bindModelSelectorJump();
            function bindHeaderLogoHome() {
              if (document.documentElement.getAttribute('data-ynow-header-logo-home') === '1') return;
              document.documentElement.setAttribute('data-ynow-header-logo-home', '1');
              document.addEventListener('click', function (ev) {
                var mark = ev.target && ev.target.closest
                  ? ev.target.closest('#ynow_header_logo_home, .ynow-header-logo-mark')
                  : null;
                if (!mark) return;
                // Currency controls sit under the same <li>; only the mark goes home.
                if (ev.target && ev.target.closest && ev.target.closest('.ynow-ccy-float')) return;
                ev.preventDefault();
                gotoTab('home');
              });
              document.addEventListener('keydown', function (ev) {
                var t = ev.target;
                if (!t || t.id !== 'ynow_header_logo_home') return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                gotoTab('home');
              });
            }
            bindHeaderLogoHome();
            function remapLegacyHash() {
              var h = String(location.hash || '');
              if (h === '#shiny-tab-business_breakdown_lab') {
                gotoTab('company_advance');
                try { history.replaceState(null, '', '#shiny-tab-company_advance'); } catch (eH) {}
              }
            }
            function registerGotoTabHandler() {
              if (!window.Shiny || !Shiny.addCustomMessageHandler) {
                setTimeout(registerGotoTabHandler, 50);
                return;
              }
              Shiny.addCustomMessageHandler('ynowGotoTab', function (payload) {
                var tab = payload && payload.tab;
                if (tab) gotoTab(tab);
              });
            }
            registerGotoTabHandler();
            window.addEventListener('hashchange', remapLegacyHash);
            function applyLiteMode(on, opts) {
              opts = opts || {};
              var enabled = !!on;
              document.body.classList.toggle('ynow-lite', enabled);
              document.querySelectorAll('.ynow-lite-toggle').forEach(function (brand) {
                brand.setAttribute('aria-pressed', enabled ? 'true' : 'false');
              });
              try {
                window.sessionStorage.setItem('ynow_lite_mode', enabled ? '1' : '0');
              } catch (e0) {}
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ynow_lite_mode', enabled, {priority: 'event'});
              }
              if (enabled) ensureLiteSnapshotDefaultsTab();
              remapLegacyHash();
              if (opts.navigate === false) return;
              var tab = currentSidebarTab();
              if (enabled) {
                if (FULL_ONLY_TABS.indexOf(tab) >= 0) gotoTab('smart_analysis');
              } else if (tab === 'smart_analysis') {
                gotoTab('dashboard');
              }
            }
            function ensureLiteSnapshotDefaultsTab() {
              if (!document.body.classList.contains('ynow-lite')) return;
              var a = document.querySelector('#snapshot_report a[data-value=\"snap_defaults\"]');
              if (!a) return;
              try {
                if (window.jQuery) window.jQuery(a).tab('show');
                else if (typeof a.click === 'function') a.click();
              } catch (eSnap) {}
            }
            window.ensureLiteSnapshotDefaultsTab = ensureLiteSnapshotDefaultsTab;
            function toggleLite() {
              applyLiteMode(!document.body.classList.contains('ynow-lite'));
            }
            function bindLiteToggle() {
              if (document.documentElement.getAttribute('data-ynow-lite-delegated') === '1') return;
              document.documentElement.setAttribute('data-ynow-lite-delegated', '1');
              document.addEventListener('click', function (ev) {
                var brand = ev.target && ev.target.closest ? ev.target.closest('.ynow-lite-toggle') : null;
                if (!brand) return;
                ev.preventDefault();
                ev.stopPropagation();
                toggleLite();
              }, true);
              document.addEventListener('keydown', function (ev) {
                var brand = ev.target && ev.target.closest ? ev.target.closest('.ynow-lite-toggle') : null;
                if (!brand) return;
                if (ev.key === 'Enter' || ev.key === ' ' || ev.keyCode === 13 || ev.keyCode === 32) {
                  ev.preventDefault();
                  toggleLite();
                }
              }, true);
            }
            function restoreLiteFromStorage() {
              var saved = null;
              try { saved = window.sessionStorage.getItem('ynow_lite_mode'); } catch (e1) {}
              applyLiteMode(saved === '1', {navigate: false});
              remapLegacyHash();
            }
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', function () {
                bindLiteToggle();
                restoreLiteFromStorage();
              });
            } else {
              bindLiteToggle();
              restoreLiteFromStorage();
            }
            if (window.jQuery) {
              jQuery(document).on('shiny:connected shiny:sessioninitialized', function () {
                bindLiteToggle();
                restoreLiteFromStorage();
              });
            }
            window.__ynowApplyLiteMode = applyLiteMode;
          })();

          /* ---- Param audit (1A+2B): go to tab + highlight input ---- */
          function ynowClearParamHighlight() {
            document.querySelectorAll('.ynow-param-highlight').forEach(function (el) {
              el.classList.remove('ynow-param-highlight');
            });
          }
          function ynowFindInputEl(inputId) {
            if (!inputId) return null;
            var id = String(inputId);
            var el = document.getElementById(id);
            if (el) return el;
            el = document.querySelector('[id=\"' + id.replace(/\"/g, '') + '\"]');
            if (el) return el;
            var named = document.querySelectorAll('[name=\"' + id.replace(/\"/g, '') + '\"]');
            if (named && named.length) {
              return named[0].closest('.form-group, .shiny-input-container, .radio, .checkbox') || named[0];
            }
            return null;
          }
          function ynowGotoAndHighlight(tab, inputId) {
            ynowClearParamHighlight();
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            if (tab && window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
            }
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('a[data-value=\"' + tab + '\"]');
            if (a) {
              try { a.click(); } catch (e) {}
            }
            setTimeout(function () {
              var el = ynowFindInputEl(inputId);
              if (!el) return;
              var box = el.closest('.form-group, .shiny-input-container, .box-body > div, .radio, .checkbox') || el;
              box.classList.add('ynow-param-highlight');
              try { box.scrollIntoView({behavior: 'smooth', block: 'center'}); } catch (e2) {
                try { box.scrollIntoView(true); } catch (e3) {}
              }
              setTimeout(function () { box.classList.remove('ynow-param-highlight'); }, 4500);
            }, 350);
          }
          document.addEventListener('click', function (ev) {
            var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-param-goto') : null;
            if (!btn) return;
            ev.preventDefault();
            ynowGotoAndHighlight(btn.getAttribute('data-tab'), btn.getAttribute('data-input-id'));
          });

          /* ---- Param audit 2A: annotated page screenshots → PDF ---- */
          function ynowLoadScriptOnce(src) {
            return new Promise(function (resolve, reject) {
              if (document.querySelector('script[data-ynow-src=\"' + src + '\"]')) {
                resolve();
                return;
              }
              var s = document.createElement('script');
              s.src = src;
              s.async = true;
              s.setAttribute('data-ynow-src', src);
              s.onload = function () { resolve(); };
              s.onerror = function () { reject(new Error('load fail: ' + src)); };
              document.head.appendChild(s);
            });
          }
          function ynowEnsurePdfLibs() {
            var h2c = 'https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js';
            var jsp = 'https://cdn.jsdelivr.net/npm/jspdf@2.5.2/dist/jspdf.umd.min.js';
            return ynowLoadScriptOnce(h2c).then(function () { return ynowLoadScriptOnce(jsp); });
          }
          function ynowSleep(ms) {
            return new Promise(function (r) { setTimeout(r, ms); });
          }
          function ynowActivateTab(tab) {
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
            }
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]') ||
                    document.querySelector('a[data-value=\"' + tab + '\"]');
            if (a) {
              try { a.click(); } catch (e) {}
            }
          }
          function ynowTabPane(tab) {
            return document.getElementById('shiny-tab-' + tab) ||
                   document.querySelector('.tab-pane[data-value=\"' + tab + '\"]') ||
                   document.querySelector('.tab-content > .tab-pane.active');
          }
          function ynowFindInputBox(inputId) {
            var el = ynowFindInputEl(inputId);
            if (!el) return null;
            return el.closest('.form-group, .shiny-input-container, .radio, .checkbox, .box') || el;
          }
          function ynowAnnotateCanvas(canvas, pane, changes) {
            if (!canvas || !pane || !changes || !changes.length) return canvas;
            var ctx = canvas.getContext('2d');
            var paneRect = pane.getBoundingClientRect();
            var sx = canvas.width / Math.max(pane.scrollWidth || pane.clientWidth || 1, 1);
            var sy = canvas.height / Math.max(pane.scrollHeight || pane.clientHeight || 1, 1);
            changes.forEach(function (ch) {
              var box = ynowFindInputBox(ch.input_id);
              if (!box) return;
              var r = box.getBoundingClientRect();
              var x = (r.left - paneRect.left + pane.scrollLeft) * sx;
              var y = (r.top - paneRect.top + pane.scrollTop) * sy;
              var w = Math.max(r.width * sx, 8);
              var h = Math.max(r.height * sy, 8);
              ctx.save();
              ctx.strokeStyle = '#f39c12';
              ctx.lineWidth = Math.max(2, 3 * sx);
              ctx.strokeRect(x - 4 * sx, y - 4 * sy, w + 8 * sx, h + 8 * sy);
              var label = (ch.label || ch.input_id) + ': ' + (ch.baseline || '—') + ' → ' + (ch.current || '—');
              ctx.font = Math.max(11, Math.round(12 * sx)) + 'px sans-serif';
              ctx.fillStyle = 'rgba(243,156,18,0.92)';
              var tw = ctx.measureText(label).width + 10;
              var ty = Math.max(0, y - 18 * sy);
              ctx.fillRect(x - 4 * sx, ty, tw, 16 * sy);
              ctx.fillStyle = '#111';
              ctx.fillText(label, x, ty + 12 * sy);
              ctx.restore();
            });
            return canvas;
          }
          function ynowAddCanvasToPdf(pdf, canvas, title) {
            var pageW = pdf.internal.pageSize.getWidth();
            var pageH = pdf.internal.pageSize.getHeight();
            var margin = 28;
            var maxW = pageW - margin * 2;
            var maxH = pageH - margin * 2 - 24;
            var imgW = maxW;
            var imgH = canvas.height * (imgW / canvas.width);
            if (imgH > maxH) {
              imgH = maxH;
              imgW = canvas.width * (imgH / canvas.height);
            }
            pdf.setFontSize(11);
            pdf.setTextColor(40);
            if (title) pdf.text(String(title), margin, margin - 8);
            var data = canvas.toDataURL('image/jpeg', 0.92);
            pdf.addImage(data, 'JPEG', margin, margin + 4, imgW, imgH);
          }
          async function ynowRunParamAuditPdf(payload) {
            var status = document.getElementById('ynow_param_audit_pdf_status');
            var setStatus = function (t) { if (status) status.textContent = t || ''; };
            var strings = (payload && payload.strings) || {};
            try {
              setStatus(strings.busy || 'Capturing…');
              await ynowEnsurePdfLibs();
              var html2canvasFn = window.html2canvas;
              var jsPDF = (window.jspdf && window.jspdf.jsPDF) ? window.jspdf.jsPDF : window.jsPDF;
              if (!html2canvasFn || !jsPDF) throw new Error('libs missing');
              var tabs = (payload && payload.tabs) || [];
              var changes = (payload && payload.changes) || [];
              var pageTitles = (payload && payload.page_titles) || {};
              if (!tabs.length) {
                setStatus(strings.need_pages || 'Select pages');
                return;
              }
              var pdf = new jsPDF({ orientation: 'portrait', unit: 'pt', format: 'a4' });
              var margin = 40;
              pdf.setFontSize(16);
              pdf.text(payload.title || 'Parameter adjustments PDF', margin, 56);
              pdf.setFontSize(11);
              pdf.text((payload.ticker || '') + '  ·  ' + (payload.baseline_at || ''), margin, 78);
              pdf.setFontSize(10);
              var y = 100;
              pdf.text(strings.summary || 'Adjusted parameters:', margin, y);
              y += 16;
              if (!changes.length) {
                pdf.text(strings.no_changes || '(No manual adjustments vs baseline)', margin, y);
              } else {
                changes.forEach(function (ch) {
                  var line = (ch.section || '') + ' / ' + (ch.label || ch.input_id) +
                    ': ' + (ch.baseline || '—') + ' → ' + (ch.current || '—');
                  var lines = pdf.splitTextToSize(line, pdf.internal.pageSize.getWidth() - margin * 2);
                  if (y + lines.length * 12 > pdf.internal.pageSize.getHeight() - 40) {
                    pdf.addPage();
                    y = 48;
                  }
                  pdf.text(lines, margin, y);
                  y += lines.length * 12 + 4;
                });
              }
              for (var i = 0; i < tabs.length; i++) {
                var tab = tabs[i];
                setStatus((strings.busy || 'Capturing…') + ' (' + (i + 1) + '/' + tabs.length + ')');
                ynowActivateTab(tab);
                await ynowSleep(650);
                var pane = ynowTabPane(tab);
                if (!pane) continue;
                try { pane.scrollTop = 0; } catch (e0) {}
                var canvas = await html2canvasFn(pane, {
                  scale: 1,
                  useCORS: true,
                  logging: false,
                  backgroundColor: '#ffffff',
                  windowWidth: Math.max(pane.scrollWidth, pane.clientWidth || 800),
                  height: Math.min(pane.scrollHeight || pane.clientHeight || 800, 5000)
                });
                var tabChanges = changes.filter(function (c) { return c.tab === tab; });
                ynowAnnotateCanvas(canvas, pane, tabChanges);
                pdf.addPage();
                ynowAddCanvasToPdf(pdf, canvas, pageTitles[tab] || tab);
              }
              ynowActivateTab('snapshot');
              await ynowSleep(200);
              var fname = (payload.filename || 'YNow_param_audit') + '.pdf';
              pdf.save(fname);
              setStatus(strings.done || 'PDF downloaded.');
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('param_audit_pdf_done', Date.now(), {priority: 'event'});
              }
            } catch (err) {
              console.error(err);
              setStatus(strings.err || 'PDF capture failed.');
              try { ynowActivateTab('snapshot'); } catch (e2) {}
            }
          }
          function registerParamAuditPdfHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerParamAuditPdfHandler, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowParamAuditPdf', function (payload) {
              ynowRunParamAuditPdf(payload || {});
            });
          }
          registerParamAuditPdfHandler();
        })();
      ")),
      
      tags$style(HTML("
        .selectize-dropdown-content {
          max-height: 300px !important;
          overflow-y: auto !important;
        }
        .selectize-dropdown {
          max-height: 300px !important;
        }

        /* Param audit highlight pulse */
        .ynow-param-highlight {
          outline: 3px solid #f39c12 !important;
          outline-offset: 3px;
          box-shadow: 0 0 0 4px rgba(243, 156, 18, 0.35) !important;
          border-radius: 4px;
          transition: box-shadow 0.2s ease;
        }

        /* Ticker typeahead suggest (Ticker/Stock Code + Radar focus): 黑字白底 */
        .ynow-ticker-suggest,
        #sc_ticker_suggest,
        #lab_cluster_focus_suggest {
          position: absolute;
          z-index: 2000;
          left: 0;
          right: 0;
          top: 100%;
          margin-top: 2px;
          max-height: 260px;
          overflow-y: auto;
          background: #ffffff;
          border: 1px solid #cccccc;
          border-radius: 4px;
          box-shadow: 0 4px 10px rgba(0,0,0,0.12);
          display: none;
        }
        .ynow-ticker-suggest .ynow-suggest-item,
        #sc_ticker_suggest .ynow-suggest-item,
        #lab_cluster_focus_suggest .ynow-suggest-item {
          display: block;
          width: 100%;
          padding: 8px 12px;
          color: #000000 !important;
          background: #ffffff;
          border: 0;
          border-bottom: 1px solid #eeeeee;
          text-align: left;
          font-size: 13px;
          cursor: pointer;
        }
        .ynow-ticker-suggest .ynow-suggest-item:hover,
        .ynow-ticker-suggest .ynow-suggest-item:focus,
        #sc_ticker_suggest .ynow-suggest-item:hover,
        #sc_ticker_suggest .ynow-suggest-item:focus,
        #lab_cluster_focus_suggest .ynow-suggest-item:hover,
        #lab_cluster_focus_suggest .ynow-suggest-item:focus {
          background: #f2f2f2;
          color: #000000 !important;
          outline: none;
        }
        .ynow-ticker-suggest .ynow-suggest-sym,
        #sc_ticker_suggest .ynow-suggest-sym,
        #lab_cluster_focus_suggest .ynow-suggest-sym {
          font-weight: 700;
          color: #000000;
          margin-right: 8px;
        }
        .ynow-ticker-suggest .ynow-suggest-lab,
        #sc_ticker_suggest .ynow-suggest-lab,
        #lab_cluster_focus_suggest .ynow-suggest-lab {
          color: #222222;
          font-weight: 400;
        }
        .ynow-sc-row {
          display: flex;
          flex-direction: row;
          flex-wrap: nowrap;
          align-items: flex-end;
          gap: 10px;
          max-width: 420px;
          margin-bottom: 8px;
        }
        .ynow-sc-wrap {
          position: relative;
          flex: 1 1 auto;
          width: 260px;
          max-width: 280px;
          min-width: 0;
        }
        .ynow-sc-wrap .form-group {
          margin-bottom: 0 !important;
        }
        .ynow-sc-wrap .control-label {
          display: block;
          margin-bottom: 5px;
        }
        .ynow-sc-row #search.ynow-search-btn,
        #search.ynow-search-btn {
          flex: 0 0 auto;
          align-self: flex-end;
          height: 34px;
          padding: 6px 14px;
          margin: 0 0 0 0;
          background: #111111 !important;
          border: 1px solid #000000 !important;
          color: #F5C518 !important;
          font-weight: 700;
          line-height: 1.2;
          box-shadow: none;
        }
        .ynow-sc-row #search.ynow-search-btn:hover,
        .ynow-sc-row #search.ynow-search-btn:focus,
        #search.ynow-search-btn:hover,
        #search.ynow-search-btn:focus {
          background: #000000 !important;
          border-color: #000000 !important;
          color: #C9A227 !important;
        }
        .ynow-sc-row #search.ynow-search-btn .fa,
        .ynow-sc-row #search.ynow-search-btn .fas,
        #search.ynow-search-btn .fa,
        #search.ynow-search-btn .fas {
          color: #F5C518 !important;
        }
        @media (max-width: 480px) {
          .ynow-sc-row {
            max-width: 100%;
          }
          .ynow-sc-wrap {
            width: auto;
            max-width: calc(100% - 100px);
          }
        }
        
        /* 大數字框（valueBox / infoBox）統一響應式：換行、縮字、避免溢出
           （還原 v16.42：手機勿再壓成 33% 三欄；不用 [class*=…] 屬性選擇器） */
        .content-wrapper .row > .col-xs-12,
        .content-wrapper .row > .col-sm-2,
        .content-wrapper .row > .col-sm-3,
        .content-wrapper .row > .col-sm-4,
        .content-wrapper .row > .col-sm-6,
        .content-wrapper .row > .col-sm-12 {
          min-width: 0;
        }
        .content-wrapper .small-box,
        .content-wrapper .info-box,
        .tab-content .small-box,
        .tab-content .info-box {
          min-width: 0;
          max-width: 100%;
          width: 100%;
          box-sizing: border-box;
          overflow: hidden;
        }
        /* Same-row KPI boxes stretch to equal height (valueBoxOutput nests col-sm-* on uiOutput).
           Attr selectors use single quotes because this CSS block is inside a double-quoted HTML string. */
        .content-wrapper .row:has(> [class*='col-'] .small-box),
        .tab-content .row:has(> [class*='col-'] .small-box),
        .tab-pane .row:has(> [class*='col-'] .small-box) {
          display: flex;
          flex-wrap: wrap;
          align-items: stretch;
        }
        .content-wrapper .row:has(> [class*='col-'] .small-box) > [class*='col-'],
        .tab-content .row:has(> [class*='col-'] .small-box) > [class*='col-'],
        .tab-pane .row:has(> [class*='col-'] .small-box) > [class*='col-'] {
          display: flex;
          flex-direction: column;
          float: none;
        }
        .content-wrapper .row:has(> [class*='col-'] .small-box) > [class*='col-'] > .shiny-html-output,
        .tab-content .row:has(> [class*='col-'] .small-box) > [class*='col-'] > .shiny-html-output,
        .tab-pane .row:has(> [class*='col-'] .small-box) > [class*='col-'] > .shiny-html-output,
        .content-wrapper .row:has(> [class*='col-'] .small-box) > [class*='col-'] > [class*='col-'],
        .tab-content .row:has(> [class*='col-'] .small-box) > [class*='col-'] > [class*='col-'],
        .tab-pane .row:has(> [class*='col-'] .small-box) > [class*='col-'] > [class*='col-'] {
          flex: 1 1 auto;
          width: 100%;
          max-width: 100%;
          height: 100%;
          display: flex;
          flex-direction: column;
          float: none;
          padding-left: 0;
          padding-right: 0;
        }
        .content-wrapper .row:has(> [class*='col-'] .small-box) .small-box,
        .tab-content .row:has(> [class*='col-'] .small-box) .small-box,
        .tab-pane .row:has(> [class*='col-'] .small-box) .small-box {
          flex: 1 1 auto;
          width: 100%;
          height: 100%;
        }
        .content-wrapper .small-box .inner {
          min-width: 0;
          padding-right: 44px;
          flex: 1 1 auto;
        }
        .content-wrapper .small-box .inner h3,
        .content-wrapper .small-box .inner h3 * {
          font-size: clamp(13px, 1.9vw + 0.35rem, 34px) !important;
          font-weight: 800;
          line-height: 1.15 !important;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.02em;
          /* Keep the whole figure on one line — never orphan a trailing digit. */
          white-space: nowrap !important;
          overflow-wrap: normal;
          word-break: keep-all;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 100%;
        }
        .content-wrapper .small-box .inner p {
          white-space: normal;
          overflow-wrap: anywhere;
          line-height: 1.25;
        }
        .content-wrapper .info-box-content {
          min-width: 0;
          overflow: hidden;
        }
        .content-wrapper .info-box .info-box-number,
        .content-wrapper .info-box .info-box-number h3 {
          font-size: clamp(13px, 1.8vw + 0.3rem, 28px) !important;
          font-weight: 700;
          line-height: 1.15 !important;
          font-variant-numeric: tabular-nums;
          white-space: nowrap !important;
          overflow-wrap: normal;
          word-break: keep-all;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 100%;
        }
        .content-wrapper .info-box .info-box-text {
          white-space: normal;
          overflow-wrap: anywhere;
        }
        /* 平板：較窄欄的彩色 BOX 兩欄並排 */
        @media (max-width: 991px) {
          .content-wrapper .row > .col-sm-2:has(.small-box),
          .content-wrapper .row > .col-sm-3:has(.small-box),
          .content-wrapper .row > .col-sm-4:has(.small-box),
          .content-wrapper .row > .col-sm-2:has(.info-box),
          .content-wrapper .row > .col-sm-3:has(.info-box),
          .content-wrapper .row > .col-sm-4:has(.info-box),
          .tab-content .row > .col-sm-2:has(.small-box),
          .tab-content .row > .col-sm-3:has(.small-box),
          .tab-content .row > .col-sm-4:has(.small-box),
          .tab-content .row > .col-sm-2:has(.info-box),
          .tab-content .row > .col-sm-3:has(.info-box),
          .tab-content .row > .col-sm-4:has(.info-box) {
            width: 50% !important;
            float: left !important;
            clear: none;
          }
        }
        /* 手機：頁籤／內容區彩色 BOX 直向全寬（勿再壓成 33% 三欄） */
        @media (max-width: 767px) {
          .content-wrapper .row > .col-xs-12:has(.small-box),
          .content-wrapper .row > .col-sm-2:has(.small-box),
          .content-wrapper .row > .col-sm-3:has(.small-box),
          .content-wrapper .row > .col-sm-4:has(.small-box),
          .content-wrapper .row > .col-sm-6:has(.small-box),
          .content-wrapper .row > .col-sm-12:has(.small-box),
          .content-wrapper .row > .col-xs-12:has(.info-box),
          .content-wrapper .row > .col-sm-2:has(.info-box),
          .content-wrapper .row > .col-sm-3:has(.info-box),
          .content-wrapper .row > .col-sm-4:has(.info-box),
          .content-wrapper .row > .col-sm-6:has(.info-box),
          .content-wrapper .row > .col-sm-12:has(.info-box),
          .tab-content .row > .col-xs-12:has(.small-box),
          .tab-content .row > .col-sm-2:has(.small-box),
          .tab-content .row > .col-sm-3:has(.small-box),
          .tab-content .row > .col-sm-4:has(.small-box),
          .tab-content .row > .col-sm-6:has(.small-box),
          .tab-content .row > .col-sm-12:has(.small-box),
          .tab-content .row > .col-xs-12:has(.info-box),
          .tab-content .row > .col-sm-2:has(.info-box),
          .tab-content .row > .col-sm-3:has(.info-box),
          .tab-content .row > .col-sm-4:has(.info-box),
          .tab-content .row > .col-sm-6:has(.info-box),
          .tab-content .row > .col-sm-12:has(.info-box),
          .tab-pane .row > .col-xs-12:has(.small-box),
          .tab-pane .row > .col-sm-2:has(.small-box),
          .tab-pane .row > .col-sm-3:has(.small-box),
          .tab-pane .row > .col-sm-4:has(.small-box),
          .tab-pane .row > .col-sm-6:has(.small-box),
          .tab-pane .row > .col-sm-12:has(.small-box),
          .tab-pane .row > .col-xs-12:has(.info-box),
          .tab-pane .row > .col-sm-2:has(.info-box),
          .tab-pane .row > .col-sm-3:has(.info-box),
          .tab-pane .row > .col-sm-4:has(.info-box),
          .tab-pane .row > .col-sm-6:has(.info-box),
          .tab-pane .row > .col-sm-12:has(.info-box) {
            width: 100% !important;
            max-width: 100% !important;
            float: none !important;
            display: block !important;
            clear: both !important;
            margin-left: 0 !important;
            margin-right: 0 !important;
          }
          .content-wrapper .small-box,
          .content-wrapper .info-box,
          .tab-content .small-box,
          .tab-content .info-box {
            margin-bottom: 10px !important;
          }
          .content-wrapper .small-box .inner {
            padding-right: 12px;
            padding-left: 12px;
          }
          .content-wrapper .small-box .icon {
            display: none;
          }
          .content-wrapper .small-box .inner h3,
          .content-wrapper .small-box .inner h3 * {
            font-size: clamp(18px, 5.5vw, 26px) !important;
          }
          .content-wrapper .small-box .inner p {
            font-size: clamp(11px, 3.2vw, 13px) !important;
          }
          .content-wrapper .info-box {
            min-height: 0;
            height: auto !important;
          }
          .content-wrapper .info-box .info-box-icon {
            width: 48px;
            height: 48px;
            font-size: 20px;
            line-height: 48px;
          }
          .content-wrapper .info-box .info-box-content {
            margin-left: 48px;
            padding: 6px 10px 6px 12px;
          }
          .content-wrapper .info-box .info-box-number,
          .content-wrapper .info-box .info-box-number h3 {
            font-size: clamp(16px, 4.8vw, 22px) !important;
          }
          .content-wrapper .info-box .info-box-text {
            font-size: clamp(11px, 3vw, 13px) !important;
          }
        }

        /* Finance Summary 卡片網格 */
        .ynow-fs-wrap {
          margin-bottom: 14px;
        }
        .ynow-fs-section {
          margin-bottom: 16px;
        }
        .ynow-fs-section-title {
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #666666;
          margin: 0 0 8px 0;
          padding-bottom: 4px;
          border-bottom: 1px solid #e5e5e5;
        }
        .ynow-fs-grid {
          display: grid;
          grid-template-columns: repeat(5, minmax(0, 1fr));
          gap: 10px;
        }
        @media (max-width: 992px) {
          .ynow-fs-grid { grid-template-columns: repeat(3, minmax(0, 1fr)); }
        }
        /* 手機：兩兩並排（FINANCIAL REPORT Finance Summary 卡片） */
        @media (max-width: 767px) {
          .ynow-fs-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 8px; }
        }
        .ynow-fs-card {
          background: linear-gradient(165deg, #fafafa 0%, #f0f0f0 100%);
          border: 1px solid #e0e0e0;
          border-left: 3px solid #222222;
          border-radius: 4px;
          padding: 10px 12px;
          min-height: 64px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          transition: border-color 0.15s ease, background 0.15s ease;
        }
        .ynow-fs-card:hover {
          background: #ffffff;
          border-left-color: #555555;
        }
        .ynow-fs-label {
          font-size: 11px;
          font-weight: 600;
          color: #777777;
          line-height: 1.25;
          margin-bottom: 4px;
        }
        .ynow-fs-value {
          font-size: clamp(12px, 2.4vw, 15px);
          font-weight: 700;
          color: #111111;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.01em;
          line-height: 1.2;
          word-break: break-word;
          overflow-wrap: anywhere;
        }
        /* 財報屬性「重視指標」點：與 APP 標題金色一致（非黃土琥珀） */
        .ynow-focus-metric-dot {
          display: inline-block;
          width: 8px;
          height: 8px;
          margin-left: 6px;
          border-radius: 50%;
          background: var(--ynow-gold, #F5C518);
          vertical-align: middle;
          box-shadow: 0 0 0 1px rgba(0,0,0,0.08);
        }
        .ynow-focus-metric-dot--legend {
          margin: 0 6px 0 0;
        }
        .ynow-kpi-legend-wrap {
          margin: 0 0 10px 0;
        }
        .ynow-focus-metric-note {
          margin: 6px 2px 0 2px;
          padding: 0;
          border: none;
          background: transparent;
          font-size: 12px;
          line-height: 1.35;
          color: #666666;
          font-weight: 500;
        }
        /* Yahoo Sector/Industry 列：屬性標籤靠右 */
        .ynow-ind-yahoo-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 10px 14px;
          margin-top: 4px;
          font-size: 12px;
          color: #777;
          line-height: 1.45;
          flex-wrap: wrap;
        }
        .ynow-ind-yahoo-text {
          min-width: 0;
          flex: 1 1 auto;
        }

        /* 財報屬性標籤：黑底＋標題同款金字金框，無圓點（High growth 等） */
        .ynow-fund-profile-badge {
          display: inline-flex;
          align-items: center;
          gap: 0;
          margin-left: auto;
          padding: 3px 10px;
          border-radius: 4px;
          border: 1px solid var(--ynow-gold, #F5C518) !important;
          background: #000000 !important;
          color: var(--ynow-gold, #F5C518) !important;
          font-size: 12px;
          font-weight: 700;
          white-space: nowrap;
          line-height: 1.2;
        }
        /* Data-limited / 資料受限: keep logo-flow fill (not HTCDI flame). */
        .ynow-fund-profile-badge .ynow-htcdi-flow {
          color: transparent !important;
          -webkit-text-fill-color: transparent !important;
        }
        .ynow-fund-profile-badge .ynow-focus-metric-dot {
          display: none !important;
        }
        @media (max-width: 767px) {
          .ynow-ind-yahoo-row {
            align-items: flex-start;
          }
          .ynow-fund-profile-badge {
            margin-left: 0;
          }
        }
        .content-wrapper .small-box .inner h3 .ynow-focus-metric-dot {
          margin-left: 8px;
          vertical-align: middle;
        }

        /* Snapshot / HFV 摘要數字：維持可覆寫 class，尺寸回預設 */
        .ynow-kpi-stat-label {
          font-size: 11px !important;
          color: #666 !important;
          line-height: 1.25 !important;
          margin: 0 !important;
        }
        .ynow-kpi-stat-value {
          font-size: clamp(14px, 3.2vw, 18px) !important;
          font-weight: 700 !important;
          line-height: 1.2 !important;
          margin: 0 !important;
          font-variant-numeric: tabular-nums;
          overflow-wrap: anywhere;
        }
        .ynow-kpi-stat-note {
          font-size: 10px !important;
          color: #888 !important;
          margin-top: 2px !important;
          line-height: 1.25 !important;
        }
        .ynow-kpi-stat-params {
          font-size: 12px !important;
          line-height: 1.4 !important;
        }
        /* HFV：此刻參數 — 圖表正下方、全寬、鍵值網格（勿擠成一行） */
        .ynow-hfv-session-params {
          width: 100%;
          box-sizing: border-box;
          margin: 14px 0 0 0;
          padding: 12px 14px 14px 14px;
          background: #fafafa;
          border: 1px solid #e6e6e6;
          border-left: 4px solid var(--ynow-gold-deep, #C9A227);
          border-radius: 4px;
        }
        .ynow-hfv-session-params__title {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0 0 10px 0;
          font-size: 13px;
          font-weight: 700;
          color: var(--ynow-gold-ink, #856404);
          letter-spacing: 0.01em;
        }
        .ynow-hfv-session-params__title > .fa,
        .ynow-hfv-session-params__title > .fas {
          color: var(--ynow-gold-deep, #C9A227);
          width: 1.15em;
          text-align: center;
        }
        .ynow-hfv-session-params__grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(148px, 1fr));
          gap: 8px 12px;
        }
        .ynow-hfv-session-params__item {
          min-width: 0;
          padding: 7px 10px;
          background: #fff;
          border: 1px solid #ececec;
          border-radius: 4px;
        }
        .ynow-hfv-session-params__key {
          display: block;
          font-size: 10.5px;
          font-weight: 600;
          color: #777;
          letter-spacing: 0.02em;
          margin: 0 0 3px 0;
          line-height: 1.25;
        }
        .ynow-hfv-session-params__val {
          display: block;
          font-size: 13px;
          font-weight: 600;
          color: #1a1a1a;
          font-variant-numeric: tabular-nums;
          line-height: 1.35;
          word-break: break-word;
          overflow-wrap: anywhere;
        }
        .ynow-hfv-session-params__span {
          grid-column: 1 / -1;
        }
        @media (max-width: 767px) {
          .ynow-hfv-session-params {
            margin-top: 10px;
            padding: 10px 10px 12px 10px;
          }
          .ynow-hfv-session-params__grid {
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 6px 8px;
          }
          .ynow-hfv-session-params__val {
            font-size: 12px;
          }
        }
        .ynow-kpi-hero-value {
          font-size: clamp(16px, 4vw, 22px) !important;
          font-weight: 700 !important;
          line-height: 1.2 !important;
          overflow-wrap: anywhere;
        }

        /* KPI：φ⁻¹ 等比例縮小 + 一列五個左排 */
        .ynow-kpi-grid {
          display: flex;
          flex-wrap: wrap;
          justify-content: flex-start;
          align-items: stretch;
          margin-left: -4px;
          margin-right: -4px;
          clear: both;
        }
        .ynow-kpi-grid > * {
          width: 20% !important;
          max-width: 20% !important;
          flex: 0 0 20%;
          float: none !important;
          padding-left: 4px;
          padding-right: 4px;
          box-sizing: border-box;
        }
        @media (max-width: 992px) {
          .ynow-kpi-grid > * {
            width: 33.333% !important;
            max-width: 33.333% !important;
            flex-basis: 33.333%;
          }
        }
        @media (max-width: 767px) {
          /* FINANCIAL REPORT／Dashboard：KPI 框格兩兩並排（勿三欄過窄） */
          .ynow-kpi-grid > * {
            width: 50% !important;
            max-width: 50% !important;
            flex-basis: 50%;
          }
          .ynow-kpi-grid .small-box .inner h3 {
            font-size: clamp(11px, 3.2vw, 16px) !important;
          }
          .ynow-kpi-grid .small-box .icon-large {
            display: none !important;
          }
        }
        .ynow-kpi-grid .small-box {
          aspect-ratio: 1.618 / 1 !important;
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
          align-items: stretch !important;
          float: none !important;
          width: 100% !important;
          min-height: 74px !important;
          height: auto !important;
          border-radius: 5px !important;
          margin-bottom: 9px !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05) !important;
        }
        /* N/A / 未設產業區間：白（KPI 僅黑白紅藍） */
        .ynow-kpi-grid .small-box.ynow-kpi-na {
          background-color: #fff !important;
          color: #444 !important;
          border: 1px solid #ddd;
        }
        .ynow-kpi-grid .small-box.ynow-kpi-na .icon-large {
          color: rgba(0, 0, 0, 0.12);
        }
        /* Value + label: true vertical & horizontal center.
           Override global .small-box .inner { padding-right:44px; flex:1 } which
           left-shifts numerals for the decorative icon and pins content to the top. */
        .ynow-kpi-grid .small-box .inner {
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
          align-items: center !important;
          flex: 1 1 auto !important;
          width: 100% !important;
          height: 100% !important;
          box-sizing: border-box !important;
          padding: 6px 9px !important;
          padding-right: 9px !important;
          text-align: center !important;
        }
        .ynow-kpi-grid .small-box .inner h3 {
          font-size: clamp(14px, 2.6vw, 23px) !important;
          font-weight: 800 !important;
          margin: 0 0 5px 0 !important;
          white-space: normal !important;
          overflow-wrap: anywhere;
          text-align: center !important;
          width: 100% !important;
          max-width: 100% !important;
        }
        .ynow-kpi-grid .small-box .inner p {
          font-size: clamp(10px, 0.75vw, 11px) !important;
          opacity: 0.9;
          font-weight: 500 !important;
          margin: 0 !important;
          line-height: 1.2 !important;
          text-align: center !important;
          width: 100% !important;
        }
        .ynow-kpi-grid .small-box .icon-large {
          font-size: 37px !important;
          top: 9px !important;
          right: 9px !important;
          opacity: 0.12 !important;
        }
        .ynow-kpi-section-title {
          font-size: 13px;
          font-weight: 700;
          color: #333;
          margin: 12px 0 6px 0;
        }
        /* Annotation 頁籤 */
        .ynow-ann-legend {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
          margin: 0 0 14px 0;
        }
        .ynow-ann-chip {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          padding: 8px 12px;
          border-radius: 6px;
          background: #fafafa;
          border: 1px solid #e5e5e5;
          font-size: 13px;
          color: #333;
          min-width: 168px;
        }
        /* 產業標準六格：手機兩欄；≥768px 六欄均分。色碼圖例：手機兩欄／桌面四欄 */
        .ynow-ind-overview-metrics {
          width: 100%;
          margin-top: 10px;
        }
        .ynow-ind-snapshot-chips,
        .ynow-kpi-legend-chips {
          display: grid !important;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 10px;
          align-items: stretch;
        }
        .ynow-ind-snapshot-chip,
        .ynow-kpi-legend-chip {
          min-width: 0 !important;
          width: 100%;
          box-sizing: border-box;
        }
        .ynow-ind-snapshot-chip {
          flex-direction: column;
          align-items: flex-start;
          gap: 2px;
        }
        @media (min-width: 768px) {
          .ynow-ind-snapshot-chips {
            grid-template-columns: repeat(6, minmax(0, 1fr));
          }
          .ynow-kpi-legend-chips {
            grid-template-columns: repeat(4, minmax(0, 1fr));
          }
        }
        .ynow-ann-swatch {
          width: 14px;
          height: 14px;
          border-radius: 50%;
          flex: 0 0 14px;
          box-shadow: inset 0 0 0 1px rgba(0,0,0,0.08);
        }
        .ynow-ann-note {
          margin: 0 0 16px 0;
          padding: 10px 12px;
          background: #f5f5f5;
          border-left: 4px solid #222222;
          border-radius: 4px;
          font-size: 12.5px;
          color: #555;
          line-height: 1.5;
        }
        .ynow-ann-h {
          margin: 18px 0 8px 0;
          font-size: 14px;
          font-weight: 700;
          color: #222222;
        }

        /* Blue Chip：美股績優篩選（對齊 Dashboard FINANCIAL REPORT：tabBox + 查詢條件／內容 box） */
        .ynow-lab-im-universe-row {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 10px;
          margin: 0 0 14px 0;
          font-size: 12.5px;
          color: #556;
        }
        .ynow-lab-im-universe-row .ynow-lab-im-universe-meta {
          flex: 1 1 auto;
          min-width: 0;
        }
        .ynow-lab-im-universe-row .btn {
          flex: 0 0 auto;
          white-space: nowrap;
          padding: 3px 10px;
          font-size: 12px;
        }
        .ynow-lab-im-muted {
          color: #888;
          font-size: 11.5px;
        }
        .ynow-lab-im-actions {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 8px;
          margin-top: 4px;
        }
        .ynow-lab-im-actions .btn {
          white-space: nowrap;
          padding: 6px 14px;
          font-weight: 700;
        }
        .ynow-lab-im-methods .shiny-options-group {
          display: inline-flex;
          flex-wrap: wrap;
          flex-direction: row;
          align-items: center;
          column-gap: 12px;
        }
        .ynow-lab-im-methods .checkbox-inline {
          margin-left: 0 !important;
          margin-right: 0 !important;
          padding-left: 18px;
        }
        .ynow-lab-im-quality .form-group {
          margin-bottom: 4px;
        }
        .ynow-lab-im-quality-hint {
          font-size: 12px;
          color: #888;
          line-height: 1.35;
          display: block;
          margin: -2px 0 0 0;
        }
        /* Lite：產業／模型欄隱藏後，篩選欄拉滿；盈餘品質與 ADR 同列 */
        body.ynow-lite .ynow-lab-im-filter-col {
          width: 100% !important;
          max-width: 100%;
        }
        .ynow-lab-im-eq-adr-row {
          margin-left: -8px;
          margin-right: -8px;
        }
        .ynow-lab-im-eq-adr-row > [class*='col-'] {
          padding-left: 8px;
          padding-right: 8px;
        }
        /* 台股：不含 ADR 選項（僅美股有意義） */
        body.ynow-market-tw .ynow-us-only {
          display: none !important;
        }
        body.ynow-market-tw .ynow-lab-im-eq-adr-row > .ynow-lab-im-eq-col {
          width: 100% !important;
          max-width: 100%;
        }
        .ynow-lab-im-eq-explain {
          margin: 2px 0 14px 0;
          padding: 8px 10px;
          color: #555;
          font-size: 12.5px;
          line-height: 1.5;
          background: #f7f8fa;
          border: 1px solid #e5e7eb;
          border-radius: 4px;
        }
        .ynow-lab-im-eq-explain b {
          color: #222;
          margin-right: 4px;
        }
        .ynow-lab-pill {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 10px;
          font-size: 11.5px;
          font-weight: 700;
          line-height: 1.4;
          white-space: nowrap;
        }
        .ynow-lab-pill-fs {
          background: #f0f0f0;
          color: #222222;
          border: 1px solid #c8c8c8;
        }
        .ynow-lab-pill-eq-ok {
          background: #e7f6ee;
          color: #1b6b3a;
          border: 1px solid #9fd4b3;
        }
        .ynow-lab-pill-eq-no {
          background: #fdecea;
          color: #a94442;
          border: 1px solid #f0b8b4;
        }
        .ynow-lab-pill-gate-ok {
          background: #fff4d6;
          color: #8a5a00;
          border: 1px solid #e6c36a;
        }
        .ynow-lab-pill-gate-no {
          background: #f4f4f4;
          color: #666;
          border: 1px solid #d0d0d0;
        }
        .ynow-lab-pill-muted {
          background: #f7f7f7;
          color: #999;
          border: 1px solid #e5e5e5;
          font-weight: 600;
        }

        /* YNOW page — three stacked blocks (same chapter chrome as HFV) */
        .ynow-funnel-report {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 0 auto 24px auto;
          box-sizing: border-box;
        }
        .ynow-funnel-report__masthead {
          margin: 0 0 16px 0;
          padding: 4px 2px 14px 2px;
          border-bottom: 2px solid #1a1a1a;
        }
        .ynow-funnel-report__masthead h2 {
          margin: 0 0 8px 0;
          font-size: clamp(20px, 2.6vw, 28px);
          font-weight: 700;
          letter-spacing: -0.01em;
          color: #1a1a1a;
        }
        .ynow-funnel-report__lead {
          margin: 0;
          max-width: 58em;
          font-size: 13.5px;
          line-height: 1.55;
          color: #555;
        }
        .ynow-funnel-chapter {
          margin: 0 0 18px 0;
          padding: 0;
          background: #fff;
          border: 1px solid #e6e6e6;
          border-radius: 8px;
          overflow: hidden;
        }
        .ynow-funnel-chapter__head {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 6px 12px;
          padding: 12px 16px;
          background: #fff;
          border-bottom: 1px solid #ececec;
        }
        .ynow-funnel-chapter__kicker {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #888;
        }
        .ynow-funnel-chapter__title {
          margin: 0;
          font-size: 16px;
          font-weight: 700;
          color: #1a1a1a;
          line-height: 1.35;
        }
        .ynow-funnel-chapter__body {
          padding: 14px 16px 16px 16px;
        }
        .ynow-funnel-chapter__lead {
          margin: 0 0 12px 0;
          font-size: 12.5px;
          line-height: 1.5;
          color: #666;
        }
        /* Notes / 附註 — collapsible annotation chrome (YNOW + siblings) */
        .ynow-notes {
          margin: 0 0 12px 0;
          border: 1px solid #ececec;
          border-radius: 6px;
          background: #fafafa;
        }
        /* Model Selector footnote: consideration dimensions (default collapsed) */
        .ynow-ms-dims {
          margin: 14px 0 0 0;
        }
        .ynow-notes > .ynow-notes__summary {
          cursor: pointer;
          list-style: none;
          padding: 8px 12px;
          font-size: 12.5px;
          font-weight: 700;
          letter-spacing: 0.02em;
          color: #444;
          user-select: none;
        }
        .ynow-notes > .ynow-notes__summary::-webkit-details-marker {
          display: none;
        }
        .ynow-notes > .ynow-notes__summary::marker {
          content: '';
        }
        .ynow-notes > .ynow-notes__summary::before {
          content: '\\25B8';
          display: inline-block;
          width: 1.1em;
          color: #888;
          font-weight: 700;
        }
        .ynow-notes[open] > .ynow-notes__summary::before {
          content: '\\25BE';
        }
        .ynow-notes__body {
          padding: 0 12px 10px 12px;
        }
        .ynow-notes__body > :last-child {
          margin-bottom: 0;
        }
        .ynow-funnel-chapter__body > .ynow-notes .ynow-funnel-chapter__lead {
          margin: 0;
        }
        .ynow-funnel-report__masthead > .ynow-funnel-report__lead {
          margin-top: 0;
          margin-bottom: 0;
        }
        .ynow-dc-item > .ynow-notes,
        .ynow-dc-live > .ynow-notes,
        .ynow-macro-kpi > .ynow-notes {
          margin-top: 8px;
          margin-bottom: 0;
        }
        .ynow-funnel-toolbar {
          margin: 0 0 14px 0;
        }
        .ynow-funnel-toolbar .form-group {
          margin-bottom: 8px;
        }
        .ynow-funnel-kpi-row {
          margin: 0 0 12px 0;
        }
        .ynow-funnel-chapter .ynow-macro-bubble h4 {
          margin: 0 0 8px 0;
          font-size: 14px;
          font-weight: 700;
          color: #1a1a1a;
        }
        .ynow-funnel-chapter .ynow-macro-hint {
          margin: 0 0 10px 0;
        }
        .ynow-funnel-scorecards {
          margin: 0 0 4px 0;
        }
        .ynow-funnel-scorecards .info-box,
        .ynow-funnel-scorecards .small-box {
          margin-bottom: 12px;
        }
        .ynow-funnel-kpi-jump-row {
          margin: 0 0 16px 0;
        }
        .ynow-funnel-kpi-jump-row > .col-sm-4 {
          margin-bottom: 0;
        }
        a.ynow-funnel-kpi-jump {
          display: block;
          color: inherit;
          text-decoration: none !important;
          cursor: pointer;
          border-radius: 8px;
        }
        a.ynow-funnel-kpi-jump:hover,
        a.ynow-funnel-kpi-jump:focus {
          text-decoration: none !important;
          color: inherit;
        }
        a.ynow-funnel-kpi-jump:focus {
          outline: 2px solid #1a1a1a;
          outline-offset: 3px;
        }
        a.ynow-funnel-kpi-jump .small-box,
        a.ynow-funnel-kpi-jump .info-box {
          margin-bottom: 0;
          width: 100%;
          border-radius: 8px;
          box-shadow: none;
          border: 1px solid #e6e6e6;
          min-height: 108px;
          cursor: pointer;
        }
        #ynow_funnel_ch1,
        #ynow_funnel_ch2,
        #ynow_funnel_ch3,
        #ynow_funnel_fscore {
          scroll-margin-top: 72px;
        }
        .ynow-funnel-table-wrap {
          overflow-x: auto;
          width: 100%;
        }
        .ynow-funnel-table-wrap table {
          width: 100% !important;
        }
        .ynow-funnel-report > .box {
          margin-bottom: 14px;
          border-radius: 8px;
          box-shadow: none;
          border: 1px solid #e6e6e6;
        }
        .ynow-funnel-report > .box > .box-header {
          padding: 10px 14px;
        }
        .ynow-funnel-report > .box > .box-body {
          padding: 12px 14px 14px 14px;
        }
        @media (max-width: 767px) {
          .ynow-funnel-chapter__body {
            padding: 12px;
          }
          .ynow-funnel-report__masthead {
            padding-bottom: 12px;
          }
        }

        /* Testing — Quantitative Backtest — report shell (same logic as HFV / YNOW) */
        /* Macro & Market Trends — logo 藍綠主題；數字 KPI＝黑底金字；同列等高；手機響應式 */
        .ynow-macro-report {
          --ynow-macro-blue: var(--ynow-logo-blue, #0C5484);
          --ynow-macro-green: var(--ynow-logo-green, #249C60);
          --ynow-macro-gold: var(--ynow-gold, #F5C518);
          --ynow-macro-gold-deep: var(--ynow-gold-deep, #C9A227);
          --ynow-macro-ink: #0b1220;
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 0 auto 24px auto;
          padding: 0 2px;
          box-sizing: border-box;
        }
        .ynow-macro-report__masthead {
          margin: 0 0 14px 0;
          padding: 8px 12px 14px 12px;
          border-bottom: 3px solid var(--ynow-macro-green);
          border-radius: 0 0 8px 8px;
          background: linear-gradient(
            105deg,
            rgba(12, 84, 132, 0.10) 0%,
            rgba(36, 156, 96, 0.08) 55%,
            rgba(12, 84, 132, 0.04) 100%
          );
        }
        .ynow-macro-report__masthead h2 {
          margin: 0 0 8px 0;
          font-size: clamp(20px, 2.6vw, 28px);
          font-weight: 700;
          color: var(--ynow-macro-blue);
        }
        .ynow-macro-report__lead {
          margin: 0;
          font-size: 13.5px;
          line-height: 1.55;
          color: #3d4f5f;
          max-width: 72em;
        }
        .ynow-macro-chapter {
          margin: 18px 0 22px 0;
          padding: 12px 12px 14px 12px;
          border: 1px solid rgba(12, 84, 132, 0.18);
          border-left: 4px solid var(--ynow-macro-green);
          border-radius: 8px;
          background: rgba(12, 84, 132, 0.03);
        }
        .ynow-macro-chapter h3,
        .ynow-macro-chapter h4 {
          margin: 0 0 8px 0;
          font-size: 16px;
          font-weight: 700;
          color: var(--ynow-macro-blue);
        }
        .ynow-macro-chapter h4 {
          font-size: 14px;
          color: var(--ynow-macro-green);
        }
        .ynow-macro-chapter__lead {
          margin: 0 0 12px 0;
          max-width: 72em;
        }
        .ynow-macro-concept-cloud-wrap {
          margin: 4px 0 14px 0;
        }
        .ynow-macro-concept-cloud-wrap > .control-label {
          display: block;
          margin-bottom: 4px;
          font-weight: 700;
          color: var(--ynow-macro-blue, #0C5484);
        }
        .ynow-macro-concept-cloud-wrap > .ynow-macro-hint {
          margin: 0 0 8px 0;
        }
        .ynow-macro-concept-cloud .shiny-options-group {
          display: flex;
          flex-wrap: wrap;
          gap: 8px 10px;
          align-items: center;
          margin: 0;
        }
        .ynow-macro-concept-cloud .checkbox-inline,
        .ynow-macro-concept-cloud label.checkbox-inline {
          margin: 0 !important;
          padding: 2px 10px !important;
          border: 1px solid rgba(12, 84, 132, 0.28);
          border-radius: 999px;
          background: #f7fafc;
          line-height: 1.25;
          white-space: nowrap;
          transition: background 0.15s ease, border-color 0.15s ease, color 0.15s ease;
        }
        .ynow-macro-concept-cloud .checkbox-inline input[type='checkbox'] {
          position: absolute;
          opacity: 0;
          width: 0;
          height: 0;
          margin: 0;
          pointer-events: none;
        }
        .ynow-macro-concept-cloud .ynow-macro-cloud-word {
          font-weight: 600;
          color: #0b1220;
          letter-spacing: 0.01em;
        }
        .ynow-macro-concept-cloud .checkbox-inline:hover {
          border-color: rgba(12, 84, 132, 0.55);
          background: #eef5fb;
        }
        .ynow-macro-concept-cloud .checkbox-inline:has(input:checked),
        .ynow-macro-concept-cloud .checkbox-inline.active {
          background: rgba(12, 84, 132, 0.12);
          border-color: #0C5484;
        }
        .ynow-macro-concept-cloud .checkbox-inline:has(input:checked) .ynow-macro-cloud-word,
        .ynow-macro-concept-cloud .checkbox-inline.active .ynow-macro-cloud-word {
          color: #0C5484;
        }
        .ynow-macro-chapter > .ynow-notes {
          margin-top: 10px;
        }
        .ynow-macro-card {
          background: #fff;
          border: 1px solid rgba(12, 84, 132, 0.22);
          border-top: 3px solid var(--ynow-macro-blue);
          border-radius: 8px;
          padding: 12px 14px;
          margin-bottom: 12px;
          min-height: 110px;
          height: 100%;
        }
        .ynow-macro-card h4 {
          margin: 0 0 8px 0;
          font-size: 13px;
          font-weight: 700;
          color: var(--ynow-macro-blue);
        }
        /* KPI 列：同列等高（flex） */
        .ynow-macro-kpi-row {
          display: flex;
          flex-wrap: wrap;
          margin-left: -8px;
          margin-right: -8px;
        }
        .ynow-macro-kpi-row > [class*='col-'] {
          display: flex;
          flex-direction: column;
          padding-left: 8px;
          padding-right: 8px;
          margin-bottom: 12px;
        }
        .ynow-macro-kpi,
        .ynow-macro-rf {
          background: var(--ynow-macro-ink);
          border: 1px solid rgba(201, 162, 39, 0.45);
          border-radius: 8px;
          padding: 12px 14px;
          margin-bottom: 0;
          flex: 1 1 auto;
          width: 100%;
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          min-height: 112px;
          box-shadow: 0 2px 8px rgba(12, 84, 132, 0.12);
        }
        .ynow-macro-kpi__label {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: var(--ynow-macro-gold-deep);
        }
        .ynow-macro-kpi__value,
        .ynow-macro-rf__value {
          font-size: clamp(18px, 4.2vw, 26px);
          font-weight: 700;
          color: var(--ynow-macro-gold);
          line-height: 1.2;
          margin: 6px 0;
          word-break: break-word;
        }
        .ynow-macro-rf__value {
          font-size: clamp(22px, 5vw, 30px);
        }
        .ynow-macro-kpi__chg { font-size: 12.5px; font-weight: 600; }
        .ynow-macro-kpi__sym {
          font-size: 11px;
          color: rgba(245, 197, 24, 0.55);
          margin-top: auto;
        }
        .ynow-macro-kpi--clickable {
          cursor: pointer;
          transition: border-color 0.15s ease, box-shadow 0.15s ease;
        }
        .ynow-macro-kpi--clickable:hover {
          border-color: rgba(201, 162, 39, 0.85);
        }
        .ynow-macro-kpi--selected {
          border-color: var(--ynow-macro-gold);
          box-shadow: 0 0 0 2px rgba(201, 162, 39, 0.45);
        }
        .ynow-macro-index-hist {
          width: 100%;
          max-width: 100%;
          margin: 0 0 12px 0;
        }
        .ynow-macro-index-hist__card {
          width: 100%;
          min-height: 0;
        }
        .ynow-macro-index-hist__card:has(.ynow-index-detail-ready) .ynow-index-pending {
          display: none;
        }
        .ynow-own-index-overlay {
          margin: 0 0 10px 0;
          padding: 8px 10px;
          background: #f7f9fc;
          border: 1px solid #e4eaef;
          border-radius: 4px;
        }
        /* Overlay index checkboxes: keep readable spacing (labels were overlapping). */
        .ynow-own-index-overlay .shiny-options-group {
          display: flex;
          flex-direction: row;
          flex-wrap: wrap;
          align-items: center;
          gap: 10px 28px;
          margin-top: 4px;
          row-gap: 10px;
        }
        .ynow-own-index-overlay .shiny-options-group .checkbox,
        .ynow-own-index-overlay .shiny-options-group .checkbox-inline {
          display: inline-flex;
          align-items: center;
          flex: 0 0 auto;
          margin: 0 !important;
          padding: 0 4px 0 0 !important;
          float: none !important;
          min-width: 0;
        }
        .ynow-own-index-overlay .shiny-options-group .checkbox + .checkbox,
        .ynow-own-index-overlay .shiny-options-group .checkbox-inline + .checkbox-inline {
          margin-left: 0 !important;
        }
        .ynow-own-index-overlay .shiny-options-group label {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          margin: 0 !important;
          padding: 0 !important;
          white-space: nowrap;
          font-weight: normal;
          line-height: 1.35;
        }
        .ynow-own-index-overlay .shiny-options-group input[type='checkbox'] {
          position: static !important;
          margin: 0 !important;
          flex: 0 0 auto;
        }
        .ynow-own-index-overlay .ynow-macro-hint {
          margin: 6px 0 0 0;
        }
        .ynow-macro-kpi .ynow-macro-up,
        .ynow-macro-rf .ynow-macro-up { color: var(--ynow-macro-green); }
        .ynow-macro-kpi .ynow-macro-down,
        .ynow-macro-rf .ynow-macro-down { color: #ff7a70; }
        .ynow-macro-up { color: var(--ynow-macro-green); }
        .ynow-macro-down { color: #c0392b; }
        /* TW market convention: red up / green down (opposite US) */
        body.ynow-market-tw .ynow-macro-kpi .ynow-macro-up,
        body.ynow-market-tw .ynow-macro-rf .ynow-macro-up,
        body.ynow-market-tw .ynow-macro-up { color: #c0392b; }
        body.ynow-market-tw .ynow-macro-kpi .ynow-macro-down,
        body.ynow-market-tw .ynow-macro-rf .ynow-macro-down { color: var(--ynow-macro-green); }
        body.ynow-market-tw .ynow-macro-down { color: var(--ynow-macro-green); }
        .ynow-macro-hint {
          margin: 6px 0 0 0;
          font-size: 12px;
          line-height: 1.45;
          color: #4a5d6c;
        }
        .ynow-macro-kpi .ynow-macro-hint,
        .ynow-macro-rf .ynow-macro-hint {
          color: rgba(245, 197, 24, 0.72);
        }
        .ynow-macro-callout {
          border-radius: 8px;
          padding: 10px 12px;
          margin: 0 0 14px 0;
        }
        .ynow-macro-callout--mode {
          background: rgba(12, 84, 132, 0.08);
          border: 1px solid rgba(12, 84, 132, 0.22);
        }
        .ynow-macro-callout--warn {
          background: rgba(36, 156, 96, 0.08);
          border: 1px solid rgba(36, 156, 96, 0.35);
          color: #1a3d2c;
        }
        .ynow-macro-kicker {
          display: block;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: var(--ynow-macro-blue);
          margin-bottom: 4px;
        }
        .ynow-macro-badge {
          display: inline-block;
          font-size: 13px;
          font-weight: 700;
          padding: 3px 10px;
          border-radius: 4px;
        }
        .ynow-macro-badge--us {
          background: var(--ynow-macro-blue);
          color: #fff;
        }
        .ynow-macro-badge--tw {
          background: var(--ynow-macro-green);
          color: #fff;
        }
        .ynow-macro-report .btn.btn-default {
          border-color: var(--ynow-macro-blue);
          color: var(--ynow-macro-blue);
        }
        .ynow-macro-report .btn.btn-default:hover {
          background: var(--ynow-macro-blue);
          color: #fff;
        }
        .ynow-macro-report .form-control:focus {
          border-color: var(--ynow-macro-green);
          box-shadow: 0 0 0 2px rgba(36, 156, 96, 0.2);
        }
        .ynow-macro-report .selectize-input.focus {
          border-color: var(--ynow-macro-green);
          box-shadow: 0 0 0 2px rgba(36, 156, 96, 0.2);
        }
        .ynow-macro-chart-row {
          display: flex;
          flex-wrap: wrap;
          margin-left: -8px;
          margin-right: -8px;
        }
        .ynow-macro-chart-row > [class*='col-'] {
          padding-left: 8px;
          padding-right: 8px;
          margin-bottom: 12px;
        }
        .ynow-bubble-pair-row {
          align-items: stretch;
        }
        .ynow-bubble-pair-col {
          display: flex;
          flex-direction: column;
        }
        .ynow-bubble-attr-plot-host {
          flex: 1 1 auto;
          width: 100%;
          min-height: 420px;
        }
        .ynow-bubble-attr-plot-host .plotly,
        .ynow-bubble-attr-plot-host .js-plotly-plot,
        .ynow-bubble-attr-plot-host .html-widget {
          width: 100% !important;
          height: 100% !important;
        }
        .ynow-bubble-buffett-plot-wrap {
          width: 100%;
          max-width: none;
          margin: 0 0 8px 0;
        }
        .ynow-bubble-buffett-plot-wrap .plotly,
        .ynow-bubble-buffett-plot-wrap .js-plotly-plot,
        .ynow-bubble-buffett-plot-wrap .html-widget {
          width: 100% !important;
          max-width: none !important;
        }
        .ynow-macro-overlay-plot-wrap {
          width: 100%;
          max-width: none;
          margin: 0 0 8px 0;
        }
        .ynow-macro-overlay-plot-wrap .plotly,
        .ynow-macro-overlay-plot-wrap .js-plotly-plot,
        .ynow-macro-overlay-plot-wrap .html-widget {
          width: 100% !important;
          max-width: none !important;
        }
        /* Rf / alert 列也等高 */
        .ynow-macro-rf-row {
          display: flex;
          flex-wrap: wrap;
          margin-left: -8px;
          margin-right: -8px;
        }
        .ynow-macro-rf-row > [class*='col-'] {
          display: flex;
          flex-direction: column;
          padding-left: 8px;
          padding-right: 8px;
          margin-bottom: 12px;
        }
        .ynow-macro-rf-row .ynow-macro-card {
          flex: 1 1 auto;
          width: 100%;
        }
        .ynow-macro-kpi--htcdi.ynow-htcdi-alert--watch {
          border-color: rgba(230, 168, 23, 0.85);
        }
        .ynow-macro-kpi--htcdi.ynow-htcdi-alert--warning {
          border-color: rgba(211, 84, 0, 0.9);
        }
        .ynow-macro-kpi--htcdi.ynow-htcdi-alert--critical {
          border-color: rgba(192, 57, 43, 0.95);
          box-shadow: 0 0 0 2px rgba(192, 57, 43, 0.25);
        }
        .ynow-macro-kpi--htcdi.ynow-htcdi-alert--unavailable {
          border-color: rgba(12, 84, 132, 0.35);
        }
        .ynow-htcdi-flow {
          background-image: var(--ynow-logo-flow-gradient, linear-gradient(105deg, #0C5484 0%, #1AA8B8 50%, #249C60 100%));
          background-size: 220% 100%;
          background-repeat: no-repeat;
          -webkit-background-clip: text;
          background-clip: text;
          -webkit-text-fill-color: transparent;
          color: transparent;
          animation: ynow-logo-flow 2.6s ease-in-out infinite;
          font-weight: 700;
        }
        /* HTCDI only: flame dynamic fill (KPI, four pillars, expand tables). */
        .ynow-macro-kpi--htcdi .ynow-htcdi-flow,
        .ynow-htcdi-sub .ynow-htcdi-flow,
        .ynow-htcdi-table .ynow-htcdi-flow,
        .ynow-htcdi-expand__card .ynow-htcdi-flow {
          background-image: var(--ynow-htcdi-flame-gradient, linear-gradient(105deg, #8B1A1A 0%, #E67E22 40%, #FFE66D 70%, #C0392B 100%));
          animation: ynow-htcdi-flame-flow 2.4s ease-in-out infinite;
        }
        .ynow-macro-own-index .ynow-macro-kpi__value.ynow-htcdi-flow,
        .ynow-macro-kpi--ynow .ynow-macro-kpi__value.ynow-htcdi-flow {
          color: transparent;
          -webkit-text-fill-color: transparent;
        }
        .ynow-htcdi-unavailable {
          color: #6b7c8a;
          -webkit-text-fill-color: #6b7c8a;
          font-weight: 600;
        }
        .ynow-macro-kpi--rf .ynow-htcdi-flow,
        .ynow-macro-rf__value.ynow-htcdi-flow {
          animation: none;
          background-image: none;
          -webkit-text-fill-color: inherit;
          color: inherit;
        }
        @media (prefers-reduced-motion: reduce) {
          .ynow-htcdi-flow { animation: none; background-position: 0% 50%; }
          .ynow-macro-kpi--htcdi .ynow-htcdi-flow,
          .ynow-htcdi-sub .ynow-htcdi-flow,
          .ynow-htcdi-table .ynow-htcdi-flow,
          .ynow-htcdi-expand__card .ynow-htcdi-flow {
            animation: none;
            background-position: 0% 50%;
          }
        }
        @supports not ((-webkit-background-clip: text) or (background-clip: text)) {
          .ynow-htcdi-flow,
          .ynow-fund-profile-badge .ynow-htcdi-flow {
            background-image: none;
            -webkit-text-fill-color: #0C5484;
            color: #0C5484;
            animation: none;
          }
          .ynow-macro-kpi--htcdi .ynow-htcdi-flow,
          .ynow-htcdi-sub .ynow-htcdi-flow,
          .ynow-htcdi-table .ynow-htcdi-flow,
          .ynow-htcdi-expand__card .ynow-htcdi-flow {
            -webkit-text-fill-color: #E67E22;
            color: #E67E22;
          }
        }
        .ynow-macro-htcdi-expand { width: 100%; margin: 0 0 12px 0; }
        .ynow-htcdi-expand__card h4 { margin: 12px 0 6px 0; font-size: 14px; }
        .ynow-htcdi-network { margin: 0 0 12px 0; }
        .ynow-htcdi-network__lead {
          margin: 0 0 4px 0;
          font-size: 13px;
          font-weight: 700;
          color: #2c3e50;
        }
        .ynow-htcdi-chain-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
          gap: 10px;
          margin: 8px 0 4px 0;
        }
        .ynow-htcdi-chain-card {
          border: 1px solid #e0b56a;
          background: linear-gradient(180deg, #fffaf0 0%, #ffffff 70%);
          border-radius: 8px;
          padding: 10px 12px;
          box-shadow: 0 1px 0 rgba(44, 62, 80, 0.04);
        }
        .ynow-htcdi-chain-card__title {
          font-size: 12px;
          font-weight: 700;
          color: #8a4b00;
          margin: 0 0 8px 0;
        }
        .ynow-htcdi-chain-flow {
          display: flex;
          flex-wrap: wrap;
          align-items: stretch;
          gap: 6px;
        }
        .ynow-htcdi-chain-arrow {
          align-self: center;
          color: #b07a2a;
          font-weight: 700;
          flex: 0 0 auto;
        }
        .ynow-htcdi-chain-node {
          flex: 1 1 88px;
          min-width: 88px;
          max-width: 180px;
          border-radius: 6px;
          padding: 6px 8px;
          border: 1px solid #d5dde5;
          background: #f7f9fb;
          display: flex;
          flex-direction: column;
          gap: 2px;
        }
        .ynow-htcdi-chain-node--cooling {
          border-color: #e0a050;
          background: #fff3e0;
        }
        .ynow-htcdi-chain-node--ok {
          border-color: #c5d0da;
          background: #f4f7fa;
          opacity: 0.88;
        }
        .ynow-htcdi-chain-node--layer {
          min-width: 110px;
        }
        .ynow-htcdi-chain-node__lab {
          font-size: 12px;
          font-weight: 700;
          color: #2c3e50;
          line-height: 1.25;
        }
        .ynow-htcdi-chain-node__state {
          font-size: 10px;
          font-weight: 700;
          color: #b35c00;
          letter-spacing: 0.02em;
          text-transform: uppercase;
        }
        .ynow-htcdi-chain-node__mem {
          font-size: 10px;
          color: #5d6d7e;
          line-height: 1.3;
          word-break: break-word;
        }
        @media (max-width: 575px) {
          .ynow-htcdi-chain-grid { grid-template-columns: 1fr; }
          .ynow-htcdi-chain-node { max-width: none; }
        }
        .ynow-htcdi-table-wrap { overflow-x: auto; margin: 0 0 10px 0; }
        .ynow-htcdi-table { font-size: 12px; margin-bottom: 0; }
        .ynow-htcdi-term-badge {
          display: inline-block;
          min-width: 2.6em;
          padding: 1px 7px;
          border-radius: 3px;
          font-weight: 700;
          font-size: 11px;
          letter-spacing: 0.02em;
          line-height: 1.5;
          border: 1px solid transparent;
        }
        .ynow-htcdi-term--stmt .ynow-htcdi-term-badge,
        .ynow-htcdi-term-badge.ynow-htcdi-term--stmt {
          color: #0b4f6c; background: #d9eef7; border-color: #9ec9dc;
        }
        .ynow-htcdi-term--mkt .ynow-htcdi-term-badge,
        .ynow-htcdi-term-badge.ynow-htcdi-term--mkt {
          color: #1b5e20; background: #e3f2e5; border-color: #9cc7a2;
        }
        .ynow-htcdi-term--inf .ynow-htcdi-term-badge,
        .ynow-htcdi-term-badge.ynow-htcdi-term--inf {
          color: #8a4b00; background: #fff0d6; border-color: #e0b56a;
        }
        .ynow-htcdi-term--traj .ynow-htcdi-term-badge,
        .ynow-htcdi-term-badge.ynow-htcdi-term--traj {
          color: #4a148c; background: #f0e6f8; border-color: #c4a3de;
        }
        .ynow-htcdi-table th.ynow-htcdi-term--stmt,
        .ynow-htcdi-table td.ynow-htcdi-term--stmt { background-color: rgba(217, 238, 247, 0.45); }
        .ynow-htcdi-table th.ynow-htcdi-term--mkt,
        .ynow-htcdi-table td.ynow-htcdi-term--mkt { background-color: rgba(227, 242, 229, 0.45); }
        .ynow-htcdi-table th.ynow-htcdi-term--inf,
        .ynow-htcdi-table td.ynow-htcdi-term--inf { background-color: rgba(255, 240, 214, 0.55); }
        .ynow-htcdi-table th.ynow-htcdi-term--traj,
        .ynow-htcdi-table td.ynow-htcdi-term--traj { background-color: rgba(240, 230, 248, 0.5); }
        .ynow-htcdi-table--inputs tr.ynow-htcdi-term-groups th {
          text-align: center; border-bottom: 0; padding-bottom: 4px;
        }
        .ynow-htcdi-table--inputs tr.ynow-htcdi-term-metrics th {
          font-weight: 600; white-space: nowrap;
        }
        .ynow-htcdi-term-bridge {
          margin: 12px 0 10px 0;
          padding: 10px 12px;
          background: #f7f9fb;
          border: 1px solid #dce3ea;
          border-radius: 6px;
        }
        .ynow-htcdi-term-bridge__lead {
          margin: 0 0 8px 0;
          font-size: 12px;
          color: #34495e;
        }
        .ynow-htcdi-term-map {
          list-style: none;
          margin: 0;
          padding: 0;
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 6px 14px;
        }
        .ynow-htcdi-term-map li {
          display: flex;
          align-items: center;
          gap: 6px;
          font-size: 12px;
          color: #2c3e50;
          min-width: 0;
        }
        .ynow-htcdi-term-map__arrow { color: #7f8c8d; flex: 0 0 auto; }
        @media (max-width: 767px) {
          .ynow-htcdi-term-map { grid-template-columns: 1fr; }
        }
        .ynow-htcdi-formula-banner { margin: 8px 0 14px 0; }
        .ynow-htcdi-formula-banner__eq {
          font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center;
          margin: 0 0 8px 0; padding: 10px; background-color: #F2F4F4; border-radius: 8px;
        }
        .ynow-htcdi-formula-banner__parts {
          font-size: 13px; color: #555; text-align: center; margin: 0 0 8px 0;
        }
        .ynow-htcdi-pair { margin: 0 0 10px 0; }
        .ynow-htcdi-sub .ynow-macro-kpi__value { font-size: clamp(16px, 3.6vw, 22px); }
        @media (max-width: 991px) {
          .ynow-macro-kpi-row > [class*='col-'] {
            width: 50%;
            float: none;
          }
          /* Rf : YNOW : HTCDI → stack as Rf full, then YNOW|HTCDI half-half */
          .ynow-macro-rf-row > .col-md-6 {
            width: 100%;
          }
          .ynow-macro-rf-row > .col-md-3 {
            width: 50%;
          }
        }
        @media (max-width: 575px) {
          .ynow-macro-report {
            margin-bottom: 16px;
            padding: 0;
          }
          .ynow-macro-report__masthead {
            padding: 8px 10px 12px 10px;
          }
          .ynow-macro-chapter {
            padding: 10px 8px 12px 8px;
            margin: 12px 0 14px 0;
          }
          .ynow-macro-kpi-row > [class*='col-'] {
            width: 50%;
          }
          .ynow-macro-kpi,
          .ynow-macro-rf {
            min-height: 100px;
            padding: 10px 11px;
          }
          .ynow-macro-rf-row > [class*='col-'] {
            width: 100%;
            float: none;
          }
          .ynow-macro-chart-row > [class*='col-'] {
            width: 100%;
            float: none;
          }
        }
        .ynow-backtest-report {
          max-width: var(--ynow-page-max, 1200px);
          width: 100%;
          margin: 0 auto 24px auto;
          box-sizing: border-box;
        }
        /* After report-shell rules: force full bleed on small screens */
        @media (max-width: 767px) {
          .ynow-home,
          .ynow-macro-report,
          .ynow-hfv-report,
          .ynow-funnel-report,
          .ynow-backtest-report,
          .ynow-bblab--report {
            max-width: 100%;
          }
        }
        .ynow-backtest-report__masthead {
          margin: 0 0 12px 0;
          padding: 4px 2px 10px 2px;
          border-bottom: 2px solid #1a1a1a;
        }
        .ynow-backtest-report__masthead h2 {
          margin: 0;
          font-size: clamp(20px, 2.6vw, 28px);
          font-weight: 700;
          letter-spacing: -0.01em;
          color: #1a1a1a;
        }
        .ynow-backtest-report__lead {
          margin: 0;
          max-width: none;
          font-size: 13.5px;
          line-height: 1.55;
          color: #555;
        }
        /* Masthead intro + Run: one light-gray band, 4:1, vertically centered */
        .ynow-backtest-zone-band {
          display: grid;
          grid-template-columns: 4fr 1fr;
          gap: 14px 18px;
          align-items: center;
          margin: 0 0 18px 0;
          padding: 14px 16px;
          background: #fafafa;
          border: 1px solid #e4e4e4;
          border-radius: 8px;
          box-sizing: border-box;
        }
        .ynow-backtest-zone-band__lead {
          min-width: 0;
        }
        .ynow-backtest-zone-band__run {
          min-width: 0;
          display: flex;
          flex-direction: column;
          align-items: stretch;
          justify-content: center;
          gap: 8px;
        }
        .ynow-backtest-zone-band__run .ynow-backtest-toolbar__label {
          text-align: center;
        }
        .ynow-backtest-zone-band__run .btn {
          width: 100%;
        }
        .ynow-backtest-toolbar {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
          gap: 12px 14px;
          margin: 0 0 18px 0;
          padding: 14px 16px;
          background: #fafafa;
          border: 1px solid #e4e4e4;
          border-radius: 8px;
          box-sizing: border-box;
        }
        .ynow-backtest-toolbar--above-chart {
          margin: 4px 0 12px 0;
        }
        .ynow-backtest-toolbar__group {
          min-width: 0;
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .ynow-backtest-toolbar__group--wide {
          grid-column: 1 / -1;
        }
        .ynow-backtest-toolbar__group--run {
          flex-direction: row;
          flex-wrap: wrap;
          align-items: center;
          gap: 10px 14px;
        }
        .ynow-backtest-toolbar__label {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #666;
          line-height: 1.35;
        }
        .ynow-backtest-toolbar__group .form-group {
          margin: 0;
        }
        .ynow-backtest-toolbar__group > label.control-label,
        .ynow-backtest-toolbar .shiny-input-radiogroup > label.control-label {
          display: block;
          margin: 0 0 6px 0;
          padding: 0;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #666;
          line-height: 1.35;
        }
        .ynow-backtest-toolbar .shiny-options-group {
          display: flex !important;
          flex-wrap: wrap !important;
          align-items: stretch;
          gap: 6px;
          margin: 0;
          padding: 0;
          clear: none;
        }
        .ynow-backtest-toolbar .radio,
        .ynow-backtest-toolbar .radio-inline {
          float: none !important;
          display: inline-flex !important;
          align-items: center;
          margin: 0 !important;
          padding: 0 !important;
          min-height: 0;
        }
        .ynow-backtest-toolbar .radio > label,
        .ynow-backtest-toolbar .radio-inline,
        .ynow-backtest-toolbar .radio-inline > label {
          margin: 0 !important;
          padding: 7px 12px 7px 28px !important;
          border: 1px solid #d0d0d0;
          border-radius: 999px;
          background: #fff;
          font-size: 12.5px;
          font-weight: 500;
          line-height: 1.25;
          color: #222;
          white-space: nowrap;
          cursor: pointer;
          transition: border-color .12s ease, background .12s ease, box-shadow .12s ease;
        }
        .ynow-backtest-toolbar .radio input[type='radio'],
        .ynow-backtest-toolbar .radio-inline input[type='radio'] {
          position: absolute;
          margin-left: -20px;
          margin-top: 1px;
        }
        .ynow-backtest-toolbar .radio > label:hover,
        .ynow-backtest-toolbar .radio-inline:hover {
          border-color: #999;
          background: #fff;
        }
        .ynow-backtest-toolbar .radio:has(input:checked) > label,
        .ynow-backtest-toolbar .radio-inline:has(input:checked) {
          border-color: #1a1a1a;
          background: #1a1a1a;
          color: #fff;
          box-shadow: 0 1px 2px rgba(0,0,0,0.12);
        }
        .ynow-backtest-chapter {
          margin: 0 0 18px 0;
          padding: 0;
          background: #fff;
          border: 1px solid #e6e6e6;
          border-radius: 8px;
          overflow: hidden;
        }
        .ynow-backtest-chapter__head {
          display: flex;
          flex-wrap: wrap;
          align-items: baseline;
          gap: 6px 12px;
          padding: 12px 16px;
          background: #fff;
          border-bottom: 1px solid #ececec;
        }
        .ynow-backtest-chapter__kicker {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #888;
        }
        .ynow-backtest-chapter__title {
          margin: 0;
          font-size: 16px;
          font-weight: 700;
          color: #1a1a1a;
          line-height: 1.35;
        }
        .ynow-backtest-chapter__body {
          padding: 14px 16px 16px 16px;
        }
        .ynow-backtest-chapter__lead {
          margin: 0 0 12px 0;
          font-size: 12.5px;
          line-height: 1.5;
          color: #666;
        }
        .ynow-backtest-subhead {
          margin: 0 0 10px 0;
          font-size: 13px;
          font-weight: 700;
          color: #333;
        }
        .ynow-backtest-legend {
          margin: 12px 0 0 0;
          padding-left: 18px;
          font-size: 12px;
          color: #666;
          line-height: 1.55;
        }
        .ynow-backtest-inline-hint {
          margin: -4px 0 12px 0;
          font-size: 11.5px;
          line-height: 1.45;
          color: #777;
        }
        .ynow-backtest-callout {
          margin-top: 8px;
          padding: 12px;
          background: #f5f5f5;
          border: 1px solid #d0d0d0;
          border-radius: 5px;
          font-size: 12px;
          color: #444;
          line-height: 1.55;
        }
        .ynow-backtest-report > .box,
        .ynow-backtest-report > .row > .col-sm-12 > .box,
        .ynow-backtest-report > .fluid-row > .col-sm-12 > .box {
          margin-bottom: 14px;
          border-radius: 8px;
          box-shadow: none;
          border: 1px solid #e6e6e6;
        }
        .ynow-backtest-report > .box > .box-header,
        .ynow-backtest-report .box > .box-header {
          padding: 10px 14px;
        }
        .ynow-backtest-report > .box > .box-body,
        .ynow-backtest-report .box > .box-body {
          padding: 12px 14px 14px 14px;
        }
        .ynow-backtest-report .ynow-bt-params .nav-tabs-custom {
          margin-bottom: 0;
          box-shadow: none;
        }
        @media (max-width: 767px) {
          .ynow-backtest-zone-band {
            grid-template-columns: 1fr;
            align-items: stretch;
          }
          .ynow-backtest-zone-band__run .ynow-backtest-toolbar__label {
            text-align: left;
          }
          .ynow-backtest-toolbar {
            grid-template-columns: 1fr;
            padding: 12px;
            gap: 14px;
          }
          .ynow-backtest-toolbar__group--wide {
            grid-column: auto;
          }
          .ynow-backtest-toolbar .radio > label,
          .ynow-backtest-toolbar .radio-inline {
            white-space: normal;
            max-width: 100%;
          }
          .ynow-backtest-chapter__body {
            padding: 12px;
          }
          .ynow-backtest-report__masthead {
            padding-bottom: 10px;
          }
        }

        /* YNOW：Schilit 自動判讀 */
        .ynow-shen-wrap { margin: 0 0 16px 0; }
        .ynow-shen-count { display: flex; flex-wrap: wrap; gap: 8px; margin: 0 0 12px 0; }
        .ynow-shen-st {
          display: inline-block; font-size: 11.5px; font-weight: 700;
          padding: 2px 8px; border-radius: 4px;
        }
        .ynow-shen-alert { background: #c0392b; color: #fff; }
        .ynow-shen-watch { background: #f4e6c8; color: #7a4e00; }
        .ynow-shen-pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-shen-na { background: #eee; color: #666; }
        .ynow-shen-strip {
          background: #fdf2f0; border: 1px solid #f0c9c4; border-radius: 6px;
          padding: 10px 12px; margin: 0 0 14px 0;
        }
        .ynow-shen-strip h4 { margin: 0 0 8px 0; font-size: 15px; color: #922b21; }
        .ynow-shen-cat { margin: 0 0 12px 0; }
        .ynow-shen-cat h5 { margin: 0 0 8px 0; font-size: 14px; color: #222222; }
        .ynow-shen-card {
          border: 1px solid #e4e8ec; border-radius: 6px;
          padding: 8px 10px; margin: 0 0 8px 0; background: #fff;
        }
        .ynow-shen-card.ynow-shen-alert { border-left: 4px solid #c0392b; }
        .ynow-shen-card.ynow-shen-watch { border-left: 4px solid #d4a017; }
        .ynow-shen-card.ynow-shen-pass { border-left: 4px solid #27ae60; }
        .ynow-shen-card.ynow-shen-na { border-left: 4px solid #bbb; }
        .ynow-shen-card-h {
          display: flex; justify-content: space-between; align-items: center;
          gap: 8px; margin-bottom: 4px;
        }
        .ynow-shen-code { font-weight: 700; font-size: 13px; color: #222; }
        .ynow-shen-why { margin: 0 0 4px 0; font-size: 12.5px; color: #444; line-height: 1.45; }
        .ynow-shen-metrics { margin: 0; font-size: 12px; color: #555; line-height: 1.4; }
        .ynow-shen-sep { color: #bbb; padding: 0 4px; }
        .ynow-shen-ok { font-size: 13px; color: #1e7a45; margin: 0 0 8px 0; }
        .ynow-shen-fold { margin: 8px 0 0 0; font-size: 12.5px; color: #666; }
        .ynow-shen-fold summary { cursor: pointer; font-weight: 600; }

        /* YNOW：F-Score quality-screen cards (default visible, like Statement alerts) */
        .ynow-fscore-wrap { margin: 0 0 16px 0; }
        .ynow-fscore-count { display: flex; flex-wrap: wrap; gap: 8px; margin: 0 0 12px 0; }
        .ynow-fscore-st {
          display: inline-block; font-size: 11.5px; font-weight: 700;
          padding: 2px 8px; border-radius: 4px;
        }
        .ynow-fscore-pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-fscore-fail { background: #faf0ef; color: #c0392b; }
        .ynow-fscore-waiting { color: #777; font-size: 13px; margin: 0; }
        .ynow-fscore-grid {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 8px;
        }
        @media (max-width: 767px) {
          .ynow-fscore-grid { grid-template-columns: 1fr; }
        }
        .ynow-fscore-card {
          border: 1px solid #e4e8ec; border-radius: 6px;
          padding: 8px 10px; background: #fff; margin: 0;
        }
        .ynow-fscore-card.ynow-fscore-pass { border-left: 4px solid #27ae60; }
        .ynow-fscore-card.ynow-fscore-fail { border-left: 4px solid #c0392b; }
        .ynow-fscore-card-h {
          display: flex; justify-content: space-between; align-items: flex-start;
          gap: 8px;
        }
        .ynow-fscore-item { font-weight: 600; font-size: 12.5px; color: #222; line-height: 1.4; }

        /* YNOW：Quality of Earnings dashboard + automated risk matrix */
        /* Beneish M-Score control-point widget */
        .ynow-mscore-widget {
          margin: 0 0 16px 0;
          padding: 14px 16px;
          border-radius: 8px;
          border: 1px solid #dee2e6;
          background: #f8f9fa;
        }
        .ynow-mscore-widget--danger {
          border-color: #e0b4b0;
          background: linear-gradient(180deg, #fdf4f3 0%, #fff 70%);
          box-shadow: 0 0 0 1px rgba(192, 57, 43, 0.08);
        }
        .ynow-mscore-widget--watch {
          border-color: #e6d6a8;
          background: linear-gradient(180deg, #fffbf0 0%, #fff 70%);
        }
        .ynow-mscore-widget--pass {
          border-color: #b8dfc8;
          background: linear-gradient(180deg, #eef8f1 0%, #fff 70%);
        }
        .ynow-mscore-widget--na {
          border-style: dashed;
          color: #666;
        }
        .ynow-mscore-widget__head {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          justify-content: space-between;
          gap: 8px;
          margin: 0 0 10px 0;
        }
        .ynow-mscore-widget__title {
          font-size: 14px;
          font-weight: 700;
          color: #1a1a1a;
        }
        .ynow-mscore-widget__badge {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 4px;
          font-size: 11px;
          font-weight: 700;
          background: #f0f2f4;
          color: #555;
        }
        .ynow-mscore-widget__badge--danger { background: #faf0ef; color: #c0392b; }
        .ynow-mscore-widget__badge--watch { background: #fff6e0; color: #9a7b1a; }
        .ynow-mscore-widget__badge--pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-mscore-widget__body {
          display: grid;
          grid-template-columns: minmax(88px, 120px) 1fr;
          gap: 12px;
          align-items: center;
        }
        @media (max-width: 640px) {
          .ynow-mscore-widget__body { grid-template-columns: 1fr; }
        }
        .ynow-mscore-widget__value {
          font-size: 34px;
          font-weight: 800;
          line-height: 1.05;
          font-variant-numeric: tabular-nums;
          color: #0C5484;
        }
        .ynow-mscore-widget--danger .ynow-mscore-widget__value { color: #c0392b; }
        .ynow-mscore-widget--watch .ynow-mscore-widget__value { color: #9a7b1a; }
        .ynow-mscore-widget--pass .ynow-mscore-widget__value { color: #1e7a45; }
        .ynow-mscore-widget__msg {
          margin: 0 0 4px 0;
          font-size: 13.5px;
          font-weight: 600;
          line-height: 1.45;
          color: #222;
        }
        .ynow-mscore-widget__hint {
          margin: 0;
          font-size: 12px;
          color: #666;
          line-height: 1.45;
        }
        .ynow-mscore-widget__nuance {
          margin: 10px 0 0 0;
          padding: 8px 10px;
          border-left: 3px solid #d68910;
          background: #fff8e8;
          font-size: 12.5px;
          line-height: 1.5;
          color: #5c4a12;
        }
        .ynow-mscore-widget__details {
          margin: 10px 0 0 0;
          font-size: 12.5px;
          color: #444;
        }
        .ynow-mscore-widget__details > summary {
          cursor: pointer;
          font-weight: 600;
          color: #0C5484;
        }
        .ynow-mscore-widget__list {
          margin: 8px 0 0 0;
          padding-left: 18px;
          line-height: 1.55;
        }

        .ynow-eq-wrap, .ynow-eq-matrix-wrap { margin: 0 0 18px 0; }
        .ynow-eq-waiting { color: #777; font-size: 13px; margin: 0; }
        .ynow-eq-head { margin: 0 0 12px 0; }
        .ynow-eq-title { margin: 0 0 4px 0; font-size: 15px; font-weight: 700; color: #1a1a1a; }
        .ynow-eq-sub { margin: 0; font-size: 12.5px; color: #666; line-height: 1.45; }
        .ynow-eq-scorecard {
          display: grid;
          grid-template-columns: minmax(140px, 180px) 1fr;
          gap: 12px;
          margin: 0 0 12px 0;
          align-items: stretch;
        }
        @media (max-width: 767px) {
          .ynow-eq-scorecard { grid-template-columns: 1fr; }
        }
        .ynow-eq-score-pill {
          border-radius: 8px; padding: 14px 12px; text-align: center;
          border: 1px solid #d5e3ec; background: #f7fbf9;
        }
        .ynow-eq-score-pill.ynow-eq-alert { border-color: #e0b4b0; background: #fdf4f3; }
        .ynow-eq-score-pill.ynow-eq-watch { border-color: #e6d6a8; background: #fffbf0; }
        .ynow-eq-score-pill.ynow-eq-pass { border-color: #b8dfc8; background: #eef8f1; }
        .ynow-eq-score-num {
          font-size: 36px; font-weight: 800; line-height: 1.1;
          color: #0C5484; letter-spacing: -0.02em;
        }
        .ynow-eq-score-meta {
          display: flex; justify-content: center; gap: 8px; flex-wrap: wrap;
          margin-top: 6px; font-size: 12px;
        }
        .ynow-eq-grade { font-weight: 700; color: #333; }
        .ynow-eq-flag {
          display: inline-block; font-size: 11px; font-weight: 700;
          padding: 1px 7px; border-radius: 3px;
        }
        .ynow-eq-flag.ynow-eq-alert { background: #faf0ef; color: #c0392b; }
        .ynow-eq-flag.ynow-eq-watch { background: #fff6e0; color: #9a7b1a; }
        .ynow-eq-flag.ynow-eq-pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-eq-flag.ynow-eq-na { background: #f0f2f4; color: #777; }
        .ynow-eq-kpis {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 8px;
        }
        .ynow-eq-kpi {
          border: 1px solid #e4e8ec; border-radius: 6px;
          padding: 8px 10px; background: #fff;
        }
        .ynow-eq-kpi-k { display: block; font-size: 11px; color: #777; margin-bottom: 2px; }
        .ynow-eq-kpi-v { display: block; font-size: 15px; font-weight: 700; color: #0C5484; }
        .ynow-eq-bars { margin: 4px 0 0 0; }
        .ynow-eq-bar-row {
          display: grid;
          grid-template-columns: minmax(90px, 140px) 1fr 42px;
          gap: 8px; align-items: center;
          margin: 0 0 6px 0; font-size: 12px;
        }
        .ynow-eq-bar-lab { color: #444; }
        .ynow-eq-bar-track {
          height: 8px; border-radius: 4px; background: #eef1f4; overflow: hidden;
        }
        .ynow-eq-bar-fill { height: 100%; border-radius: 4px; }
        .ynow-eq-bar-pos { background: #249C60; }
        .ynow-eq-bar-neg { background: #c0392b; }
        .ynow-eq-bar-pts { text-align: right; font-weight: 700; color: #333; font-variant-numeric: tabular-nums; }
        .ynow-eq-level {
          display: inline-flex; align-items: center; gap: 8px;
          margin-top: 8px; padding: 4px 10px; border-radius: 4px;
          font-size: 12.5px; font-weight: 700;
        }
        .ynow-eq-level.ynow-eq-alert { background: #faf0ef; color: #c0392b; }
        .ynow-eq-level.ynow-eq-watch { background: #fff6e0; color: #9a7b1a; }
        .ynow-eq-level.ynow-eq-pass { background: #e8f6ee; color: #1e7a45; }
        .ynow-eq-level.ynow-eq-na { background: #f0f2f4; color: #777; }
        .ynow-eq-level-k { opacity: 0.85; font-weight: 600; }
        .ynow-eq-matrix-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 8px;
          margin: 0 0 12px 0;
        }
        @media (max-width: 767px) {
          .ynow-eq-matrix-grid { grid-template-columns: 1fr; }
        }
        .ynow-eq-matrix-card {
          border: 1px solid #e4e8ec; border-radius: 6px;
          padding: 10px 12px; background: #fff;
        }
        .ynow-eq-matrix-card.ynow-eq-alert { border-left: 4px solid #c0392b; }
        .ynow-eq-matrix-card.ynow-eq-watch { border-left: 4px solid #d4a017; }
        .ynow-eq-matrix-card.ynow-eq-pass { border-left: 4px solid #27ae60; }
        .ynow-eq-matrix-card.ynow-eq-na { border-left: 4px solid #adb5bd; }
        .ynow-eq-matrix-h {
          display: flex; justify-content: space-between; align-items: flex-start;
          gap: 8px; margin-bottom: 4px;
        }
        .ynow-eq-matrix-metric { font-weight: 700; font-size: 12.5px; color: #222; }
        .ynow-eq-matrix-val { font-size: 16px; font-weight: 800; color: #0C5484; margin: 2px 0 4px; }
        .ynow-eq-matrix-note { margin: 0; font-size: 11.5px; color: #666; line-height: 1.4; }
        .ynow-eq-redflags {
          border: 1px solid #e0b4b0; border-radius: 6px;
          background: #fdf6f5; padding: 10px 12px 6px;
        }
        .ynow-eq-redflags h5 { margin: 0 0 6px 0; color: #c0392b; font-size: 13px; }
        .ynow-eq-redflags ul { margin: 0 0 6px 0; padding-left: 18px; font-size: 12.5px; color: #444; }

        /* Backtest：績效指標卡片（軟色調 + 左側色條，避免實心色塊） */
        .ynow-metric-grid {
          --ynow-metric-green: #2d8a57;
          --ynow-metric-green-tint: #eef7f1;
          --ynow-metric-red: #c0392b;
          --ynow-metric-red-tint: #faf0ef;
          --ynow-metric-violet: #5c5a8a;
          --ynow-metric-violet-tint: #f3f2f8;
          --ynow-metric-blue: #333333;
          --ynow-metric-blue-tint: #f2f2f2;
          --ynow-metric-amber: #b7791f;
          --ynow-metric-amber-tint: #faf6ee;
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 14px;
          margin: 0 0 4px 0;
        }
        @media (max-width: 992px) {
          .ynow-metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        }
        @media (max-width: 767px) {
          /* 手機兩欄（勿三欄過窄；與 FS／KPI grid 一致） */
          .ynow-metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 8px; }
          .ynow-metric-card__body { padding: 8px 8px 8px 8px; gap: 4px; }
          .ynow-metric-card__icon { width: 22px; height: 22px; font-size: 11px; border-radius: 5px; }
          .ynow-metric-card__value { font-size: clamp(11px, 3.4vw, 16px); }
          .ynow-metric-card__label { font-size: 9px; }
          .ynow-metric-card__caption { font-size: 8px; }
        }
        .ynow-metric-card {
          background: #ffffff;
          border: 1px solid #e6e8eb;
          border-radius: 10px;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
          overflow: hidden;
          display: flex;
          flex-direction: column;
          min-height: 0;
          transition: box-shadow 0.15s ease, border-color 0.15s ease;
        }
        .ynow-metric-card:hover {
          box-shadow: 0 3px 10px rgba(0, 0, 0, 0.08);
          border-color: #d5d9de;
        }
        .ynow-metric-card--green {
          border-left: 4px solid var(--ynow-metric-green);
          background: linear-gradient(180deg, var(--ynow-metric-green-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--red {
          border-left: 4px solid var(--ynow-metric-red);
          background: linear-gradient(180deg, var(--ynow-metric-red-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--violet {
          border-left: 4px solid var(--ynow-metric-violet);
          background: linear-gradient(180deg, var(--ynow-metric-violet-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--blue {
          border-left: 4px solid var(--ynow-metric-blue);
          background: linear-gradient(180deg, var(--ynow-metric-blue-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--amber {
          border-left: 4px solid var(--ynow-metric-amber);
          background: linear-gradient(180deg, var(--ynow-metric-amber-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--blue .ynow-metric-card__icon { background: var(--ynow-metric-blue); }
        .ynow-metric-card--amber .ynow-metric-card__icon { background: var(--ynow-metric-amber); }
        .ynow-metric-card--blue .ynow-metric-card__value { color: #1a1a1a; }
        .ynow-metric-card--amber .ynow-metric-card__value { color: #8a5a12; }
        /* 執行面板：避免 btn-block 蓋住下方說明文字 */
        .ynow-bt-run-panel .btn-block { margin-left: 0; margin-right: 0; }
        .ynow-metric-card__body {
          padding: 14px 16px 12px 16px;
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .ynow-metric-card__top {
          display: flex;
          align-items: center;
          gap: 10px;
        }
        .ynow-metric-card__icon {
          flex: 0 0 auto;
          width: 34px;
          height: 34px;
          border-radius: 8px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          font-size: 15px;
          color: #ffffff;
        }
        .ynow-metric-card--green .ynow-metric-card__icon { background: var(--ynow-metric-green); }
        .ynow-metric-card--red .ynow-metric-card__icon { background: var(--ynow-metric-red); }
        .ynow-metric-card--violet .ynow-metric-card__icon { background: var(--ynow-metric-violet); }
        .ynow-metric-card__label {
          font-size: 10px;
          font-weight: 600;
          color: #555555;
          line-height: 1.3;
          margin: 0;
        }
        .ynow-metric-card__value {
          font-size: clamp(18px, 4.2vw, 34px);
          font-weight: 800;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.02em;
          line-height: 1.1;
          margin: 0;
          color: #1a1a1a;
          overflow-wrap: anywhere;
          word-break: break-word;
        }
        .ynow-metric-card--green .ynow-metric-card__value { color: #1f5c3a; }
        .ynow-metric-card--red .ynow-metric-card__value { color: #8e2a20; }
        .ynow-metric-card--violet .ynow-metric-card__value { color: #3f3d62; }
        .ynow-metric-card__caption {
          margin: 0;
          font-size: 10px;
          color: #6b7280;
          line-height: 1.35;
        }

        /* 頂欄最右側：小圓標（無 wordmark）；幣別浮在 logo 正下方 */
        .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo,
        .main-header .navbar > #ynow-header-logo.ynow-header-logo {
          position: relative !important;
          height: 50px;
          display: flex !important;
          align-items: center;
          justify-content: center;
          float: left;
          list-style: none;
          padding: 0 14px 0 8px;
          margin: 0;
          overflow: visible !important;
        }
        .ynow-header-logo-mark {
          width: 36px;
          height: 36px;
          object-fit: contain;
          display: block;
          cursor: pointer;
        }
        .ynow-header-logo-mark:focus {
          outline: 2px solid rgba(255, 215, 0, 0.85);
          outline-offset: 2px;
        }
        body.ynow-market-tw .ynow-header-logo-mark {
          filter: drop-shadow(0 0 2px rgba(0,0,0,0.35));
        }

        /* 手機：單列頁首 — 標題與右上小 logo 垂直置中，夾在美股／台股與 logo 之間
           （黑色頁首置頂固定見全域 .main-header） */
        @media (max-width: 767px) {
          .main-header {
            min-height: 50px !important;
            max-height: 50px !important;
            height: 50px !important;
          }
          /* US + TW mobile: solid black bar so transparent navbar does not
             hide the centered title / progress fill. Desktop TW keeps flag. */
          body:not(.ynow-market-tw) .main-header,
          body:not(.ynow-market-tw).skin-black .main-header,
          body.ynow-market-tw .main-header,
          body.ynow-market-tw.skin-black .main-header {
            background-color: var(--ynow-ink) !important;
          }
          body.ynow-market-tw .main-header::before,
          body.ynow-market-tw .main-header::after {
            display: none !important;
            content: none !important;
            background: none !important;
            background-image: none !important;
          }
          body.ynow-market-tw .main-header .logo .ynow-app-title-fill-inner {
            text-shadow: none;
          }
          body.ynow-market-tw .main-header .navbar .nav > li > a,
          body.ynow-market-tw .ynow-market-header,
          body.ynow-market-tw .ynow-lang-header {
            text-shadow: none;
          }
          /* Collapse AdminLTE stacked logo+navbar (was ~100px) into one 50px bar.
             Title is full-bleed + viewport-centered; JS only sets title max-width
             so glyphs clear 美股／台股 and 繁中／EN (controls sit above via z-index). */
          .main-header .logo,
          .skin-black .main-header .logo,
          .skin-black .main-header .logo:hover {
            position: absolute !important;
            top: 0 !important;
            /* Full-bleed so title centers on the viewport; controls sit above via z-index */
            left: 0 !important;
            right: 0 !important;
            width: auto !important;
            max-width: none !important;
            height: 50px !important;
            min-height: 50px !important;
            line-height: 50px !important;
            padding: 0 8px !important;
            margin: 0 !important;
            float: none !important;
            display: flex !important;
            align-items: center !important;
            justify-content: center !important;
            text-align: center !important;
            background-color: transparent !important;
            background-image: none !important;
            /* Under transparent navbar so hamburger / 美股台股 / 繁中EN / mark stay on top */
            z-index: 1040 !important;
            overflow: visible !important;
            visibility: visible !important;
            opacity: 1 !important;
            pointer-events: none;
          }
          /* Opaque ::before on full-bleed title was painting over hamburger / 繁中／EN / mark */
          .skin-black .main-header .logo::before,
          body.ynow-market-tw .skin-black .main-header .logo::before {
            display: none !important;
            content: none !important;
            background: none !important;
          }
          /* Transparent navbar so title + gold progress fill show through the center gap.
             Interactive chrome (toggle / market / lang / mark) stays above via child z-index. */
          .main-header .navbar,
          .skin-black .main-header .navbar {
            position: relative !important;
            z-index: 1060 !important;
            background-color: transparent !important;
            background-image: none !important;
          }
          .main-header .logo .ynow-app-title {
            /* JS scales font into the center gap; no ellipsis — full version string stays */
            position: relative;
            font-size: 16px;
            line-height: 1.15 !important;
            white-space: nowrap;
            overflow: visible;
            text-overflow: clip;
            max-width: min(100%, calc(100vw - 200px));
            height: auto !important;
            visibility: visible !important;
            opacity: 1 !important;
          }
          .main-header .logo .ynow-app-title-base,
          .main-header .logo .ynow-app-title-fill,
          .main-header .logo .ynow-app-title-fill-inner {
            font-size: inherit;
            line-height: inherit;
            visibility: visible !important;
            opacity: 1 !important;
          }
          /* Keep progress-fill clip layer aligned while font is JS-scaled */
          .main-header .logo .ynow-app-title-fill {
            position: absolute;
            left: 0;
            top: 0;
            bottom: 0;
            width: var(--ynow-load-pct, 0%);
            max-width: 100%;
            overflow: hidden;
            white-space: nowrap;
            pointer-events: none;
          }
          /* Keep 繁中／EN / mark / toggle above the absolute title band */
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header {
            position: relative !important;
            z-index: 1062 !important;
            flex-shrink: 0 !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo {
            position: relative !important;
            z-index: 1062 !important;
            flex-shrink: 0 !important;
          }
          .main-header .navbar > .sidebar-toggle,
          .skin-black .main-header .navbar .sidebar-toggle {
            z-index: 1063 !important;
          }
          .main-header .navbar #ynow-market-header.ynow-market-header {
            z-index: 1062 !important;
          }
          /* Slightly compress 繁中／EN to free width for the centered title */
          .ynow-lang-btn {
            min-width: 28px !important;
            padding-left: 4px !important;
            padding-right: 4px !important;
            font-size: 10px !important;
            height: 26px !important;
            line-height: 24px !important;
          }
          .ynow-lang-stack {
            height: 26px !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header {
            padding: 0 4px 0 0 !important;
          }
          .main-header .navbar {
            min-height: 50px !important;
            height: 50px;
            width: 100% !important;
            margin-left: 0 !important;
            display: flex !important;
            align-items: center !important;
            overflow: visible !important;
          }
          .main-header .navbar > .sidebar-toggle,
          .skin-black .main-header .navbar .sidebar-toggle {
            height: 50px !important;
            min-height: 50px !important;
            max-height: 50px !important;
            line-height: 0 !important;
            padding: 0 !important;
            width: 44px !important;
            display: inline-flex !important;
            align-items: center !important;
            justify-content: center !important;
            float: none !important;
            margin: 0 !important;
          }
          .main-header .navbar #ynow-market-header.ynow-market-header {
            position: absolute !important;
            left: 44px !important; /* fallback；JS 依 toggle.offsetWidth 覆寫 */
            top: 0 !important;
            float: none !important;
            align-self: auto;
            margin: 0 !important;
            padding: 0 !important;
          }
          .main-header .navbar-custom-menu {
            float: none !important;
            margin-left: auto !important;
            height: 50px !important;
            display: flex !important;
            align-items: center !important;
            overflow: visible !important;
            position: relative !important;
            z-index: 1063 !important;
          }
          .main-header .navbar-custom-menu > .navbar-nav {
            display: flex !important;
            flex-direction: row;
            align-items: center !important;
            height: 50px !important;
            margin: 0 !important;
            overflow: visible !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header,
          .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo {
            float: none !important;
            height: 50px !important;
            min-height: 50px !important;
            display: flex !important;
            align-items: center !important;
            justify-content: center !important;
            margin: 0 !important;
            padding-top: 0 !important;
            padding-bottom: 0 !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li.ynow-lang-header {
            padding-left: 0 !important;
            padding-right: 4px !important;
          }
          .main-header .navbar-custom-menu .navbar-nav > li#ynow-header-logo.ynow-header-logo {
            position: relative !important;
            overflow: visible !important;
          }
          .ynow-ccy-float {
            right: 8px;
            margin-top: 8px;
            max-width: min(240px, calc(100vw - 16px));
          }
          .ynow-ccy-float .ynow-hdr-ccy-status,
          .ynow-ccy-float .ynow-hdr-ccy-status .shiny-text-output {
            max-width: min(200px, calc(100vw - 20px));
            white-space: normal;
            overflow-wrap: anywhere;
            word-break: break-word;
            line-height: 1.25;
            font-size: 9px;
          }
        }

        /* About：完整 LOGO（含文字）置頂品牌區 */
        .ynow-about-brand {
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          margin: 4px 0 18px 0;
          text-align: center;
        }
        .ynow-about-logo-full {
          width: min(240px, 55vw);
          height: auto;
          display: block;
        }

        /* About：中英左右對照簡介 */
        .ynow-about-bilingual {
          margin: 0 0 8px 0;
        }
        .ynow-about-col {
          padding-bottom: 8px;
        }
        .ynow-about-col--zh {
          border-right: 1px solid #e5e8eb;
        }
        @media (max-width: 991px) {
          .ynow-about-col--zh {
            border-right: none;
            border-bottom: 1px solid #e5e8eb;
            margin-bottom: 16px;
            padding-bottom: 16px;
          }
        }
        .ynow-about-title {
          margin: 0 0 12px 0;
          font-size: 22px;
          line-height: 1.3;
          color: #1a1a1a;
        }
        .ynow-about-lead,
        .ynow-about-method {
          font-size: 13.5px;
          line-height: 1.65;
          color: #333;
          margin: 0 0 12px 0;
        }
        .ynow-about-method {
          padding: 10px 12px;
          background: #f5f5f5;
          border-left: 4px solid #222222;
          border-radius: 0 4px 4px 0;
        }
        .ynow-about-feat-h {
          margin: 16px 0 8px 0;
          font-size: 15px;
          color: #222;
        }
        .ynow-about-feat {
          margin: 0;
          padding-left: 18px;
          font-size: 13px;
          line-height: 1.6;
          color: #444;
        }
        .ynow-about-feat > li {
          margin-bottom: 10px;
        }
        .ynow-about-feat > li > b {
          color: #1a1a1a;
        }
        .ynow-about-github {
          margin: 14px 0 0 0;
          font-size: 13px;
          line-height: 1.5;
          color: #444;
        }
        .ynow-about-github a {
          color: #0b57d0;
          word-break: break-all;
        }
        .ynow-legal-notices {
          margin: 20px 0 8px 0;
          padding-top: 8px;
        }
        .ynow-legal-notices .ynow-about-section-title {
          margin: 0 0 14px 0;
          font-size: 18px;
          line-height: 1.35;
          color: #1a1a1a;
        }
        .ynow-legal-row {
          margin: 0 0 12px 0;
        }
        .ynow-legal-h {
          margin: 0 0 6px 0;
          font-size: 14px;
          font-weight: 650;
          color: #222;
        }
        .ynow-legal-body {
          margin: 0 0 10px 0;
          font-size: 12.5px;
          line-height: 1.6;
          color: #555;
        }
        /* About 後續區塊：標題／內文與「關於 The YNow App」同左緣 */
        #shiny-tab-about .ynow-about-section-title,
        #shiny-tab-about .ynow-about-section-lead {
          margin-left: 0;
          padding-left: 0;
        }
        #shiny-tab-about .ynow-about-section-title {
          margin-top: 0;
        }
        #shiny-tab-about .ynow-about-section > ul {
          margin-left: 0;
        }

        /* Backtest：策略參數 tabBox 輕量潤飾 */
        .ynow-bt-params .nav-tabs-custom > .nav-tabs {
          border-bottom-color: #e5e8eb;
        }
        .ynow-bt-params .nav-tabs-custom > .nav-tabs > li > a {
          border-radius: 6px 6px 0 0;
          font-size: 12.5px;
          font-weight: 600;
        }
        .ynow-bt-params .nav-tabs-custom > .tab-content {
          background: #fafbfc;
          border: 1px solid #e8ecef;
          border-top: 0;
          border-radius: 0 0 8px 8px;
          padding: 14px 16px 10px;
        }
        .ynow-bt-params .form-group {
          background: #ffffff;
          border: 1px solid #e8ecef;
          border-radius: 8px;
          padding: 10px 12px 6px;
          margin-bottom: 10px;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
        }
        .ynow-bt-params .form-group > label {
          font-size: 12.5px;
          font-weight: 600;
          color: #333;
        }

        /* 僅美化：情緒策略參數（寬螢幕四參數一列；按鈕獨立列） */
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-sm-3,
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-sm-4,
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-sm-6,
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-sm-12,
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > .col-xs-12 {
          margin-bottom: 8px;
        }
        .ynow-bt-mode-b .ynow-bt-fit-row {
          clear: both;
          margin-top: 4px;
          padding-top: 12px;
          border-top: 1px solid #eee0b8;
        }
        .ynow-bt-mode-b .ynow-bt-fit-panel {
          padding: 12px 14px;
          background: #fcf8e3;
          border: 1px solid #f0e6b2;
          border-radius: 5px;
          font-size: 12px;
          color: #8a6d3b;
          line-height: 1.5;
        }
        .ynow-bt-mode-b .ynow-bt-fit-panel .btn {
          max-width: 320px;
        }

        /* 僅美化：回測驗證（寬螢幕三列） */
        .ynow-bt-validate .ynow-bt-validate-col {
          margin-bottom: 12px;
        }
        .ynow-bt-validate .ynow-bt-validate-col > h5 {
          margin: 0 0 6px 0;
          font-size: 13px;
        }
        .ynow-bt-validate .ynow-bt-validate-panel {
          height: 100%;
          padding: 10px 12px;
          background: #fafbfc;
          border: 1px solid #e8ecef;
          border-radius: 6px;
        }
        .ynow-bt-validate .ynow-bt-validate-panel .table {
          margin-bottom: 0;
          font-size: 12px;
        }

        /* Decision SOP — compact coach on Decision Checklist */
        .ynow-dc-sop--compact {
          margin: 0 0 14px 0;
          padding: 12px 14px;
          background: #fafbfc;
          border: 1px solid #e5e7eb;
          border-radius: 8px;
        }
        .ynow-dc-sop-head {
          margin: 0 0 10px 0;
        }
        .ynow-dc-sop-title {
          margin: 0 0 4px 0;
          font-size: 15px;
          font-weight: 700;
          color: #1f2937;
        }
        .ynow-dc-sop-intro {
          font-size: 12.5px;
          line-height: 1.45;
          color: #6b7280;
          margin: 0;
        }
        .ynow-dc-sop-reloc-note {
          font-size: 12.5px;
          line-height: 1.45;
          color: #6b7280;
          margin: 0 0 14px 0;
          padding: 8px 10px;
          background: #f8fafc;
          border-left: 3px solid #94a3b8;
          border-radius: 4px;
        }
        .ynow-dc-sop-steps {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 10px;
          margin: 0;
          padding: 0;
        }
        @media (max-width: 991px) {
          .ynow-dc-sop-steps {
            grid-template-columns: 1fr;
          }
        }
        .ynow-dc-sop-step {
          margin: 0;
          padding: 10px 12px;
          background: #fff;
          border: 1px solid #e5e7eb;
          border-radius: 6px;
          list-style: none;
        }
        .ynow-dc-sop-step-top {
          display: flex;
          align-items: center;
          gap: 8px;
          margin: 0 0 8px 0;
        }
        .ynow-dc-sop-step-num {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 20px;
          height: 20px;
          border-radius: 50%;
          background: #1f2937;
          color: #fff;
          font-size: 11px;
          font-weight: 700;
          flex: 0 0 auto;
        }
        .ynow-dc-sop-step-title {
          font-size: 13px;
          font-weight: 700;
          color: #111827;
          line-height: 1.3;
        }
        .ynow-dc-sop-step-body {
          margin: 0 0 8px 0;
        }
        .ynow-dc-sop-chips {
          display: flex;
          flex-wrap: wrap;
          gap: 6px;
        }
        .ynow-dc-sop-chip {
          display: inline-block;
          font-size: 11.5px;
          line-height: 1.3;
          color: #374151;
          background: #f3f4f6;
          border-radius: 999px;
          padding: 3px 8px;
        }
        .ynow-dc-sop-step .checkbox {
          margin: 0;
        }
        .ynow-dc-sop-step .checkbox label {
          font-size: 12.5px;
          font-weight: 500;
          color: #1f2937;
        }
        .ynow-dc-sop-banner {
          margin-top: 10px;
          padding: 8px 10px;
          border-radius: 6px;
          font-size: 12.5px;
          font-weight: 600;
          display: flex;
          align-items: center;
          gap: 8px;
        }
        .ynow-dc-sop-banner.is-locked {
          background: #fef2f2;
          color: #b91c1c;
          border-left: 3px solid #b91c1c;
        }
        .ynow-dc-sop-banner.is-unlocked {
          background: #ecfdf5;
          color: #047857;
          border-left: 3px solid #047857;
        }
        .ynow-sop-verdict-locked {
          border-left: 4px solid #6c757d;
          margin-top: 0;
        }

        /* Decision Checklist — summary on top; theme items side-by-side */
        .ynow-dc-summary-top {
          width: 100%;
        }
        .ynow-dc-summary-top .ynow-dc-live {
          width: 100%;
        }
        .ynow-dc-summary-grid {
          display: flex;
          flex-wrap: wrap;
          gap: 10px;
          width: 100%;
          align-items: stretch;
        }
        .ynow-dc-summary-grid > .ynow-dc-row,
        .ynow-dc-summary-grid > .ynow-dc-row-skip {
          flex: 1 1 calc(50% - 10px);
          min-width: 260px;
          box-sizing: border-box;
        }
        .ynow-dc-section {
          margin: 0 0 18px 0;
        }
        .ynow-dc-section:last-child {
          margin-bottom: 0;
        }
        .ynow-dc-section-title {
          margin: 0 0 10px 0;
          padding: 0 0 6px 0;
          border-bottom: 1px solid #dee2e6;
          font-size: 13px;
          font-weight: 700;
          color: #1a1a1a;
          letter-spacing: 0.02em;
        }
        .ynow-dc-section-items {
          display: flex;
          flex-wrap: wrap;
          gap: 12px;
          align-items: stretch;
          width: 100%;
        }
        .ynow-dc-item {
          flex: 1 1 calc(50% - 12px);
          min-width: 260px;
          margin: 0;
          padding: 10px 12px;
          border: 1px solid #e9ecef;
          border-radius: 4px;
          background: #fff;
          box-sizing: border-box;
        }
        .ynow-dc-item--default {
          border-color: #b8dae0;
          background: #f7fbfc;
        }
        .ynow-dc-panel-hint {
          margin: 0 0 14px 0;
          font-size: 12.5px;
          color: #555;
          line-height: 1.5;
        }
        .ynow-dc-hint {
          margin: 2px 0 0 26px;
          font-size: 11.5px;
          color: #6c757d;
          line-height: 1.45;
        }
        .ynow-dc-cond-hint {
          margin: -4px 0 10px 0;
          font-size: 11px;
          color: #6c757d;
          line-height: 1.4;
        }
        .ynow-dc-conds .form-group {
          margin-bottom: 6px;
        }
        .ynow-dc-badge {
          margin-left: 6px;
          padding: 1px 6px;
          font-size: 10px;
          font-weight: 700;
          color: #0c5460;
          background: #d1ecf1;
          border-radius: 3px;
          white-space: nowrap;
        }
        .ynow-dc-conds {
          margin: 8px 0 0 26px;
          max-width: 100%;
        }
        .ynow-dc-label-wrap {
          display: inline;
        }
        @media (max-width: 767px) {
          .ynow-dc-item,
          .ynow-dc-summary-grid > .ynow-dc-row,
          .ynow-dc-summary-grid > .ynow-dc-row-skip {
            flex-basis: 100%;
            min-width: 0;
          }
          .ynow-dc-conds { margin-left: 0; max-width: 100%; }
          .ynow-dc-hint { margin-left: 0; }
          .ynow-dc-cond-hint { margin-left: 0; }
        }

        .ynow-bt-hfv-wrap {
          position: relative;
        }
        .ynow-bt-hfv-controls {
          position: static;
          display: flex;
          flex-wrap: wrap;
          align-items: flex-end;
          gap: 8px 28px;
          width: 100%;
          max-width: none;
          min-width: 0;
          margin: 0 0 12px 0;
          padding: 8px 12px 10px 12px;
          background: #f5f5f5;
          border: 1px solid #e4eaef;
          border-radius: 6px;
          box-shadow: none;
          font-size: 12px;
          box-sizing: border-box;
          overflow: visible;
        }
        .ynow-bt-hfv-bench {
          flex: 0 0 auto;
          padding-bottom: 2px;
        }
        .ynow-bt-hfv-models {
          flex: 1 1 280px;
          min-width: 0;
        }
        .ynow-bt-hfv-controls .form-group {
          margin-bottom: 0;
        }
        .ynow-bt-hfv-controls .control-label {
          display: block;
          font-size: 12px;
          font-weight: 600;
          margin-top: 0;
          margin-bottom: 6px;
          line-height: 1.4;
          white-space: normal;
        }
        .ynow-bt-hfv-controls .shiny-options-group {
          margin-top: 0;
          clear: none;
          display: flex;
          flex-wrap: wrap;
          column-gap: 18px;
          row-gap: 4px;
          align-items: center;
          padding-left: 0;
        }
        .ynow-bt-hfv-controls .radio {
          margin-top: 0;
          margin-bottom: 4px;
        }
        .ynow-bt-hfv-controls .radio label {
          font-size: 12px;
          font-weight: normal;
          line-height: 1.35;
        }
        .ynow-bt-hfv-controls .checkbox,
        .ynow-bt-hfv-controls .checkbox-inline {
          margin-top: 0;
          margin-bottom: 0;
          min-height: 18px;
          padding-left: 0;
        }
        .ynow-bt-hfv-controls .checkbox label,
        .ynow-bt-hfv-controls .checkbox-inline {
          font-size: 12px;
          font-weight: normal;
          line-height: 1.35;
          padding-left: 20px;
          display: inline-block;
          min-height: 18px;
        }
        .ynow-bt-hfv-controls .checkbox input[type='checkbox'],
        .ynow-bt-hfv-controls .checkbox-inline input[type='checkbox'] {
          position: absolute;
          margin-left: -20px;
          margin-top: 2px;
        }
        @media (max-width: 767px) {
          .ynow-bt-hfv-controls {
            position: static;
            max-width: none;
            margin-top: 0;
          }
        }
        @media (max-width: 991px) {
          .ynow-bt-mode-b .ynow-bt-fit-panel .btn {
            max-width: none;
            width: 100%;
          }
        }
        
        /* 針對 search_results (產業資訊) 進行黑白主題與字體縮小；滿寬 */
        #search_results {
          width: 100% !important;
          display: block !important;
          box-sizing: border-box !important;
          white-space: pre-wrap !important;
          background-color: #1e1e1e !important;  /* 深黑色背景 */
          color: #eeeeee !important;             /* 淺白色文字 */
          font-size: 12px !important;            /* 縮小字體 */
          border: 1px solid #444444 !important;  /* 加上細緻的暗色邊框 */
          padding: 8px 12px !important;          /* 調整內邊距讓它扁平一點 */
          border-radius: 4px !important;         /* 圓角 */
          font-weight: 500 !important;
          line-height: 1.2 !important;
        }
        #search_results pre {
          width: 100% !important;
          display: block !important;
          margin: 0 !important;
          white-space: pre-wrap !important;
          background: transparent !important;
          border: none !important;
          color: inherit !important;
        }
      "))
    ),
    
    # ==========================================
    # 獨立的 sc 搜尋輸入框與按鈕區塊
    # ==========================================
    # Author credit: same visual row as USD/TWD (document flow, not fixed/sticky)
    tags$div(
      class = "ynow-hdr-subband",
      tags$div(
        class = "ynow-credit-flow",
        tags$span(class = "ynow-credit-text", "a lawrence kuo shiny app")
      )
    ),
    # Ticker search + Yahoo industry chrome: hide on Home, About, Macro, Blue Chip, Testing
    # (Business Breakdown shares global Ticker / Stock Code chrome;
    # Blue Chip uses its own universe Search, not the global ticker chrome).
    conditionalPanel(
      condition = "input.sidebar_tabs != 'about' && input.sidebar_tabs != 'macro_market' && input.sidebar_tabs != 'asset_transmission' && input.sidebar_tabs != 'bluechip' && input.sidebar_tabs != 'testing' && input.sidebar_tabs != 'home'",
      fluidRow(
        column(width = 12,
               div(
                 class = "ynow-sc-row",
                 div(
                   class = "ynow-sc-wrap",
                   textInput("sc", "Ticker / Stock Code", value = APP_DEFAULTS$stock_code),
                   uiOutput("sc_ticker_suggest_ui")
                 ),
                 actionButton(
                   "search",
                   "Search",
                   icon = icon("search"),
                   class = "ynow-search-btn"
                 )
               )
        )
      ),
      fluidRow(
        column(
          width = 12,
          h2(uiOutput("txt_corpname", inline = TRUE), class = "ynow-corpname")
        )
      ),
      fluidRow(
        column(
          width = 12,
          uiOutput("ynow_data_gap_banner")
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$div(
            style = "width: 100%; text-align: left; margin-top: 8px;",
            tags$p(
              id = "ynow_industry_info_yahoo",
              "industry info from Yahoo",
              style = "font-size: 12px; color: #888; margin: 0 0 4px 0; font-weight: bold;"
            ),
            verbatimTextOutput("search_results")
          )
        )
      ),
      br()
    ),
    # Shared ticker typeahead (Ticker/Stock Code + Radar focus); always loaded
    tags$script(HTML("
      (function() {
        if (document.documentElement.getAttribute('data-ynow-ticker-typeahead') === '1') return;
        document.documentElement.setAttribute('data-ynow-ticker-typeahead', '1');
        var typingOpen = {};

        function suggestIdForInput(inputId) {
          if (inputId === 'lab_cluster_focus') return 'lab_cluster_focus_suggest';
          return 'sc_ticker_suggest';
        }

        function inputValue(inputId) {
          var inp = document.getElementById(inputId);
          return inp ? (inp.value || '') : '';
        }

        function showSuggest(inputId) {
          var el = document.getElementById(suggestIdForInput(inputId));
          if (!el) return;
          var q = inputValue(inputId).trim();
          if (typingOpen[inputId] && q.length > 0 && el.children.length) {
            el.style.display = 'block';
          } else {
            el.style.display = 'none';
          }
        }

        function hideSuggest(inputId) {
          if (inputId) {
            typingOpen[inputId] = false;
            var el = document.getElementById(suggestIdForInput(inputId));
            if (el) el.style.display = 'none';
            return;
          }
          ['sc', 'lab_cluster_focus'].forEach(hideSuggest);
        }

        function bindTypeahead(inputId) {
          $(document).on('input', '#' + inputId, function() {
            var v = $(this).val() || '';
            Shiny.setInputValue('ticker_typeahead', v, {priority: 'event'});
            typingOpen[inputId] = v.trim().length > 0;
            showSuggest(inputId);
          });

          $(document).on('blur', '#' + inputId, function(e) {
            var rt = e.relatedTarget;
            var el = document.getElementById(suggestIdForInput(inputId));
            if (el && rt && el.contains(rt)) return;
            hideSuggest(inputId);
          });

          $(document).on('keydown', '#' + inputId, function(e) {
            if (e.key === 'Enter' || e.keyCode === 13) hideSuggest(inputId);
          });
        }

        bindTypeahead('sc');
        bindTypeahead('lab_cluster_focus');

        $(document).on('click', '#search', function() {
          hideSuggest('sc');
        });

        $(document).on('mousedown', '#sc_ticker_suggest .ynow-suggest-item, #lab_cluster_focus_suggest .ynow-suggest-item', function(e) {
          e.preventDefault();
          var sym = $(this).data('symbol');
          var list = this.closest ? this.closest('.ynow-ticker-suggest, #sc_ticker_suggest, #lab_cluster_focus_suggest') : null;
          var inputId = (list && list.id === 'lab_cluster_focus_suggest') ? 'lab_cluster_focus' : 'sc';
          hideSuggest(inputId);
          if (sym) {
            $('#' + inputId).val(sym).trigger('change');
            Shiny.setInputValue(inputId, sym, {priority: 'event'});
          }
        });

        $(document).on('shiny:value', function(e) {
          if (e.name === 'sc_ticker_suggest_ui') setTimeout(function() { showSuggest('sc'); }, 0);
          if (e.name === 'lab_cluster_focus_suggest_ui') setTimeout(function() { showSuggest('lab_cluster_focus'); }, 0);
        });
      })();
    ")),
    # Header KPIs: Company (dashboard) only (Last Price / Market Cap / EPS TTM).
    # Smart Analysis (Lite) uses its own fair-value cards — no quote KPI strip.
    conditionalPanel(
      condition = "input.sidebar_tabs == 'dashboard'",
      fluidRow(
        class = "ynow-header-kpi-row",
        column(
          width = 4,
          class = "col-xs-12",
          infoBoxOutput("ibx_stockprice", width = NULL)
        ),
        column(
          width = 4,
          class = "col-xs-12",
          infoBoxOutput("ibx_marketcap", width = NULL)
        ),
        column(
          width = 4,
          class = "col-xs-12",
          infoBoxOutput("ibx_EPS", width = NULL)
        )
      )
    ),
    # Model pages + Smart Analysis: full composite valuation block (shared output) + forecast years
    # Lite Smart Analysis: explanatory blurb sits immediately above composite status (no page heading).
    conditionalPanel(
      condition = paste(
        "input.sidebar_tabs == 'smart_analysis' ||",
        "input.sidebar_tabs == 'dcf_calculator' ||",
        "input.sidebar_tabs == 'ddm_calculator' ||",
        "input.sidebar_tabs == 'pb_calculator' ||",
        "input.sidebar_tabs == 'rel_multiples_calculator' ||",
        "input.sidebar_tabs == 'sotp_calculator' ||",
        "input.sidebar_tabs == 'ri_calculator' ||",
        "input.sidebar_tabs == 'nav_calculator'"
      ),
      conditionalPanel(
        condition = "input.sidebar_tabs == 'smart_analysis'",
        tags$div(
          class = "ynow-lite-only ynow-smart-lite-blurb-wrap",
          tags$p(
            id = "ynow_smart_page_sub",
            class = "ynow-smart-lite-blurb",
            paste0(
              "Auto-selects primary and secondary valuation models from the ticker profile, ",
              "detects the best parameter scenario (Two-Stage vs Gordon, SGR method, claim), ",
              "applies those defaults, and shows fair-value charts. No manual model settings."
            )
          )
        )
      ),
      fluidRow(
        class = "ynow-header-composite-row",
        column(
          width = 12,
          class = "col-xs-12",
          decision_valuation_compare_ui("main_decision")
        )
      ),
      # DCF-Model：模型選擇／採用現金流 與下方 預測年數 n／建議 同為 6+6 靠左對齊
      conditionalPanel(
        condition = "input.sidebar_tabs == 'dcf_calculator'",
        fluidRow(
          class = "ynow-dcf-mode-row",
          column(
            width = 6,
            class = "col-xs-12 col-sm-6",
            tags$div(
              class = "ynow-dcf-mode-col",
              radioButtons(
                "dcf_mode", "選擇 DCF 估值模型：",
                choices = list(
                  "明確預測 + Gordon 終值" = "gordon",
                  "二階段成長法 (Two-Stage Model)" = "two_stage"
                ),
                selected = APP_DEFAULTS$dcf_mode
              )
            )
          ),
          column(
            width = 6,
            class = "col-xs-12 col-sm-6",
            tags$div(
              class = "ynow-dcf-claim-col",
              radioButtons(
                "dcf_claim",
                "採用現金流",
                choices = list(
                  "FCFF（WACC，再橋接股權）" = "fcff",
                  "FCFE（Ke，直接股權）" = "fcfe"
                ),
                selected = APP_DEFAULTS$dcf_claim,
                inline = TRUE
              ),
              helpText("FCFE = FCFF − 稅後利息 + 淨舉債（負債隨 g 成長）；以 Ke 折現，不再減負債。")
            )
          )
        )
      ),
      # 預測年數 n：僅 DCF 明確預測期使用。
      # DDM／RI 各自有 n1／ri_years；P/B／NAV／Multiples／SOTP 不涉及年數假設。
      # Lite Smart Analysis：不顯示手動模型參數（含預測年數）
      conditionalPanel(
        condition = "input.sidebar_tabs == 'dcf_calculator'",
        fluidRow(
          class = "ynow-header-years-suggest-row",
          column(
            width = 6,
            class = "col-xs-12 col-sm-6",
            tags$div(
              class = "ynow-header-years",
              numericInput(
                "years", "預測年數 n",
                value = APP_DEFAULTS$years, min = 1, max = 30
              )
            )
          ),
          column(
            width = 6,
            class = "col-xs-12 col-sm-6",
            tags$div(
              class = "ynow-dcf-claim-suggest-wrap",
              uiOutput("dcf_claim_suggest")
            )
          )
        )
      )
    ),
    
    tabItems(
      tabItem(
        tabName = "home",
        .home_brand_ui()
      ),
      tabItem(
        tabName = "get_started",
        fluidRow(
          box(
            title = tagList(
              icon("route"),
              tags$span(id = "ynow_gs_model_selector_title", "Model Selector｜Valuation model recommendation")
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            uiOutput("get_started_model_selector"),
            .model_selector_dimensions_annotation_ui()
          )
        ),
        # SGR 與 BETA 同層、同 col-sm-12（勿只對 SGR 外包 fluidRow，否則欄寬會不一致）
        fluidRow(
          .dcf_core_params_box()
        ),
        fluidRow(
          tabBox(
            title = "BETA",
            width = 12,
            tabPanel(
              "Beta Overview",
              icon = icon("th-large"),
              helpText(
                id = "ynow_gs_beta_overview_help",
                paste0(
                  "Intrinsic-value path: Summary β is written into CAPM by default; ",
                  "you can switch to industry / Bottom-Up / unlevered / manual. ",
                  "Rolling estimates are for cross-check only and are not written into CAPM."
                )
              ),
              beta_overview_section_ui()
            ),
            tabPanel(
              "Peer Unlever",
              value = "peer_unlever",
              icon = icon("users"),
              helpText(
                id = "ynow_gs_peer_unlever_help",
                paste0(
                  "Left: enter peers, unlever, then average (Bottom-Up). ",
                  "Right: this firm's Hamada unlever and manual βe. ",
                  "To write into CAPM: pick the matching β source on Beta Overview or DCF → Beta."
                )
              ),
              beta_unlever_section_ui()
            ),
            tabPanel(
              "Rolling β",
              icon = icon("chart-area"),
              helpText(
                id = "ynow_gs_rolling_help",
                paste0(
                  "Use Rolling β to cross-check valuation β (includes sentiment / event noise). ",
                  "It is not written into CAPM; if it diverges sharply from Bottom-Up βᵤ, ",
                  "review peers, capital structure, events, and liquidity."
                )
              ),
              beta_rolling_section_ui()
            )
          )
        ),
        # Valuation Methodology at bottom of Basic Setup (Full only; Lite hides get_started)
        tags$div(
          class = "ynow-full-only",
          style = "margin-top: 20px;",
          tags$hr(style = "margin: 8px 0 20px 0; border-color: #e5e8eb;"),
          .valuation_methodology_section_ui(collapsible = FALSE, collapsed = FALSE)
        )
      ),

      tabItem(
        tabName = "snapshot",
        h2(tags$span(id = "ynow_snapshot_page_title", "Snapshot")),
        tags$p(
          id = "ynow_snapshot_page_help",
          class = "help-block ynow-full-only",
          paste0(
            "Three tabs: manual adjustments vs the post-Search baseline (with annotated PDF); ",
            "current live parameters (download / upload restore CSV); and APP_DEFAULTS."
          )
        ),
        tags$p(
          id = "ynow_snapshot_page_help_lite",
          class = "help-block ynow-lite-only",
          paste0(
            "Lite shows only System defaults for parameters you can set in Lite ",
            "(Dashboard industry / currency / default ticker, Blue Chip ranking and Clustering). ",
            "Manual audit and live snapshot restore stay in Full mode."
          )
        ),
        tabBox(
          # No right-side chrome title (page h2 already says Snapshot; avoids wrong remaps)
          id = "snapshot_report",
          width = "auto",

          tabPanel(
            title = tagList(
              icon("user-edit"),
              tags$span(id = "ynow_snapshot_tab_audit", "Manual adjustments (vs post-Search baseline)")
            ),
            value = "snap_audit",
            tags$p(
              id = "ynow_param_audit_help",
              style = "font-size:12.5px; color:#666; line-height:1.45; margin:0 0 10px 0;",
              paste0(
                "Baseline locks after Search and statement auto-fill. ",
                "Later manual overrides are listed by page. ",
                "Use Go & highlight to jump and frame the input. ",
                "Below: select pages and generate an annotated screenshot PDF."
              )
            ),
            uiOutput("param_audit_report"),
            tags$hr(style = "margin:14px 0;"),
            tags$h4(
              id = "ynow_param_audit_pdf_title",
              style = "margin:0 0 8px 0; font-size:15px; font-weight:700;",
              "Annotated page screenshots (PDF)"
            ),
            tags$p(
              id = "ynow_param_audit_pdf_help",
              style = "font-size:12.5px; color:#666; line-height:1.45; margin:0 0 10px 0;",
              paste0(
                "Select main pages to include. The app switches to each page, captures the live layout, ",
                "draws boxes on inputs you changed vs the post-Search baseline, and downloads one PDF."
              )
            ),
            checkboxGroupInput(
              "param_audit_pdf_pages",
              label = tags$span(id = "ynow_param_audit_pdf_pages_label", "Pages to capture"),
              choiceNames = list(
                tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_basic",
                          "Basic Setup (SGR / CAPM / WACC / Beta)"),
                tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_dcf", "DCF"),
                tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_ddm", "DDM"),
                tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_ri", "RI"),
                tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_pb", "P/B"),
                tags$span(class = "ynow-pdf-page-lab", `data-key` = "param_audit_pdf_page_nav", "NAV")
              ),
              choiceValues = c(
                "get_started", "dcf_calculator", "ddm_calculator",
                "ri_calculator", "pb_calculator", "nav_calculator"
              ),
              selected = c(
                "get_started", "dcf_calculator", "ddm_calculator",
                "ri_calculator", "pb_calculator", "nav_calculator"
              )
            ),
            div(
              style = "display:flex; flex-wrap:wrap; gap:10px; align-items:center; margin-top:8px;",
              actionButton(
                "param_audit_pdf_go",
                tagList(icon("file-pdf"), tags$span(id = "ynow_param_audit_pdf_btn", "Generate annotated PDF")),
                class = "btn-success"
              ),
              tags$span(
                id = "ynow_param_audit_pdf_status",
                style = "font-size:12.5px; color:#555;"
              )
            )
          ),

          tabPanel(
            title = tagList(
              icon("camera"),
              tags$span(id = "ynow_snapshot_tab_current", "Current App Parameter Snapshot")
            ),
            value = "snap_current",
            tags$p(
              id = "ynow_param_restore_help",
              style = "font-size:12.5px; color:#666; line-height:1.45; margin:0 0 10px 0;",
              paste0(
                "Download a restore CSV to save your current valuation inputs. ",
                "Later, upload that file and click Restore to write the values back into the App, ",
                "then continue DCF / DDM / RI / P/B / NAV analysis. ",
                "Prefer Search (load statements) first when the ticker differs."
              )
            ),
            div(
              style = "display:flex; flex-wrap:wrap; justify-content:space-between; align-items:center; gap:12px; margin-bottom:10px;",
              uiOutput("snapshot_timestamp"),
              div(
                style = "display:flex; flex-wrap:wrap; gap:8px; align-items:center;",
                downloadButton(
                  "download_snapshot",
                  tags$span(id = "ynow_download_snapshot_btn", "Download Snapshot CSV"),
                  icon = icon("download")
                ),
                downloadButton(
                  "download_param_restore",
                  tags$span(id = "ynow_download_param_restore_btn", "Download restore CSV"),
                  icon = icon("save"),
                  class = "btn-primary"
                )
              )
            ),
            tags$div(
              class = "ynow-param-restore-box",
              style = "margin:0 0 14px 0; padding:12px 14px; background:#f7f9fc; border:1px solid #e2e8f0; border-radius:6px; border-left:4px solid #3c8dbc;",
              tags$h4(
                id = "ynow_param_restore_title",
                style = "margin:0 0 8px 0; font-size:15px; font-weight:700;",
                "Restore parameters from file"
              ),
              fluidRow(
                column(
                  width = 7,
                  fileInput(
                    "param_restore_file",
                    label = tags$span(id = "ynow_param_restore_file_label", "Upload restore CSV"),
                    accept = c(".csv", "text/csv", "text/comma-separated-values"),
                    buttonLabel = "Browse…",
                    placeholder = "No file selected"
                  )
                ),
                column(
                  width = 5,
                  tags$div(
                    style = "margin-top:24px;",
                    actionButton(
                      "param_restore_go",
                      tagList(
                        icon("upload"),
                        tags$span(id = "ynow_param_restore_btn", "Restore parameters")
                      ),
                      class = "btn-success"
                    )
                  )
                )
              ),
              tags$span(
                id = "ynow_param_restore_status",
                style = "font-size:12.5px; color:#555;"
              ),
              uiOutput("param_restore_status_ui")
            ),
            dataTableOutput("snapshot_table")
          ),

          tabPanel(
            title = tagList(
              icon("sliders-h"),
              tags$span(id = "ynow_snapshot_tab_defaults", "System defaults (APP_DEFAULTS)")
            ),
            value = "snap_defaults",
            div(
              style = "display:flex; justify-content:space-between; align-items:center; gap:12px; margin-bottom:10px; flex-wrap:wrap;",
              tags$span(
                id = "ynow_snapshot_defaults_help",
                class = "ynow-full-only",
                style = "font-size:12.5px; color:#666; line-height:1.45;",
                paste0(
                  "Defaults written at App start (including items estimated from the default industry / Rf). ",
                  "May differ from Current App Parameter Snapshot; fields can still be overridden on each page."
                )
              ),
              tags$span(
                id = "ynow_snapshot_defaults_help_lite",
                class = "ynow-lite-only",
                style = "font-size:12.5px; color:#666; line-height:1.45;",
                paste0(
                  "Only parameters Lite can set (Dashboard industry / currency / default ticker, ",
                  "plus Blue Chip ranking and Clustering). Smart Analysis engine seeds and Full-only ",
                  "model parameters are hidden here."
                )
              ),
              downloadButton(
                "download_defaults",
                tags$span(id = "ynow_download_defaults_btn", "Download Defaults CSV"),
                icon = icon("download")
              )
            ),
            dataTableOutput("defaults_table")
          )
        )
      ),

      tabItem(
        tabName = "macro_market",
        uiOutput("ynow_lazy_host_macro_market")
      ),

      tabItem(tabName = "dashboard",

              tags$div(
                class = "ynow-full-only",
              tabBox(title = "FINANCIAL REPORT",
                     id = "dashboard_fin_report",
                     width = "auto",
                     
                     tabPanel("Finance Summary",
                              p("This section imports Finance Summaries from Yahoo Finance",
                                style = "margin-bottom: 12px; color: #666; font-size: 13px;"),
                              uiOutput("fs_summary_ui"),
                              downloadButton('FS_download', "Download Finance Summary")
                     ),
                     
                     tabPanel(
                       "Income Statement",
                       icon = icon("chart-line"),
                       p("This section imports Income Statements from Yahoo Finance"),
                       
                       # 🌟 新增：Income Statement 下拉選單與互動圖表
                       selectInput("is_type", "Select Income Statement Metric",
                                   choices = c("Total Revenue", "Gross Profit", "EBITDA")),
                       tags$div(
                         class = "ynow-fs-chart ynow-fs-chart-is",
                         tags$div(
                           class = "ynow-fs-chart-heading",
                           icon("chart-line"),
                           tags$span(id = "ynow_fs_is_chart_heading", "Income Statement")
                         ),
                         plotlyOutput("is_plot")
                       ),
                       tags$hr(),
                       
                       DT::dataTableOutput("tbIncomeStatement"), 
                       downloadButton('IS_download', "Download Income Statement")
                     ),
                     
                     tabPanel("Balance Sheet",
                              p("This section imports Balance Sheets from Yahoo Finance"),
                              plotlyOutput("bs_plot", height = "380px"),
                              tags$hr(),
                              DT::dataTableOutput("tbBalanceSheet"),
                              downloadButton('BS_download', "Download Balance Sheet")
                     ),
                     
                     tabPanel(
                       "Cash Flow",
                       icon = icon("money-bill-wave"),
                       p("This section imports Cash Flow data from Yahoo Finance"),
                       tags$div(
                         class = "ynow-fs-chart ynow-fs-chart-cf",
                         tags$div(
                           class = "ynow-fs-chart-heading",
                           icon("money-bill-wave"),
                           tags$span(id = "ynow_fs_cf_chart_heading", "Cash Flow")
                         ),
                         plotlyOutput("cf_plot", height = "460px") %>% withSpinner()
                       ),
                       tags$hr(),
                       DT::dataTableOutput("tbCashFlow"),
                       downloadButton('CF_download', "Download Cash Flow Data")
                     ),

                     # 財報附註擷取 (SEC EDGAR) — 僅美股模式顯示（server 以 shinyjs 控制）
                     tabPanel(
                       title = "財報附註 (SEC)",
                       value = "sec_notes",
                       icon = icon("file-alt"),
                       id = "sec_notes_panel",
                       p(
                         "直接從美國 SEC EDGAR 擷取指定美股的",
                         tags$b("最新年報（10-K／20-F／40-F）、季報（10-Q）或重大訊息（8-K／6-K）"),
                         "：年報／季報抽出「財務報表附註 (Notes)」；重大訊息則盡力萃取最新一則正文。",
                         tags$br(),
                         tags$span(
                           style = "color:#888;",
                           "資料來源：SEC EDGAR。僅支援美股（含 ADR／外國發行人：年報 20-F、重大訊息 6-K）；台股模式請改用市場切換至美股後使用。"
                         )
                       ),
                       tags$hr(),
                       fluidRow(
                         column(
                           width = 4,
                           box(
                             width = 12, status = "primary", solidHeader = TRUE,
                             title = "查詢條件",
                             tags$div(
                               style = "margin-bottom:10px;",
                               tags$label("美股代號 (US Ticker)", class = "control-label"),
                               uiOutput("lab_sec_ticker_display"),
                               tags$span(
                                 style = "color:#888; font-size:12px;",
                                 "沿用主頁「Ticker / Stock Code」；於主頁搜尋即同步更新。"
                               )
                             ),
                             radioButtons(
                               "lab_sec_form", "財報類型 (Form)",
                               choices = c(
                                 "年報 10-K / 20-F" = "10-K",
                                 "季報 10-Q" = "10-Q",
                                 "重大訊息 8-K / 6-K" = "8-K"
                               ),
                               selected = "10-K", inline = TRUE
                             ),
                             checkboxInput(
                               "lab_sec_important_only", "只顯示重要附註", value = isTRUE(APP_DEFAULTS$lab_sec_important_only)
                             ),
                             textInput(
                               "lab_sec_keyword", "關鍵字搜尋",
                               value = "",
                               placeholder = "例如 revenue、lease、tax…"
                             ),
                             tags$span(
                               style = "color:#888; font-size:12px; display:block; margin:-6px 0 10px 0;",
                               "比對附註標題、重點摘要與全文（不區分大小寫）；空白＝顯示全部。"
                             ),
                             actionButton(
                               "lab_sec_fetch", "擷取財報附註",
                               class = "btn-success btn-block",
                               icon = icon("download"),
                               style = "font-weight:bold;"
                             ),
                             tags$p(
                               style = "margin-top:10px; color:#888; font-size:12px;",
                               "提示：需向 SEC 逐條擷取附註，約需數秒。"
                             )
                           ),
                           uiOutput("lab_sec_meta")
                         ),
                         column(
                           width = 8,
                           box(
                             width = 12, status = "info", solidHeader = TRUE,
                             title = "附註索引 (Notes Index)",
                             uiOutput("lab_sec_index")
                           ),
                           box(
                             width = 12, status = "info", solidHeader = TRUE,
                             title = "附註內容 (Notes)",
                             uiOutput("lab_sec_notes")
                           )
                         )
                       )
                     )
              )
              ),
              
              tags$div(
                class = "ynow-ind-overview-block",
                tags$div(
                  class = "ynow-ind-overview-title",
                  id = "ynow_ind_overview_title",
                  "目前產業標準快覽"
                ),
                tags$div(
                  class = "ynow-ind-overview-row",
                  tags$div(
                    class = "ynow-ind-overview-picker",
                    pickerInput(
                      inputId = "industry_choice",
                      label = NULL,
                      choices = industry_picker_choices(),
                      selected = APP_DEFAULTS$industry_choice,
                      # container=body: dropdown is portaled out of the picker cell so
                      # data-size menu height cannot create a huge in-flow white gap on mobile.
                      options = list(
                        `live-search` = TRUE,
                        size = 12,
                        container = "body"
                      )
                    )
                  ),
                  tags$div(
                    class = "ynow-ind-overview-summary",
                    uiOutput("dashboard_selected_industry")
                  )
                ),
                tags$div(
                  class = "ynow-ind-overview-metrics",
                  uiOutput("dashboard_industry_metric_chips")
                )
              ),
              .kpi_band_color_legend_ui(),
              
              tabBox(title = "PERFORMANCE",
                     width = "auto",
                     
                     tabPanel("KPI by Sheet", fluidRow(
                       column(width = 12,
                              tags$h4("Balance Sheet KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  valueBoxOutput(NS("kpi", "vbx_eqt_multiplier"), width = NULL)
                              ),
                              tags$h4("Income Statement KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  valueBoxOutput(NS("kpi", "vbx_net_profit_margin"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_gross_profit_margin"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_opex_ratio"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_rev_growth"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_gross_profit_growth"), width = NULL)
                              ),
                              tags$h4("Cash Flow KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  valueBoxOutput(NS("kpi", "vbx_op_cash_flow_growth"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_inv_cash_flow_growth"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_fin_cash_flow_growth"), width = NULL)
                              )
                       )
                     )),
                     
                     tabPanel("Crossover KPIs", fluidRow(
                       column(width = 12,
                              div(class = "ynow-kpi-grid",
                                  valueBoxOutput(NS("kpi", "vbx_ROA"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_ROE"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_asset_turnover"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_ocf_net_income"), width = NULL)
                              )
                       )
                     )),
                     
                     tabPanel(
                       title = tags$span(id = "ynow_dash_annotation_tab", "Annotation"),
                       fluidRow(
                         column(
                           width = 12,
                           tags$p(
                             class = "ynow-ann-note",
                             tags$b("用途："),
                             "解讀上方 KPI 色碼如何對照 Dashboard「目前產業標準快覽」內的產業區間。",
                             "數值採年報序列（排除 TTM）的多年平均或年均 YoY，以利跨期／同業比較。"
                           ),
                           tags$p(
                             style = "margin:-4px 0 12px 0; font-size:12px; color:#777; line-height:1.45;",
                             "規則對齊 ", tags$code("get_box_color"), "：",
                             "多數指標「越高越好」；",
                             tags$b("營運費用比、財務槓桿"), " 為「越低越好」（反向著色）。",
                             "落在產業 ", tags$code("[下限, 上限]"), " 內→黑；越出有利側→藍；不利側→紅（警示）；未設區間／N/A→白。",
                             " KPI 數字框僅用黑／白／紅／藍。色碼圖例與產業標準快覽見上方 PERFORMANCE 區塊外（含產業選單）。"
                           ),
                           tags$h4("KPI 計算與產業區間", class = "ynow-ann-h"),
                           DT::dataTableOutput("annotation_kpi_guide"),
                           tags$h4("指標群組解讀穩定度", class = "ynow-ann-h"),
                           tableOutput("annotation_stability_table"),
                           tags$p(
                             id = "ynow_ann_fscore_crossref",
                             style = "margin-top:10px; font-size:12px; color:#888;",
                             "提示：資產週轉率、OCF／淨利無同業區間時固定為白；",
                             "現金品質另可對照 YNOW／F-Score 品質檢核中的盈餘品質項。"
                           )
                         )
                       )
                     )
              ),
      ),

      # ==========================================
      # Business Breakdown（Full-only；tab id 仍為 company_advance）
      # ==========================================
      tabItem(
        tabName = "company_advance",
        tags$div(
          class = "ynow-full-only ynow-company-advance-bblab",
          id = "ynow_company_advance_bblab",
          business_breakdown_lab_ui("bblab")
        )
      ),

      # ==========================================
      # Smart Analysis（Lite）：自動主／副模型＋預設參數試算與圖表
      # ==========================================
      tabItem(
        tabName = "smart_analysis",
        # Page heading removed (Lite): blurb lives immediately above composite status in the header.
        uiOutput("smart_analysis_summary"),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = tags$span(id = "ynow_smart_chart_title", "Fair value comparison"),
            plotlyOutput("smart_analysis_chart", height = "360px") %>% shinycssloaders::withSpinner()
          )
        ),
        uiOutput("smart_analysis_reason")
      ),
      
      # ==========================================
      # DDM：版面節奏對齊 DCF（模式列 → Overview → 參數 → 敏感度）
      # ==========================================
      tabItem(tabName = "ddm_calculator",
              tabBox(width = "auto",
                     tabPanel("",
                              fluidRow(
                                column(
                                  width = 6,
                                  radioButtons(
                                    "mod_ddm-ddm_mode",
                                    "選擇 DDM 估值模型：",
                                    choices = list(
                                      "Gordon 永續成長 (GGM)" = "gordon",
                                      "SPM 永續和 (Sum of Perpetuities)" = "spm",
                                      "二階段成長法 (Two-Stage Model)" = "two_stage"
                                    ),
                                    selected = APP_DEFAULTS$ddm_mode
                                  ),
                                  tags$p(
                                    id = "ynow_ddm_mode_help",
                                    class = "help-block",
                                    "Gordon (GGM)：V0 = D1/(r−g)。SPM：定額股利永續 + 保留盈餘成長（V0 = E·g/r² + D/r）。二階段：g1 後接 Gordon 終值 Pn。折現率 r = Ke（CAPM）。"
                                  )
                                ),
                                column(
                                  width = 6,
                                  numericInput(
                                    "mod_ddm-d0",
                                    "剛配股利 D0",
                                    value = APP_DEFAULTS$ddm_d0
                                  ),
                                  tags$p(
                                    id = "ynow_ddm_d0_help",
                                    class = "help-block",
                                    "由財報自動帶入：現金股利（現金流量表）÷ 股數；若無則顯示 0。可手動覆寫，或至 D0 以配息率／歷史平均覆寫。SPM 模式下此值為定額永續股利 D。"
                                  ),
                                  conditionalPanel(
                                    condition = "input['mod_ddm-ddm_mode'] == 'spm'",
                                    tags$div(
                                      id = "ynow_ddm_spm_tip",
                                      style = "margin-top: 8px; padding: 8px 10px; background: #FFF8E7; border-left: 3px solid #E0A800; border-radius: 4px; font-size: 13px;",
                                      tags$b("SPM："), " 請至下方 D0 分頁確認「預估／最新 EPS」；公式使用 E（EPS）與 D（上方股利）。"
                                    )
                                  )
                                )
                              )
                     )
              ),

              tabBox(title = "DIVIDEND DISCOUNT", width = "auto",
                     tabPanel(
                       "DDM Overview",
                       icon = icon("calculator"),
                       fluidRow(
                         column(
                           width = 12,
                           fluidRow(
                             class = "ynow-model-kpi-row",
                             column(6, class = "ynow-model-kpi-result",
                                    infoBoxOutput("mod_ddm-ibx_ddm_price", width = NULL)),
                             column(6, class = "ynow-model-kpi-tone-2",
                                    infoBoxOutput("mod_ddm-ibx_ddm_d1", width = NULL))
                           )
                         )
                       ),
                       fluidRow(
                         column(
                           width = 12,
                           .ddm_formula_banner(),
                           uiOutput("mod_ddm-ui_ddm_result"),
                           h6(helpText(id = "ynow_ddm_overview_hint", "提示：D0 在上方模式列；g／二階段在下方 Overview；折現率 r＝Ke 在 Ke 分頁。自訂參數後請再點試算。")),
                           fluidRow(
                             column(
                               width = 6,
                               ynow_calc_btn("mod_ddm-btn_calc_ddm", "試算 DDM")
                             ),
                             column(
                               width = 6,
                               ynow_reset_defaults_btn("mod_ddm-reset_ddm")
                             )
                           ),
                           tags$div(style = "margin-top: 10px;", htmlOutput("mod_ddm-vtxt_ddm_setting_details"))
                         )
                       )
                     ),
                     tabPanel(
                       "DDM Calculation Details",
                       fluidRow(
                         infoBoxOutput("mod_ddm-ibx_d0_scraped", width = 4),
                         infoBoxOutput("mod_ddm-ibx_d0_eps", width = 4),
                         infoBoxOutput("mod_ddm-ibx_d0_payout", width = 4)
                       ),
                       fluidRow(
                         column(
                           width = 12,
                           div(
                             "實務上常需對 D0 進行平滑化或還原本業配息，避免單一年度特別股利或景氣循環造成估值失真。請至 D0 覆寫。",
                             style = "font-size: 15px; font-weight: bold; color: #2C3E50; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
                           )
                         )
                       )
                     )
              ),

              tabBox(width = "auto",
                     tabPanel(
                       "Overview",
                       fluidRow(
                         column(
                           width = 12,
                           numericInput(
                             "mod_ddm-g",
                             "股利永續成長率 g (%)",
                             value = APP_DEFAULTS$ddm_g
                           ),
                           checkboxInput(
                             "mod_ddm-sync_g",
                             "與中央永續成長率（基礎設定 SGR）同步",
                             value = isTRUE(APP_DEFAULTS$ddm_sync_central_g)
                           ),
                           tags$p(
                             id = "ynow_ddm_g_sync_help",
                             class = "help-block",
                             "勾選時跟隨中央 SGR；取消勾選後可單獨覆寫股利成長率（不必等於 FCFF 終值 g）。二階段時此值即 g₂；SPM 時為公式中的盈餘成長 g。"
                           ),
                           tags$div(style = "margin-top: 12px;", .ddm_two_stage_params_box()),
                           conditionalPanel(
                             condition = "input['mod_ddm-ddm_mode'] == 'spm'",
                             tags$div(
                               style = "margin-top: 12px;",
                               tags$p(
                                 id = "ynow_ddm_spm_g_note",
                                 class = "help-block",
                                 "SPM 另需 EPS（E）：請至 D0 分頁確認「預估／最新 EPS」。成長 g 常用 ROE×保留率或 ROA×保留率。"
                               )
                             )
                           )
                         )
                       )
                     ),
                     tabPanel(
                       "D0",
                       icon = icon("cogs"),
                       fluidRow(
                         column(
                           width = 12,
                           div(
                             id = "ynow_ddm_d0_banner",
                             "D0 = 現金股利（現金流量表）÷ 股數（或 EPS × 配息率）　｜　D1 = D0 × (1 + g)",
                             style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
                           )
                         ),
                         box(
                           h4(tags$b("方法 1：目標配息率推算法")),
                           p(helpText("適用於宣告改變股利政策，或未來獲利將發生重大變化的公司")),
                           div(
                             "公式：預估 EPS × 目標配息率",
                             style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
                           ),
                           numericInput("mod_ddm-est_eps", "預估/最新 EPS (元)", value = NA, step = 0.01),
                           numericInput("mod_ddm-est_payout", "目標配息率 Payout Ratio (%)", value = NA, min = 0, max = 100, step = 0.01),
                           actionButton("mod_ddm-calc_d0_payout", "計算並套用 D0", class = "btn-primary"),
                           tags$br(),
                           htmlOutput("mod_ddm-txt_d0_payout_res")
                         ),
                         box(
                           h4(tags$b("方法 2：景氣循環平滑法")),
                           p(helpText("適用於航運、原物料等景氣循環股。系統將自動從現金流量表擷取歷史配息來平均。")),
                           numericInput("mod_ddm-cycle_years", "擷取過去幾年平均？", value = 5, min = 1, max = 10, step = 0.01),
                           actionButton("mod_ddm-calc_d0_average", "計算並套用平均 D0", class = "btn-primary"),
                           tags$br(),
                           htmlOutput("mod_ddm-txt_d0_avg_res")
                         )
                       )
                     ),
                     # Ke ↔ Beta 左右對調：Ke 在前（對齊 DCF 的 WACC → Beta 節奏）
                     tabPanel(
                       "Ke",
                       icon = icon("balance-scale"),
                       uiOutput("ddm_ke_tab_note"),
                       fluidRow(
                         infoBoxOutput("ibx_ddm_ke", width = 4),
                         infoBoxOutput("ibx_ddm_beta", width = 4),
                         infoBoxOutput("ibx_ddm_erp", width = 4)
                       ),
                       div(
                         id = "ynow_ddm_ke_formula_banner",
                         "Ke = Rf + β × (Rm − Rf)",
                         style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
                       ),
                       # 上列 100%：Ke 估算（對齊 WACC 估算盒）；下列左說明、右 CAPM
                       fluidRow(
                         box(
                           width = 12,
                           h4("Ke 估算", id = "ynow_ddm_ke_box_title"),
                           fluidRow(
                             column(
                               4,
                               numericInput(
                                 "mod_ddm-ke", "折現率 r＝Ke (%)",
                                 value = APP_DEFAULTS$ddm_ke, min = 0, step = 0.01
                               )
                             ),
                             column(
                               4,
                               tags$div(
                                 style = "margin-top: 25px;",
                                 htmlOutput("ddm_ke_erp_summary")
                               )
                             ),
                             column(
                               4,
                               tags$div(
                                 style = "margin-top: 25px;",
                                 htmlOutput("ddm_ke_source_chip")
                               )
                             )
                           ),
                           checkboxInput(
                             "ddm_use_estimated_re",
                             tags$span(
                               id = "ynow_ddm_use_estimated_ke_label",
                               "Use estimated Ke (from CAPM)"
                             ),
                             value = isTRUE(APP_DEFAULTS$use_est_re)
                           ),
                           tags$p(
                             id = "ynow_ddm_ke_help",
                             style = "margin:0 0 8px 0;color:#666;font-size:12px;",
                             paste0(
                               "Ke = Rf + β × (Rm − Rf). Bidirectionally synced with DCF→WACC \"Use estimated rₑ\" and the rₑ value; ",
                               "when checked, Ke follows CAPM; uncheck to override manually."
                             )
                           ),
                           actionButton("calc_ddm_ke", "計算 Ke（CAPM）", class = "btn-primary"),
                           tags$br(), htmlOutput("ddm_beta_ke_status")
                         )
                       ),
                       fluidRow(
                         ddm_ke_bridge_settings_ui(width = 6),
                         capm_beta_settings_ui(
                           title = "CAPM Estimate Ke",
                           calc_id = "calc_ddm_capm",
                           result_id = "ddm_capm_result",
                           width = 6,
                           id_prefix = "ddm_",
                           box_title_id = "ynow_ddm_capm_box_title",
                           sync_label_id = "ynow_ddm_sync_gs_beta_label",
                           rf_note_id = "ddm_capm_rf_source_note",
                           btn_label = "Estimate Ke (CAPM)"
                         )
                       )
                     ),
                     tabPanel(
                       "Beta (β)",
                       icon = icon("chart-line"),
                       .beta_model_source_section_ui(
                         "ddm_beta_u_apply_source",
                         "ddm_apply_beta_u_selected"
                       )
                     )
              ),

              tabBox(title = "SENSITIVITY", width = "auto",
                     fluidRow(
                       column(
                         width = 12,
                         h4("敏感度分析矩陣 (Sensitivity Analysis)"),
                         p(helpText("軸心採用目前 DDM 的 Ke 與股利永續 g；觀察鄰近組合下的每股內在價值變化。D0／二階段設定請至上方各分頁。"))
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         div(
                           style = "width: 100%; overflow-x: auto;",
                           tags$style(HTML("#ddm_sensitivity_table table { width: 100% !important; table-layout: fixed; }")),
                           tableOutput("ddm_sensitivity_table")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         uiOutput("ddm_sensitivity_analysis_panel")
                       )
                     )
              ),
              .model_param_sensitivity_box(
                "DDM 公式參數：每股估值貢獻與敏感度",
                "mod_ddm-param_sensitivity_table"
              )
      ),
      
      # ==========================================
      # DCF Calculator 分頁
      # ==========================================
      tabItem(tabName = "dcf_calculator",
              # dcf_mode／dcf_claim 已上移至 shared header（預測年數 n 正上方）
              # WACC 改由 DCF → WACC 分頁／CAPM 同步；此處隱藏保留 input$id 供計算鏈使用
              tags$div(
                style = "display:none;",
                numericInput(
                  "wacc_gordon", "折現率 WACC (%)",
                  value = APP_DEFAULTS$wacc_gordon, step = 0.01
                )
              ),
              
              tabBox(title = "DISCOUNTED CASH FLOW", width = "auto",
                     tabPanel("DCF Overview",
                              fluidRow(
                                column(width = 12,
                                       fluidRow(
                                         class = "ynow-model-kpi-row",
                                         column(6, class = "ynow-model-kpi-result",
                                                infoBoxOutput("ibx_stock_value_dcf", width = NULL)),
                                         column(6, class = "ynow-model-kpi-tone-2",
                                                infoBoxOutput("ibx_enterprise_value_dcf", width = NULL))
                                       )
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       plotOutput("plt_dcf_trajectory", height = "420px"),
                                       h6(uiOutput("dcf_chart_help")),
                                       fluidRow(
                                         column(width = 6, ynow_calc_btn("calc", "試算 DCF")),
                                         column(width = 6, ynow_reset_defaults_btn("reset_dcf"))
                                       ),
                                       tags$div(style = "margin-top: 10px;", htmlOutput("vtxt_dcf_setting_details"))
                                )
                              )
                     ),
                     
                     tabPanel("DCF Calculation Details",
                              fluidRow(
                                column(width = 12,
                                       plotOutput("mod_fcf-fcf_plot", height = "350px")
                                ),
                                uiOutput("ui_data_validation") 
                              )
                     )
              ),
              
              tabBox(width = "auto",
                     tabPanel("Overview",
                              fluidRow(
                                infoBoxOutput("ibx_estimated_g", width = 6),
                                infoBoxOutput("ibx_sgr", width = 6),
                                
                                column(width = 12,
                                       plotOutput("plt_fcf_trend", height = "350px")
                                ),
                                column(
                                  width = 12,
                                  tags$div(style = "margin-top: 12px;", .dcf_two_stage_params_box())
                                )
                              )
                     ),
                     
                     # 在 ui.R 的某個 tabBox 或 navbarMenu 中：
                     fcf_projection_module_ui(id = "mod_fcf"),
                     
                     tabPanel("WACC",
                              icon = icon("balance-scale"),
                              uiOutput("dcf_wacc_fcfe_note"),
                              fluidRow(
                                valueBoxOutput("vbx_equity_val", width = 4), # 股權市值 (E)
                                valueBoxOutput("vbx_debt_val", width = 4),   # 總負債 (D)
                                valueBoxOutput("vbx_tax_rate", width = 4)    # 有效稅率 (T)
                              ),
                              fluidRow(
                                infoBoxOutput("ibx_wacc", width = 4),
                                infoBoxOutput("ibx_rd", width = 4), 
                                infoBoxOutput("ibx_re", width = 4)
                              ),
                              
                              uiOutput("dcf_disc_formula_banner"),
                              # 上列 100%：WACC 估算；下列左 50% rᵈ、右 50% CAPM
                              fluidRow(
                                box(
                                  width = 12,
                                  h4("WACC 估算", id = "ynow_wacc_box_title"),
                                  fluidRow(
                                    column(4, numericInput(
                                      "wacc_re", "股權成本 rₑ (%)",
                                      value = APP_DEFAULTS$wacc_re, min = 0, step = 0.01
                                    )),
                                    column(4, numericInput(
                                      "wacc_rd", "負債成本 rᵈ (%)",
                                      value = APP_DEFAULTS$wacc_rd, min = 0, step = 0.01
                                    )),
                                    column(4, numericInput(
                                      "wacc_tax", "所得稅率 T (%)",
                                      value = APP_DEFAULTS$wacc_tax, min = 0, max = 100, step = 0.01
                                    ))
                                  ),
                                  checkboxInput(
                                    "use_estimated_re",
                                    tags$span(id = "ynow_use_estimated_re_label", "Use estimated rₑ (from CAPM)"),
                                    value = APP_DEFAULTS$use_est_re
                                  ),
                                  uiOutput("wacc_tax_source_note"),
                                  tags$p(
                                    id = "ynow_wacc_help",
                                    style = "margin:0 0 8px 0;color:#666;font-size:12px;",
                                    "WACC = We×rₑ + Wd×rᵈ×(1−T)。rᵈ 可由下方「估算 rᵈ」以利息費用／有息負債推估（稅前），再於此套用稅盾。"
                                  ),
                                  actionButton("calc_wacc", "計算 WACC", class = "btn-primary"),
                                  tags$br(), htmlOutput("wacc_result")
                                )
                              ),
                              fluidRow(
                                rd_estimate_settings_ui(width = 6),
                                capm_beta_settings_ui(
                                  title = "CAPM Estimate rₑ",
                                  calc_id = "calc_capm",
                                  result_id = "capm_result",
                                  width = 6
                                )
                              )
                     ),

                     tabPanel(
                       "Beta (β)",
                       icon = icon("chart-line"),
                       .beta_model_source_section_ui(
                         "dcf_beta_u_apply_source",
                         "dcf_apply_beta_u_selected"
                       )
                     )
              ),
              tabBox(title = "SENSITIVITY", width = "auto",
                     fluidRow(
                       column(
                         width = 12,
                         h4("敏感度分析矩陣 (Sensitivity Analysis)"),
                         uiOutput("dcf_sens_help")
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         div(
                           style = "width: 100%; overflow-x: auto;",
                           tags$style(HTML("#dcf_sensitivity_table table { width: 100% !important; table-layout: fixed; }")),
                           tableOutput("dcf_sensitivity_table")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         uiOutput("sensitivity_analysis_panel")
                       )
                     )
              ),
              .model_param_sensitivity_box(
                "DCF 公式參數：每股估值貢獻與敏感度",
                "dcf_param_sensitivity_table"
              )
      ),
      
      # 🌟 呼叫 RI 模型分頁介面
      ri_module_ui("mod_ri"),
      
      # 🌟 呼叫 P/B 相對估值分頁介面
      pb_asset_module_ui("mod_pb"),

      # Relative multiples: Earnings / Enterprise / P/S (Implied Price)
      relative_multiples_module_ui("mod_rel"),

      # SOTP — structural Sum-of-the-Parts (separate sidebar)
      sotp_module_ui("mod_sotp"),

      # 🌟 呼叫純 NAV 分頁介面
      nav_module_ui("mod_nav"),
      
      tabItem(tabName = "sensitivity",
              decision_ui("main_decision")
      ),

      # Blue Chip：美股績優篩選（版面節奏對齊 Dashboard FINANCIAL REPORT）
      tabItem(
        tabName = "bluechip",
        uiOutput("ynow_lazy_host_bluechip")
      ),
      
      # ==========================================
      # 歷史基本面驗證（HFV）：報告式版面 — 理論估值 vs 實際市值（非策略回測）
      # ==========================================
      tabItem(
        tabName = "hfv",
        uiOutput("ynow_lazy_host_hfv")
      ),

      # ==========================================
      # 決策檢核（投資決策通過檢核表）
      # ==========================================
      decision_checklist_tab_ui(),

      # ==========================================
      # Quant Backtest Lab（報告式版面；章節內容後接預設收合之控制區）
      # ==========================================
      tabItem(
        tabName = "lab_notes",
        uiOutput("ynow_lazy_host_lab_notes")
      ),

      # ==========================================
      # Asset transmission（側邊欄；Lite + Full）
      # ==========================================
      tabItem(
        tabName = "asset_transmission",
        uiOutput("ynow_lazy_host_asset_transmission")
      ),

      # ==========================================
      # 🧪 Testing：完整版側邊欄小按鈕入口（供後續實驗／測試功能）
      # ==========================================
      tabItem(
        tabName = "testing",
        uiOutput("ynow_lazy_host_testing")
      ),

      # ==========================================
      # 💬 意見區：使用者回饋 → GitHub Issues
      # ==========================================
      tabItem(
        tabName = "feedback",
        fluidRow(
          column(
            width = 12,
            h2(tags$b("意見區 — 使用者回饋與優化建議")),
            p(
              "歡迎回報問題、使用體驗或功能建議。送出後會以",
              tags$b("GitHub Issue"),
              "系統性收集（標籤 ",
              tags$code("feedback"),
              "），方便追蹤與排入後續優化。"
            ),
            tags$p(
              style = "color:#888;font-size:13px;",
              "需在執行環境設定環境變數 ",
              tags$code("YNOW_FEEDBACK_GITHUB_TOKEN"),
              "（具 ",
              tags$code("issues:write"),
              " 權限的 fine-grained 或 classic PAT）。可選 ",
              tags$code("YNOW_FEEDBACK_GITHUB_REPO"),
              "（預設 ",
              tags$code("lawrencekuo1118/theYNowApp"),
              "）。"
            ),
            tags$hr()
          )
        ),
        fluidRow(
          column(
            width = 7,
            box(
              width = 12, status = "primary", solidHeader = TRUE,
              title = tagList(icon("edit"), "回饋表單"),
              selectInput(
                "feedback_category", "類別",
                choices = c(
                  "優化建議" = "enhancement",
                  "問題回報" = "bug",
                  "使用體驗" = "ux",
                  "其他" = "other"
                ),
                selected = "enhancement"
              ),
              textInput(
                "feedback_title", "標題",
                value = "",
                placeholder = "簡短描述（必填）"
              ),
              textAreaInput(
                "feedback_body", "內容說明",
                value = "",
                rows = 8,
                placeholder = "請描述現象、重現步驟、期望行為，或優化想法…"
              ),
              textInput(
                "feedback_contact", "聯絡方式（選填）",
                value = "",
                placeholder = "Email 或其他，不會公開顯示於 Issue 標題"
              ),
              checkboxInput(
                "feedback_include_context",
                "附上目前 Session 脈絡（Ticker／產業／App 版本）",
                value = TRUE
              ),
              actionButton(
                "feedback_submit", "送出至 GitHub Issues",
                class = "btn-success",
                icon = icon("paper-plane")
              ),
              uiOutput("feedback_status")
            )
          ),
          column(
            width = 5,
            box(
              width = 12, status = "info", solidHeader = TRUE,
              title = "如何追蹤",
              tags$ul(
                tags$li("送出成功後會顯示 Issue 編號與連結。"),
                tags$li(
                  "亦可至 repo Issues 篩選標籤 ",
                  tags$code("feedback"),
                  "：",
                  tags$a(
                    href = "https://github.com/lawrencekuo1118/theYNowApp/issues?q=is%3Aissue+label%3Afeedback",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    "查看已收集意見"
                  )
                ),
                tags$li("請勿在表單貼上密碼、token 或其他機密資訊。")
              )
            )
          )
        )
      ),

      # ==========================================
      # ℹ️ About 分頁 (系統介紹／Lite 簡介；完整評價方法論在 Basic Setup 最下方)
      # ==========================================
      tabItem(
        tabName = "about",
        tags$div(
          class = "ynow-full-only",
          .about_bilingual_intro_ui()
        ),
        tags$div(
          class = "ynow-lite-only",
          .about_lite_intro_ui()
        ),
        tags$hr(style = "margin: 20px 0 12px 0; border-color: #e5e8eb;"),
        .legal_notices_ui()
      )
    )
  )
)
