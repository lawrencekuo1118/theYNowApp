# ==========================================
# ynow_about_ui.R -- About page bodies (Full / Lite / legal notices)
#
# C-STRUCT-02: extracted from ynow_ui.R. Sourced by the UI facade with
# local = TRUE. Call site (tabItem "about" + Lite/Full wrappers) stays in
# ynow_ui.R. No valuation / Lite-lock / auth semantic change.
# ==========================================

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
      "內建 Fair Value 引擎（DCF：FCFF／WACC 或 FCFE／Ke、DDM：Gordon／SPM／二階段、P/B＋NAV、RI）、市場定價參照 Multiples，以及綜合型估值框架 SOTP（Meta-Framework）；以個股財報屬性推薦主／副路徑，產業與資料完整性為輔，並以 Composite valuation 並陳各模型相對現價位置。"
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
      "Fair Value engines (DCF: FCFF/WACC or FCFE/Ke; DDM: Gordon / SPM / two-stage; P/B with holding NAV; Residual Income), Multiples as a market-pricing Reference, and SOTP as a Meta-Framework that can mix absolute and relative lenses by segment. The app recommends a primary/secondary path from the stock's financial-statement attributes, with industry and data completeness as supplements, and overlays model values versus the current price on the Composite valuation axis."
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
          "The YNow App (v26.19) 是一套專為專業投資人與分析師打造的「全方位量化財務與估值決策系統」。本系統整合即時財報擷取、多模型估值、決策檢核、Blue Chip Lab 與動態回測，將繁雜的市場資料轉化為可執行的投資決策架構。"
        ),
        tags$p(
          class = "ynow-about-method",
          "我們的核心方法論為：",
          tags$b("「先分類，再選模型；先推導，再校正；先給區間，再給單點。」"),
          " ",
          tags$b("可解釋性是核心功能，不是附註："),
          "每個合理價旁都應能說清公式路徑、輸入與資料來源；完整 Valuation Methodology 矩陣仍在 ",
          tags$b("Model Dashboard"),
          "。"
        ),
        tags$p(
          class = "ynow-about-method",
          tags$b("帳號登入："),
          "帳號登入預設啟用；一般使用者可用 Google、Facebook、LinkedIn 或 Email 自行註冊並立即使用，無需後台開權限；",
          "帳號表經 YNOW_DB_* 的 Supabase Postgres（自管 users；例如 Supabase pooler）；完整版的量化回測實驗室、決策檢核、到值通知需登入（寄信預設註冊信箱）；",
          "登入後（完整版）可開啟維護後台。Lite 版無需帳號即可完整使用。"
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
          "The YNow App (v26.19) is a comprehensive quantitative financial analysis and valuation decision system for professional investors and analysts. It integrates real-time financials, multi-model valuation, Decision Checklist, Blue Chip Lab, and dynamic backtesting to turn complex market data into a disciplined decision framework."
        ),
        tags$p(
          class = "ynow-about-method",
          "Our core methodology is: ",
          tags$b("\"Classify before selecting models; derive before calibrating; provide valuation ranges before absolute price targets.\""),
          " ",
          tags$b("Explainability is a core feature, not an appendix:"),
          " every Fair Value should show its formula path, inputs, and data sources. ",
          "The full Valuation Methodology matrix remains on ",
          tags$b("Model Dashboard"),
          "."
        ),
        tags$p(
          class = "ynow-about-method",
          tags$b("Account sign-in:"),
          " Account sign-in is on by default. Anyone may self-register with Google, Facebook, LinkedIn, or Email — no per-user admin approval. Account store: Supabase Postgres via YNOW_DB_* (own users table; e.g. Supabase pooler). Quantitative Backtest Lab, Decision Checklist, and Value Alerts require sign-in in Full mode; email notify defaults to the registered mailbox. After sign-in, Maintenance opens; Lite never requires an account."
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
      "依股票性質自動判別主／副 Fair Value 模型（DCF、DDM、RI、P/B、NAV）；Multiples 恆為市場定價參照（Reference）；SOTP 為綜合型估值框架（Meta-Framework），控股／多事業體常與 NAV 搭配。",
      "辨識參數情境（Two-Stage／Gordon、SGR 法、claim）並套用最合理預設後試算，",
      "顯示合理價比較圖與 MOS；不開放手動模型設定。"
    ),
    tags$li(
      tags$b("YNOW："),
      "與完整版相同的盈餘品質、F-Score 品質檢核、風險矩陣與財報警訊；動態產業泡沫與權重集中度在「總體經濟與大盤趨勢」分頁最下方。"
    ),
    tags$li(
      tags$b("績優股排行榜（Blue Chip）："),
      "保留候選截斷邏輯與宇宙檔數（N）、盈餘品質與 ADR 篩選、排行頁底部明細表、分群研究，以及自訂選股；隱藏其餘進階查詢條件。"
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
      "Auto-selects primary and secondary Fair Value models (DCF, DDM, RI, P/B, NAV) from the ticker profile; Multiples is always a market-pricing Reference; SOTP is a Meta-Framework often paired with NAV for holdings / multi-segment firms. ",
      "detects the best parameter scenario (Two-Stage vs Gordon, SGR method, claim), applies those defaults, ",
      "and shows fair-value comparison charts plus MOS—no manual model settings."
    ),
    tags$li(
      tags$b("YNOW: "),
      "Same Quality of Earnings, F-Score screen, risk matrix, and statement alerts as Full. Dynamic industry bubble & weight concentration sit at the bottom of Macro & Market Trends."
    ),
    tags$li(
      tags$b("Blue Chip Leaderboard: "),
      "Keeps Candidate truncate rules and Universe size (N), Earnings Quality and ADR filters, the detail table at the bottom of Rankings, Clustering, and Custom Screen. Hides other advanced Ranking filters."
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
          "The YNow App Lite（v26.19）是完整版的精簡工作流：共用同一套財報資料與估值公式，",
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
          "The YNow App Lite (v26.19) is a streamlined workflow of the Full app. It reuses the same financial data and valuation formulas, ",
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
