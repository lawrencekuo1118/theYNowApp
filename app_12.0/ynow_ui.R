# ==========================================
# ui.R - 前端介面設計
# ==========================================

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

.dcf_core_params_box <- function() {
  box(
    title = tagList(icon("seedling"), "DCF 估值核心參數設定"),
    width = 12, status = "warning", solidHeader = TRUE,
    selectInput(
      "perpetual_g_method",
      "估計永續成長率方法",
      choices = c(
        "總體經濟錨定法（Macroeconomic Anchoring）" = "macro",
        "永續成長公式法（Fundamental / SGR）" = "fundamental",
        "產業生命週期檢核法（Lifecycle Check）" = "lifecycle"
      ),
      selected = APP_DEFAULTS$perpetual_g_method
    ),
    helpText(
      "Macro：直接套用美國 10 年期公債 Rf。",
      "Fundamental：Retention×ROE（僅適合成熟穩健企業）。",
      "Lifecycle：依產業成熟度反推 g，可手動覆寫自動分類。"
    ),
    conditionalPanel(
      condition = "input.perpetual_g_method == 'lifecycle'",
      selectInput(
        "lifecycle_stage",
        "產業生命週期檔位（可覆寫自動偵測）",
        choices = c(
          "自動偵測" = "auto",
          "夕陽／高度成熟（≈1.5–2%）" = "mature_sunset",
          "成熟科技巨頭（≈2.5–3%）" = "mature_tech",
          "高速成長→成熟（終值≈2.5%，建議 two-stage）" = "growth_to_mature",
          "一般成熟（≈2.5%）" = "mature_general"
        ),
        selected = APP_DEFAULTS$lifecycle_stage
      )
    ),
    uiOutput("txt_perpetual_g_reason"),
    numericInput(
      "sgr",
      "DCF／RI 終值永續成長率 SGR (%)",
      value = APP_DEFAULTS$sgr
    ),
    helpText("此為 FCFF／剩餘收益終值成長率（相對 WACC）；與 DDM 股利成長率 g 分開。預設可由上方方法估計，可手動覆寫。"),
    conditionalPanel(
      condition = "input.dcf_mode == 'gordon'",
      h4(tags$b("DCF：明確預測 + Gordon 終值")),
      helpText("非單期 EV = FCF₁/(WACC−g)；為 n 年 FCFF 折現 + 終值 TV = FCFₙ(1+g)/(WACC−g)。"),
      numericInput("wacc_gordon", "折現率 WACC (%)", value = APP_DEFAULTS$wacc_gordon, step = 0.01)
    ),
    conditionalPanel(
      condition = "input.dcf_mode == 'two_stage'",
      h4(tags$b("PHASE I 高速成長假設")),
      numericInput("yr_stage1", "第一階段年數", value = APP_DEFAULTS$yr_stage1),
      numericInput("g_stage1", "第一階段成長率 g1 (%)", value = APP_DEFAULTS$g_stage1),
      numericInput("wacc_stage1", "第一階段折現率 WACC1 (%)", value = APP_DEFAULTS$wacc_stage1, step = 0.01),
      tags$hr(),
      h4(tags$b("PHASE II 永續成長假設")),
      numericInput("wacc_stage2", "第二階段折現率 WACC2 (%)", value = APP_DEFAULTS$wacc_stage2, step = 0.01)
    ),
    checkboxInput("use_calculated_wacc", "套用系統估算 WACC", value = APP_DEFAULTS$use_calc_wacc)
  )
}

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "The YNow App",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    collapsed = FALSE,
    column(width = 12,
           sidebarSearchForm(textId = "txt_search", buttonId = "btn_search", label = "Search..."),
           column(width = 12, textOutput("today"),
                  hr()
           )
    ),
    
    column(width = 12,
           sidebarMenuOutput("sidebar_menu"),
           hr()
    ),
    
    column(width = 12,
           h5("Recent Search:"),
           textOutput("recentsearch"),
           hr()
    ),
    
    column(width = 12,
           div(style = "padding: 10px; text-align: center; margin-top: 20px;",
               downloadButton("download_report", "下載完整分析報告 (PDF)", 
                              style = "width: 100%; font-weight: bold; background-color: #1a1a1a; color: #ffffff; border: 1px solid #000000; box-shadow: none; text-shadow: none;")
           )
    ),
    
    column(width = 12,
           div(style = "padding: 15px; border-radius: 5px; border-left: 4px",
               tags$b("Data Source:"), tags$br(),
               "This application integrates real-time financial data via web parsing and API resources, applying comprehensive models for valuation."
           )
    )
  ),
  
  dashboardBody(
    withMathJax(),
    
    tags$head(
      tags$style(HTML('.main-header .logo { font-weight: bold; }')),
      
      tags$style(HTML("
        .selectize-dropdown-content {
          max-height: 300px !important;
          overflow-y: auto !important;
        }
        .selectize-dropdown {
          max-height: 300px !important;
        }

        /* 主搜尋框預選清單：黑字白底 */
        #sc_ticker_suggest {
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
        #sc_ticker_suggest .ynow-suggest-item {
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
        #sc_ticker_suggest .ynow-suggest-item:hover,
        #sc_ticker_suggest .ynow-suggest-item:focus {
          background: #f2f2f2;
          color: #000000 !important;
          outline: none;
        }
        #sc_ticker_suggest .ynow-suggest-sym {
          font-weight: 700;
          color: #000000;
          margin-right: 8px;
        }
        #sc_ticker_suggest .ynow-suggest-lab {
          color: #222222;
          font-weight: 400;
        }
        .ynow-sc-wrap {
          position: relative;
          max-width: 400px;
        }
        
        .info-box .info-box-number {
          font-size: 150% !important;
          font-weight: bold;
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
        @media (max-width: 576px) {
          .ynow-fs-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
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
          font-size: 15px;
          font-weight: 700;
          color: #111111;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.01em;
          line-height: 1.2;
          word-break: break-word;
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
        @media (max-width: 576px) {
          .ynow-kpi-grid > * {
            width: 50% !important;
            max-width: 50% !important;
            flex-basis: 50%;
          }
        }
        .ynow-kpi-grid .small-box {
          aspect-ratio: 1.618 / 1 !important;
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
          float: none !important;
          width: 100% !important;
          min-height: 74px !important;
          height: auto !important;
          border-radius: 5px !important;
          margin-bottom: 9px !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05) !important;
        }
        .ynow-kpi-grid .small-box .inner {
          padding: 6px 9px !important;
          text-align: center !important;
        }
        .ynow-kpi-grid .small-box .inner h3 {
          font-size: clamp(14px, 2.6vw, 23px) !important;
          font-weight: 800 !important;
          margin: 0 0 5px 0 !important;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .ynow-kpi-grid .small-box .inner p {
          font-size: clamp(10px, 0.75vw, 11px) !important;
          opacity: 0.9;
          font-weight: 500 !important;
          margin: 0 !important;
          line-height: 1.2 !important;
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

        /* Backtest：績效指標卡片（軟色調 + 左側色條，避免實心色塊） */
        .ynow-metric-grid {
          --ynow-metric-green: #2d8a57;
          --ynow-metric-green-tint: #eef7f1;
          --ynow-metric-red: #c0392b;
          --ynow-metric-red-tint: #faf0ef;
          --ynow-metric-violet: #5c5a8a;
          --ynow-metric-violet-tint: #f3f2f8;
          --ynow-metric-blue: #2f6f9f;
          --ynow-metric-blue-tint: #eef5fa;
          --ynow-metric-amber: #b7791f;
          --ynow-metric-amber-tint: #faf6ee;
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 14px;
          margin: 0 0 4px 0;
        }
        @media (max-width: 992px) {
          .ynow-metric-grid { grid-template-columns: 1fr; }
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
        .ynow-metric-card--blue .ynow-metric-card__value { color: #1e4d6e; }
        .ynow-metric-card--amber .ynow-metric-card__value { color: #8a5a12; }
        /* 執行面板：避免 btn-block 蓋住下方說明文字 */
        .ynow-bt-run-panel .btn-block { margin-left: 0; margin-right: 0; }
        .ynow-bt-run-panel .ynow-bt-run-note {
          clear: both;
          display: block;
          position: relative;
          z-index: 1;
          margin: 12px 0 0 0;
          padding: 8px 10px;
          background: #fff8e8;
          border: 1px solid #f0e0b2;
          border-radius: 4px;
          font-size: 11.5px;
          line-height: 1.45;
          color: #6b5a2e;
        }
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
          font-size: 12px;
          font-weight: 600;
          color: #555555;
          line-height: 1.35;
          margin: 0;
        }
        .ynow-metric-card__value {
          font-size: clamp(26px, 3.2vw, 34px);
          font-weight: 800;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.02em;
          line-height: 1.15;
          margin: 2px 0 0 0;
          color: #1a1a1a;
        }
        .ynow-metric-card--green .ynow-metric-card__value { color: #1f5c3a; }
        .ynow-metric-card--red .ynow-metric-card__value { color: #8e2a20; }
        .ynow-metric-card--violet .ynow-metric-card__value { color: #3f3d62; }
        .ynow-metric-card__caption {
          margin: 2px 0 0 0;
          font-size: 11.5px;
          color: #6b7280;
          line-height: 1.45;
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
        
        /* 針對 search_results (產業資訊) 進行黑白主題與字體縮小 */
        #search_results {
          background-color: #1e1e1e !important;  /* 深黑色背景 */
          color: #eeeeee !important;             /* 淺白色文字 */
          font-size: 12px !important;            /* 縮小字體 */
          border: 1px solid #444444 !important;  /* 加上細緻的暗色邊框 */
          padding: 8px 12px !important;          /* 調整內邊距讓它扁平一點 */
          border-radius: 4px !important;         /* 圓角 */
          font-weight: 500 !important;
          line-height: 1.2 !important;
        }
      "))
    ),
    
    # ==========================================
    # 獨立的 sc 搜尋輸入框與按鈕區塊
    # ==========================================
    fluidRow(
      column(width = 12,
             titlePanel(h5("a lawrence kuo shiny app")),
             div(
               class = "ynow-sc-wrap",
               textInput("sc", "Ticker / Stock Code", value = APP_DEFAULTS$stock_code),
               uiOutput("sc_ticker_suggest_ui")
             ),
             tags$script(HTML("
               (function() {
                 /* Dropdown only while typing (not on focus/empty). */
                 var typingOpen = false;

                 function scValue() {
                   var inp = document.getElementById('sc');
                   return inp ? (inp.value || '') : '';
                 }

                 function hasTypedQuery() {
                   return scValue().trim().length > 0;
                 }

                 function showSuggest() {
                   var el = document.getElementById('sc_ticker_suggest');
                   if (!el) return;
                   if (typingOpen && hasTypedQuery() && el.children.length) {
                     el.style.display = 'block';
                   } else {
                     el.style.display = 'none';
                   }
                 }

                 function hideSuggest() {
                   typingOpen = false;
                   var el = document.getElementById('sc_ticker_suggest');
                   if (el) el.style.display = 'none';
                 }

                 $(document).on('input', '#sc', function() {
                   var v = $(this).val() || '';
                   Shiny.setInputValue('ticker_typeahead', v, {priority: 'event'});
                   typingOpen = v.trim().length > 0;
                   showSuggest();
                 });

                 $(document).on('blur', '#sc', function(e) {
                   var rt = e.relatedTarget;
                   var el = document.getElementById('sc_ticker_suggest');
                   if (el && rt && el.contains(rt)) return;
                   hideSuggest();
                 });

                 $(document).on('keydown', '#sc', function(e) {
                   if (e.key === 'Enter' || e.keyCode === 13) hideSuggest();
                 });

                 $(document).on('click', '#search', function() {
                   hideSuggest();
                 });

                 $(document).on('mousedown', '#sc_ticker_suggest .ynow-suggest-item', function(e) {
                   e.preventDefault();
                   var sym = $(this).data('symbol');
                   hideSuggest();
                   if (sym) {
                     $('#sc').val(sym).trigger('change');
                     Shiny.setInputValue('sc', sym, {priority: 'event'});
                   }
                 });

                 $(document).on('shiny:value', function(e) {
                   if (e.name === 'sc_ticker_suggest_ui') {
                     setTimeout(showSuggest, 0);
                   }
                 });
               })();
             "))
      )
    ),
    fluidRow(
      column(width = 4,
             tags$div(
               style = "display: flex; align-items: center; gap: 10px;",
               actionButton("search", "Search", icon = icon("search")))
      ),
      column(width = 8,
             h2(textOutput("txt_corpname"), style = "font-weight: bold; color: #333333; ")
      )
    ),
    br(),
    
    fluidRow(
      infoBoxOutput("ibx_stockprice"),
      infoBoxOutput("ibx_marketcap"),
      infoBoxOutput("ibx_EPS")
    ),
    
    # 插入智能估值顧問的 UI 輸出點（由 decision 模組提供）
    
    tabItems(
      tabItem(
        tabName = "get_started",
        h2(tags$b("Get Started")),
        helpText("先確認適合的估值模型；下方 SGR 主要供 DCF／RI 終值使用。DDM 股利成長率可在 DDM 分頁單獨覆寫。"),
        fluidRow(
          box(
            title = tagList(icon("route"), "Model Selector｜估值模型推薦"),
            width = 12, status = "primary", solidHeader = TRUE,
            uiOutput("get_started_model_selector")
          )
        ),
        fluidRow(
          .dcf_core_params_box()
        )
      ),

      tabItem(
        tabName = "snapshot",
        h2("Snapshot"),
        helpText("即時列出目前 App 參數值與公式；下載檔會註記產生日期與時間。"),
        fluidRow(
          box(
            title = tagList(icon("camera"), "Current App Parameter Snapshot"),
            width = 12, status = "info", solidHeader = TRUE,
            div(style = "display:flex; justify-content:space-between; align-items:center; gap:12px; margin-bottom:10px;",
                uiOutput("snapshot_timestamp"),
                downloadButton("download_snapshot", "下載 Snapshot CSV", icon = icon("download"))
            ),
            dataTableOutput("snapshot_table")
          )
        )
      ),

      tabItem(tabName = "dashboard",
              
              div(style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                  actionButton("btn_expand_all", "Expand All", 
                               icon = icon("expand"),
                               class = "btn-sm",
                               style = "background-color: #222222; color: #ffffff; border: 1px solid #555555; font-size: 12px; padding: 4px 12px; border-radius: 4px;")
              ),
              
              tabBox(title = "FINANCIAL REPORT",
                     width = "auto",
                     
                     tabPanel("Finance Summary",
                              p("This section imports Finance Summaries from Yahoo Finance",
                                style = "margin-bottom: 12px; color: #666; font-size: 13px;"),
                              uiOutput("fs_summary_ui"),
                              downloadButton('FS_download', "Download Finance Summary")
                     ),
                     
                     tabPanel("Income Statement",
                              p("This section imports Income Statements from Yahoo Finance"),
                              
                              # 🌟 新增：Income Statement 下拉選單與互動圖表
                              selectInput("is_type", "Select Income Statement Metric",
                                          choices = c("Total Revenue", "Gross Profit", "EBITDA")),
                              plotlyOutput("is_plot"),
                              tags$hr(),
                              
                              dataTableOutput("tbIncomeStatement"), 
                              downloadButton('IS_download', "Download Income Statement")
                     ),
                     
                     tabPanel("Balance Sheet",
                              p("This section imports Balance Sheets from Yahoo Finance"),
                              tags$hr(),
                              dataTableOutput("tbBalanceSheet"),
                              downloadButton('BS_download', "Download Balance Sheet")
                     ),
                     
                     tabPanel("Cash Flow",
                              p("This section imports Cash Flow data from Yahoo Finance"),
                              selectInput("cf_type", "Select Cash Flow Type",
                                          choices = c("Operating Cash Flow", "Investing Cash Flow", "Financing Cash Flow")),
                              plotlyOutput("cf_plot"),
                              tags$hr(),
                              dataTableOutput("tbCashFlow"),
                              downloadButton('CF_download', "Download Cash Flow Data")
                     )
              ),
              
              pickerInput(
                inputId = "industry_choice",
                label = "Industry Standard Comparison",
                choices = industry_picker_choices(),
                selected = APP_DEFAULTS$industry_choice,
                options = list(`live-search` = TRUE, `size` = 12)
              ),
              
              tags$p("industry info from Yahoo", style = "font-size: 12px; color: #888; margin-bottom: 5px; font-weight: bold;"),
              verbatimTextOutput("search_results"),
              
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
                     
                     tabPanel("Annotation", fluidRow(
                       column(width = 12,
                              div(style = "margin-bottom: 20px; padding: 12px; background: #fdfdfd; border: 1px dashed #ccc; border-radius: 6px; display: flex; align-items: center; justify-content: center; font-size: 13px;",
                                  span(style = "font-weight: bold; margin-right: 15px;", "同業比較圖例:"),
                                  span(icon("circle", style = "color: #0073b7;"), " 高於標準 (Better) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #00a65a;"), " 符合標準 (Standard) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #dd4b39;"), " 低於標準 (Worse) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #333;"), " 無資料 / 錯誤")
                              )
                       ),
                       column(width = 12,
                              tableOutput("stable_indicator_table")
                       )
                     ))
              ),
              
              fluidRow(
                column(width = 12,
                       div(style = "background-color: #d9534f; color: white; padding: 15px; margin-top: 20px;",
                           h4(icon("exclamation-triangle"), " Fraud Warnings", 
                              style = "font-weight: bold; margin-top: 0; border-bottom: 1px solid #ffcccc; padding-bottom: 10px;"),
                           div(style = "font-size: 15px; line-height: 1.8;",
                               textOutput("highdebttoequity"),
                               textOutput("nofreecashflow"),
                               textOutput("nooperatingcashflow"),
                               textOutput("notdoingbusiness"),
                               textOutput("notgettingcashback"),
                               textOutput("no_fraud_detected")
                           )
                       )
                )
              )
      ),
      
      # ==========================================
      # DDM 頁面設計 (升級雙分頁版)
      # ==========================================
      tabItem(tabName = "ddm_calculator",
              tabBox(title = "DIVIDEND DISCOUNT", width = "auto",
                     
                     # --- 分頁 1：DDM 估值主畫面 ---
                     tabPanel("DDM Overview", icon = icon("calculator"),
                              fluidRow(
                                column(width = 6,
                                       # 🌟 關鍵修復：統一加上 mod_ddm- 前綴
                                       numericInput("mod_ddm-d0", "今年發放股利 (D0)", value = APP_DEFAULTS$ddm_d0),
                                       numericInput(
                                         "mod_ddm-g",
                                         "股利永續成長率 g (%)",
                                         value = APP_DEFAULTS$ddm_g
                                       ),
                                       checkboxInput(
                                         "mod_ddm-sync_g",
                                         "與中央永續成長率（Get Started SGR）同步",
                                         value = isTRUE(APP_DEFAULTS$ddm_sync_central_g)
                                       ),
                                       helpText("勾選時跟隨中央 SGR；取消勾選後可單獨覆寫股利成長率（不必等於 FCFF 終值 g）。"),
                                       numericInput("mod_ddm-ke", "要求報酬率 (Ke) %", value = APP_DEFAULTS$ddm_ke),
                                       helpText("股利屬股權現金流，以 Ke（CAPM）折現；DCF 的 FCFF 則以 WACC 折現。"),
                                       tags$div(style = "margin-top: 15px; margin-bottom: 15px;",
                                                actionButton("mod_ddm-btn_calc_ddm", "試算 DDM 合理股價", class = "btn-primary", icon = icon("calculator")),
                                                HTML("&nbsp;&nbsp;"), 
                                                actionButton("mod_ddm-reset_ddm", "回復預設", class = "btn-warning", icon = icon("refresh"))
                                       )
                                ),
                                column(width = 6,
                                       # 🌟 關鍵修復：對接後端的 ui_ddm_result
                                       uiOutput("mod_ddm-ui_ddm_result")      
                                )
                              )
                     ),
                     
                     # --- 分頁 2：D0 進階參數設定 ---
                     tabPanel("D0 Settings", icon = icon("cogs"),
                              fluidRow(
                                infoBoxOutput("mod_ddm-ibx_d0_scraped", width = 4),
                                infoBoxOutput("mod_ddm-ibx_d0_eps", width = 4),
                                infoBoxOutput("mod_ddm-ibx_d0_payout", width = 4)
                              ),
                              
                              fluidRow(
                                column(width = 12,
                                       div("實務上常需對 D0 進行平滑化或還原本業配息，避免單一年度特別股利或景氣循環造成估值失真。",
                                           style = "font-size: 15px; font-weight: bold; color: #2C3E50; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;")
                                ),
                                
                                box(h4(tags$b("方法 1：目標配息率推算法")),
                                    p(helpText("適用於宣告改變股利政策，或未來獲利將發生重大變化的公司")),
                                    div("公式：預估 EPS × 目標配息率",
                                        style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"),
                                    numericInput("mod_ddm-est_eps", "預估/最新 EPS (元)", value = NA, step = 0.01),
                                    numericInput("mod_ddm-est_payout", "目標配息率 Payout Ratio (%)", value = NA, min = 0, max = 100, step = 0.01),
                                    actionButton("mod_ddm-calc_d0_payout", "計算並套用 D0", class = "btn-primary"),
                                    tags$br(),
                                    htmlOutput("mod_ddm-txt_d0_payout_res")
                                ),
                                
                                box(h4(tags$b("方法 2：景氣循環平滑法")),
                                    p(helpText("適用於航運、原物料等景氣循環股。系統將自動從現金流量表抓取歷史配息來平均。")),
                                    numericInput("mod_ddm-cycle_years", "抓取過去幾年平均？", value = 5, min = 1, max = 10, step = 0.01),
                                    actionButton("mod_ddm-calc_d0_average", "計算並套用平均 D0", class = "btn-primary"),
                                    tags$br(),
                                    htmlOutput("mod_ddm-txt_d0_avg_res")
                                )
                              )
                     )
              )
      ),
      
      # ==========================================
      # DCF Calculator 分頁
      # ==========================================
      tabItem(tabName = "dcf_calculator",
              tabBox(width = "auto",
                     tabPanel("", 
                              fluidRow(
                                column(width = 6,
                                       radioButtons("dcf_mode", "選擇 DCF 估值模型：",
                                                    choices = list(
                                                      "明確預測 + Gordon 終值" = "gordon",
                                                      "二階段成長法 (Two-Stage Model)" = "two_stage"
                                                    ),
                                                    selected = APP_DEFAULTS$dcf_mode)
                                ),
                                column(width = 6, numericInput("years", "預測年數 n", value = APP_DEFAULTS$years, min = 1, max = 30))
                              )
                     )
              ),
              
              tabBox(title = "DISCOUNTED CASH FLOW", width = "auto",
                     tabPanel("DCF Overview",
                              fluidRow(
                                column(width = 12,
                                       fluidRow(
                                         infoBoxOutput("ibx_stock_value_dcf", width = 6),
                                         infoBoxOutput("ibx_enterprise_value_dcf", width = 6)
                                       )
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       radioButtons(
                                         "dcf_chart_mode",
                                         "圖表顯示模式",
                                         choices = c(
                                           "單純模式（歷史＋預測 FCFF，無折現線）" = "simple",
                                           "顯示折現後價值（DCF）" = "with_dcf"
                                         ),
                                         selected = APP_DEFAULTS$dcf_chart_mode,
                                         inline = TRUE
                                       ),
                                       plotOutput("plt_dcf_trajectory", height = "420px"),
                                       h6(helpText("提示：圖含歷史 FCFF；切換模式可隱藏／顯示折現後 DCF 線。啟動時已自動計算，自訂參數後可再點試算。")),
                                       fluidRow(
                                         column(width = 6, actionButton("calc", "試算 DCF", class = "btn-success btn-block", style = "padding: 12px; font-weight: bold; font-size: 16px;")),
                                         column(width = 6, actionButton("reset_dcf", "回復預設", class = "btn-default btn-block", style = "padding: 12px; font-weight: bold; font-size: 16px;"))
                                       ),
                                       tags$div(style = "margin-top: 10px;", htmlOutput("vtxt_dcf_setting_details"))
                                )
                              )
                     ),
                     
                     tabPanel("DCF Calculation Details",
                              fluidRow(
                                column(width = 12,
                                       plotOutput("mod_fcf-fcf_plot", height = "350px"),
                                       htmlOutput("mod_fcf-txt_fcf_raw_data") 
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
                                
                                box(
                                  title = tagList(icon("location-arrow"), "核心參數位置"),
                                  width = 12, status = "warning", solidHeader = TRUE,
                                  tags$ul(
                                    style = "margin:0; padding-left:18px; line-height:1.55;",
                                    tags$li("DCF／RI 終值 SGR 與 WACC：側欄 Get Started"),
                                    tags$li("CapEx／ΔNWC 前瞻佔營收比：本頁 FCFF 分頁（驅動預測表）"),
                                    tags$li("DDM 股利成長率：可在 DDM 分頁單獨覆寫")
                                  )
                                )
                              )
                     ),
                     
                     # 在 ui.R 的某個 tabBox 或 navbarMenu 中：
                     fcf_projection_module_ui(id = "mod_fcf"),
                     
                     tabPanel("WACC",
                              icon = icon("balance-scale"),
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
                              
                              fluidRow(
                                div("WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)",
                                    style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"),
                                box(h4("WACC 估算"),
                                    numericInput("wacc_re", "股權成本 rₑ (%)", value = APP_DEFAULTS$wacc_re, min = 0, step = 0.01),
                                    checkboxInput("use_estimated_re", "採用估算 rₑ（來自CAPM）", value = APP_DEFAULTS$use_est_re),
                                    numericInput("wacc_rd", "負債成本 rᵈ (%)", value = APP_DEFAULTS$wacc_rd, min = 0, step = 0.01),
                                    numericInput("wacc_tax", "所得稅率 T (%)", value = APP_DEFAULTS$wacc_tax, min = 0, max = 100, step = 0.01),
                                    actionButton("calc_wacc", "計算 WACC", class = "btn-primary"),
                                    tags$br(), htmlOutput("wacc_result")
                                ),
                                box(h4("CAPM 估算 rₑ"),
                                    numericInput("capm_rf", "無風險利率 Rf (%)", value = APP_DEFAULTS$capm_rf, step = 0.01),
                                    numericInput("capm_beta", "Beta (β)", value = APP_DEFAULTS$capm_beta, step = 0.01),
                                    checkboxInput(
                                      "use_industry_beta",
                                      tags$span(style = "font-weight: bold;", "套用產業平均值（Beta）"),
                                      value = isTRUE(APP_DEFAULTS$use_industry_beta)
                                    ),
                                    helpText("預設跟 Dashboard → Finance Summary 的 Beta (5Y Monthly)；勾選才用產業平均；亦可直接手動輸入覆寫（換股票代碼後會重新跟 Summary 同步）。"),
                                    numericInput("capm_rm", "市場報酬率 Rm (%)", value = APP_DEFAULTS$capm_rm, step = 0.01),
                                    actionButton("calc_capm", "估算 rₑ（CAPM）", class = "btn-primary"),
                                    tags$br(), htmlOutput("capm_result")
                                )
                              )
                     )
              )
      ),
      
      # 🌟 呼叫 RI 模型分頁介面
      ri_module_ui("mod_ri"),
      
      # 🌟 呼叫 P/B／資產估值分頁介面
      pb_asset_module_ui("mod_pb"),
      
      tabItem(tabName = "sensitivity",
              
              decision_ui("main_decision"),
              
              tabBox(title = "SENSITIVITY", width = "auto",
                     fluidRow(
                       column(
                         width = 12,
                         h4("敏感度分析矩陣 (Sensitivity Analysis)"),
                         uiOutput("sensitivity_model_rec"),
                         p(helpText("軸心採用 Get Started／Dashboard 目前的 SGR 與 WACC；觀察鄰近組合下的每股內在價值變化。CapEx／ΔNWC 前瞻比率請至 DCF → FCFF 設定。"))
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
              )
      ),
      
      tabItem(tabName = "backtest",
              withMathJax(),
              h2("量化回測實驗室 (Backtest Zone)"),
              .bt_section_intro(
                "流程：搜尋股票 → 設定 DCF 此刻參數 → 確認過濾門檻 → 啟動回測。系統以歷史財報 Point-in-Time 重建合理價，驗證策略是否創造 Alpha（結果僅存 Session）。"
              ),

              # 1) 績效指標置頂（Alpha + 摘要整併）
              fluidRow(
                box(
                  title = tagList(icon("trophy"), "回測績效指標"),
                  width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("perf_metrics")
                )
              ),

              # 2) 淨值圖 + 執行面板
              fluidRow(
                box(
                  title = tagList(icon("chart-area"), "策略淨值比較圖"),
                  width = 8, status = "info", solidHeader = TRUE,
                  plotlyOutput("bt_equity_plot", height = "400px") %>% withSpinner(),
                  tags$ul(
                    style = "margin: 10px 0 0 0; padding-left: 18px; font-size: 12px; color: #666; line-height: 1.55;",
                    tags$li(tags$b("紅線｜純基本面價值"), " MOS／Great Filter 曝險加權的交易淨值（與 Buy&Hold 同基準）。牛市常因刻意現金部位落後，屬風控設計。"),
                    tags$li(tags$b("藍線｜情緒波動價值"), " 在紅線曝險上疊加情緒乘數（僅能在 Exp_A 的 75%～125% 內調整）。"),
                    tags$li(tags$b("綠線"), " Buy & Hold（全程 100% 持股）；", tags$b("灰虛線"), " SPY 基準。"),
                    tags$li("合理價路徑請看下方 Historical Fair Value Timeline，勿與淨值圖混比。")
                  )
                ),
                box(
                  title = tagList(icon("play-circle"), "執行面板"),
                  width = 4, status = "warning", solidHeader = TRUE,
                  class = "ynow-bt-run-panel",
                  radioButtons(
                    "bt_fv_model",
                    "回測用評價模型",
                    inline = FALSE,
                    choices = c(
                      "DCF（自由現金流折現）" = "dcf",
                      "DDM（股利折現）" = "ddm",
                      "RI（剩餘收益）" = "ri",
                      "P/B（本淨比）" = "pb",
                      "綜合均值" = "composite"
                    ),
                    selected = "dcf"
                  ),
                  .bt_hint("決定 MOS／訊號與 Fair Value 時間軸的評價路徑，並驅動「純基本面價值」曝險。"),
                  checkboxInput(
                    "bt_param_auto",
                    "自動同步參數（換股時依財報推導）",
                    value = TRUE
                  ),
                  .bt_hint(
                    "模式開關：勾選後，搜尋／載入新公司時會自動覆寫 Great Filter 門檻、曝險／情緒權重，並對齊上方推薦估值模型。手動改參數會自動取消勾選。"
                  ),
                  actionButton(
                    "bt_refresh_params", "立即依目前公司重算一次",
                    icon = icon("sync"), class = "btn-default btn-block",
                    style = "margin-bottom: 10px;"
                  ),
                  .bt_hint(
                    "單次動作：立刻用目前公司財報重算門檻／權重（可在取消自動後使用，不想持續自動覆寫時按一次即可）。"
                  ),
                  actionButton(
                    "run_bt", "啟動量化回測",
                    class = "btn-warning btn-lg btn-block",
                    style = "margin-bottom: 0;"
                  ),
                  tags$div(
                    class = "ynow-bt-run-note",
                    "季頻再平衡 · Rolling β 折現 · 依所選評價模型 PIT 重建。"
                  ),
                  uiOutput("bt_run_status")
                )
              ),

              # 3) 核心驗證圖
              fluidRow(
                box(
                  title = tagList(icon("balance-scale"), "Historical Fair Value Timeline（核心圖）"),
                  width = 12, status = "primary", solidHeader = TRUE,
                  .bt_section_intro(
                    "Market Price vs 動態重建 Fair Value（依所選估值模型；Ke／WACC 用各再平衡日 Rolling β）。僅使用公告財年 ≤ 回測日的資料，避免 Look-ahead Bias。"
                  ),
                  uiOutput("bt_valuation_summary"),
                  plotlyOutput("bt_hfv_timeline", height = "380px") %>% withSpinner(),
                  uiOutput("bt_signal_explain")
                )
              ),

              fluidRow(
                box(
                  title = tagList(icon("percentage"), "Exposure History（純基本面價值／情緒波動價值）"),
                  width = 6, status = "danger", solidHeader = TRUE, collapsible = TRUE,
                  uiOutput("bt_exposure_stats"),
                  plotlyOutput("bt_exposure_plot", height = "260px") %>% withSpinner()
                ),
                box(
                  title = tagList(icon("search-dollar"), "為何輸給 Buy & Hold？"),
                  width = 6, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  uiOutput("bt_bh_gap")
                )
              ),

              # 4) 細部參數設定
              fluidRow(column(width = 12, uiOutput("bt_param_notes"))),

              fluidRow(
                tags$div(
                  class = "ynow-bt-params",
                  tabBox(
                    title = tagList(icon("sliders-h"), "策略參數設定"),
                    width = 12,
                    tabPanel(
                      title = tagList(icon("filter"), "基本面過濾"),
                      .bt_section_intro("Great Filter：季頻再平衡日四項皆過才允許持倉；否則 A／B 皆空手。"),
                      fluidRow(
                        column(3, tipify(numericInput("bt_net_margin", "淨利率門檻 (%)", 5),
                                         "自動模式取該公司歷史淨利率約一半。", placement = "top")),
                        column(3, tipify(numericInput("bt_rev_growth", "營收成長門檻 (%)", 25),
                                         "自動模式取歷史營收成長約一半。", placement = "top")),
                        column(3, tipify(numericInput("bt_eps_growth", "EPS／淨利成長門檻 (%)", 15),
                                         "自動模式取淨利成長約一半。", placement = "top")),
                        column(3, tipify(numericInput("bt_fcf_cv", "FCF 變異係數上限 (%)", 20),
                                         "自動模式取 FCF CV × 1.25。", placement = "top"))
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("balance-scale"), "純基本面價值"),
                      .bt_section_intro(
                        "淨值圖紅線「純基本面價值」＝依 MOS 滯後曝險 × Great Filter 的交易淨值。合理價路徑在 Fair Value 時間軸；最高持股約 90%，牛市輸給 Buy&Hold 多半是現金拖累。"
                      ),
                      fluidRow(
                        column(
                          6,
                          sliderInput("bt_w_vg", "MOS／Value Gap 權重（曝險診斷）", 0, 1, 0.7, step = 0.01),
                          .bt_hint("影響「純基本面價值」與「情緒波動價值」的基準倉位。")
                        ),
                        column(
                          6,
                          tags$div(
                            style = "margin-top: 20px; padding: 12px; background: #fcf8e3; border: 1px solid #f0e6b2; border-radius: 5px; font-size: 12px; color: #8a6d3b; line-height: 1.55;",
                            tags$b("滯後曝險（診斷用）"), tags$br(),
                            "MOS≥30%→90%；≥10%→65%；≥0%→40%；≥−10%→15%；否則空手。"
                          )
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("bolt"), "情緒波動價值"),
                      .bt_section_intro(
                        "情緒只能調整權重：最終曝險夾在 Exp_A（純基本面價值倉位）的 75%～125%，且 Exp_A=0 時必須空手。"
                      ),
                      fluidRow(
                        column(6, sliderInput("bt_w_mom", "動能相對權重", 0, 1, 0.4, step = 0.01),
                               .bt_hint("與 RSI 正規化後組成情緒分數。")),
                        column(6, sliderInput("bt_w_rsi", "RSI 相對權重", 0, 1, 0.3, step = 0.01),
                               .bt_hint("情緒乘數限制在 0.75～1.25。"))
                      )
                    )
                  )
                )
              ),

              # 5) MOS／FV／參數高原驗證區塊置於頁面最下方
              fluidRow(
                box(
                  title = tagList(icon("flask"), "回測驗證：MOS／Fair Value／參數高原"),
                  width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  fluidRow(
                    column(
                      6,
                      tags$h5(tags$b("MOS 有效性驗證")),
                      .bt_hint("依 MOS 分組統計 1Y／3Y／5Y 前瞻報酬：MOS 愈高是否報酬愈好？"),
                      tableOutput("bt_mos_table")
                    ),
                    column(
                      6,
                      tags$h5(tags$b("Fair Value 預測能力")),
                      uiOutput("bt_fv_edge"),
                      tableOutput("bt_fv_table")
                    )
                  ),
                  tags$hr(style = "margin: 16px 0;"),
                  tags$h5(tags$b("參數高原（敏感度）")),
                  .bt_hint("微擾 WACC／SGR／年數，觀察「純基本面價值」合理價終值指數的相對變動。"),
                  uiOutput("bt_plateau"),
                  tableOutput("bt_plateau_table")
                )
              ),

              # 6) 最底層：數據來源與計算過程註解 + 下載
              fluidRow(
                box(
                  title = tagList(icon("book"), "回測數據來源與計算過程（方法論註解）"),
                  width = 12, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = FALSE,
                  uiOutput("bt_methodology_notes"),
                  tags$div(
                    style = "margin-top: 12px;",
                    downloadButton(
                      "download_bt_methodology",
                      "下載方法論說明（Markdown）",
                      icon = icon("download"),
                      class = "btn-primary",
                      style = "font-weight: 600;"
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # ℹ️ About 分頁 (系統介紹與評價方法論)
      # ==========================================
      tabItem(tabName = "about",
              fluidRow(
                column(width = 12,
                       h2(tags$b("About The YNow App")),
                       p("The YNow App 是一個整合自動化資料抓取、深度財報分析與動態估值模型 (DCF & DDM) 的投資輔助系統。我們致力於將華爾街機構級的財務建模邏輯，轉化為直覺、視覺化的決策工具。"),
                       tags$hr()
                )
              ),
              
              fluidRow(
                column(width = 12,
                       h3(tags$b("Financial Fraud Red Flags (財務舞弊警訊)")),
                       p("本系統內建五項核心排雷機制，透過交叉比對現金流與獲利品質，自動偵測潛在的地雷股："),
                       tags$ul(
                         tags$li(tags$b("無自由現金流 (No FCF)："), "長期 FCF 為負，代表企業無法靠自身營運創造現金，需依賴外部融資。"),
                         tags$li(tags$b("無營業現金流 (No OCF)："), "OCF 為負是極度危險的訊號，代表核心本業正在失血。"),
                         tags$li(tags$b("獲利未實現 (OCF < Net Income)："), "俗稱「紙上富貴」，損益表雖然賺錢，但現金沒有實際流入公司，可能存在應收帳款作帳疑慮。"),
                         tags$li(tags$b("虛假獲利 (Net Income > 0 but OCF < 0)："), "最經典的舞弊特徵，強烈暗示獲利品質不佳。"),
                         tags$li(tags$b("高財務槓桿 (Debt/Equity > 2)："), "負債比過高，在升息循環或景氣下行時面臨極大的流動性風險。")
                       ),
                       tags$hr()
                )
              ),
              
              fluidRow(
                column(width = 12,
                       h3(tags$b("Valuation Methodology (評價方法論)")),
                       
                       p("在進行企業估值時，選擇正確的模型與計算數字一樣重要。以下是本系統支援的三大評價邏輯與其適用場景："),
                       
                       tabBox(title = "模型選擇決策指南", width = 12, side = "left",
                              
                              # Tab 1: 方法論比較矩陣 (表格)
                              tabPanel("Decision Matrix", icon = icon("table"),
                                       tags$div(style = "overflow-x: auto; margin-bottom: 18px;",
                                                HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                                        <thead style='background-color: #2C3E50; color: white;'>
                                                          <tr>
                                                            <th>對照項目</th>
                                                            <th>DDM（股利 Gordon）</th>
                                                            <th>DCF（明確預測 + Gordon 終值）</th>
                                                          </tr>
                                                        </thead>
                                                        <tbody>
                                                          <tr>
                                                            <td><b>現金流</b></td>
                                                            <td>每股股利 D（股權請求權）</td>
                                                            <td>企業自由現金流 FCFF（全體資金提供者）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>折現率</b></td>
                                                            <td>Ke（CAPM 股權成本）</td>
                                                            <td>WACC（加權平均資本成本）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>成長率 g</b></td>
                                                            <td>股利永續成長率（可與中央 SGR 同步或覆寫）</td>
                                                            <td>FCFF 終值成長率 SGR（相對 WACC）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>「Gordon」含義</b></td>
                                                            <td>整段估值：P₀ = D₁ / (Ke − g)</td>
                                                            <td>僅終值：TV = FCFₙ(1+g)/(WACC−g)；另加 n 年明確預測折現</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>輸出</b></td>
                                                            <td>直接為每股合理價</td>
                                                            <td>先得企業價值 EV，再加減淨現金／負債後 ÷ 股數</td>
                                                          </tr>
                                                        </tbody>
                                                      </table>")
                                       ),
                                       tags$div(style = "overflow-x: auto;",
                                                HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                                        <thead style='background-color: #2C3E50; color: white;'>
                                                          <tr>
                                                            <th>考慮維度</th>
                                                            <th>股利折現模型 (DDM)</th>
                                                            <th>自由現金流 (FCFF / FCFE)</th>
                                                            <th>剩餘收益模型 (RI)</th>
                                                          </tr>
                                                        </thead>
                                                        <tbody>
                                                          <tr>
                                                            <td><b>主要資料來源</b></td>
                                                            <td>現金流量表（現金股利支付）</td>
                                                            <td>現金流量表（營運與資本支出）</td>
                                                            <td>損益表與資產負債表（淨利與權益）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>投資者身分 / 觀點</b></td>
                                                            <td>少數股東（無決策與控制權）</td>
                                                            <td>控股股東 / 併購者（有決策權）</td>
                                                            <td>皆可（尤其適用於負 FCF）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>企業發展階段</b></td>
                                                            <td>成熟期、穩健期（如公用事業）</td>
                                                            <td>成長期、擴張期（如科技股）</td>
                                                            <td>各階段皆可，尤其是資產密集型</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>對配息政策依賴度</b></td>
                                                            <td><span class='label label-danger'>極高</span></td>
                                                            <td><span class='label label-success'>低</span></td>
                                                            <td><span class='label label-success'>極低</span></td>
                                                          </tr>
                                                        </tbody>
                                                      </table>")
                                       )
                              ),
                              
                              # Tab 3: DDM 模型解說
                              tabPanel("Dividend Discount Model (DDM)", icon = icon("hand-holding-usd"),
                                       h4(tags$b("股利折現模型（股利 Gordon）")),
                                       p("DDM 將普通股價值視為未來現金股利的現值。現金流是股利、折現率是 Ke，與以 FCFF／WACC 為核心的 DCF 屬不同層級。"),
                                       tags$ul(
                                         tags$li(tags$b("$$P_0 = \\frac{D_1}{K_e - g} = \\frac{D_0 \\times (1 + g)}{K_e - g}$$"))
                                       ),
                                       p("股利成長率 g 可與中央終值 SGR 同步，亦可在 DDM 分頁單獨覆寫。基本面法可參考 $$g = ROE \\times Retention\\ Ratio$$，但不宜與 FCFF 終值 g 強制畫上等號。")
                              ),
                              
                              # Tab 2: DCF 模型解說
                              tabPanel("Discounted Cash Flow (DCF)", icon = icon("money-bill-wave"),
                                       h4(tags$b("自由現金流折現模型 (FCFF)")),
                                       p("DCF 關注企業造血能力：將未來 FCFF 以 WACC 折現得到企業價值，再橋接至股權價值與每股價格。本 app 的「Gordon」模式為明確預測期加上 Gordon 終值，而非單期 EV = FCF₁/(WACC−g)。"),
                                       tags$ul(
                                         tags$li(tags$b("$$FCFF = Net Income + D\\&A - \\Delta NWC - CapEx$$")),
                                         tags$li(tags$b("$$Enterprise\\ Value = \\sum \\frac{FCFF_t}{(1+WACC)^t} + \\frac{Terminal\\ Value}{(1+WACC)^n}$$")),
                                         tags$li(tags$b("$$Terminal\\ Value = \\frac{FCFF_n \\times (1 + g)}{WACC - g}$$"))
                                       ),
                                       p("兩階段模式則在高速成長期後，將終值成長率收斂至 SGR；約束條件為 g < WACC（不是 Ke）。")
                              )
                       ),
                       
                       uiOutput("main_decision-ui_valuation_compare")
                )
              )
      )
    )
  )
)
