# ==========================================
# ui.R - 前端介面設計
# ==========================================

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
               downloadButton("download_report", "下載完整分析報告", 
                              style = "width: 100%; font-weight: bold; background-color: #1a1a1a; color: #ffffff; border: 1px solid #000000; box-shadow: none; text-shadow: none;")
           )
    ),
    
    column(width = 12,
           div(style = "padding: 15px; border-radius: 5px; border-left: 4px",
               tags$b("ℹ Data Source:"), tags$br(),
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
        
        /* KPI 格子黃金比例佈局 */
        .small-box {
          aspect-ratio: 1.618 / 1 !important; 
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
          min-height: 120px !important; 
          height: auto !important; 
          border-radius: 8px !important;
          margin-bottom: 15px !important;
          box-shadow: 0 4px 6px rgba(0,0,0,0.05) !important;
        }
        
        .small-box .inner {
          padding: 10px 15px !important;
          text-align: center !important;
        }
        
        .small-box .inner h3 {
          font-size: clamp(22px, 4.2vw, 38px) !important; 
          font-weight: 800 !important;
          margin-bottom: 8px !important;
        }
        
        .small-box .inner p {
          font-size: clamp(12px, 1.2vw, 14px) !important;
          opacity: 0.9;
          font-weight: 500 !important;
        }
        
        .small-box .icon-large {
          font-size: 60px !important;
          top: 15px !important;
          right: 15px !important;
          opacity: 0.15 !important;
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
                 function showSuggest() {
                   var el = document.getElementById('sc_ticker_suggest');
                   if (el && el.children.length) el.style.display = 'block';
                 }
                 function hideSuggest() {
                   var el = document.getElementById('sc_ticker_suggest');
                   if (el) el.style.display = 'none';
                 }
                 $(document).on('input focus', '#sc', function() {
                   var v = $(this).val() || '';
                   Shiny.setInputValue('ticker_typeahead', v, {priority: 'event'});
                   showSuggest();
                 });
                 $(document).on('blur', '#sc', function() {
                   setTimeout(hideSuggest, 180);
                 });
                 $(document).on('mousedown', '#sc_ticker_suggest .ynow-suggest-item', function(e) {
                   e.preventDefault();
                   var sym = $(this).data('symbol');
                   if (sym) {
                     $('#sc').val(sym).trigger('input').trigger('change');
                     Shiny.setInputValue('sc', sym, {priority: 'event'});
                   }
                   hideSuggest();
                 });
                 $(document).on('shiny:value', function(e) {
                   if (e.name === 'sc_ticker_suggest_ui') setTimeout(showSuggest, 30);
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
                              p("This section imports Finance Summaries from Yahoo Finance"),
                              dataTableOutput("tbFinanceSummary"),
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
                       column(width = 12, h4("Balance Sheet KPI"), valueBoxOutput(NS("kpi", "vbx_eqt_multiplier"))),
                       column(width = 12, h4("Income Statement KPI"),
                              valueBoxOutput(NS("kpi", "vbx_net_profit_margin")),
                              valueBoxOutput(NS("kpi", "vbx_gross_profit_margin")),
                              valueBoxOutput(NS("kpi", "vbx_opex_ratio")),
                              valueBoxOutput(NS("kpi", "vbx_rev_growth")),
                              valueBoxOutput(NS("kpi", "vbx_gross_profit_growth"))
                       ),
                       column(width = 12, h4("Cash Flow KPI"),
                              valueBoxOutput(NS("kpi", "vbx_op_cash_flow_growth")),
                              valueBoxOutput(NS("kpi", "vbx_inv_cash_flow_growth")),
                              valueBoxOutput(NS("kpi", "vbx_fin_cash_flow_growth"))
                       )
                     )),
                     
                     tabPanel("Crossover KPIs", fluidRow(
                       column(width = 12,
                              valueBoxOutput(NS("kpi", "vbx_ROA")),
                              valueBoxOutput(NS("kpi", "vbx_ROE")),
                              valueBoxOutput(NS("kpi", "vbx_asset_turnover"))
                       ),
                       column(width = 12, valueBoxOutput(NS("kpi", "vbx_ocf_net_income")))
                     )),
                     
                     tabPanel("Annotation", fluidRow(
                       column(width = 12,
                              div(style = "margin-bottom: 20px; padding: 12px; background: #fdfdfd; border: 1px dashed #ccc; border-radius: 6px; display: flex; align-items: center; justify-content: center; font-size: 13px;",
                                  span(style = "font-weight: bold; margin-right: 15px;", "同業比較圖例:"),
                                  span(icon("circle", style = "color: #0073b7;"), " 🔵 高於標準 (Better) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #00a65a;"), " ⚫ 符合標準 (Standard) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #dd4b39;"), " 🔴 低於標準 (Worse) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #333;"), " ⚫ 無資料 / 錯誤")
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
                                       numericInput("mod_ddm-g", "永續成長率 (sgr) %", value = APP_DEFAULTS$ddm_g),
                                       numericInput("mod_ddm-ke", "要求報酬率 (Ke) %", value = APP_DEFAULTS$ddm_ke),
                                       
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
                                                    choices = list("永續成長法 (Gordon Growth Model)" = "gordon",
                                                                   "二階段成長法 (Two-Stage Model)" = "two_stage"),
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
                                         selected = "with_dcf",
                                         inline = TRUE
                                       ),
                                       plotOutput("plt_dcf_trajectory", height = "420px"),
                                       h6(helpText("提示：圖含歷史 FCFF；切換模式可隱藏／顯示折現後 DCF 線。啟動時已自動計算，自訂參數後可再點試算。")),
                                       fluidRow(
                                         column(width = 6, actionButton("calc", "▶ 試算 DCF", class = "btn-success btn-block", style = "padding: 12px; font-weight: bold; font-size: 16px;")),
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
                                
                                box(title = "DCF 估值核心參數設定", 
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
                                    
                                    # --- 共用終值永續成長率（避免 gordon / two_stage 重複 ID）---
                                    numericInput("sgr", "終值永續成長率 SGR (%)", value = APP_DEFAULTS$sgr),
                                    
                                    # --- 模式 A: Gordon ---
                                    conditionalPanel(
                                      condition = "input.dcf_mode == 'gordon'",
                                      h4(tags$b("Gordon 永續成長假設")),
                                      numericInput("wacc_gordon", "折現率 WACC (%)", value = APP_DEFAULTS$wacc_gordon, step = 0.01)
                                    ),
                                    
                                    # --- 模式 B: Two-Stage ---
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
                                    checkboxInput("use_calculated_wacc", "✅ 套用系統估算 WACC", value = APP_DEFAULTS$use_calc_wacc)
                                )
                              )
                     ),
                     
                     # 在 ui.R 的某個 tabBox 或 navbarMenu 中：
                     fcf_projection_module_ui(id = "mod_fcf"),
                     
                     tabPanel("WACC",
                              icon = icon("balance-scale"),
                              fluidRow(
                                infoBoxOutput("ibx_wacc", width = 4),
                                infoBoxOutput("ibx_rd", width = 4), 
                                infoBoxOutput("ibx_re", width = 4)
                              ),
                              
                              # ✨ 新增：顯示 E, D, T 的數值列
                              fluidRow(
                                valueBoxOutput("vbx_equity_val", width = 4), # 股權市值 (E)
                                valueBoxOutput("vbx_debt_val", width = 4),   # 總負債 (D)
                                valueBoxOutput("vbx_tax_rate", width = 4)    # 有效稅率 (T)
                              ),
                              
                              fluidRow(
                                div("WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)",
                                    style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"),
                                box(h4("WACC 估算"),
                                    numericInput("wacc_re", "股權成本 rₑ (%)", value = APP_DEFAULTS$wacc_re, min = 0, step = 0.01),
                                    checkboxInput("use_estimated_re", "✅ 採用估算 rₑ（來自CAPM）", value = APP_DEFAULTS$use_est_re),
                                    numericInput("wacc_rd", "負債成本 rᵈ (%)", value = APP_DEFAULTS$wacc_rd, min = 0, step = 0.01),
                                    numericInput("wacc_tax", "所得稅率 T (%)", value = APP_DEFAULTS$wacc_tax, min = 0, max = 100, step = 0.01),
                                    actionButton("calc_wacc", "計算 WACC", class = "btn-primary"),
                                    tags$br(), htmlOutput("wacc_result")
                                ),
                                box(h4("CAPM 估算 rₑ"),
                                    numericInput("capm_rf", "無風險利率 Rf (%)", value = APP_DEFAULTS$capm_rf, step = 0.01),
                                    numericInput("capm_beta", "Beta (β) [套用產業: 預設]", value = APP_DEFAULTS$capm_beta, step = 0.01),
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
                       column(width = 12,
                              h4("敏感度分析矩陣 (Sensitivity Analysis)"),
                              p(helpText("觀察不同 WACC 與 永續成長率 (sgr) 組合下，推估的每股內在價值。")),
                              tableOutput("dcf_sensitivity_table")
                       ),
                       br(), 
                       
                       box(width = 12, status = "danger", solidHeader = TRUE,
                           fluidRow(
                             column(width = 6,
                                    h4(tags$b("CapEx 預估資本支出佔營收比")),
                                    numericInput("var_capex_rate", "CapEx / Revenue (%):", value = NA, step = 0.01),
                                    htmlOutput("txt_hist_capex"),
                                    h6(helpText("註：若為空白，系統將自動套用上方歷史預估值。"))
                             ),
                             column(width = 6,
                                    h4(tags$b("ΔNWC 預估營運資本佔營收變動比")),
                                    numericInput("var_nwc_rate", "ΔNWC / ΔRevenue (%)", value = NA, step = 0.01),
                                    htmlOutput("txt_hist_nwc"),
                                    h6(helpText("註：若為空白，系統將自動套用上方歷史預估值。"))
                             )
                           )
                       )
                     )
              )
      ),
      
      tabItem(tabName = "backtest",
              withMathJax(),
              h2("量化回測實驗室 (Backtest Zone)"),
              helpText("參數預設依「目前搜尋公司」的財報／動能／安全邊際自動推導；可切換手動覆寫。"),

              fluidRow(
                box(
                  title = tagList(icon("chart-area"), "策略淨值比較圖 (Equity Curve Comparison)"),
                  width = 12, status = "info", solidHeader = TRUE,
                  plotlyOutput("bt_equity_plot", height = "450px") %>% withSpinner(),
                  helpText("藍＝模式 A（情緒增強），紅＝模式 B（純基本面），灰虛線＝大盤基準（SPY），綠＝該股買進持有。")
                )
              ),

              fluidRow(
                box(
                  title = tagList(icon("sliders-h"), "參數模式"),
                  width = 12, status = "success", solidHeader = TRUE,
                  radioButtons(
                    "bt_param_mode", NULL, inline = TRUE,
                    choices = c("自動（依公司推導）" = "auto", "手動覆寫" = "manual"),
                    selected = "auto"
                  ),
                  uiOutput("bt_param_notes"),
                  actionButton("bt_refresh_params", "依目前公司重算參數", icon = icon("sync"))
                )
              ),

              fluidRow(
                box(
                  title = tagList(icon("filter"), "1. 大過濾器：估值路徑分流 (The Great Filter)"),
                  width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  column(3, tipify(numericInput("bt_net_margin", "淨利率門檻 (%)", 5),
                                   "獲利能力門檻。自動模式取該公司歷史淨利率約一半。", placement = "top")),
                  column(3, tipify(numericInput("bt_rev_growth", "營收成長門檻 (%)", 25),
                                   "自動模式取該公司歷史營收成長約一半。", placement = "top")),
                  column(3, tipify(numericInput("bt_eps_growth", "EPS/NI 成長門檻 (g, %)", 15),
                                   "自動模式取該公司淨利成長約一半。", placement = "top")),
                  column(3, tipify(numericInput("bt_fcf_cv", "FCF 變異係數上限", 20),
                                   "自動模式取該公司 FCF CV × 1.25。", placement = "top"))
                )
              ),

              fluidRow(
                tabBox(
                  title = tagList(icon("balance-scale"), "2. 模式 A 與 B 權重因子"),
                  width = 8,
                  tabPanel("模式 A：情緒增強型",
                           helpText("權重依動能／RSI／MOS 自動分配；適合捕捉趨勢與過熱訊號。"),
                           sliderInput("bt_w_mom", "短期動能 (Momentum) 權重", 0, 1, 0.4, step = 0.01),
                           bsTooltip("bt_w_mom", "基於約 20 日報酬率捕捉趨勢。", "right"),
                           sliderInput("bt_w_rsi", "市場情緒 (RSI) 權重", 0, 1, 0.3, step = 0.01),
                           bsTooltip("bt_w_rsi", "RSI 過高時降低曝險，防止追高。", "right")
                  ),
                  tabPanel("模式 B：純基本面型",
                           helpText("核心：通過大過濾器 + 估值偏離（MOS）與均線位置。"),
                           sliderInput("bt_w_vg", "估值偏離 (Valuation Gap) 權重", 0, 1, 0.7, step = 0.01),
                           bsTooltip("bt_w_vg", "安全邊際越高，基本面倉位傾向越高。", "right"),
                           tags$div(style = "color: #d9534f; font-weight: bold; padding: 10px; background: #fcf8e3; border-radius: 5px;",
                                    icon("exclamation-triangle"), "價值陷阱警示：若低估長期不漲，請參考模式 A 判斷市場共識何時轉向。")
                  )
                ),
                box(
                  title = "執行操作", width = 4, status = "warning",
                  actionButton("run_bt", "▶ 啟動量化回測", class = "btn-warning btn-lg btn-block"),
                  hr(),
                  p("將使用目前 Ticker 過去約 5 年日線（月頻再平衡），並結合該公司財報特徵產出淨值曲線。"),
                  uiOutput("bt_run_status")
                )
              ),

              uiOutput("perf_metrics")
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
                       h3(tags$b("🚨 Financial Fraud Red Flags (財務舞弊警訊)")),
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
                       h3(tags$b("📚 Valuation Methodology (評價方法論)")),
                       
                       p("在進行企業估值時，選擇正確的模型與計算數字一樣重要。以下是本系統支援的三大評價邏輯與其適用場景："),
                       
                       tabBox(title = "模型選擇決策指南", width = 12, side = "left",
                              
                              # Tab 1: 方法論比較矩陣 (表格)
                              tabPanel("Decision Matrix", icon = icon("table"),
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
                                       h4(tags$b("股利折現模型 (Gordon Growth Model)")),
                                       p("DDM 將企業價值視為投資人未來能領到的「所有現金股利」的現值。它完全排除了會計作帳的干擾，因為發放出現金是無法作假的。"),
                                       tags$ul(
                                         tags$li(tags$b("$$P_0 = \\frac{D_1}{K_e - g} = \\frac{D_0 \\times (1 + g)}{K_e - g}$$"))
                                       ),
                                       p("本系統具備動態的基礎成長率 (Fundamental Growth Rate) 推算引擎：$$g = ROE \\times Retention\\ Ratio$$，確保 DDM 的成長假設具備強大的基本面支撐。")
                              ),
                              
                              # Tab 2: DCF 模型解說
                              tabPanel("Discounted Cash Flow (DCF)", icon = icon("money-bill-wave"),
                                       h4(tags$b("自由現金流折現模型 (FCFF)")),
                                       p("DCF 關注的是企業「真實的造血能力」。它將企業未來能創造的所有自由現金流 (Free Cash Flow to Firm, FCFF)，使用加權平均資本成本 (WACC) 折現回今天的價值。"),
                                       tags$ul(
                                         tags$li(tags$b("$$FCFF = Net Income + D\\&A - \\Delta NWC - CapEx$$")),
                                         tags$li(tags$b("$$Enterprise\\ Value = \\sum \\frac{FCFF_t}{(1+WACC)^t} + \\frac{Terminal\\ Value}{(1+WACC)^n}$$"))
                                       ),
                                       p("本系統採用業界標準的「兩階段模型」，前 1~5 年使用明確的營收成長率推算，第 5 年後切換為永續成長率 (SGR)。")
                              )
                       ),
                       
                       uiOutput("main_decision-ui_valuation_compare")
                )
              )
      )
    )
  )
)
