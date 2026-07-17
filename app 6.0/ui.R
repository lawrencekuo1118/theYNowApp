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
    column(
      width = 12,
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
      hr()
    ),
    column(
      width = 12,
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard", lib = "glyphicon")),
        menuItem("Calculator", tabName = "calculator", icon = icon("equalizer", lib = "glyphicon"), badgeLabel = "new", badgeColor = "green"),
        menuItem("Advance", tabName = "advance", icon = icon("equalizer", lib = "glyphicon"), badgeLabel = "new", badgeColor = "green"),
        menuItem("About", tabName = "about", icon = icon("info-sign", lib = "glyphicon"))
      ),
      hr()
    ),
    column(
      width = 12,
      h5("Recent Search:"),
      textOutput("recentsearch"),
      hr()
    ),
    column(width = 12, textOutput("today")),
    hr(),
    column(width = 12,
           div(style = "padding: 15px; border-radius: 5px; border-left: 4px solid #d9534f;",
               tags$b("ℹ️ Data Source:"), tags$br(),
               "This application integrates real-time financial data via web parsing and API resources, applying comprehensive models for valuation."
           )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML('.main-header .logo { font-weight: bold; }')),
      
      # 原本 selectize dropdown 高度限制
      tags$style(HTML("
    .selectize-dropdown-content {
      max-height: 300px !important;
      overflow-y: auto !important;
    }
    .selectize-dropdown {
      max-height: 300px !important;
    }
  "))
    ),
    
    tags$head(
      tags$style(HTML("
    .info-box .info-box-number {
      font-size: 150% !important;
      font-weight: bold;
    }
    
    /* 1. KPI 格子導入黃金比例佈局 (Golden Ratio) */
.small-box {
  /* 寬度由 Shiny Column 決定，高度則自動維持 1.618:1 */
  aspect-ratio: 1.618 / 1 !important; 
  
  display: flex !important;
  flex-direction: column !important;
  justify-content: center !important;
  
  /* 為了防止在極小螢幕下過扁，設定一個保底高度 */
  min-height: 120px !important; 
  height: auto !important; 
  
  border-radius: 8px !important;
  margin-bottom: 15px !important;
  box-shadow: 0 4px 6px rgba(0,0,0,0.05) !important;
}

/* 2. 數字位置優化：確保在黃金比例空間內垂直置中 */
.small-box .inner {
  padding: 10px 15px !important;
  text-align: center !important;
}

/* 3. 數字大小調整 */
.small-box .inner h3 {
  font-size: clamp(22px, 4.2vw, 38px) !important; /* 略微放大數字 */
  font-weight: 800 !important;
  margin-bottom: 8px !important;
}

/* 4. 副標題調整 */
.small-box .inner p {
  font-size: clamp(12px, 1.2vw, 14px) !important;
  opacity: 0.9;
  font-weight: 500 !important;
}

/* 5. 圖標 (Icon) 優化：稍微淡化，不干擾黃金比例的平衡 */
.small-box .icon-large {
  font-size: 60px !important;
  top: 15px !important;
  right: 15px !important;
  opacity: 0.15 !important;
}
  "))
    ),
    
    fluidRow(
      column(
        width = 8,
        titlePanel(h5("a lawrence kuo shiny app")),
        textInput("sc", "Stock Code", value = "AMZN"),
        h2(textOutput("txt_corpname"), style = "font-weight: bold; color: #333333; margin-top: 0;"),
        hr(),
        
        tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          actionButton("search", "Search", icon = icon("search")),
          tags$div(
            style = "font-weight: bold; font-size: 16px; color: #333;",
            h2(textOutput("txt_corpname", inline = TRUE), style = "font-weight: bold; color: #333333; margin-top: 0;"),
          )
        )
      )
    ),
    br(),
    
    fluidRow(
      infoBoxOutput("ibx_marketcap"),
      infoBoxOutput("ibx_stockprice"),
      infoBoxOutput("ibx_EPS")
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              tabBox(title = "Financial Reports",
                     width = "auto",
                     
                     tabPanel("Finance Summary",
                              p("This section imports Finance Summaries from Yahoo Finance"),
                              dataTableOutput("tbFinanceSummary"),
                              downloadButton('FS_download', "Download Finance Summary")
                     ),
                     
                     tabPanel("Income Statement",
                              p("This section imports Income Statements from Yahoo Finance"),
                              dataTableOutput("tbIncomeStatement"),
                              downloadButton('IS_download', "Download Income Statement")
                     ),
                     
                     tabPanel("Balance Sheet",
                              p("This section imports Balance Sheets from Yahoo Finance"),
                              dataTableOutput("tbBalanceSheet"),
                              downloadButton('BS_download', "Download Balance Sheet")
                     ),
                     
                     tabPanel("Cash Flow",
                              p("This section imports Cash Flow data from Yahoo Finance"),
                              selectInput("cf_type", "Select Cash Flow Type",
                                          choices = c("Operating Cash Flow", "Investing Cash Flow", "Financing Cash Flow")),
                              plotlyOutput("cf_plot"),
                              dataTableOutput("tbCashFlow"),
                              downloadButton('CF_download', "Download Cash Flow Data")
                     )
              ),
              
              pickerInput(
                inputId = "industry_choice",
                label = "Industry Standard Comparison",
                choices = sort(names(industry_standards)[names(industry_standards) != "" & !is.na(names(industry_standards))]),
                selected = "ecr.Ecommerce_Retail",
                options = list(
                  `live-search` = TRUE,
                  `size` = 10
                )
              ),
              
              h5("stock industry recommendations from Yahoo"),
              verbatimTextOutput("search_results"),
              
              tabBox(
                title = "Performance",
                width = "auto",
                
                # KPI by Sheet
                tabPanel("KPI by Sheet", fluidRow(
                  column(
                    width = 12, 
                    h4("Balance Sheet KPI"), 
                    valueBoxOutput(NS("kpi", "vbx_eqt_multiplier"))
                  ),
                  column(
                    width = 12,
                    h4("Income Statement KPI"),
                    valueBoxOutput(NS("kpi", "vbx_net_profit_margin")),
                    valueBoxOutput(NS("kpi", "vbx_gross_profit_margin")),
                    valueBoxOutput(NS("kpi", "vbx_opex_ratio")),
                    valueBoxOutput(NS("kpi", "vbx_rev_growth")),
                    valueBoxOutput(NS("kpi", "vbx_gross_profit_growth"))
                  ),
                  column(
                    width = 12,
                    h4("Cash Flow KPI"),
                    valueBoxOutput(NS("kpi", "vbx_op_cash_flow_growth")),
                    valueBoxOutput(NS("kpi", "vbx_inv_cash_flow_growth")),
                    valueBoxOutput(NS("kpi", "vbx_fin_cash_flow_growth"))
                  )
                )),
                
                # Crossover KPIs
                tabPanel("Crossover KPIs", fluidRow(
                  column(
                    width = 12,
                    valueBoxOutput(NS("kpi", "vbx_ROA")),
                    valueBoxOutput(NS("kpi", "vbx_ROE")),
                    valueBoxOutput(NS("kpi", "vbx_asset_turnover"))
                  ),
                  column(
                    width = 12,
                    valueBoxOutput(NS("kpi", "vbx_ocf_net_income"))
                  )
                )),
                
                # 景氣穩定指標表
                tabPanel("Stable Indicator List", fluidRow(
                  column(
                    width = 12,
                    div(style = "margin-bottom: 20px; padding: 12px; background: #fdfdfd; border: 1px dashed #ccc; border-radius: 6px; display: flex; align-items: center; justify-content: center; font-size: 13px;",
                        span(style = "font-weight: bold; margin-right: 15px;", "同業比較圖例:"),
                        span(icon("circle", style = "color: #0073b7;"), " 🔵 高於標準 (Above) ", style = "margin-right: 15px;"),
                        span(icon("circle", style = "color: #00a65a;"), " 🟢 符合標準 (Standard) ", style = "margin-right: 15px;"),
                        span(icon("circle", style = "color: #dd4b39;"), " 🔴 低於標準 (Below) ", style = "margin-right: 15px;"),
                        span(icon("circle", style = "color: #333;"), " ⚫ 無資料 / 錯誤")
                    )
                  ),
                  column(width = 12,
                         h4("Stable Indicator List 景氣穩定指標表"),
                         tableOutput("stable_indicator_table")
                  )
                ))
              ),
              
              fluidRow(
                column(
                  width = 12,
                  div(
                    # 設定紅底白字與內邊距
                    style = "background-color: #d9534f; color: white; padding: 15px; margin-top: 20px;",
                    
                    # 加上警告圖示，並在標題下方加一條白色的分隔線
                    h4(icon("exclamation-triangle"), " Fraud Warnings", 
                       style = "font-weight: bold; margin-top: 0; border-bottom: 1px solid #ffcccc; padding-bottom: 10px;"),
                    
                    # 將所有的 textOutput 放在這裡
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
      # Calculator 分頁 (專注於執行與結果呈現)
      # ==========================================
      tabItem(tabName = "calculator",
              tabBox(width = "auto",
                     
                     tabPanel(
                       fluidRow(
                         column(
                           width = 6,
                           radioButtons("dcf_mode", "選擇 DCF 估值模型：",
                                        choices = list("永續成長法 (Gordon Growth Model)" = "gordon",
                                                       "二階段成長法 (Two-Stage Model)" = "two_stage"),
                                        selected = "gordon",
                                        inline = FALSE)
                         ),
                         column(
                           width = 6,
                           numericInput("years", "預測年數 n", value = 5, min = 1, max = 30)
                         )
                       )
                     )
              ),
              
              tabBox(title = "DCF Report",
                     width = "auto",
                     
                     tabPanel("Stock Valuation",
                              fluidRow( # 頂部顯示 EV 與 股價
                                column(width = 12,
                                       fluidRow(
                                         infoBoxOutput("ibx_stock_value_dcf", width = 6),
                                         infoBoxOutput("ibx_enterprise_value_dcf", width = 6)
                                       )
                                )
                              ),
                              
                              fluidRow(
                                column(width = 12,
                                       plotOutput("plt_dcf_trajectory", height = "400px"),
                                       
                                       h6(helpText("提示：啟動時已自動計算。若已自訂相關參數，請點擊按鈕更新。")),
                                       
                                       fluidRow(
                                         column(width = 6,
                                                actionButton("calc", "▶ 試算 DCF", class = "btn-success btn-block", 
                                                             style = "padding: 12px; font-weight: bold; font-size: 16px;")),
                                         
                                         column(width = 6,
                                                actionButton("reset_dcf", "回復預設", class = "btn-default btn-block",
                                                             style = "padding: 12px; font-weight: bold; font-size: 16px;"))
                                       ),
                                       tags$div(
                                         style = "margin-top: 10px;",
                                         htmlOutput("vtxt_dcf_setting_details")
                                       )
                                )
                              )
                     ),
                     
                     tabPanel("DCF Calculation Details",
                              fluidRow(
                                column(
                                  width = 12,
                                  
                                  plotOutput(NS("fcfmod", "fcf_plot"), height = "350px"),
                                  verbatimTextOutput("vtxt_dcf_results")
                                ),
                                
                                # ==========================================
                                # 🟢 新增：資料缺漏警示與手動輸入區塊 (放在最上方)
                                # ==========================================
                                uiOutput("ui_data_validation")
                              )
                     )
              ),
              
              tabBox(
                width = "auto",
                # 📌 區塊 2：成長率與 DCF 模型設定
                tabPanel("Growth Rate",
                         fluidRow(
                           infoBoxOutput("ibx_estimated_g", width = 6),
                           infoBoxOutput("ibx_wacc", width = 6),
                           htmlOutput("txt_fcf_raw_data"),
                           
                           box(title = "DCF成長率與模型參數設定", 
                               width = 12, status = "warning", solidHeader = TRUE,
                               
                               
                               selectInput("g_growth_method", "選擇估算方法：",
                                           choices = c("複合年均成長率 (CAGR)" = "cagr",
                                                       "平均年增率" = "mean",
                                                       "中位數年增率" = "median",
                                                       "最近一年變化率" = "last_year",
                                                       "自訂輸入" = "custom"),
                                           selected = "mean"),
                               conditionalPanel(
                                 condition = "input.g_growth_method == 'custom'",
                                 numericInput("custom_g", "自訂 g 值 (%)", value = NA, step = 0.1)
                               ),
                               tags$hr(),
                               
                               # 模型 A: 永續成長法
                               conditionalPanel(
                                 condition = "input.dcf_mode == 'gordon'",
                                 numericInput("g_gordon", "永續成長率 g (%) [已自動綁定]", value = 3),
                                 numericInput("wacc_gordon", "折現率 WACC (%)", value = 10)
                               ),
                               
                               # 模型 B: 二階段成長法
                               conditionalPanel(
                                 condition = "input.dcf_mode == 'two_stage'",
                                 numericInput("yr_stage1", "第一階段預測年數", value = 5, min = 1, max = 19),
                                 numericInput("g_stage1", "第一階段成長率 g₁ (%) [已自動綁定]", value = 5),
                                 numericInput("g_stage2", "終端永續成長率 g₂ (%) [建議採用通膨率2~3%]", value = 3),
                                 numericInput("wacc_stage1", "第一階段 WACC₁ (%)", value = 10),
                                 numericInput("wacc_stage2", "第二階段 WACC₂ (%)", value = 9)
                               ),
                               
                               checkboxInput("use_calculated_wacc", "✅ 採用估算 WACC 作為折現率", value = TRUE),
                               tags$hr(),
                               
                               textOutput("g_result")
                           )
                         )
                ),
                
                tabPanel("WACC",
                         fluidRow(
                           infoBoxOutput("ibx_rd", width = 6),
                           infoBoxOutput("ibx_re", width = 6)
                         ),
                         
                         fluidRow(
                           div("WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)",
                               style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"),
                           
                           box(h4("WACC 估算"),
                               htmlOutput("wacc_result"),
                               tags$br(),
                               
                               numericInput("wacc_re", "股權成本 rₑ (%)", value = 10, min = 0, step = 0.1),
                               checkboxInput("use_estimated_re", "✅ 採用估算 rₑ（來自CAPM）", value = TRUE),
                               numericInput("wacc_rd", "負債成本 rᵈ (%)", value = 5, min = 0, step = 0.1),
                               numericInput("wacc_tax", "所得稅率 T (%)", value = 20, min = 0, max = 100, step = 0.1),
                               actionButton("calc_wacc", "計算 WACC", class = "btn-primary"),
                               tags$hr()
                           ),
                           
                           box(h4("CAPM 估算 rₑ"),
                               htmlOutput("capm_result"),
                               tags$br(),
                               
                               numericInput("capm_rf", "無風險利率 Rf (%)", value = 3, step = 0.1),
                               numericInput("capm_beta", "Beta (β) [套用產業: 預設]", value = 1.1, step = 0.1),
                               numericInput("capm_rm", "市場報酬率 Rm (%)", value = 8, step = 0.1),
                               actionButton("calc_capm", "估算 rₑ（CAPM）", class = "btn-primary")
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "advance",
              tabBox(title = "Advanced FCF Settings",
                     width = "auto",
                     fluidRow(
                       box(width = 12, status = "danger", solidHeader = TRUE,
                           fluidRow(
                             column(width = 6,
                                    h4(tags$b("CapEx 預估資本支出佔營收比")),
                                    numericInput("var_capex_rate", "CapEx / Revenue (%):", 
                                                 value = NA, step = 0.5),
                                    htmlOutput("txt_hist_capex"),
                                    h6(helpText("註：若為空白，系統將自動套用上方歷史預估值。"))
                             ),
                             
                             column(width = 6,
                                    h4(tags$b("ΔNWC 預估營運資本佔營收變動比")),
                                    numericInput("var_nwc_rate", "ΔNWC / ΔRevenue (%)", 
                                                 value = NA, step = 0.5),
                                    htmlOutput("txt_hist_nwc"),
                                    h6(helpText("註：若為空白，系統將自動套用上方歷史預估值。"))
                             )
                           )
                       )
                       
                     )
              )
      ),
      
      # ⬇️ Tab 4：About 分頁
      tabItem(tabName = "about",
              # 內嵌 CSS 樣式
              tags$head(
                tags$style(HTML("
            .btn-blackwhite {
              background-color: black !important;
              color: white !important;
              border: 1px solid #ccc !important;
              font-weight: bold;
              padding: 10px 20px;
              border-radius: 6px;
              width: 100%;
              margin-top: 20px;
            }
            .btn-blackwhite:hover {
              background-color: #222 !important;
              color: #fff !important;
            }
          "))
              ),
              
              fluidRow(
                column(width = 9, h2("About The YNow App")),
                column(width = 3, downloadButton("download_report", "📄 下載分析報告 (PDF)", class = "btn-blackwhite"))
              ),
              
              # ==========================================
              # DCF 評價模型公式與參數說明區塊
              # ==========================================
              fluidRow(
                column(width = 12,
                       box(title = "Valuation Methodology 評價模型公式與參數說明", 
                           width = NULL, status = "info", solidHeader = TRUE,
                           
                           withMathJax(), # 啟動 Shiny 的數學公式渲染引擎
                           
                           div(style = "padding: 10px; font-size: 15px; line-height: 1.8;",
                               
                               h4(tags$b("企業價值 (Enterprise Value, EV) 與 目標股價 (Intrinsic Value)")),
                               p("將所有未來預期的現金流與終值，折現回今天的價值並加總："),
                               p("$$EV = \\sum_{t=1}^{n} \\frac{FCF_t}{(1 + WACC)^t} + \\frac{TV}{(1 + WACC)^n}$$"),
                               p("接著推算股東權益價值與最終每股內在價值："),
                               p("$$Equity\\ Value = EV - Total\\ Debt + Cash$$"),
                               p("$$Intrinsic\\ Value = \\frac{Equity\\ Value}{Shares\\ Outstanding}$$"),
                               
                               tags$hr(),
                               
                               h4(tags$b("自由現金流 (Free Cash Flow, FCF)")),
                               p("$$FCF = NOPAT + D\\&A - CapEx - \\Delta NWC$$"),
                               tags$ul(
                                 tags$li("NOPAT：稅後淨營業利潤 (EBIT × (1 - Tax Rate))"),
                                 tags$li("D&A：折舊與攤銷 (Depreciation & Amortization)"),
                                 tags$li("CapEx：資本支出 (Capital Expenditure)"),
                                 tags$li("ΔNWC：營運資本變動 (Change in Net Working Capital)")
                               ),
                               
                               # ==========================================
                               # 1.1 進階參數公式說明區塊
                               # ==========================================
                               tags$div(style = "background-color: #f9f9f9; padding: 15px; border-left: 4px solid #3c8dbc; margin-top: 15px; margin-bottom: 15px;",
                                        h5(tags$b("1.1 進階預測參數 (Advanced FCF Parameters)")),
                                        
                                        tags$b("A. 資本支出佔營收比 (CapEx Rate)"),
                                        p("衡量公司維持或擴張營運需投入的資本比例，系統取最近兩年之絕對值平均，以平滑單一年度的極端資本支出波動："),
                                        p("$$CapEx\\ Rate = Average \\left( \\frac{|CapEx_{Y1}|}{Revenue_{Y1}}, \\frac{|CapEx_{Y2}|}{Revenue_{Y2}} \\right)$$"),
                                        
                                        tags$b("B. 營運資本佔營收變動比 (NWC Rate)"),
                                        p("衡量營收成長所額外佔用的營運資金。系統以近兩期變動量 (YoY Change) 計算，並嚴格排除「現金」與「短期借貸」等非營運項目的干擾："),
                                        p("$$\\Delta CA = (Current\\ Assets_{Y1} - Current\\ Assets_{Y2}) - (Cash_{Y1} - Cash_{Y2})$$"),
                                        p("$$\\Delta CL = (Current\\ Liabilities_{Y1} - Current\\ Liabilities_{Y2}) - (Short\\ Term\\ Debt_{Y1} - Short\\ Term\\ Debt_{Y2})$$"),
                                        p("$$\\Delta NWC = \\Delta CA - \\Delta CL$$"),
                                        p("$$NWC\\ Rate = \\frac{\\Delta NWC}{Revenue_{Y1} - Revenue_{Y2}}$$")
                               ),
                               # ==========================================
                               
                               tags$hr(),
                               
                               h4(tags$b("加權平均資本成本 (WACC) 與 資本資產定價模型 (CAPM)")),
                               p("WACC 作為將未來現金流折現的「折現率」，代表企業整體的資金成本："),
                               p("$$WACC = (W_e \\times K_e) + (W_d \\times K_d \\times (1 - T))$$"),
                               tags$div(style = "background-color: #f9f9f9; padding: 15px; border-left: 4px solid #3c8dbc; margin-top: 15px; margin-bottom: 15px;",
                                        
                                        p("股權成本 \\(K_e\\) 透過 CAPM 模型計算："),
                                        p("$$K_e = R_f + \\beta \\times (R_m - R_f)$$")
                               ),
                               tags$hr(),
                               
                               h4(tags$b("終值 (Terminal Value, TV)")),
                               p("假設在詳細預測期（預設 5 年）結束後，企業將以永續成長率 \\(g\\) 穩定增長："),
                               p("$$TV = \\frac{FCF_n \\times (1 + g)}{WACC - g}$$")
                           )
                       )
                )
              ),
              
              fluidRow(
                column(width = 12,
                       box(
                         title = "Financial Fraud Red Flags 財務舞弊警訊", 
                         width = NULL, status = "danger", solidHeader = TRUE,
                         
                         fluidRow(
                           column(width = 8,
                                  tags$ul(style = "font-size: 15px; line-height: 1.8;",
                                          tags$li("Unusual or unexpected increases in revenue or profits 異常的營收或利潤增長"),
                                          tags$li("Large, round numbers in the financial reports 財報中出現大量整齊的整數"),
                                          tags$li("Inflated or overstated assets 資產遭異常誇大"),
                                          tags$li("Unusual or unnecessary expenses or transfers between accounts 異常的費用支出或資金轉移"),
                                          tags$li("Unusual or inconsistent ratios or trends in the financial statements 財務比率趨勢與同業嚴重脫鉤"),
                                          tags$li("Lack of adequate documentation or supporting evidence for transactions 缺乏充分的交易佐證文件"),
                                          tags$li("Conflicts of interest among management or employees 管理層存在嚴重的利益衝突")
                                  )
                           )
                           
                         )
                       )
                )
              )
              
      )
    )
  )
)
