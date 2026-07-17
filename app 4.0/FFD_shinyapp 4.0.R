source("global.R")
source("setup2.R")
source("industry_standards.R")
source("kpi_module.R")
source("fcf_projection_module.R", encoding = "UTF-8")
source("search_module2.R")

#-------------------- UI --------------------#

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
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Calculator", tabName = "calculator", icon = icon("equalizer", lib = "glyphicon"), badgeLabel = "new", badgeColor = "green"),
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
    column(width = 12, textOutput("today"))
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
  ")),
      
      tags$head(
        tags$style(HTML("
    .info-box .info-box-number {
      font-size: 150% !important;
      font-weight: bold;
    }
  "))
      )
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
    
    tabBox(
      title = "Fraud Warnings",
      width = "auto",
      fluidRow(
        column(width = 12, textOutput("highdebttoequity")),
        column(width = 12, textOutput("nofreecashflow")),
        column(width = 12, textOutput("nooperatingcashflow")),
        column(width = 12, textOutput("notdoingbusiness")),
        column(width = 12, textOutput("notgettingcashback")),
        column(width = 12, textOutput("no_fraud_detected"))
      )
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        tabBox(
          title = "Financial Reports",
          width = "auto",
          
          tabPanel("Finance Summary",
                   p("This section imports Finance Summaries from Yahoo Finance"),
                   dataTableOutput("tbFinanceSummary"),
                   downloadButton('FS_download', "Download the data")
          ),
          
          tabPanel("Income Statement",
                   p("This section imports Income Statements from Yahoo Finance"),
                   dataTableOutput("tbIncomeStatement"),
                   downloadButton('IS_download', "Download the data")
          ),
          
          tabPanel("Balance Sheet",
                   p("This section imports Balance Sheets from Yahoo Finance"),
                   dataTableOutput("tbBalanceSheet"),
                   downloadButton('BS_download', "Download the data")
          ),
          
          tabPanel("Cash Flow",
                   p("This section imports Cash Flow data from Yahoo Finance"),
                   selectInput("cf_type", "Select Cash Flow Type",
                               choices = c("Operating Cash Flow", "Investing Cash Flow", "Financing Cash Flow")),
                   plotlyOutput("cf_plot"),
                   dataTableOutput("tbCashFlow"),
                   downloadButton('CF_download', "Download the data")
          )
        ),
        
        h6("Stock Industry Recommendations from Yahoo"),
        #search_module_ui("search"),
        #verbatimTextOutput("search_results"),
        
        pickerInput(
          inputId = "industry_choice",
          label = "Select Industry Standard",
          choices = sort(names(industry_standards)[names(industry_standards) != "" & !is.na(names(industry_standards))]),
          selected = "sc.IC_Design",
          options = list(
            `live-search` = TRUE,
            `size` = 10
          )
        ),
        
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
            column(width = 12,
                   h4("Stable Indicator List 景氣穩定指標表"),
                   tableOutput("stable_indicator_table")
            )
          ))
        )
      ),
      
      tabItem(
        tabName = "calculator",
        tabBox(
          title = "DCF Calculator",
          width = "auto",
          
          # ⬇️ Tab 1：Stock Valuation
          tabPanel("Stock Valuation",
                   fluidRow(
                     column(
                       width = 12,
                       fluidRow(
                         infoBoxOutput("ibx_enterprise_value_dcf"),
                         infoBoxOutput("ibx_stock_value_dcf")
                       )
                     ),
                     column(
                       width = 6,
                       htmlOutput("vtxt_dcf_setting_details"),
                     ),
                     column(
                       width = 6,
                       plotOutput("dft_fcf_plot")
                     )
                   )
          ),
          
          # ⬇️ Tab 2：DCF Calculator
          tabPanel("DCF Calculator",
                   
                   fluidRow(
                     column(
                       width = 12,
                       infoBoxOutput("ibx_enterprise_value_dcf"),
                       infoBoxOutput("ibx_stock_value_dcf")
                     ),
                     
                     column(
                       width = 4,
                       radioButtons("dcf_mode", "估值模式",
                                    choices = c("永續成長法（Gordon Growth）" = "gordon",
                                                "二階段成長法（Two-Stage Growth）" = "two_stage"),
                                    selected = "gordon"),
                       
                       numericInput("years", "預測年數 n", value = 5, min = 1, max = 20),
                       
                       conditionalPanel(
                         condition = "input.dcf_mode == 'gordon'",
                         numericInput("g_gordon", "永續成長率（Gordon Growth）g (%)", value = 3),
                         numericInput("wacc_gordon", "折現率 WACC (%)", value = 10)
                       ),
                       
                       conditionalPanel(
                         condition = "input.dcf_mode == 'two_stage'",
                         numericInput("yr_stage1", "第一階段預測年數", value = 3, min = 1, max = 19),
                         numericInput("g_stage1", "第一階段成長率 g₁ (%)", value = 5),
                         numericInput("g_stage2", "第二階段成長率 g₂ (%)", value = 3),
                         numericInput("wacc_stage1", "第一階段 WACC₁ (%)", value = 10),
                         numericInput("wacc_stage2", "第二階段 WACC₂ (%)", value = 9),
                       ),
                       
                       checkboxInput("use_calculated_wacc", "使用估算的 WACC 作為折現率", value = TRUE),
                       
                       fluidRow(
                         column(width = 6, actionButton("calc", "計算DCF")),
                         column(width = 6, actionButton("reset_dcf", "🔁 回復預設", icon = icon("rotate-left")))
                       )
                     ),
                     
                     # 右側：估值結果區
                     column(
                       width = 8,
                       h4("🔎 詳細估值結果"),
                       plotOutput("fcf_plot"),
                       verbatimTextOutput("vtxt_dcf_results")
                     )
                   )
          ),
          
          tabPanel("g Calculator",
                   
                   # 🔹 成長率估算 InfoBox
                   fluidRow(
                     column(width = 12,
                            infoBoxOutput("ibx_estimated_g")
                     )
                   ),
                   
                   br(),
                   
                   # 🔹 設定與結果區塊
                   fluidRow(
                     column(
                       width = 4,
                       
                       selectInput(
                         inputId = "g_growth_method",
                         label = "估算方法",
                         choices = c("平均年增率（Mean）" = "mean",
                                     "中位數年增率（Median）" = "median",
                                     "最近一年變化率（Last Year）" = "last_year",
                                     "自訂" = "custom"),
                         selected = "mean"
                       ),
                       
                       conditionalPanel(
                         condition = "input.g_growth_method == 'custom'",
                         numericInput("custom_g", "自訂 g 值 (%)", value = NA, step = 0.1)
                       ),
                       
                       actionButton("calc_growth", "📈 計算 g"),
                       helpText("根據過去 FCF，自動推估永續成長率 g")
                     ),
                     
                     column(
                       width = 8,
                       h4("計算結果"),
                       verbatimTextOutput("g_result"),
                       tags$hr(),
                       helpText("g 將自動套用至 Gordon 及 Two-Stage 模型的對應欄位。")
                     )
                   )
          ),
          
          # ⬇️ Tab 3：WACC Calculator
          tabPanel("WACC Calculator",
                   
                   fluidRow(
                     infoBoxOutput("ibx_wacc"),
                     infoBoxOutput("ibx_re"),
                     infoBoxOutput("ibx_rd")
                   ),
                   
                   br(),
                   
                   fluidRow(
                     column(
                       width = 4,
                       
                       # rₑ 區塊
                       numericInput("wacc_re", "股權成本 rₑ (%)", value = 10, min = 0, step = 0.1),
                       checkboxInput("use_estimated_re", "✅ 使用估算的 rₑ（來自 CAPM）", value = FALSE),
                       
                       # rᵈ 與稅率
                       numericInput("wacc_rd", "負債成本 rᵈ (%)", value = 5, min = 0, step = 0.1),
                       numericInput("wacc_tax", "所得稅率 T (%)", value = 20, min = 0, max = 100, step = 1),
                       
                       actionButton("calc_wacc", "📊 計算 WACC"),
                       tags$hr(),
                       
                       # CAPM 區塊
                       h4("📐 使用 CAPM 估算 rₑ"),
                       
                       numericInput("capm_rf", "無風險利率 Rf (%)", value = 3, step = 0.1),
                       numericInput("capm_beta", "Beta (β)", value = {
                         inds <- industry_standards[["sc.IC_Design"]]
                         if (!is.null(inds$beta_avg)) inds$beta_avg else 1.1
                       }, step = 0.1),
                       
                       numericInput("capm_rm", "市場報酬率 Rm (%)", value = {
                         inds <- industry_standards[["sc.IC_Design"]]
                         if (!is.null(inds$rm_avg)) inds$rm_avg else 8
                       }, step = 0.1),
                       
                       actionButton("calc_capm", "📈 估算 rₑ（CAPM）"),
                     ),
                     
                     column(
                       width = 8,
                       h4("📈 CAPM 計算結果"),
                       htmlOutput("capm_result"),
                       
                       tags$hr(),
                       
                       h4("🧮 WACC 計算結果"),
                       htmlOutput("wacc_result"),
                       
                       tags$strong("WACC 公式："),
                       helpText("WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)")
                     )
                   )
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        tags$head(
          tags$style(HTML("
    pre { overflow: auto; word-wrap: normal; }

    .btn-blackwhite {
      background-color: black !important;
      color: white !important;
      border: 1px solid #ccc !important;
      font-weight: bold;
      padding: 10px 20px;
      border-radius: 6px;
    }
    
    .btn-blackwhite:hover {
      background-color: #222 !important;
      color: #fff !important;
    }
  "))
        ),
        
        fluidRow(
          column(width = 12, h2("About The YNow App")),
          column(width = 3,  # 跟 infoBox 默認寬度一致
                 downloadButton("download_report", "📄 下載分析報告 (PDF)", class = "btn-blackwhite"))
        ),
        
        fluidRow(
          column(
            width = 8,
            p(
              "Red flags that may indicate financial fraud:", br(),
              "- Unusual or unexpected increases in revenue or profits", br(),
              "- Large, round numbers in the financial reports", br(),
              "- Inflated or overstated assets", br(),
              "- Unusual or unnecessary expenses or transfers between accounts", br(),
              "- Unusual or inconsistent ratios or trends in the financial statements", br(),
              "- Lack of adequate documentation or supporting evidence for transactions", br(),
              "- Conflicts of interest among management or employees"
            )
          ),
          column(
            width = 4,
            p("This section imports financial data from Alpha Vantage (official API, no scraping required).")
          )
        )
      )
    )
  )
)

#-------------------- SERVER --------------------#

server <- function(input, output, session) {
  
  # 輔助函數：裁切表格
  trim_financial_table <- function(df, end_metric) {
    if (is.null(df) || nrow(df) == 0) return(df)
    idx <- grep(end_metric, df[[1]], ignore.case = TRUE)
    if (length(idx) > 0) return(df[1:idx[1], ])
    return(df)
  }
  
  # 輔助函數：計算 FCF 預測
  fcf_projection <- function(start_fcf, growth_rate, years) {
    g <- growth_rate / 100
    return(start_fcf * (1 + g)^(0:(years - 1)))
  }

  # 1. 建立一個 Reactive 變數，用來儲存公司基本資訊 (點擊搜尋時觸發)
  corp_info <- eventReactive(input$searchButton, {
    req(input$searchText)
    
    # 呼叫 search_module2.R 中的函數抓取公司名稱與產業
    withProgress(message = '抓取公司基本資訊...', value = 0.5, {
      get_yahoo_industry(input$searchText)
    })
  })
  
  # 2. 將抓到的公司名稱輸出到首頁大標題
  output$txt_corpname <- renderText({
    req(summary_data())
    
    # 從抓回來的 dataframe 屬性中提取公司全稱
    name <- attr(summary_data(), "company_name")
    
    # 最後的防呆：如果真的什麼都沒抓到，至少顯示使用者輸入的代碼
    if (is.null(name) || is.na(name) || name == "") {
      return(paste("Stock:", toupper(input$searchText)))
    } else {
      return(name)
    }
  })
  
  # (選擇性) 如果你有其他地方需要顯示產業資訊，也可以從 corp_info() 拿：
  output$search_results <- renderText({ req(corp_info()); corp_info()$display_text })
  
  # ==========================================
  # 1. 建立 Summary 的 Reactive 變數
  # ==========================================
  summary_data <- eventReactive(input$search, {
    req(input$sc)
    withProgress(message = '抓取 Yahoo Summary 表格...', value = 0.5, {
      get_summary_data(input$sc)
    })
  })
  
  # ==========================================
  # 2. 輸出 tbFinanceSummary 表格
  # ==========================================
  output$tbFinanceSummary <- renderDataTable({
    req(summary_data())
    datatable(summary_data(), 
              options = list(pageLength = 20, dom = 't', scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ==========================================
  # 3. 修復三個 InfoBox (擷取 Summary 表格內的特定欄位)
  # ==========================================
  output$ibx_marketcap <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "Market Cap (intraday)" %in% df$Item) {
      df$Value[df$Item == "Market Cap (intraday)"]
    } else { "N/A" }
    
    infoBox("Market Cap", val, icon = icon("globe"), color = "blue")
  })
  
  output$ibx_stockprice <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "Previous Close" %in% df$Item) {
      df$Value[df$Item == "Previous Close"]
    } else { "N/A" }
    
    infoBox("Previous Close", val, icon = icon("chart-line"), color = "purple")
  })
  
  output$ibx_EPS <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "EPS (TTM)" %in% df$Item) {
      df$Value[df$Item == "EPS (TTM)"]
    } else { "N/A" }
    
    infoBox("EPS (TTM)", val, icon = icon("dollar-sign"), color = "green")
  })
  
  financials <- eventReactive(input$search, {
    req(input$sc)
    
    # 執行帶有進度條的抓取過程
    withProgress(message = paste('正在模擬瀏覽器抓取', input$sc, '數據...'), value = 0, {
      
      incProgress(0.2, detail = "啟動背景 Chrome 瀏覽器...")
      raw_html_list <- get.data(input$sc) # 呼叫 setup.R 內的新函數
      
      incProgress(0.5, detail = "正在從網頁中提取財務數值...")
      
      # 解析三張報表
      is_table <- extract_yf_financial_table(raw_html_list$income_statement)
      bs_table <- extract_yf_financial_table(raw_html_list$balance_sheet)
      cf_table <- extract_yf_financial_table(raw_html_list$cash_flow)
      
      incProgress(0.3, detail = "數據同步完成！")
      
      # 回傳整理後的清單
      list(
        income = is_table,
        balance = bs_table,
        cashflow = cf_table
      )
    })
  })
  
  ### INCOME STATEMENT
  # Reactive: Parse and extract Income Statement table
  d_income_statement <- reactive({
    dat <- financials()$income
    if (is.null(dat)) return(data.frame(Error = "No Income Statement Data"))
    dat
  })
  
  # Output: DataTable
  output$tbIncomeStatement <- renderDataTable({
    df <- d_income_statement()
    df <- trim_financial_table(df, "Tax Effect of Unusual Items")
    datatable(df, options = list(pageLength = 20))
  })
  
  # Output: Download Handler
  output$IS_download <- downloadHandler(
    filename = function() {
      paste0(input$sc, "_incomestatement_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(d_income_statement(), file, row.names = FALSE)
    }
  )
  
  ### BALANCE SHEET
  # Reactive: Parse and extract Balance Sheet table
  d_balance_sheet <- reactive({
    dat <- financials()$balance
    if (is.null(dat)) return(data.frame(Error = "No Balance Sheet Data"))
    dat
  })
  
  # Output: DataTable
  output$tbBalanceSheet <- renderDataTable({
    df <- d_balance_sheet()
    df <- trim_financial_table(df, "Treasury Shares Number")
    datatable(df)
  })
  
  # Output: Download Handler
  output$BS_download <- downloadHandler(
    filename = function() {
      paste0(input$sc, "_balancesheet_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(d_balance_sheet(), file, row.names = FALSE)
    }
  )
  
  ### CASH FLOW
  # Reactive: Parse and extract Cash Flow table
  d_cash_flow <- reactive({
    dat <- financials()$cashflow
    if (is.null(dat)) return(data.frame(Error = "No Cash Flow Data"))
    dat
  })
  
  # Output: 互動折線圖
  # 選哪一種現金流（放在 d_cash_flow 後面）
  selected_cashflow_data <- reactive({
    req(d_cash_flow())
    keyword <- switch(input$cf_type,
                      "Operating Cash Flow" = "Operating Cash Flow",
                      "Investing Cash Flow" = "Investing Cash Flow",
                      "Financing Cash Flow" = "Financing Cash Flow")
    
    d_cash_flow()[grepl(keyword, d_cash_flow()$Breakdown, ignore.case = TRUE), ]
  })
  
  output$cf_plot <- renderPlotly({
    df <- selected_cashflow_data()
    req(nrow(df) > 0)
    
    # 轉成 long format
    cf_vals <- df[1, -1]
    cf_vals <- as.numeric(gsub(",", "", cf_vals))
    cf_labels <- colnames(df)[-1]
    
    plot_df <- data.frame(Year = cf_labels, Value = cf_vals)
    
    p <- ggplot(plot_df, aes(x = Year, y = Value, group = 1)) +
      geom_line(color = "black", size = 1.2) +
      geom_point(aes(color = Value < 0), size = 2.5) +  # 根據負數上色
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
      theme_bw() +
      labs(title = input$cf_type, x = "", y = "USD") +
      theme(
        plot.title = element_text(size = 12, face = "bold", color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Output: DataTable
  output$tbCashFlow <- renderDataTable({
    df <- d_cash_flow()
    df <- trim_financial_table(df, "Free Cash Flow")
    datatable(df)
  })
  
  # Output: Download Handler
  output$CF_download <- downloadHandler(
    filename = function() {
      paste0(input$sc, "_cashflow_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(d_cash_flow(), file, row.names = FALSE)
    }
  )
  
  ### ⬇️ 呼叫 KPI 模組，將主資料餵入
  kpi_module_server(
    id = "kpi",
    d_income_statement = d_income_statement,
    d_balance_sheet = d_balance_sheet,
    d_cash_flow = d_cash_flow,
    industry_choice = reactive(input$industry_choice)
  )
  
  ### Fraud Risk Warnings
  # 先定義 reactiveValues 來存每個警訊結果
  fraud_warnings <- reactiveValues(
    fcf = "", ocf = "", biz = "", cashback = "", debt = ""
  )
  
  # 各種 fraud 判斷邏輯
  output$nofreecashflow <- renderText({
    fcf <- get_avg(select_clean_metric_row(d_cash_flow(), "Free Cash Flow"))
    fraud_warnings$fcf <- if (is.na(fcf)) "" else if (fcf < 0) {
      "⚠️ 自由現金流為負數，可能營運困難或大量資本支出"
    } else { "" }
    fraud_warnings$fcf
  })
  
  output$nooperatingcashflow <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    fraud_warnings$ocf <- if (is.na(ocf)) "" else if (ocf < 0) {
      "⚠️ 營業現金流為負數，代表核心業務沒有產生現金"
    } else { "" }
    fraud_warnings$ocf
  })
  
  output$notdoingbusiness <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    fraud_warnings$biz <- if (is.na(ocf) || is.na(net)) "" else if (ocf < net) {
      "⚠️ 營業現金流低於淨利，帳面賺錢但現金未實現"
    } else { "" }
    fraud_warnings$biz
  })
  
  output$notgettingcashback <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    fraud_warnings$cashback <- if (is.na(ocf) || is.na(net)) "" else if (net > 0 && ocf < 0) {
      "⚠️ 淨利為正但現金流為負，獲利品質存疑"
    } else { "" }
    fraud_warnings$cashback
  })
  
  output$highdebttoequity <- renderText({
    total_liabilities <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Debt"))
    total_equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
    # 避免分母為0或NA
    ratio <- if (is.na(total_liabilities) || is.na(total_equity) || total_equity == 0) NA else total_liabilities / total_equity
    
    fraud_warnings$debt <- if (is.na(ratio)) "" else if (ratio > 2) {
      "⚠️ 負債對權益比率過高，財務槓桿風險大"
    } else { "" }
    fraud_warnings$debt
  })
  
  # fallback 顯示 - 若都沒有風險警訊就顯示這個
  output$no_fraud_detected <- renderText({
    if (all(fraud_warnings$fcf == "",
            fraud_warnings$ocf == "",
            fraud_warnings$biz == "",
            fraud_warnings$cashback == "",
            fraud_warnings$debt == "")) {
      "✅ Currently no fraud risks detected."
    } else {
      ""
    }
  })
  
  ### Others
  output$stable_indicator_table <- renderTable({
    data.frame(
      指標名稱 = c("毛利率", "OPEX Ratio", "ROA / ROE", "存貨週轉 / 應收週轉", "Equity Multiplier", "自由現金流比"),
      穩定性 = c("★★★★☆", "★★★★☆", "★★★★☆", "★★★☆☆", "★★★☆☆", "★★★★★"),
      說明 = c(
        "技術/品牌優勢的象徵",
        "管理與營運效率穩定性",
        "去波動化後能長期觀察企業效率",
        "營運效率的直接反映",
        "財務體質穩定，不易劇變",
        "最能看出企業真實價值創造力"
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "m", width = "100%")
  
  ### Calculator
  # 📌 反應式變數定義 ----------------------------------------------------------
  
  estimated_g <- reactiveVal(NULL)
  estimated_r_e <- reactiveVal(NULL)
  calculated_wacc <- reactiveVal(NULL)
  dcf_value_result <- reactiveVal(NULL)
  stock_price_estimate_val <- reactiveVal(NULL)
  
  fcf_result <- fcf_projection_module_server(
    id = "fcfmod",
    d_income_statement = d_income_statement,
    d_cash_flow = d_cash_flow,
    d_balance_sheet = d_balance_sheet,
    input_years = reactive(input$years),
    calc_trigger = reactive(input$calc),
    input_mode = reactive(input$dcf_mode),
    g_gordon = reactive(input$g_gordon),
    g_stage1 = reactive(input$g_stage1),
    g_stage2 = reactive(input$g_stage2),
    yr_stage1 = reactive(input$yr_stage1)
  )
  
  # 📈 g 永續成長率估算 --------------------------------------------------
  
  observeEvent(input$calc_growth, {
    req(d_cash_flow())
    
    fcf_vec <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    
    if (length(fcf_vec) < 2 || all(is.na(fcf_vec))) {
      showNotification("⚠️ 無足夠自由現金流資料來估算成長率", type = "error")
      estimated_g(NULL)
      return(NULL)
    }
    
    # 年增率向量
    fcf_vec <- na.omit(fcf_vec)
    g_rate <- diff(log(fcf_vec))
    
    method <- input$g_growth_method
    val <- switch(method,
                  "mean" = round(mean(g_rate, na.rm = TRUE) * 100, 2),
                  "median" = round(median(g_rate, na.rm = TRUE) * 100, 2),
                  "last_year" = round((tail(fcf_vec, 1) / tail(fcf_vec, 2)[1] - 1) * 100, 2),
                  "custom" = input$custom_g
    )
    
    if (is.null(val) || is.na(val)) {
      showNotification("⚠️ 無法估算成長率", type = "error")
      estimated_g(NULL)
      return(NULL)
    }
    
    estimated_g(val)
    
    output$g_result <- renderText({
      glue::glue("📈 成長率估算結果：{val} % *方法：{switch(method,
                                                'mean' = '平均年增率',
                                                'median' = '中位數年增率',
                                                'last_year' = '最近一年變化率',
                                                'custom' = '自訂輸入')}")
    })
    
    # 自動更新至 DCF 模型欄位
    updateNumericInput(session, "g_gordon", value = val)
    updateNumericInput(session, "g_stage1", value = val)
    
    # InfoBox 顯示
    output$ibx_estimated_g <- renderInfoBox({
      infoBox("估算 g 成長率", paste0(val, " %"), icon = icon("chart-line"),
              color = "purple", fill = TRUE)
    })
  })
  
  # 📌 依據產業選擇自動更新 beta 和 Rm
  observeEvent(input$industry_choice, {
    inds <- industry_standards[[input$industry_choice]]
    if (!is.null(inds$beta_avg)) {
      updateNumericInput(session, "capm_beta", value = inds$beta_avg)
    }
    if (!is.null(inds$rm_avg)) {
      updateNumericInput(session, "capm_rm", value = inds$rm_avg)
    }
  })
  
  # 📈 CAPM 股東權益成本估算
  observeEvent(input$calc_capm, {
    Rf <- input$capm_rf / 100
    beta <- input$capm_beta
    Rm <- input$capm_rm / 100
    
    r_e_est <- Rf + beta * (Rm - Rf)
    estimated_r_e(r_e_est)
    
    # 🔁 自動更新至手動欄位 wacc_re
    updateNumericInput(session, "wacc_re", value = round(r_e_est * 100, 2))
    
    # 結果輸出
    output$capm_result <- renderUI({
      r_e <- estimated_r_e()
      if (is.null(r_e)) return(NULL)
      
      HTML(glue::glue(
        "<div style='font-size: 16px; line-height: 1.6;'>
        <span style='color: teal; font-size: 20px;'>
          ➤ 股東權益成本 (rₑ) = <b>{round(r_e * 100, 2)} %</b>
        </span><br/>
        <span style='color: gray;'>
          （公式：rₑ = Rf + β × (Rm - Rf)）
        </span>
      </div>"
      ))
    })
  })
  
  # 🧮 WACC 計算
  observeEvent(input$calc_wacc, {
    req(d_balance_sheet())
    
    bs_data <- d_balance_sheet()
    equity <- select_clean_metric_row(bs_data, "Common Stock Equity")[1]
    debt <- select_clean_metric_row(bs_data, "Total Debt")[1]
    T <- input$wacc_tax / 100
    
    # 使用估算 or 手動輸入 rₑ
    r_e <- if (input$use_estimated_re && !is.null(estimated_r_e())) {
      estimated_r_e()
    } else {
      input$wacc_re / 100
    }
    
    r_d <- input$wacc_rd / 100
    total_capital <- equity + debt
    
    wacc <- (equity / total_capital) * r_e + (debt / total_capital) * r_d * (1 - T)
    calculated_wacc(wacc)
    
    wacc_percent <- round(wacc * 100, 2)
    
    # 自動套用至 DCF 區域
    if (input$dcf_mode == "gordon") {
      updateNumericInput(session, "wacc_gordon", value = wacc_percent)
    } else if (input$dcf_mode == "two_stage") {
      updateNumericInput(session, "wacc_stage1", value = wacc_percent)
      updateNumericInput(session, "wacc_stage2", value = wacc_percent)
    }
    
    showNotification(glue::glue("📌 已自動將 WACC {wacc_percent}% 套用至 DCF 折現率參數"), type = "message")
    
    # WACC 詳細結果
    output$wacc_result <- renderUI({
      wacc <- calculated_wacc()
      if (is.null(wacc)) return(NULL)
      
      HTML(glue::glue(
        "<div style='font-size: 16px; line-height: 1.6;'>
        <span style='color: steelblue;'>股東權益 (E)：</span> ${formatC(equity, format = 'f', big.mark = ',', digits = 0)}<br/>
        <span style='color: steelblue;'>總負債 (D)：</span> ${formatC(debt, format = 'f', big.mark = ',', digits = 0)}<br/>
        <span style='color: teal;'>股權成本 (rₑ)：</span> <b>{round(r_e * 100, 2)} %</b><br/>
        <span style='color: limegreen;'>負債成本 (rᵈ)：</span> <b>{round(r_d * 100, 2)} %</b><br/>
        <span style='color: orange;'>所得稅率 (T)：</span> <b>{input$wacc_tax} %</b><br/><br/>
        <span style='font-size: 20px; color: purple;'><b>➡️ WACC = {wacc_percent} %</b></span>
      </div>"
      ))
    })
    
    # InfoBoxes
    output$ibx_wacc <- renderInfoBox({
      infoBox("WACC", h3(paste0(wacc_percent, " %")), icon = icon("percent"), color = "aqua", fill = TRUE)
    })
    output$ibx_re <- renderInfoBox({
      infoBox("股東權益成本 (rₑ)", h3(paste0(round(r_e * 100, 2), " %")), icon = icon("chart-line"), color = "teal", fill = TRUE)
    })
    output$ibx_rd <- renderInfoBox({
      infoBox("負債成本 (rᵈ)", h3(paste0(round(r_d * 100, 2), " %")), icon = icon("university"), color = "lime", fill = TRUE)
    })
  })
  
  ### 📦 共用 FCF 預測圖資料
  generate_fcf_plot <- reactive({
    req(input$dcf_mode, input$years)
    
    fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    fcf_history <- fcf_history[!is.na(fcf_history)]
    fcf_start <- if (length(fcf_history) > 0) head(fcf_history, 1) else 100
    base_year <- as.numeric(format(Sys.Date(), "%Y"))
    n <- input$years
    
    if (input$calc == 0) {
      
      # ✅ 用歷史成長率估算
      est_growth <- estimate_historical_growth(fcf_history)
      proj <- fcf_projection(start_fcf = fcf_start, growth_rate = est_growth, years = n)
      
      df <- data.frame(
        Year = base_year + 0:(n - 1),
        FCF = proj,
        Type = glue::glue("預設（歷史成長率 {est_growth}%）")
      )
    } else {
      if (input$dcf_mode == "gordon") {
        g <- input$g_gordon / 100
        proj <- fcf_start * (1 + g)^(0:(n - 1))
        df <- data.frame(
          Year = base_year + 0:(n - 1),
          FCF = proj,
          Type = "Gordon 預測"
        )
      } else {
        g1 <- input$g_stage1 / 100
        g2 <- input$g_stage2 / 100
        yr_stage1 <- input$yr_stage1
        
        fcf_stage1 <- fcf_start * cumprod(rep(1 + g1, yr_stage1))
        fcf_stage2 <- fcf_stage1[length(fcf_stage1)] * cumprod(rep(1 + g2, n - yr_stage1))
        
        df <- data.frame(
          Year = base_year + 0:(n - 1),
          FCF = c(fcf_stage1, fcf_stage2),
          Type = c(
            rep("第一階段", yr_stage1),
            rep("第二階段", n - yr_stage1)
          )
        )
      }
    }
    
    # 使用 generate_fcf_plot() 的結果
    fcf_df <- tryCatch(generate_fcf_plot(), error = function(e) NULL)
    fcf_values <- if (!is.null(fcf_df)) {
      paste(round(fcf_df$FCF, 2), collapse = ", ")
    } else {
      "⚠️ 無法取得預測 FCF"
    }
    
    return(df)
  })
  
  # 📌 DCF 預設 FCF 預測圖
  output$dft_fcf_plot <- renderPlot({
    df <- generate_fcf_plot()
    
    ggplot(df, aes(x = Year, y = FCF, linetype = Type)) +
      geom_line(size = 1.2, color = "steelblue") +
      geom_point(aes(color = FCF < 0), size = 3) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "steelblue"), guide = "none") +
      scale_linetype_manual(values = c(
        "預設" = "dashed",
        "Gordon 預測" = "dotted",
        "第一階段" = "dotted",
        "第二階段" = "twodash"
      )) +
      theme_minimal(base_size = 14) +
      labs(title = "📉 自由現金流預測圖（Stock Valuation）", x = "年", y = "FCF") +
      theme(plot.title = element_text(size = 14, face = "bold"), legend.position = "top")
  })
  
  # 💰 DCF 模型計算 ------------------------------------------------------------
  observeEvent(input$calc, {
    req(input$sc, input$dcf_mode)
    
    fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    if (all(is.na(fcf_history))) {
      showNotification("⚠️ 無有效自由現金流資料", type = "error")
      return(NULL)
    }
    
    fcf_start <- head(fcf_history, 1)
    n <- input$years
    base_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # ⏬ 折現率
    discount_rate <- if (input$use_calculated_wacc && !is.null(calculated_wacc())) {
      calculated_wacc()
    } else(
      if (input$dcf_mode == "gordon") { input$wacc_gordon / 100 }
      else input$wacc_stage1 / 100 )
    
    # ⏬ 股數
    share_outstanding <- as.numeric(select_clean_metric_row(d_balance_sheet(), "Share Issued")[1])
    
    ## 自定義DCF
    dcf_value <- NA
    
    # Gordon 模型 ------------------------------------------------------------
    if (input$dcf_mode == "gordon") {
      g <- input$g_gordon / 100
      if (g >= discount_rate) {
        showNotification("❌ 永續成長率 g 必須小於折現率", type = "error")
        return(NULL)
      }
      
      fcf_forecast <- fcf_start * (1 + g)^(0:(n - 1))
      pv_forecast <- sum(fcf_forecast / (1 + discount_rate)^(1:n))
      terminal_value <- fcf_forecast[n] * (1 + g) / (discount_rate - g)
      pv_terminal <- terminal_value / (1 + discount_rate)^n
      dcf_value <- pv_forecast + pv_terminal
    }
    
    # Two-Stage 模型 ---------------------------------------------------------
    if (input$dcf_mode == "two_stage") {
      g1 <- input$g_stage1 / 100
      g2 <- input$g_stage2 / 100
      r1 <- input$wacc_stage1 / 100
      r2 <- input$wacc_stage2 / 100
      yr_stage1 <- input$yr_stage1
      
      if (yr_stage1 <= 0 || yr_stage1 >= n) {
        showNotification("⚠️ 第一階段年數無效", type = "error")
        return(NULL)
      }
      
      fcf_stage1 <- fcf_start * cumprod(rep(1 + g1, yr_stage1))
      fcf_stage2 <- fcf_stage1[length(fcf_stage1)] * cumprod(rep(1 + g2, n - yr_stage1))
      pv_stage1 <- sum(fcf_stage1 / (1 + discount_rate)^(1:yr_stage1))
      pv_stage2 <- sum(fcf_stage2 / (1 + discount_rate)^((yr_stage1 + 1):n))
      terminal_value <- fcf_stage2[length(fcf_stage2)] * (1 + g2) / (discount_rate - g2)
      pv_terminal <- terminal_value / (1 + discount_rate)^n
      dcf_value <- pv_stage1 + pv_stage2 + pv_terminal
    }
    
    dcf_value_result(dcf_value)
    
    # 💵 股價估值 -------------------------------------------------------------
    if (!is.na(dcf_value) && !is.na(share_outstanding) && share_outstanding > 0) {
      stock_price_estimate_val(dcf_value / share_outstanding)
    } else {
      stock_price_estimate_val(NULL)
    }
    
    # 顯示估值結果 -----------------------------------------------------------
    output$vtxt_dcf_results <- renderText({
      if (is.na(dcf_value_result())) {
        return("⚠️ DCF 計算失敗")
      }
      
      # 基礎結果
      dcf_value <- dcf_value_result()
      msg <- glue::glue("企業總估值（DCF）：${round(dcf_value, 2)}")
      
      if (!is.na(share_outstanding) && share_outstanding > 0) {
        msg <- glue::glue("{msg}\n 每股估值：${round(dcf_value / share_outstanding, 2)}")
      } else {
        msg <- glue::glue("{msg}\n⚠️ 股數資訊無效，無法估算每股價格")
      }
      
      # 使用 generate_fcf_plot() 的結果
      fcf_df <- tryCatch(generate_fcf_plot(), error = function(e) NULL)
      fcf_values <- if (!is.null(fcf_df)) {
        paste(round(fcf_df$FCF, 2), collapse = ", ")
      } else {
        "⚠️ 無法取得預測 FCF"
      }
      
      # 加入使用參數資訊
      params <- list(
        "📌 估值模式" = input$dcf_mode,
        "📉 自由現金流預測年數" = input$years,
        "💵 預測 FCF" = fcf_values,
        "✅ 使用估算 WACC" = if (input$use_calculated_wacc) "是" else "否"
      )
      
      if (input$dcf_mode == "gordon") {
        params[["📈 永續成長率 g"]] <- paste0(input$g_gordon, " %")
        params[["🔻 折現率 WACC"]] <- paste0(input$wacc_gordon, " %")
      } else if (input$dcf_mode == "two_stage") {
        params[["📈 第一階段成長率 g₁"]] <- paste0(input$g_stage1, " %")
        params[["📈 第二階段成長率 g₂"]] <- paste0(input$g_stage2, " %")
        params[["🔻 第一階段 WACC₁"]] <- paste0(input$wacc_stage1, " %")
        params[["🔻 第二階段 WACC₂"]] <- paste0(input$wacc_stage2, " %")
        params[["📆 第一階段預測年數"]] <- input$yr_stage1
      }
      
      params[["🕒 估值時間"]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # 組合顯示文字
      param_text <- paste0(
        "\n\n🔧 使用參數一覽：\n",
        paste(names(params), params, sep = "：", collapse = "\n")
      )
      
      return(paste0(msg, param_text))
    })
    
    # 顯示圖表 ---------------------------------------------------------------
    output$fcf_plot <- renderPlot({
      df <- generate_fcf_plot()
      
      ggplot(df, aes(x = Year, y = FCF, linetype = Type)) +
        geom_line(size = 1.2, color = "black") +
        geom_point(aes(color = FCF < 0), size = 3) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
        scale_linetype_manual(values = c(
          "預設" = "dashed",
          "Gordon 預測" = "solid",
          "第一階段" = "dotted",
          "第二階段" = "twodash"
        )) +
        theme_bw() +
        labs(x = "年份", y = "FCF") + 
        theme(plot.title = element_text(size = 14, face = "bold"), legend.position = "top")
    })
  })
  
  ### 📦 顯示估算後股價 InfoBox ---------------------------------------------------
  
  output$ibx_enterprise_value_dcf <- renderInfoBox({
    dcf <- dcf_value_result()
    infoBox(
      title = "企業估值（DCF）",
      value = if (is.null(dcf) || is.na(dcf)) "N/A" else format_dollar_abbr(dcf),
      icon = icon("building"),
      color = "purple",
      fill = TRUE
    )
  })
  
  output$ibx_stock_value_dcf <- renderInfoBox({
    price <- stock_price_estimate_val()
    infoBox(
      title = "每股估值（DCF）",
      value = if (is.null(price) || is.na(price)) "N/A" else paste0("$", round(price, 2)),
      icon = icon("money-bill-wave"),
      color = "maroon",
      fill = TRUE
    )
  })
  
  ## DCF參數明細
  output$vtxt_dcf_setting_details <- renderUI({
    req(input$dcf_mode, input$years)
    
    # 模型敘述
    mode <- switch(input$dcf_mode,
                   "gordon" = "永續成長法（Gordon Growth）",
                   "two_stage" = "二階段成長法（Two-Stage Growth）")
    
    # 折現率來源
    discount <- if (input$use_calculated_wacc && !is.null(calculated_wacc())) {
      round(calculated_wacc() * 100, 2)
    } else {
      if (input$dcf_mode == "gordon") input$wacc_gordon else input$wacc_stage1
    }
    
    # FCF 起始值
    fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    fcf_start <- if (is.numeric(fcf_history) && length(fcf_history) > 0) head(fcf_history, 1) else NA
    fcf_start_text <- if (!is.na(fcf_start)) round(fcf_start) else "無資料"
    
    # FCF 預測資料
    if (input$calc > 0) {
      fcf_proj <- "已估算，圖表右方詳見"
    } else {
      rev_growth <- tryCatch(industry_standards[[input$industry_choice]]$rev_growth, error = function(e) NULL)
      growth_rate <- if (!is.null(rev_growth) && is.numeric(rev_growth) && length(rev_growth) > 0) {
        mean(rev_growth, na.rm = TRUE)
      } else {
        5
      }
      
      if (!is.na(fcf_start)) {
        proj <- tryCatch(
          fcf_projection(start_fcf = fcf_start, growth_rate = growth_rate, years = input$years),
          error = function(e) rep(NA, input$years)
        )
        fcf_proj <- paste(round(proj, 2), collapse = ", ")
      } else {
        fcf_proj <- "⚠️ 無法預測（起始 FCF 缺失）"
      }
    }
    
    # 產業參考參數（若有）
    inds <- industry_standards[[input$industry_choice]]
    beta <- if (!is.null(inds$beta_avg)) inds$beta_avg else input$capm_beta
    rm <- if (!is.null(inds$rm_avg)) inds$rm_avg else input$capm_rm
    
    # 組合顯示區塊
    tags$div(
      style = "line-height: 1.6; font-size: 16px;",
      
      tags$p(tags$b("📌 模型模式："), mode),
      tags$p(tags$b("📉 折現率 WACC："), tags$span(style = "color: steelblue;", paste0(discount, " %"))),
      tags$p(tags$b("🔮 預測年數："), paste0(input$years, " 年")),
      tags$p(tags$b("💵 起始 FCF："), tags$span(style = "color: green;", fcf_start_text)),
      tags$p(tags$b("📊 預測 FCF："), tags$code(fcf_proj)),
      
      if (input$dcf_mode == "gordon") {
        tags$p(tags$b("📈 永續成長率 g："),
               tags$span(style = "color: darkred;", paste0(input$g_gordon, " %")))
      } else {
        tagList(
          tags$p(tags$b("🔹 第一階段："),
                 paste0("g₁ = ", input$g_stage1, "%，WACC₁ = ", input$wacc_stage1, "%，年數 = ", input$yr_stage1)),
          tags$p(tags$b("🔸 第二階段："),
                 paste0("g₂ = ", input$g_stage2, "%，WACC₂ = ", input$wacc_stage2, "%"))
        )
      },
      
      # 折現率來源敘述
      tags$p(tags$b("🔧 折現率來源："),
             if (input$use_calculated_wacc) {
               tags$span(style = "color: darkgreen;", 
                         paste0("✅ 自動估算 WACC（來源：CAPM，rₑ = ", round(estimated_r_e() * 100, 2), "%）"))
             } else {
               tags$span(style = "color: red;", "⚠️ 手動輸入折現率")
             }
      ),
      
      # 額外補充 CAPM 參數
      tags$p(tags$b("📂 CAPM 參數來源：")),
      tags$ul(
        tags$li(paste("無風險利率 (Rf)：", input$capm_rf, "%")),
        tags$li(paste("β（Beta）：", beta)),
        tags$li(paste("市場報酬率 (Rm)：", rm, "%"))
      )
    )
  })
  
  observeEvent(input$reset_dcf, {
    updateRadioButtons(session, "dcf_mode", selected = "gordon")
    
    # 安全抓 rev_growth 平均
    rev_growth <- tryCatch(
      industry_standards[[input$industry_choice]]$rev_growth,
      error = function(e) NULL
    )
    
    default_growth <- if (!is.null(rev_growth) && is.numeric(rev_growth) && length(rev_growth) > 0) {
      mean(rev_growth, na.rm = TRUE)
    } else {
      5
    }
    
    # Gordon Growth 模式預設值
    updateNumericInput(session, "g_gordon", value = default_growth)
    updateNumericInput(session, "wacc_gordon", value = 10)
    
    # Two-Stage 預設值
    updateNumericInput(session, "g_stage1", value = 5)
    updateNumericInput(session, "g_stage2", value = 3)
    updateNumericInput(session, "wacc_stage1", value = 10)
    updateNumericInput(session, "wacc_stage2", value = 9)
    updateNumericInput(session, "yr_stage1", value = 3)
    
    # 通用欄位
    updateNumericInput(session, "years", value = 5)
    updateCheckboxInput(session, "use_calculated_wacc", value = FALSE)
    
    showNotification("🔁 所有 DCF 模型欄位已回復預設", type = "message")
  })
  
  # 🧠 顯示搜尋結果與歷史 ------------------------------------------------------
  values <- reactiveValues(recentsearch = c())
  
  observeEvent(input$search, {
    req(input$sc)
    ticker <- toupper(input$sc)
    
    # A. 顯示公司產業資訊
    output$search_results <- renderText({
      res <- get_yahoo_industry(ticker)
      res$display_text
    })
    
    # B. 按下搜尋後才寫入歷史 (且不重複)
    if (!(ticker %in% values$recentsearch)) {
      values$recentsearch <- head(c(ticker, values$recentsearch), 5)
      output$recentsearch <- renderText({ paste(values$recentsearch, collapse = ", ") })
    }
  })
  
  
  ### 📤 下載分析報告 ------------------------------------------------------------
  output$download_report <- downloadHandler(
    filename = function() paste0("YNow_Report_", Sys.Date(), ".html"),
    content = function(file) {
      tryCatch({
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
        
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          params = list(
            stock_code = paste("股票代碼:", input$sc),
            company_name = isolate(input$txt_corpname),
            summary = "（摘要）",
            warnings = "✅ currently no fraud risks detected..."
          ),
          envir = new.env(parent = globalenv()),
          output_format = "html_document"
        )
      }, error = function(e) {
        showNotification("❌ 報告產出失敗", type = "error")
      })
    }
  )
  
  output$today <- renderText({ format(Sys.Date(), "%Y/%m/%d") })
}

#-------------------- SHINY APP --------------------#

shinyApp(ui = ui, server = server)
