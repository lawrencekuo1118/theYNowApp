source("global.R")

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
      # 原本 logo 粗體
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
        textInput("sc", "Stock Code", value = "AAPL"),
        
        #h6("Stock Industry Recommendations from Yahoo"),
        #search_module_ui("search"),
        #verbatimTextOutput("search_results"),
        
        # 產業選單
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
        
        # 用 flexbox 對齊按鈕與公司名稱
        tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          actionButton("search", "Search", icon = icon("search")),
          tags$div(
            style = "font-weight: bold; font-size: 16px; color: #333;",
            textOutput("txt_corpname")
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
    
    box(
      title = "Fraud Warnings",
      width = 12,        # box 的寬度通常建議設為 12 (滿版) 或根據需求調整
      status = "danger",  # 讓標題列變紅色，更有警告感
      solidHeader = TRUE, 
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
            h5("This is a web-based framework for building interactive data dashboards in R. The script loads a set of R packages for data processing, visualization, and presentation, and sets up some functions for retrieving financial data from Yahoo Finance, extracting the relevant period data from the financial reports, and presenting the information in the form of tables and plots. The user interface is defined in the ui object and includes various inputs such as a text input for entering a stock code, info boxes for presenting summary information, and tabbed panels for displaying the finance summary, income statement, and balance sheet. The data retrieval and processing are performed in the get.data and f4Periods functions, and the presentation of the data is done using Shiny components such as dataTableOutput and downloadButton.")
          )
        )
      )
    )
  )
)

#-------------------- SERVER --------------------#

server <- function(input, output, session) {
  # --- 1. 核心資料抓取與反應式物件 (全面日誌監控版) ---
  values <- reactiveValues(recentsearch = character(0))
  
  # --- 1. 核心資料鏈監控 (監控 401/404) ---
  tmp <- reactive({
    target_sc <- if(!is.null(input$searchText) && input$searchText != "") input$searchText else input$sc
    req(target_sc)
    
    cat(paste0("\n[", Sys.time(), "] 🚀 [DATA_FLOW] 請求股票: ", target_sc, "\n"))
    
    withProgress(message = '正在擷取財報數據...', value = 0.5, {
      res <- tryCatch({
        data <- get.data(target_sc) # 呼叫 setup.R
        if (is.null(data)) stop("數據回傳為空")
        cat(paste0("✅ [DATA_FLOW] 成功獲取: ", nrow(data$income_statement), " 筆紀錄\n"))
        data
      }, error = function(e) {
        cat(paste0("❌ [DATA_FLOW] 嚴重錯誤: ", e$message, "\n"))
        return(NULL)
      })
      return(res)
    })
  })
  
  # 分配數據給下游模組 (加入詳細檢查日誌，防止 downstream 模組崩潰)
  d_income_statement <- reactive({ 
    req(tmp())
    if(is.null(tmp()$income_statement)) cat(paste0("⚠️ [", Sys.time(), "] [WARN] 損益表數據為空，下游計算可能出錯。\n"))
    tmp()$income_statement 
  })
  
  d_balance_sheet <- reactive({ 
    req(tmp())
    tmp()$balance_sheet 
  })
  
  d_cash_flow <- reactive({ 
    req(tmp())
    tmp()$cash_flow 
  })
  
  # --- 2. 截圖與即時數據 (監控 Chromote) ---
  screenshot_data <- eventReactive({ input$search; input$searchButton }, {
    target_code <- if(!is.null(input$searchText) && input$searchText != "") input$searchText else input$sc
    req(target_code)
    
    cat(paste0("[", Sys.time(), "] 📸 [SCREENSHOT] 開始背景抓取: ", target_code, "\n"))
    
    tryCatch({
      info <- get_screenshot_and_basic_info(target_code)
      cat(paste0("✅ [SCREENSHOT] 價格: ", info$price, " | 截圖: ", info$png_path, "\n"))
      info
    }, error = function(e) {
      cat(paste0("🚨 [SCREENSHOT] 失敗: ", e$message, "\n"))
      return(list(price="N/A", market_cap="N/A", eps="N/A", png_path=NULL))
    })
  })
  
  # --- 3. UI 渲染監控 (防止介面灰屏) ---
  output$web_screenshot <- renderUI({
    data <- screenshot_data()
    req(data$png_path)
    tags$img(src = paste0(data$png_path, "?t=", as.numeric(Sys.time())), 
             style = "width:100%; border: 1px solid #ddd; border-radius: 5px;")
  })
  
  # 數據指標卡 (帶有錯誤隔離)
  output$ibx_marketcap <- renderInfoBox({
    val <- tryCatch(screenshot_data()$market_cap %||% "N/A", error = function(e) "N/A")
    infoBox("Market Cap", h3(val, style="font-weight:bold;"), icon = icon("globe"), color = "navy")
  })
  
  output$ibx_stockprice <- renderInfoBox({
    val <- tryCatch(screenshot_data()$price %||% "N/A", error = function(e) "N/A")
    display_price <- if(val != "N/A" && !grepl("\\$", val)) paste0("$", val) else val
    infoBox("Current Price", h3(display_price, style="font-weight:bold;"), icon = icon("tag"), color = "orange")
  })
  
  output$ibx_EPS <- renderInfoBox({
    val <- tryCatch(screenshot_data()$eps %||% "N/A", error = function(e) "N/A")
    infoBox("Latest EPS", h3(val, style="font-weight:bold;"), icon = icon("chart-line"), color = "green")
  })
  
  # --- 4. 公司全稱同步 ---
  corp_name <- reactive({
    screenshot_data()$longName %||% toupper(input$sc)
  })
  
  # --- 3. 表格輸出 (DataTable) ---
  
  # 建立一個統一的配置清單，減少重複代碼
  dt_options <- list(
    pageLength = 10, 
    scrollX = TRUE,
    columnDefs = list(list(className = 'dt-left', targets = 0)),
    autoWidth = TRUE
  )
  
  # --- 損益表輸出 ---
  output$tbIncomeStatement <- renderDataTable({
    df <- d_income_statement()
    req(df)
    
    datatable(as.data.frame(df), 
              options = dt_options,
              rownames = FALSE)
  }, server = FALSE)
  
  # --- 資產負債表輸出 ---
  output$tbBalanceSheet <- renderDataTable({
    df <- d_balance_sheet()
    req(df)
    
    datatable(as.data.frame(df), 
              options = dt_options,
              rownames = FALSE)
  }, server = FALSE)
  
  # --- 現金流量表輸出 ---
  output$tbCashFlow <- renderDataTable({
    df <- d_cash_flow()
    req(df)
    
    datatable(as.data.frame(df), 
              options = dt_options,
              rownames = FALSE)
  }, server = FALSE)
  
  # --- 4. 現金流圖表修正 ---
  output$cf_plot <- renderPlotly({
    df <- d_cash_flow()
    # 同時檢查數據與輸入是否存在
    req(df, input$cf_type) 
    
    # 強化匹配邏輯：確保 Breakdown 欄位存在
    if (!"Breakdown" %in% colnames(df)) return(NULL)
    
    row_idx <- grep(input$cf_type, df$Breakdown, ignore.case = TRUE)[1]
    
    # 如果找不到匹配的行或該行為空，回傳空值以防繪圖崩潰
    if(is.na(row_idx)) return(NULL)
    
    # 數據轉型安全檢查
    vals <- as.numeric(unlist(df[row_idx, -1]))
    labels <- colnames(df)[-1]
    
    # 排除可能出現的 NA 數值 (例如抓取時資料缺失)
    if(all(is.na(vals))) return(NULL)
    
    plot_df <- data.frame(
      Year = factor(labels, levels = rev(labels)),
      Value = vals
    )
    
    p <- ggplot(plot_df, aes(x = Year, y = Value, group = 1)) +
      geom_line(color = "#2c3e50", size = 1) +
      geom_point(aes(color = Value < 0, 
                     text = paste("Year:", Year, "<br>Value:", format(Value, big.mark=","))), 
                 size = 3) +
      scale_color_manual(values = c("FALSE" = "#27ae60", "TRUE" = "#e74c3c"), 
                         guide = "none") + # 隱藏圖例
      theme_minimal() +
      labs(title = paste(input$sc, "-", input$cf_type), 
           y = "Amount (USD)", x = "")
    
    # 使用 tooltip 指定滑鼠移上去要顯示的內容
    ggplotly(p, tooltip = "text") %>% 
      layout(margin = list(l = 50, r = 30, b = 50, t = 50))
  })
  
  ### ⬇️ 呼叫 KPI 模組，將主資料餵入
  kpi_module_server(
    id = "kpi",
    d_income_statement = d_income_statement,
    d_balance_sheet = d_balance_sheet,
    d_cash_flow = d_cash_flow,
    industry_choice = reactive({ input$industry })
  )
  
  # DCF產出
  dcf_data <- fcf_projection_module_server(
    id = "fcfmod",
    d_cash_flow = reactive(values$cashflow), # 確保這是您抓取的資料
    input_years = reactive(input$years),
    calc_trigger = reactive(input$calc),
    input_mode = reactive(input$dcf_mode),
    g_gordon = reactive(input$g_gordon),
    g_stage1 = reactive(input$g_stage1),
    g_stage2 = reactive(input$g_stage2),
    yr_stage1 = reactive(input$yr_stage1),
    discount_rate_g = reactive(input$wacc_gordon),
    discount_rate_s1 = reactive(input$wacc_stage1),
    discount_rate_s2 = reactive(input$wacc_stage2),
    share_outstanding = reactive({
      # 從資產負債表抓取 Ordinary Shares Number
      res <- select_clean_metric_row(values$balance, "Ordinary Shares Number")
      if(length(res) > 0) res[1] else 1e6 
    })
  )
  
  # 渲染您的 InfoBox (對應您給的 UI ID)
  output$ibx_enterprise_value_dcf <- renderInfoBox({
    res <- dcf_data()
    infoBox("企業價值 (EV)", format_dollar_abbr(res$ev), icon = icon("building"), color = "aqua")
  })
  
  output$ibx_stock_value_dcf <- renderInfoBox({
    res <- dcf_data()
    infoBox("估計股價", paste0("$", round(res$price, 2)), icon = icon("dollar-sign"), color = "green")
  })
  
  ### Fraud Risk Warnings
  # 先定義 reactiveValues 來存每個警訊結果
  fraud_warnings <- reactiveValues(
    fcf = "",
    ocf = "",
    biz = "",
    cashback = "",
    debt = ""
  )
  
  # 各種 fraud 判斷邏輯（安全性強化版）
  output$nofreecashflow <- renderText({
    req(d_cash_flow()) 
    fcf <- get_avg(select_clean_metric_row(d_cash_flow(), "Free Cash Flow"))
    
    if (is.na(fcf)) return("⚠️ 無法取得自由現金流資料")
    
    if (fcf < 0) {
      fraud_warnings$fcf <- "🚩 警訊：該公司自由現金流為負。"
    } else {
      fraud_warnings$fcf <- ""
    }
    fraud_warnings$fcf
  })
  
  output$nooperatingcashflow <- renderText({
    req(d_cash_flow())
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    
    if (is.na(ocf)) return("⚠️ 無法取得營業現金流資料")
    
    fraud_warnings$ocf <- if (ocf < 0) {
      "⚠️ 營業現金流為負數，代表核心業務沒有產生現金"
    } else {
      ""
    }
    fraud_warnings$ocf
  })
  
  output$notdoingbusiness <- renderText({
    req(d_cash_flow(), d_income_statement())
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    
    if (is.na(ocf) || is.na(net)) return("⚠️ 資料不足，無法判斷現金實現情況")
    
    fraud_warnings$biz <- if (ocf < net) {
      "⚠️ 營業現金流低於淨利，帳面賺錢但現金未實現"
    } else {
      ""
    }
    fraud_warnings$biz
  })
  
  output$notgettingcashback <- renderText({
    req(d_cash_flow(), d_income_statement())
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    
    if (is.na(ocf) || is.na(net)) return("")
    
    fraud_warnings$cashback <- if (net > 0 && ocf < 0) {
      "⚠️ 淨利為正但現金流為負，獲利品質存疑"
    } else {
      ""
    }
    fraud_warnings$cashback
  })
  
  output$highdebttoequity <- renderText({
    req(d_balance_sheet())
    total_liabilities <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Debt"))
    total_equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
    
    if (is.na(total_liabilities) || is.na(total_equity) || total_equity == 0) return("⚠️ 資料不足，無法計算負債比")
    
    ratio <- total_liabilities / total_equity
    fraud_warnings$debt <- if (ratio > 2) {
      "⚠️ 負債對權益比率過高，財務槓桿風險大"
    } else {
      ""
    }
    fraud_warnings$debt
  })
  
  # fallback 顯示
  output$no_fraud_detected <- renderText({
    # 確保所有 reactive 都有運行過
    req(fraud_warnings)
    
    # 檢查是否所有警訊都是空的
    all_clear <- all(
      sapply(list(fraud_warnings$fcf, fraud_warnings$ocf, fraud_warnings$biz, fraud_warnings$cashback, fraud_warnings$debt), 
             function(x) is.null(x) || x == "")
    )
    
    if (all_clear) {
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
  
  # 🧠 顯示搜尋結果與歷史 ------------------------------------------------------
  values <- reactiveValues(recentsearch = NULL)
  
  observeEvent(input$search, {
    output$search_results <- renderText({
      req(input$sc)
      get_yahoo_industry(input$sc)
    })
  })
  
  observeEvent(input$sc, {
    tryCatch({
      values$recentsearch <- c(values$recentsearch, corp_name())
      output$recentsearch <- renderText({ paste(values$recentsearch, collapse = ", ") })
    }, error = function(e) {
      print(paste("Error in data retrieval:", e$message))
    })
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
