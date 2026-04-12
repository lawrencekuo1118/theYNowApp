source("global.R")
source("setup.R")
source("industry_standards.R")
source("kpi_module.R")
#source("css.R")

#-------------------- UI --------------------#

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "The YNow App",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    collapsed = TRUE,
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
      tags$style(HTML('.main-header .logo { font-weight: bold }')),
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
    
    fluidRow(
      column(
        width = 8,
        titlePanel(h5("a lawrence kuo shiny app")),
        textInput("sc", "Stock Code", value = "AAPL"),
        h6("Stock Industry Recommendations from Yahoo"),
        verbatimTextOutput("search_results"),
        
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
            column(width = 12, textOutput("no_fraud_detected"))  # fallback
          )
        )
      ),
      
      tabItem(
        tabName = "calculator",
        tabBox(
          title = "DCF Calculator",
          width = "auto",
          
          # ⬇️ Tab 1：Stock Valuation
          tabPanel("Stock Valuation",
                   
                   # 🔷 InfoBox Row：估值結果
                   fluidRow(
                     column(width = 6, infoBoxOutput("ibx_enterprise_value_dcf")),
                     column(width = 6, infoBoxOutput("ibx_stock_value_dcf"))
                   ),
                   
                   br(),
                   
                   fluidRow(
                     column(
                       width = 6,
                       h4("📌 當前估值參數明細"),
                       verbatimTextOutput("vtxt_dcf_setting_details")
                     ),
                     column(
                       width = 6,
                       h4("📉 自由現金流預測"),
                       plotOutput("dft_fcf_plot")
                     )
                   ),
                   
                   hr(),
                   
                   # 🔷 詳細結果（含文字）
                   fluidRow(
                     column(
                       width = 6,
                       h4("DCF 估值結果（摘要）"),
                       verbatimTextOutput("vtxt_dcf_results")
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
                       
                       conditionalPanel(
                         condition = "input.dcf_mode == 'gordon'",
                         numericInput("gordon_growth_rate", "永續成長率（Gordon Growth）g (%)", value = 3),
                         numericInput("gordon_discount_rate", "折現率（Gordon Growth）WACC (%)", value = 10)
                       ),
                       
                       conditionalPanel(
                         condition = "input.dcf_mode == 'two_stage'",
                         numericInput("growth_stage1", "第一階段成長率 g₁ (%)", value = 5),
                         numericInput("growth_stage2", "第二階段成長率 g₂ (%)", value = 3),
                         numericInput("wacc_stage1", "第一階段 WACC₁ (%)", value = 10),
                         numericInput("wacc_stage2", "第二階段 WACC₂ (%)", value = 9),
                         numericInput("wacc", "折現率 WACC (%)", value = 10),
                         numericInput("growth", "永續成長率 g (%)", value = 3)
                       ),
                       
                       numericInput("years", "預測年數 n", value = 5, min = 1, max = 20),
                       numericInput("stage1_years", "第一階段預測年數", value = 3, min = 1),
                       #textInput("fcf", "預測FCF（用逗號分隔）", value = "100,110,120,130,140"),
                       checkboxInput("use_calculated_wacc", "使用估算的 WACC 作為折現率", value = TRUE),
                       actionButton("calc", "計算DCF")
                     ),
                     
                     # 🔷 InfoBox Row：詳細估值結果
                     column(
                       width = 8,
                       h4("🔎 詳細估值結果"),
                       fluidRow(
                         infoBoxOutput("ibx_enterprise_value_dcf"),
                         infoBoxOutput("ibx_stock_value_dcf")
                       ),
                       
                       verbatimTextOutput("vtxt_dcf_results"),
                       plotOutput("fcf_plot")
                     )
                   )
          ),
          
          # ⬇️ Tab 3：WACC Calculator
          tabPanel("WACC Calculator",
                   
                   # 🔷 InfoBox Row：估值結果
                   fluidRow(
                     infoBoxOutput("ibx_wacc"),
                     infoBoxOutput("ibx_re"),
                     infoBoxOutput("ibx_rd")
                   ),
                   
                   br(),
                   
                   fluidRow(
                     column(
                       width = 4,
                       numericInput("wacc_re", "股權成本 rₑ (%)", value = 10),
                       numericInput("wacc_rd", "負債成本 rᵈ (%)", value = 5),
                       numericInput("wacc_tax", "所得稅率 T (%)", value = 20),
                       actionButton("calc_wacc", "計算 WACC"),
                       
                       tags$hr(),
                       h4("📐 使用 CAPM 估算 rₑ"),
                       numericInput("capm_rf", "無風險利率 Rf (%)", value = 3, step = 0.1),
                       numericInput("capm_beta", "Beta (β)", value = 1.2, step = 0.1),
                       numericInput("capm_rm", "市場報酬率 Rm (%)", value = 8, step = 0.1),
                       actionButton("calc_capm", "📊 計算 rₑ（使用 CAPM）"),
                       checkboxInput("use_estimated_re", "✅ 使用估算的 rₑ 替代手動輸入", value = FALSE)
                     ),
                     
                     column(
                       width = 8,
                       h4("WACC 計算結果"),
                       verbatimTextOutput("wacc_result"),
                       tags$strong("WACC 公式："),
                       helpText("WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)"),
                       
                       tags$hr(),
                       h4("CAPM 計算結果"),
                       verbatimTextOutput("capm_result")
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
  values <- reactiveValues(recentsearch = NULL)
  
  tmp <- reactive({
    req(input$sc)
    isolate({
      data <- get.data(input$sc)
      if (is.null(data)) stop("Data not found")
      data
    })
  })
  
  corp_name <- reactive({
    req(tmp())
    node <- tmp()[[1]] %>% html_nodes(xpath = '//*[@id="nimbus-app"]/section/section/section/article/section[1]/div[1]/div/div[1]/section/h1/text()[1]')
    if (length(node) == 0) return("Corporation Name not found")
    html_text(node)
  })
  
  output$txt_corpname <- renderText({ corp_name() })
  
  ### FINANCE SUMMARY
  d_finance_summary <- reactive({
    req(tmp())
    
    tryCatch({
      page <- tmp()[[1]]  # Assuming this is the rvest-parsed HTML for the finance summary page
      
      # Left side: li[1-8]
      left <- lapply(1:8, function(i) {
        metric_xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/div[3]/ul/li[', i, ']/span[1]')
        value_xpath  <- paste0('//*[@id="nimbus-app"]/section/section/section/article/div[3]/ul/li[', i, ']/span[2]')
        metric <- page %>% html_nodes(xpath = metric_xpath) %>% html_text(trim = TRUE)
        value  <- page %>% html_nodes(xpath = value_xpath) %>% html_text(trim = TRUE)
        c(metric = ifelse(length(metric) == 1, metric, NA),
          value  = ifelse(length(value) == 1, value, NA))
      })
      
      # Right side: li[9-16]
      right <- lapply(9:16, function(i) {
        metric_xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/div[3]/ul/li[', i, ']/span[1]')
        value_xpath  <- paste0('//*[@id="nimbus-app"]/section/section/section/article/div[3]/ul/li[', i, ']/span[2]')
        metric <- page %>% html_nodes(xpath = metric_xpath) %>% html_text(trim = TRUE)
        value  <- page %>% html_nodes(xpath = value_xpath) %>% html_text(trim = TRUE)
        c(metric = ifelse(length(metric) == 1, metric, NA),
          value  = ifelse(length(value) == 1, value, NA))
      })
      
      df_left  <- do.call(rbind, left)
      df_right <- do.call(rbind, right)
      
      df_combined <- cbind(df_left, df_right)
      colnames(df_combined) <- c("-", "-", "-", "-")
      
      as.data.frame(df_combined, stringsAsFactors = FALSE)
      
    }, error = function(e) {
      message("Error parsing finance summary: ", e$message)
      data.frame(Error = "Could not retrieve Finance Summary", stringsAsFactors = FALSE)
    })
  })
  
  output$tbFinanceSummary <- renderDataTable({ d_finance_summary() })
  
  output$FS_download <- downloadHandler(
    filename = function() {
      paste0(as.character(no_stock()), "_financesummary_", as.character(Sys.Date()), ".csv")
    }, 
    content = function(file) {
      write.csv(d_finance_summary(), file, row.names = FALSE)
    }
  )
  
  output$ibx_marketcap <- renderInfoBox({
    req(d_finance_summary())
    infoBox("Market Cap.", h3(d_finance_summary()[1, 4], style = "font-size:150%;"), icon = icon("money-bill-trend-up"), color = "navy")
  })
  
  output$ibx_stockprice <- renderInfoBox({
    req(d_finance_summary())
    infoBox("Stock Price", h3(d_finance_summary()[1, 2], style = "font-size:150%;"), icon = icon("money-bill"), color = "orange")
  })
  
  output$ibox_EPS <- renderInfoBox({
    req(d_finance_summary())
    infoBox("EPS", h3(d_finance_summary()[4, 4], style = "font-size:150%;"), icon = icon("percent"), color = "green")
  })
  
  ### INCOME STATEMENT
  # Reactive: Parse and extract Income Statement table
  d_income_statement <- reactive({
    req(tmp())
    
    tryCatch({
      # Step 1: 建立請求
      url <- paste0("https://finance.yahoo.com/quote/", input$sc, "/financials/")
      page <- httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0")) %>%
        xml2::read_html()
      
      # Step 2: 抓欄位標題 TTM + 年份
      column_headers <- sapply(2:6, function(i) {
        xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[1]/div/div[', i, ']')
        node <- rvest::html_node(page, xpath = xpath)
        if (!is.null(node)) rvest::html_text(node, trim = TRUE) else paste0("Col_", i)
      })
      
      # Step 3: 動態抓第一欄 Breakdown，偵測列數
      breakdown_nodes <- rvest::html_nodes(
        page,
        xpath = '//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div/div[1]/div'
      )
      
      breakdown <- rvest::html_text(breakdown_nodes, trim = TRUE)
      n_rows <- length(breakdown)
      
      # Step 4: 動態抓每欄資料 (共 5 欄，每欄 n_rows 行)
      data_columns <- lapply(2:6, function(col_index) {
        sapply(1:n_rows, function(row_index) {
          xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div[',
                          row_index, ']/div[', col_index, ']')
          node <- page %>% rvest::html_node(xpath = xpath)
          if (!is.null(node)) html_text(node, trim = TRUE) else NA_character_
        })
      })
      
      # Step 5: 整理成 data frame 並加入 Breakdown
      df_main <- do.call(cbind, data_columns) %>%
        as.data.frame(stringsAsFactors = FALSE)
      colnames(df_main) <- column_headers
      
      df <- cbind(Breakdown = breakdown, df_main)
      return(df)
      
    }, error = function(e) {
      message("Error scraping income statement: ", e$message)
      data.frame(Error = "Failed to retrieve Income Statement", stringsAsFactors = FALSE)
    })
  })
  
  # Output: DataTable
  output$tbIncomeStatement <- renderDataTable({
    d_income_statement()
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
    req(tmp())
    
    tryCatch({
      # Step 1: 建立請求
      url <- paste0("https://finance.yahoo.com/quote/", input$sc, "/balance-sheet/")
      page <- httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0")) %>%
        xml2::read_html()
      
      # Step 2: 抓欄位標題 年份
      column_headers <- sapply(2:5, function(i) {
        xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[1]/div/div[', i, ']')
        node <- rvest::html_node(page, xpath = xpath)
        if (!is.null(node)) rvest::html_text(node, trim = TRUE) else paste0("Col_", i)
      })
      
      # Step 3: 動態抓第一欄 Breakdown，偵測列數
      breakdown_nodes <- rvest::html_nodes(
        page,
        xpath = '//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div/div[1]/div'
      )
      
      breakdown <- rvest::html_text(breakdown_nodes, trim = TRUE)
      n_rows <- length(breakdown)
      
      # Step 4: 動態抓每欄資料 (共 4 欄，每欄 n_rows 行)
      data_columns <- lapply(2:5, function(col_index) {
        sapply(1:n_rows, function(row_index) {
          xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div[',
                          row_index, ']/div[', col_index, ']')
          node <- page %>% rvest::html_node(xpath = xpath)
          if (!is.null(node)) html_text(node, trim = TRUE) else NA_character_
        })
      })
      
      # Step 5: 整理成 data frame 並加入 Breakdown
      df_main <- do.call(cbind, data_columns) %>%
        as.data.frame(stringsAsFactors = FALSE)
      colnames(df_main) <- column_headers
      
      df <- cbind(Breakdown = breakdown, df_main)
      return(df)
      
    }, error = function(e) {
      message("Error scraping balance sheet: ", e$message)
      data.frame(Error = "Failed to retrieve Balance Sheet", stringsAsFactors = FALSE)
    })
  })
  
  # Output: DataTable
  output$tbBalanceSheet <- renderDataTable({
    d_balance_sheet()
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
    req(tmp())
    
    tryCatch({
      # Step 1: 建立請求
      url <- paste0("https://finance.yahoo.com/quote/", input$sc, "/cash-flow/")
      page <- httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0")) %>%
        xml2::read_html()
      
      # Step 2: 抓欄位標題 TTM + 年份
      column_headers <- sapply(2:6, function(i) {
        xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[1]/div/div[', i, ']')
        node <- rvest::html_node(page, xpath = xpath)
        if (!is.null(node)) rvest::html_text(node, trim = TRUE) else paste0("Col_", i)
      })
      
      # Step 3: 動態抓第一欄 Breakdown，偵測列數
      breakdown_nodes <- rvest::html_nodes(
        page,
        xpath = '//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div/div[1]/div'
      )
      
      breakdown <- rvest::html_text(breakdown_nodes, trim = TRUE)
      n_rows <- length(breakdown)
      
      # Step 4: 動態抓每欄資料 (共 5 欄，每欄 n_rows 行)
      data_columns <- lapply(2:6, function(col_index) {
        sapply(1:n_rows, function(row_index) {
          xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div[',
                          row_index, ']/div[', col_index, ']')
          node <- page %>% rvest::html_node(xpath = xpath)
          if (!is.null(node)) html_text(node, trim = TRUE) else NA_character_
        })
      })
      
      # Step 5: 整理成 data frame 並加入 Breakdown
      df_main <- do.call(cbind, data_columns) %>%
        as.data.frame(stringsAsFactors = FALSE)
      colnames(df_main) <- column_headers
      
      df <- cbind(Breakdown = breakdown, df_main)
      return(df)
      
    }, error = function(e) {
      message("Error scraping cash flow: ", e$message)
      data.frame(Error = "Failed to retrieve Cash Flow", stringsAsFactors = FALSE)
    })
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
    cf_vals <- as.numeric(gsub(",", "", cf_vals))  # 移除逗號轉數字
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
    d_cash_flow()
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
    fcf = "",
    ocf = "",
    biz = "",
    cashback = "",
    debt = ""
  )
  
  # 各種 fraud 判斷邏輯
  output$nofreecashflow <- renderText({
    fcf <- get_avg(select_clean_metric_row(d_cash_flow(), "Free Cash Flow"))
    fraud_warnings$fcf <- if (fcf < 0) {
      "⚠️ 自由現金流為負數，可能營運困難或大量資本支出"
    } else {
      ""
    }
    fraud_warnings$fcf
  })
  
  output$nooperatingcashflow <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    fraud_warnings$ocf <- if (ocf < 0) {
      "⚠️ 營業現金流為負數，代表核心業務沒有產生現金"
    } else {
      ""
    }
    fraud_warnings$ocf
  })
  
  output$notdoingbusiness <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    fraud_warnings$biz <- if (ocf < net) {
      "⚠️ 營業現金流低於淨利，帳面賺錢但現金未實現"
    } else {
      ""
    }
    fraud_warnings$biz
  })
  
  output$notgettingcashback <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    fraud_warnings$cashback <- if (net > 0 && ocf < 0) {
      "⚠️ 淨利為正但現金流為負，獲利品質存疑"
    } else {
      ""
    }
    fraud_warnings$cashback
  })
  
  output$highdebttoequity <- renderText({
    total_liabilities <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Debt"))
    total_equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
    ratio <- total_liabilities / total_equity
    fraud_warnings$debt <- if (ratio > 2) {
      "⚠️ 負債對權益比率過高，財務槓桿風險大"
    } else {
      ""
    }
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
  estimated_r_e <- reactiveVal(NULL)
  calculated_wacc <- reactiveVal(NULL)
  stock_price_estimate_val <- reactiveVal(NULL)
  dcf_value_result <- reactiveVal(NULL)  # ✅ 新增這個！
  
  # 📈 CAPM 股東權益成本估算 --------------------------------------------------
  observeEvent(input$calc_capm, {
    Rf <- input$capm_rf / 100
    beta <- input$capm_beta
    Rm <- input$capm_rm / 100
    r_e_est <- Rf + beta * (Rm - Rf)
    estimated_r_e(r_e_est)
    
    output$capm_result <- renderText({
      glue::glue("📈 估算股東權益成本 (rₑ) = {round(r_e_est * 100, 2)} %\n（使用 CAPM: rₑ = Rf + β × (Rm - Rf)）")
    })
  })
  
  # 🧮 WACC 計算 ---------------------------------------------------------------
  observeEvent(input$calc_wacc, {
    req(d_balance_sheet())
    
    bs_data <- d_balance_sheet()
    equity <- select_clean_metric_row(bs_data, "Common Stock Equity")[1]
    debt <- select_clean_metric_row(bs_data, "Total Debt")[1]
    T <- input$wacc_tax / 100
    
    r_e <- if (input$use_estimated_re && !is.null(estimated_r_e())) estimated_r_e() else input$wacc_re / 100
    r_d <- input$wacc_rd / 100
    total_capital <- equity + debt
    
    wacc <- (equity / total_capital) * r_e + (debt / total_capital) * r_d * (1 - T)
    calculated_wacc(wacc)
    
    output$wacc_result <- renderText({
      glue::glue("股東權益 (E)：${formatC(equity, format = 'f', big.mark = ',', digits = 0)}\n",
                 "總負債 (D)：${formatC(debt, format = 'f', big.mark = ',', digits = 0)}\n",
                 "使用的 rₑ：{round(r_e * 100, 2)} %\nWACC = {round(wacc * 100, 2)} %")
    })
    
    # 顯示至 infoBox
    output$ibx_wacc <- renderInfoBox({
      infoBox("WACC", h3(paste0(round(wacc * 100, 2), " %")), style = "font-size:150%;", icon = icon("percent"), color = "purple")
    })
    output$ibx_re <- renderInfoBox({
      infoBox("股東權益成本 (rₑ)", h3(paste0(round(r_e * 100, 2), " %")), style = "font-size:150%;", icon = icon("chart-line"), color = "blue")
    })
    output$ibx_rd <- renderInfoBox({
      infoBox("負債成本 (rd)", h3(paste0(round(r_d * 100, 2), " %")), style = "font-size:150%;", icon = icon("university"), color = "teal")
    })
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
    } else {
      if (input$dcf_mode == "gordon") input$gordon_discount_rate / 100 else input$wacc_stage1 / 100
    }
    
    # ⏬ 股數
    share_outstanding <- as.numeric(select_clean_metric_row(d_balance_sheet(), "Share Issued")[1])
    dcf_value <- NA
    
    # Gordon 模型 ------------------------------------------------------------
    if (input$dcf_mode == "gordon") {
      g <- input$gordon_growth_rate / 100
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
      g1 <- input$growth_stage1 / 100
      g2 <- input$growth_stage2 / 100
      r1 <- input$wacc_stage1 / 100
      r2 <- input$wacc_stage2 / 100
      stage1_years <- input$stage1_years
      
      if (stage1_years <= 0 || stage1_years >= n) {
        showNotification("⚠️ 第一階段年數無效", type = "error")
        return(NULL)
      }
      
      fcf_stage1 <- fcf_start * cumprod(rep(1 + g1, stage1_years))
      fcf_stage2 <- fcf_stage1[length(fcf_stage1)] * cumprod(rep(1 + g2, n - stage1_years))
      pv_stage1 <- sum(fcf_stage1 / (1 + discount_rate)^(1:stage1_years))
      pv_stage2 <- sum(fcf_stage2 / (1 + discount_rate)^((stage1_years + 1):n))
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
      msg <- glue::glue("企業總估值（DCF）：${round(dcf_value, 2)}")
      if (!is.na(share_outstanding) && share_outstanding > 0) {
        msg <- glue::glue("{msg}\n👉 每股估值：${round(dcf_value / share_outstanding, 2)}")
      } else {
        msg <- glue::glue("{msg}\n⚠️ 股數資訊無效，無法估算每股價格")
      }
      msg
    })
    
    # 顯示圖表 ---------------------------------------------------------------
    output$dft_fcf_plot <- renderPlot({
      # 使用者尚未按下「計算DCF」按鈕，顯示產業預設預估圖
      if (input$calc == 0) {
        fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
        fcf_start <- head(fcf_history, 1)
        
        growth_range <- industry_standards[[input$industry_choice]]$rev_growth
        growth_rate <- if (!is.null(growth_range)) mean(growth_range) else 5
        
        proj <- fcf_projection(start_fcf = fcf_start, growth_rate = growth_rate, years = input$years)
        df <- data.frame(Year = 1:input$years, FCF = proj)
        
        ggplot(df, aes(x = Year, y = FCF)) +
          geom_line(color = "steelblue", size = 1.2) +
          geom_point(size = 2.5, color = "steelblue") +
          theme_minimal(base_size = 14) +
          labs(title = "📈 預設自由現金流預測圖", x = "年", y = "FCF")
      }
    })
    
    output$fcf_plot <- renderPlot({
      fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
      fcf_start <- head(fcf_history, 1)
      years <- base_year + seq_len(n)
      
      # DCF-Gordon Plot
      if (input$dcf_mode == "gordon") {
        fcf_forecast <- fcf_start * (1 + input$gordon_growth_rate / 100)^(0:(n - 1))
        df <- data.frame(
          Year = c((base_year - length(fcf_history) + 1):base_year, years),
          Value = c(fcf_history, fcf_forecast),
          Type = c(rep("歷史", length(fcf_history)), rep("預測", n))
        )
        
        # DCF-2-Stage Plot
      } else {
        fcf_stage1 <- fcf_start * cumprod(rep(1 + input$growth_stage1 / 100, input$stage1_years))
        fcf_stage2 <- fcf_stage1[length(fcf_stage1)] * cumprod(rep(1 + input$growth_stage2 / 100, n - input$stage1_years))
        df <- data.frame(
          Year = c((base_year - length(fcf_history) + 1):base_year, years),
          Value = c(fcf_history, fcf_stage1, fcf_stage2),
          Type = c(
            rep("歷史", length(fcf_history)),
            rep("第一階段", input$stage1_years),
            rep("第二階段", n - input$stage1_years)
          )
        )
      }
      
      ggplot(df, aes(x = Year, y = Value, linetype = Type)) +
        geom_line(size = 1.2, color = "black") +
        geom_point(aes(color = Value < 0), size = 3) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
        scale_linetype_manual(values = c("歷史" = "solid", "預測" = "dashed", "第一階段" = "dotted", "第二階段" = "twodash")) +
        theme_bw() +
        labs(title = "自由現金流預測", x = "年份", y = "FCF") +
        theme(plot.title = element_text(size = 14, face = "bold"), legend.position = "top")
    })
  })
  
  ### 📦 顯示估算後股價 InfoBox ---------------------------------------------------
  
  # 📦 企業估值（總值）
  output$ibx_enterprise_value_dcf <- renderInfoBox({
    dcf <- dcf_value_result()
    
    infoBox(
      title = "企業估值（DCF）",
      value = if (is.null(dcf) || is.na(dcf)) "N/A" else format_dollar_abbr(dcf),
      icon = icon("building"),
      color = if (is.null(dcf) || is.na(dcf)) "light" else "purple",
      fill = TRUE
    )
  })
  
  # 📦 每股估值（DCF）
  output$ibx_stock_value_dcf <- renderInfoBox({
    price <- stock_price_estimate_val()
    
    infoBox(
      title = "每股估值（DCF）",
      value = if (is.null(price) || is.na(price)) "N/A" else paste0("$", round(price, 2)),
      icon = icon("money-bill-wave"),
      color = if (is.null(price) || is.na(price)) "light" else "maroon",
      fill = TRUE
    )
  })
  
  ## DCF參數明細
  output$vtxt_dcf_setting_details <- renderText({
    mode <- switch(input$dcf_mode,
                   "gordon" = "永續成長法（Gordon Growth）",
                   "two_stage" = "二階段成長法（Two-Stage Growth）")
    
    discount <- if (input$use_calculated_wacc && !is.null(calculated_wacc())) {
      round(calculated_wacc() * 100, 2)
    } else {
      if (input$dcf_mode == "gordon") input$gordon_discount_rate else input$wacc_stage1
    }
    
    ## 預設值
    # 🧠 使用真實最後一筆 FCF 作為 start_fcf（若有）
    fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    val_fcf_start <- head(fcf_history, 1)
    avg_growth_rate <- mean(industry_standards[[input$industry_choice]]$rev_growth)
    
    fcf_values <- if (input$calc <= 0) {
      proj <- fcf_projection(start_fcf = val_fcf_start, growth_rate = avg_growth_rate, years = input$years)
      paste(round(proj, 2), collapse = ", ")
    } else if (input$dcf_mode == "gordon"){
      proj <- fcf_projection(start_fcf = val_fcf_start, growth_rate = input$gordon_growth_rate, years = input$years)
      paste(round(proj, 2), collapse = ", ")
    }
    else{
      proj <- fcf_projection(start_fcf = val_fcf_start, growth_rate = input$growth_stage1, years = input$years)
      paste(round(proj, 2), collapse = ", ")
    }
    
    detail_text <- glue::glue(
      "📌 模型模式：{mode}\n",
      "🔮 預測年數：{input$years} 年\n",
      "📉 折現率（WACC）：{discount} %\n",
      "💵 預測 FCF：{fcf_values}\n"
    )
    
    if (input$dcf_mode == "gordon") {
      detail_text <- paste0(detail_text, "\n",
                            "📈 永續成長率 g：", input$gordon_growth_rate, " %\n")
    }
    if (input$dcf_mode == "two_stage") {
      detail_text <- paste0(detail_text, "\n",
                            "🔹 第一階段 g₁：", input$growth_stage1, " %，WACC₁：", input$wacc_stage1, " %，年數：", input$stage1_years, "\n",
                            "🔸 第二階段 g₂：", input$growth_stage2, " %，WACC₂：", input$wacc_stage2, " %\n",
                            "📈 永續成長率 g：", input$growth, " %\n")
    }
    
    if (input$use_calculated_wacc) {
      detail_text <- paste0(detail_text, "✅ 使用自動計算 WACC\n")
    } else {
      detail_text <- paste0(detail_text, "⚠️ 使用手動輸入折現率\n")
    }
    
    return(detail_text)
  })
  
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
  
  # 📤 下載分析報告 ------------------------------------------------------------
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
