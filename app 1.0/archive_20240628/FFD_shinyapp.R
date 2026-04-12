library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

source("global.R")
source("setup.R")
#source("industry_standard.R")

industry_standards <- list(
  Technology = list(eqt_multiplier = c(1.5, 3.0), rev_growth = c(5, 25)),
  Financial  = list(eqt_multiplier = c(5, 12), rev_growth = c(0, 10)),
  Retail     = list(eqt_multiplier = c(1, 3), rev_growth = c(0, 15)),
  Healthcare = list(eqt_multiplier = c(1, 4), rev_growth = c(0, 20))
)

get_box_color <- function(industry, metric, value) {
  bounds <- industry_standards[[industry]][[metric]]
  if (is.na(value) || length(bounds) != 2) return("black")
  if (value < bounds[1] || value > bounds[2]) return("red")
  return("black")
}

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
    tags$head(tags$style(HTML('.main-header .logo { font-weight: bold }'))),
    
    fluidRow(
      column(width = 12, titlePanel(h2("Let's find some frauds from financial reports!"))),
      column(
        width = 8,
        titlePanel(h5("a lawrence kuo shiny app")),
        textInput("sc", "Stock Code", value = "AAPL"),
        
        selectInput("industry_choice", "Select Industry",
                    choices = names(industry_standards),
                    selected = "Technology"),
        
        actionButton("search", "Search", icon = icon("search"))
      ),
      column(
        width = 4,
        textOutput("txt_corpname")
      )
    ),
    br(),
    
    fluidRow(
      infoBoxOutput("ibx_marketcap"),
      infoBoxOutput("ibx_stockprice"),
      infoBoxOutput("ibox_EPS")
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
                   dataTableOutput("tbCashFlow"),
                   downloadButton('CF_download', "Download the data")
          )
        ),
        
        tabBox(
          title = "Performance",
          width = "auto",
          
          tabPanel("KPI by Sheet", fluidRow(
            column(
              width = 12, 
              h4("Balance Sheet KPI"), 
              valueBoxOutput('vbx_eqt_multiplier')
            ),
            column(
              width = 12,
              h4("Income Statement KPI"),
              valueBoxOutput('vbx_rev_growth'),
              valueBoxOutput('vbx_gross_profit_growth'),
              valueBoxOutput('vbx_net_income_EBIT'),
              valueBoxOutput('vbx_net_profit_margin'),
              valueBoxOutput('vbx_gross_profit_margin')
            ),
            column(
              width = 12,
              h4("Cash Flow KPI"),
              valueBoxOutput('vbx_op_cash_flow_growth'),
              valueBoxOutput('vbx_inv_cash_flow_growth'),
              valueBoxOutput('vbx_fin_cash_flow_growth')
            )
          )
          ),
          
          tabPanel("Crossover KPIs", fluidRow(
            column(
              width = 12,
              valueBoxOutput('vbx_ROA'),
              valueBoxOutput('vbx_ROE'),
              valueBoxOutput('vbx_asset_turnover')
            ),
            column(
              width = 12, 
              valueBoxOutput('vbx_ocf_net_income')
            )
          )
          )
        )
      ),
      
      tabItem(
        tabName = "calculator",
        tabBox(
          title = "Calculator",
          width = "auto",
          
          tabPanel("Warnings",
                   fluidRow(
                     column(width = 12, textOutput("nofreecashflow")),
                     column(width = 12, textOutput("nooperatingcashflow")),
                     column(width = 12, textOutput("notdoingbusiness")),
                     column(width = 12, textOutput("notgettingcashback"))
                   )
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
        
        fluidRow(
          column(width = 12, h2("About The YNow App")),
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
  no_stock <- reactive({ input$sc })
  
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
    tmp()[[1]] %>% html_nodes(xpath = "//*[@id='quote-header-info']/div[2]/div[1]/div[1]/h1") %>% html_text()
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
  
  ## Output: Income Statement KPI
  # is_d.毛利率
  output$vbx_gross_profit_margin <- renderValueBox({
    req(d_income_statement(), input$industry_choice)
    gp <- get_avg(select_clean_metric_row(d_income_statement(), "Gross Profit"))
    rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue"))
    margin <- gp / rev * 100
    color <- get_box_color(input$industry_choice, "rev_growth", margin)
    valueBox(value = if (!is.na(margin)) paste0(round(margin, 2), "%") else "N/A",
             subtitle = "毛利率 Gross Profit Margin",
             color = color,
             icon = icon("percentage"))
  })
  
  # is_e.淨利率
  output$vbx_net_profit_margin <- renderValueBox({
    req(d_income_statement(), input$industry_choice)
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue"))
    margin <- net / rev * 100
    color <- get_box_color(input$industry_choice, "rev_growth", margin)
    valueBox(value = if (!is.na(margin)) paste0(round(margin, 2), "%") else "N/A",
             subtitle = "淨利率 Net Profit Margin",
             color = color,
             icon = icon("percentage"))
  })
  
  
  # is_b.毛利年均成長率
  output$vbx_gross_profit_growth <- renderValueBox({
    req(d_income_statement(), input$industry_choice)
    val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Gross Profit"))
    color <- get_box_color(input$industry_choice, "rev_growth", val)
    valueBox(value = if (!is.na(val)) paste0(val, "%") else "N/A",
             subtitle = "毛利年均成長率 Gross Profit Growth",
             color = color,
             icon = icon("chart-line"))
  })
  
  # is_a.營收年均成長率
  output$vbx_rev_growth <- renderValueBox({
    req(d_income_statement(), input$industry_choice)
    val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Total Revenue"))
    color <- get_box_color(input$industry_choice, "rev_growth", val)
    valueBox(value = if (!is.na(val)) paste0(val, "%") else "N/A",
             subtitle = "營收年均成長率 Revenue Growth",
             color = color,
             icon = icon("chart-line"))
  })
  
  # is_c.EBIT淨利轉換率
  output$vbx_net_income_EBIT <- renderValueBox({
    req(d_income_statement(), input$industry_choice)
    val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "EBIT"))
    color <- get_box_color(input$industry_choice, "rev_growth", val)
    valueBox(value = if (!is.na(val)) paste0(val, "%") else "N/A",
             subtitle = "稅前淨利 EBIT 成長率",
             color = color,
             icon = icon("chart-line"))
  })
  
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
  
  ## Balance Sheet KPI
  # cf_a.財務槓桿比率
  output$vbx_eqt_multiplier <- renderValueBox({
    req(d_balance_sheet(), input$industry_choice)
    avg_asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets"))
    avg_equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
    avg_ratio <- avg_asset / avg_equity
    color <- get_box_color(input$industry_choice, "eqt_multiplier", avg_ratio)
    valueBox(
      value = if (!is.na(avg_ratio)) round(avg_ratio, 2) else "N/A",
      subtitle = "財務槓桿比率 Financial Leverage Ratio",
      color = color,
      icon = icon("chart-line")
    )
  })
  
  
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
  
  ## Cash Flow KPI
  # cf_a.營運現金年均成長率
  output$vbx_op_cash_flow_growth <- renderValueBox({
    req(d_cash_flow(), input$industry_choice)
    val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    color <- get_box_color(input$industry_choice, "rev_growth", val)
    valueBox(value = if (!is.na(val)) paste0(val, "%") else "N/A",
             subtitle = "營運現金成長率 Operating CF Growth",
             color = color,
             icon = icon("chart-line"))
  })
  
  # cf_b.投資現金年均成長率
  output$vbx_inv_cash_flow_growth <- renderValueBox({
    req(d_cash_flow(), input$industry_choice)
    val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Investing Cash Flow"))
    color <- get_box_color(input$industry_choice, "rev_growth", val)
    valueBox(value = if (!is.na(val)) paste0(val, "%") else "N/A",
             subtitle = "投資現金成長率 Investing CF Growth",
             color = color,
             icon = icon("chart-line"))
  })
  
  # cf_c.融資現金年均成長率
  output$vbx_fin_cash_flow_growth <- renderValueBox({
    req(d_cash_flow(), input$industry_choice)
    val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Financing Cash Flow"))
    color <- get_box_color(input$industry_choice, "rev_growth", val)
    valueBox(value = if (!is.na(val)) paste0(val, "%") else "N/A",
             subtitle = "融資現金成長率 Financing CF Growth",
             color = color,
             icon = icon("chart-line"))
  })
  
  ### Crossover KPIs
  # cKPI_a.ROA資產報酬率
  output$vbx_ROA <- renderValueBox({
    req(d_income_statement(), d_balance_sheet(), input$industry_choice)
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets"))
    ratio <- net / asset * 100
    valueBox(value = if (!is.na(ratio)) paste0(round(ratio, 2), "%") else "N/A",
             subtitle = "資產報酬率 ROA",
             color = "black",
             icon = icon("chart-line"))
  })
  
  # cKPI_b.ROE股東報酬率
  output$vbx_ROE <- renderValueBox({
    req(d_income_statement(), d_balance_sheet(), input$industry_choice)
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
    ratio <- net / equity * 100
    valueBox(value = if (!is.na(ratio)) paste0(round(ratio, 2), "%") else "N/A",
             subtitle = "權益報酬率 ROE",
             color = "black",
             icon = icon("chart-line"))
  })
  
  # cKPI_c.資產周轉率
  output$vbx_asset_turnover <- renderValueBox({
    req(d_income_statement(), d_balance_sheet(), input$industry_choice)
    rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue"))
    asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets"))
    ratio <- rev / asset
    valueBox(value = if (!is.na(ratio)) round(ratio, 2) else "N/A",
             subtitle = "資產周轉率 Asset Turnover",
             color = "black",
             icon = icon("chart-line"))
  })
  
  # cKPI_d.現金流與淨利比
  output$vbx_ocf_net_income <- renderValueBox({
    req(d_income_statement(), d_cash_flow(), input$industry_choice)
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    ratio <- ocf / net
    valueBox(value = if (!is.na(ratio)) round(ratio, 2) else "N/A",
             subtitle = "現金流與淨利比 OCF/Net Income",
             color = "black",
             icon = icon("chart-line"))
  })
  
  ### Others
  observeEvent(input$sc, {
    tryCatch({
      values$recentsearch <- c(values$recentsearch, corp_name())
      output$recentsearch <- renderText({ paste(values$recentsearch, collapse = ", ") })
    }, error = function(e) {
      print(paste("Error in data retrieval: ", e$message))
    })
  })
  
  output$today <- renderText({ format(Sys.Date(), "%Y/%m/%d") })
}

#-------------------- SHINY APP --------------------#

shinyApp(ui = ui, server = server)
