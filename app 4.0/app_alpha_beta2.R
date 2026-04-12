# -------------------- Setup -------------------- #
library(shiny)
library(DT)
library(writexl)

# 引入您已經寫好 chrome 爬蟲函數的腳本
source("setup2.R") 

# ---- UI ---- #
ui <- fluidPage(
  titlePanel("📊 Financial Statements — Chrome Scraping Version"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      actionButton("load", "Load Data"),
      hr(),
      downloadButton("download_excel", "⬇️ Download Excel"),
      hr(),
      textOutput("status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Income", DTOutput("tbl_income")),
        tabPanel("Balance Sheet", DTOutput("tbl_balance")),
        tabPanel("Cash Flow", DTOutput("tbl_cashflow"))
      )
    )
  )
)

# ---- Server ---- #
server <- function(input, output, session) {
  
  fin_data <- eventReactive(input$load, {
    # 顯示爬蟲進度提示
    showNotification("啟動背景瀏覽器抓取資料中，請稍候...", type = "message", id = "scrape_msg", duration = NULL)
    
    tryCatch({
      # 呼叫 setup2.R 中的 get.data() 透過 Chrome 抓取三個分頁的 HTML
      raw_html <- get.data(input$symbol)
      
      # 透過 setup2.R 中的 clean_financial_table() 解析表格
      list(
        income = clean_financial_table(raw_html$income_statement),
        balance = clean_financial_table(raw_html$balance_sheet),
        cashflow = clean_financial_table(raw_html$cash_flow)
      )
    }, error = function(e) {
      showNotification(paste("❌ 抓取失敗:", e$message), type = "error")
      NULL
    }, finally = {
      removeNotification("scrape_msg") # 移除提示
    })
  })
  
  output$status <- renderText({
    req(fin_data())
    paste("✅ 成功透過模擬瀏覽器抓取", input$symbol, "的財務數據")
  })
  
  output$tbl_income <- renderDT({
    req(fin_data()$income)
    datatable(fin_data()$income, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tbl_balance <- renderDT({
    req(fin_data()$balance)
    datatable(fin_data()$balance, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tbl_cashflow <- renderDT({
    req(fin_data()$cashflow)
    datatable(fin_data()$cashflow, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Excel Download ---- #
  output$download_excel <- downloadHandler(
    filename = function() paste0(input$symbol, "_financials.xlsx"),
    content = function(file) {
      req(fin_data())
      writexl::write_xlsx(fin_data(), path = file)
    }
  )
}

# ---- Run ---- #
shinyApp(ui, server)
