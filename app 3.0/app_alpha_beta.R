# install.packages("alphavantager")
# install.packages("writexl")
# library(alphavantager)
# Official, free tier (5 requests/min, 500/day)

# -------------------- Setup -------------------- #
library(shiny)
library(httr)
library(jsonlite)
library(glue)
library(DT)

api_key <- "LSWV6YY7KK5NJMA7"   # <--- insert your free key

# ---- API function ---- #
get_financials <- function(symbol = "AAPL") {
  base_url <- "https://www.alphavantage.co/query"
  
  fetch <- function(fun) {
    url <- glue("{base_url}?function={fun}&symbol={symbol}&apikey={api_key}")
    res <- httr::GET(url)
    if (res$status_code != 200) stop("HTTP ", res$status_code)
    json <- httr::content(res, as = "text")
    data <- jsonlite::fromJSON(json, simplifyVector = TRUE)
    return(data)
  }
  
  list(
    income = fetch("INCOME_STATEMENT"),
    balance = fetch("BALANCE_SHEET"),
    cashflow = fetch("CASH_FLOW")
  )
}

# ---- UI ---- #
ui <- fluidPage(
  titlePanel("📊 Financial Statements — Alpha Vantage Beta"),
  
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
    tryCatch(
      get_financials(input$symbol),
      error = function(e) {
        showNotification(e$message, type = "error")
        NULL
      }
    )
  })
  
  output$status <- renderText({
    req(fin_data())
    paste("✅ Loaded Alpha Vantage data for", input$symbol)
  })
  
  output$tbl_income <- renderDT({
    req(fin_data())
    datatable(fin_data()$income$annualReports, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tbl_balance <- renderDT({
    req(fin_data())
    datatable(fin_data()$balance$annualReports, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tbl_cashflow <- renderDT({
    req(fin_data())
    datatable(fin_data()$cashflow$annualReports, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Excel Download ---- #
  output$download_excel <- downloadHandler(
    filename = function() paste0(input$symbol, "_financials.xlsx"),
    content = function(file) {
      writexl::write_xlsx(fin_data(), path = file)
    }
  )
}

# ---- Run ---- #
shinyApp(ui, server)
