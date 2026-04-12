# server.R

server <- function(input, output, session) {
  values <- reactiveValues(recentsearch = NULL)
  
  no_stock <- reactive({ input$sc })
  
  tmp <- reactive({
    req(input$sc)
    isolate({
      data <- get.data(input$sc)
      if (length(data) == 0) stop("No data found for the provided stock code.")
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
    df_finance_summary <- data.frame()
    tryCatch({
      for (div in 1:2) {
        for (tr in 1:8) {
          for (i in 1:2) {
            xpath_finance_summary <- paste0("//*[@id=\"quote-summary\"]/div[", div, "]/table/tbody/tr[", tr, "]/td[", i, "]")
            df_finance_summary[tr, div + i - 1 + (div - 1)] <- tmp()[[2]] %>% html_nodes(xpath = xpath_finance_summary) %>% html_text()
          }
        }
      }
      colnames(df_finance_summary) <- c("Name", "Value", "Name", "Value")
    }, error = function(e) {
      message("Error parsing finance summary: ", e$message)
    })
    return(df_finance_summary)
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
  
  observeEvent(input$search, {
    tryCatch({
      values$recentsearch <- c(values$recentsearch, corp_name())
      output$recentsearch <- renderText({ paste(values$recentsearch, collapse = ", ") })
    }, error = function(e) {
      message("Error in data retrieval: ", e$message)
    })
  })
  
  output$today <- renderText({ format(Sys.Date(), "%Y/%m/%d") })
}
