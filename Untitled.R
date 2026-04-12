get_data <- function(no_stock){
  c_PageLocation <- paste0(no_stock, c("?p=", "/financials?p=", "/balance-sheet?p=", "/cash-flow?p="), no_stock)
  l_url <- list(   paste0("https://finance.yahoo.com/quote/", no_stock), # corporation name 
                   paste0("https://finance.yahoo.com/quote/", c_PageLocation[1]), # finance summary
                   paste0("https://finance.yahoo.com/quote/", c_PageLocation[2]), # income statement
                   paste0("https://finance.yahoo.com/quote/", c_PageLocation[3]), # balance sheet
                   paste0("https://finance.yahoo.com/quote/", c_PageLocation[4])) # cash flow
  return(lapply(l_url, read_html))  # Html Content
}

f4Periods <- function(no_stock){
  c_4periods <- c(); c_4periods[1:2] <- c("Breakdown", "TTM")
  xpath = paste0("//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[3]/div[1]/div/div[1]/div/div[", 3, "]") # column 3
  c_4periods[3] <- get_data(no_stock)[[3]] %>% html_nodes(xpath=xpath) %>% html_text %>% str_replace_all(., "/", "-")
  c_4periods[3] <- as.String((mdy(c_4periods[3]))) # column 3
  c_4periods[4] <- as.String((ymd(c_4periods[3]) - years(1))) # column 4
  c_4periods[5] <- as.String((ymd(c_4periods[3]) - years(2))) # column 5
  return(c_4periods)
}

ui <- fluidPage(
  textInput("sc", "Stock Code", value = "KO"),
  radioButtons("is_periods", h5("Period Options"), choices = "")
)

server <- function(input, output, session) {
  no_stock <- reactive({ input$sc })
  observeEvent(input$sc, {
    updateRadioButtons(session, "is_periods", choices = f4Periods(no_stock())[2:5])
  })
}

shinyApp(ui, server)
