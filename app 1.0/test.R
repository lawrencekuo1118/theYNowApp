library(shiny)
library(rvest)
library(dplyr)

# Define the new get.data function to scrape data from MOPS
get.data <- function(stock_no, year, season) {
  # Construct the URL for MOPS
  url <- paste0("https://mops.twse.com.tw/server-java/t164sb01?step=1&CO_ID=", 
                stock_no, "&SYEAR=", year, "&SSEASON=", season, "&REPORT_ID=C")
  
  # Scrape the data from the webpage
  webpage <- read_html(url)
  data <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  if (length(data) == 0) {
    return(NULL)  # If no data is found, return NULL
  }
  
  df <- as.data.frame(data[[1]])  # Extract the first table
  
  # Clean and convert the data: remove commas and convert to numeric where appropriate
  df_cleaned <- df %>%
    mutate(across(everything(), ~ as.numeric(gsub(",", "", .)), .names = "cleaned_{col}"))  # Clean numeric values
  
  return(df_cleaned)
}

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Financial Data from MOPS"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("stock_no", "Stock Number:", value = "2330"),
      numericInput("year", "Year:", value = 2024, min = 2000, max = 2024),
      selectInput("season", "Season:", choices = c(1, 2, 3, 4), selected = 2),
      actionButton("goButton", "Get Data")
    ),
    
    mainPanel(
      tableOutput("table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to scrape data when the button is pressed
  data <- eventReactive(input$goButton, {
    stock_no <- input$stock_no
    year <- input$year
    season <- input$season
    get.data(stock_no, year, season)
  })
  
  # Output the scraped data as a table
  output$table <- renderTable({
    df <- data()
    if (is.null(df)) {
      return("No data found for the specified stock number.")
    }
    df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
