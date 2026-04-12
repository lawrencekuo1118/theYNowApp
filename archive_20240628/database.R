library(shiny)
source("global.R")

# Database connection parameters
db_host <- "database-1.cf68kyykgnd5.ap-southeast-1.rds.amazonaws.com"
db_name <- "database-1"
db_user <- "admin"
db_password <- "fOCLOEPLiPkv3p8p2pmH"
db_port <- 3306

# Function to create a database connection
connect_to_db <- function() {
  dbConnect(RMySQL::MySQL(), 
            dbname = db_name, 
            host = db_host, 
            user = db_user, 
            password = db_password, 
            port = db_port)
}

# Define UI
ui <- fluidPage(
  textInput("url", "Enter URL"),
  actionButton("submit", "Submit"),
  dataTableOutput("history_titles_table")
)

# Define server logic
server <- function(input, output, session) {
  history_data <- reactiveVal(data.frame())
  
  observeEvent(input$submit, {
    url <- input$url
    if (url != "") {
      tryCatch({
        # Fetch the news content
        webpage <- read_html(url)
        news_category <- webpage %>% html_node(xpath = "//*[@id=\"category_name\"]") %>% html_text()
        news_titles <- webpage %>% html_node(xpath = "//*[@id=\"left_column\"]/div[1]/p/span[2]") %>% html_text()
        news_contents <- webpage %>% html_node(xpath = "//*[@id=\"news_content\"]/div[3]") %>% html_text() %>% gsub("\n", "", .) %>% trimws()
        
        # Create a data frame
        news_entry <- data.frame(
          Category = news_category,
          Title = news_titles,
          Content = news_contents,
          Link = url,
          Time = Sys.time(),
          stringsAsFactors = FALSE
        )
        
        # Save to reactive value
        history_data(rbind(history_data(), news_entry))
        
        # Insert into the database
        con <- connect_to_db()
        dbWriteTable(con, "news_data", news_entry, append = TRUE, row.names = FALSE)
        dbDisconnect(con)
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Could not fetch news content:", e$message),
          easyClose = TRUE
        ))
      })
    } else {
      showModal(modalDialog(
        title = "Notice",
        "Please enter a URL",
        easyClose = TRUE
      ))
    }
  })
  
  output$history_titles_table <- renderDataTable({
    history_data()
  })
}

shinyApp(ui, server)
