library(shiny)
library(rvest)

# Define UI
ui <- fluidPage(
  textInput("searchTerm", "Enter Search Term"),
  #  actionButton("searchBtn", "Search"),
  uiOutput("imagesUI")
  #  uiOutput("imagesUI2")
)

# Define server logic
server <- function(input, output) {
  
  # Define a reactive value to store the image URLs
  imageUrls <- reactiveValues(files = character(0))
  
  #  observeEvent(input$searchBtn, {
  #    url <- paste0("https://www.google.com/search?q=", input$searchTerm, "&source=lnms&tbm=isch")
  #    imageUrls$files <- read_html(url) %>% html_nodes("img") %>% html_attr("src")
  #  })
  
  tryCatch({ #  observeEvent(input$searchTerm, {
    observe({
      url <- paste0("https://www.google.com/search?q=", gsub(" ", "+", input$searchTerm), "&source=lnms&tbm=isch")
      imageUrls$files <- read_html(url) %>% html_nodes("img") %>% html_attr("src")
    })
    #  })
    
    output$imagesUI <- renderUI({
      if (length(imageUrls$files) == 0) return(NULL)
      imgs <- lapply(imageUrls$files, function(x) {
        tags$img(src = x, width = "200px")
      })
      do.call(tagList, imgs)
    })
    
    output$imagesUI2 <- renderUI({
      if (length(imageUrls$files) == 0) return(NULL)
      imgs <- tags$img(src = imageUrls$files[1], width = "200px")
      do.call(tagList, imgs)
    })
    
  }, error = function(e){
    return(HTML("<p>Warning: Error in open.connection: URL using bad/illegal format or missing URL</p>"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
