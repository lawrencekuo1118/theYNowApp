# app.R

source("global.R")
source("helpers.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
