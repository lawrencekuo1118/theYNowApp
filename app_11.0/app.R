# ==========================================
# app.R — RStudio「Run App」進入點
# ==========================================
# 有 app.R 時 Shiny 不會自動再讀 ui.R／server.R，需明確 source。
# 請在 app_11.0 目錄按 Run App，或上一層執行：shiny::runApp("app_11.0")
.ynow_app_dir <- normalizePath(".", mustWork = TRUE)
setwd(.ynow_app_dir)
source("global.R", local = FALSE, encoding = "UTF-8")
source("ui.R", local = FALSE, encoding = "UTF-8")
source("server.R", local = FALSE, encoding = "UTF-8")
shiny::shinyApp(ui = ui, server = server)
