# ==========================================
# app.R — Shiny / shinyapps.io 進入點（app_12.0）
# ==========================================
# 重要：shinyapps.io 會在區域環境評估 app.R。
# 必須用 local=TRUE（或 sys.source 到 environment()），
# 否則 ui/server 被丟進 .GlobalEnv，會出現：
#   Error in server(...) : could not find function "server"
.ynow_app_dir <- normalizePath(".", mustWork = TRUE)
setwd(.ynow_app_dir)

app_env <- environment()
sys.source("global.R", envir = app_env, keep.source = TRUE)
sys.source("ui.R", envir = app_env, keep.source = TRUE)
sys.source("server.R", envir = app_env, keep.source = TRUE)

shiny::shinyApp(ui = ui, server = server)
