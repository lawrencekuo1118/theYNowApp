# ==========================================
# app.R — 唯一 Shiny / shinyapps.io 進入點（app_12.0）
# ==========================================
# 重要：不可與根目錄的 ui.R／server.R 並存。
# shiny::shinyAppDir() 若偵測到 server.R 會優先走 server.R 模式，
# 導致 app.R 的 shinyApp(ui, server) 被忽略，最後落到
# 「No UI defined」預設頁（www-dir/index.html）。
#
# 因此 UI／Server 放在 ynow_ui.R／ynow_server.R，只由本檔 source。
# source(..., local = TRUE) 確保物件落在 app.R 評估環境，
# 而非 .GlobalEnv（否則 shinyapps 會 could not find function "server"）。

.ynow_app_dir <- normalizePath(".", mustWork = TRUE)
setwd(.ynow_app_dir)

source("global.R", local = TRUE, encoding = "UTF-8")
source("ynow_ui.R", local = TRUE, encoding = "UTF-8")
source("ynow_server.R", local = TRUE, encoding = "UTF-8")

if (!exists("ui", inherits = FALSE) || is.null(ui)) {
  stop("ynow_ui.R 未定義有效的 ui 物件")
}
if (!exists("server", inherits = FALSE) || !is.function(server)) {
  stop("ynow_server.R 未定義有效的 server 函式")
}

shiny::shinyApp(ui = ui, server = server)
