# ==========================================
# app.R — 唯一 Shiny / shinyapps.io 進入點（app_18.0）
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

# Hosted Posit Connect / shinyapps.io workers do not set SHINY_SERVER_VERSION and
# use opaque hostnames (e.g. ef34b7e4d842), so path / user checks are required.
.ynow_is_hosted_connect <- function() {
  wd <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error = function(e) getwd())
  nzchar(Sys.getenv("SHINY_SERVER_VERSION")) ||
    grepl("shinyapps", Sys.getenv("HOSTNAME"), ignore.case = TRUE) ||
    grepl("shinyapps", Sys.getenv("R_CONFIG_ACTIVE"), ignore.case = TRUE) ||
    identical(Sys.getenv("FORCE_SHINYAPPS_PYTHON"), "1") ||
    grepl("/srv/connect/apps", wd, fixed = TRUE) ||
    (identical(Sys.getenv("USER"), "shiny") && dir.exists("/srv/connect"))
}

# Block reticulate/uv from downloading CPython during cold start.
# Do NOT gate only on hostname — Connect 2026 logs showed CPython still
# downloading before Listen when detection missed. Prefer: no local venv ⇒ block.
# Listening must happen first; .ynow_ensure_python() re-enables downloads lazily.
.ynow_boot_shinyapps <- .ynow_is_hosted_connect()
.ynow_venv_python <- file.path(.ynow_app_dir, ".ynow_venv", "bin", "python")
if (isTRUE(.ynow_boot_shinyapps) || !file.exists(.ynow_venv_python)) {
  Sys.setenv(
    UV_PYTHON_DOWNLOADS = "never",
    RETICULATE_USE_MANAGED_VENV = "false"
  )
}

# R only auto-reads ~/.Renviron (RStudio also loads a project file locally).
# shinyapps.io needs an explicit load so bundled app_18.0/.Renviron is visible.
.ynow_renviron <- file.path(.ynow_app_dir, ".Renviron")
if (file.exists(.ynow_renviron)) {
  readRenviron(.ynow_renviron)
}

source("global.R", local = TRUE, encoding = "UTF-8")
source("ynow_ui.R", local = TRUE, encoding = "UTF-8")
source("ynow_server.R", local = TRUE, encoding = "UTF-8")

if (!exists("ui", inherits = FALSE) || is.null(ui)) {
  stop("ynow_ui.R 未定義有效的 ui 物件")
}
if (!exists("server", inherits = FALSE) || !is.function(server)) {
  stop("ynow_server.R 未定義有效的 server 函式")
}

shiny::shinyApp(
  ui = ui,
  server = server,
  onStart = function() {
    # Listen has started; allow deferred reticulate/uv installs on first real use.
    if (exists(".ynow_arm_python_install", mode = "function")) {
      .ynow_arm_python_install()
    }
  }
)
