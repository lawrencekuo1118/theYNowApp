# ==========================================
# global.R - 初始化與環境設定
# ==========================================

# 🐍 必須在載入 reticulate 之前鎖定路徑，否則可能落到系統 Python
# 使用相對捷徑路徑（./.ynow_venv → ~/.venv），不要 resolve 成絕對路徑
Sys.setenv(RETICULATE_PYTHON = "")
env_dir <- "./.ynow_venv"
python_path <- file.path(env_dir, "bin", "python")
if (!file.exists(python_path)) {
  stop("⚠️ 找不到 .ynow_venv 捷徑！請先在 Terminal 執行：ln -s ~/.venv .ynow_venv")
}
Sys.setenv(RETICULATE_PYTHON = python_path)

# knitr 設定（適用於 R Markdown）
knitr::opts_chunk$set(comment = NA)
knitr::opts_knit$set(global.par = TRUE)

# 全域選項：避免科學記號、數字位數、輸出寬度
options(scipen = 20, digits = 4, width = 90)
# 延長 chromote 的啟動與等待時間
options(chromote.timeout = 30)

# 📚 套件安裝與載入 --------------------------------------------------------
pacman::p_load(
  # 1. 核心資料處理 (Data Wrangling)
  tidyverse, dplyr, stringr, purrr, magrittr,
  
  # 2. 視覺化與報表 (Visualization & Reporting)
  ggplot2, ggrepel, plotly, DT, rmarkdown, knitr, scales,
  
  # 3. 網頁爬蟲與 Python 整合 (Scraping & Reticulate)
  rvest, xml2, chromote, reticulate, 
  
  # 4. Shiny 框架與 UI 元件 (Shiny & UI)
  shiny, shinydashboard, shinyjs, shinycustomloader, shinyWidgets, shinyBS, shinycssloaders,
  
  # 5. 效能優化與快取 (Performance & Cache)
  memoise, cachem, TTR
)

# ==========================================
# 🐍 透過「替身捷徑」連結專案虛擬環境
# ==========================================
reticulate::use_virtualenv(env_dir, required = TRUE)
message("✅ 已透過替身捷徑連結至 Python 虛擬環境: ", python_path)

# #region agent log
tryCatch({
  cfg <- reticulate::py_config()
  yf_ok <- tryCatch({
    reticulate::py_run_string("import yfinance")
    TRUE
  }, error = function(e) FALSE)
  cat(sprintf(
    paste0(
      '{"sessionId":"5745e8","timestamp":%.0f,"location":"global.R:venv",',
      '"message":"symlink venv binding","data":{"env_dir":"%s","python_path":"%s",',
      '"reticulate_python":"%s","yfinance_ok":%s},"hypothesisId":"H1","runId":"post-fix"}\n'
    ),
    as.numeric(Sys.time()) * 1000,
    gsub("\"", "\\\\\"", env_dir),
    gsub("\"", "\\\\\"", python_path),
    gsub("\"", "\\\\\"", as.character(cfg$python)),
    tolower(as.character(yf_ok))
  ), file = "/Users/lawrencekuo/Library/CloudStorage/OneDrive-Personal/coding/R/Just4Fun/theYNowApp/app_9.0/.cursor/debug-5745e8.log", append = TRUE)
}, error = function(e) invisible(NULL))
# #endregion

# ==========================================
# 應用程式進入點與全域設定
# ==========================================
# 1. 載入資料抓取與爬蟲模組 (確保這行在 Python 環境設定之後！)
source("setup.R", encoding = "UTF-8")
source("web_crawler.R", encoding = "UTF-8")

# 2. 載入產業標準清單與防呆顏色設定
source("industry_standards.R", encoding = "UTF-8")

# 3. 載入自定義的 Shiny Server 模組
source("kpi_module.R", encoding = "UTF-8")
source("investment_decision_module.R", encoding = "UTF-8")

source("ddm_module.R", encoding = "UTF-8")
source("fcf_projection_module.R", encoding = "UTF-8")
source("ri_module.R", encoding = "UTF-8")
source("pb_asset_module.R", encoding = "UTF-8")

# 4. 載入我們剛寫好的全域預設值設定檔
source("default_config.R", encoding = "UTF-8")

# ✅ 完成！ --------------------------------------------------------------
cat("✔️ 所有套件已載入，環境初始化完成。\n")
