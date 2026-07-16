# ==========================================
# global.R - 初始化與環境設定
# ==========================================

# 強制清空 Python 路徑緩存，讓 reticulate 重新搜尋
Sys.setenv(RETICULATE_PYTHON = "")

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
  shiny, shinydashboard, shinyjs, shinycustomloader, shinyWidgets,
  
  # 5. 效能優化與快取 (Performance & Cache)
  memoise, cachem
)

# ==========================================
# 🐍 解決 Mac 的 Python 啟動問題與建構專屬虛擬環境
# ==========================================

# 1. 強制清空 Python 路徑緩存，讓 reticulate 重新搜尋
Sys.setenv(RETICULATE_PYTHON = "")

# 🌟 核心修正：使用「替身術 (Symlink)」指向專案內的捷徑
# 這裡使用相對路徑 "./.ynow_venv"，它會順著捷徑找到外面的 ~/.venv
env_dir <- "./.ynow_venv"
python_path <- file.path(env_dir, "bin", "python")

# 3. 強制指定路徑（動態偵測捷徑是否存在）
if (file.exists(python_path)) {
  Sys.setenv(RETICULATE_PYTHON = python_path)
  reticulate::use_virtualenv(env_dir, required = TRUE)
  message("✅ 已透過替身捷徑連結至 Python 虛擬環境: ", python_path)
} else {
  # ⚠️ 防呆機制：如果找不到捷徑，停止執行並跳出警告，避免在 OneDrive 內誤建實體環境
  stop("⚠️ 找不到 .ynow_venv 捷徑！請先在 Terminal 執行：ln -s ~/.venv .ynow_venv")
}

# ==========================================
# 應用程式進入點與全域設定
# ==========================================
# 1. 載入資料抓取與爬蟲模組 (確保這行在 Python 環境設定之後！)
source("setup7.R", encoding = "UTF-8")
source("web_crawler5.R", encoding = "UTF-8")

# 2. 載入產業標準清單與防呆顏色設定
source("industry_standards.R", encoding = "UTF-8")

# 3. 載入自定義的 Shiny Server 模組
source("kpi_module.R", encoding = "UTF-8")
source("ddm_module.R", encoding = "UTF-8")
source("fcf_projection_module.R", encoding = "UTF-8")
source("ri_module.R", encoding = "UTF-8")

# 4. 載入我們剛寫好的全域預設值設定檔
source("default_config.R", encoding = "UTF-8")

# ✅ 完成！ --------------------------------------------------------------
cat("✔️ 所有套件已載入，環境初始化完成。\n")
