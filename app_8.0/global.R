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

# 🌟 核心修正：將虛擬環境移出專案資料夾 (避開 OneDrive 雲端同步)
# 使用 "~/.ynow_venv" 將環境建立在 Mac 的使用者根目錄下的隱藏資料夾
env_dir <- path.expand("~/.ynow_venv")
python_path <- file.path(env_dir, "bin", "python")

# 3. 強制指定路徑（動態偵測相對路徑是否存在）
if (file.exists(python_path)) {
  Sys.setenv(RETICULATE_PYTHON = python_path)
  reticulate::use_virtualenv(env_dir, required = TRUE)
  message("✅ 已連結至本機虛擬環境 Python: ", python_path)
} else {
  message("⚠️ 找不到本機虛擬環境，準備於專案內重新建立 (只需執行一次)...")
  
  # 建立虛擬環境
  reticulate::virtualenv_create(envname = env_dir) 
  
  # 🌟 關鍵精簡：合併所有套件為一行，並移除 ignore_installed = TRUE
  reticulate::virtualenv_install(
    envname = env_dir, 
    packages = c("numpy", "pandas", "selenium", "webdriver-manager", "yfinance")
  )
  
  # 建立完成後，綁定剛建好的路徑
  Sys.setenv(RETICULATE_PYTHON = python_path)
}

# 4. 強制 Shiny 使用我們剛建立好的乾淨環境
reticulate::use_virtualenv(env_dir, required = TRUE)
message("✅ Python 本機端虛擬環境已成功啟動！")

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
