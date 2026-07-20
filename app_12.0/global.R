# ==========================================
# global.R - 初始化與環境設定（app_12.0 雲端相容版）
# ==========================================

# 🐍 Python：本機可選 .ynow_venv；shinyapps.io 用 requirements.txt 部署環境
Sys.setenv(RETICULATE_PYTHON = "")
env_dir <- "./.ynow_venv"
python_path <- file.path(env_dir, "bin", "python")
on_shinyapps <- nzchar(Sys.getenv("SHINY_SERVER_VERSION")) ||
  grepl("shinyapps", Sys.getenv("HOSTNAME"), ignore.case = TRUE) ||
  grepl("shinyapps", Sys.getenv("R_CONFIG_ACTIVE"), ignore.case = TRUE) ||
  identical(Sys.getenv("FORCE_SHINYAPPS_PYTHON"), "1")

if (file.exists(python_path) && !on_shinyapps) {
  Sys.setenv(RETICULATE_PYTHON = python_path)
} else {
  # 雲端：讓 reticulate 使用 Posit 依 requirements.txt 建立的環境
  Sys.unsetenv("RETICULATE_PYTHON")
}

# knitr 設定（適用於 R Markdown）
knitr::opts_chunk$set(comment = NA)
knitr::opts_knit$set(global.par = TRUE)

# 全域選項：避免科學記號、數字位數、輸出寬度
options(scipen = 20, digits = 4, width = 90)

# 📚 套件安裝與載入 --------------------------------------------------------
pacman::p_load(
  # 1. 核心資料處理 (Data Wrangling)
  tidyverse, dplyr, stringr, purrr, magrittr, glue,

  # 2. 視覺化與報表 (Visualization & Reporting)
  ggplot2, ggrepel, plotly, DT, rmarkdown, knitr, scales,

  # 3. 網頁與 Python 整合（雲端版不依賴 chromote / Chrome）
  rvest, xml2, reticulate,

  # 4. Shiny 框架與 UI 元件 (Shiny & UI)
  shiny, shinydashboard, shinyjs, shinycustomloader, shinyWidgets, shinyBS, shinycssloaders,

  # 5. 效能優化與快取 (Performance & Cache)
  memoise, cachem, TTR
)

# ==========================================
# 🐍 綁定 Python
# ==========================================
# shinyapps.io：以 py_require 宣告依賴（reticulate/uv 會安裝；單靠 requirements.txt 曾只裝到 numpy）
py_pkgs <- c(
  "pandas", "numpy", "yfinance", "requests", "beautifulsoup4", "lxml",
  "peewee", "platformdirs", "frozendict", "multitasking", "html5lib", "curl_cffi"
)
if (file.exists(python_path) && !on_shinyapps) {
  reticulate::use_virtualenv(env_dir, required = TRUE)
  message("✅ 已透過替身捷徑連結至 Python 虛擬環境: ", python_path)
} else {
  tryCatch(
    reticulate::py_require(py_pkgs),
    error = function(e) message("⚠️ py_require: ", e$message)
  )
  tryCatch(
    reticulate::py_config(),
    error = function(e) message("⚠️ py_config: ", e$message)
  )
  message("✅ 使用雲端 / 預設 Python 環境（app_12.0）")
}

# ==========================================
# 應用程式進入點與全域設定
# ==========================================
source("setup.R", encoding = "UTF-8")
source("web_crawler.R", encoding = "UTF-8")
source("industry_standards.R", encoding = "UTF-8")
source("kpi_module.R", encoding = "UTF-8")
source("investment_decision_module.R", encoding = "UTF-8")
source("ddm_module.R", encoding = "UTF-8")
source("fcf_projection_module.R", encoding = "UTF-8")
source("ri_module.R", encoding = "UTF-8")
source("pb_asset_module.R", encoding = "UTF-8")
source("backtest_module.R", encoding = "UTF-8")
source("backtest_validation.R", encoding = "UTF-8")
source("default_config.R", encoding = "UTF-8")

cat("✔️ 所有套件已載入，環境初始化完成。（app_12.0 — Backtest Logic Optimization）\n")
