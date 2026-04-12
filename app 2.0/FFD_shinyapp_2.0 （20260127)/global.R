# 📦 1. 基礎路徑與環境變數設定 -----------------------------------------------
log_dir <- "logs"
if (!dir.exists(log_dir)) {
  dir.create(log_dir)
}

# 🛠️ 定義 Helper 函數 (定義在 rm 之前)
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

# --- 🧹 2. 自動清理舊檔案 (僅保留一次即可，包含 log 與 png) ---
all_files <- list.files(log_dir, pattern = "\\.(log|png)$", full.names = TRUE)
if (length(all_files) > 0) {
  file_info <- file.info(all_files)
  to_delete <- all_files[difftime(Sys.time(), file_info$mtime, units = "days") > 7]
  if (length(to_delete) > 0) {
    file.remove(to_delete)
    cat(paste("🧹 已自動清理", length(to_delete), "個過期日誌與截圖檔案。\n"))
  }
}

# --- 📝 3. 日誌系統初始化 ---
while(sink.number() > 0) sink()
timestamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_filename <- paste0("app_debug_", timestamp, ".log")
log_path     <- file.path(log_dir, log_filename)

log_con <- file(log_path, open = "a") 
sink(log_con, split = TRUE)          
sink(log_con, type = "message")      

cat(paste0("\n==============================================\n"))
cat(paste0("🚀 Session Started: ", Sys.time(), "\n"))
cat(paste0("📂 Log Directory: ", log_dir, "\n"))
cat(paste0("📄 Log File: ", log_filename, "\n"))
cat(paste0("==============================================\n"))

# --- 🧽 4. 環境清理 (確保 rm 不會殺掉剛才定義的東西) ---
keep_list <- c(
  "log_filename", "log_path", "log_dir", "log_con", "timestamp",
  "%||%"
)

# 清理先前開發遺留的變數
rm(list = setdiff(ls(all.names = TRUE), keep_list))

# 全域選項設定
options(scipen = 20, digits = 4, width = 90)

# 📚 5. 套件載入 -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyverse, data.table, lubridate, stringr, magrittr, Hmisc, reshape2,
  readr, readxl, rio, jsonlite, openxlsx, ggplot2, ggrepel, plotly, DT, 
  rvest, xml2, shiny, shinydashboard, shinyjs, shinycustomloader, shinyWidgets
)

cat("✔️ 所有套件已載入，環境初始化完成。\n\n")

# 📂 6. 載入邏輯模組 -------------------------------------------------------
source("industry_standards.R")
source("setup.R")
source("kpi_module.R")
source("fcf_projection_module.R", encoding = "UTF-8")
source("search_module.R")
