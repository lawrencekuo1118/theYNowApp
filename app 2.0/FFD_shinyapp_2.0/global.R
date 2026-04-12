# ==============================================================================
# 🚀 系統初始化與進階除錯日誌系統 (Enhanced Logging System)
# ==============================================================================

# 📦 1. 基礎路徑與關鍵 Helper -----------------------------------------------
log_dir <- "logs"
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

# --- 🛠️ 2. 全域工具函數 (Consolidated Utils) ---

# 數據選取與清洗：從財務報表中提取特定科目
select_clean_metric_row <- function(df_input, metric_name) {
  df <- if (is.function(df_input)) df_input() else df_input
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(rep(NA_real_, 5))
  
  mapping <- c(
    "Total Revenue"       = "totalRevenue|Total Revenue",
    "Gross Profit"        = "grossProfit|Gross Profit",
    "Operating Income"    = "operatingIncome|Operating Income",
    "Net Income"          = "netIncome|Net Income from Continuing.*",
    "Common Stock Equity" = "totalStockholderEquity|Common Stock Equity",
    "Total Assets"        = "totalAssets|Total Assets",
    "Operating Cash Flow" = "totalCashFromOperatingActivities|Operating Cash Flow",
    "Free Cash Flow"      = "freeCashflow|Free Cash Flow",
    "Operating Expense"   = "Total Operating Expense|Operating Expense"
  )
  
  pattern <- if (metric_name %in% names(mapping)) mapping[[metric_name]] else metric_name
  row_idx <- grep(pattern, df$Breakdown, ignore.case = TRUE)[1]
  
  if (is.na(row_idx)) return(rep(NA_real_, ncol(df) - 1))
  
  raw_vals <- unlist(df[row_idx, -1])
  clean_vals <- suppressWarnings(as.numeric(gsub("[,% ]", "", as.character(raw_vals))))
  
  if (length(clean_vals) == 0) return(rep(NA_real_, ncol(df) - 1))
  return(clean_vals)
}

# 計算平均值
get_avg <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_real_)
  round(mean(na.omit(as.numeric(x)), na.rm = TRUE), 2)
}

# 計算平均成長率
get_avg_growth <- function(x) {
  if (is.null(x) || length(x) < 2 || all(is.na(x))) return(NA_real_)
  x_num <- na.omit(as.numeric(x))
  if (length(x_num) < 2) return(NA_real_)
  growths <- (x_num[1:(length(x_num)-1)] - x_num[2:length(x_num)]) / abs(x_num[2:length(x_num)])
  round(mean(growths, na.rm = TRUE) * 100, 2)
}

# 預估歷史成長率 (用於 DCF 初始值)
estimate_historical_growth <- function(x) {
  v <- na.omit(as.numeric(x))
  if(length(v) < 2) return(5)
  round(mean(diff(v)/abs(head(v,-1)))*100, 2)
}

# 貨幣單位縮寫轉換
format_dollar_abbr <- function(x) {
  if(is.na(x)) return("N/A")
  if(abs(x) >= 1e12) return(paste0("$", round(x/1e12, 2), "T"))
  if(abs(x) >= 1e9) return(paste0("$", round(x/1e9, 2), "B"))
  paste0("$", round(x/1e6, 2), "M")
}

# --- 📝 3. 日誌系統初始化 (核心導引優化) ---
# 強力清理先前的 sink 連線，避免日誌寫入失敗
while(sink.number() > 0) sink()

timestamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_filename <- paste0("app_debug_", timestamp, ".log")
log_path     <- file.path(log_dir, log_filename)

# 開啟檔案連線
log_con <- file(log_path, open = "wt")

# 關鍵：同時導向 stdout 與 stderr
sink(log_con, split = TRUE)              # 導向 cat(), print()
sink(log_con, type = "message")         # 導向 warning(), stop(), message()

cat(paste0("\n==============================================\n"))
cat(paste0("🚀 Session Started: ", Sys.time(), "\n"))
cat(paste0("🖥️ System: ", Sys.info()["sysname"], " (", Sys.info()["release"], ")\n"))
cat(paste0("📂 Log File: ", log_filename, "\n"))
cat(paste0("==============================================\n"))

# 📌 4. 全域錯誤捕捉設定 (此處為抓取所有錯誤的關鍵) -----------------------
options(
  scipen = 20, 
  digits = 4, 
  width = 100,
  shiny.fullstacktrace = TRUE,  # 報錯時提供完整程式碼路徑
  # 當 App 發生崩潰時，強迫把最後的錯誤訊息印到日誌中
  shiny.error = function() {
    cat("\n🔥 [CRITICAL ERROR] 偵測到致命崩潰：\n")
    print(sys.calls()) # 印出所有調用堆疊
  }
)

# 處理編碼與地誌 (防止 Log 裡面的中文變亂碼)
if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_ALL", "Chinese (Traditional)_Taiwan.950")
} else {
  # Mac/Linux 系統建議保持 UTF-8
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
}

# 📚 5. 套件載入與日誌紀錄 -----------------------------------------------------------
cat("\n[1/3] 載入核心套件庫...\n")
if (!require("pacman")) install.packages("pacman")

# 定義需要檢查的套件
required_packages <- c(
  "dplyr", "tidyverse", "data.table", "lubridate", "stringr", "jsonlite",
  "ggplot2", "plotly", "DT", "httr", "rvest", "chromote",
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "shinycustomloader"
)

# 逐一載入並記錄，若失敗則立即在 Log 標記
for (pkg in required_packages) {
  success <- suppressMessages(pacman::p_load(pkg, character.only = TRUE))
  if (success) {
    # cat(paste0("  📦 ", pkg, " 已載入\n"))
  } else {
    cat(paste0("  ❌ 警告: 套件 ", pkg, " 載入可能失敗，請檢查權限。\n"))
  }
}
cat("✔️ 所有套件檢查完成。\n")

# 📂 6. 載入邏輯模組 ---------------------------------------
cat("\n[2/3] 正在載入邏輯模組...\n")
modules <- c("industry_standards.R", "setup.R", "kpi_module.R", "fcf_projection_module.R", "search_module.R")

for (mod in modules) {
  if (file.exists(mod)) {
    # 使用 tryCatch 包裹 source，確保模組內的語法錯誤會被寫入 Log
    tryCatch({
      source(mod, encoding = "UTF-8")
      cat(paste("  ✔️ Loading:", mod, "... Done.\n"))
    }, error = function(e) {
      cat(paste("  ❌ Error in", mod, ":", e$message, "\n"))
    })
  } else {
    cat(paste("  ❌ Error: 找不到模組", mod, "\n"))
  }
}

cat("\n[3/3] 環境初始化完成，App 啟動中...\n")
cat(paste0("==============================================\n\n"))

# 確保 log_con 是在全域環境中定義
if (!exists("log_con")) {
  log_con <- file(log_path, open = "wt")
}

onStop(function() {
  cat(paste0("\n🛑 [", Sys.time(), "] Session Ended. 正在關閉日誌連線...\n"))
  
  # 1. 先復原所有 sink，避免寫入已關閉的連線
  while(sink.number() > 0) sink()
  
  # 2. 檢查連線是否還有效，才執行關閉
  try({
    if (exists("log_con") && isOpen(log_con)) {
      close(log_con)
      cat("✔️ 日誌連線已成功釋放。\n")
    }
  }, silent = TRUE)
})

