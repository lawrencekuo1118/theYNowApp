# ==========================================
# global.R - 初始化與環境設定
# ==========================================

# knitr 設定（適用於 R Markdown）
knitr::opts_chunk$set(comment = NA)
knitr::opts_knit$set(global.par = TRUE)

# 全域選項：避免科學記號、數字位數、輸出寬度
options(scipen = 20, digits = 4, width = 90)

# 📚 套件安裝與載入 --------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
if (!require("devtools")) install.packages("devtools")

pacman::p_load(
  dplyr, tidyverse, data.table, lubridate, stringr, magrittr, Hmisc, reshape2, purrr,
  readr, readxl, rio, jsonlite, openxlsx,
  ggplot2, ggrepel, plotly, DT, wordcloud,
  NLP, tm,
  rvest, xml2,
  shiny, shinydashboard, shinyjs, shinycustomloader, shinyWidgets,
  DBI, RMySQL,
  rmarkdown, knitr,
  chromote
)

# ==========================================
# 🧮 儀表板與財報共用輔助函數 (Helper Functions)
# ==========================================

# 1. 從財報 DataFrame 中抽出特定科目的數值陣列
select_clean_metric_row <- function(df, metric_name) {
  if (is.null(df) || nrow(df) == 0) return(NA)
  
  # 使用 grep 搜尋關鍵字
  row_idx <- grep(metric_name, df[[1]], ignore.case = TRUE)
  if (length(row_idx) == 0) return(NA) 
  
  vals <- as.character(df[row_idx[1], -1])
  vals <- gsub("[\\$,]", "", vals)                # 移除 $ 和 逗號
  vals <- gsub("\\((.*)\\)", "-\\1", vals)        # 將 (123) 轉為 -123
  return(as.numeric(vals))
}

# 2. 計算陣列的平均值
get_avg <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) == 0) return(NA)
  return(mean(x, na.rm = TRUE))
}

# 3. 計算陣列的平均成長率
get_avg_growth <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) < 2) return(NA)
  
  rates <- diff(x) / head(x, -1)
  rates <- rates[is.finite(rates)] 
  
  if(length(rates) == 0) return(NA)
  return(mean(rates, na.rm = TRUE) * 100)
}

# 4. 估算歷史成長率 (供 FCF 模組使用)
estimate_historical_growth <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) < 2) return(5) # 預設給予 5%
  
  prev_x <- head(x, -1)
  diff_x <- diff(x)
  
  g <- ifelse(prev_x == 0, NA, diff_x / prev_x)
  return(round(mean(g, na.rm = TRUE) * 100, 2))
}

# 🎨 5. KPI 顏色防呆判定 (覆蓋舊版，絕對不回傳 "white")
get_box_color <- function(industry_choice, metric_name, val) {
  if (is.na(val) || is.null(val)) return("black") # 如果沒有資料，一律顯示黑色
  
  # 簡單的正負值顏色邏輯 (您可以在 industry_standards.R 調整更複雜的判斷)
  if (val > 0) return("green")
  return("red")
}

# ✅ 完成！ --------------------------------------------------------------
cat("✔️ 所有套件已載入，環境初始化完成。\n")
