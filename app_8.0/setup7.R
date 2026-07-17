# ==========================================
# setup7.R - 財報數據處理與輔助函數模組
# ==========================================

# KPI 顏色防呆判定 
get_box_color <- function(industry_choice, metric_name, val) {
  if (is.na(val) || is.null(val)) return("black") 
  if (val > 0) return("green")
  return("red")
}

# 轉換數字為 K / M / B 格式
format_dollar_abbr <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return("N/A")
  
  if (abs(x) >= 1e9) {
    paste0("$", round(x / 1e9, 2), "B")
  } else if (abs(x) >= 1e6) {
    paste0("$", round(x / 1e6, 2), "M")
  } else {
    paste0("$", round(x, 2))
  }
}

# ==========================================
# 從財報 DataFrame 中抽出特定科目的數值陣列 (自動排除 TTM 與處理空值字元)
# ==========================================
select_clean_metric_row <- function(df, metric_name) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NA)
  
  # 尋找目標科目的列
  row_idx <- grep(metric_name, df[[1]], ignore.case = TRUE)
  if (length(row_idx) == 0) return(NA) 
  
  # 排除第一欄 (科目名稱) 與 TTM 欄位
  col_names <- colnames(df)
  valid_cols <- !grepl("ttm", col_names, ignore.case = TRUE)
  valid_cols[1] <- FALSE
  
  # 萃取字串數值
  vals <- as.character(df[row_idx[1], valid_cols])
  
  # 1. 清除千分位逗號與錢字號
  vals <- gsub("[,\\$]", "", vals) 
  # 2. 清除前後多餘空白
  vals <- trimws(vals)             
  
  # 🌟 關鍵修復：將 Yahoo Finance 常見的無效字元主動轉為 NA
  vals[vals %in% c("-", "", "NA", "NaN", "N/A", "--")] <- NA
  
  # 3. 安全轉換為數值 (加上 suppressWarnings 徹底阻絕極端意外字元的警告)
  return(suppressWarnings(as.numeric(vals)))
}

# 裁切財務表格至指定科目
trim_financial_table <- function(df, end_metric) {
  if (is.null(df) || nrow(df) == 0) return(df)
  idx <- grep(end_metric, df[[1]], ignore.case = TRUE)
  if (length(idx) > 0) return(df[1:idx[1], ])
  return(df)
}

# 取得最新一期期末現金餘額
get_latest_cash_position <- function(df_cf) {
  if (is.null(df_cf) || nrow(df_cf) == 0) return(NA)
  cash_kws <- c("End Cash Position", "Ending Cash Position", "Cash at End of Period")
  for (kw in cash_kws) {
    val <- select_clean_metric_row(df_cf, kw)
    if (length(val) > 0 && !all(is.na(val))) return(as.numeric(na.omit(val))[1])
  }
  return(NA) 
}

# 計算陣列的平均值
get_avg <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) == 0) return(NA)
  return(mean(x, na.rm = TRUE))
}

# 計算陣列的平均成長率
get_avg_growth <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) < 2) return(NA)
  
  rates <- diff(x) / head(x, -1)
  rates <- rates[is.finite(rates)] 
  
  if (length(rates) == 0) return(NA)
  return(mean(rates, na.rm = TRUE) * 100)
}

# ==========================================
# 🚨 數據缺失警示 UI 共用函數
# ==========================================
ui_missing_data_alert <- function(check_list, fallback_msg = "系統已自動將上述項目視為 0 代入計算。請確認是否需要手動補齊數值以確保預測精準度。") {
  
  # 找出 NA, NULL 或空字串的項目
  missing_items <- names(check_list)[sapply(check_list, function(x) {
    is.null(x) || is.na(x) || (is.character(x) && trimws(x) == "")
  })]
  
  if (length(missing_items) > 0) {
    shiny::div(
      style = "color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; padding: 15px; border-radius: 4px; margin-bottom: 20px;",
      shiny::icon("exclamation-triangle"), 
      shiny::tags$b(" 偵測到數據缺失："), 
      shiny::tags$span(style = "color: #c7254e; font-weight: bold;", paste(missing_items, collapse = "、 ")),
      shiny::br(),
      shiny::span(style = "font-size: 13px;", fallback_msg)
    )
  } else {
    NULL # 無缺值則不回傳 UI
  }
}

# 確保數值安全，若無值則回傳 0
safe_num <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return(0)
  return(x)
}

# =========================================================
# 🌟 [共用繪圖引擎] 產生具有高度解讀意義的折現互動圖表 (Using Plotly)
# =========================================================
# 此函數會自動處理：大數字格式化 (B/M/K), 負值變紅,  ticker 注入標題, 資訊豐富的懸停提示
generate_safe_line_plot <- function(data, ticker_name, metric_name) {
  req(!is.null(data) && nrow(data) > 0)
  
  # 1. 資料清洗與轉換
  vals <- as.numeric(gsub(",", "", data[1, -1]))
  labels <- colnames(data)[-1]
  
  # 計算 CAGR (複合年均成長率) 作為輔助資訊 (防呆：需大於1點且為正)
  safe_cagr_msg <- ""
  if (length(vals) > 1 && vals[length(vals)] > 0 && vals[1] > 0) {
    years <- length(vals) - 1
    # 注意：這裡假設資料最右側是最新年份，左側是過去年份。
    # 如果 Yahoo 資料順序相反 [2023, 2022], 需要反轉計算。
    # 根據 format 這裡是 [Left -> Right: Past -> Latest]，所以用 Latest / Past
    cagr <- ((vals[1] / vals[years+1])^(1/years) - 1) * 100 
    # 但檢查 Yahoo 資料發現是 [Left -> Right: Latest -> Past], 
    # 故正確應為：
    cagr <- ((vals[1] / vals[length(vals)])^(1 / (length(vals) - 1)) - 1) * 100
    safe_cagr_msg <- paste0(" (", length(vals), "Y CAGR: ", round(cagr, 1), "%)")
  }
  
  # 2. 建立繪圖專用 DataFrame，並設計「更有解讀意義」的懸停文字
  plot_df <- data.frame(
    Year = labels, 
    Value = vals,
    # 🌟 核心改進：設計豐富的 Hover 內容
    HoverText = paste0(
      "<b>", ticker_name, " - ", metric_name, "</b><br>",
      "---------------------<br>",
      "年份 (FY): <b>", labels, "</b><br>",
      "數值: <b>$", format(vals, big.mark=",", scientific=F), "</b><br>",
      "狀態: <b>", ifelse(vals < 0, "<span style='color:red;'>Negative</span>", "<span style='color:green;'>Positive</span>"), "</b>"
    )
  )
  
  # 3. 繪製圖表 (使用 ggplot)
  p <- ggplot(plot_df, aes(x = Year, y = Value, group = 1, text = HoverText)) + # 🌟 綁定設計好的文字
    geom_line(color = "#7f8c8d", linewidth = 1) + # 使用中性灰色線條
    geom_point(aes(color = Value < 0), size = 2.5) + # 負值圓點變紅
    scale_color_manual(values = c("FALSE" = "#2c3e50", "TRUE" = "#e74c3c"), guide = "none") + # 深藍/紅風格
    scale_y_continuous(
      # 🌟 核心改進：Y 軸大數字自動格式化 ($123B, $45M)
      labels = scales::label_dollar(scale_cut = scales::cut_short_scale()),
      expand = expansion(mult = c(0.1, 0.15)) # 騰出空間給點跟文字
    ) +
    theme_bw() + 
    labs(
      # 🌟 核心改進：標題注入 Ticker 與 CAGR
      title = paste0("📈 ", ticker_name, " - ", metric_name, safe_cagr_msg), 
      x = "Fiscal Period", 
      y = ""
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#2c3e50"),
      axis.text.x = element_text(face = "bold")
    )
  
  # 4. 轉換為 plotly 並指定 tooltip
  ggplotly(p, tooltip = "text") # 🌟 關鍵修正：只顯示我們設計好的 HoverText
}
