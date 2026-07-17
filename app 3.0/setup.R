source("global.R")  # 通常放 API key、共用參數等

# ⬇️ 抓取 Yahoo 財務資料頁面 ------------------------------------------------
get.data <- function(stock_code) {
  base_url <- paste0("https://finance.yahoo.com/quote/", stock_code)
  
  urls <- list(
    summary = base_url,
    income_statement = paste0(base_url, "/financials/"),
    balance_sheet = paste0(base_url, "/balance-sheet/"),
    cash_flow = paste0(base_url, "/cash-flow/")
  )
  
  fetch_page <- function(url) {
    tryCatch({
      httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0")) %>%
        xml2::read_html()
    }, error = function(e) {
      message("❌ 錯誤讀取頁面：", url, " - ", conditionMessage(e))
      return(NULL)
    })
  }
  
  pages <- lapply(urls, fetch_page)
  pages <- Filter(Negate(is.null), pages)  # 移除失敗的頁面
  return(pages)
}

# ⬇️ 清理指定的財務指標 ------------------------------------------------------
select_clean_metric_row <- function(df, metric_name) {
  if (!"Breakdown" %in% colnames(df)) {
    warning("⚠️ 資料中缺少 'Breakdown' 欄位")
    return(NULL)
  }
  
  if (!(metric_name %in% df$Breakdown)) {
    warning(paste0("⚠️ 無法找到指定欄位: ", metric_name))
    return(NULL)
  }
  
  metric_row <- df[df$Breakdown == metric_name, , drop = FALSE]
  
  metric_values <- metric_row[, !names(metric_row) %in% "Breakdown"]
  metric_values <- metric_values[, !grepl("TTM", names(metric_values)), drop = FALSE]
  
  numeric_values <- as.numeric(gsub(",", "", unlist(metric_values)))
  return(numeric_values)
}

# ⬇️ 計算某項指標的當年均值 --------------------------------------------------
get_avg <- function(numeric_values) {
  if (length(numeric_values) < 2) {
    warning("⚠️ 資料不足以計算平均值")
    return(NA)
  }
  begin <- numeric_values[2]
  end <- numeric_values[1]
  if (!is.na(begin) && !is.na(end)) {
    return(round(mean(c(begin, end), na.rm = TRUE), 2))
  }
  return(NA)
}

# ⬇️ 計算歷年成長率的平均 ----------------------------------------------------
get_avg_growth <- function(numeric_values) {
  if (length(numeric_values) < 2) {
    warning("⚠️ 資料不足以計算成長率")
    return(NA)
  }
  
  growth_rates <- numeric(length(numeric_values) - 1)
  for (i in seq_along(growth_rates)) {
    left <- numeric_values[i]
    right <- numeric_values[i + 1]
    
    if (!is.na(left) && !is.na(right) && right != 0) {
      growth_rates[i] <- (left - right) / right * 100
    } else {
      growth_rates[i] <- NA
    }
  }
  
  return(round(mean(growth_rates, na.rm = TRUE), 2))
}

# 自訂函數：轉換數字為 K / M / B 格式
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

### 按成長率估算FCF

# 🧮 成長率估算邏輯（封裝）---------------------------------------------
estimate_historical_growth <- function(fcf_vec) {
  fcf_vec <- na.omit(fcf_vec)
  if (length(fcf_vec) < 2) return(NA_real_)
  growth_rates <- diff(fcf_vec) / head(fcf_vec, -1)
  g <- mean(growth_rates, na.rm = TRUE)
  if (is.finite(g)) round(g * 100, 2) else NA_real_
}

# 📈 FCF 預測函數 ------------------------------------------------------
fcf_projection <- function(start_fcf, growth_rate, years) {
  rate <- growth_rate / 100
  fcf <- numeric(years)
  fcf[1] <- start_fcf
  for (i in 2:years) {
    fcf[i] <- fcf[i - 1] * (1 + rate)
  }
  round(fcf, 2)
}

