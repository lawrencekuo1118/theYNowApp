library(shiny)
source("global.R")

### ⬇️ Core: Get Yahoo Finance Data Pages
# setup.R
get.data <- function(stock_code) {
  base_url <- paste0("https://finance.yahoo.com/quote/", stock_code)
  
  urls <- list(
    main   = base_url,
    summary = paste0(base_url),
    income_statement = paste0(base_url, "/financials/"),
    balance_sheet    = paste0(base_url, "/balance-sheet/"),
    cash_flow        = paste0(base_url, "/cash-flow/")
  )
  
  # 使用 GET 加上 User-Agent 模擬瀏覽器
  fetch_page <- function(url) {
    tryCatch({
      httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0")) %>%
        xml2::read_html()
    }, error = function(e) {
      message("Error fetching ", url, ": ", conditionMessage(e))
      return(NULL)
    })
  }
  
  pages <- lapply(urls, fetch_page)
  # 移除 NULL 的頁面
  pages <- Filter(Negate(is.null), pages)
  
  return(pages)
}

### ⬇️ Simple Helpers
Loader1 <- function(x) {
  withLoader(x, type = "html", loader = "loader1")
}

BoxColor <- function(num) {
  if (is.na(num)) return("black")
  if (num < 0) return("red")
  return("black")
}

### ⬇️ Clean Financial Data Frame (for numeric plotting)
# 抓出指定 metric 資料列，並清理掉非數值欄位（如 TTM）
select_clean_metric_row <- function(df, metric_name) {
  if (!"Breakdown" %in% colnames(df)) {
    warning("資料缺少 'Breakdown' 欄")
    return(NULL)
  }
  
  if (!(metric_name %in% df$Breakdown)) {
    warning(paste0("找不到欄位: ", metric_name))
    return(NULL)
  }
  
  # 抓取該 metric 對應的資料列
  metric_row <- df[df$Breakdown == metric_name, , drop = FALSE]
  
  # 排除 "Breakdown" 與包含 "TTM" 的欄位
  metric_values <- metric_row[ , !names(metric_row) %in% "Breakdown"]
  metric_values <- metric_values[ , !grepl("TTM", names(metric_values)), drop = FALSE]
  
  # 數值轉換
  numeric_values <- as.numeric(gsub(",", "", unlist(metric_values)))
  
  return(numeric_values)
}

### ⬇️ 公式：計算當年均量
get_avg <- function(numeric_values) {
  if (length(numeric_values) < 2) {
    warning("資料不足以計算當年均量")
    return(NA)
  }
  # 計算當年均量：(末 + 始) / 2
  average <- numeric(length(numeric_values) - 1)
  for (i in seq_along(average)) {
    end <- numeric_values[1]
    begin <- numeric_values[2]
    if (!is.na(end) && !is.na(begin)) {
      average <- mean(end, begin, na.rm = TRUE)
    } else {
      average <- NA
    }
  }
  # 計算均量，保留兩位小數
  round(average, 2)
}


### ⬇️ 公式：計算成長率
get_avg_growth <- function(numeric_values) {
  if (length(numeric_values) < 2) {
    warning("資料不足以計算成長率")
    return(NA)
  }
  # 計算成長率：(左 - 右) / 右
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
  # 計算平均，保留兩位小數
  round(mean(growth_rates, na.rm = TRUE), 2)
}

