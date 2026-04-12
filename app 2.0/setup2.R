source("global.R")  # 通常放 API key、共用參數等

# ⬇️ 抓取 Yahoo 財報 HTML 頁面 -----------------------------------------------
get.data <- function(stock_code) {
  base_url <- glue::glue("https://finance.yahoo.com/quote/{stock_code}")
  
  urls <- list(
    summary = base_url,
    income_statement = glue::glue("{base_url}/financials/"),
    balance_sheet = glue::glue("{base_url}/balance-sheet/"),
    cash_flow = glue::glue("{base_url}/cash-flow/")
  )
  
  fetch_page <- function(url) {
    tryCatch({
      httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0")) |>
        xml2::read_html()
    }, error = function(e) {
      message("❌ 錯誤讀取頁面：", url, " - ", conditionMessage(e))
      return(NULL)
    })
  }
  
  pages <- lapply(urls, fetch_page)
  Filter(Negate(is.null), pages)
}

# ⬇️ 抓取並清理指定財報項目（例如 "Free Cash Flow"） -------------------------
select_clean_metric_row <- function(df, metric_name) {
  if (!"Breakdown" %in% colnames(df)) {
    warning("⚠️ 資料中缺少 'Breakdown' 欄位")
    return(NULL)
  }
  
  if (!(metric_name %in% df$Breakdown)) {
    warning(glue::glue("⚠️ 無法找到指定欄位: {metric_name}"))
    return(NULL)
  }
  
  # 擷取對應列
  row_data <- df[df$Breakdown == metric_name, , drop = FALSE]
  
  # 移除非財報數值欄（如 TTM 或 Breakdown）
  row_data <- row_data[, !grepl("Breakdown|TTM", names(row_data)), drop = FALSE]
  
  # 去除逗號並轉換為數值
  cleaned_values <- suppressWarnings(as.numeric(gsub(",", "", unlist(row_data))))
  
  # 若全部為 NA，回傳 NULL 並提醒
  if (all(is.na(cleaned_values))) {
    warning(glue::glue("⚠️ 指標 {metric_name} 資料皆為 NA 或無法轉為數值"))
    return(NULL)
  }
  
  return(cleaned_values)
}

# ⬇️ 計算兩年平均值 ----------------------------------------------------------
get_avg <- function(x) {
  if (length(x) < 2) {
    warning("⚠️ 資料不足以計算平均值")
    return(NA)
  }
  mean(c(x[1], x[2]), na.rm = TRUE) |> round(2)
}

# ⬇️ 計算平均成長率（%） -----------------------------------------------------
get_avg_growth <- function(x) {
  if (length(x) < 2) {
    warning("⚠️ 資料不足以計算成長率")
    return(NA)
  }
  
  growth <- numeric(length(x) - 1)
  
  for (i in seq_along(growth)) {
    left <- x[i]
    right <- x[i + 1]
    
    if (!is.na(left) && !is.na(right) && right != 0) {
      growth[i] <- (left - right) / right * 100
    } else {
      growth[i] <- NA
    }
  }
  
  round(mean(growth, na.rm = TRUE), 2)
}

# ⬇️ 計算對數成長率（連續年） --------------------------------------------------
estimate_historical_growth <- function(x) {
  if (length(x) < 2) return(0)
  round(mean(diff(log(x))) * 100, 2)
}

# ⬇️ 美元數字格式化（加上 M / B 單位） ----------------------------------------
format_dollar_abbr <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return("N/A")
  if (abs(x) >= 1e9) {
    glue::glue("${round(x / 1e9, 2)}B")
  } else if (abs(x) >= 1e6) {
    glue::glue("${round(x / 1e6, 2)}M")
  } else {
    glue::glue("${round(x, 2)}")
  }
}
