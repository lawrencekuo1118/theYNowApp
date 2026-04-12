# 📦 必要套件
library(httr)
library(jsonlite)
library(glue)
library(memoise)
library(purrr)
library(tidyr)

# ⬇️ 抓取 Yahoo 財報 HTML 頁面 -----------------------------------------------
get.data <- function(stock_code) {
  modules <- c(
    income_statement = "incomeStatementHistory",
    balance_sheet = "balanceSheetHistory",
    cash_flow = "cashflowStatementHistory"
  )
  
  # 📦 快取資料夾
  cache_dir <- "cache"
  if (!dir.exists(cache_dir)) dir.create(cache_dir)
  mem_cache <- memoise::cache_filesystem(cache_dir)
  
  # 🔁 抓 API 並內建退避策略
  fetch_module_impl <- function(stock_code, module_name) {
    base_url <- glue::glue(
      "https://query1.finance.yahoo.com/v10/finance/quoteSummary/{stock_code}?modules={module_name}"
    )
    
    backoff_times <- c(1.5, 3, 6)  # 退避秒數
    
    for (i in seq_along(backoff_times)) {
      Sys.sleep(backoff_times[i])
      
      resp <- httr::GET(
        base_url,
        httr::add_headers(
          `User-Agent` = "Mozilla/5.0",
          `Accept` = "application/json, text/javascript, */*; q=0.01",
          `Accept-Language` = "en-US,en;q=0.9"
        )
      )
      
      if (resp$status_code == 200) {
        parsed <- httr::content(resp, as = "text", encoding = "UTF-8") |>
          jsonlite::fromJSON(flatten = TRUE)
        
        if (!is.null(parsed$quoteSummary$result)) {
          return(parsed$quoteSummary$result[[1]][[module_name]][[1]])
        } else {
          warning("⚠️ 無法解析 ", module_name, " JSON 結構")
          return(NULL)
        }
      } else if (resp$status_code == 429) {
        warning(glue::glue("⚠️ 第 {i} 次限流 (HTTP 429)：{module_name}"))
      } else {
        warning("⚠️ HTTP 錯誤 ", resp$status_code, "：", module_name)
        return(NULL)
      }
    }
    warning("❌ 超過最大重試次數：", module_name)
    return(NULL)
  }
  
  # 🧠 套用快取（以 stock_code + module_name 為 key）
  fetch_module <- memoise::memoise(fetch_module_impl, cache = mem_cache)
  
  convert_yahoo_json_to_df <- function(json_list) {
    if (is.null(json_list)) return(NULL)
    
    purrr::map_dfr(json_list, \(period) {
      data.frame(
        Breakdown = names(period),
        Value = purrr::map_chr(period, \(x) {
          if (!is.null(x$raw)) as.character(x$raw) else NA_character_
        }),
        stringsAsFactors = FALSE
      )
    }) |>
      tidyr::pivot_wider(names_from = Breakdown, values_from = Value)
  }
  
  result <- purrr::imap(modules, \(module_name, name) {
    json <- fetch_module(stock_code, module_name)
    convert_yahoo_json_to_df(json)
  })
  
  names(result) <- names(modules)
  return(result)
}

# ⬇️ 抓取並清理指定財報項目（例如 "Free Cash Flow"） -------------------------
select_clean_metric_row <- function(df, metric_name) {
  if (is.null(df) || !"Breakdown" %in% colnames(df)) {
    warning("⚠️ 無效的財報資料：缺少 'Breakdown' 欄位")
    return(NULL)
  }
  
  # 處理欄位名稱：統一轉小寫，移除標點、空白（簡單正規化）
  clean_string <- function(x) {
    tolower(gsub("[^a-z0-9]", "", x))
  }
  
  target_clean <- clean_string(metric_name)
  df$Breakdown_clean <- clean_string(df$Breakdown)
  
  match_idx <- which(df$Breakdown_clean == target_clean)
  
  if (length(match_idx) == 0) {
    warning(glue::glue("⚠️ 找不到指定欄位: {metric_name}（經正規化比對）"))
    return(NULL)
  }
  
  # 提取對應 row
  values <- df[match_idx[1], , drop = FALSE]
  values <- values[, !grepl("Breakdown", names(values), ignore.case = TRUE), drop = FALSE]
  
  # 移除逗號、強制轉數值
  as.numeric(gsub(",", "", unlist(values)))
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

# ⬇️ 進行自由現金流投影 -------------------------------------------------------
fcf_projection <- function(start_fcf, growth_rate, years = 5) {
  growth_rate <- growth_rate / 100
  start_fcf * (1 + growth_rate)^(0:(years - 1))
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
