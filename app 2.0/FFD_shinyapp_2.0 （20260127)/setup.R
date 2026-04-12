# ==============================================================================
# setup.R 穩定強化版 - 完整抓取、精準截斷、強制截圖至 www
# ==============================================================================
library(httr)
library(jsonlite)
library(rvest)
library(chromote)
library(dplyr)

# --- 1. 環境確保 ---
if (!dir.exists("www")) dir.create("www", recursive = TRUE)

# --- 2. 核心抓取函數 ---
get.data <- function(stock_code) {
  cat(paste0("\n🌐 執行階梯抓取: ", stock_code, "\n"))
  res_api <- try_api_fetch(stock_code)
  if (!is.null(res_api)) return(res_api)
  cat("⚠️ API 被阻擋，啟動 Chromote 模擬...\n")
  return(try_browser_fetch(stock_code))
}

# --- 內部函數 A：API 擷取 (保持邏輯) ---
try_api_fetch <- function(stock_code) {
  s <- httr::handle("https://query1.finance.yahoo.com")
  ua <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
  url <- paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/", 
                stock_code, "?modules=incomeStatementHistory,balanceSheetHistory,cashflowStatementHistory")
  
  tryCatch({
    res <- httr::GET(url, ua, handle = s, timeout(5))
    if (httr::status_code(res) != 200) return(NULL)
    
    json <- httr::content(res, as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON()
    result <- json$quoteSummary$result[[1]]
    
    process_json <- function(statements, list_name, type) {
      if (is.null(statements[[list_name]])) return(NULL)
      data <- statements[[list_name]]
      dates <- sapply(data$endDate, `[[`, "fmt")
      metrics <- setdiff(names(data), c("maxAge", "endDate"))
      
      df <- data.frame(Breakdown = metrics, stringsAsFactors = FALSE)
      for (i in seq_along(dates)) {
        df[[dates[i]]] <- sapply(metrics, function(m) {
          val <- data[[m]][[i]]$raw
          if (is.null(val)) return(NA) else return(as.numeric(val))
        })
      }
      if (nrow(df) > 1) df <- df[-1, ]
      
      target <- switch(type, "is" = "taxEffectOfUnusualItems", "bs" = "treasurySharesNumber", "cf" = "freeCashflow")
      idx <- which(df$Breakdown == target)
      if (length(idx) > 0) df <- df[1:idx[1], ]
      return(df)
    }
    
    list(
      income_statement = process_json(result$incomeStatementHistory, "incomeStatementHistory", "is"),
      balance_sheet    = process_json(result$balanceSheetHistory, "balanceSheetStatements", "bs"),
      cash_flow        = process_json(result$cashflowStatementHistory, "cashflowStatements", "cf")
    )
  }, error = function(e) NULL)
}

# --- 內部函數 B：Chromote 模擬 (完整 Breakdown 邏輯) ---
try_browser_fetch <- function(stock_code) {
  b <- ChromoteSession$new()
  b$Emulation$setDeviceMetricsOverride(width = 1600, height = 1200, deviceScaleFactor = 1, mobile = FALSE)
  on.exit({ try(b$parent$stop(), silent = TRUE) }, add = TRUE)
  
  fetch_page <- function(sub_path) {
    cat(paste0("📄 正在模擬抓取頁面: ", sub_path, "\n"))
    url <- paste0("https://finance.yahoo.com/quote/", stock_code, "/", sub_path)
    b$Page$navigate(url)
    Sys.sleep(12) 
    
    # --- 📸 截圖至 www (絕對路徑) ---
    screenshot_path <- file.path(getwd(), "www", paste0("last_sync_", sub_path, ".png"))
    try({ b$Page$captureScreenshot(filename = screenshot_path) }, silent = TRUE)
    
    # --- 🛠️ 核心 Breakdown 抓取邏輯 ---
    js_script <- "() => {
      let results = { headers: [], rows: [] };
      let headerRow = document.querySelector('div[class*=\"tableHeader\"]');
      if (headerRow) {
        results.headers = Array.from(headerRow.querySelectorAll('div[class*=\"column\"]'))
                           .map(c => c.innerText.trim());
      }
      let rows = document.querySelectorAll('div[data-test=\"fin-row\"], div[class*=\"row\"]');
      rows.forEach(row => {
        let cols = Array.from(row.querySelectorAll('div[class*=\"column\"], span'))
                        .map(c => c.innerText.trim());
        if (cols.length >= 2) results.rows.push(cols);
      });
      return JSON.stringify(results);
    }"
    
    res <- b$Runtime$evaluate(paste0("(", js_script, ")()"))
    if (is.null(res$result$value)) return(NULL)
    
    raw_data <- jsonlite::fromJSON(res$result$value)
    if (is.null(raw_data$rows) || length(raw_data$rows) == 0) return(NULL)
    
    # 1. 補齊欄位 (處理列長度不一)
    rows_list <- raw_data$rows
    max_cols <- max(sapply(rows_list, length))
    clean_rows <- lapply(rows_list, function(r) {
      if(length(r) < max_cols) c(r, rep(NA, max_cols - length(r))) else r
    })
    df <- as.data.frame(do.call(rbind, clean_rows), stringsAsFactors = FALSE)
    
    # 2. 移除首列 (通常是重複的標題行)
    if (nrow(df) > 1) df <- df[-1, ]
    
    # 3. 處理標題 (對齊 Breakdown 與 年份)
    actual_headers <- raw_data$headers
    if (length(actual_headers) > 0) {
      # 確保第一欄固定叫 Breakdown
      col_names <- c("Breakdown", actual_headers[actual_headers != "Breakdown"])
      colnames(df) <- col_names[1:min(ncol(df), length(col_names))]
    } else {
      colnames(df)[1] <- "Breakdown"
    }
    
    # 4. 根據子頁面執行精準截斷 (避免抓到廣告或無關項目)
    target <- case_when(
      sub_path == "financials"    ~ "Tax Effect of Unusual Items",
      sub_path == "balance-sheet" ~ "Treasury Shares Number",
      sub_path == "cash-flow"     ~ "Free Cash Flow",
      TRUE ~ NA_character_
    )
    
    if (!is.na(target)) {
      idx <- which(df$Breakdown == target)
      if (length(idx) > 0) df <- df[1:idx[1], ]
    }
    
    # 5. 清洗數值欄位
    for(i in 2:ncol(df)) {
      clean_vec <- gsub("[ ,]", "", as.character(df[[i]]))
      clean_vec[clean_vec %in% c("-", "--", "", "N/A")] <- NA
      df[[i]] <- suppressWarnings(as.numeric(clean_vec))
    }
    
    cat(paste0("✅ ", sub_path, " 處理完成，共 ", nrow(df), " 列。\n"))
    return(df)
  }
  
  list(
    income_statement = fetch_page("financials"),
    balance_sheet    = fetch_page("balance-sheet"),
    cash_flow        = fetch_page("cash-flow")
  )
}

# --- 3. 數據選取與清洗 ---
select_clean_metric_row <- function(df_input, metric_name) {
  # 自動解包 Reactive 或處理 NULL
  df <- if (is.function(df_input)) df_input() else df_input
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(rep(NA_real_, 5))
  
  # 標準化標籤映射
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
  
  # 安全搜尋：grep 可能回傳空或 NA
  row_idx <- grep(pattern, df$Breakdown, ignore.case = TRUE)[1]
  
  # 💡 關鍵修復：若找不到，回傳與欄位數相符的 NA 向量，防止後續 [[1]] 崩潰
  if (is.na(row_idx)) return(rep(NA_real_, ncol(df) - 1))
  
  # 提取並轉為數值
  raw_vals <- unlist(df[row_idx, -1])
  clean_vals <- suppressWarnings(as.numeric(gsub("[,% ]", "", as.character(raw_vals))))
  
  # 確保回傳長度不為 0
  if (length(clean_vals) == 0) return(rep(NA_real_, ncol(df) - 1))
  return(clean_vals)
}

# --- 4. 效能優化：數據預處理器 ---
process_financial_data <- function(data_list) {
  if (is.null(data_list)) return(NULL)
  
  # 定義所有模組會用到的 KPI 科目
  targets <- list(
    is = c("Total Revenue", "Gross Profit", "Operating Income", "Net Income", "Operating Expense"),
    bs = c("Common Stock Equity", "Total Assets"),
    cf = c("Operating Cash Flow", "Free Cash Flow")
  )
  
  cleaned <- list()
  cleaned$Total_Revenue <- select_clean_metric_row(data_list$income_statement, "Total Revenue")
  cleaned$Gross_Profit  <- select_clean_metric_row(data_list$income_statement, "Gross Profit")
  cleaned$Net_Income    <- select_clean_metric_row(data_list$income_statement, "Net Income")
  cleaned$Op_Expense    <- select_clean_metric_row(data_list$income_statement, "Operating Expense")
  cleaned$Total_Assets  <- select_clean_metric_row(data_list$balance_sheet, "Total Assets")
  cleaned$Equity        <- select_clean_metric_row(data_list$balance_sheet, "Common Stock Equity")
  cleaned$OCF           <- select_clean_metric_row(data_list$cash_flow, "Operating Cash Flow")
  cleaned$FCF           <- select_clean_metric_row(data_list$cash_flow, "Free Cash Flow")
  
  return(cleaned)
}

# --- 5. 計算類輔助函數 (加入長度檢查) ---
get_avg <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_real_)
  round(mean(na.omit(as.numeric(x)), na.rm = TRUE), 2)
}

get_avg_growth <- function(x) {
  if (is.null(x) || length(x) < 2 || all(is.na(x))) return(NA_real_)
  x_num <- na.omit(as.numeric(x))
  if (length(x_num) < 2) return(NA_real_)
  # 計算年化成長率
  growths <- (x_num[1:(length(x_num)-1)] - x_num[2:length(x_num)]) / abs(x_num[2:length(x_num)])
  round(mean(growths, na.rm = TRUE) * 100, 2)
}

estimate_historical_growth <- function(x) {
  v <- na.omit(as.numeric(x))
  if(length(v) < 2) return(5)
  round(mean(diff(v)/abs(head(v,-1)))*100, 2)
}

format_dollar_abbr <- function(x) {
  if(is.na(x)) return("N/A")
  if(abs(x) >= 1e12) return(paste0("$", round(x/1e12, 2), "T"))
  if(abs(x) >= 1e9) return(paste0("$", round(x/1e9, 2), "B"))
  paste0("$", round(x/1e6, 2), "M")
}

# ✅ 截圖並抓取基礎資訊
get_screenshot_and_basic_info <- function(stock_code) {
  req(stock_code)
  stock_code <- toupper(stock_code)
  url <- paste0("https://finance.yahoo.com/quote/", stock_code)
  file_path <- paste0("www/", stock_code, ".png")
  
  # 啟動 Chromote
  b <- ChromoteSession$new()
  
  tryCatch({
    b$Page$navigate(url)
    b$Page$loadEventFired()
    Sys.sleep(3) # 等待 JavaScript 渲染股價
    
    # 📸 執行截圖並存入 www
    b$screenshot(filename = file_path)
    
    # 🔍 同時直接從 DOM 抓取最準確的基礎資訊 (避免 OCR 誤差)
    doc <- read_html(url)
    
    res <- list(
      png_path = paste0(stock_code, ".png"),
      longName = doc %>% html_node("h1") %>% html_text(),
      price = doc %>% html_node("fin-streamer[data-field='regularMarketPrice']") %>% html_attr("value"),
      market_cap = doc %>% html_node("td[data-test='MARKET_CAP-value']") %>% html_text(),
      eps = doc %>% html_node("td[data-test='EPS_RATIO-value']") %>% html_text()
    )
    b$close()
    return(res)
  }, error = function(e) {
    if(!is.null(b)) b$close()
    message("Screenshot Error: ", e$message)
    return(NULL)
  })
}

