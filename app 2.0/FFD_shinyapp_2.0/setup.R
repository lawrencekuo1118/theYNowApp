# ==============================================================================
# setup.R 穩定強化版 - 聚焦數據抓取與截圖
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

# --- 3. 數據預處理器 ---
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

# --- 4. 基礎資訊與截圖 ---
get_screenshot_and_basic_info <- function(stock_code) {
  req(stock_code)
  stock_code <- toupper(stock_code)
  url <- paste0("https://finance.yahoo.com/quote/", stock_code)
  file_name <- paste0(stock_code, ".png")
  file_path <- file.path(getwd(), "www", file_name)
  
  # 建立 Session 並開啟 Log 紀錄
  cat(paste0("\n[", Sys.time(), "] 🚀 啟動 Chromote Session 對應代碼: ", stock_code, "\n"))
  b <- ChromoteSession$new()
  
  tryCatch({
    # 1. 導航 ---------------------------------------------------
    cat(paste0("[", Sys.time(), "] 🌐 正在導航至: ", url, "\n"))
    b$Page$navigate(url)
    b$Page$loadEventFired()
    
    # 2. 等待渲染 -----------------------------------------------
    cat(paste0("[", Sys.time(), "] ⏳ 等待 JavaScript 渲染 (5秒)...\n"))
    Sys.sleep(5) 
    
    # 3. 📸 執行截圖 (關鍵步驟) ----------------------------------
    cat(paste0("[", Sys.time(), "] 📸 嘗試執行螢幕截圖: ", file_path, "\n"))
    b$screenshot(filename = file_path)
    
    # 檢查截圖檔案是否真的產生
    if (file.exists(file_path)) {
      cat(paste0("✅ [成功] 截圖已儲存 (大小: ", file.info(file_path)$size, " bytes)\n"))
    } else {
      cat("❌ [失敗] 截圖指令已發送但檔案未產出，請檢查 www 資料夾權限。\n")
    }
    
    # 4. 🕵️ 抓取數據 (在截圖後獲取當前穩定狀態) -------------------
    cat(paste0("[", Sys.time(), "] 📊 正在從 DOM 提取數據標籤...\n"))
    extracted_data <- b$Runtime$evaluate('
      (function() {
        const getText = (selector) => {
          const el = document.querySelector(selector);
          return el ? el.innerText.trim() : "N/A";
        };
        return {
          price: getText(\'fin-streamer[data-field="regularMarketPrice"]\'),
          market_cap: getText(\'td[data-test="MARKET_CAP-value"]\'),
          eps: getText(\'td[data-test="EPS_RATIO-value"]\')
        };
      })()
    ')$result$value
    
    cat(paste0("📌 抓取結果 -> Price: ", extracted_data$price, 
               " | Market Cap: ", extracted_data$market_cap, 
               " | EPS: ", extracted_data$eps, "\n"))
    
    b$close()
    cat(paste0("[", Sys.time(), "] 🏁 Chromote 任務正常結束。\n\n"))
    
    # 加入路徑資訊回傳給 UI
    extracted_data$png_path <- file_name
    return(extracted_data)
    
  }, error = function(e) {
    if(!is.null(b)) b$close()
    cat(paste0("🚨 [嚴重錯誤] 抓取程序中斷: ", e$message, "\n"))
    return(list(price="N/A", market_cap="N/A", eps="N/A", png_path=NULL))
  })
}
