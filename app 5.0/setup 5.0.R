# ==========================================
# setup2.R - 資料抓取與解析模組
# ==========================================

# 載入必要套件
library(chromote)
library(rvest)
library(magrittr)
library(purrr)

# 🔍 抓取並解析 Summary 表格 (setup2.R)
get_summary_data <- function(stock_code) {
  message(paste("🌐 正在讀取 Summary 頁面:", stock_code))
  url <- paste0("https://finance.yahoo.com/quote/", stock_code)
  
  # --- 修正開始：檢查並重置 Chromote 狀態 ---
  tryCatch({
    if (!is.null(chromote::default_chromote_object())) {
      chromote::default_chromote_object()$check_active()
    }
  }, error = function(e) {
    message("⚠️ Chromote 核心失效，正在重置連線...")
    chromote::set_default_chromote_object(NULL)
  })
  # ---------------------------------------
  
  b <- NULL
  df <- tryCatch({
    b <- chromote::ChromoteSession$new() # 現在有 30 秒緩衝了
    # ------------------------------------------------
    b$Page$navigate(url)
    b$Page$loadEventFired() 
    Sys.sleep(2) # 等待 JavaScript 渲染
    
    html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html_content)
    
    # 1. 精準鎖定帶有 yf- class 的 h1 標籤
    company_name <- page %>% 
      html_node("h1[class*='yf-']") %>%   # 鎖定 class 包含 'yf-' 的 h1 標籤
      html_text(trim = TRUE)
    
    # 2. 防呆機制：如果還是沒抓到，改從網頁標題 <title> 提取
    if (is.na(company_name) || company_name == "") {
      raw_title <- page %>% html_node("title") %>% html_text(trim = TRUE)
      company_name <- sub(" Stock Price.*$", "", raw_title) # 切除後方多餘文字
    }
    
    # 解析表格
    list_items <- page %>% html_nodes("ul > li")
    labels <- list_items %>% html_node("span:first-child") %>% html_text(trim = TRUE)
    values <- list_items %>% html_node("span:last-child") %>% html_text(trim = TRUE)
    
    parsed_df <- data.frame(Item = labels, Value = values, stringsAsFactors = FALSE)
    parsed_df <- parsed_df[!is.na(parsed_df$Item) & parsed_df$Item != "", ]
    
    # 3. 將公司名稱綁定在 dataframe 的隱藏屬性上，一起回傳
    attr(parsed_df, "company_name") <- company_name
    
    parsed_df
  }, error = function(e) {
    message("❌ Summary 抓取失敗: ", e$message)
    return(NULL)
  }, finally = {
    if (!is.null(b)) b$close()
  })
  
  return(df)
}

# 核心抓取函數 (透過背景 Chrome 抓取 JavaScript 渲染後的網頁)
get.data <- function(stock_code) {
  message(paste("🔍 正在透過模擬瀏覽器抓取股票代碼:", stock_code, "..."))
  
  base_url <- paste0("https://finance.yahoo.com/quote/", stock_code)
  targets <- list(
    income_statement = paste0(base_url, "/financials"),
    balance_sheet    = paste0(base_url, "/balance-sheet"),
    cash_flow        = paste0(base_url, "/cash-flow")
  )
  
  results <- list()
  b <- NULL # 預先宣告，防止 finally 找不到變數
  
  tryCatch({
    # 啟動 Chrome Session
    # ---------------- 加入這段保險機制 ----------------
    tryCatch({
      # 檢查預設的 chromote 背景核心是否還活著，若當機則強制重置
      if (!is.null(chromote::default_chromote_object())) {
        chromote::default_chromote_object()$check_active()
      }
    }, error = function(e) {
      message("⚠️ 偵測到 Chromote 核心失去回應，正在重置...")
      chromote::set_default_chromote_object(NULL)
    })
    
    # 啟動 Chrome Session
    b <- chromote::ChromoteSession$new()
    # ------------------------------------------------
    
    for (name in names(targets)) {
      message(paste("🌐 正在讀取:", name))
      
      # 將 tryCatch 移入迴圈內：一個分頁失敗，不影響其他分頁
      results[[name]] <- tryCatch({
        b$Page$navigate(targets[[name]])
        b$Page$loadEventFired() # 等待基本 HTML 載入
        
        # 💡 智慧等待：尋找財報通用關鍵字
        max_retries <- 10
        for (i in 1:max_retries) {
          js_check <- "document.body.innerText.includes('Total Revenue') || document.body.innerText.includes('Total Assets') || document.body.innerText.includes('Operating Cash Flow') || document.body.innerText.includes('ttm')"
          
          is_ready <- tryCatch(
            b$Runtime$evaluate(js_check)$result$value,
            error = function(e) FALSE
          )
          
          if (isTRUE(is_ready)) break # 渲染完成，提早跳出等待
          Sys.sleep(0.5)
        }
        
        # 抓取最終的 HTML
        html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
        read_html(html_content)
        
      }, error = function(e) {
        message(paste("⚠️", name, "分頁抓取失敗:", e$message))
        return(NULL)
      })
    }
    
  }, error = function(e) {
    message("❌ 模擬瀏覽器啟動或執行過程發生重大錯誤: ", e$message)
  }, finally = {
    # 確保釋放記憶體
    if (!is.null(b) && inherits(b, "ChromoteSession")) {
      b$close()
      message("🧹 已關閉 Chrome 背景分頁")
    }
  })
  
  return(results)
}

# Yahoo Finance 表格解析函數 (適應最新 DIV 排版，並修復重複解析問題)
extract_yf_financial_table <- function(page) {
  # 確保傳入的 page 不是空的
  if (is.null(page)) return(NULL)
  
  # 【修正 1】直接使用傳入的 page，不要再呼叫 rvest::read_html()！
  
  # 【修正 2】精準鎖定新版 Yahoo Finance 的「表頭列」與「資料列」，解決抓錯列數的問題
  rows <- rvest::html_nodes(page, "div.tableHeader div.row, div.tableBody div.row")
  
  if (length(rows) == 0) return(NULL)
  
  parsed_data <- list()
  for (i in seq_along(rows)) {
    # 精準鎖定新版的欄位 (div.column)
    cells <- rvest::html_nodes(rows[[i]], "div.column")
    vals <- rvest::html_text(cells, trim = TRUE)
    
    # 清除空值
    vals <- vals[vals != ""]
    if (length(vals) > 0) {
      parsed_data[[length(parsed_data) + 1]] <- vals
    }
  }
  
  # 如果過濾後沒有資料，提早跳出
  if (length(parsed_data) == 0) return(NULL)
  
  # 對齊陣列長度，準備轉為 Data Frame
  max_len <- max(sapply(parsed_data, length))
  parsed_data <- lapply(parsed_data, function(x) {
    length(x) <- max_len
    x
  })
  
  # 綁定成表格
  df <- do.call(rbind, parsed_data)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # 將第一列設為欄位名稱
  if(nrow(df) > 1) {
    colnames(df) <- make.unique(as.character(df[1, ])) # 確保欄位名稱不重複
    df <- df[-1, ]
  }
  
  return(df)
}

# 對外呼叫的統一接口
clean_financial_table <- function(html_obj) {
  return(extract_yf_financial_table(html_obj))
}

# 🛠️ 自訂函數：轉換數字為 K / M / B 格式
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
