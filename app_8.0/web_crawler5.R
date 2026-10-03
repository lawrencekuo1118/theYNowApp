# ==========================================
# search_module_4.0.R - 資料抓取與產業資訊爬蟲工具
# ==========================================

library(chromote)
library(rvest)
library(magrittr)
library(purrr)
library(reticulate) 
library(memoise)
library(cachem)

# ==========================================
# 🚀 1. 建立記憶體快取與 Python 爬蟲初始化
# ==========================================
# 建立一個記憶體快取空間 (最多存 50 MB，過期時間設為 1 小時)
my_cache <- cachem::cache_mem(max_size = 50 * 1024^2, max_age = 3600)

if (!py_available(initialize = TRUE)) {
  # 如果 Python 還沒準備好，嘗試手動初始化
  reticulate::py_config()
}

# 在 App 啟動時預先載入 Python 腳本 (只執行一次)
tryCatch({
  reticulate::source_python("deep_scraper.py")
  message("✅ Python 深度爬蟲腳本載入成功！")
}, error = function(e) {
  message("⚠️ Python 腳本載入失敗: ", e$message)
})

# ✨ 將 Python 爬蟲函數包裝成「具備快取能力」的版本
cached_scrape_financials <- memoise::memoise(
  function(stock_code) {
    message(paste("🚀 正在啟動 Python 深度爬蟲 (首次抓取):", stock_code))
    # 這裡呼叫 Python 函數
    return(scrape_all_financials(stock_code)) 
  }, 
  cache = my_cache
)

# ==========================================
# 🏭 2. 爬取公司基本與產業資訊 (Industry)
# ==========================================
get_yahoo_industry <- function(stock_code) {
  message(paste("🔍 正在透過極速 API 抓取公司與產業資訊:", stock_code))
  
  result <- tryCatch({
    # 呼叫我們剛剛在 Python 寫的函數
    info <- fast_get_company_info(stock_code)
    
    sector <- info$sector
    industry <- info$industry
    company_name <- info$company_name
    
    display_text <- paste0("Sector: ", sector, " | Industry: ", industry)
    
    list(
      company_name = company_name,
      sector = sector,
      industry = industry,
      display_text = display_text
    )
    
  }, error = function(e) {
    message("⚠️ 產業資訊抓取失敗: ", e$message)
    return(list(
      company_name = stock_code,
      sector = "N/A",
      industry = "N/A",
      display_text = "Sector: N/A | Industry: N/A"
    ))
  })
  
  return(result)
}

# ==========================================
# 🌐 3. 儀表板摘要資料爬蟲 (Summary)
# ==========================================
get_summary_data <- function(stock_code) {
  message(paste("🌐 正在讀取 Summary 頁面:", stock_code))
  url <- paste0("https://finance.yahoo.com/quote/", stock_code)
  
  # 檢查並重置 Chromote 狀態
  tryCatch({
    if (!is.null(chromote::default_chromote_object())) {
      chromote::default_chromote_object()$check_active()
    }
  }, error = function(e) {
    message("⚠️ Chromote 核心失效，正在重置連線...")
    chromote::set_default_chromote_object(NULL)
  })
  
  b <- NULL
  parsed_df <- tryCatch({
    b <- chromote::ChromoteSession$new()
    b$Page$navigate(url)
    b$Page$loadEventFired() 
    Sys.sleep(1) # 等待 JavaScript 渲染
    
    html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html_content)
    
    # 直接透過 Python API 精準取得公司名稱，無視 HTML 改版！
    api_info <- tryCatch({ fast_get_company_info(stock_code) }, error=function(e) NULL)
    
    if (!is.null(api_info) && !is.na(api_info$company_name)) {
      company_name <- api_info$company_name
    } else {
      # 終極防呆：若 API 也失效，再退回從網頁 title 抓取
      raw_title <- page %>% html_node("title") %>% html_text(trim = TRUE)
      company_name <- sub(" Stock Price.*$", "", raw_title)
    }
    
    # 解析表格 (精準鎖定 Summary 區塊)
    list_items <- page %>% html_nodes("div[data-testid='quote-statistics'] ul > li")
    if (length(list_items) == 0) {
      list_items <- page %>% html_nodes("ul > li")
    }
    
    labels <- list_items %>% html_node("span:first-child") %>% html_text(trim = TRUE)
    values <- list_items %>% html_node("span:last-child") %>% html_text(trim = TRUE)
    
    parsed_df_temp <- data.frame(Item = labels, Value = values, stringsAsFactors = FALSE)
    parsed_df_temp <- parsed_df_temp[!is.na(parsed_df_temp$Item) & parsed_df_temp$Item != "", ]
    
    # 只留下核心指標 (白名單過濾)
    target_keys <- c("Previous Close", "Open", "Bid", "Ask", "Day's Range", 
                     "52 Week Range", "Volume", "Avg. Volume", "Market Cap", 
                     "Beta", "PE Ratio", "EPS", "Earnings Date", 
                     "Dividend", "Yield", "Target Est")
    
    parsed_df_temp <- parsed_df_temp[grepl(paste(target_keys, collapse = "|"), parsed_df_temp$Item, ignore.case = TRUE), ]
    parsed_df_temp <- parsed_df_temp[!duplicated(parsed_df_temp$Item), ]
    
    # 將公司名稱綁定在 dataframe 的隱藏屬性上
    attr(parsed_df_temp, "company_name") <- company_name
    
    parsed_df_temp # 回傳
  }, error = function(e) {
    message("❌ Summary 抓取失敗: ", e$message)
    return(NULL)
  }, finally = {
    # 改用 try() 包覆安全關閉
    if (!is.null(b)) try(b$close(), silent = TRUE)
  })
  
  return(parsed_df)
}

# ==========================================
# 🇺🇸 4. 自動抓取美國 10 年期公債殖利率 (無風險利率 Rf)
# ==========================================
get_risk_free_rate <- function() {
  message("🔍 正在抓取最新美國 10 年期公債殖利率 (^TNX)...")
  
  tryCatch({
    if (!is.null(chromote::default_chromote_object())) {
      chromote::default_chromote_object()$check_active()
    }
  }, error = function(e) {
    chromote::set_default_chromote_object(NULL)
  })
  
  b <- NULL
  tryCatch({
    b <- chromote::ChromoteSession$new()
    b$Page$navigate("https://finance.yahoo.com/quote/^TNX")
    b$Page$loadEventFired() 
    Sys.sleep(3) 
    
    html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- rvest::read_html(html_content)
    
    rf_text <- page %>% 
      rvest::html_node("span[data-testid='qsp-price']") %>% 
      rvest::html_text(trim = TRUE)
    
    rf_value <- as.numeric(rf_text)
    
    if (!is.na(rf_value) && rf_value > 0) {
      message(paste("✅ 成功抓取最新 Rf:", rf_value, "%"))
      return(rf_value)
    } else {
      rf_text_alt <- page %>% 
        rvest::html_node("fin-streamer[data-field='regularMarketPrice']") %>% 
        rvest::html_text(trim = TRUE)
      rf_value_alt <- as.numeric(rf_text_alt)
      
      if (!is.na(rf_value_alt)) return(rf_value_alt)
      stop("抓取數值為空")
    }
    
  }, error = function(e) {
    message("⚠️ Rf 抓取失敗，套用預設值 4.2%。原因: ", e$message)
    return(4.0)
  }, finally = {
    # 改用 try() 包覆安全關閉
    if (!is.null(b)) try(b$close(), silent = TRUE)
  })
}
