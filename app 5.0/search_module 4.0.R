# ==========================================
# search_module 4.0.R - 產業資訊爬蟲工具
# ==========================================

library(chromote)
library(rvest)

# 🚀 爬蟲邏輯 (針對新版 Yahoo Finance)
get_yahoo_industry <- function(stock_code) {
  message(paste("🔍 正在透過模擬瀏覽器抓取公司與產業資訊:", stock_code))
  
  b <- chromote::ChromoteSession$new()
  url <- paste0("https://finance.yahoo.com/quote/", stock_code)
  
  result <- tryCatch({
    b$Page$navigate(url)
    b$Page$loadEventFired() 
    Sys.sleep(2) 
    
    html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html_content)
    
    # 精準抓取公司全稱
    company_name <- page %>% 
      html_node("h1[class*='yf-']") %>% 
      html_text(trim = TRUE)
    
    # 🕵️‍♂️ 精準抓取產業 (Industry)
    industry <- page %>% 
      html_node(xpath = "//h3[text()='Industry']/preceding-sibling::p") %>% 
      html_text(trim = TRUE)
    
    # 🕵️‍♂️ 精準抓取領域 (Sector)
    sector <- page %>% 
      html_node(xpath = "//h3[text()='Sector']/preceding-sibling::p") %>% 
      html_text(trim = TRUE)
    
    # 🛡️ 防呆機制
    if (is.na(industry) || industry == "") industry <- "Unknown Industry"
    if (is.na(sector) || sector == "") sector <- "Unknown Sector"
    
    # 組合顯示內容
    display_text <- paste0("產業：", industry, "\n領域：", sector)
    
    list(
      display_text = display_text,
      company_name = company_name,
      industry = industry,
      sector = sector
    )
    
  }, error = function(e) {
    list(display_text = paste("⚠️ 模擬瀏覽器連線失敗:", e$message), industry = NA, company_name = NA, sector = NA)
  }, finally = {
    b$close()
  })
  
  return(result)
}
