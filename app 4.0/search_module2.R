# ==========================================
# search_module2.R - 搜尋與產業資訊模組 (純 Chrome 爬蟲版)
# ==========================================

# 📦 套件
library(chromote)
library(rvest)

# 🚀 透過 Chrome 模擬瀏覽器抓取公司全名與產業資訊
get_yahoo_industry <- function(stock_code) {
  message(paste("🔍 正在透過模擬瀏覽器抓取公司與產業資訊:", stock_code))
  
  # 啟動背景 Chrome 實例
  b <- chromote::ChromoteSession$new()
  
  # 目標網址：Yahoo Finance 公司簡介頁面
  url <- paste0("https://finance.yahoo.com/quote/", stock_code, "/profile")
  
  result <- tryCatch({
    # 瀏覽器導航至目標 URL
    b$Page$navigate(url)
    b$Page$loadEventFired() # 等待頁面加載完成
    Sys.sleep(2) # 等待 JavaScript 渲染
    
    # 抓取當前渲染完成的 HTML
    html_content <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    page <- read_html(html_content)
    
    # 🕵️‍♂️ 1. 精準抓取公司全稱 (根據你提供的 yf-18s5v3y 或類似 class)
    company_name <- page %>% 
      html_node("h1[class*='yf-']") %>% 
      html_text(trim = TRUE)
    
    # 🕵️‍♂️ 2. 精準抓取產業名稱 (Industry)
    # 方法：尋找包含 "Industry" 字樣的 h3 標籤，並抓取它「同層上方」的 p 標籤內容
    industry <- page %>% 
      html_node(xpath = "//h3[text()='Industry']/preceding-sibling::p") %>% 
      html_text(trim = TRUE)
    
    # 如果 Xpath 沒抓到，使用 CSS Selector 作為備案 (針對你提供的 class)
    if (is.na(industry) || industry == "") {
      industry <- page %>% 
        html_node(".infoSection.yf-z5w6qk p") %>% 
        html_text(trim = TRUE)
    }
    
    # 🛡️ 3. 防呆：如果還是空值，給予預設值
    if (is.na(industry) || industry == "") industry <- "Unknown Industry"
    
    # 組合顯示內容
    display_text <- paste0("公司：", company_name, " | 產業：", industry)
    
    # 回傳 List (display_text 供顯示；industry 供後續財務標準對照)
    list(
      display_text = display_text,
      company_name = company_name,
      industry = industry,
      sector = sector
    )
    
  }, error = function(e) {
    list(display_text = paste("⚠️ 模擬瀏覽器連線失敗:", e$message), industry = NA)
  }, finally = {
    # 務必關閉分頁，釋放記憶體
    b$close()
  })
  
  return(result)
}

# 🔍 Module Server：搜尋並顯示產業資料與歷史搜尋
search_module_server <- function(id, get_yahoo_industry) {
  moduleServer(id, function(input, output, session) {
    
    search_result <- eventReactive(input$search, {
      # ⚠️ 這裡要確認是 input$sc 還是跟隨 UI 的變數名
      req(input$sc) 
      
      withProgress(message = '正在讀取公司與產業資料...', value = 0.5, {
        info <- get_yahoo_industry(input$sc)
        return(info) # 回傳整個 list
      })
    })
    
    # 在 UI 顯示
    output$search_results <- renderText({
      req(search_result())
      search_result()$display_text
    })
  })
}
