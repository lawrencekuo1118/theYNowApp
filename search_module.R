# Yahoo 產業資訊抓取函數
# 📦 套件
library(httr)
library(rvest)
library(jsonlite)

# ✅ 改為只保留 API 版 Yahoo Finance 資料擷取
get_yahoo_industry <- function(stock_code) {
  url <- paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/", stock_code, "?modules=assetProfile")
  
  page <- tryCatch(
    {
      httr::GET(url, httr::user_agent("Mozilla/5.0")) |> 
        httr::content(as = "text", encoding = "UTF-8") |> 
        jsonlite::fromJSON()
    },
    error = function(e) {
      return(NA)
    }
  )
  
  if (is.na(page)) return("⚠️ 無法連線到 Yahoo Finance API")
  
  profile <- page$quoteSummary$result[[1]]$assetProfile
  
  if (!is.null(profile)) {
    sector <- profile$sector
    industry <- profile$industry
    return(paste0("🏢 所屬產業：", industry, "\n🏭 所屬類別：", sector))
  } else {
    return("⚠️ 找不到產業資料，可能代碼錯誤或沒有資料")
  }
}

# 🔍 Module Server：搜尋並顯示產業資料與歷史搜尋
search_module_server <- function(id, get_yahoo_industry, corp_name) {
  
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(recentsearch = NULL)
    
    # 點擊搜尋時執行
    search_result <- eventReactive(input$search, {
      req(input$sc)
      get_yahoo_industry(input$sc)
    })
    
    output$search_results <- renderText({
      search_result()
    })
    
    observeEvent(input$search, {
      name <- tryCatch(corp_name(), error = function(e) NULL)
      if (!is.null(name)) {
        values$recentsearch <- c(values$recentsearch, name)
        output$recentsearch <- renderText({
          paste(values$recentsearch, collapse = ", ")
        })
      }
    })
  })
}
