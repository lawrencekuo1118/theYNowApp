# ✅ 同步 API 與 錯誤檢查：整合產業資訊與公司全稱
get_yahoo_summary_data <- function(stock_code) {
  if (is.null(stock_code) || stock_code == "") return(NULL)
  
  # 擴展 modules，同時要求 assetProfile (產業) 與 quoteType (全稱)
  url <- paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/", 
                toupper(stock_code), "?modules=assetProfile,quoteType")
  
  ua <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
  
  tryCatch({
    res <- httr::GET(url, ua, timeout(5))
    if (httr::status_code(res) == 200) {
      page <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      result <- page$quoteSummary$result
      
      if (!is.null(result) && length(result) > 0) {
        data <- result[[1]]
        
        # 提取資訊
        long_name <- data$quoteType$longName %||% toupper(stock_code)
        industry  <- data$assetProfile$industry %||% "未知"
        sector    <- data$assetProfile$sector %||% "未知"
        
        return(list(
          longName = long_name,
          display_text = paste0("🏢 所屬產業：", industry, "\n🏭 所屬類別：", sector),
          industry = industry
        ))
      }
    }
  }, error = function(e) message("Summary API Error: ", e$message))
  
  # 失敗時的回傳備案
  return(list(
    longName = toupper(stock_code),
    display_text = paste0("🏢 產業資訊：(暫無資料)\n🔎 代碼：", toupper(stock_code)),
    industry = "未知"
  ))
}

# 🔍 Module Server：搜尋並同步顯示產業資料與歷史搜尋
search_module_server <- function(id, corp_name_reactive) {
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(recentsearch = character(0))
    
    # 當按下 Search 按鈕時執行
    search_data <- eventReactive(input$search, {
      req(input$sc)
      get_yahoo_summary_data(input$sc)
    })
    
    # 輸出產業文字
    output$search_results <- renderText({
      search_data()$display_text
    })
    
    # 監聽搜尋動作，更新歷史紀錄
    observeEvent(input$search, {
      data <- search_data()
      name <- data$longName
      
      if (!is.null(name) && name != "" && !(name %in% values$recentsearch)) {
        # 將最新搜尋放在最前面
        values$recentsearch <- c(name, values$recentsearch)
        
        # 限制顯示數量為 5 筆
        if(length(values$recentsearch) > 5) {
          values$recentsearch <- values$recentsearch[1:5]
        }
        
        # 更新歷史搜尋文字
        output$recentsearch <- renderText({
          paste(values$recentsearch, collapse = " | ")
        })
      }
    })
    
    # 如果外部需要這個 longName，可以透過 return 傳出去
    return(reactive({ search_data()$longName }))
  })
}
