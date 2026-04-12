library(shiny)
library(dplyr)
library(ggplot2)

# --- 成長率估算函數 ---
estimate_revenue_growth <- function(df_is, n = 5) {
  revenue <- try(select_clean_metric_row(df_is, "Total Revenue"), silent = TRUE)
  if (inherits(revenue, "try-error") || is.null(revenue)) return(NULL)
  if (length(revenue) < 2) return(NULL)
  
  # 使用對數線性回歸估算 CAGR（複合年成長率）
  years <- seq_along(revenue)
  df <- data.frame(year = years, revenue = revenue)
  model <- lm(log(revenue) ~ year, data = df)
  annual_growth_rate <- exp(coef(model)["year"]) - 1
  round(annual_growth_rate * 100, 2)
}

# --- UI 模組 ---
growth_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("成長率估算"),
    selectInput(ns("growth_method"), "估算方式", choices = c(
      "mean" = "平均增長率",
      "median" = "中位數增長率",
      "last_year" = "最近一年變化率",
      "industry_average" = "產業平均",
      "custom" = "自訂輸入"
    )),
    
    # ✅ 移除選擇產業的欄位，改由外部 app 傳入
    conditionalPanel(
      condition = sprintf("input['%s'] == 'custom'", ns("growth_method")),
      numericInput(ns("custom_g"), "自訂年增率 (%)", value = 5)
    ),
    
    actionButton(ns("calc_growth"), "估算成長率"),
    textOutput(ns("g_result")),
    infoBoxOutput(ns("ibx_estimated_g"))
  )
}


# --- Server 模組 ---
growth_module_server <- function(id, d_income_statement, selected_industry) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    estimated_g <- reactiveVal(NULL)
    source_txt <- reactiveVal(NULL)
    
    observeEvent(input$calc_growth, {
      cat("🔁 成長率估算啟動\n")
      method <- input$growth_method
      cat("📌 選擇估算方法：", method, "\n")
      
      val <- NULL
      
      if (method %in% c("mean", "median", "last_year")) {
        cat("📥 嘗試讀取財報...\n")
        
        df <- d_income_statement
        if (is.null(df)) {
          cat("❌ d_income_statement傳回 NULL\n")
          showNotification("⚠️ 財報資料為空", type = "error")
          estimated_g(NULL)
          return()
        }
        
        cat("✅ 財報欄位：", paste(colnames(df), collapse = ", "), "\n")
        cat("📊 Breakdown 欄：", paste(head(df$Breakdown, 3), collapse = ", "), "...\n")
        
        revenue_vec <- try(select_clean_metric_row(df, "Total Revenue"), silent = TRUE)
        
        if (inherits(revenue_vec, "try-error") || is.null(revenue_vec)) {
          cat("❌ select_clean_metric_row() 錯誤或回傳 NULL\n")
          showNotification("⚠️ 無法擷取 Revenue 資料", type = "error")
          estimated_g(NULL)
          return()
        }
        
        cat("📈 擷取 Revenue 向量：", paste(revenue_vec, collapse = ", "), "\n")
        
        revenue_vec <- na.omit(revenue_vec)
        cat("📉 去除 NA 後的 Revenue 向量：", paste(revenue_vec, collapse = ", "), "\n")
        
        if (length(revenue_vec) < 2) {
          cat("❗ Revenue 向量長度不足：", length(revenue_vec), "\n")
          showNotification("⚠️ 營收資料不足", type = "error")
          estimated_g(NULL)
          return()
        }
        
        g_rate <- diff(log(revenue_vec))
        cat("📈 Log 成長率向量：", paste(round(g_rate * 100, 2), collapse = ", "), "\n")
        
        val <- switch(method,
                      "mean" = round(mean(g_rate) * 100, 2),
                      "median" = round(median(g_rate) * 100, 2),
                      "last_year" = round((tail(revenue_vec, 1) / tail(revenue_vec, 2)[1] - 1) * 100, 2)
        )
        
        cat("✅ 成長率估算結果：", val, "\n")
        source_txt("歷史財報")
      }
      
      else if (method == "industry_average") {
        cat("📦 使用產業平均估算...\n")
        industry <- selected_industry
        cat("🔎 選擇產業：", industry, "\n")
        
        if (!is.null(industry) && !is.null(industry_standards[[industry]]$rev_growth)) {
          growth_range <- industry_standards[[industry]]$rev_growth
          cat("📊 該產業成長區間：", paste(growth_range, collapse = " ~ "), "\n")
          val <- round(mean(growth_range), 2)
          cat("✅ 平均值：", val, "\n")
          source_txt(glue::glue("產業平均（{industry}）"))
        } else {
          cat("❌ 無法從 industry_standards 取得資料\n")
          showNotification("⚠️ 無法取得產業平均", type = "error")
          estimated_g(NULL)
          return()
        }
      }
      
      else if (method == "custom") {
        val <- input$custom_g
        cat("🛠 自訂輸入成長率：", val, "\n")
        source_txt("自訂輸入")
      }
      
      # 最終檢查估算值是否有效
      if (is.null(val) || is.na(val)) {
        cat("❗ 無效的 g 成長率估算結果：NULL 或 NA\n")
        showNotification("⚠️ 無法估算成長率", type = "error")
        estimated_g(NULL)
        return()
      }
      
      estimated_g(val)
      cat("📥 成功寫入 estimated_g：", val, "\n")
      
      output$g_result <- renderText({
        glue::glue("📈 成長率估算結果：{val} %")
      })
      
      output$ibx_estimated_g <- renderInfoBox({
        infoBox("估算 g", paste0(val, " %"), icon = icon("chart-line"), color = "purple", fill = TRUE)
      })
    })
    
    return(list(g = estimated_g, source_txt = source_txt))
  })
}
