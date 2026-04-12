library(shiny)
library(shinydashboard)
source("fcf_module.R")  # 包含重構後模組

# 📊 模擬三表資料 -------------------------------------------------------
d_income_statement <- data.frame(
  metric = c("Total Revenue", "Operating Income", "Income Tax Expense", "Earnings Before Tax"),
  year_1 = c(1000, 150, 30, 120),
  year_2 = c(900, 135, 27, 108)
)

d_cash_flow <- data.frame(
  metric = c("Depreciation & Amortization", "Capital Expenditure", "Free Cash Flow"),
  year_1 = c(50, -70, 80),
  year_2 = c(45, -60, 60),
  year_3 = c(40, -55, 55)
)

d_balance_sheet <- data.frame(
  metric = c("Total Current Assets", "Cash And Cash Equivalents", "Total Current Liabilities", "Short Term Debt"),
  year_1 = c(500, 100, 300, 50),
  year_2 = c(450, 80, 280, 40)
)

# 🧠 reactiveVal: g 成長率容器
estimated_g <- reactiveVal(NULL)

# 🖥️ UI ---------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "FCF 預測與 g 成長率估算"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("FCF 模型", tabName = "fcf"),
      menuItem("估算 g 成長率", tabName = "growth")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("fcf",
              fcf_estimation_module_ui("fcf1")
      ),
      tabItem("growth",
              fluidRow(
                box(title = "📈 成長率估算設定", width = 6, status = "primary", solidHeader = TRUE,
                    radioButtons("g_growth_method", "選擇估算方式",
                                 choices = c("平均年增率" = "mean",
                                             "中位數年增率" = "median",
                                             "最近一年變化率" = "last_year",
                                             "自訂輸入" = "custom"),
                                 inline = TRUE),
                    conditionalPanel(
                      condition = "input.g_growth_method == 'custom'",
                      numericInput("custom_g", "自訂 g 成長率 (%)", value = 5)
                    ),
                    actionButton("calc_growth", "估算 g 成長率", icon = icon("chart-line"))
                ),
                box(title = "📉 結果", width = 6, status = "info", solidHeader = TRUE,
                    textOutput("g_result"),
                    infoBoxOutput("ibx_estimated_g")
                )
              )
      )
    )
  )
)

# 🧠 Server -------------------------------------------------------------
server <- function(input, output, session) {
  # 🔁 成長率估算邏輯
  observeEvent(input$calc_growth, {
    req(d_cash_flow)
    
    fcf_vec <- select_clean_metric_row(d_cash_flow, "Free Cash Flow")
    fcf_vec <- na.omit(fcf_vec)
    
    if (length(fcf_vec) < 2) {
      showNotification("⚠️ 無足夠自由現金流資料來估算成長率", type = "error")
      estimated_g(NULL)
      return()
    }
    
    g_rate <- diff(log(fcf_vec))  # log 成長率
    method <- input$g_growth_method
    val <- switch(method,
                  "mean" = round(mean(g_rate, na.rm = TRUE) * 100, 2),
                  "median" = round(median(g_rate, na.rm = TRUE) * 100, 2),
                  "last_year" = round((tail(fcf_vec, 1) / tail(fcf_vec, 2)[1] - 1) * 100, 2),
                  "custom" = input$custom_g
    )
    
    if (is.null(val) || is.na(val)) {
      showNotification("⚠️ 無法估算成長率", type = "error")
      estimated_g(NULL)
      return()
    }
    
    estimated_g(val)  # 更新 g
    
    output$g_result <- renderText({
      glue::glue("📈 成長率估算結果：{val} % （方法：{switch(method,
                                                        'mean' = '平均年增率',
                                                        'median' = '中位數年增率',
                                                        'last_year' = '最近一年變化率',
                                                        'custom' = '自訂輸入')}）")
    })
    
    output$ibx_estimated_g <- renderInfoBox({
      infoBox("估算 g 成長率", paste0(val, " %"),
              icon = icon("chart-line"),
              color = "purple", fill = TRUE)
    })
  })
  
  # 🧩 套用模組（FCF 預測）
  fcf_estimation_module_server(
    id = "fcf1",
    d_income_statement = d_income_statement,
    d_cash_flow = d_cash_flow,
    d_balance_sheet = d_balance_sheet,
    estimated_g = estimated_g
  )
}

# 🚀 啟動
shinyApp(ui, server)
