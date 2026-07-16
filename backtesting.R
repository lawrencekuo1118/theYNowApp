# 安裝所需套件 (若未安裝請先執行)
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "ggplot2", "dplyr"))

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)

# ==========================================
# 1. UI 介面 (User Interface)
# ==========================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "量化估值與預測模型 (Mode A/B)"),
  
  # --- 左側參數設定欄 (Sidebar) ---
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("儀表板與預測", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("回測績效分析", tabName = "backtest", icon = icon("chart-line"))
    ),
    
    hr(),
    h4("標的與回測區間", style = "margin-left: 15px; color: #fff;"),
    textInput("ticker", "輸入美股代號 (Ticker):", value = "BRK-B"),
    dateRangeInput("date_range", "回測時間區間:", start = "2020-01-01", end = Sys.Date()),
    
    hr(),
    h4("大過濾器門檻 (The Great Filter)", style = "margin-left: 15px; color: #fff;"),
    numericInput("th_net_margin", "淨利率門檻 (%) [區分虧損/獲利]:", value = 0, step = 1),
    numericInput("th_rev_growth", "營收成長門檻 (%) [高成長標準]:", value = 25, step = 1),
    numericInput("th_eps_growth", "EPS 成長門檻 (%) [PEG 標準]:", value = 15, step = 1),
    sliderInput("th_fcf_cv", "FCF 變異係數上限 (%) [GIGO過濾]:", min = 5, max = 50, value = 20),
    
    hr(),
    h4("模式 A (情緒增強) 權重設定", style = "margin-left: 15px; color: #fff;"),
    sliderInput("wA_momentum", "短期動能權重 (%)", min=0, max=100, value=40),
    sliderInput("wA_valuation", "估值偏離權重 (%)", min=0, max=100, value=30),
    sliderInput("wA_sentiment", "市場情緒/RSI (%)", min=0, max=100, value=20),
    sliderInput("wA_stability", "財務穩定度 (%)", min=0, max=100, value=10),
    # 提醒：實際運作時後端應將其正規化至 100%
    
    hr(),
    h4("模式 B (純基本面) 權重設定", style = "margin-left: 15px; color: #fff;"),
    sliderInput("wB_valuation", "估值偏離權重 (%)", min=0, max=100, value=70),
    sliderInput("wB_stability", "財務質量權重 (%)", min=0, max=100, value=30),
    
    actionBttn(
      inputId = "run_model",
      label = "執行運算與回測",
      style = "jelly", 
      color = "primary",
      block = TRUE
    )
  ),
  
  # --- 右側主畫面 (Main Body) ---
  dashboardBody(
    tabItems(
      # 分頁 1: 預測與分流結果
      tabItem(tabName = "dashboard",
              fluidRow(
                # 顯示標的狀態框
                valueBoxOutput("box_category", width = 4),
                valueBoxOutput("box_valuation_method", width = 4),
                valueBoxOutput("box_gigo_status", width = 4)
              ),
              fluidRow(
                box(title = "明日漲跌預測機率 (Probability)", status = "primary", solidHeader = TRUE, width = 12,
                    # 這裡可以放置 Gauge 或是長條圖
                    plotOutput("prob_plot", height = "250px")
                )
              ),
              fluidRow(
                box(title = "當前因子得分拆解", status = "warning", width = 12,
                    tableOutput("factor_table")
                )
              )
      ),
      
      # 分頁 2: 回測績效比較
      tabItem(tabName = "backtest",
              fluidRow(
                box(title = "累積報酬率對比 (Mode A vs Mode B vs Benchmark)", status = "success", solidHeader = TRUE, width = 12,
                    plotOutput("cum_return_plot", height = "400px")
                )
              ),
              fluidRow(
                box(title = "回測績效指標 (KPIs)", status = "info", width = 12,
                    tableOutput("kpi_table")
                )
              )
      )
    )
  )
)

# ==========================================
# 2. Server 後端邏輯 (Server)
# ==========================================
server <- function(input, output, session) {
  
  # 監聽「執行」按鈕，觸發資料處理 (此處使用假資料展示邏輯)
  observeEvent(input$run_model, {
    
    # 模擬 1: 判定大過濾器路徑 (The Great Filter Logic)
    # 實際應用中，這裡會透過 API 抓取 input$ticker 的財報數據與 input 進行比對
    mock_net_margin <- 18  # 假設是 BRK-B
    mock_eps_growth <- 8
    mock_fcf_cv <- 12
    
    category <- ifelse(mock_net_margin > input$th_net_margin, "獲利型", "虧損型")
    
    if (category == "獲利型" & mock_eps_growth >= input$th_eps_growth) {
      val_method <- "PEG 估值"
    } else if (category == "獲利型" & mock_eps_growth < input$th_eps_growth) {
      val_method <- "P/E 估值 (成熟期)"
    } else {
      val_method <- "EV/Sales (高成長)"
    }
    
    gigo_status <- ifelse(mock_fcf_cv <= input$th_fcf_cv, "通過 (Pass)", "未通過 (High Volatility)")
    
    # --- 更新 Info Boxes ---
    output$box_category <- renderValueBox({
      valueBox(category, "標的屬性判定", icon = icon("tag"), color = "purple")
    })
    output$box_valuation_method <- renderValueBox({
      valueBox(val_method, "採用估值路徑", icon = icon("route"), color = "aqua")
    })
    output$box_gigo_status <- renderValueBox({
      color_set <- ifelse(gigo_status == "通過 (Pass)", "green", "red")
      valueBox(gigo_status, "FCF 穩定性檢驗", icon = icon("shield-alt"), color = color_set)
    })
    
    # --- 模擬機率長條圖 ---
    output$prob_plot <- renderPlot({
      # 權重正規化與機率模擬計算
      total_wA <- input$wA_momentum + input$wA_valuation + input$wA_sentiment + input$wA_stability
      total_wB <- input$wB_valuation + input$wB_stability
      
      # 假設模擬算出的最終機率 (實際需依賴資料矩陣運算)
      prob_A <- 0.58 
      prob_B <- 0.83
      
      df <- data.frame(
        Mode = c("Mode A (情緒增強)", "Mode B (純基本面)"),
        Probability = c(prob_A, prob_B)
      )
      
      ggplot(df, aes(x = Probability, y = Mode, fill = Mode)) +
        geom_col(width = 0.5) +
        geom_text(aes(label = scales::percent(Probability)), hjust = -0.2, size = 6) +
        scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
        scale_fill_manual(values = c("#FF9999", "#99CCFF")) +
        theme_minimal() +
        theme(legend.position = "none", text = element_text(size = 16)) +
        labs(x = "預測上漲機率", y = "")
    })
    
    # --- 模擬績效表 (KPI) ---
    output$kpi_table <- renderTable({
      data.frame(
        指標 = c("年化報酬率 (CAGR)", "最大回撤 (MDD)", "Sharpe Ratio", "勝率"),
        Mode_A = c("15.2%", "-25.4%", "0.85", "58%"),
        Mode_B = c("11.8%", "-14.2%", "0.92", "55%"),
        Benchmark = c("10.5%", "-19.5%", "0.65", "52%")
      )
    })
  })
}

shinyApp(ui, server)

