# ===================================================================
# The YNow App - 完整三大表與雙搜尋框版 (ShinyApps Demo)
# ===================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(quantmod)

# ===================================================================
# 1. 資料處理與輔助函數 (三表架構)
# ===================================================================
get_real_price <- function(ticker) {
  tryCatch({
    quote <- getQuote(ticker)
    return(as.numeric(quote$Last))
  }, error = function(e) return(NA))
}

# 模擬三大財報數據 (List 結構：Income, Balance, CashFlow)
get_mock_data <- function(ticker) {
  list(
    income = data.frame(
      Metric = c("Total Revenue", "Gross Profit", "Operating Income", "Net Income from Continuing & Discontinued Operation"),
      Y1 = c(150000, 60000, 35000, 25000),
      Y2 = c(140000, 55000, 30000, 22000),
      Y3 = c(130000, 50000, 25000, 20000)
    ),
    balance = data.frame(
      Metric = c("Total Assets", "Total Liabilities", "Common Stock Equity", "Total Debt"),
      Y1 = c(800000, 400000, 400000, 150000),
      Y2 = c(750000, 370000, 380000, 160000),
      Y3 = c(700000, 350000, 350000, 170000)
    ),
    cashflow = data.frame(
      Metric = c("Operating Cash Flow", "Investing Cash Flow", "Financing Cash Flow", "Capital Expenditure", "Free Cash Flow"),
      Y1 = c(35000, -12000, -5000, -8000, 27000),
      Y2 = c(32000, -10000, -4000, -7500, 24500),
      Y3 = c(30000, -9000, -3000, -7000, 23000)
    )
  )
}

format_dollar_abbr <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return("N/A")
  if (abs(x) >= 1e9) paste0("$", round(x / 1e9, 2), "B")
  else if (abs(x) >= 1e6) paste0("$", round(x / 1e6, 2), "M")
  else paste0("$", format(round(x, 2), big.mark = ","))
}

# ===================================================================
# 2. UI 前端介面
# ===================================================================
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "The YNow App", titleWidth = 250),
  
  dashboardSidebar(
    width = 250,
    collapsed = FALSE,
    column(width = 12, sidebarSearchForm(textId = "txt_search", buttonId = "btn_search", label = "Search..."), hr()),
    column(
      width = 12,
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
        menuItem("DDM", tabName = "ddm_calculator", icon = icon("hand-holding-usd"), badgeLabel = "new", badgeColor = "green"),
        menuItem("DCF", tabName = "dcf_calculator", icon = icon("calculator")),
        menuItem("Advance", tabName = "advance", icon = icon("sliders-h"), badgeLabel = "new", badgeColor = "green"),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      ),
      hr()
    ),
    column(width = 12, h5("Recent Search:"), textOutput("recentsearch"), hr()),
    column(width = 12, textOutput("today"))
  ),
  
  dashboardBody(
    withMathJax(),
    
    tabItems(
      # --- Dashboard Tab ---
      tabItem(tabName = "dashboard",
              # 主畫面雙按鈕搜尋框
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE,
                    column(width = 8, textInput("sc", "輸入股票代碼 (e.g. AMZN):", value = "AMZN")),
                    column(width = 4, actionButton("search", "執行分析", icon = icon("search"), class = "btn-primary", style = "margin-top: 25px;"))
                )
              ),
              
              h2(textOutput("dash_title")),
              fluidRow(
                valueBoxOutput("vbx_price", width = 3),
                valueBoxOutput("vbx_gross_profit_margin", width = 3),
                valueBoxOutput("vbx_net_profit_margin", width = 3),
                valueBoxOutput("vbx_roe", width = 3)
              ),
              
              # ✅ 升級：使用 tabBox 呈現完整三大表
              fluidRow(
                tabBox(
                  title = "歷史財務報表 (Demo 數據)",
                  width = 12,
                  id = "tabset_financials",
                  tabPanel("損益表 (Income Statement)", icon = icon("file-invoice-dollar"), tableOutput("tbl_income")),
                  tabPanel("資產負債表 (Balance Sheet)", icon = icon("balance-scale"), tableOutput("tbl_balance")),
                  tabPanel("現金流量表 (Cash Flow)", icon = icon("money-bill-wave"), tableOutput("tbl_cashflow"))
                )
              )
      ),
      
      # --- DDM Tab ---
      tabItem(tabName = "ddm_calculator",
              h2("股利折現模型 (Dividend Discount Model)"),
              fluidRow(
                box(title = "參數設定", status = "warning", width = 4, solidHeader = TRUE,
                    numericInput("ddm_d0", "目前股利 (D0):", 5),
                    numericInput("ddm_g", "永續成長率 g (%):", 2.0),
                    numericInput("ddm_ke", "要求報酬率 Ke (%):", 8.0),
                    actionButton("btn_calc_ddm", "計算合理價", class = "btn-success")),
                box(title = "DDM 估值結果", status = "success", width = 8, solidHeader = TRUE,
                    uiOutput("ui_ddm_result"))
              )
      ),
      
      # --- DCF Tab ---
      tabItem(tabName = "dcf_calculator",
              h2("現金流折現模型 (Discounted Cash Flow Model)"),
              fluidRow(
                box(title = "參數設定 (兩階段模型)", status = "info", width = 4, solidHeader = TRUE,
                    numericInput("dcf_fcf0", "基準年 FCF (百萬):", 27000),
                    numericInput("dcf_g1", "第1~5年 成長率 (%):", 12.0),
                    numericInput("dcf_g2", "永續成長率 TV g (%):", 2.0),
                    numericInput("dcf_wacc", "WACC (%):", 9.0),
                    numericInput("dcf_shares", "流通股數 (百萬股):", 10000),
                    actionButton("btn_calc_dcf", "計算合理價", class = "btn-success")),
                box(title = "DCF 估值結果", status = "success", width = 8, solidHeader = TRUE,
                    uiOutput("ui_dcf_result"))
              )
      ),
      
      # --- Advance Tab ---
      tabItem(tabName = "advance",
              h2("進階設定與財務模型公式"),
              box(title = "模型核心公式參考", status = "primary", width = 12, solidHeader = TRUE,
                  h4(tags$b("加權平均資本成本 (WACC)")),
                  p("$$WACC = (W_e \\times K_e) + (W_d \\times K_d \\times (1 - T))$$"),
                  tags$hr(),
                  h4(tags$b("終值 (Terminal Value, TV)")),
                  p("$$TV = \\frac{FCF_{5} \\times (1 + g)}{WACC - g}$$"),
                  p("企業總價值 (EV) = 1~5年FCF現值總和 + 終值現值")
              )
      ),
      
      # --- About Tab ---
      tabItem(tabName = "about",
              h2("關於 The YNow App"),
              p("展示版本。資料來源使用預設靜態數據，但模型計算邏輯、三大表架構與原版完全一致。")
      )
    )
  )
)

# ===================================================================
# 3. Server 後端邏輯
# ===================================================================
server <- function(input, output, session) {
  
  current_ticker <- reactiveVal("AMZN")
  recent_searches <- reactiveVal(c("AMZN"))
  current_mkt_price <- reactiveVal(NA)
  intrinsic_val_ddm <- reactiveVal(0)
  intrinsic_val_dcf <- reactiveVal(0)
  
  output$today <- renderText({ paste("📅 Today:", Sys.Date()) })
  output$recentsearch <- renderText({ paste(recent_searches(), collapse = ", ") })
  output$dash_title <- renderText({ paste("Dashboard -", current_ticker()) })
  
  # ==========================================
  # 雙按鈕監聽邏輯
  # ==========================================
  update_ticker_logic <- function(sym) {
    current_ticker(sym)
    hist <- recent_searches()
    if (!(sym %in% hist)) recent_searches(c(sym, hist)[1:min(5, length(hist) + 1)])
    current_mkt_price(get_real_price(sym))
    showNotification(paste("已載入", sym, "最新財報與市場資料"), type = "message")
  }
  
  observeEvent(input$btn_search, {
    req(input$txt_search); sym <- toupper(trimws(input$txt_search))
    updateTextInput(session, "sc", value = sym)
    update_ticker_logic(sym)
  })
  observeEvent(input$search, {
    req(input$sc); sym <- toupper(trimws(input$sc))
    updateTextInput(session, "txt_search", value = sym)
    update_ticker_logic(sym)
  })
  
  observe({ current_mkt_price(get_real_price(current_ticker())) })
  
  # ==========================================
  # 📊 渲染三大財報表格
  # ==========================================
  fin_data <- reactive({ get_mock_data(current_ticker()) })
  
  output$tbl_income <- renderTable({ fin_data()$income })
  output$tbl_balance <- renderTable({ fin_data()$balance })
  output$tbl_cashflow <- renderTable({ fin_data()$cashflow })
  
  # ==========================================
  # 📈 渲染 Dashboard KPI (跨表計算邏輯)
  # ==========================================
  output$vbx_price <- renderValueBox({
    price <- current_mkt_price()
    val <- if (is.na(price)) "N/A" else paste0("$", price)
    valueBox(val, "Current Market Price", icon = icon("dollar-sign"), color = "orange")
  })
  
  output$vbx_gross_profit_margin <- renderValueBox({
    df <- fin_data()$income
    gp <- df$Y1[df$Metric == "Gross Profit"]
    rev <- df$Y1[df$Metric == "Total Revenue"]
    margin <- (gp / rev) * 100
    valueBox(paste0(sprintf("%.2f", margin), "%"), "毛利率 Gross Profit Margin", icon = icon("percentage"), color = if(margin>0)"green" else"red")
  })
  
  output$vbx_net_profit_margin <- renderValueBox({
    df <- fin_data()$income
    net <- df$Y1[df$Metric == "Net Income from Continuing & Discontinued Operation"]
    rev <- df$Y1[df$Metric == "Total Revenue"]
    margin <- (net / rev) * 100
    valueBox(paste0(sprintf("%.2f", margin), "%"), "淨利率 Net Profit Margin", icon = icon("percentage"), color = if(margin>0)"green" else"red")
  })
  
  output$vbx_roe <- renderValueBox({
    # 🌟 跨表計算：損益表的「淨利」 / 資產負債表的「股東權益」
    df_inc <- fin_data()$income
    df_bal <- fin_data()$balance
    net <- df_inc$Y1[df_inc$Metric == "Net Income from Continuing & Discontinued Operation"]
    eq <- df_bal$Y1[df_bal$Metric == "Common Stock Equity"]
    roe <- (net / eq) * 100
    valueBox(paste0(sprintf("%.2f", roe), "%"), "股東權益報酬率 ROE", icon = icon("chart-line"), color = if(roe>0)"green" else"red")
  })
  
  # ==========================================
  # 🧮 估值模型計算
  # ==========================================
  # DDM 
  observeEvent(input$btn_calc_ddm, {
    req(input$ddm_d0, input$ddm_g, input$ddm_ke)
    g_dec <- input$ddm_g / 100; ke_dec <- input$ddm_ke / 100
    if (ke_dec <= g_dec) { showNotification("要求報酬率 (Ke) 必須大於成長率 (g)！", type = "error"); return() }
    
    p0 <- (input$ddm_d0 * (1 + g_dec)) / (ke_dec - g_dec)
    intrinsic_val_ddm(round(p0, 2))
  })
  output$ui_ddm_result <- renderUI({
    div(style = "font-size: 32px; font-weight: bold; text-align: center; padding: 20px; background-color: #ECF0F1; border-radius: 10px;",
        p(style = "font-size: 16px; color: #7F8C8D;", "預估合理股價 (DDM)"), paste0("$", intrinsic_val_ddm()))
  })
  
  # DCF 
  observeEvent(input$btn_calc_dcf, {
    req(input$dcf_fcf0, input$dcf_g1, input$dcf_g2, input$dcf_wacc, input$dcf_shares)
    fcf0 <- input$dcf_fcf0; g1 <- input$dcf_g1 / 100; g2 <- input$dcf_g2 / 100; wacc <- input$dcf_wacc / 100; shares <- input$dcf_shares
    if (wacc <= g2) { showNotification("WACC 必須大於永續成長率 (g2)！", type = "error"); return() }
    
    pv_fcf_sum <- 0; fcf_t <- fcf0
    for (t in 1:5) { fcf_t <- fcf_t * (1 + g1); pv_fcf_sum <- pv_fcf_sum + (fcf_t / (1 + wacc)^t) }
    
    tv <- (fcf_t * (1 + g2)) / (wacc - g2); pv_tv <- tv / (1 + wacc)^5
    intrinsic_val_dcf(round((pv_fcf_sum + pv_tv) / shares, 2))
  })
  output$ui_dcf_result <- renderUI({
    div(style = "font-size: 32px; font-weight: bold; text-align: center; padding: 20px; background-color: #E8F8F5; border-radius: 10px;",
        p(style = "font-size: 16px; color: #16A085;", "預估每股合理股價 (DCF)"), paste0("$", intrinsic_val_dcf()))
  })
}

shinyApp(ui, server)
