library(shiny)
library(shinydashboard)

source("global.R")
source("setup2.R")
source("industry_standards.R")
source("kpi_module.R")
source("growth_module.R")
source("fcf_module 2.R")
source("search_module.R")

ui <- dashboardPage(
  dashboardHeader(title = "DCF Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("估值計算器", tabName = "calculator", icon = icon("calculator")),
      selectInput("industry_choice", "選擇產業",
                  choices = names(industry_standards), selected = "Technology")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "calculator",
        tabBox(
          title = "DCF Calculator", width = "auto",
          
          # --- 📊 Stock Valuation Tab ---
          tabPanel("Stock Valuation",
                   fluidRow(
                     column(width = 12, fluidRow(
                       infoBoxOutput("ibx_enterprise_value_dcf"),
                       infoBoxOutput("ibx_stock_value_dcf")
                     )),
                     column(width = 6, htmlOutput("vtxt_dcf_setting_details")),
                     column(width = 6, plotOutput("plt_dft_fcf"))
                   )
          ),
          
          # --- 🧮 DCF Calculator Tab ---
          tabPanel("DCF Calculator",
                   fluidRow(
                     column(width = 4,
                            radioButtons("dcf_mode", "估值模式", choices = c(
                              "永續成長法（Gordon Growth）" = "gordon",
                              "二階段成長法（Two-Stage Growth）" = "two_stage"
                            )),
                            numericInput("years", "預測年數 n", value = 5, min = 1, max = 20),
                            conditionalPanel("input.dcf_mode == 'gordon'", tagList(
                              numericInput("g_gordon", "永續成長率 g (%)", value = 3),
                              numericInput("wacc_gordon", "折現率 WACC (%)", value = 10)
                            )),
                            conditionalPanel("input.dcf_mode == 'two_stage'", tagList(
                              numericInput("yr_stage1", "第一階段預測年數", value = 3, min = 1, max = 19),
                              numericInput("g_stage1", "第一階段成長率 g₁ (%)", value = 5),
                              numericInput("g_stage2", "第二階段成長率 g₂ (%)", value = 3),
                              numericInput("wacc_stage1", "第一階段 WACC₁ (%)", value = 10),
                              numericInput("wacc_stage2", "第二階段 WACC₂ (%)", value = 9)
                            )),
                            checkboxInput("use_calculated_wacc", "使用估算的 WACC 作為折現率", value = TRUE),
                            checkboxInput("apply_custom_to_fcf", "將 g 成長率同步套用至 FCF", value = TRUE),
                            actionButton("calc", "📊 計算DCF")
                     ),
                     column(width = 8,
                            h4("🔎 詳細估值結果"),
                            plotOutput("plt_fcf"),
                            verbatimTextOutput("vtxt_dcf_results")
                     )
                   )
          ),
          
          # --- 📈 g Calculator Tab ---
          tabPanel("g Calculator",
                   fluidRow(
                     column(width = 6, growth_module_ui("growth")),
                     column(width = 6,
                            h4("估算說明"),
                            textOutput("growth_source_txt"),
                            helpText("根據過去 Revenue 或產業平均，自動推估永續成長率 g")
                     )
                   ),
                   fluidRow(
                     column(width = 12,
                            h4("估算結果"),
                            verbatimTextOutput("g_result"),
                            infoBoxOutput("ibx_estimated_g"),
                            tags$hr(),
                            helpText("g 將自動套用至 Gordon 與 Two-Stage 模型的對應欄位")
                     )
                   )
          ),
          
          # --- 🧮 WACC Calculator Tab ---
          tabPanel("WACC Calculator",
                   fluidRow(infoBoxOutput("ibx_wacc"), infoBoxOutput("ibx_re"), infoBoxOutput("ibx_rd")),
                   br(),
                   fluidRow(
                     column(width = 4,
                            numericInput("wacc_re", "股權成本 rₑ (%)", value = 10),
                            checkboxInput("use_estimated_re", "✅ 使用估算的 rₑ（來自 CAPM）", FALSE),
                            numericInput("wacc_rd", "負債成本 rᵈ (%)", value = 5),
                            numericInput("wacc_tax", "所得稅率 T (%)", value = 20),
                            actionButton("calc_wacc", "📊 計算 WACC"),
                            tags$hr(),
                            h4("📐 使用 CAPM 估算 rₑ"),
                            numericInput("capm_rf", "無風險利率 Rf (%)", value = 3),
                            numericInput("capm_beta", "Beta (β)", value = 1.1),
                            numericInput("capm_rm", "市場報酬率 Rm (%)", value = 8),
                            actionButton("calc_capm", "📈 估算 rₑ（CAPM）")
                     ),
                     column(width = 8,
                            htmlOutput("capm_result"),
                            tags$hr(),
                            htmlOutput("wacc_result"),
                            tags$strong("WACC 公式："),
                            helpText("WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)")
                     )
                   )
          ),
          
          # --- ✍️ Manual Input Tab ---
          tabPanel("手動輸入 FCF 參數", fcf_estimation_module_ui("fcf_estimator"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  tmp <- reactive({
    req(input$sc)
    isolate({
      data <- get.data(input$sc)
      if (is.null(data)) stop("Data not found")
      data
    })
  })
  
  ### INCOME STATEMENT
  d_income_statement <- reactive({
    req(tmp())
    
    tryCatch({
      url <- paste0("https://finance.yahoo.com/quote/", input$sc, "/financials/")
      page <- httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0")) %>%
        xml2::read_html()
      
      column_headers <- sapply(2:6, function(i) {
        xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[1]/div/div[', i, ']')
        node <- rvest::html_node(page, xpath = xpath)
        if (!is.null(node)) rvest::html_text(node, trim = TRUE) else paste0("Col_", i)
      })
      
      breakdown_nodes <- rvest::html_nodes(
        page,
        xpath = '//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div/div[1]/div'
      )
      
      breakdown <- rvest::html_text(breakdown_nodes, trim = TRUE)
      n_rows <- length(breakdown)
      
      data_columns <- lapply(2:6, function(col_index) {
        sapply(1:n_rows, function(row_index) {
          xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div[',
                          row_index, ']/div[', col_index, ']')
          node <- page %>% rvest::html_node(xpath = xpath)
          if (!is.null(node)) html_text(node, trim = TRUE) else NA_character_
        })
      })
      
      df_main <- do.call(cbind, data_columns) %>%
        as.data.frame(stringsAsFactors = FALSE)
      colnames(df_main) <- column_headers
      
      df <- cbind(Breakdown = breakdown, df_main)
      return(df)
      
    }, error = function(e) {
      message("Error scraping income statement: ", e$message)
      data.frame(Error = "Failed to retrieve Income Statement", stringsAsFactors = FALSE)
    })
  })
  
  ### BALANCE SHEET
  d_balance_sheet <- reactive({
    req(tmp())
    
    tryCatch({
      url <- paste0("https://finance.yahoo.com/quote/", input$sc, "/balance-sheet/")
      page <- httr::GET(url, httr::add_headers(`User-Agent` = "Mozilla/5.0")) %>%
        xml2::read_html()
      
      column_headers <- sapply(2:5, function(i) {
        xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[1]/div/div[', i, ']')
        node <- rvest::html_node(page, xpath = xpath)
        if (!is.null(node)) rvest::html_text(node, trim = TRUE) else paste0("Col_", i)
      })
      
      breakdown_nodes <- rvest::html_nodes(
        page,
        xpath = '//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div/div[1]/div'
      )
      
      breakdown <- rvest::html_text(breakdown_nodes, trim = TRUE)
      n_rows <- length(breakdown)
      
      data_columns <- lapply(2:5, function(col_index) {
        sapply(1:n_rows, function(row_index) {
          xpath <- paste0('//*[@id="nimbus-app"]/section/section/section/article/article/section/div/div/div[2]/div[',
                          row_index, ']/div[', col_index, ']')
          node <- page %>% rvest::html_node(xpath = xpath)
          if (!is.null(node)) html_text(node, trim = TRUE) else NA_character_
        })
      })
      
      df_main <- do.call(cbind, data_columns) %>%
        as.data.frame(stringsAsFactors = FALSE)
      colnames(df_main) <- column_headers
      
      df <- cbind(Breakdown = breakdown, df_main)
      return(df)
      
    }, error = function(e) {
      message("Error scraping balance sheet: ", e$message)
      data.frame(Error = "Failed to retrieve Balance Sheet", stringsAsFactors = FALSE)
    })
  })
  
  # 預估 g 成長率的 reactive 變數（從模組取得）
  growth_result <- growth_module_server(
    id = "growth",
    d_income_statement = d_income_statement,
    selected_industry = reactive(input$industry_choice)
  )
  
  # FCF 預測模組（注入 estimated g）
  fcf_result <- fcf_estimation_module_server(
    id = "fcf",
    d_income_statement = d_income_statement,
    d_balance_sheet = d_balance_sheet,
    d_cash_flow = d_cash_flow,
    estimated_g = growth_result$g
  )
  
  # 成長率更新時同步更新輸入欄位
  observeEvent(growth_result$g(), {
    req(growth_result$g())
    updateNumericInput(session, "g_gordon", value = growth_result$g())
    updateNumericInput(session, "g_stage1", value = growth_result$g())
    updateNumericInput(session, "g_stage2", value = growth_result$g())
    if (isTRUE(input$apply_custom_to_fcf)) {
      updateNumericInput(session, "fcf_growth_rate", value = growth_result$g())
    }
  })
  
  # 成長率來源說明文字
  output$growth_source_txt <- renderText({
    req(growth_result$source_txt())
    paste0("來源：", growth_result$source_txt())
  })
  
  # DCF 計算結果儲存變數
  dcf_value_result <- reactiveVal(NULL)
  fcf_forecast_result <- reactiveVal(NULL)
  stock_price_estimate_val <- reactiveVal(NULL)
  
  # 主計算按鈕
  observeEvent(input$calc, {
    req(input$dcf_mode, input$sc)
    years <- input$years
    wacc <- if (input$use_calculated_wacc && !is.null(calculated_wacc())) {
      calculated_wacc()
    } else if (input$dcf_mode == "gordon") {
      input$wacc_gordon / 100
    } else {
      input$wacc_stage1 / 100
    }
    
    share_outstanding <- select_clean_metric_row(d_balance_sheet(), "Share Issued")[1]
    dcf_value <- NA
    fcf_forecast_df <- NULL
    
    # Gordon
    if (input$dcf_mode == "gordon") {
      g <- input$g_gordon / 100
      if (g >= wacc) {
        showNotification("❌ 永續成長率 g 必須小於折現率", type = "error")
        return(NULL)
      }
      
      df_fcf <- fcf_result$df_fcf()
      if (is.null(df_fcf)) {
        showNotification("❌ FCF 模組尚未完成預測", type = "error")
        return(NULL)
      }
      
      pv_fcf <- sum(df_fcf$FCF / (1 + wacc)^(1:years))
      terminal_value <- df_fcf$FCF[years] * (1 + g) / (wacc - g)
      pv_terminal <- terminal_value / (1 + wacc)^years
      dcf_value <- pv_fcf + pv_terminal
      
      fcf_forecast_df <- df_fcf |> mutate(Type = "Gordon 預測")
    }
    
    # Two-Stage
    if (input$dcf_mode == "two_stage") {
      g1 <- input$g_stage1 / 100
      g2 <- input$g_stage2 / 100
      r1 <- input$wacc_stage1 / 100
      r2 <- input$wacc_stage2 / 100
      yr_stage1 <- input$yr_stage1
      
      df_fcf <- fcf_result$df_fcf()
      if (is.null(df_fcf)) {
        showNotification("❌ FCF 模組尚未完成預測", type = "error")
        return(NULL)
      }
      
      base <- df_fcf$FCF[1]
      fcf_stage1 <- base * cumprod(rep(1 + g1, yr_stage1))
      fcf_stage2 <- tail(fcf_stage1, 1) * cumprod(rep(1 + g2, years - yr_stage1))
      
      pv_stage1 <- sum(fcf_stage1 / (1 + r1)^(1:yr_stage1))
      pv_stage2 <- sum(fcf_stage2 / (1 + r2)^((yr_stage1 + 1):years))
      terminal_value <- tail(fcf_stage2, 1) * (1 + g2) / (r2 - g2)
      pv_terminal <- terminal_value / (1 + r2)^years
      
      dcf_value <- pv_stage1 + pv_stage2 + pv_terminal
      base_year <- min(df_fcf$Year)
      
      fcf_forecast_df <- tibble(
        Year = base_year + 0:(years - 1),
        FCF = c(fcf_stage1, fcf_stage2),
        Type = c(rep("第一階段", yr_stage1), rep("第二階段", years - yr_stage1))
      )
    }
    
    # 動態模式（預設）
    if (input$dcf_mode != "gordon" && input$dcf_mode != "two_stage") {
      df_fcf <- fcf_result$df_fcf()
      if (is.null(df_fcf)) {
        fcf_forecast_df <- tibble(Year = NA, FCF = NA, Type = "⚠️ 模型錯誤")
      } else {
        g_val <- if (isTRUE(input$apply_custom_to_fcf)) input$fcf_growth_rate else growth_result$g()
        fcf_forecast_df <- df_fcf |>
          mutate(Type = glue("預設（g = {round(g_val, 2)}%）"))
        dcf_value <- sum(df_fcf$FCF / (1 + wacc)^(1:years))
      }
    }
    
    # 儲存結果
    dcf_value_result(dcf_value)
    fcf_forecast_result(fcf_forecast_df)
    
    if (!is.na(dcf_value) && !is.na(share_outstanding) && share_outstanding > 0) {
      stock_price_estimate_val(dcf_value / share_outstanding)
    } else {
      stock_price_estimate_val(NULL)
    }
    
    # 顯示結果文字
    output$vtxt_dcf_results <- renderText({
      if (is.na(dcf_value_result())) return("⚠️ DCF 計算失敗")
      glue::glue("企業估值：${round(dcf_value_result(), 2)}\n每股估值：${round(stock_price_estimate_val(), 2)}")
    })
    
    # 預測圖
    output$plt_fcf <- renderPlot({
      ggplot(fcf_forecast_result(), aes(x = Year, y = FCF, linetype = Type)) +
        geom_line(size = 1.2, color = "steelblue") +
        geom_point(aes(color = FCF < 0), size = 3) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "steelblue")) +
        theme_minimal(base_size = 14)
    })
  })
  
  # InfoBoxes
  output$ibx_enterprise_value_dcf <- renderInfoBox({
    val <- dcf_value_result()
    infoBox("企業估值", value = if (is.null(val)) "N/A" else round(val, 2), icon = icon("building"), color = "purple", fill = TRUE)
  })
  
  output$ibx_stock_value_dcf <- renderInfoBox({
    val <- stock_price_estimate_val()
    infoBox("每股估值", value = if (is.null(val)) "N/A" else paste0("$", round(val, 2)), icon = icon("money-bill-wave"), color = "maroon", fill = TRUE)
  })
  
  # 動態預設 FCF 預測圖
  output$plt_dft_fcf <- renderPlot({
    df <- fcf_result$df_fcf()
    if (is.null(df)) return()
    df$Type <- "預設"
    ggplot(df, aes(x = Year, y = FCF, linetype = Type)) +
      geom_line(size = 1.2, color = "steelblue") +
      geom_point(aes(color = FCF < 0), size = 3) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "steelblue")) +
      theme_minimal(base_size = 14)
  })
}

#-------------------- SHINY APP --------------------#

shinyApp(ui = ui, server = server)