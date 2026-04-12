library(shiny)
library(dplyr)
library(glue)

# --- FCF 預測函數 ---
fcf_projection_dynamic <- function(df_is, df_bs, df_cf, years = 5, revenue_growth = 0.08) {
  get <- function(df, metric) select_clean_metric_row(df, metric)
  
  revenue_hist   <- get(df_is, "Total Revenue")
  op_income_hist <- get(df_is, "Operating Income")
  tax_hist       <- get(df_is, "Tax Provision")
  pre_tax_hist   <- get(df_is, "EBIT")
  ca             <- get(df_bs, "Current Assets")
  cash           <- get(df_bs, "Cash, Cash Equivalents & Short Term Investments")
  cl             <- get(df_bs, "Current Liabilities")
  std            <- get(df_bs, "Current Debt And Capital Lease Obligation")
  da_hist        <- get(df_cf, "Depreciation & amortization")
  capex_hist     <- get(df_cf, "Capital Expenditure")
  
  # 資料檢查
  inputs <- list(revenue_hist, op_income_hist, tax_hist, pre_tax_hist, ca, cash, cl, std, da_hist, capex_hist)
  if (any(sapply(inputs, function(x) is.null(x) || length(x) < 2))) return(NULL)
  
  # 初始化預測向量
  revenue <- numeric(years)
  ebit <- nopat <- da <- capex <- delta_nwc <- fcf <- numeric(years)
  revenue[1] <- revenue_hist[1]
  
  for (t in 1:years) {
    if (t > 1) revenue[t] <- revenue[t - 1] * (1 + revenue_growth)
    
    ebit_margin <- op_income_hist[1] / revenue_hist[1]
    tax_rate <- tax_hist[1] / pre_tax_hist[1]
    da_rate <- da_hist[1] / revenue_hist[1]
    capex_rate <- abs(capex_hist[1]) / revenue_hist[1]
    
    delta_assets <- ca[1] - ca[2] - (cash[1] - cash[2])
    delta_liab <- cl[1] - cl[2] - (std[1] - std[2])
    delta_nwc_val <- delta_assets - delta_liab
    delta_rev <- revenue_hist[1] - revenue_hist[2]
    nwc_rate <- if (!is.na(delta_rev) && delta_rev != 0) delta_nwc_val / delta_rev else 0
    
    ebit[t] <- revenue[t] * ebit_margin
    nopat[t] <- ebit[t] * (1 - tax_rate)
    da[t] <- revenue[t] * da_rate
    capex[t] <- revenue[t] * capex_rate
    delta_nwc[t] <- if (t == 1) 0 else (revenue[t] - revenue[t - 1]) * nwc_rate
    fcf[t] <- nopat[t] + da[t] - capex[t] - delta_nwc[t]
  }
  
  data.frame(
    Year = seq_len(years) + as.numeric(format(Sys.Date(), "%Y")) - 1,
    Revenue = round(revenue, 2),
    EBIT = round(ebit, 2),
    NOPAT = round(nopat, 2),
    Dep_Amort = round(da, 2),
    CapEx = round(capex, 2),
    Delta_NWC = round(delta_nwc, 2),
    FCF = round(fcf, 2)
  )
}

# --- UI 模組 ---
fcf_estimation_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("🔍 自由現金流預估（FCF - DCF 模型）"),
    checkboxInput(ns("use_estimated_g"), "使用自動估算的 g 成長率？", TRUE),
    conditionalPanel(
      condition = sprintf("!input['%s']", ns("use_estimated_g")),
      sliderInput(ns("manual_growth"), "手動指定 Revenue 成長率 (%)", 0, 20, 8, 0.5)
    ),
    checkboxInput(ns("manual_override"), "手動輸入缺少資料", FALSE),
    uiOutput(ns("missing_fields_ui")),
    verbatimTextOutput(ns("g_info")),
    plotOutput(ns("plt_fcf"), height = "300px"),
    tableOutput(ns("tbl_fcf")),
    downloadButton(ns("download_fcf"), "下載 FCF 預測結果")
  )
}

# --- Server 模組 ---
fcf_estimation_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow, estimated_g) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 成長率處理（使用估算或手動）
    growth_rate <- reactive({
      if (isTRUE(input$use_estimated_g)) {
        g <- estimated_g()
        if (is.null(g)) return(NULL)
        g / 100
      } else {
        input$manual_growth / 100
      }
    })
    
    # 缺資料檢查
    missing_vars <- reactive({
      df_is <- d_income_statement()
      df_bs <- d_balance_sheet()
      df_cf <- d_cash_flow()
      
      check <- function(df, metric, min_len = 2) {
        x <- try(select_clean_metric_row(df, metric), silent = TRUE)
        is.null(x) || length(x) < min_len
      }
      
      vars <- list(
        ca = check(df_bs, "Current Assets"),
        cash = check(df_bs, "Cash, Cash Equivalents & Short Term Investments"),
        cl = check(df_bs, "Current Liabilities"),
        std = check(df_bs, "Current Debt And Capital Lease Obligation"),
        da = check(df_cf, "Depreciation & amortization", 1),
        revenue = check(df_is, "Total Revenue")
      )
      
      names(Filter(identity, vars))
    })
    
    # 動態顯示缺資料輸入欄位
    output$missing_fields_ui <- renderUI({
      req(input$manual_override)
      fields <- missing_vars()
      if (length(fields) == 0) return(helpText("✅ 所有必要資料皆已成功取得"))
      
      ui_list <- list()
      addField <- function(name, label1, label2 = NULL) {
        fluidRow(
          column(4, numericInput(ns(paste0(name, "1")), paste0("🔹 ", label1), value = NA)),
          if (!is.null(label2)) column(4, numericInput(ns(paste0(name, "2")), paste0("🔹 ", label2), value = NA))
        )
      }
      
      if ("ca" %in% fields) ui_list <- append(ui_list, list(addField("manual_ca", "Current Assets - 最新年度", "前一年")))
      if ("cash" %in% fields) ui_list <- append(ui_list, list(addField("manual_cash", "Cash - 最新年度", "前一年")))
      if ("cl" %in% fields) ui_list <- append(ui_list, list(addField("manual_cl", "Current Liabilities - 最新年度", "前一年")))
      if ("std" %in% fields) ui_list <- append(ui_list, list(addField("manual_std", "Short-Term Debt - 最新年度", "前一年")))
      if ("da" %in% fields) ui_list <- append(ui_list, list(addField("manual_da", "DA 占 Revenue 比率 (%)")))
      if ("revenue" %in% fields) ui_list <- append(ui_list, list(addField("manual_rev", "Revenue - 最新年度", "前一年")))
      
      tagList(tags$hr(), ui_list)
    })
    
    # 手動補值資料
    override_data <- reactive({
      req(input$manual_override)
      fields <- missing_vars()
      list(
        ca      = if ("ca" %in% fields) c(input$manual_ca1, input$manual_ca2) else NULL,
        cash    = if ("cash" %in% fields) c(input$manual_cash1, input$manual_cash2) else NULL,
        cl      = if ("cl" %in% fields) c(input$manual_cl1, input$manual_cl2) else NULL,
        std     = if ("std" %in% fields) c(input$manual_std1, input$manual_std2) else NULL,
        da      = if ("da" %in% fields) input$manual_da else NULL,
        revenue = if ("revenue" %in% fields) c(input$manual_rev1, input$manual_rev2) else NULL
      )
    })
    
    # 主預測 reactive
    df_fcf <- reactive({
      g <- growth_rate()
      if (is.null(g)) return(NULL)
      
      df_is <- d_income_statement()
      df_bs <- d_balance_sheet()
      df_cf <- d_cash_flow()
      od <- override_data()
      
      # 深拷貝
      df_is_mod <- df_is
      df_bs_mod <- df_bs
      df_cf_mod <- df_cf
      
      if (!is.null(od$ca))      df_bs_mod["Current Assets", 1:2] <- od$ca
      if (!is.null(od$cash))    df_bs_mod["Cash, Cash Equivalents & Short Term Investments", 1:2] <- od$cash
      if (!is.null(od$cl))      df_bs_mod["Current Liabilities", 1:2] <- od$cl
      if (!is.null(od$std))     df_bs_mod["Current Debt And Capital Lease Obligation", 1:2] <- od$std
      if (!is.null(od$da))      df_cf_mod["Depreciation & amortization", 1] <- od$da
      if (!is.null(od$revenue)) df_is_mod["Total Revenue", 1:2] <- od$revenue
      
      fcf_projection_dynamic(df_is_mod, df_bs_mod, df_cf_mod, years = 5, revenue_growth = g)
    })
    
    # 輸出
    output$tbl_fcf <- renderTable({
      df <- df_fcf()
      if (is.null(df)) return(data.frame(提醒 = "⚠️ 尚未取得資料或成長率"))
      df
    })
    
    output$plt_fcf <- renderPlot({
      df <- df_fcf()
      if (is.null(df)) return()
      plot(df$Year, df$FCF, type = "b", col = "steelblue", pch = 16,
           main = "FCF 預測圖", xlab = "Year", ylab = "FCF")
    })
    
    output$download_fcf <- downloadHandler(
      filename = function() paste0("fcf_forecast_", Sys.Date(), ".csv"),
      content = function(file) write.csv(df_fcf(), file, row.names = FALSE)
    )
    
    output$g_info <- renderText({
      if (input$use_estimated_g) {
        g_val <- estimated_g()
        if (is.null(g_val)) return("⚠️ 尚未估算 g 成長率")
        glue("📈 使用估算 g 成長率：{g_val} %")
      } else {
        glue("🛠 手動指定成長率：{input$manual_growth} %")
      }
    })
    
    return(list(df_fcf = df_fcf))
  })
}

