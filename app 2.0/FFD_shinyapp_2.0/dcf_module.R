# ==============================================================================
# 📦 dcf_module.R - 完整整合版
# 功能：處理成長率 (g) 估算、WACC (CAPM) 計算以及與核心 DCF 模組的數據對接
# ==============================================================================

# 📌 1. 反應式變數定義 ----------------------------------------------------------
estimated_g <- reactiveVal(NULL)
estimated_r_e <- reactiveVal(NULL)
calculated_wacc <- reactiveVal(NULL)

# 📌 2. 核心 DCF 模組呼叫 ------------------------------------------------------
# 此處定義折現率來源：優先使用自動估算的 WACC，否則使用手動輸入值
r_to_use <- reactive({
  if (input$use_calculated_wacc && !is.null(calculated_wacc())) {
    calculated_wacc()
  } else {
    # 根據模式選擇對應的手動輸入框
    if (input$dcf_mode == "gordon") input$wacc_gordon/100 else input$wacc_stage1/100
  }
})

# 呼叫整合後的核心模組 (fcf_projection_module.R)
dcf_out <- fcf_projection_module_server(
  id = "fcfmod",
  d_income_statement = d_income_statement,
  d_cash_flow = d_cash_flow,
  d_balance_sheet = d_balance_sheet,
  input_years = reactive(input$years),
  calc_trigger = reactive(input$calc),
  input_mode = reactive(input$dcf_mode),
  g_gordon = reactive(input$g_gordon),
  g_stage1 = reactive(input$g_stage1),
  g_stage2 = reactive(input$g_stage2),
  yr_stage1 = reactive(input$yr_stage1),
  discount_rate = r_to_use,
  share_outstanding = reactive({ 
    # 優先從資產負債表抓取 Ordinary Shares Number
    res <- select_clean_metric_row(d_balance_sheet(), "Ordinary Shares Number")
    if(length(res) == 0) res <- select_clean_metric_row(d_balance_sheet(), "Share Issued")
    as.numeric(res[1]) 
  })
)

# 📈 3. g 永續成長率估算邏輯 --------------------------------------------------
observeEvent(input$calc_growth, {
  req(d_cash_flow())
  fcf_vec <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
  
  if (length(fcf_vec) < 2 || all(is.na(fcf_vec))) {
    showNotification("⚠️ 無足夠自由現金流資料來估算成長率", type = "error")
    return(NULL)
  }
  
  fcf_vec <- na.omit(fcf_vec)
  method <- input$g_growth_method
  
  # 計算邏輯
  val <- switch(method,
                "mean" = estimate_historical_growth(fcf_vec),
                "median" = round(median(diff(fcf_vec)/abs(head(fcf_vec,-1)), na.rm=T)*100, 2),
                "last_year" = round((fcf_vec[1]/fcf_vec[2] - 1)*100, 2),
                "custom" = input$custom_g)
  
  estimated_g(val)
  
  # 更新 UI 顯示與反填
  output$g_result <- renderText({
    glue::glue("📈 成長率估算結果：{val} % (方法：{method})")
  })
  updateNumericInput(session, "g_gordon", value = val)
  updateNumericInput(session, "g_stage1", value = val)
  
  output$ibx_estimated_g <- renderInfoBox({
    infoBox("估算 g 成長率", paste0(val, " %"), icon = icon("chart-line"), color = "purple", fill = TRUE)
  })
})

# 📊 4. WACC & CAPM 計算邏輯 --------------------------------------------------
observeEvent(input$industry_choice, {
  inds <- industry_standards[[input$industry_choice]]
  if (!is.null(inds$beta_avg)) updateNumericInput(session, "capm_beta", value = inds$beta_avg)
  if (!is.null(inds$rm_avg)) updateNumericInput(session, "capm_rm", value = inds$rm_avg)
})

observeEvent(input$calc_capm, {
  r_e_est <- (input$capm_rf/100) + input$capm_beta * ((input$capm_rm/100) - (input$capm_rf/100))
  estimated_r_e(r_e_est)
  updateNumericInput(session, "wacc_re", value = round(r_e_est * 100, 2))
  
  output$capm_result <- renderUI({
    HTML(glue::glue("<div style='color: teal;'>➤ 股東權益成本 (rₑ) = <b>{round(r_e_est * 100, 2)} %</b></div>"))
  })
})

observeEvent(input$calc_wacc, {
  req(d_balance_sheet())
  bs <- d_balance_sheet()
  equity <- select_clean_metric_row(bs, "Common Stock Equity")[1]
  debt <- select_clean_metric_row(bs, "Total Debt")[1]
  
  r_e <- if (input$use_estimated_re && !is.null(estimated_r_e())) estimated_r_e() else input$wacc_re/100
  r_d <- input$wacc_rd / 100
  T_tax <- input$wacc_tax / 100
  
  wacc <- (equity/(equity+debt))*r_e + (debt/(equity+debt))*r_d*(1-T_tax)
  calculated_wacc(wacc)
  wacc_pct <- round(wacc * 100, 2)
  
  # 自動套用至主模型
  if (input$dcf_mode == "gordon") {
    updateNumericInput(session, "wacc_gordon", value = wacc_pct)
  } else {
    updateNumericInput(session, "wacc_stage1", value = wacc_pct)
    updateNumericInput(session, "wacc_stage2", value = max(0, wacc_pct - 1)) # 預設二階段略低
  }
  
  output$wacc_result <- renderUI({ HTML(paste0("<b>➡️ WACC = ", wacc_pct, " %</b>")) })
  output$ibx_wacc <- renderInfoBox({ infoBox("WACC", paste0(wacc_pct, " %"), icon = icon("percent"), color = "aqua", fill = TRUE) })
})

# 🎨 5. FCF 預測繪圖邏輯 (Stock Valuation 頁面專用) ----------------------------
generate_fcf_plot_data <- reactive({
  req(d_cash_flow(), input$years)
  fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
  fcf_start <- if (length(fcf_history) > 0) fcf_history[1] else 100
  n <- input$years
  base_yr <- as.numeric(format(Sys.Date(), "%Y"))
  
  if (input$dcf_mode == "gordon") {
    g <- input$g_gordon / 100
    proj <- fcf_start * (1 + g)^(1:n)
    df <- data.frame(Year = base_yr + (1:n), FCF = proj, Type = "Gordon")
  } else {
    g1 <- input$g_stage1 / 100
    g2 <- input$g_stage2 / 100
    y1 <- input$yr_stage1
    proj <- c(fcf_start * (1 + g1)^(1:y1), (fcf_start * (1 + g1)^y1) * (1 + g2)^(1:(n-y1)))
    df <- data.frame(Year = base_yr + (1:n), FCF = proj, Type = c(rep("Stage 1", y1), rep("Stage 2", n-y1)))
  }
  df
})

output$dft_fcf_plot <- renderPlot({
  df <- generate_fcf_plot_data()
  ggplot(df, aes(x = Year, y = FCF, fill = Type)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "自由現金流預測趨勢")
})

# 💰 6. 估值結果輸出 ----------------------------------------------------------
output$ibx_enterprise_value_dcf <- renderInfoBox({
  res <- dcf_out()
  infoBox("企業估值", if(is.null(res)) "N/A" else format_dollar_abbr(res$enterprise_value), icon = icon("building"), color = "purple")
})

output$ibx_stock_value_dcf <- renderInfoBox({
  res <- dcf_out()
  infoBox("每股估值", if(is.null(res)) "N/A" else paste0("$", round(res$stock_price, 2)), icon = icon("dollar-sign"), color = "maroon")
})
