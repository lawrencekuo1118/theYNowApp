# ==============================================================================
# 📦 KPI 模組 - Server Side (高效能預處理版)
# ==============================================================================

kpi_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow, industry_choice) {
  moduleServer(id, function(input, output, session) {
    
    # --- 1. 數據預處理中心 (解決重複計算與索引報錯) ---
    # 使用 reactive 將所有需要的指標一次性洗成純數值向量
    m <- reactive({
      # 確保數據已抓取
      req(d_income_statement(), d_balance_sheet(), d_cash_flow())
      
      list(
        # 損益表
        rev = select_clean_metric_row(d_income_statement, "Total Revenue"),
        gp  = select_clean_metric_row(d_income_statement, "Gross Profit"),
        ni  = select_clean_metric_row(d_income_statement, "Net Income"),
        op  = select_clean_metric_row(d_income_statement, "Operating Income"),
        oe  = select_clean_metric_row(d_income_statement, "Operating Expense"),
        # 資產表
        ast = select_clean_metric_row(d_balance_sheet, "Total Assets"),
        eqt = select_clean_metric_row(d_balance_sheet, "Common Stock Equity"),
        # 現金表
        ocf = select_clean_metric_row(d_cash_flow, "Operating Cash Flow"),
        fcf = select_clean_metric_row(d_cash_flow, "Free Cash Flow"),
        icf = select_clean_metric_row(d_cash_flow, "Investing Cash Flow"),
        fcf_flow = select_clean_metric_row(d_cash_flow, "Financing Cash Flow")
      )
    })
    
    # --- 2. 損益指標 (Income Statement) ---
    
    output$vbx_gross_profit_margin <- renderValueBox({
      val <- (get_avg(m()$gp) / get_avg(m()$rev)) * 100
      valueBox(
        value = if (!is.na(val)) paste0(round(val, 2), "%") else "N/A",
        subtitle = "毛利率 Gross Profit Margin",
        color = get_box_color(industry_choice(), "gross_profit_margin", val), 
        icon = icon("percentage"))
    })
    
    output$vbx_net_profit_margin <- renderValueBox({
      val <- (get_avg(m()$ni) / get_avg(m()$rev)) * 100
      valueBox(
        value = if (!is.na(val)) paste0(round(val, 2), "%") else "N/A",
        subtitle = "淨利率 Net Profit Margin",
        color = get_box_color(industry_choice(), "net_profit_margin", val), 
        icon = icon("hand-holding-usd"))
    })
    
    output$vbx_rev_growth <- renderValueBox({
      val <- get_avg_growth(m()$rev)
      valueBox(
        value = if (!is.na(val)) paste0(val, "%") else "N/A",
        subtitle = "營收成長率 Revenue Growth",
        color = get_box_color(industry_choice(), "rev_growth", val), 
        icon = icon("chart-line"))
    })
    
    output$vbx_gross_profit_growth <- renderValueBox({
      val <- get_avg_growth(m()$gp)
      valueBox(
        value = if (!is.na(val)) paste0(val, "%") else "N/A",
        subtitle = "毛利成長率 GP Growth",
        color = get_box_color(industry_choice(), "rev_growth", val), 
        icon = icon("chart-bar"))
    })
    
    # --- 3. 資產與效率指標 (Balance Sheet / Efficiency) ---
    
    output$vbx_eqt_multiplier <- renderValueBox({
      val <- get_avg(m()$ast) / get_avg(m()$eqt)
      valueBox(
        value = if (!is.na(val)) round(val, 2) else "N/A",
        subtitle = "財務槓桿比率 Leverage",
        color = get_box_color(industry_choice(), "eqt_multiplier", val), 
        icon = icon("balance-scale"))
    })
    
    output$vbx_asset_turnover <- renderValueBox({
      val <- get_avg(m()$rev) / get_avg(m()$ast)
      valueBox(
        value = if (!is.na(val)) round(val, 2) else "N/A",
        subtitle = "資產周轉率 Asset Turnover",
        color = "black", icon = icon("sync"))
    })
    
    # --- 4. 現金流指標 (Cash Flow) ---
    
    output$vbx_op_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(m()$ocf)
      valueBox(
        value = if (!is.na(val)) paste0(val, "%") else "N/A",
        subtitle = "營運現金成長率 OCF Growth",
        color = "aqua", icon = icon("tint"))
    })
    
    output$vbx_ocf_net_income <- renderValueBox({
      val <- get_avg(m()$ocf) / get_avg(m()$ni)
      valueBox(
        value = if (!is.na(val)) round(val, 2) else "N/A",
        subtitle = "現金流與淨利比 OCF/NI",
        color = if(!is.na(val) && val > 1) "green" else "orange", 
        icon = icon("money-bill-wave"))
    })
    
    # --- 5. 綜合回報指標 (Cross KPIs) ---
    
    output$vbx_ROE <- renderValueBox({
      val <- (get_avg(m()$ni) / get_avg(m()$eqt)) * 100
      valueBox(
        value = if (!is.na(val)) paste0(round(val, 2), "%") else "N/A",
        subtitle = "股東權益報酬率 ROE",
        color = get_box_color(industry_choice(), "roe", val), 
        icon = icon("trophy"))
    })
    
    output$vbx_ROA <- renderValueBox({
      val <- (get_avg(m()$ni) / get_avg(m()$ast)) * 100
      valueBox(
        value = if (!is.na(val)) paste0(round(val, 2), "%") else "N/A",
        subtitle = "資產報酬率 ROA",
        color = get_box_color(industry_choice(), "roa", val), 
        icon = icon("gem"))
    })
  })
}
