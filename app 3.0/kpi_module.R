# 📦 KPI 模組 - Server Side
kpi_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow, industry_choice) {
  moduleServer(id, function(input, output, session) {
    
    # --- Income Statement ---
    output$vbx_gross_profit_margin <- renderValueBox({
      gp <- get_avg(select_clean_metric_row(d_income_statement(), "Gross Profit"))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue"))
      margin <- gp / rev * 100
      color <- get_box_color(industry_choice(), "gross_profit_margin", margin)
      valueBox(
        value = if (!is.na(margin)) paste0(round(margin, 2), "%") else "N/A",
        subtitle = "毛利率 Gross Profit Margin",
        color = color,
        icon = icon("percentage"))
    })
    
    output$vbx_net_profit_margin <- renderValueBox({
      net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue"))
      margin <- net / rev * 100
      color <- get_box_color(industry_choice(), "rev_growth", margin)
      valueBox(
        value = if (!is.na(margin)) paste0(round(margin, 2), "%") else "N/A",
        subtitle = "淨利率 Net Profit Margin",
        color = color,
        icon = icon("percentage"))
    })
    
    output$vbx_gross_profit_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Gross Profit"))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(val, "%") else "N/A",
        subtitle = "毛利成長率 Gross Profit Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    output$vbx_rev_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Total Revenue"))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(val, "%") else "N/A",
        subtitle = "營收成長率 Revenue Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    output$vbx_opex_ratio <- renderValueBox({
      op_exp <- get_avg(select_clean_metric_row(d_income_statement(), "Operating Expense"))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue"))
      ratio <- op_exp / rev * 100
      color <- get_box_color(industry_choice(), "opex_ratio", ratio)
      valueBox(
        value = if (!is.na(ratio)) paste0(round(ratio, 2), "%") else "N/A",
        subtitle = "營運費用比 OPEX Ratio",
        color = color,
        icon = icon("balance-scale"))
    })
    
    # --- Balance Sheet ---
    output$vbx_eqt_multiplier <- renderValueBox({
      avg_asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets"))
      avg_equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
      avg_ratio <- avg_asset / avg_equity
      color <- get_box_color(industry_choice(), "eqt_multiplier", avg_ratio)
      valueBox(
        value = if (!is.na(avg_ratio)) round(avg_ratio, 2) else "N/A",
        subtitle = "財務槓桿比率 Financial Leverage",
        color = color,
        icon = icon("chart-line"))
    })
    
    # --- Cash Flow ---
    output$vbx_op_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(val, "%") else "N/A",
        subtitle = "營運現金成長率 Operating CF Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    output$vbx_inv_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Investing Cash Flow"))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(val, "%") else "N/A",
        subtitle = "投資現金成長率 Investing CF Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    output$vbx_fin_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Financing Cash Flow"))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(val, "%") else "N/A",
        subtitle = "融資現金成長率 Financing CF Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    # --- Cross KPIs ---
    output$vbx_ROA <- renderValueBox({
      net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
      asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets"))
      ratio <- net / asset * 100
      color <- get_box_color(industry_choice(), "roa", ratio)
      valueBox(
        value = if (!is.na(ratio)) paste0(round(ratio, 2), "%") else "N/A",
        subtitle = "資產報酬率 ROA",
        color = color,
        icon = icon("chart-line"))
    })
    
    output$vbx_ROE <- renderValueBox({
      net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
      equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
      ratio <- net / equity * 100
      color <- get_box_color(industry_choice(), "roe", ratio)
      valueBox(
        value = if (!is.na(ratio)) paste0(round(ratio, 2), "%") else "N/A",
        subtitle = "股東權益報酬率 ROE",
        color = color,
        icon = icon("chart-line"))
    })
    
    output$vbx_asset_turnover <- renderValueBox({
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue"))
      asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets"))
      ratio <- rev / asset
      valueBox(
        value = if (!is.na(ratio)) round(ratio, 2) else "N/A",
        subtitle = "資產周轉率 Asset Turnover",
        color = "black",
        icon = icon("chart-line"))
    })
    
    output$vbx_ocf_net_income <- renderValueBox({
      ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
      net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
      ratio <- ocf / net
      valueBox(
        value = if (!is.na(ratio)) round(ratio, 2) else "N/A",
        subtitle = "現金流與淨利比 OCF / Net Income",
        color = "black",
        icon = icon("chart-line"))
    })
  })
}
