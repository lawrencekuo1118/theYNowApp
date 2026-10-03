# 📦 KPI 模組 - Server Side
# 公式慣例：多年年均 = get_avg(年報科目)；成長率 = 年均 YoY（最新→最舊）
# 年報欄位排除 TTM（include_ttm = FALSE），與產業標準區間可比
kpi_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow, industry_choice) {
  moduleServer(id, function(input, output, session) {
    
    # --- Income Statement ---
    # 毛利率 = Gross Profit / Total Revenue
    output$vbx_gross_profit_margin <- renderValueBox({
      gp <- get_avg(select_clean_metric_row(d_income_statement(), "Gross Profit", include_ttm = FALSE))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      margin <- if (!is.na(gp) && !is.na(rev) && rev != 0) gp / rev * 100 else NA_real_
      color <- get_box_color(industry_choice(), "gross_profit_margin", margin)
      valueBox(
        value = if (!is.na(margin)) paste0(sprintf("%.2f", margin), "%") else "N/A",
        subtitle = "毛利率 Gross Profit Margin",
        color = color,
        icon = icon("percentage"))
    })
    
    # 淨利率 = Net Income / Total Revenue
    output$vbx_net_profit_margin <- renderValueBox({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      margin <- if (!is.na(net) && !is.na(rev) && rev != 0) net / rev * 100 else NA_real_
      color <- get_box_color(industry_choice(), "net_profit_margin", margin)
      valueBox(
        value = if (!is.na(margin)) paste0(sprintf("%.2f", margin), "%") else "N/A",
        subtitle = "淨利率 Net Profit Margin",
        color = color,
        icon = icon("percentage"))
    })
    
    # 毛利成長率 = 年均 YoY(Gross Profit)
    output$vbx_gross_profit_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Gross Profit", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "毛利成長率 Gross Profit Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    # 營收成長率 = 年均 YoY(Total Revenue)
    output$vbx_rev_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "營收成長率 Revenue Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    # 營運費用比 = Operating Expense / Total Revenue（不含 COGS）
    output$vbx_opex_ratio <- renderValueBox({
      op_exp <- get_avg(select_clean_metric_row_any(d_income_statement(), OPEX_PATTERNS, include_ttm = FALSE))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      ratio <- if (!is.na(op_exp) && !is.na(rev) && rev != 0) op_exp / rev * 100 else NA_real_
      color <- get_box_color(industry_choice(), "opex_ratio", ratio)
      valueBox(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "營運費用比 OPEX Ratio",
        color = color,
        icon = icon("balance-scale"))
    })
    
    # --- Balance Sheet ---
    # 財務槓桿 = Total Assets / Equity（Equity Multiplier）
    output$vbx_eqt_multiplier <- renderValueBox({
      avg_asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      avg_equity <- get_avg(select_clean_metric_row_any(d_balance_sheet(), EQUITY_PATTERNS, include_ttm = FALSE))
      avg_ratio <- if (!is.na(avg_asset) && !is.na(avg_equity) && avg_equity != 0) avg_asset / avg_equity else NA_real_
      color <- get_box_color(industry_choice(), "eqt_multiplier", avg_ratio)
      valueBox(
        value = if (!is.na(avg_ratio)) sprintf("%.2f", avg_ratio) else "N/A",
        subtitle = "財務槓桿比率 Financial Leverage",
        color = color,
        icon = icon("chart-line"))
    })
    
    # --- Cash Flow ---
    # 營運現金成長率 = 年均 YoY(Operating Cash Flow)
    output$vbx_op_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "營運現金成長率 Operating CF Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    # 投資現金成長率 = 年均 YoY(Investing Cash Flow)；負值為常態（流出）
    output$vbx_inv_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Investing Cash Flow", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "投資現金成長率 Investing CF Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    # 融資現金成長率 = 年均 YoY(Financing Cash Flow)
    output$vbx_fin_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Financing Cash Flow", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      valueBox(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "融資現金成長率 Financing CF Growth",
        color = color,
        icon = icon("chart-line"))
    })
    
    # --- Cross KPIs ---
    # ROA = Net Income / Total Assets
    output$vbx_ROA <- renderValueBox({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      ratio <- if (!is.na(net) && !is.na(asset) && asset != 0) net / asset * 100 else NA_real_
      color <- get_box_color(industry_choice(), "roa", ratio)
      valueBox(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "資產報酬率 ROA",
        color = color,
        icon = icon("chart-line"))
    })
    
    # ROE = Net Income / Equity
    output$vbx_ROE <- renderValueBox({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      equity <- get_avg(select_clean_metric_row_any(d_balance_sheet(), EQUITY_PATTERNS, include_ttm = FALSE))
      ratio <- if (!is.na(net) && !is.na(equity) && equity != 0) net / equity * 100 else NA_real_
      color <- get_box_color(industry_choice(), "roe", ratio)
      valueBox(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "股東權益報酬率 ROE",
        color = color,
        icon = icon("chart-line"))
    })
    
    # 資產周轉率 = Total Revenue / Total Assets
    output$vbx_asset_turnover <- renderValueBox({
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      ratio <- if (!is.na(rev) && !is.na(asset) && asset != 0) rev / asset else NA_real_
      valueBox(
        value = if (!is.na(ratio)) sprintf("%.2f", ratio) else "N/A",
        subtitle = "資產周轉率 Asset Turnover",
        color = "black",
        icon = icon("chart-line"))
    })
    
    # 現金流與淨利比 = Operating Cash Flow / Net Income
    output$vbx_ocf_net_income <- renderValueBox({
      ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow", include_ttm = FALSE))
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      ratio <- if (!is.na(ocf) && !is.na(net) && net != 0) ocf / net else NA_real_
      valueBox(
        value = if (!is.na(ratio)) sprintf("%.2f", ratio) else "N/A",
        subtitle = "現金流與淨利比 OCF / Net Income",
        color = "black",
        icon = icon("chart-line"))
    })
  })
}
