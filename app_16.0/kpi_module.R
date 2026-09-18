# 📦 KPI 模組 - Server Side
# 公式慣例：多年年均 = get_avg(年報科目)；成長率 = 年均 YoY（最新→最舊）
# 年報欄位排除 TTM（include_ttm = FALSE），與產業標準區間可比
# fundamental_profile：財報屬性分群 reactive（可選）；產業色碼仍由 industry_choice
kpi_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow,
                              industry_choice, fundamental_profile = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    .focus_on <- function(metric_id) {
      prof <- tryCatch(fundamental_profile(), error = function(e) NULL)
      pid <- if (is.list(prof)) {
        as.character(prof$profile %||% "")[1]
      } else {
        as.character(prof %||% "")[1]
      }
      if (!nzchar(pid) || !exists("is_profile_focus_metric", mode = "function")) return(FALSE)
      isTRUE(is_profile_focus_metric(pid, metric_id))
    }
    .focus_tip <- function() {
      if (exists("ui_str", mode = "function")) {
        tryCatch(ui_str("kpi_legend_focus_metric"), error = function(e) "本財報屬性關鍵指標")
      } else {
        "本財報屬性關鍵指標"
      }
    }
    .box <- function(value, subtitle, color, icon, metric_id) {
      kpi_band_value_box(
        value = value,
        subtitle = subtitle,
        color = color,
        icon = icon,
        mark_focus = .focus_on(metric_id),
        focus_title = .focus_tip()
      )
    }
    
    # --- Income Statement ---
    # 毛利率 = Gross Profit / Total Revenue
    output$vbx_gross_profit_margin <- renderValueBox({
      gp <- get_avg(select_clean_metric_row(d_income_statement(), "Gross Profit", include_ttm = FALSE))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      margin <- if (!is.na(gp) && !is.na(rev) && rev != 0) gp / rev * 100 else NA_real_
      color <- get_box_color(industry_choice(), "gross_profit_margin", margin)
      .box(
        value = if (!is.na(margin)) paste0(sprintf("%.2f", margin), "%") else "N/A",
        subtitle = "毛利率 Gross Profit Margin",
        color = color,
        icon = icon("percentage"),
        metric_id = "gross_profit_margin")
    })
    
    # 淨利率 = Net Income / Total Revenue
    output$vbx_net_profit_margin <- renderValueBox({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      margin <- if (!is.na(net) && !is.na(rev) && rev != 0) net / rev * 100 else NA_real_
      color <- get_box_color(industry_choice(), "net_profit_margin", margin)
      .box(
        value = if (!is.na(margin)) paste0(sprintf("%.2f", margin), "%") else "N/A",
        subtitle = "淨利率 Net Profit Margin",
        color = color,
        icon = icon("percentage"),
        metric_id = "net_profit_margin")
    })
    
    # 毛利成長率 = 年均 YoY(Gross Profit)
    output$vbx_gross_profit_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Gross Profit", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "毛利成長率 Gross Profit Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "gross_profit_growth")
    })
    
    # 營收成長率 = 年均 YoY(Total Revenue)
    output$vbx_rev_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "營收成長率 Revenue Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "rev_growth")
    })
    
    # 營運費用比 = Operating Expense / Total Revenue（不含 COGS）
    output$vbx_opex_ratio <- renderValueBox({
      op_exp <- get_avg(select_clean_metric_row_any(d_income_statement(), OPEX_PATTERNS, include_ttm = FALSE))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      ratio <- if (!is.na(op_exp) && !is.na(rev) && rev != 0) op_exp / rev * 100 else NA_real_
      color <- get_box_color(industry_choice(), "opex_ratio", ratio)
      .box(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "營運費用比 OPEX Ratio",
        color = color,
        icon = icon("balance-scale"),
        metric_id = "opex_ratio")
    })
    
    # --- Balance Sheet ---
    # 財務槓桿 = Total Assets / Equity（Equity Multiplier）
    output$vbx_eqt_multiplier <- renderValueBox({
      avg_asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      avg_equity <- get_avg(select_clean_metric_row_any(d_balance_sheet(), EQUITY_PATTERNS, include_ttm = FALSE))
      avg_ratio <- if (!is.na(avg_asset) && !is.na(avg_equity) && avg_equity != 0) avg_asset / avg_equity else NA_real_
      color <- get_box_color(industry_choice(), "eqt_multiplier", avg_ratio)
      .box(
        value = if (!is.na(avg_ratio)) sprintf("%.2f", avg_ratio) else "N/A",
        subtitle = "財務槓桿比率 Financial Leverage",
        color = color,
        icon = icon("chart-line"),
        metric_id = "eqt_multiplier")
    })
    
    # --- Cash Flow ---
    # 營運現金成長率 = 年均 YoY(Operating Cash Flow)
    output$vbx_op_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "營業現金成長率 Operating CF Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "op_cash_flow_growth")
    })
    
    # 投資現金成長率 = 年均 YoY(Investing Cash Flow)；負值為常態（流出）
    output$vbx_inv_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Investing Cash Flow", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "投資現金成長率 Investing CF Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "inv_cash_flow_growth")
    })
    
    # 融資現金成長率 = 年均 YoY(Financing Cash Flow)
    output$vbx_fin_cash_flow_growth <- renderValueBox({
      val <- get_avg_growth(select_clean_metric_row(d_cash_flow(), "Financing Cash Flow", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "籌資現金成長率 Financing CF Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "fin_cash_flow_growth")
    })
    
    # --- Cross KPIs ---
    # ROA = Net Income / Total Assets
    output$vbx_ROA <- renderValueBox({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      ratio <- if (!is.na(net) && !is.na(asset) && asset != 0) net / asset * 100 else NA_real_
      color <- get_box_color(industry_choice(), "roa", ratio)
      .box(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "資產報酬率 ROA",
        color = color,
        icon = icon("chart-line"),
        metric_id = "roa")
    })
    
    # ROE = Net Income / Equity
    output$vbx_ROE <- renderValueBox({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      equity <- get_avg(select_clean_metric_row_any(d_balance_sheet(), EQUITY_PATTERNS, include_ttm = FALSE))
      ratio <- if (!is.na(net) && !is.na(equity) && equity != 0) net / equity * 100 else NA_real_
      color <- get_box_color(industry_choice(), "roe", ratio)
      .box(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "股東權益報酬率 ROE",
        color = color,
        icon = icon("chart-line"),
        metric_id = "roe")
    })
    
    # 資產周轉率 = Total Revenue / Total Assets（無產業區間 → 白／無色）
    output$vbx_asset_turnover <- renderValueBox({
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      ratio <- if (!is.na(rev) && !is.na(asset) && asset != 0) rev / asset else NA_real_
      .box(
        value = if (!is.na(ratio)) sprintf("%.2f", ratio) else "N/A",
        subtitle = "資產週轉率 Asset Turnover",
        color = "none",
        icon = icon("chart-line"),
        metric_id = "asset_turnover")
    })
    
    # 現金流與淨利比 = Operating Cash Flow / Net Income（無產業區間 → 白／無色）
    output$vbx_ocf_net_income <- renderValueBox({
      ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow", include_ttm = FALSE))
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      ratio <- if (!is.na(ocf) && !is.na(net) && net != 0) ocf / net else NA_real_
      .box(
        value = if (!is.na(ratio)) sprintf("%.2f", ratio) else "N/A",
        subtitle = "現金流與淨利比 OCF / Net Income",
        color = "none",
        icon = icon("chart-line"),
        metric_id = "ocf_net_income")
    })
  })
}
