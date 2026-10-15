# 📦 KPI 模組 - Server Side
# 公式慣例：多年年均 = get_avg(年報科目)；成長率 = 年均 YoY（最新→最舊）
# 年報欄位排除 TTM（include_ttm = FALSE），與產業標準區間可比
# fundamental_profile：財報屬性分群 reactive（可選）；產業色碼仍由 industry_choice
# Gold focus KPIs are value-alertable (到值通知) when ticker / Search are ready.
kpi_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow,
                              industry_choice, fundamental_profile = reactive(NULL),
                              current_ticker = reactive(""),
                              user_has_searched = reactive(FALSE),
                              ui_locale = reactive("zh-TW")) {
  moduleServer(id, function(input, output, session) {

    .loc <- function() {
      tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    }
    .kpi_str <- function(key, fallback) {
      if (exists("ui_str", mode = "function")) {
        tryCatch(ui_str(key, .loc()), error = function(e) fallback)
      } else {
        fallback
      }
    }

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
    # Unwrap Bootstrap col-* / shinydashboard width=NULL wrapper so alertable
    # sits directly on .small-box (bell at inner top-right of the gold frame).
    .unwrap_kpi_box <- function(tag) {
      if (!inherits(tag, "shiny.tag") || length(tag$children) < 1L) return(tag)
      cls <- paste(as.character(tag$attribs$class %||% ""), collapse = " ")
      child <- tag$children[[1]]
      if (!inherits(child, "shiny.tag")) return(tag)
      child_cls <- paste(as.character(child$attribs$class %||% ""), collapse = " ")
      is_col <- grepl("\\bcol-(xs|sm|md|lg)-", cls)
      is_null_wrap <- !nzchar(trimws(cls)) && grepl("\\bsmall-box\\b", child_cls)
      if (is_col || is_null_wrap) return(child)
      tag
    }
    .box <- function(value, subtitle, color, icon, metric_id, numeric_value = NA_real_) {
      focus <- .focus_on(metric_id)
      box <- kpi_band_value_box(
        value = value,
        subtitle = subtitle,
        color = color,
        icon = icon,
        width = NULL,
        mark_focus = focus,
        focus_title = .focus_tip()
      )
      box <- .unwrap_kpi_box(box)
      if (!isTRUE(focus) || !exists("ynow_alertable", mode = "function")) return(box)
      tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
      num <- suppressWarnings(as.numeric(numeric_value)[1])
      ready <- isTRUE(tryCatch(user_has_searched(), error = function(e) FALSE)) &&
        is.finite(num) && nzchar(tk)
      ynow_alertable(
        box,
        metric = paste0("fund_focus_", as.character(metric_id)[1]),
        ticker = tk,
        value = num,
        label = paste(.focus_tip(), subtitle, sep = " · "),
        ready = ready,
        default_kind = "value_lte"
      )
    }
    
    # --- Income Statement ---
    # 毛利率 = Gross Profit / Total Revenue
    # Banks without GP: Net Interest Income / Revenue (labeled heuristic, never fake GM)
    output$vbx_gross_profit_margin <- renderUI({
      d_is <- d_income_statement()
      m <- tryCatch(
        kpi_gross_margin_box_metrics(d_is),
        error = function(e) list(mode = "unavailable", reason = "error",
                                 margin_pct = NA_real_, growth_pct = NA_real_)
      )
      margin <- suppressWarnings(as.numeric(m$margin_pct)[1])
      mode <- as.character(m$mode %||% "unavailable")[1]
      # #region agent log
      tryCatch({
        labels <- if (is.data.frame(d_is) && ncol(d_is) >= 1L) as.character(d_is[[1]]) else character(0)
        payload <- list(
          hypothesisId = "A",
          location = "kpi_module.R:vbx_gross_profit_margin",
          message = "KPI gross margin compute (post-fix)",
          data = list(
            ticker = tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) ""),
            industry = tryCatch(as.character(industry_choice() %||% "")[1], error = function(e) ""),
            n_is_rows = length(labels),
            has_gross_profit_label = any(grepl("^Gross Profit$", labels, ignore.case = TRUE)),
            has_cogs_label = any(grepl("Cost Of Revenue|Cost Of Goods Sold", labels, ignore.case = TRUE)),
            has_total_revenue = any(grepl("^Total Revenue$", labels, ignore.case = TRUE)),
            has_net_interest_income = any(grepl("Net Interest Income", labels, ignore.case = TRUE)),
            mode = mode,
            reason = as.character(m$reason %||% "")[1],
            margin_pct = if (is.finite(margin)) margin else NA_real_,
            display = if (is.finite(margin)) "pct" else "N/A",
            runPhase = "post-fix"
          ),
          timestamp = as.numeric(Sys.time()) * 1000,
          runId = "tw-2884-gm"
        )
        cat(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
            "\n", file = "/opt/cursor/logs/debug.log", append = TRUE)
      }, error = function(e) NULL)
      # #endregion
      if (identical(mode, "bank_nii")) {
        # No industry GPM band for banks — keep white / N/A color path honest.
        color <- get_box_color(industry_choice(), "gross_profit_margin", NA_real_)
        sub <- .kpi_str(
          "kpi_bank_nii_margin_subtitle",
          "淨利息收入／營收（銀行啟發式，非毛利率）"
        )
        val_txt <- if (is.finite(margin)) paste0(sprintf("%.2f", margin), "%") else "N/A"
      } else if (identical(mode, "gross") && is.finite(margin)) {
        color <- get_box_color(industry_choice(), "gross_profit_margin", margin)
        sub <- .kpi_str("kpi_gross_margin_subtitle", "毛利率 Gross Profit Margin")
        val_txt <- paste0(sprintf("%.2f", margin), "%")
      } else {
        color <- get_box_color(industry_choice(), "gross_profit_margin", NA_real_)
        sub <- .kpi_str(
          "kpi_gross_margin_unavailable_subtitle",
          "毛利率 · 資料受限（損益表無 Gross Profit）"
        )
        val_txt <- "N/A"
        margin <- NA_real_
      }
      .box(
        value = val_txt,
        subtitle = sub,
        color = color,
        icon = icon("percentage"),
        metric_id = "gross_profit_margin",
        numeric_value = margin)
    })
    
    # 淨利率 = Net Income / Total Revenue
    output$vbx_net_profit_margin <- renderUI({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      margin <- if (!is.na(net) && !is.na(rev) && rev != 0) net / rev * 100 else NA_real_
      color <- get_box_color(industry_choice(), "net_profit_margin", margin)
      .box(
        value = if (!is.na(margin)) paste0(sprintf("%.2f", margin), "%") else "N/A",
        subtitle = "淨利率 Net Profit Margin",
        color = color,
        icon = icon("percentage"),
        metric_id = "net_profit_margin",
        numeric_value = margin)
    })
    
    # 毛利成長率 = 年均 YoY(Gross Profit); banks → NII growth (labeled heuristic)
    output$vbx_gross_profit_growth <- renderUI({
      d_is <- d_income_statement()
      m <- tryCatch(
        kpi_gross_margin_box_metrics(d_is),
        error = function(e) list(mode = "unavailable", reason = "error",
                                 margin_pct = NA_real_, growth_pct = NA_real_)
      )
      val <- suppressWarnings(as.numeric(m$growth_pct)[1])
      mode <- as.character(m$mode %||% "unavailable")[1]
      # #region agent log
      tryCatch({
        labels <- if (is.data.frame(d_is) && ncol(d_is) >= 1L) as.character(d_is[[1]]) else character(0)
        payload <- list(
          hypothesisId = "A",
          location = "kpi_module.R:vbx_gross_profit_growth",
          message = "KPI gross profit growth compute (post-fix)",
          data = list(
            ticker = tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) ""),
            has_gross_profit_label = any(grepl("^Gross Profit$", labels, ignore.case = TRUE)),
            has_net_interest_income = any(grepl("Net Interest Income", labels, ignore.case = TRUE)),
            mode = mode,
            reason = as.character(m$reason %||% "")[1],
            growth_pct = if (is.finite(val)) val else NA_real_,
            display = if (is.finite(val)) "pct" else "N/A",
            runPhase = "post-fix"
          ),
          timestamp = as.numeric(Sys.time()) * 1000,
          runId = "tw-2884-gm"
        )
        cat(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
            "\n", file = "/opt/cursor/logs/debug.log", append = TRUE)
      }, error = function(e) NULL)
      # #endregion
      if (identical(mode, "bank_nii")) {
        color <- get_box_color(industry_choice(), "rev_growth", val)
        sub <- .kpi_str(
          "kpi_bank_nii_growth_subtitle",
          "淨利息收入成長率（銀行啟發式，非毛利成長率）"
        )
        val_txt <- if (is.finite(val)) paste0(sprintf("%.2f", val), "%") else "N/A"
      } else if (identical(mode, "gross") && is.finite(val)) {
        color <- get_box_color(industry_choice(), "rev_growth", val)
        sub <- .kpi_str("kpi_gross_profit_growth_subtitle", "毛利成長率 Gross Profit Growth")
        val_txt <- paste0(sprintf("%.2f", val), "%")
      } else {
        color <- get_box_color(industry_choice(), "rev_growth", NA_real_)
        sub <- .kpi_str(
          "kpi_gross_profit_growth_unavailable_subtitle",
          "毛利成長率 · 資料受限（損益表無 Gross Profit）"
        )
        val_txt <- "N/A"
        val <- NA_real_
      }
      .box(
        value = val_txt,
        subtitle = sub,
        color = color,
        icon = icon("chart-line"),
        metric_id = "gross_profit_growth",
        numeric_value = val)
    })
    
    # 營收成長率 = 年均 YoY(Total Revenue)
    output$vbx_rev_growth <- renderUI({
      val <- get_avg_growth(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "營收成長率 Revenue Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "rev_growth",
        numeric_value = val)
    })
    
    # 營運費用比 = Operating Expense / Total Revenue（不含 COGS）
    output$vbx_opex_ratio <- renderUI({
      op_exp <- get_avg(select_clean_metric_row_any(d_income_statement(), OPEX_PATTERNS, include_ttm = FALSE))
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      ratio <- if (!is.na(op_exp) && !is.na(rev) && rev != 0) op_exp / rev * 100 else NA_real_
      color <- get_box_color(industry_choice(), "opex_ratio", ratio)
      .box(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "營運費用比 OPEX Ratio",
        color = color,
        icon = icon("balance-scale"),
        metric_id = "opex_ratio",
        numeric_value = ratio)
    })
    
    # --- Balance Sheet ---
    # 財務槓桿 = Total Assets / Equity（Equity Multiplier）
    output$vbx_eqt_multiplier <- renderUI({
      avg_asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      avg_equity <- get_avg(select_clean_metric_row_any(d_balance_sheet(), EQUITY_PATTERNS, include_ttm = FALSE))
      avg_ratio <- if (!is.na(avg_asset) && !is.na(avg_equity) && avg_equity != 0) avg_asset / avg_equity else NA_real_
      color <- get_box_color(industry_choice(), "eqt_multiplier", avg_ratio)
      .box(
        value = if (!is.na(avg_ratio)) sprintf("%.2f", avg_ratio) else "N/A",
        subtitle = "財務槓桿比率 Financial Leverage",
        color = color,
        icon = icon("chart-line"),
        metric_id = "eqt_multiplier",
        numeric_value = avg_ratio)
    })
    
    # --- Cash Flow ---
    # 營運現金成長率 = 年均 YoY(Operating Cash Flow)
    output$vbx_op_cash_flow_growth <- renderUI({
      val <- get_avg_growth(cashflow_total_row(d_cash_flow(), "ocf", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "營業現金成長率 Operating CF Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "op_cash_flow_growth",
        numeric_value = val)
    })
    
    # 投資現金成長率 = 年均 YoY(Investing Cash Flow)；負值為常態（流出）
    output$vbx_inv_cash_flow_growth <- renderUI({
      val <- get_avg_growth(cashflow_total_row(d_cash_flow(), "icf", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "投資現金成長率 Investing CF Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "inv_cash_flow_growth",
        numeric_value = val)
    })
    
    # 融資現金成長率 = 年均 YoY(Financing Cash Flow)
    output$vbx_fin_cash_flow_growth <- renderUI({
      val <- get_avg_growth(cashflow_total_row(d_cash_flow(), "fin", include_ttm = FALSE))
      color <- get_box_color(industry_choice(), "rev_growth", val)
      .box(
        value = if (!is.na(val)) paste0(sprintf("%.2f", val), "%") else "N/A",
        subtitle = "籌資現金成長率 Financing CF Growth",
        color = color,
        icon = icon("chart-line"),
        metric_id = "fin_cash_flow_growth",
        numeric_value = val)
    })
    
    # --- Cross KPIs ---
    # ROA = Net Income / Total Assets
    output$vbx_ROA <- renderUI({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      ratio <- if (!is.na(net) && !is.na(asset) && asset != 0) net / asset * 100 else NA_real_
      color <- get_box_color(industry_choice(), "roa", ratio)
      .box(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "資產報酬率 ROA",
        color = color,
        icon = icon("chart-line"),
        metric_id = "roa",
        numeric_value = ratio)
    })
    
    # ROE = Net Income / Equity
    output$vbx_ROE <- renderUI({
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      equity <- get_avg(select_clean_metric_row_any(d_balance_sheet(), EQUITY_PATTERNS, include_ttm = FALSE))
      ratio <- if (!is.na(net) && !is.na(equity) && equity != 0) net / equity * 100 else NA_real_
      color <- get_box_color(industry_choice(), "roe", ratio)
      .box(
        value = if (!is.na(ratio)) paste0(sprintf("%.2f", ratio), "%") else "N/A",
        subtitle = "股東權益報酬率 ROE",
        color = color,
        icon = icon("chart-line"),
        metric_id = "roe",
        numeric_value = ratio)
    })
    
    # 資產周轉率 = Total Revenue / Total Assets（無產業區間 → 白／無色）
    output$vbx_asset_turnover <- renderUI({
      rev <- get_avg(select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE))
      asset <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Assets", include_ttm = FALSE))
      ratio <- if (!is.na(rev) && !is.na(asset) && asset != 0) rev / asset else NA_real_
      .box(
        value = if (!is.na(ratio)) sprintf("%.2f", ratio) else "N/A",
        subtitle = "資產週轉率 Asset Turnover",
        color = "none",
        icon = icon("chart-line"),
        metric_id = "asset_turnover",
        numeric_value = ratio)
    })
    
    # 現金流與淨利比 = Operating Cash Flow / Net Income（無產業區間 → 白／無色）
    output$vbx_ocf_net_income <- renderUI({
      ocf <- get_avg(cashflow_total_row(d_cash_flow(), "ocf", include_ttm = FALSE))
      net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
      ratio <- if (!is.na(ocf) && !is.na(net) && net != 0) ocf / net else NA_real_
      .box(
        value = if (!is.na(ratio)) sprintf("%.2f", ratio) else "N/A",
        subtitle = "現金流與淨利比 OCF / Net Income",
        color = "none",
        icon = icon("chart-line"),
        metric_id = "ocf_net_income",
        numeric_value = ratio)
    })
  })
}
