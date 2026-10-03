fcf_projection_module_server <- function(id,
                                         d_income_statement,
                                         d_cash_flow,
                                         d_balance_sheet,
                                         input_years,
                                         calc_trigger,
                                         input_mode = reactive("gordon"),
                                         g_gordon = reactive(NULL),
                                         g_stage1 = reactive(NULL),
                                         g_stage2 = reactive(NULL),
                                         yr_stage1 = reactive(NULL)) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 🧮 輔助函數：估算成長率
    estimate_historical_growth <- function(x) {
      x <- na.omit(x)
      if (length(x) < 2) return(5)
      g <- diff(x) / head(x, -1)
      round(mean(g, na.rm = TRUE) * 100, 2)
    }
    
    # 📦 抽取財報參數
    extract_dcf_parameters <- function(df_is, df_cf, df_bs) {
      
      revenue <- select_clean_metric_row("Total Revenue", df_is)
      op_income <- select_clean_metric_row("Operating Income", df_is)
      income_tax <- select_clean_metric_row("Income Tax Expense", df_is)
      pre_tax <- select_clean_metric_row("Earnings Before Tax", df_is)
      d_and_a <- select_clean_metric_row("Depreciation & Amortization", df_cf)
      capex <- select_clean_metric_row("Capital Expenditure", df_cf)
      ca <- select_clean_metric_row("Total Current Assets", df_bs)
      cash <- select_clean_metric_row("Cash And Cash Equivalents", df_bs)
      cl <- select_clean_metric_row("Total Current Liabilities", df_bs)
      std <- select_clean_metric_row("Short Term Debt", df_bs)
      
      delta_assets <- ca[1] - ca[2] - (cash[1] - cash[2])
      delta_liab <- cl[1] - cl[2] - ifelse(length(std) >= 2, std[1] - std[2], 0)
      delta_nwc <- delta_assets - delta_liab
      delta_rev <- revenue[1] - revenue[2]
      
      list(
        revenue_start = revenue[1],
        revenue_growth = estimate_historical_growth(revenue) / 100,
        ebit_margin = mean(op_income[1:2] / revenue[1:2], na.rm = TRUE),
        tax_rate = mean(income_tax[1:2] / pre_tax[1:2], na.rm = TRUE),
        da_rate = mean(d_and_a[1:2] / revenue[1:2], na.rm = TRUE),
        capex_rate = mean(abs(capex[1:2]) / revenue[1:2], na.rm = TRUE),
        nwc_rate = if (!is.na(delta_rev) && delta_rev != 0) delta_nwc / delta_rev else 0
      )
    }
    
    # 🧮 推估 FCF（全動態計算）
    project_fcf <- function(params, years) {
      with(params, {
        revenue <- numeric(years)
        ebit <- nopat <- da <- capex <- delta_nwc <- fcf <- numeric(years)
        revenue[1] <- revenue_start
        
        for (t in 1:years) {
          if (t > 1) revenue[t] <- revenue[t - 1] * (1 + revenue_growth)
          ebit[t] <- revenue[t] * ebit_margin
          nopat[t] <- ebit[t] * (1 - tax_rate)
          da[t] <- revenue[t] * da_rate
          capex[t] <- revenue[t] * capex_rate
          delta_rev <- if (t == 1) 0 else revenue[t] - revenue[t - 1]
          delta_nwc[t] <- delta_rev * nwc_rate
          fcf[t] <- nopat[t] + da[t] - capex[t] - delta_nwc[t]
        }
        
        return(round(fcf, 2))
      })
    }
    
    # 📊 回傳 FCF 預測資料框
    fcf_df <- reactive({
      df_is <- d_income_statement()
      df_cf <- d_cash_flow()
      df_bs <- d_balance_sheet()
      n <- input_years()
      base_year <- as.numeric(format(Sys.Date(), "%Y"))
      
      if (is.null(df_is) || is.null(df_cf) || is.null(df_bs) || n < 1) return(NULL)
      
      # 擷取起始 FCF 用於 Gordon/Two-stage
      fcf_hist <- select_clean_metric_row(df_cf, "Free Cash Flow")
      fcf_hist <- na.omit(fcf_hist)
      fcf_start <- if (length(fcf_hist) > 0) head(fcf_hist, 1) else 100
      
      if (calc_trigger() == 0) {
        # 預設情境：從財報推估參數
        p <- extract_dcf_parameters(df_is, df_cf, df_bs)
        proj <- project_fcf(p, n)
        
        data.frame(
          Year = base_year + 0:(n - 1),
          FCF = proj,
          Type = glue::glue("預設（估算 g = {round(p$revenue_growth * 100, 2)}%）")
        )
      } else if (input_mode() == "gordon") {
        g <- g_gordon()
        if (is.null(g)) return(NULL)
        
        proj <- fcf_start * (1 + g / 100)^(0:(n - 1))
        data.frame(
          Year = base_year + 0:(n - 1),
          FCF = round(proj, 2),
          Type = "Gordon 預測"
        )
      } else {
        g1 <- g_stage1(); g2 <- g_stage2(); s1 <- yr_stage1()
        if (any(sapply(list(g1, g2, s1), is.null))) return(NULL)
        
        g1 <- g1 / 100; g2 <- g2 / 100
        s1 <- min(s1, n); s2 <- max(n - s1, 0)
        
        fcf1 <- fcf_start * cumprod(rep(1 + g1, s1))
        fcf2 <- if (s2 > 0) fcf1[length(fcf1)] * cumprod(rep(1 + g2, s2)) else numeric(0)
        
        data.frame(
          Year = base_year + 0:(n - 1),
          FCF = round(c(fcf1, fcf2), 2),
          Type = c(rep("第一階段", s1), rep("第二階段", s2))
        )
      }
    })
    
    # 📈 圖表輸出
    output$fcf_plot <- renderPlot({
      df <- fcf_df()
      if (is.null(df) || nrow(df) == 0) {
        plot.new()
        text(0.5, 0.5, "⚠️ 無法產生預測圖", cex = 1.4)
      } else {
        ggplot(df, aes(x = Year, y = FCF, linetype = Type)) +
          geom_line(size = 1.2, color = "steelblue") +
          geom_point(color = "darkblue", size = 3) +
          theme_minimal(base_size = 14) +
          labs(title = "📈 自由現金流預測圖", x = "年", y = "FCF") +
          theme(legend.position = "top")
      }
    })
    
    # 📁 表格輸出
    output$fcf_table <- renderTable({
      df <- fcf_df()
      if (is.null(df)) data.frame(提醒 = "⚠️ 無法產生預測") else df
    })
    
    # ⬇️ 匯出功能
    output$download_fcf <- downloadHandler(
      filename = function() paste0("fcf_projection_", Sys.Date(), ".csv"),
      content = function(file) write.csv(fcf_df(), file, row.names = FALSE)
    )
    
    # 模組 return（可抓外部）
    return(list(
      fcf_df = fcf_df
    ))
  })
}
