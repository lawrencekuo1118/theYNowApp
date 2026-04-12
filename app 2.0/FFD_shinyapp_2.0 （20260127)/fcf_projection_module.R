# 📦 自由現金流投影模組 - Server Side
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
    
    # 📦 抽取財報參數
    extract_dcf_parameters <- function(df_is, df_cf, df_bs) {
      
      revenue    <- select_clean_metric_row(df_is, "Total Revenue")
      op_income  <- select_clean_metric_row(df_is, "Operating Income")
      income_tax <- select_clean_metric_row(df_is, "Income Tax Expense")
      pre_tax    <- select_clean_metric_row(df_is, "Earnings Before Tax")
      d_and_a    <- select_clean_metric_row(df_cf, "Depreciation & Amortization")
      capex      <- select_clean_metric_row(df_cf, "Capital Expenditure")
      ca         <- select_clean_metric_row(df_bs, "Total Current Assets")
      cash       <- select_clean_metric_row(df_bs, "Cash And Cash Equivalents")
      cl         <- select_clean_metric_row(df_bs, "Total Current Liabilities")
      std        <- select_clean_metric_row(df_bs, "Short Term Debt")
      
      # 安全檢查：確保長度足夠進行差分計算
      has_min_data <- length(revenue) >= 2 && length(ca) >= 2 && length(cl) >= 2
      
      if (!has_min_data) return(NULL)
      
      delta_assets <- (ca[1] - (cash[1] %||% 0)) - (ca[2] - (cash[2] %||% 0))
      delta_liab   <- (cl[1] - (std[1] %||% 0)) - (cl[2] - (std[2] %||% 0))
      delta_nwc    <- delta_assets - delta_liab
      delta_rev    <- revenue[1] - revenue[2]
      
      list(
        revenue_start = revenue[1],
        revenue_growth = estimate_historical_growth(revenue) / 100,
        ebit_margin = mean(op_income[1:min(2, length(op_income))] / revenue[1:min(2, length(revenue))], na.rm = TRUE),
        tax_rate = abs(mean(income_tax[1:min(2, length(income_tax))] / pre_tax[1:min(2, length(pre_tax))], na.rm = TRUE)),
        da_rate = mean(d_and_a[1:min(2, length(d_and_a))] / revenue[1:min(2, length(revenue))], na.rm = TRUE),
        capex_rate = mean(abs(capex[1:min(2, length(capex))]) / revenue[1:min(2, length(revenue))], na.rm = TRUE),
        nwc_rate = if (!is.na(delta_rev) && delta_rev != 0) delta_nwc / delta_rev else 0.05
      )
    }
    
    # 🧮 推估 FCF
    project_fcf <- function(params, years) {
      if (is.null(params)) return(rep(0, years))
      with(params, {
        revenue <- numeric(years)
        fcf <- numeric(years)
        revenue[1] <- revenue_start * (1 + revenue_growth) # 第一年預測從去年末開始成長
        
        for (t in 1:years) {
          if (t > 1) revenue[t] <- revenue[t - 1] * (1 + revenue_growth)
          ebit <- revenue[t] * ebit_margin
          nopat <- ebit * (1 - tax_rate)
          da <- revenue[t] * da_rate
          cpx <- revenue[t] * capex_rate
          delta_rev <- if (t == 1) (revenue[t] - revenue_start) else (revenue[t] - revenue[t - 1])
          dnwc <- delta_rev * nwc_rate
          fcf[t] <- nopat + da - cpx - dnwc
        }
        return(round(fcf, 2))
      })
    }
    
    # 📊 反應式資料框：修復 Log 中的拼接錯誤
    fcf_df <- reactive({
      df_is <- d_income_statement()
      df_cf <- d_cash_flow()
      df_bs <- d_balance_sheet()
      n <- input_years()
      base_year <- as.numeric(format(Sys.Date(), "%Y"))
      
      if (is.null(df_is) || is.null(df_cf) || is.null(df_bs) || n < 1) return(NULL)
      
      fcf_hist <- select_clean_metric_row(df_cf, "Free Cash Flow")
      fcf_start <- if (!all(is.na(fcf_hist))) fcf_hist[1] else 100
      
      # 🔴 核心修復：使用明確的逗號分隔 data.frame 參數
      if (calc_trigger() == 0) {
        p <- extract_dcf_parameters(df_is, df_cf, df_bs)
        proj <- project_fcf(p, n)
        
        data.frame(
          Year = base_year + 1:n,
          FCF = proj,
          Type = "財報推估預設",
          stringsAsFactors = FALSE
        )
      } else if (input_mode() == "gordon") {
        g <- g_gordon() %||% 5
        proj <- fcf_start * (1 + g / 100)^(1:n)
        
        data.frame(
          Year = base_year + 1:n,
          FCF = round(proj, 2),
          Type = "Gordon 預測",
          stringsAsFactors = FALSE
        )
      } else {
        g1 <- (g_stage1() %||% 5) / 100
        g2 <- (g_stage2() %||% 2) / 100
        s1 <- min(yr_stage1() %||% 5, n)
        s2 <- max(n - s1, 0)
        
        fcf1 <- fcf_start * cumprod(rep(1 + g1, s1))
        fcf2 <- if (s2 > 0) fcf1[length(fcf1)] * cumprod(rep(1 + g2, s2)) else numeric(0)
        
        # 修正拼接：確保 FCF 與 Type 長度與 Year 相同
        data.frame(
          Year = base_year + 1:n,
          FCF = round(c(fcf1, fcf2), 2),
          Type = c(rep("第一階段", s1), rep("第二階段", s2)),
          stringsAsFactors = FALSE
        )
      }
    })
    
    # ... 圖表與表格輸出維持不變 ...
    output$fcf_plot <- renderPlot({
      df <- fcf_df()
      req(df)
      ggplot(df, aes(x = Year, y = FCF, color = Type, group = 1)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = "未來自由現金流預測", subtitle = paste("起始 FCF:", format_dollar_abbr(fcf_start)))
    })
  })
}
