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
                                         yr_stage1 = reactive(NULL),
                                         input_capex_rate = reactive(NA), 
                                         input_nwc_rate = reactive(NA),
                                         input_manual_fcf = reactive(NULL)) {
  
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
      
      revenue <- select_clean_metric_row(df_is, "Total Revenue")
      op_income <- select_clean_metric_row(df_is, "Operating Income")
      income_tax <- select_clean_metric_row(df_is, "Income Tax Expense")
      pre_tax <- select_clean_metric_row(df_is, "Earnings Before Tax")
      d_and_a <- select_clean_metric_row(df_cf, "Depreciation & Amortization")
      capex <- select_clean_metric_row(df_cf, "Capital Expenditure")
      ca <- select_clean_metric_row(df_bs, "Total Current Assets")
      cash <- select_clean_metric_row(df_bs, "Cash And Cash Equivalents")
      cl <- select_clean_metric_row(df_bs, "Total Current Liabilities")
      std <- select_clean_metric_row(df_bs, "Short Term Debt")
      
      # 🟢 需求 1：確保自動帶入最近期的收入資料 (排除 NA)
      clean_revenue <- na.omit(revenue)
      rev_start <- if(length(clean_revenue) > 0) clean_revenue[1] else 0
      
      delta_assets <- ca[1] - ca[2] - (cash[1] - cash[2])
      delta_liab <- cl[1] - cl[2] - ifelse(length(std) >= 2, std[1] - std[2], 0)
      delta_nwc <- delta_assets - delta_liab
      delta_rev <- revenue[1] - revenue[2]
      
      list(
        revenue_start = rev_start,  # 已確保為最新一期
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
        
        # 🟢 判斷：如果有自訂值就用自訂值 (記得轉小數)，否則用歷史值 params$capex_rate
        final_capex_rate <- if(!is.na(input_capex_rate())) input_capex_rate() / 100 else capex_rate
        final_nwc_rate <- if(!is.na(input_nwc_rate())) input_nwc_rate() / 100 else nwc_rate
        
        for (t in 1:years) {
          if (t > 1) revenue[t] <- revenue[t - 1] * (1 + revenue_growth)
          ebit[t] <- revenue[t] * ebit_margin
          nopat[t] <- ebit[t] * (1 - tax_rate)
          da[t] <- revenue[t] * da_rate
          
          # 🟢 使用決定好的 final_rate 進行計算
          capex[t] <- revenue[t] * final_capex_rate
          delta_rev <- if (t == 1) 0 else revenue[t] - revenue[t - 1]
          delta_nwc[t] <- delta_rev * final_nwc_rate
          
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
      
      # 🟢 修改：判斷起始 FCF 是否有被使用者手動覆寫
      fcf_history <- select_clean_metric_row(df_cf, "Free Cash Flow")
      
      fcf_start <- if (!is.null(input_manual_fcf()) && !is.na(input_manual_fcf())) {
        input_manual_fcf() # 優先吃使用者的手動數值
      } else if (is.numeric(fcf_history) && length(na.omit(fcf_history)) > 0) {
        head(na.omit(fcf_history), 1) # 其次吃爬蟲
      } else {
        NA
      }
      
      # 若連手動都沒填就跳出，等待輸入
      if (is.na(fcf_start)) return(NULL)
      
      if (calc_trigger() == 0) {
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
    
    # 📈 建立圖表物件 (獨立出來，讓外部可以取用)
    fcf_plot_obj <- reactive({
      df_proj <- fcf_df()
      if (is.null(df_proj) || nrow(df_proj) == 0) return(NULL) # 如果沒有資料，回傳 NULL
      
      hist_fcf_raw <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
      hist_fcf <- rev(na.omit(hist_fcf_raw))
      
      if (length(hist_fcf) > 0) {
        start_proj_year <- df_proj$Year[1]
        hist_years <- seq(start_proj_year - length(hist_fcf), start_proj_year - 1)
        df_hist <- data.frame(Year = hist_years, FCF = hist_fcf, Type = "歷史數據")
        df_plot <- rbind(df_hist, df_proj)
      } else {
        df_plot <- df_proj
      }
      
      # 回傳 ggplot 物件
      ggplot(df_plot, aes(x = Year, y = FCF, color = Type, linetype = Type)) +
        geom_line(linewidth = 1.2) + 
        geom_point(size = 3) +
        geom_text(aes(label = formatC(FCF, format = "f", big.mark = ",", digits = 0)), 
                  vjust = -1.5, size = 4.5, show.legend = FALSE, fontface = "bold") +
        scale_color_manual(values = c("歷史數據" = "#7f8c8d", "Gordon 預測" = "#2980b9", "第一階段" = "#d35400", "第二階段" = "#27ae60")) +
        scale_linetype_manual(values = c("歷史數據" = "solid", "Gordon 預測" = "dashed", "第一階段" = "dashed", "第二階段" = "dotted")) +
        theme_minimal(base_size = 14) +
        labs(title = "📈 歷史與預測自由現金流 (FCF) 走勢圖", x = "年份", y = "自由現金流 (FCF)") +
        theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(face = "bold", size = 16)) +
        expand_limits(y = max(df_plot$FCF, na.rm = TRUE) * 1.2) 
    })
    
    # 讓 UI 可以顯示這張圖
    output$fcf_plot <- renderPlot({
      p <- fcf_plot_obj()
      if (is.null(p)) {
        plot.new()
        text(0.5, 0.5, "⚠️ 無法產生預測圖，請確認參數設定", cex = 1.4)
      } else {
        p
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
    
    # 🟢 新增：獨立抽出歷史參數，準備傳遞給 UI 顯示
    hist_params <- reactive({
      df_is <- d_income_statement()
      df_cf <- d_cash_flow()
      df_bs <- d_balance_sheet()
      
      # 防呆：確保財報資料已經載入
      if (is.null(df_is) || is.null(df_cf) || is.null(df_bs)) return(NULL)
      
      # 呼叫已經寫好的函數來抓取近兩年平均比例
      extract_dcf_parameters(df_is, df_cf, df_bs)
    })
    
    # 🟢 匯出供外部 (server.R) 呼叫
    return(list(
      fcf_df = fcf_df,
      hist_params = hist_params,
      fcf_plot_obj = fcf_plot_obj # <-- 新增這行：把圖表物件傳遞出去！
    ))
  }) # moduleServer 的結尾
}
