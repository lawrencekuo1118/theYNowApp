# ==========================================
# server.R - 後端邏輯與資料運算
# ==========================================

library(reticulate)
reticulate::use_python("/usr/local/bin/python3", required = TRUE)

server <- function(input, output, session) {
  
  # ==========================================
  # 輔助函數：取得最新一期期末現金餘額 (DRY 重構)
  # ==========================================
  get_latest_cash_position <- function(df_cf) {
    if (is.null(df_cf) || nrow(df_cf) == 0) return(NA)
    
    cash_kws <- c("End Cash Position", "Ending Cash Position", "Cash at End of Period")
    for (kw in cash_kws) {
      val <- select_clean_metric_row(df_cf, kw)
      if (length(val) > 0 && !all(is.na(val))) {
        return(as.numeric(na.omit(val))[1])
      }
    }
    return(NA) # 如果所有關鍵字都找不到，就回傳 NA
  }
  
  # 輔助函數：裁切表格
  trim_financial_table <- function(df, end_metric) {
    if (is.null(df) || nrow(df) == 0) return(df)
    idx <- grep(end_metric, df[[1]], ignore.case = TRUE)
    if (length(idx) > 0) return(df[1:idx[1], ])
    return(df)
  }
  
  # 輔助函數：計算 FCF 預測
  fcf_projection <- function(start_fcf, growth_rate, years) {
    g <- growth_rate / 100
    return(start_fcf * (1 + g)^(0:(years - 1)))
  }
  
  # 1. 建立一個 Reactive 變數，用來儲存公司基本資訊 (點擊搜尋時觸發)
  corp_info <- eventReactive(input$btn_search, {
    req(input$txt_search)
    
    # 呼叫 search_module2.R 中的函數抓取公司名稱與產業
    withProgress(message = '抓取公司基本資訊...', value = 0.5, {
      get_yahoo_industry(input$txt_search)
    })
  })
  
  # 2. 將抓到的公司名稱輸出到首頁大標題
  output$txt_corpname <- renderText({
    req(summary_data())
    
    # 從抓回來的 dataframe 屬性中提取公司全稱
    name <- attr(summary_data(), "company_name")
    
    # 最後的防呆：如果真的什麼都沒抓到，至少顯示使用者輸入的代碼
    if (is.null(name) || is.na(name) || name == "") {
      return(paste("Stock:", toupper(input$txt_search)))
    } else {
      return(name)
    }
  })
  
  # (選擇性) 如果你有其他地方需要顯示產業資訊，也可以從 corp_info() 拿：
  # output$search_results <- renderText({ req(corp_info()); corp_info()$display_text })
  
  # ==========================================
  # 1. 建立 Summary 的 Reactive 變數
  # ==========================================
  summary_data <- eventReactive(input$search, {
    req(input$sc)
    withProgress(message = '抓取 Yahoo Summary 表格...', {
      get_summary_data(input$sc)
    })
  })
  
  # ==========================================
  # 2. 輸出 tbFinanceSummary 表格
  # ==========================================
  output$tbFinanceSummary <- renderDataTable({
    req(summary_data())
    datatable(summary_data(), 
              options = list(pageLength = 20, dom = 't', scrollX = TRUE),
              rownames = TRUE)
  })
  
  # ==========================================
  # 3. 擷取 Summary 表格內的特定欄位
  # ==========================================
  output$ibx_marketcap <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "Market Cap (intraday)" %in% df$Item) {
      df$Value[df$Item == "Market Cap (intraday)"]
    } else { "N/A" }
    
    infoBox("Market Cap", val, icon = icon("globe"), color = "blue")
  })
  
  output$ibx_stockprice <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "Previous Close" %in% df$Item) {
      df$Value[df$Item == "Previous Close"]
    } else { "N/A" }
    
    infoBox("Previous Close", val, icon = icon("chart-line"), color = "purple")
  })
  
  output$ibx_EPS <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "EPS (TTM)" %in% df$Item) {
      df$Value[df$Item == "EPS (TTM)"]
    } else { "N/A" }
    
    infoBox("EPS (TTM)", val, icon = icon("dollar-sign"), color = "green")
  })
  
  financials <- eventReactive(input$search, {
    req(input$sc)
    
    # 執行帶有進度條的抓取過程
    withProgress(message = paste('正在模擬瀏覽器抓取', input$sc, '數據...'), value = 0, {
      
      incProgress(0.2, detail = "啟動背景 Chrome 瀏覽器...")
      raw_html_list <- get.data(input$sc) # 呼叫 setup.R 內的新函數
      
      incProgress(0.5, detail = "正在從網頁中提取財務數值...")
      
      # 解析三張報表
      is_table <- extract_yf_financial_table(raw_html_list$income_statement)
      bs_table <- extract_yf_financial_table(raw_html_list$balance_sheet)
      cf_table <- extract_yf_financial_table(raw_html_list$cash_flow)
      
      incProgress(0.3, detail = "數據同步完成！")
      
      # 回傳整理後的清單
      list(
        income = is_table,
        balance = bs_table,
        cashflow = cf_table
      )
    })
  })
  
  ### INCOME STATEMENT
  # Reactive: Parse and extract Income Statement table
  d_income_statement <- reactive({
    dat <- financials()$income
    if (is.null(dat)) return(data.frame(Error = "No Income Statement Data"))
    dat
  })
  
  # Output: DataTable
  output$tbIncomeStatement <- renderDataTable({
    df <- d_income_statement()
    df <- trim_financial_table(df, "Tax Effect of Unusual Items")
    datatable(df, options = list(pageLength = 20))
  })
  
  # Output: Download Handler
  output$IS_download <- downloadHandler(
    filename = function() {
      paste0(input$sc, "_incomestatement_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(d_income_statement(), file, row.names = FALSE)
    }
  )
  
  ### BALANCE SHEET
  # Reactive: Parse and extract Balance Sheet table
  d_balance_sheet <- reactive({
    dat <- financials()$balance
    if (is.null(dat)) return(data.frame(Error = "No Balance Sheet Data"))
    dat
  })
  
  # Output: DataTable
  output$tbBalanceSheet <- renderDataTable({
    df <- d_balance_sheet()
    df <- trim_financial_table(df, "Treasury Shares Number")
    datatable(df)
  })
  
  # Output: Download Handler
  output$BS_download <- downloadHandler(
    filename = function() {
      paste0(input$sc, "_balancesheet_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(d_balance_sheet(), file, row.names = FALSE)
    }
  )
  
  ### CASH FLOW
  # Reactive: Parse and extract Cash Flow table
  d_cash_flow <- reactive({
    dat <- financials()$cashflow
    if (is.null(dat)) return(data.frame(Error = "No Cash Flow Data"))
    dat
  })
  
  # Output: 互動折線圖
  selected_cashflow_data <- reactive({
    req(d_cash_flow())
    keyword <- switch(input$cf_type,
                      "Operating Cash Flow" = "Operating Cash Flow",
                      "Investing Cash Flow" = "Investing Cash Flow",
                      "Financing Cash Flow" = "Financing Cash Flow")
    
    d_cash_flow()[grepl(keyword, d_cash_flow()$Breakdown, ignore.case = TRUE), ]
  })
  
  output$cf_plot <- renderPlotly({
    df <- selected_cashflow_data()
    req(nrow(df) > 0)
    
    # 轉成 long format
    cf_vals <- df[1, -1]
    cf_vals <- as.numeric(gsub(",", "", cf_vals))
    cf_labels <- colnames(df)[-1]
    
    plot_df <- data.frame(Year = cf_labels, Value = cf_vals)
    
    p <- ggplot(plot_df, aes(x = Year, y = Value, group = 1)) +
      geom_line(color = "black", size = 1.2) +
      geom_point(aes(color = Value < 0), size = 2.5) +  # 根據負數上色
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
      theme_bw() +
      labs(title = input$cf_type, x = "", y = "USD") +
      theme(
        plot.title = element_text(size = 12, face = "bold", color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Output: DataTable
  output$tbCashFlow <- renderDataTable({
    df <- d_cash_flow()
    df <- trim_financial_table(df, "Free Cash Flow")
    datatable(df)
  })
  
  # Output: Download Handler
  output$CF_download <- downloadHandler(
    filename = function() {
      paste0(input$sc, "_cashflow_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(d_cash_flow(), file, row.names = FALSE)
    }
  )
  
  ### ⬇️ 呼叫 KPI 模組，將主資料餵入
  kpi_module_server(
    id = "kpi",
    d_income_statement = d_income_statement,
    d_balance_sheet = d_balance_sheet,
    d_cash_flow = d_cash_flow,
    industry_choice = reactive(input$industry_choice)
  )
  
  ### Fraud Risk Warnings
  # 先定義 reactiveValues 來存每個警訊結果
  fraud_warnings <- reactiveValues(
    fcf = "", ocf = "", biz = "", cashback = "", debt = ""
  )
  
  # 各種 fraud 判斷邏輯
  output$nofreecashflow <- renderText({
    fcf <- get_avg(select_clean_metric_row(d_cash_flow(), "Free Cash Flow"))
    fraud_warnings$fcf <- if (is.na(fcf)) "" else if (fcf < 0) {
      "⚠️ 自由現金流為負數，可能營運困難或大量資本支出"
    } else { "" }
    fraud_warnings$fcf
  })
  
  output$nooperatingcashflow <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    fraud_warnings$ocf <- if (is.na(ocf)) "" else if (ocf < 0) {
      "⚠️ 營業現金流為負數，代表核心業務沒有產生現金"
    } else { "" }
    fraud_warnings$ocf
  })
  
  output$notdoingbusiness <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    fraud_warnings$biz <- if (is.na(ocf) || is.na(net)) "" else if (ocf < net) {
      "⚠️ 營業現金流低於淨利，帳面賺錢但現金未實現"
    } else { "" }
    fraud_warnings$biz
  })
  
  output$notgettingcashback <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    fraud_warnings$cashback <- if (is.na(ocf) || is.na(net)) "" else if (net > 0 && ocf < 0) {
      "⚠️ 淨利為正但現金流為負，獲利品質存疑"
    } else { "" }
    fraud_warnings$cashback
  })
  
  output$highdebttoequity <- renderText({
    total_liabilities <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Debt"))
    total_equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
    # 避免分母為0或NA
    ratio <- if (is.na(total_liabilities) || is.na(total_equity) || total_equity == 0) NA else total_liabilities / total_equity
    
    fraud_warnings$debt <- if (is.na(ratio)) "" else if (ratio > 2) {
      "⚠️ 負債對權益比率過高，財務槓桿風險大"
    } else { "" }
    fraud_warnings$debt
  })
  
  # fallback 顯示 - 若都沒有風險警訊就顯示這個
  output$no_fraud_detected <- renderText({
    if (all(fraud_warnings$fcf == "",
            fraud_warnings$ocf == "",
            fraud_warnings$biz == "",
            fraud_warnings$cashback == "",
            fraud_warnings$debt == "")) {
      "Currently no fraud risks detected."
    } else {
      ""
    }
  })
  
  ### Others
  output$stable_indicator_table <- renderTable({
    data.frame(
      指標名稱 = c("毛利率", "OPEX Ratio", "ROA / ROE", "存貨週轉 / 應收週轉", "Equity Multiplier", "自由現金流比"),
      穩定性 = c("★★★★☆", "★★★★☆", "★★★★☆", "★★★☆☆", "★★★☆☆", "★★★★★"),
      說明 = c(
        "技術/品牌優勢的象徵",
        "管理與營運效率穩定性",
        "去波動化後能長期觀察企業效率",
        "營運效率的直接反映",
        "財務體質穩定，不易劇變",
        "最能看出企業真實價值創造力"
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "m", width = "100%")
  
  # ==========================================
  # 📊 Variances 分頁 - CAPM 與 WACC 運算邏輯
  # ==========================================
  
  # 🟢 需求 2：產業切換時自動帶入 Beta
  observeEvent(input$industry_choice, {
    req(input$industry_choice)
    inds <- industry_standards[[input$industry_choice]]
    if (!is.null(inds)) {
      updateNumericInput(session, "capm_beta", value = inds$beta_avg)
      updateNumericInput(session, "capm_rm", value = inds$rm_avg)
    }
  })
  
  # 🟢 需求 3：將 Calculator 的年數同步顯示於 Variances
  output$txt_display_years <- renderUI({
    HTML(paste0("<b>目前設定之預測年數：<span style='color:red; font-size:16px;'>", input$years, "</span> 年</b>"))
  })
  
  # 1. 計算並渲染 CAPM (權益成本)
  output$vbx_capm <- renderValueBox({
    # 將百分比轉為小數點進行計算
    rf <- input$var_rf / 100
    rm <- input$var_rm / 100
    beta <- input$var_beta
    
    # 🧮 CAPM 公式: Ke = Rf + Beta * (Rm - Rf)
    capm <- rf + beta * (rm - rf)
    
    valueBox(
      value = paste0(round(capm * 100, 2), "%"),
      subtitle = "預估權益成本 (CAPM / Cost of Equity)",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  # 2. 計算並渲染 WACC (加權平均資本成本)
  output$vbx_wacc <- renderValueBox({
    # 先算 Ke (權益成本)
    rf <- input$var_rf / 100
    rm <- input$var_rm / 100
    beta <- input$var_beta
    capm <- rf + beta * (rm - rf)
    
    # 讀取其他參數
    kd <- input$var_cost_debt / 100
    t <- input$var_tax_rate / 100
    we <- input$var_weight_equity / 100
    wd <- 1 - we # 債務佔比自動計算
    
    # 🧮 WACC 公式: (We * Ke) + (Wd * Kd * (1 - t))
    wacc <- (we * capm) + (wd * kd * (1 - t))
    
    valueBox(
      value = paste0(round(wacc * 100, 2), "%"),
      subtitle = "加權平均資本成本 (WACC)",
      icon = icon("calculator"),
      color = "purple"
    )
  })
  
  ### Calculator
  # 📌 反應式變數定義 ----------------------------------------------------------
  
  estimated_g <- reactiveVal(NULL)
  estimated_r_e <- reactiveVal(NULL)
  calculated_wacc <- reactiveVal(NULL)
  dcf_value_result <- reactiveVal(NULL)
  stock_price_estimate_val <- reactiveVal(NULL)
  
  # ==========================================
  # 🟢 這裡是你漏掉或貼錯位置的兩個重要資料抓取函數！
  # 必須放在 server 函數的「最外層」（不能包在任何 observe 裡面）
  # ==========================================
  scraped_debt <- reactive({
    req(d_balance_sheet())
    val <- select_clean_metric_row(d_balance_sheet(), "Total Debt")
    if (length(val) > 0 && !all(is.na(val))) return(as.numeric(na.omit(val))[1])
    return(0)
  })
  
  scraped_shares <- reactive({
    req(d_balance_sheet())
    val <- select_clean_metric_row(d_balance_sheet(), "Share Issued")
    if (length(val) > 0 && !all(is.na(val))) return(as.numeric(na.omit(val))[1])
    return(1)
  })
  # ==========================================
  
  # 1. 建立一個虛擬的觸發信號，初始值為 0
  run_calc_trigger <- reactiveVal(0)
  
  # 2. 情況 A：當使用者手動點擊「計算」按鈕時，信號 +1
  observeEvent(input$calc, {  # ⚠️ 請把 input$calc 替換成你 UI 中實際的按鈕 ID
    run_calc_trigger(run_calc_trigger() + 1)
  })
  
  # 3. 情況 B：當「財報資料」成功抓取回來時，信號自動 +1 (等同於幫使用者按了一次按鈕)
  observeEvent(d_cash_flow(), {   
    # 確保資料真的有抓到，而不是空值
    req(is.data.frame(d_cash_flow()), nrow(d_cash_flow()) > 0)
    
    # 資料載入完成，觸發計算！
    run_calc_trigger(run_calc_trigger() + 1)
  })
  
  # ==========================================
  # 🔌 啟動 DDM 估值模組 (包含 DCF 正向同步)
  # ==========================================
  ddm_results <- ddm_module_server(
    id = "ddm_app", 
    dcf_g = reactive(input$g_gordon) %>% debounce(500), 
    dcf_ke = reactive(input$wacc_re) %>% debounce(500) 
  )
  
  # ==========================================
  # 🔄 2. 當 DDM 的參數被手動修改時，反向更新 DCF 的輸入框 (反向同步)
  # ==========================================
  observeEvent(ddm_results$ddm_g(), {
    req(ddm_results$ddm_g())
    
    old_g <- input$g_gordon
    new_g <- ddm_results$ddm_g()
    
    # 🟢 檢查 NA 防當機
    if (is.null(old_g) || is.na(old_g) || is.na(new_g) || abs(new_g - old_g) > 1e-4) {
      updateNumericInput(session, "g_gordon", value = new_g)
    }
  })
  
  observeEvent(ddm_results$ddm_ke(), {
    req(ddm_results$ddm_ke())
    
    old_ke <- input$wacc_re
    new_ke <- ddm_results$ddm_ke()
    
    # 🟢 檢查 NA 防當機
    if (is.null(old_ke) || is.na(old_ke) || is.na(new_ke) || abs(new_ke - old_ke) > 1e-4) {
      updateNumericInput(session, "wacc_re", value = new_ke)
    }
  })
  
  # 🟢 呼叫 FCF 模組
  fcf_results <- fcf_projection_module_server(
    id = "mod_fcf", 
    d_balance_sheet = d_balance_sheet,
    d_income_statement = d_income_statement, 
    d_cash_flow = d_cash_flow,
    input_mode = reactive(input$dcf_mode),
    input_years = reactive(input$years),
    g_gordon = reactive(input$g_gordon),
    g_stage1 = reactive(input$g_stage1),
    yr_stage1 = reactive(input$yr_stage1),
    g_stage2 = reactive(input$g_stage2),
    
    # 🎯 這裡將 Advance 的進階參數傳入模組
    input_capex_rate = reactive(input$var_capex_rate),
    input_nwc_rate   = reactive(input$var_nwc_rate),
    input_manual_fcf = reactive(input$manual_fcf),
    
    calc_trigger = run_calc_trigger
  )
  
  # ==========================================
  # 🟢 渲染 FCF 進階參數的「系統歷史預設值」到 UI
  # ==========================================
  
  # 1. 渲染 CapEx 歷史比例
  output$txt_hist_capex <- renderUI({
    params <- fcf_results$hist_params() # 從模組抓取資料
    
    if(is.null(params) || is.na(params$capex_rate)) {
      return(HTML("<div style='color: gray; font-size: 13px; margin-bottom: 5px;'>⏳ 系統推算值：等待財報資料匯入...</div>"))
    }
    
    # 轉為百分比並加上藍色顯眼字體
    val <- round(params$capex_rate * 100, 2)
    HTML(paste0("<div style='color: #3c8dbc; font-size: 14px; margin-bottom: 5px;'>",
                "📊 系統歷史推算值：<b>", val, " %</b></div>"))
  })
  
  # 2. 渲染 ΔNWC 歷史比例
  output$txt_hist_nwc <- renderUI({
    params <- fcf_results$hist_params()
    
    if(is.null(params) || is.na(params$nwc_rate)) {
      return(HTML("<div style='color: gray; font-size: 13px; margin-bottom: 5px;'>⏳ 系統推算值：等待財報資料匯入...</div>"))
    }
    
    val <- round(params$nwc_rate * 100, 2)
    HTML(paste0("<div style='color: #3c8dbc; font-size: 14px; margin-bottom: 5px;'>",
                "📊 系統歷史推算值：<b>", val, " %</b></div>"))
  })
  
  # 🟢 需求 4：擷取 FCF 模組結果並顯示於 Calculator 同步狀態
  output$txt_fcf_sync_status <- renderPrint({
    df <- fcf_results$fcf_df()
    if (is.null(df)) {
      cat("尚未匯入財報資料，或正在等待計算...")
    } else {
      cat("✅ FCF 預測資料已同步！\n")
      cat("-------------------------\n")
      cat("第 1 年預測現金流:", df$FCF[1], "\n")
      cat("第", nrow(df), "年預測現金流:", df$FCF[nrow(df)], "\n")
      cat("模型狀態:", unique(df$Type)[1], "\n")
    }
  })
  
  # 📈 g 永續成長率估算 (全自動即時版) --------------------------------------------------
  observe({
    req(d_cash_flow(), d_income_statement(), d_balance_sheet(), input$g_growth_method)
    
    method <- input$g_growth_method
    
    # 1. 準備 FCF 歷史資料 (給 CAGR、Mean、Median 使用)
    fcf_vec <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    fcf_chrono <- rev(fcf_vec) 
    g_rate_raw <- diff(fcf_chrono) / abs(head(fcf_chrono, -1))
    g_rate <- g_rate_raw[is.finite(g_rate_raw)]
    g_rate <- g_rate[g_rate > -5 & g_rate < 5] 
    
    # 2. 🟢 新增：學理基本面推估法 (Fundamental) 的運算邏輯
    fund_res <- NULL
    if (method == "fundamental") {
      # a. NOPAT = EBIT * (1 - Tax)
      ebit <- select_clean_metric_row(d_income_statement(), "Operating Income")[1]
      tax_rate <- input$wacc_tax / 100 # 沿用使用者設定的稅率
      nopat <- ebit * (1 - tax_rate)
      
      # b. 投入資本 (Invested Capital) = 總資產 - (流動負債 - 短期帶息負債)
      total_assets <- select_clean_metric_row(d_balance_sheet(), "Total Assets")[1]
      curr_liab <- select_clean_metric_row(d_balance_sheet(), "Total Current Liabilities")[1]
      st_debt <- select_clean_metric_row(d_balance_sheet(), "Short Term Debt")[1]
      st_debt <- ifelse(is.na(st_debt), 0, st_debt)
      invested_capital <- total_assets - (curr_liab - st_debt)
      
      # c. 計算 ROIC 與 再投資率 (RR)
      roic <- nopat / invested_capital
      
      capex <- abs(select_clean_metric_row(d_cash_flow(), "Capital Expenditure")[1])
      depre <- select_clean_metric_row(d_cash_flow(), "Depreciation & Amortization")[1]
      delta_nwc <- select_clean_metric_row(d_cash_flow(), "Change in Working Capital")[1]
      
      capex <- ifelse(is.na(capex), 0, capex)
      depre <- ifelse(is.na(depre), 0, depre)
      delta_nwc <- ifelse(is.na(delta_nwc), 0, delta_nwc)
      
      reinvestment_rate <- (capex - depre + delta_nwc) / nopat
      
      # d. 學理 g = ROIC * RR
      fund_g <- reinvestment_rate * roic
      
      # 將結果打包供後續顯示
      fund_res <- list(g = round(fund_g * 100, 2), roic = roic, rr = reinvestment_rate, 
                       nopat = nopat, ic = invested_capital)
    }
    
    # 3. 根據方法取得最終數值
    val <- switch(method,
                  "fundamental" = if(!is.null(fund_res)) fund_res$g else NA,
                  "cagr" = { 
                    valid_fcf <- na.omit(fcf_chrono)
                    first_val <- head(valid_fcf, 1)
                    last_val <- tail(valid_fcf, 1)
                    n_years <- length(valid_fcf) - 1
                    if (first_val <= 0 || last_val <= 0 || n_years < 1) NA 
                    else round(((last_val / first_val)^(1 / n_years) - 1) * 100, 2)
                  },
                  "mean" = if(length(g_rate) > 0) round(mean(g_rate) * 100, 2) else NA,
                  "median" = if(length(g_rate) > 0) round(median(g_rate) * 100, 2) else NA,
                  "last_year" = {                    
                    newest <- fcf_vec[1]
                    previous <- fcf_vec[2]
                    if (is.na(newest) || is.na(previous) || previous == 0) NA 
                    else round(((newest - previous) / abs(previous)) * 100, 2)
                  },
                  "custom" = input$custom_g
    )
    
    # 防呆
    if (is.null(val) || is.na(val)) {
      showNotification("⚠️ 數據不足或異常，無法自動估算，請改用「自訂輸入」", type = "warning")
      estimated_g(NULL)
      return(NULL)
    }
    
    estimated_g(val)
    
    # 4. 更新結果文字與公式參數明細 UI
    output$g_result <- renderUI({
      method_name <- switch(method,
                            'fundamental' = '學理基本面推估 (Fundamental)',
                            'cagr' = '複合年均成長率 (CAGR)',
                            'mean' = '平均年增率',
                            'median' = '中位數年增率',
                            'last_year' = '最近一年變化',
                            'custom' = '自訂輸入')
      
      detail_html <- ""
      
      # 🟢 如果是 Fundamental，顯示深度拆解！
      if (method == "fundamental" && !is.null(fund_res)) {
        detail_html <- glue::glue("
          <div style='color: #7f8c8d; font-size: 14px; margin-top: 10px; border-top: 1px dashed #ccc; padding-top: 10px;'>
            <b>參數拆解：</b><br/>
            • 稅後淨營業利潤 (NOPAT)： $ {formatC(fund_res$nopat, format='f', big.mark=',', digits=0)}<br/>
            • 投入資本 (Invested Capital)： $ {formatC(fund_res$ic, format='f', big.mark=',', digits=0)}<br/>
            • 投入資本回報率 (ROIC)： {round(fund_res$roic * 100, 2)} %<br/>
            • 再投資率 (Reinvestment Rate)： {round(fund_res$rr * 100, 2)} %<br/>
            <br/><b>背後公式：</b> <br/>g = Reinvestment Rate × ROIC<br/>
            <span style='font-size:12px; color: #e74c3c;'>*註：若估算結果超過無風險利率，建議手動下修至貼近長期經濟成長率 (2%~3%) 較為穩健。</span>
          </div>
        ")
      } else if (method == "cagr" && !is.na(val)) {
        valid_fcf <- na.omit(fcf_chrono)
        detail_html <- glue::glue("
          <div style='color: #7f8c8d; font-size: 14px; margin-top: 10px; border-top: 1px dashed #ccc; padding-top: 10px;'>
            <b>參數拆解：</b><br/>
            • 最新期 FCF (FV)： $ {formatC(tail(valid_fcf, 1), format='f', big.mark=',', digits=0)}<br/>
            • 最早期 FCF (PV)： $ {formatC(head(valid_fcf, 1), format='f', big.mark=',', digits=0)}<br/>
            • 經歷年數 (n)： {length(valid_fcf) - 1} 年<br/>
            <br/><b>背後公式：</b> <br/>g = (FV / PV)<sup>(1/n)</sup> - 1
          </div>
        ")
      } else if (method == "last_year" && !is.na(val)) {
        detail_html <- glue::glue("
          <div style='color: #7f8c8d; font-size: 14px; margin-top: 10px; border-top: 1px dashed #ccc; padding-top: 10px;'>
            <b>參數拆解：</b><br/>
            • 最新期 FCF (T)： $ {formatC(fcf_vec[1], format='f', big.mark=',', digits=0)}<br/>
            • 前一期 FCF (T-1)： $ {formatC(fcf_vec[2], format='f', big.mark=',', digits=0)}<br/>
            <br/><b>背後公式：</b> <br/>g = (T - T<sub>-1</sub>) / |T<sub>-1</sub>|
          </div>
        ")
      } else if (method %in% c("mean", "median") && !is.na(val)) {
        detail_html <- glue::glue("
          <div style='color: #7f8c8d; font-size: 14px; margin-top: 10px; border-top: 1px dashed #ccc; padding-top: 10px;'>
            <b>參數拆解：</b><br/>
            • 資料來源：歷史各年度 YoY 自由現金流成長率集合<br/>
            • 雜訊處理：已排除 > 500% 或 < -500% 之極端值<br/>
            <br/><b>背後公式：</b> <br/>g = 取有效集合之{ifelse(method=='mean', '算術平均 (Mean)', '中位數 (Median)')}
          </div>
        ")
      }
      
      HTML(glue::glue("
        <div style='font-size: 16px; line-height: 1.6; padding: 15px; background-color: #fdfdfd; border: 1px solid #e0e0e0; border-radius: 6px; border-left: 4px solid #d35400;'>
          <span style='color: #d35400; font-size: 20px; font-weight: bold;'>
            ➤ 永續成長率估算 (g) = {val} %
          </span><br/>
          <span style='color: #2980b9; font-weight: bold;'>
            採用方法：{method_name}
          </span>
          {detail_html}
        </div>
      "))
    })
    
    # 5. 自動更新至輸入框
    if (is.null(isolate(input$g_gordon)) || abs(isolate(input$g_gordon) - val) > 1e-4) {
      updateNumericInput(session, "g_gordon", value = val)
    }
    if (is.null(isolate(input$g_stage1)) || abs(isolate(input$g_stage1) - val) > 1e-4) {
      updateNumericInput(session, "g_stage1", value = val)
    }
    
    output$ibx_estimated_g <- renderInfoBox({
      infoBox("估算 g 成長率", paste0(val, " %"), icon = icon("chart-line"),
              color = "purple", fill = TRUE)
    })
  })
  
  # ==========================================
  # 🟢 雙向綁定：偵測手動修改，自動切換為「自訂輸入」
  # ==========================================
  
  # 1. 監聽「永續成長法」的 g 被手動修改
  observeEvent(input$g_gordon, {
    req(input$g_gordon)
    sys_g <- isolate(estimated_g()) 
    
    # 🟢 終極防護：確保不是 NULL、不是 NA，再來算數學相減
    if (!is.null(sys_g) && !is.na(sys_g) && !is.na(input$g_gordon) && abs(input$g_gordon - sys_g) > 1e-4) {
      
      if (isolate(input$g_growth_method) != "custom") {
        updateSelectInput(session, "g_growth_method", selected = "custom")
        showNotification("💡 已偵測到手動修改，自動切換為「自訂輸入」模式", type = "message")
      }
      
      # 🟢 同樣加上防護
      custom_val <- isolate(input$custom_g)
      if (is.null(custom_val) || is.na(custom_val) || is.na(input$g_gordon) || abs(custom_val - input$g_gordon) > 1e-4) {
        updateNumericInput(session, "custom_g", value = input$g_gordon)
      }
    }
  }, ignoreInit = TRUE)
  
  # 2. 監聽「二階段成長法」的 g1 被手動修改
  observeEvent(input$g_stage1, {
    req(input$g_stage1)
    
    sys_g <- isolate(estimated_g())
    
    if (!is.null(sys_g) && input$g_stage1 != sys_g) {
      if (isolate(input$g_growth_method) != "custom") {
        updateSelectInput(session, "g_growth_method", selected = "custom")
        showNotification("💡 已偵測到手動修改，自動切換為「自訂輸入」模式", type = "message")
      }
      updateNumericInput(session, "custom_g", value = input$g_stage1)
    }
  }, ignoreInit = TRUE)
  
  # 🟢 需求 6：產業切換時自動帶入 Beta，並在標題顯示產業名稱
  observeEvent(input$industry_choice, {
    req(input$industry_choice)
    inds <- industry_standards[[input$industry_choice]]
    
    if (!is.null(inds)) {
      # 同步更新「數值」與「文字標題」
      updateNumericInput(session, "capm_beta", 
                         label = paste0("Beta (β) [套用產業: ", input$industry_choice, "]"),
                         value = inds$beta_avg)
      
      updateNumericInput(session, "capm_rm", value = inds$rm_avg)
    }
  })
  
  # 📈 CAPM 股東權益成本估算
  observeEvent(input$calc_capm, {
    Rf <- input$capm_rf / 100
    beta <- input$capm_beta
    Rm <- input$capm_rm / 100
    
    r_e_est <- Rf + beta * (Rm - Rf)
    estimated_r_e(r_e_est)
    
    # 🔁 自動更新至手動欄位 wacc_re
    updateNumericInput(session, "wacc_re", value = round(r_e_est * 100, 2))
    
    # 結果輸出
    output$capm_result <- renderUI({
      r_e <- estimated_r_e()
      if (is.null(r_e)) return(NULL)
      
      HTML(glue::glue(
        "<div style='font-size: 16px; line-height: 1.6;'>
        <span style='color: teal; font-size: 20px;'>
          ➤ 股東權益成本 (rₑ) = <b>{round(r_e * 100, 2)} %</b>
        </span><br/>
        <span style='color: gray;'>
          rₑ = Rf + β × (Rm - Rf)
        </span>
      </div>"
      ))
    })
  })
  
  # 🧮 WACC 計算
  observeEvent(input$calc_wacc, {
    req(d_balance_sheet())
    
    bs_data <- d_balance_sheet()
    equity <- select_clean_metric_row(bs_data, "Common Stock Equity")[1]
    debt <- select_clean_metric_row(bs_data, "Total Debt")[1]
    T <- input$wacc_tax / 100
    
    # 使用估算 or 手動輸入 rₑ
    r_e <- if (input$use_estimated_re && !is.null(estimated_r_e())) {
      estimated_r_e()
    } else {
      input$wacc_re / 100
    }
    
    r_d <- input$wacc_rd / 100
    total_capital <- equity + debt
    
    wacc <- (equity / total_capital) * r_e + (debt / total_capital) * r_d * (1 - T)
    calculated_wacc(wacc)
    
    wacc_percent <- round(wacc * 100, 2)
    
    # 自動套用至 DCF 區域
    if (input$dcf_mode == "gordon") {
      updateNumericInput(session, "wacc_gordon", value = wacc_percent)
    } else if (input$dcf_mode == "two_stage") {
      updateNumericInput(session, "wacc_stage1", value = wacc_percent)
      updateNumericInput(session, "wacc_stage2", value = wacc_percent)
    }
    
    showNotification(glue::glue("📌 已自動將 WACC {wacc_percent}% 套用至 DCF 折現率參數"), type = "message")
    
    # WACC 詳細結果
    output$wacc_result <- renderUI({
      wacc <- calculated_wacc()
      if (is.null(wacc)) return(NULL)
      
      HTML(glue::glue(
        "<div style='font-size: 16px; line-height: 1.6;'>
        <span style='color: steelblue;'>股東權益 (E)：</span> ${formatC(equity, format = 'f', big.mark = ',', digits = 0)}<br/>
        <span style='color: steelblue;'>總負債 (D)：</span> ${formatC(debt, format = 'f', big.mark = ',', digits = 0)}<br/>
        <span style='color: teal;'>股權成本 (rₑ)：</span> <b>{round(r_e * 100, 2)} %</b><br/>
        <span style='color: limegreen;'>負債成本 (rᵈ)：</span> <b>{round(r_d * 100, 2)} %</b><br/>
        <span style='color: orange;'>所得稅率 (T)：</span> <b>{input$wacc_tax} %</b><br/><br/>
        <span style='font-size: 20px; color: purple;'><b>➡️ WACC = {wacc_percent} %</b></span>
      </div>"
      ))
    })
    
    # InfoBoxes
    output$ibx_wacc <- renderInfoBox({
      infoBox("WACC", h3(paste0(wacc_percent, " %")), icon = icon("percent"), color = "aqua", fill = TRUE)
    })
  })
  
  # ==========================================
  # 🟢 獨立渲染的 InfoBox (App 啟動即顯示預設值，且即時連動)
  # ==========================================
  
  # 1. 股東權益成本 (rₑ)
  output$ibx_re <- renderInfoBox({
    # 讀取輸入框的值；如果剛啟動還沒抓到，就吃設定檔的預設值
    val_re <- input$wacc_re
    if (is.null(val_re)) val_re <- APP_DEFAULTS$wacc_re
    
    # 如果使用者有勾選「套用估算」，且 CAPM 已經算出來了，就優先顯示 CAPM 的值
    if (isTRUE(input$use_estimated_re) && !is.null(estimated_r_e())) {
      val_re <- estimated_r_e() * 100
    }
    
    infoBox("股東權益成本 (rₑ)", h3(paste0(round(val_re, 2), " %")), 
            icon = icon("chart-line"), color = "teal", fill = TRUE)
  })
  
  # 2. 負債成本 (rᵈ)
  output$ibx_rd <- renderInfoBox({
    # 即時讀取輸入框的值，防呆預設為設定檔的數值
    val_rd <- input$wacc_rd
    if (is.null(val_rd)) val_rd <- APP_DEFAULTS$wacc_rd
    
    infoBox("負債成本 (rᵈ)", h3(paste0(round(val_rd, 2), " %")), 
            icon = icon("university"), color = "lime", fill = TRUE)
  })
  
  # ==========================================
  # 📊 DCF 折現現金流軌跡圖
  # ==========================================
  output$plt_dcf_trajectory <- renderPlot({
    # 1. 取得未來預測的 FCF 資料 (來自 FCF 模組)
    proj_df <- fcf_results$fcf_df()
    
    if (is.null(proj_df) || nrow(proj_df) == 0) {
      plot.new()
      text(0.5, 0.5, "⚠️ 尚無預測資料，請確認模型參數設定", cex = 1.4)
      return()
    }
    
    n_years <- nrow(proj_df)
    
    # 2. 決定使用的 WACC 折現率陣列 (精確對應每一年的折現條件)
    wacc_array <- numeric(n_years)
    
    # 判斷使用者是否勾選「自動估算 WACC」
    if (input$use_calculated_wacc && !is.null(calculated_wacc())) {
      wacc_array <- rep(calculated_wacc(), n_years) # 全部使用系統 CAPM 算出的 WACC
    } else {
      # 使用手動輸入的 WACC
      if (input$dcf_mode == "gordon") {
        wacc_array <- rep(input$wacc_gordon / 100, n_years)
      } else {
        # 二階段模式：根據階段年數給予不同的 WACC
        s1 <- min(input$yr_stage1, n_years)
        s2 <- max(n_years - s1, 0)
        wacc_array <- c(rep(input$wacc_stage1 / 100, s1), rep(input$wacc_stage2 / 100, s2))
      }
    }
    
    # 3. 🧮 核心數學：計算每一年的折現係數與 DCF (現值)
    # 折現公式： PV = FCF / (1 + WACC)^t 
    # 使用 cumprod (累乘) 來確保即使二階段 WACC 變動，折現率也能正確遞延
    discount_factors <- cumprod(1 + wacc_array)
    proj_df$DCF <- round(proj_df$FCF / discount_factors, 2)
    
    # 4. 資料塑形 (寬轉長，讓 ggplot 可以同時畫出兩條線)
    plot_df <- data.frame(
      Year = rep(proj_df$Year, 2),
      Value = c(proj_df$FCF, proj_df$DCF),
      Metric = factor(rep(c("預測現金流 (未折現 FCF)", "折現後真實價值 (DCF)"), each = n_years),
                      levels = c("預測現金流 (未折現 FCF)", "折現後真實價值 (DCF)"))
    )
    
    # 5. 🎨 繪製專業對比圖表
    ggplot(plot_df, aes(x = Year, y = Value, color = Metric, linetype = Metric)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      # 在點的上方加上千分位數字標籤
      geom_text(aes(label = formatC(Value, format = "f", big.mark = ",", digits = 0)), 
                vjust = -1.5, size = 4.5, show.legend = FALSE, fontface = "bold") +
      
      # 顏色設定：未折現的放背景色(灰色/虛線)，真實價值的用亮色(紅色/實線)凸顯
      scale_color_manual(values = c("預測現金流 (未折現 FCF)" = "#95a5a6",  
                                    "折現後真實價值 (DCF)" = "#e74c3c")) +       
      scale_linetype_manual(values = c("預測現金流 (未折現 FCF)" = "dashed", 
                                       "折現後真實價值 (DCF)" = "solid")) +
      
      theme_minimal(base_size = 14) +
      labs(title = "📉 時間價值展現：自由現金流 (FCF) vs 現值 (DCF)", 
           x = "預測年份", 
           y = "金額") +
      theme(
        legend.position = "top", 
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16)
      ) +
      # 擴大上方 Y 軸空間，避免數字標籤被切到
      expand_limits(y = max(plot_df$Value, na.rm = TRUE) * 1.2)
  })
  
  # ==========================================
  # 📌 專屬功能：DCF 計算前的「即時 FCF 預覽」資料生成器
  # ==========================================
  generate_fcf_plot <- reactive({
    req(d_cash_flow()) # 確保已經成功抓到財報
    
    # 抓取歷史 FCF 並以最新一年為起點
    fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    fcf_history <- na.omit(fcf_history)
    if (length(fcf_history) == 0) return(NULL)
    
    fcf_start <- fcf_history[1]
    n_years <- input$years
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # 建立一個清單來存放繪圖資料
    df_list <- list()
    df_list[[1]] <- data.frame(Year = current_year, FCF = fcf_start, Type = "預設") 
    
    # 根據 UI 當下選擇的模型，即時推算未來 FCF
    if (input$dcf_mode == "gordon") {
      g <- input$g_gordon / 100
      future_fcf <- fcf_start * (1 + g)^(1:n_years)
      df_list[[2]] <- data.frame(Year = (current_year + 1):(current_year + n_years), 
                                 FCF = future_fcf, Type = "Gordon 預測")
      
    } else if (input$dcf_mode == "two_stage") {
      g1 <- input$g_stage1 / 100
      g2 <- input$g_stage2 / 100
      yr1 <- input$yr_stage1
      
      # 防呆：確保第一階段年數不超過總年數
      yr1 <- min(yr1, n_years)
      yr2 <- max(n_years - yr1, 0)
      
      if (yr1 > 0) {
        fcf_s1 <- fcf_start * cumprod(rep(1 + g1, yr1))
        df_list[[2]] <- data.frame(Year = (current_year + 1):(current_year + yr1), 
                                   FCF = fcf_s1, Type = "第一階段")
      }
      if (yr2 > 0) {
        last_s1 <- if(yr1 > 0) tail(fcf_s1, 1) else fcf_start
        fcf_s2 <- last_s1 * cumprod(rep(1 + g2, yr2))
        df_list[[3]] <- data.frame(Year = (current_year + yr1 + 1):(current_year + n_years), 
                                   FCF = fcf_s2, Type = "第二階段")
      }
    }
    
    # 將所有階段合併成一張表並回傳
    return(do.call(rbind, df_list))
  })
  
  # ==========================================
  # 📌 DCF 預設 FCF 預測圖 (參數即時預覽)
  # ==========================================
  output$dft_fcf_plot <- renderPlot({
    df <- generate_fcf_plot() # 呼叫剛剛寫好的預覽函數
    
    # 如果還沒抓到資料，顯示提示文字
    if (is.null(df) || nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "⏳ 等待財報資料匯入，以顯示 FCF 即時預覽...", cex = 1.4)
      return()
    }
    
    ggplot(df, aes(x = Year, y = FCF, linetype = Type)) +
      geom_line(size = 1.2, color = "steelblue") +
      geom_point(aes(color = FCF < 0), size = 3) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "steelblue"), guide = "none") +
      scale_linetype_manual(values = c(
        "預設" = "solid",           # 🟢 起點改用實線，視覺更清晰
        "Gordon 預測" = "dotted",
        "第一階段" = "dotted",
        "第二階段" = "twodash"
      )) +
      theme_minimal(base_size = 14) +
      labs(title = "📉 自由現金流預測圖 (計算前即時預覽)", 
           subtitle = "💡 圖表會根據左側參數設定即時變動",
           x = "預測年份", y = "FCF (USD)") +
      theme(plot.title = element_text(size = 14, face = "bold"), 
            plot.subtitle = element_text(color = "#7f8c8d", size = 11),
            legend.position = "top", legend.title = element_blank())
  })
  
  # 💰 DCF 模型計算 ------------------------------------------------------------
  observeEvent(input$calc, {
    req(input$sc, input$dcf_mode)
    
    fcf_history <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    if (all(is.na(fcf_history))) {
      showNotification("⚠️ 無有效自由現金流資料", type = "error")
      return(NULL)
    }
    
    fcf_start <- head(fcf_history, 1)
    n <- input$years
    base_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # ⏬ 折現率
    discount_rate <- if (input$use_calculated_wacc && !is.null(calculated_wacc())) {
      calculated_wacc()
    } else(
      if (input$dcf_mode == "gordon") { input$wacc_gordon / 100 }
      else input$wacc_stage1 / 100 )
    
    # ⏬ 股數
    share_outstanding <- as.numeric(select_clean_metric_row(d_balance_sheet(), "Share Issued")[1])
    
    ## 自定義DCF
    dcf_value <- NA
    
    # Gordon 模型 ------------------------------------------------------------
    if (input$dcf_mode == "gordon") {
      g <- input$g_gordon / 100
      if (g >= discount_rate) {
        showNotification("❌ 永續成長率 g 必須小於折現率", type = "error")
        return(NULL)
      }
      
      fcf_forecast <- fcf_start * (1 + g)^(0:(n - 1))
      pv_forecast <- sum(fcf_forecast / (1 + discount_rate)^(1:n))
      terminal_value <- fcf_forecast[n] * (1 + g) / (discount_rate - g)
      pv_terminal <- terminal_value / (1 + discount_rate)^n
      dcf_value <- pv_forecast + pv_terminal
    }
    
    # Two-Stage 模型 ---------------------------------------------------------
    if (input$dcf_mode == "two_stage") {
      g1 <- input$g_stage1 / 100
      g2 <- input$g_stage2 / 100
      r1 <- input$wacc_stage1 / 100
      r2 <- input$wacc_stage2 / 100
      yr_stage1 <- input$yr_stage1
      
      if (yr_stage1 <= 0 || yr_stage1 >= n) {
        showNotification("⚠️ 第一階段年數無效", type = "error")
        return(NULL)
      }
      
      fcf_stage1 <- fcf_start * cumprod(rep(1 + g1, yr_stage1))
      fcf_stage2 <- fcf_stage1[length(fcf_stage1)] * cumprod(rep(1 + g2, n - yr_stage1))
      pv_stage1 <- sum(fcf_stage1 / (1 + discount_rate)^(1:yr_stage1))
      pv_stage2 <- sum(fcf_stage2 / (1 + discount_rate)^((yr_stage1 + 1):n))
      terminal_value <- fcf_stage2[length(fcf_stage2)] * (1 + g2) / (discount_rate - g2)
      pv_terminal <- terminal_value / (1 + discount_rate)^n
      dcf_value <- pv_stage1 + pv_stage2 + pv_terminal
    }
    
    dcf_value_result(dcf_value)
    
    # ==========================================
    # 🚨 財報數據缺漏檢查與動態警示模組
    # ==========================================
    output$ui_data_validation <- renderUI({
      req(d_balance_sheet(), d_cash_flow())
      
      # 1. 檢查現金 (🟢 呼叫我們自己寫的輔助函數)
      latest_cash <- get_latest_cash_position(d_cash_flow())
      
      # 2. 檢查負債
      val_debt <- select_clean_metric_row(d_balance_sheet(), "Total Debt")
      scraped_debt <- if (length(val_debt) > 0 && !all(is.na(val_debt))) as.numeric(na.omit(val_debt))[1] else NA
      
      # 3. 檢查 FCF
      val_fcf <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
      scraped_fcf <- if (length(val_fcf) > 0 && !all(is.na(val_fcf))) as.numeric(na.omit(val_fcf))[1] else NA
      
      # 統整缺漏項目
      missing <- c()
      if (is.na(scraped_fcf)) missing <- c(missing, "自由現金流 (FCF)")
      if (is.na(latest_cash)) missing <- c(missing, "現金與約當現金 (Cash)")
      if (is.na(scraped_debt)) missing <- c(missing, "總負債 (Total Debt)")
      
      # 如果有缺漏，就彈出紅色警告與輸入框
      if (length(missing) > 0) {
        box(title = "⚠️ 財報數據缺漏警示！請手動補齊", status = "danger", solidHeader = TRUE, width = 12,
            p("系統無法自動從財報中抓取以下數據：", tags$b(paste(missing, collapse = "、"), style="color:#d9534f; font-size: 16px;")),
            fluidRow(
              # 動態產生對應的輸入框 (只顯示缺少的)
              if(is.na(scraped_fcf)) column(4, numericInput("manual_fcf", "手動輸入最新一期 FCF:", value = NA)),
              if(is.na(latest_cash)) column(4, numericInput("manual_cash", "手動輸入最新一期 Cash:", value = NA)),
              if(is.na(scraped_debt)) column(4, numericInput("manual_debt", "手動輸入最新一期 Debt:", value = NA))
            ),
            helpText("請查閱公司最新財報後在此填入數值，並點擊按鈕更新估值。")
        )
      } else {
        NULL # 資料齊全就不顯示任何東西
      }
    })
    
    # 💵 股價估值 -----------------------------
    
    # 1. 取得爬蟲 Cash (🟢 呼叫我們自己寫的輔助函數)
    latest_cash <- get_latest_cash_position(d_cash_flow())
    
    # 2. 取得爬蟲 Debt
    temp_debt <- select_clean_metric_row(d_balance_sheet(), "Total Debt")
    scraped_debt <- if (length(temp_debt) > 0 && !all(is.na(temp_debt))) as.numeric(na.omit(temp_debt))[1] else NA
    
    # 🟢 判斷：優先使用手動輸入值
    latest_debt <- if (!is.null(input$manual_debt) && !is.na(input$manual_debt)) {
      input$manual_debt
    } else if (!is.na(scraped_debt)) {
      scraped_debt
    } else {
      0 # 終極防呆
    }
    
    # 3. 計算 企業總價值 (EV) 與 股權價值 (Equity Value)
    enterprise_value <- as.numeric(dcf_value)[1] # 確保是單一數字
    
    # 公式： Equity Value = EV + 最新現金 - 最新負債
    equity_value <- enterprise_value + latest_cash - latest_debt
    
    # 4. 計算並輸出每股合理價 (Intrinsic Value)
    if (!is.na(equity_value) && !is.na(share_outstanding) && share_outstanding > 0) {
      stock_price_estimate_val(equity_value / share_outstanding)
    } else {
      stock_price_estimate_val(NULL)
    }
    # -------------------------------------------------------------------------
    
    # 顯示估值結果 -----------------------------------------------------------
    output$vtxt_dcf_results <- renderText({
      if (is.na(dcf_value_result())) {
        return("⚠️ DCF 計算失敗")
      }
      
      # 基礎結果
      dcf_value <- dcf_value_result()
      msg <- glue::glue("企業總價值 (Enterprise Value)：${round(dcf_value, 2)}")
      
      estimated_price <- stock_price_estimate_val()
      if (!is.null(estimated_price) && !is.na(estimated_price) && !is.na(share_outstanding)) {
        msg <- glue::glue("{msg}\n 股東權益價值 (Equity Value)：${round(estimated_price * share_outstanding, 2)}")
        msg <- glue::glue("{msg}\n 最終每股合理價：${round(estimated_price, 2)}")
      } else {
        msg <- glue::glue("{msg}\n⚠️ 股數資訊無效，無法估算每股價格")
      }
      
      # 使用 generate_fcf_plot() 的結果
      fcf_df <- tryCatch(generate_fcf_plot(), error = function(e) NULL)
      fcf_values <- if (!is.null(fcf_df)) {
        paste(round(fcf_df$FCF, 2), collapse = ", ")
      } else {
        "⚠️ 無法取得預測 FCF"
      }
      
      params[["🕒 估值時間"]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # 加入使用參數資訊
      params <- list(
        "📌 估值模式" = input$dcf_mode,
        "📉 自由現金流預測年數" = input$years,
        "💵 預測 FCF" = fcf_values,
        "✅ 採用估算 WACC" = if (input$use_calculated_wacc) "是" else "否"
      )
      
      if (input$dcf_mode == "gordon") {
        params[["📈 永續成長率 g"]] <- paste0(input$g_gordon, " %")
        params[["🔻 折現率 WACC"]] <- paste0(input$wacc_gordon, " %")
      } else if (input$dcf_mode == "two_stage") {
        params[["📈 第一階段成長率 g₁"]] <- paste0(input$g_stage1, " %")
        params[["📈 第二階段成長率 g₂"]] <- paste0(input$g_stage2, " %")
        params[["🔻 第一階段 WACC₁"]] <- paste0(input$wacc_stage1, " %")
        params[["🔻 第二階段 WACC₂"]] <- paste0(input$wacc_stage2, " %")
        params[["📆 第一階段預測年數"]] <- input$yr_stage1
      }
      
      # 組合顯示文字
      param_text <- paste0(
        "\n\n🔧 使用參數一覽：\n",
        paste(names(params), params, sep = "：", collapse = "\n")
      )
      
      return(paste0(msg, param_text))
    })
    
    cat("\n=== 估值除錯資訊 ===\n",
        "FCF 起點:", fcf_start, "\n",
        "DCF 企業價值:", dcf_value, "\n",
        "現金:", latest_cash, "\n",
        "負債:", latest_debt, "\n",
        "股權價值:", equity_value, "\n",
        "流通在外股數:", share_outstanding, "\n",
        "====================\n")
  })
  
  # ==========================================
  # 📊 敏感度分析矩陣 (支援 Gordon 與 Two-Stage 雙模式自動切換)
  # ==========================================
  output$dcf_sensitivity_table <- renderTable({
    # 確保基礎資料已存在
    req(fcf_results$fcf_df(), input$calc) 
    
    # 1. 取得當前核心參數 (🌟 根據模式動態切換)
    
    # 📌 決定 WACC 基準點
    base_wacc <- if (isTRUE(input$use_calculated_wacc) && !is.null(calculated_wacc())) {
      calculated_wacc() * 100  # 系統估算值為小數，需轉為 %
    } else if (input$dcf_mode == "gordon") {
      input$wacc_gordon        # Gordon 模式的手動 WACC
    } else {
      input$wacc_stage1        # 二階段模式，以第一階段 WACC 為主
    }
    
    # 📌 決定永續成長率 (g) 基準點
    base_g <- if (input$dcf_mode == "gordon") {
      input$g_gordon           # Gordon 模式的 g
    } else {
      input$g_stage2           # 二階段模式的終端永續成長率 (g2)
    }
    
    # 🛡️ 終極防護罩：確保有數值，防止 App 崩潰
    if (is.null(base_wacc) || is.na(base_wacc) || length(base_wacc) != 1) base_wacc <- 10
    if (is.null(base_g) || is.na(base_g) || length(base_g) != 1) base_g <- 3
    
    # 2. 取得估算所需的其他零件 (債務、現金、股數)
    total_debt <- isolate(scraped_debt())
    cash <- isolate(get_latest_cash_position(d_cash_flow()))
    shares <- isolate(scraped_shares())
    
    # 3. 取得最後一期預測 FCF (用於算 TV)
    fcf_df <- fcf_results$fcf_df()
    n_years <- input$years
    
    # 防呆：確保行數足夠
    req(nrow(fcf_df) >= n_years + 1)
    fcf_n <- fcf_df$FCF[n_years + 1] 
    
    # 4. 取得預測期 FCF 的折現現值 (PV of Stage 1)
    pv_stage1 <- sum(fcf_df$FCF[2:(n_years+1)] / (1 + base_wacc/100)^(1:n_years))
    
    # 5. 定義變動級距 (WACC ± 1%, g ± 1%)
    wacc_range <- seq(base_wacc + 1, base_wacc - 1, length.out = 5)
    g_range <- seq(base_g - 1, base_g + 1, length.out = 5)
    
    # 6. 建立矩陣
    sens_matrix <- matrix(NA, nrow = 5, ncol = 5)
    rownames(sens_matrix) <- paste0("WACC ", round(wacc_range, 1), "%")
    colnames(sens_matrix) <- paste0("g ", round(g_range, 1), "%")
    
    for (i in 1:5) { 
      for (j in 1:5) { 
        w <- wacc_range[i] / 100
        g_val <- g_range[j] / 100
        
        # 排除 WACC <= g 的不合理情況
        if (w <= g_val) {
          sens_matrix[i, j] <- NA
        } else {
          # 計算該組合下的 TV (Terminal Value)
          tv <- (fcf_n * (1 + g_val)) / (w - g_val)
          pv_tv <- tv / (1 + w)^n_years
          
          # 計算每股合理價值
          ev <- pv_stage1 + pv_tv
          eq_val <- ev - total_debt + cash
          sens_matrix[i, j] <- eq_val / shares
        }
      }
    }
    
    # 7. 格式化表格輸出
    df_sens <- as.data.frame(sens_matrix)
    df_sens <- cbind(WACC_Rate = rownames(sens_matrix), df_sens)
    return(df_sens)
    
  }, digits = 2, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c', na = "無效 (W<g)")
  
  ### 📦 顯示估算後股價 InfoBox ---------------------------------------------------
  
  output$ibx_enterprise_value_dcf <- renderInfoBox({
    dcf <- dcf_value_result()
    infoBox(
      title = "企業估值（DCF）",
      value = if (is.null(dcf) || is.na(dcf)) "N/A" else format_dollar_abbr(dcf),
      icon = icon("building"),
      color = "purple",
      fill = TRUE
    )
  })
  
  output$ibx_stock_value_dcf <- renderInfoBox({
    price <- stock_price_estimate_val()
    infoBox(
      title = "每股估值（DCF）",
      value = if (is.null(price) || is.na(price)) "N/A" else paste0("$", round(price, 2)),
      icon = icon("money-bill-wave"),
      color = "maroon",
      fill = TRUE
    )
  })
  
  ## 🟢 修正與優化後的 DCF 參數明細
  output$vtxt_dcf_setting_details <- renderUI({
    # 確保必要的 input 存在
    req(input$dcf_mode, input$years)
    
    # 1. 模型模式文字敘述 (修正原程式碼 mode_text 變數錯誤)
    mode_display <- switch(input$dcf_mode,
                           "gordon" = "永續成長法 (Gordon Growth)",
                           "two_stage" = "二階段成長法 (Two-Stage Growth)")
    
    # 2. 折現率 WACC 數值判定
    # 優先顯示估算的 WACC，若未開啟則顯示手動值
    wacc_val <- if (input$use_calculated_wacc && !is.null(calculated_wacc())) {
      paste0(round(calculated_wacc() * 100, 2), "% (系統估算)")
    } else {
      if (input$dcf_mode == "gordon") paste0(input$wacc_gordon, "% (手動)") 
      else paste0(input$wacc_stage1, "% (S1) / ", input$wacc_stage2, "% (S2)")
    }
    
    # 3. 組合簡約風介面 (去除標籤與花俏顏色)
    tags$div(
      style = "padding: 15px; background-color: #fcfcfc; border: 1px solid #eee; border-radius: 5px; color: #444; font-size: 14px; line-height: 1.8;",
      
      # 第一行：模式與年數
      fluidRow(
        column(6, tags$b("🔹 評價模式："), mode_display),
        column(6, tags$b("🔹 預測年數："), input$years, " 年")
      ),
      
      # ==========================================
      # 第二行：成長率 (左邊) 與 折現率 (右邊) 並排
      # ==========================================
      fluidRow(
        
        # ⬅️ 左半邊 (width = 6)：根據模式顯示成長率細節
        column(6, 
               if (input$dcf_mode == "gordon") {
                 tags$div(style = "margin-bottom: 5px;", 
                          tags$b("📈 永續成長率 (g)："), paste0(input$g_gordon, " %"))
               } else {
                 tagList(
                   tags$div(style = "margin-bottom: 5px;", 
                            tags$b("📈 第一階段 (g₁)："), paste0(input$g_stage1, " % (維持 ", input$yr_stage1, " 年)")),
                   tags$div(style = "margin-bottom: 5px;", 
                            tags$b("📈 第二階段 (g₂)："), paste0(input$g_stage2, " %"))
                 )
               }
        ),
        
        # ➡️ 右半邊 (width = 6)：折現率資訊
        column(6, 
               tags$div(style = "margin-bottom: 5px;", 
                        tags$b("🔹 折現率 (WACC)："), tags$span(style = "color: #2c3e50;", wacc_val))
        )
        
      ),
      
      # 第四行：補充資訊 (CAPM 來源)
      tags$div(
        style = "margin-top: 10px; padding-top: 10px; border-top: 1px dashed #ddd; font-size: 12px; color: #888;",
        paste0("註：系統目前採用的 Beta 為 ", 
               if (!is.null(industry_standards[[input$industry_choice]]$beta_avg)) industry_standards[[input$industry_choice]]$beta_avg else input$capm_beta,
               "；無風險利率為 ", input$capm_rf, "%。")
      )
    )
  })
  
  # ==========================================
  # 🔁 DDM 重設按鈕邏輯
  # ==========================================
  observeEvent(input$reset_ddm, {
    
    # 🟢 注意：因為這些輸入框在 UI 是交給模組管理的，所以 ID 必須加上 "ddm_app-" 前綴！
    updateNumericInput(session, "ddm_app-d0", value = APP_DEFAULTS$ddm_d0)
    updateNumericInput(session, "ddm_app-g", value = APP_DEFAULTS$ddm_g)
    updateNumericInput(session, "ddm_app-ke", value = APP_DEFAULTS$ddm_ke)
    
    # 彈出小提示通知使用者
    showNotification("🔁 DDM 模型參數已回復系統預設值", type = "message")
  })
  
  observeEvent(input$reset_dcf, {
    updateRadioButtons(session, "dcf_mode", selected = APP_DEFAULTS$dcf_mode)
    
    # Gordon Growth 模式預設值
    updateNumericInput(session, "g_gordon", value = APP_DEFAULTS$g_gordon)
    updateNumericInput(session, "wacc_gordon", value = APP_DEFAULTS$wacc_gordon)
    
    # Two-Stage 預設值
    updateNumericInput(session, "g_stage1", value = APP_DEFAULTS$g_stage1)
    updateNumericInput(session, "g_stage2", value = APP_DEFAULTS$g_stage2)
    updateNumericInput(session, "wacc_stage1", value = APP_DEFAULTS$wacc_stage1)
    updateNumericInput(session, "wacc_stage2", value = APP_DEFAULTS$wacc_stage2)
    updateNumericInput(session, "yr_stage1", value = APP_DEFAULTS$yr_stage1)
    
    # 通用欄位
    updateNumericInput(session, "years", value = APP_DEFAULTS$years)
    updateCheckboxInput(session, "use_calculated_wacc", value = APP_DEFAULTS$use_calc_wacc)
    
    showNotification("🔁 所有 DCF 模型欄位已回復系統預設值", type = "message")
  })
  
  # 定義一個 ReactiveVal 來存 Python 抓回來的資料
  python_data <- reactiveVal(NULL)
  
  # ==========================================
  # 🐍 執行 Python 深度抓取邏輯 (修正版)
  # ==========================================
  # 1. 建立一個存儲三張表的 ReactiveVal
  python_full_data <- reactiveVal(list())
  
  observeEvent(input$btn_expand_all, {
    # 🎯 需求實現：優先抓搜尋框，若無則抓 APP_DEFAULTS 的預設代碼
    current_ticker <- if(!is.null(input$sc) && input$sc != "") {
      toupper(input$sc)
    } else {
      # 確保你的 default_config.R 裡有定義 stock_price
      APP_DEFAULTS$stock_price 
    }
    
    withProgress(message = paste('🐍 正在執行 Python 三表深度同步:', current_ticker), value = 0.5, {
      tryCatch({
        reticulate::source_python("deep_scraper.py")
        
        # 執行 Python 函數
        results <- scrape_all_financials(current_ticker)
        
        if (!is.null(results)) {
          python_full_data(results) # 存入全域 reactive 變數
          showNotification(paste("✅", current_ticker, "深度資料同步完成！"), type = "message")
        }
      }, error = function(e) {
        showNotification(paste("❌ Python 錯誤:", e$message), type = "error")
      })
    })
  })
  
  # 2. 根據切換按鈕顯示對應的表格
  output$tb_python_results <- renderDataTable({
    req(python_full_data(), input$py_table_choice)
    df <- python_full_data()[[input$py_table_choice]]
    
    if (is.null(df)) {
      return(data.frame(Status = "等待深度抓取資料中..."))
    }
    
    datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  # ==========================================
  # 📊 顯示抓取結果
  # ==========================================
  output$tb_python_scrape <- renderDataTable({
    req(python_data())
    datatable(python_data(), 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # 🧠 顯示搜尋結果與歷史 ------------------------------------------------------
  values <- reactiveValues(recentsearch = c())
  
  observeEvent(input$search, {
    req(input$sc)
    ticker <- toupper(input$sc)
    
    # A. 顯示公司產業資訊
    output$search_results <- renderText({
      res <- get_yahoo_industry(ticker)
      res$display_text
    })
    
    # B. 按下搜尋後才寫入歷史 (且不重複)
    if (!(ticker %in% values$recentsearch)) {
      values$recentsearch <- head(c(ticker, values$recentsearch), 5)
      output$recentsearch <- renderText({ paste(values$recentsearch, collapse = ", ") })
    }
  })
  
  
  ### 📤 下載分析報告 ------------------------------------------------------------
  output$download_report <- downloadHandler(
    filename = function() paste0("YNow_Report_", toupper(input$sc), "_", Sys.Date(), ".html"),
    content = function(file) {
      tryCatch({
        showNotification("正在生成完整分析報告，請稍候...", type = "message")
        
        # 1. 複製模板到暫存區
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
        
        # 2. 🛑 解決記憶體崩潰的關鍵：將圖表先存成實體圖片！
        plot_path <- NA
        if (exists("fcf_results") && !is.null(fcf_results$fcf_plot_obj())) {
          p <- fcf_results$fcf_plot_obj()
          plot_path <- file.path(tempdir(), "fcf_plot_temp.png")
          # 用 ggsave 把圖表存成 png 檔案
          ggsave(plot_path, plot = p, width = 8, height = 5, dpi = 300)
        }
        
        # 3. 執行 R Markdown 渲染
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          params = list(
            stock_code = toupper(input$sc),
            company_name = isolate(input$txt_corpname),
            
            # --- 估值資料 ---
            current_price = "待串接", 
            dcf_value = "待串接",     
            margin_of_safety = NA, 
            
            # 🟢 傳遞圖片路徑，而不是整個圖表物件！
            fcf_plot_path = plot_path,
            
            # --- 原本的資料 ---
            kpi = "請參考儀表板",
            warnings = "目前無重大異常警訊",
            summary_df = summary_data(),          
            income_df = d_income_statement(),     
            balance_df = d_balance_sheet(),       
            cashflow_df = d_cash_flow()           
          ),
          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        showNotification(paste("報告生成失敗:", e$message), type = "error")
      })
    }
  )
  
  output$today <- renderText({ format(Sys.Date(), "%Y/%m/%d") })
}
