# ==========================================
# server.R - 後端邏輯與資料運算 (專業財務修正版)
# ==========================================

server <- function(input, output, session) {
  
  # ==========================================
  # 🗄️ 全域資料容器 (儲存爬蟲結果)
  # ==========================================
  summary_data <- reactiveVal(NULL)
  scraped_financials <- reactiveVal(NULL)
  is_expanded <- reactiveVal(FALSE) 
  
  values <- reactiveValues(recentsearch = c())
  corp_industry_text <- reactiveVal("等待搜尋...")
  
  # 初始值設為 NULL，避免一開啟 App 就自動執行爬蟲
  current_ticker <- reactiveVal(NULL)
  
  # ==========================================
  # 🚀 雙按鈕監聽：確保左右兩個搜尋框獨立運作，互不覆寫
  # ==========================================
  observeEvent(input$btn_search, {
    req(input$txt_search)
    current_ticker(toupper(trimws(input$txt_search)))
  })
  
  observeEvent(input$search, {
    req(input$sc)
    current_ticker(toupper(trimws(input$sc)))
  })
  
  # ==========================================
  # 🌐 核心爬蟲：只要中央大腦的代碼改變，就自動執行完整抓取
  # ==========================================
  observeEvent(current_ticker(), {
    req(current_ticker()) 
    stock_code <- current_ticker()
    
    withProgress(message = paste('🚀 正在獲取', stock_code, '的最新數據...'), value = 0, {
      tryCatch({
        incProgress(0.2, detail = "正在讀取 Summary 頁面...")
        sum_df <- get_summary_data(stock_code)
        summary_data(sum_df)
        
        ind_info <- get_yahoo_industry(stock_code)
        if (!is.null(ind_info)) corp_industry_text(ind_info$display_text)
        
        if (!(stock_code %in% values$recentsearch)) {
          values$recentsearch <- head(c(stock_code, values$recentsearch), 5)
        }
        
        incProgress(0.5, detail = "正在展開深度財報明細 (快取加速中)...")
        res <- cached_scrape_financials(stock_code)
        scraped_financials(res)
        
        is_expanded(FALSE)
        updateActionButton(session, "btn_expand_all", label = "Expand All", icon = icon("expand"))
        
        incProgress(0.9, detail = "數據同步完成！✅")
        
      }, error = function(e) {
        showNotification(paste("❌ 獲取資料失敗，請確認代碼。錯誤:", e$message), type = "error")
      })
    })
  })
  
  # ==========================================
  # 📊 1. 基本資訊與 Summary 介面輸出
  # ==========================================
  render_corpname_logic <- function() {
    if (is.null(summary_data())) return("")
    name <- attr(summary_data(), "company_name")
    if (is.null(name) || is.na(name) || name == "") return(paste("Stock:", current_ticker())) else return(name)
  }
  
  output$txt_corpname <- renderText({ render_corpname_logic() })
  output$search_results <- renderText({ corp_industry_text() })
  output$recentsearch <- renderText({ paste(values$recentsearch, collapse = ", ") })
  output$today <- renderText({ format(Sys.Date(), "%Y/%m/%d") })
  
  output$ibx_stockprice <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "Previous Close" %in% df$Item) df$Value[df$Item == "Previous Close"] else "N/A"
    infoBox("Previous Close", val, icon = icon("chart-line"), color = "purple")
  })
  
  output$ibx_marketcap <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "Market Cap (intraday)" %in% df$Item) df$Value[df$Item == "Market Cap (intraday)"] else "N/A"
    infoBox("Market Cap", val, icon = icon("globe"), color = "blue")
  })
  
  output$ibx_EPS <- renderInfoBox({
    df <- summary_data()
    val <- if(!is.null(df) && "EPS (TTM)" %in% df$Item) df$Value[df$Item == "EPS (TTM)"] else "N/A"
    infoBox("EPS (TTM)", val, icon = icon("dollar-sign"), color = "green")
  })
  
  output$tbFinanceSummary <- renderDataTable({
    req(summary_data())
    datatable(summary_data(), options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = TRUE)
  })
  
  # ==========================================
  # 📑 2. 三大財報資料分發與顯示
  # ==========================================
  observeEvent(input$btn_expand_all, {
    new_state <- !is_expanded()
    is_expanded(new_state)
    if (new_state) {
      updateActionButton(session, "btn_expand_all", label = "Compress (切換回基本版)", icon = icon("compress"))
      showNotification("✅ 已切換至深度展開明細！", type = "message")
    } else {
      updateActionButton(session, "btn_expand_all", label = "Expand All", icon = icon("expand"))
      showNotification("已切換回精簡版報表", type = "message")
    }
  })
  
  d_income_statement <- reactive({ req(scraped_financials()); return(scraped_financials()[["Income Statement"]]$expanded) })
  d_balance_sheet <- reactive({ req(scraped_financials()); return(scraped_financials()[["Balance Sheet"]]$expanded) })
  d_cash_flow <- reactive({ req(scraped_financials()); return(scraped_financials()[["Cash Flow"]]$expanded) })
  
  output$tbIncomeStatement <- renderDataTable({
    req(scraped_financials())
    df <- if(is_expanded()) scraped_financials()[["Income Statement"]]$expanded else scraped_financials()[["Income Statement"]]$collapsed
    datatable(trim_financial_table(df, "Tax Effect of Unusual Items"), options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$tbBalanceSheet <- renderDataTable({
    req(scraped_financials())
    df <- if(is_expanded()) scraped_financials()[["Balance Sheet"]]$expanded else scraped_financials()[["Balance Sheet"]]$collapsed
    datatable(trim_financial_table(df, "Treasury Shares Number"), options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$tbCashFlow <- renderDataTable({
    req(scraped_financials())
    df <- if(is_expanded()) scraped_financials()[["Cash Flow"]]$expanded else scraped_financials()[["Cash Flow"]]$collapsed
    datatable(trim_financial_table(df, "Free Cash Flow"), options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$IS_download <- downloadHandler(
    filename = function() paste0(current_ticker(), "_incomestatement_", Sys.Date(), ".csv"),
    content = function(file) write.csv(d_income_statement(), file, row.names = FALSE)
  )
  output$BS_download <- downloadHandler(
    filename = function() paste0(current_ticker(), "_balancesheet_", Sys.Date(), ".csv"),
    content = function(file) write.csv(d_balance_sheet(), file, row.names = FALSE)
  )
  output$CF_download <- downloadHandler(
    filename = function() paste0(current_ticker(), "_cashflow_", Sys.Date(), ".csv"),
    content = function(file) write.csv(d_cash_flow(), file, row.names = FALSE)
  )
  
  # ==========================================
  # 📈 Income Statement 互動圖表
  # ==========================================
  selected_is_data <- reactive({
    req(d_income_statement())
    keyword <- switch(input$is_type,
                      "Total Revenue" = "Total Revenue",
                      "Gross Profit" = "Gross Profit",
                      "EBITDA" = "EBITDA")
    
    # 搜尋對應的會計科目名稱，並只取符合的第一筆資料
    res <- d_income_statement()[grepl(keyword, d_income_statement()[[1]], ignore.case = TRUE), ]
    if(nrow(res) > 0) return(res[1, ])
    return(NULL)
  })
  
  # ==========================================
  # 📈 Income Statement 互動圖表 (視覺優化版)
  # ==========================================
  output$is_plot <- renderPlotly({
    # 使用共用引擎繪圖
    generate_safe_line_plot(
      data = selected_is_data(), 
      ticker_name = current_ticker(), 
      metric_name = input$is_type
    )
  })
  
  # ==========================================
  # 📈 3. Cash Flow 互動圖表
  # ==========================================
  selected_cashflow_data <- reactive({
    req(d_cash_flow())
    keyword <- switch(input$cf_type,
                      "Operating Cash Flow" = "Operating Cash Flow",
                      "Investing Cash Flow" = "Investing Cash Flow",
                      "Financing Cash Flow" = "Financing Cash Flow")
    d_cash_flow()[grepl(keyword, d_cash_flow()[[1]], ignore.case = TRUE), ]
  })
  
  # ==========================================
  # 📈 3. Cash Flow 互動圖表 (視覺優化版)
  # ==========================================
  # (注意：這裡保留原本的 selected_cashflow_data reactive，但修改 output 渲染邏輯)
  output$cf_plot <- renderPlotly({
    # 使用共用引擎繪圖
    generate_safe_line_plot(
      data = selected_cashflow_data(), 
      ticker_name = current_ticker(), 
      metric_name = input$cf_type
    )
  })
  
  
  # ==========================================
  # 🔌 4. 呼叫外部模組 (KPI, FCF, DDM)
  # ==========================================
  kpi_module_server("kpi", d_income_statement, d_balance_sheet, d_cash_flow, reactive(input$industry_choice))
  
  run_calc_trigger <- reactiveVal(0)
  observeEvent(input$calc, { run_calc_trigger(run_calc_trigger() + 1) })
  observeEvent(d_cash_flow(), { 
    req(is.data.frame(d_cash_flow()), nrow(d_cash_flow()) > 0)
    run_calc_trigger(run_calc_trigger() + 1) 
  })
  
  # ==========================================
  # 🏦 擷取財報資料推算每股股利 (D0)
  # ==========================================
  auto_scraped_d0 <- reactive({
    req(d_cash_flow(), d_balance_sheet())
    
    # 從現金流量表抓取「發放股利」(通常是負數，表示現金流出)
    div_paid <- select_clean_metric_row(d_cash_flow(), "Cash Dividends Paid")[1]
    
    # 從資產負債表抓取「流通在外股數」
    shares <- as.numeric(select_clean_metric_row(d_balance_sheet(), "Share Issued")[1])
    
    if (!is.na(div_paid) && !is.na(shares) && shares > 0) {
      # 轉為絕對值並除以股數，得出每股股利 (D0)
      dps <- abs(div_paid) / shares
      return(round(dps, 2))
    } else {
      return(NA) # 若抓不到資料，回傳 NA 保留使用者手動輸入的空間
    }
  })
  
  # ==========================================
  # 掛載 DDM 模組
  # ==========================================
  ddm_results <- ddm_module_server(
    id = "mod_ddm", 
    ddm_g = reactive({ APP_DEFAULTS$ddm_g }), 
    ddm_ke = reactive({ APP_DEFAULTS$ddm_ke }),
    
    # 🌟 修正：不要傳整個表格！從 summary_data 自動找出包含 Dividend 的數字
    scraped_d0 = reactive({
      df <- summary_data()
      if (is.null(df)) return(NA)
      
      # 尋找 "Forward Dividend & Yield" 等包含 Dividend 的欄位
      div_row <- df[grepl("Dividend", df$Item, ignore.case = TRUE), ]
      if (nrow(div_row) > 0) {
        # Yahoo 格式通常是 "1.50 (2.34%)"，我們用正則表達式把前面的 1.50 抽出來
        suppressWarnings(as.numeric(stringr::str_extract(div_row$Value[1], "^[0-9.]+")))
      } else {
        NA
      }
    }),
    
    summary_df = summary_data,
    d_cash_flow = d_cash_flow, 
    d_balance_sheet = d_balance_sheet,
    d_income_statement = d_income_statement
  )
  
  # ==========================================
  # 呼叫 FCFF 模組，並把參數餵進去
  # ==========================================
  fcf_results <- fcf_projection_module_server(
    id = "mod_fcf", 
    d_balance_sheet = d_balance_sheet,
    d_income_statement = d_income_statement, 
    d_cash_flow = d_cash_flow,
    input_mode = reactive(input$dcf_mode), 
    input_years = reactive(input$years),
    sgr = reactive(input$sgr), 
    g_stage1 = reactive(input$g_stage1), 
    g_stage2 = reactive(input$sgr), 
    input_capex_rate = reactive(input$var_capex_rate),
    input_nwc_rate   = reactive(input$var_nwc_rate),
    input_manual_fcf = reactive(input$manual_fcf),
    calc_trigger = run_calc_trigger,
    global_est_g = estimated_g  # 加入這行：把大腦算好的數值餵給模組
  )
  
  observeEvent({
    input$sgr; input$g_stage1; input$dcf_mode
  }, {
    run_calc_trigger(run_calc_trigger() + 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$dcf_mode, {
    req(input$dcf_mode)
    if (isTRUE(input$dcf_mode == "gordon")) {
      current_wacc <- if(!is.na(input$wacc_gordon)) input$wacc_gordon else 10
      if (!is.na(input$sgr) && input$sgr >= current_wacc) {
        safe_sgr <- max(0, current_wacc - 2)
        updateNumericInput(session, "sgr", value = safe_sgr)
        showNotification("Gordon 模型需滿足 g < WACC，已自動調整 SGR", type = "warning")
      }
    }
  })
  
  observeEvent(input$sgr, {
    req(input$dcf_mode == "two_stage", input$wacc_stage2)
    curr_sgr <- as.numeric(input$sgr)
    curr_wacc2 <- as.numeric(input$wacc_stage2)
    if (!is.na(curr_sgr) && !is.na(curr_wacc2) && curr_sgr >= curr_wacc2) {
      safe_val <- max(0, curr_wacc2 - 2) 
      updateNumericInput(session, "sgr", value = safe_val)
      showNotification(paste("⚠️ 終端成長率不得高於折現率，已修正為", safe_val, "%"), type = "warning")
    }
  })
  
  observeEvent(input$years, {
    updateNumericInput(session, "yr_stage1", value = input$years)
  })
  
  # ==========================================
  # 呼叫 RI (剩餘收益) 模組
  # ==========================================
  ri_results <- ri_module_server(
    id = "mod_ri", 
    d_income_statement = d_income_statement, 
    d_balance_sheet = d_balance_sheet, 
    d_cash_flow = d_cash_flow, 
    global_re = estimated_re,      # 傳入系統算好的 CAPM (Ke)
    scraped_shares = scraped_shares # 傳入股數來算 BVPS
  )
  
  # ==========================================
  # 🚨 6. 詐欺風險警示 (Fraud Risk Warnings)
  # ==========================================
  fraud_warnings <- reactiveValues(fcf = "", ocf = "", biz = "", cashback = "", debt = "")
  
  output$nofreecashflow <- renderText({
    fcf <- get_avg(select_clean_metric_row(d_cash_flow(), "Free Cash Flow"))
    fraud_warnings$fcf <- if (is.na(fcf)) "" else if (fcf < 0) "⚠️ 自由現金流為負數，可能營運困難或大量資本支出" else ""
    fraud_warnings$fcf
  })
  
  output$nooperatingcashflow <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    fraud_warnings$ocf <- if (is.na(ocf)) "" else if (ocf < 0) "⚠️ 營業現金流為負數，代表核心業務沒有產生現金" else ""
    fraud_warnings$ocf
  })
  
  output$notdoingbusiness <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    fraud_warnings$biz <- if (is.na(ocf) || is.na(net)) "" else if (ocf < net) "⚠️ 營業現金流低於淨利，帳面賺錢但現金未實現" else ""
    fraud_warnings$biz
  })
  
  output$notgettingcashback <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow"))
    net <- get_avg(select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation"))
    fraud_warnings$cashback <- if (is.na(ocf) || is.na(net)) "" else if (net > 0 && ocf < 0) "⚠️ 淨利為正但現金流為負，獲利品質存疑" else ""
    fraud_warnings$cashback
  })
  
  output$highdebttoequity <- renderText({
    total_liabilities <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Debt"))
    total_equity <- get_avg(select_clean_metric_row(d_balance_sheet(), "Common Stock Equity"))
    ratio <- if (is.na(total_liabilities) || is.na(total_equity) || total_equity == 0) NA else total_liabilities / total_equity
    fraud_warnings$debt <- if (is.na(ratio)) "" else if (ratio > 2) "⚠️ 負債對權益比率過高，財務槓桿風險大" else ""
    fraud_warnings$debt
  })
  
  output$no_fraud_detected <- renderText({
    if (all(fraud_warnings$fcf == "", fraud_warnings$ocf == "", fraud_warnings$biz == "", fraud_warnings$cashback == "", fraud_warnings$debt == "")) {
      "Currently no fraud risks detected."
    } else ""
  })
  
  output$stable_indicator_table <- renderTable({
    data.frame(
      指標名稱 = c("毛利率", "OPEX Ratio", "ROA / ROE", "存貨週轉 / 應收週轉", "Equity Multiplier", "自由現金流比"),
      穩定性 = c("★★★★☆", "★★★★☆", "★★★★☆", "★★★☆☆", "★★★☆☆", "★★★★★"),
      說明 = c("技術/品牌優勢的象徵", "管理與營運效率穩定性", "去波動化後能長期觀察企業效率", "營運效率的直接反映", "財務體質穩定，不易劇變", "最能看出企業真實價值創造力"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "m", width = "100%")
  
  # ==========================================
  # 🧮 7. CAPM, WACC 與 DCF 估值計算
  # ==========================================
  scraped_debt <- reactive({
    req(d_balance_sheet())
    val <- select_clean_metric_row(d_balance_sheet(), "Total Debt")
    if (length(val) > 0 && !all(is.na(val))) return(as.numeric(na.omit(val))[1])
    return(0)
  })
  
  scraped_shares <- reactive({
    req(d_balance_sheet())
    # 🌟 擴大搜尋範圍：涵蓋 Yahoo Finance 常見的三種股數命名
    val <- select_clean_metric_row(d_balance_sheet(), "Ordinary Shares Number|Share Issued|Total Shares Outstanding")
    
    if (length(val) > 0 && !all(is.na(val))) {
      return(as.numeric(na.omit(val))[1])
    }
    return(1) # 如果真的都找不到，才退回預設防呆值 1
  })
  
  observeEvent(input$industry_choice, {
    req(input$industry_choice)
    inds <- industry_standards[[input$industry_choice]]
    if (!is.null(inds)) {
      updateNumericInput(session, "capm_beta", label = paste0("Beta (β) [套用產業: ", input$industry_choice, "]"), value = inds$beta_avg)
      updateNumericInput(session, "capm_rm", value = inds$rm_avg)
    }
  })
  
  output$txt_display_years <- renderUI({
    HTML(paste0("<b>目前預測年數：<span style='color:red; font-size:16px;'>", input$years, "</span> 年</b>"))
  })
  
  output$vbx_capm <- renderValueBox({
    rf <- input$var_rf / 100
    rm <- input$var_rm / 100
    beta <- input$var_beta
    capm <- rf + beta * (rm - rf)
    valueBox(value = paste0(round(capm * 100, 2), "%"), subtitle = "預估權益成本 (CAPM / Cost of Equity)", icon = icon("chart-line"), color = "blue")
  })
  
  output$vbx_wacc <- renderValueBox({
    rf <- input$var_rf / 100
    rm <- input$var_rm / 100
    beta <- input$var_beta
    capm <- rf + beta * (rm - rf)
    
    kd <- input$var_cost_debt / 100
    t <- input$var_tax_rate / 100
    we <- input$var_weight_equity / 100
    wd <- 1 - we 
    
    wacc <- (we * capm) + (wd * kd * (1 - t))
    valueBox(value = paste0(round(wacc * 100, 2), "%"), subtitle = "加權平均資本成本 (WACC)", icon = icon("calculator"), color = "purple")
  })
  
  estimated_g <- reactiveVal(NULL)
  estimated_re <- reactiveVal(NULL)
  calculated_wacc <- reactiveVal(NULL)
  dcf_value_result <- reactiveVal(NULL)
  stock_price_estimate_val <- reactiveVal(NULL)
  
  output$txt_hist_capex <- renderUI({
    params <- fcf_results$hist_params() 
    if(is.null(params) || is.na(params$capex_rate)) return(HTML("<div style='color: gray; font-size: 13px; margin-bottom: 5px;'>⏳ 系統推算值：等待財報資料匯入...</div>"))
    val <- round(params$capex_rate * 100, 2)
    HTML(paste0("<div style='color: #3c8dbc; font-size: 14px; margin-bottom: 5px;'>📊 系統歷史推算值：<b>", val, " %</b></div>"))
  })
  
  output$txt_hist_nwc <- renderUI({
    params <- fcf_results$hist_params()
    if(is.null(params) || is.na(params$nwc_rate)) return(HTML("<div style='color: gray; font-size: 13px; margin-bottom: 5px;'>⏳ 系統推算值：等待財報資料匯入...</div>"))
    val <- round(params$nwc_rate * 100, 2)
    HTML(paste0("<div style='color: #3c8dbc; font-size: 14px; margin-bottom: 5px;'>📊 系統歷史推算值：<b>", val, " %</b></div>"))
  })
  
  output$txt_fcf_sync_status <- renderPrint({
    df <- fcf_results$df_fcf()
    if (is.null(df)) {
      cat("尚未匯入財報資料，或正在等待計算...")
    } else {
      cat("✅ FCF 預測資料已同步！\n-------------------------\n")
      cat("第 1 年預測現金流:", df$FCF[1], "\n")
      cat("第", nrow(df), "年預測現金流:", df$FCF[nrow(df)], "\n")
      cat("模型狀態:", unique(df$Type)[1], "\n")
    }
  })
  
  observe({
    req(d_cash_flow(), d_income_statement(), d_balance_sheet(), input$g_growth_method)
    method <- input$g_growth_method
    if (is.null(method)) return()
    
    vec_fcf <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    if (all(is.na(vec_fcf))) {
      estimated_g(NULL)
      return()
    }
    
    fcf_chrono <- rev(vec_fcf) 
    g_rate_raw <- diff(fcf_chrono) / abs(head(fcf_chrono, -1))
    g_rate <- g_rate_raw[is.finite(g_rate_raw)]
    g_rate <- g_rate[g_rate > -5 & g_rate < 5] 
    
    fund_res <- NULL
    if (isTRUE(method == "fundamental")) {
      ebit <- select_clean_metric_row(d_income_statement(), "Operating Income|EBIT")[1]
      tax_rate <- if(!is.null(input$wacc_tax)) input$wacc_tax / 100 else APP_DEFAULTS$wacc_tax
      nopat <- ebit * (1 - tax_rate)
      
      total_assets <- select_clean_metric_row(d_balance_sheet(), "Total Assets")[1]
      curr_liab <- select_clean_metric_row(d_balance_sheet(), "Total Current Liabilities|Current Liabilities")[1] 
      st_debt <- select_clean_metric_row(d_balance_sheet(), "Current Debt|Short Term Debt")[1] 
      cash_eq <- select_clean_metric_row(d_balance_sheet(), "Cash And Cash Equivalents|Cash & Cash Equivalents")[1]
      
      st_debt <- ifelse(is.na(st_debt), 0, st_debt)
      curr_liab <- ifelse(is.na(curr_liab), 0, curr_liab)
      cash_eq <- ifelse(is.na(cash_eq), 0, cash_eq)
      total_assets <- ifelse(is.na(total_assets), 0, total_assets)
      
      invested_capital <- (total_assets - cash_eq) - (curr_liab - st_debt)
      roic <- if(!is.na(invested_capital) && invested_capital > 0) nopat / invested_capital else 0
      
      capex <- abs(select_clean_metric_row(d_cash_flow(), "Capital Expenditure")[1])
      depre <- select_clean_metric_row(d_cash_flow(), "Depreciation")[1] 
      cf_delta_nwc <- select_clean_metric_row(d_cash_flow(), "Change In Working Capital|Changes In Working Capital")[1]
      
      capex <- ifelse(is.na(capex), 0, capex)
      depre <- ifelse(is.na(depre), 0, depre)
      cf_delta_nwc <- ifelse(is.na(cf_delta_nwc), 0, cf_delta_nwc)
      nwc_investment <- -cf_delta_nwc 
      
      if (!is.na(nopat) && nopat > 0) {
        reinvestment_rate <- (capex - depre + nwc_investment) / nopat
      } else {
        reinvestment_rate <- 0 
      }
      
      raw_fund_g <- reinvestment_rate * roic
      
      # 🌟 聽從使用者的勾選框指令
      apply_ceiling <- if (!is.null(input$apply_g_ceiling)) input$apply_g_ceiling else TRUE
      
      if (apply_ceiling) {
        final_fund_g <- max(-0.05, min(raw_fund_g, 0.25)) # 封頂 25%
      } else {
        final_fund_g <- max(-0.05, raw_fund_g)            # 解除封頂
      }
      
      fund_res <- list(
        g = round(final_fund_g * 100, 2), 
        raw_g = round(raw_fund_g * 100, 2), 
        roic = roic, 
        rr = reinvestment_rate, 
        nopat = nopat, 
        ic = invested_capital,
        ceiling_applied = apply_ceiling
      )
    }
    
    val <- switch(method,
                  "fundamental" = if(!is.null(fund_res)) fund_res$g else NA,
                  "cagr" = { 
                    valid_fcf <- na.omit(fcf_chrono)
                    if (length(valid_fcf) < 2 || head(valid_fcf, 1) <= 0 || tail(valid_fcf, 1) <= 0) NA else round(((tail(valid_fcf, 1) / head(valid_fcf, 1))^(1 / (length(valid_fcf) - 1)) - 1) * 100, 2)
                  },
                  "mean" = if(length(g_rate) > 0) round(mean(g_rate) * 100, 2) else NA,
                  "median" = if(length(g_rate) > 0) round(median(g_rate) * 100, 2) else NA,
                  "last_year" = if (length(vec_fcf) >= 2 && !is.na(vec_fcf[1]) && !is.na(vec_fcf[2]) && vec_fcf[2] != 0) round(((vec_fcf[1] - vec_fcf[2]) / abs(vec_fcf[2])) * 100, 2) else NA,
                  "custom" = input$custom_g
    )
    
    if (is.null(val) || any(is.na(val))) {
      estimated_g(NULL)
      updateSelectInput(session, "g_growth_method", label = "預估 FCFF 成長率 (⚠️ 缺乏數據)")
      return()
    }
    
    estimated_g(val)
    updateSelectInput(session, "g_growth_method", label = paste0("預估 FCFF 成長率 ➔ ", val, " %"))
    
    # 🌟 動態 UI：加入「實際輸出」的明確標示
    output$g_result <- renderUI({
      if (method == "fundamental" && !is.null(fund_res)) {
        hit_ceiling_raw <- fund_res$raw_g > 25
        
        ceiling_status_msg <- if (hit_ceiling_raw && fund_res$ceiling_applied) {
          glue::glue("<div style='color: #d9534f; margin-top: 5px; font-weight: bold;'>⚠️ 原始成長率過高，已啟動防呆強制封頂。(實際輸出至模型: 25.00 %)</div>")
        } else if (hit_ceiling_raw && !fund_res$ceiling_applied) {
          glue::glue("<div style='color: #8e44ad; margin-top: 5px; font-weight: bold; padding: 5px; border: 1px solid #8e44ad; background: #f4ecf7;'>🔥 警告：已解除天花板！將使用極端成長率進行估值 (實際輸出至模型: {fund_res$g} %)</div>")
        } else {
          glue::glue("<div style='color: #00a65a; margin-top: 5px; font-weight: bold;'>✅ 成長率處於合理範圍內 (實際輸出至模型: {fund_res$g} %)</div>")
        }
        
        HTML(glue::glue(
          "<div style='padding: 12px; background-color: #fdfaf6; border-left: 4px solid #d35400; font-size: 13px;'>
             <b>💡 學理推估 (Fundamental) 拆解：</b><br/>
             <span style='color: #555;'>公式：投資報酬率 (ROIC) × 再投資率 (RR)</span><br/>
             <span style='color: #2980b9; font-weight: bold;'>
               {round(fund_res$roic * 100, 2)} % × {round(fund_res$rr * 100, 2)} % = {fund_res$raw_g} %
             </span><br/>
             {ceiling_status_msg}
           </div>"
        ))
      } else if (method == "last_year") {
        HTML(glue::glue(
          "<div style='padding: 10px; border-left: 4px solid #7f8c8d; font-size: 13px; color: #7f8c8d;'>
             💡 採用最近一年成長率：直接取用財報最新一期 vs 前一期的變化幅度。
           </div>"
        ))
      } else {
        NULL 
      }
    })
    
    # 🌟 關鍵修復：強制覆寫！只要不是選 custom，就把算好的 val 強制灌入第一階段成長率中
    if (method != "custom" && !is.na(val)) {
      updateNumericInput(session, "g_stage1", value = val)
    }
  })
  
  output$ibx_estimated_g <- renderInfoBox({ 
    val_g <- if (!is.null(estimated_g())) estimated_g() else "N/A"
    infoBox("預估營收成長率 (est.g)", paste0(val_g, " %"), icon = icon("chart-line"), color = "purple", fill = TRUE) 
  })
  
  output$ibx_sgr <- renderInfoBox({ 
    val_sgr <- if (!is.null(input$sgr)) input$sgr else "N/A"
    infoBox("永續成長率 (SGR)", paste0(val_sgr, " %"), icon = icon("infinity"), color = "maroon", fill = TRUE) 
  })
  
  output$ibx_wacc <- renderInfoBox({ 
    val_wacc <- if (!is.null(calculated_wacc())) round(calculated_wacc() * 100, 2) else APP_DEFAULTS$wacc_gordon
    infoBox("WACC", h3(paste0(val_wacc, " %")), icon = icon("percent"), color = "aqua", fill = TRUE) 
  })
  
  output$plt_fcf_trend <- renderPlot({
    # 確保模組已經算出資料，避免初始載入時報錯
    req(fcf_results$df_fcf()) 
    
    # 透過模組對外的接口取得預測表
    df <- fcf_results$df_fcf() 
    
    ggplot(df, aes(x = Year)) +
      geom_bar(aes(y = FCFF, fill = FCFF > 0), stat = "identity", width = 0.6, alpha = 0.8) +
      scale_fill_manual(values = c("TRUE" = "#00a65a", "FALSE" = "#d9534f"), guide = "none") +
      # 🌟 修正：把 Net_Income 改為 NOPAT，並將圖例名稱改為 NOPAT
      geom_line(aes(y = NOPAT, group = 1, color = "預估稅後營業利潤 (NOPAT)"), size = 1.5) +
      geom_point(aes(y = NOPAT), size = 3, color = "#3c8dbc") +
      scale_color_manual(name = "", values = c("預估稅後營業利潤 (NOPAT)" = "#3c8dbc")) +
      geom_text(aes(y = FCFF, label = paste0("$", round(FCFF, 1))), 
                vjust = ifelse(df$FCFF >= 0, -0.5, 1.5), size = 4, fontface = "bold") +
      theme_minimal() +
      labs(title = "FCFF 與 營業利潤 成長軌跡", x = "預測年份", y = "金額 (百萬)") +
      theme(
        # 🌟 修正：確保這裡是 face 而不是 fontface (其實原本這段就是 face，但保險起見再看一次)
        plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        legend.position = "top"
      )
  })
  
  observeEvent(input$calc_capm, {
    r_e_est <- (input$capm_rf / 100) + input$capm_beta * ((input$capm_rm / 100) - (input$capm_rf / 100))
    estimated_re(r_e_est)
    updateNumericInput(session, "wacc_re", value = round(r_e_est * 100, 2))
  })
  
  observeEvent(input$calc_wacc, {
    req(d_balance_sheet(), summary_data())
    
    # 修正 1：市值推估 (Market Value of Equity)
    shares <- scraped_shares() 
    df_sum <- summary_data()
    price_str <- if("Previous Close" %in% df_sum$Item) df_sum$Value[df_sum$Item == "Previous Close"] else NA
    price_val <- as.numeric(gsub("[,\\$]", "", price_str))
    
    equity_mv <- if (!is.na(price_val) && shares > 0) shares * price_val else select_clean_metric_row(d_balance_sheet(), "Common Stock Equity")[1]
    
    debt <- select_clean_metric_row(d_balance_sheet(), "Total Debt")[1]
    debt <- if (is.na(debt)) 0 else debt
    
    total_capital <- equity_mv + debt
    
    r_e <- if (input$use_estimated_re && !is.null(estimated_re())) estimated_re() else input$wacc_re / 100
    r_d <- input$wacc_rd / 100
    
    wacc <- (equity_mv / total_capital) * r_e + (debt / total_capital) * r_d * (1 - (input$wacc_tax / 100))
    calculated_wacc(wacc)
    wacc_percent <- round(wacc * 100, 2)
    
    if (input$dcf_mode == "gordon") updateNumericInput(session, "wacc_gordon", value = wacc_percent)
    else { updateNumericInput(session, "wacc_stage1", value = wacc_percent); updateNumericInput(session, "wacc_stage2", value = wacc_percent) }
    
    showNotification(glue::glue("📌 已自動將 WACC {wacc_percent}% 套用至 DCF"), type = "message")
  })
  
  output$ibx_re <- renderInfoBox({
    val_re <- input$wacc_re
    if (is.null(val_re)) val_re <- APP_DEFAULTS$wacc_re
    if (isTRUE(input$use_estimated_re) && !is.null(estimated_re())) val_re <- estimated_re() * 100
    infoBox("股權成本 (rₑ)", h3(paste0(round(val_re, 2), " %")), icon = icon("chart-line"), color = "teal", fill = TRUE)
  })
  
  output$ibx_rd <- renderInfoBox({
    val_rd <- input$wacc_rd
    if (is.null(val_rd)) val_rd <- APP_DEFAULTS$wacc_rd
    infoBox("負債成本 (rᵈ)", h3(paste0(round(val_rd, 2), " %")), icon = icon("university"), color = "lime", fill = TRUE)
  })
  
  # ==========================================
  # 📉 DCF 折現軌跡圖 (plt_dcf_trajectory)
  # ==========================================
  output$plt_dcf_trajectory <- renderPlot({
    req(fcf_results$df_fcf(), current_ticker())
    proj_df <- fcf_results$df_fcf()
    if (nrow(proj_df) < 2) {
      plot.new()
      text(0.5, 0.5, "⚠️ 財報數據不足或年份過少，無法繪圖", cex = 1.4)
      return()
    }
    
    n_years <- nrow(proj_df)
    
    # 1. 取得對應的 WACC 陣列
    wacc_val <- tryCatch({
      if (isTRUE(input$use_calculated_wacc) && !is.null(calculated_wacc())) {
        rep(as.numeric(calculated_wacc()), n_years)
      } else if (input$dcf_mode == "gordon") {
        rep(as.numeric(input$wacc_gordon) / 100, n_years)
      } else {
        s1_yrs <- as.numeric(input$yr_stage1)
        c(rep(as.numeric(input$wacc_stage1) / 100, min(s1_yrs, n_years)), 
          rep(as.numeric(input$wacc_stage2) / 100, max(n_years - s1_yrs, 0)))
      }
    }, error = function(e) rep(0.1, n_years)) 
    
    proj_df$FCF <- as.numeric(proj_df$FCF)
    discount_factors <- cumprod(1 + wacc_val)
    proj_df$DCF <- round(proj_df$FCF / discount_factors, 2)
    
    # 🌟 關鍵修復：將 SGR 算出的終值 (TV) 現值，強制疊加到最後一年的 DCF 上！
    g_terminal <- if (is.numeric(input$sgr)) input$sgr / 100 else 0.03
    terminal_wacc <- tail(wacc_val, 1)
    tv_annotation <- ""
    
    if (!is.na(terminal_wacc) && !is.na(g_terminal) && terminal_wacc > g_terminal) {
      last_fcf <- proj_df$FCF[n_years]
      tv <- (last_fcf * (1 + g_terminal)) / (terminal_wacc - g_terminal)
      pv_tv <- tv / discount_factors[n_years]
      
      # 將 PV of TV 灌入最後一年的 DCF 點位中
      proj_df$DCF[n_years] <- round(proj_df$DCF[n_years] + pv_tv, 2)
      # 準備圖表副標題，讓使用者知道圖表發生了什麼事
      tv_annotation <- paste0("\n( 💡 第 ", n_years, " 年的 DCF 點位已包含永續終值現值 PV of TV: $", scales::comma(round(pv_tv, 2)), " )")
    }
    
    # 2. 建立繪圖資料集
    plot_df <- data.frame(
      Year   = rep(proj_df$Year, 2),
      Value  = c(proj_df$FCF, proj_df$DCF),
      Metric = factor(rep(c("預測現金流 (FCF)", "折現後價值 (DCF)"), each = n_years),
                      levels = c("預測現金流 (FCF)", "折現後價值 (DCF)"))
    )
    
    # 3. 繪製精美圖表
    ggplot(plot_df, aes(x = Year, y = Value, color = Metric, linetype = Metric, group = Metric)) +
      geom_line(linewidth = 1.2) + 
      geom_point(size = 3) +
      # 🌟 geom_text 裡面用 fontface 是可以的 (這是 grid 層級的參數)
      geom_text(aes(label = scales::comma(Value)), 
                vjust = -1.5, size = 4.5, show.legend = FALSE) +
      scale_color_manual(values = c("預測現金流 (FCF)" = "#95a5a6", "折現後價值 (DCF)" = "#e74c3c")) +      
      theme_minimal(base_size = 14) + 
      labs(title = paste0(current_ticker(), " - FCF vs DCF 現值折現軌跡圖"), 
           subtitle = tv_annotation, # 顯示 TV 提示
           x = "年份", y = "USD (Millions)") + 
      theme(legend.position = "top", 
            plot.title = element_text(face = "bold", hjust = 0.5),
            # 🌟 關鍵修正：將 element_text 裡面的 fontface 改為 face
            plot.subtitle = element_text(color = "#8e44ad", face = "bold", hjust = 0.5)) 
  })
  
  generate_fcf_plot <- reactive({
    req(d_cash_flow()) 
    fcf_history <- na.omit(select_clean_metric_row(d_cash_flow(), "Free Cash Flow"))
    if (length(fcf_history) == 0) return(NULL)
    
    fcf_start <- fcf_history[1]
    n_years <- input$years
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    df_list <- list(data.frame(Year = current_year, FCF = fcf_start, Type = "預設")) 
    
    sgr <- if(is.null(input$sgr) || is.na(input$sgr)) 0 else input$sgr
    g_stage1 <- if(is.null(input$g_stage1) || is.na(input$g_stage1)) 0 else input$g_stage1
    g_stage2 <- sgr
    
    if (input$dcf_mode == "gordon") {
      df_list[[2]] <- data.frame(Year = (current_year + 1):(current_year + n_years), FCF = fcf_start * (1 + sgr / 100)^(1:n_years), Type = "Gordon")
    } else {
      yr1 <- min(input$yr_stage1, n_years)
      yr2 <- max(n_years - yr1, 0)
      if (yr1 > 0) df_list[[2]] <- data.frame(Year = (current_year + 1):(current_year + yr1), FCF = fcf_start * cumprod(rep(1 + g_stage1 / 100, yr1)), Type = "第一階段")
      if (yr2 > 0) df_list[[3]] <- data.frame(Year = (current_year + yr1 + 1):(current_year + n_years), 
                                              FCF = (if(yr1 > 0) tail(df_list[[2]]$FCF, 1) else fcf_start) * cumprod(rep(1 + g_stage2 / 100, yr2)), Type = "第二階段")
    }
    
    final_df <- do.call(rbind, df_list)
    final_df <- final_df[!is.na(final_df$FCF), ]
    if (nrow(final_df) < 2) return(NULL)
    return(final_df)
  })
  
  output$dft_fcf_plot <- renderPlot({
    df <- generate_fcf_plot()
    if (is.null(df) || nrow(df) == 0) { plot.new(); text(0.5, 0.5, "⏳ 等待財報資料匯入...", cex = 1.4); return() }
    
    ggplot(df, aes(x = Year, y = FCF, linetype = Type, group = 1)) + 
      geom_line(linewidth = 1.2, color = "steelblue") + 
      geom_point(aes(color = FCF < 0), size = 3) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "steelblue"), guide = "none") + 
      theme_minimal(base_size = 14) +
      labs(title = "📉 自由現金流即時預覽", x = "年份", y = "FCF (USD)") + theme(legend.position = "top")
  })
  
  # ==========================================
  # 💰 8. DCF 計算核心與企業估值 (對接 FCFF 預測序列)
  # ==========================================
  observeEvent(input$calc, {
    req(current_ticker(), input$dcf_mode, input$years, fcf_results$df_fcf()) 
    
    n <- as.numeric(input$years)
    if (is.na(n) || n <= 0) return(NULL)
    
    # 核心修正：直接讀取 FCFF 模組動態推算出來的「未來現金流預測表」
    proj_df <- fcf_results$df_fcf()
    future_fcfs <- as.numeric(proj_df$FCFF) # 取出未來的自由現金流向量
    
    if (length(future_fcfs) != n) {
      showNotification("⚠️ 預測年數與 FCFF 表格不符，請重新計算", type = "error")
      return(NULL)
    }
    
    dcf_value <- NA
    
    # 取得終端永續成長率 (統一由 DCF 主畫面的 sgr 掌控)
    g_terminal <- input$sgr / 100
    
    # 判斷是否使用系統估算出來的 WACC
    use_calc_wacc <- isTRUE(input$use_calculated_wacc) && !is.null(calculated_wacc())
    
    # --- 依據不同模型設定折現率與折現因子 ---
    if (input$dcf_mode == "gordon") {
      req(input$sgr, input$wacc_gordon)
      r1 <- if(use_calc_wacc) calculated_wacc() else (input$wacc_gordon / 100)
      r2 <- r1 # Gordon 模型從頭到尾只有一個折現率
      
      if (!is.na(r2) && g_terminal >= r2) { 
        showNotification("❌ 成長率 g 必須嚴格小於折現率 WACC", type = "error")
        return(NULL) 
      }
      
      # 建立折現因子序列
      discount_factors <- (1 + r1)^(1:n)
      
    } else {
      # 二階段模型
      req(input$g_stage1, input$sgr, input$yr_stage1, input$wacc_stage1, input$wacc_stage2)
      
      if (use_calc_wacc) {
        r1 <- calculated_wacc()
        r2 <- calculated_wacc()
      } else {
        r1 <- input$wacc_stage1 / 100
        r2 <- input$wacc_stage2 / 100
      }
      
      if (g_terminal >= r2) { 
        showNotification("❌ 永續成長率 g2 必須小於第二階段折現率 WACC2", type = "error")
        return(NULL) 
      }
      
      yr1 <- as.numeric(input$yr_stage1)
      if (yr1 <= 0 || yr1 >= n) { 
        showNotification("⚠️ 第一階段年數無效 (需大於 0 且小於預測總年數 n)", type = "error")
        return(NULL) 
      }
      
      # 建立二階段專用的 WACC 序列與折現因子
      wacc_sequence <- c(rep(r1, min(yr1, n)), rep(r2, max(0, n - yr1)))
      discount_factors <- cumprod(1 + wacc_sequence)
    }
    
    # -------------------------------------
    # 估值計算開始：PV + TV
    # -------------------------------------
    # 1. 預測期現值 (PV of Forecast)
    pv_forecast <- sum(future_fcfs / discount_factors)
    
    # 2. 終值 (TV) 及其現值：使用模組預測出來的最後一年 FCFF 作為基數
    last_fcf <- future_fcfs[n]
    tv <- (last_fcf * (1 + g_terminal)) / (r2 - g_terminal)
    pv_tv <- tv / discount_factors[n]
    
    # 3. 企業總價值 (EV)
    dcf_value <- pv_forecast + pv_tv
    dcf_value_result(dcf_value) # 寫入 Reactive 變數
    
    # -------------------------------------
    # 股權價值轉換：加現金、扣負債、除以股數
    # -------------------------------------
    latest_cash <- get_latest_cash_position(d_cash_flow())
    temp_debt <- select_clean_metric_row(d_balance_sheet(), "Total Debt")
    scraped_debt <- if (length(temp_debt) > 0 && !all(is.na(temp_debt))) as.numeric(na.omit(temp_debt))[1] else NA
    latest_debt <- if (!is.null(input$manual_debt) && !is.na(input$manual_debt)) input$manual_debt else if (!is.na(scraped_debt)) scraped_debt else 0
    
    # 與上方同步，擴大搜尋股數科目
    temp_shares <- select_clean_metric_row(d_balance_sheet(), "Ordinary Shares Number|Share Issued|Total Shares Outstanding")
    share_outstanding <- if(length(temp_shares) > 0 && !all(is.na(temp_shares))) as.numeric(na.omit(temp_shares))[1] else 1
    
    equity_value <- as.numeric(dcf_value)[1] + latest_cash - latest_debt
    if (!is.na(equity_value) && !is.na(share_outstanding) && share_outstanding > 0) {
      stock_price_estimate_val(equity_value / share_outstanding) # 寫入 Reactive 變數
    } else {
      stock_price_estimate_val(NULL)
    }
    
    # 計算完畢後發出通知
    wacc_source <- if(use_calc_wacc) "系統估算值" else "手動輸入值"
    showNotification(glue::glue("✅ 估值更新：已成功將模組 FCFF 序列套入 DCF 運算引擎 (採用 {wacc_source} WACC)"), type = "message")
  })
  
  # ==========================================
  # 渲染估值結果與 InfoBox
  # ==========================================
  
  output$vtxt_dcf_results <- renderText({
    ev_val <- dcf_value_result()
    stock_val <- stock_price_estimate_val()
    
    # 關鍵修復：先檢查長度是否為 0 (攔截 NULL 與空向量)，再檢查是否為 NA
    if (length(ev_val) == 0 || is.na(ev_val)) {
      return("⚠️ 尚未計算 DCF，請確認參數後按下「▶ 試算 DCF」")
    }
    
    msg <- glue::glue("企業總價值 (EV)：${round(ev_val, 2)}")
    
    if (length(stock_val) > 0 && !is.na(stock_val)) {
      msg <- glue::glue("{msg}\n 最終每股合理價：${round(stock_val, 2)}")
    }
    
    return(msg)
  })
  
  output$ibx_stock_value_dcf <- renderInfoBox({ 
    infoBox("每股估值（DCF）", 
            if(is.null(stock_price_estimate_val())) "N/A" else paste0("$", round(stock_price_estimate_val(), 2)), 
            icon = icon("money-bill-wave"), color = "maroon", fill = TRUE) 
  })
  
  output$ibx_enterprise_value_dcf <- renderInfoBox({ 
    infoBox("企業估值（DCF）", 
            if(is.null(dcf_value_result())) "N/A" else format_dollar_abbr(dcf_value_result()), 
            icon = icon("building"), color = "purple", fill = TRUE) 
  })
  
  output$vtxt_dcf_setting_details <- renderUI({
    req(input$dcf_mode, input$years)
    use_calc <- isTRUE(input$use_calculated_wacc) && !is.null(calculated_wacc())
    
    wacc_val <- if (use_calc) {
      paste0(round(calculated_wacc() * 100, 2), "% (估算)")
    } else if (isTRUE(input$dcf_mode == "gordon")) {
      paste0(input$wacc_gordon, "% (手動)")
    } else {
      paste0(input$wacc_stage1, "% / ", input$wacc_stage2, "% (手動)")
    }
    
    HTML(glue::glue("<div style='padding: 15px; background: #fcfcfc; border: 1px solid #eee; font-size: 14px;'>
                  <b>評價模式：</b> {input$dcf_mode} <br/>
                  <b>預測年數：</b> {input$years} 年 <br/>
                  <b>折現率 WACC：</b> {wacc_val}</div>"))
  })
  
  # ==========================================
  # 📊 綜合估值對比水位圖 (結合智能決策矩陣)
  # ==========================================
  output$ui_valuation_compare <- renderUI({
    # 1. 取得三種價格
    p_dcf <- stock_price_estimate_val()
    p_ddm <- tryCatch({ ddm_results$ddm_price() }, error = function(e) NULL)
    
    df_sum <- summary_data()
    p_curr <- if (!is.null(df_sum) && "Previous Close" %in% df_sum$Item) {
      val_str <- df_sum$Value[df_sum$Item == "Previous Close"][1]
      suppressWarnings(as.numeric(gsub("[,\\$]", "", val_str)))
    } else { NA }
    
    is_valid <- function(x) { length(x) == 1 && !is.na(x) && is.numeric(x) && is.finite(x) }
    if (!is_valid(p_curr)) return(NULL)
    
    val_dcf <- if (is_valid(p_dcf)) p_dcf else NA
    val_ddm <- if (is_valid(p_ddm)) p_ddm else NA
    if (is.na(val_dcf) && is.na(val_ddm)) return(NULL)
    
    plot_dcf <- if (!is.na(val_dcf)) val_dcf else p_curr
    plot_ddm <- if (!is.na(val_ddm)) val_ddm else p_curr
    
    # -----------------------------------------------------
    # 🌟 核心引擎：Valuation Method Decision Matrix (模型決策矩陣)
    # -----------------------------------------------------
    # 自動掃描財報特徵，推薦「正確的評價方法」
    fcf_seq <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    div_seq <- select_clean_metric_row(d_cash_flow(), "Cash Dividends Paid")
    
    is_fcf_positive <- length(fcf_seq) > 0 && !all(is.na(fcf_seq)) && mean(fcf_seq, na.rm=TRUE) > 0
    is_dividend_paying <- length(div_seq) > 0 && !all(is.na(div_seq)) && mean(abs(div_seq), na.rm=TRUE) > 0
    
    rec_title <- "正在分析..."
    rec_desc <- ""
    rec_color <- "#7f8c8d"
    rec_icon <- "lightbulb"
    
    if (is_dividend_paying && !is_fcf_positive) {
      rec_title <- "推薦首選：股利折現模型 (DDM)"
      rec_desc <- "偵測到該公司維持配息，但近期自由現金流為負。此時 DCF 可能失真，少數股東應以 DDM (實際收到的現金) 評估最為準確。"
      rec_color <- "#3498db" 
      rec_icon <- "hand-holding-usd"
    } else if (!is_dividend_paying && is_fcf_positive) {
      rec_title <- "推薦首選：自由現金流模型 (DCF)"
      rec_desc <- "偵測到該公司不配息，但具備強勁的現金造血能力。獲利皆用於內部再投資，應採用 FCFF 衡量企業真實內在價值。"
      rec_color <- "#9b59b6" 
      rec_icon <- "seedling"
    } else if (is_dividend_paying && is_fcf_positive) {
      rec_title <- "雙模型皆適用 (DCF & DDM 皆可)"
      rec_desc <- "該公司具備穩健的正向現金流，且維持配息政策。可結合 DCF (企業整體價值) 與 DDM (股東直接回報) 進行交叉驗證。"
      rec_color <- "#f39c12"
      rec_icon <- "balance-scale"
    } else {
      rec_title <- "⚠️ 傳統折現模型可能失效 (建議使用 RI 或 P/B)"
      rec_desc <- "警示：該公司不配息且自由現金流為負！傳統 DCF 與 DDM 皆難以定價。建議改採「剩餘收益模型 (Residual Income)」或關注其帳面價值。"
      rec_color <- "#d9534f"
      rec_icon <- "exclamation-triangle"
    }
    
    # -----------------------------------------------------
    # 2. 計算視覺化比例與狀態
    # -----------------------------------------------------
    all_vals <- c(p_curr, plot_dcf, plot_ddm)
    min_val <- min(all_vals) * 0.8  
    max_val <- max(all_vals) * 1.2  
    range_val <- max_val - min_val
    if (range_val == 0) range_val <- 1 
    
    pos_curr <- (p_curr - min_val) / range_val * 100
    pos_dcf <- (plot_dcf - min_val) / range_val * 100
    pos_ddm <- (plot_ddm - min_val) / range_val * 100
    
    disp_dcf <- if (!is.na(val_dcf)) paste0("$", round(val_dcf, 2)) else ""
    disp_ddm <- if (!is.na(val_ddm)) paste0("$", round(val_ddm, 2)) else ""
    op_dcf <- if (!is.na(val_dcf)) 1 else 0
    op_ddm <- if (!is.na(val_ddm)) 1 else 0
    
    status_text <- "合理區間"
    status_color <- "#f39c12" 
    
    if (!is.na(val_dcf) && !is.na(val_ddm)) {
      if (isTRUE(p_curr < val_ddm) && isTRUE(p_curr < val_dcf)) {
        status_text <- "💰 強烈低估 (低於 DDM 地板價)"
        status_color <- "#00a65a"
      } else if (isTRUE(p_curr > val_ddm) && isTRUE(p_curr > val_dcf)) {
        status_text <- "🔥 嚴重超買 (高於 DCF 成長價)"
        status_color <- "#d9534f"
      } else {
        status_text <- "⚖️ 落在合理估值光譜內"
        status_color <- "#f39c12"
      }
    } else if (!is.na(val_dcf)) {
      if (isTRUE(p_curr < val_dcf)) { status_text <- "💰 低估 (低於 DCF 成長價)"; status_color <- "#00a65a" }
      else { status_text <- "🔥 高估 (高於 DCF 成長價)"; status_color <- "#d9534f" }
    } else if (!is.na(val_ddm)) {
      if (isTRUE(p_curr < val_ddm)) { status_text <- "💰 低估 (低於 DDM 地板價)"; status_color <- "#00a65a" }
      else { status_text <- "🔥 高估 (高於 DDM 地板價)"; status_color <- "#d9534f" }
    }
    
    # -----------------------------------------------------
    # 3. 產出 HTML / CSS 視覺圖
    # -----------------------------------------------------
    HTML(glue::glue("
      <div style='background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; border-top: 3px solid {status_color};'>
        
        <div style='background: {rec_color}15; border-left: 5px solid {rec_color}; padding: 12px; border-radius: 4px; margin-bottom: 25px;'>
          <h5 style='color: {rec_color}; margin-top: 0; font-weight: bold;'>
            <i class='fa fa-{rec_icon}'></i> {rec_title}
          </h5>
          <p style='margin-bottom: 0; font-size: 13px; color: #555;'>{rec_desc}</p>
        </div>

        <h4 style='margin-top: 0; font-weight: bold;'>
          <i class='fa fa-balance-scale'></i> 綜合估值狀態：<span style='color: {status_color};'>{status_text}</span>
        </h4>
        
        <div style='position: relative; height: 60px; margin-top: 30px; margin-bottom: 10px;'>
          <div style='position: absolute; top: 25px; left: 0; right: 0; height: 10px; background: #ecf0f1; border-radius: 5px;'></div>
          
          <div style='position: absolute; top: 0; left: {pos_ddm}%; transform: translateX(-50%); text-align: center; opacity: {op_ddm}; transition: opacity 0.3s;'>
            <div style='font-size: 12px; color: #7f8c8d; font-weight: bold;'>DDM 估值</div>
            <div style='width: 4px; height: 35px; background: #3498db; margin: 0 auto;'></div>
            <div style='font-size: 14px; color: #3498db; font-weight: bold;'>{disp_ddm}</div>
          </div>
          
          <div style='position: absolute; top: 0; left: {pos_dcf}%; transform: translateX(-50%); text-align: center; opacity: {op_dcf}; transition: opacity 0.3s;'>
            <div style='font-size: 12px; color: #7f8c8d; font-weight: bold;'>DCF 估值</div>
            <div style='width: 4px; height: 35px; background: #9b59b6; margin: 0 auto;'></div>
            <div style='font-size: 14px; color: #9b59b6; font-weight: bold;'>{disp_dcf}</div>
          </div>
          
          <div style='position: absolute; top: -10px; left: {pos_curr}%; transform: translateX(-50%); text-align: center; z-index: 10;'>
            <div style='font-size: 13px; color: white; background: #2c3e50; padding: 2px 8px; border-radius: 4px; font-weight: bold;'>當前市價</div>
            <div style='width: 0; height: 0; border-left: 6px solid transparent; border-right: 6px solid transparent; border-top: 6px solid #2c3e50; margin: 0 auto;'></div>
            <div style='width: 14px; height: 14px; background: #2c3e50; border: 3px solid white; border-radius: 50%; margin: -2px auto 0; box-shadow: 0 0 4px rgba(0,0,0,0.3);'></div>
            <div style='font-size: 16px; color: #2c3e50; font-weight: bold; margin-top: 2px;'>${round(p_curr, 2)}</div>
          </div>
        </div>
      </div>
    "))
  })
  
  # ==========================================
  # 📊 9. 敏感度分析矩陣 (修復殘留變數 + 升級股數網羅)
  # ==========================================
  output$dcf_sensitivity_table <- renderTable({
    # 確保已經按下試算按鈕，且 FCFF 預測表已產生
    req(fcf_results$df_fcf(), input$calc) 
    
    df_fcf <- fcf_results$df_fcf()
    n_years <- as.numeric(input$years)
    
    req(nrow(df_fcf) == n_years)
    future_fcfs <- as.numeric(df_fcf$FCFF)
    fcf_n <- tail(future_fcfs, 1) 
    
    use_calc <- isTRUE(input$use_calculated_wacc) && !is.null(calculated_wacc())
    base_wacc <- if (use_calc) {
      calculated_wacc() * 100 
    } else if (input$dcf_mode == "gordon") {
      input$wacc_gordon 
    } else {
      input$wacc_stage1
    }
    
    base_g <- input$sgr
    
    if (is.null(base_wacc) || is.na(base_wacc)) base_wacc <- 10
    if (is.null(base_g) || is.na(base_g)) base_g <- 3
    
    # --- 取得估值必要的資產負債數據 ---
    latest_cash <- get_latest_cash_position(d_cash_flow())
    temp_debt <- select_clean_metric_row(d_balance_sheet(), "Total Debt")
    scraped_debt <- if (length(temp_debt) > 0 && !all(is.na(temp_debt))) as.numeric(na.omit(temp_debt))[1] else 0
    total_debt <- if (!is.null(input$manual_debt) && !is.na(input$manual_debt)) input$manual_debt else scraped_debt
    
    # 🌟 關鍵修復 1：同步套用多目標網羅股數 (避免又除以 1)
    temp_shares <- select_clean_metric_row(d_balance_sheet(), "Ordinary Shares Number|Share Issued|Total Shares Outstanding")
    shares <- if(length(temp_shares) > 0 && !all(is.na(temp_shares))) as.numeric(na.omit(temp_shares))[1] else 1
    
    wacc_range <- seq(base_wacc + 2, base_wacc - 2, length.out = 5)
    g_range <- seq(base_g - 1, base_g + 1, length.out = 5)
    
    sens_matrix <- matrix(NA, nrow = 5, ncol = 5, 
                          dimnames = list(paste0("WACC ", round(wacc_range, 1), "%"), 
                                          paste0("g ", round(g_range, 1), "%")))
    
    base_wacc_seq <- if (input$dcf_mode == "gordon") {
      rep(base_wacc / 100, n_years)
    } else {
      s1 <- as.numeric(input$yr_stage1)
      r2_base <- if(use_calc) base_wacc / 100 else input$wacc_stage2 / 100
      c(rep(base_wacc / 100, min(s1, n_years)), rep(r2_base, max(n_years - s1, 0)))
    }
    
    # --- 計算 5x5 矩陣情境 ---
    for (i in 1:5) { 
      for (j in 1:5) { 
        w_val <- wacc_range[i] / 100
        g_val <- g_range[j] / 100
        w_delta <- w_val - (base_wacc / 100) 
        
        scenario_w_seq <- base_wacc_seq + w_delta
        terminal_wacc <- tail(scenario_w_seq, 1)
        
        if (!is.na(terminal_wacc) && !is.na(g_val) && terminal_wacc > g_val) {
          discount_factors <- cumprod(1 + scenario_w_seq)
          pv_fcf <- sum(future_fcfs / discount_factors)
          tv <- (fcf_n * (1 + g_val)) / (terminal_wacc - g_val)
          pv_tv <- tv / discount_factors[n_years]
          
          # 🌟 關鍵修復 2：使用迴圈內算出的 ev，不再出現 dcf_value
          ev <- pv_fcf + pv_tv
          equity_val <- ev + latest_cash - total_debt
          
          if (!is.na(shares) && shares > 0) {
            sens_matrix[i, j] <- equity_val / shares
          }
        }
      }
    }
    
    out_df <- cbind(WACC_Rate = rownames(sens_matrix), as.data.frame(sens_matrix))
    return(out_df)
    
  }, digits = 2, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c', na = "無效 (W<g)")
  
  # ==========================================
  # 🛡️ 10. 數據缺漏檢查 UI 
  # ==========================================
  output$ui_data_validation <- renderUI({
    if (is.null(d_balance_sheet()) || is.null(d_cash_flow())) return(NULL)
    
    val_fcf  <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
    scraped_fcf <- if (length(val_fcf) > 0) as.numeric(na.omit(val_fcf))[1] else NA
    val_cash <- get_latest_cash_position(d_cash_flow())
    val_debt <- select_clean_metric_row(d_balance_sheet(), "Total Debt")
    scraped_debt <- if (length(val_debt) > 0) as.numeric(na.omit(val_debt))[1] else NA
    
    # 打包要檢查的項目
    check_list <- list(
      "Free Cash Flow (FCF)" = scraped_fcf,
      "Cash Position" = val_cash,
      "Total Debt" = scraped_debt
    )
    
    # 產生共用警示框，並自訂 DCF 專用的說明文字
    alert_box <- ui_missing_data_alert(
      check_list = check_list,
      fallback_msg = "無法從財報抓取上述數值。請在下方手動輸入以確保企業估值 (DCF) 計算準確。"
    )
    
    # 如果有缺值 (alert_box 不是 NULL)，就連同輸入框一起顯示出來
    if (!is.null(alert_box)) {
      box(title = "⚠️ 核心評價數據缺失提醒", status = "danger", width = 12, solidHeader = TRUE,
          alert_box, # 插入共用的警示橫幅
          fluidRow(
            if(is.na(scraped_fcf)) column(4, numericInput("manual_fcf", "手動 FCF:", value = NA)) else NULL,
            if(is.na(val_cash)) column(4, numericInput("manual_cash", "手動 Cash:", value = NA)) else NULL,
            if(is.na(scraped_debt)) column(4, numericInput("manual_debt", "手動 Debt:", value = NA)) else NULL
          )
      )
    } else {
      NULL
    }
  })
  
  # ==========================================
  # 11. 系統按鈕與報告輸出
  # ==========================================
  observeEvent(input$reset_ddm, {
    updateNumericInput(session, "ddm_app-d0", value = APP_DEFAULTS$ddm_d0)
    updateNumericInput(session, "ddm_app-g", value = APP_DEFAULTS$ddm_g)
    updateNumericInput(session, "ddm_app-ke", value = APP_DEFAULTS$ddm_ke)
    showNotification("🔁 DDM 模型參數已回復系統預設值", type = "message")
  })
  
  observeEvent(input$reset_dcf, {
    updateNumericInput(session, "years", value = APP_DEFAULTS$years)
    updateNumericInput(session, "sgr", value = APP_DEFAULTS$sgr)
    updateNumericInput(session, "wacc_gordon", value = APP_DEFAULTS$wacc_gordon)
    updateNumericInput(session, "yr_stage1", value = APP_DEFAULTS$yr_stage1)
    updateNumericInput(session, "g_stage2", value = APP_DEFAULTS$g_stage2)
    updateNumericInput(session, "wacc_stage1", value = APP_DEFAULTS$wacc_gordon)
    updateNumericInput(session, "wacc_stage2", value = APP_DEFAULTS$wacc_gordon)
    showNotification("🔁 所有 DCF 模型欄位已回復", type = "message")
  })
  
  output$download_report <- downloadHandler(
    filename = function() paste0("YNow_Report_", current_ticker(), "_", Sys.Date(), ".html"),
    content = function(file) {
      tryCatch({
        showNotification("正在生成完整分析報告，請稍候...", type = "message")
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
        plot_path <- NA
        if (exists("fcf_results") && !is.null(fcf_results$fcf_plot_obj())) {
          plot_path <- file.path(tempdir(), "fcf_plot_temp.png")
          ggsave(plot_path, plot = fcf_results$fcf_plot_obj(), width = 8, height = 5, dpi = 300)
        }
        rmarkdown::render(
          input = tempReport, output_file = file,
          params = list(
            stock_code = current_ticker(), 
            company_name = isolate(input$txt_corpname),
            current_price = "請參考儀表板", 
            dcf_value = "請參考儀表板", 
            margin_of_safety = NA, 
            fcf_plot_path = plot_path, 
            kpi = "請參考儀表板", 
            warnings = "無重大異常",
            summary_df = summary_data(), 
            income_df = d_income_statement(), 
            balance_df = d_balance_sheet(), 
            cashflow_df = d_cash_flow()
          ),
          envir = new.env(parent = globalenv())
        )
      }, error = function(e) { showNotification(paste("報告生成失敗:", e$message), type = "error") })
    }
  )
}
