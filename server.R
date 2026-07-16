# ==========================================
# server.R - 後端邏輯與資料運算 (專業財務修正版)
# ==========================================

server <- function(input, output, session) {
  
  # ==========================================
  # 🗄️ 全域資料容器 (儲存爬蟲結果與跨模組變數)
  # ==========================================
  summary_data <- reactiveVal(NULL)
  scraped_financials <- reactiveVal(NULL)
  is_expanded <- reactiveVal(FALSE) 
  
  values <- reactiveValues(recentsearch = c())
  corp_industry_text <- reactiveVal("等待搜尋...")
  
  # 初始值設為 NULL，避免一開啟 App 就自動執行爬蟲
  current_ticker <- reactiveVal(NULL)
  
  # 系統核心估值變數
  estimated_g <- reactiveVal(NULL)
  estimated_re <- reactiveVal(NULL)
  calculated_wacc <- reactiveVal(NULL)
  dcf_value_result <- reactiveVal(NULL)
  stock_price_estimate_val <- reactiveVal(NULL)
  
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
        incProgress(0.2, detail = "正在讀取 Summary（yfinance）...")
        sum_df <- get_summary_data(stock_code)
        summary_data(sum_df)

        ind_info <- get_yahoo_industry(stock_code)
        if (!is.null(ind_info)) corp_industry_text(ind_info$display_text)

        if (!(stock_code %in% values$recentsearch)) {
          values$recentsearch <- head(c(stock_code, values$recentsearch), 5)
        }

        incProgress(0.5, detail = "正在抓取財報明細（yfinance）...")
        res <- cached_scrape_financials(stock_code)
        res <- normalize_all_financials(res)
        scraped_financials(res)

        is_expanded(FALSE)
        updateActionButton(session, "btn_expand_all", label = "Expand All", icon = icon("expand"))

        incProgress(0.9, detail = "數據同步完成！✅")

      }, error = function(e) {
        showNotification(
          paste("❌ 獲取資料失敗，請確認代碼。錯誤:", e$message),
          type = "error",
          duration = 12
        )
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
  
  d_income_statement <- reactive({
    req(scraped_financials())
    reorder_financial_columns(scraped_financials()[["Income Statement"]]$expanded)
  })
  d_balance_sheet <- reactive({
    req(scraped_financials())
    reorder_financial_columns(scraped_financials()[["Balance Sheet"]]$expanded)
  })
  d_cash_flow <- reactive({
    req(scraped_financials())
    reorder_financial_columns(scraped_financials()[["Cash Flow"]]$expanded)
  })
  
  output$tbIncomeStatement <- renderDataTable({
    req(scraped_financials())
    df <- if (is_expanded()) scraped_financials()[["Income Statement"]]$expanded else scraped_financials()[["Income Statement"]]$collapsed
    df <- reorder_financial_columns(df)
    datatable(trim_financial_table(df, "Tax Effect of Unusual Items"), options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$tbBalanceSheet <- renderDataTable({
    req(scraped_financials())
    df <- if (is_expanded()) scraped_financials()[["Balance Sheet"]]$expanded else scraped_financials()[["Balance Sheet"]]$collapsed
    df <- reorder_financial_columns(df)
    datatable(trim_financial_table(df, "Treasury Shares Number"), options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$tbCashFlow <- renderDataTable({
    req(scraped_financials())
    df <- if (is_expanded()) scraped_financials()[["Cash Flow"]]$expanded else scraped_financials()[["Cash Flow"]]$collapsed
    df <- reorder_financial_columns(df)
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
    
    res <- d_income_statement()[grepl(keyword, d_income_statement()[[1]], ignore.case = TRUE), ]
    if(nrow(res) > 0) return(res[1, ])
    return(NULL)
  })
  
  output$is_plot <- renderPlotly({
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
    df <- d_cash_flow()
    keyword <- switch(input$cf_type,
                      "Operating Cash Flow" = "Operating Cash Flow",
                      "Investing Cash Flow" = "Investing Cash Flow",
                      "Financing Cash Flow" = "Financing Cash Flow")
    # 先精確匹配科目名，避免命中 "Cash Flow From Continuing ..." 等長名列
    exact <- which(tolower(trimws(df[[1]])) == tolower(keyword))
    if (length(exact) > 0) return(df[exact[1], , drop = FALSE])
    hit <- grepl(keyword, df[[1]], ignore.case = TRUE)
    if (!any(hit)) return(df[FALSE, , drop = FALSE])
    df[which(hit)[1], , drop = FALSE]
  })
  
  output$cf_plot <- renderPlotly({
    generate_safe_line_plot(
      data = selected_cashflow_data(), 
      ticker_name = current_ticker(), 
      metric_name = input$cf_type
    )
  })
  
  
  # ==========================================
  # 🔌 4. 呼叫外部模組 (KPI, FCF, DDM)
  # ==========================================
  
  # --- 新增 1：歷史股價抓取 (用於決策模組的動能分析) ---
  hist_stock_data <- reactive({
    req(current_ticker())
    tryCatch({
      # 使用 quantmod 抓取 Yahoo Finance 數據 (請確保已安裝 quantmod 套件)
      df <- quantmod::getSymbols(current_ticker(), src = "yahoo", auto.assign = FALSE, 
                                 from = Sys.Date() - 180, to = Sys.Date())
      df_final <- data.frame(Date=zoo::index(df), zoo::coredata(df))
      names(df_final) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      return(df_final)
    }, error = function(e) {
      warning("無法取得歷史股價: ", e$message)
      return(NULL)
    })
  })
  
  # --- 新增 2：掛載投資決策漏斗模組 (Decision Funnel) ---
  decision_server(
    id = "main_decision", 
    d_is = d_income_statement,
    d_bs = d_balance_sheet,
    d_cf = d_cash_flow,
    intrinsic_val_dcf = stock_price_estimate_val,
    intrinsic_val_ddm = reactive({ 
      if(!is.null(ddm_results$ddm_price)) ddm_results$ddm_price() else NA 
    }),
    intrinsic_val_pb = reactive({
      if (!is.null(pb_results$pb_price)) pb_results$pb_price() else NA
    }),
    current_price = reactive({ 
      req(scraped_market_cap())
      scraped_market_cap()$price 
    }),
    hist_price_data = hist_stock_data,
    industry_text = corp_industry_text
  )

  kpi_module_server("kpi", d_income_statement, d_balance_sheet, d_cash_flow, reactive(input$industry_choice))
  
  run_calc_trigger <- reactiveVal(0)
  observeEvent(input$calc, { run_calc_trigger(run_calc_trigger() + 1) })
  observeEvent(d_cash_flow(), { 
    req(is.data.frame(d_cash_flow()), nrow(d_cash_flow()) > 0)
    run_calc_trigger(run_calc_trigger() + 1) 
  })
  
  # ==========================================
  # 🧠 建立「中央折現率大腦」(統一供應 Ke 給各模組)
  # ==========================================
  central_ke <- reactive({
    if (isTRUE(input$use_estimated_re) && !is.null(estimated_re())) {
      estimated_re()
    } else if (!is.null(input$wacc_re)) {
      input$wacc_re / 100
    } else {
      if(!is.null(APP_DEFAULTS$ddm_ke)) APP_DEFAULTS$ddm_ke / 100 else 0.1
    }
  })
  
  # ==========================================
  # 掛載 DDM 模組 (🌟 套用中央大腦 Ke)
  # ==========================================
  ddm_results <- ddm_module_server(
    id = "mod_ddm", 
    ddm_g = reactive({ APP_DEFAULTS$ddm_g }), 
    ddm_ke = reactive({ central_ke() * 100 }),  # 🌟 連動！
    
    scraped_d0 = reactive({
      # 優先：財報推算每股股利；其次：Summary 股利欄
      cf <- d_cash_flow()
      bs <- d_balance_sheet()
      if (is.data.frame(cf) && nrow(cf) > 0 && is.data.frame(bs) && nrow(bs) > 0) {
        div_paid <- select_current_metric(cf, "Cash Dividends Paid", "flow")
        shares <- select_current_metric(bs, "Share Issued|Ordinary Shares Number", "stock")
        if (!is.na(div_paid) && !is.na(shares) && shares > 0) {
          return(round(abs(div_paid) / shares, 2))
        }
      }
      df <- summary_data()
      if (is.null(df)) return(NA)
      div_row <- df[grepl("Dividend", df$Item, ignore.case = TRUE), ]
      if (nrow(div_row) > 0) {
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
  # 呼叫 FCFF 模組
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
    yr_stage1 = reactive(input$yr_stage1),
    input_capex_rate = reactive(input$var_capex_rate),
    input_nwc_rate   = reactive(input$var_nwc_rate),
    input_manual_fcf = reactive(input$manual_fcf),
    calc_trigger = run_calc_trigger,
    global_est_g = estimated_g 
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
    n <- as.numeric(input$years)
    if (is.na(n) || n <= 1) return()
    safe_yr1 <- clamp_yr_stage1(n, input$yr_stage1, APP_DEFAULTS$yr_stage1)
    if (!identical(as.numeric(input$yr_stage1), as.numeric(safe_yr1))) {
      updateNumericInput(session, "yr_stage1", value = safe_yr1)
    }
  }, ignoreInit = TRUE)
  
  # ==========================================
  # 呼叫 RI (剩餘收益) 模組 (🌟 套用中央大腦 Ke)
  # ==========================================
  ri_results <- ri_module_server(
    id = "mod_ri", 
    d_income_statement = d_income_statement, 
    d_balance_sheet = d_balance_sheet, 
    d_cash_flow = d_cash_flow, 
    global_re = central_ke
  )
  
  # ==========================================
  # 呼叫 P/B／資產估值模組
  # ==========================================
  pb_results <- pb_asset_module_server(
    id = "mod_pb",
    d_balance_sheet = d_balance_sheet,
    d_income_statement = d_income_statement,
    current_price = reactive({
      tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    }),
    industry_choice = reactive(input$industry_choice),
    industry_text = corp_industry_text
  )
  
  # ==========================================
  # 🚨 6. 詐欺風險警示 (Fraud Risk Warnings)
  # ==========================================
  fraud_warnings <- reactiveValues(fcf = "", ocf = "", biz = "", cashback = "", debt = "")
  
  output$nofreecashflow <- renderText({
    fcf <- get_avg(select_clean_metric_row(d_cash_flow(), "Free Cash Flow", include_ttm = FALSE))
    fraud_warnings$fcf <- if (is.na(fcf)) "" else if (fcf < 0) "⚠️ 自由現金流為負數，可能營運困難或大量資本支出" else ""
    fraud_warnings$fcf
  })
  
  output$nooperatingcashflow <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow", include_ttm = FALSE))
    fraud_warnings$ocf <- if (is.na(ocf)) "" else if (ocf < 0) "⚠️ 營業現金流為負數，代表核心業務沒有產生現金" else ""
    fraud_warnings$ocf
  })
  
  output$notdoingbusiness <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow", include_ttm = FALSE))
    net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
    fraud_warnings$biz <- if (is.na(ocf) || is.na(net)) "" else if (ocf < net) "⚠️ 營業現金流低於淨利，帳面賺錢但現金未實現" else ""
    fraud_warnings$biz
  })
  
  output$notgettingcashback <- renderText({
    ocf <- get_avg(select_clean_metric_row(d_cash_flow(), "Operating Cash Flow", include_ttm = FALSE))
    net <- get_avg(select_clean_metric_row_any(d_income_statement(), NET_INCOME_PATTERNS, include_ttm = FALSE))
    fraud_warnings$cashback <- if (is.na(ocf) || is.na(net)) "" else if (net > 0 && ocf < 0) "⚠️ 淨利為正但現金流為負，獲利品質存疑" else ""
    fraud_warnings$cashback
  })
  
  output$highdebttoequity <- renderText({
    total_liabilities <- get_avg(select_clean_metric_row(d_balance_sheet(), "Total Debt", include_ttm = FALSE))
    total_equity <- get_avg(select_clean_metric_row_any(d_balance_sheet(), EQUITY_PATTERNS, include_ttm = FALSE))
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
  # --- 優化後的債務抓取：處理 Total Debt 不存在的情況 ---
  scraped_debt <- reactive({
    req(d_balance_sheet())
    df_bs <- d_balance_sheet()
    
    # 優先抓取 Total Debt，若無則嘗試「短期+長期」加總
    val <- select_clean_metric_row(df_bs, "^Total Debt$", include_ttm = FALSE)
    if (length(val) == 0 || all(is.na(val))) {
      st_debt <- select_clean_metric_row(df_bs, "Current Debt|Short Term Debt", include_ttm = FALSE)
      lt_debt <- select_clean_metric_row(df_bs, "Long Term Debt", include_ttm = FALSE)
      val <- sum(c(st_debt[1], lt_debt[1]), na.rm = TRUE)
    } else {
      val <- val[1]
    }
    
    return(ifelse(is.na(val), 0, val))
  })
  
  # --- 優化後的股數與市值計算 ---
  scraped_market_cap <- reactive({
    req(d_balance_sheet(), summary_data())
    
    # 1. 抓取股數：擴充匹配名稱
    raw_shares <- select_current_metric(d_balance_sheet(), "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock")
    shares <- as.numeric(raw_shares)
    if (is.na(shares) || shares <= 0) shares <- 1 
    
    # 2. 解析股價：處理字串格式
    df_sum <- summary_data()
    price_row <- df_sum[grep("Previous Close|Market Price", df_sum$Item), ]
    price_val <- if(nrow(price_row) > 0) parse_financial_number(price_row$Value[1]) else NA
    
    if (is.na(price_val)) return(list(e_val = NA, shares = shares, price = NA))
    
    return(list(
      e_val = shares * price_val,
      shares = shares,
      price = price_val
    ))
  })
  
  # --- 優化後的稅率計算 ---
  scraped_tax_rate <- reactive({
    req(d_income_statement())
    df_is <- d_income_statement()
    
    tax_exp <- select_current_metric(df_is, "Tax Provision", "flow")
    pre_tax_inc <- select_current_metric(df_is, "Pretax Income", "flow")
    
    # 邏輯優化：處理負稅率或極端值
    if (is.na(tax_exp) || is.na(pre_tax_inc) || pre_tax_inc <= 0) {
      return(21) # 預設法定稅率 (如美國 21%)
    } else {
      t_rate <- (tax_exp / pre_tax_inc) * 100
      return(max(0, min(t_rate, 35))) # 限制在合理區間 0~35%
    }
  })
  
  # --- 1. 渲染股權市值 (E) ---
  output$vbx_equity_val <- renderValueBox({
    mkt_data <- scraped_market_cap()
    valueBox(
      value = format_dollar_abbr(mkt_data$e_val),
      subtitle = "股權市值 (Market Equity - E)",
      icon = icon("coins"),
      color = "blue"
    )
  })
  
  # --- 2. 渲染總負債 (D) ---
  output$vbx_debt_val <- renderValueBox({
    d_val <- scraped_debt()
    valueBox(
      value = format_dollar_abbr(d_val),
      subtitle = "總負債 (Total Debt - D)",
      icon = icon("file-invoice-dollar"),
      color = "red"
    )
  })
  
  # --- 3. 渲染有效稅率 (T) ---
  output$vbx_tax_rate <- renderValueBox({
    t_rate <- scraped_tax_rate()
    valueBox(
      value = paste0(round(t_rate, 2), "%"),
      subtitle = "有效稅率 (Effective Tax Rate - T)",
      icon = icon("percent"),
      color = "purple"
    )
  })
  
  # 🎯 智慧標籤：市場報酬率 Rm (當數值等於預設時顯示藍色標籤)
  observeEvent(c(input$capm_rm, input$industry_choice), {
    req(input$industry_choice)
    default_rm <- if (!is.null(industry_standards[[input$industry_choice]]$rm_avg)) 
      industry_standards[[input$industry_choice]]$rm_avg else 8.0
    
    if (!is.null(input$capm_rm) && abs(as.numeric(input$capm_rm) - default_rm) < 1e-4) {
      updateNumericInput(session, "capm_rm", 
                         label = HTML("Rm <span style='color: #2980b9; font-size: 12px;'>[套用產業平均值]</span>"))
    } else {
      updateNumericInput(session, "capm_rm", 
                         label = HTML("Rm <span style='color: #e67e22; font-size: 12px;'>[自訂數值]</span>"))
    }
  }, ignoreInit = FALSE)
  
  # 🎯 智慧標籤：貝他係數 Beta (當數值等於預設時顯示藍色標籤)
  observeEvent(c(input$capm_beta, input$industry_choice), {
    req(input$industry_choice)
    default_beta <- if (!is.null(industry_standards[[input$industry_choice]]$beta_avg)) 
      industry_standards[[input$industry_choice]]$beta_avg else 1.0
    
    if (!is.null(input$capm_beta) && abs(as.numeric(input$capm_beta) - default_beta) < 1e-4) {
      updateNumericInput(session, "capm_beta", 
                         label = HTML("Beta (β) <span style='color: #2980b9; font-size: 12px;'>[套用產業平均值]</span>"))
    } else {
      updateNumericInput(session, "capm_beta", 
                         label = HTML("Beta (β) <span style='color: #e67e22; font-size: 12px;'>[自訂數值]</span>"))
    }
  }, ignoreInit = FALSE)
  
  # 保留：切換產業時，強制將輸入框的值刷新為該產業預設值
  observeEvent(input$industry_choice, {
    req(input$industry_choice)
    inds <- industry_standards[[input$industry_choice]]
    if (!is.null(inds)) {
      updateNumericInput(session, "capm_beta", value = inds$beta_avg)
      updateNumericInput(session, "capm_rm", value = inds$rm_avg)
      
      # 同步短期成長／P/B 區間（有設定才更新）
      if (!is.null(inds$rev_growth)) {
        g_mid <- round(max(2, min(mean(inds$rev_growth), 12)), 2)
        updateNumericInput(session, "custom_g", value = g_mid)
        updateNumericInput(session, "g_stage1", value = g_mid)
      }
      if (!is.null(inds$pb_band) && length(inds$pb_band) >= 2) {
        lo <- inds$pb_band[1]; hi <- inds$pb_band[2]
        mid <- if (length(inds$pb_band) >= 3) inds$pb_band[3] else mean(c(lo, hi))
        updateNumericInput(session, "mod_pb-pb_low",  value = round(lo, 2))
        updateNumericInput(session, "mod_pb-pb_mid",  value = round(mid, 2))
        updateNumericInput(session, "mod_pb-pb_high", value = round(hi, 2))
      }
    }
  })
  
  output$txt_display_years <- renderUI({
    HTML(paste0("<b>目前預測年數：<span style='color:red; font-size:16px;'>", input$years, "</span> 年</b>"))
  })
  
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
      fcff_vals <- extract_fcff_series(df)
      cat("✅ FCFF 預測資料已同步！\n-------------------------\n")
      cat("第 1 年預測現金流:", if (length(fcff_vals) > 0) round(fcff_vals[1], 2) else "N/A", "\n")
      cat("第", nrow(df), "年預測現金流:", if (length(fcff_vals) > 0) round(tail(fcff_vals, 1), 2) else "N/A", "\n")
      cat("DCF 模式:", input$dcf_mode, "\n")
    }
  })
  
  estimated_g_meta <- reactiveValues(method = NULL, fund_res = NULL)
  observe({
    req(d_cash_flow(), d_income_statement(), d_balance_sheet(), input$g_growth_method)
    method <- input$g_growth_method
    if (is.null(method)) return()
    
    vec_fcf <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow", include_ttm = FALSE)
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
      ebit <- select_current_metric(d_income_statement(), "Operating Income|EBIT", "flow")
      tax_rate <- if(!is.null(input$wacc_tax)) input$wacc_tax / 100 else APP_DEFAULTS$wacc_tax
      nopat <- ebit * (1 - tax_rate)
      
      total_assets <- select_current_metric(d_balance_sheet(), "Total Assets", "stock")
      curr_liab <- select_current_metric(d_balance_sheet(), "Total Current Liabilities|Current Liabilities", "stock")
      st_debt <- select_current_metric(d_balance_sheet(), "Current Debt|Short Term Debt", "stock")
      cash_eq <- select_current_metric(d_balance_sheet(), "Cash And Cash Equivalents|Cash & Cash Equivalents", "stock")
      
      st_debt <- ifelse(is.na(st_debt), 0, st_debt)
      curr_liab <- ifelse(is.na(curr_liab), 0, curr_liab)
      cash_eq <- ifelse(is.na(cash_eq), 0, cash_eq)
      total_assets <- ifelse(is.na(total_assets), 0, total_assets)
      
      invested_capital <- (total_assets - cash_eq) - (curr_liab - st_debt)
      roic <- if(!is.na(invested_capital) && invested_capital > 0) nopat / invested_capital else 0
      
      capex <- abs(select_current_metric(d_cash_flow(), "Capital Expenditure", "flow"))
      depre <- select_current_metric(d_cash_flow(), "Depreciation", "flow")
      cf_delta_nwc <- select_current_metric(d_cash_flow(), "Change In Working Capital|Changes In Working Capital", "flow")
      
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
      
      # 聽從 FCFF 模組命名空間的天花板勾選框
      ceiling_ns <- input[["mod_fcf-apply_g_ceiling"]]
      apply_ceiling <- if (!is.null(ceiling_ns)) isTRUE(ceiling_ns) else TRUE
      
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
    estimated_g_meta$method <- method
    estimated_g_meta$fund_res <- fund_res
    updateSelectInput(session, "g_growth_method", label = paste0("預估 FCFF 成長率 ➔ ", val, " %"))
    
    if (method != "custom" && !is.na(val) && !identical(input$dcf_mode, "two_stage")) {
      updateNumericInput(session, "g_stage1", value = val)
    }
  })
  
  output$g_result <- renderUI({
    method <- estimated_g_meta$method
    fund_res <- estimated_g_meta$fund_res
    if (is.null(method)) return(NULL)
    
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
    req(fcf_results$df_fcf()) 
    df <- fcf_results$df_fcf() 
    
    ggplot(df, aes(x = Year)) +
      geom_bar(aes(y = FCFF, fill = FCFF > 0), stat = "identity", width = 0.6, alpha = 0.8) +
      scale_fill_manual(values = c("TRUE" = "#00a65a", "FALSE" = "#d9534f"), guide = "none") +
      geom_line(aes(y = NOPAT, group = 1, color = "預估稅後營業利潤 (NOPAT)"), size = 1.5) +
      geom_point(aes(y = NOPAT), size = 3, color = "#3c8dbc") +
      scale_color_manual(name = "", values = c("預估稅後營業利潤 (NOPAT)" = "#3c8dbc")) +
      geom_text(aes(y = FCFF, label = paste0("$", round(FCFF, 1))), 
                vjust = ifelse(df$FCFF >= 0, -0.5, 1.5), size = 4, fontface = "bold") +
      theme_minimal() +
      labs(title = "FCFF 與 營業利潤 成長軌跡", x = "預測年份", y = "金額 (百萬)") +
      theme(
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
    
    # 1. 確保有抓到資產負債表
    req(d_balance_sheet()) 
    
    # 2. 正確從資產負債表中取得在外流通股數 (Share Issued)
    shares <- select_current_metric(d_balance_sheet(), "Share Issued|Ordinary Shares Number", "stock")
    
    # 萬一抓不到資料的防呆處理
    if (is.na(shares) || shares == 0) {
      showNotification("無法取得股數，請確認資料來源！", type = "error")
      return()
    }
    
    df_sum <- summary_data()
    price_str <- if("Previous Close" %in% df_sum$Item) df_sum$Value[df_sum$Item == "Previous Close"] else NA
    price_val <- parse_financial_number(price_str)
    
    equity_mv <- if (!is.na(price_val) && shares > 0) shares * price_val else select_current_metric(d_balance_sheet(), "Common Stock Equity", "stock")
    
    debt <- select_current_metric(d_balance_sheet(), "Total Debt", "stock")
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
    
    fcff_vals <- extract_fcff_series(proj_df)
    discount_factors <- cumprod(1 + wacc_val)
    proj_df$DCF <- round(fcff_vals / discount_factors, 2)
    
    g_terminal <- if (is.numeric(input$sgr)) input$sgr / 100 else 0.03
    terminal_wacc <- tail(wacc_val, 1)
    tv_annotation <- ""
    
    if (!is.na(terminal_wacc) && !is.na(g_terminal) && terminal_wacc > g_terminal) {
      last_fcf <- tail(fcff_vals, 1)
      tv <- (last_fcf * (1 + g_terminal)) / (terminal_wacc - g_terminal)
      pv_tv <- tv / discount_factors[n_years]
      
      proj_df$DCF[n_years] <- round(proj_df$DCF[n_years] + pv_tv, 2)
      tv_annotation <- paste0("\n( 💡 第 ", n_years, " 年的 DCF 點位已包含永續終值現值 PV of TV: $", scales::comma(round(pv_tv, 2)), " )")
    }
    
    plot_df <- data.frame(
      Year   = rep(proj_df$Year, 2),
      Value  = c(fcff_vals, proj_df$DCF),
      Metric = factor(rep(c("預測現金流 (FCFF)", "折現後價值 (DCF)"), each = n_years),
                      levels = c("預測現金流 (FCFF)", "折現後價值 (DCF)"))
    )
    
    ggplot(plot_df, aes(x = Year, y = Value, color = Metric, linetype = Metric, group = Metric)) +
      geom_line(linewidth = 1.2) + 
      geom_point(size = 3) +
      geom_text(aes(label = scales::comma(Value)), 
                vjust = -1.5, size = 4.5, show.legend = FALSE) +
      scale_color_manual(values = c("預測現金流 (FCFF)" = "#95a5a6", "折現後價值 (DCF)" = "#e74c3c")) +      
      theme_minimal(base_size = 14) + 
      labs(title = paste0(current_ticker(), " - FCFF vs DCF 現值折現軌跡圖"), 
           subtitle = tv_annotation, 
           x = "年份", y = "USD (Millions)") + 
      theme(legend.position = "top", 
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(color = "#8e44ad", face = "bold", hjust = 0.5)) 
  })
  
  output$dft_fcf_plot <- renderPlot({
    df <- fcf_results$df_fcf()
    if (is.null(df) || nrow(df) == 0) { plot.new(); text(0.5, 0.5, "⏳ 等待財報資料匯入...", cex = 1.4); return() }
    fcff_vals <- extract_fcff_series(df)
    plot_df <- data.frame(Year = df$Year, FCFF = fcff_vals, stringsAsFactors = FALSE)
    plot_df <- plot_df[!is.na(plot_df$FCFF), ]
    if (nrow(plot_df) == 0) { plot.new(); text(0.5, 0.5, "⏳ 等待財報資料匯入...", cex = 1.4); return() }
    
    ggplot(plot_df, aes(x = Year, y = FCFF, group = 1)) + 
      geom_line(linewidth = 1.2, color = "steelblue") + 
      geom_point(aes(color = FCFF < 0), size = 3) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "steelblue"), guide = "none") + 
      theme_minimal(base_size = 14) +
      labs(title = "📉 FCFF 預測即時預覽", x = "預測期", y = "FCFF (USD)") + theme(legend.position = "top")
  })
  
  # ==========================================
  # 💰 8. DCF 計算核心與企業估值 (對接 FCFF 預測序列)
  # ==========================================
  observeEvent(input$calc, {
    req(current_ticker(), input$dcf_mode, input$years, fcf_results$df_fcf()) 
    
    n <- as.numeric(input$years)
    if (is.na(n) || n <= 0) return(NULL)
    
    proj_df <- fcf_results$df_fcf()
    future_fcfs <- extract_fcff_series(proj_df)
    
    if (length(future_fcfs) != n) {
      showNotification("⚠️ 預測年數與 FCFF 表格不符，請重新計算", type = "error")
      return(NULL)
    }
    
    dcf_value <- NA
    g_terminal <- input$sgr / 100
    use_calc_wacc <- isTRUE(input$use_calculated_wacc) && !is.null(calculated_wacc())
    
    if (input$dcf_mode == "gordon") {
      req(input$sgr, input$wacc_gordon)
      r1 <- if(use_calc_wacc) calculated_wacc() else (input$wacc_gordon / 100)
      r2 <- r1 
      
      if (!is.na(r2) && g_terminal >= r2) { 
        showNotification("❌ 成長率 g 必須嚴格小於折現率 WACC", type = "error")
        return(NULL) 
      }
      discount_factors <- cumprod(1 + rep(r1, n))
      
    } else {
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
      
      yr1 <- clamp_yr_stage1(n, input$yr_stage1, APP_DEFAULTS$yr_stage1)
      if (yr1 <= 0 || yr1 >= n) {
        showNotification("⚠️ 第一階段年數無效 (需大於 0 且小於預測總年數 n)", type = "error")
        return(NULL) 
      }
      
      wacc_sequence <- c(rep(r1, min(yr1, n)), rep(r2, max(0, n - yr1)))
      discount_factors <- cumprod(1 + wacc_sequence)
    }
    
    pv_forecast <- sum(future_fcfs / discount_factors)
    last_fcf <- future_fcfs[n]
    tv <- (last_fcf * (1 + g_terminal)) / (r2 - g_terminal)
    pv_tv <- tv / discount_factors[n]
    
    dcf_value <- pv_forecast + pv_tv
    dcf_value_result(dcf_value)
    
    # ==========================================
    # 🌟 執行橋接參數抓取：現金、負債、股數 (防呆強化版)
    # ==========================================
    
    # 1. 抓取現金 (Cash) - 涵蓋所有可能的 Yahoo Finance 命名，找不到強制設 0
    raw_cash <- select_current_metric(d_balance_sheet(), "Cash.*Equivalents.*Investments|Cash And Cash Equivalents|^Total Cash$", "stock")
    scraped_cash <- ifelse(is.na(raw_cash), 0, raw_cash)
    latest_cash <- if (!is.null(input$manual_cash) && !is.na(input$manual_cash)) input$manual_cash else scraped_cash
    
    raw_total_debt <- select_current_metric(d_balance_sheet(), "^Total Debt$", "stock")
    if (is.na(raw_total_debt)) {
      st_debt <- select_current_metric(d_balance_sheet(), "Current Debt|Short Term Debt", "stock")
      lt_debt <- select_current_metric(d_balance_sheet(), "Long Term Debt", "stock")
      st_debt <- ifelse(is.na(st_debt), 0, st_debt)
      lt_debt <- ifelse(is.na(lt_debt), 0, lt_debt)
      scraped_debt <- st_debt + lt_debt
    } else {
      scraped_debt <- raw_total_debt
    }
    latest_debt <- if (!is.null(input$manual_debt) && !is.na(input$manual_debt)) input$manual_debt else scraped_debt
    
    raw_shares <- select_current_metric(d_balance_sheet(), "Ordinary Shares Number|Share Issued|Total Shares Outstanding|Basic Average Shares", "stock")
    share_outstanding <- ifelse(is.na(raw_shares) || raw_shares <= 0, 1, raw_shares)
    
    # ==========================================
    # 🌟 執行橋接：企業價值 (EV) 轉 股權價值 (Equity Value)
    # ==========================================
    equity_value <- as.numeric(dcf_value)[1] + latest_cash - latest_debt
    
    # 計算每股目標價並防呆
    if (!is.na(equity_value) && share_outstanding > 1) {
      stock_price_estimate_val(equity_value / share_outstanding)
    } else {
      stock_price_estimate_val(NULL)
      # 如果股數回傳 1 (代表剛剛抓不到被我們設為預設值 1)，則跳出明確警告
      showNotification("⚠️ 警告：無法計算目標股價，未找到流通在外股數 (Shares Outstanding) 資料", type = "warning")
    }
    
    wacc_source <- if(use_calc_wacc) "系統估算值" else "手動輸入值"
    showNotification(glue::glue("✅ 估值更新：已成功將模組 FCFF 序列套入 DCF 運算引擎 (採用 {wacc_source} WACC)"), type = "message")
  })
  
  # ==========================================
  # 渲染估值結果與 InfoBox
  # ==========================================
  output$vtxt_dcf_results <- renderText({
    ev_val <- dcf_value_result()
    stock_val <- stock_price_estimate_val()
    
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
  # 📊 9. 敏感度分析矩陣 
  # ==========================================
  output$dcf_sensitivity_table <- renderTable({
    req(fcf_results$df_fcf(), input$calc) 
    
    df_fcf <- fcf_results$df_fcf()
    n_years <- as.numeric(input$years)
    
    req(nrow(df_fcf) == n_years)
    future_fcfs <- extract_fcff_series(df_fcf)
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
    
    latest_cash <- get_latest_cash_position(d_cash_flow())
    temp_debt <- select_current_metric(d_balance_sheet(), "Total Debt", "stock")
    total_debt <- if (!is.null(input$manual_debt) && !is.na(input$manual_debt)) input$manual_debt else ifelse(is.na(temp_debt), 0, temp_debt)
    
    shares <- select_current_metric(d_balance_sheet(), "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock")
    if (is.na(shares) || shares <= 0) shares <- 1
    
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
    
    scraped_fcf <- select_current_metric(d_cash_flow(), "Free Cash Flow", "flow")
    
    val_cash_raw <- select_current_metric(d_balance_sheet(), "Cash, Cash Equivalents & Short Term Investments|Cash And Cash Equivalents", "stock")
    val_cash <- val_cash_raw
    
    val_debt <- select_current_metric(d_balance_sheet(), "Total Debt", "stock")
    scraped_debt <- val_debt
    
    check_list <- list(
      "Free Cash Flow (FCF)" = scraped_fcf,
      "Cash Position" = val_cash,
      "Total Debt" = scraped_debt
    )
    
    alert_box <- ui_missing_data_alert(
      check_list = check_list,
      fallback_msg = "無法從財報抓取上述數值。請在下方手動輸入以確保企業估值 (DCF) 計算準確。"
    )
    
    if (!is.null(alert_box)) {
      box(title = "⚠️ 核心評價數據缺失提醒", status = "danger", width = 12, solidHeader = TRUE,
          alert_box, 
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
  
  output$bt_equity_plot <- renderPlotly({
    req(input$run_bt) # 只有按下執行按鈕才計算
    
    # 模擬數據生成 (實作時請替換成你的回測運算結果)
    dates <- seq(as.Date("2020-01-01"), Sys.Date(), by="day")
    n <- length(dates)
    
    # 模擬三條曲線：模式 A (高波動)、模式 B (穩健)、大盤
    set.seed(42)
    bench <- cumprod(1 + rnorm(n, 0.0003, 0.01))
    model_a <- cumprod(1 + rnorm(n, 0.0005, 0.015)) 
    model_b <- cumprod(1 + rnorm(n, 0.0004, 0.008))
    
    df_plot <- data.frame(
      Date = dates,
      Benchmark = bench,
      Model_A = model_a,
      Model_B = model_b
    )
    
    # 使用 ggplot2 繪圖並轉換為 plotly
    p <- ggplot(df_plot, aes(x = Date)) +
      geom_line(aes(y = Model_A, color = "模式 A (情緒增強)"), size = 1) +
      geom_line(aes(y = Model_B, color = "模式 B (純基本面)"), size = 1) +
      geom_line(aes(y = Benchmark, color = "大盤基準"), linetype = "dashed") +
      scale_color_manual(values = c("模式 A (情緒增強)" = "#007bff", 
                                    "模式 B (純基本面)" = "#dc3545", 
                                    "大盤基準" = "#6c757d")) +
      labs(y = "累積淨值", x = "日期", color = "策略") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  output$perf_metrics <- renderUI({
    # 假設這些數值來自你的回測模組
    fluidRow(
      column(4,
             tipify(valueBox("1.25", "Sharpe Ratio", icon = icon("chart-line"), color = "green"),
                    "承受單位總風險帶來的超額回報。", placement = "bottom")
      ),
      column(4,
             tipify(valueBox("-12%", "Max Drawdown", icon = icon("descending"), color = "red"),
                    "資金最大回撤幅度，衡量策略在極端情況下的抗壓能力。", placement = "bottom")
      ),
      column(4,
             tipify(valueBox("穩定", "參數高原", icon = icon("mountain"), color = "purple"),
                    "調整成長率 g 績效是否崩盤？若穩定代表無過度擬合。", placement = "bottom")
      )
    )
  })
  
  # ==========================================
  # 11. 系統按鈕與報告輸出
  # ==========================================
  # DDM Reset 由 ddm_module_server 內的 input$reset_ddm 處理（ns: mod_ddm）
  
  observeEvent(input$reset_dcf, {
    updateNumericInput(session, "years", value = APP_DEFAULTS$years)
    updateNumericInput(session, "sgr", value = APP_DEFAULTS$sgr)
    updateNumericInput(session, "wacc_gordon", value = APP_DEFAULTS$wacc_gordon)
    updateNumericInput(session, "yr_stage1", value = APP_DEFAULTS$yr_stage1)
    updateNumericInput(session, "g_stage1", value = APP_DEFAULTS$g_stage1)
    updateNumericInput(session, "wacc_stage1", value = APP_DEFAULTS$wacc_gordon)
    updateNumericInput(session, "wacc_stage2", value = APP_DEFAULTS$wacc_gordon)
    showNotification("🔁 所有 DCF 模型欄位已回復", type = "message")
  })
  
  output$download_report <- downloadHandler(
    filename = function() paste0("YNow_Report_", current_ticker(), "_", Sys.Date(), ".html"),
    content = function(file) {
      tryCatch({
        showNotification("正在生成投資意見報告，請稍候...", type = "message")
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
        
        plot_path <- NA
        if (exists("fcf_results") && !is.null(fcf_results$fcf_plot_obj())) {
          plot_path <- file.path(tempdir(), "fcf_plot_temp.png")
          ggsave(plot_path, plot = fcf_results$fcf_plot_obj(), width = 9, height = 5.5, dpi = 300)
        }
        
        # --- 蒐集報告所需即時資料 ---
        cur_price <- tryCatch(isolate(scraped_market_cap()$price), error = function(e) NA)
        tgt_price <- isolate(stock_price_estimate_val())
        ddm_val <- tryCatch(isolate(ddm_results$ddm_price()), error = function(e) NA)
        pb_val <- tryCatch(isolate(pb_results$pb_price()), error = function(e) NA)
        ev_val <- isolate(dcf_value_result())
        
        ind_text_early <- isolate(corp_industry_text())
        rating_anchor <- if (!is.na(pb_val) && grepl("Bank|Insurance|Financial|Conglomerate", ind_text_early, ignore.case = TRUE)) {
          pb_val
        } else if (!is.na(tgt_price)) {
          tgt_price
        } else {
          pb_val
        }
        rating_info <- derive_investment_rating(cur_price, rating_anchor)
        val_method <- derive_valuation_method(isolate(d_cash_flow()), industry_text = ind_text_early)
        
        sum_df <- isolate(summary_data())
        co_name <- isolate(attr(sum_df, "company_name"))
        if (is.null(co_name) || is.na(co_name) || co_name == "") co_name <- isolate(current_ticker())
        
        ind_text <- isolate(corp_industry_text())
        sector_str <- "N/A"; industry_str <- "N/A"
        if (!is.null(ind_text) && grepl("\\|", ind_text)) {
          parts <- strsplit(ind_text, "\\|")[[1]]
          sector_str <- trimws(sub("Sector:\\s*", "", parts[1]))
          if (length(parts) > 1) industry_str <- trimws(sub("Industry:\\s*", "", parts[2]))
        }
        
        use_calc_wacc <- isTRUE(isolate(input$use_calculated_wacc)) && !is.null(isolate(calculated_wacc()))
        wacc_str <- if (use_calc_wacc) {
          paste0(round(isolate(calculated_wacc()) * 100, 2), "% (CAPM 估算)")
        } else if (isolate(input$dcf_mode) == "gordon") {
          paste0(isolate(input$wacc_gordon), "% (手動)")
        } else {
          paste0(isolate(input$wacc_stage1), "% / ", isolate(input$wacc_stage2), "% (兩階段)")
        }
        
        warn_msgs <- collect_fraud_warnings(
          isolate(d_cash_flow()), isolate(d_income_statement()), isolate(d_balance_sheet())
        )
        
        highlights <- c()
        if (!is.na(rating_info$upside_pct)) {
          highlights <- c(highlights, sprintf(
            "依 %s 估值，目標價 %s，潛在報酬 %+.1f%%，評等「%s」。",
            val_method$method,
            ifelse(is.na(rating_anchor), "N/A", paste0("$", round(rating_anchor, 2))),
            rating_info$upside_pct, rating_info$rating
          ))
        }
        if (!is.na(ev_val)) highlights <- c(highlights, paste0("企業價值 (EV) 估算：", format_dollar_abbr(ev_val), "。"))
        if (!is.na(ddm_val)) highlights <- c(highlights, paste0("DDM 交叉驗證合理價：$", round(ddm_val, 2), "。"))
        if (!is.na(pb_val)) highlights <- c(highlights, paste0("P/B 基準合理價：$", round(pb_val, 2), "。"))
        highlights <- c(highlights, val_method$rationale)
        
        rmarkdown::render(
          input = tempReport, output_file = file,
          params = list(
            stock_code = isolate(current_ticker()),
            company_name = co_name,
            sector = sector_str,
            industry = industry_str,
            report_date = format(Sys.Date(), "%Y/%m/%d"),
            rating = rating_info$rating,
            rating_en = rating_info$rating_en,
            rating_color = rating_info$rating_color,
            current_price = cur_price,
            target_price = rating_anchor,
            upside_pct = rating_info$upside_pct,
            ddm_value = ddm_val,
            pb_value = pb_val,
            ev_value = ev_val,
            margin_of_safety = rating_info$margin_of_safety,
            primary_method = val_method$method,
            method_rationale = val_method$rationale,
            wacc = wacc_str,
            terminal_growth = paste0(isolate(input$sgr), "%"),
            forecast_years = isolate(input$years),
            dcf_mode = isolate(input$dcf_mode),
            market_cap = extract_summary_item(sum_df, "Market Cap"),
            pe_ratio = extract_summary_item(sum_df, "PE Ratio|Trailing P/E"),
            beta = extract_summary_item(sum_df, "^Beta"),
            dividend_yield = extract_summary_item(sum_df, "Yield|Dividend"),
            kpi_df = build_report_kpi_df(
              isolate(d_income_statement()), isolate(d_balance_sheet()), isolate(d_cash_flow())
            ),
            fcf_plot_path = plot_path,
            warnings = if (length(warn_msgs) > 0) paste(warn_msgs, collapse = "\n") else "",
            investment_highlights = highlights,
            summary_df = sum_df,
            income_df = trim_report_table(isolate(d_income_statement())),
            balance_df = trim_report_table(isolate(d_balance_sheet())),
            cashflow_df = trim_report_table(isolate(d_cash_flow()))
          ),
          envir = new.env(parent = globalenv())
        )
        showNotification("✅ 投資意見報告已產出", type = "message")
      }, error = function(e) {
        showNotification(paste("報告生成失敗:", e$message), type = "error")
      })
    }
  )
}
