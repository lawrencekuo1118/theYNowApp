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

  # CAPM Beta：手動覆寫旗標（換 ticker 後清除，重新跟 Finance Summary 同步）
  capm_beta_dirty <- reactiveVal(FALSE)
  capm_beta_updating <- reactiveVal(FALSE)
  
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
  # 🔎 主搜尋框預選清單（黑字；側邊欄維持原 UI）
  # ==========================================
  sc_datalist_choices <- reactiveVal(TICKER_PRESETS)

  output$sc_ticker_suggest_ui <- renderUI({
    ch <- sc_datalist_choices()
    if (is.null(ch) || length(ch) == 0) ch <- TICKER_PRESETS
    labs <- names(ch)
    if (is.null(labs)) labs <- unname(ch)
    labs[!nzchar(labs)] <- unname(ch)[!nzchar(labs)]
    n <- min(length(ch), 12L)
    tags$div(
      id = "sc_ticker_suggest",
      role = "listbox",
      lapply(seq_len(n), function(i) {
        sym <- as.character(unname(ch)[[i]])
        lab <- as.character(labs[[i]])
        extra <- sub(paste0("^", sym, "(\\s|[—\\-–])+"), "", lab, perl = TRUE)
        tags$button(
          type = "button",
          class = "ynow-suggest-item",
          `data-symbol` = sym,
          tags$span(class = "ynow-suggest-sym", sym),
          if (nzchar(trimws(extra)) && !identical(trimws(extra), sym)) {
            tags$span(class = "ynow-suggest-lab", extra)
          }
        )
      })
    )
  })

  session$onFlushed(function() {
    sc_datalist_choices(TICKER_PRESETS)
  }, once = TRUE)

  ticker_typeahead_q <- shiny::debounce(
    reactive({ input$ticker_typeahead }),
    millis = 280
  )

  observeEvent(ticker_typeahead_q(), {
    q <- trimws(as.character(ticker_typeahead_q() %||% ""))
    if (!nzchar(q)) {
      base <- TICKER_PRESETS
      recent <- values$recentsearch
      if (length(recent)) {
        recent <- unique(toupper(trimws(recent)))
        recent_named <- stats::setNames(recent, recent)
        base <- c(recent_named, base[!(unname(base) %in% recent)])
      }
      sc_datalist_choices(base)
      return()
    }
    hits <- tryCatch(search_ticker_choices(q), error = function(e) TICKER_PRESETS)
    sc_datalist_choices(hits)
  }, ignoreInit = TRUE)

  observeEvent(current_ticker(), {
    tk <- current_ticker()
    req(nzchar(tk))
    base <- sc_datalist_choices()
    if (is.null(base)) base <- TICKER_PRESETS
    if (!(tk %in% unname(base))) {
      sc_datalist_choices(c(stats::setNames(tk, tk), base))
    }
  }, ignoreInit = TRUE)
  
  # ==========================================
  # 🌐 核心爬蟲：只要中央大腦的代碼改變，就自動執行完整抓取
  # ==========================================
  observeEvent(current_ticker(), {
    req(current_ticker())
    # 換股票：解除手動覆寫，讓新 Summary β 可自動帶入
    capm_beta_dirty(FALSE)
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

        # 先更新即時 Rf，其餘 CAPM／WACC 在財報 reactive 就緒後自動估算
        tryCatch({
          rf_now <- cached_get_risk_free_rate()
          if (is.finite(rf_now) && rf_now > 0) {
            updateNumericInput(session, "capm_rf", value = round(as.numeric(rf_now), 2))
          }
        }, error = function(e) NULL)

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
  
  output$fs_summary_ui <- renderUI({
    req(summary_data())
    df <- summary_data()
    if (is.null(df) || nrow(df) < 1) {
      return(tags$p("No finance summary available.", style = "color:#888;"))
    }

    # 分組僅影響版面；所有 Item/Value 皆會輸出（未歸類者歸入 Other）
    groups <- list(
      Price = c("Previous Close", "Open", "Bid", "Ask", "Day's Range", "52 Week Range"),
      Volume = c("Volume", "Avg. Volume"),
      Valuation = c("Market Cap (intraday)", "Beta (5Y Monthly)", "PE Ratio (TTM)", "EPS (TTM)", "Target Est"),
      Dividend = c("Dividend", "Yield")
    )
    known <- unique(unlist(groups, use.names = FALSE))
    leftover <- setdiff(as.character(df$Item), known)
    if (length(leftover) > 0) groups$Other <- leftover

    mk_card <- function(item, value) {
      tags$div(
        class = "ynow-fs-card",
        tags$div(class = "ynow-fs-label", item),
        tags$div(class = "ynow-fs-value", value)
      )
    }

    sections <- lapply(names(groups), function(gname) {
      items <- groups[[gname]]
      rows <- df[match(items, df$Item), , drop = FALSE]
      rows <- rows[!is.na(rows$Item), , drop = FALSE]
      if (nrow(rows) < 1) return(NULL)
      tags$div(
        class = "ynow-fs-section",
        tags$div(class = "ynow-fs-section-title", gname),
        tags$div(
          class = "ynow-fs-grid",
          lapply(seq_len(nrow(rows)), function(i) {
            mk_card(rows$Item[i], rows$Value[i])
          })
        )
      )
    })

    tags$div(class = "ynow-fs-wrap", sections)
  })

  # 保留表格輸出供下載／相容（不在 UI 顯示）
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

  # ==========================================
  # 📌 v11.0：側邊欄依 Model Selector 標記「推薦」
  # ==========================================
  model_sidebar_rec <- reactive({
    cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
    is <- tryCatch(d_income_statement(), error = function(e) NULL)
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    ind <- corp_industry_text()
    if (is.null(cf) || !is.data.frame(cf) || nrow(cf) == 0) {
      return(list(ddm = FALSE, dcf = FALSE, pb = FALSE, ri = FALSE, tags = character(0)))
    }
    recommend_valuation_models(cf, industry_text = ind, d_is = is, d_bs = bs)
  })

  output$sidebar_menu <- renderMenu({
    rec <- model_sidebar_rec()
    sel <- if (!is.null(input$sidebar_tabs) && nzchar(input$sidebar_tabs)) {
      input$sidebar_tabs
    } else {
      "get_started"
    }

    mk <- function(text, tab, ic, recommended = FALSE,
                   fallback_label = NULL, fallback_color = NULL) {
      b <- .sidebar_badge(recommended, fallback_label, fallback_color)
      args <- list(
        text = text,
        tabName = tab,
        icon = icon(ic),
        selected = identical(sel, tab)
      )
      if (!is.null(b$label) && nzchar(b$label)) {
        args$badgeLabel <- b$label
        args$badgeColor <- b$color
      }
      do.call(menuItem, args)
    }

    sidebarMenu(
      id = "sidebar_tabs",
      mk("Get Started", "get_started", "play-circle",
         fallback_label = "start", fallback_color = "purple"),
      mk("Dashboard", "dashboard", "chart-line"),
      # 順序對齊 Model Selector 左→右：DCF → DDM → P/B → RI
      mk("DCF-Model", "dcf_calculator", "calculator",
         recommended = isTRUE(rec$dcf)),
      mk("DDM", "ddm_calculator", "hand-holding-usd",
         recommended = isTRUE(rec$ddm),
         fallback_label = "new", fallback_color = "green"),
      mk("P/B-Asset", "pb_calculator", "landmark",
         recommended = isTRUE(rec$pb),
         fallback_label = "new", fallback_color = "aqua"),
      mk("RI-Model", "ri_calculator", "gem",
         recommended = isTRUE(rec$ri),
         fallback_label = "pro", fallback_color = "blue"),
      mk("Sensitivity", "sensitivity", "sliders-h",
         fallback_label = "new", fallback_color = "green"),
      mk("Backtest Zone", "backtest", "vial",
         fallback_label = "Alpha", fallback_color = "orange"),
      mk("Snapshot", "snapshot", "camera"),
      mk("About", "about", "info-circle")
    )
  })

  output$get_started_model_selector <- renderUI({
    rec <- model_sidebar_rec()
    make_card <- function(title, key, icon_name, color, formula, notes) {
      active <- isTRUE(rec[[key]])
      tags$div(
        class = paste("col-sm-3", if (active) "ynow-model-rec-active" else ""),
        tags$div(
          style = paste0(
            "border:1px solid ", if (active) color else "#ddd", ";",
            "border-radius:8px; padding:14px; min-height:170px; background:",
            if (active) "#fffaf2" else "#fff", "; box-shadow:0 2px 4px rgba(0,0,0,0.04);"
          ),
          tags$div(style = paste0("font-size:22px; color:", color, ";"), icon(icon_name)),
          tags$h4(style = "margin:8px 0 4px 0; font-weight:700;", title),
          tags$span(
            style = paste0(
              "display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px; color:#fff; background:",
              if (active) color else "#999", ";"
            ),
            if (active) "推薦" else "備選"
          ),
          tags$p(style = "margin:10px 0 4px 0; font-size:12px; color:#555;", formula),
          tags$p(style = "margin:0; font-size:12px; color:#777; line-height:1.4;", notes)
        )
      )
    }

    tagList(
      tags$style(HTML("
        .ynow-model-rec-active { transform: translateY(-2px); }
        .ynow-model-selector-summary {
          margin-bottom: 14px; padding: 10px 12px; border-left: 4px solid #3c8dbc;
          background: #f7fbff; color: #333; font-size: 13px; line-height: 1.5;
        }
      ")),
      tags$div(
        class = "ynow-model-selector-summary",
        tags$b("推薦首選："), rec$summary_method %||% "等待財報資料",
        tags$br(),
        tags$span(rec$reason %||% "搜尋股票並載入財報後產生推薦。")
      ),
      fluidRow(
        make_card("DCF", "dcf", "calculator", "#00a65a", "EV = Σ FCFF / (1+WACC)^t + TV / (1+WACC)^n", "適合 FCF 為正且相對穩定的企業。"),
        make_card("DDM", "ddm", "hand-holding-usd", "#f39c12", "P0 = D1 / (Ke - g)", "適合持續且穩定配息的企業。"),
        make_card("P/B", "pb", "landmark", "#3c8dbc", "P = BVPS × 合理 P/B", "適合金融、保險、資產驅動或 FCF 不穩的企業。"),
        make_card("RI", "ri", "gem", "#605ca8", "Value = Book Value + Σ Residual Income / (1+Ke)^t", "適合帳面價值與 ROE 具參考性的企業。")
      )
    )
  })

  .snapshot_value <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (length(x) > 1) x <- x[1]
    if (isTRUE(is.na(x))) return(NA_character_)
    as.character(x)
  }

  snapshot_rows <- reactive({
    ticker <- current_ticker() %||% APP_DEFAULTS$stock_code
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    wacc_pct <- if (!is.null(calculated_wacc())) round(calculated_wacc() * 100, 2) else NA_real_
    est_g <- tryCatch(central_perpetual_g(), error = function(e) NULL)

    rows <- list(
      c("Meta", "Downloaded At", ts, "Timestamp at download/render"),
      c("Meta", "Ticker", ticker, "Selected ticker"),
      c("Model Selector", "Recommended Method", .snapshot_value(rec$summary_method), "Rule-based model ranking"),
      c("DCF", "DCF Mode", .snapshot_value(input$dcf_mode), "Gordon or Two-Stage DCF"),
      c("DCF", "Forecast Years (n)", .snapshot_value(input$years), "n"),
      c("DCF", "Chart Mode", .snapshot_value(input$dcf_chart_mode), "simple = history + forecast FCFF; with_dcf = add discounted line"),
      c("Perpetual Growth", "Method", .snapshot_value(input$perpetual_g_method), "macro / fundamental / lifecycle"),
      c("Perpetual Growth", "Terminal g / SGR (%)", .snapshot_value(input$sgr), "DCF/RI terminal g; TV = FCF_n × (1+g) / (WACC-g)"),
      c("Perpetual Growth", "Estimated g (%)", if (!is.null(est_g)) .snapshot_value(est_g$g_pct) else NA_character_, "Selected perpetual-growth method output"),
      c("Perpetual Growth", "Lifecycle Stage", .snapshot_value(input$lifecycle_stage), "Lifecycle classification used when method = lifecycle"),
      c("DCF - Explicit+Gordon TV", "WACC (%)", .snapshot_value(input$wacc_gordon), "EV = Σ PV(FCFF) + PV(TV); not single-period Gordon"),
      c("DCF - Two Stage", "Stage 1 Years", .snapshot_value(input$yr_stage1), "Explicit high-growth period"),
      c("DCF - Two Stage", "g1 (%)", .snapshot_value(input$g_stage1), "FCFF_t = FCFF_(t-1) × (1+g1)"),
      c("DCF - Two Stage", "WACC1 (%)", .snapshot_value(input$wacc_stage1), "PV stage 1 = FCFF_t / (1+WACC1)^t"),
      c("DCF - Two Stage", "WACC2 (%)", .snapshot_value(input$wacc_stage2), "Terminal discount rate"),
      c("DCF - WACC Source", "Use Calculated WACC", .snapshot_value(input$use_calculated_wacc), "TRUE uses system WACC"),
      c("CAPM", "Rf (%)", .snapshot_value(input$capm_rf), "Ke = Rf + Beta × (Rm-Rf)"),
      c("CAPM", "Beta", .snapshot_value(input$capm_beta), "Systematic risk coefficient"),
      c("CAPM", "Use Industry Beta", .snapshot_value(input$use_industry_beta), "TRUE = industry avg; FALSE = Finance Summary β (manual sticky until ticker change)"),
      c("CAPM", "Rm (%)", .snapshot_value(input$capm_rm), "Expected market return"),
      c("WACC", "Calculated WACC (%)", .snapshot_value(wacc_pct), "WACC = E/(E+D)×Re + D/(E+D)×Rd×(1-T)"),
      c("WACC", "Re (%)", .snapshot_value(input$wacc_re), "Cost of equity"),
      c("WACC", "Use CAPM Re", .snapshot_value(input$use_estimated_re), "TRUE uses CAPM-estimated Re"),
      c("WACC", "Rd (%)", .snapshot_value(input$wacc_rd), "Cost of debt"),
      c("WACC", "Tax Rate T (%)", .snapshot_value(input$wacc_tax), "After-tax debt cost = Rd×(1-T)"),
      c("DDM", "D0", .snapshot_value(input[["mod_ddm-d0"]]), "P0 = D1 / (Ke-g); D1 = D0×(1+g)"),
      c("DDM", "g (%)", .snapshot_value(input[["mod_ddm-g"]]), "Dividend growth; optional sync with central SGR"),
      c("DDM", "Sync g with SGR", .snapshot_value(input[["mod_ddm-sync_g"]]), "If TRUE, DDM g follows Get Started SGR"),
      c("DDM", "Ke (%)", .snapshot_value(input[["mod_ddm-ke"]]), "Equity required return (CAPM)"),
      c("RI", "RI g (%)", .snapshot_value(input[["mod_ri-ri_g"]]), "RI terminal growth"),
      c("P/B", "P/B Low", .snapshot_value(input[["mod_pb-pb_low"]]), "Price = BVPS × P/B"),
      c("P/B", "P/B Mid", .snapshot_value(input[["mod_pb-pb_mid"]]), "Price = BVPS × P/B"),
      c("P/B", "P/B High", .snapshot_value(input[["mod_pb-pb_high"]]), "Price = BVPS × P/B"),
      c("P/B", "約當股數校正", .snapshot_value(input[["mod_pb-adjust_share_class"]]), "例外：市值÷股價／雙重股權"),
      c("Backtest", "Net Margin Threshold (%)", .snapshot_value(input$bt_net_margin), "Pass if Net Margin >= threshold"),
      c("Backtest", "Revenue Growth Threshold (%)", .snapshot_value(input$bt_rev_growth), "Pass if Revenue Growth >= threshold"),
      c("Backtest", "EPS / NI Growth Threshold (%)", .snapshot_value(input$bt_eps_growth), "Pass if EPS/NI Growth >= threshold"),
      c("Backtest", "FCF CV Ceiling (%)", .snapshot_value(input$bt_fcf_cv), "Pass if FCF CV <= ceiling")
    )
    df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(df) <- c("Section", "Parameter", "Current Value", "Formula")
    df
  })

  output$snapshot_timestamp <- renderUI({
    tags$span(style = "font-size:12px; color:#666;", "Snapshot time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  })

  output$snapshot_table <- renderDataTable({
    datatable(snapshot_rows(), rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_snapshot <- downloadHandler(
    filename = function() {
      ticker <- gsub("[^A-Za-z0-9._-]", "_", current_ticker() %||% APP_DEFAULTS$stock_code)
      paste0("YNow_snapshot_", ticker, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- snapshot_rows()
      df$Downloaded_At <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
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
  # 優先 yfinance（雲端穩定）；quantmod 作後備。快取避免搜尋後重複阻塞 UI。
  .hist_price_cache <- new.env(parent = emptyenv())
  hist_stock_data <- reactive({
    req(current_ticker())
    tk <- toupper(trimws(current_ticker()))
    if (exists(tk, envir = .hist_price_cache, inherits = FALSE)) {
      return(get(tk, envir = .hist_price_cache, inherits = FALSE))
    }
    df_final <- tryCatch({
      # 1y 足夠動能；與 backtest fetch 共用 yfinance-first 路徑
      hist <- fetch_price_history_df(tk, "1y")
      if (is.null(hist) || nrow(hist) < 30) stop("insufficient history")
      # 決策模組只需近約 180 日
      cutoff <- Sys.Date() - 180
      hist <- hist[hist$Date >= cutoff, , drop = FALSE]
      data.frame(
        Date = hist$Date,
        Open = NA_real_, High = NA_real_, Low = NA_real_,
        Close = hist$Close,
        Volume = if ("Volume" %in% names(hist)) hist$Volume else NA_real_,
        Adjusted = hist$Close,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning("無法取得歷史股價: ", e$message)
      NULL
    })
    if (!is.null(df_final)) assign(tk, df_final, envir = .hist_price_cache)
    df_final
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
    ddm_g = reactive({
      if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) as.numeric(input$sgr) else APP_DEFAULTS$ddm_g
    }), 
    ddm_ke = reactive({ central_ke() * 100 }),  # 🌟 連動！
    
    scraped_d0 = reactive({
      # 優先：財報推算每股股利；其次：Summary 股利欄
      cf <- d_cash_flow()
      bs <- d_balance_sheet()
      if (is.data.frame(cf) && nrow(cf) > 0 && is.data.frame(bs) && nrow(bs) > 0) {
        div_paid <- select_current_metric(cf, "Cash Dividends Paid", "flow")
        shares <- select_current_metric_any(bs, SHARE_PATTERNS, "stock")
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
    input_manual_fcf = reactive(input$manual_fcf),
    calc_trigger = run_calc_trigger,
    global_est_g = estimated_g,
    global_g_method = reactive(input$g_growth_method)
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

  # ==========================================
  # 🌱 中央永續成長率方法（同步 DCF sgr／RI ri_g；DDM g 可選同步）
  # ==========================================
  .current_wacc_pct <- function() {
    if (isTRUE(input$use_calculated_wacc) && !is.null(calculated_wacc()) && is.finite(calculated_wacc())) {
      return(as.numeric(calculated_wacc()) * 100)
    }
    if (isTRUE(input$dcf_mode == "two_stage") && !is.null(input$wacc_stage2) && is.finite(input$wacc_stage2)) {
      return(as.numeric(input$wacc_stage2))
    }
    if (!is.null(input$wacc_gordon) && is.finite(input$wacc_gordon)) {
      return(as.numeric(input$wacc_gordon))
    }
    APP_DEFAULTS$wacc_gordon
  }

  .current_rf_pct <- function() {
    if (!is.null(input$capm_rf) && is.finite(as.numeric(input$capm_rf))) {
      return(as.numeric(input$capm_rf))
    }
    tryCatch(as.numeric(cached_get_risk_free_rate()), error = function(e) APP_DEFAULTS$capm_rf)
  }

  central_perpetual_g <- reactive({
    d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
    d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    d_cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
    estimate_perpetual_g(
      method = input$perpetual_g_method %||% APP_DEFAULTS$perpetual_g_method,
      rf_pct = .current_rf_pct(),
      d_is = d_is,
      d_bs = d_bs,
      d_cf = d_cf,
      industry_text = corp_industry_text() %||% "",
      ticker = current_ticker() %||% APP_DEFAULTS$stock_code,
      lifecycle_stage = input$lifecycle_stage %||% "auto",
      wacc_pct = .current_wacc_pct()
    )
  })

  output$txt_perpetual_g_reason <- renderUI({
    est <- central_perpetual_g()
    tags$div(
      style = "background:#f8f9fa; border-left:4px solid #e67e22; padding:8px 12px; margin-bottom:12px; font-size:13px; color:#333;",
      tags$b("目前 g 估計："), est$reason %||% ""
    )
  })

  .push_perpetual_g <- function(est, notify_two_stage = TRUE) {
    if (is.null(est) || !is.finite(est$g_pct)) return(invisible(NULL))
    g_val <- round(as.numeric(est$g_pct), 2)
    if (is.null(input$sgr) || is.na(as.numeric(input$sgr)) || abs(as.numeric(input$sgr) - g_val) > 1e-4) {
      updateNumericInput(session, "sgr", value = g_val)
    }
    # DDM 股利 g：僅在勾選「與中央同步」時覆寫，允許與 FCFF 終值 SGR 分開
    if (isTRUE(input[["mod_ddm-sync_g"]] %||% TRUE)) {
      updateNumericInput(session, "mod_ddm-g", value = g_val)
    }
    updateNumericInput(session, "mod_ri-ri_g", value = g_val)

    if (isTRUE(est$suggest_two_stage)) {
      if (!identical(input$dcf_mode, "two_stage")) {
        updateRadioButtons(session, "dcf_mode", selected = "two_stage")
        if (isTRUE(notify_two_stage)) {
          showNotification(
            "Lifecycle：高速→成熟，已切換 DCF 為 Two-Stage，終值 g 收斂至 2–3% 區間。",
            type = "message", duration = 6
          )
        }
      }
      if (is.finite(est$g_stage1_pct)) {
        g1 <- as.numeric(est$g_stage1_pct)
        if (is.null(input$g_stage1) || is.na(as.numeric(input$g_stage1)) ||
            abs(as.numeric(input$g_stage1) - g1) > 1e-4) {
          updateNumericInput(session, "g_stage1", value = g1)
        }
      }
    }
    invisible(g_val)
  }

  observeEvent({
    list(
      input$perpetual_g_method,
      input$lifecycle_stage,
      input$capm_rf,
      scraped_financials(),
      corp_industry_text(),
      current_ticker(),
      calculated_wacc(),
      input$wacc_gordon,
      input$wacc_stage2,
      input$use_calculated_wacc
    )
  }, {
    est <- central_perpetual_g()
    .push_perpetual_g(est, notify_two_stage = TRUE)
  }, ignoreInit = FALSE)
  
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
    global_re = central_ke,
    global_g = reactive({
      if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) as.numeric(input$sgr) else APP_DEFAULTS$sgr
    })
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
    market_cap = reactive({
      df <- tryCatch(summary_data(), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NA_real_)
      row <- df[df$Item == "Market Cap (intraday)", , drop = FALSE]
      if (nrow(row) < 1) return(NA_real_)
      parse_financial_number(row$Value[1])[1]
    }),
    current_ticker = current_ticker,
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
  
  # ---------- CAPM Beta：Finance Summary 預設／產業平均可選／手動覆寫 ----------
  .summary_beta_value <- function() {
    df <- tryCatch(summary_data(), error = function(e) NULL)
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
    idx <- grep("^Beta", df$Item, ignore.case = TRUE)
    if (length(idx) == 0) return(NA_real_)
    parse_financial_number(df$Value[idx[1]])[1]
  }

  .industry_beta_value <- function() {
    ind <- input$industry_choice
    if (is.null(ind) || !nzchar(ind)) return(NA_real_)
    inds <- industry_standards[[ind]]
    if (is.null(inds) || is.null(inds$beta_avg)) return(1.0)
    suppressWarnings(as.numeric(inds$beta_avg))
  }

  .set_capm_beta <- function(val) {
    val <- suppressWarnings(as.numeric(val))
    if (!is.finite(val)) return(invisible(FALSE))
    val <- round(val, 2)
    cur <- suppressWarnings(as.numeric(input$capm_beta))
    if (is.finite(cur) && abs(cur - val) < 1e-4) return(invisible(FALSE))
    capm_beta_updating(TRUE)
    updateNumericInput(session, "capm_beta", value = val)
    invisible(TRUE)
  }

  .sync_capm_beta <- function() {
    if (isTRUE(input$use_industry_beta)) {
      b <- .industry_beta_value()
      if (is.finite(b)) {
        capm_beta_dirty(FALSE)
        .set_capm_beta(b)
      }
      return(invisible(NULL))
    }
    # 未勾產業平均：跟 Finance Summary（手動覆寫期間不打擾）
    if (isTRUE(capm_beta_dirty())) return(invisible(NULL))
    b <- .summary_beta_value()
    if (is.finite(b)) .set_capm_beta(b)
    invisible(NULL)
  }

  # 手動輸入 → dirty（產業平均模式不記 dirty，下次勾選／換產業仍可覆寫）
  observeEvent(input$capm_beta, {
    if (isTRUE(capm_beta_updating())) {
      capm_beta_updating(FALSE)
      return()
    }
    if (!isTRUE(input$use_industry_beta)) {
      capm_beta_dirty(TRUE)
    }
  }, ignoreInit = TRUE)

  # 勾選產業平均／換產業：套用產業 β；取消勾選且非 dirty：改跟 Summary
  observeEvent(list(input$use_industry_beta, input$industry_choice), {
    .sync_capm_beta()
  }, ignoreInit = TRUE)

  # Finance Summary 更新（新股票／重整）→ 非產業模式且非 dirty 時同步 β
  observeEvent(summary_data(), {
    if (!isTRUE(input$use_industry_beta) && !isTRUE(capm_beta_dirty())) {
      .sync_capm_beta()
    }
  }, ignoreInit = TRUE)

  # 智慧標籤：產業平均 / Finance Summary / 自訂
  observeEvent(list(input$capm_beta, input$industry_choice, input$use_industry_beta, summary_data()), {
    beta <- suppressWarnings(as.numeric(input$capm_beta))
    if (!is.finite(beta)) return()
    ind_b <- .industry_beta_value()
    fs_b <- .summary_beta_value()

    if (isTRUE(input$use_industry_beta) && is.finite(ind_b) && abs(beta - ind_b) < 1e-4) {
      updateNumericInput(session, "capm_beta",
                         label = HTML("Beta (β) <span style='color: #2980b9; font-size: 12px;'>[套用產業平均值]</span>"))
    } else if (!isTRUE(input$use_industry_beta) && is.finite(fs_b) && abs(beta - fs_b) < 1e-4) {
      updateNumericInput(session, "capm_beta",
                         label = HTML("Beta (β) <span style='color: #27ae60; font-size: 12px;'>[Finance Summary]</span>"))
    } else {
      updateNumericInput(session, "capm_beta",
                         label = HTML("Beta (β) <span style='color: #e67e22; font-size: 12px;'>[自訂數值]</span>"))
    }
  }, ignoreInit = FALSE)
  
  # 保留：切換產業時刷新 Rm／成長／P/B；Beta 僅在勾選產業平均時由上方 .sync_capm_beta 處理
  observeEvent(input$industry_choice, {
    req(input$industry_choice)
    inds <- industry_standards[[input$industry_choice]]
    if (!is.null(inds)) {
      updateNumericInput(session, "capm_rm", value = inds$rm_avg)
      
      # 同步短期成長／P/B 區間（有設定才更新）
      if (!is.null(inds$rev_growth)) {
        g_mid <- round(max(2, min(mean(inds$rev_growth), 12)), 2)
        updateNumericInput(session, "custom_g", value = g_mid)
        updateNumericInput(session, "g_stage1", value = g_mid)
      }
      # 僅在勾選「套用產業預設本淨比」時覆寫 P/B 區間
      if (isTRUE(input[["mod_pb-use_industry_pb"]]) &&
          !is.null(inds$pb_band) && length(inds$pb_band) >= 2) {
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
      prev_g_na <- isolate(estimated_g())
      estimated_g(NULL)
      if (!is.null(prev_g_na)) {
        updateSelectInput(session, "g_growth_method", label = "預估 FCFF 成長率 (缺乏數據)")
      }
      return()
    }
    
    prev_g <- isolate(estimated_g())
    prev_method <- isolate(estimated_g_meta$method)
    estimated_g(val)
    estimated_g_meta$method <- method
    estimated_g_meta$fund_res <- fund_res
    changed <- !identical(prev_g, val) || !identical(prev_method, method)
    if (isTRUE(changed)) {
      updateSelectInput(session, "g_growth_method", label = paste0("預估 FCFF 成長率 ➔ ", val, " %"))
    }
    
    if (method != "custom" && !is.na(val) && !identical(input$dcf_mode, "two_stage")) {
      if (is.null(input$g_stage1) || is.na(as.numeric(input$g_stage1)) ||
          abs(as.numeric(input$g_stage1) - as.numeric(val)) > 1e-4) {
        updateNumericInput(session, "g_stage1", value = val)
      }
    }
    # 觸發 FCFF 投影表依新成長率重算（必須 isolate，否則 observe 自讀自寫會無限迴圈）
    if (isTRUE(changed)) {
      run_calc_trigger(isolate(run_calc_trigger()) + 1)
    }
  })
  output$g_result <- renderUI({
    method <- estimated_g_meta$method
    fund_res <- estimated_g_meta$fund_res
    if (is.null(method)) return(NULL)
    
    if (method == "fundamental" && !is.null(fund_res)) {
      hit_ceiling_raw <- fund_res$raw_g > 25
      
      ceiling_status_msg <- if (hit_ceiling_raw && fund_res$ceiling_applied) {
        glue::glue("<div style='color: #d9534f; margin-top: 5px; font-weight: bold;'>原始成長率過高，已啟動防呆強制封頂。(實際輸出至模型: 25.00 %)</div>")
      } else if (hit_ceiling_raw && !fund_res$ceiling_applied) {
        glue::glue("<div style='color: #8e44ad; margin-top: 5px; font-weight: bold; padding: 5px; border: 1px solid #8e44ad; background: #f4ecf7;'>警告：已解除天花板！將使用極端成長率進行估值 (實際輸出至模型: {fund_res$g} %)</div>")
      } else {
        glue::glue("<div style='color: #00a65a; margin-top: 5px; font-weight: bold;'>成長率處於合理範圍內 (實際輸出至模型: {fund_res$g} %)</div>")
      }
      
      HTML(glue::glue(
        "<div style='padding: 12px; background-color: #fdfaf6; border-left: 4px solid #d35400; font-size: 13px;'>
           <b>學理推估 (Fundamental) 拆解：</b><br/>
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
    method <- input$g_growth_method %||% "fundamental"
    method_lab <- switch(
      as.character(method),
      "fundamental" = "基本面",
      "cagr" = "CAGR",
      "mean" = "平均",
      "median" = "中位數",
      "last_year" = "最近一年",
      "custom" = "自訂",
      method
    )
    infoBox(
      paste0("預估 FCFF 成長率 (", method_lab, ")"),
      paste0(val_g, " %"),
      icon = icon("chart-line"),
      color = "purple",
      fill = TRUE
    )
  })
  
  output$ibx_sgr <- renderInfoBox({ 
    val_sgr <- if (!is.null(input$sgr)) input$sgr else "N/A"
    infoBox("DCF／RI 終值永續成長率 (SGR)", paste0(val_sgr, " %"), icon = icon("infinity"), color = "maroon", fill = TRUE) 
  })
  
  output$ibx_wacc <- renderInfoBox({ 
    val_wacc <- if (!is.null(calculated_wacc())) round(calculated_wacc() * 100, 2) else APP_DEFAULTS$wacc_gordon
    infoBox("WACC", h3(paste0(val_wacc, " %")), icon = icon("percent"), color = "aqua", fill = TRUE) 
  })
  
  output$plt_fcf_trend <- renderPlot({
    req(fcf_results$df_fcf()) 
    df <- fcf_results$df_fcf() 
    
    ggplot(df, aes(x = Year)) +
      geom_col(aes(y = NOPAT, fill = "預估稅後營業利潤 (NOPAT)"), width = 0.6, alpha = 0.8) +
      scale_fill_manual(name = "", values = c("預估稅後營業利潤 (NOPAT)" = "#00a65a")) +
      geom_line(aes(y = FCFF, group = 1, color = "企業自由現金流 (FCFF)"), size = 1.5) +
      geom_point(aes(y = FCFF, color = "企業自由現金流 (FCFF)"), size = 3) +
      scale_color_manual(name = "", values = c("企業自由現金流 (FCFF)" = "#3c8dbc")) +
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
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = FALSE)
  })
  
  .auto_recalc_capm_wacc <- function(notify = FALSE, wacc_too = TRUE, rf_override = NULL) {
    # CAPM → Re
    rf <- if (!is.null(rf_override) && is.finite(as.numeric(rf_override))) {
      as.numeric(rf_override)
    } else {
      suppressWarnings(as.numeric(input$capm_rf))
    }
    beta <- suppressWarnings(as.numeric(input$capm_beta))
    rm <- suppressWarnings(as.numeric(input$capm_rm))
    if (is.finite(rf) && is.finite(beta) && is.finite(rm)) {
      r_e_est <- (rf / 100) + beta * ((rm / 100) - (rf / 100))
      estimated_re(r_e_est)
      updateNumericInput(session, "wacc_re", value = round(r_e_est * 100, 2))
    }

    if (!isTRUE(wacc_too)) {
      if (isTRUE(notify) && !is.null(estimated_re())) {
        showNotification(
          glue::glue("📌 已估算 rₑ = {round(estimated_re() * 100, 2)}%"),
          type = "message"
        )
      }
      return(invisible(NULL))
    }

    # WACC（需財報／股價）
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    sum_df <- tryCatch(summary_data(), error = function(e) NULL)
    if (is.null(bs) || !is.data.frame(bs) || nrow(bs) == 0) return(invisible(NULL))

    shares <- select_current_metric(bs, "Share Issued|Ordinary Shares Number", "stock")
    if (is.na(shares) || shares == 0) {
      return(invisible(NULL))
    }

    price_val <- NA_real_
    if (!is.null(sum_df) && is.data.frame(sum_df) && "Previous Close" %in% sum_df$Item) {
      price_val <- parse_financial_number(sum_df$Value[sum_df$Item == "Previous Close"][1])
    }
    equity_mv <- if (!is.na(price_val) && shares > 0) {
      shares * price_val
    } else {
      select_current_metric(bs, "Common Stock Equity", "stock")
    }
    debt <- select_current_metric(bs, "Total Debt", "stock")
    debt <- if (is.na(debt)) 0 else debt
    if (is.na(equity_mv) || equity_mv <= 0) return(invisible(NULL))

    total_capital <- equity_mv + debt
    if (!is.finite(total_capital) || total_capital <= 0) return(invisible(NULL))

    r_e <- if (isTRUE(input$use_estimated_re) && !is.null(estimated_re())) {
      estimated_re()
    } else if (!is.null(input$wacc_re) && is.finite(input$wacc_re)) {
      input$wacc_re / 100
    } else {
      APP_DEFAULTS$wacc_re / 100
    }
    r_d <- if (!is.null(input$wacc_rd) && is.finite(input$wacc_rd)) input$wacc_rd / 100 else APP_DEFAULTS$wacc_rd / 100
    tax <- if (!is.null(input$wacc_tax) && is.finite(input$wacc_tax)) input$wacc_tax / 100 else APP_DEFAULTS$wacc_tax / 100

    wacc <- (equity_mv / total_capital) * r_e + (debt / total_capital) * r_d * (1 - tax)
    if (!is.finite(wacc) || wacc <= 0) return(invisible(NULL))

    calculated_wacc(wacc)
    wacc_percent <- round(wacc * 100, 2)

    if (identical(input$dcf_mode, "gordon") || is.null(input$dcf_mode)) {
      updateNumericInput(session, "wacc_gordon", value = wacc_percent)
    } else {
      updateNumericInput(session, "wacc_stage1", value = wacc_percent)
      updateNumericInput(session, "wacc_stage2", value = wacc_percent)
    }

    if (isTRUE(notify)) {
      showNotification(
        glue::glue("📌 已自動估算並套用 WACC {wacc_percent}%（含 CAPM rₑ）"),
        type = "message",
        duration = 5
      )
    }
    invisible(wacc_percent)
  }

  observeEvent(input$calc_wacc, {
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = TRUE)
  })

  # 查詢新股票／財報更新後：自動帶入相關數值並重估 WACC
  observeEvent(list(scraped_financials(), summary_data()), {
    req(scraped_financials(), summary_data())
    rf_now <- tryCatch(as.numeric(cached_get_risk_free_rate()), error = function(e) NA_real_)
    if (is.finite(rf_now) && rf_now > 0) {
      updateNumericInput(session, "capm_rf", value = round(rf_now, 2))
    }
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = TRUE, rf_override = rf_now)
  }, ignoreInit = TRUE)

  # 產業／Beta／Rm 變更時靜默重估（避免重複通知）
  observeEvent(list(input$capm_beta, input$capm_rm, input$industry_choice), {
    req(scraped_financials(), summary_data())
    .auto_recalc_capm_wacc(notify = FALSE, wacc_too = TRUE)
  }, ignoreInit = TRUE)

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
  # 📉 DCF Overview 圖：歷史 FCFF + 預測；可選單純／含折現
  # ==========================================
  output$plt_dcf_trajectory <- renderPlot({
    req(fcf_results$df_fcf(), current_ticker())
    proj_df <- fcf_results$df_fcf()
    if (is.null(proj_df) || nrow(proj_df) < 1) {
      plot.new()
      text(0.5, 0.5, "⚠️ 財報數據不足，無法繪圖", cex = 1.4)
      return()
    }

    chart_mode <- input$dcf_chart_mode %||% "with_dcf"
    n_years <- nrow(proj_df)
    fcff_vals <- extract_fcff_series(proj_df)

    # --- 歷史 FCFF（財報期，舊→新）---
    hist_df <- tryCatch({
      cf <- d_cash_flow()
      row_idx <- grep("^Free Cash Flow$|Free Cash Flow", cf[[1]], ignore.case = TRUE)
      if (length(row_idx) == 0) return(NULL)
      period_cols <- colnames(cf)[-1]
      period_cols <- period_cols[!grepl("^ttm$", period_cols, ignore.case = TRUE)]
      if (length(period_cols) == 0) return(NULL)
      vals <- parse_financial_number(as.character(cf[row_idx[1], period_cols, drop = FALSE]))
      # 欄位通常為最新→最舊；繪圖改為舊→新
      ord <- rev(seq_along(period_cols))
      data.frame(
        Period = as.character(period_cols[ord]),
        Value = as.numeric(vals[ord]),
        Metric = "歷史現金流 (FCFF)",
        Segment = "History",
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)

    if (!is.null(hist_df)) {
      hist_df <- hist_df[is.finite(hist_df$Value), , drop = FALSE]
    }

    # --- 預測期標籤 ---
    forecast_periods <- as.character(proj_df$Year)
    if (length(forecast_periods) == 0) forecast_periods <- paste0("Y", seq_len(n_years))

    wacc_val <- tryCatch({
      if (isTRUE(input$use_calculated_wacc) && !is.null(calculated_wacc())) {
        rep(as.numeric(calculated_wacc()), n_years)
      } else if (identical(input$dcf_mode, "gordon")) {
        rep(as.numeric(input$wacc_gordon) / 100, n_years)
      } else {
        s1_yrs <- as.numeric(input$yr_stage1)
        if (!is.finite(s1_yrs)) s1_yrs <- 1
        c(
          rep(as.numeric(input$wacc_stage1) / 100, min(s1_yrs, n_years)),
          rep(as.numeric(input$wacc_stage2) / 100, max(n_years - s1_yrs, 0))
        )
      }
    }, error = function(e) rep(0.1, n_years))

    discount_factors <- cumprod(1 + wacc_val)
    dcf_vals <- round(fcff_vals / discount_factors, 2)

    g_terminal <- if (is.numeric(input$sgr)) input$sgr / 100 else 0.03
    terminal_wacc <- tail(wacc_val, 1)
    tv_annotation <- ""

    if (identical(chart_mode, "with_dcf") &&
        is.finite(terminal_wacc) && is.finite(g_terminal) && terminal_wacc > g_terminal) {
      last_fcf <- tail(fcff_vals, 1)
      tv <- (last_fcf * (1 + g_terminal)) / (terminal_wacc - g_terminal)
      pv_tv <- tv / discount_factors[n_years]
      dcf_vals[n_years] <- round(dcf_vals[n_years] + pv_tv, 2)
      tv_annotation <- paste0(
        "\n( 第 ", n_years, " 年 DCF 已含永續終值 PV of TV: $",
        scales::comma(round(pv_tv, 2)), " )"
      )
    }

    forecast_fcff <- data.frame(
      Period = forecast_periods,
      Value = as.numeric(fcff_vals),
      Metric = "預測現金流 (FCFF)",
      Segment = "Forecast",
      stringsAsFactors = FALSE
    )

    plot_parts <- list()
    if (!is.null(hist_df) && nrow(hist_df) > 0) plot_parts <- c(plot_parts, list(hist_df))
    plot_parts <- c(plot_parts, list(forecast_fcff))

    if (identical(chart_mode, "with_dcf")) {
      plot_parts <- c(plot_parts, list(data.frame(
        Period = forecast_periods,
        Value = as.numeric(dcf_vals),
        Metric = "折現後價值 (DCF)",
        Segment = "Forecast",
        stringsAsFactors = FALSE
      )))
    }

    plot_df <- do.call(rbind, plot_parts)
    plot_df <- plot_df[is.finite(plot_df$Value), , drop = FALSE]
    if (nrow(plot_df) == 0) {
      plot.new()
      text(0.5, 0.5, "⚠️ 無可繪製數值", cex = 1.4)
      return()
    }

    # 固定橫軸順序：歷史期 → 預測期
    x_levels <- unique(c(
      if (!is.null(hist_df) && nrow(hist_df) > 0) hist_df$Period else character(0),
      forecast_periods
    ))
    plot_df$Period <- factor(plot_df$Period, levels = x_levels)
    plot_df$Metric <- factor(
      plot_df$Metric,
      levels = c("歷史現金流 (FCFF)", "預測現金流 (FCFF)", "折現後價值 (DCF)")
    )

    title_txt <- if (identical(chart_mode, "simple")) {
      paste0(current_ticker(), " - 歷史與預測 FCFF（單純模式）")
    } else {
      paste0(current_ticker(), " - 歷史／預測 FCFF vs 折現後 DCF")
    }

    color_map <- c(
      "歷史現金流 (FCFF)" = "#3498db",
      "預測現金流 (FCFF)" = "#95a5a6",
      "折現後價值 (DCF)" = "#e74c3c"
    )
    lty_map <- c(
      "歷史現金流 (FCFF)" = "solid",
      "預測現金流 (FCFF)" = "solid",
      "折現後價值 (DCF)" = "dashed"
    )

    ggplot(plot_df, aes(x = Period, y = Value, color = Metric, linetype = Metric, group = Metric)) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.8) +
      geom_text(
        aes(label = scales::comma(round(Value, 1))),
        vjust = -1.2, size = 3.2, show.legend = FALSE, check_overlap = TRUE
      ) +
      scale_color_manual(values = color_map, drop = TRUE) +
      scale_linetype_manual(values = lty_map, drop = TRUE) +
      theme_minimal(base_size = 14) +
      labs(
        title = title_txt,
        subtitle = if (identical(chart_mode, "with_dcf")) tv_annotation else "不含折現線；僅歷史與預測 FCFF",
        x = "期間", y = "USD (Millions)"
      ) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "#8e44ad", face = "bold", hjust = 0.5)
      )
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
      labs(title = "FCFF 預測即時預覽", x = "預測期", y = "FCFF (USD)") + theme(legend.position = "top")
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
      return("⚠️ 尚未計算 DCF，請確認參數後按下「試算 DCF」")
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
  # 📊 9. 敏感度分析矩陣（即時 SGR／WACC；自動 DCF 或 DDM）
  # ==========================================
  .sensitivity_matrix_model <- reactive({
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    if (is.null(rec)) return("DCF")
    # 僅 DDM 明確推薦、且非 DCF → 用 DDM 矩陣；其餘絕對估值路徑以 DCF 為主
    if (isTRUE(rec$ddm) && !isTRUE(rec$dcf)) return("DDM")
    "DCF"
  })

  output$sensitivity_model_rec <- renderUI({
    matrix_model <- .sensitivity_matrix_model()
    accent <- if (identical(matrix_model, "DDM")) "#f39c12" else "#00a65a"

    tags$div(
      style = paste0(
        "background-color:#f8f9fa; border-left:5px solid ", accent,
        "; padding:12px 14px; border-radius:5px; margin-bottom:12px;"
      ),
      tags$p(
        style = "font-size:15px; margin:0 0 6px 0;",
        tags$strong("推薦主體："),
        tags$span(style = paste0("color:", accent, "; font-weight:700;"), "DCF 或 DDM"),
        tags$span(
          style = "margin-left:8px; font-size:12px; color:#666;",
          paste0("（矩陣自動採用 ", matrix_model, "）")
        )
      ),
      tags$p(
        style = "font-size:13px; color:#555; margin:0;",
        tags$strong("背後邏輯："),
        "獲利穩定、成長放緩 (<15%)，屬於成熟型企業，適合絕對估值模型。"
      )
    )
  })

  .build_dcf_sensitivity_matrix <- function(base_wacc, base_g) {
    df_fcf <- fcf_results$df_fcf()
    n_years <- as.numeric(input$years)
    if (is.null(df_fcf) || !is.data.frame(df_fcf) || nrow(df_fcf) != n_years) {
      return(NULL)
    }
    future_fcfs <- extract_fcff_series(df_fcf)
    fcf_n <- tail(future_fcfs, 1)

    latest_cash <- get_latest_cash_position(d_cash_flow())
    temp_debt <- select_current_metric(d_balance_sheet(), "Total Debt", "stock")
    total_debt <- if (!is.null(input$manual_debt) && !is.na(input$manual_debt)) {
      input$manual_debt
    } else {
      ifelse(is.na(temp_debt), 0, temp_debt)
    }

    shares <- select_current_metric(
      d_balance_sheet(),
      "Ordinary Shares Number|Share Issued|Total Shares Outstanding",
      "stock"
    )
    if (is.na(shares) || shares <= 0) shares <- 1

    wacc_range <- seq(base_wacc + 2, base_wacc - 2, length.out = 5)
    g_range <- seq(base_g - 1, base_g + 1, length.out = 5)

    sens_matrix <- matrix(
      NA, nrow = 5, ncol = 5,
      dimnames = list(
        paste0("WACC ", round(wacc_range, 1), "%"),
        paste0("g ", round(g_range, 1), "%")
      )
    )

    use_calc <- isTRUE(input$use_calculated_wacc) &&
      !is.null(calculated_wacc()) && is.finite(calculated_wacc())
    base_wacc_seq <- if (identical(input$dcf_mode, "gordon")) {
      rep(base_wacc / 100, n_years)
    } else {
      s1 <- as.numeric(input$yr_stage1)
      r2_base <- if (use_calc) {
        base_wacc / 100
      } else if (!is.null(input$wacc_stage2) && is.finite(input$wacc_stage2)) {
        input$wacc_stage2 / 100
      } else {
        base_wacc / 100
      }
      # 敏感度以「目前 WACC」為軸心：Stage1 也相對目前 WACC 平移
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
    list(matrix = sens_matrix, center = sens_matrix[3, 3], axes = list(wacc = base_wacc, g = base_g))
  }

  .build_ddm_sensitivity_matrix <- function(base_ke, base_g) {
    d0 <- tryCatch({
      if (!is.null(input[["mod_ddm-d0"]]) && is.finite(as.numeric(input[["mod_ddm-d0"]]))) {
        as.numeric(input[["mod_ddm-d0"]])
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
    if (is.na(d0) || d0 <= 0) return(NULL)

    ke_range <- seq(base_ke + 2, base_ke - 2, length.out = 5)
    g_range <- seq(base_g - 1, base_g + 1, length.out = 5)
    sens_matrix <- matrix(
      NA, nrow = 5, ncol = 5,
      dimnames = list(
        paste0("Ke ", round(ke_range, 1), "%"),
        paste0("g ", round(g_range, 1), "%")
      )
    )
    for (i in 1:5) {
      for (j in 1:5) {
        ke_val <- ke_range[i] / 100
        g_val <- g_range[j] / 100
        if (!is.na(ke_val) && !is.na(g_val) && ke_val > g_val) {
          d1 <- d0 * (1 + g_val)
          sens_matrix[i, j] <- d1 / (ke_val - g_val)
        }
      }
    }
    list(matrix = sens_matrix, center = sens_matrix[3, 3], axes = list(ke = base_ke, g = base_g))
  }

  sensitivity_state <- reactive({
    req(input$calc)
    matrix_model <- .sensitivity_matrix_model()

    base_g <- if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) {
      as.numeric(input$sgr)
    } else {
      APP_DEFAULTS$sgr
    }

    if (identical(matrix_model, "DDM")) {
      base_ke <- tryCatch({
        ke_ui <- input[["mod_ddm-ke"]]
        if (!is.null(ke_ui) && is.finite(as.numeric(ke_ui))) {
          as.numeric(ke_ui)
        } else {
          central_ke() * 100
        }
      }, error = function(e) central_ke() * 100)
      if (is.null(base_ke) || !is.finite(base_ke)) base_ke <- 10
      built <- .build_ddm_sensitivity_matrix(base_ke, base_g)
      return(list(
        model = "DDM",
        base_g = base_g,
        base_disc = base_ke,
        disc_label = "Ke",
        built = built
      ))
    }

    # DCF：與 Dashboard／Get Started 同一套「目前 WACC」
    base_wacc <- tryCatch(.current_wacc_pct(), error = function(e) NA_real_)
    if (is.null(base_wacc) || !is.finite(base_wacc)) base_wacc <- APP_DEFAULTS$wacc_gordon
    req(fcf_results$df_fcf())
    built <- .build_dcf_sensitivity_matrix(base_wacc, base_g)
    list(
      model = "DCF",
      base_g = base_g,
      base_disc = base_wacc,
      disc_label = "WACC",
      built = built
    )
  })

  output$dcf_sensitivity_table <- renderTable({
    st <- sensitivity_state()
    req(!is.null(st$built), !is.null(st$built$matrix))
    sens_matrix <- st$built$matrix
    out_df <- cbind(Rate = rownames(sens_matrix), as.data.frame(sens_matrix, check.names = FALSE))
    names(out_df)[1] <- if (identical(st$model, "DDM")) "Ke_Rate" else "WACC_Rate"
    out_df
  }, digits = 2, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c",
     width = "100%", na = "無效 (折現率≤g)")

  output$sensitivity_analysis_panel <- renderUI({
    st <- tryCatch(sensitivity_state(), error = function(e) NULL)
    if (is.null(st) || is.null(st$built)) {
      return(tags$div(
        style = "background:#fff8f0; border:1px solid #f0ad4e; border-radius:6px; padding:12px; font-size:13px; color:#666;",
        "請先完成 Get Started 參數並執行估值計算後，即可顯示敏感度解讀。"
      ))
    }

    center_val <- st$built$center
    curr_price <- tryCatch({
      p <- scraped_market_cap()$price
      if (!is.null(p) && is.finite(as.numeric(p))) as.numeric(p) else NA_real_
    }, error = function(e) NA_real_)
    fair_val <- tryCatch({
      if (identical(st$model, "DDM")) {
        if (!is.null(ddm_results$ddm_price)) ddm_results$ddm_price() else NA_real_
      } else {
        stock_price_estimate_val()
      }
    }, error = function(e) NA_real_)

    fmt <- function(x) {
      if (is.null(x) || length(x) < 1 || !is.finite(as.numeric(x)[1])) return("N/A")
      sprintf("%.2f", as.numeric(x)[1])
    }

    vs_price <- if (is.finite(center_val) && is.finite(curr_price) && curr_price > 0) {
      pct <- (center_val - curr_price) / curr_price * 100
      sprintf("中心格內在價值 %s，相對現價 %s 約 %+.1f%%。", fmt(center_val), fmt(curr_price), pct)
    } else if (is.finite(center_val)) {
      sprintf("中心格內在價值約 %s；現價資料不足，暫無法比較。", fmt(center_val))
    } else {
      "中心格組合無效（折現率需大於 g），請調降 SGR 或提高折現率後重算。"
    }

    vs_fair <- if (is.finite(center_val) && is.finite(as.numeric(fair_val)[1])) {
      sprintf("與目前 %s 公允價 %s 對照：差異約 %s。",
              st$model, fmt(fair_val),
              sprintf("%+.2f", center_val - as.numeric(fair_val)[1]))
    } else {
      paste0("公允價尚未就緒；矩陣以目前 ", st$disc_label, "／SGR 為軸心展開。")
    }

    tags$div(
      style = "background:#f7fbff; border-left:4px solid #3c8dbc; border-radius:6px; padding:14px; font-size:13px; line-height:1.55; color:#333; margin-top:12px;",
      tags$h5(style = "margin-top:0; color:#3c8dbc; font-weight:700;", icon("lightbulb"), " 簡要分析"),
      tags$p(
        tags$b("目前軸心："),
        sprintf("%s = %s%%，SGR (g) = %s%%（與 Get Started／Dashboard 同步）",
                st$disc_label, fmt(st$base_disc), fmt(st$base_g))
      ),
      tags$p(tags$b("矩陣解讀："), vs_price),
      tags$p(vs_fair),
      tags$p(
        style = "margin-bottom:0; color:#555;",
        tags$b("適用提醒："),
        "本矩陣適用絕對估值情境（DCF／DDM）；觀察 WACC（或 Ke）與 g 鄰近組合對每股內在價值的敏感度。"
      )
    )
  })

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
      box(title = "核心評價數據缺失提醒", status = "danger", width = 12, solidHeader = TRUE,
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
  
  # ==========================================
  # 🧪 Backtest Zone：公司專屬參數 + 真實回測
  # ==========================================
  bt_param_notes_txt <- reactiveVal("請先搜尋股票並載入財報，系統會依公司自動推導參數。")
  bt_result <- reactiveVal(NULL)
  bt_run_msg <- reactiveVal("")

  bt_current_mos <- reactive({
    cur <- tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    tgt <- tryCatch(stock_price_estimate_val(), error = function(e) NA_real_)
    if (is.null(tgt) || length(tgt) < 1) tgt <- NA_real_
    cur <- suppressWarnings(as.numeric(cur)[1])
    tgt <- suppressWarnings(as.numeric(tgt)[1])
    if (is.na(cur) || is.na(tgt) || !is.finite(cur) || !is.finite(tgt) || tgt == 0) return(NA_real_)
    (tgt - cur) / tgt
  })

  apply_bt_params_to_ui <- function(p) {
    updateNumericInput(session, "bt_net_margin", value = p$bt_net_margin)
    updateNumericInput(session, "bt_rev_growth", value = p$bt_rev_growth)
    updateNumericInput(session, "bt_eps_growth", value = p$bt_eps_growth)
    updateNumericInput(session, "bt_fcf_cv", value = p$bt_fcf_cv)
    updateSliderInput(session, "bt_w_mom", value = p$bt_w_mom)
    updateSliderInput(session, "bt_w_rsi", value = p$bt_w_rsi)
    updateSliderInput(session, "bt_w_vg", value = p$bt_w_vg)
    bt_param_notes_txt(p$notes)
  }

  refresh_bt_params <- function(fetch_hist = TRUE) {
    req(current_ticker(), d_income_statement(), d_cash_flow())
    hist_long <- NULL
    if (isTRUE(fetch_hist)) {
      # 優先用搜尋後已快取的股價，避免再打一次網路
      cached <- tryCatch(hist_stock_data(), error = function(e) NULL)
      if (!is.null(cached) && nrow(cached) >= 30) {
        hist_long <- cached[, c("Date", "Close", "Volume"), drop = FALSE]
      } else {
        hist_long <- tryCatch(fetch_price_history_df(current_ticker(), "1y"), error = function(e) NULL)
      }
    }
    p <- derive_bt_params(
      d_is = d_income_statement(),
      d_bs = d_balance_sheet(),
      d_cf = d_cash_flow(),
      hist_df = hist_long,
      mos = bt_current_mos(),
      industry_choice = input$industry_choice
    )
    apply_bt_params_to_ui(p)
    invisible(p)
  }

  # 搜尋後先用財報推導參數（不另抓股價），避免與 Overview 繪圖搶同一條 session
  observeEvent(list(current_ticker(), scraped_financials()), {
    req(current_ticker(), scraped_financials())
    if (!identical(input$bt_param_mode, "auto")) return()
    tryCatch(refresh_bt_params(fetch_hist = FALSE), error = function(e) {
      bt_param_notes_txt(paste("自動推導失敗：", e$message))
    })
  }, ignoreInit = FALSE)

  observeEvent(input$bt_refresh_params, {
    tryCatch({
      refresh_bt_params(fetch_hist = TRUE)
      showNotification("✅ 已依目前公司重算 Backtest 參數", type = "message")
    }, error = function(e) {
      showNotification(paste("參數重算失敗：", e$message), type = "error")
    })
  })

  observeEvent(input$bt_param_mode, {
    if (identical(input$bt_param_mode, "auto")) {
      tryCatch(refresh_bt_params(fetch_hist = FALSE), error = function(e) NULL)
    } else {
      bt_param_notes_txt("手動覆寫模式：調整下方門檻／權重後再執行回測。")
    }
  })

  # 自動模式：使用者改動參數後不強制鎖死（允許微調）；切回 auto 才重算

  output$bt_param_notes <- renderUI({
    msg <- bt_param_notes_txt()
    tags$div(
      style = "margin: 8px 0 12px 0; padding: 8px 10px; background: #f9f9f9; border-left: 3px solid #00a65a; border-radius: 3px; font-size: 12px; color: #444; line-height: 1.5;",
      icon("info-circle"), " ", msg
    )
  })

  output$bt_run_status <- renderUI({
    msg <- bt_run_msg()
    if (!nzchar(msg)) return(NULL)
    tags$p(style = "margin: 10px 0 0 0; color: #666; font-size: 12px; line-height: 1.45;", icon("clock"), " ", msg)
  })

  observeEvent(input$run_bt, {
    req(current_ticker())
    bt_result(NULL)
    bt_run_msg("回測計算中…")
    tryCatch({
      if (is.null(d_income_statement()) || is.null(d_cash_flow())) {
        stop("請先在 Dashboard 搜尋並載入該公司財報")
      }
      # 若在自動模式，先重算一次再跑
      if (identical(input$bt_param_mode, "auto")) {
        tryCatch(refresh_bt_params(), error = function(e) NULL)
      }
      params <- list(
        bt_net_margin = input$bt_net_margin,
        bt_rev_growth = input$bt_rev_growth,
        bt_eps_growth = input$bt_eps_growth,
        bt_fcf_cv = input$bt_fcf_cv,
        bt_w_mom = input$bt_w_mom,
        bt_w_rsi = input$bt_w_rsi,
        bt_w_vg = input$bt_w_vg
      )
      withProgress(message = paste("回測", current_ticker(), "中…"), value = 0.3, {
        res <- run_company_backtest(
          ticker = current_ticker(),
          d_is = d_income_statement(),
          d_bs = d_balance_sheet(),
          d_cf = d_cash_flow(),
          params = params,
          mos = bt_current_mos(),
          bench_ticker = "SPY",
          years = 5
        )
        incProgress(0.9)
        bt_result(res)
        bt_run_msg(sprintf(
          "完成：%s 日資料，基準=%s，較佳策略=模式 %s",
          res$n_days, res$bench_ticker, res$metrics$best
        ))
      })
    }, error = function(e) {
      bt_run_msg(paste("失敗：", e$message))
      showNotification(paste("❌ 回測失敗：", e$message), type = "error", duration = 12)
    })
  })

  output$bt_equity_plot <- renderPlotly({
    res <- bt_result()
    validate(need(!is.null(res) && !is.null(res$equity_df), "請先成功執行回測"))

    df_plot <- res$equity_df
    p <- ggplot(df_plot, aes(x = Date)) +
      geom_line(aes(y = Model_A, color = "模式 A (情緒增強)"), linewidth = 0.9) +
      geom_line(aes(y = Model_B, color = "模式 B (純基本面)"), linewidth = 0.9) +
      geom_line(aes(y = BuyHold, color = "該股買進持有"), linewidth = 0.7) +
      geom_line(aes(y = Benchmark, color = "大盤基準"), linetype = "dashed", linewidth = 0.7) +
      scale_color_manual(values = c(
        "模式 A (情緒增強)" = "#007bff",
        "模式 B (純基本面)" = "#dc3545",
        "該股買進持有" = "#28a745",
        "大盤基準" = "#6c757d"
      )) +
      labs(y = "累積淨值", x = "日期", color = "策略") +
      theme_minimal()

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$perf_metrics <- renderUI({
    res <- bt_result()
    if (is.null(res) || is.null(res$metrics)) {
      return(
        tags$div(
          style = "color: #888; font-size: 12.5px; line-height: 1.55;",
          icon("chart-bar"),
          " 執行回測後，此處會顯示 ",
          tags$b("Sharpe 比率"), "（風險調整後報酬）、",
          tags$b("最大回撤"), "（歷史最大虧損幅度）與",
          tags$b("參數高原"), "（兩策略穩定性粗評）。"
        )
      )
    }
    m <- res$metrics
    best <- m$best
    sharpe_show <- if (identical(best, "A")) m$sharpe_a else m$sharpe_b
    mdd_show <- if (identical(best, "A")) m$mdd_a else m$mdd_b
    label_best <- if (identical(best, "A")) "模式 A" else "模式 B"
    sharpe_a_txt <- if (is.na(m$sharpe_a)) "N/A" else sprintf("%.2f", m$sharpe_a)
    sharpe_b_txt <- if (is.na(m$sharpe_b)) "N/A" else sprintf("%.2f", m$sharpe_b)

    .ynow_metric_card <- function(value, label, caption, icon_name, tone, tip) {
      tipify(
        tags$div(
          class = paste0("ynow-metric-card ynow-metric-card--", tone),
          tags$div(
            class = "ynow-metric-card__body",
            tags$div(
              class = "ynow-metric-card__top",
              tags$span(class = "ynow-metric-card__icon", icon(icon_name)),
              tags$p(class = "ynow-metric-card__label", label)
            ),
            tags$div(class = "ynow-metric-card__value", value),
            tags$p(class = "ynow-metric-card__caption", caption)
          )
        ),
        tip,
        placement = "bottom"
      )
    }

    tagList(
      tags$p(
        style = "margin: 0 0 12px 0; font-size: 12px; color: #666;",
        "以下以 Sharpe 較高的策略為主顯示；A＝", sharpe_a_txt, "，B＝", sharpe_b_txt,
        "。數值僅供策略比較參考，不代表未來績效。"
      ),
      tags$div(
        id = "bt_perf_metrics_boxes",
        class = "ynow-metric-grid",
        .ynow_metric_card(
          value = if (is.na(sharpe_show)) "N/A" else sprintf("%.2f", sharpe_show),
          label = paste0("Sharpe 比率（較佳：", label_best, "）"),
          caption = "風險調整後報酬；>1 通常視為不錯，>2 屬優異（依市場而異）。",
          icon_name = "chart-line",
          tone = "green",
          tip = "年化 Sharpe ≈ 日報酬均值 ÷ 標準差 × √252。愈高代表單位風險下報酬愈佳。"
        ),
        .ynow_metric_card(
          value = if (is.na(mdd_show)) "N/A" else paste0(sprintf("%.1f", mdd_show * 100), "%"),
          label = paste0("最大回撤 Max DD（", label_best, "）"),
          caption = "歷史最大虧損幅度；愈接近 0 代表回撤愈小（負值愈大風險愈高）。",
          icon_name = "arrow-down",
          tone = "red",
          tip = "淨值自歷史高點回落的最大百分比幅度。"
        ),
        .ynow_metric_card(
          value = m$plateau,
          label = "參數高原（粗評）",
          caption = "「高原」代表參數微調不致劇烈改變結果；「敏感」宜再檢查門檻設定。",
          icon_name = "mountain",
          tone = "violet",
          tip = "比較模式 A/B 的 Sharpe 差距；差距小代表策略分化不大。"
        )
      )
    )
  })
  
  # ==========================================
  # 11. 系統按鈕與報告輸出
  # ==========================================
  # DDM Reset 由 ddm_module_server 內的 input$reset_ddm 處理（ns: mod_ddm）
  
  observeEvent(input$reset_dcf, {
    updateNumericInput(session, "years", value = APP_DEFAULTS$years)
    updateSelectInput(session, "perpetual_g_method", selected = APP_DEFAULTS$perpetual_g_method)
    updateSelectInput(session, "lifecycle_stage", selected = APP_DEFAULTS$lifecycle_stage)
    updateNumericInput(session, "wacc_gordon", value = APP_DEFAULTS$wacc_gordon)
    updateNumericInput(session, "yr_stage1", value = APP_DEFAULTS$yr_stage1)
    updateNumericInput(session, "g_stage1", value = APP_DEFAULTS$g_stage1)
    updateNumericInput(session, "wacc_stage1", value = APP_DEFAULTS$wacc_gordon)
    updateNumericInput(session, "wacc_stage2", value = APP_DEFAULTS$wacc_gordon)
    # 依當前方法重算 g（勿寫死舊 SGR）
    est <- tryCatch(isolate(central_perpetual_g()), error = function(e) NULL)
    if (is.null(est) || !is.finite(est$g_pct)) {
      updateNumericInput(session, "sgr", value = APP_DEFAULTS$sgr)
    } else {
      .push_perpetual_g(est, notify_two_stage = FALSE)
    }
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
