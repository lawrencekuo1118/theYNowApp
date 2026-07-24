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
  corp_display_name <- reactiveVal("")
  
  # 初始值設為 NULL，避免一開啟 App 就自動執行爬蟲
  current_ticker <- reactiveVal(NULL)
  # 首次按下 Search 前：不標示模型推薦／側邊欄「推薦」
  user_has_searched <- reactiveVal(FALSE)
  
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
    user_has_searched(TRUE)
    current_ticker(toupper(trimws(input$txt_search)))
  })
  
  observeEvent(input$search, {
    req(input$sc)
    user_has_searched(TRUE)
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

        # Prefer full legal/display name from Summary or industry lookup (not ticker alone).
        .pick_company_name <- function(..., ticker = "") {
          cands <- unlist(list(...), use.names = FALSE)
          cands <- trimws(as.character(cands))
          cands <- cands[!is.na(cands) & nzchar(cands)]
          if (!length(cands)) {
            return(if (nzchar(ticker)) as.character(ticker) else "")
          }
          tk <- toupper(trimws(as.character(ticker)))
          non_tk <- cands[toupper(cands) != tk]
          pool <- if (length(non_tk)) non_tk else cands
          pool[[which.max(nchar(pool))]]
        }
        corp_display_name(.pick_company_name(
          attr(sum_df, "company_name"),
          if (!is.null(ind_info)) ind_info$company_name else NULL,
          ticker = stock_code
        ))

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
    nm <- corp_display_name()
    if (!is.null(nm) && nzchar(trimws(as.character(nm)))) {
      return(as.character(nm)[1])
    }
    if (is.null(summary_data())) return("")
    name <- attr(summary_data(), "company_name")
    if (is.null(name) || is.na(name) || !nzchar(as.character(name))) {
      tk <- current_ticker()
      if (!is.null(tk) && nzchar(tk)) return(paste("Stock:", tk))
      return("")
    }
    as.character(name)[1]
  }
  
  output$txt_corpname <- renderText({ render_corpname_logic() })
  output$search_results <- renderText({ corp_industry_text() })
  output$recentsearch <- renderText({ paste(values$recentsearch, collapse = ", ") })
  output$today <- renderText({ format(Sys.Date(), "%Y/%m/%d") })

  output$dashboard_selected_industry <- renderUI({
    key <- as.character(input$industry_choice %||% "")
    if (!nzchar(key)) {
      return(tags$div(
        style = "margin: 0 0 12px 0; padding: 10px 12px; background: #f7f7f7; border-left: 4px solid #999; border-radius: 4px;",
        tags$span(style = "color:#666; font-size:13px;", "尚未選擇比較產業（請至 Get Started → Industry Standard Comparison）")
      ))
    }
    lab <- industry_labels[[key]]
    if (is.null(lab) || is.na(lab) || !nzchar(as.character(lab))) lab <- key
    yahoo <- corp_industry_text()
    inds <- industry_standards[[key]]
    meta_bits <- character(0)
    if (!is.null(inds$beta_avg) && is.finite(inds$beta_avg)) {
      meta_bits <- c(meta_bits, paste0("β≈", round(inds$beta_avg, 2)))
    }
    if (!is.null(inds$rm_avg) && is.finite(inds$rm_avg)) {
      meta_bits <- c(meta_bits, paste0("Rm≈", round(inds$rm_avg, 1), "%"))
    }
    if (!is.null(inds$roe) && length(inds$roe) >= 2) {
      meta_bits <- c(meta_bits, paste0("ROE ", round(inds$roe[1], 1), "–", round(inds$roe[2], 1), "%"))
    }
    meta_line <- if (length(meta_bits)) paste(meta_bits, collapse = "　") else NULL

    tags$div(
      style = "margin: 0 0 12px 0; padding: 10px 14px; background: #f4f8fb; border-left: 4px solid #3c8dbc; border-radius: 4px;",
      tags$div(
        style = "font-size: 14px; color: #222; line-height: 1.45;",
        tags$b("目前比較產業："),
        tags$span(style = "font-weight: 700; color: #1a5276;", lab),
        tags$span(style = "color:#888; font-size:12px; margin-left:8px;", paste0("(", key, ")"))
      ),
      if (!is.null(meta_line)) {
        tags$div(style = "margin-top:4px; font-size:12.5px; color:#555;", meta_line)
      },
      if (!is.null(yahoo) && nzchar(as.character(yahoo))) {
        tags$div(
          style = "margin-top:4px; font-size:12px; color:#777;",
          tags$span(style = "font-weight:600;", "Yahoo："),
          as.character(yahoo)[1]
        )
      }
    )
  })
  
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

  # Dashboard「回測濾鏡」：目前公司 KPI vs 持倉回測條件門檻
  bt_filter_state <- reactiveVal(NULL)

  observeEvent(input$bt_kpi_filter, {
    # Toggle: when a filter result is showing, second click clears it
    st0 <- bt_filter_state()
    if (!is.null(st0)) {
      bt_filter_state(NULL)
      showNotification("已取消回測濾鏡。", type = "message", duration = 4)
      return()
    }
    is_df <- tryCatch(d_income_statement(), error = function(e) NULL)
    cf_df <- tryCatch(d_cash_flow(), error = function(e) NULL)
    if (is.null(is_df) || is.null(cf_df)) {
      bt_filter_state(list(status = "empty", message = "尚無財報資料，請先搜尋並載入公司。"))
      showNotification("尚無財報，無法評估回測濾鏡。", type = "warning", duration = 6)
      return()
    }
    metrics <- tryCatch(
      compute_dashboard_filter_metrics(is_df, cf_df),
      error = function(e) NULL
    )
    if (is.null(metrics)) {
      bt_filter_state(list(status = "empty", message = "指標計算失敗。"))
      showNotification("回測濾鏡計算失敗。", type = "error", duration = 6)
      return()
    }
    thr <- list(
      bt_net_margin = input$bt_net_margin,
      bt_rev_growth = input$bt_rev_growth,
      bt_eps_growth = input$bt_eps_growth,
      bt_fcf_cv = input$bt_fcf_cv
    )
    ev <- evaluate_holding_filter(metrics, thr)
    bt_filter_state(list(
      status = if (isTRUE(ev$overall)) "pass" else "fail",
      eval = ev,
      metrics = metrics,
      ticker = current_ticker() %||% ""
    ))
    showNotification(
      if (isTRUE(ev$overall)) "✅ 回測濾鏡：達標（再點一次可取消）"
      else "⚠️ 回測濾鏡：未達標（再點一次可取消）",
      type = if (isTRUE(ev$overall)) "message" else "warning",
      duration = 7
    )
  })

  output$bt_filter_badge <- renderUI({
    st <- bt_filter_state()
    if (is.null(st)) {
      return(tags$span(
        style = "font-size:12px;color:#888;padding:4px 10px;border:1px solid #ddd;border-radius:4px;background:#f7f7f7;",
        "尚未比對"
      ))
    }
    if (identical(st$status, "empty")) {
      return(tags$span(
        style = "font-size:12px;font-weight:600;color:#666;padding:4px 10px;border:1px solid #ccc;border-radius:4px;background:#eee;",
        "尚無資料"
      ))
    }
    if (identical(st$status, "pass")) {
      tags$span(
        style = "font-size:12px;font-weight:700;color:#fff;padding:4px 12px;border-radius:4px;background:#1e8449;",
        "達標"
      )
    } else {
      tags$span(
        style = "font-size:12px;font-weight:700;color:#fff;padding:4px 12px;border-radius:4px;background:#c0392b;",
        "未達標"
      )
    }
  })

  output$bt_filter_detail <- renderUI({
    st <- bt_filter_state()
    if (is.null(st) || identical(st$status, "empty")) {
      if (!is.null(st) && !is.null(st$message)) {
        return(tags$div(
          style = "margin:0 0 12px 0;padding:8px 12px;background:#f5f5f5;border-left:3px solid #999;font-size:12px;color:#555;",
          st$message
        ))
      }
      return(NULL)
    }
    rows <- st$eval$rows
    fmt <- function(x) {
      if (is.null(x) || length(x) < 1 || is.na(x) || !is.finite(x)) return("N/A")
      sprintf("%.2f%%", as.numeric(x))
    }
    cells <- lapply(rows, function(r) {
      ok <- isTRUE(r$pass)
      tags$tr(
        tags$td(r$label),
        tags$td(fmt(r$actual)),
        tags$td(paste0(r$op, " ", fmt(r$threshold))),
        tags$td(
          style = if (ok) "color:#1e8449;font-weight:600;" else "color:#c0392b;font-weight:600;",
          if (ok) "過" else "未過"
        )
      )
    })
    tags$div(
      style = "margin:0 0 14px 0;padding:10px 12px;background:#f8fafc;border:1px solid #dce3ea;border-radius:4px;",
      tags$div(
        style = "font-size:12px;color:#444;margin-bottom:6px;",
        tags$b("回測濾鏡明細"),
        if (nzchar(st$ticker %||% "")) paste0(" · ", st$ticker) else NULL,
        "（對照「持倉回測條件」門檻；虧損期淨利率／NI 成長可放寬，同回測引擎）"
      ),
      tags$table(
        style = "width:100%;font-size:12px;border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th("指標"), tags$th("實際"), tags$th("門檻"), tags$th("結果")
        )),
        tags$tbody(cells)
      )
    )
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
  # 📌 v13.0：分類 → 主／副模型（側邊欄僅標主模型「推薦」）
  # ==========================================
  .empty_model_rec <- function(summary_method, reason, company_type = "pending") {
    list(
      company_type = company_type, primary = "", secondary = NULL,
      ddm = FALSE, dcf = FALSE, pb = FALSE, ri = FALSE, tags = character(0),
      summary_method = summary_method, reason = reason,
      suggest_two_stage = FALSE,
      confidence_inputs = list(data_complete = FALSE)
    )
  }

  model_sidebar_rec <- reactive({
    if (!isTRUE(user_has_searched())) {
      return(.empty_model_rec(
        "尚未搜尋",
        "請先按下 Search 載入公司後產生推薦。"
      ))
    }
    cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
    is <- tryCatch(d_income_statement(), error = function(e) NULL)
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    ind <- corp_industry_text()
    if (is.null(cf) || !is.data.frame(cf) || nrow(cf) == 0) {
      return(.empty_model_rec(
        "等待財報資料",
        "搜尋股票並載入財報後產生推薦。",
        company_type = "fallback"
      ))
    }
    recommend_valuation_models(
      cf,
      industry_text = ind,
      d_is = is,
      d_bs = bs,
      industry_choice = input$industry_choice
    )
  })

  # Dynamic 「推薦」only on primary — patch badges in-place.
  sidebar_badge_sig <- reactiveVal("")
  observe({
    rec <- model_sidebar_rec()
    prim <- as.character(rec$primary %||% "")
    sig <- paste(prim, rec$secondary %||% "", rec$company_type %||% "", sep = "|")
    if (identical(sidebar_badge_sig(), sig)) return()
    sidebar_badge_sig(sig)
    payload <- list(
      dcf_calculator = list(on = identical(prim, "dcf")),
      ddm_calculator = list(on = identical(prim, "ddm")),
      pb_calculator = list(on = identical(prim, "pb")),
      ri_calculator = list(on = identical(prim, "ri"))
    )
    session$sendCustomMessage("ynowSidebarBadges", payload)
  })

  # Growth classification → 僅提示 Two-Stage（不再強制覆寫；預設維持 Gordon）
  observeEvent(model_sidebar_rec(), {
    rec <- model_sidebar_rec()
    if (!isTRUE(rec$suggest_two_stage)) return()
    if (identical(input$dcf_mode, "two_stage")) return()
    showNotification(
      "模型建議：高成長標的可考慮切換為「二階段成長法」，目前預設仍為明確預測 + Gordon 終值。",
      type = "message", duration = 6, id = "ynow_suggest_two_stage"
    )
  }, ignoreInit = TRUE)

  output$get_started_model_selector <- renderUI({
    rec <- model_sidebar_rec()
    prim <- as.character(rec$primary %||% "")
    sec <- as.character(rec$secondary %||% "")
    mark_roles <- nzchar(prim)
    make_card <- function(title, key, icon_name, color, formula, notes) {
      role <- if (!mark_roles) {
        NULL
      } else if (identical(key, prim)) {
        "主模型"
      } else if (nzchar(sec) && identical(key, sec)) {
        "副模型"
      } else {
        "備選"
      }
      active <- identical(role, "主模型")
      border_col <- if (identical(role, "主模型")) color else if (identical(role, "副模型")) "#888" else "#ddd"
      bg <- if (identical(role, "主模型")) "#fffaf2" else if (identical(role, "副模型")) "#f7f9fc" else "#fff"
      badge_bg <- if (identical(role, "主模型")) color else if (identical(role, "副模型")) "#6c757d" else "#999"
      tags$div(
        class = paste("col-sm-3", if (isTRUE(active)) "ynow-model-rec-active" else ""),
        tags$div(
          style = paste0(
            "border:1px solid ", border_col, ";",
            "border-radius:8px; padding:14px; min-height:170px; background:", bg,
            "; box-shadow:0 2px 4px rgba(0,0,0,0.04);"
          ),
          tags$div(style = paste0("font-size:22px; color:", color, ";"), icon(icon_name)),
          tags$h4(style = "margin:8px 0 4px 0; font-weight:700;", title),
          if (!is.null(role)) tags$span(
            style = paste0(
              "display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px; color:#fff; background:",
              badge_bg, ";"
            ),
            role
          ),
          tags$p(style = "margin:10px 0 4px 0; font-size:12px; color:#555;", formula),
          tags$p(style = "margin:0; font-size:12px; color:#777; line-height:1.4;", notes)
        )
      )
    }

    type_lab <- switch(
      as.character(rec$company_type %||% ""),
      "financial" = "金融／帳面驅動",
      "holding_asset" = "控股／資產導向",
      "growth" = "高成長",
      "mature" = "成熟穩定",
      "fallback" = "資料受限",
      "pending" = "待搜尋",
      "待分類"
    )

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
        tags$b("公司分類："), type_lab,
        if (mark_roles) tagList(
          tags$span(style = "margin:0 8px; color:#bbb;", "|"),
          tags$b("主模型："), .model_label(prim),
          if (nzchar(sec)) tagList(
            tags$span(style = "margin:0 8px; color:#bbb;", "|"),
            tags$b("副模型："), .model_label(sec)
          )
        ) else tagList(
          tags$span(style = "margin:0 8px; color:#bbb;", "|"),
          tags$b("主模型："), "尚未標示"
        ),
        tags$br(),
        tags$span(rec$reason %||% "請先按下 Search 載入公司後產生推薦。")
      ),
      fluidRow(
        make_card("DCF", "dcf", "calculator", "#00a65a", "EV = Σ FCFF / (1+WACC)^t + TV / (1+WACC)^n", "適合 FCF 為正且相對穩定的企業。"),
        make_card("DDM", "ddm", "hand-holding-usd", "#f39c12", "P0 = D1 / (Ke - g)", "適合持續且穩定配息的企業。"),
        make_card("P/B", "pb", "landmark", "#3c8dbc", "P = BVPS × 有來源目標 P/B", "金融／保險／控股；倍數來自 Justified＋產業＋歷史。"),
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
      c("DCF", "Chart Layers", paste(.snapshot_value(input$dcf_chart_layers), collapse = "+"), "Overlay: hist / forecast FCFF bars + PV / DCF lines"),
      c("DCF", "Chart Mode", {
        ly <- input$dcf_chart_layers %||% character(0)
        if ("dcf" %in% ly || "pv_fcff" %in% ly) "with_dcf" else "simple"
      }, "Derived from dcf_chart_layers"),
      c("Perpetual Growth", "Method", .snapshot_value(input$perpetual_g_method), "macro / fundamental / lifecycle"),
      c("Perpetual Growth", "Terminal g / SGR (%)", .snapshot_value(input$sgr), "DCF/RI terminal g; TV = FCF_n × (1+g) / (WACC-g)"),
      c("Perpetual Growth", "Estimated g (%)", if (!is.null(est_g)) .snapshot_value(est_g$g_pct) else NA_character_, "Selected perpetual-growth method output"),
      c("Perpetual Growth", "Lifecycle Stage", .snapshot_value(input$lifecycle_stage), "Lifecycle classification used when method = lifecycle"),
      c("DCF - Explicit+Gordon TV", "WACC (%)", .snapshot_value(input$wacc_gordon), "EV = Σ PV(FCFF) + PV(TV); not single-period Gordon"),
      c("DCF - Two Stage", "Stage 1 Years", .snapshot_value(input$yr_stage1), "Explicit high-growth period"),
      c("DCF - Two Stage", "g1 (%)", .snapshot_value(input$g_stage1), "FCFF_t = FCFF_(t-1) × (1+g1)"),
      c("DCF - Two Stage", "WACC1 (%)", .snapshot_value(input$wacc_stage1), "PV stage 1 = FCFF_t / (1+WACC1)^t"),
      c("DCF - Two Stage", "WACC2 (%)", .snapshot_value(input$wacc_stage2), "Terminal discount rate"),
      c("DCF - WACC", "Calculated WACC (%)", .snapshot_value(wacc_pct), "System CAPM/WACC estimate (also synced into WACC inputs)"),
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
      c("Backtest", "Net Margin Threshold (%)", .snapshot_value(input$bt_net_margin), "持倉回測條件: Net Margin >= threshold"),
      c("Backtest", "Revenue Growth Threshold (%)", .snapshot_value(input$bt_rev_growth), "持倉回測條件: Revenue Growth >= threshold"),
      c("Backtest", "EPS / NI Growth Threshold (%)", .snapshot_value(input$bt_eps_growth), "持倉回測條件: EPS/NI Growth >= threshold"),
      c("Backtest", "FCF CV Ceiling (%)", .snapshot_value(input$bt_fcf_cv), "持倉回測條件: FCF CV <= ceiling"),
      c("Backtest", "Max Exposure (bt_max_exp)", .snapshot_value(input$bt_max_exp), "Mode A ceiling; 1.0 can fit Buy&Hold"),
      c("Backtest", "Min Exp After Pass (bt_min_exp_pass)", .snapshot_value(input$bt_min_exp_pass), "Floor when filter passes & MOS >= -10%"),
      c("Backtest", "Auto Derive Params", .snapshot_value(input$bt_param_auto), "TRUE = sync thresholds/weights/model on ticker load"),
      c("Backtest", "回測用評價模型", paste(.snapshot_value(input$bt_fv_models), collapse = ", "), "Multi-select FV overlay; first drives MOS/Exp_A"),
      c("Backtest", "MOS / VG Weight (bt_w_vg)", .snapshot_value(input$bt_w_vg), "Exposure diagnostic blend; not FV path"),
      c("Backtest", "Momentum Weight (bt_w_mom)", .snapshot_value(input$bt_w_mom), "Sentiment overlay relative weight"),
      c("Backtest", "RSI Weight (bt_w_rsi)", .snapshot_value(input$bt_w_rsi), "Sentiment overlay relative weight"),
      c("Backtest", "Hist Discount Beta", "Rolling β (≈5Y monthly vs SPY)", "PIT Ke/WACC at each rebalance; not fixed session β")
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

  .format_default_value <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (length(x) == 1 && isTRUE(is.na(x))) return(NA_character_)
    if (is.logical(x)) return(paste(as.character(x), collapse = ", "))
    if (is.numeric(x)) {
      return(paste(vapply(x, function(v) {
        if (!is.finite(v)) return(NA_character_)
        if (abs(v - round(v)) < 1e-9) as.character(as.integer(round(v))) else as.character(round(v, 4))
      }, character(1)), collapse = ", "))
    }
    paste(as.character(x), collapse = ", ")
  }

  defaults_rows <- reactive({
    # Human labels for APP_DEFAULTS keys (unlisted keys still appear by key name)
    label_map <- list(
      stock_code = c("基本設定", "預設股票代碼", "啟動／重置用 ticker"),
      industry_choice = c("基本設定", "預設產業鍵", "industry_standards 鍵名"),
      years = c("DCF", "預測年數 n", "Explicit forecast horizon"),
      ddm_d0 = c("DDM", "D0", "通常由財報自動帶入"),
      ddm_g = c("DDM", "股利成長 g (%)", "預設對齊中央 SGR"),
      ddm_ke = c("DDM", "Ke (%)", "預設對齊 CAPM Re"),
      ddm_sync_central_g = c("DDM", "與中央 SGR 同步", "TRUE 時 DDM g 跟隨 SGR"),
      dcf_mode = c("DCF", "DCF 模式", "gordon / two_stage"),
      dcf_chart_mode = c("DCF", "圖表模式(相容)", "simple / with_dcf（由疊圖層推導）"),
      dcf_chart_layers = c("DCF", "疊圖層級", "hist / forecast / dcf / pv_fcff"),
      g_growth_method = c("DCF", "營收成長估計法", "fundamental / revenue CAGR 等"),
      custom_g = c("DCF", "自訂營收成長 g (%)", "封頂後的短中期營收成長"),
      perpetual_g_method = c("永續成長", "方法", "macro / fundamental / lifecycle"),
      lifecycle_stage = c("永續成長", "生命週期", "auto 或手動階段"),
      sgr = c("永續成長", "SGR / 終值 g (%)", "DCF／RI 終值成長上限錨"),
      wacc_gordon = c("DCF", "Gordon WACC (%)", "由 WACC 分頁同步；隱藏欄位"),
      yr_stage1 = c("Two-Stage", "高速期年數", "Stage 1 years"),
      g_stage1 = c("Two-Stage", "高速期 g1 (%)", "Stage 1 growth"),
      g_stage2 = c("Two-Stage", "穩定期 g2 (%)", "通常對齊 SGR"),
      wacc_stage1 = c("Two-Stage", "WACC1 (%)", "Stage 1 discount"),
      wacc_stage2 = c("Two-Stage", "WACC2 (%)", "Terminal discount"),
      wacc_re = c("WACC", "Re (%)", "Cost of equity"),
      wacc_rd = c("WACC", "Rd (%)", "Cost of debt"),
      wacc_tax = c("WACC", "稅率 T (%)", "After-tax debt cost"),
      use_est_re = c("WACC", "使用 CAPM Re", "TRUE = Re 跟 CAPM"),
      capm_rf = c("CAPM", "Rf (%)", "無風險利率（啟動時估）"),
      capm_beta = c("CAPM", "Beta", "啟動占位；搜尋後可跟 Summary"),
      use_industry_beta = c("CAPM", "使用產業 Beta", "FALSE = 跟 Finance Summary β"),
      beta_bench = c("Beta", "基準指數", "Rolling β 對齊標的，預設 SPY"),
      beta_lookback_months = c("Beta", "回溯月數", "月末報酬視窗"),
      beta_min_obs = c("Beta", "最少觀測", "估計所需最低月數"),
      capm_rm = c("CAPM", "Rm (%)", "預期市場報酬"),
      pb_bvps = c("P/B", "BVPS", "通常由財報帶入"),
      pb_tbvps = c("P/B", "TBVPS", "通常由財報帶入"),
      pb_low = c("P/B", "P/B Low", "產業帶／保守下緣"),
      pb_mid = c("P/B", "P/B Mid", "產業帶中位"),
      pb_high = c("P/B", "P/B High", "產業帶上緣"),
      pb_basis = c("P/B", "Basis", "bvps / tbvps"),
      pb_use_industry = c("P/B", "使用產業 P/B", "TRUE = 跟產業帶"),
      pb_adjust_share_class = c("P/B", "約當股數校正", "雙重股權／市值÷股價例外")
    )

    keys <- names(APP_DEFAULTS)
    rows <- lapply(keys, function(k) {
      meta <- label_map[[k]]
      if (is.null(meta)) {
        meta <- c("其他", k, "APP_DEFAULTS 欄位")
      }
      c(meta[1], meta[2], k, .format_default_value(APP_DEFAULTS[[k]]), meta[3])
    })

    # Module / UI hard defaults not stored in APP_DEFAULTS
    ind_roe_def <- {
      v <- if (exists(".industry_roe_pct", mode = "function")) {
        .industry_roe_pct(APP_DEFAULTS$industry_choice)
      } else {
        NA_real_
      }
      .format_default_value(if (is.finite(v)) round(v, 2) else 12)
    }
    extra <- list(
      c("RI", "預測期 (Years)", "ri_years", "5", "RI 模組重設預設"),
      c("RI", "起始／預期 ROE (%)", "ri_roe", "15", "財報載入前占位；載入後覆寫"),
      c("RI", "配息率 Payout (%)", "ri_payout", "40", "財報載入前占位；載入後覆寫"),
      c("RI", "ROE 預測方法", "roe_method", "constant", "constant / linear / industry / custom"),
      c("RI", "Industry Average ROE (%)", "roe_industry", ind_roe_def,
        "依預設產業 ROE 區間中位；可編輯"),
      c("Backtest", "FV 模型勾選", "bt_fv_models", "(none)", "HFV 圖預設不勾選，勾選才計算")
    )
    rows <- c(rows, extra)

    df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(df) <- c("Section", "Parameter", "Key", "Default Value", "Note")
    df
  })

  output$defaults_table <- renderDataTable({
    datatable(
      defaults_rows(),
      rownames = FALSE,
      options = list(pageLength = 30, scrollX = TRUE, order = list(list(0, "asc")))
    )
  })

  output$download_defaults <- downloadHandler(
    filename = function() {
      paste0("YNow_defaults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- defaults_rows()
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
  # 📈 Balance Sheet 圓餅圖（資產／負債／權益）
  # 會計恆等式：Assets = Liabilities + Equity
  # 甜甜圈顯示負債／權益佔總資產比例；中心標註總資產
  # ==========================================
  output$bs_plot <- renderPlotly({
    tk <- current_ticker()
    empty_title <- paste0(if (is.null(tk) || !nzchar(tk)) "Balance Sheet" else tk,
                          " - 資產／負債／權益 (無資料)")
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    if (is.null(bs) || !is.data.frame(bs) || nrow(bs) == 0) {
      return(plotly::plotly_empty() %>% plotly::layout(title = empty_title))
    }

    assets <- tryCatch(
      select_current_metric(bs, "Total Assets", "stock"),
      error = function(e) NA_real_
    )
    equity <- tryCatch(
      select_current_metric_any(bs, EQUITY_PATTERNS, "stock"),
      error = function(e) NA_real_
    )
    liabilities <- tryCatch(
      select_current_metric_any(
        bs,
        c(
          "Total Liabilities Net Minority Interest",
          "^Total Liabilities$",
          "Total Liab"
        ),
        "stock"
      ),
      error = function(e) NA_real_
    )
    # Yahoo 偶缺總負債列：以會計恆等式 Assets − Equity 回推
    if ((!is.finite(liabilities) || liabilities == 0) &&
        is.finite(assets) && is.finite(equity)) {
      liabilities <- assets - equity
    }
    # 缺總資產時，以 負債＋權益 回推（僅正值）
    if (!is.finite(assets) || assets <= 0) {
      parts <- c(liabilities, equity)
      parts <- parts[is.finite(parts) & parts > 0]
      if (length(parts) > 0) assets <- sum(parts)
    }

    labels <- c("負債 Liabilities", "權益 Equity")
    values <- c(liabilities, equity)
    colors <- c("#dd4b39", "#00a65a")
    keep <- is.finite(values) & values > 0
    if (!any(keep) || !is.finite(assets) || assets <= 0) {
      return(plotly::plotly_empty() %>% plotly::layout(title = empty_title))
    }

    plot_df <- data.frame(
      Category = labels[keep],
      Value = values[keep],
      Color = colors[keep],
      stringsAsFactors = FALSE
    )
    # 佔總資產比例（會計恆等式口徑）
    plot_df$Pct <- plot_df$Value / assets * 100
    plot_df$Hover <- paste0(
      "<b>", plot_df$Category, "</b><br>",
      "金額: <b>", format_dollar_abbr(plot_df$Value), "</b><br>",
      "佔總資產: <b>", sprintf("%.1f%%", plot_df$Pct), "</b><br>",
      "總資產 Assets: <b>", format_dollar_abbr(assets), "</b>"
    )

    center_txt <- paste0(
      "總資產<br><b>", format_dollar_abbr(assets), "</b>"
    )

    plotly::plot_ly(
      plot_df,
      labels = ~Category,
      values = ~Value,
      type = "pie",
      hole = 0.48,
      marker = list(colors = plot_df$Color, line = list(color = "#ffffff", width = 1.5)),
      textinfo = "label+percent",
      hovertext = ~Hover,
      hoverinfo = "text",
      sort = FALSE,
      direction = "clockwise"
    ) %>%
      plotly::layout(
        title = list(
          text = paste0(tk %||% "", " - 資產負債結構（資產／負債／權益）"),
          font = list(size = 15, color = "#2c3e50")
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
        margin = list(t = 50, b = 40, l = 20, r = 20),
        annotations = list(
          list(
            text = center_txt,
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 13, color = "#2c3e50"),
            align = "center"
          )
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
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
  
  # --- 新增 2：掛載投資決策漏斗模組 (Decision Funnel; v13 區間＋可信度) ---
  # primary_band / secondary / confidence 於各估值模組掛載後定義（lazy 查找）
  decision_server(
    id = "main_decision",
    d_is = d_income_statement,
    d_bs = d_balance_sheet,
    d_cf = d_cash_flow,
    intrinsic_val_dcf = stock_price_estimate_val,
    intrinsic_val_ddm = reactive({
      if (!is.null(ddm_results$ddm_price)) ddm_results$ddm_price() else NA
    }),
    intrinsic_val_pb = reactive({
      if (!is.null(pb_results$pb_price)) pb_results$pb_price() else NA
    }),
    current_price = reactive({
      req(scraped_market_cap())
      scraped_market_cap()$price
    }),
    hist_price_data = hist_stock_data,
    industry_text = corp_industry_text,
    model_rec = reactive({ model_sidebar_rec() }),
    primary_band = reactive({ primary_valuation_band() }),
    secondary_point = reactive({ secondary_valuation_point() }),
    confidence = reactive({ valuation_confidence() })
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
    # Prefer live WACC inputs (auto-filled from CAPM/WACC calc on the DCF tab).
    if (isTRUE(input$dcf_mode == "two_stage") && !is.null(input$wacc_stage2) && is.finite(input$wacc_stage2)) {
      return(as.numeric(input$wacc_stage2))
    }
    if (!is.null(input$wacc_gordon) && is.finite(input$wacc_gordon)) {
      return(as.numeric(input$wacc_gordon))
    }
    if (!is.null(calculated_wacc()) && is.finite(calculated_wacc())) {
      return(as.numeric(calculated_wacc()) * 100)
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
      # 不再自動切換 dcf_mode（維持 Gordon 預設）；僅同步 g1 供使用者改 Two-Stage 時使用
      if (isTRUE(notify_two_stage) && !identical(input$dcf_mode, "two_stage")) {
        showNotification(
          "Lifecycle：高速→成熟。終值 g 已收斂；若要改用 Two-Stage，請在 DCF 模型手動切換。",
          type = "message", duration = 6, id = "ynow_lifecycle_two_stage"
        )
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
      input$wacc_stage2
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
    }),
    industry_choice = reactive(input$industry_choice),
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
    adjust_share_class = reactive(isTRUE(input[["mod_pb-adjust_share_class"]]))
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
    industry_text = corp_industry_text,
    central_ke = central_ke,
    central_g_pct = reactive({
      if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) as.numeric(input$sgr) else APP_DEFAULTS$sgr
    }),
    hist_prices = hist_stock_data
  )

  # ==========================================
  # v13：股數級距（DCF／RI／敏感度共用）
  # ==========================================
  .valuation_shares <- reactive({
    raw_shares <- tryCatch(
      select_current_metric(
        d_balance_sheet(),
        "Ordinary Shares Number|Share Issued|Total Shares Outstanding|Basic Average Shares",
        "stock"
      ),
      error = function(e) NA_real_
    )
    px <- tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    mcap <- tryCatch({
      df <- summary_data()
      if (is.null(df) || !is.data.frame(df)) return(NA_real_)
      row <- df[df$Item == "Market Cap (intraday)", , drop = FALSE]
      if (nrow(row) < 1) return(NA_real_)
      parse_financial_number(row$Value[1])[1]
    }, error = function(e) NA_real_)
    tk <- tryCatch(current_ticker(), error = function(e) "")
    sh <- resolve_shares_for_price(raw_shares, price = px, market_cap = mcap, ticker = tk)
    apply_adj <- isTRUE(input[["mod_pb-adjust_share_class"]]) ||
      identical(sh$method, "brk_b_x1500") ||
      identical(sh$method, "market_cap_per_price")
    if (isTRUE(apply_adj) && is.finite(sh$shares) && sh$shares > 0) {
      return(list(shares = sh$shares, note = sh$note, method = sh$method))
    }
    if (is.finite(raw_shares) && raw_shares > 0) {
      return(list(shares = raw_shares, note = NULL, method = "balance_sheet"))
    }
    list(shares = if (is.finite(sh$shares) && sh$shares > 0) sh$shares else 1,
         note = sh$note, method = sh$method %||% "fallback")
  })

  # ==========================================
  # v13：Bear / Base / Bull 情境 + 主／副點 + 可信度
  # ==========================================
  .dcf_valuation_bundle <- function(wacc_pp_delta = 0, g_pp_delta = 0,
                                    near_g_mult = 1, cash_mult = 1, debt_mult = 1,
                                    fcf_mult = 1,
                                    wacc1_pp_delta = 0, wacc2_pp_delta = 0,
                                    wacc_override_pct = NULL,
                                    yr_stage1_override = NULL,
                                    years_override = NULL) {
    empty <- list(
      ok = FALSE, price = NA_real_, shares = NA_real_,
      pv_fcf = NA_real_, pv_tv = NA_real_, cash = NA_real_, debt = NA_real_,
      equity = NA_real_, ev = NA_real_
    )
    df_fcf <- tryCatch(fcf_results$df_fcf(), error = function(e) NULL)
    n_base <- suppressWarnings(as.numeric(input$years)[1])
    if (is.null(df_fcf) || !is.data.frame(df_fcf) || !is.finite(n_base) || nrow(df_fcf) < 1) {
      return(empty)
    }
    n_years <- if (!is.null(years_override) && is.finite(years_override)) {
      as.integer(max(1, round(years_override)))
    } else {
      as.integer(n_base)
    }
    future_fcfs <- extract_fcff_series(df_fcf)
    # 年數衝擊：截斷或以前一期 FCFF 延展
    if (length(future_fcfs) >= 1L) {
      if (n_years <= length(future_fcfs)) {
        future_fcfs <- future_fcfs[seq_len(n_years)]
      } else {
        pad_n <- n_years - length(future_fcfs)
        last <- tail(future_fcfs, 1)
        future_fcfs <- c(future_fcfs, rep(last, pad_n))
      }
    }
    future_fcfs <- future_fcfs * fcf_mult
    if (isTRUE(near_g_mult != 1) && length(future_fcfs) >= 2) {
      base0 <- future_fcfs[1]
      if (is.finite(base0) && base0 != 0) {
        scaled <- future_fcfs
        for (i in seq_along(scaled)) {
          w <- i / length(scaled)
          scaled[i] <- base0 + (future_fcfs[i] - base0) * (1 + (near_g_mult - 1) * w)
        }
        future_fcfs <- scaled
      }
    }
    g_terminal <- if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) {
      as.numeric(input$sgr) / 100 + g_pp_delta / 100
    } else {
      APP_DEFAULTS$sgr / 100 + g_pp_delta / 100
    }
    if (identical(input$dcf_mode, "gordon")) {
      if (!is.null(wacc_override_pct) && is.finite(wacc_override_pct)) {
        r1 <- wacc_override_pct / 100
      } else {
        r1 <- as.numeric(input$wacc_gordon) / 100 + wacc_pp_delta / 100
      }
      r2 <- r1
      if (!is.finite(r2) || !is.finite(g_terminal) || g_terminal >= r2) return(empty)
      discount_factors <- cumprod(1 + rep(r1, n_years))
    } else {
      if (!is.null(wacc_override_pct) && is.finite(wacc_override_pct)) {
        r1 <- wacc_override_pct / 100 + wacc1_pp_delta / 100
        r2 <- wacc_override_pct / 100 + wacc2_pp_delta / 100
      } else {
        r1 <- as.numeric(input$wacc_stage1) / 100 + wacc_pp_delta / 100 + wacc1_pp_delta / 100
        r2 <- as.numeric(input$wacc_stage2) / 100 + wacc_pp_delta / 100 + wacc2_pp_delta / 100
      }
      if (!is.finite(r2) || !is.finite(g_terminal) || g_terminal >= r2) return(empty)
      yr1_src <- if (!is.null(yr_stage1_override) && is.finite(yr_stage1_override)) {
        yr_stage1_override
      } else {
        input$yr_stage1
      }
      yr1 <- clamp_yr_stage1(n_years, yr1_src, APP_DEFAULTS$yr_stage1)
      wacc_sequence <- c(rep(r1, min(yr1, n_years)), rep(r2, max(0, n_years - yr1)))
      discount_factors <- cumprod(1 + wacc_sequence)
    }
    pv_forecast <- sum(future_fcfs / discount_factors)
    last_fcf <- future_fcfs[n_years]
    tv <- (last_fcf * (1 + g_terminal)) / (r2 - g_terminal)
    pv_tv <- tv / discount_factors[n_years]
    dcf_value <- pv_forecast + pv_tv
    raw_cash <- tryCatch(
      select_current_metric(d_balance_sheet(), "Cash.*Equivalents.*Investments|Cash And Cash Equivalents|^Total Cash$", "stock"),
      error = function(e) NA_real_
    )
    latest_cash <- if (!is.null(input$manual_cash) && !is.na(input$manual_cash)) {
      input$manual_cash
    } else {
      ifelse(is.na(raw_cash), 0, raw_cash)
    }
    latest_cash <- latest_cash * cash_mult
    raw_total_debt <- tryCatch(select_current_metric(d_balance_sheet(), "^Total Debt$", "stock"), error = function(e) NA_real_)
    scraped_debt <- if (is.na(raw_total_debt)) 0 else raw_total_debt
    latest_debt <- if (!is.null(input$manual_debt) && !is.na(input$manual_debt)) {
      input$manual_debt
    } else {
      scraped_debt
    }
    latest_debt <- latest_debt * debt_mult
    equity_value <- as.numeric(dcf_value)[1] + latest_cash - latest_debt
    shares <- .valuation_shares()$shares
    if (!is.finite(equity_value) || !is.finite(shares) || shares <= 0) return(empty)
    list(
      ok = TRUE,
      price = equity_value / shares,
      shares = shares,
      pv_fcf = pv_forecast,
      pv_tv = pv_tv,
      cash = latest_cash,
      debt = latest_debt,
      equity = equity_value,
      ev = as.numeric(dcf_value)[1]
    )
  }

  .dcf_price_at <- function(wacc_pp_delta = 0, g_pp_delta = 0, near_g_mult = 1) {
    b <- .dcf_valuation_bundle(
      wacc_pp_delta = wacc_pp_delta,
      g_pp_delta = g_pp_delta,
      near_g_mult = near_g_mult
    )
    if (!isTRUE(b$ok)) return(NA_real_)
    b$price
  }

  .ddm_price_at <- function(ke_pp_delta = 0, g_pp_delta = 0) {
    d0 <- suppressWarnings(as.numeric(input[["mod_ddm-d0"]])[1])
    g0 <- suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1])
    ke0 <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
    if (!is.finite(d0) || d0 <= 0) return(NA_real_)
    if (!is.finite(g0)) g0 <- if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) as.numeric(input$sgr) else APP_DEFAULTS$sgr
    if (!is.finite(ke0)) ke0 <- central_ke() * 100
    g <- (g0 + g_pp_delta) / 100
    ke <- (ke0 + ke_pp_delta) / 100
    if (!is.finite(ke) || !is.finite(g) || ke <= g) return(NA_real_)
    d0 * (1 + g) / (ke - g)
  }

  .ri_price_at <- function(ke_pp_delta = 0, g_pp_delta = 0, roe_pp_delta = 0) {
    if (!exists("compute_ri_valuation", mode = "function")) return(NA_real_)
    b0 <- suppressWarnings(as.numeric(input[["mod_ri-b0"]])[1])
    ke0 <- suppressWarnings(as.numeric(input[["mod_ri-ri_ke"]])[1])
    g0 <- suppressWarnings(as.numeric(input[["mod_ri-ri_g"]])[1])
    n <- suppressWarnings(as.integer(input[["mod_ri-ri_years"]])[1])
    payout <- suppressWarnings(as.numeric(input[["mod_ri-ri_payout"]])[1])
    roe0 <- suppressWarnings(as.numeric(input[["mod_ri-ri_roe"]])[1])
    if (!is.finite(b0) || !is.finite(ke0) || !is.finite(g0) || !is.finite(n)) return(NA_real_)
    if (!is.finite(payout)) payout <- 0
    if (!is.finite(roe0)) roe0 <- 12
    ke <- (ke0 + ke_pp_delta) / 100
    g <- (g0 + g_pp_delta) / 100
    roe <- (roe0 + roe_pp_delta) / 100
    if (!is.finite(ke) || !is.finite(g) || ke <= g) return(NA_real_)
    res <- tryCatch(
      compute_ri_valuation(
        b0 = b0, ke = ke, g = g, n = max(1L, n),
        payout = payout / 100,
        roe_path = rep(roe, max(1L, n)),
        validate = TRUE
      ),
      error = function(e) NULL
    )
    if (is.null(res) || !identical(res$status, "success")) return(NA_real_)
    suppressWarnings(as.numeric(res$intrinsic)[1])
  }

  dcf_scenario_band <- reactive({
    sf <- scenario_stress_factors("dcf")
    base <- .dcf_price_at(0, 0, 1)
    if (!is.finite(base)) {
      base <- suppressWarnings(as.numeric(stock_price_estimate_val())[1])
    }
    bear <- .dcf_price_at(sf$bear$wacc_pp, sf$bear$g_pp, sf$bear$near_g_mult)
    bull <- .dcf_price_at(sf$bull$wacc_pp, sf$bull$g_pp, sf$bull$near_g_mult)
    list(bear = bear, base = base, bull = bull, label = "DCF")
  })

  ddm_scenario_band <- reactive({
    sf <- scenario_stress_factors("ddm")
    base <- .ddm_price_at(0, 0)
    if (!is.finite(base)) {
      base <- tryCatch(ddm_results$ddm_price(), error = function(e) NA_real_)
    }
    list(
      bear = .ddm_price_at(sf$bear$ke_pp, sf$bear$g_pp),
      base = base,
      bull = .ddm_price_at(sf$bull$ke_pp, sf$bull$g_pp),
      label = "DDM"
    )
  })

  ri_scenario_band <- reactive({
    sf <- scenario_stress_factors("ri")
    base <- .ri_price_at(0, 0, 0)
    if (!is.finite(base)) {
      base <- tryCatch(ri_results$ri_price(), error = function(e) NA_real_)
    }
    list(
      bear = .ri_price_at(sf$bear$ke_pp, sf$bear$g_pp, sf$bear$roe_pp),
      base = base,
      bull = .ri_price_at(sf$bull$ke_pp, sf$bull$g_pp, sf$bull$roe_pp),
      label = "RI"
    )
  })

  pb_scenario_band <- reactive({
    band <- tryCatch(pb_results$pb_band(), error = function(e) NULL)
    if (!is.null(band) && is.finite(band$mid)) {
      return(list(bear = band$low, base = band$mid, bull = band$high, label = "P/B"))
    }
    mid <- tryCatch(pb_results$pb_price(), error = function(e) NA_real_)
    list(bear = NA_real_, base = mid, bull = NA_real_, label = "P/B")
  })

  .model_point <- function(key) {
    switch(
      as.character(key %||% ""),
      "dcf" = {
        b <- tryCatch(dcf_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else suppressWarnings(as.numeric(stock_price_estimate_val())[1])
      },
      "ddm" = {
        b <- tryCatch(ddm_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else tryCatch(ddm_results$ddm_price(), error = function(e) NA_real_)
      },
      "pb" = {
        b <- tryCatch(pb_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else tryCatch(pb_results$pb_price(), error = function(e) NA_real_)
      },
      "ri" = {
        b <- tryCatch(ri_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else tryCatch(ri_results$ri_price(), error = function(e) NA_real_)
      },
      NA_real_
    )
  }

  primary_valuation_band <- reactive({
    rec <- model_sidebar_rec()
    prim <- as.character(rec$primary %||% "")
    band <- switch(
      prim,
      "dcf" = dcf_scenario_band(),
      "ddm" = ddm_scenario_band(),
      "pb" = pb_scenario_band(),
      "ri" = ri_scenario_band(),
      dcf_scenario_band()
    )
    if (is.null(band)) band <- list(bear = NA_real_, base = NA_real_, bull = NA_real_, label = .model_label(prim))
    band$label <- .model_label(prim)
    # ensure ordered when all present
    if (is.finite(band$bear) && is.finite(band$base) && is.finite(band$bull)) {
      xs <- sort(c(band$bear, band$base, band$bull))
      band$bear <- xs[1]; band$base <- xs[2]; band$bull <- xs[3]
    }
    band
  })

  secondary_valuation_point <- reactive({
    rec <- model_sidebar_rec()
    sec <- as.character(rec$secondary %||% "")
    if (!nzchar(sec)) return(NA_real_)
    .model_point(sec)
  })

  valuation_confidence <- reactive({
    rec <- model_sidebar_rec()
    band <- tryCatch(primary_valuation_band(), error = function(e) NULL)
    sec_pt <- tryCatch(secondary_valuation_point(), error = function(e) NA_real_)
    # F-Score light proxy: OCF > NI quality if available
    f_score <- tryCatch({
      ni <- select_current_metric_any(d_income_statement(), NET_INCOME_PATTERNS, "flow")
      ocf <- select_current_metric(d_cash_flow(), "Operating Cash Flow", "flow")
      assets <- select_current_metric(d_balance_sheet(), "Total Assets", "stock")
      s <- 0
      if (is.finite(ni) && is.finite(assets) && assets > 0 && ni / assets > 0) s <- s + 1
      if (is.finite(ocf) && ocf > 0) s <- s + 1
      if (is.finite(ocf) && is.finite(ni) && ocf > ni) s <- s + 1
      # scale 0–3 → approximate 0–9 for scorer thresholds
      s * 3
    }, error = function(e) NA_real_)
    tv_weight <- tryCatch({
      df_fcf <- fcf_results$df_fcf()
      n_years <- as.numeric(input$years)
      if (is.null(df_fcf) || nrow(df_fcf) != n_years) return(NA_real_)
      future_fcfs <- extract_fcff_series(df_fcf)
      r2 <- if (identical(input$dcf_mode, "gordon")) {
        as.numeric(input$wacc_gordon) / 100
      } else {
        as.numeric(input$wacc_stage2) / 100
      }
      g <- as.numeric(input$sgr) / 100
      if (!is.finite(r2) || !is.finite(g) || r2 <= g) return(NA_real_)
      dfs <- cumprod(1 + rep(r2, n_years))
      pv_fcf <- sum(future_fcfs / dfs)
      tv <- (tail(future_fcfs, 1) * (1 + g)) / (r2 - g) / dfs[n_years]
      tv / (pv_fcf + tv)
    }, error = function(e) NA_real_)
    score_valuation_confidence(
      confidence_inputs = rec$confidence_inputs %||% list(),
      f_score = f_score,
      primary_base = if (!is.null(band)) band$base else NA_real_,
      secondary_point = sec_pt,
      tv_weight = tv_weight
    )
  })
  
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

  # ---------- Beta (β) 分頁：Rolling 估計 vs Summary／產業 ----------
  beta_est_result <- reactiveVal(NULL)  # list(beta, n_obs, method, rs, rm, dates, bench, lookback)
  .beta_price_cache <- new.env(parent = emptyenv())

  .fetch_beta_prices <- function(ticker, period = "5y") {
    tk <- toupper(trimws(as.character(ticker)[1]))
    if (!nzchar(tk)) return(NULL)
    key <- paste0(tk, "|", period)
    if (exists(key, envir = .beta_price_cache, inherits = FALSE)) {
      return(get(key, envir = .beta_price_cache, inherits = FALSE))
    }
    df <- tryCatch(fetch_price_history_df(tk, period), error = function(e) NULL)
    if (!is.null(df) && is.data.frame(df) && nrow(df) >= 40) {
      assign(key, df, envir = .beta_price_cache)
    }
    df
  }

  .estimate_session_beta <- function() {
    req(current_ticker())
    bench <- toupper(trimws(as.character(input$beta_bench %||% "SPY")[1]))
    if (!nzchar(bench)) bench <- "SPY"
    lookback <- as.integer(suppressWarnings(as.numeric(input$beta_lookback_months)[1]))
    if (!is.finite(lookback) || lookback < 24L) lookback <- 60L
    min_obs <- as.integer(suppressWarnings(as.numeric(input$beta_min_obs)[1]))
    if (!is.finite(min_obs) || min_obs < 12L) min_obs <- 24L

    stk <- .fetch_beta_prices(current_ticker(), "5y")
    mkt <- .fetch_beta_prices(bench, "5y")
    if (is.null(stk) || is.null(mkt) || nrow(stk) < 40 || nrow(mkt) < 40) {
      return(list(ok = FALSE, reason = "無法取得足夠的股價／基準指數歷史（需約 5 年）。"))
    }
    # Align by date
    merged <- merge(
      data.frame(Date = stk$Date, S = stk$Close),
      data.frame(Date = mkt$Date, M = mkt$Close),
      by = "Date", all = FALSE
    )
    merged <- merged[order(merged$Date), , drop = FALSE]
    if (nrow(merged) < 40) {
      return(list(ok = FALSE, reason = "標的與基準交易日對齊後樣本不足。"))
    }
    as_of <- max(merged$Date, na.rm = TRUE)
    beta <- estimate_rolling_beta(
      merged$S, merged$M, merged$Date, as_of,
      lookback_months = lookback, min_obs = min_obs
    )
    if (!is.finite(beta)) {
      return(list(ok = FALSE, reason = "Rolling β 估計失敗（變異過低或觀測不足）。"))
    }

    # Month-end returns for scatter (same preference as estimate_rolling_beta)
    ym <- format(merged$Date, "%Y-%m")
    mth <- merged[!duplicated(ym, fromLast = TRUE), , drop = FALSE]
    mth <- utils::tail(mth, lookback + 1L)
    method <- "月末報酬"
    if (nrow(mth) < min_obs + 1L) {
      yw <- format(merged$Date, "%Y-%W")
      mth <- merged[!duplicated(yw, fromLast = TRUE), , drop = FALSE]
      mth <- utils::tail(mth, max(lookback * 4L, 52L) + 1L)
      method <- "週報酬（月末樣本不足）"
    }
    rs <- diff(mth$S) / head(mth$S, -1)
    rm <- diff(mth$M) / head(mth$M, -1)
    fine <- is.finite(rs) & is.finite(rm)
    rs <- rs[fine]; rm <- rm[fine]
    list(
      ok = TRUE,
      beta = round(as.numeric(beta), 3),
      n_obs = length(rs),
      method = method,
      rs = rs,
      rm = rm,
      as_of = as_of,
      bench = bench,
      lookback = lookback
    )
  }

  observeEvent(input$calc_beta_est, {
    withProgress(message = "估計 Rolling β…", value = 0.3, {
      res <- tryCatch(.estimate_session_beta(), error = function(e) {
        list(ok = FALSE, reason = e$message)
      })
      incProgress(1)
    })
    beta_est_result(res)
    if (isTRUE(res$ok)) {
      showNotification(
        glue::glue("✅ Rolling β = {res$beta}（{res$method}，n={res$n_obs}，基準 {res$bench}）"),
        type = "message", duration = 6
      )
    } else {
      showNotification(paste0("❌ ", res$reason %||% "估計失敗"), type = "error", duration = 8)
    }
  })

  observeEvent(input$apply_beta_est, {
    res <- beta_est_result()
    if (is.null(res) || !isTRUE(res$ok) || !is.finite(res$beta)) {
      showNotification("請先成功估計 Rolling β，再套用至 CAPM。", type = "warning")
      return()
    }
    updateCheckboxInput(session, "use_industry_beta", value = FALSE)
    capm_beta_dirty(TRUE)
    .set_capm_beta(res$beta)
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = TRUE)
    showNotification(
      glue::glue("已套用 β={res$beta} 至 CAPM，並重估 rₑ／WACC。"),
      type = "message", duration = 5
    )
  })

  # 搜尋新標的後清掉舊估計（避免套用他股 β）
  observeEvent(current_ticker(), {
    beta_est_result(NULL)
  }, ignoreInit = TRUE)

  output$vbx_beta_summary <- renderValueBox({
    b <- .summary_beta_value()
    valueBox(
      if (is.finite(b)) round(b, 2) else "N/A",
      "Finance Summary β",
      icon = icon("file-invoice"),
      color = "green"
    )
  })
  output$vbx_beta_industry <- renderValueBox({
    b <- .industry_beta_value()
    valueBox(
      if (is.finite(b)) round(b, 2) else "N/A",
      "產業平均 β",
      icon = icon("industry"),
      color = "blue"
    )
  })
  output$vbx_beta_estimated <- renderValueBox({
    res <- beta_est_result()
    valueBox(
      if (!is.null(res) && isTRUE(res$ok)) res$beta else "—",
      "Rolling 估計 β",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })

  output$beta_est_result <- renderUI({
    res <- beta_est_result()
    if (is.null(res)) {
      return(tags$p(style = "color:#888;font-size:13px;", "尚未估計。搜尋標的後按「估計 Rolling β」。"))
    }
    if (!isTRUE(res$ok)) {
      return(tags$p(style = "color:#c0392b;", res$reason %||% "估計失敗"))
    }
    rf <- suppressWarnings(as.numeric(input$capm_rf)[1])
    rm <- suppressWarnings(as.numeric(input$capm_rm)[1])
    ke <- if (is.finite(rf) && is.finite(rm)) rf + res$beta * (rm - rf) else NA_real_
    HTML(glue::glue(
      "<div style='padding:10px;border-left:4px solid #f39c12;background:#fdf6e3;font-size:13px;'>
         <b>β = {res$beta}</b> · {res$method} · n={res$n_obs}<br/>
         基準 {res$bench} · 回溯 {res$lookback} 月 · as-of {res$as_of}<br/>
         若套用：Ke = Rf + β×(Rm−Rf) ≈ <b>{if (is.finite(ke)) sprintf('%.2f%%', ke) else 'N/A'}</b>
       </div>"
    ))
  })

  output$beta_sources_table <- renderTable({
    res <- beta_est_result()
    est <- if (!is.null(res) && isTRUE(res$ok)) res$beta else NA_real_
    cur <- suppressWarnings(as.numeric(input$capm_beta)[1])
    data.frame(
      來源 = c("Finance Summary (Yahoo 5Y Monthly)", "產業平均", "Rolling 估計", "目前 CAPM 使用中"),
      Beta = c(
        { b <- .summary_beta_value(); if (is.finite(b)) sprintf("%.3f", b) else "N/A" },
        { b <- .industry_beta_value(); if (is.finite(b)) sprintf("%.3f", b) else "N/A" },
        if (is.finite(est)) sprintf("%.3f", est) else "尚未估計",
        if (is.finite(cur)) sprintf("%.3f", cur) else "N/A"
      ),
      說明 = c(
        "Dashboard → Finance Summary",
        "Get Started 所選產業標準",
        if (!is.null(res) && isTRUE(res$ok)) paste0(res$method, " vs ", res$bench) else "按左側按鈕估計",
        if (isTRUE(input$use_industry_beta)) "產業平均模式" else "Summary／手動／Rolling 套用"
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  output$plt_beta_scatter <- renderPlot({
    res <- beta_est_result()
    if (is.null(res) || !isTRUE(res$ok) || length(res$rs) < 5) {
      plot.new()
      text(0.5, 0.5, "估計成功後顯示月／週報酬散佈與回歸線", cex = 1.1, col = "#888")
      return(invisible(NULL))
    }
    df <- data.frame(rm = res$rm * 100, rs = res$rs * 100)
    ggplot(df, aes(x = rm, y = rs)) +
      geom_point(color = "#3c8dbc", alpha = 0.75, size = 2.2) +
      geom_smooth(method = "lm", se = TRUE, color = "#e67e22", fill = "#fdebd0", size = 1) +
      geom_hline(yintercept = 0, color = "#bbb") +
      geom_vline(xintercept = 0, color = "#bbb") +
      theme_minimal(base_size = 13) +
      labs(
        title = glue::glue("報酬回歸：β ≈ {res$beta}（{res$method}）"),
        x = paste0(res$bench, " 報酬 (%)"),
        y = paste0(current_ticker(), " 報酬 (%)")
      )
  })
  
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
  
  estimated_g_meta <- reactiveValues(method = NULL, fund_res = NULL, source = NULL)
  .clamp_near_term_g_pct <- function(g, lo = -5, hi = 25) {
    g <- suppressWarnings(as.numeric(g)[1])
    if (!is.finite(g)) return(NA_real_)
    max(lo, min(hi, g))
  }
  .yoy_rates_newest_first <- function(vec, abs_cap = 1) {
    # vec: newest → oldest. Return chronological YoY rates in (-abs_cap, abs_cap).
    x <- suppressWarnings(as.numeric(vec))
    x <- x[is.finite(x)]
    if (length(x) < 2L) return(numeric(0))
    chrono <- rev(x)
    rates <- diff(chrono) / abs(head(chrono, -1))
    rates <- rates[is.finite(rates)]
    rates[rates > -abs_cap & rates < abs_cap]
  }
  .series_cagr_pct_newest_first <- function(vec) {
    x <- suppressWarnings(as.numeric(vec))
    x <- x[is.finite(x)]
    if (length(x) < 2L) return(NA_real_)
    chrono <- rev(x)
    a <- head(chrono, 1); b <- tail(chrono, 1)
    n <- length(chrono) - 1L
    if (is.finite(a) && is.finite(b) && a > 0 && b > 0 && n > 0) {
      return(((b / a)^(1 / n) - 1) * 100)
    }
    rates <- .yoy_rates_newest_first(vec, abs_cap = 1)
    if (!length(rates)) return(NA_real_)
    mean(rates) * 100
  }
  observe({
    req(d_cash_flow(), d_income_statement(), d_balance_sheet(), input$g_growth_method)
    method <- input$g_growth_method
    if (is.null(method)) return()

    # Projection grows REVENUE then derives FCFF — historical methods must use revenue,
    # not noisy FCF YoY (except fundamental RR×ROIC which is a sustainable-g proxy).
    vec_rev <- tryCatch(
      select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE),
      error = function(e) numeric(0)
    )
    vec_fcf <- tryCatch(
      select_clean_metric_row(d_cash_flow(), "Free Cash Flow", include_ttm = FALSE),
      error = function(e) numeric(0)
    )

    fund_res <- NULL
    source_lab <- NULL
    if (isTRUE(method == "fundamental")) {
      ebit <- select_current_metric(d_income_statement(), "Operating Income|EBIT", "flow")
      tax_rate <- if (!is.null(input$wacc_tax)) input$wacc_tax / 100 else APP_DEFAULTS$wacc_tax / 100
      if (!is.finite(tax_rate)) tax_rate <- 0.21
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
      roic <- if (!is.na(invested_capital) && invested_capital > 0) nopat / invested_capital else 0

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
      # RR outside [0,1] is usually accounting noise for a one-year snapshot
      reinvestment_rate <- max(-0.2, min(1.2, reinvestment_rate))

      raw_fund_g <- reinvestment_rate * roic

      ceiling_ns <- input[["mod_fcf-apply_g_ceiling"]]
      apply_ceiling <- if (!is.null(ceiling_ns)) isTRUE(ceiling_ns) else TRUE

      if (apply_ceiling) {
        final_fund_g <- max(-0.05, min(raw_fund_g, 0.25))
      } else {
        final_fund_g <- max(-0.05, min(raw_fund_g, 0.50))
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
      source_lab <- "RR×ROIC"
    }

    val_raw <- switch(
      method,
      "fundamental" = if (!is.null(fund_res)) fund_res$g else NA_real_,
      "cagr" = .series_cagr_pct_newest_first(vec_rev),
      "mean" = {
        rates <- .yoy_rates_newest_first(vec_rev, abs_cap = 1)
        if (length(rates)) mean(rates) * 100 else NA_real_
      },
      "median" = {
        rates <- .yoy_rates_newest_first(vec_rev, abs_cap = 1)
        if (length(rates)) stats::median(rates) * 100 else NA_real_
      },
      "last_year" = {
        if (length(vec_rev) >= 2L && is.finite(vec_rev[1]) && is.finite(vec_rev[2]) && vec_rev[2] != 0) {
          ((vec_rev[1] - vec_rev[2]) / abs(vec_rev[2])) * 100
        } else NA_real_
      },
      "custom" = suppressWarnings(as.numeric(input$custom_g)[1]),
      NA_real_
    )

    # Near-term growth fed into revenue projection: clamp all non-custom methods.
    val <- if (identical(method, "custom")) {
      suppressWarnings(as.numeric(val_raw)[1])
    } else {
      .clamp_near_term_g_pct(val_raw, lo = -5, hi = 25)
    }
    if (is.null(source_lab)) {
      source_lab <- switch(
        method,
        "cagr" = "營收 CAGR",
        "mean" = "營收 YoY 平均",
        "median" = "營收 YoY 中位",
        "last_year" = "營收最近一年",
        "custom" = "自訂",
        method
      )
    }

    if (is.null(val) || length(val) < 1 || is.na(val) || !is.finite(val)) {
      # Fallback: if revenue path fails but FCF exists, try mild FCF mean (still clamped)
      if (!identical(method, "custom") && !identical(method, "fundamental")) {
        fcf_rates <- .yoy_rates_newest_first(vec_fcf, abs_cap = 1)
        if (length(fcf_rates)) {
          val <- .clamp_near_term_g_pct(mean(fcf_rates) * 100)
          source_lab <- paste0(source_lab, "（營收不足→FCF 回退）")
        }
      }
    }

    if (is.null(val) || length(val) < 1 || is.na(val) || !is.finite(val)) {
      prev_g_na <- isolate(estimated_g())
      estimated_g(NULL)
      estimated_g_meta$source <- NULL
      if (!is.null(prev_g_na)) {
        updateSelectInput(session, "g_growth_method", label = "預估營收成長率 (缺乏數據)")
      }
      return()
    }

    val <- round(as.numeric(val), 2)
    prev_g <- isolate(estimated_g())
    prev_method <- isolate(estimated_g_meta$method)
    estimated_g(val)
    estimated_g_meta$method <- method
    estimated_g_meta$fund_res <- fund_res
    estimated_g_meta$source <- source_lab
    changed <- !identical(prev_g, val) || !identical(prev_method, method)
    if (isTRUE(changed)) {
      updateSelectInput(session, "g_growth_method",
                        label = paste0("預估營收成長率 ➔ ", val, " %"))
    }

    if (method != "custom" && !is.na(val) && !identical(input$dcf_mode, "two_stage")) {
      if (is.null(input$g_stage1) || is.na(as.numeric(input$g_stage1)) ||
          abs(as.numeric(input$g_stage1) - as.numeric(val)) > 1e-4) {
        updateNumericInput(session, "g_stage1", value = val)
      }
    }
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
    } else if (method %in% c("cagr", "mean", "median", "last_year")) {
      src <- estimated_g_meta$source %||% "營收"
      HTML(glue::glue(
        "<div style='padding: 10px; border-left: 4px solid #3c8dbc; font-size: 13px; color: #555;'>
           以<strong>營收</strong>歷史計算近中期成長（{src}），再驅動營收→FCFF 預測。
           已套用 −5%～25% 防呆，避免把單年暴衝／暴跌寫進模型。
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
      "fundamental" = "基本面 RR×ROIC",
      "cagr" = "營收 CAGR",
      "mean" = "營收平均",
      "median" = "營收中位",
      "last_year" = "營收最近一年",
      "custom" = "自訂營收",
      method
    )
    infoBox(
      paste0("預估營收成長率 (", method_lab, ")"),
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
      geom_text(aes(y = FCFF, label = format_dollar_abbr(FCFF)),
                vjust = ifelse(df$FCFF >= 0, -0.5, 1.5), size = 4, fontface = "bold") +
      scale_y_continuous(labels = label_chart_number(prefix = "$")) +
      theme_minimal() +
      labs(title = "FCFF 與 營業利潤 成長軌跡", x = "預測年份", y = "金額 (百萬)") +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        legend.position = "top"
      )
  })

  # DCF 頁底部：所有可設定參數對每股估值的影響力%（相對 ±10%）
  output$dcf_param_sensitivity_table <- renderTable({
    base <- .dcf_valuation_bundle()
    validate(need(isTRUE(base$ok), "基準估值尚未就緒：請先完成 FCFF 預測並確保 g < WACC。"))
    p0 <- base$price
    shock_pct <- 0.10

    .rel <- function(x, sign = -1) {
      x <- suppressWarnings(as.numeric(x)[1])
      if (!is.finite(x) || abs(x) < 1e-12) return(NA_real_)
      x * (1 + sign * shock_pct)
    }
    .infl_row <- function(param, base_val, unit, b_down, b_up, note = "") {
      pd <- if (isTRUE(b_down$ok)) b_down$price else NA_real_
      pu <- if (isTRUE(b_up$ok)) b_up$price else NA_real_
      d_dn <- if (is.finite(pd) && is.finite(p0) && abs(p0) > 1e-9) 100 * (pd - p0) / abs(p0) else NA_real_
      d_up <- if (is.finite(pu) && is.finite(p0) && abs(p0) > 1e-9) 100 * (pu - p0) / abs(p0) else NA_real_
      infl <- mean(c(abs(d_dn), abs(d_up)), na.rm = TRUE)
      if (!is.finite(infl)) infl <- NA_real_
      data.frame(
        參數 = param,
        基準值 = if (identical(unit, "%")) sprintf("%.2f%%", base_val) else
          if (identical(unit, "$")) format_dollar_abbr(base_val) else
            if (identical(unit, "x")) sprintf("%.2f", base_val) else as.character(base_val),
        衝擊 = "±10%",
        `估值Δ% (−10%)` = if (is.finite(d_dn)) sprintf("%+.1f%%", d_dn) else "N/A",
        `估值Δ% (+10%)` = if (is.finite(d_up)) sprintf("%+.1f%%", d_up) else "N/A",
        `影響力%` = if (is.finite(infl)) sprintf("%.1f%%", infl) else "N/A",
        說明 = note,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    # 資本結構權重（供 CAPM／Rd／Tax → WACC 衝擊）
    .wacc_from_components <- function(rf_pct = NULL, beta = NULL, rm_pct = NULL,
                                      re_pct = NULL, rd_pct = NULL, tax_pct = NULL) {
      rf <- if (!is.null(rf_pct)) rf_pct else suppressWarnings(as.numeric(input$capm_rf)[1])
      b <- if (!is.null(beta)) beta else suppressWarnings(as.numeric(input$capm_beta)[1])
      rm <- if (!is.null(rm_pct)) rm_pct else suppressWarnings(as.numeric(input$capm_rm)[1])
      rd <- if (!is.null(rd_pct)) rd_pct else suppressWarnings(as.numeric(input$wacc_rd)[1])
      tax <- if (!is.null(tax_pct)) tax_pct else suppressWarnings(as.numeric(input$wacc_tax)[1])
      re <- if (!is.null(re_pct)) {
        re_pct
      } else if (isTRUE(input$use_estimated_re) && is.finite(rf) && is.finite(b) && is.finite(rm)) {
        rf + b * (rm - rf)
      } else {
        suppressWarnings(as.numeric(input$wacc_re)[1])
      }
      eq <- tryCatch({
        sh <- .valuation_shares()$shares
        px <- tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
        if (is.finite(sh) && is.finite(px)) sh * px else NA_real_
      }, error = function(e) NA_real_)
      debt <- base$debt
      if (!is.finite(eq) || eq <= 0) {
        # fallback：用目前 WACC 輸入
        return(if (identical(input$dcf_mode, "gordon")) {
          suppressWarnings(as.numeric(input$wacc_gordon)[1])
        } else {
          suppressWarnings(as.numeric(input$wacc_stage1)[1])
        })
      }
      if (!is.finite(debt) || debt < 0) debt <- 0
      tot <- eq + debt
      if (!is.finite(tot) || tot <= 0) return(NA_real_)
      if (!is.finite(re) || !is.finite(rd) || !is.finite(tax)) return(NA_real_)
      (eq / tot) * re + (debt / tot) * rd * (1 - tax / 100)
    }

    rows <- list()
    n0 <- suppressWarnings(as.numeric(input$years)[1])
    if (is.finite(n0) && n0 >= 1) {
      rows[[length(rows) + 1]] <- .infl_row(
        "預測年數 n", n0, "n",
        .dcf_valuation_bundle(years_override = max(1, round(n0 * 0.9))),
        .dcf_valuation_bundle(years_override = max(1, round(n0 * 1.1))),
        "截斷／延展 FCFF 序列"
      )
    }

    g_near <- suppressWarnings(as.numeric(estimated_g())[1])
    if (!is.finite(g_near)) g_near <- suppressWarnings(as.numeric(input$custom_g)[1])
    if (is.finite(g_near)) {
      # 近中期成長：以軌跡倍率近似 ±10% 成長衝擊
      rows[[length(rows) + 1]] <- .infl_row(
        "近中期營收成長率", g_near, "%",
        .dcf_valuation_bundle(near_g_mult = 0.9),
        .dcf_valuation_bundle(near_g_mult = 1.1),
        "透過 FCFF 軌跡倍率近似"
      )
    }

    g0 <- suppressWarnings(as.numeric(input$sgr)[1])
    if (is.finite(g0)) {
      rows[[length(rows) + 1]] <- .infl_row(
        "終值成長率 SGR (g)", g0, "%",
        .dcf_valuation_bundle(g_pp_delta = .rel(g0, -1) - g0),
        .dcf_valuation_bundle(g_pp_delta = .rel(g0, +1) - g0),
        "Gordon／終值 TV"
      )
    }

    if (identical(input$dcf_mode, "gordon")) {
      w0 <- suppressWarnings(as.numeric(input$wacc_gordon)[1])
      if (is.finite(w0)) {
        rows[[length(rows) + 1]] <- .infl_row(
          "折現率 WACC", w0, "%",
          .dcf_valuation_bundle(wacc_pp_delta = .rel(w0, -1) - w0),
          .dcf_valuation_bundle(wacc_pp_delta = .rel(w0, +1) - w0),
          "明確預測 + Gordon"
        )
      }
    } else {
      w1 <- suppressWarnings(as.numeric(input$wacc_stage1)[1])
      w2 <- suppressWarnings(as.numeric(input$wacc_stage2)[1])
      g1 <- suppressWarnings(as.numeric(input$g_stage1)[1])
      yr1 <- suppressWarnings(as.numeric(input$yr_stage1)[1])
      if (is.finite(w1)) {
        rows[[length(rows) + 1]] <- .infl_row(
          "折現率 WACC1", w1, "%",
          .dcf_valuation_bundle(wacc1_pp_delta = .rel(w1, -1) - w1),
          .dcf_valuation_bundle(wacc1_pp_delta = .rel(w1, +1) - w1),
          "兩階段｜高速期"
        )
      }
      if (is.finite(w2)) {
        rows[[length(rows) + 1]] <- .infl_row(
          "折現率 WACC2", w2, "%",
          .dcf_valuation_bundle(wacc2_pp_delta = .rel(w2, -1) - w2),
          .dcf_valuation_bundle(wacc2_pp_delta = .rel(w2, +1) - w2),
          "兩階段｜終值折現"
        )
      }
      if (is.finite(g1)) {
        rows[[length(rows) + 1]] <- .infl_row(
          "高速成長率 g1", g1, "%",
          .dcf_valuation_bundle(near_g_mult = 0.9),
          .dcf_valuation_bundle(near_g_mult = 1.1),
          "以 FCFF 軌跡近似 g1 衝擊"
        )
      }
      if (is.finite(yr1) && is.finite(n0)) {
        rows[[length(rows) + 1]] <- .infl_row(
          "第一階段年數", yr1, "n",
          .dcf_valuation_bundle(yr_stage1_override = max(1, round(yr1 * 0.9))),
          .dcf_valuation_bundle(yr_stage1_override = max(1, min(n0 - 1, round(yr1 * 1.1)))),
          "兩階段分界"
        )
      }
    }

    rows[[length(rows) + 1]] <- .infl_row(
      "FCFF 水準（整體）", 1, "x",
      .dcf_valuation_bundle(fcf_mult = 0.9),
      .dcf_valuation_bundle(fcf_mult = 1.1),
      "涵蓋營收／NOPAT／再投資設定的綜合效果"
    )

    # FCFF 分頁前瞻比率（若有填）
    capex_r <- suppressWarnings(as.numeric(input[["mod_fcf-proj_capex_rate"]])[1])
    nwc_r <- suppressWarnings(as.numeric(input[["mod_fcf-proj_nwc_rate"]])[1])
    if (is.finite(capex_r)) {
      # CapEx 上升通常降低 FCFF：以反向 fcf 衝擊近似
      rows[[length(rows) + 1]] <- .infl_row(
        "CapEx / Revenue", capex_r, "%",
        .dcf_valuation_bundle(fcf_mult = 1.1),
        .dcf_valuation_bundle(fcf_mult = 0.9),
        "近似：CapEx↑ → FCFF↓"
      )
    }
    if (is.finite(nwc_r)) {
      rows[[length(rows) + 1]] <- .infl_row(
        "ΔNWC / ΔRevenue", nwc_r, "%",
        .dcf_valuation_bundle(fcf_mult = 1.1),
        .dcf_valuation_bundle(fcf_mult = 0.9),
        "近似：ΔNWC↑ → FCFF↓"
      )
    }

    if (is.finite(base$cash) && abs(base$cash) > 1) {
      rows[[length(rows) + 1]] <- .infl_row(
        "現金／約當現金", base$cash, "$",
        .dcf_valuation_bundle(cash_mult = 0.9),
        .dcf_valuation_bundle(cash_mult = 1.1),
        "EV → Equity 橋接"
      )
    }
    if (is.finite(base$debt) && abs(base$debt) > 1) {
      rows[[length(rows) + 1]] <- .infl_row(
        "總負債", base$debt, "$",
        .dcf_valuation_bundle(debt_mult = 0.9),
        .dcf_valuation_bundle(debt_mult = 1.1),
        "EV → Equity 橋接"
      )
    }

    # CAPM／WACC 組成
    rf0 <- suppressWarnings(as.numeric(input$capm_rf)[1])
    beta0 <- suppressWarnings(as.numeric(input$capm_beta)[1])
    rm0 <- suppressWarnings(as.numeric(input$capm_rm)[1])
    rd0 <- suppressWarnings(as.numeric(input$wacc_rd)[1])
    tax0 <- suppressWarnings(as.numeric(input$wacc_tax)[1])
    re0 <- suppressWarnings(as.numeric(input$wacc_re)[1])

    if (is.finite(rf0)) {
      w_dn <- .wacc_from_components(rf_pct = .rel(rf0, -1))
      w_up <- .wacc_from_components(rf_pct = .rel(rf0, +1))
      rows[[length(rows) + 1]] <- .infl_row(
        "無風險利率 Rf", rf0, "%",
        .dcf_valuation_bundle(wacc_override_pct = w_dn),
        .dcf_valuation_bundle(wacc_override_pct = w_up),
        "經 CAPM → WACC"
      )
    }
    if (is.finite(beta0)) {
      w_dn <- .wacc_from_components(beta = .rel(beta0, -1))
      w_up <- .wacc_from_components(beta = .rel(beta0, +1))
      rows[[length(rows) + 1]] <- .infl_row(
        "Beta (β)", beta0, "x",
        .dcf_valuation_bundle(wacc_override_pct = w_dn),
        .dcf_valuation_bundle(wacc_override_pct = w_up),
        "經 CAPM → WACC"
      )
    }
    if (is.finite(rm0)) {
      w_dn <- .wacc_from_components(rm_pct = .rel(rm0, -1))
      w_up <- .wacc_from_components(rm_pct = .rel(rm0, +1))
      rows[[length(rows) + 1]] <- .infl_row(
        "市場報酬率 Rm", rm0, "%",
        .dcf_valuation_bundle(wacc_override_pct = w_dn),
        .dcf_valuation_bundle(wacc_override_pct = w_up),
        "經 CAPM → WACC"
      )
    }
    if (is.finite(re0) && !isTRUE(input$use_estimated_re)) {
      w_dn <- .wacc_from_components(re_pct = .rel(re0, -1))
      w_up <- .wacc_from_components(re_pct = .rel(re0, +1))
      rows[[length(rows) + 1]] <- .infl_row(
        "股權成本 rₑ", re0, "%",
        .dcf_valuation_bundle(wacc_override_pct = w_dn),
        .dcf_valuation_bundle(wacc_override_pct = w_up),
        "手動 rₑ（未勾選 CAPM）"
      )
    }
    if (is.finite(rd0)) {
      w_dn <- .wacc_from_components(rd_pct = .rel(rd0, -1))
      w_up <- .wacc_from_components(rd_pct = .rel(rd0, +1))
      rows[[length(rows) + 1]] <- .infl_row(
        "負債成本 rᵈ", rd0, "%",
        .dcf_valuation_bundle(wacc_override_pct = w_dn),
        .dcf_valuation_bundle(wacc_override_pct = w_up),
        "經 WACC 權重"
      )
    }
    if (is.finite(tax0)) {
      w_dn <- .wacc_from_components(tax_pct = .rel(tax0, -1))
      w_up <- .wacc_from_components(tax_pct = .rel(tax0, +1))
      rows[[length(rows) + 1]] <- .infl_row(
        "所得稅率 T", tax0, "%",
        .dcf_valuation_bundle(wacc_override_pct = w_dn),
        .dcf_valuation_bundle(wacc_override_pct = w_up),
        "稅盾：Rd×(1−T)"
      )
    }

    out <- do.call(rbind, rows)
    # 依影響力% 由高到低排序
    infl_num <- suppressWarnings(as.numeric(gsub("%", "", out$`影響力%`)))
    out <- out[order(-infl_num, na.last = TRUE), , drop = FALSE]
    rownames(out) <- NULL
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")
  
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
  # 📉 DCF Overview 圖：歷史／預測 FCFF 柱 + 折現疊線（可多選層級）
  # ==========================================
  output$plt_dcf_trajectory <- renderPlotly({
    req(fcf_results$df_fcf(), current_ticker())
    proj_df <- fcf_results$df_fcf()
    empty_plot <- function(msg) {
      plotly::plotly_empty() %>%
        plotly::layout(
          title = list(text = msg, x = 0.5),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        )
    }
    if (is.null(proj_df) || nrow(proj_df) < 1) {
      return(empty_plot("⚠️ 財報數據不足，無法繪圖"))
    }

    layers <- input$dcf_chart_layers %||% APP_DEFAULTS$dcf_chart_layers
    if (length(layers) < 1) layers <- c("hist", "forecast")
    show_hist <- "hist" %in% layers
    show_fcst <- "forecast" %in% layers
    show_dcf <- "dcf" %in% layers
    show_pv <- "pv_fcff" %in% layers

    n_years <- nrow(proj_df)
    fcff_vals <- extract_fcff_series(proj_df)

    hist_df <- tryCatch({
      cf <- d_cash_flow()
      row_idx <- grep("^Free Cash Flow$|Free Cash Flow", cf[[1]], ignore.case = TRUE)
      if (length(row_idx) == 0) return(NULL)
      period_cols <- colnames(cf)[-1]
      period_cols <- period_cols[!grepl("^ttm$", period_cols, ignore.case = TRUE)]
      if (length(period_cols) == 0) return(NULL)
      vals <- parse_financial_number(as.character(cf[row_idx[1], period_cols, drop = FALSE]))
      ord <- rev(seq_along(period_cols))
      data.frame(
        Period = as.character(period_cols[ord]),
        Value = as.numeric(vals[ord]),
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
    if (!is.null(hist_df)) {
      hist_df <- hist_df[is.finite(hist_df$Value), , drop = FALSE]
    }

    forecast_periods <- as.character(proj_df$Year)
    if (length(forecast_periods) == 0) forecast_periods <- paste0("Y", seq_len(n_years))

    wacc_val <- tryCatch({
      if (identical(input$dcf_mode, "gordon")) {
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
    pv_fcff_only <- round(fcff_vals / discount_factors, 2)
    dcf_vals <- pv_fcff_only
    g_terminal <- if (is.numeric(input$sgr)) input$sgr / 100 else 0.03
    terminal_wacc <- tail(wacc_val, 1)
    tv_note <- ""
    if (is.finite(terminal_wacc) && is.finite(g_terminal) && terminal_wacc > g_terminal) {
      last_fcf <- tail(fcff_vals, 1)
      tv <- (last_fcf * (1 + g_terminal)) / (terminal_wacc - g_terminal)
      pv_tv <- tv / discount_factors[n_years]
      dcf_vals[n_years] <- round(dcf_vals[n_years] + pv_tv, 2)
      tv_note <- paste0("末年 DCF 含終值 PV ", format_dollar_abbr(pv_tv))
    }

    x_levels <- unique(c(
      if (!is.null(hist_df) && nrow(hist_df) > 0) hist_df$Period else character(0),
      forecast_periods
    ))
    if (length(x_levels) < 1) return(empty_plot("⚠️ 無可繪製期間"))

    bar_parts <- list()
    if (isTRUE(show_hist) && !is.null(hist_df) && nrow(hist_df) > 0) {
      tmp <- hist_df
      tmp$Series <- "歷史 FCFF"
      bar_parts[[length(bar_parts) + 1]] <- tmp
    }
    if (isTRUE(show_fcst)) {
      bar_parts[[length(bar_parts) + 1]] <- data.frame(
        Period = forecast_periods,
        Value = as.numeric(fcff_vals),
        Series = "預測 FCFF",
        stringsAsFactors = FALSE
      )
    }
    line_parts <- list()
    if (isTRUE(show_pv)) {
      line_parts[[length(line_parts) + 1]] <- data.frame(
        Period = forecast_periods,
        Value = as.numeric(pv_fcff_only),
        Series = "逐年折現 PV(FCFF)",
        stringsAsFactors = FALSE
      )
    }
    if (isTRUE(show_dcf)) {
      line_parts[[length(line_parts) + 1]] <- data.frame(
        Period = forecast_periods,
        Value = as.numeric(dcf_vals),
        Series = "折現後價值 (含 TV)",
        stringsAsFactors = FALSE
      )
    }

    if (length(bar_parts) < 1 && length(line_parts) < 1) {
      return(empty_plot("請至少勾選一個疊圖層級"))
    }

    color_map <- c(
      "歷史 FCFF" = "#2E86AB",
      "預測 FCFF" = "#A8D5E5",
      "逐年折現 PV(FCFF)" = "#F39C12",
      "折現後價值 (含 TV)" = "#C0392B"
    )
    symbol_map <- c(
      "歷史 FCFF" = "square",
      "預測 FCFF" = "diamond",
      "逐年折現 PV(FCFF)" = "circle",
      "折現後價值 (含 TV)" = "triangle-up"
    )

    n_hist <- if (!is.null(hist_df) && nrow(hist_df) > 0) nrow(hist_df) else 0L
    shapes <- list()
    annotations <- list()
    if (n_hist > 0 && length(forecast_periods) > 0 && length(x_levels) > 0) {
      x0_paper <- n_hist / length(x_levels)
      shapes[[1]] <- list(
        type = "rect", xref = "paper", yref = "paper",
        x0 = x0_paper, x1 = 1, y0 = 0, y1 = 1,
        fillcolor = "rgba(168, 213, 229, 0.10)", line = list(width = 0)
      )
      shapes[[2]] <- list(
        type = "line", xref = "paper", yref = "paper",
        x0 = x0_paper, x1 = x0_paper, y0 = 0, y1 = 1,
        line = list(color = "#95A5A6", width = 1.5, dash = "dot")
      )
      annotations[[1]] <- list(
        x = x0_paper, y = 1.02, xref = "paper", yref = "paper",
        text = "歷史 | 預測", showarrow = FALSE, xanchor = "center",
        font = list(size = 11, color = "#7F8C8D")
      )
    }

    p <- plotly::plot_ly()
    for (dfb in bar_parts) {
      dfb$Period <- factor(as.character(dfb$Period), levels = x_levels)
      ser <- as.character(dfb$Series[1])
      p <- p %>% plotly::add_trace(
        data = dfb,
        x = ~Period, y = ~Value,
        type = "bar",
        name = ser,
        marker = list(color = unname(color_map[[ser]]), line = list(width = 0)),
        hovertemplate = paste0("<b>", ser, "</b><br>%{x}<br>$%{y:,.2f}<extra></extra>"),
        legendgroup = ser,
        showlegend = TRUE
      )
    }
    for (dfl in line_parts) {
      dfl$Period <- factor(as.character(dfl$Period), levels = x_levels)
      ser <- as.character(dfl$Series[1])
      is_tv <- identical(ser, "折現後價值 (含 TV)")
      p <- p %>% plotly::add_trace(
        data = dfl,
        x = ~Period, y = ~Value,
        type = "scatter", mode = "lines+markers",
        name = ser,
        line = list(
          color = unname(color_map[[ser]]),
          width = if (is_tv) 3 else 2.2,
          dash = if (is_tv) "dash" else "solid"
        ),
        marker = list(
          color = unname(color_map[[ser]]),
          size = 10,
          symbol = unname(symbol_map[[ser]]),
          line = list(color = "#FFFFFF", width = 1.2)
        ),
        hovertemplate = paste0("<b>", ser, "</b><br>%{x}<br>$%{y:,.2f}<extra></extra>"),
        legendgroup = ser,
        showlegend = TRUE
      )
    }

    subtitle <- if (nzchar(tv_note) && isTRUE(show_dcf)) tv_note else "柱＝現金流；線＝折現疊圖（圖例可點擊顯隱）"
    title_main <- paste0(current_ticker(), " · Cash Flow 折現軌跡")

    p %>%
      plotly::layout(
        barmode = "group",
        title = list(
          text = paste0(
            "<b>", htmltools::htmlEscape(title_main), "</b>",
            "<br><span style='font-size:12px;color:#8e44ad;'>",
            htmltools::htmlEscape(subtitle), "</span>"
          ),
          x = 0.02
        ),
        xaxis = list(
          title = "期間", tickangle = -30,
          categoryorder = "array", categoryarray = x_levels
        ),
        yaxis = list(
          title = "USD", tickprefix = "$",
          separatethousands = TRUE, zeroline = TRUE,
          gridcolor = "#EEF2F5"
        ),
        legend = list(
          orientation = "h",
          x = 0, y = 1.14,
          bgcolor = "rgba(255,255,255,0.92)",
          bordercolor = "#D5DBDB",
          borderwidth = 1,
          font = list(size = 12),
          itemsizing = "constant",
          itemwidth = 36,
          traceorder = "normal"
        ),
        shapes = shapes,
        annotations = annotations,
        margin = list(t = 90, b = 70, l = 70, r = 24),
        hovermode = "x unified",
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FAFBFC"
      ) %>%
      plotly::config(displayModeBar = TRUE, responsive = TRUE, displaylogo = FALSE)
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
      scale_y_continuous(labels = label_chart_number(prefix = "$")) +
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
    
    if (input$dcf_mode == "gordon") {
      req(input$sgr, input$wacc_gordon)
      r1 <- input$wacc_gordon / 100
      r2 <- r1 
      
      if (!is.na(r2) && g_terminal >= r2) { 
        showNotification("❌ 成長率 g 必須嚴格小於折現率 WACC", type = "error")
        return(NULL) 
      }
      discount_factors <- cumprod(1 + rep(r1, n))
      
    } else {
      req(input$g_stage1, input$sgr, input$yr_stage1, input$wacc_stage1, input$wacc_stage2)
      
      r1 <- input$wacc_stage1 / 100
      r2 <- input$wacc_stage2 / 100
      
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
    share_outstanding <- tryCatch({
      sh <- .valuation_shares()
      if (is.finite(sh$shares) && sh$shares > 0) sh$shares else ifelse(is.na(raw_shares) || raw_shares <= 0, 1, raw_shares)
    }, error = function(e) ifelse(is.na(raw_shares) || raw_shares <= 0, 1, raw_shares))
    
    # ==========================================
    # 🌟 執行橋接：企業價值 (EV) 轉 股權價值 (Equity Value)
    # ==========================================
    equity_value <- as.numeric(dcf_value)[1] + latest_cash - latest_debt
    
    # 計算每股目標價並防呆
    if (!is.na(equity_value) && share_outstanding > 1) {
      stock_price_estimate_val(equity_value / share_outstanding)
      sh_note <- tryCatch(.valuation_shares()$note, error = function(e) NULL)
      if (!is.null(sh_note) && nzchar(sh_note)) {
        showNotification(paste0("DCF 股數：", sh_note), type = "message", duration = 6)
      }
    } else {
      stock_price_estimate_val(NULL)
      # 如果股數回傳 1 (代表剛剛抓不到被我們設為預設值 1)，則跳出明確警告
      showNotification("⚠️ 警告：無法計算目標股價，未找到流通在外股數 (Shares Outstanding) 資料", type = "warning")
    }
    
    showNotification(
      glue::glue("✅ 估值更新：已成功將模組 FCFF 序列套入 DCF 運算引擎 (採用目前 WACC 輸入值)"),
      type = "message"
    )
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
    
    wacc_val <- if (isTRUE(input$dcf_mode == "gordon")) {
      paste0(input$wacc_gordon, "%")
    } else {
      paste0(input$wacc_stage1, "% / ", input$wacc_stage2, "%")
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
    prim <- as.character(rec$primary %||% "")
    if (identical(prim, "ddm")) return("DDM")
    if (identical(prim, "dcf")) return("DCF")
    # 副模型／其他主模型：DDM 旗標且無 DCF 時用 DDM，否則 DCF
    if (isTRUE(rec$ddm) && !isTRUE(rec$dcf)) return("DDM")
    "DCF"
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

    shares <- tryCatch({
      sh <- .valuation_shares()
      if (is.finite(sh$shares) && sh$shares > 0) sh$shares else 1
    }, error = function(e) {
      s <- select_current_metric(
        d_balance_sheet(),
        "Ordinary Shares Number|Share Issued|Total Shares Outstanding",
        "stock"
      )
      if (is.na(s) || s <= 0) 1 else s
    })
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

    base_wacc_seq <- if (identical(input$dcf_mode, "gordon")) {
      rep(base_wacc / 100, n_years)
    } else {
      s1 <- as.numeric(input$yr_stage1)
      # 敏感度以「目前 WACC」為軸心：Stage1／Stage2 皆相對目前 WACC 平移
      c(rep(base_wacc / 100, min(s1, n_years)), rep(base_wacc / 100, max(n_years - s1, 0)))
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
  # 🧪 Backtest Zone v12：PIT 多模型重建 + Alpha／MOS 驗證
  # ==========================================
  bt_param_notes_txt <- reactiveVal("請先搜尋股票並載入財報，系統會依公司自動推導參數。")
  bt_result <- reactiveVal(NULL)
  bt_validation <- reactiveVal(NULL)
  bt_run_msg <- reactiveVal("")
  bt_applying_params <- reactiveVal(FALSE)
  bt_fv_visible <- reactiveVal(FALSE)
  bt_hfv_fv <- reactiveVal(NULL)

  .bt_fv_model_specs <- function() {
    list(
      dcf = list(col = "FV_DCF", label = "DCF", color = "#c0392b"),
      ddm = list(col = "FV_DDM", label = "DDM", color = "#8e44ad"),
      ri  = list(col = "FV_RI",  label = "RI",  color = "#16a085"),
      pb  = list(col = "FV_PB",  label = "P/B", color = "#e67e22")
    )
  }

  .bt_raw_fv_models <- reactive({
    sel <- input$bt_fv_models
    if (is.null(sel) || length(sel) < 1) return(character(0))
    ord <- c("dcf", "ddm", "ri", "pb")
    intersect(ord, as.character(sel))
  })

  .bt_selected_fv_models <- reactive({
    hit <- .bt_raw_fv_models()
    # Backtest / primary fallback when none checked on HFV overlay
    if (length(hit) < 1) "dcf" else hit
  })

  .bt_primary_fv_model <- reactive({
    .bt_selected_fv_models()[1]
  })

  bt_hfv_base <- reactive({
    req(current_ticker())
    tryCatch(
      fetch_hfv_price_frame(current_ticker(), bench_ticker = "SPY", years = 5),
      error = function(e) NULL
    )
  })

  observeEvent(current_ticker(), {
    bt_fv_visible(FALSE)
    bt_hfv_fv(NULL)
    was_applying <- isTRUE(bt_applying_params())
    bt_applying_params(TRUE)
    updateCheckboxGroupInput(session, "bt_fv_models", selected = character(0))
    if (!was_applying) bt_applying_params(FALSE)
  }, ignoreInit = TRUE)

  bt_current_mos <- reactive({
    cur <- tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    tgt <- tryCatch(stock_price_estimate_val(), error = function(e) NA_real_)
    if (is.null(tgt) || length(tgt) < 1) tgt <- NA_real_
    cur <- suppressWarnings(as.numeric(cur)[1])
    tgt <- suppressWarnings(as.numeric(tgt)[1])
    if (is.na(cur) || is.na(tgt) || !is.finite(cur) || !is.finite(tgt) || tgt == 0) return(NA_real_)
    (tgt - cur) / tgt
  })

  # Session「此刻」模型參數（動態重建用；不落庫）
  # 歷史 PIT 的 Ke/WACC 會在再平衡日以 Rolling β 覆寫；此處提供 Rf/Rm／資本結構與定值 fallback。
  bt_current_model_params <- reactive({
    wacc <- if (identical(input$dcf_mode, "two_stage")) {
      suppressWarnings(as.numeric(input$wacc_stage1)[1]) / 100
    } else {
      suppressWarnings(as.numeric(input$wacc_gordon)[1]) / 100
    }
    if (!is.finite(wacc) || wacc <= 0) {
      wacc <- if (!is.null(calculated_wacc()) && is.finite(calculated_wacc())) {
        as.numeric(calculated_wacc())
      } else {
        APP_DEFAULTS$wacc_gordon / 100
      }
    }
    sgr <- suppressWarnings(as.numeric(input$sgr)[1]) / 100
    n_years <- suppressWarnings(as.integer(input$years)[1])
    if (is.na(n_years) || n_years < 1L) n_years <- 5L
    g_explicit <- if (identical(input$dcf_mode, "two_stage")) {
      suppressWarnings(as.numeric(input$g_stage1)[1]) / 100
    } else {
      cg <- suppressWarnings(as.numeric(input$custom_g)[1])
      if (is.finite(cg)) cg / 100 else sgr
    }
    ke <- tryCatch(as.numeric(central_ke())[1], error = function(e) NA_real_)
    if (!is.finite(ke) || ke <= 0) {
      ke <- suppressWarnings(as.numeric(input$wacc_re)[1]) / 100
    }
    if (!is.finite(ke) || ke <= 0) ke <- wacc

    # P/B tab (module id mod_pb)
    pb_mid <- suppressWarnings(as.numeric(input[["mod_pb-pb_mid"]])[1])
    if (!is.finite(pb_mid) || pb_mid <= 0) {
      pb_mid <- suppressWarnings(as.numeric(input$pb_mid)[1])
    }
    if (!is.finite(pb_mid) || pb_mid <= 0) pb_mid <- APP_DEFAULTS$pb_mid

    # DDM tab
    ddm_g <- suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1])
    if (!is.finite(ddm_g)) ddm_g <- sgr * 100
    ddm_g <- ddm_g / 100
    ddm_ke <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
    if (is.finite(ddm_ke) && ddm_ke > 0) {
      ddm_ke <- ddm_ke / 100
    } else {
      ddm_ke <- ke
    }

    # RI / DDM / P/B tabs: applied only to HFV chart tip (latest point)
    ri_g <- suppressWarnings(as.numeric(input[["mod_ri-ri_g"]])[1])
    if (is.finite(ri_g)) ri_g <- ri_g / 100 else ri_g <- g_explicit
    ri_ke <- suppressWarnings(as.numeric(input[["mod_ri-ri_ke"]])[1])
    if (is.finite(ri_ke) && ri_ke > 0) ri_ke <- ri_ke / 100 else ri_ke <- ke
    ri_years <- suppressWarnings(as.integer(input[["mod_ri-ri_years"]])[1])
    if (!is.finite(ri_years) || ri_years < 1L) ri_years <- n_years
    ri_roe <- suppressWarnings(as.numeric(input[["mod_ri-ri_roe"]])[1])
    if (is.finite(ri_roe)) ri_roe <- ri_roe / 100 else ri_roe <- NA_real_
    ri_payout <- suppressWarnings(as.numeric(input[["mod_ri-ri_payout"]])[1])
    if (is.finite(ri_payout)) ri_payout <- max(0, min(1, ri_payout / 100)) else ri_payout <- NA_real_
    roe_method <- as.character(input[["mod_ri-roe_method"]] %||% "constant")[1]
    roe_terminal <- suppressWarnings(as.numeric(input[["mod_ri-roe_terminal"]])[1])
    if (is.finite(roe_terminal)) roe_terminal <- roe_terminal / 100 else roe_terminal <- ri_roe
    roe_industry <- suppressWarnings(as.numeric(input[["mod_ri-roe_industry"]])[1])
    if (is.finite(roe_industry)) roe_industry <- roe_industry / 100 else roe_industry <- 0.12
    roe_custom_vec <- NULL
    if (identical(roe_method, "custom") && exists(".parse_roe_pct_vector", mode = "function")) {
      roe_custom_vec <- tryCatch(
        .parse_roe_pct_vector(input[["mod_ri-roe_custom_txt"]]),
        error = function(e) NULL
      )
    }

    if (!is.finite(wacc) || wacc <= 0) wacc <- APP_DEFAULTS$wacc_gordon / 100
    if (!is.finite(sgr)) sgr <- APP_DEFAULTS$sgr / 100
    if (!is.finite(g_explicit)) g_explicit <- sgr
    fv_model <- .bt_primary_fv_model()
    if (!fv_model %in% c("dcf", "ddm", "ri", "pb")) fv_model <- "dcf"

    rf <- suppressWarnings(as.numeric(input$capm_rf)[1]) / 100
    if (!is.finite(rf) || rf <= 0) {
      rf <- tryCatch(as.numeric(cached_get_risk_free_rate()) / 100, error = function(e) NA_real_)
    }
    if (!is.finite(rf) || rf <= 0) rf <- APP_DEFAULTS$capm_rf / 100
    rm <- suppressWarnings(as.numeric(input$capm_rm)[1]) / 100
    if (!is.finite(rm) || rm <= 0) rm <- APP_DEFAULTS$capm_rm / 100
    rd <- suppressWarnings(as.numeric(input$wacc_rd)[1]) / 100
    if (!is.finite(rd) || rd < 0) rd <- APP_DEFAULTS$wacc_rd / 100
    tax <- suppressWarnings(as.numeric(input$wacc_tax)[1]) / 100
    if (!is.finite(tax) || tax < 0) tax <- APP_DEFAULTS$wacc_tax / 100
    beta_fb <- suppressWarnings(as.numeric(input$capm_beta)[1])
    if (!is.finite(beta_fb)) beta_fb <- APP_DEFAULTS$capm_beta

    # Freeze session capital structure weights for rolling-Ke → WACC mapping.
    we <- NA_real_; wd <- NA_real_
    tryCatch({
      bs <- d_balance_sheet()
      sum_df <- summary_data()
      shares <- select_current_metric(bs, "Share Issued|Ordinary Shares Number", "stock")
      price_val <- NA_real_
      if (!is.null(sum_df) && is.data.frame(sum_df) && "Previous Close" %in% sum_df$Item) {
        price_val <- parse_financial_number(sum_df$Value[sum_df$Item == "Previous Close"][1])
      }
      equity_mv <- if (is.finite(shares) && shares > 0 && is.finite(price_val)) {
        shares * price_val
      } else {
        select_current_metric(bs, "Common Stock Equity", "stock")
      }
      debt <- select_current_metric(bs, "Total Debt", "stock")
      debt <- if (is.na(debt)) 0 else debt
      if (is.finite(equity_mv) && equity_mv > 0) {
        tot <- equity_mv + debt
        if (is.finite(tot) && tot > 0) {
          we <- equity_mv / tot
          wd <- debt / tot
        }
      }
    }, error = function(e) NULL)

    list(
      wacc = wacc, ke = ke, sgr = sgr, g_explicit = g_explicit,
      n_years = n_years, pb_mid = pb_mid, ddm_g = ddm_g, ddm_ke = ddm_ke,
      ri_roe = ri_roe, ri_payout = ri_payout, ri_years = ri_years,
      ri_g = ri_g, ri_ke = ri_ke,
      roe_method = roe_method, roe_terminal = roe_terminal,
      roe_industry = roe_industry, roe_custom_vec = roe_custom_vec,
      fv_model = fv_model,
      rf = rf, rm = rm, rd = rd, tax = tax,
      we = we, wd = wd,
      beta_fallback = beta_fb,
      beta_lookback_months = 60L,
      beta_min_months = 24L
    )
  })

  apply_bt_params_to_ui <- function(p) {
    bt_applying_params(TRUE)
    on.exit(bt_applying_params(FALSE), add = TRUE)
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

  observeEvent(list(current_ticker(), scraped_financials()), {
    req(current_ticker(), scraped_financials())
    if (!isTRUE(input$bt_param_auto)) return()
    tryCatch(refresh_bt_params(fetch_hist = FALSE), error = function(e) {
      bt_param_notes_txt(paste("自動推導失敗：", e$message))
    })
  }, ignoreInit = FALSE)

  observeEvent(input$bt_refresh_params, {
    tryCatch({
      refresh_bt_params(fetch_hist = TRUE)
      showNotification("✅ 已依目前公司重算一次（門檻／權重）", type = "message")
    }, error = function(e) {
      showNotification(paste("參數重算失敗：", e$message), type = "error")
    })
  })

  observeEvent(input$bt_param_auto, {
    if (isTRUE(input$bt_param_auto)) {
      tryCatch(refresh_bt_params(fetch_hist = FALSE), error = function(e) NULL)
      bt_param_notes_txt(
        "自動同步已開啟：換股／載入財報時會覆寫門檻、權重與推薦估值模型。若只要算一次，可取消勾選後按「立即依目前公司重算一次」。"
      )
    } else {
      bt_param_notes_txt(
        "自動同步已關閉：參數不會因換股被覆寫。需要時可按「立即依目前公司重算一次」單次推導。"
      )
    }
  })

  # 勾選評價模型即重建基本面價值；取消全部則隱藏折線
  observeEvent(input$bt_fv_models, {
    if (isTRUE(bt_applying_params())) return()
    if (isTRUE(input$bt_param_auto)) {
      updateCheckboxInput(session, "bt_param_auto", value = FALSE)
    }

    sel <- .bt_raw_fv_models()
    if (length(sel) < 1) {
      bt_fv_visible(FALSE)
      bt_hfv_fv(NULL)
      return()
    }
    if (is.null(current_ticker()) || !nzchar(as.character(current_ticker())[1])) return()

    tryCatch({
      if (is.null(d_income_statement()) || is.null(d_cash_flow()) || is.null(d_balance_sheet())) {
        stop("請先在 Dashboard 搜尋並載入該公司財報")
      }
      mp <- bt_current_model_params()
      fund <- build_annual_fundamentals(
        d_income_statement(), d_balance_sheet(), d_cash_flow()
      )
      withProgress(message = "重建基本面價值…", value = 0.2, {
        fv_res <- compute_fair_value_timeline(
          ticker = current_ticker(),
          d_is = d_income_statement(),
          d_bs = d_balance_sheet(),
          d_cf = d_cash_flow(),
          model_params = mp,
          mos = bt_current_mos(),
          bench_ticker = "SPY",
          years = 5
        )
        bt_hfv_fv(fv_res)
        bt_fv_visible(TRUE)
        if (!is.null(bt_result())) {
          bt_result(refresh_backtest_fair_value(bt_result(), fund, mp))
        }
      })
    }, error = function(e) {
      bt_fv_visible(FALSE)
      showNotification(paste("❌ 基本面價值計算失敗：", e$message), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(list(input$bt_w_vg, input$bt_w_mom, input$bt_w_rsi,
                    input$bt_net_margin, input$bt_rev_growth, input$bt_eps_growth, input$bt_fcf_cv,
                    input$bt_max_exp, input$bt_min_exp_pass), {
    if (isTRUE(bt_applying_params())) return()
    if (!isTRUE(input$bt_param_auto)) return()
    updateCheckboxInput(session, "bt_param_auto", value = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$bt_fit_bh_preset, {
    bt_applying_params(TRUE)
    on.exit(bt_applying_params(FALSE), add = TRUE)
    updateCheckboxInput(session, "bt_param_auto", value = FALSE)
    updateSliderInput(session, "bt_max_exp", value = 1)
    updateSliderInput(session, "bt_min_exp_pass", value = 0.4)
    updateSliderInput(session, "bt_w_vg", value = 0.35)
    showNotification("✅ 已套用「貼近買進持有」：max=100%、min=40%、w_vg=0.35。請重新啟動回測。",
                     type = "message", duration = 8)
  })

  output$bt_param_notes <- renderUI({
    msg <- bt_param_notes_txt()
    tags$div(
      style = "margin: 0 0 12px 0; padding: 12px 14px; background: #f7fbf8; border-left: 4px solid #00a65a; border-radius: 4px; font-size: 13px; color: #333; line-height: 1.55; width: 100%;",
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
    bt_validation(NULL)
    bt_hfv_fv(NULL)
    bt_fv_visible(FALSE)
    bt_run_msg("V12 回測計算中（PIT 多模型重建）…")
    tryCatch({
      if (is.null(d_income_statement()) || is.null(d_cash_flow()) || is.null(d_balance_sheet())) {
        stop("請先在 Dashboard 搜尋並載入該公司財報")
      }
      params <- list(
        bt_net_margin = input$bt_net_margin,
        bt_rev_growth = input$bt_rev_growth,
        bt_eps_growth = input$bt_eps_growth,
        bt_fcf_cv = input$bt_fcf_cv,
        bt_w_mom = input$bt_w_mom,
        bt_w_rsi = input$bt_w_rsi,
        bt_w_vg = input$bt_w_vg,
        bt_max_exp = input$bt_max_exp,
        bt_min_exp_pass = input$bt_min_exp_pass
      )
      mp <- bt_current_model_params()
      withProgress(message = paste("V12 回測", current_ticker(), "…"), value = 0.15, {
        res <- run_company_backtest(
          ticker = current_ticker(),
          d_is = d_income_statement(),
          d_bs = d_balance_sheet(),
          d_cf = d_cash_flow(),
          params = params,
          model_params = mp,
          mos = bt_current_mos(),
          bench_ticker = "SPY",
          years = 5
        )
        incProgress(0.55, detail = "Alpha／MOS／Gap 驗證…")
        # Prefer backtest-native daily Close (same window as equity curve).
        px <- if (!is.null(res$equity_df$Close)) {
          data.frame(Date = res$equity_df$Date, Close = res$equity_df$Close,
                     stringsAsFactors = FALSE)
        } else {
          tryCatch({
            cached <- hist_stock_data()
            if (!is.null(cached) && nrow(cached) > 0) cached[, c("Date", "Close"), drop = FALSE]
            else data.frame(Date = res$valuation_df$Date, Close = res$valuation_df$hist_price,
                            stringsAsFactors = FALSE)
          }, error = function(e) {
            data.frame(Date = res$valuation_df$Date, Close = res$valuation_df$hist_price,
                       stringsAsFactors = FALSE)
          })
        }
        rf_ann <- tryCatch({
          r <- as.numeric(cached_get_risk_free_rate())
          if (is.finite(r) && r > 0) r / 100 else 0.04
        }, error = function(e) 0.04)

        alpha_df <- tryCatch(compute_alpha_dashboard(res$equity_df, rf_annual = rf_ann),
                             error = function(e) NULL)
        gap <- tryCatch(
          analyze_bh_gap(
            res$equity_df, res$valuation_df,
            max_exp = .safe_num(params$bt_max_exp, 0.90)
          ),
          error = function(e) list(narrative_a = e$message)
        )
        mos_tab <- tryCatch(validate_mos_effectiveness(res$valuation_df, px),
                            error = function(e) NULL)
        fv_edge <- tryCatch(validate_fair_value_edge(res$valuation_df, px),
                            error = function(e) NULL)
        # 參數高原已自 UI 移除（與 Sensitivity 重疊）；略過以縮短回測時間
        bt_result(res)
        bt_validation(list(
          alpha = alpha_df, gap = gap, mos = mos_tab, fv = fv_edge, plateau = NULL
        ))
        bt_hfv_fv(NULL)
        bt_fv_visible(TRUE)
        bt_run_msg(sprintf(
          "完成：%s 日 · 季頻 PIT · Rolling β · 較佳=%s · Session WACC=%.2f%% Ke=%.2f%% SGR=%.2f%%",
          res$n_days, res$metrics$best,
          mp$wacc * 100, mp$ke * 100, mp$sgr * 100
        ))
      })
    }, error = function(e) {
      bt_run_msg(paste("失敗：", e$message))
      showNotification(paste("❌ 回測失敗：", e$message), type = "error", duration = 12)
    })
  })

  .fmt_pct <- function(x, digits = 1) {
    if (is.null(x) || length(x) < 1 || is.na(x) || !is.finite(x)) return("N/A")
    sprintf(paste0("%.", digits, "f%%"), 100 * as.numeric(x))
  }
  .fmt_num <- function(x, digits = 2) {
    if (is.null(x) || length(x) < 1 || is.na(x) || !is.finite(x)) return("N/A")
    sprintf(paste0("%.", digits, "f"), as.numeric(x))
  }

  .bt_hfv_chart_source <- function() {
    base <- bt_hfv_base()
    res <- bt_result()
    fv_only <- bt_hfv_fv()
    show_fv <- isTRUE(bt_fv_visible())

    if (!is.null(res) && !is.null(res$equity_df) && nrow(res$equity_df) > 0) {
      ed <- res$equity_df
      vd <- res$valuation_df
      mp <- res$model_params_used
    } else if (show_fv && !is.null(fv_only) && !is.null(fv_only$equity_df)) {
      ed <- fv_only$equity_df
      vd <- fv_only$valuation_df
      mp <- fv_only$model_params_used
    } else if (!is.null(base) && nrow(base) > 0) {
      ed <- base
      vd <- NULL
      mp <- NULL
    } else {
      return(NULL)
    }

    list(
      ed = ed,
      vd = vd,
      mp = mp,
      show_fv = show_fv && length(.bt_raw_fv_models()) > 0 && any(vapply(
        .bt_raw_fv_models(),
        function(m) {
          specs <- .bt_fv_model_specs()
          sp <- specs[[m]]
          if (is.null(sp)) return(FALSE)
          col <- sp$col
          col %in% names(ed) && any(is.finite(ed[[col]]))
        },
        logical(1)
      ))
    )
  }

  output$bt_valuation_summary <- renderUI({
    src <- .bt_hfv_chart_source()
    if (is.null(src)) {
      return(tags$p(
        style = "color:#888;font-size:12.5px;",
        "搜尋股票後將預先顯示股價與大盤；勾選右下角評價模型可計算基本面價值與 MOS 摘要。"
      ))
    }
    if (!isTRUE(src$show_fv)) {
      return(tags$p(
        style = "color:#888;font-size:12.5px;",
        "已顯示情緒波動價值（實際股價）與大盤。勾選右下角評價模型以顯示基本面價值與下方摘要。"
      ))
    }
    m <- if (!is.null(bt_result()) && !is.null(bt_result()$metrics)) {
      bt_result()$metrics
    } else if (!is.null(bt_hfv_fv()) && !is.null(bt_hfv_fv()$metrics)) {
      bt_hfv_fv()$metrics
    } else {
      NULL
    }
    mp <- src$mp
    if (is.null(m) || is.null(mp)) {
      return(tags$p(style = "color:#888;font-size:12.5px;", "尚無估值摘要。"))
    }
    bias <- as.character(m$market_pricing_bias %||% "—")
    bias_col <- if (grepl("低估", bias, fixed = TRUE)) {
      "#00a65a"
    } else if (grepl("高估", bias, fixed = TRUE)) {
      "#d9534f"
    } else {
      "#666"
    }
    bias_bg <- if (grepl("低估", bias, fixed = TRUE)) {
      "#f7fbf8"
    } else if (grepl("高估", bias, fixed = TRUE)) {
      "#fdf7f7"
    } else {
      "#fafafa"
    }
    bias_val <- bias
    pct_under <- m$pct_market_under %||% m$pct_value_over
    tags$div(
      style = "display:flex;flex-wrap:wrap;gap:10px;margin-bottom:8px;",
      tags$div(style = paste0("flex:1;min-width:120px;padding:8px 10px;background:", bias_bg,
                              ";border-left:4px solid ", bias_col, ";"),
               tags$div(class = "ynow-kpi-stat-label", "歷史市場定價"),
               tags$div(class = "ynow-kpi-stat-value", style = paste0("color:", bias_col, ";"), bias_val)),
      tags$div(style = "flex:1;min-width:120px;padding:8px 10px;background:#f7fbf8;border-left:4px solid #00a65a;",
               tags$div(class = "ynow-kpi-stat-label", "市場低估率"),
               tags$div(class = "ynow-kpi-stat-value", style = "color:#00a65a;",
                        .fmt_pct(pct_under, 0)),
               tags$div(class = "ynow-kpi-stat-note",
                        "股價低於模型合理價的再平衡日佔比")),
      tags$div(style = "flex:1;min-width:120px;padding:8px 10px;background:#f7f9fb;border-left:4px solid #3c8dbc;",
               tags$div(class = "ynow-kpi-stat-label", "平均 MOS"),
               tags$div(class = "ynow-kpi-stat-value", style = "color:#3c8dbc;", .fmt_pct(m$mean_hist_mos))),
      tags$div(style = "flex:2;min-width:180px;padding:8px 10px;background:#fafafa;border-left:4px solid #555;",
               tags$div(class = "ynow-kpi-stat-label", "此刻參數（Session）"),
               tags$div(class = "ynow-kpi-stat-params", {
                 models <- paste(toupper(.bt_raw_fv_models()), collapse = "+")
                 base <- sprintf(
                   "模型 %s · WACC %.2f%% · Ke %.2f%% · SGR %.2f%% · n=%s · PB mid %.2f",
                   models,
                   .safe_num(mp$wacc, NA) * 100, .safe_num(mp$ke, NA) * 100,
                   .safe_num(mp$sgr, NA) * 100, mp$n_years, .safe_num(mp$pb_mid, NA)
                 )
                 if ("ri" %in% .bt_raw_fv_models()) {
                   paste0(
                     base,
                     sprintf(
                       " · RI[ROE %.1f%% · payout %.0f%% · n=%s · g %.2f%% · Ke %.2f%% · fade=%s]",
                       .safe_num(mp$ri_roe, NA) * 100,
                       .safe_num(mp$ri_payout, NA) * 100,
                       mp$ri_years %||% mp$n_years,
                       .safe_num(mp$ri_g, NA) * 100,
                       .safe_num(mp$ri_ke, NA) * 100,
                       mp$roe_method %||% "constant"
                     )
                   )
                 } else if ("ddm" %in% .bt_raw_fv_models()) {
                   paste0(
                     base,
                     sprintf(
                       " · DDM[g %.2f%% · Ke %.2f%%]",
                       .safe_num(mp$ddm_g, NA) * 100,
                       .safe_num(mp$ddm_ke, NA) * 100
                     )
                   )
                 } else {
                   base
                 }
               }))
    )
  })

  output$bt_hfv_timeline <- renderPlotly({
    src <- .bt_hfv_chart_source()
    validate(need(!is.null(src) && !is.null(src$ed) && nrow(src$ed) > 0,
                  "請先搜尋股票以載入歷史股價"))
    ed <- src$ed
    has_bench <- "Bench" %in% names(ed)

    p <- plotly::plot_ly(ed, x = ~Date)
    if (isTRUE(src$show_fv)) {
      specs <- .bt_fv_model_specs()
      for (m in .bt_raw_fv_models()) {
        sp <- specs[[m]]
        if (is.null(sp)) next
        col <- sp$col
        if (!col %in% names(ed) || !any(is.finite(ed[[col]]))) next
        p <- plotly::add_trace(
          p, y = ed[[col]], name = sp$label, type = "scatter", mode = "lines",
          line = list(color = sp$color, width = 2),
          hovertemplate = paste0(sp$label, ": %{y:$.2f}<extra></extra>")
        )
      }
    }
    p <- plotly::add_trace(
      p, y = ~Close, name = "情緒波動價值（實際股價）", type = "scatter", mode = "lines",
      line = list(color = "#2c3e50", width = 2),
      hovertemplate = "情緒波動價值: %{y:$.2f}<extra></extra>"
    )
    if (has_bench && any(is.finite(ed$Bench))) {
      p <- plotly::add_trace(
        p, y = ~Bench, name = "大盤基準", type = "scatter", mode = "lines",
        line = list(color = "#7f8c8d", width = 1.6, dash = "dot"),
        yaxis = "y2",
        hovertemplate = "大盤: %{y:$.2f}<extra></extra>"
      )
    }
    vd <- src$vd
    if (isTRUE(src$show_fv) && !is.null(vd) && nrow(vd) > 0 &&
        length(.bt_raw_fv_models()) == 1L) {
      primary <- .bt_raw_fv_models()[1]
      marker_col <- switch(primary,
        "dcf" = "fv_dcf", "ddm" = "fv_ddm", "ri" = "fv_ri", "pb" = "fv_pb", "fair_value")
      if (marker_col %in% names(vd) && any(is.finite(vd[[marker_col]]))) {
        p <- plotly::add_trace(
          p,
          x = vd$Date,
          y = vd[[marker_col]],
          name = "季再平衡 FV",
          type = "scatter",
          mode = "markers",
          marker = list(color = "#e74c3c", size = 7, symbol = "diamond"),
          hovertemplate = paste0(
            "再平衡 %{x|%Y-%m-%d}<br>FV %{y:$.2f}",
            if ("rolling_beta" %in% names(vd)) "<br>β %{customdata:.2f}" else "",
            "<extra></extra>"
          ),
          customdata = if ("rolling_beta" %in% names(vd)) vd$rolling_beta else NULL
        )
      }
    }
    plotly::layout(
      p,
      title = list(
        text = if (isTRUE(src$show_fv)) "折現比較（Rolling β PIT）" else "折現比較（股價／大盤預覽）",
        font = list(size = 14)
      ),
      legend = list(orientation = "h", y = -0.18),
      yaxis = list(title = "每股（美元）", tickprefix = "$", side = "left"),
      yaxis2 = list(
        title = "大盤價格", overlaying = "y", side = "right",
        showgrid = FALSE, tickprefix = "$"
      ),
      xaxis = list(title = NULL),
      margin = list(l = 60, r = 60, t = 40, b = 60),
      hovermode = "x unified"
    )
  })

  output$bt_signal_explain <- renderUI({
    if (!isTRUE(bt_fv_visible())) return(NULL)
    res <- bt_result()
    fv_only <- bt_hfv_fv()
    ex <- if (!is.null(res) && !is.null(res$explain_last)) {
      res$explain_last
    } else if (!is.null(fv_only) && !is.null(fv_only$explain_last)) {
      fv_only$explain_last
    } else {
      NULL
    }
    if (is.null(ex)) return(NULL)
    vd <- if (!is.null(res) && !is.null(res$valuation_df)) {
      res$valuation_df
    } else if (!is.null(fv_only) && !is.null(fv_only$valuation_df)) {
      fv_only$valuation_df
    } else {
      NULL
    }
    row <- ex
    if (is.null(row$hist_price) && !is.null(row$price)) row$hist_price <- row$price
    if (is.null(row$Date) && !is.null(vd) && nrow(vd) > 0) {
      row$Date <- tail(vd$Date, 1)
      row$explain <- tail(vd$explain, 1)
    }
    txt <- tryCatch(build_signal_explain(row)$text, error = function(e) NULL)
    if (is.null(txt) || !nzchar(as.character(txt)[1])) {
      txt <- sprintf(
        "最近訊號：%s | MOS=%s | Score=%s | Exp A=%s | DCF=%s DDM=%s RI=%s PB=%s",
        as.character(ex$signal), .fmt_pct(ex$mos), .fmt_num(ex$valuation_score, 0),
        .fmt_pct(ex$exp_a, 0),
        .fmt_num(ex$fv_dcf), .fmt_num(ex$fv_ddm), .fmt_num(ex$fv_ri), .fmt_num(ex$fv_pb)
      )
    }
    tags$pre(
      style = "margin-top:10px;padding:12px;background:#f8f9fa;border-left:4px solid #3c8dbc;font-size:12px;white-space:pre-wrap;",
      txt
    )
  })

  output$bt_exposure_stats <- renderUI({
    res <- bt_result()
    if (is.null(res) || is.null(res$exposure)) {
      return(tags$p(style = "color:#888;font-size:12px;", "回測後顯示平均／最高／最低持股與現金比例。"))
    }
    e <- res$exposure
    tags$div(
      style = "font-size:13px;line-height:1.7;",
      tags$div(tags$b("基本面倉位 平均 "), .fmt_pct(e$avg_a, 0),
               " ｜ 最高 ", .fmt_pct(e$max_a, 0),
               " ｜ 最低 ", .fmt_pct(e$min_a, 0),
               " ｜ 現金 ", .fmt_pct(e$cash_avg_a, 0)),
      tags$div(tags$b("情緒倉位 平均 "), .fmt_pct(e$avg_b, 0),
               " ｜ 最高 ", .fmt_pct(e$max_b, 0),
               " ｜ 最低 ", .fmt_pct(e$min_b, 0),
               " ｜ 現金 ", .fmt_pct(e$cash_avg_b, 0)),
      tags$hr(),
      tags$div(style = "font-size:11px;color:#777;",
               "倉位曲線驅動上方策略淨值。平均持股偏低時，輸給 B&H 多半是 Cash Drag（最高約 90% 屬刻意風控）。")
    )
  })

  output$bt_exposure_plot <- renderPlotly({
    res <- bt_result()
    validate(need(!is.null(res) && !is.null(res$equity_df), "請先回測"))
    df <- res$equity_df
    df_long <- rbind(
      data.frame(Date = df$Date, Exp = df$Exp_A, Series = "基本面倉位 Exp_A", stringsAsFactors = FALSE),
      data.frame(Date = df$Date, Exp = df$Exp_B, Series = "情緒倉位 Exp_B", stringsAsFactors = FALSE)
    )
    p <- ggplot(df_long, aes(x = Date, y = Exp, color = Series)) +
      geom_line(linewidth = 0.8) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      scale_color_manual(values = c(
        "基本面倉位 Exp_A" = "#e67e22",
        "情緒倉位 Exp_B" = "#2980b9"
      )) +
      labs(y = "目標持股比例", x = NULL, color = NULL) +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$bt_bh_gap <- renderUI({
    v <- bt_validation()
    if (is.null(v) || is.null(v$gap)) {
      return(tags$p(style = "color:#888;font-size:12px;",
                    "回測後拆解相對 Buy&Hold 的缺口：現金拖累／過早出場／高估減碼／情緒加減碼。牛市落後常因持股上限與 Great Filter 空手。"))
    }
    g <- v$gap
    fr <- g$fractions_a
    beat <- isTRUE(g$beat_bh_a)
    mk <- function(label, key, tip) {
      val <- if (!is.null(fr) && !is.null(fr[[key]])) fr[[key]] else NA
      # Also show absolute pp from components when available
      abs_pp <- if (!is.null(g$components_a) && !is.null(g$components_a[[key]])) {
        g$components_a[[key]]
      } else NA
      tags$div(
        style = "margin-bottom:8px;",
        tags$div(style = "display:flex;justify-content:space-between;font-size:12px;",
                 tags$span(tags$b(label)),
                 tags$span(
                   if (beat || !is.finite(val)) {
                     paste0(.fmt_pct(abs_pp, 1), "（絕對）")
                   } else {
                     paste0(.fmt_pct(val, 0), " of gap")
                   }
                 )),
        tags$div(style = "font-size:11px;color:#777;", tip)
      )
    }
    tagList(
      if (beat) {
        tags$div(
          style = "margin:0 0 10px 0;padding:8px 10px;background:#eef8f1;border-left:4px solid #28a745;font-size:12px;color:#1e5c35;",
          tags$b("結論："), "純基本面價值未輸給 Buy&Hold。"
        )
      } else {
        tags$div(
          style = "margin:0 0 10px 0;padding:8px 10px;background:#fff8e8;border-left:4px solid #f0ad4e;font-size:12px;color:#7a5b10;",
          tags$b("結論："), "相對 Buy&Hold 落後；下列為上漲日減碼歸因（日報酬加總近似，非複利拆解）。"
        )
      },
      tags$p(style = "font-size:12.5px;line-height:1.55;", g$narrative_a),
      if (!is.null(g$narrative_b)) tags$p(style = "font-size:12px;color:#555;", g$narrative_b),
      mk("Cash Drag 現金拖累", "cash_drag", "持股不足 100% 時錯過上漲"),
      mk("Early Exit 提早賣出", "early_exit", "近 20 日相對降倉後市場續漲"),
      mk("Overvaluation Reduction", "overvaluation_reduction", "因高估／負 MOS 減碼"),
      mk("Missed Trend 殘差", "missed_trend", "缺口 − 上述三項（可為負，表示歸因重疊／近似誤差）"),
      if (!is.null(g$sentiment_reduction_b) || !is.null(g$sentiment_boost_b))
        tags$p(
          style = "font-size:12px;",
          tags$b("情緒 vs 純基本面："),
          "減碼 ", .fmt_pct(g$sentiment_reduction_b, 1),
          " ／加碼 ", .fmt_pct(g$sentiment_boost_b %||% 0, 1),
          "（上漲日）"
        )
    )
  })

  output$bt_equity_plot <- renderPlotly({
    res <- bt_result()
    validate(need(!is.null(res) && !is.null(res$equity_df), "請先成功執行回測"))
    df_plot <- res$equity_df
    validate(need("Trade_A" %in% names(df_plot), "缺少純基本面價值策略淨值 (Trade_A)"))
    eq_b <- if ("Trade_B" %in% names(df_plot)) df_plot$Trade_B else df_plot$Model_B
    df_long <- rbind(
      data.frame(Date = df_plot$Date, Value = df_plot$Trade_A, Series = "純基本面價值", stringsAsFactors = FALSE),
      data.frame(Date = df_plot$Date, Value = eq_b, Series = "情緒波動價值", stringsAsFactors = FALSE),
      data.frame(Date = df_plot$Date, Value = df_plot$BuyHold, Series = "該股買進持有", stringsAsFactors = FALSE),
      data.frame(Date = df_plot$Date, Value = df_plot$Benchmark, Series = "大盤基準", stringsAsFactors = FALSE)
    )
    df_long$Series <- factor(
      df_long$Series,
      levels = c("純基本面價值", "情緒波動價值", "該股買進持有", "大盤基準")
    )
    p <- ggplot(df_long, aes(x = Date, y = Value, color = Series, group = Series, linetype = Series)) +
      geom_line(linewidth = 0.85) +
      scale_color_manual(values = c(
        "純基本面價值" = "#e67e22",
        "情緒波動價值" = "#2980b9",
        "該股買進持有" = "#28a745",
        "大盤基準" = "#6c757d"
      )) +
      scale_linetype_manual(values = c(
        "純基本面價值" = "solid",
        "情緒波動價值" = "solid",
        "該股買進持有" = "solid",
        "大盤基準" = "dashed"
      )) +
      scale_y_continuous(labels = label_chart_number()) +
      labs(y = "策略累積淨值（起點=1）", x = "日期", color = "序列", linetype = "序列") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$bt_mos_table <- renderTable({
    v <- bt_validation()
    validate(need(!is.null(v) && !is.null(v$mos) && nrow(v$mos) > 0, "尚無 MOS 分組結果"))
    tab <- v$mos
    data.frame(
      MOS分組 = tab$bucket,
      樣本數 = tab$n,
      `1Y報酬` = ifelse(is.na(tab$ret_1y), NA, sprintf("%.1f%%", 100 * tab$ret_1y)),
      `3Y報酬` = ifelse(is.na(tab$ret_3y), NA, sprintf("%.1f%%", 100 * tab$ret_3y)),
      `5Y報酬` = ifelse(is.na(tab$ret_5y), NA, sprintf("%.1f%%", 100 * tab$ret_5y)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$bt_fv_edge <- renderUI({
    v <- bt_validation()
    if (is.null(v) || is.null(v$fv)) return(tags$p(style="color:#888;font-size:12px;", "回測後回答：價格遠低於合理價時，前瞻報酬是否較高？"))
    tags$p(style = "font-size:13px;line-height:1.55;", tags$b("結論："), v$fv$answer)
  })

  output$bt_fv_table <- renderTable({
    v <- bt_validation()
    validate(need(!is.null(v) && !is.null(v$fv) && !is.null(v$fv$table), "尚無 FV 驗證表"))
    tab <- v$fv$table
    data.frame(
      組別 = tab$group,
      樣本數 = tab$n,
      `1Y` = ifelse(is.na(tab$ret_1y), NA, sprintf("%.1f%%", 100 * tab$ret_1y)),
      `3Y` = ifelse(is.na(tab$ret_3y), NA, sprintf("%.1f%%", 100 * tab$ret_3y)),
      `5Y` = ifelse(is.na(tab$ret_5y), NA, sprintf("%.1f%%", 100 * tab$ret_5y)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$bt_plateau <- renderUI({
    v <- bt_validation()
    if (is.null(v) || is.null(v$plateau)) {
      return(tags$p(style="color:#888;font-size:12px;", "微擾 WACC／SGR／年數後，輸出 Stable／Moderate／Sensitive 與原因。"))
    }
    p <- v$plateau
    st <- as.character(p$status)
    col <- if (grepl("Stable|穩定", st, ignore.case = TRUE)) "#00a65a"
    else if (grepl("Moderate|中等", st, ignore.case = TRUE)) "#f39c12"
    else if (grepl("Sensitive|敏感", st, ignore.case = TRUE)) "#d9534f"
    else "#777"
    tags$div(
      tags$span(class = "ynow-kpi-hero-value", style = paste0("color:", col, ";"), p$status),
      tags$p(style = "margin-top:8px;font-size:12px;line-height:1.55;", p$reason)
    )
  })

  output$bt_plateau_table <- renderTable({
    v <- bt_validation()
    validate(need(!is.null(v) && !is.null(v$plateau) && !is.null(v$plateau$details), "尚無敏感度明細"))
    d <- v$plateau$details
    if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
    if (all(c("model_a_end", "d_rel") %in% names(d))) {
      data.frame(
        scenario = d$scenario,
        model_a_end = round(d$model_a_end, 4),
        d_rel = ifelse(is.na(d$d_rel), NA, sprintf("%+.1f%%", 100 * d$d_rel)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      d
    }
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$perf_metrics <- renderUI({
    res <- bt_result()
    v <- bt_validation()
    if (is.null(res) || is.null(res$metrics)) {
      return(
        tags$div(
          style = "color: #888; font-size: 12.5px; line-height: 1.55;",
          icon("chart-bar"),
          " 執行回測後，此處會以卡片顯示 ",
          tags$b("Sharpe"), "、", tags$b("Max DD"), "、",
          tags$b("CAGR"), "、", tags$b("Excess vs BH"), "、",
          tags$b("Jensen α"), "。"
        )
      )
    }
    m <- res$metrics
    best <- m$best
    label_best <- if (identical(best, "A")) "純基本面價值" else "情緒波動價值"
    sharpe_show <- if (identical(best, "A")) m$sharpe_a else m$sharpe_b
    mdd_show <- if (identical(best, "A")) m$mdd_a else m$mdd_b
    cagr_show <- if (identical(best, "A")) m$cagr_a else m$cagr_b
    sharpe_a_txt <- if (is.na(m$sharpe_a)) "N/A" else sprintf("%.2f", m$sharpe_a)
    sharpe_b_txt <- if (is.na(m$sharpe_b)) "N/A" else sprintf("%.2f", m$sharpe_b)

    # Alpha 列（較佳策略優先，否則 A）
    alpha_df <- if (!is.null(v) && !is.null(v$alpha) && is.data.frame(v$alpha)) v$alpha else NULL
    pick_alpha <- function(series) {
      if (is.null(alpha_df) || nrow(alpha_df) == 0) return(NULL)
      hit <- alpha_df[alpha_df$Series == series, , drop = FALSE]
      if (nrow(hit) == 0) NULL else hit[1, ]
    }
    a_row <- pick_alpha("StrategyA")
    b_row <- pick_alpha("StrategyB")
    bh_row <- pick_alpha("BuyHold")
    best_row <- if (identical(best, "A")) a_row else b_row
    excess_show <- if (!is.null(best_row)) best_row$ExcessReturn else NA_real_
    jensen_show <- if (!is.null(best_row)) best_row$JensenAlpha else NA_real_
    if (!is.null(best_row) && is.finite(best_row$CAGR)) cagr_show <- best_row$CAGR

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
        "以下以 Sharpe 較高的策略為主顯示（", label_best, "）；A＝", sharpe_a_txt,
        "，B＝", sharpe_b_txt, "。已整併 Alpha（Excess／Jensen α）。數值僅供比較參考。"
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
          tip = "年化 Sharpe ≈ 日報酬均值 ÷ 標準差 × √252。"
        ),
        .ynow_metric_card(
          value = if (is.na(mdd_show)) "N/A" else paste0(sprintf("%.1f", mdd_show * 100), "%"),
          label = paste0("最大回撤 Max DD（", label_best, "）"),
          caption = "歷史最大虧損幅度；愈接近 0 代表回撤愈小。",
          icon_name = "arrow-down",
          tone = "red",
          tip = "淨值自歷史高點回落的最大百分比幅度。"
        ),
        .ynow_metric_card(
          value = if (is.na(cagr_show)) "N/A" else paste0(sprintf("%.1f", cagr_show * 100), "%"),
          label = paste0("CAGR（", label_best, "）"),
          caption = sprintf(
            "Buy&Hold CAGR＝%s。用來對照策略成長速度。",
            if (is.null(bh_row) || is.na(bh_row$CAGR)) "N/A" else paste0(sprintf("%.1f", bh_row$CAGR * 100), "%")
          ),
          icon_name = "percentage",
          tone = "blue",
          tip = "年化複合成長率（CAGR）。"
        ),
        .ynow_metric_card(
          value = if (is.na(excess_show)) "N/A" else paste0(sprintf("%+.1f", excess_show * 100), "%"),
          label = paste0("Excess vs BH（", label_best, "）"),
          caption = "相對 Buy & Hold 的超額報酬；>0 代表創造價值。",
          icon_name = "balance-scale",
          tone = "amber",
          tip = "策略期末報酬 − Buy&Hold 期末報酬。"
        ),
        .ynow_metric_card(
          value = if (is.na(jensen_show)) "N/A" else paste0(sprintf("%+.1f", jensen_show * 100), "%"),
          label = paste0("Jensen α（", label_best, "）"),
          caption = "相對大盤基準的風險調整超額報酬（年化）。",
          icon_name = "rocket",
          tone = "blue",
          tip = "對 Benchmark 日超額報酬做 CAPM 回歸後的年化截距。"
        )
      )
    )
  })

  # ==========================================
  # 11. 系統按鈕與報告輸出
  # ==========================================
  # DDM Reset 由 ddm_module_server 內的 input$reset_ddm 處理（ns: mod_ddm）

  .bt_methodology_meta <- reactive({
    mp <- tryCatch(bt_current_model_params(), error = function(e) NULL)
    res <- bt_result()
    sel <- tryCatch(isolate(.bt_selected_fv_models()), error = function(e) "dcf")
    fv_lab <- paste(vapply(sel, function(m) {
      switch(m,
        "dcf" = "DCF", "ddm" = "DDM", "ri" = "RI", "pb" = "P/B", toupper(m))
    }, character(1)), collapse = " + ")
    list(
      ticker = isolate(current_ticker()) %||% "N/A",
      bench = if (!is.null(res$bench_ticker)) res$bench_ticker else "SPY",
      sim_years = "5",
      fv_model = fv_lab,
      filters = sprintf(
        "%.1f / %.1f / %.1f / %.1f",
        as.numeric(isolate(input$bt_net_margin) %||% NA),
        as.numeric(isolate(input$bt_rev_growth) %||% NA),
        as.numeric(isolate(input$bt_eps_growth) %||% NA),
        as.numeric(isolate(input$bt_fcf_cv) %||% NA)
      ),
      fit_exp = sprintf(
        "%.2f / %.2f",
        as.numeric(isolate(input$bt_max_exp) %||% 0.9),
        as.numeric(isolate(input$bt_min_exp_pass) %||% 0)
      ),
      weights = sprintf(
        "%.2f / %.2f / %.2f",
        as.numeric(isolate(input$bt_w_vg) %||% NA),
        as.numeric(isolate(input$bt_w_mom) %||% NA),
        as.numeric(isolate(input$bt_w_rsi) %||% NA)
      ),
      sgr_n = if (!is.null(mp)) {
        sprintf("SGR=%.2f%% · n=%s 年", 100 * .safe_num(mp$sgr, NA_real_),
                as.character(mp$n_years %||% "N/A"))
      } else {
        "（尚未載入模型參數）"
      },
      n_days = if (!is.null(res$n_days)) as.character(res$n_days) else "（尚未回測）"
    )
  })

  output$bt_methodology_notes <- renderUI({
    tags$div(
      style = "font-size: 12.5px; line-height: 1.65; color: #333;",
      tags$p(
        style = "margin-top:0;",
        tags$b("淨值圖 ↔ 兩模式："),
        "評價模型算出合理價／MOS → ", tags$b("純基本面價值"), " Exp_A → 橘線；",
        tags$b("情緒波動價值"), " 將 Exp_A 與動能／RSI 情緒目標加權混合得 Exp_B → 藍線（應可分開）。",
        "折現圖上的「情緒波動價值」＝實際股價，語意不同。淨值圖只比策略績效。"
      ),
      tags$h5(tags$b("一、數據來源")),
      tags$ul(
        tags$li(tags$b("股價／基準："), "Yahoo Finance（yfinance，auto_adjust）；基準預設 SPY。"),
        tags$li(tags$b("財報："), "本次 Session 已載入之年度 IS／BS／CF。"),
        tags$li(tags$b("Rf："), "^TNX（驗證用）；失敗時約 4%。"),
        tags$li(tags$b("評價假設："), "Session SGR／年數等；Ke／WACC 於各季再平衡以 Rolling β 重估。")
      ),
      tags$h5(tags$b("二、計算過程（季頻 PIT）")),
      tags$ol(
        tags$li("再平衡日：fund_year ≤ 日曆年−1 重建合理價 → MOS。"),
        tags$li("持倉回測條件未過 → Exp_A = Exp_B = 0（兩模式皆空手）。"),
        tags$li("通過則 Exp_A 依 MOS 滯後映射；Exp_B = (1−blend)×Exp_A + blend×(sentiment×max_exp)。"),
        tags$li("每日：策略淨值用 Exp×日報酬；Buy&Hold 滿倉；現金報酬=0；未扣交易成本。"),
        tags$li("比較視窗自首次有效季再平衡對齊。")
      ),
      tags$h5(tags$b("三、為何常輸給 Buy&Hold")),
      tags$p(
        style = "margin-bottom:0;",
        "持股上限（預設約 90%）與 Great Filter 空手會造成現金拖累；牛市滿倉的 B&H 易勝出。",
        "可用「貼近買進持有」調高上限。完整公式請下載方法論檔。"
      )
    )
  })

  output$download_bt_methodology <- downloadHandler(
    filename = function() {
      tk <- tryCatch(current_ticker(), error = function(e) "NA")
      if (is.null(tk) || !nzchar(as.character(tk))) tk <- "session"
      paste0("YNow_Backtest_Methodology_", tk, "_", Sys.Date(), ".md")
    },
    content = function(file) {
      txt <- build_bt_methodology_doc(.bt_methodology_meta())
      writeLines(txt, file, useBytes = TRUE)
    }
  )
  
  observeEvent(input$reset_dcf, {
    updateRadioButtons(session, "dcf_mode", selected = APP_DEFAULTS$dcf_mode)
    updateCheckboxGroupInput(session, "dcf_chart_layers", selected = APP_DEFAULTS$dcf_chart_layers)
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
    filename = function() {
      tk <- tryCatch(current_ticker(), error = function(e) "NA")
      if (is.null(tk) || !nzchar(as.character(tk))) tk <- "NA"
      paste0("YNow_Report_", tk, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tryCatch({
        showNotification("正在生成 PDF 投資意見報告，請稍候...", type = "message", duration = 8)
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)

        plot_path <- NA_character_
        if (exists("fcf_results") && !is.null(fcf_results$fcf_plot_obj())) {
          plot_path <- file.path(tempdir(), "fcf_plot_temp.png")
          ggsave(plot_path, plot = fcf_results$fcf_plot_obj(), width = 9, height = 5.5, dpi = 200)
        }

        cur_price <- .report_num(tryCatch(isolate(scraped_market_cap()$price), error = function(e) NA))
        dcf_price <- .report_num(isolate(stock_price_estimate_val()))
        ddm_val <- .report_num(tryCatch(isolate(ddm_results$ddm_price()), error = function(e) NA))
        pb_val <- .report_num(tryCatch(isolate(pb_results$pb_price()), error = function(e) NA))
        ri_val <- .report_num(tryCatch(isolate(ri_results$ri_price()), error = function(e) NA))
        ev_val <- .report_num(isolate(dcf_value_result()))

        ind_text_early <- isolate(corp_industry_text())
        rec_full <- tryCatch(
          recommend_valuation_models(
            isolate(d_cash_flow()),
            industry_text = ind_text_early,
            d_is = isolate(d_income_statement()),
            d_bs = isolate(d_balance_sheet()),
            industry_choice = isolate(input$industry_choice)
          ),
          error = function(e) NULL
        )
        val_method <- if (!is.null(rec_full)) {
          list(method = rec_full$summary_method, rationale = rec_full$reason)
        } else {
          derive_valuation_method(isolate(d_cash_flow()), industry_text = ind_text_early)
        }

        band <- tryCatch(isolate(primary_valuation_band()), error = function(e) NULL)
        sec_pt <- .report_num(tryCatch(isolate(secondary_valuation_point()), error = function(e) NA))
        conf <- tryCatch(isolate(valuation_confidence()), error = function(e) NULL)

        # 目標價＝主模型 Base；缺則依 primary key 回退
        rating_anchor <- {
          if (!is.null(band) && is.finite(suppressWarnings(as.numeric(band$base)[1]))) {
            as.numeric(band$base)[1]
          } else {
            prim <- as.character(rec_full$primary %||% "")
            switch(
              prim,
              "dcf" = dcf_price,
              "ddm" = ddm_val,
              "pb" = pb_val,
              "ri" = ri_val,
              if (is.finite(dcf_price)) dcf_price else if (is.finite(pb_val)) pb_val else if (is.finite(ddm_val)) ddm_val else ri_val
            )
          }
        }

        rating_info <- derive_investment_rating(cur_price, rating_anchor)

        sum_df <- isolate(summary_data())
        co_name <- isolate(attr(sum_df, "company_name"))
        if (is.null(co_name) || is.na(co_name) || !nzchar(as.character(co_name))) {
          co_name <- isolate(current_ticker())
        }

        ind_text <- isolate(corp_industry_text())
        sector_str <- "N/A"; industry_str <- "N/A"
        if (!is.null(ind_text) && grepl("\\|", ind_text)) {
          parts <- strsplit(ind_text, "\\|")[[1]]
          sector_str <- trimws(sub("Sector:\\s*", "", parts[1]))
          if (length(parts) > 1) industry_str <- trimws(sub("Industry:\\s*", "", parts[2]))
        }

        wacc_str <- if (identical(isolate(input$dcf_mode), "gordon")) {
          paste0(isolate(input$wacc_gordon), "%")
        } else {
          paste0(isolate(input$wacc_stage1), "% / ", isolate(input$wacc_stage2), "% (兩階段)")
        }

        warn_msgs <- collect_fraud_warnings(
          isolate(d_cash_flow()), isolate(d_income_statement()), isolate(d_balance_sheet())
        )

        highlights <- c()
        if (is.finite(rating_info$upside_pct)) {
          highlights <- c(highlights, sprintf(
            "依主模型「%s」Base 評價，目標價 %s，潛在報酬 %+.1f%%，評等「%s」。",
            val_method$method,
            ifelse(is.finite(rating_anchor), paste0("$", round(rating_anchor, 2)), "N/A"),
            rating_info$upside_pct, rating_info$rating
          ))
        }
        if (!is.null(band) && is.finite(band$bear) && is.finite(band$bull)) {
          highlights <- c(highlights, sprintf(
            "主模型區間 Bear/Base/Bull：$%.2f / $%.2f / $%.2f。",
            band$bear, band$base, band$bull
          ))
        }
        if (is.finite(sec_pt) && !is.null(rec_full$secondary)) {
          highlights <- c(highlights, sprintf(
            "副模型（%s）檢核點：$%.2f。",
            .model_label(rec_full$secondary), sec_pt
          ))
        }
        if (is.list(conf) && !is.null(conf$level)) {
          highlights <- c(highlights, sprintf("估值可信度：%s（%s）。", conf$level, conf$score %||% "—"))
        }
        if (is.finite(dcf_price)) highlights <- c(highlights, paste0("DCF 每股合理價：$", round(dcf_price, 2), "。"))
        if (is.finite(ev_val)) highlights <- c(highlights, paste0("DCF 企業價值 (EV)：", format_dollar_abbr(ev_val), "。"))
        if (is.finite(ddm_val)) highlights <- c(highlights, paste0("DDM 每股合理價：$", round(ddm_val, 2), "。"))
        if (is.finite(ri_val)) highlights <- c(highlights, paste0("RI 每股合理價：$", round(ri_val, 2), "。"))
        if (is.finite(pb_val)) highlights <- c(highlights, paste0("P/B 每股合理價：$", round(pb_val, 2), "。"))
        highlights <- c(highlights, val_method$rationale)

        tmp_html <- tempfile(fileext = ".html")
        rmarkdown::render(
          input = tempReport,
          output_file = basename(tmp_html),
          output_dir = dirname(tmp_html),
          intermediates_dir = tempdir(),
          clean = TRUE,
          quiet = TRUE,
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
            dcf_price = dcf_price,
            ddm_value = ddm_val,
            pb_value = pb_val,
            ri_value = ri_val,
            ev_value = ev_val,
            margin_of_safety = rating_info$margin_of_safety,
            primary_method = val_method$method,
            method_rationale = val_method$rationale,
            primary_bear = if (!is.null(band)) .report_num(band$bear) else NA_real_,
            primary_base = if (!is.null(band)) .report_num(band$base) else rating_anchor,
            primary_bull = if (!is.null(band)) .report_num(band$bull) else NA_real_,
            secondary_point = sec_pt,
            confidence_level = if (is.list(conf)) conf$level else NA_character_,
            confidence_score = if (is.list(conf)) conf$score else NA_real_,
            wacc = wacc_str,
            terminal_growth = paste0(isolate(input$sgr), "%"),
            forecast_years = isolate(input$years),
            dcf_mode = .report_dcf_mode_label(isolate(input$dcf_mode)),
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

        render_report_pdf(tmp_html, file)
        showNotification("✅ PDF 投資意見報告已產出", type = "message")
      }, error = function(e) {
        showNotification(paste("報告生成失敗:", e$message), type = "error", duration = 12)
        # 寫入最小錯誤 PDF 避免下載 handler 空檔
        tryCatch({
          grDevices::pdf(file, width = 8.27, height = 11.69)
          plot.new()
          text(0.5, 0.6, "YNow 報告生成失敗", cex = 1.4)
          text(0.5, 0.45, paste(strwrap(e$message, 60), collapse = "\n"), cex = 0.8)
          grDevices::dev.off()
        }, error = function(e2) NULL)
      })
    }
  )
}
