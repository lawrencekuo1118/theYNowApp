# ==========================================
# ddm_module.R - 股利折現模型 (DDM) 後端模組
# ==========================================

ddm_module_server <- function(id, ddm_g = reactive(NULL), ddm_ke = reactive(NULL), 
                              scraped_d0 = reactive(NULL), 
                              summary_df = reactive(NULL), 
                              d_cash_flow = reactive(NULL), 
                              d_balance_sheet = reactive(NULL),
                              d_income_statement = reactive(NULL)) { # 🌟 新增損益表參數
  
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # 🔄 接收主畫面傳來的變數
    # ==========================================
    observeEvent(scraped_d0(), {
      val <- scraped_d0()
      # 🌟 強化防呆：確保傳入的是「單一數字」才更新
      if (is.numeric(val) && length(val) == 1 && !is.na(val) && val >= 0) {
        updateNumericInput(session, "d0", value = val)
      }
    })
    
    observeEvent(ddm_g(), {
      req(ddm_g())
      if (is.null(input$g) || abs(ddm_g() - input$g) > 1e-4) {
        updateNumericInput(session, "g", value = ddm_g())
      }
    })
    
    observeEvent(ddm_ke(), {
      req(ddm_ke())
      if (is.null(input$ke) || abs(ddm_ke() - input$ke) > 1e-4) {
        updateNumericInput(session, "ke", value = ddm_ke())
      }
    })
    
    # ==========================================
    # 🌟 核心引擎：從當前財報「動態計算」預設值與基本面 g
    # ==========================================
    sync_ddm_to_financials <- function() {
      req(d_cash_flow(), d_balance_sheet())
      
      # --- 1. 計算動態 D0 (最近一期總發放股利 / 總發行股數) ---
      div_paid_total <- abs(select_clean_metric_row(d_cash_flow(), "Cash Dividends Paid")[1])
      shares_issued  <- as.numeric(select_clean_metric_row(d_balance_sheet(), "Share Issued")[1])
      
      if (!is.na(div_paid_total) && !is.na(shares_issued) && shares_issued > 0) {
        dynamic_d0 <- div_paid_total / shares_issued
        updateNumericInput(session, "d0", value = round(dynamic_d0, 2))
      } else {
        fallback_d0 <- scraped_d0()
        if (is.numeric(fallback_d0) && length(fallback_d0) == 1 && !is.na(fallback_d0)) {
          updateNumericInput(session, "d0", value = fallback_d0)
        } else {
          # 🌟 關鍵修復：如果財報跟爬蟲都找不到股利，代表「該公司不配息」，強制填入 0
          updateNumericInput(session, "d0", value = 0)
          showNotification("ℹ️ 系統偵測到該公司未發放股利，D0 自動設為 0", type = "warning", duration = 5)
        }
      }
      
      # --- 2. 計算基本面隱含成長率（Fundamental Growth） g = ROE × 保留盈餘率 (Retention Ratio) ---
      # 確保有成功接收到損益表資料
      if (!is.null(d_income_statement) && !is.null(d_income_statement())) {
        
        ni <- select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation")[1]
        if (is.na(ni)) ni <- select_clean_metric_row(d_income_statement(), "Net Income")[1]
        
        equity <- select_clean_metric_row(d_balance_sheet(), "Common Stock Equity")[1]
        if (is.na(equity)) equity <- select_clean_metric_row(d_balance_sheet(), "Total Equity Gross Minority Interest")[1]
        
        if (!is.na(ni) && !is.na(equity) && equity > 0) {
          # 步驟 A: 計算 ROE
          roe <- ni / equity
          
          # 步驟 B: 計算配息率 (Payout Ratio)
          # 防呆：只有在有獲利 (ni > 0) 且有發股利的情況下計算，最高限制在 1 (避免超額配息致使 g 變負)
          payout_ratio <- 0
          if (ni > 0 && !is.na(div_paid_total)) {
            payout_ratio <- div_paid_total / ni
            payout_ratio <- min(max(payout_ratio, 0), 1) 
          }
          
          ## 步驟 C: 計算保留盈餘率 (Retention Ratio)
          retention_ratio <- 1 - payout_ratio
          
          # 步驟 D: 算出 Fundamental g
          fund_g <- roe * retention_ratio
          fund_g_pct <- round(fund_g * 100, 2)
          
          # 🌟 聽從專業建議：移除硬性極端值裁切，讓模型誠實呈現基本面真實的 g
          # 讓高成長率去觸發 ddm_calc 內部的 (Ke <= g) 報錯，藉此提醒使用者改用兩階段模型
          updateNumericInput(session, "g", value = fund_g_pct)
        } else if (!is.null(ddm_g())) {
          updateNumericInput(session, "g", value = ddm_g())
        }
      } else if (!is.null(ddm_g())) {
        updateNumericInput(session, "g", value = ddm_g())
      }
      
      # --- 3. 同步折現率 (ke) ---
      if (!is.null(ddm_ke())) updateNumericInput(session, "ke", value = ddm_ke())
    }
    
    # 🚀 自動連動：當主畫面搜尋完新股票、財報更新的瞬間，自動灌入數值！
    observeEvent(d_cash_flow(), {
      sync_ddm_to_financials()
    })
    
    # 🔄 手動重置：使用者在沙盒玩壞參數後，點擊 Reset 按鈕的行為
    observeEvent(input$reset_ddm, {
      sync_ddm_to_financials()
      showNotification("🔁 DDM 模型參數已依據「最新財報數據」重新計算並回復", type = "message")
    })
    
    # ==========================================
    # 🧮 DDM 核心計算邏輯
    # ==========================================
    ddm_calc <- eventReactive(input$btn_calc_ddm, {
      req(input$d0, input$g, input$ke)
      d0 <- input$d0; g_dec <- input$g / 100; ke_dec <- input$ke / 100
      
      if (ke_dec <= g_dec) {
        return(list(status = "error", message = "⚠️ 計算無效：要求報酬率 (Ke) 必須嚴格大於基本面隱含成長率！"))
      }
      
      d1 <- d0 * (1 + g_dec)
      p0 <- d1 / (ke_dec - g_dec)
      return(list(status = "success", value = round(p0, 2), d1 = round(d1, 2)))
    }, ignoreNULL = FALSE) 
    
    output$ui_ddm_result <- renderUI({
      res <- ddm_calc()
      if (res$status == "error") {
        div(style = "color: #d9534f; font-weight: bold; padding: 10px; background-color: #fdf2f2; border-left: 4px solid #d9534f;", res$message)
      } else {
        div(style = "font-size: 32px; font-weight: bold; color: #2C3E50; text-align: center; padding: 20px; background-color: #ECF0F1; border-radius: 10px;",
            p(style = "font-size: 16px; color: #7F8C8D; margin-bottom: 5px;", "DDM 推估每股合理價"),
            paste0("$", res$value),
            p(style = "font-size: 14px; color: #95A5A6; margin-top: 10px;", paste0("預估明年股利 (D1): $", res$d1))
        )
      }
    })
    
    # ==========================================
    # ⚙️ D0 Settings 分頁邏輯
    # ==========================================
    output$ibx_d0_scraped <- renderInfoBox({
      val <- if(is.na(scraped_d0()) || is.null(scraped_d0())) "N/A" else paste0("$", scraped_d0())
      infoBox("財報最新股利 (D0)", val, icon = icon("money-bill-wave"), color = "blue", fill = TRUE)
    })
    
    output$ibx_d0_eps <- renderInfoBox({
      df <- summary_df()
      val <- if(!is.null(df) && "EPS (TTM)" %in% df$Item) df$Value[df$Item == "EPS (TTM)"] else "N/A"
      infoBox("近四季 EPS (TTM)", val, icon = icon("chart-bar"), color = "green", fill = TRUE)
    })
    
    output$ibx_d0_payout <- renderInfoBox({
      d0 <- scraped_d0()
      df <- summary_df()
      eps_str <- if(!is.null(df) && "EPS (TTM)" %in% df$Item) df$Value[df$Item == "EPS (TTM)"] else NA
      eps_val <- suppressWarnings(as.numeric(eps_str))
      
      payout <- if(!is.na(d0) && !is.na(eps_val) && eps_val > 0) round((d0 / eps_val) * 100, 2) else NA
      val <- if(is.na(payout)) "N/A" else paste0(payout, "%")
      infoBox("當前配息率", val, icon = icon("percent"), color = "purple", fill = TRUE)
    })
    
    # 🌟 關鍵修復：使用模糊搜尋抓 EPS，並在找不到時補 0
    observeEvent(summary_df(), {
      df <- summary_df()
      req(df)
      
      # 使用 grepl 模糊匹配任何包含 EPS 的欄位 (避免 Yahoo 偷改標籤名稱)
      eps_row <- df[grepl("EPS", df$Item, ignore.case = TRUE), ]
      
      if (nrow(eps_row) > 0) {
        # 用正則表達式把數字 (包含負號與小數點) 抽出來
        eps_val <- suppressWarnings(as.numeric(stringr::str_extract(eps_row$Value[1], "^[-0-9.]+")))
        
        if (!is.na(eps_val)) {
          updateNumericInput(session, "est_eps", value = eps_val)
        } else {
          updateNumericInput(session, "est_eps", value = 0)
        }
      } else {
        updateNumericInput(session, "est_eps", value = 0)
      }
    })
    
    observeEvent(input$calc_d0_payout, {
      req(input$est_eps, input$est_payout)
      new_d0 <- input$est_eps * (input$est_payout / 100)
      updateNumericInput(session, "d0", value = round(new_d0, 2))
      output$txt_d0_payout_res <- renderUI({ HTML(glue::glue("<div style='color: #00a65a; font-weight: bold;'>✅ 已成功將 D0 更新為 ${round(new_d0, 2)}</div>")) })
      showNotification("🎯 D0 已依目標配息率更新，請回 Overview 重新試算", type = "message")
    })
    
    observeEvent(input$calc_d0_average, {
      req(d_cash_flow(), d_balance_sheet(), input$cycle_years)
      div_paid_seq <- select_clean_metric_row(d_cash_flow(), "Cash Dividends Paid")
      shares <- as.numeric(select_clean_metric_row(d_balance_sheet(), "Share Issued")[1])
      
      if (length(div_paid_seq) == 0 || all(is.na(div_paid_seq)) || is.na(shares) || shares <= 0) {
        showNotification("⚠️ 無法取得足夠的歷史股利或股數資料", type = "error"); return()
      }
      
      n_years <- min(input$cycle_years, length(div_paid_seq))
      valid_divs <- abs(na.omit(div_paid_seq[1:n_years]))
      if (length(valid_divs) == 0) return()
      
      new_d0 <- mean(valid_divs) / shares
      updateNumericInput(session, "d0", value = round(new_d0, 2))
      output$txt_d0_avg_res <- renderUI({ HTML(glue::glue("<div style='color: #00a65a; font-weight: bold;'>✅ 已成功將 D0 更新為 ${round(new_d0, 2)} (過去 {n_years} 年平均)</div>")) })
      showNotification("🎯 D0 已依歷史平均更新，請回 Overview 重新試算", type = "message")
    })
    
    # 傳出計算結果
    return(list(
      ddm_price = reactive({ res <- ddm_calc(); if(res$status == "success") res$value else NA })
    ))
  })
}
