# ==========================================
# ddm_module.R - 股利折現模型 (DDM) 後端模組
# ==========================================

ddm_module_server <- function(id, ddm_g = reactive(NULL), ddm_ke = reactive(NULL), 
                              scraped_d0 = reactive(NULL), 
                              summary_df = reactive(NULL), 
                              d_cash_flow = reactive(NULL), 
                              d_balance_sheet = reactive(NULL),
                              d_income_statement = reactive(NULL)) {
  
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # 接收主畫面傳來的變數
    # ==========================================
    observeEvent(scraped_d0(), {
      val <- scraped_d0()
      if (is.numeric(val) && length(val) == 1 && !is.na(val) && val >= 0) {
        updateNumericInput(session, "d0", value = val)
      }
    })
    
    # 僅在「與中央 SGR 同步」時覆寫股利成長率 g
    observeEvent(list(ddm_g(), input$sync_g), {
      if (!isTRUE(input$sync_g)) return()
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
    # 從當前財報動態計算 D0；g／Ke 依中央設定
    # ==========================================
    sync_ddm_to_financials <- function() {
      req(d_cash_flow(), d_balance_sheet())
      
      # --- 1. 動態 D0 = 最近一期總發放股利 / 流通股數（偏好序別名）---
      div_paid_total <- abs(select_current_metric(d_cash_flow(), "Cash Dividends Paid", "flow"))
      shares_issued <- select_current_metric_any(d_balance_sheet(), SHARE_PATTERNS, "stock")
      
      if (!is.na(div_paid_total) && !is.na(shares_issued) && shares_issued > 0) {
        dynamic_d0 <- div_paid_total / shares_issued
        updateNumericInput(session, "d0", value = round(dynamic_d0, 2))
      } else {
        fallback_d0 <- scraped_d0()
        if (is.numeric(fallback_d0) && length(fallback_d0) == 1 && !is.na(fallback_d0)) {
          updateNumericInput(session, "d0", value = fallback_d0)
        } else {
          updateNumericInput(session, "d0", value = 0)
          showNotification("系統偵測到該公司未發放股利，D0 自動設為 0", type = "warning", duration = 5)
        }
      }
      
      # --- 2. 股利 g：僅同步模式才套用中央值 ---
      if (isTRUE(input$sync_g) &&
          !is.null(ddm_g()) && is.numeric(ddm_g()) && length(ddm_g()) == 1 && !is.na(ddm_g())) {
        updateNumericInput(session, "g", value = round(as.numeric(ddm_g()), 2))
      }
      
      # --- 3. 同步折現率 (ke) ---
      if (!is.null(ddm_ke())) updateNumericInput(session, "ke", value = ddm_ke())
    }
    
    observeEvent(d_cash_flow(), {
      sync_ddm_to_financials()
    })
    
    observeEvent(input$reset_ddm, {
      updateCheckboxInput(session, "sync_g", value = TRUE)
      sync_ddm_to_financials()
      showNotification("DDM 模型參數已依據最新財報與中央設定回復", type = "message")
    })
    
    # ==========================================
    # DDM 核心：P0 = D1 / (Ke - g), D1 = D0(1+g)
    # ==========================================
    ddm_calc <- eventReactive(input$btn_calc_ddm, {
      req(input$d0, input$g, input$ke)
      d0 <- input$d0; g_dec <- input$g / 100; ke_dec <- input$ke / 100
      
      if (ke_dec <= g_dec) {
        return(list(status = "error", message = "計算無效：要求報酬率 (Ke) 必須嚴格大於股利成長率 g！"))
      }
      
      d1 <- d0 * (1 + g_dec)
      p0 <- d1 / (ke_dec - g_dec)
      
      return(list(status = "success", value = round(p0, 2), d1 = round(d1, 2)))
    })
    
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
    # D0 Settings
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
    
    observeEvent(summary_df(), {
      df <- summary_df()
      req(df)
      
      eps_row <- df[grepl("EPS", df$Item, ignore.case = TRUE), ]
      
      if (nrow(eps_row) > 0) {
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
      output$txt_d0_payout_res <- renderUI({ HTML(glue::glue("<div style='color: #00a65a; font-weight: bold;'>已成功將 D0 更新為 ${round(new_d0, 2)}</div>")) })
      showNotification("D0 已依目標配息率更新，請回 Overview 重新試算", type = "message")
    })
    
    observeEvent(input$calc_d0_average, {
      req(d_cash_flow(), d_balance_sheet(), input$cycle_years)
      
      div_paid_seq <- select_clean_metric_row(d_cash_flow(), "Cash Dividends Paid", include_ttm = FALSE)
      shares <- select_current_metric_any(d_balance_sheet(), SHARE_PATTERNS, "stock")
      
      n_years <- min(input$cycle_years, length(div_paid_seq))
      valid_divs <- abs(na.omit(div_paid_seq[1:n_years]))
      
      avg_div <- mean(valid_divs)
      new_d0 <- if (!is.na(shares) && shares > 0) avg_div / shares else NA_real_
      if (is.na(new_d0)) {
        showNotification("無法計算平均 D0：股數資料不足", type = "error")
        return()
      }
      ui_text <- paste0("$", round(new_d0, 2))
      
      updateNumericInput(session, "d0", value = round(new_d0, 2))
      output$txt_d0_avg_res <- renderUI({ HTML(glue::glue("<div style='color: #00a65a; font-weight: bold;'>已成功將 D0 更新為 {ui_text}</div>")) })
      showNotification("D0 已依歷史平均更新，請回 Overview 重新試算", type = "message")
    })
    
    return(list(
      ddm_price = reactive({ res <- ddm_calc(); if(res$status == "success") res$value else NA })
    ))
  })
}
