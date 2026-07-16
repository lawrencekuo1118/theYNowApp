# ==========================================
# ri_module.R - 剩餘收益模型 (Residual Income Model)
# 專治：金融股、重資產、負自由現金流但具備龐大帳面淨值的企業
# ==========================================

# ==========================================
# 🖥️ 前端 UI 介面 (ri_module_ui)
# ==========================================
ri_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "ri_calculator",
          tabBox(title = "RESIDUAL INCOME", width = "auto",
                 
                 # --- 💎 子分頁 1：RI 估值主畫面 (Overview) ---
                 tabPanel("RI Overview", icon = icon("gem"),
                          
                          fluidRow(
                            div("Residual Income = Net Income - (Equity Capital × Cost of Equity)",
                                style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;")
                          ),
                          
                          # 🌟 補回：執行試算的大按鈕
                          fluidRow(
                            div(style = "text-align: center; margin-bottom: 20px;",
                                actionButton(ns("btn_calc_ri"), "▶ 試算 RI 模型", 
                                             style = "background-color: #27ae60; color: white; font-weight: bold; font-size: 18px; padding: 12px 30px; border-radius: 8px; border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
                            )
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   fluidRow(
                                     uiOutput(ns("ui_ri_result"))
                                   ),
                                   fluidRow(
                                     box(title = "📈 每股帳面淨值 vs 剩餘收益 軌跡圖", width = 12, status = "info",
                                         plotOutput(ns("plt_ri_trajectory"), height = "350px")
                                     )
                                   ),
                                   fluidRow(
                                     box(title = "📊 剩餘收益預測細節", width = 12, status = "primary",
                                         tableOutput(ns("tbl_ri_details"))
                                     )
                                   )
                            )
                          )
                 ),
                 
                 # --- ⚙️ 子分頁 2：模型參數與 B0 設定 (Settings) ---
                 tabPanel("RI Settings", icon = icon("cogs"),
                          
                          # -- B0 估算區 (仿 FCFF UI 風格) --
                          h4(tags$b("🎯 每股帳面淨值 (B0) 估算區")),
                          fluidRow(
                            div("B0 = 普通股股東權益 (Common Equity) ÷ 發行股數 (Shares Outstanding)",
                                style = "font-size: 16px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F8F9F9; border-left: 4px solid #2980B9; border-radius: 4px;")
                          ),
                          fluidRow(
                            column(4, 
                                   numericInput(ns("b0"), "期初每股帳面淨值 B0 (USD)", value = NA, step = 0.5)
                            ),
                            column(8,
                                   br(), # 對齊下推
                                   actionButton(ns("btn_sync_b0"), "從最新財報自動帶入數值", 
                                                icon = icon("sync"), 
                                                class = "btn-sm",
                                                style = "background-color: #2980b9; color: white; border: none; padding: 8px 15px; font-weight: bold; border-radius: 5px; margin-top: 5px;")
                            )
                          ),
                          
                          hr(style = "border-top: 1px solid #BDC3C7;"),
                          
                          # -- 核心參數設定區 --
                          h4(tags$b("⚙️ 模型參數假設")),
                          fluidRow(
                            column(4, numericInput(ns("ri_years"), "預測期 (Years)", value = 5, min = 1, max = 10)),
                            column(4, numericInput(ns("ri_ke"), "股東權益成本 (Ke, %)", value = 8.0, step = 0.1)),
                            column(4, numericInput(ns("ri_g"), "終值永續成長率 (g, %)", value = 2.0, step = 0.1))
                          ),
                          # 🌟 補回：RI 必備的 ROE 與配息率參數
                          fluidRow(
                            column(6, numericInput(ns("ri_roe"), "預期股東權益報酬率 (ROE, %)", value = 15.0, step = 0.1)),
                            column(6, numericInput(ns("ri_payout"), "預期現金配息率 (Payout Ratio, %)", value = 40.0, step = 1))
                          ),
                          fluidRow(
                            column(12,
                                   actionButton(ns("btn_reset_ri_params"), "回復系統預設參數", 
                                                icon = icon("undo"), 
                                                class = "btn-sm",
                                                style = "background-color: #7f8c8d; color: white; border: none; margin-top: 10px;")
                            )
                          )
                 )
          )
  )
}

# ==========================================
# ⚙️ 後端 Server 邏輯
# ==========================================
# 🔴 注意：這裡的參數移除了 scraped_shares，讓模組更獨立
ri_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow, global_re) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # 🔄 自動從財報同步預設值 (初次載入時)
    # ==========================================
    observeEvent(d_balance_sheet(), {
      req(d_balance_sheet(), d_income_statement())
      
      df_bs <- d_balance_sheet()
      
      # 🌟 1. 獨立抓取股數防呆邏輯 (不再依賴 server.R 傳入)
      raw_shares <- select_current_metric(df_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock")
      shares <- if (is.na(raw_shares) || raw_shares <= 0) 1 else raw_shares
      
      equity <- select_current_metric_any(df_bs, EQUITY_PATTERNS, "stock")
      
      if (!is.na(equity) && !is.na(shares) && shares > 0) {
        bvps <- equity / shares
        updateNumericInput(session, "b0", value = round(bvps, 2))
      }
      
      # 3. 計算歷史 ROE
      ni <- select_current_metric_any(d_income_statement(), NET_INCOME_PATTERNS, "flow")
      
      if (!is.na(ni) && !is.na(equity) && equity > 0) {
        roe <- (ni / equity) * 100
        roe_safe <- max(-50, min(roe, 50))
        updateNumericInput(session, "ri_roe", value = round(roe_safe, 2))
      }
      
      # 4. 計算歷史配息率
      div_paid_total <- abs(select_current_metric(d_cash_flow(), "Cash Dividends Paid", "flow"))
      if (!is.na(div_paid_total) && !is.na(ni) && ni > 0) {
        payout <- (div_paid_total / ni) * 100
        payout_safe <- max(0, min(payout, 100))
        updateNumericInput(session, "ri_payout", value = round(payout_safe, 2))
      } else {
        updateNumericInput(session, "ri_payout", value = 0)
      }
    })
    
    # ==========================================
    # 🔘 按鈕邏輯：手動從財報再次同步 B0
    # ==========================================
    observeEvent(input$btn_sync_b0, {
      req(d_balance_sheet())
      df_bs <- d_balance_sheet()
      
      # 🌟 同樣使用獨立抓取邏輯
      raw_shares <- select_current_metric(df_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock")
      shares <- if (is.na(raw_shares) || raw_shares <= 0) 1 else raw_shares
      
      equity <- select_current_metric_any(df_bs, EQUITY_PATTERNS, "stock")
      
      if (!is.na(equity) && !is.na(shares) && shares > 0) {
        calc_b0 <- round(equity / shares, 2)
        updateNumericInput(session, "b0", value = calc_b0)
        showNotification(paste("✅ 已成功從資產負債表更新 B0 為 $", calc_b0), type = "message")
      } else {
        showNotification("⚠️ 無法從當前財報讀取完整 B0 所需欄位", type = "error")
      }
    })
    
    # 🔘 按鈕邏輯：重設所有 RI 參數
    observeEvent(input$btn_reset_ri_params, {
      updateNumericInput(session, "ri_years", value = 5)
      updateNumericInput(session, "ri_g", value = 2.0)
      if (!is.null(global_re())) {
        updateNumericInput(session, "ri_ke", value = round(global_re() * 100, 2))
      }
      showNotification("🔁 已重設為系統預設參數", type = "message")
    })
    
    # 同步中央大腦的全域 Ke (來自 WACC/CAPM)
    observeEvent(global_re(), {
      req(global_re())
      updateNumericInput(session, "ri_ke", value = round(global_re() * 100, 2))
    })
    
    # ==========================================
    # 🧮 核心運算引擎 (RI Model)
    # ==========================================
    ri_calc <- eventReactive(input$btn_calc_ri, {
      req(input$b0, input$ri_roe, input$ri_ke, input$ri_payout, input$ri_years, input$ri_g)
      
      b0 <- input$b0
      roe <- input$ri_roe / 100
      ke <- input$ri_ke / 100
      payout <- input$ri_payout / 100
      g <- input$ri_g / 100
      n <- input$ri_years
      
      if (ke <= g) {
        return(list(status = "error", message = "⚠️ 計算無效：要求股權報酬率 (Ke) 必須嚴格大於終端成長率 (g)！"))
      }
      
      df <- data.frame(
        Year = 1:n,
        Beg_BVPS = numeric(n),
        EPS = numeric(n),
        DPS = numeric(n),
        Equity_Charge = numeric(n),
        RI = numeric(n),
        PV_RI = numeric(n)
      )
      
      curr_bv <- b0
      discount_sum <- 0
      
      # 逐年推算 (Per Share 基礎)
      for (i in 1:n) {
        df$Beg_BVPS[i] <- curr_bv
        df$EPS[i] <- curr_bv * roe
        df$DPS[i] <- df$EPS[i] * payout
        df$Equity_Charge[i] <- curr_bv * ke
        df$RI[i] <- df$EPS[i] - df$Equity_Charge[i] 
        df$PV_RI[i] <- df$RI[i] / ((1 + ke)^i)
        
        discount_sum <- discount_sum + df$PV_RI[i]
        curr_bv <- curr_bv + df$EPS[i] - df$DPS[i] # 期末淨值 = 期初 + EPS - 股利
      }
      
      # 終值 (Terminal Value) 假設最後一年的 RI 以 g 成長
      tv_ri <- (df$RI[n] * (1 + g)) / (ke - g)
      pv_tv_ri <- tv_ri / ((1 + ke)^n)
      
      # 企業內在價值 = B0 + 預測期 RI 現值加總 + 終端 RI 現值
      intrinsic_value <- b0 + discount_sum + pv_tv_ri
      
      return(list(
        status = "success", 
        value = intrinsic_value, 
        b0 = b0,
        pv_ri = discount_sum,
        pv_tv = pv_tv_ri,
        df = df
      ))
    }, ignoreNULL = FALSE)
    
    # ==========================================
    # 📊 UI 輸出渲染
    # ==========================================
    output$ui_ri_result <- renderUI({
      res <- ri_calc()
      if (res$status == "error") {
        div(style = "color: #d9534f; font-weight: bold; padding: 15px; background-color: #fdf2f2; border-left: 5px solid #d9534f; border-radius: 4px;", 
            icon("exclamation-triangle"), " ", res$message)
      } else {
        # 判斷是在創造價值還是毀滅價值
        value_creation <- res$pv_ri + res$pv_tv
        color <- if(value_creation >= 0) "#00a65a" else "#d9534f"
        sign_txt <- if(value_creation >= 0) "+" else ""
        
        div(style = "display: flex; justify-content: space-between; align-items: center; padding: 20px; background-color: #fcfcfc; border: 1px solid #ddd; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
            div(style = "text-align: center;",
                p(style = "font-size: 14px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "當前每股淨值 (B0)"),
                p(style = "font-size: 24px; color: #2c3e50; font-weight: bold; margin: 0;", paste0("$", round(res$b0, 2)))
            ),
            div(style = "text-align: center;",
                p(style = "font-size: 20px; color: #95a5a6; margin: 0;", "+")
            ),
            div(style = "text-align: center;",
                p(style = "font-size: 14px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "超額利潤現值 PV(RI)"),
                p(style = paste0("font-size: 24px; font-weight: bold; margin: 0; color: ", color, ";"), 
                  paste0(sign_txt, "$", round(value_creation, 2)))
            ),
            div(style = "text-align: center;",
                p(style = "font-size: 20px; color: #95a5a6; margin: 0;", "=")
            ),
            div(style = "text-align: center; background-color: #e8f8f5; padding: 15px; border-radius: 8px; border-left: 4px solid #1abc9c;",
                p(style = "font-size: 14px; color: #16a085; margin-bottom: 5px; font-weight: bold; text-transform: uppercase;", "RI 推估每股合理價"),
                p(style = "font-size: 32px; color: #1abc9c; font-weight: bold; margin: 0;", paste0("$", round(res$value, 2)))
            )
        )
      }
    })
    
    output$plt_ri_trajectory <- renderPlot({
      res <- ri_calc()
      req(res$status == "success")
      df <- res$df
      
      df$Cum_PV_RI <- cumsum(df$PV_RI)
      df$Intrinsic_Path <- res$b0 + df$Cum_PV_RI
      
      ggplot(df, aes(x = as.factor(Year))) +
        geom_bar(aes(y = Intrinsic_Path, fill = "累計企業價值 (B0 + PV of RI)"), stat = "identity", alpha = 0.7) +
        geom_hline(yintercept = res$b0, linetype = "dashed", color = "#34495e", linewidth = 1.2) +
        geom_text(aes(x = 1.5, y = res$b0, label = paste("期初帳面淨值 B0: $", round(res$b0, 2))), vjust = -1, color = "#34495e", fontface = "bold") +
        geom_point(aes(y = Intrinsic_Path), size = 3, color = "#2980b9") +
        geom_line(aes(y = Intrinsic_Path, group = 1), color = "#2980b9", linewidth = 1) +
        scale_fill_manual(name = "", values = c("累計企業價值 (B0 + PV of RI)" = "#aed6f1")) +
        theme_minimal(base_size = 14) +
        labs(x = "預測年份", y = "每股價值 (USD)") +
        theme(legend.position = "bottom")
    })
    
    output$tbl_ri_details <- renderTable({
      res <- ri_calc()
      req(res$status == "success")
      df <- res$df
      
      out_df <- data.frame(
        "預測年 (Year)" = df$Year,
        "期初淨值 (Beg BV)" = sprintf("$%.2f", df$Beg_BVPS),
        "預估 EPS" = sprintf("$%.2f", df$EPS),
        "預估 DPS" = sprintf("$%.2f", df$DPS),
        "股權資本成本 (Ke × BV)" = sprintf("$%.2f", df$Equity_Charge),
        "剩餘收益 (RI)" = sprintf("$%.2f", df$RI),
        "折現後 RI (PV)" = sprintf("$%.2f", df$PV_RI)
      )
      return(out_df)
    }, align = 'c', striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # 傳出計算結果
    return(list(
      ri_price = reactive({ res <- ri_calc(); if(res$status == "success") res$value else NA })
    ))
  })
}
