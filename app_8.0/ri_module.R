# ==========================================
# ri_module.R - 剩餘收益模型 (Residual Income Model)
# 專治：金融股、重資產、負自由現金流但具備龐大帳面淨值的企業
# ==========================================

# ------------------------------------------
# 🖥️ 前端 UI 介面
# ------------------------------------------
ri_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "ri_calculator",
          tabBox(title = "RESIDUAL INCOME (RI) MODEL", width = "auto",
                 
                 # --- 分頁：RI 估值主畫面 ---
                 tabPanel("RI Overview", icon = icon("gem"),
                          
                          fluidRow(
                            div("Residual Income = Net Income - (Equity Capital × Cost of Equity)",
                                style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;")
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
                                   )
                            ),
                            
                            column(width = 12,
                                   box(title = "⚙️ 模型參數假設", width = 12, status = "primary", solidHeader = TRUE,
                                       numericInput(ns("b0"), "每股帳面淨值 (BVPS, B0)", value = NA, step = 0.1),
                                       numericInput(ns("roe"), "預估股東權益報酬率 (ROE) %", value = NA, step = 0.1),
                                       numericInput(ns("ke"), "要求股權報酬率 (Ke) %", value = NA, step = 0.1),
                                       numericInput(ns("payout"), "預估配息率 (Payout Ratio) %", value = NA, min = 0, max = 100, step = 1),
                                       numericInput(ns("years"), "預測期 (Years)", value = 5, min = 1, max = 10),
                                       numericInput(ns("g"), "RI 終端永續成長率 (g) %", value = 0, step = 0.1),
                                       helpText("實務上 RI 終端成長率通常設為 0 或負數 (超額利潤會隨競爭消失)"),
                                       
                                       tags$div(style = "margin-top: 20px;",
                                                actionButton(ns("btn_calc_ri"), "▶ 試算 RI 合理股價", class = "btn-success", icon = icon("calculator"), style="width: 100%; font-weight: bold; font-size: 16px;")
                                       )
                                   )
                            )
                          ),
                          
                          # 下方：明細數據表
                          fluidRow(
                            box(title = "📊 預測期財務明細 (Per Share)", width = 12,
                                tableOutput(ns("tbl_ri_details"))
                            )
                          )
                 )
          )
  )
}

# ------------------------------------------
# ⚙️ 後端 Server 邏輯
# ------------------------------------------
ri_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow, global_re, scraped_shares) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # 🔄 自動從財報同步預設值
    # ==========================================
    observeEvent(d_balance_sheet(), {
      req(d_balance_sheet(), d_income_statement(), scraped_shares())
      
      shares <- scraped_shares()
      
      # 1. 計算 BVPS (每股淨值 B0)
      equity <- select_clean_metric_row(d_balance_sheet(), "Common Stock Equity")[1]
      if (is.na(equity)) equity <- select_clean_metric_row(d_balance_sheet(), "Total Equity Gross Minority Interest")[1]
      
      if (!is.na(equity) && !is.na(shares) && shares > 0) {
        bvps <- equity / shares
        updateNumericInput(session, "b0", value = round(bvps, 2))
      }
      
      # 2. 計算歷史 ROE
      ni <- select_clean_metric_row(d_income_statement(), "Net Income from Continuing & Discontinued Operation")[1]
      if (is.na(ni)) ni <- select_clean_metric_row(d_income_statement(), "Net Income")[1]
      
      if (!is.na(ni) && !is.na(equity) && equity > 0) {
        roe <- (ni / equity) * 100
        # 防呆：避免極端值 (例如庫藏股導致的異常高 ROE)
        roe_safe <- max(-50, min(roe, 50))
        updateNumericInput(session, "roe", value = round(roe_safe, 2))
      }
      
      # 3. 計算歷史配息率
      div_paid_total <- abs(select_clean_metric_row(d_cash_flow(), "Cash Dividends Paid")[1])
      if (!is.na(div_paid_total) && !is.na(ni) && ni > 0) {
        payout <- (div_paid_total / ni) * 100
        payout_safe <- max(0, min(payout, 100))
        updateNumericInput(session, "payout", value = round(payout_safe, 2))
      } else {
        updateNumericInput(session, "payout", value = 0)
      }
    })
    
    # 同步全域的 Ke (CAPM)
    observeEvent(global_re(), {
      req(global_re())
      updateNumericInput(session, "ke", value = round(global_re() * 100, 2))
    })
    
    # ==========================================
    # 🧮 核心運算引擎 (RI Model)
    # ==========================================
    ri_calc <- eventReactive(input$btn_calc_ri, {
      req(input$b0, input$roe, input$ke, input$payout, input$years)
      
      b0 <- input$b0
      roe <- input$roe / 100
      ke <- input$ke / 100
      payout <- input$payout / 100
      g <- input$g / 100
      n <- input$years
      
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
        df$RI[i] <- df$EPS[i] - df$Equity_Charge[i]  # 或者寫作 (roe - ke) * curr_bv
        df$PV_RI[i] <- df$RI[i] / ((1 + ke)^i)
        
        discount_sum <- discount_sum + df$PV_RI[i]
        curr_bv <- curr_bv + df$EPS[i] - df$DPS[i] # 期末淨值
      }
      
      # 終值 (Terminal Value)
      # 假設最後一年的 RI 以 g 成長
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
      
      # 計算累積的 RI 現值，用來畫堆疊圖
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
      
      # 美化表格
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
    
    # 傳出計算結果 (供 Football Field 圖表使用)
    return(list(
      ri_price = reactive({ res <- ri_calc(); if(res$status == "success") res$value else NA })
    ))
  })
}
