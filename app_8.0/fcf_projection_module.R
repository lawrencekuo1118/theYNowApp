# ==========================================
# fcf_projection_module.R - 自由現金流 (FCF) 預測模組
# ==========================================

# ==========================================
# 🖥️ 前端 UI 介面
# ==========================================
fcf_projection_module_ui <- function(id) {
  # 建立 Namespace 函數
  ns <- NS(id) 
  
  tabPanel("FCFF", 
           icon = icon("seedling"),
           
           fluidRow(
             column(4, valueBoxOutput(ns("vbx_est_ocf"), width = 12)),
             column(4, valueBoxOutput(ns("vbx_est_fcf"), width = 12)),
             column(4, valueBoxOutput(ns("vbx_est_g_fund"), width = 12)) 
           ),
           
           fluidRow(
             div("FCFF = NOPAT + D&A - ΔNWC - CapEx",
                 style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;")
             ),   
           br(),
           
           fluidRow(
             column(12,
                    actionButton(ns("btn_sync_fcf_params"), "從最新財報自動帶入數值", 
                                 icon = icon("sync"), 
                                 class = "btn-sm",
                                 style = "background-color: #222222; color: #ffffff; border: 1px solid #555555; font-size: 12px; padding: 4px 12px; border-radius: 4px;")
             )
           ),
           br(),
           
           fluidRow(
             # 營收輸入框
             column(4, numericInput(ns("fcf_revenue"), "當期營收 (Revenue)", value = 100)),
             column(4, numericInput(ns("fcf_nopat"), "稅後營業利潤 (NOPAT)", value = 0)),
             column(4, numericInput(ns("fcf_depreciation"), "折舊與攤銷 (D&A) [+]", value = 0))
           ),
           fluidRow(
             column(4, numericInput(ns("fcf_delta_nwc"), "營運資金變動 (ΔNWC) [-]", value = 0)),
             column(4, numericInput(ns("fcf_capex"), "資本支出 (CapEx) [-]", value = 0)),
             column(4, numericInput(ns("fcf_invested_capital"), "總投入資本", value = 1))
           ),
           
           # 🌟 修正：改用 paste0 串接 Namespace
           conditionalPanel(
             condition = paste0("input['", ns("g_growth_method"), "'] == 'fundamental'"),
             checkboxInput(ns("apply_g_ceiling"), 
                           tags$span(style = "color: #d35400; font-weight: bold;", "🔒 啟用 25% 成長率天花板防呆 (建議)"), 
                           value = TRUE)
           ),
           br(),

           actionButton(ns("btn_apply_g_to_dcf"), "將此成長率 (g) 套用至 FCFF 模型", 
                        icon = icon("check"), class = "btn-success"),
           
           div(style = "background-color: #f9f9f9; padding: 15px; border-left: 4px solid #00a65a; margin-bottom: 20px;",
               h4(tags$b("📝 自由現金流 (FCF) 參數拆解")),
               p("您可以點擊「同步按鈕」帶入最新財報，或手動微調參數來模擬不同的營運情境，藉此還原 FCF 與基本面永續成長率 (g) 的計算過程。"),
               
               # 縮寫對照表 (使用灰色小字，不搶視覺焦點)
               p(style = "font-size: 13px; color: #7f8c8d; margin-top: 5px; margin-bottom: 0px;", 
                 "※ 符號說明：NI (稅後淨利), D&A (折舊與攤銷), ΔNWC (營運資金變動), CapEx (資本支出)")
           ),
           
           fluidRow(
             column(6, 
                    selectInput("g_growth_method", "預估FCFF成長率",
                                   choices = c(
                                     "基本面 (Fundamental)" = "fundamental",
                                     "CAGR" = "cagr", 
                                     "平均數" = "mean", 
                                     "中位數" = "median",
                                     "自訂" = "custom"),
                                   selected = APP_DEFAULTS$g_growth_method)
             ),
             column(6, 
                    # 當選擇「自訂(custom)」時，顯示數字輸入框
                    conditionalPanel(
                      condition = "input.g_growth_method == 'custom'",
                      numericInput("custom_g", "自訂營收成長率 (%)", value = APP_DEFAULTS$custom_g)
                    ),
                    
                    # 當選擇「非自訂」的其他方法時，顯示系統計算出的數值
                    conditionalPanel(
                      condition = "input.g_growth_method != 'custom'",
                      # 加上 margin-top 讓文字與左側的下拉選單高度對齊
                      tags$div(style = "margin-top: 25px; padding-left: 10px;", 
                               uiOutput("g_result_display")
                      )
                    )
             )
           ),
           
           # 動態預測表與圖表展示區
           uiOutput(ns("title_dynamic_years")),
           
           # 即時缺值警告橫幅的顯示區塊
           uiOutput(ns("alert_missing_values")),
           
           # 🌟 修正後的 UI 表格區塊
           fluidRow(
             column(12, 
                    div(style = "overflow-x: auto; background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                        DT::dataTableOutput(ns("tbl_fcf_projection"))
                    )
             )
           )
  )
}

# ==========================================
# ⚙️ 後端 Server 邏輯
# ==========================================
fcf_projection_module_server <- function(
    id,
    d_income_statement,
    d_balance_sheet,
    d_cash_flow,
    calc_trigger,
    input_mode       = reactive(NULL),
    input_years,
    sgr              = reactive(NULL),
    g_stage1         = reactive(NULL),
    g_stage2         = sgr,
    yr_stage1        = reactive(NULL),
    input_capex_rate = reactive(NA), 
    input_nwc_rate   = reactive(NA),
    input_manual_fcf = reactive(NULL),
    global_est_g     = reactive(NULL)
) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 🧮 輔助函數與萃取參數邏輯
    estimate_historical_growth <- function(x) {
      x <- na.omit(x)
      if (length(x) < 2) return(5)
      g <- diff(x) / head(x, -1)
      round(mean(g, na.rm = TRUE) * 100, 2)
    }
    
    extract_dcf_parameters <- function(df_is, df_cf, df_bs) {
      revenue    <- select_clean_metric_row(df_is, "Total Revenue")
      op_income  <- select_clean_metric_row(df_is, "Operating Income")
      income_tax <- select_clean_metric_row(df_is, "Income Tax Expense")
      pre_tax    <- select_clean_metric_row(df_is, "Pretax Income")
      capex <- select_clean_metric_row(df_cf, "Capital Expenditure")
      if (all(is.na(capex))) capex <- select_clean_metric_row(df_cf, "Capital Expenditures")
      
      depre <- select_clean_metric_row(df_cf, "Depreciation")
      depre_val <- if(is.na(depre[1])) 0 else depre
      
      curr_assets <- select_clean_metric_row(df_bs, "Current Assets")
      cash        <- select_clean_metric_row(df_bs, "Cash And Cash Equivalents")
      curr_liab   <- select_clean_metric_row(df_bs, "Current Liabilities")
      st_debt     <- select_clean_metric_row(df_bs, "Current Debt")
      if (all(is.na(st_debt))) st_debt <- rep(0, length(curr_liab))
      
      non_cash_ca <- curr_assets - cash
      non_debt_cl <- curr_liab - st_debt
      nwc <- non_cash_ca - non_debt_cl
      
      tax_rate <- ifelse(is.na(pre_tax) | pre_tax == 0, 0.21, income_tax / pre_tax)
      tax_rate <- pmax(0, pmin(tax_rate, 0.5)) 

      net_capex <- abs(capex) - depre_val
      net_capex_margin <- net_capex / revenue
      
      # 🌟 修正：因為資料是 [2023, 2022, 2021]，正確的當期變動應該是 (當期 - 前期)
      # 即 nwc[1:(n-1)] - nwc[2:n]
      delta_nwc <- c(nwc[-length(nwc)] - nwc[-1], NA) 
      delta_rev <- c(revenue[-length(revenue)] - revenue[-1], NA)
      
      nwc_margin <- ifelse(is.na(delta_rev) | delta_rev == 0, 0, delta_nwc / delta_rev)
      
      calc_tax <- mean(tax_rate, na.rm = TRUE)
      calc_net_capex <- mean(net_capex_margin[1:2], na.rm = TRUE) 
      calc_nwc <- mean(nwc_margin[1:2], na.rm = TRUE)
      
      return(list(
        tax_rate         = if(is.nan(calc_tax)) 0.21 else calc_tax,
        net_capex_margin = if(is.nan(calc_net_capex)) 0 else calc_net_capex, 
        nwc_margin       = if(is.nan(calc_nwc)) 0 else calc_nwc
      ))
    }
    
    hist_params <- reactiveVal(list(capex_rate = NA, nwc_rate = NA))
    
    # 主模型的 FCF 運算
    fcf_calc <- eventReactive(calc_trigger(), {
      df_is <- d_income_statement()
      df_cf <- d_cash_flow()
      df_bs <- d_balance_sheet()
      req(nrow(df_is) > 0, nrow(df_cf) > 0, nrow(df_bs) > 0)
      
      latest_rev <- select_clean_metric_row(df_is, "Total Revenue")[1]
      latest_op  <- select_clean_metric_row(df_is, "Operating Income")[1]
      op_margin  <- latest_op / latest_rev
      latest_fcf <- select_clean_metric_row(df_cf, "Free Cash Flow")[1]
      
      if (!is.null(input_manual_fcf()) && !is.na(input_manual_fcf())) {
        latest_fcf <- input_manual_fcf()
      }
      
      params <- extract_dcf_parameters(df_is, df_cf, df_bs)
      final_capex_rate <- if (!is.na(input_capex_rate())) input_capex_rate() / 100 else params$net_capex_margin
      final_nwc_rate   <- if (!is.na(input_nwc_rate())) input_nwc_rate() / 100 else params$nwc_margin
      hist_params(list(capex_rate = params$net_capex_margin, nwc_rate = params$nwc_margin))
      
      n_years <- as.numeric(input_years())
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      
      proj_df <- data.frame(
        Year      = (current_year + 1):(current_year + n_years),
        Revenue   = NA, EBIT = NA, NOPAT = NA, CapEx = NA,
        Delta_NWC = NA, FCF = NA, Type = character(n_years),
        stringsAsFactors = FALSE
      )

      # 🌟 核心修正：徹底與 SGR (終值永續成長率) 脫鉤！
      # 未來 1~5 年的 FCFF 推算，一律只聽從短期的「預估營收成長率 (EST.G)」
      g_rev_rate <- final_sim_g() / 100
      
      for(i in 1:n_years) {
        if (i == 1) {
          proj_df$Revenue[i] <- latest_rev * (1 + g_rev_rate)
        } else {
          proj_df$Revenue[i] <- proj_df$Revenue[i-1] * (1 + g_rev_rate)
        }
        proj_df$Type[i] <- "Projected (預測期)"
      }
      
      proj_df$EBIT <- proj_df$Revenue * op_margin
      proj_df$NOPAT <- proj_df$EBIT * (1 - params$tax_rate)
      proj_df$CapEx <- proj_df$Revenue * final_capex_rate
      delta_rev_proj <- c(proj_df$Revenue[1] - latest_rev, diff(proj_df$Revenue))
      proj_df$Delta_NWC <- delta_rev_proj * final_nwc_rate
      proj_df$FCF <- proj_df$NOPAT - proj_df$CapEx - proj_df$Delta_NWC
      
      hist_df <- data.frame(
        Year = current_year, Revenue = latest_rev, EBIT = latest_op,
        NOPAT = latest_op * (1 - params$tax_rate), CapEx = latest_rev * final_capex_rate,
        Delta_NWC = 0, FCF = latest_fcf, Type = "Actual (最新財報)",
        stringsAsFactors = FALSE
      )
      
      final_df <- rbind(hist_df, proj_df)
      return(final_df)
    })
    
    # 產生 DCF 主畫面的趨勢圖
    fcf_plot_obj <- reactive({
      df_plot <- fcf_calc()
      if (is.null(df_plot) || nrow(df_plot) < 2) return(NULL)
      df_plot <- df_plot[!is.na(df_plot$FCF), ]
      
      ggplot(df_plot, aes(x = Year, y = FCF, fill = Type)) +
        geom_bar(stat = "identity", alpha = 0.6, width = 0.5) +
        geom_line(color = "gray50", linewidth = 1.2, group = 1) +
        geom_point(aes(color = Type), size = 3) +
        geom_text(aes(label = formatC(FCF, format = "f", big.mark = ",", digits = 0)), 
                  vjust = -0.5, size = 4, color = "black") +
        scale_color_brewer(palette = "Set1") +
        scale_fill_brewer(palette = "Set1") +
        theme_minimal(base_size = 14) +
        labs(title = "📈 歷史與預測自由現金流 (FCF) 走勢圖", x = "年份", y = "自由現金流 (FCF)") +
        theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(face = "bold", size = 16)) +
        expand_limits(y = max(df_plot$FCF, na.rm = TRUE) * 1.2)
    })
    
    # ==========================================
    # 🌟 數據同步引擎：將邏輯抽離為共用函數
    # ==========================================
    do_sync_financials <- function() {
      req(d_cash_flow(), d_income_statement(), d_balance_sheet())
      
      rev  <- select_clean_metric_row(d_income_statement(), "Total Revenue")[1]
      ebit <- select_clean_metric_row(d_income_statement(), "Operating Income")[1]
      tax_exp <- select_clean_metric_row(d_income_statement(), "Income Tax Expense")[1]
      pre_tax <- select_clean_metric_row(d_income_statement(), "Pretax Income")[1]
      
      # 計算有效稅率並推算 NOPAT
      tax_rate <- if (!is.na(pre_tax) && pre_tax > 0 && !is.na(tax_exp)) max(0, min(tax_exp / pre_tax, 0.5)) else 0.21
      nopat <- if (!is.na(ebit)) ebit * (1 - tax_rate) else select_clean_metric_row(d_income_statement(), "Net Income")[1]
      
      dep  <- select_clean_metric_row(d_cash_flow(), "Depreciation")[1]
      raw_nwc <- select_clean_metric_row(d_cash_flow(), "Change in Working Capital")[1]
      cap  <- abs(select_clean_metric_row(d_cash_flow(), "Capital Expenditure")[1])
      
      asst <- select_clean_metric_row(d_balance_sheet(), "Total Assets")[1]
      liab <- select_clean_metric_row(d_balance_sheet(), "Current Liabilities")[1]
      debt <- select_clean_metric_row(d_balance_sheet(), "Current Debt")[1]
      
      updateNumericInput(session, "fcf_revenue", value = rev)
      updateNumericInput(session, "fcf_nopat", value = nopat) # 🌟 改為更新 NOPAT
      updateNumericInput(session, "fcf_depreciation", value = dep)
      updateNumericInput(session, "fcf_delta_nwc", value = if(!is.na(raw_nwc)) -raw_nwc else 0)
      updateNumericInput(session, "fcf_capex", value = cap)
      updateNumericInput(session, "fcf_invested_capital", value = asst - (liab - ifelse(is.na(debt), 0, debt)))
    }
    
    # 🚀 1. 全自動連動：只要主程式抓到新財報，模擬沙盒直接自動更新！
    observeEvent(d_cash_flow(), {
      do_sync_financials()
    })
    
    # 🔄 2. 手動連動：保留給使用者「玩壞參數」後，想一鍵重置的按鈕
    observeEvent(input$btn_sync_fcf_params, {
      do_sync_financials()
      showNotification("✅ 已手動重置並重新載入最新財報 FCF 參數！", type = "message")
    })
    
    # --- 核心估算邏輯：包含天花板開關 ---
    fcf_estimator_results <- reactive({
      req(input$fcf_net_income, input$fcf_invested_capital)
      
      ni    <- safe_num(input$fcf_net_income)
      depre <- safe_num(input$fcf_depreciation)
      nwc   <- safe_num(input$fcf_delta_nwc)
      capex <- safe_num(input$fcf_capex)
      ic    <- safe_num(input$fcf_invested_capital)
      
      ebit_val <- select_clean_metric_row(d_income_statement(), "Operating Income")[1]
      tax_exp  <- select_clean_metric_row(d_income_statement(), "Income Tax Expense")[1]
      pre_tax  <- select_clean_metric_row(d_income_statement(), "Pretax Income")[1]
      
      tax_rate <- 0.21
      if (!is.na(pre_tax) && pre_tax > 0 && !is.na(tax_exp)) {
        tax_rate <- max(0, min(tax_exp / pre_tax, 0.5))
      }
      
      nopat <- if (!is.na(ebit_val)) ebit_val * (1 - tax_rate) else ni
      reinvestment <- (capex - depre) + nwc
      
      rr   <- if(nopat > 0) reinvestment / nopat else 0
      roic <- if(ic > 0) nopat / ic else 0
      raw_g <- roic * rr
      
      # 🌟 天花板邏輯切換
      apply_ceiling <- if (!is.null(input$apply_g_ceiling)) input$apply_g_ceiling else TRUE
      g_est <- if (apply_ceiling) max(-0.05, min(raw_g, 0.25)) else max(-0.05, raw_g)
      
      return(list(
        ocf = ni + depre - nwc, 
        fcf = (ni + depre - nwc) - capex, 
        g = round(g_est * 100, 2), 
        raw_g = round(raw_g * 100, 2),
        roic = round(roic * 100, 2), 
        rr = round(rr * 100, 2),
        ceiling_on = apply_ceiling
      ))
    })
    
    # 🌟 核心修正：廢除模組內部獨立的選單判斷，強制 100% 聽從主程式 (global_est_g)
    final_sim_g <- reactive({
      current_g <- global_est_g()
      if (is.null(current_g)) return(0)
      return(safe_num(current_g))
    })
    
    # --- 渲染成長率細節 (同步顯示主程式傳來的數字) ---
    output$g_result_display <- renderUI({
      g_val <- final_sim_g()
      
      # 為了保留天花板警告的連動
      res <- fcf_estimator_results()
      hit <- res$raw_g > 25
      
      status_msg <- ""
      # 如果主程式傳來的數值剛好是基本面推估值，才顯示防呆警告
      if (g_val == res$g && hit && res$ceiling_on) {
        status_msg <- "<div style='color: #d9534f; font-weight: bold; font-size: 12px; margin-top: 5px;'>⚠️ 實際模型輸出: 已強制封頂於 25%</div>"
      } else if (g_val == res$raw_g && hit && !res$ceiling_on) {
        status_msg <- "<div style='color: #8e44ad; font-weight: bold; font-size: 12px; margin-top: 5px;'>🔥 實際模型輸出: 已解除天花板限制</div>"
      }
      
      HTML(paste0("<div style='font-size: 15px; color: #2c3e50;'>目前模型代入成長率: <b style='color: #d35400; font-size: 18px;'>", g_val, " %</b></div>", status_msg))
    })
    
    # 🌟 2. 建立 FCFF 模擬表專屬的成長率引擎 (預設為 0)
    active_fcff_g <- reactiveVal(0)
    
    # 🌟 3. 監聽按鈕：點擊時，把外部算好的 g 灌入 FCFF 專屬引擎
    observeEvent(input$btn_apply_g_to_fcff, {
      req(global_est_g())
      active_fcff_g(global_est_g())
      showNotification(paste0("✅ 成功！已將 ", global_est_g(), "% 套用為 FCFF 模擬營收成長率"), type = "message")
    })
    
    # ==========================================
    # 營收佔比法動態預測矩陣
    # ==========================================
    proj_table_data <- reactive({
      req(input_years())
      n_years <- as.numeric(input_years()) 
      
      # 🌟 關鍵修正 1：直接抓取下拉選單算出的最新數值 (也就是 Box 顯示的 EST.G)
      current_g <- global_est_g()
      if (is.null(current_g)) current_g <- 0
      
      # 🌟 讓矩陣對齊我們選擇的 g
      g_rev_rate <- final_sim_g() / 100
      
      # 取得基礎財報數值
      base_rev       <- safe_num(input$fcf_revenue)
      base_nopat     <- safe_num(input$fcf_nopat) # 🌟 接收 NOPAT
      base_depre     <- safe_num(input$fcf_depreciation)
      base_capex     <- safe_num(input$fcf_capex)
      base_delta_nwc <- safe_num(input$fcf_delta_nwc)
      
      # 計算各項佔營收比率 (防呆：分母不為 0)
      nopat_margin     <- if(base_rev == 0) 0 else base_nopat / base_rev # 🌟 改算 NOPAT 利潤率
      depre_margin     <- if(base_rev == 0) 0 else base_depre / base_rev
      capex_margin     <- if(base_rev == 0) 0 else base_capex / base_rev
      delta_nwc_margin <- if(base_rev == 0) 0 else base_delta_nwc / base_rev
      
      # 初始化空表
      df <- data.frame(
        Year = paste0("Year ", 1:n_years),
        Revenue = numeric(n_years),
        NOPAT = numeric(n_years), # 🌟 表格欄位改為 NOPAT
        Depreciation = numeric(n_years),
        CapEx = numeric(n_years),
        Delta_NWC = numeric(n_years),
        FCFF = numeric(n_years)
      )
      
      # 🌟 關鍵修正 2：動態推算 (現在營收會完美跟隨你選擇的方法進行複利)
      for(i in 1:n_years) {
        df$Revenue[i]      <- base_rev * (1 + g_rev_rate)^i
        df$NOPAT[i]        <- df$Revenue[i] * nopat_margin # 🌟
        df$Depreciation[i] <- df$Revenue[i] * depre_margin
        df$CapEx[i]        <- df$Revenue[i] * capex_margin
        if (i == 1) {
          df$Delta_NWC[i] <- (df$Revenue[i] - base_rev) * delta_nwc_margin
        } else {
          df$Delta_NWC[i] <- (df$Revenue[i] - df$Revenue[i-1]) * delta_nwc_margin
        }
        # FCFF 核心公式：淨利 + 折舊 - 資本支出 - 營運資金增加量
        df$FCFF[i] <- df$NOPAT[i] + df$Depreciation[i] - df$CapEx[i] - df$Delta_NWC[i]
      }
      
      return(df)
    })
    
    # --- 渲染 UI 元件 ---
    output$vbx_est_ocf <- renderValueBox({
      res <- fcf_estimator_results()
      valueBox(format_dollar_abbr(res$ocf), "營業現金流 (OCF)", icon = icon("money-bill-wave"), color = "teal")
    })
    
    output$vbx_est_fcf <- renderValueBox({
      res <- fcf_estimator_results()
      color <- if (res$fcf > 0) "green" else "red"
      valueBox(format_dollar_abbr(res$fcf), "企業自由現金流 (FCFF)", icon = icon("piggy-bank"), color = color)
    })
    
    output$vbx_est_g_fund <- renderValueBox({
      res <- fcf_estimator_results()
      
      # 🌟 關鍵修復：不管有沒有勾選，畫面上永遠顯示財報真實算出來的數字 (raw_g)
      display_g <- res$raw_g
      
      # 動態標題：如果算出極端值且啟動了防呆，在小字體提醒使用者「進入模型的其實是 25%」
      subtitle_html <- if(res$ceiling_on && res$raw_g > 25) {
        HTML("基本面隱含成長率 (ROIC × RR) <span style='color: #ffcccc; font-size: 12px; font-weight: bold;'>(⚠️ 模型代入 25%)</span>")
      } else {
        "基本面隱含成長率 (ROIC × RR)"
      }
      
      valueBox(paste0(display_g, " %"), subtitle_html, icon = icon("microscope"), color = "purple")
    })
    
    output$title_dynamic_years <- renderUI({
      h4(tags$b(paste0("📈 未來 ", input_years(), " 年自由現金流 (營收佔比推算)")))
    })
    
    output$tbl_fcf_projection <- DT::renderDataTable({
      df <- proj_table_data()
      colnames(df) <- c("預測期", "預估營收", "稅後淨利 (NI)", "折舊攤銷 (D&A)", "資本支出 (CapEx)", "營運資金變動 (ΔNWC)", "企業自由現金流 (FCFF)")
      
      DT::datatable(df, 
                    options = list(
                      dom = 't', 
                      ordering = FALSE,
                      scrollX = TRUE,         # 🌟 核心修正：允許橫向捲動
                      columnDefs = list(list(className = 'dt-center', targets = "_all")) # 順便讓文字置中比較好看
                    ),
                    rownames = FALSE,
                    class = 'cell-border stripe hover') %>%
        DT::formatCurrency(columns = 2:7, currency = "$", digits = 2) %>%
        DT::formatStyle('企業自由現金流 (FCFF)', backgroundColor = '#e8f4f8', fontWeight = 'bold')
    })
    
    # 🌟 動態偵測缺失值 (套用 setup7.R 的共用函數)
    output$alert_missing_values <- renderUI({
      ui_missing_data_alert(
        check_list = list(
          "當期營收" = input$fcf_revenue,
          "稅後淨利" = input$fcf_net_income,
          "折舊與攤銷" = input$fcf_depreciation,
          "資本支出" = input$fcf_capex,
          "營運資金變動" = input$fcf_delta_nwc,
          "總投入資本" = input$fcf_invested_capital
        ),
        fallback_msg = "系統已自動將缺失項目視為 0 代入計算（總投入資本預設為 1）。請確認是否需要手動補齊數值。"
      )
    })
    
    # 1. 捕捉按鈕事件，並傳送回 server.R
    applied_g_val <- eventReactive(input$btn_apply_g_to_dcf, {
      fcf_estimator_results()$g
    })
    
    # 2. 當按下按鈕時，同步更新 FCF 模組自身的營收成長率
    observeEvent(input$btn_apply_g_to_dcf, {
      new_g <- fcf_estimator_results()$g
      updateNumericInput(session, "input_g_rev", value = new_g)
    })
    
    # 1. 渲染圖表 (對應 UI 的 mod_fcf-fcf_plot)
    output$fcf_plot <- renderPlot({
      req(fcf_plot_obj()) # 確保底片(圖表物件)存在
      fcf_plot_obj()      # 將 ggplot 物件洗成真正的圖片
    })
    
    # 2. 渲染文字狀態 (對應 UI 的 mod_fcf-txt_fcf_raw_data)
    # 因為 UI 是寫 htmlOutput，所以這裡我們用 renderUI 搭配 HTML 輸出
    output$txt_fcf_raw_data <- renderUI({
      df <- proj_table_data()
      if (is.null(df)) {
        return(HTML("<div style='color: gray; font-size: 14px;'>⏳ 尚未匯入財報資料，或正在等待計算...</div>"))
      }
      
      HTML(glue::glue(
        "<div style='background-color: #f9f9f9; padding: 15px; border-left: 4px solid #00a65a; margin-top: 10px;'>
           <b>✅ FCF 預測資料已同步！</b><br/>
           -------------------------<br/>
           第 1 年預測現金流: <b>${round(df$FCFF[1], 2)}</b><br/>
           第 {nrow(df)} 年預測現金流: <b>${round(df$FCFF[nrow(df)], 2)}</b><br/>
         </div>"
      ))
    })
    
    # ==========================================
    # 📤 匯出資料給其他模組與主程式
    # ==========================================
    return(list(
      df_fcf        = reactive({ proj_table_data() }), 
      hist_params   = reactive({ hist_params() }),
      fcf_plot_obj  = fcf_plot_obj,
      # 🌟 這裡回傳當前模擬出的成長率
      applied_g = reactive({ final_sim_g() })
    ))
  })
}
