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

           # 前瞻比率屬 FCFF 預測假設（非 Sensitivity）：空白則用當期絕對金額推算佔比
           fluidRow(
             box(
               title = tagList(icon("percent"), "預測假設：CapEx／ΔNWC 佔營收比"),
               width = 12, status = "danger", solidHeader = TRUE,
               p(
                 style = "font-size:13px; color:#555; margin-top:0;",
                 "此處比率驅動下方多期 FCFF 預測表。留白時以當期 CapEx、ΔNWC 絕對金額／營收推算；可手動覆寫以模擬更高／更低再投資。"
               ),
               fluidRow(
                 column(
                   width = 6,
                   h4(tags$b("CapEx 預估資本支出佔營收比")),
                   numericInput(ns("proj_capex_rate"), "CapEx / Revenue (%):", value = NA, step = 0.01),
                   uiOutput(ns("txt_hist_capex")),
                   h6(helpText("註：空白＝套用當期金額推算（並顯示歷史淨 CapEx 參考值）。"))
                 ),
                 column(
                   width = 6,
                   h4(tags$b("ΔNWC 預估營運資本佔營收變動比")),
                   numericInput(ns("proj_nwc_rate"), "ΔNWC / ΔRevenue (%)", value = NA, step = 0.01),
                   uiOutput(ns("txt_hist_nwc")),
                   h6(helpText("註：空白＝套用當期金額推算（並顯示歷史 ΔNWC／ΔRevenue 參考值）。"))
                 )
               )
             )
           ),
           
           # g_growth_method 在模組外為全域 ID（無 ns）
           conditionalPanel(
             condition = "input.g_growth_method == 'fundamental'",
             checkboxInput(ns("apply_g_ceiling"), 
                           tags$span(style = "color: #d35400; font-weight: bold;", "啟用 25% 成長率天花板防呆 (建議)"), 
                           value = TRUE)
           ),
           br(),
           
           actionButton(ns("btn_apply_g_to_dcf"), "將此成長率 (g) 套用至 FCFF 模型", 
                        icon = icon("check"), class = "btn-success"),
           
           div(style = "background-color: #f9f9f9; padding: 15px; border-left: 4px solid #00a65a; margin-bottom: 20px;",
               h4(tags$b("自由現金流 (FCF) 參數拆解")),
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
                                  "最近一年" = "last_year",
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
    global_est_g     = reactive(NULL),
    global_g_method  = reactive(NULL)
) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 🧮 輔助：歷史參數萃取
    extract_dcf_parameters <- function(df_is, df_cf, df_bs) {
      revenue    <- select_clean_metric_row(df_is, "Total Revenue", include_ttm = FALSE)
      op_income  <- select_clean_metric_row(df_is, "Operating Income", include_ttm = FALSE)
      income_tax <- select_clean_metric_row(df_is, "Income Tax Expense", include_ttm = FALSE)
      pre_tax    <- select_clean_metric_row(df_is, "Pretax Income", include_ttm = FALSE)
      capex <- select_clean_metric_row(df_cf, "Capital Expenditure", include_ttm = FALSE)
      if (all(is.na(capex))) capex <- select_clean_metric_row(df_cf, "Capital Expenditures", include_ttm = FALSE)
      
      depre <- select_clean_metric_row(df_cf, "Depreciation", include_ttm = FALSE)
      depre_val <- if(is.na(depre[1])) 0 else depre[1]
      
      curr_assets <- select_clean_metric_row(df_bs, "Current Assets", include_ttm = FALSE)
      cash        <- select_clean_metric_row(df_bs, "Cash And Cash Equivalents", include_ttm = FALSE)
      curr_liab   <- select_clean_metric_row(df_bs, "Current Liabilities", include_ttm = FALSE)
      st_debt     <- select_clean_metric_row(df_bs, "Current Debt", include_ttm = FALSE)
      if (all(is.na(st_debt))) st_debt <- rep(0, length(curr_liab))
      
      non_cash_ca <- curr_assets - cash
      non_debt_cl <- curr_liab - st_debt
      nwc <- non_cash_ca - non_debt_cl
      
      tax_rate <- ifelse(is.na(pre_tax) | pre_tax == 0, 0.21, income_tax / pre_tax)
      tax_rate <- pmax(0, pmin(tax_rate, 0.5)) 
      
      net_capex <- abs(capex) - depre_val
      net_capex_margin <- net_capex / revenue
      
      # 當期 - 前期（向量已為 最新財年 → 最舊財年）
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
    
    # 依 DCF 模式決定各年營收成長率 (%)
    growth_pct_for_year <- function(year_idx) {
      revenue_growth_pct_for_year(
        year_idx = year_idx,
        mode = input_mode(),
        g_est = final_sim_g(),
        g_stage1 = g_stage1(),
        g_stage2 = g_stage2(),
        yr_stage1 = yr_stage1()
      )
    }
    
    # 產生 DCF / 報告用趨勢圖（與 proj_table_data 同一套 FCFF）
    fcf_plot_obj <- reactive({
      df_proj <- proj_table_data()
      if (is.null(df_proj) || nrow(df_proj) < 1) return(NULL)
      
      latest_fcf <- select_current_metric(d_cash_flow(), "Free Cash Flow", "flow")
      if (!is.null(input_manual_fcf()) && !is.na(input_manual_fcf())) {
        latest_fcf <- input_manual_fcf()
      }
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      
      hist_df <- data.frame(
        Year = current_year,
        FCF = safe_num(latest_fcf),
        Type = "Actual (最新財報)",
        stringsAsFactors = FALSE
      )
      proj_df <- data.frame(
        Year = (current_year + 1):(current_year + nrow(df_proj)),
        FCF = as.numeric(df_proj$FCFF),
        Type = "Projected (預測期)",
        stringsAsFactors = FALSE
      )
      df_plot <- rbind(hist_df, proj_df)
      df_plot <- df_plot[!is.na(df_plot$FCF), ]
      if (nrow(df_plot) < 1) return(NULL)
      
      ggplot(df_plot, aes(x = Year, y = FCF, fill = Type)) +
        geom_bar(stat = "identity", alpha = 0.6, width = 0.5) +
        geom_line(color = "gray50", linewidth = 1.2, group = 1) +
        geom_point(aes(color = Type), size = 3) +
        geom_text(aes(label = formatC(FCF, format = "f", big.mark = ",", digits = 0)), 
                  vjust = -0.5, size = 4, color = "black") +
        scale_color_brewer(palette = "Set1") +
        scale_fill_brewer(palette = "Set1") +
        theme_minimal(base_size = 14) +
        labs(title = "歷史與預測自由現金流 (FCFF) 走勢圖", x = "年份", y = "自由現金流 (FCFF)") +
        theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(face = "bold", size = 16)) +
        expand_limits(y = max(df_plot$FCF, na.rm = TRUE) * 1.2)
    })
    
    # 財報載入時同步歷史 CapEx / NWC 比率（供 FCFF 預測假設參考）
    observeEvent(calc_trigger(), {
      df_is <- d_income_statement()
      df_cf <- d_cash_flow()
      df_bs <- d_balance_sheet()
      if (is.null(df_is) || is.null(df_cf) || is.null(df_bs)) return()
      if (nrow(df_is) == 0 || nrow(df_cf) == 0 || nrow(df_bs) == 0) return()
      params <- extract_dcf_parameters(df_is, df_cf, df_bs)
      hist_params(list(capex_rate = params$net_capex_margin, nwc_rate = params$nwc_margin))
    })

    output$txt_hist_capex <- renderUI({
      params <- hist_params()
      if (is.null(params) || is.na(params$capex_rate)) {
        return(HTML("<div style='color: gray; font-size: 13px; margin-bottom: 5px;'>系統歷史推算值：等待財報資料匯入...</div>"))
      }
      val <- round(params$capex_rate * 100, 2)
      HTML(paste0("<div style='color: #3c8dbc; font-size: 14px; margin-bottom: 5px;'>系統歷史推算值：<b>", val, " %</b></div>"))
    })

    output$txt_hist_nwc <- renderUI({
      params <- hist_params()
      if (is.null(params) || is.na(params$nwc_rate)) {
        return(HTML("<div style='color: gray; font-size: 13px; margin-bottom: 5px;'>系統歷史推算值：等待財報資料匯入...</div>"))
      }
      val <- round(params$nwc_rate * 100, 2)
      HTML(paste0("<div style='color: #3c8dbc; font-size: 14px; margin-bottom: 5px;'>系統歷史推算值：<b>", val, " %</b></div>"))
    })
    
    # ==========================================
    # 🌟 數據同步引擎：將邏輯抽離為共用函數
    # ==========================================
    do_sync_financials <- function() {
      req(d_cash_flow(), d_income_statement(), d_balance_sheet())
      
      rev  <- select_current_metric(d_income_statement(), "Total Revenue", "flow")
      ebit <- select_current_metric(d_income_statement(), "Operating Income", "flow")
      tax_exp <- select_current_metric(d_income_statement(), "Income Tax Expense", "flow")
      pre_tax <- select_current_metric(d_income_statement(), "Pretax Income", "flow")
      
      tax_rate <- if (!is.na(pre_tax) && pre_tax > 0 && !is.na(tax_exp)) max(0, min(tax_exp / pre_tax, 0.5)) else 0.21
      nopat <- if (!is.na(ebit)) ebit * (1 - tax_rate) else select_current_metric(d_income_statement(), "Net Income", "flow")
      
      dep  <- select_current_metric(d_cash_flow(), "Depreciation", "flow")
      raw_nwc <- select_current_metric(d_cash_flow(), "Change in Working Capital", "flow")
      cap  <- abs(select_current_metric(d_cash_flow(), "Capital Expenditure", "flow"))
      
      asst <- select_current_metric(d_balance_sheet(), "Total Assets", "stock")
      liab <- select_current_metric(d_balance_sheet(), "Current Liabilities", "stock")
      debt <- select_current_metric(d_balance_sheet(), "Current Debt", "stock")
      
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
      # 🌟 關鍵修正：將原本錯誤殘留的 input$fcf_net_income 替換為最新的 input$fcf_nopat
      req(input$fcf_nopat, input$fcf_invested_capital)
      
      nopat <- safe_num(input$fcf_nopat) # 🌟 改為接 NOPAT
      depre <- safe_num(input$fcf_depreciation)
      nwc   <- safe_num(input$fcf_delta_nwc)
      capex <- safe_num(input$fcf_capex)
      ic    <- safe_num(input$fcf_invested_capital)
      
      # 由於 NOPAT 已經直接從 UI 取得，不需再透過 EBIT 推算
      reinvestment <- (capex - depre) + nwc
      
      rr   <- if(nopat > 0) reinvestment / nopat else 0
      roic <- if(ic > 0) nopat / ic else 0
      raw_g <- roic * rr
      
      # 🌟 天花板邏輯切換
      apply_ceiling <- if (!is.null(input$apply_g_ceiling)) input$apply_g_ceiling else TRUE
      g_est <- if (apply_ceiling) max(-0.05, min(raw_g, 0.25)) else max(-0.05, raw_g)
      
      return(list(
        ocf = nopat + depre - nwc, 
        fcf = (nopat + depre - nwc) - capex, 
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
        status_msg <- "<div style='color: #d9534f; font-weight: bold; font-size: 12px; margin-top: 5px;'>實際模型輸出: 已強制封頂於 25%</div>"
      } else if (g_val == res$raw_g && hit && !res$ceiling_on) {
        status_msg <- "<div style='color: #8e44ad; font-weight: bold; font-size: 12px; margin-top: 5px;'>實際模型輸出: 已解除天花板限制</div>"
      }
      
      HTML(paste0("<div style='font-size: 15px; color: #2c3e50;'>目前模型代入成長率: <b style='color: #d35400; font-size: 18px;'>", g_val, " %</b></div>", status_msg))
    })
    
    # ==========================================
    # 營收佔比法動態預測矩陣
    # ==========================================
    proj_table_data <- reactive({
      req(input_years())
      n_years <- as.numeric(input_years()) 
      
      base_rev       <- safe_num(input$fcf_revenue)
      base_nopat     <- safe_num(input$fcf_nopat)
      base_depre     <- safe_num(input$fcf_depreciation)
      base_capex     <- safe_num(input$fcf_capex)
      base_delta_nwc <- safe_num(input$fcf_delta_nwc)
      
      nopat_margin     <- if(base_rev == 0) 0 else base_nopat / base_rev
      depre_margin     <- if(base_rev == 0) 0 else base_depre / base_rev

      # 前瞻比率優先：模組內手動覆寫 → 否則當期絕對金額／營收
      user_capex_pct <- suppressWarnings(as.numeric(input$proj_capex_rate)[1])
      user_nwc_pct   <- suppressWarnings(as.numeric(input$proj_nwc_rate)[1])
      capex_margin <- if (is.finite(user_capex_pct)) {
        user_capex_pct / 100
      } else if (base_rev == 0) {
        0
      } else {
        base_capex / base_rev
      }
      delta_nwc_margin <- if (is.finite(user_nwc_pct)) {
        user_nwc_pct / 100
      } else if (base_rev == 0) {
        0
      } else {
        base_delta_nwc / base_rev
      }
      
      df <- data.frame(
        Year = paste0("Year ", 1:n_years),
        Revenue = numeric(n_years),
        NOPAT = numeric(n_years),
        Depreciation = numeric(n_years),
        CapEx = numeric(n_years),
        Delta_NWC = numeric(n_years),
        FCFF = numeric(n_years)
      )
      
      g_path <- vapply(seq_len(n_years), growth_pct_for_year, numeric(1))
      
      for(i in 1:n_years) {
        g_rev_rate <- g_path[i] / 100
        if (i == 1) {
          df$Revenue[i] <- base_rev * (1 + g_rev_rate)
        } else {
          df$Revenue[i] <- df$Revenue[i - 1] * (1 + g_rev_rate)
        }
        df$NOPAT[i]        <- df$Revenue[i] * nopat_margin
        df$Depreciation[i] <- df$Revenue[i] * depre_margin
        df$CapEx[i]        <- df$Revenue[i] * capex_margin
        if (i == 1) {
          df$Delta_NWC[i] <- (df$Revenue[i] - base_rev) * delta_nwc_margin
        } else {
          df$Delta_NWC[i] <- (df$Revenue[i] - df$Revenue[i - 1]) * delta_nwc_margin
        }
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
      g_val <- final_sim_g()
      method <- as.character(global_g_method() %||% "")
      method_label <- switch(
        method,
        "fundamental" = "基本面 (ROIC × RR)",
        "cagr" = "CAGR",
        "mean" = "平均成長率",
        "median" = "中位數成長率",
        "last_year" = "最近一年成長率",
        "custom" = "自訂成長率",
        "預估 FCFF 成長率"
      )
      res <- fcf_estimator_results()
      subtitle_html <- method_label
      if (identical(method, "fundamental") && isTRUE(res$ceiling_on) && isTRUE(res$raw_g > 25)) {
        subtitle_html <- HTML(paste0(
          method_label,
          " <span style='color:#ffcccc;font-size:12px;font-weight:bold;'>(模型代入 25%)</span>"
        ))
      }
      valueBox(
        paste0(round(as.numeric(g_val), 2), " %"),
        subtitle_html,
        icon = icon("chart-line"),
        color = "purple"
      )
    })
    
    output$title_dynamic_years <- renderUI({
      h4(tags$b(paste0("未來 ", input_years(), " 年自由現金流 (營收佔比推算)")))
    })
    
    output$tbl_fcf_projection <- DT::renderDataTable({
      df <- proj_table_data()
      colnames(df) <- c("預測期", "預估營收", "稅後營業利潤 (NOPAT)", "折舊攤銷 (D&A)", "資本支出 (CapEx)", "營運資金變動 (ΔNWC)", "企業自由現金流 (FCFF)")
      
      DT::datatable(df, 
                    options = list(
                      dom = 't', 
                      ordering = FALSE,
                      scrollX = TRUE,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ),
                    rownames = FALSE,
                    class = 'cell-border stripe hover') %>%
        DT::formatCurrency(columns = 2:7, currency = "$", digits = 2) %>%
        DT::formatStyle('企業自由現金流 (FCFF)', backgroundColor = '#e8f4f8', fontWeight = 'bold')
    })
    
    # 🌟 動態偵測缺失值
    output$alert_missing_values <- renderUI({
      ui_missing_data_alert(
        check_list = list(
          "當期營收" = input$fcf_revenue,
          "稅後營業利潤" = input$fcf_nopat,
          "折舊與攤銷" = input$fcf_depreciation,
          "資本支出" = input$fcf_capex,
          "營運資金變動" = input$fcf_delta_nwc,
          "總投入資本" = input$fcf_invested_capital
        ),
        fallback_msg = "系統已自動將缺失項目視為 0 代入計算（總投入資本預設為 1）。請確認是否需要手動補齊數值。"
      )
    })
    
    # 捕捉按鈕事件，並傳送回 server.R
    applied_g_val <- eventReactive(input$btn_apply_g_to_dcf, {
      fcf_estimator_results()$g
    })
    
    # 1. 渲染圖表 (對應 UI 的 mod_fcf-fcf_plot)
    output$fcf_plot <- renderPlot({
      req(fcf_plot_obj())
      fcf_plot_obj()
    })
    
    # 2. 渲染文字狀態 (對應 UI 的 mod_fcf-txt_fcf_raw_data)
    output$txt_fcf_raw_data <- renderUI({
      df <- proj_table_data()
      if (is.null(df)) {
        return(HTML("<div style='color: gray; font-size: 14px;'>⏳ 尚未匯入財報資料，或正在等待計算...</div>"))
      }
      
      HTML(glue::glue(
        "<div style='background-color: #f9f9f9; padding: 15px; border-left: 4px solid #00a65a; margin-top: 10px;'>
           <b>FCF 預測資料已同步！</b><br/>
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
      applied_g = reactive({ final_sim_g() })
    ))
  })
}
