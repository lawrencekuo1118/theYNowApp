# ==========================================
# pb_asset_module.R - P/B／資產估值法
# 專治：金融股、保險、控股集團、負 FCF 但帳面淨值可信的企業（如 BRK-B）
# 核心：合理價 = BVPS × 目標本淨比 (Target P/B)
# ==========================================

# ==========================================
# 🖥️ 前端 UI
# ==========================================
pb_asset_module_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "pb_calculator",
          tabBox(title = "P/B & ASSET VALUE", width = "auto",
                 
                 tabPanel("P/B Overview", icon = icon("landmark"),
                          fluidRow(
                            column(4, valueBoxOutput(ns("vbx_bvps"), width = 12)),
                            column(4, valueBoxOutput(ns("vbx_tbvps"), width = 12)),
                            column(4, valueBoxOutput(ns("vbx_mkt_pb"), width = 12))
                          ),
                          fluidRow(
                            div("Fair Price = Book Value per Share (BVPS) × Target P/B Multiple",
                                style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;")
                          ),
                          fluidRow(
                            div(style = "text-align: center; margin-bottom: 20px;",
                                actionButton(ns("btn_calc_pb"), "試算 P/B 合理價",
                                             style = "background-color: #2980b9; color: white; font-weight: bold; font-size: 18px; padding: 12px 30px; border-radius: 8px; border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
                            )
                          ),
                          fluidRow(
                            column(width = 12,
                                   uiOutput(ns("ui_pb_result")),
                                   br(),
                                   box(title = "估值區間（保守／基準／樂觀）", width = 12, status = "primary", solidHeader = TRUE,
                                       tableOutput(ns("tbl_pb_band")),
                                       plotOutput(ns("plt_pb_band"), height = "280px")
                                   )
                            )
                          )
                 ),
                 
                 tabPanel("P/B Settings", icon = icon("cogs"),
                          h4(tags$b("每股帳面淨值 (BVPS) 與資產基礎")),
                          fluidRow(
                            div("標準公式：BVPS = Common Equity ÷ 財報流通股數；TBVPS 另扣除商譽／無形資產。雙重股權（如 BRK-B）等「報價股 ≠ 財報股數口徑」時，可選擇下方例外校正。",
                                style = "font-size: 14px; font-weight: bold; color: #2C3E50; text-align: left; margin-bottom: 10px; padding: 10px; background-color: #F8F9F9; border-left: 4px solid #2980B9; border-radius: 4px;")
                          ),
                          fluidRow(
                            column(
                              12,
                              checkboxInput(
                                ns("adjust_share_class"),
                                tags$span(
                                  style = "font-weight: bold;",
                                  "套用約當股數校正（例外補償：市值÷股價或 BRK-B×1500）"
                                ),
                                value = isTRUE(APP_DEFAULTS$pb_adjust_share_class)
                              ),
                              tags$p(
                                style = "margin: -8px 0 12px 0; font-size: 12px; color: #666; line-height: 1.45;",
                                "非標準自動估值步驟。僅在財報股數與目前報價股級距明顯不符時勾選；勾選後請按「從最新財報自動帶入」或等待自動同步。"
                              )
                            )
                          ),
                          uiOutput(ns("txt_shares_resolve_note")),
                          fluidRow(
                            column(4, numericInput(ns("bvps"), "每股帳面淨值 BVPS (USD)", value = APP_DEFAULTS$pb_bvps, step = 0.1, min = 0)),
                            column(4, numericInput(ns("tbvps"), "有形每股淨值 TBVPS (USD)", value = APP_DEFAULTS$pb_tbvps, step = 0.1, min = 0)),
                            column(4,
                                   br(),
                                   actionButton(ns("btn_sync_bv"), "從最新財報自動帶入",
                                                icon = icon("sync"), class = "btn-sm",
                                                style = "background-color: #2980b9; color: white; border: none; padding: 8px 15px; font-weight: bold; border-radius: 5px; margin-top: 5px;")
                            )
                          ),
                          fluidRow(
                            column(12, uiOutput(ns("alert_missing_bv")))
                          ),
                          hr(style = "border-top: 1px solid #BDC3C7;"),
                          h4(tags$b("目標本淨比假設")),
                          fluidRow(
                            column(4, numericInput(ns("pb_low"),  "保守 P/B (×)", value = APP_DEFAULTS$pb_low,  step = 0.05, min = 0.1)),
                            column(4, numericInput(ns("pb_mid"),  "基準 P/B (×)", value = APP_DEFAULTS$pb_mid,  step = 0.05, min = 0.1)),
                            column(4, numericInput(ns("pb_high"), "樂觀 P/B (×)", value = APP_DEFAULTS$pb_high, step = 0.05, min = 0.1))
                          ),
                          fluidRow(
                            column(6,
                                   selectInput(ns("basis"), "估值基礎",
                                               choices = c("帳面淨值 BVPS" = "bvps",
                                                           "有形淨值 TBVPS" = "tbvps"),
                                               selected = APP_DEFAULTS$pb_basis)
                            ),
                            column(6,
                                   checkboxInput(ns("use_industry_pb"),
                                                 tags$span(style = "font-weight: bold;", "套用產業預設本淨比區間（若有）"),
                                                 value = APP_DEFAULTS$pb_use_industry)
                            )
                          ),
                          fluidRow(
                            column(12,
                                   actionButton(ns("btn_reset_pb"), "回復系統預設參數",
                                                icon = icon("undo"), class = "btn-sm",
                                                style = "background-color: #7f8c8d; color: white; border: none; margin-top: 10px;")
                            )
                          ),
                          br(),
                          div(style = "background-color: #f9f9f9; padding: 15px; border-left: 4px solid #2980b9;",
                              h4(tags$b("使用情境")),
                              p("適用於銀行、保險、控股／綜合企業：折現模型（DCF／DDM）前提常不成立時，以淨資產與合理本淨比定價。"),
                              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 0;",
                                "※ Buffett／Berkshire 實務常以 Book Value 為錨；目標 P/B 請依產業與利率環境調整，勿固定單一倍數。")
                          )
                 )
          )
  )
}

# ==========================================
# ⚙️ 後端 Server
# ==========================================
pb_asset_module_server <- function(id,
                                   d_balance_sheet,
                                   d_income_statement = reactive(NULL),
                                   current_price = reactive(NA),
                                   market_cap = reactive(NA),
                                   current_ticker = reactive(""),
                                   industry_choice = reactive(NULL),
                                   industry_text = reactive("")) {
  moduleServer(id, function(input, output, session) {
    
    # --- 從產業標準推估本淨比區間（若未定義則回傳 NULL）---
    industry_pb_band <- reactive({
      ind <- industry_choice()
      if (is.null(ind) || !nzchar(ind) || !exists("industry_standards")) return(NULL)
      std <- industry_standards[[ind]]
      if (is.null(std)) return(NULL)
      if (!is.null(std$pb_band) && length(std$pb_band) >= 2) {
        lo <- std$pb_band[1]; hi <- std$pb_band[2]
        mid <- if (length(std$pb_band) >= 3) std$pb_band[3] else mean(c(lo, hi))
        return(list(low = lo, mid = mid, high = hi))
      }
      # 金融／保險啟發式（無 pb_band 時）
      txt <- paste(ind, industry_text(), collapse = " ")
      if (grepl("Insurance|Bank|Financial|fn\\.", txt, ignore.case = TRUE)) {
        return(list(low = 1.0, mid = 1.35, high = 1.7))
      }
      NULL
    })

    shares_resolve_note <- reactiveVal(NULL)
    
    # --- 財報同步 BVPS / TBVPS（標準：財報股數；例外校正需使用者勾選）---
    sync_book_values <- function() {
      req(d_balance_sheet())
      df_bs <- d_balance_sheet()
      
      equity <- select_current_metric_any(df_bs, EQUITY_PATTERNS, "stock")
      
      shares_bs <- select_current_metric_any(
        df_bs,
        SHARE_PATTERNS,
        "stock"
      )
      px <- suppressWarnings(as.numeric(current_price())[1])
      mcap <- suppressWarnings(as.numeric(market_cap())[1])
      tk <- tryCatch(current_ticker(), error = function(e) "")
      sh_adj <- resolve_shares_for_price(shares_bs, price = px, market_cap = mcap, ticker = tk)

      apply_adj <- isTRUE(input$adjust_share_class)
      if (apply_adj) {
        shares <- sh_adj$shares
        shares_resolve_note(sh_adj$note)
      } else {
        # 標準路徑：只用財報股數；若偵測到可校正情況，提示使用者自行勾選
        if (is.finite(shares_bs) && shares_bs > 0) {
          shares <- shares_bs
        } else {
          shares <- sh_adj$shares
        }
        if (!is.null(sh_adj$note) && nzchar(sh_adj$note) &&
            !identical(sh_adj$method, "balance_sheet") &&
            is.finite(shares_bs) && shares_bs > 0 &&
            is.finite(sh_adj$shares) &&
            abs(sh_adj$shares / shares_bs - 1) > 0.05) {
          shares_resolve_note(paste0(
            "偵測到股數級距／雙重股權可能不符（未套用校正）。",
            "若要補償，請勾選「套用約當股數校正」後再同步。",
            " 建議原因：", sh_adj$note
          ))
        } else {
          shares_resolve_note(NULL)
        }
      }
      
      # TBVPS 扣除：優先 Goodwill + Other Intangible Assets；
      # 若僅有合計列則只用一次，避免與獨立 Goodwill 雙重扣減
      goodwill <- select_current_metric(df_bs, "^Goodwill$", "stock")
      other_intang <- select_current_metric(df_bs, "^Other Intangible Assets$", "stock")
      combined_gi <- select_current_metric(
        df_bs,
        "Goodwill And Other Intangible Assets|Goodwill & Other Intangible Assets",
        "stock"
      )
      if (!is.na(goodwill) || !is.na(other_intang)) {
        intang_deduct <- ifelse(is.na(goodwill), 0, goodwill) +
          ifelse(is.na(other_intang), 0, other_intang)
      } else if (!is.na(combined_gi)) {
        intang_deduct <- combined_gi
      } else {
        loose <- select_current_metric(df_bs, "^Intangible Assets$", "stock")
        intang_deduct <- ifelse(is.na(loose), 0, loose)
      }
      
      if (!is.na(equity) && !is.na(shares) && shares > 0) {
        bvps <- equity / shares
        tbvps <- max(equity - intang_deduct, 0) / shares
        updateNumericInput(session, "bvps", value = round(bvps, 2))
        updateNumericInput(session, "tbvps", value = round(tbvps, 2))
        if (apply_adj && !is.null(sh_adj$note) && nzchar(sh_adj$note)) {
          showNotification(sh_adj$note, type = "message", duration = 8)
        }
      } else {
        showNotification("無法從財報推算 BVPS，請手動輸入淨值與股數相關科目", type = "warning", duration = 6)
      }
    }
    
    observeEvent(
      list(
        d_balance_sheet(), current_price(), market_cap(), current_ticker(),
        input$adjust_share_class
      ),
      {
        sync_book_values()
      },
      ignoreInit = FALSE
    )
    
    observeEvent(input$btn_sync_bv, {
      sync_book_values()
      showNotification("已自財報同步 BVPS／TBVPS", type = "message")
    })
    
    output$txt_shares_resolve_note <- renderUI({
      note <- shares_resolve_note()
      if (is.null(note) || !nzchar(note)) return(NULL)
      applied <- isTRUE(input$adjust_share_class)
      tags$p(
        style = paste0(
          "font-size: 12px; margin: 0 0 10px 0; padding: 8px 10px; border-left: 3px solid ",
          if (applied) "#e67e22" else "#7f8c8d",
          "; background: ", if (applied) "#fff8e6" else "#f4f4f4",
          "; color: ", if (applied) "#b85c00" else "#555", ";"
        ),
        if (applied) paste0("已套用例外校正：", note) else note
      )
    })
    
    observeEvent(list(input$use_industry_pb, industry_choice(), industry_text()), {
      if (!isTRUE(input$use_industry_pb)) return()
      band <- industry_pb_band()
      if (is.null(band)) return()
      updateNumericInput(session, "pb_low",  value = round(band$low, 2))
      updateNumericInput(session, "pb_mid",  value = round(band$mid, 2))
      updateNumericInput(session, "pb_high", value = round(band$high, 2))
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_reset_pb, {
      updateNumericInput(session, "pb_low",  value = APP_DEFAULTS$pb_low)
      updateNumericInput(session, "pb_mid",  value = APP_DEFAULTS$pb_mid)
      updateNumericInput(session, "pb_high", value = APP_DEFAULTS$pb_high)
      updateSelectInput(session, "basis", selected = APP_DEFAULTS$pb_basis)
      updateCheckboxInput(session, "use_industry_pb", value = APP_DEFAULTS$pb_use_industry)
      updateCheckboxInput(session, "adjust_share_class", value = isTRUE(APP_DEFAULTS$pb_adjust_share_class))
      sync_book_values()
      showNotification("P/B 參數已回復系統預設", type = "message")
    })
    
    output$alert_missing_bv <- renderUI({
      ui_missing_data_alert(
        check_list = list("BVPS" = input$bvps, "TBVPS" = input$tbvps),
        fallback_msg = "請先載入財報或手動輸入每股淨值，否則無法計算合理價。"
      )
    })
    
    # --- 核心計算 ---
    pb_calc <- eventReactive(input$btn_calc_pb, {
      basis_val <- if (identical(input$basis, "tbvps")) safe_num(input$tbvps) else safe_num(input$bvps)
      if (is.na(basis_val) || basis_val <= 0) {
        return(list(status = "error", message = "計算無效：請先提供有效的 BVPS／TBVPS（須 > 0）。"))
      }
      lo <- safe_num(input$pb_low)
      mid <- safe_num(input$pb_mid)
      hi <- safe_num(input$pb_high)
      if (lo <= 0 || mid <= 0 || hi <= 0) {
        return(list(status = "error", message = "目標 P/B 倍數必須大於 0。"))
      }
      if (lo > mid || mid > hi) {
        return(list(status = "error", message = "請維持 保守 ≤ 基準 ≤ 樂觀 的 P/B 順序。"))
      }
      
      px <- suppressWarnings(as.numeric(current_price()))
      mkt_pb <- if (length(px) == 1 && !is.na(px) && basis_val > 0) px / basis_val else NA_real_
      
      list(
        status = "success",
        basis = input$basis,
        basis_val = basis_val,
        fair_low = basis_val * lo,
        fair_mid = basis_val * mid,
        fair_high = basis_val * hi,
        pb_low = lo, pb_mid = mid, pb_high = hi,
        market_price = px,
        market_pb = mkt_pb
      )
    }, ignoreNULL = FALSE)
    
    # 初次／參數變更時若尚未按過也可顯示提示；正式結果依按鈕
    output$ui_pb_result <- renderUI({
      if (is.null(input$btn_calc_pb) || input$btn_calc_pb == 0) {
        return(div(style = "color: #7f8c8d; padding: 15px; text-align: center;",
                   "請確認 Settings 中的 BVPS 與目標 P/B，然後按下「試算 P/B 合理價」。"))
      }
      res <- pb_calc()
      if (res$status == "error") {
        return(div(style = "color: #d9534f; font-weight: bold; padding: 15px; background-color: #fdf2f2; border-left: 5px solid #d9534f; border-radius: 4px;",
                   icon("exclamation-triangle"), " ", res$message))
      }
      
      mkt_txt <- if (!is.na(res$market_price)) paste0("$", round(res$market_price, 2)) else "N/A"
      mkt_pb_txt <- if (!is.na(res$market_pb)) sprintf("%.2f×", res$market_pb) else "N/A"
      upside <- if (!is.na(res$market_price) && res$market_price > 0) {
        (res$fair_mid - res$market_price) / res$market_price * 100
      } else NA_real_
      upside_txt <- if (is.na(upside)) "N/A" else sprintf("%+.1f%%", upside)
      upside_color <- if (is.na(upside)) "#7f8c8d" else if (upside >= 15) "#00a65a" else if (upside <= -10) "#d9534f" else "#f39c12"
      
      div(style = "display: flex; justify-content: space-between; align-items: stretch; gap: 10px; padding: 20px; background-color: #fcfcfc; border: 1px solid #ddd; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.05); flex-wrap: wrap;",
          div(style = "text-align: center; flex: 1; min-width: 120px;",
              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "估值基礎"),
              p(style = "font-size: 22px; color: #2c3e50; font-weight: bold; margin: 0;", paste0("$", round(res$basis_val, 2))),
              p(style = "font-size: 12px; color: #95a5a6;", if (identical(res$basis, "tbvps")) "TBVPS" else "BVPS")
          ),
          div(style = "text-align: center; flex: 1; min-width: 120px;",
              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "基準目標價"),
              p(style = "font-size: 28px; color: #2980b9; font-weight: bold; margin: 0;", paste0("$", round(res$fair_mid, 2))),
              p(style = "font-size: 12px; color: #95a5a6;", sprintf("@ %.2f× P/B", res$pb_mid))
          ),
          div(style = "text-align: center; flex: 1; min-width: 120px;",
              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "市價／市價本淨比"),
              p(style = "font-size: 22px; color: #2c3e50; font-weight: bold; margin: 0;", mkt_txt),
              p(style = "font-size: 12px; color: #95a5a6;", mkt_pb_txt)
          ),
          div(style = "text-align: center; flex: 1; min-width: 140px; background-color: #eaf2f8; padding: 12px; border-radius: 8px; border-left: 4px solid #2980b9;",
              p(style = "font-size: 13px; color: #2471a3; margin-bottom: 5px; font-weight: bold;", "相對基準潛在報酬"),
              p(style = paste0("font-size: 28px; font-weight: bold; margin: 0; color: ", upside_color, ";"), upside_txt)
          )
      )
    })
    
    output$vbx_bvps <- renderValueBox({
      val <- input$bvps
      valueBox(
        if (is.null(val) || is.na(val)) "N/A" else paste0("$", round(val, 2)),
        "每股帳面淨值 BVPS", icon = icon("book"), color = "aqua"
      )
    })
    
    output$vbx_tbvps <- renderValueBox({
      val <- input$tbvps
      valueBox(
        if (is.null(val) || is.na(val)) "N/A" else paste0("$", round(val, 2)),
        "有形每股淨值 TBVPS", icon = icon("cube"), color = "light-blue"
      )
    })
    
    output$vbx_mkt_pb <- renderValueBox({
      basis_val <- if (identical(input$basis, "tbvps")) safe_num(input$tbvps) else safe_num(input$bvps)
      px <- suppressWarnings(as.numeric(current_price()))
      mkt_pb <- if (length(px) == 1 && !is.na(px) && !is.na(basis_val) && basis_val > 0) px / basis_val else NA
      valueBox(
        if (is.na(mkt_pb)) "N/A" else sprintf("%.2f×", mkt_pb),
        "目前市價本淨比", icon = icon("chart-bar"), color = "navy"
      )
    })
    
    output$tbl_pb_band <- renderTable({
      req(input$btn_calc_pb > 0)
      res <- pb_calc()
      req(res$status == "success")
      data.frame(
        情境 = c("保守", "基準", "樂觀"),
        `目標 P/B` = sprintf("%.2f×", c(res$pb_low, res$pb_mid, res$pb_high)),
        `合理股價` = sprintf("$%.2f", c(res$fair_low, res$fair_mid, res$fair_high)),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c", width = "100%")
    
    output$plt_pb_band <- renderPlot({
      req(input$btn_calc_pb > 0)
      res <- pb_calc()
      req(res$status == "success")
      
      df <- data.frame(
        Scenario = factor(c("保守", "基準", "樂觀"), levels = c("保守", "基準", "樂觀")),
        Price = c(res$fair_low, res$fair_mid, res$fair_high)
      )
      
      p <- ggplot(df, aes(x = Scenario, y = Price, fill = Scenario)) +
        geom_col(width = 0.55, alpha = 0.85) +
        geom_text(aes(label = format_dollar_abbr(Price)), vjust = -0.4, fontface = "bold", size = 4.2) +
        scale_fill_manual(values = c("保守" = "#7f8c8d", "基準" = "#2980b9", "樂觀" = "#27ae60")) +
        scale_y_continuous(labels = label_chart_number(prefix = "$")) +
        theme_minimal(base_size = 14) +
        labs(title = "P/B 合理價區間", x = NULL, y = "每股合理價 (USD)") +
        theme(legend.position = "none", plot.title = element_text(face = "bold")) +
        expand_limits(y = max(df$Price, na.rm = TRUE) * 1.15)
      
      if (!is.na(res$market_price)) {
        p <- p + geom_hline(yintercept = res$market_price, linetype = "dashed", color = "#c0392b", linewidth = 1) +
          annotate("text", x = 1.2, y = res$market_price,
                   label = paste0("市價 ", format_dollar_abbr(res$market_price)),
                   vjust = -0.6, color = "#c0392b", fontface = "bold")
      }
      p
    })
    
    return(list(
      pb_price = reactive({
        if (is.null(input$btn_calc_pb) || input$btn_calc_pb == 0) return(NA_real_)
        res <- pb_calc()
        if (identical(res$status, "success")) res$fair_mid else NA_real_
      }),
      pb_band = reactive({
        if (is.null(input$btn_calc_pb) || input$btn_calc_pb == 0) return(NULL)
        res <- pb_calc()
        if (!identical(res$status, "success")) return(NULL)
        list(low = res$fair_low, mid = res$fair_mid, high = res$fair_high,
             market_pb = res$market_pb, basis_val = res$basis_val)
      })
    ))
  })
}
