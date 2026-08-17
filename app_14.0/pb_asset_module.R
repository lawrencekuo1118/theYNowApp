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
                            column(3, valueBoxOutput(ns("vbx_bvps"), width = 12)),
                            column(3, valueBoxOutput(ns("vbx_tbvps"), width = 12)),
                            column(3, valueBoxOutput(ns("vbx_navps"), width = 12)),
                            column(3, valueBoxOutput(ns("vbx_mkt_pb"), width = 12))
                          ),
                          fluidRow(
                            div("Fair Price = (BVPS / TBVPS / NAVPS) × Target P/B　｜　NAV = Equity − holdco discount × investments",
                                style = "font-size: 16px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;")
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
                            div("標準公式：BVPS = Common Equity ÷ 流通股數；TBVPS 另扣除商譽／無形資產。控股 NAV＝權益 − 折價×已辨識投資科目。雙重股權／ADR 等「報價股數 ≠ 財報股數」時，與 DCF／RI／回測相同，一律以市值÷股價自動約當報價股。",
                                style = "font-size: 14px; font-weight: bold; color: #2C3E50; text-align: left; margin-bottom: 10px; padding: 10px; background-color: #F8F9F9; border-left: 4px solid #2980B9; border-radius: 4px;")
                          ),
                          uiOutput(ns("txt_shares_resolve_note")),
                          fluidRow(
                            column(4, numericInput(ns("bvps"), "每股帳面淨值 BVPS", value = APP_DEFAULTS$pb_bvps, step = 0.1, min = 0)),
                            column(4, numericInput(ns("tbvps"), "有形每股淨值 TBVPS", value = APP_DEFAULTS$pb_tbvps, step = 0.1, min = 0)),
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
                          fluidRow(
                            column(4, numericInput(ns("navps"), "每股淨資產 NAVPS", value = NA, step = 0.1, min = 0)),
                            column(4, numericInput(
                              ns("holdco_discount"), "控股折價 (%)",
                              value = APP_DEFAULTS$pb_holdco_discount * 100,
                              min = 0, max = 50, step = 1
                            )),
                            column(4, helpText("NAV＝股東權益 − 折價×已辨識投資科目。無投資科目時 NAV＝帳面權益。折價 0–50%。"))
                          ),
                          fluidRow(
                            column(12, tableOutput(ns("tbl_nav_breakdown")))
                          ),
                          hr(style = "border-top: 1px solid #BDC3C7;"),
                          h4(tags$b("目標本淨比假設")),
                          fluidRow(
                            column(4, numericInput(ns("pb_low"),  "保守 P/B (×)", value = APP_DEFAULTS$pb_low,  step = 0.05, min = 0.1)),
                            column(4, numericInput(ns("pb_mid"),  "基準 P/B (×)", value = APP_DEFAULTS$pb_mid,  step = 0.05, min = 0.1)),
                            column(4, numericInput(ns("pb_high"), "樂觀 P/B (×)", value = APP_DEFAULTS$pb_high, step = 0.05, min = 0.1))
                          ),
                          uiOutput(ns("ui_pb_source_note")),
                          fluidRow(
                            column(6,
                                   selectInput(ns("basis"), "估值基礎",
                                               choices = c("帳面淨值 BVPS" = "bvps",
                                                           "有形淨值 TBVPS" = "tbvps",
                                                           "控股 NAVPS" = "navps"),
                                               selected = APP_DEFAULTS$pb_basis)
                            ),
                            column(6,
                                   checkboxInput(ns("use_industry_pb"),
                                                 tags$span(style = "font-weight: bold;", "納入產業本淨比先驗（與 Justified／歷史合成）"),
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
                              p("適用於銀行、保險、控股／綜合企業：折現模型（DCF／DDM）前提常不成立時，以淨資產與合理本淨比定價。控股可改用 NAVPS（帳面 SOTP＋控股折價）。"),
                              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 0;",
                                "※ Buffett／Berkshire 實務常以 Book Value 為錨；目標 P/B 請依產業與利率環境調整，勿固定單一倍數。")
                          )
                 )
          ),
          .model_param_sensitivity_box(
            "P/B 公式參數：每股估值貢獻與敏感度",
            ns("param_sensitivity_table")
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
                                   quote_price = reactive(NA),
                                   current_ticker = reactive(""),
                                   quote_currency = reactive(NA),
                                   financial_currency = reactive(NA),
                                   industry_choice = reactive(NULL),
                                   industry_text = reactive(""),
                                   central_ke = reactive(NA),
                                   central_g_pct = reactive(NA),
                                   hist_prices = reactive(NULL),
                                   capm_rf = reactive(NA),
                                   capm_beta = reactive(NA),
                                   capm_rm = reactive(NA),
                                   use_estimated_re = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    
    nav_shares <- reactiveVal(NA_real_)
    nav_components <- reactiveVal(NULL)

    .pb_basis_val <- function() {
      basis <- as.character(input$basis %||% "bvps")[1]
      if (identical(basis, "tbvps")) return(safe_num(input$tbvps))
      if (identical(basis, "navps")) return(safe_num(input$navps))
      safe_num(input$bvps)
    }
    .pb_basis_label <- function(basis = NULL) {
      b <- as.character((basis %||% input$basis) %||% "bvps")[1]
      switch(b, tbvps = "TBVPS", navps = "NAVPS", "BVPS")
    }

    sync_navps <- function(df_bs, shares) {
      disc_pct <- suppressWarnings(as.numeric(input$holdco_discount)[1])
      if (!is.finite(disc_pct)) disc_pct <- APP_DEFAULTS$pb_holdco_discount * 100
      navc <- extract_nav_components(df_bs, holdco_discount = disc_pct / 100)
      nav_components(navc)
      if (is.finite(navc$nav) && is.finite(shares) && shares > 0) {
        updateNumericInput(session, "navps", value = round(navc$nav / shares, 2))
      }
    }

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
      txt <- paste(ind, industry_text(), collapse = " ")
      if (grepl("Insurance|Bank|Financial|fn\\.", txt, ignore.case = TRUE)) {
        return(list(low = 1.0, mid = 1.35, high = 1.7))
      }
      NULL
    })

    current_roe_pct <- reactive({
      d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
      d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      ni <- tryCatch(select_current_metric_any(d_is, NET_INCOME_PATTERNS, "flow"), error = function(e) NA_real_)
      eq <- tryCatch(select_current_metric_any(d_bs, EQUITY_PATTERNS, "stock"), error = function(e) NA_real_)
      if (is.finite(ni) && is.finite(eq) && eq > 0) ni / eq * 100 else NA_real_
    })

    hist_pb_series <- reactive({
      px <- tryCatch(hist_prices(), error = function(e) NULL)
      bv <- suppressWarnings(as.numeric(input$bvps)[1])
      if (is.null(px) || !is.data.frame(px) || !("Close" %in% names(px)) || !is.finite(bv) || bv <= 0) {
        return(NULL)
      }
      closes <- suppressWarnings(as.numeric(px$Close))
      closes <- closes[is.finite(closes) & closes > 0]
      if (length(closes) < 4) return(NULL)
      closes / bv
    })

    pb_targets_derived <- reactive({
      ind_band <- if (isTRUE(input$use_industry_pb)) industry_pb_band() else NULL
      derive_pb_targets(
        roe_pct = current_roe_pct(),
        ke_pct = suppressWarnings(as.numeric(central_ke())[1]) * 100,
        g_pct = suppressWarnings(as.numeric(central_g_pct())[1]),
        industry_band = ind_band,
        hist_pb = hist_pb_series()
      )
    })

    shares_resolve_note <- reactiveVal(NULL)
    pb_source_note <- reactiveVal("")

    output$ui_pb_source_note <- renderUI({
      d <- tryCatch(pb_targets_derived(), error = function(e) NULL)
      note <- pb_source_note()
      if (is.null(d) || !is.finite(suppressWarnings(as.numeric(d$mid)[1]))) {
        if (is.null(note) || !nzchar(note)) return(NULL)
        return(tags$div(
          style = "margin: 0 0 10px 0; padding: 8px 10px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; color: #475569; font-size: 12px;",
          note
        ))
      }
      fmt <- function(x) {
        x <- suppressWarnings(as.numeric(x)[1])
        if (!is.finite(x)) "—" else sprintf("%.2f", x)
      }
      tags$div(
        style = "margin: 0 0 10px 0; padding: 8px 10px; background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px; color: #1e3a8a; font-size: 12px;",
        HTML(sprintf(
          "P/B 來源：Justified <b>%s</b>（ROE/Ke）｜產業中位 <b>%s</b>｜歷史中位 <b>%s</b> → 建議 Bear/Base/Bull = <b>%.2f / %.2f / %.2f</b>%s",
          fmt(d$justified), fmt(d$industry_mid), fmt(d$history_mid),
          as.numeric(d$low), as.numeric(d$mid), as.numeric(d$high),
          if (nzchar(d$source_note %||% "")) paste0("<br/>", htmltools::htmlEscape(d$source_note)) else ""
        ))
      )
    })
    
    # --- 財報同步 BVPS / TBVPS（ADR／雙重股權：一律自動約當報價股）---
    sync_book_values <- function() {
      req(d_balance_sheet())
      df_bs <- d_balance_sheet()
      
      equity <- select_current_metric_any(df_bs, EQUITY_PATTERNS, "stock")
      
      shares_bs <- select_current_metric_any(
        df_bs,
        SHARE_PATTERNS,
        "stock"
      )
      px_quote <- suppressWarnings(as.numeric(quote_price())[1])
      if (!is.finite(px_quote) || px_quote <= 0) {
        px_quote <- suppressWarnings(as.numeric(current_price())[1])
      }
      mcap <- suppressWarnings(as.numeric(market_cap())[1])
      tk <- tryCatch(current_ticker(), error = function(e) "")
      sh_adj <- resolve_shares_for_price(
        shares_bs,
        price = px_quote,
        market_cap = mcap,
        ticker = tk,
        quote_currency = tryCatch(quote_currency(), error = function(e) NULL),
        financial_currency = tryCatch(financial_currency(), error = function(e) NULL)
      )

      auto_adj <- shares_auto_adjust_method(sh_adj$method)
      if (isTRUE(auto_adj) && is.finite(sh_adj$shares) && sh_adj$shares > 0) {
        shares <- sh_adj$shares
        shares_resolve_note(sh_adj$note)
      } else {
        if (is.finite(shares_bs) && shares_bs > 0) {
          shares <- shares_bs
        } else {
          shares <- sh_adj$shares
        }
        shares_resolve_note(NULL)
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
        nav_shares(shares)
        sync_navps(df_bs, shares)
        if (isTRUE(auto_adj) && !is.null(sh_adj$note) && nzchar(sh_adj$note)) {
          showNotification(sh_adj$note, type = "message", duration = 8)
        }
      } else {
        showNotification("無法從財報推算 BVPS，請手動輸入淨值與股數相關科目", type = "warning", duration = 6)
      }
    }
    
    observeEvent(
      list(
        d_balance_sheet(), current_price(), quote_price(), market_cap(), current_ticker()
      ),
      {
        sync_book_values()
      },
      ignoreInit = FALSE
    )
    
    observeEvent(input$btn_sync_bv, {
      sync_book_values()
      showNotification("已自財報同步 BVPS／TBVPS／NAVPS", type = "message")
    })

    observeEvent(input$holdco_discount, {
      df_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      sh <- suppressWarnings(as.numeric(nav_shares())[1])
      if (is.null(df_bs) || !is.finite(sh) || sh <= 0) return()
      sync_navps(df_bs, sh)
    }, ignoreInit = TRUE)
    
    output$txt_shares_resolve_note <- renderUI({
      note <- shares_resolve_note()
      if (is.null(note) || !nzchar(note)) return(NULL)
      tags$p(
        style = paste0(
          "font-size: 12px; margin: 0 0 10px 0; padding: 8px 10px; border-left: 3px solid #e67e22;",
          " background: #fff8e6; color: #b85c00;"
        ),
        paste0("已自動對齊報價股：", note)
      )
    })
    
    observeEvent(list(input$use_industry_pb, industry_choice(), industry_text(),
                      current_roe_pct(), central_ke(), central_g_pct(), hist_pb_series(),
                      input$bvps), {
      tgt <- pb_targets_derived()
      if (is.null(tgt) || !is.finite(tgt$mid)) return()
      # Always refresh when fundamentals/industry/history change (v13 derived multiples)
      updateNumericInput(session, "pb_low",  value = round(tgt$low, 2))
      updateNumericInput(session, "pb_mid",  value = round(tgt$mid, 2))
      updateNumericInput(session, "pb_high", value = round(tgt$high, 2))
      pb_source_note(tgt$source_note %||% "")
    }, ignoreInit = FALSE)
    
    observeEvent(input$btn_reset_pb, {
      tgt <- pb_targets_derived()
      if (!is.null(tgt) && is.finite(tgt$mid)) {
        updateNumericInput(session, "pb_low",  value = round(tgt$low, 2))
        updateNumericInput(session, "pb_mid",  value = round(tgt$mid, 2))
        updateNumericInput(session, "pb_high", value = round(tgt$high, 2))
      } else {
        updateNumericInput(session, "pb_low",  value = APP_DEFAULTS$pb_low)
        updateNumericInput(session, "pb_mid",  value = APP_DEFAULTS$pb_mid)
        updateNumericInput(session, "pb_high", value = APP_DEFAULTS$pb_high)
      }
      updateSelectInput(session, "basis", selected = APP_DEFAULTS$pb_basis)
      updateCheckboxInput(session, "use_industry_pb", value = APP_DEFAULTS$pb_use_industry)
      updateNumericInput(session, "holdco_discount", value = APP_DEFAULTS$pb_holdco_discount * 100)
      sync_book_values()
      showNotification("P/B 參數已依 Justified／產業／歷史重估", type = "message")
    })
    
    output$alert_missing_bv <- renderUI({
      ui_missing_data_alert(
        check_list = list("BVPS" = input$bvps, "TBVPS" = input$tbvps, "NAVPS" = input$navps),
        fallback_msg = "請先載入財報或手動輸入每股淨值，否則無法計算合理價。"
      )
    })

    output$tbl_nav_breakdown <- renderTable({
      navc <- nav_components()
      if (is.null(navc) || !is.finite(navc$equity)) {
        return(data.frame(科目 = "NAV 拆解", 金額 = "載入財報後顯示", check.names = FALSE))
      }
      sh <- suppressWarnings(as.numeric(nav_shares())[1])
      navps <- if (is.finite(navc$nav) && is.finite(sh) && sh > 0) navc$nav / sh else NA_real_
      fmt <- function(x) {
        if (!is.finite(x)) return("N/A")
        format(round(x, 0), big.mark = ",", scientific = FALSE)
      }
      data.frame(
        科目 = c("股東權益", "已辨識投資", "控股折價", "NAV", "NAVPS", "說明"),
        金額 = c(
          fmt(navc$equity),
          fmt(navc$investments),
          sprintf("%.0f%%", 100 * (navc$discount %||% 0)),
          fmt(navc$nav),
          if (is.finite(navps)) sprintf("%.2f", navps) else "N/A",
          as.character(navc$note %||% "")
        ),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
    
    # --- 核心計算 ---
    pb_calc <- eventReactive(input$btn_calc_pb, {
      basis_val <- .pb_basis_val()
      if (is.na(basis_val) || basis_val <= 0) {
        return(list(status = "error", message = "計算無效：請先提供有效的 BVPS／TBVPS／NAVPS（須 > 0）。"))
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
                   "請確認 Settings 中的 BVPS／TBVPS／NAVPS 與目標 P/B，然後按下「試算 P/B 合理價」。"))
      }
      res <- pb_calc()
      if (res$status == "error") {
        return(div(style = "color: #d9534f; font-weight: bold; padding: 15px; background-color: #fdf2f2; border-left: 5px solid #d9534f; border-radius: 4px;",
                   icon("exclamation-triangle"), " ", res$message))
      }
      
      mkt_txt <- if (!is.na(res$market_price)) paste0(money_prefix(), round(res$market_price, 2)) else "N/A"
      mkt_pb_txt <- if (!is.na(res$market_pb)) sprintf("%.2f×", res$market_pb) else "N/A"
      upside <- if (!is.na(res$market_price) && res$market_price > 0) {
        (res$fair_mid - res$market_price) / res$market_price * 100
      } else NA_real_
      upside_txt <- if (is.na(upside)) "N/A" else sprintf("%+.1f%%", upside)
      upside_color <- if (is.na(upside)) "#7f8c8d" else if (upside >= 15) "#00a65a" else if (upside <= -10) "#d9534f" else "#f39c12"
      
      div(style = "display: flex; justify-content: space-between; align-items: stretch; gap: 10px; padding: 20px; background-color: #fcfcfc; border: 1px solid #ddd; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.05); flex-wrap: wrap;",
          div(style = "text-align: center; flex: 1; min-width: 120px;",
              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "估值基礎"),
              p(style = "font-size: 22px; color: #2c3e50; font-weight: bold; margin: 0;", paste0(money_prefix(), round(res$basis_val, 2))),
              p(style = "font-size: 12px; color: #95a5a6;", .pb_basis_label(res$basis))
          ),
          div(style = "text-align: center; flex: 1; min-width: 120px;",
              p(style = "font-size: 13px; color: #7f8c8d; margin-bottom: 5px; font-weight: bold;", "基準目標價"),
              p(style = "font-size: 28px; color: #2980b9; font-weight: bold; margin: 0;", paste0(money_prefix(), round(res$fair_mid, 2))),
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
        if (is.null(val) || is.na(val)) "N/A" else paste0(money_prefix(), round(val, 2)),
        "每股帳面淨值 BVPS", icon = icon("book"), color = "aqua"
      )
    })
    
    output$vbx_tbvps <- renderValueBox({
      val <- input$tbvps
      valueBox(
        if (is.null(val) || is.na(val)) "N/A" else paste0(money_prefix(), round(val, 2)),
        "有形每股淨值 TBVPS", icon = icon("cube"), color = "light-blue"
      )
    })
    
    output$vbx_navps <- renderValueBox({
      val <- input$navps
      valueBox(
        if (is.null(val) || is.na(val)) "N/A" else paste0(money_prefix(), round(val, 2)),
        "每股淨資產 NAVPS", icon = icon("sitemap"), color = "olive"
      )
    })
    
    output$vbx_mkt_pb <- renderValueBox({
      basis_val <- .pb_basis_val()
      px <- suppressWarnings(as.numeric(current_price()))
      mkt_pb <- if (length(px) == 1 && !is.na(px) && !is.na(basis_val) && basis_val > 0) px / basis_val else NA
      valueBox(
        if (is.na(mkt_pb)) "N/A" else sprintf("%.2f×", mkt_pb),
        paste0("目前市價／", .pb_basis_label()), icon = icon("chart-bar"), color = "navy"
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
        scale_y_continuous(labels = label_chart_number(prefix = money_prefix())) +
        theme_minimal(base_size = 14) +
        labs(title = "P/B 合理價區間", x = NULL, y = paste0("每股合理價 (", money_label(), ")")) +
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
    
    # Live band from current inputs (Dashboard / confidence); button still gates Overview UI
    pb_live_band <- reactive({
      basis_val <- .pb_basis_val()
      lo <- safe_num(input$pb_low)
      mid <- safe_num(input$pb_mid)
      hi <- safe_num(input$pb_high)
      if (is.na(basis_val) || basis_val <= 0 || lo <= 0 || mid <= 0 || hi <= 0) return(NULL)
      list(
        low = basis_val * lo,
        mid = basis_val * mid,
        high = basis_val * hi,
        basis_val = basis_val
      )
    })

    output$param_sensitivity_table <- renderTable({
      shock_pct <- if (exists("PARAM_SENSITIVITY_SHOCK", inherits = TRUE)) PARAM_SENSITIVITY_SHOCK else 0.01
      basis0 <- .pb_basis_val()
      mid0 <- safe_num(input$pb_mid)
      .p <- function(basis_u = 1, pb = mid0) .pb_formula_p(basis = basis_u, pb = pb)
      v0 <- .p()
      validate(need(
        is.finite(v0),
        "基準公式尚未就緒：請先提供目標 P/B（BVPS／TBVPS／NAVPS 僅影響顯示基準值）。"
      ))
      .rel <- function(x, sign = -1) .param_rel_shock(x, sign = sign, shock = shock_pct)
      basis_show <- if (is.finite(basis0) && basis0 > 0) basis0 else 1
      basis_unit <- if (is.finite(basis0) && basis0 > 0) money_prefix() else "x"
      rows <- list(
        .param_sensitivity_infl_row(
          .pb_basis_label(),
          basis_show, basis_unit, v0,
          .p(basis_u = 1 - shock_pct), .p(basis_u = 1 + shock_pct),
          "P ∝ 選定基礎 ⇒ |ε|=1（公式；與帳面金額／股價無關）"
        )
      )
      if (.param_sensitivity_rel_ok(mid0) && mid0 > 0) {
        rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
          "基準目標 P/B", mid0, "x", v0,
          .p(pb = .rel(mid0, -1)), .p(pb = .rel(mid0, +1)),
          "P ∝ 目標 P/B ⇒ |ε|=1（公式；與個股無關）"
        )
      }
      roe0 <- tryCatch(current_roe_pct(), error = function(e) NA_real_)
      ke0 <- suppressWarnings(as.numeric(central_ke())[1]) * 100
      g0 <- suppressWarnings(as.numeric(central_g_pct())[1])
      ind_band <- if (isTRUE(input$use_industry_pb)) {
        tryCatch(industry_pb_band(), error = function(e) NULL)
      } else {
        NULL
      }
      hist0 <- tryCatch(hist_pb_series(), error = function(e) NULL)
      tgt0 <- tryCatch(
        derive_pb_targets(
          roe_pct = roe0, ke_pct = ke0, g_pct = g0,
          industry_band = ind_band, hist_pb = hist0
        ),
        error = function(e) NULL
      )
      just0 <- if (!is.null(tgt0)) suppressWarnings(as.numeric(tgt0$justified)[1]) else NA_real_
      .p_tgt <- function(roe = roe0, ke = ke0, g = g0, band = ind_band) {
        d <- derive_pb_targets(
          roe_pct = roe, ke_pct = ke, g_pct = g,
          industry_band = band, hist_pb = hist0
        )
        mid <- suppressWarnings(as.numeric(d$mid)[1])
        .pb_formula_p(basis = 1, pb = mid)
      }
      if (is.finite(just0)) {
        if (.param_sensitivity_rel_ok(roe0)) {
          rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
            "Justified ROE", roe0, "%", v0,
            .p_tgt(roe = .rel(roe0, -1)), .p_tgt(roe = .rel(roe0, +1)),
            "目標 P/B：(ROE−g)/(Ke−g) 再與產業／歷史合成"
          )
        }
        if (.param_sensitivity_rel_ok(ke0)) {
          rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
            "股權成本 Ke", ke0, "%", v0,
            .p_tgt(ke = .rel(ke0, -1)), .p_tgt(ke = .rel(ke0, +1)),
            "Justified P/B 分母 Ke−g（合成後目標）"
          )
        }
        if (.param_sensitivity_rel_ok(g0)) {
          rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
            "永續成長率 g", g0, "%", v0,
            .p_tgt(g = .rel(g0, -1)), .p_tgt(g = .rel(g0, +1)),
            "Justified P/B：(ROE−g)/(Ke−g)"
          )
        }
        if (isTRUE(use_estimated_re())) {
          rf0 <- suppressWarnings(as.numeric(capm_rf())[1])
          beta0 <- suppressWarnings(as.numeric(capm_beta())[1])
          rm0 <- suppressWarnings(as.numeric(capm_rm())[1])
          capm_rows <- .param_sensitivity_capm_ke_rows(
            v0, function(ke_pct) .p_tgt(ke = ke_pct),
            rf0, beta0, rm0, shock = shock_pct
          )
          if (length(capm_rows)) rows <- c(rows, capm_rows)
        }
      }
      ind_mid <- if (is.list(ind_band)) suppressWarnings(as.numeric(ind_band$mid)[1]) else NA_real_
      if (isTRUE(input$use_industry_pb) && .param_sensitivity_rel_ok(ind_mid) && ind_mid > 0) {
        .band_at <- function(mid) {
          b <- ind_band
          b$mid <- mid
          b
        }
        rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
          "產業 P/B 先驗", ind_mid, "x", v0,
          .p_tgt(band = .band_at(.rel(ind_mid, -1))),
          .p_tgt(band = .band_at(.rel(ind_mid, +1))),
          "derive_pb_targets 產業權重來源"
        )
      }
      .param_sensitivity_sort_by_abs_eps(do.call(rbind, rows))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", width = "100%")

    return(list(
      pb_price = reactive({
        live <- pb_live_band()
        if (!is.null(live) && is.finite(live$mid)) return(live$mid)
        if (is.null(input$btn_calc_pb) || input$btn_calc_pb == 0) return(NA_real_)
        res <- pb_calc()
        if (identical(res$status, "success")) res$fair_mid else NA_real_
      }),
      pb_band = reactive({
        live <- pb_live_band()
        if (!is.null(live)) {
          return(list(
            low = live$low, mid = live$mid, high = live$high,
            basis_val = live$basis_val
          ))
        }
        if (is.null(input$btn_calc_pb) || input$btn_calc_pb == 0) return(NULL)
        res <- pb_calc()
        if (!identical(res$status, "success")) return(NULL)
        list(low = res$fair_low, mid = res$fair_mid, high = res$fair_high,
             market_pb = res$market_pb, basis_val = res$basis_val)
      })
    ))
  })
}
