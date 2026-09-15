# ==========================================
# nav_module.R — 純 NAV（帳面控股淨資產）
# 獨立估值模型：合理價 = NAVPS × 折價／溢價倍數
# 不依賴 P/B 倍數、Justified P/B、SGR。
# NAV = Equity − holdco_discount × identified investments（帳面拆解，非市場法分部 SOTP）
# ==========================================

.nav_formula_p <- function(navps, multiple = 1) {
  navps <- suppressWarnings(as.numeric(navps)[1])
  multiple <- suppressWarnings(as.numeric(multiple)[1])
  if (!is.finite(navps) || navps <= 0 || !is.finite(multiple) || multiple <= 0) {
    return(NA_real_)
  }
  navps * multiple
}

# ==========================================
# UI
# ==========================================
nav_module_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "nav_calculator",
    fluidRow(
      column(
        12,
        div(
          style = paste(
            "margin:0 0 12px 0; padding:10px 12px; background:#f8fafc;",
            "border:1px solid #e2e8f0; border-radius:8px; font-size:13px; color:#334155;"
          ),
          tags$b("純 NAV｜帳面控股淨資產："),
          "評估「現在家底（存量）」。",
          tags$span(style = "color:#64748b;", "非市場法分部 SOTP；不使用 Justified P/B／SGR。")
        )
      )
    ),
    tabBox(
      title = "NET ASSET VALUE", width = "auto",

      tabPanel(
        "NAV Overview", icon = icon("sitemap"),
        fluidRow(
          column(4, valueBoxOutput(ns("vbx_navps"), width = 12)),
          column(4, valueBoxOutput(ns("vbx_nav_mid"), width = 12)),
          column(4, valueBoxOutput(ns("vbx_mkt"), width = 12))
        ),
        fluidRow(
          div(
            "Fair Price = NAVPS × NAV multiple　｜　NAV = Equity − holdco discount × investments",
            style = paste(
              "font-size:16px; font-weight:bold; color:#2C3E50; text-align:center;",
              "margin-bottom:15px; padding:10px; background-color:#F2F4F4; border-radius:8px;"
            )
          )
        ),
        fluidRow(
          div(
            style = "text-align:center; margin-bottom:20px;",
            actionButton(
              ns("btn_calc_nav"), "試算 NAV 合理價",
              style = paste(
                "background-color:#1a1a1a; color:white; font-weight:bold; font-size:18px;",
                "padding:12px 30px; border-radius:8px; border:none;",
                "box-shadow:0 4px 6px rgba(0,0,0,0.1);"
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            uiOutput(ns("ui_nav_result")),
            br(),
            box(
              title = "估值區間（保守／基準／樂觀）", width = 12,
              status = "primary", solidHeader = TRUE,
              tableOutput(ns("tbl_nav_band")),
              plotOutput(ns("plt_nav_band"), height = "280px")
            )
          )
        )
      ),

      tabPanel(
        "NAV Settings", icon = icon("cogs"),
        h4(tags$b("控股 NAV 與折價參數")),
        fluidRow(
          div(
            paste0(
              "NAV＝股東權益 − 控股折價×已辨識投資科目（帳面拆解）。",
              "無獨立投資科目時 NAV＝帳面權益。",
              "雙重股權／ADR 時與 DCF／P/B／RI 相同，以市值÷股價約當報價股。"
            ),
            style = paste(
              "font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
              "margin-bottom:10px; padding:10px; background-color:#F8F9F9;",
              "border-left:4px solid #1a1a1a; border-radius:4px;"
            )
          )
        ),
        uiOutput(ns("txt_shares_resolve_note")),
        fluidRow(
          column(4, numericInput(ns("navps"), "每股淨資產 NAVPS", value = NA, step = 0.1, min = 0)),
          column(4, numericInput(
            ns("holdco_discount"), "控股折價 (%)",
            value = (APP_DEFAULTS$nav_holdco_discount %||% 0) * 100,
            min = 0, max = 50, step = 1
          )),
          column(4, br(), actionButton(
            ns("btn_sync_nav"), "從最新財報自動帶入",
            icon = icon("sync"), class = "btn-sm",
            style = paste(
              "background-color:#1a1a1a; color:white; border:none;",
              "padding:8px 15px; font-weight:bold; border-radius:5px; margin-top:5px;"
            )
          ))
        ),
        fluidRow(column(12, uiOutput(ns("alert_missing_nav")))),
        fluidRow(column(12, tableOutput(ns("tbl_nav_breakdown")))),
        hr(style = "border-top:1px solid #BDC3C7;"),
        h4(tags$b("NAV 折價／溢價倍數（Bear／Base／Bull）")),
        helpText("合理價 = NAVPS × 倍數。×1.00＝平價帳面 NAV；不使用產業／歷史 P/B 或 Justified／SGR。"),
        fluidRow(
          column(4, numericInput(
            ns("nav_low"), "保守 NAV 倍數 (×)",
            value = APP_DEFAULTS$nav_low %||% 0.90, step = 0.05, min = 0.1
          )),
          column(4, numericInput(
            ns("nav_mid"), "基準 NAV 倍數 (×)",
            value = APP_DEFAULTS$nav_mid %||% 1.00, step = 0.05, min = 0.1
          )),
          column(4, numericInput(
            ns("nav_high"), "樂觀 NAV 倍數 (×)",
            value = APP_DEFAULTS$nav_high %||% 1.05, step = 0.05, min = 0.1
          ))
        ),
        fluidRow(
          column(12, actionButton(
            ns("btn_reset_nav"), "回復系統預設參數",
            icon = icon("undo"), class = "btn-sm",
            style = "background-color:#7f8c8d; color:white; border:none; margin-top:10px;"
          ))
        )
      )
    )
  )
}

# ==========================================
# Server
# ==========================================
nav_module_server <- function(id,
                              auto_calc_pulse = reactive(0L),
                              d_balance_sheet,
                              current_price = reactive(NA),
                              market_cap = reactive(NA),
                              quote_price = reactive(NA),
                              current_ticker = reactive(""),
                              quote_currency = reactive(NA),
                              financial_currency = reactive(NA)) {
  moduleServer(id, function(input, output, session) {
    nav_shares <- reactiveVal(NA_real_)
    nav_components <- reactiveVal(NULL)
    shares_resolve_note <- reactiveVal(NULL)

    sync_nav_from_bs <- function() {
      req(d_balance_sheet())
      df_bs <- d_balance_sheet()
      shares_bs <- select_current_metric_any(df_bs, SHARE_PATTERNS, "stock")
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
      q_ccy0 <- tryCatch(quote_currency(), error = function(e) NA_character_)
      f_ccy0 <- tryCatch(financial_currency(), error = function(e) NA_character_)
      if (isTRUE(auto_adj) && is.finite(sh_adj$shares) && sh_adj$shares > 0) {
        shares <- sh_adj$shares
        shares_resolve_note(sh_adj$note)
      } else if (statement_quote_units_differ(f_ccy0, q_ccy0)) {
        shares <- NA_real_
        shares_resolve_note("報價幣≠財報幣且無法約當 ADR 股數，不顯示每股淨資產")
      } else {
        shares <- if (is.finite(shares_bs) && shares_bs > 0) shares_bs else sh_adj$shares
        shares_resolve_note(NULL)
      }
      if (!is.finite(shares) || shares <= 0) {
        showNotification("無法推算股數，請檢查財報／市值。", type = "warning", duration = 6)
        return(invisible(NULL))
      }
      nav_shares(shares)
      disc_pct <- suppressWarnings(as.numeric(input$holdco_discount)[1])
      if (!is.finite(disc_pct)) disc_pct <- (APP_DEFAULTS$nav_holdco_discount %||% 0) * 100
      navc <- extract_nav_components(df_bs, holdco_discount = disc_pct / 100)
      nav_components(navc)
      if (!is.finite(navc$nav)) {
        showNotification("無法計算 NAV（缺少股東權益）。", type = "warning", duration = 6)
        return(invisible(NULL))
      }
      q_ccy <- tryCatch(quote_currency(), error = function(e) NA_character_)
      f_ccy <- tryCatch(financial_currency(), error = function(e) NA_character_)
      eq_ccy <- equity_money_ccy(df_bs, session_ccy = q_ccy, statement_ccy = f_ccy)
      navps <- per_share_in_quote(
        navc$nav, shares,
        equity_ccy = eq_ccy,
        to_ccy = q_ccy,
        share_method = sh_adj$method %||% "balance_sheet",
        statement_ccy = f_ccy
      )
      if (is.finite(navps)) {
        updateNumericInput(session, "navps", value = round(navps, 2))
        if (isTRUE(auto_adj) && !is.null(sh_adj$note) && nzchar(sh_adj$note)) {
          showNotification(sh_adj$note, type = "message", duration = 8)
        }
      }
      invisible(NULL)
    }

    observeEvent(
      list(d_balance_sheet(), current_price(), quote_price(), market_cap(), current_ticker()),
      { sync_nav_from_bs() },
      ignoreInit = FALSE
    )
    observeEvent(input$btn_sync_nav, {
      sync_nav_from_bs()
      showNotification("已自財報同步 NAV／NAVPS", type = "message")
    })
    observeEvent(input$holdco_discount, {
      df_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      sh <- suppressWarnings(as.numeric(nav_shares())[1])
      if (is.null(df_bs) || !is.finite(sh) || sh <= 0) return()
      sync_nav_from_bs()
    }, ignoreInit = TRUE)

    observeEvent(input$btn_reset_nav, {
      updateNumericInput(session, "holdco_discount",
                         value = (APP_DEFAULTS$nav_holdco_discount %||% 0) * 100)
      updateNumericInput(session, "nav_low", value = APP_DEFAULTS$nav_low %||% 0.90)
      updateNumericInput(session, "nav_mid", value = APP_DEFAULTS$nav_mid %||% 1.00)
      updateNumericInput(session, "nav_high", value = APP_DEFAULTS$nav_high %||% 1.05)
      sync_nav_from_bs()
      showNotification("NAV 參數已回復系統預設", type = "message")
    })

    output$txt_shares_resolve_note <- renderUI({
      note <- shares_resolve_note()
      if (is.null(note) || !nzchar(note)) return(NULL)
      tags$p(
        style = paste0(
          "font-size:12px; margin:0 0 10px 0; padding:8px 10px; border-left:3px solid #e67e22;",
          " background:#fff8e6; color:#b85c00;"
        ),
        paste0("已自動對齊報價股：", note)
      )
    })

    output$alert_missing_nav <- renderUI({
      ui_missing_data_alert(
        check_list = list("NAVPS" = input$navps),
        fallback_msg = "請先載入財報或手動輸入 NAVPS，否則無法計算合理價。"
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

    .nav_calc_requested <- function() {
      btn <- suppressWarnings(as.integer(input$btn_calc_nav)[1])
      pulse <- suppressWarnings(as.integer(auto_calc_pulse())[1])
      (is.finite(btn) && btn >= 1L) || (is.finite(pulse) && pulse >= 1L)
    }

    nav_calc <- eventReactive(list(input$btn_calc_nav, auto_calc_pulse()), {
      if (!isTRUE(.nav_calc_requested())) return(list(status = "idle"))
      navps <- safe_num(input$navps)
      if (!is.finite(navps) || navps <= 0) {
        return(list(status = "error", message = "計算無效：請先提供有效的 NAVPS（須 > 0）。"))
      }
      lo <- safe_num(input$nav_low)
      mid <- safe_num(input$nav_mid)
      hi <- safe_num(input$nav_high)
      if (lo <= 0 || mid <= 0 || hi <= 0) {
        return(list(status = "error", message = "NAV 倍數必須大於 0。"))
      }
      if (lo > mid || mid > hi) {
        return(list(status = "error", message = "請維持 保守 ≤ 基準 ≤ 樂觀 的 NAV 倍數順序。"))
      }
      px <- suppressWarnings(as.numeric(current_price())[1])
      list(
        status = "success",
        navps = navps,
        fair_low = .nav_formula_p(navps, lo),
        fair_mid = .nav_formula_p(navps, mid),
        fair_high = .nav_formula_p(navps, hi),
        nav_low = lo, nav_mid = mid, nav_high = hi,
        market_price = px
      )
    }, ignoreNULL = FALSE)

    nav_live_band <- reactive({
      navps <- safe_num(input$navps)
      lo <- safe_num(input$nav_low)
      mid <- safe_num(input$nav_mid)
      hi <- safe_num(input$nav_high)
      if (!is.finite(navps) || navps <= 0 || lo <= 0 || mid <= 0 || hi <= 0) return(NULL)
      list(
        low = .nav_formula_p(navps, lo),
        mid = .nav_formula_p(navps, mid),
        high = .nav_formula_p(navps, hi),
        navps = navps
      )
    })

    output$ui_nav_result <- renderUI({
      res <- nav_calc()
      if (identical(res$status, "idle")) {
        return(div(
          style = "color:#7f8c8d; padding:15px; text-align:center;",
          "請確認 NAV Settings 的 NAVPS 與倍數，然後按下「試算 NAV 合理價」。"
        ))
      }
      if (identical(res$status, "error")) {
        return(div(
          style = paste(
            "color:#d9534f; font-weight:bold; padding:15px; background-color:#fdf2f2;",
            "border-left:5px solid #d9534f; border-radius:4px;"
          ),
          icon("exclamation-triangle"), " ", res$message
        ))
      }
      mkt_txt <- if (is.finite(res$market_price)) {
        paste0(money_prefix(), round(res$market_price, 2))
      } else {
        "N/A"
      }
      upside <- if (is.finite(res$market_price) && res$market_price > 0) {
        (res$fair_mid - res$market_price) / res$market_price * 100
      } else {
        NA_real_
      }
      upside_txt <- if (is.na(upside)) "N/A" else sprintf("%+.1f%%", upside)
      upside_color <- if (is.na(upside)) {
        "#7f8c8d"
      } else if (upside >= 15) {
        "#00a65a"
      } else if (upside <= -10) {
        "#d9534f"
      } else {
        "#f39c12"
      }
      div(
        style = paste(
          "display:flex; justify-content:space-between; align-items:stretch; gap:10px;",
          "padding:20px; background-color:#fcfcfc; border:1px solid #ddd; border-radius:10px;",
          "box-shadow:0 4px 6px rgba(0,0,0,0.05); flex-wrap:wrap;"
        ),
        div(
          style = "text-align:center; flex:1; min-width:120px;",
          p(style = "font-size:13px; color:#7f8c8d; margin-bottom:5px; font-weight:bold;", "NAVPS"),
          p(style = "font-size:22px; color:#2c3e50; font-weight:bold; margin:0;",
            paste0(money_prefix(), round(res$navps, 2)))
        ),
        div(
          style = "text-align:center; flex:1; min-width:120px;",
          p(style = "font-size:13px; color:#7f8c8d; margin-bottom:5px; font-weight:bold;", "基準目標價"),
          p(style = "font-size:28px; color:#1a1a1a; font-weight:bold; margin:0;",
            paste0(money_prefix(), round(res$fair_mid, 2))),
          p(style = "font-size:12px; color:#95a5a6;", sprintf("@ %.2f× NAV", res$nav_mid))
        ),
        div(
          style = "text-align:center; flex:1; min-width:120px;",
          p(style = "font-size:13px; color:#7f8c8d; margin-bottom:5px; font-weight:bold;", "市價"),
          p(style = "font-size:22px; color:#2c3e50; font-weight:bold; margin:0;", mkt_txt)
        ),
        div(
          style = paste(
            "text-align:center; flex:1; min-width:140px; background-color:#f5f5f5;",
            "padding:12px; border-radius:8px; border-left:4px solid #1a1a1a;"
          ),
          p(style = "font-size:13px; color:#2471a3; margin-bottom:5px; font-weight:bold;", "相對基準潛在報酬"),
          p(style = paste0("font-size:28px; font-weight:bold; margin:0; color:", upside_color, ";"),
            upside_txt)
        )
      )
    })

    output$vbx_navps <- renderValueBox({
      val <- input$navps
      valueBox(
        if (is.null(val) || is.na(val)) "N/A" else paste0(money_prefix(), round(val, 2)),
        "每股淨資產 NAVPS", icon = icon("sitemap"), color = "olive"
      )
    })
    output$vbx_nav_mid <- renderValueBox({
      live <- nav_live_band()
      valueBox(
        if (is.null(live) || !is.finite(live$mid)) "N/A" else paste0(money_prefix(), round(live$mid, 2)),
        "基準 NAV 合理價", icon = icon("balance-scale"), color = "aqua"
      )
    })
    output$vbx_mkt <- renderValueBox({
      px <- suppressWarnings(as.numeric(current_price())[1])
      valueBox(
        if (!is.finite(px)) "N/A" else paste0(money_prefix(), round(px, 2)),
        "目前市價", icon = icon("chart-line"), color = "maroon"
      )
    })

    output$tbl_nav_band <- renderTable({
      req(.nav_calc_requested())
      res <- nav_calc()
      req(identical(res$status, "success"))
      data.frame(
        情境 = c("保守", "基準", "樂觀"),
        `NAV 倍數` = sprintf("%.2f×", c(res$nav_low, res$nav_mid, res$nav_high)),
        `合理股價` = sprintf("$%.2f", c(res$fair_low, res$fair_mid, res$fair_high)),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c", width = "100%")

    output$plt_nav_band <- renderPlot({
      req(.nav_calc_requested())
      res <- nav_calc()
      req(identical(res$status, "success"))
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
        labs(title = "NAV 合理價區間", x = NULL, y = paste0("每股合理價 (", money_label(), ")")) +
        theme(legend.position = "none", plot.title = element_text(face = "bold")) +
        expand_limits(y = max(df$Price, na.rm = TRUE) * 1.15)
      if (is.finite(res$market_price)) {
        p <- p +
          geom_hline(yintercept = res$market_price, linetype = "dashed", color = "#c0392b", linewidth = 1) +
          annotate(
            "text", x = 1.2, y = res$market_price,
            label = paste0("市價 ", format_dollar_abbr(res$market_price)),
            vjust = -0.6, color = "#c0392b", fontface = "bold"
          )
      }
      p
    })

    return(list(
      nav_price = reactive({
        live <- nav_live_band()
        if (!is.null(live) && is.finite(live$mid)) return(live$mid)
        res <- tryCatch(nav_calc(), error = function(e) NULL)
        if (!is.null(res) && identical(res$status, "success")) res$fair_mid else NA_real_
      }),
      nav_band = reactive({
        live <- nav_live_band()
        if (!is.null(live)) {
          return(list(low = live$low, mid = live$mid, high = live$high, navps = live$navps))
        }
        res <- tryCatch(nav_calc(), error = function(e) NULL)
        if (is.null(res) || !identical(res$status, "success")) return(NULL)
        list(low = res$fair_low, mid = res$fair_mid, high = res$fair_high, navps = res$navps)
      })
    ))
  })
}
