# ==========================================
# ri_module.R - Residual Income Model (enhanced)
# 金融股／重資產／負 FCFF 但帳面淨值具參考性之企業
# ==========================================
# Engine helpers are pure (no Shiny) so sensitivity can recompute every cell.
# Module return contract preserved: list(ri_price = reactive(...))
# ==========================================

# ---------- pure helpers ----------

.ri_clip <- function(x, lo, hi) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return(NA_real_)
  max(lo, min(hi, x))
}

#' Industry average ROE (%) from `industry_standards` band (midpoint).
.industry_roe_pct <- function(industry_key) {
  if (is.null(industry_key) || !nzchar(as.character(industry_key)[1])) return(NA_real_)
  if (!exists("industry_standards", inherits = TRUE)) return(NA_real_)
  std <- industry_standards[[as.character(industry_key)[1]]]
  if (is.null(std) || is.null(std$roe)) return(NA_real_)
  roe <- suppressWarnings(as.numeric(std$roe))
  roe <- roe[is.finite(roe)]
  if (length(roe) < 1L) return(NA_real_)
  # Prefer explicit mid (3rd element) when present; else midpoint of band
  if (length(roe) >= 3L) roe[3L] else mean(roe)
}

#' Build ROE path (decimal) for n forecast years.
#' @param method one of constant|linear|industry|custom
#' @param roe_start / roe_terminal / roe_industry in decimal
#' @param custom_vec optional numeric vector (decimal); recycled/truncated to n
build_roe_path <- function(method = "constant",
                           n = 5,
                           roe_start = 0.15,
                           roe_terminal = 0.12,
                           roe_industry = 0.12,
                           custom_vec = NULL) {
  n <- max(1L, as.integer(n)[1])
  method <- match.arg(method, c("constant", "linear", "industry", "custom"))
  roe_start <- as.numeric(roe_start)[1]
  if (!is.finite(roe_start)) roe_start <- 0.15

  if (method == "constant") {
    return(rep(roe_start, n))
  }
  if (method == "linear") {
    rt <- as.numeric(roe_terminal)[1]
    if (!is.finite(rt)) rt <- roe_start
    if (n == 1L) return(roe_start)
    return(as.numeric(seq(roe_start, rt, length.out = n)))
  }
  if (method == "industry") {
    ri <- as.numeric(roe_industry)[1]
    if (!is.finite(ri)) ri <- 0.12
    if (n == 1L) return(roe_start)
    return(as.numeric(seq(roe_start, ri, length.out = n)))
  }
  # custom
  v <- suppressWarnings(as.numeric(custom_vec))
  v <- v[is.finite(v)]
  if (length(v) < 1L) v <- roe_start
  if (length(v) < n) v <- c(v, rep(utils::tail(v, 1), n - length(v)))
  if (length(v) > n) v <- v[seq_len(n)]
  as.numeric(v)
}

#' Core RI valuation (per-share).
#' @return list with status, components, forecast df, warnings
compute_ri_valuation <- function(b0,
                                 ke,
                                 g,
                                 n,
                                 payout,
                                 roe_path,
                                 validate = TRUE) {
  b0 <- suppressWarnings(as.numeric(b0)[1])
  ke <- suppressWarnings(as.numeric(ke)[1])
  g <- suppressWarnings(as.numeric(g)[1])
  n <- max(1L, as.integer(n)[1])
  payout <- suppressWarnings(as.numeric(payout)[1])
  if (!is.finite(payout)) payout <- 0
  payout <- max(0, min(1, payout))

  warnings <- character(0)
  if (!is.finite(b0)) {
    return(list(status = "error", message = "B0 無效，無法計算。", warnings = warnings))
  }
  if (validate && b0 <= 0) {
    warnings <- c(warnings, "Book Value (B0) ≤ 0：RI 模型可能不可靠。")
  }
  if (!is.finite(ke) || !is.finite(g)) {
    return(list(status = "error", message = "Ke 或 g 無效。", warnings = warnings))
  }
  if (g >= ke) {
    return(list(
      status = "error",
      message = "無法計算：永續成長率 g 必須嚴格小於股權成本 Ke（g ≥ Ke）。",
      warnings = warnings
    ))
  }

  roe_path <- as.numeric(roe_path)
  if (length(roe_path) != n || any(!is.finite(roe_path))) {
    return(list(status = "error", message = "ROE 路徑長度／數值無效。", warnings = warnings))
  }
  if (any(roe_path < ke)) {
    warnings <- c(warnings, "部分年度 ROE < Ke：剩餘收益將為負（價值銷毀）。")
  }

  df <- data.frame(
    Year = seq_len(n),
    Beg_BV = numeric(n),
    ROE = roe_path,
    Net_Income = numeric(n),
    Dividend = numeric(n),
    End_BV = numeric(n),
    RI = numeric(n),
    PV_RI = numeric(n),
    stringsAsFactors = FALSE
  )

  curr_bv <- b0
  for (i in seq_len(n)) {
    roe_i <- roe_path[i]
    ni <- curr_bv * roe_i
    dps <- ni * payout
    ri <- (roe_i - ke) * curr_bv
    pv <- ri / ((1 + ke)^i)
    end_bv <- curr_bv + ni - dps

    df$Beg_BV[i] <- curr_bv
    df$Net_Income[i] <- ni
    df$Dividend[i] <- dps
    df$End_BV[i] <- end_bv
    df$RI[i] <- ri
    df$PV_RI[i] <- pv
    curr_bv <- end_bv
  }

  pv_ri <- sum(df$PV_RI)
  # Terminal: next year's RI grows at g in perpetuity, discounted n years
  ri_next <- df$RI[n] * (1 + g)
  tv_ri <- ri_next / (ke - g)
  pv_terminal <- tv_ri / ((1 + ke)^n)
  intrinsic <- b0 + pv_ri + pv_terminal
  tv_ratio <- if (is.finite(intrinsic) && abs(intrinsic) > 1e-12) {
    pv_terminal / intrinsic
  } else {
    NA_real_
  }

  list(
    status = "success",
    message = NULL,
    warnings = warnings,
    b0 = b0,
    ke = ke,
    g = g,
    n = n,
    payout = payout,
    pv_ri = pv_ri,
    pv_terminal = pv_terminal,
    tv_ri_undiscounted = tv_ri,
    intrinsic = intrinsic,
    tv_ratio = tv_ratio,
    df = df
  )
}

.parse_roe_pct_vector <- function(txt) {
  if (is.null(txt) || !nzchar(as.character(txt)[1])) return(numeric(0))
  parts <- unlist(strsplit(as.character(txt)[1], "[,;\\s]+"))
  suppressWarnings(as.numeric(parts)) / 100
}

# ==========================================
# UI
# ==========================================
ri_module_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "ri_calculator",
    tabBox(
      title = "RESIDUAL INCOME", width = "auto",

      # ----- Overview -----
      tabPanel(
        "RI Overview", icon = icon("gem"),
        fluidRow(
          div(
            "Residual Income = (ROE − Ke) × Beginning Book Value",
            style = paste(
              "font-size: 16px; font-weight: bold; color: #2C3E50; text-align: center;",
              "margin-bottom: 12px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
            )
          )
        ),
        fluidRow(
          column(
            12,
            uiOutput(ns("ui_ri_warnings")),
            uiOutput(ns("ui_ri_breakdown"))
          )
        ),
        fluidRow(
          box(
            title = "Valuation Waterfall", width = 12, status = "success", solidHeader = TRUE,
            plotlyOutput(ns("plt_ri_waterfall"), height = "360px") %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "每股帳面淨值 vs 剩餘收益軌跡", width = 12, status = "info", solidHeader = TRUE,
            plotOutput(ns("plt_ri_trajectory"), height = "320px")
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("table"), "Forecast Detail"),
            width = 12, status = "primary", solidHeader = TRUE,
            collapsible = TRUE, collapsed = FALSE,
            div(
              style = "width: 100%; overflow-x: auto;",
              DT::dataTableOutput(ns("tbl_ri_details"))
            )
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("book"), "Model Formula"),
            width = 12, status = "warning", solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            uiOutput(ns("ui_ri_formula"))
          )
        )
      ),

      # ----- Settings -----
      tabPanel(
        "RI Settings", icon = icon("cogs"),
        h4(tags$b("每股帳面淨值 (B0) 估算區")),
        fluidRow(
          div(
            "B0 = 普通股股東權益 (Common Equity) ÷ 發行股數 (Shares Outstanding)",
            style = paste(
              "font-size: 15px; font-weight: bold; color: #2C3E50; text-align: center;",
              "margin-bottom: 12px; padding: 10px; background-color: #F8F9F9;",
              "border-left: 4px solid #2980B9; border-radius: 4px;"
            )
          )
        ),
        fluidRow(
          column(4, numericInput(ns("b0"), "期初每股帳面淨值 B0 (USD)", value = NA, step = 0.5)),
          column(
            8, br(),
            actionButton(
              ns("btn_sync_b0"), "從最新財報自動帶入數值",
              icon = icon("sync"), class = "btn-sm",
              style = "background-color: #2980b9; color: white; border: none; padding: 8px 15px; font-weight: bold; border-radius: 5px; margin-top: 5px;"
            )
          )
        ),
        hr(style = "border-top: 1px solid #BDC3C7;"),
        h4(tags$b("模型參數假設")),
        fluidRow(
          column(4, numericInput(ns("ri_years"), "預測期 (Years)", value = 5, min = 1, max = 15, step = 1)),
          column(4, numericInput(ns("ri_ke"), "股東權益成本 (Ke, %)", value = 8.0, step = 0.1)),
          column(4, numericInput(ns("ri_g"), "終值永續成長率 (g, %)", value = 2.0, step = 0.1))
        ),
        fluidRow(
          column(6, numericInput(ns("ri_roe"), "起始／預期 ROE (%)", value = 15.0, step = 0.1)),
          column(6, numericInput(ns("ri_payout"), "預期現金配息率 (Payout, %)", value = 40.0, step = 1))
        ),
        hr(style = "border-top: 1px solid #BDC3C7;"),
        h4(tags$b("ROE Forecast Method")),
        fluidRow(
          column(
            6,
            selectInput(
              ns("roe_method"), "ROE 預測方法",
              choices = c(
                "Constant ROE" = "constant",
                "Linear Fade" = "linear",
                "Industry Fade" = "industry",
                "Custom Vector" = "custom"
              ),
              selected = "constant"
            )
          ),
          column(6, uiOutput(ns("ui_roe_path_preview")))
        ),
        uiOutput(ns("ui_roe_method_params")),
        fluidRow(
          column(
            12,
            actionButton(
              ns("btn_reset_ri_params"), "回復系統預設參數",
              icon = icon("undo"), class = "btn-sm",
              style = "background-color: #7f8c8d; color: white; border: none; margin-top: 10px;"
            ),
            tags$span(
              style = "margin-left: 12px; color: #7f8c8d; font-size: 12px;",
              "參數變更後估值會自動更新（無需另按試算）。"
            )
          )
        )
      ),

      # ----- Sensitivity -----
      tabPanel(
        "Sensitivity Analysis", icon = icon("th"),
        fluidRow(
          box(
            title = "Ke × g 估值矩陣（每股內在價值）",
            width = 12, status = "primary", solidHeader = TRUE,
            helpText("列＝永續成長率 g；欄＝股權成本 Ke。藍框＝目前設定。"),
            DT::dataTableOutput(ns("tbl_ri_sensitivity"))
          )
        ),
        fluidRow(
          box(
            title = "Interactive Heatmap",
            width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput(ns("plt_ri_heatmap"), height = "420px") %>% withSpinner()
          )
        )
      )
    )
  )
}

# ==========================================
# Server
# ==========================================
ri_module_server <- function(id, d_income_statement, d_balance_sheet, d_cash_flow, global_re,
                             global_g = reactive(NULL),
                             industry_choice = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    industry_roe_pct <- reactive({
      v <- .industry_roe_pct(industry_choice())
      if (is.finite(v)) round(v, 2) else 12
    })

    # Keep Industry Average ROE aligned with Dashboard industry (still editable)
    observeEvent(industry_choice(), {
      updateNumericInput(session, "roe_industry", value = industry_roe_pct())
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    observeEvent(input$roe_method, {
      if (!identical(input$roe_method, "industry")) return()
      updateNumericInput(session, "roe_industry", value = industry_roe_pct())
    }, ignoreInit = TRUE)

    # ----- Sync B0 / ROE / payout from statements -----
    observeEvent(d_balance_sheet(), {
      req(d_balance_sheet(), d_income_statement())
      df_bs <- d_balance_sheet()

      raw_shares <- select_current_metric(
        df_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock"
      )
      shares <- if (is.na(raw_shares) || raw_shares <= 0) 1 else raw_shares
      equity <- select_current_metric_any(df_bs, EQUITY_PATTERNS, "stock")

      if (!is.na(equity) && !is.na(shares) && shares > 0) {
        updateNumericInput(session, "b0", value = round(equity / shares, 2))
      }

      ni <- select_current_metric_any(d_income_statement(), NET_INCOME_PATTERNS, "flow")
      if (!is.na(ni) && !is.na(equity) && equity > 0) {
        roe <- (ni / equity) * 100
        updateNumericInput(session, "ri_roe", value = round(.ri_clip(roe, -50, 80), 2))
      }

      div_paid_total <- abs(select_current_metric(d_cash_flow(), "Cash Dividends Paid", "flow"))
      if (!is.na(div_paid_total) && !is.na(ni) && ni > 0) {
        payout <- .ri_clip((div_paid_total / ni) * 100, 0, 100)
        updateNumericInput(session, "ri_payout", value = round(payout, 2))
      } else {
        updateNumericInput(session, "ri_payout", value = 0)
      }

      if (!is.null(global_g) && !is.null(global_g()) && is.finite(global_g())) {
        updateNumericInput(session, "ri_g", value = round(as.numeric(global_g()), 2))
      }
    })

    observeEvent(input$btn_sync_b0, {
      req(d_balance_sheet())
      df_bs <- d_balance_sheet()
      raw_shares <- select_current_metric(
        df_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock"
      )
      shares <- if (is.na(raw_shares) || raw_shares <= 0) 1 else raw_shares
      equity <- select_current_metric_any(df_bs, EQUITY_PATTERNS, "stock")
      if (!is.na(equity) && !is.na(shares) && shares > 0) {
        calc_b0 <- round(equity / shares, 2)
        updateNumericInput(session, "b0", value = calc_b0)
        showNotification(paste0("✅ 已成功從資產負債表更新 B0 為 $", calc_b0), type = "message")
      } else {
        showNotification("⚠️ 無法從當前財報讀取完整 B0 所需欄位", type = "error")
      }
    })

    observeEvent(input$btn_reset_ri_params, {
      updateNumericInput(session, "ri_years", value = 5)
      g_reset <- if (!is.null(global_g) && !is.null(global_g()) && is.finite(global_g())) {
        round(as.numeric(global_g()), 2)
      } else {
        2.0
      }
      updateNumericInput(session, "ri_g", value = g_reset)
      if (!is.null(global_re()) && is.finite(global_re())) {
        updateNumericInput(session, "ri_ke", value = round(global_re() * 100, 2))
      }
      updateSelectInput(session, "roe_method", selected = "constant")
      updateNumericInput(session, "roe_industry", value = industry_roe_pct())
      showNotification("🔁 已重設為系統預設參數", type = "message")
    })

    observeEvent(global_re(), {
      req(global_re())
      updateNumericInput(session, "ri_ke", value = round(global_re() * 100, 2))
    })

    observeEvent(global_g(), {
      req(!is.null(global_g()), is.finite(global_g()))
      g_val <- round(as.numeric(global_g()), 2)
      if (is.null(input$ri_g) || abs(as.numeric(input$ri_g) - g_val) > 1e-4) {
        updateNumericInput(session, "ri_g", value = g_val)
      }
    }, ignoreInit = FALSE)

    # Keep custom ROE vector length aligned with forecast years
    observeEvent(list(input$ri_years, input$roe_method), {
      req(identical(input$roe_method, "custom"))
      n <- max(1L, as.integer(input$ri_years %||% 5))
      cur <- .parse_roe_pct_vector(input$roe_custom_txt)
      start <- (input$ri_roe %||% 15) / 100
      if (length(cur) < 1L) cur <- start
      if (length(cur) < n) cur <- c(cur, rep(utils::tail(cur, 1), n - length(cur)))
      if (length(cur) > n) cur <- cur[seq_len(n)]
      updateTextInput(session, "roe_custom_txt", value = paste(round(cur * 100, 1), collapse = ", "))
    }, ignoreInit = TRUE)

    output$ui_roe_method_params <- renderUI({
      method <- input$roe_method %||% "constant"
      n <- max(1L, as.integer(input$ri_years %||% 5))
      start_roe <- input$ri_roe %||% 15

      if (identical(method, "linear")) {
        fluidRow(
          column(4, numericInput(ns("roe_terminal"), "Terminal ROE (%)", value = max(5, start_roe - 8), step = 0.1)),
          column(8, helpText("由起始 ROE 線性淡化至 Terminal ROE，年數＝預測期。"))
        )
      } else if (identical(method, "industry")) {
        ind_val <- {
          cur <- suppressWarnings(as.numeric(isolate(input$roe_industry))[1])
          if (is.finite(cur)) cur else industry_roe_pct()
        }
        fluidRow(
          column(
            4,
            numericInput(
              ns("roe_industry"), "Industry Average ROE (%)",
              value = ind_val, step = 0.1
            )
          ),
          column(
            8,
            helpText(sprintf(
              "預設＝目前所選產業 ROE 估計（中位 %.1f%%）；可手動覆寫。由起始 ROE 線性收斂至產業平均。",
              industry_roe_pct()
            ))
          )
        )
      } else if (identical(method, "custom")) {
        default_vec <- paste(round(rep(start_roe, n), 1), collapse = ", ")
        fluidRow(
          column(
            12,
            textInput(
              ns("roe_custom_txt"),
              sprintf("Custom ROE Vector (%%, %d 年，逗號分隔)", n),
              value = default_vec
            ),
            helpText("例：31, 29, 27, 25, 23 — 長度不足會沿用最後一值；過長會截斷。")
          )
        )
      } else {
        helpText("Constant：各年 ROE 皆等於「起始／預期 ROE」。")
      }
    })

    roe_path_pct <- reactive({
      n <- max(1L, as.integer(input$ri_years %||% 5))
      method <- input$roe_method %||% "constant"
      start <- (input$ri_roe %||% 15) / 100
      term <- (input$roe_terminal %||% (input$ri_roe %||% 15)) / 100
      ind <- (input$roe_industry %||% 12) / 100
      custom <- if (identical(method, "custom")) {
        .parse_roe_pct_vector(input$roe_custom_txt)
      } else {
        NULL
      }
      build_roe_path(method, n, start, term, ind, custom) * 100
    })

    output$ui_roe_path_preview <- renderUI({
      path <- tryCatch(roe_path_pct(), error = function(e) numeric(0))
      if (!length(path)) return(NULL)
      tags$div(
        style = "margin-top: 8px; font-size: 13px; color: #34495e;",
        tags$b("ROE path (%): "),
        paste(sprintf("%.1f", path), collapse = " → ")
      )
    })

    # ----- Core reactive valuation (auto-updates) -----
    ri_calc <- reactive({
      req(!is.null(input$b0), !is.null(input$ri_ke), !is.null(input$ri_g),
          !is.null(input$ri_years), !is.null(input$ri_payout), !is.null(input$ri_roe))

      n <- max(1L, as.integer(input$ri_years))
      method <- input$roe_method %||% "constant"
      path_dec <- build_roe_path(
        method = method,
        n = n,
        roe_start = (input$ri_roe %||% 15) / 100,
        roe_terminal = (input$roe_terminal %||% (input$ri_roe %||% 15)) / 100,
        roe_industry = (input$roe_industry %||% 12) / 100,
        custom_vec = if (identical(method, "custom")) .parse_roe_pct_vector(input$roe_custom_txt) else NULL
      )

      compute_ri_valuation(
        b0 = input$b0,
        ke = (input$ri_ke %||% 8) / 100,
        g = (input$ri_g %||% 2) / 100,
        n = n,
        payout = (input$ri_payout %||% 0) / 100,
        roe_path = path_dec,
        validate = TRUE
      )
    })

    # ----- Warnings -----
    output$ui_ri_warnings <- renderUI({
      res <- ri_calc()
      tags_list <- list()

      if (!is.null(res$message) && identical(res$status, "error")) {
        tags_list <- c(tags_list, list(
          div(
            style = "margin-bottom:10px;padding:12px;background:#fdf2f2;border-left:5px solid #d9534f;border-radius:4px;color:#a94442;font-weight:600;",
            icon("exclamation-triangle"), " ", res$message
          )
        ))
      }

      for (w in res$warnings %||% character(0)) {
        tags_list <- c(tags_list, list(
          div(
            style = "margin-bottom:8px;padding:10px;background:#fff8e6;border-left:5px solid #f0ad4e;border-radius:4px;color:#8a6d3b;",
            icon("exclamation-circle"), " ", w
          )
        ))
      }

      if (identical(res$status, "success") && is.finite(res$tv_ratio)) {
        if (res$tv_ratio > 0.85) {
          tags_list <- c(tags_list, list(
            div(
              style = "margin-bottom:10px;padding:12px;background:#fdf2f2;border-left:5px solid #d9534f;border-radius:4px;color:#a94442;font-weight:600;",
              icon("fire"), " ",
              "Terminal Value dominates the valuation. Consider lowering perpetual growth or extending forecast years."
            )
          ))
        } else if (res$tv_ratio > 0.70) {
          tags_list <- c(tags_list, list(
            div(
              style = "margin-bottom:10px;padding:12px;background:#fff8e6;border-left:5px solid #f0ad4e;border-radius:4px;color:#8a6d3b;font-weight:600;",
              icon("exclamation-triangle"), " ",
              "Terminal Value contributes over 70% of total valuation. The model is highly sensitive to Ke and perpetual growth."
            )
          ))
        }
      }

      if (!length(tags_list)) return(NULL)
      do.call(tagList, tags_list)
    })

    # ----- Breakdown cards -----
    output$ui_ri_breakdown <- renderUI({
      res <- ri_calc()
      if (!identical(res$status, "success")) return(NULL)

      card <- function(label, value, sub = NULL, accent = "#2c3e50", bg = "#fcfcfc") {
        div(
          style = paste0(
            "flex:1;min-width:140px;margin:6px;padding:14px 12px;text-align:center;",
            "background:", bg, ";border:1px solid #e5e5e5;border-radius:8px;"
          ),
          p(style = "font-size:12px;color:#7f8c8d;margin:0 0 6px 0;font-weight:700;text-transform:uppercase;", label),
          p(style = paste0("font-size:22px;font-weight:700;margin:0;color:", accent, ";"), value),
          if (!is.null(sub)) p(style = "font-size:11px;color:#95a5a6;margin:6px 0 0 0;", sub)
        )
      }

      tv_pct <- if (is.finite(res$tv_ratio)) sprintf("%.1f%%", 100 * res$tv_ratio) else "N/A"
      tv_col <- if (is.finite(res$tv_ratio) && res$tv_ratio > 0.85) "#d9534f"
      else if (is.finite(res$tv_ratio) && res$tv_ratio > 0.70) "#e67e22"
      else "#8e44ad"

      tagList(
        h4(tags$b("Residual Income Breakdown"), style = "margin: 8px 0 4px 0;"),
        div(
          style = "display:flex;flex-wrap:wrap;justify-content:space-between;align-items:stretch;",
          card("Book Value (B0)", paste0("$", sprintf("%.2f", res$b0))),
          card("Forecast RI (PV)", paste0("$", sprintf("%.2f", res$pv_ri)),
               accent = if (res$pv_ri >= 0) "#27ae60" else "#c0392b"),
          card("Terminal Value (PV)", paste0("$", sprintf("%.2f", res$pv_terminal)),
               accent = "#8e44ad"),
          card("Intrinsic Value", paste0("$", sprintf("%.2f", res$intrinsic)),
               accent = "#1abc9c", bg = "#e8f8f5"),
          card("Terminal Contribution", tv_pct, accent = tv_col,
               sub = "PV_Terminal / Intrinsic")
        ),
        p(
          style = "font-size:12px;color:#7f8c8d;margin-top:4px;",
          "Intrinsic Value = B0 + PV(Forecast RI) + PV(Terminal RI)"
        )
      )
    })

    # ----- Waterfall -----
    output$plt_ri_waterfall <- renderPlotly({
      res <- ri_calc()
      validate(need(identical(res$status, "success"), "調整參數後顯示瀑布圖"))

      # Plotly waterfall: measure relative/total
      x <- c("Book Value (B0)", "Forecast RI", "Terminal RI", "Intrinsic Value")
      measure <- c("absolute", "relative", "relative", "total")
      y <- c(res$b0, res$pv_ri, res$pv_terminal, res$intrinsic)
      text <- sprintf("$%.2f", y)

      plot_ly(
        type = "waterfall",
        x = x,
        measure = measure,
        y = y,
        text = text,
        textposition = "outside",
        connector = list(line = list(color = "#95a5a6")),
        increasing = list(marker = list(color = "#27ae60")),
        decreasing = list(marker = list(color = "#c0392b")),
        totals = list(marker = list(color = "#1abc9c"))
      ) %>%
        layout(
          title = list(text = "RI Valuation Waterfall", font = list(size = 14)),
          yaxis = list(title = "USD / share", zeroline = TRUE),
          xaxis = list(title = ""),
          margin = list(t = 50, b = 80)
        )
    })

    # ----- Trajectory (ggplot) -----
    output$plt_ri_trajectory <- renderPlot({
      res <- ri_calc()
      req(identical(res$status, "success"))
      df <- res$df
      df$Cum_PV_RI <- cumsum(df$PV_RI)
      df$Intrinsic_Path <- res$b0 + df$Cum_PV_RI

      ggplot(df, aes(x = as.factor(Year))) +
        geom_bar(aes(y = Intrinsic_Path, fill = "B0 + 累計 PV(Forecast RI)"),
                 stat = "identity", alpha = 0.7) +
        geom_hline(yintercept = res$b0, linetype = "dashed", color = "#34495e", linewidth = 1.1) +
        geom_hline(yintercept = res$intrinsic, linetype = "dotted", color = "#1abc9c", linewidth = 1) +
        geom_point(aes(y = Intrinsic_Path), size = 3, color = "#2980b9") +
        geom_line(aes(y = Intrinsic_Path, group = 1), color = "#2980b9", linewidth = 1) +
        scale_fill_manual(name = "", values = c("B0 + 累計 PV(Forecast RI)" = "#aed6f1")) +
        scale_y_continuous(labels = label_chart_number(prefix = "$")) +
        theme_minimal(base_size = 13) +
        labs(x = "預測年份", y = "每股價值 (USD)",
             caption = sprintf("虛線＝B0；點線＝完整內在價值（含 Terminal）$%.2f", res$intrinsic)) +
        theme(legend.position = "bottom")
    })

    # ----- Forecast detail DT -----
    output$tbl_ri_details <- DT::renderDataTable({
      res <- ri_calc()
      validate(need(identical(res$status, "success"), "尚無有效預測表"))
      df <- res$df
      out <- data.frame(
        Year = df$Year,
        `Beginning BV` = round(df$Beg_BV, 4),
        `ROE (%)` = round(df$ROE * 100, 2),
        `Net Income` = round(df$Net_Income, 4),
        Dividend = round(df$Dividend, 4),
        `Ending BV` = round(df$End_BV, 4),
        `Residual Income` = round(df$RI, 4),
        `PV(RI)` = round(df$PV_RI, 4),
        check.names = FALSE
      )
      DT::datatable(
        out,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 20, scrollX = TRUE),
        class = "stripe hover compact"
      ) %>%
        DT::formatCurrency(
          columns = c("Beginning BV", "Net Income", "Dividend", "Ending BV",
                      "Residual Income", "PV(RI)"),
          currency = "$", digits = 2
        )
    })

    # ----- Formula panel -----
    output$ui_ri_formula <- renderUI({
      withMathJax(tagList(
        tags$p(tags$b("剩餘收益 (Residual Income)")),
        tags$p("$$RI_t = (ROE_t - K_e) \\times BV_{t-1}$$"),
        tags$p("等價於：淨利 − 股權資金成本＝$$NI_t - K_e \\times BV_{t-1}$$"),
        tags$hr(),
        tags$p(tags$b("內在價值")),
        tags$p("$$V_0 = BV_0 + \\sum_{t=1}^{N} \\frac{RI_t}{(1+K_e)^t} + \\frac{TV}{(1+K_e)^N}$$"),
        tags$p("其中終值 $$TV = \\dfrac{RI_N (1+g)}{K_e - g}$$（永續成長剩餘收益）"),
        tags$p(tags$b("Terminal Contribution")),
        tags$p("$$TV\\ Ratio = \\dfrac{PV(Terminal)}{V_0}$$")
      ))
    })

    # ----- Sensitivity matrix -----
    .ri_sens_grid <- reactive({
      res0 <- ri_calc()
      validate(need(identical(res0$status, "success") || !is.null(input$b0), "需先有 B0／參數"))

      g_grid <- c(0.02, 0.03, 0.04, 0.05, 0.06)
      ke_grid <- c(0.08, 0.09, 0.10, 0.11, 0.12)
      n <- max(1L, as.integer(input$ri_years %||% 5))
      method <- input$roe_method %||% "constant"
      path_dec <- build_roe_path(
        method = method,
        n = n,
        roe_start = (input$ri_roe %||% 15) / 100,
        roe_terminal = (input$roe_terminal %||% (input$ri_roe %||% 15)) / 100,
        roe_industry = (input$roe_industry %||% 12) / 100,
        custom_vec = if (identical(method, "custom")) .parse_roe_pct_vector(input$roe_custom_txt) else NULL
      )
      b0 <- input$b0 %||% NA_real_
      payout <- (input$ri_payout %||% 0) / 100

      mat_v <- matrix(NA_real_, nrow = length(g_grid), ncol = length(ke_grid),
                      dimnames = list(
                        paste0("g=", g_grid * 100, "%"),
                        paste0("Ke=", ke_grid * 100, "%")
                      ))
      mat_tv <- mat_v
      for (i in seq_along(g_grid)) {
        for (j in seq_along(ke_grid)) {
          cell <- compute_ri_valuation(
            b0 = b0, ke = ke_grid[j], g = g_grid[i], n = n,
            payout = payout, roe_path = path_dec, validate = FALSE
          )
          if (identical(cell$status, "success")) {
            mat_v[i, j] <- cell$intrinsic
            mat_tv[i, j] <- cell$tv_ratio
          }
        }
      }
      list(
        value = mat_v, tv = mat_tv,
        g_grid = g_grid, ke_grid = ke_grid,
        cur_g = (input$ri_g %||% 2) / 100,
        cur_ke = (input$ri_ke %||% 8) / 100
      )
    })

    output$tbl_ri_sensitivity <- DT::renderDataTable({
      grid <- .ri_sens_grid()
      mat <- round(grid$value, 2)
      df <- as.data.frame(mat, check.names = FALSE)
      df <- cbind(`g \\ Ke` = rownames(mat), df)
      rownames(df) <- NULL

      # Highlight current Ke/g cell via JS after draw
      cur_g_i <- which.min(abs(grid$g_grid - grid$cur_g))
      cur_ke_j <- which.min(abs(grid$ke_grid - grid$cur_ke))
      # DT is 0-indexed; +1 for rowname col
      target_row <- cur_g_i - 1L
      target_col <- cur_ke_j  # +1 offset handled in JS below (column 0 = label)

      brks <- suppressWarnings(pretty(range(mat, na.rm = TRUE), n = 8))
      if (!length(brks) || any(!is.finite(brks))) {
        brks <- c(0, 1)
      }
      clrs <- grDevices::colorRampPalette(c("#f5b7b1", "#fef9e7", "#abebc6"))(length(brks) + 1)

      DT::datatable(
        df,
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          ordering = FALSE,
          pageLength = 10,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          rowCallback = htmlwidgets::JS(sprintf(
            "function(row, data, displayNum, displayIndex) {
               if (displayIndex === %d) {
                 $('td', row).eq(%d).css({
                   'outline': '3px solid #2980b9',
                   'outline-offset': '-3px',
                   'font-weight': '700'
                 });
               }
             }",
            target_row, cur_ke_j
          ))
        )
      ) %>%
        DT::formatStyle(
          columns = names(df)[-1],
          backgroundColor = DT::styleInterval(brks, clrs)
        ) %>%
        DT::formatCurrency(columns = names(df)[-1], currency = "$", digits = 2)
    })

    output$plt_ri_heatmap <- renderPlotly({
      grid <- .ri_sens_grid()
      validate(need(any(is.finite(grid$value)), "無法產生熱力圖（檢查 g < Ke）"))

      # Long format for hover (Ke, g, V, TV ratio)
      g_lab <- grid$g_grid * 100
      ke_lab <- grid$ke_grid * 100
      z <- grid$value
      tv <- grid$tv
      hover <- matrix("", nrow = nrow(z), ncol = ncol(z))
      for (i in seq_len(nrow(z))) {
        for (j in seq_len(ncol(z))) {
          hover[i, j] <- sprintf(
            "Ke: %.0f%%<br>g: %.0f%%<br>Intrinsic: $%.2f<br>TV Ratio: %s",
            ke_lab[j], g_lab[i], z[i, j],
            if (is.finite(tv[i, j])) sprintf("%.1f%%", 100 * tv[i, j]) else "N/A"
          )
        }
      }

      plot_ly(
        x = ke_lab,
        y = g_lab,
        z = z,
        type = "heatmap",
        colorscale = list(
          list(0, "#e74c3c"),
          list(0.5, "#f7dc6f"),
          list(1, "#27ae60")
        ),
        hoverinfo = "text",
        text = hover,
        colorbar = list(title = "Intrinsic")
      ) %>%
        layout(
          title = list(text = "RI Intrinsic Value Heatmap", font = list(size = 14)),
          xaxis = list(title = "Cost of Equity Ke (%)", dtick = 1),
          yaxis = list(title = "Perpetual Growth g (%)", dtick = 1),
          margin = list(t = 50)
        )
    })

    # Preserve external contract
    return(list(
      ri_price = reactive({
        res <- ri_calc()
        if (identical(res$status, "success")) res$intrinsic else NA_real_
      }),
      ri_result = ri_calc
    ))
  })
}
