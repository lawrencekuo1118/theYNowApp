# =========================================================================
# 投資決策權威模組 (Investment Decision Scorecard) - v14.0
# 維度：財務質量 (Quality) -> 估值區間 (Value) -> 回歸動能 (Timing, 輔助)
# =========================================================================

library(shiny)
library(shinydashboard)
library(TTR)
library(glue)

# -------------------------------------------
# 1. UI 模組：視覺化決策看板
# -------------------------------------------
decision_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("vbox_fscore"), width = 3),
      uiOutput(ns("vbox_mos")),
      valueBoxOutput(ns("vbox_momentum"), width = 3),
      uiOutput(ns("vbox_fraud"))
    ),
    fluidRow(
      box(
        title = "智能決策矩陣 The Decision Funnel", width = 12, status = "primary", solidHeader = TRUE,
        column(
          width = 5,
          h4("F-Score 體質檢核清單"),
          tableOutput(ns("table_checklist"))
        ),
        column(
          width = 7,
          uiOutput(ns("ui_recommendation"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        style = "padding: 0 15px 8px 15px;",
        tags$div(
          class = "ynow-fraud-banner",
          style = "background-color: #d9534f; color: white; padding: 12px 14px; margin: 0 0 14px 0; border-radius: 4px;",
          tags$h4(
            icon("exclamation-triangle"), " Fraud Warnings",
            style = "font-weight: bold; margin: 0 0 8px 0; font-size: 15px; border-bottom: 1px solid #ffcccc; padding-bottom: 8px;"
          ),
          tags$div(
            style = "font-size: 13px; line-height: 1.55;",
            textOutput("highdebttoequity"),
            textOutput("nofreecashflow"),
            textOutput("nooperatingcashflow"),
            textOutput("notdoingbusiness"),
            textOutput("notgettingcashback"),
            textOutput("no_fraud_detected")
          )
        ),
        tags$div(
          class = "ynow-about-section",
          h3(class = "ynow-about-section-title", tags$b("Financial Fraud Red Flags (財務舞弊警訊)")),
          p(
            class = "ynow-about-section-lead",
            "本系統內建五項核心排雷機制，透過交叉比對現金流與獲利品質，自動偵測潛在的地雷股："
          ),
          tags$ul(
            tags$li(tags$b("無自由現金流 (No FCF)："), "長期 FCF 為負，代表企業無法靠自身營運創造現金，需依賴外部融資。"),
            tags$li(tags$b("無營業現金流 (No OCF)："), "OCF 為負是極度危險的訊號，代表核心本業正在失血。"),
            tags$li(tags$b("獲利未實現 (OCF < Net Income)："), "俗稱「紙上富貴」，損益表雖然賺錢，但現金沒有實際流入公司，可能存在應收帳款作帳疑慮。"),
            tags$li(tags$b("虛假獲利 (Net Income > 0 but OCF < 0)："), "最經典的舞弊特徵，強烈暗示獲利品質不佳。"),
            tags$li(tags$b("高財務槓桿 (Debt/Equity > 2)："), "負債比過高，在升息循環或景氣下行時面臨極大的流動性風險。")
          )
        )
      )
    )
  )
}

# -------------------------------------------
# 2. Pure helpers
# -------------------------------------------
safe_cagr <- function(row_data) {
  vals <- na.omit(parse_financial_number(row_data))
  if (length(vals) < 2) return(NA)
  newest <- vals[1]
  oldest <- vals[length(vals)]
  if (oldest <= 0 || newest <= 0) return(NA)
  (newest / oldest)^(1 / (length(vals) - 1)) - 1
}

# -------------------------------------------
# 3. Server：主模型區間 + 副模型檢核 + 可信度
# -------------------------------------------
decision_server <- function(id, d_is, d_bs, d_cf, intrinsic_val_dcf, intrinsic_val_ddm, current_price, hist_price_data, industry_text,
                            intrinsic_val_pb = reactive(NA),
                            model_rec = reactive(NULL),
                            primary_band = reactive(NULL),
                            secondary_point = reactive(NA),
                            confidence = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    get_row_safe <- function(df, label) {
      res <- select_clean_metric_row(df, label, include_ttm = FALSE)
      if (length(res) < 2) return(c(NA, NA))
      as.numeric(res[1:2])
    }

    .pick_num <- function(x) {
      x <- suppressWarnings(as.numeric(x)[1])
      if (length(x) != 1 || is.null(x) || is.na(x) || !is.finite(x) || x == 0) NA_real_ else x
    }

    f_score_eval <- reactive({
      req(d_is(), d_bs(), d_cf())
      res <- compute_report_f_score(d_is(), d_bs(), d_cf())
      # UI 表格仍以 1/0 轉成 ✅/❌
      if (is.data.frame(res$checklist) && nrow(res$checklist) > 0 &&
          is.character(res$checklist$`得分`)) {
        res$checklist$`得分` <- ifelse(res$checklist$`得分` == "通過", 1, 0)
      }
      if (!is.finite(res$total)) res$total <- 0
      if (!is.finite(res$quality_flag)) res$quality_flag <- 0
      res
    })

    primary_values <- reactive({
      band <- tryCatch(primary_band(), error = function(e) NULL)
      if (is.list(band) && !is.null(band$base)) {
        return(list(
          bear = .pick_num(band$bear),
          base = .pick_num(band$base),
          bull = .pick_num(band$bull),
          label = as.character(band$label %||% "主模型")
        ))
      }
      rec <- tryCatch(model_rec(), error = function(e) NULL)
      prim <- as.character(rec$primary %||% "")
      dcf_v <- .pick_num(tryCatch(intrinsic_val_dcf(), error = function(e) NA))
      ddm_v <- .pick_num(tryCatch(intrinsic_val_ddm(), error = function(e) NA))
      pb_v  <- .pick_num(tryCatch(intrinsic_val_pb(), error = function(e) NA))
      base <- switch(prim, "dcf" = dcf_v, "ddm" = ddm_v, "pb" = pb_v, "ri" = NA_real_, dcf_v)
      if (is.na(base)) base <- if (!is.na(dcf_v)) dcf_v else if (!is.na(ddm_v)) ddm_v else pb_v
      list(bear = NA_real_, base = base, bull = NA_real_, label = .model_label(prim))
    })

    mos_calc <- reactive({
      base <- primary_values()$base
      curr_p <- .pick_num(current_price())
      if (is.na(base) || is.na(curr_p) || base == 0) return(NA_real_)
      (base - curr_p) / base
    })

    mom_status <- reactive({
      req(hist_price_data())
      prices <- hist_price_data()$Close
      ma20 <- tail(SMA(prices, 20), 1)
      ma60 <- tail(SMA(prices, 60), 1)
      curr_p <- tail(prices, 1)
      cond1 <- curr_p > ma20 && curr_p > ma60
      cond2 <- ma20 > ma60
      list(
        triggered = (isTRUE(cond1) && isTRUE(cond2)),
        dist_to_ma20 = if (is.na(ma20) || is.na(curr_p)) NA else (curr_p - ma20) / ma20
      )
    })

    final_recommendation <- reactive({
      f_score <- f_score_eval()$total
      f_quality <- f_score_eval()$quality_flag
      mos <- mos_calc()
      mom <- mom_status()
      if (f_score < 4 || f_quality == 0) {
        return(list(class = "alert-danger", icon = "skull-crossbones", title = "價值陷阱警訊",
                    text = "財務質量偏弱，或經營現金流難以支撐帳面獲利。即便估值看似便宜，亦不宜貿然摸底。"))
      }
      if (!is.na(mos) && mos < 0) {
        if (mom$triggered) {
          return(list(class = "alert-warning", icon = "fire", title = "動能強勁但估值偏高",
                      text = "右側趨勢仍佳，惟市價已高於主模型基準內在價值。若已持有可續抱；空手者不宜此時追高。"))
        }
        return(list(class = "alert-warning", icon = "hourglass-half", title = "估值偏高且動能轉弱",
                    text = "體質通過檢核，但市價已高於基準合理價，且趨勢尚未轉強。建議耐心等待拉回再評估。"))
      }
      if (!is.na(mos) && mos >= 0.2) {
        if (mom$triggered) {
          return(list(class = "alert-success", icon = "rocket", title = "戴維斯雙擊區",
                      text = "估值具安全邊際、體質佳，且技術動能已確認。可分批布局，惟仍應控制部位與風險。"))
        }
        return(list(class = "alert-info", icon = "anchor", title = "價值突出、等待趨勢",
                    text = "基本面價值突出，惟市場資金尚未顯著關注。可分批布局，待趨勢轉折後再考慮加碼。"))
      }
      list(class = "alert-secondary", icon = "balance-scale", title = "觀望中立",
           text = "市價約在主模型合理區間附近，體質穩健。可依資產配置彈性決定是否介入。")
    })

    output$vbox_fscore <- renderValueBox({
      score <- f_score_eval()$total
      color <- if (score >= 7) "green" else if (score >= 4) "yellow" else "red"
      valueBox(paste0(score, " / 9"), "體質過濾 (F-Score)", icon = icon("gem"), color = color)
    })

    output$vbox_mos <- renderUI({
      val <- tryCatch(mos_calc(), error = function(e) NA_real_)
      if (length(val) != 1L || is.null(val) || is.na(val) || !is.finite(val)) {
        return(NULL)
      }
      conf <- tryCatch(confidence(), error = function(e) NULL)
      conf_lab <- if (is.list(conf) && !is.null(conf$level)) paste0("｜可信度", conf$level) else ""
      v_pct <- round(as.numeric(val) * 100, 1)
      color <- if (v_pct >= 20) "green" else if (v_pct >= 0) "yellow" else "red"
      valueBox(
        paste0(v_pct, "%"),
        paste0("安全邊際 (vs Base)", conf_lab),
        icon = icon("shield-halved"),
        color = color,
        width = 3
      )
    })

    fraud_flag_n <- reactive({
      is_df <- tryCatch(d_is(), error = function(e) NULL)
      bs_df <- tryCatch(d_bs(), error = function(e) NULL)
      cf_df <- tryCatch(d_cf(), error = function(e) NULL)
      if (is.null(is_df) || is.null(bs_df) || is.null(cf_df) ||
          !is.data.frame(is_df) || !is.data.frame(bs_df) || !is.data.frame(cf_df) ||
          nrow(is_df) == 0L || nrow(bs_df) == 0L || nrow(cf_df) == 0L) {
        return(NA_integer_)
      }
      msgs <- tryCatch(collect_fraud_warnings(cf_df, is_df, bs_df), error = function(e) NULL)
      if (is.null(msgs)) return(NA_integer_)
      as.integer(length(msgs))
    })

    output$vbox_fraud <- renderUI({
      n <- tryCatch(fraud_flag_n(), error = function(e) NA_integer_)
      if (length(n) != 1L || is.null(n) || is.na(n) || !is.finite(n)) return(NULL)
      n <- as.integer(n)
      valueBox(
        paste0(n, " 項"),
        "財務舞弊警訊",
        icon = icon("exclamation-triangle"),
        color = "red",
        width = 3
      )
    })

    output$vbox_momentum <- renderValueBox({
      status <- mom_status()
      color <- if (isTRUE(status$triggered)) "green" else "navy"
      txt <- if (isTRUE(status$triggered)) "多頭確認" else "盤整/偏空"
      valueBox(txt, "趨勢動能（交易輔助）", icon = icon("chart-line"), color = color)
    })

    output$ui_recommendation <- renderUI({
      rec <- final_recommendation()
      div(class = paste("alert", rec$class),
          h4(icon(rec$icon), " ", rec$title),
          p(rec$text))
    })

    output$table_checklist <- renderTable({
      df <- f_score_eval()$checklist
      if (nrow(df) > 0) {
        df$`得分` <- ifelse(df$`得分` == 1, "✅ 通過", "❌ 未達標")
      }
      df
    }, striped = TRUE, hover = TRUE, width = "100%")

    output$ui_valuation_compare <- renderUI({
      pv <- primary_values()
      p_curr <- .pick_num(current_price())
      if (is.na(p_curr)) return(div(class = "alert alert-info", "正在等待市場資料..."))

      rec <- tryCatch(model_rec(), error = function(e) NULL)
      prim <- as.character(rec$primary %||% "")
      sec <- as.character(rec$secondary %||% "")
      conf <- tryCatch(confidence(), error = function(e) NULL)
      sec_pt <- .pick_num(tryCatch(secondary_point(), error = function(e) NA))

      bear <- pv$bear
      base <- pv$base
      bull <- pv$bull
      if (is.na(base)) {
        p_dcf <- .pick_num(tryCatch(intrinsic_val_dcf(), error = function(e) NA))
        p_ddm <- .pick_num(tryCatch(intrinsic_val_ddm(), error = function(e) NA))
        p_pb  <- .pick_num(tryCatch(intrinsic_val_pb(), error = function(e) NA))
        base <- if (!is.na(p_dcf)) p_dcf else if (!is.na(p_pb)) p_pb else p_ddm
      }

      rec_title <- paste0(
        "主模型：", .model_label(prim),
        if (nzchar(sec)) paste0("　｜　副模型：", .model_label(sec)) else ""
      )
      rec_desc <- as.character(rec$reason %||% "")
      conf_txt <- if (is.list(conf) && !is.null(conf$level)) {
        paste0(
          "可信度：", conf$level,
          if (!is.null(conf$score)) paste0("（", conf$score, "）") else "",
          if (length(conf$reasons)) paste0(" — ", paste(utils::head(conf$reasons, 3), collapse = "；")) else ""
        )
      } else {
        "可信度：計算中"
      }

      all_vals <- stats::na.omit(c(p_curr, bear, base, bull, sec_pt))
      if (!length(all_vals)) return(div(class = "alert alert-info", "等待估值結果..."))
      min_val <- min(all_vals) * 0.85
      max_val <- max(all_vals) * 1.15
      range_val <- max(max_val - min_val, 1)
      pos <- function(x) if (is.na(x)) NA_real_ else (x - min_val) / range_val * 100

      pos_curr <- pos(p_curr)
      pos_base <- pos(base)
      pos_bear <- pos(bear)
      pos_bull <- pos(bull)
      band_left <- if (!is.na(pos_bear) && !is.na(pos_bull)) min(pos_bear, pos_bull) else 0
      band_width <- if (!is.na(pos_bear) && !is.na(pos_bull)) abs(pos_bull - pos_bear) else 0
      band_opacity <- if (band_width > 0) 0.85 else 0
      base_opacity <- if (is.na(pos_base)) 0 else 1
      pos_base_css <- if (is.na(pos_base)) 0 else pos_base

      status_text <- "合理區間"
      status_color <- "#f39c12"
      if (!is.na(base) && p_curr < base * 0.8) {
        status_text <- "低估（相對 Base）"
        status_color <- "#00a65a"
      } else if (!is.na(base) && p_curr > base * 1.2) {
        status_text <- "高估（相對 Base）"
        status_color <- "#d9534f"
      }

      fmt <- function(x) if (is.na(x)) "—" else sprintf("$%.2f", x)
      upside <- if (!is.na(base) && p_curr > 0) (base - p_curr) / p_curr * 100 else NA_real_
      upside_txt <- if (is.na(upside)) "—" else sprintf("%+.1f%%", upside)

      HTML(paste0(
        "<div style='background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; border-top: 3px solid ", status_color, ";'>",
        "<div style='background: #3c8dbc15; border-left: 5px solid #3c8dbc; padding: 12px; border-radius: 4px; margin-bottom: 16px;'>",
        "<h5 style='color: #3c8dbc; margin-top: 0; font-weight: bold;'>", htmltools::htmlEscape(rec_title), "</h5>",
        "<p style='margin-bottom: 6px; font-size: 13px; color: #555;'>", htmltools::htmlEscape(rec_desc), "</p>",
        "<p style='margin: 0; font-size: 12.5px; color: #333;'><b>", htmltools::htmlEscape(conf_txt), "</b></p>",
        "</div>",
        "<div style='display:flex; gap:14px; flex-wrap:wrap; margin-bottom: 18px;'>",
        "<div style='flex:1; min-width:120px; padding:10px; background:#fdf2f2; border-radius:6px;'>",
        "<div style='font-size:12px; color:#888;'>Bear</div>",
        "<div style='font-size:20px; font-weight:700; color:#c0392b;'>", fmt(bear), "</div></div>",
        "<div style='flex:1; min-width:120px; padding:10px; background:#eaf2f8; border-radius:6px;'>",
        "<div style='font-size:12px; color:#888;'>Base</div>",
        "<div style='font-size:22px; font-weight:700; color:#2980b9;'>", fmt(base), "</div>",
        "<div style='font-size:12px; color:#555;'>潛在報酬 ", upside_txt, "</div></div>",
        "<div style='flex:1; min-width:120px; padding:10px; background:#eafaf1; border-radius:6px;'>",
        "<div style='font-size:12px; color:#888;'>Bull</div>",
        "<div style='font-size:20px; font-weight:700; color:#1e8449;'>", fmt(bull), "</div></div>",
        "<div style='flex:1; min-width:120px; padding:10px; background:#f4f6f7; border-radius:6px;'>",
        "<div style='font-size:12px; color:#888;'>副模型檢核</div>",
        "<div style='font-size:18px; font-weight:700; color:#566573;'>", fmt(sec_pt), "</div>",
        "<div style='font-size:12px; color:#777;'>", htmltools::htmlEscape(.model_label(sec)), "</div></div>",
        "</div>",
        "<h4 style='margin-top: 0; font-weight: bold;'><i class='fa fa-balance-scale'></i> 綜合估值狀態：",
        "<span style='color: ", status_color, ";'>", status_text, "</span></h4>",
        "<div style='position: relative; height: 80px; margin-top: 28px;'>",
        "<div style='position: absolute; top: 28px; left: 0; right: 0; height: 10px; background: #ecf0f1; border-radius: 5px;'></div>",
        "<div style='position: absolute; top: 28px; left: ", band_left, "%; width: ", band_width,
        "%; height: 10px; background: #aed6f1; border-radius: 5px; opacity: ", band_opacity, ";'></div>",
        "<div style='position: absolute; top: 0; left: ", pos_base_css, "%; transform: translateX(-50%); opacity: ", base_opacity, ";'>",
        "<div style='font-size: 11px; color: #7f8c8d;'>Base</div>",
        "<div style='width: 3px; height: 35px; background: #2980b9; margin: 0 auto;'></div></div>",
        "<div style='position: absolute; top: -10px; left: ", pos_curr, "%; transform: translateX(-50%); z-index: 10;'>",
        "<div style='font-size: 12px; color: white; background: #2c3e50; padding: 2px 6px; border-radius: 4px;'>目前市價</div>",
        "<div style='width: 12px; height: 12px; background: #2c3e50; border: 2px solid white; border-radius: 50%; margin: 2px auto;'></div>",
        "<div style='font-size: 15px; color: #2c3e50; font-weight: bold;'>$", round(p_curr, 2), "</div></div>",
        "</div>",
        "<p style='margin: 8px 0 0 0; font-size: 12px; color: #888;'>藍帶 = 主模型 Bear–Bull；點位為 Base。副模型僅作交叉驗證。</p>",
        "</div>"
      ))
    })
  })
}
