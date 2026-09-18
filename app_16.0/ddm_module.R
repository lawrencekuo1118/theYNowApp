# ==========================================
# ddm_module.R - 股利折現模型 (DDM) 後端模組
# ==========================================

ddm_module_server <- function(id, auto_calc_pulse = reactive(0L),
                              ddm_g = reactive(NULL), ddm_ke = reactive(NULL),
                              scraped_d0 = reactive(NULL),
                              summary_df = reactive(NULL), 
                              d_cash_flow = reactive(NULL), 
                              d_balance_sheet = reactive(NULL),
                              d_income_statement = reactive(NULL),
                              current_ticker = reactive(""),
                              quote_currency = reactive(NA),
                              financial_currency = reactive(NA),
                              capm_rf = reactive(NA),
                              capm_beta = reactive(NA),
                              capm_rm = reactive(NA),
                              use_estimated_re = reactive(FALSE)) {
  
  moduleServer(id, function(input, output, session) {
    
    .ddm_quote_shares <- function() {
      q_ccy <- tryCatch(quote_currency(), error = function(e) NULL)
      f_ccy <- tryCatch(financial_currency(), error = function(e) NULL)
      sh <- tryCatch(
        resolve_valuation_shares(
          d_balance_sheet(),
          summary_df(),
          ticker = tryCatch(current_ticker(), error = function(e) ""),
          quote_currency = q_ccy,
          financial_currency = f_ccy
        ),
        error = function(e) NULL
      )
      if (!is.null(sh) && is.finite(sh$shares) && sh$shares > 0) {
        # ADR／雙重股權：用約當報價股；報價幣≠財報幣且未約當 → 拒絕普通股 fallback
        if (shares_auto_adjust_method(sh$method)) return(sh$shares)
        if (statement_quote_units_differ(f_ccy, q_ccy)) return(NA_real_)
        return(sh$shares)
      }
      if (statement_quote_units_differ(f_ccy, q_ccy)) return(NA_real_)
      select_current_metric_any(d_balance_sheet(), SHARE_PATTERNS, "stock")
    }

    #' CF→D0 only when money units match session／quote (ADR FX already applied or same ccy).
    .ddm_cf_money_ok_for_d0 <- function(cf) {
      if (is.null(cf) || !is.data.frame(cf)) return(FALSE)
      q_ccy <- tryCatch(normalize_ccy(quote_currency()), error = function(e) NA_character_)
      f_ccy <- tryCatch(normalize_ccy(financial_currency()), error = function(e) NA_character_)
      mc <- tryCatch(normalize_ccy(attr(cf, "money_ccy")), error = function(e) NA_character_)
      if (is.na(mc)) mc <- f_ccy
      # Same statement／quote → OK; scaled CF tagged in quote／session USD／TWD → OK
      if (!statement_quote_units_differ(f_ccy, q_ccy)) return(TRUE)
      if (isTRUE(attr(cf, "money_scaled")) && !is.na(mc) && !is.na(q_ccy) &&
          (identical(mc, q_ccy) || identical(mc, "USD") || identical(mc, "TWD"))) {
        return(TRUE)
      }
      FALSE
    }

    # ==========================================
    # D0 = 現金股利（現金流量表）÷ 股數；若無則 0（新搜尋覆寫，可再手動改）
    # ==========================================
    .resolve_auto_d0 <- function() {
      d0 <- NA_real_
      cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
      bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      if (is.data.frame(cf) && nrow(cf) > 0 && is.data.frame(bs) && nrow(bs) > 0 &&
          isTRUE(.ddm_cf_money_ok_for_d0(cf))) {
        div_paid_total <- abs(select_current_metric(cf, "Cash Dividends Paid", "flow"))
        shares_issued <- .ddm_quote_shares()
        if (is.finite(div_paid_total) && is.finite(shares_issued) && shares_issued > 0) {
          d0 <- div_paid_total / shares_issued
        }
      }
      if (!is.finite(d0) || d0 < 0) {
        fallback_d0 <- tryCatch(scraped_d0(), error = function(e) NA_real_)
        if (is.numeric(fallback_d0) && length(fallback_d0) == 1 &&
            is.finite(fallback_d0) && fallback_d0 >= 0) {
          d0 <- fallback_d0
        }
      }
      if (!is.finite(d0) || d0 < 0) d0 <- 0
      round(as.numeric(d0)[1], 2)
    }

    .apply_auto_d0 <- function() {
      updateNumericInput(session, "d0", value = .resolve_auto_d0())
    }

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
      .apply_auto_d0()
      
      # --- 2. 股利 g：僅同步模式才套用中央值 ---
      if (isTRUE(input$sync_g) &&
          !is.null(ddm_g()) && is.numeric(ddm_g()) && length(ddm_g()) == 1 && !is.na(ddm_g())) {
        updateNumericInput(session, "g", value = round(as.numeric(ddm_g()), 2))
      }
      
      # --- 3. 同步折現率 (ke) ---
      if (!is.null(ddm_ke())) updateNumericInput(session, "ke", value = ddm_ke())
    }

    # 新搜尋先清成 0，避免留下上一檔的 D0；財報載入後再覆寫
    observeEvent(current_ticker(), {
      updateNumericInput(session, "d0", value = 0)
    }, ignoreNULL = TRUE)

    observeEvent(d_cash_flow(), {
      sync_ddm_to_financials()
    }, ignoreNULL = TRUE)
    
    observeEvent(input$reset_ddm, {
      updateCheckboxInput(session, "sync_g", value = TRUE)
      updateRadioButtons(session, "ddm_mode", selected = APP_DEFAULTS$ddm_mode)
      updateNumericInput(session, "g_stage1", value = APP_DEFAULTS$ddm_g_stage1)
      updateNumericInput(session, "yr_stage1", value = APP_DEFAULTS$ddm_yr_stage1)
      sync_ddm_to_financials()
      showNotification("🔁 DDM 參數已回復預設", type = "message")
    })
    
    .ddm_calc_requested <- function() {
      btn <- suppressWarnings(as.integer(input$btn_calc_ddm)[1])
      pulse <- suppressWarnings(as.integer(auto_calc_pulse())[1])
      (is.finite(btn) && btn >= 1L) || (is.finite(pulse) && pulse >= 1L)
    }

    # DDM 核心：Gordon／SPM／二階段
    ddm_calc <- eventReactive(list(input$btn_calc_ddm, auto_calc_pulse()), {
      if (!isTRUE(.ddm_calc_requested())) {
        return(list(status = "idle"))
      }
      d0 <- suppressWarnings(as.numeric(input$d0)[1])
      g_in <- suppressWarnings(as.numeric(input$g)[1])
      ke_in <- suppressWarnings(as.numeric(input$ke)[1])
      if (!is.finite(d0) || d0 < 0) d0 <- 0
      req(is.finite(g_in), is.finite(ke_in))
      g_dec <- g_in / 100
      ke_dec <- ke_in / 100
      mode <- as.character(input$ddm_mode %||% "gordon")[1]

      if (identical(mode, "two_stage")) {
        g1 <- suppressWarnings(as.numeric(input$g_stage1)[1]) / 100
        n1 <- suppressWarnings(as.integer(input$yr_stage1)[1])
        if (!is.finite(g1)) g1 <- g_dec
        if (!is.finite(n1) || n1 < 1L) n1 <- 5L
        if (ke_dec <= g_dec) {
          return(list(status = "error", message = "計算無效：Ke 必須嚴格大於永續股利成長率 g2！"))
        }
        p0 <- .ddm_formula_two_stage(d0 = d0, g1 = g1, n = n1, g2 = g_dec, ke = ke_dec)
        d1 <- d0 * (1 + g1)
        if (!is.finite(p0)) {
          return(list(status = "error", message = "二階段 DDM 無法計算，請檢查 g1／g2／Ke／年數。"))
        }
        return(list(status = "success", value = round(p0, 2), d1 = round(d1, 2), mode = "two_stage"))
      }

      if (identical(mode, "spm")) {
        eps <- suppressWarnings(as.numeric(input$est_eps)[1])
        if (!is.finite(eps)) {
          return(list(status = "error", message = "SPM 需要有效 EPS：請至 D0 分頁填入預估／最新 EPS。"))
        }
        if (ke_dec <= 0) {
          return(list(status = "error", message = "計算無效：要求報酬率 (Ke) 必須大於 0！"))
        }
        p0 <- .ddm_formula_spm(eps = eps, d = d0, g = g_dec, ke = ke_dec)
        if (!is.finite(p0)) {
          return(list(status = "error", message = "SPM 無法計算，請檢查 EPS／D0／g／Ke。"))
        }
        return(list(
          status = "success", value = round(p0, 2), d1 = round(d0, 2),
          eps = round(eps, 2), mode = "spm"
        ))
      }

      if (ke_dec <= g_dec) {
        return(list(status = "error", message = "計算無效：要求報酬率 (Ke) 必須嚴格大於股利成長率 g！"))
      }
      d1 <- d0 * (1 + g_dec)
      p0 <- d1 / (ke_dec - g_dec)
      return(list(status = "success", value = round(p0, 2), d1 = round(d1, 2), mode = "gordon"))
    }, ignoreNULL = FALSE)

    .ddm_calc_snapshot <- function() {
      if (!isTRUE(.ddm_calc_requested())) return(NULL)
      res <- tryCatch(ddm_calc(), error = function(e) NULL)
      if (is.null(res) || identical(res$status, "idle")) return(NULL)
      res
    }

    output$ui_ddm_result <- renderUI({
      res <- .ddm_calc_snapshot()
      if (is.null(res) || !identical(res$status, "error")) return(NULL)
      div(style = "color: #d9534f; font-weight: bold; padding: 10px; background-color: #fdf2f2; border-left: 4px solid #d9534f; margin-bottom: 10px;", res$message)
    })

    output$ibx_ddm_price <- renderInfoBox({
      res <- .ddm_calc_snapshot()
      val <- if (is.null(res) || !identical(res$status, "success")) {
        "N/A"
      } else {
        paste0(money_prefix(), res$value)
      }
      infoBox("每股估值（DDM）", val, icon = icon("hand-holding-usd"), color = "yellow", fill = TRUE)
    })

    output$ibx_ddm_d1 <- renderInfoBox({
      res <- .ddm_calc_snapshot()
      title <- if (!is.null(res) && identical(res$mode, "two_stage")) {
        "明年股利 D1（二階段）"
      } else if (!is.null(res) && identical(res$mode, "spm")) {
        "股利 D（SPM 定額永續）"
      } else {
        "預估明年股利 (D1)"
      }
      val <- if (is.null(res) || !identical(res$status, "success")) {
        "N/A"
      } else {
        paste0(money_prefix(), res$d1)
      }
      infoBox(title, val, icon = icon("money-bill-wave"), color = "aqua", fill = TRUE)
    })

    output$vtxt_ddm_setting_details <- renderUI({
      mode <- as.character(input$ddm_mode %||% "gordon")[1]
      d0 <- suppressWarnings(as.numeric(input$d0)[1])
      g <- suppressWarnings(as.numeric(input$g)[1])
      ke <- suppressWarnings(as.numeric(input$ke)[1])
      mode_lab <- if (identical(mode, "two_stage")) {
        "two_stage"
      } else if (identical(mode, "spm")) {
        "spm"
      } else {
        "gordon"
      }
      extra <- ""
      if (identical(mode, "two_stage")) {
        g1 <- suppressWarnings(as.numeric(input$g_stage1)[1])
        n1 <- suppressWarnings(as.integer(input$yr_stage1)[1])
        extra <- glue::glue("<b>高速期：</b> {if (is.finite(n1)) n1 else '—'} 年 ／ g1 = {if (is.finite(g1)) paste0(g1, '%') else '—'} <br/>")
      }
      if (identical(mode, "spm")) {
        eps <- suppressWarnings(as.numeric(input$est_eps)[1])
        eps_txt <- if (is.finite(eps)) paste0(money_prefix(), round(eps, 2)) else "—"
        extra <- glue::glue("<b>EPS (E)：</b> {eps_txt} <br/>")
      }
      d0_txt <- if (is.finite(d0)) paste0(money_prefix(), round(d0, 2)) else "—"
      g_txt <- if (is.finite(g)) paste0(g, "%") else "—"
      ke_txt <- if (is.finite(ke)) paste0(ke, "%") else "—"
      d_label <- if (identical(mode, "spm")) "股利 D（定額永續）" else "今年股利 D0"
      g_label <- if (identical(mode, "spm")) "盈餘／股利成長 g" else "永續股利成長 g"
      HTML(glue::glue("<div style='padding: 15px; background: #fcfcfc; border: 1px solid #eee; font-size: 14px;'>
                  <b>評價模式：</b> {mode_lab} <br/>
                  {extra}<b>{d_label}：</b> {d0_txt} <br/>
                  <b>{g_label}：</b> {g_txt} <br/>
                  <b>折現率 Ke：</b> {ke_txt}</div>"))
    })

    # DDM：Gordon／SPM／二階段；單位 D0（SPM 另含 EPS）
    output$param_sensitivity_table <- renderTable({
      shock_pct <- if (exists("PARAM_SENSITIVITY_SHOCK", inherits = TRUE)) PARAM_SENSITIVITY_SHOCK else 0.01
      d0 <- suppressWarnings(as.numeric(input$d0)[1])
      g0 <- suppressWarnings(as.numeric(input$g)[1])
      ke0 <- suppressWarnings(as.numeric(input$ke)[1])
      mode <- as.character(input$ddm_mode %||% "gordon")[1]
      g1_0 <- suppressWarnings(as.numeric(input$g_stage1)[1])
      n1_0 <- suppressWarnings(as.numeric(input$yr_stage1)[1])
      eps0 <- suppressWarnings(as.numeric(input$est_eps)[1])
      if (!is.finite(g1_0)) g1_0 <- g0
      if (!is.finite(n1_0) || n1_0 < 1) n1_0 <- 5
      .p <- function(d0u = 1, g_pct = g0, ke_pct = ke0, g1_pct = g1_0, n1 = n1_0, eps_u = eps0) {
        if (identical(mode, "two_stage")) {
          .ddm_formula_two_stage(
            d0 = d0u, g1 = g1_pct / 100, n = n1, g2 = g_pct / 100, ke = ke_pct / 100
          )
        } else if (identical(mode, "spm")) {
          .ddm_formula_spm(
            eps = eps_u, d = d0u, g = g_pct / 100, ke = ke_pct / 100
          )
        } else {
          .ddm_formula_p0(d0 = d0u, g = g_pct / 100, ke = ke_pct / 100)
        }
      }
      v0 <- if (identical(mode, "spm")) .p(d0u = if (is.finite(d0)) d0 else 0) else .p()
      validate(need(
        is.finite(v0),
        if (identical(mode, "spm")) {
          "基準公式尚未就緒：請確認 EPS、D、g、Ke（Ke > 0）。"
        } else {
          "基準公式尚未就緒：請確認 g、Ke（且 Ke > g）。"
        }
      ))
      .rel <- function(x, sign = -1) .param_rel_shock(x, sign = sign, shock = shock_pct)
      d0_show <- if (is.finite(d0)) d0 else 1
      d0_unit <- if (is.finite(d0)) money_prefix() else "x"
      d0_label <- if (identical(mode, "spm")) "股利 D（定額）" else "今年股利 D0"
      rows <- list(
        .param_sensitivity_infl_row(
          d0_label, d0_show, d0_unit, v0,
          if (identical(mode, "spm")) .p(d0u = d0_show * (1 - shock_pct)) else .p(d0u = 1 - shock_pct),
          if (identical(mode, "spm")) .p(d0u = d0_show * (1 + shock_pct)) else .p(d0u = 1 + shock_pct),
          if (identical(mode, "spm")) "SPM：P 含 D/Ke 項" else "P0 ∝ D0 ⇒ |ε|=1（公式；與股利金額／股價無關）"
        )
      )
      if (identical(mode, "spm") && .param_sensitivity_rel_ok(eps0)) {
        rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
          "EPS (E)", eps0, money_prefix(), v0,
          .p(eps_u = .rel(eps0, -1)),
          .p(eps_u = .rel(eps0, +1)),
          "SPM：成長項 ∝ E × g / Ke²"
        )
      }
      if (identical(mode, "two_stage") && .param_sensitivity_rel_ok(g1_0)) {
        rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
          "高速期成長 g1", g1_0, "%", v0,
          .p(g1_pct = .rel(g1_0, -1)),
          .p(g1_pct = .rel(g1_0, +1)),
          "二階段：前 n1 年股利成長"
        )
      }
      g_label <- if (identical(mode, "two_stage")) {
        "永續股利成長 g2"
      } else if (identical(mode, "spm")) {
        "成長率 g"
      } else {
        "股利成長率 g"
      }
      if (.param_sensitivity_rel_ok(g0)) {
        rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
          g_label, g0, "%", v0,
          .p(g_pct = .rel(g0, -1)),
          .p(g_pct = .rel(g0, +1)),
          if (identical(mode, "two_stage")) {
            "終值 Gordon：g2 < Ke"
          } else if (identical(mode, "spm")) {
            "SPM：成長項 ∝ g（無需 Ke > g）"
          } else {
            "ε = g/(1+g)+g/(Ke−g)（取決於 Ke、g）"
          }
        )
      }
      if (.param_sensitivity_rel_ok(ke0)) {
        rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
          "要求報酬率 Ke", ke0, "%", v0,
          .p(ke_pct = .rel(ke0, -1)),
          .p(ke_pct = .rel(ke0, +1)),
          if (identical(mode, "spm")) {
            "SPM：對 Ke 敏感度通常低於 Gordon"
          } else {
            "ε = −Ke/(Ke−g)（取決於 Ke、g）"
          }
        )
      }
      if (isTRUE(use_estimated_re())) {
        rf0 <- suppressWarnings(as.numeric(capm_rf())[1])
        beta0 <- suppressWarnings(as.numeric(capm_beta())[1])
        rm0 <- suppressWarnings(as.numeric(capm_rm())[1])
        capm_rows <- .param_sensitivity_capm_ke_rows(
          v0, function(ke_pct) .p(ke_pct = ke_pct),
          rf0, beta0, rm0, shock = shock_pct
        )
        if (length(capm_rows)) rows <- c(rows, capm_rows)
      }
      .param_sensitivity_sort_by_abs_eps(do.call(rbind, rows))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", width = "100%")
    
    # ==========================================
    # D0
    # ==========================================
    output$ibx_d0_scraped <- renderInfoBox({
      d0_sc <- tryCatch(scraped_d0(), error = function(e) 0)
      d0_sc <- suppressWarnings(as.numeric(d0_sc)[1])
      if (!is.finite(d0_sc) || d0_sc < 0) d0_sc <- 0
      val <- paste0(money_prefix(), round(d0_sc, 2))
      infoBox("財報最新股利 (D0)", val, icon = icon("money-bill-wave"), color = "aqua", fill = TRUE)
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
      infoBox("目前配息率", val, icon = icon("percent"), color = "purple", fill = TRUE)
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
      output$txt_d0_payout_res <- renderUI({ HTML(glue::glue("<div style='color: #00a65a; font-weight: bold;'>已成功將 D0 更新為 {money_prefix()}{round(new_d0, 2)}</div>")) })
      showNotification("D0 已依目標配息率更新，請回 Overview 重新試算", type = "message")
    })
    
    observeEvent(input$calc_d0_average, {
      req(d_cash_flow(), d_balance_sheet(), input$cycle_years)
      cf <- d_cash_flow()
      if (!isTRUE(.ddm_cf_money_ok_for_d0(cf))) {
        showNotification(
          "無法計算平均 D0：財報幣與報價幣未對齊／缺少匯率換算（請改用 Summary 股利或手動輸入）",
          type = "error"
        )
        return()
      }

      div_paid_seq <- select_clean_metric_row(cf, "Cash Dividends Paid", include_ttm = FALSE)
      shares <- .ddm_quote_shares()
      
      n_years <- min(input$cycle_years, length(div_paid_seq))
      valid_divs <- abs(na.omit(div_paid_seq[1:n_years]))
      
      avg_div <- mean(valid_divs)
      new_d0 <- if (!is.na(shares) && shares > 0) avg_div / shares else NA_real_
      if (is.na(new_d0)) {
        showNotification("無法計算平均 D0：股數資料不足", type = "error")
        return()
      }
      ui_text <- paste0(money_prefix(), round(new_d0, 2))
      
      updateNumericInput(session, "d0", value = round(new_d0, 2))
      output$txt_d0_avg_res <- renderUI({ HTML(glue::glue("<div style='color: #00a65a; font-weight: bold;'>已成功將 D0 更新為 {ui_text}</div>")) })
      showNotification("D0 已依歷史平均更新，請回 Overview 重新試算", type = "message")
    })
    
    return(list(
      ddm_price = reactive({
        res <- tryCatch(ddm_calc(), error = function(e) NULL)
        if (!is.null(res) && identical(res$status, "success")) res$value else NA
      })
    ))
  })
}
