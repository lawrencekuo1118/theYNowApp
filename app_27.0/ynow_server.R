# ==========================================
# server.R - 後端邏輯與資料運算 (專業財務修正版)
# ==========================================

# Focus cancel map. bump_search stays false on ticker tabs because
# .ynow_fire_ticker_search is the one gen bump that restarts keep.
ynow_logo_cancel_plan <- function(keep, ticker_tabs) {
  keep <- if (is.null(keep) || !length(keep)) "" else as.character(keep)[1]
  if (is.na(keep)) keep <- ""
  ticker_tabs <- if (is.null(ticker_tabs)) character() else as.character(ticker_tabs)
  list(
    bump_search = !isTRUE(keep %in% ticker_tabs),
    bump_bluechip = !identical(keep, "bluechip"),
    bump_cluster = !identical(keep, "lab_notes")
  )
}

ynow_logo_payload_focus <- function(x) {
  if (is.null(x) || !length(x)) return(FALSE)
  x <- x[[1]]
  if (isTRUE(x)) return(TRUE)
  if (is.character(x) && tolower(trimws(x)) %in% c("true", "1")) return(TRUE)
  if (is.numeric(x) && length(x) == 1L && is.finite(x) && identical(as.integer(x), 1L)) return(TRUE)
  FALSE
}

ynow_logo_inflight_stale <- function(inflight_gen, latest_gen) {
  if (is.null(inflight_gen) || !length(inflight_gen)) return(FALSE)
  g <- suppressWarnings(as.integer(inflight_gen[[1]])[1])
  latest <- suppressWarnings(as.integer(if (is.null(latest_gen) || !length(latest_gen)) NA else latest_gen[[1]])[1])
  if (length(g) != 1L || length(latest) != 1L || is.na(g) || is.na(latest)) return(FALSE)
  !identical(g, latest)
}

server <- function(input, output, session) {
  # iOS Safari drops the websocket during a long Lab Search; reconnect in-session
  # instead of forcing the user onto the gray "Disconnected / Reload" overlay.
  tryCatch(session$allowReconnect("force"), error = function(e) NULL)

  # Sidebar tab label busy dots (JS `ynowTabBusy` → animated 「…」 on menu item)
  .ynow_set_tab_busy <- function(tab, busy = TRUE) {
    tab <- as.character(tab %||% "")[1]
    if (!nzchar(tab) || is.na(tab)) return(invisible(FALSE))
    tryCatch(
      session$sendCustomMessage(
        "ynowTabBusy",
        list(tab = tab, busy = isTRUE(busy), ts = as.numeric(Sys.time()))
      ),
      error = function(e) NULL
    )
    invisible(TRUE)
  }
  # Action button label while a long job runs (JS `ynowBtnBusy`).
  .ynow_set_btn_busy <- function(id, busy = TRUE, idle_key = NULL, busy_key = NULL,
                                 label_id = NULL) {
    id <- as.character(id %||% "")[1]
    if (!nzchar(id) || is.na(id)) return(invisible(FALSE))
    loc <- tryCatch(
      normalize_ui_locale(isolate(ui_locale())),
      error = function(e) "en"
    )
    key <- if (isTRUE(busy)) {
      as.character(busy_key %||% "")[1]
    } else {
      as.character(idle_key %||% "")[1]
    }
    label <- if (nzchar(key) && exists("ui_str", mode = "function")) {
      tryCatch(ui_str(key, loc), error = function(e) key)
    } else {
      key
    }
    lid <- as.character(label_id %||% "")[1]
    tryCatch(
      session$sendCustomMessage(
        "ynowBtnBusy",
        list(
          id = id,
          label_id = if (nzchar(lid)) lid else NULL,
          busy = isTRUE(busy),
          label = as.character(label %||% "")[1],
          ts = as.numeric(Sys.time())
        )
      ),
      error = function(e) NULL
    )
    invisible(TRUE)
  }
  .ynow_busy_tab_now <- function(fallback = "dashboard") {
    tab <- tryCatch(
      as.character(isolate(input$sidebar_tabs) %||% "")[1],
      error = function(e) ""
    )
    if (!nzchar(tab) || is.na(tab)) {
      tab <- as.character(fallback %||% "dashboard")[1]
    }
    tab
  }
  
  # ==========================================
  # 🗄️ 全域資料容器 (儲存爬蟲結果與跨模組變數)
  # ==========================================
  summary_data <- reactiveVal(NULL)
  scraped_financials <- reactiveVal(NULL)
  # 載入後資料缺口：IS／BS／CF 全空；興櫃短註；櫃買季報摘要 fallback
  fs_all_empty <- reactiveVal(FALSE)
  tw_is_esb <- reactiveVal(FALSE)
  tw_tpex_fs_fallback <- reactiveVal(FALSE)  # TRUE 時 IS／BS 來自櫃買簡報（非完整三表）

  values <- reactiveValues(
    recentsearch = c(),
    pinned_search = c(),
    pinned_sidebar = c(),
    sidebar_tab_order = c(),
    user_settings = NULL
  )
  corp_industry_text <- reactiveVal("等待搜尋...")
  corp_display_name <- reactiveVal("")
  # 台股雙語：英文全稱（中文在 corp_display_name）；美股維持空字串
  corp_display_name_en <- reactiveVal("")
  # Search 後公司全稱字體色：官方 logo 主色（至多 2 色，#RRGGBB）
  corp_name_logo_colors <- reactiveVal(character(0))
  # Yahoo 公司簡介（Dashboard 概況：sector／HQ／summary…）
  company_profile <- reactiveVal(NULL)
  # 個股重要日期（Yahoo calendar）＋董監改選／可轉債（MOPS／TPEx）
  company_key_dates <- reactiveVal(NULL)
  company_board_election <- reactiveVal(NULL)
  company_cb_dilution <- reactiveVal(NULL)
  # 股權結構分析（Yahoo / TWSE / MOPS）
  company_ownership <- reactiveVal(NULL)
  own_selected_holder <- reactiveVal(NULL)
  own_holder_detail <- reactiveVal(NULL)
  
  # 初始值設為 NULL，避免一開啟 App 就自動執行爬蟲
  current_ticker <- reactiveVal(NULL)
  # Search must re-fetch even when the symbol is unchanged (Shiny skips
  # identical reactiveVal writes, so a second Search on TSM/NVDA was a no-op).
  ticker_search_gen <- reactiveVal(0L)
  # Gen value that means "drop the in-flight Search, do not fetch again".
  ticker_search_discard_gen <- reactiveVal(NA_integer_)
  .ynow_search_gen_is_discard <- function() {
    g <- suppressWarnings(as.integer(isolate(ticker_search_gen()) %||% 0L)[1])
    d <- suppressWarnings(as.integer(isolate(ticker_search_discard_gen()))[1])
    length(g) == 1L && length(d) == 1L && !is.na(g) && !is.na(d) && identical(g, d)
  }
  # Recent Search chip: pending fetch after a cross-market mode switch
  pending_recent_fetch <- reactiveVal(NULL)
  # 首次按下 Search 前：不標示模型推薦／側邊欄「推薦」
  user_has_searched <- reactiveVal(FALSE)

  # 參數更動稽核（1A）：Search／財報自動帶入後鎖定基準
  param_audit_baseline <- reactiveVal(NULL)
  param_audit_baseline_at <- reactiveVal(NULL)
  param_audit_baseline_ticker <- reactiveVal(NULL)
  param_audit_capture_token <- reactiveVal(0L)

  .capture_param_audit_baseline <- function(reason = "delay") {
    if (!isTRUE(isolate(user_has_searched()))) return(invisible(FALSE))
    tk <- isolate(current_ticker())
    if (is.null(tk) || !nzchar(as.character(tk)[1])) return(invisible(FALSE))
    vals <- tryCatch(ynow_capture_tracked_params(input), error = function(e) NULL)
    if (is.null(vals) || !length(vals)) return(invisible(FALSE))
    param_audit_baseline(vals)
    param_audit_baseline_at(Sys.time())
    param_audit_baseline_ticker(as.character(tk)[1])
    invisible(TRUE)
  }

  .schedule_param_audit_baseline <- function(delay_ms = 2200L) {
    tok <- isolate(param_audit_capture_token()) + 1L
    param_audit_capture_token(tok)
    shinyjs::delay(as.integer(delay_ms)[1], {
      if (!identical(isolate(param_audit_capture_token()), tok)) return()
      .capture_param_audit_baseline(reason = paste0("delay_", delay_ms))
    })
    invisible(tok)
  }

  # Session 幣別：原生 quote/statement → 單一顯示／估值幣別
  quote_currency <- reactiveVal("USD")
  statement_currency <- reactiveVal("USD")
  session_currency <- reactiveVal("USD")
  fx_usd_twd <- reactiveVal(NA_real_)
  fx_fetched_at <- reactiveVal(NULL)

  # 市場模式：美股｜台股（與顯示幣別分開）
  market_mode <- reactiveVal("US")
  set_market_mode("US")

  active_market_profile <- reactive({
    market_profile(market_mode())
  })

  active_bench_ticker <- reactive({
    as.character(active_market_profile()$backtest_bench %||% "SPY")[1]
  })
  
  # 系統核心估值變數
  estimated_g <- reactiveVal(NULL)
  estimated_g_raw <- reactiveVal(NULL)  # 封頂前原始預估營收成長率 (%)
  estimated_re <- reactiveVal(NULL)
  calculated_wacc <- reactiveVal(NULL)
  dcf_value_result <- reactiveVal(NULL)
  stock_price_estimate_val <- reactiveVal(NULL)
  dcf_audit_snap <- reactiveVal(NULL)

  # CAPM Beta：WACC「與基礎設定同步」（預設開）時跟隨基礎設定套用來源
  # driver: gs（基礎設定選定來源）| rolling | industry | manual（WACC 獨立）
  capm_beta_dirty <- reactiveVal(FALSE)
  capm_beta_updating <- reactiveVal(FALSE)
  sync_gs_beta_updating <- reactiveVal(FALSE)
  beta_capm_driver <- reactiveVal("gs")
  beta_link_from_capm <- reactiveVal(FALSE)
  beta_apply_choices_updating <- reactiveVal(FALSE)

  # 搜尋建議／Blue Chip 狀態（市場切換會提早寫入）
  sc_datalist_choices <- reactiveVal(ticker_presets_for_market("US"))
  lab_im_catalog_nonce <- reactiveVal(0L)
  lab_im_scores <- reactiveVal(NULL)
  lab_quality_snap_nonce <- reactiveVal(0L)
  # Pulse + busy flag: Search Blue Chips button and Blue Chip Leaderboard tab share one runner.
  lab_im_run_pulse <- reactiveVal(0L)
  lab_im_eval_busy <- reactiveVal(FALSE)
  lab_im_universe_busy <- reactiveVal(FALSE)
  lab_cluster_busy <- reactiveVal(FALSE)
  bt_async_busy <- reactiveVal(FALSE)
  ticker_search_busy <- reactiveVal(FALSE)
  # Latest button click wins: bump gen on each press; pending args restart when
  # the current ExtendedTask slot frees (cannot cancel hosted sequential workers).
  lab_im_eval_gen <- reactiveVal(0L)
  lab_im_eval_pending <- reactiveVal(NULL)
  lab_im_ind_mcap_gen <- reactiveVal(0L)
  lab_im_ind_mcap_pending <- reactiveVal(NULL)
  lab_cluster_gen <- reactiveVal(0L)
  lab_cluster_pending <- reactiveVal(NULL)
  # Gen of the worker inside ExtendedTask. Finish drops a result when focus
  # moved the latest gen and left no pending restart. Lock stays until return.
  .ynow_inflight_gen <- new.env(parent = emptyenv())
  .ynow_inflight_stale <- function(key, latest) {
    ynow_logo_inflight_stale(.ynow_inflight_gen[[key]], latest)
  }
  # ExtendedTask runners: long jobs leave the session thread so ≥2 features can overlap.
  .ynow_async_ok <- isTRUE(tryCatch(.ynow_async_available(), error = function(e) FALSE))
  lab_im_bc_task <- if (isTRUE(.ynow_async_ok)) {
    tryCatch(
      {
        fn <- lab_im_run_eval_job
        attr(fn, "ynow_job_name") <- "lab_im_run_eval_job"
        .ynow_make_extended_task(fn)
      },
      error = function(e) NULL
    )
  } else {
    NULL
  }
  lab_cluster_task <- if (isTRUE(.ynow_async_ok)) {
    tryCatch(
      {
        fn <- lab_cluster_run_job
        attr(fn, "ynow_job_name") <- "lab_cluster_run_job"
        .ynow_make_extended_task(fn)
      },
      error = function(e) NULL
    )
  } else {
    NULL
  }
  lab_im_ind_mcap_task <- if (isTRUE(.ynow_async_ok)) {
    tryCatch(
      {
        fn <- lab_im_run_ind_mcap_job
        attr(fn, "ynow_job_name") <- "lab_im_run_ind_mcap_job"
        .ynow_make_extended_task(fn)
      },
      error = function(e) NULL
    )
  } else {
    NULL
  }
  # Mirror Blue Chip eval / ind-mcap busy onto sidebar 「績優股排行榜」label dots
  observe({
    .ynow_set_tab_busy(
      "bluechip",
      isTRUE(lab_im_eval_busy()) || isTRUE(lab_im_ind_mcap_busy())
    )
  })
  observe({
    .ynow_set_tab_busy("lab_notes", isTRUE(lab_cluster_busy()))
  })
  observe({
    busy <- isTRUE(ticker_search_busy())
    .ynow_set_btn_busy(
      "search",
      busy = busy,
      idle_key = "btn_ticker_search",
      busy_key = "btn_ticker_search_busy"
    )
  })
  observe({
    busy <- isTRUE(lab_im_eval_busy())
    .ynow_set_btn_busy(
      "lab_im_run_fscore",
      busy = busy,
      idle_key = "btn_lab_im_run",
      busy_key = "btn_lab_im_run_busy"
    )
  })
  observe({
    busy <- isTRUE(lab_im_universe_busy())
    .ynow_set_btn_busy(
      "lab_im_refresh_universe",
      busy = busy,
      idle_key = "btn_lab_im_refresh",
      busy_key = "btn_lab_im_refresh_busy",
      label_id = "ynow_lab_im_refresh_label"
    )
  })
  observe({
    busy <- isTRUE(lab_im_ind_mcap_busy())
    .ynow_set_btn_busy(
      "lab_im_refresh_ind_mcap",
      busy = busy,
      idle_key = "btn_lab_im_refresh_ind_mcap",
      busy_key = "btn_lab_im_refresh_ind_mcap_busy",
      label_id = "ynow_lab_im_refresh_ind_mcap_label"
    )
  })
  observe({
    busy <- isTRUE(lab_cluster_busy())
    .ynow_set_btn_busy(
      "lab_cluster_run",
      busy = busy,
      idle_key = "btn_lab_cluster_run",
      busy_key = "btn_lab_cluster_run_busy",
      label_id = "ynow_lab_cluster_run_label"
    )
  })
  # Industry-chain mcap rank: empty until Blue Chip first mount (Starter RSS).
  lab_im_ind_mcap_snap <- reactiveVal(
    tryCatch(
      lab_ind_mcap_chain_snapshot_empty(),
      error = function(e) NULL
    )
  )
  lab_im_ind_mcap_busy <- reactiveVal(FALSE)
  # Armed once per market visit when Blue Chip tab auto-starts ind-mcap job.
  lab_im_ind_mcap_auto_armed <- reactiveVal(FALSE)
  # Armed once when sidebar Shared Search auto-start schedules deferred eval.
  lab_im_auto_start_armed <- reactiveVal(FALSE)
  lab_cluster_result <- reactiveVal(NULL)
  # Destroy Plotly/DT via renderUI idle placeholders on market switch
  # (empty plotly/validate leave stale htmlwidgets in the DOM).
  .clear_lab_cluster_result <- function(reason = NULL) {
    # Always write NULL (even if already NULL) so renderUI hosts rebuild.
    lab_cluster_result(NULL)
    tryCatch(
      updateTextInput(session, "lab_cluster_focus", value = ""),
      error = function(e) NULL
    )
    # Nuclear: strip any leftover htmlwidget hosts before idle placeholder paints.
    tryCatch(
      shinyjs::runjs(paste(
        "['lab_cluster_scatter_ui','lab_cluster_radar_ui','lab_cluster_table_ui'].forEach(function(id){",
        "  var el=document.getElementById(id); if(el){ el.innerHTML=''; }",
        "});"
      )),
      error = function(e) NULL
    )
    invisible(reason)
  }

  # Belt-and-suspenders: clear whenever market mode reactive flips.
  observeEvent(market_mode(), {
    .clear_lab_cluster_result("market_mode")
  }, ignoreInit = TRUE)

  # Lazy-mount nonce: bumped after each first-visit page mount (locale / input refresh)
  lazy_mount_nonce <- reactiveVal(0L)

  # Valuation-model / Blue Chip page accent; Basic Setup / other tabs clear theme
  observeEvent(input$sidebar_tabs, {
    tab <- as.character(input$sidebar_tabs %||% "")[1]
    if (identical(tab, "business_breakdown_lab")) {
      session$sendCustomMessage("ynowGotoTab", list(tab = "company_advance"))
      tab <- "company_advance"
    }
    # Signed-in account members cannot open Sys. Admin (internal tab `testing`).
    if (identical(tab, "testing")) {
      logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
      if (isTRUE(logged)) {
        tryCatch(
          updateTabItems(session, "sidebar_tabs", "home"),
          error = function(e) {
            tryCatch(
              session$sendInputMessage("sidebar_tabs", list(value = "home")),
              error = function(e2) NULL
            )
          }
        )
        tryCatch(
          session$sendCustomMessage("ynowGotoTab", list(tab = "home")),
          error = function(e) NULL
        )
        return()
      }
    }
    session$sendCustomMessage("ynowModelTheme", list(tab = tab))
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # JS market buttons also pulse this tick so Clustering clears even if mode is unchanged.
  observeEvent(input$lab_cluster_clear_tick, {
    .clear_lab_cluster_result("clear_tick")
  }, ignoreInit = TRUE)
  auto_calc_primary_sig <- reactiveVal("")
  auto_calc_fail_sig <- reactiveVal("")
  lite_scenario_applied_sig <- reactiveVal("")
  # Desired Lite scenario (survives hidden DCF radio lag after updateRadioButtons)
  lite_desired_des <- reactiveVal(NULL)
  # Lite Smart Analysis: silent DCF auto-calc (heal g≥discount; no error toasts)
  lite_dcf_silent <- reactiveVal(FALSE)
  # "" while calculating; "neg_terminal" when Gordon TV is refused and no anchor exists
  lite_dcf_block <- reactiveVal("")
  auto_calc_ddm_pulse <- reactiveVal(0L)
  auto_calc_pb_pulse <- reactiveVal(0L)
  auto_calc_nav_pulse <- reactiveVal(0L)
  auto_calc_ri_pulse <- reactiveVal(0L)
  auto_calc_rel_pulse <- reactiveVal(0L)
  auto_calc_sotp_pulse <- reactiveVal(0L)

  # ==========================================
  # 🚀 股票代號：僅主區 Ticker / Stock Code（sc + Search）
  # ==========================================
  # Shared Search path: main Search button + Recent Search chips.
  # goto_tab: NULL = stay on current tab; "dashboard" = 個股 (Lite → Smart Analysis).
  .ynow_fire_ticker_search <- function(sym_raw, goto_tab = NULL) {
    mode <- tryCatch(
      normalize_market_mode(isolate(market_mode())),
      error = function(e) "US"
    )
    raw <- trimws(as.character(sym_raw %||% "")[1])
    if (!nzchar(raw) || identical(toupper(raw), "NA")) return(invisible(FALSE))
    # YNOW / TYNOW (and /YNOW|/TYNOW) are Macro own-indices — open Macro expand,
    # do not scrape them as company tickers (they are not Yahoo listings).
    own_idx <- if (exists("ynow_index_parse_symbol", mode = "function")) {
      tryCatch(ynow_index_parse_symbol(raw), error = function(e) NULL)
    } else {
      NULL
    }
    if (is.list(own_idx) && nzchar(as.character(own_idx$symbol %||% "")[1])) {
      want_mkt <- as.character(own_idx$market %||% "US")[1]
      sym <- as.character(own_idx$symbol)[1]
      if (!identical(mode, want_mkt)) {
        mkt_js <- if (identical(want_mkt, "TW")) {
          "if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('market_mode_pick', 'TW', {priority: 'event'}); }"
        } else {
          "if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('market_mode_pick', 'US', {priority: 'event'}); }"
        }
        tryCatch(shinyjs::runjs(mkt_js), error = function(e) NULL)
      }
      tryCatch(updateTextInput(session, "sc", value = sym), error = function(e) NULL)
      tryCatch(
        session$sendCustomMessage("ynowGotoTab", list(tab = "macro_market")),
        error = function(e) NULL
      )
      tryCatch(
        session$sendCustomMessage(
          "ynowMacroOpenIndex",
          list(symbol = sym, market = want_mkt)
        ),
        error = function(e) NULL
      )
      tryCatch({
        loc <- tryCatch(isolate(ui_locale()), error = function(e) "en")
        tmpl <- if (exists("ui_str", mode = "function")) {
          ui_str("notif_open_own_index", loc)
        } else {
          "Opening {symbol} on Macro…"
        }
        msg <- if (exists(".ynow_fmt_str", mode = "function")) {
          .ynow_fmt_str(tmpl, symbol = sym)
        } else {
          gsub("\\{symbol\\}", sym, tmpl, fixed = FALSE)
        }
        showNotification(msg, type = "message", duration = 4)
      }, error = function(e) NULL)
      return(invisible(TRUE))
    }
    # Prefer stored Yahoo suffix when it already matches the active market.
    tk <- if (identical(mode, "TW") &&
              grepl("\\.(TW|TWO)$", raw, ignore.case = TRUE)) {
      toupper(gsub("\\s+", "", raw))
    } else {
      normalize_ticker_for_market(raw, mode)
    }
    if (is.na(tk) || !nzchar(tk)) return(invisible(FALSE))
    user_has_searched(TRUE)
    param_audit_baseline(NULL)
    param_audit_baseline_at(NULL)
    param_audit_baseline_ticker(NULL)
    param_audit_capture_token(isolate(param_audit_capture_token()) + 1L)
    current_ticker(tk)
    # A real Search wins over a focus discard that has not flushed yet.
    ticker_search_discard_gen(NA_integer_)
    ticker_search_gen(isolate(ticker_search_gen()) + 1L)
    disp <- display_ticker_for_market(tk, mode)
    tryCatch(updateTextInput(session, "sc", value = disp), error = function(e) NULL)
    if (!is.null(goto_tab) && nzchar(as.character(goto_tab)[1])) {
      tab <- as.character(goto_tab)[1]
      tryCatch(
        session$sendCustomMessage("ynowGotoTab", list(tab = tab)),
        error = function(e) NULL
      )
    }
    invisible(TRUE)
  }

  # Search button / submit only — never switch market on ticker typing alone.
  observeEvent(input$search, {
    req(input$sc)
    raw <- trimws(as.character(input$sc %||% "")[1])
    mode <- tryCatch(
      normalize_market_mode(isolate(market_mode())),
      error = function(e) "US"
    )
    # US mode + clear 4-digit TW code (e.g. 2330) → same path as toggling TW,
    # then search via pending_recent_fetch so the user is not stuck in US mode.
    if (identical(mode, "US") &&
        exists("looks_like_tw_stock_code", mode = "function") &&
        isTRUE(looks_like_tw_stock_code(raw))) {
      pend_sym <- toupper(gsub(
        "\\s+", "",
        sub("\\s+[—\\-–].*$", "", raw)
      ))
      pending_recent_fetch(list(
        symbol = pend_sym,
        goto_tab = NULL
      ))
      tryCatch(
        shinyjs::runjs(
          "if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('market_mode_pick', 'TW', {priority: 'event'}); }"
        ),
        error = function(e) NULL
      )
      return(invisible(TRUE))
    }
    .ynow_fire_ticker_search(input$sc, goto_tab = NULL)
  })

  # Recent Search chip: restore prior ticker scrape and jump to 個股 / Smart Analysis.
  observeEvent(input$recent_search_pick, {
    raw <- trimws(as.character(input$recent_search_pick %||% "")[1])
    req(nzchar(raw))
    mode <- tryCatch(
      normalize_market_mode(isolate(market_mode())),
      error = function(e) "US"
    )
    want_tw <- grepl("\\.(TW|TWO)$", raw, ignore.case = TRUE)
    # Cross-market recent: switch market first, then search after clear settles.
    if (isTRUE(want_tw) && !identical(mode, "TW")) {
      pending_recent_fetch(toupper(gsub("\\s+", "", raw)))
      tryCatch(
        shinyjs::runjs(
          "if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('market_mode_pick', 'TW', {priority: 'event'}); }"
        ),
        error = function(e) NULL
      )
      return(invisible(FALSE))
    }
    if (!isTRUE(want_tw) && identical(mode, "TW")) {
      pending_recent_fetch(toupper(gsub("\\s+", "", raw)))
      tryCatch(
        shinyjs::runjs(
          "if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('market_mode_pick', 'US', {priority: 'event'}); }"
        ),
        error = function(e) NULL
      )
      return(invisible(FALSE))
    }
    .ynow_fire_ticker_search(raw, goto_tab = "dashboard")
  }, ignoreInit = TRUE)

  # ==========================================
  # UI locale（語言）與顯示幣別分開；市場切換不覆寫語言
  # 初次預設：HTTP Accept-Language → device_ui_locale；手動 Language 優先
  # ==========================================
  .boot_locale_from_request <- tryCatch({
    req <- session$request
    al <- NULL
    if (!is.null(req)) {
      al <- req[["HTTP_ACCEPT_LANGUAGE"]]
      if (is.null(al) || !nzchar(as.character(al %||% "")[1])) {
        hdrs <- req$HEADERS
        if (!is.null(hdrs)) {
          al <- hdrs[["accept-language"]]
          if (is.null(al)) al <- hdrs[["Accept-Language"]]
        }
      }
    }
    locale_from_device_language(al, default = "en")
  }, error = function(e) "en")
  ui_locale <- reactiveVal(.boot_locale_from_request)
  ui_locale_user_picked <- reactiveVal(FALSE)
  device_ui_locale_applied <- reactiveVal(FALSE)

  # Account login (designed; no-op UI/actions while YNOW_ENABLE_ACCOUNT_LOGIN is off)
  ynow_auth <- if (exists("ynow_auth_server", mode = "function")) {
    tryCatch(
      ynow_auth_server(input, output, session, ui_locale = ui_locale),
      error = function(e) list(
        enabled = FALSE,
        user = reactiveVal(NULL),
        is_logged_in = reactive(FALSE),
        is_guest = reactive(TRUE)
      )
    )
  } else {
    list(
      enabled = FALSE,
      user = reactiveVal(NULL),
      is_logged_in = reactive(FALSE),
      is_guest = reactive(TRUE)
    )
  }
  session$userData$ynow_auth <- ynow_auth

  # Post-chrome login/resume work (ACL unlock, mint / last-ui, points).
  # Tick advances after the flush that delivers ynow-authed so chrome is first.
  # Login-class ACL must clear on that first post-chrome flush, before points,
  # so Asset Transmission (often ACL-locked with unlock_type=login) unlocks like
  # freemium Checklist instead of waiting on the points waterfall.
  ynow_auth_heavy_tick <- reactiveVal(0L)
  session$userData$ynow_schedule_auth_heavy <- function(uid = "",
                                                       notify_base = NULL,
                                                       after_chrome = NULL) {
    uid <- as.character(uid %||% "")[1]
    base_msg <- if (is.null(notify_base)) NULL else as.character(notify_base)[1]
    after_fn <- if (is.function(after_chrome)) after_chrome else NULL
    session$onFlushed(function() {
      # Class A ACL unlock (login) before mint/points. Chrome already flushed.
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      tryCatch(.ynow_push_maintenance(), error = function(e) NULL)
      # Mint / durable last-ui reconcile — never block the ynow-authed flush.
      if (is.function(after_fn)) {
        tryCatch(after_fn(), error = function(e) NULL)
      }
      pts_login <- NULL
      if (nzchar(uid) && exists("ynow_points_on_login", mode = "function")) {
        pts_login <- tryCatch(ynow_points_on_login(uid), error = function(e) NULL)
      }
      if (!is.null(base_msg) && nzchar(base_msg)) {
        note <- base_msg
        loc_fb <- function(key, fb) {
          tryCatch({
            if (exists("ui_str", mode = "function")) {
              ui_str(key, tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en"))
            } else fb
          }, error = function(e) fb)
        }
        if (isTRUE(pts_login$checkin_granted)) {
          amt <- tryCatch(ynow_points_daily_checkin_amount(), error = function(e) 30L)
          fmt <- loc_fb("pts_daily_checkin_fmt", "Daily check-in: +%s FYN.")
          note <- paste(base_msg, sprintf(fmt, as.character(amt)))
        } else if (isTRUE(pts_login$gift_granted)) {
          gift_n <- tryCatch(ynow_points_signup_bonus(), error = function(e) 100L)
          fmt <- loc_fb("pts_first_gift_fmt", "Welcome gift: +%s FYN.")
          note <- paste(base_msg, sprintf(fmt, as.character(gift_n)))
        }
        tryCatch(showNotification(note, type = "message", duration = 5), error = function(e) NULL)
      }
      ynow_auth_heavy_tick(isolate(ynow_auth_heavy_tick()) + 1L)
    }, once = TRUE)
    invisible(TRUE)
  }

  # Sysadmin view-as (defined early so pinned prefs / ACL subject can prefer it).
  # Value: list(public_id=, user_id=, role=) or NULL. No PII.
  ynow_pts_view_as <- reactiveVal(NULL)

  # ---- Pinned recent search (long-press 2s) ----
  #' Prefs subject: view-as target when active, else signed-in account.
  .ynow_pinned_auth_uid <- function() {
    va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
    if (is.list(va)) {
      vid <- as.character(va$user_id %||% "")[1]
      if (nzchar(vid)) return(vid)
    }
    u <- tryCatch(isolate(ynow_auth$user()), error = function(e) NULL)
    if (is.function(u)) u <- tryCatch(u(), error = function(e) NULL)
    if (!is.list(u)) return("")
    as.character(u$id %||% "")[1]
  }

  # Last screen is the signed-in member's. View-as is a different subject.
  .ynow_signed_in_uid <- function() {
    u <- tryCatch(isolate(ynow_auth$user()), error = function(e) NULL)
    if (is.function(u)) u <- tryCatch(u(), error = function(e) NULL)
    if (!is.list(u)) return("")
    as.character(u$id %||% "")[1]
  }

  # Persist last member screen (tab) while signed in — survives Sign out / resume.
  observeEvent(input$sidebar_tabs, {
    logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
    if (!isTRUE(logged)) return()
    if (!exists("ynow_prefs_update_last_ui_surface", mode = "function")) return()
    tab <- as.character(input$sidebar_tabs %||% "")[1]
    if (!nzchar(tab) || identical(tab, "testing")) return()
    if (identical(tab, "business_breakdown_lab")) tab <- "company_advance"
    uid <- tryCatch(.ynow_signed_in_uid(), error = function(e) "")
    if (!nzchar(uid %||% "")) return()
    tryCatch(
      ynow_prefs_update_last_ui_surface(uid, tab = tab),
      error = function(e) NULL
    )
  }, ignoreInit = TRUE)

  # Client/post-login asks server to re-run the last ticker search.
  observeEvent(input$ynow_auth_restore_ticker, {
    tk <- as.character(input$ynow_auth_restore_ticker %||% "")[1]
    if (!nzchar(tk)) return()
    logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
    if (!isTRUE(logged)) return()
    tryCatch(.ynow_fire_ticker_search(tk, goto_tab = NULL), error = function(e) NULL)
  }, ignoreInit = TRUE)
  .ynow_pinned_sync_guest_client <- function(pins) {
    tryCatch(
      session$sendCustomMessage(
        "ynowPinnedGuestSync",
        list(tickers = as.list(ynow_prefs_normalize_tickers(pins)))
      ),
      error = function(e) NULL
    )
  }
  .ynow_pinned_load_for_user <- function(uid) {
    if (!nzchar(uid) || !exists("ynow_prefs_get_pinned_recent", mode = "function")) {
      return(character(0))
    }
    tryCatch(ynow_prefs_get_pinned_recent(uid), error = function(e) character(0))
  }
  .ynow_pinned_persist <- function(uid, pins) {
    if (!nzchar(uid) || !exists("ynow_prefs_set_pinned_recent", mode = "function")) {
      return(invisible(pins))
    }
    tryCatch(ynow_prefs_set_pinned_recent(uid, pins), error = function(e) pins)
  }
  .ynow_recent_24h_load_for_user <- function(uid) {
    if (!nzchar(uid) || !exists("ynow_prefs_get_recent_search_24h", mode = "function")) {
      return(character(0))
    }
    tryCatch(ynow_prefs_get_recent_search_24h(uid), error = function(e) character(0))
  }
  .ynow_recent_24h_record <- function(uid, ticker) {
    if (!nzchar(uid) || !exists("ynow_prefs_record_recent_search", mode = "function")) {
      return(invisible(character(0)))
    }
    tryCatch(
      ynow_prefs_record_recent_search(uid, ticker),
      error = function(e) character(0)
    )
  }
  .ynow_recent_24h_merge_guest <- function(uid, guest_tickers) {
    if (!nzchar(uid) || !exists("ynow_prefs_merge_recent_search_24h", mode = "function")) {
      return(.ynow_recent_24h_load_for_user(uid))
    }
    tryCatch(
      ynow_prefs_merge_recent_search_24h(uid, guest_tickers),
      error = function(e) .ynow_recent_24h_load_for_user(uid)
    )
  }

  # Guest localStorage → session (browser-local only; not account-durable).
  observeEvent(input$pinned_search_guest_load, {
    uid <- .ynow_pinned_auth_uid()
    if (nzchar(uid)) return(invisible(NULL))
    raw <- as.character(input$pinned_search_guest_load %||% "")[1]
    part <- strsplit(raw, "|", fixed = TRUE)[[1]][1]
    tickers <- if (nzchar(part %||% "")) {
      strsplit(part, ",", fixed = TRUE)[[1]]
    } else {
      character(0)
    }
    values$pinned_search <- ynow_prefs_normalize_tickers(tickers)
  }, ignoreInit = TRUE)

  # Auth / view-as transitions: load durable pins + 24h search history for the
  # effective subject; drop them when neither real auth nor view-as applies.
  # ignoreInit: guests hydrate via pinned_search_guest_load; do not wipe that race.
  .ynow_pinned_last_uid <- reactiveVal("")
  observeEvent(
    {
      list(
        logged = tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE),
        uid = .ynow_pinned_auth_uid(),
        view_as = tryCatch(ynow_pts_view_as(), error = function(e) NULL),
        heavy = ynow_auth_heavy_tick()
      )
    },
    {
      uid <- .ynow_pinned_auth_uid()
      prev <- isolate(as.character(.ynow_pinned_last_uid() %||% "")[1])
      va_on <- {
        va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
        is.list(va) && nzchar(as.character(va$user_id %||% "")[1])
      }
      logged_now <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
      if (isTRUE(logged_now) && !isTRUE(va_on) &&
            as.integer(isolate(ynow_auth_heavy_tick()) %||% 0L)[1] < 1L) {
        return(invisible(NULL))
      }
      if (nzchar(uid)) {
        guest_pins <- isolate(as.character(values$pinned_search %||% character(0)))
        server_pins <- .ynow_pinned_load_for_user(uid)
        # Merge guest local pins only for a real sign-in (not view-as impersonation).
        if (!isTRUE(va_on) && length(guest_pins) &&
              exists("ynow_prefs_normalize_tickers", mode = "function")) {
          merged <- ynow_prefs_normalize_tickers(c(server_pins, guest_pins))
          if (!identical(merged, server_pins)) {
            server_pins <- .ynow_pinned_persist(uid, merged)
          }
        }
        values$pinned_search <- server_pins

        # Signed-in 24h search history → Recent Search chips + empty typeahead.
        guest_recent <- isolate(as.character(values$recentsearch %||% character(0)))
        if (!isTRUE(va_on) && length(guest_recent)) {
          server_recent <- .ynow_recent_24h_merge_guest(uid, guest_recent)
        } else {
          server_recent <- .ynow_recent_24h_load_for_user(uid)
        }
        # Keep pinned tickers out of the recent chip row (same rule as Search).
        if (length(server_pins)) {
          server_recent <- setdiff(server_recent, server_pins)
        }
        values$recentsearch <- head(
          server_recent,
          if (exists(".YNOW_RECENT_SEARCH_24H_MAX", inherits = TRUE)) {
            as.integer(.YNOW_RECENT_SEARCH_24H_MAX)[1]
          } else {
            20L
          }
        )

        tryCatch(
          session$sendCustomMessage("ynowPinnedGuestSync", list(clear = TRUE)),
          error = function(e) NULL
        )
        .ynow_pinned_last_uid(uid)
      } else if (nzchar(prev)) {
        # Signed out / left view-as: hide account pins + durable recent history
        # (guest session recent stays in-memory only for this browser session).
        values$pinned_search <- character(0)
        values$recentsearch <- character(0)
        .ynow_pinned_last_uid("")
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(input$pinned_search_toggle, {
    raw <- as.character(input$pinned_search_toggle %||% "")[1]
    req(nzchar(raw))
    tk <- strsplit(raw, "|", fixed = TRUE)[[1]][1]
    tk <- ynow_prefs_normalize_tickers(tk, max_n = 1L)
    req(length(tk) == 1L)
    uid <- .ynow_pinned_auth_uid()
    cur <- isolate(as.character(values$pinned_search %||% character(0)))
    was_pinned <- tk[[1]] %in% cur
    at_cap <- FALSE
    if (nzchar(uid) && exists("ynow_prefs_toggle_pinned_recent", mode = "function")) {
      nxt <- tryCatch(
        ynow_prefs_toggle_pinned_recent(uid, tk[[1]]),
        error = function(e) {
          if (was_pinned) {
            setdiff(cur, tk[[1]])
          } else if (length(cur) >= 12L) {
            at_cap <<- TRUE
            cur
          } else {
            c(tk[[1]], setdiff(cur, tk[[1]]))
          }
        }
      )
      at_cap <- isTRUE(attr(nxt, "at_cap"))
      attr(nxt, "at_cap") <- NULL
    } else {
      if (was_pinned) {
        nxt <- setdiff(cur, tk[[1]])
      } else if (length(cur) >= 12L) {
        at_cap <- TRUE
        nxt <- cur
      } else {
        nxt <- c(tk[[1]], setdiff(cur, tk[[1]]))
      }
      nxt <- ynow_prefs_normalize_tickers(nxt)
      .ynow_pinned_sync_guest_client(nxt)
    }
    values$pinned_search <- as.character(nxt %||% character(0))
    # Keep recent list free of pinned tickers (display + session order).
    values$recentsearch <- setdiff(
      as.character(isolate(values$recentsearch %||% character(0))),
      values$pinned_search
    )
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    if (isTRUE(at_cap)) {
      msg <- tryCatch(
        ui_str("recent_search_max_toast", loc),
        error = function(e) "You can pin up to 12 tickers."
      )
      tryCatch(showNotification(msg, type = "warning", duration = 2.8), error = function(e) NULL)
      return(invisible(NULL))
    }
    now_pinned <- tk[[1]] %in% values$pinned_search
    msg <- tryCatch(
      ui_str(if (now_pinned) "recent_search_pinned_toast" else "recent_search_unpinned_toast", loc),
      error = function(e) if (now_pinned) "Pinned." else "Unpinned."
    )
    msg <- gsub("{ticker}", tk[[1]], msg, fixed = TRUE)
    tryCatch(
      showNotification(msg, type = "message", duration = 2.2),
      error = function(e) NULL
    )
  }, ignoreInit = TRUE)

  # ---- Pinned sidebar tabs (max 12, 3-column tray; durable after login) ----
  .ynow_sidebar_pin_sync_guest <- function(pins) {
    tryCatch(
      session$sendCustomMessage(
        "ynowPinnedSidebarGuestSync",
        list(tabs = as.list(ynow_prefs_normalize_sidebar_tabs(pins)))
      ),
      error = function(e) NULL
    )
  }
  .ynow_sidebar_pin_mark_client <- function(pins) {
    tryCatch(
      session$sendCustomMessage(
        "ynowPinnedSidebarMark",
        list(tabs = as.list(ynow_prefs_normalize_sidebar_tabs(pins)))
      ),
      error = function(e) NULL
    )
  }
  .ynow_sidebar_pin_load_for_user <- function(uid) {
    if (!nzchar(uid) || !exists("ynow_prefs_get_pinned_sidebar", mode = "function")) {
      return(character(0))
    }
    tryCatch(ynow_prefs_get_pinned_sidebar(uid), error = function(e) character(0))
  }
  .ynow_sidebar_pin_persist <- function(uid, pins) {
    if (!nzchar(uid) || !exists("ynow_prefs_set_pinned_sidebar", mode = "function")) {
      return(invisible(pins))
    }
    tryCatch(ynow_prefs_set_pinned_sidebar(uid, pins), error = function(e) pins)
  }
  .ynow_sidebar_pin_label <- function(tab, loc = "en") {
    meta <- if (exists("ynow_sidebar_pin_meta", mode = "function")) {
      ynow_sidebar_pin_meta(tab)
    } else {
      list(locale = "", short_en = tab, short_zh = tab)
    }
    loc_key <- as.character(meta$locale %||% "")[1]
    if (nzchar(loc_key) && exists("ui_str", mode = "function")) {
      lab <- tryCatch(ui_str(loc_key, loc), error = function(e) "")
      lab <- trimws(as.character(lab %||% "")[1])
      if (nzchar(lab)) return(lab)
    }
    if (identical(loc, "zh-TW")) {
      return(as.character(meta$short_zh %||% tab)[1])
    }
    as.character(meta$short_en %||% tab)[1]
  }
  .ynow_sidebar_pin_short <- function(tab, loc = "en") {
    meta <- if (exists("ynow_sidebar_pin_meta", mode = "function")) {
      ynow_sidebar_pin_meta(tab)
    } else {
      list(short_en = tab, short_zh = tab)
    }
    if (identical(loc, "zh-TW")) {
      return(as.character(meta$short_zh %||% tab)[1])
    }
    as.character(meta$short_en %||% tab)[1]
  }

  observeEvent(input$pinned_sidebar_guest_load, {
    uid <- .ynow_pinned_auth_uid()
    if (nzchar(uid)) return(invisible(NULL))
    raw <- as.character(input$pinned_sidebar_guest_load %||% "")[1]
    part <- strsplit(raw, "|", fixed = TRUE)[[1]][1]
    tabs <- if (nzchar(part %||% "")) {
      strsplit(part, ",", fixed = TRUE)[[1]]
    } else {
      character(0)
    }
    pins <- ynow_prefs_normalize_sidebar_tabs(tabs)
    values$pinned_sidebar <- pins
    .ynow_sidebar_pin_mark_client(pins)
  }, ignoreInit = TRUE)

  .ynow_sidebar_pin_last_uid <- reactiveVal("")
  observeEvent(
    {
      list(
        logged = tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE),
        uid = .ynow_pinned_auth_uid(),
        view_as = tryCatch(ynow_pts_view_as(), error = function(e) NULL),
        heavy = ynow_auth_heavy_tick()
      )
    },
    {
      uid <- .ynow_pinned_auth_uid()
      prev <- isolate(as.character(.ynow_sidebar_pin_last_uid() %||% "")[1])
      va_on <- {
        va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
        is.list(va) && nzchar(as.character(va$user_id %||% "")[1])
      }
      logged_now <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
      if (isTRUE(logged_now) && !isTRUE(va_on) &&
            as.integer(isolate(ynow_auth_heavy_tick()) %||% 0L)[1] < 1L) {
        return(invisible(NULL))
      }
      if (nzchar(uid)) {
        guest_pins <- isolate(as.character(values$pinned_sidebar %||% character(0)))
        server_pins <- .ynow_sidebar_pin_load_for_user(uid)
        if (!isTRUE(va_on) && length(guest_pins) &&
              exists("ynow_prefs_normalize_sidebar_tabs", mode = "function")) {
          merged <- ynow_prefs_normalize_sidebar_tabs(c(server_pins, guest_pins))
          if (!identical(merged, server_pins)) {
            server_pins <- .ynow_sidebar_pin_persist(uid, merged)
          }
        }
        values$pinned_sidebar <- server_pins
        tryCatch(
          session$sendCustomMessage("ynowPinnedSidebarGuestSync", list(clear = TRUE)),
          error = function(e) NULL
        )
        .ynow_sidebar_pin_mark_client(server_pins)
        .ynow_sidebar_pin_last_uid(uid)
      } else if (nzchar(prev)) {
        values$pinned_sidebar <- character(0)
        .ynow_sidebar_pin_mark_client(character(0))
        .ynow_sidebar_pin_last_uid("")
      }
    },
    ignoreInit = TRUE
  )

  # Page-pin toggle disabled — long-press on tabs is edit-mode reorder only.
  observeEvent(input$pinned_sidebar_toggle, {
    invisible(NULL)
  }, ignoreInit = TRUE)

  # ---- Sidebar tab order (signed-in customize + long-press drag) ----
  .ynow_sidebar_order_push <- function(order) {
    ids <- if (exists("ynow_prefs_normalize_sidebar_order", mode = "function")) {
      ynow_prefs_normalize_sidebar_order(order, fill_defaults = TRUE)
    } else {
      as.character(order %||% character(0))
    }
    values$sidebar_tab_order <- ids
    tryCatch(
      session$sendCustomMessage(
        "ynowSidebarTabOrder",
        list(order = as.list(ids))
      ),
      error = function(e) NULL
    )
  }
  .ynow_sidebar_order_load <- function(uid) {
    if (!nzchar(uid) || !exists("ynow_prefs_get_sidebar_tab_order", mode = "function")) {
      if (exists("ynow_sidebar_default_order", mode = "function")) {
        return(ynow_sidebar_default_order())
      }
      return(character(0))
    }
    tryCatch(
      ynow_prefs_get_sidebar_tab_order(uid),
      error = function(e) {
        if (exists("ynow_sidebar_default_order", mode = "function")) {
          ynow_sidebar_default_order()
        } else {
          character(0)
        }
      }
    )
  }
  .ynow_sidebar_order_persist <- function(uid, ids) {
    if (!nzchar(uid) || !exists("ynow_prefs_set_sidebar_tab_order", mode = "function")) {
      return(invisible(ids))
    }
    tryCatch(ynow_prefs_set_sidebar_tab_order(uid, ids), error = function(e) ids)
  }

  .ynow_sidebar_order_last_uid <- reactiveVal("")
  observeEvent(
    {
      list(
        logged = tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE),
        uid = .ynow_pinned_auth_uid(),
        view_as = tryCatch(ynow_pts_view_as(), error = function(e) NULL),
        heavy = ynow_auth_heavy_tick()
      )
    },
    {
      uid <- .ynow_pinned_auth_uid()
      prev <- isolate(as.character(.ynow_sidebar_order_last_uid() %||% "")[1])
      logged_now <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
      va_on <- {
        va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
        is.list(va) && nzchar(as.character(va$user_id %||% "")[1])
      }
      if (isTRUE(logged_now) && !isTRUE(va_on) &&
            as.integer(isolate(ynow_auth_heavy_tick()) %||% 0L)[1] < 1L) {
        return(invisible(NULL))
      }
      if (nzchar(uid)) {
        order_ids <- .ynow_sidebar_order_load(uid)
        .ynow_sidebar_order_push(order_ids)
        .ynow_sidebar_order_last_uid(uid)
      } else if (nzchar(prev)) {
        if (exists("ynow_sidebar_default_order", mode = "function")) {
          .ynow_sidebar_order_push(ynow_sidebar_default_order())
        }
        .ynow_sidebar_order_last_uid("")
      }
    },
    ignoreInit = FALSE
  )

  observeEvent(input$sidebar_tab_order_guest, {
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    msg <- tryCatch(
      ui_str("sidebar_reorder_guest_toast", loc),
      error = function(e) "Sign in to rearrange sidebar order."
    )
    tryCatch(showNotification(msg, type = "message", duration = 2.4), error = function(e) NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$sidebar_tab_order_save, {
    uid <- .ynow_pinned_auth_uid()
    if (!nzchar(uid)) {
      loc <- tryCatch(ui_locale(), error = function(e) "en")
      msg <- tryCatch(
        ui_str("sidebar_reorder_guest_toast", loc),
        error = function(e) "Sign in to rearrange sidebar order."
      )
      tryCatch(showNotification(msg, type = "message", duration = 2.4), error = function(e) NULL)
      return(invisible(NULL))
    }
    raw <- as.character(input$sidebar_tab_order_save %||% "")[1]
    req(nzchar(raw))
    part <- strsplit(raw, "|", fixed = TRUE)[[1]][1]
    ids <- if (nzchar(part %||% "")) {
      strsplit(part, ",", fixed = TRUE)[[1]]
    } else {
      character(0)
    }
    # Do not invent Lite-omitted tabs into the saved order beyond defaults fill;
    # normalize keeps known vocabulary only (ACL / Lite omit still hide items in CSS).
    saved <- .ynow_sidebar_order_persist(uid, ids)
    .ynow_sidebar_order_push(saved)
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    msg <- tryCatch(
      ui_str("sidebar_reorder_saved_toast", loc),
      error = function(e) "Sidebar order saved."
    )
    tryCatch(showNotification(msg, type = "message", duration = 2.2), error = function(e) NULL)
  }, ignoreInit = TRUE)

  # ---- Signed-in Settings (hide locked / locale / market / compact / gain-loss) ----
  .ynow_settings_default <- function() {
    if (exists("ynow_prefs_default_user_settings", mode = "function")) {
      return(ynow_prefs_default_user_settings())
    }
    list(
      hide_locked_sidebar = FALSE,
      preferred_ui_locale = "auto",
      preferred_market = "auto",
      sidebar_compact = FALSE,
      gain_loss_scheme = "auto"
    )
  }
  .ynow_settings_push_client <- function(settings) {
    s <- if (exists("ynow_prefs_normalize_user_settings", mode = "function")) {
      ynow_prefs_normalize_user_settings(settings)
    } else {
      .ynow_settings_default()
    }
    values$user_settings <- s
    gain <- if (exists("ynow_prefs_normalize_gain_loss_scheme", mode = "function")) {
      ynow_prefs_normalize_gain_loss_scheme(s$gain_loss_scheme %||% "auto")
    } else {
      as.character(s$gain_loss_scheme %||% "auto")[1]
    }
    tryCatch(
      session$sendCustomMessage(
        "ynowUserSettings",
        list(
          hide_locked_sidebar = isTRUE(s$hide_locked_sidebar),
          sidebar_compact = isTRUE(s$sidebar_compact),
          gain_loss_scheme = gain
        )
      ),
      error = function(e) NULL
    )
    invisible(s)
  }
  .ynow_settings_load <- function(uid) {
    if (!nzchar(uid) || !exists("ynow_prefs_get_user_settings", mode = "function")) {
      return(.ynow_settings_default())
    }
    tryCatch(ynow_prefs_get_user_settings(uid), error = function(e) .ynow_settings_default())
  }
  .ynow_settings_persist <- function(uid, settings) {
    if (!nzchar(uid) || !exists("ynow_prefs_set_user_settings", mode = "function")) {
      return(ynow_prefs_normalize_user_settings(settings))
    }
    tryCatch(
      ynow_prefs_set_user_settings(uid, settings),
      error = function(e) ynow_prefs_normalize_user_settings(settings)
    )
  }
  .ynow_settings_apply_boot <- function(settings, apply_locale_market = TRUE) {
    s <- .ynow_settings_push_client(settings)
    if (!isTRUE(apply_locale_market)) return(invisible(s))
    loc_pref <- as.character(s$preferred_ui_locale %||% "auto")[1]
    if (nzchar(loc_pref) && !identical(loc_pref, "auto")) {
      tryCatch({
        ui_locale_user_picked(TRUE)
        .push_ui_locale(loc_pref, sync_picker = TRUE, force = TRUE)
      }, error = function(e) NULL)
    }
    mkt_pref <- as.character(s$preferred_market %||% "auto")[1]
    if (identical(mkt_pref, "US") || identical(mkt_pref, "TW")) {
      tryCatch({
        cur <- tryCatch(normalize_market_mode(isolate(market_mode())), error = function(e) "US")
        if (!identical(cur, mkt_pref)) {
          market_mode(mkt_pref)
          if (exists("set_market_mode", mode = "function")) {
            tryCatch(set_market_mode(mkt_pref), error = function(e) NULL)
          }
          .push_ui_locale(isolate(ui_locale()), sync_picker = FALSE)
        }
      }, error = function(e) NULL)
    }
    invisible(s)
  }
  .ynow_settings_last_uid <- reactiveVal("")
  observeEvent(
    {
      list(
        logged = tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE),
        uid = .ynow_pinned_auth_uid(),
        view_as = tryCatch(ynow_pts_view_as(), error = function(e) NULL),
        heavy = ynow_auth_heavy_tick()
      )
    },
    {
      uid <- .ynow_pinned_auth_uid()
      prev <- isolate(as.character(.ynow_settings_last_uid() %||% "")[1])
      logged_now <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
      va_on <- {
        va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
        is.list(va) && nzchar(as.character(va$user_id %||% "")[1])
      }
      if (isTRUE(logged_now) && !isTRUE(va_on) &&
            as.integer(isolate(ynow_auth_heavy_tick()) %||% 0L)[1] < 1L) {
        return(invisible(NULL))
      }
      if (nzchar(uid)) {
        loaded <- .ynow_settings_load(uid)
        # Apply locale/market only on uid change (login / view-as switch), not every tick.
        apply_boot <- !identical(uid, prev)
        .ynow_settings_apply_boot(loaded, apply_locale_market = apply_boot)
        .ynow_settings_last_uid(uid)
      } else if (nzchar(prev)) {
        .ynow_settings_push_client(.ynow_settings_default())
        .ynow_settings_last_uid("")
      }
    },
    ignoreInit = FALSE
  )

  output$ynow_settings_panel <- renderUI({
    # Re-render when self-view mode changes (signed-in / omniscient open Settings).
    ynow_pts_panel_tick()
    tryCatch(ynow_pts_self_view_mode(), error = function(e) NULL)
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    gated <- .ynow_account_gate_ui(loc)
    if (!is.null(gated)) return(gated)
    s <- isolate(values$user_settings)
    if (is.null(s)) {
      uid <- .ynow_pinned_auth_uid()
      s <- .ynow_settings_load(uid)
      values$user_settings <- s
    }
    if (exists("ynow_settings_panel_ui", mode = "function")) {
      ynow_settings_panel_ui(s, locale = loc)
    } else {
      tags$p(tryCatch(ui_str("settings_title", loc), error = function(e) "Settings"))
    }
  })

  observeEvent(input$ynow_settings_save, {
    uid <- .ynow_pinned_auth_uid()
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    self_member <- isTRUE(tryCatch(.ynow_self_view_is_member_like(), error = function(e) FALSE))
    if (!nzchar(uid) && !isTRUE(self_member)) {
      msg <- tryCatch(
        ui_str("settings_guest_toast", loc),
        error = function(e) "Sign in to change settings."
      )
      tryCatch(showNotification(msg, type = "message", duration = 2.4), error = function(e) NULL)
      return(invisible(NULL))
    }
    draft <- list(
      hide_locked_sidebar = isTRUE(input$ynow_settings_hide_locked),
      preferred_ui_locale = as.character(input$ynow_settings_ui_locale %||% "auto")[1],
      preferred_market = as.character(input$ynow_settings_market %||% "auto")[1],
      sidebar_compact = isTRUE(input$ynow_settings_sidebar_compact),
      gain_loss_scheme = as.character(input$ynow_settings_gain_loss %||% "auto")[1]
    )
    # Self-view signed-in / omniscient: operate Settings like a member, session-local only.
    if (isTRUE(self_member)) {
      saved <- if (exists("ynow_prefs_normalize_user_settings", mode = "function")) {
        ynow_prefs_normalize_user_settings(draft)
      } else {
        draft
      }
      .ynow_settings_apply_boot(saved, apply_locale_market = TRUE)
      msg <- tryCatch(
        ui_str("settings_self_view_toast", loc),
        error = function(e) {
          "Settings applied for this self view (not saved to an account)."
        }
      )
      tryCatch(showNotification(msg, type = "message", duration = 2.4), error = function(e) NULL)
      return(invisible(NULL))
    }
    saved <- .ynow_settings_persist(uid, draft)
    .ynow_settings_apply_boot(saved, apply_locale_market = TRUE)
    msg <- tryCatch(
      ui_str("settings_saved_toast", loc),
      error = function(e) "Settings saved."
    )
    tryCatch(showNotification(msg, type = "message", duration = 2.2), error = function(e) NULL)
  }, ignoreInit = TRUE)

  ynow_pts_panel_tick <- reactiveVal(0L)
  ynow_pts_sysadmin_on <- reactiveVal(FALSE)
  ynow_pts_sysadmin_tick <- reactiveVal(0L)
  # Ops portal login username (actor for admin_ops_audit); never store password.
  ynow_pts_sysadmin_actor <- reactiveVal("")
  ynow_admin_ops_display_tick <- reactiveVal(0L)
  # Panel payload: list(kind="locked"|"loading"|"ready"|"error", ...). Built async
  # so Postgres list_users cannot block the session (exit / Switch view stay live).
  ynow_pts_sysadmin_panel_state <- reactiveVal(list(kind = "locked"))
  # ynow_pts_view_as is created earlier (before pinned prefs).
  # Self view modes (admin preview): "guest" | "signed_in" | "omniscient" | NULL.
  ynow_pts_self_view_mode <- reactiveVal(NULL)

  #' Append one system-admin audit row (best-effort; fail closed inside store).
  .ynow_admin_ops_log <- function(action, detail = "") {
    if (!exists("ynow_admin_ops_append", mode = "function")) return(invisible(FALSE))
    actor <- as.character(
      tryCatch(isolate(ynow_pts_sysadmin_actor()), error = function(e) "") %||% ""
    )[1]
    tryCatch({
      ynow_admin_ops_append(
        action = action,
        detail = detail,
        actor = actor
      )
      ynow_admin_ops_display_tick(isolate(ynow_admin_ops_display_tick()) + 1L)
      TRUE
    }, error = function(e) FALSE)
  }

  .ynow_access_subject_id <- function() {
    mode <- tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL)
    mode <- as.character(mode %||% "")[1]
    if (identical(mode, "guest")) return("")
    if (identical(mode, "omniscient")) return("__omniscient__")
    if (identical(mode, "signed_in")) return("__self_member__")
    va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
    if (is.list(va)) {
      vid <- as.character(va$user_id %||% "")[1]
      if (nzchar(vid)) return(vid)
    }
    u <- tryCatch(isolate(ynow_auth$user()), error = function(e) NULL)
    if (is.function(u)) u <- tryCatch(u(), error = function(e) NULL)
    if (!is.list(u)) return("")
    as.character(u$id %||% "")[1]
  }

  .ynow_push_access_locks <- function() {
    if (!exists("ynow_access_effective_entries", mode = "function") &&
        !exists("ynow_access_entries", mode = "function")) {
      return(invisible(NULL))
    }
    mode <- tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL)
    mode <- as.character(mode %||% "")[1]
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    note <- tryCatch(
      ui_str("access_pane_note", loc),
      error = function(e) "This page is locked for the signed-in account."
    )
    if (identical(mode, "omniscient")) {
      session$sendCustomMessage(
        "ynowAccessLocks",
        list(
          ids = list(),
          entries = list(),
          note = note,
          badges = list(),
          server_now = format(
            as.POSIXct(Sys.time(), tz = "UTC"),
            "%Y-%m-%dT%H:%M:%SZ",
            tz = "UTC"
          )
        )
      )
      return(invisible(character()))
    }
    # guest / signed_in: only global (__all__) — no per-user overrides.
    # empty uid → ynow_access_entries merges global only.
    if (identical(mode, "guest") || identical(mode, "signed_in")) {
      uid <- ""
      authed <- identical(mode, "signed_in")
    } else {
      uid <- .ynow_access_subject_id()
      # Synthetic self-view subjects already handled above; treat real subjects normally.
      if (identical(uid, "__self_member__") || identical(uid, "__omniscient__")) {
        uid <- ""
      }
      authed <- nzchar(as.character(uid %||% "")[1])
    }
    # Guests still receive global (__all__) restrictions; login/points/conditional
    # unlock rules filter out items that are currently usable.
    ent <- tryCatch(
      {
        if (exists("ynow_access_effective_entries", mode = "function")) {
          ynow_access_effective_entries(uid, authed = authed)
        } else {
          ynow_access_entries(uid)
        }
      },
      error = function(e) {
        data.frame(id = character(), effect = character(), stringsAsFactors = FALSE)
      }
    )
    entries <- if (is.data.frame(ent) && nrow(ent) > 0L) {
      lapply(seq_len(nrow(ent)), function(i) {
        if (exists("ynow_access_lock_client_row", mode = "function")) {
          ynow_access_lock_client_row(
            id = as.character(ent$id[[i]]),
            effect = as.character(ent$effect[[i]]),
            unlock_type = if ("unlock_type" %in% names(ent)) ent$unlock_type[[i]] else NA_character_,
            points_cost = if ("points_cost" %in% names(ent)) ent$points_cost[[i]] else NA_integer_
          )
        } else {
          list(id = as.character(ent$id[[i]]), effect = as.character(ent$effect[[i]]))
        }
      })
    } else {
      list()
    }
    ids <- if (is.data.frame(ent)) as.character(ent$id) else character()
    badges <- if (exists("ynow_access_quota_badges", mode = "function")) {
      tryCatch(
        ynow_access_quota_badges(uid, authed = authed),
        error = function(e) list()
      )
    } else {
      list()
    }
    server_now <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    session$sendCustomMessage(
      "ynowAccessLocks",
      list(
        ids = as.list(ids),
        entries = entries,
        note = note,
        badges = badges,
        server_now = server_now
      )
    )
    invisible(ids)
  }

  #' Consume Class C free use and/or start time window after a successful use.
  .ynow_access_note_conditional_use <- function(item_id, consume_free = TRUE) {
    if (!exists("ynow_access_item_entry", mode = "function") ||
        !exists("ynow_access_touch_conditional", mode = "function")) {
      return(invisible(FALSE))
    }
    mode <- tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL)
    mode <- as.character(mode %||% "")[1]
    if (identical(mode, "omniscient") || identical(mode, "guest")) {
      return(invisible(FALSE))
    }
    uid <- .ynow_access_subject_id()
    uid <- as.character(uid %||% "")[1]
    if (!nzchar(uid) || identical(uid, "__omniscient__") ||
        identical(uid, "__self_member__")) {
      return(invisible(FALSE))
    }
    id <- as.character(item_id %||% "")[1]
    if (!nzchar(id)) return(invisible(FALSE))
    entry <- tryCatch(ynow_access_item_entry(uid, id), error = function(e) NULL)
    if (is.null(entry) || !identical(entry$unlock_type, "conditional")) {
      return(invisible(FALSE))
    }
    has_free <- !is.na(entry$free_count) && as.integer(entry$free_count) > 0L
    has_time <- !is.na(entry$time_limit_hours) && as.integer(entry$time_limit_hours) > 0L
    do_consume <- isTRUE(consume_free) && isTRUE(has_free)
    # Time-only: start window on first use; free+time: consume and refresh window.
    if (!do_consume && !isTRUE(has_time)) return(invisible(FALSE))
    tryCatch(
      ynow_access_touch_conditional(uid, id, consume_free = do_consume),
      error = function(e) NULL
    )
    tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
    invisible(TRUE)
  }


  .ynow_lock_spend_notify <- function(key, ..., fallback = key) {
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    msg <- tryCatch(ui_str(key, loc), error = function(e) fallback)
    args <- list(...)
    if (length(args) > 0L && grepl("%s", msg, fixed = TRUE)) {
      msg <- tryCatch(do.call(sprintf, c(list(msg), args)), error = function(e) msg)
    }
    tryCatch(
      showNotification(msg, type = "message", duration = 6),
      error = function(e) NULL
    )
    invisible(msg)
  }

  observeEvent(input$ynow_lock_spend, {
    raw <- input$ynow_lock_spend
    item_id <- if (is.list(raw)) {
      as.character(raw$item_id %||% "")[1]
    } else {
      as.character(raw %||% "")[1]
    }
    if (!nzchar(item_id)) return()

    mode <- tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL)
    mode <- as.character(mode %||% "")[1]
    va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
    if (identical(mode, "omniscient") || identical(mode, "guest") ||
        identical(mode, "signed_in") || !is.null(va) ||
        identical(mode, "__self_member__") || identical(mode, "__omniscient__")) {
      .ynow_lock_spend_notify(
        "lock_gate_preview_no_spend",
        fallback = "Points are not spent in this preview."
      )
      return()
    }

    uid <- .ynow_access_subject_id()
    uid <- as.character(uid %||% "")[1]
    if (!nzchar(uid) || identical(uid, "__omniscient__") ||
        identical(uid, "__self_member__") || identical(uid, YNOW_ACCESS_ALL_USERS)) {
      .ynow_lock_spend_notify(
        "lock_gate_preview_no_spend",
        fallback = "Points are not spent in this preview."
      )
      return()
    }

    if (!exists("ynow_access_item_entry", mode = "function") ||
        !exists("ynow_lock_gate_mode", mode = "function")) {
      return()
    }
    entry <- tryCatch(ynow_access_item_entry(uid, item_id), error = function(e) NULL)
    if (is.null(entry) || !identical(as.character(entry$effect %||% "")[1], "lock")) {
      .ynow_lock_spend_notify(
        "lock_gate_spend_denied",
        fallback = "This item cannot be unlocked with FYN."
      )
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      return()
    }

    if (exists("ynow_access_unlock_satisfied_for_item", mode = "function") &&
        isTRUE(ynow_access_unlock_satisfied_for_item(uid, item_id, authed = TRUE))) {
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      return()
    }

    gmode <- ynow_lock_gate_mode(
      TRUE,
      unlock_type = entry$unlock_type,
      points_cost = entry$points_cost
    )
    if (!identical(gmode, "spend")) {
      .ynow_lock_spend_notify(
        "lock_gate_spend_denied",
        fallback = "This item cannot be unlocked with FYN."
      )
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      return()
    }

    cost <- suppressWarnings(as.integer(entry$points_cost)[1])
    if (!is.finite(cost) || is.na(cost) || cost < 1L) {
      .ynow_lock_spend_notify(
        "lock_gate_spend_denied",
        fallback = "This item cannot be unlocked with FYN."
      )
      return()
    }

    if (!exists("ynow_points_credit", mode = "function") ||
        !exists("ynow_access_mark_points_unlocked", mode = "function")) {
      return()
    }
    if (exists("ynow_points_enabled", mode = "function") &&
        !isTRUE(ynow_points_enabled())) {
      .ynow_lock_spend_notify(
        "lock_gate_spend_denied",
        fallback = "This item cannot be unlocked with FYN."
      )
      return()
    }

    res <- tryCatch(
      ynow_points_credit(
        uid,
        delta = -as.integer(cost),
        reason = "access_unlock",
        ref_id = paste0("acl-unlock:", item_id),
        wallet = "fyn",
        meta = list(item_id = item_id, points_cost = cost)
      ),
      error = function(e) list(ok = FALSE, message_key = "pts_msg_backend")
    )
    if (isTRUE(res$ok)) {
      tryCatch(ynow_access_mark_points_unlocked(uid, item_id), error = function(e) NULL)
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      .ynow_lock_spend_notify("lock_gate_spend_ok", fallback = "Unlocked.")
      return()
    }
    if (identical(as.character(res$message_key %||% "")[1], "pts_msg_insufficient")) {
      bal <- suppressWarnings(as.integer(res$fyn %||% res$balance %||% 0L)[1])
      .ynow_lock_spend_notify(
        "lock_gate_insufficient_fmt",
        as.character(cost),
        as.character(if (is.finite(bal)) bal else 0L),
        fallback = sprintf(
          "Not enough FYN (need %s; balance %s). Open Points top-up to credit FYN.",
          cost, if (is.finite(bal)) bal else 0L
        )
      )
      return()
    }
    .ynow_lock_spend_notify(
      "lock_gate_spend_denied",
      fallback = "This item cannot be unlocked with FYN."
    )
  }, ignoreInit = TRUE)

  #' Push active maintenance windows (all users). Separate from ACL / guest locks.
  .ynow_push_maintenance <- function() {
    if (!exists("ynow_maintenance_client_payload", mode = "function")) {
      return(invisible(NULL))
    }
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    payload <- tryCatch(
      ynow_maintenance_client_payload(locale = loc),
      error = function(e) {
        list(
          entries = list(), ids = list(),
          server_now = NA_character_, next_end = NA_character_
        )
      }
    )
    session$sendCustomMessage("ynowMaintenance", payload)
    invisible(payload)
  }

  .ynow_access_deny_download <- function(item_id, file) {
    mode <- tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL)
    if (identical(as.character(mode %||% "")[1], "omniscient")) return(FALSE)
    if (exists("ynow_maintenance_is_active", mode = "function") &&
        isTRUE(ynow_maintenance_is_active(item_id))) {
      loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
      msg <- tryCatch(
        ui_str("maint_denied_report", loc),
        error = function(e) "This report is under maintenance."
      )
      tryCatch(showNotification(msg, type = "warning", duration = 6), error = function(e) NULL)
      writeLines(msg, file, useBytes = TRUE)
      return(TRUE)
    }
    if (!exists("ynow_access_is_locked", mode = "function")) return(FALSE)
    uid <- .ynow_access_subject_id()
    if (identical(uid, "__omniscient__")) return(FALSE)
    # Global (__all__) applies even when not signed in; honor unlock conditions.
    # signed_in self-view uses synthetic __self_member__ → global-only via entries merge.
    if (!isTRUE(ynow_access_is_locked(uid, item_id, effective = TRUE))) return(FALSE)
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    msg <- tryCatch(
      ui_str("access_denied_report", loc),
      error = function(e) "This report is locked for the signed-in account."
    )
    tryCatch(showNotification(msg, type = "warning", duration = 6), error = function(e) NULL)
    writeLines(msg, file, useBytes = TRUE)
    TRUE
  }

  #' Deny guest downloads of app-generated artifacts. Writes a note file when
  #' `file` is supplied (downloadHandler). Opens the mono Sign-in modal.
  .ynow_deny_guest_download <- function(file = NULL, msg_key = "download_need_login") {
    lite <- isTRUE(tryCatch(isolate(input$ynow_lite_mode), error = function(e) FALSE))
    if (isTRUE(lite)) return(FALSE)
    mode <- tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL)
    mode <- as.character(mode %||% "")[1]
    if (identical(mode, "omniscient") || identical(mode, "signed_in")) return(FALSE)
    uid <- .ynow_access_subject_id()
    denied <- if (exists("ynow_guest_download_denied", mode = "function")) {
      isTRUE(ynow_guest_download_denied(uid, lite = FALSE))
    } else {
      auth_on <- exists("ynow_auth_enabled", mode = "function") && isTRUE(ynow_auth_enabled())
      isTRUE(auth_on) && !nzchar(uid)
    }
    if (!denied) return(FALSE)
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    msg <- tryCatch(
      ui_str(msg_key, loc),
      error = function(e) "Sign in to download this file."
    )
    if (identical(msg, msg_key) || !nzchar(as.character(msg)[1])) {
      msg <- tryCatch(
        ui_str("auth_pa_gate_title", loc),
        error = function(e) "Sign in required"
      )
    }
    tryCatch(showNotification(msg, type = "warning", duration = 6), error = function(e) NULL)
    tryCatch(
      session$sendCustomMessage("ynowAuthOpenModal", list(ok = TRUE)),
      error = function(e) {
        tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e2) NULL)
      }
    )
    if (!is.null(file)) {
      tryCatch(writeLines(as.character(msg)[1], file, useBytes = TRUE), error = function(e) NULL)
    }
    TRUE
  }
  session$userData$ynow_deny_guest_download <- .ynow_deny_guest_download

  .ynow_pdf_note_file <- function(file, msg) {
    msg <- as.character(msg %||% "")[1]
    tryCatch({
      grDevices::pdf(file, width = 8.27, height = 11.69)
      plot.new()
      text(0.5, 0.55, msg, cex = 1.05)
      grDevices::dev.off()
    }, error = function(e) {
      writeLines(msg, file, useBytes = TRUE)
    })
    invisible(TRUE)
  }

  .ynow_pdf_fingerprint <- function() {
    grab <- function(id) {
      tryCatch(trimws(as.character(isolate(input[[id]]) %||% "")[1]), error = function(e) "")
    }
    tk <- tryCatch(trimws(as.character(isolate(current_ticker()) %||% "")[1]), error = function(e) "")
    settings <- list(
      market = tryCatch(trimws(as.character(isolate(market_mode()) %||% "")[1]), error = function(e) ""),
      dcf_mode = grab("dcf_mode"),
      wacc_gordon = grab("wacc_gordon"),
      wacc_stage1 = grab("wacc_stage1"),
      wacc_stage2 = grab("wacc_stage2"),
      sgr = grab("sgr"),
      g_stage1 = grab("g_stage1"),
      industry = grab("industry_choice")
    )
    if (!exists("ynow_pdf_settings_key", mode = "function")) return("")
    ynow_pdf_settings_key(tk, settings)
  }

  #' Login + daily free quota, then 10 FYN. NULL means the denial file is already written.
  #' Lite: free PDF (no login / FYN); still requires a ticker.
  .ynow_pdf_authorize <- function(file) {
    lite <- isTRUE(tryCatch(isolate(input$ynow_lite_mode), error = function(e) FALSE)) ||
      isTRUE(tryCatch(isolate(lite_mode()), error = function(e) FALSE))
    auth_on <- exists("ynow_auth_enabled", mode = "function") && isTRUE(ynow_auth_enabled())
    claim_ok <- exists("ynow_pdf_claim", mode = "function")
    uid <- .ynow_access_subject_id()
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    tk <- tryCatch(trimws(as.character(isolate(current_ticker()) %||% "")[1]), error = function(e) "")
    if (!nzchar(tk) || identical(toupper(tk), "NA")) {
      msg <- .lab("pdf_need_ticker", "Enter a stock ticker before downloading the report.")
      showNotification(msg, type = "warning", duration = 6)
      .ynow_pdf_note_file(file, msg)
      return(NULL)
    }
    if (isTRUE(lite)) {
      return(list(ok = TRUE, kind = "lite"))
    }
    # Omniscient self-view: lift download gates for admin preview.
    if (identical(uid, "__omniscient__") ||
        identical(as.character(
          tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL) %||% ""
        )[1], "omniscient")) {
      return(list(ok = TRUE, kind = "omniscient"))
    }
    if (!auth_on || !claim_ok) {
      return(list(ok = TRUE, kind = "bypass"))
    }
    # Signed-in self-view: past login gate; skip real FYN claim (no wallet).
    if (identical(uid, "__self_member__")) {
      return(list(ok = TRUE, kind = "self_signed_in"))
    }
    if (!nzchar(uid)) {
      msg <- .lab("pdf_need_login", "Sign in to download the valuation report (PDF).")
      showNotification(msg, type = "warning", duration = 6)
      tryCatch(
        session$sendCustomMessage("ynowAuthOpenModal", list(ok = TRUE)),
        error = function(e) {
          tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e2) NULL)
        }
      )
      .ynow_pdf_note_file(file, msg)
      return(NULL)
    }
    fp <- tryCatch(.ynow_pdf_fingerprint(), error = function(e) "")
    ticket <- ynow_pdf_claim(uid, fingerprint = fp)
    if (!isTRUE(ticket$ok)) {
      cost <- ticket$cost %||% 10L
      bal <- ticket$fyn %||% 0L
      title <- if (exists("ynow_points_sku_title", mode = "function")) {
        ynow_points_sku_title("pdf_report", loc)
      } else {
        "PDF"
      }
      msg <- tryCatch(
        sprintf(
          ui_str("pts_msg_insufficient_fmt", loc),
          as.character(cost), title, as.character(bal)
        ),
        error = function(e) "Not enough FYN points for this PDF download."
      )
      showNotification(msg, type = "warning", duration = 8)
      .ynow_pdf_note_file(file, msg)
      return(NULL)
    }
    ticket
  }

  ynow_maint_display_tick <- reactiveVal(0L)

  observe({
    logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
    tryCatch(ynow_auth$user(), error = function(e) NULL)
    ynow_pts_view_as()
    ynow_pts_self_view_mode()
    heavy <- ynow_auth_heavy_tick()
    # Guest / view-mode: push immediately. Fresh sign-in waits for post-chrome tick.
    if (isTRUE(logged) && as.integer(heavy %||% 0L)[1] < 1L) {
      return(invisible(NULL))
    }
    .ynow_push_access_locks()
    .ynow_push_maintenance()
  })

  # Auto-lift: recompute on server clock when the next maintenance window ends
  # (no admin click). Cap poll so long-empty sessions stay light.
  observe({
    if (!exists("ynow_maintenance_next_end", mode = "function")) {
      return(invisible(NULL))
    }
    now <- tryCatch(ynow_maintenance_now(), error = function(e) Sys.time())
    nxt <- tryCatch(ynow_maintenance_next_end(now = now), error = function(e) NA)
    delay_ms <- 60000L
    if (inherits(nxt, "POSIXt") && length(nxt) >= 1L && !is.na(nxt[[1]])) {
      secs <- as.numeric(difftime(nxt[[1]], now, units = "secs"))
      if (is.finite(secs)) {
        # Wake shortly after end; also poll at least every 60s while windows exist.
        delay_ms <- as.integer(max(1000L, min(60000L, ceiling(secs * 1000) + 250L)))
      }
    }
    invalidateLater(delay_ms, session)
    .ynow_push_maintenance()
    ynow_maint_display_tick(isolate(ynow_maint_display_tick()) + 1L)
    invisible(NULL)
  })

  # Client asks for a recompute after a scheduled window end (backup to invalidateLater).
  observeEvent(input$ynow_maint_refresh, {
    .ynow_push_maintenance()
    ynow_maint_display_tick(isolate(ynow_maint_display_tick()) + 1L)
  }, ignoreInit = TRUE)

  # Client countdown expired → refresh Class C badge remaining / ends_at.
  observeEvent(input$ynow_quota_badge_refresh, {
    tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
    tryCatch(.ynow_push_maintenance(), error = function(e) NULL)
    tryCatch(.ynow_pdf_quota_push(), error = function(e) NULL)
  }, ignoreInit = TRUE)

  #' Effective points user: view-as uuid when active, else signed-in account.
  .ynow_pts_effective_user <- function() {
    va <- tryCatch(ynow_pts_view_as(), error = function(e) NULL)
    if (is.list(va)) {
      vid <- as.character(va$user_id %||% "")[1]
      if (nzchar(vid)) {
        return(list(
          id = vid,
          role = as.character(va$role %||% "user")[1],
          public_id = as.character(va$public_id %||% "")[1],
          view_as = TRUE
        ))
      }
    }
    tryCatch(ynow_auth$user(), error = function(e) NULL)
  }
  session$userData$ynow_pts_effective_user <- .ynow_pts_effective_user
  session$userData$ynow_pts_view_as <- ynow_pts_view_as

  #' Push / clear view-as chrome + silver title Loading (no PII beyond public_id).
  #' Client mirrors that user's session UI (ynow-authed chrome, unlocks, prefs).
  .ynow_view_as_push_chrome <- function(va = NULL) {
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    if (is.null(va)) {
      va <- tryCatch(ynow_pts_view_as(), error = function(e) NULL)
    }
    if (!is.list(va) || !nzchar(as.character(va$public_id %||% "")[1])) {
      tryCatch(
        session$sendCustomMessage("ynowViewAsMode", list(active = FALSE)),
        error = function(e) NULL
      )
      # Restore real auth chrome (sysadmin portal is guest unless email-signed-in).
      logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
      email <- ""
      if (isTRUE(logged)) {
        u <- tryCatch(ynow_auth$user(), error = function(e) NULL)
        if (is.function(u)) u <- tryCatch(u(), error = function(e) NULL)
        if (is.list(u)) email <- as.character(u$email %||% "")[1]
      }
      if (is.function(tryCatch(ynow_auth$push_auth_state, error = function(e) NULL))) {
        tryCatch(ynow_auth$push_auth_state(logged, email), error = function(e) NULL)
      }
      return(invisible(FALSE))
    }
    pid <- as.character(va$public_id %||% "")[1]
    tryCatch(
      session$sendCustomMessage(
        "ynowViewAsMode",
        list(
          active = TRUE,
          public_id = pid,
          # Session-like UI: treat as signed-in for guest locks / member gates / chrome.
          session_like = TRUE,
          active_label = .lab("pts_sysadmin_viewas_active", "User view active"),
          hint = .lab(
            "pts_sysadmin_viewas_chrome_hint",
            "Title Loading is silver. Chrome, unlocks, balances, and prefs follow this user."
          ),
          exit_label = .lab("pts_sysadmin_viewas_exit", "Exit user view"),
          label_sign_out = .lab("auth_sign_out", "Sign out"),
          reminder_title = .lab(
            "pts_sysadmin_mode_reminder_viewas_title",
            "View-as selected user"
          ),
          reminder_body = .lab(
            "pts_sysadmin_mode_reminder_viewas_body",
            paste0(
              "You are viewing the app as this de-identified user. ",
              "Title Loading is silver. Exit from the top bar when finished."
            )
          ),
          reminder_dismiss = .lab("pts_sysadmin_mode_reminder_dismiss", "Dismiss"),
          reminder_aria = .lab(
            "pts_sysadmin_mode_reminder_aria",
            "Active system admin view mode"
          )
        )
      ),
      error = function(e) NULL
    )
    invisible(TRUE)
  }

  # Keep silver Loading / chrome in sync whenever view-as changes.
  observe({
    va <- ynow_pts_view_as()
    .ynow_view_as_push_chrome(va)
  })

  #' Push / clear self view body classes (guest / signed_in / omniscient).
  #' Also sends localized copy for the top-of-screen mode reminder.
  .ynow_self_view_push_chrome <- function(mode = NULL) {
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    if (is.null(mode)) {
      mode <- tryCatch(ynow_pts_self_view_mode(), error = function(e) NULL)
    }
    mode <- as.character(mode %||% "")[1]
    if (!mode %in% c("guest", "signed_in", "omniscient")) {
      tryCatch(
        session$sendCustomMessage("ynowSelfViewMode", list(mode = "")),
        error = function(e) NULL
      )
      return(invisible(FALSE))
    }
    title_key <- switch(
      mode,
      guest = "pts_sysadmin_self_guest",
      signed_in = "pts_sysadmin_self_signed_in",
      omniscient = "pts_sysadmin_self_omniscient",
      "pts_sysadmin_self_active"
    )
    title_fb <- switch(
      mode,
      guest = "Guest mode",
      signed_in = "Signed-in user mode",
      omniscient = "Omniscient mode",
      "Self view active"
    )
    body_key <- switch(
      mode,
      guest = "pts_sysadmin_self_chrome_guest",
      signed_in = "pts_sysadmin_self_chrome_signed_in",
      omniscient = "pts_sysadmin_self_chrome_omniscient",
      "pts_sysadmin_self_chrome_signed_in"
    )
    body_fb <- switch(
      mode,
      guest = "Guest mode: locks and hides match all-user guest restrictions.",
      signed_in = paste0(
        "Signed-in user mode: only all-user member restrictions (no per-user overrides). ",
        "Settings and Account are openable like a signed-in member."
      ),
      omniscient = "Omniscient mode: all feature locks and hides lifted. Title Loading is warning red.",
      ""
    )
    tryCatch(
      session$sendCustomMessage(
        "ynowSelfViewMode",
        list(
          mode = mode,
          exit_label = .lab("pts_sysadmin_self_exit", "Exit self view"),
          label_sign_out = .lab("auth_sign_out", "Sign out"),
          reminder_title = .lab(title_key, title_fb),
          reminder_body = .lab(body_key, body_fb),
          reminder_dismiss = .lab("pts_sysadmin_mode_reminder_dismiss", "Dismiss"),
          reminder_aria = .lab(
            "pts_sysadmin_mode_reminder_aria",
            "Active system admin view mode"
          )
        )
      ),
      error = function(e) NULL
    )
    invisible(TRUE)
  }

  observe({
    mode <- ynow_pts_self_view_mode()
    .ynow_self_view_push_chrome(mode)
  })

  #' Toggle body.ynow-sysadmin-on — hides login/intro chrome when unlocked.
  .ynow_sysadmin_push_mode <- function() {
    on <- isTRUE(tryCatch(ynow_pts_sysadmin_on(), error = function(e) FALSE))
    tryCatch(
      session$sendCustomMessage("ynowSysadminMode", list(on = on)),
      error = function(e) NULL
    )
    invisible(on)
  }

  #' Apply self-view chrome first; ACL / audit / panel refresh after flush.
  #' Matches member login chrome-first (ynow_schedule_auth_heavy).
  #' @param log_ops when FALSE, caller owns the audit row (e.g. sysadmin.enter).
  .ynow_self_view_apply <- function(mode,
                                    notify = TRUE,
                                    goto_home = TRUE,
                                    log_ops = TRUE) {
    mode <- as.character(mode %||% "")[1]
    if (!mode %in% c("guest", "signed_in", "omniscient")) {
      return(invisible(FALSE))
    }
    if (!isTRUE(ynow_pts_sysadmin_on())) {
      showNotification(
        tryCatch(
          ui_str("pts_sysadmin_need_enter", normalize_ui_locale(ui_locale())),
          error = function(e) "Sign in to system admin first."
        ),
        type = "warning"
      )
      return(invisible(FALSE))
    }
    # Mutually exclusive with selected-user view-as.
    ynow_pts_view_as(NULL)
    .ynow_view_as_push_chrome(NULL)
    ynow_pts_self_view_mode(mode)
    .ynow_self_view_push_chrome(mode)
    # Banner for self modes is placeholder / ∞ — no DB.
    tryCatch(.ynow_pts_push_banner(), error = function(e) NULL)
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "en")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    ok_key <- switch(
      mode,
      guest = "pts_sysadmin_self_guest_ok",
      signed_in = "pts_sysadmin_self_signed_in_ok",
      omniscient = "pts_sysadmin_self_omniscient_ok",
      "pts_sysadmin_self_guest_ok"
    )
    ok_fb <- switch(
      mode,
      guest = "Guest mode applied.",
      signed_in = "Signed-in user mode applied (all-user restrictions only).",
      omniscient = "Omniscient mode applied — all locks and hides lifted. Title is warning red.",
      "Self view applied."
    )
    if (isTRUE(notify)) {
      showNotification(.lab(ok_key, ok_fb), type = "message", duration = 4)
    }
    if (isTRUE(goto_home)) {
      tryCatch({
        if (exists("updateTabItems", mode = "function")) {
          updateTabItems(session, "sidebar_tabs", "home")
        }
        session$sendCustomMessage("ynowGotoTab", list(tab = "home"))
      }, error = function(e) NULL)
    }
    mode_for_log <- mode
    do_log <- isTRUE(log_ops)
    session$onFlushed(function() {
      # ACL + audit + panel/user-list tick after chrome (ynow-sysadmin-on / self-view) flushed.
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      if (isTRUE(do_log)) {
        .ynow_admin_ops_log("self_view.apply", paste0("mode=", mode_for_log))
      }
      ynow_pts_sysadmin_tick(isolate(ynow_pts_sysadmin_tick()) + 1L)
      ynow_pts_panel_tick(isolate(ynow_pts_panel_tick()) + 1L)
    }, once = TRUE)
    invisible(TRUE)
  }

  .ynow_self_view_clear <- function(notify = TRUE) {
    had <- !is.null(tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL))
    prev <- as.character(
      tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL) %||% ""
    )[1]
    ynow_pts_self_view_mode(NULL)
    .ynow_self_view_push_chrome(NULL)
    tryCatch(.ynow_pts_push_banner(), error = function(e) NULL)
    if (isTRUE(notify) && isTRUE(had)) {
      showNotification(
        tryCatch(
          ui_str("pts_sysadmin_self_exit_ok", normalize_ui_locale(ui_locale())),
          error = function(e) "Exited self view mode."
        ),
        type = "message",
        duration = 3
      )
    }
    tryCatch({
      if (exists("updateTabItems", mode = "function")) {
        updateTabItems(session, "sidebar_tabs", "testing")
      }
      session$sendCustomMessage("ynowGotoTab", list(tab = "testing"))
    }, error = function(e) NULL)
    prev_for_log <- prev
    had_for_log <- isTRUE(had)
    session$onFlushed(function() {
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      if (isTRUE(had_for_log)) {
        .ynow_admin_ops_log(
          "self_view.exit",
          if (nzchar(prev_for_log)) paste0("mode=", prev_for_log) else "cleared"
        )
      }
      ynow_pts_sysadmin_tick(isolate(ynow_pts_sysadmin_tick()) + 1L)
      ynow_pts_panel_tick(isolate(ynow_pts_panel_tick()) + 1L)
    }, once = TRUE)
    invisible(TRUE)
  }

  #' Self-view signed-in / omniscient act as a logged-in member for Account/Settings.
  .ynow_self_view_is_member_like <- function() {
    mode <- as.character(
      tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL) %||% ""
    )[1]
    identical(mode, "signed_in") || identical(mode, "omniscient")
  }

  #' Preview member for Account / Points when self-view has no email session.
  .ynow_self_view_preview_user <- function() {
    list(
      id = "__self_member__",
      email = "",
      role = "user",
      logged_in_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      self_view = TRUE
    )
  }

  .ynow_account_gate_ui <- function(loc) {
    if (!isTRUE(ynow_auth_enabled())) {
      return(tags$p(style = "color:#888;", tryCatch(
        ui_str("auth_msg_disabled", loc),
        error = function(e) "Account sign-in is not enabled on this deployment."
      )))
    }
    if (isTRUE(tryCatch(lite_mode(), error = function(e) FALSE))) {
      return(tags$p(style = "color:#888;", tryCatch(
        ui_str("auth_admin_lite_blocked", loc),
        error = function(e) "Account pages are Full-mode only. Lite stays fully usable without an account."
      )))
    }
    # Self-view: signed-in / omniscient open Account+Settings like a real member.
    # Guest self-view keeps css_hide empty shell (no Sign-in promo).
    if (isTRUE(.ynow_self_view_is_member_like())) {
      return(NULL)
    }
    mode <- as.character(
      tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL) %||% ""
    )[1]
    if (identical(mode, "guest")) {
      return(tags$div(
        class = "ynow-account-guest-hidden",
        style = "display:none;",
        `aria-hidden` = "true",
        `data-ynow-gate-feature` = "account"
      ))
    }
    u <- tryCatch(ynow_auth$user(), error = function(e) NULL)
    if (is.null(u)) {
      # Guest-hidden workspace (css_hide): hide vs show only — no 推廣登入頁面.
      return(tags$div(
        class = "ynow-account-guest-hidden",
        style = "display:none;",
        `aria-hidden` = "true",
        `data-ynow-gate-feature` = "account"
      ))
    }
    NULL
  }

  .ynow_pts_user_bundle <- function(u, include_ledger = FALSE) {
    uid <- as.character(u$id %||% "")[1]
    bals <- if (nzchar(uid) && exists("ynow_points_balances", mode = "function")) {
      tryCatch(ynow_points_balances(uid), error = function(e) list(fyn = NA_integer_, pyn = NA_integer_))
    } else if (nzchar(uid) && exists("ynow_points_balance", mode = "function")) {
      list(
        fyn = tryCatch(ynow_points_balance(uid), error = function(e) NA_integer_),
        pyn = 0L
      )
    } else {
      list(fyn = NA_integer_, pyn = NA_integer_)
    }
    pts_bal <- suppressWarnings(as.integer(bals$fyn %||% NA_integer_)[1])
    # Header banner only needs balances. Full ledger is for the Points tab.
    pts_led <- NULL
    if (isTRUE(include_ledger) && nzchar(uid)) {
      pts_led <- if (exists("ynow_points_ledger_all", mode = "function")) {
        tryCatch(ynow_points_ledger_all(uid), error = function(e) NULL)
      } else if (exists("ynow_points_ledger_tail", mode = "function")) {
        tryCatch(ynow_points_ledger_tail(uid, n = NULL), error = function(e) NULL)
      } else {
        NULL
      }
    }
    list(
      uid = uid,
      balance = pts_bal,
      fyn = suppressWarnings(as.integer(bals$fyn %||% NA_integer_)[1]),
      pyn = suppressWarnings(as.integer(bals$pyn %||% NA_integer_)[1]),
      ledger = pts_led
    )
  }

  .ynow_pts_push_banner <- function() {
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    pts_colon <- if (identical(loc, "zh-TW")) "：" else ":"
    fyn_lab <- tryCatch(paste0(ui_str("pts_fyn_label", loc), pts_colon), error = function(e) "FYN points:")
    pyn_lab <- tryCatch(paste0(ui_str("pts_pyn_label", loc), pts_colon), error = function(e) "PYN points:")
    # Sys. Admin portal chrome is ops context, not a member wallet → show ∞.
    # View-as of a specific user still shows that user's real FYN / PYN.
    # Self-view: guest hides banner; signed-in shows placeholders; omniscient → ∞.
    sys_on <- isTRUE(tryCatch(isolate(ynow_pts_sysadmin_on()), error = function(e) FALSE))
    va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
    view_as_on <- is.list(va) && nzchar(as.character(va$user_id %||% "")[1])
    self_mode <- as.character(
      tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL) %||% ""
    )[1]
    if (identical(self_mode, "guest")) {
      session$sendCustomMessage(
        "ynowPtsBanner",
        list(fyn = "", pyn = "", fyn_label = fyn_lab, pyn_label = pyn_lab, visible = FALSE)
      )
      return(invisible(NULL))
    }
    if (identical(self_mode, "signed_in")) {
      session$sendCustomMessage(
        "ynowPtsBanner",
        list(
          fyn = "—", pyn = "—",
          fyn_label = fyn_lab, pyn_label = pyn_lab,
          visible = TRUE
        )
      )
      return(invisible(NULL))
    }
    if (identical(self_mode, "omniscient") ||
        (isTRUE(sys_on) && !isTRUE(view_as_on) && !nzchar(self_mode))) {
      session$sendCustomMessage(
        "ynowPtsBanner",
        list(
          fyn = "\u221E", pyn = "\u221E",
          fyn_label = fyn_lab, pyn_label = pyn_lab,
          visible = TRUE
        )
      )
      return(invisible(NULL))
    }
    # View-as prefers target DB balances; otherwise signed-in user.
    u <- .ynow_pts_effective_user()
    if (is.null(u)) {
      # Guests: hide FYN/PYN header entirely (no placeholder "—" next to locale).
      session$sendCustomMessage(
        "ynowPtsBanner",
        list(fyn = "", pyn = "", fyn_label = fyn_lab, pyn_label = pyn_lab, visible = FALSE)
      )
      return(invisible(NULL))
    }
    bund <- .ynow_pts_user_bundle(u)
    fyn_txt <- if (is.finite(bund$fyn)) as.character(bund$fyn) else "—"
    pyn_txt <- if (is.finite(bund$pyn)) as.character(bund$pyn) else "—"
    session$sendCustomMessage(
      "ynowPtsBanner",
      list(
        fyn = fyn_txt, pyn = pyn_txt,
        fyn_label = fyn_lab, pyn_label = pyn_lab,
        visible = TRUE
      )
    )
    invisible(NULL)
  }

  #' Push remaining free PDF downloads + points rules near download chrome.
  #' Lite stays ungated; Full guests see policy; signed-in users see live left count.
  .ynow_pdf_quota_push <- function() {
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    lite <- isTRUE(tryCatch(isolate(input$ynow_lite_mode), error = function(e) FALSE)) ||
      isTRUE(tryCatch(isolate(lite_mode()), error = function(e) FALSE))
    limit <- if (exists("ynow_pdf_free_per_day", mode = "function")) {
      as.integer(ynow_pdf_free_per_day())[1]
    } else {
      3L
    }
    if (!is.finite(limit) || limit < 0L) limit <- 3L
    cost <- if (exists("ynow_points_sku_cost", mode = "function")) {
      suppressWarnings(as.integer(ynow_points_sku_cost("pdf_report"))[1])
    } else {
      10L
    }
    if (!is.finite(cost) || cost < 0L) cost <- 10L
    rules <- sprintf(
      .lab(
        "pdf_quota_rules_fmt",
        "Points rule: after the free quota, each new download costs %s FYN. Re-downloading the same ticker and settings is free."
      ),
      as.character(cost)
    )
    if (isTRUE(lite)) {
      session$sendCustomMessage(
        "ynowPdfQuota",
        list(
          state = "lite",
          remaining = .lab("pdf_quota_hint_lite", "Lite: free PDF downloads. Enter a ticker first."),
          rules = "",
          snap = .lab("snapshot_download_quota_hint_lite", "Lite: Snapshot downloads stay free."),
          left = NA_integer_,
          limit = limit,
          cost = cost
        )
      )
      return(invisible(NULL))
    }
    uid <- tryCatch(.ynow_access_subject_id(), error = function(e) "")
    uid <- as.character(uid %||% "")[1]
    if (!nzchar(uid)) {
      rem <- sprintf(
        .lab(
          "pdf_quota_guest_remaining_fmt",
          "Sign-in required. After sign-in: %s free PDF downloads each Asia/Taipei day."
        ),
        as.character(limit)
      )
      session$sendCustomMessage(
        "ynowPdfQuota",
        list(
          state = "guest",
          remaining = rem,
          rules = rules,
          snap = .lab(
            "snapshot_download_quota_hint",
            "Sign-in required. Snapshot / restore CSV downloads are free after sign-in (no FYN)."
          ),
          left = limit,
          limit = limit,
          cost = cost
        )
      )
      return(invisible(NULL))
    }
    st <- if (exists("ynow_pdf_quota_status", mode = "function")) {
      tryCatch(ynow_pdf_quota_status(uid), error = function(e) NULL)
    } else {
      NULL
    }
    left <- if (is.list(st) && is.finite(st$left)) as.integer(st$left)[1] else limit
    lim <- if (is.list(st) && is.finite(st$limit)) as.integer(st$limit)[1] else limit
    if (!is.finite(left)) left <- lim
    rem <- if (left <= 0L) {
      sprintf(
        .lab(
          "pdf_quota_exhausted_fmt",
          "No free PDF downloads left today. Next new download costs %s FYN."
        ),
        as.character(cost)
      )
    } else {
      sprintf(
        .lab(
          "pdf_quota_remaining_fmt",
          "Free PDF downloads left today: %s of %s (Asia/Taipei)."
        ),
        as.character(left),
        as.character(lim)
      )
    }
    session$sendCustomMessage(
      "ynowPdfQuota",
      list(
        state = "authed",
        remaining = rem,
        rules = rules,
        snap = .lab(
          "snapshot_download_quota_hint_authed",
          "Snapshot / restore CSV downloads are free (no FYN)."
        ),
        left = left,
        limit = lim,
        cost = cost
      )
    )
    invisible(NULL)
  }

  # Account overview (Full + signed-in). Lite never requires login.
  output$ynow_admin_panel <- renderUI({
    ynow_pts_panel_tick()
    tryCatch(ynow_pts_self_view_mode(), error = function(e) NULL)
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    gated <- .ynow_account_gate_ui(loc)
    if (!is.null(gated)) return(gated)
    u <- if (isTRUE(tryCatch(.ynow_self_view_is_member_like(), error = function(e) FALSE))) {
      .ynow_self_view_preview_user()
    } else {
      tryCatch(ynow_auth$user(), error = function(e) NULL)
    }
    build_ver <- tryCatch(
      as.character(.YNOW_BUILD_VERSION %||% "")[1],
      error = function(e) ""
    )
    bund <- .ynow_pts_user_bundle(u)
    if (exists("ynow_admin_panel_ui", mode = "function")) {
      ynow_admin_panel_ui(
        u,
        locale = loc,
        build_version = build_ver,
        points_balance = bund$balance,
        fyn_balance = bund$fyn,
        pyn_balance = bund$pyn,
        points_ledger = NULL
      )
    } else {
      tags$p(u$email %||% "")
    }
  })

  # Points ledger tab — full history for the signed-in user.
  output$ynow_points_panel <- renderUI({
    ynow_pts_panel_tick()
    tryCatch(ynow_pts_self_view_mode(), error = function(e) NULL)
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    gated <- .ynow_account_gate_ui(loc)
    if (!is.null(gated)) return(gated)
    u <- if (isTRUE(tryCatch(.ynow_self_view_is_member_like(), error = function(e) FALSE))) {
      .ynow_self_view_preview_user()
    } else {
      tryCatch(ynow_auth$user(), error = function(e) NULL)
    }
    bund <- .ynow_pts_user_bundle(u, include_ledger = TRUE)
    if (exists("ynow_points_panel_ui", mode = "function")) {
      ynow_points_panel_ui(
        u,
        locale = loc,
        points_balance = bund$balance,
        fyn_balance = bund$fyn,
        pyn_balance = bund$pyn,
        points_ledger = bund$ledger
      )
    } else {
      tags$p(tryCatch(ui_str("pts_ledger_title", loc), error = function(e) "Points ledger"))
    }
  })

  .ynow_pts_ledger_display_df <- function() {
    ynow_pts_panel_tick()
    u <- tryCatch(ynow_auth$user(), error = function(e) NULL)
    if (is.null(u)) {
      return(data.frame(
        created_at = character(), delta = integer(), balance_after = integer(),
        wallet = character(), reason = character(), sku = character(),
        ref_id = character(), note = character(), stringsAsFactors = FALSE
      ))
    }
    bund <- .ynow_pts_user_bundle(u, include_ledger = TRUE)
    df <- bund$ledger
    if (!is.data.frame(df) || nrow(df) < 1L) {
      return(data.frame(
        created_at = character(), delta = integer(), balance_after = integer(),
        wallet = character(), reason = character(), sku = character(),
        ref_id = character(), note = character(), stringsAsFactors = FALSE
      ))
    }
    cols <- intersect(
      c("created_at", "delta", "balance_after", "wallet", "reason", "sku", "ref_id", "note"),
      names(df)
    )
    df[, cols, drop = FALSE]
  }

  if (requireNamespace("DT", quietly = TRUE)) {
    output$ynow_pts_ledger_dt <- DT::renderDT({
      loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
      .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
      df <- .ynow_pts_ledger_display_df()
      if (!nrow(df)) {
        return(DT::datatable(
          data.frame(Message = .lab("pts_ledger_empty", "No points activity yet.")),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        ))
      }
      names(df) <- vapply(names(df), function(nm) {
        switch(
          nm,
          created_at = .lab("pts_col_when", "When"),
          delta = .lab("pts_col_delta", "Delta"),
          balance_after = .lab("pts_col_balance", "Balance"),
          wallet = .lab("pts_col_wallet", "Wallet"),
          reason = .lab("pts_col_reason", "Reason"),
          sku = .lab("pts_col_sku", "SKU"),
          ref_id = .lab("pts_col_ref", "Reference"),
          note = .lab("pts_col_note", "Note"),
          nm
        )
      }, character(1))
      DT::datatable(
        df,
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
          order = list(list(0, "desc")),
          scrollX = TRUE,
          deferRender = TRUE
        )
      )
    }, server = TRUE)
  } else {
    output$ynow_pts_ledger_fallback <- renderUI({
      loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
      .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
      df <- .ynow_pts_ledger_display_df()
      if (!nrow(df)) {
        return(tags$p(class = "text-muted", .lab("pts_ledger_empty", "No points activity yet.")))
      }
      tags$div(
        style = "overflow-x:auto; max-height:70vh;",
        tags$table(
          class = "table table-condensed table-striped",
          tags$thead(tags$tr(
            tags$th(.lab("pts_col_when", "When")),
            tags$th(.lab("pts_col_delta", "Delta")),
            tags$th(.lab("pts_col_balance", "Balance")),
            tags$th(.lab("pts_col_wallet", "Wallet")),
            tags$th(.lab("pts_col_reason", "Reason")),
            tags$th(.lab("pts_col_sku", "SKU")),
            tags$th(.lab("pts_col_ref", "Reference")),
            tags$th(.lab("pts_col_note", "Note"))
          )),
          tags$tbody(lapply(seq_len(nrow(df)), function(i) {
            tags$tr(
              tags$td(as.character(df$created_at[[i]] %||% "")),
              tags$td(as.character(df$delta[[i]] %||% "")),
              tags$td(as.character(df$balance_after[[i]] %||% "")),
              tags$td(as.character(df$wallet[[i]] %||% "")),
              tags$td(as.character(df$reason[[i]] %||% "")),
              tags$td(as.character(df$sku[[i]] %||% "")),
              tags$td(as.character(df$ref_id[[i]] %||% "")),
              tags$td(as.character(df$note[[i]] %||% ""))
            )
          }))
        )
      )
    })
  }

  # Header FYN / PYN banner (right of locale switch) + PDF/Snapshot download quota chrome.
  observe({
    ynow_pts_panel_tick()
    ynow_pts_view_as()
    ynow_pts_sysadmin_on()
    logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
    tryCatch(ynow_auth$user(), error = function(e) NULL)
    tryCatch(ui_locale(), error = function(e) NULL)
    tryCatch(input$ynow_lite_mode, error = function(e) NULL)
    tryCatch(lite_mode(), error = function(e) NULL)
    heavy <- ynow_auth_heavy_tick()
    if (isTRUE(logged) && as.integer(heavy %||% 0L)[1] < 1L) {
      return(invisible(NULL))
    }
    .ynow_pts_push_banner()
    tryCatch(.ynow_pdf_quota_push(), error = function(e) NULL)
  })

  # System-admin portal — username `ynow` + Asia/Taipei YYYYMMDD password.
  observeEvent(input$ynow_pts_sysadmin_enter, {
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    user_in <- as.character(input$ynow_pts_sysadmin_login_user %||% "")[1]
    pass_in <- as.character(input$ynow_pts_sysadmin_login_pass %||% "")[1]
    # Clear password field immediately; never log credentials.
    tryCatch(
      updateTextInput(session, "ynow_pts_sysadmin_login_pass", value = ""),
      error = function(e) NULL
    )
    ok <- FALSE
    if (exists("ynow_sysadmin_validate_login", mode = "function")) {
      res <- tryCatch(
        ynow_sysadmin_validate_login(user_in, pass_in),
        error = function(e) list(ok = FALSE)
      )
      ok <- isTRUE(res$ok)
    }
    # Drop local copies; do not include password in any message.
    pass_in <- NULL
    if (!isTRUE(ok)) {
      showNotification(
        .lab("pts_sysadmin_login_bad", "Invalid system-admin username or password."),
        type = "error",
        duration = 5
      )
      return()
    }
    actor_name <- tolower(trimws(as.character(user_in %||% "")[1]))
    if (!nzchar(actor_name)) actor_name <- "ynow"
    ynow_pts_sysadmin_actor(actor_name)
    ynow_pts_sysadmin_on(TRUE)
    # Enter = exited self-view: no guest / signed-in / omniscient overlay, no Exit chrome.
    ynow_pts_view_as(NULL)
    ynow_pts_self_view_mode(NULL)
    .ynow_sysadmin_push_mode()
    .ynow_view_as_push_chrome(NULL)
    .ynow_self_view_push_chrome(NULL)
    tryCatch({
      if (exists("updateTabItems", mode = "function")) {
        updateTabItems(session, "sidebar_tabs", "testing")
      }
      session$sendCustomMessage("ynowGotoTab", list(tab = "testing"))
    }, error = function(e) NULL)
    actor_for_log <- actor_name
    session$onFlushed(function() {
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      .ynow_admin_ops_log(
        "sysadmin.enter",
        paste0("actor=", actor_for_log, "; self_view=none")
      )
      ynow_pts_sysadmin_tick(isolate(ynow_pts_sysadmin_tick()) + 1L)
      ynow_pts_panel_tick(isolate(ynow_pts_panel_tick()) + 1L)
    }, once = TRUE)
    showNotification(
      .lab("pts_sysadmin_login_ok", "System admin unlocked."),
      type = "message",
      duration = 3
    )
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_pts_sysadmin_exit, {
    .ynow_admin_ops_log("sysadmin.exit", "portal locked")
    ynow_pts_sysadmin_on(FALSE)
    ynow_pts_sysadmin_actor("")
    ynow_pts_view_as(NULL)
    ynow_pts_self_view_mode(NULL)
    .ynow_sysadmin_push_mode()
    .ynow_view_as_push_chrome(NULL)
    .ynow_self_view_push_chrome(NULL)
    tryCatch(.ynow_pts_push_banner(), error = function(e) NULL)
    ynow_pts_sysadmin_tick(isolate(ynow_pts_sysadmin_tick()) + 1L)
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_pts_sysadmin_self_guest, {
    .ynow_self_view_apply("guest")
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_pts_sysadmin_self_signed_in, {
    .ynow_self_view_apply("signed_in")
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_pts_sysadmin_self_omniscient, {
    .ynow_self_view_apply("omniscient")
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_pts_sysadmin_self_exit, {
    .ynow_self_view_clear(notify = TRUE)
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_pts_sysadmin_viewas, {
    if (!isTRUE(ynow_pts_sysadmin_on())) {
      showNotification(
        tryCatch(
          ui_str("pts_sysadmin_need_enter", normalize_ui_locale(ui_locale())),
          error = function(e) "Sign in to system admin first."
        ),
        type = "warning"
      )
      return()
    }
    # Mutually exclusive with self view modes.
    ynow_pts_self_view_mode(NULL)
    .ynow_self_view_push_chrome(NULL)
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    # selectInput value is internal user_id; label (names) is opaque public_id.
    # Race: freshly rendered select may not have reported yet — fall back to first listed user.
    uid <- as.character(input$ynow_pts_sysadmin_user %||% "")[1]
    pid <- ""
    role <- "user"
    if (!nzchar(uid) && exists("ynow_points_admin_list_users", mode = "function")) {
      df0 <- tryCatch(ynow_points_admin_list_users(), error = function(e) NULL)
      if (is.data.frame(df0) && nrow(df0) > 0L) {
        uid <- as.character(df0$user_id[[1]] %||% "")[1]
        pid <- as.character(df0$public_id[[1]] %||% "")[1]
        if ("role" %in% names(df0)) role <- as.character(df0$role[[1]] %||% "user")[1]
      }
    }
    if (nzchar(uid) && !nzchar(pid) && exists("ynow_points_public_id", mode = "function")) {
      pid <- tryCatch(ynow_points_public_id(uid), error = function(e) "")
    }
    if (!nzchar(uid)) {
      showNotification(.lab("pts_sysadmin_bad_user", "Select a user from the list."), type = "warning")
      return()
    }
    if (!nzchar(pid)) pid <- uid
    va <- list(public_id = pid, user_id = uid, role = role)
    ynow_pts_view_as(va)
    # Chrome/silver first (no DB); banner may hit Postgres and must not block the signal.
    .ynow_view_as_push_chrome(va)
    .ynow_admin_ops_log(
      "view_as.enter",
      paste0("public_id=", pid, "; user_id=", uid)
    )
    showNotification(
      sprintf(
        .lab("pts_sysadmin_viewas_ok_fmt", "Viewing as %s (de-identified)."),
        pid
      ),
      type = "message",
      duration = 4
    )
    tryCatch(.ynow_pts_push_banner(), error = function(e) NULL)
    # Navigate after chrome so silver Loading is visible on Home.
    tryCatch({
      if (exists("updateTabItems", mode = "function")) {
        updateTabItems(session, "sidebar_tabs", "home")
      }
      session$sendCustomMessage("ynowGotoTab", list(tab = "home"))
    }, error = function(e) NULL)
    # Refresh panel last (avoid interrupting view-as side effects).
    ynow_pts_sysadmin_tick(isolate(ynow_pts_sysadmin_tick()) + 1L)
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_pts_sysadmin_viewas_exit, {
    va_prev <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
    pid_prev <- if (is.list(va_prev)) as.character(va_prev$public_id %||% "")[1] else ""
    ynow_pts_view_as(NULL)
    .ynow_view_as_push_chrome(NULL)
    .ynow_admin_ops_log(
      "view_as.exit",
      if (nzchar(pid_prev)) paste0("public_id=", pid_prev) else "cleared"
    )
    tryCatch(.ynow_pts_push_banner(), error = function(e) NULL)
    # Return to the system-admin portal after exiting view-as.
    tryCatch({
      if (exists("updateTabItems", mode = "function")) {
        updateTabItems(session, "sidebar_tabs", "testing")
      }
      session$sendCustomMessage("ynowGotoTab", list(tab = "testing"))
    }, error = function(e) NULL)
    ynow_pts_sysadmin_tick(isolate(ynow_pts_sysadmin_tick()) + 1L)
  }, ignoreInit = TRUE)

  # Sign out during view-as / self-view = exit preview (ops admin is not email-authed).
  .ynow_view_as_exit_from_signout <- function() {
    va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
    self_mode <- tryCatch(isolate(ynow_pts_self_view_mode()), error = function(e) NULL)
    cleared <- FALSE
    if (is.list(va) && nzchar(as.character(va$user_id %||% "")[1])) {
      ynow_pts_view_as(NULL)
      .ynow_view_as_push_chrome(NULL)
      cleared <- TRUE
    }
    if (!is.null(self_mode) && nzchar(as.character(self_mode %||% "")[1])) {
      ynow_pts_self_view_mode(NULL)
      .ynow_self_view_push_chrome(NULL)
      tryCatch(.ynow_push_access_locks(), error = function(e) NULL)
      cleared <- TRUE
    }
    if (!isTRUE(cleared)) return(invisible(FALSE))
    tryCatch(.ynow_pts_push_banner(), error = function(e) NULL)
    tryCatch({
      if (exists("updateTabItems", mode = "function")) {
        updateTabItems(session, "sidebar_tabs", "testing")
      }
      session$sendCustomMessage("ynowGotoTab", list(tab = "testing"))
    }, error = function(e) NULL)
    ynow_pts_sysadmin_tick(isolate(ynow_pts_sysadmin_tick()) + 1L)
    invisible(TRUE)
  }
  observeEvent(input$ynow_auth_sign_out, {
    .ynow_view_as_exit_from_signout()
  }, ignoreInit = TRUE, priority = 50)
  observeEvent(input$ynow_auth_sign_out_date, {
    .ynow_view_as_exit_from_signout()
  }, ignoreInit = TRUE, priority = 50)
  observeEvent(input$ynow_auth_sign_out_home, {
    .ynow_view_as_exit_from_signout()
  }, ignoreInit = TRUE, priority = 50)

  # Load user list off the reactive flush so Exit / Switch view stay responsive.
  observeEvent(ynow_pts_sysadmin_tick(), {
    if (!isTRUE(isolate(ynow_pts_sysadmin_on()))) {
      ynow_pts_sysadmin_panel_state(list(kind = "locked"))
      return()
    }
    load_id <- isolate(ynow_pts_sysadmin_tick())
    ynow_pts_sysadmin_panel_state(list(kind = "loading", load_id = load_id))
    fetch_users <- function() {
      if (!isTRUE(isolate(ynow_pts_sysadmin_on()))) return()
      if (!identical(isolate(ynow_pts_sysadmin_tick()), load_id)) return()
      backend <- tryCatch(ynow_points_backend(), error = function(e) "memory")
      backend_label <- tryCatch({
        if (exists("ynow_points_backend_label", mode = "function")) {
          ynow_points_backend_label()
        } else {
          as.character(backend)
        }
      }, error = function(e) as.character(backend))
      uses_db <- tryCatch({
        if (exists("ynow_points_uses_db", mode = "function")) ynow_points_uses_db()
        else identical(backend, "postgres")
      }, error = function(e) FALSE)
      db_ready <- exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready())
      df <- if (exists("ynow_points_admin_list_users", mode = "function")) {
        tryCatch(ynow_points_admin_list_users(), error = function(e) NULL)
      } else {
        NULL
      }
      if (!isTRUE(isolate(ynow_pts_sysadmin_on()))) return()
      if (!identical(isolate(ynow_pts_sysadmin_tick()), load_id)) return()
      ynow_pts_sysadmin_panel_state(list(
        kind = "ready",
        load_id = load_id,
        backend_label = backend_label,
        uses_db = uses_db,
        db_ready = db_ready,
        df = df,
        has_list_fn = exists("ynow_points_admin_list_users", mode = "function")
      ))
    }
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(fetch_users, delay = 0)
    } else {
      fetch_users()
    }
  }, ignoreNULL = FALSE)

  output$ynow_pts_sysadmin_panel <- renderUI({
    st <- ynow_pts_sysadmin_panel_state()
    loc <- tryCatch(
      normalize_ui_locale(isolate(ui_locale())),
      error = function(e) "zh-TW"
    )
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    kind <- as.character(st$kind %||% "locked")[1]
    if (identical(kind, "locked")) {
      return(tags$div(
        tags$hr(),
        tags$p(
          class = "text-muted",
          id = "ynow_pts_sysadmin_need_enter",
          .lab(
            "pts_sysadmin_need_enter",
            "Sign in with the system-admin username and password to load the de-identified user list."
          )
        )
      ))
    }
    if (identical(kind, "loading")) {
      return(tags$div(
        tags$p(
          class = "text-muted",
          id = "ynow_pts_sysadmin_loading",
          style = "margin-top:8px;",
          .lab("pts_sysadmin_loading", "Loading de-identified users…")
        )
      ))
    }
    if (!isTRUE(st$has_list_fn)) {
      return(tags$p(class = "text-danger", .lab("pts_msg_backend", "Points backend is unavailable.")))
    }
    backend_label <- as.character(st$backend_label %||% "—")[1]
    df <- st$df
    if (!is.data.frame(df) || nrow(df) < 1L) {
      empty_key <- if (!isTRUE(st$db_ready) || !isTRUE(st$uses_db)) {
        "pts_sysadmin_no_db"
      } else {
        "pts_sysadmin_empty"
      }
      empty_msg <- if (identical(empty_key, "pts_sysadmin_no_db")) {
        .lab(
          "pts_sysadmin_no_db",
          .lab(
            "pts_sysadmin_no_postgres",
            "Account database is not configured on this deployment (missing YNOW_DB_* / YNOW_DATABASE_URL, or RPostgres/pool not installed). Testing admin reads the same ynow_users table as sign-in — without it the list stays empty."
          )
        )
      } else {
        .lab("pts_sysadmin_empty", "No users in the database yet.")
      }
      empty_controls <- tags$div(
        class = "ynow-sysadmin-controls-pane",
        tags$p(
          class = "text-muted",
          style = "font-size:12.5px; margin-top:4px;",
          tags$span(
            id = "ynow_pts_sysadmin_backend",
            sprintf(
              .lab("pts_sysadmin_backend_fmt", "Points backend: %s"),
              backend_label
            )
          )
        ),
        tags$p(class = "text-warning", empty_msg)
      )
      return(tags$div(
        class = "ynow-sysadmin-ops-tabs",
        tabsetPanel(
          id = "ynow_pts_sysadmin_tabs",
          type = "tabs",
          tabPanel(
            title = tags$span(
              id = "ynow_admin_ops_controls_tab_label",
              .lab("admin_ops_controls_tab", "Controls")
            ),
            value = "controls",
            empty_controls
          ),
          tabPanel(
            title = tags$span(
              id = "ynow_admin_ops_tab_label",
              .lab("admin_ops_tab", "Audit log")
            ),
            value = "audit",
            tags$div(
              class = "ynow-admin-ops-audit-pane",
              tags$h4(
                tags$span(id = "ynow_admin_ops_title", .lab("admin_ops_title", "Audit log"))
              ),
              tags$p(
                class = "text-muted",
                style = "font-size:12.5px;",
                tags$span(
                  id = "ynow_admin_ops_help",
                  .lab(
                    "admin_ops_help",
                    paste0(
                      "Append-only record of system-admin operations (newest first). ",
                      "Each entry is kept for at most 24 hours (server clock) and then expires automatically. ",
                      "There is no clear or delete control."
                    )
                  )
                )
              ),
              uiOutput("ynow_admin_ops_audit_list")
            )
          )
        )
      ))
    }
    status_ok <- .lab("pts_sysadmin_status_ok", "Active")
    status_off <- .lab("pts_sysadmin_status_disabled", "Disabled")
    choices <- stats::setNames(as.character(df$user_id), as.character(df$public_id))
    va <- tryCatch(isolate(ynow_pts_view_as()), error = function(e) NULL)
    va_uid <- if (is.list(va)) as.character(va$user_id %||% "")[1] else ""
    cur_uid <- as.character(isolate(input$ynow_pts_sysadmin_user) %||% "")[1]
    sel <- if (nzchar(va_uid) && va_uid %in% as.character(choices)) {
      va_uid
    } else if (nzchar(cur_uid) && cur_uid %in% as.character(choices)) {
      cur_uid
    } else {
      as.character(choices[[1]])
    }
    cur_scope <- as.character(isolate(input$ynow_access_scope) %||% "user")[1]
    if (!cur_scope %in% c("user", "all")) cur_scope <- "user"
    cur_action <- as.character(isolate(input$ynow_access_action) %||% "apply")[1]
    if (!cur_action %in% c("apply", "remove")) cur_action <- "apply"
    cur_effect <- as.character(isolate(input$ynow_access_effect) %||% "lock")[1]
    if (!cur_effect %in% c("lock", "hide")) cur_effect <- "lock"
    cur_unlock <- as.character(isolate(input$ynow_access_unlock_type) %||% "login")[1]
    if (!cur_unlock %in% c("login", "points", "conditional")) cur_unlock <- "login"
    cur_points <- suppressWarnings(as.integer(isolate(input$ynow_access_points_cost) %||% 1L)[1])
    if (!is.finite(cur_points) || cur_points < 1L) cur_points <- 1L
    cur_free <- suppressWarnings(as.integer(isolate(input$ynow_access_free_count) %||% 0L)[1])
    if (!is.finite(cur_free) || cur_free < 0L) cur_free <- 0L
    cur_hours <- suppressWarnings(as.integer(isolate(input$ynow_access_time_hours) %||% 0L)[1])
    if (!is.finite(cur_hours) || cur_hours < 0L) cur_hours <- 0L
    controls_pane <- tags$div(
      class = "ynow-sysadmin-controls-pane",
      tags$p(
        class = "text-muted",
        style = "font-size:12.5px; margin-top:4px;",
        tags$span(
          id = "ynow_pts_sysadmin_backend",
          sprintf(
            .lab("pts_sysadmin_backend_fmt", "Points backend: %s"),
            backend_label
          )
        )
      ),
      tags$div(
        class = "ynow-sysadmin-users-title-row",
        style = "display:flex; align-items:center; justify-content:space-between; gap:12px; flex-wrap:wrap; margin:8px 0 6px;",
        tags$h4(
          style = "margin:0;",
          .lab("pts_sysadmin_users", "Users (de-identified)")
        ),
        tags$button(
          id = "ynow_pts_sysadmin_user_search_btn",
          type = "button",
          class = "btn btn-default btn-sm",
          .lab("pts_sysadmin_user_search", "Search users")
        )
      ),
      tags$div(
        style = "overflow-x:auto; max-height:40vh; margin-bottom:12px;",
        tags$table(
          class = "table table-condensed table-striped",
          id = "ynow_pts_sysadmin_users_table",
          tags$thead(tags$tr(
            tags$th(
              style = "min-width:9em; vertical-align:bottom;",
              tags$div(
                style = "font-weight:bold; margin-bottom:4px;",
                .lab("pts_sysadmin_col_id", "User id")
              ),
              tags$input(
                id = "ynow_pts_sysadmin_user_search",
                type = "search",
                class = "form-control input-sm",
                value = "",
                placeholder = .lab("pts_sysadmin_user_search_ph", "Filter by user id…"),
                `aria-label` = .lab("pts_sysadmin_col_id", "User id"),
                autocomplete = "off"
              )
            ),
            tags$th(.lab("pts_sysadmin_col_fyn", "FYN")),
            tags$th(.lab("pts_sysadmin_col_pyn", "PYN")),
            tags$th(.lab("pts_sysadmin_col_role", "Role")),
            tags$th(.lab("pts_sysadmin_col_status", "Status"))
          )),
          tags$tbody(
            id = "ynow_pts_sysadmin_users_tbody",
            lapply(seq_len(nrow(df)), function(i) {
              tags$tr(
                `data-ynow-uid` = as.character(df$user_id[[i]]),
                `data-ynow-search` = tolower(as.character(df$public_id[[i]])),
                tags$td(tags$code(as.character(df$public_id[[i]]))),
                tags$td(as.character(df$fyn[[i]])),
                tags$td(as.character(df$pyn[[i]])),
                tags$td(as.character(df$role[[i]] %||% "user")),
                tags$td(if (isTRUE(df$disabled[[i]])) status_off else status_ok)
              )
            })
          )
        )
      ),
      tags$script(HTML(paste(
        "(function(){",
        "function ynowFilterSysadminUsers(){",
        "  var inp=document.getElementById('ynow_pts_sysadmin_user_search');",
        "  var q=((inp&&inp.value)||'').toLowerCase().trim();",
        "  var rows=document.querySelectorAll('#ynow_pts_sysadmin_users_tbody tr');",
        "  rows.forEach(function(tr){",
        "    var hay=(tr.getAttribute('data-ynow-search')||'').toLowerCase();",
        "    tr.style.display=(!q||hay.indexOf(q)>=0)?'':'none';",
        "  });",
        "}",
        "var inp=document.getElementById('ynow_pts_sysadmin_user_search');",
        "if(inp&&!inp.__ynowBound){inp.__ynowBound=1;inp.addEventListener('input',ynowFilterSysadminUsers);}",
        "var btn=document.getElementById('ynow_pts_sysadmin_user_search_btn');",
        "if(btn&&!btn.__ynowBound){btn.__ynowBound=1;btn.addEventListener('click',function(e){",
        "  e.preventDefault();",
        "  var target=document.getElementById('ynow_pts_sysadmin_user_search');",
        "  if(target){target.focus(); if(target.select) target.select();}",
        "});}",
        "ynowFilterSysadminUsers();",
        "})();"
      , collapse = "\n"))),
      fluidRow(
        column(
          4,
          selectInput(
            "ynow_pts_sysadmin_user",
            .lab("pts_sysadmin_select", "Select user id"),
            choices = choices,
            selected = sel
          )
        ),
        column(
          2,
          numericInput(
            "ynow_pts_sysadmin_fyn_delta",
            .lab("pts_sysadmin_fyn_delta", "FYN change (+/−)"),
            value = 0, step = 1
          )
        ),
        column(
          2,
          numericInput(
            "ynow_pts_sysadmin_pyn_delta",
            .lab("pts_sysadmin_pyn_delta", "PYN change (+/−)"),
            value = 0, step = 1
          )
        ),
        column(
          4,
          textInput(
            "ynow_pts_sysadmin_note",
            .lab("pts_sysadmin_note", "Note (optional)"),
            value = ""
          )
        )
      ),
      tags$div(
        style = "margin-top:4px; display:flex; flex-wrap:wrap; gap:8px; align-items:center;",
        actionButton(
          "ynow_pts_sysadmin_apply",
          label = tags$span(
            id = "ynow_pts_sysadmin_apply_label",
            .lab("pts_sysadmin_apply", "Apply change")
          ),
          class = "btn-danger ynow-sysadmin-btn-apply"
        )
      ),
      tags$hr(),
      tags$h4(.lab("access_lock_title", "Item locks")),
      tags$p(
        class = "text-muted",
        style = "font-size:12.5px;",
        .lab(
          "access_lock_help",
          paste0(
            "Choose the apply target first, then Apply restriction or Remove restriction. ",
            "Apply only lists items still restrictable for that target; already-restricted ",
            "items are shown read-only. Remove only lists items already restricted. ",
            "Selected user(s) keeps the user list with keyword search and multi-select. ",
            "Per-user rules override all-users rules for the same item. ",
            "All-users also applies to guests."
          )
        )
      ),
      radioButtons(
        "ynow_access_scope",
        .lab("access_scope_label", "Apply to"),
        choiceNames = list(
          .lab("access_scope_user", "Selected user(s)"),
          .lab("access_scope_all", "All users")
        ),
        choiceValues = c("user", "all"),
        selected = cur_scope,
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.ynow_access_scope === 'user'",
        {
          cur_access_users <- as.character(isolate(input$ynow_access_users) %||% character())
          cur_access_users <- cur_access_users[nzchar(cur_access_users) & cur_access_users %in% as.character(choices)]
          if (!length(cur_access_users) && nzchar(sel) && sel %in% as.character(choices)) {
            cur_access_users <- sel
          }
          selectizeInput(
            "ynow_access_users",
            .lab("access_users_multi", "Selected users (multi-select)"),
            choices = choices,
            selected = cur_access_users,
            multiple = TRUE,
            options = list(
              plugins = list("remove_button"),
              placeholder = .lab("access_users_multi_ph", "Search and select one or more users…")
            )
          )
        }
      ),
      # Live: currently restricted under the selected apply-target (always visible).
      uiOutput("ynow_access_locked_now"),
      radioButtons(
        "ynow_access_action",
        .lab("access_action_label", "Action"),
        choiceNames = list(
          .lab("access_action_apply", "Apply restriction"),
          .lab("access_action_remove", "Remove restriction")
        ),
        choiceValues = c("apply", "remove"),
        selected = cur_action,
        inline = TRUE
      ),
      uiOutput("ynow_access_action_fields"),
      tags$div(
        style = "margin-top:8px; display:flex; flex-wrap:wrap; gap:8px; align-items:center;",
        actionButton(
          "ynow_access_apply_changes",
          .lab("access_apply_changes_btn", "Apply changes"),
          class = "btn-danger ynow-sysadmin-btn-apply"
        )
      ),
      tags$hr(),
      tags$h4(.lab("maint_title", "Maintenance")),
      tags$p(
        class = "text-muted",
        style = "font-size:12.5px;",
        .lab(
          "maint_help",
          paste0(
            "Put a page, tab, or element into maintenance for all users during a start–end window. ",
            "While active, content is unusable and the menu item cannot be clicked ",
            "(unlike Item locks). When the end time is reached, access restores automatically."
          )
        )
      ),
      uiOutput("ynow_maint_status_now"),
      uiOutput("ynow_maint_fields"),
      tags$div(
        style = "margin-top:8px; display:flex; flex-wrap:wrap; gap:8px; align-items:center;",
        actionButton(
          "ynow_maint_apply",
          .lab("maint_apply_btn", "Set maintenance"),
          class = "btn-warning"
        ),
        actionButton(
          "ynow_maint_clear",
          .lab("maint_clear_btn", "Clear maintenance"),
          class = "btn-default"
        )
      )
    )
    audit_pane <- tags$div(
      class = "ynow-admin-ops-audit-pane",
      tags$h4(
        tags$span(id = "ynow_admin_ops_title", .lab("admin_ops_title", "Audit log"))
      ),
      tags$p(
        class = "text-muted",
        style = "font-size:12.5px;",
        tags$span(
          id = "ynow_admin_ops_help",
          .lab(
            "admin_ops_help",
            paste0(
              "Append-only record of system-admin operations (newest first). ",
              "Each entry is kept for at most 24 hours (server clock) and then expires automatically. ",
              "There is no clear or delete control."
            )
          )
        )
      ),
      uiOutput("ynow_admin_ops_audit_list")
    )
    tags$div(
      class = "ynow-sysadmin-ops-tabs",
      tabsetPanel(
        id = "ynow_pts_sysadmin_tabs",
        type = "tabs",
        tabPanel(
          title = tags$span(
            id = "ynow_admin_ops_controls_tab_label",
            .lab("admin_ops_controls_tab", "Controls")
          ),
          value = "controls",
          controls_pane
        ),
        tabPanel(
          title = tags$span(
            id = "ynow_admin_ops_tab_label",
            .lab("admin_ops_tab", "Audit log")
          ),
          value = "audit",
          audit_pane
        )
      )
    )
  })

  output$ynow_admin_ops_audit_list <- renderUI({
    ynow_admin_ops_display_tick()
    ynow_pts_sysadmin_tick()
    loc <- tryCatch(
      normalize_ui_locale(isolate(ui_locale())),
      error = function(e) "zh-TW"
    )
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    if (!isTRUE(isolate(ynow_pts_sysadmin_on()))) {
      return(tags$p(
        class = "text-muted",
        .lab("admin_ops_empty", "No operations recorded in the last 24 hours.")
      ))
    }
    if (!exists("ynow_admin_ops_list", mode = "function")) {
      return(tags$p(
        class = "text-warning",
        .lab("admin_ops_unavailable", "Audit log store is unavailable.")
      ))
    }
    df <- tryCatch(ynow_admin_ops_list(), error = function(e) NULL)
    if (!is.data.frame(df) || nrow(df) < 1L) {
      return(tags$p(
        id = "ynow_admin_ops_empty",
        class = "text-muted",
        .lab("admin_ops_empty", "No operations recorded in the last 24 hours.")
      ))
    }
    rows <- lapply(seq_len(nrow(df)), function(i) {
      fmt <- if (exists("ynow_admin_ops_format_row", mode = "function")) {
        ynow_admin_ops_format_row(df[i, , drop = FALSE], locale = loc)
      } else {
        list(
          timestamp = as.character(df$created_at[[i]]),
          actor = as.character(df$actor[[i]]),
          action = as.character(df$action[[i]]),
          detail = as.character(df$detail[[i]])
        )
      }
      tags$tr(
        tags$td(fmt$timestamp %||% "—"),
        tags$td(fmt$actor %||% "—"),
        tags$td(fmt$action %||% "—"),
        tags$td(fmt$detail %||% "—")
      )
    })
    tags$div(
      style = "overflow-x:auto; max-height:55vh;",
      tags$table(
        class = "table table-condensed table-striped ynow-admin-ops-audit-table",
        id = "ynow_admin_ops_audit_table",
        tags$thead(tags$tr(
          tags$th(.lab("admin_ops_col_time", "Time (UTC)")),
          tags$th(.lab("admin_ops_col_actor", "Actor")),
          tags$th(.lab("admin_ops_col_action", "Action")),
          tags$th(.lab("admin_ops_col_detail", "Detail"))
        )),
        tags$tbody(rows)
      )
    )
  })

  # Lightweight invalidation for currently-restricted / item pickers after apply/remove.
  ynow_access_display_tick <- reactiveVal(0L)

  .ynow_access_admin_target_uids <- function() {
    scope <- as.character(input$ynow_access_scope %||% "user")[1]
    all_uid <- if (exists("YNOW_ACCESS_ALL_USERS", inherits = TRUE)) {
      as.character(get("YNOW_ACCESS_ALL_USERS", inherits = TRUE))[1]
    } else {
      "__all__"
    }
    if (identical(scope, "all")) return(all_uid)
    uids <- as.character(input$ynow_access_users %||% character())
    uids <- uids[nzchar(uids)]
    if (!length(uids)) {
      # Fallback: points single-select when multi-select empty.
      one <- as.character(input$ynow_pts_sysadmin_user %||% "")[1]
      if (nzchar(one)) uids <- one
    }
    unique(uids)
  }

  .ynow_access_admin_selected_uids <- function(scope) {
    if (identical(scope, "all")) return(character())
    uids <- as.character(input$ynow_access_users %||% character())
    uids <- uids[nzchar(uids)]
    if (!length(uids)) {
      one <- as.character(input$ynow_pts_sysadmin_user %||% "")[1]
      if (nzchar(one)) uids <- one
    }
    unique(uids)
  }

  #' Obvious read-only list of currently restricted items under the apply target.
  .ynow_access_restricted_panel_ui <- function(title_lab,
                                              value_id,
                                              panel_id,
                                              scope,
                                              uids,
                                              loc,
                                              .lab,
                                              not_selectable = FALSE) {
    none_lab <- .lab("access_none", "Nothing is restricted for this scope.")
    pick_lab <- .lab(
      "access_pick_users",
      "Select one or more users to see currently restricted items."
    )
    blocks <- if (exists("ynow_access_admin_status_blocks", mode = "function")) {
      ynow_access_admin_status_blocks(
        user_id = uids,
        scope = scope,
        locale = loc,
        user_scope_lab = .lab("access_scope_user_short", "User"),
        all_scope_lab = .lab("access_scope_all_short", "All")
      )
    } else {
      list()
    }
    # Plain-text mirror for tests / screen readers (tracks target + multi-select).
    flat_txt <- if (exists("ynow_access_admin_status_text", mode = "function")) {
      ynow_access_admin_status_text(
        user_id = uids,
        scope = scope,
        locale = loc,
        user_scope_lab = .lab("access_scope_user_short", "User"),
        all_scope_lab = .lab("access_scope_all_short", "All"),
        none_lab = none_lab,
        pick_users_lab = pick_lab
      )
    } else {
      none_lab
    }
    body <- if (identical(scope, "user") && !length(blocks)) {
      tags$p(
        class = "ynow-access-restricted-panel__empty text-muted",
        pick_lab
      )
    } else if (length(blocks) == 1L && !length(blocks[[1]]$lines)) {
      tags$p(
        class = "ynow-access-restricted-panel__empty text-muted",
        none_lab
      )
    } else {
      show_uid <- identical(scope, "user") && length(blocks) > 1L
      tagList(lapply(blocks, function(b) {
        lines <- b$lines
        item_nodes <- if (!length(lines)) {
          list(tags$li(
            class = "ynow-access-restricted-panel__none text-muted",
            none_lab
          ))
        } else {
          lapply(lines, function(ln) {
            tags$li(class = "ynow-access-restricted-panel__item", ln)
          })
        }
        tags$div(
          class = "ynow-access-restricted-panel__block",
          if (show_uid) {
            tags$div(
              class = "ynow-access-restricted-panel__uid",
              as.character(b$uid %||% "")
            )
          },
          tags$ul(
            class = "ynow-access-restricted-panel__list",
            item_nodes
          )
        )
      }))
    }
    tags$div(
      id = panel_id,
      class = paste(
        "ynow-access-restricted-panel",
        if (isTRUE(not_selectable)) "ynow-access-restricted-panel--apply" else NULL
      ),
      tags$div(
        class = "ynow-access-restricted-panel__title",
        tags$b(title_lab)
      ),
      tags$div(
        id = value_id,
        class = "ynow-access-restricted-panel__body",
        `data-ynow-restricted-flat` = flat_txt,
        body
      )
    )
  }

  output$ynow_access_locked_now <- renderUI({
    ynow_access_display_tick()
    ynow_pts_sysadmin_tick()
    scope <- as.character(input$ynow_access_scope %||% "user")[1]
    uids <- .ynow_access_admin_selected_uids(scope)
    loc <- tryCatch(
      normalize_ui_locale(isolate(ui_locale())),
      error = function(e) "zh-TW"
    )
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    .ynow_access_restricted_panel_ui(
      title_lab = .lab("access_locked_now", "Currently restricted"),
      value_id = "ynow_access_locked_now_value",
      panel_id = "ynow_access_locked_now_txt",
      scope = scope,
      uids = uids,
      loc = loc,
      .lab = .lab,
      not_selectable = FALSE
    )
  })

  output$ynow_access_action_fields <- renderUI({
    ynow_access_display_tick()
    ynow_pts_sysadmin_tick()
    scope <- as.character(input$ynow_access_scope %||% "user")[1]
    action <- as.character(input$ynow_access_action %||% "apply")[1]
    if (!action %in% c("apply", "remove")) action <- "apply"
    uids <- .ynow_access_admin_selected_uids(scope)
    loc <- tryCatch(
      normalize_ui_locale(isolate(ui_locale())),
      error = function(e) "zh-TW"
    )
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    cur_effect <- as.character(isolate(input$ynow_access_effect) %||% "lock")[1]
    if (!cur_effect %in% c("lock", "hide")) cur_effect <- "lock"
    cur_unlock <- as.character(isolate(input$ynow_access_unlock_type) %||% "login")[1]
    if (!cur_unlock %in% c("login", "points", "conditional")) cur_unlock <- "login"
    cur_points <- suppressWarnings(as.integer(isolate(input$ynow_access_points_cost) %||% 1L)[1])
    if (!is.finite(cur_points) || cur_points < 1L) cur_points <- 1L
    cur_free <- suppressWarnings(as.integer(isolate(input$ynow_access_free_count) %||% 0L)[1])
    if (!is.finite(cur_free) || cur_free < 0L) cur_free <- 0L
    cur_hours <- suppressWarnings(as.integer(isolate(input$ynow_access_time_hours) %||% 0L)[1])
    if (!is.finite(cur_hours) || cur_hours < 0L) cur_hours <- 0L
    cur_items <- as.character(isolate(input$ynow_access_item) %||% character())
    cur_items <- unique(cur_items[nzchar(cur_items)])

    if (identical(action, "remove")) {
      rem_ids <- if (exists("ynow_access_restricted_ids_for_scope", mode = "function")) {
        ynow_access_restricted_ids_for_scope(uids, scope, multi_mode = "union")
      } else {
        character()
      }
      rem_choices <- if (exists("ynow_access_item_choice_list", mode = "function")) {
        ynow_access_item_choice_list(rem_ids, locale = loc, group = TRUE)
      } else {
        character()
      }
      rem_flat <- if (is.list(rem_choices)) unlist(rem_choices, use.names = TRUE) else rem_choices
      rem_flat_chr <- as.character(rem_flat)
      rem_sel <- cur_items[cur_items %in% rem_flat_chr]
      if (!length(rem_sel) && length(rem_flat_chr)) {
        rem_sel <- rem_flat_chr[[1]]
      }
      return(tagList(
        if (!length(rem_flat_chr)) {
          tags$p(
            class = "text-muted",
            style = "font-size:12.5px;",
            .lab("access_none_remove", "No restricted items for this apply target.")
          )
        } else {
          selectizeInput(
            "ynow_access_item",
            .lab("access_remove_item", "Items to unlock"),
            choices = rem_choices,
            selected = rem_sel,
            multiple = TRUE,
            options = list(
              plugins = list("remove_button"),
              placeholder = .lab(
                "access_remove_item_ph",
                "Search and select one or more items…"
              )
            )
          )
        }
      ))
    }

    apply_ids <- if (exists("ynow_access_restrictable_ids_for_scope", mode = "function")) {
      ynow_access_restrictable_ids_for_scope(uids, scope)
    } else {
      character()
    }
    apply_choices <- if (exists("ynow_access_item_choice_list", mode = "function")) {
      ynow_access_item_choice_list(apply_ids, locale = loc, group = TRUE)
    } else {
      character()
    }
    apply_flat <- if (is.list(apply_choices)) unlist(apply_choices, use.names = TRUE) else apply_choices
    apply_flat_chr <- as.character(apply_flat)
    apply_sel <- cur_items[cur_items %in% apply_flat_chr]
    if (!length(apply_sel) && length(apply_flat_chr)) {
      apply_sel <- apply_flat_chr[[1]]
    }
    tagList(
      radioButtons(
        "ynow_access_effect",
        .lab("access_effect_label", "Restriction"),
        choiceNames = list(
          .lab("access_effect_lock", "Lock (show locked placeholder)"),
          .lab("access_effect_hide", "Hide (remove from UI)")
        ),
        choiceValues = c("lock", "hide"),
        selected = cur_effect,
        inline = TRUE
      ),
      if (!length(apply_flat_chr)) {
        tags$p(
          class = "text-muted",
          style = "font-size:12.5px;",
          .lab("access_none_apply", "Every catalog item is already restricted for this apply target.")
        )
      } else {
        selectizeInput(
          "ynow_access_item",
          .lab("access_apply_item", "Items to restrict"),
          choices = apply_choices,
          selected = apply_sel,
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = .lab(
              "access_apply_item_ph",
              "Search and select one or more items…"
            )
          )
        )
      },
      .ynow_access_restricted_panel_ui(
        title_lab = .lab(
          "access_already_restricted_label",
          "Currently restricted (not selectable)"
        ),
        value_id = "ynow_access_already_restricted_value",
        panel_id = "ynow_access_already_restricted_txt",
        scope = scope,
        uids = uids,
        loc = loc,
        .lab = .lab,
        not_selectable = TRUE
      ),
      radioButtons(
        "ynow_access_unlock_type",
        .lab("access_unlock_type_label", "Unlock condition"),
        choiceNames = list(
          .lab("access_unlock_login", "Unlock after login"),
          .lab("access_unlock_points", "Points unlock"),
          .lab("access_unlock_conditional", "Conditional unlock")
        ),
        choiceValues = c("login", "points", "conditional"),
        selected = cur_unlock,
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.ynow_access_unlock_type === 'points'",
        numericInput(
          "ynow_access_points_cost",
          .lab("access_unlock_points_cost", "Points required (FYN)"),
          value = cur_points,
          min = 1L,
          step = 1L
        )
      ),
      conditionalPanel(
        condition = "input.ynow_access_unlock_type === 'conditional'",
        fluidRow(
          column(
            6,
            numericInput(
              "ynow_access_free_count",
              .lab("access_unlock_free_count", "Free uses (0 = unused)"),
              value = cur_free,
              min = 0L,
              step = 1L
            )
          ),
          column(
            6,
            numericInput(
              "ynow_access_time_hours",
              .lab("access_unlock_time_hours", "Time limit (hours; 0 = unused)"),
              value = cur_hours,
              min = 0L,
              step = 1L
            )
          )
        ),
        tags$p(
          class = "text-muted",
          style = "font-size:12px; margin-top:0;",
          .lab(
            "access_unlock_conditional_help",
            "Set free uses and/or a time window. At least one must be greater than zero."
          )
        )
      )
    )
  })

  .ynow_access_admin_apply_changes <- function() {
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    if (!isTRUE(ynow_pts_sysadmin_on())) {
      showNotification(.lab("pts_admin_denied", "Only admins can add points."), type = "error")
      return()
    }
    if (!exists("ynow_access_set", mode = "function")) return()
    scope <- as.character(input$ynow_access_scope %||% "user")[1]
    action <- as.character(input$ynow_access_action %||% "apply")[1]
    if (!action %in% c("apply", "remove")) action <- "apply"
    locked <- identical(action, "apply")
    uids <- .ynow_access_admin_target_uids()
    if (!length(uids) || !any(nzchar(uids))) {
      showNotification(
        .lab("access_bad_user", "Select a user from the list."),
        type = "warning"
      )
      return()
    }
    items <- as.character(input$ynow_access_item %||% character())
    items <- unique(items[nzchar(items)])
    if (!length(items)) {
      showNotification(
        .lab(
          if (locked) "access_bad_item" else "access_none_remove",
          if (locked) "Choose an item from the catalog." else "No restricted items for this apply target."
        ),
        type = "warning"
      )
      return()
    }
    effect <- as.character(input$ynow_access_effect %||% "lock")[1]
    unlock_type <- as.character(input$ynow_access_unlock_type %||% "login")[1]
    if (!unlock_type %in% c("login", "points", "conditional")) unlock_type <- "login"
    points_cost <- suppressWarnings(as.integer(input$ynow_access_points_cost %||% 1L)[1])
    if (!is.finite(points_cost) || points_cost < 1L) points_cost <- 1L
    free_count <- suppressWarnings(as.integer(input$ynow_access_free_count %||% 0L)[1])
    if (!is.finite(free_count) || free_count < 0L) free_count <- 0L
    time_hours <- suppressWarnings(as.integer(input$ynow_access_time_hours %||% 0L)[1])
    if (!is.finite(time_hours) || time_hours < 0L) time_hours <- 0L
    free_arg <- if (identical(unlock_type, "conditional") && free_count > 0L) {
      free_count
    } else {
      NA_integer_
    }
    hours_arg <- if (identical(unlock_type, "conditional") && time_hours > 0L) {
      time_hours
    } else {
      NA_integer_
    }
    n_ok <- 0L
    n_fail <- 0L
    last_key <- "access_bad_item"
    for (item in items) {
      for (uid in uids) {
        res <- ynow_access_set(
          uid,
          item,
          locked = locked,
          effect = effect,
          unlock_type = unlock_type,
          points_cost = if (identical(unlock_type, "points")) points_cost else NA_integer_,
          free_count = free_arg,
          time_limit_hours = hours_arg
        )
        last_key <- as.character(res$message_key %||% last_key)
        if (isTRUE(res$ok)) n_ok <- n_ok + 1L else n_fail <- n_fail + 1L
      }
    }
    items_csv <- paste(items, collapse = ",")
    msg_for <- function(key) {
      switch(
        as.character(key %||% ""),
        access_bad_points_cost = .lab(
          "access_bad_points_cost",
          "Enter how many FYN are required for points unlock."
        ),
        access_bad_conditional = .lab(
          "access_bad_conditional",
          "For conditional unlock, set free uses and/or a time limit greater than zero."
        ),
        access_already_restricted = .lab(
          "access_already_restricted",
          "This item is already restricted for the selected apply target. Remove it first to change the rule."
        ),
        access_not_restricted = .lab(
          "access_not_restricted",
          "This item is not restricted for the selected apply target."
        ),
        .lab(key %||% "access_bad_item", "Choose an item from the catalog.")
      )
    }
    if (n_ok > 0L && n_fail == 0L) {
      showNotification(msg_for(last_key), type = "message")
      ynow_access_display_tick(isolate(ynow_access_display_tick()) + 1L)
      .ynow_push_access_locks()
      .ynow_admin_ops_log(
        if (locked) "acl.apply" else "acl.remove",
        paste0(
          "item=", items_csv,
          "; scope=", scope,
          "; effect=", effect,
          "; unlock=", unlock_type,
          "; users=", paste(uids, collapse = ","),
          "; ok=", as.character(n_ok)
        )
      )
    } else if (n_ok > 0L && n_fail > 0L) {
      showNotification(
        sprintf(
          .lab(
            "access_partial_fmt",
            "Updated %s user(s); %s skipped (already restricted or not restricted)."
          ),
          as.character(n_ok),
          as.character(n_fail)
        ),
        type = "warning"
      )
      ynow_access_display_tick(isolate(ynow_access_display_tick()) + 1L)
      .ynow_push_access_locks()
      .ynow_admin_ops_log(
        if (locked) "acl.apply" else "acl.remove",
        paste0(
          "item=", items_csv,
          "; scope=", scope,
          "; partial ok=", as.character(n_ok),
          " fail=", as.character(n_fail)
        )
      )
    } else {
      showNotification(msg_for(last_key), type = "warning")
    }
  }

  observeEvent(input$ynow_access_apply_changes, .ynow_access_admin_apply_changes())

  output$ynow_maint_status_now <- renderUI({
    ynow_maint_display_tick()
    loc <- tryCatch(
      normalize_ui_locale(isolate(ui_locale())),
      error = function(e) "zh-TW"
    )
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    status_txt <- if (exists("ynow_maintenance_admin_status_text", mode = "function")) {
      ynow_maintenance_admin_status_text(
        locale = loc,
        none_lab = .lab("maint_none", "No maintenance entries.")
      )
    } else {
      .lab("maint_none", "No maintenance entries.")
    }
    tags$div(
      id = "ynow_maint_status_now_txt",
      style = "margin:8px 0 10px; font-size:12.5px;",
      tags$b(.lab("maint_status_now", "Current maintenance")),
      ": ",
      tags$span(id = "ynow_maint_status_now_value", status_txt)
    )
  })

  output$ynow_maint_fields <- renderUI({
    ynow_maint_display_tick()
    loc <- tryCatch(
      normalize_ui_locale(isolate(ui_locale())),
      error = function(e) "zh-TW"
    )
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    cat_ids <- if (exists("ynow_maintenance_catalog", mode = "function")) {
      as.character(ynow_maintenance_catalog()$id)
    } else if (exists("ynow_access_catalog", mode = "function")) {
      as.character(ynow_access_catalog()$id)
    } else {
      character()
    }
    choices <- if (exists("ynow_maintenance_item_choice_list", mode = "function")) {
      ynow_maintenance_item_choice_list(cat_ids, locale = loc, group = TRUE)
    } else if (exists("ynow_access_item_choice_list", mode = "function")) {
      ynow_access_item_choice_list(cat_ids, locale = loc, group = TRUE)
    } else {
      character()
    }
    flat <- if (is.list(choices)) unlist(choices, use.names = TRUE) else choices
    cur_item <- as.character(isolate(input$ynow_maint_item) %||% "")[1]
    sel <- if (nzchar(cur_item) && cur_item %in% as.character(flat)) {
      cur_item
    } else if (length(flat)) {
      as.character(flat[[1]])
    } else {
      NULL
    }
    now <- tryCatch(ynow_maintenance_now(), error = function(e) Sys.time())
    default_start <- as.POSIXct(now, tz = "UTC")
    default_end <- default_start + 3600
    parse_t <- function(x, fallback) {
      if (exists("ynow_maintenance_parse_time", mode = "function")) {
        tt <- ynow_maintenance_parse_time(x)
        if (inherits(tt, "POSIXt") && length(tt) >= 1L && !is.na(tt[[1]])) return(tt)
      }
      fallback
    }
    cur_start <- parse_t(isolate(input$ynow_maint_start), default_start)
    cur_end <- parse_t(isolate(input$ynow_maint_end), default_end)
    tagList(
      if (!length(flat)) {
        tags$p(
          class = "text-muted",
          style = "font-size:12.5px;",
          .lab("maint_bad_item", "Choose an item from the catalog.")
        )
      } else {
        selectInput(
          "ynow_maint_item",
          .lab("maint_item", "Item"),
          choices = choices,
          selected = sel
        )
      },
      fluidRow(
        column(
          6,
          textInput(
            "ynow_maint_start",
            .lab("maint_start", "Window start (UTC)"),
            value = format(as.POSIXct(cur_start, tz = "UTC"), "%Y-%m-%d %H:%M", tz = "UTC"),
            placeholder = "YYYY-MM-DD HH:MM"
          )
        ),
        column(
          6,
          textInput(
            "ynow_maint_end",
            .lab("maint_end", "Window end (UTC)"),
            value = format(as.POSIXct(cur_end, tz = "UTC"), "%Y-%m-%d %H:%M", tz = "UTC"),
            placeholder = "YYYY-MM-DD HH:MM"
          )
        )
      ),
      tags$p(
        class = "text-muted",
        style = "font-size:12px; margin-top:0;",
        .lab(
          "maint_window_preview_help",
          "Users see: Under maintenance / 維護中, then Expected: / 預計時間: start-end."
        )
      )
    )
  })

  .ynow_maint_admin_apply <- function(clear = FALSE) {
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    if (!isTRUE(ynow_pts_sysadmin_on())) {
      showNotification(.lab("pts_admin_denied", "Only admins can add points."), type = "error")
      return()
    }
    if (!exists("ynow_maintenance_set", mode = "function")) return()
    item <- as.character(input$ynow_maint_item %||% "")[1]
    if (!nzchar(item)) {
      showNotification(.lab("maint_bad_item", "Choose an item from the catalog."), type = "warning")
      return()
    }
    res <- if (isTRUE(clear)) {
      ynow_maintenance_set(item, locked = FALSE)
    } else {
      ynow_maintenance_set(
        item,
        locked = TRUE,
        window_start = input$ynow_maint_start,
        window_end = input$ynow_maint_end
      )
    }
    msg <- switch(
      as.character(res$message_key %||% ""),
      maint_ok_set = .lab("maint_ok_set", "Maintenance window set for all users."),
      maint_ok_clear = .lab("maint_ok_clear", "Maintenance cleared."),
      maint_bad_window = .lab(
        "maint_bad_window",
        "Set both start and end times for the maintenance window."
      ),
      maint_bad_window_order = .lab(
        "maint_bad_window_order",
        "End time must be after start time."
      ),
      .lab("maint_bad_item", "Choose an item from the catalog.")
    )
    if (isTRUE(res$ok)) {
      showNotification(msg, type = "message")
      ynow_maint_display_tick(isolate(ynow_maint_display_tick()) + 1L)
      .ynow_push_maintenance()
      .ynow_admin_ops_log(
        if (isTRUE(clear)) "maintenance.clear" else "maintenance.set",
        if (isTRUE(clear)) {
          paste0("item=", item)
        } else {
          paste0(
            "item=", item,
            "; start=", as.character(input$ynow_maint_start %||% "")[1],
            "; end=", as.character(input$ynow_maint_end %||% "")[1]
          )
        }
      )
    } else {
      showNotification(msg, type = "warning")
    }
  }

  observeEvent(input$ynow_maint_apply, .ynow_maint_admin_apply(FALSE))
  observeEvent(input$ynow_maint_clear, .ynow_maint_admin_apply(TRUE))

  observeEvent(input$ynow_pts_sysadmin_apply, {
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    if (!isTRUE(ynow_pts_sysadmin_on())) {
      showNotification(.lab("pts_admin_denied", "Only admins can add points."), type = "error")
      return()
    }
    if (!exists("ynow_points_admin_adjust", mode = "function")) {
      showNotification(.lab("pts_msg_backend", "Points backend is unavailable."), type = "error")
      return()
    }
    # selectInput value is internal user_id (label remains opaque public_id).
    uid <- as.character(input$ynow_pts_sysadmin_user %||% "")[1]
    if (!nzchar(uid)) {
      showNotification(.lab("pts_sysadmin_bad_user", "Select a user from the list."), type = "warning")
      return()
    }
    fd <- suppressWarnings(as.integer(input$ynow_pts_sysadmin_fyn_delta)[1])
    pd <- suppressWarnings(as.integer(input$ynow_pts_sysadmin_pyn_delta)[1])
    if (!is.finite(fd)) fd <- 0L
    if (!is.finite(pd)) pd <- 0L
    if (identical(fd, 0L) && identical(pd, 0L)) {
      showNotification(
        .lab("pts_sysadmin_bad_delta", "Enter a non-zero whole-number change for FYN and/or PYN."),
        type = "warning"
      )
      return()
    }
    note <- as.character(input$ynow_pts_sysadmin_note %||% "")[1]
    res <- ynow_points_admin_adjust(
      uid, fyn_delta = fd, pyn_delta = pd, note = note, by = "sysadmin_portal"
    )
    if (isTRUE(res$ok)) {
      showNotification(
        sprintf(
          .lab("pts_sysadmin_ok_verify_fmt", "Balances updated and verified (FYN %s, PYN %s)."),
          as.character(res$fyn %||% "—"),
          as.character(res$pyn %||% "—")
        ),
        type = "message"
      )
      .ynow_admin_ops_log(
        "points.adjust",
        paste0(
          "user_id=", uid,
          "; fyn_delta=", as.character(fd),
          "; pyn_delta=", as.character(pd),
          if (nzchar(note)) paste0("; note=", note) else ""
        )
      )
      ynow_pts_sysadmin_tick(isolate(ynow_pts_sysadmin_tick()) + 1L)
      ynow_pts_panel_tick(isolate(ynow_pts_panel_tick()) + 1L)
      # Refresh banner only when the effective user is the grant target
      # (signed-in self or active view-as) — avoid overwriting with the wrong wallet.
      eff <- .ynow_pts_effective_user()
      eff_id <- as.character(eff$id %||% "")[1]
      if (identical(eff_id, uid)) {
        .ynow_pts_push_banner()
      }
    } else {
      showNotification(
        .lab(
          res$message_key %||% "pts_sysadmin_verify_failed",
          .lab("pts_msg_backend", "Points backend error.")
        ),
        type = "error"
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_admin_goto_points, {
    tryCatch(
      updateTabItems(session, "sidebar_tabs", "account_points"),
      error = function(e) {
        tryCatch(
          session$sendInputMessage("sidebar_tabs", list(value = "account_points")),
          error = function(e2) NULL
        )
      }
    )
  }, ignoreInit = TRUE)

  # Account top-up: redeem code → ledger credit (no payment processor).
  observeEvent(input$ynow_pts_redeem_submit, {
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    .set_status <- function(msg, ok = NA) {
      tryCatch(
        session$sendCustomMessage(
          "ynowPtsRedeemStatus",
          list(text = as.character(msg %||% "")[1], ok = ok)
        ),
        error = function(e) NULL
      )
    }
    u <- tryCatch(ynow_auth$user(), error = function(e) NULL)
    uid <- as.character(u$id %||% "")[1]
    if (!nzchar(uid)) {
      msg <- .lab("pts_topup_no_user", "Sign in to redeem a top-up code.")
      .set_status(msg, FALSE)
      showNotification(msg, type = "warning", duration = 4)
      return()
    }
    if (!exists("ynow_points_redeem", mode = "function")) {
      msg <- .lab("pts_topup_backend", "Points ledger is unavailable. Try again later.")
      .set_status(msg, FALSE)
      showNotification(msg, type = "error", duration = 5)
      return()
    }
    code <- as.character(input$ynow_pts_redeem_code %||% "")[1]
    res <- ynow_points_redeem(uid, code)
    if (isTRUE(res$ok)) {
      wlab <- toupper(as.character(res$wallet %||% "PYN")[1])
      msg <- sprintf(
        .lab("pts_topup_ok_fmt", "Redeemed +%s %s. FYN %s · PYN %s."),
        as.character(res$amount %||% ""),
        wlab,
        as.character(res$fyn %||% "—"),
        as.character(res$pyn %||% "—")
      )
      .set_status(msg, TRUE)
      showNotification(msg, type = "message", duration = 5)
      tryCatch(updateTextInput(session, "ynow_pts_redeem_code", value = ""), error = function(e) NULL)
      ynow_pts_panel_tick(isolate(ynow_pts_panel_tick()) + 1L)
      tryCatch(.ynow_pts_push_banner(), error = function(e) NULL)
    } else {
      key <- as.character(res$message_key %||% "pts_topup_bad_code")[1]
      fb <- switch(
        key,
        pts_topup_used = "You already redeemed this code on this account.",
        pts_topup_expired = "That redeem code has expired.",
        pts_topup_disabled = "That redeem code is disabled.",
        pts_topup_exhausted = "That redeem code has no redemptions left.",
        pts_topup_no_user = "Sign in to redeem a top-up code.",
        pts_msg_backend = "Points ledger is unavailable. Try again later.",
        "That redeem code is not valid."
      )
      msg <- .lab(key, fb)
      .set_status(msg, FALSE)
      showNotification(msg, type = "warning", duration = 5)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_pts_goto_overview, {
    tryCatch(
      updateTabItems(session, "sidebar_tabs", "account_admin"),
      error = function(e) {
        tryCatch(
          session$sendInputMessage("sidebar_tabs", list(value = "account_admin")),
          error = function(e2) NULL
        )
      }
    )
  }, ignoreInit = TRUE)

  # Admin-only: credit points to a Postgres account user by email.
  observeEvent(input$ynow_pts_admin_submit, {
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .pts_lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
    u <- tryCatch(ynow_auth$user(), error = function(e) NULL)
    if (is.null(u) || !identical(as.character(u$role %||% "")[1], "admin")) {
      showNotification(.pts_lab("pts_admin_denied", "Only admins can add points."), type = "error")
      return()
    }
    if (!exists("ynow_points_credit", mode = "function")) {
      showNotification(.pts_lab("pts_msg_backend", "Points backend is unavailable."), type = "error")
      return()
    }
    em <- ynow_auth_norm_email(input$ynow_pts_admin_email %||% "")
    amt <- suppressWarnings(as.integer(input$ynow_pts_admin_amount)[1])
    note <- as.character(input$ynow_pts_admin_note %||% "")[1]
    if (!nzchar(em)) {
      showNotification(.pts_lab("pts_admin_bad_user", "No account found for that email."), type = "warning")
      return()
    }
    if (!is.finite(amt) || amt < 1L) {
      showNotification(.pts_lab("pts_admin_bad_amount", "Enter a positive whole-number amount."), type = "warning")
      return()
    }
    target <- if (exists("ynow_db_get_user_by_email", mode = "function")) {
      tryCatch(ynow_db_get_user_by_email(em), error = function(e) NULL)
    } else {
      NULL
    }
    tid <- as.character(target$id %||% "")[1]
    if (!nzchar(tid)) {
      showNotification(.pts_lab("pts_admin_bad_user", "No account found for that email."), type = "warning")
      return()
    }
    ref <- paste0(
      "admin_credit:", tid, ":", as.integer(as.numeric(Sys.time())), ":",
      paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
    )
    res <- ynow_points_credit(
      tid,
      delta = amt,
      reason = "earn_admin",
      ref_id = ref,
      meta = list(by = u$email %||% "", note = note)
    )
    if (isTRUE(res$ok)) {
      showNotification(.pts_lab("pts_admin_ok", "Points added."), type = "message")
      ynow_pts_panel_tick(isolate(ynow_pts_panel_tick()) + 1L)
    } else {
      showNotification(.pts_lab(res$message_key %||% "pts_msg_backend", "Points backend error."), type = "error")
    }
  }, ignoreInit = TRUE)

  # Prefill Value Alert email with the signed-in account mailbox (Full; Lite unchanged).
  observe({
    if (!isTRUE(ynow_auth_enabled())) return()
    if (isTRUE(tryCatch(lite_mode(), error = function(e) FALSE))) return()
    em <- ynow_auth_session_email(ynow_auth)
    if (!nzchar(em)) return()
    tryCatch(updateTextInput(session, "pa_email", value = em), error = function(e) NULL)
    tryCatch(updateTextInput(session, "pa_quick_email", value = em), error = function(e) NULL)
  })

  .ui_msg <- function(key, ..., loc = NULL) {
    if (is.null(loc)) {
      loc <- tryCatch(isolate(ui_locale()), error = function(e) "en")
    }
    loc <- normalize_ui_locale(loc)
    msg <- ui_str(key, loc)
    dots <- list(...)
    if (length(dots)) {
      for (nm in names(dots)) {
        msg <- gsub(paste0("{", nm, "}"), as.character(dots[[nm]] %||% ""), msg, fixed = TRUE)
      }
    }
    msg
  }

  # g ≥ WACC／Ke gate: toast + open sidebar + pulse Model Dashboard + jump to SGR
  .notify_g_discount_and_guide <- function(msg_key, input_id = "sgr") {
    showNotification(
      .ui_msg(msg_key),
      type = "error",
      duration = 12,
      id = "ynow_g_discount_gate"
    )
    loc <- tryCatch(isolate(ui_locale()), error = function(e) "en")
    tryCatch(
      session$sendCustomMessage(
        "ynowGuideModelDashboard",
        list(
          tab = "get_started",
          inputId = as.character(input_id %||% "sgr")[1],
          openSidebar = TRUE,
          menuHint = ui_str("notif_g_discount_guide_menu", loc)
        )
      ),
      error = function(e) NULL
    )
    invisible(TRUE)
  }

  .push_ui_locale <- function(locale, sync_picker = TRUE, force = FALSE) {
    loc <- normalize_ui_locale(locale)
    ui_locale(loc)
    # Coalesce identical bursts (onFlushed + device + resync) so the client is not
    # flooded with multi‑KB ynowUiLocale payloads that delay the user's zh-TW apply.
    # Manual picks pass force=TRUE so a missed message can still be recovered.
    last <- session$userData$ynow_last_locale_push
    now_num <- as.numeric(Sys.time())
    if (!isTRUE(force) && is.list(last) && identical(last$loc, loc) &&
        is.numeric(last$at_num) && (now_num - last$at_num) < 0.35) {
      return(invisible(loc))
    }
    # Language chrome is custom header buttons; labels/active state sync via ynowUiLocale
    # (no shinyWidgets updateRadioGroupButtons — that re-render displaced the control on mobile).
    mode <- tryCatch(
      normalize_market_mode(isolate(market_mode())),
      error = function(e) get_market_mode()
    )
    payload <- list(
      locale = loc,
      market = mode,
      strings = ui_locale_payload(loc),
      tabs = ui_tab_label_map(loc),
      boxes = ui_box_header_specs(loc)
    )
    session$userData$ynow_last_locale_push <- list(loc = loc, at_num = now_num)
    session$sendCustomMessage("ynowUiLocale", payload)
    tryCatch({
      if (exists("mos_assumptions_push_locale", mode = "function")) {
        mos_assumptions_push_locale(session, loc)
      }
    }, error = function(e) NULL)
    # SGR 自訂輸入標籤隨 locale
    tryCatch({
      updateNumericInput(
        session, "sgr",
        label = ui_str("sgr_custom_label", loc)
      )
    }, error = function(e) NULL)
    # SGR 方法／Lifecycle 檔位選項隨 locale
    tryCatch({
      g_sel <- isolate(input$perpetual_g_method)
      if (is.null(g_sel) || !g_sel %in% c("macro", "fundamental", "lifecycle")) {
        g_sel <- APP_DEFAULTS$perpetual_g_method
      }
      updateSelectInput(
        session,
        "perpetual_g_method",
        choices = stats::setNames(
          c("macro", "fundamental", "lifecycle"),
          c(
            ui_str("sgr_method_opt_macro", loc),
            ui_str("sgr_method_opt_fundamental", loc),
            ui_str("sgr_method_opt_lifecycle", loc)
          )
        ),
        selected = g_sel
      )
    }, error = function(e) NULL)
    tryCatch({
      life_sel <- isolate(input$lifecycle_stage)
      mapped_life <- if (exists("map_legacy_lifecycle_stage", mode = "function")) {
        map_legacy_lifecycle_stage(life_sel)
      } else {
        list(selected_stage = life_sel)
      }
      life_vals <- if (exists("lifecycle_stage_ids", mode = "function")) {
        lifecycle_stage_ids(include_auto = TRUE)
      } else {
        c("auto", "HIGH_GROWTH", "GROWTH_TO_MATURE", "MATURE_GROWTH",
          "MATURE_STABLE", "DECLINING_OR_FINITE_LIFE", "REGULATED_UTILITY",
          "FINANCIAL_INSTITUTION")
      }
      life_sel <- mapped_life$selected_stage
      if (is.null(life_sel) || !life_sel %in% life_vals) life_sel <- APP_DEFAULTS$lifecycle_stage
      updateSelectInput(
        session,
        "lifecycle_stage",
        choices = if (exists("lifecycle_stage_choices", mode = "function")) {
          lifecycle_stage_choices(locale = loc, include_auto = TRUE)
        } else {
          stats::setNames(life_vals, life_vals)
        },
        selected = life_sel
      )
    }, error = function(e) NULL)
    # HFV 驗證樣本範圍：標籤／三選項隨 locale 更新（值不變）
    tryCatch({
      oos_sel <- isolate(input$bt_fv_oos_mode)
      if (is.null(oos_sel) || !oos_sel %in% c("realized", "expanding", "insample")) {
        oos_sel <- "realized"
      }
      updateRadioButtons(
        session,
        "bt_fv_oos_mode",
        label = ui_str("hfv_oos_mode_label", loc),
        choices = stats::setNames(
          c("realized", "expanding", "insample"),
          c(
            ui_str("hfv_oos_realized", loc),
            ui_str("hfv_oos_expanding", loc),
            ui_str("hfv_oos_insample", loc)
          )
        ),
        selected = oos_sel
      )
    }, error = function(e) NULL)
    # HFV: show-benchmark + sample window
    tryCatch({
      updateCheckboxInput(
        session, "bt_hfv_show_bench",
        label = ui_str("hfv_show_bench", loc)
      )
    }, error = function(e) NULL)
    tryCatch({
      win_sel <- isolate(input$bt_fv_conv_window)
      if (is.null(win_sel) || !win_sel %in% c("all", "1y", "3y", "5y", "custom")) {
        win_sel <- "all"
      }
      updateRadioButtons(
        session,
        "bt_fv_conv_window",
        label = ui_str("hfv_conv_window_label", loc),
        choices = stats::setNames(
          c("all", "1y", "3y", "5y", "custom"),
          c(
            ui_str("hfv_win_all", loc),
            ui_str("hfv_win_1y", loc),
            ui_str("hfv_win_3y", loc),
            ui_str("hfv_win_5y", loc),
            ui_str("hfv_win_custom", loc)
          )
        ),
        selected = win_sel
      )
    }, error = function(e) NULL)
    tryCatch({
      updateDateRangeInput(
        session,
        "bt_fv_conv_custom",
        language = if (identical(loc, "en")) "en" else "zh-TW"
      )
    }, error = function(e) NULL)
    # Backtest: NAV window chips + holding / strategy control labels
    tryCatch({
      nav_sel <- isolate(input$bt_nav_window)
      if (is.null(nav_sel) || !nav_sel %in% c("all", "1y", "3y", "5y", "custom")) {
        nav_sel <- "all"
      }
      updateRadioButtons(
        session,
        "bt_nav_window",
        label = ui_str("bt_nav_window_label", loc),
        choices = stats::setNames(
          c("all", "1y", "3y", "5y", "custom"),
          c(
            ui_str("hfv_win_all", loc),
            ui_str("hfv_win_1y", loc),
            ui_str("hfv_win_3y", loc),
            ui_str("hfv_win_5y", loc),
            ui_str("hfv_win_custom", loc)
          )
        ),
        selected = nav_sel
      )
    }, error = function(e) NULL)
    tryCatch({
      updateDateRangeInput(
        session,
        "bt_nav_custom",
        language = if (identical(loc, "en")) "en" else "zh-TW"
      )
    }, error = function(e) NULL)
    tryCatch({
      updateNumericInput(session, "bt_net_margin", label = ui_str("bt_net_margin_label", loc))
      updateNumericInput(session, "bt_rev_growth", label = ui_str("bt_rev_growth_label", loc))
      updateNumericInput(session, "bt_eps_growth", label = ui_str("bt_eps_growth_label", loc))
      updateNumericInput(session, "bt_fcf_cv", label = ui_str("bt_fcf_cv_label", loc))
    }, error = function(e) NULL)
    tryCatch({
      updateCheckboxInput(session, "bt_param_auto", label = ui_str("bt_param_auto_label", loc))
    }, error = function(e) NULL)
    tryCatch({
      updateSliderInput(session, "bt_w_vg", label = ui_str("bt_w_vg_label", loc))
      updateSliderInput(session, "bt_w_mom", label = ui_str("bt_w_mom_label", loc))
      updateSliderInput(session, "bt_w_rsi", label = ui_str("bt_w_rsi_label", loc))
      updateSliderInput(session, "bt_max_exp", label = ui_str("bt_max_exp_label", loc))
      updateSliderInput(session, "bt_min_exp_pass", label = ui_str("bt_min_exp_label", loc))
    }, error = function(e) NULL)
    # DDM 模型選項：標籤隨 locale（值不變）
    tryCatch({
      ddm_sel <- isolate(input[["mod_ddm-ddm_mode"]])
      if (is.null(ddm_sel) || !ddm_sel %in% c("gordon", "spm", "two_stage")) {
        ddm_sel <- APP_DEFAULTS$ddm_mode %||% "gordon"
      }
      updateRadioButtons(
        session,
        "mod_ddm-ddm_mode",
        label = ui_str("ddm_mode_label", loc),
        choices = stats::setNames(
          c("gordon", "spm", "two_stage"),
          c(
            ui_str("ddm_mode_gordon", loc),
            ui_str("ddm_mode_spm", loc),
            ui_str("ddm_mode_two_stage", loc)
          )
        ),
        selected = ddm_sel
      )
    }, error = function(e) NULL)
    # Relative multiples capital-claim families: Equity | Enterprise (legacy earnings/ps → equity)
    tryCatch({
      rel_sel <- .rel_normalize_mode(isolate(input[["mod_rel-rel_mode"]]))
      if (!rel_sel %in% c("equity", "enterprise")) {
        rel_sel <- .rel_normalize_mode(APP_DEFAULTS$rel_mode %||% "equity")
      }
      updateRadioButtons(
        session,
        "mod_rel-rel_mode",
        label = ui_str("rel_mode_label", loc),
        choices = stats::setNames(
          c("equity", "enterprise"),
          c(
            ui_str("rel_mode_equity", loc),
            ui_str("rel_mode_enterprise", loc)
          )
        ),
        selected = rel_sel
      )
    }, error = function(e) NULL)
    # CAPM Rf / Rm labels (β label is owned by smart-tag observer)
    tryCatch({
      updateNumericInput(session, "capm_rf", label = ui_str("capm_rf_label", loc))
      updateNumericInput(session, "capm_rm", label = ui_str("capm_rm_label", loc))
      updateNumericInput(session, "ddm_capm_rf", label = ui_str("capm_rf_label", loc))
      updateNumericInput(session, "ddm_capm_rm", label = ui_str("capm_rm_label", loc))
    }, error = function(e) NULL)
    tryCatch({
      updateCheckboxInput(
        session, "use_estimated_re",
        label = ui_str("use_estimated_re_label", loc)
      )
    }, error = function(e) NULL)
    tryCatch({
      updateCheckboxInput(
        session, "ddm_use_estimated_re",
        label = ui_str("ddm_use_estimated_ke_label", loc)
      )
    }, error = function(e) NULL)
    # Rolling lookback choices + Bottom-Up aggregation
    tryCatch({
      lb_sel <- as.character(isolate(input$beta_lookback_months) %||% APP_DEFAULTS$beta_lookback_months)[1]
      if (!lb_sel %in% c("12", "24", "60")) lb_sel <- "60"
      updateSelectInput(
        session,
        "beta_lookback_months",
        label = ui_str("beta_lookback_label", loc),
        choices = stats::setNames(
          c(12, 24, 60),
          c(
            ui_str("beta_lookback_1y", loc),
            ui_str("beta_lookback_2y", loc),
            ui_str("beta_lookback_5y", loc)
          )
        ),
        selected = lb_sel
      )
    }, error = function(e) NULL)
    tryCatch({
      updateSelectizeInput(
        session, "beta_bench",
        label = ui_str("beta_bench_label", loc),
        options = list(
          create = TRUE,
          placeholder = ui_str("beta_bench_placeholder", loc),
          maxItems = 1
        )
      )
    }, error = function(e) NULL)
    tryCatch({
      agg_sel <- as.character(isolate(input$beta_bottomup_agg) %||% APP_DEFAULTS$beta_bottomup_agg)[1]
      if (!agg_sel %in% c("mean", "median")) agg_sel <- "mean"
      updateRadioButtons(
        session,
        "beta_bottomup_agg",
        label = ui_str("beta_bottomup_agg_label", loc),
        choices = stats::setNames(
          c("mean", "median"),
          c(
            ui_str("beta_bottomup_agg_mean", loc),
            ui_str("beta_bottomup_agg_median", loc)
          )
        ),
        selected = agg_sel
      )
    }, error = function(e) NULL)
    tryCatch({
      updateSelectizeInput(
        session, "beta_peers",
        label = ui_str("beta_peers_label", loc),
        options = list(
          create = TRUE,
          placeholder = ui_str("beta_peers_placeholder", loc),
          plugins = list("remove_button"),
          maxItems = 15
        )
      )
    }, error = function(e) NULL)
    tryCatch({
      fb_sel <- as.character(isolate(input$feedback_category) %||% "enhancement")[1]
      if (!fb_sel %in% c("enhancement", "bug", "ux", "other")) fb_sel <- "enhancement"
      updateSelectInput(
        session, "feedback_category",
        label = ui_str("feedback_category_label", loc),
        choices = stats::setNames(
          c("enhancement", "bug", "ux", "other"),
          c(
            ui_str("feedback_cat_enhancement", loc),
            ui_str("feedback_cat_bug", loc),
            ui_str("feedback_cat_ux", loc),
            ui_str("feedback_cat_other", loc)
          )
        ),
        selected = fb_sel
      )
    }, error = function(e) NULL)
    tryCatch({
      g_sel <- as.character(isolate(input$g_growth_method) %||% APP_DEFAULTS$g_growth_method)[1]
      g_vals <- c("fundamental", "cagr", "mean", "median", "last_year", "custom")
      if (!g_sel %in% g_vals) g_sel <- APP_DEFAULTS$g_growth_method
      updateSelectInput(
        session, "g_growth_method",
        label = ui_str("fcf_g_method_label", loc),
        choices = stats::setNames(
          g_vals,
          c(
            ui_str("fcf_g_fundamental", loc),
            ui_str("fcf_g_cagr", loc),
            ui_str("fcf_g_mean", loc),
            ui_str("fcf_g_median", loc),
            ui_str("fcf_g_last", loc),
            ui_str("fcf_g_custom", loc)
          )
        ),
        selected = g_sel
      )
    }, error = function(e) NULL)
    tryCatch({
      b_sel <- as.character(isolate(input[["mod_pb-basis"]]) %||% APP_DEFAULTS$pb_basis)[1]
      if (!b_sel %in% c("bvps", "tbvps", "navps")) b_sel <- APP_DEFAULTS$pb_basis
      updateSelectInput(
        session, "mod_pb-basis",
        label = ui_str("pb_basis_label", loc),
        choices = stats::setNames(
          c("bvps", "tbvps", "navps"),
          c(
            ui_str("pb_basis_bvps", loc),
            ui_str("pb_basis_tbvps", loc),
            ui_str("pb_basis_navps", loc)
          )
        ),
        selected = b_sel
      )
    }, error = function(e) NULL)
  }

  # Once-per-session lazy page mounts (heavy tabs). Revisit keeps mounted UI.
  .ynow_lazy_tab_ctrl <- .ynow_register_lazy_tabs(
    input, output, session,
    builders = list(
      macro_market = function() {
        .ynow_page_ui_macro_market(
          locale = isolate(ui_locale()),
          market_mode = isolate(market_mode())
        )
      },
      bluechip = function() {
        .ynow_page_ui_bluechip(
          market_mode = isolate(market_mode()),
          locale = isolate(ui_locale())
        )
      },
      hfv = function() .ynow_page_ui_hfv(),
      lab_notes = function() .ynow_page_ui_lab_notes(),
      testing = function() {
        # Signed-in account members must not receive the Sys. Admin portal UI.
        logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
        if (isTRUE(logged)) {
          loc <- tryCatch(
            normalize_ui_locale(isolate(ui_locale())),
            error = function(e2) "en"
          )
          return(tags$div(
            class = "ynow-sysadmin-blocked",
            style = "padding:1.25rem;color:#666;",
            tryCatch(
              ui_str("pts_sysadmin_member_blocked", loc),
              error = function(e) {
                "System admin is available only before account sign-in. Sign out to open the ops portal."
              }
            )
          ))
        }
        .ynow_page_ui_testing()
      },
      asset_transmission = function() {
        .ynow_page_ui_asset_transmission(isolate(ui_locale()))
      },
      global_prediction = function() {
        .ynow_page_ui_global_prediction(isolate(ui_locale()))
      },
      decision_checklist = function() decision_checklist_tab_body_ui(),
      about = function() .ynow_page_ui_about(),
      feedback = function() .ynow_page_ui_feedback(),
      sensitivity = function() .ynow_page_ui_sensitivity(),
      smart_analysis = function() .ynow_page_ui_smart_analysis(),
      company_advance = function() .ynow_page_ui_company_advance(),
      price_alerts = function() price_alert_tab_body_ui()
    ),
    on_first_mount = list(
      macro_market = function() {
        macro_market_server(
          "macro",
          market_mode_rv = market_mode,
          ui_locale_rv = ui_locale,
          lite_mode_rv = reactive(isTRUE(input$ynow_lite_mode)),
          tab_active_rv = reactive(
            identical(as.character(input$sidebar_tabs %||% "")[1], "macro_market")
          ),
          # Only link after a ticker is loaded — ignore APP_DEFAULTS industry alone.
          company_industry_rv = reactive({
            tk <- tryCatch(current_ticker(), error = function(e) NULL)
            tk <- as.character(tk %||% "")[1]
            if (!nzchar(tk) || is.na(tk)) return("")
            as.character(input$industry_choice %||% "")[1]
          })
        )
      },
      bluechip = function() {
        # Load chain-rank snapshot once when user opens Blue Chip (not every connect).
        mm <- tryCatch(
          normalize_market_mode(isolate(market_mode())),
          error = function(e) "US"
        )
        snap <- tryCatch(
          lab_load_ind_mcap_chain_snapshot(mm),
          error = function(e) lab_ind_mcap_chain_snapshot_empty()
        )
        lab_im_ind_mcap_snap(snap)
        invisible(TRUE)
      },
      asset_transmission = function() {
        asset_transmission_server(
          "atx",
          ui_locale_rv = ui_locale,
          market_mode_rv = market_mode,
          gain_scheme_rv = reactive({
            s <- tryCatch(values$user_settings, error = function(e) NULL)
            as.character(s$gain_loss_scheme %||% "auto")[1]
          }),
          tab_active_rv = reactive(
            identical(as.character(input$sidebar_tabs %||% ""), "asset_transmission")
          )
        )
      },
      global_prediction = function() {
        global_prediction_server(
          "global_prediction",
          ui_locale_rv = ui_locale,
          market_mode_rv = market_mode,
          tab_active_rv = reactive(
            identical(as.character(input$sidebar_tabs %||% ""), "global_prediction")
          ),
          subtab_active_rv = reactive(TRUE)
        )
      }
    ),
    after_mount = function(tab) {
      lazy_mount_nonce(isolate(as.integer(lazy_mount_nonce()) %||% 0L) + 1L)
      # One immediate push + one delayed after paint (selectize may not bind yet).
      # Avoid the old triple push (0 / 0.05 / 0.25) which re-invalidated locale
      # reactives three times on every first visit to a lazy tab.
      push_loc <- function() {
        tryCatch(
          .push_ui_locale(isolate(ui_locale()), sync_picker = FALSE),
          error = function(e) NULL
        )
      }
      push_loc()
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() {
          tryCatch({
            lazy_mount_nonce(isolate(as.integer(lazy_mount_nonce()) %||% 0L) + 1L)
            push_loc()
          }, error = function(e) NULL)
        }, delay = 0.12)
      }
      invisible(tab)
    }
  )

  # Active sidebar tab (for gating heavy Lab / Dashboard / Snapshot work).
  .ynow_sidebar_tab <- reactive({
    as.character(input$sidebar_tabs %||% "")[1]
  })
  .ynow_bluechip_mounted <- function() {
    tryCatch(isTRUE(.ynow_lazy_tab_ctrl$mounted("bluechip")), error = function(e) FALSE)
  }

  # Banner right logo: soft-refresh the active sidebar tab only (no session$reload).
  .ynow_ticker_refresh_tabs <- c(
    "dashboard", "smart_analysis", "company_advance",
    "dcf_calculator", "ddm_calculator", "pb_calculator", "nav_calculator",
    "ri_calculator", "sotp_calculator", "rel_mult_ev", "rel_multiples_calculator",
    "rel_mult_rev", "hfv", "decision_checklist", "lab_notes", "get_started"
  )
  .ynow_cancel_other_tab_work <- function(keep) {
    plan <- ynow_logo_cancel_plan(keep, .ynow_ticker_refresh_tabs)
    if (isTRUE(plan$bump_search)) {
      # No replacement fetch. .search_done leaves ticker_search_busy set when
      # the ticker string is unchanged, so the discard branch clears it.
      gen <- isolate(as.integer(ticker_search_gen()) %||% 0L) + 1L
      ticker_search_discard_gen(gen)
      ticker_search_gen(gen)
    }
    if (isTRUE(plan$bump_bluechip)) {
      lab_im_eval_gen(isolate(as.integer(lab_im_eval_gen()) %||% 0L) + 1L)
      lab_im_eval_pending(NULL)
      lab_im_ind_mcap_gen(isolate(as.integer(lab_im_ind_mcap_gen()) %||% 0L) + 1L)
      lab_im_ind_mcap_pending(NULL)
    }
    if (isTRUE(plan$bump_cluster)) {
      lab_cluster_gen(isolate(as.integer(lab_cluster_gen()) %||% 0L) + 1L)
      lab_cluster_pending(NULL)
    }
    invisible(NULL)
  }
  observeEvent(input$ynow_tab_refresh, {
    payload <- input$ynow_tab_refresh
    tab <- if (is.list(payload)) {
      as.character(payload$tab %||% "")[1]
    } else {
      as.character(payload %||% "")[1]
    }
    if (identical(tab, "business_breakdown_lab")) tab <- "company_advance"
    if (identical(tab, "bluechip_lab")) tab <- "bluechip"
    if (!nzchar(tab)) {
      tab <- as.character(isolate(input$sidebar_tabs) %||% "")[1]
    }
    if (!nzchar(tab)) return()

    focus <- is.list(payload) && isTRUE(ynow_logo_payload_focus(payload$focus))
    n <- if (is.list(payload)) {
      suppressWarnings(as.integer(payload$n %||% NA_integer_)[1])
    } else {
      NA_integer_
    }
    if (length(n) != 1L || is.na(n)) n <- NA_integer_

    if (isTRUE(focus)) {
      tryCatch(.ynow_cancel_other_tab_work(keep = tab), error = function(e) NULL)
    } else {
      tryCatch(
        session$sendCustomMessage("ynowTabBusy", list(tab = tab, busy = TRUE)),
        error = function(e) NULL
      )
    }
    tryCatch(
      session$sendCustomMessage(
        "ynowHeaderLogoSpin",
        list(
          spin = TRUE,
          tab = tab,
          rate = if (isTRUE(focus)) "fast" else "slow",
          n = n
        )
      ),
      error = function(e) NULL
    )

    refreshed <- tryCatch(
      isTRUE(.ynow_lazy_tab_ctrl$refresh(tab)),
      error = function(e) FALSE
    )
    lazy_mount_nonce(isolate(as.integer(lazy_mount_nonce()) %||% 0L) + 1L)
    tryCatch(
      .push_ui_locale(isolate(ui_locale()), sync_picker = FALSE),
      error = function(e) NULL
    )

    # Company / model tabs: re-run the current ticker search without leaving the tab.
    if (isTRUE(tab %in% .ynow_ticker_refresh_tabs)) {
      tk <- tryCatch(as.character(isolate(current_ticker()) %||% "")[1], error = function(e) "")
      if (!nzchar(tk)) {
        tk <- tryCatch(as.character(isolate(input$sc) %||% "")[1], error = function(e) "")
      }
      if (nzchar(tk) && !identical(toupper(tk), "NA")) {
        tryCatch(.ynow_fire_ticker_search(tk, goto_tab = NULL), error = function(e) NULL)
      }
    }

    # Blue Chip: re-arm auto-eval when scores are empty (same path as first open).
    if (identical(tab, "bluechip") && isTRUE(refreshed)) {
      tryCatch(lab_im_auto_start_armed(FALSE), error = function(e) NULL)
    }

    # A tab-busy false at 0.2s would clear the Search dot this click just started.
    # Spin-off still goes out. The client ignores it while the keep tab has
    # other busy sources, and drops the fast spin once those sources finish.
    done_tab <- tab
    done_n <- n
    focus_click <- isTRUE(focus)
    session$onFlushed(function() {
      finish <- function() {
        if (!isTRUE(focus_click)) {
          tryCatch(
            session$sendCustomMessage(
              "ynowTabBusy",
              list(tab = done_tab, busy = FALSE, n = done_n)
            ),
            error = function(e) NULL
          )
        }
        tryCatch(
          session$sendCustomMessage(
            "ynowHeaderLogoSpin",
            list(spin = FALSE, tab = done_tab, n = done_n)
          ),
          error = function(e) NULL
        )
      }
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(finish, delay = 0.2)
      } else {
        finish()
      }
    }, once = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Starter (1 GB / 1 worker): drop heavy session payloads on disconnect so the
  # next browser can reuse RAM instead of waiting for opportunistic GC.
  session$onSessionEnded(function() {
    ynow_session_capacity_cleanup(list(
      function() tryCatch(scraped_financials(NULL), error = function(e) NULL),
      function() tryCatch(summary_data(NULL), error = function(e) NULL),
      function() tryCatch(company_profile(NULL), error = function(e) NULL),
      function() tryCatch(dcf_value_result(NULL), error = function(e) NULL),
      function() tryCatch(dcf_audit_snap(NULL), error = function(e) NULL),
      function() tryCatch(bt_result(NULL), error = function(e) NULL),
      function() tryCatch(bt_validation(NULL), error = function(e) NULL),
      function() tryCatch(bt_hfv_fv(NULL), error = function(e) NULL),
      function() tryCatch(lab_im_scores(NULL), error = function(e) NULL),
      function() tryCatch(lab_cluster_result(NULL), error = function(e) NULL),
      function() tryCatch(lab_im_ind_mcap_snap(
        tryCatch(lab_ind_mcap_chain_snapshot_empty(), error = function(e) NULL)
      ), error = function(e) NULL),
      function() {
        # If this session held the process heavy-job lock, free it.
        own <- tryCatch(ynow_heavy_job_owner(), error = function(e) "")
        if (nzchar(own)) {
          tryCatch(ynow_heavy_job_release(own), error = function(e) NULL)
        }
      }
    ))
  })

  # Business Breakdown — sidebar tab (shares global ticker; id company_advance)
  business_breakdown_lab_server(
    "bblab",
    market_mode_rv = market_mode,
    ui_locale_rv = ui_locale,
    current_ticker_rv = current_ticker,
    logo_colors_rv = corp_name_logo_colors
  )

  value_alert_metric_snapshot <- reactive({
    tk <- tryCatch(toupper(trimws(as.character(current_ticker() %||% "")[1])), error = function(e) "")
    if (!nzchar(tk)) return(NULL)
    mets <- list()
    # Price / FV / MOS from smart path
    tryCatch({
      px <- suppressWarnings(as.numeric(scraped_market_cap()$price)[1])
      if (is.finite(px)) mets$last_price <- px
    }, error = function(e) NULL)
    tryCatch({
      band <- primary_valuation_band()
      base <- suppressWarnings(as.numeric(band$base)[1])
      if (is.finite(base)) mets$fair_value <- base
      sec <- suppressWarnings(as.numeric(secondary_valuation_point())[1])
      if (is.finite(sec)) mets$fair_value_secondary <- sec
      if (is.finite(base) && is.finite(px) && abs(base) > 0) {
        mets$mos_pct <- (base - px) / abs(base) * 100
        mets$ynow_mos_pct <- mets$mos_pct
      }
    }, error = function(e) NULL)
    tryCatch({
      b <- .summary_beta_value()
      if (is.finite(b)) mets$summary_beta <- b
    }, error = function(e) NULL)
    tryCatch({
      f <- firm_unlever_reactive()
      bu <- if (!is.null(f)) suppressWarnings(as.numeric(f$beta_u)[1]) else NA_real_
      if (is.finite(bu)) mets$hamada_beta_u <- bu
    }, error = function(e) NULL)
    tryCatch({
      df <- summary_data()
      if (!is.null(df) && "EPS (TTM)" %in% df$Item) {
        eps <- suppressWarnings(as.numeric(gsub(
          "[^0-9.eE+-]", "", as.character(df$Value[df$Item == "EPS (TTM)"][1])
        ))[1])
        if (is.finite(eps)) mets$eps_ttm <- eps
      }
    }, error = function(e) NULL)
    out <- list()
    out[[tk]] <- mets
    out
  })
  price_alert_server(
    input, output, session,
    ui_locale_rv = ui_locale,
    metric_snapshot_rv = value_alert_metric_snapshot
  )

  # MOS Assumptions → Value Alert form default MOS % (rule can still override).
  observeEvent(input$mos_alert_default_pct, {
    v <- suppressWarnings(as.numeric(input$mos_alert_default_pct)[1])
    if (!is.finite(v)) return()
    tryCatch({
      updateNumericInput(session, "pa_mos", value = v)
      updateNumericInput(session, "pa_quick_mos", value = v)
    }, error = function(e) NULL)
  }, ignoreInit = TRUE)

  # 語言控制：只改 UI locale，不碰顯示幣別；手動選擇鎖定後不再套用裝置預設。
  # Always re-push (even if pick == current): a missed ynowUiLocale custom message
  # leaves the client with optimistic .active chrome but English strings / null lang;
  # a second click used to early-return as noop and never recover.
  observeEvent(input$ui_locale_pick, {
    pick <- normalize_ui_locale(input$ui_locale_pick)
    ui_locale_user_picked(TRUE)
    .push_ui_locale(pick, sync_picker = FALSE, force = TRUE)
  }, ignoreInit = TRUE)

  # Client asks for a locale re-broadcast after connect / missed custom messages.
  observeEvent(input$ynow_locale_resync, {
    .push_ui_locale(isolate(ui_locale()), sync_picker = FALSE)
  }, ignoreInit = TRUE)

  # 裝置／瀏覽器語言 → 初次 UI locale（僅一次；不覆寫手動選擇）
  observeEvent(input$device_ui_locale, {
    if (isTRUE(isolate(ui_locale_user_picked())) ||
        isTRUE(isolate(device_ui_locale_applied()))) {
      return()
    }
    loc <- locale_from_device_language(input$device_ui_locale, default = "en")
    device_ui_locale_applied(TRUE)
    if (identical(loc, isolate(ui_locale()))) return()
    .push_ui_locale(loc, sync_picker = TRUE)
  }, ignoreInit = TRUE)

  # Push Blue Chip concept-group choices for the active market (US ≠ TW).
  .lab_im_push_concept_choices <- function(mode, locale = NULL, clear_selected = FALSE) {
    mm <- tryCatch(normalize_market_mode(mode), error = function(e) "US")
    loc <- tryCatch({
      raw_loc <- locale
      if (is.null(raw_loc) || !nzchar(as.character(raw_loc)[1])) {
        raw_loc <- isolate(ui_locale())
      }
      normalize_ui_locale(raw_loc)
    }, error = function(e) "zh-TW")
    if (!exists("lab_concept_group_choices", mode = "function")) return(invisible(NULL))
    concept_choices <- tryCatch(
      lab_concept_group_choices(mm, loc),
      error = function(e) character(0)
    )
    concept_cur <- if (isTRUE(clear_selected)) {
      character(0)
    } else {
      cur <- tryCatch(
        as.character(isolate(input$lab_im_concepts) %||% character(0)),
        error = function(e) character(0)
      )
      cur[cur %in% unname(concept_choices)]
    }
    tryCatch(
      updateSelectizeInput(
        session,
        "lab_im_concepts",
        choices = concept_choices,
        selected = concept_cur,
        server = FALSE,
        options = list(
          placeholder = tryCatch(
            ui_str("lab_im_concepts_placeholder", loc),
            error = function(e) "Select one or more concept groups…"
          ),
          plugins = list("remove_button")
        )
      ),
      error = function(e) NULL
    )
    invisible(NULL)
  }

  observeEvent(input$market_mode_pick, {
    mode <- normalize_market_mode(input$market_mode_pick)
    prev <- market_mode()
    if (identical(mode, prev)) return()
    market_mode(mode)
    set_market_mode(mode)
    prof <- market_profile(mode)

    # Concept groups are market-scoped — refresh if Blue Chip is already mounted.
    if (isTRUE(tryCatch(.ynow_lazy_tab_ctrl$mounted("bluechip"), error = function(e) FALSE))) {
      .lab_im_push_concept_choices(mode, clear_selected = TRUE)
    }

    # 市場切換：不覆寫使用者語言；僅刷新 chrome 的 market class／美股｜台股按鈕
    .push_ui_locale(isolate(ui_locale()), sync_picker = FALSE)

    # 損益表圖表選單：台股顯示台灣中文標籤（值仍為英文，供比對）
    tryCatch({
      ch <- is_metric_choices_for_locale(mode)
      sel <- isolate(input$is_type)
      if (is.null(sel) || !sel %in% unname(ch)) sel <- unname(ch)[1]
      updateSelectInput(session, "is_type", choices = ch, selected = sel)
    }, error = function(e) NULL)

    # 顯示幣別預設（與語言無關；使用者仍可再切 USD／TWD）
    session_currency(prof$session_currency)
    shinyWidgets::updateRadioGroupButtons(
      session, "session_ccy_pick", selected = prof$session_currency
    )
    tryCatch(
      set_ynow_currency_context(session_ccy = prof$session_currency),
      error = function(e) NULL
    )
    tryCatch(.apply_session_currency(prof$session_currency), error = function(e) NULL)
    updateNumericInput(session, "wacc_tax", value = prof$wacc_tax)
    updateSelectizeInput(
      session, "beta_bench",
      choices = prof$beta_bench_choices,
      selected = prof$beta_bench,
      server = FALSE
    )

    sc_datalist_choices(ticker_presets_for_market(mode))
    lab_im_scores(NULL)
    lab_im_auto_start_armed(FALSE)
    lab_im_ind_mcap_auto_armed(FALSE)
    .clear_lab_cluster_result()
    lab_im_catalog_nonce(isolate(lab_im_catalog_nonce()) + 1L)

    # Rf
    rf_new <- tryCatch(cached_get_risk_free_rate(mode), error = function(e) prof$rf_fallback)
    if (is.finite(rf_new) && rf_new > 0) {
      updateNumericInput(session, "capm_rf", value = round(as.numeric(rf_new), 2))
    }

    # No market-default ticker preload (TW 2330 / US TSM). Clear the input;
    # set market-specific placeholder here (updateTextInput would otherwise
    # wipe the JS placeholder from applyUiLocale).
    sc_ph <- tryCatch(
      ui_str(
        if (identical(mode, "TW")) "ticker_placeholder_tw" else "ticker_placeholder_us",
        isolate(ui_locale())
      ),
      error = function(e) if (identical(mode, "TW")) "e.g. 2330" else "e.g. AAPL / TSM"
    )
    tryCatch(
      updateTextInput(session, "sc", value = "", placeholder = sc_ph),
      error = function(e) {
        tryCatch(updateTextInput(session, "sc", value = ""), error = function(e2) NULL)
      }
    )
    user_has_searched(FALSE)
    param_audit_baseline(NULL)
    param_audit_baseline_at(NULL)
    param_audit_baseline_ticker(NULL)
    param_audit_capture_token(isolate(param_audit_capture_token()) + 1L)
    # Drop prior market's ticker / statements so no cross-market scrape runs.
    current_ticker(NULL)
    summary_data(NULL)
    scraped_financials(NULL)
    fs_all_empty(FALSE)
    tw_is_esb(FALSE)
    tw_tpex_fs_fallback(FALSE)
    corp_display_name("")
    corp_display_name_en("")
    corp_name_logo_colors(character(0))
    corp_industry_text("等待搜尋...")
    company_profile(NULL)
    company_key_dates(NULL)
    company_board_election(NULL)
    company_cb_dilution(NULL)
    company_ownership(NULL)
    own_selected_holder(NULL)
    own_holder_detail(NULL)

    # SEC 頁籤：僅美股
    if (isTRUE(prof$show_sec_lab)) {
      shinyjs::runjs("$('a[data-value=\"sec_notes\"]').closest('li').show();")
    } else {
      shinyjs::runjs(paste0(
        "$('a[data-value=\"sec_notes\"]').closest('li').hide();",
        "if ($('#dashboard_report a[data-value=\"sec_notes\"]').parent().hasClass('active') || ",
        "$('a[data-value=\"sec_notes\"]').parent().hasClass('active')) {",
        "$('a[data-value=\"Income Statement\"]').tab('show');",
        "}"
      ))
    }

    showNotification(
      .ui_msg(
        "notif_market_switched",
        market = prof$label_zh %||% as.character(mode),
        rf = prof$rf_label_zh %||% "",
        ccy = prof$session_currency
      ),
      type = "message", duration = 8
    )

    # Cross-market pending (Recent Search chip or US→TW 4-digit auto-switch)
    # — fire after clear settles.
    pend <- isolate(pending_recent_fetch())
    pending_recent_fetch(NULL)
    pend_sym <- ""
    pend_goto <- "dashboard"
    if (is.list(pend)) {
      pend_sym <- trimws(as.character(pend$symbol %||% "")[1])
      gt <- pend$goto_tab
      if (!is.null(gt) && nzchar(as.character(gt)[1])) {
        pend_goto <- as.character(gt)[1]
      } else {
        pend_goto <- NULL
      }
    } else {
      pend_sym <- trimws(as.character(pend %||% "")[1])
    }
    if (nzchar(pend_sym)) {
      shinyjs::delay(180L, {
        .ynow_fire_ticker_search(pend_sym, goto_tab = pend_goto)
      })
    }
  }, ignoreInit = TRUE)

  # ==========================================
  # 🔎 代號預選清單（Ticker／Stock Code + Radar focus ticker）
  # ==========================================

  .ynow_ticker_suggest_div <- function(div_id, choices, mode) {
    ch <- choices
    if (is.null(ch) || length(ch) == 0) ch <- ticker_presets_for_market(mode)
    labs <- names(ch)
    if (is.null(labs)) labs <- unname(ch)
    labs[!nzchar(labs)] <- unname(ch)[!nzchar(labs)]
    n <- min(length(ch), 12L)
    tags$div(
      id = div_id,
      class = "ynow-ticker-suggest",
      role = "listbox",
      lapply(seq_len(n), function(i) {
        fetch_sym <- as.character(unname(ch)[[i]])
        disp_sym <- display_ticker_for_market(fetch_sym, mode)
        lab <- as.character(labs[[i]])
        # 剝除標籤開頭代號，留下公司名
        extra <- lab
        for (pat in unique(c(fetch_sym, disp_sym))) {
          if (!nzchar(pat)) next
          # perl=TRUE：TRE 字元類不可含裸 {}，否則 suggest UI 整段失敗
          esc <- gsub("([.\\^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", pat, perl = TRUE)
          extra <- sub(paste0("^", esc, "(\\s|[—\\-–])+"), "", extra, perl = TRUE)
        }
        extra <- sub("\\.(TW|TWO)\\s*[—\\-–]\\s*", "", extra, ignore.case = TRUE, perl = TRUE)
        extra <- trimws(extra)
        tags$button(
          type = "button",
          class = "ynow-suggest-item",
          `data-symbol` = disp_sym,
          tags$span(class = "ynow-suggest-sym", disp_sym),
          if (nzchar(extra) && !identical(toupper(extra), toupper(disp_sym)) &&
              !identical(toupper(extra), toupper(fetch_sym))) {
            tags$span(class = "ynow-suggest-lab", extra)
          }
        )
      })
    )
  }

  output$sc_ticker_suggest_ui <- renderUI({
    .ynow_ticker_suggest_div("sc_ticker_suggest", sc_datalist_choices(), market_mode())
  })
  # Keep suggest HTML warm while stock chrome is hidden (Home / Macro / Blue Chip).
  # Otherwise Shiny suspends the output and the first keystrokes after return to
  # Company/Dashboard hit an empty host until resume (~debounce + re-render).
  tryCatch(
    outputOptions(output, "sc_ticker_suggest_ui", suspendWhenHidden = FALSE),
    error = function(e) NULL
  )

  output$lab_cluster_focus_suggest_ui <- renderUI({
    mode <- market_mode()
    res <- tryCatch(lab_cluster_result(), error = function(e) NULL)
    # After a cluster run, suggest only names in the clustered set — market-wide
    # presets (e.g. 2881) are not radar-usable and made focus look "2330-only".
    ch <- NULL
    if (isTRUE(tryCatch(lab_cluster_has_result(res), error = function(e) FALSE))) {
      tks <- as.character(res$data$ticker)
      disp <- tryCatch(
        display_tickers_for_market(tks, mode),
        error = function(e) sub("\\.(TW|TWO)$", "", tks, ignore.case = TRUE)
      )
      nm <- if ("name" %in% names(res$data)) {
        as.character(res$data$name)
      } else {
        as.character(disp)
      }
      labs <- paste0(disp, " — ", nm)
      ch <- stats::setNames(as.character(tks), labs)
      q <- trimws(as.character(input$ticker_typeahead %||% "")[1])
      if (nzchar(q)) {
        keep <- grepl(q, disp, ignore.case = TRUE) |
          grepl(q, tks, ignore.case = TRUE) |
          grepl(q, nm, ignore.case = TRUE)
        ch <- ch[keep]
      }
    }
    if (is.null(ch) || !length(ch)) ch <- sc_datalist_choices()
    .ynow_ticker_suggest_div("lab_cluster_focus_suggest", ch, mode)
  })

  session$onFlushed(function() {
    sc_datalist_choices(TICKER_PRESETS)
    # Refresh chrome with current locale only — do NOT reset from market mode
    # (device_ui_locale may refine language shortly after; market never owns language).
    .push_ui_locale(isolate(ui_locale()), sync_picker = TRUE)
  }, once = TRUE)

  ticker_typeahead_q <- shiny::debounce(
    reactive({ input$ticker_typeahead }),
    millis = 280
  )

  observeEvent(ticker_typeahead_q(), {
    q <- trimws(as.character(ticker_typeahead_q() %||% ""))
    mode <- market_mode()
    if (!nzchar(q)) {
      base <- ticker_presets_for_market(mode)
      recent <- values$recentsearch
      if (length(recent)) {
        recent <- unique(toupper(trimws(recent)))
        recent_labs <- display_tickers_for_market(recent, mode)
        recent_named <- stats::setNames(recent, recent_labs)
        base <- c(recent_named, base[!(unname(base) %in% recent)])
      }
      sc_datalist_choices(base)
      return()
    }
    hits <- tryCatch(
      search_ticker_choices(q, market = mode),
      error = function(e) ticker_presets_for_market(mode)
    )
    sc_datalist_choices(hits)
  }, ignoreInit = TRUE)

  observeEvent(current_ticker(), {
    tk <- current_ticker()
    req(nzchar(tk))
    mode <- market_mode()
    base <- sc_datalist_choices()
    if (is.null(base)) base <- ticker_presets_for_market(mode)
    if (!(tk %in% unname(base))) {
      disp <- display_ticker_for_market(tk, mode)
      sc_datalist_choices(c(stats::setNames(tk, disp), base))
    }
    logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
    if (isTRUE(logged) && exists("ynow_prefs_update_last_ui_surface", mode = "function")) {
      uid <- tryCatch(.ynow_signed_in_uid(), error = function(e) "")
      if (nzchar(uid %||% "")) {
        tryCatch(
          ynow_prefs_update_last_ui_surface(uid, ticker = tk),
          error = function(e) NULL
        )
      }
    }
  }, ignoreInit = TRUE)
  
  # ==========================================
  # 🌐 核心爬蟲：只要中央大腦的代碼改變，就自動執行完整抓取
  # ==========================================
  # TW：.TW 抓取失敗時一次性改試 .TWO，避免 observe 無限迴圈
  .tw_two_fallback_tried_for <- reactiveVal(NA_character_)

  observeEvent(list(current_ticker(), ticker_search_gen()), {
    if (isTRUE(.ynow_search_gen_is_discard())) {
      # Focus cancel bumps gen without a replacement fetch. Clear busy here
      # because .search_done would keep the flag when the ticker is unchanged.
      tryCatch(ticker_search_busy(FALSE), error = function(e) NULL)
      tryCatch(
        .ynow_set_tab_busy(.ynow_busy_tab_now("dashboard"), FALSE),
        error = function(e) NULL
      )
      return()
    }
    req(current_ticker())
    # 換股票：回到基礎設定連動，讓新 Summary／Unlever 路徑可自動帶入 CAPM
    capm_beta_dirty(FALSE)
    beta_capm_driver("gs")
    stock_code <- current_ticker()
    tried_for <- .tw_two_fallback_tried_for()
    alt_of_tried <- if (!is.na(tried_for) && nzchar(tried_for)) {
      tw_yahoo_alt_ticker(tried_for)
    } else {
      NA_character_
    }
    is_fallback_retry <- !is.na(alt_of_tried) &&
      identical(toupper(stock_code), toupper(alt_of_tried))
    if (!is_fallback_retry) {
      .tw_two_fallback_tried_for(NA_character_)
    }
    
    my_gen <- isolate(as.integer(ticker_search_gen()) %||% 0L)
    busy_tab <- .ynow_busy_tab_now("dashboard")
    .ynow_set_tab_busy(busy_tab, TRUE)
    ticker_search_busy(TRUE)
    .search_alive <- function() {
      identical(isolate(as.integer(ticker_search_gen()) %||% 0L), my_gen) &&
        identical(isolate(current_ticker()), stock_code)
    }
    .search_done <- function() {
      # Clear when this run is still current. Also clear when the ticker was
      # dropped (market switch) so no later run will take the flag. A newer
      # symbol owns the flag itself. Focus discard also clears so an aborted
      # yield does not leave the Search button stuck.
      nxt <- tryCatch(isolate(current_ticker()), error = function(e) NULL)
      nxt_s <- as.character(nxt %||% "")[1]
      nxt_ok <- !is.null(nxt) && length(nxt_s) == 1L && !is.na(nxt_s) && nzchar(nxt_s)
      if (isTRUE(.search_alive()) || !nxt_ok || isTRUE(.ynow_search_gen_is_discard())) {
        tryCatch(.ynow_set_tab_busy(busy_tab, FALSE), error = function(e) NULL)
        tryCatch(ticker_search_busy(FALSE), error = function(e) NULL)
      }
    }
    active_prog <- NULL
    # shiny::incProgress only updates a withProgress stack frame, which does not survive later ticks.
    incProgress <- function(amount = 0.1, message = NULL, detail = NULL) {
      if (is.null(active_prog)) return(invisible(NULL))
      active_prog$inc(amount, message = message, detail = detail)
      invisible(NULL)
    }
    .mm <- function() isolate(market_mode())

    .run_search_phase1b <- function(prog, sum_df, q_ccy, f_ccy) {
      if (!isTRUE(.search_alive())) {
        tryCatch(prog$close(), error = function(e) NULL)
        .search_done()
        return(invisible(NULL))
      }
      active_prog <<- prog
      tryCatch({
        ind_info <- cached_get_yahoo_industry(stock_code)
        if (!is.null(ind_info)) {
          disp <- as.character(ind_info$display_text %||% "")[1]
          # TW: if Yahoo Sector/Industry is empty, fall back to exchange industry label
          if (identical(.mm(), "TW") &&
              (is.null(disp) || !nzchar(disp) || grepl("N/A", disp, fixed = TRUE)) &&
              exists("lookup_tw_universe_industry_raw", mode = "function")) {
            tw_raw <- tryCatch(
              lookup_tw_universe_industry_raw(stock_code),
              error = function(e) ""
            )
            if (nzchar(as.character(tw_raw %||% "")[1])) {
              disp <- paste0("TW industry: ", tw_raw)
              ind_info$display_text <- disp
              if (!nzchar(as.character(ind_info$industry %||% "")[1]) ||
                  identical(as.character(ind_info$industry)[1], "N/A")) {
                ind_info$industry <- tw_raw
              }
            }
          }
          corp_industry_text(disp)
          company_profile(list(
            ticker = stock_code,
            company_name = as.character(ind_info$company_name %||% "")[1],
            sector = as.character(ind_info$sector %||% "")[1],
            industry = as.character(ind_info$industry %||% "")[1],
            summary = as.character(ind_info$summary %||% "")[1],
            website = as.character(ind_info$website %||% "")[1],
            country = as.character(ind_info$country %||% "")[1],
            city = as.character(ind_info$city %||% "")[1],
            exchange = as.character(ind_info$exchange %||% "")[1],
            employees = suppressWarnings(as.numeric(ind_info$employees %||% NA_real_))
          ))
        }

        # Key dates (Yahoo) + TW board / CB open data — tied to Search ticker
        kd <- tryCatch(
          cached_fetch_company_key_dates(stock_code),
          error = function(e) NULL
        )
        company_key_dates(kd)
        mode_now <- tryCatch(
          normalize_market_mode(.mm()),
          error = function(e) "US"
        )
        if (identical(mode_now, "TW")) {
          company_board_election(tryCatch(
            cached_fetch_tw_board_election(stock_code),
            error = function(e) NULL
          ))
          fx_now <- tryCatch(
            suppressWarnings(as.numeric(isolate(fx_usd_twd()))[1]),
            error = function(e) NA_real_
          )
          sh_now <- suppressWarnings(as.numeric(kd$shares_outstanding %||% NA_real_)[1])
          company_cb_dilution(tryCatch(
            fetch_tw_convertible_bonds_live(
              stock_code,
              shares_out = sh_now,
              fx_usd_twd = fx_now
            ),
            error = function(e) NULL
          ))
        } else {
          company_board_election(tryCatch(
            fetch_us_board_election_stub(stock_code),
            error = function(e) NULL
          ))
          company_cb_dilution(tryCatch(
            fetch_us_cb_soft(
              stock_code,
              shares_out = suppressWarnings(as.numeric(kd$shares_outstanding %||% NA_real_)[1]),
              shares_diluted = suppressWarnings(as.numeric(kd$shares_diluted %||% NA_real_)[1])
            ),
            error = function(e) NULL
          ))
        }
        # Ownership Structure Analysis (Lite + Full Company tab)
        company_ownership(tryCatch({
          fetch_fn <- if (exists("cached_fetch_ownership_structure", mode = "function")) {
            cached_fetch_ownership_structure
          } else {
            fetch_ownership_structure
          }
          fetch_fn(stock_code, market = mode_now)
        }, error = function(e) NULL))
        own_selected_holder(NULL)
        own_holder_detail(NULL)
        # Always sync Dashboard「目前產業標準快覽」to the searched ticker's industry
        # (TW universe / US overlay·S&P / Yahoo heuristic). Unknown → keep prior pick.
        mapped <- tryCatch(
          resolve_industry_key_for_ticker(
            ticker = stock_code,
            market_mode = .mm(),
            display_text = if (!is.null(ind_info)) ind_info$display_text else "",
            sector = if (!is.null(ind_info)) ind_info$sector else "",
            industry = if (!is.null(ind_info)) ind_info$industry else ""
          ),
          error = function(e) ""
        )
        mapped <- as.character(mapped %||% "")[1]
        if (nzchar(mapped) && mapped %in% names(industry_standards)) {
          tryCatch(
            shinyWidgets::updatePickerInput(
              session, "industry_choice", selected = mapped
            ),
            error = function(e) NULL
          )
        }

        # Prefer full legal/display name from Summary or industry lookup (not ticker alone).
        .pick_company_name <- function(..., ticker = "") {
          cands <- unlist(list(...), use.names = FALSE)
          cands <- trimws(as.character(cands))
          cands <- cands[!is.na(cands) & nzchar(cands)]
          if (!length(cands)) {
            return(if (nzchar(ticker)) as.character(ticker) else "")
          }
          tk <- toupper(trimws(as.character(ticker)))
          non_tk <- cands[toupper(cands) != tk]
          pool <- if (length(non_tk)) non_tk else cands
          pool[[which.max(nchar(pool))]]
        }
        yahoo_cname <- attr(sum_df, "company_name")
        ind_cname <- if (!is.null(ind_info)) ind_info$company_name else NULL
        if (identical(.mm(), "TW")) {
          uni_zh <- tryCatch(
            lookup_tw_universe_company_name(stock_code),
            error = function(e) ""
          )
          parts <- split_corp_names_zh_en(
            yahoo_cname, ind_cname, uni_zh,
            ticker = stock_code,
            prefer_zh = uni_zh
          )
          if (nzchar(parts$zh) && nzchar(parts$en)) {
            corp_display_name(parts$zh)
            corp_display_name_en(parts$en)
          } else if (nzchar(parts$zh)) {
            corp_display_name(parts$zh)
            corp_display_name_en("")
          } else if (nzchar(parts$en)) {
            corp_display_name(parts$en)
            corp_display_name_en("")
          } else {
            corp_display_name(.pick_company_name(yahoo_cname, ind_cname, ticker = stock_code))
            corp_display_name_en("")
          }
        } else {
          corp_display_name(.pick_company_name(yahoo_cname, ind_cname, ticker = stock_code))
          corp_display_name_en("")
        }

        # Keep most-recent first; re-Search moves an existing ticker to the front.
        # Skip the recent list when the ticker is already pinned (shown above the label).
        # Signed-in: also persist into rolling 24h history (ynow_user_prefs).
        pinned_now <- as.character(isolate(values$pinned_search %||% character(0)))
        if (!(stock_code %in% pinned_now)) {
          recent_cap <- if (exists(".YNOW_RECENT_SEARCH_24H_MAX", inherits = TRUE)) {
            as.integer(.YNOW_RECENT_SEARCH_24H_MAX)[1]
          } else {
            20L
          }
          values$recentsearch <- head(
            c(stock_code, setdiff(isolate(values$recentsearch), stock_code)),
            recent_cap
          )
          uid_hist <- tryCatch(.ynow_pinned_auth_uid(), error = function(e) "")
          if (nzchar(uid_hist %||% "")) {
            persisted <- .ynow_recent_24h_record(uid_hist, stock_code)
            if (length(persisted)) {
              values$recentsearch <- head(
                setdiff(persisted, pinned_now),
                recent_cap
              )
            }
          }
        }
        # 成功載入後：輸入框維持顯示乾淨代號
        tryCatch(
          updateTextInput(
            session, "sc",
            value = display_ticker_for_market(stock_code, .mm())
          ),
          error = function(e) NULL
        )

        # Brand tint for company name + LAST PRICE / MARKET CAP / EPS icons.
        # Schedule off the financials critical path, but do NOT gate on phase2/3
        # success — a statements miss must not leave chrome stuck on default gray/green.
        .schedule_corp_logo_tint <- function(tk, website = NULL) {
          tk <- as.character(tk %||% "")[1]
          web <- trimws(as.character(website %||% "")[1])
          if (!nzchar(tk)) return(invisible(NULL))
          run_tint <- function() {
            tk_now <- tryCatch(
              as.character(isolate(current_ticker()) %||% "")[1],
              error = function(e) ""
            )
            if (!identical(toupper(tk_now), toupper(tk))) return(invisible(NULL))
            logo_cols <- tryCatch(
              cached_get_corp_logo_colors(
                tk,
                website = if (nzchar(web)) web else NULL
              ),
              error = function(e) character(0)
            )
            corp_name_logo_colors(.ensure_logo_color_pair(logo_cols))
            invisible(NULL)
          }
          if (requireNamespace("later", quietly = TRUE)) {
            later::later(function() {
              tryCatch(run_tint(), error = function(e) NULL)
            }, delay = 0)
          } else {
            tryCatch(run_tint(), error = function(e) NULL)
          }
          invisible(NULL)
        }
        logo_web <- tryCatch({
          p <- isolate(company_profile())
          as.character(p$website %||% "")[1]
        }, error = function(e) "")
        .schedule_corp_logo_tint(
          stock_code,
          website = if (nzchar(logo_web %||% "")) logo_web else NULL
        )

        if (!isTRUE(.search_alive())) {
          tryCatch(prog$close(), error = function(e) NULL)
          .search_done()
          return(invisible(NULL))
        }
        # Yield so sidebar_tabs / lazy mounts can start before financials scrape.
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(function() {
            tryCatch(
              .run_search_phase2(prog, sum_df, q_ccy, f_ccy),
              error = function(e) {
                tryCatch(prog$close(), error = function(e2) NULL)
                .search_done()
              }
            )
          }, delay = 0)
        } else {
          .run_search_phase2(prog, sum_df, q_ccy, f_ccy)
        }
        return(invisible(NULL))
      }, error = function(e) {
        fs_all_empty(FALSE)
        showNotification(
          .ui_msg("notif_fetch_fail", err = e$message),
          type = "error",
          duration = 12
        )
        tryCatch(prog$close(), error = function(e2) NULL)
        .search_done()
      })
      invisible(NULL)
    }

    .run_search_phase2 <- function(prog, sum_df, q_ccy, f_ccy) {
      if (!isTRUE(.search_alive())) {
        tryCatch(prog$close(), error = function(e) NULL)
        .search_done()
        return(invisible(NULL))
      }
      active_prog <<- prog
      tryCatch({
        incProgress(0.5, detail = "正在擷取財報明細（yfinance）...")
        # cached_scrape_financials already runs normalize_all_financials — do not re-normalize
        res <- cached_scrape_financials(stock_code)

        # Notify when ADR quoted ticker had empty Yahoo statements and we used
        # the HTCDI local ordinary listing (config-driven, not ticker-hardcoded).
        fb_src <- as.character(attr(res, "statement_source_ticker") %||% "")[1]
        fb_why <- as.character(attr(res, "statement_fallback") %||% "")[1]
        if (nzchar(fb_src) && grepl("local_ordinary", fb_why, fixed = TRUE)) {
          showNotification(
            paste0(
              "Quoted ticker statements were empty; loaded filings from local ordinary ",
              fb_src, "."
            ),
            type = "message", duration = 10, id = "ynow_fs_ordinary_fallback"
          )
        }

        # .TWO：Yahoo IS／BS 缺漏（含全空或部分空）→ 櫃買「財務資料簡報」補齊
        # （上櫃 O_／興櫃 U_）。只填空表、不捏造 CF；閘門用 should_try_* 而非要求三表全空。
        tpex_used <- FALSE
        if (exists("apply_tpex_financial_fallback", mode = "function") &&
            grepl("\\.TWO$", stock_code, ignore.case = TRUE) &&
            exists("should_try_tpex_financial_fallback", mode = "function") &&
            isTRUE(should_try_tpex_financial_fallback(stock_code, res))) {
          incProgress(0.55, detail = "Yahoo 年報缺漏，改試櫃買季報彙總…")
          fb <- tryCatch(
            apply_tpex_financial_fallback(res, stock_code),
            error = function(e) list(res = res, used = FALSE, error = conditionMessage(e))
          )
          if (isTRUE(fb$used)) {
            res <- fb$res
            tpex_used <- TRUE
            tw_tpex_fs_fallback(TRUE)
            showNotification(
              .ui_msg(
                "notif_data_gap_yahoo_tpex",
                board = if (identical(fb$meta$board %||% "", "ESB")) "ESB" else "OTC"
              ),
              type = "message", duration = 12, id = "ynow_tpex_fs_fallback"
            )
          } else if (nzchar(as.character(fb$error %||% "")[1])) {
            if (exists(".ynow_log", mode = "function")) {
              .ynow_log("ℹ️ TPEx fallback 未採用: ", fb$error)
            }
          }
        }
        scraped_financials(res)

        # 1A：財報寫入後延遲鎖定參數基準（等 WACC／SGR／模組自動帶入）
        if (isTRUE(isolate(user_has_searched()))) {
          .schedule_param_audit_baseline(2200L)
        }

        # P0／P2：IS／BS／CF 全空 → 明確提示（勿以為算完）；導向 MOPS／櫃買
        empty_fs <- exists("financials_is_bs_cf_all_empty", mode = "function") &&
          isTRUE(financials_is_bs_cf_all_empty(res))
        fs_all_empty(empty_fs)
        if (isTRUE(empty_fs)) {
          showNotification(
            .ui_msg("notif_data_gap_empty"),
            type = "warning", duration = 12, id = "ynow_fs_empty_gap"
          )
        } else if (isTRUE(tpex_used)) {
          # 摘要有資料但非完整三表：勿顯示「全空」紅標
          fs_all_empty(FALSE)
        }
        if (isTRUE(isolate(tw_is_esb()))) {
          showNotification(
            .ui_msg("notif_esb_beta_hint"),
            type = "message", duration = 10, id = "ynow_esb_board_note"
          )
        }

        # Prefer statement currency from financials _meta when Summary omitted it.
        meta_fc <- normalize_ccy(attr(res, "financialCurrency") %||% "")
        meta_qc <- normalize_ccy(attr(res, "currency") %||% "")
        if (!is.na(meta_fc) && (is.na(isolate(statement_currency())) || identical(isolate(statement_currency()), isolate(quote_currency())))) {
          if (!identical(meta_fc, isolate(quote_currency())) || is.na(isolate(statement_currency()))) {
            statement_currency(meta_fc)
            set_ynow_currency_context(
              isolate(session_currency()), isolate(fx_usd_twd()), isolate(quote_currency()), meta_fc
            )
          }
        }
        if (!is.na(meta_qc) && is.na(isolate(quote_currency()))) {
          quote_currency(meta_qc)
        }

        if (!isTRUE(.search_alive())) {
          tryCatch(prog$close(), error = function(e) NULL)
          .search_done()
          return(invisible(NULL))
        }
        # Yield before Rf and logo so sidebar_tabs can run after statements are stored.
        nxt <- function() .run_search_phase3(prog, sum_df, q_ccy, f_ccy, res)
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(function() {
            tryCatch(nxt(), error = function(e) {
              tryCatch(prog$close(), error = function(e2) NULL)
              .search_done()
            })
          }, delay = 0)
        } else {
          nxt()
        }
        return(invisible(NULL))

      }, error = function(e) {
        fs_all_empty(FALSE)
        # 興櫃旗標若已偵測仍保留短註（Summary 失敗也可能是興櫃）
        showNotification(
          .ui_msg("notif_fetch_fail", err = e$message),
          type = "error",
          duration = 12
        )
      })
      tryCatch(prog$close(), error = function(e) NULL)
      .search_done()
      invisible(NULL)
    }

    .run_search_phase3 <- function(prog, sum_df, q_ccy, f_ccy, res) {
      if (!isTRUE(.search_alive())) {
        tryCatch(prog$close(), error = function(e) NULL)
        .search_done()
        return(invisible(NULL))
      }
      active_prog <<- prog
      tryCatch({
        # 搜尋後：ADR／股數級距自動約當（市值÷報價股價）
        tryCatch({
          bs_df <- reorder_financial_columns(
            coerce_financial_df(res[["Balance Sheet"]]$expanded)
          )
          sh_res <- resolve_valuation_shares(
            bs_df, sum_df, ticker = stock_code,
            quote_currency = q_ccy, financial_currency = f_ccy
          )
          if (shares_auto_adjust_method(sh_res$method)) {
            msg <- sh_res$note
            if (is.null(msg) || !nzchar(msg)) {
              msg <- sprintf(
                "已自動換算報價股約當股數（方法：%s）",
                sh_res$method
              )
            }
            showNotification(
              msg, type = "message", duration = 8,
              id = "ynow_shares_adr_note"
            )
          }
        }, error = function(e) NULL)

        # 先更新即時 Rf，其餘 CAPM／WACC 在財報 reactive 就緒後自動估算
        tryCatch({
          rf_now <- cached_get_risk_free_rate()
          if (is.finite(rf_now) && rf_now > 0) {
            updateNumericInput(session, "capm_rf", value = round(as.numeric(rf_now), 2))
          }
        }, error = function(e) NULL)

        # Brand tint primary path is phase1b (independent of statements). Phase3
        # only backfills when the async tint still empty (e.g. website arrived late).
        have_logo <- tryCatch({
          length(.ensure_logo_color_pair(isolate(corp_name_logo_colors()))) >= 2L
        }, error = function(e) FALSE)
        if (!isTRUE(have_logo)) {
          logo_web <- tryCatch({
            p <- isolate(company_profile())
            as.character(p$website %||% "")[1]
          }, error = function(e) "")
          logo_cols <- tryCatch(
            cached_get_corp_logo_colors(
              stock_code,
              website = if (nzchar(logo_web %||% "")) logo_web else NULL
            ),
            error = function(e) character(0)
          )
          corp_name_logo_colors(.ensure_logo_color_pair(logo_cols))
        }

        incProgress(0.9, detail = "資料同步完成！✅")
      }, error = function(e) {
        fs_all_empty(FALSE)
        # 興櫃旗標若已偵測仍保留短註（Summary 失敗也可能是興櫃）
        showNotification(
          .ui_msg("notif_fetch_fail", err = e$message),
          type = "error",
          duration = 12
        )
      })
      tryCatch(prog$close(), error = function(e) NULL)
      .search_done()
      invisible(NULL)
    }

    .run_search_phase1 <- function() {
      if (!isTRUE(.search_alive())) {
        .search_done()
        return(invisible(NULL))
      }
      prog <- shiny::Progress$new(session, min = 0, max = 1)
      active_prog <<- prog
      prog$set(
        message = paste(
          "🚀 正在取得", display_ticker_for_market(stock_code, .mm()), "的最新資料..."
        ),
        value = 0
      )
      sum_df <- NULL
      q_ccy <- NA_character_
      f_ccy <- NA_character_
      tryCatch({
        # 換檔先清缺口旗標，避免殘留舊標的提示
        fs_all_empty(FALSE)
        tw_is_esb(FALSE)
        tw_tpex_fs_fallback(FALSE)
        corp_name_logo_colors(character(0))
        company_profile(NULL)
        company_key_dates(NULL)
        company_board_election(NULL)
        company_ownership(NULL)
        own_selected_holder(NULL)
        own_holder_detail(NULL)
        company_cb_dilution(NULL)

        # 興櫃：宇宙板別（ESB）；可估但 Summary β／Blue Chip 有限制
        if (identical(.mm(), "TW") &&
            exists("is_tw_esb_ticker", mode = "function") &&
            isTRUE(is_tw_esb_ticker(stock_code))) {
          tw_is_esb(TRUE)
        }

        incProgress(0.2, detail = "正在讀取 Summary（yfinance）...")
        sum_df <- tryCatch(cached_get_summary_data(stock_code), error = function(e) e)
        # TWSE 後綴無資料／失敗 → 試上櫃 .TWO（僅一次）
        if ((inherits(sum_df, "error") || !is.data.frame(sum_df) || nrow(sum_df) < 1L) &&
            identical(.mm(), "TW") &&
            grepl("\\.TW$", stock_code, ignore.case = TRUE) &&
            is.na(isolate(.tw_two_fallback_tried_for()))) {
          alt <- tw_yahoo_alt_ticker(stock_code)
          if (!is.na(alt) && nzchar(alt) && !identical(toupper(alt), toupper(stock_code))) {
            .tw_two_fallback_tried_for(toupper(stock_code))
            alt_disp <- display_ticker_for_market(alt, "TW")
            showNotification(
              paste0(
                display_ticker_for_market(stock_code, "TW"),
                " 無資料，改試上櫃代號 ", alt_disp, "…"
              ),
              type = "message", duration = 5
            )
            current_ticker(alt)
            tryCatch(updateTextInput(session, "sc", value = alt_disp), error = function(e) NULL)
            tryCatch(prog$close(), error = function(e) NULL)
            .search_done()
            return()
          }
        }
        if (inherits(sum_df, "error")) stop(sum_df$message)
        if (!is.data.frame(sum_df) || nrow(sum_df) < 1L) {
          stop("yfinance 回傳空的 summary 表")
        }
        summary_data(sum_df)

        q_ccy <- normalize_ccy(attr(sum_df, "currency") %||% "")
        f_ccy <- normalize_ccy(attr(sum_df, "financialCurrency") %||% "")
        if (is.na(q_ccy)) {
          q_ccy <- if (grepl("\\.(TW|TWO)$", stock_code, ignore.case = TRUE)) "TWD" else "USD"
        }
        # Missing reporting ccy: keep NA (do not copy quote — avoids TWD-as-USD for ADR).
        if (is.na(f_ccy) && grepl("\\.(TW|TWO)$", stock_code, ignore.case = TRUE)) {
          f_ccy <- "TWD"
        }
        quote_currency(q_ccy)
        statement_currency(f_ccy)
        sess <- default_session_currency(q_ccy, f_ccy, stock_code)
        session_currency(sess)
        fx_now <- tryCatch(cached_get_usd_twd_fx(), error = function(e) NA_real_)
        if (!is.finite(fx_now) || fx_now <= 0) fx_now <- NA_real_
        fx_usd_twd(fx_now)
        fx_fetched_at(Sys.time())
        set_ynow_currency_context(sess, fx_now, q_ccy, f_ccy)
        if (isTRUE(fx_conversion_required(q_ccy, f_ccy)) ||
            isTRUE(fx_conversion_required(sess, q_ccy))) {
          if (!is.finite(fx_now) || fx_now <= 0) {
            showNotification(
              .ui_msg("notif_fx_usd_twd_fail_keep"),
              type = "warning", duration = 10
            )
          }
        }
        shinyWidgets::updateRadioGroupButtons(
          session, "session_ccy_pick", selected = sess
        )

        if (!isTRUE(.search_alive())) {
          tryCatch(prog$close(), error = function(e) NULL)
          .search_done()
          return(invisible(NULL))
        }
        # Yield so a re-clicked Search can bump ticker_search_gen and preempt.
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(function() {
            tryCatch(
              .run_search_phase1b(prog, sum_df, q_ccy, f_ccy),
              error = function(e) {
                tryCatch(prog$close(), error = function(e2) NULL)
                .search_done()
              }
            )
          }, delay = 0)
          return(invisible(NULL))
        }

        .run_search_phase1b(prog, sum_df, q_ccy, f_ccy)
        return(invisible(NULL))
      }, error = function(e) {
        fs_all_empty(FALSE)
        # 興櫃旗標若已偵測仍保留短註（Summary 失敗也可能是興櫃）
        showNotification(
          .ui_msg("notif_fetch_fail", err = e$message),
          type = "error",
          duration = 12
        )
        tryCatch(prog$close(), error = function(e2) NULL)
        .search_done()
      })
      invisible(NULL)
    }

    if (requireNamespace("later", quietly = TRUE)) {
      later::later(function() {
        tryCatch(.run_search_phase1(), error = function(e) .search_done())
      }, delay = 0)
    } else {
      .run_search_phase1()
    }
  })

  # ==========================================
  # 💱 Session 幣別切換（USD ⇄ TWD）
  # ==========================================
  .refresh_fx_if_stale <- function(max_age_sec = .YNOW_CACHE_FAST_TTL_SEC) {
    if (!exists(".YNOW_CACHE_FAST_TTL_SEC")) max_age_sec <- 15L * 60L
    ts <- fx_fetched_at()
    stale <- is.null(ts) || !inherits(ts, "POSIXt") ||
      (as.numeric(difftime(Sys.time(), ts, units = "secs")) > max_age_sec)
    if (!stale) return(invisible(fx_usd_twd()))
    fx_now <- tryCatch(cached_get_usd_twd_fx(), error = function(e) NA_real_)
    if (!is.finite(fx_now) || fx_now <= 0) {
      showNotification(.ui_msg("notif_fx_usd_twd_fail"), type = "warning", duration = 8)
      return(invisible(fx_usd_twd()))
    }
    fx_usd_twd(fx_now)
    fx_fetched_at(Sys.time())
    fx_now
  }

  .apply_session_currency <- function(new_ccy) {
    sc <- normalize_ccy(new_ccy)
    if (is.na(sc) || !(sc %in% c("USD", "TWD"))) return()
    if (identical(sc, session_currency())) {
      set_ynow_currency_context(
        sc, fx_usd_twd(), quote_currency(), statement_currency()
      )
      return()
    }
    fx_now <- tryCatch(.refresh_fx_if_stale(), error = function(e) fx_usd_twd())
    if (!is.finite(fx_now) || fx_now <= 0) {
      q <- quote_currency(); f <- statement_currency()
      if (isTRUE(fx_conversion_required(sc, q)) ||
          isTRUE(fx_conversion_required(sc, f)) ||
          isTRUE(fx_conversion_required(q, f))) {
        showNotification(.ui_msg("notif_fx_convert_fail"), type = "warning", duration = 8)
      }
    }
    session_currency(sc)
    set_ynow_currency_context(
      sc, fx_now, quote_currency(), statement_currency()
    )
  }

  observeEvent(input$session_ccy_pick, {
    # 顯示幣別：只換匯，不改 UI 語言
    pick <- as.character(input$session_ccy_pick %||% "")[1]
    .apply_session_currency(pick)
  }, ignoreInit = TRUE)

  output$hdr_ccy_status <- renderText({
    loc <- ui_locale()
    q <- quote_currency() %||% "?"
    f <- statement_currency() %||% "?"
    sc <- session_currency() %||% "?"
    fx <- fx_usd_twd()
    fx_txt <- if (is.finite(fx) && fx > 0) {
      paste0("1 USD = ", round(as.numeric(fx), 2), " TWD")
    } else {
      ui_str("hdr_ccy_fx_missing", loc)
    }
    paste0(
      ui_str("hdr_ccy_quote", loc), " ", q, " · ",
      ui_str("hdr_ccy_stmt", loc), " ", f, " · ",
      ui_str("hdr_ccy_display", loc), " ", sc, " · ",
      fx_txt
    )
  })
  
  # ==========================================
  # 📊 1. 基本資訊與 Summary 介面輸出
  # ==========================================
  .corpname_logo_style <- function(cols, which = 1L) {
    cols <- tryCatch(.ensure_logo_color_pair(cols), error = function(e) character(0))
    if (!length(cols)) return(NULL)
    c1 <- cols[[1]]
    c2 <- cols[[2]]
    if (identical(as.integer(which)[1], 2L)) {
      return(sprintf("color:%s;", c2))
    }
    # 單行全稱：雙色水平漸層（單色→黑；純黑單色→白）
    paste0(
      "--ynow-corp-c1:", c1, ";",
      "--ynow-corp-c2:", c2, ";"
    )
  }

  render_corpname_ui <- function() {
    mode <- tryCatch(
      normalize_market_mode(market_mode()),
      error = function(e) "US"
    )
    nm <- trimws(as.character(corp_display_name() %||% "")[1])
    en <- trimws(as.character(corp_display_name_en() %||% "")[1])
    logo_cols <- tryCatch(
      .ensure_logo_color_pair(corp_name_logo_colors()),
      error = function(e) character(0)
    )
    if (!nzchar(nm)) {
      if (!is.null(summary_data())) {
        name <- attr(summary_data(), "company_name")
        if (!is.null(name) && !is.na(name) && nzchar(as.character(name)[1])) {
          nm <- as.character(name)[1]
        }
      }
      if (!nzchar(nm)) {
        tk <- current_ticker()
        if (!is.null(tk) && nzchar(tk)) {
          nm <- paste("Stock:", display_ticker_for_market(tk, mode))
        }
      }
    }
    if (!nzchar(nm) && !nzchar(en)) return(NULL)

    # 台股：中文上、英文下；品牌色系固定兩色分給中／英
    # （缺第二色→黑；sole black logo→白；白英文加陰影以免溶進白底）
    if (identical(mode, "TW") && nzchar(nm) && nzchar(en) &&
        isTRUE(query_has_cjk(nm)) && !isTRUE(query_has_cjk(en))) {
      zh_style <- if (length(logo_cols) >= 2L) {
        sprintf("color:%s;", logo_cols[[1]])
      } else {
        NULL
      }
      en_cls <- "ynow-corpname-en"
      en_style <- NULL
      if (length(logo_cols) >= 2L) {
        en_style <- sprintf("color:%s;", logo_cols[[2]])
        if (identical(logo_cols[[2]], "#FFFFFF")) {
          en_cls <- paste(en_cls, "ynow-corpname-en-on-white")
        }
      }
      return(tags$span(
        class = "ynow-corpname-stack",
        tags$span(
          class = "ynow-corpname-zh",
          style = zh_style,
          htmltools::htmlEscape(nm)
        ),
        tags$span(
          class = en_cls,
          style = en_style,
          htmltools::htmlEscape(en)
        )
      ))
    }

    single_cls <- "ynow-corpname-single"
    single_style <- NULL
    if (length(logo_cols) >= 2L) {
      single_cls <- paste(single_cls, "ynow-corpname-logo-grad")
      single_style <- .corpname_logo_style(logo_cols, which = 1L)
    }
    tags$span(
      class = single_cls,
      style = single_style,
      htmltools::htmlEscape(if (nzchar(nm)) nm else en)
    )
  }

  output$txt_corpname <- renderUI({ render_corpname_ui() })

  # Quote KPI icon fills share the company-name logo pair (c1 → c2).
  observe({
    cols <- tryCatch(
      .ensure_logo_color_pair(corp_name_logo_colors()),
      error = function(e) character(0)
    )
    ok <- length(cols) >= 2L && nzchar(cols[[1]]) && nzchar(cols[[2]])
    session$sendCustomMessage("ynowCorpLogoColors", list(
      c1 = if (ok) cols[[1]] else "",
      c2 = if (ok) cols[[2]] else ""
    ))
  })
  output$search_results <- renderText({ corp_industry_text() })

  # Dashboard：FINANCIAL REPORT 上方 — 個股基本資訊總說明（一秒認識公司）
  .fmt_company_employees <- function(n) {
    n <- suppressWarnings(as.numeric(n)[1])
    if (!is.finite(n) || n <= 0) return("")
    format(round(n), big.mark = ",", scientific = FALSE)
  }
  .company_hq_label <- function(city, country) {
    parts <- trimws(c(as.character(city %||% "")[1], as.character(country %||% "")[1]))
    parts <- parts[!is.na(parts) & nzchar(parts) & !parts %in% c("N/A", "None")]
    if (!length(parts)) return("")
    paste(parts, collapse = ", ")
  }
  .website_host <- function(url) {
    u <- trimws(as.character(url %||% "")[1])
    if (!nzchar(u)) return("")
    u2 <- sub("^https?://", "", u, ignore.case = TRUE)
    u2 <- sub("/.*$", "", u2)
    u2 <- sub("^www\\.", "", u2, ignore.case = TRUE)
    if (nzchar(u2)) u2 else u
  }
  .na_profile_val <- function(x) {
    s <- trimws(as.character(x %||% "")[1])
    if (!nzchar(s) || s %in% c("N/A", "None", "Unknown Sector", "Unknown Industry")) {
      return("")
    }
    s
  }

  ynow_news_now <- reactive({
    tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    empty <- list(
      ok = FALSE, score = NA_real_, n = 0L, flags = character(), nudges = character(),
      rows = data.frame(title = character(), score = numeric(), stringsAsFactors = FALSE)
    )
    if (!nzchar(tk)) return(empty)
    tryCatch(ynow_news_bundle(tk), error = function(e) empty)
  })

  output$ynow_news_thermometer <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    pack <- ynow_news_now()
    score <- suppressWarnings(as.numeric(pack$score)[1])
    tone_cls <- if (!is.finite(score)) {
      "ynow-thermo-empty"
    } else if (score <= -0.25) {
      "ynow-thermo-flame"
    } else if (score >= 0.25) {
      "ynow-thermo-ocean"
    } else {
      "ynow-thermo-mix"
    }
    tone <- if (!is.finite(score)) {
      ui_str("news_thermo_empty", loc)
    } else if (score <= -0.25) {
      ui_str("news_thermo_pessimistic", loc)
    } else if (score >= 0.25) {
      ui_str("news_thermo_optimistic", loc)
    } else {
      ui_str("news_thermo_neutral", loc)
    }
    pct <- if (is.finite(score)) round((score + 1) / 2 * 100) else 50
    n_head <- suppressWarnings(as.integer(pack$n)[1])
    if (!is.finite(n_head) || n_head < 0L) n_head <- 0L
    flag_lab <- c(
      management = ui_str("news_flag_management", loc),
      sec = ui_str("news_flag_sec", loc),
      lawsuit = ui_str("news_flag_lawsuit", loc)
    )
    flags <- intersect(as.character(pack$flags %||% character()), names(flag_lab))
    nudges <- as.character(pack$nudges %||% character())
    nudge_txt <- character()
    if ("capex" %in% nudges) nudge_txt <- c(nudge_txt, ui_str("news_nudge_capex", loc))
    if ("dividend" %in% nudges) nudge_txt <- c(nudge_txt, ui_str("news_nudge_dividend", loc))
    tags$div(
      class = "ynow-full-only",
      style = "margin: 8px 0 14px; padding: 10px 12px; border: 1px solid #d9d9d9; border-radius: 6px;",
      tags$b(ui_str("news_thermo_title", loc)),
      tags$span(class = paste("ynow-thermo-tone", tone_cls), tone),
      if (is.finite(score)) tags$span(style = "margin-left: 8px;", sprintf("%+.2f", score)),
      tags$div(
        class = paste("ynow-thermo-track", tone_cls),
        tags$div(class = "ynow-thermo-marker", style = sprintf("left:%s%%;", pct))
      ),
      if (is.finite(score) && n_head > 0L) tags$p(
        style = "margin: 6px 0 0; color: #666; font-size: 12px;",
        sprintf(ui_str("news_thermo_n", loc), n_head)
      ),
      tags$p(style = "margin: 6px 0 0; color: #666; font-size: 12px;", ui_str("news_thermo_method", loc)),
      if (length(flags)) tags$p(
        style = "margin: 6px 0 0; color: #c62828;",
        paste(ui_str("news_flag_title", loc), paste(flag_lab[flags], collapse = " · "))
      ),
      if (length(nudge_txt)) tags$ul(
        lapply(nudge_txt, function(x) tags$li(style = "font-size: 13px;", x))
      )
    )
  })

  output$company_overview_ui <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    loc <- normalize_ui_locale(loc)
    prof <- company_profile()
    title <- ui_str("company_overview_title", loc)
    source_lab <- ui_str("company_overview_source", loc)
    mode_ov <- tryCatch(
      normalize_market_mode(market_mode()),
      error = function(e) "US"
    )
    date_chips <- tryCatch(
      .ynow_key_date_chips_ui(company_key_dates(), market_mode = mode_ov, loc = loc),
      error = function(e) NULL
    )

    if (is.null(prof) || !is.list(prof)) {
      return(tags$div(
        class = "ynow-company-overview",
        tags$div(
          class = "ynow-company-overview-head",
          tags$div(
            class = "ynow-company-overview-head-main",
            tags$h3(class = "ynow-company-overview-title", title),
            date_chips
          ),
          tags$p(class = "ynow-company-overview-source", source_lab)
        ),
        tags$p(
          class = "ynow-company-overview-waiting",
          ui_str("company_overview_waiting", loc)
        )
      ))
    }

    sector <- .na_profile_val(prof$sector)
    industry <- .na_profile_val(prof$industry)
    hq <- .company_hq_label(prof$city, prof$country)
    exchange <- .na_profile_val(prof$exchange)
    employees <- .fmt_company_employees(prof$employees)
    website <- .na_profile_val(prof$website)
    if (nzchar(website) && !grepl("^https?://", website, ignore.case = TRUE)) {
      website_href <- paste0("https://", website)
    } else {
      website_href <- website
    }
    summary <- trimws(as.character(prof$summary %||% "")[1])
    if (identical(summary, "None")) summary <- ""

    chip <- function(k, v, href = NULL) {
      if (!nzchar(v)) return(NULL)
      val_node <- if (!is.null(href) && nzchar(href)) {
        tags$a(class = "v", href = href, target = "_blank", rel = "noopener noreferrer", v)
      } else {
        tags$span(class = "v", v)
      }
      tags$div(class = "ynow-company-overview-chip", tags$span(class = "k", k), val_node)
    }

    chips <- Filter(Negate(is.null), list(
      chip(ui_str("company_overview_sector", loc), sector),
      chip(ui_str("company_overview_industry", loc), industry),
      chip(ui_str("company_overview_hq", loc), hq),
      chip(ui_str("company_overview_exchange", loc), exchange),
      chip(ui_str("company_overview_employees", loc), employees),
      chip(
        ui_str("company_overview_website", loc),
        .website_host(website),
        href = if (nzchar(website_href)) website_href else NULL
      )
    ))

    summary_long <- nchar(summary, type = "chars") > 320L
    summary_node <- if (!nzchar(summary)) {
      tags$p(
        class = "ynow-company-overview-summary ynow-company-overview-waiting",
        ui_str("company_overview_summary_empty", loc)
      )
    } else if (summary_long) {
      uid <- paste0("ynow_co_sum_", as.integer(runif(1, 1e6, 1e9 - 1)))
      more_lab <- ui_str("company_overview_show_more", loc)
      less_lab <- ui_str("company_overview_show_less", loc)
      tagList(
        tags$p(
          id = uid,
          class = "ynow-company-overview-summary ynow-co-clamp",
          summary
        ),
        tags$button(
          type = "button",
          class = "ynow-company-overview-toggle",
          `data-more` = more_lab,
          `data-less` = less_lab,
          onclick = paste0(
            "(function(btn){",
            "var p=document.getElementById('", uid, "');",
            "if(!p)return;",
            "var open=p.classList.toggle('ynow-co-clamp')===false;",
            "btn.textContent=open?(btn.getAttribute('data-less')||'Show less'):(btn.getAttribute('data-more')||'Show more');",
            "})(this);"
          ),
          more_lab
        )
      )
    } else {
      tags$p(class = "ynow-company-overview-summary", summary)
    }

    tags$div(
      class = "ynow-company-overview",
      tags$div(
        class = "ynow-company-overview-head",
        tags$div(
          class = "ynow-company-overview-head-main",
          tags$h3(class = "ynow-company-overview-title", title),
          date_chips
        ),
        tags$p(class = "ynow-company-overview-source", source_lab)
      ),
      if (length(chips)) tags$div(class = "ynow-company-overview-meta", chips),
      summary_node
    )
  })

  # 股權結構分析（Company → Board / CB，Convertible bonds — dilution 下方；Lite + Full）
  observeEvent(input$own_holder_click, {
    click <- input$own_holder_click
    nm <- if (is.list(click)) {
      as.character(click$name %||% "")[1]
    } else {
      as.character(click %||% "")[1]
    }
    nm <- trimws(nm)
    if (!nzchar(nm)) return()
    own_selected_holder(nm)
    snap <- tryCatch(company_ownership(), error = function(e) NULL)
    tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    mode_now <- tryCatch(
      normalize_market_mode(market_mode()),
      error = function(e) "US"
    )
    holders <- if (is.list(snap)) snap$holders else NULL
    own_holder_detail(tryCatch(
      ownership_institution_detail(
        nm,
        subject_ticker = tk,
        subject_holders = holders,
        market = mode_now
      ),
      error = function(e) NULL
    ))
  }, ignoreNULL = TRUE)

  output$ownership_structure_top_ui <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    loc <- normalize_ui_locale(loc)
    snap <- tryCatch(company_ownership(), error = function(e) NULL)
    .ynow_ownership_top_ui(snap, loc = loc)
  })

  output$ownership_structure_holders_ui <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    loc <- normalize_ui_locale(loc)
    snap <- tryCatch(company_ownership(), error = function(e) NULL)
    detail <- tryCatch(own_holder_detail(), error = function(e) NULL)
    sel <- tryCatch(as.character(own_selected_holder() %||% "")[1], error = function(e) "")
    .ynow_ownership_holders_ui(
      snap, detail = detail, loc = loc, selected_name = sel
    )
  })

  output$own_volume_plot <- renderPlotly({
    req(identical(.ynow_sidebar_tab(), "dashboard"))
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    loc <- normalize_ui_locale(loc)
    snap <- tryCatch(company_ownership(), error = function(e) NULL)
    vol <- if (is.list(snap)) snap$volume else NULL
    empty_title <- ui_str("own_volume_empty", loc)
    if (is.null(vol) || !is.data.frame(vol) || !nrow(vol)) {
      return(plotly::plotly_empty() %>% plotly::layout(title = empty_title))
    }
    plotly::plot_ly(
      vol,
      x = ~Date,
      y = ~Volume,
      type = "scatter",
      mode = "lines",
      line = list(color = "#0A72AE", width = 1.5),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d}<br>",
        ui_str("own_volume_title", loc),
        ": %{y:,.0f}<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = list(text = ui_str("own_volume_title", loc), font = list(size = 14)),
        xaxis = list(title = ""),
        yaxis = list(title = ui_str("own_volume_title", loc), tickformat = "~s"),
        margin = list(l = 50, r = 20, t = 40, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  # 董監改選／可轉債稀釋（Company → FINANCIAL REPORT → Board / CB；ownership 接在下方）
  output$corp_events_ui <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    loc <- normalize_ui_locale(loc)
    mode_ce <- tryCatch(
      normalize_market_mode(market_mode()),
      error = function(e) "US"
    )
    tk <- tryCatch(current_ticker(), error = function(e) NULL)
    board <- company_board_election()
    cb <- company_cb_dilution()
    # Recompute TW CB dilution when bridge shares become available after FS load
    if (identical(mode_ce, "TW") && !is.null(tk) && nzchar(as.character(tk)[1])) {
      sh_bridge <- tryCatch({
        sb <- .shared_bridge()
        suppressWarnings(as.numeric(sb$shares)[1])
      }, error = function(e) NA_real_)
      kd <- company_key_dates()
      sh_yahoo <- suppressWarnings(as.numeric(kd$shares_outstanding %||% NA_real_)[1])
      sh_use <- if (is.finite(sh_bridge) && sh_bridge > 0) sh_bridge else sh_yahoo
      fx_now <- tryCatch(
        suppressWarnings(as.numeric(fx_usd_twd())[1]),
        error = function(e) NA_real_
      )
      need_refresh <- is.null(cb) ||
        !isTRUE(cb$has_data) ||
        identical(cb$note, "shares_missing") ||
        (is.finite(sh_use) && is.finite(cb$shares_out) &&
           abs(sh_use - cb$shares_out) / max(sh_use, 1) > 0.02) ||
        (is.finite(sh_use) && (!is.finite(cb$shares_out) || cb$shares_out <= 0))
      if (isTRUE(need_refresh)) {
        cb2 <- tryCatch(
          fetch_tw_convertible_bonds_live(tk, shares_out = sh_use, fx_usd_twd = fx_now),
          error = function(e) cb
        )
        if (!is.null(cb2)) {
          company_cb_dilution(cb2)
          cb <- cb2
        }
      }
    }
    if (is.null(tk) || !nzchar(as.character(tk)[1])) {
      return(tags$div(
        class = "ynow-corp-events",
        tags$p(class = "ynow-corp-events-waiting", ui_str("corp_events_waiting", loc))
      ))
    }
    tagList(
      tags$p(
        style = "margin:0 0 12px 0; color:#666; font-size:13px; line-height:1.5;",
        ui_str("corp_events_intro", loc)
      ),
      .ynow_corp_events_panel_ui(board, cb, market_mode = mode_ce, loc = loc)
    )
  })

  # P0／P1a／P2：公司標題下方資料缺口／興櫃／櫃買摘要 fallback 短註 banner
  output$ynow_data_gap_banner <- renderUI({
    empty_fs <- isTRUE(fs_all_empty())
    esb <- isTRUE(tw_is_esb())
    tpex_fb <- isTRUE(tw_tpex_fs_fallback())
    if (!empty_fs && !esb && !tpex_fb) return(NULL)

    blocks <- list()
    if (empty_fs) {
      mops_url <- if (exists("YNOW_MOPS_HOME_URL", inherits = TRUE)) {
        YNOW_MOPS_HOME_URL
      } else {
        "https://mops.twse.com.tw/"
      }
      tpex_url <- if (exists("YNOW_TPEX_HOME_URL", inherits = TRUE)) {
        YNOW_TPEX_HOME_URL
      } else {
        "https://www.tpex.org.tw/"
      }
      blocks[[length(blocks) + 1L]] <- tags$div(
        class = "ynow-data-gap-empty-fs",
        style = paste0(
          "margin: 6px 0 8px 0; padding: 10px 12px; border-left: 4px solid #c0392b;",
          " background: #fdf2f2; color: #5a1a1a; font-size: 13px; line-height: 1.55;"
        ),
        tags$b("Yahoo 尚無年報，基本面模型不可用。"),
        " IS／BS／CF 皆空（常見於新上櫃／新掛牌）；請勿將空結果視為已完成估值。",
        tags$br(),
        "完整財報請至",
        tags$a(href = mops_url, target = "_blank", rel = "noopener noreferrer",
               "公開資訊觀測站（MOPS）"),
        "／",
        tags$a(href = tpex_url, target = "_blank", rel = "noopener noreferrer",
               "櫃買中心"),
        "查詢。"
      )
    }
    if (tpex_fb && !empty_fs) {
      sum_url <- if (exists("YNOW_TPEX_FINANCIAL_SUMMARY_URL", inherits = TRUE)) {
        YNOW_TPEX_FINANCIAL_SUMMARY_URL
      } else {
        "https://www.tpex.org.tw/zh-tw/mainboard/listed/financial/summary.html"
      }
      blocks[[length(blocks) + 1L]] <- tags$div(
        class = "ynow-data-gap-tpex-fs",
        style = paste0(
          "margin: 6px 0 8px 0; padding: 10px 12px; border-left: 4px solid #2980b9;",
          " background: #f0f7fc; color: #1a3a5a; font-size: 13px; line-height: 1.55;"
        ),
        tags$b("已改用櫃買「財務資料簡報」摘要："),
        " Yahoo 年報三表為空時，以官方上櫃／興櫃季報彙總補齊營收、營業利益、稅後純益、股本、EPS、每股淨值等。",
        "此非完整 IS／BS／CF（現金流量表仍空；CapEx／FCF／現金／負債等未提供項目不會捏造）。",
        "可支援 P/B、RI 與部分簡單投入；完整年報請至 MOPS／",
        tags$a(href = sum_url, target = "_blank", rel = "noopener noreferrer",
               "櫃買財務資料簡報"),
        "核對。"
      )
    }
    if (esb) {
      blocks[[length(blocks) + 1L]] <- tags$div(
        class = "ynow-data-gap-esb",
        style = paste0(
          "margin: 0 0 8px 0; padding: 8px 12px; border-left: 4px solid #f39c12;",
          " background: #fff8ef; color: #5a3a10; font-size: 12.5px; line-height: 1.5;"
        ),
        tags$b("興櫃（ESB）："),
        "可估，但 Yahoo Summary β 常缺；Blue Chip 僅上市＋上櫃、不納興櫃。",
        "建議改用產業／手動／Rolling β（標明為工程啟發式）。"
      )
    }
    do.call(tagList, blocks)
  })

  .capm_rf_source_note_ui <- function() {
    prof <- tryCatch(active_market_profile(), error = function(e) NULL)
    note <- if (!is.null(prof)) as.character(prof$data_source_note_zh %||% "")[1] else ""
    if (!nzchar(note)) {
      note <- as.character(prof$rf_label_zh %||% "")[1]
    }
    if (!nzchar(note)) return(NULL)
    helpText(style = "margin-top:-6px; margin-bottom:8px; font-size:12px;", note)
  }
  output$capm_rf_source_note <- renderUI({ .capm_rf_source_note_ui() })
  output$ddm_capm_rf_source_note <- renderUI({ .capm_rf_source_note_ui() })

  output$wacc_tax_source_note <- renderUI({
    mode <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
    if (!identical(mode, "TW")) return(NULL)
    helpText(
      style = "margin-top:-6px; margin-bottom:8px; font-size:12px;",
      "台股市場預設法定 T＝20%（可覆寫）；估值主源 Yahoo。"
    )
  })

  # Page-pin tray removed (v25.93 misread). Stock pins 4×3 + tab reorder-edit.
  output$pinned_sidebar_tabs <- renderUI({
    NULL
  })

  # Pinned tickers from Recent Search: 4×3 grid (max 12).
  output$pinned_recent_search <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    mode <- tryCatch(
      normalize_market_mode(market_mode()),
      error = function(e) "US"
    )
    raw <- as.character(values$pinned_search %||% character(0))
    raw <- if (exists("ynow_prefs_normalize_tickers", mode = "function")) {
      ynow_prefs_normalize_tickers(raw)
    } else {
      unique(raw[nzchar(raw) & !is.na(raw)])
    }
    if (!length(raw)) return(NULL)
    pin_hint <- tryCatch(
      ui_str("recent_search_unpin_hint", loc),
      error = function(e) "Hold 2 seconds to unpin"
    )
    section_lab <- tryCatch(
      ui_str("recent_search_pinned_label", loc),
      error = function(e) "Pinned tickers"
    )
    chips <- lapply(raw, function(fetch_tk) {
      disp <- display_ticker_for_market(fetch_tk, mode)
      if (!nzchar(disp)) disp <- fetch_tk
      aria <- tryCatch(
        {
          msg <- ui_str("recent_search_aria", loc)
          paste0(gsub("{ticker}", disp, msg, fixed = TRUE), " — ", pin_hint)
        },
        error = function(e) disp
      )
      tags$button(
        type = "button",
        class = "ynow-pinned-chip",
        `data-ticker` = fetch_tk,
        title = paste(aria, pin_hint, sep = " · "),
        `aria-label` = aria,
        tags$span(class = "ynow-pinned-chip-text", disp)
      )
    })
    tags$div(
      class = "ynow-pinned-search",
      id = "ynow_pinned_recent_search",
      role = "list",
      `aria-label` = section_lab,
      tags$div(class = "ynow-pinned-search-label", section_lab),
      chips
    )
  })

  output$recentsearch <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    mode <- tryCatch(
      normalize_market_mode(market_mode()),
      error = function(e) "US"
    )
    signed_in <- tryCatch({
      nzchar(.ynow_pinned_auth_uid() %||% "")
    }, error = function(e) FALSE)
    hint_24h <- tryCatch(
      ui_str("recent_search_24h_hint", loc),
      error = function(e) "Signed-in search history is kept for 24 hours."
    )
    # Keep the static label title in sync (24h hint for signed-in users).
    tryCatch(
      session$sendCustomMessage(
        "ynowRecentSearchLabel",
        list(
          title = if (isTRUE(signed_in)) hint_24h else "",
          hint = if (isTRUE(signed_in)) hint_24h else ""
        )
      ),
      error = function(e) NULL
    )
    pinned <- unique(as.character(values$pinned_search %||% character(0)))
    raw <- unique(as.character(values$recentsearch %||% character(0)))
    raw <- raw[nzchar(raw) & !is.na(raw)]
    # Pinned items render above the label — keep them out of the recent list.
    if (length(pinned)) raw <- setdiff(raw, pinned)
    if (!length(raw)) {
      empty_key <- if (isTRUE(signed_in)) "recent_search_24h_empty" else "recent_search_empty"
      return(tags$p(
        class = "ynow-recent-search-empty",
        title = if (isTRUE(signed_in)) hint_24h else NULL,
        ui_str(empty_key, loc)
      ))
    }
    pin_hint <- tryCatch(
      ui_str("recent_search_pin_hint", loc),
      error = function(e) "Hold 2 seconds to pin"
    )
    chips <- lapply(raw, function(fetch_tk) {
      disp <- display_ticker_for_market(fetch_tk, mode)
      if (!nzchar(disp)) disp <- fetch_tk
      aria <- tryCatch(
        {
          msg <- ui_str("recent_search_aria", loc)
          paste0(gsub("{ticker}", disp, msg, fixed = TRUE), " — ", pin_hint)
        },
        error = function(e) disp
      )
      chip_title <- paste(aria, pin_hint, sep = " · ")
      if (isTRUE(signed_in)) chip_title <- paste(chip_title, hint_24h, sep = " · ")
      tags$button(
        type = "button",
        class = "ynow-recent-chip",
        `data-ticker` = fetch_tk,
        title = chip_title,
        `aria-label` = aria,
        disp
      )
    })
    tags$div(
      class = "ynow-recent-search",
      role = "list",
      title = if (isTRUE(signed_in)) hint_24h else NULL,
      chips
    )
  })
  output$today <- renderText({ format(Sys.Date(), "%Y/%m/%d") })

  output$dashboard_selected_industry <- renderUI({
    ui_locale()
    fp <- tryCatch(fundamental_profile_rec(), error = function(e) NULL)
    fp_id <- as.character(fp$profile %||% "")[1]
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    fp_lab <- ""
    fp_why <- ""
    if (nzchar(fp_id)) {
      fp_lab <- if (exists("ui_str", mode = "function")) {
        tryCatch({
          k <- paste0("fund_profile_", fp_id)
          s <- ui_str(k)
          if (identical(s, k) || !nzchar(s)) {
            .fundamental_profile_label_fallback(fp_id, loc)
          } else {
            s
          }
        }, error = function(e) .fundamental_profile_label_fallback(fp_id, loc))
      } else {
        .fundamental_profile_label_fallback(fp_id, loc)
      }
      fp_why <- if (exists("ui_str", mode = "function")) {
        tryCatch({
          k <- paste0("fund_profile_why_", fp_id)
          s <- ui_str(k)
          if (identical(s, k) || !nzchar(s)) {
            .fundamental_profile_why_fallback(fp_id, loc)
          } else {
            s
          }
        }, error = function(e) .fundamental_profile_why_fallback(fp_id, loc))
      } else {
        .fundamental_profile_why_fallback(fp_id, loc)
      }
    }
    industry_standard_snapshot_ui(
      industry_key = input$industry_choice,
      yahoo_text = corp_industry_text(),
      show_chips = FALSE,
      show_title = FALSE,
      embedded = TRUE,
      empty_message = if (exists("ui_str", mode = "function")) {
        tryCatch(ui_str("industry_picker_empty"), error = function(e) {
          "尚未選擇比較產業（請於上方產業選單選取）。"
        })
      } else {
        "尚未選擇比較產業（請於上方產業選單選取）。"
      },
      profile_id = if (nzchar(fp_id)) fp_id else NULL,
      profile_label = if (nzchar(fp_lab)) fp_lab else NULL,
      profile_title = if (nzchar(fp_why)) fp_why else NULL
    )
  })

  output$dashboard_industry_metric_chips <- renderUI({
    ui_locale()
    input$industry_choice
    industry_standard_metric_chips_ui(input$industry_choice)
  })
  
  output$ibx_stockprice <- renderUI({
    df <- summary_data()
    loc <- ui_locale()
    session_currency(); fx_usd_twd(); quote_currency()
    tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    items <- if (!is.null(df) && is.data.frame(df)) as.character(df$Item) else character()
    pick_nm <- NA_character_
    pick_raw <- NA_character_
    for (nm in c("Market Price", "Last Price", "Previous Close")) {
      hit <- which(items == nm)
      if (!length(hit)) next
      raw <- as.character(df$Value[hit[1]])[1]
      if (nzchar(raw) && !identical(raw, "N/A")) {
        pick_nm <- nm
        pick_raw <- raw
        break
      }
    }
    val <- if (!is.na(pick_nm)) {
      convert_summary_value_display(
        pick_nm, pick_raw,
        quote_currency(), session_currency(), fx_usd_twd()
      )
    } else {
      "N/A"
    }
    px_num <- suppressWarnings(as.numeric(tryCatch(scraped_market_cap()$price, error = function(e) NA_real_))[1])
    if (!is.finite(px_num) && !is.na(pick_raw)) {
      px_num <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", as.character(pick_raw)[1]))[1])
    }
    ready <- isTRUE(user_has_searched()) && is.finite(px_num) && px_num > 0 &&
      nzchar(tk) && !identical(val, "N/A")
    ynow_alertable(
      infoBox(
        ui_str("kpi_last_price", loc),
        val,
        icon = icon("chart-line"),
        color = "purple",
        width = NULL
      ),
      metric = "last_price",
      ticker = tk,
      value = px_num,
      label = ui_str("kpi_last_price", loc),
      ready = ready,
      default_kind = "price_lte"
    )
  })
  
  output$ibx_marketcap <- renderInfoBox({
    df <- summary_data()
    session_currency(); fx_usd_twd(); quote_currency()
    val <- if (!is.null(df) && "Market Cap (intraday)" %in% df$Item) {
      convert_summary_value_display(
        "Market Cap (intraday)", df$Value[df$Item == "Market Cap (intraday)"][1],
        quote_currency(), session_currency(), fx_usd_twd()
      )
    } else "N/A"
    infoBox("Market Cap", val, icon = icon("globe"), color = "aqua")
  })
  
  output$ibx_EPS <- renderUI({
    df <- summary_data()
    session_currency(); fx_usd_twd(); quote_currency()
    loc <- ui_locale()
    tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    raw <- if (!is.null(df) && "EPS (TTM)" %in% df$Item) {
      df$Value[df$Item == "EPS (TTM)"][1]
    } else {
      NA
    }
    val <- if (!is.null(df) && "EPS (TTM)" %in% df$Item) {
      convert_summary_value_display(
        "EPS (TTM)", raw,
        quote_currency(), session_currency(), fx_usd_twd()
      )
    } else {
      "N/A"
    }
    eps_num <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", as.character(raw)[1]))[1])
    ready <- isTRUE(user_has_searched()) && is.finite(eps_num) && nzchar(tk) && !identical(val, "N/A")
    ynow_alertable(
      infoBox("EPS (TTM)", val, icon = icon("dollar-sign"), color = "green", width = NULL),
      metric = "eps_ttm",
      ticker = tk,
      value = eps_num,
      label = "EPS (TTM)",
      ready = ready,
      default_kind = "value_lte"
    )
  })
  
  output$fs_summary_ui <- renderUI({
    req(summary_data())
    session_currency(); fx_usd_twd(); quote_currency(); market_mode(); ui_locale()
    df <- summary_data()
    if (is.null(df) || nrow(df) < 1) {
      return(tags$p("No finance summary available.", style = "color:#888;"))
    }

    # 分組僅影響版面；所有 Item/Value 皆會輸出（未歸類者歸入 Other）
    groups <- list(
      Price = c("Market Price", "Previous Close", "Open", "Bid", "Ask", "Day's Range", "52 Week Range"),
      Volume = c("Volume", "Avg. Volume"),
      Valuation = c("Market Cap (intraday)", "Beta (5Y Monthly)", "PE Ratio (TTM)", "EPS (TTM)", "Target Est"),
      Dividend = c("Dividend", "Yield")
    )
    known <- unique(unlist(groups, use.names = FALSE))
    leftover <- setdiff(as.character(df$Item), known)
    if (length(leftover) > 0) groups$Other <- leftover

    use_zh_fs <- should_localize_fs_zh_tw(mode = market_mode(), locale = ui_locale())
    fp <- tryCatch(fundamental_profile_rec(), error = function(e) NULL)
    fp_id <- as.character(fp$profile %||% "")[1]
    focus_tip <- tryCatch(ui_str("kpi_legend_focus_metric"), error = function(e) "本財報屬性關鍵指標")
    tk_fs <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    searched_fs <- isTRUE(user_has_searched())
    mk_card <- function(item, value, raw_item = item, raw_value = NA) {
      mid <- if (exists("fs_item_to_focus_metric", mode = "function")) {
        fs_item_to_focus_metric(raw_item)
      } else {
        NA_character_
      }
      is_focus <- nzchar(fp_id) && !is.na(mid) &&
        exists("is_profile_focus_metric", mode = "function") &&
        isTRUE(is_profile_focus_metric(fp_id, mid))
      card <- tags$div(
        class = paste(c("ynow-fs-card", if (is_focus) "ynow-focus-metric-box"), collapse = " "),
        title = if (is_focus) focus_tip else NULL,
        `aria-label` = if (is_focus) focus_tip else NULL,
        tags$div(class = "ynow-fs-label", item),
        tags$div(class = "ynow-fs-value", value)
      )
      if (!isTRUE(is_focus)) return(card)
      num <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", as.character(raw_value)[1]))[1])
      ynow_alertable(
        card,
        metric = paste0("fund_focus_", as.character(mid)[1]),
        ticker = tk_fs,
        value = num,
        label = paste(focus_tip, item, sep = " · "),
        ready = searched_fs && is.finite(num) && nzchar(tk_fs),
        default_kind = "value_lte"
      )
    }

    sections <- lapply(names(groups), function(gname) {
      items <- groups[[gname]]
      rows <- df[match(items, df$Item), , drop = FALSE]
      rows <- rows[!is.na(rows$Item), , drop = FALSE]
      if (nrow(rows) < 1) return(NULL)
      tags$div(
        class = "ynow-fs-section",
        tags$div(
          class = "ynow-fs-section-title",
          localize_summary_section_zh_tw(gname, enabled = use_zh_fs)
        ),
        tags$div(
          class = "ynow-fs-grid",
          lapply(seq_len(nrow(rows)), function(i) {
            disp <- convert_summary_value_display(
              rows$Item[i], rows$Value[i],
              quote_currency(), session_currency(), fx_usd_twd()
            )
            mk_card(
              localize_summary_item_zh_tw(rows$Item[i], enabled = use_zh_fs),
              disp,
              raw_item = rows$Item[i],
              raw_value = rows$Value[i]
            )
          })
        )
      )
    })

    tags$div(
      class = "ynow-fs-wrap",
      tags$p(
        style = "margin:0 0 8px 0; font-size:11px; color:#888;",
        paste0("金額已換算為 session 幣別：", money_label(session_currency()),
               "（報價 ", quote_currency(), " → 顯示 ", session_currency(), "）")
      ),
      sections
    )
  })

  # 保留表格輸出供下載／相容（不在 UI 顯示）
  output$tbFinanceSummary <- renderDataTable({
    req(summary_data())
    datatable(summary_data(), options = list(pageLength = 20, dom = 't', scrollX = TRUE), rownames = TRUE)
  })
  
  # ==========================================
  # 📑 2. 三大財報資料分發與顯示
  # ==========================================
  # 「測試」回測濾鏡：目前公司 KPI vs 持倉回測條件門檻
  bt_filter_state <- reactiveVal(NULL)

  observeEvent(input$bt_kpi_filter, {
    # Toggle: when a filter result is showing, second click clears it
    st0 <- bt_filter_state()
    if (!is.null(st0)) {
      bt_filter_state(NULL)
      showNotification(.ui_msg("notif_bt_filter_cancel"), type = "message", duration = 4)
      return()
    }
    is_df <- tryCatch(d_income_statement(), error = function(e) NULL)
    cf_df <- tryCatch(d_cash_flow(), error = function(e) NULL)
    if (is.null(is_df) || is.null(cf_df)) {
      bt_filter_state(list(status = "empty", message = "尚無財報資料，請先搜尋並載入公司。"))
      showNotification(.ui_msg("notif_bt_filter_no_fs"), type = "warning", duration = 6)
      return()
    }
    metrics <- tryCatch(
      compute_dashboard_filter_metrics(is_df, cf_df),
      error = function(e) NULL
    )
    if (is.null(metrics)) {
      bt_filter_state(list(status = "empty", message = "指標計算失敗。"))
      showNotification(.ui_msg("notif_bt_filter_fail"), type = "error", duration = 6)
      return()
    }
    thr <- list(
      bt_net_margin = input$bt_net_margin,
      bt_rev_growth = input$bt_rev_growth,
      bt_eps_growth = input$bt_eps_growth,
      bt_fcf_cv = input$bt_fcf_cv
    )
    ev <- evaluate_holding_filter(metrics, thr)
    bt_filter_state(list(
      status = if (isTRUE(ev$overall)) "pass" else "fail",
      eval = ev,
      metrics = metrics,
      ticker = current_ticker() %||% ""
    ))
    showNotification(
      if (isTRUE(ev$overall)) .ui_msg("notif_bt_filter_pass")
      else .ui_msg("notif_bt_filter_fail_soft"),
      type = if (isTRUE(ev$overall)) "message" else "warning",
      duration = 7
    )
  })

  output$bt_filter_badge <- renderUI({
    st <- bt_filter_state()
    if (is.null(st)) {
      return(tags$span(
        style = "font-size:12px;color:#888;padding:4px 10px;border:1px solid #ddd;border-radius:4px;background:#f7f7f7;",
        "尚未比對"
      ))
    }
    if (identical(st$status, "empty")) {
      return(tags$span(
        style = "font-size:12px;font-weight:600;color:#666;padding:4px 10px;border:1px solid #ccc;border-radius:4px;background:#eee;",
        "尚無資料"
      ))
    }
    if (identical(st$status, "pass")) {
      tags$span(
        style = "font-size:12px;font-weight:700;color:#fff;padding:4px 12px;border-radius:4px;background:#1e8449;",
        "達標"
      )
    } else {
      tags$span(
        style = "font-size:12px;font-weight:700;color:#fff;padding:4px 12px;border-radius:4px;background:#c0392b;",
        "未達標"
      )
    }
  })

  output$bt_filter_detail <- renderUI({
    st <- bt_filter_state()
    if (is.null(st) || identical(st$status, "empty")) {
      if (!is.null(st) && !is.null(st$message)) {
        return(tags$div(
          style = "margin:0 0 12px 0;padding:8px 12px;background:#f5f5f5;border-left:3px solid #999;font-size:12px;color:#555;",
          st$message
        ))
      }
      return(NULL)
    }
    rows <- st$eval$rows
    fmt <- function(x) {
      if (is.null(x) || length(x) < 1 || is.na(x) || !is.finite(x)) return("N/A")
      sprintf("%.2f%%", as.numeric(x))
    }
    cells <- lapply(rows, function(r) {
      ok <- isTRUE(r$pass)
      tags$tr(
        tags$td(r$label),
        tags$td(fmt(r$actual)),
        tags$td(paste0(r$op, " ", fmt(r$threshold))),
        tags$td(
          style = if (ok) "color:#1e8449;font-weight:600;" else "color:#c0392b;font-weight:600;",
          if (ok) "過" else "未過"
        )
      )
    })
    tags$div(
      style = "margin:0 0 14px 0;padding:10px 12px;background:#f8fafc;border:1px solid #dce3ea;border-radius:4px;",
      tags$div(
        style = "font-size:12px;color:#444;margin-bottom:6px;",
        tags$b("回測濾鏡明細"),
        if (nzchar(st$ticker %||% "")) paste0(" · ", st$ticker) else NULL,
        "（對照「持倉回測條件」門檻；虧損期淨利率／NI 成長可放寬，同回測引擎）"
      ),
      tags$table(
        style = "width:100%;font-size:12px;border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th("指標"), tags$th("實際"), tags$th("門檻"), tags$th("結果")
        )),
        tags$tbody(cells)
      )
    )
  })
  
  d_income_statement <- reactive({
    req(scraped_financials())
    session_currency(); fx_usd_twd()
    df <- reorder_financial_columns(scraped_financials()[["Income Statement"]]$expanded)
    scale_financial_df_money(df, statement_currency(), session_currency(), fx_usd_twd())
  })
  d_balance_sheet <- reactive({
    req(scraped_financials())
    session_currency(); fx_usd_twd()
    df <- reorder_financial_columns(scraped_financials()[["Balance Sheet"]]$expanded)
    scale_financial_df_money(df, statement_currency(), session_currency(), fx_usd_twd())
  })
  d_cash_flow <- reactive({
    req(scraped_financials())
    session_currency(); fx_usd_twd()
    df <- reorder_financial_columns(scraped_financials()[["Cash Flow"]]$expanded)
    scale_financial_df_money(df, statement_currency(), session_currency(), fx_usd_twd())
  })

  # ==========================================
  # 📌 v13.0：分類 → 主／副模型（側邊欄標主「推薦」、副「備選」，並上浮至 Appr. 父層）
  # ==========================================
  .empty_model_rec <- function(summary_method, reason, company_type = "pending",
                               reason_en = NULL, summary_method_en = NULL) {
    list(
      company_type = company_type, primary = "", secondary = NULL, reference = NULL,
      ddm = FALSE, dcf = FALSE, pb = FALSE, ri = FALSE, nav = FALSE, tags = character(0),
      summary_method = summary_method,
      summary_method_en = summary_method_en %||% summary_method,
      reason = reason,
      reason_en = reason_en %||% reason,
      suggest_two_stage = FALSE,
      confidence_inputs = list(data_complete = FALSE)
    )
  }

  model_sidebar_rec <- reactive({
    loc_now <- tryCatch(ui_locale(), error = function(e) "en")
    if (!isTRUE(user_has_searched())) {
      return(.empty_model_rec(
        ui_str("model_rec_empty_summary", "zh-TW"),
        ui_str("model_rec_empty_reason", "zh-TW"),
        reason_en = ui_str("model_rec_empty_reason", "en"),
        summary_method_en = ui_str("model_rec_empty_summary", "en")
      ))
    }
    if (isTRUE(fs_all_empty())) {
      return(.empty_model_rec(
        ui_str("model_rec_no_yahoo_summary", "zh-TW"),
        ui_str("model_rec_no_yahoo_reason", "zh-TW"),
        company_type = "fallback",
        reason_en = ui_str("model_rec_no_yahoo_reason", "en"),
        summary_method_en = ui_str("model_rec_no_yahoo_summary", "en")
      ))
    }
    if (isTRUE(tw_tpex_fs_fallback())) {
      # CF 必空；以 IS／BS 摘要決定 P/B／RI（不開 DCF）
      is <- tryCatch(d_income_statement(), error = function(e) NULL)
      bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      ni <- tryCatch(
        select_current_metric_any(is, NET_INCOME_PATTERNS, "flow"),
        error = function(e) NA_real_
      )
      equity <- tryCatch(
        select_current_metric_any(bs, EQUITY_PATTERNS, "stock"),
        error = function(e) NA_real_
      )
      has_roe <- is.finite(ni) && is.finite(equity) && equity > 0
      has_bv <- is.finite(equity) && equity > 0
      list(
        company_type = "fallback",
        primary = if (isTRUE(has_roe)) "ri" else "pb",
        secondary = if (isTRUE(has_roe) && isTRUE(has_bv)) "pb" else NULL,
        reference = NULL,
        ddm = FALSE, dcf = FALSE,
        pb = TRUE,
        ri = isTRUE(has_roe),
        tags = c(
          if (isTRUE(has_roe)) "ri",
          "pb",
          "tpex_summary"
        ),
        summary_method = ui_str("model_rec_tpex_summary", "zh-TW"),
        summary_method_en = ui_str("model_rec_tpex_summary", "en"),
        reason = ui_str("model_rec_tpex_reason", "zh-TW"),
        reason_en = ui_str("model_rec_tpex_reason", "en"),
        suggest_two_stage = FALSE,
        confidence_inputs = list(
          fcf_cv = NA_real_, div_cv = NA_real_,
          has_fcf = FALSE, has_div = FALSE,
          has_roe = isTRUE(has_roe),
          data_complete = FALSE,
          source = "tpex_financial_summary"
        )
      )
    } else {
    cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
    is <- tryCatch(d_income_statement(), error = function(e) NULL)
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    ind <- corp_industry_text()
    if (is.null(cf) || !is.data.frame(cf) || nrow(cf) == 0) {
      return(.empty_model_rec(
        ui_str("model_rec_waiting_fs_summary", "zh-TW"),
        ui_str("model_rec_waiting_fs_reason", "zh-TW"),
        company_type = "fallback",
        reason_en = ui_str("model_rec_waiting_fs_reason", "en"),
        summary_method_en = ui_str("model_rec_waiting_fs_summary", "en")
      ))
    }
    rec_out <- recommend_valuation_models(
      cf,
      industry_text = ind,
      d_is = is,
      d_bs = bs,
      industry_choice = input$industry_choice,
      wacc_pct = tryCatch(.current_wacc_pct(), error = function(e) NA_real_),
      lifecycle_result = tryCatch(central_lifecycle(), error = function(e) NULL),
      selected_stage = input$lifecycle_stage %||% "auto",
      locale = loc_now
    )
    rec_out
    }
  })

  # 財報屬性分群（與產業正交；供 KPI 琥珀點／Annotation／Get Started）
  fundamental_profile_rec <- reactive({
    if (!isTRUE(user_has_searched())) {
      return(list(
        profile = "fallback",
        signals = list(),
        reason = "請先搜尋載入財報。",
        focus_metrics = character(0)
      ))
    }
    if (!exists("classify_fundamental_profile", mode = "function")) {
      return(list(profile = "fallback", signals = list(), reason = "", focus_metrics = character(0)))
    }
    cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
    is <- tryCatch(d_income_statement(), error = function(e) NULL)
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    classify_fundamental_profile(
      d_cf = cf, d_is = is, d_bs = bs,
      industry_text = tryCatch(corp_industry_text(), error = function(e) ""),
      industry_choice = input$industry_choice
    )
  })

  # Dynamic 「推薦」／「備選」／「參照」— primary + secondary + Multiples Reference
  sidebar_badge_sig <- reactiveVal("")
  .push_sidebar_badges <- function(force = FALSE) {
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    if (is.null(rec)) return(invisible(FALSE))
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    prim <- as.character(rec$primary %||% "")[1]
    sec <- as.character(rec$secondary %||% "")[1]
    ref <- as.character(rec$reference %||% "")[1]
    if (is.na(prim)) prim <- ""
    if (is.na(sec)) sec <- ""
    if (is.na(ref)) ref <- ""
    # Harden: Multiples never wears the secondary badge
    if (identical(sec, "multiples")) sec <- ""
    sig <- paste(prim, sec, ref, loc, rec$company_type %||% "", sep = "|")
    if (!isTRUE(force) && identical(sidebar_badge_sig(), sig)) return(invisible(FALSE))
    sidebar_badge_sig(sig)
    role_of <- function(key) {
      if (nzchar(prim) && identical(prim, key)) return("primary")
      if (nzchar(sec) && identical(sec, key)) return("secondary")
      ""
    }
    meth <- tryCatch({
      if (exists("rel_results", inherits = TRUE) && !is.null(rel_results$active_method_key)) {
        as.character(rel_results$active_method_key())[1]
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)
    fam <- if (exists(".rel_method_family", mode = "function")) .rel_method_family(meth) else NA_character_
    mult_tab <- switch(
      as.character(fam %||% ""),
      enterprise = "rel_mult_ev",
      revenue = "rel_mult_rev",
      "rel_multiples_calculator"
    )
    payload <- list(
      labels = list(
        primary = ui_str("menu_badge_primary", loc),
        secondary = ui_str("menu_badge_secondary", loc),
        reference = ui_str("menu_badge_reference", loc)
      ),
      dcf_calculator = list(role = role_of("dcf")),
      ddm_calculator = list(role = role_of("ddm")),
      pb_calculator = list(role = role_of("pb")),
      ri_calculator = list(role = role_of("ri")),
      nav_calculator = list(role = role_of("nav")),
      rel_mult_ev = list(role = if (identical(mult_tab, "rel_mult_ev")) role_of("multiples") else ""),
      rel_multiples_calculator = list(role = if (identical(mult_tab, "rel_multiples_calculator")) role_of("multiples") else ""),
      rel_mult_rev = list(role = if (identical(mult_tab, "rel_mult_rev")) role_of("multiples") else ""),
      sotp_calculator = list(role = role_of("sotp"))
    )
    session$sendCustomMessage("ynowSidebarBadges", payload)
    invisible(TRUE)
  }
  observe({
    model_sidebar_rec()
    ui_locale()
    .push_sidebar_badges(force = FALSE)
  })
  # Client handler may register after the first push — re-send when JS pings ready
  observeEvent(input$ynow_sidebar_badges_ready, {
    session$onFlushed(function() {
      .push_sidebar_badges(force = TRUE)
    }, once = TRUE)
    .push_sidebar_badges(force = TRUE)
  }, ignoreNULL = TRUE)
  # After Search completes, force a badge refresh (empty → primary mapping)
  observeEvent(user_has_searched(), {
    if (!isTRUE(user_has_searched())) return()
    session$onFlushed(function() {
      .push_sidebar_badges(force = TRUE)
    }, once = TRUE)
  }, ignoreInit = TRUE)

  # Growth classification → Full：僅提示 Two-Stage；Lite 由智慧分析自動套用
  observeEvent(model_sidebar_rec(), {
    if (isTRUE(isolate(lite_mode()))) return()
    rec <- model_sidebar_rec()
    if (!isTRUE(rec$suggest_two_stage)) return()
    if (identical(input$dcf_mode, "two_stage")) return()
    showNotification(
      .ui_msg("notif_suggest_two_stage"),
      type = "message", duration = 6, id = "ynow_suggest_two_stage"
    )
  }, ignoreInit = TRUE)

  output$get_started_model_selector <- renderUI({
    rec <- model_sidebar_rec()
    prim <- as.character(rec$primary %||% "")
    sec <- as.character(rec$secondary %||% "")
    mark_roles <- nzchar(prim)
    .ms_key_to_tab <- function(key) {
      switch(
        as.character(key %||% "")[1],
        nav = "nav_calculator",
        dcf = "dcf_calculator",
        ddm = "ddm_calculator",
        ri = "ri_calculator",
        pb = "pb_calculator",
        multiples = "rel_multiples_calculator",
        enterprise = "rel_mult_ev",
        revenue = "rel_mult_rev",
        sotp = "sotp_calculator",
        NULL
      )
    }
    make_card <- function(title, key, icon_name, color, formula, notes,
                          role_override = NULL, role_kind = NULL, tab_override = NULL) {
      # Only label 主模型／副模型／參照; no「備選」chip on remaining cards
      role <- if (!is.null(role_override)) {
        role_override
      } else if (!mark_roles) {
        NULL
      } else if (identical(key, prim)) {
        "主模型"
      } else if (nzchar(sec) && identical(key, sec)) {
        "副模型"
      } else {
        NULL
      }
      kind <- if (!is.null(role_kind)) {
        role_kind
      } else if (identical(role, "主模型")) {
        "primary"
      } else if (identical(role, "副模型")) {
        "secondary"
      } else {
        NULL
      }
      active <- identical(kind, "primary")
      is_secondary <- identical(kind, "secondary")
      is_ref <- identical(kind, "reference")
      border_col <- if (isTRUE(active)) "var(--ynow-card-accent)" else if (is_secondary) "#888" else if (is_ref) "#17a2b8" else "#ddd"
      bg <- if (isTRUE(active)) "#fffaf2" else if (is_secondary) "#f7f9fc" else if (is_ref) "#f3fbfc" else "#fff"
      tab <- if (!is.null(tab_override)) tab_override else .ms_key_to_tab(key)
      jump_title <- tryCatch({
        tpl <- .ui_msg("ms_card_jump_title")
        gsub("{model}", as.character(title)[1], tpl, fixed = TRUE)
      }, error = function(e) paste0("Open ", title))
      card_classes <- paste(
        "ynow-model-card",
        paste0("ynow-model-card--", key),
        if (isTRUE(active)) "ynow-model-card--primary" else "",
        if (is_secondary) "ynow-model-card--secondary" else "",
        if (is_ref) "ynow-model-card--reference" else ""
      )
      tags$div(
        class = paste("ynow-model-card-col", if (isTRUE(active)) "ynow-model-rec-active" else ""),
        tags$div(
          class = card_classes,
          role = "button",
          tabindex = "0",
          `data-tab` = tab,
          `data-model-key` = key,
          `data-model-color` = color,
          title = jump_title,
          `aria-label` = jump_title,
          style = paste0(
            "border:1px solid ", border_col, ";",
            "border-radius:8px; padding:14px; min-height:170px; background:", bg,
            "; box-shadow:0 2px 4px rgba(0,0,0,0.04); height:100%;",
            "cursor:pointer;"
          ),
          tags$div(class = "ynow-model-card__icon", style = "font-size:22px;", icon(icon_name)),
          tags$h4(style = "margin:8px 0 4px 0; font-weight:700;", title),
          if (!is.null(role)) tags$span(
            class = "ynow-model-card__badge",
            style = paste0(
              "display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px; color:#fff;",
              if (is_secondary) " background:#6c757d;" else if (is_ref) " background:#17a2b8;" else ""
            ),
            role
          ),
          tags$p(style = "margin:10px 0 4px 0; font-size:12px; color:#555;", formula),
          tags$p(style = "margin:0; font-size:12px; color:#777; line-height:1.4;", notes)
        )
      )
    }

    type_lab <- switch(
      as.character(rec$company_type %||% ""),
      "financial" = "金融／帳面驅動",
      "holding_asset" = "控股／資產導向",
      "growth" = "高成長",
      "mature" = "成熟穩定",
      "fallback" = "資料受限",
      "pending" = "待搜尋",
      "待分類"
    )
    fp <- tryCatch(fundamental_profile_rec(), error = function(e) NULL)
    fp_id <- as.character(fp$profile %||% "fallback")[1]
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    fp_lab <- if (exists("ui_str", mode = "function")) {
      tryCatch({
        k <- paste0("fund_profile_", fp_id)
        s <- ui_str(k)
        if (identical(s, k) || !nzchar(s)) {
          .fundamental_profile_label_fallback(fp_id, loc)
        } else {
          s
        }
      }, error = function(e) .fundamental_profile_label_fallback(fp_id, loc))
    } else if (exists(".fundamental_profile_label_fallback", mode = "function")) {
      .fundamental_profile_label_fallback(fp_id, loc)
    } else {
      fp_id
    }
    fp_why <- if (exists("ui_str", mode = "function")) {
      tryCatch({
        k <- paste0("fund_profile_why_", fp_id)
        s <- ui_str(k)
        if (identical(s, k) || !nzchar(s)) {
          .fundamental_profile_why_fallback(fp_id, loc)
        } else {
          s
        }
      }, error = function(e) .fundamental_profile_why_fallback(fp_id, loc))
    } else if (exists(".fundamental_profile_why_fallback", mode = "function")) {
      .fundamental_profile_why_fallback(fp_id, loc)
    } else {
      as.character(fp$reason %||% "")[1]
    }

    tagList(
      tags$style(HTML("
        .ynow-model-rec-active { transform: translateY(-2px); }
        .ynow-model-selector-summary {
          margin-bottom: 14px; padding: 10px 12px; border-left: 4px solid #222222;
          background: #f5f5f5; color: #333; font-size: 13px; line-height: 1.5;
        }
        /* 八卡：桌面可換行，小螢幕兩欄／單欄；點擊跳模型分頁 */
        .ynow-model-selector-row {
          display: flex;
          flex-wrap: wrap;
          align-items: stretch;
          margin-left: -7.5px;
          margin-right: -7.5px;
        }
        .ynow-model-selector-row > .ynow-model-card-col {
          flex: 1 1 11%;
          min-width: 110px;
          width: auto;
          float: none;
          padding-left: 7.5px;
          padding-right: 7.5px;
          box-sizing: border-box;
          margin-bottom: 10px;
        }
        .ynow-model-card[data-tab] {
          transition: box-shadow 0.15s ease, transform 0.15s ease, border-color 0.15s ease;
        }
        .ynow-model-card[data-tab]:hover,
        .ynow-model-card[data-tab]:focus-visible {
          box-shadow: 0 4px 14px rgba(0,0,0,0.12);
          transform: translateY(-1px);
          outline: none;
        }
        @media (max-width: 991px) {
          .ynow-model-selector-row > .ynow-model-card-col {
            flex: 1 1 45%;
          }
        }
        @media (max-width: 767px) {
          .ynow-model-selector-row > .ynow-model-card-col {
            flex: 1 1 100%;
          }
        }
      ")),
      tags$div(
        class = "ynow-model-selector-summary",
        tags$b("公司分類："),
        if (exists(".ynow_data_limited_flow_span", mode = "function")) {
          .ynow_data_limited_flow_span(type_lab)
        } else {
          type_lab
        },
        if (mark_roles) tagList(
          tags$span(style = "margin:0 8px; color:#bbb;", "|"),
          tags$b("主模型："), .model_label(prim),
          if (nzchar(sec)) tagList(
            tags$span(style = "margin:0 8px; color:#bbb;", "|"),
            tags$b("副模型："), .model_label(sec)
          )
        ) else tagList(
          tags$span(style = "margin:0 8px; color:#bbb;", "|"),
          tags$b("主模型："), "尚未標示"
        ),
        tags$br(),
        tags$b(id = "ynow_fund_profile_label", "財報屬性："),
        if (exists(".ynow_data_limited_flow_span", mode = "function")) {
          # Resolved profiles (Mature dividend, …) → logo-flow; Data-limited stays plain.
          .ynow_data_limited_flow_span(fp_lab, force = !identical(fp_id, "fallback"))
        } else {
          fp_lab
        },
        tags$span(style = "margin:0 8px; color:#bbb;", "|"),
        tags$span(style = "color:#555;", fp_why),
        tags$br(),
        tags$span(
          if (exists(".pick_rec_reason", mode = "function")) {
            .pick_rec_reason(rec, loc)
          } else {
            as.character(rec$reason %||% ui_str("model_rec_empty_reason", loc))[1]
          }
        )
      ),
      # L→R 對齊側欄：NAV → Justified P/B → DCF → DDM → RI → Multiples → SOTP
      tags$div(
        class = "ynow-model-selector-row",
        make_card("NAV", "nav", "sitemap", "#d81b60", "P = NAVPS × NAV multiple", "控股／綜合：帳面控股 NAV；無需 SGR。"),
        make_card(
          "Justified P/B", "pb", "landmark", "#1a5276",
          ui_str("ms_card_pb_formula", loc),
          ui_str("ms_card_pb_notes", loc)
        ),
        make_card("DCF", "dcf", "calculator", "#00a65a", "FCFF／WACC 或 FCFE／Ke", "適合 FCF 為正且相對穩定的企業。"),
        make_card("DDM", "ddm", "hand-holding-usd", "#f39c12", "Gordon／SPM／二階段 P0 = PV(股利)", "適合持續且穩定配息的企業。"),
        make_card("RI", "ri", "gem", "#605ca8", "Value = Book Value + Σ Residual Income / (1+Ke)^t", "適合帳面價值與 ROE 具參考性的企業。"),
        {
          meth <- tryCatch({
            if (exists("rel_results", inherits = TRUE) && !is.null(rel_results$active_method_key)) {
              as.character(rel_results$active_method_key())[1]
            } else {
              NA_character_
            }
          }, error = function(e) NA_character_)
          # A finished family run wins. Before that, the recommendation lens names the family.
          if (!nzchar(meth %||% "") || is.na(meth)) {
            meth <- as.character(rec$multiples_method %||% "")[1]
          }
          fam_id <- if (exists(".rel_method_family", mode = "function")) .rel_method_family(meth) else NA_character_
          if (!nzchar(fam_id %||% "") || is.na(fam_id)) {
            fam_id <- as.character(rec$multiples_family %||% "earnings")[1]
          }
          fam_lab <- function(fid) {
            ui_str(switch(
              fid,
              enterprise = "ms_family_enterprise",
              revenue = "ms_family_revenue",
              book = "ms_family_book",
              "ms_family_earnings"
            ), loc)
          }
          tag <- if (exists(".rel_method_chart_tag", mode = "function")) .rel_method_chart_tag(meth) else NA_character_
          fam_txt <- fam_lab(fam_id)
          if (!is.na(tag) && nzchar(tag %||% "")) fam_txt <- paste0(fam_txt, " · ", tag)
          ref_key <- as.character(rec$reference %||% "")[1]
          # Justified P/B is its own Fair Value card; Multiples never inherits pb as primary.
          role_kind <- if (identical(prim, "multiples")) {
            "primary"
          } else if (identical(sec, "multiples")) {
            "secondary"
          } else if (identical(ref_key, "multiples")) {
            "reference"
          } else {
            NULL
          }
          role_lab <- if (identical(role_kind, "primary")) {
            "主模型"
          } else if (identical(role_kind, "secondary")) {
            "副模型"
          } else if (identical(role_kind, "reference")) {
            ui_str("ms_card_role_reference", loc)
          } else {
            NULL
          }
          if (!is.null(role_lab)) role_lab <- paste0(role_lab, " · ", fam_txt)
          note_bits <- character(0)
          if (identical(prim, "multiples")) {
            note_bits <- c(note_bits, paste0(ui_str("ms_pos_primary", loc), " · ", fam_txt))
          }
          if (identical(sec, "multiples")) {
            note_bits <- c(note_bits, paste0(ui_str("ms_pos_secondary", loc), " · ", fam_txt))
          }
          if (identical(ref_key, "multiples") && !identical(prim, "multiples")) {
            note_bits <- c(note_bits, paste0(ui_str("ms_pos_reference", loc), " · ", fam_txt))
          }
          if (!length(note_bits)) note_bits <- ui_str("ms_card_multiples_notes", loc)
          jump_tab <- switch(
            fam_id,
            enterprise = "rel_mult_ev",
            revenue = "rel_mult_rev",
            book = "pb_calculator",
            "rel_multiples_calculator"
          )
          make_card(
            "Multiples", "multiples", "percentage", "#17a2b8",
            ui_str("ms_card_multiples_formula", loc),
            paste(note_bits, collapse = " · "),
            role_override = role_lab,
            role_kind = role_kind,
            tab_override = jump_tab
          )
        },
        make_card(
          "SOTP", "sotp", "puzzle-piece", "#343a40",
          ui_str("ms_card_sotp_formula", loc),
          ui_str("ms_card_sotp_notes", loc)
        )
      ),
      {
        ref_key <- as.character(rec$reference %||% "")[1]
        cur_px <- suppressWarnings(as.numeric(tryCatch(scraped_market_cap()$price, error = function(e) NA_real_))[1])
        prim_px <- if (nzchar(prim)) suppressWarnings(as.numeric(tryCatch(.model_point(prim), error = function(e) NA_real_))[1]) else NA_real_
        sec_px <- if (nzchar(sec) && !identical(sec, "multiples")) suppressWarnings(as.numeric(tryCatch(.model_point(sec), error = function(e) NA_real_))[1]) else NA_real_
        ref_px <- if (identical(ref_key, "multiples")) suppressWarnings(as.numeric(tryCatch(.model_point("multiples"), error = function(e) NA_real_))[1]) else NA_real_
        fmt_px <- function(x) if (!is.finite(x)) "—" else format(round(x, 2), big.mark = ",", nsmall = 2)
        marks <- list(
          list(lab = ui_str("ms_pos_market", loc), val = cur_px, col = "#333333"),
          list(lab = paste0(ui_str("ms_pos_primary", loc), " · ", .model_label(prim)), val = prim_px, col = "#0C5484"),
          list(lab = paste0(ui_str("ms_pos_secondary", loc), " · ", if (nzchar(sec)) .model_label(sec) else "—"), val = sec_px, col = "#888888"),
          list(lab = paste0(ui_str("ms_pos_reference", loc), " · ", tryCatch({
            lab <- if (exists("rel_results", inherits = TRUE) && !is.null(rel_results$chart_label)) rel_results$chart_label() else "Multiples"
            lab <- as.character(lab %||% "Multiples")[1]
            if (!grepl(" · ", lab, fixed = TRUE)) {
              tag <- as.character(rec$multiples_method_label %||% "")[1]
              if (nzchar(tag) && !is.na(tag)) lab <- paste0("Multiples · ", tag)
            }
            lab
          }, error = function(e) "Multiples")), val = ref_px, col = "#17a2b8")
        )
        finite <- vapply(marks, function(m) is.finite(m$val), logical(1))
        tags$div(
          class = "ynow-ms-pos",
          style = "margin-top:8px; padding:10px 12px; background:#f8fafc; border:1px solid #e2e8f0; border-radius:8px;",
          tags$div(style = "font-size:12px; font-weight:700; margin-bottom:8px; color:#334155;", ui_str("ms_pos_title", loc)),
          if (!any(finite)) {
            tags$p(style = "margin:0; font-size:12px; color:#64748b;", ui_str("ms_pos_empty", loc))
          } else {
            vals <- vapply(marks[finite], function(m) m$val, numeric(1))
            lo <- min(vals) * 0.9
            hi <- max(vals) * 1.1
            span <- max(hi - lo, 1)
            tagList(
              tags$div(style = "position:relative; height:46px; margin:18px 8px 28px 8px; background:#e5e7eb; border-radius:4px;",
                lapply(marks, function(m) {
                  if (!is.finite(m$val)) return(NULL)
                  pct <- (m$val - lo) / span * 100
                  tags$div(
                    style = paste0("position:absolute; left:", sprintf("%.1f", pct), "%; top:0; transform:translateX(-50%); text-align:center;"),
                    tags$div(style = paste0("width:10px; height:10px; border-radius:50%; background:", m$col, "; margin:18px auto 0 auto;")),
                    tags$div(style = "font-size:10px; color:#334155; white-space:nowrap; margin-top:2px;", paste0(m$lab, " ", fmt_px(m$val)))
                  )
                })
              )
            )
          }
        )
      }
    )
  })

  .snapshot_value <- function(x) {
    ynow_param_norm_value(x)
  }

  snapshot_rows <- reactive({
    ticker <- display_ticker_for_market(
      current_ticker() %||% APP_DEFAULTS$stock_code,
      market_mode()
    )
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    wacc_pct <- if (!is.null(calculated_wacc())) round(calculated_wacc() * 100, 2) else NA_real_
    est_g <- tryCatch(central_perpetual_g(), error = function(e) NULL)
    lite <- isTRUE(input$ynow_lite_mode)

    # Engine assumptions that are not editable inputs but still affect results —
    # must appear on Snapshot so nothing is a fully hidden substitute.
    hfv_thr <- tryCatch(
      if (exists("mos_assumptions_hfv_thr", mode = "function")) {
        mos_assumptions_hfv_thr(input)
      } else if (exists("hfv_scenario_threshold_defaults", mode = "function")) {
        hfv_scenario_threshold_defaults()
      } else {
        list()
      },
      error = function(e) list()
    )
    mos_lad <- tryCatch(
      if (exists("mos_assumptions_ladder", mode = "function")) {
        mos_assumptions_ladder(input)
      } else {
        list(deep = 0.30, mid = 0.10, fair = 0.00, mild_rich = -0.10)
      },
      error = function(e) list(deep = 0.30, mid = 0.10, fair = 0.00, mild_rich = -0.10)
    )
    capex_prior <- tryCatch(
      as.integer(APP_DEFAULTS$capex_spike_prior_years %||% 2L)[1],
      error = function(e) 2L
    )
    g2_sync <- if (!is.null(est_g) && is.finite(suppressWarnings(as.numeric(est_g$g_pct)[1]))) {
      .snapshot_value(est_g$g_pct)
    } else {
      .snapshot_value(tryCatch(input$sgr, error = function(e) APP_DEFAULTS$sgr))
    }
    statutory_tax <- tryCatch({
      mode <- normalize_market_mode(market_mode())
      if (identical(mode, "TW")) 20 else 21
    }, error = function(e) 21)

    extras <- list(
      c("Meta", "Downloaded At", ts, "Timestamp at download/render"),
      c("Meta", "Ticker", .snapshot_value(ticker), "Selected ticker"),
      c(
        "Model Selector", "Recommended Method",
        .snapshot_value(rec$summary_method),
        "Rule-based model ranking (derived)"
      ),
      c(
        "WACC", "Calculated WACC (%)",
        .snapshot_value(wacc_pct),
        "System CAPM/WACC estimate (derived; also synced into WACC inputs)"
      ),
      c(
        "SGR", "Estimated g (%)",
        if (!is.null(est_g)) .snapshot_value(est_g$g_pct) else NA_character_,
        "Selected perpetual-growth method output (derived)"
      ),
      c(
        "Two-Stage", "Stage-2 g2 (%) (synced to SGR)",
        g2_sync,
        "No separate UI; DCF stage-2 g follows central SGR / Estimated g"
      ),
      c(
        "FCF", "CapEx spike prior years",
        .snapshot_value(capex_prior),
        "APP_DEFAULTS; prior-window years excluding latest year for spike detect"
      ),
      c(
        "Tax", "Statutory tax fallback (%)",
        .snapshot_value(statutory_tax),
        "Used when income-statement effective tax is missing/NaN (TW 20 / US 21)"
      ),
      c(
        "HFV", "Scenario |Δ|/prev flat band",
        .snapshot_value(hfv_thr$momentum_flat_band),
        "Fixed heuristic; classifies FV/price momentum as flat (not editable)"
      ),
      c(
        "MOS Assumptions", "Scenario Price≪FV MOS floor",
        .snapshot_value(hfv_thr$mos_deep_cheap),
        "MOS Assumptions / HFV; MOS ≥ this → Price ≪ FV (scenario A)"
      ),
      c(
        "MOS Assumptions", "Scenario Price≫FV MOS ceiling",
        .snapshot_value(hfv_thr$mos_deep_rich),
        "MOS Assumptions / HFV; MOS ≤ this → Price ≫ FV (scenario D)"
      ),
      c(
        "MOS Assumptions", "Scenario Price≈FV |MOS| band",
        .snapshot_value(hfv_thr$mos_near),
        "MOS Assumptions / HFV; |MOS| ≤ this → Price ≈ FV"
      ),
      c(
        "MOS Assumptions", "Scenario D price strong-up",
        .snapshot_value(hfv_thr$price_strong_up),
        "MOS Assumptions / HFV; price momentum ≥ this for scenario D"
      ),
      c(
        "MOS Assumptions", "MOS hysteresis ladder cut-points",
        sprintf(
          "deep≥%.0f%%; mid≥%.0f%%; fair≥%.0f%%; mild-rich≥%.0f%% → max / ~72%% / ~44%% / ~17%% / 0; blend via bt_w_vg",
          100 * as.numeric(mos_lad$deep %||% 0.3),
          100 * as.numeric(mos_lad$mid %||% 0.1),
          100 * as.numeric(mos_lad$fair %||% 0),
          100 * as.numeric(mos_lad$mild_rich %||% -0.1)
        ),
        "Quant Lab Mode A exposure; cut-points from MOS Assumptions tab"
      ),
      c(
        "Backtest", "Mode B sentiment blend (fixed)",
        "clip(0.28 + 0.40×w_avg, 0.28, 0.60)",
        "Quant Lab Mode B; mixes Mode A with momentum/RSI — not editable"
      ),
      c(
        "SOTP", "Conglomerate discount",
        "not applied",
        "Market narrative often 10%–30%; engine never auto-discounts Σ(seg×EV/Sales) — adjust multiples manually if needed"
      )
    )
    ynow_snapshot_registry_rows(input, lite = lite, extras = extras)
  })

  output$snapshot_timestamp <- renderUI({
    tags$span(style = "font-size:12px; color:#666;", "Snapshot time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  })

  output$snapshot_table <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "snapshot"))
    DT::datatable(
      snapshot_rows(),
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        deferRender = TRUE,
        processing = TRUE,
        dom = "ltip"
      )
    )
  }, server = TRUE)

  output$download_snapshot <- downloadHandler(
    filename = function() {
      ticker <- gsub("[^A-Za-z0-9._-]", "_", current_ticker() %||% APP_DEFAULTS$stock_code)
      paste0("YNow_snapshot_", ticker, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if (isTRUE(.ynow_deny_guest_download(file))) return()
      if (isTRUE(.ynow_access_deny_download("report:download_snapshot", file))) return()
      tryCatch(
        .ynow_access_note_conditional_use("report:download_snapshot", consume_free = TRUE),
        error = function(e) NULL
      )
      df <- snapshot_rows()
      df$Downloaded_At <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$download_param_restore <- downloadHandler(
    filename = function() {
      ticker <- gsub("[^A-Za-z0-9._-]", "_", current_ticker() %||% APP_DEFAULTS$stock_code)
      paste0("YNow_param_restore_", ticker, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if (isTRUE(.ynow_deny_guest_download(file))) return()
      if (isTRUE(.ynow_access_deny_download("report:download_param_restore", file))) return()
      tryCatch(
        .ynow_access_note_conditional_use("report:download_param_restore", consume_free = TRUE),
        error = function(e) NULL
      )
      df <- ynow_param_restore_export_df(
        input,
        ticker = display_ticker_for_market(
          current_ticker() %||% APP_DEFAULTS$stock_code,
          market_mode()
        ),
        market_mode = market_mode()
      )
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  param_restore_status <- reactiveVal(NULL)

  output$param_restore_status_ui <- renderUI({
    st <- param_restore_status()
    if (is.null(st) || !nzchar(as.character(st$message %||% "")[1])) return(NULL)
    col <- if (isTRUE(st$ok)) "#1e7e34" else "#a94442"
    tags$p(style = paste0("margin:6px 0 0 0; font-size:12.5px; color:", col, ";"), st$message)
  })

  observeEvent(input$param_restore_go, {
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    f <- input$param_restore_file
    if (is.null(f) || !nzchar(as.character(f$datapath %||% "")[1])) {
      msg <- ui_str("param_restore_need_file", loc)
      param_restore_status(list(ok = FALSE, message = msg))
      showNotification(msg, type = "warning", duration = 6)
      return()
    }
    parsed <- ynow_param_restore_parse_file(f$datapath)
    if (!isTRUE(parsed$ok)) {
      err_key <- paste0("param_restore_err_", parsed$error %||% "unreadable")
      msg <- ui_str(err_key, loc)
      if (identical(msg, err_key) || !nzchar(msg)) {
        msg <- ui_str("param_restore_err_unreadable", loc)
      }
      param_restore_status(list(ok = FALSE, message = msg))
      showNotification(msg, type = "error", duration = 8)
      return()
    }

    meta_tk <- trimws(as.character(parsed$meta$ticker %||% "")[1])
    if (nzchar(meta_tk)) {
      updateTextInput(session, "sc", value = meta_tk)
    }
    meta_mkt <- toupper(trimws(as.character(parsed$meta$market_mode %||% "")[1]))
    if (nzchar(meta_mkt) && meta_mkt %in% c("US", "TW")) {
      tryCatch({
        shinyjs::runjs(sprintf(
          paste0(
            "(function(){",
            "var stack=document.querySelector('#ynow-market-header .ynow-market-stack');",
            "if(stack){stack.querySelectorAll('.ynow-mkt-btn').forEach(function(b){",
            "b.classList.toggle('active', b.getAttribute('data-value')===%s);});}",
            "if(window.Shiny&&Shiny.setInputValue){",
            "Shiny.setInputValue('market_mode_pick', %s, {priority:'event'});}",
            "})();"
          ),
          jsonlite::toJSON(meta_mkt, auto_unbox = TRUE),
          jsonlite::toJSON(meta_mkt, auto_unbox = TRUE)
        ))
      }, error = function(e) invisible(NULL))
    }

    res <- ynow_param_restore_apply(session, parsed$rows)
    tip <- if (nzchar(meta_tk)) {
      ui_str("param_restore_ok_with_ticker", loc)
    } else {
      ui_str("param_restore_ok", loc)
    }
    tip <- gsub("\\{n\\}", as.character(res$applied), tip, fixed = TRUE)
    tip <- gsub("\\{ticker\\}", meta_tk, tip, fixed = TRUE)
    if (isTRUE(res$skipped > 0L)) {
      tip <- paste0(tip, " ", gsub("\\{n\\}", as.character(res$skipped),
                                   ui_str("param_restore_skipped", loc), fixed = TRUE))
    }
    if (nzchar(meta_tk)) {
      tip <- paste0(tip, " ", ui_str("param_restore_search_hint", loc))
    }
    param_restore_status(list(ok = TRUE, message = tip))
    showNotification(tip, type = "message", duration = 10)
  })

  .format_default_value <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (length(x) == 1 && isTRUE(is.na(x))) return(NA_character_)
    if (is.logical(x)) return(paste(as.character(x), collapse = ", "))
    if (is.numeric(x)) {
      return(paste(vapply(x, function(v) {
        if (!is.finite(v)) return(NA_character_)
        if (abs(v - round(v)) < 1e-9) as.character(as.integer(round(v))) else as.character(round(v, 4))
      }, character(1)), collapse = ", "))
    }
    paste(as.character(x), collapse = ", ")
  }

  defaults_rows <- reactive({
    # Human labels for APP_DEFAULTS keys (unlisted keys still appear by key name)
    label_map <- list(
      stock_code = c("基本設定", "預設股票代號", "啟動／重置用 ticker"),
      industry_choice = c("基本設定", "預設產業鍵", "industry_standards 鍵名"),
      years = c("DCF", "預測年數 n", "Explicit forecast horizon"),
      ddm_d0 = c("DDM", "D0", "由財報自動帶入；若無則 0"),
      ddm_g = c("DDM", "股利成長 g (%)", "預設對齊中央 SGR"),
      ddm_ke = c("DDM", "Ke (%)", "預設對齊 CAPM Re"),
      ddm_sync_central_g = c("DDM", "與中央 SGR 同步", "TRUE 時 DDM g 跟隨 SGR"),
      ddm_mode = c("DDM", "DDM 結構", "gordon / spm / two_stage"),
      ddm_g_stage1 = c("DDM", "高速期 g1 (%)", "二階段前段股利成長"),
      ddm_yr_stage1 = c("DDM", "高速期年數 n1", "二階段前段年數"),
      dcf_mode = c("DCF", "DCF 模式", "gordon / two_stage"),
      dcf_claim = c("DCF", "採用現金流", "fcff / fcfe"),
      dcf_chart_mode = c("DCF", "圖表模式", "固定 with_dcf（各年 PV，不含終值）"),
      cf_flow_series = c("Dashboard", "Cash Flow 疊圖序列", "ocf / icf / fcf(融資)"),
      g_growth_method = c("DCF", "營收成長估計法", "fundamental / revenue CAGR 等"),
      custom_g = c("DCF", "自訂營收成長 g (%)", "封頂後的短中期營收成長"),
      perpetual_g_method = c("永續成長", "方法", "macro / fundamental / lifecycle；預設 fundamental"),
      lifecycle_stage = c("永續成長", "生命週期", "auto 或手動階段"),
      sgr = c("永續成長", "SGR / 終值 g (%)", "啟動錨 Rf；方法跑完後可覆寫；須 < WACC"),
      wacc_gordon = c("DCF", "Gordon WACC (%)", "由 WACC 分頁同步；隱藏欄位"),
      yr_stage1 = c("Two-Stage", "高速期年數", "Stage 1 years"),
      g_stage1 = c("Two-Stage", "高速期 g1 (%)", "Stage 1 growth"),
      g_stage2 = c("Two-Stage", "穩定期 g2 (%)", "通常對齊 SGR（內部預設）"),
      wacc_stage1 = c("Two-Stage", "WACC1 (%)", "Stage 1 discount"),
      wacc_stage2 = c("Two-Stage", "WACC2 (%)", "Terminal discount"),
      wacc_re = c("WACC", "Re (%)", "Cost of equity"),
      wacc_rd = c("WACC", "rᵈ (%)", "NA＝無靜態預設；利息／有息負債後覆寫"),
      wacc_rd_min = c("WACC", "rᵈ 下限 (%)", "推估 rᵈ 夾限下限"),
      wacc_rd_max = c("WACC", "rᵈ 上限 (%)", "推估 rᵈ 夾限上限"),
      use_est_rd = c("WACC", "使用估算 rᵈ", "UI: use_estimated_rd；TRUE = rᵈ 跟利息／有息負債"),
      rd_interest_expense = c("WACC", "利息費用", "rᵈ 分子；損益 Interest Expense 或 CF Interest Paid"),
      rd_interest_bearing_debt = c("WACC", "有息負債", "rᵈ 分母；Total Debt 或 ST+LT Debt"),
      wacc_tax = c("WACC", "稅率 T (%)", "After-tax debt cost"),
      use_est_re = c("WACC", "使用 CAPM Re", "UI: use_estimated_re；TRUE = Re 跟 CAPM"),
      capm_rf = c("CAPM", "Rf (%)", "無風險利率（啟動時估）"),
      capm_beta = c("CAPM", "Beta", "啟動暫定值；估值路徑就緒後改寫入選定來源"),
      sync_gs_beta = c("CAPM", "與基礎設定同步", "TRUE = WACC/CAPM β 跟隨基礎設定 β 來源（預設 Summary β）"),
      beta_bench = c("Beta", "基準指數", "Rolling β 對照標的，預設 SPY（不寫入 CAPM）"),
      beta_lookback_months = c("Beta", "回溯月數", "常見 36／60／84；預設 60 對齊 Yahoo 5Y"),
      beta_min_obs = c("Beta", "最少觀測", "Rolling 估計最低月數"),
      beta_purpose = c("Beta", "用途", "valuation；Rolling 不得寫入 CAPM"),
      beta_bl_source = c("Beta", "Unlever β_L 來源", "summary / rolling / auto"),
      beta_bottomup_agg = c("Beta", "Bottom-Up 聚合", "mean / median"),
      beta_u_apply_source = c("Beta", "β 來源", "summary / industry / bottomup / unlever_firm / manual (rolling blocked)"),
      beta_u_manual = c("Beta", "手動 β", "數值（直接寫入 CAPM）"),
      beta_peers = c("Beta", "Bottom-up 同業", "逗號分隔代號；UI 多選"),
      beta_relever_de_mode = c("Beta（舊版相容）", "再槓桿 D/E 模式", "隱藏相容；UI 已移除"),
      beta_target_de = c("Beta（舊版相容）", "目標 D/E", "隱藏相容；UI 已移除"),
      capm_rm = c("CAPM", "Rm (%)", "預期市場報酬"),
      ri_years = c("RI", "預測期 (Years)", "RI 模組預設年數"),
      ri_roe = c("RI", "起始／預期 ROE (%)", "財報載入前暫定值；載入後覆寫"),
      ri_payout = c("RI", "配息率 Payout (%)", "財報載入前暫定值；載入後覆寫"),
      roe_method = c("RI", "ROE 預測方法", "constant / linear / industry / custom"),
      pb_bvps = c("P/B", "BVPS", "通常由財報帶入"),
      pb_tbvps = c("P/B", "TBVPS", "通常由財報帶入"),
      pb_low = c("P/B", "P/B Low", "產業帶／保守下緣"),
      pb_mid = c("P/B", "P/B Mid", "產業帶中位"),
      pb_high = c("P/B", "P/B High", "產業帶上緣"),
      pb_basis = c("P/B", "Basis", "bvps / tbvps / navps"),
      pb_use_industry = c("P/B", "使用產業 P/B", "TRUE = 跟產業帶"),
      pb_holdco_discount = c("P/B", "控股折價", "套用在已辨識投資科目"),
      pb_target_mode = c("P/B", "目標模式", "multiples / justified"),
      nav_holdco_discount = c("NAV", "控股折價", "套用在已辨識投資科目"),
      nav_low = c("NAV", "NAV Low 倍數", "Bear 倍數"),
      nav_mid = c("NAV", "NAV Mid 倍數", "Base 倍數"),
      nav_high = c("NAV", "NAV High 倍數", "Bull 倍數"),
      rel_mode = c("Multiples", "Relative mode", "equity / enterprise"),
      rel_pe_multiple = c("Multiples", "Selected P/E", "App／產業假設；非 Yahoo 回推"),
      rel_fwd_pe_multiple = c("Multiples", "Selected Forward P/E", "App／產業假設"),
      rel_ev_fcf_multiple = c("Multiples", "EV/FCF", "App／產業假設"),
      rel_ev_ebit_multiple = c("Multiples", "EV/EBIT", "App／產業假設"),
      rel_ev_ebitda_multiple = c("Multiples", "EV/EBITDA", "App／產業假設"),
      rel_ev_sales_multiple = c("Multiples", "EV/Sales", "App／產業假設；SOTP 預設亦用此鍵"),
      rel_ps_multiple = c("Multiples", "P/S", "App／產業假設"),
      rel_ev_arr_multiple = c("Multiples", "EV/ARR", "App／產業假設"),
      apply_capex_spike_smooth = c("FCF", "啟用 CapEx 暴衝平滑", "TRUE = 暴衝時改採均值"),
      capex_spike_mult = c("FCF", "暴衝倍數閾值", "最新 CapEx/Rev > mult × 前期均值"),
      capex_spike_avg_years = c("FCF", "暴衝均值年數", "暴衝時投影採最近 N 年均值"),
      capex_spike_prior_years = c("FCF", "暴衝判定前期年數", "不含最新年的前期均值窗口"),
      apply_g_ceiling = c("FCF", "啟用成長率天花板", "近端 g 25% 防呆"),
      session_ccy = c("Meta", "Session Currency", "USD / TWD 顯示幣別"),
      chk_bear_base = c("MOS Assumptions", "Gate: Bear vs Base", "啟發式；預設勾選"),
      cond_bear_base_bear_mos_floor = c("MOS Assumptions", "Bear MOS floor (%)", "Bear FV 最低 MOS"),
      chk_base_mos = c("MOS Assumptions", "Gate: Base MOS", "啟發式；自行加選"),
      cond_base_mos_base_mos_floor = c("MOS Assumptions", "Base MOS floor (%)", "Base FV 最低 MOS"),
      mos_alert_default_pct = c("MOS Assumptions", "Default alert MOS (%)", "到值通知預設折價"),
      mos_hfv_near_pct = c("MOS Assumptions", "HFV |MOS|≈FV (%)", "情境近公允帶"),
      mos_hfv_deep_cheap_pct = c("MOS Assumptions", "HFV deep-cheap (%)", "情境 A 下限"),
      mos_hfv_deep_rich_pct = c("MOS Assumptions", "HFV deep-rich (%)", "情境 D 上限"),
      mos_hfv_flat_band_pct = c("MOS Assumptions", "HFV flat |Δ| (%)", "動能持平帶"),
      mos_hfv_price_strong_up_pct = c("MOS Assumptions", "HFV D strong-up (%)", "情境 D 強升"),
      mos_ladder_deep_pct = c("MOS Assumptions", "Ladder deep (%)", "Mode A 深度便宜切點"),
      mos_ladder_mid_pct = c("MOS Assumptions", "Ladder mid (%)", "Mode A 溫和便宜切點"),
      mos_ladder_fair_pct = c("MOS Assumptions", "Ladder fair (%)", "Mode A 近公允切點"),
      mos_ladder_mild_rich_pct = c("MOS Assumptions", "Ladder mild-rich (%)", "Mode A 溫和偏貴切點"),
      chk_g_sgr = c("Decision Checklist", "Gate: g vs SGR", "啟發式；自行加選"),
      cond_g_sgr_g_sgr_gap_min = c("Decision Checklist", "g−SGR gap min (pp)", "成長與 SGR 最小差距"),
      cond_g_sgr_sgr_wacc_buffer = c("Decision Checklist", "SGR–WACC buffer (pp)", "SGR 須低於 WACC 的緩衝"),
      chk_model_align = c("Decision Checklist", "Gate: Model align", "主模型對齊；自行加選"),
      dc_user_primary = c("Decision Checklist", "Adopted primary model", "使用者採用主模型"),
      chk_hfv_veto = c("Decision Checklist", "Gate: HFV veto", "HFV 僅否決，不作買訊；預設勾選"),
      cond_hfv_veto_max_c_freq = c("Decision Checklist", "HFV max C-freq (%)", "歷史 C 頻率上限"),
      chk_fscore = c("Decision Checklist", "Gate: F-Score quality screen", "F-Score 品質檢核；自行加選"),
      cond_fscore_fscore_min = c("Decision Checklist", "F-Score min", "最低 F-Score"),
      chk_no_rank_chase = c("Decision Checklist", "Gate: No rank chase", "不追排行榜；自行加選"),
      bt_net_margin = c("Backtest", "Net margin threshold (%)", "持倉門檻"),
      bt_rev_growth = c("Backtest", "Revenue growth threshold (%)", "持倉門檻"),
      bt_eps_growth = c("Backtest", "EPS / NI growth threshold (%)", "持倉門檻"),
      bt_fcf_cv = c("Backtest", "FCF CV ceiling (%)", "持倉門檻"),
      bt_w_vg = c("Backtest", "MOS / VG weight", "曝險診斷權重"),
      bt_w_mom = c("Backtest", "Momentum weight", "情緒相對權重"),
      bt_w_rsi = c("Backtest", "RSI weight", "情緒相對權重"),
      bt_max_exp = c("Backtest", "Max exposure", "持倉上限"),
      bt_min_exp_pass = c("Backtest", "Min exp after pass", "通過閘門後下限"),
      bt_param_auto = c("Backtest", "Auto-update parameters", "換股時自動更新參數"),
      bt_fv_replay_model = c("Backtest", "Replay model", "HFV 復盤單選模型"),
      bt_fv_models = c("Backtest", "Chart overlay models", "HFV 圖疊加；預設不勾選"),
      bt_fv_conv_window = c("Backtest", "Sample window", "all / 1y / 3y / 5y / custom"),
      bt_fv_oos_mode = c("Backtest", "Validation sample scope", "realized / expanding / insample"),
      bt_fv_analysis_freq = c("Backtest", "Analysis frequency", "monthly / quarterly / yearly"),
      bt_hfv_show_bench = c("Backtest", "Show benchmark", "HFV 圖顯示基準"),
      bt_nav_window = c("Backtest", "NAV window", "Strategy NAV 視窗"),
      lab_im_pool_rank = c("Lab", "候選截斷邏輯", "市值／概念股／近一年漲幅／隨機"),
      lab_im_concepts = c("Lab", "概念股群", "pool = concept 時"),
      lab_im_lb_mode = c("Lab", "Leaderboard mode", "overall / by_industry / undervalued"),
      lab_im_boards = c("Lab", "TW boards", "TWSE / TPEX / ESB"),
      lab_im_max_n = c("Lab", "Universe size N", "顯示上限"),
      lab_im_max_n_custom = c("Lab", "Custom universe N", "N = custom 時"),
      lab_im_eq_only = c("Lab", "Earnings quality only", "僅盈餘品質通過"),
      lab_im_no_alert = c("Lab", "No FS alerts", "無財報警訊"),
      lab_im_include_adr = c("Lab", "Include ADRs", "含 ADR"),
      lab_im_gate_only = c("Lab", "Gate only", "Piotroski 高門檻"),
      lab_im_methods = c("Lab", "Valuation models", "Lab 計分模型"),
      lab_cluster_k = c("Lab", "Cluster k", "群數"),
      lab_cluster_x = c("Lab", "Cluster scatter X", "散點 X"),
      lab_cluster_y = c("Lab", "Cluster scatter Y", "散點 Y"),
      lab_sec_important_only = c("Lab", "Important notes only", "SEC 附註過濾")
    )

    # Lite Snapshot defaults: only inputs Lite users can set in the UI
    # (header currency, Dashboard industry / default ticker, Blue Chip + Clustering).
    # Smart Analysis engines and Full-only model pages are auto / hidden — omit here.
    lite_default_keys <- c(
      "stock_code", "industry_choice", "session_ccy",
      "lab_im_pool_rank", "lab_im_concepts",
      "lab_im_lb_mode", "lab_im_boards", "lab_im_max_n", "lab_im_max_n_custom",
      "lab_im_eq_only", "lab_im_no_alert", "lab_im_include_adr",
      "lab_cluster_k", "lab_cluster_x", "lab_cluster_y"
    )
    # Lite has no module extras outside APP_DEFAULTS (RI industry ROE is Full-only).
    lite_extra_keys <- character(0)

    keys <- names(APP_DEFAULTS)
    if (isTRUE(input$ynow_lite_mode)) {
      keys <- intersect(keys, lite_default_keys)
    }
    rows <- lapply(keys, function(k) {
      meta <- label_map[[k]]
      if (is.null(meta)) {
        meta <- c("其他", k, "APP_DEFAULTS 欄位")
      }
      c(meta[1], meta[2], k, .format_default_value(APP_DEFAULTS[[k]]), meta[3])
    })

    # Module / UI hard defaults not stored in APP_DEFAULTS
    ind_roe_def <- {
      v <- if (exists(".industry_roe_pct", mode = "function")) {
        .industry_roe_pct(APP_DEFAULTS$industry_choice)
      } else {
        NA_real_
      }
      .format_default_value(if (is.finite(v)) round(v, 2) else 12)
    }
    extra <- list(
      c("RI", "Industry Average ROE (%)", "roe_industry", ind_roe_def,
        "依預設產業 ROE 區間中位；可編輯（非 APP_DEFAULTS 鍵）"),
      c("彈性表", "參數相對衝擊", "PARAM_SENSITIVITY_SHOCK",
        if (exists("PARAM_SENSITIVITY_SHOCK", inherits = TRUE)) as.character(PARAM_SENSITIVITY_SHOCK) else "0.01",
        "setup.R：公式參數彈性相對 ±1%（與個股價格無關）")
    )
    if (isTRUE(input$ynow_lite_mode)) {
      extra <- Filter(function(r) as.character(r[[3]])[1] %in% lite_extra_keys, extra)
    }
    rows <- c(rows, extra)

    df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(df) <- c("Section", "Parameter", "Key", "Default Value", "Note")
    df
  })

  output$defaults_table <- renderDataTable({
    # Same unlock as Points → System admin (ynow + Taipei YYYYMMDD).
    req(isTRUE(ynow_pts_sysadmin_on()))
    datatable(
      defaults_rows(),
      rownames = FALSE,
      options = list(pageLength = 30, scrollX = TRUE, order = list(list(0, "asc")))
    )
  })

  output$download_defaults <- downloadHandler(
    filename = function() {
      paste0("YNow_defaults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if (isTRUE(.ynow_deny_guest_download(file))) return()
      if (!isTRUE(isolate(ynow_pts_sysadmin_on()))) {
        write.csv(
          data.frame(
            error = "System defaults (APP_DEFAULTS) require System admin unlock.",
            stringsAsFactors = FALSE
          ),
          file,
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )
        return(invisible(NULL))
      }
      df <- defaults_rows()
      df$Downloaded_At <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  .param_audit_eval_summary_html <- function() {
    loc <- isolate(ui_locale())
    use_zh <- grepl("^zh", loc, ignore.case = TRUE)
    tk <- display_ticker_for_market(
      current_ticker() %||% APP_DEFAULTS$stock_code,
      market_mode()
    )
    px <- suppressWarnings(as.numeric(stock_price_estimate_val())[1])
    wacc <- suppressWarnings(as.numeric(calculated_wacc())[1])
    sgr <- suppressWarnings(as.numeric(input$sgr)[1])
    parts <- c(
      if (use_zh) paste0("Ticker：", tk) else paste0("Ticker: ", tk),
      if (is.finite(px)) {
        if (use_zh) paste0("DCF 每股估價 ≈ ", round(px, 2))
        else paste0("DCF per-share ≈ ", round(px, 2))
      } else {
        if (use_zh) "DCF 每股估價：尚未試算" else "DCF per-share: not yet run"
      },
      if (is.finite(wacc)) paste0("WACC ", round(wacc * 100, 2), "%") else NULL,
      if (is.finite(sgr)) paste0("SGR / terminal g ", round(sgr, 2), "%") else NULL
    )
    paste(parts[!vapply(parts, is.null, logical(1))], collapse = " · ")
  }

  output$param_audit_report <- renderUI({
    loc <- ui_locale()
    baseline <- param_audit_baseline()
    baselined_at <- param_audit_baseline_at()
    cur <- ynow_capture_tracked_params(input)
    diff_df <- ynow_param_diff_df(baseline, cur, locale = loc)
    empty_msg <- if (is.null(baseline)) {
      ui_str("param_audit_empty_no_baseline", loc)
    } else {
      ui_str("param_audit_empty_no_changes", loc)
    }
    ynow_param_audit_report_ui(
      diff_df,
      eval_summary = .param_audit_eval_summary_html(),
      baseline_at = baselined_at,
      locale = loc,
      empty_message = empty_msg
    )
  })

  observeEvent(input$param_audit_pdf_go, {
    if (isTRUE(.ynow_deny_guest_download(NULL))) return()
    loc <- isolate(ui_locale())
    ids <- as.character(input$param_audit_pdf_pages %||% character(0))
    ids <- ids[nzchar(ids)]
    if (!length(ids)) {
      showNotification(ui_str("param_audit_pdf_need_pages", loc), type = "warning", duration = 4)
      return()
    }
    catalog <- ynow_param_audit_pdf_pages()
    # Preserve user checkbox order; drop unknown ids.
    ids <- ids[ids %in% catalog$id]
    if (!length(ids)) {
      showNotification(ui_str("param_audit_pdf_need_pages", loc), type = "warning", duration = 4)
      return()
    }
    sel <- catalog[match(ids, catalog$id), , drop = FALSE]
    page_titles <- stats::setNames(
      vapply(catalog$locale_key, function(k) ui_str(k, loc), character(1)),
      catalog$id
    )
    pages_payload <- lapply(seq_len(nrow(sel)), function(i) {
      list(
        id = as.character(sel$id[[i]]),
        tab = as.character(sel$tab[[i]]),
        inner = as.character(sel$inner[[i]]),
        annotate_tab = as.character(sel$annotate_tab[[i]]),
        title = unname(page_titles[[sel$id[[i]]]] %||% sel$id[[i]])
      )
    })
    baseline <- isolate(param_audit_baseline())
    cur <- ynow_capture_tracked_params(input)
    diff_df <- ynow_param_diff_df(baseline, cur, locale = loc)
    changes <- list()
    if (!is.null(diff_df) && nrow(diff_df) > 0) {
      changes <- lapply(seq_len(nrow(diff_df)), function(i) {
        list(
          input_id = as.character(diff_df$input_id[[i]]),
          tab = as.character(diff_df$tab[[i]]),
          section = as.character(diff_df$section[[i]]),
          label = as.character(diff_df$label[[i]]),
          baseline = as.character(diff_df$baseline[[i]]),
          current = as.character(diff_df$current[[i]])
        )
      })
    }
    tk <- display_ticker_for_market(
      isolate(current_ticker()) %||% APP_DEFAULTS$stock_code,
      isolate(market_mode())
    )
    baselined_at <- isolate(param_audit_baseline_at())
    ts_txt <- if (!is.null(baselined_at)) {
      tryCatch(format(baselined_at, "%Y-%m-%d %H:%M:%S"), error = function(e) as.character(baselined_at))
    } else {
      "—"
    }
    session$sendCustomMessage("ynowParamAuditPdf", list(
      tabs = as.list(as.character(sel$tab)),
      pages = pages_payload,
      changes = changes,
      page_titles = as.list(page_titles),
      ticker = as.character(tk)[1],
      baseline_at = ts_txt,
      title = ui_str("param_audit_pdf_title", loc),
      filename = paste0(
        "YNow_param_audit_",
        gsub("[^A-Za-z0-9._-]", "_", as.character(tk)[1]),
        "_", format(Sys.time(), "%Y%m%d_%H%M%S")
      ),
      strings = list(
        busy = ui_str("param_audit_pdf_busy", loc),
        done = ui_str("param_audit_pdf_done", loc),
        err = ui_str("param_audit_pdf_err", loc),
        need_pages = ui_str("param_audit_pdf_need_pages", loc),
        summary = ui_str("param_audit_pdf_summary", loc),
        no_changes = ui_str("param_audit_empty_no_changes", loc)
      )
    ))
  }, ignoreInit = TRUE)
  
  # 顯示層管線：FX →（台股）仟元 → 小數二位 → zh-TW 科目
  .prep_fs_statement_display <- function(df) {
    use_thou <- should_scale_fs_thousands(mode = market_mode())
    use_zh <- should_localize_fs_zh_tw(mode = market_mode(), locale = ui_locale())
    out <- scale_financial_df_money(
      reorder_financial_columns(df), statement_currency(), session_currency(), fx_usd_twd()
    )
    out <- scale_financial_df_thousands_display(out, enabled = use_thou)
    out <- format_financial_df_display(out)
    out <- localize_financial_df_zh_tw(out, enabled = use_zh)
    list(
      df = out,
      caption = fs_thousands_unit_caption(session_currency(), enabled = use_thou)
    )
  }

  # Shared DT options for large statement tables (server-side paging + deferRender).
  .ynow_fs_dt_options <- list(
    pageLength = 20,
    scrollX = TRUE,
    deferRender = TRUE,
    autoWidth = FALSE,
    processing = TRUE,
    dom = "ltip",
    language = list(processing = "…")
  )

  output$tbIncomeStatement <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "dashboard"))
    req(identical(as.character(input$dashboard_fin_report %||% "")[1], "Income Statement"))
    req(scraped_financials())
    session_currency(); fx_usd_twd(); market_mode(); ui_locale()
    raw <- scraped_financials()[["Income Statement"]]$collapsed
    prep <- .prep_fs_statement_display(raw)
    DT::datatable(
      trim_financial_table(prep$df, "Tax Effect of Unusual Items"),
      caption = if (!is.null(prep$caption)) htmltools::tags$caption(style = "caption-side: top; text-align: left; color: #555;", prep$caption) else NULL,
      rownames = FALSE,
      options = .ynow_fs_dt_options
    )
  }, server = TRUE)
  
  output$tbBalanceSheet <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "dashboard"))
    req(identical(as.character(input$dashboard_fin_report %||% "")[1], "Balance Sheet"))
    req(scraped_financials())
    session_currency(); fx_usd_twd(); market_mode(); ui_locale()
    raw <- scraped_financials()[["Balance Sheet"]]$collapsed
    prep <- .prep_fs_statement_display(raw)
    DT::datatable(
      trim_financial_table(prep$df, "Treasury Shares Number"),
      caption = if (!is.null(prep$caption)) htmltools::tags$caption(style = "caption-side: top; text-align: left; color: #555;", prep$caption) else NULL,
      rownames = FALSE,
      options = .ynow_fs_dt_options
    )
  }, server = TRUE)
  
  output$tbCashFlow <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "dashboard"))
    req(identical(as.character(input$dashboard_fin_report %||% "")[1], "Cash Flow"))
    req(scraped_financials())
    session_currency(); fx_usd_twd(); market_mode(); ui_locale()
    raw <- scraped_financials()[["Cash Flow"]]$collapsed
    prep <- .prep_fs_statement_display(raw)
    DT::datatable(
      trim_financial_table(prep$df, "Free Cash Flow"),
      caption = if (!is.null(prep$caption)) htmltools::tags$caption(style = "caption-side: top; text-align: left; color: #555;", prep$caption) else NULL,
      rownames = FALSE,
      options = .ynow_fs_dt_options
    )
  }, server = TRUE)
  
  output$IS_download <- downloadHandler(
    filename = function() paste0(current_ticker(), "_incomestatement_", Sys.Date(), ".csv"),
    content = function(file) {
      if (isTRUE(.ynow_access_deny_download("report:IS_download", file))) return()
      write.csv(d_income_statement(), file, row.names = FALSE)
      tryCatch(
        .ynow_access_note_conditional_use("report:IS_download", consume_free = TRUE),
        error = function(e) NULL
      )
    }
  )
  output$BS_download <- downloadHandler(
    filename = function() paste0(current_ticker(), "_balancesheet_", Sys.Date(), ".csv"),
    content = function(file) {
      if (isTRUE(.ynow_access_deny_download("report:BS_download", file))) return()
      write.csv(d_balance_sheet(), file, row.names = FALSE)
      tryCatch(
        .ynow_access_note_conditional_use("report:BS_download", consume_free = TRUE),
        error = function(e) NULL
      )
    }
  )
  output$CF_download <- downloadHandler(
    filename = function() paste0(current_ticker(), "_cashflow_", Sys.Date(), ".csv"),
    content = function(file) {
      if (isTRUE(.ynow_access_deny_download("report:CF_download", file))) return()
      write.csv(d_cash_flow(), file, row.names = FALSE)
      tryCatch(
        .ynow_access_note_conditional_use("report:CF_download", consume_free = TRUE),
        error = function(e) NULL
      )
    }
  )
  
  # ==========================================
  # 📈 Income Statement 互動圖表
  # ==========================================
  selected_is_data <- reactive({
    req(d_income_statement())
    keyword <- switch(input$is_type,
                      "Total Revenue" = "Total Revenue",
                      "Gross Profit" = "Gross Profit",
                      "EBITDA" = "EBITDA")
    
    res <- d_income_statement()[grepl(keyword, d_income_statement()[[1]], ignore.case = TRUE), ]
    if(nrow(res) > 0) return(res[1, ])
    return(NULL)
  })
  
  output$is_plot <- renderPlotly({
    req(identical(.ynow_sidebar_tab(), "dashboard"))
    req(identical(as.character(input$dashboard_fin_report %||% "")[1], "Income Statement"))
    generate_safe_line_plot(
      data = selected_is_data(), 
      ticker_name = current_ticker(), 
      metric_name = input$is_type
    )
  })

  # ==========================================
  # 📈 Balance Sheet 圓餅圖（資產／負債／權益）
  # 會計恆等式：Assets = Liabilities + Equity
  # 甜甜圈顯示負債／權益佔總資產比例；中心標註總資產
  # 旁側：若可區分股份性質，顯示股權結構圓餅圖
  # ==========================================
  .eq_structure_slices_now <- reactive({
    ui_locale()
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    sum_df <- tryCatch(summary_data(), error = function(e) NULL)
    tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    equity_structure_slices(bs, sum_df, ticker = tk, locale = loc)
  })

  # Stable Plotly hosts: do not swap column layouts on locale/slice changes
  # (that tore down and rebuilt Plotly DOM on every currency/locale tick).
  output$bs_equity_charts_ui <- renderUI({
    fluidRow(
      column(6, plotlyOutput("bs_plot", height = "380px")),
      column(6, plotlyOutput("eq_structure_plot", height = "380px"))
    )
  })

  output$bs_plot <- renderPlotly({
    req(identical(.ynow_sidebar_tab(), "dashboard"))
    req(identical(as.character(input$dashboard_fin_report %||% "")[1], "Balance Sheet"))
    tk <- current_ticker()
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    empty_title <- paste0(
      if (is.null(tk) || !nzchar(tk)) "Balance Sheet" else tk, " - ",
      ui_str("bs_pie_empty", loc)
    )
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    if (is.null(bs) || !is.data.frame(bs) || nrow(bs) == 0) {
      return(plotly::plotly_empty() %>% plotly::layout(title = empty_title))
    }

    assets <- tryCatch(
      select_current_metric(bs, "Total Assets", "stock"),
      error = function(e) NA_real_
    )
    equity <- tryCatch(
      select_current_metric_any(bs, EQUITY_PATTERNS, "stock"),
      error = function(e) NA_real_
    )
    liabilities <- tryCatch(
      select_current_metric_any(
        bs,
        c(
          "Total Liabilities Net Minority Interest",
          "^Total Liabilities$",
          "Total Liab"
        ),
        "stock"
      ),
      error = function(e) NA_real_
    )
    # Yahoo 偶缺總負債列：以會計恆等式 Assets − Equity 回推
    if ((!is.finite(liabilities) || liabilities == 0) &&
        is.finite(assets) && is.finite(equity)) {
      liabilities <- assets - equity
    }
    # 缺總資產時，以 負債＋權益 回推（僅正值）
    if (!is.finite(assets) || assets <= 0) {
      parts <- c(liabilities, equity)
      parts <- parts[is.finite(parts) & parts > 0]
      if (length(parts) > 0) assets <- sum(parts)
    }

    labels <- c(ui_str("bs_pie_liabilities", loc), ui_str("bs_pie_equity", loc))
    values <- c(liabilities, equity)
    colors <- c("#dd4b39", "#00a65a")
    keep <- is.finite(values) & values > 0
    if (!any(keep) || !is.finite(assets) || assets <= 0) {
      return(plotly::plotly_empty() %>% plotly::layout(title = empty_title))
    }

    plot_df <- data.frame(
      Category = labels[keep],
      Value = values[keep],
      Color = colors[keep],
      stringsAsFactors = FALSE
    )
    # 佔總資產比例（會計恆等式）
    plot_df$Pct <- plot_df$Value / assets * 100
    plot_df$Hover <- paste0(
      "<b>", plot_df$Category, "</b><br>",
      ui_str("bs_pie_hover_amount", loc), ": <b>", format_dollar_abbr(plot_df$Value), "</b><br>",
      ui_str("bs_pie_hover_pct_assets", loc), ": <b>", sprintf("%.1f%%", plot_df$Pct), "</b><br>",
      ui_str("bs_pie_hover_total_assets", loc), ": <b>", format_dollar_abbr(assets), "</b>"
    )

    center_txt <- paste0(
      ui_str("bs_pie_center_assets", loc), "<br><b>", format_dollar_abbr(assets), "</b>"
    )

    plotly::plot_ly(
      plot_df,
      labels = ~Category,
      values = ~Value,
      type = "pie",
      hole = 0.48,
      marker = list(colors = plot_df$Color, line = list(color = "#ffffff", width = 1.5)),
      textinfo = "label+percent",
      hovertext = ~Hover,
      hoverinfo = "text",
      sort = FALSE,
      direction = "clockwise"
    ) %>%
      plotly::layout(
        title = list(
          text = paste0(tk %||% "", " - ", ui_str("bs_pie_title", loc)),
          font = list(size = 15, color = "#2c3e50")
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
        margin = list(t = 50, b = 40, l = 20, r = 20),
        annotations = list(
          list(
            text = center_txt,
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 13, color = "#2c3e50"),
            align = "center"
          )
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$eq_structure_plot <- renderPlotly({
    req(identical(.ynow_sidebar_tab(), "dashboard"))
    req(identical(as.character(input$dashboard_fin_report %||% "")[1], "Balance Sheet"))
    tk <- current_ticker()
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    slices <- tryCatch(.eq_structure_slices_now(), error = function(e) NULL)
    if (!is.data.frame(slices) || nrow(slices) < 2L) {
      return(plotly::plotly_empty())
    }
    total <- sum(slices$value, na.rm = TRUE)
    unit <- as.character(slices$unit[[1]] %||% "shares")[1]
    fmt_val <- function(v) {
      if (identical(unit, "currency")) return(format_dollar_abbr(v))
      format(round(v), big.mark = ",", scientific = FALSE)
    }
    center_key <- if (identical(unit, "currency")) "eq_pie_center_equity" else "eq_pie_center_shares"
    slices$Hover <- paste0(
      "<b>", slices$label, "</b><br>",
      ui_str("eq_pie_hover_value", loc), ": <b>", fmt_val(slices$value), "</b><br>",
      ui_str("eq_pie_hover_pct", loc), ": <b>",
      sprintf("%.1f%%", slices$value / total * 100), "</b>"
    )
    plotly::plot_ly(
      slices,
      labels = ~label,
      values = ~value,
      type = "pie",
      hole = 0.48,
      marker = list(colors = slices$color, line = list(color = "#ffffff", width = 1.5)),
      textinfo = "label+percent",
      hovertext = ~Hover,
      hoverinfo = "text",
      sort = FALSE,
      direction = "clockwise"
    ) %>%
      plotly::layout(
        title = list(
          text = paste0(tk %||% "", " - ", ui_str("eq_pie_title", loc)),
          font = list(size = 15, color = "#2c3e50")
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
        margin = list(t = 50, b = 40, l = 20, r = 20),
        annotations = list(
          list(
            text = paste0(ui_str(center_key, loc), "<br><b>", fmt_val(total), "</b>"),
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 13, color = "#2c3e50"),
            align = "center"
          )
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # ==========================================
  # 📈 3. Cash Flow 互動圖表（Dashboard：OCF／ICF／融資 FCF 多線疊圖）
  # ==========================================
  .cf_row_series <- function(cf_df, keyword) {
    if (is.null(cf_df) || !is.data.frame(cf_df) || nrow(cf_df) < 1) return(NULL)
    exact <- which(tolower(trimws(cf_df[[1]])) == tolower(keyword))
    row_idx <- if (length(exact) > 0) exact[1] else {
      hit <- grepl(keyword, cf_df[[1]], ignore.case = TRUE)
      if (!any(hit)) return(NULL)
      which(hit)[1]
    }
    period_cols <- colnames(cf_df)[-1]
    period_cols <- period_cols[!grepl("^ttm$", period_cols, ignore.case = TRUE)]
    if (length(period_cols) < 1) return(NULL)
    vals <- parse_financial_number(as.character(cf_df[row_idx, period_cols, drop = FALSE]))
    # 欄位通常最新→最舊；改舊→新
    ord <- rev(seq_along(period_cols))
    out <- data.frame(
      Period = as.character(period_cols[ord]),
      Value = as.numeric(vals[ord]),
      stringsAsFactors = FALSE
    )
    out[is.finite(out$Value), , drop = FALSE]
  }

  output$cf_plot <- renderPlotly({
    req(identical(.ynow_sidebar_tab(), "dashboard"))
    req(identical(as.character(input$dashboard_fin_report %||% "")[1], "Cash Flow"))
    session_currency(); fx_usd_twd()
    empty_plot <- function(msg) {
      plotly::plotly_empty() %>%
        plotly::layout(
          title = list(
            text = msg, x = 0.5,
            font = list(size = 15, color = "#856404", family = "Arial, sans-serif")
          ),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        )
    }

    req(d_cash_flow(), current_ticker())
    # Always show all three series (OCF / ICF / Financing FCF); no UI multi-select
    series_sel <- APP_DEFAULTS$cf_flow_series %||% c("ocf", "icf", "fcf")

    # 序列色對齊 logo 藍／金／綠（勿用紫系 AI 預設）
    spec <- list(
      ocf = list(keys = CASHFLOW_OCF_PATTERNS, label = "營業現金流 OCF", color = "#0C5484", symbol = "circle"),
      icf = list(keys = CASHFLOW_ICF_PATTERNS, label = "投資現金流 ICF", color = "#C9A227", symbol = "diamond"),
      fcf = list(keys = CASHFLOW_FIN_PATTERNS, label = "籌資現金流 FCF", color = "#249C60", symbol = "square")
    )

    cf <- d_cash_flow()
    parts <- list()
    x_levels <- character(0)
    for (id in c("ocf", "icf", "fcf")) {
      if (!(id %in% series_sel)) next
      sp <- spec[[id]]
      df <- NULL
      for (kw in sp$keys) {
        df <- .cf_row_series(cf, kw)
        if (!is.null(df) && nrow(df) > 0 && any(is.finite(df$Value))) break
        df <- NULL
      }
      if (is.null(df) || nrow(df) < 1) next
      df$Series <- sp$label
      df$SeriesId <- id
      parts[[length(parts) + 1]] <- df
      x_levels <- unique(c(x_levels, as.character(df$Period)))
    }

    if (length(parts) < 1) {
      return(empty_plot("⚠️ 找不到現金流資料（OCF／ICF／融資 FCF）"))
    }
    if (length(x_levels) < 1) return(empty_plot("⚠️ 無可繪製期間"))

    p <- plotly::plot_ly()
    for (dfl in parts) {
      id <- as.character(dfl$SeriesId[1])
      sp <- spec[[id]]
      dfl$Period <- factor(as.character(dfl$Period), levels = x_levels)
      p <- p %>% plotly::add_trace(
        data = dfl,
        x = ~Period, y = ~Value,
        type = "scatter", mode = "lines+markers",
        name = sp$label,
        line = list(color = sp$color, width = 2.4),
        marker = list(
          color = sp$color, size = 9, symbol = sp$symbol,
          line = list(color = "#FFFFFF", width = 1.1)
        ),
        hovertemplate = paste0("<b>", sp$label, "</b><br>%{x}<br>$%{y:,.2f}<extra></extra>"),
        legendgroup = sp$label,
        showlegend = TRUE
      )
    }

    title_main <- paste0(current_ticker(), " · Cash Flow 三線疊圖")
    # 圖內不放副標字樣（會與水平圖例重疊）
    legend_cfg <- list(
      orientation = "h",
      x = 0, y = -0.22,
      xanchor = "left", yanchor = "top",
      bgcolor = "rgba(255,255,255,0.92)",
      bordercolor = "#D5DBDB", borderwidth = 1,
      font = list(size = 12)
    )
    margin_cfg <- list(t = 56, b = 96, l = 70, r = 24)

    p %>%
      plotly::layout(
        title = list(
          text = paste0("<b>", htmltools::htmlEscape(title_main), "</b>"),
          x = 0.02,
          font = list(size = 15, color = "#856404", family = "Arial, sans-serif")
        ),
        xaxis = list(
          title = "期間", tickangle = -30,
          categoryorder = "array", categoryarray = x_levels
        ),
        yaxis = list(
          title = money_label(), tickprefix = money_prefix(),
          separatethousands = TRUE, zeroline = TRUE,
          gridcolor = "#EEF2F5"
        ),
        legend = legend_cfg,
        margin = margin_cfg,
        hovermode = "x unified",
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FAFBFC"
      ) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE, displaylogo = FALSE)
  })

  # ==========================================
  # 🔌 4. 呼叫外部模組 (KPI, FCF, DDM)
  # ==========================================
  
  # --- 歷史股價：process-global TTL cache（多 session 共用，Starter 省 RAM／Yahoo）
  hist_stock_data <- reactive({
    req(current_ticker())
    tk <- toupper(trimws(current_ticker()))
    df_final <- tryCatch(
      ynow_cached_hist_stock_trim(tk),
      error = function(e) {
        .ynow_log("無法取得歷史股價: ", e$message)
        NULL
      }
    )
    df_final
  })
  
  institutional_sop_gate <- reactiveVal(list(unlocked = FALSE, reason = "acks"))

  sop_val_meta <- reactive({
    w <- suppressWarnings(as.numeric(input$wacc_gordon)[1])
    if (!is.finite(w) && identical(input$dcf_mode, "two_stage")) {
      w <- suppressWarnings(as.numeric(input$wacc_stage2)[1])
    }
    sgr <- suppressWarnings(as.numeric(input$sgr)[1])
    spread <- if (is.finite(w) && is.finite(sgr)) w - sgr else NA_real_
    tv_pct <- NA_real_
    tryCatch({
      df_fcf <- fcf_results$df_fcf()
      n_years <- as.numeric(input$years)
      if (is.data.frame(df_fcf) && nrow(df_fcf) == n_years) {
        future_fcfs <- extract_fcff_series(df_fcf)
        r2 <- if (is.finite(w)) w / 100 else NA_real_
        g <- if (is.finite(sgr)) sgr / 100 else NA_real_
        if (is.finite(r2) && is.finite(g) && r2 > g) {
          dfs <- cumprod(1 + rep(r2, n_years))
          pv_fcf <- sum(future_fcfs / dfs)
          tv <- (tail(future_fcfs, 1) * (1 + g)) / (r2 - g) / dfs[n_years]
          if (is.finite(pv_fcf) && is.finite(tv) && (pv_fcf + tv) > 0) {
            tv_pct <- tv / (pv_fcf + tv) * 100
          }
        }
      }
    }, error = function(e) NULL)
    list(wacc_pct = w, sgr_pct = sgr, spread_pp = spread, tv_weight_pct = tv_pct)
  })

  sop_quality_snapshot <- reactive({
    tryCatch({
      is_df <- d_income_statement()
      bs_df <- d_balance_sheet()
      cf_df <- d_cash_flow()
      fs <- compute_report_f_score(is_df, bs_df, cf_df)
      eq <- evaluate_earnings_quality(is_df, bs_df, cf_df, f_score_pack = fs)
      list(
        fscore = suppressWarnings(as.numeric(fs$total)[1]),
        qoe_grade = as.character(eq$grade %||% "")[1],
        qoe_score = suppressWarnings(as.numeric(eq$score)[1]),
        red_n = as.integer(length(eq$red_flags %||% list()))
      )
    }, error = function(e) {
      list(fscore = NA_real_, qoe_grade = "", qoe_score = NA_real_, red_n = 0L)
    })
  })

  # Corporate AML scoring stays server-side for YNOW overlap marks + veto
  # (no dedicated corp_aml sidebar page).
  corp_aml_snapshot <- reactive({
    if (!exists("evaluate_corp_aml_signals", mode = "function")) return(NULL)
    tryCatch({
      is_df <- d_income_statement()
      bs_df <- d_balance_sheet()
      cf_df <- d_cash_flow()
      fs <- tryCatch(compute_report_f_score(is_df, bs_df, cf_df), error = function(e) NULL)
      eq <- tryCatch(
        evaluate_earnings_quality(is_df, bs_df, cf_df, f_score_pack = fs),
        error = function(e) NULL
      )
      n_schilit <- tryCatch({
        if (!exists("evaluate_shenanigans", mode = "function")) return(NA_integer_)
        sh <- evaluate_shenanigans(is_df, bs_df, cf_df)
        if (is.list(sh) && isTRUE(sh$ok)) {
          as.integer(sh$n_alert %||% 0L)[1]
        } else {
          NA_integer_
        }
      }, error = function(e) NA_integer_)
      prof <- tryCatch(company_profile(), error = function(e) NULL)
      ctry <- if (is.list(prof)) as.character(prof$country %||% "")[1] else ""
      evaluate_corp_aml_signals(
        is_df, bs_df, cf_df,
        meta = list(country = ctry, n_schilit = n_schilit),
        eq_pack = eq
      )
    }, error = function(e) NULL)
  })

  # --- 新增 2：掛載投資決策漏斗模組 (Decision Funnel; v13 區間＋可信度) ---
  # primary_band / secondary / confidence 於各估值模組掛載後定義（lazy 查找）
  decision_server(
    id = "main_decision",
    d_is = d_income_statement,
    d_bs = d_balance_sheet,
    d_cf = d_cash_flow,
    intrinsic_val_dcf = stock_price_estimate_val,
    intrinsic_val_ddm = reactive({
      if (!is.null(ddm_results$ddm_price)) ddm_results$ddm_price() else NA
    }),
    intrinsic_val_pb = reactive({
      if (!is.null(pb_results$pb_price)) pb_results$pb_price() else NA
    }),
    intrinsic_val_nav = reactive({
      if (!is.null(nav_results$nav_price)) nav_results$nav_price() else NA
    }),
    intrinsic_val_ri = reactive({
      if (!is.null(ri_results$ri_price)) ri_results$ri_price() else NA
    }),
    current_price = reactive({
      req(scraped_market_cap())
      scraped_market_cap()$price
    }),
    hist_price_data = hist_stock_data,
    industry_text = corp_industry_text,
    model_rec = reactive({ model_sidebar_rec() }),
    primary_band = reactive({ primary_valuation_band() }),
    secondary_point = reactive({ secondary_valuation_point() }),
    model_points = reactive({ all_model_valuation_points() }),
    model_point_labels = reactive({
      labs <- lapply(
        c("dcf", "ddm", "ri", "pb", "nav", "multiples", "sotp"),
        function(k) .model_label(k)
      )
      names(labs) <- c("dcf", "ddm", "ri", "pb", "nav", "multiples", "sotp")
      # Multiples chart / composite: show active method (P/E · EV/EBITDA · …)
      if (exists("rel_results", inherits = TRUE) && !is.null(rel_results$chart_label)) {
        ml <- tryCatch(rel_results$chart_label(), error = function(e) NULL)
        if (nzchar(as.character(ml %||% "")[1])) labs$multiples <- as.character(ml)[1]
      }
      labs
    }),
    active_model_key = reactive({ active_valuation_model_key() }),
    confidence = reactive({ valuation_confidence() }),
    industry_key = reactive(input$industry_choice),
    ui_locale = ui_locale,
    current_ticker = current_ticker,
    aml_pack = corp_aml_snapshot,
    sop_verdict_gate = reactive({
      if (isTRUE(tryCatch(isolate(lite_mode()), error = function(e) FALSE))) {
        return(list(unlocked = TRUE, reason = "lite"))
      }
      institutional_sop_gate()
    })
  )

  # --- 決策檢核（獨立側邊 tab：檢核摘要置頂 + 主題項目並排）---
  decision_checklist_server(
    input, output, session,
    ui_locale = ui_locale,
    primary_band = reactive({ primary_valuation_band() }),
    current_price = reactive({
      tryCatch({
        req(scraped_market_cap())
        scraped_market_cap()$price
      }, error = function(e) NA_real_)
    }),
    model_rec = reactive({ model_sidebar_rec() }),
    g_near_pct = reactive({
      tryCatch(.session_near_term_g_pct(), error = function(e) NA_real_)
    }),
    sgr_pct = reactive({
      sgr <- suppressWarnings(as.numeric(input$sgr)[1])
      if (!is.finite(sgr)) sgr <- suppressWarnings(as.numeric(APP_DEFAULTS$sgr)[1])
      sgr
    }),
    wacc_pct = reactive({
      # Prefer session Gordon WACC; fall back to stage-2 / Ke when needed
      w <- suppressWarnings(as.numeric(input$wacc_gordon)[1])
      if (!is.finite(w) && identical(input$dcf_mode, "two_stage")) {
        w <- suppressWarnings(as.numeric(input$wacc_stage2)[1])
      }
      if (!is.finite(w)) {
        ke <- tryCatch(central_ke(), error = function(e) NA_real_)
        if (is.finite(ke)) w <- ke * 100
      }
      if (!is.finite(w)) w <- suppressWarnings(as.numeric(APP_DEFAULTS$wacc_gordon)[1])
      w
    }),
    hfv_scenarios = reactive({
      s <- tryCatch(bt_fv_conv(), error = function(e) NULL)
      if (is.null(s) || is.null(s$scenarios)) return(NULL)
      s$scenarios
    }),
    fscore_total = reactive({
      tryCatch({
        res <- compute_report_f_score(d_income_statement(), d_balance_sheet(), d_cash_flow())
        suppressWarnings(as.numeric(res$total)[1])
      }, error = function(e) NA_real_)
    }),
    quality_snapshot = sop_quality_snapshot,
    val_meta = sop_val_meta,
    sop_gate_out = institutional_sop_gate,
    current_ticker = current_ticker
  )

  kpi_module_server(
    "kpi", d_income_statement, d_balance_sheet, d_cash_flow,
    reactive(input$industry_choice),
    fundamental_profile = fundamental_profile_rec,
    current_ticker = current_ticker,
    user_has_searched = user_has_searched,
    ui_locale = ui_locale
  )

  # macro_market_server: first visit only (lazy_tabs); BBLab mounts with Company
  

  run_calc_trigger <- reactiveVal(0)
  observeEvent(input$calc, { run_calc_trigger(run_calc_trigger() + 1) })
  observeEvent(d_cash_flow(), { 
    req(is.data.frame(d_cash_flow()), nrow(d_cash_flow()) > 0)
    run_calc_trigger(run_calc_trigger() + 1) 
  })
  
  # ==========================================
  # 🧠 建立「中央折現率大腦」(統一供應 Ke 給各模組)
  # ==========================================
  central_ke <- reactive({
    if (isTRUE(input$use_estimated_re) && !is.null(estimated_re())) {
      estimated_re()
    } else if (!is.null(input$wacc_re)) {
      input$wacc_re / 100
    } else {
      if(!is.null(APP_DEFAULTS$ddm_ke)) APP_DEFAULTS$ddm_ke / 100 else 0.1
    }
  })
  
  # ==========================================
  # 掛載 DDM 模組 (🌟 套用中央大腦 Ke)
  # ==========================================
  ddm_results <- ddm_module_server(
    id = "mod_ddm",
    auto_calc_pulse = reactive(auto_calc_ddm_pulse()),
    ddm_g = reactive({
      if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) as.numeric(input$sgr) else APP_DEFAULTS$ddm_g
    }), 
    ddm_ke = reactive({ central_ke() * 100 }),  # 🌟 連動！
    
    scraped_d0 = reactive({
      # 優先：財報推算每股股利（報價股約當股數）；其次：Summary 股利欄（報價幣 DPS）
      cf <- d_cash_flow()
      bs <- d_balance_sheet()
      q_ccy <- quote_currency()
      f_ccy <- statement_currency()
      if (is.data.frame(cf) && nrow(cf) > 0 && is.data.frame(bs) && nrow(bs) > 0) {
        # 報價幣≠財報幣且 CF 未成功 FX 換算 → 勿用原幣總股利÷股數冒充報價 DPS
        money_ok <- {
          mc <- normalize_ccy(attr(cf, "money_ccy") %||% f_ccy)
          if (!statement_quote_units_differ(f_ccy, q_ccy)) {
            TRUE
          } else {
            isTRUE(attr(cf, "money_scaled")) && !is.na(mc) &&
              (identical(mc, normalize_ccy(q_ccy)) ||
                 identical(mc, "USD") || identical(mc, "TWD"))
          }
        }
        if (isTRUE(money_ok)) {
          div_paid <- select_current_metric(cf, "Cash Dividends Paid", "flow")
          sh <- tryCatch(
            resolve_valuation_shares(
              bs, summary_data(),
              ticker = current_ticker() %||% "",
              quote_currency = q_ccy,
              financial_currency = f_ccy
            ),
            error = function(e) NULL
          )
          shares <- NA_real_
          if (!is.null(sh) && is.finite(sh$shares) && sh$shares > 0) {
            if (shares_auto_adjust_method(sh$method)) {
              shares <- sh$shares
            } else if (!statement_quote_units_differ(f_ccy, q_ccy)) {
              shares <- sh$shares
            }
          } else if (!statement_quote_units_differ(f_ccy, q_ccy)) {
            shares <- select_current_metric_any(bs, SHARE_PATTERNS, "stock")
          }
          if (!is.na(div_paid) && is.finite(shares) && shares > 0) {
            return(round(abs(div_paid) / shares, 2))
          }
        }
      }
      df <- summary_data()
      if (!is.null(df)) {
        div_row <- df[grepl("Dividend", df$Item, ignore.case = TRUE), ]
        if (nrow(div_row) > 0) {
          fallback <- suppressWarnings(as.numeric(stringr::str_extract(div_row$Value[1], "^[0-9.]+")))
          if (is.finite(fallback) && fallback >= 0) return(round(fallback, 2))
        }
      }
      0
    }),
    
    summary_df = summary_data,
    d_cash_flow = d_cash_flow, 
    d_balance_sheet = d_balance_sheet,
    d_income_statement = d_income_statement,
    current_ticker = current_ticker,
    quote_currency = quote_currency,
    financial_currency = statement_currency,
    capm_rf = reactive(suppressWarnings(as.numeric(input$capm_rf)[1])),
    capm_beta = reactive(suppressWarnings(as.numeric(input$capm_beta)[1])),
    capm_rm = reactive(suppressWarnings(as.numeric(input$capm_rm)[1])),
    use_estimated_re = reactive(isTRUE(input$use_estimated_re))
  )
  
  # ==========================================
  # FCFE 轉換參數（與 .dcf_valuation_bundle 同一套：稅後利息＋g×負債）
  # ==========================================
  .dcf_fcfe_bridge <- reactive({
    latest_debt <- .shared_bridge()$debt
    if (!is.finite(latest_debt) || latest_debt < 0) latest_debt <- 0
    rd <- suppressWarnings(as.numeric(input$wacc_rd)[1]) / 100
    if (!is.finite(rd) || rd < 0) rd <- 0
    tax <- suppressWarnings(as.numeric(input$wacc_tax)[1]) / 100
    if (!is.finite(tax)) tax <- APP_DEFAULTS$wacc_tax / 100
    iat <- after_tax_interest(latest_debt, rd, tax)
    g_terminal <- if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) {
      as.numeric(input$sgr) / 100
    } else {
      APP_DEFAULTS$sgr / 100
    }
    ke <- tryCatch(as.numeric(central_ke())[1], error = function(e) NA_real_)
    list(debt = latest_debt, iat = iat, g = g_terminal, ke = ke, rd = rd, tax = tax)
  })

  # ==========================================
  # 呼叫 FCFF／FCFE 預測模組
  # ==========================================
    fcf_results <- fcf_projection_module_server(
    id = "mod_fcf", 
    d_balance_sheet = d_balance_sheet,
    d_income_statement = d_income_statement, 
    d_cash_flow = d_cash_flow,
    input_mode = reactive(input$dcf_mode), 
    input_years = reactive(input$years),
    sgr = reactive(input$sgr), 
    g_stage1 = reactive(input$g_stage1), 
    g_stage2 = reactive(input$sgr), 
    yr_stage1 = reactive(input$yr_stage1),
    input_manual_fcf = reactive(input$manual_fcf),
    calc_trigger = run_calc_trigger,
    global_est_g = estimated_g,
    global_g_method = reactive(input$g_growth_method),
    global_raw_g = estimated_g_raw,
    dcf_claim = reactive(input$dcf_claim %||% "fcff"),
    fcfe_interest_after_tax = reactive(.dcf_fcfe_bridge()$iat),
    fcfe_debt0 = reactive(.dcf_fcfe_bridge()$debt),
    fcfe_g = reactive(.dcf_fcfe_bridge()$g),
    # Inline (not .session_tax_decimal): that helper is defined later in this file.
    tax_ratio = reactive({
      t <- suppressWarnings(as.numeric(input$wacc_tax)[1])
      if (!is.finite(t)) t <- APP_DEFAULTS$wacc_tax
      ias12_tax_ratio(t, from_pct = TRUE)
    }),
    shared_bridge = function() .shared_bridge()
  )
  
  observeEvent({
    input$sgr; input$g_stage1; input$dcf_mode
  }, {
    run_calc_trigger(run_calc_trigger() + 1)
  }, ignoreInit = TRUE)

  # 切換至 Two-Stage 時，確保 g1 帶入目前預估營收成長率
  observeEvent(input$dcf_mode, {
    if (!identical(input$dcf_mode, "two_stage")) return()
    eg <- suppressWarnings(as.numeric(isolate(estimated_g()))[1])
    if (!is.finite(eg)) return()
    cur <- suppressWarnings(as.numeric(input$g_stage1)[1])
    if (is.null(input$g_stage1) || !is.finite(cur) || abs(cur - eg) > 1e-4) {
      updateNumericInput(session, "g_stage1", value = round(eg, 2))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$dcf_claim, {
    claim <- input$dcf_claim %||% "fcff"
    tag <- dcf_cf_tag(claim)
    updateSelectInput(
      session, "g_growth_method",
      label = sprintf("預估營收成長率（驅動 %s 預測）", tag)
    )
  }, ignoreInit = FALSE)

  output$dcf_claim_suggest <- renderUI({
    fcf_hist <- tryCatch(
      select_clean_metric_row(d_cash_flow(), "Free Cash Flow", include_ttm = FALSE),
      error = function(e) NULL
    )
    fcf_hist <- suppressWarnings(as.numeric(fcf_hist))
    last_fcff <- NA_real_
    if (length(fcf_hist)) {
      hit <- which(is.finite(fcf_hist))
      if (length(hit)) last_fcff <- fcf_hist[hit[1]]
    }
    br <- .dcf_fcfe_bridge()
    last_fcfe <- tryCatch(
      fcff_to_fcfe(last_fcff, interest_after_tax = br$iat, debt0 = br$debt, g_path = 0)[1],
      error = function(e) NA_real_
    )
    rec <- recommend_dcf_claim(
      d_bs = d_balance_sheet(),
      fcff = last_fcff,
      fcfe = last_fcfe
    )
    cur <- as.character(input$dcf_claim %||% "fcff")[1]
    if (isTRUE(rec$fcfe_ok)) {
      head <- "建議：負債比相對穩定，FCFF 與 FCFE 皆可用（預設仍為 FCFF；不自動切換）。"
    } else {
      head <- "建議採用 FCFF／WACC（不自動切換）。"
    }
    if (identical(cur, "fcfe") && !isTRUE(rec$fcfe_ok)) {
      head <- paste0(head, " 目前選 FCFE：請確認淨舉債假設與 Ke 配對。")
    }
    tags$div(
      class = "ynow-dcf-claim-suggest",
      tags$b(head),
      tags$ul(
        style = "margin: 6px 0 0 18px; padding: 0;",
        lapply(rec$reasons, function(x) tags$li(x))
      )
    )
  })

  output$dcf_chart_help <- renderUI({
    tag <- dcf_cf_tag(input$dcf_claim %||% "fcff")
    helpText(sprintf(
      "提示：圖含歷史現金流與預測 %s；折現線為各年現金流以 %s 折現的現值（不含永續終值）。終值現值見圖下註，不會疊進最後一年以免壓扁軸距。",
      tag, dcf_disc_tag(input$dcf_claim %||% "fcff")
    ))
  })

  output$dcf_sens_help <- renderUI({
    claim <- input$dcf_claim %||% "fcff"
    disc <- dcf_disc_tag(claim)
    p(helpText(sprintf(
      "軸心採用基礎設定／Dashboard 目前的 SGR 與 %s；觀察鄰近組合下的每股內在價值變化。",
      disc
    )))
  })

  output$dcf_wacc_fcfe_note <- renderUI({
    if (!dcf_claim_is_fcfe(input$dcf_claim)) return(NULL)
    tags$div(
      style = "background:#f5f5f5; border-left:4px solid #222222; padding:10px 12px; margin-bottom:12px; font-size:13px;",
      tags$b("FCFE 模式："),
      "本頁折現率為 Ke（下方 rₑ／CAPM）。WACC 僅供對照；FCFE＝FCFF−稅後利息＋淨舉債，轉換仍使用 rᵈ 與稅率。"
    )
  })

  output$dcf_disc_formula_banner <- renderUI({
    banner_style <- "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"
    if (dcf_claim_is_fcfe(input$dcf_claim)) {
      tagList(
        div("Ke = Rf + β × (Rm − Rf)", style = banner_style),
        div(
          "WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)　（對照；FCFE 轉換仍用 rᵈ、T）",
          style = "font-size: 14px; color: #555; text-align: center; margin-bottom: 15px;"
        )
      )
    } else {
      div("WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)", style = banner_style)
    }
  })

  # ==========================================
  # 🌱 中央永續成長率方法（同步 DCF sgr／RI ri_g；DDM g 可選同步）
  # ==========================================
  .current_wacc_pct <- function() {
    # Prefer live WACC inputs (auto-filled from CAPM/WACC calc on the DCF tab).
    if (isTRUE(input$dcf_mode == "two_stage") && !is.null(input$wacc_stage2) && is.finite(input$wacc_stage2)) {
      return(as.numeric(input$wacc_stage2))
    }
    if (!is.null(input$wacc_gordon) && is.finite(input$wacc_gordon)) {
      return(as.numeric(input$wacc_gordon))
    }
    if (!is.null(calculated_wacc()) && is.finite(calculated_wacc())) {
      return(as.numeric(calculated_wacc()) * 100)
    }
    APP_DEFAULTS$wacc_gordon
  }

  .current_rf_detail <- function() {
    det <- tryCatch({
      if (exists("cached_get_risk_free_rate_detail", mode = "function")) {
        cached_get_risk_free_rate_detail()
      } else if (exists("get_risk_free_rate_detail", mode = "function")) {
        get_risk_free_rate_detail()
      } else {
        r <- as.numeric(cached_get_risk_free_rate())
        list(
          rf_pct = r, source = "live", label = "Rf",
          symbol = "", is_fallback = FALSE
        )
      }
    }, error = function(e) {
      list(
        rf_pct = NA_real_, source = "fallback", label = "Rf",
        symbol = "", is_fallback = TRUE
      )
    })
    # Prefer live / last-known scrape for Macro anchoring
    if (!isTRUE(det$is_fallback) && is.finite(suppressWarnings(as.numeric(det$rf_pct)[1]))) {
      return(det)
    }
    sess <- suppressWarnings(as.numeric(input$capm_rf)[1])
    if (is.finite(sess) && sess > 0) {
      return(list(
        rf_pct = round(sess, 2),
        source = "session",
        label = as.character(det$label %||% "Rf")[1],
        symbol = as.character(det$symbol %||% "")[1],
        is_fallback = FALSE
      ))
    }
    fb <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_rf)[1])
    if (!is.finite(fb) || fb <= 0) fb <- 5
    list(
      rf_pct = round(fb, 2),
      source = "fallback",
      label = as.character(det$label %||% "Rf")[1],
      symbol = as.character(det$symbol %||% "")[1],
      is_fallback = TRUE
    )
  }

  .current_rf_pct <- function() {
    suppressWarnings(as.numeric(.current_rf_detail()$rf_pct)[1])
  }

  central_lifecycle <- reactive({
    tryCatch(ui_locale(), error = function(e) NULL)
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    raw_sel <- input$lifecycle_stage %||% APP_DEFAULTS$lifecycle_stage
    mapped <- if (exists("map_legacy_lifecycle_stage", mode = "function")) {
      map_legacy_lifecycle_stage(raw_sel)
    } else {
      list(selected_stage = raw_sel, selection_mode = "auto", legacy_value = raw_sel)
    }
    classify_lifecycle_result(
      industry_text = corp_industry_text() %||% "",
      industry_key = input$industry_choice %||% "",
      ticker = current_ticker() %||% APP_DEFAULTS$stock_code,
      d_is = tryCatch(d_income_statement(), error = function(e) NULL),
      d_bs = tryCatch(d_balance_sheet(), error = function(e) NULL),
      d_cf = tryCatch(d_cash_flow(), error = function(e) NULL),
      wacc_pct = .current_wacc_pct(),
      selected_stage = mapped$selected_stage,
      selection_mode = if (identical(as.character(raw_sel)[1], "auto")) "auto" else "manual",
      legacy_value = mapped$legacy_value,
      locale = loc
    )
  })

  central_perpetual_g <- reactive({
    # Locale affects Macro reason bilingual copy
    tryCatch(ui_locale(), error = function(e) NULL)
    d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
    d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    d_cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
    rf_det <- .current_rf_detail()
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    life <- tryCatch(central_lifecycle(), error = function(e) NULL)
    estimate_perpetual_g(
      method = input$perpetual_g_method %||% APP_DEFAULTS$perpetual_g_method,
      rf_pct = rf_det$rf_pct,
      rf_source = rf_det$source,
      rf_label = rf_det$label,
      locale = loc,
      d_is = d_is,
      d_bs = d_bs,
      d_cf = d_cf,
      industry_text = corp_industry_text() %||% "",
      ticker = current_ticker() %||% APP_DEFAULTS$stock_code,
      lifecycle_stage = input$lifecycle_stage %||% "auto",
      wacc_pct = .current_wacc_pct(),
      industry_key = input$industry_choice %||% "",
      lifecycle_result = life
    )
  })

  # SGR tab：頂部 valueBox（與 BETA Overview 同款 small-box）
  .session_near_term_g_pct <- function() {
    # 近期末／session g（非終值 SGR）：優先 estimated_g，其次 g_stage1
    eg <- tryCatch(estimated_g(), error = function(e) NULL)
    eg <- suppressWarnings(as.numeric(eg)[1])
    if (is.finite(eg)) return(eg)
    g1 <- suppressWarnings(as.numeric(input$g_stage1)[1])
    if (is.finite(g1)) return(g1)
    suppressWarnings(as.numeric(APP_DEFAULTS$g_stage1)[1])
  }

  # SGR KPI hosts match BETA Overview: bare small-box (width=NULL) inside col-sm-6.
  output$vbx_sgr_pct <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    sgr_pct <- suppressWarnings(as.numeric(input$sgr)[1])
    if (!is.finite(sgr_pct)) sgr_pct <- suppressWarnings(as.numeric(APP_DEFAULTS$sgr)[1])
    valueBox(
      if (is.finite(sgr_pct)) paste0(round(sgr_pct, 2), " %") else "N/A",
      ui_str("vbx_sgr_subtitle", loc),
      icon = icon("infinity"),
      color = "maroon",
      width = NULL
    )
  })

  output$vbx_session_g <- renderValueBox({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    g_pct <- .session_near_term_g_pct()
    valueBox(
      if (is.finite(g_pct)) paste0(round(g_pct, 2), " %") else "N/A",
      ui_str("vbx_session_g_subtitle", loc),
      icon = icon("chart-line"),
      color = "olive",
      width = NULL
    )
  })

  output$txt_perpetual_g_reason <- renderUI({
    est <- central_perpetual_g()
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    tags$div(
      style = "background:#f8f9fa; border-left:4px solid #e67e22; padding:8px 12px; margin-bottom:12px; font-size:13px; color:#333;",
      tags$b(ui_str("sgr_estimate_reason_prefix", loc)), est$reason %||% ""
    )
  })

  output$txt_sgr_identity <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    est <- tryCatch(central_perpetual_g(), error = function(e) NULL)
    adopted <- if (!is.null(est)) suppressWarnings(as.numeric(est$g_pct)[1]) else NA_real_
    if (!is.finite(adopted)) adopted <- suppressWarnings(as.numeric(input$sgr)[1])
    ni <- tryCatch(select_current_metric_any(d_income_statement(), NET_INCOME_PATTERNS, "flow"), error = function(e) NA_real_)
    eq <- tryCatch(select_current_metric_any(d_balance_sheet(), EQUITY_PATTERNS, "stock"), error = function(e) NA_real_)
    div_paid <- tryCatch(select_current_metric(d_cash_flow(), "Cash Dividends Paid", "flow"), error = function(e) NA_real_)
    ident <- tryCatch(fundamental_sgr_identity(ni, eq, div_paid), error = function(e) NULL)
    fmt_pct <- function(x) {
      if (!is.finite(suppressWarnings(as.numeric(x)[1]))) return("—")
      paste0(format(round(as.numeric(x), 2), nsmall = 2), "%")
    }
    fmt_num <- function(x) {
      if (!is.finite(suppressWarnings(as.numeric(x)[1]))) return("—")
      format(round(as.numeric(x), 0), big.mark = ",", scientific = FALSE)
    }
    ke_pct <- suppressWarnings(as.numeric(input$wacc_re)[1])
    if (!is.finite(ke_pct)) ke_pct <- NA_real_
    gate_w <- tryCatch(terminal_gordon_gate(adopted, .current_wacc_pct()), error = function(e) NULL)
    gate_k <- tryCatch(terminal_gordon_gate(adopted, ke_pct), error = function(e) NULL)
    gate_line <- function(label, gate) {
      if (is.null(gate) || !is.finite(gate$spread_pp)) {
        return(tags$div(tags$b(label), " ", ui_str("sgr_gate_na", loc)))
      }
      verdict <- if (isTRUE(gate$computable)) ui_str("sgr_gate_pass", loc) else ui_str("sgr_gate_fail", loc)
      tone <- if (isTRUE(gate$computable)) "#1e8449" else "#922b21"
      tags$div(
        tags$b(label), " ",
        fmt_pct(gate$discount_pct), " − ", fmt_pct(gate$g_pct),
        " = ", format(round(gate$spread_pp, 2), nsmall = 2), " pp. ",
        tags$span(style = paste0("color:", tone, ";"), verdict)
      )
    }
    match_line <- if (is.null(ident) || !isTRUE(ident$ok)) {
      ui_str("sgr_audit_unavailable", loc)
    } else if (isTRUE(ident$matches_formula)) {
      ui_str("sgr_audit_match", loc)
    } else {
      ui_str("sgr_audit_mismatch", loc)
    }
    tags$div(
      style = "background:#f4f8fb; border:1px solid #d6e2ea; padding:10px 12px; margin:8px 0 12px 0; font-size:13px; color:#333; line-height:1.55;",
      tags$div(style = "font-weight:700; margin-bottom:6px;", ui_str("sgr_audit_title", loc)),
      tags$div(tags$b(ui_str("sgr_audit_adopted", loc)), ": ", fmt_pct(adopted)),
      tags$div(tags$b(ui_str("sgr_audit_formula", loc)), ": ", if (is.null(ident)) "—" else fmt_pct(ident$g_pct)),
      tags$div(
        ui_str("sgr_audit_ni", loc), " ", if (is.null(ident)) "—" else fmt_num(ident$ni), " · ",
        ui_str("sgr_audit_equity", loc), " ", if (is.null(ident)) "—" else fmt_num(ident$equity), " · ",
        ui_str("sgr_audit_div", loc), " ", if (is.null(ident)) "—" else fmt_num(ident$dividends)
      ),
      tags$div(
        ui_str("sgr_audit_roe", loc), " ", if (is.null(ident)) "—" else fmt_pct(ident$roe_pct), " · ",
        ui_str("sgr_audit_payout", loc), " ", if (is.null(ident) || !is.finite(ident$payout)) "—" else fmt_pct(ident$payout * 100), " · ",
        ui_str("sgr_audit_retention", loc), " ", if (is.null(ident) || !is.finite(ident$retention)) "—" else fmt_pct(ident$retention * 100)
      ),
      tags$div(style = "margin-top:4px; color:#555;", match_line),
      tags$div(style = "font-weight:700; margin:10px 0 4px 0;", ui_str("sgr_gate_title", loc)),
      gate_line("WACC", gate_w),
      gate_line("Ke", gate_k)
    )
  })

  output$txt_scenario_audit <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    tab <- tryCatch(valuation_scenario_audit(loc), error = function(e) NULL)
    if (is.null(tab) || !is.data.frame(tab) || nrow(tab) < 1L) return(NULL)
    fmt_shock <- function(x) {
      if (!is.finite(suppressWarnings(as.numeric(x)[1]))) return("—")
      format(round(as.numeric(x), 2), nsmall = 2)
    }
    header <- tags$tr(
      tags$th(ui_str("sgr_col_model", loc)),
      tags$th(ui_str("sgr_col_case", loc)),
      tags$th(ui_str("sgr_col_param", loc)),
      tags$th(ui_str("sgr_col_shock", loc)),
      tags$th(ui_str("sgr_col_rule", loc))
    )
    body <- lapply(seq_len(nrow(tab)), function(i) {
      tags$tr(
        tags$td(tab$model[[i]]),
        tags$td(tab$case[[i]]),
        tags$td(tab$parameter[[i]]),
        tags$td(fmt_shock(tab$shock[[i]])),
        tags$td(tab$rule[[i]])
      )
    })
    tags$div(
      style = "margin:0;",
      tags$div(
        id = "ynow_sgr_scenario_title",
        style = "font-weight:700; margin-bottom:4px; font-size:14px;",
        ui_str("sgr_scenario_title", loc)
      ),
      tags$p(
        id = "ynow_sgr_scenario_help",
        style = "font-size:12px; color:#666; line-height:1.45; margin:0 0 8px 0;",
        ui_str("sgr_scenario_help", loc)
      ),
      tags$div(
        style = "overflow-x:auto;",
        tags$table(
          class = "table table-condensed table-striped",
          style = "font-size:12px; margin-bottom:0;",
          tags$thead(header),
          tags$tbody(body)
        )
      )
    )
  })

  output$txt_perpetual_g_method_suggest <- renderUI({
    est <- central_perpetual_g()
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    cur <- as.character(input$perpetual_g_method %||% APP_DEFAULTS$perpetual_g_method)[1]
    rec <- as.character(est$recommended_method %||% "")[1]
    if (!nzchar(rec)) return(NULL)
    same <- identical(cur, rec)
    tone_bd <- if (same) "#27ae60" else "#2980b9"
    tone_bg <- if (same) "#eafaf1" else "#ebf5fb"
    evidence <- as.character(est$lifecycle_evidence %||% "")[1]
    auto_tier <- as.character(est$auto_lifecycle %||% "")[1]
    tier_lab <- tryCatch(
      lifecycle_stage_label(auto_tier, locale = loc),
      error = function(e) auto_tier
    )
    tags$div(
      style = paste0(
        "background:", tone_bg, "; border-left:4px solid ", tone_bd,
        "; padding:8px 12px; margin:8px 0 10px 0; font-size:13px; color:#333; line-height:1.5;"
      ),
      tags$div(
        tags$b(
          if (same) ui_str("sgr_suggest_adopted", loc) else ui_str("sgr_suggest_pending", loc)
        ),
        est$recommend_label %||% rec
      ),
      tags$div(style = "margin-top:4px; color:#555;", est$recommend_reason %||% ""),
      if (nzchar(evidence) || nzchar(auto_tier)) {
        tags$div(
          style = "margin-top:6px; color:#444; font-size:12px;",
          tags$b(ui_str("sgr_suggest_evidence_prefix", loc)),
          if (nzchar(auto_tier)) {
            paste0(ui_str("sgr_suggest_auto_tier", loc), tier_lab, "（", auto_tier, "）。")
          } else {
            NULL
          },
          if (nzchar(evidence)) evidence else NULL
        )
      },
      if (!same) {
        tags$div(
          style = "margin-top:8px;",
          actionButton(
            "btn_apply_sgr_method_suggest",
            paste0(ui_str("sgr_suggest_apply", loc), est$recommend_label %||% rec),
            class = "btn-sm btn-primary",
            icon = icon("magic")
          )
        )
      }
    )
  })

  output$txt_lifecycle_classification <- renderUI({
    life <- tryCatch(central_lifecycle(), error = function(e) NULL)
    if (is.null(life)) return(NULL)
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    est <- tryCatch(central_perpetual_g(), error = function(e) NULL)
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    diags <- tryCatch({
      lifecycle_terminal_diagnostics(
        lifecycle_result = life,
        g_term = if (!is.null(est)) est$g_pct else NA_real_,
        wacc = .current_wacc_pct(),
        long_run_nominal_g_cap = LIFECYCLE_CONFIG$terminal$long_run_nominal_g_cap_pct
      )
    }, error = function(e) list())
    lab <- function(id) {
      tryCatch(lifecycle_stage_label(id, locale = loc), error = function(e) id)
    }
    auto_id <- as.character(life$autoDetectedStage %||% "")[1]
    sel_id <- as.character(life$selectedStage %||% "")[1]
    sec_id <- as.character(life$secondaryCandidate %||% "")[1]
    rec_primary <- as.character(rec$primary %||% "")[1]
    rec_summary <- as.character(rec$summary_method %||% "")[1]
    adopted <- as.character(input$dcf_claim %||% rec_primary)[1]
    g_txt <- if (!is.null(est) && is.finite(est$g_pct)) sprintf("%.2f%%", est$g_pct) else "—"
    tags$div(
      style = "background:#f4f6f7; border-left:4px solid #7f8c8d; padding:8px 12px; margin:8px 0 10px 0; font-size:13px; color:#333; line-height:1.55;",
      tags$div(tags$b(ui_str("lifecycle_auto_detected", loc)), lab(auto_id),
               if (nzchar(auto_id)) paste0(" (", auto_id, ")") else ""),
      tags$div(tags$b(ui_str("lifecycle_selected", loc)), lab(sel_id),
               if (nzchar(sel_id)) paste0(" (", sel_id, ")") else "",
               " · ",
               if (identical(life$selectionMode, "manual")) {
                 ui_str("lifecycle_mode_manual", loc)
               } else {
                 ui_str("lifecycle_mode_auto", loc)
               }),
      if (nzchar(sec_id) && !is.na(sec_id)) {
        tags$div(tags$b(ui_str("lifecycle_secondary", loc)), lab(sec_id),
                 paste0(" (", sec_id, ")"))
      },
      tags$div(
        tags$b(ui_str("lifecycle_confidence", loc)),
        if (is.finite(life$confidenceScore)) sprintf("%.2f", life$confidenceScore) else "—",
        " · ",
        tags$b(ui_str("lifecycle_completeness", loc)),
        if (is.finite(life$dataCompletenessScore)) sprintf("%.2f", life$dataCompletenessScore) else "—"
      ),
      if (length(life$classificationReasons)) {
        tags$div(
          tags$b(ui_str("lifecycle_reasons", loc)),
          tags$ul(style = "margin:4px 0 0 18px;",
                  lapply(life$classificationReasons, tags$li))
        )
      },
      if (length(life$missingInputs)) {
        tags$div(tags$b(ui_str("lifecycle_missing", loc)),
                 paste(life$missingInputs, collapse = ", "))
      },
      if (is.finite(life$recommendedForecastYears)) {
        tags$div(tags$b(ui_str("lifecycle_forecast_years", loc)),
                 as.integer(life$recommendedForecastYears))
      },
      if (nzchar(rec_summary) || nzchar(rec_primary)) {
        tags$div(
          style = "margin-top:6px;",
          tags$b(ui_str("lifecycle_rec_suggested", loc)), rec_summary,
          if (nzchar(rec_primary)) paste0(" · ", ui_str("lifecycle_rec_adopted", loc), rec_primary)
        )
      },
      tags$div(
        style = "margin-top:4px;",
        tags$b(ui_str("lifecycle_terminal_summary", loc)),
        paste0(" g=", g_txt)
      ),
      if (length(diags)) {
        tags$div(
          style = "margin-top:6px; font-size:12px; color:#555;",
          lapply(diags, function(d) {
            tags$div(format_valuation_diagnostic(d, loc))
          })
        )
      }
    )
  })

  observeEvent(input$btn_apply_sgr_method_suggest, {
    est <- tryCatch(central_perpetual_g(), error = function(e) NULL)
    rec <- as.character(est$recommended_method %||% "")[1]
    if (!nzchar(rec)) return()
    updateSelectInput(session, "perpetual_g_method", selected = rec)
    # Lifecycle 建議時，檔位跟自動偵測對齊，方便一次套用
    if (identical(rec, "lifecycle") && nzchar(as.character(est$auto_lifecycle %||% "")[1])) {
      updateSelectInput(session, "lifecycle_stage", selected = "auto")
    }
    showNotification(
      .ui_msg("notif_sgr_method_switched", label = est$recommend_label %||% rec),
      type = "message", duration = 5, id = "ynow_apply_sgr_method"
    )
  })

  .push_perpetual_g <- function(est, notify_two_stage = TRUE) {
    if (is.null(est) || !is.finite(est$g_pct)) return(invisible(NULL))
    g_val <- round(as.numeric(est$g_pct), 2)
    if (is.null(input$sgr) || is.na(as.numeric(input$sgr)) || abs(as.numeric(input$sgr) - g_val) > 1e-4) {
      updateNumericInput(session, "sgr", value = g_val)
    }
    # DDM 股利 g：僅在勾選「與中央同步」時覆寫，允許與 FCFF 終值 SGR 分開
    if (isTRUE(input[["mod_ddm-sync_g"]] %||% TRUE)) {
      updateNumericInput(session, "mod_ddm-g", value = g_val)
    }
    updateNumericInput(session, "mod_ri-ri_g", value = g_val)

    if (isTRUE(est$suggest_two_stage)) {
      # 不再自動切換 dcf_mode（維持 Gordon 預設）；僅同步 g1 供使用者改 Two-Stage 時使用
      if (isTRUE(notify_two_stage) && !identical(input$dcf_mode, "two_stage")) {
        showNotification(
          .ui_msg("notif_lifecycle_two_stage"),
          type = "message", duration = 6, id = "ynow_lifecycle_two_stage"
        )
      }
      if (is.finite(est$g_stage1_pct)) {
        g1 <- as.numeric(est$g_stage1_pct)
        if (is.null(input$g_stage1) || is.na(as.numeric(input$g_stage1)) ||
            abs(as.numeric(input$g_stage1) - g1) > 1e-4) {
          updateNumericInput(session, "g_stage1", value = g1)
        }
      }
    }
    invisible(g_val)
  }

  observeEvent({
    list(
      input$perpetual_g_method,
      input$lifecycle_stage,
      input$capm_rf,
      scraped_financials(),
      corp_industry_text(),
      current_ticker(),
      calculated_wacc(),
      input$wacc_gordon,
      input$wacc_stage2
    )
  }, {
    est <- central_perpetual_g()
    .push_perpetual_g(est, notify_two_stage = TRUE)
  }, ignoreInit = FALSE)
  
  observeEvent(input$years, {
    n <- as.numeric(input$years)
    if (is.na(n) || n <= 1) return()
    safe_yr1 <- clamp_yr_stage1(n, input$yr_stage1, APP_DEFAULTS$yr_stage1)
    if (!identical(as.numeric(input$yr_stage1), as.numeric(safe_yr1))) {
      updateNumericInput(session, "yr_stage1", value = safe_yr1)
    }
  }, ignoreInit = TRUE)
  
  # ==========================================
  # 呼叫 RI (剩餘收益) 模組 (🌟 套用中央大腦 Ke)
  # ==========================================
  ri_results <- ri_module_server(
    id = "mod_ri", 
    d_income_statement = d_income_statement, 
    d_balance_sheet = d_balance_sheet, 
    d_cash_flow = d_cash_flow, 
    global_re = central_ke,
    global_g = reactive({
      if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) as.numeric(input$sgr) else APP_DEFAULTS$sgr
    }),
    industry_choice = reactive(input$industry_choice),
    current_price = reactive({
      tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    }),
    market_cap = reactive({
      extract_quote_price_mcap(summary_data())$market_cap
    }),
    quote_price = reactive({
      extract_quote_price_mcap(summary_data())$price
    }),
    current_ticker = current_ticker,
    quote_currency = quote_currency,
    financial_currency = statement_currency,
    capm_rf = reactive(suppressWarnings(as.numeric(input$capm_rf)[1])),
    capm_beta = reactive(suppressWarnings(as.numeric(input$capm_beta)[1])),
    capm_rm = reactive(suppressWarnings(as.numeric(input$capm_rm)[1])),
    use_estimated_re = reactive(isTRUE(input$use_estimated_re)),
    auto_calc_pulse = reactive(auto_calc_ri_pulse()),
    ui_locale = ui_locale
  )
  
  # ==========================================
  # 呼叫 P/B／資產估值模組
  # ==========================================
  pb_results <- pb_asset_module_server(
    id = "mod_pb",
    auto_calc_pulse = reactive(auto_calc_pb_pulse()),
    d_balance_sheet = d_balance_sheet,
    d_income_statement = d_income_statement,
    current_price = reactive({
      tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    }),
    market_cap = reactive({
      extract_quote_price_mcap(summary_data())$market_cap
    }),
    quote_price = reactive({
      extract_quote_price_mcap(summary_data())$price
    }),
    current_ticker = current_ticker,
    quote_currency = quote_currency,
    financial_currency = statement_currency,
    industry_choice = reactive(input$industry_choice),
    industry_text = corp_industry_text,
    central_ke = central_ke,
    central_g_pct = reactive({
      if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) as.numeric(input$sgr) else APP_DEFAULTS$sgr
    }),
    hist_prices = hist_stock_data,
    capm_rf = reactive(suppressWarnings(as.numeric(input$capm_rf)[1])),
    capm_beta = reactive(suppressWarnings(as.numeric(input$capm_beta)[1])),
    capm_rm = reactive(suppressWarnings(as.numeric(input$capm_rm)[1])),
    use_estimated_re = reactive(isTRUE(input$use_estimated_re)),
    ui_locale = ui_locale
  )

  # ==========================================
  # Relative multiples: P/E · Forward P/E · PEG · EV/FCF
  # ==========================================
  rel_results <- relative_multiples_module_server(
    id = "mod_rel",
    auto_calc_pulse = reactive(auto_calc_rel_pulse()),
    summary_df = summary_data,
    d_income_statement = d_income_statement,
    d_balance_sheet = d_balance_sheet,
    d_cash_flow = d_cash_flow,
    current_price = reactive({
      tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    }),
    market_cap = reactive({
      extract_quote_price_mcap(summary_data())$market_cap
    }),
    quote_price = reactive({
      extract_quote_price_mcap(summary_data())$price
    }),
    current_ticker = current_ticker,
    quote_currency = quote_currency,
    financial_currency = statement_currency,
    central_sgr_pct = reactive({
      if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) {
        as.numeric(input$sgr)
      } else {
        APP_DEFAULTS$sgr
      }
    }),
    ui_locale = ui_locale,
    shared_bridge = function() .shared_bridge()
  )

  # ==========================================
  # SOTP — structural Sum-of-the-Parts (separate from Multiples)
  # ==========================================
  sotp_results <- sotp_module_server(
    id = "mod_sotp",
    auto_calc_pulse = reactive(auto_calc_sotp_pulse()),
    d_income_statement = d_income_statement,
    d_balance_sheet = d_balance_sheet,
    current_price = reactive({
      tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    }),
    market_cap = reactive({
      extract_quote_price_mcap(summary_data())$market_cap
    }),
    quote_price = reactive({
      extract_quote_price_mcap(summary_data())$price
    }),
    current_ticker = current_ticker,
    quote_currency = quote_currency,
    financial_currency = statement_currency,
    ui_locale = ui_locale,
    shared_bridge = function() .shared_bridge()
  )

  # ==========================================
  # 呼叫純 NAV 模組
  # ==========================================
  nav_results <- nav_module_server(
    id = "mod_nav",
    auto_calc_pulse = reactive(auto_calc_nav_pulse()),
    d_balance_sheet = d_balance_sheet,
    current_price = reactive({
      tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    }),
    market_cap = reactive({
      extract_quote_price_mcap(summary_data())$market_cap
    }),
    quote_price = reactive({
      extract_quote_price_mcap(summary_data())$price
    }),
    current_ticker = current_ticker,
    quote_currency = quote_currency,
    financial_currency = statement_currency,
    ui_locale = ui_locale
  )

  # ==========================================
  # v13：股數級距（DCF／RI／敏感度共用）
  # ==========================================
  .valuation_shares <- reactive({
    sh <- tryCatch(
      resolve_valuation_shares(
        d_balance_sheet(),
        summary_data(),
        ticker = current_ticker() %||% "",
        quote_currency = quote_currency(),
        financial_currency = statement_currency()
      ),
      error = function(e) list(shares = NA_real_, method = "none", note = NULL, shares_bs = NA_real_)
    )
    adopt_valuation_shares(sh, statement_currency(), quote_currency())
  })

  # One editor on Model Dashboard. Until the user edits a field, that field
  # follows the statement extract. Other pages only display the same numbers.
  bridge_touched <- reactiveValues(cash = FALSE, debt = FALSE, shares = FALSE, revenue = FALSE)
  bridge_prog_val <- reactiveValues(cash = NA_real_, debt = NA_real_, shares = NA_real_, revenue = NA_real_)

  .bridge_same <- function(a, b) {
    a <- suppressWarnings(as.numeric(a)[1])
    b <- suppressWarnings(as.numeric(b)[1])
    is.finite(a) && is.finite(b) && abs(a - b) <= max(1, abs(a)) * 1e-6
  }
  .bridge_write_canonical <- function(id, slot, value, digits) {
    value <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(value)) return(invisible(NULL))
    value <- round(value, digits)
    cur <- suppressWarnings(as.numeric(isolate(input[[id]]))[1])
    if (.bridge_same(cur, value)) return(invisible(NULL))
    bridge_prog_val[[slot]] <- value
    updateNumericInput(session, id, value = value)
    invisible(NULL)
  }
  .bridge_on_user <- function(id, slot) {
    observeEvent(input[[id]], {
      v <- suppressWarnings(as.numeric(input[[id]])[1])
      prog <- suppressWarnings(as.numeric(isolate(bridge_prog_val[[slot]]))[1])
      if (.bridge_same(v, prog)) {
        bridge_prog_val[[slot]] <- NA_real_
        return()
      }
      bridge_touched[[slot]] <- TRUE
    }, ignoreInit = TRUE)
  }
  .bridge_on_user("bridge_cash", "cash")
  .bridge_on_user("bridge_debt", "debt")
  .bridge_on_user("bridge_shares", "shares")
  .bridge_on_user("bridge_revenue", "revenue")

  .bridge_clear_touch <- function() {
    bridge_touched$cash <- FALSE
    bridge_touched$debt <- FALSE
    bridge_touched$shares <- FALSE
    bridge_touched$revenue <- FALSE
  }
  observeEvent(current_ticker(), { .bridge_clear_touch() }, ignoreInit = TRUE)
  observeEvent(input$bridge_reload, { .bridge_clear_touch() })

  .shared_bridge <- reactive({
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    is_df <- tryCatch(d_income_statement(), error = function(e) NULL)
    cd <- statement_cash_debt(bs)
    rev_stmt <- statement_total_revenue(is_df)
    sh_stmt <- tryCatch(.valuation_shares()$shares, error = function(e) NA_real_)
    cash <- if (isTRUE(bridge_touched$cash)) bridge_pick_number(input$bridge_cash, cd$cash) else cd$cash
    debt <- if (isTRUE(bridge_touched$debt)) bridge_pick_number(input$bridge_debt, cd$debt) else cd$debt
    shares <- if (isTRUE(bridge_touched$shares)) {
      u <- bridge_pick_number(input$bridge_shares)
      if (is.finite(u) && u > 0) u else sh_stmt
    } else {
      sh_stmt
    }
    revenue <- if (isTRUE(bridge_touched$revenue)) bridge_pick_number(input$bridge_revenue, rev_stmt) else rev_stmt
    if (!is.finite(cash) || cash < 0) cash <- 0
    if (!is.finite(debt) || debt < 0) debt <- 0
    list(cash = unname(cash), debt = unname(debt), shares = unname(shares), revenue = unname(revenue))
  })

  observe({
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    if (is.null(bs) || !is.data.frame(bs) || !nrow(bs)) return()
    sb <- .shared_bridge()
    if (!isTRUE(bridge_touched$cash)) .bridge_write_canonical("bridge_cash", "cash", sb$cash, 2)
    if (!isTRUE(bridge_touched$debt)) .bridge_write_canonical("bridge_debt", "debt", sb$debt, 2)
    if (!isTRUE(bridge_touched$shares) && is.finite(sb$shares) && sb$shares > 0) {
      .bridge_write_canonical("bridge_shares", "shares", sb$shares, 0)
    }
    if (!isTRUE(bridge_touched$revenue) && is.finite(sb$revenue)) {
      .bridge_write_canonical("bridge_revenue", "revenue", sb$revenue, 2)
    }
  })

  .push_shared_bridge_display <- function(sb) {
    if (!is.list(sb)) return(invisible(NULL))
    pairs <- list(
      list("mod_rel-cash", sb$cash, 2),
      list("mod_rel-debt", sb$debt, 2),
      list("mod_rel-shares", sb$shares, 0),
      list("mod_rel-revenue", sb$revenue, 2),
      list("mod_rel-ps_revenue", sb$revenue, 2),
      list("mod_sotp-cash", sb$cash, 2),
      list("mod_sotp-debt", sb$debt, 2),
      list("mod_sotp-shares", sb$shares, 0),
      list("mod_fcf-fcf_revenue", sb$revenue, 2)
    )
    for (p in pairs) {
      value <- suppressWarnings(as.numeric(p[[2]])[1])
      if (!is.finite(value)) next
      value <- round(value, p[[3]])
      cur <- suppressWarnings(as.numeric(isolate(input[[p[[1]]]]))[1])
      if (.bridge_same(cur, value)) next
      updateNumericInput(session, p[[1]], value = value)
    }
    invisible(NULL)
  }
  observe({
    .push_shared_bridge_display(.shared_bridge())
  })

  .dcf_per_share <- function(equity_value, sh = NULL) {
    if (is.null(sh)) sh <- tryCatch(.valuation_shares(), error = function(e) NULL)
    if (is.null(sh)) {
      out <- NA_real_
      attr(out, "alignment_failure") <- "REQUIRED_PER_SHARE_VALUE_NON_FINITE"
      return(out)
    }
    st_eff <- infer_statement_currency(
      statement_currency(), quote_currency(), sh$method
    )
    eq_ccy <- tryCatch(
      equity_money_ccy(d_balance_sheet(), session_currency(), st_eff),
      error = function(e) normalize_ccy(session_currency())
    )
    if (is.na(normalize_ccy(eq_ccy))) eq_ccy <- st_eff
    px <- per_share_in_quote(
      equity_value,
      sh$shares,
      equity_ccy = eq_ccy,
      to_ccy = quote_currency(),
      usd_twd = fx_usd_twd(),
      share_method = sh$method,
      statement_ccy = st_eff
    )
    code <- classify_per_share_alignment_failure(
      statement_ccy = statement_currency(),
      quote_ccy = quote_currency(),
      usd_twd = fx_usd_twd(),
      share_method = sh$method,
      equity_ccy = eq_ccy,
      shares = sh$shares,
      adr_ratio = sh$ratio,
      required_per_share = c(dcf = px),
      method = "dcf"
    )
    attr(px, "alignment_failure") <- code
    px
  }

  # ==========================================
  # v13：Bear / Base / Bull 情境 + 主／副點 + 可信度
  # ==========================================
  .dcf_valuation_bundle <- function(wacc_pp_delta = 0, g_pp_delta = 0,
                                    near_g_mult = 1, cash_mult = 1, debt_mult = 1,
                                    fcf_mult = 1,
                                    wacc1_pp_delta = 0, wacc2_pp_delta = 0,
                                    wacc_override_pct = NULL,
                                    yr_stage1_override = NULL,
                                    years_override = NULL) {
    empty <- list(
      ok = FALSE, price = NA_real_, shares = NA_real_,
      pv_fcf = NA_real_, pv_tv = NA_real_, cash = NA_real_, debt = NA_real_,
      equity = NA_real_, ev = NA_real_
    )
    df_fcf <- tryCatch(fcf_results$df_fcf(), error = function(e) NULL)
    n_base <- suppressWarnings(as.numeric(input$years)[1])
    if (is.null(df_fcf) || !is.data.frame(df_fcf) || !is.finite(n_base) || nrow(df_fcf) < 1) {
      return(empty)
    }
    n_years <- if (!is.null(years_override) && is.finite(years_override)) {
      as.integer(max(1, round(years_override)))
    } else {
      as.integer(n_base)
    }
    future_fcfs <- extract_fcff_series(df_fcf)
    # 年數衝擊：截斷或以前一期 FCFF 延展
    if (length(future_fcfs) >= 1L) {
      if (n_years <= length(future_fcfs)) {
        future_fcfs <- future_fcfs[seq_len(n_years)]
      } else {
        pad_n <- n_years - length(future_fcfs)
        last <- tail(future_fcfs, 1)
        future_fcfs <- c(future_fcfs, rep(last, pad_n))
      }
    }
    # Don't Gordon-grow a negative NOPAT-build FCFF (Lite and Full share this band).
    if (length(future_fcfs) >= 1L) {
      last_raw <- suppressWarnings(as.numeric(tail(future_fcfs, 1))[1])
      if (is.finite(last_raw) && last_raw < 0) {
        des_b <- tryCatch(lite_desired_des(), error = function(e) NULL)
        mode_b <- if (!is.null(des_b)) {
          if (isTRUE(des_b$two_stage)) "two_stage" else "gordon"
        } else {
          as.character(input$dcf_mode %||% "gordon")[1]
        }
        anchored <- .lite_fcff_from_trailing(length(future_fcfs), mode_b)
        if (!is.null(anchored)) future_fcfs <- anchored
      }
    }
    future_fcfs <- future_fcfs * fcf_mult
    if (isTRUE(near_g_mult != 1) && length(future_fcfs) >= 2) {
      base0 <- future_fcfs[1]
      if (is.finite(base0) && base0 != 0) {
        scaled <- future_fcfs
        for (i in seq_along(scaled)) {
          w <- i / length(scaled)
          scaled[i] <- base0 + (future_fcfs[i] - base0) * (1 + (near_g_mult - 1) * w)
        }
        future_fcfs <- scaled
      }
    }
    g_terminal <- if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) {
      as.numeric(input$sgr) / 100 + g_pp_delta / 100
    } else {
      APP_DEFAULTS$sgr / 100 + g_pp_delta / 100
    }
    claim_bundle <- as.character(input$dcf_claim %||% "fcff")[1]
    skip_wacc_gate <- identical(claim_bundle, "fcfe")
    if (identical(input$dcf_mode, "gordon")) {
      if (!is.null(wacc_override_pct) && is.finite(wacc_override_pct)) {
        r1 <- wacc_override_pct / 100
      } else {
        r1 <- as.numeric(input$wacc_gordon) / 100 + wacc_pp_delta / 100
      }
      r2 <- r1
      if (!isTRUE(skip_wacc_gate) && (!is.finite(r2) || !is.finite(g_terminal) || g_terminal >= r2)) return(empty)
      if (!is.finite(r1)) r1 <- 0.1
      discount_factors <- cumprod(1 + rep(r1, n_years))
    } else {
      if (!is.null(wacc_override_pct) && is.finite(wacc_override_pct)) {
        r1 <- wacc_override_pct / 100 + wacc1_pp_delta / 100
        r2 <- wacc_override_pct / 100 + wacc2_pp_delta / 100
      } else {
        r1 <- as.numeric(input$wacc_stage1) / 100 + wacc_pp_delta / 100 + wacc1_pp_delta / 100
        r2 <- as.numeric(input$wacc_stage2) / 100 + wacc_pp_delta / 100 + wacc2_pp_delta / 100
      }
      if (!isTRUE(skip_wacc_gate) && (!is.finite(r2) || !is.finite(g_terminal) || g_terminal >= r2)) return(empty)
      if (!is.finite(r1)) r1 <- 0.1
      if (!is.finite(r2)) r2 <- r1
      yr1_src <- if (!is.null(yr_stage1_override) && is.finite(yr_stage1_override)) {
        yr_stage1_override
      } else {
        input$yr_stage1
      }
      yr1 <- clamp_yr_stage1(n_years, yr1_src, APP_DEFAULTS$yr_stage1)
      wacc_sequence <- c(rep(r1, min(yr1, n_years)), rep(r2, max(0, n_years - yr1)))
      discount_factors <- cumprod(1 + wacc_sequence)
    }
    if (!isTRUE(skip_wacc_gate)) {
      pv_forecast <- sum(future_fcfs / discount_factors)
      last_fcf <- future_fcfs[n_years]
      tv <- (last_fcf * (1 + g_terminal)) / (r2 - g_terminal)
      pv_tv <- tv / discount_factors[n_years]
      dcf_value <- pv_forecast + pv_tv
    } else {
      pv_forecast <- NA_real_
      pv_tv <- NA_real_
      dcf_value <- NA_real_
    }
    sb_bridge <- .shared_bridge()
    latest_cash <- sb_bridge$cash * cash_mult
    latest_debt <- sb_bridge$debt * debt_mult
    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    if (identical(claim, "fcfe")) {
      ke <- if (isTRUE(input$use_estimated_re) && !is.null(estimated_re())) {
        as.numeric(estimated_re())[1]
      } else {
        suppressWarnings(as.numeric(input$wacc_re)[1]) / 100
      }
      if (!is.finite(ke) || ke <= 0) ke <- r2
      if (!is.finite(ke) || !is.finite(g_terminal) || g_terminal >= ke) return(empty)
      rd <- suppressWarnings(as.numeric(input$wacc_rd)[1]) / 100
      if (!is.finite(rd) || rd < 0) rd <- 0
      tax <- suppressWarnings(as.numeric(input$wacc_tax)[1]) / 100
      if (!is.finite(tax)) tax <- APP_DEFAULTS$wacc_tax / 100
      iat <- after_tax_interest(latest_debt, rd, tax)
      fcfe <- fcff_to_fcfe(future_fcfs, interest_after_tax = iat, debt0 = latest_debt, g_path = g_terminal)
      ke_dfs <- cumprod(rep(1 + ke, n_years))
      if (length(ke_dfs) != length(fcfe)) return(empty)
      pv_forecast <- sum(fcfe / ke_dfs)
      last_fcf <- fcfe[n_years]
      tv <- (last_fcf * (1 + g_terminal)) / (ke - g_terminal)
      pv_tv <- tv / ke_dfs[n_years]
      dcf_value <- pv_forecast + pv_tv
      equity_value <- as.numeric(dcf_value)[1]
    } else {
      equity_value <- as.numeric(dcf_value)[1] + latest_cash - latest_debt
    }
    shares_info <- tryCatch(.valuation_shares(), error = function(e) NULL)
    shares <- if (!is.null(shares_info)) suppressWarnings(as.numeric(shares_info$shares)[1]) else NA_real_
    if (!is.finite(equity_value) || !is.finite(shares) || shares <= 0) return(empty)
    px <- .dcf_per_share(equity_value, shares_info)
    if (!is.finite(px)) return(empty)
    list(
      ok = TRUE,
      price = px,
      shares = shares,
      pv_fcf = pv_forecast,
      pv_tv = pv_tv,
      cash = latest_cash,
      debt = latest_debt,
      equity = equity_value,
      ev = if (identical(claim, "fcfe")) NA_real_ else as.numeric(dcf_value)[1]
    )
  }

  .dcf_price_at <- function(wacc_pp_delta = 0, g_pp_delta = 0, near_g_mult = 1) {
    b <- .dcf_valuation_bundle(
      wacc_pp_delta = wacc_pp_delta,
      g_pp_delta = g_pp_delta,
      near_g_mult = near_g_mult
    )
    if (!isTRUE(b$ok)) return(NA_real_)
    b$price
  }

  .ddm_price_at <- function(ke_pp_delta = 0, g_pp_delta = 0) {
    d0 <- suppressWarnings(as.numeric(input[["mod_ddm-d0"]])[1])
    g0 <- suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1])
    ke0 <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
    if (!is.finite(d0) || d0 < 0) return(NA_real_)
    if (!is.finite(g0)) g0 <- if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) as.numeric(input$sgr) else APP_DEFAULTS$sgr
    if (!is.finite(ke0)) ke0 <- central_ke() * 100
    g <- (g0 + g_pp_delta) / 100
    ke <- (ke0 + ke_pp_delta) / 100
    mode <- as.character(input[["mod_ddm-ddm_mode"]] %||% "gordon")[1]
    if (identical(mode, "spm")) {
      eps <- suppressWarnings(as.numeric(input[["mod_ddm-est_eps"]])[1])
      if (!is.finite(eps) || !is.finite(ke) || !is.finite(g) || ke <= 0) return(NA_real_)
      return(.ddm_formula_spm(eps = eps, d = d0, g = g, ke = ke))
    }
    if (!is.finite(d0) || d0 <= 0) return(NA_real_)
    if (!is.finite(ke) || !is.finite(g) || ke <= g) return(NA_real_)
    if (identical(mode, "two_stage")) {
      g1 <- suppressWarnings(as.numeric(input[["mod_ddm-g_stage1"]])[1]) / 100
      n1 <- suppressWarnings(as.integer(input[["mod_ddm-yr_stage1"]])[1])
      if (!is.finite(g1)) g1 <- g
      if (!is.finite(n1) || n1 < 1L) n1 <- 5L
      return(.ddm_formula_two_stage(d0 = d0, g1 = g1, n = n1, g2 = g, ke = ke))
    }
    d0 * (1 + g) / (ke - g)
  }

  .ri_price_at <- function(ke_pp_delta = 0, g_pp_delta = 0, roe_pp_delta = 0) {
    if (!exists("compute_ri_valuation", mode = "function")) return(NA_real_)
    b0 <- suppressWarnings(as.numeric(input[["mod_ri-b0"]])[1])
    ke0 <- suppressWarnings(as.numeric(input[["mod_ri-ri_ke"]])[1])
    g0 <- suppressWarnings(as.numeric(input[["mod_ri-ri_g"]])[1])
    n <- suppressWarnings(as.integer(input[["mod_ri-ri_years"]])[1])
    payout <- suppressWarnings(as.numeric(input[["mod_ri-ri_payout"]])[1])
    roe0 <- suppressWarnings(as.numeric(input[["mod_ri-ri_roe"]])[1])
    if (!is.finite(b0) || !is.finite(ke0) || !is.finite(g0) || !is.finite(n)) return(NA_real_)
    if (!is.finite(payout)) payout <- 0
    if (!is.finite(roe0)) roe0 <- 12
    ke <- (ke0 + ke_pp_delta) / 100
    g <- (g0 + g_pp_delta) / 100
    roe <- (roe0 + roe_pp_delta) / 100
    if (!is.finite(ke) || !is.finite(g) || ke <= g) return(NA_real_)
    res <- tryCatch(
      compute_ri_valuation(
        b0 = b0, ke = ke, g = g, n = max(1L, n),
        payout = payout / 100,
        roe_path = rep(roe, max(1L, n)),
        validate = TRUE
      ),
      error = function(e) NULL
    )
    if (is.null(res) || !identical(res$status, "success")) return(NA_real_)
    suppressWarnings(as.numeric(res$intrinsic)[1])
  }

  # Session run-state: true only after 試算 / Run (or silent primary auto-calc).
  # Live formula helpers (.dcf_price_at / .ddm_price_at / .ri_price_at) must NOT
  # feed Composite overlays or primary/secondary bands before the user has run.
  .model_has_run <- function(key) {
    key <- as.character(key %||% "")[1]
    v <- switch(
      key,
      "dcf" = suppressWarnings(as.numeric(stock_price_estimate_val())[1]),
      "ddm" = tryCatch({
        v <- if (!is.null(ddm_results$ddm_price)) ddm_results$ddm_price() else NA_real_
        v <- suppressWarnings(as.numeric(v)[1])
        # D0=0 Gordon used to return finite 0 — treat as not run / not usable
        if (!is.finite(v) || v <= 0) NA_real_ else v
      }, error = function(e) NA_real_),
      "ri" = tryCatch({
        if (!is.null(ri_results$ri_price)) ri_results$ri_price() else NA_real_
      }, error = function(e) NA_real_),
      "pb" = tryCatch({
        if (!is.null(pb_results$pb_price)) pb_results$pb_price() else NA_real_
      }, error = function(e) NA_real_),
      "nav" = tryCatch({
        if (!is.null(nav_results$nav_price)) nav_results$nav_price() else NA_real_
      }, error = function(e) NA_real_),
      "multiples" = tryCatch({
        if (!is.null(rel_results$any_implied_price)) {
          rel_results$any_implied_price()
        } else if (!is.null(rel_results$pe_price)) {
          rel_results$pe_price()
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_),
      "sotp" = tryCatch({
        if (!is.null(sotp_results$sotp_price)) sotp_results$sotp_price() else NA_real_
      }, error = function(e) NA_real_),
      NA_real_
    )
    is.finite(suppressWarnings(as.numeric(v)[1]))
  }

  .model_empty_band <- function(label) {
    list(bear = NA_real_, base = NA_real_, bull = NA_real_, label = label)
  }

  dcf_scenario_band <- reactive({
    if (!isTRUE(.model_has_run("dcf"))) return(.model_empty_band("DCF"))
    sf <- scenario_stress_factors("dcf")
    base <- .dcf_price_at(0, 0, 1)
    if (!is.finite(base)) {
      base <- suppressWarnings(as.numeric(stock_price_estimate_val())[1])
    }
    bear <- .dcf_price_at(sf$bear$wacc_pp, sf$bear$g_pp, sf$bear$near_g_mult)
    bull <- .dcf_price_at(sf$bull$wacc_pp, sf$bull$g_pp, sf$bull$near_g_mult)
    list(bear = bear, base = base, bull = bull, label = "DCF")
  })

  ddm_scenario_band <- reactive({
    if (!isTRUE(.model_has_run("ddm"))) return(.model_empty_band("DDM"))
    sf <- scenario_stress_factors("ddm")
    base <- .ddm_price_at(0, 0)
    if (!is.finite(base)) {
      base <- tryCatch(ddm_results$ddm_price(), error = function(e) NA_real_)
    }
    list(
      bear = .ddm_price_at(sf$bear$ke_pp, sf$bear$g_pp),
      base = base,
      bull = .ddm_price_at(sf$bull$ke_pp, sf$bull$g_pp),
      label = "DDM"
    )
  })

  ri_scenario_band <- reactive({
    if (!isTRUE(.model_has_run("ri"))) return(.model_empty_band("RI"))
    sf <- scenario_stress_factors("ri")
    base <- .ri_price_at(0, 0, 0)
    if (!is.finite(base)) {
      base <- tryCatch(ri_results$ri_price(), error = function(e) NA_real_)
    }
    list(
      bear = .ri_price_at(sf$bear$ke_pp, sf$bear$g_pp, sf$bear$roe_pp),
      base = base,
      bull = .ri_price_at(sf$bull$ke_pp, sf$bull$g_pp, sf$bull$roe_pp),
      label = "RI"
    )
  })

  pb_scenario_band <- reactive({
    if (!isTRUE(.model_has_run("pb"))) return(.model_empty_band("P/B"))
    band <- tryCatch(pb_results$pb_band(), error = function(e) NULL)
    if (!is.null(band) && is.finite(band$mid)) {
      return(list(bear = band$low, base = band$mid, bull = band$high, label = "P/B"))
    }
    mid <- tryCatch(pb_results$pb_price(), error = function(e) NA_real_)
    list(bear = NA_real_, base = mid, bull = NA_real_, label = "P/B")
  })

  nav_scenario_band <- reactive({
    if (!isTRUE(.model_has_run("nav"))) return(.model_empty_band("NAV"))
    band <- tryCatch(nav_results$nav_band(), error = function(e) NULL)
    if (!is.null(band) && is.finite(band$mid)) {
      return(list(bear = band$low, base = band$mid, bull = band$high, label = "NAV"))
    }
    mid <- tryCatch(nav_results$nav_price(), error = function(e) NA_real_)
    list(bear = NA_real_, base = mid, bull = NA_real_, label = "NAV")
  })

  .model_point <- function(key) {
    key <- as.character(key %||% "")[1]
    if (!nzchar(key) || !isTRUE(.model_has_run(key))) return(NA_real_)
    switch(
      key,
      "dcf" = {
        b <- tryCatch(dcf_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else suppressWarnings(as.numeric(stock_price_estimate_val())[1])
      },
      "ddm" = {
        b <- tryCatch(ddm_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else tryCatch(ddm_results$ddm_price(), error = function(e) NA_real_)
      },
      "pb" = {
        b <- tryCatch(pb_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else tryCatch(pb_results$pb_price(), error = function(e) NA_real_)
      },
      "nav" = {
        b <- tryCatch(nav_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else tryCatch(nav_results$nav_price(), error = function(e) NA_real_)
      },
      "ri" = {
        b <- tryCatch(ri_scenario_band(), error = function(e) NULL)
        if (!is.null(b) && is.finite(b$base)) b$base else tryCatch(ri_results$ri_price(), error = function(e) NA_real_)
      },
      "multiples" = tryCatch({
        if (!is.null(rel_results$any_implied_price)) {
          rel_results$any_implied_price()
        } else if (!is.null(rel_results$pe_price)) {
          rel_results$pe_price()
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_),
      "sotp" = tryCatch({
        if (!is.null(sotp_results$sotp_price)) sotp_results$sotp_price() else NA_real_
      }, error = function(e) NA_real_),
      NA_real_
    )
  }

  primary_valuation_band <- reactive({
    rec <- model_sidebar_rec()
    prim <- as.character(rec$primary %||% "")[1]
    # Fair Value engines only. Empty / insufficient primary (e.g. TW 3363-like thin ROE +
    # neg FCFF) must not fall through to the DCF band — that made Composite status look
    # DCF-driven while Model Selector correctly said no usable Fair Value primary.
    # Multiples / SOTP are Implied Price / Meta-Framework — never drive Bear–Base–Bull.
    fv_ok <- prim %in% c("dcf", "ddm", "pb", "nav", "ri")
    if (!isTRUE(fv_ok)) {
      return(.model_empty_band(""))
    }
    band <- switch(
      prim,
      "dcf" = dcf_scenario_band(),
      "ddm" = ddm_scenario_band(),
      "pb" = pb_scenario_band(),
      "nav" = nav_scenario_band(),
      "ri" = ri_scenario_band(),
      .model_empty_band(.model_label(prim))
    )
    if (is.null(band)) band <- .model_empty_band(.model_label(prim))
    band$label <- .model_label(prim)
    # ensure ordered when all present
    if (is.finite(band$bear) && is.finite(band$base) && is.finite(band$bull)) {
      xs <- sort(c(band$bear, band$base, band$bull))
      band$bear <- xs[1]; band$base <- xs[2]; band$bull <- xs[3]
    }
    band
  })

  secondary_valuation_point <- reactive({
    rec <- model_sidebar_rec()
    sec <- as.character(rec$secondary %||% "")
    if (!nzchar(sec)) return(NA_real_)
    .model_point(sec)
  })

  # All model Base/FV points for composite Current-price axis overlays
  # (only models with a successful 試算 / Run in this session)
  all_model_valuation_points <- reactive({
    keys <- c("dcf", "ddm", "ri", "pb", "nav", "multiples", "sotp")
    out <- lapply(keys, function(k) {
      if (!isTRUE(.model_has_run(k))) return(NA_real_)
      v <- suppressWarnings(as.numeric(tryCatch(.model_point(k), error = function(e) NA_real_))[1])
      if (length(v) != 1L || is.null(v) || is.na(v) || !is.finite(v) || v == 0) NA_real_ else v
    })
    names(out) <- keys
    out
  })

  active_valuation_model_key <- reactive({
    tab <- as.character(input$sidebar_tabs %||% "")[1]
    # Smart Analysis: highlight the recommended primary on the shared composite strip
    if (identical(tab, "smart_analysis")) {
      rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
      prim <- as.character(rec$primary %||% "")[1]
      if (nzchar(prim) && prim %in% c("dcf", "ddm", "ri", "pb", "nav", "multiples", "sotp")) {
        return(prim)
      }
      return(NA_character_)
    }
    switch(
      tab,
      "dcf_calculator" = "dcf",
      "ddm_calculator" = "ddm",
      "ri_calculator" = "ri",
      "pb_calculator" = "pb",
      "nav_calculator" = "nav",
      "rel_mult_ev" = "multiples",
      "rel_multiples_calculator" = "multiples",
      "rel_mult_rev" = "multiples",
      "sotp_calculator" = "sotp",
      NA_character_
    )
  })

  valuation_confidence <- reactive({
    rec <- model_sidebar_rec()
    band <- tryCatch(primary_valuation_band(), error = function(e) NULL)
    sec_pt <- tryCatch(secondary_valuation_point(), error = function(e) NA_real_)
    # F-Score light proxy: OCF vs 營運利潤（扣除投資證券未實現損益）
    f_score <- tryCatch({
      ni <- select_current_metric_any(d_income_statement(), NET_INCOME_PATTERNS, "flow")
      unreal <- tryCatch(
        select_current_metric_any(d_income_statement(), UNREALIZED_INVESTMENT_GL_PATTERNS, "flow"),
        error = function(e) NA_real_
      )
      if (!is.finite(unreal)) unreal <- 0
      op_earn <- if (is.finite(ni)) ni - unreal else NA_real_
      ocf <- select_current_metric_any(d_cash_flow(), CASHFLOW_OCF_PATTERNS, "flow")
      assets <- select_current_metric(d_balance_sheet(), "Total Assets", "stock")
      s <- 0
      if (is.finite(ni) && is.finite(assets) && assets > 0 && ni / assets > 0) s <- s + 1
      if (is.finite(ocf) && ocf > 0) s <- s + 1
      if (is.finite(ocf) && is.finite(op_earn) && ocf > op_earn) s <- s + 1
      # scale 0–3 → approximate 0–9 for scorer thresholds
      s * 3
    }, error = function(e) NA_real_)
    tv_weight <- tryCatch({
      df_fcf <- fcf_results$df_fcf()
      n_years <- as.numeric(input$years)
      if (is.null(df_fcf) || nrow(df_fcf) != n_years) return(NA_real_)
      future_fcfs <- extract_fcff_series(df_fcf)
      r2 <- if (identical(input$dcf_mode, "gordon")) {
        as.numeric(input$wacc_gordon) / 100
      } else {
        as.numeric(input$wacc_stage2) / 100
      }
      g <- as.numeric(input$sgr) / 100
      if (!is.finite(r2) || !is.finite(g) || r2 <= g) return(NA_real_)
      dfs <- cumprod(1 + rep(r2, n_years))
      pv_fcf <- sum(future_fcfs / dfs)
      tv <- (tail(future_fcfs, 1) * (1 + g)) / (r2 - g) / dfs[n_years]
      tv / (pv_fcf + tv)
    }, error = function(e) NA_real_)
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    score_valuation_confidence(
      confidence_inputs = rec$confidence_inputs %||% list(),
      f_score = f_score,
      primary_base = if (!is.null(band)) band$base else NA_real_,
      secondary_point = sec_pt,
      tv_weight = tv_weight,
      locale = loc
    )
  })
  
  # 財報警訊已併入 YNOW 分頁 Schilit 自動判讀（decision_server$shenanigans_panel）

  # Annotation 產業快覽已併入 dashboard_selected_industry；此處保留空輸出以免舊引用
  output$annotation_industry_bands <- renderUI({ NULL })

  output$annotation_kpi_guide <- DT::renderDataTable({
    key <- as.character(input$industry_choice %||% "")[1]
    fp <- tryCatch(fundamental_profile_rec(), error = function(e) NULL)
    pid <- as.character(fp$profile %||% "")[1]
    df <- annotation_kpi_guide_df(key, profile_id = pid)
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        dom = "t",
        scrollX = TRUE,
        ordering = FALSE
      )
    )
  })

  output$annotation_stability_table <- renderTable({
    annotation_stability_df()
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "m", width = "100%")

  # ==========================================
  # 🧮 7. CAPM, WACC 與 DCF 估值計算
  # ==========================================
  # --- 優化後的債務抓取：處理 Total Debt 不存在的情況 ---
  scraped_debt <- reactive({
    req(d_balance_sheet())
    df_bs <- d_balance_sheet()
    
    # 優先抓取 Total Debt，若無則嘗試「短期+長期」加總
    val <- select_clean_metric_row(df_bs, "^Total Debt$", include_ttm = FALSE)
    if (length(val) == 0 || all(is.na(val))) {
      st_debt <- select_clean_metric_row(df_bs, "Current Debt|Short Term Debt", include_ttm = FALSE)
      lt_debt <- select_clean_metric_row(df_bs, "Long Term Debt", include_ttm = FALSE)
      val <- sum(c(st_debt[1], lt_debt[1]), na.rm = TRUE)
    } else {
      val <- val[1]
    }
    
    return(ifelse(is.na(val), 0, val))
  })

  # --- 負債成本 rᵈ (%)：利息費用／有息負債（稅前；無全域預設）；上下限由 UI 參數決定 ---
  .rd_clamp_bounds <- function() {
    lo <- suppressWarnings(as.numeric(input$wacc_rd_min)[1])
    hi <- suppressWarnings(as.numeric(input$wacc_rd_max)[1])
    if (!is.finite(lo)) lo <- APP_DEFAULTS$wacc_rd_min %||% 0
    if (!is.finite(hi)) hi <- APP_DEFAULTS$wacc_rd_max %||% 40
    if (hi < lo) {
      tmp <- lo; lo <- hi; hi <- tmp
    }
    c(lo = lo, hi = hi)
  }

  .scraped_interest_expense <- function() {
    interest <- tryCatch(
      abs(as.numeric(select_current_metric_any(
        d_income_statement(), INTEREST_EXPENSE_PATTERNS, "flow"
      ))[1]),
      error = function(e) NA_real_
    )
    if (!is.finite(interest) || interest <= 0) {
      cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
      if (!is.null(cf) && is.data.frame(cf) && nrow(cf) > 0) {
        interest <- tryCatch(
          abs(as.numeric(select_current_metric_any(
            cf, INTEREST_PAID_PATTERNS, "flow"
          ))[1]),
          error = function(e) NA_real_
        )
        if (is.finite(interest) && interest > 0) {
          attr(interest, "source") <- "cash_flow_interest_paid"
          return(interest)
        }
      }
      return(NA_real_)
    }
    attr(interest, "source") <- "income_interest_expense"
    interest
  }

  scraped_rd_pct <- reactive({
    interest <- suppressWarnings(as.numeric(input$rd_interest_expense)[1])
    debt <- suppressWarnings(as.numeric(input$rd_interest_bearing_debt)[1])
    if (!is.finite(interest) || interest <= 0 || !is.finite(debt) || debt <= 0) {
      return(NA_real_)
    }
    rd <- 100 * interest / debt
    b <- .rd_clamp_bounds()
    max(b[["lo"]], min(rd, b[["hi"]]))
  })

  .apply_estimated_rd <- function(notify = FALSE) {
    rd <- tryCatch(scraped_rd_pct(), error = function(e) NA_real_)
    if (!is.finite(rd)) return(invisible(NA_real_))
    updateNumericInput(session, "wacc_rd", value = round(rd, 2))
    if (isTRUE(notify)) {
      showNotification(
        .ui_msg("notif_rd_estimated", rd = round(rd, 2)),
        type = "message",
        duration = 5
      )
    }
    invisible(rd)
  }

  # 財報就緒：帶入利息費用與有息負債（Total Debt 或 ST+LT）
  observeEvent(
    list(d_income_statement(), d_balance_sheet(), d_cash_flow(), scraped_debt()),
    {
      interest <- tryCatch(.scraped_interest_expense(), error = function(e) NA_real_)
      debt <- tryCatch(as.numeric(scraped_debt())[1], error = function(e) NA_real_)
      if (is.finite(interest) && interest > 0) {
        updateNumericInput(session, "rd_interest_expense", value = round(interest, 2))
      }
      if (is.finite(debt) && debt > 0) {
        updateNumericInput(session, "rd_interest_bearing_debt", value = round(debt, 2))
      }
    },
    ignoreInit = FALSE
  )

  # 估算參數變更：勾選「採用估算 rᵈ」時覆寫 wacc_rd
  observeEvent(
    list(
      input$rd_interest_expense, input$rd_interest_bearing_debt,
      input$wacc_rd_min, input$wacc_rd_max, input$use_estimated_rd
    ),
    {
      if (!isTRUE(input$use_estimated_rd)) return()
      .apply_estimated_rd(notify = FALSE)
    },
    ignoreInit = FALSE
  )

  observeEvent(input$calc_rd, {
    .apply_estimated_rd(notify = TRUE)
  })

  output$rd_interest_source_note <- renderUI({
    interest <- tryCatch(.scraped_interest_expense(), error = function(e) NA_real_)
    src <- attr(interest, "source")
    lab <- if (identical(src, "cash_flow_interest_paid")) {
      "財報來源：現金流量表 Interest Paid"
    } else if (identical(src, "income_interest_expense")) {
      "財報來源：損益表 Interest Expense"
    } else {
      "財報來源：尚無利息費用；可手動輸入"
    }
    tags$p(style = "margin:-6px 0 8px 0;color:#666;font-size:11px;", lab)
  })

  output$rd_debt_source_note <- renderUI({
    debt <- tryCatch(as.numeric(scraped_debt())[1], error = function(e) NA_real_)
    lab <- if (is.finite(debt) && debt > 0) {
      "財報來源：有息負債（優先 Total Debt；否則 Short-term + Long-term Debt）"
    } else {
      "財報來源：尚無有息負債；可手動輸入"
    }
    tags$p(style = "margin:-6px 0 8px 0;color:#666;font-size:11px;", lab)
  })

  output$rd_result <- renderUI({
    interest <- suppressWarnings(as.numeric(input$rd_interest_expense)[1])
    debt <- suppressWarnings(as.numeric(input$rd_interest_bearing_debt)[1])
    rd <- tryCatch(scraped_rd_pct(), error = function(e) NA_real_)
    if (!is.finite(interest) || interest <= 0 || !is.finite(debt) || debt <= 0) {
      return(tags$p(style = "color:#888;font-size:13px;", .ui_msg("rd_need_inputs")))
    }
    raw <- 100 * interest / debt
    shown <- if (is.finite(rd)) rd else raw
    HTML(glue::glue(
      "<div style='padding:8px;border-left:4px solid #222222;background:#f5f5f5;font-size:13px;'>
         rᵈ = 利息費用 ÷ 有息負債 = <b>{sprintf('%.2f%%', shown)}</b>
       </div>"
    ))
  })
 
  # --- 股數與市值（報價 → session；ADR 自動約當股數 = 市值÷股價）---
  scraped_market_cap <- reactive({
    req(d_balance_sheet(), summary_data())
    session_currency(); fx_usd_twd(); quote_currency()

    df_sum <- summary_data()
    sh <- resolve_valuation_shares(
      d_balance_sheet(), df_sum,
      ticker = current_ticker() %||% "",
      quote_currency = quote_currency(),
      financial_currency = statement_currency()
    )
    shares <- suppressWarnings(as.numeric(sh$shares)[1])
    if (!is.finite(shares) || shares <= 0) shares <- NA_real_

    price_native <- suppressWarnings(as.numeric(sh$price)[1])
    if (!is.finite(price_native)) {
      price_row <- df_sum[grep("Previous Close|Market Price", df_sum$Item), ]
      price_native <- if (nrow(price_row) > 0) parse_financial_number(price_row$Value[1]) else NA
    }
    if (is.na(price_native) || !is.finite(price_native)) {
      return(list(e_val = NA, shares = shares, price = NA, share_method = sh$method, share_note = sh$note))
    }

    price_val <- money_to_session(
      price_native, quote_currency(), session_currency(), fx_usd_twd()
    )

    # 優先用 Summary 市值（已是報價股數），再換算 session；否則約當股數×價
    e_native <- suppressWarnings(as.numeric(sh$market_cap)[1])
    if (!is.finite(e_native) || e_native <= 0) {
      e_native <- shares * price_native
    }
    e_val <- money_to_session(
      e_native, quote_currency(), session_currency(), fx_usd_twd()
    )

    list(
      e_val = e_val,
      shares = shares,
      price = price_val,
      share_method = sh$method,
      share_note = sh$note
    )
  })
  
  # --- 優化後的稅率計算 ---
  scraped_tax_rate <- reactive({
    req(d_income_statement())
    df_is <- d_income_statement()
    
    tax_exp <- select_current_metric(df_is, "Tax Provision", "flow")
    pre_tax_inc <- select_current_metric(df_is, "Pretax Income", "flow")
    
    # 邏輯優化：處理負稅率或極端值
    if (is.na(tax_exp) || is.na(pre_tax_inc) || pre_tax_inc <= 0) {
      return(NA_real_)
    }
    (tax_exp / pre_tax_inc) * 100
  })
  
  # --- 1. 渲染股權市值 (E) ---
  output$vbx_equity_val <- renderValueBox({
    mkt_data <- scraped_market_cap()
    valueBox(
      value = format_dollar_abbr(mkt_data$e_val),
      subtitle = "股權市值 (Market Equity - E)",
      icon = icon("coins"),
      color = "aqua"
    )
  })
  
  # --- 2. 渲染總負債 (D) ---
  output$vbx_debt_val <- renderValueBox({
    d_val <- scraped_debt()
    valueBox(
      value = format_dollar_abbr(d_val),
      subtitle = "總負債 (Total Debt - D)",
      icon = icon("file-invoice-dollar"),
      color = "red"
    )
  })
  
  # --- 3. 渲染有效稅率 (T) ---
  output$vbx_tax_rate <- renderValueBox({
    t_rate <- scraped_tax_rate()
    valueBox(
      value = if (is.finite(t_rate)) paste0(round(t_rate, 2), "%") else "N/A",
      subtitle = "有效稅率 (Effective Tax Rate - T；WACC 用使用者／enacted T)",
      icon = icon("percent"),
      color = "purple"
    )
  })
  
  # 🎯 智慧標籤：市場報酬率 Rm (當數值等於預設時顯示藍色標籤)
  observeEvent(c(input$capm_rm, input$industry_choice), {
    req(input$industry_choice)
    default_rm <- if (!is.null(industry_standards[[input$industry_choice]]$rm_avg)) 
      industry_standards[[input$industry_choice]]$rm_avg else 8.0
    
    if (!is.null(input$capm_rm) && abs(as.numeric(input$capm_rm) - default_rm) < 1e-4) {
      updateNumericInput(session, "capm_rm", 
                         label = HTML("Rm <span style='color: #1a1a1a; font-size: 12px;'>[套用產業平均值]</span>"))
    } else {
      updateNumericInput(session, "capm_rm", 
                         label = HTML("Rm <span style='color: #e67e22; font-size: 12px;'>[自訂數值]</span>"))
    }
  }, ignoreInit = FALSE)
  
  # ---------- CAPM Beta：與基礎設定 BETA 雙向連動 ----------
  .summary_beta_value <- function() {
    df <- tryCatch(summary_data(), error = function(e) NULL)
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
    idx <- grep("^Beta", df$Item, ignore.case = TRUE)
    if (length(idx) == 0) return(NA_real_)
    parse_financial_number(df$Value[idx[1]])[1]
  }

  .industry_beta_value <- function() {
    ind <- input$industry_choice
    if (is.null(ind) || !nzchar(ind)) return(NA_real_)
    inds <- industry_standards[[ind]]
    if (is.null(inds) || is.null(inds$beta_avg)) return(1.0)
    suppressWarnings(as.numeric(inds$beta_avg))
  }

  .set_capm_beta <- function(val) {
    val <- suppressWarnings(as.numeric(val))
    if (!is.finite(val)) return(invisible(FALSE))
    val <- round(val, 2)
    cur <- suppressWarnings(as.numeric(input$capm_beta))
    if (is.finite(cur) && abs(cur - val) < 1e-4) return(invisible(FALSE))
    capm_beta_updating(TRUE)
    updateNumericInput(session, "capm_beta", value = val)
    invisible(TRUE)
  }

  # 產業來源就緒且「與基礎設定同步」時，把產業 β 寫入 CAPM
  .sync_capm_beta_industry <- function() {
    if (!isTRUE(input$sync_gs_beta)) return(invisible(NULL))
    src <- as.character(input$beta_u_apply_source %||% "")[1]
    if (!identical(src, "industry")) return(invisible(NULL))
    b <- .industry_beta_value()
    if (is.finite(b)) {
      beta_capm_driver("gs")
      capm_beta_dirty(FALSE)
      .set_capm_beta(b)
    }
    invisible(NULL)
  }

  .set_sync_gs_beta <- function(val) {
    val <- isTRUE(val)
    if (identical(isTRUE(input$sync_gs_beta), val)) return(invisible(FALSE))
    sync_gs_beta_updating(TRUE)
    updateCheckboxInput(session, "sync_gs_beta", value = val)
    invisible(TRUE)
  }

  # CAPM 手動改 β → 取消「與基礎設定同步」，WACC 獨立；不改寫基礎設定來源
  observeEvent(input$capm_beta, {
    if (isTRUE(capm_beta_updating())) {
      capm_beta_updating(FALSE)
      return()
    }

    beta_val <- suppressWarnings(as.numeric(input$capm_beta)[1])
    if (!is.finite(beta_val)) return()

    if (isTRUE(input$sync_gs_beta)) {
      .set_sync_gs_beta(FALSE)
    }
    beta_capm_driver("manual")
    capm_beta_dirty(TRUE)
  }, ignoreInit = TRUE)

  # 換產業：僅在同步開啟且基礎設定來源為產業預設時更新 CAPM β
  observeEvent(input$industry_choice, {
    if (!isTRUE(input$sync_gs_beta)) return()
    src <- as.character(input$beta_u_apply_source %||% "")[1]
    if (identical(src, "industry")) {
      .sync_capm_beta_industry()
    }
  }, ignoreInit = TRUE)

  # 智慧標籤：與基礎設定來源鎖步（或 WACC 獨立）
  .capm_beta_label_html <- function(beta) {
    loc <- tryCatch(isolate(ui_locale()), error = function(e) "en")
    src <- as.character(input$beta_u_apply_source %||% "summary")[1]
    gs_tag <- switch(
      src,
      "summary" = ui_str("capm_beta_src_summary", loc),
      "industry" = ui_str("capm_beta_src_industry", loc),
      "bottomup" = ui_str("capm_beta_src_bottomup", loc),
      "unlever_firm" = ui_str("capm_beta_src_unlever", loc),
      "manual" = ui_str("capm_beta_src_manual", loc),
      ui_str("capm_beta_src_generic", loc)
    )
    base <- ui_str("capm_beta_label_base", loc)
    if (identical(src, "rolling")) {
      HTML(paste0(
        base, " <span style='color: #c0392b; font-size: 12px;'>",
        htmltools::htmlEscape(ui_str("capm_beta_tag_rolling_excluded", loc)),
        "</span>"
      ))
    } else if (isTRUE(input$sync_gs_beta)) {
      tag <- gsub("{src}", gs_tag, ui_str("capm_beta_tag_synced", loc), fixed = TRUE)
      HTML(sprintf(
        "%s <span style='color: #27ae60; font-size: 12px;'>%s</span>",
        base, htmltools::htmlEscape(tag)
      ))
    } else {
      HTML(paste0(
        base, " <span style='color: #e67e22; font-size: 12px;'>",
        htmltools::htmlEscape(ui_str("capm_beta_tag_wacc_indep", loc)),
        "</span>"
      ))
    }
  }

  observeEvent(list(
    input$capm_beta, input$industry_choice, input$sync_gs_beta,
    input$beta_u_apply_source, summary_data(), beta_capm_driver(),
    ui_locale()
  ), {
    beta <- suppressWarnings(as.numeric(input$capm_beta)[1])
    if (length(beta) < 1L || !is.finite(beta)) return()
    lab <- .capm_beta_label_html(beta)
    updateNumericInput(session, "capm_beta", label = lab)
    updateNumericInput(session, "ddm_capm_beta", label = lab)
  }, ignoreInit = FALSE)

  # ---------- DDM「採用估算 Ke」↔ WACC「採用估算 rₑ」同步 ----------
  ke_syncing <- reactiveVal(FALSE)
  observeEvent(input$use_estimated_re, {
    if (isTRUE(ke_syncing())) return()
    ke_syncing(TRUE)
    on.exit(ke_syncing(FALSE), add = TRUE)
    if (!identical(isTRUE(input$ddm_use_estimated_re), isTRUE(input$use_estimated_re))) {
      updateCheckboxInput(session, "ddm_use_estimated_re", value = isTRUE(input$use_estimated_re))
    }
  }, ignoreInit = FALSE)
  observeEvent(input$ddm_use_estimated_re, {
    if (isTRUE(ke_syncing())) return()
    ke_syncing(TRUE)
    on.exit(ke_syncing(FALSE), add = TRUE)
    if (!identical(isTRUE(input$use_estimated_re), isTRUE(input$ddm_use_estimated_re))) {
      updateCheckboxInput(session, "use_estimated_re", value = isTRUE(input$ddm_use_estimated_re))
    }
  }, ignoreInit = TRUE)

  # ---------- DDM Ke ↔ WACC rₑ 數值雙向同步（手動覆寫時同源） ----------
  ke_re_num_syncing <- reactiveVal(FALSE)
  observeEvent(input$wacc_re, {
    if (isTRUE(ke_re_num_syncing())) return()
    ke_re_num_syncing(TRUE)
    on.exit(ke_re_num_syncing(FALSE), add = TRUE)
    re <- suppressWarnings(as.numeric(input$wacc_re)[1])
    if (!is.finite(re)) return()
    cur <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
    if (!is.finite(cur) || abs(cur - re) > 1e-4) {
      updateNumericInput(session, "mod_ddm-ke", value = round(re, 2))
    }
  }, ignoreInit = FALSE)
  observeEvent(input[["mod_ddm-ke"]], {
    if (isTRUE(ke_re_num_syncing())) return()
    # 採用 CAPM 估算時以 CAPM／wacc_re 為準，勿把 DDM 殘值回寫
    if (isTRUE(input$use_estimated_re)) return()
    ke_re_num_syncing(TRUE)
    on.exit(ke_re_num_syncing(FALSE), add = TRUE)
    ke <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
    if (!is.finite(ke)) return()
    cur <- suppressWarnings(as.numeric(input$wacc_re)[1])
    if (!is.finite(cur) || abs(cur - ke) > 1e-4) {
      updateNumericInput(session, "wacc_re", value = round(ke, 2))
    }
  }, ignoreInit = TRUE)

  # ---------- DDM CAPM 鏡像 ↔ 正規 CAPM（WACC 分頁）雙向同步 ----------
  capm_mirror_syncing <- reactiveVal(FALSE)
  .sync_capm_pair <- function(from_id, to_id, is_checkbox = FALSE) {
    if (isTRUE(capm_mirror_syncing())) return()
    capm_mirror_syncing(TRUE)
    on.exit(capm_mirror_syncing(FALSE), add = TRUE)
    if (isTRUE(is_checkbox)) {
      v <- isTRUE(input[[from_id]])
      if (!identical(isTRUE(input[[to_id]]), v)) {
        updateCheckboxInput(session, to_id, value = v)
      }
    } else {
      v <- suppressWarnings(as.numeric(input[[from_id]])[1])
      if (!is.finite(v)) return()
      cur <- suppressWarnings(as.numeric(input[[to_id]])[1])
      if (!is.finite(cur) || abs(cur - v) > 1e-6) {
        updateNumericInput(session, to_id, value = v)
      }
    }
  }
  observeEvent(input$capm_rf, { .sync_capm_pair("capm_rf", "ddm_capm_rf") }, ignoreInit = FALSE)
  observeEvent(input$ddm_capm_rf, { .sync_capm_pair("ddm_capm_rf", "capm_rf") }, ignoreInit = TRUE)
  observeEvent(input$capm_rm, { .sync_capm_pair("capm_rm", "ddm_capm_rm") }, ignoreInit = FALSE)
  observeEvent(input$ddm_capm_rm, { .sync_capm_pair("ddm_capm_rm", "capm_rm") }, ignoreInit = TRUE)
  observeEvent(input$capm_beta, { .sync_capm_pair("capm_beta", "ddm_capm_beta") }, ignoreInit = FALSE)
  observeEvent(input$ddm_capm_beta, { .sync_capm_pair("ddm_capm_beta", "capm_beta") }, ignoreInit = TRUE)
  observeEvent(input$sync_gs_beta, {
    .sync_capm_pair("sync_gs_beta", "ddm_sync_gs_beta", is_checkbox = TRUE)
  }, ignoreInit = FALSE)
  observeEvent(input$ddm_sync_gs_beta, {
    .sync_capm_pair("ddm_sync_gs_beta", "sync_gs_beta", is_checkbox = TRUE)
  }, ignoreInit = TRUE)

  .capm_result_html <- function() {
    rf <- suppressWarnings(as.numeric(input$capm_rf)[1])
    b  <- suppressWarnings(as.numeric(input$capm_beta)[1])
    rm <- suppressWarnings(as.numeric(input$capm_rm)[1])
    re <- if (is.finite(rf) && is.finite(b) && is.finite(rm)) rf + b * (rm - rf) else NA_real_
    if (!is.finite(re)) {
      return(tags$p(style = "color:#888;font-size:13px;", .ui_msg("capm_need_inputs")))
    }
    HTML(glue::glue(
      "<div style='padding:8px;border-left:4px solid #222222;background:#f5f5f5;font-size:13px;'>
         rₑ / Ke = Rf + β×(Rm−Rf) = <b>{sprintf('%.2f%%', re)}</b>
       </div>"
    ))
  }
  output$capm_result <- renderUI({ .capm_result_html() })
  output$ddm_capm_result <- renderUI({ .capm_result_html() })
  output$ddm_beta_ke_status <- renderUI({
    ke <- tryCatch(central_ke() * 100, error = function(e) NA_real_)
    b <- suppressWarnings(as.numeric(input$capm_beta)[1])
    HTML(glue::glue(
      "<div style='padding:8px;border-left:4px solid #222222;background:#f5f5f5;font-size:13px;line-height:1.6;'>
         <b>目前 CAPM β</b>：{if (is.finite(b)) sprintf('%.3f', b) else 'N/A'}<br/>
         <b>目前 Ke（供 DDM）</b>：{if (is.finite(ke)) sprintf('%.2f%%', ke) else 'N/A'}<br/>
         <span style='color:#666;font-size:12px;'>來源：{if (isTRUE(input$use_estimated_re)) 'CAPM 估算' else '手動／WACC 分頁 rₑ 覆寫'}</span>
       </div>"
    ))
  })
  output$ddm_ke_bridge_status <- renderUI({
    rf <- suppressWarnings(as.numeric(input$capm_rf)[1])
    rm <- suppressWarnings(as.numeric(input$capm_rm)[1])
    b <- suppressWarnings(as.numeric(input$capm_beta)[1])
    erp <- if (is.finite(rf) && is.finite(rm)) rm - rf else NA_real_
    ke <- tryCatch(central_ke() * 100, error = function(e) NA_real_)
    HTML(glue::glue(
      "<div style='font-size:13px;line-height:1.55;'>
         <b>Rf</b>：{if (is.finite(rf)) sprintf('%.2f%%', rf) else 'N/A'}　
         <b>Rm</b>：{if (is.finite(rm)) sprintf('%.2f%%', rm) else 'N/A'}　
         <b>ERP</b>：{if (is.finite(erp)) sprintf('%.2f%%', erp) else 'N/A'}<br/>
         <b>β</b>：{if (is.finite(b)) sprintf('%.3f', b) else 'N/A'}　
         <b>Ke</b>：{if (is.finite(ke)) sprintf('%.2f%%', ke) else 'N/A'}
       </div>"
    ))
  })
  output$ddm_ke_erp_summary <- renderUI({
    rf <- suppressWarnings(as.numeric(input$capm_rf)[1])
    rm <- suppressWarnings(as.numeric(input$capm_rm)[1])
    erp <- if (is.finite(rf) && is.finite(rm)) rm - rf else NA_real_
    tags$span(
      style = "color:#555;font-size:13px;",
      if (is.finite(erp)) sprintf("ERP (Rm−Rf)＝%.2f%%", erp) else "ERP (Rm−Rf)＝—"
    )
  })
  output$ddm_ke_source_chip <- renderUI({
    tags$span(
      style = "color:#555;font-size:13px;",
      if (isTRUE(input$use_estimated_re)) "來源：CAPM 估算" else "來源：手動／rₑ 覆寫"
    )
  })
  output$ddm_ke_tab_note <- renderUI({
    tags$div(
      style = "background:#f5f5f5; border-left:4px solid #222222; padding:10px 12px; margin-bottom:12px; font-size:13px;",
      tags$b("DDM："),
      "本頁折現率為 Ke（CAPM 股權成本）。版面節奏對齊 DCF→WACC：上列估算、下列 CAPM；β 來源見右側 Beta (β) 分頁。"
    )
  })
  output$ibx_ddm_ke <- renderInfoBox({
    ke <- tryCatch(central_ke() * 100, error = function(e) NA_real_)
    if (!is.finite(ke)) ke <- APP_DEFAULTS$ddm_ke
    infoBox("股權成本 (Ke)", h3(paste0(round(ke, 2), " %")), icon = icon("percent"), color = "teal", fill = TRUE)
  })
  output$ibx_ddm_beta <- renderInfoBox({
    b <- suppressWarnings(as.numeric(input$capm_beta)[1])
    disp <- if (is.finite(b)) sprintf("%.3f", b) else "N/A"
    infoBox("CAPM β", h3(disp), icon = icon("chart-line"), color = "aqua", fill = TRUE)
  })
  output$ibx_ddm_erp <- renderInfoBox({
    rf <- suppressWarnings(as.numeric(input$capm_rf)[1])
    rm <- suppressWarnings(as.numeric(input$capm_rm)[1])
    erp <- if (is.finite(rf) && is.finite(rm)) rm - rf else NA_real_
    disp <- if (is.finite(erp)) paste0(round(erp, 2), " %") else "N/A"
    infoBox("ERP (Rm−Rf)", h3(disp), icon = icon("arrow-up"), color = "purple", fill = TRUE)
  })

  # ---------- 基礎設定：Rolling／Unlevered Beta 預估 ----------
  beta_est_result <- reactiveVal(NULL)  # list(beta, n_obs, method, rs, rm, dates, bench, lookback)
  .beta_price_cache <- new.env(parent = emptyenv())

  .fetch_beta_prices <- function(ticker, period = "5y") {
    tk <- toupper(trimws(as.character(ticker)[1]))
    if (!nzchar(tk)) return(NULL)
    key <- paste0(tk, "|", period)
    if (exists(key, envir = .beta_price_cache, inherits = FALSE)) {
      return(get(key, envir = .beta_price_cache, inherits = FALSE))
    }
    df <- tryCatch(fetch_price_history_df(tk, period), error = function(e) NULL)
    if (!is.null(df) && is.data.frame(df) && nrow(df) >= 40) {
      assign(key, df, envir = .beta_price_cache)
    }
    df
  }

  .estimate_session_beta <- function() {
    req(current_ticker())
    bench <- toupper(trimws(as.character(input$beta_bench %||% "SPY")[1]))
    if (!nzchar(bench)) bench <- "SPY"
    lookback <- as.integer(suppressWarnings(as.numeric(input$beta_lookback_months)[1]))
    if (!is.finite(lookback) || lookback < 12L) lookback <- 60L
    min_obs_cfg <- as.integer(suppressWarnings(as.numeric(input$beta_min_obs)[1]))
    if (!is.finite(min_obs_cfg) || min_obs_cfg < 8L) min_obs_cfg <- 24L
    .min_obs_for <- function(lb) {
      lb <- as.integer(lb)[1]
      if (!is.finite(lb)) lb <- 60L
      # 短窗口放寬門檻，但仍保留最低樣本
      max(8L, min(as.integer(min_obs_cfg), max(8L, lb - 2L)))
    }
    min_obs <- .min_obs_for(lookback)

    stk <- .fetch_beta_prices(current_ticker(), "5y")
    mkt <- .fetch_beta_prices(bench, "5y")
    if (is.null(stk) || is.null(mkt) || nrow(stk) < 40 || nrow(mkt) < 40) {
      return(list(ok = FALSE, reason = "無法取得足夠的股價／基準指數歷史（需約 5 年）。"))
    }
    # Align by date
    merged <- merge(
      data.frame(Date = stk$Date, S = stk$Close),
      data.frame(Date = mkt$Date, M = mkt$Close),
      by = "Date", all = FALSE
    )
    merged <- merged[order(merged$Date), , drop = FALSE]
    if (nrow(merged) < 40) {
      return(list(ok = FALSE, reason = "標的與基準交易日對齊後樣本不足。"))
    }
    as_of <- max(merged$Date, na.rm = TRUE)

    window_months <- c(12L, 24L, 60L)
    window_betas <- setNames(rep(NA_real_, length(window_months)), paste0(window_months, "m"))
    for (wm in window_months) {
      b_i <- estimate_rolling_beta(
        merged$S, merged$M, merged$Date, as_of,
        lookback_months = wm, min_obs = .min_obs_for(wm)
      )
      if (is.finite(b_i)) window_betas[paste0(wm, "m")] <- round(as.numeric(b_i), 3)
    }

    beta <- estimate_rolling_beta(
      merged$S, merged$M, merged$Date, as_of,
      lookback_months = lookback, min_obs = min_obs
    )
    if (!is.finite(beta)) {
      return(list(ok = FALSE, reason = "Rolling β 估計失敗（變異過低或觀測不足）。"))
    }

    # Month-end returns for scatter (same preference as estimate_rolling_beta)
    ym <- format(merged$Date, "%Y-%m")
    mth <- merged[!duplicated(ym, fromLast = TRUE), , drop = FALSE]
    mth <- utils::tail(mth, lookback + 1L)
    method <- "月末報酬"
    if (nrow(mth) < min_obs + 1L) {
      yw <- format(merged$Date, "%Y-%W")
      mth <- merged[!duplicated(yw, fromLast = TRUE), , drop = FALSE]
      mth <- utils::tail(mth, max(lookback * 4L, 52L) + 1L)
      method <- "週報酬（月末樣本不足）"
    }
    rs <- diff(mth$S) / head(mth$S, -1)
    rm <- diff(mth$M) / head(mth$M, -1)
    fine <- is.finite(rs) & is.finite(rm)
    rs <- rs[fine]; rm <- rm[fine]
    list(
      ok = TRUE,
      beta = round(as.numeric(beta), 3),
      n_obs = length(rs),
      method = method,
      rs = rs,
      rm = rm,
      as_of = as_of,
      bench = bench,
      lookback = lookback,
      windows = as.list(window_betas)
    )
  }

  observeEvent(input$calc_beta_est, {
    withProgress(message = "估計 Rolling β…", value = 0.3, {
      res <- tryCatch(.estimate_session_beta(), error = function(e) {
        list(ok = FALSE, reason = e$message)
      })
      incProgress(1)
    })
    beta_est_result(res)
    if (isTRUE(res$ok)) {
      showNotification(
        .ui_msg("notif_rolling_beta_ok", beta = res$beta, method = res$method, n = res$n_obs, bench = res$bench),
        type = "message", duration = 6
      )
      # Rolling 僅對照：估計後不寫入 CAPM
    } else {
      showNotification(.ui_msg("notif_est_failed", reason = res$reason %||% .ui_msg("notif_est_failed_default")), type = "error", duration = 8)
    }
  })

  .apply_rolling_beta_to_capm <- function(silent = FALSE) {
    # 已排除：Rolling β 含市場情緒／短窗噪音，不得寫入 CAPM／Ke／WACC
    if (!isTRUE(silent)) {
      showNotification(
        .ui_msg("notif_rolling_no_write_capm"),
        type = "warning", duration = 8
      )
    }
    invisible(FALSE)
  }
  observeEvent(input$apply_beta_est, { .apply_rolling_beta_to_capm(silent = FALSE) }, ignoreNULL = TRUE)

  # 搜尋新標的後清掉舊估計（避免套用他股 β）
  observeEvent(current_ticker(), {
    beta_est_result(NULL)
  }, ignoreInit = TRUE)

  .vbx_beta_summary_content <- function() {
    b <- .summary_beta_value()
    tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    ready <- isTRUE(user_has_searched()) && is.finite(b) && nzchar(tk)
    ynow_alertable(
      valueBox(
        if (is.finite(b)) round(b, 2) else "N/A",
        "Summary β",
        icon = icon("file-invoice"),
        color = "green",
        width = NULL
      ),
      metric = "summary_beta",
      ticker = tk,
      value = b,
      label = "Summary β",
      ready = ready,
      default_kind = "value_gte"
    )
  }
  output$vbx_beta_summary <- renderUI({ .vbx_beta_summary_content() })

  .vbx_beta_industry_content <- function() {
    b <- .industry_beta_value()
    ind_key <- as.character(input$industry_choice %||% "")[1]
    ind_lab <- if (nzchar(ind_key)) {
      as.character(industry_labels[[ind_key]] %||% ind_key)[1]
    } else {
      "未選產業"
    }
    valueBox(
      if (is.finite(b)) round(b, 2) else "N/A",
      paste0("產業預設 β（", ind_lab, "）"),
      icon = icon("industry"),
      color = "aqua"
    )
  }
  output$vbx_beta_industry <- renderValueBox({ .vbx_beta_industry_content() })

  .vbx_beta_estimated_content <- function() {
    res <- beta_est_result()
    valueBox(
      if (!is.null(res) && isTRUE(res$ok)) res$beta else "—",
      "Rolling β（對照用）",
      icon = icon("chart-line"),
      color = "yellow"
    )
  }
  output$vbx_beta_estimated <- renderValueBox({ .vbx_beta_estimated_content() })

  .beta_est_result_content <- function() {
    res <- beta_est_result()
    if (is.null(res)) {
      return(tags$p(style = "color:#888;font-size:13px;", .ui_msg("beta_roll_need")))
    }
    if (!isTRUE(res$ok)) {
      return(tags$p(style = "color:#c0392b;", res$reason %||% .ui_msg("beta_est_fail")))
    }
    rf <- suppressWarnings(as.numeric(input$capm_rf)[1])
    rm <- suppressWarnings(as.numeric(input$capm_rm)[1])
    ke <- if (is.finite(rf) && is.finite(rm)) rf + res$beta * (rm - rf) else NA_real_
    HTML(glue::glue(
      "<div style='padding:10px;border-left:4px solid #f39c12;background:#fdf6e3;font-size:13px;'>
         <b>β = {res$beta}</b> · {res$method} · n={res$n_obs}<br/>
         基準 {res$bench} · 回溯 {res$lookback} 月 · as-of {res$as_of}<br/>
         <span style='color:#666;'>僅供與估值 β 對照，不會寫入 CAPM／Ke。</span>
         （若誤用）Ke ≈ <b>{if (is.finite(ke)) sprintf('%.2f%%', ke) else 'N/A'}</b>
       </div>"
    ))
  }
  output$beta_est_result <- renderUI({ .beta_est_result_content() })

  .plt_beta_scatter_draw <- function() {
    res <- beta_est_result()
    if (is.null(res) || !isTRUE(res$ok) || length(res$rs) < 5) {
      plot.new()
      text(0.5, 0.5, "估計成功後顯示月／週報酬散佈與回歸線", cex = 1.1, col = "#888")
      return(invisible(NULL))
    }
    df <- data.frame(rm = res$rm * 100, rs = res$rs * 100)
    ggplot(df, aes(x = rm, y = rs)) +
      geom_point(color = "#3c8dbc", alpha = 0.75, size = 2.2) +
      geom_smooth(method = "lm", se = TRUE, color = "#e67e22", fill = "#fdebd0", size = 1) +
      geom_hline(yintercept = 0, color = "#bbb") +
      geom_vline(xintercept = 0, color = "#bbb") +
      theme_minimal(base_size = 13) +
      labs(
        title = glue::glue("報酬回歸：β ≈ {res$beta}（{res$method}）"),
        x = paste0(res$bench, " 報酬 (%)"),
        y = paste0(current_ticker(), " 報酬 (%)")
      )
  }
  output$plt_beta_scatter <- renderPlot({ .plt_beta_scatter_draw() })

  # ---------- Unlevered / Bottom-up 產業 β ----------
  beta_bottomup_result <- reactiveVal(NULL)

  .unlever_beta <- function(beta_l, tax, de) {
    beta_l <- suppressWarnings(as.numeric(beta_l)[1])
    tax <- suppressWarnings(as.numeric(tax)[1])
    de <- suppressWarnings(as.numeric(de)[1])
    if (!is.finite(beta_l) || !is.finite(tax) || !is.finite(de) || de < 0) return(NA_real_)
    denom <- 1 + (1 - tax) * de
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    beta_l / denom
  }
  .relever_beta <- function(beta_u, tax, de) {
    beta_u <- suppressWarnings(as.numeric(beta_u)[1])
    tax <- suppressWarnings(as.numeric(tax)[1])
    de <- suppressWarnings(as.numeric(de)[1])
    if (!is.finite(beta_u) || !is.finite(tax) || !is.finite(de) || de < 0) return(NA_real_)
    beta_u * (1 + (1 - tax) * de)
  }
  .session_tax_decimal <- function() {
    t <- suppressWarnings(as.numeric(input$wacc_tax)[1])
    if (!is.finite(t)) {
      t <- tryCatch(scraped_tax_rate(), error = function(e) APP_DEFAULTS$wacc_tax)
    }
    if (!is.finite(t)) t <- APP_DEFAULTS$wacc_tax
    ias12_tax_ratio(t, from_pct = TRUE)
  }
  .firm_market_de <- function() {
    d <- tryCatch(suppressWarnings(as.numeric(scraped_debt())[1]), error = function(e) NA_real_)
    e <- tryCatch({
      m <- scraped_market_cap()
      suppressWarnings(as.numeric(m$e_val)[1])
    }, error = function(e) NA_real_)
    if ((!is.finite(e) || e <= 0)) {
      df <- tryCatch(summary_data(), error = function(e) NULL)
      if (!is.null(df) && is.data.frame(df)) {
        idx <- grep("^Market Cap", df$Item, ignore.case = TRUE)
        if (length(idx) > 0) e <- parse_financial_number(df$Value[idx[1]])[1]
      }
    }
    if (!is.finite(d)) d <- NA_real_
    if (!is.finite(e) || e <= 0) {
      return(list(ok = FALSE, d = d, e = e, de = NA_real_, source = "missing"))
    }
    if (!is.finite(d) || d < 0) d <- 0
    list(ok = TRUE, d = d, e = e, de = d / e, source = "market_D/E")
  }
  .target_relever_de <- function() {
    mode <- as.character(input$beta_relever_de_mode %||% "current")[1]
    if (identical(mode, "manual")) {
      de <- suppressWarnings(as.numeric(input$beta_target_de)[1])
      if (is.finite(de) && de >= 0) {
        return(list(ok = TRUE, de = de, source = "manual_target_D/E"))
      }
    }
    cur <- .firm_market_de()
    if (isTRUE(cur$ok)) {
      return(list(ok = TRUE, de = cur$de, source = cur$source %||% "market_D/E"))
    }
    list(ok = FALSE, de = NA_real_, source = "missing")
  }
  .resolve_beta_l <- function() {
    src <- as.character(input$beta_bl_source %||% "summary")[1]
    # 舊版殘值：capm 易與 CAPM 輸出循環，改走 Summary
    if (identical(src, "capm")) src <- "summary"
    pick <- function(which) {
      if (identical(which, "rolling")) {
        res <- beta_est_result()
        if (!is.null(res) && isTRUE(res$ok) && is.finite(res$beta)) {
          return(list(beta = res$beta, label = "Rolling 估計"))
        }
        return(NULL)
      }
      if (identical(which, "summary")) {
        b <- .summary_beta_value()
        if (is.finite(b)) return(list(beta = b, label = "Finance Summary"))
        return(NULL)
      }
      NULL
    }
    if (identical(src, "auto")) {
      # 常見優先：Summary 隨手可得；Rolling 僅在已估計時使用
      for (w in c("summary", "rolling")) {
        got <- pick(w)
        if (!is.null(got)) return(got)
      }
      return(list(beta = NA_real_, label = "無可用 β_L"))
    }
    got <- pick(src)
    if (!is.null(got)) return(got)
    list(beta = NA_real_, label = paste0(src, " 無資料"))
  }
  .industry_unlever_proxy <- function(tax) {
    ind <- input$industry_choice
    if (is.null(ind) || !nzchar(ind)) return(NULL)
    inds <- industry_standards[[ind]]
    if (is.null(inds) || is.null(inds$beta_avg)) return(NULL)
    bl <- suppressWarnings(as.numeric(inds$beta_avg)[1])
    dr <- suppressWarnings(as.numeric(inds$debt_ratio_avg)[1])  # D/(D+E)
    if (!is.finite(bl)) return(NULL)
    de <- if (is.finite(dr) && dr >= 0 && dr < 0.95) dr / (1 - dr) else NA_real_
    bu <- .unlever_beta(bl, tax, de)
    list(
      ok = is.finite(bu),
      beta_l = bl,
      de = de,
      beta_u = bu,
      label = paste0("產業基準（", industry_labels[[ind]] %||% ind, "）")
    )
  }

  firm_unlever_reactive <- reactive({
    tax <- .session_tax_decimal()
    de_info <- .firm_market_de()
    tgt <- .target_relever_de()
    bl_info <- .resolve_beta_l()
    # 去槓桿仍用本公司目前市值 D/E；再槓桿可用目標 D/E
    bu <- if (isTRUE(de_info$ok)) .unlever_beta(bl_info$beta, tax, de_info$de) else NA_real_
    list(
      tax = tax,
      de_info = de_info,
      target_de = tgt,
      bl = bl_info$beta,
      bl_label = bl_info$label,
      beta_u = bu,
      relevered = if (is.finite(bu) && isTRUE(tgt$ok)) {
        .relever_beta(bu, tax, tgt$de)
      } else {
        NA_real_
      }
    )
  })

  observeEvent(TRUE, {
    choices <- tryCatch(TICKER_PRESETS, error = function(e) character(0))
    updateSelectizeInput(
      session, "beta_peers",
      choices = choices,
      server = TRUE
    )
  }, once = TRUE)

  .run_beta_bottomup <- function(source_tag = "get_started") {
    tax <- .session_tax_decimal()
    peers <- unique(toupper(trimws(as.character(input$beta_peers %||% character(0)))))
    peers <- peers[nzchar(peers)]
    tk <- tryCatch(toupper(current_ticker()), error = function(e) "")
    peers <- setdiff(peers, tk)

    if (length(peers) == 0) {
      proxy <- .industry_unlever_proxy(tax)
      firm <- firm_unlever_reactive()
      if (is.null(proxy) || !isTRUE(proxy$ok)) {
        beta_bottomup_result(list(
          ok = FALSE,
          reason = "未指定同業，且產業基準無法去槓桿（缺 β 或負債比）。請輸入同業代號後再算。"
        ))
        showNotification(.ui_msg("notif_bottomup_need_peers"), type = "warning", duration = 7)
        return(invisible(NULL))
      }
      tgt <- .target_relever_de()
      rel <- if (isTRUE(tgt$ok)) .relever_beta(proxy$beta_u, tax, tgt$de) else NA_real_
      beta_bottomup_result(list(
        ok = TRUE,
        mode = "industry_proxy",
        tax = tax,
        beta_u_avg = round(proxy$beta_u, 3),
        beta_l_relevered = if (is.finite(rel)) round(rel, 3) else NA_real_,
        target_de = tgt$de,
        n_peers = 0L,
        label = proxy$label,
        peers_df = data.frame(
          代號 = character(0), β_L = numeric(0), D_E = numeric(0), β_u = numeric(0),
          狀態 = character(0), stringsAsFactors = FALSE
        )
      ))
      showNotification(
        .ui_msg("notif_bottomup_industry_proxy", beta = round(proxy$beta_u, 3)),
        type = "message", duration = 6
      )
      return(invisible(NULL))
    }

    withProgress(message = "擷取同業 β／D/E…", value = 0.2, {
      raw <- tryCatch(fetch_beta_unlever_inputs_batch(peers), error = function(e) list())
      incProgress(0.7)
    })

    rows <- lapply(seq_along(raw), function(i) {
      x <- raw[[i]]
      if (is.null(x)) {
        return(data.frame(
          代號 = peers[i], β_L = NA_real_, D_E = NA_real_, β_u = NA_real_,
          狀態 = "擷取失敗", stringsAsFactors = FALSE
        ))
      }
      # reticulate may return named list
      tk_i <- as.character(x[["ticker"]] %||% peers[i])
      bl <- suppressWarnings(as.numeric(x[["beta"]])[1])
      de <- suppressWarnings(as.numeric(x[["de_ratio"]])[1])
      ok <- isTRUE(x[["ok"]]) || (is.finite(bl) && is.finite(de))
      bu <- if (ok) .unlever_beta(bl, tax, de) else NA_real_
      err <- as.character(x[["error"]] %||% "")
      st <- if (is.finite(bu)) "OK" else if (nzchar(err)) err else "缺 β 或 D/E"
      data.frame(
        代號 = tk_i,
        β_L = if (is.finite(bl)) round(bl, 3) else NA_real_,
        D_E = if (is.finite(de)) round(de, 3) else NA_real_,
        β_u = if (is.finite(bu)) round(bu, 3) else NA_real_,
        狀態 = st,
        stringsAsFactors = FALSE
      )
    })
    peers_df <- if (length(rows) > 0) do.call(rbind, rows) else {
      data.frame(代號 = character(0), β_L = numeric(0), D_E = numeric(0),
                 β_u = numeric(0), 狀態 = character(0), stringsAsFactors = FALSE)
    }
    ok_u <- peers_df$β_u[is.finite(peers_df$β_u)]
    if (length(ok_u) == 0) {
      beta_bottomup_result(list(
        ok = FALSE,
        reason = "同業皆無法去槓桿（缺 Yahoo β 或 D/E）。",
        peers_df = peers_df
      ))
      showNotification(.ui_msg("notif_bottomup_fail"), type = "error", duration = 8)
      return(invisible(NULL))
    }
    agg <- as.character(input$beta_bottomup_agg %||% "mean")[1]
    bu_avg <- if (identical(agg, "median")) stats::median(ok_u) else mean(ok_u)
    agg_lab <- if (identical(agg, "median")) "同業中位數" else "同業平均"
    tgt <- .target_relever_de()
    rel <- if (isTRUE(tgt$ok)) .relever_beta(bu_avg, tax, tgt$de) else NA_real_
    beta_bottomup_result(list(
      ok = TRUE,
      mode = "peers",
      tax = tax,
      beta_u_avg = round(bu_avg, 3),
      beta_l_relevered = if (is.finite(rel)) round(rel, 3) else NA_real_,
      target_de = tgt$de,
      n_peers = length(ok_u),
      label = glue::glue("{agg_lab}（n={length(ok_u)}）"),
      peers_df = peers_df
    ))
    showNotification(
      .ui_msg("notif_bottomup_ok", beta = round(bu_avg, 3), n = length(ok_u)),
      type = "message", duration = 6
    )
  }
  observeEvent(input$calc_beta_bottomup, { .run_beta_bottomup("get_started") })

  .compute_selected_beta_u <- function() {
    src <- as.character(input$beta_u_apply_source %||% APP_DEFAULTS$beta_u_apply_source)[1]
    # 允許寫入 CAPM：
    # - summary：Yahoo Summary β（預設）
    # - industry：產業預設 β
    # - bottomup：自選公司平均 Bottom-Up βᵤ
    # - unlever_firm：去槓桿化 Hamada βᵤ
    # - manual：使用者手動 β
    # rolling 一律拒絕（短窗估計僅對照）
    if (identical(src, "rolling")) {
      return(list(ok = FALSE, reason = "sentiment_blocked", src = src))
    }
    .ok <- function(beta, label, kind = "direct", beta_u = NA_real_, de = NA_real_) {
      list(
        ok = TRUE,
        beta = round(as.numeric(beta), 3),
        beta_u = if (is.finite(suppressWarnings(as.numeric(beta_u)[1]))) {
          round(as.numeric(beta_u), 3)
        } else {
          NA_real_
        },
        de = suppressWarnings(as.numeric(de)[1]),
        label = label,
        kind = kind,
        src = src
      )
    }

    if (identical(src, "summary")) {
      b <- .summary_beta_value()
      if (!is.finite(b)) {
        return(list(ok = FALSE, reason = "no_summary", src = src))
      }
      return(.ok(b, "Summary β", "levered"))
    }
    if (identical(src, "industry")) {
      b <- .industry_beta_value()
      if (!is.finite(b)) {
        return(list(ok = FALSE, reason = "no_industry", src = src))
      }
      return(.ok(b, "產業預設 β", "levered"))
    }
    if (identical(src, "bottomup")) {
      res <- beta_bottomup_result()
      if (is.null(res) || !isTRUE(res$ok) || !is.finite(res$beta_u_avg)) {
        return(list(ok = FALSE, reason = "no_bottomup", src = src))
      }
      return(.ok(
        res$beta_u_avg,
        "自選公司平均 Bottom-Up βᵤ",
        "unlever",
        beta_u = res$beta_u_avg
      ))
    }
    if (identical(src, "unlever_firm")) {
      f <- tryCatch(firm_unlever_reactive(), error = function(e) NULL)
      bu <- if (!is.null(f)) suppressWarnings(as.numeric(f$beta_u)[1]) else NA_real_
      if (!is.finite(bu)) {
        return(list(ok = FALSE, reason = "no_firm_unlever", src = src))
      }
      return(.ok(
        bu,
        "去槓桿化 βᵤ（Hamada）",
        "unlever",
        beta_u = bu,
        de = if (!is.null(f$de_info)) f$de_info$de else NA_real_
      ))
    }
    if (identical(src, "manual")) {
      b <- suppressWarnings(as.numeric(input$beta_u_manual)[1])
      if (!is.finite(b) || b < 0) {
        return(list(ok = FALSE, reason = "no_manual", src = src))
      }
      return(.ok(b, "手動 β", "manual"))
    }
    list(ok = FALSE, reason = "unknown_source", src = src)
  }

  .apply_selected_beta_u_to_capm <- function(silent = FALSE, force = FALSE) {
    if (!isTRUE(force) && !isTRUE(input$sync_gs_beta)) {
      return(invisible(FALSE))
    }
    got <- .compute_selected_beta_u()
    if (!isTRUE(got$ok)) {
      if (!isTRUE(silent)) {
        msg <- switch(
          got$reason %||% "",
          "sentiment_blocked" = "Rolling 估計不可寫入 CAPM。請改用 Summary／產業預設／Bottom-Up／去槓桿化 βᵤ。",
          "no_summary" = "尚無 Summary β（請先搜尋標的並載入 Finance Summary）。",
          "no_firm_unlever" = "去槓桿化 βᵤ 尚未就緒（需 β_L 與市值 D/E）。請先搜尋標的。",
          "no_bottomup" = "請先成功計算自選公司平均 Bottom-Up βᵤ（建議先填同業）。",
          "no_industry" = "尚無產業預設 β（請先選擇產業）。",
          "no_manual" = "請先在「手動 β」輸入有效數值。",
          "未知的 β 來源或尚未就緒。"
        )
        showNotification(msg, type = "warning", duration = 7)
      }
      # Rolling／尚未就緒：靜默時不覆寫 CAPM
      return(invisible(FALSE))
    }

    beta_val <- got$beta %||% got$beta_u

    if (isTRUE(force)) {
      .set_sync_gs_beta(TRUE)
    }

    beta_capm_driver("gs")
    capm_beta_dirty(FALSE)
    .set_capm_beta(beta_val)
    .auto_recalc_capm_wacc(notify = !isTRUE(silent), wacc_too = TRUE)
    if (!isTRUE(silent)) {
      showNotification(
        .ui_msg("notif_beta_applied", label = got$label, beta = beta_val),
        type = "message", duration = 7
      )
    }
    invisible(TRUE)
  }
  observeEvent(input$apply_beta_u_selected, { .apply_selected_beta_u_to_capm(silent = FALSE, force = TRUE) })
  # Mirror apply buttons on DCF / DDM / RI Beta tabs (Basic Setup remains canonical)
  observeEvent(input$dcf_apply_beta_u_selected, { .apply_selected_beta_u_to_capm(silent = FALSE, force = TRUE) })
  observeEvent(input$ddm_apply_beta_u_selected, { .apply_selected_beta_u_to_capm(silent = FALSE, force = TRUE) })
  observeEvent(input$ri_apply_beta_u_selected, { .apply_selected_beta_u_to_capm(silent = FALSE, force = TRUE) })

  # β 來源：Basic Setup ↔ DCF ↔ DDM ↔ RI Beta 分頁雙向同步（防回授）
  .beta_apply_source_syncing <- reactiveVal(FALSE)
  .beta_u_apply_source_ids <- function() {
    ids <- tryCatch(.BETA_U_APPLY_SOURCE_IDS, error = function(e) NULL)
    if (is.null(ids) || !length(ids)) {
      ids <- c(
        "beta_u_apply_source", "dcf_beta_u_apply_source",
        "ddm_beta_u_apply_source", "ri_beta_u_apply_source"
      )
    }
    as.character(ids)
  }
  .set_beta_u_apply_source_all <- function(src, except_id = NULL) {
    src <- as.character(src %||% "")[1]
    if (!nzchar(src)) return(invisible(FALSE))
    if (isTRUE(.beta_apply_source_syncing())) return(invisible(FALSE))
    .beta_apply_source_syncing(TRUE)
    on.exit(.beta_apply_source_syncing(FALSE), add = TRUE)
    for (id in .beta_u_apply_source_ids()) {
      if (!is.null(except_id) && identical(id, except_id)) next
      cur <- as.character(input[[id]] %||% "")[1]
      if (!identical(cur, src)) {
        updateRadioButtons(session, id, selected = src)
      }
    }
    invisible(TRUE)
  }

  # Beta Overview／模型鏡像：選項旁動態顯示各來源當前 β（數字粗體）+ 分項說明
  .fmt_beta_choice_val <- function(v, digits = 2) {
    v <- suppressWarnings(as.numeric(v)[1])
    if (is.finite(v)) sprintf("<b>%.*f</b>", digits, v) else "<b>n/a</b>"
  }
  .beta_apply_opt_label <- function(title, val_html, help_txt = NULL) {
    help_html <- if (!is.null(help_txt) && nzchar(help_txt)) {
      paste0(
        " <span style='color:#666;font-size:12px;'>— ",
        htmltools::htmlEscape(help_txt),
        "</span>"
      )
    } else {
      ""
    }
    HTML(paste0(htmltools::htmlEscape(title), " ", val_html, help_html))
  }
  .beta_apply_source_choice_ui <- function() {
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    sum_v <- tryCatch(.summary_beta_value(), error = function(e) NA_real_)
    firm <- tryCatch(firm_unlever_reactive(), error = function(e) NULL)
    firm_v <- if (!is.null(firm)) suppressWarnings(as.numeric(firm$beta_u)[1]) else NA_real_
    bu <- tryCatch(beta_bottomup_result(), error = function(e) NULL)
    bu_v <- if (!is.null(bu) && isTRUE(bu$ok)) bu$beta_u_avg else NA_real_
    ind_b <- tryCatch(.industry_beta_value(), error = function(e) NA_real_)
    man_b <- suppressWarnings(as.numeric(input$beta_u_manual)[1])
    ind_key <- as.character(input$industry_choice %||% "")[1]
    ind_lab <- if (nzchar(ind_key)) {
      as.character(industry_labels[[ind_key]] %||% ind_key)[1]
    } else {
      ui_str("beta_opt_industry_none", loc)
    }
    ind_title <- gsub(
      "{ind}", ind_lab, ui_str("beta_opt_industry_title", loc),
      fixed = TRUE
    )
    values <- c("summary", "industry", "bottomup", "unlever_firm", "manual")
    names_ui <- list(
      .beta_apply_opt_label(
        ui_str("beta_opt_summary_title", loc), .fmt_beta_choice_val(sum_v),
        ui_str("beta_opt_summary_help", loc)
      ),
      .beta_apply_opt_label(
        ind_title, .fmt_beta_choice_val(ind_b),
        ui_str("beta_opt_industry_help", loc)
      ),
      .beta_apply_opt_label(
        ui_str("beta_opt_bottomup_title", loc), .fmt_beta_choice_val(bu_v),
        ui_str("beta_opt_bottomup_help", loc)
      ),
      .beta_apply_opt_label(
        ui_str("beta_opt_unlever_title", loc), .fmt_beta_choice_val(firm_v),
        ui_str("beta_opt_unlever_help", loc)
      ),
      .beta_apply_opt_label(
        ui_str("beta_opt_manual_title", loc), .fmt_beta_choice_val(man_b)
      )
    )
    label_key <- paste(
      c(
        loc,
        .fmt_beta_choice_val(sum_v),
        paste0(.fmt_beta_choice_val(ind_b), "|", ind_lab),
        .fmt_beta_choice_val(bu_v),
        .fmt_beta_choice_val(firm_v),
        .fmt_beta_choice_val(man_b)
      ),
      collapse = "||"
    )
    list(
      choiceNames = names_ui,
      choiceValues = as.list(values),
      values = values,
      label_key = label_key
    )
  }
  beta_apply_choice_labels <- reactiveVal(NULL)
  observe({
    firm_unlever_reactive()
    beta_bottomup_result()
    summary_data()
    beta_est_result()
    input$industry_choice
    input$beta_u_manual
    ui_locale()
    ui_ch <- .beta_apply_source_choice_ui()
    if (identical(ui_ch$label_key, isolate(beta_apply_choice_labels()))) return()
    beta_apply_choice_labels(ui_ch$label_key)
    sel <- isolate(as.character(input$beta_u_apply_source %||% APP_DEFAULTS$beta_u_apply_source)[1])
    if (!sel %in% ui_ch$values) sel <- APP_DEFAULTS$beta_u_apply_source
    beta_apply_choices_updating(TRUE)
    for (id in .beta_u_apply_source_ids()) {
      updateRadioButtons(
        session, id,
        choiceNames = ui_ch$choiceNames,
        choiceValues = ui_ch$choiceValues,
        selected = sel
      )
    }
    session$onFlushed(function() {
      beta_apply_choices_updating(FALSE)
    }, once = TRUE)
  })

  # 基礎設定 → CAPM 自動同步（僅在「與基礎設定同步」勾選時）
  .maybe_sync_gs_beta_to_capm <- function() {
    if (!isTRUE(input$sync_gs_beta)) return(invisible(FALSE))
    src <- as.character(input$beta_u_apply_source %||% APP_DEFAULTS$beta_u_apply_source)[1]

    drv <- as.character(beta_capm_driver() %||% "gs")[1]
    # Rolling 驅動時：僅當選擇器也是 rolling 才允許同步（估計更新）
    if (identical(drv, "rolling") && !identical(src, "rolling")) return(invisible(FALSE))
    # WACC 獨立（手動改 CAPM β）時不自動覆寫
    if (identical(drv, "manual")) return(invisible(FALSE))

    # 舊值／誤選 Rolling 時，改回預設 Summary β（不寫入 CAPM）
    if (identical(src, "rolling")) {
      .set_beta_u_apply_source_all("summary")
      return(invisible(TRUE))
    }

    beta_capm_driver("gs")
    ok <- .apply_selected_beta_u_to_capm(silent = TRUE)
    # 選定來源尚未就緒時維持原選（預設 Summary β），不要自動跳到產業／Bottom-Up
    invisible(isTRUE(ok))
  }

  observeEvent(list(
    input$beta_bl_source,
    input$wacc_tax,
    input$beta_bottomup_agg,
    firm_unlever_reactive(),
    beta_bottomup_result(),
    summary_data(),
    beta_est_result()
  ), {
    if (identical(as.character(beta_capm_driver() %||% "gs")[1], "manual")) return()
    .maybe_sync_gs_beta_to_capm()
  }, ignoreInit = FALSE)

  .on_beta_u_apply_source_change <- function(id) {
    observeEvent(input[[id]], {
      if (isTRUE(beta_apply_choices_updating())) return()
      if (isTRUE(.beta_apply_source_syncing())) return()
      if (isTRUE(beta_link_from_capm())) {
        beta_link_from_capm(FALSE)
        return()
      }
      src <- as.character(input[[id]] %||% "")[1]
      if (identical(src, "rolling")) {
        .set_beta_u_apply_source_all("summary")
        return()
      }
      .set_beta_u_apply_source_all(src, except_id = id)
      if (!isTRUE(input$sync_gs_beta)) return()
      beta_capm_driver("gs")
      .apply_selected_beta_u_to_capm(silent = TRUE)
    }, ignoreInit = TRUE)
  }
  lapply(.beta_u_apply_source_ids(), .on_beta_u_apply_source_change)

  observeEvent(input$beta_u_manual, {
    if (isTRUE(beta_link_from_capm())) {
      beta_link_from_capm(FALSE)
      return()
    }
    src <- as.character(input$beta_u_apply_source %||% "")[1]
    if (!identical(src, "manual")) {
      # Unlevered「手動設算」改值 → 改選手動來源（radio observer 會同步 CAPM）
      beta_link_from_capm(FALSE)
      .set_beta_u_apply_source_all("manual")
      return()
    }
    # 基礎設定側改手動 β → 推回 CAPM（維持連動）
    beta_capm_driver("gs")
    .apply_selected_beta_u_to_capm(silent = TRUE)
  }, ignoreInit = TRUE)

  # 「與基礎設定同步」：勾選則帶入目前基礎設定β；取消則 WACC 獨立
  observeEvent(input$sync_gs_beta, {
    if (isTRUE(sync_gs_beta_updating())) {
      sync_gs_beta_updating(FALSE)
      return()
    }
    if (isTRUE(input$sync_gs_beta)) {
      beta_capm_driver("gs")
      capm_beta_dirty(FALSE)
      .maybe_sync_gs_beta_to_capm()
    } else {
      beta_capm_driver("manual")
      capm_beta_dirty(TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(current_ticker(), {
    beta_bottomup_result(NULL)
  }, ignoreInit = TRUE)

  .vbx_beta_unlever_firm_content <- function() {
    f <- firm_unlever_reactive()
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    lab <- if (identical(loc, "en")) {
      "Unlevered βᵤ (Hamada)"
    } else {
      "去槓桿化 βᵤ（Hamada）"
    }
    bu <- suppressWarnings(as.numeric(f$beta_u)[1])
    tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    ready <- isTRUE(user_has_searched()) && is.finite(bu) && nzchar(tk)
    box <- valueBox(
      if (is.finite(bu)) round(bu, 3) else "—",
      lab,
      icon = icon("balance-scale"),
      color = "orange",
      width = NULL
    )
    if (!exists("ynow_alertable", mode = "function")) return(box)
    ynow_alertable(
      box,
      metric = "hamada_beta_u",
      ticker = tk,
      value = bu,
      label = lab,
      ready = ready,
      default_kind = "value_gte"
    )
  }
  output$vbx_beta_unlever_firm <- renderUI({ .vbx_beta_unlever_firm_content() })

  .vbx_beta_unlever_bottomup_content <- function() {
    res <- beta_bottomup_result()
    valueBox(
      if (!is.null(res) && isTRUE(res$ok)) res$beta_u_avg else "—",
      "自選公司平均 Bottom-Up βᵤ",
      icon = icon("users"),
      color = "teal"
    )
  }
  output$vbx_beta_unlever_bottomup <- renderValueBox({ .vbx_beta_unlever_bottomup_content() })

  # vbx_beta_relevered 已移除（不再提供再槓桿 βe 設定／展示）

  .beta_unlever_firm_result_content <- function() {
    f <- firm_unlever_reactive()
    if (!is.finite(f$bl)) {
      return(tags$p(style = "color:#c0392b;", .ui_msg("beta_no_bl")))
    }
    if (!isTRUE(f$de_info$ok)) {
      return(HTML(glue::glue(
        "<div style='padding:10px;border-left:4px solid #e67e22;background:#fef5e7;font-size:13px;'>
           β_L = <b>{round(f$bl, 3)}</b>（{f$bl_label}）· T = {sprintf('%.1f%%', f$tax * 100)}<br/>
           <span style='color:#c0392b;'>無法計算 D/E（缺 Total Debt 或股權市值）。</span>
         </div>"
      )))
    }
    HTML(glue::glue(
      "<div style='padding:10px;border-left:4px solid #e67e22;background:#fef5e7;font-size:13px;'>
         <b>βᵤ = β_L / (1+(1−T)·D/E)</b>（Hamada；可選為 CAPM「去槓桿化 βᵤ」）<br/>
         β_L = {round(f$bl, 3)}（{f$bl_label}）· T = {sprintf('%.1f%%', f$tax * 100)} ·
         D/E = {sprintf('%.3f', f$de_info$de)}<br/>
         → <b>βᵤ = {if (is.finite(f$beta_u)) sprintf('%.3f', f$beta_u) else 'N/A'}</b>
       </div>"
    ))
  }
  output$beta_unlever_firm_result <- renderUI({ .beta_unlever_firm_result_content() })

  .beta_bottomup_result_content <- function() {
    res <- beta_bottomup_result()
    if (is.null(res)) {
      return(tags$p(style = "color:#888;font-size:13px;",
                    "輸入同業後按「計算 Bottom-Up βᵤ」；或留空以產業基準估算。"))
    }
    if (!isTRUE(res$ok)) {
      return(tags$p(style = "color:#c0392b;", res$reason %||% .ui_msg("beta_calc_fail")))
    }
    # glue 不支援多行 if/else 區塊；先組字串再嵌入
    HTML(glue::glue(
      "<div style='padding:10px;border-left:4px solid #27ae60;background:#eafaf1;font-size:13px;'>
         <b>{res$label}</b> · T = {sprintf('%.1f%%', res$tax * 100)}<br/>
         βᵤ = <b>{res$beta_u_avg}</b>（寫入 CAPM 時直接使用此值）
       </div>"
    ))
  }
  output$beta_bottomup_result <- renderUI({ .beta_bottomup_result_content() })

  .beta_bottomup_peers_table_df <- function() {
    res <- beta_bottomup_result()
    if (is.null(res) || is.null(res$peers_df) || nrow(res$peers_df) == 0) {
      return(data.frame(說明 = "尚無同業明細（產業參考模式或未計算）", stringsAsFactors = FALSE))
    }
    res$peers_df
  }
  output$beta_bottomup_peers_table <- renderTable({
    .beta_bottomup_peers_table_df()
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  # ---------- β 決策樹：用途 → 建議來源 → CAPM ----------
  .beta_has_peers <- function() {
    peers <- unique(toupper(trimws(as.character(input$beta_peers %||% character(0)))))
    peers <- peers[nzchar(peers)]
    tk <- tryCatch(toupper(current_ticker()), error = function(e) "")
    length(setdiff(peers, tk)) > 0
  }
  .beta_decision_recommend <- function() {
    # 預設建議 Summary β；Rolling 不可寫入 CAPM
    purpose <- "valuation"
    sum_b <- tryCatch(.summary_beta_value(), error = function(e) NA_real_)
    bu <- tryCatch(beta_bottomup_result(), error = function(e) NULL)
    has_peers <- .beta_has_peers()
    bu_ready <- !is.null(bu) && isTRUE(bu$ok) && is.finite(bu$beta_u_avg)
    firm <- tryCatch(firm_unlever_reactive(), error = function(e) NULL)
    firm_ready <- !is.null(firm) && is.finite(suppressWarnings(as.numeric(firm$beta_u)[1]))
    ind_b <- tryCatch(.industry_beta_value(), error = function(e) NA_real_)

    if (is.finite(sum_b)) {
      return(list(
        purpose = purpose,
        source = "summary",
        ready = TRUE,
        title = "預設來源：Summary β",
        rationale = "採用 Yahoo Finance Summary「Beta (5Y Monthly)」作為 CAPM 預設 β。",
        next_steps = "可改選產業預設、自選公司平均 Bottom-Up、去槓桿化 βᵤ 或手動定義。"
      ))
    }

    if (is.finite(ind_b)) {
      return(list(
        purpose = purpose,
        source = "industry",
        ready = TRUE,
        title = "備援：產業預設 β",
        rationale = "尚無 Summary β 時，改用所選產業結構 β。",
        next_steps = "請先搜尋標的載入 Summary，或改用 Bottom-Up／去槓桿化 βᵤ。"
      ))
    }

    if (has_peers || bu_ready) {
      if (bu_ready) {
        return(list(
          purpose = purpose,
          source = "bottomup",
          ready = TRUE,
          title = "備援：自選公司平均 Bottom-Up βᵤ",
          rationale = "以同業去槓桿平均／中位 βᵤ 供 CAPM／Ke／WACC。",
          next_steps = "可按下方按鈕套用。Rolling 估計只供對照，不會寫入 CAPM。"
        ))
      }
      return(list(
        purpose = purpose,
        source = "bottomup",
        ready = FALSE,
        title = "備援：Bottom-Up（請先計算）",
        rationale = "已指定同業，但尚未算出 Bottom-Up βᵤ。",
        next_steps = "請至「同業去槓桿」分頁按「計算 Bottom-Up βᵤ」。"
      ))
    }

    if (firm_ready) {
      return(list(
        purpose = purpose,
        source = "unlever_firm",
        ready = TRUE,
        title = "備援：去槓桿化 βᵤ（Hamada）",
        rationale = "以本公司 β_L 經 Hamada 去槓桿得到 βᵤ 供 CAPM。",
        next_steps = "建議補齊 Summary／產業後改回預設來源。"
      ))
    }

    return(list(
      purpose = purpose,
      source = "manual",
      ready = FALSE,
      title = "待輸入：手動 βe 或補齊 Summary／產業",
      rationale = "Summary、產業預設、Bottom-Up 與去槓桿化 βᵤ 皆未就緒。",
      next_steps = "請搜尋標的、選擇產業、填同業計算 Bottom-Up，或手動輸入 βe。"
    ))
  }

  output$beta_decision_tree_panel <- renderUI({
    rec <- .beta_decision_recommend()
    color <- if (isTRUE(rec$ready)) "#1f6f5b" else "#9a5b00"
    bg <- if (isTRUE(rec$ready)) "#e8f6f1" else "#fff7e8"
    tags$div(
      style = paste0(
        "padding:12px 14px;border-left:4px solid ", color,
        ";background:", bg, ";font-size:13px;line-height:1.55;"
      ),
      tags$div(tags$b(rec$title)),
      tags$div(style = "margin-top:6px;", rec$rationale),
      tags$div(
        style = "margin-top:6px;color:#555;",
        tags$span("決策節點："),
        if (identical(rec$source, "summary")) {
          "內在價值 → Summary β（預設）"
        } else if (identical(rec$source, "industry")) {
          "內在價值 → 產業預設 β"
        } else if (identical(rec$source, "bottomup")) {
          "內在價值 → 自選公司平均 Bottom-Up βᵤ"
        } else if (identical(rec$source, "unlever_firm")) {
          "內在價值 → 去槓桿化 βᵤ（Hamada）"
        } else {
          "內在價值 → 待 Summary／產業／Bottom-Up／手動（Rolling 禁用）"
        }
      ),
      tags$div(style = "margin-top:6px;", tags$i(rec$next_steps))
    )
  })

  observeEvent(input$apply_beta_decision_tree, {
    rec <- .beta_decision_recommend()
    src <- as.character(rec$source %||% "")[1]
    if (!nzchar(src)) {
      showNotification(.ui_msg("notif_decision_tree_no_src"), type = "warning", duration = 6)
      return()
    }
    # 估值且建議 Bottom-Up 但未算完：提示先算
    if (identical(src, "bottomup") && !isTRUE(rec$ready)) {
      showNotification(rec$next_steps %||% .ui_msg("notif_need_bottomup_first"), type = "warning", duration = 8)
      .set_beta_u_apply_source_all("bottomup")
      return()
    }
    if (identical(src, "rolling")) {
      showNotification(.ui_msg("notif_rolling_rewrite_src"), type = "warning", duration = 7)
      src <- "summary"
    }
    cur <- as.character(input$beta_u_apply_source %||% "")[1]
    if (!identical(cur, src)) {
      .set_beta_u_apply_source_all(src)
      # radio observer 會同步 CAPM
    } else {
      .apply_selected_beta_u_to_capm(silent = FALSE, force = TRUE)
    }
    showNotification(
      .ui_msg("notif_decision_tree_picked", title = rec$title),
      type = "message", duration = 6
    )
  })

  # 用途固定為估值／去情緒；不再提供監控→Rolling 寫入 CAPM 的切換
  observeEvent(input$beta_purpose, {
    # hidden fixed input; keep CAPM on recommended source
    if (identical(as.character(beta_capm_driver() %||% "gs")[1], "manual")) return()
    rec <- .beta_decision_recommend()
    src <- as.character(rec$source %||% "")[1]
    if (!nzchar(src) || identical(src, "rolling")) return()
    cur <- as.character(input$beta_u_apply_source %||% "")[1]
    if (!identical(cur, src) && src %in% c("summary", "industry", "bottomup", "unlever_firm", "manual")) {
      .set_beta_u_apply_source_all(src)
    }
  }, ignoreInit = TRUE)

  output$beta_crosscheck_panel <- renderUI({
    bu <- tryCatch(beta_bottomup_result(), error = function(e) NULL)
    roll <- tryCatch(beta_est_result(), error = function(e) NULL)
    be <- if (!is.null(bu) && isTRUE(bu$ok)) suppressWarnings(as.numeric(bu$beta_u_avg)[1]) else NA_real_
    rb <- if (!is.null(roll) && isTRUE(roll$ok)) suppressWarnings(as.numeric(roll$beta)[1]) else NA_real_
    if (!is.finite(be) || !is.finite(rb)) {
      return(NULL)
    }
    gap <- abs(be - rb)
    warn <- gap > 0.35
    tags$div(
      style = paste0(
        "margin-top:10px;padding:10px;border-left:4px solid ",
        if (warn) "#c0392b" else "#1a1a1a",
        ";background:", if (warn) "#fdedec" else "#f5f5f5",
        ";font-size:12.5px;line-height:1.5;"
      ),
      tags$b("與估值 β 對照"),
      tags$br(),
      HTML(glue::glue(
        "Bottom-Up βᵤ = <b>{sprintf('%.3f', be)}</b> · Rolling β = <b>{sprintf('%.3f', rb)}</b> · |Δ| = <b>{sprintf('%.3f', gap)}</b>"
      )),
      tags$br(),
      if (warn) {
        tags$span(
          style = "color:#922b21;",
          "差距偏大：請回頭檢查可比公司是否選錯、資本結構是否異常、期間是否含重大事件、股價流動性是否不足。"
        )
      } else {
        tags$span(style = "color:#222222;", "差距可控；CAPM 仍用 Bottom-Up βᵤ，Rolling 只作對照。")
      }
    )
  })

  output$beta_window_table <- renderTable({
    res <- beta_est_result()
    if (is.null(res) || !isTRUE(res$ok)) {
      return(data.frame(
        期間 = c("1Y (12m)", "2Y (24m)", "5Y (60m)"),
        Rollingβ = c("尚未估計", "尚未估計", "尚未估計"),
        用途提示 = c("近期敏感度", "中期變化", "對齊 Yahoo／長期"),
        stringsAsFactors = FALSE
      ))
    }
    wins <- res$windows %||% list()
    fmt <- function(k) {
      v <- suppressWarnings(as.numeric(wins[[k]])[1])
      if (is.finite(v)) sprintf("%.3f", v) else "n/a"
    }
    main_lb <- as.integer(res$lookback %||% NA_integer_)
    mark <- function(m) if (is.finite(main_lb) && identical(as.integer(m), main_lb)) "← 主期間" else ""
    data.frame(
      期間 = c("1Y (12m)", "2Y (24m)", "5Y (60m)"),
      Rollingβ = c(fmt("12m"), fmt("24m"), fmt("60m")),
      備註 = c(mark(12L), mark(24L), mark(60L)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  # 保留：切換產業時刷新 Rm／成長／P/B；Beta 僅在基礎設定來源為產業且同步開啟時由上方處理
  observeEvent(input$industry_choice, {
    req(input$industry_choice)
    inds <- industry_standards[[input$industry_choice]]
    if (!is.null(inds)) {
      updateNumericInput(session, "capm_rm", value = inds$rm_avg)
      
      # 同步短期成長／P/B 區間（有設定才更新）
      if (!is.null(inds$rev_growth)) {
        g_mid <- round(max(2, min(mean(inds$rev_growth), 12)), 2)
        updateNumericInput(session, "custom_g", value = g_mid)
        updateNumericInput(session, "g_stage1", value = g_mid)
      }
      # 僅在勾選「套用產業預設本淨比」時覆寫 P/B 區間
      if (isTRUE(input[["mod_pb-use_industry_pb"]]) &&
          !is.null(inds$pb_band) && length(inds$pb_band) >= 2) {
        lo <- inds$pb_band[1]; hi <- inds$pb_band[2]
        mid <- if (length(inds$pb_band) >= 3) inds$pb_band[3] else mean(c(lo, hi))
        updateNumericInput(session, "mod_pb-pb_low",  value = round(lo, 2))
        updateNumericInput(session, "mod_pb-pb_mid",  value = round(mid, 2))
        updateNumericInput(session, "mod_pb-pb_high", value = round(hi, 2))
      }
    }
  })
  
  estimated_g_meta <- reactiveValues(method = NULL, fund_res = NULL, source = NULL, raw_g = NULL)
  .clamp_near_term_g_pct <- function(g, lo = -5, hi = 25) {
    g <- suppressWarnings(as.numeric(g)[1])
    if (!is.finite(g)) return(NA_real_)
    max(lo, min(hi, g))
  }
  .yoy_rates_newest_first <- function(vec, abs_cap = 1) {
    # vec: newest → oldest. Return chronological YoY rates in (-abs_cap, abs_cap).
    x <- suppressWarnings(as.numeric(vec))
    x <- x[is.finite(x)]
    if (length(x) < 2L) return(numeric(0))
    chrono <- rev(x)
    rates <- diff(chrono) / abs(head(chrono, -1))
    rates <- rates[is.finite(rates)]
    rates[rates > -abs_cap & rates < abs_cap]
  }
  .series_cagr_pct_newest_first <- function(vec) {
    x <- suppressWarnings(as.numeric(vec))
    x <- x[is.finite(x)]
    if (length(x) < 2L) return(NA_real_)
    chrono <- rev(x)
    a <- head(chrono, 1); b <- tail(chrono, 1)
    n <- length(chrono) - 1L
    if (is.finite(a) && is.finite(b) && a > 0 && b > 0 && n > 0) {
      return(((b / a)^(1 / n) - 1) * 100)
    }
    rates <- .yoy_rates_newest_first(vec, abs_cap = 1)
    if (!length(rates)) return(NA_real_)
    mean(rates) * 100
  }
  observe({
    req(d_cash_flow(), d_income_statement(), d_balance_sheet(), input$g_growth_method)
    method <- input$g_growth_method
    if (is.null(method)) return()

    # Projection grows REVENUE then derives FCFF — historical methods must use revenue,
    # not noisy FCF YoY (except fundamental RR×ROIC which is a sustainable-g proxy).
    vec_rev <- tryCatch(
      select_clean_metric_row(d_income_statement(), "Total Revenue", include_ttm = FALSE),
      error = function(e) numeric(0)
    )
    vec_fcf <- tryCatch(
      select_clean_metric_row(d_cash_flow(), "Free Cash Flow", include_ttm = FALSE),
      error = function(e) numeric(0)
    )

    fund_res <- NULL
    source_lab <- NULL
    if (isTRUE(method == "fundamental")) {
      ebit <- select_current_metric(d_income_statement(), "Operating Income|EBIT", "flow")
      tax_rate <- if (!is.null(input$wacc_tax)) input$wacc_tax / 100 else APP_DEFAULTS$wacc_tax / 100
      if (!is.finite(tax_rate)) tax_rate <- 0.21
      nopat <- ebit * (1 - tax_rate)

      total_assets <- select_current_metric(d_balance_sheet(), "Total Assets", "stock")
      curr_liab <- select_current_metric(d_balance_sheet(), "Total Current Liabilities|Current Liabilities", "stock")
      st_debt <- select_current_metric(d_balance_sheet(), "Current Debt|Short Term Debt", "stock")
      cash_eq <- select_current_metric(d_balance_sheet(), "Cash And Cash Equivalents|Cash & Cash Equivalents", "stock")

      st_debt <- ifelse(is.na(st_debt), 0, st_debt)
      curr_liab <- ifelse(is.na(curr_liab), 0, curr_liab)
      cash_eq <- ifelse(is.na(cash_eq), 0, cash_eq)
      total_assets <- ifelse(is.na(total_assets), 0, total_assets)

      invested_capital <- (total_assets - cash_eq) - (curr_liab - st_debt)
      roic <- if (!is.na(invested_capital) && invested_capital > 0) nopat / invested_capital else 0

      capex <- abs(select_current_metric(d_cash_flow(), "Capital Expenditure", "flow"))
      depre <- select_current_metric_any(d_cash_flow(), DA_PATTERNS, "flow")
      cf_delta_nwc <- select_current_metric_any(d_cash_flow(), NWC_CHANGE_PATTERNS, "flow")

      capex <- ifelse(is.na(capex), 0, capex)
      depre <- ifelse(is.na(depre), 0, depre)
      cf_delta_nwc <- ifelse(is.na(cf_delta_nwc), 0, cf_delta_nwc)
      nwc_investment <- -cf_delta_nwc

      if (!is.na(nopat) && nopat > 0) {
        reinvestment_rate <- (capex - depre + nwc_investment) / nopat
      } else {
        reinvestment_rate <- 0
      }
      # RR outside [0,1] is usually accounting noise for a one-year snapshot
      reinvestment_rate <- max(-0.2, min(1.2, reinvestment_rate))

      raw_fund_g <- reinvestment_rate * roic

      ceiling_ns <- input[["mod_fcf-apply_g_ceiling"]]
      apply_ceiling <- if (!is.null(ceiling_ns)) isTRUE(ceiling_ns) else TRUE

      if (apply_ceiling) {
        final_fund_g <- max(-0.05, min(raw_fund_g, 0.25))
      } else {
        final_fund_g <- max(-0.05, min(raw_fund_g, 0.50))
      }

      fund_res <- list(
        g = round(final_fund_g * 100, 2),
        raw_g = round(raw_fund_g * 100, 2),
        roic = roic,
        rr = reinvestment_rate,
        nopat = nopat,
        ic = invested_capital,
        ceiling_applied = apply_ceiling
      )
      source_lab <- "RR×ROIC"
    }

    val_raw <- switch(
      method,
      "fundamental" = if (!is.null(fund_res)) fund_res$g else NA_real_,
      "cagr" = .series_cagr_pct_newest_first(vec_rev),
      "mean" = {
        rates <- .yoy_rates_newest_first(vec_rev, abs_cap = 1)
        if (length(rates)) mean(rates) * 100 else NA_real_
      },
      "median" = {
        rates <- .yoy_rates_newest_first(vec_rev, abs_cap = 1)
        if (length(rates)) stats::median(rates) * 100 else NA_real_
      },
      "last_year" = {
        if (length(vec_rev) >= 2L && is.finite(vec_rev[1]) && is.finite(vec_rev[2]) && vec_rev[2] != 0) {
          ((vec_rev[1] - vec_rev[2]) / abs(vec_rev[2])) * 100
        } else NA_real_
      },
      "custom" = suppressWarnings(as.numeric(input$custom_g)[1]),
      NA_real_
    )

    # 封頂前原始成長率：供 UI 決定是否顯示 25% 防呆
    raw_g_pct <- if (identical(method, "fundamental") && !is.null(fund_res)) {
      fund_res$raw_g
    } else {
      suppressWarnings(as.numeric(val_raw)[1])
    }
    estimated_g_meta$raw_g <- raw_g_pct
    estimated_g_raw(if (is.finite(raw_g_pct)) raw_g_pct else NULL)

    # Near-term growth fed into revenue projection.
    # raw > 25% 且防呆勾選（或尚未渲染）時封頂 25%；解除防呆時放寬（非自訂上限 50%）。
    needs_ceiling_ui <- is.finite(raw_g_pct) && raw_g_pct > 25
    ceil_ns <- input[["mod_fcf-apply_g_ceiling"]]
    ceil_on <- if (needs_ceiling_ui) {
      if (is.null(ceil_ns)) TRUE else isTRUE(ceil_ns)
    } else {
      TRUE
    }
    val <- if (identical(method, "custom")) {
      raw <- suppressWarnings(as.numeric(val_raw)[1])
      if (needs_ceiling_ui && isTRUE(ceil_on)) min(raw, 25) else raw
    } else if (identical(method, "fundamental")) {
      suppressWarnings(as.numeric(val_raw)[1])
    } else {
      hi <- if (needs_ceiling_ui && !isTRUE(ceil_on)) 50 else 25
      .clamp_near_term_g_pct(val_raw, lo = -5, hi = hi)
    }
    if (is.null(source_lab)) {
      source_lab <- switch(
        method,
        "cagr" = "營收 CAGR",
        "mean" = "營收 YoY 平均",
        "median" = "營收 YoY 中位",
        "last_year" = "營收最近一年",
        "custom" = "自訂",
        method
      )
    }

    if (is.null(val) || length(val) < 1 || is.na(val) || !is.finite(val)) {
      # Fallback: if revenue path fails but FCF exists, try mild FCF mean (still clamped)
      if (!identical(method, "custom") && !identical(method, "fundamental")) {
        fcf_rates <- .yoy_rates_newest_first(vec_fcf, abs_cap = 1)
        if (length(fcf_rates)) {
          fb_raw <- mean(fcf_rates) * 100
          raw_g_pct <- fb_raw
          estimated_g_meta$raw_g <- raw_g_pct
          estimated_g_raw(if (is.finite(raw_g_pct)) raw_g_pct else NULL)
          needs_ceiling_ui <- is.finite(fb_raw) && fb_raw > 25
          ceil_ns <- input[["mod_fcf-apply_g_ceiling"]]
          ceil_on <- if (needs_ceiling_ui) {
            if (is.null(ceil_ns)) TRUE else isTRUE(ceil_ns)
          } else TRUE
          hi <- if (needs_ceiling_ui && !isTRUE(ceil_on)) 50 else 25
          val <- .clamp_near_term_g_pct(fb_raw, lo = -5, hi = hi)
          source_lab <- paste0(source_lab, "（營收不足→FCF 回退）")
        }
      }
    }

    if (is.null(val) || length(val) < 1 || is.na(val) || !is.finite(val)) {
      prev_g_na <- isolate(estimated_g())
      estimated_g(NULL)
      estimated_g_raw(NULL)
      estimated_g_meta$source <- NULL
      estimated_g_meta$raw_g <- NULL
      if (!is.null(prev_g_na)) {
        updateSelectInput(session, "g_growth_method", label = "預估營收成長率 (資料不足)")
      }
      return()
    }

    val <- round(as.numeric(val), 2)
    prev_g <- isolate(estimated_g())
    prev_method <- isolate(estimated_g_meta$method)
    estimated_g(val)
    estimated_g_meta$method <- method
    estimated_g_meta$fund_res <- fund_res
    estimated_g_meta$source <- source_lab
    changed <- !identical(prev_g, val) || !identical(prev_method, method)
    if (isTRUE(changed)) {
      updateSelectInput(session, "g_growth_method",
                        label = paste0("預估營收成長率 ➔ ", val, " %"))
    }

    if (!is.na(val)) {
      # 成長率 g1 預設帶入預估營收成長率（含 Two-Stage；自訂方法亦同）
      if (is.null(input$g_stage1) || is.na(as.numeric(input$g_stage1)) ||
          abs(as.numeric(input$g_stage1) - as.numeric(val)) > 1e-4) {
        updateNumericInput(session, "g_stage1", value = val)
      }
    }
    if (isTRUE(changed)) {
      run_calc_trigger(isolate(run_calc_trigger()) + 1)
    }
  })
  output$g_result <- renderUI({
    method <- estimated_g_meta$method
    fund_res <- estimated_g_meta$fund_res
    if (is.null(method)) return(NULL)
    
    if (method == "fundamental" && !is.null(fund_res)) {
      hit_ceiling_raw <- fund_res$raw_g > 25
      
      ceiling_status_msg <- if (hit_ceiling_raw && fund_res$ceiling_applied) {
        glue::glue("<div style='color: #d9534f; margin-top: 5px; font-weight: bold;'>原始成長率過高，已啟動防呆強制封頂。(實際輸出至模型: 25.00 %)</div>")
      } else if (hit_ceiling_raw && !fund_res$ceiling_applied) {
        glue::glue("<div style='color: #8e44ad; margin-top: 5px; font-weight: bold; padding: 5px; border: 1px solid #8e44ad; background: #f4ecf7;'>警告：已解除天花板！將使用極端成長率進行估值 (實際輸出至模型: {fund_res$g} %)</div>")
      } else {
        glue::glue("<div style='color: #00a65a; margin-top: 5px; font-weight: bold;'>成長率處於合理範圍內 (實際輸出至模型: {fund_res$g} %)</div>")
      }
      
      HTML(glue::glue(
        "<div style='padding: 12px; background-color: #fdfaf6; border-left: 4px solid #d35400; font-size: 13px;'>
           <b>學理推估 (Fundamental) 拆解：</b><br/>
           <span style='color: #555;'>公式：投資報酬率 (ROIC) × 再投資率 (RR)</span><br/>
           <span style='color: #1a1a1a; font-weight: bold;'>
             {round(fund_res$roic * 100, 2)} % × {round(fund_res$rr * 100, 2)} % = {fund_res$raw_g} %
           </span><br/>
           {ceiling_status_msg}
         </div>"
      ))
    } else if (method %in% c("cagr", "mean", "median", "last_year")) {
      src <- estimated_g_meta$source %||% "營收"
      cf_tag <- dcf_cf_tag(input$dcf_claim %||% "fcff")
      HTML(glue::glue(
        "<div style='padding: 10px; border-left: 4px solid #222222; font-size: 13px; color: #555;'>
           以<strong>營收</strong>歷史計算近中期成長（{src}），再驅動營收→{cf_tag} 預測。
           已套用 −5%～25% 防呆，避免把單年暴衝／暴跌寫進模型。
         </div>"
      ))
    } else {
      NULL
    }
  })
  
  output$ibx_estimated_g <- renderInfoBox({
    val_g <- if (!is.null(estimated_g())) estimated_g() else "N/A"
    method <- input$g_growth_method %||% "fundamental"
    method_lab <- switch(
      as.character(method),
      "fundamental" = "基本面 RR×ROIC",
      "cagr" = "營收 CAGR",
      "mean" = "營收平均",
      "median" = "營收中位",
      "last_year" = "營收最近一年",
      "custom" = "自訂營收",
      method
    )
    infoBox(
      paste0("預估營收成長率 (", method_lab, ")"),
      paste0(val_g, " %"),
      icon = icon("chart-line"),
      color = "purple",
      fill = TRUE
    )
  })
  
  output$ibx_sgr <- renderInfoBox({ 
    val_sgr <- if (!is.null(input$sgr)) input$sgr else "N/A"
    infoBox("DCF／RI 終值永續成長率 (SGR)", paste0(val_sgr, " %"), icon = icon("infinity"), color = "maroon", fill = TRUE) 
  })
  
  output$ibx_wacc <- renderInfoBox({ 
    val_wacc <- if (!is.null(calculated_wacc())) round(calculated_wacc() * 100, 2) else APP_DEFAULTS$wacc_gordon
    infoBox("WACC", h3(paste0(val_wacc, " %")), icon = icon("percent"), color = "aqua", fill = TRUE) 
  })
  
  output$plt_fcf_trend <- renderPlot({
    session_currency()
    req(fcf_results$df_fcf()) 
    df <- fcf_results$df_fcf()
    claim <- input$dcf_claim %||% "fcff"
    cf_lab <- dcf_cf_full_zh(claim)
    tag <- dcf_cf_tag(claim)
    br <- .dcf_fcfe_bridge()
    df$CF <- extract_dcf_claim_series(
      df, claim,
      interest_after_tax = br$iat, debt0 = br$debt, g_path = br$g
    )
    df$cf_metric <- cf_lab
    
    ggplot(df, aes(x = Year)) +
      geom_col(aes(y = NOPAT, fill = "預估稅後營業利益 (NOPAT)"), width = 0.6, alpha = 0.8) +
      scale_fill_manual(name = "", values = c("預估稅後營業利益 (NOPAT)" = "#00a65a")) +
      geom_line(aes(y = CF, group = 1, color = cf_metric), size = 1.5) +
      geom_point(aes(y = CF, color = cf_metric), size = 3) +
      scale_color_manual(name = "", values = setNames("#3c8dbc", cf_lab)) +
      geom_text(aes(y = CF, label = format_dollar_abbr(CF)),
                vjust = ifelse(df$CF >= 0, -0.5, 1.5), size = 4, fontface = "bold") +
      scale_y_continuous(labels = label_chart_number(prefix = money_prefix())) +
      theme_minimal() +
      labs(title = paste0(tag, " 與 營業利益 成長軌跡"), x = "預測年份", y = paste0("金額 (", money_label(), ")")) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        legend.position = "top"
      )
  })

  # DCF 頁底部：公式參數對 EV 的邊際彈性（相對 ±1%；與個股價格／股數／現金負債無關）
  output$dcf_param_sensitivity_table <- renderTable({
    shock_pct <- if (exists("PARAM_SENSITIVITY_SHOCK", inherits = TRUE)) PARAM_SENSITIVITY_SHOCK else 0.01
    gordon <- identical(input$dcf_mode, "gordon") || is.null(input$dcf_mode)

    n0 <- suppressWarnings(as.numeric(input$years)[1])
    if (!is.finite(n0) || n0 < 1) n0 <- APP_DEFAULTS$years
    n0 <- max(1L, as.integer(round(n0)))

    gt_pct <- suppressWarnings(as.numeric(input$sgr)[1])
    if (!is.finite(gt_pct)) gt_pct <- APP_DEFAULTS$sgr
    gt0 <- gt_pct / 100

    g_near_pct <- suppressWarnings(as.numeric(estimated_g())[1])
    if (!is.finite(g_near_pct)) g_near_pct <- suppressWarnings(as.numeric(input$custom_g)[1])
    if (!is.finite(g_near_pct)) g_near_pct <- 0
    gn0 <- g_near_pct / 100

    if (gordon) {
      w_pct <- suppressWarnings(as.numeric(input$wacc_gordon)[1])
      if (!is.finite(w_pct)) w_pct <- APP_DEFAULTS$wacc_gordon
      r1_0 <- w_pct / 100
      r2_0 <- r1_0
      g1_pct <- g_near_pct
      g2_pct <- g_near_pct
      yr1_0 <- NA_integer_
    } else {
      w1_pct <- suppressWarnings(as.numeric(input$wacc_stage1)[1])
      w2_pct <- suppressWarnings(as.numeric(input$wacc_stage2)[1])
      if (!is.finite(w1_pct)) w1_pct <- APP_DEFAULTS$wacc_stage1
      if (!is.finite(w2_pct)) w2_pct <- APP_DEFAULTS$wacc_stage2
      r1_0 <- w1_pct / 100
      r2_0 <- w2_pct / 100
      g1_pct <- suppressWarnings(as.numeric(input$g_stage1)[1])
      if (!is.finite(g1_pct)) g1_pct <- g_near_pct
      g2_pct <- suppressWarnings(as.numeric(input$g_stage2)[1])
      if (!is.finite(g2_pct)) g2_pct <- gt_pct
      yr1_0 <- suppressWarnings(as.numeric(input$yr_stage1)[1])
      yr1_0 <- clamp_yr_stage1(n0, yr1_0, APP_DEFAULTS$yr_stage1)
    }
    gn_stage <- if (gordon) gn0 else g1_pct / 100
    g2_0 <- if (gordon) gn0 else g2_pct / 100

    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    claim_fcfe <- dcf_claim_is_fcfe(claim)
    cf_lab <- dcf_cf_tag(claim)
    disc_lab <- dcf_disc_tag(claim)
    if (claim_fcfe) {
      ke <- tryCatch(as.numeric(central_ke())[1], error = function(e) NA_real_)
      if (!is.finite(ke) || ke <= 0) ke <- r1_0
      r1_0 <- ke
      r2_0 <- ke
    }

    .ev <- function(n = n0, r1 = r1_0, r2 = r2_0, g_term = gt0,
                    g_near = gn_stage, yr1 = yr1_0, g2 = g2_0, f0 = 1) {
      if (gordon) {
        .dcf_formula_ev(n = n, r1 = r1, g_term = g_term, g_near = g_near, f0 = f0)
      } else {
        .dcf_formula_ev(
          n = n, r1 = r1, r2 = r2, g_term = g_term, g_near = g_near,
          yr_stage1 = yr1, g_stage2 = g2, f0 = f0
        )
      }
    }
    v0 <- .ev()
    shiny::validate(shiny::need(
      is.finite(v0),
      sprintf("基準公式尚未就緒：請確認終值 g < %s 與預測年數。", disc_lab)
    ))

    .rel <- function(x, sign = -1) .param_rel_shock(x, sign = sign, shock = shock_pct)
    .row <- function(param, base_val, unit, v_dn, v_up, note = "") {
      .param_sensitivity_infl_row(param, base_val, unit, v0, v_dn, v_up, note)
    }
    .row_xy <- function(param, base_val, unit, v_dn, v_up, x0, x_dn, x_up, note = "") {
      .param_sensitivity_infl_row_xy(
        param, base_val, unit, v0, v_dn, v_up, x0, x_dn, x_up, note, shock = shock_pct
      )
    }

    # Snapshot WACC weights once (capital-structure formula inputs). Do not re-read
    # live price inside each shock — |ε| of WACC/g/n must not chase the quote.
    we <- NA_real_
    wd <- NA_real_
    tryCatch({
      sh <- .valuation_shares()$shares
      px <- tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
      eq <- if (is.finite(sh) && is.finite(px) && sh > 0 && px > 0) sh * px else NA_real_
      debt <- tryCatch(select_current_metric(d_balance_sheet(), "^Total Debt$", "stock"), error = function(e) NA_real_)
      if (!is.finite(debt) || debt < 0) debt <- 0
      if (is.finite(eq) && eq > 0) {
        tot <- eq + debt
        if (is.finite(tot) && tot > 0) {
          we <- eq / tot
          wd <- debt / tot
        }
      }
    }, error = function(e) NULL)
    if (!is.finite(we) || !is.finite(wd)) {
      we <- NA_real_
      wd <- NA_real_
    }

    rf0 <- suppressWarnings(as.numeric(input$capm_rf)[1])
    beta0 <- suppressWarnings(as.numeric(input$capm_beta)[1])
    rm0 <- suppressWarnings(as.numeric(input$capm_rm)[1])
    rd0 <- suppressWarnings(as.numeric(input$wacc_rd)[1])
    tax0 <- suppressWarnings(as.numeric(input$wacc_tax)[1])
    re0 <- suppressWarnings(as.numeric(input$wacc_re)[1])

    .wacc_pct <- function(rf_pct = rf0, beta = beta0, rm_pct = rm0,
                          re_pct = NULL, rd_pct = rd0, tax_pct = tax0,
                          we_w = we, wd_w = wd) {
      re <- if (!is.null(re_pct) && is.finite(re_pct)) {
        re_pct
      } else if (isTRUE(input$use_estimated_re) && is.finite(rf_pct) && is.finite(beta) && is.finite(rm_pct)) {
        rf_pct + beta * (rm_pct - rf_pct)
      } else {
        re0
      }
      if (!is.finite(re)) return(NA_real_)
      rd <- if (is.finite(rd_pct)) rd_pct else 0
      tax <- if (is.finite(tax_pct)) tax_pct else 0
      we_w <- suppressWarnings(as.numeric(we_w)[1])
      wd_w <- suppressWarnings(as.numeric(wd_w)[1])
      if (!is.finite(we_w) || !is.finite(wd_w)) return(NA_real_)
      we_w * re + wd_w * rd * (1 - tax / 100)
    }
    .ev_wacc_pct <- function(w_pct) {
      if (!is.finite(w_pct)) return(NA_real_)
      r <- w_pct / 100
      .ev(r1 = r, r2 = r)
    }
    .ke_pct <- function(rf_pct = rf0, beta = beta0, rm_pct = rm0, re_pct = NULL) {
      if (!is.null(re_pct) && is.finite(re_pct)) return(re_pct)
      if (isTRUE(input$use_estimated_re) && is.finite(rf_pct) && is.finite(beta) && is.finite(rm_pct)) {
        return(rf_pct + beta * (rm_pct - rf_pct))
      }
      re0
    }
    .shock_disc <- function(...) {
      if (claim_fcfe) .ev_wacc_pct(.ke_pct(...)) else .ev_wacc_pct(.wacc_pct(...))
    }

    rows <- list()
    n_dn <- if (n0 > 1L) n0 - 1L else n0
    n_up <- n0 + 1L
    rows[[length(rows) + 1]] <- .row_xy(
      "預測年數 n", n0, "n",
      .ev(n = n_dn), .ev(n = n_up), n0, n_dn, n_up,
      "公式：明確預測期長度（與 FCFF 金額無關）"
    )

    if (gordon && is.finite(g_near_pct) && abs(g_near_pct) > 1e-8) {
      rows[[length(rows) + 1]] <- .row(
        "近中期營收成長率", g_near_pct, "%",
        .ev(g_near = .rel(g_near_pct, -1) / 100),
        .ev(g_near = .rel(g_near_pct, +1) / 100),
        sprintf("公式：單位 %s 路徑的明確期成長", cf_lab)
      )
    }

    if (is.finite(gt_pct) && abs(gt_pct) > 1e-8) {
      rows[[length(rows) + 1]] <- .row(
        "終值成長率 SGR (g)", gt_pct, "%",
        .ev(g_term = .rel(gt_pct, -1) / 100),
        .ev(g_term = .rel(gt_pct, +1) / 100),
        sprintf("Gordon 終值：ε 取決於 %s 與 g", disc_lab)
      )
    }

    if (gordon || claim_fcfe) {
      w0 <- r1_0 * 100
      rows[[length(rows) + 1]] <- .row(
        paste0("折現率 ", disc_lab), w0, "%",
        .ev(r1 = .rel(w0, -1) / 100, r2 = .rel(w0, -1) / 100),
        .ev(r1 = .rel(w0, +1) / 100, r2 = .rel(w0, +1) / 100),
        if (claim_fcfe) "公式：FCFE 以 Ke 折現（與股價／淨現金無關）" else "公式：明確預測 + Gordon（與股價／淨現金無關）"
      )
    }
    if (!gordon && !claim_fcfe) {
      w1 <- r1_0 * 100
      w2 <- r2_0 * 100
      rows[[length(rows) + 1]] <- .row(
        "折現率 WACC1", w1, "%",
        .ev(r1 = .rel(w1, -1) / 100),
        .ev(r1 = .rel(w1, +1) / 100),
        "兩階段｜高速期折現"
      )
      rows[[length(rows) + 1]] <- .row(
        "折現率 WACC2", w2, "%",
        .ev(r2 = .rel(w2, -1) / 100),
        .ev(r2 = .rel(w2, +1) / 100),
        "兩階段｜終值折現"
      )
    }
    if (!gordon) {
      if (is.finite(g1_pct) && abs(g1_pct) > 1e-8) {
        rows[[length(rows) + 1]] <- .row(
          "高速成長率 g1", g1_pct, "%",
          .ev(g_near = .rel(g1_pct, -1) / 100),
          .ev(g_near = .rel(g1_pct, +1) / 100),
          sprintf("公式：第一階段 %s 路徑成長", cf_lab)
        )
      }
      if (is.finite(g2_pct) && abs(g2_pct) > 1e-8) {
        rows[[length(rows) + 1]] <- .row(
          "第二階段成長率 g2", g2_pct, "%",
          .ev(g2 = .rel(g2_pct, -1) / 100),
          .ev(g2 = .rel(g2_pct, +1) / 100),
          sprintf("公式：第二階段 %s 路徑成長", cf_lab)
        )
      }
      y_dn <- max(1L, yr1_0 - 1L)
      y_up <- min(n0 - 1L, yr1_0 + 1L)
      if (is.finite(yr1_0) && n0 > 2L && y_up > y_dn) {
        rows[[length(rows) + 1]] <- .row_xy(
          "第一階段年數", yr1_0, "n",
          .ev(yr1 = y_dn), .ev(yr1 = y_up), yr1_0, y_dn, y_up,
          "兩階段分界（公式年數，非個股）"
        )
      }
    }

    rows[[length(rows) + 1]] <- .row(
      sprintf("%s 水準（整體）", cf_lab), 1, "x",
      .ev(f0 = 1 - shock_pct),
      .ev(f0 = 1 + shock_pct),
      if (claim_fcfe) "Equity ∝ FCFE ⇒ |ε|=1（公式；與金額無關）" else "EV ∝ FCFF ⇒ |ε|=1（公式；與金額無關）"
    )

    rev0 <- .shared_bridge()$revenue
    if (!is.finite(rev0)) rev0 <- suppressWarnings(as.numeric(input[["mod_fcf-fcf_revenue"]])[1])
    nopat0 <- suppressWarnings(as.numeric(input[["mod_fcf-fcf_nopat"]])[1])
    depre0 <- suppressWarnings(as.numeric(input[["mod_fcf-fcf_depreciation"]])[1])
    capex0 <- suppressWarnings(as.numeric(input[["mod_fcf-fcf_capex"]])[1])
    nwc0 <- suppressWarnings(as.numeric(input[["mod_fcf-fcf_delta_nwc"]])[1])
    capex_pct_in <- suppressWarnings(as.numeric(input[["mod_fcf-proj_capex_rate"]])[1])
    nwc_pct_in <- suppressWarnings(as.numeric(input[["mod_fcf-proj_nwc_rate"]])[1])
    if (is.finite(rev0) && abs(rev0) > 1e-12) {
      nopat_m0 <- if (is.finite(nopat0)) nopat0 / rev0 else 0
      depre_m0 <- if (is.finite(depre0)) depre0 / rev0 else 0
      capex_m0 <- if (is.finite(capex_pct_in)) capex_pct_in / 100 else if (is.finite(capex0)) capex0 / rev0 else 0
      nwc_m0 <- if (is.finite(nwc_pct_in)) nwc_pct_in / 100 else if (is.finite(nwc0)) nwc0 / rev0 else 0
      .fcff <- function(nm = nopat_m0, dm = depre_m0, cm = capex_m0, nwm = nwc_m0) {
        .dcf_unit_fcff_path(
          n = n0, g_near = gn_stage, g_stage2 = g2_0, yr_stage1 = yr1_0,
          nopat_m = nm, depre_m = dm, capex_m = cm, nwc_m = nwm,
          two_stage = !gordon
        )
      }
      .ev_fcff <- function(...) {
        if (gordon) {
          .dcf_formula_ev_from_fcff(.fcff(...), r1 = r1_0, g_term = gt0)
        } else {
          .dcf_formula_ev_from_fcff(
            .fcff(...), r1 = r1_0, r2 = r2_0, g_term = gt0, yr_stage1 = yr1_0
          )
        }
      }
      v_fcff0 <- .ev_fcff()
      if (is.finite(v_fcff0) && abs(v_fcff0) > 1e-9) {
        .row_fcff <- function(param, base_val, unit, v_dn, v_up, note = "") {
          .param_sensitivity_infl_row(param, base_val, unit, v_fcff0, v_dn, v_up, note)
        }
        if (.param_sensitivity_rel_ok(nopat_m0)) {
          rows[[length(rows) + 1]] <- .row_fcff(
            "NOPAT／營收", nopat_m0 * 100, "%",
            .ev_fcff(nm = .rel(nopat_m0, -1)),
            .ev_fcff(nm = .rel(nopat_m0, +1)),
            sprintf("%s 路徑：單位營收 × NOPAT 佔比", cf_lab)
          )
        }
        if (.param_sensitivity_rel_ok(depre_m0)) {
          rows[[length(rows) + 1]] <- .row_fcff(
            "D&A／營收", depre_m0 * 100, "%",
            .ev_fcff(dm = .rel(depre_m0, -1)),
            .ev_fcff(dm = .rel(depre_m0, +1)),
            sprintf("%s 路徑：單位營收 × D&A 佔比", cf_lab)
          )
        }
        if (.param_sensitivity_rel_ok(capex_m0)) {
          rows[[length(rows) + 1]] <- .row_fcff(
            "CapEx／營收", capex_m0 * 100, "%",
            .ev_fcff(cm = .rel(capex_m0, -1)),
            .ev_fcff(cm = .rel(capex_m0, +1)),
            sprintf("%s 路徑：前瞻 CapEx 佔營收比", cf_lab)
          )
        }
        if (.param_sensitivity_rel_ok(nwc_m0)) {
          rows[[length(rows) + 1]] <- .row_fcff(
            "ΔNWC／Δ營收", nwc_m0 * 100, "%",
            .ev_fcff(nwm = .rel(nwc_m0, -1)),
            .ev_fcff(nwm = .rel(nwc_m0, +1)),
            sprintf("%s 路徑：前瞻 ΔNWC／ΔRevenue", cf_lab)
          )
        }
      }
    }

    use_capm <- isTRUE(input$use_estimated_re)
    capm_note <- if (claim_fcfe) "Ke 公式：CAPM → rₑ" else "WACC 公式：CAPM → rₑ（權重為本次資本結構）"
    if (use_capm && is.finite(rf0)) {
      rows[[length(rows) + 1]] <- .row(
        "無風險利率 Rf", rf0, "%",
        .shock_disc(rf_pct = .rel(rf0, -1)),
        .shock_disc(rf_pct = .rel(rf0, +1)),
        capm_note
      )
    }
    if (use_capm && is.finite(beta0)) {
      rows[[length(rows) + 1]] <- .row(
        "Beta (β)", beta0, "x",
        .shock_disc(beta = .rel(beta0, -1)),
        .shock_disc(beta = .rel(beta0, +1)),
        capm_note
      )
    }
    if (use_capm && is.finite(rm0)) {
      rows[[length(rows) + 1]] <- .row(
        "市場報酬率 Rm", rm0, "%",
        .shock_disc(rm_pct = .rel(rm0, -1)),
        .shock_disc(rm_pct = .rel(rm0, +1)),
        capm_note
      )
    }
    if (is.finite(re0) && !use_capm) {
      rows[[length(rows) + 1]] <- .row(
        "股權成本 rₑ", re0, "%",
        .shock_disc(re_pct = .rel(re0, -1)),
        .shock_disc(re_pct = .rel(re0, +1)),
        if (claim_fcfe) "Ke 公式：手動 rₑ（未勾選 CAPM）" else "WACC 公式：手動 rₑ（未勾選 CAPM）"
      )
    }
    .w_ok <- function(w) is.finite(w) && w >= 0 && w <= 1
    .wd_debt_ok <- isTRUE(is.finite(wd) && wd > 1e-8)
    if (!claim_fcfe && .param_sensitivity_rel_ok(we) && .wd_debt_ok) {
      we_dn <- .rel(we, -1)
      we_up <- .rel(we, +1)
      rows[[length(rows) + 1]] <- .row(
        "股權權重 We", we * 100, "%",
        if (.w_ok(we_dn)) .ev_wacc_pct(.wacc_pct(we_w = we_dn, wd_w = 1 - we_dn)) else NA_real_,
        if (.w_ok(we_up)) .ev_wacc_pct(.wacc_pct(we_w = we_up, wd_w = 1 - we_up)) else NA_real_,
        "WACC 公式：We + Wd = 1（相對衝擊後補齊 Wd）"
      )
    }
    if (!claim_fcfe && .param_sensitivity_rel_ok(wd) && .wd_debt_ok) {
      wd_dn <- .rel(wd, -1)
      wd_up <- .rel(wd, +1)
      rows[[length(rows) + 1]] <- .row(
        "負債權重 Wd", wd * 100, "%",
        if (.w_ok(wd_dn)) .ev_wacc_pct(.wacc_pct(we_w = 1 - wd_dn, wd_w = wd_dn)) else NA_real_,
        if (.w_ok(wd_up)) .ev_wacc_pct(.wacc_pct(we_w = 1 - wd_up, wd_w = wd_up)) else NA_real_,
        "WACC 公式：We + Wd = 1（相對衝擊後補齊 We）"
      )
    }
    if (!claim_fcfe && is.finite(rd0) && .wd_debt_ok) {
      rows[[length(rows) + 1]] <- .row(
        "負債成本 rᵈ", rd0, "%",
        .ev_wacc_pct(.wacc_pct(rd_pct = .rel(rd0, -1))),
        .ev_wacc_pct(.wacc_pct(rd_pct = .rel(rd0, +1))),
        "WACC 公式：wₑrₑ + wᵈrᵈ(1−T)"
      )
    }
    if (!claim_fcfe && is.finite(tax0) && .wd_debt_ok) {
      rows[[length(rows) + 1]] <- .row(
        "所得稅率 T", tax0, "%",
        .ev_wacc_pct(.wacc_pct(tax_pct = .rel(tax0, -1))),
        .ev_wacc_pct(.wacc_pct(tax_pct = .rel(tax0, +1))),
        "WACC 公式：稅盾 rᵈ×(1−T)"
      )
    }

    out <- do.call(rbind, rows)
    out <- .param_sensitivity_sort_by_abs_eps(out)
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  observeEvent(input$calc_capm, {
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = FALSE)
  })
  observeEvent(input$calc_ddm_capm, {
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = FALSE)
  })
  observeEvent(input$calc_ddm_ke, {
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = FALSE)
  })
  
  .auto_recalc_capm_wacc <- function(notify = FALSE, wacc_too = TRUE, rf_override = NULL) {
    # CAPM → Re
    rf <- if (!is.null(rf_override) && is.finite(as.numeric(rf_override))) {
      as.numeric(rf_override)
    } else {
      suppressWarnings(as.numeric(input$capm_rf))
    }
    beta <- suppressWarnings(as.numeric(input$capm_beta))
    rm <- suppressWarnings(as.numeric(input$capm_rm))
    if (is.finite(rf) && is.finite(beta) && is.finite(rm)) {
      r_e_est <- (rf / 100) + beta * ((rm / 100) - (rf / 100))
      estimated_re(r_e_est)
      updateNumericInput(session, "wacc_re", value = round(r_e_est * 100, 2))
    }

    if (!isTRUE(wacc_too)) {
      if (isTRUE(notify) && !is.null(estimated_re())) {
        showNotification(
          .ui_msg("notif_re_estimated", re = round(estimated_re() * 100, 2)),
          type = "message"
        )
      }
      return(invisible(NULL))
    }

    # WACC（需財報／股價）；股權市值用報價股數（ADR 自動約當）
    bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
    sum_df <- tryCatch(summary_data(), error = function(e) NULL)
    if (is.null(bs) || !is.data.frame(bs) || nrow(bs) == 0) return(invisible(NULL))

    sh <- tryCatch(
      resolve_valuation_shares(
        bs, sum_df,
        ticker = current_ticker() %||% "",
        quote_currency = quote_currency(),
        financial_currency = statement_currency()
      ),
      error = function(e) NULL
    )
    shares <- if (!is.null(sh)) suppressWarnings(as.numeric(sh$shares)[1]) else NA_real_
    if (!is.finite(shares) || shares <= 0) {
      return(invisible(NULL))
    }

    price_val <- if (!is.null(sh)) suppressWarnings(as.numeric(sh$price)[1]) else NA_real_
    mcap_val <- if (!is.null(sh)) suppressWarnings(as.numeric(sh$market_cap)[1]) else NA_real_
    equity_mv <- if (is.finite(mcap_val) && mcap_val > 0) {
      mcap_val
    } else if (is.finite(price_val) && price_val > 0) {
      shares * price_val
    } else {
      select_current_metric(bs, "Common Stock Equity", "stock")
    }
    debt <- .shared_bridge()$debt
    if (!is.finite(debt) || debt < 0) debt <- 0
    if (is.na(equity_mv) || equity_mv <= 0) return(invisible(NULL))

    total_capital <- equity_mv + debt
    if (!is.finite(total_capital) || total_capital <= 0) return(invisible(NULL))

    r_e <- if (isTRUE(input$use_estimated_re) && !is.null(estimated_re())) {
      estimated_re()
    } else if (!is.null(input$wacc_re) && is.finite(input$wacc_re)) {
      input$wacc_re / 100
    } else {
      APP_DEFAULTS$wacc_re / 100
    }
    r_d <- suppressWarnings(as.numeric(input$wacc_rd)[1]) / 100
    if (!is.finite(r_d) || r_d < 0) {
      # 無負債時 rᵈ 不影響；有負債則等財報覆寫後再算
      if (isTRUE(debt > 0)) return(invisible(NULL))
      r_d <- 0
    }
    tax <- if (!is.null(input$wacc_tax) && is.finite(input$wacc_tax)) input$wacc_tax / 100 else APP_DEFAULTS$wacc_tax / 100

    wacc <- (equity_mv / total_capital) * r_e + (debt / total_capital) * r_d * (1 - tax)
    if (!is.finite(wacc) || wacc <= 0) return(invisible(NULL))

    calculated_wacc(wacc)
    wacc_percent <- round(wacc * 100, 2)

    if (identical(input$dcf_mode, "gordon") || is.null(input$dcf_mode)) {
      updateNumericInput(session, "wacc_gordon", value = wacc_percent)
    } else {
      updateNumericInput(session, "wacc_stage1", value = wacc_percent)
      updateNumericInput(session, "wacc_stage2", value = wacc_percent)
    }

    if (isTRUE(notify)) {
      showNotification(
        .ui_msg("notif_wacc_auto", wacc = wacc_percent),
        type = "message",
        duration = 5
      )
    }
    invisible(wacc_percent)
  }

  observeEvent(input$calc_wacc, {
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = TRUE)
  })

  # 查詢新股票／財報更新後：自動帶入相關數值並重估 WACC
  observeEvent(list(scraped_financials(), summary_data()), {
    req(scraped_financials(), summary_data())
    rf_now <- tryCatch(as.numeric(cached_get_risk_free_rate()), error = function(e) NA_real_)
    if (is.finite(rf_now) && rf_now > 0) {
      updateNumericInput(session, "capm_rf", value = round(rf_now, 2))
    }
    .auto_recalc_capm_wacc(notify = TRUE, wacc_too = TRUE, rf_override = rf_now)
  }, ignoreInit = TRUE)

  # 產業／Beta／Rm 變更時靜默重估（避免重複通知）
  observeEvent(list(input$capm_beta, input$capm_rm, input$industry_choice), {
    req(scraped_financials(), summary_data())
    .auto_recalc_capm_wacc(notify = FALSE, wacc_too = TRUE)
  }, ignoreInit = TRUE)

  output$ibx_re <- renderInfoBox({
    val_re <- input$wacc_re
    if (is.null(val_re)) val_re <- APP_DEFAULTS$wacc_re
    if (isTRUE(input$use_estimated_re) && !is.null(estimated_re())) val_re <- estimated_re() * 100
    infoBox("股權成本 (rₑ)", h3(paste0(round(val_re, 2), " %")), icon = icon("chart-line"), color = "teal", fill = TRUE)
  })
  
  output$ibx_rd <- renderInfoBox({
    val_rd <- suppressWarnings(as.numeric(input$wacc_rd)[1])
    disp <- if (is.finite(val_rd)) paste0(round(val_rd, 2), " %") else "N/A"
    infoBox("負債成本 (rᵈ)", h3(disp), icon = icon("university"), color = "lime", fill = TRUE)
  })
  
  # ==========================================
  # 📉 DCF Overview 圖：歷史／預測 FCFF（可選折現線）— 恢復上一版 ggplot
  # ==========================================
  output$plt_dcf_trajectory <- renderPlot({
    session_currency()
    req(fcf_results$df_fcf(), current_ticker())
    proj_df <- fcf_results$df_fcf()
    if (is.null(proj_df) || nrow(proj_df) < 1) {
      plot.new()
      text(0.5, 0.5, "⚠️ 財報資料不足，無法繪圖", cex = 1.4)
      return()
    }

    # Overview 固定：歷史／預測 CF + 各年折現現值（PV，不含終值）
    n_years <- nrow(proj_df)
    claim <- input$dcf_claim %||% "fcff"
    hist_lab <- dcf_hist_cf_label(claim)
    fcst_lab <- dcf_fcst_cf_label(claim)
    pv_lab <- dcf_yearly_pv_label(claim)
    tag <- dcf_cf_tag(claim)
    disc_tag <- dcf_disc_tag(claim)
    br <- .dcf_fcfe_bridge()
    cf_vals <- extract_dcf_claim_series(
      proj_df, claim,
      interest_after_tax = br$iat, debt0 = br$debt, g_path = br$g
    )

    hist_df <- tryCatch({
      cf <- d_cash_flow()
      row_idx <- grep("^Free Cash Flow$|Free Cash Flow", cf[[1]], ignore.case = TRUE)
      if (length(row_idx) == 0) return(NULL)
      period_cols <- colnames(cf)[-1]
      period_cols <- period_cols[!grepl("^ttm$", period_cols, ignore.case = TRUE)]
      if (length(period_cols) == 0) return(NULL)
      vals <- parse_financial_number(as.character(cf[row_idx[1], period_cols, drop = FALSE]))
      ord <- rev(seq_along(period_cols))
      data.frame(
        Period = as.character(period_cols[ord]),
        Value = as.numeric(vals[ord]),
        Metric = hist_lab,
        Segment = "History",
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)

    if (!is.null(hist_df)) {
      hist_df <- hist_df[is.finite(hist_df$Value), , drop = FALSE]
    }

    forecast_periods <- as.character(proj_df$Year)
    if (length(forecast_periods) == 0) forecast_periods <- paste0("Y", seq_len(n_years))

    wacc_val <- tryCatch({
      if (dcf_claim_is_fcfe(claim)) {
        ke <- suppressWarnings(as.numeric(br$ke)[1])
        if (!is.finite(ke) || ke <= 0) ke <- 0.1
        rep(ke, n_years)
      } else if (identical(input$dcf_mode, "gordon")) {
        r <- suppressWarnings(as.numeric(input$wacc_gordon)[1]) / 100
        if (!is.finite(r) || r <= -0.999) r <- 0.1
        rep(r, n_years)
      } else {
        s1_yrs <- clamp_yr_stage1(n_years, input$yr_stage1, APP_DEFAULTS$yr_stage1)
        r1 <- suppressWarnings(as.numeric(input$wacc_stage1)[1]) / 100
        r2 <- suppressWarnings(as.numeric(input$wacc_stage2)[1]) / 100
        if (!is.finite(r1) || r1 <= -0.999) r1 <- 0.1
        if (!is.finite(r2) || r2 <= -0.999) r2 <- r1
        c(rep(r1, min(s1_yrs, n_years)), rep(r2, max(n_years - s1_yrs, 0)))
      }
    }, error = function(e) rep(0.1, n_years))

    # Red series = PV of that year's cash flow only. Do not add PV(TV) to year n
    # (that used to spike the last point to enterprise-value scale and flatten history).
    dcf_vals <- dcf_yearly_cf_pv(cf_vals, wacc_val)
    discount_factors <- cumprod(1 + wacc_val)

    g_terminal <- if (is.numeric(input$sgr)) input$sgr / 100 else 0.03
    tv_pack <- dcf_gordon_tv_pv(
      tail(cf_vals, 1), g_terminal, tail(wacc_val, 1),
      if (length(discount_factors) >= 1L) discount_factors[n_years] else NA_real_
    )
    tv_annotation <- ""
    if (is.finite(tv_pack$pv_tv)) {
      tv_annotation <- paste0(
        "紅線＝各年 ", tag, " 以 ", disc_tag, " 折現之現值（不含終值）。",
        "永續終值現值 PV of TV: ", format_dollar_abbr(tv_pack$pv_tv),
        "（未疊入第 ", n_years, " 年，以免壓扁走勢）"
      )
    } else {
      tv_annotation <- paste0(
        "紅線＝各年 ", tag, " 以 ", disc_tag, " 折現之現值（不含終值）。",
        "終值未計（需 ", disc_tag, " > g）"
      )
    }

    forecast_cf <- data.frame(
      Period = forecast_periods,
      Value = as.numeric(cf_vals),
      Metric = fcst_lab,
      Segment = "Forecast",
      stringsAsFactors = FALSE
    )

    plot_parts <- list()
    if (!is.null(hist_df) && nrow(hist_df) > 0) plot_parts <- c(plot_parts, list(hist_df))
    plot_parts <- c(plot_parts, list(forecast_cf))
    plot_parts <- c(plot_parts, list(data.frame(
      Period = forecast_periods,
      Value = as.numeric(dcf_vals),
      Metric = pv_lab,
      Segment = "Forecast",
      stringsAsFactors = FALSE
    )))

    plot_df <- do.call(rbind, plot_parts)
    plot_df <- plot_df[is.finite(plot_df$Value), , drop = FALSE]
    if (nrow(plot_df) == 0) {
      plot.new()
      text(0.5, 0.5, "⚠️ 無可繪製數值", cex = 1.4)
      return()
    }

    x_levels <- unique(c(
      if (!is.null(hist_df) && nrow(hist_df) > 0) hist_df$Period else character(0),
      forecast_periods
    ))
    plot_df$Period <- factor(plot_df$Period, levels = x_levels)
    plot_df$Metric <- factor(
      plot_df$Metric,
      levels = c(hist_lab, fcst_lab, pv_lab)
    )

    title_txt <- paste0(current_ticker(), " - 歷史／預測 ", tag, " vs 各年折現現值")

    color_map <- setNames(
      c("#3498db", "#95a5a6", "#e74c3c"),
      c(hist_lab, fcst_lab, pv_lab)
    )
    lty_map <- setNames(
      c("solid", "solid", "dashed"),
      c(hist_lab, fcst_lab, pv_lab)
    )

    label_cf <- plot_df[as.character(plot_df$Metric) != pv_lab, , drop = FALSE]
    label_pv <- plot_df[as.character(plot_df$Metric) == pv_lab, , drop = FALSE]
    y_pad <- max(abs(plot_df$Value), na.rm = TRUE)
    if (!is.finite(y_pad) || y_pad <= 0) y_pad <- 1

    p <- ggplot(plot_df, aes(x = Period, y = Value, color = Metric, linetype = Metric, group = Metric)) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.8) +
      geom_text(
        data = label_cf,
        aes(label = format_dollar_abbr(Value)),
        vjust = -1.15, size = 3.2, show.legend = FALSE
      )
    if (nrow(label_pv) > 0) {
      p <- p + geom_text(
        data = label_pv,
        aes(label = format_dollar_abbr(Value)),
        vjust = 1.7, size = 3.2, show.legend = FALSE
      )
    }
    p +
      scale_color_manual(values = color_map, drop = TRUE) +
      scale_linetype_manual(values = lty_map, drop = TRUE) +
      scale_y_continuous(labels = label_chart_number(prefix = money_prefix())) +
      expand_limits(y = c(min(plot_df$Value, na.rm = TRUE) - 0.08 * y_pad,
                          max(plot_df$Value, na.rm = TRUE) + 0.12 * y_pad)) +
      theme_minimal(base_size = 14) +
      labs(
        title = title_txt,
        subtitle = tv_annotation,
        x = "期間", y = paste0("金額 (", money_label(), ")")
      ) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "#8e44ad", face = "bold", hjust = 0.5, size = 11)
      )
  })
  
  output$dft_fcf_plot <- renderPlot({
    session_currency()
    df <- fcf_results$df_fcf()
    if (is.null(df) || nrow(df) == 0) { plot.new(); text(0.5, 0.5, "⏳ 等待財報資料匯入...", cex = 1.4); return() }
    fcff_vals <- extract_fcff_series(df)
    plot_df <- data.frame(Year = df$Year, FCFF = fcff_vals, stringsAsFactors = FALSE)
    plot_df <- plot_df[!is.na(plot_df$FCFF), ]
    if (nrow(plot_df) == 0) { plot.new(); text(0.5, 0.5, "⏳ 等待財報資料匯入...", cex = 1.4); return() }
    
    ggplot(plot_df, aes(x = Year, y = FCFF, group = 1)) + 
      geom_line(linewidth = 1.2, color = "steelblue") + 
      geom_point(aes(color = FCFF < 0), size = 3) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "steelblue"), guide = "none") +
      scale_y_continuous(labels = label_chart_number(prefix = money_prefix())) +
      theme_minimal(base_size = 14) +
      labs(title = "FCFF 預測即時預覽", x = "預測期", y = paste0("FCFF (", money_label(), ")")) + theme(legend.position = "top")
  })
  
  # ==========================================
  # 💰 8. DCF 計算核心與企業估值 (對接 FCFF 預測序列)
  # ==========================================
  # Grow trailing FCFF (CFO identity) at the scenario g path. Used only when the
  # NOPAT-margin forecast terminal year is negative.
  .lite_fcff_from_trailing <- function(n, mode) {
    base <- tryCatch(
      latest_hist_fcff(
        d_cash_flow(),
        d_is = d_income_statement(),
        tax = .session_tax_decimal()
      ),
      error = function(e) NA_real_
    )
    base <- suppressWarnings(as.numeric(base)[1])
    if (!is.finite(base) || base <= 0) return(NULL)
    g2 <- suppressWarnings(as.numeric(input$sgr)[1]) / 100
    g1 <- suppressWarnings(as.numeric(input$g_stage1)[1]) / 100
    if (!is.finite(g2)) g2 <- 0
    if (!is.finite(g1)) g1 <- g2
    yr1 <- if (identical(mode, "two_stage")) {
      clamp_yr_stage1(n, input$yr_stage1, APP_DEFAULTS$yr_stage1)
    } else {
      0L
    }
    out <- numeric(n)
    level <- base
    for (i in seq_len(n)) {
      g <- if (yr1 > 0L && i <= yr1) g1 else g2
      if (!is.finite(g)) g <- 0
      level <- level * (1 + g)
      out[i] <- level
    }
    if (!is.finite(out[n]) || out[n] <= 0) return(NULL)
    out
  }

  .execute_dcf_calc <- function(notify = NULL) {
    silent <- isTRUE(isolate(lite_dcf_silent()))
    show_toast <- if (is.null(notify)) !silent else isTRUE(notify)
    des <- if (silent) tryCatch(isolate(lite_desired_des()), error = function(e) NULL) else NULL
    dcf_mode_eff <- as.character(input$dcf_mode %||% "")[1]
    if (silent && !is.null(des)) {
      # Hidden radios lag behind the recommended scenario; don't value the stale mode.
      dcf_mode_eff <- if (isTRUE(des$two_stage)) "two_stage" else "gordon"
    } else if (!nzchar(dcf_mode_eff) && !is.null(des)) {
      dcf_mode_eff <- if (isTRUE(des$two_stage)) "two_stage" else "gordon"
    }
    if (!nzchar(dcf_mode_eff)) dcf_mode_eff <- "gordon"
    if (silent) {
      # Avoid req() abort on hidden radios that have not echoed yet
      if (is.null(current_ticker()) || !nzchar(as.character(current_ticker())[1])) return(NULL)
      if (is.null(input$years) || is.null(fcf_results$df_fcf())) return(NULL)
    } else {
      req(current_ticker(), input$dcf_mode, input$years, fcf_results$df_fcf())
    }

    n <- as.numeric(input$years)
    if (is.na(n) || n <= 0) return(NULL)

    proj_df <- fcf_results$df_fcf()
    future_fcfs <- extract_fcff_series(proj_df)
    nopat_ends <- c(NA_real_, NA_real_)
    if (is.data.frame(proj_df) && all(c("NOPAT", "Revenue") %in% names(proj_df)) && nrow(proj_df) > 0) {
      nm <- suppressWarnings(as.numeric(proj_df$NOPAT) / as.numeric(proj_df$Revenue))
      nopat_ends <- c(nm[1], nm[length(nm)])
    }

    if (length(future_fcfs) != n) {
      if (!silent) showNotification(.ui_msg("notif_dcf_n_mismatch"), type = "error")
      return(NULL)
    }

    # A loss-year NOPAT build can make every forecast FCFF negative even when
    # trailing FCFF (CFO + after-tax interest − CapEx) is positive. Gordon then
    # refuses TV, Run DCF stores no price, and the success toast still fires.
    # Anchor on that trailing FCFF for both Lite auto-calc and Full Run DCF.
    hist_anchor <- FALSE
    if (is.finite(future_fcfs[n]) && future_fcfs[n] < 0) {
      anchored <- .lite_fcff_from_trailing(n, dcf_mode_eff)
      if (!is.null(anchored)) {
        future_fcfs <- anchored
        hist_anchor <- TRUE
      }
    }

    dcf_value <- NA
    g_terminal <- suppressWarnings(as.numeric(input$sgr)[1]) / 100
    if (!is.finite(g_terminal)) g_terminal <- NA_real_

    # Lite: last-chance heal so updateNumericInput lag cannot trip g ≥ discount
    .heal_g_vs_rate <- function(g_dec, rate_dec) {
      if (!is.finite(g_dec) || !is.finite(rate_dec) || rate_dec <= 0) return(g_dec)
      if (g_dec < rate_dec - 1e-9) return(g_dec)
      g_ok_pct <- .clamp_g_below_rate(g_dec * 100, rate_dec * 100, margin = 0.5)
      if (!is.finite(g_ok_pct)) return(g_dec)
      updateNumericInput(session, "sgr", value = round(g_ok_pct, 2))
      g_ok_pct / 100
    }

    claim_pre <- as.character(input$dcf_claim %||% "fcff")[1]
    if (silent && !is.null(des) && nzchar(as.character(des$claim %||% "")[1])) {
      claim_pre <- as.character(des$claim)[1]
    }
    if (identical(dcf_mode_eff, "gordon")) {
      w_g <- suppressWarnings(as.numeric(input$wacc_gordon)[1])
      if (!is.finite(w_g) || w_g <= 0) {
        w_c <- suppressWarnings(as.numeric(calculated_wacc())[1])
        w_g <- if (is.finite(w_c) && w_c > 0) w_c * 100 else APP_DEFAULTS$wacc_gordon
      }
      if (!silent) req(input$sgr, input$wacc_gordon)
      r1 <- w_g / 100
      r2 <- r1

      if (!identical(claim_pre, "fcfe") && is.finite(r2) && is.finite(g_terminal) && g_terminal >= r2) {
        if (silent) {
          g_terminal <- .heal_g_vs_rate(g_terminal, r2)
        } else {
          .notify_g_discount_and_guide("notif_g_ge_wacc", input_id = "sgr")
          return(NULL)
        }
      }
      discount_factors <- cumprod(1 + rep(r1, n))

    } else {
      w1_in <- suppressWarnings(as.numeric(input$wacc_stage1)[1])
      w2_in <- suppressWarnings(as.numeric(input$wacc_stage2)[1])
      w_c <- suppressWarnings(as.numeric(calculated_wacc())[1])
      w_fb <- if (is.finite(w_c) && w_c > 0) w_c * 100 else APP_DEFAULTS$wacc_gordon
      if (!is.finite(w1_in) || w1_in <= 0) w1_in <- w_fb
      if (!is.finite(w2_in) || w2_in <= 0) w2_in <- w_fb
      if (!silent) req(input$g_stage1, input$sgr, input$yr_stage1, input$wacc_stage1, input$wacc_stage2)

      r1 <- w1_in / 100
      r2 <- w2_in / 100

      if (!identical(claim_pre, "fcfe") && is.finite(r2) && is.finite(g_terminal) && g_terminal >= r2) {
        if (silent) {
          g_terminal <- .heal_g_vs_rate(g_terminal, r2)
        } else {
          .notify_g_discount_and_guide("notif_g2_ge_wacc2", input_id = "sgr")
          return(NULL)
        }
      }

      yr1 <- clamp_yr_stage1(n, input$yr_stage1, APP_DEFAULTS$yr_stage1)
      if (yr1 <= 0 || yr1 >= n) {
        if (!silent) showNotification(.ui_msg("notif_yr1_invalid"), type = "error")
        return(NULL)
      }

      wacc_sequence <- c(rep(r1, min(yr1, n)), rep(r2, max(0, n - yr1)))
      discount_factors <- cumprod(1 + wacc_sequence)
    }
    
    if (!identical(claim_pre, "fcfe")) {
      pv_forecast <- sum(future_fcfs / discount_factors)
      last_fcf <- future_fcfs[n]
      tv <- if (exists("dcf_gordon_tv", mode = "function")) {
        dcf_gordon_tv(last_fcf, g_terminal, r2)
      } else if (is.finite(last_fcf) && is.finite(g_terminal) && is.finite(r2) && r2 > g_terminal) {
        (last_fcf * (1 + g_terminal)) / (r2 - g_terminal)
      } else {
        NA_real_
      }
      pv_tv <- if (is.finite(tv)) tv / discount_factors[n] else NA_real_
      
      dcf_value <- if (is.finite(pv_forecast) && is.finite(pv_tv)) pv_forecast + pv_tv else NA_real_
      dcf_value_result(dcf_value)
    }
    
    # ==========================================
    # 🌟 執行橋接參數抓取：現金、負債、股數 (防呆強化版)
    # ==========================================
    
    sb_bridge <- .shared_bridge()
    latest_cash <- sb_bridge$cash
    latest_debt <- sb_bridge$debt
    if (!is.finite(latest_cash) || latest_cash < 0) latest_cash <- 0
    if (!is.finite(latest_debt) || latest_debt < 0) latest_debt <- 0

    share_outstanding <- sb_bridge$shares
    if (!is.finite(share_outstanding) || share_outstanding <= 0) {
      share_outstanding <- tryCatch({
        sh <- .valuation_shares()
        if (!is.null(sh) && is.finite(sh$shares) && sh$shares > 0) sh$shares else NA_real_
      }, error = function(e) NA_real_)
    }

    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    if (silent && !is.null(des) && nzchar(as.character(des$claim %||% "")[1])) {
      claim <- as.character(des$claim)[1]
    }
    if (identical(claim, "fcfe")) {
      ke <- if (isTRUE(input$use_estimated_re) && !is.null(estimated_re())) {
        as.numeric(estimated_re())[1]
      } else {
        suppressWarnings(as.numeric(input$wacc_re)[1]) / 100
      }
      if (!is.finite(ke) || ke <= 0) ke <- r2
      if (!is.finite(ke) || (is.finite(g_terminal) && g_terminal >= ke)) {
        if (silent && is.finite(ke) && ke > 0) {
          g_terminal <- .heal_g_vs_rate(g_terminal, ke)
        } else {
          if (!silent) .notify_g_discount_and_guide("notif_fcfe_g_ge_ke", input_id = "sgr")
          return(NULL)
        }
      }
      rd <- suppressWarnings(as.numeric(input$wacc_rd)[1]) / 100
      if (!is.finite(rd) || rd < 0) rd <- 0
      tax <- suppressWarnings(as.numeric(input$wacc_tax)[1]) / 100
      if (!is.finite(tax)) tax <- APP_DEFAULTS$wacc_tax / 100
      iat <- after_tax_interest(latest_debt, rd, tax)
      fcfe <- fcff_to_fcfe(future_fcfs, interest_after_tax = iat, debt0 = latest_debt, g_path = g_terminal)
      ke_dfs <- cumprod(rep(1 + ke, n))
      pv_forecast <- sum(fcfe / ke_dfs)
      last_fcf <- fcfe[n]
      tv <- if (exists("dcf_gordon_tv", mode = "function")) {
        dcf_gordon_tv(last_fcf, g_terminal, ke)
      } else if (is.finite(last_fcf) && is.finite(g_terminal) && is.finite(ke) && ke > g_terminal) {
        (last_fcf * (1 + g_terminal)) / (ke - g_terminal)
      } else {
        NA_real_
      }
      pv_tv <- if (is.finite(tv)) tv / ke_dfs[n] else NA_real_
      dcf_value <- if (is.finite(pv_forecast) && is.finite(pv_tv)) pv_forecast + pv_tv else NA_real_
      dcf_value_result(dcf_value)
      equity_value <- as.numeric(dcf_value)[1]
    } else {
      # 企業價值 (EV) 轉 股權價值 (Equity Value)
      equity_value <- if (exists("dcf_ev_to_equity", mode = "function")) {
        dcf_ev_to_equity(dcf_value, cash = latest_cash, debt = latest_debt)
      } else {
        as.numeric(dcf_value)[1] + latest_cash - latest_debt
      }
    }

    # 計算每股目標價並防呆（報價幣；拒絕 TWD／普通股標成 USD／ADR）
    sh_info <- tryCatch(.valuation_shares(), error = function(e) NULL)
    px <- if (!is.null(sh_info)) .dcf_per_share(equity_value, sh_info) else NA_real_
    if (is.finite(px)) {
      lite_dcf_block("")
      stock_price_estimate_val(px)
      dcf_audit_snap(list(
        pv_explicit = pv_forecast,
        pv_tv = pv_tv,
        last_fcf = last_fcf,
        discount = if (identical(claim, "fcfe")) ke else r2,
        df_terminal = if (identical(claim, "fcfe")) ke_dfs[n] else discount_factors[n],
        cash = latest_cash,
        debt = latest_debt,
        shares = suppressWarnings(as.numeric(sh_info$shares)[1]),
        price = px,
        equity_claim = identical(claim, "fcfe"),
        g = g_terminal,
        nopat_y1 = nopat_ends[1],
        nopat_yn = nopat_ends[2],
        fcf1 = future_fcfs[1]
      ))
      sh_note <- sh_info$note
      if (show_toast && !is.null(sh_note) && nzchar(sh_note)) {
        showNotification(.ui_msg("notif_dcf_shares_note", note = sh_note), type = "message", duration = 6)
      }
    } else {
      if (silent && is.finite(future_fcfs[n]) && future_fcfs[n] < 0) {
        lite_dcf_block("neg_terminal")
      }
      stock_price_estimate_val(NULL)
      dcf_audit_snap(NULL)
      if (show_toast) {
        eq_now <- suppressWarnings(as.numeric(equity_value)[1])
        msg <- if (is.finite(eq_now) && eq_now < 0) {
          .ui_msg("notif_dcf_neg_equity")
        } else if (!is.finite(eq_now)) {
          NULL
        } else {
          code <- tryCatch(
            attr(px, "alignment_failure"),
            error = function(e) NULL
          )
          if (is.null(code) || (is.character(code) && !nzchar(code[1])) ||
              (length(code) == 1L && is.na(code))) {
            st_eff <- infer_statement_currency(
              statement_currency(), quote_currency(),
              if (!is.null(sh_info)) sh_info$method else NULL
            )
            eq_ccy <- tryCatch(
              equity_money_ccy(d_balance_sheet(), session_currency(), st_eff),
              error = function(e) st_eff
            )
            code <- classify_per_share_alignment_failure(
              statement_ccy = statement_currency(),
              quote_ccy = quote_currency(),
              usd_twd = fx_usd_twd(),
              share_method = if (!is.null(sh_info)) sh_info$method else NULL,
              equity_ccy = eq_ccy,
              shares = if (!is.null(sh_info)) sh_info$shares else NULL,
              adr_ratio = if (!is.null(sh_info)) sh_info$ratio else NULL,
              required_per_share = c(dcf = px),
              method = "dcf"
            )
          }
          toast_key <- per_share_alignment_toast_key(code)
          if (!is.na(toast_key) && nzchar(toast_key)) .ui_msg(toast_key) else NULL
        }
        if (!is.null(msg) && nzchar(msg)) {
          showNotification(msg, type = "warning")
        }
      }
    }

    if (show_toast && is.finite(px)) {
      showNotification(
        .ui_msg("notif_dcf_updated", claim = if (identical(claim, "fcfe")) .ui_msg("notif_dcf_claim_fcfe") else .ui_msg("notif_dcf_claim_fcff")),
        type = "message"
      )
      if (isTRUE(hist_anchor)) {
        showNotification(.ui_msg("notif_dcf_hist_fcff_anchor"), type = "message", duration = 8)
      }
    } else if (show_toast && is.finite(future_fcfs[n]) && future_fcfs[n] < 0) {
      showNotification(.ui_msg("notif_dcf_neg_fcff_skip_tv"), type = "warning", duration = 8)
    }
    invisible(is.finite(px))
  }

  observeEvent(input$calc, { .execute_dcf_calc(notify = TRUE) })

  # Search 後：推薦主模型靜默自動試算（美股／台股；參數未就緒則略過）
  # Lite 智慧分析：先套用推薦參數情境（Two-Stage／SGR 法／claim／折現一致性），
  # 再對主／副模型試算——避免「參數假設錯誤無法計算」。
  observeEvent(list(current_ticker(), ticker_search_gen()), {
    if (isTRUE(.ynow_search_gen_is_discard())) {
      return()
    }
    # Clear prior-ticker DCF so Composite does not keep stale run-state overlays
    stock_price_estimate_val(NULL)
    dcf_audit_snap(NULL)
    dcf_value_result(NULL)
    lite_dcf_block("")
    auto_calc_primary_sig("")
    auto_calc_fail_sig("")
    lite_scenario_applied_sig("")
    lite_desired_des(NULL)
    lite_dcf_silent(FALSE)
    auto_calc_ddm_pulse(0L)
    auto_calc_pb_pulse(0L)
    auto_calc_nav_pulse(0L)
    auto_calc_ri_pulse(0L)
    auto_calc_rel_pulse(0L)
    auto_calc_sotp_pulse(0L)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  lite_mode <- reactive({
    isTRUE(input$ynow_lite_mode)
  })

  .clamp_g_below_rate <- function(g_pct, rate_pct, margin = 0.5) {
    g_pct <- suppressWarnings(as.numeric(g_pct)[1])
    rate_pct <- suppressWarnings(as.numeric(rate_pct)[1])
    if (!is.finite(g_pct)) return(NA_real_)
    if (!is.finite(rate_pct) || rate_pct <= 0) return(g_pct)
    if (g_pct < rate_pct - 1e-6) return(g_pct)
    max(rate_pct - margin, 0)
  }

  .lite_desired_scenario <- function(rec) {
    est <- tryCatch(central_perpetual_g(), error = function(e) NULL)
    want_two <- isTRUE(rec$suggest_two_stage) ||
      isTRUE(!is.null(est) && isTRUE(est$suggest_two_stage))
    method <- as.character(
      if (!is.null(est) && nzchar(as.character(est$recommended_method %||% "")[1])) {
        est$recommended_method
      } else {
        input$perpetual_g_method %||% "fundamental"
      }
    )[1]
    # Claim recommendation (prefer stays FCFF unless clearly FCFF-required reasons)
    # Prefer CFA FCFF identity (CFO + after-tax interest − CapEx); fall back to Yahoo FCF.
    last_fcff <- tryCatch(
      latest_hist_fcff(
        d_cash_flow(),
        d_is = d_income_statement(),
        tax = .session_tax_decimal()
      ),
      error = function(e) NA_real_
    )
    if (!is.finite(suppressWarnings(as.numeric(last_fcff)[1]))) {
      fcf_hist <- tryCatch(
        select_clean_metric_row(d_cash_flow(), "Free Cash Flow", include_ttm = FALSE),
        error = function(e) NULL
      )
      fcf_hist <- suppressWarnings(as.numeric(fcf_hist))
      last_fcff <- NA_real_
      if (length(fcf_hist)) {
        hit <- which(is.finite(fcf_hist))
        if (length(hit)) last_fcff <- fcf_hist[hit[1]]
      }
    }
    br <- tryCatch(.dcf_fcfe_bridge(), error = function(e) list(iat = NA_real_, debt = NA_real_))
    last_fcfe <- tryCatch(
      fcff_to_fcfe(last_fcff, interest_after_tax = br$iat, debt0 = br$debt, g_path = 0)[1],
      error = function(e) NA_real_
    )
    claim_rec <- tryCatch(
      recommend_dcf_claim(
        d_bs = d_balance_sheet(),
        fcff = last_fcff,
        fcfe = last_fcfe
      ),
      error = function(e) list(prefer = "fcff")
    )
    claim <- as.character(claim_rec$prefer %||% "fcff")[1]
    if (!claim %in% c("fcff", "fcfe")) claim <- "fcff"
    list(
      two_stage = want_two,
      method = method,
      claim = claim,
      est = est,
      claim_rec = claim_rec
    )
  }

  .apply_lite_recommended_scenario <- function(rec) {
    des <- .lite_desired_scenario(rec)
    lite_desired_des(des)
    # DCF mode
    if (isTRUE(des$two_stage)) {
      if (!identical(as.character(input$dcf_mode %||% "")[1], "two_stage")) {
        updateRadioButtons(session, "dcf_mode", selected = "two_stage")
      }
    } else if (!identical(as.character(input$dcf_mode %||% "")[1], "gordon")) {
      updateRadioButtons(session, "dcf_mode", selected = "gordon")
    }
    # SGR method (+ lifecycle auto tier)
    cur_method <- as.character(input$perpetual_g_method %||% "")[1]
    if (nzchar(des$method) && !identical(cur_method, des$method)) {
      updateSelectInput(session, "perpetual_g_method", selected = des$method)
      if (identical(des$method, "lifecycle")) {
        updateSelectInput(session, "lifecycle_stage", selected = "auto")
      }
    }
    # Push terminal g / g1 using the *desired* SGR method (not the stale UI method)
    rf_det <- tryCatch(.current_rf_detail(), error = function(e) list(rf_pct = NA_real_, source = "", label = ""))
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    est_forced <- tryCatch(
      estimate_perpetual_g(
        method = des$method %||% "fundamental",
        rf_pct = rf_det$rf_pct,
        rf_source = rf_det$source,
        rf_label = rf_det$label,
        locale = loc,
        d_is = tryCatch(d_income_statement(), error = function(e) NULL),
        d_bs = tryCatch(d_balance_sheet(), error = function(e) NULL),
        d_cf = tryCatch(d_cash_flow(), error = function(e) NULL),
        industry_text = corp_industry_text() %||% "",
        ticker = current_ticker() %||% APP_DEFAULTS$stock_code,
        lifecycle_stage = if (identical(des$method, "lifecycle")) "auto" else (input$lifecycle_stage %||% "auto"),
        wacc_pct = .current_wacc_pct()
      ),
      error = function(e) des$est
    )
    if (!is.null(est_forced)) {
      .push_perpetual_g(est_forced, notify_two_stage = FALSE)
    }
    # Cash-flow claim
    if (!identical(as.character(input$dcf_claim %||% "fcff")[1], des$claim)) {
      updateRadioButtons(session, "dcf_claim", selected = des$claim)
    }
    # Sync WACC inputs from calculated WACC when available
    w <- suppressWarnings(as.numeric(calculated_wacc())[1])
    if (is.finite(w) && w > 0) {
      wp <- round(w * 100, 2)
      if (isTRUE(des$two_stage) || identical(as.character(input$dcf_mode %||% "")[1], "two_stage")) {
        updateNumericInput(session, "wacc_stage1", value = wp)
        updateNumericInput(session, "wacc_stage2", value = wp)
      } else {
        updateNumericInput(session, "wacc_gordon", value = wp)
      }
      # Keep terminal g strictly below discount rate (Gordon / stage-2 / Ke)
      g_now <- suppressWarnings(as.numeric(input$sgr)[1])
      if (!is.finite(g_now) && !is.null(des$est)) {
        g_now <- suppressWarnings(as.numeric(des$est$g_pct)[1])
      }
      g_ok <- .clamp_g_below_rate(g_now, wp, margin = 0.5)
      if (is.finite(g_ok) && (!is.finite(g_now) || abs(g_ok - g_now) > 1e-4)) {
        updateNumericInput(session, "sgr", value = round(g_ok, 2))
        if (isTRUE(input[["mod_ddm-sync_g"]] %||% TRUE)) {
          updateNumericInput(session, "mod_ddm-g", value = round(g_ok, 2))
        }
        updateNumericInput(session, "mod_ri-ri_g", value = round(g_ok, 2))
      }
    }
    # DDM / RI: ensure g < Ke with a small margin
    for (pair in list(
      list(g = "mod_ddm-g", ke = "mod_ddm-ke"),
      list(g = "mod_ri-ri_g", ke = "mod_ri-ri_ke")
    )) {
      g0 <- suppressWarnings(as.numeric(input[[pair$g]])[1])
      ke0 <- suppressWarnings(as.numeric(input[[pair$ke]])[1])
      g1 <- .clamp_g_below_rate(g0, ke0, margin = 0.5)
      if (is.finite(g1) && is.finite(g0) && abs(g1 - g0) > 1e-4) {
        updateNumericInput(session, pair$g, value = round(g1, 2))
      }
    }
    invisible(des)
  }

  # After first scenario apply: keep WACC / terminal g consistent as calculated_wacc settles
  .lite_resync_discount_consistency <- function(des = NULL) {
    w <- suppressWarnings(as.numeric(calculated_wacc())[1])
    if (!is.finite(w) || w <= 0) return(invisible(FALSE))
    wp <- round(w * 100, 2)
    two <- if (!is.null(des)) {
      isTRUE(des$two_stage)
    } else {
      identical(as.character(input$dcf_mode %||% "")[1], "two_stage")
    }
    if (isTRUE(two)) {
      w1 <- suppressWarnings(as.numeric(input$wacc_stage1)[1])
      w2 <- suppressWarnings(as.numeric(input$wacc_stage2)[1])
      if (!is.finite(w1) || abs(w1 - wp) > 0.05) {
        updateNumericInput(session, "wacc_stage1", value = wp)
      }
      if (!is.finite(w2) || abs(w2 - wp) > 0.05) {
        updateNumericInput(session, "wacc_stage2", value = wp)
      }
      r2 <- wp
    } else {
      wg <- suppressWarnings(as.numeric(input$wacc_gordon)[1])
      if (!is.finite(wg) || abs(wg - wp) > 0.05) {
        updateNumericInput(session, "wacc_gordon", value = wp)
      }
      r2 <- wp
    }
    claim <- as.character(input$dcf_claim %||% (des$claim %||% "fcff"))[1]
    if (identical(claim, "fcfe")) {
      ke <- if (isTRUE(input$use_estimated_re) && !is.null(estimated_re()) &&
                is.finite(as.numeric(estimated_re())[1])) {
        as.numeric(estimated_re())[1] * 100
      } else {
        suppressWarnings(as.numeric(input$wacc_re)[1])
      }
      if (is.finite(ke) && ke > 0) r2 <- ke
    }
    g_now <- suppressWarnings(as.numeric(input$sgr)[1])
    g_ok <- .clamp_g_below_rate(g_now, r2, margin = 0.5)
    if (is.finite(g_ok) && is.finite(g_now) && abs(g_ok - g_now) > 1e-4) {
      updateNumericInput(session, "sgr", value = round(g_ok, 2))
      if (isTRUE(input[["mod_ddm-sync_g"]] %||% TRUE)) {
        updateNumericInput(session, "mod_ddm-g", value = round(g_ok, 2))
      }
      updateNumericInput(session, "mod_ri-ri_g", value = round(g_ok, 2))
    }
    for (pair in list(
      list(g = "mod_ddm-g", ke = "mod_ddm-ke"),
      list(g = "mod_ri-ri_g", ke = "mod_ri-ri_ke")
    )) {
      g0 <- suppressWarnings(as.numeric(input[[pair$g]])[1])
      ke0 <- suppressWarnings(as.numeric(input[[pair$ke]])[1])
      g1 <- .clamp_g_below_rate(g0, ke0, margin = 0.5)
      if (is.finite(g1) && is.finite(g0) && abs(g1 - g0) > 1e-4) {
        updateNumericInput(session, pair$g, value = round(g1, 2))
      }
    }
    invisible(TRUE)
  }

  .lite_scenario_matches_ui <- function(des) {
    mode_ok <- if (isTRUE(des$two_stage)) {
      identical(as.character(input$dcf_mode %||% "")[1], "two_stage")
    } else {
      identical(as.character(input$dcf_mode %||% "gordon")[1], "gordon")
    }
    method_ok <- identical(
      as.character(input$perpetual_g_method %||% "")[1],
      as.character(des$method %||% "")[1]
    )
    claim_ok <- identical(
      as.character(input$dcf_claim %||% "fcff")[1],
      as.character(des$claim %||% "fcff")[1]
    )
    isTRUE(mode_ok && method_ok && claim_ok)
  }

  .auto_calc_shares_ready <- function() {
    sh <- tryCatch(.valuation_shares(), error = function(e) NULL)
    if (is.null(sh)) return(FALSE)
    shares <- suppressWarnings(as.numeric(sh$shares)[1])
    if (!is.finite(shares) || shares <= 0) return(FALSE)
    eq_ccy <- tryCatch(
      equity_money_ccy(
        d_balance_sheet(),
        session_currency(),
        infer_statement_currency(statement_currency(), quote_currency(), sh$method)
      ),
      error = function(e) NA_character_
    )
    isTRUE(per_share_bridge_ready(
      statement_currency(),
      quote_currency(),
      usd_twd = fx_usd_twd(),
      share_method = sh$method,
      equity_ccy = eq_ccy
    ))
  }

  .auto_calc_primary_ready <- function(prim) {
    prim <- as.character(prim %||% "")[1]
    if (!nzchar(prim)) return(FALSE)
    if (identical(prim, "ri")) {
      b0 <- suppressWarnings(as.numeric(input[["mod_ri-b0"]])[1])
      ke <- suppressWarnings(as.numeric(input[["mod_ri-ri_ke"]])[1])
      g <- suppressWarnings(as.numeric(input[["mod_ri-ri_g"]])[1])
      if (!(is.finite(b0) && is.finite(ke) && ke > 0 && is.finite(g) && g < ke)) return(FALSE)
      return(isTRUE(.auto_calc_shares_ready()))
    }
    if (identical(prim, "dcf")) {
      proj <- tryCatch(fcf_results$df_fcf(), error = function(e) NULL)
      n <- suppressWarnings(as.numeric(input$years)[1])
      if (is.null(proj) || !is.data.frame(proj) || nrow(proj) < 1L) return(FALSE)
      if (!is.finite(n) || n <= 0L || nrow(proj) != as.integer(n)) return(FALSE)
      # Don't value on the revenue=100 sandbox or a previous ticker's forecast.
      if (isTRUE(isolate(lite_mode()))) {
        rev_in <- .shared_bridge()$revenue
        rev_stmt <- tryCatch(
          select_current_metric(d_income_statement(), "Total Revenue", "flow"),
          error = function(e) NA_real_
        )
        if (is.finite(rev_stmt) && rev_stmt > 0 && is.finite(rev_in) && rev_in > 0) {
          ratio <- rev_in / rev_stmt
          if (ratio < 0.3 || ratio > 3) return(FALSE)
        } else if (is.finite(rev_stmt) && rev_stmt > 1e6) {
          return(FALSE)
        }
      }
      # Prefer settled estimated WACC over placeholder APP_DEFAULTS seed
      w_calc <- suppressWarnings(as.numeric(calculated_wacc())[1])
      lite <- isTRUE(isolate(lite_mode()))
      des <- if (lite) tryCatch(isolate(lite_desired_des()), error = function(e) NULL) else NULL
      # Lite Smart Analysis must not hang when calculated WACC is still NA:
      # fall back to UI / App-default WACC so auto-calc can fire.
      if ((!is.finite(w_calc) || w_calc <= 0) && isTRUE(lite)) {
        wg <- suppressWarnings(as.numeric(input$wacc_gordon)[1])
        w1 <- suppressWarnings(as.numeric(input$wacc_stage1)[1])
        w2 <- suppressWarnings(as.numeric(input$wacc_stage2)[1])
        fallback <- if (isTRUE(des$two_stage)) {
          if (is.finite(w2) && w2 > 0) w2 else if (is.finite(w1) && w1 > 0) w1 else NA_real_
        } else {
          if (is.finite(wg) && wg > 0) wg else NA_real_
        }
        if (!is.finite(fallback) || fallback <= 0) {
          fallback <- suppressWarnings(as.numeric(APP_DEFAULTS$wacc_gordon)[1])
        }
        if (!is.finite(fallback) || fallback <= 0) return(FALSE)
        w_calc <- fallback / 100
      } else if (!is.finite(w_calc) || w_calc <= 0) {
        return(FALSE)
      }
      mode <- as.character(input$dcf_mode %||% "gordon")[1]
      # Prefer stored Lite desired mode when hidden radios have not echoed yet
      if (isTRUE(lite) && !is.null(des)) {
        mode <- if (isTRUE(des$two_stage)) "two_stage" else "gordon"
      }
      if (identical(mode, "gordon")) {
        w <- suppressWarnings(as.numeric(input$wacc_gordon)[1])
        if (!is.finite(w) || w <= 0) {
          if (isTRUE(lite)) w <- w_calc * 100 else return(FALSE)
        }
        r2 <- w
      } else {
        w1 <- suppressWarnings(as.numeric(input$wacc_stage1)[1])
        w2 <- suppressWarnings(as.numeric(input$wacc_stage2)[1])
        if (!is.finite(w1) || w1 <= 0 || !is.finite(w2) || w2 <= 0) {
          if (isTRUE(lite)) {
            w1 <- if (is.finite(w1) && w1 > 0) w1 else w_calc * 100
            w2 <- if (is.finite(w2) && w2 > 0) w2 else w_calc * 100
          } else {
            return(FALSE)
          }
        }
        r2 <- w2
      }
      # Wait until UI WACC reflects calculated (avoid firing on stale APP_DEFAULTS).
      # In Lite, skip strict match when we already fell back to UI/default WACC.
      if (!isTRUE(lite) && abs(r2 - w_calc * 100) > 0.15) return(FALSE)
      if (isTRUE(lite) && is.finite(w_calc) && abs(r2 - w_calc * 100) > 0.15) {
        # Allow mismatch: updateNumericInput may lag under Lite; silent heal covers g.
        r2 <- w_calc * 100
      }
      sgr <- suppressWarnings(as.numeric(input$sgr)[1])
      if (!is.finite(sgr)) return(FALSE)
      claim <- as.character(input$dcf_claim %||% "fcff")[1]
      if (isTRUE(lite) && !is.null(des) && nzchar(as.character(des$claim %||% "")[1])) {
        claim <- as.character(des$claim)[1]
      }
      if (identical(claim, "fcfe")) {
        ke <- if (isTRUE(input$use_estimated_re) && !is.null(estimated_re()) &&
                  is.finite(as.numeric(estimated_re())[1])) {
          as.numeric(estimated_re())[1] * 100
        } else {
          suppressWarnings(as.numeric(input$wacc_re)[1])
        }
        if (!is.finite(ke) || ke <= 0 || sgr >= ke) {
          if (isTRUE(lite) && is.finite(ke) && ke > 0 && sgr >= ke) {
            # Silent path will heal g; treat as ready
          } else if (!is.finite(ke) || ke <= 0) {
            return(FALSE)
          } else if (!isTRUE(lite)) {
            return(FALSE)
          }
        }
      } else if (sgr >= r2) {
        if (!isTRUE(lite)) return(FALSE)
        # Lite: heal in .execute_dcf_calc; do not block readiness
      }
      return(isTRUE(.auto_calc_shares_ready()))
    }
    if (identical(prim, "ddm")) {
      d0 <- suppressWarnings(as.numeric(input[["mod_ddm-d0"]])[1])
      g <- suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1])
      ke <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
      mode <- as.character(input[["mod_ddm-ddm_mode"]] %||% "gordon")[1]
      if (!(is.finite(g) && is.finite(ke) && ke > 0 && g < ke)) return(FALSE)
      # Gordon／二階段需要正 D0；SPM 可用 EPS + D（D 可為 0）
      if (identical(mode, "spm")) {
        eps <- suppressWarnings(as.numeric(input[["mod_ddm-est_eps"]])[1])
        return(is.finite(eps))
      }
      return(is.finite(d0) && d0 > 0)
    }
    if (identical(prim, "pb")) {
      bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      if (is.null(bs) || !is.data.frame(bs) || nrow(bs) == 0L) return(FALSE)
      equity <- tryCatch(
        select_current_metric_any(bs, EQUITY_PATTERNS, "stock"),
        error = function(e) NA_real_
      )
      if (!is.finite(equity) || equity <= 0) return(FALSE)
      lo <- suppressWarnings(as.numeric(input[["mod_pb-pb_low"]])[1])
      mid <- suppressWarnings(as.numeric(input[["mod_pb-pb_mid"]])[1])
      hi <- suppressWarnings(as.numeric(input[["mod_pb-pb_high"]])[1])
      if (!(is.finite(lo) && is.finite(mid) && is.finite(hi) &&
            lo > 0 && mid > 0 && hi > 0)) return(FALSE)
      return(isTRUE(.auto_calc_shares_ready()))
    }
    if (identical(prim, "nav")) {
      bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      if (is.null(bs) || !is.data.frame(bs) || nrow(bs) == 0L) return(FALSE)
      navps <- suppressWarnings(as.numeric(input[["mod_nav-navps"]])[1])
      mid <- suppressWarnings(as.numeric(input[["mod_nav-nav_mid"]])[1])
      if (!(is.finite(navps) && navps > 0 && is.finite(mid) && mid > 0)) return(FALSE)
      return(isTRUE(.auto_calc_shares_ready()))
    }
    if (identical(prim, "multiples")) {
      # Do not rely on mod_rel-* inputs: updateNumericInput is async, so Lite
      # readiness must come from summary / statements (same sources as sync).
      sum_df <- tryCatch(summary_data(), error = function(e) NULL)
      d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
      d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      eps <- NA_real_
      if (exists("extract_summary_item", mode = "function")) {
        eps <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", as.character(
          extract_summary_item(sum_df, "EPS \\(TTM\\)|Trailing EPS|^EPS$", default = NA_character_)
        )))[1])
      }
      if (!is.finite(eps) && exists(".report_eps_bvps", mode = "function")) {
        eps <- tryCatch(.report_eps_bvps(sum_df, d_is = d_is, d_bs = d_bs)$eps, error = function(e) NA_real_)
      }
      if (is.finite(eps) && eps != 0) return(TRUE)
      # EV / P/S path: need share bridge + at least one statement metric
      if (!isTRUE(.auto_calc_shares_ready())) return(FALSE)
      d_cf <- tryCatch(d_cash_flow(), error = function(e) NULL)
      has_metric <- FALSE
      if (exists(".rel_latest_fcff", mode = "function")) {
        fcff <- tryCatch(.rel_latest_fcff(d_cf, d_is = d_is), error = function(e) NA_real_)
        has_metric <- is.finite(fcff) && fcff != 0
      }
      if (!isTRUE(has_metric) && exists(".rel_is_metric", mode = "function")) {
        rev <- tryCatch(.rel_is_metric(d_is, "Total Revenue|^Revenue$|Operating Revenue"), error = function(e) NA_real_)
        has_metric <- is.finite(rev) && rev > 0
      }
      return(isTRUE(has_metric))
    }
    if (identical(prim, "sotp")) {
      shares <- suppressWarnings(as.numeric(input[["mod_sotp-shares"]])[1])
      if (!(is.finite(shares) && shares > 0)) return(FALSE)
      return(isTRUE(.auto_calc_shares_ready()))
    }
    FALSE
  }

  .fire_auto_calc_primary <- function(prim) {
    prim <- as.character(prim %||% "")[1]
    if (identical(prim, "dcf")) {
      was_silent <- isTRUE(isolate(lite_dcf_silent()))
      if (isTRUE(isolate(lite_mode()))) {
        lite_dcf_silent(TRUE)
        on.exit(lite_dcf_silent(was_silent), add = TRUE)
      }
      # Auto-calc (Full or Lite): do not toast FX/ADR per-share refusal.
      # Manual Run DCF still notifies.
      tryCatch(.execute_dcf_calc(notify = FALSE), error = function(e) invisible(NULL))
    } else if (identical(prim, "ddm")) {
      auto_calc_ddm_pulse(isolate(auto_calc_ddm_pulse()) + 1L)
    } else if (identical(prim, "pb")) {
      auto_calc_pb_pulse(isolate(auto_calc_pb_pulse()) + 1L)
    } else if (identical(prim, "nav")) {
      auto_calc_nav_pulse(isolate(auto_calc_nav_pulse()) + 1L)
    } else if (identical(prim, "ri")) {
      auto_calc_ri_pulse(isolate(auto_calc_ri_pulse()) + 1L)
    } else if (identical(prim, "multiples")) {
      auto_calc_rel_pulse(isolate(auto_calc_rel_pulse()) + 1L)
    } else if (identical(prim, "sotp")) {
      auto_calc_sotp_pulse(isolate(auto_calc_sotp_pulse()) + 1L)
    }
    invisible(NULL)
  }

  observe({
    req(isTRUE(user_has_searched()))
    tk <- current_ticker()
    req(nzchar(tk))
    rec <- model_sidebar_rec()
    prim <- as.character(rec$primary %||% "")[1]
    sec <- as.character(rec$secondary %||% "")[1]
    ref <- as.character(rec$reference %||% "")[1]
    fv_prim <- nzchar(prim) && prim %in% c("dcf", "ddm", "pb", "ri", "nav")
    # TW 3363-like insufficient FV still keeps Multiples as Reference — do not require
    # a Fair Value primary before companion-running that Implied Price path in Lite.
    ref_only <- !isTRUE(fv_prim) && identical(ref, "multiples")
    req(isTRUE(fv_prim) || isTRUE(ref_only))

    # Lite: apply recommended scenario once, then proceed without waiting on hidden radios.
    # Do not block on .lite_scenario_matches_ui — CSS-hidden DCF radios in Lite may
    # lag or never echo updateRadioButtons, which left Smart Analysis on waiting copy.
    # Lite Smart Analysis must not hang on NA calculated WACC either; readiness falls
    # back to UI / App-default WACC and silent heal covers g ≥ discount.
    # Skip DCF scenario heal when there is no Fair Value primary (Reference-only path).
    if (isTRUE(lite_mode()) && isTRUE(fv_prim)) {
      des <- .lite_desired_scenario(rec)
      want_sig <- paste(
        tk,
        if (isTRUE(des$two_stage)) "two_stage" else "gordon",
        des$method %||% "",
        des$claim %||% "fcff",
        sep = "|"
      )
      if (!identical(lite_scenario_applied_sig(), want_sig)) {
        .apply_lite_recommended_scenario(rec)
        lite_scenario_applied_sig(want_sig)
        return()
      }
      # Re-sync WACC / g after UI lag or late calculated_wacc settle
      .lite_resync_discount_consistency(des)
    }

    if (isTRUE(fv_prim) && !isTRUE(.auto_calc_primary_ready(prim))) {
      # Surface why primary cannot auto-calc (mobile Lite never opens model tabs)
      if (identical(prim, "ddm")) {
        d0 <- suppressWarnings(as.numeric(input[["mod_ddm-d0"]])[1])
        mode <- as.character(input[["mod_ddm-ddm_mode"]] %||% "gordon")[1]
        need_d0 <- !identical(mode, "spm")
        if (isTRUE(need_d0) && !(is.finite(d0) && d0 > 0)) {
          fail_sig <- paste(tk, "ddm_no_d0", sep = "|")
          if (!identical(auto_calc_fail_sig(), fail_sig)) {
            auto_calc_fail_sig(fail_sig)
            showNotification(
              .ui_msg("notif_ddm_no_d0"),
              type = "warning",
              duration = 10,
              id = "ynow_ddm_no_d0"
            )
          }
        }
      }
      return()
    }
    keys <- if (isTRUE(fv_prim)) prim else character(0)
    # Lite secondary auto-calc: Fair Value engines + SOTP structural cross-check.
    # Multiples is a universal Reference (not a secondary) — always companion-run when ready.
    sec_auto_allowlist <- c("dcf", "ddm", "pb", "ri", "nav", "sotp")
    sec_in_auto_allowlist <- nzchar(sec) && sec %in% sec_auto_allowlist && !identical(sec, "multiples")
    sec_ready <- if (isTRUE(sec_in_auto_allowlist)) isTRUE(.auto_calc_primary_ready(sec)) else FALSE
    if (isTRUE(lite_mode()) && isTRUE(fv_prim) && isTRUE(sec_in_auto_allowlist) &&
        !identical(sec, prim) && isTRUE(sec_ready)) {
      keys <- c(prim, sec)
    }
    if (isTRUE(lite_mode()) && identical(ref, "multiples") &&
        isTRUE(.auto_calc_primary_ready("multiples"))) {
      keys <- unique(c(keys, "multiples"))
    }
    if (!length(keys)) return()
    mode <- as.character(input$dcf_mode %||% "gordon")[1]
    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    w_calc <- suppressWarnings(as.numeric(calculated_wacc())[1])
    sgr <- suppressWarnings(as.numeric(input$sgr)[1])
    sh_m <- tryCatch(.valuation_shares()$method, error = function(e) "none")
    fx_now <- suppressWarnings(as.numeric(fx_usd_twd())[1])
    # Wider signature: retry once params settle (WACC / SGR / shares / FX / scenario)
    sig <- paste(
      c(
        tk, keys,
        if (isTRUE(lite_mode())) "L" else "F",
        mode, claim,
        if (is.finite(w_calc)) round(w_calc * 100, 2) else "NA",
        if (is.finite(sgr)) round(sgr, 2) else "NA",
        sh_m %||% "none",
        if (is.finite(fx_now) && fx_now > 0) round(fx_now, 2) else "NA"
      ),
      collapse = "|"
    )
    if (identical(auto_calc_primary_sig(), sig)) return()

    auto_calc_primary_sig(sig)
    for (k in keys) .fire_auto_calc_primary(k)
  })

  # ==========================================
  # Smart Analysis（Lite）：主／副模型摘要與合理價圖
  # ==========================================
  output$smart_analysis_summary <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    if (!isTRUE(user_has_searched())) {
      return(tags$p(class = "ynow-smart-card-meta", ui_str("smart_waiting", loc)))
    }
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    if (is.null(rec)) {
      return(tags$p(class = "ynow-smart-card-meta", ui_str("smart_calc_pending", loc)))
    }
    prim <- as.character(rec$primary %||% "")[1]
    sec <- as.character(rec$secondary %||% "")[1]
    band <- tryCatch(primary_valuation_band(), error = function(e) NULL)
    sec_pt <- tryCatch(secondary_valuation_point(), error = function(e) NA_real_)
    cur <- tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    cur <- suppressWarnings(as.numeric(cur)[1])
    base <- if (!is.null(band)) suppressWarnings(as.numeric(band$base)[1]) else NA_real_
    bear <- if (!is.null(band)) suppressWarnings(as.numeric(band$bear)[1]) else NA_real_
    bull <- if (!is.null(band)) suppressWarnings(as.numeric(band$bull)[1]) else NA_real_
    # Guard near-zero Base (e.g. RI clean-surplus g≈ROE → FV≈0 float residue)
    # so MOS never becomes Inf / 1e16 %.
    mos_dec <- tryCatch(
      mos_vs_fair_value(base, cur),
      error = function(e) NA_real_
    )
    mos <- if (is.finite(mos_dec)) mos_dec * 100 else NA_real_
    fmt_px <- function(x) {
      if (!is.finite(x)) return("—")
      paste0(money_prefix(), format(round(x, 2), nsmall = 2, big.mark = ","))
    }
    fmt_pct <- function(x) {
      if (!is.finite(x)) return("—")
      paste0(sprintf("%+.1f", x), "%")
    }
    block <- tryCatch(lite_dcf_block(), error = function(e) "")
    ctype <- as.character(rec$company_type %||% "")[1]
    no_fv_primary <- !nzchar(prim) || identical(ctype, "insufficient")
    prim_meta <- if (isTRUE(no_fv_primary)) {
      ui_str("smart_no_fv_primary_meta", loc)
    } else if (is.finite(bear) && is.finite(bull)) {
      paste0("Bear ", fmt_px(bear), " · Bull ", fmt_px(bull))
    } else if (identical(block, "neg_terminal")) {
      ui_str("notif_dcf_neg_fcff_skip_tv", loc)
    } else if (!is.finite(base) && identical(prim, "ddm")) {
      d0_now <- suppressWarnings(as.numeric(input[["mod_ddm-d0"]])[1])
      if (!(is.finite(d0_now) && d0_now > 0)) {
        ui_str("smart_ddm_no_d0", loc)
      } else {
        ui_str("smart_calc_failed", loc)
      }
    } else if (!is.finite(base) && nzchar(prim) &&
               identical(as.character(auto_calc_fail_sig() %||% "")[1], paste(current_ticker(), "ddm_no_d0", sep = "|"))) {
      ui_str("smart_ddm_no_d0", loc)
    } else {
      ui_str("smart_calc_pending", loc)
    }
    prim_title <- if (isTRUE(no_fv_primary)) {
      ui_str("smart_no_fv_primary", loc)
    } else {
      .model_label(prim)
    }
    tk <- tryCatch(as.character(current_ticker() %||% "")[1], error = function(e) "")
    searched <- isTRUE(user_has_searched())
    tags$div(
      class = "ynow-smart-card-row",
      ynow_alertable(
        tags$div(
          class = "ynow-smart-card",
          `data-ynow-lock` = "output:smart_primary_fv",
          tags$p(class = "ynow-smart-card-kicker", ui_str("smart_primary_kicker", loc)),
          tags$p(class = "ynow-smart-card-title", prim_title),
          tags$p(class = "ynow-smart-card-value", fmt_px(base)),
          tags$p(class = "ynow-smart-card-meta", prim_meta)
        ),
        metric = "fair_value",
        ticker = tk,
        value = base,
        label = paste(ui_str("smart_primary_kicker", loc), prim_title),
        ready = searched && is.finite(base) && base > 0,
        default_kind = "below_fair",
        fair_value = base
      ),
      ynow_alertable(
        tags$div(
          class = "ynow-smart-card",
          tags$p(class = "ynow-smart-card-kicker", ui_str("smart_secondary_kicker", loc)),
          tags$p(
            class = "ynow-smart-card-title",
            if (nzchar(sec) && !identical(sec, "multiples")) .model_label(sec) else "—"
          ),
          tags$p(
            class = "ynow-smart-card-value",
            if (nzchar(sec) && !identical(sec, "multiples")) fmt_px(sec_pt) else "—"
          ),
          tags$p(
            class = "ynow-smart-card-meta",
            {
              ref <- as.character(rec$reference %||% "")[1]
              bits <- character(0)
              if (nzchar(sec) && !identical(sec, "multiples")) {
                bits <- c(bits, ui_str("composite_secondary_check", loc))
              }
              if (identical(ref, "multiples")) {
                bits <- c(bits, ui_str("smart_reference_kicker", loc))
              }
              paste(bits, collapse = " · ")
            }
          )
        ),
        metric = "fair_value_secondary",
        ticker = tk,
        value = sec_pt,
        label = paste(
          ui_str("smart_secondary_kicker", loc),
          if (nzchar(sec) && !identical(sec, "multiples")) .model_label(sec) else "—"
        ),
        ready = searched && nzchar(sec) && !identical(sec, "multiples") &&
          is.finite(sec_pt) && sec_pt > 0,
        default_kind = "below_fair",
        fair_value = sec_pt
      ),
      ynow_alertable(
        tags$div(
          class = "ynow-smart-card",
          tags$p(class = "ynow-smart-card-kicker", ui_str("smart_price_kicker", loc)),
          tags$p(class = "ynow-smart-card-title", " "),
          tags$p(class = "ynow-smart-card-value", fmt_px(cur)),
          tags$p(class = "ynow-smart-card-meta", " ")
        ),
        metric = "last_price",
        ticker = tk,
        value = cur,
        label = ui_str("smart_price_kicker", loc),
        ready = searched && is.finite(cur) && cur > 0,
        default_kind = "price_lte"
      ),
      ynow_alertable(
        tags$div(
          class = "ynow-smart-card",
          `data-ynow-lock` = "output:smart_mos",
          tags$p(class = "ynow-smart-card-kicker", ui_str("smart_mos_kicker", loc)),
          tags$p(class = "ynow-smart-card-title", " "),
          tags$p(class = "ynow-smart-card-value", fmt_pct(mos)),
          tags$p(class = "ynow-smart-card-meta", " ")
        ),
        metric = "mos_pct",
        ticker = tk,
        value = mos,
        label = ui_str("smart_mos_kicker", loc),
        ready = searched && is.finite(mos) && is.finite(base) && base > 0,
        default_kind = "below_fair",
        fair_value = base
      )
    )
  })

  output$smart_analysis_reason <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    if (!isTRUE(user_has_searched())) return(NULL)
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    if (is.null(rec)) return(NULL)
    reason <- if (exists(".pick_rec_reason", mode = "function")) {
      .pick_rec_reason(rec, loc)
    } else if (identical(loc, "en") && nzchar(as.character(rec$reason_en %||% "")[1])) {
      as.character(rec$reason_en)[1]
    } else {
      as.character(rec$reason %||% "")[1]
    }
    des <- tryCatch(.lite_desired_scenario(rec), error = function(e) NULL)
    scenario_bits <- character(0)
    if (!is.null(des)) {
      scenario_bits <- c(
        scenario_bits,
        if (isTRUE(des$two_stage)) {
          ui_str("smart_scenario_two_stage", loc)
        } else {
          ui_str("smart_scenario_gordon", loc)
        },
        paste0(
          ui_str("smart_scenario_sgr", loc),
          as.character(des$method %||% "")
        ),
        paste0(
          ui_str("smart_scenario_claim", loc),
          toupper(as.character(des$claim %||% "fcff"))
        )
      )
    }
    tags$div(
      style = "margin: 8px 0 18px 0; padding: 12px 14px; background: #f7f8fa; border: 1px solid #e5e7eb; border-radius: 6px;",
      if (length(scenario_bits)) tags$p(
        style = "margin: 0 0 8px 0; color: #333; line-height: 1.45;",
        tags$b(ui_str("smart_scenario_title", loc)),
        " ",
        paste(scenario_bits, collapse = " · ")
      ),
      if (nzchar(reason)) tagList(
        tags$b(ui_str("smart_reason_title", loc)),
        tags$p(style = "margin: 6px 0 0 0; color: #555; line-height: 1.45;", reason)
      )
    )
  })

  output$smart_analysis_chart <- plotly::renderPlotly({
    empty <- plotly::plotly_empty(type = "bar") %>%
      plotly::layout(
        title = list(text = "", font = list(size = 12)),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
    if (!isTRUE(user_has_searched())) return(empty)

    loc <- tryCatch(ui_locale(), error = function(e) "en")
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    band <- tryCatch(primary_valuation_band(), error = function(e) NULL)
    sec_pt <- tryCatch(secondary_valuation_point(), error = function(e) NA_real_)
    cur <- suppressWarnings(as.numeric(tryCatch(scraped_market_cap()$price, error = function(e) NA_real_))[1])
    prim <- as.character(rec$primary %||% "")[1]
    sec <- as.character(rec$secondary %||% "")[1]
    base <- if (!is.null(band)) suppressWarnings(as.numeric(band$base)[1]) else NA_real_
    bear <- if (!is.null(band)) suppressWarnings(as.numeric(band$bear)[1]) else NA_real_
    bull <- if (!is.null(band)) suppressWarnings(as.numeric(band$bull)[1]) else NA_real_

    labs <- c(
      ui_str("smart_price_kicker", loc),
      paste0(.model_label(prim), " Bear"),
      paste0(.model_label(prim), " Base"),
      paste0(.model_label(prim), " Bull")
    )
    vals <- c(cur, bear, base, bull)
    cols <- c("#333333", "#9aa0a6", "#0C5484", "#5b8def")
    if (nzchar(sec) && !identical(sec, "multiples") && is.finite(sec_pt)) {
      labs <- c(labs, paste0(.model_label(sec), " FV"))
      vals <- c(vals, sec_pt)
      cols <- c(cols, "#888888")
    }
    # Multiples as universal Reference overlay (not a Fair Value secondary bar)
    ref <- as.character(rec$reference %||% "")[1]
    if (identical(ref, "multiples")) {
      ref_pt <- tryCatch({
        if (!is.null(rel_results$any_implied_price)) {
          suppressWarnings(as.numeric(rel_results$any_implied_price())[1])
        } else if (!is.null(rel_results$pe_price)) {
          suppressWarnings(as.numeric(rel_results$pe_price())[1])
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_)
      if (is.finite(ref_pt)) {
        ref_lab <- if (!is.null(rel_results$chart_label)) {
          tryCatch(rel_results$chart_label(), error = function(e) .model_label("multiples"))
        } else {
          .model_label("multiples")
        }
        labs <- c(labs, paste0(ref_lab, " ", ui_str("smart_multiples_bar_suffix", loc)))
        vals <- c(vals, ref_pt)
        cols <- c(cols, "#17a2b8")
      }
    }
    ok <- is.finite(vals)
    if (!any(ok)) return(empty)
    labs <- labs[ok]
    vals <- vals[ok]
    cols <- cols[ok]
    y_title <- paste0("Price (", money_prefix(), ")")

    plotly::plot_ly(
      x = labs,
      y = vals,
      type = "bar",
      marker = list(color = cols),
      text = round(vals, 2),
      textposition = "outside",
      hovertemplate = "%{x}<br>%{y:.2f}<extra></extra>"
    ) %>%
      plotly::layout(
        margin = list(l = 48, r = 16, t = 24, b = 64),
        yaxis = list(title = y_title, zeroline = FALSE),
        xaxis = list(title = "", tickangle = -20),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  # ==========================================
  # 渲染估值結果與 InfoBox
  # ==========================================
  output$vtxt_dcf_results <- renderText({
    ev_val <- dcf_value_result()
    stock_val <- stock_price_estimate_val()
    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    
    if (length(ev_val) == 0 || is.na(ev_val)) {
      return(paste0("⚠️ ", ui_str("dcf_idle_hint", isolate(ui_locale()))))
    }
    
    msg <- if (identical(claim, "fcfe")) {
      glue::glue("股權現金流現值 (PV of FCFE)：${round(ev_val, 2)}")
    } else {
      glue::glue("企業總價值 (EV)：${round(ev_val, 2)}")
    }
    
    if (length(stock_val) > 0 && !is.na(stock_val)) {
      msg <- glue::glue("{msg}\n 最終每股合理價：{money_prefix()}{round(stock_val, 2)}")
    }
    return(msg)
  })
  
  output$ibx_stock_value_dcf <- renderInfoBox({
    raw <- if (is.null(stock_price_estimate_val())) {
      "N/A"
    } else {
      paste0(money_prefix(), round(stock_price_estimate_val(), 2))
    }
    infoBox(
      "每股估值（DCF）",
      ynow_model_result_num(raw),
      icon = icon("money-bill-wave"), color = "maroon", fill = TRUE
    )
  })
  
  output$ibx_enterprise_value_dcf <- renderInfoBox({
    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    title <- if (identical(claim, "fcfe")) "FCFE 現值（股權）" else "企業估值（DCF）"
    infoBox(title, 
            if(is.null(dcf_value_result())) "N/A" else format_dollar_abbr(dcf_value_result()), 
            icon = icon("building"), color = "purple", fill = TRUE) 
  })

  # Core explainability: Why this DCF Fair Value (audit snap bridge under KPI)
  output$dcf_value_why <- renderUI({
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    title <- tryCatch(
      ui_str("explain_dcf_why_title", loc),
      error = function(e) if (identical(loc, "en")) {
        "Why this Fair Value (DCF)"
      } else {
        "為何是這個合理價（DCF）"
      }
    )
    empty_msg <- tryCatch(
      ui_str("explain_dcf_why_empty", loc),
      error = function(e) if (identical(loc, "en")) {
        "Run Search / Calculate DCF to see the formula path, PV bridge, and Terminal Value drivers."
      } else {
        "請先搜尋／試算 DCF，即可看到公式路徑、現值橋接與終值驅動因子。"
      }
    )
    foot <- tryCatch(
      ui_str("explain_dcf_why_foot", loc),
      error = function(e) if (identical(loc, "en")) {
        "Explainability is a core surface: this panel mirrors the live DCF audit snapshot."
      } else {
        "可解釋性為核心表面：本區塊對應即時 DCF 稽核快照，非附註。"
      }
    )
    snap <- tryCatch(dcf_audit_snap(), error = function(e) NULL)
    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    mode <- as.character(input$dcf_mode %||% "gordon")[1]
    g_pct <- suppressWarnings(as.numeric(input$sgr)[1])
    rows <- if (exists("ynow_dcf_why_rows", mode = "function")) {
      ynow_dcf_why_rows(
        snap, claim = claim, mode = mode, g_pct = g_pct, locale = loc
      )
    } else {
      NULL
    }
    if (!exists("ynow_value_why_panel", mode = "function")) {
      return(tags$p(style = "color:#888;", empty_msg))
    }
    ynow_value_why_panel(
      title = title,
      rows = rows,
      empty_message = empty_msg,
      footnote = foot
    )
  })

  # Core explainability: Why this DDM Fair Value
  output$ddm_value_why <- renderUI({
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    empty_msg <- tryCatch(ui_str("explain_ddm_why_empty", loc), error = function(e) {
      "Run Search / Calculate DDM to see path, Ke, g, and dividend inputs."
    })
    if (!exists("ynow_value_why_panel", mode = "function") ||
        !exists("ynow_ddm_why_rows", mode = "function")) {
      return(tags$p(style = "color:#888;", empty_msg))
    }
    mode <- as.character(input[["mod_ddm-ddm_mode"]] %||% "gordon")[1]
    ke_pct <- tryCatch(central_ke() * 100, error = function(e) NA_real_)
    g_pct <- suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1])
    d0 <- suppressWarnings(as.numeric(input[["mod_ddm-d0"]])[1])
    eps <- suppressWarnings(as.numeric(input[["mod_ddm-spm_eps"]])[1])
    price <- tryCatch({
      if (!is.null(ddm_results$ddm_price)) ddm_results$ddm_price() else NA_real_
    }, error = function(e) NA_real_)
    rows <- if (is.finite(price)) {
      ynow_ddm_why_rows(
        mode = mode, ke_pct = ke_pct, g_pct = g_pct, d0 = d0, eps = eps,
        price = price, locale = loc
      )
    } else {
      NULL
    }
    ynow_value_why_panel(
      title = tryCatch(ui_str("explain_ddm_why_title", loc), error = function(e) "Why this Fair Value (DDM)"),
      rows = rows,
      empty_message = empty_msg,
      footnote = tryCatch(ui_str("explain_ddm_why_foot", loc), error = function(e) NULL)
    )
  })

  # Shared primary-workflow explainability builder (Smart Analysis + Model Dashboard)
  .ynow_build_primary_explain_ui <- function(loc) {
    if (!exists("ynow_smart_explain_ui", mode = "function")) return(NULL)
    fv_empty <- tryCatch(ui_str("explain_smart_fv_why_empty", loc), error = function(e) {
      "Search a ticker so Smart Analysis can show the primary Fair Value path and inputs."
    })
    gate_empty <- tryCatch(ui_str("explain_gate_why_empty", loc), error = function(e) {
      "Open Decision Checklist / complete Search to see mandatory gate rationale."
    })
    fv_rows <- NULL
    extra <- list()
    if (isTRUE(user_has_searched())) {
      rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
      prim <- as.character(rec$primary %||% "")[1]
      sec <- as.character(rec$secondary %||% "")[1]
      ref <- as.character(rec$reference %||% "")[1]
      if (identical(prim, "dcf") && exists("ynow_dcf_why_rows", mode = "function")) {
        snap <- tryCatch(dcf_audit_snap(), error = function(e) NULL)
        fv_rows <- ynow_dcf_why_rows(
          snap,
          claim = as.character(input$dcf_claim %||% "fcff")[1],
          mode = as.character(input$dcf_mode %||% "gordon")[1],
          g_pct = suppressWarnings(as.numeric(input$sgr)[1]),
          locale = loc
        )
      } else if (identical(prim, "ddm") && exists("ynow_ddm_why_rows", mode = "function")) {
        price <- tryCatch(ddm_results$ddm_price(), error = function(e) NA_real_)
        if (is.finite(price)) {
          fv_rows <- ynow_ddm_why_rows(
            mode = as.character(input[["mod_ddm-ddm_mode"]] %||% "gordon")[1],
            ke_pct = tryCatch(central_ke() * 100, error = function(e) NA_real_),
            g_pct = suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1]),
            d0 = suppressWarnings(as.numeric(input[["mod_ddm-d0"]])[1]),
            eps = suppressWarnings(as.numeric(input[["mod_ddm-spm_eps"]])[1]),
            price = price,
            locale = loc
          )
        }
      } else if (identical(prim, "ri") && exists("ynow_ri_why_rows", mode = "function")) {
        res <- tryCatch({
          if (!is.null(ri_results$ri_result)) ri_results$ri_result() else NULL
        }, error = function(e) NULL)
        if (!is.null(res) && identical(res$status, "success")) {
          roe0 <- tryCatch({
            if (!is.null(res$df) && "ROE" %in% names(res$df)) {
              suppressWarnings(as.numeric(res$df$ROE[[1]])[1]) * 100
            } else NA_real_
          }, error = function(e) NA_real_)
          fv_rows <- ynow_ri_why_rows(
            b0 = res$b0,
            ke_pct = suppressWarnings(as.numeric(res$ke)[1]) * 100,
            roe_pct = roe0,
            g_pct = suppressWarnings(as.numeric(res$g)[1]) * 100,
            pv_ri = res$pv_ri,
            tv = res$pv_terminal,
            price = res$intrinsic,
            locale = loc
          )
        }
      } else if (identical(prim, "pb") && exists("ynow_pb_why_rows", mode = "function")) {
        price <- tryCatch(pb_results$pb_price(), error = function(e) NA_real_)
        band <- tryCatch(pb_results$pb_band(), error = function(e) NULL)
        if (is.finite(price)) {
          fv_rows <- ynow_pb_why_rows(
            mode = as.character(input[["mod_pb-target_mode"]] %||% "justified")[1],
            book = if (!is.null(band)) band$basis_val else NA_real_,
            target_pb = suppressWarnings(as.numeric(input[["mod_pb-pb_mid"]])[1]),
            price = price,
            locale = loc
          )
        }
      } else if (identical(prim, "nav") && exists("ynow_nav_why_rows", mode = "function")) {
        price <- tryCatch(nav_results$nav_price(), error = function(e) NA_real_)
        band <- tryCatch(nav_results$nav_band(), error = function(e) NULL)
        if (is.finite(price)) {
          fv_rows <- ynow_nav_why_rows(
            navps = if (!is.null(band)) band$navps else NA_real_,
            multiple = suppressWarnings(as.numeric(input[["mod_nav-nav_mid"]])[1]),
            holdco_pct = suppressWarnings(as.numeric(input[["mod_nav-holdco_discount"]])[1]),
            price = price,
            locale = loc
          )
        }
      } else if (identical(prim, "sotp") && exists("ynow_sotp_why_rows", mode = "function")) {
        price <- tryCatch(sotp_results$sotp_price(), error = function(e) NA_real_)
        if (is.finite(price)) {
          fv_rows <- ynow_sotp_why_rows(implied_price = price, locale = loc)
        }
      }
      # Companion Reference / Meta panels when relevant
      if (identical(ref, "multiples") && exists("ynow_multiples_why_rows", mode = "function") &&
          exists("ynow_value_why_panel", mode = "function")) {
        px <- tryCatch(rel_results$any_implied_price(), error = function(e) NA_real_)
        mk <- tryCatch(rel_results$active_method_key(), error = function(e) "")
        if (is.finite(px)) {
          extra[[length(extra) + 1L]] <- ynow_value_why_panel(
            title = tryCatch(ui_str("explain_mult_why_title", loc), error = function(e) "Why this Implied Price (Multiples)"),
            rows = ynow_multiples_why_rows(method_key = mk, implied_price = px, locale = loc),
            empty_message = tryCatch(ui_str("explain_mult_why_empty", loc), error = function(e) ""),
            footnote = tryCatch(ui_str("explain_mult_why_foot", loc), error = function(e) NULL)
          )
        }
      }
      if ((identical(sec, "sotp") || identical(prim, "sotp")) &&
          !identical(prim, "sotp") &&
          exists("ynow_sotp_why_rows", mode = "function") &&
          exists("ynow_value_why_panel", mode = "function")) {
        px <- tryCatch(sotp_results$sotp_price(), error = function(e) NA_real_)
        if (is.finite(px)) {
          extra[[length(extra) + 1L]] <- ynow_value_why_panel(
            title = tryCatch(ui_str("explain_sotp_why_title", loc), error = function(e) "Why this SOTP value"),
            rows = ynow_sotp_why_rows(implied_price = px, locale = loc),
            empty_message = tryCatch(ui_str("explain_sotp_why_empty", loc), error = function(e) ""),
            footnote = tryCatch(ui_str("explain_sotp_why_foot", loc), error = function(e) NULL)
          )
        }
      }
    }
    gate_rows <- if (exists("ynow_decision_gate_why_rows", mode = "function")) {
      ynow_decision_gate_why_rows(
        tryCatch(institutional_sop_gate(), error = function(e) NULL),
        locale = loc
      )
    } else {
      NULL
    }
    ynow_smart_explain_ui(
      fv_title = tryCatch(ui_str("explain_smart_fv_why_title", loc), error = function(e) "Why this Fair Value (primary)"),
      fv_rows = fv_rows,
      fv_empty = fv_empty,
      fv_foot = tryCatch(ui_str("explain_smart_fv_why_foot", loc), error = function(e) NULL),
      gate_title = tryCatch(ui_str("explain_gate_why_title", loc), error = function(e) "Why this Decision Checklist gate"),
      gate_rows = gate_rows,
      gate_empty = gate_empty,
      gate_foot = tryCatch(ui_str("explain_gate_why_foot", loc), error = function(e) NULL),
      extra_panels = if (length(extra)) extra else NULL
    )
  }

  output$smart_value_why <- renderUI({
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .ynow_build_primary_explain_ui(loc)
  })

  output$dashboard_value_why <- renderUI({
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .ynow_build_primary_explain_ui(loc)
  })
  
  output$vtxt_dcf_setting_details <- renderUI({
    req(input$dcf_mode, input$years)
    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    
    if (identical(claim, "fcfe")) {
      ke_pct <- if (isTRUE(input$use_estimated_re) && !is.null(estimated_re()) &&
                    is.finite(as.numeric(estimated_re())[1])) {
        round(as.numeric(estimated_re())[1] * 100, 2)
      } else {
        suppressWarnings(as.numeric(input$wacc_re)[1])
      }
      disc_txt <- paste0(ke_pct, "%")
      HTML(glue::glue("<div style='padding: 15px; background: #fcfcfc; border: 1px solid #eee; font-size: 14px;'>
                  <b>評價模式：</b> {input$dcf_mode} <br/>
                  <b>採用現金流：</b> FCFE（Ke，直接股權） <br/>
                  <b>預測年數：</b> {input$years} 年 <br/>
                  <b>折現率 Ke：</b> {disc_txt}</div>"))
    } else {
      wacc_val <- if (isTRUE(input$dcf_mode == "gordon")) {
        paste0(input$wacc_gordon, "%")
      } else {
        paste0(input$wacc_stage1, "% / ", input$wacc_stage2, "%")
      }
      HTML(glue::glue("<div style='padding: 15px; background: #fcfcfc; border: 1px solid #eee; font-size: 14px;'>
                  <b>評價模式：</b> {input$dcf_mode} <br/>
                  <b>採用現金流：</b> FCFF（WACC，再橋接股權） <br/>
                  <b>預測年數：</b> {input$years} 年 <br/>
                  <b>折現率 WACC：</b> {wacc_val}</div>"))
    }
  })
  
  # ==========================================
  # 📊 9. 敏感度分析矩陣（即時 SGR／WACC；自動 DCF 或 DDM）
  # ==========================================
  .sensitivity_matrix_model <- reactive({
    rec <- tryCatch(model_sidebar_rec(), error = function(e) NULL)
    if (is.null(rec)) return("DCF")
    prim <- as.character(rec$primary %||% "")
    if (identical(prim, "ddm")) return("DDM")
    if (identical(prim, "dcf")) return("DCF")
    # 副模型／其他主模型：DDM 旗標且無 DCF 時用 DDM，否則 DCF
    if (isTRUE(rec$ddm) && !isTRUE(rec$dcf)) return("DDM")
    "DCF"
  })

  .build_dcf_sensitivity_matrix <- function(base_wacc, base_g) {
    df_fcf <- fcf_results$df_fcf()
    n_years <- as.numeric(input$years)
    if (is.null(df_fcf) || !is.data.frame(df_fcf) || nrow(df_fcf) != n_years) {
      return(NULL)
    }
    future_fcfs <- extract_fcff_series(df_fcf)
    fcf_n <- tail(future_fcfs, 1)

    sb_bridge <- .shared_bridge()
    latest_cash <- sb_bridge$cash
    total_debt <- sb_bridge$debt
    if (!is.finite(latest_cash) || latest_cash < 0) latest_cash <- 0
    if (!is.finite(total_debt) || total_debt < 0) total_debt <- 0

    shares <- sb_bridge$shares
    if (!is.finite(shares) || shares <= 0) {
      shares <- tryCatch({
        sh <- .valuation_shares()
        if (!is.null(sh) && is.finite(sh$shares) && sh$shares > 0) sh$shares else NA_real_
      }, error = function(e) NA_real_)
    }
    if (is.na(shares) || shares <= 0) return(NULL)

    wacc_range <- seq(base_wacc + 2, base_wacc - 2, length.out = 5)
    g_range <- seq(base_g - 1, base_g + 1, length.out = 5)
    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    # Row labels use WACC／Ke; table corner column stays "Rate (%)" for FCFF.
    rate_lab <- if (identical(claim, "fcfe")) "Ke " else "WACC "

    sens_matrix <- matrix(
      NA, nrow = 5, ncol = 5,
      dimnames = list(
        paste0(rate_lab, round(wacc_range, 1), "%"),
        paste0("g (SGR) ", round(g_range, 1), "%")
      )
    )

    base_wacc_seq <- if (identical(input$dcf_mode, "gordon")) {
      rep(base_wacc / 100, n_years)
    } else {
      s1 <- as.numeric(input$yr_stage1)
      # 敏感度以「目前 WACC」為軸心：Stage1／Stage2 皆相對目前 WACC 平移
      c(rep(base_wacc / 100, min(s1, n_years)), rep(base_wacc / 100, max(n_years - s1, 0)))
    }

    for (i in 1:5) {
      for (j in 1:5) {
        w_val <- wacc_range[i] / 100
        g_val <- g_range[j] / 100
        w_delta <- w_val - (base_wacc / 100)
        scenario_w_seq <- base_wacc_seq + w_delta
        terminal_wacc <- tail(scenario_w_seq, 1)

        if (!is.na(terminal_wacc) && !is.na(g_val) && terminal_wacc > g_val) {
          if (identical(claim, "fcfe")) {
            rd <- suppressWarnings(as.numeric(input$wacc_rd)[1]) / 100
            if (!is.finite(rd) || rd < 0) rd <- 0
            tax <- suppressWarnings(as.numeric(input$wacc_tax)[1]) / 100
            if (!is.finite(tax)) tax <- APP_DEFAULTS$wacc_tax / 100
            iat <- after_tax_interest(total_debt, rd, tax)
            cfs <- fcff_to_fcfe(future_fcfs, interest_after_tax = iat, debt0 = total_debt, g_path = g_val)
            if (any(!is.finite(cfs))) next
            dfs <- cumprod(rep(1 + w_val, n_years))
            pv <- sum(cfs / dfs)
            tv <- tail(cfs, 1) * (1 + g_val) / (w_val - g_val)
            equity_val <- pv + tv / dfs[n_years]
          } else {
            discount_factors <- cumprod(1 + scenario_w_seq)
            pv_fcf <- sum(future_fcfs / discount_factors)
            tv <- (fcf_n * (1 + g_val)) / (terminal_wacc - g_val)
            pv_tv <- tv / discount_factors[n_years]
            ev <- pv_fcf + pv_tv
            equity_val <- ev + latest_cash - total_debt
          }
          if (!is.na(shares) && shares > 0) {
            sens_matrix[i, j] <- .dcf_per_share(
              equity_val,
              list(shares = shares, method = tryCatch(.valuation_shares()$method, error = function(e) "none"))
            )
          }
        }
      }
    }
    list(matrix = sens_matrix, center = sens_matrix[3, 3], axes = list(wacc = base_wacc, g = base_g))
  }

  .build_ddm_sensitivity_matrix <- function(base_ke, base_g) {
    d0 <- tryCatch({
      if (!is.null(input[["mod_ddm-d0"]]) && is.finite(as.numeric(input[["mod_ddm-d0"]]))) {
        as.numeric(input[["mod_ddm-d0"]])
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
    mode <- as.character(input[["mod_ddm-ddm_mode"]] %||% "gordon")[1]
    if (identical(mode, "spm")) {
      if (is.na(d0) || d0 < 0) d0 <- 0
      eps <- suppressWarnings(as.numeric(input[["mod_ddm-est_eps"]])[1])
      if (!is.finite(eps)) return(NULL)
    } else if (is.na(d0) || d0 <= 0) {
      return(NULL)
    }

    ke_range <- seq(base_ke + 2, base_ke - 2, length.out = 5)
    g_range <- seq(base_g - 1, base_g + 1, length.out = 5)
    sens_matrix <- matrix(
      NA, nrow = 5, ncol = 5,
      dimnames = list(
        paste0("Ke ", round(ke_range, 1), "%"),
        paste0("g (SGR) ", round(g_range, 1), "%")
      )
    )
    for (i in 1:5) {
      for (j in 1:5) {
        ke_val <- ke_range[i] / 100
        g_val <- g_range[j] / 100
        if (is.na(ke_val) || is.na(g_val)) next
        if (identical(mode, "spm")) {
          eps <- suppressWarnings(as.numeric(input[["mod_ddm-est_eps"]])[1])
          if (is.finite(eps) && ke_val > 0) {
            sens_matrix[i, j] <- .ddm_formula_spm(eps = eps, d = d0, g = g_val, ke = ke_val)
          }
        } else if (ke_val > g_val) {
          if (identical(mode, "two_stage")) {
            g1 <- suppressWarnings(as.numeric(input[["mod_ddm-g_stage1"]])[1]) / 100
            n1 <- suppressWarnings(as.integer(input[["mod_ddm-yr_stage1"]])[1])
            if (!is.finite(g1)) g1 <- g_val
            if (!is.finite(n1) || n1 < 1L) n1 <- 5L
            sens_matrix[i, j] <- .ddm_formula_two_stage(
              d0 = d0, g1 = g1, n = n1, g2 = g_val, ke = ke_val
            )
          } else {
            d1 <- d0 * (1 + g_val)
            sens_matrix[i, j] <- d1 / (ke_val - g_val)
          }
        }
      }
    }
    list(matrix = sens_matrix, center = sens_matrix[3, 3], axes = list(ke = base_ke, g = base_g))
  }

  sensitivity_state <- reactive({
    req(input$calc)
    matrix_model <- .sensitivity_matrix_model()

    base_g <- if (!is.null(input$sgr) && is.finite(as.numeric(input$sgr))) {
      as.numeric(input$sgr)
    } else {
      APP_DEFAULTS$sgr
    }

    if (identical(matrix_model, "DDM")) {
      base_ke <- tryCatch({
        ke_ui <- input[["mod_ddm-ke"]]
        if (!is.null(ke_ui) && is.finite(as.numeric(ke_ui))) {
          as.numeric(ke_ui)
        } else {
          central_ke() * 100
        }
      }, error = function(e) central_ke() * 100)
      if (is.null(base_ke) || !is.finite(base_ke)) base_ke <- 10
      built <- .build_ddm_sensitivity_matrix(base_ke, base_g)
      return(list(
        model = "DDM",
        base_g = base_g,
        base_disc = base_ke,
        disc_label = "Ke",
        built = built
      ))
    }

    # DCF：FCFF 用 WACC；FCFE 用 Ke
    claim <- as.character(input$dcf_claim %||% "fcff")[1]
    if (identical(claim, "fcfe")) {
      base_ke <- if (isTRUE(input$use_estimated_re) && !is.null(estimated_re()) &&
                     is.finite(as.numeric(estimated_re())[1])) {
        as.numeric(estimated_re())[1] * 100
      } else {
        suppressWarnings(as.numeric(input$wacc_re)[1])
      }
      if (is.null(base_ke) || !is.finite(base_ke)) base_ke <- APP_DEFAULTS$wacc_re
      req(fcf_results$df_fcf())
      built <- .build_dcf_sensitivity_matrix(base_ke, base_g)
      return(list(
        model = "DCF",
        base_g = base_g,
        base_disc = base_ke,
        disc_label = "Ke",
        built = built
      ))
    }

    # DCF：與 Dashboard／基礎設定同一套「目前 WACC」
    base_wacc <- tryCatch(.current_wacc_pct(), error = function(e) NA_real_)
    if (is.null(base_wacc) || !is.finite(base_wacc)) base_wacc <- APP_DEFAULTS$wacc_gordon
    req(fcf_results$df_fcf())
    built <- .build_dcf_sensitivity_matrix(base_wacc, base_g)
    list(
      model = "DCF",
      base_g = base_g,
      base_disc = base_wacc,
      disc_label = "WACC",
      built = built
    )
  })

  output$dcf_sensitivity_table <- renderTable({
    st <- sensitivity_state()
    req(!is.null(st$built), !is.null(st$built$matrix))
    sens_matrix <- st$built$matrix
    out_df <- cbind(Rate = rownames(sens_matrix), as.data.frame(sens_matrix, check.names = FALSE))
    names(out_df)[1] <- if (identical(st$disc_label, "Ke")) "Ke (%)" else "Rate (%)"
    out_df
  }, digits = 2, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c",
     width = "100%", na = "無效 (折現率≤g)")

  output$sensitivity_analysis_panel <- renderUI({
    st <- tryCatch(sensitivity_state(), error = function(e) NULL)
    if (is.null(st) || is.null(st$built)) {
      return(tags$div(
        style = "background:#fff8f0; border:1px solid #f0ad4e; border-radius:6px; padding:12px; font-size:13px; color:#666;",
        "請先完成基礎設定參數並執行估值計算後，即可顯示敏感度解讀。"
      ))
    }

    center_val <- st$built$center
    curr_price <- tryCatch({
      p <- scraped_market_cap()$price
      if (!is.null(p) && is.finite(as.numeric(p))) as.numeric(p) else NA_real_
    }, error = function(e) NA_real_)
    fair_val <- tryCatch({
      if (identical(st$model, "DDM")) {
        if (!is.null(ddm_results$ddm_price)) ddm_results$ddm_price() else NA_real_
      } else {
        stock_price_estimate_val()
      }
    }, error = function(e) NA_real_)

    fmt <- function(x) {
      if (is.null(x) || length(x) < 1 || !is.finite(as.numeric(x)[1])) return("N/A")
      sprintf("%.2f", as.numeric(x)[1])
    }

    vs_price <- if (is.finite(center_val) && is.finite(curr_price) && curr_price > 0) {
      pct <- (center_val - curr_price) / curr_price * 100
      sprintf("中心格內在價值 %s，相對現價 %s 約 %+.1f%%。", fmt(center_val), fmt(curr_price), pct)
    } else if (is.finite(center_val)) {
      sprintf("中心格內在價值約 %s；現價資料不足，暫無法比較。", fmt(center_val))
    } else {
      "中心格組合無效（折現率需大於 g），請調降 SGR 或提高折現率後重算。"
    }

    vs_fair <- if (is.finite(center_val) && is.finite(as.numeric(fair_val)[1])) {
      sprintf("與目前 %s 合理價 %s 對照：差異約 %s。",
              st$model, fmt(fair_val),
              sprintf("%+.2f", center_val - as.numeric(fair_val)[1]))
    } else {
      paste0("合理價尚未就緒；矩陣以目前 ", st$disc_label, "／SGR 為軸心展開。")
    }

    tags$div(
      style = "background:#f5f5f5; border-left:4px solid #222222; border-radius:6px; padding:14px; font-size:13px; line-height:1.55; color:#333; margin-top:12px;",
      tags$h5(style = "margin-top:0; color:#222222; font-weight:700;", icon("lightbulb"), " 簡要分析"),
      tags$p(
        tags$b("目前軸心："),
        sprintf("%s = %s%%，SGR (g) = %s%%（與基礎設定／Dashboard 同步）",
                st$disc_label, fmt(st$base_disc), fmt(st$base_g))
      ),
      tags$p(tags$b("矩陣解讀："), vs_price),
      tags$p(vs_fair),
      tags$p(
        style = "margin-bottom:0; color:#555;",
        tags$b("適用提醒："),
        "本矩陣適用絕對估值情境（DCF／DDM）；觀察 WACC（或 Ke）與 g 鄰近組合對每股內在價值的敏感度。"
      )
    )
  })

  output$txt_dcf_investment_checks <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    snap <- tryCatch(dcf_audit_snap(), error = function(e) NULL)
    if (is.null(snap) || !is.finite(suppressWarnings(as.numeric(snap$price)[1]))) {
      return(tags$p(class = "help-block", ui_str("dcf_checks_need_run", loc)))
    }
    pct <- function(x) {
      x <- suppressWarnings(as.numeric(x)[1])
      if (!is.finite(x)) return("—")
      paste0(format(round(x * 100, 2), nsmall = 2), "%")
    }
    pxf <- function(x) {
      x <- suppressWarnings(as.numeric(x)[1])
      if (!is.finite(x)) return("—")
      format(round(x, 2), nsmall = 2)
    }
    tv <- tv_ev_share(snap$pv_explicit, snap$pv_tv)
    mkt <- tryCatch(suppressWarnings(as.numeric(scraped_market_cap()$price)[1]), error = function(e) NA_real_)
    rev <- dcf_implied_terminal_g(
      snap$pv_explicit, snap$last_fcf, snap$discount, snap$df_terminal,
      price = mkt, shares = snap$shares, cash = snap$cash, debt = snap$debt,
      equity_claim = isTRUE(snap$equity_claim)
    )
    ev_for_gordon <- if (isTRUE(snap$equity_claim)) {
      if (is.finite(mkt) && is.finite(snap$shares)) mkt * snap$shares else NA_real_
    } else if (is.finite(mkt) && is.finite(snap$shares)) {
      mkt * snap$shares - (if (is.finite(snap$cash)) snap$cash else 0) + (if (is.finite(snap$debt)) snap$debt else 0)
    } else {
      NA_real_
    }
    gordon <- single_stage_implied_g(snap$fcf1, ev_for_gordon, snap$discount)
    flat <- is.finite(snap$nopat_y1) && is.finite(snap$nopat_yn) && abs(snap$nopat_y1 - snap$nopat_yn) < 1e-6
    ddm_px <- tryCatch(.ddm_price_at(0, 0), error = function(e) NA_real_)
    cross <- dcf_ddm_crosscheck(snap$price, ddm_px)
    ke_up <- tryCatch(.ddm_price_at(1, 0), error = function(e) NA_real_)
    ke_dn <- tryCatch(.ddm_price_at(-1, 0), error = function(e) NA_real_)
    g_up <- tryCatch(.ddm_price_at(0, 0.5), error = function(e) NA_real_)
    g_dn <- tryCatch(.ddm_price_at(0, -0.5), error = function(e) NA_real_)
    band <- tryCatch(dcf_scenario_band(), error = function(e) NULL)
    row <- function(title, body) {
      tags$div(
        style = "margin:0 0 8px 0;",
        tags$div(style = "font-weight:700;", title),
        tags$div(body)
      )
    }
    tags$div(
      style = "background:#f4f8fb; border:1px solid #d6e2ea; padding:12px 14px; margin:12px 0; font-size:13px; line-height:1.55; color:#333;",
      tags$div(style = "font-weight:700; margin-bottom:8px;", ui_str("dcf_checks_title", loc)),
      row(
        ui_str("dcf_checks_tv", loc),
        tagList(
          pct(tv$tv_share),
          " · ",
          if (isTRUE(tv$heavy)) ui_str("dcf_checks_tv_heavy", loc) else ui_str("dcf_checks_tv_ok", loc)
        )
      ),
      row(
        ui_str("dcf_checks_margin", loc),
        tagList(
          pct(snap$nopat_y1), " → ", pct(snap$nopat_yn), ". ",
          if (flat) ui_str("dcf_checks_margin_flat", loc) else ui_str("dcf_checks_margin_fade", loc)
        )
      ),
      row(
        ui_str("dcf_checks_gk", loc),
        tagList(
          "Ke −1 pp ", pxf(ke_dn), " · base ", pxf(ddm_px), " · Ke +1 pp ", pxf(ke_up),
          ". g −0.5 pp ", pxf(g_dn), " · g +0.5 pp ", pxf(g_up), "."
        )
      ),
      row(
        ui_str("dcf_checks_reverse", loc),
        tagList(
          ui_str("dcf_checks_reverse_g", loc), ": ",
          if (isTRUE(rev$ok)) pct(rev$g) else ui_str("dcf_checks_reverse_na", loc),
          ". ",
          ui_str("dcf_checks_reverse_gordon", loc), ": ",
          if (isTRUE(gordon$ok)) pct(gordon$g) else "—"
        )
      ),
      row(
        ui_str("dcf_checks_scenario", loc),
        if (is.null(band)) "—" else tagList(
          "Bear ", pxf(band$bear), " · Base ", pxf(band$base), " · Bull ", pxf(band$bull), "."
        )
      ),
      row(
        ui_str("dcf_checks_cross", loc),
        tagList(
          "DCF ", pxf(cross$dcf), " · DDM ", pxf(cross$ddm),
          if (isTRUE(cross$ok)) paste0(" · ", pct(cross$gap), ". ") else ". ",
          if (!isTRUE(cross$ok)) {
            ui_str("dcf_checks_cross_na", loc)
          } else if (isTRUE(cross$wide)) {
            ui_str("dcf_checks_cross_wide", loc)
          } else {
            ui_str("dcf_checks_cross_close", loc)
          }
        )
      )
    )
  })

  output$ddm_sensitivity_table <- renderTable({
    ke <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
    g <- suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1])
    if (!is.finite(ke)) ke <- APP_DEFAULTS$ddm_ke
    if (!is.finite(g)) g <- APP_DEFAULTS$ddm_g
    built <- .build_ddm_sensitivity_matrix(ke, g)
    req(!is.null(built), !is.null(built$matrix))
    sens_matrix <- built$matrix
    out_df <- cbind(Rate = rownames(sens_matrix), as.data.frame(sens_matrix, check.names = FALSE))
    names(out_df)[1] <- "Ke_Rate"
    out_df
  }, digits = 2, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c",
     width = "100%", na = "無效 (折現率≤g)")

  output$ddm_sensitivity_analysis_panel <- renderUI({
    ke <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
    g <- suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1])
    if (!is.finite(ke)) ke <- APP_DEFAULTS$ddm_ke
    if (!is.finite(g)) g <- APP_DEFAULTS$ddm_g
    built <- tryCatch(.build_ddm_sensitivity_matrix(ke, g), error = function(e) NULL)
    if (is.null(built) || is.null(built$matrix)) {
      return(tags$div(
        style = "background:#fff8f0; border:1px solid #f0ad4e; border-radius:6px; padding:12px; font-size:13px; color:#666;",
        "請先確認 D0 > 0 且 Ke > g 後，即可顯示敏感度解讀。"
      ))
    }

    center_val <- built$center
    curr_price <- tryCatch({
      p <- scraped_market_cap()$price
      if (!is.null(p) && is.finite(as.numeric(p))) as.numeric(p) else NA_real_
    }, error = function(e) NA_real_)

    fmt <- function(x) {
      if (is.null(x) || length(x) < 1 || !is.finite(as.numeric(x)[1])) return("N/A")
      sprintf("%.2f", as.numeric(x)[1])
    }

    vs_price <- if (is.finite(center_val) && is.finite(curr_price) && curr_price > 0) {
      pct <- (center_val - curr_price) / curr_price * 100
      sprintf("中心格內在價值 %s，相對現價 %s 約 %+.1f%%。", fmt(center_val), fmt(curr_price), pct)
    } else if (is.finite(center_val)) {
      sprintf("中心格內在價值約 %s；現價資料不足，暫無法比較。", fmt(center_val))
    } else {
      "中心格組合無效（Ke 需大於 g），請調降股利成長率或提高 Ke。"
    }

    vs_fair <- "矩陣中心格即目前 Ke／股利 g 下的公式價值（不必先按試算）。"

    tags$div(
      style = "background:#f5f5f5; border-left:4px solid #222222; border-radius:6px; padding:14px; font-size:13px; line-height:1.55; color:#333; margin-top:12px;",
      tags$h5(style = "margin-top:0; color:#222222; font-weight:700;", icon("lightbulb"), " 簡要分析"),
      tags$p(
        tags$b("目前軸心："),
        sprintf("Ke = %s%%，股利 g = %s%%", fmt(ke), fmt(g))
      ),
      tags$p(tags$b("矩陣解讀："), vs_price),
      tags$p(vs_fair),
      tags$p(
        style = "margin-bottom:0; color:#555;",
        tags$b("適用提醒："),
        "本矩陣適用 DDM 絕對估值；觀察 Ke 與股利永續 g 鄰近組合對每股內在價值的敏感度。"
      )
    )
  })

  # ==========================================
  # 🛡️ 10. 數據缺漏檢查 UI 
  # ==========================================
  output$ui_data_validation <- renderUI({
    if (is.null(d_balance_sheet()) || is.null(d_cash_flow())) return(NULL)
    
    scraped_fcf <- select_current_metric(d_cash_flow(), "Free Cash Flow", "flow")
    
    val_cash_raw <- select_current_metric(d_balance_sheet(), "Cash, Cash Equivalents & Short Term Investments|Cash And Cash Equivalents", "stock")
    val_cash <- val_cash_raw
    
    val_debt <- select_current_metric(d_balance_sheet(), "Total Debt", "stock")
    scraped_debt <- val_debt
    
    check_list <- list(
      "Free Cash Flow (FCF)" = scraped_fcf,
      "Cash Position" = val_cash,
      "Total Debt" = scraped_debt
    )
    

    alert_box <- ui_missing_data_alert(
      check_list = check_list,
      fallback_msg = "無法從財報擷取上述數值。請在下方手動輸入以確保企業估值 (DCF) 計算準確。"
    )
    
    if (!is.null(alert_box)) {
      box(title = "核心評價資料缺漏提醒", status = "danger", width = 12, solidHeader = TRUE,
          alert_box, 
          fluidRow(
            if(is.na(scraped_fcf)) column(4, numericInput("manual_fcf", "手動 FCF:", value = NA)) else NULL
          ),
          if (is.na(val_cash) || is.na(scraped_debt)) {
            tags$p(
              class = "help-block",
              "Cash 與 Total Debt 請到 Model Dashboard → Cash、Debt、股數與營收（各模型共用）填寫，DCF、WACC、倍數與 SOTP 會一起套用。"
            )
          } else {
            NULL
          }
      )
    } else {
      NULL
    }
  })
  
  # ==========================================
  # 🧪 Backtest Zone v12：PIT 多模型重建 + Alpha／MOS 驗證
  # ==========================================
  bt_param_notes_txt <- reactiveVal("請先搜尋股票並載入財報，系統會依公司自動推導參數。")
  bt_result <- reactiveVal(NULL)
  bt_validation <- reactiveVal(NULL)
  bt_run_msg <- reactiveVal("")
  bt_applying_params <- reactiveVal(FALSE)
  bt_fv_visible <- reactiveVal(FALSE)
  bt_hfv_fv <- reactiveVal(NULL)
  bt_freq_applying <- reactiveVal(FALSE)

  .bt_normalize_rebal_freq <- function(x, default = "quarterly") {
    if (exists(".normalize_rebal_freq", mode = "function")) {
      return(.normalize_rebal_freq(x, default = default))
    }
    x <- tolower(trimws(as.character(x %||% default)[1]))
    if (x %in% c("monthly", "每月")) return("monthly")
    if (x %in% c("yearly", "每年")) return("yearly")
    "quarterly"
  }

  .bt_rebal_freq_label <- function(freq, loc = NULL) {
    loc <- loc %||% tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
    switch(
      .bt_normalize_rebal_freq(freq),
      monthly = ui_str("bt_freq_monthly", loc),
      yearly = ui_str("bt_freq_yearly", loc),
      ui_str("bt_freq_quarterly", loc)
    )
  }

  .bt_selected_rebal_freq <- reactive({
    .bt_normalize_rebal_freq(input$bt_fv_analysis_freq %||% "quarterly")
  })

  .bt_price_dates_for_freq <- reactive({
    dates <- NULL
    base <- tryCatch(bt_hfv_base(), error = function(e) NULL)
    if (!is.null(base) && is.data.frame(base) && "Date" %in% names(base)) {
      dates <- base$Date
    }
    if ((is.null(dates) || length(dates) < 40L)) {
      cached <- tryCatch(hist_stock_data(), error = function(e) NULL)
      if (!is.null(cached) && is.data.frame(cached) && "Date" %in% names(cached)) {
        dates <- cached$Date
      }
    }
    res <- bt_result()
    if ((is.null(dates) || length(dates) < 40L) && !is.null(res) &&
        !is.null(res$equity_df) && "Date" %in% names(res$equity_df)) {
      dates <- res$equity_df$Date
    }
    dates
  })

  .bt_supported_analysis_freqs <- reactive({
    px_dates <- .bt_price_dates_for_freq()
    vd_dates <- NULL
    res <- bt_result()
    if (!is.null(res) && !is.null(res$valuation_df) && "Date" %in% names(res$valuation_df)) {
      vd_dates <- res$valuation_df$Date
    } else {
      fv <- bt_hfv_fv()
      if (!is.null(fv) && !is.null(fv$valuation_df) && "Date" %in% names(fv$valuation_df)) {
        vd_dates <- fv$valuation_df$Date
      }
    }
    if (exists("supported_analysis_freqs", mode = "function")) {
      supported_analysis_freqs(price_dates = px_dates, valuation_dates = vd_dates)
    } else if (exists("detect_supported_rebal_freqs", mode = "function") &&
               !is.null(px_dates) && length(px_dates) > 0) {
      detect_supported_rebal_freqs(px_dates)
    } else {
      "quarterly"
    }
  })

  output$bt_fv_analysis_freq_ui <- renderUI({
    loc <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
    freqs <- .bt_supported_analysis_freqs()
    choices <- c()
    if ("monthly" %in% freqs) {
      choices <- c(choices, setNames("monthly", ui_str("bt_freq_monthly", loc)))
    }
    if ("quarterly" %in% freqs) {
      choices <- c(choices, setNames("quarterly", ui_str("bt_freq_quarterly", loc)))
    }
    if ("yearly" %in% freqs) {
      choices <- c(choices, setNames("yearly", ui_str("bt_freq_yearly", loc)))
    }
    if (length(choices) < 1L) {
      return(tags$p(
        class = "ynow-hfv-page-controls__hint",
        ui_str("bt_freq_insufficient", loc)
      ))
    }
    cur <- isolate(input$bt_fv_analysis_freq)
    if (is.null(cur) || !(as.character(cur)[1] %in% unname(choices))) {
      pref <- as.character(APP_DEFAULTS$bt_fv_analysis_freq %||% "quarterly")[1]
      cur <- if (pref %in% unname(choices)) pref else if ("quarterly" %in% unname(choices)) "quarterly" else unname(choices)[[1]]
    }
    # Compact page-level control (under How-to-read); no long hint block
    radioButtons(
      "bt_fv_analysis_freq",
      ui_str("hfv_analysis_freq_label", loc),
      inline = TRUE,
      choices = choices,
      selected = cur
    )
  })

  .bt_fv_model_specs <- function() {
    # Key order matches YNOW_FV_ENGINE_ORDER (sidebar L→R).
    list(
      nav = list(col = "FV_NAV", label = "NAV", color = "#d81b60"),
      pb  = list(col = "FV_PB",  label = "Justified P/B", color = "#e67e22"),
      dcf = list(col = "FV_DCF", label = "DCF", color = "#c0392b"),
      ddm = list(col = "FV_DDM", label = "DDM", color = "#8e44ad"),
      ri  = list(col = "FV_RI",  label = "RI",  color = "#16a085")
    )
  }

  .bt_raw_fv_models <- reactive({
    # Chart overlay: multi-select (sidebar-aligned order)
    sel <- input$bt_fv_models
    if (is.null(sel) || length(sel) < 1) return(character(0))
    ynow_order_fv_engines(sel)
  })

  .bt_replay_fv_model <- reactive({
    # Replay / strategy FV: single-select only
    sel <- input$bt_fv_replay_model
    if (is.null(sel) || length(sel) < 1) return(character(0))
    m <- tolower(trimws(as.character(sel)[1]))
    if (!nzchar(m) || identical(m, "none")) return(character(0))
    ynow_order_fv_engines(m)
  })

  .bt_selected_fv_models <- reactive({
    # Strategy fair_value / MOS / HFV odds & magnitude: replay model only
    .bt_replay_fv_model()
  })

  bt_hfv_base <- reactive({
    tk <- current_ticker()
    if (is.null(tk) || !nzchar(as.character(tk)[1])) return(NULL)
    tryCatch(
      fetch_hfv_price_frame(tk, bench_ticker = active_bench_ticker(), years = 5),
      error = function(e) NULL
    )
  })

  observeEvent(current_ticker(), {
    bt_fv_visible(FALSE)
    bt_hfv_fv(NULL)
    bt_result(NULL)
    bt_validation(NULL)
    bt_run_msg("")
    was_applying <- isTRUE(bt_applying_params())
    bt_applying_params(TRUE)
    updateCheckboxGroupInput(session, "bt_fv_models", selected = character(0))
    updateRadioButtons(session, "bt_fv_replay_model", selected = "dcf")
    if (!was_applying) bt_applying_params(FALSE)
  }, ignoreInit = TRUE)

  bt_current_mos <- reactive({
    cur <- tryCatch(scraped_market_cap()$price, error = function(e) NA_real_)
    tgt <- tryCatch(stock_price_estimate_val(), error = function(e) NA_real_)
    if (is.null(tgt) || length(tgt) < 1) tgt <- NA_real_
    cur <- suppressWarnings(as.numeric(cur)[1])
    tgt <- suppressWarnings(as.numeric(tgt)[1])
    if (is.na(cur) || is.na(tgt) || !is.finite(cur) || !is.finite(tgt) || tgt == 0) return(NA_real_)
    (tgt - cur) / tgt
  })

  # Session「此刻」模型參數（動態重建用；不落庫）
  # 歷史 PIT 的 Ke/WACC 會在再平衡日以 Rolling β＋當年 ^TNX Rf＋截至該日基準已實現 Rm＋當日市值 We/Wd 覆寫；
  # 此處 Rf/Rm／We/Wd 僅作抓不到 TNX／負債／基準報酬時的 fallback。 Session Rm 用於折線末端。
  bt_current_model_params <- reactive({
    wacc <- if (identical(input$dcf_mode, "two_stage")) {
      suppressWarnings(as.numeric(input$wacc_stage1)[1]) / 100
    } else {
      suppressWarnings(as.numeric(input$wacc_gordon)[1]) / 100
    }
    if (!is.finite(wacc) || wacc <= 0) {
      wacc <- if (!is.null(calculated_wacc()) && is.finite(calculated_wacc())) {
        as.numeric(calculated_wacc())
      } else {
        APP_DEFAULTS$wacc_gordon / 100
      }
    }
    sgr <- suppressWarnings(as.numeric(input$sgr)[1]) / 100
    n_years <- suppressWarnings(as.integer(input$years)[1])
    if (is.na(n_years) || n_years < 1L) n_years <- 5L
    g_explicit <- if (identical(input$dcf_mode, "two_stage")) {
      suppressWarnings(as.numeric(input$g_stage1)[1]) / 100
    } else {
      cg <- suppressWarnings(as.numeric(input$custom_g)[1])
      if (is.finite(cg)) cg / 100 else sgr
    }
    ke <- tryCatch(as.numeric(central_ke())[1], error = function(e) NA_real_)
    if (!is.finite(ke) || ke <= 0) {
      ke <- suppressWarnings(as.numeric(input$wacc_re)[1]) / 100
    }
    if (!is.finite(ke) || ke <= 0) ke <- wacc

    # P/B tab (module id mod_pb)
    pb_mid <- suppressWarnings(as.numeric(input[["mod_pb-pb_mid"]])[1])
    if (!is.finite(pb_mid) || pb_mid <= 0) {
      pb_mid <- suppressWarnings(as.numeric(input$pb_mid)[1])
    }
    if (!is.finite(pb_mid) || pb_mid <= 0) pb_mid <- APP_DEFAULTS$pb_mid

    # NAV tab (module id mod_nav)
    nav_mid <- suppressWarnings(as.numeric(input[["mod_nav-nav_mid"]])[1])
    if (!is.finite(nav_mid) || nav_mid <= 0) nav_mid <- APP_DEFAULTS$nav_mid %||% 1

    # DDM tab
    ddm_g <- suppressWarnings(as.numeric(input[["mod_ddm-g"]])[1])
    if (!is.finite(ddm_g)) ddm_g <- sgr * 100
    ddm_g <- ddm_g / 100
    ddm_ke <- suppressWarnings(as.numeric(input[["mod_ddm-ke"]])[1])
    if (is.finite(ddm_ke) && ddm_ke > 0) {
      ddm_ke <- ddm_ke / 100
    } else {
      ddm_ke <- ke
    }

    # RI / DDM / P/B tabs: applied only to HFV chart tip (latest point)
    ri_g <- suppressWarnings(as.numeric(input[["mod_ri-ri_g"]])[1])
    if (is.finite(ri_g)) ri_g <- ri_g / 100 else ri_g <- g_explicit
    ri_ke <- suppressWarnings(as.numeric(input[["mod_ri-ri_ke"]])[1])
    if (is.finite(ri_ke) && ri_ke > 0) ri_ke <- ri_ke / 100 else ri_ke <- ke
    ri_years <- suppressWarnings(as.integer(input[["mod_ri-ri_years"]])[1])
    if (!is.finite(ri_years) || ri_years < 1L) ri_years <- n_years
    ri_roe <- suppressWarnings(as.numeric(input[["mod_ri-ri_roe"]])[1])
    if (is.finite(ri_roe)) ri_roe <- ri_roe / 100 else ri_roe <- NA_real_
    ri_payout <- suppressWarnings(as.numeric(input[["mod_ri-ri_payout"]])[1])
    if (is.finite(ri_payout)) ri_payout <- max(0, min(1, ri_payout / 100)) else ri_payout <- NA_real_
    roe_method <- as.character(input[["mod_ri-roe_method"]] %||% "constant")[1]
    roe_terminal <- suppressWarnings(as.numeric(input[["mod_ri-roe_terminal"]])[1])
    if (is.finite(roe_terminal)) roe_terminal <- roe_terminal / 100 else roe_terminal <- ri_roe
    roe_industry <- suppressWarnings(as.numeric(input[["mod_ri-roe_industry"]])[1])
    if (is.finite(roe_industry)) roe_industry <- roe_industry / 100 else roe_industry <- 0.12
    roe_custom_vec <- NULL
    if (identical(roe_method, "custom") && exists(".parse_roe_pct_vector", mode = "function")) {
      roe_custom_vec <- tryCatch(
        .parse_roe_pct_vector(input[["mod_ri-roe_custom_txt"]]),
        error = function(e) NULL
      )
    }

    if (!is.finite(wacc) || wacc <= 0) wacc <- APP_DEFAULTS$wacc_gordon / 100
    if (!is.finite(sgr)) sgr <- APP_DEFAULTS$sgr / 100
    if (!is.finite(g_explicit)) g_explicit <- sgr
    fv_models <- ynow_order_fv_engines(.bt_selected_fv_models())
    # 未選復盤模型：保持空向量，策略 fair_value／MOS = NA（不暗設 DCF）

    rf <- suppressWarnings(as.numeric(input$capm_rf)[1]) / 100
    if (!is.finite(rf) || rf <= 0) {
      rf <- tryCatch(as.numeric(cached_get_risk_free_rate()) / 100, error = function(e) NA_real_)
    }
    if (!is.finite(rf) || rf <= 0) rf <- APP_DEFAULTS$capm_rf / 100
    rm <- suppressWarnings(as.numeric(input$capm_rm)[1]) / 100
    if (!is.finite(rm) || rm <= 0) rm <- APP_DEFAULTS$capm_rm / 100
    rd <- suppressWarnings(as.numeric(input$wacc_rd)[1]) / 100
    if (!is.finite(rd) || rd < 0) rd <- NA_real_
    tax <- suppressWarnings(as.numeric(input$wacc_tax)[1]) / 100
    if (!is.finite(tax) || tax < 0) tax <- APP_DEFAULTS$wacc_tax / 100
    beta_fb <- suppressWarnings(as.numeric(input$capm_beta)[1])
    if (!is.finite(beta_fb)) beta_fb <- APP_DEFAULTS$capm_beta

    # Session We/Wd: fallback if a rebalance cannot form PIT market-value weights.
    we <- NA_real_; wd <- NA_real_
    tryCatch({
      bs <- d_balance_sheet()
      sum_df <- summary_data()
      sh <- resolve_valuation_shares(
        bs, sum_df,
        ticker = current_ticker() %||% "",
        quote_currency = quote_currency(),
        financial_currency = statement_currency()
      )
      shares <- suppressWarnings(as.numeric(sh$shares)[1])
      price_val <- suppressWarnings(as.numeric(sh$price)[1])
      mcap_val <- suppressWarnings(as.numeric(sh$market_cap)[1])
      equity_mv <- if (is.finite(mcap_val) && mcap_val > 0) {
        mcap_val
      } else if (is.finite(shares) && shares > 0 && is.finite(price_val)) {
        shares * price_val
      } else {
        NA_real_
      }
      debt <- select_current_metric(bs, "Total Debt", "stock")
      debt <- if (is.na(debt)) 0 else debt
      if (is.finite(equity_mv) && equity_mv > 0) {
        tot <- equity_mv + debt
        if (is.finite(tot) && tot > 0) {
          we <- equity_mv / tot
          wd <- debt / tot
        }
      }
    }, error = function(e) NULL)

    list(
      wacc = wacc, ke = ke, sgr = sgr, g_explicit = g_explicit,
      n_years = n_years, pb_mid = pb_mid, nav_mid = nav_mid, ddm_g = ddm_g, ddm_ke = ddm_ke,
      ddm_mode = as.character(input[["mod_ddm-ddm_mode"]] %||% APP_DEFAULTS$ddm_mode)[1],
      ddm_eps = {
        v <- suppressWarnings(as.numeric(input[["mod_ddm-est_eps"]])[1])
        if (is.finite(v)) v else NA_real_
      },
      ddm_g_stage1 = {
        v <- suppressWarnings(as.numeric(input[["mod_ddm-g_stage1"]])[1])
        if (is.finite(v)) v / 100 else g_explicit
      },
      ddm_yr_stage1 = {
        v <- suppressWarnings(as.integer(input[["mod_ddm-yr_stage1"]])[1])
        if (is.finite(v) && v >= 1L) v else n_years
      },
      dcf_claim = as.character(input$dcf_claim %||% APP_DEFAULTS$dcf_claim)[1],
      ri_roe = ri_roe, ri_payout = ri_payout, ri_years = ri_years,
      ri_g = ri_g, ri_ke = ri_ke,
      roe_method = roe_method, roe_terminal = roe_terminal,
      roe_industry = roe_industry, roe_custom_vec = roe_custom_vec,
      fv_model = fv_models,
      fv_models = fv_models,
      rf = rf, rm = rm, rd = rd, tax = tax,
      we = we, wd = wd,
      beta_fallback = beta_fb,
      beta_lookback_months = 60L,
      beta_min_months = 24L,
      # ADR／雙重股權：折現比較股數對齊報價股（市值÷股價倍率套用各財年）
      summary_df = tryCatch(summary_data(), error = function(e) NULL),
      quote_currency = tryCatch(quote_currency(), error = function(e) NULL),
      financial_currency = tryCatch(statement_currency(), error = function(e) NULL),
      quote_price = tryCatch(extract_quote_price_mcap(summary_data())$price, error = function(e) NA_real_),
      market_cap = tryCatch(extract_quote_price_mcap(summary_data())$market_cap, error = function(e) NA_real_)
    )
  })

  apply_bt_params_to_ui <- function(p) {
    bt_applying_params(TRUE)
    on.exit(bt_applying_params(FALSE), add = TRUE)
    updateNumericInput(session, "bt_net_margin", value = p$bt_net_margin)
    updateNumericInput(session, "bt_rev_growth", value = p$bt_rev_growth)
    updateNumericInput(session, "bt_eps_growth", value = p$bt_eps_growth)
    updateNumericInput(session, "bt_fcf_cv", value = p$bt_fcf_cv)
    updateSliderInput(session, "bt_w_mom", value = p$bt_w_mom)
    updateSliderInput(session, "bt_w_rsi", value = p$bt_w_rsi)
    updateSliderInput(session, "bt_w_vg", value = p$bt_w_vg)
    bt_param_notes_txt(p$notes)
  }

  refresh_bt_params <- function(fetch_hist = TRUE) {
    req(current_ticker(), d_income_statement(), d_cash_flow())
    hist_long <- NULL
    if (isTRUE(fetch_hist)) {
      cached <- tryCatch(hist_stock_data(), error = function(e) NULL)
      if (!is.null(cached) && nrow(cached) >= 30) {
        hist_long <- cached[, c("Date", "Close", "Volume"), drop = FALSE]
      } else {
        hist_long <- tryCatch(fetch_price_history_df(current_ticker(), "1y"), error = function(e) NULL)
      }
    }
    p <- derive_bt_params(
      d_is = d_income_statement(),
      d_bs = d_balance_sheet(),
      d_cf = d_cash_flow(),
      hist_df = hist_long,
      mos = bt_current_mos(),
      industry_choice = input$industry_choice
    )
    apply_bt_params_to_ui(p)
    invisible(p)
  }

  observeEvent(list(current_ticker(), scraped_financials()), {
    req(current_ticker(), scraped_financials())
    if (!isTRUE(input$bt_param_auto)) return()
    tryCatch(refresh_bt_params(fetch_hist = FALSE), error = function(e) {
      bt_param_notes_txt(paste("自動推導失敗：", e$message))
    })
  }, ignoreInit = FALSE)

  observeEvent(input$bt_refresh_params, {
    tryCatch({
      refresh_bt_params(fetch_hist = TRUE)
      showNotification(.ui_msg("notif_params_recalc_ok"), type = "message")
    }, error = function(e) {
      showNotification(.ui_msg("notif_params_recalc_fail", err = e$message), type = "error")
    })
  })

  observeEvent(input$bt_param_auto, {
    if (isTRUE(input$bt_param_auto)) {
      tryCatch(refresh_bt_params(fetch_hist = FALSE), error = function(e) NULL)
      bt_param_notes_txt(
        "「換股時自動更新參數」已開啟：換股／載入財報時會覆寫門檻、權重與推薦估值模型。若只要算一次，可取消勾選後按「依目前財報更新參數」。"
      )
    } else {
      bt_param_notes_txt(
        "「換股時自動更新參數」已關閉：參數不會因換股被覆寫。需要時可按「依目前財報更新參數」單次更新。"
      )
    }
  })

  # 圖表複選或復盤單選變更 → 重建基本面價值；兩者皆空則隱藏折線
  .bt_refresh_hfv_fv <- function(progress_msg = "重建基本面價值…") {
    chart_sel <- .bt_raw_fv_models()
    replay_sel <- .bt_replay_fv_model()
    if (length(chart_sel) < 1L && length(replay_sel) < 1L) {
      bt_fv_visible(FALSE)
      bt_hfv_fv(NULL)
      return(invisible(NULL))
    }
    if (is.null(current_ticker()) || !nzchar(as.character(current_ticker())[1])) {
      return(invisible(NULL))
    }
    if (is.null(d_income_statement()) || is.null(d_cash_flow()) || is.null(d_balance_sheet())) {
      stop("請先在 Dashboard 搜尋並載入該公司財報")
    }
    mp <- bt_current_model_params()
    fund <- build_annual_fundamentals_for_quote(
      d_income_statement(), d_balance_sheet(), d_cash_flow(),
      ticker = current_ticker(), model_params = mp
    )
    withProgress(message = progress_msg, value = 0.2, {
      fv_res <- compute_fair_value_timeline(
        ticker = current_ticker(),
        d_is = d_income_statement(),
        d_bs = d_balance_sheet(),
        d_cf = d_cash_flow(),
        model_params = mp,
        mos = bt_current_mos(),
        bench_ticker = active_bench_ticker(),
        years = 5,
        rebal_freq = .bt_selected_rebal_freq()
      )
      bt_hfv_fv(fv_res)
      bt_fv_visible(TRUE)
      if (!is.null(bt_result())) {
        bt_result(refresh_backtest_fair_value(bt_result(), fund, mp))
      }
      sa <- attr(fund, "share_align")
      if (is.list(sa) && shares_auto_adjust_method(sa$method) &&
          is.finite(sa$scale) && abs(sa$scale - 1) > 0.05) {
        showNotification(
          sprintf(
            "折現比較股數已對齊報價股（×%.3g；%s）",
            sa$scale, sa$method
          ),
          type = "message",
          duration = 6
        )
      }
    })
    invisible(NULL)
  }

  observeEvent(list(input$bt_fv_models, input$bt_fv_replay_model), {
    if (isTRUE(bt_applying_params())) return()
    if (isTRUE(input$bt_param_auto)) {
      updateCheckboxInput(session, "bt_param_auto", value = FALSE)
    }
    tryCatch(
      .bt_refresh_hfv_fv(),
      error = function(e) {
        bt_fv_visible(FALSE)
        showNotification(.ui_msg("notif_fv_fail", err = e$message), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # 搜尋載入財報後：若已有復盤模型（預設 DCF）且尚無 FV，自動重建一次
  observeEvent(list(current_ticker(), d_income_statement(), d_balance_sheet(), d_cash_flow()), {
    if (isTRUE(bt_applying_params())) return()
    if (!is.null(bt_hfv_fv())) return()
    if (length(.bt_replay_fv_model()) < 1L && length(.bt_raw_fv_models()) < 1L) return()
    if (is.null(current_ticker()) || !nzchar(as.character(current_ticker())[1])) return()
    if (is.null(d_income_statement()) || is.null(d_cash_flow()) || is.null(d_balance_sheet())) return()
    tryCatch(
      .bt_refresh_hfv_fv(),
      error = function(e) NULL
    )
  }, ignoreInit = TRUE)

  # 分析頻率變更：以該頻率重建 Date_t／FV；策略回測需重跑（再平衡日會變）
  observeEvent(input$bt_fv_analysis_freq, {
    if (isTRUE(bt_freq_applying())) return()
    if (isTRUE(bt_applying_params())) return()
    freq <- .bt_selected_rebal_freq()
    # Invalidate strategy result if its rebalance calendar no longer matches.
    res <- bt_result()
    if (!is.null(res)) {
      res_freq <- .bt_normalize_rebal_freq(res$rebal_freq %||% "quarterly")
      if (!identical(res_freq, freq)) {
        bt_result(NULL)
        bt_validation(NULL)
        loc_freq <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
        bt_run_msg(sprintf(
          ui_str("hfv_freq_switched_msg", loc_freq),
          .bt_rebal_freq_label(freq, loc_freq)
        ))
      }
    }
    sel_chart <- .bt_raw_fv_models()
    sel_replay <- .bt_replay_fv_model()
    if (length(sel_chart) < 1L && length(sel_replay) < 1L) {
      bt_hfv_fv(NULL)
      return()
    }
    if (is.null(current_ticker()) || !nzchar(as.character(current_ticker())[1])) return()
    if (is.null(d_income_statement()) || is.null(d_cash_flow()) || is.null(d_balance_sheet())) {
      return()
    }
    tryCatch({
      mp <- bt_current_model_params()
      loc_freq <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
      withProgress(
        message = sprintf(ui_str("hfv_freq_rebuild_progress", loc_freq), .bt_rebal_freq_label(freq, loc_freq)),
        value = 0.2, {
        fv_res <- compute_fair_value_timeline(
          ticker = current_ticker(),
          d_is = d_income_statement(),
          d_bs = d_balance_sheet(),
          d_cf = d_cash_flow(),
          model_params = mp,
          mos = bt_current_mos(),
          bench_ticker = active_bench_ticker(),
          years = 5,
          rebal_freq = freq
        )
        bt_hfv_fv(fv_res)
        bt_fv_visible(TRUE)
      })
    }, error = function(e) {
      showNotification(.ui_msg("notif_freq_rebuild_fail", err = e$message), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)

  observeEvent(list(input$bt_w_vg, input$bt_w_mom, input$bt_w_rsi,
                    input$bt_net_margin, input$bt_rev_growth, input$bt_eps_growth, input$bt_fcf_cv,
                    input$bt_max_exp, input$bt_min_exp_pass), {
    if (isTRUE(bt_applying_params())) return()
    if (!isTRUE(input$bt_param_auto)) return()
    updateCheckboxInput(session, "bt_param_auto", value = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$bt_fit_bh_preset, {
    bt_applying_params(TRUE)
    on.exit(bt_applying_params(FALSE), add = TRUE)
    updateCheckboxInput(session, "bt_param_auto", value = FALSE)
    updateSliderInput(session, "bt_max_exp", value = 1)
    updateSliderInput(session, "bt_min_exp_pass", value = 0.4)
    updateSliderInput(session, "bt_w_vg", value = 0.35)
    showNotification(.ui_msg("notif_bh_preset"),
                     type = "message", duration = 8)
  })

  output$bt_param_notes <- renderUI({
    msg <- bt_param_notes_txt()
    tags$div(
      style = "margin: 0 0 12px 0; padding: 12px 14px; background: #f7fbf8; border-left: 4px solid #00a65a; border-radius: 4px; font-size: 13px; color: #333; line-height: 1.55; width: 100%;",
      icon("info-circle"), " ", msg
    )
  })

  output$bt_run_status <- renderUI({
    msg <- bt_run_msg()
    if (!nzchar(msg)) return(NULL)
    tags$p(style = "margin: 10px 0 0 0; color: #666; font-size: 12px; line-height: 1.45;", icon("clock"), " ", msg)
  })

  observeEvent(input$run_bt, {
    req(current_ticker())
    # Points gate (Full + enforce): Backtest full run.
    if (!isTRUE(tryCatch(lite_mode(), error = function(e) FALSE)) &&
        exists("ynow_points_enabled", mode = "function") && isTRUE(ynow_points_enabled()) &&
        exists("ynow_points_gate", mode = "function")) {
      u <- .ynow_pts_effective_user()
      ref <- paste0(
        "bt_run:", as.character(u$id %||% "anon")[1], ":",
        toupper(as.character(current_ticker())[1]), ":",
        as.character(as.numeric(Sys.time())), ":",
        paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
      )
      loc_now <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
      if (!isTRUE(ynow_points_gate(session, "bt_run", ref, user = u, locale = loc_now))) {
        return()
      }
    }
    if (!isTRUE(ynow_heavy_job_try_acquire("hfv_bt"))) {
      showNotification(
        tryCatch(
          .ui_msg("capacity_heavy_job_busy"),
          error = function(e) "Another heavy job is running on this server. Please retry shortly."
        ),
        type = "warning",
        duration = 8
      )
      return()
    }
    bt_result(NULL)
    bt_validation(NULL)
    bt_hfv_fv(NULL)
    bt_fv_visible(FALSE)
    bt_run_msg("V12 回測計算中（PIT 多模型重建）…")
    .ynow_set_tab_busy("hfv", TRUE)
    on.exit({
      tryCatch(.ynow_set_tab_busy("hfv", FALSE), error = function(e) NULL)
      tryCatch(ynow_heavy_job_release("hfv_bt"), error = function(e) NULL)
    }, add = TRUE)
    tryCatch({
      if (is.null(d_income_statement()) || is.null(d_cash_flow()) || is.null(d_balance_sheet())) {
        stop("請先在 Dashboard 搜尋並載入該公司財報")
      }
      lad <- if (exists("mos_assumptions_ladder", mode = "function")) {
        mos_assumptions_ladder(input)
      } else {
        list(deep = 0.30, mid = 0.10, fair = 0.00, mild_rich = -0.10)
      }
      params <- list(
        bt_net_margin = input$bt_net_margin,
        bt_rev_growth = input$bt_rev_growth,
        bt_eps_growth = input$bt_eps_growth,
        bt_fcf_cv = input$bt_fcf_cv,
        bt_w_mom = input$bt_w_mom,
        bt_w_rsi = input$bt_w_rsi,
        bt_w_vg = input$bt_w_vg,
        bt_max_exp = input$bt_max_exp,
        bt_min_exp_pass = input$bt_min_exp_pass,
        mos_ladder_deep = lad$deep,
        mos_ladder_mid = lad$mid,
        mos_ladder_fair = lad$fair,
        mos_ladder_mild_rich = lad$mild_rich,
        bt_rebal_freq = .bt_selected_rebal_freq(),
        bt_news_on = isTRUE(input$bt_news_on),
        bt_news_score = tryCatch(ynow_news_now()$score, error = function(e) NA_real_)
      )
      mp <- bt_current_model_params()
      withProgress(message = paste("V12 回測", current_ticker(), "…"), value = 0.15, {
        res <- run_company_backtest(
          ticker = current_ticker(),
          d_is = d_income_statement(),
          d_bs = d_balance_sheet(),
          d_cf = d_cash_flow(),
          params = params,
          model_params = mp,
          mos = bt_current_mos(),
          bench_ticker = active_bench_ticker(),
          years = 5
        )
        incProgress(0.55, detail = "Alpha／MOS／Gap 驗證…")
        # Prefer backtest-native daily Close (same window as equity curve).
        px <- if (!is.null(res$equity_df$Close)) {
          data.frame(Date = res$equity_df$Date, Close = res$equity_df$Close,
                     stringsAsFactors = FALSE)
        } else {
          tryCatch({
            cached <- hist_stock_data()
            if (!is.null(cached) && nrow(cached) > 0) cached[, c("Date", "Close"), drop = FALSE]
            else data.frame(Date = res$valuation_df$Date, Close = res$valuation_df$hist_price,
                            stringsAsFactors = FALSE)
          }, error = function(e) {
            data.frame(Date = res$valuation_df$Date, Close = res$valuation_df$hist_price,
                       stringsAsFactors = FALSE)
          })
        }
        rf_ann <- tryCatch({
          r <- as.numeric(cached_get_risk_free_rate())
          if (is.finite(r) && r > 0) r / 100 else 0.04
        }, error = function(e) 0.04)

        alpha_df <- tryCatch(compute_alpha_dashboard(res$equity_df, rf_annual = rf_ann),
                             error = function(e) NULL)
        gap <- tryCatch(
          analyze_bh_gap(
            res$equity_df, res$valuation_df,
            max_exp = .safe_num(params$bt_max_exp, 0.90)
          ),
          error = function(e) list(narrative_a = e$message)
        )
        mos_tab <- tryCatch(validate_mos_effectiveness(res$valuation_df, px),
                            error = function(e) NULL)
        fv_edge <- tryCatch(validate_fair_value_edge(res$valuation_df, px),
                            error = function(e) NULL)
        # 參數高原已自 UI 移除（與 Sensitivity 重疊）；略過以縮短回測時間
        bt_result(res)
        bt_validation(list(
          alpha = alpha_df, gap = gap, mos = mos_tab, fv = fv_edge
        ))
        bt_hfv_fv(NULL)
        bt_fv_visible(TRUE)
        bt_run_msg(sprintf(
          "完成：%s 日 · %s PIT · Rolling β · 較佳=%s · Session WACC=%.2f%% Ke=%.2f%% SGR=%.2f%%",
          res$n_days, .bt_rebal_freq_label(res$rebal_freq %||% params$bt_rebal_freq),
          res$metrics$best,
          mp$wacc * 100, mp$ke * 100, mp$sgr * 100
        ))
      })
    }, error = function(e) {
      bt_run_msg(paste("失敗：", e$message))
      showNotification(.ui_msg("notif_bt_fail", err = e$message), type = "error", duration = 12)
    })
  })

  .fmt_pct <- function(x, digits = 1) {
    if (is.null(x) || length(x) < 1 || is.na(x) || !is.finite(x)) return("N/A")
    sprintf(paste0("%.", digits, "f%%"), 100 * as.numeric(x))
  }

  .bt_nav_window_bounds <- function(dates) {
    dates <- as.Date(dates)
    dates <- dates[!is.na(dates)]
    if (length(dates) < 1) {
      return(list(from = as.Date(NA), to = as.Date(NA), label = "全部", mode = "all"))
    }
    dmin <- min(dates)
    dmax <- max(dates)
    from <- dmin
    to <- dmax
    mode <- as.character(input$bt_nav_window %||% "all")[1]
    label <- "全部"
    if (identical(mode, "1y")) {
      from <- dmax - as.difftime(round(365.25), units = "days")
      label <- "近1年"
    } else if (identical(mode, "3y")) {
      from <- dmax - as.difftime(round(3 * 365.25), units = "days")
      label <- "近3年"
    } else if (identical(mode, "5y")) {
      from <- dmax - as.difftime(round(5 * 365.25), units = "days")
      label <- "近5年"
    } else if (identical(mode, "custom")) {
      rng <- input$bt_nav_custom
      if (!is.null(rng) && length(rng) >= 2 && !is.na(rng[[1]]) && !is.na(rng[[2]])) {
        from <- as.Date(rng[[1]])
        to <- as.Date(rng[[2]])
        if (from > to) {
          tmp <- from; from <- to; to <- tmp
        }
        label <- sprintf("自訂 %s～%s", format(from, "%Y-%m-%d"), format(to, "%Y-%m-%d"))
      } else {
        label <- "自訂（未選日期，用全部）"
      }
    }
    if (is.finite(from) && from < dmin) from <- dmin
    if (is.finite(to) && to > dmax) to <- dmax
    list(from = from, to = to, label = label, mode = mode)
  }

  observeEvent(bt_result(), {
    res <- bt_result()
    if (is.null(res) || is.null(res$equity_df) || nrow(res$equity_df) < 1) return()
    dts <- as.Date(res$equity_df$Date)
    dts <- dts[!is.na(dts)]
    if (length(dts) < 1) return()
    updateDateRangeInput(session, "bt_nav_custom", start = min(dts), end = max(dts))
  }, ignoreNULL = TRUE)

  bt_nav_view <- reactive({
    res <- bt_result()
    if (is.null(res) || is.null(res$equity_df) || nrow(res$equity_df) < 1) return(NULL)
    b <- .bt_nav_window_bounds(res$equity_df$Date)
    eq <- slice_rebase_nav(res$equity_df, b$from, b$to)
    vd <- res$valuation_df
    if (!is.null(vd) && is.data.frame(vd) && nrow(vd) > 0) {
      vd <- vd[as.Date(vd$Date) >= b$from & as.Date(vd$Date) <= b$to, , drop = FALSE]
    }
    list(equity_df = eq, valuation_df = vd, bounds = b, label = b$label)
  })

  .bt_hfv_chart_source <- function() {
    base <- bt_hfv_base()
    res <- bt_result()
    fv_only <- bt_hfv_fv()
    show_fv <- isTRUE(bt_fv_visible())

    if (!is.null(res) && !is.null(res$equity_df) && nrow(res$equity_df) > 0) {
      ed <- res$equity_df
      vd <- res$valuation_df
      mp <- res$model_params_used
    } else if (show_fv && !is.null(fv_only) && !is.null(fv_only$equity_df)) {
      ed <- fv_only$equity_df
      vd <- fv_only$valuation_df
      mp <- fv_only$model_params_used
    } else if (!is.null(base) && nrow(base) > 0) {
      ed <- base
      vd <- NULL
      mp <- NULL
    } else {
      return(NULL)
    }

    list(
      ed = ed,
      vd = vd,
      mp = mp,
      show_fv = show_fv && length(.bt_raw_fv_models()) > 0 && any(vapply(
        .bt_raw_fv_models(),
        function(m) {
          specs <- .bt_fv_model_specs()
          sp <- specs[[m]]
          if (is.null(sp)) return(FALSE)
          col <- sp$col
          col %in% names(ed) && any(is.finite(ed[[col]]))
        },
        logical(1)
      ))
    )
  }

  output$bt_valuation_summary <- renderUI({
    loc <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
    src <- .bt_hfv_chart_source()
    if (is.null(src)) {
      return(tags$p(
        style = "color:#888;font-size:12.5px;",
        ui_str("hfv_val_hint_search", loc)
      ))
    }
    if (!isTRUE(src$show_fv)) {
      return(tags$p(
        style = "color:#888;font-size:12.5px;",
        ui_str("hfv_val_hint_price_only", loc)
      ))
    }
    m <- if (!is.null(bt_result()) && !is.null(bt_result()$metrics)) {
      bt_result()$metrics
    } else if (!is.null(bt_hfv_fv()) && !is.null(bt_hfv_fv()$metrics)) {
      bt_hfv_fv()$metrics
    } else {
      NULL
    }
    mp <- src$mp
    if (is.null(m) || is.null(mp)) {
      return(tags$p(style = "color:#888;font-size:12.5px;", ui_str("hfv_val_hint_no_summary", loc)))
    }
    bias <- as.character(m$market_pricing_bias %||% "—")
    bias_tone <- if (grepl("低估", bias, fixed = TRUE)) {
      "bull"
    } else if (grepl("高估", bias, fixed = TRUE)) {
      "bear"
    } else {
      NULL
    }
    bias_val <- if (grepl("低估", bias, fixed = TRUE)) {
      ui_str("hfv_bias_undervalued", loc)
    } else if (grepl("高估", bias, fixed = TRUE)) {
      ui_str("hfv_bias_overvalued", loc)
    } else if (grepl("資料不足", bias, fixed = TRUE) || grepl("Insufficient", bias, fixed = TRUE)) {
      ui_str("hfv_bias_na", loc)
    } else {
      bias
    }
    pct_under <- suppressWarnings(as.numeric(m$pct_market_under %||% m$pct_strategy_under %||% NA_real_)[1])
    pct_over <- suppressWarnings(as.numeric(m$pct_market_over %||% m$pct_value_over %||% NA_real_)[1])
    # Majority only: underpricing OR overpricing — whichever share is larger
    maj_is_under <- if (is.finite(pct_under) && is.finite(pct_over)) {
      pct_under >= pct_over
    } else if (is.finite(pct_under)) {
      TRUE
    } else if (is.finite(pct_over)) {
      FALSE
    } else {
      NA
    }
    maj_label <- if (isTRUE(maj_is_under)) {
      ui_str("hfv_kpi_under_rate", loc)
    } else if (identical(maj_is_under, FALSE)) {
      ui_str("hfv_kpi_over_rate", loc)
    } else {
      ui_str("hfv_kpi_under_rate", loc)
    }
    maj_pct <- if (isTRUE(maj_is_under)) pct_under else if (identical(maj_is_under, FALSE)) pct_over else NA_real_
    maj_tone <- if (isTRUE(maj_is_under)) "bull" else if (identical(maj_is_under, FALSE)) "bear" else NULL
    maj_note <- if (isTRUE(maj_is_under)) {
      ui_str("hfv_kpi_under_note", loc)
    } else if (identical(maj_is_under, FALSE)) {
      ui_str("hfv_kpi_over_note", loc)
    } else {
      ui_str("hfv_kpi_under_note", loc)
    }
    last_sig <- as.character(m$last_signal %||% "—")
    sig_tone <- if (grepl("便宜", last_sig, fixed = TRUE) || grepl("Cheap", last_sig, fixed = TRUE)) {
      "bull"
    } else if (grepl("偏貴", last_sig, fixed = TRUE) || grepl("Rich", last_sig, fixed = TRUE)) {
      "bear"
    } else {
      NULL
    }
    last_sig_disp <- if (grepl("便宜", last_sig, fixed = TRUE) || grepl("Cheap", last_sig, fixed = TRUE)) {
      ui_str("hfv_sig_cheap", loc)
    } else if (grepl("偏貴", last_sig, fixed = TRUE) || grepl("Rich", last_sig, fixed = TRUE)) {
      ui_str("hfv_sig_expensive", loc)
    } else if (grepl("資料不足", last_sig, fixed = TRUE) || grepl("Insufficient", last_sig, fixed = TRUE)) {
      ui_str("hfv_sig_na", loc)
    } else {
      last_sig
    }
    # tone: bull=利多 (US green / TW red), bear=利空 (US red / TW green) via CSS
    .kpi <- function(label, value, note = NULL, tone = NULL, color = NULL, bg = NULL, border = NULL) {
      cell_cls <- "ynow-hfv-kpi-cell"
      val_cls <- "ynow-kpi-stat-value"
      sty <- NULL
      if (identical(tone, "bull")) {
        cell_cls <- paste(cell_cls, "ynow-hfv-kpi-cell--bull")
        val_cls <- paste(val_cls, "ynow-hfv-bull")
      } else if (identical(tone, "bear")) {
        cell_cls <- paste(cell_cls, "ynow-hfv-kpi-cell--bear")
        val_cls <- paste(val_cls, "ynow-hfv-bear")
      } else {
        bg <- bg %||% "#fff"
        border <- border %||% "#ddd"
        color <- color %||% "#222"
        sty <- paste0("background:", bg, ";border-left:4px solid ", border, ";")
      }
      tags$div(
        class = cell_cls,
        style = sty,
        tags$div(class = "ynow-kpi-stat-label", label),
        tags$div(
          class = val_cls,
          style = if (!is.null(tone)) NULL else paste0("color:", color, ";"),
          value
        ),
        if (!is.null(note) && nzchar(as.character(note)[1])) {
          tags$div(class = "ynow-kpi-stat-note", note)
        } else NULL
      )
    }
    tags$div(
      class = "ynow-hfv-kpi-grid",
      .kpi(ui_str("hfv_kpi_hist_pricing", loc), bias_val, tone = bias_tone),
      .kpi(maj_label, .fmt_pct(maj_pct, 0), note = maj_note, tone = maj_tone),
      .kpi(ui_str("hfv_kpi_last_signal", loc), last_sig_disp,
           note = ui_str("hfv_kpi_signal_note", loc), tone = sig_tone),
      .kpi(ui_str("hfv_kpi_mean_mos", loc), .fmt_pct(m$mean_hist_mos),
           color = "#222222", bg = "#f5f5f5", border = "#222222")
    )
  })

  # 折現比較圖正下方：此刻 Session 參數（全寬鍵值網格）
  output$bt_session_params <- renderUI({
    loc <- tryCatch(ui_locale(), error = function(e) "zh-TW")
    src <- .bt_hfv_chart_source()
    if (is.null(src) || !isTRUE(src$show_fv) || is.null(src$mp)) return(NULL)
    mp <- src$mp
    chart_m <- paste(toupper(.bt_raw_fv_models()), collapse = "+")
    if (!nzchar(chart_m)) chart_m <- "—"
    replay_m <- toupper(.bt_replay_fv_model())
    if (length(replay_m) < 1L || !nzchar(replay_m[1])) replay_m <- "—"
    claim <- toupper(as.character(mp$dcf_claim %||% "fcff")[1])
    ddm_mode <- as.character(mp$ddm_mode %||% "gordon")[1]
    .fmt_pct2 <- function(x) {
      v <- .safe_num(x, NA_real_)
      if (!is.finite(v)) return("—")
      sprintf("%.2f%%", v * 100)
    }
    .fmt_num2 <- function(x) {
      v <- .safe_num(x, NA_real_)
      if (!is.finite(v)) return("—")
      sprintf("%.2f", v)
    }
    .item <- function(key, val, span = FALSE) {
      tags$div(
        class = if (isTRUE(span)) {
          "ynow-hfv-session-params__item ynow-hfv-session-params__span"
        } else {
          "ynow-hfv-session-params__item"
        },
        tags$span(class = "ynow-hfv-session-params__key", key),
        tags$span(class = "ynow-hfv-session-params__val", val)
      )
    }
    items <- list(
      .item(ui_str("hfv_param_chart_models", loc), chart_m),
      .item(ui_str("hfv_param_replay_model", loc), replay_m[1]),
      .item("WACC", .fmt_pct2(mp$wacc)),
      .item("Ke", .fmt_pct2(mp$ke)),
      .item("SGR", .fmt_pct2(mp$sgr)),
      .item(ui_str("hfv_param_n_years", loc), as.character(mp$n_years %||% "—")),
      .item("PB mid", .fmt_num2(mp$pb_mid)),
      .item("DCF claim", claim)
    )
    if ("ri" %in% .bt_replay_fv_model() || "ri" %in% .bt_raw_fv_models()) {
      items <- c(
        items,
        list(
          .item("RI ROE", .fmt_pct2(mp$ri_roe)),
          .item("RI payout", {
            v <- .safe_num(mp$ri_payout, NA_real_)
            if (!is.finite(v)) "—" else sprintf("%.0f%%", v * 100)
          }),
          .item("RI n", as.character(mp$ri_years %||% mp$n_years %||% "—")),
          .item("RI g", .fmt_pct2(mp$ri_g)),
          .item("RI Ke", .fmt_pct2(mp$ri_ke)),
          .item("RI fade", as.character(mp$roe_method %||% "constant"))
        )
      )
    } else if ("ddm" %in% .bt_replay_fv_model() || "ddm" %in% .bt_raw_fv_models()) {
      items <- c(
        items,
        list(
          .item("DDM mode", ddm_mode),
          .item("DDM g", .fmt_pct2(mp$ddm_g)),
          .item("DDM Ke", .fmt_pct2(mp$ddm_ke))
        )
      )
    }
    title_txt <- ui_str("hfv_session_params_title", loc)
    tags$div(
      class = "ynow-hfv-session-params",
      tags$div(
        class = "ynow-hfv-session-params__title",
        icon("sliders-h"),
        tags$span(id = "ynow_hfv_session_params_title", title_txt)
      ),
      tags$div(class = "ynow-hfv-session-params__grid", items)
    )
  })

  output$bt_hfv_timeline <- renderPlotly({
    loc <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
    .hfv_empty <- function(msg) {
      plotly::plotly_empty() %>%
        plotly::layout(annotations = list(list(
          text = as.character(msg)[1],
          showarrow = FALSE,
          font = list(size = 14, color = "#888")
        )))
    }
    tryCatch({
      src <- .bt_hfv_chart_source()
      if (is.null(src) || is.null(src$ed) || !is.data.frame(src$ed) || nrow(src$ed) < 1) {
        return(.hfv_empty(ui_str("hfv_empty_need_search", loc)))
      }
      ed <- src$ed
      if (!("Date" %in% names(ed)) || !("Close" %in% names(ed))) {
        return(.hfv_empty(ui_str("hfv_empty_price_cols", loc)))
      }
      has_bench <- "Bench" %in% names(ed)
      price_lab <- ui_str("hfv_series_price", loc)
      bench_lab <- ui_str("hfv_series_bench", loc)

      p <- plotly::plot_ly()
      if (isTRUE(src$show_fv)) {
        specs <- .bt_fv_model_specs()
        for (m in .bt_raw_fv_models()) {
          sp <- specs[[m]]
          if (is.null(sp)) next
          col <- sp$col
          if (!col %in% names(ed) || !any(is.finite(ed[[col]]))) next
          p <- plotly::add_trace(
            p, x = ed$Date, y = ed[[col]], name = sp$label,
            type = "scatter", mode = "lines", inherit = FALSE,
            line = list(color = sp$color, width = 2),
            hovertemplate = paste0(sp$label, ": %{y:$.2f}<extra></extra>")
          )
        }
      }
      p <- plotly::add_trace(
        p, x = ed$Date, y = ed$Close, name = price_lab,
        type = "scatter", mode = "lines", inherit = FALSE,
        line = list(color = "#2c3e50", width = 2),
        hovertemplate = paste0(price_lab, ": %{y:$.2f}<extra></extra>")
      )
      show_bench <- !isFALSE(input$bt_hfv_show_bench) && has_bench && any(is.finite(ed$Bench))
      if (show_bench) {
        p <- plotly::add_trace(
          p, x = ed$Date, y = ed$Bench, name = bench_lab,
          type = "scatter", mode = "lines", inherit = FALSE,
          line = list(color = "#7f8c8d", width = 1.6, dash = "dot"),
          yaxis = "y2",
          hovertemplate = paste0(bench_lab, ": %{y:$.2f}<extra></extra>")
        )
      }
      vd <- src$vd
      if (isTRUE(src$show_fv) && !is.null(vd) && is.data.frame(vd) && nrow(vd) > 0) {
        replay <- .bt_replay_fv_model()
        marker_col <- if (length(replay) == 1L) {
          switch(replay[1],
            "dcf" = "fv_dcf", "ddm" = "fv_ddm", "ri" = "fv_ri", "pb" = "fv_pb", "nav" = "fv_nav",
            "fair_value")
        } else {
          "fair_value"
        }
        if (marker_col %in% names(vd) && any(is.finite(vd[[marker_col]]))) {
          fmt_h <- function(x, digits = 1, pct = FALSE) {
            x <- suppressWarnings(as.numeric(x))
            out <- ifelse(is.finite(x),
                          if (pct) sprintf(paste0("%.", digits, "f%%"), 100 * x)
                          else sprintf(paste0("%.", digits, "f"), x),
                          "N/A")
            as.character(out)
          }
          n_vd <- nrow(vd)
          win <- if ("rm_window" %in% names(vd)) as.character(vd$rm_window) else rep("—", n_vd)
          if (length(win) != n_vd) win <- rep(as.character(win[1] %||% "—"), n_vd)
          win[!nzchar(win) | is.na(win)] <- "—"
          ht <- paste0(
            ui_str("hfv_hover_rebal", loc), " ", format(as.Date(vd$Date), "%Y-%m-%d"),
            "<br>FV ", fmt_h(vd[[marker_col]], 2),
            if ("rolling_beta" %in% names(vd)) paste0("<br>β ", fmt_h(vd$rolling_beta, 2)) else "",
            if ("rf_pit" %in% names(vd)) paste0("<br>Rf ", fmt_h(vd$rf_pit, 1, pct = TRUE)) else "",
            if ("rm_pit" %in% names(vd)) paste0("<br>Rm ", fmt_h(vd$rm_pit, 1, pct = TRUE), " (", win, ")") else "",
            if ("we_pit" %in% names(vd)) paste0("<br>We ", fmt_h(vd$we_pit, 0, pct = TRUE)) else ""
          )
          p <- plotly::add_trace(
            p,
            x = vd$Date,
            y = vd[[marker_col]],
            name = ui_str("hfv_marker_rebal_fv", loc),
            type = "scatter",
            mode = "markers",
            inherit = FALSE,
            marker = list(color = "#e74c3c", size = 7, symbol = "diamond"),
            text = ht,
            hovertemplate = "%{text}<extra></extra>"
          )
        }
      }
      title_txt <- if (isTRUE(src$show_fv)) {
        ui_str("hfv_chart_title_fv", loc)
      } else if (show_bench) {
        ui_str("hfv_chart_title_bench", loc)
      } else {
        ui_str("hfv_chart_title_price", loc)
      }
      qc <- tryCatch(quote_currency(), error = function(e) "USD")
      y1 <- list(
        title = .ui_msg("hfv_yaxis_per_share", ccy = money_label(qc), loc = loc),
        tickprefix = money_prefix(qc), side = "left"
      )
      if (show_bench) {
        plotly::layout(
          p,
          title = list(text = title_txt, font = list(size = 14)),
          legend = list(orientation = "h", y = -0.18),
          yaxis = y1,
          yaxis2 = list(
            title = ui_str("hfv_yaxis_bench", loc), overlaying = "y", side = "right",
            showgrid = FALSE, tickprefix = money_prefix(qc)
          ),
          xaxis = list(title = NULL),
          margin = list(l = 60, r = 60, t = 40, b = 60),
          hovermode = "x unified"
        )
      } else {
        plotly::layout(
          p,
          title = list(text = title_txt, font = list(size = 14)),
          legend = list(orientation = "h", y = -0.18),
          yaxis = y1,
          xaxis = list(title = NULL),
          margin = list(l = 60, r = 40, t = 40, b = 60),
          hovermode = "x unified"
        )
      }
    }, error = function(e) {
      .hfv_empty(.ui_msg("hfv_empty_plot_fail", err = conditionMessage(e), loc = loc))
    })
  })

  output$bt_exposure_stats <- renderUI({
    res <- bt_result()
    if (is.null(res) || is.null(res$exposure)) {
      return(tags$p(style = "color:#888;font-size:12px;", .ui_msg("bt_exposure_empty")))
    }
    e <- res$exposure
    tags$div(
      style = "font-size:13px;line-height:1.7;",
      tags$div(tags$b("基本面部位 平均 "), .fmt_pct(e$avg_a, 0),
               " ｜ 最高 ", .fmt_pct(e$max_a, 0),
               " ｜ 最低 ", .fmt_pct(e$min_a, 0),
               " ｜ 現金 ", .fmt_pct(e$cash_avg_a, 0)),
      tags$div(tags$b("情緒部位 平均 "), .fmt_pct(e$avg_b, 0),
               " ｜ 最高 ", .fmt_pct(e$max_b, 0),
               " ｜ 最低 ", .fmt_pct(e$min_b, 0),
               " ｜ 現金 ", .fmt_pct(e$cash_avg_b, 0)),
      tags$hr(),
      tags$div(style = "font-size:11px;color:#777;",
               "部位曲線驅動上方策略淨值（財富指數）。平均持股偏低時，終值常低於滿持股 B&H（現金報酬＝0）。")
    )
  })

  output$bt_exposure_plot <- renderPlotly({
    res <- bt_result()
    shiny::validate(shiny::need(!is.null(res) && !is.null(res$equity_df), "請先回測"))
    df <- res$equity_df
    if (exists(".ynow_downsample_df", mode = "function")) {
      df <- .ynow_downsample_df(df, max_n = 600L)
    }
    plotly::plot_ly() %>%
      plotly::add_lines(
        x = df$Date, y = df$Exp_A, name = "基本面部位 Exp_A",
        line = list(color = "#e67e22", width = 1.6),
        hovertemplate = "%{x|%Y-%m-%d}<br>Exp_A: %{y:.0%}<extra></extra>"
      ) %>%
      plotly::add_lines(
        x = df$Date, y = df$Exp_B, name = "情緒部位 Exp_B",
        line = list(color = "#2980b9", width = 1.6),
        hovertemplate = "%{x|%Y-%m-%d}<br>Exp_B: %{y:.0%}<extra></extra>"
      ) %>%
      plotly::layout(
        yaxis = list(title = "目標持股比例", tickformat = ".0%", range = c(0, 1)),
        xaxis = list(title = ""),
        legend = list(orientation = "h", y = -0.3),
        margin = list(t = 24, b = 56),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FAFBFC"
      ) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE, displaylogo = FALSE)
  })

  output$bt_bh_gap <- renderUI({
    view <- bt_nav_view()
    g <- NULL
    if (!is.null(view) && !is.null(view$equity_df) && nrow(view$equity_df) > 2) {
      g <- tryCatch(
        analyze_bh_gap(
          view$equity_df, view$valuation_df,
          max_exp = .safe_num(input$bt_max_exp, 0.90)
        ),
        error = function(e) NULL
      )
    }
    if (is.null(g)) {
      v <- bt_validation()
      if (is.null(v) || is.null(v$gap) || is.null(v$gap$terminal)) {
        return(tags$p(
          style = "color:#888;font-size:12px;",
          "回測後，本頁用本次數字拆相對 Buy&Hold：上漲日 (1−Exp_A)×r 加總（非複利），並列終值落差。"
        ))
      }
      g <- v$gap
    }
    if (!is.null(g$narrative_a) && is.null(g$terminal) &&
        (is.null(g$components_a))) {
      return(tags$p(style = "color:#c0392b;font-size:12px;", g$narrative_a))
    }
    fmt_pp <- function(x, digits = 1) {
      x <- suppressWarnings(as.numeric(x)[1])
      if (!is.finite(x)) return("—")
      sprintf("%+.*f pp", digits, 100 * x)
    }
    fmt_pct0 <- function(x) {
      x <- suppressWarnings(as.numeric(x)[1])
      if (!is.finite(x)) return("—")
      sprintf("%.0f%%", 100 * x)
    }
    fmt_term <- function(x) {
      x <- suppressWarnings(as.numeric(x)[1])
      if (!is.finite(x)) return("—")
      sprintf("%+.1f%%", 100 * x)
    }
    outcome <- as.character(g$outcome %||% if (isTRUE(g$beat_bh_a)) "ahead" else "behind")[1]
    headline <- as.character(g$headline %||% "相對 Buy & Hold")[1]
    tone_bg <- if (identical(outcome, "ahead") || identical(outcome, "flat")) "#eef8f1" else "#fff8e8"
    tone_bd <- if (identical(outcome, "ahead") || identical(outcome, "flat")) "#28a745" else "#f0ad4e"
    tone_fg <- if (identical(outcome, "ahead") || identical(outcome, "flat")) "#1e5c35" else "#7a5b10"
    ca <- g$components_a %||% list()
    n_fail <- as.integer(.safe_num(g$n_filter_fail, 0))
    n_rebal <- as.integer(.safe_num(g$n_rebal, 0))
    facts <- tags$div(
      style = "display:flex;flex-wrap:wrap;gap:8px;margin:0 0 10px 0;font-size:11.5px;color:#444;",
      tags$span(tags$b("累積區間 "), as.character((bt_nav_view()$label %||% "全部")[1])),
      tags$span("·"),
      tags$span(tags$b("最大持股 "), fmt_pct0(g$max_exp)),
      tags$span("·"),
      tags$span(tags$b("平均 Exp_A "), fmt_pct0(g$avg_exp_a)),
      tags$span("·"),
      tags$span(tags$b("空手日 "), sprintf("%s／%s（%s）",
        as.character(g$n_zero_a %||% "—"),
        as.character(g$n_days %||% "—"),
        fmt_pct0(g$pct_zero_a))),
      tags$span("·"),
      tags$span(tags$b("Filter 未過 "), sprintf("%s／%s 次再平衡",
        as.character(n_fail), as.character(n_rebal))),
      tags$span("·"),
      tags$span(tags$b("現金報酬＝0")),
      tags$span("·"),
      tags$span(tags$b("上漲日 "), sprintf("%s／%s",
        as.character(g$n_up_days %||% "—"),
        as.character(g$n_days %||% "—")))
    )
    tbl <- tags$table(
      style = "width:100%;font-size:12px;border-collapse:collapse;margin:0 0 10px 0;",
      tags$thead(tags$tr(
        tags$th(style = "text-align:left;padding:4px 6px;border-bottom:1px solid #ddd;", "分項"),
        tags$th(style = "text-align:right;padding:4px 6px;border-bottom:1px solid #ddd;", "數值")
      )),
      tags$tbody(
        tags$tr(
          tags$td(style = "padding:4px 6px;", "基本面策略終值"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_term(g$terminal$a))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;", "Buy&Hold 終值"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_term(g$terminal$bh))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;", "終值落差（複利，B&H − 基本面）"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_pp(g$terminal$shortfall_a))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;", "上漲日 (1−Exp_A)×r 加總"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_pp(ca$additive_up))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;padding-left:14px;", "　現金拖累（未歸入下兩項）"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_pp(ca$cash_drag))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;padding-left:14px;", "　提前出場"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_pp(ca$early_exit))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;padding-left:14px;", "　高估減碼"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_pp(ca$overvaluation_reduction))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;", "殘差＝終值落差 − 上漲日加總（非恆等）"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_pp(ca$missed_trend))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;", "情緒策略終值"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_term(g$terminal$b))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;", "情緒減碼 vs 基本面（上漲日 (Exp_A−Exp_B)×r）"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_pp(g$sentiment_reduction_b))
        ),
        tags$tr(
          tags$td(style = "padding:4px 6px;", "情緒加碼 vs 基本面（上漲日）"),
          tags$td(style = "padding:4px 6px;text-align:right;", fmt_pp(g$sentiment_boost_b))
        )
      )
    )
    bullets <- g$bullets
    if (is.null(bullets) || length(bullets) < 1) {
      bullets <- g$narrative_a
    }
    tagList(
      tags$div(
        style = paste0(
          "margin:0 0 10px 0;padding:8px 10px;background:", tone_bg,
          ";border-left:4px solid ", tone_bd, ";font-size:13px;color:", tone_fg, ";"
        ),
        tags$b(headline)
      ),
      facts,
      tbl,
      tags$ul(
        style = "margin:0;padding-left:18px;font-size:12px;line-height:1.55;color:#333;",
        lapply(bullets, function(b) tags$li(b))
      ),
      tags$p(
        style = "margin:8px 0 0 0;font-size:11px;color:#777;",
        "分項只加總 B&H 上漲日的 (1−Exp_A)×r，不是複利拆解；殘差用來對照終值落差。"
      )
    )
  })

  output$bt_equity_plot <- renderPlotly({
    view <- bt_nav_view()
    shiny::validate(shiny::need(!is.null(view) && !is.null(view$equity_df), "請先成功執行回測"))
    df_plot <- view$equity_df
    shiny::validate(shiny::need(nrow(df_plot) > 1, "此累積區間沒有足夠的交易日"))
    shiny::validate(shiny::need("Trade_A" %in% names(df_plot), "缺少基本面策略淨值 (Trade_A)"))
    if (exists(".ynow_downsample_df", mode = "function")) {
      df_plot <- .ynow_downsample_df(df_plot, max_n = 600L)
    }
    eq_b <- if ("Trade_B" %in% names(df_plot)) df_plot$Trade_B else df_plot$Model_B
    win_lab <- view$label %||% "全部"
    plotly::plot_ly() %>%
      plotly::add_lines(
        x = df_plot$Date, y = df_plot$Trade_A, name = "基本面策略淨值",
        line = list(color = "#e67e22", width = 1.8),
        hovertemplate = "%{x|%Y-%m-%d}<br>基本面: %{y:.3f}<extra></extra>"
      ) %>%
      plotly::add_lines(
        x = df_plot$Date, y = eq_b, name = "情緒策略淨值",
        line = list(color = "#2980b9", width = 1.8),
        hovertemplate = "%{x|%Y-%m-%d}<br>情緒: %{y:.3f}<extra></extra>"
      ) %>%
      plotly::add_lines(
        x = df_plot$Date, y = df_plot$BuyHold, name = "該股買進持有",
        line = list(color = "#28a745", width = 1.6),
        hovertemplate = "%{x|%Y-%m-%d}<br>B&H: %{y:.3f}<extra></extra>"
      ) %>%
      plotly::add_lines(
        x = df_plot$Date, y = df_plot$Benchmark, name = "大盤基準",
        line = list(color = "#6c757d", width = 1.4, dash = "dash"),
        hovertemplate = "%{x|%Y-%m-%d}<br>基準: %{y:.3f}<extra></extra>"
      ) %>%
      plotly::layout(
        title = list(
          text = paste0("策略淨值（累積財富，起始＝1 · ", win_lab, "）"),
          font = list(size = 14)
        ),
        yaxis = list(title = "累積財富（區間起點＝1）"),
        xaxis = list(title = "日期"),
        legend = list(orientation = "h", y = -0.2),
        margin = list(t = 48, b = 64),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FAFBFC"
      ) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE, displaylogo = FALSE)
  })

  output$bt_mos_table <- renderTable({
    v <- bt_validation()
    shiny::validate(shiny::need(!is.null(v) && !is.null(v$mos) && nrow(v$mos) > 0, "尚無 MOS 分組結果"))
    tab <- v$mos
    data.frame(
      MOS分組 = tab$bucket,
      樣本數 = tab$n,
      `1Y報酬` = ifelse(is.na(tab$ret_1y), NA, sprintf("%.1f%%", 100 * tab$ret_1y)),
      `3Y報酬` = ifelse(is.na(tab$ret_3y), NA, sprintf("%.1f%%", 100 * tab$ret_3y)),
      `5Y報酬` = ifelse(is.na(tab$ret_5y), NA, sprintf("%.1f%%", 100 * tab$ret_5y)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$bt_param_inventory <- renderTable({
    if (exists("pit_param_inventory_table", mode = "function")) {
      pit_param_inventory_table()
    } else {
      data.frame(訊息 = "參數盤點表未載入", stringsAsFactors = FALSE)
    }
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  .bt_fv_conv_bounds <- reactive({
    win <- as.character(input$bt_fv_conv_window %||% "all")[1]
    today <- Sys.Date()
    if (identical(win, "1y")) return(list(from = today - 365, to = today))
    if (identical(win, "3y")) return(list(from = today - 365 * 3, to = today))
    if (identical(win, "5y")) return(list(from = today - 365 * 5, to = today))
    if (identical(win, "custom")) {
      dr <- input$bt_fv_conv_custom
      if (is.null(dr) || length(dr) < 2) return(list(from = NULL, to = NULL))
      return(list(from = as.Date(dr[1]), to = as.Date(dr[2])))
    }
    list(from = NULL, to = NULL)
  })

  .bt_hfv_valuation_df <- function() {
    freq <- .bt_selected_rebal_freq()
    res <- bt_result()
    fv_only <- bt_hfv_fv()
    vd <- NULL
    if (!is.null(res) && !is.null(res$valuation_df)) {
      res_freq <- .bt_normalize_rebal_freq(res$rebal_freq %||% NA_character_, default = NA_character_)
      if (is.na(res_freq) || identical(res_freq, freq)) {
        vd <- res$valuation_df
      }
    }
    if (is.null(vd) && !is.null(fv_only) && !is.null(fv_only$valuation_df)) {
      fv_freq <- .bt_normalize_rebal_freq(fv_only$rebal_freq %||% NA_character_, default = NA_character_)
      if (is.na(fv_freq) || identical(fv_freq, freq)) {
        vd <- fv_only$valuation_df
      }
    }
    vd
  }

  .bt_run_fv_validation <- function(vd) {
    if (is.null(vd)) return(NULL)
    b <- .bt_fv_conv_bounds()
    mode <- as.character(input$bt_fv_oos_mode %||% "realized")[1]
    if (!mode %in% c("realized", "insample", "expanding")) mode <- "realized"
    thr <- if (exists("mos_assumptions_hfv_thr", mode = "function")) {
      tryCatch(mos_assumptions_hfv_thr(input), error = function(e) NULL)
    } else {
      NULL
    }
    tryCatch(
      summarize_fv_market_validation(
        vd, from = b$from, to = b$to,
        as_of = Sys.Date(), oos_mode = mode,
        locale = isolate(ui_locale()),
        thr = thr
      ),
      error = function(e) {
        tryCatch(
          summarize_fv_convergence(
            vd, from = b$from, to = b$to,
            as_of = Sys.Date(), oos_mode = mode,
            locale = isolate(ui_locale())
          ),
          error = function(e2) NULL
        )
      }
    )
  }

  # Replay-driven: odds / tip / MOS / gap / pair table
  bt_fv_conv <- reactive({
    .bt_run_fv_validation(.bt_hfv_valuation_df())
  })

  # Overlay-driven scenarios (multi-select → average FV); NULL unless A–D conclusion
  bt_fv_overlay_scenarios <- reactive({
    models <- .bt_raw_fv_models()
    if (length(models) < 1L) return(NULL)
    vd_avg <- overlay_avg_fair_value_df(.bt_hfv_valuation_df(), models)
    if (is.null(vd_avg)) return(NULL)
    s <- .bt_run_fv_validation(vd_avg)
    if (is.null(s) || is.null(s$scenarios) || !is.list(s$scenarios)) return(NULL)
    lead_c <- as.character(s$scenarios$most_frequent %||% NA_character_)[1]
    if (!nzchar(lead_c) || identical(lead_c, "NA")) lead_c <- NA_character_
    if (is.na(lead_c) || !(lead_c %in% c("A", "B", "C", "D"))) return(NULL)
    list(summary = s, scenarios = s$scenarios, models = models)
  })

  # Investor-report findings: one reactive builds chapter parts (II–V)
  bt_hfv_findings_parts <- reactive({
    s <- bt_fv_conv()
    loc <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
    if (is.null(s)) {
      empty <- tags$div(
        style = "margin:0 0 8px 0;padding:12px 14px;background:#f7f7f7;border:1px solid #e6e6e6;border-radius:6px;font-size:13px;color:#666;",
        ui_str("hfv_sum_empty", loc)
      )
      # Show the empty hint once (Section II only); later chapters keep leads only.
      return(list(
        empty = empty,
        investor = empty,
        price = NULL,
        fv = NULL,
        scenario = NULL
      ))
    }
    pct <- function(x) if (is.finite(x)) sprintf("%.0f%%", 100 * x) else "—"
    gap_pct <- function(x) if (is.finite(x)) sprintf("%+.1f%%", 100 * x) else "—"
    border <- if (isTRUE(s$small_sample) || isTRUE(s$no_strategy_fv)) "#f39c12" else "#00a65a"
    period_txt <- {
      if (!is.null(s$from) || !is.null(s$to)) {
        sprintf(
          ui_str("hfv_period_range_fmt", loc),
          if (is.null(s$from)) "…" else format(s$from, "%Y-%m-%d"),
          if (is.null(s$to)) "…" else format(s$to, "%Y-%m-%d")
        )
      } else ui_str("hfv_period_all", loc)
    }
    oos_lab <- switch(
      as.character(s$oos_mode %||% "realized")[1],
      realized = ui_str("hfv_oos_realized", loc),
      expanding = ui_str("hfv_oos_expanding", loc),
      insample = ui_str("hfv_oos_insample", loc),
      ui_str("hfv_oos_realized", loc)
    )
    # Sample snapshot KPIs (Replay). Scenarios sit under Ch1 chart (Overlay).
    # Up+down and shrink+expand share one cell each; bull/bear colors flip in TW via CSS.
    .hfv_kpi_pair <- function(label, v_bull, v_bear, note = NULL) {
      tags$div(
        class = "ynow-hfv-kpi-cell ynow-hfv-kpi-cell--pair",
        tags$div(class = "ynow-kpi-stat-label", label),
        tags$div(
          class = "ynow-kpi-stat-value ynow-hfv-kpi-pair-vals",
          tags$span(class = "ynow-hfv-bull", v_bull),
          tags$span(class = "ynow-hfv-kpi-pair-sep", " / "),
          tags$span(class = "ynow-hfv-bear", v_bear)
        ),
        if (!is.null(note) && nzchar(as.character(note)[1])) {
          tags$div(class = "ynow-kpi-stat-note", note)
        } else NULL
      )
    }
    conclusion_card <- {
      n_pairs <- as.integer(s$n %||% 0L)
      if (n_pairs > 0L && (is.finite(s$p_up) || is.finite(s$p_above) || is.finite(s$p_toward))) {
        q12_n <- as.integer(s$q12_diverge_n %||% 0L)
        tags$div(
          style = "margin:0 0 14px 0;",
          tags$div(
            style = "margin:0 0 8px 0;color:#555;font-size:12px;",
            ui_str("hfv_sum_qmap_lead", loc)
          ),
          tags$div(
            class = "ynow-hfv-kpi-grid ynow-hfv-kpi-grid--3",
            .hfv_kpi_pair(
              ui_str("hfv_kpi_ch2_up_down", loc),
              if (is.finite(s$p_up)) pct(s$p_up) else "—",
              if (is.finite(s$p_down)) pct(s$p_down) else "—",
              note = sprintf(ui_str("hfv_pair_n_fmt", loc), n_pairs)
            ),
            .hfv_kpi_pair(
              ui_str("hfv_kpi_ch2_toward_away", loc),
              if (is.finite(s$p_toward)) pct(s$p_toward) else "—",
              if (is.finite(s$p_away)) pct(s$p_away) else "—",
              note = sprintf(ui_str("hfv_pair_n_fmt", loc), n_pairs)
            ),
            tags$div(
              class = "ynow-hfv-kpi-cell",
              style = paste0("background:#fafafa;border-left:4px solid ", border, ";"),
              tags$div(class = "ynow-kpi-stat-label", ui_str("hfv_oos_mode_label", loc)),
              tags$div(class = "ynow-kpi-stat-value", style = "color:#555;font-size:14px !important;", oos_lab),
              tags$div(class = "ynow-kpi-stat-note", period_txt)
            )
          ),
          if (q12_n > 0L) {
            tags$div(
              style = "margin:8px 0 0 0;color:#555;font-size:12px;line-height:1.5;",
              sprintf(
                ui_str("hfv_sum_q12_diverge_fmt", loc),
                q12_n,
                as.integer(s$n_up_below %||% 0L),
                as.integer(s$n_down_above %||% 0L)
              )
            )
          } else NULL
        )
      } else {
        tags$div(
          style = paste0(
            "margin:0 0 14px 0;padding:12px 14px;background:#fafafa;",
            "border:1px solid #ddd;border-left:4px solid ", border, ";",
            "border-radius:6px;font-size:13px;line-height:1.55;color:#666;"
          ),
          ui_str("hfv_sum_conclusion_na", loc)
        )
      }
    }

    mo <- s$mos_outlook
    mos_body <- {
      if (!is.null(mo) && is.list(mo) && is.finite(mo$mos_now)) {
        tags$ul(
          style = "margin:6px 0 0 0;padding-left:18px;",
          tags$li(sprintf(
            ui_str("hfv_mos_now_fmt", loc),
            100 * mo$mos_now,
            mo$bucket %||% "—",
            mo$n %||% 0L,
            if (isTRUE(mo$small_sample)) ui_str("hfv_mos_small_sample", loc) else ""
          )),
          tags$li(sprintf(
            ui_str("hfv_mos_bucket_hist_fmt", loc),
            pct(mo$p_up), pct(mo$p_down),
            gap_pct(mo$median_ret), gap_pct(mo$mean_ret)
          ))
        )
      } else {
        tags$div(style = "margin-top:6px;color:#888;font-size:12px;", ui_str("hfv_mos_outlook_empty", loc))
      }
    }

    .hfv_kpi <- function(label, value, note = NULL, color = "#222", bg = "#fff", border_col = "#ddd",
                         tone = NULL) {
      cell_cls <- "ynow-hfv-kpi-cell"
      val_cls <- "ynow-kpi-stat-value"
      sty <- NULL
      if (identical(tone, "bull")) {
        cell_cls <- paste(cell_cls, "ynow-hfv-kpi-cell--bull")
        val_cls <- paste(val_cls, "ynow-hfv-bull")
      } else if (identical(tone, "bear")) {
        cell_cls <- paste(cell_cls, "ynow-hfv-kpi-cell--bear")
        val_cls <- paste(val_cls, "ynow-hfv-bear")
      } else {
        sty <- paste0("background:", bg, ";border-left:4px solid ", border_col, ";")
      }
      tags$div(
        class = cell_cls,
        style = sty,
        tags$div(class = "ynow-kpi-stat-label", label),
        tags$div(
          class = val_cls,
          style = if (!is.null(tone)) NULL else paste0("color:", color, ";"),
          value
        ),
        if (!is.null(note) && nzchar(as.character(note)[1])) {
          tags$div(class = "ynow-kpi-stat-note", note)
        } else NULL
      )
    }

    price_card <- tags$div(
      style = "margin:0;",
      tags$div(style = "margin:0 0 8px 0;color:#6c757d;font-size:11.5px;", ui_str("hfv_sum_price_formula", loc)),
      tags$p(style = "margin:0 0 8px 0;color:#555;font-size:12px;", period_txt),
      tags$div(
        class = "ynow-hfv-kpi-grid ynow-hfv-kpi-grid--3",
        .hfv_kpi(ui_str("hfv_kpi_ch3_n", loc), as.character(s$n %||% 0L), border_col = border),
        .hfv_kpi_pair(
          ui_str("hfv_kpi_ch3_up_down", loc),
          if (is.finite(s$p_up)) pct(s$p_up) else "—",
          if (is.finite(s$p_down)) pct(s$p_down) else "—",
          note = sprintf(
            ui_str("hfv_kpi_ch3_up_down_note", loc),
            s$n_up %||% 0L, s$n_down %||% 0L
          )
        ),
        .hfv_kpi(
          ui_str("hfv_kpi_ch3_med_ret", loc),
          gap_pct(s$median_ret),
          note = sprintf(ui_str("hfv_kpi_ch3_mean_ret_note", loc), gap_pct(s$mean_ret)),
          color = "#3c8dbc", bg = "#f5f9fc", border_col = "#3c8dbc"
        )
      ),
      tags$div(
        style = paste0(
          "margin:12px 0 0 0;padding:12px 14px;background:#fff;",
          "border:1px solid #d9e6f2;border-left:4px solid ", border, ";border-radius:6px;"
        ),
        tags$div(tags$b(ui_str("hfv_sum_mos_block", loc))),
        mos_body
      )
    )

    fv_card <- tags$div(
      style = "margin:0;",
      tags$div(style = "margin:0 0 8px 0;color:#6c757d;font-size:11.5px;", ui_str("hfv_sum_fv_formula", loc)),
      tags$p(style = "margin:0 0 8px 0;color:#555;font-size:12px;", period_txt),
      tags$div(
        class = "ynow-hfv-kpi-grid ynow-hfv-kpi-grid--3",
        .hfv_kpi(ui_str("hfv_kpi_ch4_n", loc), as.character(s$n %||% 0L), border_col = border),
        .hfv_kpi_pair(
          ui_str("hfv_kpi_ch4_toward_away", loc),
          if (is.finite(s$p_toward)) pct(s$p_toward) else "—",
          if (is.finite(s$p_away)) pct(s$p_away) else "—",
          note = sprintf(
            ui_str("hfv_kpi_ch4_toward_away_note", loc),
            s$n_toward %||% 0L, s$n_away %||% 0L
          )
        ),
        .hfv_kpi(
          ui_str("hfv_kpi_ch4_med_gap", loc),
          gap_pct(s$median_gap),
          note = sprintf(ui_str("hfv_kpi_ch4_mean_gap_note", loc), gap_pct(s$mean_gap)),
          color = "#3c8dbc", bg = "#f5f9fc", border_col = "#3c8dbc"
        )
      ),
      tags$p(
        style = "margin:8px 0 0 0;color:#555;font-size:12px;line-height:1.45;",
        ui_str("hfv_toward_read", loc)
      )
    )

    sc <- s$scenarios
    sc_lab <- function(code) {
      key <- switch(
        as.character(code)[1],
        A = "hfv_scenario_A",
        B = "hfv_scenario_B",
        C = "hfv_scenario_C",
        D = "hfv_scenario_D",
        "hfv_scenario_other"
      )
      ui_str(key, loc)
    }
    scenario_card <- {
      if (!is.null(sc) && is.list(sc) && as.integer(sc$n %||% 0L) > 0L) {
        cnt <- sc$counts
        frq <- sc$freq
        abcd <- c("A", "B", "C", "D")
        n_abcd <- vapply(abcd, function(code) {
          if (!is.null(cnt) && code %in% names(cnt)) as.integer(cnt[[code]]) else 0L
        }, integer(1))
        lead_code <- if (any(n_abcd > 0L)) abcd[which.max(n_abcd)] else NA_character_
        n_other <- if (!is.null(cnt) && "other" %in% names(cnt)) {
          as.integer(cnt[["other"]])
        } else {
          0L
        }
        p_other <- if (!is.null(frq) && "other" %in% names(frq)) {
          as.numeric(frq[["other"]])
        } else {
          NA_real_
        }
        make_sc_card <- function(code, icon_name, color) {
          n_c <- if (!is.null(cnt) && code %in% names(cnt)) {
            as.integer(cnt[[code]])
          } else {
            0L
          }
          p_c <- if (!is.null(frq) && code %in% names(frq)) {
            as.numeric(frq[[code]])
          } else {
            NA_real_
          }
          is_lead <- identical(code, lead_code) && n_c > 0L
          border_col <- if (is_lead) color else "#ddd"
          bg <- if (is_lead) "#fffaf2" else "#fff"
          tags$div(
            class = paste(
              "ynow-hfv-scenario-card-col",
              if (is_lead) "ynow-hfv-scenario-lead" else ""
            ),
            tags$div(
              class = "ynow-hfv-scenario-card",
              style = paste0(
                "border:1px solid ", border_col, ";",
                "border-radius:8px; padding:12px 12px 10px 12px; min-height:140px; background:", bg,
                "; box-shadow:0 2px 4px rgba(0,0,0,0.04); height:100%;"
              ),
              tags$div(style = paste0("font-size:18px; color:", color, ";"), icon(icon_name)),
              tags$h4(
                style = "margin:6px 0 4px 0; font-weight:700; font-size:14px; line-height:1.3;",
                sc_lab(code)
              ),
              if (is_lead) tags$span(
                style = paste0(
                  "display:inline-block; padding:2px 8px; border-radius:10px; font-size:11px; color:#fff; background:",
                  color, ";"
                ),
                ui_str("hfv_scenario_lead_badge", loc)
              ),
              tags$p(
                style = "margin:8px 0 4px 0; font-size:13px; font-weight:600; color:#333;",
                sprintf(ui_str("hfv_scenario_stat_fmt", loc), pct(p_c), n_c)
              ),
              tags$p(
                style = "margin:0; font-size:11.5px; color:#666; line-height:1.4;",
                ui_str(paste0("hfv_scenario_cue_", code), loc)
              )
            )
          )
        }
        tags$div(
          style = paste0(
            "margin:0 0 10px 0;padding:12px 14px;background:#fff;",
            "border:1px solid #e8dfd0;border-left:4px solid ", border, ";border-radius:6px;",
            "font-size:13px;line-height:1.55;"
          ),
          tags$style(HTML("
            .ynow-hfv-scenario-lead { transform: translateY(-2px); }
            .ynow-hfv-scenario-row {
              display: grid;
              grid-template-columns: 1fr 1fr;
              gap: 12px 14px;
              align-items: stretch;
              margin: 0 0 4px 0;
            }
            .ynow-hfv-scenario-row > .ynow-hfv-scenario-card-col {
              min-width: 0;
              width: auto;
              float: none;
              box-sizing: border-box;
            }
            @media (max-width: 575px) {
              .ynow-hfv-scenario-row {
                grid-template-columns: 1fr;
              }
            }
          ")),
          # Chapter V lead + method box cover formula/meaning; keep period + counts + cards.
          tags$p(style = "margin:0 0 6px 0;color:#555;font-size:12px;", period_txt),
          tags$div(
            style = "margin:0 0 6px 0;color:#555;font-size:12px;",
            sprintf(ui_str("hfv_pair_n_fmt", loc), sc$n %||% 0L)
          ),
          tags$div(
            class = "ynow-hfv-scenario-row",
            # 2×2: A B / C D
            make_sc_card("A", "gem", "#c9a227"),
            make_sc_card("B", "chart-line", "#00a65a"),
            make_sc_card("C", "exclamation-triangle", "#f39c12"),
            make_sc_card("D", "fire", "#dd4b39")
          ),
          {
            sc_color <- function(code) {
              switch(
                as.character(code)[1],
                A = "#c9a227",
                B = "#00a65a",
                C = "#f39c12",
                D = "#dd4b39",
                "#6c757d"
              )
            }
            make_concl_body <- function(code) {
              code <- as.character(code)[1]
              if (identical(code, "A")) {
                tagList(
                  tags$b(ui_str("hfv_scenario_concl_A_lead", loc)), " ",
                  ui_str("hfv_scenario_concl_A_body", loc)
                )
              } else if (identical(code, "B")) {
                tagList(
                  tags$b(ui_str("hfv_scenario_concl_B_lead", loc)), " ",
                  ui_str("hfv_scenario_concl_B_body", loc)
                )
              } else if (identical(code, "C")) {
                tagList(
                  tags$b(ui_str("hfv_scenario_concl_C_lead", loc)), " ",
                  ui_str("hfv_scenario_concl_C_body", loc), " ",
                  tags$b(ui_str("hfv_scenario_concl_C_emph", loc))
                )
              } else if (identical(code, "D")) {
                tagList(
                  tags$b(ui_str("hfv_scenario_concl_D_lead", loc)), " ",
                  ui_str("hfv_scenario_concl_D_body", loc)
                )
              } else {
                ui_str("hfv_scenario_concl_other", loc)
              }
            }
            make_concl_callout <- function(scope_label, code, accent_col) {
              tags$div(
                class = "ynow-hfv-scenario-concl",
                style = paste0(
                  "margin:10px 0 0 0;padding:10px 12px;background:#f5f5f5;",
                  "border-left:4px solid ", accent_col, ";",
                  "border-radius:0 4px 4px 0;font-size:13px;line-height:1.55;color:#333;"
                ),
                tags$div(
                  style = "margin:0 0 4px 0;font-size:12px;color:#555;",
                  tags$b(scope_label),
                  tags$span(style = "margin:0 6px;color:#bbb;", "|"),
                  tags$span(style = paste0("color:", accent_col, ";"), sc_lab(code))
                ),
                tags$div(make_concl_body(code))
              )
            }
            lead_c <- as.character(sc$most_frequent %||% NA_character_)[1]
            if (!nzchar(lead_c) || identical(lead_c, "NA")) lead_c <- NA_character_
            # Fallback if older summarize payload lacks most_frequent
            if (is.na(lead_c) && !is.null(lead_code) && !is.na(lead_code)) {
              lead_c <- as.character(lead_code)[1]
            }
            latest_c <- as.character(sc$latest %||% NA_character_)[1]
            if (!nzchar(latest_c) || identical(latest_c, "NA")) {
              # Fallback: last pair in attached pairs frame
              pp_sc <- sc$pairs
              if (!is.null(pp_sc) && is.data.frame(pp_sc) && nrow(pp_sc) > 0L &&
                  "scenario" %in% names(pp_sc)) {
                latest_c <- as.character(pp_sc$scenario[nrow(pp_sc)])[1]
              } else {
                latest_c <- NA_character_
              }
            }
            d0 <- tryCatch(as.Date(sc$latest_date), error = function(e) as.Date(NA))
            d1 <- tryCatch(as.Date(sc$latest_date_next), error = function(e) as.Date(NA))
            lead_n <- suppressWarnings(as.integer(sc$most_frequent_n %||% NA_integer_)[1])
            if (!is.finite(lead_n) || lead_n < 1L) {
              lead_n <- if (!is.na(lead_c) && lead_c %in% abcd) {
                as.integer(n_abcd[match(lead_c, abcd)])
              } else {
                0L
              }
            }
            lead_label <- if (!is.na(lead_c) && lead_c %in% abcd) {
              sprintf(
                ui_str("hfv_scenario_concl_lead_fmt", loc),
                sc_lab(lead_c),
                lead_n
              )
            } else {
              ui_str("hfv_scenario_concl_scope_lead", loc)
            }
            latest_label <- if (is.finite(d0) && is.finite(d1)) {
              sprintf(
                ui_str("hfv_scenario_concl_latest_fmt", loc),
                format(d0, "%Y-%m-%d"),
                format(d1, "%Y-%m-%d")
              )
            } else {
              ui_str("hfv_scenario_concl_latest_nodate", loc)
            }
            diverge_q3 <- !is.na(lead_c) && !is.na(latest_c) && !identical(lead_c, latest_c)
            tagList(
              if (!is.na(lead_c) && lead_c %in% abcd) {
                make_concl_callout(lead_label, lead_c, sc_color(lead_c))
              } else {
                tags$div(
                  class = "ynow-hfv-scenario-concl",
                  style = paste0(
                    "margin:10px 0 0 0;padding:10px 12px;background:#f5f5f5;",
                    "border-left:4px solid #6c757d;",
                    "border-radius:0 4px 4px 0;font-size:12.5px;line-height:1.5;color:#555;"
                  ),
                  tags$b(ui_str("hfv_scenario_concl_scope_lead", loc)),
                  tags$span(style = "margin:0 6px;color:#bbb;", "|"),
                  ui_str("hfv_scenario_concl_none_abcd", loc)
                )
              },
              if (!is.na(latest_c)) {
                make_concl_callout(latest_label, latest_c, sc_color(latest_c))
              } else NULL,
              if (isTRUE(diverge_q3)) {
                tags$div(
                  style = paste0(
                    "margin:8px 0 0 0;padding:8px 10px;background:#fff8e8;",
                    "border:1px solid #f0d78c;border-radius:4px;",
                    "font-size:12px;line-height:1.5;color:#555;"
                  ),
                  ui_str("hfv_scenario_concl_diverge", loc)
                )
              } else NULL,
              tags$div(
                style = "margin:8px 0 0 0;color:#888;font-size:11.5px;line-height:1.45;",
                ui_str("hfv_scenario_concl_note", loc)
              )
            )
          },
          if (n_other > 0L) {
            tags$div(
              style = "margin:8px 0 0 0;color:#666;font-size:12px;",
              sprintf(
                ui_str("hfv_scenario_other_line", loc),
                pct(p_other),
                n_other
              )
            )
          } else NULL
          # Method-block notes cover thresholds / FV quality; avoid a second caveat here.
        )
      } else {
        tags$div(
          style = paste0(
            "margin:0 0 10px 0;padding:12px 14px;background:#fafafa;",
            "border:1px solid #ddd;border-left:4px solid ", border, ";border-radius:6px;",
            "font-size:13px;line-height:1.55;color:#666;"
          ),
          ui_str("hfv_sum_scenario_empty", loc)
        )
      }
    }

    # 結果附註／fallback：全寬置於兩欄下方
    fb <- s$fallbacks
    notes_ui <- tagList()
    fv_meta <- tryCatch(bt_hfv_fv(), error = function(e) NULL)
    srcs <- if (!is.null(fv_meta)) fv_meta$hfv_data_sources else NULL
    data_notes <- if (!is.null(fv_meta)) fv_meta$hfv_data_notes else NULL
    if ((!is.null(srcs) && length(srcs) > 0) || (!is.null(data_notes) && length(data_notes) > 0)) {
      notes_ui <- tagAppendChild(
        notes_ui,
        tags$div(
          style = "margin:0 0 8px 0;padding:0;font-size:12px;",
          tags$div(tags$b(ui_str("hfv_data_sources_label", loc))),
          if (!is.null(srcs) && length(srcs) > 0) {
            tags$div(style = "margin-top:4px;", paste(srcs, collapse = " · "))
          } else NULL,
          if (!is.null(data_notes) && length(data_notes) > 0) {
            tags$ul(
              style = "margin:6px 0 0 0;padding-left:18px;color:#555;",
              lapply(data_notes, function(n) tags$li(n))
            )
          } else NULL
        )
      )
    }
    if (nzchar(s$note %||% "")) {
      notes_ui <- tagAppendChild(
        notes_ui,
        tags$p(style = "margin:0 0 6px 0;color:#666;font-size:12px;", s$note)
      )
    }
    if (!is.null(mo) && is.list(mo) && nzchar(mo$note %||% "")) {
      notes_ui <- tagAppendChild(
        notes_ui,
        tags$p(style = "margin:0 0 6px 0;color:#666;font-size:12px;", mo$note)
      )
    }
    if (!is.null(fb) && isTRUE(fb$any_fallback) && !is.null(fb$items) && nrow(fb$items) > 0) {
      item_fmt <- ui_str("hfv_fb_item_fmt", loc)
      notes_ui <- tagAppendChild(
        notes_ui,
        tags$div(
          style = "margin:8px 0 0 0;padding:0;font-size:12.5px;line-height:1.55;",
          tags$div(tags$b(ui_str("hfv_fb_title", loc))),
          tags$ul(
            style = "margin:6px 0 0 0;padding-left:18px;",
            lapply(seq_len(nrow(fb$items)), function(i) {
              row <- fb$items[i, , drop = FALSE]
              tags$li(sprintf(
                item_fmt,
                row$label[1], row$src_label[1], as.integer(row$n_rows[1])
              ))
            })
          )
        )
      )
    } else if (!is.null(fb) && !isTRUE(fb$any_fallback) && nzchar(fb$note %||% "")) {
      notes_ui <- tagAppendChild(
        notes_ui,
        tags$p(style = "margin:6px 0 0 0;color:#666;font-size:12px;", fb$note)
      )
    }

    # Expanding-window next actual market-price direction forecast (MOS conditions P(up)/P(down) on hist_price R)
    tip_fc <- s$direction_tip
    forecast_card <- {
      if (!is.null(tip_fc) && is.list(tip_fc) && is.finite(tip_fc$p_up_hat)) {
        method_lab <- switch(
          as.character(tip_fc$method %||% "none")[1],
          mos_bucket = ui_str("hfv_fc_method_bucket", loc),
          blend = ui_str("hfv_fc_method_blend", loc),
          unconditional = ui_str("hfv_fc_method_base", loc),
          ui_str("hfv_fc_method_none", loc)
        )
        d_tip <- tryCatch(as.Date(tip_fc$Date), error = function(e) as.Date(NA))
        date_txt <- if (is.finite(d_tip)) format(d_tip, "%Y-%m-%d") else "—"
        tags$div(
          style = paste0(
            "margin:0 0 14px 0;padding:12px 14px;background:#eef4fb;",
            "border:1px solid #c5d4ef;border-left:4px solid #3c8dbc;",
            "border-radius:6px;font-size:13px;line-height:1.55;"
          ),
          tags$div(tags$b(ui_str("hfv_fc_title", loc))),
          tags$div(
            style = "margin:4px 0 0 0;color:#6c757d;font-size:11.5px;",
            paste(ui_str("hfv_fc_formula", loc), ui_str("hfv_fc_caveat", loc), sep = " ")
          ),
          tags$div(
            style = "margin:6px 0 0 0;font-size:15px;font-weight:700;",
            sprintf(
              ui_str("hfv_fc_tip_fmt", loc),
              date_txt,
              pct(tip_fc$p_up_hat),
              pct(tip_fc$p_down_hat)
            )
          ),
          tags$ul(
            style = "margin:6px 0 0 0;padding-left:18px;font-size:12.5px;",
            tags$li(sprintf(
              ui_str("hfv_fc_mos_fmt", loc),
              if (is.finite(tip_fc$mos)) 100 * tip_fc$mos else NA_real_,
              tip_fc$bucket %||% "—",
              as.integer(tip_fc$n_bucket %||% 0L),
              as.integer(tip_fc$n_prior %||% 0L)
            )),
            tags$li(paste0(ui_str("hfv_fc_method_label", loc), "：", method_lab)),
            if (is.finite(tip_fc$p_up_base)) {
              tags$li(sprintf(ui_str("hfv_fc_base_fmt", loc), pct(tip_fc$p_up_base)))
            } else NULL
          ),
          if (nzchar(tip_fc$note %||% "")) {
            tags$div(
              style = "margin:6px 0 0 0;color:#555;font-size:11.5px;line-height:1.45;",
              tip_fc$note
            )
          } else NULL,
          if (isTRUE(tip_fc$small_sample)) {
            tags$div(
              style = "margin:4px 0 0 0;color:#c27d0e;font-size:11.5px;",
              ui_str("hfv_badge_small_sample", loc)
            )
          } else NULL
        )
      } else {
        tags$div(
          style = paste0(
            "margin:0 0 14px 0;padding:12px 14px;background:#fafafa;",
            "border:1px solid #ddd;border-left:4px solid #3c8dbc;",
            "border-radius:6px;font-size:13px;line-height:1.55;color:#666;"
          ),
          tags$div(tags$b(ui_str("hfv_fc_title", loc))),
          tags$div(
            style = "margin:4px 0 0 0;",
            ui_str("hfv_fc_empty", loc)
          )
        )
      }
    }

    investor_block <- tagList(
      conclusion_card,
      forecast_card,
      tags$div(
        style = "margin:0 0 8px 0;font-size:13px;",
        if (isTRUE(s$small_sample) || isTRUE(s$no_strategy_fv)) {
          tagList(
            tags$b(ui_str("hfv_sum_title", loc)),
            if (isTRUE(s$small_sample)) {
              tags$span(style = "color:#c27d0e;margin-left:8px;", ui_str("hfv_badge_small_sample", loc))
            } else NULL,
            if (isTRUE(s$no_strategy_fv)) {
              tags$span(style = "color:#c27d0e;margin-left:8px;", ui_str("hfv_badge_no_strategy_fv", loc))
            } else NULL
          )
        } else NULL
      ),
      if (length(notes_ui) > 0) {
        tags$div(
          style = "margin:8px 0 0 0;padding:10px 12px;background:#fafafa;border:1px dashed #ccc;border-radius:6px;",
          tags$div(
            style = "font-size:12px;font-weight:700;color:#666;margin-bottom:4px;",
            ui_str("hfv_sum_notes", loc)
          ),
          notes_ui
        )
      } else NULL
    )
    list(
      empty = NULL,
      investor = investor_block,
      price = price_card,
      fv = fv_card,
      scenario = scenario_card
    )
  })

  output$bt_hfv_investor_summary <- renderUI({
    parts <- bt_hfv_findings_parts()
    parts$investor
  })
  # Back-compat alias (older bookmarks / tests may still bind this id)
  output$bt_fv_conv_summary <- renderUI({
    parts <- bt_hfv_findings_parts()
    parts$investor
  })
  output$bt_hfv_price_findings <- renderUI({
    parts <- bt_hfv_findings_parts()
    parts$price
  })
  output$bt_hfv_fv_findings <- renderUI({
    parts <- bt_hfv_findings_parts()
    parts$fv
  })
  output$bt_hfv_scenario_findings <- renderUI({
    parts <- bt_hfv_findings_parts()
    parts$scenario
  })

  output$bt_fv_conv_table <- renderTable({
    s <- bt_fv_conv()
    loc <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
    shiny::validate(shiny::need(
      !is.null(s) && !is.null(s$pairs) && nrow(s$pairs) > 0,
      if (isTRUE(s$no_strategy_fv)) {
        ui_str("hfv_table_need_replay", loc)
      } else {
        ui_str("hfv_table_no_pairs", loc)
      }
    ))
    pp <- s$pairs
    gapv <- if ("gap_next" %in% names(pp)) pp$gap_next else (pp$price_next - pp$fair_value) / pp$fair_value
    retv <- if ("ret_next" %in% names(pp)) {
      pp$ret_next
    } else {
      (pp$price_next - pp$price) / pp$price
    }
    dirp <- if ("dir_price" %in% names(pp)) {
      pp$dir_price
    } else {
      ifelse(!is.finite(retv), NA_character_,
             ifelse(retv > 0, "漲", ifelse(retv < 0, "跌", "平")))
    }
    dir_disp <- vapply(as.character(dirp), function(x) {
      if (is.na(x) || !nzchar(x)) return("—")
      if (identical(x, "漲")) return(ui_str("hfv_dir_up", loc))
      if (identical(x, "跌")) return(ui_str("hfv_dir_down", loc))
      if (identical(x, "平")) return(ui_str("hfv_dir_flat", loc))
      x
    }, character(1))
    fb_lab <- if ("fallback_keys" %in% names(pp)) {
      ifelse(is.na(pp$fallback_keys) | !nzchar(pp$fallback_keys), "—", pp$fallback_keys)
    } else {
      rep("—", nrow(pp))
    }
    sc_pairs <- if (!is.null(s$scenarios) && is.list(s$scenarios) &&
                    !is.null(s$scenarios$pairs) && is.data.frame(s$scenarios$pairs)) {
      s$scenarios$pairs
    } else {
      NULL
    }
    sc_code <- rep(NA_character_, nrow(pp))
    if (!is.null(sc_pairs) && nrow(sc_pairs) > 0L &&
        all(c("Date", "scenario") %in% names(sc_pairs))) {
      ix <- match(pp$Date, sc_pairs$Date)
      sc_code <- as.character(sc_pairs$scenario[ix])
    }
    sc_lab <- vapply(sc_code, function(code) {
      if (is.na(code) || !nzchar(code)) return("—")
      key <- switch(
        code,
        A = "hfv_scenario_A",
        B = "hfv_scenario_B",
        C = "hfv_scenario_C",
        D = "hfv_scenario_D",
        other = "hfv_scenario_other",
        "hfv_scenario_other"
      )
      ui_str(key, loc)
    }, character(1))
    vs_disp <- vapply(as.character(pp$vs_fv), function(x) {
      if (is.na(x) || !nzchar(x)) return("—")
      if (identical(x, "之上")) return(ui_str("hfv_vs_above", loc))
      if (identical(x, "之下")) return(ui_str("hfv_vs_below", loc))
      if (identical(x, "持平")) return(ui_str("hfv_vs_flat", loc))
      x
    }, character(1))
    toward_raw <- if ("outcome" %in% names(pp)) {
      as.character(pp$outcome)
    } else {
      rep(NA_character_, nrow(pp))
    }
    toward_disp <- vapply(toward_raw, function(x) {
      if (is.na(x) || !nzchar(x)) return("—")
      if (identical(x, "趨近")) return(ui_str("hfv_toward_toward", loc))
      if (identical(x, "遠離")) return(ui_str("hfv_toward_away", loc))
      if (identical(x, "持平")) return(ui_str("hfv_toward_flat", loc))
      x
    }, character(1))
    fc_df <- s$direction_forecasts
    p_up_hat_disp <- rep("—", nrow(pp))
    if (!is.null(fc_df) && is.data.frame(fc_df) && nrow(fc_df) > 0L &&
        all(c("Date", "p_up_hat") %in% names(fc_df))) {
      ix_fc <- match(pp$Date, fc_df$Date)
      p_hat <- fc_df$p_up_hat[ix_fc]
      p_up_hat_disp <- ifelse(
        is.finite(p_hat),
        paste0(sprintf("%.0f", 100 * p_hat), "%"),
        "—"
      )
    }
    out <- data.frame(
      col1 = format(pp$Date, "%Y-%m-%d"),
      col2 = format(pp$Date_next, "%Y-%m-%d"),
      col3 = round(pp$price, 2),
      col4 = round(pp$fair_value, 2),
      col5 = round(pp$price_next, 2),
      col6 = paste0(sprintf("%+.1f", 100 * retv), "%"),
      col7 = dir_disp,
      col8 = p_up_hat_disp,
      col9 = toward_disp,
      col10 = paste0(sprintf("%+.1f", 100 * gapv), "%"),
      col11 = vs_disp,
      col12 = sc_lab,
      col13 = fb_lab,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(out) <- c(
      ui_str("hfv_col_date", loc),
      ui_str("hfv_col_next_date", loc),
      ui_str("hfv_col_price", loc),
      ui_str("hfv_col_fv", loc),
      ui_str("hfv_col_next_price", loc),
      ui_str("hfv_col_next_ret", loc),
      ui_str("hfv_col_dir", loc),
      ui_str("hfv_col_p_up_hat", loc),
      ui_str("hfv_col_toward", loc),
      ui_str("hfv_col_gap", loc),
      ui_str("hfv_col_vs_fv", loc),
      ui_str("hfv_col_scenario", loc),
      ui_str("hfv_col_fallback", loc)
    )
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  output$bt_fv_conv_plot <- renderPlotly({
    s <- bt_fv_conv()
    loc <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
    empty <- plotly::plotly_empty() %>%
      plotly::layout(annotations = list(list(
        text = ui_str("hfv_plot_need_data", loc), showarrow = FALSE, font = list(size = 14, color = "#888")
      )))
    if (is.null(s) || is.null(s$pairs) || nrow(s$pairs) < 1) return(empty)
    pp <- s$pairs
    pp$gap_pct <- if ("gap_next" %in% names(pp)) {
      pp$gap_next
    } else {
      (pp$price_next - pp$fair_value) / pp$fair_value
    }
    cols <- ifelse(pp$vs_fv == "之上", "#00a65a",
            ifelse(pp$vs_fv == "之下", "#dd4b39", "#999"))
    plotly::plot_ly(
      pp, x = ~Date, y = ~gap_pct, type = "bar",
      text = ~paste0(Date, " → ", Date_next, "<br>", vs_fv,
                     "<br>幅度 ", sprintf("%+.1f%%", 100 * gap_pct)),
      hoverinfo = "text",
      marker = list(color = cols)
    ) %>%
      plotly::layout(
        xaxis = list(title = "估值日"),
        yaxis = list(title = "(P下一期 − FV) / FV", tickformat = ".0%"),
        margin = list(l = 50, r = 20, t = 20, b = 40),
        shapes = list(list(
          type = "line", x0 = min(pp$Date), x1 = max(pp$Date), y0 = 0, y1 = 0,
          line = list(dash = "dot", color = "#aaa")
        ))
      )
  })

  output$bt_fv_edge <- renderUI({
    v <- bt_validation()
    if (is.null(v) || is.null(v$fv)) return(tags$p(style="color:#888;font-size:12px;", .ui_msg("bt_fv_question")))
    tags$p(style = "font-size:13px;line-height:1.55;", tags$b("結論："), v$fv$answer)
  })

  output$bt_fv_table <- renderTable({
    v <- bt_validation()
    shiny::validate(shiny::need(!is.null(v) && !is.null(v$fv) && !is.null(v$fv$table), "尚無 FV 驗證表"))
    tab <- v$fv$table
    data.frame(
      組別 = tab$group,
      樣本數 = tab$n,
      `1Y` = ifelse(is.na(tab$ret_1y), NA, sprintf("%.1f%%", 100 * tab$ret_1y)),
      `3Y` = ifelse(is.na(tab$ret_3y), NA, sprintf("%.1f%%", 100 * tab$ret_3y)),
      `5Y` = ifelse(is.na(tab$ret_5y), NA, sprintf("%.1f%%", 100 * tab$ret_5y)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$perf_metrics <- renderUI({
    res <- bt_result()
    view <- tryCatch(bt_nav_view(), error = function(e) NULL)
    if (is.null(res) || is.null(res$metrics)) {
      return(
        tags$div(
          style = "color: #888; font-size: 12.5px; line-height: 1.55;",
          icon("chart-bar"),
          " 執行回測後，此處會以卡片顯示 ",
          tags$b("Sharpe"), "、", tags$b("Max DD"), "、",
          tags$b("CAGR"), "、", tags$b("Excess vs BH"), "、",
          tags$b("Jensen α"), "。"
        )
      )
    }
    win_lab <- if (!is.null(view) && !is.null(view$label)) view$label else "全部"
    eq_win <- if (!is.null(view) && !is.null(view$equity_df)) view$equity_df else res$equity_df
    m <- nav_perf_metrics(eq_win)
    m$market_pricing_bias <- res$metrics$market_pricing_bias
    best <- m$best
    label_best <- if (identical(best, "A")) "基本面策略淨值" else "情緒策略淨值"
    sharpe_show <- if (identical(best, "A")) m$sharpe_a else m$sharpe_b
    mdd_show <- if (identical(best, "A")) m$mdd_a else m$mdd_b
    cagr_show <- if (identical(best, "A")) m$cagr_a else m$cagr_b
    sharpe_a_txt <- if (is.na(m$sharpe_a)) "N/A" else sprintf("%.2f", m$sharpe_a)
    sharpe_b_txt <- if (is.na(m$sharpe_b)) "N/A" else sprintf("%.2f", m$sharpe_b)

    rf_ann <- tryCatch({
      r <- as.numeric(cached_get_risk_free_rate())
      if (is.finite(r) && r > 0) r / 100 else 0.04
    }, error = function(e) 0.04)
    alpha_df <- tryCatch(compute_alpha_dashboard(eq_win, rf_annual = rf_ann),
                         error = function(e) NULL)
    pick_alpha <- function(series) {
      if (is.null(alpha_df) || nrow(alpha_df) == 0) return(NULL)
      hit <- alpha_df[alpha_df$Series == series, , drop = FALSE]
      if (nrow(hit) == 0) NULL else hit[1, ]
    }
    a_row <- pick_alpha("StrategyA")
    b_row <- pick_alpha("StrategyB")
    bh_row <- pick_alpha("BuyHold")
    best_row <- if (identical(best, "A")) a_row else b_row
    excess_show <- if (!is.null(best_row)) best_row$ExcessReturn else NA_real_
    jensen_show <- if (!is.null(best_row)) best_row$JensenAlpha else NA_real_
    if (!is.null(best_row) && is.finite(best_row$CAGR)) cagr_show <- best_row$CAGR

    .ynow_metric_card <- function(value, label, caption, icon_name, tone, tip) {
      tipify(
        tags$div(
          class = paste0("ynow-metric-card ynow-metric-card--", tone),
          tags$div(
            class = "ynow-metric-card__body",
            tags$div(
              class = "ynow-metric-card__top",
              tags$span(class = "ynow-metric-card__icon", icon(icon_name)),
              tags$p(class = "ynow-metric-card__label", label)
            ),
            tags$div(class = "ynow-metric-card__value", value),
            tags$p(class = "ynow-metric-card__caption", caption)
          )
        ),
        tip,
        placement = "bottom"
      )
    }

    tagList(
      tags$p(
        style = "margin: 0 0 12px 0; font-size: 12px; color: #666;",
        "以下對應淨值圖累積區間「", win_lab, "」。以 Sharpe 較高的策略為主顯示（", label_best, "）；A＝", sharpe_a_txt,
        "，B＝", sharpe_b_txt, "。已整併 Alpha（Excess／Jensen α）。"
      ),
      tags$div(
        id = "bt_perf_metrics_boxes",
        class = "ynow-metric-grid",
        .ynow_metric_card(
          value = if (is.na(sharpe_show)) "N/A" else sprintf("%.2f", sharpe_show),
          label = paste0("Sharpe 比率（較佳：", label_best, "）"),
          caption = "風險調整後報酬；>1 通常視為不錯，>2 屬優異（依市場而異）。",
          icon_name = "chart-line",
          tone = "green",
          tip = "年化 Sharpe ≈ 日報酬均值 ÷ 標準差 × √252。"
        ),
        .ynow_metric_card(
          value = if (is.na(mdd_show)) "N/A" else paste0(sprintf("%.1f", mdd_show * 100), "%"),
          label = paste0("最大回撤 Max DD（", label_best, "）"),
          caption = "歷史最大虧損幅度；愈接近 0 代表回撤愈小。",
          icon_name = "arrow-down",
          tone = "red",
          tip = "淨值自歷史高點回落的最大百分比幅度。"
        ),
        .ynow_metric_card(
          value = if (is.na(cagr_show)) "N/A" else paste0(sprintf("%.1f", cagr_show * 100), "%"),
          label = paste0("CAGR（", label_best, "）"),
          caption = sprintf(
            "Buy&Hold CAGR＝%s。用來對照策略成長速度。",
            if (is.null(bh_row) || is.na(bh_row$CAGR)) "N/A" else paste0(sprintf("%.1f", bh_row$CAGR * 100), "%")
          ),
          icon_name = "percentage",
          tone = "blue",
          tip = "年化複合成長率（CAGR）。"
        ),
        .ynow_metric_card(
          value = if (is.na(excess_show)) "N/A" else paste0(sprintf("%+.1f", excess_show * 100), "%"),
          label = paste0("Excess vs BH（", label_best, "）"),
          caption = "相對 Buy & Hold 的超額報酬；>0 代表創造價值。",
          icon_name = "balance-scale",
          tone = "amber",
          tip = "策略期末報酬 − Buy&Hold 期末報酬。"
        ),
        .ynow_metric_card(
          value = if (is.na(jensen_show)) "N/A" else paste0(sprintf("%+.1f", jensen_show * 100), "%"),
          label = paste0("Jensen α（", label_best, "）"),
          caption = "相對大盤基準的風險調整超額報酬（年化）。",
          icon_name = "rocket",
          tone = "blue",
          tip = "對 Benchmark 日超額報酬做 CAPM 回歸後的年化截距。"
        )
      )
    )
  })

  # ==========================================
  # 11. 系統按鈕與報告輸出
  # ==========================================
  # DDM Reset 由 ddm_module_server 內的 input$reset_ddm 處理（ns: mod_ddm）

  output$bt_methodology_notes <- renderUI({
    tags$div(
      style = "font-size: 12.5px; line-height: 1.65; color: #333;",
      tags$p(
        style = "margin-top:0;",
        tags$b("折現圖 vs 淨值圖："),
        "折現圖（側邊「歷史基本面驗證」）是每股合理價 vs 實際股價；策略 MOS／部位用「復盤模型」單選。",
        "淨值圖（本頁「量化回測實驗室」）是部位×日報酬的累積財富（起始＝1）——",
        tags$b("基本面策略淨值"), "＝Exp_A；",
        tags$b("情緒策略淨值"), "＝Exp_B（Exp_A 混入動能／RSI）。兩圖標籤不可互換。"
      ),
      tags$h5(tags$b("一、折現比較圖（合理價 vs 實際股價｜側邊「歷史基本面驗證」）")),
      tags$ul(
        tags$li(tags$b("實際股價："), "該股歷史收盤（Yahoo 調整後）。搜尋後即預覽股價，不必先勾模型。"),
        tags$li(tags$b("大盤："), "圖上方「顯示大盤」開關疊加基準（預設 SPY，右軸）；與合理價無關。"),
        tags$li(tags$b("圖表模型："), "圖上方可複選疊多條合理價線（預設不勾選）。"),
        tags$li(tags$b("復盤模型："), "驗證區「設定」內單選；機率／幅度／下期上漲頻率與策略 FV／MOS 僅依此模型（非圖表複選平均）。"),
        tags$li(
          tags$b("PIT DCF（優先邊際路徑）："),
          "歷史點若有營收與 NOPAT/D&A/CapEx/ΔNWC 邊際，走與 Live 相同的 unit FCFF 路徑再折現；",
          "否則退回 ", tags$code("FCFF_t = FCF0×(1+g)^t"), "＋Gordon。",
          "財報可用條件：財報年 ≤ 日曆年−1，且 period_end＋約 90 日 ≤ 估值日（Yahoo 重編風險仍在）。",
          "僅折線末端最新點才掛目前 APP 分頁（含 FCFE／二階段 DDM）與 Session Rm。"
        ),
        tags$li(tags$b("策略 MOS／部位："), "用「復盤模型」單選之合理價，不是圖表複選平均。未選復盤模型＝不套用策略 FV／MOS（不暗設 DCF）。"),
        tags$li(
          tags$b("歷史基本面驗證（非策略回測）："),
          "同時報告（1）實際市價下期漲跌 ", tags$code("R=(P_{t+1}-P_t)/P_t"),
          " 經驗頻率，以及以復盤估值×歷史實際股價（MOS）做成的可衡量時刻「下期實際市價」漲跌機率",
          "（擴張窗因果：僅 Date下一期≤tip，嚴格禁止未來資料回推；非預測估值／FV）；",
          "（2）相對復盤理論估值 FV_t（＝復盤模型單選；結果隨復盤模型而變）縮小／擴大與幅度 (P−FV)/FV。",
          "市價漲跌與估值落點是兩套定義，不可混稱。預設只計已實現下期，可選擴張窗樣本外命中率。",
          "若歷史點套用 APP_DEFAULTS／Session／法定稅率，摘要會列出預設／fallback 與對應分頁。",
          "此區塊在側邊「歷史基本面驗證」，與本頁策略淨值交易回測分開閱讀。"
        ),
        tags$li(
          tags$b("歷史各點 vs 末端："),
          "歷史點用當時可得財報、^TNX Rf、截至該日基準已實現 Rm、Rolling β、當日 We/Wd；",
          "g 由截至該年營收／NI／FCF 成長推估（缺則 APP_DEFAULTS SGR；clamp＜折現率）；",
          "預測年數 n 於歷史點固定 APP_DEFAULTS$years；Rd／稅率優先 Interest/Debt、Tax/Pretax；",
          "P/B 用 Justified (ROE−g)/(Ke−g)（缺則 APP_DEFAULTS pb_mid）。僅折線末端最新點掛勾目前 APP 分頁與 Session Rm。"
        ),
        tags$li(
          tags$b("ADR／雙重股權："),
          "股數依目前市值÷股價對齊報價股數後再算合理價（倍率固定套用各財年）。"
        ),
        tags$li(
          tags$b("歷史基本面驗證區塊："),
          "同上：機率＋幅度；與本頁「策略淨值」交易回測分開閱讀。"
        )
      ),
      tags$h5(tags$b("二、資料來源")),
      tags$ul(
        tags$li(tags$b("股價／基準："), "Yahoo Finance（yfinance，auto_adjust）；基準預設 SPY。"),
        tags$li(
          tags$b("財報（PIT 近似）："),
          "本次 Session 已載入之年度 IS／BS／CF；估值日只用 fund_year ≤ 日曆年−1，",
          "且 period_end＋約 90 日 ≤ 估值日。Yahoo 可能為重編，非 as-filed SEC。"
        ),
        tags$li(tags$b("Rf（歷史點）："), "再平衡日 ^TNX 當時收盤；抓不到才用 Session／約 4%。"),
        tags$li(
          tags$b("Rm（歷史點）："),
          "截至再平衡日的基準（預設 SPY）已實現年化總報酬（優先近 12 個月，無前瞻）；不是 Session 預期溢酬。末端才用 Session Rm。"
        ),
        tags$li(tags$b("Rolling β："), "各再平衡日以標的 vs SPY 約 60 個月月報酬估計。"),
        tags$li(tags$b("We／Wd（歷史點）："), "再平衡日 股數×收盤 與當時 Total Debt。"),
        tags$li(tags$b("Rd／Tax（歷史點）："), "有 Interest／Debt、Tax／Pretax 則用當時值，否則 Session。"),
        tags$li(
          tags$b("評價假設："),
          "歷史點 g／P/B 由當時財報推估；Ke／WACC 於各季以 Rolling β＋當年 Rf／結構重估。末端才掛目前分頁與 Session Rm。"
        )
      ),
      tags$h5(tags$b("三、計算過程（依分析頻率 PIT）")),
      tags$ol(
        tags$li("再平衡日：fund_year ≤ 日曆年−1 重建各模型合理價；策略 FV＝復盤模型單選（未選＝NA） → MOS＝(FV−Price)/FV。"),
        tags$li("持倉回測條件未過 → Exp_A = Exp_B = 0（兩模式皆空手）。"),
        tags$li("通過則 Exp_A 依 MOS 滯後映射；Exp_B = (1−blend)×Exp_A + blend×(sentiment×max_exp)。"),
        tags$li("每日：策略淨值用 Exp×日報酬；Buy&Hold 滿持股；現金報酬=0；未扣交易成本。"),
        tags$li("比較視窗自首次有效季再平衡對齊。")
      ),
      tags$h5(tags$b("四、相對 Buy & Hold")),
      tags$p(
        style = "margin-bottom:0;",
        "上漲日把 (1−Exp_A)×r 加總，拆成現金拖累／提前出場／高估減碼；加總 ≠ 複利終值落差（殘差＝終值差−加總）。",
        "結論以該次回測頁上的平均部位、空手日、Filter 未過次數為準，不作「牛市必輸」套話。"
      )
    )
  })
  
  observeEvent(input$reset_dcf, {
    updateRadioButtons(session, "dcf_mode", selected = APP_DEFAULTS$dcf_mode)
    updateRadioButtons(session, "dcf_claim", selected = APP_DEFAULTS$dcf_claim)
    updateNumericInput(session, "years", value = APP_DEFAULTS$years)
    updateSelectInput(session, "perpetual_g_method", selected = APP_DEFAULTS$perpetual_g_method)
    updateSelectInput(session, "lifecycle_stage", selected = APP_DEFAULTS$lifecycle_stage)
    updateNumericInput(session, "wacc_gordon", value = APP_DEFAULTS$wacc_gordon)
    updateNumericInput(session, "yr_stage1", value = APP_DEFAULTS$yr_stage1)
    eg_reset <- suppressWarnings(as.numeric(isolate(estimated_g()))[1])
    updateNumericInput(
      session, "g_stage1",
      value = if (is.finite(eg_reset)) round(eg_reset, 2) else APP_DEFAULTS$g_stage1
    )
    updateNumericInput(session, "wacc_stage1", value = APP_DEFAULTS$wacc_gordon)
    updateNumericInput(session, "wacc_stage2", value = APP_DEFAULTS$wacc_gordon)
    # 依當前方法重算 g（勿寫死舊 SGR）
    est <- tryCatch(isolate(central_perpetual_g()), error = function(e) NULL)
    if (is.null(est) || !is.finite(est$g_pct)) {
      updateNumericInput(session, "sgr", value = APP_DEFAULTS$sgr)
    } else {
      .push_perpetual_g(est, notify_two_stage = FALSE)
    }
    showNotification(.ui_msg("notif_dcf_defaults"), type = "message")
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      tk <- tryCatch({
        display_ticker_for_market(current_ticker(), market_mode())
      }, error = function(e) "NA")
      if (is.null(tk) || !nzchar(as.character(tk))) tk <- "NA"
      mm <- tryCatch(normalize_market_mode(isolate(market_mode())), error = function(e) "US")
      ed <- if (isTRUE(tryCatch(isolate(lite_mode()), error = function(e) FALSE))) "Lite" else "Full"
      paste0("YNow_Report_", mm, "_", ed, "_", tk, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      if (isTRUE(.ynow_access_deny_download("report:download_report", file))) return()
      pdf_ticket <- .ynow_pdf_authorize(file)
      if (is.null(pdf_ticket)) return()
      tryCatch({
        showNotification(.ui_msg("notif_pdf_busy"), type = "message", duration = 8)
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)

        plot_path <- NA_character_
        if (exists("fcf_results") && !is.null(fcf_results$fcf_plot_obj())) {
          plot_path <- file.path(tempdir(), "fcf_plot_temp.png")
          ggsave(plot_path, plot = fcf_results$fcf_plot_obj(), width = 9, height = 5.5, dpi = 200)
        }

        cur_price <- .report_num(tryCatch(isolate(scraped_market_cap()$price), error = function(e) NA))
        dcf_price <- .report_num(isolate(stock_price_estimate_val()))
        ddm_val <- .report_num(tryCatch(isolate(ddm_results$ddm_price()), error = function(e) NA))
        pb_val <- .report_num(tryCatch(isolate(pb_results$pb_price()), error = function(e) NA))
        ri_val <- .report_num(tryCatch(isolate(ri_results$ri_price()), error = function(e) NA))
        ev_val <- .report_num(isolate(dcf_value_result()))

        ind_text_early <- isolate(corp_industry_text())
        rec_full <- tryCatch(
          recommend_valuation_models(
            isolate(d_cash_flow()),
            industry_text = ind_text_early,
            d_is = isolate(d_income_statement()),
            d_bs = isolate(d_balance_sheet()),
            industry_choice = isolate(input$industry_choice),
            wacc_pct = isolate(tryCatch(.current_wacc_pct(), error = function(e) NA_real_)),
            lifecycle_result = isolate(tryCatch(central_lifecycle(), error = function(e) NULL)),
            selected_stage = isolate(input$lifecycle_stage %||% "auto")
          ),
          error = function(e) NULL
        )
        val_method <- if (!is.null(rec_full)) {
          list(method = rec_full$summary_method, rationale = rec_full$reason)
        } else {
          derive_valuation_method(isolate(d_cash_flow()), industry_text = ind_text_early)
        }

        band <- tryCatch(isolate(primary_valuation_band()), error = function(e) NULL)
        sec_pt <- .report_num(tryCatch(isolate(secondary_valuation_point()), error = function(e) NA))
        conf <- tryCatch(isolate(valuation_confidence()), error = function(e) NULL)

        # 目標價＝主模型 Base；缺則依 primary key 回退（僅個股模型，不含同業／Lab）
        rating_anchor <- {
          if (!is.null(band) && is.finite(suppressWarnings(as.numeric(band$base)[1]))) {
            as.numeric(band$base)[1]
          } else {
            prim <- as.character(rec_full$primary %||% "")
            switch(
              prim,
              "dcf" = dcf_price,
              "ddm" = ddm_val,
              "pb" = pb_val,
              "ri" = ri_val,
              if (is.finite(dcf_price)) dcf_price else if (is.finite(pb_val)) pb_val else if (is.finite(ddm_val)) ddm_val else ri_val
            )
          }
        }

        rating_info <- derive_investment_rating(cur_price, rating_anchor)

        sum_df <- isolate(summary_data())
        co_name <- isolate(attr(sum_df, "company_name"))
        if (is.null(co_name) || is.na(co_name) || !nzchar(as.character(co_name))) {
          co_name <- display_ticker_for_market(
            isolate(current_ticker()), isolate(market_mode())
          )
        }

        ind_text <- isolate(corp_industry_text())
        sector_str <- "N/A"; industry_str <- "N/A"
        if (!is.null(ind_text) && grepl("\\|", ind_text)) {
          parts <- strsplit(ind_text, "\\|")[[1]]
          sector_str <- trimws(sub("Sector:\\s*", "", parts[1]))
          if (length(parts) > 1) industry_str <- trimws(sub("Industry:\\s*", "", parts[2]))
        }

        rep_loc <- tryCatch(isolate(ui_locale()), error = function(e) "zh-TW")
        if (!identical(as.character(rep_loc)[1], "en")) rep_loc <- "zh-TW"

        wacc_str <- if (identical(isolate(input$dcf_mode), "gordon")) {
          paste0(isolate(input$wacc_gordon), "%")
        } else if (identical(rep_loc, "en")) {
          paste0(isolate(input$wacc_stage1), "% / ", isolate(input$wacc_stage2), "% (Two-Stage)")
        } else {
          paste0(isolate(input$wacc_stage1), "% / ", isolate(input$wacc_stage2), "% (兩階段)")
        }

        warn_msgs <- collect_fraud_warnings(
          isolate(d_cash_flow()), isolate(d_income_statement()), isolate(d_balance_sheet()),
          industry_key = isolate(input$industry_choice)
        )
        fscore_info <- compute_report_f_score(
          isolate(d_income_statement()), isolate(d_balance_sheet()), isolate(d_cash_flow())
        )
        if (is.list(fscore_info) && is.data.frame(fscore_info$checklist) &&
            exists("localize_fscore_checklist", mode = "function")) {
          fscore_info$checklist <- localize_fscore_checklist(fscore_info$checklist, rep_loc)
        }

        eps_bv <- .report_eps_bvps(
          sum_df, isolate(d_income_statement()), isolate(d_balance_sheet())
        )
        # P/B 模組 BVPS 優先（已依股數／幣別對齊）
        pb_bvps_in <- .report_num(tryCatch(isolate(input[["mod_pb-bvps"]]), error = function(e) NA))
        if (is.finite(pb_bvps_in) && pb_bvps_in > 0) eps_bv$bvps <- pb_bvps_in

        kpi_df <- build_report_kpi_df(
          isolate(d_income_statement()), isolate(d_balance_sheet()), isolate(d_cash_flow())
        )
        roe_pct <- NA_real_
        rev_g_pct <- NA_real_
        if (!is.null(kpi_df) && is.data.frame(kpi_df) && nrow(kpi_df) > 0) {
          roe_row <- grep("^ROE$", kpi_df[[1]], ignore.case = TRUE)
          if (length(roe_row) >= 1L) {
            roe_pct <- suppressWarnings(as.numeric(gsub("%", "", kpi_df[[2]][roe_row[1]])))
          }
          rg_row <- grep("營收成長|Revenue growth", kpi_df[[1]], ignore.case = TRUE)
          if (length(rg_row) >= 1L) {
            rev_g_pct <- suppressWarnings(as.numeric(gsub("%", "", kpi_df[[2]][rg_row[1]])))
          }
        }
        capex_info <- .report_capex_revenue_ratio(
          isolate(d_income_statement()), isolate(d_cash_flow())
        )

        sens_df <- tryCatch({
          st <- isolate(sensitivity_state())
          mat <- if (is.list(st) && is.list(st$built)) st$built$matrix else NULL
          .report_sensitivity_df(mat)
        }, error = function(e) NULL)

        px <- money_prefix()
        dcf_mode_lab <- .report_dcf_mode_label(isolate(input$dcf_mode), locale = rep_loc)
        sec_lab <- if (!is.null(rec_full$secondary)) {
          tryCatch(.model_label(rec_full$secondary), error = function(e) as.character(rec_full$secondary))
        } else {
          NA_character_
        }

        rep_mm <- tryCatch(
          normalize_market_mode(isolate(market_mode())),
          error = function(e) "US"
        )
        rep_lite <- isTRUE(isolate(lite_mode()))
        app_ver <- tryCatch({
          build_path <- file.path("www", "ynow_build.json")
          if (!file.exists(build_path)) build_path <- "ynow_build.json"
          raw <- paste(readLines(build_path, warn = FALSE), collapse = "\n")
          m <- regmatches(raw, regexpr('"version"\\s*:\\s*"[^"]+"', raw))
          if (length(m) == 1L && nzchar(m)) {
            sub('^"version"\\s*:\\s*"([^"]+)".*$', "\\1", m)
          } else {
            "v20.66"
          }
        }, error = function(e) "v20.66")

        report_copy <- build_ticker_report_copy(
          locale = rep_loc,
          stock_code = display_ticker_for_market(
            isolate(current_ticker()), isolate(market_mode())
          ),
          company_name = co_name,
          sector = sector_str,
          industry = industry_str,
          current_price = cur_price,
          target_price = rating_anchor,
          primary_method = val_method$method,
          method_rationale = val_method$rationale,
          margin_of_safety = rating_info$margin_of_safety,
          upside_pct = rating_info$upside_pct,
          dcf_price = dcf_price,
          ddm_value = ddm_val,
          pb_value = pb_val,
          ri_value = ri_val,
          primary_bear = if (!is.null(band)) .report_num(band$bear) else NA_real_,
          primary_base = if (!is.null(band)) .report_num(band$base) else rating_anchor,
          primary_bull = if (!is.null(band)) .report_num(band$bull) else NA_real_,
          secondary_point = sec_pt,
          secondary_label = sec_lab,
          confidence_level = if (is.list(conf)) conf$level else NA_character_,
          confidence_score = if (is.list(conf)) conf$score else NA_real_,
          wacc = wacc_str,
          terminal_growth = paste0(isolate(input$sgr), "%"),
          forecast_years = isolate(input$years),
          dcf_mode = dcf_mode_lab,
          eps = eps_bv$eps,
          bvps = eps_bv$bvps,
          pe_ratio = extract_summary_item(sum_df, "PE Ratio|Trailing P/E"),
          market_cap = extract_summary_item(sum_df, "Market Cap"),
          beta = extract_summary_item(sum_df, "^Beta"),
          dividend_yield = extract_summary_item(sum_df, "Yield|Dividend"),
          roe_pct = roe_pct,
          rev_growth_pct = rev_g_pct,
          capex_rev_pct = capex_info$ratio_pct,
          capex_avg_pct = capex_info$avg_pct,
          capex_n_years = capex_info$n_years,
          fscore_total = fscore_info$total,
          money_prefix = px,
          market_mode = rep_mm,
          report_condensed = rep_lite
        )

        tmp_html <- tempfile(fileext = ".html")
        rmarkdown::render(
          input = tempReport,
          output_file = basename(tmp_html),
          output_dir = dirname(tmp_html),
          intermediates_dir = tempdir(),
          clean = TRUE,
          quiet = TRUE,
          params = list(
            stock_code = display_ticker_for_market(
              isolate(current_ticker()), isolate(market_mode())
            ),
            company_name = co_name,
            sector = sector_str,
            industry = industry_str,
            report_date = format(Sys.Date(), "%Y/%m/%d"),
            rating = rating_info$rating,
            rating_en = rating_info$rating_en,
            rating_color = rating_info$rating_color,
            current_price = cur_price,
            target_price = rating_anchor,
            upside_pct = rating_info$upside_pct,
            dcf_price = dcf_price,
            ddm_value = ddm_val,
            pb_value = pb_val,
            ri_value = ri_val,
            ev_value = ev_val,
            margin_of_safety = rating_info$margin_of_safety,
            primary_method = val_method$method,
            method_rationale = val_method$rationale,
            primary_bear = if (!is.null(band)) .report_num(band$bear) else NA_real_,
            primary_base = if (!is.null(band)) .report_num(band$base) else rating_anchor,
            primary_bull = if (!is.null(band)) .report_num(band$bull) else NA_real_,
            secondary_point = sec_pt,
            confidence_level = if (is.list(conf)) conf$level else NA_character_,
            confidence_score = if (is.list(conf)) conf$score else NA_real_,
            wacc = wacc_str,
            terminal_growth = paste0(isolate(input$sgr), "%"),
            forecast_years = isolate(input$years),
            dcf_mode = dcf_mode_lab,
            market_cap = extract_summary_item(sum_df, "Market Cap"),
            pe_ratio = extract_summary_item(sum_df, "PE Ratio|Trailing P/E"),
            beta = extract_summary_item(sum_df, "^Beta"),
            dividend_yield = extract_summary_item(sum_df, "Yield|Dividend"),
            eps = eps_bv$eps,
            bvps = eps_bv$bvps,
            kpi_df = kpi_df,
            fcf_plot_path = plot_path,
            warnings = if (length(warn_msgs) > 0) paste(warn_msgs, collapse = "\n") else "",
            fscore_total = fscore_info$total,
            fscore_quality = fscore_info$quality_flag,
            fscore_checklist = fscore_info$checklist,
            investment_highlights = report_copy$investment_bullets,
            session_currency = isolate(session_currency()),
            fx_usd_twd = isolate(fx_usd_twd()),
            report_locale = rep_loc,
            report_copy = report_copy,
            sensitivity_df = if (rep_lite) NULL else sens_df,
            app_version = app_ver,
            report_condensed = rep_lite,
            market_mode = rep_mm,
            summary_df = {
              sd <- sum_df
              if (!is.null(sd) && is.data.frame(sd) && nrow(sd) > 0) {
                sd$Value <- vapply(seq_len(nrow(sd)), function(i) {
                  convert_summary_value_display(
                    sd$Item[i], sd$Value[i],
                    isolate(quote_currency()), isolate(session_currency()), isolate(fx_usd_twd())
                  )
                }, character(1))
              }
              sd
            },
            income_df = trim_report_table(isolate(d_income_statement())),
            balance_df = trim_report_table(isolate(d_balance_sheet())),
            cashflow_df = trim_report_table(isolate(d_cash_flow()))
          ),
          envir = new.env(parent = globalenv())
        )

        render_report_pdf(tmp_html, file)
        if (nzchar(as.character(pdf_ticket$fingerprint %||% "")[1]) &&
            exists("ynow_pdf_repeat_mark", mode = "function")) {
          tryCatch(
            ynow_pdf_repeat_mark(pdf_ticket$user_id, pdf_ticket$fingerprint),
            error = function(e) NULL
          )
        }
        loc_pdf <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "en")
        if (identical(pdf_ticket$kind, "repeat")) {
          showNotification(
            tryCatch(
              ui_str("pdf_repeat_free", loc_pdf),
              error = function(e) "This download does not use a free download or FYN."
            ),
            type = "message"
          )
        } else if (identical(pdf_ticket$kind, "free")) {
          showNotification(
            sprintf(
              tryCatch(ui_str("pdf_free_ok_fmt", loc_pdf), error = function(e) "Free PDF download (%s of %s today)."),
              as.character(pdf_ticket$used %||% ""),
              as.character(pdf_ticket$limit %||% 3L)
            ),
            type = "message"
          )
        } else if (identical(pdf_ticket$kind, "paid")) {
          showNotification(
            sprintf(
              tryCatch(ui_str("pdf_paid_ok_fmt", loc_pdf), error = function(e) "PDF download charged %s FYN (balance %s)."),
              as.character(pdf_ticket$cost %||% 10L),
              as.character(pdf_ticket$fyn %||% "")
            ),
            type = "message"
          )
          tryCatch(.ynow_pts_push_banner(), error = function(e) NULL)
        } else {
          showNotification(.ui_msg("notif_pdf_ok"), type = "message")
        }
        tryCatch(
          .ynow_access_note_conditional_use("report:download_report", consume_free = TRUE),
          error = function(e) NULL
        )
        tryCatch(.ynow_pdf_quota_push(), error = function(e) NULL)
      }, error = function(e) {
        if (exists("ynow_pdf_release", mode = "function")) {
          tryCatch(ynow_pdf_release(pdf_ticket), error = function(e2) NULL)
        }
        showNotification(.ui_msg("notif_pdf_fail", err = e$message), type = "error", duration = 12)
        # 寫入最小錯誤 PDF 避免下載 handler 空檔
        tryCatch({
          grDevices::pdf(file, width = 8.27, height = 11.69)
          plot.new()
          text(0.5, 0.6, "YNow 報告產出失敗", cex = 1.4)
          text(0.5, 0.45, paste(strwrap(e$message, 60), collapse = "\n"), cex = 0.8)
          grDevices::dev.off()
        }, error = function(e2) NULL)
      })
    }
  )

  # ==========================================
  # Dashboard：財報附註擷取 (SEC EDGAR)
  # ==========================================
  lab_sec_result <- reactiveVal(NULL)

  # 側邊欄「系統管理」入口（完整版；Lite／已登入帳號隱藏）
  observeEvent(input$sidebar_test_click, {
    logged <- tryCatch(isTRUE(ynow_auth$is_logged_in()), error = function(e) FALSE)
    if (isTRUE(logged)) {
      tryCatch(
        updateTabItems(session, "sidebar_tabs", "home"),
        error = function(e) {
          tryCatch(
            session$sendInputMessage("sidebar_tabs", list(value = "home")),
            error = function(e2) NULL
          )
        }
      )
      return()
    }
    showNotification(.ui_msg("notif_test_opened"), type = "message", duration = 3)
  }, ignoreInit = TRUE)

  # ------------------------------------------
  # Lab：產業×模型複選；Piotroski 高門檻（F-Score≥7）後依年化估值漲幅排序
  # ------------------------------------------
  .lab_im_boards <- reactive({
    mode <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
    if (!identical(mode, "TW")) return(character(0))
    raw <- input$lab_im_boards
    boards <- if (exists("lab_normalize_tw_boards", mode = "function")) {
      lab_normalize_tw_boards(raw %||% APP_DEFAULTS$lab_im_boards)
    } else {
      as.character(raw %||% c("TWSE", "TPEX"))
    }
    if (!length(boards)) boards <- c("TWSE", "TPEX")
    boards
  })

  lab_im_catalog <- reactive({
    lab_im_catalog_nonce()
    # Defer catalog build until Blue Chip has been opened at least once this session.
    req(isTRUE(.ynow_bluechip_mounted()))
    mode <- market_mode()
    boards <- tryCatch(.lab_im_boards(), error = function(e) c("TWSE", "TPEX"))
    tryCatch(
      lab_build_industry_method_catalog(market_mode = mode, boards = boards),
      error = function(e) {
        showNotification(.ui_msg("notif_industry_catalog_fail", err = e$message), type = "error")
        data.frame()
      }
    )
  })

  # Industry chain × sub-industry → industry_key filter for Blue Chip Lab
  lab_im_industry_filter <- reactive({
    lab_resolve_industry_filter_keys(
      chain_ids = input$lab_im_chains,
      sub_keys = input$lab_im_industries
    )
  })

  # Coalesce Blue Chip picker locale refreshes so ZH/EN stays snappy during eval.
  .lab_im_locale_picker_gen <- reactiveVal(0L)
  .lab_im_schedule_picker_locale <- function(fn) {
    gen <- tryCatch(
      as.integer(isolate(.lab_im_locale_picker_gen()) %||% 0L) + 1L,
      error = function(e) 1L
    )
    .lab_im_locale_picker_gen(gen)
    run <- function() {
      if (!identical(isolate(.lab_im_locale_picker_gen()), gen)) return()
      tryCatch(fn(), error = function(e) NULL)
    }
    busy <- isTRUE(isolate(lab_im_eval_busy())) || isTRUE(isolate(lab_im_universe_busy()))
    if (busy && requireNamespace("later", quietly = TRUE)) {
      later::later(run, delay = 0.35)
    } else if (busy) {
      # No later package: skip heavy picker remount while Blue Chip is running
      invisible(NULL)
    } else {
      run()
    }
  }

  # Refresh chain / sub-industry pickers on locale + chain selection
  observeEvent(
    list(ui_locale(), lazy_mount_nonce()),
    {
      if (!isTRUE(.ynow_bluechip_mounted())) return()
      .lab_im_schedule_picker_locale(function() {
        loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "zh-TW")
        chain_choices <- lab_industry_chain_picker_choices(loc)
        cur_ch <- as.character(isolate(input$lab_im_chains) %||% character(0))
        cur_ch <- cur_ch[cur_ch %in% unname(chain_choices)]
        shinyWidgets::updatePickerInput(
          session,
          "lab_im_chains",
          choices = chain_choices,
          selected = if (length(cur_ch)) cur_ch else unname(chain_choices),
          options = list(
            noneSelectedText = tryCatch(
              ui_str("lab_im_chains_none", loc),
              error = function(e) "全部產業鏈"
            )
          )
        )
      })
    },
    ignoreNULL = FALSE
  )

  observeEvent(
    list(input$lab_im_chains, ui_locale(), lazy_mount_nonce()),
    {
      if (!isTRUE(.ynow_bluechip_mounted())) return()
      .lab_im_schedule_picker_locale(function() {
        loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "zh-TW")
        chains <- as.character(isolate(input$lab_im_chains) %||% character(0))
        sub_choices <- lab_industry_sub_picker_choices(chains, loc)
        cur_sub <- as.character(isolate(input$lab_im_industries) %||% character(0))
        cur_sub <- cur_sub[cur_sub %in% unname(sub_choices)]
        shinyWidgets::updatePickerInput(
          session,
          "lab_im_industries",
          choices = sub_choices,
          selected = if (length(cur_sub)) cur_sub else unname(sub_choices),
          options = list(
            noneSelectedText = tryCatch(
              ui_str("lab_im_industries_none", loc),
              error = function(e) "全部子產業"
            )
          )
        )
      })
    },
    ignoreNULL = FALSE
  )

  output$lab_im_bluechip_blurb <- renderUI({
    loc <- ui_locale()
    mode <- market_mode()
    key <- if (identical(normalize_market_mode(mode), "TW")) {
      "bluechip_blurb_tw"
    } else {
      "bluechip_blurb_us"
    }
    tags$p(ui_str(key, loc))
  })

  output$lab_im_universe_meta <- renderUI({
    lab_im_catalog_nonce()
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    mode <- market_mode()
    if (identical(mode, "TW")) {
      meta <- tryCatch(lab_tw_universe_meta(), error = function(e) NULL)
      n_twse <- if (is.null(meta)) 0L else as.integer(meta$n_twse %||% 0L)
      n_tpex <- if (is.null(meta)) 0L else as.integer(meta$n_tpex %||% 0L)
      n_esb <- if (is.null(meta)) 0L else as.integer(meta$n_esb %||% 0L)
      boards_sel <- tryCatch(.lab_im_boards(), error = function(e) c("TWSE", "TPEX"))
      board_txt <- paste(boards_sel, collapse = "+")
      if (!nzchar(board_txt)) board_txt <- "TWSE+TPEX"
      label <- tryCatch(
        .ynow_fmt_str(
          ui_str("lab_im_universe_tw", loc),
          n_twse = n_twse, n_tpex = n_tpex, n_esb = n_esb, boards = board_txt
        ),
        error = function(e) sprintf(
          "上市 %d／上櫃 %d／興櫃 %d（搜尋全納；績優板別＝%s）",
          n_twse, n_tpex, n_esb, board_txt
        )
      )
    } else {
      meta <- tryCatch(lab_us_universe_meta(), error = function(e) NULL)
      if (is.null(meta) || as.integer(meta$n %||% 0L) < 1L) {
        meta <- tryCatch(lab_sp500_universe_meta(), error = function(e) NULL)
      }
      n_nas <- if (is.null(meta)) 0L else as.integer(meta$n_nasdaq %||% 0L)
      n_ny <- if (is.null(meta)) 0L else as.integer(meta$n_nyse %||% 0L)
      if (n_nas > 0L || n_ny > 0L) {
        label <- tryCatch(
          .ynow_fmt_str(ui_str("lab_im_universe_us", loc), n_nas = n_nas, n_ny = n_ny),
          error = function(e) sprintf(
            "美股主要上市（Nasdaq %d／NYSE %d；含 NYSE American）", n_nas, n_ny
          )
        )
      } else {
        label <- tryCatch(
          ui_str("lab_im_universe_us_fallback", loc),
          error = function(e) "美股主要上市（Nasdaq＋NYSE＋NYSE American）"
        )
      }
    }
    n <- if (is.null(meta)) 0L else as.integer(meta$n %||% 0L)
    fetched <- if (is.null(meta)) "—" else lab_format_fetched_at(meta$fetched_at)
    n_un <- if (is.null(meta)) 0L else as.integer(meta$n_unmapped %||% 0L)
    src <- if (is.null(meta) || !nzchar(as.character(meta$source %||% ""))) "" else
      as.character(meta$source)[1]
    count_txt <- tryCatch(
      .ynow_fmt_str(ui_str("lab_im_universe_count", loc), label = label, n = n),
      error = function(e) sprintf("%s · %d 檔", label, n)
    )
    tags$div(
      class = "ynow-lab-im-universe-meta",
      tags$b(count_txt),
      tags$span(tryCatch(
        .ynow_fmt_str(ui_str("lab_im_universe_updated", loc), fetched = fetched),
        error = function(e) sprintf(" · 更新於 %s", fetched)
      )),
      if (n_un > 0L) tags$span(tryCatch(
        .ynow_fmt_str(ui_str("lab_im_universe_unmapped", loc), n = n_un),
        error = function(e) sprintf(" · 未對應產業 %d 檔", n_un)
      )) else NULL,
      if (nzchar(src)) tags$span(
        class = "ynow-lab-im-muted",
        tryCatch(
          .ynow_fmt_str(ui_str("lab_im_universe_source", loc), src = src),
          error = function(e) sprintf(" · 來源 %s", src)
        )
      ) else NULL
    )
  })

  observeEvent(input$lab_im_refresh_universe, {
    # Exclusive with Blue Chip eval (shared quality snapshot IO) — Clustering / HFV OK.
    # Re-click while this refresh itself is running: ignore duplicate (sync withProgress).
    if (isTRUE(isolate(lab_im_universe_busy()))) {
      return()
    }
    if (isTRUE(isolate(lab_im_eval_busy()))) {
      showNotification(.ui_msg("lab_im_quality_busy"), type = "warning", duration = 5)
      return()
    }
    mode <- market_mode()
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    msg <- if (identical(mode, "TW")) "更新台股上市／上櫃／興櫃名單…" else "更新美股主要上市名單…"
    lab_im_universe_busy(TRUE)
    on.exit(tryCatch(lab_im_universe_busy(FALSE), error = function(e) NULL), add = TRUE)
    fetched <- withProgress(
      message = msg,
      value = 0.4, {
        out <- if (identical(mode, "TW")) {
          tryCatch(lab_refresh_tw_universe(), error = function(e) NULL)
        } else {
          tryCatch(lab_refresh_us_universe(), error = function(e) NULL)
        }
        incProgress(0.5)
        out
      }
    )
    if (!is.data.frame(fetched) || nrow(fetched) < 1L) {
      showNotification(.ui_msg("notif_update_cache_fallback"), type = "warning", duration = 8)
      return()
    }
    lab_im_catalog_nonce(isolate(lab_im_catalog_nonce()) + 1L)
    showNotification(
      sprintf("已更新名單：%d 檔。", length(unique(fetched$ticker))),
      type = "message", duration = 6
    )
    mm <- tryCatch(normalize_market_mode(mode), error = function(e) "US")
    boards <- tryCatch(isolate(.lab_im_boards()), error = function(e) c("TWSE", "TPEX"))
    catlg <- tryCatch(
      lab_build_industry_method_catalog(market_mode = mode, boards = boards),
      error = function(e) NULL
    )
    include_adr <- if (identical(mm, "TW")) TRUE else isTRUE(isolate(input$lab_im_include_adr) %||% TRUE)
    pool <- if (is.data.frame(catlg) && nrow(catlg) > 0L) {
      tryCatch(
        lab_structural_filter_pool(
          catlg,
          method_filter = isolate(input$lab_im_methods),
          industry_filter = isolate(lab_im_industry_filter()),
          include_adr = include_adr,
          market_mode = mm
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    if (!is.data.frame(pool) || !nrow(pool)) {
      showNotification(.ui_msg("notif_no_candidates_ind_model"), type = "warning")
      return()
    }
    result <- withProgress(
      message = tryCatch(ui_str("lab_im_quality_progress", loc), error = function(e) "Updating quality snapshot…"),
      value = 0,
      {
        lab_refresh_quality_snapshot(
          pool,
          market = mm,
          force = FALSE,
          progress_cb = function(i, n, tk) {
            incProgress(1 / max(n, 1), detail = paste0(tk, " (", i, "/", n, ")"))
          }
        )
      }
    )
    lab_quality_snap_nonce(isolate(lab_quality_snap_nonce()) + 1L)
    showNotification(
      .ui_msg(
        "lab_im_quality_done",
        run = as.integer(result$n_run %||% 0L),
        skip = as.integer(result$n_skip %||% 0L),
        loc = loc
      ),
      type = "message",
      duration = 6
    )
  })

  # 首屏用快取；畫完後若名單超過約 7 天才嘗試網路更新（失敗不擋）
  session$onFlushed(function() {
    mode <- tryCatch(isolate(market_mode()), error = function(e) "US")
    stale <- if (identical(mode, "TW")) {
      tryCatch(lab_tw_is_stale(), error = function(e) FALSE)
    } else {
      tryCatch(lab_us_is_stale(), error = function(e) FALSE)
    }
    if (!isTRUE(stale)) return()
    fetched <- if (identical(mode, "TW")) {
      tryCatch(lab_refresh_tw_universe(), error = function(e) NULL)
    } else {
      tryCatch(lab_refresh_us_universe(), error = function(e) NULL)
    }
    if (is.data.frame(fetched) && nrow(fetched) > 0) {
      lab_im_catalog_nonce(isolate(lab_im_catalog_nonce()) + 1L)
    }
  }, once = TRUE)

  lab_im_pool <- reactive({
    catlg <- lab_im_catalog()
    req(is.data.frame(catlg), nrow(catlg) > 0)
    lab_merge_catalog_scores(
      catlg,
      scores = lab_im_scores(),
      method_filter = input$lab_im_methods,
      industry_filter = lab_im_industry_filter(),
      eq_only = FALSE,
      gate_only = FALSE
    )
  })

  # Explicit Search Blue Chips button
  observeEvent(input$lab_im_run_fscore, {
    lab_im_run_pulse(isolate(as.integer(lab_im_run_pulse()) %||% 0L) + 1L)
  }, ignoreInit = TRUE)

  # Opening sidebar「Blue Chip Leaderboard」auto-starts evaluation when none yet
  # (same runner as Search Blue Chips). Wait for lazy mount so filter inputs exist.
  # Defer pulse via onFlushed + later::later so lazy bluechip UI can paint first.
  # Eval itself runs via ExtendedTask so Clustering / HFV can start concurrently.
  observeEvent(
    list(input$sidebar_tabs, lazy_mount_nonce()),
    {
      tab <- as.character(input$sidebar_tabs %||% "")[1]
      if (!identical(tab, "bluechip")) return()
      mounted <- tryCatch(
        isTRUE(.ynow_lazy_tab_ctrl$mounted("bluechip")),
        error = function(e) FALSE
      )
      if (!isTRUE(mounted)) return()
      scores <- isolate(lab_im_scores())
      if (is.data.frame(scores) && nrow(scores) > 0L) return()
      if (isTRUE(isolate(lab_im_eval_busy())) || isTRUE(isolate(lab_im_universe_busy()))) return()
      if (isTRUE(isolate(lab_im_auto_start_armed()))) return()
      lab_im_auto_start_armed(TRUE)
      session$onFlushed(function() {
        start_eval <- function() {
          if (!identical(as.character(isolate(input$sidebar_tabs) %||% "")[1], "bluechip")) {
            tryCatch(lab_im_auto_start_armed(FALSE), error = function(e) NULL)
            return()
          }
          sc2 <- isolate(lab_im_scores())
          if (is.data.frame(sc2) && nrow(sc2) > 0L) {
            tryCatch(lab_im_auto_start_armed(FALSE), error = function(e) NULL)
            return()
          }
          if (isTRUE(isolate(lab_im_eval_busy())) || isTRUE(isolate(lab_im_universe_busy()))) {
            tryCatch(lab_im_auto_start_armed(FALSE), error = function(e) NULL)
            return()
          }
          lab_im_run_pulse(isolate(as.integer(lab_im_run_pulse()) %||% 0L) + 1L)
        }
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(start_eval, delay = 0.05)
        } else {
          start_eval()
        }
      }, once = TRUE)
    },
    ignoreInit = TRUE
  )

  # Apply Blue Chip job result on the session thread (shared by async + sync paths).
  .lab_im_apply_eval_result <- function(out) {
    if (is.null(out) || !is.list(out)) return(invisible(FALSE))
    if (!isTRUE(out$ok)) {
      reason <- as.character(out$reason %||% "")[1]
      if (identical(reason, "no_candidates") || identical(reason, "empty_catalog")) {
        showNotification(.ui_msg("notif_no_candidates"), type = "warning")
      } else if (identical(reason, "no_candidates_ind_model")) {
        showNotification(.ui_msg("notif_no_candidates_ind_model"), type = "warning")
      }
      return(invisible(FALSE))
    }
    if (nzchar(as.character(out$pool_note %||% "")[1])) {
      showNotification(as.character(out$pool_note)[1], type = "message", duration = 6)
    }
    scores <- out$scores
    if (is.null(scores) || !is.data.frame(scores) || nrow(scores) == 0L) {
      return(invisible(FALSE))
    }
    lab_im_scores(scores)
    mm0 <- tryCatch(
      normalize_market_mode(out$market_mode %||% isolate(market_mode())),
      error = function(e) "US"
    )
    tryCatch(
      lab_upsert_quality_rows(scores, market = mm0),
      error = function(e) NULL
    )
    lab_quality_snap_nonce(isolate(lab_quality_snap_nonce()) + 1L)
    display_n <- if (exists("lab_coerce_im_job_n", mode = "function")) {
      lab_coerce_im_job_n(out$display_n)
    } else {
      suppressWarnings(as.numeric(out$display_n)[1])
    }
    fs_pass <- suppressWarnings(as.numeric(scores$f_score))
    pass_idx <- which(is.finite(fs_pass) & fs_pass >= 7)
    n_q <- length(pass_idx)
    best <- NA_real_
    if (n_q > 0) {
      best <- suppressWarnings(max(scores$upside_cagr_pct[pass_idx], na.rm = TRUE))
      if (!is.finite(best)) best <- NA_real_
    }
    top_tk <- NA_character_
    if (is.finite(best)) {
      hit <- which(is.finite(fs_pass) & fs_pass >= 7 &
                     is.finite(scores$upside_cagr_pct) &
                     abs(scores$upside_cagr_pct - best) < 1e-9)
      if (length(hit) > 0) top_tk <- scores$ticker[hit[1]]
    }
    loc_done <- tryCatch(
      normalize_ui_locale(out$locale %||% isolate(ui_locale())),
      error = function(e) "zh-TW"
    )
    n_yrs <- tryCatch({
      yr <- suppressWarnings(as.numeric(scores$n_years[pass_idx][1]))
      if (!is.finite(yr)) yr <- suppressWarnings(as.numeric(scores$years[pass_idx][1]))
      if (!is.finite(yr)) 1L else as.integer(round(yr))
    }, error = function(e) 1L)
    top_part <- if (is.finite(best) && nzchar(top_tk %||% "")) {
      .ynow_fmt_str(
        ui_str("lab_im_done_top", loc_done),
        ticker = top_tk, n = n_yrs, pct = sprintf("%+.1f%%", best)
      )
    } else {
      ""
    }
    cap_part <- if (is.finite(display_n)) {
      .ynow_fmt_str(ui_str("lab_im_done_detail_cap", loc_done), n = as.character(display_n))
    } else {
      ui_str("lab_im_done_detail_cap_all", loc_done)
    }
    msg <- paste0(
      .ui_msg("lab_im_done_gate", n = nrow(scores), q = n_q),
      top_part,
      cap_part
    )
    showNotification(msg, type = "message", duration = 10)
    invisible(TRUE)
  }

  .lab_im_start_eval_args <- function(args, notify = TRUE) {
    if (!is.list(args) || !is.data.frame(args$catlg) || nrow(args$catlg) == 0L) {
      return(invisible(FALSE))
    }
    gen <- as.integer(args$.gen %||% 0L)[1]
    if (!isTRUE(ynow_heavy_job_try_acquire("lab_im_eval"))) {
      # Same button pressed again: keep latest args; retry when the slot frees.
      lab_im_eval_pending(args)
      return(invisible(FALSE))
    }
    if (isTRUE(notify)) {
      showNotification(
        tryCatch(.ui_msg("lab_im_async_started"), error = function(e) .ui_msg("lab_im_progress")),
        type = "message", duration = 4
      )
    }
    lab_im_eval_busy(TRUE)
    lab_im_eval_pending(NULL)
    .ynow_inflight_gen$lab_im_eval <- as.integer(args$.gen %||% 0L)[1]
    if (!is.null(lab_im_bc_task)) {
      tryCatch(
        lab_im_bc_task$invoke(
          catlg = args$catlg,
          method_filter = args$method_filter,
          industry_filter = args$industry_filter,
          include_adr = args$include_adr,
          display_n = args$display_n,
          eval_n = args$eval_n,
          rank_mode = args$rank_mode,
          concept_keys = args$concept_keys,
          market_mode = args$market_mode,
          locale = args$locale
        ),
        error = function(e) {
          lab_im_eval_busy(FALSE)
          tryCatch(ynow_heavy_job_release("lab_im_eval"), error = function(e2) NULL)
          showNotification(
            paste("Blue Chip async start failed:", conditionMessage(e)),
            type = "error", duration = 8
          )
        }
      )
      return(invisible(TRUE))
    }
    on.exit({
      tryCatch(lab_im_eval_busy(FALSE), error = function(e) NULL)
      tryCatch(ynow_heavy_job_release("lab_im_eval"), error = function(e) NULL)
    }, add = TRUE)
    out <- tryCatch(
      lab_im_run_eval_job(
        catlg = args$catlg,
        method_filter = args$method_filter,
        industry_filter = args$industry_filter,
        include_adr = args$include_adr,
        display_n = args$display_n,
        eval_n = args$eval_n,
        rank_mode = args$rank_mode,
        concept_keys = args$concept_keys,
        market_mode = args$market_mode,
        locale = args$locale
      ),
      error = function(e) {
        showNotification(paste("Blue Chip eval failed:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      }
    )
    if (identical(isolate(as.integer(lab_im_eval_gen()) %||% 0L), gen)) {
      .lab_im_apply_eval_result(out)
    }
    invisible(TRUE)
  }

  .lab_im_finish_eval_slot <- function(out = NULL, apply = TRUE, had_error = FALSE) {
    lab_im_eval_busy(FALSE)
    tryCatch(ynow_heavy_job_release("lab_im_eval"), error = function(e) NULL)
    pending <- isolate(lab_im_eval_pending())
    latest <- isolate(as.integer(lab_im_eval_gen()) %||% 0L)
    if (is.list(pending) && identical(as.integer(pending$.gen %||% 0L)[1], latest)) {
      lab_im_eval_pending(NULL)
      .lab_im_start_eval_args(pending, notify = TRUE)
      return(invisible(FALSE))
    }
    # Superseded by a newer click with no pending left: drop stale result.
    if (is.list(pending) || !isTRUE(apply) || isTRUE(had_error)) {
      return(invisible(TRUE))
    }
    if (isTRUE(.ynow_inflight_stale("lab_im_eval", latest))) {
      return(invisible(TRUE))
    }
    .lab_im_apply_eval_result(out)
    invisible(TRUE)
  }

  observeEvent(lab_im_run_pulse(), {
    # Universe refresh still exclusive (shared quality snapshot IO).
    if (isTRUE(isolate(lab_im_universe_busy()))) {
      return()
    }
    catlg <- tryCatch(lab_im_catalog(), error = function(e) NULL)
    if (!is.data.frame(catlg) || nrow(catlg) == 0L) return()
    mm0 <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
    include_adr <- if (identical(mm0, "TW")) {
      TRUE
    } else {
      isTRUE(input$lab_im_include_adr %||% TRUE)
    }
    display_n <- lab_resolve_im_max_n(input$lab_im_max_n, input$lab_im_max_n_custom)
    eval_n <- lab_resolve_im_eval_n(display_n)
    rank_mode <- lab_normalize_pool_rank_mode(input$lab_im_pool_rank %||% "mcap")
    concept_keys <- input$lab_im_concepts
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    method_filter <- input$lab_im_methods
    industry_filter <- lab_im_industry_filter()
    gen <- isolate(as.integer(lab_im_eval_gen()) %||% 0L) + 1L
    lab_im_eval_gen(gen)
    args <- list(
      .gen = gen,
      catlg = catlg,
      method_filter = method_filter,
      industry_filter = industry_filter,
      include_adr = include_adr,
      display_n = display_n,
      eval_n = eval_n,
      rank_mode = rank_mode,
      concept_keys = concept_keys,
      market_mode = mm0,
      locale = loc
    )
    running <- isTRUE(isolate(lab_im_eval_busy()))
    if (!is.null(lab_im_bc_task)) {
      st <- tryCatch(lab_im_bc_task$status(), error = function(e) "error")
      if (identical(st, "running")) running <- TRUE
    }
    if (isTRUE(running)) {
      # Re-click: supersede in-flight result; restart as soon as the slot frees.
      lab_im_eval_pending(args)
      return()
    }
    .lab_im_start_eval_args(args, notify = TRUE)
  }, ignoreInit = TRUE)

  # Blue Chip ExtendedTask completion (session stays free while status == running)
  if (!is.null(lab_im_bc_task)) {
    observeEvent(lab_im_bc_task$status(), {
      st <- tryCatch(lab_im_bc_task$status(), error = function(e) "initial")
      if (identical(st, "error")) {
        err <- tryCatch(lab_im_bc_task$result(), error = function(e) conditionMessage(e))
        if (is.character(err) && nzchar(err[1])) {
          showNotification(paste("Blue Chip eval failed:", err[1]), type = "error", duration = 8)
        }
        .lab_im_finish_eval_slot(had_error = TRUE, apply = FALSE)
        return()
      }
      if (!identical(st, "success")) return()
      out <- tryCatch(lab_im_bc_task$result(), error = function(e) NULL)
      .lab_im_finish_eval_slot(out = out, apply = TRUE)
    }, ignoreInit = TRUE)
  }

  # Models filter ↔ 建議評價方法 table: same sidebar order; lock ones with no primary / reference family.
  .lab_im_method_scope_catalog <- reactive({
    catlg <- tryCatch(lab_im_catalog(), error = function(e) NULL)
    if (!is.data.frame(catlg) || nrow(catlg) == 0L) {
      return(data.frame())
    }
    # Industry / board scope only — do not apply Models filter (that is what we are driving).
    lab_merge_catalog_scores(
      catlg,
      scores = NULL,
      method_filter = character(0),
      industry_filter = lab_im_industry_filter(),
      eq_only = FALSE,
      gate_only = FALSE
    )
  })

  observeEvent(
    list(.lab_im_method_scope_catalog(), lazy_mount_nonce()),
    {
      scope <- tryCatch(.lab_im_method_scope_catalog(), error = function(e) data.frame())
      available <- tryCatch(
        lab_im_available_primary_methods(scope),
        error = function(e) character(0)
      )
      available <- intersect(as.character(LAB_SIDEBAR_METHOD_ORDER), as.character(available))
      # Catalog not ready yet: keep HTML Absolute defaults.
      # Do not updateCheckboxGroupInput(selected=empty) and do not push an empty
      # lock payload (client lock would uncheck every method when avail=[]).
      if (!length(available)) {
        return()
      }
      cur <- isolate(as.character(input$lab_im_methods %||% character(0)))
      cur <- cur[nzchar(cur)]
      # Keep user picks among still-available methods.
      # Empty (pre-bind / remount): APP_DEFAULTS Absolute-only — never force Relatives on.
      defaults <- intersect(
        as.character(APP_DEFAULTS$lab_im_methods %||% character(0)),
        available
      )
      # Stale "select all" from an older default (Absolute defaults never include Relative):
      # snap back to Absolute defaults on first catalog bind so Relatives stay off.
      rel_keys <- as.character(LAB_METHOD_REL_KEYS)
      default_rel <- intersect(
        as.character(APP_DEFAULTS$lab_im_methods %||% character(0)),
        rel_keys
      )
      if (length(cur) && !length(default_rel) &&
          any(rel_keys %in% cur) && setequal(sort(cur), sort(available))) {
        cur <- character(0)
      }
      new_sel <- if (length(cur)) {
        intersect(cur, available)
      } else {
        defaults
      }
      if (!length(new_sel) && length(defaults)) new_sel <- defaults
      if (!length(new_sel) && length(available)) {
        abs_fallback <- intersect(as.character(LAB_METHOD_ABS_KEYS), available)
        new_sel <- if (length(abs_fallback)) abs_fallback else available
      }
      tryCatch(
        updateCheckboxGroupInput(session, "lab_im_methods", selected = new_sel),
        error = function(e) NULL
      )
      session$sendCustomMessage(
        "ynowLabImMethodsLock",
        list(
          available = as.character(available),
          all = as.character(LAB_SIDEBAR_METHOD_ORDER)
        )
      )
    },
    ignoreInit = FALSE
  )

  output$lab_im_summary <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "bluechip"))
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    empty_msg <- tryCatch(
      ui_str("lab_im_summary_empty", loc),
      error = function(e) "目前複選下無產業／候選"
    )
    msg_col <- if (identical(loc, "en")) "Message" else "訊息"
    scope <- .lab_im_method_scope_catalog()
    if (!is.data.frame(scope) || nrow(scope) == 0L) {
      return(ynow_sortable_datatable(
        stats::setNames(data.frame(empty_msg, stringsAsFactors = FALSE), msg_col)
      ))
    }
    sm <- lab_method_group_summary(scope, include_all = TRUE, locale = loc)
    if (is.null(sm) || nrow(sm) == 0) {
      return(ynow_sortable_datatable(
        stats::setNames(data.frame(empty_msg, stringsAsFactors = FALSE), msg_col)
      ))
    }
    want <- if (identical(loc, "en")) {
      c("Suggested valuation method", "Industries", "Candidates")
    } else {
      c("建議評價方法", "產業數", "候選檔數")
    }
    ynow_sortable_datatable(sm[, want, drop = FALSE])
  }, server = TRUE)

  # Live pool size under current query filters (industry × model × board × ADR).
  # Quality checkboxes apply only through the on-disk quality snapshot.
  output$lab_im_filter_count <- renderUI({
    req(identical(.ynow_sidebar_tab(), "bluechip"))
    lab_quality_snap_nonce()
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    catlg <- tryCatch(lab_im_catalog(), error = function(e) NULL)
    mm <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
    include_adr <- if (identical(mm, "TW")) {
      TRUE
    } else {
      isTRUE(input$lab_im_include_adr %||% TRUE)
    }
    n_struct <- if (is.data.frame(catlg) && nrow(catlg) > 0L) {
      lab_count_filter_pool_tickers(
        catlg,
        method_filter = input$lab_im_methods,
        industry_filter = lab_im_industry_filter(),
        include_adr = include_adr,
        market_mode = mm
      )
    } else {
      0L
    }
    struct <- if (is.data.frame(catlg) && nrow(catlg) > 0L) {
      tryCatch(
        lab_structural_filter_pool(
          catlg,
          method_filter = input$lab_im_methods,
          industry_filter = lab_im_industry_filter(),
          include_adr = include_adr,
          market_mode = mm
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    snap <- tryCatch(lab_load_quality_snapshot(mm), error = function(e) NULL)
    gated <- tryCatch(
      lab_apply_quality_snapshot(
        if (is.data.frame(struct)) struct$ticker else character(0),
        snap,
        eq_only = isTRUE(input$lab_im_eq_only),
        gate_only = isTRUE(input$lab_im_gate_only),
        no_alert = isTRUE(input$lab_im_no_alert)
      ),
      error = function(e) NULL
    )
    n_pool <- if (is.list(gated)) as.integer(gated$n_show) else as.integer(n_struct)
    n_pool <- as.integer(n_pool %||% 0L)[1]
    if (!is.finite(n_pool) || n_pool < 0L) n_pool <- 0L
    snap_ui <- NULL
    if (is.list(gated)) {
      if (!is.finite(gated$n_covered) || gated$n_covered < 1L) {
        snap_txt <- tryCatch(
          ui_str("lab_im_quality_snap_none", loc),
          error = function(e) ""
        )
      } else {
        when <- tryCatch(
          format(gated$updated_at, "%Y-%m-%d %H:%M", tz = "Asia/Taipei"),
          error = function(e) ""
        )
        age_sec <- tryCatch(
          as.numeric(difftime(Sys.time(), gated$updated_at, units = "secs")),
          error = function(e) NA_real_
        )
        ago <- if (!is.finite(age_sec) || age_sec < 90) {
          tryCatch(ui_str("lab_im_quality_ago_now", loc), error = function(e) "")
        } else if (age_sec < 3600) {
          tryCatch(
            .ui_msg("lab_im_quality_ago_min", n = max(1L, as.integer(round(age_sec / 60))), loc = loc),
            error = function(e) ""
          )
        } else if (age_sec < 86400 * 2) {
          tryCatch(
            .ui_msg("lab_im_quality_ago_hr", n = max(1L, as.integer(round(age_sec / 3600))), loc = loc),
            error = function(e) ""
          )
        } else {
          tryCatch(
            .ui_msg("lab_im_quality_ago_day", n = max(1L, as.integer(round(age_sec / 86400))), loc = loc),
            error = function(e) ""
          )
        }
        snap_txt <- tryCatch(
          .ui_msg(
            "lab_im_quality_snap_status",
            when = when, ago = ago,
            covered = as.integer(gated$n_covered),
            pool = as.integer(gated$n_pool),
            loc = loc
          ),
          error = function(e) ""
        )
        if (isTRUE(gated$stale)) {
          stale <- tryCatch(ui_str("lab_im_quality_snap_stale", loc), error = function(e) "")
          if (nzchar(stale)) snap_txt <- paste(snap_txt, stale)
        }
        if (isTRUE(gated$gates_on) && is.finite(gated$n_missing) && gated$n_missing > 0L) {
          miss <- tryCatch(
            .ui_msg("lab_im_quality_snap_missing", n = as.integer(gated$n_missing), loc = loc),
            error = function(e) ""
          )
          if (nzchar(miss)) snap_txt <- paste(snap_txt, miss)
        }
      }
      if (nzchar(snap_txt %||% "")) {
        snap_ui <- tags$span(
          id = "ynow_lab_im_quality_snap_status",
          class = "ynow-lab-im-filter-count-hint",
          snap_txt
        )
      }
    }

    scores <- tryCatch(lab_im_scores(), error = function(e) NULL)
    qual_ui <- NULL
    if (is.data.frame(scores) && nrow(scores) > 0L && is.data.frame(catlg) && nrow(catlg) > 0L) {
      merged <- lab_merge_catalog_scores(
        catlg,
        scores = scores,
        method_filter = input$lab_im_methods,
        industry_filter = lab_im_industry_filter(),
        eq_only = FALSE,
        gate_only = FALSE,
        evaluated_only = TRUE
      )
      if (!identical(mm, "TW") &&
          exists("lab_filter_pool_adr", mode = "function") &&
          !isTRUE(include_adr)) {
        merged <- lab_filter_pool_adr(merged, include_adr = FALSE)
      }
      pool_q <- lab_leaderboard_pool(
        merged,
        eq_only = isTRUE(input$lab_im_eq_only),
        gate_only = isTRUE(input$lab_im_gate_only),
        no_alert = isTRUE(input$lab_im_no_alert)
      )
      n_q <- if (is.null(pool_q) || !nrow(pool_q)) {
        0L
      } else {
        length(unique(as.character(pool_q$ticker)))
      }
      qual_txt <- tryCatch(
        .ui_msg("lab_im_filter_count_qualified", n = as.integer(n_q), loc = loc),
        error = function(e) sprintf("Qualified after last Search: %d names", as.integer(n_q))
      )
      qual_ui <- tags$span(
        id = "ynow_lab_im_filter_count_qualified",
        class = "ynow-lab-im-filter-count-hint",
        qual_txt
      )
    }

    tags$div(
      class = "ynow-lab-im-filter-count",
      role = "status",
      `aria-live` = "polite",
      tags$span(
        id = "ynow_lab_im_filter_count_label",
        class = "ynow-lab-im-filter-count-label",
        tryCatch(
          ui_str("lab_im_filter_count_label", loc),
          error = function(e) "目前篩選條件下池內股票數"
        )
      ),
      tags$span(
        id = "ynow_lab_im_filter_count_n",
        class = "ynow-lab-im-filter-count-n",
        format(n_pool, big.mark = ",", scientific = FALSE)
      ),
      tags$span(
        id = "ynow_lab_im_filter_count_unit",
        class = "ynow-lab-im-filter-count-unit",
        tryCatch(
          ui_str("lab_im_filter_count_unit", loc),
          error = function(e) "檔"
        )
      ),
      tags$span(
        id = "ynow_lab_im_filter_count_hint",
        class = "ynow-lab-im-filter-count-hint",
        tryCatch(
          ui_str("lab_im_filter_count_hint", loc),
          error = function(e) ""
        )
      ),
      snap_ui,
      qual_ui
    )
  })

  lab_im_merged <- reactive({
    catlg <- lab_im_catalog()
    req(is.data.frame(catlg), nrow(catlg) > 0)
    out <- lab_merge_catalog_scores(
      catlg,
      scores = lab_im_scores(),
      method_filter = input$lab_im_methods,
      industry_filter = lab_im_industry_filter(),
      eq_only = FALSE,
      gate_only = FALSE,
      evaluated_only = TRUE
    )
    rank_mode <- lab_normalize_pool_rank_mode(input$lab_im_pool_rank %||% "mcap")
    concept_keys <- as.character(input$lab_im_concepts %||% character(0))
    if (identical(rank_mode, "concept") && length(concept_keys) > 0L &&
        exists("lab_filter_df_by_concept_keys", mode = "function")) {
      mm <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
      out <- lab_filter_df_by_concept_keys(out, concept_keys, market_mode = mm)
    }
    out
  })

  output$lab_im_leader_note <- renderUI({
    scores <- lab_im_scores()
    display_n <- lab_resolve_im_max_n(input$lab_im_max_n, input$lab_im_max_n_custom)
    max_n_label <- lab_resolve_im_max_n_label(input$lab_im_max_n, input$lab_im_max_n_custom)
    eval_n <- lab_resolve_im_eval_n(display_n)
    n_eval <- if (is.data.frame(scores) && nrow(scores) > 0) nrow(scores) else 0L
    scope <- as.character(input$lab_im_lb_mode %||% "overall")[1]
    if (!scope %in% c("overall", "by_industry", "undervalued")) scope <- "overall"
    gate_on <- isTRUE(input$lab_im_gate_only)
    eq_on <- isTRUE(input$lab_im_eq_only)
    no_alert_on <- isTRUE(input$lab_im_no_alert)
    loc_note <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    sep <- if (identical(loc_note, "en")) " / " else "／"
    scope_txt <- tryCatch(
      ui_str(
        if (identical(scope, "by_industry")) "lab_im_scope_by_ind"
        else if (identical(scope, "undervalued")) "lab_im_scope_undervalued"
        else "lab_im_scope_overall",
        loc_note
      ),
      error = function(e) scope
    )
    gate_txt <- paste0(
      if (gate_on) {
        tryCatch(ui_str("lab_im_gate_on", loc_note), error = function(e) "F-Score≥7")
      } else {
        tryCatch(ui_str("lab_im_gate_off", loc_note), error = function(e) "不設 F-Score 門檻")
      },
      sep,
      if (eq_on) {
        tryCatch(ui_str("lab_im_eq_on", loc_note), error = function(e) "盈餘品質通過")
      } else {
        tryCatch(ui_str("lab_im_eq_off", loc_note), error = function(e) "不過濾盈餘品質")
      },
      sep,
      if (no_alert_on) {
        tryCatch(ui_str("lab_im_no_alert_on", loc_note), error = function(e) "無財報警訊")
      } else {
        tryCatch(ui_str("lab_im_no_alert_off", loc_note), error = function(e) "不過濾財報警訊")
      }
    )
    if (n_eval == 0L) {
      cap_txt <- if (is.finite(display_n)) {
        tryCatch(
          .ynow_fmt_str(
            ui_str("lab_im_leader_note_cap_finite", loc_note),
            eval_n = as.character(eval_n), max_n = max_n_label
          ),
          error = function(e) paste0("will evaluate about ", eval_n, "; Detail cap ", max_n_label)
        )
      } else {
        tryCatch(
          ui_str("lab_im_leader_note_cap_all", loc_note),
          error = function(e) "will evaluate all filtered candidates"
        )
      }
      txt <- tryCatch(
        .ynow_fmt_str(
          ui_str("lab_im_leader_note_waiting", loc_note),
          cap = cap_txt, gate = gate_txt, scope = scope_txt
        ),
        error = function(e) paste("No evaluation yet.", cap_txt)
      )
      return(tags$p(style = "color:#888; font-size:12.5px;", txt))
    }
    sort_key_txt <- tryCatch(
      ui_str(
        if (identical(scope, "undervalued")) "lab_im_sort_key_total" else "lab_im_sort_key_cagr",
        loc_note
      ),
      error = function(e) ""
    )
    txt <- tryCatch(
      .ynow_fmt_str(
        ui_str("lab_im_leader_note_ready", loc_note),
        n = as.integer(n_eval), max_n = max_n_label,
        sort_key = sort_key_txt, gate = gate_txt, scope = scope_txt
      ),
      error = function(e) sprintf(
        "Evaluated %d; N=%s. %s; gate=%s; view=%s.",
        n_eval, max_n_label, sort_key_txt, gate_txt, scope_txt
      )
    )
    tags$p(style = "color:#555; font-size:12.5px;", txt)
  })

  # Refresh truncate-rule / N / concept labels when market / locale changes
  # (also after Blue Chip lazy mount so newly created inputs get locale choices).
  # Defer while Blue Chip eval/universe is busy so ZH/EN chrome can finish first.
  observe({
    lazy_mount_nonce()
    if (!isTRUE(.ynow_bluechip_mounted())) return()
    mm <- tryCatch(market_mode(), error = function(e) "US")
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .lab_im_schedule_picker_locale(function() {
      mm2 <- tryCatch(isolate(market_mode()), error = function(e) mm)
      loc2 <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) loc)
      rank_cur <- as.character(isolate(input$lab_im_pool_rank) %||% "mcap")[1]
      rank_choices <- lab_im_pool_rank_choices(loc2)
      if (!rank_cur %in% unname(rank_choices)) rank_cur <- "mcap"
      updateSelectInput(session, "lab_im_pool_rank", choices = rank_choices, selected = rank_cur)

      max_n_cur <- as.character(isolate(input$lab_im_max_n) %||% "25")[1]
      max_n_choices <- lab_im_max_n_select_choices(loc2)
      if (!max_n_cur %in% unname(max_n_choices)) max_n_cur <- "25"
      updateSelectInput(session, "lab_im_max_n", choices = max_n_choices, selected = max_n_cur)

      .lab_im_push_concept_choices(mm2, locale = loc2, clear_selected = FALSE)
    })
  })

  # Legacy Ranking view value moved to dedicated board above Candidate truncate
  observeEvent(input$lab_im_lb_mode, {
    if (identical(as.character(input$lab_im_lb_mode %||% "")[1], "industry_avg")) {
      updateRadioButtons(session, "lab_im_lb_mode", selected = "overall")
    }
  }, ignoreInit = FALSE)

  # Dedicated industry-chain mcap-weighted board — independent all-chains job
  # (decoupled from Search / page filters). Snapshot-only UI; bundled snap while busy.
  .lab_im_apply_ind_mcap_result <- function(out, notify = TRUE) {
    loc <- tryCatch(
      normalize_ui_locale(out$locale %||% isolate(ui_locale())),
      error = function(e) "zh-TW"
    )
    if (is.null(out) || !is.list(out) || !isTRUE(out$ok) ||
        !is.data.frame(out$snapshot) || nrow(out$snapshot) < 1L) {
      if (isTRUE(notify)) {
        showNotification(
          paste0(
            "Industry-chain ranking failed",
            if (nzchar(as.character(out$reason %||% "")[1]))
              paste0(": ", as.character(out$reason)[1]) else "."
          ),
          type = "warning",
          duration = 8
        )
      }
      return(invisible(FALSE))
    }
    new_snap <- out$snapshot
    new_has_upside <- any(
      is.finite(suppressWarnings(as.numeric(new_snap$wavg_cagr))),
      na.rm = TRUE
    )
    if (!isTRUE(new_has_upside)) {
      old <- tryCatch(isolate(lab_im_ind_mcap_snap()), error = function(e) NULL)
      old_has_upside <- is.data.frame(old) && nrow(old) > 0L &&
        any(is.finite(suppressWarnings(as.numeric(old$wavg_cagr))), na.rm = TRUE)
      if (isTRUE(old_has_upside)) {
        # Keep prior heatmap when refresh cannot compute upside (e.g. offline Yahoo).
        if (isTRUE(notify)) {
          showNotification(
            tryCatch(
              ui_str("lab_im_ind_mcap_refresh_kept_prior", loc),
              error = function(e) if (identical(loc, "en")) {
                "Industry-chain refresh finished without upside samples; kept prior ranking."
              } else {
                "產業鏈刷新未取得估值漲幅樣本，仍保留先前排名。"
              }
            ),
            type = "warning",
            duration = 7
          )
        }
        return(invisible(TRUE))
      }
    }
    lab_im_ind_mcap_snap(new_snap)
    if (isTRUE(notify)) {
      n_ch <- as.integer(out$n_chains %||% nrow(out$snapshot))
      showNotification(
        tryCatch(
          sprintf(ui_str("lab_im_ind_mcap_refresh_done", loc), n_ch),
          error = function(e) sprintf("已更新產業鏈排名（%d 條）。", n_ch)
        ),
        type = "message",
        duration = 6
      )
    }
    invisible(TRUE)
  }

  .lab_im_start_ind_mcap_job <- function(reason = "manual", notify_busy = TRUE) {
    loc <- tryCatch(normalize_ui_locale(isolate(ui_locale())), error = function(e) "zh-TW")
    mm <- tryCatch(normalize_market_mode(isolate(market_mode())), error = function(e) "US")
    # Full market catalog — ignore page boards / industry / methods filters.
    catlg <- tryCatch(
      lab_build_industry_method_catalog(
        market_mode = mm,
        boards = c("TWSE", "TPEX")
      ),
      error = function(e) NULL
    )
    if (!is.data.frame(catlg) || nrow(catlg) < 1L) {
      catlg <- tryCatch(isolate(lab_im_catalog()), error = function(e) NULL)
    }
    if (!is.data.frame(catlg) || nrow(catlg) < 1L) {
      return(invisible(FALSE))
    }
    gen <- isolate(as.integer(lab_im_ind_mcap_gen()) %||% 0L) + 1L
    lab_im_ind_mcap_gen(gen)
    args <- list(.gen = gen, catlg = catlg, market_mode = mm, locale = loc, per_chain = 2L)
    running <- isTRUE(isolate(lab_im_ind_mcap_busy()))
    if (!is.null(lab_im_ind_mcap_task)) {
      st <- tryCatch(lab_im_ind_mcap_task$status(), error = function(e) "error")
      if (identical(st, "running")) running <- TRUE
    }
    if (isTRUE(running)) {
      lab_im_ind_mcap_pending(args)
      return(invisible(TRUE))
    }
    .lab_im_invoke_ind_mcap(args)
  }

  .lab_im_invoke_ind_mcap <- function(args) {
    if (!is.list(args) || !is.data.frame(args$catlg) || nrow(args$catlg) < 1L) {
      return(invisible(FALSE))
    }
    lab_im_ind_mcap_busy(TRUE)
    lab_im_ind_mcap_pending(NULL)
    .ynow_inflight_gen$lab_im_ind_mcap <- as.integer(args$.gen %||% 0L)[1]
    if (!is.null(lab_im_ind_mcap_task)) {
      tryCatch(
        lab_im_ind_mcap_task$invoke(
          catlg = args$catlg,
          market_mode = args$market_mode,
          locale = args$locale,
          per_chain = as.integer(args$per_chain %||% 2L)[1]
        ),
        error = function(e) {
          lab_im_ind_mcap_busy(FALSE)
          showNotification(
            paste("Industry-chain ranking start failed:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
        }
      )
      return(invisible(TRUE))
    }
    on.exit(tryCatch(lab_im_ind_mcap_busy(FALSE), error = function(e) NULL), add = TRUE)
    out <- tryCatch(
      lab_im_run_ind_mcap_job(
        catlg = args$catlg,
        market_mode = args$market_mode,
        locale = args$locale,
        per_chain = as.integer(args$per_chain %||% 2L)[1]
      ),
      error = function(e) list(ok = FALSE, reason = conditionMessage(e))
    )
    lab_im_ind_mcap_busy(FALSE)
    gen <- as.integer(args$.gen %||% 0L)[1]
    if (identical(isolate(as.integer(lab_im_ind_mcap_gen()) %||% 0L), gen)) {
      .lab_im_apply_ind_mcap_result(out, notify = TRUE)
    }
    invisible(TRUE)
  }

  .lab_im_finish_ind_mcap_slot <- function(out = NULL, apply = TRUE) {
    tryCatch(lab_im_ind_mcap_busy(FALSE), error = function(e) NULL)
    pending <- isolate(lab_im_ind_mcap_pending())
    latest <- isolate(as.integer(lab_im_ind_mcap_gen()) %||% 0L)
    if (is.list(pending) && identical(as.integer(pending$.gen %||% 0L)[1], latest)) {
      lab_im_ind_mcap_pending(NULL)
      .lab_im_invoke_ind_mcap(pending)
      return(invisible(FALSE))
    }
    if (isTRUE(.ynow_inflight_stale("lab_im_ind_mcap", latest))) {
      return(invisible(TRUE))
    }
    if (isTRUE(apply)) {
      tryCatch(
        .lab_im_apply_ind_mcap_result(out, notify = TRUE),
        error = function(e) NULL
      )
    }
    invisible(TRUE)
  }

  if (!is.null(lab_im_ind_mcap_task)) {
    observeEvent(lab_im_ind_mcap_task$status(), {
      st <- tryCatch(lab_im_ind_mcap_task$status(), error = function(e) "initial")
      if (identical(st, "error")) {
        err <- tryCatch(lab_im_ind_mcap_task$result(), error = function(e) conditionMessage(e))
        if (is.character(err) && nzchar(err[1])) {
          tryCatch(
            showNotification(
              paste("Industry-chain ranking failed:", err[1]),
              type = "error",
              duration = 8
            ),
            error = function(e) NULL
          )
        }
        .lab_im_finish_ind_mcap_slot(apply = FALSE)
        return()
      }
      if (!identical(st, "success")) return()
      out <- tryCatch(lab_im_ind_mcap_task$result(), error = function(e) NULL)
      .lab_im_finish_ind_mcap_slot(out = out, apply = TRUE)
    }, ignoreInit = TRUE)
  }

  .lab_im_ind_mcap_board <- reactive({
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    snap <- lab_im_ind_mcap_snap()
    busy <- isTRUE(lab_im_ind_mcap_busy())
    n_chains_all <- tryCatch({
      length(unname(lab_industry_chain_picker_choices(loc)))
    }, error = function(e) 0L)

    # Snapshot-only board (independent of Search scores / page gates / methods).
    if (is.data.frame(snap) && nrow(snap) > 0L) {
      tbl <- tryCatch(
        lab_format_ind_mcap_chain_table(snap, loc),
        error = function(e) NULL
      )
      snap_at <- tryCatch(
        as.character(attr(snap, "snapshot_at") %||% snap$snapshot_at[[1]] %||% ""),
        error = function(e) ""
      )
      return(list(
        table = tbl,
        n_show = if (is.data.frame(tbl)) nrow(tbl) else nrow(snap),
        n_chains_all = max(as.integer(n_chains_all), nrow(snap)),
        n_qual = NA_integer_,
        n_eval = NA_integer_,
        waiting = FALSE,
        from_snapshot = TRUE,
        busy = busy,
        snapshot_at = snap_at,
        loc = loc
      ))
    }

    list(
      table = NULL,
      n_show = 0L,
      n_chains_all = as.integer(n_chains_all),
      n_qual = 0L,
      n_eval = 0L,
      waiting = TRUE,
      from_snapshot = FALSE,
      busy = busy,
      snapshot_at = NA_character_,
      loc = loc
    )
  })

  # Reload default snapshot when market switches — only if Blue Chip was opened.
  observeEvent(market_mode(), {
    if (!isTRUE(.ynow_bluechip_mounted())) return()
    mm <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
    snap <- tryCatch(
      lab_load_ind_mcap_chain_snapshot(mm, force = TRUE),
      error = function(e) lab_ind_mcap_chain_snapshot_empty()
    )
    lab_im_ind_mcap_snap(snap)
  }, ignoreInit = TRUE)

  # Opening Blue Chip tab auto-starts independent all-chains ind-mcap job (once / market).
  observeEvent(
    list(input$sidebar_tabs, lazy_mount_nonce()),
    {
      tab <- as.character(input$sidebar_tabs %||% "")[1]
      if (!identical(tab, "bluechip")) return()
      mounted <- tryCatch(
        isTRUE(.ynow_lazy_tab_ctrl$mounted("bluechip")),
        error = function(e) FALSE
      )
      if (!isTRUE(mounted)) return()
      if (isTRUE(isolate(lab_im_ind_mcap_auto_armed()))) return()
      if (isTRUE(isolate(lab_im_ind_mcap_busy()))) return()
      lab_im_ind_mcap_auto_armed(TRUE)
      session$onFlushed(function() {
        start_job <- function() {
          if (!identical(as.character(isolate(input$sidebar_tabs) %||% "")[1], "bluechip")) {
            tryCatch(lab_im_ind_mcap_auto_armed(FALSE), error = function(e) NULL)
            return()
          }
          .lab_im_start_ind_mcap_job(reason = "tab_enter", notify_busy = FALSE)
        }
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(start_job, delay = 0.05)
        } else {
          start_job()
        }
      }, once = TRUE)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$lab_im_refresh_ind_mcap, {
    .lab_im_start_ind_mcap_job(reason = "refresh_button", notify_busy = TRUE)
  })

  output$lab_im_ind_mcap_status <- renderUI({
    board <- .lab_im_ind_mcap_board()
    loc <- board$loc %||% "zh-TW"
    busy <- isTRUE(board$busy) || isTRUE(lab_im_ind_mcap_busy())
    if (isTRUE(board$waiting) && !busy) {
      return(tags$p(
        style = "color:#888; font-size:12.5px; margin:0 0 8px 0;",
        tryCatch(ui_str("lab_im_ind_mcap_waiting", loc),
                 error = function(e) "尚無預設排名。請按「更新排名」。")
      ))
    }
    if (isTRUE(board$from_snapshot)) {
      at <- as.character(board$snapshot_at %||% "")[1]
      at_bit <- if (nzchar(at) && !is.na(at)) {
        tryCatch(
          sprintf(ui_str("lab_im_ind_mcap_snap_at", loc), at),
          error = function(e) sprintf("（資料時間 %s）", at)
        )
      } else {
        ""
      }
      txt <- tryCatch(
        sprintf(
          ui_str("lab_im_ind_mcap_status_snap", loc),
          as.integer(board$n_show), at_bit
        ),
        error = function(e) sprintf(
          "預設排名：%d 條產業鏈%s。按「更新排名」可重算最新版。",
          as.integer(board$n_show), at_bit
        )
      )
      if (busy) {
        busy_txt <- tryCatch(
          ui_str("lab_im_ind_mcap_refresh_busy", loc),
          error = function(e) "產業鏈排名更新進行中…"
        )
        txt <- paste0(txt, " ", busy_txt)
      }
      return(tags$p(style = "color:#555; font-size:12.5px; margin:0 0 8px 0;", txt))
    }
    if (busy) {
      return(tags$p(
        style = "color:#555; font-size:12.5px; margin:0 0 8px 0;",
        tryCatch(
          ui_str("lab_im_ind_mcap_refresh_busy", loc),
          error = function(e) "產業鏈排名更新進行中…"
        )
      ))
    }
    tags$p(
      style = "color:#888; font-size:12.5px; margin:0 0 8px 0;",
      tryCatch(ui_str("lab_im_ind_mcap_waiting", loc),
               error = function(e) "尚無預設排名。開啟績優股排行榜會自動計算，或按「更新排名」。")
    )
  })

  output$lab_im_ind_mcap_treemap <- plotly::renderPlotly({
    req(identical(.ynow_sidebar_tab(), "bluechip"))
    board <- .lab_im_ind_mcap_board()
    loc <- board$loc %||% "zh-TW"
    mm <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
    snap <- lab_im_ind_mcap_snap()
    if (isTRUE(board$waiting) || is.null(snap) || !is.data.frame(snap) || nrow(snap) < 1L) {
      snap <- lab_ind_mcap_chain_snapshot_empty()
    }
    gain_scheme <- tryCatch({
      s <- values$user_settings
      as.character(s$gain_loss_scheme %||% "auto")[1]
    }, error = function(e) "auto")
    lab_ind_mcap_treemap_plotly(
      snap,
      market = mm,
      locale = loc,
      gain_scheme = gain_scheme
    )
  })

  output$lab_im_ind_mcap_table <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "bluechip"))
    board <- .lab_im_ind_mcap_board()
    loc <- board$loc %||% "zh-TW"
    msg_col <- if (identical(loc, "en")) "Message" else "訊息"
    if (isTRUE(board$waiting)) {
      msg <- tryCatch(ui_str("lab_im_ind_mcap_waiting", loc),
                      error = function(e) "尚無預設排名。開啟績優股排行榜會自動計算，或按「更新排名」。")
      return(ynow_sortable_datatable(
        stats::setNames(data.frame(msg, stringsAsFactors = FALSE), msg_col)
      ))
    }
    tbl <- board$table
    if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) {
      msg <- tryCatch(
        sprintf(
          ui_str("lab_im_ind_mcap_empty", loc),
          as.integer(board$n_qual %||% 0L), as.integer(board$n_eval %||% 0L)
        ),
        error = function(e) "尚無產業鏈列可顯示。"
      )
      return(ynow_sortable_datatable(
        stats::setNames(data.frame(msg, stringsAsFactors = FALSE), msg_col)
      ))
    }
    ynow_sortable_datatable(lab_localize_im_colnames(tbl, loc))
  }, server = TRUE)

  output$lab_im_leaderboard <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "bluechip"))
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    msg_col <- if (identical(loc, "en")) "Message" else "訊息"
    scores <- lab_im_scores()
    rank_mode_lb <- lab_normalize_pool_rank_mode(input$lab_im_pool_rank %||% "mcap")
    concept_keys_lb <- as.character(input$lab_im_concepts %||% character(0))
    if (is.null(scores) || !is.data.frame(scores) || nrow(scores) == 0) {
      msg <- tryCatch(ui_str("lab_im_lb_none", loc),
                      error = function(e) "（尚無績優排行 — 請先按「搜尋績優股」）")
      return(ynow_sortable_datatable(
        stats::setNames(data.frame(msg, stringsAsFactors = FALSE), msg_col)
      ))
    }
    merged <- tryCatch(lab_im_merged(), error = function(e) NULL)
    if (is.null(merged) || nrow(merged) == 0) {
      n_scored <- nrow(scores)
      n_pass <- sum(is.finite(suppressWarnings(as.numeric(scores$f_score))) &
                      suppressWarnings(as.numeric(scores$f_score)) >= 7, na.rm = TRUE)
      msg <- tryCatch(
        .ynow_fmt_str(
          ui_str("lab_im_lb_filter_empty", loc),
          n = as.integer(n_scored), pass = as.integer(n_pass)
        ),
        error = function(e) paste0(
          "評估有 ", n_scored, " 檔（F-Score≥7 通過 ", n_pass, "），",
          "但目前複選條件下明細為空。"
        )
      )
      return(ynow_sortable_datatable(
        stats::setNames(data.frame(msg, stringsAsFactors = FALSE), msg_col)
      ))
    }
    scope <- as.character(input$lab_im_lb_mode %||% "overall")[1]
    if (!scope %in% c("overall", "by_industry", "undervalued")) scope <- "overall"
    # overall = Top 10 across all evaluated industries (annualized upside);
    # by_industry = Top 10 within selected industries (Industry rank from 1 within each industry);
    # undervalued = Top 10 by total FV–price gap (primary model; still undervalued only)
    # (industry mcap-weighted board is a dedicated auto block above Candidate truncate)
    lb_src <- merged
    ind_filter <- character(0)
    if (scope %in% c("overall", "undervalued")) {
      catlg <- tryCatch(lab_im_catalog(), error = function(e) NULL)
      if (is.data.frame(catlg) && nrow(catlg) > 0) {
        lb_src <- tryCatch(
          lab_merge_catalog_scores(
            catlg,
            scores = scores,
            method_filter = input$lab_im_methods,
            industry_filter = character(0),
            eq_only = FALSE,
            gate_only = FALSE,
            evaluated_only = TRUE
          ),
          error = function(e) merged
        )
      }
    } else {
      ind_filter <- as.character(lab_im_industry_filter() %||% character(0))
    }
    if (identical(rank_mode_lb, "concept") && length(concept_keys_lb) > 0L &&
        exists("lab_filter_df_by_concept_keys", mode = "function")) {
      mm_lb <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
      lb_src <- lab_filter_df_by_concept_keys(lb_src, concept_keys_lb, market_mode = mm_lb)
    }
    lb <- lab_quality_leaderboard(
      lb_src,
      top_n = 10L,
      eq_only = isTRUE(input$lab_im_eq_only),
      gate_only = isTRUE(input$lab_im_gate_only),
      no_alert = isTRUE(input$lab_im_no_alert),
      scope = scope,
      industry_filter = ind_filter
    )
    if (nrow(lb) == 0) {
      pool <- lab_leaderboard_pool(
        if (identical(scope, "by_industry")) merged else lb_src,
        eq_only = isTRUE(input$lab_im_eq_only),
        gate_only = isTRUE(input$lab_im_gate_only),
        no_alert = isTRUE(input$lab_im_no_alert)
      )
      fs_m <- suppressWarnings(as.numeric(merged$f_score))
      n_f7 <- sum(is.finite(fs_m) & fs_m >= 7, na.rm = TRUE)
      n_up <- sum(is.finite(merged$upside_cagr_pct), na.rm = TRUE)
      loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
      msg <- tryCatch(
        sprintf(
          ui_str("lab_im_lb_empty", loc),
          as.integer(nrow(merged)), as.integer(n_up), as.integer(n_f7), as.integer(nrow(pool))
        ),
        error = function(e) paste0(
          "前十名尚無列可顯示。已評估 ", nrow(merged),
          " 檔；能量到年化漲幅 ", n_up,
          "；F-Score≥7 通過 ", n_f7,
          "；目前勾選條件下合格 ", nrow(pool),
          "。說明：N 是分析後顯示上限；前十名只從「合格者」取最多 10 檔，不會補足到 10。"
        )
      )
      return(ynow_sortable_datatable(
        stats::setNames(data.frame(msg, stringsAsFactors = FALSE), msg_col)
      ))
    }
    if ("產業" %in% names(lb) && "industry_key" %in% names(lb_src) &&
        exists("industry_label", mode = "function")) {
      tk <- as.character(lb[["代號"]] %||% character(0))
      src_tk <- if (exists("display_tickers_for_market", mode = "function")) {
        display_tickers_for_market(lb_src$ticker, tryCatch(market_mode(), error = function(e) "US"))
      } else {
        as.character(lb_src$ticker)
      }
      idx <- match(tk, src_tk)
      keys <- ifelse(
        is.na(idx),
        NA_character_,
        as.character(lb_src$industry_key[idx])
      )
      ok <- !is.na(keys) & nzchar(keys)
      if (any(ok)) {
        lb$產業[ok] <- vapply(
          keys[ok],
          function(k) tryCatch(industry_label(k, locale = loc), error = function(e) k),
          character(1)
        )
      }
    }
    ynow_sortable_datatable(lab_localize_im_colnames(lb, loc))
  }, server = TRUE)

  output$lab_im_leaderboard_status <- renderUI({
    scores <- lab_im_scores()
    if (is.null(scores) || !is.data.frame(scores) || nrow(scores) == 0) return(NULL)
    merged <- tryCatch(lab_im_merged(), error = function(e) NULL)
    if (is.null(merged) || nrow(merged) == 0) return(NULL)
    gate_on <- isTRUE(input$lab_im_gate_only)
    eq_on <- isTRUE(input$lab_im_eq_only)
    no_alert_on <- isTRUE(input$lab_im_no_alert)
    scope <- as.character(input$lab_im_lb_mode %||% "overall")[1]
    if (!scope %in% c("overall", "by_industry", "undervalued")) scope <- "overall"
    lb_src <- merged
    ind_filter <- character(0)
    if (scope %in% c("overall", "undervalued")) {
      catlg <- tryCatch(lab_im_catalog(), error = function(e) NULL)
      if (is.data.frame(catlg) && nrow(catlg) > 0) {
        lb_src <- tryCatch(
          lab_merge_catalog_scores(
            catlg,
            scores = scores,
            method_filter = input$lab_im_methods,
            industry_filter = character(0),
            eq_only = FALSE,
            gate_only = FALSE,
            evaluated_only = TRUE
          ),
          error = function(e) merged
        )
      }
    } else {
      ind_filter <- as.character(lab_im_industry_filter() %||% character(0))
    }
    lb <- tryCatch(
      lab_quality_leaderboard(
        lb_src, top_n = 10L, eq_only = eq_on, gate_only = gate_on,
        no_alert = no_alert_on,
        scope = scope, industry_filter = ind_filter
      ),
      error = function(e) NULL
    )
    pool <- tryCatch(
      lab_leaderboard_pool(
        if (identical(scope, "by_industry")) merged else lb_src,
        eq_only = eq_on, gate_only = gate_on, no_alert = no_alert_on
      ),
      error = function(e) NULL
    )
    n_show <- if (is.data.frame(lb)) nrow(lb) else 0L
    n_qual <- if (is.data.frame(pool)) nrow(pool) else 0L
    n_eval <- nrow(if (identical(scope, "by_industry")) merged else lb_src)
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    if (identical(scope, "by_industry")) {
      txt <- tryCatch(
        sprintf(
          ui_str("lab_im_lb_status_by_ind", loc),
          as.integer(n_show), as.integer(n_qual), as.integer(n_eval)
        ),
        error = function(e) sprintf(
          "選定產業內前十名顯示 %d／10（合格 %d／已評估 %d）。產業內排名自該產業第 1 名起算；不足不湊滿。",
          as.integer(n_show), as.integer(n_qual), as.integer(n_eval)
        )
      )
    } else if (identical(scope, "undervalued")) {
      txt <- tryCatch(
        sprintf(
          ui_str("lab_im_lb_status_undervalued", loc),
          as.integer(n_show), as.integer(n_qual), as.integer(n_eval)
        ),
        error = function(e) sprintf(
          "價值低估前十名顯示 %d／10（合格 %d／已評估 %d）。依推薦主模型總潛在漲幅（FV vs 市價）排序；年化漲幅僅供對照。",
          as.integer(n_show), as.integer(n_qual), as.integer(n_eval)
        )
      )
    } else {
      txt <- tryCatch(
        sprintf(ui_str("lab_im_lb_status", loc), as.integer(n_show), as.integer(n_qual), as.integer(n_eval)),
        error = function(e) sprintf(
          "前十名顯示 %d／10（合格 %d／已評估 %d）。N＝顯示上限；前十名只取合格者最多 10 檔，不會為湊滿 10 而另抽樣。",
          as.integer(n_show), as.integer(n_qual), as.integer(n_eval)
        )
      )
    }
    tags$p(
      class = "ynow-lab-im-lb-status",
      style = "margin:6px 0 10px 0; color:#555; font-size:12px;",
      txt
    )
  })

  output$lab_im_table <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "bluechip"))
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    msg_col <- if (identical(loc, "en")) "Message" else "訊息"
    catlg <- lab_im_catalog()
    req(is.data.frame(catlg), nrow(catlg) > 0)
    # 明細＝合格池取前 display_n（不足不湊滿）；品質勾選與排行榜同一合格定義
    merged <- lab_merge_catalog_scores(
      catlg,
      scores = lab_im_scores(),
      method_filter = input$lab_im_methods,
      industry_filter = lab_im_industry_filter(),
      eq_only = FALSE,
      gate_only = FALSE,
      evaluated_only = TRUE
    )
    rank_mode_dt <- lab_normalize_pool_rank_mode(input$lab_im_pool_rank %||% "mcap")
    concept_keys_dt <- as.character(input$lab_im_concepts %||% character(0))
    if (identical(rank_mode_dt, "concept") && length(concept_keys_dt) > 0L &&
        exists("lab_filter_df_by_concept_keys", mode = "function")) {
      mm_dt <- tryCatch(normalize_market_mode(market_mode()), error = function(e) "US")
      merged <- lab_filter_df_by_concept_keys(merged, concept_keys_dt, market_mode = mm_dt)
    }
    display_n <- lab_resolve_im_max_n(input$lab_im_max_n, input$lab_im_max_n_custom)
    # Unlimited / large N: show all rows (page_length=NULL → nrow); else page 20.
    page_len <- if (!is.finite(display_n) || display_n > 100) NULL else 20L
    eq_on <- isTRUE(input$lab_im_eq_only)
    gate_on <- isTRUE(input$lab_im_gate_only)
    no_alert_on <- isTRUE(input$lab_im_no_alert)
    if (nrow(merged) == 0) {
      scores <- lab_im_scores()
      msg <- if (is.null(scores) || !is.data.frame(scores) || nrow(scores) == 0) {
        tryCatch(ui_str("lab_im_detail_waiting", loc),
                 error = function(e) "尚未評估。請按「搜尋績優股」；明細最多顯示宇宙檔數 N 檔合格列（條件不足時不湊滿）。")
      } else {
        tryCatch(ui_str("lab_im_detail_no_match", loc),
                 error = function(e) "沒有符合篩選的已評估列。可放寬產業／模型，或重新評估。")
      }
      return(ynow_sortable_datatable(
        stats::setNames(data.frame(msg, stringsAsFactors = FALSE), msg_col)
      ))
    }
    merged <- lab_cap_detail_display(
      merged,
      display_n = display_n,
      eq_only = eq_on,
      gate_only = gate_on,
      no_alert = no_alert_on
    )
    if (is.null(merged) || nrow(merged) == 0L) {
      return(ynow_sortable_datatable(
        stats::setNames(
          data.frame(
            tryCatch(ui_str("lab_im_detail_no_qual", loc),
                     error = function(e) "目前勾選條件下尚無合格列可顯示（不會為湊滿 N 而另抽樣）。"),
            stringsAsFactors = FALSE
          ),
          msg_col
        )
      ))
    }
    yahoo_nm <- if ("company_name" %in% names(merged)) merged$company_name else NA_character_
    ind_disp <- if ("industry_key" %in% names(merged) && exists("industry_label", mode = "function")) {
      vapply(
        as.character(merged$industry_key),
        function(k) tryCatch(industry_label(k, locale = loc), error = function(e) k),
        character(1)
      )
    } else {
      merged$industry_label
    }
    show_df <- lab_localize_im_colnames(
      data.frame(
        代號 = display_tickers_for_market(merged$ticker, market_mode()),
        公司名稱 = vapply(
          seq_len(nrow(merged)),
          function(i) lab_company_display_name(merged$ticker[[i]], yahoo_nm[[i]]),
          character(1)
        ),
        年化估值漲幅 = ifelse(
          is.na(merged$upside_cagr_pct), "—",
          sprintf("%+.1f%%", merged$upside_cagr_pct)
        ),
        總潛在漲幅 = ifelse(
          is.na(merged$upside_total_pct), "",
          sprintf("%+.1f%%", merged$upside_total_pct)
        ),
        實際估值方法 = ifelse(
          is.na(merged$method_used) | !nzchar(as.character(merged$method_used)),
          "", toupper(as.character(merged$method_used))
        ),
        產業 = ind_disp,
        現價 = ifelse(is.na(merged$price), "", signif(merged$price, 4)),
        合理價 = ifelse(is.na(merged$fv), "", signif(merged$fv, 4)),
        `F-Score` = lab_html_fscore_pill(merged$f_score),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      loc
    )
    ynow_sortable_datatable(
      show_df,
      escape = FALSE,
      page_length = page_len,
      filter = "top"
    ) %>%
      DT::formatStyle(
        "F-Score",
        backgroundColor = "#f3f6fb",
        fontWeight = "700"
      )
  }, server = TRUE)

  lab_im_detail_export_df <- function() {
    catlg <- tryCatch(lab_im_catalog(), error = function(e) NULL)
    if (is.null(catlg) || !is.data.frame(catlg) || nrow(catlg) == 0) {
      return(data.frame(訊息 = "無目錄"))
    }
    merged <- lab_merge_catalog_scores(
      catlg,
      scores = lab_im_scores(),
      method_filter = isolate(input$lab_im_methods),
      industry_filter = isolate(lab_im_industry_filter()),
      eq_only = FALSE,
      gate_only = FALSE,
      evaluated_only = TRUE
    )
    rank_mode_ex <- lab_normalize_pool_rank_mode(
      isolate(input$lab_im_pool_rank) %||% "mcap"
    )
    concept_keys_ex <- as.character(isolate(input$lab_im_concepts) %||% character(0))
    if (identical(rank_mode_ex, "concept") && length(concept_keys_ex) > 0L &&
        exists("lab_filter_df_by_concept_keys", mode = "function")) {
      mm_ex <- tryCatch(
        normalize_market_mode(isolate(market_mode())),
        error = function(e) "US"
      )
      merged <- lab_filter_df_by_concept_keys(merged, concept_keys_ex, market_mode = mm_ex)
    }
    if (nrow(merged) == 0) {
      return(data.frame(訊息 = "目前篩選下無已評估明細列（請先按「搜尋績優股」）"))
    }
    display_n <- lab_resolve_im_max_n(
      isolate(input$lab_im_max_n),
      isolate(input$lab_im_max_n_custom)
    )
    merged <- lab_cap_detail_display(
      merged,
      display_n = display_n,
      eq_only = isTRUE(isolate(input$lab_im_eq_only)),
      gate_only = isTRUE(isolate(input$lab_im_gate_only)),
      no_alert = isTRUE(isolate(input$lab_im_no_alert))
    )
    if (is.null(merged) || nrow(merged) == 0L) {
      return(data.frame(訊息 = "目前勾選條件下尚無合格明細列（不足不湊滿）"))
    }
    yahoo_nm <- if ("company_name" %in% names(merged)) merged$company_name else NA_character_
    data.frame(
      代號 = display_tickers_for_market(merged$ticker, market_mode()),
      公司名稱 = vapply(
        seq_len(nrow(merged)),
        function(i) lab_company_display_name(merged$ticker[[i]], yahoo_nm[[i]]),
        character(1)
      ),
      年化估值漲幅 = ifelse(
        is.na(merged$upside_cagr_pct), "—",
        sprintf("%+.1f%%", merged$upside_cagr_pct)
      ),
      總潛在漲幅 = ifelse(
        is.na(merged$upside_total_pct), "",
        sprintf("%+.1f%%", merged$upside_total_pct)
      ),
      實際估值方法 = ifelse(
        is.na(merged$method_used) | !nzchar(as.character(merged$method_used)),
        "", toupper(as.character(merged$method_used))
      ),
      產業 = merged$industry_label,
      現價 = ifelse(is.na(merged$price), "", signif(merged$price, 4)),
      合理價 = ifelse(is.na(merged$fv), "", signif(merged$fv, 4)),
      `F-Score` = ifelse(is.na(merged$f_score), "", as.character(as.integer(round(merged$f_score)))),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  output$lab_im_download_report <- downloadHandler(
    filename = function() {
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      if (isTRUE(lab_lab_report_use_zip())) {
        paste0("YNow_Lab_US_", ts, ".zip")
      } else {
        paste0("YNow_Lab_US_", ts, ".md")
      }
    },
    content = function(file) {
      if (isTRUE(.ynow_deny_guest_download(file))) return()
      if (isTRUE(.ynow_access_deny_download("report:lab_im_download_report", file))) return()
      tryCatch(
        .ynow_access_note_conditional_use("report:lab_im_download_report", consume_free = TRUE),
        error = function(e) NULL
      )
      meta <- tryCatch(lab_us_universe_meta(), error = function(e) NULL)
      if (is.null(meta) || as.integer(meta$n %||% 0L) < 1L) {
        meta <- tryCatch(lab_sp500_universe_meta(), error = function(e) NULL)
      }
      n_uni <- if (is.null(meta)) 0L else as.integer(meta$n %||% 0L)
      fetched <- if (is.null(meta)) "—" else lab_format_fetched_at(meta$fetched_at)
      src <- if (is.null(meta)) "" else as.character(meta$source %||% "")[1]
      n_un <- if (is.null(meta)) 0L else as.integer(meta$n_unmapped %||% 0L)
      all_ind <- unname(lab_industry_picker_choices())
      ind_sel <- lab_normalize_multi_filter(isolate(lab_im_industry_filter()))
      ind_txt <- if (!length(ind_sel) || (length(all_ind) > 0 && setequal(ind_sel, all_ind))) {
        sprintf("全部產業（%d）", length(all_ind))
      } else {
        labs <- vapply(ind_sel, function(k) {
          if (identical(k, LAB_UNMAPPED_KEY)) LAB_UNMAPPED_LABEL else
            as.character(industry_labels[k] %||% k)[1]
        }, character(1))
        paste(labs, collapse = "、")
      }
      meth_sel <- lab_order_methods_like_sidebar(
        lab_normalize_multi_filter(isolate(input$lab_im_methods))
      )
      meth_txt <- if (!length(meth_sel)) {
        "不過濾"
      } else {
        paste(unname(LAB_METHOD_LABELS[meth_sel]), collapse = "、")
      }
      eq_on <- isTRUE(isolate(input$lab_im_eq_only))
      gate_on <- isTRUE(isolate(input$lab_im_gate_only))
      no_alert_on <- isTRUE(isolate(input$lab_im_no_alert))
      max_n <- lab_resolve_im_max_n(isolate(input$lab_im_max_n), isolate(input$lab_im_max_n_custom))
      max_n_label <- lab_resolve_im_max_n_label(isolate(input$lab_im_max_n), isolate(input$lab_im_max_n_custom))
      eval_n <- lab_resolve_im_eval_n(max_n)
      rank_mode <- lab_normalize_pool_rank_mode(isolate(input$lab_im_pool_rank %||% "mcap"))
      rank_choices <- lab_im_pool_rank_choices("zh-TW")
      rank_mode_label <- {
        hit <- names(rank_choices)[match(rank_mode, unname(rank_choices))]
        if (length(hit) == 1L && !is.na(hit) && nzchar(hit)) hit else rank_mode
      }
      scores <- lab_im_scores()
      evaluated <- is.data.frame(scores) && nrow(scores) > 0
      catlg <- tryCatch(lab_im_catalog(), error = function(e) NULL)
      filtered <- if (is.data.frame(catlg) && nrow(catlg) > 0) {
        lab_merge_catalog_scores(
          catlg, scores = NULL,
          method_filter = character(0),
          industry_filter = isolate(lab_im_industry_filter()),
          eq_only = FALSE, gate_only = FALSE
        )
      } else {
        data.frame()
      }
      sm <- tryCatch(
        lab_method_group_summary(filtered, include_all = TRUE),
        error = function(e) NULL
      )
      if (!is.null(sm) && nrow(sm) > 0 && "method_key" %in% names(sm)) {
        sm <- sm[, c("建議評價方法", "產業數", "候選檔數"), drop = FALSE]
      }
      lb <- NULL
      if (isTRUE(evaluated)) {
        merged_lb <- tryCatch(lab_im_merged(), error = function(e) NULL)
        if (is.data.frame(merged_lb) && nrow(merged_lb) > 0) {
          lb_scope <- as.character(isolate(input$lab_im_lb_mode) %||% "overall")[1]
          if (!lb_scope %in% c("overall", "by_industry", "undervalued")) lb_scope <- "overall"
          lb_src <- merged_lb
          ind_filter <- character(0)
          if (lb_scope %in% c("overall", "undervalued")) {
            lb_src <- tryCatch(
              lab_merge_catalog_scores(
                catlg,
                scores = scores,
                method_filter = isolate(input$lab_im_methods),
                industry_filter = character(0),
                eq_only = FALSE,
                gate_only = FALSE,
                evaluated_only = TRUE
              ),
              error = function(e) merged_lb
            )
          } else {
            ind_filter <- as.character(isolate(lab_im_industry_filter()) %||% character(0))
          }
          lb <- lab_quality_leaderboard(
            lb_src, top_n = 10L, eq_only = eq_on,
            gate_only = isTRUE(isolate(input$lab_im_gate_only)),
            no_alert = no_alert_on,
            scope = lb_scope, industry_filter = ind_filter
          )
        }
        if (is.null(lb) || !nrow(lb)) {
          lb <- data.frame(訊息 = "尚無符合目前排行門檻的列（或目前篩選為空）")
        }
      } else {
        lb <- data.frame(訊息 = "尚未評估")
      }
      detail <- lab_im_detail_export_df()
      tks <- if (is.data.frame(detail) && "代號" %in% names(detail)) {
        unique(as.character(detail$代號))
      } else {
        character(0)
      }
      tks <- tks[nzchar(tks)]
      hdr <- c(
        "# Blue Chip 美股績優篩選 — 本頁報告",
        "",
        sprintf("- 匯出時間：%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        sprintf("- 宇宙：美股主要上市 · %d 檔 · 更新於 %s%s",
                n_uni, fetched,
                if (nzchar(src)) paste0(" · 來源 ", src) else ""),
        sprintf("- 未對應產業：%d 檔", n_un),
        sprintf("- 產業：%s", ind_txt),
        sprintf("- 模型：%s", meth_txt),
        sprintf("- 盈餘品質過濾：%s", if (eq_on) "開" else "關"),
        sprintf("- Piotroski 高門檻過濾（F-Score≥7）：%s", if (gate_on) "開" else "關"),
        sprintf("- 無財報警訊過濾：%s", if (no_alert_on) "開" else "關"),
        sprintf("- 候選截斷邏輯 lab_im_pool_rank：%s", rank_mode_label %||% rank_mode),
        sprintf("- 宇宙檔數（N／顯示上限）lab_im_max_n：%s", max_n_label),
        sprintf(
          "- 評估檔數（撈取）lab_resolve_im_eval_n：%s",
          if (is.finite(eval_n)) as.character(eval_n) else "全部"
        ),
        sprintf("- 評估狀態：%s", if (evaluated) sprintf("已評估 %d 檔", nrow(scores)) else "尚未評估"),
        sprintf("- 本頁代號數：%d", length(tks)),
        sprintf("- 本頁代號：%s", if (length(tks)) paste(tks, collapse = ", ") else "（無）"),
        "",
        if (is.finite(max_n)) {
          paste0(
            "宇宙依市場模式（美股 Nasdaq／NYSE 主要上市／台股上市＋上櫃；搜尋另含興櫃但不納入績優）。",
            "先對宇宙池套用「候選截斷邏輯」全市排序／篩選，再取評估檔數 eval_n 的前段（市值／概念股／近一年漲幅／隨機；市值缺值則改依代號排序；近一年漲幅在全市場過大時可能先做 Yahoo 成本預篩）；",
            "N＝分析後顯示上限；明細／排行自合格池取至多 N 檔，條件不足時不湊滿；排行榜＝同一批合格者最多 Top 10",
            if (gate_on) "（目前 Piotroski 高門檻開：F-Score≥7）" else "（目前不設 F-Score 門檻）",
            "；合格不足 10 時不湊滿。"
          )
        } else {
          paste0(
            "宇宙依市場模式（美股 Nasdaq／NYSE 主要上市／台股上市＋上櫃；搜尋另含興櫃但不納入績優）。",
            "本次選「全部」：先套用候選截斷邏輯後評估篩選後全部候選（全市場過大時先預篩）；明細＝全部合格列；排行榜＝同一批合格者最多 Top 10",
            if (gate_on) "（目前 Piotroski 高門檻開：F-Score≥7）" else "（目前不設 F-Score 門檻）",
            "；合格不足 10 時不湊滿。"
          )
        }
      )
      lab_write_lab_page_report(
        dest = file,
        header_lines = hdr,
        summary_df = sm,
        leaderboard_df = lb,
        detail_df = detail
      )
    },
    contentType = if (isTRUE(lab_lab_report_use_zip())) "application/zip" else "text/markdown; charset=utf-8"
  )

  # ------------------------------------------
  # Lab：基本面 K-Means 分群（研究用；非買進訊號）
  # ------------------------------------------
  # Universe size (N) + Candidate truncate live above BLUE CHIP (shared lab_im_* inputs)

  # Plotly/DT htmlwidgets keep the last figure when validate() fails or when
  # renderPlotly returns plotly_empty — destroy outputs via renderUI instead.
  .lab_cluster_idle_msg <- function(kind = c("map", "radar", "table", "focus")) {
    kind <- match.arg(kind)
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "en")
    key <- switch(
      kind,
      map = "lab_cluster_idle_map",
      radar = "lab_cluster_idle_radar",
      table = "lab_cluster_idle_table",
      focus = "lab_cluster_idle_focus"
    )
    ui_str(key, loc)
  }

  .lab_cluster_apply_job_out <- function(out, loc) {
    if (is.null(out) || !is.list(out)) return(invisible(FALSE))
    for (n in as.character(out$notes %||% character(0))) {
      if (nzchar(n)) showNotification(n, type = "message", duration = 6)
    }
    for (w in as.character(out$warnings %||% character(0))) {
      if (nzchar(w)) showNotification(w, type = "warning", duration = 8)
    }
    for (e in as.character(out$errors %||% character(0))) {
      if (nzchar(e)) showNotification(e, type = "error", duration = 12)
    }
    if (!isTRUE(out$ok)) return(invisible(FALSE))
    result <- out$result
    if (is.null(result) || is.null(result$data) || nrow(result$data) == 0L) {
      return(invisible(FALSE))
    }
    lab_cluster_result(result)
    user_focus_raw <- trimws(as.character(isolate(input$lab_cluster_focus) %||% "")[1])
    matched_user <- if (nzchar(user_focus_raw)) {
      lab_cluster_match_ticker(result$data$ticker, user_focus_raw)
    } else {
      NA_character_
    }
    if (!is.na(matched_user) && nzchar(matched_user)) {
      focus_disp <- tryCatch(
        display_ticker_for_market(matched_user, market_mode()),
        error = function(e) as.character(matched_user)[1]
      )
      updateTextInput(session, "lab_cluster_focus", value = focus_disp)
    } else if (nzchar(user_focus_raw)) {
      showNotification(
        tryCatch(
          ui_str("lab_cluster_focus_not_in_cluster", loc),
          error = function(e) {
            "Focus ticker is not in this clustered set. Re-run clustering with the focus entered."
          }
        ),
        type = "warning",
        duration = 8
      )
    } else {
      updateTextInput(session, "lab_cluster_focus", value = "")
    }
    showNotification(
      sprintf("Clustered %d names into %d groups.", result$n, result$k),
      type = "message",
      duration = 6
    )
    invisible(TRUE)
  }

  .lab_cluster_invoke <- function(job_args, notify = TRUE) {
    if (!is.list(job_args)) return(invisible(FALSE))
    loc <- job_args$locale %||% "en"
    if (!isTRUE(ynow_heavy_job_try_acquire("lab_cluster"))) {
      lab_cluster_pending(job_args)
      return(invisible(FALSE))
    }
    if (isTRUE(notify)) {
      prog_msg <- if (identical(normalize_ui_locale(loc), "zh-TW")) {
        "分群中（擷取比率特徵 → K-Means）…"
      } else {
        "Clustering (fetch ratios → K-Means)…"
      }
      showNotification(prog_msg, type = "message", duration = 4)
    }
    lab_cluster_busy(TRUE)
    lab_cluster_pending(NULL)
    .ynow_inflight_gen$lab_cluster <- as.integer(job_args$.gen %||% 0L)[1]
    if (!is.null(lab_cluster_task)) {
      tryCatch(
        lab_cluster_task$invoke(
          catlg = job_args$catlg,
          industry_filter = job_args$industry_filter,
          method_filter = job_args$method_filter,
          max_n = job_args$max_n,
          max_n_label = job_args$max_n_label,
          session_ticker = job_args$session_ticker,
          focus_ticker = job_args$focus_ticker,
          rank_mode = job_args$rank_mode,
          concept_keys = job_args$concept_keys,
          market_mode = job_args$market_mode,
          include_adr = job_args$include_adr,
          k = job_args$k,
          locale = job_args$locale
        ),
        error = function(e) {
          lab_cluster_busy(FALSE)
          tryCatch(ynow_heavy_job_release("lab_cluster"), error = function(e2) NULL)
          showNotification(
            paste("Clustering async start failed:", conditionMessage(e)),
            type = "error", duration = 8
          )
        }
      )
      return(invisible(TRUE))
    }
    on.exit({
      tryCatch(lab_cluster_busy(FALSE), error = function(e) NULL)
      tryCatch(ynow_heavy_job_release("lab_cluster"), error = function(e) NULL)
    }, add = TRUE)
    out <- tryCatch(
      lab_cluster_run_job(
        catlg = job_args$catlg,
        industry_filter = job_args$industry_filter,
        method_filter = job_args$method_filter,
        max_n = job_args$max_n,
        max_n_label = job_args$max_n_label,
        session_ticker = job_args$session_ticker,
        focus_ticker = job_args$focus_ticker,
        rank_mode = job_args$rank_mode,
        concept_keys = job_args$concept_keys,
        market_mode = job_args$market_mode,
        include_adr = job_args$include_adr,
        k = job_args$k,
        locale = job_args$locale
      ),
      error = function(e) {
        showNotification(paste("Clustering failed:", conditionMessage(e)),
                         type = "error", duration = 12)
        NULL
      }
    )
    gen <- as.integer(job_args$.gen %||% 0L)[1]
    if (identical(isolate(as.integer(lab_cluster_gen()) %||% 0L), gen)) {
      .lab_cluster_apply_job_out(out, loc)
    }
    invisible(TRUE)
  }

  .lab_cluster_finish_slot <- function(out = NULL, apply = TRUE, had_error = FALSE) {
    lab_cluster_busy(FALSE)
    tryCatch(ynow_heavy_job_release("lab_cluster"), error = function(e) NULL)
    pending <- isolate(lab_cluster_pending())
    latest <- isolate(as.integer(lab_cluster_gen()) %||% 0L)
    if (is.list(pending) && identical(as.integer(pending$.gen %||% 0L)[1], latest)) {
      lab_cluster_pending(NULL)
      .lab_cluster_invoke(pending, notify = TRUE)
      return(invisible(FALSE))
    }
    if (isTRUE(.ynow_inflight_stale("lab_cluster", latest))) {
      return(invisible(TRUE))
    }
    if (isTRUE(apply) && !isTRUE(had_error)) {
      loc <- tryCatch(ui_locale(), error = function(e) "en")
      .lab_cluster_apply_job_out(out, loc)
    }
    invisible(TRUE)
  }

  observeEvent(input$lab_cluster_run, {
    catlg <- lab_im_catalog()
    req(is.data.frame(catlg), nrow(catlg) > 0)
    loc <- ui_locale()
    k <- suppressWarnings(as.integer(input$lab_cluster_k %||% 3L)[1])
    if (!is.finite(k)) k <- 3L
    k <- max(2L, min(8L, k))
    max_n <- lab_resolve_im_max_n(
      input$lab_im_max_n,
      input$lab_im_max_n_custom,
      lo = 1L,
      hi = 500L
    )
    max_n_label <- lab_resolve_im_max_n_label(
      input$lab_im_max_n,
      input$lab_im_max_n_custom
    )
    session_tk <- tryCatch({
      tk <- current_ticker()
      if (is.null(tk) || !nzchar(trimws(as.character(tk)[1]))) tk <- input$sc
      normalize_ticker_for_market(tk, market_mode())
    }, error = function(e) "")
    if (is.null(session_tk) || is.na(session_tk)) session_tk <- ""
    session_tk <- toupper(trimws(as.character(session_tk)[1]))
    focus_tk <- tryCatch({
      raw <- isolate(input$lab_cluster_focus)
      if (is.null(raw) || !nzchar(trimws(as.character(raw)[1]))) {
        ""
      } else {
        normalize_ticker_for_market(raw, market_mode())
      }
    }, error = function(e) "")
    if (is.null(focus_tk) || is.na(focus_tk)) focus_tk <- ""
    focus_tk <- toupper(trimws(as.character(focus_tk)[1]))
    rank_mode_run <- lab_normalize_pool_rank_mode(
      isolate(input$lab_im_pool_rank %||% "mcap")
    )
    mm <- tryCatch(isolate(market_mode()), error = function(e) "US")
    include_adr <- {
      mm_cl <- tryCatch(normalize_market_mode(mm), error = function(e) "US")
      if (identical(mm_cl, "TW")) TRUE else isTRUE(isolate(input$lab_im_include_adr %||% TRUE))
    }
    gen <- isolate(as.integer(lab_cluster_gen()) %||% 0L) + 1L
    lab_cluster_gen(gen)
    job_args <- list(
      .gen = gen,
      catlg = catlg,
      industry_filter = lab_im_industry_filter(),
      method_filter = input$lab_im_methods,
      max_n = max_n,
      max_n_label = max_n_label,
      session_ticker = session_tk,
      focus_ticker = focus_tk,
      rank_mode = rank_mode_run,
      concept_keys = isolate(input$lab_im_concepts),
      market_mode = mm,
      include_adr = include_adr,
      k = k,
      locale = loc
    )
    running <- isTRUE(isolate(lab_cluster_busy()))
    if (!is.null(lab_cluster_task)) {
      st <- tryCatch(lab_cluster_task$status(), error = function(e) "error")
      if (identical(st, "running")) running <- TRUE
    }
    if (isTRUE(running)) {
      lab_cluster_pending(job_args)
      return()
    }
    .lab_cluster_invoke(job_args, notify = TRUE)
  }, ignoreInit = TRUE)

  if (!is.null(lab_cluster_task)) {
    observeEvent(lab_cluster_task$status(), {
      st <- tryCatch(lab_cluster_task$status(), error = function(e) "initial")
      if (identical(st, "error")) {
        err <- tryCatch(lab_cluster_task$result(), error = function(e) conditionMessage(e))
        if (is.character(err) && nzchar(err[1])) {
          showNotification(paste("Clustering failed:", err[1]), type = "error", duration = 12)
        }
        .lab_cluster_finish_slot(had_error = TRUE, apply = FALSE)
        return()
      }
      if (!identical(st, "success")) return()
      out <- tryCatch(lab_cluster_task$result(), error = function(e) NULL)
      .lab_cluster_finish_slot(out = out, apply = TRUE)
    }, ignoreInit = TRUE)
  }


  # Dynamic hosts: idle → placeholder (no plotly/DT node); live → recreate outputs
  output$lab_cluster_scatter_ui <- renderUI({
    res <- lab_cluster_result()
    mode <- lab_cluster_panel_mode(res)
    if (identical(mode, "idle")) {
      return(lab_cluster_idle_placeholder(.lab_cluster_idle_msg("map"), min_height = "420px"))
    }
    shinycssloaders::withSpinner(
      plotly::plotlyOutput("lab_cluster_scatter", height = "420px")
    )
  })

  output$lab_cluster_radar_ui <- renderUI({
    res <- lab_cluster_result()
    mode <- lab_cluster_panel_mode(res)
    # Do NOT read input$lab_cluster_focus here. Gating / remounting plotlyOutput on
    # every focus edit destroyed the widget and made non-default focus look broken
    # (Search/2330 worked because it was set once at cluster time).
    if (identical(mode, "idle")) {
      return(lab_cluster_idle_placeholder(.lab_cluster_idle_msg("radar"), min_height = "420px"))
    }
    shinycssloaders::withSpinner(
      plotly::plotlyOutput("lab_cluster_radar", height = "420px")
    )
  })

  output$lab_cluster_table_ui <- renderUI({
    res <- lab_cluster_result()
    mode <- lab_cluster_panel_mode(res)
    if (identical(mode, "idle")) {
      return(lab_cluster_idle_placeholder(.lab_cluster_idle_msg("table"), min_height = "120px"))
    }
    tagList(
      uiOutput("lab_cluster_coverage_note"),
      shinycssloaders::withSpinner(DT::dataTableOutput("lab_cluster_table"))
    )
  })

  output$lab_cluster_coverage_note <- renderUI({
    res <- lab_cluster_result()
    if (!lab_cluster_has_result(res)) return(NULL)
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    df <- res$data
    nf <- if ("n_finite" %in% names(df)) {
      suppressWarnings(as.integer(df$n_finite))
    } else {
      rep(NA_integer_, nrow(df))
    }
    n_sparse <- sum(!is.finite(nf) | nf < 2L, na.rm = TRUE)
    session_tk <- tryCatch({
      tk <- current_ticker()
      if (is.null(tk) || !nzchar(trimws(as.character(tk)[1]))) tk <- input$sc
      normalize_ticker_for_market(tk, market_mode())
    }, error = function(e) "")
    session_tk <- toupper(trimws(as.character(session_tk %||% "")[1]))
    fp <- tryCatch(fundamental_profile_rec(), error = function(e) NULL)
    search_fallback <- identical(as.character(fp$profile %||% "")[1], "fallback")
    search_sparse <- FALSE
    if (nzchar(session_tk) && "ticker" %in% names(df)) {
      m <- lab_cluster_match_ticker(df$ticker, session_tk)
      if (!is.na(m) && nzchar(m)) {
        i <- match(m, toupper(trimws(as.character(df$ticker))))
        if (is.finite(i)) {
          search_sparse <- !is.finite(nf[[i]]) || nf[[i]] < 2L
        }
      }
    }
    msgs <- character(0)
    if (isTRUE(search_fallback) || isTRUE(search_sparse)) {
      msgs <- c(msgs, tryCatch(
        ui_str("lab_cluster_note_search_datalimited", loc),
        error = function(e) {
          if (identical(loc, "zh-TW")) {
            "Search 代號為資料受限（財報屬性或 Clustering 比率不足）：請交叉閱讀完整財報，勿只依雷達／分群距離。"
          } else {
            "Search ticker is Data-limited (fundamental profile and/or sparse cluster ratios): cross-read the full statements; do not rely on radar/cluster distance alone."
          }
        }
      ))
    }
    if (n_sparse > 0L) {
      msgs <- c(msgs, tryCatch(
        sprintf(ui_str("lab_cluster_note_impute", loc), as.integer(n_sparse)),
        error = function(e) {
          if (identical(loc, "zh-TW")) {
            sprintf(
              "宇宙中有 %d 檔比率特徵不足（資料受限）；表內／雷達數值可能含中位數補值，請交叉閱讀財報。",
              as.integer(n_sparse)
            )
          } else {
            sprintf(
              "%d names have sparse ratios (Data-limited); table/radar values may include median imputation — cross-read statements.",
              as.integer(n_sparse)
            )
          }
        }
      ))
    }
    if (!length(msgs)) return(NULL)
    tags$div(
      class = "ynow-lab-cluster-coverage-note",
      style = "margin:0 0 10px 0; padding:8px 10px; background:#fff8e8; border:1px solid #f0d78c; border-radius:4px; color:#5a3a10; font-size:12.5px; line-height:1.45;",
      lapply(msgs, function(m) tags$p(style = "margin:0 0 4px 0;", m))
    )
  })

  output$lab_cluster_scatter <- plotly::renderPlotly({
    req(identical(.ynow_sidebar_tab(), "lab_notes"))
    res <- lab_cluster_result()
    req(lab_cluster_has_result(res))
    lab_cluster_scatter_plotly(
      res,
      x_feat = as.character(input$lab_cluster_x %||% "ROE")[1],
      y_feat = as.character(input$lab_cluster_y %||% "PE_Ratio")[1],
      locale = ui_locale(),
      focus_ticker = as.character(input$lab_cluster_focus %||% "")[1]
    )
  })

  output$lab_cluster_radar <- plotly::renderPlotly({
    req(identical(.ynow_sidebar_tab(), "lab_notes"))
    res <- lab_cluster_result()
    req(lab_cluster_has_result(res))
    focus <- as.character(input$lab_cluster_focus %||% "")[1]
    loc <- tryCatch(ui_locale(), error = function(e) "en")
    empty_msg <- function(msg) {
      plotly::plotly_empty() %>%
        plotly::layout(annotations = list(list(
          text = msg, showarrow = FALSE, font = list(size = 14)
        )))
    }
    if (!nzchar(focus)) {
      return(empty_msg(tryCatch(
        ui_str("lab_cluster_idle_focus", loc),
        error = function(e) "Pick a focus ticker for the radar."
      )))
    }
    matched <- lab_cluster_match_ticker(res$data$ticker, focus)
    if (is.na(matched) || !nzchar(matched)) {
      return(empty_msg(tryCatch(
        ui_str("lab_cluster_focus_not_in_cluster", loc),
        error = function(e) {
          "Focus ticker is not in this clustered set. Pick a name from the assignments table."
        }
      )))
    }
    lab_cluster_radar_plotly(res, focus_ticker = focus, locale = loc)
  })

  output$lab_cluster_table <- DT::renderDT({
    req(identical(.ynow_sidebar_tab(), "lab_notes"))
    res <- lab_cluster_result()
    req(lab_cluster_has_result(res))
    loc <- tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    df <- res$data
    session_tk <- tryCatch({
      tk <- current_ticker()
      if (is.null(tk) || !nzchar(trimws(as.character(tk)[1]))) tk <- input$sc
      normalize_ticker_for_market(tk, market_mode())
    }, error = function(e) "")
    session_tk <- toupper(trimws(as.character(session_tk %||% "")[1]))
    fp <- tryCatch(fundamental_profile_rec(), error = function(e) NULL)
    search_fallback <- identical(as.character(fp$profile %||% "")[1], "fallback")
    nf <- if ("n_finite" %in% names(df)) df$n_finite else rep(NA_integer_, nrow(df))
    cov_lab <- lab_cluster_coverage_labels(
      nf,
      df$ticker,
      search_ticker = session_tk,
      search_is_fallback = search_fallback,
      locale = loc
    )
    cov_col <- if (identical(loc, "zh-TW")) "資料覆蓋" else "Coverage"
    df[[cov_col]] <- cov_lab
    cols <- intersect(
      c(
        "ticker", "name", cov_col, "Cluster_ID", "Cluster_Label", "industry_key",
        "ROE", "Operating_Margin", "Rev_YoY", "OpInc_YoY",
        "Debt_Ratio", "PE_Ratio", "PB_Ratio", "market_cap", "ret_1y", "n_finite"
      ),
      names(df)
    )
    # Keep n_finite out of display; Coverage is enough
    cols <- setdiff(cols, "n_finite")
    out <- lab_cluster_format_assignments_df(df[, cols, drop = FALSE])
    cov_idx <- match(cov_col, names(out))
    if (is.finite(cov_idx) && exists("lab_cluster_coverage_flow_html", mode = "function")) {
      out[[cov_col]] <- lab_cluster_coverage_flow_html(out[[cov_col]])
    }
    num_cols <- names(out)[vapply(out, is.numeric, logical(1)) & names(out) != "Cluster_ID"]
    # Default row order already: Radar focus first, then truncate-logic sort
    dt <- ynow_sortable_datatable(
      out,
      escape = if (is.finite(cov_idx)) -as.integer(cov_idx) else TRUE,
      page_length = 25L
    )
    if (length(num_cols)) {
      dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
    }
    dt
  }, server = TRUE)

  # 沿用主頁 Ticker / Stock Code：優先用已搜尋的代碼，否則用主頁輸入框
  lab_ticker <- reactive({
    tk <- current_ticker()
    if (is.null(tk) || !nzchar(trimws(as.character(tk)))) {
      tk <- input$sc
    }
    normalize_ticker_for_market(tk, market_mode())
  })

  output$lab_sec_ticker_display <- renderUI({
    tk <- lab_ticker()
    if (!nzchar(tk)) {
      return(tags$div(
        class = "alert alert-warning", style = "padding:6px 10px; margin:4px 0;",
        "尚未設定主頁代號，請至主頁輸入 Ticker / Stock Code。"
      ))
    }
    tags$div(
      style = "font-size:20px; font-weight:bold; padding:2px 0;",
      display_ticker_for_market(tk, market_mode())
    )
  })

  observeEvent(input$lab_sec_fetch, {
    tk <- lab_ticker()
    form <- input$lab_sec_form %||% "10-K"
    if (!nzchar(tk)) {
      showNotification(.ui_msg("notif_need_ticker"), type = "warning")
      return()
    }
    res <- withProgress(
      message = paste0("正在向 SEC 擷取 ", tk, " ", form, " 財報附註..."),
      value = 0.3, {
        out <- cached_fetch_sec_report_notes(tk, form)
        incProgress(0.6)
        out
      }
    )
    lab_sec_result(res)
    if (!isTRUE(res$ok)) {
      showNotification(
        paste0("擷取失敗：", res$error %||% "未知錯誤"),
        type = "error", duration = 10
      )
    }
  })

  # Filter note indices by important-only + keyword (title / summary / full text).
  lab_sec_filtered_idx <- reactive({
    res <- lab_sec_result()
    if (is.null(res) || !isTRUE(res$ok) || length(res$short_names) == 0) {
      return(integer(0))
    }
    n <- length(res$short_names)
    idx <- seq_len(n)
    imp <- as.logical(res$important)
    if (length(imp) != n) imp <- rep(FALSE, n)
    if (isTRUE(input$lab_sec_important_only)) {
      idx <- idx[imp]
    }
    kw <- tolower(trimws(as.character(input$lab_sec_keyword %||% "")[1]))
    if (!nzchar(kw) || length(idx) == 0) return(idx)

    summaries <- res$summaries
    keep <- vapply(idx, function(i) {
      title <- tolower(as.character(res$short_names[i] %||% ""))
      body <- tolower(as.character(res$full_texts[i] %||% ""))
      excerpt <- tolower(as.character(res$excerpts[i] %||% ""))
      bullets <- tryCatch(
        tolower(paste(as.character(unlist(summaries[[i]])), collapse = " ")),
        error = function(e) ""
      )
      hay <- paste(title, excerpt, body, bullets, sep = "\n")
      grepl(kw, hay, fixed = TRUE)
    }, logical(1))
    idx[keep]
  })

  output$lab_sec_meta <- renderUI({
    res <- lab_sec_result()
    if (is.null(res)) {
      return(div(style = "color:#888;", "尚未查詢。按「擷取財報附註」以取得主頁代號的最新財報附註。"))
    }
    if (!isTRUE(res$ok)) {
      return(div(
        class = "alert alert-danger",
        tags$b("擷取失敗："), res$error %||% "未知錯誤"
      ))
    }
    n_total <- length(res$short_names)
    n_imp <- sum(as.logical(res$important))
    n_shown <- length(lab_sec_filtered_idx())
    kw <- trimws(as.character(input$lab_sec_keyword %||% "")[1])
    shown_label <- if (nzchar(kw) || isTRUE(input$lab_sec_important_only)) {
      paste0(n_total, "（重要 ", n_imp, "；目前顯示 ", n_shown, "）")
    } else {
      paste0(n_total, "（重要 ", n_imp, "）")
    }
    box(
      width = 12, status = "success", solidHeader = TRUE, title = "財報資訊",
      tags$table(
        class = "table table-condensed",
        tags$tr(tags$td(tags$b("公司")), tags$td(res$company)),
        tags$tr(tags$td(tags$b("財報類型")), tags$td(res$form)),
        tags$tr(tags$td(tags$b("申報日")), tags$td(res$filing_date)),
        tags$tr(tags$td(tags$b("財報期間")), tags$td(res$report_date)),
        tags$tr(tags$td(tags$b("Accession")), tags$td(res$accession)),
        tags$tr(tags$td(tags$b("附註數")), tags$td(shown_label))
      ),
      tags$a(href = res$primary_doc_url, target = "_blank",
             icon("external-link-alt"), " 於 SEC EDGAR 開啟原始財報")
    )
  })

  output$lab_sec_index <- renderUI({
    res <- lab_sec_result()
    if (is.null(res) || !isTRUE(res$ok) || length(res$short_names) == 0) {
      return(div(style = "color:#888;", "（無資料）"))
    }
    idx <- lab_sec_filtered_idx()
    if (length(idx) == 0) {
      return(div(style = "color:#888;", "沒有符合關鍵字／篩選條件的附註。"))
    }
    imp <- as.logical(res$important)
    tags$ol(
      lapply(idx, function(i) {
        badge <- if (isTRUE(imp[i])) {
          tags$span(class = "label label-danger", style = "margin-left:6px;", "重要")
        } else NULL
        tags$li(
          tags$a(href = res$urls[i], target = "_blank", res$short_names[i]),
          badge,
          tags$span(style = "color:#999; margin-left:6px;",
                    paste0("(", res$char_counts[i], " 字元)"))
        )
      })
    )
  })

  output$lab_sec_notes <- renderUI({
    res <- lab_sec_result()
    if (is.null(res) || !isTRUE(res$ok) || length(res$short_names) == 0) {
      return(div(style = "color:#888;", "（無資料）"))
    }
    imp <- as.logical(res$important)
    idx <- lab_sec_filtered_idx()
    if (length(idx) == 0) {
      kw <- trimws(as.character(input$lab_sec_keyword %||% "")[1])
      if (nzchar(kw)) {
        return(div(style = "color:#888;", "沒有符合關鍵字的附註；試試其他字詞或清空搜尋。"))
      }
      return(div(style = "color:#888;", "此財報未偵測到「重要」附註；取消勾選可顯示全部。"))
    }
    summaries <- res$summaries
    kw <- tolower(trimws(as.character(input$lab_sec_keyword %||% "")[1]))
    tagList(
      if (nzchar(kw)) {
        tags$div(
          style = "margin-bottom:10px; color:#555;",
          tags$span(class = "label label-info", style = "margin-right:6px;", "關鍵字"),
          tags$code(kw),
          tags$span(style = "margin-left:8px; color:#888;",
                    paste0("顯示 ", length(idx), " 則"))
        )
      } else NULL,
      lapply(idx, function(i) {
        title <- res$short_names[i]
        if (isTRUE(imp[i])) title <- paste0("⭐ ", title)
        bullets <- tryCatch(as.character(unlist(summaries[[i]])), error = function(e) character(0))
        bullets <- bullets[nzchar(trimws(bullets))]
        summary_ui <- if (length(bullets) > 0) {
          tags$div(
            style = "margin:6px 0 8px 0; background:#f5f5f5; border-left:4px solid #222222; padding:8px 10px; border-radius:3px;",
            tags$b("重點摘要"),
            tags$ul(
              style = "margin:6px 0 0 0; padding-left:20px;",
              lapply(bullets, function(b) tags$li(style = "margin-bottom:4px;", b))
            )
          )
        } else if (isTRUE(imp[i])) {
          tags$div(style = "color:#999; margin:6px 0;", "（此附註未擷取到可摘要的重點句）")
        } else NULL
        tags$div(
          style = "margin-bottom:12px; border:1px solid #e3e3e3; border-radius:4px; padding:10px;",
          tags$div(
            style = "font-weight:bold; font-size:15px;",
            title,
            tags$span(style = "color:#999; font-weight:normal; margin-left:6px;",
                      paste0("(", res$char_counts[i], " 字元)"))
          ),
          summary_ui,
          tags$details(
            style = "margin-top:4px;",
            # Default collapsed; user expands per note.
            tags$summary(style = "cursor:pointer; color:#222222;", "展開附註全文"),
            div(
              style = "margin-top:8px; max-height:340px; overflow-y:auto; white-space:pre-wrap; font-size:13px; line-height:1.5;",
              res$full_texts[i]
            )
          )
        )
      })
    )
  })

  # ==========================================
  # 💬 意見區：表單 → GitHub Issues
  # ==========================================
  observeEvent(input$sidebar_feedback_click, {
    showNotification(.ui_msg("notif_feedback_opened"), type = "message", duration = 3)
  }, ignoreInit = TRUE)

  output$feedback_status <- renderUI({
    tok <- .ynow_feedback_github_token()
    repo <- .ynow_feedback_github_repo()
    if (!nzchar(tok)) {
      return(tags$div(
        class = "alert alert-warning", style = "margin-top:12px;",
        tags$b("尚未設定 token："),
        "請設定環境變數 ", tags$code("YNOW_FEEDBACK_GITHUB_TOKEN"),
        " 後重新部署／重啟 app，才能送出 Issue。"
      ))
    }
    tags$div(
      style = "margin-top:12px; color:#666; font-size:12px;",
      "目標 repo：", tags$code(repo),
      " · token 已偵測"
    )
  })

  observeEvent(input$feedback_submit, {
    cat <- as.character(input$feedback_category %||% "other")[1]
    title_raw <- trimws(as.character(input$feedback_title %||% "")[1])
    body_raw <- trimws(as.character(input$feedback_body %||% "")[1])
    contact <- trimws(as.character(input$feedback_contact %||% "")[1])
    if (!nzchar(title_raw)) {
      showNotification(.ui_msg("notif_feedback_need_title"), type = "warning")
      return()
    }
    if (!nzchar(body_raw) || nchar(body_raw) < 8) {
      showNotification(.ui_msg("notif_feedback_need_body"), type = "warning")
      return()
    }
    cat_label <- switch(
      cat,
      enhancement = "優化建議",
      bug = "問題回報",
      ux = "使用體驗",
      "其他"
    )
    type_label <- switch(
      cat,
      enhancement = "feedback-enhancement",
      bug = "feedback-bug",
      ux = "feedback-ux",
      "feedback-other"
    )
    issue_title <- paste0("[Feedback/", cat_label, "] ", title_raw)
    if (nchar(issue_title) > 240) issue_title <- paste0(substr(issue_title, 1, 237), "...")

    ctx_lines <- c(
      "## 使用者回饋",
      "",
      paste0("- **類別：** ", cat_label, " (`", cat, "`)"),
      paste0("- **App：** The YNow App v21"),
      paste0("- **送出時間 (UTC)：** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"))
    )
    if (isTRUE(input$feedback_include_context)) {
      tk <- tryCatch(current_ticker(), error = function(e) NULL)
      if (is.null(tk) || !nzchar(trimws(as.character(tk)))) {
        tk <- input$sc
      }
      ind <- tryCatch(as.character(input$industry_choice %||% "")[1], error = function(e) "")
      ctx_lines <- c(
        ctx_lines,
        paste0("- **Ticker：** ", ifelse(nzchar(trimws(as.character(tk %||% ""))), as.character(tk), "(未設定)")),
        paste0("- **產業鍵：** ", ifelse(nzchar(ind), ind, "(未設定)"))
      )
    }
    if (nzchar(contact)) {
      ctx_lines <- c(ctx_lines, paste0("- **聯絡方式（選填）：** ", contact))
    }
    issue_body <- paste(
      c(
        ctx_lines,
        "",
        "## 內容",
        "",
        body_raw,
        "",
        "---",
        "_Submitted via The YNow App 「意見區」 form._"
      ),
      collapse = "\n"
    )

    res <- withProgress(message = "正在建立 GitHub Issue…", value = 0.4, {
      .ynow_create_feedback_issue(
        title = issue_title,
        body = issue_body,
        labels = c("feedback", type_label)
      )
    })

    if (isTRUE(res$ok)) {
      link <- res$html_url
      num <- res$number
      showNotification(
        paste0("已建立 Issue", if (is.finite(num)) paste0(" #", num) else "", "。"),
        type = "message", duration = 6
      )
      output$feedback_status <- renderUI({
        tags$div(
          class = "alert alert-success", style = "margin-top:12px;",
          tags$b("送出成功。"),
          if (nzchar(as.character(link %||% ""))) {
            tagList(" 請至 ", tags$a(href = link, target = "_blank", rel = "noopener noreferrer", link), " 追蹤。")
          }
        )
      })
      updateTextInput(session, "feedback_title", value = "")
      updateTextAreaInput(session, "feedback_body", value = "")
    } else {
      showNotification(.ui_msg("notif_feedback_fail", err = res$message %||% "unknown"), type = "error", duration = 10)
      output$feedback_status <- renderUI({
        tags$div(
          class = "alert alert-danger", style = "margin-top:12px;",
          tags$b("送出失敗："), as.character(res$message %||% "unknown")
        )
      })
    }
  })
}
