# Required model inputs, manual-fill guidance, and reference assumptions.
# Full suggestions are display-only; Lite applies available references with an assumption ledger.
ynow_parameter_input_ids <- function() {
  c("bridge_cash", "bridge_debt", "bridge_shares", "bridge_revenue", "bridge_other_claims", "bridge_fx", "years", "sgr",
    "dcf_mode", "dcf_claim", "wacc_gordon", "wacc_stage1", "wacc_stage2", "wacc_re",
    "wacc_rd", "wacc_tax", "yr_stage1", "g_stage1", "g_stage2",
    paste0("mod_fcf-", c("fcf_nopat", "fcf_depreciation", "fcf_capex", "fcf_delta_nwc", "fcf_invested_capital")),
    paste0("mod_ddm-", c("d0", "g", "ke", "ddm_mode", "est_eps", "g_stage1", "yr_stage1")),
    paste0("mod_ri-", c("b0", "ri_ke", "ri_g", "ri_years", "ri_payout", "ri_roe", "roe_method", "roe_terminal", "roe_industry")),
    paste0("mod_pb-", c("basis", "bvps", "tbvps", "navps", "pb_low", "pb_mid", "pb_high")),
    paste0("mod_nav-", c("navps", "nav_low", "nav_mid", "nav_high")),
    paste0("mod_rel-", c("rel_mode", "trailing_eps", "forward_eps", "pe_multiple", "fwd_pe_multiple", "fcff", "ev_fcf_multiple", "ebit", "ev_ebit_multiple", "ebitda", "ev_ebitda_multiple", "revenue", "ev_sales_multiple", "ps_multiple")),
    paste0("mod_sotp-", c("default_ev_sales", "nonop", "manual_segments")), "mod_ri-roe_custom_txt")
}

ynow_required_parameter_rows <- function(values, defaults = list(), locale = "en", ke_pct = NA_real_, wacc_pct = NA_real_) {
  zh <- identical(locale, "zh-TW")
  rows <- list()
  add <- function(model, id, label, tab, source_en, source_zh, default = NULL, suggestion = NA_real_, present = NULL) {
    if (isTRUE(present)) return()
    value <- suppressWarnings(as.numeric(values[[id]])[1])
    if (is.null(present) && is.finite(value)) return()
    ref <- suppressWarnings(as.numeric(if (!is.null(default)) defaults[[default]] else suggestion)[1])
    source <- if (zh) source_zh else source_en
    if (is.finite(ref)) {
      suggestion_source <- if (!is.null(default)) {
        if (zh) "系統預設的估算假設，非財報值" else "App default assumption, not a reported financial value"
      } else if (zh) "目前已計算的折現率" else "Current calculated discount rate"
      hint <- paste0(ui_str("param_missing_reference", locale), ": ", format(round(ref, 4), trim = TRUE),
                     " (", suggestion_source, "). ", source)
    } else hint <- paste0(ui_str("param_missing_no_reference", locale), " ", source)
    rows[[length(rows) + 1L]] <<- data.frame(model = model, id = id, label = label,
      tab = tab, hint = hint, suggestion = ref, stringsAsFactors = FALSE)
  }
  bridge <- function(model, cash = TRUE, revenue = FALSE) {
    if (cash) add(model, "bridge_cash", "Cash", "dashboard", "Cash and cash equivalents in the analysis currency.", "填入分析幣別的現金及約當現金。")
    add(model, "bridge_other_claims", "Other equity claims", "dashboard", "Enter the market value of preferred and noncontrolling claims. Zero is appropriate only after confirming no such claims. With other capital classes, calculate WACC manually; the two-class automatic WACC is unavailable.", "請填特別股及非控制權益等其他權益請求權的市值；確認不存在時才填零。有其他資本類別時須人工計算 WACC，自動二類資本 WACC 不適用。")
    add(model, "bridge_debt", "Debt", "dashboard", "Interest-bearing debt, not total liabilities.", "填入有息負債，勿填負債總額。")
    add(model, "bridge_shares", "Shares", "dashboard", "Shares in the quoted class; align ordinary shares and ADR units.", "使用報價股權級距的股數，須對齊普通股與 ADR 單位。")
    if (revenue) add(model, "bridge_revenue", "Revenue", "dashboard", "Revenue for the same period as the operating cash-flow inputs.", "營收須與營業現金流參數採相同期間。")
  }
  bridge("dcf", cash = !identical(values$dcf_claim, "fcfe"), revenue = TRUE)
  for (pair in list(c("fcf_nopat", "NOPAT"), c("fcf_depreciation", "D&A"), c("fcf_capex", "CapEx"), c("fcf_delta_nwc", "ΔNWC"))) {
    add("dcf", paste0("mod_fcf-", pair[1]), pair[2], "dcf_calculator",
        "Use the matching fiscal-period operating input. NOPAT is EBIT × (1−T), not Net Income. Do not assume missing data is zero.",
        "請使用同一財報期間的營運資料；NOPAT 為 EBIT × (1−T)，勿填 Net Income，缺值也不代表零。")
  }
  add("fcf_growth", "mod_fcf-fcf_invested_capital", "Operating IC", "dcf_calculator", "Operating assets less non-interest-bearing operating liabilities; exclude cash and nonoperating assets. Do not substitute total assets.", "請以營運資產扣除不付息營運負債，並排除現金及非營運資產；勿直接填總資產。")
  add("dcf", "years", "Forecast years", "dcf_calculator", "Review the forecast horizon.", "請確認預測年數。", "years")
  add("dcf", "sgr", "Terminal g", "dcf_calculator", "Use a sustainable terminal rate below the discount rate.", "請評估可持續的永續成長率，並低於折現率。", "sgr")
  if (identical(values$dcf_claim, "fcfe")) {
    add("dcf", "wacc_re", "Ke", "dcf_calculator", "FCFE uses Ke, not WACC.", "FCFE 應採 Ke，勿以 WACC 代替。", suggestion = ke_pct)
    add("dcf", "wacc_rd", "Rd", "dcf_calculator", "Gross accrual interest expense / interest-bearing debt.", "以利息費用除以有息負債，勿用淨利息或現金支付利息代替。")
    add("dcf", "wacc_tax", "Tax", "dcf_calculator", "Confirm the applicable tax assumption.", "請確認適用稅率假設。", "wacc_tax")
  } else if (identical(values$dcf_mode, "two_stage")) {
    for (id in c("wacc_stage1", "wacc_stage2")) add("dcf", id, id, "dcf_calculator", "Review each stage's WACC.", "請確認各階段 WACC。", suggestion = wacc_pct)
  } else add("dcf", "wacc_gordon", "WACC", "dcf_calculator", "Market-value capital weights must use a common currency.", "股權市值與有息負債須換成相同幣別。", suggestion = wacc_pct)
  if (identical(values$dcf_mode, "two_stage")) for (id in c("yr_stage1", "g_stage1")) add("dcf", id, id, "dcf_calculator", "Review the stage-specific forecast assumption.", "請確認各階段的預測假設。", id)
  for (id in c("d0", "g", "ke")) add("ddm", paste0("mod_ddm-", id), id, "ddm_calculator",
    "D0 is trailing paid annual DPS in quote units; g is sustainable growth; r is Ke.", "D0 應為報價幣別的已支付年度 DPS；g 為永續成長率，r 採 Ke。",
    default = if (id == "g") "ddm_g" else NULL, suggestion = if (id == "ke") ke_pct else NA_real_)
  if (identical(values[["mod_ddm-ddm_mode"]], "spm")) add("ddm", "mod_ddm-est_eps", "EPS", "ddm_calculator", "Use common-share EPS with a weighted-average denominator.", "使用普通股 EPS；分母須為加權平均股數。")
  if (identical(values[["mod_ddm-ddm_mode"]], "two_stage")) for (id in c("g_stage1", "yr_stage1")) add("ddm", paste0("mod_ddm-", id), id, "ddm_calculator", "Review stage-one assumptions.", "請確認第一階段假設。", paste0("ddm_", id))
  for (id in c("b0", "ri_ke", "ri_g", "ri_years", "ri_payout", "ri_roe")) add("ri", paste0("mod_ri-", id), id, "ri_calculator",
    "B0 uses common book equity per quote-class share. Use common earnings and common dividends for ROE and payout.", "B0 為每報價股的普通股帳面權益；ROE 與配息率須使用普通股盈餘及普通股股利。",
    default = if (id %in% c("ri_g", "ri_years", "ri_payout", "ri_roe")) id else NULL,
    suggestion = if (id == "ri_ke") ke_pct else NA_real_)
  method <- values[["mod_ri-roe_method"]]
  if (length(method) && method %in% c("linear", "industry")) {
    id <- if (method == "linear") "roe_terminal" else "roe_industry"
    add("ri", paste0("mod_ri-", id), id, "ri_calculator", "Use the selected ROE path's endpoint.", "請填所選 ROE 路徑的終點假設。")
  }
  if (identical(method, "custom")) add("ri", "mod_ri-roe_custom_txt", "Custom ROE path", "ri_calculator", "Enter the explicit ROE path in percentages.", "請填入明確的 ROE 百分比路徑。", present = nzchar(trimws(values[["mod_ri-roe_custom_txt"]] %||% "")))
  basis <- values[["mod_pb-basis"]] %||% "bvps"
  add("pb", paste0("mod_pb-", basis), basis, "pb_calculator", "Use common BVPS/TBVPS or the selected NAVPS, in quote units.", "依所選基礎使用普通股 BVPS／TBVPS 或 NAVPS，並對齊報價單位。")
  for (id in c("pb_low", "pb_mid", "pb_high")) add("pb", paste0("mod_pb-", id), id, "pb_calculator", "These are valuation multiple assumptions, not reported financial values.", "此為估值倍數假設，並非財報數據。", id)
  add("nav", "mod_nav-navps", "NAVPS", "nav_calculator", "Use common net assets per quote-class share.", "使用每報價股的普通股淨資產。")
  for (id in c("nav_low", "nav_mid", "nav_high")) add("nav", paste0("mod_nav-", id), id, "nav_calculator", "Review the NAV valuation multiple assumption.", "請確認 NAV 估值倍數假設。", id)
  family <- values[["mod_rel-rel_mode"]] %||% "equity"
  finite <- function(id) is.finite(suppressWarnings(as.numeric(values[[id]])[1]))
  if (identical(family, "enterprise")) {
    bridge("multiples")
    candidates <- c("fcff", "ebit", "ebitda")
    valid <- candidates[vapply(paste0("mod_rel-", candidates), finite, logical(1))]
    chosen <- if (length(valid)) valid[1] else "fcff"
    multiple <- switch(chosen, fcff = "ev_fcf_multiple", ebit = "ev_ebit_multiple", ebitda = "ev_ebitda_multiple")
  } else if (identical(family, "revenue")) {
    bridge("multiples", cash = FALSE, revenue = TRUE)
    chosen <- "revenue"; multiple <- "ps_multiple"
  } else {
    chosen <- if (finite("mod_rel-trailing_eps")) "trailing_eps" else if (finite("mod_rel-forward_eps")) "forward_eps" else "trailing_eps"
    multiple <- if (chosen == "forward_eps") "fwd_pe_multiple" else "pe_multiple"
  }
  add("multiples", paste0("mod_rel-", chosen), chosen, "rel_multiples_calculator", "Fill the metric for the chosen multiple; never substitute a different accounting metric.", "請補填欲採用倍數所需的科目，勿以其他會計科目替代。")
  add("multiples", paste0("mod_rel-", multiple), multiple, "rel_multiples_calculator", "This is an explicit valuation multiple assumption.", "此為明確的估值倍數假設。", paste0("rel_", multiple))
  bridge("sotp")
  add("sotp", "mod_sotp-nonop", "Non-operating assets", "sotp_calculator", "Include only separately valued non-operating assets; do not count cash twice. Enter zero only if that is the intended assumption.", "僅填獨立估值的非營運資產，勿重複計入現金；確認假設為零時才填零。")
  if (!is.null(values[[".sotp_ready"]])) add("sotp", "mod_sotp-manual_segments", "Segment data", "sotp_calculator", "Supply at least two distinct positive segment revenues and explicit EV/Sales multiples. Revenue must be in the analysis currency.", "請補填至少兩個不同部門的正值營收及各自 EV/Sales 倍數，營收須採分析幣別。", present = isTRUE(values[[".sotp_ready"]]))
  add("sotp", "mod_sotp-default_ev_sales", "Segment EV/Sales", "sotp_calculator", "Use distinct reported segments and an explicit multiple assumption.", "須有可辨識的部門資料，並明確設定倍數假設。", "rel_ev_sales_multiple")
  if (isTRUE(values[[".fx_needed"]])) for (model in c("dcf", "ddm", "ri", "pb", "nav", "multiples", "sotp")) add(model, "bridge_fx", "USD/TWD FX", "dashboard", "Enter TWD per USD using the valuation-date exchange rate. A manual value overrides the automatic quote; clear it to restore automatic FX.", "請填估值日每一美元兌新台幣的匯率；人工值優先於自動報價，清除此欄可恢復自動匯率。", present = FALSE)
  if (!length(rows)) return(data.frame(model = character(), id = character(), label = character(), tab = character(), hint = character(), suggestion = numeric()))
  do.call(rbind, rows)
}

ynow_parameter_guidance_server <- function(input, session, searched, ticker, busy,
                                           model_rec, locale, defaults, ke, wacc, sotp_ready = function() TRUE, fx_needed = function() FALSE, lite = function() FALSE) {
  applied <- shiny::reactiveVal(list())
  requested <- shiny::reactiveVal("")
  shiny::observeEvent(ticker(), { requested(""); applied(list()) }, ignoreInit = TRUE, priority = 101)
  buttons <- c(calc = "dcf", `mod_ddm-btn_calc_ddm` = "ddm", `mod_ri-btn_calc_ri` = "ri",
               `mod_pb-btn_calc_pb` = "pb", `mod_nav-btn_calc_nav` = "nav",
               `mod_rel-btn_calc_rel` = "multiples", `mod_rel-btn_calc_rel_ev` = "multiples",
               `mod_rel-btn_calc_rel_rev` = "multiples", `mod_sotp-btn_calc_sotp` = "sotp")
  for (id in names(buttons)) local({
    button_id <- id; model <- buttons[[id]]
    shiny::observeEvent(input[[button_id]], requested(model), ignoreInit = TRUE)
  })
  missing <- shiny::debounce(shiny::reactive({
    shiny::req(isTRUE(searched()))
    ids <- ynow_parameter_input_ids()
    values <- setNames(lapply(ids, function(id) input[[id]]), ids)
    values[[".sotp_ready"]] <- sotp_ready()
    values[[".fx_needed"]] <- fx_needed()
    ynow_required_parameter_rows(values, defaults, locale(), ke() * 100, wacc() * 100)
  }), 300)
  shiny::observe({
    if (!isTRUE(searched()) || isTRUE(busy())) return()
    rows <- missing()
    ledger <- applied()
    if (isTRUE(lite())) {
      refs <- rows[is.finite(rows$suggestion) & !duplicated(rows$id), , drop = FALSE]
      for (i in seq_len(nrow(refs))) {
        id <- refs$id[i]
        # Do not repeatedly send an update while the browser echoes it.
        if (!is.null(ledger[[id]])) next
        ledger[[id]] <- list(value = refs$suggestion[i], hint = refs$hint[i], label = refs$label[i])
        session$sendInputMessage(id, list(value = refs$suggestion[i]))
      }
      applied(ledger)
    }
    hints <- rows[!duplicated(rows$id), , drop = FALSE]
    for (id in names(ledger)) {
      value <- suppressWarnings(as.numeric(input[[id]])[1])
      ref <- ledger[[id]]
      if (is.finite(value) && abs(value - ref$value) <= max(1, abs(ref$value)) * 1e-7) {
        hints <- hints[hints$id != id, , drop = FALSE]
        hints <- rbind(hints, data.frame(model = "assumption", id = id, label = ref$label,
          tab = "", hint = paste0(ui_str("param_assumption_applied", locale()), ": ",
          format(ref$value, trim = TRUE), ". ", ref$hint), suggestion = ref$value))
      }
    }
    session$sendCustomMessage("ynowParameterGuidance", list(items = lapply(seq_len(nrow(hints)), function(i) {
      as.list(hints[i, c("id", "hint"), drop = FALSE])
    })))
    chosen <- requested()
    if (!nzchar(chosen)) chosen <- as.character(model_rec()$primary %||% "")[1]
    needed <- rows[rows$model == chosen, , drop = FALSE]
    if (!nrow(needed)) {
      shiny::removeNotification("ynow_missing_parameters", session = session)
      return()
    }
    loc <- locale()
    items <- lapply(seq_len(nrow(needed)), function(i) {
      shiny::tags$li(needed$label[i], " ", shiny::tags$button(
        type = "button", class = "btn btn-link btn-xs ynow-fill-parameter",
        `data-ynow-fill-id` = needed$id[i], `data-ynow-fill-tab` = needed$tab[i],
        ui_str("param_missing_go_fill", loc)))
    })
    shiny::showNotification(shiny::tagList(
      shiny::tags$strong(ui_str("param_missing_title", loc)),
      shiny::tags$p(ui_str("param_missing_body", loc)),
      shiny::tags$ul(items)), id = "ynow_missing_parameters", type = "warning",
      duration = NULL, session = session)
  })
  shiny::reactive({
    ledger <- applied()
    used <- Filter(function(x) {
      value <- suppressWarnings(as.numeric(input[[x$id]])[1])
      is.finite(value) && abs(value - x$value) <= max(1, abs(x$value)) * 1e-7
    }, lapply(names(ledger), function(id) c(list(id = id), ledger[[id]])))
    vapply(used, function(x) paste0(ui_str("param_assumption_applied", locale()), ": ",
      x$label, " = ", format(x$value, trim = TRUE), ". ", x$hint), character(1))
  })
}

parse_manual_sotp_segments <- function(text) {
  empty <- data.frame(name = character(), revenue = numeric(), multiple = numeric())
  if (!is.character(text) || !length(text) || !nzchar(trimws(text[1]))) return(empty)
  d <- tryCatch(utils::read.csv(text = text[1], stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.data.frame(d) || !all(c("name", "revenue", "multiple") %in% names(d))) return(empty)
  d <- d[, c("name", "revenue", "multiple")]
  d$name <- trimws(d$name)
  d$revenue <- suppressWarnings(as.numeric(d$revenue))
  d$multiple <- suppressWarnings(as.numeric(d$multiple))
  if (nrow(d) < 2L || anyDuplicated(d$name) || any(!nzchar(d$name)) ||
      any(!is.finite(d$revenue) | d$revenue <= 0) || any(!is.finite(d$multiple) | d$multiple <= 0)) return(empty)
  d
}

# UI updates travel asynchronously. Automatic valuation may not consume a
# previous ticker's still-visible values while the new source update is in flight.
ynow_input_echo_guard <- function(input, session, ticker, tracked) {
  expected <- shiny::reactiveValues()
  acknowledged <- shiny::reactiveValues()
  ids <- function() if (is.function(tracked)) tracked() else tracked
  update <- function(session, inputId, ...) {
    args <- list(...)
    if ("value" %in% names(args)) {
      expected[[inputId]] <- suppressWarnings(as.numeric(args$value)[1])
      acknowledged[[inputId]] <- FALSE
    }
    shiny::updateNumericInput(session, inputId, ...)
  }
  shiny::observeEvent(ticker(), {
    for (id in ids()) update(session, id, value = NA_real_)
  }, ignoreInit = FALSE, priority = 100)
  shiny::observe({
    for (id in ids()) {
      a <- suppressWarnings(as.numeric(input[[id]])[1])
      b <- expected[[id]]
      matches <- length(b) == 1L && ((is.na(a) && is.na(b)) ||
        (is.finite(a) && is.finite(b) && abs(a - b) <= max(1, abs(b)) * 1e-7))
      if (matches && !isTRUE(shiny::isolate(acknowledged[[id]]))) acknowledged[[id]] <- TRUE
    }
  })
  ready <- shiny::reactive(all(vapply(ids(), function(id) isTRUE(acknowledged[[id]]), logical(1))))
  list(update = update, ready = ready, expected = expected)
}
