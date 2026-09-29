# ==========================================
# param_audit.R — 使用者參數更動稽核（相對 Search 後基準）
# 1A：基準＝Search／財報載入後自動帶入值
# 2B：結構化視覺報告（非整頁螢幕截圖）
# ==========================================

#' Main pages available for annotated screenshot PDF (2A)
ynow_param_audit_pdf_pages <- function() {
  data.frame(
    tab = c(
      "get_started", "dcf_calculator", "ddm_calculator",
      "ri_calculator", "pb_calculator", "nav_calculator"
    ),
    locale_key = c(
      "param_audit_pdf_page_basic", "param_audit_pdf_page_dcf", "param_audit_pdf_page_ddm",
      "param_audit_pdf_page_ri", "param_audit_pdf_page_pb", "param_audit_pdf_page_nav"
    ),
    stringsAsFactors = FALSE
  )
}

#' Tracked inputs for Snapshot / param-audit / restore CSV (single source of truth).
#' Keep columns equal-length. `full_only=TRUE` rows are hidden from Lite Snapshot.
#' Lite (`full_only=FALSE`) = inputs Lite can set: session currency, industry,
#' Blue Chip ranking filters, and Clustering controls.
ynow_tracked_param_registry <- function() {
  rows <- list(
    # ---- Meta / Basic ----
    c("session_ccy_pick", "Meta", "Session Currency", "get_started", "radio", "FALSE", "USD / TWD display conversion"),
    c("industry_choice", "Basic Setup", "Industry", "get_started", "select", "FALSE", "industry_standards key"),
    c("years", "DCF", "Forecast years n", "dcf_calculator", "numeric", "TRUE", "Explicit forecast horizon"),
    c("dcf_mode", "DCF", "DCF mode", "dcf_calculator", "radio", "TRUE", "gordon / two_stage"),
    c("dcf_claim", "DCF", "Cash-flow claim", "dcf_calculator", "radio", "TRUE", "fcff = WACC+EV; fcfe = Ke"),
    c("g_growth_method", "DCF", "Revenue growth method", "dcf_calculator", "select", "TRUE", "Near-term FCFF growth method"),
    c("custom_g", "DCF", "Custom near-term g (%)", "dcf_calculator", "numeric", "TRUE", "Used when growth method = custom"),
    c("perpetual_g_method", "SGR", "Terminal g method", "get_started", "select", "TRUE", "macro / fundamental / lifecycle"),
    c("lifecycle_stage", "SGR", "Lifecycle stage", "get_started", "select", "TRUE", "Lifecycle when method = lifecycle"),
    c("sgr", "SGR", "SGR / terminal g (%)", "get_started", "numeric", "TRUE", "DCF/RI terminal g; must be < WACC"),
    c("wacc_gordon", "DCF", "Gordon WACC (%)", "dcf_calculator", "numeric", "TRUE", "Gordon / single-stage discount"),
    c("yr_stage1", "DCF", "Stage-1 years", "dcf_calculator", "numeric", "TRUE", "Two-stage high-growth years"),
    c("g_stage1", "DCF", "Stage-1 g1 (%)", "dcf_calculator", "numeric", "TRUE", "Two-stage high-growth rate"),
    c("wacc_stage1", "DCF", "Stage-1 WACC (%)", "dcf_calculator", "numeric", "TRUE", "Stage-1 discount"),
    c("wacc_stage2", "DCF", "Stage-2 WACC (%)", "dcf_calculator", "numeric", "TRUE", "Terminal discount"),
    # ---- CAPM / Beta / WACC ----
    c("capm_rf", "CAPM", "Rf (%)", "get_started", "numeric", "TRUE", "Risk-free rate"),
    c("capm_beta", "CAPM", "Beta", "get_started", "numeric", "TRUE", "Systematic risk"),
    c("capm_rm", "CAPM", "Rm (%)", "get_started", "numeric", "TRUE", "Expected market return"),
    c("sync_gs_beta", "CAPM", "Sync Basic Setup β", "get_started", "checkbox", "TRUE", "TRUE = WACC β follows Basic Setup source"),
    c("beta_purpose", "Beta", "Beta purpose", "get_started", "radio", "TRUE", "valuation; Rolling blocked from CAPM"),
    c("beta_bl_source", "Beta", "Unlever β_L source", "get_started", "radio", "TRUE", "feeds Hamada unlevered β"),
    c("beta_u_apply_source", "Beta", "β apply source", "get_started", "radio", "TRUE", "summary / industry / bottomup / unlever_firm / manual"),
    c("beta_u_manual", "Beta", "Manual β", "get_started", "numeric", "TRUE", "When apply source = manual"),
    c("beta_bottomup_agg", "Beta", "Bottom-up agg", "get_started", "radio", "TRUE", "mean / median"),
    c("beta_peers", "Beta", "Bottom-up peers", "get_started", "select_multi", "TRUE", "Peer tickers for industry unlevered β"),
    c("beta_bench", "Beta", "Rolling Benchmark", "get_started", "select", "TRUE", "Cross-check only; not written to CAPM"),
    c("beta_lookback_months", "Beta", "Rolling lookback (mo)", "get_started", "numeric", "TRUE", "Cross-check window; default 60 ≈ Yahoo 5Y"),
    c("beta_min_obs", "Beta", "Rolling min obs", "get_started", "numeric", "TRUE", "Minimum months for Rolling β"),
    c("wacc_re", "WACC", "Re (%)", "get_started", "numeric", "TRUE", "Cost of equity"),
    c("use_estimated_re", "WACC", "Use CAPM Re", "get_started", "checkbox", "TRUE", "TRUE uses CAPM-estimated Re"),
    c("wacc_rd", "WACC", "rᵈ (%)", "get_started", "numeric", "TRUE", "Cost of debt"),
    c("use_estimated_rd", "WACC", "Use estimated rᵈ", "get_started", "checkbox", "TRUE", "TRUE uses Interest/Debt rᵈ"),
    c("wacc_rd_min", "WACC", "rᵈ min (%)", "get_started", "numeric", "TRUE", "Clamp floor for estimated rᵈ"),
    c("wacc_rd_max", "WACC", "rᵈ max (%)", "get_started", "numeric", "TRUE", "Clamp ceiling for estimated rᵈ"),
    c("wacc_tax", "WACC", "Tax T (%)", "get_started", "numeric", "TRUE", "After-tax debt cost = rᵈ×(1-T)"),
    c("rd_interest_expense", "WACC", "Interest expense", "get_started", "numeric", "TRUE", "Numerator for pre-tax rᵈ"),
    c("rd_interest_bearing_debt", "WACC", "Interest-bearing debt", "get_started", "numeric", "TRUE", "Denominator for pre-tax rᵈ"),
    # ---- FCF projection / CapEx spike ----
    c("mod_fcf-apply_g_ceiling", "FCF", "Apply g ceiling", "dcf_calculator", "checkbox", "TRUE", "25% near-term growth ceiling"),
    c("mod_fcf-fcf_revenue", "FCF", "Revenue", "dcf_calculator", "numeric", "TRUE", "Current-period revenue seed"),
    c("mod_fcf-fcf_nopat", "FCF", "NOPAT", "dcf_calculator", "numeric", "TRUE", "After-tax operating profit"),
    c("mod_fcf-fcf_depreciation", "FCF", "D&A", "dcf_calculator", "numeric", "TRUE", "Depreciation & amortization"),
    c("mod_fcf-fcf_delta_nwc", "FCF", "ΔNWC", "dcf_calculator", "numeric", "TRUE", "Change in net working capital"),
    c("mod_fcf-fcf_capex", "FCF", "CapEx", "dcf_calculator", "numeric", "TRUE", "Capital expenditure (absolute)"),
    c("mod_fcf-fcf_invested_capital", "FCF", "Invested capital", "dcf_calculator", "numeric", "TRUE", "Total invested capital"),
    c("mod_fcf-proj_capex_rate", "FCF", "CapEx / Revenue (%)", "dcf_calculator", "numeric", "TRUE", "Forward CapEx ratio assumption"),
    c("mod_fcf-proj_nwc_rate", "FCF", "ΔNWC / Revenue (%)", "dcf_calculator", "numeric", "TRUE", "Forward ΔNWC ratio assumption"),
    c("mod_fcf-apply_capex_spike_smooth", "FCF", "CapEx spike smooth", "dcf_calculator", "checkbox", "TRUE", "Spike → N-year average"),
    c("mod_fcf-capex_spike_mult", "FCF", "CapEx spike mult", "dcf_calculator", "numeric", "TRUE", "Latest CapEx/Rev > mult × prior avg"),
    c("mod_fcf-capex_spike_avg_years", "FCF", "CapEx spike avg years", "dcf_calculator", "numeric", "TRUE", "Years in spike-average window"),
    # ---- DDM ----
    c("mod_ddm-d0", "DDM", "D0", "ddm_calculator", "numeric", "TRUE", "Last paid DPS"),
    c("mod_ddm-g", "DDM", "Dividend g (%)", "ddm_calculator", "numeric", "TRUE", "Dividend growth; optional SGR sync"),
    c("mod_ddm-sync_g", "DDM", "Sync g with SGR", "ddm_calculator", "checkbox", "TRUE", "If TRUE, DDM g follows central SGR"),
    c("mod_ddm-ddm_mode", "DDM", "DDM mode", "ddm_calculator", "radio", "TRUE", "gordon / spm / two_stage"),
    c("mod_ddm-g_stage1", "DDM", "DDM stage-1 g1 (%)", "ddm_calculator", "numeric", "TRUE", "Two-stage high-growth dividend g"),
    c("mod_ddm-yr_stage1", "DDM", "DDM stage-1 years", "ddm_calculator", "numeric", "TRUE", "Two-stage high-growth years"),
    c("mod_ddm-ke", "DDM", "Ke (%)", "ddm_calculator", "numeric", "TRUE", "Equity required return (CAPM)"),
    c("mod_ddm-est_eps", "DDM", "Estimated EPS", "ddm_calculator", "numeric", "TRUE", "SPM / D0 helper EPS"),
    c("mod_ddm-est_payout", "DDM", "Target payout (%)", "ddm_calculator", "numeric", "TRUE", "Optional payout for D0 rebuild"),
    c("mod_ddm-cycle_years", "DDM", "History avg years", "ddm_calculator", "numeric", "TRUE", "Years for historical payout / DPS avg"),
    # ---- RI ----
    c("mod_ri-b0", "RI", "Book equity B0", "ri_calculator", "numeric", "TRUE", "Starting book equity"),
    c("mod_ri-ri_years", "RI", "RI years", "ri_calculator", "numeric", "TRUE", "Explicit RI forecast horizon"),
    c("mod_ri-ri_ke", "RI", "RI Ke (%)", "ri_calculator", "numeric", "TRUE", "Equity required return"),
    c("mod_ri-ri_g", "RI", "RI g (%)", "ri_calculator", "numeric", "TRUE", "RI terminal growth"),
    c("mod_ri-ri_roe", "RI", "Starting ROE (%)", "ri_calculator", "numeric", "TRUE", "Residual income driver"),
    c("mod_ri-ri_payout", "RI", "Payout (%)", "ri_calculator", "numeric", "TRUE", "Affects book-value compounding"),
    c("mod_ri-roe_method", "RI", "ROE method", "ri_calculator", "select", "TRUE", "constant / linear / industry / custom"),
    c("mod_ri-roe_terminal", "RI", "Terminal ROE (%)", "ri_calculator", "numeric", "TRUE", "Linear fade end ROE"),
    c("mod_ri-roe_industry", "RI", "Industry ROE (%)", "ri_calculator", "numeric", "TRUE", "Industry ROE target"),
    c("mod_ri-roe_custom_txt", "RI", "Custom ROE path", "ri_calculator", "text", "TRUE", "Comma-separated annual ROE (%)"),
    # ---- P/B / NAV ----
    c("mod_pb-bvps", "P/B", "BVPS", "pb_calculator", "numeric", "TRUE", "Book value per share"),
    c("mod_pb-tbvps", "P/B", "TBVPS", "pb_calculator", "numeric", "TRUE", "Tangible book value per share"),
    c("mod_pb-navps", "P/B", "NAVPS (P/B)", "pb_calculator", "numeric", "TRUE", "NAV per share for P/B basis"),
    c("mod_pb-basis", "P/B", "P/B basis", "pb_calculator", "select", "TRUE", "bvps / tbvps / navps"),
    c("mod_pb-holdco_discount", "P/B", "P/B holdco discount (%)", "pb_calculator", "numeric", "TRUE", "Applied to identified investments"),
    c("mod_pb-use_industry_pb", "P/B", "Use industry P/B", "pb_calculator", "checkbox", "TRUE", "TRUE = follow industry band"),
    c("mod_pb-pb_low", "P/B", "P/B low", "pb_calculator", "numeric", "TRUE", "Bear multiple"),
    c("mod_pb-pb_mid", "P/B", "P/B mid", "pb_calculator", "numeric", "TRUE", "Base multiple"),
    c("mod_pb-pb_high", "P/B", "P/B high", "pb_calculator", "numeric", "TRUE", "Bull multiple"),
    c("mod_pb-target_mode", "P/B", "Target mode", "pb_calculator", "select", "TRUE", "multiples | justified"),
    c("mod_nav-navps", "NAV", "NAVPS", "nav_calculator", "numeric", "TRUE", "Book holdco NAV per share"),
    c("mod_nav-holdco_discount", "NAV", "NAV holdco discount (%)", "nav_calculator", "numeric", "TRUE", "Applied to identified investments"),
    c("mod_nav-nav_low", "NAV", "NAV low", "nav_calculator", "numeric", "TRUE", "Bear NAV multiple"),
    c("mod_nav-nav_mid", "NAV", "NAV mid", "nav_calculator", "numeric", "TRUE", "Base NAV multiple"),
    c("mod_nav-nav_high", "NAV", "NAV high", "nav_calculator", "numeric", "TRUE", "Bull NAV multiple"),
    # ---- Decision Checklist ----
    c("chk_bear_base", "Decision Checklist", "Gate: Bear vs Base", "decision_checklist", "checkbox", "TRUE", "Include Bear/Base MOS gate"),
    c("cond_bear_base_bear_mos_floor", "Decision Checklist", "Bear MOS floor (%)", "decision_checklist", "numeric", "TRUE", "Min MOS at Bear FV"),
    c("chk_base_mos", "Decision Checklist", "Gate: Base MOS", "decision_checklist", "checkbox", "TRUE", "Include Base MOS gate"),
    c("cond_base_mos_base_mos_floor", "Decision Checklist", "Base MOS floor (%)", "decision_checklist", "numeric", "TRUE", "Min MOS at Base FV"),
    c("chk_g_sgr", "Decision Checklist", "Gate: g vs SGR", "decision_checklist", "checkbox", "TRUE", "Include growth vs SGR gate"),
    c("cond_g_sgr_g_sgr_gap_min", "Decision Checklist", "g−SGR gap min (pp)", "decision_checklist", "numeric", "TRUE", "Min g vs SGR gap"),
    c("cond_g_sgr_sgr_wacc_buffer", "Decision Checklist", "SGR–WACC buffer (pp)", "decision_checklist", "numeric", "TRUE", "SGR must stay below WACC by buffer"),
    c("chk_model_align", "Decision Checklist", "Gate: Model align", "decision_checklist", "checkbox", "TRUE", "Primary model alignment gate"),
    c("dc_user_primary", "Decision Checklist", "Adopted primary model", "decision_checklist", "select", "TRUE", "User-adopted primary model"),
    c("chk_hfv_veto", "Decision Checklist", "Gate: HFV veto", "decision_checklist", "checkbox", "TRUE", "HFV is veto-only, never a buy signal"),
    c("cond_hfv_veto_max_c_freq", "Decision Checklist", "HFV max C-freq (%)", "decision_checklist", "numeric", "TRUE", "Max historical C-frequency before veto"),
    c("chk_fscore", "Decision Checklist", "Gate: F-Score", "decision_checklist", "checkbox", "TRUE", "Include F-Score gate"),
    c("cond_fscore_fscore_min", "Decision Checklist", "F-Score min", "decision_checklist", "numeric", "TRUE", "Minimum F-Score"),
    c("chk_no_rank_chase", "Decision Checklist", "Gate: No rank chase", "decision_checklist", "checkbox", "TRUE", "Avoid chasing leaderboard ranks"),
    # ---- Backtest / HFV ----
    c("bt_net_margin", "Backtest", "Net margin threshold (%)", "hfv", "numeric", "TRUE", "Holding filter: Net Margin >="),
    c("bt_rev_growth", "Backtest", "Revenue growth threshold (%)", "hfv", "numeric", "TRUE", "Holding filter: Revenue Growth >="),
    c("bt_eps_growth", "Backtest", "EPS / NI growth threshold (%)", "hfv", "numeric", "TRUE", "Holding filter: EPS/NI Growth >="),
    c("bt_fcf_cv", "Backtest", "FCF CV ceiling (%)", "hfv", "numeric", "TRUE", "Holding filter: FCF CV <="),
    c("bt_max_exp", "Backtest", "Max exposure", "hfv", "slider", "TRUE", "Mode A ceiling; 1.0 can fit Buy&Hold"),
    c("bt_min_exp_pass", "Backtest", "Min exp after pass", "hfv", "slider", "TRUE", "Floor when filter passes & MOS >= -10%"),
    c("bt_param_auto", "Backtest", "Auto derive params", "hfv", "checkbox", "TRUE", "Sync thresholds/weights/model on ticker load"),
    c("bt_fv_models", "Backtest", "Chart overlay models", "hfv", "checkboxGroup", "TRUE", "Multi-select HFV chart FV overlay"),
    c("bt_fv_replay_model", "Backtest", "Replay model", "hfv", "radio", "TRUE", "Single-select replay FV for odds/MOS"),
    c("bt_fv_conv_window", "Backtest", "Sample window", "hfv", "radio", "TRUE", "all / 1y / 3y / 5y / custom"),
    c("bt_fv_conv_custom", "Backtest", "Custom sample dates", "hfv", "daterange", "TRUE", "When sample window = custom"),
    c("bt_fv_oos_mode", "Backtest", "Validation sample scope", "hfv", "radio", "TRUE", "realized / expanding / insample"),
    c("bt_fv_analysis_freq", "Backtest", "Analysis frequency", "hfv", "radio", "TRUE", "monthly / quarterly / yearly"),
    c("bt_hfv_show_bench", "Backtest", "Show benchmark", "hfv", "checkbox", "TRUE", "HFV chart benchmark overlay"),
    c("bt_nav_window", "Backtest", "NAV window", "hfv", "radio", "TRUE", "Strategy NAV sample window"),
    c("bt_nav_custom", "Backtest", "Custom NAV dates", "hfv", "daterange", "TRUE", "When NAV window = custom"),
    c("bt_w_vg", "Backtest", "MOS / VG weight", "hfv", "slider", "TRUE", "Exposure diagnostic blend"),
    c("bt_w_mom", "Backtest", "Momentum weight", "hfv", "slider", "TRUE", "Sentiment overlay relative weight"),
    c("bt_w_rsi", "Backtest", "RSI weight", "hfv", "slider", "TRUE", "Sentiment overlay relative weight"),
    # ---- Blue Chip Lab ----
    c("lab_im_pool_rank", "Lab", "Pool rank logic", "bluechip_lab", "select", "FALSE", "Candidate truncation logic"),
    c("lab_im_concepts", "Lab", "Concept groups", "bluechip_lab", "select_multi", "FALSE", "Concept-stock groups when pool = concept"),
    c("lab_im_max_n", "Lab", "Universe size N", "bluechip_lab", "select", "FALSE", "Post-analysis display cap"),
    c("lab_im_max_n_custom", "Lab", "Custom universe N", "bluechip_lab", "numeric", "FALSE", "When N = custom"),
    c("lab_im_industries", "Lab", "Industries", "bluechip_lab", "picker", "TRUE", "Industry filter (Full)"),
    c("lab_im_methods", "Lab", "Valuation models", "bluechip_lab", "checkboxGroup", "TRUE", "Models included in Lab scoring"),
    c("lab_im_eq_only", "Lab", "Earnings quality only", "bluechip_lab", "checkbox", "FALSE", "Filter to earnings-quality pass"),
    c("lab_im_include_adr", "Lab", "Include ADRs", "bluechip_lab", "checkbox", "FALSE", "Include ADR tickers"),
    c("lab_im_gate_only", "Lab", "Gate only", "bluechip_lab", "checkbox", "TRUE", "Show only gate-pass names"),
    c("lab_im_lb_mode", "Lab", "Leaderboard mode", "bluechip_lab", "radio", "FALSE", "overall / by_industry"),
    c("lab_cluster_k", "Lab", "Cluster k", "bluechip_lab", "numeric", "FALSE", "Number of clusters"),
    c("lab_cluster_x", "Lab", "Cluster scatter X", "bluechip_lab", "select", "FALSE", "Scatter X metric"),
    c("lab_cluster_y", "Lab", "Cluster scatter Y", "bluechip_lab", "select", "FALSE", "Scatter Y metric"),
    c("lab_cluster_focus", "Lab", "Cluster focus ticker", "bluechip_lab", "select", "FALSE", "Highlight ticker in cluster map"),
    c("lab_sec_form", "Lab", "SEC form type", "bluechip_lab", "select", "TRUE", "10-K / 10-Q etc."),
    c("lab_sec_important_only", "Lab", "Important notes only", "bluechip_lab", "checkbox", "TRUE", "Filter SEC notes"),
    c("lab_sec_keyword", "Lab", "SEC keyword", "bluechip_lab", "text", "TRUE", "Keyword search in notes")
  )
  mat <- do.call(rbind, rows)
  data.frame(
    input_id = mat[, 1],
    section = mat[, 2],
    label = mat[, 3],
    tab = mat[, 4],
    input_type = mat[, 5],
    full_only = mat[, 6] == "TRUE",
    note = mat[, 7],
    stringsAsFactors = FALSE
  )
}

#' Build Snapshot live rows from the registry (+ optional derived extras).
#' @param input Shiny input
#' @param lite if TRUE, drop full_only registry rows
#' @param extras optional list of character(4) vectors: Section, Parameter, Value, Formula
ynow_snapshot_registry_rows <- function(input, lite = FALSE, extras = NULL) {
  reg <- ynow_tracked_param_registry()
  if (isTRUE(lite)) reg <- reg[!reg$full_only, , drop = FALSE]
  live <- lapply(seq_len(nrow(reg)), function(i) {
    id <- reg$input_id[[i]]
    raw <- tryCatch(input[[id]], error = function(e) NULL)
    val <- ynow_param_norm_value(raw)
    c(reg$section[[i]], reg$label[[i]], if (is.na(val)) NA_character_ else val, reg$note[[i]])
  })
  if (!is.null(extras) && length(extras)) live <- c(extras, live)
  df <- as.data.frame(do.call(rbind, live), stringsAsFactors = FALSE)
  names(df) <- c("Section", "Parameter", "Current Value", "Formula")
  df
}

#' Normalize a single input value for stable string compare / Snapshot CSV
ynow_param_norm_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(paste(format(as.Date(x)), collapse = ","))
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  if (length(x) > 1) {
    x <- paste(as.character(x), collapse = ",")
  } else {
    x <- x[[1]]
  }
  if (length(x) == 0 || isTRUE(is.na(x))) return(NA_character_)
  if (is.logical(x)) return(tolower(as.character(x)))
  if (is.numeric(x)) {
    if (!is.finite(x)) return(NA_character_)
    if (abs(x - round(x)) < 1e-9) return(as.character(as.integer(round(x))))
    return(format(round(as.numeric(x), 6), scientific = FALSE, trim = TRUE))
  }
  s <- trimws(as.character(x))
  if (!nzchar(s) || identical(s, "NA")) return(NA_character_)
  s
}

#' Capture current tracked params from a Shiny input object
ynow_capture_tracked_params <- function(input) {
  reg <- ynow_tracked_param_registry()
  vals <- lapply(reg$input_id, function(id) {
    ynow_param_norm_value(tryCatch(input[[id]], error = function(e) NULL))
  })
  stats::setNames(vals, reg$input_id)
}

#' Diff current vs baseline; return data.frame of changes only
ynow_param_diff_df <- function(baseline, current, locale = "en") {
  reg <- ynow_tracked_param_registry()
  if (is.null(baseline) || !length(baseline)) {
    return(data.frame(
      section = character(0), label = character(0), input_id = character(0),
      tab = character(0), baseline = character(0), current = character(0),
      stringsAsFactors = FALSE
    ))
  }
  cur <- current %||% list()
  rows <- list()
  for (i in seq_len(nrow(reg))) {
    id <- reg$input_id[[i]]
    b <- ynow_param_norm_value(baseline[[id]])
    cval <- ynow_param_norm_value(cur[[id]])
    # Treat both NA/empty as equal
    b_na <- is.na(b) || !nzchar(b)
    c_na <- is.na(cval) || !nzchar(cval)
    if (isTRUE(b_na) && isTRUE(c_na)) next
    if (identical(b, cval)) next
    rows[[length(rows) + 1L]] <- data.frame(
      section = reg$section[[i]],
      label = reg$label[[i]],
      input_id = id,
      tab = reg$tab[[i]],
      baseline = if (isTRUE(b_na)) "—" else b,
      current = if (isTRUE(c_na)) "—" else cval,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(data.frame(
      section = character(0), label = character(0), input_id = character(0),
      tab = character(0), baseline = character(0), current = character(0),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


# Locale maps for adjustment-report section / label (zh-TW); English uses registry text
.ynow_param_audit_section_zh <- c(
  "Basic Setup" = "基本設定",
  "DCF" = "DCF",
  "SGR" = "SGR",
  "CAPM" = "CAPM",
  "Beta" = "Beta",
  "WACC" = "WACC",
  "FCF" = "FCF",
  "DDM" = "DDM",
  "RI" = "RI",
  "P/B" = "P/B",
  "NAV" = "NAV"
)

.ynow_param_audit_label_zh <- c(
  "Industry" = "產業",
  "Forecast years n" = "預測年數 n",
  "DCF mode" = "DCF 模式",
  "Cash-flow claim" = "採用現金流",
  "Revenue growth method" = "營收成長估計法",
  "Custom near-term g (%)" = "自訂短中期 g (%)",
  "Terminal g method" = "終值 g 方法",
  "Lifecycle stage" = "生命週期階段",
  "SGR / terminal g (%)" = "SGR／終值 g (%)",
  "Gordon WACC (%)" = "Gordon WACC (%)",
  "Stage-1 years" = "高速期年數",
  "Stage-1 g1 (%)" = "高速期 g1 (%)",
  "Stage-1 WACC (%)" = "高速期 WACC (%)",
  "Stage-2 WACC (%)" = "穩定期 WACC (%)",
  "Rf (%)" = "Rf (%)",
  "Beta" = "Beta",
  "Rm (%)" = "Rm (%)",
  "Sync Basic Setup β" = "與基本設定同步 β",
  "Beta purpose" = "Beta 用途",
  "Unlever β_L source" = "Unlever β_L 來源",
  "β apply source" = "β 套用來源",
  "Manual β" = "手動 β",
  "Bottom-up agg" = "Bottom-Up 聚合",
  "Rolling lookback (mo)" = "Rolling 回溯月數",
  "Rolling min obs" = "Rolling 最少觀測",
  "Re (%)" = "Re (%)",
  "Use CAPM Re" = "使用 CAPM Re",
  "rᵈ (%)" = "rᵈ (%)",
  "Use estimated rᵈ" = "使用估算 rᵈ",
  "rᵈ min (%)" = "rᵈ 下限 (%)",
  "rᵈ max (%)" = "rᵈ 上限 (%)",
  "Tax T (%)" = "稅率 T (%)",
  "Interest expense" = "利息費用",
  "Interest-bearing debt" = "有息負債",
  "CapEx spike smooth" = "CapEx 暴衝平滑",
  "CapEx spike mult" = "暴衝倍數閾值",
  "CapEx spike avg years" = "均值年數",
  "D0" = "D0",
  "Dividend g (%)" = "股利成長 g (%)",
  "Sync g with SGR" = "與 SGR 同步 g",
  "DDM mode" = "DDM 模式",
  "DDM stage-1 g1 (%)" = "DDM 高速期 g1 (%)",
  "DDM stage-1 years" = "DDM 高速期年數",
  "Ke (%)" = "Ke (%)",
  "RI years" = "RI 年數",
  "RI Ke (%)" = "RI Ke (%)",
  "RI g (%)" = "RI g (%)",
  "Starting ROE (%)" = "起始 ROE (%)",
  "Payout (%)" = "配息率 (%)",
  "ROE method" = "ROE 方法",
  "BVPS" = "BVPS",
  "TBVPS" = "TBVPS",
  "NAVPS" = "NAVPS",
  "P/B basis" = "P/B 基礎",
  "Holdco discount (%)" = "控股折價 (%)",
  "Use industry P/B" = "使用產業 P/B",
  "P/B low" = "P/B 低",
  "P/B mid" = "P/B 中",
  "P/B high" = "P/B 高",
  "Target mode" = "目標模式",
  "NAV low" = "NAV 低",
  "NAV mid" = "NAV 中",
  "NAV high" = "NAV 高"
)

ynow_param_audit_localize_row <- function(section, label, locale = "en") {
  loc <- as.character(locale %||% "en")[1]
  use_zh <- grepl("^zh", loc, ignore.case = TRUE)
  sec <- as.character(section %||% "")[1]
  lab <- as.character(label %||% "")[1]
  if (isTRUE(use_zh)) {
    if (sec %in% names(.ynow_param_audit_section_zh)) sec <- unname(.ynow_param_audit_section_zh[[sec]])
    if (lab %in% names(.ynow_param_audit_label_zh)) lab <- unname(.ynow_param_audit_label_zh[[lab]])
  }
  list(section = sec, label = lab)
}

#' Build structured visual report UI (cards by section)
ynow_param_audit_report_ui <- function(diff_df, eval_summary = NULL,
                                       baseline_at = NULL, locale = "en",
                                       empty_message = NULL) {
  loc <- as.character(locale %||% "en")[1]
  use_zh <- grepl("^zh", loc, ignore.case = TRUE)
  if (is.null(diff_df) || !is.data.frame(diff_df) || nrow(diff_df) == 0) {
    msg <- empty_message %||% ui_str("param_audit_empty_no_changes", loc)
    return(htmltools::tags$p(
      style = "color:#666; font-size:13px; margin:8px 0;",
      msg
    ))
  }

  ts_txt <- if (!is.null(baseline_at)) {
    tryCatch(format(baseline_at, "%Y-%m-%d %H:%M:%S"), error = function(e) as.character(baseline_at))
  } else {
    "—"
  }
  hdr <- htmltools::tags$p(
    style = "font-size:12.5px; color:#555; margin:0 0 10px 0;",
    sprintf(ui_str("param_audit_baseline_hdr", loc), ts_txt, nrow(diff_df))
  )

  eval_box <- NULL
  if (!is.null(eval_summary) && nzchar(as.character(eval_summary %||% "")[1])) {
    eval_box <- htmltools::tags$div(
      class = "ynow-param-audit-eval",
      style = "margin:0 0 12px 0; padding:10px 12px; background:#f7f9fc; border-left:4px solid #3c8dbc; border-radius:4px; font-size:13px; line-height:1.45;",
      htmltools::tags$b(ui_str("param_audit_eval_title", loc)),
      htmltools::tags$div(style = "margin-top:4px; color:#333;", htmltools::HTML(as.character(eval_summary)[1]))
    )
  }

  sections <- unique(as.character(diff_df$section))
  cards <- lapply(sections, function(sec) {
    sub <- diff_df[diff_df$section == sec, , drop = FALSE]
    sec_disp <- ynow_param_audit_localize_row(sec, "", loc)$section
    rows <- lapply(seq_len(nrow(sub)), function(i) {
      loc_row <- ynow_param_audit_localize_row(sub$section[[i]], sub$label[[i]], loc)
      htmltools::tags$div(
        class = "ynow-param-audit-row",
        style = "display:flex; flex-wrap:wrap; gap:8px 14px; align-items:baseline; padding:8px 0; border-bottom:1px solid #eee;",
        htmltools::tags$div(
          style = "flex:1 1 160px; font-weight:600; color:#222;",
          loc_row$label
        ),
        htmltools::tags$div(
          style = "flex:1 1 220px; font-size:12.5px; color:#555;",
          htmltools::tags$span(style = "color:#888;", ui_str("param_audit_baseline_lbl", loc)),
          htmltools::tags$code(sub$baseline[[i]]),
          htmltools::tags$span(style = "margin:0 6px; color:#aaa;", "→"),
          htmltools::tags$span(style = "color:#888;", ui_str("param_audit_now_lbl", loc)),
          htmltools::tags$code(style = "color:#b85c00; font-weight:700;", sub$current[[i]])
        ),
        htmltools::tags$button(
          type = "button",
          class = "btn btn-xs btn-default ynow-param-goto",
          `data-tab` = sub$tab[[i]],
          `data-input-id` = sub$input_id[[i]],
          style = "flex:0 0 auto;",
          ui_str("param_audit_goto_btn", loc)
        )
      )
    })
    htmltools::tags$div(
      class = "ynow-param-audit-card",
      style = "margin:0 0 14px 0; padding:12px 14px; background:#fff; border:1px solid #e5e5e5; border-radius:6px; border-top:3px solid #222;",
      htmltools::tags$div(
        style = "font-size:14px; font-weight:700; margin:0 0 6px 0;",
        sec_disp
      ),
      rows
    )
  })

  htmltools::tagList(hdr, eval_box, cards)
}

# ---------------------------------------------------------------------------
# Parameter restore pack (Snapshot upload / download)
# ---------------------------------------------------------------------------

.ynow_param_restore_legacy_id_map <- function() {
  c(
    "apply_capex_spike_smooth" = "mod_fcf-apply_capex_spike_smooth",
    "capex_spike_mult" = "mod_fcf-capex_spike_mult",
    "capex_spike_avg_years" = "mod_fcf-capex_spike_avg_years"
  )
}

#' Build a machine-readable restore dataframe from current Shiny inputs
#' @param input Shiny input
#' @param ticker optional ticker string for meta
#' @param market_mode optional US/TW
#' @return data.frame with InputId, Section, Label, Value, Type
ynow_param_restore_export_df <- function(input, ticker = NULL, market_mode = NULL) {
  reg <- ynow_tracked_param_registry()
  vals <- vapply(seq_len(nrow(reg)), function(i) {
    id <- reg$input_id[[i]]
    ynow_param_norm_value(tryCatch(input[[id]], error = function(e) NULL))
  }, character(1))
  df <- data.frame(
    InputId = reg$input_id,
    Section = reg$section,
    Label = reg$label,
    Value = vals,
    Type = reg$input_type,
    stringsAsFactors = FALSE
  )
  meta <- data.frame(
    InputId = c("_meta.format", "_meta.exported_at", "_meta.ticker", "_meta.market_mode"),
    Section = rep("Meta", 4L),
    Label = c("Format", "Exported At", "Ticker", "Market"),
    Value = c(
      "ynow_param_restore_v1",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      as.character(ticker %||% "")[1],
      as.character(market_mode %||% "")[1]
    ),
    Type = rep("meta", 4L),
    stringsAsFactors = FALSE
  )
  rbind(meta, df)
}

.ynow_param_restore_pick_col <- function(nms, candidates) {
  low <- tolower(gsub("[^a-z0-9]", "", nms))
  cand_low <- tolower(gsub("[^a-z0-9]", "", candidates))
  for (cnd in cand_low) {
    hit <- which(low == cnd)
    if (length(hit)) return(nms[[hit[[1]]]])
  }
  NA_character_
}

#' Parse an uploaded restore / snapshot CSV into InputId + Value rows
#' @param path file path
#' @return list(ok, error, rows=data.frame(input_id,value,type), meta=list)
ynow_param_restore_parse_file <- function(path) {
  if (is.null(path) || !nzchar(as.character(path)[1]) || !file.exists(path)) {
    return(list(ok = FALSE, error = "missing_file", rows = NULL, meta = list()))
  }
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) {
      tryCatch(
        utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e2) NULL
      )
    }
  )
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) {
    return(list(ok = FALSE, error = "unreadable", rows = NULL, meta = list()))
  }
  # Strip UTF-8 BOM from first column name if present
  names(raw) <- sub("^\ufeff", "", names(raw))
  nms <- names(raw)
  id_col <- .ynow_param_restore_pick_col(nms, c("InputId", "input_id", "Input_Id", "id"))
  val_col <- .ynow_param_restore_pick_col(
    nms, c("Value", "Current Value", "CurrentValue", "current", "baseline")
  )
  type_col <- .ynow_param_restore_pick_col(nms, c("Type", "input_type"))
  label_col <- .ynow_param_restore_pick_col(nms, c("Label", "Parameter", "parameter"))

  reg <- ynow_tracked_param_registry()
  legacy <- .ynow_param_restore_legacy_id_map()
  meta <- list()
  out_ids <- character(0)
  out_vals <- character(0)
  out_types <- character(0)

  if (!is.na(id_col) && !is.na(val_col)) {
    for (i in seq_len(nrow(raw))) {
      id <- trimws(as.character(raw[[id_col]][[i]] %||% ""))
      val <- ynow_param_norm_value(raw[[val_col]][[i]])
      if (!nzchar(id)) next
      if (startsWith(id, "_meta.")) {
        key <- sub("^_meta\\.", "", id)
        meta[[key]] <- if (is.na(val)) "" else val
        next
      }
      if (id %in% names(legacy)) id <- unname(legacy[[id]])
      if (!(id %in% reg$input_id)) next
      if (is.na(val) || !nzchar(val)) next
      typ <- if (!is.na(type_col)) {
        trimws(as.character(raw[[type_col]][[i]] %||% ""))
      } else {
        reg$input_type[match(id, reg$input_id)]
      }
      if (!nzchar(typ) || is.na(typ)) {
        typ <- reg$input_type[match(id, reg$input_id)]
      }
      out_ids <- c(out_ids, id)
      out_vals <- c(out_vals, val)
      out_types <- c(out_types, typ)
    }
  } else if (!is.na(label_col) && !is.na(val_col)) {
    # Human Snapshot CSV fallback: match Parameter / Label text
    for (i in seq_len(nrow(raw))) {
      lab <- trimws(as.character(raw[[label_col]][[i]] %||% ""))
      val <- ynow_param_norm_value(raw[[val_col]][[i]])
      if (!nzchar(lab) || is.na(val) || !nzchar(val)) next
      hit <- which(tolower(reg$label) == tolower(lab))
      if (!length(hit)) next
      id <- reg$input_id[[hit[[1]]]]
      out_ids <- c(out_ids, id)
      out_vals <- c(out_vals, val)
      out_types <- c(out_types, reg$input_type[[hit[[1]]]])
    }
    # Ticker from Meta section if present
    sec_col <- .ynow_param_restore_pick_col(nms, c("Section", "section"))
    if (!is.na(sec_col)) {
      for (i in seq_len(nrow(raw))) {
        sec <- trimws(as.character(raw[[sec_col]][[i]] %||% ""))
        lab <- trimws(as.character(raw[[label_col]][[i]] %||% ""))
        val <- ynow_param_norm_value(raw[[val_col]][[i]])
        if (identical(tolower(sec), "meta") && grepl("^ticker$", lab, ignore.case = TRUE)) {
          meta$ticker <- if (is.na(val)) "" else val
        }
      }
    }
  } else {
    return(list(ok = FALSE, error = "bad_columns", rows = NULL, meta = list()))
  }

  if (!length(out_ids)) {
    return(list(ok = FALSE, error = "no_params", rows = NULL, meta = meta))
  }
  # Deduplicate: last wins
  keep <- !duplicated(out_ids, fromLast = TRUE)
  rows <- data.frame(
    input_id = out_ids[keep],
    value = out_vals[keep],
    type = out_types[keep],
    stringsAsFactors = FALSE
  )
  list(ok = TRUE, error = NULL, rows = rows, meta = meta)
}

.ynow_param_restore_parse_logical <- function(x) {
  s <- tolower(trimws(as.character(x %||% "")[1]))
  if (s %in% c("true", "t", "1", "yes", "y", "on")) return(TRUE)
  if (s %in% c("false", "f", "0", "no", "n", "off")) return(FALSE)
  NA
}

#' Apply parsed restore rows to a Shiny session
#' @return list(applied=integer, skipped=integer, ticker=character|NULL)
ynow_param_restore_apply <- function(session, rows) {
  if (is.null(rows) || !is.data.frame(rows) || !nrow(rows)) {
    return(list(applied = 0L, skipped = 0L))
  }
  reg <- ynow_tracked_param_registry()
  type_map <- stats::setNames(reg$input_type, reg$input_id)

  .split_multi <- function(raw_val) {
    parts <- unlist(strsplit(as.character(raw_val %||% ""), "[,|;]"), use.names = FALSE)
    parts <- trimws(as.character(parts))
    parts[nzchar(parts) & !identical(parts, "NA")]
  }

  # Apply non-checkbox first so auto-sync toggles do not immediately overwrite values
  typ0 <- tolower(as.character(rows$type %||% ""))
  is_cb <- typ0 %in% c("checkbox", "checkboxgroup")
  ord <- c(which(!is_cb), which(is_cb))
  applied <- 0L
  skipped <- 0L

  for (i in ord) {
    id <- as.character(rows$input_id[[i]])[1]
    raw_val <- rows$value[[i]]
    typ <- tolower(as.character(rows$type[[i]] %||% type_map[[id]] %||% "numeric")[1])
    if (!nzchar(id) || !(id %in% reg$input_id)) {
      skipped <- skipped + 1L
      next
    }
    ok <- tryCatch({
      if (identical(typ, "checkbox")) {
        lv <- .ynow_param_restore_parse_logical(raw_val)
        if (is.na(lv)) return(FALSE)
        shiny::updateCheckboxInput(session, id, value = lv)
      } else if (identical(typ, "checkboxgroup")) {
        shiny::updateCheckboxGroupInput(session, id, selected = .split_multi(raw_val))
      } else if (identical(typ, "radio")) {
        sel <- as.character(raw_val)[1]
        tryCatch(
          shiny::updateRadioButtons(session, id, selected = sel),
          error = function(e) {
            if (requireNamespace("shinyWidgets", quietly = TRUE)) {
              shinyWidgets::updateRadioGroupButtons(session, id, selected = sel)
            } else {
              stop(e)
            }
          }
        )
      } else if (identical(typ, "select")) {
        shiny::updateSelectInput(session, id, selected = as.character(raw_val)[1])
      } else if (identical(typ, "select_multi")) {
        shiny::updateSelectizeInput(session, id, selected = .split_multi(raw_val))
      } else if (identical(typ, "picker")) {
        if (requireNamespace("shinyWidgets", quietly = TRUE)) {
          shinyWidgets::updatePickerInput(session, id, selected = .split_multi(raw_val))
        } else {
          shiny::updateSelectInput(session, id, selected = .split_multi(raw_val))
        }
      } else if (identical(typ, "slider")) {
        num <- suppressWarnings(as.numeric(raw_val)[1])
        if (!is.finite(num)) return(FALSE)
        shiny::updateSliderInput(session, id, value = num)
      } else if (identical(typ, "daterange")) {
        parts <- .split_multi(raw_val)
        if (length(parts) < 2L) return(FALSE)
        d1 <- suppressWarnings(as.Date(parts[[1]]))
        d2 <- suppressWarnings(as.Date(parts[[2]]))
        if (is.na(d1) || is.na(d2)) return(FALSE)
        shiny::updateDateRangeInput(session, id, start = d1, end = d2)
      } else if (identical(typ, "text")) {
        shiny::updateTextInput(session, id, value = as.character(raw_val)[1])
      } else {
        num <- suppressWarnings(as.numeric(raw_val)[1])
        if (!is.finite(num)) return(FALSE)
        shiny::updateNumericInput(session, id, value = num)
      }
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) applied <- applied + 1L else skipped <- skipped + 1L
  }
  list(applied = applied, skipped = skipped)
}
