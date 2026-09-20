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

#' Tracked valuation / setup inputs for the adjustment report + restore CSV
ynow_tracked_param_registry <- function() {
  data.frame(
    input_id = c(
      "industry_choice", "years", "dcf_mode", "dcf_claim",
      "g_growth_method", "custom_g",
      "perpetual_g_method", "lifecycle_stage", "sgr",
      "wacc_gordon", "yr_stage1", "g_stage1", "wacc_stage1", "wacc_stage2",
      "capm_rf", "capm_beta", "capm_rm", "sync_gs_beta",
      "beta_purpose", "beta_bl_source", "beta_u_apply_source", "beta_u_manual",
      "beta_bottomup_agg", "beta_lookback_months", "beta_min_obs",
      "wacc_re", "use_estimated_re", "wacc_rd", "use_estimated_rd",
      "wacc_rd_min", "wacc_rd_max", "wacc_tax",
      "rd_interest_expense", "rd_interest_bearing_debt",
      "mod_fcf-apply_capex_spike_smooth", "mod_fcf-capex_spike_mult", "mod_fcf-capex_spike_avg_years",
      "mod_ddm-d0", "mod_ddm-g", "mod_ddm-sync_g", "mod_ddm-ddm_mode",
      "mod_ddm-g_stage1", "mod_ddm-yr_stage1", "mod_ddm-ke",
      "mod_ri-ri_years", "mod_ri-ri_ke", "mod_ri-ri_g", "mod_ri-ri_roe",
      "mod_ri-ri_payout", "mod_ri-roe_method",
      "mod_pb-bvps", "mod_pb-tbvps", "mod_pb-navps", "mod_pb-basis",
      "mod_pb-holdco_discount", "mod_pb-use_industry_pb",
      "mod_pb-pb_low", "mod_pb-pb_mid", "mod_pb-pb_high", "mod_pb-target_mode",
      "mod_nav-navps", "mod_nav-holdco_discount",
      "mod_nav-nav_low", "mod_nav-nav_mid", "mod_nav-nav_high"
    ),
    section = c(
      "Basic Setup", "DCF", "DCF", "DCF",
      "DCF", "DCF",
      "SGR", "SGR", "SGR",
      "DCF", "DCF", "DCF", "DCF", "DCF",
      "CAPM", "CAPM", "CAPM", "CAPM",
      "Beta", "Beta", "Beta", "Beta",
      "Beta", "Beta", "Beta",
      "WACC", "WACC", "WACC", "WACC",
      "WACC", "WACC", "WACC",
      "WACC", "WACC",
      "FCF", "FCF", "FCF",
      "DDM", "DDM", "DDM", "DDM",
      "DDM", "DDM", "DDM",
      "RI", "RI", "RI", "RI",
      "RI", "RI",
      "P/B", "P/B", "P/B", "P/B",
      "P/B", "P/B",
      "P/B", "P/B", "P/B", "P/B",
      "NAV", "NAV",
      "NAV", "NAV", "NAV"
    ),
    label = c(
      "Industry", "Forecast years n", "DCF mode", "Cash-flow claim",
      "Revenue growth method", "Custom near-term g (%)",
      "Terminal g method", "Lifecycle stage", "SGR / terminal g (%)",
      "Gordon WACC (%)", "Stage-1 years", "Stage-1 g1 (%)", "Stage-1 WACC (%)", "Stage-2 WACC (%)",
      "Rf (%)", "Beta", "Rm (%)", "Sync Basic Setup β",
      "Beta purpose", "Unlever β_L source", "β apply source", "Manual β",
      "Bottom-up agg", "Rolling lookback (mo)", "Rolling min obs",
      "Re (%)", "Use CAPM Re", "rᵈ (%)", "Use estimated rᵈ",
      "rᵈ min (%)", "rᵈ max (%)", "Tax T (%)",
      "Interest expense", "Interest-bearing debt",
      "CapEx spike smooth", "CapEx spike mult", "CapEx spike avg years",
      "D0", "Dividend g (%)", "Sync g with SGR", "DDM mode",
      "DDM stage-1 g1 (%)", "DDM stage-1 years", "Ke (%)",
      "RI years", "RI Ke (%)", "RI g (%)", "Starting ROE (%)",
      "Payout (%)", "ROE method",
      "BVPS", "TBVPS", "NAVPS", "P/B basis",
      "Holdco discount (%)", "Use industry P/B",
      "P/B low", "P/B mid", "P/B high", "Target mode",
      "NAVPS", "Holdco discount (%)",
      "NAV low", "NAV mid", "NAV high"
    ),
    tab = c(
      "get_started", "dcf_calculator", "dcf_calculator", "dcf_calculator",
      "dcf_calculator", "dcf_calculator",
      "get_started", "get_started", "get_started",
      "dcf_calculator", "dcf_calculator", "dcf_calculator", "dcf_calculator", "dcf_calculator",
      "get_started", "get_started", "get_started", "get_started",
      "get_started", "get_started", "get_started", "get_started",
      "get_started", "get_started", "get_started",
      "get_started", "get_started", "get_started", "get_started",
      "get_started", "get_started", "get_started",
      "get_started", "get_started",
      "dcf_calculator", "dcf_calculator", "dcf_calculator",
      "ddm_calculator", "ddm_calculator", "ddm_calculator", "ddm_calculator",
      "ddm_calculator", "ddm_calculator", "ddm_calculator",
      "ri_calculator", "ri_calculator", "ri_calculator", "ri_calculator",
      "ri_calculator", "ri_calculator",
      "pb_calculator", "pb_calculator", "pb_calculator", "pb_calculator",
      "pb_calculator", "pb_calculator",
      "pb_calculator", "pb_calculator", "pb_calculator", "pb_calculator",
      "nav_calculator", "nav_calculator",
      "nav_calculator", "nav_calculator", "nav_calculator"
    ),
    input_type = c(
      "select", "numeric", "radio", "radio",
      "select", "numeric",
      "select", "select", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "checkbox",
      "radio", "radio", "radio", "numeric",
      "radio", "numeric", "numeric",
      "numeric", "checkbox", "numeric", "checkbox",
      "numeric", "numeric", "numeric",
      "numeric", "numeric",
      "checkbox", "numeric", "numeric",
      "numeric", "numeric", "checkbox", "radio",
      "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric",
      "numeric", "select",
      "numeric", "numeric", "numeric", "select",
      "numeric", "checkbox",
      "numeric", "numeric", "numeric", "select",
      "numeric", "numeric",
      "numeric", "numeric", "numeric"
    ),
    stringsAsFactors = FALSE
  )
}

#' Normalize a single input value for stable string compare
ynow_param_norm_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
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

#' Build structured visual report UI (cards by section)
ynow_param_audit_report_ui <- function(diff_df, eval_summary = NULL,
                                       baseline_at = NULL, locale = "en",
                                       empty_message = NULL) {
  loc <- as.character(locale %||% "en")[1]
  use_zh <- grepl("^zh", loc, ignore.case = TRUE)
  if (is.null(diff_df) || !is.data.frame(diff_df) || nrow(diff_df) == 0) {
    msg <- empty_message %||% if (use_zh) {
      "尚無相對 Search 後基準的手改參數。載入財報後若手動覆寫，變更會列於此。"
    } else {
      "No manual adjustments vs the post-Search baseline yet. Edits after load appear here."
    }
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
    if (use_zh) {
      sprintf("基準時間（Search 後自動帶入）：%s · 手改參數 %d 項", ts_txt, nrow(diff_df))
    } else {
      sprintf("Baseline (post-Search auto-fill): %s · %d adjusted parameter(s)", ts_txt, nrow(diff_df))
    }
  )

  eval_box <- NULL
  if (!is.null(eval_summary) && nzchar(as.character(eval_summary %||% "")[1])) {
    eval_box <- htmltools::tags$div(
      class = "ynow-param-audit-eval",
      style = "margin:0 0 12px 0; padding:10px 12px; background:#f7f9fc; border-left:4px solid #3c8dbc; border-radius:4px; font-size:13px; line-height:1.45;",
      htmltools::tags$b(if (use_zh) "目前評估結果（摘要）" else "Current evaluation summary"),
      htmltools::tags$div(style = "margin-top:4px; color:#333;", htmltools::HTML(as.character(eval_summary)[1]))
    )
  }

  sections <- unique(as.character(diff_df$section))
  cards <- lapply(sections, function(sec) {
    sub <- diff_df[diff_df$section == sec, , drop = FALSE]
    rows <- lapply(seq_len(nrow(sub)), function(i) {
      htmltools::tags$div(
        class = "ynow-param-audit-row",
        style = "display:flex; flex-wrap:wrap; gap:8px 14px; align-items:baseline; padding:8px 0; border-bottom:1px solid #eee;",
        htmltools::tags$div(
          style = "flex:1 1 160px; font-weight:600; color:#222;",
          sub$label[[i]]
        ),
        htmltools::tags$div(
          style = "flex:1 1 220px; font-size:12.5px; color:#555;",
          htmltools::tags$span(style = "color:#888;", if (use_zh) "基準 " else "Baseline "),
          htmltools::tags$code(sub$baseline[[i]]),
          htmltools::tags$span(style = "margin:0 6px; color:#aaa;", "→"),
          htmltools::tags$span(style = "color:#888;", if (use_zh) "現值 " else "Now "),
          htmltools::tags$code(style = "color:#b85c00; font-weight:700;", sub$current[[i]])
        ),
        htmltools::tags$button(
          type = "button",
          class = "btn btn-xs btn-default ynow-param-goto",
          `data-tab` = sub$tab[[i]],
          `data-input-id` = sub$input_id[[i]],
          style = "flex:0 0 auto;",
          if (use_zh) "前往並框選" else "Go & highlight"
        )
      )
    })
    htmltools::tags$div(
      class = "ynow-param-audit-card",
      style = "margin:0 0 14px 0; padding:12px 14px; background:#fff; border:1px solid #e5e5e5; border-radius:6px; border-top:3px solid #222;",
      htmltools::tags$div(
        style = "font-size:14px; font-weight:700; margin:0 0 6px 0;",
        sec
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

  # Apply non-checkbox first so auto-sync toggles do not immediately overwrite values
  is_cb <- tolower(as.character(rows$type)) == "checkbox"
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
      } else if (identical(typ, "radio")) {
        shiny::updateRadioButtons(session, id, selected = as.character(raw_val)[1])
      } else if (identical(typ, "select")) {
        shiny::updateSelectInput(session, id, selected = as.character(raw_val)[1])
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
