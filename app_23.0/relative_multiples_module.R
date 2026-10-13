# ==========================================
# relative_multiples_module.R
# Relative valuation: P/E, Forward P/E, PEG, EV/FCF,
# EV/EBIT, EV/EBITDA, EV/Sales, P/S, EV/ARR, revenue-SOTP.
# Implied Price / relative indicator only — not Intrinsic Value.
# Reuses shares, FX/ADR guards, cash/debt bridge, FCFF, BB Lab segments.
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ---------- Pure calculation helpers (no Shiny) ----------

.parse_summary_num <- function(x) {
  if (is.null(x) || length(x) < 1L) return(NA_real_)
  if (is.numeric(x)) {
    v <- suppressWarnings(as.numeric(x)[1])
    return(if (is.finite(v)) v else NA_real_)
  }
  s <- trimws(as.character(x)[1])
  if (!nzchar(s) || identical(toupper(s), "N/A") || identical(s, "—")) return(NA_real_)
  v <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", s)))
  if (is.finite(v)) v else NA_real_
}

#' Trailing / forward P/E → Implied Price = EPS × multiple.
calc_pe_implied_price <- function(eps, pe_multiple) {
  eps <- suppressWarnings(as.numeric(eps)[1])
  pe <- suppressWarnings(as.numeric(pe_multiple)[1])
  if (!is.finite(eps) || eps <= 0) {
    return(list(
      status = "unavailable", reason = "eps_nonpositive",
      implied_price = NA_real_, eps = eps, pe_multiple = pe
    ))
  }
  if (!is.finite(pe) || pe <= 0) {
    return(list(
      status = "unavailable", reason = "pe_invalid",
      implied_price = NA_real_, eps = eps, pe_multiple = pe
    ))
  }
  list(
    status = "ok", reason = NA_character_,
    implied_price = eps * pe, eps = eps, pe_multiple = pe
  )
}

#' @deprecated Do not use for valuation sync — inverts market price into EPS (回推).
#' Kept only for offline regression of the pure math; Multiples sync must not call this.
calc_forward_eps_from_price_pe <- function(price, forward_pe) {
  px <- suppressWarnings(as.numeric(price)[1])
  fpe <- suppressWarnings(as.numeric(forward_pe)[1])
  if (!is.finite(px) || px <= 0 || !is.finite(fpe) || fpe <= 0) return(NA_real_)
  px / fpe
}

#' App / industry-assumption multiples (never Yahoo Trailing／Forward P/E or target price).
.rel_app_default_multiples <- function(defaults = NULL) {
  d <- defaults
  if (is.null(d) && exists("APP_DEFAULTS", inherits = TRUE)) {
    d <- get("APP_DEFAULTS", inherits = TRUE)
  }
  if (!is.list(d)) d <- list()
  pe <- suppressWarnings(as.numeric(d$rel_pe_multiple %||% 18)[1])
  if (!is.finite(pe) || pe <= 0) pe <- 18
  fpe <- suppressWarnings(as.numeric(d$rel_fwd_pe_multiple %||% pe)[1])
  if (!is.finite(fpe) || fpe <= 0) fpe <- pe
  list(
    pe_multiple = pe,
    fwd_pe_multiple = fpe,
    ev_fcf_multiple = {
      v <- suppressWarnings(as.numeric(d$rel_ev_fcf_multiple %||% 15)[1])
      if (is.finite(v) && v > 0) v else 15
    },
    ev_ebit_multiple = {
      v <- suppressWarnings(as.numeric(d$rel_ev_ebit_multiple %||% 12)[1])
      if (is.finite(v) && v > 0) v else 12
    },
    ev_ebitda_multiple = {
      v <- suppressWarnings(as.numeric(d$rel_ev_ebitda_multiple %||% 10)[1])
      if (is.finite(v) && v > 0) v else 10
    },
    ev_sales_multiple = {
      v <- suppressWarnings(as.numeric(d$rel_ev_sales_multiple %||% 3)[1])
      if (is.finite(v) && v > 0) v else 3
    },
    ps_multiple = {
      v <- suppressWarnings(as.numeric(d$rel_ps_multiple %||% 3)[1])
      if (is.finite(v) && v > 0) v else 3
    },
    ev_arr_multiple = {
      v <- suppressWarnings(as.numeric(d$rel_ev_arr_multiple %||% 10)[1])
      if (is.finite(v) && v > 0) v else 10
    }
  )
}

#' Trailing EPS from latest statements first; Yahoo EPS (TTM) only as reported earnings fallback.
#' Never derives EPS from price ÷ P/E.
.rel_trailing_eps_from_statements <- function(sum_df = NULL, d_is = NULL, d_bs = NULL) {
  # 1) Net Income ÷ shares from IS / BS
  if (!is.null(d_is) && is.data.frame(d_is) && nrow(d_is) > 0 &&
      exists("select_current_metric", mode = "function")) {
    ni <- tryCatch(
      select_current_metric(d_is, "Net Income Common Stockholders|Net Income$", "flow"),
      error = function(e) NA_real_
    )
    sh <- NA_real_
    if (!is.null(d_bs) && is.data.frame(d_bs) && nrow(d_bs) > 0) {
      sh <- tryCatch(
        select_current_metric(
          d_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock"
        ),
        error = function(e) NA_real_
      )
    }
    if (is.finite(ni) && is.finite(sh) && sh > 0) return(as.numeric(ni / sh)[1])
  }
  # 2) Shared report helper (may still read Yahoo EPS TTM as earnings figure)
  if (exists(".report_eps_bvps", mode = "function")) {
    eps <- tryCatch(
      .report_eps_bvps(sum_df, d_is = d_is, d_bs = d_bs)$eps,
      error = function(e) NA_real_
    )
    if (is.finite(eps)) return(as.numeric(eps)[1])
  }
  # 3) Yahoo EPS (TTM) label only — reported earnings, not a valuation multiple
  if (exists("extract_summary_item", mode = "function")) {
    teps <- .parse_summary_num(extract_summary_item(
      sum_df, "EPS \\(TTM\\)|Trailing EPS|^EPS$", default = NA_character_
    ))
    if (is.finite(teps)) return(teps)
  }
  NA_real_
}

#' Forward EPS from trailing × (1 + g/100). No price, no Yahoo Forward P/E, no 回推.
.rel_forward_eps_from_trailing <- function(trailing_eps, growth_pct) {
  teps <- suppressWarnings(as.numeric(trailing_eps)[1])
  g <- suppressWarnings(as.numeric(growth_pct)[1])
  if (!is.finite(teps) || teps <= 0) return(NA_real_)
  if (!is.finite(g)) return(NA_real_)
  teps * (1 + g / 100)
}

#' Normalize legacy radio values: earnings／ps → equity; enterprise unchanged.
.rel_normalize_mode <- function(mode = "equity") {
  m <- tolower(trimws(as.character(mode %||% "equity")[1]))
  if (m %in% c("enterprise", "revenue", "equity")) m else "equity"
}

#' Which of the four relative families a method key belongs to.
.rel_method_family <- function(key) {
  k <- as.character(key %||% "")[1]
  if (k %in% c("ev_ebitda", "ev_ebit", "ev_fcf")) return("enterprise")
  if (k %in% c("pe", "forward_pe", "peg")) return("earnings")
  if (k %in% c("ps", "ev_sales", "ev_arr")) return("revenue")
  if (identical(k, "pb")) return("book")
  NA_character_
}

#' Method keys that can produce Implied Price within a Multiples family.
#' Taxonomy: Equity (P/E · Fwd P/E · P/S) vs Enterprise (EV/*). PEG is indicator-only.
#' SOTP is a separate sidebar engine in the Enterprise-structural claim family.
.rel_method_order <- function(mode = "equity") {
  switch(
    .rel_normalize_mode(mode),
    "enterprise" = c("ev_fcf", "ev_ebit", "ev_ebitda", "ev_sales", "ev_arr"),
    "revenue" = c("ps", "ev_sales", "ev_arr"),
    # equity (default): PEG has no Implied Price
    c("pe", "forward_pe", "ps")
  )
}

#' First successful Implied Price in the selected family (respects rel_mode).
.rel_pick_active_implied <- function(res, mode = "equity") {
  empty <- list(key = NA_character_, implied_price = NA_real_)
  if (is.null(res) || !is.list(res)) return(empty)
  for (k in .rel_method_order(mode)) {
    node <- res[[k]]
    if (!is.null(node) && identical(node$status, "ok") &&
        is.finite(suppressWarnings(as.numeric(node$implied_price)[1]))) {
      return(list(
        key = as.character(k)[1],
        implied_price = suppressWarnings(as.numeric(node$implied_price)[1])
      ))
    }
  }
  empty
}

#' Short finance-term tag for chart / composite pills (en-US formal terms).
.rel_method_chart_tag <- function(key) {
  k <- as.character(key %||% "")[1]
  if (!nzchar(k) || identical(k, "NA") || is.na(key)) return(NA_character_)
  out <- switch(
    k,
    "pe" = "P/E",
    "forward_pe" = "Fwd P/E",
    "ev_fcf" = "EV/FCF",
    "ev_ebit" = "EV/EBIT",
    "ev_ebitda" = "EV/EBITDA",
    "ev_sales" = "EV/Sales",
    "ev_arr" = "EV/ARR",
    "ps" = "P/S",
    NA_character_
  )
  if (is.null(out) || is.na(out) || !nzchar(out)) NA_character_ else out
}

#' Chart / composite label: "Multiples · P/E" when a method is active.
.rel_chart_label <- function(method_key, base = "Multiples") {
  base_lab <- as.character(base %||% "Multiples")[1]
  if (!nzchar(base_lab)) base_lab <- "Multiples"
  tag <- .rel_method_chart_tag(method_key)
  if (is.null(tag) || length(tag) < 1L || is.na(tag) || !nzchar(tag)) return(base_lab)
  paste0(base_lab, " · ", tag)
}

calc_peg <- function(pe, growth_pct,
                     growth_definition = NA_character_,
                     growth_period = NA_character_) {
  pe <- suppressWarnings(as.numeric(pe)[1])
  g <- suppressWarnings(as.numeric(growth_pct)[1])
  if (!is.finite(pe) || pe <= 0) {
    return(list(
      status = "unavailable", reason = "pe_invalid",
      peg = NA_real_, pe = pe, growth_pct = g,
      growth_definition = as.character(growth_definition %||% NA_character_)[1],
      growth_period = as.character(growth_period %||% NA_character_)[1]
    ))
  }
  if (!is.finite(g) || g <= 0) {
    return(list(
      status = "unavailable", reason = "growth_nonpositive",
      peg = NA_real_, pe = pe, growth_pct = g,
      growth_definition = as.character(growth_definition %||% NA_character_)[1],
      growth_period = as.character(growth_period %||% NA_character_)[1]
    ))
  }
  list(
    status = "ok", reason = NA_character_,
    peg = pe / g, pe = pe, growth_pct = g,
    growth_definition = as.character(growth_definition %||% NA_character_)[1],
    growth_period = as.character(growth_period %||% NA_character_)[1]
  )
}

#' Enterprise multiple → Implied EV → Equity (cash−debt) → Implied Price.
#' metric ≤ 0 or missing → unavailable (Missing ≠ 0).
calc_ev_metric_implied_price <- function(metric, multiple, cash, debt, shares,
                                         metric_name = "metric") {
  metric <- suppressWarnings(as.numeric(metric)[1])
  mult <- suppressWarnings(as.numeric(multiple)[1])
  cash <- suppressWarnings(as.numeric(cash)[1])
  debt <- suppressWarnings(as.numeric(debt)[1])
  shares <- suppressWarnings(as.numeric(shares)[1])
  empty <- function(reason, ev = NA_real_, eq = NA_real_) {
    list(
      status = "unavailable", reason = reason, metric_name = metric_name,
      implied_ev = ev, equity_value = eq, implied_price = NA_real_,
      metric = metric, multiple = mult
    )
  }
  if (!is.finite(metric) || metric <= 0) return(empty("metric_nonpositive"))
  if (!is.finite(mult) || mult <= 0) return(empty("multiple_invalid"))
  if (!is.finite(shares) || shares <= 0) {
    return(empty("shares_missing", ev = metric * mult))
  }
  if (!is.finite(cash)) cash <- 0
  if (!is.finite(debt)) debt <- 0
  ev <- metric * mult
  eq <- if (exists("dcf_ev_to_equity", mode = "function")) {
    dcf_ev_to_equity(ev, cash = cash, debt = debt)
  } else {
    ev + cash - debt
  }
  if (!is.finite(eq)) return(empty("equity_invalid", ev = ev, eq = eq))
  list(
    status = "ok", reason = NA_character_, metric_name = metric_name,
    implied_ev = ev, equity_value = eq, implied_price = eq / shares,
    metric = metric, multiple = mult, cash = cash, debt = debt, shares = shares
  )
}

calc_ev_fcf_implied_price <- function(fcf, ev_fcf_multiple, cash, debt, shares) {
  out <- calc_ev_metric_implied_price(
    fcf, ev_fcf_multiple, cash, debt, shares, metric_name = "FCFF"
  )
  if (identical(out$reason, "metric_nonpositive")) out$reason <- "fcf_nonpositive"
  out$fcf <- out$metric
  out
}

#' P/S → Implied Equity = Revenue × P/S; Implied Price = Equity / shares.
#' Equity-side (not EV). Revenue ≤ 0 → unavailable.
calc_ps_implied_price <- function(revenue, ps_multiple, shares) {
  rev <- suppressWarnings(as.numeric(revenue)[1])
  mult <- suppressWarnings(as.numeric(ps_multiple)[1])
  shares <- suppressWarnings(as.numeric(shares)[1])
  if (!is.finite(rev) || rev <= 0) {
    return(list(
      status = "unavailable", reason = "revenue_nonpositive",
      equity_value = NA_real_, implied_price = NA_real_,
      revenue = rev, multiple = mult
    ))
  }
  if (!is.finite(mult) || mult <= 0) {
    return(list(
      status = "unavailable", reason = "multiple_invalid",
      equity_value = NA_real_, implied_price = NA_real_,
      revenue = rev, multiple = mult
    ))
  }
  if (!is.finite(shares) || shares <= 0) {
    return(list(
      status = "unavailable", reason = "shares_missing",
      equity_value = rev * mult, implied_price = NA_real_,
      revenue = rev, multiple = mult
    ))
  }
  eq <- rev * mult
  list(
    status = "ok", reason = NA_character_,
    equity_value = eq, implied_price = eq / shares,
    revenue = rev, multiple = mult, shares = shares
  )
}

#' Revenue-multiple SOTP: Σ(segment revenue × EV/Sales) − Net Debt (+ non-op).
#' Requires ≥2 positive reported segment revenues. Not segment-EBIT SOTP.
#' If `segments$multiple` is present, each row uses its own EV/Sales; otherwise
#' `ev_sales_multiple` is applied to every segment.
calc_sotp_revenue_implied <- function(segments, ev_sales_multiple, cash, debt, shares,
                                      non_operating = 0) {
  mult_default <- suppressWarnings(as.numeric(ev_sales_multiple)[1])
  cash <- suppressWarnings(as.numeric(cash)[1])
  debt <- suppressWarnings(as.numeric(debt)[1])
  shares <- suppressWarnings(as.numeric(shares)[1])
  non_op <- suppressWarnings(as.numeric(non_operating)[1])
  if (!is.finite(non_op)) non_op <- 0
  empty <- function(reason) {
    list(
      status = "unavailable", reason = reason,
      implied_ev = NA_real_, equity_value = NA_real_, implied_price = NA_real_,
      n_segments = 0L, segments = data.frame(
        name = character(0), revenue = numeric(0), multiple = numeric(0),
        value = numeric(0), stringsAsFactors = FALSE
      )
    )
  }
  if (!is.data.frame(segments) || !nrow(segments)) return(empty("no_segments"))
  if (!all(c("name", "revenue") %in% names(segments))) return(empty("no_segments"))
  seg <- segments
  seg$name <- as.character(seg$name)
  seg$revenue <- suppressWarnings(as.numeric(seg$revenue))
  if ("multiple" %in% names(seg)) {
    seg$multiple <- suppressWarnings(as.numeric(seg$multiple))
  } else {
    seg$multiple <- rep(mult_default, nrow(seg))
  }
  seg <- seg[is.finite(seg$revenue) & seg$revenue > 0 & nzchar(seg$name), , drop = FALSE]
  if (nrow(seg) < 2L) return(empty("need_multi_segment"))
  bad_m <- !is.finite(seg$multiple) | seg$multiple <= 0
  if (any(bad_m)) {
    if (!is.finite(mult_default) || mult_default <= 0) return(empty("multiple_invalid"))
    seg$multiple[bad_m] <- mult_default
  }
  if (any(!is.finite(seg$multiple) | seg$multiple <= 0)) return(empty("multiple_invalid"))
  if (!is.finite(shares) || shares <= 0) return(empty("shares_missing"))
  if (!is.finite(cash)) cash <- 0
  if (!is.finite(debt)) debt <- 0
  seg$value <- seg$revenue * seg$multiple
  ev <- sum(seg$value) + non_op
  eq <- if (exists("dcf_ev_to_equity", mode = "function")) {
    dcf_ev_to_equity(ev, cash = cash, debt = debt)
  } else {
    ev + cash - debt
  }
  if (!is.finite(eq)) return(empty("equity_invalid"))
  list(
    status = "ok", reason = NA_character_,
    implied_ev = ev, equity_value = eq, implied_price = eq / shares,
    n_segments = nrow(seg), segments = seg,
    multiple = if (length(unique(round(seg$multiple, 6))) == 1L) seg$multiple[[1]] else NA_real_,
    cash = cash, debt = debt, shares = shares,
    non_operating = non_op
  )
}

.rel_rev_cagr_pct <- function(d_is, max_years = 5L) {
  if (is.null(d_is) || !is.data.frame(d_is) || !nrow(d_is)) return(NA_real_)
  rev <- tryCatch(
    select_clean_metric_row(
      d_is, "Total Revenue|^Revenue$|Operating Revenue", include_ttm = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(rev)) return(NA_real_)
  vals <- suppressWarnings(as.numeric(rev))
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(vals) < 2L) return(NA_real_)
  n_keep <- min(length(vals), max(2L, as.integer(max_years)[1]))
  vals <- vals[seq_len(n_keep)]
  n_yr <- length(vals) - 1L
  if (n_yr < 1L) return(NA_real_)
  newest <- vals[[1]]
  oldest <- vals[[length(vals)]]
  if (!is.finite(newest) || !is.finite(oldest) || oldest <= 0) return(NA_real_)
  ((newest / oldest)^(1 / n_yr) - 1) * 100
}

.rel_latest_fcff <- function(d_cf, d_is = NULL) {
  if (exists("reconstruct_hist_fcff", mode = "function")) {
    rec <- tryCatch(reconstruct_hist_fcff(d_cf, d_is = d_is), error = function(e) NULL)
    if (!is.null(rec) && length(rec$fcff)) {
      v <- suppressWarnings(as.numeric(rec$fcff))
      hit <- which(is.finite(v))[1]
      if (length(hit) && is.finite(v[[hit]])) return(v[[hit]])
    }
  }
  NA_real_
}

.rel_is_metric <- function(d_is, patterns) {
  if (is.null(d_is) || !is.data.frame(d_is)) return(NA_real_)
  tryCatch(select_current_metric(d_is, patterns, "flow"), error = function(e) NA_real_)
}

#' US EDGAR segment notes for SOTP / BB Lab (same cached path as Business Breakdown).
.rel_sotp_fetch_segment_notes <- function(ticker) {
  tk <- as.character(ticker %||% "")[1]
  if (!nzchar(tk) || grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)) return(NULL)
  if (!exists("cached_fetch_sec_segment_notes", mode = "function")) return(NULL)
  tryCatch(cached_fetch_sec_segment_notes(tk, "10-K"), error = function(e) NULL)
}

#' Pull ≥2 positive segment revenues via BB Lab when multi-business split exists.
#' Prefers income-statement rows; when `notes` / `segment_tables` are supplied
#' (e.g. SEC segment notes), those feed the same BB Lab payload path.
.rel_sotp_segments_from_is <- function(d_is, ticker = "", statement_currency = NA,
                                       notes = NULL, segment_tables = NULL) {
  empty <- data.frame(name = character(0), revenue = numeric(0), stringsAsFactors = FALSE)
  if (!exists("bblab_payload_from_statements", mode = "function") ||
      !exists("bblab_analyze", mode = "function")) {
    return(empty)
  }
  if (is.null(d_is) || !is.data.frame(d_is) || !nrow(d_is)) return(empty)
  payload <- tryCatch(
    bblab_payload_from_statements(
      d_is,
      ticker = as.character(ticker %||% "")[1],
      statement_currency = statement_currency,
      notes = notes,
      segment_tables = segment_tables
    ),
    error = function(e) NULL
  )
  if (is.null(payload)) return(empty)
  res <- tryCatch(bblab_analyze(payload), error = function(e) NULL)
  if (is.null(res) || !is.list(res)) return(empty)
  lim <- res$limitations %||% character(0)
  if ("no_multi_business_split" %in% lim) return(empty)
  biz <- res$businesses %||% list()
  if (!length(biz)) return(empty)
  rows <- lapply(biz, function(c) {
    nm <- as.character(c$name %||% c$id %||% "")[1]
    rev <- suppressWarnings(as.numeric(c$revenue)[1])
    data.frame(name = nm, revenue = rev, stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || !nrow(df)) return(empty)
  df <- df[is.finite(df$revenue) & df$revenue > 0 & nzchar(df$name), , drop = FALSE]
  if (nrow(df) < 2L) return(empty)
  rownames(df) <- NULL
  df
}

# ==========================================
# UI
# ==========================================

# Four sidebar pages (enterprise / earnings / revenue). Book value stays pb_asset_module.
# Page chrome matches P/B: Overview (KPI row, formula bar, Run / Reset) + Settings.
# input['mod_rel-rel_mode'] remains so a hidden mode radio can steer the active Implied Price.
.rel_formula_banner <- function(id, text) {
  div(
    id = id,
    text,
    style = paste(
      "font-size:16px; font-weight:bold; color:#2C3E50; text-align:center;",
      "margin-bottom:15px; padding:10px; background-color:#F2F4F4; border-radius:8px;"
    )
  )
}
.rel_settings_note <- function(id, text) {
  div(
    id = id,
    text,
    style = paste(
      "font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
      "margin-bottom:10px; padding:10px; background-color:#F8F9F9;",
      "border-left:4px solid #1a1a1a; border-radius:4px;"
    )
  )
}

.rel_run_row <- function(ns, calc_id, reset_id, sync_id) {
  fluidRow(
    column(width = 4, ynow_calc_btn(ns(calc_id), label = tags$span(class = "ynow-rel-calc-lbl", "Run multiples"))),
    column(width = 4, ynow_reset_defaults_btn(ns(reset_id))),
    column(
      width = 4,
      br(),
      actionButton(
        ns(sync_id),
        label = tags$span(class = "ynow-rel-sync-lbl", "Sync from statements"),
        icon = icon("sync"), class = "btn-sm",
        style = "background-color:#1a1a1a;color:white;border:none;padding:8px 15px;font-weight:bold;border-radius:5px;margin-top:5px;width:100%;"
      )
    )
  )
}

relative_multiples_module_ui <- function(id) {
  ns <- NS(id)
  .mode <- function(val) sprintf("input['mod_rel-rel_mode'] == '%s'", val)
  list(
    tabItem(
      tabName = "rel_mult_ev",
      tabBox(
        title = tags$span(id = "ynow_rel_ev_box", "Enterprise value multiples"),
        width = "auto",
        tabPanel(
          title = tags$span(id = "ynow_rel_multiples_tab_enterprise", "Overview"),
          icon = icon("building"),
          fluidRow(
            class = "ynow-model-kpi-row",
            column(4, class = "ynow-model-kpi-result", valueBoxOutput(ns("vbx_evebitda"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-2", valueBoxOutput(ns("vbx_evebit"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-3", valueBoxOutput(ns("vbx_evfcf"), width = 12))
          ),
          .rel_formula_banner(
            "ynow_rel_formula_enterprise",
            "Implied EV = Metric × Multiple　｜　Equity = EV + Cash − Debt　｜　Price = Equity ÷ Shares"
          ),
          .rel_run_row(ns, "btn_calc_rel_ev", "btn_reset_rel_ev", "btn_sync_rel_ev")
        ),
        tabPanel(
          "Settings",
          icon = icon("cogs"),
          h4(tags$b(id = "ynow_rel_multiples_ev_heading", "Enterprise multiples")),
          .rel_settings_note(
            "ynow_rel_settings_ev_note",
            "Implied EV = Metric × Multiple; Equity = EV + Cash − Debt; Implied Price = Equity ÷ Shares. EV/EBITDA is the cross-capital-structure comparison; it fails when EBITDA is negative."
          ),
          fluidRow(
            column(3, numericInput(ns("ebitda"), tags$span(id = "ynow_rel_lbl_ebitda", "EBITDA"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_ebitda_multiple"), tags$span(id = "ynow_rel_lbl_evebitda_mult", "EV/EBITDA"), value = APP_DEFAULTS$rel_ev_ebitda_multiple %||% 10, min = 0.1, step = 0.5)),
            column(3, numericInput(ns("ebit"), tags$span(id = "ynow_rel_lbl_ebit", "EBIT"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_ebit_multiple"), tags$span(id = "ynow_rel_lbl_evebit_mult", "EV/EBIT"), value = APP_DEFAULTS$rel_ev_ebit_multiple %||% 12, min = 0.1, step = 0.5))
          ),
          fluidRow(
            column(3, numericInput(ns("fcff"), tags$span(id = "ynow_rel_lbl_fcff", "FCFF"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_fcf_multiple"), tags$span(id = "ynow_rel_lbl_evfcf_mult", "EV/FCF"), value = APP_DEFAULTS$rel_ev_fcf_multiple %||% 15, min = 0.1, step = 0.5))
          ),
          tags$p(id = "ynow_rel_multiples_ev_help", class = "help-block", "FCFE is not used. EV/Sales and EV/ARR live on the revenue page."),
          hr(style = "border-top:1px solid #BDC3C7;"),
          h4(tags$b(id = "ynow_rel_multiples_tab_bridge", "Capital bridge & shares")),
          .rel_settings_note(
            "ynow_rel_settings_bridge_note",
            "Shared with revenue multiples and SOTP: Equity = EV + Cash − Debt; Price = Equity ÷ Shares."
          ),
          uiOutput(ns("txt_shares_note")),
          fluidRow(
            column(4, numericInput(ns("cash"), tags$span(id = "ynow_rel_lbl_cash", "Cash"), value = NA, step = 1)),
            column(4, numericInput(ns("debt"), tags$span(id = "ynow_rel_lbl_debt", "Total Debt"), value = NA, step = 1)),
            column(4, numericInput(ns("shares"), tags$span(id = "ynow_rel_lbl_shares", "Shares (quote)"), value = NA, step = 1))
          ),
          tags$p(
            id = "ynow_rel_multiples_bridge_help", class = "help-block",
            "Cash / Debt used for the EV→Equity bridge. Earnings P/E does not use this bridge."
          )
        )
      )
    ),
    tabItem(
      tabName = "rel_multiples_calculator",
      div(
        style = "display:none;",
        radioButtons(
          ns("rel_mode"),
          label = tags$span(id = "ynow_rel_mode_label", "Select capital-claim family:"),
          choices = list(
            "Equity (P/E · Fwd P/E · PEG · P/S)" = "equity",
            "Enterprise (EV/FCF · EV/EBIT · EV/EBITDA · EV/Sales · EV/ARR)" = "enterprise",
            "Revenue (P/S · EV/Sales · EV/ARR)" = "revenue"
          ),
          selected = .rel_normalize_mode(APP_DEFAULTS$rel_mode %||% "equity")
        ),
        tags$p(id = "ynow_rel_mode_help", .mode("equity"))
      ),
      tabBox(
        title = tags$span(id = "ynow_rel_earn_box", "Earnings multiples"),
        width = "auto",
        tabPanel(
          title = tags$span(id = "ynow_rel_multiples_tab_equity", "Overview"),
          icon = icon("chart-line"),
          fluidRow(
            class = "ynow-model-kpi-row",
            column(4, class = "ynow-model-kpi-result", valueBoxOutput(ns("vbx_pe"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-2", valueBoxOutput(ns("vbx_fpe"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-3", valueBoxOutput(ns("vbx_peg"), width = 12))
          ),
          .rel_formula_banner(
            "ynow_rel_formula_equity",
            "Implied Price = EPS × P/E　｜　PEG = P/E ÷ g(%)"
          ),
          .rel_run_row(ns, "btn_calc_rel", "btn_reset_rel", "btn_sync_rel"),
          fluidRow(column(12, uiOutput(ns("ui_rel_result")))),
          uiOutput(ns("ui_rel_param_matrix"))
        ),
        tabPanel(
          "Settings",
          icon = icon("cogs"),
          h4(tags$b(id = "ynow_rel_multiples_pe_heading", "P/E & Forward P/E")),
          .rel_settings_note(
            "ynow_rel_settings_pe_note",
            "Implied Price = Trailing EPS × Selected P/E; Forward Implied Price = Forward EPS × Selected Forward P/E."
          ),
          fluidRow(
            column(3, numericInput(ns("trailing_eps"), tags$span(id = "ynow_rel_lbl_teps", "Trailing EPS"), value = NA, step = 0.01)),
            column(3, numericInput(ns("pe_multiple"), tags$span(id = "ynow_rel_lbl_pe_mult", "Selected P/E"), value = APP_DEFAULTS$rel_pe_multiple %||% 18, min = 0.1, step = 0.5)),
            column(3, numericInput(ns("forward_eps"), tags$span(id = "ynow_rel_lbl_feps", "Forward EPS"), value = NA, step = 0.01)),
            column(3, numericInput(ns("fwd_pe_multiple"), tags$span(id = "ynow_rel_lbl_fpe_mult", "Selected Forward P/E"), value = APP_DEFAULTS$rel_fwd_pe_multiple %||% 18, min = 0.1, step = 0.5))
          ),
          tags$p(
            id = "ynow_rel_multiples_pe_help", class = "help-block",
            "Trailing EPS from latest statements; Forward EPS = Trailing × (1+g) using SGR／revenue CAGR. Selected P/E from App industry defaults — not Yahoo market P/E or price÷P/E back-solve. EPS ≤ 0 → N/A."
          ),
          hr(style = "border-top:1px solid #BDC3C7;"),
          h4(tags$b(id = "ynow_rel_multiples_peg_heading", "PEG (relative indicator)")),
          .rel_settings_note(
            "ynow_rel_settings_peg_note",
            "PEG = P/E ÷ growth(%). Relative indicator only — not a buy/sell threshold."
          ),
          fluidRow(
            column(4, selectInput(
              ns("peg_growth_src"),
              tags$span(id = "ynow_rel_lbl_peg_src", "Growth definition"),
              choices = c("Central terminal SGR" = "sgr", "Historical revenue CAGR" = "rev_cagr"),
              selected = "sgr"
            )),
            column(4, numericInput(ns("peg_growth_pct"), tags$span(id = "ynow_rel_lbl_peg_g", "Growth (%)"), value = NA, step = 0.1)),
            column(4, numericInput(ns("peg_pe"), tags$span(id = "ynow_rel_lbl_peg_pe", "P/E for PEG"), value = NA, min = 0.1, step = 0.5))
          ),
          tags$p(id = "ynow_rel_multiples_peg_help", class = "help-block", "PEG = P/E ÷ growth(%). Not a buy/sell threshold.")
        )
      )
    ),
    tabItem(
      tabName = "rel_mult_rev",
      tabBox(
        title = tags$span(id = "ynow_rel_rev_box", "Revenue multiples"),
        width = "auto",
        tabPanel(
          "Overview",
          icon = icon("shopping-cart"),
          fluidRow(
            class = "ynow-model-kpi-row",
            column(4, class = "ynow-model-kpi-result", valueBoxOutput(ns("vbx_ps"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-2", valueBoxOutput(ns("vbx_evsales"), width = 12)),
            column(4, class = "ynow-model-kpi-tone-3", valueBoxOutput(ns("vbx_evarr"), width = 12))
          ),
          .rel_formula_banner(
            "ynow_rel_formula_revenue",
            "P/S: Implied Equity = Revenue × P/S　｜　EV/Sales: Equity = EV + Cash − Debt"
          ),
          .rel_run_row(ns, "btn_calc_rel_rev", "btn_reset_rel_rev", "btn_sync_rel_rev")
        ),
        tabPanel(
          "Settings",
          icon = icon("cogs"),
          h4(tags$b(id = "ynow_rel_multiples_ps_heading", "P/S (equity sales)")),
          .rel_settings_note(
            "ynow_rel_settings_ps_note",
            "Implied Equity = Revenue × P/S; Implied Price = Equity ÷ Shares (no Cash−Debt bridge). Use when earnings and EBITDA are not usable. Do not compare unlike gross margins with the same P/S."
          ),
          fluidRow(
            column(4, numericInput(ns("ps_revenue"), tags$span(id = "ynow_rel_lbl_ps_revenue", "Revenue"), value = NA, step = 1)),
            column(4, numericInput(ns("ps_multiple"), tags$span(id = "ynow_rel_lbl_ps_mult", "P/S"), value = APP_DEFAULTS$rel_ps_multiple %||% 3, min = 0.01, step = 0.1))
          ),
          tags$p(id = "ynow_rel_multiples_ps_help", class = "help-block", "Equity-side: Implied Equity = Revenue × P/S; Implied Price = Equity ÷ shares. No Cash−Debt bridge (unlike EV/Sales)."),
          tags$p(id = "ynow_rel_multiples_ps_rev_note", class = "help-block", "P/S Revenue syncs with Enterprise Revenue when you Sync from statements; you may override either field."),
          hr(style = "border-top:1px solid #BDC3C7;"),
          h4(tags$b("EV/Sales & EV/ARR")),
          fluidRow(
            column(3, numericInput(ns("revenue"), tags$span(id = "ynow_rel_lbl_revenue", "Revenue"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_sales_multiple"), tags$span(id = "ynow_rel_lbl_evsales_mult", "EV/Sales"), value = APP_DEFAULTS$rel_ev_sales_multiple %||% 3, min = 0.01, step = 0.1)),
            column(3, numericInput(ns("arr"), tags$span(id = "ynow_rel_lbl_arr", "ARR"), value = NA, step = 1)),
            column(3, numericInput(ns("ev_arr_multiple"), tags$span(id = "ynow_rel_lbl_evarr_mult", "EV/ARR"), value = APP_DEFAULTS$rel_ev_arr_multiple %||% 10, min = 0.1, step = 0.5))
          ),
          tags$p(id = "ynow_rel_multiples_arr_help", class = "help-block", "ARR is not in core statements — enter manually or leave blank (N/A). Cash / Debt / Shares are on the enterprise-value Settings page.")
        )
      )
    )
  )
}

# ==========================================
# Server
# ==========================================
relative_multiples_module_server <- function(id,
                                            auto_calc_pulse = reactive(0L),
                                            summary_df = reactive(NULL),
                                            d_income_statement = reactive(NULL),
                                            d_balance_sheet = reactive(NULL),
                                            d_cash_flow = reactive(NULL),
                                            current_price = reactive(NA),
                                            market_cap = reactive(NA),
                                            quote_price = reactive(NA),
                                            current_ticker = reactive(""),
                                            quote_currency = reactive(NA),
                                            financial_currency = reactive(NA),
                                            central_sgr_pct = reactive(NA),
                                            ui_locale = reactive("zh-TW")) {
  moduleServer(id, function(input, output, session) {
    shares_resolve_note <- reactiveVal(NULL)
    calc_token <- reactiveVal(0L)
    last_result <- reactiveVal(NULL)
    family_mode_rv <- reactiveVal(NULL)

    .loc <- function() tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    .str <- function(key) ui_str(key, .loc())
    .quote_px <- function() {
      px <- suppressWarnings(as.numeric(quote_price())[1])
      if (!is.finite(px) || px <= 0) px <- suppressWarnings(as.numeric(current_price())[1])
      px
    }

    sync_from_statements <- function() {
      sum_df <- tryCatch(summary_df(), error = function(e) NULL)
      d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
      d_bs <- tryCatch(d_balance_sheet(), error = function(e) NULL)
      d_cf <- tryCatch(d_cash_flow(), error = function(e) NULL)

      # Metrics from latest statements; multiples from App defaults (industry assumptions).
      # Never seed Selected P/E from Yahoo Trailing／Forward P/E, target price, or price÷P/E 回推.
      mults <- .rel_app_default_multiples()
      pe_def <- round(mults$pe_multiple, 2)
      fpe_def <- round(mults$fwd_pe_multiple, 2)

      teps <- .rel_trailing_eps_from_statements(sum_df, d_is = d_is, d_bs = d_bs)

      g_src <- as.character(input$peg_growth_src %||% "sgr")[1]
      g_pct <- if (identical(g_src, "rev_cagr")) {
        .rel_rev_cagr_pct(d_is)
      } else {
        suppressWarnings(as.numeric(central_sgr_pct())[1])
      }
      feps <- .rel_forward_eps_from_trailing(teps, g_pct)

      updateNumericInput(session, "trailing_eps", value = if (is.finite(teps)) round(teps, 4) else NA)
      updateNumericInput(session, "pe_multiple", value = pe_def)
      updateNumericInput(session, "forward_eps", value = if (is.finite(feps)) round(feps, 4) else NA)
      updateNumericInput(session, "fwd_pe_multiple", value = fpe_def)
      updateNumericInput(session, "peg_pe", value = pe_def)
      updateNumericInput(session, "peg_growth_pct", value = if (is.finite(g_pct)) round(g_pct, 2) else NA)

      fcff <- .rel_latest_fcff(d_cf, d_is = d_is)
      ebit <- .rel_is_metric(d_is, "Operating Income|^EBIT$")
      ebitda <- .rel_is_metric(d_is, "^EBITDA$|Normalized EBITDA")
      revenue <- statement_total_revenue(d_is)
      cd <- statement_cash_debt(d_bs)
      cash <- cd$cash
      debt <- cd$debt

      px_quote <- .quote_px()
      mcap <- suppressWarnings(as.numeric(market_cap())[1])
      tk <- tryCatch(current_ticker(), error = function(e) "")
      q_ccy0 <- tryCatch(quote_currency(), error = function(e) NA_character_)
      f_ccy0 <- tryCatch(financial_currency(), error = function(e) NA_character_)
      sh_res <- tryCatch(
        resolve_valuation_shares(
          d_bs, sum_df, ticker = tk,
          quote_currency = q_ccy0, financial_currency = f_ccy0
        ),
        error = function(e) NULL
      )
      if (is.null(sh_res)) {
        sh_res <- resolve_shares_for_price(
          NA_real_, price = px_quote, market_cap = mcap, ticker = tk,
          quote_currency = q_ccy0, financial_currency = f_ccy0
        )
      }
      adopted <- adopt_valuation_shares(sh_res, f_ccy0, q_ccy0)
      shares <- adopted$shares
      fx_block <- isTRUE(fx_conversion_required(
        infer_statement_currency(f_ccy0, q_ccy0, adopted$method), q_ccy0
      )) || isTRUE(adr_conversion_required(
        infer_statement_currency(f_ccy0, q_ccy0, adopted$method), q_ccy0, adopted$method
      ))
      if (shares_auto_adjust_method(adopted$method)) {
        shares_resolve_note(adopted$note)
      } else if (!is.finite(shares) && fx_block) {
        shares_resolve_note(.str("rel_multiples_shares_fx_block"))
      } else if (!is.finite(shares)) {
        shares_resolve_note(adopted$note)
      } else {
        shares_resolve_note(.str("rel_multiples_shares_basic"))
      }

      # ARR: never invent — leave NA (Missing ≠ 0)
      updateNumericInput(session, "arr", value = NA)

      updateNumericInput(session, "fcff", value = if (is.finite(fcff)) round(fcff, 2) else NA)
      updateNumericInput(session, "ebit", value = if (is.finite(ebit)) round(ebit, 2) else NA)
      updateNumericInput(session, "ebitda", value = if (is.finite(ebitda)) round(ebitda, 2) else NA)
      updateNumericInput(session, "revenue", value = if (is.finite(revenue)) round(revenue, 2) else NA)
      updateNumericInput(session, "ps_revenue", value = if (is.finite(revenue)) round(revenue, 2) else NA)
      updateNumericInput(session, "cash", value = if (is.finite(cash)) round(cash, 2) else NA)
      updateNumericInput(session, "debt", value = if (is.finite(debt)) round(debt, 2) else NA)
      updateNumericInput(session, "shares", value = if (is.finite(shares)) round(shares, 0) else NA)
      # Return server-side snapshot: updateNumericInput is async to the browser,
      # so Lite auto-calc must not wait for input$ round-trip before run_calc().
      list(
        trailing_eps = if (is.finite(teps)) round(teps, 4) else NA_real_,
        pe_multiple = pe_def,
        forward_eps = if (is.finite(feps)) round(feps, 4) else NA_real_,
        fwd_pe_multiple = fpe_def,
        peg_pe = pe_def,
        peg_growth_pct = if (is.finite(g_pct)) round(g_pct, 2) else NA_real_,
        fcff = if (is.finite(fcff)) round(fcff, 2) else NA_real_,
        ebit = if (is.finite(ebit)) round(ebit, 2) else NA_real_,
        ebitda = if (is.finite(ebitda)) round(ebitda, 2) else NA_real_,
        revenue = if (is.finite(revenue)) round(revenue, 2) else NA_real_,
        ps_revenue = if (is.finite(revenue)) round(revenue, 2) else NA_real_,
        cash = if (is.finite(cash)) round(cash, 2) else NA_real_,
        debt = if (is.finite(debt)) round(debt, 2) else NA_real_,
        shares = if (is.finite(shares)) round(shares, 0) else NA_real_,
        arr = NA_real_
      )
    }

    observeEvent(
      list(summary_df(), d_income_statement(), d_balance_sheet(), d_cash_flow(),
           current_price(), quote_price(), market_cap(), current_ticker(), central_sgr_pct()),
      { sync_from_statements() },
      ignoreInit = FALSE
    )
    observeEvent(input$btn_sync_rel, {
      sync_from_statements()
      showNotification(.str("rel_multiples_synced"), type = "message")
    })
    observeEvent(input$btn_sync_rel_ev, {
      sync_from_statements()
      showNotification(.str("rel_multiples_synced"), type = "message")
    })
    observeEvent(input$btn_sync_rel_rev, {
      sync_from_statements()
      showNotification(.str("rel_multiples_synced"), type = "message")
    })
    observeEvent(input$peg_growth_src, {
      d_is <- tryCatch(d_income_statement(), error = function(e) NULL)
      g_pct <- if (identical(as.character(input$peg_growth_src)[1], "rev_cagr")) {
        .rel_rev_cagr_pct(d_is)
      } else {
        suppressWarnings(as.numeric(central_sgr_pct())[1])
      }
      updateNumericInput(session, "peg_growth_pct", value = if (is.finite(g_pct)) round(g_pct, 2) else NA)
    }, ignoreInit = TRUE)

    .reset_rel_inputs <- function() {
      updateRadioButtons(session, "rel_mode", selected = .rel_normalize_mode(APP_DEFAULTS$rel_mode %||% "equity"))
      updateNumericInput(session, "pe_multiple", value = APP_DEFAULTS$rel_pe_multiple %||% 18)
      updateNumericInput(session, "fwd_pe_multiple", value = APP_DEFAULTS$rel_fwd_pe_multiple %||% 18)
      updateNumericInput(session, "ev_fcf_multiple", value = APP_DEFAULTS$rel_ev_fcf_multiple %||% 15)
      updateNumericInput(session, "ev_ebit_multiple", value = APP_DEFAULTS$rel_ev_ebit_multiple %||% 12)
      updateNumericInput(session, "ev_ebitda_multiple", value = APP_DEFAULTS$rel_ev_ebitda_multiple %||% 10)
      updateNumericInput(session, "ev_sales_multiple", value = APP_DEFAULTS$rel_ev_sales_multiple %||% 3)
      updateNumericInput(session, "ps_multiple", value = APP_DEFAULTS$rel_ps_multiple %||% 3)
      updateNumericInput(session, "ev_arr_multiple", value = APP_DEFAULTS$rel_ev_arr_multiple %||% 10)
      updateSelectInput(session, "peg_growth_src", selected = "sgr")
      sync_from_statements()
      family_mode_rv(NULL)
      last_result(NULL)
      calc_token(0L)
    }
    observeEvent(input$btn_reset_rel, { .reset_rel_inputs() })
    observeEvent(input$btn_reset_rel_ev, { .reset_rel_inputs() })
    observeEvent(input$btn_reset_rel_rev, { .reset_rel_inputs() })

    run_calc <- function(snap = NULL) {
      .num <- function(key, fallback = NA_real_) {
        if (is.list(snap) && !is.null(snap[[key]])) {
          v <- suppressWarnings(as.numeric(snap[[key]])[1])
          if (is.finite(v)) return(v)
        }
        suppressWarnings(as.numeric(input[[key]])[1])
      }
      g_src <- as.character(input$peg_growth_src %||% "sgr")[1]
      g_def <- if (identical(g_src, "rev_cagr")) .str("rel_multiples_growth_rev_cagr") else .str("rel_multiples_growth_sgr")
      g_period <- if (identical(g_src, "rev_cagr")) .str("rel_multiples_period_hist_rev") else .str("rel_multiples_period_terminal_sgr")
      pe_mult <- .num("pe_multiple")
      if (!is.finite(pe_mult) || pe_mult <= 0) pe_mult <- APP_DEFAULTS$rel_pe_multiple %||% 18
      fwd_pe_mult <- .num("fwd_pe_multiple")
      if (!is.finite(fwd_pe_mult) || fwd_pe_mult <= 0) {
        fwd_pe_mult <- APP_DEFAULTS$rel_fwd_pe_multiple %||% pe_mult
      }
      pe <- calc_pe_implied_price(.num("trailing_eps"), pe_mult)
      fpe <- calc_pe_implied_price(.num("forward_eps"), fwd_pe_mult)
      if (identical(fpe$status, "unavailable") && identical(fpe$reason, "eps_nonpositive")) {
        fpe$reason <- "forward_eps_unavailable"
      }
      peg_pe <- .num("peg_pe")
      if (!is.finite(peg_pe) || peg_pe <= 0) peg_pe <- pe_mult
      peg <- calc_peg(peg_pe, .num("peg_growth_pct"), growth_definition = g_def, growth_period = g_period)
      cash <- .num("cash")
      debt <- .num("debt")
      shares <- .num("shares")
      ev_fcf_m <- .num("ev_fcf_multiple"); if (!is.finite(ev_fcf_m) || ev_fcf_m <= 0) ev_fcf_m <- APP_DEFAULTS$rel_ev_fcf_multiple %||% 15
      ev_ebit_m <- .num("ev_ebit_multiple"); if (!is.finite(ev_ebit_m) || ev_ebit_m <= 0) ev_ebit_m <- APP_DEFAULTS$rel_ev_ebit_multiple %||% 12
      ev_ebitda_m <- .num("ev_ebitda_multiple"); if (!is.finite(ev_ebitda_m) || ev_ebitda_m <= 0) ev_ebitda_m <- APP_DEFAULTS$rel_ev_ebitda_multiple %||% 10
      ev_sales_m <- .num("ev_sales_multiple"); if (!is.finite(ev_sales_m) || ev_sales_m <= 0) ev_sales_m <- APP_DEFAULTS$rel_ev_sales_multiple %||% 3
      ps_m <- .num("ps_multiple"); if (!is.finite(ps_m) || ps_m <= 0) ps_m <- APP_DEFAULTS$rel_ps_multiple %||% 3
      ev_arr_m <- .num("ev_arr_multiple"); if (!is.finite(ev_arr_m) || ev_arr_m <= 0) ev_arr_m <- APP_DEFAULTS$rel_ev_arr_multiple %||% 10
      evf <- calc_ev_fcf_implied_price(.num("fcff"), ev_fcf_m, cash, debt, shares)
      eve <- calc_ev_metric_implied_price(.num("ebit"), ev_ebit_m, cash, debt, shares, "EBIT")
      eveda <- calc_ev_metric_implied_price(.num("ebitda"), ev_ebitda_m, cash, debt, shares, "EBITDA")
      evs <- calc_ev_metric_implied_price(.num("revenue"), ev_sales_m, cash, debt, shares, "Revenue")
      ps_rev <- .num("ps_revenue")
      if (!is.finite(ps_rev)) ps_rev <- .num("revenue")
      ps <- calc_ps_implied_price(ps_rev, ps_m, shares)
      evarr <- calc_ev_metric_implied_price(.num("arr"), ev_arr_m, cash, debt, shares, "ARR")
      if (identical(evarr$reason, "metric_nonpositive")) evarr$reason <- "arr_unavailable"
      list(
        pe = pe, forward_pe = fpe, peg = peg, ev_fcf = evf,
        ev_ebit = eve, ev_ebitda = eveda, ev_sales = evs, ps = ps,
        ev_arr = evarr, ran_at = Sys.time()
      )
    }

    .run_family <- function(mode) {
      mode <- .rel_normalize_mode(mode)
      family_mode_rv(mode)
      updateRadioButtons(session, "rel_mode", selected = mode)
      last_result(run_calc())
      calc_token(as.integer(calc_token()) + 1L)
    }
    observeEvent(input$btn_calc_rel, { .run_family("equity") })
    observeEvent(input$btn_calc_rel_ev, { .run_family("enterprise") })
    observeEvent(input$btn_calc_rel_rev, { .run_family("revenue") })
    observeEvent(auto_calc_pulse(), {
      pulse <- suppressWarnings(as.numeric(auto_calc_pulse())[1])
      # Pulse reset (ticker clear) must drop stale Implied Price so Composite
      # cannot keep a previous ticker's Multiples marker.
      if (!is.finite(pulse) || pulse <= 0) {
        last_result(NULL)
        return()
      }
      snap <- tryCatch(sync_from_statements(), error = function(e) NULL)
      last_result(run_calc(snap = snap))
      calc_token(as.integer(calc_token()) + 1L)
    }, ignoreInit = TRUE)
    observeEvent(current_ticker(), {
      last_result(NULL)
      calc_token(0L)
    }, ignoreInit = TRUE)

    output$txt_shares_note <- renderUI({
      note <- shares_resolve_note()
      if (!nzchar(note %||% "")) return(NULL)
      tags$p(class = "help-block", style = "color:#9a3412;", note)
    })

    .fmt_px <- function(x) {
      if (!is.finite(x)) return("—")
      format(round(x, 2), big.mark = ",", nsmall = 2)
    }
    .vbx <- function(res_key, label_key, color, icon_name, is_peg = FALSE, is_result = FALSE) {
      renderValueBox({
        res <- last_result()
        node <- if (!is.null(res)) res[[res_key]] else NULL
        val <- if (!is.null(node) && identical(node$status, "ok")) {
          if (isTRUE(is_peg)) sprintf("%.2f", node$peg) else .fmt_px(node$implied_price)
        } else {
          "—"
        }
        disp <- if (isTRUE(is_result)) ynow_model_result_num(val) else val
        valueBox(disp, .str(label_key), icon = icon(icon_name), color = color)
      })
    }
    output$vbx_pe <- .vbx("pe", "rel_multiples_vbx_pe", "aqua", "chart-line", is_result = TRUE)
    output$vbx_fpe <- .vbx("forward_pe", "rel_multiples_vbx_fpe", "light-blue", "binoculars")
    output$vbx_peg <- .vbx("peg", "rel_multiples_vbx_peg", "yellow", "balance-scale", is_peg = TRUE)
    output$vbx_evfcf <- .vbx("ev_fcf", "rel_multiples_vbx_evfcf", "teal", "industry", is_result = TRUE)
    output$vbx_evebit <- .vbx("ev_ebit", "rel_multiples_vbx_evebit", "purple", "briefcase")
    output$vbx_evebitda <- .vbx("ev_ebitda", "rel_multiples_vbx_evebitda", "fuchsia", "cubes")
    output$vbx_evsales <- .vbx("ev_sales", "rel_multiples_vbx_evsales", "navy", "shopping-cart")
    output$vbx_ps <- .vbx("ps", "rel_multiples_vbx_ps", "olive", "tag", is_result = TRUE)
    output$vbx_evarr <- .vbx("ev_arr", "rel_multiples_vbx_evarr", "maroon", "cloud")

    .st_label <- function(node, special = NULL) {
      if (is.null(node)) return(.str("rel_multiples_status_na"))
      if (identical(node$status, "ok")) return(.str("rel_multiples_status_ok"))
      if (!is.null(special) && identical(node$reason, special)) {
        return(.str(paste0("rel_multiples_status_", special)))
      }
      if (identical(node$reason, "forward_eps_unavailable") || identical(node$reason, "eps_nonpositive")) {
        return(.str("rel_multiples_status_fwd_eps"))
      }
      if (identical(node$reason, "fcf_nonpositive")) return(.str("rel_multiples_status_fcf"))
      if (identical(node$reason, "arr_unavailable") || identical(node$reason, "metric_nonpositive")) {
        if (identical(node$metric_name, "ARR") || identical(node$reason, "arr_unavailable")) {
          return(.str("rel_multiples_status_arr"))
        }
      }
      .str("rel_multiples_status_na")
    }

    output$ui_rel_param_matrix <- renderUI({
      tags$div(
        class = "ynow-rel-param-matrix",
        style = "margin:10px 0 14px 0; padding:10px 12px; background:#f8fafc; border:1px solid #e2e8f0; border-radius:8px; font-size:12.5px; color:#334155; line-height:1.45;",
        tags$p(style = "margin:0 0 6px 0;", tags$b(id = "ynow_rel_param_matrix_title", .str("rel_param_matrix_title"))),
        tags$p(style = "margin:0 0 4px 0;", tags$b(.str("rel_param_shared_heading")), " ", .str("rel_param_shared_body")),
        tags$p(style = "margin:0 0 4px 0;", tags$b(.str("rel_param_equity_heading")), " ", .str("rel_param_equity_body")),
        tags$p(style = "margin:0 0 4px 0;", tags$b(.str("rel_param_enterprise_heading")), " ", .str("rel_param_enterprise_body")),
        tags$p(style = "margin:0;", tags$b(.str("rel_param_sotp_heading")), " ", .str("rel_param_sotp_body"))
      )
    })

    output$ui_rel_result <- renderUI({
      calc_token()
      input$rel_mode
      res <- last_result()
      if (is.null(res)) {
        return(tags$p(class = "ynow-macro-hint", .str("rel_multiples_need_run")))
      }
      .row <- function(model, status, detail) {
        tags$tr(tags$td(tags$b(model)), tags$td(status), tags$td(detail))
      }
      .ev_detail <- function(node, label) {
        sprintf(
          "%s: %s · EV %s · Equity %s · %s %s · %.2f×",
          .str("rel_multiples_implied_price"),
          .fmt_px(node$implied_price),
          .fmt_px(node$implied_ev),
          .fmt_px(node$equity_value),
          label,
          if (is.finite(node$metric)) format(round(node$metric, 0), big.mark = ",") else "—",
          if (is.finite(node$multiple)) node$multiple else NA_real_
        )
      }
      rows <- tagList(
          .row(.str("rel_multiples_model_pe"), .st_label(res$pe),
               sprintf("%s: %s · P/E %.2f× · EPS %s", .str("rel_multiples_implied_price"),
                       .fmt_px(res$pe$implied_price),
                       if (is.finite(res$pe$pe_multiple)) res$pe$pe_multiple else NA_real_,
                       if (is.finite(res$pe$eps)) sprintf("%.4f", res$pe$eps) else "—")),
          .row(.str("rel_multiples_model_fpe"), .st_label(res$forward_pe),
               sprintf("%s: %s · Fwd P/E %.2f× · Fwd EPS %s", .str("rel_multiples_implied_price"),
                       .fmt_px(res$forward_pe$implied_price),
                       if (is.finite(res$forward_pe$pe_multiple)) res$forward_pe$pe_multiple else NA_real_,
                       if (is.finite(res$forward_pe$eps)) sprintf("%.4f", res$forward_pe$eps) else "—")),
          .row(.str("rel_multiples_model_peg"), .st_label(res$peg),
               sprintf("PEG %s · P/E %.2f · g %.2f%% · %s · %s",
                       if (is.finite(res$peg$peg)) sprintf("%.2f", res$peg$peg) else "—",
                       if (is.finite(res$peg$pe)) res$peg$pe else NA_real_,
                       if (is.finite(res$peg$growth_pct)) res$peg$growth_pct else NA_real_,
                       res$peg$growth_definition %||% "—", res$peg$growth_period %||% "—")),
          .row(.str("rel_multiples_model_ps"), .st_label(res$ps),
               sprintf("%s: %s · Equity %s · Rev %s · P/S %.2f×",
                       .str("rel_multiples_implied_price"),
                       .fmt_px(res$ps$implied_price), .fmt_px(res$ps$equity_value),
                       if (is.finite(res$ps$revenue)) format(round(res$ps$revenue, 0), big.mark = ",") else "—",
                       if (is.finite(res$ps$multiple)) res$ps$multiple else NA_real_))
      )
      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-condensed table-striped",
          tags$thead(tags$tr(
            tags$th(.str("rel_multiples_col_model")),
            tags$th(.str("rel_multiples_col_status")),
            tags$th(.str("rel_multiples_col_detail"))
          )),
          tags$tbody(rows)
        ),
        tags$p(class = "help-block", style = "margin-top:8px;", .str("rel_multiples_disclaimer")),
        {
          lines <- list()
          if (identical(res$pe$status, "ok")) {
            pe_chk <- multiple_implied_price(res$pe$pe_multiple, res$pe$eps, 1)
            pe_ok <- isTRUE(pe_chk$ok) && is.finite(res$pe$implied_price) &&
              abs(pe_chk$price - res$pe$implied_price) < 0.05
            lines[[length(lines) + 1L]] <- tags$p(
              class = "help-block", style = "margin:4px 0 0 0;",
              "P/E: ", .str(if (pe_ok) "audit_formula_match" else "audit_formula_mismatch"),
              " multiple × EPS."
            )
          }
          if (identical(res$ev_ebitda$status, "ok")) {
            ev_chk <- enterprise_implied_price(
              res$ev_ebitda$metric, res$ev_ebitda$multiple,
              res$ev_ebitda$cash, res$ev_ebitda$debt, res$ev_ebitda$shares
            )
            ev_ok <- isTRUE(ev_chk$ok) && is.finite(res$ev_ebitda$implied_price) &&
              abs(ev_chk$price - res$ev_ebitda$implied_price) < 0.05
            lines[[length(lines) + 1L]] <- tags$p(
              class = "help-block", style = "margin:0;",
              "EV/EBITDA: ", .str(if (ev_ok) "audit_formula_match" else "audit_formula_mismatch"),
              " ", ev_chk$identity, "."
            )
          }
          if (identical(res$forward_pe$status, "ok")) {
            fpe_chk <- multiple_implied_price(res$forward_pe$pe_multiple, res$forward_pe$eps, 1)
            fpe_ok <- isTRUE(fpe_chk$ok) && is.finite(res$forward_pe$implied_price) &&
              abs(fpe_chk$price - res$forward_pe$implied_price) < 0.05
            lines[[length(lines) + 1L]] <- tags$p(
              class = "help-block", style = "margin:0;",
              "Forward P/E: ", .str(if (fpe_ok) "audit_formula_match" else "audit_formula_mismatch"),
              " multiple × forward EPS."
            )
          }
          do.call(tagList, lines)
        }
      )
    })

    .active_pick <- reactive({
      mode <- .rel_normalize_mode(family_mode_rv() %||% input$rel_mode %||% "equity")
      .rel_pick_active_implied(last_result(), mode = mode)
    })

    return(list(
      pe_price = reactive({
        res <- last_result(); if (!is.null(res) && identical(res$pe$status, "ok")) res$pe$implied_price else NA_real_
      }),
      active_method_key = reactive({
        pick <- .active_pick()
        as.character(pick$key %||% NA_character_)[1]
      }),
      chart_label = reactive({
        .rel_chart_label(.active_pick()$key, base = "Multiples")
      }),
      any_implied_price = reactive({
        pick <- .active_pick()
        suppressWarnings(as.numeric(pick$implied_price)[1])
      })
    ))
  })
}
