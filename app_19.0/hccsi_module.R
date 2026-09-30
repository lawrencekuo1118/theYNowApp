# HCCSI Macro UI: composite KPI 1:1 beside Rf; four-index expand is Full-only.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists(".finite1", mode = "function")) {
  .finite1 <- function(x) {
    num <- suppressWarnings(as.numeric(x)[1])
    length(num) == 1L && is.finite(num)
  }
}
if (!exists(".hccsi_mean_excl_na", mode = "function")) {
  .hccsi_mean_excl_na <- function(vals, wts = NULL) {
    v <- suppressWarnings(as.numeric(vals))
    w <- if (is.null(wts)) rep(1, length(v)) else suppressWarnings(as.numeric(wts))
    if (length(w) != length(v)) w <- rep(1, length(v))
    ok <- is.finite(v) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(v[ok] * w[ok]) / sum(w[ok])
  }
}

hccsi_fmt_score <- function(x, digits = 1) {
  num <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(num)) return("—")
  sprintf(paste0("%.", digits, "f"), num)
}

hccsi_alert_class <- function(alert) {
  a <- tolower(as.character(alert %||% "")[1])
  if (a %in% c("critical", "contracting")) return("ynow-hccsi-alert--critical")
  if (a %in% c("warning", "cooling")) return("ynow-hccsi-alert--warning")
  if (a %in% c("watch", "steady")) return("ynow-hccsi-alert--watch")
  if (identical(a, "unavailable") || identical(a, "na") || !nzchar(a)) return("ynow-hccsi-alert--unavailable")
  "ynow-hccsi-alert--normal"
}

hccsi_score_span <- function(x, digits = 1, extra_class = NULL) {
  finite <- .finite1(x)
  cls <- c("ynow-hccsi-num", if (isTRUE(finite)) "ynow-hccsi-flow" else "ynow-hccsi-unavailable", extra_class)
  tags$span(class = paste(cls, collapse = " "), hccsi_fmt_score(x, digits))
}

.hccsi_fmt_signed_pct <- function(x) {
  if (!.finite1(x)) return("—")
  sprintf("%+.1f%%", 100 * as.numeric(x)[1])
}

.hccsi_issuer_label <- function(r) {
  id <- as.character(r$id %||% "")[1]
  if (is.na(id)) id <- ""
  tks <- unique(as.character(r$tickers %||% character(0)))
  tks <- tks[!is.na(tks) & nzchar(tks)]
  codes <- unique(c(if (nzchar(id)) id else character(0), tks))
  if (!length(codes)) return("—")
  paste(codes, collapse = "/")
}

.hccsi_issuer_stages <- function(id, cfg, locale = "en") {
  raw <- as.character(id %||% "")[1]
  lys <- character(0)
  for (ly in names(cfg$layers %||% list())) {
    mem <- as.character(cfg$layers[[ly]])
    if (raw %in% mem) lys <- c(lys, .hccsi_named("ly", ly, locale))
  }
  lys <- unique(lys[nzchar(lys)])
  if (!length(lys)) return("—")
  sep <- if (identical(as.character(locale)[1], "zh-TW")) "；" else "; "
  paste(lys, collapse = sep)
}

.hccsi_html_table <- function(header, body) {
  tags$div(class = "ynow-hccsi-table-wrap",
           tags$table(class = "table table-condensed ynow-hccsi-table", tags$thead(header), tags$tbody(body)))
}

.hccsi_ui <- function(key, locale = "en") {
  if (exists("ui_str", mode = "function")) tryCatch(ui_str(key, locale), error = function(e) key) else key
}

.hccsi_named <- function(kind, id, locale = "en") {
  raw <- as.character(id %||% "")[1]
  if (!nzchar(raw) || is.na(raw)) return("—")
  key <- paste0("hccsi_", kind, "_", raw)
  lab <- .hccsi_ui(key, locale)
  if (!nzchar(lab) || identical(lab, key)) raw else lab
}

hccsi_kpi_box <- function(result, lite = FALSE, locale = "en", ns = NULL, selected = FALSE) {
  score <- if (is.null(result)) NA_real_ else result$composite
  unavailable <- is.null(result) || !.finite1(score) || identical(result$availability, "unavailable")
  alert <- if (isTRUE(unavailable)) "Unavailable" else (result$alert %||% "Unavailable")
  cls <- c("ynow-macro-kpi", "ynow-macro-kpi--hccsi", hccsi_alert_class(alert))
  extra <- NULL
  if (!isTRUE(lite)) {
    cls <- c(cls, "ynow-macro-kpi--clickable")
    if (isTRUE(selected)) cls <- c(cls, "ynow-macro-kpi--selected")
    click_id <- if (!is.null(ns) && is.function(ns)) ns("hccsi_click") else "macro-hccsi_click"
    extra <- list(
      role = "button", tabindex = "0",
      onclick = sprintf(
        paste0(
          "if (document.body && document.body.classList.contains('ynow-lite')) return; ",
          "if (window.Shiny && Shiny.setInputValue) {",
          " Shiny.setInputValue('%s', Date.now(), {priority: 'event'}); }"
        ), click_id
      )
    )
  }
  hint <- if (isTRUE(unavailable)) {
    .hccsi_ui("hccsi_unavailable", locale)
  } else if (isTRUE(lite)) {
    .hccsi_ui("hccsi_disclosure_short", locale)
  } else {
    .hccsi_ui("hccsi_click_hint", locale)
  }
  do.call(tags$div, c(list(
    id = "ynow_macro_hccsi_box",
    class = paste(cls, collapse = " "),
    `data-hccsi-alert` = alert,
    tags$div(class = "ynow-macro-kpi__label", id = "ynow_macro_hccsi_title", .hccsi_ui("hccsi_title", locale)),
    tags$div(class = "ynow-macro-kpi__value ynow-hccsi__value", hccsi_score_span(score)),
    tags$div(class = paste("ynow-macro-kpi__chg", hccsi_alert_class(alert)), id = "ynow_macro_hccsi_alert",
             paste(.hccsi_ui("hccsi_alert_label", locale), alert)),
    tags$div(class = "ynow-macro-hint", id = "ynow_macro_hccsi_hint", hint)
  ), extra))
}

.hccsi_four_boxes <- function(result, locale = "en") {
  idx <- result$indices %||% list()
  items <- list(
    list(id = "health", key = "hccsi_index_health", gloss = "hccsi_index_health_gloss",
         val = idx$statement_development %||% idx$systems_health),
    list(id = "market", key = "hccsi_index_market", gloss = "hccsi_index_market_gloss",
         val = idx$market_vs_benchmark %||% idx$market_observation),
    list(id = "fragility", key = "hccsi_index_fragility", gloss = "hccsi_index_fragility_gloss",
         val = idx$influence_vs_market %||% idx$concentration_fragility),
    list(id = "stress", key = "hccsi_index_stress", gloss = "hccsi_index_stress_gloss",
         val = idx$trajectory_vs_history %||% idx$systemic_stress)
  )
  cols <- lapply(items, function(it) {
    column(width = 3, class = "col-xs-6 col-sm-6 col-md-3",
           tags$div(class = paste("ynow-macro-kpi ynow-hccsi-sub", paste0("ynow-hccsi-sub--", it$id)),
                    tags$div(class = "ynow-macro-kpi__label", .hccsi_ui(it$key, locale)),
                    tags$div(class = "ynow-macro-kpi__value", hccsi_score_span(it$val)),
                    tags$div(class = "ynow-macro-hint", .hccsi_ui(it$gloss, locale))))
  })
  do.call(fluidRow, c(list(class = "ynow-macro-kpi-row ynow-hccsi-four"), cols))
}

.hccsi_layer_table <- function(result, locale = "en") {
  rows <- result$issuers
  if (is.null(rows) || !length(rows)) return(tags$p(class = "ynow-macro-hint", .hccsi_ui("hccsi_empty", locale)))
  cfg <- hccsi_load_config()
  header <- tags$tr(
    tags$th(.hccsi_ui("hccsi_col_layer", locale)),
    tags$th(.hccsi_ui("hccsi_col_issuer", locale)),
    tags$th(.hccsi_ui("hccsi_col_function", locale)),
    tags$th(.hccsi_ui("hccsi_col_health", locale)),
    tags$th(.hccsi_ui("hccsi_col_stress", locale)),
    tags$th(.hccsi_ui("hccsi_col_weight", locale)),
    tags$th(.hccsi_ui("hccsi_col_concentration", locale)))
  seen <- character(0)
  order_ids <- character(0)
  for (ly in names(cfg$layers %||% list())) {
    mem <- intersect(as.character(cfg$layers[[ly]]), names(rows))
    for (id in mem) {
      if (!id %in% seen) {
        seen <- c(seen, id)
        order_ids <- c(order_ids, id)
      }
    }
  }
  order_ids <- c(order_ids, setdiff(names(rows), order_ids))
  body <- lapply(order_ids, function(id) {
    r <- rows[[id]]
    w <- suppressWarnings(as.numeric(r$weight)[1])
    tags$tr(
      tags$td(.hccsi_issuer_stages(id, cfg, locale)),
      tags$td(.hccsi_issuer_label(r)),
      tags$td(.hccsi_named("fn", r$function_id, locale)),
      tags$td(hccsi_score_span(result$issuer_health[[id]])),
      tags$td(hccsi_score_span(result$issuer_stress[[id]])),
      tags$td(if (is.finite(w)) sprintf("%.1f%%", 100 * w) else "—"),
      tags$td(hccsi_score_span(r$inf_score)))
  })
  .hccsi_html_table(header, body)
}

.hccsi_network_ui <- function(result, locale = "en") {
  ch <- result$contagion$channels %||% character(0)
  persist <- result$contagion$persistent_issuers %||% character(0)
  tags$div(
    class = "ynow-hccsi-network",
    tags$p(tags$b(.hccsi_ui("hccsi_contagion_paths", locale)), ": ",
           if (length(ch)) {
             sep <- if (identical(as.character(locale)[1], "zh-TW")) "；" else ", "
             paste(vapply(ch, function(id) .hccsi_named("ch", id, locale), character(1)), collapse = sep)
           } else .hccsi_ui("hccsi_contagion_none", locale)),
    tags$p(tags$b(.hccsi_ui("hccsi_persistent_issuers", locale)), ": ",
           if (length(persist)) paste(persist, collapse = ", ") else "—"),
    tags$p(class = "ynow-macro-hint", .hccsi_ui("hccsi_network_note", locale))
  )
}

.hccsi_in_composite_table <- function(result, locale = "en") {
  rows <- result$issuers
  if (is.null(rows) || !length(rows)) return(tags$p(class = "ynow-macro-hint", .hccsi_ui("hccsi_empty", locale)))
  header <- tags$tr(
    tags$th(.hccsi_ui("hccsi_col_issuer", locale)),
    tags$th(.hccsi_ui("hccsi_col_rev_yoy", locale)),
    tags$th(.hccsi_ui("hccsi_col_gm_delta", locale)),
    tags$th(.hccsi_ui("hccsi_col_capex_own", locale)),
    tags$th(.hccsi_ui("hccsi_col_dd", locale)),
    tags$th(.hccsi_ui("hccsi_col_ret", locale)),
    tags$th(.hccsi_ui("hccsi_col_beta", locale)),
    tags$th(.hccsi_ui("hccsi_col_price_hist", locale)),
    tags$th(.hccsi_ui("hccsi_col_rev_vs_own", locale)))
  body <- lapply(rows, function(r) {
    ret <- if (.finite1(r$ret_1y)) r$ret_1y else r$ret_1m
    ex <- if (.finite1(r$excess_1y)) r$excess_1y else r$abnormal_return
    tags$tr(
      tags$td(.hccsi_issuer_label(r)),
      tags$td(.hccsi_fmt_signed_pct(r$rev_yoy)),
      tags$td(.hccsi_fmt_signed_pct(r$gm_delta)),
      tags$td(.hccsi_fmt_signed_pct(r$capex_vs_own)),
      tags$td(.hccsi_fmt_signed_pct(ex)),
      tags$td(.hccsi_fmt_signed_pct(ret)),
      tags$td(hccsi_fmt_score(r$beta_60d)),
      tags$td(.hccsi_fmt_signed_pct(r$price_vs_hist)),
      tags$td(.hccsi_fmt_signed_pct(r$rev_yoy_vs_own)))
  })
  .hccsi_html_table(header, body)
}

.hccsi_in_composite_block <- function(result, locale = "en") {
  tags$div(
    tags$h4(id = "ynow_macro_hccsi_in_title", .hccsi_ui("hccsi_in_composite_title", locale)),
    .hccsi_in_composite_table(result, locale)
  )
}

hccsi_methodology_notes <- function(locale = "en") {
  body <- tags$div(
    id = "ynow_macro_hccsi_method_body", class = "ynow-hccsi-method",
    tags$p(.hccsi_ui("hccsi_method_selection", locale)),
    tags$p(.hccsi_ui("hccsi_method_scoring", locale)),
    tags$p(.hccsi_ui("hccsi_method_weighting", locale)),
    tags$p(.hccsi_ui("hccsi_method_rebalance", locale)),
    tags$p(.hccsi_ui("hccsi_method_missing", locale)),
    tags$p(.hccsi_ui("hccsi_method_fx_adr", locale)),
    tags$p(.hccsi_ui("hccsi_method_limits", locale))
  )
  if (exists("ynow_notes_block", mode = "function")) ynow_notes_block(body, locale = locale) else body
}

.hccsi_formula_banner <- function(result, locale = "en") {
  dropped <- result$dropped_terms %||% character(0)
  dropped_txt <- if (length(dropped)) {
    gsub("{terms}", paste(dropped, collapse = ", "), .hccsi_ui("hccsi_dropped", locale), fixed = TRUE)
  } else .hccsi_ui("hccsi_dropped_none", locale)
  tags$div(
    class = "ynow-hccsi-formula-banner",
    tags$div(id = "ynow_macro_hccsi_formula", class = "ynow-hccsi-formula-banner__eq",
             .hccsi_ui("hccsi_formula_eq", locale)),
    tags$div(id = "ynow_macro_hccsi_formula_parts", class = "ynow-hccsi-formula-banner__parts",
             .hccsi_ui("hccsi_formula_parts", locale)),
    tags$p(class = "ynow-macro-hint", id = "ynow_macro_hccsi_dropped", dropped_txt)
  )
}

hccsi_expand_ui <- function(result, locale = "en") {
  if (is.null(result)) return(tags$p(class = "ynow-macro-hint", .hccsi_ui("hccsi_empty", locale)))
  tags$div(
    class = "ynow-macro-card ynow-hccsi-expand__card",
    tags$h4(id = "ynow_macro_hccsi_overview_title", .hccsi_ui("hccsi_overview_title", locale)),
    tags$p(class = "ynow-macro-hint", id = "ynow_macro_hccsi_disclosure", .hccsi_ui("hccsi_disclosure", locale)),
    .hccsi_formula_banner(result, locale),
    tags$p(tags$b(.hccsi_ui("hccsi_alert_label", locale)), ": ", result$alert %||% "Unavailable", " · ",
           tags$b(.hccsi_ui("hccsi_highest_risk_layer", locale)), ": ",
           .hccsi_named("ly", result$highest_risk_layer, locale), " · ",
           tags$b(.hccsi_ui("hccsi_top_contributors", locale)), ": ",
           paste(result$top_contributors %||% character(0), collapse = ", ")),
    .hccsi_four_boxes(result, locale),
    tags$h4(id = "ynow_macro_hccsi_layer_title", .hccsi_ui("hccsi_layer_title", locale)),
    .hccsi_layer_table(result, locale),
    tags$h4(id = "ynow_macro_hccsi_network_title", .hccsi_ui("hccsi_network_title", locale)),
    .hccsi_network_ui(result, locale),
    .hccsi_in_composite_block(result, locale),
    tags$h4(id = "ynow_macro_hccsi_method_title", .hccsi_ui("hccsi_method_title", locale)),
    hccsi_methodology_notes(locale)
  )
}
