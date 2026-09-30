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
  if (identical(a, "critical")) return("ynow-hccsi-alert--critical")
  if (identical(a, "warning")) return("ynow-hccsi-alert--warning")
  if (identical(a, "watch")) return("ynow-hccsi-alert--watch")
  if (identical(a, "unavailable") || identical(a, "na") || !nzchar(a)) return("ynow-hccsi-alert--unavailable")
  "ynow-hccsi-alert--normal"
}

hccsi_score_span <- function(x, digits = 1, extra_class = NULL) {
  finite <- .finite1(x)
  cls <- c("ynow-hccsi-num", if (isTRUE(finite)) "ynow-hccsi-flow" else "ynow-hccsi-unavailable", extra_class)
  tags$span(class = paste(cls, collapse = " "), hccsi_fmt_score(x, digits))
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
    list(id = "health", key = "hccsi_index_health", gloss = "hccsi_index_health_gloss", val = idx$systems_health),
    list(id = "stress", key = "hccsi_index_stress", gloss = "hccsi_index_stress_gloss", val = idx$systemic_stress),
    list(id = "fragility", key = "hccsi_index_fragility", gloss = "hccsi_index_fragility_gloss", val = idx$concentration_fragility),
    list(id = "market", key = "hccsi_index_market", gloss = "hccsi_index_market_gloss", val = idx$market_observation)
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
    tags$th(.hccsi_ui("hccsi_col_layer", locale)), tags$th(.hccsi_ui("hccsi_col_health", locale)),
    tags$th(.hccsi_ui("hccsi_col_stress", locale)), tags$th(.hccsi_ui("hccsi_col_weight", locale)),
    tags$th(.hccsi_ui("hccsi_col_concentration", locale)), tags$th(.hccsi_ui("hccsi_col_substitutes", locale)),
    tags$th(.hccsi_ui("hccsi_col_replacement", locale)))
  body <- lapply(names(cfg$layers), function(ly) {
    mem <- intersect(as.character(cfg$layers[[ly]]), names(rows))
    if (!length(mem)) return(NULL)
    h <- .hccsi_mean_excl_na(vapply(mem, function(id) result$issuer_health[[id]], numeric(1)),
                             vapply(mem, function(id) rows[[id]]$weight, numeric(1)))
    s <- .hccsi_mean_excl_na(vapply(mem, function(id) result$issuer_stress[[id]], numeric(1)),
                             vapply(mem, function(id) rows[[id]]$weight, numeric(1)))
    w <- sum(vapply(mem, function(id) as.numeric(rows[[id]]$weight)[1], numeric(1)), na.rm = TRUE)
    subs <- paste(unique(vapply(mem, function(id) as.character(rows[[id]]$substitutes)[1], character(1))), collapse = "; ")
    repl <- mean(vapply(mem, function(id) as.numeric(rows[[id]]$replacement_time_years)[1], numeric(1)), na.rm = TRUE)
    tags$tr(tags$td(.hccsi_named("ly", ly, locale)), tags$td(hccsi_score_span(h)), tags$td(hccsi_score_span(s)),
            tags$td(sprintf("%.1f%%", 100 * w)), tags$td(hccsi_score_span(result$layer_stress[[ly]])),
            tags$td(subs), tags$td(if (is.finite(repl)) sprintf("%.1f", repl) else "—"))
  })
  tags$div(class = "ynow-hccsi-table-wrap",
           tags$table(class = "table table-condensed ynow-hccsi-table", tags$thead(header), tags$tbody(body)))
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

.hccsi_constituent_table <- function(result, locale = "en") {
  rows <- result$issuers
  if (is.null(rows) || !length(rows)) return(tags$p(class = "ynow-macro-hint", .hccsi_ui("hccsi_empty", locale)))
  header <- tags$tr(
    tags$th(.hccsi_ui("hccsi_col_issuer", locale)), tags$th(.hccsi_ui("hccsi_col_function", locale)),
    tags$th(.hccsi_ui("hccsi_col_criticality", locale)), tags$th(.hccsi_ui("hccsi_col_weight_raw", locale)),
    tags$th(.hccsi_ui("hccsi_col_weight", locale)), tags$th(.hccsi_ui("hccsi_col_perf", locale)),
    tags$th(.hccsi_ui("hccsi_col_beta", locale)), tags$th(.hccsi_ui("hccsi_col_dd", locale)),
    tags$th(.hccsi_ui("hccsi_col_fin_ops", locale)), tags$th(.hccsi_ui("hccsi_col_substitutes", locale)),
    tags$th(.hccsi_ui("hccsi_col_confidence", locale)))
  body <- lapply(rows, function(r) {
    tags$tr(
      tags$td(paste(r$id, paste(r$tickers, collapse = "/"))),
      tags$td(.hccsi_named("fn", r$function_id, locale)),
      tags$td(hccsi_fmt_score(r$criticality_prior, 0)),
      tags$td(sprintf("%.1f%%", 100 * as.numeric(r$weight_raw)[1])),
      tags$td(sprintf("%.1f%%", 100 * as.numeric(r$weight)[1])),
      tags$td(if (.finite1(r$ret_1m)) sprintf("%+.1f%%", 100 * r$ret_1m) else "—"),
      tags$td(hccsi_fmt_score(r$beta_60d)),
      tags$td(if (.finite1(r$max_dd)) sprintf("%.1f%%", 100 * r$max_dd) else "—"),
      tags$td(paste(hccsi_fmt_score(r$financial_resilience), "/", hccsi_fmt_score(r$operational_continuity))),
      tags$td(r$substitutes), tags$td(hccsi_fmt_score(r$data_confidence, 0)))
  })
  tags$div(class = "ynow-hccsi-table-wrap",
           tags$table(class = "table table-condensed ynow-hccsi-table", tags$thead(header), tags$tbody(body)))
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

hccsi_expand_ui <- function(result, locale = "en") {
  if (is.null(result)) return(tags$p(class = "ynow-macro-hint", .hccsi_ui("hccsi_empty", locale)))
  dropped <- result$dropped_terms %||% character(0)
  dropped_txt <- if (length(dropped)) {
    gsub("{terms}", paste(dropped, collapse = ", "), .hccsi_ui("hccsi_dropped", locale), fixed = TRUE)
  } else .hccsi_ui("hccsi_dropped_none", locale)
  tags$div(
    class = "ynow-macro-card ynow-hccsi-expand__card",
    tags$h4(id = "ynow_macro_hccsi_overview_title", .hccsi_ui("hccsi_overview_title", locale)),
    tags$p(class = "ynow-macro-hint", id = "ynow_macro_hccsi_disclosure", .hccsi_ui("hccsi_disclosure", locale)),
    tags$p(tags$b(.hccsi_ui("hccsi_alert_label", locale)), ": ", result$alert %||% "Unavailable", " · ",
           tags$b(.hccsi_ui("hccsi_highest_risk_layer", locale)), ": ",
           .hccsi_named("ly", result$highest_risk_layer, locale), " · ",
           tags$b(.hccsi_ui("hccsi_top_contributors", locale)), ": ",
           paste(result$top_contributors %||% character(0), collapse = ", ")),
    tags$p(class = "ynow-macro-hint", id = "ynow_macro_hccsi_dropped", dropped_txt),
    tags$p(class = "ynow-macro-hint", result$formula %||% ""),
    .hccsi_four_boxes(result, locale),
    tags$h4(id = "ynow_macro_hccsi_layer_title", .hccsi_ui("hccsi_layer_title", locale)),
    .hccsi_layer_table(result, locale),
    tags$h4(id = "ynow_macro_hccsi_network_title", .hccsi_ui("hccsi_network_title", locale)),
    .hccsi_network_ui(result, locale),
    tags$h4(id = "ynow_macro_hccsi_constituent_title", .hccsi_ui("hccsi_constituent_title", locale)),
    .hccsi_constituent_table(result, locale),
    tags$h4(id = "ynow_macro_hccsi_method_title", .hccsi_ui("hccsi_method_title", locale)),
    hccsi_methodology_notes(locale)
  )
}
