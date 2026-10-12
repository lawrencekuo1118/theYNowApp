# HTCDI Macro UI: composite KPI 1:1 beside Rf; four-index expand is Full-only.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists(".finite1", mode = "function")) {
  .finite1 <- function(x) {
    num <- suppressWarnings(as.numeric(x)[1])
    length(num) == 1L && is.finite(num)
  }
}
if (!exists(".htcdi_mean_excl_na", mode = "function")) {
  .htcdi_mean_excl_na <- function(vals, wts = NULL) {
    v <- suppressWarnings(as.numeric(vals))
    w <- if (is.null(wts)) rep(1, length(v)) else suppressWarnings(as.numeric(wts))
    if (length(w) != length(v)) w <- rep(1, length(v))
    ok <- is.finite(v) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(v[ok] * w[ok]) / sum(w[ok])
  }
}

htcdi_fmt_score <- function(x, digits = 1) {
  num <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(num)) return("—")
  sprintf(paste0("%.", digits, "f"), num)
}

htcdi_alert_class <- function(alert) {
  a <- tolower(as.character(alert %||% "")[1])
  if (a %in% c("critical", "contracting")) return("ynow-htcdi-alert--critical")
  if (a %in% c("warning", "cooling")) return("ynow-htcdi-alert--warning")
  if (a %in% c("watch", "steady")) return("ynow-htcdi-alert--watch")
  if (identical(a, "unavailable") || identical(a, "na") || !nzchar(a)) return("ynow-htcdi-alert--unavailable")
  "ynow-htcdi-alert--normal"
}

htcdi_score_span <- function(x, digits = 1, extra_class = NULL) {
  finite <- .finite1(x)
  cls <- c("ynow-htcdi-num", if (isTRUE(finite)) "ynow-htcdi-flow" else "ynow-htcdi-unavailable", extra_class)
  tags$span(class = paste(cls, collapse = " "), htcdi_fmt_score(x, digits))
}

.htcdi_fmt_signed_pct <- function(x) {
  if (!.finite1(x)) return("—")
  sprintf("%+.1f%%", 100 * as.numeric(x)[1])
}

.htcdi_issuer_label <- function(r) {
  id <- as.character(r$id %||% "")[1]
  if (is.na(id)) id <- ""
  tks <- unique(as.character(r$tickers %||% character(0)))
  tks <- tks[!is.na(tks) & nzchar(tks)]
  codes <- unique(c(if (nzchar(id)) id else character(0), tks))
  if (!length(codes)) return("—")
  paste(codes, collapse = "/")
}

.htcdi_issuer_stages <- function(id, cfg, locale = "en") {
  raw <- as.character(id %||% "")[1]
  lys <- character(0)
  for (ly in names(cfg$layers %||% list())) {
    mem <- as.character(cfg$layers[[ly]])
    if (raw %in% mem) lys <- c(lys, .htcdi_named("ly", ly, locale))
  }
  lys <- unique(lys[nzchar(lys)])
  if (!length(lys)) return("—")
  sep <- if (identical(as.character(locale)[1], "zh-TW")) "；" else "; "
  paste(lys, collapse = sep)
}

.htcdi_html_table <- function(header, body, table_class = NULL) {
  cls <- c("table", "table-condensed", "ynow-htcdi-table", table_class)
  cls <- paste(Filter(function(x) !is.null(x) && nzchar(as.character(x)[1]), cls), collapse = " ")
  tags$div(
    class = "ynow-htcdi-table-wrap",
    tags$table(class = cls, tags$thead(header), tags$tbody(body))
  )
}

.htcdi_term_badge <- function(term, locale = "en", arrow = FALSE) {
  term <- tolower(as.character(term %||% "")[1])
  lab <- switch(
    term,
    stmt = .htcdi_ui("htcdi_term_stmt", locale),
    mkt = .htcdi_ui("htcdi_term_mkt", locale),
    inf = .htcdi_ui("htcdi_term_inf", locale),
    traj = .htcdi_ui("htcdi_term_traj", locale),
    term
  )
  tags$span(
    class = paste("ynow-htcdi-term-badge", paste0("ynow-htcdi-term--", term)),
    `data-htcdi-term` = term,
    paste0(if (isTRUE(arrow)) "→ " else "", lab)
  )
}

.htcdi_term_th <- function(term, locale = "en") {
  tags$th(
    class = paste("ynow-htcdi-term-col", paste0("ynow-htcdi-term--", term)),
    `data-htcdi-term` = term,
    .htcdi_term_badge(term, locale)
  )
}

.htcdi_term_bridge <- function(locale = "en") {
  tags$div(
    class = "ynow-htcdi-term-bridge",
    tags$p(
      id = "ynow_macro_htcdi_term_bridge",
      class = "ynow-htcdi-term-bridge__lead",
      .htcdi_ui("htcdi_term_bridge", locale)
    ),
    tags$ul(
      class = "ynow-htcdi-term-map",
      tags$li(
        .htcdi_term_badge("stmt", locale),
        tags$span(class = "ynow-htcdi-term-map__arrow", "←"),
        tags$span(id = "ynow_macro_htcdi_map_stmt", .htcdi_ui("htcdi_term_map_stmt", locale))
      ),
      tags$li(
        .htcdi_term_badge("mkt", locale),
        tags$span(class = "ynow-htcdi-term-map__arrow", "←"),
        tags$span(id = "ynow_macro_htcdi_map_mkt", .htcdi_ui("htcdi_term_map_mkt", locale))
      ),
      tags$li(
        .htcdi_term_badge("inf", locale),
        tags$span(class = "ynow-htcdi-term-map__arrow", "←"),
        tags$span(id = "ynow_macro_htcdi_map_inf", .htcdi_ui("htcdi_term_map_inf", locale))
      ),
      tags$li(
        .htcdi_term_badge("traj", locale),
        tags$span(class = "ynow-htcdi-term-map__arrow", "←"),
        tags$span(id = "ynow_macro_htcdi_map_traj", .htcdi_ui("htcdi_term_map_traj", locale))
      )
    )
  )
}

.htcdi_ui <- function(key, locale = "en") {
  if (exists("ui_str", mode = "function")) tryCatch(ui_str(key, locale), error = function(e) key) else key
}

.htcdi_named <- function(kind, id, locale = "en") {
  raw <- as.character(id %||% "")[1]
  if (!nzchar(raw) || is.na(raw)) return("—")
  key <- paste0("htcdi_", kind, "_", raw)
  lab <- .htcdi_ui(key, locale)
  if (!nzchar(lab) || identical(lab, key)) raw else lab
}

htcdi_kpi_box <- function(result, lite = FALSE, locale = "en", ns = NULL, selected = FALSE) {
  score <- if (is.null(result)) NA_real_ else result$composite
  loading <- is.list(result) && identical(result$availability, "loading")
  unavailable <- !loading && (is.null(result) || !.finite1(score) || identical(result$availability, "unavailable"))
  alert <- if (isTRUE(loading) || isTRUE(unavailable)) "Unavailable" else (result$alert %||% "Unavailable")
  alert_txt <- if (isTRUE(loading)) .htcdi_ui("htcdi_loading", locale) else alert
  cls <- c("ynow-macro-kpi", "ynow-macro-kpi--htcdi", htcdi_alert_class(alert))
  extra <- NULL
  if (!isTRUE(lite)) {
    cls <- c(cls, "ynow-macro-kpi--clickable")
    if (isTRUE(selected)) cls <- c(cls, "ynow-macro-kpi--selected")
    click_id <- if (!is.null(ns) && is.function(ns)) ns("htcdi_click") else "macro-htcdi_click"
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
  hint <- if (isTRUE(loading)) {
    .htcdi_ui("htcdi_loading", locale)
  } else if (isTRUE(unavailable)) {
    .htcdi_ui("htcdi_unavailable", locale)
  } else if (isTRUE(lite)) {
    .htcdi_ui("htcdi_disclosure_short", locale)
  } else {
    .htcdi_ui("htcdi_click_hint", locale)
  }
  do.call(tags$div, c(list(
    id = "ynow_macro_htcdi_box",
    class = paste(cls, collapse = " "),
    `data-htcdi-alert` = alert,
    tags$div(class = "ynow-macro-kpi__label", id = "ynow_macro_htcdi_title", .htcdi_ui("htcdi_title", locale)),
    tags$div(class = "ynow-macro-kpi__value ynow-htcdi__value", htcdi_score_span(score)),
    tags$div(class = paste("ynow-macro-kpi__chg", htcdi_alert_class(alert)), id = "ynow_macro_htcdi_alert",
             paste(.htcdi_ui("htcdi_alert_label", locale), alert_txt)),
    tags$div(class = "ynow-macro-hint", id = "ynow_macro_htcdi_hint", hint)
  ), extra))
}

.htcdi_four_boxes <- function(result, locale = "en") {
  idx <- result$indices %||% list()
  items <- list(
    list(id = "health", key = "htcdi_index_health", gloss = "htcdi_index_health_gloss",
         val = idx$statement_development %||% idx$systems_health),
    list(id = "market", key = "htcdi_index_market", gloss = "htcdi_index_market_gloss",
         val = idx$market_vs_benchmark %||% idx$market_observation),
    list(id = "fragility", key = "htcdi_index_fragility", gloss = "htcdi_index_fragility_gloss",
         val = idx$influence_vs_market %||% idx$concentration_fragility),
    list(id = "stress", key = "htcdi_index_stress", gloss = "htcdi_index_stress_gloss",
         val = idx$trajectory_vs_history %||% idx$systemic_stress)
  )
  cols <- lapply(items, function(it) {
    column(width = 3, class = "col-xs-6 col-sm-6 col-md-3",
           tags$div(class = paste("ynow-macro-kpi ynow-htcdi-sub", paste0("ynow-htcdi-sub--", it$id)),
                    tags$div(class = "ynow-macro-kpi__label", .htcdi_ui(it$key, locale)),
                    tags$div(class = "ynow-macro-kpi__value", htcdi_score_span(it$val)),
                    tags$div(class = "ynow-macro-hint", .htcdi_ui(it$gloss, locale))))
  })
  do.call(fluidRow, c(list(class = "ynow-macro-kpi-row ynow-htcdi-four"), cols))
}

.htcdi_layer_table <- function(result, locale = "en") {
  rows <- result$issuers
  if (is.null(rows) || !length(rows)) return(tags$p(class = "ynow-macro-hint", .htcdi_ui("htcdi_empty", locale)))
  cfg <- htcdi_load_config()
  # Column order mirrors the formula banner: Stmt · Mkt · Inf · Traj.
  header <- tags$tr(
    tags$th(.htcdi_ui("htcdi_col_layer", locale)),
    tags$th(.htcdi_ui("htcdi_col_issuer", locale)),
    tags$th(.htcdi_ui("htcdi_col_function", locale)),
    .htcdi_term_th("stmt", locale),
    .htcdi_term_th("mkt", locale),
    .htcdi_term_th("inf", locale),
    .htcdi_term_th("traj", locale),
    tags$th(.htcdi_ui("htcdi_col_weight", locale)))
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
    stmt_v <- result$issuer_health[[id]] %||% r$stmt_score
    mkt_v <- r$mkt_score
    inf_v <- r$inf_score
    traj_v <- result$issuer_stress[[id]] %||% r$traj_score
    tags$tr(
      tags$td(.htcdi_issuer_stages(id, cfg, locale)),
      tags$td(.htcdi_issuer_label(r)),
      tags$td(.htcdi_named("fn", r$function_id, locale)),
      tags$td(class = "ynow-htcdi-term--stmt", htcdi_score_span(stmt_v)),
      tags$td(class = "ynow-htcdi-term--mkt", htcdi_score_span(mkt_v)),
      tags$td(class = "ynow-htcdi-term--inf", htcdi_score_span(inf_v)),
      tags$td(class = "ynow-htcdi-term--traj", htcdi_score_span(traj_v)),
      tags$td(if (is.finite(w)) sprintf("%.1f%%", 100 * w) else "—"))
  })
  .htcdi_html_table(header, body, table_class = "ynow-htcdi-table--stages")
}

.htcdi_channel_def <- function(id, cfg = NULL) {
  cfg <- htcdi_load_config(cfg)
  for (ch in cfg$contagion_channels %||% list()) {
    if (identical(as.character(ch$id %||% "")[1], as.character(id %||% "")[1])) return(ch)
  }
  NULL
}

.htcdi_channel_chain_nodes <- function(ch, cfg, locale, result) {
  cooling_ids <- as.character(result$contagion$persistent_issuers %||% character(0))
  rows <- result$issuers %||% list()
  if (!is.null(ch$layers) && length(ch$layers)) {
    lapply(as.character(ch$layers), function(ly) {
      mem <- intersect(as.character(cfg$layers[[ly]] %||% character(0)), names(rows))
      cool_mem <- intersect(mem, cooling_ids)
      list(
        kind = "layer",
        id = ly,
        label = .htcdi_named("ly", ly, locale),
        cooling = length(cool_mem) > 0,
        members = mem,
        cool_members = cool_mem
      )
    })
  } else {
    ids <- as.character(ch$issuers %||% character(0))
    lapply(ids, function(id) {
      r <- rows[[id]]
      list(
        kind = "issuer",
        id = id,
        label = if (!is.null(r)) .htcdi_issuer_label(r) else id,
        cooling = id %in% cooling_ids,
        members = id,
        cool_members = if (id %in% cooling_ids) id else character(0)
      )
    })
  }
}

.htcdi_chain_node <- function(node, locale = "en") {
  cool <- isTRUE(node$cooling)
  cls <- c(
    "ynow-htcdi-chain-node",
    if (identical(node$kind, "layer")) "ynow-htcdi-chain-node--layer" else "ynow-htcdi-chain-node--issuer",
    if (cool) "ynow-htcdi-chain-node--cooling" else "ynow-htcdi-chain-node--ok"
  )
  sub <- NULL
  if (identical(node$kind, "layer") && length(node$members)) {
    sub <- tags$span(
      class = "ynow-htcdi-chain-node__mem",
      paste(node$members, collapse = " · ")
    )
  }
  tags$div(
    class = paste(cls, collapse = " "),
    `data-htcdi-node` = as.character(node$id %||% "")[1],
    tags$span(class = "ynow-htcdi-chain-node__lab", node$label),
    if (cool) {
      tags$span(class = "ynow-htcdi-chain-node__state", .htcdi_ui("htcdi_chain_cooling", locale))
    } else {
      NULL
    },
    sub
  )
}

.htcdi_chain_card <- function(ch_id, result, locale = "en") {
  cfg <- htcdi_load_config()
  ch <- .htcdi_channel_def(ch_id, cfg)
  if (is.null(ch)) return(NULL)
  nodes <- .htcdi_channel_chain_nodes(ch, cfg, locale, result)
  if (!length(nodes)) return(NULL)
  flow <- list()
  for (i in seq_along(nodes)) {
    flow <- c(flow, list(.htcdi_chain_node(nodes[[i]], locale)))
    if (i < length(nodes)) {
      flow <- c(flow, list(tags$span(class = "ynow-htcdi-chain-arrow", `aria-hidden` = "true", "→")))
    }
  }
  tags$div(
    class = "ynow-htcdi-chain-card ynow-htcdi-chain-card--lit",
    `data-htcdi-channel` = as.character(ch_id)[1],
    tags$div(class = "ynow-htcdi-chain-card__title", .htcdi_named("ch", ch_id, locale)),
    tags$div(class = "ynow-htcdi-chain-flow", flow)
  )
}

.htcdi_network_ui <- function(result, locale = "en") {
  ch <- result$contagion$channels %||% character(0)
  if (!length(ch)) {
    return(tags$p(class = "ynow-macro-hint", .htcdi_ui("htcdi_contagion_none", locale)))
  }
  cards <- Filter(Negate(is.null), lapply(ch, function(id) .htcdi_chain_card(id, result, locale)))
  tags$div(
    class = "ynow-htcdi-network",
    tags$p(
      id = "ynow_macro_htcdi_contagion_paths",
      class = "ynow-htcdi-network__lead",
      .htcdi_ui("htcdi_contagion_paths", locale)
    ),
    tags$p(
      id = "ynow_macro_htcdi_network_note",
      class = "ynow-macro-hint",
      .htcdi_ui("htcdi_network_note", locale)
    ),
    tags$div(class = "ynow-htcdi-chain-grid", cards)
  )
}

.htcdi_in_composite_table <- function(result, locale = "en") {
  rows <- result$issuers
  if (is.null(rows) || !length(rows)) return(tags$p(class = "ynow-macro-hint", .htcdi_ui("htcdi_empty", locale)))
  # Group live inputs under the same Stmt / Mkt / Inf / Traj terms shown in Function stages.
  header <- htmltools::tagList(
    tags$tr(
      class = "ynow-htcdi-term-groups",
      tags$th(rowspan = "2", .htcdi_ui("htcdi_col_issuer", locale)),
      tags$th(colspan = "3", class = "ynow-htcdi-term--stmt", .htcdi_term_badge("stmt", locale, arrow = TRUE)),
      tags$th(colspan = "2", class = "ynow-htcdi-term--mkt", .htcdi_term_badge("mkt", locale, arrow = TRUE)),
      tags$th(colspan = "1", class = "ynow-htcdi-term--inf", .htcdi_term_badge("inf", locale, arrow = TRUE)),
      tags$th(colspan = "2", class = "ynow-htcdi-term--traj", .htcdi_term_badge("traj", locale, arrow = TRUE))
    ),
    tags$tr(
      class = "ynow-htcdi-term-metrics",
      tags$th(class = "ynow-htcdi-term--stmt", .htcdi_ui("htcdi_col_rev_yoy", locale)),
      tags$th(class = "ynow-htcdi-term--stmt", .htcdi_ui("htcdi_col_gm_delta", locale)),
      tags$th(class = "ynow-htcdi-term--stmt", .htcdi_ui("htcdi_col_capex_own", locale)),
      tags$th(class = "ynow-htcdi-term--mkt", .htcdi_ui("htcdi_col_dd", locale)),
      tags$th(class = "ynow-htcdi-term--mkt", .htcdi_ui("htcdi_col_ret", locale)),
      tags$th(class = "ynow-htcdi-term--inf", .htcdi_ui("htcdi_col_beta", locale)),
      tags$th(class = "ynow-htcdi-term--traj", .htcdi_ui("htcdi_col_price_hist", locale)),
      tags$th(class = "ynow-htcdi-term--traj", .htcdi_ui("htcdi_col_rev_vs_own", locale))
    )
  )
  body <- lapply(rows, function(r) {
    ret <- if (.finite1(r$ret_1y)) r$ret_1y else r$ret_1m
    ex <- if (.finite1(r$excess_1y)) r$excess_1y else r$abnormal_return
    tags$tr(
      tags$td(.htcdi_issuer_label(r)),
      tags$td(class = "ynow-htcdi-term--stmt", .htcdi_fmt_signed_pct(r$rev_yoy)),
      tags$td(class = "ynow-htcdi-term--stmt", .htcdi_fmt_signed_pct(r$gm_delta)),
      tags$td(class = "ynow-htcdi-term--stmt", .htcdi_fmt_signed_pct(r$capex_vs_own)),
      tags$td(class = "ynow-htcdi-term--mkt", .htcdi_fmt_signed_pct(ex)),
      tags$td(class = "ynow-htcdi-term--mkt", .htcdi_fmt_signed_pct(ret)),
      tags$td(class = "ynow-htcdi-term--inf", htcdi_fmt_score(r$beta_60d)),
      tags$td(class = "ynow-htcdi-term--traj", .htcdi_fmt_signed_pct(r$price_vs_hist)),
      tags$td(class = "ynow-htcdi-term--traj", .htcdi_fmt_signed_pct(r$rev_yoy_vs_own)))
  })
  .htcdi_html_table(header, body, table_class = "ynow-htcdi-table--inputs")
}

.htcdi_in_composite_block <- function(result, locale = "en") {
  tags$div(
    class = "ynow-htcdi-in-composite",
    .htcdi_term_bridge(locale),
    tags$h4(id = "ynow_macro_htcdi_in_title", .htcdi_ui("htcdi_in_composite_title", locale)),
    tags$p(
      id = "ynow_macro_htcdi_in_help",
      class = "ynow-macro-hint",
      .htcdi_ui("htcdi_in_composite_help", locale)
    ),
    .htcdi_in_composite_table(result, locale)
  )
}

htcdi_methodology_notes <- function(locale = "en") {
  body <- tags$div(
    id = "ynow_macro_htcdi_method_body", class = "ynow-htcdi-method",
    tags$p(.htcdi_ui("htcdi_method_selection", locale)),
    tags$p(.htcdi_ui("htcdi_method_scoring", locale)),
    tags$p(.htcdi_ui("htcdi_method_weighting", locale)),
    tags$p(.htcdi_ui("htcdi_method_rebalance", locale)),
    tags$p(.htcdi_ui("htcdi_method_missing", locale)),
    tags$p(.htcdi_ui("htcdi_method_fx_adr", locale)),
    tags$p(.htcdi_ui("htcdi_method_limits", locale))
  )
  if (exists("ynow_notes_block", mode = "function")) ynow_notes_block(body, locale = locale) else body
}

.htcdi_formula_banner <- function(result, locale = "en") {
  dropped <- result$dropped_terms %||% character(0)
  dropped_txt <- if (length(dropped)) {
    gsub("{terms}", paste(dropped, collapse = ", "), .htcdi_ui("htcdi_dropped", locale), fixed = TRUE)
  } else .htcdi_ui("htcdi_dropped_none", locale)
  tags$div(
    class = "ynow-htcdi-formula-banner",
    tags$div(id = "ynow_macro_htcdi_formula", class = "ynow-htcdi-formula-banner__eq",
             .htcdi_ui("htcdi_formula_eq", locale)),
    tags$div(id = "ynow_macro_htcdi_formula_parts", class = "ynow-htcdi-formula-banner__parts",
             .htcdi_ui("htcdi_formula_parts", locale)),
    tags$p(class = "ynow-macro-hint", id = "ynow_macro_htcdi_dropped", dropped_txt)
  )
}

htcdi_expand_ui <- function(result, locale = "en") {
  if (is.null(result)) return(tags$p(class = "ynow-macro-hint", .htcdi_ui("htcdi_empty", locale)))
  ch <- result$contagion$channels %||% character(0)
  tags$div(
    class = "ynow-macro-card ynow-htcdi-expand__card",
    tags$h4(id = "ynow_macro_htcdi_overview_title", .htcdi_ui("htcdi_overview_title", locale)),
    tags$p(class = "ynow-macro-hint", id = "ynow_macro_htcdi_disclosure", .htcdi_ui("htcdi_disclosure", locale)),
    .htcdi_formula_banner(result, locale),
    tags$p(tags$b(.htcdi_ui("htcdi_alert_label", locale)), ": ", result$alert %||% "Unavailable", " · ",
           tags$b(.htcdi_ui("htcdi_highest_risk_layer", locale)), ": ",
           .htcdi_named("ly", result$highest_risk_layer, locale), " · ",
           tags$b(.htcdi_ui("htcdi_top_contributors", locale)), ": ",
           paste(result$top_contributors %||% character(0), collapse = ", ")),
    .htcdi_four_boxes(result, locale),
    # Linked-stage cooling sits between the four indices and Function stages.
    if (length(ch)) htmltools::tagList(
      tags$h4(id = "ynow_macro_htcdi_network_title", .htcdi_ui("htcdi_network_title", locale)),
      .htcdi_network_ui(result, locale)
    ),
    tags$h4(id = "ynow_macro_htcdi_layer_title", .htcdi_ui("htcdi_layer_title", locale)),
    .htcdi_layer_table(result, locale),
    .htcdi_in_composite_block(result, locale),
    tags$h4(id = "ynow_macro_htcdi_method_title", .htcdi_ui("htcdi_method_title", locale)),
    htcdi_methodology_notes(locale)
  )
}
