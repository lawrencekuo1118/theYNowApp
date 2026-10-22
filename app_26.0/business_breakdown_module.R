# Business Breakdown — sidebar report module (tab id: company_advance).
# Shares the global Ticker / Stock Code field; not a valuation engine.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.bblab_ui <- function(key, locale = "en") {
  if (exists("ui_str", mode = "function")) {
    tryCatch(ui_str(key, locale), error = function(e) key)
  } else key
}

.bblab_history_visible <- function(res) {
  if (is.null(res) || !is.list(res)) return(FALSE)
  hist <- res$history
  isTRUE(hist$eligible) && length(hist$years) >= 2L && length(hist$series) >= 1L
}

.bblab_fmt_amt <- function(x, digits = 0) {
  num <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(num)) return("—")
  prettyNum(round(num, digits), big.mark = ",", scientific = FALSE)
}

.bblab_fmt_pct <- function(x) {
  num <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(num)) return("—")
  if (abs(num) <= 1.0000001) num <- num * 100
  sprintf("%.1f%%", num)
}

#' Commercial (investment-desk) HTML for the Reconciliation chapter.
#' Explains filed vs sum-of-businesses vs unassigned gap in plain language.
.bblab_recon_commercial_html <- function(rec, locale = "en", ui_msg = NULL) {
  msg <- if (is.function(ui_msg)) {
    ui_msg
  } else {
    function(key, ...) .bblab_ui(key, locale)
  }
  if (is.null(rec) || !is.list(rec)) {
    return(tags$p(class = "help-block", msg("bblab_waiting")))
  }
  overall_ok <- isTRUE(rec$pass)
  verdict <- if (isTRUE(overall_ok)) {
    tags$div(
      class = "ynow-bblab-recon-verdict ynow-bblab-recon-verdict--pass",
      tags$span(class = "ynow-bblab-recon ynow-bblab-recon--pass", msg("bblab_recon_verdict_pass")),
      tags$p(class = "ynow-bblab-recon-verdict__body", msg("bblab_recon_verdict_pass_body"))
    )
  } else {
    tags$div(
      class = "ynow-bblab-recon-verdict ynow-bblab-recon-verdict--fail",
      tags$span(class = "ynow-bblab-recon ynow-bblab-recon--fail", msg("bblab_recon_verdict_fail")),
      tags$p(class = "ynow-bblab-recon-verdict__body", msg("bblab_recon_verdict_fail_body"))
    )
  }
  glossary <- tags$ul(
    class = "ynow-bblab-recon-glossary help-block",
    tags$li(tags$b(msg("bblab_recon_col_reported")), " — ", msg("bblab_recon_col_reported_help")),
    tags$li(tags$b(msg("bblab_recon_col_recast")), " — ", msg("bblab_recon_col_recast_help")),
    tags$li(tags$b(msg("bblab_recon_col_leftover")), " — ", msg("bblab_recon_col_leftover_help")),
    tags$li(tags$b(msg("bblab_recon_col_share")), " — ", msg("bblab_recon_col_share_help"))
  )
  line_note <- function(chk, line_key) {
    if (is.null(chk) || !is.list(chk)) return(NULL)
    if (isTRUE(chk$pass)) return(msg("bblab_recon_line_match"))
    lo <- suppressWarnings(as.numeric(chk$leftover)[1])
    rep <- suppressWarnings(as.numeric(chk$reported)[1])
    if (!is.finite(lo) || abs(lo) < 1e-12) return(msg("bblab_recon_line_gap_unknown"))
    share <- if (is.finite(rep) && abs(rep) > 0) abs(lo) / abs(rep) else NA_real_
    direction <- if (lo > 0) {
      msg("bblab_recon_line_gap_under")
    } else {
      msg("bblab_recon_line_gap_over")
    }
    paste0(
      direction, " ",
      msg("bblab_recon_line_gap_amt"), " ", .bblab_fmt_amt(abs(lo)),
      if (is.finite(share)) paste0(" (", .bblab_fmt_pct(share), " ", msg("bblab_recon_of_filed"), ")") else "",
      "."
    )
  }
  row <- function(label_key, chk) {
    if (is.null(chk)) return(NULL)
    lo <- suppressWarnings(as.numeric(chk$leftover)[1])
    rep <- suppressWarnings(as.numeric(chk$reported)[1])
    share <- if (is.finite(lo) && is.finite(rep) && abs(rep) > 0) lo / abs(rep) else NA_real_
    status_lab <- if (isTRUE(chk$pass)) {
      msg("bblab_recon_status_match")
    } else {
      msg("bblab_recon_status_gap")
    }
    tags$tr(
      class = if (isTRUE(chk$pass)) "ynow-bblab-recon-row--ok" else "ynow-bblab-recon-row--gap",
      tags$td(msg(label_key)),
      tags$td(.bblab_fmt_amt(chk$reported)),
      tags$td(.bblab_fmt_amt(chk$recast)),
      tags$td(.bblab_fmt_amt(chk$leftover)),
      tags$td(if (is.finite(share)) .bblab_fmt_pct(share) else "—"),
      tags$td(status_lab)
    )
  }
  note_block <- function(label_key, chk) {
    note <- line_note(chk, label_key)
    if (is.null(note) || !nzchar(as.character(note)[1])) return(NULL)
    tags$li(
      tags$b(msg(label_key)), ": ", note
    )
  }
  tags$div(
    class = "ynow-bblab-recon-commercial",
    verdict,
    glossary,
    tags$table(
      class = "table table-condensed ynow-bblab-recon-table",
      tags$thead(tags$tr(
        tags$th(msg("bblab_recon_col_line")),
        tags$th(msg("bblab_recon_col_reported")),
        tags$th(msg("bblab_recon_col_recast")),
        tags$th(msg("bblab_recon_col_leftover")),
        tags$th(msg("bblab_recon_col_share")),
        tags$th(msg("bblab_recon_col_status"))
      )),
      tags$tbody(
        row("bblab_kpi_revenue", rec$revenue),
        row("bblab_kpi_cor", rec$cor),
        row("bblab_kpi_gp", rec$gp)
      )
    ),
    tags$div(
      class = "ynow-bblab-recon-line-notes",
      tags$h5(
        id = "ynow_bblab_recon_notes_title",
        class = "ynow-bblab-subhead",
        msg("bblab_recon_notes_title")
      ),
      tags$ul(
        note_block("bblab_kpi_revenue", rec$revenue),
        note_block("bblab_kpi_cor", rec$cor),
        note_block("bblab_kpi_gp", rec$gp)
      ),
      tags$p(class = "help-block", msg("bblab_recon_commercial_foot"))
    )
  )
}

.bblab_snapshot_is_html <- function(cons, locale = "en",
                                    statement_currency = NA_character_,
                                    period = NA_character_) {
  if (is.null(cons) || !is.list(cons)) return(NULL)
  msg <- function(k) .bblab_ui(k, locale)
  gm <- if (.bblab_finite(cons$revenue) && .bblab_finite(cons$gp) && cons$revenue != 0) {
    cons$gp / cons$revenue
  } else NA_real_
  ccy <- cons$currency %||% statement_currency
  per <- cons$period %||% period
  if (!nzchar(.bblab_chr(ccy))) ccy <- "—"
  if (!nzchar(.bblab_chr(per))) per <- "—"
  line <- function(op, lab, amt, formula = NULL, extra = NULL) {
    tags$tr(
      class = paste(c("ynow-bblab-is__row", extra), collapse = " "),
      tags$td(class = "ynow-bblab-is__op", op),
      tags$td(class = "ynow-bblab-is__lab", lab),
      tags$td(class = "ynow-bblab-is__amt", amt),
      tags$td(class = "ynow-bblab-is__fml", if (nzchar(formula %||% "")) formula else "")
    )
  }
  rows <- list(
    line("", msg("bblab_kpi_revenue"), .bblab_fmt_amt(cons$revenue)),
    line("\u2212", msg("bblab_kpi_cor"), .bblab_fmt_amt(cons$cor)),
    line("=", msg("bblab_kpi_gp"), .bblab_fmt_amt(cons$gp),
         msg("bblab_formula_gp"), "ynow-bblab-is__row--total"),
    line("", msg("bblab_kpi_gm"), .bblab_fmt_pct(gm),
         msg("bblab_formula_gm"), "ynow-bblab-is__row--ratio")
  )
  if (.bblab_finite(cons$ni)) {
    rows[[length(rows) + 1L]] <- line(
      "", msg("bblab_kpi_ni"), .bblab_fmt_amt(cons$ni), extra = "ynow-bblab-is__row--ni"
    )
  }
  tags$div(
    class = "ynow-bblab-is",
    tags$div(
      class = "ynow-bblab-is__meta",
      tags$span(
        class = "ynow-bblab-is__meta-item",
        tags$span(class = "ynow-bblab-is__meta-lab", msg("bblab_period_label")),
        tags$span(class = "ynow-bblab-is__meta-val", per)
      ),
      tags$span(
        class = "ynow-bblab-is__meta-item",
        tags$span(class = "ynow-bblab-is__meta-lab", msg("bblab_statement_ccy_label")),
        tags$span(class = "ynow-bblab-is__meta-val", ccy)
      )
    ),
    tags$table(
      class = "ynow-bblab-is__table",
      tags$tbody(rows)
    )
  )
}

.bblab_neutral_class <- function(classification) {
  classification %in% c("OTHER", "UNALLOCATED", "RECONCILIATION", "ROUNDING")
}

.bblab_structure_panel_html <- function(sa, locale = "en") {
  msg <- function(k) .bblab_ui(k, locale)
  if (is.null(sa) || !is.list(sa)) {
    return(tags$p(class = "help-block", msg("bblab_waiting")))
  }
  nd <- msg("bblab_struct_not_disclosed")
  fmt_oi <- function(row) {
    if (!isTRUE(row$operating_income_disclosed)) return(nd)
    .bblab_fmt_amt(row$operating_income)
  }
  fmt_margin <- function(row) {
    if (!isTRUE(row$operating_income_disclosed)) return(nd)
    .bblab_fmt_pct(row$margin)
  }
  fmt_pc <- function(row) {
    if (!isTRUE(row$operating_income_disclosed) || !.bblab_finite(row$profit_contribution)) {
      return(nd)
    }
    .bblab_fmt_pct(row$profit_contribution)
  }
  biz_rows <- sa$businesses %||% list()
  adj_rows <- sa$adjustments %||% list()
  biz_table <- if (!length(biz_rows)) {
    tags$p(class = "help-block", msg("bblab_struct_no_business"))
  } else {
    tags$table(
      class = "table table-condensed table-striped ynow-bblab-struct-table",
      tags$thead(tags$tr(
        tags$th(msg("bblab_struct_col_business")),
        tags$th(msg("bblab_struct_col_revenue")),
        tags$th(msg("bblab_struct_col_rev_pct")),
        tags$th(msg("bblab_struct_col_oi")),
        tags$th(msg("bblab_struct_col_margin")),
        tags$th(msg("bblab_struct_col_profit_pct"))
      )),
      tags$tbody(lapply(biz_rows, function(r) {
        tags$tr(
          tags$td(r$business),
          tags$td(.bblab_fmt_amt(r$revenue)),
          tags$td(.bblab_fmt_pct(r$revenue_pct)),
          tags$td(fmt_oi(r)),
          tags$td(fmt_margin(r)),
          tags$td(fmt_pc(r))
        )
      }))
    )
  }
  adj_table <- if (!length(adj_rows)) {
    tags$p(class = "help-block", msg("bblab_struct_no_adjustments"))
  } else {
    tags$table(
      class = "table table-condensed table-striped ynow-bblab-struct-table",
      tags$thead(tags$tr(
        tags$th(msg("bblab_struct_col_adjustment")),
        tags$th(msg("bblab_struct_col_amount")),
        tags$th(msg("bblab_struct_col_type")),
        tags$th(msg("bblab_struct_col_expl"))
      )),
      tags$tbody(lapply(adj_rows, function(r) {
        tags$tr(
          tags$td(r$adjustment),
          tags$td(.bblab_fmt_amt(r$amount)),
          tags$td(r$type_label %||% r$type),
          tags$td(r$explanation %||% "")
        )
      }))
    )
  }
  tags$div(
    class = "ynow-bblab-struct ynow-bblab-struct--tables",
    tags$h4(id = "ynow_bblab_struct_biz_h", class = "ynow-bblab-subhead",
            msg("bblab_struct_biz_h")),
    biz_table,
    tags$h4(id = "ynow_bblab_struct_adj_h", class = "ynow-bblab-subhead",
            msg("bblab_struct_adj_h")),
    adj_table,
    if (!is.null(sa$bridge_note) && nzchar(.bblab_chr(sa$bridge_note))) {
      tags$p(class = "help-block", sa$bridge_note)
    } else NULL,
    if (length(sa$missing)) {
      tags$p(class = "help-block", paste(sa$missing, collapse = " "))
    } else NULL
  )
}

.bblab_structure_conclusions_html <- function(sa, locale = "en") {
  if (is.null(sa)) return(NULL)
  msg <- function(key) ui_str(key, locale)
  nd <- msg("bblab_struct_not_disclosed")
  conc <- sa$conclusions %||% list()
  conc_list <- tags$ol(
    class = "ynow-bblab-struct-conclusions",
    tags$li(tags$b(msg("bblab_struct_c1")), " ", conc$primary_revenue %||% nd),
    tags$li(tags$b(msg("bblab_struct_c2")), " ", conc$primary_profit %||% nd),
    tags$li(tags$b(msg("bblab_struct_c3")), " ", conc$high_revenue_low_profit %||% nd),
    tags$li(tags$b(msg("bblab_struct_c4")), " ", conc$high_profit_low_revenue %||% nd),
    tags$li(tags$b(msg("bblab_struct_c5")), " ", conc$corporate_consolidation %||% nd),
    tags$li(tags$b(msg("bblab_struct_c6")), " ", conc$non_operating %||% nd)
  )
  tags$div(
    class = "ynow-bblab-struct ynow-bblab-struct--conclusions",
    tags$h4(id = "ynow_bblab_struct_conc_h", class = "ynow-bblab-subhead",
            msg("bblab_struct_conc_h")),
    conc_list,
    tags$p(
      class = "ynow-bblab-struct-summary",
      tags$em(sa$summary_sentence %||% "")
    )
  )
}

#' TRUE only when Search input is clearly not a listed Taiwan or U.S. name.
#' Ambiguous symbols stay FALSE so valid TW/US listings are never blocked.
#' Company-agnostic: no ticker-specific formula branches.
bblab_clearly_not_listed_tw_us <- function(ticker) {
  raw <- toupper(trimws(as.character(ticker %||% "")[1]))
  raw <- sub("\\s+[—\\-–].*$", "", raw)
  raw <- gsub("\\s+", "", raw)
  if (!nzchar(raw) || identical(raw, "NA")) return(FALSE)
  if (grepl("^\\^", raw)) return(TRUE)
  if (grepl("=(X|F)$", raw)) return(TRUE)
  if (grepl("-(USD|USDT|USDC|BTC|ETH)$", raw)) return(TRUE)
  # TW listed: 4–6 digit codes, optional board letter, .TW / .TWO
  if (grepl("^[0-9]{4,6}[A-Z]?(\\.(TW|TWO))?$", raw)) return(FALSE)
  # US listed: 1–5 letters, optional share class (BRK.B / BRK-B)
  if (grepl("^[A-Z]{1,5}([.-][A-Z])?$", raw)) return(FALSE)
  if (grepl("\\.(HK|L|T|SS|SZ|KS|AX|TO|PA|DE|F|SW|MI|MC|SA|MX|NS|BO|JK|SI|NZ|OL|ST|CO|HE|BR|IR|IC|AS|VI|PK|OB)$", raw)) {
    return(TRUE)
  }
  FALSE
}

.bblab_report_section <- function(num, title_id, title_default, help_id = NULL,
                                  help_default = NULL, chapter = NULL, ...) {
  tags$section(
    class = "ynow-bblab-report__section",
    `data-bblab-chapter` = chapter %||% num,
    tags$header(
      class = "ynow-bblab-report__section-head",
      tags$span(class = "ynow-bblab-report__section-num", num),
      tags$h3(id = title_id, class = "ynow-bblab-report__section-title", title_default)
    ),
    if (!is.null(help_id)) {
      tags$p(id = help_id, class = "ynow-bblab-report__section-help", help_default %||% "")
    } else NULL,
    tags$div(class = "ynow-bblab-report__section-body", ...)
  )
}

business_breakdown_lab_ui <- function(id = "bblab") {
  ns <- NS(id)
  tags$div(
    class = "ynow-bblab ynow-bblab--report ynow-full-only",
    tags$article(
      class = "ynow-bblab-report",
      tags$header(
        class = "ynow-bblab-report__cover",
        tags$p(
          id = "ynow_bblab_report_kicker",
          class = "ynow-bblab-report__kicker",
          "Business Breakdown"
        ),
        tags$h1(
          id = "ynow_bblab_page_title",
          class = "ynow-bblab-report__title",
          "Business Breakdown"
        ),
        tags$p(
          id = "ynow_bblab_page_sub",
          class = "ynow-bblab-report__deck",
          paste0(
            "Investment-report view of the filer's business and profitability structure: ",
            "reportable segments, revenue and operating-income mix, accounting / ",
            "consolidation adjustments kept separate, and per-business cards. ",
            "Uses the global Ticker / Stock Code field. Not a valuation engine."
          )
        ),
        tags$style(HTML(paste(
          ".ynow-bblab-struct--tables{width:100%;margin:0 0 16px 0;}",
          ".ynow-bblab-struct--tables .ynow-bblab-struct-table{width:100%;}",
          ".ynow-bblab-struct-mix{display:flex;flex-wrap:wrap;gap:16px;align-items:flex-start;width:100%;}",
          ".ynow-bblab-struct-mix__conc{flex:1 1 48%;min-width:280px;}",
          ".ynow-bblab-struct-mix__chart{flex:1 1 44%;min-width:300px;max-width:560px;overflow:visible;}",
          ".ynow-bblab-struct-mix__chart .plotly,",
          ".ynow-bblab-struct-mix__chart .js-plotly-plot,",
          ".ynow-bblab-struct-mix__chart .plot-container,",
          ".ynow-bblab-struct-mix__chart .svg-container{width:100% !important;overflow:visible !important;}",
          ".ynow-bblab-struct-table{table-layout:fixed;width:100%;}",
          ".ynow-bblab-struct-table thead th{",
          "vertical-align:bottom;min-height:3.25em;height:3.25em;",
          "line-height:1.2;padding:8px 6px;white-space:normal;hyphens:auto;",
          "}",
          ".ynow-bblab-struct-mix__controls{margin:0 0 8px 0;}",
          ".ynow-bblab-struct-mix__controls .shiny-input-container{margin-bottom:6px;}",
          ".ynow-bblab-struct-mix__exports{margin-top:4px;}",
          ".ynow-bblab-struct-mix__exports .btn{margin:0 6px 6px 0;}",
          ".ynow-bblab-bcg{width:100%;margin:18px 0 8px 0;padding:12px 14px;",
          "border:1px solid #e5e5e5;border-radius:6px;background:#fff;}",
          ".ynow-bblab-bcg__grid{display:flex;flex-wrap:wrap;gap:14px;align-items:flex-start;}",
          ".ynow-bblab-bcg__chart{flex:1 1 56%;min-width:300px;}",
          ".ynow-bblab-bcg__list{flex:1 1 36%;min-width:240px;}",
          ".ynow-bblab-bcg__table{width:100%;font-size:12px;}",
          ".ynow-bblab-bcg__table th,.ynow-bblab-bcg__table td{padding:4px 6px;vertical-align:top;}",
          ".ynow-bblab-bcg__q-star{color:#C9A227;font-weight:700;}",
          ".ynow-bblab-bcg__q-cash_cow{color:#249C60;font-weight:700;}",
          ".ynow-bblab-bcg__q-question_mark{color:#1AA8B8;font-weight:700;}",
          ".ynow-bblab-bcg__q-dog{color:#666;font-weight:700;}",
          ".ynow-bblab-orders{width:100%;margin:0;}",
          ".ynow-bblab-orders__grid{display:flex;flex-wrap:wrap;gap:16px;align-items:flex-start;width:100%;}",
          ".ynow-bblab-orders__col{flex:1 1 46%;min-width:280px;}",
          ".ynow-bblab-orders__col .plotly,",
          ".ynow-bblab-orders__col .js-plotly-plot,",
          ".ynow-bblab-orders__col .plot-container,",
          ".ynow-bblab-orders__col .svg-container{width:100% !important;overflow:visible !important;}",
          ".ynow-bblab-orders__meta{font-size:12px;color:#555;margin:4px 0 8px 0;}",
          ".ynow-bblab-or{width:100%;margin:18px 0 0 0;padding-top:8px;border-top:1px solid #e5e5e5;}",
          ".ynow-bblab-or__cards{display:flex;flex-wrap:wrap;gap:10px;margin:10px 0;}",
          ".ynow-bblab-or__card{flex:1 1 200px;min-width:180px;border:1px solid #e5e5e5;border-radius:6px;padding:8px 10px;background:#fff;}",
          ".ynow-bblab-or__card--primary{border-color:#0C5484;}",
          ".ynow-bblab-or__card--callout{background:#f4f8fb;}",
          ".ynow-bblab-or__formula{font-size:12px;color:#555;margin:2px 0;}",
          ".ynow-bblab-or__value{font-size:18px;font-weight:650;margin:0;}",
          ".ynow-bblab-or__layers details{margin:6px 0;border:1px solid #eee;border-radius:6px;padding:6px 10px;background:#fff;}",
          ".ynow-bblab-recon-commercial{width:100%;}",
          ".ynow-bblab-recon-verdict{margin:0 0 12px 0;padding:10px 12px;",
          "border:1px solid #e5e5e5;border-radius:6px;background:#fff;}",
          ".ynow-bblab-recon-verdict__body{margin:6px 0 0 0;}",
          ".ynow-bblab-recon-glossary{margin:0 0 12px 0;padding-left:18px;}",
          ".ynow-bblab-recon-glossary li{margin:2px 0;}",
          ".ynow-bblab-recon-line-notes{margin-top:12px;}",
          ".ynow-bblab-recon-row--gap td{background:#fff8f6;}",
          sep = ""
        ))),
        tags$p(
          id = "ynow_bblab_listed_only_notice",
          class = "ynow-bblab__listed-notice",
          role = "note",
          "Listed stocks only (Taiwan and U.S. exchanges)."
        ),
        tags$div(
          class = "ynow-bblab-report__meta",
          tags$div(
            class = "ynow-bblab-report__meta-item",
            tags$span(id = "ynow_bblab_company_label", class = "ynow-bblab-report__meta-lab", "Company"),
            uiOutput(ns("company_name"))
          ),
          tags$div(
            class = "ynow-bblab-report__meta-item",
            uiOutput(ns("statement_ccy"))
          ),
          tags$div(
            class = "ynow-bblab-report__meta-item ynow-bblab-report__meta-item--grow",
            uiOutput(ns("source_status"))
          )
        ),
        uiOutput(ns("listed_scope"))
      ),

      tags$div(
        class = "ynow-bblab-report__toolbar",
        fluidRow(
          column(
            width = 3,
            selectInput(
              ns("frequency"),
              label = tags$span(id = "ynow_bblab_period_label", "Period"),
              choices = c("Annual" = "annual", "Quarter" = "quarter"),
              selected = "annual"
            )
          ),
          column(
            width = 9,
            tags$div(
              class = "ynow-bblab-report__toolbar-check",
              checkboxInput(
                ns("use_gm_fallback"),
                label = tags$span(
                  id = "ynow_bblab_fallback_gm_label",
                  "Use consolidated Gross Margin as low-confidence fallback"
                ),
                value = FALSE
              )
            )
          )
        ),
        tags$p(
          id = "ynow_bblab_shared_ticker_hint",
          class = "ynow-bblab-report__hint",
          paste0(
            "Uses the header Ticker / Stock Code. Search there to load or refresh this report."
          )
        )
      ),

      uiOutput(ns("toasts_slot")),

      .bblab_report_section(
        "I", "ynow_bblab_ch1_title", "Consolidated statement snapshot",
        "ynow_bblab_ch1_help",
        paste0(
          "Reported consolidated Income Statement totals in statement currency. ",
          "Gross Profit = Revenue − Cost of Revenue. This is the whole firm, before any business split."
        ),
        chapter = "1",
        uiOutput(ns("snapshot"))
      ),

      .bblab_report_section(
        "II", "ynow_bblab_ch2_title", "How the statements split the business",
        "ynow_bblab_ch2_help",
        paste0(
          "One primary reporting dimension is selected. Geography that only describes ",
          "customer location is never the business split. Overlapping dimensions are never added together."
        ),
        chapter = "2",
        uiOutput(ns("summary"))
      ),

      .bblab_report_section(
        "III", "ynow_bblab_struct_title", "Business & profitability structure",
        "ynow_bblab_struct_help",
        paste0(
          "Main businesses come from the filer's reportable segments. ",
          "Corporate, eliminations, reconciliation, non-operating, and accounting ",
          "adjustments are kept separate and are never treated as operating businesses. ",
          "Segment Operating Income is shown only when disclosed — never estimated. ",
          "Main businesses and accounting adjustments span the full width; ",
          "profit-structure conclusions sit beside the revenue-mix chart. ",
          "Revenue-mix slices are shares of reported consolidated revenue."
        ),
        chapter = "3",
        uiOutput(ns("structure_panel")),
        tags$div(
          class = "ynow-bblab-struct-mix",
          tags$div(
            class = "ynow-bblab-struct-mix__conc",
            uiOutput(ns("structure_conclusions"))
          ),
          tags$div(
            class = "ynow-bblab-struct-mix__chart",
            tags$h4(
              class = "ynow-bblab-subhead",
              id = "ynow_bblab_ch3_title",
              "Revenue mix"
            ),
            tags$p(
              id = "ynow_bblab_ch3_help",
              class = "help-block",
              "Current-period slices are shares of reported consolidated revenue."
            ),
            tags$h5(
              class = "ynow-bblab-subhead",
              id = "ynow_bblab_ch3_current_label",
              "Current period"
            ),
            tags$div(
              class = "ynow-bblab-struct-mix__controls",
              radioButtons(ns("chart_mode"), NULL,
                           choices = c("Share %" = "pct", "Amount" = "amount"),
                           selected = "pct", inline = TRUE),
              radioButtons(ns("view_mode"), NULL,
                           choices = c("Reported" = "reported", "Adjusted" = "adjusted"),
                           selected = "reported", inline = TRUE),
              checkboxInput(ns("expand_other"),
                            label = tags$span(id = "ynow_bblab_expand_other", "Expand Other"),
                            value = FALSE),
              tags$div(
                class = "ynow-bblab-struct-mix__exports",
                downloadButton(ns("export_data"), "Export data", class = "btn-sm"),
                downloadButton(ns("export_chart"), "Export chart", class = "btn-sm")
              )
            ),
            uiOutput(ns("chart_status")),
            plotly::plotlyOutput(ns("donut"), height = "360px")
          )
        ),
        uiOutput(ns("bcg_panel")),
        uiOutput(ns("history_panel"))
      ),

      .bblab_report_section(
        "IV", "ynow_bblab_orders_title", "Company orders & market concentration",
        "ynow_bblab_orders_help",
        paste0(
          "Filed major-market geography and counterparty concentration from statements / notes. ",
          "Major markets show revenue by customer location when disclosed. ",
          "Major suppliers and major customers are shown only when the filer reports them — ",
          "never estimated. This block is not the business-dimension split."
        ),
        chapter = "4",
        tags$div(
          class = "ynow-bblab-orders",
          tags$div(
            class = "ynow-bblab-orders__grid",
            tags$div(
              class = "ynow-bblab-orders__col",
              tags$h4(
                id = "ynow_bblab_orders_markets_title",
                class = "ynow-bblab-subhead",
                "Major markets"
              ),
              tags$p(
                id = "ynow_bblab_orders_markets_help",
                class = "help-block",
                "Revenue by customer location (geography) from filed segment notes when available."
              ),
              uiOutput(ns("orders_markets_status")),
              plotly::plotlyOutput(ns("orders_markets_chart"), height = "340px")
            ),
            tags$div(
              class = "ynow-bblab-orders__col",
              tags$h4(
                id = "ynow_bblab_orders_customers_title",
                class = "ynow-bblab-subhead",
                "Major customers"
              ),
              tags$p(
                id = "ynow_bblab_orders_customers_help",
                class = "help-block",
                "Named or anonymized major-customer concentration when disclosed in filings."
              ),
              uiOutput(ns("orders_customers_status")),
              plotly::plotlyOutput(ns("orders_customers_chart"), height = "340px")
            ),
            tags$div(
              class = "ynow-bblab-orders__col",
              tags$h4(
                id = "ynow_bblab_orders_suppliers_title",
                class = "ynow-bblab-subhead",
                "Major suppliers"
              ),
              tags$p(
                id = "ynow_bblab_orders_suppliers_help",
                class = "help-block",
                "Major-supplier concentration when disclosed in filings (often rarer than customers)."
              ),
              uiOutput(ns("orders_suppliers_status")),
              plotly::plotlyOutput(ns("orders_suppliers_chart"), height = "340px")
            )
          ),
          tags$div(
            class = "ynow-bblab-or",
            tags$h4(
              id = "ynow_bblab_or_title",
              class = "ynow-bblab-subhead",
              "Order research"
            ),
            tags$p(
              id = "ynow_bblab_or_help",
              class = "help-block",
              paste0(
                "Three disclosed facts: order quantity, major suppliers, and major-customer ",
                "revenue mix. Taiwan source priority is MOPS, then company IR, then a statement ",
                "cross-check. Broker and industry sources are estimates only and do not fill order amounts."
              )
            ),
            actionButton(
              ns("order_research_run"),
              label = tags$span(id = "ynow_bblab_or_start", "Start analysis"),
              class = "btn-primary btn-sm"
            ),
            uiOutput(ns("order_research_panel")),
            plotly::plotlyOutput(ns("order_research_chart"), height = "280px"),
            plotly::plotlyOutput(ns("order_research_rates_chart"), height = "280px")
          )
        )
      ),

      .bblab_report_section(
        "V", "ynow_bblab_ch5_title", "Business cards",
        "ynow_bblab_ch5_help",
        paste0(
          "One card per supportable business: reported Revenue, Cost of Revenue, ",
          "Gross Profit, and Gross Margin when disclosed or derived. Click a donut slice to focus a card."
        ),
        chapter = "5",
        uiOutput(ns("cards"))
      ),

      .bblab_report_section(
        "VI", "ynow_bblab_ch6_title", "Reconciliation",
        "ynow_bblab_ch6_help",
        paste0(
          "Check whether the business cards add up to the filed consolidated Income Statement. ",
          "Filed consolidated = what the company reported; Sum of businesses = what we assigned ",
          "to operating businesses; Unassigned gap = the leftover not yet attributed. ",
          "This is a disclosure cross-check — not a valuation engine."
        ),
        chapter = "6",
        uiOutput(ns("recon"))
      ),

      tags$section(
        class = "ynow-bblab-report__section ynow-bblab-report__section--muted",
        tags$header(
          class = "ynow-bblab-report__section-head",
          tags$h3(
            id = "ynow_bblab_shared_title",
            class = "ynow-bblab-report__section-title",
            "Shared and Corporate Items"
          )
        ),
        tags$div(class = "ynow-bblab-report__section-body", uiOutput(ns("shared")))
      ),

      .bblab_report_section(
        "VII", "ynow_bblab_ch7_title", "Sources",
        chapter = "7",
        tags$p(
          id = "ynow_bblab_sources_chrome",
          class = "ynow-bblab-report__section-help",
          paste0(
            "Disclosure priority: operating segments → segment notes → product/service revenue → ",
            "revenue disaggregation → MD&A → earnings → IR decks → official descriptions. ",
            "Filed / audited sources are preferred. This page does not write into valuation."
          )
        ),
        uiOutput(ns("notes"))
      )
    )
  )
}

.bblab_card_html <- function(comp, locale = "en", focused = FALSE, cons_rev = NA_real_) {
  if (is.null(comp)) return(NULL)
  share <- if (.bblab_finite(comp$revenue_pct)) comp$revenue_pct else
    if (.bblab_finite(comp$revenue) && .bblab_finite(cons_rev) && cons_rev != 0) comp$revenue / cons_rev else NA_real_
  unest <- .bblab_ui("bblab_gm_unestimable", locale)
  gp_raw <- if (!is.null(comp$gp_display) && nzchar(comp$gp_display)) comp$gp_display else NULL
  gm_raw <- if (!is.null(comp$gm_display) && nzchar(comp$gm_display)) comp$gm_display else NULL
  # Engine stores an English sentinel; localize at render time.
  if (identical(gp_raw, "Not reliably estimable")) gp_raw <- unest
  if (identical(gm_raw, "Not reliably estimable")) gm_raw <- unest
  gp_txt <- if (!is.null(gp_raw) && nzchar(gp_raw)) gp_raw else .bblab_fmt_amt(comp$gp)
  gm_txt <- if (!is.null(gm_raw) && nzchar(gm_raw)) gm_raw else .bblab_fmt_pct(comp$gm)
  reval <- comp$revaluation
  reval_txt <- if (is.null(reval) || identical(reval$status, "UNAVAILABLE") || !is.finite(reval$revaluationRatio)) {
    .bblab_ui("bblab_reval_unavailable", locale)
  } else {
    sprintf("%s  %.3f  (%s)", reval$status, reval$revaluationRatio, .bblab_fmt_pct(reval$revaluationPercentage))
  }
  cls <- c("ynow-bblab-card", paste0("ynow-bblab-card--", tolower(comp$classification %||% "major")))
  if (isTRUE(focused)) cls <- c(cls, "ynow-bblab-card--focus")
  if (.bblab_neutral_class(comp$classification)) cls <- c(cls, "ynow-bblab-card--neutral")
  tags$div(
    class = paste(cls, collapse = " "),
    `data-component-id` = comp$id,
    tags$h4(comp$name),
    tags$p(class = "ynow-bblab-card__class", paste(comp$classification, "·",
                                                   comp$revenue_evidence$confidence %||% "UNAVAILABLE")),
    tags$ul(
      class = "ynow-bblab-card__metrics",
      tags$li(tags$b(paste0(.bblab_ui("bblab_kpi_revenue", locale), ": ")), .bblab_fmt_amt(comp$revenue),
              "  ", tags$span(class = "muted", paste0("(", .bblab_fmt_pct(share), ")"))),
      tags$li(tags$b(paste0(.bblab_ui("bblab_kpi_cor", locale), ": ")),
              if (.bblab_finite(comp$cor)) .bblab_fmt_amt(comp$cor) else unest,
              if (!is.null(comp$cor_label) && !identical(comp$cor_label, "REPORTED"))
                tags$span(class = "ynow-bblab-tag", comp$cor_label) else NULL),
      tags$li(tags$b(paste0(.bblab_ui("bblab_kpi_gp", locale), ": ")), gp_txt),
      tags$li(tags$b(paste0(.bblab_ui("bblab_kpi_gm", locale), ": ")), gm_txt),
      if (.bblab_finite(comp$operating_income)) {
        tags$li(tags$b(paste0(.bblab_ui("bblab_struct_col_oi", locale), ": ")), .bblab_fmt_amt(comp$operating_income))
      } else NULL,
      tags$li(tags$b(.bblab_ui("bblab_reval_label", locale), ": "), reval_txt)
    ),
    if (isTRUE(comp$qualitative_only) && nzchar(comp$description %||% "")) {
      tags$p(class = "muted", comp$description)
    } else NULL
  )
}

.or_fmt_value <- function(value, unit) {
  num <- suppressWarnings(as.numeric(value)[1])
  if (!is.finite(num)) return("—")
  if (identical(unit, "multiple")) return(sprintf("%.2f×", num))
  if (identical(unit, "ratio")) return(sprintf("%.1f%%", num * 100))
  .bblab_fmt_amt(num)
}

.or_gap_text <- function(gap, locale) {
  code <- .bblab_chr(gap, "")
  if (!nzchar(code) || is.na(code)) return("—")
  key <- paste0("notif_bblab_", tolower(code))
  mapped <- .bblab_ui(key, locale)
  if (identical(mapped, key)) "—" else mapped
}

.or_fact_label <- function(fact, locale) {
  nm <- .bblab_chr(fact$name, "")
  if (nzchar(nm)) return(nm)
  key <- .bblab_chr(fact$label_key, "")
  if (!nzchar(key)) return("")
  .bblab_ui(key, locale)
}

#' Order-research panel. Primary surface is the three disclosed buckets.
bblab_order_research_panel_ui <- function(pack, locale = "en", started = FALSE) {
  msg <- function(key) .bblab_ui(key, locale)
  if (!isTRUE(started)) return(tags$p(class = "help-block", msg("bblab_or_idle")))
  if (!is.list(pack) || !length(pack$kpis)) {
    return(tags$p(class = "help-block", msg("bblab_or_waiting")))
  }
  focus <- lapply(pack$focus %||% list(), function(bucket) {
    bid <- .bblab_chr(bucket$id, "")
    bits <- lapply(bucket$facts %||% list(), function(f) {
      if (!is.finite(.bblab_num(f$value))) return(NULL)
      src <- .bblab_chr(f$source, "")
      prov <- msg(if (nzchar(src)) paste0("bblab_or_prov_", src) else "bblab_or_prov_filing")
      cred_id <- .bblab_chr(f$credibility, "")
      cred <- if (nzchar(cred_id)) msg(paste0("bblab_or_cred_", cred_id)) else ""
      tags$li(
        tags$b(paste0(.or_fact_label(f, locale), ": ")),
        .or_fmt_value(f$value, f$unit),
        tags$span(class = "ynow-bblab-or__prov", paste0(" · ", prov)),
        if (nzchar(cred)) tags$span(class = "ynow-bblab-or__cred", paste0(" · ", cred)) else NULL
      )
    })
    bits <- Filter(Negate(is.null), bits)
    if (!length(bits)) bits <- list(tags$li(.or_gap_text(bucket$gap, locale)))
    tags$details(
      class = "ynow-bblab-or__focus",
      open = "open",
      tags$summary(msg(paste0("bblab_or_focus_", bid))),
      tags$ul(bits)
    )
  })
  layers <- lapply(pack$layers %||% list(), function(layer) {
    key <- .bblab_chr(layer$key, "")
    bits <- lapply(layer$metrics %||% list(), function(m) {
      mid <- .bblab_chr(m$id, "")
      label <- msg(paste0("bblab_or_metric_", mid))
      if (identical(label, paste0("bblab_or_metric_", mid))) label <- msg(paste0("bblab_or_kpi_", mid))
      val <- if (nzchar(.bblab_chr(m$text, ""))) {
        .bblab_chr(m$text, "")
      } else if (is.finite(.bblab_num(m$value)) && !identical(m$unit, "flag")) {
        .or_fmt_value(m$value, m$unit)
      } else if (identical(m$status, "REPORTED") && identical(m$unit, "flag")) {
        msg("bblab_or_disclosed_flag")
      } else {
        .or_gap_text(m$gap, locale)
      }
      tags$li(tags$b(paste0(label, ": ")), val)
    })
    ev_bits <- lapply(layer$evidence %||% list(), function(ev) {
      if (!identical(ev$basis, "revenue_not_orders")) return(NULL)
      tags$li(
        msg("bblab_or_revenue_concentration"), " ",
        .or_fmt_value(ev$value, "ratio"),
        " (", .bblab_chr(ev$source, ""), ")"
      )
    })
    tags$details(
      tags$summary(
        paste0(
          msg(paste0("bblab_or_layer_", key)),
          " · ",
          msg(paste0("bblab_or_status_", .bblab_chr(layer$status, "gap")))
        )
      ),
      tags$p(class = "help-block", msg(.bblab_chr(layer$note_key, ""))),
      tags$ul(bits, ev_bits)
    )
  })
  stack_bits <- lapply(pack$source_stack %||% list(), function(t) {
    tid <- .bblab_chr(t$id, "")
    label <- msg(paste0("bblab_or_prov_", tid))
    cred <- msg(paste0("bblab_or_cred_", .bblab_chr(t$credibility, "")))
    state <- msg(if (isTRUE(t$contributed)) "bblab_or_stack_contributed" else "bblab_or_stack_idle")
    amt <- msg(if (isTRUE(t$can_fill_amount)) "bblab_or_stack_amount_ok" else "bblab_or_stack_amount_no")
    tags$li(
      class = "ynow-bblab-or__tier",
      `data-tier` = tid,
      tags$span(class = "ynow-bblab-or__prio", as.character(.bblab_num(t$priority))),
      " ",
      tags$b(label),
      tags$span(class = "ynow-bblab-or__cred", paste0(" · ", cred)),
      tags$span(class = "ynow-bblab-or__stack-state", paste0(" · ", state)),
      tags$span(class = "ynow-bblab-or__stack-amt", paste0(" · ", amt))
    )
  })
  x_bits <- lapply(pack$cross_check %||% list(), function(item) {
    iid <- .bblab_chr(item$id, "")
    label <- msg(paste0("bblab_or_xcheck_", iid))
    val <- if (is.finite(.bblab_num(item$value))) {
      .or_fmt_value(item$value, item$unit)
    } else {
      .or_gap_text(item$gap, locale)
    }
    note <- msg(.bblab_chr(item$note_key, paste0("bblab_or_xcheck_note_", iid)))
    tags$li(
      class = "ynow-bblab-or__xrow",
      `data-xcheck` = iid,
      tags$b(paste0(label, ": ")),
      val,
      tags$span(class = "ynow-bblab-or__xnote", paste0(" · ", note))
    )
  })
  us_ec <- pack$us_earnings_call
  us_block <- NULL
  if (is.list(us_ec) && length(us_ec)) {
    status <- .bblab_chr(us_ec$status, "empty")
    src_id <- .bblab_chr(us_ec$source_id, "empty")
    title <- .bblab_chr(us_ec$title, "")
    excerpt <- .bblab_chr(us_ec$excerpt, "")
    cred <- .bblab_chr(us_ec$credibility, "management")
    links <- list()
    if (nzchar(.bblab_chr(us_ec$url, ""))) {
      links[[length(links) + 1L]] <- tags$a(
        href = us_ec$url, target = "_blank", rel = "noopener noreferrer",
        msg("bblab_or_us_ec_source_link")
      )
    }
    if (nzchar(.bblab_chr(us_ec$sec_url, ""))) {
      links[[length(links) + 1L]] <- tags$a(
        href = us_ec$sec_url, target = "_blank", rel = "noopener noreferrer",
        msg("bblab_or_us_ec_sec_link")
      )
    }
    if (nzchar(.bblab_chr(us_ec$ir_url, ""))) {
      links[[length(links) + 1L]] <- tags$a(
        href = us_ec$ir_url, target = "_blank", rel = "noopener noreferrer",
        msg("bblab_or_us_ec_ir_link")
      )
    }
    # Seeking Alpha = human outbound only (never scraped).
    if (nzchar(.bblab_chr(us_ec$sa_url, ""))) {
      links[[length(links) + 1L]] <- tags$a(
        href = us_ec$sa_url, target = "_blank", rel = "noopener noreferrer",
        msg("bblab_or_us_ec_sa_link")
      )
    }
    link_row <- if (length(links)) {
      tags$p(
        class = "ynow-bblab-or__us-ec-links",
        lapply(seq_along(links), function(i) {
          tagList(if (i > 1L) tags$span(" · "), links[[i]])
        })
      )
    } else {
      NULL
    }
    us_block <- tags$div(
      class = "ynow-bblab-or__us-ec",
      `data-status` = status,
      `data-source` = src_id,
      `data-credibility` = cred,
      tags$p(tags$b(msg("bblab_or_us_ec_title"))),
      tags$p(
        class = "help-block ynow-bblab-or__cred-tier",
        paste0(msg("bblab_or_ec_cred_label"), ": ", msg(paste0("bblab_or_cred_", cred)))
      ),
      if (identical(status, "empty") || !nzchar(title)) {
        tags$p(class = "help-block", msg("bblab_or_us_ec_empty"))
      } else {
        tagList(
          tags$p(
            tags$b(paste0(msg("bblab_or_us_ec_label"), ": ")),
            title,
            if (nzchar(.bblab_chr(us_ec$published, ""))) {
              tags$span(class = "ynow-bblab-or__prov", paste0(" · ", us_ec$published))
            } else NULL
          ),
          if (nzchar(excerpt)) {
            tags$p(class = "help-block ynow-bblab-or__us-ec-excerpt", excerpt)
          } else NULL
        )
      },
      tags$p(class = "help-block", msg("bblab_or_us_ec_method_note")),
      tags$p(class = "help-block", msg("bblab_or_us_ec_sa_note")),
      link_row
    )
  }
  tw_ec <- pack$tw_earnings_call
  tw_block <- NULL
  if (is.list(tw_ec) && length(tw_ec)) {
    status <- .bblab_chr(tw_ec$status, "empty")
    src_id <- .bblab_chr(tw_ec$source_id, "alphamemo_index")
    cred <- .bblab_chr(tw_ec$credibility, "estimate")
    links <- list()
    if (nzchar(.bblab_chr(tw_ec$alphamemo_url, ""))) {
      links[[length(links) + 1L]] <- tags$a(
        href = tw_ec$alphamemo_url, target = "_blank", rel = "noopener noreferrer",
        msg("bblab_or_tw_ec_alphamemo_link")
      )
    }
    if (nzchar(.bblab_chr(tw_ec$alphamemo_search_url, ""))) {
      links[[length(links) + 1L]] <- tags$a(
        href = tw_ec$alphamemo_search_url, target = "_blank", rel = "noopener noreferrer",
        msg("bblab_or_tw_ec_search_link")
      )
    }
    if (nzchar(.bblab_chr(tw_ec$sa_url, ""))) {
      links[[length(links) + 1L]] <- tags$a(
        href = tw_ec$sa_url, target = "_blank", rel = "noopener noreferrer",
        msg("bblab_or_tw_ec_sa_link")
      )
    }
    if (nzchar(.bblab_chr(tw_ec$url, "")) &&
          identical(.bblab_chr(tw_ec$source_id, ""), "seeking_alpha_html")) {
      links[[length(links) + 1L]] <- tags$a(
        href = tw_ec$url, target = "_blank", rel = "noopener noreferrer",
        msg("bblab_or_us_ec_source_link")
      )
    }
    title <- .bblab_chr(tw_ec$title, "")
    excerpt <- .bblab_chr(tw_ec$excerpt, "")
    link_row <- if (length(links)) {
      tags$p(
        class = "ynow-bblab-or__tw-ec-links",
        lapply(seq_along(links), function(i) {
          tagList(if (i > 1L) tags$span(" · "), links[[i]])
        })
      )
    } else {
      NULL
    }
    tw_block <- tags$div(
      class = "ynow-bblab-or__tw-ec",
      `data-status` = status,
      `data-source` = src_id,
      `data-credibility` = cred,
      tags$p(tags$b(msg("bblab_or_tw_ec_title"))),
      tags$p(
        class = "help-block ynow-bblab-or__cred-tier",
        paste0(msg("bblab_or_ec_cred_label"), ": ", msg(paste0("bblab_or_cred_", cred)))
      ),
      if (identical(status, "empty") || !nzchar(title)) {
        tags$p(class = "help-block", msg("bblab_or_tw_ec_empty"))
      } else {
        tagList(
          tags$p(
            tags$b(paste0(msg("bblab_or_us_ec_label"), ": ")),
            title
          ),
          if (nzchar(excerpt)) {
            tags$p(class = "help-block ynow-bblab-or__tw-ec-excerpt", excerpt)
          } else NULL
        )
      },
      tags$p(class = "help-block", msg("bblab_or_tw_ec_note")),
      link_row
    )
  }
  tagList(
    us_block,
    tw_block,
    tags$div(class = "ynow-bblab-or__focuses", focus),
    tags$div(
      class = "ynow-bblab-or__sources",
      tags$p(tags$b(msg("bblab_or_sources_title"))),
      tags$ul(stack_bits)
    ),
    tags$div(
      class = "ynow-bblab-or__xcheck",
      tags$p(tags$b(msg("bblab_or_xcheck_title"))),
      tags$ul(x_bits)
    ),
    tags$details(
      class = "ynow-bblab-or__secondary",
      tags$summary(msg("bblab_or_secondary")),
      tags$div(class = "ynow-bblab-or__layers", layers)
    )
  )
}

bblab_order_research_bar <- function(pack, locale = "en",
                                     which = c("orders", "customers", "multiples", "rates"),
                                     started = FALSE) {
  which <- match.arg(which)
  empty <- plotly::layout(
    plotly::plot_ly(type = "bar"),
    showlegend = FALSE, title = NULL,
    xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
  )
  if (!isTRUE(started) || !is.list(pack)) return(empty)
  points <- pack$chart[[which]] %||% list()
  if (!length(points)) return(empty)
  as_pct <- which %in% c("rates", "customers")
  as_amt <- identical(which, "orders")
  labels <- vapply(points, function(p) {
    lab <- .bblab_chr(p$label, "")
    if (nzchar(lab)) return(lab)
    key <- paste0("bblab_or_metric_", .bblab_chr(p$id, ""))
    mapped <- .bblab_ui(key, locale)
    if (!identical(mapped, key)) return(mapped)
    .bblab_ui(paste0("bblab_or_kpi_", .bblab_chr(p$id, "")), locale)
  }, character(1))
  values <- vapply(points, function(p) {
    v <- .bblab_num(p$value)
    if (isTRUE(as_pct)) v * 100 else v
  }, numeric(1))
  if (!any(is.finite(values))) return(empty)
  pal <- c("#0C5484", "#1AA8B8", "#249C60")
  axis_key <- if (identical(which, "orders")) "bblab_or_chart_orders" else if (identical(which, "customers")) {
    "bblab_or_chart_customers"
  } else if (isTRUE(as_pct)) "bblab_or_chart_rates" else "bblab_or_chart_multiples"
  axis <- .bblab_ui(axis_key, locale)
  fig <- plotly::plot_ly(
    x = labels, y = values, type = "bar",
    marker = list(color = pal[((seq_along(labels) - 1L) %% length(pal)) + 1L]),
    text = if (isTRUE(as_pct)) sprintf("%.1f%%", values) else if (isTRUE(as_amt)) {
      vapply(values, .bblab_fmt_amt, character(1))
    } else sprintf("%.2f×", values),
    textposition = "outside",
    hoverinfo = "text"
  )
  plotly::layout(
    fig,
    showlegend = FALSE,
    xaxis = list(title = ""),
    yaxis = list(title = axis, rangemode = "tozero"),
    margin = list(t = 24, b = 80)
  )
}

business_breakdown_lab_server <- function(id = "bblab",
                                          market_mode_rv = NULL,
                                          ui_locale_rv = NULL,
                                          current_ticker_rv = NULL,
                                          disclosures_provider = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lab_ticker <- reactiveVal(NULL)
    lab_entity <- reactiveVal("")
    lab_payload <- reactiveVal(NULL)
    lab_result <- reactiveVal(NULL)
    lab_codes <- reactiveVal(character(0))
    focus_id <- reactiveVal(NULL)
    or_started <- reactiveVal(FALSE)
    source_status <- reactiveVal("idle")
    listed_scope_on <- reactiveVal(FALSE)

    loc <- function() {
      if (is.reactive(ui_locale_rv)) {
        tryCatch(normalize_ui_locale(ui_locale_rv()), error = function(e) "en")
      } else "en"
    }
    ui_msg <- function(key, ...) {
      msg <- .bblab_ui(key, loc())
      dots <- list(...)
      if (length(dots)) {
        for (nm in names(dots)) {
          msg <- gsub(paste0("{", nm, "}"), as.character(dots[[nm]] %||% ""), msg, fixed = TRUE)
        }
      }
      msg
    }

    # Shared global Ticker / Stock Code (current_ticker_rv) — no in-page Search.
    # Heavy scrape only while Business Breakdown tab is active (avoids Home/DCF double-load).
    observeEvent(
      list(
        if (is.reactive(current_ticker_rv)) current_ticker_rv() else NULL,
        input$frequency,
        tryCatch(session$rootScope()$input$sidebar_tabs, error = function(e) NULL)
      ),
      {
      # NULL ticker → character(0); if (!nzchar(raw)) then crashes ("argument is of length zero").
      raw <- tryCatch({
        v <- if (is.reactive(current_ticker_rv)) current_ticker_rv() else NULL
        if (is.null(v) || length(v) < 1L || (length(v) == 1L && is.na(v))) {
          ""
        } else {
          trimws(as.character(v[[1]]))
        }
      }, error = function(e) "")
      if (length(raw) != 1L || is.na(raw)) raw <- ""
      tab_now <- tryCatch(
        as.character(isolate(session$rootScope()$input$sidebar_tabs %||% ""))[1],
        error = function(e) ""
      )
      if (length(tab_now) != 1L || is.na(tab_now)) tab_now <- ""
      if (!nzchar(raw)) {
        lab_ticker(NULL)
        lab_entity("")
        lab_payload(NULL)
        lab_result(NULL)
        lab_codes(character(0))
        or_started(FALSE)
        source_status("idle")
        listed_scope_on(FALSE)
        return(invisible(NULL))
      }
      # Defer Yahoo/SEC heavy path until the Business Breakdown tab is visible.
      if (!identical(tab_now, "company_advance") &&
          !identical(tab_now, "business_breakdown_lab")) {
        return(invisible(NULL))
      }
      mode <- if (is.reactive(market_mode_rv)) {
        tryCatch(market_mode_rv(), error = function(e) "US")
      } else "US"
      tk <- if (exists("normalize_ticker_for_market", mode = "function")) {
        normalize_ticker_for_market(raw, mode)
      } else raw
      listed_scope_on(
        isTRUE(bblab_clearly_not_listed_tw_us(raw)) ||
          isTRUE(bblab_clearly_not_listed_tw_us(tk))
      )
      lab_ticker(tk)
      lab_result(NULL)
      lab_codes(character(0))
      or_started(FALSE)
      source_status("running")
      focus_id(NULL)

      tryCatch({
      withProgress(message = ui_msg("bblab_progress_running"), value = 0, {
        incProgress(0.1, detail = ui_msg("bblab_stage_resolve"))
        entity_name <- tk
        f_ccy <- NA_character_
        q_ccy <- NA_character_
        usd_twd <- NA_real_
        inst <- "ordinary"
        adr <- NA_real_
        d_is <- NULL
        d_bs <- NULL
        d_cf <- NULL
        inf <- NULL
        sum_df <- NULL
        pull_stmt <- function(bundle, stmt) {
          if (is.null(bundle) || is.null(bundle[[stmt]])) return(NULL)
          exp <- tryCatch(bundle[[stmt]]$expanded, error = function(e) NULL)
          if (is.null(exp)) return(NULL)
          out <- tryCatch({
            if (exists("reorder_financial_columns", mode = "function")) {
              reorder_financial_columns(exp)
            } else if (exists("coerce_financial_df", mode = "function")) {
              coerce_financial_df(exp)
            } else {
              exp
            }
          }, error = function(e) NULL)
          if (!is.null(out) && !is.data.frame(out)) NULL else out
        }

        incProgress(0.25, detail = ui_msg("bblab_stage_retrieve"))
        if (exists("cached_get_summary_data", mode = "function")) {
          sum_df <- tryCatch(cached_get_summary_data(tk), error = function(e) NULL)
        } else if (exists("get_summary_data", mode = "function")) {
          sum_df <- tryCatch(get_summary_data(tk), error = function(e) NULL)
        }
        if (is.data.frame(sum_df) && nrow(sum_df) > 0) {
          entity_name <- as.character(sum_df$Name[1] %||% sum_df$shortName[1] %||% tk)
          if (exists("normalize_ccy", mode = "function")) {
            q_ccy <- normalize_ccy(attr(sum_df, "currency") %||% "")
            f_ccy <- normalize_ccy(attr(sum_df, "financialCurrency") %||% "")
          }
        }
        if (exists("cached_get_yahoo_industry", mode = "function")) {
          inf <- tryCatch(cached_get_yahoo_industry(tk), error = function(e) NULL)
          if (!is.null(inf) && nzchar(as.character(inf$company_name %||% "")[1])) {
            entity_name <- as.character(inf$company_name)[1]
          }
        } else if (exists("get_yahoo_industry", mode = "function")) {
          inf <- tryCatch(get_yahoo_industry(tk), error = function(e) NULL)
          if (!is.null(inf) && nzchar(as.character(inf$company_name %||% "")[1])) {
            entity_name <- as.character(inf$company_name)[1]
          }
        }
        lab_entity(entity_name)
        if (exists("cached_scrape_financials", mode = "function")) {
          # cached_scrape_financials already normalizes — avoid double work on cache hits
          res <- tryCatch(cached_scrape_financials(tk), error = function(e) NULL)
          if (!is.null(res) && !is.null(res[["Income Statement"]])) {
            d_is <- tryCatch({
              exp <- res[["Income Statement"]]$expanded
              if (exists("reorder_financial_columns", mode = "function")) {
                reorder_financial_columns(exp)
              } else if (exists("coerce_financial_df", mode = "function")) {
                coerce_financial_df(exp)
              } else exp
            }, error = function(e) NULL)
            if (!is.null(d_is) && !is.data.frame(d_is)) d_is <- NULL
            meta_fc <- attr(res, "financialCurrency")
            if (!is.null(meta_fc) && exists("normalize_ccy", mode = "function")) {
              mc <- normalize_ccy(meta_fc)
              if (!is.na(mc)) f_ccy <- mc
            }
          }
          d_bs <- pull_stmt(res, "Balance Sheet")
          d_cf <- pull_stmt(res, "Cash Flow")
        }
        if (is.na(f_ccy) || !nzchar(.bblab_chr(f_ccy))) {
          if (grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)) f_ccy <- "TWD"
        }
        if (exists("cached_get_usd_twd_fx", mode = "function")) {
          usd_twd <- tryCatch(cached_get_usd_twd_fx(), error = function(e) NA_real_)
        }

        incProgress(0.45, detail = ui_msg("bblab_stage_parse"))
        extra_disc <- NULL
        if (is.function(disclosures_provider)) {
          extra_disc <- tryCatch(disclosures_provider(tk, d_is), error = function(e) NULL)
        }
        notes <- NULL
        if (is.null(extra_disc) || !length(extra_disc)) {
          is_tw <- grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)
          if (isTRUE(is_tw) &&
              exists("cached_fetch_tw_annual_order_notes", mode = "function")) {
            notes <- tryCatch(
              cached_fetch_tw_annual_order_notes(tk),
              error = function(e) NULL
            )
          } else if (!isTRUE(is_tw) &&
                     exists("cached_fetch_sec_segment_notes", mode = "function")) {
            notes <- tryCatch(cached_fetch_sec_segment_notes(tk, "10-K"), error = function(e) NULL)
          }
        }
        tw_seg_tables <- if (is.list(notes)) notes$segment_tables %||% NULL else NULL
        payload <- bblab_payload_from_statements(
          d_is, ticker = tk, entity_name = entity_name,
          statement_currency = f_ccy, period = NA_character_,
          frequency = isolate(input$frequency) %||% "annual",
          disclosures = extra_disc,
          notes = notes,
          segment_tables = tw_seg_tables,
          instrument_type = inst, adr_ratio = adr, usd_twd = usd_twd,
          display_currency = f_ccy
        )
        industry_key <- ""
        industry_text <- ""
        if (is.list(inf)) {
          industry_text <- paste(inf$sector %||% "", inf$industry %||% "")
          if (exists("resolve_industry_key_from_yahoo", mode = "function")) {
            industry_key <- tryCatch(
              resolve_industry_key_from_yahoo(
                display_text = inf$display_text %||% "",
                sector = inf$sector %||% "",
                industry = inf$industry %||% ""
              ),
              error = function(e) ""
            )
          }
        }
        payload$balance_sheet <- d_bs
        payload$cash_flow <- d_cf
        payload$industry_key <- industry_key
        payload$industry_text <- industry_text
        # US: licensed API / SEC 8-K / IR outbound. TW: AlphaMemo index outbound only.
        is_tw_tk <- grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)
        if (!isTRUE(is_tw_tk) && exists("ynow_us_earnings_call_fetch", mode = "function")) {
          us_ec <- tryCatch(ynow_us_earnings_call_fetch(tk), error = function(e) NULL)
          if (is.list(us_ec)) {
            disc <- if (exists("ynow_us_earnings_call_to_disclosure", mode = "function")) {
              ynow_us_earnings_call_to_disclosure(us_ec)
            } else {
              us_ec
            }
            payload$order_disclosures <- c(
              payload$order_disclosures %||% list(),
              list(earnings_call = disc)
            )
            payload$us_earnings_call <- disc
          }
        }
        if (isTRUE(is_tw_tk) && exists("ynow_tw_earnings_call_fetch", mode = "function")) {
          tw_ec <- tryCatch(ynow_tw_earnings_call_fetch(tk), error = function(e) NULL)
          if (is.list(tw_ec)) {
            disc <- if (exists("ynow_tw_earnings_call_to_disclosure", mode = "function")) {
              ynow_tw_earnings_call_to_disclosure(tw_ec)
            } else {
              tw_ec
            }
            # Do not overwrite a richer earnings_call fact list if already present.
            if (is.null(payload$order_disclosures$earnings_call) ||
                  !length(payload$order_disclosures$earnings_call)) {
              payload$order_disclosures <- c(
                payload$order_disclosures %||% list(),
                list(earnings_call = disc)
              )
            }
            payload$tw_earnings_call <- disc
          }
        }
        lab_payload(payload)
        extracted <- length(payload$dimensions %||% list()) >= 1L &&
          !all(vapply(payload$dimensions, function(d) {
            identical(d$kind %||% "", "official_description")
          }, logical(1)))
        if (is.null(d_is) && !length(extra_disc) && !.bblab_finite(payload$consolidated$revenue)) {
          source_status("statements_unavailable")
        } else if (isTRUE(extracted)) {
          source_status("ok")
        } else {
          source_status("statements_no_segment")
        }

        incProgress(0.7, detail = ui_msg("bblab_stage_analyze"))
        result <- bblab_analyze(
          payload,
          options = list(
            use_consolidated_gm_fallback = isTRUE(isolate(input$use_gm_fallback)),
            view = isolate(input$view_mode) %||% "reported",
            usd_twd = usd_twd,
            display_currency = f_ccy,
            per_adr_display = FALSE
          )
        )
        lab_result(result)
        lab_codes(result$codes %||% character(0))
        incProgress(1, detail = ui_msg("bblab_stage_done"))

        for (tst in result$toasts %||% list()) {
          # Scoped degradation: missing reval / overlapping dims are not Search errors.
          if (identical(tst$code, "BUSINESS_REVALUATION_UNAVAILABLE") ||
              identical(tst$code, "BUSINESS_OVERLAPPING_DIMENSIONS_BLOCKED") ||
              identical(tst$code, "BUSINESS_HISTORY_INSUFFICIENT_YEARS")) {
            next
          }
          msg <- ui_msg(paste0("notif_bblab_", tolower(tst$code)))
          if (identical(msg, paste0("notif_bblab_", tolower(tst$code)))) {
            msg <- paste0(
              tst$code, " — blocked: ",
              paste(tst$blocked_outputs, collapse = ", "),
              "; remaining: ", paste(tst$remaining_outputs, collapse = ", "),
              "; recon ", if (isTRUE(tst$recon_pass)) "pass" else "fail"
            )
          }
          is_page <- identical(tst$code, "BUSINESS_STATEMENTS_UNAVAILABLE") ||
            identical(tst$code, "BUSINESS_ISSUER_UNRESOLVED")
          showNotification(
            msg,
            type = if (isTRUE(is_page) || grepl("FAIL|MISSING", tst$code)) "warning" else "message",
            duration = 8
          )
        }
      })
      }, error = function(e) {
        pl <- tryCatch(isolate(lab_payload()), error = function(e2) NULL)
        has_stmt <- is.list(pl) && .bblab_finite(pl$consolidated$revenue)
        if (isTRUE(has_stmt)) {
          extracted <- length(pl$dimensions %||% list()) >= 1L &&
            !all(vapply(pl$dimensions, function(d) {
              identical(d$kind %||% "", "official_description")
            }, logical(1)))
          source_status(if (isTRUE(extracted)) "ok" else "statements_no_segment")
        } else {
          source_status("statements_unavailable")
          showNotification(
            ui_msg("bblab_source_unavailable"),
            type = "warning",
            duration = 8
          )
        }
      })
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    observeEvent(list(input$use_gm_fallback, input$view_mode), {
      payload <- lab_payload()
      if (is.null(payload)) return()
      result <- bblab_analyze(
        payload,
        options = list(
          use_consolidated_gm_fallback = isTRUE(input$use_gm_fallback),
          view = input$view_mode %||% "reported",
          usd_twd = payload$usd_twd,
          display_currency = payload$display_currency,
          per_adr_display = FALSE
        )
      )
      lab_result(result)
      lab_codes(result$codes %||% character(0))
    }, ignoreInit = TRUE)

    output$company_name <- renderUI({
      nm <- lab_entity()
      tk <- lab_ticker()
      label <- if (nzchar(nm)) nm else "—"
      if (nzchar(.bblab_chr(tk)) && !identical(label, tk)) {
        label <- paste0(label, "  ·  ", tk)
      } else if (nzchar(.bblab_chr(tk)) && identical(label, "—")) {
        label <- tk
      }
      tags$div(class = "ynow-bblab-report__meta-val", label)
    })
    output$statement_ccy <- renderUI({
      payload <- lab_payload()
      ccy <- payload$statement_currency %||% "—"
      tagList(
        tags$span(id = "ynow_bblab_statement_ccy_label", class = "ynow-bblab-report__meta-lab",
                  ui_msg("bblab_statement_ccy_label")),
        tags$div(class = "ynow-bblab-report__meta-val", ccy)
      )
    })
    output$source_status <- renderUI({
      st <- source_status()
      key <- switch(st,
                    idle = "bblab_source_idle",
                    running = "bblab_source_running",
                    ok = "bblab_source_ok",
                    statements_no_segment = "bblab_source_no_segment",
                    statements_unavailable = "bblab_source_unavailable",
                    "bblab_source_idle")
      tags$div(
        class = "ynow-bblab-report__meta-item",
        tags$span(class = "ynow-bblab-report__meta-lab", ui_msg("bblab_source_status_label")),
        tags$div(class = "ynow-bblab-report__meta-val", id = "ynow_bblab_source_status", ui_msg(key))
      )
    })
    output$listed_scope <- renderUI({
      if (!isTRUE(listed_scope_on())) return(NULL)
      tags$p(
        id = "ynow_bblab_listed_only_scope",
        class = "ynow-bblab__listed-scope help-block",
        ui_msg("bblab_listed_only_scope")
      )
    })

    output$snapshot <- renderUI({
      res <- lab_result()
      if (is.null(res)) {
        return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      }
      .bblab_snapshot_is_html(
        res$consolidated %||% list(),
        locale = loc(),
        statement_currency = res$statement_currency,
        period = res$period
      )
    })

    output$summary <- renderUI({
      res <- lab_result()
      if (is.null(res)) {
        return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      }
      kind <- res$primary_dimension$kind %||% "—"
      dim_key <- paste0("bblab_dim_", gsub("[^a-z_]", "", kind))
      dim_lab <- {
        mapped <- ui_msg(dim_key)
        if (identical(mapped, dim_key)) kind else mapped
      }
      n_biz <- length(res$businesses %||% list())
      recon <- res$reconciliation
      codes <- res$codes %||% character(0)
      tags$div(
        class = "ynow-bblab-summary",
        tags$ul(
          class = "ynow-bblab-split",
          tags$li(tags$b(ui_msg("bblab_dimension_label"), ": "), dim_lab),
          tags$li(tags$b(ui_msg("bblab_count_label"), ": "), n_biz),
          tags$li(tags$b(ui_msg("bblab_level_label"), ": "), res$level %||% "—"),
          tags$li(tags$b(ui_msg("bblab_confidence_label"), ": "), res$confidence$overall %||% "UNAVAILABLE"),
          tags$li(tags$b(ui_msg("bblab_rev_recon_label"), ": "),
                  if (isTRUE(recon$revenue$pass)) ui_msg("bblab_pass") else ui_msg("bblab_fail")),
          tags$li(tags$b(ui_msg("bblab_cor_recon_label"), ": "),
                  if (isTRUE(recon$cor$pass) || is.null(recon$cor)) ui_msg("bblab_pass") else ui_msg("bblab_fail")),
          tags$li(tags$b(ui_msg("bblab_reval_avail_label"), ": "),
                  if (isTRUE(res$revaluation_available)) ui_msg("bblab_available") else ui_msg("bblab_reval_unavailable")),
          tags$li(tags$b(ui_msg("bblab_limitations_label"), ": "),
                  if (length(res$limitations)) paste(res$limitations, collapse = "; ") else "—")
        ),
        if ("BUSINESS_GEOGRAPHY_CUSTOMER_LOCATION_ONLY" %in% codes) {
          tags$p(id = "ynow_bblab_geo_veto_why", class = "help-block", ui_msg("bblab_geo_veto_why"))
        } else NULL,
        if ("BUSINESS_OVERLAPPING_DIMENSIONS_BLOCKED" %in% codes) {
          tags$p(id = "ynow_bblab_overlap_why", class = "help-block", ui_msg("bblab_overlap_why"))
        } else NULL,
        if ("no_multi_business_split" %in% (res$limitations %||% character(0))) {
          tags$p(class = "help-block", ui_msg("bblab_single_business_note"))
        } else NULL
      )
    })

    .structure_analysis_sa <- function() {
      res <- lab_result()
      if (is.null(res)) return(NULL)
      sa <- res$structure_analysis
      if (is.null(sa) && exists("bblab_build_structure_analysis", mode = "function")) {
        sa <- bblab_build_structure_analysis(res, locale = loc())
      }
      sa
    }

    output$structure_panel <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      .bblab_structure_panel_html(.structure_analysis_sa(), loc())
    })

    output$structure_conclusions <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(NULL)
      .bblab_structure_conclusions_html(.structure_analysis_sa(), loc())
    })

    .bcg_analysis <- function() {
      sa <- .structure_analysis_sa()
      res <- lab_result()
      hist <- if (!is.null(res)) res$history else NULL
      if (!exists("bblab_build_bcg_analysis", mode = "function")) return(NULL)
      bblab_build_bcg_analysis(sa, history = hist)
    }

    .bcg_plot_labels <- function() {
      list(
        x_axis = ui_msg("bblab_bcg_xaxis"),
        y_axis = ui_msg("bblab_bcg_yaxis"),
        x_axis_short = ui_msg("bblab_bcg_xaxis_short"),
        y_axis_short = ui_msg("bblab_bcg_yaxis_short"),
        quadrant = ui_msg("bblab_bcg_col_quadrant"),
        star = ui_msg("bblab_bcg_q_star"),
        cash_cow = ui_msg("bblab_bcg_q_cash_cow"),
        question_mark = ui_msg("bblab_bcg_q_question_mark"),
        dog = ui_msg("bblab_bcg_q_dog")
      )
    }

    output$bcg_panel <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(NULL)
      sa <- .structure_analysis_sa()
      if (is.null(sa) || !isTRUE(sa$ok) || !length(sa$businesses)) return(NULL)
      bcg <- .bcg_analysis()
      if (is.null(bcg) || !isTRUE(bcg$ok)) return(NULL)

      q_name <- function(q) {
        switch(
          as.character(q %||% "")[1],
          star = ui_msg("bblab_bcg_q_star"),
          cash_cow = ui_msg("bblab_bcg_q_cash_cow"),
          question_mark = ui_msg("bblab_bcg_q_question_mark"),
          dog = ui_msg("bblab_bcg_q_dog"),
          "—"
        )
      }

      body <- if (isTRUE(bcg$eligible) && length(bcg$points)) {
        rows <- lapply(bcg$points, function(p) {
          q <- as.character(p$quadrant %||% "")[1]
          tags$tr(
            tags$td(p$business),
            tags$td(sprintf("%.2f×", p$relative_share)),
            tags$td(
              if (is.finite(p$growth)) sprintf("%.1f%%", p$growth * 100) else "—"
            ),
            tags$td(
              class = paste0("ynow-bblab-bcg__q-", if (nzchar(q)) q else "dog"),
              q_name(q)
            )
          )
        })
        tags$div(
          class = "ynow-bblab-bcg__grid",
          tags$div(
            class = "ynow-bblab-bcg__chart",
            plotly::plotlyOutput(ns("bcg_plot"), height = "420px")
          ),
          tags$div(
            class = "ynow-bblab-bcg__list",
            tags$table(
              class = "table table-condensed ynow-bblab-bcg__table",
              tags$thead(tags$tr(
                tags$th(ui_msg("bblab_bcg_col_business")),
                tags$th(ui_msg("bblab_bcg_col_rel_share")),
                tags$th(ui_msg("bblab_bcg_col_growth")),
                tags$th(ui_msg("bblab_bcg_col_quadrant"))
              )),
              tags$tbody(rows)
            ),
            tags$p(
              class = "help-block",
              sprintf(
                ui_msg("bblab_bcg_midline"),
                sprintf("%.2f", bcg$x_mid),
                sprintf("%.1f%%", bcg$y_mid * 100)
              )
            )
          )
        )
      } else {
        tags$p(class = "help-block", ui_msg("bblab_bcg_need_history"))
      }

      tags$div(
        class = "ynow-bblab-bcg",
        tags$h4(
          id = "ynow_bblab_bcg_title",
          class = "ynow-bblab-subhead",
          ui_msg("bblab_bcg_title")
        ),
        tags$p(
          id = "ynow_bblab_bcg_help",
          class = "help-block",
          ui_msg("bblab_bcg_help")
        ),
        body
      )
    })

    output$bcg_plot <- plotly::renderPlotly({
      empty <- plotly::layout(
        plotly::plot_ly(type = "scatter", mode = "markers"),
        showlegend = FALSE
      )
      bcg <- .bcg_analysis()
      if (is.null(bcg) || !isTRUE(bcg$eligible)) return(empty)
      if (!exists("bblab_bcg_plotly", mode = "function")) return(empty)
      bblab_bcg_plotly(bcg, labels = .bcg_plot_labels())
    })

    output$chart_status <- renderUI({
      res <- lab_result()
      if (is.null(res) || isTRUE(res$chart$eligible)) return(NULL)
      tags$p(class = "help-block", paste(res$chart$codes, collapse = "; "))
    })

    output$donut <- plotly::renderPlotly({
      res <- lab_result()
      empty <- plotly::layout(plotly::plot_ly(type = "pie"), showlegend = FALSE, title = NULL)
      if (is.null(res) || !isTRUE(res$chart$eligible) || !length(res$chart$slices)) {
        return(empty)
      }
      slices <- res$chart$slices
      if (isTRUE(input$expand_other) && !is.null(res$other$members)) {
        slices <- Filter(function(s) !identical(s$id, "other_businesses") && !isTRUE(s$display_group), slices)
        for (m in res$other$members) {
          slices[[length(slices) + 1L]] <- list(
            id = m$id, name = m$name, revenue = m$revenue,
            share = m$revenue_pct, classification = m$classification %||% "OTHER",
            confidence = m$revenue_evidence$confidence %||% "MEDIUM",
            period = res$period, currency = res$statement_currency
          )
        }
      }
      labels <- vapply(slices, function(s) s$name, character(1))
      values <- vapply(slices, function(s) {
        if (identical(input$chart_mode, "amount")) .bblab_num(s$revenue, 0) else {
          sh <- .bblab_num(s$share, 0)
          if (is.finite(sh)) abs(sh) else 0
        }
      }, numeric(1))
      pal <- c("#0C5484", "#1AA8B8", "#249C60", "#E8A838", "#8E6BB5", "#D96B5F")
      col <- vapply(seq_along(slices), function(i) {
        if (.bblab_neutral_class(slices[[i]]$classification)) "#9AA3AB"
        else pal[((i - 1L) %% length(pal)) + 1L]
      }, character(1))
      hover <- vapply(slices, function(s) {
        paste0(
          s$name, "<br>Revenue: ", .bblab_fmt_amt(s$revenue),
          "<br>Share: ", .bblab_fmt_pct(s$share),
          "<br>", s$classification, " · ", s$confidence %||% "",
          "<br>", res$period %||% "", " · ", res$statement_currency %||% "",
          "<br>Recon: ", if (isTRUE(res$reconciliation$pass)) "pass" else "fail"
        )
      }, character(1))
      # Outside labels = percent only; full names stay in legend / hover (narrow side column).
      fig <- plotly::plot_ly(
        labels = labels, values = pmax(values, 0), type = "pie", hole = 0.45,
        customdata = vapply(slices, function(s) s$id, character(1)),
        textinfo = "percent",
        textposition = "outside",
        hovertext = hover, hoverinfo = "text",
        marker = list(colors = col, line = list(color = "#ffffff", width = 1)),
        domain = list(x = c(0.15, 0.85), y = c(0.1, 0.85)),
        source = ns("donut")
      )
      if (!is.null(res$chart$eliminations_legend)) {
        el <- res$chart$eliminations_legend
        fig <- plotly::add_annotations(
          fig,
          text = paste0(el$name, ": ", .bblab_fmt_amt(el$revenue), " (not a pie slice)"),
          x = 0.5, y = -0.12, showarrow = FALSE, xref = "paper", yref = "paper"
        )
      }
      plotly::layout(
        fig,
        showlegend = TRUE,
        margin = list(l = 70, r = 70, t = 50, b = 60),
        legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center"),
        uniformtext = list(minsize = 10, mode = "hide")
      )
    })

    output$history_panel <- renderUI({
      res <- lab_result()
      if (!isTRUE(.bblab_history_visible(res))) return(NULL)
      tagList(
        tags$h4(
          class = "ynow-bblab-subhead",
          id = "ynow_bblab_ch4_title",
          ui_msg("bblab_ch4_title")
        ),
        tags$p(
          id = "ynow_bblab_ch4_help",
          class = "help-block",
          ui_msg("bblab_ch4_help")
        ),
        plotly::plotlyOutput(ns("history"), height = "420px")
      )
    })

    output$history <- plotly::renderPlotly({
      empty <- plotly::layout(
        plotly::plot_ly(type = "bar"),
        showlegend = FALSE, title = NULL,
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      )
      res <- lab_result()
      hist <- res$history
      if (!isTRUE(.bblab_history_visible(res))) {
        return(empty)
      }
      years <- as.character(hist$years)
      pal <- c("#0C5484", "#1AA8B8", "#249C60", "#E8A838", "#8E6BB5", "#D96B5F")
      fig <- plotly::plot_ly()
      for (i in seq_along(hist$series)) {
        s <- hist$series[[i]]
        y <- vapply(years, function(ys) {
          v <- s$shares[[ys]]
          if (.bblab_finite(v) && v > 0) as.numeric(v)[1] * 100 else NA_real_
        }, numeric(1))
        col <- if (.bblab_neutral_class(s$classification)) "#9AA3AB"
        else pal[((i - 1L) %% length(pal)) + 1L]
        hover <- vapply(seq_along(years), function(j) {
          sh <- s$shares[[years[[j]]]]
          rv <- s$revenues[[years[[j]]]]
          paste0(
            s$name, "<br>", years[[j]],
            "<br>Share: ", .bblab_fmt_pct(sh),
            "<br>Revenue: ", .bblab_fmt_amt(rv),
            "<br>Denom: ", .bblab_fmt_amt(hist$denominator[[years[[j]]]])
          )
        }, character(1))
        fig <- plotly::add_trace(
          fig,
          x = years, y = y, name = s$name, type = "bar",
          marker = list(color = col),
          hovertext = hover, hoverinfo = "text"
        )
      }
      plotly::layout(
        fig,
        barmode = "stack",
        showlegend = TRUE,
        xaxis = list(title = "", type = "category"),
        yaxis = list(
          title = ui_msg("bblab_history_yaxis"),
          range = c(0, 100), ticksuffix = "%"
        ),
        margin = list(b = 40)
      )
    })

    .orders_pack <- function(which) {
      res <- lab_result()
      oc <- res$order_concentration
      if (is.null(oc) || !is.list(oc)) return(NULL)
      oc[[which]]
    }

    .orders_empty_msg <- function(pack, fallback_key) {
      codes <- as.character(pack$codes %||% character(0))
      if (!length(codes) || !nzchar(codes[[1]])) return(ui_msg(fallback_key))
      key <- paste0("notif_bblab_", tolower(codes[[1]]))
      mapped <- ui_msg(key)
      if (identical(mapped, key)) ui_msg(fallback_key) else mapped
    }

    .orders_bar_plotly <- function(pack, empty_title = NULL) {
      empty <- plotly::layout(
        plotly::plot_ly(type = "bar"),
        showlegend = FALSE, title = NULL,
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
      )
      rows <- pack$rows %||% pack$slices %||% list()
      if (!isTRUE(pack$eligible) || !length(rows)) return(empty)
      labels <- vapply(rows, function(r) .bblab_chr(r$name, "—"), character(1))
      shares <- vapply(rows, function(r) {
        s <- suppressWarnings(as.numeric(r$share)[1])
        if (is.finite(s)) {
          if (abs(s) <= 1.0000001) s * 100 else s
        } else {
          NA_real_
        }
      }, numeric(1))
      keep <- is.finite(shares) & shares > 0
      if (!any(keep)) return(empty)
      labels <- labels[keep]
      shares <- shares[keep]
      rows <- rows[keep]
      pal <- c("#0C5484", "#1AA8B8", "#249C60", "#E8A838", "#8E6BB5", "#D96B5F")
      cols <- pal[((seq_along(labels) - 1L) %% length(pal)) + 1L]
      hover <- vapply(seq_along(rows), function(i) {
        r <- rows[[i]]
        paste0(
          r$name,
          "<br>", ui_msg("bblab_kpi_revenue"), ": ", .bblab_fmt_amt(r$revenue),
          "<br>", ui_msg("bblab_orders_share_label"), ": ", .bblab_fmt_pct(r$share),
          if (nzchar(.bblab_chr(r$status))) paste0("<br>", r$status) else "",
          if (nzchar(.bblab_chr(pack$period))) paste0("<br>", pack$period) else "",
          if (nzchar(.bblab_chr(pack$currency))) paste0(" · ", pack$currency) else ""
        )
      }, character(1))
      # Horizontal bars: largest at top.
      ord <- order(shares, decreasing = FALSE)
      fig <- plotly::plot_ly(
        x = shares[ord],
        y = labels[ord],
        type = "bar",
        orientation = "h",
        marker = list(color = cols[ord]),
        hovertext = hover[ord],
        hoverinfo = "text"
      )
      plotly::layout(
        fig,
        showlegend = FALSE,
        xaxis = list(
          title = ui_msg("bblab_orders_share_axis"),
          ticksuffix = "%",
          rangemode = "tozero"
        ),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 20, r = 20, t = 10, b = 40)
      )
    }

    .orders_donut_plotly <- function(pack) {
      empty <- plotly::layout(plotly::plot_ly(type = "pie"), showlegend = FALSE, title = NULL)
      slices <- pack$slices %||% pack$rows %||% list()
      if (!isTRUE(pack$eligible) || !length(slices)) return(empty)
      labels <- vapply(slices, function(s) .bblab_chr(s$name, "—"), character(1))
      values <- vapply(slices, function(s) {
        sh <- suppressWarnings(as.numeric(s$share)[1])
        if (is.finite(sh) && sh > 0) {
          if (abs(sh) <= 1.0000001) abs(sh) else abs(sh) / 100
        } else {
          rv <- suppressWarnings(as.numeric(s$revenue)[1])
          if (is.finite(rv) && rv > 0) rv else 0
        }
      }, numeric(1))
      if (!any(values > 0)) return(empty)
      pal <- c("#0C5484", "#1AA8B8", "#249C60", "#E8A838", "#8E6BB5", "#D96B5F")
      hover <- vapply(slices, function(s) {
        paste0(
          s$name,
          "<br>", ui_msg("bblab_kpi_revenue"), ": ", .bblab_fmt_amt(s$revenue),
          "<br>", ui_msg("bblab_orders_share_label"), ": ", .bblab_fmt_pct(s$share)
        )
      }, character(1))
      fig <- plotly::plot_ly(
        labels = labels, values = pmax(values, 0), type = "pie", hole = 0.42,
        textinfo = "percent", textposition = "outside",
        hovertext = hover, hoverinfo = "text",
        marker = list(
          colors = pal[((seq_along(labels) - 1L) %% length(pal)) + 1L],
          line = list(color = "#ffffff", width = 1)
        ),
        domain = list(x = c(0.12, 0.88), y = c(0.08, 0.88))
      )
      plotly::layout(
        fig,
        showlegend = TRUE,
        margin = list(l = 40, r = 40, t = 30, b = 40),
        legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center"),
        uniformtext = list(minsize = 10, mode = "hide")
      )
    }

    output$orders_markets_status <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      pack <- .orders_pack("market_geography")
      if (is.null(pack)) {
        return(tags$p(class = "help-block", ui_msg("bblab_orders_markets_empty")))
      }
      if (isTRUE(pack$eligible)) {
        note <- if (isTRUE(pack$is_customer_location_only)) {
          ui_msg("bblab_orders_markets_geo_note")
        } else {
          NULL
        }
        return(tags$p(class = "ynow-bblab-orders__meta help-block", note))
      }
      tags$p(class = "help-block", .orders_empty_msg(pack, "bblab_orders_markets_empty"))
    })

    output$orders_customers_status <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      pack <- .orders_pack("customer_concentration")
      if (is.null(pack) || !isTRUE(pack$eligible)) {
        return(tags$p(
          class = "help-block",
          .orders_empty_msg(pack %||% list(), "bblab_orders_customers_empty")
        ))
      }
      meta <- character(0)
      if (.bblab_finite(pack$top1_share)) {
        meta <- c(meta, paste0(ui_msg("bblab_orders_top1"), ": ", .bblab_fmt_pct(pack$top1_share)))
      }
      if (.bblab_finite(pack$top3_share)) {
        meta <- c(meta, paste0(ui_msg("bblab_orders_top3"), ": ", .bblab_fmt_pct(pack$top3_share)))
      }
      if (!length(meta)) return(NULL)
      tags$p(class = "ynow-bblab-orders__meta help-block", paste(meta, collapse = " · "))
    })

    output$orders_suppliers_status <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      pack <- .orders_pack("supplier_concentration")
      if (is.null(pack) || !isTRUE(pack$eligible)) {
        return(tags$p(
          class = "help-block",
          .orders_empty_msg(pack %||% list(), "bblab_orders_suppliers_empty")
        ))
      }
      meta <- character(0)
      if (.bblab_finite(pack$top1_share)) {
        meta <- c(meta, paste0(ui_msg("bblab_orders_top1"), ": ", .bblab_fmt_pct(pack$top1_share)))
      }
      if (.bblab_finite(pack$top3_share)) {
        meta <- c(meta, paste0(ui_msg("bblab_orders_top3"), ": ", .bblab_fmt_pct(pack$top3_share)))
      }
      if (!length(meta)) return(NULL)
      tags$p(class = "ynow-bblab-orders__meta help-block", paste(meta, collapse = " · "))
    })

    output$orders_markets_chart <- plotly::renderPlotly({
      pack <- .orders_pack("market_geography")
      if (is.null(pack)) {
        return(plotly::layout(plotly::plot_ly(type = "pie"), showlegend = FALSE))
      }
      .orders_donut_plotly(pack)
    })

    output$orders_customers_chart <- plotly::renderPlotly({
      pack <- .orders_pack("customer_concentration")
      if (is.null(pack)) {
        return(plotly::layout(
          plotly::plot_ly(type = "bar"),
          showlegend = FALSE, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        ))
      }
      .orders_bar_plotly(pack)
    })

    output$orders_suppliers_chart <- plotly::renderPlotly({
      pack <- .orders_pack("supplier_concentration")
      if (is.null(pack)) {
        return(plotly::layout(
          plotly::plot_ly(type = "bar"),
          showlegend = FALSE, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        ))
      }
      .orders_bar_plotly(pack)
    })

    .or_pack <- function() {
      res <- lab_result()
      if (!is.list(res)) return(NULL)
      res$order_research
    }

    observeEvent(input$order_research_run, {
      or_started(TRUE)
    }, ignoreInit = TRUE)

    output$order_research_panel <- renderUI({
      bblab_order_research_panel_ui(.or_pack(), loc(), isTRUE(or_started()))
    })

    output$order_research_chart <- plotly::renderPlotly({
      bblab_order_research_bar(.or_pack(), loc(), "orders", isTRUE(or_started()))
    })

    output$order_research_rates_chart <- plotly::renderPlotly({
      bblab_order_research_bar(.or_pack(), loc(), "customers", isTRUE(or_started()))
    })

    observeEvent(plotly::event_data("plotly_click", source = ns("donut")), {
      ev <- plotly::event_data("plotly_click", source = ns("donut"))
      if (!is.null(ev) && !is.null(ev$customdata)) focus_id(as.character(ev$customdata)[1])
    }, ignoreNULL = TRUE)

    output$cards <- renderUI({
      res <- lab_result()
      if (is.null(res)) return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      # Missing revaluation never withholds reported / derived Rev, CoR, GP cards.
      biz <- res$businesses %||% list()
      cons_rev <- .bblab_num(res$consolidated$revenue)
      cards <- lapply(biz, function(c) {
        column(width = 4, .bblab_card_html(c, loc(), identical(focus_id(), c$id), cons_rev))
      })
      extra <- list()
      if (!is.null(res$other)) {
        extra[[length(extra) + 1L]] <- column(
          width = 4, .bblab_card_html(res$other, loc(), FALSE, cons_rev)
        )
      }
      if (!length(cards) && !length(extra)) {
        return(tags$p(class = "help-block", ui_msg("bblab_cards_empty")))
      }
      tagList(fluidRow(cards), if (length(extra)) fluidRow(extra) else NULL)
    })

    output$recon <- renderUI({
      res <- lab_result()
      if (is.null(res) || is.null(res$reconciliation)) {
        return(tags$p(class = "help-block", ui_msg("bblab_waiting")))
      }
      .bblab_recon_commercial_html(res$reconciliation, locale = loc(), ui_msg = ui_msg)
    })

    output$shared <- renderUI({
      res <- lab_result()
      items <- res$shared_corporate %||% list()
      if (!length(items)) return(tags$p(class = "help-block", ui_msg("bblab_shared_empty")))
      tags$ul(lapply(names(items), function(nm) tags$li(tags$b(nm), ": ", .bblab_fmt_amt(items[[nm]]))))
    })

    output$notes <- renderUI({
      res <- lab_result()
      body <- tagList(
        tags$p(ui_msg("bblab_notes_body")),
        if (!is.null(res) && length(res$codes)) tags$p(paste("Codes:", paste(res$codes, collapse = ", "))) else NULL,
        if (!is.null(res) && "rev_share_cost" %in% (res$cost_notices %||% character(0))) {
          tags$p(ui_msg("bblab_rev_share_cost_notice"))
        } else NULL
      )
      if (exists("ynow_notes_block", mode = "function")) {
        ynow_notes_block(body, locale = loc())
      } else body
    })

    .bblab_deny_guest_download <- function(file) {
      deny <- tryCatch(session$userData$ynow_deny_guest_download, error = function(e) NULL)
      if (!is.function(deny)) {
        root <- tryCatch(session$rootScope(), error = function(e) NULL)
        deny <- tryCatch(root$userData$ynow_deny_guest_download, error = function(e) NULL)
      }
      if (is.function(deny)) return(isTRUE(deny(file)))
      FALSE
    }

    output$export_data <- downloadHandler(
      filename = function() {
        paste0("business_breakdown_", lab_ticker() %||% "lab", "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        if (isTRUE(.bblab_deny_guest_download(file))) return()
        res <- lab_result()
        rows <- lapply(res$businesses %||% list(), function(c) {
          data.frame(
            id = c$id, name = c$name, classification = c$classification,
            revenue = c$revenue %||% NA_real_, cor = c$cor %||% NA_real_,
            gp = c$gp %||% NA_real_, gm = c$gm %||% NA_real_,
            status_rev = c$revenue_evidence$status %||% NA_character_,
            status_cor = c$cor_evidence$status %||% NA_character_,
            stringsAsFactors = FALSE
          )
        })
        df <- if (length(rows)) do.call(rbind, rows) else data.frame()
        utils::write.csv(df, file, row.names = FALSE)
      }
    )
    output$export_chart <- downloadHandler(
      filename = function() paste0("business_breakdown_chart_", Sys.Date(), ".html"),
      content = function(file) {
        if (isTRUE(.bblab_deny_guest_download(file))) return()
        res <- lab_result()
        if (is.null(res) || !isTRUE(res$chart$eligible)) {
          writeLines("<html><body>Chart not eligible</body></html>", file)
          return()
        }
        htmlwidgets::saveWidget(plotly::last_plot(), file, selfcontained = TRUE)
      }
    )
  })
}
