#!/usr/bin/env Rscript
# Earnings-call catalog + multi-select merge for BBLab orders.
# Run: cd app_28.0 && Rscript tests/test_earnings_call_catalog.R
Sys.setenv(
  YNOW_DEBUG_SKIP_PY = "1",
  YNOW_SA_SCRAPE = "0",
  YNOW_FUGLE_SCRAPE = "1",
  YNOW_FINMOCONF_SCRAPE = "1",
  YNOW_FINMOCONF_PDF = "0"
)

app_dir <- if (file.exists("earnings_call_catalog.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../earnings_call_catalog.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate earnings_call_catalog.R")
}
setwd(app_dir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK  ", label)
  } else {
    bad <<- bad + 1L
    message("FAIL ", label)
  }
}

source("seeking_alpha_earnings_call.R", local = TRUE, encoding = "UTF-8")
source("earnings_call_keyword_lexicon.R", local = TRUE, encoding = "UTF-8")
source("earnings_call_keyword_framework.R", local = TRUE, encoding = "UTF-8")
source("supply_chain_order_report.R", local = TRUE, encoding = "UTF-8")
source("us_earnings_call.R", local = TRUE, encoding = "UTF-8")
source("fugle_earnings_call.R", local = TRUE, encoding = "UTF-8")
source("finmoconf_earnings_call.R", local = TRUE, encoding = "UTF-8")
source("tw_earnings_call.R", local = TRUE, encoding = "UTF-8")
source("earnings_call_catalog.R", local = TRUE, encoding = "UTF-8")

fugle_fix <- file.path(app_dir, "tests/fixtures/fugle_earnings_memo_sample.html")
finmo_fix <- file.path(app_dir, "tests/fixtures/finmoconf_company_8182.html")
finmo_txt <- file.path(app_dir, "tests/fixtures/finmoconf_presentation_8182_status.txt")
check("fixtures present", {
  file.exists(fugle_fix) && file.exists(finmo_fix) && file.exists(finmo_txt)
})

Sys.setenv(YNOW_FUGLE_SCRAPE_FIXTURE_HTML = fugle_fix)
fugle_rows <- ynow_fugle_earnings_call_catalog_rows("8182.TW", max_rows = 8L)
check("Fugle catalog lists two 8182 posts newest first", {
  length(fugle_rows) >= 2L &&
    identical(fugle_rows[[1]]$published %||% "", "2026-10-21") &&
    identical(fugle_rows[[2]]$published %||% "", "2026-03-01") &&
    !any(grepl("2330", vapply(fugle_rows, function(r) r$url %||% "", character(1)), fixed = TRUE))
})

Sys.setenv(YNOW_FINMOCONF_SCRAPE_FIXTURE_HTML = finmo_fix)
finmo_rows <- ynow_finmoconf_earnings_call_catalog_rows("8182.TW", max_rows = 8L)
check("FinmoConf catalog lists multiple presentation dates", {
  length(finmo_rows) >= 3L &&
    identical(finmo_rows[[1]]$published %||% "", "2026-10-21") &&
    grepl("818220261021", finmo_rows[[1]]$url %||% "", fixed = TRUE)
})

Sys.setenv(
  YNOW_FUGLE_SCRAPE_FIXTURE_HTML = fugle_fix,
  YNOW_FINMOCONF_SCRAPE_FIXTURE_HTML = finmo_fix
)
cat_tw <- ynow_tw_earnings_call_catalog("8182.TW", max_rows = 12L)
check("TW catalog defaults to newest id", {
  is.list(cat_tw) &&
    length(cat_tw$rows) >= 2L &&
    nzchar(cat_tw$default_id %||% "") &&
    identical(cat_tw$default_id, cat_tw$rows[[1]]$id) &&
    identical(cat_tw$market, "TW")
})

picked_default <- ynow_ec_catalog_select_rows(cat_tw, selected_ids = NULL)
check("empty selection falls back to latest", {
  length(picked_default) == 1L &&
    identical(picked_default[[1]]$id, cat_tw$default_id)
})

# Prefer two FinmoConf dates when present for multi-select.
finmo_ids <- vapply(
  Filter(function(r) identical(r$source_id, "finmoconf_presentation_pdf"), cat_tw$rows),
  function(r) r$id,
  character(1)
)
multi_ids <- utils::head(finmo_ids, 2L)
if (length(multi_ids) < 2L) {
  multi_ids <- utils::head(vapply(cat_tw$rows, function(r) r$id, character(1)), 2L)
}
picked_multi <- ynow_ec_catalog_select_rows(cat_tw, selected_ids = multi_ids)
check("multi-select returns two rows newest first", {
  length(picked_multi) == 2L &&
    all(vapply(picked_multi, function(r) r$id, character(1)) %in% multi_ids)
})

# Merge two synthetic disclosures: newest backlog wins; parties union.
a <- list(
  ok = TRUE, status = "ok", source_id = "fugle_earnings_memo",
  credibility = "estimate", title = "A", url = "u-a", published = "2026-10-21",
  backlog = 100, bookings = NA_real_, capex = NA_real_,
  themes = "cfo_guidance", suppliers = list(), customers = list(
    list(name = "台積電", share = 0.18, amount = NA_real_, kind = "customer")
  ),
  order_lines = list(), excerpt = "newer", order_notes = "新訂單",
  supply_chain_notes = character(0), outlook_notes = character(0)
)
b <- list(
  ok = TRUE, status = "ok", source_id = "finmoconf_presentation_pdf",
  credibility = "estimate", title = "B", url = "u-b", published = "2025-10-21",
  backlog = 50, bookings = 40, capex = NA_real_,
  themes = "outlook_status", suppliers = list(
    list(name = "聯發科", share = NA_real_, amount = 12e8, kind = "supplier")
  ), customers = list(), order_lines = list(), excerpt = "older",
  order_notes = character(0), supply_chain_notes = "供應鏈", outlook_notes = "展望"
)
merged <- ynow_ec_merge_results(list(a, b))
check("merge keeps newest backlog and fills older bookings", {
  is.list(merged) &&
    isTRUE(abs((merged$backlog %||% 0) - 100) < 1e-9) &&
    isTRUE(abs((merged$bookings %||% 0) - 40) < 1e-9) &&
    length(merged$customers) >= 1L &&
    length(merged$suppliers) >= 1L &&
    identical(merged$analysis_axis_published %||% "", "2026-10-21")
})

Sys.setenv(
  YNOW_FUGLE_SCRAPE_FIXTURE_HTML = fugle_fix,
  YNOW_FINMOCONF_SCRAPE_FIXTURE_HTML = finmo_fix,
  YNOW_FINMOCONF_PDF_FIXTURE_TEXT = finmo_txt
)
# Fetch selected Fugle latest via catalog row (fixture article body).
fugle_latest <- cat_tw$rows[[which(vapply(cat_tw$rows, function(r) {
  identical(r$source_id, "fugle_earnings_memo")
}, logical(1)))[1]]]
got_f <- ynow_ec_fetch_catalog_row("8182.TW", fugle_latest)
check("fetch Fugle catalog row yields usable facts from fixture", {
  is.list(got_f) && isTRUE(got_f$ok) &&
    is.finite(got_f$backlog %||% NA_real_) &&
    nzchar(got_f$excerpt %||% "")
})

disc <- ynow_ec_to_disclosure(got_f, market = "TW")
check("disclosure keeps backlog and catalog_id is not required on disclosure", {
  is.list(disc) && is.finite(disc$backlog %||% NA_real_) &&
    identical(disc$source_id %||% "", "fugle_earnings_memo")
})

# Locale keys present for UI row.
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
check("en + zh-TW transcript locale keys", {
  identical(.UI_STRINGS$en$bblab_or_transcripts_title, "Past earnings-call transcripts") &&
    identical(.UI_STRINGS$`zh-TW`$bblab_or_transcripts_title, "歷次法說會逐字稿") &&
    nzchar(.UI_STRINGS$en$bblab_or_transcripts_help) &&
    nzchar(.UI_STRINGS$`zh-TW`$bblab_or_transcripts_help) &&
    grepl("點選", .UI_STRINGS$`zh-TW`$bblab_or_transcripts_help, fixed = TRUE) &&
    !grepl("点击|點擊", .UI_STRINGS$`zh-TW`$bblab_or_transcripts_help)
})

# US catalog without FMP/SA stays empty or SEC placeholder only (no crash).
Sys.setenv(YNOW_SA_SCRAPE = "0")
cat_us <- ynow_us_earnings_call_catalog("AAPL", max_rows = 5L)
check("US catalog returns list without crash", {
  is.list(cat_us) && identical(cat_us$market, "US")
})

check("global.R sources earnings_call_catalog.R after tw_earnings_call.R", {
  g <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ti <- regexpr('source("tw_earnings_call.R"', g, fixed = TRUE)
  ci <- regexpr('source("earnings_call_catalog.R"', g, fixed = TRUE)
  ti[1] > 0 && ci[1] > ti[1]
})

check("toolbar mounts transcript uiOutput above Start", {
  m <- paste(readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("order_research_transcripts_ui", m, fixed = TRUE) &&
    grepl("order_research_transcript_ids", m, fixed = TRUE) &&
    regexpr("order_research_transcripts_ui", m, fixed = TRUE)[1] <
      regexpr('ns("order_research_run")', m, fixed = TRUE)[1]
})

# IV layout: transcript chips wrap; orders charts stay inside the pane (TYA-127).
css_widgets <- paste(
  readLines("www/ynow_widgets.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
mod_src <- paste(
  readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check("IV transcript chips flex-wrap onto next row", {
  grepl("\\.ynow-bblab-or-transcripts \\.shiny-options-group", css_widgets) &&
    grepl("flex-wrap:\\s*wrap\\s*!important", css_widgets) &&
    grepl("\\.ynow-bblab-or-transcripts \\.shiny-input-container", css_widgets) &&
    grepl("width:\\s*100%\\s*!important", css_widgets) &&
    grepl("margin-left:\\s*0\\s*!important", css_widgets) &&
    grepl("white-space:\\s*normal", css_widgets) &&
    grepl("float:\\s*none\\s*!important", css_widgets)
})
check("IV orders charts constrained (no overflow:visible)", {
  # Scope to the orders CSS block only — structure-mix charts may still use
  # overflow:visible elsewhere in the same tags$style string.
  orders_css <- regmatches(
    mod_src,
    regexpr(
      "\\.ynow-bblab-orders\\{[^`]{0,2500}?\\.ynow-bblab-or__card\\{",
      mod_src,
      perl = TRUE
    )
  )
  length(orders_css) == 1L &&
    grepl("max-width:100%", orders_css, fixed = TRUE) &&
    grepl("overflow-x:auto", orders_css, fixed = TRUE) &&
    grepl("min-width:0", orders_css, fixed = TRUE) &&
    !grepl("overflow:visible", orders_css, fixed = TRUE) &&
    grepl(
      "\\.ynow-bblab-orders__group \\.plotly[\\s\\S]{0,400}?max-width:\\s*100%",
      css_widgets,
      perl = TRUE
    ) &&
    grepl("overflow-x:\\s*auto\\s*!important", css_widgets) &&
    grepl("min-width:\\s*0", css_widgets)
})

message("")
message("PASS ", ok, "  FAIL ", bad)
if (bad > 0L) quit(status = 1L)
message("ALL PASS")
