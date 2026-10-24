# Offline tests for Ownership Structure Analysis (Company tab).
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_ownership_structure.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

root <- if (file.exists("ui_locale.R")) {
  normalizePath("..")
} else if (file.exists("../ui_locale.R")) {
  normalizePath("..")
} else {
  normalizePath("../..")
}
setwd(root)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    message("OK  ", label)
  } else {
    fail <<- fail + 1L
    message("FAIL ", label)
  }
}

source("ui_locale.R", local = TRUE)
source("corp_events.R", local = TRUE)
source("ownership_structure.R", local = TRUE)

# --- Locale keys (en + zh-TW) ---
need_keys <- c(
  "tab_ownership_structure", "own_intro", "own_waiting",
  "own_kpi_structure", "own_kpi_flow", "own_volume_title",
  "own_holders_title", "own_holders_title_float", "own_holders_title_insider",
  "own_detail_title", "own_cross_unknown",
  "own_slice_institutions_float", "own_note_float_yahoo", "own_note_float_yahoo_adr",
  "own_detail_intro_mapped", "own_detail_intro_tw_mapped",
  "own_detail_intro_private_mgr", "own_detail_intro_tw_gov",
  "box_ownership_structure"
)
for (k in need_keys) {
  en <- tryCatch(ui_str(k, "en"), error = function(e) "")
  zh <- tryCatch(ui_str(k, "zh-TW"), error = function(e) "")
  check(paste("locale en", k), nzchar(en) && !identical(en, k))
  check(paste("locale zh", k), nzchar(zh) && !identical(zh, k))
}
check("tab en Ownership", grepl("Ownership", ui_str("tab_ownership_structure", "en"), fixed = TRUE))
check("tab zh 股權結構", grepl("股權結構", ui_str("tab_ownership_structure", "zh-TW"), fixed = TRUE))
check("float title en free-float", grepl("free-float", ui_str("own_holders_title_float", "en"), fixed = TRUE))
check("float title zh 流通股", grepl("流通股", ui_str("own_holders_title_float", "zh-TW"), fixed = TRUE))
check("no Mainland 点击 in own strings", {
  zh_blob <- paste(vapply(need_keys, function(k) ui_str(k, "zh-TW"), character(1)), collapse = " ")
  !grepl("点击|默认|数据|用户|口徑|點擊", zh_blob)
})

# --- Tab mount / wiring ---
ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
gl <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("global sources ownership_structure", grepl('source("ownership_structure.R"', gl, fixed = TRUE))
check("ui no standalone ownership tabBox", !grepl('id = "dashboard_ownership"', ui, fixed = TRUE))
check("ui ownership panel under Board/CB", grepl('id = "ownership_structure_panel"', ui, fixed = TRUE))
dash_pos <- regexpr("tabItem(tabName = \"dashboard\"", ui, fixed = TRUE)[1]
dash_chunk <- if (dash_pos > 0) substr(ui, dash_pos, min(nchar(ui), dash_pos + 25000L)) else ui
pos_corp <- regexpr('value = "corp_events"', dash_chunk, fixed = TRUE)[1]
pos_own <- regexpr('id = "ownership_structure_panel"', dash_chunk, fixed = TRUE)[1]
pos_own_out <- regexpr("ownership_structure_top_ui", dash_chunk, fixed = TRUE)[1]
pos_vol <- regexpr("own_volume_plot", dash_chunk, fixed = TRUE)[1]
# Board / CB tab chunk: from corp_events value to next tabPanel (sec_notes)
corp_end <- regexpr('value = "sec_notes"', dash_chunk, fixed = TRUE)[1]
corp_chunk <- if (pos_corp > 0 && corp_end > pos_corp) {
  substr(dash_chunk, pos_corp, corp_end)
} else {
  ""
}
check(
  "ui ownership inside Board/CB after corp_events_ui",
  is.finite(pos_corp) && pos_corp > 0 &&
    is.finite(pos_own) && pos_own > pos_corp &&
    grepl("corp_events_ui", corp_chunk, fixed = TRUE) &&
    grepl("ownership_structure_top_ui", corp_chunk, fixed = TRUE) &&
    grepl("own_volume_plot", corp_chunk, fixed = TRUE) &&
    grepl("ownership_structure_holders_ui", corp_chunk, fixed = TRUE) &&
    grepl("ynow-ownership-under-cb", corp_chunk, fixed = TRUE) &&
    # ownership block must come after corp_events_ui in the same tab
    regexpr("corp_events_ui", corp_chunk, fixed = TRUE)[1] <
      regexpr("ynow-ownership-under-cb", corp_chunk, fixed = TRUE)[1]
)
check(
  "ui fin-report wrap not full-only (Lite keeps Board/CB)",
  grepl("ynow-dashboard-fin-report-wrap", ui, fixed = TRUE) &&
    grepl("ensureLiteCorpEventsTab", ui, fixed = TRUE) &&
    grepl('data-value="corp_events"', ui, fixed = TRUE)
)
check("server ownership top ui", grepl("output\\$ownership_structure_top_ui", srv))
check("server ownership holders ui", grepl("output\\$ownership_structure_holders_ui", srv))
check("server own_volume_plot", grepl("output\\$own_volume_plot", srv))
check("ui static own_volume_plot", grepl("own_volume_plot", ui, fixed = TRUE))
check("locale tab map ownership", grepl('"ownership_structure" = "tab_ownership_structure"', loc, fixed = TRUE))
check("locale box map OWNERSHIP", grepl('"OWNERSHIP STRUCTURE" = "box_ownership_structure"', loc, fixed = TRUE))
check(
  "ui ownership outputs ordered top then volume then holders",
  is.finite(pos_own_out) && is.finite(pos_vol) && pos_own_out < pos_vol
)

# Lite omit must NOT include ownership / dashboard (string scan; avoid sourcing auth stack)
omit_src <- paste(readLines("auth_account.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("lite omit fn exists", grepl("ynow_sidebar_lite_omit_tabs", omit_src, fixed = TRUE))
check(
  "lite omit source omits ownership_structure id",
  !grepl("ownership_structure", omit_src, fixed = TRUE)
)
check(
  "lite omit keeps Company dashboard free",
  grepl("company_advance", omit_src, fixed = TRUE) &&
    !grepl('"dashboard"', omit_src, fixed = TRUE)
)

# --- Pure helpers ---
check("institution name bank", .ownership_is_institution_name("Vanguard Group Inc", "institution"))
check("individual not institution", !.ownership_is_institution_name("Cook Timothy", "insider"))
check("TW 法人 bank", .ownership_is_institution_name("行政院國家發展基金管理會", ""))
check("fmt pct fraction", identical(.ownership_fmt_pct(0.738), "73.8%"))
check("fmt shares M", grepl("M", .ownership_fmt_shares(12500000)))

# Yahoo normalize from fixture (no network)
raw <- list(
  ok = TRUE,
  error = "",
  institutions_pct = 0.61,
  institutions_float_pct = 0.72,
  insiders_pct = 0.05,
  float_shares = 1.5e10,
  shares_outstanding = 1.6e10,
  holder_name = list("Vanguard Group Inc", "BlackRock Inc", "Cook Timothy"),
  holder_pct = list(0.08, 0.07, 0.01),
  holder_shares = list(1e9, 9e8, 1e7),
  holder_value = list(NA, NA, NA),
  holder_report_date = list("2024-06-30", "2024-06-30", "2024-05-01"),
  holder_kind = list("institution", "institution", "insider"),
  insider_net_shares = -1000,
  insider_txn_count = 2L
)
y <- .ownership_from_yahoo(raw, "AAPL")
check("yahoo ok", isTRUE(y$ok))
check("yahoo structure has institutions", {
  keys <- vapply(y$structure, function(s) s$key, character(1))
  "institutions" %in% keys
})
check("yahoo structure has institutions_float", {
  keys <- vapply(y$structure, function(s) s$key, character(1))
  "institutions_float" %in% keys
})
check("yahoo holders_kind free_float", identical(y$holders_kind, "free_float"))
check("yahoo holders n=3", is.data.frame(y$holders) && nrow(y$holders) == 3L)
check("yahoo has institution rows", .ownership_holders_has_institution(y$holders))
check("yahoo vanguard clickable", {
  i <- match("Vanguard Group Inc", y$holders$name)
  isTRUE(y$holders$clickable[i])
})
check("yahoo cook not clickable", {
  i <- match("Cook Timothy", y$holders$name)
  !isTRUE(y$holders$clickable[i])
})

# Institution ticker map (no network)
blk <- .ownership_resolve_institution_ticker("BlackRock Inc")
check("map BlackRock → BLK", identical(blk$ticker, "BLK") && isTRUE(blk$matched))
cathay <- .ownership_resolve_institution_ticker("國泰人壽保險股份有限公司")
check("map 國泰人壽 → 2882.TW", identical(cathay$ticker, "2882.TW") && isTRUE(cathay$matched))
vang <- .ownership_resolve_institution_ticker("Vanguard Group Inc")
check("map Vanguard private empty ticker", isTRUE(vang$matched) && !nzchar(vang$ticker))
check(
  "map Vanguard private intro",
  identical(vang$intro_key, "own_detail_intro_private_mgr")
)

# Detail: private manager honest empty top-5
d <- ownership_institution_detail("Vanguard Group Inc", subject_ticker = "AAPL", subject_holders = y$holders)
check("detail name set", identical(d$name, "Vanguard Group Inc"))
check("detail private intro", identical(d$intro_key, "own_detail_intro_private_mgr"))
check("detail top5 empty for private mgr", is.data.frame(d$top5) && nrow(d$top5) == 0L)
check("detail cross unknown when listed only", identical(d$cross_hold$status, "unknown"))

# Cross-hold match helper
ch_yes <- .ownership_cross_hold_match(
  c("Foo Corp", "Apple Inc", "Bar LLC"), "AAPL", subject_aliases = c("Apple")
)
check("cross-hold yes on alias", identical(ch_yes$status, "yes"))
ch_no <- .ownership_cross_hold_match(c("Foo Corp", "Bar LLC"), "AAPL")
check("cross-hold no when absent", identical(ch_no$status, "no"))

# Detail BlackRock maps to BLK; offline Yahoo empty → top5 empty but mapped
d_blk <- ownership_institution_detail("BlackRock Fund Advisors", subject_ticker = "AAPL")
check("detail BLK mapped ticker", identical(d_blk$mapped_ticker, "BLK"))
check("detail BLK intro mapped", identical(d_blk$intro_key, "own_detail_intro_mapped"))

# ROC date + TW NDF intro (no invented top-5)
check(
  "ROC slash date",
  identical(as.character(.ownership_roc_slash_date("115/10/01")), "2026-10-01")
)
check("STOCK_DAY url constant", exists("YNOW_TWSE_STOCK_DAY_URL") && grepl("STOCK_DAY", YNOW_TWSE_STOCK_DAY_URL))
ndf <- ownership_institution_detail(
  "行政院國家發展基金管理會",
  subject_ticker = "2330.TW",
  market = "TW"
)
check("NDF intro key", identical(ndf$intro_key, "own_detail_intro_tw_ndf"))
check("NDF top5 still empty", is.data.frame(ndf$top5) && nrow(ndf$top5) == 0L)
check("NDF credibility official", identical(ndf$credibility, "official"))
check("locale ndf en", nzchar(ui_str("own_detail_intro_tw_ndf", "en")))
check("locale ndf zh", grepl("國家發展基金", ui_str("own_detail_intro_tw_ndf", "zh-TW")))
check("locale volume twse note", nzchar(ui_str("own_note_volume_twse", "zh-TW")))
check("server uses cached ownership", grepl("cached_fetch_ownership_structure", srv, fixed = TRUE))
check("cached_fetch exists", exists("cached_fetch_ownership_structure", mode = "function") ||
        exists("fetch_ownership_structure", mode = "function"))
py <- paste(readLines("deep_scraper.py", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("py float_shares field", grepl("float_shares", py, fixed = TRUE))

# Prefer free-float holders in UI title
float_snap <- .ownership_empty_snapshot("AAPL", "US")
float_snap$holders <- y$holders
float_snap$holders_kind <- "free_float"
float_snap$notes <- c("holders_free_float_yahoo")
float_snap$ok <- TRUE

# Empty snapshot + UI (needs htmltools/shiny tags)
empty <- .ownership_empty_snapshot("", "US")
if (requireNamespace("htmltools", quietly = TRUE) || requireNamespace("shiny", quietly = TRUE)) {
  suppressPackageStartupMessages({
    if (requireNamespace("shiny", quietly = TRUE)) library(shiny)
    else library(htmltools)
  })
  ui_empty <- .ynow_ownership_panel_ui(empty, loc = "en")
  check(
    "empty ui is shiny tag",
    inherits(ui_empty, "shiny.tag") || inherits(ui_empty, "shiny.tag.list") ||
      inherits(ui_empty, "html")
  )
  ui_float <- .ynow_ownership_holders_ui(float_snap, loc = "en")
  float_html <- paste(as.character(ui_float), collapse = " ")
  check(
    "float ui title",
    grepl("free-float", float_html, fixed = TRUE) ||
      grepl(ui_str("own_holders_title_float", "en"), float_html, fixed = TRUE)
  )
  check(
    "float ui note",
    grepl("free-float", float_html, ignore.case = TRUE)
  )
} else {
  message("SKIP empty ui (shiny/htmltools not installed)")
}

# py helper present in deep_scraper
check("py get_ownership_quote", grepl("def get_ownership_quote", py, fixed = TRUE))

# ADR helper: HTCDI TSM ↔ 2330 when config available
if (file.exists("htcdi_config.R")) {
  tryCatch(source("htcdi_config.R", local = TRUE), error = function(e) NULL)
}
if (exists("htcdi_find_issuer", mode = "function")) {
  adr <- .ownership_adr_ticker_for_tw("2330.TW")
  check("ADR map 2330 → TSM", identical(adr, "TSM") || identical(adr, "TSM"))
} else {
  message("SKIP ADR map (htcdi_find_issuer unavailable)")
}

# deep_scraper sourced in web_crawler path is out of scope offline

if (fail > 0L) {
  quit(status = 1L)
}
message("All ownership_structure offline checks passed.")
