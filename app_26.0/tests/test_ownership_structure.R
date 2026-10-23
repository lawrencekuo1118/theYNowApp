# Offline tests for Ownership Structure Analysis (Company tab).
# Run: cd app_26.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_ownership_structure.R

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
  "own_holders_title", "own_detail_title", "own_cross_unknown",
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
check("no Mainland 点击 in own strings", {
  zh_blob <- paste(vapply(need_keys, function(k) ui_str(k, "zh-TW"), character(1)), collapse = " ")
  !grepl("点击|默认|数据|用户", zh_blob)
})

# --- Tab mount / wiring ---
ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
gl <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("global sources ownership_structure", grepl('source("ownership_structure.R"', gl, fixed = TRUE))
check("ui ownership tab value", grepl('value = "ownership_structure"', ui, fixed = TRUE))
dash_pos <- regexpr("tabItem(tabName = \"dashboard\"", ui, fixed = TRUE)[1]
dash_chunk <- if (dash_pos > 0) substr(ui, dash_pos, min(nchar(ui), dash_pos + 25000L)) else ui
pos_own <- regexpr("id = \"dashboard_ownership\"", dash_chunk, fixed = TRUE)[1]
pos_ind <- regexpr("class = \"ynow-ind-overview-block\"", dash_chunk, fixed = TRUE)[1]
pos_fin <- regexpr("title = \"FINANCIAL REPORT\"", dash_chunk, fixed = TRUE)[1]
own_block <- if (pos_own > 0) {
  substr(dash_chunk, max(1L, pos_own - 120L), min(nchar(dash_chunk), pos_own + 350L))
} else {
  ""
}
check(
  "ui ownership outside full-only before ind overview",
  is.finite(pos_own) && pos_own > 0 &&
    is.finite(pos_ind) && pos_own < pos_ind &&
    is.finite(pos_fin) && pos_fin > 0 && pos_fin < pos_own &&
    grepl("tabBox(", own_block, fixed = TRUE) &&
    !grepl("class = \"ynow-full-only\"", own_block, fixed = TRUE)
)
check("ui OWNERSHIP STRUCTURE box", grepl("OWNERSHIP STRUCTURE", ui, fixed = TRUE))
check("server ownership top ui", grepl("output\\$ownership_structure_top_ui", srv))
check("server ownership holders ui", grepl("output\\$ownership_structure_holders_ui", srv))
check("server own_volume_plot", grepl("output\\$own_volume_plot", srv))
check("ui static own_volume_plot", grepl("own_volume_plot", ui, fixed = TRUE))
check("locale tab map ownership", grepl('"ownership_structure" = "tab_ownership_structure"', loc, fixed = TRUE))
check("locale box map OWNERSHIP", grepl('"OWNERSHIP STRUCTURE" = "box_ownership_structure"', loc, fixed = TRUE))

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
  insiders_pct = 0.05,
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
check("yahoo holders n=3", is.data.frame(y$holders) && nrow(y$holders) == 3L)
check("yahoo vanguard clickable", {
  i <- match("Vanguard Group Inc", y$holders$name)
  isTRUE(y$holders$clickable[i])
})
check("yahoo cook not clickable", {
  i <- match("Cook Timothy", y$holders$name)
  !isTRUE(y$holders$clickable[i])
})

# Detail honest empty
d <- ownership_institution_detail("Vanguard Group Inc", subject_ticker = "AAPL", subject_holders = y$holders)
check("detail name set", identical(d$name, "Vanguard Group Inc"))
check("detail top5 empty without ticker map", is.data.frame(d$top5) && nrow(d$top5) == 0L)
check("detail cross unknown default", identical(d$cross_hold$status, "unknown") || identical(d$cross_hold$status, "unknown"))

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
} else {
  message("SKIP empty ui (shiny/htmltools not installed)")
}

# py helper present in deep_scraper
py <- paste(readLines("deep_scraper.py", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("py get_ownership_quote", grepl("def get_ownership_quote", py, fixed = TRUE))

# deep_scraper sourced in web_crawler path is out of scope offline

if (fail > 0L) {
  quit(status = 1L)
}
message("All ownership_structure offline checks passed.")
