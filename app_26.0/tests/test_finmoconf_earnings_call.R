# FinmoConf TW presentation PDF → status-only order / supply / outlook facts.
# Run: cd app_26.0 && Rscript tests/test_finmoconf_earnings_call.R
Sys.setenv(
  YNOW_DEBUG_SKIP_PY = "1",
  YNOW_SA_SCRAPE = "0",
  YNOW_FUGLE_SCRAPE = "0",
  YNOW_FINMOCONF_SCRAPE = "0"
)

app_dir <- if (file.exists("finmoconf_earnings_call.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../finmoconf_earnings_call.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate finmoconf_earnings_call.R")
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
source("us_earnings_call.R", local = TRUE, encoding = "UTF-8")
source("fugle_earnings_call.R", local = TRUE, encoding = "UTF-8")
source("finmoconf_earnings_call.R", local = TRUE, encoding = "UTF-8")
source("tw_earnings_call.R", local = TRUE, encoding = "UTF-8")

html_fix <- file.path(app_dir, "tests/fixtures/finmoconf_company_8182.html")
text_fix <- file.path(app_dir, "tests/fixtures/finmoconf_presentation_sample.txt")
status_fix <- file.path(app_dir, "tests/fixtures/finmoconf_presentation_8182_status.txt")

check("company url stable", {
  identical(ynow_finmoconf_company_url("8182.TW"), "https://finmoconf.diveinvest.net/company/8182")
})
check("offline HTML prefers latest zh PDF", {
  html <- paste(readLines(html_fix, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  got <- .ynow_finmo_ec_parse_html_offline(html, "8182")
  is.list(got) &&
    identical(got$published %||% "", "2026-10-21") &&
    identical(got$lang %||% "", "zh") &&
    grepl("818220261021M001\\.pdf", got$url %||% "") &&
    grepl("doc\\.twse\\.com\\.tw", got$url %||% "")
})
check("status notes harvest supply + outlook without inventing backlog", {
  txt <- paste(readLines(status_fix, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  notes <- .ynow_finmo_ec_status_notes(txt)
  length(notes$supply_chain_notes) >= 1L &&
    length(notes$outlook_notes) >= 1L &&
    length(notes$order_notes) == 0L
})

Sys.setenv(
  YNOW_FINMOCONF_SCRAPE = "1",
  YNOW_FUGLE_SCRAPE = "0",
  YNOW_SA_SCRAPE = "0",
  YNOW_FINMOCONF_SCRAPE_FIXTURE_HTML = html_fix,
  YNOW_FINMOCONF_PDF_FIXTURE_TEXT = text_fix
)
got <- ynow_tw_earnings_call_fetch("8182.TW")
check("FinmoConf fixture fills estimate + source_id", {
  is.list(got) &&
    identical(got$credibility %||% "", "estimate") &&
    identical(got$source_id %||% "", "finmoconf_presentation_pdf") &&
    identical(got$published %||% "", "2026-10-21") &&
    isTRUE(abs((got$backlog %||% 0) - 12.5e8) < 1e5) &&
    isTRUE(abs((got$bookings %||% 0) - 3.2e8) < 1e5) &&
    length(got$supply_chain_notes %||% character(0)) >= 1L &&
    length(got$outlook_notes %||% character(0)) >= 1L &&
    grepl("finmoconf.diveinvest.net/company/8182", got$finmoconf_url %||% "", fixed = TRUE)
})
check("disclosure carries status note fields", {
  disc <- ynow_tw_earnings_call_to_disclosure(got)
  length(disc$supply_chain_notes %||% character(0)) >= 1L &&
    length(disc$outlook_notes %||% character(0)) >= 1L &&
    identical(disc$source_id %||% "", "finmoconf_presentation_pdf")
})

# Merge: FinmoConf + Fugle both on → FinmoConf remains primary source_id.
Sys.setenv(
  YNOW_FUGLE_SCRAPE = "1",
  YNOW_FUGLE_SCRAPE_FIXTURE_HTML = file.path(app_dir, "tests/fixtures/fugle_earnings_memo_sample.html")
)
merged <- ynow_tw_earnings_call_fetch("8182.TW")
check("FinmoConf primary when merged with Fugle fixture", {
  identical(merged$source_id %||% "", "finmoconf_presentation_pdf") &&
    is.finite(merged$backlog %||% NA_real_)
})

Sys.unsetenv("YNOW_FINMOCONF_SCRAPE_FIXTURE_HTML")
Sys.unsetenv("YNOW_FINMOCONF_PDF_FIXTURE_TEXT")
Sys.unsetenv("YNOW_FUGLE_SCRAPE_FIXTURE_HTML")
Sys.setenv(YNOW_FINMOCONF_SCRAPE = "0", YNOW_FUGLE_SCRAPE = "0", YNOW_SA_SCRAPE = "0")

check("disabled FinmoConf returns NULL fetch helper", {
  is.null(ynow_finmoconf_earnings_call_fetch("8182.TW"))
})
check("locale keys for FinmoConf present en+zh", {
  loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("bblab_or_tw_ec_finmoconf_link", loc, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_finmoconf_note", loc, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_orders_title", loc, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_supply_title", loc, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_outlook_title", loc, fixed = TRUE) &&
    grepl("FinmoConf 公司頁（估計）", loc, fixed = TRUE) &&
    grepl("不會呼叫 FinmoConf /api/", loc, fixed = TRUE) &&
    grepl("FinmoConf /api/ is never called", loc, fixed = TRUE)
})
check("global sources finmoconf before tw", {
  g <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  fi <- regexpr('source("finmoconf_earnings_call.R"', g, fixed = TRUE)
  ti <- regexpr('source("tw_earnings_call.R"', g, fixed = TRUE)
  fi > 0L && ti > fi
})
check("BBLab UI wires FinmoConf notes + links", {
  mod <- paste(readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("bblab_or_tw_ec_finmoconf_link", mod, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_finmoconf_note", mod, fixed = TRUE) &&
    grepl("supply_chain_notes", mod, fixed = TRUE) &&
    grepl("outlook_notes", mod, fixed = TRUE)
})
check("no /api/ calls in FinmoConf modules", {
  r <- paste(readLines("finmoconf_earnings_call.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  py <- paste(readLines("finmoconf_earnings_pdf.py", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("Disallow /api/", r, fixed = TRUE) &&
    grepl("never call /api/", r, fixed = TRUE) &&
    grepl("NEVER calls /api/", py, fixed = TRUE) &&
    !grepl("finmoconf.diveinvest.net/api/", py, fixed = TRUE) &&
    !grepl("\"/api/", py, fixed = TRUE) &&
    !grepl("'/api/", py, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
