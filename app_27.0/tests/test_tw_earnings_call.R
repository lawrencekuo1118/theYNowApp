# TW earnings-call — FinmoConf PDF + Fugle memo + AlphaMemo outbound + SA last-resort; honest empty.
# Run: cd app_27.0 && Rscript tests/test_tw_earnings_call.R
Sys.setenv(
  YNOW_DEBUG_SKIP_PY = "1",
  YNOW_SA_SCRAPE = "0",
  YNOW_FUGLE_SCRAPE = "0",
  YNOW_FINMOCONF_SCRAPE = "0"
)

app_dir <- if (file.exists("tw_earnings_call.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../tw_earnings_call.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate tw_earnings_call.R")
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

check("AlphaMemo index url stable", {
  identical(
    ynow_tw_earnings_call_alphamemo_index_url(),
    "https://www.alphamemo.ai/free-transcripts"
  )
})
check("Fugle topic url stable", {
  identical(
    ynow_fugle_earnings_call_topic_url(),
    "https://blog.fugle.tw/topic/earnings-call-memo"
  )
})
check("search url mentions alphamemo + ticker", {
  u <- ynow_tw_earnings_call_alphamemo_search_url("2330.TW")
  grepl("alphamemo", u, fixed = TRUE) && grepl("2330", u, fixed = TRUE)
})
check("zh extract backlog 億元 and capex", {
  f <- .ynow_us_ec_extract_facts("在手訂單約 12.5 億元，資本支出約 3.2 億元。")
  isTRUE(abs((f$backlog %||% 0) - 12.5e8) < 1e5) &&
    isTRUE(abs((f$capex %||% 0) - 3.2e8) < 1e5) &&
    !is.finite(f$bookings %||% NA_real_)
})
check("zh extract 百萬 萬元 and does not invent backlog from 新訂單", {
  f <- .ynow_us_ec_extract_facts("在手訂單 80 百萬。資本支出 2500 萬元。新訂單 40 億元。")
  isTRUE(abs((f$backlog %||% 0) - 80e6) < 1) &&
    isTRUE(abs((f$capex %||% 0) - 2500 * 1e4) < 1) &&
    isTRUE(abs((f$bookings %||% 0) - 40e8) < 1)
})
check("zh extract named customer supplier and themes", {
  f <- .ynow_us_ec_extract_facts(
    "主要客戶為台積電，占營收 18%。主要供應商為聯發科，採購金額 12 億元。展望與問答。"
  )
  cust <- f$customers %||% list()
  sup <- f$suppliers %||% list()
  length(cust) >= 1L &&
    grepl("台積電", cust[[1]]$name %||% "", fixed = TRUE) &&
    isTRUE(abs((cust[[1]]$share %||% 0) - 0.18) < 1e-6) &&
    length(sup) >= 1L &&
    grepl("聯發科", sup[[1]]$name %||% "", fixed = TRUE) &&
    isTRUE(abs((sup[[1]]$amount %||% 0) - 12e8) < 1) &&
    !is.finite(f$backlog %||% NA_real_) &&
    all(c("cfo_guidance", "ceo_outlook", "analyst_qa") %in% (f$themes %||% character(0)))
})

Sys.setenv(YNOW_SA_SCRAPE = "0", YNOW_FUGLE_SCRAPE = "0")
got <- ynow_tw_earnings_call_fetch("2330.TW")
check("TW fetch returns AlphaMemo + Fugle + FinmoConf links", {
  is.list(got) &&
    grepl("alphamemo.ai/free-transcripts", got$alphamemo_url %||% "", fixed = TRUE) &&
    grepl("blog.fugle.tw/topic/earnings-call-memo", got$fugle_url %||% "", fixed = TRUE) &&
    grepl("finmoconf.diveinvest.net/company/2330", got$finmoconf_url %||% "", fixed = TRUE)
})
check("TW fetch honest empty without scrapers", {
  identical(got$status %||% "", "empty") &&
    identical(got$credibility %||% "", "estimate") &&
    !is.finite(got$backlog %||% NA_real_) &&
    grepl("finmoconf_fugle_empty", got$empty_reason %||% "", fixed = TRUE)
})
check("US ticker leave TW path", {
  u <- ynow_tw_earnings_call_fetch("AAPL")
  identical(u$empty_reason %||% "", "us_leave_as_is")
})

# Fixture-driven Fugle preferred path (FinmoConf off).
Sys.setenv(
  YNOW_FUGLE_SCRAPE = "1",
  YNOW_SA_SCRAPE = "0",
  YNOW_FINMOCONF_SCRAPE = "0",
  YNOW_FUGLE_SCRAPE_FIXTURE_HTML = file.path(app_dir, "tests/fixtures/fugle_earnings_memo_sample.html")
)
fugle_got <- ynow_tw_earnings_call_fetch("8182.TW")
check("TW Fugle fixture labels estimate + fugle_earnings_memo", {
  is.list(fugle_got) &&
    identical(fugle_got$credibility %||% "", "estimate") &&
    identical(fugle_got$source_id %||% "", "fugle_earnings_memo") &&
    isTRUE(abs((fugle_got$backlog %||% 0) - 12.5e8) < 1e5) &&
    isTRUE(abs((fugle_got$capex %||% 0) - 3.2e8) < 1e5) &&
    grepl("earnings-call-8182-2026-10-21", fugle_got$url %||% "", fixed = TRUE) &&
    identical(fugle_got$published %||% "", "2026-10-21") &&
    grepl("alphamemo.ai/free-transcripts", fugle_got$alphamemo_url %||% "", fixed = TRUE) &&
    grepl("blog.fugle.tw/topic/earnings-call-memo", fugle_got$fugle_url %||% "", fixed = TRUE)
})
check("TW Fugle fixture disclosure carries parties", {
  disc_f <- ynow_tw_earnings_call_to_disclosure(fugle_got)
  cust <- disc_f$customers %||% list()
  length(cust) >= 1L &&
    grepl("台積電", cust[[1]]$name %||% "", fixed = TRUE) &&
    identical(disc_f$source_id %||% "", "fugle_earnings_memo") &&
    is.finite(disc_f$backlog %||% NA_real_)
})
# Both fixtures on: Fugle wins for a ticker the memo names. SA stays last.
Sys.setenv(
  YNOW_SA_SCRAPE = "1",
  YNOW_SA_SCRAPE_FIXTURE_HTML = file.path(app_dir, "tests/fixtures/sa_transcript_sample.html")
)
both <- ynow_tw_earnings_call_fetch("8182.TW")
check("Fugle wins over SA fixture when both are on", {
  identical(both$source_id %||% "", "fugle_earnings_memo")
})
miss <- ynow_tw_earnings_call_fetch("8916.TW")
check("ticker absent from Fugle fixture falls through to SA", {
  identical(miss$source_id %||% "", "seeking_alpha_html") &&
    identical(miss$credibility %||% "", "last_resort")
})
Sys.unsetenv("YNOW_FUGLE_SCRAPE_FIXTURE_HTML")
Sys.unsetenv("YNOW_SA_SCRAPE_FIXTURE_HTML")
Sys.setenv(YNOW_FUGLE_SCRAPE = "0", YNOW_SA_SCRAPE = "0")

# Fixture-driven SA last-resort for TW bare ticker (Fugle off).
Sys.setenv(
  YNOW_SA_SCRAPE = "1",
  YNOW_FUGLE_SCRAPE = "0",
  YNOW_SA_SCRAPE_FIXTURE_HTML = file.path(app_dir, "tests/fixtures/sa_transcript_sample.html")
)
sa_got <- ynow_tw_earnings_call_fetch("8916.TW")
check("TW SA last-resort from fixture labels last_resort", {
  is.list(sa_got) &&
    identical(sa_got$credibility %||% "", "last_resort") &&
    identical(sa_got$source_id %||% "", "seeking_alpha_html") &&
    is.finite(sa_got$backlog %||% NA_real_)
})
Sys.unsetenv("YNOW_SA_SCRAPE_FIXTURE_HTML")
Sys.setenv(YNOW_SA_SCRAPE = "0")

disc <- ynow_tw_earnings_call_to_disclosure(got)
check("TW disclosure shape includes fugle_url + finmoconf_url", {
  all(c(
    "status", "alphamemo_url", "fugle_url", "finmoconf_url",
    "credibility", "source_id", "supply_chain_notes", "outlook_notes"
  ) %in% names(disc))
})

mod <- paste(readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("BBLab wires TW earnings-call fetch + Fugle + FinmoConf link", {
  grepl("ynow_tw_earnings_call_fetch", mod, fixed = TRUE) &&
    grepl("tw_earnings_call", mod, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_fugle_link", mod, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_fugle_note", mod, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_finmoconf_link", mod, fixed = TRUE)
})
check("global sources fugle/finmoconf before tw", {
  g <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  fi <- regexpr('source("fugle_earnings_call.R"', g, fixed = TRUE)
  fc <- regexpr('source("finmoconf_earnings_call.R"', g, fixed = TRUE)
  ti <- regexpr('source("tw_earnings_call.R"', g, fixed = TRUE)
  fi > 0L && fc > 0L && ti > fi && ti > fc &&
    grepl('source("seeking_alpha_earnings_call.R"', g, fixed = TRUE)
})
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("locale en + zh-TW TW EC + Fugle + FinmoConf + last_resort", {
  grepl("bblab_or_tw_ec_empty", loc, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_fugle_link", loc, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_fugle_note", loc, fixed = TRUE) &&
    grepl("bblab_or_tw_ec_finmoconf_link", loc, fixed = TRUE) &&
    grepl("bblab_or_cred_last_resort", loc, fixed = TRUE) &&
    grepl("富果法說會備忘錄", loc, fixed = TRUE) &&
    grepl("不會擷取 AlphaMemo", loc, fixed = TRUE) &&
    grepl("AlphaMemo is not scraped", loc, fixed = TRUE) &&
    grepl("Last-resort (third-party HTML)", loc, fixed = TRUE) &&
    grepl("FinmoConf", loc, fixed = TRUE)
})
check("no AlphaMemo scrape in TW/Fugle sources", {
  tw <- paste(readLines("tw_earnings_call.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  fg <- paste(readLines("fugle_earnings_call.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  !grepl("alphamemo.ai/free-transcripts/", fg, fixed = TRUE) &&
    grepl("no AlphaMemo HTML/API scrape", tw, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
