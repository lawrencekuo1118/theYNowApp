# US earnings-call ingest — prefer IR/SEC/licensed; SA last-resort; honest empty.
# Run: cd app_26.0 && Rscript tests/test_us_earnings_call.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1", YNOW_SA_SCRAPE = "0")

app_dir <- if (file.exists("us_earnings_call.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../us_earnings_call.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate us_earnings_call.R")
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

check("SA url is seekingalpha transcripts path", {
  u <- ynow_us_earnings_call_sa_url("AAPL")
  grepl("seekingalpha.com/symbol/AAPL/earnings/transcripts", u, fixed = TRUE)
})
check("SA url blank for TW ticker", !nzchar(ynow_us_earnings_call_sa_url("2330.TW")))
check("SEC search url mentions 8-K", {
  grepl("sec.gov/edgar/search", ynow_us_earnings_call_sec_search_url("MSFT"), fixed = TRUE)
})

facts <- .ynow_us_ec_extract_facts(
  "Remaining performance obligations were $12.5 billion at quarter end. Net new bookings of $3.2 billion. CapEx of $1.0 billion. Next quarter guidance and Question-and-Answer Session follow."
)
check("extracts RPO backlog billions", isTRUE(abs(facts$backlog - 12.5e9) < 1e6))
check("extracts bookings billions", isTRUE(abs(facts$bookings - 3.2e9) < 1e6))
check("extracts CapEx billions", isTRUE(abs(facts$capex - 1.0e9) < 1e6))
check("themes include guidance/capex/qa", {
  all(c("cfo_guidance", "capex_plan", "analyst_qa") %in% (facts$themes %||% character(0)))
})
check("excerpt non-empty", nzchar(facts$excerpt))

empty_facts <- .ynow_us_ec_extract_facts("We are excited about the quarter.")
check("no invented backlog", !is.finite(empty_facts$backlog) && !is.finite(empty_facts$bookings))

# No API key / SA scrape off: honest empty + outbound links.
old_fmp <- Sys.getenv("YNOW_FMP_API_KEY", unset = NA_character_)
old_fmp2 <- Sys.getenv("FMP_API_KEY", unset = NA_character_)
Sys.unsetenv("YNOW_FMP_API_KEY")
Sys.unsetenv("FMP_API_KEY")
Sys.unsetenv("YNOW_TRANSCRIPT_API_KEY")
Sys.setenv(YNOW_SA_SCRAPE = "0")
on.exit({
  if (is.na(old_fmp)) Sys.unsetenv("YNOW_FMP_API_KEY") else Sys.setenv(YNOW_FMP_API_KEY = old_fmp)
  if (is.na(old_fmp2)) Sys.unsetenv("FMP_API_KEY") else Sys.setenv(FMP_API_KEY = old_fmp2)
}, add = TRUE)

got <- ynow_us_earnings_call_fetch("AAPL")
check("fetch returns list with SA link", {
  is.list(got) && grepl("seekingalpha.com", got$sa_url %||% "", fixed = TRUE)
})
check("without SA scrape, preferred empty is not seeking_alpha_html", {
  !identical(got$source_id %||% "", "seeking_alpha_html")
})
disc <- ynow_us_earnings_call_to_disclosure(got)
check("disclosure shape has status + links", {
  all(c("status", "sa_url", "sec_url", "source_id", "capex") %in% names(disc))
})

# Fixture: SA last-resort labels credibility correctly.
Sys.setenv(
  YNOW_SA_SCRAPE = "1",
  YNOW_SA_SCRAPE_FIXTURE_HTML = file.path(app_dir, "tests/fixtures/sa_transcript_sample.html")
)
sa <- ynow_us_earnings_call_fetch("EXMP")
check("SA last-resort fixture status ok", identical(sa$status %||% "", "ok"))
check("SA last-resort credibility last_resort", identical(sa$credibility %||% "", "last_resort"))
check("SA last-resort source_id seeking_alpha_html", identical(sa$source_id %||% "", "seeking_alpha_html"))
check("SA last-resort extracts backlog from fixture", isTRUE(abs((sa$backlog %||% 0) - 4.5e9) < 1e6))
Sys.unsetenv("YNOW_SA_SCRAPE_FIXTURE_HTML")
Sys.setenv(YNOW_SA_SCRAPE = "0")

# Parser unit checks (no network).
idx_html <- '<a href="/article/123456-example-corp-q1-2026-earnings-call-transcript">t</a>'
links <- .ynow_sa_ec_parse_index_links(idx_html)
check("index parser finds transcript article", {
  length(links) >= 1L && grepl("earnings-call-transcript", links[[1]], fixed = TRUE)
})
check("captcha index yields no links", {
  length(.ynow_sa_ec_parse_index_links("<html>Access to this page has been denied px-captcha</html>")) == 0L
})

mod <- paste(readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("BBLab wires US earnings-call fetch", {
  grepl("ynow_us_earnings_call_fetch", mod, fixed = TRUE) &&
    grepl("order_disclosures", mod, fixed = TRUE) &&
    grepl("bblab_or_us_ec_sa_link", mod, fixed = TRUE)
})
check("global sources us + sa helpers", {
  g <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source("us_earnings_call.R"', g, fixed = TRUE) &&
    grepl('source("seeking_alpha_earnings_call.R"', g, fixed = TRUE)
})
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("locale en + zh-TW US EC strings", {
  grepl("bblab_or_us_ec_empty", loc, fixed = TRUE) &&
    grepl("last-resort fallback", loc, fixed = TRUE) &&
    grepl("最後備援", loc, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
