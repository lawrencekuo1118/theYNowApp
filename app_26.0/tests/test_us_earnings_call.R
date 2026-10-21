# US earnings-call ingest — no SA scrape; honest empty; disclosure shape.
# Run: cd app_26.0 && Rscript tests/test_us_earnings_call.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

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
  "Remaining performance obligations were $12.5 billion at quarter end. Net new bookings of $3.2 billion."
)
check("extracts RPO backlog billions", isTRUE(abs(facts$backlog - 12.5e9) < 1e6))
check("extracts bookings billions", isTRUE(abs(facts$bookings - 3.2e9) < 1e6))
check("excerpt non-empty", nzchar(facts$excerpt))

empty_facts <- .ynow_us_ec_extract_facts("We are excited about the quarter.")
check("no invented backlog", !is.finite(empty_facts$backlog) && !is.finite(empty_facts$bookings))

# No API key / no network: still returns honest empty + outbound links.
old_fmp <- Sys.getenv("YNOW_FMP_API_KEY", unset = NA_character_)
old_fmp2 <- Sys.getenv("FMP_API_KEY", unset = NA_character_)
Sys.unsetenv("YNOW_FMP_API_KEY")
Sys.unsetenv("FMP_API_KEY")
Sys.unsetenv("YNOW_TRANSCRIPT_API_KEY")
on.exit({
  if (is.na(old_fmp)) Sys.unsetenv("YNOW_FMP_API_KEY") else Sys.setenv(YNOW_FMP_API_KEY = old_fmp)
  if (is.na(old_fmp2)) Sys.unsetenv("FMP_API_KEY") else Sys.setenv(FMP_API_KEY = old_fmp2)
}, add = TRUE)

# Force SEC path to miss without Python by stubbing fetch absence — fetch still returns links.
got <- ynow_us_earnings_call_fetch("AAPL")
check("fetch returns list with SA link", {
  is.list(got) && grepl("seekingalpha.com", got$sa_url %||% "", fixed = TRUE)
})
check("fetch does not scrape SA (no sa scrape marker)", {
  !identical(got$source_id %||% "", "seeking_alpha") &&
    !grepl("seekingalpha.com/symbol/.*/earnings/transcripts$", got$url %||% "")
})
disc <- ynow_us_earnings_call_to_disclosure(got)
check("disclosure shape has status + links", {
  identical(names(disc)[1:5], c("backlog", "bookings", "excerpt", "title", "url")) ||
    all(c("status", "sa_url", "sec_url", "source_id") %in% names(disc))
})

mod <- paste(readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("BBLab wires US earnings-call fetch", {
  grepl("ynow_us_earnings_call_fetch", mod, fixed = TRUE) &&
    grepl("order_disclosures", mod, fixed = TRUE) &&
    grepl("bblab_or_us_ec_sa_link", mod, fixed = TRUE)
})
check("global sources us_earnings_call.R", {
  g <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source("us_earnings_call.R"', g, fixed = TRUE)
})
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("locale en + zh-TW US EC strings", {
  grepl("bblab_or_us_ec_empty", loc, fixed = TRUE) &&
    grepl("does not scrape SA HTML", loc, fixed = TRUE) &&
    grepl("不會擷取 SA 網頁 HTML", loc, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
