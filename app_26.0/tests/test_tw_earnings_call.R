# TW earnings-call — AlphaMemo outbound + SA last-resort; honest empty.
# Run: cd app_26.0 && Rscript tests/test_tw_earnings_call.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1", YNOW_SA_SCRAPE = "0")

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
source("tw_earnings_call.R", local = TRUE, encoding = "UTF-8")

check("AlphaMemo index url stable", {
  identical(
    ynow_tw_earnings_call_alphamemo_index_url(),
    "https://www.alphamemo.ai/free-transcripts"
  )
})
check("search url mentions alphamemo + ticker", {
  u <- ynow_tw_earnings_call_alphamemo_search_url("2330.TW")
  grepl("alphamemo", u, fixed = TRUE) && grepl("2330", u, fixed = TRUE)
})

Sys.setenv(YNOW_SA_SCRAPE = "0")
got <- ynow_tw_earnings_call_fetch("2330.TW")
check("TW fetch returns AlphaMemo link", {
  is.list(got) && grepl("alphamemo.ai/free-transcripts", got$alphamemo_url %||% "", fixed = TRUE)
})
check("TW fetch honest empty without SA when scrape off", {
  identical(got$status %||% "", "empty") &&
    identical(got$credibility %||% "", "estimate") &&
    !is.finite(got$backlog %||% NA_real_)
})
check("US ticker leave TW path", {
  u <- ynow_tw_earnings_call_fetch("AAPL")
  identical(u$empty_reason %||% "", "us_leave_as_is")
})

# Fixture-driven SA last-resort for TW bare ticker.
Sys.setenv(
  YNOW_SA_SCRAPE = "1",
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
check("TW disclosure shape", {
  all(c("status", "alphamemo_url", "credibility", "source_id") %in% names(disc))
})

mod <- paste(readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("BBLab wires TW earnings-call fetch", {
  grepl("ynow_tw_earnings_call_fetch", mod, fixed = TRUE) &&
    grepl("tw_earnings_call", mod, fixed = TRUE)
})
check("global sources tw + sa helpers", {
  g <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source("tw_earnings_call.R"', g, fixed = TRUE) &&
    grepl('source("seeking_alpha_earnings_call.R"', g, fixed = TRUE)
})
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("locale en + zh-TW TW EC + last_resort", {
  grepl("bblab_or_tw_ec_empty", loc, fixed = TRUE) &&
    grepl("bblab_or_cred_last_resort", loc, fixed = TRUE) &&
    grepl("最後備援（第三方 HTML）", loc, fixed = TRUE) &&
    grepl("Last-resort (third-party HTML)", loc, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
