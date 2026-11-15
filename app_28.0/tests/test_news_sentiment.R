#!/usr/bin/env Rscript
# News lexicon, red flags, and Mode B veto (no network).
root <- if (file.exists("news_sentiment.R")) {
  getwd()
} else if (file.exists("../news_sentiment.R")) {
  normalizePath("..")
} else {
  stop("Cannot locate news_sentiment.R")
}
setwd(root)
source("news_sentiment.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check("neg", ynow_news_score_headline("SEC investigation into accounting fraud") < 0)
check("pos", ynow_news_score_headline("Company announces share buyback and special dividend") > 0)
check("flat", identical(ynow_news_score_headline("Quarterly update"), 0))

bundle <- ynow_news_score_bundle(c(
  "CEO resigns after SEC probe",
  "New plant expansion lifts capital expenditure"
))
check("flags", all(c("management", "sec") %in% bundle$flags))
check("nudge capex", "capex" %in% bundle$nudges)
check("score range", is.finite(bundle$score) && bundle$score >= -1 && bundle$score <= 1)

now <- as.POSIXct("2026-10-18 12:00:00", tz = "UTC")
rss_xml <- paste0(
  "<rss><channel><title>Yahoo Finance</title>",
  "<item><title>Lawsuit filed</title><pubDate>Sat, 18 Oct 2026 10:00:00 +0000</pubDate></item>",
  "<item><title>Old layoff headline</title><pubDate>Thu, 16 Oct 2026 10:00:00 +0000</pubDate></item>",
  "<item><title>Undated headline</title></item>",
  paste0(vapply(1:14, function(i) {
    sprintf("<item><title>Fresh headline %02d</title><pubDate>Sat, 18 Oct 2026 11:%02d:00 +0000</pubDate></item>", i, i)
  }, character(1)), collapse = ""),
  "</channel></rss>"
)
rss <- ynow_news_parse_rss(rss_xml, now = now)
check("parse skips channel title", rss$title[1] == "Lawsuit filed")
check("drops older than 24h", !any(grepl("Old layoff", rss$title)))
check("drops undated", !any(grepl("Undated", rss$title)))
check("keeps every headline inside 24h", length(rss$title) == 15L)

veto <- ynow_news_apply_veto(0.4, 0.8, 0.85, -0.8, TRUE)
check("block oversold", identical(veto$veto, "block_oversold") && veto$pos_b <= 0.4)
soft <- ynow_news_apply_veto(0.6, 0.3, 0.15, 0.8, TRUE)
check("soften overbought", identical(soft$veto, "soften_overbought") && soft$pos_b >= 0.6)
off <- ynow_news_apply_veto(0.4, 0.8, 0.85, -0.8, FALSE)
check("off", identical(off$veto, "off") && identical(off$pos_b, 0.8))

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
