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

rss <- ynow_news_parse_rss("<rss><channel><title>Yahoo Finance</title><item><title>Lawsuit filed</title></item></channel></rss>")
check("parse skips channel title", identical(rss$title, "Lawsuit filed"))

veto <- ynow_news_apply_veto(0.4, 0.8, 0.85, -0.8, TRUE)
check("block oversold", identical(veto$veto, "block_oversold") && veto$pos_b <= 0.4)
soft <- ynow_news_apply_veto(0.6, 0.3, 0.15, 0.8, TRUE)
check("soften overbought", identical(soft$veto, "soften_overbought") && soft$pos_b >= 0.6)
off <- ynow_news_apply_veto(0.4, 0.8, 0.85, -0.8, FALSE)
check("off", identical(off$veto, "off") && identical(off$pos_b, 0.8))

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
