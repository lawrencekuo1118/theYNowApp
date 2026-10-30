#!/usr/bin/env Rscript
args <- commandArgs(FALSE)
f <- sub('^--file=', '', grep('^--file=',args,value=TRUE)[1])
app <- dirname(dirname(normalizePath(f)))
library(shiny)
source(file.path(app,'async_jobs.R'))
source(file.path(app,'setup.R'))
source(file.path(app,'market_profile.R'))
source(file.path(app,'lab_tw_universe.R'))
session <- MockShinySession$new()
value <- reactiveVal('2330.TW')
seen <- NULL
cb <- function() {
  stopifnot(identical(getDefaultReactiveDomain(),session))
  seen <<- value()
  showNotification('test',id='search_test')
}
failed <- tryCatch({cb();FALSE},error=function(e) TRUE)
stopifnot(failed)
wrapped <- ynow_session_callback(session,cb)
later::later(wrapped,0)
later::run_now()
stopifnot(identical(seen,'2330.TW'))
# A failed request must still have a session for progress cleanup and notification.
closed <- FALSE
later::later(ynow_session_callback(session,function() {
  tryCatch(stop('summary unavailable'), error=function(e) {
    showNotification(conditionMessage(e),type='error')
    closed <<- TRUE
  })
}),0)
later::run_now()
stopifnot(closed)
for (pair in list(c('2330-TW','2330.TW'), c('7772-TWO','7772.TWO'),
                  c('7772-two','7772.TWO'),c('2330','2330.TW'),c('7772','7772.TWO'))) {
  stopifnot(looks_like_tw_stock_code(pair[1]),
            identical(normalize_ticker_for_market(pair[1],'TW'),pair[2]))
}
stopifnot(normalize_ticker_for_market('BRK.B','US')=='BRK-B',
          !looks_like_tw_stock_code('BRK-B'),
          normalize_ticker_for_market('7772-TWO','US')=='7772.TWO')
# Use committed official-universe fixtures, never the network.
u <- read.csv(file.path(app,'data/tw_universe.csv'),stringsAsFactors=FALSE)
lab_get_tw_universe <- function(...) u
stopifnot(lookup_tw_universe_company_name('2330.TW')=='台灣積體電路製造股份有限公司',
          lookup_tw_universe_company_name('7772.TWO')=='耀穎光電股份有限公司')
session$close()
seen <- NULL
wrapped()
stopifnot(is.null(seen))
cat('PASS: delayed session/reactive scope, failure cleanup, closed-session cancellation, TW dot/hyphen/bare aliases and official company names\n')
setwd(app)
Sys.setenv(YNOW_DEBUG_SKIP_PY='1')
source('web_crawler.R')
search_tickers <- function(...) list()
choices <- search_ticker_choices('7772-TWO',market='TW')
stopifnot('7772.TWO' %in% unname(choices))
cat('PASS: hyphenated TW alias resolves in offline typeahead as well as Search\n')
# An upstream timeout must not poison all retries for the 15-minute quote TTL.
calls <- 0L
get_summary_data <- function(ticker) {
  calls <<- calls + 1L
  if(calls==1L) return(.empty_summary(ticker))
  data.frame(Item='Previous Close',Value='100')
}
key <- paste0('summary-retry-',Sys.getpid())
stopifnot(nrow(cached_get_summary_data(key))==0L,
          nrow(cached_get_summary_data(key))==1L,
          nrow(cached_get_summary_data(key))==1L,calls==2L)
cat('PASS: failed Summary is not cached; successful retry uses the fast cache\n')
