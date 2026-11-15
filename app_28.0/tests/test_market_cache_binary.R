#!/usr/bin/env Rscript
args <- commandArgs(FALSE)
f <- sub('^--file=', '', grep('^--file=',args,value=TRUE)[1])
app <- dirname(dirname(normalizePath(f)))
library(DBI)
source(file.path(app,'market_cache.R'))
setClass('YnowCacheTestCon',contains='DBIConnection')
received <- NULL
setMethod('dbExecute',signature(conn='YnowCacheTestCon',statement='character'),
  function(conn,statement,...,params=list()) {
    # RPostgres requires one SQL parameter value per row, including bytea.
    stopifnot(all(lengths(params)==1L))
    received <<- params
    1L
  })
.ynow_market_db_on <- function() TRUE
ynow_market_cache_ensure_schema <- function() TRUE
ynow_db_with <- function(fn) fn(new('YnowCacheTestCon'))
value <- data.frame(Date=as.Date('2026-10-30'),Close=100)
stopifnot(isTRUE(ynow_market_cache_put('test','price_day','7772.TWO',value)),
          is.raw(received[[4]][[1]]),
          identical(unserialize(received[[4]][[1]]),value))
cat('PASS: serialized cache payload binds as one bytea parameter and round-trips\n')
