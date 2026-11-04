# Actual app/server with deterministic providers and forced recommendations.
# No prices/valuation results are injected. This file is never loaded in production.
Sys.setenv(YNOW_DEBUG_SKIP_PY='1')
source('global.R');source('ynow_ui.R');source('ynow_server.R')
fixture_currency <- function(tk)if(grepl('\\.(TW|TWO)$',tk))'TWD' else 'USD'
fixture_table <- function(labels,values,factor=1) {
 d<-data.frame(Breakdown=labels,check.names=FALSE)
 for(year in 2025:2022)d[[paste0('12/31/',year)]]<-values*factor*(0.95^(2025-year))
 shares<-grepl('Shares|Share Issued',labels)
 for(j in 2:ncol(d))d[shares,j]<-values[shares]
 d
}
cached_scrape_financials <- function(tk) {
 ccy<-fixture_currency(tk);f<-if(ccy=='TWD')32.6 else 1
 income<-fixture_table(c('Total Revenue','Operating Income','Pretax Income','Tax Provision','Interest Expense','Net Income','Net Income Common Stockholders','Basic EPS','Diluted EPS','Basic Average Shares','Diluted Average Shares','Reconciled Depreciation','Segment A Revenue','Segment B Revenue','Annual Dividends Per Share'),c(1000,200,190,38,10,152,152,7.6,7.6,20,20,60,600,400,2),f)
 cash<-fixture_table(c('Operating Cash Flow','Capital Expenditure','Depreciation And Amortization','Change In Working Capital','Common Stock Dividend Paid','Cash Dividends Paid','Interest Paid'),c(250,-80,60,-20,-40,-40,10),f)
 balance<-fixture_table(c('Total Assets','Total Liabilities Net Minority Interest','Total Liabilities','Common Stock Equity','Stockholders Equity','Total Equity Gross Minority Interest','Cash And Cash Equivalents','Total Debt','Current Debt','Current Assets','Current Liabilities','Non Operating Assets','Ordinary Shares Number','Share Issued'),c(2000,800,800,1200,1200,1200,100,200,50,600,300,50,20,20),f)
 res<-list('Income Statement'=list(collapsed=income,expanded=income),'Cash Flow'=list(collapsed=cash,expanded=cash),'Balance Sheet'=list(collapsed=balance,expanded=balance))
 for(key in names(res))for(part in c('collapsed','expanded')) {
  attr(res[[key]][[part]],'financialCurrency')<-ccy
  attr(res[[key]][[part]],'money_ccy')<-ccy
 }
 attr(res,'financialCurrency')<-ccy;attr(res,'currency')<-ccy
 res
}
cached_get_summary_data <- function(tk) {
 ccy<-fixture_currency(tk);f<-if(ccy=='TWD')32.6 else 1
 d<-data.frame(Item=c('Market Price','Market Cap (intraday)','Previous Close','Open','Beta (5Y Monthly)','PE Ratio (TTM)','EPS (TTM)','Trailing Annual Dividend Rate','Forward Dividend & Yield'),Value=as.character(c(100*f,2000*f,100*f,100*f,1.2,13.16,7.6*f,2*f,2*f)))
 attr(d,'company_name')<-'Chart fixture / 圖表測試資料'
 attr(d,'company_profile')<-list(sector='Technology',industry='Semiconductors',display_text='Sector: Technology | Industry: Semiconductors')
 attr(d,'currency')<-ccy;attr(d,'financialCurrency')<-ccy
 d
}
cached_get_usd_twd_fx <- function(...)32.6
cached_get_risk_free_rate <- function(market=NULL)if(identical(market,'TW'))1.6 else 4
cached_get_risk_free_rate_detail <- function(market=NULL)list(rf_pct=cached_get_risk_free_rate(market),source='fixture',label='Fixture',is_fallback=FALSE)
ynow_cached_hist_stock_trim <- function(tk,...) {
 f<-if(fixture_currency(tk)=='TWD')32.6 else 1
 data.frame(Date=seq(as.Date('2025-01-01'),by='day',length.out=300),Close=seq(75,100,length.out=300)*f,Volume=1000000)
}
.rel_sotp_fetch_segment_notes <- function(...)NULL
.rel_sotp_segments_from_is <- function(d_is,...)data.frame(name=c('Fixture A','Fixture B'),revenue=c(select_current_metric(d_is,'^Segment A Revenue$'),select_current_metric(d_is,'^Segment B Revenue$')))
fixture_guards <- new.env(parent=emptyenv())
original_guard <- ynow_input_echo_guard
ynow_input_echo_guard <- function(input,session,ticker,tracked) {
 g <- original_guard(input,session,ticker,tracked)
 ids <- function() if(is.function(tracked))tracked() else tracked
 fixture_guards[[session$ns('')]] <- shiny::reactive(list(
  values=setNames(lapply(ids(),function(id)input[[id]]),ids()),
  expected=setNames(lapply(ids(),function(id)g$expected[[id]]),ids()),ready=g$ready()))
 g
}
cached_get_corp_logo_colors <- function(...)character(0)
cached_get_yahoo_industry <- function(...)list(sector='Technology',industry='Semiconductors',display_text='Fixture')
resolve_industry_key_for_ticker <- function(...)'sc.Foundry'
b<-body(server)
for(i in seq_along(b)) {
 x<-b[[i]]
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('model_sidebar_rec'))) {
  b[[i]][[3]]<-quote(shiny::reactive({
   if(!isTRUE(user_has_searched()))return(.empty_model_rec('Fixture','Fixture'))
   key<-input$ynow_fixture_model %||% 'dcf'
   rec<-.empty_model_rec('Fixture','Fixture',company_type='stable')
   rec$primary<-if(key=='multiples')'' else if(key=='sotp')'ri' else key
   rec$secondary<-if(key=='sotp')'sotp' else ''
   rec$reference<-'multiples';rec$suggest_two_stage<-FALSE
   rec
  }))
 }
}
b[[length(b)+1L]] <- quote({
 output$fixture_diag <- shiny::renderText({
  jsonlite::toJSON(list(busy=ticker_search_busy(),searched=user_has_searched(),ke=central_ke(),assumptions=parameter_assumptions(),guards=lapply(as.list(fixture_guards),function(fn)fn()),root_nopat=input[['mod_fcf-fcf_nopat']],fcf_ready=tryCatch(fcf_results$source_ready(),error=function(e)conditionMessage(e)),
   projection=tryCatch(fcf_results$df_fcf(),error=function(e)conditionMessage(e)),
   ready=tryCatch(.auto_calc_primary_ready(model_sidebar_rec()$primary),error=function(e)conditionMessage(e)),
   shares_ready=tryCatch(.auto_calc_shares_ready(),error=function(e)conditionMessage(e)),
   wacc=calculated_wacc(),scenario=.effective_dcf_scenario(),rate=input$wacc_gordon,
   stage2=input$wacc_stage2,sgr=input$sgr,years=input$years,
   source=tryCatch(.valuation_shares(),error=function(e)conditionMessage(e))),auto_unbox=TRUE,na='null')
 })
 shiny::outputOptions(output,'fixture_diag',suspendWhenHidden=FALSE)
})
body(server)<-b
ui <- htmltools::tagAppendChild(ui, shiny::textOutput('fixture_diag'))
shiny::runApp(shiny::shinyApp(ui,server),host='0.0.0.0',port=as.integer(Sys.getenv('YNOW_FIXTURE_PORT','3852')),launch.browser=FALSE)
