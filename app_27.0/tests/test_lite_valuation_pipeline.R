#!/usr/bin/env Rscript
args<-commandArgs(FALSE)
script<-sub('^--file=','',grep('^--file=',args,value=TRUE)[1])
app<-dirname(dirname(normalizePath(script)))
source(file.path(app,'setup.R'));source(file.path(app,'ui_locale.R'))
source(file.path(app,'industry_standards.R'));source(file.path(app,'default_config.R'))
source(file.path(app,'parameter_completeness.R'))
suppressPackageStartupMessages(library(shiny))
assignments<-list();growth<-NULL;automatic<-NULL;delayed<-NULL
walk<-function(x) {
 if(missing(x)||(!is.call(x)&&!is.expression(x)))return()
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&is.symbol(x[[2]]))assignments[[as.character(x[[2]])]]<<-x[[3]]
 if(is.call(x)&&identical(x[[1]],as.name('observe'))) {
  text<-paste(deparse(x),collapse='\n')
  if(grepl('raw_fund_g',text,fixed=TRUE))growth<<-x
  if(grepl('sec_auto_allowlist',text,fixed=TRUE))automatic<<-x
 }
 if(is.call(x)&&identical(paste(deparse(x[[1]]),collapse=''),'shinyjs::delay')&&grepl('param_audit_capture_token',paste(deparse(x),collapse=''),fixed=TRUE))delayed<<-x[[3]]
 for(j in seq_along(x))walk(x[[j]])
}
walk(parse(file.path(app,'ynow_server.R')))
stopifnot(!is.null(growth),!is.null(automatic),!is.null(delayed))
# Evaluate the actual delayed callback in a returned function frame. A stale
# token must skip the capture without a non-local return()/session disconnect.
make_frame<-function(){tok<-1L;delay_ms<-2200L;environment()}
f<-make_frame();f$isolate<-function(x)x;f$count<-0L
f$param_audit_capture_token<-function()2L
f$.capture_param_audit_baseline<-function(...)f$count<-f$count+1L
eval(delayed,f);stopifnot(f$count==0L)
f$param_audit_capture_token<-function()1L
eval(delayed,f);stopifnot(f$count==1L)
cat('PASS: expired delayed audit capture is harmless; current capture executes once\n')

for(edition in c(FALSE,TRUE))for(secondary in c('ddm','ri','pb','nav','sotp')) {
 shiny::testServer(function(input,output,session) {
  executions<-new.env(parent=emptyenv());executions$keys<-character()
  e<-new.env(parent=globalenv())
  e$input<-list(dcf_mode='gordon',dcf_claim='fcff',sgr=2)
  e$user_has_searched<-function()TRUE;e$current_ticker<-function()'FIXTURE'
  e$model_sidebar_rec<-function()list(primary='dcf',secondary=secondary,reference='multiples')
  e$lite_mode<-function()edition
  e$.lite_desired_scenario<-function(...)list(two_stage=FALSE,method='fundamental',claim='fcff')
  e$lite_scenario_applied_sig<-reactiveVal('FIXTURE|gordon|fundamental|fcff')
  e$.lite_resync_discount_consistency<-function(...)NULL
  e$.auto_calc_primary_ready<-function(key)key!='dcf'
  e$auto_calc_fail_sig<-reactiveVal('');e$auto_calc_primary_sig<-reactiveVal('')
  e$calculated_wacc<-function().08;e$.valuation_shares<-function()list(method='balance_sheet')
  e$fx_usd_twd<-function()32.6
  e$.fire_auto_calc_primary<-function(key)executions$keys<-c(executions$keys,key)
  observer<-eval(automatic,e)
 },{
  session$flushReact()
  if(edition)stopifnot(setequal(executions$keys,c(secondary,'multiples')))
  else stopifnot(length(executions$keys)==0L)
 })
}
cat('PASS: all five Lite companion models and Multiples calculate independently when primary DCF is unavailable; Full keeps its workflow\n')

for(edition in c(FALSE,TRUE))for(missing_ic in c(FALSE,TRUE)) {
 shiny::testServer(function(input,output,session) {
  e<-new.env(parent=globalenv())
  e$input<-list(g_growth_method='fundamental',wacc_tax=20,`mod_fcf-apply_g_ceiling`=TRUE)
  e$d_income_statement<-function()data.frame(Breakdown=c('Total Revenue','Operating Income','Pretax Income','Tax Provision'),`2025`=c(1000,200,200,40),`2024`=c(950,190,190,38),check.names=FALSE)
  e$d_cash_flow<-function()data.frame(Breakdown=c('Depreciation And Amortization','Capital Expenditure','Change In Working Capital'),`2025`=c(60,-80,-20),`2024`=c(57,-76,-19),check.names=FALSE)
  e$d_balance_sheet<-function(){d<-data.frame(Breakdown=c('Total Assets','Total Liabilities','Total Debt','Cash And Cash Equivalents','Non Operating Assets'),`2025`=c(2000,800,200,100,50),check.names=FALSE);if(missing_ic)d<-d[-5,];d}
  e$estimated_g<-reactiveVal(NULL);e$estimated_g_raw<-reactiveVal(NULL)
  e$estimated_g_meta<-reactiveValues(method=NULL,fund_res=NULL,source=NULL,raw_g=NULL,is_assumption=FALSE)
  e$run_calc_trigger<-reactiveVal(0L)
  e$lite_mode<-function()edition;e$ui_locale<-function()'en'
  e$updateSelectInput<-function(...)NULL;e$updateNumericInput<-function(...)NULL
  observer<-eval(growth,e)
 },{
  session$flushReact()
  if(!missing_ic)stopifnot(e$estimated_g()==3.2,!e$estimated_g_meta$is_assumption,e$estimated_g_meta$fund_res$ic==1250)
  else if(edition)stopifnot(e$estimated_g()==APP_DEFAULTS$custom_g,e$estimated_g_meta$is_assumption,is.na(statement_operating_invested_capital(e$d_balance_sheet())))
  else stopifnot(is.null(e$estimated_g()))
 })
}
cat('PASS: production growth uses operating IC and matching-period NOPAT/D&A; missing fundamentals use a disclosed Lite forecast assumption, not invented accounting values\n')
source(file.path(app,'ri_module.R'))
e<-new.env(parent=globalenv())
e$input<-list(`mod_ri-b0`=20,`mod_ri-ri_ke`=10,`mod_ri-ri_g`=2,`mod_ri-ri_years`=5,
 `mod_ri-ri_payout`=NA_real_,`mod_ri-ri_roe`=15,`mod_ri-roe_method`='constant')
price<-eval(assignments[['.ri_price_at']],e)
stopifnot(is.na(price()))
e$input[['mod_ri-ri_payout']]<-40
stopifnot(is.finite(price()))
e$input[['mod_ri-ri_roe']]<-NA_real_
stopifnot(is.na(price()))
cat('PASS: RI scenario charts never replace missing payout with zero or missing ROE with an undisclosed rate\n')
