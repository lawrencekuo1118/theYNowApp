#!/usr/bin/env Rscript
args<-commandArgs(FALSE);script<-sub('^--file=','',grep('^--file=',args,value=TRUE)[1])
app<-dirname(dirname(normalizePath(script)));source(file.path(app,'setup.R'))
source(file.path(app,'web_crawler.R'))
frame<-function(labels,values){d<-data.frame(Breakdown=labels,check.names=FALSE);d[['12/31/2025']]<-values;d[['12/31/2024']]<-values*0.9;d}
is<-frame(c('Total Revenue','Operating Income','Pretax Income','Tax Provision','Interest Expense','Net Income Common Stockholders','Diluted EPS','Diluted Average Shares','Annual Dividends Per Share','Annual Recurring Revenue','Segment A Revenue','Segment B Revenue'),c(1000,200,190,38,10,152,7.6,20,2,300,600,400))
bs<-frame(c('Common Stock Equity','Total Equity Gross Minority Interest','Ordinary Shares Number','Cash And Cash Equivalents','Total Debt','Total Assets','Total Liabilities Net Minority Interest','Non Operating Assets','Goodwill','Other Intangible Assets'),c(1200,1200,20,100,200,2000,800,50,10,5))
cf<-frame(c('Depreciation And Amortization','Capital Expenditure','Change In Working Capital','Operating Cash Flow'),c(60,-80,-20,250))
data<-list('Income Statement'=is,'Balance Sheet'=bs,'Cash Flow'=cf)
source_of<-function(data,origin='cache',currency='USD',ticker='TEST',annual=TRUE)ynow_statement_source(data,origin,ticker,currency,annual)
context<-list(financial_currency='USD',ticker='TEST');sources<-list(source_of(data))
models<-c('dcf','ddm','ri','pb','nav','multiples','sotp');periods<-c('2025-12-31','2024-12-31')
audit<-ynow_audit_model_years(sources,context=context)
stopifnot(length(audit)==14L,all(vapply(audit,function(x)x$complete,logical(1))))
f<-audit[['dcf@2025-12-31']]$facts
stopifnot(f$nopat$value==160,f$da$value==60,f$capex$value==80,f$delta_nwc$value==20,f$b0$value==60,f$eps$value==7.6,f$fcff_cfo$value==178,f$operating_ic$value==1250)
# Remove every necessary financial parameter at its actual source, including all
# legitimate derivation paths. Older-year values remain available deliberately.
remove<-list(revenue='Total Revenue',nopat='Operating Income',da='Depreciation And Amortization',
 capex='Capital Expenditure',delta_nwc='Change In Working Capital',cash='Cash And Cash Equivalents',
 debt='Total Debt',outstanding='Ordinary Shares Number',other_claims='Total Equity Gross Minority Interest',
 interest='Interest Expense',tax='Tax Provision',d0='Annual Dividends Per Share',b0='Common Stock Equity',
 tbvps='Common Stock Equity',navps='Common Stock Equity',eps=c('Diluted EPS','Net Income Common Stockholders'),
 fcff_cfo='Operating Cash Flow',ebit='Operating Income',ebitda=c('Operating Income','Depreciation And Amortization'),
 arr='Annual Recurring Revenue',segments=c('Segment A Revenue','Segment B Revenue'))
checks<-0L
for(formula in names(ynow_formula_specs()))for(period in periods) {
 spec<-ynow_formula_specs()[[formula]]
 for(parameter in ynow_model_financial_requirements(spec$model,spec$config %||% list())) {
 broken<-data
 for(table in names(broken)) {
  rows<-broken[[table]][[1]] %in% remove[[parameter]]
  col<-if(period=='2025-12-31')2L else 3L
  broken[[table]][rows,col]<-NA_real_
 }
 snap<-ynow_model_snapshot(list(source_of(broken)),period,spec$model,spec$config %||% list(),context)
 if (snap$complete || !parameter %in% snap$missing$parameter)
  stop('omission did not block: ',formula,' / ',period,' / ',parameter)
 checks<-checks+1L
 }
}
cat('PASS:',checks,'formula variant × fiscal-year × required-parameter omissions block calculation\n')
# One request contains every gap, and a provider repairs all models together.
broken<-data;broken[['Cash Flow']][1:3,2]<-NA;broken[['Balance Sheet']][c(3,4,5),2]<-NA
calls<-list()
recovered<-ynow_recover_model_inputs(list(source_of(broken)),periods[1],list(old_cache=function(request){calls[[1]]<<-request;source_of(broken,'old_cache')},original_source=function(request){calls[[2]]<<-request;source_of(data,'original')}),context=context)
stopifnot(length(calls)==2L,
 all(c('da','capex','delta_nwc','cash','debt','outstanding') %in% calls[[1]]$parameters),
 all(vapply(recovered$after,function(x)x$complete,logical(1))))
# Wrong-year, interim, wrong-currency and wrong-issuer sources cannot repair.
wrong_year<-lapply(data,function(d)d[,c(1,3),drop=FALSE])
for(candidate in list(source_of(wrong_year),source_of(data,annual=FALSE),source_of(data,currency='TWD'),source_of(data,ticker='OTHER'))) {
 result<-ynow_recover_model_inputs(list(source_of(broken)),periods[1],list(provider=function(request)candidate),context=context)
 stopifnot(!result$after[['dcf@2025-12-31']]$complete)
}
# Every missing item is retained after a partial repair or provider failure.
partial<-broken;partial[['Cash Flow']][1,2]<-60
result<-ynow_recover_model_inputs(list(source_of(broken)),periods[1],list(partial=function(request)source_of(partial),failed=function(request)stop('offline')),context=context)
gaps<-result$after[['dcf@2025-12-31']]$missing$parameter
stopifnot(all(c('capex','delta_nwc','cash','debt','outstanding') %in% gaps),!('da' %in% gaps),identical(result$attempts$failed$error,'offline'))
# TTM is not a fiscal year; column position never determines the year.
ttm<-data;ttm[['Cash Flow']]<-data[['Cash Flow']][,1:2];names(ttm[['Cash Flow']])[2]<-'TTM'
stopifnot(is.na(ynow_integrity_facts(list(source_of(ttm)),periods[1],context)$da$value))
view<-ynow_fiscal_frame(recovered$sources,'Cash Flow',periods[1],context)
stopifnot(ncol(view)==2L,identical(names(view)[2],periods[1]),identical(attr(view,'valuation_fiscal_period'),periods[1]))
# Current Summary EPS cannot silently replace a missing selected-year EPS.
set_ynow_currency_context('USD',32.6,'USD','USD')
empty<-view;empty[[1]]<-'unrelated'
stopifnot(is.na(valuation_trailing_eps(data.frame(Item='EPS (TTM)',Value='999'),empty)))
# Dividend events are allocated to the issuer's adjacent fiscal endpoints, not
# calendar years. The event after the endpoint belongs to the next fiscal year.
div_res<-list('Income Statement'=list(expanded=is),'Cash Flow'=list(expanded=cf))
attr(div_res,'currency')<-'USD'
events<-data.frame(Date=c('2024-12-31','2025-03-01','2025-12-31','2026-01-01'),
 Dividend=c(1,2,3,100))
div_res<-ynow_attach_fiscal_dividends(div_res,events)
stopifnot(div_res$Dividends[['12/31/2025']]==5,
 identical(attr(div_res$Dividends,'money_ccy'),'USD'))
# A Full-mode manual fact is scoped to the exact ticker and fiscal period.
missing_revenue<-data;missing_revenue[['Income Statement']][1,2]<-NA
manual<-list(revenue=list(value=1000,period=periods[1],ticker='TEST',source='user_manual'))
stopifnot(ynow_model_snapshot(list(source_of(missing_revenue)),periods[1],'dcf',
 context=c(context,list(manual=manual)))$complete)
stopifnot(!ynow_model_snapshot(list(source_of(missing_revenue)),periods[1],'dcf',
 context=list(financial_currency='USD',ticker='OTHER',manual=manual))$complete)
stopifnot(!identical(ynow_model_snapshot(list(source_of(missing_revenue)),periods[2],'dcf',
 context=c(context,list(manual=manual)))$facts$revenue$kind,'manual'))
# A positive gross-equity/common-equity gap is a disclosed Lite book proxy, not
# a reported market value. Full therefore remains incomplete until manual input.
claims_data<-data;claims_data[['Balance Sheet']][claims_data[['Balance Sheet']][[1]]=='Total Equity Gross Minority Interest',2]<-1250
full_claims<-ynow_integrity_facts(list(source_of(claims_data)),periods[1],context)$other_claims
lite_claims<-ynow_integrity_facts(list(source_of(claims_data)),periods[1],
 c(context,list(allow_book_claim_proxy=TRUE)))$other_claims
stopifnot(is.na(full_claims$value),lite_claims$value==50,
 identical(lite_claims$kind,'assumption_derived'))
# Exact financial fact semantics: no aggregate Other, no standalone Depreciation.
for(label in c('Depreciation, Amortization & Other','Depreciation')) {
 invalid<-data;invalid[['Cash Flow']][[1]][1]<-label
 stopifnot(is.na(ynow_integrity_facts(list(source_of(invalid)),periods[1],context)$da$value))
}
cat('PASS: batch recovery; strict period/annual/currency/issuer matching; complete missing list; provenance and TTM isolation\n')
