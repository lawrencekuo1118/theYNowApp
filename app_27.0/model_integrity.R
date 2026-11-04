# Shared, fiscal-period-scoped source registry. No model may resolve a financial
# parameter from a different fiscal period. Market observations and forecast
# assumptions are separate types, never reported financial facts.
ynow_fiscal_key <- function(x) {
 x<-trimws(as.character(x));out<-rep(NA_character_,length(x))
 dates<-suppressWarnings(as.Date(x,format='%m/%d/%Y'));i<-is.na(dates)
 dates[i]<-suppressWarnings(as.Date(x[i],format='%Y-%m-%d'))
 out[!is.na(dates)]<-format(dates[!is.na(dates)],'%Y-%m-%d')
 year<-grepl('^(FY|Y)?[12][0-9]{3}$',x,ignore.case=TRUE)
 out[year]<-paste0('FY',sub('^(FY|Y)','',toupper(x[year])))
 out
}
ynow_statement_source <- function(data,origin='cache',ticker='',currency=NULL,annual=TRUE) {
 filled<-attr(data,'tpex_filled') %||% character()
 if(length(filled)){flags<-setNames(rep(TRUE,5),c('Income Statement','Balance Sheet','Cash Flow','Dividends','Segments'));flags[filled]<-FALSE;annual<-flags}
 list(data=data,origin=origin,ticker=toupper(ticker),currency=currency %||% attr(data,'financialCurrency'),annual=annual)
}
ynow_source_is_annual <- function(source,table) {
 a<-source$annual
 if(length(a)>1L&&!is.null(names(a)))isTRUE(a[[table]]) else isTRUE(a)
}
ynow_source_frame <- function(source,table) {
 x<-source$data[[table]]
 if(is.data.frame(x))return(x)
 if(is.list(x))return(x$expanded %||% x$collapsed)
 NULL
}
ynow_fiscal_periods <- function(sources) {
 keys<-unlist(lapply(sources,function(s)unlist(lapply(c('Income Statement','Balance Sheet','Cash Flow','Dividends','Segments'),function(t){d<-ynow_source_frame(s,t);if(is.data.frame(d))ynow_fiscal_key(names(d)[-1]) else character()}))),use.names=FALSE)
 keys<-unique(keys[!is.na(keys)])
 dated_years<-substr(keys[grepl('^[12][0-9]{3}-',keys)],1,4)
 keys<-keys[!keys %in% paste0('FY',dated_years)]
 keys[order(sub("^FY", "", keys), decreasing=TRUE)]
}
ynow_period_columns <- function(source,table,period,all_periods) {
 d<-ynow_source_frame(source,table)
 if(!is.data.frame(d)||ncol(d)<2L)return(integer())
 keys<-ynow_fiscal_key(names(d)[-1]);hit<-which(keys==period)+1L
 if(length(hit))return(hit)
 # A year-only label can match a dated fiscal year only when that year's
 # annual endpoint is unambiguous. Quarterly/YTD labels are never promoted.
 if(!ynow_source_is_annual(source,table))return(integer())
 yr<-sub('^FY','',substr(period,1,if(startsWith(period,'FY'))6 else 4))
 dated<-all_periods[grepl(paste0('^',yr,'-[0-9]{2}-[0-9]{2}$'),all_periods)]
 if(length(unique(dated))==1L&&(period==dated[1]||period==paste0('FY',yr))) {
  aliases<-c(dated[1],paste0('FY',yr));return(which(keys %in% aliases)+1L)
 }
 integer()
}
ynow_fact_catalog <- function() {
 atom<-function(table,patterns,unit='money',annual=FALSE)list(table=table,patterns=patterns,unit=unit,annual=annual)
 list(
 revenue=atom('Income Statement',c('^Total Revenue$','^Operating Revenue$'),annual=TRUE),
 ebit=atom('Income Statement',c('^Operating Income$','^EBIT$'),annual=TRUE),
 pretax=atom('Income Statement','^Pretax Income$',annual=TRUE),
 tax_expense=atom('Income Statement',c('^Tax Provision$','^Income Tax Expense$'),annual=TRUE),
 interest=atom('Income Statement',INTEREST_EXPENSE_PATTERNS,annual=TRUE),
 common_income=atom('Income Statement','^Net Income Common Stockholders$',annual=TRUE),
 common_equity=atom('Balance Sheet','^Common Stock Equity$'),
 stockholder_equity=atom('Balance Sheet','^Stockholders Equity$'),
 preferred_equity=atom('Balance Sheet',c('^Preferred Stock Equity$','^Preferred Stock$')),
 gross_equity=atom('Balance Sheet','^Total Equity Gross Minority Interest$'),
 shares=atom('Balance Sheet','^Ordinary Shares Number$',unit='shares'),
 issued_shares=atom('Balance Sheet','^Share Issued$',unit='shares'),
 treasury_shares=atom('Balance Sheet','^Treasury Shares Number$',unit='shares'),
 weighted_diluted=atom('Income Statement','^Diluted Average Shares$',unit='shares',annual=TRUE),
 weighted_basic=atom('Income Statement','^Basic Average Shares$',unit='shares',annual=TRUE),
 diluted_eps=atom('Income Statement','^Diluted EPS$',unit='per_share',annual=TRUE),
 basic_eps=atom('Income Statement','^Basic EPS$',unit='per_share',annual=TRUE),
 cash=atom('Balance Sheet',c('^Cash And Cash Equivalents$','^Cash & Cash Equivalents$')),
 debt=atom('Balance Sheet','^Total Debt$'),
 assets=atom('Balance Sheet','^Total Assets$'),
 liabilities=atom('Balance Sheet',c('^Total Liabilities Net Minority Interest$','^Total Liabilities$')),
 nonoperating_book=atom('Balance Sheet','^Non Operating Assets$'),
 goodwill=atom('Balance Sheet','^Goodwill$'),
 intangibles=atom('Balance Sheet',c('^Other Intangible Assets$','^Intangible Assets$')),
 investments=atom('Balance Sheet',c('^Investments And Advances$','^Long Term Investments$',
  '^Available For Sale Securities$','^Equity Method Investments$')),
 capital_expenditure=atom('Cash Flow',CAPEX_PATTERNS,annual=TRUE),
 nwc_cash_change=atom('Cash Flow',NWC_CHANGE_PATTERNS,annual=TRUE),
 cfo=atom('Cash Flow',c('^Operating Cash Flow$','^Total Cash From Operating Activities$'),annual=TRUE),
 da_total=atom('Cash Flow',setdiff(DA_PATTERNS,c('^Depreciation$','^Depreciation, Amortization & Other$')),annual=TRUE),
 da_reconciled=atom('Income Statement','^Reconciled Depreciation$',annual=TRUE),
 depreciation=atom('Cash Flow','^Depreciation$',annual=TRUE),
 amortization=atom('Cash Flow',c('^Amortization Of Intangibles$','^Amortization Of Intangible Assets$'),annual=TRUE),
 dps_reported=atom('Income Statement',c('^Cash Dividends Per Share$','^Annual Dividends Per Share$','^Dividend Per Share$'),unit='per_share',annual=TRUE),
 dps_events=atom('Dividends','^Cash DPS By Ex Date$',unit='quote_per_share',annual=TRUE),
 common_dividends=atom('Cash Flow','^Common Stock Dividend Paid$',annual=TRUE),
 ebitda_reported=atom('Income Statement','^EBITDA$',annual=TRUE),
 arr=atom('Income Statement',c('^Annual Recurring Revenue$','^ARR$'),annual=TRUE)
 )
}
ynow_integrity_facts <- function(sources,period,context=list()) {
 catalog<-ynow_fact_catalog();periods<-ynow_fiscal_periods(sources)
 memo<-new.env(parent=emptyenv())
 missing<-function(id,reason='missing')list(id=id,value=NA_real_,period=period,status=reason,kind='financial',currency='',lineage=list())
 atom<-function(id) {
  spec<-catalog[[id]]
  for(source in sources) {
   if(nzchar(context$ticker %||% '')&&nzchar(source$ticker %||% '')&&!identical(toupper(context$ticker),source$ticker))next
   if(isTRUE(spec$annual)&&!ynow_source_is_annual(source,spec$table))next
   d<-ynow_source_frame(source,spec$table);cols<-ynow_period_columns(source,spec$table,period,periods)
   if(!is.data.frame(d)||!length(cols))next
   ccy<-toupper(as.character(attr(d,'money_ccy') %||% attr(d,'financialCurrency') %||% source$currency %||% '')[1])
   target<-toupper(as.character(if(identical(spec$unit,'quote_per_share'))
    context$quote_currency %||% ccy else context$financial_currency %||% ccy)[1])
   if(is.na(ccy))ccy<-''
   if(is.na(target))target<-''
   if(spec$unit!='shares'&&nzchar(target)&&(!nzchar(ccy)||target!=ccy))next
   labels<-trimws(as.character(d[[1]]),whitespace='[\\h\\v]')
   for(pattern in spec$patterns)for(row in grep(pattern,labels,ignore.case=TRUE))for(col in cols) {
    v<-parse_financial_number(as.character(d[row,col]))[1]
    if(is.finite(v))return(list(id=id,value=v,period=period,status='resolved',kind='financial',unit=spec$unit,currency=ccy,
     lineage=list(list(source=source$origin,table=spec$table,field=labels[row],source_period=names(d)[col],period=period,currency=ccy))))
   }
  }
  missing(id)
 }
 derived<-function(id,deps,fn,kind='financial') {
  rows<-lapply(deps,get);values<-vapply(rows,function(x)x$value,numeric(1))
  if(any(!is.finite(values)))return(missing(id,paste0('missing:',paste(deps[!is.finite(values)],collapse=','))))
  ccy<-unique(vapply(rows,function(x)x$currency %||% '',character(1)));ccy<-ccy[nzchar(ccy)]
  if(length(ccy)>1)return(missing(id,'currency_mismatch'))
  v<-tryCatch(fn(values),error=function(e)NA_real_)
  if(!is.finite(v))return(missing(id,'invalid'))
  list(id=id,value=unname(v),period=period,status='resolved',kind=kind,currency=if(length(ccy))ccy[1] else '',
   lineage=unlist(lapply(rows,function(x)x$lineage),recursive=FALSE))
 }
 get<-function(id) {
  if(exists(id,memo,inherits=FALSE))return(memo[[id]])
  manual<-context$manual[[id]]
  if(is.list(manual)&&identical(manual$period,period)&&
     (!nzchar(context$ticker %||% '')||identical(toupper(manual$ticker %||% ''),toupper(context$ticker)))&&
     length(manual$value)==1L&&is.finite(manual$value)) {
   r<-list(id=id,value=manual$value,period=period,status='resolved',kind='manual',currency=context$financial_currency %||% '',lineage=list(manual))
   memo[[id]]<-r;return(r)
  }
  if(id %in% names(catalog))r<-atom(id) else r<-switch(id,
   common_book={r<-get('common_equity');if(!is.finite(r$value))r<-derived(id,c('stockholder_equity','preferred_equity'),function(v)v[1]-v[2]);r},
   outstanding={r<-get('shares');if(!is.finite(r$value))r<-derived(id,c('issued_shares','treasury_shares'),function(v)if(v[1]>v[2])v[1]-v[2] else NA_real_);r},
   da={r<-get('da_total');if(!is.finite(r$value))r<-get('da_reconciled');if(!is.finite(r$value))r<-derived(id,c('depreciation','amortization'),sum);r},
   tax=derived(id,c('tax_expense','pretax'),function(v)if(v[2]>0&&v[1]>=0&&v[1]<=v[2])v[1]/v[2] else NA_real_),
   nopat={tax<-get('tax');if(is.finite(tax$value))derived(id,c('ebit','tax'),function(v)v[1]*(1-v[2])) else {
    e<-get('ebit');t<-suppressWarnings(as.numeric(context$tax_assumption)[1])
    if(is.finite(e$value)&&length(t)==1L&&is.finite(t)&&t>=0&&t<=1){e$value<-e$value*(1-t);e$kind<-'assumption_derived';e$lineage<-c(e$lineage,list(list(source='forecast_assumption',parameter='tax',value=t,period=period)));e}else missing(id)
   }},
   capex=derived(id,'capital_expenditure',abs),
   delta_nwc=derived(id,'nwc_cash_change',function(v)-v[1]),
   operating_ic=derived(id,c('assets','cash','nonoperating_book','liabilities','debt'),function(v)v[1]-v[2]-v[3]-(v[4]-v[5])),
   b0=derived(id,c('common_book','outstanding'),function(v)if(v[2]>0)v[1]/v[2] else NA_real_),
   tbvps=derived(id,c('common_book','goodwill','intangibles','outstanding'),function(v)if(v[4]>0)(v[1]-v[2]-v[3])/v[4] else NA_real_),
   navps={r<-get('b0');r},
   eps={r<-get('diluted_eps');if(!is.finite(r$value))r<-get('basic_eps');if(!is.finite(r$value)){
    d<-get('weighted_diluted');key<-if(is.finite(d$value))'weighted_diluted' else 'weighted_basic'
    r<-derived(id,c('common_income',key),function(v)if(v[2]>0)v[1]/v[2] else NA_real_)
   };r},
   d0={r<-get('dps_reported');if(!is.finite(r$value))r<-get('dps_events');r},
   ebitda={r<-get('ebitda_reported');if(!is.finite(r$value))r<-derived(id,c('ebit','da'),sum);r},
   other_claims=derived(id,c('gross_equity','common_book'),function(v)if(abs(v[1]-v[2])<max(1,abs(v[2]))*1e-8)0 else NA_real_),
   fcff_cfo=derived(id,c('cfo','interest','tax','capex'),function(v)v[1]+v[2]*(1-v[3])-v[4]),
   missing(id,'unknown_parameter'))
  r$id<-id;memo[[id]]<-r;r
 }
 ids<-c(names(catalog),'common_book','outstanding','da','tax','nopat','capex','delta_nwc','operating_ic','b0','tbvps','navps','eps','d0','ebitda','other_claims','fcff_cfo')
 setNames(lapply(ids,get),ids)
}
ynow_model_financial_requirements <- function(model,config=list()) {
 bridge<-c('cash','debt','outstanding','other_claims')
 switch(model,
  dcf=c('revenue','nopat','da','capex','delta_nwc',
   if(identical(config$claim,'fcfe'))c('debt','outstanding','interest','tax') else
   if(identical(config$claim,'all'))c(bridge,'interest','tax') else bridge),
  ddm=c('d0',if((config$mode %||% '') %in% c('spm','all'))'eps'),
  ri='b0',pb=if(identical(config$basis,'all'))c('b0','tbvps','navps') else if(identical(config$basis,'tbvps'))'tbvps' else if(identical(config$basis,'navps'))'navps' else 'b0',
  nav=c('navps',if(is.finite(config$holdco_discount %||% NA_real_)&&config$holdco_discount>0)'investments'),
  multiples=switch(config$method %||% 'pe',all=unique(c('eps','revenue','outstanding','fcff_cfo',bridge,'ebit','ebitda','arr')),pe='eps',forward_pe='eps',ps=c('revenue','outstanding'),
   ev_fcf=c('fcff_cfo',bridge),ev_ebit=c('ebit',bridge),ev_ebitda=c('ebitda',bridge),ev_sales=c('revenue',bridge),ev_arr=c('arr',bridge),stop('Unknown multiples method')),
  sotp=c('segments',bridge),stop('Unknown valuation model'))
}
ynow_fiscal_segments <- function(sources,period,context=list()) {
 periods<-ynow_fiscal_periods(sources)
 for(s in sources)for(table in c('Segments','Income Statement')) {
  if(!ynow_source_is_annual(s,table))next
  d<-ynow_source_frame(s,table);cols<-ynow_period_columns(s,table,period,periods)
  if(!is.data.frame(d)||!length(cols))next
  ccy<-toupper(as.character(attr(d,'money_ccy') %||% attr(d,'financialCurrency') %||% s$currency %||% '')[1])
  target<-toupper(context$financial_currency %||% ccy)
  if(is.na(ccy))ccy<-''
  if(is.na(target))target<-''
  if(nzchar(target)&&(!nzchar(ccy)||ccy!=target))next
  if(nzchar(context$ticker %||% '')&&nzchar(s$ticker %||% '')&&!identical(toupper(context$ticker),s$ticker))next
  labels<-as.character(d[[1]]);rows<-if(table=='Segments')seq_len(nrow(d)) else grep('^Segment .+ Revenue$',labels)
  if(length(rows)<2L)next
  values<-parse_financial_number(as.character(d[rows,cols[1]]));ok<-is.finite(values)&values>0
  if(sum(ok)>=2L)return(data.frame(name=labels[rows][ok],revenue=values[ok],period=period,source=s$origin,stringsAsFactors=FALSE))
 }
 data.frame(name=character(),revenue=numeric(),period=character(),source=character())
}
ynow_model_snapshot <- function(sources,period,model,config=list(),context=list()) {
 facts<-ynow_integrity_facts(sources,period,context)
 discount<-suppressWarnings(as.numeric(config$holdco_discount %||% 0)[1])
 if((identical(model,'nav')||(identical(model,'pb')&&identical(config$basis,'navps')))&&
    is.finite(discount)&&discount>0) {
  deps<-facts[c('common_book','investments','outstanding')]
  values<-vapply(deps,function(x)x$value,numeric(1))
  if(all(is.finite(values))&&values[3]>0) {
   facts$navps<-list(id='navps',value=(values[1]-discount*values[2])/values[3],
    period=period,status='resolved',kind='financial',currency=deps[[1]]$currency,
    lineage=unlist(lapply(deps,function(x)x$lineage),recursive=FALSE))
  } else {
   facts$navps<-list(id='navps',value=NA_real_,period=period,status='missing',
    kind='financial',currency='',lineage=list())
  }
 }
 seg<-ynow_fiscal_segments(sources,period,context)
 facts$segments<-list(id='segments',value=if(nrow(seg)>=2L)nrow(seg) else NA_real_,period=period,status=if(nrow(seg)>=2L)'resolved' else 'missing',kind='financial',lineage=as.list(seg))
 required<-unique(ynow_model_financial_requirements(model,config))
 rows<-do.call(rbind,lapply(required,function(id){r<-facts[[id]];data.frame(model=model,period=period,parameter=id,value=r$value,status=r$status,kind=r$kind,stringsAsFactors=FALSE)}))
 missing<-rows[!is.finite(rows$value)|!rows$status %in% c('resolved','assumed'),,drop=FALSE]
 list(model=model,period=period,complete=nrow(missing)==0L,parameters=rows,missing=missing,facts=facts,segments=seg,config=config)
}
ynow_audit_model_years <- function(sources,models=c('dcf','ddm','ri','pb','nav','multiples','sotp'),periods=ynow_fiscal_periods(sources),configs=list(),context=list()) {
 out<-list()
 for(period in periods)for(model in models)out[[paste(model,period,sep='@')]]<-ynow_model_snapshot(sources,period,model,configs[[model]] %||% list(),context)
 out
}
ynow_formula_specs <- function() list(
 dcf_fcff=list(model='dcf',config=list(claim='fcff')),
 dcf_fcfe=list(model='dcf',config=list(claim='fcfe')),
 ddm_gordon=list(model='ddm',config=list(mode='gordon')),
 ddm_spm=list(model='ddm',config=list(mode='spm')),ri=list(model='ri'),
 pb_bvps=list(model='pb',config=list(basis='bvps')),
 pb_tbvps=list(model='pb',config=list(basis='tbvps')),
 pb_navps=list(model='pb',config=list(basis='navps')),
 nav=list(model='nav'),
 multiples_pe=list(model='multiples',config=list(method='pe')),
 multiples_forward_pe=list(model='multiples',config=list(method='forward_pe')),
 multiples_ps=list(model='multiples',config=list(method='ps')),
 multiples_ev_fcf=list(model='multiples',config=list(method='ev_fcf')),
 multiples_ev_ebit=list(model='multiples',config=list(method='ev_ebit')),
 multiples_ev_ebitda=list(model='multiples',config=list(method='ev_ebitda')),
 multiples_ev_sales=list(model='multiples',config=list(method='ev_sales')),
 multiples_ev_arr=list(model='multiples',config=list(method='ev_arr')),
 sotp=list(model='sotp'))
ynow_audit_formula_years <- function(sources,periods=ynow_fiscal_periods(sources),context=list()) {
 out<-list()
 for(period in periods)for(name in names(ynow_formula_specs())) {
  spec<-ynow_formula_specs()[[name]]
  snap<-ynow_model_snapshot(sources,period,spec$model,spec$config %||% list(),context)
  snap$formula<-name;snap$parameters$model<-name
  if(nrow(snap$missing))snap$missing$model<-name
  out[[paste(name,period,sep='@')]]<-snap
 }
 out
}
ynow_recover_model_inputs <- function(sources,periods,providers,configs=list(),context=list(),models=c('dcf','ddm','ri','pb','nav','multiples','sotp')) {
 if(!length(configs))configs<-list(dcf=list(claim='all'),ddm=list(mode='all'),
  pb=list(basis='all'),multiples=list(method='all'))
 before<-ynow_audit_model_years(sources,models,periods,configs,context)
 gaps<-do.call(rbind,lapply(before,function(x)x$missing))
 attempts<-list()
 if(!length(periods)||(!is.null(gaps)&&nrow(gaps)))for(provider in names(providers)) {
  request<-list(periods=unique(gaps$period),models=unique(gaps$model),parameters=unique(gaps$parameter))
  result<-tryCatch(providers[[provider]](request),error=function(e)e)
  attempts[[provider]]<-list(request=request,error=if(inherits(result,'error'))conditionMessage(result) else NULL)
  if(!inherits(result,'error')&&!is.null(result)) {
   if(!is.null(result$data)&&!is.null(result$origin))sources<-c(list(result),sources)
   else if(is.list(result)&&length(result)&&all(vapply(result,function(x)is.list(x)&&!is.null(x$data)&&!is.null(x$origin),logical(1))))sources<-c(result,sources)
  }
  if(!length(periods))periods<-ynow_fiscal_periods(sources)
  audit<-ynow_audit_model_years(sources,models,periods,configs,context)
  gaps<-do.call(rbind,lapply(audit,function(x)x$missing))
  if(length(periods)&&(is.null(gaps)||!nrow(gaps)))break
 }
 list(sources=sources,before=before,after=ynow_audit_model_years(sources,models,periods,configs,context),attempts=attempts)
}
# Materialize a model-only view. Financial tables on the Dashboard retain the
# original raw payload; calculations receive only the selected fiscal period.
ynow_fiscal_frame <- function(sources,table,period,context=list()) {
 periods<-ynow_fiscal_periods(sources);values<-list();lineage<-list()
 for(source in sources) {
  d<-ynow_source_frame(source,table);cols<-ynow_period_columns(source,table,period,periods)
  if(!is.data.frame(d)||!length(cols))next
  # Interim cumulative flows are not annual financial facts.
  if(table %in% c('Income Statement','Cash Flow')&&!ynow_source_is_annual(source,table))next
  ccy<-as.character(attr(d,'money_ccy') %||% attr(d,'financialCurrency') %||% source$currency %||% '')[1]
  target<-context$financial_currency %||% ccy
  if(is.na(ccy))ccy<-''
  if(is.na(target))target<-''
  if(nzchar(target)&&(!nzchar(ccy)||toupper(ccy)!=toupper(target)))next
  for(row in seq_len(nrow(d))) {
   key<-trimws(as.character(d[[1]][row]),whitespace='[\\h\\v]')
   v<-parse_financial_number(as.character(d[row,cols[1]]))[1]
   if(is.null(values[[key]])||(!is.finite(values[[key]])&&is.finite(v))) {
    values[[key]]<-v;lineage[[key]]<-list(source=source$origin,field=key,period=period,source_period=names(d)[cols[1]])
   }
  }
 }
 facts<-ynow_integrity_facts(sources,period,context)
 canonical<-switch(table,
  'Income Statement'=c(revenue='Total Revenue',ebit='Operating Income',pretax='Pretax Income',tax_expense='Tax Provision',interest='Interest Expense',common_income='Net Income Common Stockholders',diluted_eps='Diluted EPS',basic_eps='Basic EPS',weighted_diluted='Diluted Average Shares',weighted_basic='Basic Average Shares',ebitda='EBITDA',dps_reported='Annual Dividends Per Share'),
  'Cash Flow'=c(da='Depreciation And Amortization',capital_expenditure='Capital Expenditure',nwc_cash_change='Change In Working Capital',cfo='Operating Cash Flow',common_dividends='Common Stock Dividend Paid'),
  'Balance Sheet'=c(common_book='Common Stock Equity',outstanding='Ordinary Shares Number',cash='Cash And Cash Equivalents',debt='Total Debt',assets='Total Assets',liabilities='Total Liabilities Net Minority Interest',nonoperating_book='Non Operating Assets'),character())
 for(id in names(canonical)){label<-canonical[[id]];values[[label]]<-facts[[id]]$value;lineage[[label]]<-facts[[id]]$lineage}
 d<-data.frame(Breakdown=names(values),check.names=FALSE,stringsAsFactors=FALSE)
 d[[period]]<-if(length(values))unname(unlist(values)) else numeric()
 attr(d,'valuation_fiscal_period')<-period
 attr(d,'financialCurrency')<-context$financial_currency %||% ''
 attr(d,'money_ccy')<-context$financial_currency %||% ''
 attr(d,'parameter_lineage')<-lineage
 d
}
ynow_integrity_binding <- function(model,facts,financial_currency,quote_currency,analysis_currency,fx,quote_share_factor=1) {
 value<-function(id)facts[[id]]$value %||% NA_real_
 analysis<-function(v)money_to_session(v,financial_currency,analysis_currency,fx)
 quote<-function(v)money_to_session(v,financial_currency,quote_currency,fx)
 # Per-quote share values require an explicitly verified unit conversion.
 per_share<-function(v)if(is.finite(quote_share_factor))quote(v)*quote_share_factor else NA_real_
 per_share_id<-function(id) {
  row<-facts[[id]]
  if(identical(row$unit,'quote_per_share'))return(row$value)
  per_share(row$value)
 }
 shares<-if(is.finite(quote_share_factor)&&quote_share_factor>0)value('outstanding')/quote_share_factor else NA_real_
 switch(model,
  dcf=c(`mod_fcf-fcf_revenue`=analysis(value('revenue')),`mod_fcf-fcf_nopat`=analysis(value('nopat')),`mod_fcf-fcf_depreciation`=analysis(value('da')),`mod_fcf-fcf_capex`=analysis(value('capex')),`mod_fcf-fcf_delta_nwc`=analysis(value('delta_nwc')),bridge_cash=analysis(value('cash')),bridge_debt=analysis(value('debt')),bridge_revenue=analysis(value('revenue')),bridge_shares=shares,bridge_other_claims=analysis(value('other_claims'))),
  ddm=c(`mod_ddm-d0`=per_share_id('d0'),`mod_ddm-est_eps`=per_share_id('eps')),
  ri=c(`mod_ri-b0`=per_share(value('b0'))),
  pb=c(`mod_pb-bvps`=per_share(value('b0')),`mod_pb-tbvps`=per_share(value('tbvps')),`mod_pb-navps`=per_share(value('navps'))),
  nav=c(`mod_nav-navps`=per_share(value('navps'))),
  multiples=c(`mod_rel-trailing_eps`=per_share(value('eps')),`mod_rel-fcff`=analysis(value('fcff_cfo')),`mod_rel-ebit`=analysis(value('ebit')),`mod_rel-ebitda`=analysis(value('ebitda')),`mod_rel-arr`=analysis(value('arr')),`mod_rel-revenue`=analysis(value('revenue')),`mod_rel-ps_revenue`=analysis(value('revenue')),`mod_rel-cash`=analysis(value('cash')),`mod_rel-debt`=analysis(value('debt')),`mod_rel-shares`=shares),
  sotp=c(`mod_sotp-cash`=analysis(value('cash')),`mod_sotp-debt`=analysis(value('debt')),`mod_sotp-shares`=shares),numeric())
}
ynow_binding_matches <- function(input,expected,ids=names(expected),tolerance=1e-6) {
 all(vapply(intersect(names(expected),ids),function(id){a<-suppressWarnings(as.numeric(input[[id]])[1]);b<-expected[[id]];is.finite(a)&&is.finite(b)&&abs(a-b)<=max(0.011,max(1,abs(b))*tolerance)},logical(1)))
}
# Match the financial inputs actually consumed by the selected formula. Forecast
# rates and multiples are validated by their engines and disclosed separately.
ynow_required_binding_ids <- function(model,config=list()) {
 switch(model,
  dcf=c('mod_fcf-fcf_revenue','mod_fcf-fcf_nopat','mod_fcf-fcf_depreciation','mod_fcf-fcf_capex','mod_fcf-fcf_delta_nwc','bridge_cash','bridge_debt','bridge_shares','bridge_other_claims'),
  ddm=c('mod_ddm-d0',if(identical(config$mode,'spm'))'mod_ddm-est_eps'),
  ri='mod_ri-b0',pb=paste0('mod_pb-',config$basis %||% 'bvps'),nav='mod_nav-navps',
  multiples=switch(config$method %||% 'pe',pe='mod_rel-trailing_eps',forward_pe='mod_rel-trailing_eps',
   ps=c('mod_rel-ps_revenue','mod_rel-shares'),ev_fcf=c('mod_rel-fcff','mod_rel-cash','mod_rel-debt','mod_rel-shares'),
   ev_ebit=c('mod_rel-ebit','mod_rel-cash','mod_rel-debt','mod_rel-shares'),ev_ebitda=c('mod_rel-ebitda','mod_rel-cash','mod_rel-debt','mod_rel-shares'),
   ev_sales=c('mod_rel-revenue','mod_rel-cash','mod_rel-debt','mod_rel-shares'),ev_arr=c('mod_rel-arr','mod_rel-cash','mod_rel-debt','mod_rel-shares'),character()),
  sotp=c('mod_sotp-cash','mod_sotp-debt','mod_sotp-shares'),character())
}
