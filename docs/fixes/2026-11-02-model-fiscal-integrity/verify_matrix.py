"""Actual app browser matrix using _fixture_lite_valuation_app.R providers.
Recommendations are forced to cover every model; valuation results are real.
This tests rendering/auto-calc, not live provider coverage or model selection.
"""
from playwright.sync_api import sync_playwright
import json,os,time,itertools
url=os.environ.get('VERIFY_URL','http://127.0.0.1:3852/')
folder=os.environ.get('VERIFY_EVIDENCE','/workspace/scratch/goog-lite/matrix')
os.makedirs(folder,exist_ok=True)
models=['dcf','ddm','ri','pb','nav','multiples','sotp']
contexts=[('US','en','mobile')] if os.environ.get('VERIFY_SMOKE')=='1' else itertools.product(['US','TW'],['en','zh-TW'],['mobile','desktop'])
state_js="""()=>{
 let node=document.getElementById('smart_analysis_chart');
 let plot=node?.querySelector('.js-plotly-plot')||node;
 let rect=node?.getBoundingClientRect();
 let summary=document.getElementById('smart_analysis_summary')?.innerText||'';
 return {diagnostic:document.getElementById('fixture_diag')?.innerText||'',summary,connected:Shiny.shinyapp.isConnected(),lite:document.body.classList.contains('ynow-lite'),
 output_errors:[...document.querySelectorAll('.shiny-output-error')].map(x=>x.innerText),
 notifications:document.getElementById('shiny-notification-panel')?.innerText||'',
 chart:{width:rect?.width||0,height:rect?.height||0,visible:!!node&&getComputedStyle(node).display!='none',
 svg:node?.querySelectorAll('svg.main-svg').length||0,
 traces:(plot?.data||[]).map(x=>({type:x.type,x:x.x,y:x.y}))},
 parameters:Object.fromEntries(Object.entries(Shiny.shinyapp.$inputValues).filter(([k])=>/^mod_(fcf|ddm|ri|pb|nav|rel|sotp)-/.test(k)))}
}"""
records=[]
with sync_playwright() as p:
 browser=p.chromium.launch(executable_path='/usr/bin/chromium',args=['--no-sandbox'])
 for market,locale,viewport in contexts:
  width=390 if viewport=='mobile' else 1440
  ctx=browser.new_context(ignore_https_errors=True,viewport={'width':width,'height':844 if viewport=='mobile' else 1000},is_mobile=viewport=='mobile',has_touch=viewport=='mobile')
  ctx.add_init_script("sessionStorage.setItem('ynow_lite_mode','1')")
  page=ctx.new_page();errors=[];page.on('pageerror',lambda e:errors.append(str(e)))
  page.goto(url,wait_until='domcontentloaded',timeout=120000)
  page.wait_for_function('window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()',timeout=120000)
  if market=='TW':
   page.evaluate('(market)=>Shiny.setInputValue("market_mode_pick",market,{priority:"event"})',market)
   page.wait_for_function('document.body.classList.contains("ynow-market-tw")',timeout=30000)
  page.wait_for_timeout(2000)
  page.evaluate('(locale)=>Shiny.setInputValue("ui_locale_pick",locale,{priority:"event"})',locale)
  page.evaluate('window.ynowGotoTab("smart_analysis")')
  page.wait_for_selector('#smart_analysis_chart',timeout=30000)
  page.wait_for_timeout(500)
  for n,model in enumerate(models,1):
   page.evaluate('(model)=>Shiny.setInputValue("ynow_fixture_model",model,{priority:"event"})',model)
   page.locator('#sc').fill('2330' if market=='TW' else 'GOOG')
   page.wait_for_timeout(300)
   page.locator('#search').click()
   page.wait_for_timeout(500)
   expected={'dcf':'DCF','ddm':'DDM','ri':'RI','pb':'P/B','nav':'NAV','multiples':'Implied','sotp':'SOTP'}[model]
   start=time.monotonic();valid=False
   for attempt in range(60):
    page.wait_for_timeout(500)
    state=page.evaluate(state_js)
    traces=state['chart']['traces']
    labels=[str(x) for tr in traces for x in (tr.get('x') or [])]
    values=[v for tr in traces for v in (tr.get('y') or []) if isinstance(v,(int,float))]
    try:
     diagnostic=json.loads(state['diagnostic'])
    except (ValueError,TypeError):
     diagnostic={}
    settled=diagnostic.get('busy') is False and diagnostic.get('fcf_ready') is True
    if settled and any(expected.lower() in x.lower() for x in labels) and len(values)>=2 and '🚀' not in state['notifications']:
     valid=True;break
   row={'market':market,'locale':locale,'viewport':viewport,'model':model,'elapsed':round(time.monotonic()-start,2),'pass':valid,'js_errors':list(errors),**state}
   row['pass']=valid and state['connected'] and state['lite'] and not errors and not state['output_errors'] and state['chart']['width']>100 and state['chart']['height']>100 and state['chart']['width']<=width+2 and state['chart']['svg']>0
   records.append(row)
   print(json.dumps(row,ensure_ascii=False),flush=True)
   if not row['pass']:
    page.screenshot(path=folder+'/'+market+'-'+locale+'-'+viewport+'-'+model+'-FAIL.png',full_page=True)
    raise AssertionError('Chart failed: '+str((market,locale,viewport,model)))
   if model=='sotp':
    labels=[str(x) for tr in state['chart']['traces'] for x in tr.get('x',[])]
    assert not any('SOTP' in x and x.endswith(' FV') for x in labels),'SOTP mislabeled as Fair Value'
   page.locator('#smart_analysis_chart').scroll_into_view_if_needed()
   page.screenshot(path=folder+'/'+market+'-'+locale+'-'+viewport+'-'+model+'.png',full_page=True)
  ctx.close()
 browser.close()
print(json.dumps({'total':len(records),'passed':sum(r['pass'] for r in records)},ensure_ascii=False),flush=True)
