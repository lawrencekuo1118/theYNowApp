from playwright.sync_api import sync_playwright
import json,time,urllib.request
url='https://hopesmasher1118.shinyapps.io/TheYNowApp/'
with sync_playwright() as p:
 browser=p.chromium.launch(executable_path='/usr/bin/chromium',args=['--no-sandbox'])
 for ticker,expected in [('2330','台灣積體電路製造股份有限公司'),('7772-TWO','耀穎光電股份有限公司')]:
  context=browser.new_context(ignore_https_errors=True)
  page=context.new_page();errors=[];page.on('pageerror',lambda e:errors.append(str(e)))
  page.goto(url,wait_until='domcontentloaded',timeout=120000)
  page.wait_for_function('window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()',timeout=120000)
  build=json.load(urllib.request.urlopen(url+'ynow_build.json?t='+str(time.time()),timeout=60))
  assert build['version']=='v27.60',build
  page.evaluate("Shiny.setInputValue('market_mode_pick','TW',{priority:'event'}); if(window.ynowGotoTab)window.ynowGotoTab('dashboard'); Shiny.setInputValue('sidebar_tabs','dashboard',{priority:'event'}); Shiny.setInputValue('dashboard_fin_report','Finance Summary',{priority:'event'});")
  page.wait_for_timeout(3000)
  page.evaluate('(tk)=>{Shiny.setInputValue("sc",tk,{priority:"event"}); Shiny.setInputValue("search",1,{priority:"event"})}',ticker)
  start=time.monotonic();done=False
  for n in range(36):
   page.wait_for_timeout(5000)
   name=page.locator('#txt_corpname').inner_text() if page.locator('#txt_corpname').count() else ''
   notices=page.locator('#shiny-notification-panel').inner_text() if page.locator('#shiny-notification-panel').count() else ''
   summary=page.locator('#fs_summary_ui').inner_text() if page.locator('#fs_summary_ui').count() else ''
   row={'ticker':ticker,'elapsed_sec':round(time.monotonic()-start,1),'company':name,'connected':page.evaluate('Shiny.shinyapp.isConnected()'),'js_errors':errors,'output_errors':page.locator('.shiny-output-error').all_text_contents(),'has_summary':bool(summary and 'No finance summary' not in summary),'search_progress_open':'🚀' in notices,'notifications':notices}
   print(json.dumps(row,ensure_ascii=False),flush=True)
   if expected in name and row['has_summary'] and not row['search_progress_open']:
    assert row['connected'] and not errors and not row['output_errors'],row
    assert 'Data retrieval failed' not in notices,row
    done=True;break
  page.screenshot(path='/workspace/scratch/tw-search/production-'+ticker+'.png')
  assert done,'Search did not complete for '+ticker
  context.close()
 browser.close()
print('ALL PRODUCTION TAIWAN SEARCH CHECKS PASSED',flush=True)
