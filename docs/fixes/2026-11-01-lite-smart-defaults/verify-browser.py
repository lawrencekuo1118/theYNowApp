from playwright.sync_api import sync_playwright
import json,time,os,urllib.request
url=os.environ.get('VERIFY_URL','http://127.0.0.1:3848/')
expected=os.environ.get('EXPECTED_VERSION','')
if expected:
 build=json.load(urllib.request.urlopen(url+'ynow_build.json?t='+str(time.time()),timeout=60))
 assert build['version']==expected,build
 print(json.dumps({'build':build},ensure_ascii=False),flush=True)
with sync_playwright() as p:
 b=p.chromium.launch(executable_path='/usr/bin/chromium',args=['--no-sandbox'])
 ctx=b.new_context(ignore_https_errors=True)
 ctx.add_init_script("sessionStorage.setItem('ynow_lite_mode','1')")
 page=ctx.new_page();errors=[];page.on('pageerror',lambda e:errors.append(str(e)))
 page.goto(url,wait_until='domcontentloaded',timeout=120000)
 page.wait_for_function('window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()',timeout=120000)
 page.evaluate("Shiny.setInputValue('market_mode_pick','TW',{priority:'event'})")
 page.wait_for_timeout(3000)
 locale=os.environ.get('VERIFY_LOCALE','en')
 page.evaluate('(loc)=>Shiny.setInputValue("ui_locale_pick",loc,{priority:"event"})',locale)
 page.evaluate("Shiny.setInputValue('sc','7772-TWO',{priority:'event'});Shiny.setInputValue('search',1,{priority:'event'});window.ynowGotoTab('smart_analysis')")
 done=False
 for i in range(30):
  page.wait_for_timeout(5000)
  smart=page.locator('#smart_analysis_summary').inner_text() if page.locator('#smart_analysis_summary').count() else ''
  notices=page.locator('#shiny-notification-panel').inner_text() if page.locator('#shiny-notification-panel').count() else ''
  row={'elapsed':(i+1)*5,'lite':page.locator('body').evaluate('(el)=>el.classList.contains("ynow-lite")'),'summary':smart,'errors':errors,'output_errors':page.locator('.shiny-output-error').all_text_contents(),'notifications':notices,'inputs':page.evaluate('Object.fromEntries(Object.entries(Shiny.shinyapp.$inputValues).filter(([k])=>["mod_ri-ri_payout","mod_ri-ri_roe","mod_ri-ri_ke","mod_ri-ri_g","mod_ri-b0"].some(id=>k.split(":")[0]===id)))')}
  print(json.dumps(row,ensure_ascii=False),flush=True)
  if ('40' in smart and ('Assumed value applied by Lite' in smart or 'Lite 已套用假設數值' in smart) and page.locator('#smart_analysis_summary .ynow-smart-card-value').count()):
   vals=page.locator('#smart_analysis_summary .ynow-smart-card-value').all_text_contents()
   if vals[0]!='—' and '🚀' not in notices:
    assert row['lite'] and not errors and not row['output_errors'],row
    assert 'ynow_missing_parameters' not in page.locator('#shiny-notification-panel').inner_html()
    done=True;break
 page.screenshot(path='/workspace/scratch/lite-smart.png')
 assert done,'Lite did not calculate automatically'
 b.close()
print('PASS: actual Lite guest Taiwan 7772-TWO automatic RI result and disclosed payout assumption',flush=True)
