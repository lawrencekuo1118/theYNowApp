import time
import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager

import yfinance as yf

def fast_get_company_info(ticker="AMZN"):
    print(f"⚡ 使用高速 API 獲取 {ticker} 公司與產業資訊...")
    try:
        stock = yf.Ticker(ticker)
        info = stock.info
        
        # 抓取短名，若無則抓長名，再沒有就退回股票代碼
        comp_name = info.get("shortName", info.get("longName", ticker))
        sector = info.get("sector", "Unknown Sector")
        industry = info.get("industry", "Unknown Industry")
        
        return {
            "company_name": comp_name,
            "sector": sector,
            "industry": industry
        }
    except Exception as e:
        print(f"⚠️ 獲取公司資訊失敗: {e}")
        return {
            "company_name": ticker,
            "sector": "N/A",
            "industry": "N/A"
        }
        
def scrape_all_financials(ticker="AMZN"):
    chrome_options = Options()
    chrome_options.add_argument("--headless") 
    chrome_options.add_argument("--disable-gpu")
    chrome_options.add_argument("--no-sandbox")
    chrome_options.add_argument("--disable-dev-shm-usage")
    chrome_options.add_argument("--disable-blink-features=AutomationControlled")
    chrome_options.add_argument("user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")

    driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=chrome_options)
    
    pages = {
        "Income Statement": f"https://finance.yahoo.com/quote/{ticker}/financials/",
        "Balance Sheet": f"https://finance.yahoo.com/quote/{ticker}/balance-sheet/",
        "Cash Flow": f"https://finance.yahoo.com/quote/{ticker}/cash-flow/"
    }
    
    all_results = {}

    try:
        for name, url in pages.items():
            print(f"🌐 正在處理 {name}: {url}")
            driver.get(url)
            wait = WebDriverWait(driver, 15)
            
            # --- 輔助函數：抓取當前畫面的表格 ---
            def extract_table():
                wait.until(EC.presence_of_element_located((By.XPATH, "//div[contains(@class, 'tableHeader')]")))
                header_el = driver.find_element(By.XPATH, "//div[contains(@class, 'tableHeader')]//div[contains(@class, 'row')]")
                headers = [col.text for col in header_el.find_elements(By.XPATH, ".//div[contains(@class, 'column')]") if col.text]
                
                rows_elements = driver.find_elements(By.XPATH, "//div[contains(@class, 'tableBody')]//div[contains(@class, 'row')]")
                table_data = []
                for row in rows_elements:
                    cols = row.find_elements(By.XPATH, ".//div[contains(@class, 'column')]")
                    row_text = [c.text.strip() for c in cols]
                    if row_text and row_text[0]:
                        table_data.append(row_text)
                        
                df = pd.DataFrame(table_data)
                if not df.empty and len(headers) == df.shape[1]:
                    df.columns = headers
                return df
            # -----------------------------------

            # 1. 抓取「點擊展開前」的簡易表
            print(f"📄 抓取 {name} (未展開版本)...")
            df_collapsed = extract_table()
            
            # 2. 點擊 Expand All
            try:
                expand_btn = wait.until(EC.element_to_be_clickable((By.XPATH, "//button[contains(text(), 'Expand All')] | //button[.//span[contains(text(), 'Expand All')]]")))
                driver.execute_script("arguments[0].click();", expand_btn)
                print(f"✅ 已成功點擊 Expand All")
                wait.until(EC.presence_of_element_located((By.XPATH, "//div[contains(@class, 'expanded-row-class-name') or @data-test='fin-row']")))
            except Exception as e:
                print(f"⚠️ 找不到 Expand All 按鈕或已是展開狀態")

            # 3. 抓取「點擊展開後」的完整表
            print(f"📄 抓取 {name} (已展開版本)...")
            df_expanded = extract_table()
            
            # 4. 將兩個版本存入字典
            all_results[name] = {
                "collapsed": df_collapsed,
                "expanded": df_expanded
            }
            
    except Exception as e:
        print(f"❌ 爬蟲發生錯誤: {e}")
    finally:
        driver.quit()
        
    return all_results
  
