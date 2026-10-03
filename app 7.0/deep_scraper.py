import time
import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager

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
            print(f"🚀 正在開啟並展開報表: {name}")
            driver.get(url)
            wait = WebDriverWait(driver, 15)
            
            # 1. 處理可能的 Cookie 彈窗
            try:
                consent_btn = driver.find_elements(By.XPATH, "//button[contains(@class, 'agree')]")
                if consent_btn:
                    consent_btn[0].click()
                    time.sleep(1)
            except:
                pass

            # 🟢 2. 關鍵步驟：尋找並點擊「Expand All」
            try:
                # Yahoo Finance 的 Expand All 通常是一個帶有文字的按鈕或 div
                expand_btn = wait.until(EC.element_to_be_clickable((By.XPATH, "//span[contains(text(), 'Expand All')] | //button[.//span[contains(text(), 'Expand All')]]")))
                driver.execute_script("arguments[0].click();", expand_btn) # 使用 JS 點擊較穩定
                print(f"✅ 已成功點擊 Expand All")
                time.sleep(2) # 等待動畫展開
            except Exception as e:
                print(f"⚠️ 找不到 Expand All 按鈕或已是展開狀態: {e}")

            # 3. 抓取表頭 (對齊 setup 5.0.R 邏輯)
            wait.until(EC.presence_of_element_located((By.XPATH, "//div[contains(@class, 'tableHeader')]")))
            header_el = driver.find_element(By.XPATH, "//div[contains(@class, 'tableHeader')]//div[contains(@class, 'row')]")
            headers = [col.text for col in header_el.find_elements(By.XPATH, ".//div[contains(@class, 'column')]") if col.text]

            # 4. 抓取資料列
            rows_elements = driver.find_elements(By.XPATH, "//div[contains(@class, 'tableBody')]//div[contains(@class, 'row')]")
            
            table_data = []
            for row in rows_elements:
                cols = row.find_elements(By.XPATH, ".//div[contains(@class, 'column')]")
                row_text = [c.text.strip() for c in cols]
                if row_text and row_text[0]:
                    table_data.append(row_text)

            if table_data:
                all_results[name] = pd.DataFrame(table_data, columns=headers)
        
        return all_results

    except Exception as e:
        print(f"❌ 深度抓取失敗: {e}")
        return None
    finally:
        driver.quit()
        
