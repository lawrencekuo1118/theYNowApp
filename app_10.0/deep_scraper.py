"""
app_10.0 — cloud-compatible financials.
Primary: yfinance API (shinyapps.io / no Chrome).
Post-process to match app_9.0 Yahoo HTML shape: TTM column, B/M display,
Yahoo-like labels, expanded vs collapsed row sets.
Selenium remains an optional local fallback only.
"""
import re
import pandas as pd
import yfinance as yf


# Selenium is optional (usually unavailable on shinyapps.io)
try:
    from selenium import webdriver
    from selenium.webdriver.chrome.service import Service
    from selenium.webdriver.chrome.options import Options
    from selenium.webdriver.common.by import By
    from selenium.webdriver.support.ui import WebDriverWait
    from selenium.webdriver.support import expected_conditions as EC
    from webdriver_manager.chrome import ChromeDriverManager

    SELENIUM_AVAILABLE = True
except Exception:
    SELENIUM_AVAILABLE = False


# Yahoo Finance HTML (app_9.0) row orders — used to shape yfinance tables
_INCOME_EXPANDED = [
    "Total Revenue", "Operating Revenue", "Cost of Revenue", "Gross Profit",
    "Operating Expense", "Selling General and Administrative",
    "General & Administrative Expense", "Other G and A",
    "Selling & Marketing Expense", "Other Operating Expenses", "Operating Income",
    "Net Non Operating Interest Income Expense", "Interest Income Non Operating",
    "Interest Expense Non Operating", "Other Income Expense", "Gain on Sale of Security",
    "Other Non Operating Income Expenses", "Pretax Income", "Tax Provision",
    "Earnings from Equity Interest Net of Tax", "Net Income Common Stockholders",
    "Net Income", "Net Income Including Non-Controlling Interests",
    "Net Income Continuous Operations", "Diluted NI Available to Com Stockholders",
    "Basic EPS", "Diluted EPS", "Basic Average Shares", "Diluted Average Shares",
    "Total Operating Income as Reported", "Total Expenses",
    "Net Income from Continuing & Discontinued Operation", "Normalized Income",
    "Interest Income", "Interest Expense", "Net Interest Income", "EBIT", "EBITDA",
    "Reconciled Cost of Revenue", "Reconciled Depreciation",
    "Net Income from Continuing Operation Net Minority Interest",
    "Total Unusual Items Excluding Goodwill", "Total Unusual Items",
    "Normalized EBITDA", "Tax Rate for Calcs", "Tax Effect of Unusual Items",
]
_INCOME_COLLAPSED = [
    "Total Revenue", "Cost of Revenue", "Gross Profit", "Operating Expense",
    "Operating Income", "Net Non Operating Interest Income Expense",
    "Other Income Expense", "Pretax Income", "Tax Provision",
    "Earnings from Equity Interest Net of Tax", "Net Income Common Stockholders",
    "Diluted NI Available to Com Stockholders", "Basic EPS", "Diluted EPS",
    "Basic Average Shares", "Diluted Average Shares",
    "Total Operating Income as Reported", "Total Expenses",
    "Net Income from Continuing & Discontinued Operation", "Normalized Income",
    "Interest Income", "Interest Expense", "Net Interest Income", "EBIT", "EBITDA",
    "Reconciled Cost of Revenue", "Reconciled Depreciation",
    "Net Income from Continuing Operation Net Minority Interest",
    "Total Unusual Items Excluding Goodwill", "Total Unusual Items",
    "Normalized EBITDA", "Tax Rate for Calcs", "Tax Effect of Unusual Items",
]
_BALANCE_EXPANDED = [
    "Total Assets", "Current Assets",
    "Cash, Cash Equivalents & Short Term Investments", "Cash And Cash Equivalents",
    "Other Short Term Investments", "Receivables", "Accounts receivable",
    "Gross Accounts Receivable", "Allowance For Doubtful Accounts Receivable",
    "Inventory", "Other Inventories", "Inventories Adjustments Allowances",
    "Total non-current assets", "Net PPE", "Gross PPE", "Properties",
    "Land And Improvements", "Other Properties", "Construction in Progress",
    "Accumulated Depreciation", "Goodwill And Other Intangible Assets", "Goodwill",
    "Other Intangible Assets", "Other Non Current Assets",
    "Total Liabilities Net Minority Interest", "Current Liabilities",
    "Payables And Accrued Expenses", "Payables", "Accounts Payable",
    "Current Accrued Expenses", "Current Deferred Liabilities",
    "Current Deferred Revenue", "Total Non Current Liabilities Net Minority Interest",
    "Long Term Debt And Capital Lease Obligation", "Long Term Debt",
    "Long Term Capital Lease Obligation", "Other Non Current Liabilities",
    "Total Equity Gross Minority Interest", "Stockholders' Equity", "Capital Stock",
    "Preferred Stock", "Common Stock", "Additional Paid in Capital",
    "Retained Earnings", "Treasury Stock",
    "Gains Losses Not Affecting Retained Earnings", "Other Equity Adjustments",
    "Total Capitalization", "Common Stock Equity", "Capital Lease Obligations",
    "Net Tangible Assets", "Working Capital", "Invested Capital",
    "Tangible Book Value", "Total Debt", "Net Debt", "Share Issued",
    "Ordinary Shares Number", "Treasury Shares Number",
]
_BALANCE_COLLAPSED = [
    "Total Assets", "Total Liabilities Net Minority Interest",
    "Total Equity Gross Minority Interest", "Total Capitalization",
    "Common Stock Equity", "Capital Lease Obligations", "Net Tangible Assets",
    "Working Capital", "Invested Capital", "Tangible Book Value", "Total Debt",
    "Net Debt", "Share Issued", "Ordinary Shares Number", "Treasury Shares Number",
]
_CASH_EXPANDED = [
    "Operating Cash Flow", "Cash Flow from Continuing Operating Activities",
    "Net Income from Continuing Operations", "Depreciation Amortization Depletion",
    "Depreciation & amortization", "Deferred Tax", "Deferred Income Tax",
    "Stock based compensation", "Other non-cash items", "Change in working capital",
    "Change in Receivables", "Changes in Account Receivables", "Change in Inventory",
    "Change in Payables And Accrued Expense", "Change in Payable",
    "Change in Account Payable", "Change in Accrued Expense",
    "Change in Other Current Assets", "Change in Other Working Capital",
    "Investing Cash Flow", "Cash Flow from Continuing Investing Activities",
    "Net PPE Purchase And Sale", "Purchase of PPE", "Sale of PPE",
    "Net Business Purchase And Sale", "Purchase of Business",
    "Net Investment Purchase And Sale", "Purchase of Investment", "Sale of Investment",
    "Financing Cash Flow", "Cash Flow from Continuing Financing Activities",
    "Net Issuance Payments of Debt", "Net Long Term Debt Issuance",
    "Long Term Debt Issuance", "Long Term Debt Payments",
    "Net Short Term Debt Issuance", "Short Term Debt Issuance",
    "Short Term Debt Payments", "Net Common Stock Issuance", "Common Stock Payments",
    "Net Other Financing Charges", "End Cash Position", "Changes in Cash",
    "Effect of Exchange Rate Changes", "Beginning Cash Position",
    "Income Tax Paid Supplemental Data", "Interest Paid Supplemental Data",
    "Capital Expenditure", "Issuance of Debt", "Repayment of Debt",
    "Repurchase of Capital Stock", "Free Cash Flow",
]
_CASH_COLLAPSED = [
    "Operating Cash Flow", "Investing Cash Flow", "Financing Cash Flow",
    "End Cash Position", "Income Tax Paid Supplemental Data",
    "Interest Paid Supplemental Data", "Capital Expenditure", "Issuance of Debt",
    "Repayment of Debt", "Repurchase of Capital Stock", "Free Cash Flow",
]

# yfinance quirks that need an exact rename before fuzzy match
_LABEL_OVERRIDES = {
    "Diluted NI Availto Com Stockholders": "Diluted NI Available to Com Stockholders",
    "Other Gand A": "Other G and A",
    "Stockholders Equity": "Stockholders' Equity",
    "Cash Cash Equivalents And Short Term Investments": "Cash, Cash Equivalents & Short Term Investments",
    "Selling General And Administration": "Selling General and Administrative",
    "Depreciation And Amortization": "Depreciation & amortization",
    "Change In Working Capital": "Change in working capital",
    "Stock Based Compensation": "Stock based compensation",
    "Other Non Cash Items": "Other non-cash items",
}


def fast_get_company_info(ticker="AMZN"):
    print(f"⚡ 使用高速 API 獲取 {ticker} 公司與產業資訊...")
    try:
        stock = yf.Ticker(ticker)
        info = stock.info

        comp_name = info.get("shortName", info.get("longName", ticker))
        sector = info.get("sector", "Unknown Sector")
        industry = info.get("industry", "Unknown Industry")

        return {
            "company_name": comp_name,
            "sector": sector,
            "industry": industry,
        }
    except Exception as e:
        print(f"⚠️ 獲取公司資訊失敗: {e}")
        return {
            "company_name": ticker,
            "sector": "N/A",
            "industry": "N/A",
        }


def _fmt_num(v, digits=2):
    if v is None:
        return "N/A"
    try:
        if pd.isna(v):
            return "N/A"
    except Exception:
        pass
    try:
        fv = float(v)
        if abs(fv) >= 1e12:
            return f"{fv/1e12:.2f}T"
        if abs(fv) >= 1e9:
            return f"{fv/1e9:.2f}B"
        if abs(fv) >= 1e6:
            return f"{fv/1e6:.2f}M"
        if abs(fv) >= 1e3 and digits == 0:
            return f"{fv:,.0f}"
        return f"{fv:.{digits}f}"
    except Exception:
        return str(v)


def get_summary_quote(ticker="AMZN"):
    """Cloud-safe Yahoo summary metrics via yfinance (no Chromote/Chrome)."""
    print(f"📊 yfinance summary quote: {ticker}")
    stock = yf.Ticker(ticker)
    info = stock.info or {}

    company_name = info.get("shortName") or info.get("longName") or ticker
    day_low = info.get("dayLow")
    day_high = info.get("dayHigh")
    w52_low = info.get("fiftyTwoWeekLow")
    w52_high = info.get("fiftyTwoWeekHigh")
    div_rate = info.get("dividendRate")
    div_yield = info.get("dividendYield")

    rows = [
        ("Previous Close", _fmt_num(info.get("previousClose"))),
        ("Open", _fmt_num(info.get("open"))),
        ("Bid", _fmt_num(info.get("bid"))),
        ("Ask", _fmt_num(info.get("ask"))),
        ("Day's Range", f"{_fmt_num(day_low)} - {_fmt_num(day_high)}" if day_low is not None else "N/A"),
        ("52 Week Range", f"{_fmt_num(w52_low)} - {_fmt_num(w52_high)}" if w52_low is not None else "N/A"),
        ("Volume", _fmt_num(info.get("volume"), digits=0)),
        ("Avg. Volume", _fmt_num(info.get("averageVolume"), digits=0)),
        ("Market Cap (intraday)", _fmt_num(info.get("marketCap"), digits=0)),
        ("Beta (5Y Monthly)", _fmt_num(info.get("beta"))),
        ("PE Ratio (TTM)", _fmt_num(info.get("trailingPE"))),
        ("EPS (TTM)", _fmt_num(info.get("trailingEps"))),
        ("Dividend", _fmt_num(div_rate) if div_rate is not None else "N/A"),
        ("Yield", f"{float(div_yield)*100:.2f}%" if isinstance(div_yield, (int, float)) else "N/A"),
        ("Target Est", _fmt_num(info.get("targetMeanPrice"))),
    ]

    df = pd.DataFrame(rows, columns=["Item", "Value"])
    return {"company_name": company_name, "table": df}


def get_risk_free_rate_yf():
    """10Y Treasury yield (^TNX) via yfinance — no Chromote."""
    print("📊 yfinance Rf ^TNX")
    tnx = yf.Ticker("^TNX")
    try:
        hist = tnx.history(period="5d")
        if hist is not None and not hist.empty:
            return float(hist["Close"].dropna().iloc[-1])
    except Exception as e:
        print(f"history fail: {e}")
    info = tnx.info or {}
    for key in ("regularMarketPrice", "previousClose", "open"):
        v = info.get(key)
        if v is not None:
            try:
                return float(v)
            except Exception:
                pass
    raise RuntimeError("无法取得 ^TNX")


def _norm_label_key(s):
    s = str(s).lower().replace("&", " and ").replace("'", "")
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return " ".join(s.split())


def _fmt_yahoo_cell(v):
    """Match Yahoo Finance HTML magnitude style (e.g. 742.78B, 90.8B)."""
    if v is None:
        return ""
    try:
        if pd.isna(v):
            return ""
    except Exception:
        pass
    try:
        fv = float(v)
    except Exception:
        return str(v)

    sign = "-" if fv < 0 else ""
    af = abs(fv)

    def _trim(n):
        s = f"{n:.2f}".rstrip("0").rstrip(".")
        return s if s else "0"

    if af >= 1e12:
        return f"{sign}{_trim(af / 1e12)}T"
    if af >= 1e9:
        return f"{sign}{_trim(af / 1e9)}B"
    if af >= 1e6:
        return f"{sign}{_trim(af / 1e6)}M"
    if af >= 1e3:
        return f"{sign}{_trim(af / 1e3)}K"
    if af == 0:
        return "0"
    if af < 1:
        return f"{sign}{_trim(af)}"
    # shares / small integers: keep full figure with grouping
    if fv.is_integer():
        return f"{sign}{int(af):,}"
    return f"{sign}{_trim(af)}"


def _pick_attr(stock, *names):
    for name in names:
        df = getattr(stock, name, None)
        if callable(df):
            try:
                df = df()
            except Exception:
                df = None
        if df is not None and hasattr(df, "empty") and not df.empty:
            return df
    return pd.DataFrame()


def _merge_annual_ttm(annual_df, ttm_df, max_fy=4):
    """annual (metrics x FY) + ttm_* first column → metrics x [TTM + FY…]."""
    if (annual_df is None or annual_df.empty) and (ttm_df is None or ttm_df.empty):
        return pd.DataFrame()

    if annual_df is None or annual_df.empty:
        base = pd.DataFrame(index=ttm_df.index)
    else:
        base = annual_df.copy()
        try:
            fy_cols = sorted(list(base.columns), reverse=True)[:max_fy]
            base = base.loc[:, fy_cols]
        except Exception:
            pass

    if ttm_df is not None and not ttm_df.empty:
        ttm_series = ttm_df.iloc[:, 0]
        all_idx = base.index.union(ttm_series.index)
        base = base.reindex(all_idx)
        base.insert(0, "TTM", ttm_series.reindex(all_idx))
    return base


def _canonicalize_label(raw, preferred):
    raw = str(raw)
    if raw in _LABEL_OVERRIDES:
        raw = _LABEL_OVERRIDES[raw]
    key = _norm_label_key(raw)
    pref_map = {_norm_label_key(p): p for p in preferred}
    if key in pref_map:
        return pref_map[key]
    return raw


def _stmt_to_table(df, preferred_order=None, collapsed_order=None):
    """
    Convert yfinance metrics×dates → UI table.
    Applies Yahoo B/M format, label canonicalization, row order, collapsed filter.
    Returns dict with collapsed/expanded DataFrames.
    """
    empty = pd.DataFrame()
    if df is None or (hasattr(df, "empty") and df.empty):
        return {"collapsed": empty, "expanded": empty}

    preferred_order = preferred_order or []
    collapsed_order = collapsed_order or preferred_order

    out = df.copy()
    # Keep TTM first; sort remaining date columns newest→oldest
    cols = list(out.columns)
    ttm_cols = [c for c in cols if str(c).strip().upper() == "TTM"]
    other = [c for c in cols if c not in ttm_cols]
    try:
        other = sorted(other, reverse=True)
    except Exception:
        pass
    out = out.loc[:, ttm_cols + other]

    out = out.reset_index()
    first = out.columns[0]
    out = out.rename(columns={first: "Breakdown"})

    new_cols = ["Breakdown"]
    for c in out.columns[1:]:
        if str(c).strip().upper() == "TTM":
            new_cols.append("TTM")
            continue
        try:
            ts = pd.Timestamp(c)
            new_cols.append(ts.strftime("%m/%d/%Y"))
        except Exception:
            new_cols.append(str(c))
    out.columns = new_cols

    out["Breakdown"] = out["Breakdown"].map(
        lambda x: _canonicalize_label(x, preferred_order)
    )
    # Deduplicate labels after rename (keep first)
    out = out.drop_duplicates(subset=["Breakdown"], keep="first")

    for col in out.columns[1:]:
        out[col] = out[col].apply(_fmt_yahoo_cell)

    # Order expanded rows
    pref_rank = {_norm_label_key(p): i for i, p in enumerate(preferred_order)}

    def _row_rank(label):
        k = _norm_label_key(label)
        return pref_rank.get(k, 10_000)

    out = out.assign(_rank=out["Breakdown"].map(_row_rank)).sort_values(
        ["_rank"], kind="mergesort"
    ).drop(columns=["_rank"]).reset_index(drop=True)

    # Collapsed = whitelist (Yahoo "before Expand All")
    coll_keys = {_norm_label_key(x) for x in collapsed_order}
    collapsed = out[out["Breakdown"].map(lambda x: _norm_label_key(x) in coll_keys)].copy()
    coll_rank = {_norm_label_key(p): i for i, p in enumerate(collapsed_order)}
    collapsed = collapsed.assign(
        _rank=collapsed["Breakdown"].map(lambda x: coll_rank.get(_norm_label_key(x), 10_000))
    ).sort_values(["_rank"], kind="mergesort").drop(columns=["_rank"]).reset_index(drop=True)

    if collapsed.empty:
        collapsed = out.copy()

    return {"collapsed": collapsed, "expanded": out}


def scrape_all_financials_yf(ticker="AMZN"):
    """Cloud-safe financials via yfinance, shaped like app_9.0 Yahoo HTML."""
    print(f"📊 使用 yfinance 獲取 {ticker} 財報（app_10.0，對齊 v9 格式）...")
    stock = yf.Ticker(ticker)

    income_ann = _pick_attr(stock, "income_stmt", "financials", "incomestmt")
    income_ttm = _pick_attr(stock, "ttm_income_stmt", "ttm_incomestmt", "ttm_financials")
    balance_ann = _pick_attr(stock, "balance_sheet", "balancesheet")
    cash_ann = _pick_attr(stock, "cashflow", "cash_flow")
    cash_ttm = _pick_attr(stock, "ttm_cashflow", "ttm_cash_flow")

    income_m = _merge_annual_ttm(income_ann, income_ttm, max_fy=4)
    # Balance sheet has no TTM on Yahoo HTML
    balance_m = _merge_annual_ttm(balance_ann, None, max_fy=4)
    cash_m = _merge_annual_ttm(cash_ann, cash_ttm, max_fy=4)

    income = _stmt_to_table(income_m, _INCOME_EXPANDED, _INCOME_COLLAPSED)
    balance = _stmt_to_table(balance_m, _BALANCE_EXPANDED, _BALANCE_COLLAPSED)
    cash = _stmt_to_table(cash_m, _CASH_EXPANDED, _CASH_COLLAPSED)

    

    return {
        "Income Statement": income,
        "Balance Sheet": balance,
        "Cash Flow": cash,
    }


def scrape_all_financials_selenium(ticker="AMZN"):
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument("--disable-gpu")
    chrome_options.add_argument("--no-sandbox")
    chrome_options.add_argument("--disable-dev-shm-usage")
    chrome_options.add_argument("--disable-blink-features=AutomationControlled")
    chrome_options.add_argument(
        "user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    )

    driver = webdriver.Chrome(
        service=Service(ChromeDriverManager().install()), options=chrome_options
    )

    pages = {
        "Income Statement": f"https://finance.yahoo.com/quote/{ticker}/financials/",
        "Balance Sheet": f"https://finance.yahoo.com/quote/{ticker}/balance-sheet/",
        "Cash Flow": f"https://finance.yahoo.com/quote/{ticker}/cash-flow/",
    }

    all_results = {}

    try:
        for name, url in pages.items():
            print(f"🌐 正在處理 {name}: {url}")
            driver.get(url)
            wait = WebDriverWait(driver, 15)

            def extract_table():
                wait.until(
                    EC.presence_of_element_located(
                        (By.XPATH, "//div[contains(@class, 'tableHeader')]")
                    )
                )
                header_el = driver.find_element(
                    By.XPATH,
                    "//div[contains(@class, 'tableHeader')]//div[contains(@class, 'row')]",
                )
                headers = [
                    col.text
                    for col in header_el.find_elements(
                        By.XPATH, ".//div[contains(@class, 'column')]"
                    )
                    if col.text
                ]

                rows_elements = driver.find_elements(
                    By.XPATH,
                    "//div[contains(@class, 'tableBody')]//div[contains(@class, 'row')]",
                )
                table_data = []
                for row in rows_elements:
                    cols = row.find_elements(
                        By.XPATH, ".//div[contains(@class, 'column')]"
                    )
                    row_text = [c.text.strip() for c in cols]
                    if row_text and row_text[0]:
                        table_data.append(row_text)

                df = pd.DataFrame(table_data)
                if not df.empty and len(headers) == df.shape[1]:
                    df.columns = headers
                return df

            print(f"📄 抓取 {name} (未展開版本)...")
            df_collapsed = extract_table()

            try:
                expand_btn = wait.until(
                    EC.element_to_be_clickable(
                        (
                            By.XPATH,
                            "//button[contains(text(), 'Expand All')] | "
                            "//button[.//span[contains(text(), 'Expand All')]]",
                        )
                    )
                )
                driver.execute_script("arguments[0].click();", expand_btn)
                print("✅ 已成功點擊 Expand All")
                wait.until(
                    EC.presence_of_element_located(
                        (
                            By.XPATH,
                            "//div[contains(@class, 'expanded-row-class-name') or @data-test='fin-row']",
                        )
                    )
                )
            except Exception:
                print("⚠️ 找不到 Expand All 按鈕或已是展開狀態")

            print(f"📄 抓取 {name} (已展開版本)...")
            df_expanded = extract_table()

            all_results[name] = {
                "collapsed": df_collapsed,
                "expanded": df_expanded,
            }
            

    except Exception as e:
        print(f"❌ 爬蟲發生錯誤: {e}")
        raise
    finally:
        driver.quit()

    return all_results


def scrape_all_financials(ticker="AMZN"):
    """Prefer yfinance (cloud); Selenium is optional local fallback only."""
    try:
        result = scrape_all_financials_yf(ticker)
        has_data = any(not result[k]["expanded"].empty for k in result)
        if has_data:
            return result
        print("⚠️ yfinance 回傳空表，改試 Selenium（若可用）...")
    except Exception as e:
        print(f"⚠️ yfinance 失敗: {e}")

    if SELENIUM_AVAILABLE:
        try:
            return scrape_all_financials_selenium(ticker)
        except Exception as e:
            print(f"⚠️ Selenium 失敗: {e}")

    empty = pd.DataFrame()
    return {
        "Income Statement": {"collapsed": empty, "expanded": empty},
        "Balance Sheet": {"collapsed": empty, "expanded": empty},
        "Cash Flow": {"collapsed": empty, "expanded": empty},
    }
