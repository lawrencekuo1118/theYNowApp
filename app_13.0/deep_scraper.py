"""
app_11.0 — cloud-compatible financials.
Prefer yfinance API (works on shinyapps.io). Selenium is optional fallback for local only.
"""
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


def _best_company_name(info, ticker=""):
    """Prefer full legal/display name; Yahoo shortName is often truncated."""
    if not isinstance(info, dict):
        info = {}
    candidates = [
        info.get("longName"),
        info.get("longname"),
        info.get("displayName"),
        info.get("shortName"),
        info.get("shortname"),
    ]
    for c in candidates:
        if c is None:
            continue
        s = str(c).strip()
        if s:
            return s
    t = str(ticker or "").strip()
    return t if t else "Unknown"


def fast_get_company_info(ticker="AMZN"):
    print(f"⚡ 使用高速 API 獲取 {ticker} 公司與產業資訊...")
    try:
        stock = yf.Ticker(ticker)
        info = stock.info

        comp_name = _best_company_name(info, ticker)
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
    info = {}
    try:
        info = stock.info or {}
    except Exception as e:
        print(f"⚠️ stock.info failed: {e}; trying fast_info/history fallback")
        try:
            fi = getattr(stock, "fast_info", None)
            if fi is not None:
                # fast_info may be dict-like or object
                def _get(k, default=None):
                    try:
                        return fi[k] if hasattr(fi, "__getitem__") else getattr(fi, k, default)
                    except Exception:
                        return default
                info = {
                    "shortName": ticker,
                    "previousClose": _get("previous_close") or _get("previousClose"),
                    "open": _get("open"),
                    "dayLow": _get("day_low") or _get("dayLow"),
                    "dayHigh": _get("day_high") or _get("dayHigh"),
                    "fiftyTwoWeekLow": _get("year_low") or _get("fiftyTwoWeekLow"),
                    "fiftyTwoWeekHigh": _get("year_high") or _get("fiftyTwoWeekHigh"),
                    "volume": _get("last_volume") or _get("volume"),
                    "averageVolume": _get("three_month_average_volume") or _get("averageVolume"),
                    "marketCap": _get("market_cap") or _get("marketCap"),
                }
        except Exception as e2:
            print(f"⚠️ fast_info fallback failed: {e2}")
            info = {}

    company_name = _best_company_name(info, ticker)

    # Quote vs reporting currency (ADR often differs)
    def _ccy(key, default=""):
        v = info.get(key)
        if v is None:
            return default
        s = str(v).strip().upper()
        return s if s else default

    quote_ccy = _ccy("currency", "")
    fin_ccy = _ccy("financialCurrency", "") or quote_ccy
    if not quote_ccy and ticker.upper().endswith((".TW", ".TWO")):
        quote_ccy = "TWD"
    if not fin_ccy and ticker.upper().endswith((".TW", ".TWO")):
        fin_ccy = "TWD"
    if not quote_ccy:
        quote_ccy = "USD"
    if not fin_ccy:
        fin_ccy = quote_ccy

    # 若 info 幾乎為空，用 history 補 Previous Close
    if info.get("previousClose") is None:
        try:
            hist = stock.history(period="5d")
            if hist is not None and not hist.empty:
                info["previousClose"] = float(hist["Close"].dropna().iloc[-1])
                if info.get("open") is None:
                    info["open"] = float(hist["Open"].dropna().iloc[-1])
                if info.get("volume") is None:
                    info["volume"] = float(hist["Volume"].dropna().iloc[-1])
                if info.get("dayLow") is None:
                    info["dayLow"] = float(hist["Low"].dropna().iloc[-1])
                if info.get("dayHigh") is None:
                    info["dayHigh"] = float(hist["High"].dropna().iloc[-1])
        except Exception as e:
            print(f"⚠️ history fallback: {e}")

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

    # Return plain lists so reticulate on shinyapps converts reliably
    # (nested pandas DataFrame inside dict often becomes empty in R).
    items = [r[0] for r in rows]
    values = [r[1] for r in rows]
    print(f"✅ summary rows={len(items)} name={company_name} ccy={quote_ccy}/{fin_ccy}")
    return {
        "company_name": str(company_name),
        "currency": str(quote_ccy),
        "financialCurrency": str(fin_ccy),
        "Item": items,
        "Value": values,
    }


def get_usd_twd_rate():
    """USD→TWD spot via yfinance (TWD=X = TWD per 1 USD)."""
    print("💱 yfinance FX TWD=X")
    for sym in ("TWD=X", "USDTWD=X"):
        try:
            t = yf.Ticker(sym)
            hist = t.history(period="5d")
            if hist is not None and not hist.empty:
                px = float(hist["Close"].dropna().iloc[-1])
                if px > 0:
                    print(f"✅ FX {sym} = {px}")
                    return px
            info = t.info or {}
            for key in ("regularMarketPrice", "previousClose", "open"):
                v = info.get(key)
                if v is not None:
                    px = float(v)
                    if px > 0:
                        print(f"✅ FX {sym} info.{key} = {px}")
                        return px
        except Exception as e:
            print(f"⚠️ FX {sym}: {e}")
    print("⚠️ FX fallback 32.0")
    return 32.0


def get_price_history(ticker="AMZN", period="5y"):
    """Daily OHLCV for backtests — plain lists for reticulate."""
    print(f"📈 yfinance price history: {ticker} period={period}")
    stock = yf.Ticker(ticker)
    hist = stock.history(period=period, auto_adjust=True)
    if hist is None or hist.empty:
        return {"Date": [], "Close": [], "Volume": []}
    hist = hist.reset_index()
    # Date column may be DatetimeIndex name 'Date' or 'index'
    date_col = "Date" if "Date" in hist.columns else hist.columns[0]
    dates = []
    for d in hist[date_col]:
        try:
            dates.append(pd.Timestamp(d).strftime("%Y-%m-%d"))
        except Exception:
            dates.append(str(d)[:10])
    closes = [float(x) if pd.notna(x) else None for x in hist["Close"].tolist()]
    vols = []
    if "Volume" in hist.columns:
        vols = [float(x) if pd.notna(x) else 0.0 for x in hist["Volume"].tolist()]
    else:
        vols = [0.0] * len(closes)
    return {"Date": dates, "Close": closes, "Volume": vols}


def get_risk_free_rate_yf():
    """10Y Treasury yield (^TNX) via yfinance — no Chromote."""
    print("📊 yfinance Rf ^TNX")
    tnx = yf.Ticker("^TNX")
    # prefer fast_info / history last close
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


def _safe_float(v):
    if v is None:
        return None
    try:
        if pd.isna(v):
            return None
    except Exception:
        pass
    try:
        return float(v)
    except Exception:
        return None


def get_beta_unlever_inputs(ticker="AAPL"):
    """
    Lightweight Yahoo fields for unlevered / bottom-up beta.
    Returns plain scalars for reticulate (no nested frames).

    D/E preference: totalDebt / marketCap (market); else Yahoo debtToEquity.
    Yahoo debtToEquity is usually Equity*100 style (e.g. 54.2 → 0.542).
    """
    tk = str(ticker or "").strip().upper()
    out = {
        "ticker": tk,
        "ok": False,
        "beta": None,
        "market_cap": None,
        "total_debt": None,
        "de_ratio": None,
        "de_source": "",
        "name": tk,
        "error": "",
    }
    if not tk:
        out["error"] = "empty ticker"
        return out
    print(f"📐 beta unlever inputs: {tk}")
    try:
        stock = yf.Ticker(tk)
        info = {}
        try:
            info = stock.info or {}
        except Exception as e:
            out["error"] = f"info failed: {e}"
            info = {}
        out["name"] = _best_company_name(info, tk)
        beta = _safe_float(info.get("beta"))
        mcap = _safe_float(info.get("marketCap"))
        debt = _safe_float(info.get("totalDebt"))
        de_raw = _safe_float(info.get("debtToEquity"))
        out["beta"] = beta
        out["market_cap"] = mcap
        out["total_debt"] = debt

        de = None
        de_src = ""
        if mcap is not None and mcap > 0 and debt is not None and debt >= 0:
            de = debt / mcap
            de_src = "market_D/E"
        elif de_raw is not None and de_raw >= 0:
            # Yahoo often stores Debt/Equity × 100
            de = (de_raw / 100.0) if de_raw > 5.0 else de_raw
            de_src = "yahoo_debtToEquity"
        out["de_ratio"] = de
        out["de_source"] = de_src
        out["ok"] = beta is not None and de is not None
        if beta is None:
            out["error"] = out["error"] or "missing beta"
        elif de is None:
            out["error"] = out["error"] or "missing D/E"
    except Exception as e:
        out["error"] = str(e)
    return out


def get_beta_unlever_inputs_batch(tickers):
    """Batch wrapper; tickers may be list/tuple from reticulate."""
    if tickers is None:
        return []
    try:
        seq = list(tickers)
    except Exception:
        seq = [tickers]
    cleaned = []
    for t in seq:
        s = str(t or "").strip().upper()
        if s and s not in cleaned:
            cleaned.append(s)
    return [get_beta_unlever_inputs(t) for t in cleaned]


def _stmt_to_payload(df):
    """Convert yfinance statement to plain dict for reticulate (columns + rows)."""
    empty = {"columns": ["Breakdown"], "data": []}
    if df is None or (hasattr(df, "empty") and df.empty):
        return empty

    out = df.copy()
    try:
        out = out.loc[:, sorted(out.columns, reverse=True)]
    except Exception:
        pass

    out = out.reset_index()
    first = out.columns[0]
    out = out.rename(columns={first: "Breakdown"})

    new_cols = ["Breakdown"]
    for c in out.columns[1:]:
        try:
            ts = pd.Timestamp(c)
            new_cols.append(ts.strftime("%m/%d/%Y"))
        except Exception:
            new_cols.append(str(c))
    out.columns = new_cols

    data = []
    for _, row in out.iterrows():
        cells = []
        for v in row.tolist():
            if pd.isna(v):
                cells.append("")
            elif isinstance(v, (int, float)):
                fv = float(v)
                cells.append(str(int(fv)) if fv.is_integer() else f"{fv:.2f}")
            else:
                cells.append(str(v))
        data.append(cells)

    # yfinance 列序與 Yahoo 網頁相反（明細在上、營收在下）。
    # 反轉成與本機 Selenium／trim_financial_table 相同：營收在上、裁切點在下。
    data = list(reversed(data))

    return {"columns": [str(c) for c in out.columns.tolist()], "data": data}


def scrape_all_financials_yf(ticker="AMZN"):
    """Cloud-safe financials via Yahoo Finance API (no Chrome)."""
    print(f"📊 使用 yfinance 獲取 {ticker} 財報（app_11.0）...")
    stock = yf.Ticker(ticker)

    def pick(*names):
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

    income = pick("income_stmt", "financials", "incomestmt")
    balance = pick("balance_sheet", "balancesheet")
    cash = pick("cashflow", "cash_flow")

    income_p = _stmt_to_payload(income)
    balance_p = _stmt_to_payload(balance)
    cash_p = _stmt_to_payload(cash)
    print(
        f"✅ financials income_rows={len(income_p['data'])} "
        f"bs_rows={len(balance_p['data'])} cf_rows={len(cash_p['data'])}"
    )

    # UI expects collapsed/expanded; API provides one granularity → reuse payload
    return {
        "Income Statement": {"collapsed": income_p, "expanded": income_p},
        "Balance Sheet": {"collapsed": balance_p, "expanded": balance_p},
        "Cash Flow": {"collapsed": cash_p, "expanded": cash_p},
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
    """Prefer yfinance (cloud-safe); optionally try Selenium locally."""
    empty_payload = {"columns": ["Breakdown"], "data": []}

    def _has_rows(payload):
        try:
            return isinstance(payload, dict) and len(payload.get("data") or []) > 0
        except Exception:
            return False

    try:
        result = scrape_all_financials_yf(ticker)
        has_data = any(_has_rows(result[k]["expanded"]) for k in result)
        if has_data:
            return result
        print("⚠️ yfinance 回傳空表，改試 Selenium（若可用）...")
    except Exception as e:
        print(f"⚠️ yfinance 失敗: {e}")

    if SELENIUM_AVAILABLE:
        try:
            raw = scrape_all_financials_selenium(ticker)
            # Convert selenium DataFrames → plain payloads for R
            out = {}
            for name, stmt in raw.items():
                out[name] = {
                    "collapsed": _stmt_to_payload(stmt.get("collapsed")),
                    "expanded": _stmt_to_payload(stmt.get("expanded")),
                }
            return out
        except Exception as e:
            print(f"⚠️ Selenium 失敗: {e}")

    return {
        "Income Statement": {"collapsed": empty_payload, "expanded": empty_payload},
        "Balance Sheet": {"collapsed": empty_payload, "expanded": empty_payload},
        "Cash Flow": {"collapsed": empty_payload, "expanded": empty_payload},
    }

def search_tickers(query="", max_results=12):
    """
    Typeahead ticker suggestions via yfinance.Search.
    Returns plain list of dicts for reticulate: symbol, name, type, exchange, label.
    """
    q = (query or "").strip()
    if len(q) < 1:
        return []
    max_results = int(max_results) if max_results else 12
    max_results = max(1, min(max_results, 25))
    out = []
    try:
        s = yf.Search(q, max_results=max(max_results * 2, 12))
        quotes = getattr(s, "quotes", None) or []
        preferred = []
        other = []
        for item in quotes:
            if not isinstance(item, dict):
                continue
            sym = item.get("symbol")
            if not sym:
                continue
            name = (
                item.get("longname")
                or item.get("longName")
                or item.get("shortname")
                or item.get("shortName")
                or ""
            )
            qtype = item.get("quoteType") or item.get("typeDisp") or ""
            exch = item.get("exchDisp") or item.get("exchange") or ""
            label = f"{sym} — {name}" if name else str(sym)
            if qtype or exch:
                extras = " · ".join([x for x in [qtype, exch] if x])
                if extras:
                    label = f"{label} ({extras})"
            row = {
                "symbol": str(sym),
                "name": str(name),
                "type": str(qtype),
                "exchange": str(exch),
                "label": label,
            }
            qt = str(qtype).upper()
            if qt in ("EQUITY", "ETF", "INDEX"):
                preferred.append(row)
            else:
                other.append(row)
        out = (preferred + other)[:max_results]
    except Exception as e:
        print(f"⚠️ search_tickers failed ({q}): {e}")
        return []
    return out


# ==========================================================================
# 🧪 Experimental: SEC EDGAR financial-report notes (US filings only)
# --------------------------------------------------------------------------
# yfinance does not expose statement footnotes, so the notes are pulled from
# SEC EDGAR: ticker -> CIK -> latest annual/interim/material filing ->
# FilingSummary.xml (Notes) or primaryDocument body for 8-K/6-K.
# Domestic: 10-K / 10-Q / 8-K; foreign private issuers (ADR etc.): 20-F / 40-F / 6-K.
# ==========================================================================
import os as _os
import re as _re

_SEC_TICKERS_URL = "https://www.sec.gov/files/company_tickers.json"
_SEC_SUBMISSIONS_URL = "https://data.sec.gov/submissions/CIK{cik}.json"
_SEC_ARCHIVES = "https://www.sec.gov/Archives/edgar/data/{cik}/{accn}"
_SEC_UA = _os.environ.get(
    "SEC_EDGAR_UA", "theYNowApp research (set SEC_EDGAR_UA=you@example.com)"
)

_SEC_IMPORTANT_KEYWORDS = (
    "accounting policies", "basis of presentation", "revenue", "segment",
    "geographic", "income tax", "debt", "borrow", "lease", "commitment",
    "contingenc", "financial instrument", "fair value", "derivative",
    "goodwill", "intangible", "share-based", "stock-based", "pension",
    "retirement", "business combination", "acquisition", "per share",
    "related party", "restructuring", "property, plant",
)
_SEC_NON_FINANCIAL_HINTS = ("insider trading", "cybersecurity")

_SEC_BANNER = _re.compile(
    r"^\s*(?:XML\s+\d+\s+)?[Rr]\d+\.htm\s+IDEA:\s*XBRL DOCUMENT\s+v?[\d.]+\s*",
    _re.IGNORECASE,
)

# Marks the start of the per-note XBRL element metadata footer appended to each
# R#.htm rendering ("X - References ... X - Definition ... No definition ...").
_SEC_XBRL_FOOTER = _re.compile(r"\s*X\s*-\s*(?:References|Definition)\b")


def _sec_session():
    """curl_cffi session with a SEC-compliant User-Agent (browser impersonation
    also avoids datacenter-IP blocks). Falls back to requests."""
    try:
        from curl_cffi import requests as creq
        return creq.Session(headers={"User-Agent": _SEC_UA}, impersonate="chrome")
    except Exception:
        import requests
        s = requests.Session()
        s.headers.update({"User-Agent": _SEC_UA})
        return s


def _sec_get(session, url, is_json=False, tries=3):
    import time
    last = None
    for attempt in range(tries):
        try:
            r = session.get(url, timeout=30)
            if r.status_code == 200:
                return r.json() if is_json else r.text
            last = f"HTTP {r.status_code}"
        except Exception as e:  # noqa: BLE001
            last = str(e)
        time.sleep(0.5 * (attempt + 1))
    raise RuntimeError(f"GET failed ({last}): {url}")


def _sec_clean_note_text(raw):
    from bs4 import BeautifulSoup
    try:
        soup = BeautifulSoup(raw, "lxml")
    except Exception:
        soup = BeautifulSoup(raw, "html.parser")
    text = soup.get_text(" ", strip=True)
    text = _re.sub(r"\s+", " ", text)
    text = _SEC_BANNER.sub("", text).strip()
    m = _SEC_XBRL_FOOTER.search(text)
    if m:
        text = text[:m.start()].strip()
    return text


def _sec_is_important(short_name):
    s = (short_name or "").lower()
    if any(h in s for h in _SEC_NON_FINANCIAL_HINTS):
        return False
    return any(k in s for k in _SEC_IMPORTANT_KEYWORDS)


# Salient-sentence keywords for the lightweight extractive summary.
_SEC_SUMMARY_KEYWORDS = (
    "recogn", "increase", "decrease", "obligation", "maturit",
    "effective tax rate", "valuation allowance", "repurchas", "dividend",
    "lease", "guarantee", "litigation", "contingenc", "impair", "goodwill",
    "amortiz", "depreciat", "deferred", "unrecognized", "commitment",
    "interest rate", "fair value", "hedge", "derivative", "segment",
    "revenue", "operating", "allowance", "provision", "settlement",
    "restructuring", "outstanding", "expense", "benefit", "liabilit",
)

_SEC_ABBR = _re.compile(
    r"\b(U\.S|Inc|Corp|Ltd|No|vs|approx|Sept|Oct|Nov|Dec|Jan|Feb|Mar|Apr|Jun|Jul|Aug|e\.g|i\.e)\.",
    _re.IGNORECASE,
)


def _sec_split_sentences(text):
    t = _SEC_ABBR.sub(lambda m: m.group(0).replace(".", "<DOT>"), text or "")
    parts = _re.split(r"(?<=[.;])\s+(?=[A-Z(“\"])", t)
    out = []
    for p in parts:
        s = p.replace("<DOT>", ".").strip()
        if s:
            out.append(s)
    return out


def _sec_summarize(text, max_bullets=4, max_len=280):
    """Rule-based extractive summary: pick the most information-dense sentences
    (dollar amounts, percentages, years, financial keywords). No external API,
    so it runs unchanged on shinyapps.io."""
    if not text:
        return []
    sentences = _sec_split_sentences(text)
    scored = []
    for pos, s in enumerate(sentences):
        words = s.split()
        if len(words) < 7 or len(s) < 45 or len(s) > 360:
            continue
        low = s.lower()
        if "[abstract]" in low or "months ended" in low or s.endswith(":"):
            continue
        score = 0.0
        if "$" in s:
            score += 2.0
        if "%" in s:
            score += 2.0
        if _re.search(r"\b(19|20)\d{2}\b", s):
            score += 1.0
        kw = sum(1 for k in _SEC_SUMMARY_KEYWORDS if k in low)
        score += min(kw, 3)
        if score <= 0:
            continue
        scored.append((score, pos, s))
    if not scored:
        # Fallback: first couple of reasonably long, non-boilerplate sentences.
        fallback = [
            s for s in sentences
            if len(s.split()) >= 8
            and "[abstract]" not in s.lower()
            and "months ended" not in s.lower()
            and len(s) <= 360
        ][:2]
        return [(s[:max_len] + ("…" if len(s) > max_len else "")) for s in fallback]

    scored.sort(key=lambda x: (-x[0], x[1]))
    picked = []
    seen = set()
    for _, pos, s in scored:
        key = s[:60].lower()
        if key in seen:
            continue
        seen.add(key)
        picked.append((pos, s))
        if len(picked) >= max_bullets:
            break
    picked.sort(key=lambda x: x[0])
    return [(s[:max_len] + ("…" if len(s) > max_len else "")) for _, s in picked]


def _sec_empty_result(error=""):
    return {
        "ok": False, "error": str(error), "company": "", "form": "",
        "filing_date": "", "report_date": "", "accession": "",
        "primary_doc_url": "", "short_names": [], "urls": [],
        "important": [], "char_counts": [], "excerpts": [], "full_texts": [],
        "summaries": [],
    }


# Preferred form candidates: domestic first, then foreign (UI label order).
# 年報 10-K → 20-F/40-F; 重大訊息 8-K → 6-K.
_SEC_FORM_CANDIDATES = {
    "10-K": ("10-K", "20-F", "40-F"),
    "20-F": ("20-F", "10-K", "40-F"),
    "40-F": ("40-F", "20-F", "10-K"),
    "10-Q": ("10-Q",),
    "8-K": ("8-K", "8-K/A", "6-K", "6-K/A"),
    "6-K": ("6-K", "6-K/A", "8-K", "8-K/A"),
}

_SEC_MATERIAL_FORMS = frozenset({"8-K", "8-K/A", "6-K", "6-K/A"})


def _sec_is_material_form(form):
    return str(form or "") in _SEC_MATERIAL_FORMS


def _sec_append_text_item(result, short_name, url, text, max_chars, important=True):
    """Append one parallel-list row (used by notes and 8-K/6-K body extract)."""
    text = text or ""
    result["short_names"].append(str(short_name))
    result["urls"].append(url)
    result["important"].append(bool(important))
    result["char_counts"].append(len(text))
    result["excerpts"].append(text[:max_chars])
    result["full_texts"].append(text)
    result["summaries"].append(_sec_summarize(text) if important else [])


def _sec_extract_notes_from_summary(session, folder, result, max_chars):
    """Fill result lists from FilingSummary.xml Notes; returns True if any note found."""
    import time
    from bs4 import BeautifulSoup
    try:
        xml = _sec_get(session, f"{folder}/FilingSummary.xml")
    except Exception:
        return False
    try:
        soup = BeautifulSoup(xml, "lxml-xml")
    except Exception:
        soup = BeautifulSoup(xml, "html.parser")

    def _find(el, name):
        if el is None:
            return None
        return el.find(name) or el.find(name.lower())

    reports = soup.find_all("Report") or soup.find_all("report")
    found = False
    for rep in reports:
        cat = _find(rep, "MenuCategory")
        if not cat or (cat.text or "").strip().lower() != "notes":
            continue
        htmf = _find(rep, "HtmlFileName")
        if not htmf or not htmf.text:
            continue
        short = _find(rep, "ShortName")
        short_name = short.text if short else ""
        url = f"{folder}/{htmf.text}"
        try:
            text = _sec_clean_note_text(_sec_get(session, url))
        except Exception as e:  # noqa: BLE001
            text = f"(failed to fetch note: {e})"
        _sec_append_text_item(
            result, short_name, url, text, max_chars,
            important=bool(_sec_is_important(short_name)),
        )
        found = True
        time.sleep(0.12)
    return found


def _sec_extract_primary_body(session, folder, primary_doc, form, filing_date, result, max_chars):
    """Fallback for 8-K/6-K: clean primary document HTML into one important item."""
    url = f"{folder}/{primary_doc}"
    try:
        raw = _sec_get(session, url)
        text = _sec_clean_note_text(raw)
    except Exception as e:  # noqa: BLE001
        text = f"(failed to fetch primary document: {e})"
    label = f"Form {form}"
    if filing_date:
        label = f"{label} ({filing_date})"
    _sec_append_text_item(result, label, url, text, max_chars, important=True)
    return bool(text) and not text.startswith("(failed to fetch")


def sec_report_notes(ticker="AAPL", form="10-K", max_chars=1500):
    """Fetch a US stock's latest annual (10-K/20-F/40-F), interim (10-Q), or
    material disclosures (8-K/6-K) from SEC EDGAR. Annual/interim extract Notes;
    material filings fall back to primary-document body text.
    Returns a dict of parallel lists that reticulate converts cleanly to R."""
    tk = str(ticker or "").strip().upper()
    form = str(form or "10-K").strip().upper()
    try:
        max_chars = int(max_chars)
    except Exception:
        max_chars = 1500
    if not tk:
        return _sec_empty_result("empty ticker")
    if form not in _SEC_FORM_CANDIDATES:
        form = "10-K"
    candidates = _SEC_FORM_CANDIDATES[form]
    requested_form = form

    print(f"📄 SEC EDGAR notes: {tk} {form} (try {', '.join(candidates)})")
    try:
        session = _sec_session()

        # 1. ticker -> CIK
        tickers = _sec_get(session, _SEC_TICKERS_URL, is_json=True)
        cik = None
        company = tk
        for row in tickers.values():
            if str(row.get("ticker", "")).upper() == tk:
                cik = str(row["cik_str"]).zfill(10)
                company = row.get("title", tk)
                break
        if cik is None:
            return _sec_empty_result(f"ticker '{tk}' not found on SEC EDGAR (US filers only)")

        # 2. CIK -> latest filing among form candidates (keeps filing-date order)
        sub = _sec_get(session, _SEC_SUBMISSIONS_URL.format(cik=cik), is_json=True)
        recent = sub["filings"]["recent"]
        forms = recent["form"]
        has_10q = any(f == "10-Q" or str(f).startswith("10-Q") for f in forms)
        has_20f = any(f == "20-F" or str(f).startswith("20-F") for f in forms)
        has_6k = any(f == "6-K" or str(f).startswith("6-K") for f in forms)
        idx = None
        matched_form = None
        for i, f in enumerate(forms):
            if f in candidates:
                idx = i
                matched_form = f
                break
        if idx is None:
            if requested_form == "10-Q" and (has_20f or has_6k) and not has_10q:
                return _sec_empty_result(
                    f"no 10-Q filing found for {tk} "
                    f"(foreign issuer — use 年報 10-K/20-F or 重大訊息 8-K/6-K)"
                )
            return _sec_empty_result(f"no {requested_form} filing found for {tk}")
        form = matched_form  # report the actual form used (e.g. 20-F for TSM)
        accession = recent["accessionNumber"][idx]
        filing_date = recent["filingDate"][idx]
        report_date = recent.get("reportDate", [""] * len(forms))[idx]
        primary_doc = recent["primaryDocument"][idx]
        company = sub.get("name", company)
        folder = _SEC_ARCHIVES.format(cik=int(cik), accn=accession.replace("-", ""))

        # 3. filing -> Notes (annual/interim) or primary body (8-K/6-K fallback)
        result = _sec_empty_result("")
        result.update({
            "ok": True, "company": str(company), "form": form,
            "filing_date": str(filing_date), "report_date": str(report_date),
            "accession": str(accession),
            "primary_doc_url": f"{folder}/{primary_doc}",
        })
        has_notes = _sec_extract_notes_from_summary(session, folder, result, max_chars)
        if not has_notes:
            if _sec_is_material_form(form):
                ok_body = _sec_extract_primary_body(
                    session, folder, primary_doc, form, filing_date, result, max_chars
                )
                if not ok_body and not result["short_names"]:
                    result["ok"] = False
                    result["error"] = f"failed to extract body from {form} primary document"
            else:
                result["ok"] = False
                result["error"] = "no Notes section found in FilingSummary.xml"
        print(f"✅ SEC notes: {tk} {form} rows={len(result['short_names'])}")
        return result
    except Exception as e:  # noqa: BLE001
        print(f"⚠️ sec_report_notes failed ({tk} {form}): {e}")
        return _sec_empty_result(str(e))
