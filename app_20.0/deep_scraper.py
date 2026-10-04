"""
Cloud-compatible financials via yfinance HTTP (no headless Chrome).
"""
import os
import time
import pandas as pd
import yfinance as yf


def _dbg(*args, **kwargs):
    """Console traces only when YNOW_DEBUG=1 (inherited from R / the shell)."""
    if os.environ.get("YNOW_DEBUG", "").strip() in ("1", "true", "TRUE", "yes", "on"):
        print(*args, **kwargs)


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
    """Yahoo Sector / Industry via yfinance (no browser). Used by UI 「industry info from Yahoo」."""
    _dbg(f"⚡ 使用高速 API 獲取 {ticker} 公司與產業資訊...")
    try:
        stock = yf.Ticker(ticker)
        info = {}
        try:
            info = stock.info or {}
        except Exception as e:
            _dbg(f"⚠️ stock.info failed: {e}")
            info = {}
        # Newer yfinance: get_info() can succeed when .info is empty / rate-limited
        if not info.get("sector") and not info.get("industry"):
            try:
                gi = getattr(stock, "get_info", None)
                if callable(gi):
                    info2 = gi() or {}
                    if isinstance(info2, dict) and info2:
                        info = {**info, **info2}
            except Exception as e2:
                _dbg(f"⚠️ get_info fallback failed: {e2}")

        if not isinstance(info, dict):
            info = {}

        comp_name = _best_company_name(info, ticker)
        sector = info.get("sector") or info.get("sectorDisp") or "Unknown Sector"
        industry = info.get("industry") or info.get("industryDisp") or "Unknown Industry"
        if not str(sector).strip():
            sector = "Unknown Sector"
        if not str(industry).strip():
            industry = "Unknown Industry"

        return {
            "company_name": comp_name,
            "sector": str(sector).strip(),
            "industry": str(industry).strip(),
        }
    except Exception as e:
        _dbg(f"⚠️ 獲取公司資訊失敗: {e}")
        return {
            "company_name": ticker,
            "sector": "N/A",
            "industry": "N/A",
        }


def _fmt_num(v, digits=2):
    """Format a number for Summary Value cells (always a string for reticulate).

    Large Python ints (e.g. TSM marketCap ≈ 2e12) must NOT be passed to R as
    raw integers — reticulate maps them to 32-bit R integer and overflows.
    """
    if v is None:
        return "N/A"
    try:
        if pd.isna(v):
            return "N/A"
    except Exception:
        pass
    try:
        # Cast via str→float so values > 2^31 stay exact enough for T/B suffixes
        # and never become reticulate int32 overflow when R later parses the cell.
        fv = float(v) if not isinstance(v, str) else float(str(v).replace(",", ""))
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
    _dbg(f"📊 yfinance summary quote: {ticker}")
    stock = yf.Ticker(ticker)
    info = {}
    try:
        info = stock.info or {}
    except Exception as e:
        _dbg(f"⚠️ stock.info failed: {e}; trying fast_info/history fallback")
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
                    "currentPrice": _get("last_price") or _get("lastPrice") or _get("regular_market_price"),
                    "regularMarketPrice": _get("last_price") or _get("regular_market_price"),
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
            _dbg(f"⚠️ fast_info fallback failed: {e2}")
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

    def _hist_last_close(period="5d", interval=None):
        try:
            kw = {"period": period, "auto_adjust": True}
            if interval:
                kw["interval"] = interval
            hist = stock.history(**kw)
            if hist is not None and not hist.empty and "Close" in hist.columns:
                closes = hist["Close"].dropna()
                if len(closes):
                    return float(closes.iloc[-1])
        except Exception as e:
            _dbg(f"⚠️ history last close {period}/{interval}: {e}")
        return None

    def _fast_last_price():
        try:
            fi = getattr(stock, "fast_info", None)
            if fi is None:
                return None
            for k in ("last_price", "lastPrice", "regular_market_price"):
                try:
                    v = fi[k] if hasattr(fi, "__getitem__") else getattr(fi, k, None)
                except Exception:
                    v = getattr(fi, k, None) if hasattr(fi, k) else None
                if v is None:
                    continue
                fv = float(v)
                if fv > 0:
                    return fv
        except Exception as e:
            _dbg(f"⚠️ fast_info last_price: {e}")
        return None

    def _live_last_price():
        for v in (
            info.get("currentPrice"),
            info.get("regularMarketPrice"),
            info.get("regularMarketLastPrice"),
            _fast_last_price(),
        ):
            try:
                if v is None:
                    continue
                fv = float(v)
                if fv > 0:
                    return fv
            except Exception:
                pass
        px_1m = _hist_last_close(period="1d", interval="1m")
        if px_1m and px_1m > 0:
            return px_1m
        return _hist_last_close(period="5d")

    last_px = _live_last_price()

    # 若 info 幾乎為空，用 history 補 Previous Close / OHLC
    if info.get("previousClose") is None:
        try:
            hist = stock.history(period="5d")
            if hist is not None and not hist.empty:
                closes = hist["Close"].dropna()
                if last_px is None and len(closes):
                    last_px = float(closes.iloc[-1])
                if len(closes) >= 2:
                    info["previousClose"] = float(closes.iloc[-2])
                elif len(closes) == 1:
                    info["previousClose"] = float(closes.iloc[-1])
                if info.get("open") is None:
                    info["open"] = float(hist["Open"].dropna().iloc[-1])
                if info.get("volume") is None:
                    info["volume"] = float(hist["Volume"].dropna().iloc[-1])
                if info.get("dayLow") is None:
                    info["dayLow"] = float(hist["Low"].dropna().iloc[-1])
                if info.get("dayHigh") is None:
                    info["dayHigh"] = float(hist["High"].dropna().iloc[-1])
        except Exception as e:
            _dbg(f"⚠️ history fallback: {e}")

    day_low = info.get("dayLow")
    day_high = info.get("dayHigh")
    w52_low = info.get("fiftyTwoWeekLow")
    w52_high = info.get("fiftyTwoWeekHigh")
    div_rate = info.get("dividendRate")
    div_yield = info.get("dividendYield")

    rows = [
        ("Market Price", _fmt_num(last_px if last_px is not None else info.get("regularMarketPrice"))),
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
    _dbg(f"✅ summary rows={len(items)} name={company_name} ccy={quote_ccy}/{fin_ccy}")
    return {
        "company_name": str(company_name),
        "currency": str(quote_ccy),
        "financialCurrency": str(fin_ccy),
        "Item": items,
        "Value": values,
    }


def get_market_caps_batch(tickers):
    """Yahoo market cap (USD) for many tickers. Returns {SYMBOL: float|None}.

    Used by Lab to rank the evaluate pool by size before the Yahoo F-Score/FV
    pass (so 評估檔數 N is the largest-cap names, not ticker/industry order).
    """
    cleaned = []
    seen = set()
    for raw in tickers or []:
        t = str(raw or "").strip().upper().replace("/", "-")
        if not t or t in seen:
            continue
        seen.add(t)
        cleaned.append(t)
    out = {t: None for t in cleaned}
    if not cleaned:
        return out

    def _apply_quotes(quotes):
        n_ok = 0
        for q in quotes or []:
            if not isinstance(q, dict):
                continue
            sym = str(q.get("symbol") or "").strip().upper()
            cap = q.get("marketCap")
            if not sym:
                continue
            try:
                cap_f = float(cap) if cap is not None else None
            except (TypeError, ValueError):
                cap_f = None
            if cap_f is None or cap_f <= 0:
                continue
            for key in (sym, sym.replace(".", "-"), sym.replace("-", ".")):
                if key in out and out[key] is None:
                    out[key] = cap_f
            if sym not in out:
                out[sym] = cap_f
            n_ok += 1
        return n_ok

    chunk_size = 40
    got_any = False
    try:
        from yfinance.data import YfData

        yd = YfData()
        url = "https://query2.finance.yahoo.com/v7/finance/quote"
        for i in range(0, len(cleaned), chunk_size):
            chunk = cleaned[i : i + chunk_size]
            try:
                raw = yd.get(url, params={"symbols": ",".join(chunk)})
                if hasattr(raw, "json") and not isinstance(raw, dict):
                    raw = raw.json()
                quotes = []
                if isinstance(raw, dict):
                    quotes = (raw.get("quoteResponse") or {}).get("result") or []
                if _apply_quotes(quotes):
                    got_any = True
            except Exception as e:  # noqa: BLE001
                _dbg(f"⚠️ market cap quote chunk failed: {e}")
    except Exception as e:  # noqa: BLE001
        _dbg(f"⚠️ YfData quote batch unavailable: {e}")

    n_ok = sum(1 for v in out.values() if v is not None)
    if got_any and n_ok:
        _dbg(f"✅ market caps {n_ok}/{len(cleaned)}")
    else:
        _dbg("⚠️ market cap batch empty; Lab uses offline snapshot / keep universe order")
    return out


def get_returns_1y_batch(tickers):
    """1Y total return (fraction) for many tickers via yfinance multi-download.

    Returns {SYMBOL: float|None}. Used by Lab pool ranking (ret_1y mode).
    Prefer multi-ticker download batches over serial history() for speed.
    """
    cleaned = []
    seen = set()
    for raw in tickers or []:
        t = str(raw or "").strip().upper().replace("/", "-")
        if not t or t in seen:
            continue
        seen.add(t)
        cleaned.append(t)
    out = {t: None for t in cleaned}
    if not cleaned:
        return out

    def _ret_from_close(closes):
        try:
            s = closes.dropna()
            if len(s) < 2:
                return None
            a = float(s.iloc[0])
            b = float(s.iloc[-1])
            if a <= 0 or b <= 0:
                return None
            return (b / a) - 1.0
        except Exception:  # noqa: BLE001
            return None

    batch = 50
    for i in range(0, len(cleaned), batch):
        chunk = cleaned[i : i + batch]
        try:
            data = yf.download(
                tickers=" ".join(chunk),
                period="1y",
                group_by="ticker",
                auto_adjust=True,
                threads=True,
                progress=False,
            )
        except Exception as e:  # noqa: BLE001
            _dbg(f"⚠️ ret_1y download batch: {e}")
            # Fallback: serial history for this chunk
            for sym in chunk:
                try:
                    hist = yf.Ticker(sym).history(period="1y", auto_adjust=True)
                    if hist is not None and not hist.empty and "Close" in hist.columns:
                        out[sym] = _ret_from_close(hist["Close"])
                except Exception as e2:  # noqa: BLE001
                    _dbg(f"⚠️ ret_1y {sym}: {e2}")
            time.sleep(0.35)
            continue

        if data is None or getattr(data, "empty", True):
            time.sleep(0.25)
            continue

        if len(chunk) == 1:
            sym = chunk[0]
            if "Close" in getattr(data, "columns", []):
                out[sym] = _ret_from_close(data["Close"])
        else:
            cols = data.columns
            if getattr(cols, "nlevels", 1) >= 2:
                level0 = set(str(x) for x in cols.get_level_values(0))
                if any(t in level0 for t in chunk):
                    for sym in chunk:
                        try:
                            if sym in data.columns.get_level_values(0):
                                sub = data[sym]
                                if "Close" in sub.columns:
                                    out[sym] = _ret_from_close(sub["Close"])
                        except Exception:  # noqa: BLE001
                            pass
                else:
                    for sym in chunk:
                        try:
                            if ("Close", sym) in data.columns:
                                out[sym] = _ret_from_close(data[("Close", sym)])
                            elif (sym, "Close") in data.columns:
                                out[sym] = _ret_from_close(data[(sym, "Close")])
                        except Exception:  # noqa: BLE001
                            pass

        if i and i % 200 == 0:
            time.sleep(0.5)
        else:
            time.sleep(0.2)

    n_ok = sum(1 for v in out.values() if v is not None)
    _dbg(f"✅ returns 1y {n_ok}/{len(cleaned)}")
    return out


def get_usd_twd_rate():
    """USD→TWD spot via yfinance (TWD=X = TWD per 1 USD)."""
    _dbg("💱 yfinance FX TWD=X")
    for sym in ("TWD=X", "USDTWD=X"):
        try:
            t = yf.Ticker(sym)
            hist = t.history(period="5d")
            if hist is not None and not hist.empty:
                px = float(hist["Close"].dropna().iloc[-1])
                if px > 0:
                    _dbg(f"✅ FX {sym} = {px}")
                    return px
            info = t.info or {}
            for key in ("regularMarketPrice", "previousClose", "open"):
                v = info.get(key)
                if v is not None:
                    px = float(v)
                    if px > 0:
                        _dbg(f"✅ FX {sym} info.{key} = {px}")
                        return px
        except Exception as e:
            _dbg(f"⚠️ FX {sym}: {e}")
    _dbg("⚠️ FX unavailable (refuse silent fallback)")
    return None


def get_price_history(ticker="AMZN", period="5y"):
    """Daily OHLCV for backtests — plain lists for reticulate."""
    _dbg(f"📈 yfinance price history: {ticker} period={period}")
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


def get_last_quotes(tickers=None):
    """
    Last closes for macro index KPI strip (native Yahoo currency; no FX).
    Returns parallel lists for reticulate: Symbol, Last, PrevClose, ChangePct.
    """
    if tickers is None:
        tickers = ["^GSPC", "^IXIC", "^DJI", "^SOX"]
    if isinstance(tickers, str):
        tickers = [tickers]
    syms, lasts, prevs, chgs = [], [], [], []
    for raw in list(tickers):
        sym = str(raw or "").strip()
        if not sym:
            continue
        last = None
        prev = None
        try:
            hist = yf.Ticker(sym).history(period="10d", auto_adjust=True)
            if hist is not None and not hist.empty and "Close" in hist.columns:
                closes = [float(x) for x in hist["Close"].tolist() if pd.notna(x)]
                if closes:
                    last = closes[-1]
                    if len(closes) >= 2:
                        prev = closes[-2]
        except Exception as e:
            _dbg(f"⚠️ get_last_quotes {sym}: {e}")
        chg = None
        if last is not None and prev is not None and prev != 0:
            chg = 100.0 * (last / prev - 1.0)
        syms.append(sym)
        lasts.append(last)
        prevs.append(prev)
        chgs.append(chg)
    return {
        "Symbol": syms,
        "Last": lasts,
        "PrevClose": prevs,
        "ChangePct": chgs,
    }


def get_tw_10y_gov_bond_yield():
    """
    Latest Taiwan 10Y government bond yield (%) from TPEx daily Curve XLS.

    Flow: POST /www/zh-tw/bond/govDaily2 (fileCode=Curve) → download newest
    Curve.*.xls → sheet「含息殖利率曲線」row「10年(Year)」.
    """
    import datetime as _dt
    import io
    import tempfile

    try:
        import requests
    except Exception as e:
        raise RuntimeError(f"requests unavailable for TW Rf: {e}") from e
    try:
        import xlrd
    except Exception as e:
        raise RuntimeError(f"xlrd unavailable for TW Rf XLS: {e}") from e

    sess = requests.Session()
    sess.verify = False  # TPEx cert chain often fails on cloud hosts
    sess.headers.update(
        {
            "User-Agent": (
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
            ),
            "Accept": "application/json, text/plain, */*",
            "X-Requested-With": "XMLHttpRequest",
            "Referer": "https://www.tpex.org.tw/zh-tw/bond/info/statistics-gb/day/yield.html",
        }
    )
    # Suppress only the verify=False warning noise
    try:
        import urllib3

        urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)
    except Exception:
        pass

    api = "https://www.tpex.org.tw/www/zh-tw/bond/govDaily2"
    today = _dt.date.today()
    last_err = None
    meta = None
    for months_back in range(0, 4):
        y = today.year
        m = today.month - months_back
        while m <= 0:
            m += 12
            y -= 1
        date_param = f"{y:04d}/{m:02d}/01"
        try:
            r = sess.post(
                api,
                data={
                    "date": date_param,
                    "fileCode": "Curve",
                    "id": "",
                    "response": "json",
                },
                timeout=45,
            )
            r.raise_for_status()
            meta = r.json()
            tables = meta.get("tables") or []
            if tables and (tables[0].get("data") or []):
                _dbg(f"📊 TPEx govDaily2 OK date={date_param} rows={len(tables[0]['data'])}")
                break
            last_err = f"empty tables for {date_param}: {meta}"
            meta = None
        except Exception as e:
            last_err = str(e)
            meta = None
    if not meta:
        raise RuntimeError(f"TPEx govDaily2 failed: {last_err}")

    rows = (meta.get("tables") or [{}])[0].get("data") or []
    if not rows:
        raise RuntimeError("TPEx Curve list empty")
    xls_rel = rows[0][1] if isinstance(rows[0], (list, tuple)) and len(rows[0]) > 1 else None
    if not xls_rel:
        raise RuntimeError(f"TPEx Curve row missing xls path: {rows[0]!r}")
    xls_url = (
        xls_rel
        if str(xls_rel).startswith("http")
        else "https://www.tpex.org.tw" + str(xls_rel)
    )
    _dbg(f"📊 TPEx Curve XLS {xls_url}")
    xr = sess.get(xls_url, timeout=60)
    xr.raise_for_status()
    raw = xr.content
    if not raw or len(raw) < 100:
        raise RuntimeError("TPEx Curve XLS empty")

    book = xlrd.open_workbook(file_contents=raw)
    # Prefer on-the-run Treasury Yield Curve sheet
    sheet = None
    for name in book.sheet_names():
        if "Treasury Yield Curve" in name or "含息殖利率" in name:
            sheet = book.sheet_by_name(name)
            break
    if sheet is None:
        sheet = book.sheet_by_index(0)

    tenors = []
    for r in range(sheet.nrows):
        cells = [sheet.cell_value(r, c) for c in range(sheet.ncols)]
        tenor = str(cells[1]) if len(cells) > 1 else ""
        if "10" in tenor and ("年" in tenor or "Year" in tenor or "Y" in tenor.upper()):
            yld = cells[2] if len(cells) > 2 else None
            try:
                yv = float(yld)
            except Exception:
                continue
            if yv > 0:
                # XLS stores percent (e.g. 1.9205), not decimal
                if yv < 0.5:
                    yv = yv * 100.0
                _dbg(f"✅ TW 10Y gov bond yield {yv}% (tenor={tenor})")
                return float(yv)
            tenors.append(tenor)
    raise RuntimeError(f"10Y row not found in Curve XLS (tenors seen: {tenors[:8]})")


def get_risk_free_rate_yf(market="US"):
    """
    Risk-free rate via live scrape.
    US: 10Y Treasury (^TNX) via yfinance.
    TW: 10Y government bond yield via TPEx Curve XLS (recent trading day).
    """
    m = str(market or "US").strip().upper()
    if m in ("TW", "TWN", "TAIWAN"):
        _dbg("📊 TW Rf via TPEx 10Y Curve")
        return float(get_tw_10y_gov_bond_yield())
    _dbg("📊 yfinance Rf ^TNX")
    tnx = yf.Ticker("^TNX")
    # prefer fast_info / history last close
    try:
        hist = tnx.history(period="5d")
        if hist is not None and not hist.empty:
            return float(hist["Close"].dropna().iloc[-1])
    except Exception as e:
        _dbg(f"history fail: {e}")
    info = tnx.info or {}
    for key in ("regularMarketPrice", "previousClose", "open"):
        v = info.get(key)
        if v is not None:
            try:
                return float(v)
            except Exception:
                pass
    raise RuntimeError("cannot fetch ^TNX")


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
    _dbg(f"📐 beta unlever inputs: {tk}")
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
    # 反轉成與 trim_financial_table 相同：營收在上、裁切點在下。
    data = list(reversed(data))

    return {"columns": [str(c) for c in out.columns.tolist()], "data": data}


def scrape_all_financials_yf(ticker="AMZN"):
    """Cloud-safe financials via Yahoo Finance API (no Chrome)."""
    _dbg(f"📊 使用 yfinance 獲取 {ticker} 財報（app_11.0）...")
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

    # Quote vs reporting currency (ADR often differs) — prefer live info, never invent USD.
    info = {}
    try:
        info = stock.info or {}
    except Exception:
        info = {}

    def _ccy_info(key):
        v = info.get(key) if isinstance(info, dict) else None
        if v is None:
            return ""
        s = str(v).strip().upper()
        return s if s else ""

    quote_ccy = _ccy_info("currency")
    fin_ccy = _ccy_info("financialCurrency")
    tk_u = str(ticker or "").strip().upper()
    if not quote_ccy and tk_u.endswith((".TW", ".TWO")):
        quote_ccy = "TWD"
    if not fin_ccy and tk_u.endswith((".TW", ".TWO")):
        fin_ccy = "TWD"
    if not quote_ccy:
        quote_ccy = "USD"
    # Leave fin_ccy empty when unknown — R must not assume quote == statement.

    _dbg(
        f"✅ financials income_rows={len(income_p['data'])} "
        f"bs_rows={len(balance_p['data'])} cf_rows={len(cash_p['data'])} "
        f"ccy={quote_ccy}/{fin_ccy or 'NA'}"
    )

    # UI expects collapsed/expanded; API provides one granularity → reuse payload
    return {
        "Income Statement": {"collapsed": income_p, "expanded": income_p},
        "Balance Sheet": {"collapsed": balance_p, "expanded": balance_p},
        "Cash Flow": {"collapsed": cash_p, "expanded": cash_p},
        "_meta": {
            "currency": quote_ccy,
            "financialCurrency": fin_ccy,
            "quote_currency": quote_ccy,
            "financial_currency": fin_ccy,
        },
    }


def scrape_all_financials(ticker="AMZN"):
    """Yahoo statements via yfinance HTTP only. Never launches a browser."""
    empty_payload = {"columns": ["Breakdown"], "data": []}
    empty = {
        "Income Statement": {"collapsed": empty_payload, "expanded": empty_payload},
        "Balance Sheet": {"collapsed": empty_payload, "expanded": empty_payload},
        "Cash Flow": {"collapsed": empty_payload, "expanded": empty_payload},
    }

    def _has_rows(payload):
        try:
            return isinstance(payload, dict) and len(payload.get("data") or []) > 0
        except Exception:
            return False

    try:
        result = scrape_all_financials_yf(ticker)
        has_data = any(
            _has_rows((result.get(k) or {}).get("expanded"))
            for k in ("Income Statement", "Balance Sheet", "Cash Flow")
        )
        if has_data:
            return result
        _dbg("⚠️ yfinance 回傳空表（不啟動瀏覽器）")
    except Exception as e:
        _dbg(f"⚠️ yfinance 失敗: {e}")
    return empty

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
        _dbg(f"⚠️ search_tickers failed ({q}): {e}")
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


def _sec_get(session, url, is_json=False, tries=3, timeout=30):
    import time
    last = None
    n_tries = max(1, int(tries or 1))
    to = 30 if timeout is None else float(timeout)
    for attempt in range(n_tries):
        try:
            r = session.get(url, timeout=to)
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


# Lab Search only needs operating-segment / revenue-disaggregation notes.
# Downloading every 10-K/20-F note blocks the Shiny event loop and drops iOS sessions.
_SEC_SEGMENT_NOTE_HINTS = (
    "segment", "disaggregat", "revenue from contract", "products and service",
    "product revenue", "revenue by", "net sales by", "operating segment",
    "revenues by", "sales by reportable",
)


def _sec_note_matches_filter(short_name, note_filter):
    if not note_filter:
        return True
    kind = str(note_filter).strip().lower()
    if kind in ("segment", "segments"):
        s = (short_name or "").lower()
        return any(h in s for h in _SEC_SEGMENT_NOTE_HINTS)
    return True


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
        "segment_tables_json": "[]",
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


def _sec_segment_kind(short_name):
    """Map a filing-note title to a generic disclosure kind. None = skip tables.

    Geography footnotes (customer location / countries) are not ASC 280 operating
    segments even when the XBRL title is nested under "Segment Information - …".
    Non-revenue rollforwards (goodwill, PP&E, D&A) are skipped.
    """
    s = (short_name or "").lower()
    if any(h in s for h in ("insider trading", "cybersecurity")):
        return None
    if any(k in s for k in (
        "goodwill", "intangible", "property and equipment",
        "depreciation and amortization", "segment assets",
        "net additions", "additional information",
    )) and not any(k in s for k in ("net sales", "revenue", "operating income")):
        return None
    if any(k in s for k in (
        "geograph", "by country", "by region", "customer location",
        "attributed to countries", "countries representing",
    )):
        return "geography"
    if any(k in s for k in (
        "disaggregat", "revenue by", "net sales by", "net revenue by",
        "groups of similar",
    )):
        return "revenue_disaggregation"
    if "product" in s or "service" in s:
        return "product_service"
    if "segment" in s:
        return "operating_segment"
    return None


_SEC_YEAR_TOKEN = _re.compile(r"^(19|20)\d{2}$")
_SEC_CURRENCY_TOKEN = _re.compile(r"^[\$€£¥]$")
_SEC_COUNTRY_NAME = _re.compile(
    r"\b(united states|u\.s\.a?\.?|usa|germany|united kingdom|u\.k\.|japan|"
    r"china|france|canada|taiwan|korea|australia|india|brazil|mexico|italy|"
    r"spain|netherlands|switzerland|sweden|ireland|singapore|hong kong|"
    r"rest of( the)? world|other countries)\b",
    _re.IGNORECASE,
)


def _sec_int_attr(el, *names, default=1):
    for nm in names:
        raw = el.get(nm) if el is not None else None
        if raw is None:
            continue
        try:
            return max(1, int(raw))
        except (TypeError, ValueError):
            continue
    return default


def _sec_direct_rows(table):
    """<tr> of this table only (do not walk nested tables)."""
    out = []
    for child in getattr(table, "children", []):
        name = getattr(child, "name", None)
        if name == "tr":
            out.append(child)
        elif name in ("tbody", "thead", "tfoot"):
            for gc in getattr(child, "children", []):
                if getattr(gc, "name", None) == "tr":
                    out.append(gc)
    if out:
        return out
    return table.find_all("tr", recursive=False)


def _sec_table_grid(table):
    """Expand colspan/rowspan into a rectangular grid of cell texts."""
    occupancy = {}
    trs = _sec_direct_rows(table)
    for ri, tr in enumerate(trs):
        cells = [
            c for c in getattr(tr, "children", [])
            if getattr(c, "name", None) in ("td", "th")
        ]
        if not cells:
            cells = tr.find_all(["td", "th"], recursive=False)
        col = 0
        for cell in cells:
            while (ri, col) in occupancy:
                col += 1
            txt = cell.get_text(" ", strip=True) if cell is not None else ""
            cs = _sec_int_attr(cell, "colspan", "colSpan")
            rs = _sec_int_attr(cell, "rowspan", "rowSpan")
            for dr in range(rs):
                for dc in range(cs):
                    key = (ri + dr, col + dc)
                    if key in occupancy:
                        continue
                    occupancy[key] = txt if dr == 0 and dc == 0 else ""
            col += cs
    if not occupancy:
        return []
    n_r = max(r for r, _c in occupancy) + 1
    n_c = max(c for _r, c in occupancy) + 1
    return [
        [occupancy.get((r, c), "") for c in range(n_c)]
        for r in range(n_r)
    ]


def _sec_parse_cell_number(text):
    t = (text or "").strip()
    if not t or _SEC_CURRENCY_TOKEN.match(t) or t in {
        "—", "–", "-", "--", "n/a", "N/A", "nm", "NM",
    }:
        return None
    neg = t.startswith("(") and t.endswith(")")
    s = t.strip("()")
    s = s.replace("$", "").replace("€", "").replace("£", "").replace("¥", "")
    s = s.replace(",", "").replace("\xa0", "").replace(" ", "")
    if not s or not _re.match(r"^-?\d+(\.\d+)?$", s):
        return None
    try:
        n = float(s)
    except ValueError:
        return None
    if neg and n > 0:
        n = -n
    return n


def _sec_row_label_nums(row):
    """Split a grid row into (label, amount list, year list), ignoring $ cells."""
    label_parts = []
    nums = []
    years = []
    seen_num = False
    for c in row or []:
        t = (c or "").strip()
        if not t or _SEC_CURRENCY_TOKEN.match(t):
            continue
        if _SEC_YEAR_TOKEN.match(t):
            years.append(int(t))
            seen_num = True
            continue
        n = _sec_parse_cell_number(t)
        if n is not None:
            nums.append(n)
            seen_num = True
            continue
        if not seen_num:
            label_parts.append(t)
    label = _re.sub(r"\s+", " ", " ".join(label_parts)).strip()
    return label, nums, years


def _sec_fmt_num(n):
    if n is None or not isinstance(n, (int, float)):
        return ""
    if n != n:  # NaN
        return ""
    if abs(n - round(n)) < 1e-9:
        return str(int(round(n)))
    return ("%f" % n).rstrip("0").rstrip(".")


def _sec_is_junk_grid(grid):
    if not grid:
        return True
    for row in grid[:4]:
        for c in row:
            t = (c or "").strip()
            if len(t) > 400:
                return True
            low = t.lower()
            if low.startswith("- definition") or "xbrli:" in low:
                return True
            if low in {"x"} and len(grid) <= 8:
                # XBRL metadata stub tables start with a lone "X"
                blob = " ".join((x or "") for r in grid[:2] for x in r).lower()
                if "definition" in blob or "namespace prefix" in blob:
                    return True
    return False


def _sec_normalize_financial_table(grid):
    """Rebuild year-column tables from a colspan-expanded grid.

    Filed 10-K notes often put '$' in its own cell and use colspans, so column
    indexes do not line up across rows. Collect label + numeric tokens instead.
    """
    parsed = [_sec_row_label_nums(r) for r in (grid or [])]
    parsed = [p for p in parsed if p[0] or p[1] or p[2]]
    if len(parsed) < 2:
        return None
    if any(len(p[0]) > 400 for p in parsed):
        return None
    year_vals = None
    for _lab, nums, years in parsed:
        if len(years) >= 2 and not nums:
            year_vals = years
            break
        if len(years) >= 2:
            year_vals = years
            break
        if len(nums) >= 2 and all(1900 < n < 2100 and float(n).is_integer() for n in nums):
            if _lab.lower() in ("", "year ended", "december 31", "year ended december 31"):
                year_vals = [int(n) for n in nums]
                break
    if not year_vals:
        return None
    n_y = len(year_vals)
    headers = [""] + [str(y) for y in year_vals]
    rows = []
    for lab, nums, years in parsed:
        if years and not nums and (not lab or "year ended" in lab.lower() or "december" in lab.lower()):
            continue
        if not lab and not nums:
            continue
        if nums:
            amt = list(nums[:n_y])
            if len(amt) < n_y:
                amt = amt + [None] * (n_y - len(amt))
            rows.append([lab] + [_sec_fmt_num(a) for a in amt])
        else:
            rows.append([lab] + [""] * n_y)
    if len(rows) < 2:
        return None
    return {"headers": headers, "rows": rows}


def _sec_html_tables(raw):
    """Preserve filed HTML tables (amounts) that get_text() would otherwise flatten.

    Expands colspan/rowspan, drops XBRL definition stubs and narrative wrappers,
    and normalizes year-column amount tables so '$' cells do not shift amounts.
    """
    from bs4 import BeautifulSoup
    try:
        soup = BeautifulSoup(raw or "", "lxml")
    except Exception:
        soup = BeautifulSoup(raw or "", "html.parser")
    out = []
    seen = set()
    for table in soup.find_all("table"):
        # Skip tables nested inside a table we already walked via descendants
        parent_table = table.find_parent("table")
        grid = _sec_table_grid(table)
        if not grid or _sec_is_junk_grid(grid):
            continue
        nonempty = [r for r in grid if any(str(c).strip() for c in r)]
        if len(nonempty) < 2:
            continue
        normalized = _sec_normalize_financial_table(nonempty)
        if normalized is not None:
            headers = normalized["headers"]
            body = normalized["rows"]
        else:
            # Keep raw grid (currency tokens stripped) for simple name/metric tables.
            stripped = []
            for r in nonempty:
                cells = [
                    c for c in ((x or "").strip() for x in r)
                    if c and not _SEC_CURRENCY_TOKEN.match(c)
                ]
                if cells:
                    stripped.append(cells)
            if len(stripped) < 2:
                continue
            headers = stripped[0]
            body = stripped[1:]
        if not any(body):
            continue
        sig = (
            tuple(headers),
            tuple(tuple(r[:6]) for r in body[:6]),
        )
        if sig in seen:
            continue
        seen.add(sig)
        item = {"headers": headers, "rows": body}
        if parent_table is not None and normalized is None:
            # Nested metadata tables inside a parent note wrapper.
            blob = " ".join(headers + [c for r in body[:2] for c in r]).lower()
            if "namespace prefix" in blob or "xbrli:" in blob:
                continue
        out.append(item)
    return out


def _sec_detect_scale(raw_html):
    t = (raw_html or "").lower()
    if "in billions" in t or "$ billion" in t:
        return 1e9
    if "in millions" in t or "$ million" in t or "$ in millions" in t:
        return 1e6
    if "in thousands" in t or "$ thousand" in t:
        return 1e3
    return 1.0


def _sec_norm_label(text):
    s = _re.sub(r"[^a-z0-9]+", " ", (text or "").lower())
    return _re.sub(r"\s+", " ", s).strip()


def _sec_classify_extracted_table(short_name, note_kind, headers, rows):
    """Per-table kind. Operating-segment P&L is never tagged geography."""
    labels = [(r[0] if r else "") for r in (rows or [])]
    norms = [_sec_norm_label(l) for l in labels]
    lab_blob = " ".join(norms)

    def is_rev_label(n):
        return n in (
            "net sales", "net revenue", "revenue", "sales", "total revenue",
            "total net sales", "total net revenue",
        ) or n.startswith("net sales")

    has_ns = any(is_rev_label(n) for n in norms)
    has_oi = any("operating income" in n or "operating profit" in n for n in norms)
    skip_if_no_rev = (
        "goodwill" in lab_blob
        or "depreciation and amortization" in lab_blob
        or "property and equipment" in lab_blob
        or "net additions" in lab_blob
        or ("segment assets" in lab_blob or "total segment assets" in lab_blob)
    )
    if skip_if_no_rev and not has_ns:
        return None
    date_banner = any(
        n.startswith("year ended") or n.startswith("december") for n in norms
    )
    country_hits_preview = sum(1 for n in norms if _SEC_COUNTRY_NAME.search(n))
    has_corporate = any("corporate" == n or n.startswith("corporate ") for n in norms)
    # Balance-sheet / capex / D&A tables keep a date banner and segment names
    # but no Net sales. Do not skip country footnotes that share the same banner.
    if date_banner and not has_ns and not has_oi and country_hits_preview < 2:
        return None
    if has_corporate and not has_ns:
        return None

    skip_names = {
        "consolidated", "total", "net sales", "net revenue", "revenue", "sales",
        "operating income", "operating income loss", "operating profit",
        "operating expenses", "operating expense", "cost of sales",
        "cost of revenue", "gross profit", "net sales",
    }
    data_names = [
        n for n in norms
        if n and n not in skip_names
        and not n.startswith("year ended")
        and "december" not in n
    ]
    # Filed ASC 280 P&L (segment header rows + Net sales / Operating income).
    if has_ns and has_oi:
        return {
            "kind": "operating_segment",
            "layout": "grouped_metrics",
            "is_customer_location_only": False,
        }
    country_hits = sum(1 for n in data_names if _SEC_COUNTRY_NAME.search(n))
    if data_names and country_hits >= max(2, int(0.8 * len(data_names) + 0.999)):
        return {
            "kind": "geography",
            "layout": "stub",
            "is_customer_location_only": True,
        }
    has_ns_banner = any(
        _re.match(r"net sales:?$", (l or "").strip(), _re.IGNORECASE) for l in labels
    )
    if has_ns_banner and not has_oi and len(data_names) >= 2:
        return {
            "kind": "revenue_disaggregation",
            "layout": "stub",
            "is_customer_location_only": False,
        }
    if note_kind:
        return {
            "kind": note_kind,
            "layout": "stub",
            "is_customer_location_only": note_kind == "geography",
        }
    return None


def _sec_append_segment_tables(result, short_name, raw_html):
    kind = _sec_segment_kind(short_name)
    if not kind:
        return
    tables = _sec_html_tables(raw_html)
    if not tables:
        return
    scale = _sec_detect_scale(raw_html)
    try:
        import json
        existing = json.loads(result.get("segment_tables_json") or "[]")
    except Exception:
        existing = []
    for tbl in tables:
        headers = tbl.get("headers") or []
        rows = tbl.get("rows") or []
        role = _sec_classify_extracted_table(short_name, kind, headers, rows)
        if not role:
            continue
        existing.append({
            "short_name": str(short_name or ""),
            "kind": role.get("kind") or kind,
            "layout": role.get("layout") or "",
            "is_customer_location_only": bool(role.get("is_customer_location_only")),
            "scale": scale,
            "headers": headers,
            "rows": rows,
        })
    try:
        import json
        result["segment_tables_json"] = json.dumps(existing)
    except Exception:
        pass


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


def _sec_extract_notes_from_summary(session, folder, result, max_chars,
                                    note_filter=None, deadline=None,
                                    get_tries=3, get_timeout=30):
    """Fill result lists from FilingSummary.xml Notes; returns True if any note found."""
    import time
    from bs4 import BeautifulSoup
    try:
        xml = _sec_get(
            session, f"{folder}/FilingSummary.xml",
            tries=get_tries, timeout=get_timeout,
        )
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
        if deadline is not None and time.monotonic() >= float(deadline):
            break
        cat = _find(rep, "MenuCategory")
        if not cat or (cat.text or "").strip().lower() != "notes":
            continue
        htmf = _find(rep, "HtmlFileName")
        if not htmf or not htmf.text:
            continue
        short = _find(rep, "ShortName")
        short_name = short.text if short else ""
        if not _sec_note_matches_filter(short_name, note_filter):
            continue
        url = f"{folder}/{htmf.text}"
        raw_html = ""
        try:
            raw_html = _sec_get(
                session, url, tries=get_tries, timeout=get_timeout,
            )
            text = _sec_clean_note_text(raw_html)
        except Exception as e:  # noqa: BLE001
            text = f"(failed to fetch note: {e})"
        _sec_append_text_item(
            result, short_name, url, text, max_chars,
            important=bool(_sec_is_important(short_name)),
        )
        if raw_html:
            _sec_append_segment_tables(result, short_name, raw_html)
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


def sec_report_notes(ticker="AAPL", form="10-K", max_chars=1500,
                     note_filter=None, max_seconds=None):
    """Fetch a US stock's latest annual (10-K/20-F/40-F), interim (10-Q), or
    material disclosures (8-K/6-K) from SEC EDGAR. Annual/interim extract Notes;
    material filings fall back to primary-document body text.
    Returns a dict of parallel lists that reticulate converts cleanly to R.

    note_filter='segment' downloads only segment / revenue-disaggregation notes
    (used by Business Breakdown Lab Search so the Shiny session is not blocked).
    max_seconds is a wall-clock budget; remaining notes are skipped.
    """
    import time
    tk = str(ticker or "").strip().upper()
    form = str(form or "10-K").strip().upper()
    try:
        max_chars = int(max_chars)
    except Exception:
        max_chars = 1500
    deadline = None
    if max_seconds is not None:
        try:
            budget = float(max_seconds)
            if budget > 0:
                deadline = time.monotonic() + budget
        except Exception:
            deadline = None
    segment_fast = str(note_filter or "").strip().lower() in ("segment", "segments")
    get_tries = 1 if segment_fast else 3
    get_timeout = 12 if segment_fast else 30
    if not tk:
        return _sec_empty_result("empty ticker")
    if form not in _SEC_FORM_CANDIDATES:
        form = "10-K"
    candidates = _SEC_FORM_CANDIDATES[form]
    requested_form = form

    _dbg(f"📄 SEC EDGAR notes: {tk} {form} (try {', '.join(candidates)})")
    try:
        session = _sec_session()

        # 1. ticker -> CIK
        tickers = _sec_get(
            session, _SEC_TICKERS_URL, is_json=True,
            tries=get_tries, timeout=get_timeout,
        )
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
        sub = _sec_get(
            session, _SEC_SUBMISSIONS_URL.format(cik=cik), is_json=True,
            tries=get_tries, timeout=get_timeout,
        )
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
        has_notes = _sec_extract_notes_from_summary(
            session, folder, result, max_chars,
            note_filter=note_filter, deadline=deadline,
            get_tries=get_tries, get_timeout=get_timeout,
        )
        if not has_notes:
            if segment_fast:
                # Yahoo statements still render; missing segment notes is not a hard fail.
                result["ok"] = True
            elif _sec_is_material_form(form):
                ok_body = _sec_extract_primary_body(
                    session, folder, primary_doc, form, filing_date, result, max_chars
                )
                if not ok_body and not result["short_names"]:
                    result["ok"] = False
                    result["error"] = f"failed to extract body from {form} primary document"
            else:
                result["ok"] = False
                result["error"] = "no Notes section found in FilingSummary.xml"
        _dbg(f"✅ SEC notes: {tk} {form} rows={len(result['short_names'])}")
        return result
    except Exception as e:  # noqa: BLE001
        _dbg(f"⚠️ sec_report_notes failed ({tk} {form}): {e}")
        return _sec_empty_result(str(e))


def get_cluster_features_batch(tickers):
    """Yahoo ratio features for Clustering Lab.

    Returns a pandas DataFrame (reticulate-friendly) with columns:
      ticker, name, market_cap,
      ROE, Operating_Margin, Rev_YoY, OpInc_YoY, Debt_Ratio, PE_Ratio, PB_Ratio
    Percent-style fields are percent points (15.0 = 15%).
    Uses a small thread pool + 24h disk cache so shinyapps can finish N≈25.
    """
    import json
    import tempfile
    import time
    from concurrent.futures import ThreadPoolExecutor, as_completed
    from pathlib import Path

    cleaned = []
    seen = set()
    for raw in tickers or []:
        # reticulate may pass a single string or list/tuple/Series
        if isinstance(raw, (list, tuple)):
            seq = raw
        else:
            seq = [raw]
        for item in seq:
            t = str(item or "").strip().upper().replace("/", "-")
            if not t or t in seen:
                continue
            seen.add(t)
            cleaned.append(t)

    def _sf(v):
        try:
            if v is None:
                return None
            x = float(v)
            if x != x:
                return None
            return x
        except (TypeError, ValueError):
            return None

    def _pct_points(v):
        x = _sf(v)
        if x is None:
            return None
        if abs(x) <= 1.5:
            return x * 100.0
        return x

    def _debt_pct(v):
        x = _sf(v)
        if x is None:
            return None
        if abs(x) < 5:
            return x * 100.0
        return x

    def _empty(sym):
        return {
            "ticker": sym,
            "name": None,
            "market_cap": None,
            "ROE": None,
            "Operating_Margin": None,
            "Rev_YoY": None,
            "OpInc_YoY": None,
            "Debt_Ratio": None,
            "PE_Ratio": None,
            "PB_Ratio": None,
        }

    cache_dir = Path(tempfile.gettempdir()) / "ynow_cluster_feat_cache"
    try:
        cache_dir.mkdir(parents=True, exist_ok=True)
    except Exception:  # noqa: BLE001
        cache_dir = None
    ttl = 24 * 3600

    def _cache_get(sym):
        if cache_dir is None:
            return None
        p = cache_dir / f"{sym.replace('.', '_')}.json"
        try:
            if not p.is_file() or time.time() - p.stat().st_mtime > ttl:
                return None
            return json.loads(p.read_text(encoding="utf-8"))
        except Exception:  # noqa: BLE001
            return None

    def _cache_put(sym, row):
        if cache_dir is None:
            return
        p = cache_dir / f"{sym.replace('.', '_')}.json"
        try:
            p.write_text(json.dumps(row), encoding="utf-8")
        except Exception:  # noqa: BLE001
            pass

    def _yahoo_raw(v):
        """Unwrap quoteSummary {raw,fmt} shells or plain scalars."""
        if isinstance(v, dict) and "raw" in v:
            return v.get("raw")
        return v

    def _row_usable(r):
        if not isinstance(r, dict):
            return False
        n_fin = 0
        for k in (
            "ROE",
            "Operating_Margin",
            "Rev_YoY",
            "OpInc_YoY",
            "Debt_Ratio",
            "PE_Ratio",
            "PB_Ratio",
        ):
            if r.get(k) is not None:
                n_fin += 1
        return n_fin >= 2

    def _one(sym, allow_quote_summary=True):
        cached = _cache_get(sym)
        if isinstance(cached, dict) and _row_usable(cached):
            cached["ticker"] = sym
            return cached
        row = _empty(sym)

        def _fill_from_info(info):
            if not isinstance(info, dict):
                return False
            row["name"] = _best_company_name(info, sym)
            row["market_cap"] = _sf(info.get("marketCap"))
            row["ROE"] = _pct_points(info.get("returnOnEquity"))
            row["Operating_Margin"] = _pct_points(info.get("operatingMargins"))
            row["Rev_YoY"] = _pct_points(info.get("revenueGrowth"))
            row["OpInc_YoY"] = _pct_points(
                info.get("earningsQuarterlyGrowth")
                if info.get("earningsQuarterlyGrowth") is not None
                else info.get("earningsGrowth")
            )
            row["Debt_Ratio"] = _debt_pct(info.get("debtToEquity"))
            pe = _sf(
                info.get("trailingPE")
                if info.get("trailingPE") is not None
                else info.get("forwardPE")
            )
            pb = _sf(info.get("priceToBook"))
            if pe is not None and pe > 0:
                row["PE_Ratio"] = pe
            if pb is not None and pb > 0:
                row["PB_Ratio"] = pb
            return _row_usable(row) or (
                row.get("ROE") is not None
                or row.get("PE_Ratio") is not None
                or row.get("Operating_Margin") is not None
            )

        def _fill_from_quote_summary():
            """Authenticated quoteSummary via yfinance YfData (crumb+cookie).

            Plain R/httr calls without crumb get 401 Invalid Crumb and empty shells.
            """
            try:
                from yfinance.data import YfData

                yd = YfData()
                # Prefer query2; fall back to query1 on soft failure
                last_err = None
                for host in (
                    "https://query2.finance.yahoo.com",
                    "https://query1.finance.yahoo.com",
                ):
                    try:
                        url = f"{host}/v10/finance/quoteSummary/{sym}"
                        raw = yd.get(
                            url,
                            params={
                                "modules": (
                                    "financialData,defaultKeyStatistics,"
                                    "summaryDetail,price"
                                )
                            },
                        )
                        if hasattr(raw, "json") and not isinstance(raw, dict):
                            raw = raw.json()
                        if not isinstance(raw, dict):
                            continue
                        results = (raw.get("quoteSummary") or {}).get("result") or []
                        if not results:
                            err = (raw.get("quoteSummary") or {}).get("error") or {}
                            last_err = err.get("description") or err.get("code")
                            continue
                        mod = results[0] or {}
                        fd = mod.get("financialData") or {}
                        ks = mod.get("defaultKeyStatistics") or {}
                        sd = mod.get("summaryDetail") or {}
                        pr = mod.get("price") or {}
                        info = {
                            "shortName": _yahoo_raw(pr.get("shortName"))
                            or _yahoo_raw(pr.get("longName")),
                            "longName": _yahoo_raw(pr.get("longName")),
                            "marketCap": _yahoo_raw(
                                pr.get("marketCap") or ks.get("enterpriseValue")
                            ),
                            "returnOnEquity": _yahoo_raw(fd.get("returnOnEquity")),
                            "operatingMargins": _yahoo_raw(fd.get("operatingMargins")),
                            "revenueGrowth": _yahoo_raw(fd.get("revenueGrowth")),
                            "earningsGrowth": _yahoo_raw(fd.get("earningsGrowth")),
                            "earningsQuarterlyGrowth": _yahoo_raw(
                                ks.get("earningsQuarterlyGrowth")
                            ),
                            "debtToEquity": _yahoo_raw(fd.get("debtToEquity")),
                            "trailingPE": _yahoo_raw(sd.get("trailingPE")),
                            "forwardPE": _yahoo_raw(ks.get("forwardPE")),
                            "priceToBook": _yahoo_raw(
                                sd.get("priceToBook") or ks.get("priceToBook")
                            ),
                        }
                        return _fill_from_info(info)
                    except Exception as e:  # noqa: BLE001
                        last_err = str(e)
                        continue
                if last_err:
                    _dbg(f"⚠️ cluster quoteSummary {sym}: {last_err}")
            except Exception as e:  # noqa: BLE001
                _dbg(f"⚠️ cluster quoteSummary {sym}: {e}")
            return False

        try:
            ok = False
            # Retry: Yahoo / yfinance intermittently returns empty .info for TW/TWO
            for attempt in range(3):
                try:
                    tk = yf.Ticker(sym)
                    info = {}
                    try:
                        info = tk.info or {}
                    except Exception as e:  # noqa: BLE001
                        _dbg(f"⚠️ cluster info {sym} attempt={attempt}: {e}")
                        info = {}
                    if not isinstance(info, dict):
                        info = {}
                    ok = _fill_from_info(info)
                    if ok:
                        break
                    # Second pass: get_info() when available (some yfinance builds)
                    if attempt < 2 and hasattr(tk, "get_info"):
                        try:
                            info2 = tk.get_info() or {}
                            if _fill_from_info(info2):
                                ok = True
                                break
                        except Exception as e2:  # noqa: BLE001
                            _dbg(f"⚠️ cluster get_info {sym}: {e2}")
                    if attempt < 2:
                        time.sleep(0.25 + 0.2 * attempt)
                except Exception as e:  # noqa: BLE001
                    _dbg(f"⚠️ cluster feature {sym} attempt={attempt}: {e}")
                    if attempt < 2:
                        time.sleep(0.25 + 0.2 * attempt)
            # Crumb-authenticated quoteSummary when .info stayed empty
            if not ok and allow_quote_summary:
                ok = _fill_from_quote_summary()
            if ok:
                _cache_put(sym, row)
        except Exception as e:  # noqa: BLE001
            _dbg(f"⚠️ cluster feature {sym}: {e}")
        return row

    if not cleaned:
        return pd.DataFrame(columns=list(_empty("").keys()))

    # Warm crumb/cookie once so first .info / quoteSummary is less likely to 401
    try:
        from yfinance.data import YfData

        YfData().get(
            "https://query2.finance.yahoo.com/v7/finance/quote",
            params={"symbols": cleaned[0]},
        )
    except Exception as e:  # noqa: BLE001
        _dbg(f"⚠️ cluster warm session: {e}")

    # Fewer workers reduces Yahoo 429 bursts (esp. TW / N=100)
    workers = 1 if len(cleaned) > 25 else min(2, max(1, len(cleaned)))
    out_map = {}
    with ThreadPoolExecutor(max_workers=workers) as ex:
        futs = {ex.submit(_one, s): s for s in cleaned}
        for fut in as_completed(futs):
            sym = futs[fut]
            try:
                out_map[sym] = fut.result()
            except Exception as e:  # noqa: BLE001
                _dbg(f"⚠️ cluster worker {sym}: {e}")
                out_map[sym] = _empty(sym)

    # Serial gap-fill for names that still lack ≥2 finite ratios
    misses = [s for s in cleaned if not _row_usable(out_map.get(s))]
    if misses:
        _dbg(f"↻ cluster serial gap-fill {len(misses)}/{len(cleaned)}")
        for i, sym in enumerate(misses):
            try:
                time.sleep(0.15 if i else 0.0)
                out_map[sym] = _one(sym, allow_quote_summary=True)
            except Exception as e:  # noqa: BLE001
                _dbg(f"⚠️ cluster gap-fill {sym}: {e}")
                out_map[sym] = out_map.get(sym) or _empty(sym)

    out = [out_map.get(s, _empty(s)) for s in cleaned]
    n_ok = sum(1 for r in out if _row_usable(r))
    _dbg(f"✅ cluster features {n_ok}/{len(out)}")
    try:
        return pd.DataFrame(out)
    except Exception:  # noqa: BLE001
        return out

