"""
TPEx 上櫃／興櫃「財務資料簡報」fetcher（官方季報彙總，非完整 IS／BS／CF）。

Page: https://www.tpex.org.tw/zh-tw/mainboard/listed/financial/summary.html
Index API: GET /www/zh-tw/statistics/financial?date=YYYY
Bulk XLS:
  上櫃 O_YYYYQn.xls  → /storage/statistic/financial/O_YYYYQn.xls
  興櫃 U_YYYYQn.xls  → /storage/statistic/financial/U_YYYYQn.xls

Exposed fields (YTD cumulative; money in NT$ thousand except EPS／BVPS／ratios):
  營業收入、營業利益、營業外收支、稅後純益、期末股本、每股稅後純益、每股淨值、
  淨值／總資產、流動比率、速動比率。

Does NOT provide CapEx、FCF、現金、負債明細、完整三表科目 — do not invent them.
"""

from __future__ import annotations

import json
import os
import re
import tempfile
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

import requests

try:
    import xlrd
except Exception:  # pragma: no cover
    xlrd = None

TPEX_ORIGIN = "https://www.tpex.org.tw"
TPEX_INDEX_URL = f"{TPEX_ORIGIN}/www/zh-tw/statistics/financial"
TPEX_XLS_TMPL = f"{TPEX_ORIGIN}/storage/statistic/financial/{{prefix}}_{{yyyy}}Q{{q}}.xls"
TPEX_UA = "theYNowApp/15.0 (TPEx financial summary; research)"

# XLS column layout (row of company data; header spans rows 0–7)
COL = {
    "code": 0,
    "name": 1,
    "revenue_cur": 2,
    "revenue_prior": 3,
    "revenue_chg_pct": 4,
    "op_income_cur": 5,
    "op_income_prior": 6,
    "nonop_cur": 7,
    "nonop_prior": 8,
    "ni_cur": 9,
    "ni_prior": 10,
    "ni_chg_pct": 11,
    "capital_stock_k": 12,
    "eps_cur": 13,
    "eps_prior": 14,
    "bvps": 15,
    "equity_to_assets_pct": 16,
    "current_ratio": 17,
    "quick_ratio": 18,
}

# Unmapped / not inventable from this summary (document for UI／agents)
TPEX_UNMAPPED_FIELDS = (
    "CapEx",
    "Free Cash Flow",
    "Depreciation And Amortization",
    "Change In Working Capital",
    "Cash And Cash Equivalents",
    "Total Debt",
    "Total Assets (absolute; only equity/assets % is published)",
    "full Income Statement line items",
    "full Balance Sheet line items",
    "Cash Flow statement (entirety)",
)

_CACHE_DIR = Path(tempfile.gettempdir()) / "ynow_tpex_fs_cache"
_MEM: Dict[str, Tuple[float, Any]] = {}
_MEM_TTL = 6 * 3600
_DISK_TTL = 24 * 3600


def _dbg(*args, **kwargs):
    if os.environ.get("YNOW_DEBUG", "").strip() in ("1", "true", "TRUE", "yes", "on"):
        print(*args, **kwargs)


def _session() -> requests.Session:
    s = requests.Session()
    s.headers.update(
        {
            "User-Agent": TPEX_UA,
            "Accept": "application/json,application/octet-stream,*/*",
            "Referer": f"{TPEX_ORIGIN}/zh-tw/mainboard/listed/financial/summary.html",
        }
    )
    return s


def _cache_get(key: str):
    now = time.time()
    hit = _MEM.get(key)
    if hit and now - hit[0] < _MEM_TTL:
        return hit[1]
    path = _CACHE_DIR / f"{key}.bin"
    meta = _CACHE_DIR / f"{key}.meta"
    if path.exists() and meta.exists():
        try:
            ts = float(meta.read_text().strip())
            if now - ts < _DISK_TTL:
                data = path.read_bytes()
                _MEM[key] = (now, data)
                return data
        except Exception:
            pass
    return None


def _cache_set(key: str, data: bytes) -> None:
    try:
        _CACHE_DIR.mkdir(parents=True, exist_ok=True)
        (_CACHE_DIR / f"{key}.bin").write_bytes(data)
        (_CACHE_DIR / f"{key}.meta").write_text(str(time.time()))
        _MEM[key] = (time.time(), data)
    except Exception as e:
        _dbg("tpex cache write fail:", e)


def normalize_tw_code(ticker: str) -> str:
    s = str(ticker or "").strip().upper()
    s = re.sub(r"\.(TW|TWO)$", "", s)
    s = re.sub(r"\D", "", s)
    return s


def period_end_label(year: int, quarter: int) -> str:
    md = {1: "03/31", 2: "06/30", 3: "09/30", 4: "12/31"}.get(int(quarter), "12/31")
    return f"{md}/{int(year)}"


def _to_float(v) -> Optional[float]:
    if v is None:
        return None
    if isinstance(v, (int, float)):
        try:
            fv = float(v)
            if fv != fv:  # NaN
                return None
            return fv
        except Exception:
            return None
    s = str(v).strip().replace(",", "")
    if not s or s in {"+++++", "-----", "N/A", "NA", "-"}:
        return None
    try:
        return float(s)
    except Exception:
        return None


def list_tpex_financial_periods(year: Optional[int] = None, timeout: float = 25.0) -> Dict[str, Any]:
    """Return index JSON: tables for 上櫃／興櫃 with XLS paths."""
    y = int(year or time.gmtime().tm_year)
    cache_key = f"index_{y}"
    cached = _cache_get(cache_key)
    if cached:
        try:
            return json.loads(cached.decode("utf-8"))
        except Exception:
            pass
    sess = _session()
    r = sess.get(TPEX_INDEX_URL, params={"date": str(y)}, timeout=timeout)
    r.raise_for_status()
    data = r.json()
    if not isinstance(data, dict) or data.get("stat") not in (None, "ok"):
        # default year listing sometimes omits date and still returns ok
        if not (isinstance(data, dict) and "tables" in data):
            raise RuntimeError(f"TPEx index 失敗: {data!r}")
    _cache_set(cache_key, json.dumps(data, ensure_ascii=False).encode("utf-8"))
    return data


def _xls_url(board: str, year: int, quarter: int) -> str:
    prefix = "O" if board.upper() in ("O", "OTC", "TPEX", "MAINBOARD", "上櫃") else "U"
    return TPEX_XLS_TMPL.format(prefix=prefix, yyyy=int(year), q=int(quarter))


def download_tpex_xls(board: str, year: int, quarter: int, timeout: float = 60.0) -> bytes:
    url = _xls_url(board, year, quarter)
    # Cache key by file prefix (O/U), not board[0] — "ESB"[0]=="E" collided with OTC.
    prefix = "O" if str(board).upper() in ("O", "OTC", "TPEX", "MAINBOARD", "上櫃") or str(
        board
    ).upper().startswith(("O", "MAIN")) else "U"
    key = f"xls_{prefix}_{year}Q{quarter}"
    cached = _cache_get(key)
    if cached and len(cached) > 1000:
        head = cached[:200].lstrip().lower()
        if head.startswith(b"<!doctype") or head.startswith(b"<html"):
            pass  # fall through to re-download
        else:
            return cached
    sess = _session()
    r = sess.get(url, timeout=timeout)
    if r.status_code != 200 or len(r.content) < 1000:
        raise RuntimeError(f"TPEx XLS 下載失敗 ({r.status_code}): {url}")
    # Reject HTML error pages (index may list a quarter before the file is published)
    head = r.content[:200].lstrip().lower()
    if head.startswith(b"<!doctype") or head.startswith(b"<html"):
        raise RuntimeError(f"TPEx XLS 回傳 HTML（檔案可能尚未上架）: {url}")
    _cache_set(key, r.content)
    return r.content


def tpex_xls_available(board: str, year: int, quarter: int, timeout: float = 15.0) -> bool:
    """True if the bulk XLS is a real OLE compound file (not 404 HTML)."""
    try:
        raw = download_tpex_xls(board, year, quarter, timeout=timeout)
        return isinstance(raw, (bytes, bytearray)) and len(raw) > 1000
    except Exception:
        return False


def _is_company_code(v) -> bool:
    s = str(v).strip()
    return bool(re.fullmatch(r"\d{3,6}", s))


def parse_tpex_xls_bytes(raw: bytes, board: str, year: int, quarter: int) -> List[Dict[str, Any]]:
    if xlrd is None:
        raise RuntimeError("缺少 xlrd，無法解析 TPEx .xls（請安裝 xlrd）")
    book = xlrd.open_workbook(file_contents=raw)
    out: List[Dict[str, Any]] = []
    period = period_end_label(year, quarter)
    for si in range(book.nsheets):
        sh = book.sheet_by_index(si)
        sheet_name = book.sheet_names()[si]
        for r in range(sh.nrows):
            code = str(sh.cell_value(r, COL["code"])).strip()
            if not _is_company_code(code):
                continue
            name = str(sh.cell_value(r, COL["name"])).strip()
            # Industry section headers sometimes have numeric fillers in money cols
            # Skip rows where name looks like industry category (no Chinese company pattern + short)
            row = {
                "code": code,
                "name": name,
                "board": "OTC" if str(board).upper().startswith(("O", "TPEX", "MAIN")) or board == "上櫃" else "ESB",
                "sheet": sheet_name,
                "year": int(year),
                "quarter": int(quarter),
                "period": period,
                "unit": "NTD_thousand",
                "revenue": _to_float(sh.cell_value(r, COL["revenue_cur"])),
                "revenue_prior": _to_float(sh.cell_value(r, COL["revenue_prior"])),
                "op_income": _to_float(sh.cell_value(r, COL["op_income_cur"])),
                "op_income_prior": _to_float(sh.cell_value(r, COL["op_income_prior"])),
                "nonop": _to_float(sh.cell_value(r, COL["nonop_cur"])),
                "ni": _to_float(sh.cell_value(r, COL["ni_cur"])),
                "ni_prior": _to_float(sh.cell_value(r, COL["ni_prior"])),
                "capital_stock_k": _to_float(sh.cell_value(r, COL["capital_stock_k"])),
                "eps": _to_float(sh.cell_value(r, COL["eps_cur"])),
                "eps_prior": _to_float(sh.cell_value(r, COL["eps_prior"])),
                "bvps": _to_float(sh.cell_value(r, COL["bvps"])),
                "equity_to_assets_pct": _to_float(sh.cell_value(r, COL["equity_to_assets_pct"])),
                "current_ratio": _to_float(sh.cell_value(r, COL["current_ratio"])),
                "quick_ratio": _to_float(sh.cell_value(r, COL["quick_ratio"])),
            }
            # Filter industry aggregate-ish rows: revenue tiny filler (==4.0) AND name ends with 工業 etc.
            if row["revenue"] is not None and abs(row["revenue"] - 4.0) < 1e-9 and "工業" in name:
                continue
            out.append(row)
    return out


def _discover_available(board: str, years: Sequence[int], timeout: float) -> List[Tuple[int, int]]:
    """Prefer index API; verify each XLS exists; fall back to probing Q4→Q1."""
    found: List[Tuple[int, int]] = []
    # Prefer explicit board names; avoid startswith("T") matching unrelated labels.
    bu = str(board or "").upper()
    if bu in ("O", "OTC", "TPEX", "TWO_OTC", "MAINBOARD") or board == "上櫃" or bu.startswith("O"):
        prefix = "O"
    else:
        prefix = "U"
    title_key = "上櫃" if prefix == "O" else "興櫃"
    for y in years:
        try:
            idx = list_tpex_financial_periods(y, timeout=timeout)
            for tbl in idx.get("tables") or []:
                if title_key not in str(tbl.get("title") or ""):
                    continue
                for row in tbl.get("data") or []:
                    if not row:
                        continue
                    path = str(row[1] if len(row) > 1 else "")
                    m = re.search(rf"{prefix}_(\d{{4}})Q([1-4])\.xls", path, re.I)
                    if m:
                        yy, qq = int(m.group(1)), int(m.group(2))
                        if "null" in path.lower():
                            continue
                        # Index often lists the next quarter before the file is published (404 HTML).
                        if tpex_xls_available(prefix, yy, qq, timeout=min(timeout, 20.0)):
                            found.append((yy, qq))
                        else:
                            _dbg("index lists missing xls", prefix, yy, qq)
        except Exception as e:
            _dbg("index year fail", y, e)
    # unique, newest first
    uniq = sorted(set(found), key=lambda t: (t[0], t[1]), reverse=True)
    if uniq:
        return uniq
    # probe fallback
    probed = []
    for y in years:
        for q in (4, 3, 2, 1):
            if tpex_xls_available(prefix, y, q, timeout=min(timeout, 20.0)):
                probed.append((y, q))
    return sorted(set(probed), key=lambda t: (t[0], t[1]), reverse=True)


def fetch_tpex_ticker_rows(
    ticker: str,
    board: Optional[str] = None,
    max_periods: int = 8,
    years_back: int = 4,
    timeout: float = 30.0,
) -> Dict[str, Any]:
    """
    Fetch multi-period summary rows for one ticker.

    board: 'OTC' | 'ESB' | None (try OTC then ESB for .TWO / bare codes)
    """
    code = normalize_tw_code(ticker)
    if not code:
        return {"ok": False, "error": "無效代號", "code": "", "rows": []}

    boards: List[str]
    if board:
        b = str(board).upper()
        boards = ["OTC"] if b in ("O", "OTC", "TPEX", "TWO_OTC", "MAINBOARD", "上櫃") else ["ESB"]
    else:
        boards = ["OTC", "ESB"]

    now_y = time.gmtime().tm_year
    years = list(range(now_y, now_y - max(years_back, 1) - 1, -1))
    rows: List[Dict[str, Any]] = []
    used_board = None
    errors: List[str] = []

    for b in boards:
        avail = _discover_available(b, years, timeout=timeout)
        if not avail:
            errors.append(f"{b}: 無可用季報清單")
            continue
        prefix = "O" if b == "OTC" else "U"
        board_rows: List[Dict[str, Any]] = []
        # Prefer annual (Q4) history; allow one latest interim if newer than newest Q4
        q4_hits: List[Dict[str, Any]] = []
        interim_hits: List[Dict[str, Any]] = []
        for yy, qq in avail:
            if len(q4_hits) + len(interim_hits) >= max_periods * 2:
                break
            try:
                raw = download_tpex_xls(prefix, yy, qq, timeout=timeout)
                parsed = parse_tpex_xls_bytes(raw, b, yy, qq)
                hit = [r for r in parsed if r["code"] == code]
                if not hit:
                    continue
                if int(qq) == 4:
                    q4_hits.append(hit[0])
                else:
                    interim_hits.append(hit[0])
            except Exception as e:
                errors.append(f"{b} {yy}Q{qq}: {e}")
                continue
        board_rows = list(q4_hits)
        if interim_hits:
            newest_interim = max(interim_hits, key=lambda r: (r["year"], r["quarter"]))
            newest_q4 = max(q4_hits, key=lambda r: (r["year"], r["quarter"])) if q4_hits else None
            if newest_q4 is None or (newest_interim["year"], newest_interim["quarter"]) > (
                newest_q4["year"],
                newest_q4["quarter"],
            ):
                board_rows.insert(0, newest_interim)
        board_rows = board_rows[:max_periods]
        if board_rows:
            rows = board_rows
            used_board = board_rows[0]["board"]
            break

    if not rows:
        return {
            "ok": False,
            "error": "櫃買季報彙總找不到此代號（可能尚未入表或代號非上櫃／興櫃）",
            "code": code,
            "rows": [],
            "errors": errors[:6],
            "unmapped": list(TPEX_UNMAPPED_FIELDS),
        }

    # Prefer one board; sort newest first
    rows = sorted(rows, key=lambda r: (r["year"], r["quarter"]), reverse=True)
    return {
        "ok": True,
        "code": code,
        "name": rows[0].get("name") or "",
        "board": used_board,
        "rows": rows,
        "source": "tpex_financial_summary_xls",
        "source_url": f"{TPEX_ORIGIN}/zh-tw/mainboard/listed/financial/summary.html",
        "unmapped": list(TPEX_UNMAPPED_FIELDS),
        "coverage_note_zh": (
            "櫃買「財務資料簡報」為截至當季累計摘要（營收／營業利益／稅後純益／股本／EPS／每股淨值等），"
            "不是完整損益表、資產負債表或現金流量表；未提供項目不會捏造。"
        ),
    }


def _k_to_ntd(v: Optional[float]) -> Optional[float]:
    if v is None:
        return None
    return float(v) * 1000.0


def _shares_from_capital_k(capital_k: Optional[float], par: float = 10.0) -> Optional[float]:
    """TW common assumption: 股本(千元) → 股數 = capital_k * 1000 / par."""
    if capital_k is None or not par:
        return None
    return float(capital_k) * 1000.0 / float(par)


def rows_to_yahoo_shaped_statements(payload: Dict[str, Any], par_value: float = 10.0) -> Dict[str, Any]:
    """
    Map TPEx summary rows into Yahoo-like statement payloads (columns/data).

    Mapped:
      IS: Total Revenue, Operating Income, Net Income, Basic EPS, Diluted EPS
      BS: Common Stock, Ordinary Shares Number, Stockholders Equity (derived),
          Book Value Per Share
      CF: empty (honest)

    Stockholders Equity = BVPS × shares（股數由股本／面額推得；面額預設 NT$10）.
    """
    empty = {"columns": ["Breakdown"], "data": []}
    if not payload or not payload.get("ok") or not payload.get("rows"):
        return {
            "Income Statement": {"collapsed": empty, "expanded": empty},
            "Balance Sheet": {"collapsed": empty, "expanded": empty},
            "Cash Flow": {"collapsed": empty, "expanded": empty},
            "_meta": {
                "currency": "TWD",
                "financialCurrency": "TWD",
                "source": "tpex_financial_summary",
                "ok": False,
            },
        }

    rows = payload["rows"]
    # Deduplicate by period label keeping newest board row
    by_period: Dict[str, Dict[str, Any]] = {}
    for r in rows:
        by_period[r["period"]] = r
    ordered = sorted(
        by_period.values(),
        key=lambda r: (r["year"], r["quarter"]),
        reverse=True,
    )
    periods = [r["period"] for r in ordered]

    def col_vals(getter) -> List[str]:
        out = []
        for r in ordered:
            v = getter(r)
            if v is None:
                out.append("")
            elif isinstance(v, float) and abs(v - int(v)) < 1e-9:
                out.append(str(int(v)))
            else:
                out.append(str(v))
        return out

    is_rows = [
        ["Total Revenue"] + col_vals(lambda r: _k_to_ntd(r.get("revenue"))),
        ["Operating Income"] + col_vals(lambda r: _k_to_ntd(r.get("op_income"))),
        ["Net Income"] + col_vals(lambda r: _k_to_ntd(r.get("ni"))),
        ["Net Income Common Stockholders"] + col_vals(lambda r: _k_to_ntd(r.get("ni"))),
        ["Basic EPS"] + col_vals(lambda r: r.get("eps")),
        ["Diluted EPS"] + col_vals(lambda r: r.get("eps")),
    ]

    def equity(r):
        shares = _shares_from_capital_k(r.get("capital_stock_k"), par_value)
        bv = r.get("bvps")
        if shares is None or bv is None:
            return None
        return float(bv) * float(shares)

    bs_rows = [
        ["Common Stock"] + col_vals(lambda r: _k_to_ntd(r.get("capital_stock_k"))),
        ["Ordinary Shares Number"]
        + col_vals(lambda r: _shares_from_capital_k(r.get("capital_stock_k"), par_value)),
        ["Stockholders Equity"] + col_vals(equity),
        ["Common Stock Equity"] + col_vals(equity),
        ["Book Value Per Share"] + col_vals(lambda r: r.get("bvps")),
    ]

    is_payload = {"columns": ["Breakdown"] + periods, "data": is_rows}
    bs_payload = {"columns": ["Breakdown"] + periods, "data": bs_rows}
    cf_payload = empty

    return {
        "Income Statement": {"collapsed": is_payload, "expanded": is_payload},
        "Balance Sheet": {"collapsed": bs_payload, "expanded": bs_payload},
        "Cash Flow": {"collapsed": cf_payload, "expanded": cf_payload},
        "_meta": {
            "currency": "TWD",
            "financialCurrency": "TWD",
            "quote_currency": "TWD",
            "financial_currency": "TWD",
            "source": "tpex_financial_summary",
            "source_url": payload.get("source_url"),
            "board": payload.get("board"),
            "code": payload.get("code"),
            "company_name": payload.get("name"),
            "ok": True,
            "coverage": "summary_ytd_not_full_statements",
            "unmapped": payload.get("unmapped") or list(TPEX_UNMAPPED_FIELDS),
            "coverage_note_zh": payload.get("coverage_note_zh"),
            "equity_derivation_zh": (
                f"股東權益＝每股淨值×股數；股數由期末股本／面額 NT${par_value:g} 推得（台灣常見面額假設）。"
            ),
        },
    }


def scrape_tpex_financials_fallback(ticker: str, board: Optional[str] = None) -> Dict[str, Any]:
    """Entry for R／reticulate: Yahoo-empty .TWO fallback."""
    try:
        raw = fetch_tpex_ticker_rows(ticker, board=board)
        stmts = rows_to_yahoo_shaped_statements(raw)
        if not raw.get("ok"):
            stmts.setdefault("_meta", {})
            stmts["_meta"]["ok"] = False
            stmts["_meta"]["error"] = raw.get("error") or "TPEx 抓取失敗"
            stmts["_meta"]["errors"] = raw.get("errors") or []
        return stmts
    except Exception as e:
        empty = {"columns": ["Breakdown"], "data": []}
        return {
            "Income Statement": {"collapsed": empty, "expanded": empty},
            "Balance Sheet": {"collapsed": empty, "expanded": empty},
            "Cash Flow": {"collapsed": empty, "expanded": empty},
            "_meta": {
                "currency": "TWD",
                "financialCurrency": "TWD",
                "source": "tpex_financial_summary",
                "ok": False,
                "error": str(e),
            },
        }


def statements_from_fixture_rows(fixture_rows: List[Dict[str, Any]], code: str) -> Dict[str, Any]:
    """Test helper: build statements from fixture JSON rows (no network)."""
    code = normalize_tw_code(code)
    mapped = []
    for fr in fixture_rows:
        vals = fr.get("row") or []
        if not vals or str(vals[0]).strip() != code:
            continue
        year = int(fr.get("year") or 2024)
        quarter = int(fr.get("quarter") or 4)
        board = str(fr.get("board") or "")
        if not board:
            board = "ESB" if "興櫃" in str(fr.get("sheet") or "") else "OTC"
        mapped.append(
            {
                "code": code,
                "name": str(vals[1]).strip() if len(vals) > 1 else "",
                "board": board,
                "sheet": fr.get("sheet"),
                "year": year,
                "quarter": quarter,
                "period": period_end_label(year, quarter),
                "revenue": _to_float(vals[2]) if len(vals) > 2 else None,
                "op_income": _to_float(vals[5]) if len(vals) > 5 else None,
                "ni": _to_float(vals[9]) if len(vals) > 9 else None,
                "capital_stock_k": _to_float(vals[12]) if len(vals) > 12 else None,
                "eps": _to_float(vals[13]) if len(vals) > 13 else None,
                "bvps": _to_float(vals[15]) if len(vals) > 15 else None,
                "equity_to_assets_pct": _to_float(vals[16]) if len(vals) > 16 else None,
            }
        )
    payload = {
        "ok": bool(mapped),
        "code": code,
        "name": mapped[0]["name"] if mapped else "",
        "board": mapped[0]["board"] if mapped else None,
        "rows": mapped,
        "source_url": f"{TPEX_ORIGIN}/zh-tw/mainboard/listed/financial/summary.html",
        "unmapped": list(TPEX_UNMAPPED_FIELDS),
        "coverage_note_zh": "fixture",
    }
    return rows_to_yahoo_shaped_statements(payload)
