# tw_annual_report_orders.py — MOPS / doc.twse 股東會年報 → major customer / supplier tables
#
# Official source only (no marketing scrapers):
#   List:  GET  https://doc.twse.com.tw/server-java/t57sb01?…&mtype=F&
#   File:  POST step=9 → HTML with /pdf/<temp>.pdf link → download PDF
# Prefer English 年報 (FE4) when present; else Chinese (F04).
# Parse ASC/IFRS-style and Art. 18 附表十六 patterns from extracted text.
# Returns dict compatible with SEC segment_notes shape (segment_tables + excerpts).

from __future__ import annotations

import re
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from html.parser import HTMLParser
from typing import Any, Dict, List, Optional, Tuple

DOC_TWSE = "https://doc.twse.com.tw"
LIST_PATH = "/server-java/t57sb01"
USER_AGENT = "theYNowApp/tw-annual-report-orders (+https://github.com/lawrencekuo1118/theYNowApp)"
TIMEOUT_SEC = 90


class _HrefCollector(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.hrefs: List[str] = []
        self._capture = False
        self.texts: List[str] = []

    def handle_starttag(self, tag: str, attrs) -> None:
        if tag.lower() != "a":
            return
        href = dict(attrs).get("href") or ""
        if href:
            self.hrefs.append(href)

    def handle_data(self, data: str) -> None:
        t = (data or "").strip()
        if t:
            self.texts.append(t)


def _http_get(url: str, timeout: int = TIMEOUT_SEC) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read()


def _http_post(url: str, data: Dict[str, str], timeout: int = TIMEOUT_SEC) -> bytes:
    body = urllib.parse.urlencode(data).encode("utf-8")
    req = urllib.request.Request(
        url,
        data=body,
        headers={
            "User-Agent": USER_AGENT,
            "Content-Type": "application/x-www-form-urlencoded",
        },
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read()


def _decode_html(raw: bytes) -> str:
    for enc in ("big5", "cp950", "utf-8", "latin-1"):
        try:
            return raw.decode(enc)
        except Exception:
            continue
    return raw.decode("utf-8", errors="ignore")


def _issuer_code(ticker: str) -> str:
    tk = (ticker or "").strip().upper()
    tk = re.sub(r"\.(TW|TWO)$", "", tk)
    return re.sub(r"[^0-9A-Z]", "", tk)


def _roc_years(max_n: int = 3) -> List[int]:
    """Recent ROC calendar years for MOPS year= parameter (e.g. 114)."""
    import datetime as _dt

    west = _dt.date.today().year
    roc = west - 1911
    return [roc - i for i in range(max_n)]


def _list_annual_reports(co_id: str, year_roc: int) -> List[Dict[str, str]]:
    """Return [{filename, kind, label, lang}] for 股東會年報 rows."""
    url = (
        f"{DOC_TWSE}{LIST_PATH}?step=1&colorchg=1&co_id={urllib.parse.quote(co_id)}"
        f"&year={int(year_roc)}&seamon=&mtype=F&"
    )
    try:
        html = _decode_html(_http_get(url))
    except Exception:
        return []
    out: List[Dict[str, str]] = []
    # javascript:readfile2("F","2330","2024_2330_20250603F04.pdf");
    for m in re.finditer(
        r"readfile2?\(\s*[\"']([A-Za-z])[\"']\s*,\s*[\"']([^\"']+)[\"']\s*,\s*[\"']([^\"']+\.pdf)[\"']\s*\)",
        html,
        flags=re.I,
    ):
        kind, coid, filename = m.group(1), m.group(2), m.group(3)
        # Grab surrounding window for label (doc type column).
        start = max(0, m.start() - 400)
        window = html[start : m.end() + 80]
        label = ""
        # Prefer Chinese/English 年報 labels near the link.
        lm = re.search(
            r"(英文版-)?股東會年報[^<]{0,40}|Annual Report[^<]{0,40}",
            window,
            flags=re.I,
        )
        if lm:
            label = re.sub(r"\s+", " ", lm.group(0)).strip()
        is_ar = bool(
            re.search(r"股東會年報|Annual Report", window, flags=re.I)
        ) or bool(re.search(r"F0?4\.pdf|FE4\.pdf", filename, flags=re.I))
        if not is_ar:
            # Filename heuristic: *F04.pdf / *FE4.pdf are standard 年報 codes.
            if not re.search(r"F0?4\.pdf$|FE4\.pdf$", filename, flags=re.I):
                continue
        lang = "en" if (
            re.search(r"英文|English|FE4", label + filename, flags=re.I)
        ) else "zh"
        out.append(
            {
                "filename": filename,
                "kind": kind,
                "co_id": coid or co_id,
                "label": label or filename,
                "lang": lang,
                "year_roc": str(year_roc),
            }
        )
    # De-dupe by filename preserving order.
    seen = set()
    uniq = []
    for row in out:
        if row["filename"] in seen:
            continue
        seen.add(row["filename"])
        uniq.append(row)
    return uniq


def _pick_annual_report(rows: List[Dict[str, str]]) -> Optional[Dict[str, str]]:
    if not rows:
        return None
    en = [r for r in rows if r.get("lang") == "en"]
    if en:
        return en[0]
    return rows[0]


def _download_pdf(kind: str, co_id: str, filename: str) -> Optional[bytes]:
    try:
        html = _decode_html(
            _http_post(
                f"{DOC_TWSE}{LIST_PATH}",
                {
                    "step": "9",
                    "kind": kind,
                    "co_id": co_id,
                    "filename": filename,
                    "colorchg": "1",
                },
            )
        )
    except Exception:
        return None
    m = re.search(r"(/pdf/[^\"'\s>]+\.pdf)", html, flags=re.I)
    if not m:
        return None
    pdf_url = DOC_TWSE + m.group(1)
    try:
        data = _http_get(pdf_url, timeout=180)
    except Exception:
        return None
    if not data or len(data) < 1000 or not data.startswith(b"%PDF"):
        return None
    return data


def _extract_pdf_text(pdf_bytes: bytes, max_pages: int = 250) -> str:
    try:
        from pypdf import PdfReader  # type: ignore
    except Exception as e:
        raise RuntimeError(f"pypdf unavailable: {e}") from e
    with tempfile.NamedTemporaryFile(suffix=".pdf") as tmp:
        tmp.write(pdf_bytes)
        tmp.flush()
        reader = PdfReader(tmp.name)
        parts: List[str] = []
        for i, page in enumerate(reader.pages):
            if i >= max_pages:
                break
            try:
                t = page.extract_text() or ""
            except Exception:
                t = ""
            if t:
                parts.append(t)
        return "\n".join(parts)


_PCT_RE = re.compile(
    r"(?P<pct>\d{1,3}(?:\.\d+)?)\s*%"
)
_AMT_RE = re.compile(
    r"(?P<amt>\d{1,3}(?:,\d{3})+(?:\.\d+)?|\d+(?:\.\d+)?)"
)


def _parse_pct_token(tok: str) -> Optional[float]:
    m = _PCT_RE.search(tok or "")
    if not m:
        return None
    v = float(m.group("pct"))
    if v > 100.0:
        return None
    return v / 100.0


def _parse_amount_token(tok: str) -> Optional[float]:
    s = (tok or "").strip().replace(",", "")
    if not s or s.upper() in {"N/A", "NA", "-", "—", "－"}:
        return None
    try:
        return float(s)
    except Exception:
        return None


def _is_skip_counterparty(name: str) -> bool:
    n = re.sub(r"\s+", " ", (name or "").strip())
    if not n:
        return True
    low = n.lower()
    if re.match(r"^(others?|其他|合計|總計|小計|total|net (sales|revenue)|銷貨淨額|進貨淨額)\b", low):
        return True
    if re.match(r"^(item|項目|名稱|name|customer|supplier|客戶|供應商)$", low):
        return True
    # Section titles / headers mistaken for counterparties.
    if re.search(
        r"accounting for|at least|most recent|information on|percentage of|"
        r"占全年度|最近二年度|主要銷貨|主要供應|relation to|"
        r"no single|more than|did not have|並無|無單一",
        low,
    ):
        return True
    return False


def _row_from_customer_line(line: str) -> Optional[Dict[str, Any]]:
    """Parse lines like: Customer A  624,345,477  22%  None  …"""
    s = re.sub(r"[ \t]+", " ", (line or "").strip())
    if not s:
        return None
    # English anonymized / named
    m = re.match(
        r"^(Customer\s+[A-Z0-9]+|Client\s+[A-Z0-9]+|"
        r"客戶\s*[A-Za-z0-9甲乙丙丁戊己庚辛壬癸一二三四五六七八九十]+|"
        r"[A-Za-z][A-Za-z0-9 .&\-]{1,40})\s+(.+)$",
        s,
        flags=re.I,
    )
    if not m:
        return None
    name, rest = m.group(1).strip(), m.group(2).strip()
    if _is_skip_counterparty(name):
        return None
    if re.match(r"^(19|20)\d{2}$", name):
        return None
    # Prefer the first percentage on the line (current / most recent year column).
    pcts: List[float] = []
    for pm in _PCT_RE.finditer(rest):
        v = float(pm.group("pct"))
        if v <= 100.0:
            pcts.append(v / 100.0)
    if not pcts:
        return None
    amt = None
    for am in _AMT_RE.finditer(rest):
        token = am.group("amt")
        # Amounts in these tables are large; skip bare 1–2 digit ints that are ranks.
        if "," in token or (len(token.split(".")[0]) >= 4):
            amt = _parse_amount_token(token)
            if amt is not None:
                break
    return {
        "name": name,
        "revenue": amt,
        "revenue_pct": pcts[0],
    }


def _extract_block(text: str, start_pat: str, end_pats: List[str], window: int = 3500) -> str:
    m = re.search(start_pat, text, flags=re.I | re.M)
    if not m:
        return ""
    start = m.start()
    chunk = text[start : start + window]
    end_at = len(chunk)
    for ep in end_pats:
        em = re.search(ep, chunk[80:], flags=re.I | re.M)
        if em:
            end_at = min(end_at, 80 + em.start())
    return chunk[:end_at]


def _parse_customer_table(text: str) -> List[Dict[str, Any]]:
    block = _extract_block(
        text,
        start_pat=(
            r"Customers?\s+Accounting\s+for\s+at\s+Least\s+10%\s+of|"
            r"Information\s+on\s+Major\s+Customers|"
            r"Major\s+Customers\s+for\s+the\s+Most\s+Recent|"
            r"最近二年度主要銷貨客戶|"
            r"主要銷貨客戶資料|"
            r"占[全當]?年度?銷貨淨額"
        ),
        end_pats=[
            r"\n\s*5\.\d+",
            r"Reason for Increase or Decrease",
            r"增減變動原因",
            r"主要供應商",
            r"Major\s+Suppliers?\s+for\s+the\s+Most\s+Recent",
            r"Open Innovation",
            r"Information Security",
        ],
        window=4000,
    )
    if not block:
        # Fallback: scan whole text for Customer A/B lines with %.
        block = text
    rows: List[Dict[str, Any]] = []
    seen = set()
    for line in block.splitlines():
        if not re.search(r"%", line):
            continue
        if re.search(r"no single|more than \d|did not have|並無|無單一|accounted for more", line, flags=re.I):
            continue
        if not re.search(r"Customer|Client|客戶", line, flags=re.I):
            # Allow named counterparties only inside a detected customer block.
            if block is text:
                continue
            if not re.search(r"[A-Za-z\u4e00-\u9fff]{2,}", line):
                continue
        parsed = _row_from_customer_line(line)
        if not parsed:
            continue
        key = parsed["name"].lower()
        if key in seen:
            continue
        if _is_skip_counterparty(parsed["name"]):
            continue
        # Require customer-like name OR be inside dedicated block with pct >= 0.05
        if not re.search(r"customer|client|客戶", parsed["name"], flags=re.I):
            if parsed.get("revenue_pct") is None or parsed["revenue_pct"] < 0.05:
                continue
        seen.add(key)
        rows.append(parsed)
        if len(rows) >= 12:
            break
    return rows


def _parse_supplier_table(text: str) -> List[Dict[str, Any]]:
    block = _extract_block(
        text,
        start_pat=(
            r"Information\s+on\s+Major\s+Suppliers|"
            r"Major\s+Suppliers?\s+for\s+the\s+Most\s+Recent|"
            r"Suppliers?\s+Accounting\s+for\s+at\s+Least\s+10%|"
            r"最近二年度主要供應商|"
            r"主要供應商資料|"
            r"占[全當]?年度?進貨淨額"
        ),
        end_pats=[
            r"\n\s*5\.\d+",
            r"Reason for Increase or Decrease",
            r"增減變動原因",
            r"主要銷貨客戶",
            r"Major\s+Customers",
            r"Customers?\s+Accounting",
        ],
        window=4000,
    )
    if not block:
        return []
    rows: List[Dict[str, Any]] = []
    seen = set()
    for line in block.splitlines():
        if not re.search(r"%", line):
            continue
        # Prefer Supplier / 供應商 labels; also accept coded A Company style inside block.
        cleaned = re.sub(r"[ \t]+", " ", line.strip())
        # Drop leading rank index ("1 ", "10.") from Art. 18 tables.
        cleaned = re.sub(r"^\d{1,2}[\.、\)]?\s+", "", cleaned)
        m = re.match(
            r"^(Supplier\s+[A-Z0-9]+|Vendor\s+[A-Z0-9]+|"
            r"供應商\s*[A-Za-z0-9甲乙丙丁戊己庚辛壬癸一二三四五六七八九十]+|"
            r"[A-Z]\s*Company|"
            r"[A-Za-z\u4e00-\u9fff][A-Za-z0-9\u4e00-\u9fff .&\-]{1,40})\s+(.+)$",
            cleaned,
            flags=re.I,
        )
        if not m:
            continue
        name, rest = m.group(1).strip(), m.group(2).strip()
        if _is_skip_counterparty(name):
            continue
        if re.search(r"Major Materials|Market Status|Procurement", name, flags=re.I):
            continue
        pcts = []
        for pm in _PCT_RE.finditer(rest):
            v = float(pm.group("pct"))
            if v <= 100.0:
                pcts.append(v / 100.0)
        if not pcts:
            continue
        # Art. 18 threshold is 10%; keep ≥5% to tolerate formatting noise.
        if pcts[0] < 0.05:
            continue
        key = name.lower()
        if key in seen:
            continue
        seen.add(key)
        amt = None
        for am in _AMT_RE.finditer(rest):
            token = am.group("amt")
            if "," in token or (len(token.split(".")[0]) >= 4):
                amt = _parse_amount_token(token)
                if amt is not None:
                    break
        rows.append({"name": name, "revenue": amt, "revenue_pct": pcts[0]})
        if len(rows) >= 12:
            break
    return rows


def _parse_geography_shares(text: str) -> List[Dict[str, Any]]:
    """Parse narrative 'Net revenue by geography … was: 70% from North America; …'."""
    flat = re.sub(r"\s+", " ", text or "")
    blob = ""
    m = re.search(
        r"Net revenue by geography.{0,220}?was:\s*(.{20,700}?)(?:\. Differentiation|\. Strategy|Net revenue by platform|$)",
        flat,
        flags=re.I,
    )
    if m:
        blob = m.group(1)
    else:
        m2 = re.search(
            r"(地區別營收|營收地區|依地區別).{0,80}[：:]\s*(.{20,500})",
            flat,
        )
        if m2:
            blob = m2.group(2)
    if not blob:
        return []
    rows: List[Dict[str, Any]] = []
    for pm in re.finditer(
        r"(?P<pct>\d{1,3}(?:\.\d+)?)\s*%\s*(?:from|來自)\s*(?P<name>.+?)(?=(?:;|；|,\s*\d|\.\s*[A-Z]|$))",
        blob,
        flags=re.I,
    ):
        name = re.sub(r"\s+", " ", pm.group("name")).strip(" .,;:，。；")
        name = re.sub(r"^(and|及)\s+", "", name, flags=re.I).strip()
        name = re.sub(r"\s+(and|及)\s*$", "", name, flags=re.I).strip()
        # Trim trailing "and 4%" fragments already split out.
        name = re.split(r"\s+and\s+\d", name, maxsplit=1, flags=re.I)[0].strip()
        if len(name) < 2 or len(name) > 80:
            continue
        pct = float(pm.group("pct")) / 100.0
        if pct <= 0 or pct > 1:
            continue
        rows.append({"name": name, "revenue": None, "revenue_pct": pct})
    return rows[:12]


def _rows_to_table(kind: str, short_name: str, rows: List[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    if not rows:
        return None
    headers = ["Name", "Amount", "% of Total"]
    table_rows = []
    for r in rows:
        pct = r.get("revenue_pct")
        pct_s = f"{pct * 100:.4g}%" if isinstance(pct, float) else ""
        amt = r.get("revenue")
        amt_s = f"{amt:,.0f}" if isinstance(amt, (int, float)) and amt == amt else ""
        table_rows.append([r["name"], amt_s, pct_s])
    return {
        "short_name": short_name,
        "label": short_name,
        "kind": kind,
        "layout": "stub",
        "scale": 1,
        "headers": headers,
        "rows": table_rows,
        "source": "mops_annual_report",
    }


def _scan_none_above(text: str, kind: str) -> Optional[str]:
    low = (text or "").lower()
    if kind == "customer":
        if re.search(
            r"no (single )?(customer|client) accounted for more than|"
            r"did not have any customer.{0,40}10\s*%|"
            r"無.{0,6}(單一)?客戶.{0,20}10\s*%|"
            r"並無.{0,10}客戶.{0,20}百分之十",
            low,
        ):
            return "ORDERS_NO_MAJOR_CUSTOMER_ABOVE_THRESHOLD"
    else:
        if re.search(
            r"no (single )?(supplier|vendor) accounted for more than|"
            r"無.{0,6}(單一)?供應商.{0,20}10\s*%|"
            r"並無.{0,10}供應商.{0,20}百分之十",
            low,
        ):
            return "ORDERS_NO_MAJOR_SUPPLIER_ABOVE_THRESHOLD"
    return None


def parse_annual_report_text(text: str) -> Dict[str, Any]:
    """Pure parser (offline-testable) → segment_tables + excerpt signals."""
    text = text or ""
    tables: List[Dict[str, Any]] = []
    excerpts: List[str] = []
    cust_rows = _parse_customer_table(text)
    cust_tbl = _rows_to_table(
        "customer_concentration",
        "Major Customers (MOPS Annual Report)",
        cust_rows,
    )
    if cust_tbl:
        tables.append(cust_tbl)
        excerpts.append("MOPS annual report: major-customer concentration table parsed.")
    else:
        code = _scan_none_above(text, "customer")
        if code:
            excerpts.append("No single customer accounted for more than 10% of net sales.")

    supp_rows = _parse_supplier_table(text)
    supp_tbl = _rows_to_table(
        "supplier_concentration",
        "Major Suppliers (MOPS Annual Report)",
        supp_rows,
    )
    if supp_tbl:
        tables.append(supp_tbl)
        excerpts.append("MOPS annual report: major-supplier concentration table parsed.")
    else:
        code = _scan_none_above(text, "supplier")
        if code:
            excerpts.append("No single supplier accounted for more than 10% of purchases.")

    geo_rows = _parse_geography_shares(text)
    geo_tbl = _rows_to_table(
        "geography",
        "Net Revenue by Geography (MOPS Annual Report)",
        geo_rows,
    )
    if geo_tbl:
        geo_tbl["is_customer_location_only"] = True
        tables.append(geo_tbl)
        excerpts.append("MOPS annual report: geography revenue shares parsed.")

    return {
        "segment_tables": tables,
        "excerpts": excerpts,
        "customer_rows": cust_rows,
        "supplier_rows": supp_rows,
        "geography_rows": geo_rows,
    }


def fetch_tw_annual_order_notes(ticker: str, max_years: int = 3) -> Dict[str, Any]:
    """
    Fetch TW major customer/supplier/geography concentration from official 年報.

    Returns a dict shaped like sec_report_notes for R glue:
      ok, ticker, source, form, excerpts, segment_tables, error, meta
    """
    empty: Dict[str, Any] = {
        "ok": False,
        "ticker": ticker,
        "source": "mops_annual_report",
        "form": "TW-AR",
        "excerpts": [],
        "full_texts": [],
        "short_names": [],
        "segment_tables": [],
        "error": None,
        "meta": {},
    }
    co_id = _issuer_code(ticker)
    if not co_id:
        empty["error"] = "invalid_tw_ticker"
        return empty

    picked = None
    list_errs: List[str] = []
    for y in _roc_years(max_years):
        rows = _list_annual_reports(co_id, y)
        if not rows:
            list_errs.append(f"no_ar_year_{y}")
            continue
        picked = _pick_annual_report(rows)
        if picked:
            picked = dict(picked)
            picked["list_year_roc"] = str(y)
            break
    if not picked:
        empty["error"] = "annual_report_not_found"
        empty["meta"] = {"list_errors": list_errs, "co_id": co_id}
        return empty

    try:
        pdf = _download_pdf(picked["kind"], picked.get("co_id") or co_id, picked["filename"])
    except Exception as e:
        empty["error"] = f"download_failed: {e}"
        empty["meta"] = {"picked": picked}
        return empty
    if not pdf:
        empty["error"] = "download_failed"
        empty["meta"] = {"picked": picked}
        return empty

    try:
        text = _extract_pdf_text(pdf)
    except Exception as e:
        empty["error"] = f"pdf_extract_failed: {e}"
        empty["meta"] = {"picked": picked, "pdf_bytes": len(pdf)}
        return empty
    if not text or len(text) < 200:
        empty["error"] = "pdf_text_empty"
        empty["meta"] = {"picked": picked, "pdf_bytes": len(pdf)}
        return empty

    parsed = parse_annual_report_text(text)
    tables = parsed.get("segment_tables") or []
    excerpts = list(parsed.get("excerpts") or [])
    ok = bool(tables) or bool(excerpts)
    return {
        "ok": ok,
        "ticker": ticker,
        "source": "mops_annual_report",
        "form": "TW-AR",
        "excerpts": excerpts,
        "full_texts": [text[:12000]] if text else [],
        "short_names": [t.get("short_name") for t in tables if isinstance(t, dict)],
        "segment_tables": tables,
        "error": None if ok else "concentration_not_found_in_annual_report",
        "meta": {
            "co_id": co_id,
            "filename": picked.get("filename"),
            "label": picked.get("label"),
            "lang": picked.get("lang"),
            "year_roc": picked.get("list_year_roc") or picked.get("year_roc"),
            "pdf_bytes": len(pdf),
            "text_chars": len(text),
            "n_customer": len(parsed.get("customer_rows") or []),
            "n_supplier": len(parsed.get("supplier_rows") or []),
            "n_geography": len(parsed.get("geography_rows") or []),
        },
    }


# Reticulate-friendly alias
scrape_tw_annual_order_notes = fetch_tw_annual_order_notes
