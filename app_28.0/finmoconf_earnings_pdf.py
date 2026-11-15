# finmoconf_earnings_pdf.py — FinmoConf company-page discovery → MOPS STR PDF text
#
# FinmoConf (finmoconf.diveinvest.net) robots.txt: Allow / ; Disallow /api/ /admin/.
# This module NEVER calls /api/. Discovery uses public /company/{ticker} HTML only.
# PDF bytes are official TWSE STR files (same family as tw_annual_report_orders.py),
# fetched from doc.twse.com.tw when possible (mopsov host rewritten).
# Credibility for parsed facts is Estimate (PDF layout / extract limits).
# Never invent amounts. Fail closed on empty / blocked / parse failure.

from __future__ import annotations

import io
import re
import urllib.parse
import urllib.request
from typing import Any, Dict, List, Optional, Tuple

FINMOCONF_ORIGIN = "https://finmoconf.diveinvest.net"
DOC_TWSE_ORIGIN = "https://doc.twse.com.tw"
MOPSOV_ORIGIN = "https://mopsov.twse.com.tw"
USER_AGENT = "TheYNowApp/finmoconf-earnings (+https://github.com/lawrencekuo1118/theYNowApp)"
TIMEOUT_SEC = 60
PDF_TIMEOUT_SEC = 120
MAX_PDF_PAGES = 80

_STR_PDF_RE = re.compile(
    r"https?://(?:mopsov\.twse\.com\.tw|doc\.twse\.com\.tw)/nas/STR/"
    r"(?P<co>[0-9]{4,6}[A-Z]?)"
    r"(?P<ymd>20[0-9]{6})"
    r"(?P<lang>[ME])"
    r"(?P<seq>[0-9]{3})\.pdf",
    flags=re.I,
)


def _issuer_code(ticker: str) -> str:
    tk = (ticker or "").strip().upper()
    tk = re.sub(r"\.(TW|TWO)$", "", tk)
    return re.sub(r"[^0-9A-Z]", "", tk)


def _http_get(url: str, timeout: int = TIMEOUT_SEC) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read()


def _company_url(co_id: str) -> str:
    return f"{FINMOCONF_ORIGIN}/company/{urllib.parse.quote(co_id)}"


def _rewrite_pdf_url(url: str) -> str:
    """Prefer doc.twse.com.tw (annual-report host already used in-app)."""
    u = (url or "").strip()
    if not u:
        return ""
    u = u.replace("\\/", "/")
    if "mopsov.twse.com.tw/nas/STR/" in u:
        return u.replace("https://mopsov.twse.com.tw", DOC_TWSE_ORIGIN).replace(
            "http://mopsov.twse.com.tw", DOC_TWSE_ORIGIN
        )
    return u


def parse_finmoconf_company_pdfs(html: str, co_id: str) -> List[Dict[str, str]]:
    """Return presentation PDF rows sorted newest-first: date, lang (zh|en), url, seq."""
    html = (html or "").replace("\\/", "/")
    co = _issuer_code(co_id)
    if not html or not co:
        return []
    rows: List[Dict[str, str]] = []
    seen = set()
    for m in _STR_PDF_RE.finditer(html):
        if m.group("co").upper() != co.upper():
            continue
        lang_raw = m.group("lang").upper()
        lang = "zh" if lang_raw == "M" else "en"
        ymd = m.group("ymd")
        seq = m.group("seq")
        url = _rewrite_pdf_url(m.group(0))
        key = (ymd, lang, seq, url)
        if key in seen:
            continue
        seen.add(key)
        rows.append(
            {
                "date": f"{ymd[0:4]}-{ymd[4:6]}-{ymd[6:8]}",
                "ymd": ymd,
                "lang": lang,
                "lang_code": lang_raw,
                "seq": seq,
                "url": url,
                "mopsov_url": (
                    f"{MOPSOV_ORIGIN}/nas/STR/{co}{ymd}{lang_raw}{seq}.pdf"
                ),
            }
        )
    rows.sort(key=lambda r: (r["ymd"], r["lang"] == "zh", r["seq"]), reverse=True)
    return rows


def pick_latest_presentation(
    rows: List[Dict[str, str]], prefer_lang: str = "zh"
) -> Optional[Dict[str, Any]]:
    """Pick newest card; zh preferred, else en for that date."""
    if not rows:
        return None
    prefer = (prefer_lang or "zh").lower()
    by_date: Dict[str, Dict[str, Dict[str, str]]] = {}
    for r in rows:
        by_date.setdefault(r["ymd"], {})[r["lang"]] = r
    for ymd in sorted(by_date.keys(), reverse=True):
        bucket = by_date[ymd]
        chosen = bucket.get(prefer) or bucket.get("zh") or bucket.get("en")
        if not chosen:
            continue
        return {
            "date": chosen["date"],
            "ymd": ymd,
            "lang": chosen["lang"],
            "url": chosen["url"],
            "mopsov_url": chosen.get("mopsov_url", ""),
            "zh_url": (bucket.get("zh") or {}).get("url", ""),
            "en_url": (bucket.get("en") or {}).get("url", ""),
            "all_langs": sorted(bucket.keys()),
        }
    return None


def _extract_pdf_text(pdf_bytes: bytes, max_pages: int = MAX_PDF_PAGES) -> str:
    try:
        from pypdf import PdfReader  # type: ignore
    except Exception as e:
        raise RuntimeError(f"pypdf unavailable: {e}") from e
    reader = PdfReader(io.BytesIO(pdf_bytes))
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


def _download_pdf(url: str) -> Tuple[Optional[bytes], str]:
    url = _rewrite_pdf_url(url)
    if not url:
        return None, "blank_url"
    try:
        data = _http_get(url, timeout=PDF_TIMEOUT_SEC)
    except Exception as e:
        # One fallback to mopsov if doc.twse failed.
        if "doc.twse.com.tw" in url:
            alt = url.replace(DOC_TWSE_ORIGIN, MOPSOV_ORIGIN)
            try:
                data = _http_get(alt, timeout=PDF_TIMEOUT_SEC)
                url = alt
            except Exception as e2:
                return None, f"pdf_download_failed:{e2}"
        else:
            return None, f"pdf_download_failed:{e}"
    if not data or len(data) < 64 or not data[:5].startswith(b"%PDF"):
        return None, "pdf_not_pdf"
    return data, url


def fetch_finmoconf_latest_presentation(
    ticker: str,
    prefer_lang: str = "zh",
    html_fixture: str = "",
    text_fixture: str = "",
    skip_pdf: bool = False,
) -> Dict[str, Any]:
    """Discover latest FinmoConf card + optional PDF text for a TW ticker."""
    co = _issuer_code(ticker)
    empty: Dict[str, Any] = {
        "ok": False,
        "ticker": co,
        "status": "empty",
        "source_id": "finmoconf_presentation_pdf",
        "credibility": "estimate",
        "company_url": f"{FINMOCONF_ORIGIN}/company/{co}" if co else FINMOCONF_ORIGIN,
        "title": "",
        "url": "",
        "published": "",
        "lang": "",
        "zh_url": "",
        "en_url": "",
        "text": "",
        "pdf_bytes": 0,
        "error": "empty",
        "meta": {},
    }
    if not co or not re.match(r"^[0-9]{4,6}[A-Z]?$", co):
        empty["error"] = "bad_ticker"
        return empty

    html = html_fixture or ""
    if not html:
        try:
            raw = _http_get(_company_url(co), timeout=TIMEOUT_SEC)
            html = raw.decode("utf-8", errors="ignore")
        except Exception as e:
            empty["error"] = f"company_page_failed:{e}"
            return empty

    rows = parse_finmoconf_company_pdfs(html, co)
    picked = pick_latest_presentation(rows, prefer_lang=prefer_lang)
    if not picked:
        empty["error"] = "no_presentation_pdf"
        empty["meta"] = {"row_count": len(rows)}
        return empty

    empty.update(
        {
            "published": picked["date"],
            "lang": picked["lang"],
            "url": picked["url"],
            "zh_url": picked.get("zh_url", ""),
            "en_url": picked.get("en_url", ""),
            "title": f"{co} 法說會簡報 ({picked['date']}, {picked['lang']})",
            "meta": {
                "ymd": picked["ymd"],
                "all_langs": picked.get("all_langs", []),
                "row_count": len(rows),
                "mopsov_url": picked.get("mopsov_url", ""),
            },
        }
    )

    if text_fixture:
        text = text_fixture
        empty["ok"] = bool(text.strip())
        empty["status"] = "ok" if empty["ok"] else "empty"
        empty["text"] = text
        empty["error"] = "" if empty["ok"] else "text_fixture_empty"
        empty["pdf_bytes"] = 0
        return empty

    if skip_pdf:
        empty["error"] = "pdf_download_disabled"
        return empty

    pdf, final_url = _download_pdf(picked["url"])
    if not pdf:
        empty["error"] = final_url if final_url.startswith("pdf_") else "pdf_download_failed"
        return empty
    empty["url"] = final_url
    empty["pdf_bytes"] = len(pdf)
    try:
        text = _extract_pdf_text(pdf)
    except Exception as e:
        empty["error"] = f"pdf_extract_failed:{e}"
        return empty
    if not (text or "").strip():
        empty["error"] = "pdf_text_empty"
        return empty
    empty["ok"] = True
    empty["status"] = "ok"
    empty["text"] = text
    empty["error"] = ""
    return empty
