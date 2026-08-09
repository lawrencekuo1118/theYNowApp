#!/usr/bin/env python3
"""
Standalone exploration (PoC): fetch a US stock's LATEST financial report from
SEC EDGAR and extract the important "Notes to the Financial Statements".

This is a research spike, independent of the Shiny app. It is NOT wired into
app_13.0. yfinance (the app's data layer) does not expose financial-statement
footnotes, so this pulls them straight from EDGAR.

How it works
------------
1. ticker  -> CIK            via https://www.sec.gov/files/company_tickers.json
2. CIK     -> latest 10-K    via https://data.sec.gov/submissions/CIK##########.json
   (10-Q available with --form 10-Q)
3. filing  -> list of notes  via the filing's FilingSummary.xml
   (each <Report> whose <MenuCategory> == "Notes" is one footnote/disclosure)
4. note    -> readable text  by fetching each note's R#.htm and stripping markup

The notes are ranked: financial-statement footnotes that are usually the most
decision-relevant (accounting policies, revenue, segments, income taxes, debt,
leases, commitments & contingencies, ...) are flagged IMPORTANT.

SEC requires a descriptive User-Agent with contact info and rate-limits to
~10 requests/second. Override the User-Agent with the SEC_EDGAR_UA env var.

Usage
-----
    python scripts/explore_sec_notes.py AAPL
    python scripts/explore_sec_notes.py MSFT --form 10-K --max-chars 1500
    python scripts/explore_sec_notes.py NVDA --json out.json --markdown out.md

Run it with the app's venv python so curl_cffi/bs4 are available:
    app_13.0/.ynow_venv/bin/python scripts/explore_sec_notes.py AAPL
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time

TICKERS_URL = "https://www.sec.gov/files/company_tickers.json"
SUBMISSIONS_URL = "https://data.sec.gov/submissions/CIK{cik}.json"
ARCHIVES = "https://www.sec.gov/Archives/edgar/data/{cik}/{accn}"
DEFAULT_UA = os.environ.get(
    "SEC_EDGAR_UA", "theYNowApp research (set SEC_EDGAR_UA=you@example.com)"
)

# Footnotes that are usually the most decision-relevant. Matching is done on a
# lower-cased, keyword basis against the note's ShortName.
IMPORTANT_KEYWORDS = (
    "accounting policies",
    "basis of presentation",
    "revenue",
    "segment",
    "geographic",
    "income tax",
    "debt",
    "borrow",
    "lease",
    "commitment",
    "contingenc",
    "financial instrument",
    "fair value",
    "derivative",
    "goodwill",
    "intangible",
    "share-based",
    "stock-based",
    "pension",
    "retirement",
    "business combination",
    "acquisition",
    "per share",
    "related party",
    "restructuring",
    "property, plant",
)

# Notes-category reports that are governance/other disclosures rather than
# financial-statement footnotes; kept in the listing but not flagged important.
NON_FINANCIAL_HINTS = (
    "insider trading",
    "cybersecurity",
)


def make_session():
    """curl_cffi session with a SEC-compliant User-Agent (browser impersonation
    also dodges datacenter-IP blocks). Falls back to plain requests."""
    try:
        from curl_cffi import requests as creq

        return creq.Session(headers={"User-Agent": DEFAULT_UA}, impersonate="chrome")
    except Exception:
        import requests

        s = requests.Session()
        s.headers.update({"User-Agent": DEFAULT_UA})
        return s


def _get(session, url, *, is_json=False, tries=3):
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


def ticker_to_cik(session, ticker):
    data = _get(session, TICKERS_URL, is_json=True)
    tk = ticker.strip().upper()
    for row in data.values():
        if str(row.get("ticker", "")).upper() == tk:
            return str(row["cik_str"]).zfill(10), row.get("title", tk)
    raise SystemExit(f"Ticker '{ticker}' not found in SEC company_tickers.json")


def latest_filing(session, cik, form):
    sub = _get(session, SUBMISSIONS_URL.format(cik=cik), is_json=True)
    recent = sub["filings"]["recent"]
    forms = recent["form"]
    for i, f in enumerate(forms):
        if f == form:
            return {
                "form": form,
                "accession": recent["accessionNumber"][i],
                "primary_doc": recent["primaryDocument"][i],
                "filing_date": recent["filingDate"][i],
                "report_date": recent.get("reportDate", [""] * len(forms))[i],
                "company": sub.get("name", ""),
                "fiscal_year_end": sub.get("fiscalYearEnd", ""),
            }
    raise SystemExit(f"No {form} filing found for CIK {cik}")


def filing_folder(cik, accession):
    return ARCHIVES.format(cik=int(cik), accn=accession.replace("-", ""))


def list_notes(session, folder):
    """Return note reports from FilingSummary.xml (MenuCategory == 'Notes')."""
    from bs4 import BeautifulSoup

    xml = _get(session, f"{folder}/FilingSummary.xml")
    soup = BeautifulSoup(xml, "lxml-xml")
    notes = []
    for rep in soup.find_all("Report"):
        cat = rep.find("MenuCategory")
        if not cat or cat.text != "Notes":
            continue
        short = rep.find("ShortName")
        htmf = rep.find("HtmlFileName")
        if not htmf or not htmf.text:
            continue
        notes.append(
            {
                "short_name": short.text if short else "",
                "html_file": htmf.text,
            }
        )
    return notes


# Strips only the leading "XML NN R9.htm IDEA: XBRL DOCUMENT vX.Y.Z " banner.
# The note text is collapsed to a single line, so the pattern must stop at the
# version token (not consume the rest of the line).
_BOILERPLATE = re.compile(
    r"^\s*(?:XML\s+\d+\s+)?[Rr]\d+\.htm\s+IDEA:\s*XBRL DOCUMENT\s+v?[\d.]+\s*",
    re.IGNORECASE,
)


def clean_note_text(raw):
    from bs4 import BeautifulSoup

    text = BeautifulSoup(raw, "lxml").get_text(" ", strip=True)
    text = re.sub(r"\s+", " ", text)
    text = _BOILERPLATE.sub("", text).strip()
    return text


def is_important(short_name):
    s = (short_name or "").lower()
    if any(h in s for h in NON_FINANCIAL_HINTS):
        return False
    return any(k in s for k in IMPORTANT_KEYWORDS)


def fetch_notes(session, ticker, form, max_chars):
    cik, title = ticker_to_cik(session, ticker)
    filing = latest_filing(session, cik, form)
    folder = filing_folder(cik, filing["accession"])
    filing["cik"] = cik
    filing["title"] = title
    filing["edgar_index"] = f"{folder}/{filing['primary_doc']}"
    filing["folder"] = folder

    notes = list_notes(session, folder)
    results = []
    for n in notes:
        raw = _get(session, f"{folder}/{n['html_file']}")
        text = clean_note_text(raw)
        results.append(
            {
                "short_name": n["short_name"],
                "html_file": n["html_file"],
                "url": f"{folder}/{n['html_file']}",
                "important": is_important(n["short_name"]),
                "char_count": len(text),
                "excerpt": text[:max_chars],
                "full_text": text,
            }
        )
        time.sleep(0.12)  # be polite to SEC (well under 10 req/s)
    return filing, results


def print_report(filing, notes, max_chars):
    line = "=" * 78
    print(line)
    print(f"{filing['title']}  ({filing['form']})")
    print(line)
    print(f"CIK           : {filing['cik']}")
    print(f"Filing date   : {filing['filing_date']}   (report period: {filing['report_date']})")
    print(f"Accession     : {filing['accession']}")
    print(f"Primary doc   : {filing['edgar_index']}")
    print(f"Notes found   : {len(notes)}  "
          f"(important: {sum(1 for n in notes if n['important'])})")
    print()

    print("Notes index")
    print("-" * 78)
    for i, n in enumerate(notes, 1):
        flag = "IMPORTANT" if n["important"] else "         "
        print(f"{i:2}. [{flag}] {n['short_name']}  ({n['char_count']} chars)")
    print()

    print("Important notes (excerpts)")
    print("=" * 78)
    for n in notes:
        if not n["important"]:
            continue
        print(f"\n### {n['short_name']}")
        print(f"({n['url']})")
        print("-" * 78)
        excerpt = n["excerpt"]
        if n["char_count"] > max_chars:
            excerpt += f" ...[truncated, {n['char_count']} chars total]"
        print(excerpt)


def to_markdown(filing, notes, max_chars):
    out = []
    out.append(f"# {filing['title']} — {filing['form']} notes\n")
    out.append(f"- CIK: `{filing['cik']}`")
    out.append(f"- Filing date: {filing['filing_date']} (report period: {filing['report_date']})")
    out.append(f"- Accession: `{filing['accession']}`")
    out.append(f"- Primary document: {filing['edgar_index']}")
    out.append(f"- Notes found: {len(notes)} (important: {sum(1 for n in notes if n['important'])})\n")
    out.append("## Notes index\n")
    for i, n in enumerate(notes, 1):
        flag = " **(IMPORTANT)**" if n["important"] else ""
        out.append(f"{i}. [{n['short_name']}]({n['url']}){flag} — {n['char_count']} chars")
    out.append("\n## Important notes (excerpts)\n")
    for n in notes:
        if not n["important"]:
            continue
        out.append(f"### {n['short_name']}\n")
        out.append(f"[{n['url']}]({n['url']})\n")
        excerpt = n["excerpt"]
        if n["char_count"] > max_chars:
            excerpt += f" ...[truncated, {n['char_count']} chars total]"
        out.append(excerpt + "\n")
    return "\n".join(out)


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("ticker", help="US stock ticker, e.g. AAPL")
    p.add_argument("--form", default="10-K", choices=["10-K", "10-Q"], help="Filing type (default 10-K)")
    p.add_argument("--max-chars", type=int, default=1200, help="Excerpt length per important note")
    p.add_argument("--json", metavar="FILE", help="Write full results (incl. full note text) as JSON")
    p.add_argument("--markdown", metavar="FILE", help="Write a markdown summary")
    args = p.parse_args(argv)

    session = make_session()
    filing, notes = fetch_notes(session, args.ticker, args.form, args.max_chars)

    print_report(filing, notes, args.max_chars)

    if args.json:
        with open(args.json, "w", encoding="utf-8") as fh:
            json.dump({"filing": filing, "notes": notes}, fh, ensure_ascii=False, indent=2)
        print(f"\n[wrote JSON: {args.json}]")
    if args.markdown:
        with open(args.markdown, "w", encoding="utf-8") as fh:
            fh.write(to_markdown(filing, notes, args.max_chars))
        print(f"[wrote markdown: {args.markdown}]")

    return 0


if __name__ == "__main__":
    sys.exit(main())
