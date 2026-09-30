#!/usr/bin/env python3
"""Offline tests for 10-K segment-table extraction (colspan / $ cells / kinds)."""
from __future__ import annotations

import os
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from deep_scraper import (  # noqa: E402
    _sec_append_segment_tables,
    _sec_classify_extracted_table,
    _sec_html_tables,
    _sec_segment_kind,
)


def check(label, cond):
    if not cond:
        raise AssertionError(f"FAIL: {label}")
    print(f"OK: {label}")


# Recorded ASC 280 layout: segment names as row groups, years as columns,
# '$' in its own cell, amounts with colspan. Company-agnostic HTML.
GROUPED_HTML = """
<html><body>
<table>
  <tr><td colspan="3"></td><td colspan="15">Year Ended December 31,</td></tr>
  <tr><td colspan="3"></td><td colspan="3">2023</td><td colspan="3"></td>
      <td colspan="3">2024</td><td colspan="3"></td><td colspan="3">2025</td></tr>
  <tr><td colspan="3">North America</td><td colspan="3"></td><td colspan="3"></td>
      <td colspan="3"></td><td colspan="3"></td><td colspan="3"></td></tr>
  <tr><td colspan="3">Net sales</td><td>$</td><td>352,828</td><td></td><td colspan="3"></td>
      <td>$</td><td>387,497</td><td></td><td colspan="3"></td>
      <td>$</td><td>426,305</td><td></td></tr>
  <tr><td colspan="3">Operating expenses</td><td colspan="2">337,951</td><td></td>
      <td colspan="3"></td><td colspan="2">362,530</td><td></td>
      <td colspan="3"></td><td colspan="2">396,686</td><td></td></tr>
  <tr><td colspan="3">Operating income</td><td>$</td><td>14,877</td><td></td>
      <td colspan="3"></td><td>$</td><td>24,967</td><td></td>
      <td colspan="3"></td><td>$</td><td>29,619</td><td></td></tr>
  <tr><td colspan="3">International</td><td colspan="3"></td><td colspan="3"></td>
      <td colspan="3"></td><td colspan="3"></td><td colspan="3"></td></tr>
  <tr><td colspan="3">Net sales</td><td>$</td><td>131,200</td><td></td>
      <td colspan="3"></td><td>$</td><td>142,906</td><td></td>
      <td colspan="3"></td><td>$</td><td>161,894</td><td></td></tr>
  <tr><td colspan="3">Operating income (loss)</td><td>$</td><td>(2,656)</td><td></td>
      <td colspan="3"></td><td>$</td><td>3,792</td><td></td>
      <td colspan="3"></td><td>$</td><td>4,750</td><td></td></tr>
  <tr><td colspan="3">Cloud platform</td><td colspan="3"></td><td colspan="3"></td>
      <td colspan="3"></td><td colspan="3"></td><td colspan="3"></td></tr>
  <tr><td colspan="3">Net sales</td><td>$</td><td>90,757</td><td></td>
      <td colspan="3"></td><td>$</td><td>107,556</td><td></td>
      <td colspan="3"></td><td>$</td><td>128,725</td><td></td></tr>
  <tr><td colspan="3">Operating income</td><td>$</td><td>24,631</td><td></td>
      <td colspan="3"></td><td>$</td><td>39,834</td><td></td>
      <td colspan="3"></td><td>$</td><td>45,606</td><td></td></tr>
</table>
<table>
  <tr><td colspan="3"></td><td colspan="15">Year Ended December 31,</td></tr>
  <tr><td colspan="3"></td><td colspan="3">2023</td><td colspan="3"></td>
      <td colspan="3">2024</td><td colspan="3"></td><td colspan="3">2025</td></tr>
  <tr><td colspan="3">Net Sales:</td><td colspan="3"></td><td colspan="3"></td>
      <td colspan="3"></td><td colspan="3"></td><td colspan="3"></td></tr>
  <tr><td colspan="3">Online stores (1)</td><td>$</td><td>231,872</td><td></td>
      <td colspan="3"></td><td>$</td><td>247,029</td><td></td>
      <td colspan="3"></td><td>$</td><td>269,287</td><td></td></tr>
  <tr><td colspan="3">Advertising services (4)</td><td colspan="2">46,906</td><td></td>
      <td colspan="3"></td><td colspan="2">56,214</td><td></td>
      <td colspan="3"></td><td colspan="2">68,635</td><td></td></tr>
  <tr><td colspan="3">Consolidated</td><td>$</td><td>574,785</td><td></td>
      <td colspan="3"></td><td>$</td><td>637,959</td><td></td>
      <td colspan="3"></td><td>$</td><td>716,924</td><td></td></tr>
</table>
<table>
  <tr><td colspan="3"></td><td colspan="15">Year Ended December 31,</td></tr>
  <tr><td colspan="3"></td><td colspan="3">2023</td><td colspan="3"></td>
      <td colspan="3">2024</td><td colspan="3"></td><td colspan="3">2025</td></tr>
  <tr><td colspan="3">United States</td><td>$</td><td>395,637</td><td></td>
      <td colspan="3"></td><td>$</td><td>438,015</td><td></td>
      <td colspan="3"></td><td>$</td><td>489,657</td><td></td></tr>
  <tr><td colspan="3">Germany</td><td colspan="2">37,588</td><td></td>
      <td colspan="3"></td><td colspan="2">40,856</td><td></td>
      <td colspan="3"></td><td colspan="2">45,900</td><td></td></tr>
  <tr><td colspan="3">Japan</td><td colspan="2">26,002</td><td></td>
      <td colspan="3"></td><td colspan="2">27,401</td><td></td>
      <td colspan="3"></td><td colspan="2">30,688</td><td></td></tr>
  <tr><td colspan="3">Rest of world</td><td colspan="2">81,967</td><td></td>
      <td colspan="3"></td><td colspan="2">93,832</td><td></td>
      <td colspan="3"></td><td colspan="2">107,467</td><td></td></tr>
</table>
<p>Information on reportable segments (in millions).</p>
</body></html>
"""


def test_kinds():
    check(
        "segment note is operating_segment",
        _sec_segment_kind("Segment Information") == "operating_segment",
    )
    check(
        "countries footnote is geography even under Segment Information title",
        _sec_segment_kind(
            "Segment Information - Net Sales Attributed to Countries Representing Portion of Consolidated Net Sales (Details)"
        )
        == "geography",
    )
    check(
        "disaggregation stays its own dimension",
        _sec_segment_kind("Segment Information - Disaggregation of Revenue (Details)")
        == "revenue_disaggregation",
    )
    check(
        "goodwill by segment is skipped",
        _sec_segment_kind(
            "Acquisitions, Goodwill, and Acquired Intangible Assets - Summary of Goodwill Activity by Segment (Details)"
        )
        is None,
    )


def test_html_tables():
    tables = _sec_html_tables(GROUPED_HTML)
    check("extracted at least 3 tables", len(tables) >= 3)
    first = tables[0]
    check("year headers", "2025" in (first.get("headers") or []))
    labels = [r[0] for r in first.get("rows") or []]
    check("grouped segment headers present", "North America" in labels and "Net sales" in labels)
    ns = next(r for r in first["rows"] if r and r[0] == "Net sales")
    check("2025 net sales not dropped by colspan/$", ns[-1].replace(",", "") in {"426305", "426,305"})


def test_classify_and_append():
    result = {"segment_tables_json": "[]"}
    _sec_append_segment_tables(result, "Segment Information", GROUPED_HTML)
    import json

    tables = json.loads(result["segment_tables_json"])
    kinds = [t.get("kind") for t in tables]
    check("operating-segment P&L kept", "operating_segment" in kinds)
    check("geography not merged into operating_segment", "geography" in kinds)
    check("product/disaggregation separate", "revenue_disaggregation" in kinds)
    op = next(t for t in tables if t["kind"] == "operating_segment")
    check("operating-segment not flagged customer-location", not op.get("is_customer_location_only"))
    geo = next(t for t in tables if t["kind"] == "geography")
    check("geo is customer-location-only", bool(geo.get("is_customer_location_only")))
    check("scale millions", float(op.get("scale") or 0) == 1e6)
    role = _sec_classify_extracted_table(
        "Segment Information",
        "operating_segment",
        op["headers"],
        op["rows"],
    )
    check("P&L stays operating_segment", role and role["kind"] == "operating_segment")


if __name__ == "__main__":
    test_kinds()
    test_html_tables()
    test_classify_and_append()
    print("All SEC segment-table checks passed.")
