#!/usr/bin/env python3
"""Offline parser tests for TW MOPS annual-report concentration extraction."""

from __future__ import annotations

import os
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, ROOT)

from tw_annual_report_orders import (  # noqa: E402
    parse_annual_report_text,
    _parse_customer_table,
    _parse_geography_shares,
)


def check(name: str, cond: bool) -> None:
    if not cond:
        raise AssertionError(name)
    print("OK:", name)


FIXTURE = """
5.4 Customer Trust
Customers Accounting for at Least 10% of Annual Consolidated Net Revenue in 2024 and 2023
Unit: NT$ thousands
                                                      2024                                                      2023
        Customer
                                                 As % of 2024 Total                                        As % of 2023 Total
                                 Net Revenue                          Relation to TSMC     Net Revenue                          Relation to TSMC
                                                       Net Revenue                                               Net Revenue
        Customer A                 624,345,477                 22%    None                   546,550,925                 25%    None
        Customer B                 352,271,213                 12%    None                          N/A                   N/A   None
        Customer C                        N/A                   N/A   None                   241,152,357                 11%    None
        Others                   1,917,691,009                 66%    -                    1,374,032,559                 64%    -
        Total Net Revenue        2,894,307,699                100%    -                    2,161,735,841                100%    -
        Reason for Increase or Decrease: The changes of sales amount and percentage were mainly due to customer product demand
Net revenue by geography, calculated mainly on the country in which
customer companies are headquartered, was: 70% from
North America; 11% from China; 10% from the Asia Pacific
region, excluding China and Japan; 5% from Japan; and 4%
from Europe, the Middle East and Africa. Net revenue by
platform was: 51% HPC.
Differentiation
TSMC's leadership position is based on three defining
最近二年度主要供應商資料
項目 名稱 金額 占全年度進貨淨額比率
1 供應商甲 12,000,000 15%
2 供應商乙 8,500,000 11%
其他 其他 74%
進貨淨額 100%
增減變動原因：產能配置調整。
"""


def test_customer_rows():
    rows = _parse_customer_table(FIXTURE)
    check("parses at least Customer A and B", len(rows) >= 2)
    names = [r["name"] for r in rows]
    check("has Customer A", any(n == "Customer A" for n in names))
    by_name = {r["name"]: r for r in rows}
    check("Customer A ~22%", abs(by_name["Customer A"]["revenue_pct"] - 0.22) < 1e-9)
    check("skips Others", not any(n.lower().startswith("other") for n in names))
    check("skips section title", not any("Accounting for" in n for n in names))


def test_geography_rows():
    rows = _parse_geography_shares(FIXTURE)
    check("geography rows >= 3", len(rows) >= 3)
    check("North America present", any("North America" in r["name"] for r in rows))
    top = max(rows, key=lambda r: r["revenue_pct"])
    check("top geo ~70%", abs(top["revenue_pct"] - 0.70) < 1e-9)


def test_full_parse():
    parsed = parse_annual_report_text(FIXTURE)
    kinds = [t["kind"] for t in parsed["segment_tables"]]
    check("customer table kind", "customer_concentration" in kinds)
    check("supplier table kind", "supplier_concentration" in kinds)
    check("geography kind", "geography" in kinds)
    check("excerpts non-empty", len(parsed["excerpts"]) >= 1)


def test_none_above_threshold():
    text = "The Company had no single customer accounted for more than 10% of net sales in 2024."
    parsed = parse_annual_report_text(text)
    check("no customer table", not any(t["kind"] == "customer_concentration" for t in parsed["segment_tables"]))
    check("threshold excerpt", any("10%" in e for e in parsed["excerpts"]))


if __name__ == "__main__":
    test_customer_rows()
    test_geography_rows()
    test_full_parse()
    test_none_above_threshold()
    print("All tw_annual_report_orders offline checks passed.")
