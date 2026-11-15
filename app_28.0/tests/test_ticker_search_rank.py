#!/usr/bin/env python3
"""Offline ranking check mirroring deep_scraper.search_tickers tiers."""
quotes = [
    {"symbol": "AAPU", "quoteType": "ETF", "shortname": "Bull 2X", "exchDisp": "NASDAQ"},
    {"symbol": "AAPL", "quoteType": "EQUITY", "shortname": "Apple Inc.", "exchDisp": "NASDAQ"},
    {"symbol": "AAPD", "quoteType": "ETF", "shortname": "Bear 1X", "exchDisp": "NASDAQ"},
]
q = "AAPL"
q_up = q.upper()
exact, equity, etf_idx, other = [], [], [], []
for item in quotes:
    sym = item.get("symbol")
    qt = (item.get("quoteType") or "").upper()
    sym_up = sym.upper()
    row = {"symbol": sym, "type": qt}
    if sym_up == q_up or sym_up.split(".")[0] == q_up:
        exact.append(row)
    elif qt == "EQUITY":
        equity.append(row)
    elif qt in ("ETF", "INDEX"):
        etf_idx.append(row)
    else:
        other.append(row)
seen = set()
ranked = []
for bucket in (exact, equity, etf_idx, other):
    for row in bucket:
        su = row["symbol"].upper()
        if su in seen:
            continue
        seen.add(su)
        ranked.append(row["symbol"])
assert ranked[0] == "AAPL", ranked
assert ranked.index("AAPL") < ranked.index("AAPU"), ranked
print("OK", ",".join(ranked))
