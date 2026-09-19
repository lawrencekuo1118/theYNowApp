#!/usr/bin/env python3
"""Build offline Clustering feature snapshot (S&P 500 + TWSE/TPEX).

Writes data/cluster_features_snapshot.csv for app_17.0 so Clustering works
when Yahoo crumb/quoteSummary is rate-limited on shinyapps.
"""
from __future__ import annotations

import csv
import json
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path

import yfinance as yf

APP = Path(__file__).resolve().parents[1]
OUT = APP / "data" / "cluster_features_snapshot.csv"
SP500 = APP / "data" / "sp500_universe.csv"
TW = APP / "data" / "tw_universe.csv"


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


def _pct(v):
    x = _sf(v)
    if x is None:
        return None
    return x * 100.0 if abs(x) <= 1.5 else x


def _debt(v):
    x = _sf(v)
    if x is None:
        return None
    return x * 100.0 if abs(x) < 5 else x


def _row(sym, info):
    info = info or {}
    pe = _sf(info.get("trailingPE") if info.get("trailingPE") is not None else info.get("forwardPE"))
    pb = _sf(info.get("priceToBook"))
    return {
        "ticker": sym,
        "name": info.get("shortName") or info.get("longName") or sym,
        "market_cap": _sf(info.get("marketCap")),
        "ROE": _pct(info.get("returnOnEquity")),
        "Operating_Margin": _pct(info.get("operatingMargins")),
        "Rev_YoY": _pct(info.get("revenueGrowth")),
        "OpInc_YoY": _pct(
            info.get("earningsQuarterlyGrowth")
            if info.get("earningsQuarterlyGrowth") is not None
            else info.get("earningsGrowth")
        ),
        "Debt_Ratio": _debt(info.get("debtToEquity")),
        "PE_Ratio": pe if pe is not None and pe > 0 else None,
        "PB_Ratio": pb if pb is not None and pb > 0 else None,
    }


def _usable(r):
    keys = ("ROE", "Operating_Margin", "Rev_YoY", "OpInc_YoY", "Debt_Ratio", "PE_Ratio", "PB_Ratio")
    return sum(1 for k in keys if r.get(k) is not None) >= 2


def load_tickers():
    tks = []
    with SP500.open(newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            t = (row.get("ticker") or "").strip().upper()
            if t and not t.endswith((".TW", ".TWO")):
                tks.append(t)
    with TW.open(newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            ex = (row.get("exchange") or "").strip().upper()
            if ex in ("ESB",):
                continue  # 興櫃 excluded from Blue Chip quality pool
            t = (row.get("ticker") or "").strip().upper()
            if t.endswith((".TW", ".TWO")):
                tks.append(t)
    # unique preserve order
    seen = set()
    out = []
    for t in tks:
        if t not in seen:
            seen.add(t)
            out.append(t)
    return out


def fetch_one(sym):
    try:
        info = yf.Ticker(sym).info or {}
        r = _row(sym, info)
        if _usable(r):
            return r
        # one retry after short pause
        time.sleep(0.25)
        info = yf.Ticker(sym).info or {}
        return _row(sym, info)
    except Exception as e:  # noqa: BLE001
        return {
            "ticker": sym,
            "name": sym,
            "market_cap": None,
            "ROE": None,
            "Operating_Margin": None,
            "Rev_YoY": None,
            "OpInc_YoY": None,
            "Debt_Ratio": None,
            "PE_Ratio": None,
            "PB_Ratio": None,
            "_err": str(e)[:120],
        }


def main():
    tickers = load_tickers()
    # Cap TW volume for first ship if env set; default = all
    max_n = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    if max_n > 0:
        tickers = tickers[:max_n]
    print(f"Fetching {len(tickers)} tickers → {OUT}", flush=True)
    rows = []
    workers = 6
    done = 0
    with ThreadPoolExecutor(max_workers=workers) as ex:
        futs = {ex.submit(fetch_one, s): s for s in tickers}
        for fut in as_completed(futs):
            rows.append(fut.result())
            done += 1
            if done % 50 == 0 or done == len(tickers):
                ok = sum(1 for r in rows if _usable(r))
                print(f"  {done}/{len(tickers)} fetched; usable so far≈{ok}", flush=True)

    # stable order by input list
    by = {r["ticker"]: r for r in rows}
    ordered = [by[t] for t in tickers if t in by]
    usable = [r for r in ordered if _usable(r)]
    print(f"usable {len(usable)}/{len(ordered)}", flush=True)

    fields = [
        "ticker",
        "name",
        "market_cap",
        "ROE",
        "Operating_Margin",
        "Rev_YoY",
        "OpInc_YoY",
        "Debt_Ratio",
        "PE_Ratio",
        "PB_Ratio",
        "snapshot_at",
    ]
    snap_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with OUT.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for r in usable:
            w.writerow({k: r.get(k) for k in fields[:-1]} | {"snapshot_at": snap_at})
    meta = {
        "path": str(OUT),
        "n_requested": len(tickers),
        "n_usable": len(usable),
        "snapshot_at": snap_at,
    }
    print(json.dumps(meta), flush=True)


if __name__ == "__main__":
    main()
