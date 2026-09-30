#!/usr/bin/env python3
"""Build data/us_industry_overlay.csv for non–S&P US primary listings.

Ranks Unmapped (or missing industry_key) names by market_cap from
universe_metrics_snapshot.csv, fetches Yahoo Sector/Industry, writes rows.
industry_key is filled later by R (resolve_industry_key_from_yahoo) on load
and by scripts/fill_us_industry_overlay_keys.R after this script.

Usage (from app_18.0/):
  python3 scripts/build_us_industry_overlay.py
  python3 scripts/build_us_industry_overlay.py --max-n 80
"""
from __future__ import annotations

import argparse
import csv
import time
from datetime import datetime, timezone
from pathlib import Path

import yfinance as yf

APP = Path(__file__).resolve().parents[1]
US = APP / "data" / "us_universe.csv"
SP = APP / "data" / "sp500_universe.csv"
METRICS = APP / "data" / "universe_metrics_snapshot.csv"
OUT = APP / "data" / "us_industry_overlay.csv"
UNMAPPED = "lab.Unmapped"


def _read_csv(path: Path) -> list[dict]:
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--max-n", type=int, default=200)
    ap.add_argument("--sleep", type=float, default=0.15)
    args = ap.parse_args()

    us = _read_csv(US)
    sp = {r["ticker"].strip().upper() for r in _read_csv(SP) if r.get("ticker")}
    # Prefer already-mapped overlay / skip known ADR keys by excluding prior mapped overlay
    prior_mapped = {
        (r.get("ticker") or "").strip().upper()
        for r in _read_csv(OUT)
        if (r.get("ticker") or "").strip()
        and (r.get("industry_key") or "").strip()
        and (r.get("industry_key") or "").strip() != UNMAPPED
    }
    caps = {}
    for r in _read_csv(METRICS):
        tk = (r.get("ticker") or "").strip().upper()
        try:
            caps[tk] = float(r.get("market_cap") or 0)
        except Exception:
            caps[tk] = 0.0

    # Prefer US common stocks: skip obvious multi-share ADR-heavy names already in prior_mapped
    # by requiring either no prior row or empty industry_key in prior.
    candidates = []
    for r in us:
        tk = (r.get("ticker") or "").strip().upper()
        if not tk or tk in sp or tk in prior_mapped:
            continue
        key = (r.get("industry_key") or "").strip()
        if key and key != UNMAPPED:
            continue
        candidates.append((caps.get(tk, 0.0), tk))
    candidates.sort(reverse=True)
    candidates = candidates[: max(10, int(args.max_n))]
    print(f"Fetching Yahoo industry for {len(candidates)} tickers…", flush=True)

    prior = {
        (r.get("ticker") or "").strip().upper(): r
        for r in _read_csv(OUT)
        if (r.get("ticker") or "").strip()
    }
    refresh = {tk for _, tk in candidates}
    kept = [r for tk, r in prior.items() if tk not in refresh]

    now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    new_rows = []
    for i, (_, tk) in enumerate(candidates, 1):
        sector = industry = ""
        try:
            info = yf.Ticker(tk).info or {}
            sector = str(info.get("sector") or info.get("sectorDisp") or "").strip()
            industry = str(info.get("industry") or info.get("industryDisp") or "").strip()
        except Exception as e:
            print(f"[{i}/{len(candidates)}] {tk} ERR {e}", flush=True)
            time.sleep(args.sleep)
            continue
        if not sector and not industry:
            print(f"[{i}/{len(candidates)}] {tk} empty", flush=True)
            time.sleep(args.sleep)
            continue
        raw = f"Sector: {sector} | Industry: {industry}"
        new_rows.append(
            {
                "ticker": tk,
                "sector": sector,
                "industry": industry,
                "industry_key": "",
                "industry_raw": raw,
                "source": "yahoo",
                "fetched_at": now,
            }
        )
        print(f"[{i}/{len(candidates)}] {tk} → {sector} / {industry}", flush=True)
        time.sleep(args.sleep)

    # merge
    by_tk = {r["ticker"].strip().upper(): r for r in kept if r.get("ticker")}
    for r in new_rows:
        by_tk[r["ticker"]] = r
    rows = [by_tk[k] for k in sorted(by_tk)]
    fields = [
        "ticker",
        "sector",
        "industry",
        "industry_key",
        "industry_raw",
        "source",
        "fetched_at",
    ]
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with OUT.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for r in rows:
            w.writerow({k: r.get(k, "") for k in fields})
    print(f"Wrote {len(rows)} rows → {OUT}", flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
