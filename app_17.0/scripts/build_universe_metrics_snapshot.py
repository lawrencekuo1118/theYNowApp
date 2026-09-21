#!/usr/bin/env python3
"""Build offline universe metrics: market_cap, ret_1y, paid_in_capital (TW).

Writes data/universe_metrics_snapshot.csv so Blue Chip / Clustering truncate
can rank by market cap or 1Y return without live Yahoo for the full universe.

Sources:
  - market_cap / ret_1y: Yahoo Finance (quote batch + history download)
  - paid_in_capital (實收資本額): TWSE/TPEx MOPS open data (TW only)
  - optional seed: data/cluster_features_snapshot.csv market_cap

Usage:
  python3 scripts/build_universe_metrics_snapshot.py
  python3 scripts/build_universe_metrics_snapshot.py --max-n 200   # smoke
  python3 scripts/build_universe_metrics_snapshot.py --skip-ret    # caps + capital only
"""
from __future__ import annotations

import argparse
import csv
import io
import json
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from urllib.request import Request, urlopen

import yfinance as yf

APP = Path(__file__).resolve().parents[1]
OUT = APP / "data" / "universe_metrics_snapshot.csv"
US = APP / "data" / "us_universe.csv"
TW = APP / "data" / "tw_universe.csv"
CLUSTER_SNAP = APP / "data" / "cluster_features_snapshot.csv"

MOPS_URLS = {
    "TWSE": "https://mopsfin.twse.com.tw/opendata/t187ap03_L.csv",
    "TPEX": "https://mopsfin.twse.com.tw/opendata/t187ap03_O.csv",
    "ESB": "https://mopsfin.twse.com.tw/opendata/t187ap03_R.csv",
}


def _sf(v):
    try:
        if v is None or v == "":
            return None
        x = float(v)
        if x != x:
            return None
        return x
    except (TypeError, ValueError):
        return None


def _norm_sym(s: str) -> str:
    return str(s or "").strip().upper().replace("/", "-")


def load_universe_tickers(max_n: int = 0) -> list[str]:
    tks: list[str] = []
    for path in (US, TW):
        if not path.exists():
            continue
        with path.open(newline="", encoding="utf-8") as f:
            for row in csv.DictReader(f):
                t = _norm_sym(row.get("ticker") or "")
                if t:
                    tks.append(t)
    seen = set()
    out = []
    for t in tks:
        if t not in seen:
            seen.add(t)
            out.append(t)
    if max_n > 0:
        out = out[:max_n]
    return out


def seed_mcap_from_cluster() -> dict[str, float]:
    out: dict[str, float] = {}
    if not CLUSTER_SNAP.exists():
        return out
    with CLUSTER_SNAP.open(newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            t = _norm_sym(row.get("ticker") or "")
            cap = _sf(row.get("market_cap"))
            if t and cap is not None and cap > 0:
                out[t] = cap
    return out


def fetch_market_caps(tickers: list[str]) -> dict[str, float | None]:
    out: dict[str, float | None] = {t: None for t in tickers}
    if not tickers:
        return out
    chunk_size = 40
    try:
        from yfinance.data import YfData

        yd = YfData()
        url = "https://query2.finance.yahoo.com/v7/finance/quote"
    except Exception as e:  # noqa: BLE001
        print(f"WARN YfData unavailable: {e}", flush=True)
        return out

    for i in range(0, len(tickers), chunk_size):
        chunk = tickers[i : i + chunk_size]
        try:
            raw = yd.get(url, params={"symbols": ",".join(chunk)})
            if hasattr(raw, "json") and not isinstance(raw, dict):
                raw = raw.json()
            quotes = []
            if isinstance(raw, dict):
                quotes = (raw.get("quoteResponse") or {}).get("result") or []
            for q in quotes or []:
                if not isinstance(q, dict):
                    continue
                sym = _norm_sym(q.get("symbol") or "")
                cap = _sf(q.get("marketCap"))
                if not sym or cap is None or cap <= 0:
                    continue
                for key in (sym, sym.replace(".", "-"), sym.replace("-", ".")):
                    if key in out and out[key] is None:
                        out[key] = cap
                if sym not in out:
                    out[sym] = cap
        except Exception as e:  # noqa: BLE001
            print(f"WARN quote chunk @{i}: {e}", flush=True)
        if i and i % 400 == 0:
            ok = sum(1 for v in out.values() if v is not None)
            print(f"  mcap progress {i}/{len(tickers)} ok≈{ok}", flush=True)
        time.sleep(0.08)
    return out


def fetch_returns_1y(tickers: list[str], batch: int = 60) -> dict[str, float | None]:
    out: dict[str, float | None] = {t: None for t in tickers}
    if not tickers:
        return out
    for i in range(0, len(tickers), batch):
        chunk = tickers[i : i + batch]
        try:
            # Multi-ticker download is far faster than serial Ticker.history
            data = yf.download(
                tickers=" ".join(chunk),
                period="1y",
                group_by="ticker",
                auto_adjust=True,
                threads=True,
                progress=False,
            )
        except Exception as e:  # noqa: BLE001
            print(f"WARN download batch @{i}: {e}", flush=True)
            time.sleep(1.0)
            continue

        def _ret_from_close(closes) -> float | None:
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

        if data is None or getattr(data, "empty", True):
            time.sleep(0.4)
            continue

        # Single-ticker: columns are OHLCV; multi: MultiIndex (ticker, field)
        if len(chunk) == 1:
            sym = chunk[0]
            col = "Close" if "Close" in data.columns else None
            if col is not None:
                out[sym] = _ret_from_close(data[col])
        else:
            # yfinance may return MultiIndex columns (ticker, field) or (field, ticker)
            cols = data.columns
            if getattr(cols, "nlevels", 1) >= 2:
                level0 = set(str(x) for x in cols.get_level_values(0))
                # Prefer ticker at level 0 when symbols appear there
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

        ok = sum(1 for v in out.values() if v is not None)
        print(
            f"  ret_1y progress {min(i + batch, len(tickers))}/{len(tickers)} ok≈{ok}",
            flush=True,
        )
        time.sleep(0.35)
    return out


def _decode_mops(raw: bytes) -> str:
    for enc in ("utf-8-sig", "cp950", "big5", "utf-8"):
        try:
            return raw.decode(enc)
        except UnicodeDecodeError:
            continue
    return raw.decode("utf-8", errors="replace")


def fetch_tw_paid_in_capital() -> dict[str, float]:
    """實收資本額 (TWD) from MOPS open CSV → Yahoo-style ticker keys."""
    out: dict[str, float] = {}
    suffix = {"TWSE": ".TW", "TPEX": ".TWO", "ESB": ".TWO"}
    for board, url in MOPS_URLS.items():
        try:
            req = Request(url, headers={"User-Agent": "Mozilla/5.0 theYNowApp"})
            with urlopen(req, timeout=45) as resp:
                text = _decode_mops(resp.read())
        except Exception as e:  # noqa: BLE001
            print(f"WARN MOPS {board}: {e}", flush=True)
            continue
        reader = csv.DictReader(io.StringIO(text))
        n = 0
        for row in reader:
            code = (row.get("公司代號") or row.get("Code") or "").strip()
            code = "".join(ch for ch in code if ch.isalnum())
            if not code:
                continue
            cap = _sf(row.get("實收資本額") or row.get("實收資本額(元)"))
            if cap is None or cap <= 0:
                continue
            sym = (code.upper() + suffix[board])
            # Prefer TWSE over TPEx/ESB if duplicate keys collide on .TWO
            if sym in out and board == "ESB":
                continue
            out[sym] = cap
            n += 1
        print(f"  paid_in_capital {board}: {n}", flush=True)
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--max-n", type=int, default=0, help="Limit tickers (smoke test)")
    ap.add_argument("--skip-ret", action="store_true", help="Skip 1Y return download")
    ap.add_argument("--skip-mcap", action="store_true", help="Skip Yahoo market-cap quotes")
    ap.add_argument("--ret-batch", type=int, default=60)
    args = ap.parse_args()

    tickers = load_universe_tickers(args.max_n)
    print(f"Universe tickers: {len(tickers)} → {OUT}", flush=True)

    seed = seed_mcap_from_cluster()
    print(f"Seed mcap from cluster snapshot: {len(seed)}", flush=True)

    caps: dict[str, float | None] = {t: seed.get(t) for t in tickers}
    if not args.skip_mcap:
        print("Fetching Yahoo market caps…", flush=True)
        live = fetch_market_caps(tickers)
        for t, v in live.items():
            if v is not None and v > 0:
                caps[t] = v
    n_cap = sum(1 for v in caps.values() if v is not None and v > 0)
    print(f"market_cap filled: {n_cap}/{len(tickers)}", flush=True)

    rets: dict[str, float | None] = {t: None for t in tickers}
    if not args.skip_ret:
        print("Fetching Yahoo 1Y returns…", flush=True)
        rets = fetch_returns_1y(tickers, batch=max(10, args.ret_batch))
    n_ret = sum(1 for v in rets.values() if v is not None)
    print(f"ret_1y filled: {n_ret}/{len(tickers)}", flush=True)

    print("Fetching TW paid-in capital (實收資本額)…", flush=True)
    paid = fetch_tw_paid_in_capital()
    n_paid = sum(1 for t in tickers if t in paid)
    print(f"paid_in_capital matched: {n_paid}/{len(tickers)} (TW subset)", flush=True)

    snap_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    fields = [
        "ticker",
        "market_cap",
        "ret_1y",
        "paid_in_capital",
        "snapshot_at",
        "source",
    ]
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with OUT.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for t in tickers:
            src_bits = []
            if caps.get(t) is not None:
                src_bits.append("yahoo_mcap" if t not in seed or seed.get(t) != caps.get(t) else "cluster_seed")
            if rets.get(t) is not None:
                src_bits.append("yahoo_ret")
            if t in paid:
                src_bits.append("mops_paid_in")
            w.writerow(
                {
                    "ticker": t,
                    "market_cap": caps.get(t) if caps.get(t) is not None else "",
                    "ret_1y": rets.get(t) if rets.get(t) is not None else "",
                    "paid_in_capital": paid.get(t) if t in paid else "",
                    "snapshot_at": snap_at,
                    "source": "|".join(src_bits) if src_bits else "empty",
                }
            )

    meta = {
        "path": str(OUT),
        "n_tickers": len(tickers),
        "n_market_cap": n_cap,
        "n_ret_1y": n_ret,
        "n_paid_in_capital": n_paid,
        "snapshot_at": snap_at,
    }
    print(json.dumps(meta, ensure_ascii=False), flush=True)


if __name__ == "__main__":
    main()
