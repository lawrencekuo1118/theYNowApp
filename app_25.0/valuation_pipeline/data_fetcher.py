"""
DataFetcher — Yahoo Finance (yfinance) primary; optional FMP stub.

IMPORTANT (Look-ahead bias):
    Annual IS/BS/CF from Yahoo are restated. Do not treat column year T as
    the numbers available to investors on the original filing date.
    Point-in-time as-filed history is deferred to SEC EDGAR (Phase 2+).
"""

from __future__ import annotations

import logging
import warnings
from dataclasses import dataclass, field
from typing import Any, Dict, Optional, Tuple

import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)

# Soft ADR / dual-class heuristics: if |scale - 1| exceeds this, warn.
ADR_SCALE_WARN_THRESHOLD = 0.05
ADR_SCALE_HARD_THRESHOLD = 0.50  # likely wrong share class


@dataclass
class FetchResult:
    """Normalized annual fundamentals + price histories for one ticker."""

    ticker: str
    income: pd.DataFrame  # rows=metrics, cols=period timestamps
    balance: pd.DataFrame
    cashflow: pd.DataFrame
    annual: pd.DataFrame  # tidy wide panel: index=year, columns=normalized fields
    price: pd.DataFrame  # Date index, Close
    bench_price: pd.DataFrame
    rf_series: pd.Series  # decimal yield, Date index (^TNX / 100)
    info: Dict[str, Any] = field(default_factory=dict)
    share_align: Dict[str, Any] = field(default_factory=dict)
    warnings: list = field(default_factory=list)
    restated_disclaimer: str = (
        "Phase-1 source uses restated Yahoo/FMP annuals — look-ahead bias vs as-filed PIT."
    )


class DataFetcher:
    """
    Fetch and normalize restated annual financials + market series.

    Parameters
    ----------
    source : {'yfinance', 'fmp'}
        Phase 1 implements yfinance fully; FMP raises NotImplemented unless
        ``fmp_api_key`` and a future adapter are provided.
    bench_ticker : str
        Benchmark for ERP / beta (default SPY).
    rf_ticker : str
        10Y Treasury proxy (default ^TNX; values are percent → convert to decimal).
    """

    def __init__(
        self,
        source: str = "yfinance",
        bench_ticker: str = "SPY",
        rf_ticker: str = "^TNX",
        fmp_api_key: Optional[str] = None,
    ) -> None:
        self.source = (source or "yfinance").lower().strip()
        self.bench_ticker = bench_ticker
        self.rf_ticker = rf_ticker
        self.fmp_api_key = fmp_api_key

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def fetch(
        self,
        ticker: str,
        price_period: str = "10y",
        auto_align_shares: bool = True,
    ) -> FetchResult:
        ticker = str(ticker or "").strip().upper()
        if not ticker:
            raise ValueError("ticker is required")

        warns: list = []
        warns.append(
            "Restated annuals: Phase-1 Yahoo/FMP data is not as-filed PIT "
            "(look-ahead bias). See module docstring."
        )

        if self.source == "fmp":
            return self._fetch_fmp(ticker, price_period, auto_align_shares, warns)
        if self.source != "yfinance":
            raise ValueError(f"Unsupported source: {self.source}")

        import yfinance as yf

        stock = yf.Ticker(ticker)
        info = {}
        try:
            info = dict(stock.info or {})
        except Exception as exc:  # noqa: BLE001
            warns.append(f"info fetch failed: {exc}")

        income = self._safe_stmt(stock, "income_stmt", warns)
        balance = self._safe_stmt(stock, "balance_sheet", warns)
        cashflow = self._safe_stmt(stock, "cashflow", warns)

        annual = self._build_annual_panel(income, balance, cashflow, warns)
        share_align = {}
        if auto_align_shares and not annual.empty:
            annual, share_align, w = self._align_shares_adr_guard(
                annual, info, ticker
            )
            warns.extend(w)

        price = self._history_close(ticker, price_period, warns)
        bench = self._history_close(self.bench_ticker, price_period, warns)
        rf = self._history_rf(price_period, warns)

        return FetchResult(
            ticker=ticker,
            income=income,
            balance=balance,
            cashflow=cashflow,
            annual=annual,
            price=price,
            bench_price=bench,
            rf_series=rf,
            info=info,
            share_align=share_align,
            warnings=warns,
        )

    # ------------------------------------------------------------------
    # Statement helpers
    # ------------------------------------------------------------------

    def _safe_stmt(self, stock: Any, attr: str, warns: list) -> pd.DataFrame:
        try:
            df = getattr(stock, attr, None)
            if df is None or not isinstance(df, pd.DataFrame) or df.empty:
                warns.append(f"{attr}: empty")
                return pd.DataFrame()
            return df.copy()
        except Exception as exc:  # noqa: BLE001
            warns.append(f"{attr}: {exc}")
            return pd.DataFrame()

    def _pick_row(self, df: pd.DataFrame, patterns: Tuple[str, ...]) -> Optional[pd.Series]:
        if df is None or df.empty:
            return None
        names = df.index.to_series().astype(str)
        for pat in patterns:
            # Prefer regex if pattern looks like regex; else substring
            try:
                if any(ch in pat for ch in ".*^$[]()|\\"):
                    hits = names.str.contains(pat, case=False, regex=True, na=False)
                else:
                    hits = names.str.contains(pat, case=False, regex=False, na=False)
                if hits.any():
                    return df.loc[hits.to_numpy().nonzero()[0][0]]
            except Exception:  # noqa: BLE001
                continue
        return None

    def _col_year(self, col: Any) -> Optional[int]:
        try:
            ts = pd.Timestamp(col)
            return int(ts.year)
        except Exception:  # noqa: BLE001
            s = str(col)
            digits = "".join(ch for ch in s if ch.isdigit())
            if len(digits) >= 4:
                try:
                    return int(digits[:4])
                except ValueError:
                    return None
        return None

    def _build_annual_panel(
        self,
        income: pd.DataFrame,
        balance: pd.DataFrame,
        cashflow: pd.DataFrame,
        warns: list,
    ) -> pd.DataFrame:
        """
        Build year-indexed panel with normalized fields used by ValuationEngine.

        Missing metrics become NaN for that year; never raises on a single miss.
        """
        # Collect years from any statement
        years = set()
        for df in (income, balance, cashflow):
            if df is None or df.empty:
                continue
            for c in df.columns:
                y = self._col_year(c)
                if y is not None:
                    years.add(y)
        if not years:
            warns.append("No fiscal year columns found")
            return pd.DataFrame()

        years = sorted(years)

        def val(df: pd.DataFrame, patterns: Tuple[str, ...], year: int) -> float:
            if df is None or df.empty:
                return np.nan
            row = self._pick_row(df, patterns)
            if row is None:
                return np.nan
            # match column by year
            for c in df.columns:
                if self._col_year(c) == year:
                    try:
                        v = float(pd.to_numeric(row[c], errors="coerce"))
                        return v if np.isfinite(v) else np.nan
                    except Exception:  # noqa: BLE001
                        return np.nan
            return np.nan

        rows = []
        for y in years:
            rev = val(income, ("Total Revenue", "^Total Revenue$", "^Revenue$"), y)
            ni = val(
                income,
                (
                    "Net Income Common Stockholders",
                    "Net Income From Continuing",
                    "^Net Income$",
                ),
                y,
            )
            interest = val(
                income,
                ("Interest Expense", "Net Interest Expense", "Interest Expense Non Operating"),
                y,
            )
            tax_exp = val(income, ("Tax Provision", "Income Tax Expense", "Provision For Income Taxes"), y)
            pretax = val(income, ("Pretax Income", "Income Before Tax", "^EBT$"), y)
            equity = val(
                balance,
                ("Common Stock Equity", "Stockholders Equity", "Total Equity Gross Minority Interest"),
                y,
            )
            debt = val(balance, ("^Total Debt$", "Total Debt"), y)
            if not np.isfinite(debt):
                st = val(balance, ("Current Debt", "Short Term Debt"), y)
                lt = val(balance, ("Long Term Debt",), y)
                parts = [x for x in (st, lt) if np.isfinite(x)]
                debt = float(np.nansum(parts)) if parts else np.nan
            shares = val(
                balance,
                (
                    "Ordinary Shares Number",
                    "Share Issued",
                    "Basic Average Shares",
                    "Total Shares Outstanding",
                ),
                y,
            )
            goodwill = val(balance, ("Goodwill", "^Goodwill$"), y)
            intang = val(balance, ("Other Intangible Assets", "Intangible Assets"), y)
            fcf = val(cashflow, ("^Free Cash Flow$", "Free Cash Flow"), y)
            div = val(
                cashflow,
                ("Cash Dividends Paid", "Common Stock Dividend Paid", "^Dividends Paid$"),
                y,
            )
            rows.append(
                {
                    "year": y,
                    "revenue": rev,
                    "net_income": ni,
                    "interest_expense": interest,
                    "tax_expense": tax_exp,
                    "pretax_income": pretax,
                    "equity": equity,
                    "total_debt": debt,
                    "shares": shares,
                    "goodwill": goodwill if np.isfinite(goodwill) else 0.0,
                    "intangibles": intang if np.isfinite(intang) else 0.0,
                    "fcf": fcf,
                    "dividends_paid": div,
                }
            )

        panel = pd.DataFrame(rows).set_index("year").sort_index()
        # Derived ratios (row-wise; NaN-safe)
        with np.errstate(divide="ignore", invalid="ignore"):
            panel["roe"] = panel["net_income"] / panel["equity"]
            panel["payout"] = (-panel["dividends_paid"].abs())  # placeholder
            # Cash dividends are usually negative in Yahoo CF → use abs / NI
            panel["payout"] = np.where(
                (panel["net_income"] > 0) & np.isfinite(panel["dividends_paid"]),
                panel["dividends_paid"].abs() / panel["net_income"],
                np.nan,
            )
            panel["payout"] = panel["payout"].clip(lower=0, upper=1)
        return panel

    def _align_shares_adr_guard(
        self,
        annual: pd.DataFrame,
        info: Dict[str, Any],
        ticker: str,
    ) -> Tuple[pd.DataFrame, Dict[str, Any], list]:
        """
        Scale statement shares toward quote-implied shares using mcap/price.

        Warns (does not auto-apply extreme scales) when ADR / dual-class
        mismatch is likely.
        """
        warns: list = []
        out = annual.copy()
        price = info.get("currentPrice") or info.get("regularMarketPrice") or info.get("previousClose")
        mcap = info.get("marketCap")
        meta = {"method": "none", "scale": 1.0, "applied": False}
        try:
            price_f = float(price) if price is not None else np.nan
            mcap_f = float(mcap) if mcap is not None else np.nan
        except (TypeError, ValueError):
            return out, meta, warns

        if not (np.isfinite(price_f) and price_f > 0 and np.isfinite(mcap_f) and mcap_f > 0):
            return out, meta, warns

        quote_shares = mcap_f / price_f
        stmt = out["shares"].dropna()
        if stmt.empty or not np.isfinite(stmt.iloc[-1]) or stmt.iloc[-1] <= 0:
            warns.append("ADR guard: no statement shares to align")
            return out, meta, warns

        scale = quote_shares / float(stmt.iloc[-1])
        meta = {
            "method": "mcap_over_price",
            "scale": float(scale),
            "quote_shares": float(quote_shares),
            "stmt_shares_latest": float(stmt.iloc[-1]),
            "applied": False,
            "ticker": ticker,
        }

        if abs(scale - 1.0) < ADR_SCALE_WARN_THRESHOLD:
            return out, meta, warns

        if abs(scale - 1.0) >= ADR_SCALE_HARD_THRESHOLD:
            msg = (
                f"ADR/share-class WARNING: implied scale={scale:.3g} for {ticker}. "
                "Likely ADR or dual-class mismatch; shares NOT auto-scaled. "
                "Verify Diluted/ADR ratio before per-share valuation."
            )
            warns.append(msg)
            warnings.warn(msg, UserWarning, stacklevel=2)
            meta["applied"] = False
            return out, meta, warns

        # Mild mismatch: apply uniform scale to all years (Phase-1 compromise)
        out["shares"] = out["shares"] * scale
        meta["applied"] = True
        warns.append(
            f"Shares scaled ×{scale:.3g} to align statement shares with quote "
            f"(mcap/price). Uniform scale on all FY — ADR look-through risk remains."
        )
        return out, meta, warns

    def _history_close(self, ticker: str, period: str, warns: list) -> pd.DataFrame:
        try:
            import yfinance as yf

            hist = yf.Ticker(ticker).history(period=period, auto_adjust=True)
            if hist is None or hist.empty:
                warns.append(f"price history empty: {ticker}")
                return pd.DataFrame(columns=["Close"])
            out = hist[["Close"]].copy()
            out.index = pd.to_datetime(out.index).tz_localize(None)
            return out
        except Exception as exc:  # noqa: BLE001
            warns.append(f"price history {ticker}: {exc}")
            return pd.DataFrame(columns=["Close"])

    def _history_rf(self, period: str, warns: list) -> pd.Series:
        """Return ^TNX as decimal yield (e.g. 0.04 for 4%)."""
        df = self._history_close(self.rf_ticker, period, warns)
        if df.empty:
            return pd.Series(dtype=float)
        # TNX quotes are in percent points
        s = pd.to_numeric(df["Close"], errors="coerce") / 100.0
        s.name = "rf"
        return s

    def _fetch_fmp(
        self,
        ticker: str,
        price_period: str,
        auto_align_shares: bool,
        warns: list,
    ) -> FetchResult:
        if not self.fmp_api_key:
            raise NotImplementedError(
                "FMP source requires fmp_api_key; Phase-1 default is yfinance."
            )
        raise NotImplementedError(
            "FMP adapter is stubbed for Phase 1 — wire financialmodelingprep "
            "income-statement / balance-sheet-statement / cash-flow-statement "
            "endpoints here, then reuse _build_annual_panel column mapping."
        )
