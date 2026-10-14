"""
ValuationEngine — Phase-1 parameter calculations for historical DCF / multiples.

Look-ahead bias: inputs from DataFetcher are restated annuals. Rolling beta /
Rf are as-of date when callers pass truncated price series; annual fund fields
for year T still reflect today's restated filing, not original as-filed PIT.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Literal, Optional, Sequence, Tuple, Union

import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)

Number = Union[float, int]


@dataclass
class ValuationConfig:
    """Phase-1 model assumptions (single-stage only)."""

    n_years: int = 5  # 5 or 10; two-stage deferred to Phase 2
    beta_lookback_years: int = 5  # 3 or 5
    beta_freq: Literal["W", "D"] = "W"
    near_term_cagr_years: int = 4  # use 3–5; default 4
    g_cap_mode: Literal["wacc", "gdp", "both"] = "both"
    nominal_gdp_growth: float = 0.04  # soft US nominal GDP proxy cap
    statutory_tax: float = 0.21
    tax_floor: float = 0.0
    tax_ceil: float = 0.50
    min_discount_cushion: float = 0.005  # g <= r - cushion
    erp_mode: Literal["realized", "damodaran"] = "realized"
    damodaran_erp: Optional[pd.Series] = None  # optional external ERP by date


class ValuationEngine:
    """
    Pure calculation helpers. Each method is NaN-safe and will not raise on
    a single missing field (returns np.nan / skips that period instead).
    """

    def __init__(self, config: Optional[ValuationConfig] = None) -> None:
        self.config = config or ValuationConfig()
        if self.config.n_years not in (5, 10):
            # allow other positive ints but document Phase-1 defaults
            if self.config.n_years < 1:
                self.config.n_years = 5

    # ------------------------------------------------------------------
    # Growth
    # ------------------------------------------------------------------

    def sustainable_growth_rate(
        self,
        roe: Number,
        payout: Number,
    ) -> float:
        """Historical SGR = ROE * (1 - Dividend Payout Ratio)."""
        roe_f = self._f(roe)
        pay_f = self._f(payout)
        if not np.isfinite(roe_f):
            return np.nan
        if not np.isfinite(pay_f):
            pay_f = 0.0
        pay_f = float(np.clip(pay_f, 0.0, 1.0))
        return float(roe_f * (1.0 - pay_f))

    def near_term_cagr(
        self,
        series: pd.Series,
        n_years: Optional[int] = None,
        prefer: Literal["last", "max_span"] = "last",
    ) -> float:
        """
        CAGR over the last ``n_years`` spanning observations of Revenue or FCF.

        Skips non-positive endpoints (common for FCF). Returns np.nan if
        insufficient history rather than crashing.
        """
        n = int(n_years or self.config.near_term_cagr_years)
        n = int(np.clip(n, 3, 5))
        s = pd.to_numeric(series, errors="coerce").dropna()
        if len(s) < 2:
            return np.nan
        # take last n+1 points if available (n years of growth)
        if prefer == "last":
            window = s.iloc[-(n + 1) :]
        else:
            window = s
        if len(window) < 2:
            return np.nan
        start = float(window.iloc[0])
        end = float(window.iloc[-1])
        span = len(window) - 1
        if span < 1 or not np.isfinite(start) or not np.isfinite(end):
            return np.nan
        if start <= 0 or end <= 0:
            # try absolute values for FCF sign flips — still skip if either ~0
            if start == 0 or end == 0:
                return np.nan
            # signed CAGR not meaningful for sign change
            if start * end < 0:
                return np.nan
            start, end = abs(start), abs(end)
        try:
            return float((end / start) ** (1.0 / span) - 1.0)
        except Exception:  # noqa: BLE001
            return np.nan

    def cap_growth(
        self,
        g: Number,
        wacc: Optional[Number] = None,
        ke: Optional[Number] = None,
    ) -> float:
        """
        Force g below discount rate (and optionally nominal GDP).

        Priority: g < min(WACC, Ke) - cushion; also g <= nominal_gdp if mode includes gdp.
        """
        g_f = self._f(g)
        if not np.isfinite(g_f):
            return np.nan
        caps = []
        mode = self.config.g_cap_mode
        if mode in ("wacc", "both"):
            for r in (wacc, ke):
                r_f = self._f(r)
                if np.isfinite(r_f):
                    caps.append(r_f - self.config.min_discount_cushion)
        if mode in ("gdp", "both"):
            caps.append(float(self.config.nominal_gdp_growth))
        if not caps:
            return g_f
        return float(min(g_f, min(caps)))

    def growth_bundle(
        self,
        annual_row: pd.Series,
        annual_panel: pd.DataFrame,
        wacc: Optional[Number] = None,
        ke: Optional[Number] = None,
        prefer_fcf: bool = False,
    ) -> dict:
        """
        Compute SGR, near-term g (CAGR), and capped g for one fiscal year.

        Uses panel history **up to and including** that year only (no future FY).
        """
        year = int(annual_row.name) if annual_row.name is not None else None
        hist = annual_panel
        if year is not None and "year" not in hist.columns:
            hist = hist.loc[hist.index <= year]

        roe = self._f(annual_row.get("roe", np.nan))
        if not np.isfinite(roe):
            eq = self._f(annual_row.get("equity", np.nan))
            ni = self._f(annual_row.get("net_income", np.nan))
            roe = ni / eq if np.isfinite(ni) and np.isfinite(eq) and eq != 0 else np.nan
        payout = self._f(annual_row.get("payout", np.nan))
        sgr = self.sustainable_growth_rate(roe, payout)

        rev = hist["revenue"] if "revenue" in hist.columns else pd.Series(dtype=float)
        fcf = hist["fcf"] if "fcf" in hist.columns else pd.Series(dtype=float)
        g_rev = self.near_term_cagr(rev)
        g_fcf = self.near_term_cagr(fcf)
        if prefer_fcf and np.isfinite(g_fcf):
            g_raw = g_fcf
            g_src = "fcf_cagr"
        elif np.isfinite(g_rev):
            g_raw = g_rev
            g_src = "revenue_cagr"
        elif np.isfinite(g_fcf):
            g_raw = g_fcf
            g_src = "fcf_cagr"
        elif np.isfinite(sgr):
            g_raw = sgr
            g_src = "sgr"
        else:
            g_raw = np.nan
            g_src = "missing"

        g_cap = self.cap_growth(g_raw, wacc=wacc, ke=ke)
        return {
            "roe": roe,
            "payout": payout,
            "sgr": sgr,
            "g_revenue_cagr": g_rev,
            "g_fcf_cagr": g_fcf,
            "g_raw": g_raw,
            "g_capped": g_cap,
            "g_source": g_src,
            "n_years": self.config.n_years,
        }

    # ------------------------------------------------------------------
    # Discount / CAPM
    # ------------------------------------------------------------------

    def rolling_beta(
        self,
        stock_close: pd.Series,
        bench_close: pd.Series,
        as_of: Optional[pd.Timestamp] = None,
        lookback_years: Optional[int] = None,
        freq: Optional[Literal["W", "D"]] = None,
        min_obs: int = 24,
    ) -> float:
        """
        Rolling beta from covariance/variance of returns — not Yahoo snapshot β.

        Uses only observations on/before ``as_of`` when provided.
        """
        lookback_years = int(lookback_years or self.config.beta_lookback_years)
        freq = freq or self.config.beta_freq
        s = self._align_close(stock_close, as_of)
        b = self._align_close(bench_close, as_of)
        if s.empty or b.empty:
            return np.nan
        df = pd.concat([s.rename("s"), b.rename("b")], axis=1, join="inner").dropna()
        if df.empty:
            return np.nan
        if freq == "W":
            df = df.resample("W-FRI").last().dropna()
        rets = df.pct_change().dropna()
        # lookback window
        if lookback_years > 0 and len(rets) > 0:
            cutoff = rets.index.max() - pd.DateOffset(years=lookback_years)
            rets = rets.loc[rets.index >= cutoff]
        if len(rets) < min_obs:
            return np.nan
        var_b = float(rets["b"].var(ddof=1))
        if not np.isfinite(var_b) or var_b <= 0:
            return np.nan
        cov = float(rets["s"].cov(rets["b"]))
        if not np.isfinite(cov):
            return np.nan
        return float(cov / var_b)

    def rf_asof(self, rf_series: pd.Series, as_of: Optional[pd.Timestamp] = None) -> float:
        """10Y Treasury decimal yield on/before as_of."""
        s = self._align_close(rf_series, as_of)
        if s.empty:
            return np.nan
        v = float(pd.to_numeric(s.iloc[-1], errors="coerce"))
        return v if np.isfinite(v) else np.nan

    def realized_erp(
        self,
        bench_close: pd.Series,
        rf_series: pd.Series,
        as_of: Optional[pd.Timestamp] = None,
        window_years: float = 1.0,
    ) -> float:
        """
        Realized ERP ≈ trailing annualized SPY total return − Rf_asof.

        Falls back to Damodaran series if ``erp_mode='damodaran'`` and series set.
        """
        if self.config.erp_mode == "damodaran" and self.config.damodaran_erp is not None:
            return self._lookup_damodaran_erp(as_of)

        b = self._align_close(bench_close, as_of)
        if len(b) < 2:
            return np.nan
        end = b.index.max()
        start_target = end - pd.DateOffset(years=window_years)
        prior = b.loc[b.index <= start_target]
        if prior.empty:
            # longest available ≥ 6m
            span_days = (b.index.max() - b.index.min()).days
            if span_days < 180:
                return np.nan
            p0 = float(b.iloc[0])
            p1 = float(b.iloc[-1])
            years = max(span_days / 365.25, 1e-6)
        else:
            p0 = float(prior.iloc[-1])
            p1 = float(b.iloc[-1])
            years = max((end - prior.index[-1]).days / 365.25, 1e-6)
        if p0 <= 0 or p1 <= 0 or not np.isfinite(p0) or not np.isfinite(p1):
            return np.nan
        ann = (p1 / p0) ** (1.0 / years) - 1.0
        rf = self.rf_asof(rf_series, as_of)
        if not np.isfinite(rf):
            return float(ann)
        return float(ann - rf)

    def cost_of_equity(
        self,
        rf: Number,
        beta: Number,
        erp: Number,
        floor: float = 0.01,
    ) -> float:
        """Ke = Rf + β * ERP; floor if non-positive."""
        rf_f, beta_f, erp_f = self._f(rf), self._f(beta), self._f(erp)
        if not all(np.isfinite(x) for x in (rf_f, beta_f, erp_f)):
            return np.nan
        ke = rf_f + beta_f * erp_f
        if not np.isfinite(ke):
            return np.nan
        return float(max(ke, floor))

    def wacc(
        self,
        ke: Number,
        rd: Number,
        tax: Number,
        we: Number,
        wd: Number,
        floor: float = 0.01,
    ) -> float:
        """WACC = We*Ke + Wd*Rd*(1-Tax)."""
        vals = [self._f(x) for x in (ke, rd, tax, we, wd)]
        if not all(np.isfinite(v) for v in vals):
            # if no debt weight, WACC ≈ Ke
            ke_f, we_f = self._f(ke), self._f(we)
            if np.isfinite(ke_f) and (not np.isfinite(self._f(wd)) or self._f(wd) == 0):
                return float(max(ke_f, floor))
            return np.nan
        ke_f, rd_f, tax_f, we_f, wd_f = vals
        if we_f + wd_f <= 0:
            return np.nan
        # renormalize
        tot = we_f + wd_f
        we_f, wd_f = we_f / tot, wd_f / tot
        w = we_f * ke_f + wd_f * rd_f * (1.0 - tax_f)
        if not np.isfinite(w):
            return np.nan
        return float(max(w, floor))

    # ------------------------------------------------------------------
    # Financial metrics
    # ------------------------------------------------------------------

    def cost_of_debt(self, interest_expense: Number, total_debt: Number) -> float:
        """Rd = Interest Expense / Total Debt. Skip if Debt≈0."""
        interest = self._f(interest_expense)
        debt = self._f(total_debt)
        if not np.isfinite(interest) or not np.isfinite(debt):
            return np.nan
        if abs(debt) < 1e-6:
            return np.nan  # no meaningful Rd; caller should fallback
        rd = abs(interest) / abs(debt)
        if not np.isfinite(rd) or rd < 0 or rd > 0.35:
            return np.nan
        return float(rd)

    def effective_tax_rate(
        self,
        tax_expense: Number,
        pretax_income: Number,
        statutory: Optional[float] = None,
    ) -> float:
        """
        Tax = Tax Expense / EBT; fallback to statutory 21% if abnormal.
        """
        statutory = float(statutory if statutory is not None else self.config.statutory_tax)
        tax_e = self._f(tax_expense)
        ebt = self._f(pretax_income)
        if not np.isfinite(tax_e) or not np.isfinite(ebt) or abs(ebt) < 1e-6:
            return statutory
        t = tax_e / ebt
        if not np.isfinite(t) or t < self.config.tax_floor or t > self.config.tax_ceil:
            return statutory
        return float(t)

    def tbvps(
        self,
        equity: Number,
        goodwill: Number,
        intangibles: Number,
        shares: Number,
    ) -> float:
        """TBVPS = (Equity - Goodwill - Intangibles) / Basic Shares."""
        eq = self._f(equity)
        gw = self._f(goodwill)
        ia = self._f(intangibles)
        sh = self._f(shares)
        if not np.isfinite(eq) or not np.isfinite(sh) or sh <= 1:
            return np.nan
        gw = 0.0 if not np.isfinite(gw) else gw
        ia = 0.0 if not np.isfinite(ia) else ia
        tang = eq - gw - ia
        if not np.isfinite(tang):
            return np.nan
        return float(tang / sh)

    def justified_pb(
        self,
        roe: Number,
        g: Number,
        ke: Number,
        clamp: Tuple[float, float] = (0.3, 6.0),
    ) -> float:
        """
        Justified P/B = (ROE - g) / (Ke - g).

        Returns np.nan if Ke - g <= 0 (denominator guard).
        """
        roe_f, g_f, ke_f = self._f(roe), self._f(g), self._f(ke)
        if not all(np.isfinite(x) for x in (roe_f, g_f, ke_f)):
            return np.nan
        denom = ke_f - g_f
        if denom <= 0:
            return np.nan
        pb = (roe_f - g_f) / denom
        if not np.isfinite(pb) or pb <= 0:
            return np.nan
        lo, hi = clamp
        return float(np.clip(pb, lo, hi))

    def capital_weights(
        self,
        shares: Number,
        price: Number,
        total_debt: Number,
    ) -> Tuple[float, float]:
        """Market We/Wd from price × shares and book Total Debt."""
        sh, px, debt = self._f(shares), self._f(price), self._f(total_debt)
        if not np.isfinite(sh) or not np.isfinite(px) or sh <= 0 or px <= 0:
            return np.nan, np.nan
        equity_mv = sh * px
        debt = 0.0 if not np.isfinite(debt) or debt < 0 else debt
        tot = equity_mv + debt
        if tot <= 0:
            return np.nan, np.nan
        return float(equity_mv / tot), float(debt / tot)

    # ------------------------------------------------------------------
    # Period panel builder
    # ------------------------------------------------------------------

    def compute_period_params(
        self,
        annual: pd.DataFrame,
        price: pd.DataFrame,
        bench_price: pd.DataFrame,
        rf_series: pd.Series,
        as_of_price_map: Optional[dict] = None,
    ) -> pd.DataFrame:
        """
        For each fiscal year row, compute Phase-1 valuation parameter bundle.

        Price for We/Wd: last close in that calendar year if available,
        else ``as_of_price_map[year]``, else NaN (weights skipped).

        A missing field causes that metric to be NaN for the year — the loop continues.
        """
        if annual is None or annual.empty:
            return pd.DataFrame()

        rows = []
        for year, row in annual.sort_index().iterrows():
            try:
                y = int(year)
            except Exception:  # noqa: BLE001
                continue
            try:
                as_of = pd.Timestamp(year=y, month=12, day=31)
                beta = self.rolling_beta(
                    price["Close"] if "Close" in price.columns else price.squeeze(),
                    bench_price["Close"] if "Close" in bench_price.columns else bench_price.squeeze(),
                    as_of=as_of,
                )
                rf = self.rf_asof(rf_series, as_of)
                erp = self.realized_erp(
                    bench_price["Close"] if "Close" in bench_price.columns else bench_price.squeeze(),
                    rf_series,
                    as_of=as_of,
                )
                ke = self.cost_of_equity(rf, beta, erp)
                rd = self.cost_of_debt(row.get("interest_expense"), row.get("total_debt"))
                tax = self.effective_tax_rate(row.get("tax_expense"), row.get("pretax_income"))
                px = self._price_year_end(price, y, as_of_price_map)
                we, wd = self.capital_weights(row.get("shares"), px, row.get("total_debt"))
                wacc = self.wacc(ke, rd if np.isfinite(rd) else 0.0, tax, we, wd)
                hist = annual.loc[annual.index <= y] if isinstance(annual.index, pd.Index) else annual
                gbun = self.growth_bundle(row, hist, wacc=wacc, ke=ke)
                g = gbun["g_capped"]
                tbv = self.tbvps(
                    row.get("equity"),
                    row.get("goodwill", 0),
                    row.get("intangibles", 0),
                    row.get("shares"),
                )
                jpb = self.justified_pb(gbun["roe"], g, ke)
                fcf = self._f(row.get("fcf"))
                rows.append(
                    {
                        "year": y,
                        "fcf": fcf,
                        "rf": rf,
                        "erp": erp,
                        "beta": beta,
                        "ke": ke,
                        "rd": rd,
                        "tax": tax,
                        "we": we,
                        "wd": wd,
                        "wacc": wacc,
                        "roe": gbun["roe"],
                        "payout": gbun["payout"],
                        "sgr": gbun["sgr"],
                        "g_raw": gbun["g_raw"],
                        "g": g,
                        "g_source": gbun["g_source"],
                        "n_years": self.config.n_years,
                        "tbvps": tbv,
                        "justified_pb": jpb,
                        "price_ye": px,
                        "shares": self._f(row.get("shares")),
                    }
                )
            except Exception as exc:  # noqa: BLE001 — never kill the loop
                logger.warning("year %s skipped: %s", year, exc)
                continue
        return pd.DataFrame(rows).set_index("year") if rows else pd.DataFrame()

    # ------------------------------------------------------------------
    # Internals
    # ------------------------------------------------------------------

    @staticmethod
    def _f(x: Number) -> float:
        try:
            v = float(x)
            return v if np.isfinite(v) else np.nan
        except Exception:  # noqa: BLE001
            return np.nan

    @staticmethod
    def _align_close(series: pd.Series, as_of: Optional[pd.Timestamp]) -> pd.Series:
        if series is None:
            return pd.Series(dtype=float)
        s = series.copy()
        if not isinstance(s.index, pd.DatetimeIndex):
            s.index = pd.to_datetime(s.index, errors="coerce")
        s = pd.to_numeric(s, errors="coerce").dropna()
        s = s[~s.index.isna()].sort_index()
        if as_of is not None:
            as_of = pd.Timestamp(as_of)
            s = s.loc[s.index <= as_of]
        return s

    def _lookup_damodaran_erp(self, as_of: Optional[pd.Timestamp]) -> float:
        erp = self.config.damodaran_erp
        if erp is None or len(erp) == 0:
            return np.nan
        s = self._align_close(erp, as_of)
        if s.empty:
            return np.nan
        v = float(s.iloc[-1])
        # allow percent or decimal
        if np.isfinite(v) and v > 1.0:
            v = v / 100.0
        return v if np.isfinite(v) else np.nan

    def _price_year_end(
        self,
        price: pd.DataFrame,
        year: int,
        as_of_price_map: Optional[dict],
    ) -> float:
        if as_of_price_map and year in as_of_price_map:
            return self._f(as_of_price_map[year])
        if price is None or price.empty:
            return np.nan
        col = "Close" if "Close" in price.columns else price.columns[0]
        s = price[col].copy()
        s.index = pd.to_datetime(s.index, errors="coerce")
        s = pd.to_numeric(s, errors="coerce").dropna()
        mask = s.index.year == year
        if not mask.any():
            return np.nan
        return float(s.loc[mask].iloc[-1])
