"""
Offline unit tests for valuation_pipeline (no network required).

Run:
  cd app_15.0 && python -m pytest tests/test_valuation_pipeline.py -q
  # or:
  python tests/test_valuation_pipeline.py
"""

from __future__ import annotations

import os
import sys

import numpy as np
import pandas as pd

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from valuation_pipeline import ValuationConfig, ValuationEngine  # noqa: E402


def approx(a, b, tol=1e-8):
    return np.isfinite(a) and np.isfinite(b) and abs(a - b) <= tol


def check(label, cond):
    if not cond:
        raise AssertionError(f"FAIL: {label}")
    print(f"OK: {label}")


def test_sgr_and_caps():
    eng_w = ValuationEngine(ValuationConfig(g_cap_mode="wacc", min_discount_cushion=0.005))
    check("SGR", approx(eng_w.sustainable_growth_rate(0.20, 0.40), 0.12))
    # g raw 0.15, wacc 0.09 → capped to 0.085
    check("cap below wacc", approx(eng_w.cap_growth(0.15, wacc=0.09), 0.085))
    eng_both = ValuationEngine(ValuationConfig(nominal_gdp_growth=0.04, g_cap_mode="both"))
    # both: min(wacc-cushion, gdp) → 0.04
    check("cap vs gdp", approx(eng_both.cap_growth(0.15, wacc=0.20), 0.04))


def test_tax_fallback():
    eng = ValuationEngine()
    check("normal tax", approx(eng.effective_tax_rate(21, 100), 0.21))
    check("negative tax → statutory", approx(eng.effective_tax_rate(-5, 100), 0.21))
    check("high tax → statutory", approx(eng.effective_tax_rate(80, 100), 0.21))
    check("ebt zero → statutory", approx(eng.effective_tax_rate(10, 0), 0.21))


def test_rd_and_zero_debt():
    eng = ValuationEngine()
    check("rd", approx(eng.cost_of_debt(50, 1000), 0.05))
    check("debt zero → nan", np.isnan(eng.cost_of_debt(10, 0)))


def test_justified_pb_guard():
    eng = ValuationEngine()
    check("jpb", approx(eng.justified_pb(0.15, 0.03, 0.10), (0.15 - 0.03) / (0.10 - 0.03)))
    check("ke<=g → nan", np.isnan(eng.justified_pb(0.15, 0.10, 0.09)))


def test_tbvps():
    eng = ValuationEngine()
    check("tbvps", approx(eng.tbvps(1000, 100, 50, 100), 8.5))
    check("shares bad → nan", np.isnan(eng.tbvps(1000, 0, 0, 0)))


def test_cagr():
    eng = ValuationEngine()
    s = pd.Series([100, 110, 121, 133.1], index=[2018, 2019, 2020, 2021])
    # 3y CAGR ~ 0.10
    g = eng.near_term_cagr(s, n_years=3)
    check("cagr ~10%", abs(g - 0.10) < 1e-3)
    # endpoints opposite sign → CAGR not meaningful
    fcf = pd.Series([-10, 5, 20], index=[2019, 2020, 2021])
    check("fcf sign flip → nan", np.isnan(eng.near_term_cagr(fcf, n_years=3)))


def test_rolling_beta_synthetic():
    eng = ValuationEngine(ValuationConfig(beta_lookback_years=3, beta_freq="D"))
    rng = np.random.default_rng(42)
    idx = pd.date_range("2018-01-01", periods=800, freq="B")
    bench = pd.Series(100 * np.cumprod(1 + rng.normal(0.0003, 0.01, len(idx))), index=idx)
    # stock = 1.5 * bench shock + noise
    stock_rets = 1.5 * bench.pct_change().fillna(0) + rng.normal(0, 0.002, len(idx))
    stock = pd.Series(100 * np.cumprod(1 + stock_rets), index=idx)
    beta = eng.rolling_beta(stock, bench, as_of=idx[-1], min_obs=60)
    check("beta finite", np.isfinite(beta))
    check("beta near 1.5", abs(beta - 1.5) < 0.35)


def test_period_loop_skips_bad_years():
    eng = ValuationEngine()
    annual = pd.DataFrame(
        {
            "revenue": [100, 110, 120],
            "net_income": [10, 12, np.nan],
            "interest_expense": [5, 5, 5],
            "tax_expense": [2, 2, 2],
            "pretax_income": [12, 14, 0],  # year3 tax fallback
            "equity": [50, 55, 60],
            "total_debt": [0, 100, 100],  # year1 rd nan
            "shares": [10, 10, 10],
            "goodwill": [0, 0, 0],
            "intangibles": [0, 0, 0],
            "fcf": [8, 9, 10],
            "dividends_paid": [-2, -2, -2],
            "roe": [0.2, 0.218, np.nan],
            "payout": [0.2, 0.167, np.nan],
        },
        index=[2019, 2020, 2021],
    )
    annual.index.name = "year"
    # empty prices → betas nan but loop must not crash
    price = pd.DataFrame({"Close": []})
    bench = pd.DataFrame({"Close": []})
    rf = pd.Series(dtype=float)
    out = eng.compute_period_params(annual, price, bench, rf)
    check("loop returned frame", isinstance(out, pd.DataFrame))
    check("has 3 years or fewer", len(out) <= 3)


def main():
    test_sgr_and_caps()
    test_tax_fallback()
    test_rd_and_zero_debt()
    test_justified_pb_guard()
    test_tbvps()
    test_cagr()
    test_rolling_beta_synthetic()
    test_period_loop_skips_bad_years()
    print("ALL PASS")


if __name__ == "__main__":
    main()
