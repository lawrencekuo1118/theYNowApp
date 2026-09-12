# valuation_pipeline — Phase 1 MVP

Historical financial fetch + valuation parameter engine (Python).

## Look-ahead bias

Yahoo/FMP annuals are **restated**. Do not treat them as as-filed Point-in-Time.
SEC EDGAR PIT is Phase 2+.

## Quick start

```python
from valuation_pipeline import DataFetcher, ValuationEngine, ValuationConfig

fetch = DataFetcher(source="yfinance", bench_ticker="SPY")
data = fetch.fetch("AAPL", price_period="10y")
print(data.warnings)          # restated + ADR notes
print(data.share_align)       # ADR scale diagnostics

eng = ValuationEngine(ValuationConfig(n_years=5, beta_lookback_years=5))
params = eng.compute_period_params(
    data.annual, data.price, data.bench_price, data.rf_series
)
print(params[["fcf", "g", "ke", "wacc", "rd", "tax", "justified_pb", "tbvps"]])
```

## Tests (offline)

```bash
cd app_15.0
python tests/test_valuation_pipeline.py
```

## Layout

| File | Role |
|------|------|
| `data_fetcher.py` | `DataFetcher` / `FetchResult` — yfinance normalize + ADR guard |
| `valuation_engine.py` | `ValuationEngine` — SGR, CAGR g-cap, Rd, Tax, Rolling β, Justified P/B, TBVPS |
