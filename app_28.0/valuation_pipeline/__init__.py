"""
valuation_pipeline — Phase 1 historical financial fetch & valuation params.

Look-ahead bias (Phase 1): Yahoo / FMP annual statements are typically
**restated**. Figures for fiscal year T as seen today may differ from what
was originally filed. True as-filed Point-in-Time (SEC EDGAR) is Phase 2+.

ADR / share-class risk: Shares Outstanding from statements may be parent
shares while the traded ticker is an ADR (or dual-class). Always cross-check
market_cap / price when converting to per-share metrics.
"""

from .data_fetcher import DataFetcher, FetchResult
from .valuation_engine import ValuationEngine, ValuationConfig

__all__ = [
    "DataFetcher",
    "FetchResult",
    "ValuationEngine",
    "ValuationConfig",
]

__version__ = "0.1.0"
