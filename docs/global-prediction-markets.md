# Global Prediction (全球預測)

Standalone Full sidebar tab (`global_prediction`). Lite omits it; Full guests see a Sign-in gate.

## Boards

| Board | Frequency / filter | Kalshi | Polymarket |
|---|---|---|---|
| **Economic** | Monthly+ | `/category/economics` (+ Commodities series) | `/economy`, `/finance/commodities` |
| **Finance** | Monthly+ | `/category/financials/frequency/monthly` + `/annual` | `/finance/monthly`, `/finance/indicies` |
| **Company** | TW/US listed names | `company-reports` / Companies | `/tech`, `/finance/stocks`, `/finance/earnings` |

Theme cards within each board (examples):

- **Economic:** Fed / policy rates · Inflation / CPI · Growth / jobs · Recession / labor · Housing · Energy / commodities · Trade / tariffs · Other
- **Finance:** Rates / credit · Equity indices · Volatility / credit stress · FX · IPO / M&A · Other
- **Company:** US mega-cap · Semiconductors · Taiwan listed · Earnings · Other

Crowd-implied probabilities from public APIs only. Not Fair Value, not a buy signal. Empty panels stay blank — never invent Yes probs.

## Venues

| Venue | Brand | Notes |
|---|---|---|
| Kalshi | `#00D66C` | Series `frequency` filter for Eco/Finance monthly+ |
| Polymarket | `#2E5CFF` | Tag + public-search fallback |
| 未來事件交易所 | `#C17A2E` | Honest empty (no live public feed) |

## Freshness / crawl

| Layer | TTL | Cache |
|---|---|---|
| Quote | 3 min | `.ynow_fast_cache` (`gp…` keys) |
| Discovery | 15 min | same |

Hard triggers: `tab_open`, `tab_focus`, `mode_switch`, `manual_refresh` → invalidate → rescan. Soft: `periodic_visible` every 3 min while the tab is open (min gap 30s).

## Ship note

This redesign is PR-only while tip is held elsewhere — do not tip-serialize deploy from this branch until tip is free.
