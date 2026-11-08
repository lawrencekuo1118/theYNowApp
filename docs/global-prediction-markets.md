# Global Prediction (全球預測)

Standalone Full sidebar tab (`global_prediction`). Lite omits it; Full guests see a Sign-in gate.

## Boards

| Board | Frequency / filter | Kalshi | Polymarket |
|---|---|---|---|
| **Economic** | Monthly+ | `/category/economics` (+ Commodities series) | `/economy`, `/finance/commodities` |
| **Finance** | Monthly+ | `/category/financials/frequency/monthly` + `/annual` | `/finance/monthly`, `/finance/indicies` |
| **Company** | All open markets except Daily / Hourly | [Big Tech & Business](https://kalshi.com/category/science/big-tech-business) (`Science and Technology` + tag `Big Tech & Business`, paginated search) | [Big Tech](https://polymarket.com/tech/big-tech) (`tag_slug=big-tech`, Active, every gamma sort unioned) |

Theme cards within each board (examples):

- **Economic:** Fed / policy rates · Inflation / CPI · Growth / jobs · Recession / labor · Housing · Energy / commodities · Trade / tariffs · Other
- **Finance:** Rates / credit · Equity indices · Volatility / credit stress · FX · IPO / M&A · Other
- **Company:** one block per company. Kalshi on the left, Polymarket on the right. A company on only one venue still shows; the other side stays empty. Same proposition is one multi-line chart.

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

## Company sources

- Kalshi public search `GET /v1/search/series` plus series `frequency`. Cursor pages until `next_cursor` is empty. Status `open`. Drop a series when frequency is missing or is Daily/Hourly.
- Polymarket gamma `GET /events?tag_slug=big-tech&active=true&closed=false`, paged with `offset` for each sort: 24hr volume, total volume, liquidity, newest (`startDate`), ending soon (`endDate`), competitive, and `createdAt`. Union by event id. Active matches the page default (not All, which would include closed).
- The page label “Earn 3.25%” is Polymarket’s rewards-APY sort. Gamma returns HTTP 422 for a rewards `order` on `/events`. That control reorders this same Active catalog, so the union already includes those events.
- Recurrence comes from `GET /series?slug=`. Explicit `daily` / `hourly` events are dropped. Events with no series have no Daily/Hourly badge and stay. A series slug whose lookup fails is dropped (frequency unknown).
- Requests are paced (~0.15s Kalshi, ~0.08s Polymarket). A failed Kalshi page fails the Kalshi board closed. Polymarket returns quotes when at least one sort produced events.
