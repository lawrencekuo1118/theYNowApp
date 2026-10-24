# Global Prediction (全球預測)

Status: **v27.24 / bundle 12733971 live** ([#494](https://github.com/lawrencekuo1118/theYNowApp/pull/494) + baseline [#498](https://github.com/lawrencekuo1118/theYNowApp/pull/498) + [Release v27.24](https://github.com/lawrencekuo1118/theYNowApp/releases/tag/v27.24)). Tip free.

## Theme sort key（總體 → 個體）

Display order is **breadth first**, never ticker-led when broader themes exist:

| `breadth_tier` | Meaning | Examples |
|---|---|---|
| **1** | 總體 Macro | Fed/rates, CPI/inflation, recession, 台灣總經／選舉 |
| **2** | 市場／指數 Market / index | Treasury yields, equities/index, 台股／TAIEX |
| **3** | 題材 Sector / theme | 台灣房市 |
| **4** | 個股 Single-stock | *(reserved; none shipped yet)* |

Within a tier, `breadth_ord` breaks ties. `gp_theme_registry()` always returns rows sorted by `(breadth_tier, breadth_ord, theme_id)`.

### Ordered registries (confirmed)

**US mode:** `fed_rates` → `inflation_cpi` → `recession` → `treasury_yields` → `equities_index`

**TW mode:** `fed_rates` → `inflation_cpi` → `tw_macro` → `tw_equity` → `tw_housing`

## Mode → venue matrix

| Mode | Theme set | Venues |
|---|---|---|
| **US** | US ordered list above | Kalshi `#00D66C` + Polymarket `#2E5CFF` + 未來事件交易所 `#C17A2E` (honest empty) |
| **TW** | TW ordered list; Fed/CPI overlap | Same three columns; FEEX labeled priority but **no live public feed** |

## 未來事件交易所

xFuture / NCCU historical prediction market. Live probe: hosts DNS-fail. Academic DB was application-only. UI: honest empty, never invent probs.

## 時效性重掃規則

Prediction themes are **time-bound** (next FOMC, CPI print, election windows). Classification and quotes must not stick on the slow 24h statement cache.

### Layers (fast cache only)

| Layer | TTL | What | Key helper |
|---|---|---|---|
| **Quote** | **3 min** (`.GP_QUOTE_TTL_SEC`) | Live Yes probs | `.gp_cache_key("kalshi"\|"poly"\|"feex", …)` |
| **Discovery** | **15 min** (`.GP_DISCOVERY_TTL_SEC`) | Theme→contract series/query scan + reclassify | same keys, stamped `layer` |
| App fast tier ceiling | ≤15 min | Shared `.ynow_fast_cache` (never slow/24h) | — |

Keys are **cachem-safe** (`^[a-z0-9]+$`, always prefixed `gp…`). Entries are **stamped** (`cached_at` + `layer` + `value`). Legacy unstamped payloads are treated as **miss** (force refresh). Hard triggers call `gp_invalidate_crawl_cache()` which drops every `gp*` key.

### Triggers (rescan)

| Trigger | Kind | Behavior |
|---|---|---|
| `tab_open` | **Hard** | ATX → Global Prediction becomes visible: invalidate → discovery rescan |
| `tab_focus` | **Hard** (rate-gated 30s) | `visibilitychange` / window `focus` while panel visible |
| `mode_switch` | **Hard** | TW↔US market mode change → invalidate + rescan |
| `manual_refresh` | **Hard** | Refresh button |
| `periodic_visible` | **Soft** | Every **3 min** while panel visible; quote TTL; min gap **30s** |

### Rescan → update or reclassify

1. **Drop** contracts with closed/settled/expired `raw_status` or past `expires_at`.
2. **Dedupe** by `venue + market_id`.
3. **Reclassify** live titles against registry `keywords`; promote into best-fit `theme_id` / `breadth_tier`.
4. Re-bucket into mode registry order. Never invent Yes probs; fail closed / honest empty (FEEX stays defunct until a public feed exists).

## 學習筆記

[Medium wisdom of crowds](https://medium.com/marketingdatascience/提高預測準確率的解方-群眾智慧-the-wisdom-of-crowds-4a1aafe21755): Galton aggregation; fee/prize = filtered incentivized crowd; Iowa Electronic Market + 未來事件交易所. UI: diversity, independence/herding caution, labeled simple mean, participation/risk-appetite caveats. ≠ Fair Value, ≠ buy signal.

## Ship

- Live: **v27.24 / 12733971** ([Release](https://github.com/lawrencekuo1118/theYNowApp/releases/tag/v27.24)).
- Prior: v27.23 / 12733850 (#495 points gate); v27.22 / 12733795 (#492 GP initial).
