# Capacity: ≥500 concurrent sessions

## Verdict (live audit)

| Item | Value |
|------|--------|
| App | https://hopesmasher1118.shinyapps.io/TheYNowApp/ |
| Plan | **Starter** ($13/mo) |
| Max instances | **1** |
| Max workers / instance | **1** |
| Max instance size | **large** (1024 MB) |
| Theoretical max connections | `1 × 1 × 50 = 50` |
| **Can hold 500 concurrent?** | **No — plan ceiling is 50** |

shinyapps.io concurrent capacity is:

```text
instances × max_workers × max_connections
```

Starter cannot exceed one instance and one worker, so **500 simultaneous logins is impossible** until the subscription is upgraded.

## Required upgrade

| Plan | Max instances | Max workers | Large+ sizes | Fits 500*? |
|------|---------------|-------------|--------------|------------|
| Starter (current) | 1 | 1 | ≤ large (1 GB) | No (≤50) |
| Basic | 3 | 10 | ≤ xxxlarge | Unlikely (memory) |
| Standard | 5 | 10 | ≤ xxxlarge | Marginal |
| **Professional** | **10** | **10** | **≤ xxxlarge (8 GB)** | **Yes (recommended)** |

\*With this app’s memory profile (valuation + optional Python + Lab/HFV), use a **≥120 MB usable RAM per connection** floor after reserving ~280 MB base RSS per worker.

**Recommended Professional layout** (applied automatically after upgrade):

- Instance size: `xxxlarge` (8192 MB)
- Instances: `10`
- Max workers: ~4–5 (not 10 — base RSS)
- Max connections / worker: ~10–13
- Product: ≥500
- Connection idle timeout: 600s (reclaim abandoned tabs)

Billing: [shinyapps.io account billing](https://www.shinyapps.io/admin/#/account/hopesmasher1118/billing) → upgrade to **Professional**, then run:

```bash
Rscript scripts/configure_shinyapps_capacity.R
```

## Architecture tuned for *current* Starter ceiling

Design target while still on Starter: **maximize safe concurrent browse/search sessions on 1× large (1 GB) × 1 worker** (about **7** connections at ≥100 MB usable/conn after opts), not fake a 500 number.

| Lever | Where | Effect |
|-------|--------|--------|
| Session teardown | `ynow_server.R` `onSessionEnded` | Drop financials / HFV / Lab payloads + `gc()` on disconnect |
| Shared hist cache | `session_capacity.R` `ynow_cached_hist_stock_trim` | One 180d frame per ticker per worker (not per session) |
| Defer Blue Chip snapshot | lazy `on_first_mount` for `bluechip` | No chain-rank CSV copy on every connect |
| Heavy-job single-flight | Lab eval / Clustering / HFV `run_bt` | Only one memory-heavy job on the worker at a time |
| Price-alert timer gate | `price_alert.R` | `invalidateLater` only while alerts are armed |
| Hosted sequential futures | `async_jobs.R` | No multisession RSS multiplier |
| Neon pool cap | `auth_db.R` | `maxSize ≤ 2` on hosted |
| shinyapps props | `configure_shinyapps_capacity.R` | `max_connections` matched to RAM floor; `timeout_conn=600` |

## What we already hardened in app code

1. **Shared disk caches** on hosted workers (`web_crawler.R` / `YNOW_CACHE_ROOT`) — avoid stampeding Yahoo/SEC.
2. **Hosted async = sequential** (`async_jobs.R`) — do not spawn `future` multisession children that multiply RSS under multi-tenant workers.
3. **Neon pool maxSize ≤ 2 on hosted** (`auth_db.R`) — many workers × large pools exhaust Neon Free.
4. **Capacity planner + apply script** — `scripts/ynow_capacity.R`, `scripts/configure_shinyapps_capacity.R`.
5. **`session_capacity.R`** — process locks + shared hist + cleanup helpers.

## Ops checklist

1. Upgrade shinyapps.io to **Professional**.
2. `Rscript scripts/configure_shinyapps_capacity.R` (applies size / workers / connections / timeouts).
3. **Dashboard manual step:** Application → Settings → add instances with `[+] Add instance` until you reach **10** (the shinyapps API cannot set instance count).
4. Confirm Advanced: workers, max connections, instance size `xxxlarge`, start count.
5. Smoke: open several independent browsers/devices (cookies pin one browser to one instance).
6. Watch Metrics for memory; if workers OOM, lower `max_connections` or raise instance size (already max on Pro).

## Local verification

```bash
cd app_25.0/tests && Rscript test_capacity_500.R
Rscript scripts/configure_shinyapps_capacity.R --dry-run
```
