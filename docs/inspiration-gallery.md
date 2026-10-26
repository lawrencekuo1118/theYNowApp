# Inspiration Gallery（靈感藝廊）

Internal name only. The homepage shows **no gallery title**.

## Placement

Below the home option cards:

1. Disclaimer (“Research and education only — not investment advice…”)
2. Divider
3. One row: **Sign in · Register** on the left, then **About · Privacy**
4. Gallery under that row

## Display rules

- One centered image at a time
- Click → random other image (avoid immediate repeat when pool length > 1)
- Optional admin auto-rotation when `rotation_interval_sec` > 0
- Each picture already contains its own copyright notice. The homepage does not add another copyright overlay, caption, or corner mark.

## Responsive

| Viewport | Width | Aspect |
|---|---|---|
| ≥992px (3-col home grid) | Width of two home menu cards | Shared `aspect-ratio: 4 / 3` box; `object-fit: contain` |
| 721–991px (2-col) | Full home content width (= two cards) | Same box |
| ≤720px (1-col) | Fluid `width: 100%` | Same box; may cap max-height so the first screen stays usable |

Layout is CSS-only (home grid breakpoints). No resize observers on first paint.

## Performance

Critical: **must not slow first homepage paint.**

- Empty aspect shell in first HTML (reserved space, no image bytes)
- After `requestIdleCallback` / `window.load`, fetch tiny `inspiration/manifest.json` (plus optional admin override message)
- Load **one** image with `loading="lazy"`; never preload the pool
- Bundled slides are small WebP (max edge 960, ~15–90 KB each; ~560 KB for 18 defaults)

## Admin frequency semantics

Persisted in `data/inspiration_gallery.json` (env: `YNOW_INSPIRATION_GALLERY_PATH`). Guest-writable: never.

| Field | Meaning | Default |
|---|---|---|
| `show_probability` | On each homepage visit, probability the gallery mounts (`0` = never, `1` = always) | `1.0` |
| `rotation_interval_sec` | If `> 0`, auto-advance to a random other image on that interval; `0` = click-only | `0` |

Sys. Admin tab: add (upload) / remove images; edit the two frequency fields; en-US + zh-TW labels. Uploads land under `data/inspiration_uploads/` and are served via `addResourcePath`; processing resizes to WebP and does not paint an extra copyright line.

## Hard-to-translate / excluded sources

OCR of `media/inspiration-gallery/src/` (2026-10-25). These were **excluded** from the default bundled pool because they are live-app or third-party UI captures with baked Traditional Chinese chrome that cannot be cleanly re-exported as English-only art:

| Source file | Why hard |
|---|---|
| `*_L0_001.jpg` phone captures (several) | Full App chrome: 資產傳導根系、系統管理、登入 flows |
| `ee0ef6db-….png` | Homepage legal strip screenshot (基本面價值、免責聲明) |
| `d9966851-…`, `9aa58ad6-…`, `4f74d6e0-…` | FinmoConf / 法說會 browser captures |
| `7bbf74d7-…`, `d1abafbb-…`, `0a89ecdc-…`, `7d638a3f-…`, `35eb2473-…` | Product / OAuth / gate / GitHub UI screenshots |

Conceptual slides with Chinese that were **patched to English** (best-effort overlay) and shipped:

| Term / phrase (zh) | English used | Notes |
|---|---|---|
| 參考 DNA雙股螺旋模式 | Reference: DNA double helix model | Title strip |
| 實務操作模組 | Ops module | Kept “Reification: Practice = …” |
| 學習輔助模組 | Learning module | Kept “Participation: Activity = …” |
| 偏向異質 / 同質；隨機 / 特定 | HETEROGENEOUS / HOMOGENEOUS; RANDOM / SPECIFIC | Already bilingual on art; legend → “X carrier · Y knowledge” |
| 理想群眾 / 群眾 / 潛在顧客 / 社群 / 既有顧客 | IDEAL CROWD / CROWD / POTENTIAL CUSTOMERS / COMMUNITY / CUSTOMERS | English already on GIF; “COSTUMERS” typo left as in source numbers-only constraint |
| 原物料供應商、生產製造、品牌、顧客市場、通路加值商 | suppliers · manufacturing · brand · market | Title strip; dense diagram labels remain partially bilingual |
| 群眾智慧、內部研發人才 | Crowd wisdom; Internal R&D talent | Overlay |

Re-process: `python3 app_27.0/scripts/process_inspiration_gallery.py`

## Paths

- Bundled: `app_27.0/www/inspiration/*.webp` + `manifest.json`
- Durable admin catalog: `data/inspiration_gallery.json`
- Uploads: `data/inspiration_uploads/<id>.webp`
