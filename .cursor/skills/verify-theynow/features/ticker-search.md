# Ticker search to Company

Ticker search lets a user type a US or TW stock code in the chrome field, run Search, and see company identity (name / industry text) for valuation tabs.

## Sub-features

- `search-fill` enters a ticker in `#sc`.
- `search-run` activates Search and waits for results.
- `search-company-chrome` shows corp name / search results text.
- `search-open-company` reaches the Company tab with that ticker context.

## How to get to it (user POV)

- With stock chrome visible (not on gated-only tabs that hide it), focus **Ticker / Stock Code** (`#sc`).
- Enter a symbol (e.g. `AAPL` on US market) and choose **Search** (`#search`).
- Open sidebar **Company** (`a[data-value="dashboard"]`) or the Home **Company** card.

## Driving it with verify-theynow

Preconditions:

- Doctor passed; market is US (`#ynow_mkt_btn_us` has class `active`) unless testing TW.
- Network egress to Yahoo Finance / yfinance is available (Cloud allowlist). If blocked, report Gotcha — do not fake corp name via internal setters.
- Prefer ticker `AAPL` on US for the seed proof.

- **Show chrome.** Navigate so `#sc` is in the DOM (Home or Company). Capture before.
- **Fill ticker.** Set `#sc` value to `AAPL` (input event so Shiny sees it: set value then dispatch `input` / `change`, or type via CDP Input).
- **Search.** Click `#search`. Wait until `#txt_corpname` or `#search_results` shows non-empty company-related text (Apple / AAPL). Capture after.
- **Company tab.** Click `a[data-value="dashboard"]` or Home Company card. Company surface remains keyed to the searched ticker.
- **Proof.** Before/after screenshots plus DOM dump where `corp` or `searchResults` contains the company identity. Feature ID `search-run`.

CDP note: `verify-cdp-shot.mjs` click-only helper may need a small evaluate fill for `#sc`; computerUse can type naturally. Prefer real typing over `Shiny.setInputValue` test hooks.

## Gotchas

- Four-digit TW codes may auto-switch market; assert `#ynow_mkt_btn_tw.active` when proving TW.
- Empty or invalid tickers leave chrome empty — that is a failed search proof, not a pass.
- Offline `tests/*.R` that stub crawlers are not UI search proof.
- Stock chrome is hidden on some tabs (About, settings); return to Home/Company first.
