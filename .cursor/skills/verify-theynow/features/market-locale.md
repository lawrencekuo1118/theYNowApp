# Market and locale header

Header controls switch the active equity market (US / TW) and UI locale (en-US / zh-TW) without a full page reload.

## Sub-features

- `market-us` selects US market (`#ynow_mkt_btn_us`).
- `market-tw` selects TW market (`#ynow_mkt_btn_tw`).
- `locale-en` selects English (`#ynow_lang_btn_en`).
- `locale-zh` selects Taiwan Traditional Chinese (`#ynow_lang_btn_zh`).

## How to get to it (user POV)

- In the top header, choose the US or TW flag button (`aria-label` `US` / `TW`).
- Choose **EN** or **繁中** in the language stack (`#ynow_lang_stack`).

## Driving it with verify-theynow

Preconditions:

- Doctor passed; page loaded.
- Start from known defaults (typically US + EN active on first paint).

- **Market TW.** Click `#ynow_mkt_btn_tw`. Button gains class `active`; `#ynow_mkt_btn_us` loses `active`. Capture header screenshot.
- **Market US.** Click `#ynow_mkt_btn_us`. US is `active` again.
- **Locale zh-TW.** Click `#ynow_lang_btn_zh`. Button becomes `active`; visible menu/home strings switch to Taiwan Traditional Chinese (e.g. Home label via locale push). Assert no Simplified-only forms (`默认` / `数据` / `参数`).
- **Locale en-US.** Click `#ynow_lang_btn_en`. English labels restore; finance terms stay English.
- **Proof.** DOM dump of button `className` values plus a screenshot where the active market/locale is visible. Feature IDs `market-tw` / `locale-zh` as driven.

## Gotchas

- Market change can refresh Macro / Blue Chip lazy content — wait for settle before asserting page titles.
- Locale strings come from `ui_locale.R`; if a control stays English it may be a finance-term exception — do not fail solely on WACC/FCFF remaining English.
- Currency USD/TWD (`session_ccy_pick`) is separate from market US/TW — do not conflate them.
- Header auth links are not required for Guest market/locale proof.
