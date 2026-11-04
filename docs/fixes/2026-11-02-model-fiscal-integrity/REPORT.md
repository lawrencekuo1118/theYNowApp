# Model fiscal-integrity and Lite valuation report

Report date: 2026-11-04 (Asia/Taipei)

## Decision and scope

The D&A-only fallback was withdrawn. D&A is now one field in the same fiscal-period source registry used by every valuation formula. No model-specific cache repair is allowed.

The verification scope covers US and Taiwan market modes, en-US and zh-TW, Lite and Full behavior, authenticated and guest sessions through the same server calculation path, and mobile and desktop layouts. Authentication controls access and saved state; it does not select a different financial-parameter source.

## Enforced data path

1. The registry identifies an exact issuer, fiscal endpoint, reporting currency, source table, source field, and lineage for every financial fact.
2. Every formula variant requests all of its required financial facts as one set.
3. If any fact is absent, the recovery operation checks the slow statement cache and then the original provider once for the complete gap set.
4. Only the requested fiscal endpoint is accepted. TTM, interim/YTD, another issuer, another currency, and another fiscal year are rejected.
5. A formula runs only when its financial snapshot is complete and the values bound to the model inputs match that snapshot.
6. Lite applies disclosed forecast assumptions where allowed. It never fabricates a missing reported financial fact.
7. Full accepts a user-entered financial fact only after a real browser edit, and scopes it to the current issuer and fiscal endpoint. Changing the issuer or fiscal year invalidates it.
8. Remaining gaps are listed by formula variant. The affected formula produces no valuation or chart point.

## Formula-to-source mapping

| Formula variant | Required reported financial facts | Formula use |
|---|---|---|
| DCF / FCFF | Revenue, EBIT and tax for NOPAT, D&A, CapEx, ΔNWC, cash, debt, common shares, other claims | `FCFF = NOPAT + D&A − CapEx − ΔNWC`; enterprise-to-equity bridge uses the same fiscal endpoint |
| DCF / FCFE | Revenue, NOPAT, D&A, CapEx, ΔNWC, interest, tax, debt, common shares | FCFE claim path; no FCFF cash/debt bridge substitution |
| DDM / Gordon | Annual DPS | D0 from reported annual DPS or actual ex-date payments allocated between adjacent fiscal endpoints |
| DDM / SPM | Annual DPS and EPS | Payout and earnings use the same fiscal endpoint |
| RI | Common book value and common shares | B0 is common book value per verified quote-class share |
| P/B / BVPS | Common book value and common shares | BVPS |
| P/B / TBVPS | Common book value, goodwill, intangibles, common shares | Tangible book value per share |
| P/B / NAVPS and NAV | Common book value and common shares; investments are additionally required when a holding-company discount is nonzero | NAVPS; no missing investment balance is treated as zero when a discount is applied |
| P/E and Forward P/E | Fiscal EPS | A TTM Summary EPS cannot replace a selected annual EPS |
| P/S | Revenue and common shares | Equity-value multiple |
| EV/FCF | CFO, interest, tax, CapEx, cash, debt, common shares, other claims | Unlevered trailing FCF and EV-to-equity bridge |
| EV/EBIT | EBIT plus EV bridge facts | Enterprise multiple |
| EV/EBITDA | Reported EBITDA, or same-period EBIT plus D&A, plus EV bridge facts | Enterprise multiple |
| EV/Sales | Revenue plus EV bridge facts | Enterprise multiple |
| EV/ARR | Explicit ARR plus EV bridge facts | ARR is never inferred from revenue |
| SOTP | At least two positive reported segment revenues plus EV bridge facts | Unverified or single-segment data does not produce SOTP |

Market price, Rf, FX, beta, and valuation multiples are valuation-date observations or explicit assumptions. They remain typed separately from fiscal financial facts.

## Verification evidence

The offline integrity suite removes every required parameter, one at a time, from every formula variant and from two fiscal years. All 120 omission cases blocked valuation. It also verified batched recovery, partial-repair blocking, complete missing-item reporting, strict issuer/currency/annual-period matching, TTM isolation, fiscal dividend allocation, and issuer-and-year-scoped Full manual entries.

Parameter-source and accounting tests passed 672 market/mode source contexts and 3,136 accounting-semantic contexts. Phase 1 independently verified 43 valuation formulas and boundary conditions.

A fresh original-source audit is recorded in [live-source-audit.jsonl](evidence/live-source-audit.jsonl). GOOG resolved the 2025 D&A value at USD 21.136 billion and completed 15 of 18 formula variants; TBVPS, EV/ARR, and SOTP remained explicitly unavailable. In the strict reported-fact audit, 2330.TW completed 11 variants and retained unresolved market-value other claims, ARR, and segments. Lite may use the disclosed same-year gross-equity-minus-common-equity book proxy for other claims and labels it as an assumption; Full still requires the market-value input. Yahoo returned no annual statement endpoint for 7772.TWO; the app's TPEx search fallback remains responsible for any verified partial IS/BS facts, while unavailable cash-flow formulas stay blocked.

The browser matrix exercises seven displayed model roles across US/Taiwan, en-US/zh-TW, mobile/desktop: 56 combinations. All 56 passed, with zero JavaScript errors and zero Shiny output errors. Each case requires a connected Shiny session, a completed source echo gate, finite Plotly values, a rendered SVG, and a chart within the viewport. Raw results are in [browser-matrix.jsonl](browser-matrix.jsonl), with a compact [matrix summary](evidence/browser-matrix-summary.json).

## Known availability behavior

Absence is a valid outcome. A company without explicit ARR will show EV/ARR as unavailable while P/E or another complete formula may still calculate. A company without verified multi-segment revenue will not receive SOTP. Cross-currency/ADR per-share formulas remain unavailable until a verified share-class conversion exists; current market cap divided by price is not used to manufacture fiscal-year shares.

Lite continues to calculate with disclosed forecast defaults when reported facts are complete. Full shows field guidance and accepts an exact-issuer, exact-fiscal-period manual value. Neither edition produces a partial valuation when a required reported fact is unresolved.
