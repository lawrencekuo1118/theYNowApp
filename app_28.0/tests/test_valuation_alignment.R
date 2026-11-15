#!/usr/bin/env Rscript
# Conditional FX / ADR alignment classifier (no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "ui_locale.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

ok <- function(...) {
  is.null(classify_per_share_alignment_failure(...))
}
code_of <- function(...) classify_per_share_alignment_failure(...)

equity_twd <- 250920e8
common <- 25.932e9
adr <- common / 5
fx <- 32

# --- 2330-like TWD/TWD ordinary: FX=NA, no ADR metadata ---
check(
  "2330 TWD/TWD ordinary OK",
  ok("TWD", "TWD", usd_twd = NA_real_, share_method = "balance_sheet",
     equity_ccy = "TWD", shares = common)
)
px_2330 <- per_share_in_quote(
  equity_twd, common,
  equity_ccy = "TWD", to_ccy = "TWD", usd_twd = NA_real_,
  share_method = "balance_sheet", statement_ccy = "TWD"
)
check("2330 per-share finite", is.finite(px_2330) && abs(px_2330 - equity_twd / common) < 1e-6)

# --- AAPL-like USD/USD ordinary with statement N/A ---
check(
  "AAPL N/A statement OK",
  ok("N/A", "USD", usd_twd = NA_real_, share_method = "balance_sheet",
     equity_ccy = NA, shares = 1.5e10)
)
check(
  "AAPL unknown statement OK",
  ok(NA, "USD", usd_twd = NA_real_, share_method = "balance_sheet")
)

# --- TSM-like TWD→USD + valid FX + ADR ---
check(
  "TSM TWD→USD + FX + ADR OK",
  ok("TWD", "USD", usd_twd = fx, share_method = "market_cap_per_price",
     equity_ccy = "TWD", shares = adr, adr_ratio = adr / common)
)
px_tsm <- per_share_in_quote(
  equity_twd, adr,
  equity_ccy = "TWD", to_ccy = "USD", usd_twd = fx,
  share_method = "market_cap_per_price", statement_ccy = "TWD"
)
check("TSM ~151 path", is.finite(px_tsm) && abs(px_tsm - 151.19) < 0.5)

# --- TSM-like + FX missing / invalid ---
check(
  "TSM FX missing",
  identical(
    code_of("TWD", "USD", usd_twd = NA_real_,
            share_method = "market_cap_per_price", equity_ccy = "TWD"),
    "REQUIRED_FX_RATE_MISSING"
  )
)
check(
  "TSM FX ≤0 invalid",
  identical(
    code_of("TWD", "USD", usd_twd = 0,
            share_method = "market_cap_per_price", equity_ccy = "TWD"),
    "REQUIRED_FX_RATE_INVALID"
  )
)
check(
  "TSM FX negative invalid",
  identical(
    code_of("TWD", "USD", usd_twd = -1,
            share_method = "market_cap_per_price", equity_ccy = "TWD"),
    "REQUIRED_FX_RATE_INVALID"
  )
)

# --- ADR needed but no ratio / invalid ratio ---
check(
  "TWD→USD no ADR ratio",
  identical(
    code_of("TWD", "USD", usd_twd = fx, share_method = "balance_sheet",
            equity_ccy = "TWD", shares = common),
    "APPLICABLE_ADR_RATIO_MISSING"
  )
)
check(
  "ADR ratio ≤0 invalid",
  identical(
    code_of("TWD", "USD", usd_twd = fx, share_method = "market_cap_per_price",
            equity_ccy = "TWD", adr_ratio = 0),
    "APPLICABLE_ADR_RATIO_INVALID"
  )
)
check(
  "ADR ratio Inf invalid",
  identical(
    code_of("TWD", "USD", usd_twd = fx, share_method = "market_cap_per_price",
            equity_ccy = "TWD", adr_ratio = Inf),
    "APPLICABLE_ADR_RATIO_INVALID"
  )
)

# --- Statement N/A vs conversion required ---
check(
  "conversion required + statement N/A",
  identical(
    code_of("N/A", "USD", usd_twd = fx, share_method = "market_cap_per_price"),
    "STATEMENT_CURRENCY_UNAVAILABLE"
  )
)
check(
  "conversion required + unknown statement",
  identical(
    code_of(NA, "USD", usd_twd = fx, share_method = "market_cap_per_price"),
    "STATEMENT_CURRENCY_UNAVAILABLE"
  )
)
check(
  "conversion NOT required + statement N/A",
  ok("N/A", "USD", usd_twd = NA_real_, share_method = "balance_sheet")
)

# --- Selected-method per-share gating ---
check(
  "DCF ignores unused NA BVPS/NAVPS",
  ok("USD", "USD", usd_twd = NA_real_, share_method = "balance_sheet",
     required_per_share = c(dcf = 12.5, bvps = NA, navps = NA), method = "dcf")
)
check(
  "DCF required non-finite → code",
  identical(
    code_of("USD", "USD", usd_twd = NA_real_, share_method = "balance_sheet",
            required_per_share = c(dcf = Inf, bvps = 10), method = "dcf"),
    "REQUIRED_PER_SHARE_VALUE_NON_FINITE"
  )
)
check(
  "RI required B0 NA → code",
  identical(
    code_of("USD", "USD", usd_twd = NA_real_, share_method = "balance_sheet",
            required_per_share = c(b0 = NA, dcf = 20), method = "ri"),
    "REQUIRED_PER_SHARE_VALUE_NON_FINITE"
  )
)
check(
  "RI ignores unused NA DCF",
  ok("USD", "USD", usd_twd = NA_real_, share_method = "balance_sheet",
     required_per_share = c(b0 = 8, dcf = NA), method = "ri")
)

# --- Existing refuse cases map onto new codes ---
check(
  "CNY ADR → REQUIRED_FX_RATE_MISSING",
  identical(
    code_of("CNY", "USD", usd_twd = 32, share_method = "market_cap_per_price",
            equity_ccy = "CNY"),
    "REQUIRED_FX_RATE_MISSING"
  )
)
check(
  "EUR ordinary vs USD → REQUIRED_FX_RATE_MISSING",
  identical(
    code_of("EUR", "USD", usd_twd = 32, share_method = "balance_sheet",
            equity_ccy = "EUR"),
    "REQUIRED_FX_RATE_MISSING"
  )
)
check(
  "common as ADR → APPLICABLE_ADR_RATIO_MISSING",
  identical(
    code_of("TWD", "USD", usd_twd = fx, share_method = "balance_sheet",
            equity_ccy = "TWD", shares = common),
    "APPLICABLE_ADR_RATIO_MISSING"
  )
)

# Same-currency must not depend on a junk USD/TWD print
check(
  "TWD/TWD ignores FX=0",
  ok("TWD", "TWD", usd_twd = 0, share_method = "balance_sheet", equity_ccy = "TWD")
)
check(
  "USD/USD ignores FX=NA",
  ok("USD", "USD", usd_twd = NA_real_, share_method = "balance_sheet")
)

# Toast-key mapping
check(
  "toast map STATEMENT_CURRENCY_UNAVAILABLE",
  identical(
    per_share_alignment_toast_key("STATEMENT_CURRENCY_UNAVAILABLE"),
    "notif_dcf_statement_ccy_unavailable"
  )
)
check(
  "toast map REQUIRED_FX_RATE_MISSING",
  identical(
    per_share_alignment_toast_key("REQUIRED_FX_RATE_MISSING"),
    "notif_dcf_fx_rate_missing"
  )
)
check(
  "toast map REQUIRED_FX_RATE_INVALID",
  identical(
    per_share_alignment_toast_key("REQUIRED_FX_RATE_INVALID"),
    "notif_dcf_fx_rate_invalid"
  )
)
check(
  "toast map APPLICABLE_ADR_RATIO_MISSING",
  identical(
    per_share_alignment_toast_key("APPLICABLE_ADR_RATIO_MISSING"),
    "notif_dcf_adr_ratio_missing"
  )
)
check(
  "toast map APPLICABLE_ADR_RATIO_INVALID",
  identical(
    per_share_alignment_toast_key("APPLICABLE_ADR_RATIO_INVALID"),
    "notif_dcf_adr_ratio_invalid"
  )
)
check(
  "toast map REQUIRED_PER_SHARE_VALUE_NON_FINITE",
  identical(
    per_share_alignment_toast_key("REQUIRED_PER_SHARE_VALUE_NON_FINITE"),
    "notif_dcf_per_share_non_finite"
  )
)
check("toast map NULL → NA", is.na(per_share_alignment_toast_key(NULL)))

new_keys <- c(
  "notif_dcf_statement_ccy_unavailable",
  "notif_dcf_fx_rate_missing",
  "notif_dcf_fx_rate_invalid",
  "notif_dcf_adr_ratio_missing",
  "notif_dcf_adr_ratio_invalid",
  "notif_dcf_per_share_non_finite"
)
for (k in new_keys) {
  en <- ui_str(k, "en")
  zh <- ui_str(k, "zh-TW")
  check(paste("en key", k), nzchar(en) && !identical(en, k))
  check(paste("zh-TW key", k), nzchar(zh) && !identical(zh, k) && !identical(zh, en))
}

# Server: generic toast no longer used; auto-calc stays silent
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
             collapse = "\n")
check(
  "server no generic notif_dcf_no_per_share",
  !grepl("notif_dcf_no_per_share", srv, fixed = TRUE)
)
check(
  "server uses classifier",
  grepl("classify_per_share_alignment_failure", srv, fixed = TRUE)
)
check(
  "server maps toast keys",
  grepl("per_share_alignment_toast_key", srv, fixed = TRUE)
)
check(
  "auto-calc still notify = FALSE",
  grepl(".execute_dcf_calc(notify = FALSE)", srv, fixed = TRUE)
)
check(
  "neg equity toast kept",
  grepl("notif_dcf_neg_equity", srv, fixed = TRUE)
)

if (fail > 0L) {
  cat("\n", fail, " failure(s)\n", sep = "")
  quit(status = 1)
}
cat("\nAll valuation alignment checks passed.\n")
