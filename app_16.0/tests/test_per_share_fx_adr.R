#!/usr/bin/env Rscript
# TSM-like 5:1 ADR + TWD↔USD FX unit checks (no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

# Refuse silent FX=32
check("fx missing → NA", !is.finite(fx_factor("TWD", "USD", usd_twd = NA_real_)))
check("fx zero → NA", !is.finite(fx_factor("TWD", "USD", usd_twd = 0)))
check("fx live 32", abs(fx_factor("TWD", "USD", usd_twd = 32) - 1 / 32) < 1e-12)
check("same ccy → 1", identical(fx_factor("USD", "USD", usd_twd = NA_real_), 1))

# TSM 20-F: Equity_TWD / FX / ADR == NT$ per common × 5 / FX
equity_twd <- 250920e8  # ~NT$25,092bn in absolute units as in CSV rebuild
common <- 25.932e9
adr <- common / 5
fx <- 32
nt_per_common <- equity_twd / common
usd_per_adr_via_common <- nt_per_common * 5 / fx
usd_per_adr_via_adr <- (equity_twd / fx) / adr
check("TSM paths equal", abs(usd_per_adr_via_common - usd_per_adr_via_adr) < 1e-9)

px <- per_share_in_quote(
  equity_twd, adr,
  equity_ccy = "TWD", to_ccy = "USD", usd_twd = fx,
  share_method = "market_cap_per_price", statement_ccy = "TWD"
)
check("TSM ADR+FX", is.finite(px) && abs(px - usd_per_adr_via_adr) < 1e-6)
check("TSM ~151", abs(px - 151.19) < 0.5)

# Refuse: TWD equity / common labeled as USD/ADR (no ADR align)
bad <- per_share_in_quote(
  equity_twd, common,
  equity_ccy = "TWD", to_ccy = "USD", usd_twd = fx,
  share_method = "balance_sheet", statement_ccy = "TWD"
)
check("refuse common as ADR", !is.finite(bad))

# Refuse: missing FX even with ADR shares
nofx <- per_share_in_quote(
  equity_twd, adr,
  equity_ccy = "TWD", to_ccy = "USD", usd_twd = NA_real_,
  share_method = "market_cap_per_price", statement_ccy = "TWD"
)
check("refuse missing FX", !is.finite(nofx))

# Already-converted equity (session USD) + ADR shares
px2 <- per_share_in_quote(
  equity_twd / fx, adr,
  equity_ccy = "USD", to_ccy = "USD", usd_twd = fx,
  share_method = "market_cap_per_price", statement_ccy = "TWD"
)
check("scaled USD + ADR", is.finite(px2) && abs(px2 - px) < 1e-6)

# Wrong path that produced ~$1005: TWD/common labeled USD (6.4×)
wrong_label <- equity_twd / common  # ~967.6 "as USD"
check("wrong label ~968", abs(wrong_label - 967.6) < 1)
check("wrong/correct ≈ 6.4", abs(wrong_label / px - 32 / 5) < 0.05)

# scale_financial_df_money refuses when FX missing
df <- data.frame(Breakdown = "Cash", `2024` = "100", check.names = FALSE, stringsAsFactors = FALSE)
out <- scale_financial_df_money(df, "TWD", "USD", usd_twd = NA_real_)
check("scale refuse FX", isFALSE(attr(out, "money_scaled")))
check("scale keep TWD tag", identical(attr(out, "money_ccy"), "TWD"))

if (fail > 0L) {
  cat("\n", fail, " failure(s)\n", sep = "")
  quit(status = 1)
}
cat("\nAll per_share FX/ADR checks passed.\n")
