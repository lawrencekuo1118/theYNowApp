# Pre-tax Rd = Interest Expense / Interest-bearing Debt (WACC then applies Rd×(1−T))
# Reference case aligned with rich01.com WACC article numbers (interest 52, debt 1200).

.fail <- function(msg) stop(msg, call. = FALSE)
.check <- function(cond, msg) if (!isTRUE(cond)) .fail(msg)

interest <- 52
debt <- 1200
tax <- 0.35
rd_pct <- 100 * interest / debt
.check(abs(rd_pct - (52 / 12)) < 1e-9, "Rd % = interest/debt*100")
rd_after_tax <- (rd_pct / 100) * (1 - tax)
.check(abs(rd_after_tax - 0.0281666667) < 1e-8, "after-tax Rd in WACC")

# Clamp behavior
lo <- 0; hi <- 40
rd_hi <- 100 * 600 / 1000  # 60%
clamped <- max(lo, min(rd_hi, hi))
.check(identical(clamped, 40), "Rd clamp to ceiling")

cat("test_rd_estimate.R: OK\n")
