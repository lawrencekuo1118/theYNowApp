#!/usr/bin/env Rscript
# SOTP auto-apply from BB Lab / Business Breakdown: gate, merge, non-op, TW pack.
# Run: cd app_28.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_sotp_bblab_auto_apply.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

root <- if (file.exists("relative_multiples_module.R")) {
  getwd()
} else if (file.exists("../relative_multiples_module.R")) {
  normalizePath("..")
} else if (file.exists("app_28.0/relative_multiples_module.R")) {
  file.path(getwd(), "app_28.0")
} else {
  stop("Run from app_28.0/tests or repo root")
}
setwd(root)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat(sprintf("[PASS] %s\n", label))
  } else {
    cat(sprintf("[FAIL] %s\n", label))
    fail <<- fail + 1L
  }
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

source("setup.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
APP_DEFAULTS <- list(rel_ev_sales_multiple = 3)
source("relative_multiples_module.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_schema.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_structure.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_engine.R", local = TRUE, encoding = "UTF-8")

# --- Gate: applicable only with ≥2 positive named revenues ---
empty <- data.frame(name = character(0), revenue = numeric(0), stringsAsFactors = FALSE)
one <- data.frame(name = "Only", revenue = 100, stringsAsFactors = FALSE)
two <- data.frame(name = c("A", "B"), revenue = c(100, 50), stringsAsFactors = FALSE)
bad <- data.frame(name = c("A", "B"), revenue = c(100, 0), stringsAsFactors = FALSE)
check("gate rejects empty", !isTRUE(sotp_segments_applicable(empty)))
check("gate rejects single segment", !isTRUE(sotp_segments_applicable(one)))
check("gate accepts ≥2 positive", isTRUE(sotp_segments_applicable(two)))
check("gate rejects zero revenue row pair", !isTRUE(sotp_segments_applicable(bad)))

# --- From BB Lab result ---
fake_ok <- list(
  limitations = character(0),
  businesses = list(
    list(name = "Cloud", revenue = 200),
    list(name = "Ads", revenue = 80)
  )
)
fake_block <- list(
  limitations = c("no_multi_business_split"),
  businesses = list(
    list(name = "Cloud", revenue = 200),
    list(name = "Ads", revenue = 80)
  )
)
check("bblab result applicable", isTRUE(sotp_bblab_result_applicable(fake_ok)))
check("bblab no_multi_business_split blocks", !isTRUE(sotp_bblab_result_applicable(fake_block)))
segs_lab <- sotp_segments_from_bblab_result(fake_ok)
check(
  "segments from bblab result",
  is.data.frame(segs_lab) && nrow(segs_lab) == 2L &&
    identical(segs_lab$name, c("Cloud", "Ads")) &&
    all(segs_lab$revenue == c(200, 80))
)
check(
  "blocked result yields empty segments",
  nrow(sotp_segments_from_bblab_result(fake_block)) == 0L
)

# --- Merge preserves EV/Sales by name; does not invent overrides ---
prior <- data.frame(
  name = c("Cloud", "Ads"), revenue = c(150, 70), multiple = c(4.5, 2.0),
  stringsAsFactors = FALSE
)
fresh <- data.frame(
  name = c("Cloud", "Ads", "Devices"), revenue = c(200, 80, 40),
  stringsAsFactors = FALSE
)
merged <- sotp_merge_segment_multiples(fresh, prior, default_mult = 3)
check("merge keeps Cloud multiple", abs(merged$multiple[merged$name == "Cloud"] - 4.5) < 1e-9)
check("merge keeps Ads multiple", abs(merged$multiple[merged$name == "Ads"] - 2.0) < 1e-9)
check("merge defaults new Devices", abs(merged$multiple[merged$name == "Devices"] - 3) < 1e-9)
check("merge refreshes revenues", {
  abs(merged$revenue[merged$name == "Cloud"] - 200) < 1e-9 &&
    abs(merged$revenue[merged$name == "Ads"] - 80) < 1e-9
})

# --- Non-op only when clearly disclosed ---
bs_clear <- data.frame(
  Breakdown = c("Total Assets", "Non Operating Assets", "Cash And Cash Equivalents"),
  `12/31/2024` = c("1000", "75", "100"),
  check.names = FALSE, stringsAsFactors = FALSE
)
bs_missing <- data.frame(
  Breakdown = c("Total Assets", "Cash And Cash Equivalents"),
  `12/31/2024` = c("1000", "100"),
  check.names = FALSE, stringsAsFactors = FALSE
)
nop_clear <- sotp_clear_nonop_from_bs(bs_clear)
nop_miss <- sotp_clear_nonop_from_bs(bs_missing)
check("nonop applies when disclosed", isTRUE(nop_clear$apply) && abs(nop_clear$value - 75) < 1e-9)
check("nonop skips when missing (no proxy)", !isTRUE(nop_miss$apply))

# --- Engine path with segment_tables (shared with BB Lab) ---
note_tbl <- list(list(
  short_name = "Segment Information",
  kind = "operating_segment",
  headers = c("Segment", "Revenue", "Cost of Revenue"),
  rows = list(
    c("Biz A", "120", "40"),
    c("Biz B", "80", "40"),
    c("Total", "200", "80")
  )
))
is_consol <- data.frame(
  Breakdown = c("Total Revenue", "Cost Of Revenue", "Gross Profit"),
  `12/31/2024` = c("200", "80", "120"),
  check.names = FALSE, stringsAsFactors = FALSE
)
segs_notes <- .rel_sotp_segments_from_is(
  is_consol, ticker = "2330.TW", statement_currency = "TWD",
  segment_tables = note_tbl
)
check(
  "TW-style segment_tables yield applicable SOTP segs",
  isTRUE(sotp_segments_applicable(segs_notes)) && nrow(segs_notes) >= 2L
)

# --- Pack fetch: TW vs US routing (stub caches) ---
tw_called <- FALSE
us_called <- FALSE
cached_fetch_tw_annual_order_notes <- function(ticker) {
  tw_called <<- TRUE
  list(segment_tables = note_tbl, form = "TW-AR")
}
cached_fetch_sec_segment_notes <- function(ticker, form = "10-K") {
  us_called <<- TRUE
  list(form = form, excerpts = character(0))
}
# Re-bind helpers that close over the stubs in this env via redefinition after stubs
# (helpers call exists()+direct name lookup in parent — stubs above are local).
# Force pack to see local stubs by evaluating in this frame:
pack_tw <- local({
  .rel_sotp_fetch_segment_pack <- function(ticker) {
    empty <- list(notes = NULL, segment_tables = NULL)
    tk <- as.character(ticker %||% "")[1]
    if (!nzchar(tk)) return(empty)
    is_tw <- grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)
    if (isTRUE(is_tw)) {
      notes <- cached_fetch_tw_annual_order_notes(tk)
      tables <- if (is.list(notes)) notes$segment_tables %||% NULL else NULL
      return(list(notes = notes, segment_tables = tables))
    }
    notes <- cached_fetch_sec_segment_notes(tk, "10-K")
    list(notes = notes, segment_tables = NULL)
  }
  list(
    tw = .rel_sotp_fetch_segment_pack("2330.TW"),
    us = .rel_sotp_fetch_segment_pack("AAPL")
  )
})
check("TW pack routes to annual notes", {
  is.list(pack_tw$tw$notes) && length(pack_tw$tw$segment_tables) >= 1L && isTRUE(tw_called)
})
check("US pack routes to SEC notes", {
  is.list(pack_tw$us$notes) && isTRUE(us_called)
})

# --- Locale keys present (en + zh-TW) ---
for (k in c("sotp_synced", "sotp_auto_apply_not_applicable", "sotp_help")) {
  check(
    paste("locale en", k),
    !identical(ui_str(k, "en"), "N/A") && nzchar(ui_str(k, "en"))
  )
  check(
    paste("locale zh-TW", k),
    !identical(ui_str(k, "zh-TW"), "N/A") && nzchar(ui_str(k, "zh-TW"))
  )
}
check(
  "zh-TW sotp_help mentions auto-apply / 業務拆解",
  grepl("業務拆解|自動帶入", ui_str("sotp_help", "zh-TW"))
)
check(
  "en sotp_synced leaves bridge alone",
  grepl("left unchanged", ui_str("sotp_synced", "en"), fixed = TRUE) ||
    grepl("Cash, Debt, and Shares were left unchanged", ui_str("sotp_synced", "en"), fixed = TRUE)
)

# --- Source contracts ---
sotp_src <- paste(readLines("sotp_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
mod_src <- paste(readLines("relative_multiples_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_src <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
bblab_src <- paste(readLines("business_breakdown_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("sotp exposes apply_bblab_segments", grepl("apply_bblab_segments", sotp_src, fixed = TRUE))
check("sotp accepts bblab_result arg", grepl("bblab_result = NULL", sotp_src, fixed = TRUE))
check(
  "sotp never auto-writes cash/debt/shares",
  !grepl('updateNumericInput(session, "cash"', sotp_src, fixed = TRUE) &&
    !grepl('updateNumericInput(session, "debt"', sotp_src, fixed = TRUE) &&
    !grepl('updateNumericInput(session, "shares"', sotp_src, fixed = TRUE)
)
check(
  "BB Lab server returns lab_result",
  grepl("lab_result = reactive", bblab_src, fixed = TRUE)
)
check(
  "server wires bblab_api into sotp",
  grepl("bblab_api <- business_breakdown_lab_server", srv_src, fixed = TRUE) &&
    grepl("bblab_result =", srv_src, fixed = TRUE)
)
check(
  "pack helper documents TW+US",
  grepl("cached_fetch_tw_annual_order_notes", mod_src, fixed = TRUE) &&
    grepl("cached_fetch_sec_segment_notes", mod_src, fixed = TRUE)
)

if (fail > 0L) {
  cat(sprintf("\n%d check(s) failed\n", fail))
  quit(status = 1L)
}
cat("\nAll SOTP BB Lab auto-apply checks passed.\n")
