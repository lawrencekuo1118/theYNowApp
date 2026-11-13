#!/usr/bin/env Rscript
# Offline-first Macro last-good cache (disk + client seed helpers).
# Run: cd app_27.0 && Rscript tests/test_macro_last_good.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

root <- if (file.exists("macro_last_good.R")) {
  normalizePath(".")
} else if (file.exists("../macro_last_good.R")) {
  normalizePath("..")
} else if (dir.exists("app_27.0") && file.exists("app_27.0/macro_last_good.R")) {
  normalizePath("app_27.0")
} else {
  stop("Cannot locate app_27.0/macro_last_good.R")
}
setwd(root)
options(warn = 1)

pass <- 0L
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("PASS:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
  }
}

# Isolate durable files under tempdir for this process.
Sys.setenv(YNOW_SLOW_CACHE_DIR = file.path(tempdir(), paste0("ynow_lg_", as.integer(Sys.time()))))

source("macro_last_good.R", local = TRUE, encoding = "UTF-8")

check("version constant", identical(as.integer(MACRO_LAST_GOOD_VERSION), 1L))
check("max age is multi-day", is.finite(MACRO_LAST_GOOD_MAX_AGE_SEC) && MACRO_LAST_GOOD_MAX_AGE_SEC >= 86400)

empty <- macro_last_good_empty("US")
check("empty has mode US", identical(empty$mode, "US"))
check("empty quotes not usable", !macro_last_good_quotes_usable(empty$quotes))

quotes <- list(
  list(symbol = "^DJI", label = "Dow", last = 42000.5, chg_pct = 0.42),
  list(symbol = "^GSPC", label = "S&P", last = 5200.1, chg_pct = -0.11)
)
rf <- list(rf_pct = 4.25, source = "live", label = "US 10Y", symbol = "^TNX")
htcdi <- list(composite = 51.2, availability = "ready")

check("quotes usable", macro_last_good_quotes_usable(quotes))
check("rf usable", macro_last_good_rf_usable(rf))
check("htcdi usable", macro_last_good_htcdi_usable(htcdi))
check("blank quotes helper", isTRUE(macro_quotes_all_blank(list(
  list(symbol = "^DJI", last = NA_real_)
))))

merged <- macro_last_good_merge("US", quotes = quotes, rf = rf, htcdi = htcdi)
check("merge returns snapshot", is.list(merged) && identical(merged$mode, "US"))
check("merge wrote disk", file.exists(macro_last_good_path("US")))

back <- macro_last_good_read("US")
check("read round-trip quotes", {
  is.list(back) &&
    length(back$quotes) >= 2L &&
    isTRUE(abs(as.numeric(back$quotes[[1]]$last) - 42000.5) < 1e-6)
})
check("read round-trip rf", {
  is.list(back$rf) && isTRUE(abs(as.numeric(back$rf$rf_pct) - 4.25) < 1e-6)
})
check("read round-trip htcdi", {
  is.list(back$htcdi) && isTRUE(abs(as.numeric(back$htcdi$composite) - 51.2) < 1e-6)
})

# Partial merge keeps prior RF when only quotes update.
merged2 <- macro_last_good_merge(
  "US",
  quotes = list(list(symbol = "^DJI", label = "Dow", last = 42100, chg_pct = 0.1))
)
check("partial merge keeps rf", {
  is.list(merged2$rf) && isTRUE(abs(as.numeric(merged2$rf$rf_pct) - 4.25) < 1e-6)
})
check("partial merge updates quote", {
  dji <- Filter(function(r) identical(r$symbol, "^DJI"), merged2$quotes)[[1]]
  isTRUE(abs(as.numeric(dji$last) - 42100) < 1e-6)
})

# Client seed validation
client_json <- jsonlite::toJSON(back, auto_unbox = TRUE, null = "null")
from_client <- macro_last_good_from_client(as.character(client_json))
check("client JSON seed", is.list(from_client) && identical(from_client$mode, "US"))

bad <- macro_last_good_from_client(list(v = 1, mode = "US", captured_at = "not-a-date"))
check("rejects undated empty client", is.null(bad))

# TW path isolation
tw <- macro_last_good_merge(
  "TW",
  quotes = list(list(symbol = "^TWII", label = "TAIEX", last = 22000, chg_pct = 0.2)),
  rf = list(rf_pct = 1.6, source = "live", label = "TW", symbol = "")
)
check("TW write separate file", {
  file.exists(macro_last_good_path("TW")) &&
    !identical(macro_last_good_path("TW"), macro_last_good_path("US"))
})
check("TW read", {
  hit <- macro_last_good_read("TW")
  is.list(hit) && identical(hit$mode, "TW") &&
    isTRUE(abs(as.numeric(hit$rf$rf_pct) - 1.6) < 1e-6)
})
invisible(tw)

# Module + UI wiring (source text checks; avoids full Shiny boot).
mod <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
ui_js <- paste(readLines("www/ynow_app.js", warn = FALSE), collapse = "\n")
css <- paste(readLines("www/ynow_widgets.css", warn = FALSE), collapse = "\n")
loc <- paste(readLines("ui_locale.R", warn = FALSE), collapse = "\n")
glob <- paste(readLines("global.R", warn = FALSE), collapse = "\n")

check("global sources macro_last_good", grepl("macro_last_good.R", glob, fixed = TRUE))
check("UI has stale banner", grepl("ynow_macro_stale_banner", mod, fixed = TRUE))
check("banner Lite-free (no ynow-full-only on banner class)", {
  grepl("ynow-macro-stale-banner", mod, fixed = TRUE) &&
    !grepl("ynow-macro-stale-banner[^\n]*ynow-full-only", mod)
})
check("server seeds quotes from last-good", grepl(".macro_lg_seed_quotes", mod, fixed = TRUE))
check("server listens for client seed", grepl("last_good_client", mod, fixed = TRUE))
check("server pushes ynowMacroLastGood", grepl("ynowMacroLastGood", mod, fixed = TRUE))
check("server pushes ynowMacroStale", grepl("ynowMacroStale", mod, fixed = TRUE))
check("live_hits tracks network success", grepl("qstate$live_hits", mod, fixed = TRUE))
check("HTCDI seed from last-good", {
  grepl("macro_last_good_htcdi_usable", mod, fixed = TRUE) &&
    grepl("from_cache", mod, fixed = TRUE)
})
check("client localStorage key", grepl("ynow_macro_last_good_v1", ui_js, fixed = TRUE))
check("client hydrates skeletons", grepl("hydrateSkeletons", ui_js, fixed = TRUE))
check("client offline listener", grepl("window.addEventListener('offline'", ui_js, fixed = TRUE))
check("CSS stale banner", grepl("ynow-macro-stale-banner", css, fixed = TRUE))
check("i18n en macro_offline_stale", grepl("macro_offline_stale = \"Offline", loc, fixed = TRUE))
check("i18n zh-TW macro_offline_stale", grepl("離線／訊號不佳", loc, fixed = TRUE))
check("i18n en macro_offline_stale_at", grepl("macro_offline_stale_at = \"Offline", loc, fixed = TRUE))
check("i18n zh-TW macro_offline_stale_at", grepl("macro_offline_stale_at = \"離線", loc, fixed = TRUE))

cat("\nPASS", pass, "FAIL", fail, "\n")
if (fail > 0L) quit(status = 1L)
cat("All macro last-good checks passed.\n")
