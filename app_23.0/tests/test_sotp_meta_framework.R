#!/usr/bin/env Rscript
# SOTP Meta-Framework positioning: taxonomy, locale, sidebar category.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

root <- normalizePath("..", winslash = "/", mustWork = TRUE)
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat(sprintf("[PASS] %s\n", label))
  } else {
    cat(sprintf("[FAIL] %s\n", label))
    fail <<- fail + 1L
  }
}

src <- function(...) paste(readLines(file.path(root, ...), warn = FALSE), collapse = "\n")

ui <- src("ynow_ui.R")
loc <- src("ui_locale.R")
vg <- src("valuation_guard.R")
sotp <- src("sotp_module.R")

check(
  "sidebar has Meta-Framework category",
  grepl('ynow_menu_cat_meta', ui, fixed = TRUE) &&
    grepl("Appr. Meta-Framework", ui, fixed = TRUE)
)
check(
  "SOTP parent is meta (not relative)",
  grepl("sotp_calculator: 'ynow_menu_cat_meta'", ui, fixed = TRUE) &&
    !grepl("sotp_calculator: 'ynow_menu_cat_relative'", ui, fixed = TRUE)
)
check(
  "relative menu no longer nests SOTP",
  grepl(
    'ynow_menu_cat_relative[\\s\\S]*?ynow_menu_rel_ev[\\s\\S]*?ynow_menu_rel_earn[\\s\\S]*?ynow_menu_rel_rev[\\s\\S]*?ynow_menu_rel_book[\\s\\S]*?menuItem\\(\\s*\\n\\s*text = tags\\$span\\(id = "ynow_menu_cat_meta"',
    ui,
    perl = TRUE
  )
)
check(
  "Justified P/B nests under Asset-based",
  grepl(
    'ynow_menu_cat_asset[\\s\\S]*?ynow_menu_nav[\\s\\S]*?ynow_menu_pb[\\s\\S]*?menuItem\\(\\s*\\n\\s*text = tags\\$span\\(id = "ynow_menu_cat_income"',
    ui,
    perl = TRUE
  )
)

check("locale en menu_cat_meta", grepl('menu_cat_meta = "Appr. Meta-Framework"', loc, fixed = TRUE))
check("locale zh-TW menu_cat_meta", grepl('menu_cat_meta = "法｜綜合框架"', loc, fixed = TRUE))
check(
  "locale lead Meta-Framework (en)",
  grepl("Meta-Framework", loc, fixed = TRUE) &&
    grepl("conglomerate discount", loc, fixed = TRUE)
)
check(
  "locale lead Meta-Framework (zh-TW)",
  grepl("綜合型估值框架", loc, fixed = TRUE) &&
    grepl("控股折價", loc, fixed = TRUE)
)
check(
  "ms_card_sotp Meta-Framework formula",
  grepl("Meta-Framework: Σ\\(segment EV\\)", loc, fixed = TRUE) ||
    grepl("綜合框架：Σ(部門 EV)", loc, fixed = TRUE)
)

check(
  "lens meta_framework_sotp",
  grepl('sotp = "meta_framework_sotp"', vg, fixed = TRUE)
)
check(
  ".vg_is_meta_framework helper",
  grepl("\\.vg_is_meta_framework\\s*<-\\s*function", vg, perl = TRUE)
)
rel_fn <- regmatches(
  vg,
  regexpr(
    "\\.vg_is_relative_model\\s*<-\\s*function\\(model\\)\\s*\\{[^}]+\\}",
    vg,
    perl = TRUE
  )
)
check(
  ".vg_is_relative_model is Multiples-only",
  length(rel_fn) == 1L &&
    grepl('"multiples"', rel_fn, fixed = TRUE) &&
    !grepl('"sotp"', rel_fn, fixed = TRUE)
)

check(
  "sotp module lead Meta-Framework",
  grepl("Meta-Framework", sotp, fixed = TRUE) &&
    grepl("conglomerate discount", sotp, fixed = TRUE)
)
check(
  "methodology positions SOTP as Meta-Framework",
  grepl("SOTP \\(Meta-Framework\\)", ui, fixed = TRUE) ||
    grepl("Meta-Framework that can mix", ui, fixed = TRUE)
)

# Runtime helpers (no statement fixtures required)
source(file.path(root, "valuation_guard.R"), local = TRUE)
`%||%` <- function(a, b) if (!is.null(a)) a else b

check("lens runtime", identical(.vg_lens("sotp"), "meta_framework_sotp"))
check(
  "meta helper runtime",
  isTRUE(.vg_is_meta_framework("sotp")) && !isTRUE(.vg_is_relative_model("sotp"))
)
check("multiples still relative", isTRUE(.vg_is_relative_model("multiples")))
check(
  "hold_note copy Meta-Framework",
  grepl("Meta-Framework", vg, fixed = TRUE) &&
    grepl("綜合型估值框架", vg, fixed = TRUE)
)

if (fail > 0L) {
  cat(sprintf("\n%d check(s) failed.\n", fail))
  quit(status = 1L)
}
cat("\nAll SOTP Meta-Framework checks passed.\n")
