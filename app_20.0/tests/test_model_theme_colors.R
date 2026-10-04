# Model tab themes must match Model Selector card accents.
# Run: cd app_20.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_model_theme_colors.R

root <- if (file.exists("ynow_ui.R")) {
  getwd()
} else if (file.exists("app_20.0/ynow_ui.R")) {
  file.path(getwd(), "app_20.0")
} else {
  stop("Cannot find app_20.0 root")
}
setwd(root)

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat("OK:", label, "\n")
}

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_src <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# CSS tokens = Model Selector card hexes
expect_vars <- c(
  "--ynow-model-nav: #d81b60",
  "--ynow-model-dcf: #00a65a",
  "--ynow-model-ddm: #f39c12",
  "--ynow-model-ri: #605ca8",
  "--ynow-model-pb: #3c8dbc",
  "--ynow-model-multiples: #17a2b8",
  "--ynow-model-sotp: #343a40"
)
for (v in expect_vars) {
  check(paste("CSS token", v), grepl(v, ui_src, fixed = TRUE))
}

# Theme body classes for all seven models
for (k in c("nav", "dcf", "ddm", "ri", "pb", "multiples", "sotp")) {
  check(
    paste("body theme class", k),
    grepl(paste0("body.ynow-theme-", k), ui_src, fixed = TRUE)
  )
  check(
    paste("card accent class", k),
    grepl(paste0(".ynow-model-card--", k), ui_src, fixed = TRUE)
  )
}

# JS map: Multiples/SOTP must not fall back to pb
check(
  "JS maps Multiples → multiples",
  grepl("rel_multiples_calculator: 'multiples'", ui_src, fixed = TRUE)
)
check(
  "JS maps SOTP → sotp",
  grepl("sotp_calculator: 'sotp'", ui_src, fixed = TRUE)
)
check(
  "JS theme toggle includes multiples/sotp",
  grepl("['nav', 'dcf', 'ddm', 'ri', 'pb', 'multiples', 'sotp']", ui_src, fixed = TRUE) ||
    grepl('["nav", "dcf", "ddm", "ri", "pb", "multiples", "sotp"]', ui_src, fixed = TRUE) ||
    grepl("['nav', 'dcf', 'ddm', 'ri', 'pb', 'multiples', 'sotp']", ui_src, fixed = TRUE)
)

# valueBox / small-box follow model accent on themed pages
check(
  "small-box uses model accent",
  grepl("body.ynow-theme-model .content-wrapper .small-box", ui_src, fixed = TRUE) &&
    grepl("background-color: var(--ynow-model-accent)", ui_src, fixed = TRUE)
)

# Cards use CSS accent classes (single source of truth with page themes)
check(
  "make_card applies ynow-model-card--{key}",
  grepl('paste0("ynow-model-card--", key)', srv_src, fixed = TRUE)
)
check(
  "card icon uses shared class",
  grepl('class = "ynow-model-card__icon"', srv_src, fixed = TRUE)
)

# Card call sites keep the seven hexes documented as data-model-color args
check("card NAV hex", grepl('"NAV", "nav", "sitemap", "#d81b60"', srv_src, fixed = TRUE))
check("card DCF hex", grepl('"DCF", "dcf", "calculator", "#00a65a"', srv_src, fixed = TRUE))
check("card DDM hex", grepl('"DDM", "ddm", "hand-holding-usd", "#f39c12"', srv_src, fixed = TRUE))
check("card RI hex", grepl('"RI", "ri", "gem", "#605ca8"', srv_src, fixed = TRUE))
check("card P/B hex", grepl('"P/B", "pb", "landmark", "#3c8dbc"', srv_src, fixed = TRUE))
check("card Multiples hex", grepl('"Multiples", "multiples", "percentage", "#17a2b8"', srv_src, fixed = TRUE))
check("card SOTP hex", grepl('"SOTP", "sotp", "puzzle-piece", "#343a40"', srv_src, fixed = TRUE))

cat("PASS test_model_theme_colors\n")
