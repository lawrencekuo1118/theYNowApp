#!/usr/bin/env Rscript
# Corporate AML tab registration + Lite-open + locale keys (no network).
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_corp_aml_tab_ui.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
prefs <- paste(readLines(file.path(app_dir, "user_prefs.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
dec <- paste(readLines(file.path(app_dir, "investment_decision_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
about <- paste(readLines(file.path(app_dir, "ynow_about_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check('menuItem tabName corp_aml', grepl('tabName = "corp_aml"', ui, fixed = TRUE))
check("menu span id", grepl("ynow_menu_corp_aml", ui, fixed = TRUE))
check("lazy host", grepl("ynow_lazy_host_corp_aml", ui, fixed = TRUE))
check("page builder", grepl(".ynow_page_ui_corp_aml", ui, fixed = TRUE))
check("applyUiLocale menu key", grepl("corp_aml: s.menu_corp_aml", ui, fixed = TRUE))
check("lazy builder registered", grepl("corp_aml = function\\(\\) \\.ynow_page_ui_corp_aml\\(\\)", srv))
check("server snapshot", grepl("corp_aml_snapshot", srv, fixed = TRUE))
check("body output", grepl("output\\$corp_aml_body_ui", srv))
check("decision_server aml_pack", grepl("aml_pack = corp_aml_snapshot", srv, fixed = TRUE))
check("decision_server accepts aml_pack", grepl("aml_pack = reactive\\(NULL\\)", dec))
check("veto chip class", grepl("ynow-aml-veto", dec, fixed = TRUE))

check("locale en menu", grepl('menu_corp_aml = "Corporate AML Signals"', loc, fixed = TRUE))
check("locale zh menu", grepl('menu_corp_aml = "法人洗錢跡象"', loc, fixed = TRUE))
check("locale en disclaimer", grepl("aml_disclaimer = paste0", loc, fixed = TRUE))
check("locale zh veto", grepl("aml_veto_title = \"法人洗錢跡象代理：警示\"", loc, fixed = TRUE))

check("pinnable includes corp_aml", grepl('"corp_aml"', prefs, fixed = TRUE))
check("default order includes corp_aml", grepl('"corp_aml"', prefs, fixed = TRUE))

# Must stay Lite-open: not in guest-lock / lite-omit lists
guest_block <- sub(
  "(?s).*ynow_sidebar_guest_lock_tabs <- function\\(\\) \\{(.*?)\\n\\}",
  "\\1",
  auth,
  perl = TRUE
)
check("not guest-locked", !grepl("corp_aml", guest_block, fixed = TRUE))
omit_block <- sub(
  "(?s).*ynow_sidebar_lite_omit_tabs <- function\\(\\) \\{(.*?)\\n\\}",
  "\\1",
  auth,
  perl = TRUE
)
# omit calls guest_lock + extras; still must not name corp_aml explicitly
check("not lite-omitted by name", !grepl("corp_aml", omit_block, fixed = TRUE))

check("about en mentions Corporate AML", grepl("Corporate AML Signals", about, fixed = TRUE))
check("about zh mentions 法人洗錢跡象", grepl("法人洗錢跡象", about, fixed = TRUE))
check("no bank fileInput for AML", !grepl("fileInput\\([^)]*aml", ui, ignore.case = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
