#!/usr/bin/env Rscript
# Corporate AML folded into YNOW: no sidebar tab; marks + snapshot remain.
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
aml <- paste(readLines(file.path(app_dir, "corp_aml_signals.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Dedicated sidebar tab removed (Lite + Full)
check("no menuItem corp_aml", !grepl('tabName = "corp_aml"', ui, fixed = TRUE))
check("no menu span corp_aml", !grepl("ynow_menu_corp_aml", ui, fixed = TRUE))
check("no lazy host corp_aml", !grepl("ynow_lazy_host_corp_aml", ui, fixed = TRUE))
check("no page builder corp_aml", !grepl(".ynow_page_ui_corp_aml", ui, fixed = TRUE))
check("no applyUiLocale corp_aml key", !grepl("corp_aml: s.menu_corp_aml", ui, fixed = TRUE))
check("no lazy builder corp_aml", !grepl("corp_aml = function\\(\\) \\.ynow_page_ui_corp_aml\\(\\)", srv))
check("no body output corp_aml", !grepl("output\\$corp_aml_body_ui", srv))

# Server still scores AML for YNOW marks + veto
check("server snapshot", grepl("corp_aml_snapshot", srv, fixed = TRUE))
check("decision_server aml_pack", grepl("aml_pack = corp_aml_snapshot", srv, fixed = TRUE))
check("decision_server accepts aml_pack", grepl("aml_pack = reactive\\(NULL\\)", dec))
check("veto chip class", grepl("ynow-aml-veto", dec, fixed = TRUE))
check("AML mark helpers used", grepl("corp_aml_with_mark|corp_aml_mark_ui|corp_aml_surface_marks", dec))
check("surface marks helper", grepl("corp_aml_surface_marks", aml, fixed = TRUE))
check("mark ui helper", grepl("corp_aml_mark_ui", aml, fixed = TRUE))
check("AML mark CSS", grepl("ynow-aml-mark", ui, fixed = TRUE))

# Locale: badge + veto (no open-standalone-tab cue)
check("locale en mark label", grepl('aml_mark_label = "AML"', loc, fixed = TRUE))
check("locale zh mark label", grepl("aml_mark_label = \"AML\"", loc, fixed = TRUE))
check("locale en mark tooltip", grepl("aml_mark_tooltip = paste0", loc, fixed = TRUE))
check("locale zh mark tooltip", grepl("aml_mark_tooltip = paste0", loc) && grepl("法人洗錢跡象代理", loc, fixed = TRUE))
check("locale en veto no open tab", grepl("Overlapping YNOW indicators show an AML mark", loc, fixed = TRUE))
check("locale zh veto no open tab", grepl("YNOW 重疊指標旁會標示 AML", loc, fixed = TRUE))
check("locale no menu_corp_aml", !grepl("menu_corp_aml", loc, fixed = TRUE))

# Prefs: not pinnable / not in default order
check("pinnable omits corp_aml", !grepl('"corp_aml"', prefs, fixed = TRUE))
check("default order omits corp_aml", !grepl('"corp_aml"', prefs, fixed = TRUE))

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
check("not lite-omitted by name", !grepl("corp_aml", omit_block, fixed = TRUE))

check("about en mentions AML on YNOW", grepl("AML overlap marks on YNOW", about, fixed = TRUE))
check("about zh mentions AML", grepl("AML 重疊標示", about, fixed = TRUE))
check("about no standalone Corporate AML Signals", !grepl("Corporate AML Signals", about, fixed = TRUE))
check("about no standalone 法人洗錢跡象 board phrasing", !grepl("法人洗錢跡象（公開財報代理風險板", about, fixed = TRUE))
check("no bank fileInput for AML", !grepl("fileInput\\([^)]*aml", ui, ignore.case = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
