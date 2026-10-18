#!/usr/bin/env Rscript
# Per-user item locks (tabs, sections, inputs, reports).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
Sys.setenv(YNOW_ACCESS_LOCKS_FORCE_FILE = "1")
tmp <- tempfile(fileext = ".json")
Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp)
on.exit(unlink(tmp), add = TRUE)

ok <- 0L
bad <- 0L
check <- function(name, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK ", name)
  } else {
    bad <<- bad + 1L
    message("FAIL ", name)
  }
}

app_dir <- normalizePath("..", mustWork = TRUE)
if (!grepl("app_25.0$", app_dir)) {
  app_dir <- normalizePath(".", mustWork = TRUE)
}
source(file.path(app_dir, "access_locks.R"), local = FALSE)
ynow_access_reset_memory()

cat <- ynow_access_catalog()
check("catalog has four kinds", {
  all(c("tab", "section", "input", "report") %in% cat$kind)
})
check("catalog ids are unique", !anyDuplicated(cat$id))
check("backtest tab is lockable", "tab:lab_notes" %in% cat$id)
check("checklist tab is lockable", "tab:decision_checklist" %in% cat$id)
check("alerts tab is lockable", "tab:price_alerts" %in% cat$id)
check("sgr input is lockable", "input:sgr" %in% cat$id)
check("pdf report is lockable", "report:download_report" %in% cat$id)

check("unknown item rejected", {
  res <- ynow_access_set("user-a", "tab:not_a_page", TRUE)
  isTRUE(!res$ok) && identical(res$message_key, "access_bad_item")
})
check("lock then listed", {
  res <- ynow_access_set("user-a", "tab:lab_notes", TRUE)
  isTRUE(res$ok) && isTRUE(ynow_access_is_locked("user-a", "tab:lab_notes"))
})
check("second user stays open", !isTRUE(ynow_access_is_locked("user-b", "tab:lab_notes")))
check("unlock clears", {
  ynow_access_set("user-a", "tab:lab_notes", FALSE)
  !isTRUE(ynow_access_is_locked("user-a", "tab:lab_notes"))
})
check("file round-trip", {
  ynow_access_reset_memory()
  ynow_access_set("user-c", "input:sgr", TRUE)
  ynow_access_set("user-c", "report:download_report", TRUE)
  .ynow_access_mem$by_user <- list()
  ids <- ynow_access_locked_ids("user-c")
  all(c("input:sgr", "report:download_report") %in% ids)
})
check("zh label", identical(ynow_access_label("tab:decision_checklist", "zh-TW"), "決策檢核"))
check("en kind", identical(ynow_access_kind_label("report", "en"), "Report"))
check("zh kind", identical(ynow_access_kind_label("section", "zh-TW"), "區塊"))

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("ui marks shared bridge", grepl("section:shared_bridge", ui, fixed = TRUE))
check("ui applies lock handler", grepl("ynowAccessLocks", ui, fixed = TRUE))
check("server admin lock button", grepl("ynow_access_lock", srv, fixed = TRUE))
check("server denies pdf", grepl("report:download_report", srv, fixed = TRUE))
check("locale en lock", grepl("access_lock_btn = \"Lock item\"", loc, fixed = TRUE))
check("locale zh unlock", grepl("access_unlock_btn = \"解鎖項目\"", loc, fixed = TRUE))
check("ui parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_ui.R")), error = function(e) e), "expression"))
check("server parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_server.R")), error = function(e) e), "expression"))

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
