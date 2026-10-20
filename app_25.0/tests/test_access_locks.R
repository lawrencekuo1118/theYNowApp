#!/usr/bin/env Rscript
# Per-user / all-users item locks with lock vs hide effects.
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
  res <- ynow_access_set("user-a", "tab:lab_notes", TRUE, effect = "lock")
  isTRUE(res$ok) &&
    isTRUE(ynow_access_is_locked("user-a", "tab:lab_notes")) &&
    identical(ynow_access_item_effect("user-a", "tab:lab_notes"), "lock")
})
check("second user stays open", !isTRUE(ynow_access_is_locked("user-b", "tab:lab_notes")))
check("hide effect stored", {
  ynow_access_set("user-a", "input:sgr", TRUE, effect = "hide")
  identical(ynow_access_item_effect("user-a", "input:sgr"), "hide")
})
check("unlock clears", {
  ynow_access_set("user-a", "tab:lab_notes", FALSE)
  !isTRUE(ynow_access_is_locked("user-a", "tab:lab_notes"))
})
check("all-users applies to everyone", {
  ynow_access_reset_memory()
  unlink(tmp)
  Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp)
  res <- ynow_access_set(YNOW_ACCESS_ALL_USERS, "report:download_report", TRUE, effect = "hide")
  isTRUE(res$ok) &&
    isTRUE(ynow_access_is_locked("user-x", "report:download_report")) &&
    isTRUE(ynow_access_is_locked("", "report:download_report")) &&
    identical(ynow_access_item_effect("user-x", "report:download_report"), "hide")
})
check("per-user overrides all-users effect", {
  ynow_access_set("user-x", "report:download_report", TRUE, effect = "lock")
  identical(ynow_access_item_effect("user-x", "report:download_report"), "lock") &&
    identical(ynow_access_item_effect("user-y", "report:download_report"), "hide")
})
check("clear all-users only", {
  ynow_access_set(YNOW_ACCESS_ALL_USERS, "report:download_report", FALSE)
  !isTRUE(ynow_access_is_locked("user-y", "report:download_report")) &&
    isTRUE(ynow_access_is_locked("user-x", "report:download_report"))
})
check("file round-trip effects", {
  ynow_access_reset_memory()
  ynow_access_set("user-c", "input:sgr", TRUE, effect = "lock")
  ynow_access_set("user-c", "report:download_report", TRUE, effect = "hide")
  .ynow_access_mem$by_user <- list()
  ids <- ynow_access_locked_ids("user-c")
  identical(ynow_access_item_effect("user-c", "input:sgr"), "lock") &&
    identical(ynow_access_item_effect("user-c", "report:download_report"), "hide") &&
    all(c("input:sgr", "report:download_report") %in% ids)
})
check("legacy plain-id list becomes hide", {
  store <- list(`user-legacy` = list("tab:lab_notes", "input:sgr"))
  jsonlite::write_json(store, tmp, auto_unbox = TRUE, pretty = TRUE)
  ynow_access_reset_memory()
  identical(ynow_access_item_effect("user-legacy", "tab:lab_notes"), "hide") &&
    identical(ynow_access_item_effect("user-legacy", "input:sgr"), "hide")
})
check("zh label", identical(ynow_access_label("tab:decision_checklist", "zh-TW"), "決策檢核"))
check("en kind", identical(ynow_access_kind_label("report", "en"), "Report"))
check("zh kind", identical(ynow_access_kind_label("section", "zh-TW"), "區塊"))
check("zh effect lock", grepl("鎖上", ynow_access_effect_label("lock", "zh-TW"), fixed = TRUE))
check("en effect hide", identical(ynow_access_effect_label("hide", "en"), "Hide"))

# Shared labels with membership_locks (true duplicates only; domains stay separate)
check("shared helpers loaded", exists("ynow_lock_shared_surface_labels", mode = "function"))
check("access tabs match shared labels", {
  identical(ynow_access_label("tab:lab_notes", "en"), ynow_lock_shared_label("lab_notes", "en")) &&
    identical(ynow_access_label("tab:decision_checklist", "zh-TW"),
              ynow_lock_shared_label("decision_checklist", "zh-TW")) &&
    identical(ynow_access_label("tab:price_alerts", "en"), ynow_lock_shared_label("price_alerts", "en")) &&
    identical(ynow_access_label("section:checklist_body", "zh-TW"),
              ynow_lock_shared_label("checklist_body", "zh-TW"))
})
check("ACL report labels stay distinct from membership download copy", {
  # membership guest-gate: "Download Report (PDF)" / ACL: "Valuation report (PDF)"
  "report:download_report" %in% cat$id &&
    identical(ynow_access_label("report:download_report", "en"), "Valuation report (PDF)") &&
    identical(ynow_access_label("report:download_report", "zh-TW"), "估值報表（PDF）")
})
check("Lab body ACL label stays ACL-specific", {
  identical(ynow_access_label("section:backtest_body", "en"), "Quant Backtest Lab body")
})

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("ui marks shared bridge", grepl("section:shared_bridge", ui, fixed = TRUE))
check("ui applies lock handler", grepl("ynowAccessLocks", ui, fixed = TRUE))
check("ui distinguishes hide vs lock classes", {
  grepl("ynow-access-hidden", ui, fixed = TRUE) &&
    grepl("ynow-access-locked-nav", ui, fixed = TRUE) &&
    grepl("ynow-access-locked-pane", ui, fixed = TRUE)
})
check("sysadmin sidebar lock swaps the glyph for a lock", {
  grepl("li.ynow-access-locked-nav .ynow-menu-lock", ui, fixed = TRUE) &&
    grepl("li.ynow-access-locked-nav > a > i:not(.ynow-menu-lock)", ui, fixed = TRUE) &&
    grepl("ynow-access-lock", ui, fixed = TRUE) &&
    grepl("ynowApplyAccessLocks()", ui, fixed = TRUE)
})
check("guest locks share unified sidebar locked chrome with ACL (ATX)", {
  grepl("ynow-sidebar-locked", ui, fixed = TRUE) &&
    grepl("Unified locked sidebar", ui, fixed = TRUE) &&
    grepl("ynowSyncGuestSidebarLocked", ui, fixed = TRUE) &&
    grepl(
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(> .ynow-menu-lock)",
      ui,
      fixed = TRUE
    ) &&
    grepl("opacity: 0.55", ui, fixed = TRUE) &&
    grepl("a:has(.ynow-menu-lock) > i:not(.ynow-menu-lock)", ui, fixed = TRUE)
})
check("locked asset transmission pane shows feature promo and sign-in", {
  auth_src <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_atx_lock_promo_tpl", ui, fixed = TRUE) &&
    grepl("ynowEnsureAtxLockPromo", ui, fixed = TRUE) &&
    grepl("tab === 'asset_transmission'", ui, fixed = TRUE) &&
    grepl("ynow-atx-locked-pane", ui, fixed = TRUE) &&
    grepl('feature = "atx"', ui, fixed = TRUE) &&
    grepl("\"atx\"", auth_src, fixed = TRUE)
})
check("inner targets lock the containing tab and promo", {
  grepl("ynowMarkLockedInnerTarget", ui, fixed = TRUE) &&
    grepl("ynow-inner-tab-locked", ui, fixed = TRUE) &&
    grepl("ynow-inner-tab-lock-host", ui, fixed = TRUE) &&
    grepl("ynow_inner_tab_lock_promo_tpl", ui, fixed = TRUE) &&
    grepl('feature = "inner_lock"', ui, fixed = TRUE) &&
    grepl("ynow-inner-locked-pane > .ynow-inner-tab-lock-host", ui, fixed = TRUE)
})
check("js reads entries effect", {
  grepl("__ynowAccessEntries", ui, fixed = TRUE) &&
    grepl("effect === 'hide'", ui, fixed = TRUE)
})
check("server admin lock button", grepl("ynow_access_lock", srv, fixed = TRUE))
check("server has scope + effect radios", {
  grepl("ynow_access_scope", srv, fixed = TRUE) &&
    grepl("ynow_access_effect", srv, fixed = TRUE)
})
check("server pushes entries", grepl("entries = entries", srv, fixed = TRUE) || grepl("entries =", srv, fixed = TRUE))
check("server denies pdf", grepl("report:download_report", srv, fixed = TRUE))
check("locale en apply", grepl("access_lock_btn = \"Apply restriction\"", loc, fixed = TRUE))
check("locale zh clear", grepl("access_unlock_btn = \"清除限制\"", loc, fixed = TRUE))
check("locale scope all en+zh", {
  grepl("access_scope_all = \"All users\"", loc, fixed = TRUE) &&
    grepl("access_scope_all = \"所有使用者\"", loc, fixed = TRUE)
})
check("locale effect lock zh", grepl("access_effect_lock = \"鎖上", loc, fixed = TRUE))
check("ui parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_ui.R")), error = function(e) e), "expression"))
check("server parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_server.R")), error = function(e) e), "expression"))

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
