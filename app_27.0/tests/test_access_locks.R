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
if (!grepl("app_27.0$", app_dir)) {
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
check("product service tabs in catalog", {
  all(c(
    "tab:home", "tab:about", "tab:settings",
    "tab:account_admin", "tab:account_points", "tab:feedback"
  ) %in% cat$id) &&
    !"tab:testing" %in% cat$id
})
check("fine inputs and sections in catalog", {
  all(c(
    "input:years", "input:wacc_re", "input:mod_ddm-ke",
    "section:sgr_box", "section:wacc_box", "section:fcf_projection",
    "report:download_defaults", "report:bblab-export_data"
  ) %in% cat$id)
})

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
  grepl("li.ynow-access-locked-nav > a > .ynow-menu-lock", ui, fixed = TRUE) &&
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
    grepl("a:has(.ynow-menu-lock) > i:not(.ynow-menu-lock)", ui, fixed = TRUE) &&
    grepl("display: none !important", ui, fixed = TRUE) &&
    grepl("__YNOW_SIDEBAR_PERSIST_LOCK_TABS", ui, fixed = TRUE)
})
check("post-login still-locked chrome kept for ACL/persist (not hidden)", {
  grepl("Still-locked after auth: ACL + persist B", ui, fixed = TRUE) &&
    grepl("li.classList.remove('ynow-access-hidden')", ui, fixed = TRUE) &&
    grepl("ynowEnsureSidebarLockGlyph", ui, fixed = TRUE)
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
check("ATX ACL template is the surface overlay shell", {
  grepl("ynow-atx-lock-tpl ynow-member-gate-surface", ui, fixed = TRUE) &&
    grepl("ynow-member-gate-surface--locked", ui, fixed = TRUE) &&
    grepl("ynow_member_gate_fade_backdrop_ui", ui, fixed = TRUE) &&
    grepl("ynow-member-gate--surface-overlay", ui, fixed = TRUE) &&
    grepl("ynow-atx-lock-host > .ynow-member-gate-fade-backdrop", ui, fixed = TRUE)
})
check("other sidebar tab locks mount a surface host", {
  grepl("ynowEnsureSidebarTabLockSurface", ui, fixed = TRUE) &&
    grepl("ynow-tab-lock-host", ui, fixed = TRUE) &&
    grepl("ynow-member-gate-surface--locked ynow-tab-lock-host", ui, fixed = TRUE)
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
check("server apply-changes button", grepl("ynow_access_apply_changes", srv, fixed = TRUE))
check("server has scope + action + effect + unlock", {
  grepl("ynow_access_scope", srv, fixed = TRUE) &&
    grepl("ynow_access_action", srv, fixed = TRUE) &&
    grepl("ynow_access_effect", srv, fixed = TRUE) &&
    grepl("ynow_access_unlock_type", srv, fixed = TRUE) &&
    grepl("ynow_access_points_cost", srv, fixed = TRUE) &&
    grepl("ynow_access_free_count", srv, fixed = TRUE) &&
    grepl("ynow_access_time_hours", srv, fixed = TRUE) &&
    grepl("ynow_access_users", srv, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_user_search", srv, fixed = TRUE)
})
check("admin Item locks form order: scope → action → fields → apply", {
  m <- regexpr(
    'access_lock_title[\\s\\S]*?ynow_access_apply_changes',
    srv,
    perl = TRUE
  )
  if (m[1] < 0) return(FALSE)
  block <- substr(srv, as.integer(m[1]), as.integer(m[1]) + attr(m, "match.length") - 1L)
  pos_scope <- regexpr("ynow_access_scope", block, fixed = TRUE)[1]
  pos_locked <- regexpr("ynow_access_locked_now", block, fixed = TRUE)[1]
  pos_action <- regexpr("ynow_access_action", block, fixed = TRUE)[1]
  pos_fields <- regexpr("ynow_access_action_fields", block, fixed = TRUE)[1]
  pos_apply <- regexpr("ynow_access_apply_changes", block, fixed = TRUE)[1]
  # Match ends at apply-changes id (closing quote may be outside non-greedy slice).
  pos_scope > 0 &&
    pos_locked > pos_scope &&
    pos_action > pos_locked &&
    pos_fields > pos_action &&
    pos_apply > pos_fields
})
check("locale help leads with apply target (en+zh)", {
  grepl("Choose the apply target first", loc, fixed = TRUE) &&
    grepl("請先選擇套用對象", loc, fixed = TRUE) &&
    grepl("multi-select", loc, fixed = TRUE) &&
    grepl("複選", loc, fixed = TRUE) &&
    !grepl("First select the item", loc, fixed = TRUE) &&
    !grepl("請先選擇項目", loc, fixed = TRUE)
})
check("server pushes effective entries", {
  grepl("ynow_access_effective_entries", srv, fixed = TRUE)
})
check("server pushes entries", grepl("entries = entries", srv, fixed = TRUE) || grepl("entries =", srv, fixed = TRUE))
check("server denies pdf", grepl("report:download_report", srv, fixed = TRUE))
check("server live-updates locked-now from selected users", {
  grepl('uiOutput("ynow_access_locked_now")', srv, fixed = TRUE) &&
    grepl("output$ynow_access_locked_now", srv, fixed = TRUE) &&
    grepl("input$ynow_access_users", srv, fixed = TRUE) &&
    grepl("ynow_access_admin_status_text", srv, fixed = TRUE) &&
    grepl("ynow_access_display_tick", srv, fixed = TRUE) &&
    grepl("ynow_access_locked_now_value", srv, fixed = TRUE)
})
check("server preserves selected user across panel refresh", {
  grepl("isolate(input$ynow_pts_sysadmin_user)", srv, fixed = TRUE) &&
    grepl("cur_uid %in% as.character(choices)", srv, fixed = TRUE)
})
check("server keeps user list table with search", {
  grepl("ynow_pts_sysadmin_users_tbody", srv, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_user_search", srv, fixed = TRUE) &&
    grepl("ynowFilterSysadminUsers", srv, fixed = TRUE) &&
    grepl("selectizeInput", srv, fixed = TRUE) &&
    grepl("ynow_access_users", srv, fixed = TRUE)
})
check("locale en apply changes", grepl("access_apply_changes_btn = \"Apply changes\"", loc, fixed = TRUE))
check("locale zh action remove", grepl("access_action_remove = \"解除限制\"", loc, fixed = TRUE))
check("locale scope all en+zh", {
  grepl("access_scope_all = \"All users\"", loc, fixed = TRUE) &&
    grepl("access_scope_all = \"所有使用者\"", loc, fixed = TRUE)
})
check("locale effect lock zh", grepl("access_effect_lock = \"鎖上", loc, fixed = TRUE))
check("locale zh currently restricted", grepl("access_locked_now = \"目前已限制者\"", loc, fixed = TRUE))
check("locale unlock types en+zh", {
  grepl("access_unlock_type_label = \"Unlock condition\"", loc, fixed = TRUE) &&
    grepl("access_unlock_login = \"Unlock after login\"", loc, fixed = TRUE) &&
    grepl("access_unlock_points = \"Points unlock\"", loc, fixed = TRUE) &&
    grepl("access_unlock_conditional = \"Conditional unlock\"", loc, fixed = TRUE) &&
    grepl("access_unlock_type_label = \"解鎖條件\"", loc, fixed = TRUE) &&
    grepl("access_unlock_login = \"登入即解鎖\"", loc, fixed = TRUE) &&
    grepl("access_unlock_points = \"點數解鎖\"", loc, fixed = TRUE) &&
    grepl("access_unlock_conditional = \"條件解鎖\"", loc, fixed = TRUE) &&
    grepl("access_already_restricted =", loc, fixed = TRUE) &&
    grepl("access_not_restricted =", loc, fixed = TRUE) &&
    !grepl("一鍵", loc, fixed = TRUE)
})
check("locale menu main/sub en+zh", {
  grepl("access_menu_main = \"Main menu\"", loc, fixed = TRUE) &&
    grepl("access_menu_sub = \"Sub-options\"", loc, fixed = TRUE) &&
    grepl("access_menu_main = \"主選單\"", loc, fixed = TRUE) &&
    grepl("access_menu_sub = \"子選項\"", loc, fixed = TRUE)
})
check("ui parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_ui.R")), error = function(e) e), "expression"))
check("server parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_server.R")), error = function(e) e), "expression"))

# Live status text must track the selected user under scope=user (not global-only / not other user).
ynow_access_reset_memory()
unlink(tmp)
Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp)
ynow_access_set("user-alice", "tab:lab_notes", TRUE, effect = "lock")
ynow_access_set("user-bob", "input:sgr", TRUE, effect = "hide")
ynow_access_set(YNOW_ACCESS_ALL_USERS, "report:download_report", TRUE, effect = "hide")
check("status lines for alice under user scope", {
  lines <- ynow_access_admin_status_lines("user-alice", scope = "user", locale = "en",
                                          user_scope_lab = "User", all_scope_lab = "All")
  length(lines) == 1L &&
    grepl("Quant Backtest", lines[[1]], fixed = TRUE) &&
    grepl("Lock", lines[[1]], fixed = TRUE) &&
    !grepl("SGR", paste(lines, collapse = " "), fixed = TRUE) &&
    !grepl("Valuation report", paste(lines, collapse = " "), fixed = TRUE)
})
check("status lines switch when selected user changes", {
  a <- ynow_access_admin_status_text("user-alice", scope = "user", locale = "en",
                                     user_scope_lab = "User", all_scope_lab = "All",
                                     none_lab = "NONE")
  b <- ynow_access_admin_status_text("user-bob", scope = "user", locale = "en",
                                     user_scope_lab = "User", all_scope_lab = "All",
                                     none_lab = "NONE")
  grepl("Quant Backtest", a, fixed = TRUE) &&
    grepl("SGR", b, fixed = TRUE) &&
    !identical(a, b) &&
    !grepl("SGR", a, fixed = TRUE) &&
    !grepl("Quant Backtest", b, fixed = TRUE)
})
check("status lines for all-users scope ignore selected uid", {
  lines <- ynow_access_admin_status_lines("user-alice", scope = "all", locale = "en",
                                          user_scope_lab = "User", all_scope_lab = "All")
  length(lines) == 1L &&
    grepl("Valuation report", lines[[1]], fixed = TRUE) &&
    !grepl("Quant Backtest", paste(lines, collapse = " "), fixed = TRUE)
})
check("status text empty for user with no personal locks", {
  identical(
    ynow_access_admin_status_text("user-carol", scope = "user", locale = "zh-TW",
                                  user_scope_lab = "個人", all_scope_lab = "全部",
                                  none_lab = "此範圍沒有限制項目。"),
    "此範圍沒有限制項目。"
  )
})
check("zh status separator uses enumeration comma", {
  ynow_access_set("user-alice", "input:sgr", TRUE, effect = "hide")
  txt <- ynow_access_admin_status_text("user-alice", scope = "user", locale = "zh-TW",
                                       user_scope_lab = "個人", all_scope_lab = "全部",
                                       none_lab = "此範圍沒有限制項目。")
  grepl("、", txt, fixed = TRUE) && grepl("個人 ·", txt, fixed = TRUE)
})

# --- Unlock conditions (login / points / conditional) ---
tmp_state <- tempfile(fileext = ".json")
Sys.setenv(YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)
on.exit(unlink(tmp_state), add = TRUE)
ynow_access_reset_memory()
unlink(tmp)
Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp)

check("login unlock lifts for signed-in subject", {
  ynow_access_set(YNOW_ACCESS_ALL_USERS, "tab:lab_notes", TRUE,
                  effect = "lock", unlock_type = "login")
  isTRUE(ynow_access_is_locked("", "tab:lab_notes", effective = TRUE, authed = FALSE)) &&
    !isTRUE(ynow_access_is_locked("user-a", "tab:lab_notes", effective = TRUE, authed = TRUE)) &&
    identical(ynow_access_unlock_class("login"), "A")
})
check("points unlock stays locked until marked paid", {
  ynow_access_reset_memory()
  unlink(tmp); unlink(tmp_state)
  Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp, YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)
  ynow_access_set("user-a", "tab:decision_checklist", TRUE,
                  effect = "lock", unlock_type = "points", points_cost = 7L)
  still <- isTRUE(ynow_access_is_locked("user-a", "tab:decision_checklist",
                                        effective = TRUE, authed = TRUE))
  ynow_access_mark_points_unlocked("user-a", "tab:decision_checklist")
  opened <- !isTRUE(ynow_access_is_locked("user-a", "tab:decision_checklist",
                                          effective = TRUE, authed = TRUE))
  entry <- ynow_access_item_entry("user-a", "tab:decision_checklist")
  still && opened && identical(as.integer(entry$points_cost), 7L) &&
    identical(ynow_access_unlock_class("points"), "B")
})
check("points cost required", {
  res <- ynow_access_set("user-a", "tab:price_alerts", TRUE,
                         effect = "lock", unlock_type = "points", points_cost = 0L)
  isTRUE(!res$ok) && identical(res$message_key, "access_bad_points_cost")
})
check("conditional needs free count or time", {
  res <- ynow_access_set("user-a", "tab:price_alerts", TRUE,
                         effect = "lock", unlock_type = "conditional")
  isTRUE(!res$ok) && identical(res$message_key, "access_bad_conditional")
})
check("conditional free count unlocks then exhausts", {
  ynow_access_reset_memory()
  unlink(tmp); unlink(tmp_state)
  Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp, YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)
  ynow_access_set("user-a", "report:download_snapshot", TRUE,
                  effect = "hide", unlock_type = "conditional", free_count = 2L)
  open1 <- isTRUE(ynow_access_unlock_satisfied_for_item("user-a", "report:download_snapshot",
                                                       authed = TRUE))
  ynow_access_touch_conditional("user-a", "report:download_snapshot", consume_free = TRUE)
  ynow_access_touch_conditional("user-a", "report:download_snapshot", consume_free = TRUE)
  closed <- !isTRUE(ynow_access_unlock_satisfied_for_item("user-a", "report:download_snapshot",
                                                         authed = TRUE))
  open1 && closed && identical(ynow_access_unlock_class("conditional"), "C")
})
check("conditional time window unlocks while active", {
  ynow_access_reset_memory()
  unlink(tmp); unlink(tmp_state)
  Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp, YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)
  ynow_access_set(YNOW_ACCESS_ALL_USERS, "input:sgr", TRUE,
                  effect = "lock", unlock_type = "conditional", time_limit_hours = 2L)
  # Before touch: no window yet → still locked for signed-in user
  before <- !isTRUE(ynow_access_unlock_satisfied_for_item("user-z", "input:sgr", authed = TRUE))
  ynow_access_touch_conditional("user-z", "input:sgr", consume_free = FALSE, now = Sys.time())
  during <- isTRUE(ynow_access_unlock_satisfied_for_item("user-z", "input:sgr", authed = TRUE))
  expired <- !isTRUE(ynow_access_unlock_satisfied_for_item(
    "user-z", "input:sgr", authed = TRUE, now = Sys.time() + 3 * 3600
  ))
  before && during && expired
})
check("per-user unlock overrides all-users unlock type", {
  ynow_access_reset_memory()
  unlink(tmp); unlink(tmp_state)
  Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp, YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)
  ynow_access_set(YNOW_ACCESS_ALL_USERS, "tab:lab_notes", TRUE,
                  effect = "lock", unlock_type = "login")
  ynow_access_set("user-vip", "tab:lab_notes", TRUE,
                  effect = "lock", unlock_type = "points", points_cost = 3L)
  # VIP still needs points; other signed-in users unlock after login.
  vip_locked <- isTRUE(ynow_access_is_locked("user-vip", "tab:lab_notes",
                                            effective = TRUE, authed = TRUE))
  other_open <- !isTRUE(ynow_access_is_locked("user-other", "tab:lab_notes",
                                             effective = TRUE, authed = TRUE))
  vip_locked && other_open
})
check("status lines include unlock condition", {
  lines <- ynow_access_admin_status_lines("user-vip", scope = "user", locale = "en",
                                          user_scope_lab = "User", all_scope_lab = "All")
  length(lines) >= 1L && grepl("Points unlock", paste(lines, collapse = " "), fixed = TRUE)
})
check("legacy plain effect stays sticky (no auto unlock)", {
  store <- list(`user-legacy2` = list(`tab:lab_notes` = "lock"))
  jsonlite::write_json(store, tmp, auto_unbox = TRUE, pretty = TRUE)
  ynow_access_reset_memory()
  # Character-only map → no unlock_type → sticky after login
  isTRUE(ynow_access_is_locked("user-legacy2", "tab:lab_notes",
                               effective = TRUE, authed = TRUE))
})
check("zh unlock label for points", {
  grepl("點數解鎖", ynow_access_unlock_type_label("points", "zh-TW", points_cost = 5L), fixed = TRUE)
})

# --- Apply / remove invariants + menu layers + multi-user helpers ---
ynow_access_reset_memory()
unlink(tmp); unlink(tmp_state)
Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp, YNOW_ACCESS_UNLOCK_STATE_PATH = tmp_state)

check("menu layer tab/report are main", {
  identical(ynow_access_menu_layer("tab"), "main") &&
    identical(ynow_access_menu_layer("report"), "main")
})
check("menu layer section/input are sub", {
  identical(ynow_access_menu_layer("section"), "sub") &&
    identical(ynow_access_menu_layer("input"), "sub")
})
check("menu layer labels zh", {
  identical(ynow_access_menu_layer_label("main", "zh-TW"), "主選單") &&
    identical(ynow_access_menu_layer_label("sub", "zh-TW"), "子選項")
})
check("item choice list groups main/sub", {
  ch <- ynow_access_item_choice_list(
    c("tab:lab_notes", "input:sgr", "report:download_report"),
    locale = "en",
    group = TRUE
  )
  is.list(ch) &&
    "Main menu" %in% names(ch) &&
    "Sub-options" %in% names(ch) &&
    "tab:lab_notes" %in% unname(ch[["Main menu"]]) &&
    "input:sgr" %in% unname(ch[["Sub-options"]])
})
check("reject apply when already restricted", {
  ynow_access_set("user-a", "tab:lab_notes", TRUE, effect = "lock", unlock_type = "login")
  res <- ynow_access_set("user-a", "tab:lab_notes", TRUE, effect = "hide", unlock_type = "points",
                         points_cost = 3L)
  isTRUE(!res$ok) && identical(res$message_key, "access_already_restricted") &&
    identical(ynow_access_item_effect("user-a", "tab:lab_notes"), "lock")
})
check("reject remove when not restricted", {
  res <- ynow_access_set("user-a", "tab:price_alerts", FALSE)
  isTRUE(!res$ok) && identical(res$message_key, "access_not_restricted")
})
check("restrictable excludes already restricted for scope", {
  ids <- ynow_access_restrictable_ids_for_scope("user-a", "user")
  !"tab:lab_notes" %in% ids && "tab:price_alerts" %in% ids
})
check("restricted ids for multi-user union", {
  ynow_access_set("user-b", "input:sgr", TRUE, effect = "hide", unlock_type = "login")
  ids <- ynow_access_restricted_ids_for_scope(c("user-a", "user-b"), "user", multi_mode = "union")
  all(c("tab:lab_notes", "input:sgr") %in% ids)
})
check("restrictable for multi-user excludes any restricted", {
  ids <- ynow_access_restrictable_ids_for_scope(c("user-a", "user-b"), "user")
  !"tab:lab_notes" %in% ids && !"input:sgr" %in% ids
})
check("multi-user status text includes both uids", {
  txt <- ynow_access_admin_status_text(
    c("user-a", "user-b"), scope = "user", locale = "en",
    user_scope_lab = "User", all_scope_lab = "All", none_lab = "NONE"
  )
  grepl("user-a:", txt, fixed = TRUE) && grepl("user-b:", txt, fixed = TRUE) &&
    grepl("Quant Backtest", txt, fixed = TRUE) && grepl("SGR", txt, fixed = TRUE)
})
check("clear then re-apply allowed", {
  ynow_access_set("user-a", "tab:lab_notes", FALSE)
  res <- ynow_access_set("user-a", "tab:lab_notes", TRUE, effect = "hide", unlock_type = "login")
  isTRUE(res$ok) && identical(ynow_access_item_effect("user-a", "tab:lab_notes"), "hide")
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
