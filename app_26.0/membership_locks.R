# ==========================================
# Membership lock catalog — A / B only
# ==========================================
# Single source of truth for Full-mode freemium locks.
#
# Tier A = unlock-after-login (guest blocked until signed in; no FYN/PYN for the
#          surface itself).
# Tier B = unlock-after-login-and-points (needs account AND FYN/PYN spend when
#          YNOW_POINTS_ENABLED=1; guest still blocked first).
#
# Every membership-gated id MUST appear here with tier A or B.
# Sysadmin ACL unlock rules (`access_locks.R`) add a third formal class for
# admin-configured item locks only:
#   A login | B points (+ cost) | C conditional (free count and/or time window).
# That C class does NOT replace freemium A/B rows in this catalog.
# Sysadmin ACL (`access_locks.R`), APP_DEFAULTS unlock, Decision SOP, Blue Chip
# method locks, and numeric input locks are NON-membership — see
# ynow_non_membership_lock_catalog(). They must not be left as "unclassified".
#
# Lite: member gates + proprietary download login gates stay open / omitted per
# ynow_sidebar_lite_omit_tabs() and free PDF prefs — this catalog does not
# change Lite exemptions.
#
# Shared bilingual labels for surfaces that also appear in access_locks.R live
# in lock_catalog_shared.R (domains stay separate; only true duplicate copy).

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

# Ensure shared catalog helpers (standalone source / tests).
if (!exists("ynow_lock_shared_surface_labels", mode = "function")) {
  .ynow_lcs_ofile <- tryCatch(
    normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
    error = function(e) ""
  )
  .ynow_lcs_cands <- unique(c(
    if (is.character(.ynow_lcs_ofile) && nzchar(.ynow_lcs_ofile)) {
      file.path(dirname(.ynow_lcs_ofile), "lock_catalog_shared.R")
    } else {
      character()
    },
    "lock_catalog_shared.R",
    file.path("app_26.0", "lock_catalog_shared.R"),
    file.path("..", "lock_catalog_shared.R")
  ))
  .ynow_lcs_loaded <- FALSE
  for (.ynow_lcs_p in .ynow_lcs_cands) {
    if (is.character(.ynow_lcs_p) && nzchar(.ynow_lcs_p) && file.exists(.ynow_lcs_p)) {
      sys.source(.ynow_lcs_p, envir = environment(), keep.source = TRUE)
      .ynow_lcs_loaded <- exists("ynow_lock_shared_surface_labels", mode = "function")
      if (isTRUE(.ynow_lcs_loaded)) break
    }
  }
  if (!isTRUE(.ynow_lcs_loaded)) {
    warning("lock_catalog_shared.R not found; shared lock labels unavailable", call. = FALSE)
  }
  rm(list = intersect(
    c(".ynow_lcs_ofile", ".ynow_lcs_cands", ".ynow_lcs_p", ".ynow_lcs_loaded"),
    ls(all.names = TRUE)
  ))
}

YNOW_LOCK_TIER_A <- "A"
YNOW_LOCK_TIER_B <- "B"
.YNOW_LOCK_TIERS <- c(YNOW_LOCK_TIER_A, YNOW_LOCK_TIER_B)

#' Membership lock catalog (freemium A/B).
#'
#' Columns: id, kind, tier, label_en, label_zh_tw, mechanism, notes
#' kind: sidebar_tab | gate_feature | download | points_sku | surface
#' mechanism: stable code path tag (guest_menu_lock, member_gate, guest_download,
#'            points_spend, css_hide, display_only, parent_gate)
ynow_membership_lock_catalog <- function() {
  .sl <- function(key) ynow_lock_shared_label_pair(key)
  lab_notes <- .sl("lab_notes")
  decision_checklist <- .sl("decision_checklist")
  price_alerts <- .sl("price_alerts")
  checklist_body <- .sl("checklist_body")
  rows <- list(
    # --- Sidebar tabs (Full guest lock glyph / hide) ---
    # Labels from lock_catalog_shared (also used by access tab:*)
    c("lab_notes", "sidebar_tab", "A", lab_notes$en, lab_notes$zh,
      "guest_menu_lock", "Full guest lock; Lite omitted"),
    c("decision_checklist", "sidebar_tab", "A", decision_checklist$en, decision_checklist$zh,
      "guest_menu_lock", "Full guest lock; Lite omitted"),
    c("price_alerts", "sidebar_tab", "A", price_alerts$en, price_alerts$zh,
      "guest_menu_lock", "Full guest lock; Lite omitted (prefs)"),
    c("asset_transmission", "sidebar_tab", "A", "Asset Transmission Root", "資產傳導根系",
      "guest_menu_lock", "Full guest lock; Lite omitted"),
    c("account_admin", "sidebar_tab", "A", "Account / Overview", "帳號／總覽",
      "css_hide", "Guest CSS hide; signed-in visible; Lite omitted"),
    c("account_points", "sidebar_tab", "A", "Points ledger", "點數流水",
      "css_hide", "Guest CSS hide; signed-in visible; Lite omitted"),
    c("settings", "sidebar_tab", "A", "Settings", "設定",
      "css_hide", "Guest CSS hide; signed-in visible; Lite omitted"),

    # --- Gate feature ids used by member / PDF promo chrome ---
    c("lab", "gate_feature", "A", "Quant Backtest Lab gate", "量化回測實驗室閘道",
      "member_gate", "ynow_member_gate_ui feature=lab"),
    c("checklist", "gate_feature", "A", "Decision Checklist gate", "決策檢核閘道",
      "member_gate", "ynow_member_gate_ui feature=checklist"),
    c("alerts", "gate_feature", "A", "Value Alerts gate", "到值通知閘道",
      "member_gate", "ynow_member_gate_ui feature=alerts"),
    c("account", "gate_feature", "A", "Account workspace gate", "帳號工作區閘道",
      "member_gate", "ynow_account_gate_ui feature=account"),
    c("pdf", "gate_feature", "B", "Valuation PDF gate", "估值 PDF 閘道",
      "guest_download", "Login then free quota / FYN (pdf_report)"),

    # --- Proprietary downloads (Full guest login gate; Lite open) ---
    # Wording intentionally differs from access report:* ACL labels.
    c("download_report", "download", "B", "Download Report (PDF)", "下載報告 (PDF)",
      "guest_download", "After login: free×3/day then pdf_report SKU"),
    c("download_snapshot", "download", "A", "Download Snapshot CSV", "下載快照 CSV",
      "guest_download", "Login only"),
    c("download_param_restore", "download", "A", "Download restore CSV", "下載還原 CSV",
      "guest_download", "Login only"),
    c("download_defaults", "download", "A", "Download Defaults CSV", "下載系統預設 CSV",
      "guest_download", "Guest login gate; APP_DEFAULTS body is ops_sysadmin separately"),
    c("lab_im_download_report", "download", "A", "Lab page report download", "實驗室本頁報告下載",
      "guest_download", "Login only"),
    c("bblab-export_data", "download", "A", "BBLab export data", "業務拆解匯出資料",
      "guest_download", "Login only"),
    c("bblab-export_chart", "download", "A", "BBLab export chart", "業務拆解匯出圖表",
      "guest_download", "Login only"),
    c("param_audit_pdf_go", "download", "A", "Annotated PDF capture", "參數稽核 PDF",
      "guest_download", "Login only"),

    # --- Points / FYN SKUs (always B) ---
    c("alert_slot", "points_sku", "B", "Value Alert slot", "到值通知一筆",
      "points_spend", "Default cost 1 FYN"),
    c("bt_run", "points_sku", "B", "Backtest full run", "回測完整執行",
      "points_spend", "Default cost 5 FYN"),
    c("pdf_report", "points_sku", "B", "Valuation report (PDF)", "估值報告（PDF）",
      "points_spend", "Default cost 10 FYN after free quota"),

    # --- Nested surfaces inherit parent tab (explicit for inventory/tests) ---
    # Lab / alerts body copy intentionally differs from access section:* body labels.
    c("ynow_lab_body", "surface", "A", "Lab worksheets / charts", "實驗室工作表／圖表",
      "parent_gate", "Hidden behind lab member gate"),
    c("ynow_chk_body", "surface", "A", checklist_body$en, checklist_body$zh,
      "parent_gate", "Hidden behind checklist member gate"),
    c("ynow_pa_body", "surface", "A", "Value Alerts tables", "到值通知表格",
      "parent_gate", "Hidden behind alerts member gate"),
    c("ynow_admin_panel", "surface", "A", "Account overview panel", "帳號總覽面板",
      "parent_gate", "Account workspace"),
    c("ynow_points_panel", "surface", "A", "Points ledger panel", "點數流水面板",
      "parent_gate", "Account workspace"),
    c("ynow-pts-sidebar", "surface", "A", "FYN/PYN header banner", "點數橫幅",
      "display_only", "Hidden for guests"),
    c("ynow-alert-bell", "surface", "A", "KPI Value Alert bell", "KPI 到值鈴鐺",
      "css_hide", "Full guest: pointer-events none; create spend is alert_slot B")
  )
  out <- .ynow_lock_catalog_df(
    rows,
    c("id", "kind", "tier", "label_en", "label_zh_tw", "mechanism", "notes")
  )
  if (anyDuplicated(out$id)) {
    stop("ynow_membership_lock_catalog: duplicate id(s): ",
         paste(unique(out$id[duplicated(out$id)]), collapse = ", "),
         call. = FALSE)
  }
  bad_tier <- out$tier[!out$tier %in% .YNOW_LOCK_TIERS]
  if (length(bad_tier)) {
    stop("ynow_membership_lock_catalog: invalid tier(s): ",
         paste(unique(bad_tier), collapse = ", "),
         call. = FALSE)
  }
  out
}

#' Non-membership locks (ops ACL / sysadmin / domain) — outside freemium A/B.
#' Documented so nothing is left "unclassified".
ynow_non_membership_lock_catalog <- function() {
  rows <- list(
    c("snap_defaults", "gate_feature", "ops_sysadmin",
      "Snapshot APP_DEFAULTS tab", "快照系統預設分頁",
      "Requires sysadmin session unlock; not login/points freemium"),
    c("inner_lock", "gate_feature", "ops_acl",
      "Inner tab ACL lock promo", "內層分頁 ACL 鎖定文案",
      "Shown when access_locks applies; admin-imposed"),
    c("atx", "gate_feature", "ops_acl",
      "Asset Transmission ACL lock promo", "資產傳導根系 ACL 鎖定文案",
      "Promo copy for the ATX page gate and for tab:asset_transmission ACL lock"),
    c("access_locks", "ops_catalog", "ops_acl",
      "Sysadmin access_locks catalog", "系統管理 ACL 目錄",
      "access_locks.R — lock/hide + unlock A/B/C (login/points/conditional); per-user / __all__"),
    c("dc_sop", "domain", "domain_discipline",
      "Decision SOP verdict banner", "決策 SOP 檢核橫幅",
      "Discipline unlock after acks — not membership"),
    c("lab_im_method", "domain", "domain_data",
      "Blue Chip method checkboxes", "Blue Chip 方法勾選",
      "Unlocks when model is primary/listed"),
    c("lock_numeric_input", "domain", "domain_ux",
      "Bridge numeric input lock", "橋接數值框鎖定",
      "After Search/auto-fill — calculation UX"),
    c("hfv_run_retired", "retired_sku", "retired",
      "HFV full recalculation (retired SKU)", "HFV 完整重算（已下架 SKU）",
      "Removed from points catalog — never wired to ynow_points_gate")
  )
  .ynow_lock_catalog_df(
    rows,
    c("id", "kind", "scope", "label_en", "label_zh_tw", "notes")
  )
}

#' Look up freemium tier for a membership lock id (A / B / NA).
ynow_membership_lock_tier <- function(id) {
  id <- as.character(id %||% "")[1]
  if (!nzchar(id)) return(NA_character_)
  cat <- ynow_membership_lock_catalog()
  hit <- cat[cat$id == id, , drop = FALSE]
  if (!nrow(hit)) return(NA_character_)
  as.character(hit$tier[[1]])
}

#' TRUE when id is a classified membership lock (A or B).
ynow_membership_lock_is_classified <- function(id) {
  tier <- ynow_membership_lock_tier(id)
  !is.na(tier) && tier %in% .YNOW_LOCK_TIERS
}

#' Filter membership catalog ids.
#' @param tier "A", "B", or both (default)
#' @param kind optional kind filter (character vector)
#' @param mechanism optional mechanism filter
ynow_membership_lock_ids <- function(tier = c("A", "B"), kind = NULL, mechanism = NULL) {
  cat <- ynow_membership_lock_catalog()
  want_tier <- unique(as.character(tier %||% c("A", "B")))
  cat <- cat[cat$tier %in% want_tier, , drop = FALSE]
  if (!is.null(kind)) {
    cat <- cat[cat$kind %in% as.character(kind), , drop = FALSE]
  }
  if (!is.null(mechanism)) {
    cat <- cat[cat$mechanism %in% as.character(mechanism), , drop = FALSE]
  }
  as.character(cat$id)
}

#' Membership gate feature ids (promo / demo) — excludes ops_acl gate ids.
ynow_membership_gate_features <- function() {
  ynow_membership_lock_ids(kind = "gate_feature")
}

#' Ops / non-membership gate feature ids still used for promo templates + demos.
ynow_ops_gate_features <- function() {
  cat <- ynow_non_membership_lock_catalog()
  as.character(cat$id[cat$kind == "gate_feature"])
}

#' Points SKUs that spend FYN (all tier B).
ynow_membership_points_skus <- function() {
  ynow_membership_lock_ids(kind = "points_sku", tier = "B")
}

#' Default cost / titles for membership points SKUs (env overrides applied by caller).
#' Titles come from the membership lock catalog (single source).
ynow_membership_points_sku_defaults <- function() {
  # Cost defaults live here so points_wallet stays aligned with the A/B catalog.
  skus <- c("alert_slot", "bt_run", "pdf_report")
  cat <- ynow_membership_lock_catalog()
  title_en <- vapply(skus, function(s) {
    hit <- cat[cat$id == s, , drop = FALSE]
    if (!nrow(hit)) s else as.character(hit$label_en[[1]])
  }, character(1))
  title_zh_tw <- vapply(skus, function(s) {
    hit <- cat[cat$id == s, , drop = FALSE]
    if (!nrow(hit)) s else as.character(hit$label_zh_tw[[1]])
  }, character(1))
  data.frame(
    sku = skus,
    title_en = title_en,
    title_zh_tw = title_zh_tw,
    cost_default = c(1L, 5L, 10L),
    env_key = c(
      "YNOW_POINTS_PRICE_ALERT_SLOT",
      "YNOW_POINTS_PRICE_BT_RUN",
      "YNOW_POINTS_PRICE_PDF_REPORT"
    ),
    enabled = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

#' HTML data attribute value for a membership id (`A` / `B` / "").
ynow_membership_lock_tier_attr <- function(id) {
  tier <- ynow_membership_lock_tier(id)
  if (is.na(tier)) "" else tier
}

#' Assert every membership catalog row is A or B (for tests / boot checks).
ynow_membership_lock_assert_classified <- function() {
  cat <- ynow_membership_lock_catalog()
  if (!nrow(cat)) stop("membership lock catalog is empty", call. = FALSE)
  bad <- cat$id[!cat$tier %in% .YNOW_LOCK_TIERS]
  if (length(bad)) {
    stop(
      "Unclassified membership lock id(s) (must be A or B): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
