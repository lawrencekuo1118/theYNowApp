# Per-user / global UI restrictions set from System admin.
# item id = kind:target  (tab | section | input | report)
# effect  = "lock" (show locked placeholder / disable) | "hide" (remove from UI)
# scope   = selected user id, or YNOW_ACCESS_ALL_USERS for every account (+ guests)
#
# Unlock condition (per locked item; default scope = all users; per-user override):
#   login       = unlock-after-login（登入即解鎖）
#   points      = unlock-after-login-and-points（點數解鎖）+ points_cost
#   conditional = 條件解鎖 — free_count and/or time_limit_hours (after login)
# Legacy rows without unlock_type stay sticky (locked until admin clears).
#
# Product freemium A/B gates live in membership_locks.R — do not merge domains.
# Identical bilingual labels for overlapping surfaces come from
# lock_catalog_shared.R (tabs lab_notes / decision_checklist / price_alerts,
# and section:checklist_body). Download/report ACL labels stay ACL-specific.

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
    file.path("app_27.0", "lock_catalog_shared.R"),
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

YNOW_ACCESS_ALL_USERS <- "__all__"
.YNOW_ACCESS_EFFECTS <- c("lock", "hide")
YNOW_UNLOCK_LOGIN <- "login"
YNOW_UNLOCK_POINTS <- "points"
YNOW_UNLOCK_CONDITIONAL <- "conditional"
.YNOW_UNLOCK_TYPES <- c(YNOW_UNLOCK_LOGIN, YNOW_UNLOCK_POINTS, YNOW_UNLOCK_CONDITIONAL)

.ynow_access_mem <- new.env(parent = emptyenv())
.ynow_access_mem$by_user <- list()
.ynow_access_mem$unlock_state <- list()

ynow_access_catalog <- function() {
  .sl <- function(key) ynow_lock_shared_label_pair(key)
  lab_notes <- .sl("lab_notes")
  decision_checklist <- .sl("decision_checklist")
  price_alerts <- .sl("price_alerts")
  checklist_body <- .sl("checklist_body")
  rows <- list(
    # ---- Tabs (pages / sidebar + foot product surfaces) ----
    c("tab:home", "tab", "Home", "首頁"),
    c("tab:macro_market", "tab", "Macro & Market Trends", "總體與市場趨勢"),
    c("tab:asset_transmission", "tab", "Asset Transmission Root", "資產傳導根"),
    c("tab:bluechip", "tab", "Blue Chip Leaderboard", "Blue Chip 排行"),
    c("tab:dashboard", "tab", "Company", "公司"),
    c("tab:company_advance", "tab", "Business Breakdown", "業務拆解"),
    c("tab:sensitivity", "tab", "YNOW", "YNOW"),
    c("tab:smart_analysis", "tab", "Smart Analysis", "智慧分析"),
    c("tab:get_started", "tab", "Model Dashboard", "模型儀表板"),
    c("tab:nav_calculator", "tab", "NAV", "NAV"),
    c("tab:pb_calculator", "tab", "Justified P/B", "Justified P/B"),
    c("tab:dcf_calculator", "tab", "DCF-Model", "DCF-Model"),
    c("tab:ddm_calculator", "tab", "DDM", "DDM"),
    c("tab:ri_calculator", "tab", "RI-Model", "RI-Model"),
    c("tab:rel_mult_ev", "tab", "Enterprise value", "企業價值"),
    c("tab:rel_multiples_calculator", "tab", "Earnings", "盈餘倍數"),
    c("tab:rel_mult_rev", "tab", "Revenue", "營收倍數"),
    c("tab:sotp_calculator", "tab", "SOTP", "SOTP"),
    c("tab:hfv", "tab", "Hist. FV Validation", "歷史公允價值驗證"),
    # Shared labels with membership sidebar_tab rows
    c("tab:lab_notes", "tab", lab_notes$en, lab_notes$zh),
    c("tab:decision_checklist", "tab", decision_checklist$en, decision_checklist$zh),
    c("tab:price_alerts", "tab", price_alerts$en, price_alerts$zh),
    c("tab:about", "tab", "About", "關於"),
    c("tab:settings", "tab", "Settings", "設定"),
    c("tab:account_admin", "tab", "Account / Overview", "帳號／總覽"),
    c("tab:account_points", "tab", "Points ledger", "點數流水"),
    c("tab:snapshot", "tab", "Snapshot", "快照"),
    c("tab:feedback", "tab", "Feedback", "意見回饋"),
    # ---- Sections (need matching data-ynow-lock wrappers in UI) ----
    c("section:shared_bridge", "section", "Cash, Debt, Shares & Revenue (shared)", "Cash、Debt、股數與營收（各模型共用）"),
    c("section:sgr_box", "section", "SGR (sustainable growth rate)", "永續成長率 g（SGR）區塊"),
    c("section:beta_box", "section", "Beta (Model Dashboard)", "Beta（模型儀表板）"),
    c("section:wacc_box", "section", "WACC settings", "WACC 設定"),
    c("section:dcf_two_stage", "section", "DCF two-stage assumptions", "DCF 兩階段假設"),
    c("section:ddm_two_stage", "section", "DDM two-stage assumptions", "DDM 兩階段假設"),
    c("section:fcf_projection", "section", "FCF projection", "自由現金流預測"),
    c("section:bt_holding_gate", "section", "Lab holding gate", "實驗室持股門檻"),
    c("section:bt_strategy_params", "section", "Lab strategy parameters", "實驗室策略參數"),
    c("section:snap_audit", "section", "Snapshot manual adjustments", "快照手動調整"),
    c("section:snapshot_restore", "section", "Snapshot restore", "快照還原"),
    # Lab / alerts body wording stays ACL-specific (≠ membership surface copy)
    c("section:value_alerts_body", "section", "Value Alerts body", "到值通知內容"),
    c("section:backtest_body", "section", "Quant Backtest Lab body", "量化回測實驗室內容"),
    c("section:checklist_body", "section", checklist_body$en, checklist_body$zh),
    # ---- Inputs (fine controls; DOM id = target after input:) ----
    c("input:bridge_cash", "input", "Cash", "Cash"),
    c("input:bridge_debt", "input", "Total Debt", "Total Debt"),
    c("input:bridge_shares", "input", "Shares", "股數"),
    c("input:bridge_revenue", "input", "Total revenue", "總營收"),
    c("input:sgr", "input", "Terminal g (SGR)", "永續成長率 g（SGR）"),
    c("input:years", "input", "Forecast years n", "預測年數 n"),
    c("input:g_stage1", "input", "Near-term g1", "近端成長率 g1"),
    c("input:yr_stage1", "input", "Stage-1 years", "第一階段年數"),
    c("input:wacc_stage1", "input", "WACC1", "WACC1"),
    c("input:wacc_stage2", "input", "WACC2", "WACC2"),
    c("input:wacc_re", "input", "Cost of equity rₑ", "權益資金成本 rₑ"),
    c("input:wacc_rd", "input", "Cost of debt rᵈ", "負債資金成本 rᵈ"),
    c("input:wacc_tax", "input", "Tax rate T", "稅率 T"),
    c("input:capm_rf", "input", "CAPM Rf", "CAPM Rf"),
    c("input:capm_rm", "input", "CAPM Rm", "CAPM Rm"),
    c("input:capm_beta", "input", "CAPM Beta", "CAPM Beta"),
    c("input:custom_g", "input", "Custom revenue growth g", "自訂營收成長率 g"),
    c("input:mod_fcf-proj_capex_rate", "input", "CapEx / Revenue (%)", "CapEx／營收比（%）"),
    c("input:mod_fcf-proj_nwc_rate", "input", "ΔNWC / ΔRevenue (%)", "ΔNWC／Δ營收（%）"),
    c("input:mod_fcf-capex_spike_mult", "input", "CapEx spike multiple", "CapEx 暴衝倍數閾值"),
    c("input:mod_fcf-capex_spike_avg_years", "input", "CapEx spike average years", "CapEx 暴衝均值年數"),
    c("input:mod_ddm-d0", "input", "DDM D0", "DDM D0"),
    c("input:mod_ddm-g", "input", "DDM g", "DDM g"),
    c("input:mod_ddm-ke", "input", "DDM Ke", "DDM Ke"),
    c("input:mod_ddm-yr_stage1", "input", "DDM stage-1 years", "DDM 第一階段年數"),
    c("input:mod_ddm-g_stage1", "input", "DDM near-term g1", "DDM 近端成長率 g1"),
    c("input:mod_ri-b0", "input", "RI book equity B0", "RI 帳面權益 B0"),
    c("input:mod_ri-ri_years", "input", "RI years", "RI 年數"),
    c("input:mod_ri-ri_ke", "input", "RI Ke", "RI Ke"),
    c("input:mod_ri-ri_g", "input", "RI g", "RI g"),
    c("input:mod_ri-ri_roe", "input", "RI ROE", "RI ROE"),
    c("input:mod_ri-ri_payout", "input", "RI payout", "RI 配息率"),
    c("input:mod_nav-navps", "input", "NAVPS", "NAVPS"),
    c("input:mod_nav-nav_mid", "input", "NAV mid multiple", "NAV 基準倍數"),
    c("input:mod_pb-bvps", "input", "BVPS", "BVPS"),
    c("input:mod_pb-pb_mid", "input", "Justified P/B mid", "合理 P/B 基準"),
    c("input:mod_rel-pe_multiple", "input", "P/E multiple", "本益比倍數"),
    c("input:mod_rel-ev_ebitda_multiple", "input", "EV/EBITDA multiple", "EV／EBITDA 倍數"),
    c("input:mod_rel-ev_sales_multiple", "input", "EV/Sales multiple", "EV／營收倍數"),
    c("input:mod_rel-ps_multiple", "input", "P/S multiple", "股價／營收倍數"),
    c("input:mod_sotp-default_ev_sales", "input", "SOTP default EV/Sales", "SOTP 預設 EV／營收"),
    c("input:mod_sotp-nonop", "input", "SOTP non-operating assets", "SOTP 非營業資產"),
    c("input:bt_net_margin", "input", "Lab net margin threshold", "實驗室淨利率門檻"),
    c("input:bt_rev_growth", "input", "Lab revenue growth threshold", "實驗室營收成長門檻"),
    c("input:bt_eps_growth", "input", "Lab EPS growth threshold", "實驗室 EPS 成長門檻"),
    c("input:bt_fcf_cv", "input", "Lab FCF CV ceiling", "實驗室 FCF CV 上限"),
    # ---- Reports / downloads ----
    # ACL report labels intentionally differ from membership download_* guest-gate copy
    c("report:download_report", "report", "Valuation report (PDF)", "估值報表（PDF）"),
    c("report:download_snapshot", "report", "Snapshot CSV", "快照 CSV"),
    c("report:download_param_restore", "report", "Parameter restore CSV", "參數還原 CSV"),
    c("report:download_defaults", "report", "Defaults CSV", "系統預設 CSV"),
    c("report:lab_im_download_report", "report", "Lab page report", "實驗室本頁報告"),
    c("report:bblab-export_data", "report", "Business Breakdown export data", "業務拆解匯出資料"),
    c("report:bblab-export_chart", "report", "Business Breakdown export chart", "業務拆解匯出圖表"),
    c("report:FS_download", "report", "Finance summary", "財務摘要"),
    c("report:IS_download", "report", "Income statement", "損益表"),
    c("report:BS_download", "report", "Balance sheet", "資產負債表"),
    c("report:CF_download", "report", "Cash flow statement", "現金流量表")
  )
  # Column name label_zh kept for admin UI / ynow_server.R API stability
  # (membership uses label_zh_tw; shared helper stores label_zh_tw).
  .ynow_lock_catalog_df(
    rows,
    c("id", "kind", "label_en", "label_zh")
  )
}

ynow_access_label <- function(id, locale = "en") {
  cat <- ynow_access_catalog()
  hit <- which(cat$id == as.character(id)[1])
  if (!length(hit)) return(as.character(id)[1])
  ynow_lock_pick_locale_label(
    cat$label_en[[hit[[1]]]],
    cat$label_zh[[hit[[1]]]],
    locale
  )
}

ynow_access_kind_label <- function(kind, locale = "en") {
  zh <- identical(as.character(locale)[1], "zh-TW")
  k <- as.character(kind)[1]
  if (zh) {
    switch(k, tab = "分頁", section = "區塊", input = "數值框", report = "報表", k)
  } else {
    switch(k, tab = "Tab", section = "Section", input = "Input", report = "Report", k)
  }
}

#' Menu layer for admin item picker: main tabs/chrome vs nested sub-options.
#' tab + report = 主選單; section + input = 子選項.
ynow_access_menu_layer <- function(kind) {
  k <- as.character(kind %||% "")[1]
  if (k %in% c("tab", "report")) "main" else "sub"
}

ynow_access_menu_layer_label <- function(layer, locale = "en") {
  zh <- identical(as.character(locale)[1], "zh-TW")
  ly <- as.character(layer %||% "")[1]
  if (identical(ly, "main")) {
    if (zh) "主選單" else "Main menu"
  } else if (identical(ly, "sub")) {
    if (zh) "子選項" else "Sub-options"
  } else {
    ly
  }
}

#' Resolve store uid for an admin apply-target (all users vs selected user).
ynow_access_scope_store_uid <- function(scope = "user", user_id = "") {
  scope <- as.character(scope %||% "user")[1]
  if (identical(scope, "all")) {
    return(as.character(YNOW_ACCESS_ALL_USERS)[1])
  }
  as.character(user_id %||% "")[1]
}

#' Item ids already restricted under the selected apply-target (raw store, not merged).
#' @param user_id character scalar or vector (multi-select); ignored when scope=all
ynow_access_restricted_ids_for_scope <- function(user_id = "", scope = "user",
                                                 multi_mode = c("union", "intersect")) {
  multi_mode <- match.arg(multi_mode)
  scope <- as.character(scope %||% "user")[1]
  if (identical(scope, "all")) {
    uid <- ynow_access_scope_store_uid("all", "")
    ent <- ynow_access_store_entries(uid)
    if (!is.data.frame(ent) || nrow(ent) < 1L) return(character())
    return(sort(unique(as.character(ent$id))))
  }
  uids <- unique(as.character(user_id %||% character()))
  uids <- uids[nzchar(uids)]
  if (!length(uids)) return(character())
  id_lists <- lapply(uids, function(u) {
    ent <- ynow_access_store_entries(u)
    if (!is.data.frame(ent) || nrow(ent) < 1L) character() else as.character(ent$id)
  })
  out <- if (identical(multi_mode, "intersect")) {
    if (!length(id_lists)) character() else Reduce(intersect, id_lists)
  } else {
    unique(unlist(id_lists, use.names = FALSE))
  }
  sort(unique(as.character(out)))
}

#' Catalog ids still restrictable under the selected apply-target.
#' For multi-selected users (scope=user): intersection — only items none already have.
ynow_access_restrictable_ids_for_scope <- function(user_id = "", scope = "user") {
  cat <- ynow_access_catalog()
  restricted <- ynow_access_restricted_ids_for_scope(
    user_id, scope, multi_mode = if (identical(scope, "all")) "union" else "union"
  )
  # For apply with multi users we want ids restricted by *any* selected user excluded
  # (intersection of restrictable ≡ catalog minus union of restricted).
  setdiff(as.character(cat$id), restricted)
}

#' Build labeled select choices (optgrouped Main menu / Sub-options when group=TRUE).
ynow_access_item_choice_list <- function(ids, locale = "en", group = TRUE) {
  ids <- unique(as.character(ids %||% character()))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) return(character())
  cat <- ynow_access_catalog()
  hit <- cat[cat$id %in% ids, , drop = FALSE]
  if (!nrow(hit)) return(character())
  # Preserve catalog order.
  hit <- hit[order(match(hit$id, cat$id)), , drop = FALSE]
  labs <- paste0(
    vapply(hit$kind, function(k) ynow_access_kind_label(k, locale), character(1)),
    " · ",
    if (identical(as.character(locale)[1], "zh-TW")) hit$label_zh else hit$label_en
  )
  named <- stats::setNames(as.character(hit$id), labs)
  if (!isTRUE(group)) return(named)
  layers <- vapply(hit$kind, ynow_access_menu_layer, character(1))
  main_ids <- named[layers == "main"]
  sub_ids <- named[layers == "sub"]
  out <- list()
  if (length(main_ids)) {
    out[[ynow_access_menu_layer_label("main", locale)]] <- main_ids
  }
  if (length(sub_ids)) {
    out[[ynow_access_menu_layer_label("sub", locale)]] <- sub_ids
  }
  if (!length(out)) return(character())
  out
}

ynow_access_effect_label <- function(effect, locale = "en") {
  zh <- identical(as.character(locale)[1], "zh-TW")
  e <- ynow_access_normalize_effect(effect)
  if (zh) {
    switch(e, lock = "鎖上（顯示鎖定）", hide = "隱藏", e)
  } else {
    switch(e, lock = "Lock (show locked)", hide = "Hide", e)
  }
}

ynow_access_normalize_effect <- function(effect) {
  e <- tolower(as.character(effect %||% "hide")[1])
  if (!nzchar(e) || !e %in% .YNOW_ACCESS_EFFECTS) "hide" else e
}

ynow_access_normalize_unlock_type <- function(unlock_type, allow_empty = TRUE) {
  u <- tolower(trimws(as.character(unlock_type %||% "")[1]))
  if (!nzchar(u)) {
    if (isTRUE(allow_empty)) return(NA_character_)
    return(YNOW_UNLOCK_LOGIN)
  }
  if (u %in% .YNOW_UNLOCK_TYPES) u else NA_character_
}

ynow_access_normalize_nonneg_int <- function(x, default = NA_integer_) {
  n <- suppressWarnings(as.integer(x)[1])
  if (!is.finite(n) || is.na(n) || n < 0L) {
    as.integer(default)
  } else {
    as.integer(n)
  }
}

#' Normalize one lock entry (effect + unlock condition fields).
ynow_access_normalize_entry <- function(raw = NULL,
                                        effect = "hide",
                                        unlock_type = NA_character_,
                                        points_cost = NA_integer_,
                                        free_count = NA_integer_,
                                        time_limit_hours = NA_integer_) {
  if (is.list(raw) && !is.null(raw$effect)) {
    effect <- raw$effect
    unlock_type <- raw$unlock_type %||% unlock_type
    points_cost <- raw$points_cost %||% points_cost
    free_count <- raw$free_count %||% free_count
    time_limit_hours <- raw$time_limit_hours %||% time_limit_hours
  } else if (!is.null(raw) && (is.character(raw) || is.numeric(raw))) {
    effect <- raw
  }
  ut <- ynow_access_normalize_unlock_type(unlock_type, allow_empty = TRUE)
  pc <- ynow_access_normalize_nonneg_int(points_cost, NA_integer_)
  fc <- ynow_access_normalize_nonneg_int(free_count, NA_integer_)
  th <- ynow_access_normalize_nonneg_int(time_limit_hours, NA_integer_)
  if (identical(ut, YNOW_UNLOCK_POINTS)) {
    if (is.na(pc)) pc <- 1L
  } else {
    pc <- NA_integer_
  }
  if (!identical(ut, YNOW_UNLOCK_CONDITIONAL)) {
    fc <- NA_integer_
    th <- NA_integer_
  } else {
    # 0 means "unused" in the admin form — store as NA.
    if (!is.na(fc) && fc <= 0L) fc <- NA_integer_
    if (!is.na(th) && th <= 0L) th <- NA_integer_
  }
  list(
    effect = ynow_access_normalize_effect(effect),
    unlock_type = ut,
    points_cost = pc,
    free_count = fc,
    time_limit_hours = th
  )
}

ynow_access_unlock_type_label <- function(unlock_type, locale = "en",
                                          points_cost = NA_integer_,
                                          free_count = NA_integer_,
                                          time_limit_hours = NA_integer_) {
  zh <- identical(as.character(locale)[1], "zh-TW")
  u <- ynow_access_normalize_unlock_type(unlock_type, allow_empty = TRUE)
  if (is.na(u) || !nzchar(u)) {
    return(if (zh) "管理員清除前維持鎖定" else "Sticky until admin clears")
  }
  if (identical(u, YNOW_UNLOCK_LOGIN)) {
    return(if (zh) "登入即解鎖" else "Unlock after login")
  }
  if (identical(u, YNOW_UNLOCK_POINTS)) {
    pc <- ynow_access_normalize_nonneg_int(points_cost, 1L)
    if (zh) {
      sprintf("點數解鎖（%s FYN）", as.character(pc))
    } else {
      sprintf("Points unlock (%s FYN)", as.character(pc))
    }
  } else if (identical(u, YNOW_UNLOCK_CONDITIONAL)) {
    bits <- character()
    fc <- ynow_access_normalize_nonneg_int(free_count, NA_integer_)
    th <- ynow_access_normalize_nonneg_int(time_limit_hours, NA_integer_)
    if (!is.na(fc)) {
      bits <- c(bits, if (zh) sprintf("免費 %s 次", fc) else sprintf("%s free uses", fc))
    }
    if (!is.na(th) && th > 0L) {
      bits <- c(bits, if (zh) sprintf("限時 %s 小時", th) else sprintf("%s h window", th))
    }
    detail <- if (length(bits)) paste(bits, collapse = if (zh) "／" else " / ") else {
      if (zh) "未設定免費次數或限時" else "no free count or time window"
    }
    if (zh) paste0("條件解鎖（", detail, "）") else paste0("Conditional unlock (", detail, ")")
  } else {
    as.character(u)
  }
}

#' Map admin unlock_type to freemium-style class letter.
#' A = unlock-after-login; B = unlock-after-login-and-points; C = conditional.
ynow_access_unlock_class <- function(unlock_type) {
  u <- ynow_access_normalize_unlock_type(unlock_type, allow_empty = TRUE)
  if (identical(u, YNOW_UNLOCK_LOGIN)) return("A")
  if (identical(u, YNOW_UNLOCK_POINTS)) return("B")
  if (identical(u, YNOW_UNLOCK_CONDITIONAL)) return("C")
  NA_character_
}

ynow_access_reset_memory <- function() {
  .ynow_access_mem$by_user <- list()
  .ynow_access_mem$unlock_state <- list()
  invisible(TRUE)
}

#' Remove one user's access locks from memory + file (never touches `__all__`).
ynow_access_purge_user <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(invisible(FALSE))
  if (identical(uid, YNOW_ACCESS_ALL_USERS)) return(invisible(FALSE))
  if (!is.null(.ynow_access_mem$by_user[[uid]])) {
    .ynow_access_mem$by_user[[uid]] <- NULL
  }
  store <- tryCatch(.ynow_access_read_file(), error = function(e) list())
  if (is.list(store) && !is.null(store[[uid]])) {
    store[[uid]] <- NULL
    tryCatch(.ynow_access_write_file(store), error = function(e) NULL)
  }
  if (!is.null(.ynow_access_mem$unlock_state[[uid]])) {
    .ynow_access_mem$unlock_state[[uid]] <- NULL
  }
  state_store <- tryCatch(.ynow_access_read_state_file(), error = function(e) list())
  if (is.list(state_store) && !is.null(state_store[[uid]])) {
    state_store[[uid]] <- NULL
    tryCatch(.ynow_access_write_state_file(state_store), error = function(e) NULL)
  }
  invisible(TRUE)
}

.ynow_access_file <- function() {
  override <- Sys.getenv("YNOW_ACCESS_LOCKS_PATH", "")
  if (nzchar(override)) return(override)
  file.path("data", "access_locks.json")
}

.ynow_access_state_file <- function() {
  override <- Sys.getenv("YNOW_ACCESS_UNLOCK_STATE_PATH", "")
  if (nzchar(override)) return(override)
  file.path("data", "access_unlock_state.json")
}

#' Normalize one user's stored map to named list of entries.
.ynow_access_coerce_map <- function(raw) {
  if (is.null(raw)) return(list())
  # Named character vector of effects (legacy simple form)
  if (is.character(raw) && !is.null(names(raw)) && any(nzchar(names(raw)))) {
    out <- lapply(raw, function(v) ynow_access_normalize_entry(effect = v))
    names(out) <- names(raw)
    out <- out[nzchar(names(out))]
    return(out)
  }
  if (is.list(raw) || is.character(raw)) {
    # Named list of effects or full entries
    if (is.list(raw) && !is.null(names(raw)) && any(nzchar(names(raw)))) {
      nms <- names(raw)
      keep <- nzchar(nms)
      vals <- lapply(raw[keep], function(v) {
        if (is.list(v) && !is.null(v$effect)) {
          ynow_access_normalize_entry(raw = v)
        } else {
          ynow_access_normalize_entry(effect = v)
        }
      })
      names(vals) <- nms[keep]
      return(vals)
    }
    # Legacy: plain list/vector of item ids → hide (matches prior CSS)
    ids <- as.character(unlist(raw, use.names = FALSE))
    ids <- ids[nzchar(ids)]
    if (!length(ids)) return(list())
    out <- lapply(ids, function(id) ynow_access_normalize_entry(effect = "hide"))
    names(out) <- ids
    return(out)
  }
  list()
}

.ynow_access_entry_to_jsonable <- function(entry) {
  e <- ynow_access_normalize_entry(raw = entry)
  out <- list(effect = e$effect)
  if (!is.na(e$unlock_type) && nzchar(e$unlock_type)) {
    out$unlock_type <- e$unlock_type
  }
  if (!is.na(e$points_cost)) out$points_cost <- as.integer(e$points_cost)
  if (!is.na(e$free_count)) out$free_count <- as.integer(e$free_count)
  if (!is.na(e$time_limit_hours)) out$time_limit_hours <- as.integer(e$time_limit_hours)
  out
}

.ynow_access_read_file <- function() {
  path <- .ynow_access_file()
  if (!file.exists(path)) return(list())
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(raw)) return(list())
  raw
}

.ynow_access_write_file <- function(store) {
  path <- .ynow_access_file()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(store, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

.ynow_access_read_state_file <- function() {
  path <- .ynow_access_state_file()
  if (!file.exists(path)) return(list())
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(raw)) return(list())
  raw
}

.ynow_access_write_state_file <- function(store) {
  path <- .ynow_access_state_file()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(store, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

.ynow_access_db_on <- function() {
  exists("ynow_db_ready", mode = "function") &&
    isTRUE(ynow_db_ready()) &&
    exists("ynow_db_with", mode = "function") &&
    !identical(Sys.getenv("YNOW_ACCESS_LOCKS_FORCE_FILE", ""), "1")
}

ynow_access_ensure_schema <- function() {
  if (!.ynow_access_db_on()) return(invisible(FALSE))
  ynow_db_with(function(con) {
    DBI::dbExecute(con, paste(
      "CREATE TABLE IF NOT EXISTS ynow_access_locks (",
      "user_id TEXT NOT NULL,",
      "item_id TEXT NOT NULL,",
      "locked BOOLEAN NOT NULL DEFAULT TRUE,",
      "effect TEXT NOT NULL DEFAULT 'hide',",
      "unlock_type TEXT,",
      "points_cost INTEGER,",
      "free_count INTEGER,",
      "time_limit_hours INTEGER,",
      "updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),",
      "PRIMARY KEY (user_id, item_id)",
      ")"
    ))
    cols <- tryCatch(
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT column_name FROM information_schema.columns",
          "WHERE table_name = 'ynow_access_locks'"
        )
      ),
      error = function(e) NULL
    )
    if (is.data.frame(cols)) {
      if (!"effect" %in% cols$column_name) {
        tryCatch(
          DBI::dbExecute(
            con,
            "ALTER TABLE ynow_access_locks ADD COLUMN IF NOT EXISTS effect TEXT NOT NULL DEFAULT 'hide'"
          ),
          error = function(e) NULL
        )
      }
      for (col_sql in c(
        "unlock_type TEXT",
        "points_cost INTEGER",
        "free_count INTEGER",
        "time_limit_hours INTEGER"
      )) {
        col_name <- sub(" .*", "", col_sql)
        if (!col_name %in% cols$column_name) {
          tryCatch(
            DBI::dbExecute(
              con,
              paste0("ALTER TABLE ynow_access_locks ADD COLUMN IF NOT EXISTS ", col_sql)
            ),
            error = function(e) NULL
          )
        }
      }
    }
    DBI::dbExecute(con, paste(
      "CREATE TABLE IF NOT EXISTS ynow_access_unlock_state (",
      "user_id TEXT NOT NULL,",
      "item_id TEXT NOT NULL,",
      "free_used INTEGER NOT NULL DEFAULT 0,",
      "points_unlocked BOOLEAN NOT NULL DEFAULT FALSE,",
      "unlocked_until TIMESTAMPTZ,",
      "updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),",
      "PRIMARY KEY (user_id, item_id)",
      ")"
    ))
    if (exists(".ynow_db_rls_fail_closed", mode = "function")) {
      tryCatch(.ynow_db_rls_fail_closed(con, "ynow_access_locks"), error = function(e) NULL)
      tryCatch(.ynow_db_rls_fail_closed(con, "ynow_access_unlock_state"), error = function(e) NULL)
    }
  })
  invisible(TRUE)
}

.ynow_access_df_to_map <- function(df) {
  if (!is.data.frame(df) || nrow(df) < 1L) return(list())
  out <- lapply(seq_len(nrow(df)), function(i) {
    ynow_access_normalize_entry(
      effect = df$effect[[i]],
      unlock_type = if ("unlock_type" %in% names(df)) df$unlock_type[[i]] else NA_character_,
      points_cost = if ("points_cost" %in% names(df)) df$points_cost[[i]] else NA_integer_,
      free_count = if ("free_count" %in% names(df)) df$free_count[[i]] else NA_integer_,
      time_limit_hours = if ("time_limit_hours" %in% names(df)) df$time_limit_hours[[i]] else NA_integer_
    )
  })
  names(out) <- as.character(df$item_id)
  out
}

.ynow_access_map_for_uid <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(list())
  if (.ynow_access_db_on()) {
    ynow_access_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT item_id,",
          "COALESCE(NULLIF(effect, ''), 'hide') AS effect,",
          "unlock_type, points_cost, free_count, time_limit_hours",
          "FROM ynow_access_locks",
          "WHERE user_id = $1 AND locked IS TRUE",
          "ORDER BY item_id"
        ),
        params = list(uid)
      )
    })
    return(.ynow_access_df_to_map(df))
  }
  mem <- .ynow_access_mem$by_user[[uid]]
  if (is.null(mem)) {
    store <- .ynow_access_read_file()
    mem <- store[[uid]]
  }
  .ynow_access_coerce_map(mem)
}

.ynow_access_merge_maps <- function(global, personal) {
  merged <- global
  if (length(personal)) {
    for (nm in names(personal)) merged[[nm]] <- personal[[nm]]
  }
  merged
}

.ynow_access_map_to_df <- function(mp) {
  if (!length(mp)) {
    return(data.frame(
      id = character(),
      effect = character(),
      unlock_type = character(),
      points_cost = integer(),
      free_count = integer(),
      time_limit_hours = integer(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    id = names(mp),
    effect = vapply(mp, function(e) e$effect, character(1)),
    unlock_type = vapply(mp, function(e) {
      u <- e$unlock_type
      if (is.null(u) || length(u) < 1L || is.na(u)) NA_character_ else as.character(u)[1]
    }, character(1)),
    points_cost = vapply(mp, function(e) {
      n <- e$points_cost
      if (is.null(n) || length(n) < 1L || is.na(n)) NA_integer_ else as.integer(n)[1]
    }, integer(1)),
    free_count = vapply(mp, function(e) {
      n <- e$free_count
      if (is.null(n) || length(n) < 1L || is.na(n)) NA_integer_ else as.integer(n)[1]
    }, integer(1)),
    time_limit_hours = vapply(mp, function(e) {
      n <- e$time_limit_hours
      if (is.null(n) || length(n) < 1L || is.na(n)) NA_integer_ else as.integer(n)[1]
    }, integer(1)),
    stringsAsFactors = FALSE
  )
}

#' Effective restrictions for a subject: global (__all__) then per-user override.
#' Returns raw configured restrictions (does not apply unlock evaluation).
ynow_access_entries <- function(user_id = "") {
  uid <- as.character(user_id %||% "")[1]
  global <- .ynow_access_map_for_uid(YNOW_ACCESS_ALL_USERS)
  personal <- if (nzchar(uid) && !identical(uid, YNOW_ACCESS_ALL_USERS)) {
    .ynow_access_map_for_uid(uid)
  } else {
    list()
  }
  .ynow_access_map_to_df(.ynow_access_merge_maps(global, personal))
}

#' Raw store map for one uid key (not merged with __all__).
ynow_access_store_entries <- function(user_id = "") {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) {
    return(.ynow_access_map_to_df(list()))
  }
  .ynow_access_map_to_df(.ynow_access_map_for_uid(uid))
}

ynow_access_item_entry <- function(user_id, item_id) {
  id <- as.character(item_id %||% "")[1]
  ent <- ynow_access_entries(user_id)
  hit <- which(ent$id == id)
  if (!length(hit)) return(NULL)
  i <- hit[[1]]
  ynow_access_normalize_entry(
    effect = ent$effect[[i]],
    unlock_type = ent$unlock_type[[i]],
    points_cost = ent$points_cost[[i]],
    free_count = ent$free_count[[i]],
    time_limit_hours = ent$time_limit_hours[[i]]
  )
}

# --- Per-user unlock runtime state (points / conditional) ---

.ynow_access_parse_until <- function(unlocked_until) {
  if (inherits(unlocked_until, "POSIXt") && length(unlocked_until) >= 1L &&
      !is.na(unlocked_until[1])) {
    return(as.POSIXct(unlocked_until[1], tz = "UTC"))
  }
  if (is.character(unlocked_until) && nzchar(unlocked_until[1])) {
    raw <- as.character(unlocked_until[1])
    parsed <- tryCatch(
      as.POSIXct(raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      error = function(e) NA
    )
    if (inherits(parsed, "POSIXt") && length(parsed) >= 1L && !is.na(parsed[1])) {
      return(as.POSIXct(parsed[1], tz = "UTC"))
    }
    parsed <- tryCatch(
      as.POSIXct(raw, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      error = function(e) NA
    )
    if (inherits(parsed, "POSIXt") && length(parsed) >= 1L && !is.na(parsed[1])) {
      return(as.POSIXct(parsed[1], tz = "UTC"))
    }
  }
  NA
}

.ynow_access_normalize_state <- function(raw = NULL) {
  if (!is.list(raw)) raw <- list()
  free_used <- ynow_access_normalize_nonneg_int(raw$free_used %||% 0L, 0L)
  points_unlocked <- isTRUE(raw$points_unlocked)
  unlocked_until <- .ynow_access_parse_until(raw$unlocked_until %||% NA)
  list(
    free_used = free_used,
    points_unlocked = points_unlocked,
    unlocked_until = unlocked_until
  )
}

ynow_access_get_unlock_state <- function(user_id, item_id) {
  uid <- as.character(user_id %||% "")[1]
  id <- as.character(item_id %||% "")[1]
  if (!nzchar(uid) || !nzchar(id) || identical(uid, YNOW_ACCESS_ALL_USERS)) {
    return(.ynow_access_normalize_state())
  }
  if (.ynow_access_db_on()) {
    ynow_access_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT free_used, points_unlocked, unlocked_until",
          "FROM ynow_access_unlock_state",
          "WHERE user_id = $1 AND item_id = $2 LIMIT 1"
        ),
        params = list(uid, id)
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) {
      return(.ynow_access_normalize_state(as.list(df[1, , drop = FALSE])))
    }
    return(.ynow_access_normalize_state())
  }
  mem_u <- .ynow_access_mem$unlock_state[[uid]]
  if (is.null(mem_u)) {
    store <- .ynow_access_read_state_file()
    mem_u <- store[[uid]]
    .ynow_access_mem$unlock_state[[uid]] <- mem_u %||% list()
  }
  .ynow_access_normalize_state(mem_u[[id]])
}

ynow_access_set_unlock_state <- function(user_id, item_id, state) {
  uid <- as.character(user_id %||% "")[1]
  id <- as.character(item_id %||% "")[1]
  if (!nzchar(uid) || !nzchar(id) || identical(uid, YNOW_ACCESS_ALL_USERS)) {
    return(invisible(FALSE))
  }
  st <- .ynow_access_normalize_state(state)
  until_sql <- if (inherits(st$unlocked_until, "POSIXt") && !is.na(st$unlocked_until)) {
    format(as.POSIXct(st$unlocked_until, tz = "UTC"), "%Y-%m-%d %H:%M:%S", usetz = FALSE)
  } else {
    NA_character_
  }
  if (.ynow_access_db_on()) {
    ynow_access_ensure_schema()
    ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO ynow_access_unlock_state",
          "(user_id, item_id, free_used, points_unlocked, unlocked_until, updated_at)",
          "VALUES ($1, $2, $3, $4, $5::timestamptz, NOW())",
          "ON CONFLICT (user_id, item_id) DO UPDATE SET",
          "free_used = EXCLUDED.free_used,",
          "points_unlocked = EXCLUDED.points_unlocked,",
          "unlocked_until = EXCLUDED.unlocked_until,",
          "updated_at = NOW()"
        ),
        params = list(uid, id, as.integer(st$free_used), isTRUE(st$points_unlocked), until_sql)
      )
    })
    return(invisible(TRUE))
  }
  mem_u <- .ynow_access_mem$unlock_state[[uid]] %||% list()
  mem_u[[id]] <- list(
    free_used = as.integer(st$free_used),
    points_unlocked = isTRUE(st$points_unlocked),
    unlocked_until = if (inherits(st$unlocked_until, "POSIXt") && !is.na(st$unlocked_until)) {
      format(as.POSIXct(st$unlocked_until, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    } else {
      NULL
    }
  )
  .ynow_access_mem$unlock_state[[uid]] <- mem_u
  store <- .ynow_access_read_state_file()
  store[[uid]] <- mem_u
  .ynow_access_write_state_file(store)
  invisible(TRUE)
}

#' Whether the unlock condition is currently satisfied for user+item (usable).
#' Returns TRUE when there is no restriction on that item.
ynow_access_unlock_satisfied_for_item <- function(user_id, item_id,
                                                  authed = NULL,
                                                  now = Sys.time()) {
  id <- as.character(item_id %||% "")[1]
  entry <- ynow_access_item_entry(user_id, id)
  if (is.null(entry)) return(TRUE) # not restricted
  uid <- as.character(user_id %||% "")[1]
  is_authed <- if (is.null(authed)) {
    nzchar(uid) && !identical(uid, YNOW_ACCESS_ALL_USERS)
  } else {
    isTRUE(authed)
  }
  ut <- entry$unlock_type
  if (is.na(ut) || !nzchar(ut)) return(FALSE) # legacy sticky
  if (!is_authed) return(FALSE)
  if (identical(ut, YNOW_UNLOCK_LOGIN)) return(TRUE)
  st <- ynow_access_get_unlock_state(uid, id)
  if (identical(ut, YNOW_UNLOCK_POINTS)) return(isTRUE(st$points_unlocked))
  if (identical(ut, YNOW_UNLOCK_CONDITIONAL)) {
    has_free <- !is.na(entry$free_count)
    has_time <- !is.na(entry$time_limit_hours) && entry$time_limit_hours > 0L
    ok_free <- FALSE
    ok_time <- FALSE
    if (has_free) ok_free <- as.integer(st$free_used) < as.integer(entry$free_count)
    if (has_time) {
      until <- st$unlocked_until
      ok_time <- inherits(until, "POSIXt") && !is.na(until) &&
        as.POSIXct(until) > as.POSIXct(now)
    }
    if (has_free || has_time) return(isTRUE(ok_free) || isTRUE(ok_time))
    return(FALSE)
  }
  FALSE
}

#' Restrictions still in force after unlock evaluation (for client push / deny).
ynow_access_effective_entries <- function(user_id = "", authed = NULL, now = Sys.time()) {
  ent <- ynow_access_entries(user_id)
  if (!is.data.frame(ent) || nrow(ent) < 1L) return(ent)
  uid <- as.character(user_id %||% "")[1]
  is_authed <- if (is.null(authed)) nzchar(uid) && !identical(uid, YNOW_ACCESS_ALL_USERS) else isTRUE(authed)
  keep <- vapply(seq_len(nrow(ent)), function(i) {
    !isTRUE(ynow_access_unlock_satisfied_for_item(
      uid, ent$id[[i]], authed = is_authed, now = now
    ))
  }, logical(1))
  ent[keep, , drop = FALSE]
}

#' Mark points unlock paid for one user+item (after FYN spend succeeds).
ynow_access_mark_points_unlocked <- function(user_id, item_id) {
  st <- ynow_access_get_unlock_state(user_id, item_id)
  st$points_unlocked <- TRUE
  ynow_access_set_unlock_state(user_id, item_id, st)
  invisible(TRUE)
}

#' Format ISO-8601 UTC end time for client countdown badges.
ynow_access_ends_at_iso <- function(unlocked_until) {
  until <- .ynow_access_parse_until(unlocked_until)
  if (!inherits(until, "POSIXt") || is.na(until)) return(NA_character_)
  format(as.POSIXct(until, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Client payload rows for Class C free-count / time-window badges.
#'
#' Includes configured conditional restrictions even while currently unlocked
#' (so remaining/total stays visible). Guests see total with remaining = total
#' when no per-user state exists.
#'
#' @return list of lists: id, remaining, total, ends_at (ISO or NA)
ynow_access_quota_badges <- function(user_id = "", authed = NULL, now = Sys.time()) {
  ent <- ynow_access_entries(user_id)
  if (!is.data.frame(ent) || nrow(ent) < 1L) return(list())
  uid <- as.character(user_id %||% "")[1]
  is_authed <- if (is.null(authed)) {
    nzchar(uid) && !identical(uid, YNOW_ACCESS_ALL_USERS)
  } else {
    isTRUE(authed)
  }
  now <- as.POSIXct(now, tz = "UTC")
  out <- list()
  for (i in seq_len(nrow(ent))) {
    ut <- as.character(ent$unlock_type[[i]] %||% "")[1]
    if (!identical(ut, YNOW_UNLOCK_CONDITIONAL)) next
    id <- as.character(ent$id[[i]])[1]
    if (!nzchar(id)) next
    fc <- suppressWarnings(as.integer(ent$free_count[[i]])[1])
    th <- suppressWarnings(as.integer(ent$time_limit_hours[[i]])[1])
    has_free <- is.finite(fc) && !is.na(fc) && fc > 0L
    has_time <- is.finite(th) && !is.na(th) && th > 0L
    if (!has_free && !has_time) next
    remaining <- NA_integer_
    total <- NA_integer_
    ends_at <- NA_character_
    st <- if (is_authed && nzchar(uid) && !identical(uid, YNOW_ACCESS_ALL_USERS)) {
      ynow_access_get_unlock_state(uid, id)
    } else {
      .ynow_access_normalize_state()
    }
    if (has_free) {
      total <- as.integer(fc)
      used <- as.integer(st$free_used %||% 0L)[1]
      if (!is.finite(used) || used < 0L) used <- 0L
      remaining <- max(0L, total - used)
    }
    if (has_time) {
      until <- st$unlocked_until
      if (inherits(until, "POSIXt") && !is.na(until) && as.POSIXct(until) > now) {
        ends_at <- ynow_access_ends_at_iso(until)
      }
    }
    out[[length(out) + 1L]] <- list(
      id = id,
      remaining = if (is.na(remaining)) NULL else remaining,
      total = if (is.na(total)) NULL else total,
      ends_at = if (is.na(ends_at) || !nzchar(ends_at)) NULL else ends_at
    )
  }
  out
}

#' Start / refresh conditional time window; optionally consume one free use.
#' @param consume_free TRUE to increment free_used by 1 when free_count is set
ynow_access_touch_conditional <- function(user_id, item_id, consume_free = FALSE, now = Sys.time()) {
  entry <- ynow_access_item_entry(user_id, item_id)
  if (is.null(entry) || !identical(entry$unlock_type, YNOW_UNLOCK_CONDITIONAL)) {
    return(list(ok = FALSE, message_key = "access_bad_item"))
  }
  st <- ynow_access_get_unlock_state(user_id, item_id)
  if (isTRUE(consume_free) && !is.na(entry$free_count)) {
    st$free_used <- as.integer(st$free_used) + 1L
  }
  if (!is.na(entry$time_limit_hours) && entry$time_limit_hours > 0L) {
    st$unlocked_until <- as.POSIXct(now, tz = "UTC") + (as.integer(entry$time_limit_hours) * 3600)
  }
  ynow_access_set_unlock_state(user_id, item_id, st)
  list(ok = TRUE, state = st)
}

#' Admin 「目前限制」 lines for the current apply-scope + selected user.
#'
#' When scope is "user" / selected user: personal store for that uid (not stale
#' first-row / not global-only). When scope is "all": only the __all__ store.
#' @return character vector of "scope · label（effect · unlock）" lines (may be empty)
ynow_access_admin_status_lines <- function(user_id = "",
                                           scope = "user",
                                           locale = "en",
                                           user_scope_lab = "User",
                                           all_scope_lab = "All") {
  scope <- as.character(scope %||% "user")[1]
  if (!nzchar(scope)) scope <- "user"
  all_uid <- if (exists("YNOW_ACCESS_ALL_USERS", inherits = TRUE)) {
    as.character(get("YNOW_ACCESS_ALL_USERS", inherits = TRUE))[1]
  } else {
    "__all__"
  }
  .fmt <- function(ent, scope_lab) {
    if (!is.data.frame(ent) || nrow(ent) < 1L) return(character())
    vapply(seq_len(nrow(ent)), function(i) {
      lab <- ynow_access_label(ent$id[[i]], locale)
      eff <- ynow_access_effect_label(ent$effect[[i]], locale)
      unl <- ynow_access_unlock_type_label(
        ent$unlock_type[[i]],
        locale = locale,
        points_cost = ent$points_cost[[i]],
        free_count = ent$free_count[[i]],
        time_limit_hours = ent$time_limit_hours[[i]]
      )
      paste0(scope_lab, " · ", lab, "（", eff, " · ", unl, "）")
    }, character(1))
  }
  if (identical(scope, "all")) {
    return(.fmt(ynow_access_store_entries(all_uid), all_scope_lab))
  }
  uid <- as.character(user_id %||% "")[1]
  .fmt(ynow_access_store_entries(uid), user_scope_lab)
}

#' Collapse admin status lines into the UI paragraph text.
ynow_access_admin_status_text <- function(user_id = "",
                                          scope = "user",
                                          locale = "en",
                                          user_scope_lab = "User",
                                          all_scope_lab = "All",
                                          none_lab = "Nothing is restricted for this scope.") {
  scope <- as.character(scope %||% "user")[1]
  uids <- unique(as.character(user_id %||% character()))
  uids <- uids[nzchar(uids)]
  # Multi-select (scope=user): one status block per selected uid.
  if (identical(scope, "user") && length(uids) > 1L) {
    zh <- identical(as.character(locale)[1], "zh-TW")
    blocks <- vapply(uids, function(u) {
      lines <- ynow_access_admin_status_lines(
        user_id = u,
        scope = "user",
        locale = locale,
        user_scope_lab = user_scope_lab,
        all_scope_lab = all_scope_lab
      )
      body <- if (!length(lines)) {
        as.character(none_lab)[1]
      } else {
        sep <- if (zh) "、" else "; "
        paste(lines, collapse = sep)
      }
      paste0(u, ": ", body)
    }, character(1))
    return(paste(blocks, collapse = if (zh) " ｜ " else " | "))
  }
  lines <- ynow_access_admin_status_lines(
    user_id = if (length(uids)) uids[[1]] else "",
    scope = scope,
    locale = locale,
    user_scope_lab = user_scope_lab,
    all_scope_lab = all_scope_lab
  )
  if (!length(lines)) return(as.character(none_lab)[1])
  sep <- if (identical(as.character(locale)[1], "zh-TW")) "、" else "; "
  paste(lines, collapse = sep)
}

ynow_access_locked_ids <- function(user_id, effective = FALSE, authed = NULL) {
  ent <- if (isTRUE(effective)) {
    ynow_access_effective_entries(user_id, authed = authed)
  } else {
    ynow_access_entries(user_id)
  }
  sort(unique(as.character(ent$id)))
}

ynow_access_is_locked <- function(user_id, item_id, effective = FALSE, authed = NULL) {
  id <- as.character(item_id %||% "")[1]
  id %in% ynow_access_locked_ids(user_id, effective = effective, authed = authed)
}

ynow_access_item_effect <- function(user_id, item_id) {
  id <- as.character(item_id %||% "")[1]
  ent <- ynow_access_entries(user_id)
  hit <- which(ent$id == id)
  if (!length(hit)) return(NA_character_)
  as.character(ent$effect[[hit[[1]]]])
}

#' Set or clear a restriction (with optional unlock condition).
#' @param user_id account uuid, or YNOW_ACCESS_ALL_USERS for everyone
#' @param effect "lock" or "hide" (ignored when locked=FALSE)
#' @param unlock_type "login" | "points" | "conditional" (default login when locking)
#' @param points_cost FYN required when unlock_type=points
#' @param free_count free uses when unlock_type=conditional (optional)
#' @param time_limit_hours hours window when unlock_type=conditional (optional)
ynow_access_set <- function(user_id,
                            item_id,
                            locked = TRUE,
                            effect = "hide",
                            unlock_type = YNOW_UNLOCK_LOGIN,
                            points_cost = NA_integer_,
                            free_count = NA_integer_,
                            time_limit_hours = NA_integer_) {
  uid <- as.character(user_id %||% "")[1]
  id <- as.character(item_id %||% "")[1]
  cat <- ynow_access_catalog()
  if (!nzchar(uid)) return(list(ok = FALSE, message_key = "access_bad_user"))
  if (!id %in% cat$id) return(list(ok = FALSE, message_key = "access_bad_item"))
  on <- isTRUE(locked)
  # Enforce apply/remove invariants against the raw store for this target uid
  # (not merged effective locks): no double-restrict; no remove of missing.
  store_ids <- {
    ent <- ynow_access_store_entries(uid)
    if (!is.data.frame(ent) || nrow(ent) < 1L) character() else as.character(ent$id)
  }
  if (on && id %in% store_ids) {
    return(list(ok = FALSE, message_key = "access_already_restricted"))
  }
  if (!on && !id %in% store_ids) {
    return(list(ok = FALSE, message_key = "access_not_restricted"))
  }
  entry <- ynow_access_normalize_entry(
    effect = effect,
    unlock_type = if (on) {
      ynow_access_normalize_unlock_type(unlock_type, allow_empty = FALSE)
    } else {
      NA_character_
    },
    points_cost = points_cost,
    free_count = free_count,
    time_limit_hours = time_limit_hours
  )
  if (on && identical(entry$unlock_type, YNOW_UNLOCK_POINTS) &&
      (is.na(entry$points_cost) || entry$points_cost < 1L)) {
    return(list(ok = FALSE, message_key = "access_bad_points_cost"))
  }
  if (on && identical(entry$unlock_type, YNOW_UNLOCK_CONDITIONAL)) {
    has_free <- !is.na(entry$free_count)
    has_time <- !is.na(entry$time_limit_hours) && entry$time_limit_hours > 0L
    if (!has_free && !has_time) {
      return(list(ok = FALSE, message_key = "access_bad_conditional"))
    }
  }
  eff <- entry$effect
  if (.ynow_access_db_on()) {
    ynow_access_ensure_schema()
    ynow_db_with(function(con) {
      if (on) {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO ynow_access_locks",
            "(user_id, item_id, locked, effect, unlock_type, points_cost,",
            " free_count, time_limit_hours, updated_at)",
            "VALUES ($1, $2, TRUE, $3, $4, $5, $6, $7, NOW())",
            "ON CONFLICT (user_id, item_id) DO UPDATE SET",
            "locked = TRUE,",
            "effect = EXCLUDED.effect,",
            "unlock_type = EXCLUDED.unlock_type,",
            "points_cost = EXCLUDED.points_cost,",
            "free_count = EXCLUDED.free_count,",
            "time_limit_hours = EXCLUDED.time_limit_hours,",
            "updated_at = NOW()"
          ),
          params = list(
            uid, id, eff,
            if (is.na(entry$unlock_type)) NA_character_ else entry$unlock_type,
            if (is.na(entry$points_cost)) NA_integer_ else as.integer(entry$points_cost),
            if (is.na(entry$free_count)) NA_integer_ else as.integer(entry$free_count),
            if (is.na(entry$time_limit_hours)) NA_integer_ else as.integer(entry$time_limit_hours)
          )
        )
      } else {
        DBI::dbExecute(
          con,
          "DELETE FROM ynow_access_locks WHERE user_id = $1 AND item_id = $2",
          params = list(uid, id)
        )
      }
    })
  } else {
    store <- .ynow_access_read_file()
    cur <- .ynow_access_coerce_map(
      .ynow_access_mem$by_user[[uid]] %||% store[[uid]]
    )
    if (on) {
      cur[[id]] <- entry
    } else {
      cur <- cur[setdiff(names(cur), id)]
    }
    .ynow_access_mem$by_user[[uid]] <- cur
    store[[uid]] <- lapply(cur, .ynow_access_entry_to_jsonable)
    .ynow_access_write_file(store)
  }
  list(
    ok = TRUE,
    message_key = if (on) {
      if (identical(uid, YNOW_ACCESS_ALL_USERS)) {
        if (identical(eff, "lock")) "access_ok_lock_all" else "access_ok_hide_all"
      } else {
        if (identical(eff, "lock")) "access_ok_lock" else "access_ok_hide"
      }
    } else {
      if (identical(uid, YNOW_ACCESS_ALL_USERS)) "access_ok_unlock_all" else "access_ok_unlock"
    },
    ids = ynow_access_locked_ids(uid),
    effect = if (on) eff else NA_character_,
    unlock_type = if (on) entry$unlock_type else NA_character_,
    points_cost = if (on) entry$points_cost else NA_integer_,
    free_count = if (on) entry$free_count else NA_integer_,
    time_limit_hours = if (on) entry$time_limit_hours else NA_integer_,
    scope = if (identical(uid, YNOW_ACCESS_ALL_USERS)) "all" else "user"
  )
}
