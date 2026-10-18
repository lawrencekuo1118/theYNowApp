# Per-user / global UI restrictions set from System admin.
# item id = kind:target  (tab | section | input | report)
# effect  = "lock" (show locked placeholder / disable) | "hide" (remove from UI)
# scope   = selected user id, or YNOW_ACCESS_ALL_USERS for every account (+ guests)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

YNOW_ACCESS_ALL_USERS <- "__all__"
.YNOW_ACCESS_EFFECTS <- c("lock", "hide")

.ynow_access_mem <- new.env(parent = emptyenv())
.ynow_access_mem$by_user <- list()

ynow_access_catalog <- function() {
  rows <- list(
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
    c("tab:lab_notes", "tab", "Quant Backtest Lab", "量化回測實驗室"),
    c("tab:decision_checklist", "tab", "Decision Checklist", "決策檢核"),
    c("tab:price_alerts", "tab", "Value Alerts", "到值通知"),
    c("tab:snapshot", "tab", "Snapshot", "快照"),
    c("section:shared_bridge", "section", "Shared bridge (Cash / Debt / Shares / Revenue)", "共用橋接（Cash／Debt／股數／營收）"),
    c("section:dcf_two_stage", "section", "DCF two-stage assumptions", "DCF 兩階段假設"),
    c("section:snapshot_restore", "section", "Snapshot restore", "快照還原"),
    c("section:value_alerts_body", "section", "Value Alerts body", "到值通知內容"),
    c("section:backtest_body", "section", "Quant Backtest Lab body", "量化回測實驗室內容"),
    c("section:checklist_body", "section", "Decision Checklist body", "決策檢核內容"),
    c("input:bridge_cash", "input", "Cash", "Cash"),
    c("input:bridge_debt", "input", "Total Debt", "Total Debt"),
    c("input:bridge_shares", "input", "Shares", "股數"),
    c("input:bridge_revenue", "input", "Total revenue", "總營收"),
    c("input:sgr", "input", "Terminal g (SGR)", "永續成長率 g（SGR）"),
    c("input:g_stage1", "input", "Near-term g1", "近端成長率 g1"),
    c("input:yr_stage1", "input", "Stage-1 years", "第一階段年數"),
    c("input:wacc_stage1", "input", "WACC1", "WACC1"),
    c("input:wacc_stage2", "input", "WACC2", "WACC2"),
    c("report:download_report", "report", "Valuation report (PDF)", "估值報表（PDF）"),
    c("report:download_snapshot", "report", "Snapshot CSV", "快照 CSV"),
    c("report:download_param_restore", "report", "Parameter restore CSV", "參數還原 CSV"),
    c("report:FS_download", "report", "Finance summary", "財務摘要"),
    c("report:IS_download", "report", "Income statement", "損益表"),
    c("report:BS_download", "report", "Balance sheet", "資產負債表"),
    c("report:CF_download", "report", "Cash flow statement", "現金流量表")
  )
  data.frame(
    id = vapply(rows, function(r) r[[1]], character(1)),
    kind = vapply(rows, function(r) r[[2]], character(1)),
    label_en = vapply(rows, function(r) r[[3]], character(1)),
    label_zh = vapply(rows, function(r) r[[4]], character(1)),
    stringsAsFactors = FALSE
  )
}

ynow_access_label <- function(id, locale = "en") {
  cat <- ynow_access_catalog()
  hit <- which(cat$id == as.character(id)[1])
  if (!length(hit)) return(as.character(id)[1])
  zh <- identical(as.character(locale)[1], "zh-TW")
  if (zh) cat$label_zh[[hit[[1]]]] else cat$label_en[[hit[[1]]]]
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

ynow_access_reset_memory <- function() {
  .ynow_access_mem$by_user <- list()
  invisible(TRUE)
}

.ynow_access_file <- function() {
  override <- Sys.getenv("YNOW_ACCESS_LOCKS_PATH", "")
  if (nzchar(override)) return(override)
  file.path("data", "access_locks.json")
}

#' Normalize one user's stored map to named character vector: item_id -> effect.
.ynow_access_coerce_map <- function(raw) {
  if (is.null(raw)) return(character())
  if (is.character(raw) && !is.null(names(raw)) && any(nzchar(names(raw)))) {
    out <- vapply(raw, ynow_access_normalize_effect, character(1), USE.NAMES = TRUE)
    out <- out[nzchar(names(out))]
    return(out)
  }
  # Legacy: plain list/vector of item ids → hide (matches prior CSS)
  if (is.list(raw) || is.character(raw)) {
    ids <- as.character(unlist(raw, use.names = FALSE))
    ids <- ids[nzchar(ids)]
    if (!length(ids)) return(character())
    # Named list of effects: list(`tab:x` = "lock")
    if (is.list(raw) && !is.null(names(raw)) && any(nzchar(names(raw)))) {
      nms <- names(raw)
      keep <- nzchar(nms)
      vals <- vapply(raw[keep], function(v) {
        if (is.list(v) && !is.null(v$effect)) {
          ynow_access_normalize_effect(v$effect)
        } else {
          ynow_access_normalize_effect(v)
        }
      }, character(1))
      names(vals) <- nms[keep]
      return(vals)
    }
    out <- rep("hide", length(ids))
    names(out) <- ids
    return(out)
  }
  character()
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
      "updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),",
      "PRIMARY KEY (user_id, item_id)",
      ")"
    ))
    # Older deploys may lack effect column.
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
    if (is.data.frame(cols) && !"effect" %in% cols$column_name) {
      tryCatch(
        DBI::dbExecute(
          con,
          "ALTER TABLE ynow_access_locks ADD COLUMN IF NOT EXISTS effect TEXT NOT NULL DEFAULT 'hide'"
        ),
        error = function(e) NULL
      )
    }
  })
  invisible(TRUE)
}

.ynow_access_map_for_uid <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character())
  if (.ynow_access_db_on()) {
    ynow_access_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT item_id, COALESCE(NULLIF(effect, ''), 'hide') AS effect",
          "FROM ynow_access_locks",
          "WHERE user_id = $1 AND locked IS TRUE",
          "ORDER BY item_id"
        ),
        params = list(uid)
      )
    })
    if (!is.data.frame(df) || nrow(df) < 1L) return(character())
    out <- vapply(df$effect, ynow_access_normalize_effect, character(1))
    names(out) <- as.character(df$item_id)
    return(out)
  }
  mem <- .ynow_access_mem$by_user[[uid]]
  if (is.null(mem)) {
    store <- .ynow_access_read_file()
    mem <- store[[uid]]
  }
  .ynow_access_coerce_map(mem)
}

#' Effective restrictions for a subject: global (__all__) then per-user override.
ynow_access_entries <- function(user_id = "") {
  uid <- as.character(user_id %||% "")[1]
  global <- .ynow_access_map_for_uid(YNOW_ACCESS_ALL_USERS)
  personal <- if (nzchar(uid) && !identical(uid, YNOW_ACCESS_ALL_USERS)) {
    .ynow_access_map_for_uid(uid)
  } else {
    character()
  }
  merged <- global
  if (length(personal)) {
    for (nm in names(personal)) merged[[nm]] <- personal[[nm]]
  }
  if (!length(merged)) {
    return(data.frame(id = character(), effect = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    id = names(merged),
    effect = unname(vapply(merged, ynow_access_normalize_effect, character(1))),
    stringsAsFactors = FALSE
  )
}

ynow_access_locked_ids <- function(user_id) {
  ent <- ynow_access_entries(user_id)
  sort(unique(as.character(ent$id)))
}

ynow_access_is_locked <- function(user_id, item_id) {
  id <- as.character(item_id %||% "")[1]
  id %in% ynow_access_locked_ids(user_id)
}

ynow_access_item_effect <- function(user_id, item_id) {
  id <- as.character(item_id %||% "")[1]
  ent <- ynow_access_entries(user_id)
  hit <- which(ent$id == id)
  if (!length(hit)) return(NA_character_)
  as.character(ent$effect[[hit[[1]]]])
}

#' Set or clear a restriction.
#' @param user_id account uuid, or YNOW_ACCESS_ALL_USERS for everyone
#' @param effect "lock" or "hide" (ignored when locked=FALSE)
ynow_access_set <- function(user_id, item_id, locked = TRUE, effect = "hide") {
  uid <- as.character(user_id %||% "")[1]
  id <- as.character(item_id %||% "")[1]
  cat <- ynow_access_catalog()
  if (!nzchar(uid)) return(list(ok = FALSE, message_key = "access_bad_user"))
  if (!id %in% cat$id) return(list(ok = FALSE, message_key = "access_bad_item"))
  on <- isTRUE(locked)
  eff <- ynow_access_normalize_effect(effect)
  if (.ynow_access_db_on()) {
    ynow_access_ensure_schema()
    ynow_db_with(function(con) {
      if (on) {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO ynow_access_locks (user_id, item_id, locked, effect, updated_at)",
            "VALUES ($1, $2, TRUE, $3, NOW())",
            "ON CONFLICT (user_id, item_id) DO UPDATE SET",
            "locked = TRUE, effect = EXCLUDED.effect, updated_at = NOW()"
          ),
          params = list(uid, id, eff)
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
      cur[[id]] <- eff
    } else {
      cur <- cur[setdiff(names(cur), id)]
    }
    .ynow_access_mem$by_user[[uid]] <- cur
    # Persist as named list for JSON clarity
    store[[uid]] <- as.list(cur)
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
    scope = if (identical(uid, YNOW_ACCESS_ALL_USERS)) "all" else "user"
  )
}
