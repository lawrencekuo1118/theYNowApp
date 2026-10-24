# ==========================================
# Shared lock-catalog helpers (not a merged domain)
# ==========================================
# membership_locks.R  = product freemium A/B gate catalog
# access_locks.R      = per-user / __all__ sysadmin hide|lock store
#
# Keep those two modules separate. This file only holds:
#   1) catalog row-list → data.frame builder
#   2) bilingual labels for surfaces that appear in BOTH catalogs with
#      identical copy (true duplicates)
#   3) locale pick helper
#
# Intentionally NOT shared (wording differs by product vs ACL job):
#   - download / report labels (guest gate "Download …" vs ACL "… CSV/PDF")
#   - Lab / Value Alerts body surfaces (membership "worksheets/tables" vs
#     ACL "… body")
#   - PDF SKU「估值報告」vs ACL report「估值報表」

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

#' Build a character data.frame from a list of equal-length row vectors.
#' @param rows list of character (or coercible) vectors
#' @param columns character vector of column names (positional)
#' @return data.frame with stringsAsFactors = FALSE
.ynow_lock_catalog_df <- function(rows, columns) {
  columns <- as.character(columns)
  if (!length(columns)) {
    stop(".ynow_lock_catalog_df: columns must be non-empty", call. = FALSE)
  }
  if (!is.list(rows)) {
    stop(".ynow_lock_catalog_df: rows must be a list", call. = FALSE)
  }
  n_col <- length(columns)
  for (i in seq_along(rows)) {
    if (length(rows[[i]]) < n_col) {
      stop(
        ".ynow_lock_catalog_df: row ", i, " has ", length(rows[[i]]),
        " fields; expected ", n_col,
        call. = FALSE
      )
    }
  }
  vals <- lapply(seq_len(n_col), function(j) {
    vapply(rows, function(r) as.character(r[[j]] %||% ""), character(1))
  })
  names(vals) <- columns
  do.call(data.frame, c(vals, list(stringsAsFactors = FALSE, check.names = FALSE)))
}

#' Surfaces with identical en-US + zh-TW copy in membership + access catalogs.
#'
#' Keys are bare stems. Access ids are `kind:key` (e.g. `tab:lab_notes`);
#' membership uses the bare stem for sidebar tabs / related surfaces.
ynow_lock_shared_surface_labels <- function() {
  rows <- list(
    c("lab_notes", "Quant Backtest Lab", "量化回測實驗室"),
    c("decision_checklist", "Decision Checklist", "決策檢核"),
    c("mos_assumptions", "MOS Assumptions", "MOS假設"),
    c("price_alerts", "Value Alerts", "到值通知"),
    # membership surface ynow_chk_body == access section:checklist_body
    c("checklist_body", "Decision Checklist body", "決策檢核內容"),
    c("mos_assumptions_body", "MOS Assumptions body", "MOS假設內容")
  )
  .ynow_lock_catalog_df(
    rows,
    c("key", "label_en", "label_zh_tw")
  )
}

#' Look up a shared surface label.
#' @param key bare stem (lab_notes, decision_checklist, …)
#' @param locale "en" / "en-US" or "zh-TW"
#' @return character(1); empty string when unknown
ynow_lock_shared_label <- function(key, locale = "en") {
  key <- as.character(key %||% "")[1]
  if (!nzchar(key)) return("")
  cat <- ynow_lock_shared_surface_labels()
  hit <- which(cat$key == key)
  if (!length(hit)) return("")
  zh <- identical(as.character(locale)[1], "zh-TW")
  if (zh) cat$label_zh_tw[[hit[[1]]]] else cat$label_en[[hit[[1]]]]
}

#' Both language strings for a shared surface (for catalog row builders).
#' @return list(en = …, zh = …); empty strings when unknown
ynow_lock_shared_label_pair <- function(key) {
  key <- as.character(key %||% "")[1]
  cat <- ynow_lock_shared_surface_labels()
  hit <- which(cat$key == key)
  if (!length(hit)) {
    return(list(en = "", zh = ""))
  }
  list(
    en = as.character(cat$label_en[[hit[[1]]]]),
    zh = as.character(cat$label_zh_tw[[hit[[1]]]])
  )
}

#' Pick en vs zh-TW from a pair of strings (access / membership label helpers).
ynow_lock_pick_locale_label <- function(label_en, label_zh, locale = "en") {
  zh <- identical(as.character(locale)[1], "zh-TW")
  if (zh) {
    as.character(label_zh %||% "")[1]
  } else {
    as.character(label_en %||% "")[1]
  }
}
