# =========================================================================
# corp_aml_signals.R — Corporate AML / fraud-possibility proxies (statements)
#
# Adapts AML course *method* (typology registry, thresholds, risk tier,
# evidence trail) to public-issuer financial statements. Not bank KYC/txn
# monitoring. Day-window cash ticket rules are out of scope.
#
# Scores are veto / risk screens only — never a buy signal.
# =========================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.AML_THRESHOLDS <- list(
  cash_rev_alert = 0.55,
  cash_rev_watch = 0.35,
  wc_sales_alert = 0.20,
  wc_sales_watch = 0.12,
  other_recv_alert = 0.12,
  other_recv_watch = 0.06,
  cff_assets_alert = 0.25,
  cff_assets_watch = 0.12,
  peer_z_alert = 2.0,
  peer_z_watch = 1.25
)

#' Static typology catalog (en-US + zh-TW). Codes are stable API.
list_corp_aml_typology_catalog <- function() {
  data.frame(
    code = paste0("AML_C", 1:8),
    category = rep("法人洗錢跡象代理", 8L),
    name_zh = c(
      "現金密集度異常",
      "營運現金與獲利背離",
      "應收／存貨循環膨脹",
      "其他應收／關係人應收代理",
      "管轄地／結構風險",
      "融資現金流暴衝",
      "Beneish／Schilit 舞弊疊加",
      "審計師／意見異常"
    ),
    name_en = c(
      "Elevated cash intensity",
      "Operating cash vs earnings divergence",
      "Receivables / inventory cycle inflation",
      "Other / related-party receivables proxy",
      "Jurisdiction / structure risk tier",
      "Financing cash-flow spike",
      "Beneish / Schilit composition",
      "Auditor / opinion anomaly"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.aml_finite <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  length(x) == 1L && is.finite(x)
}

.aml_pick <- function(df, patterns, include_ttm = FALSE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(numeric(0))
  if (!exists("select_clean_metric_row_any", mode = "function")) return(numeric(0))
  v <- tryCatch(
    select_clean_metric_row_any(df, patterns, include_ttm = include_ttm),
    error = function(e) NA
  )
  if (is.null(v) || (length(v) == 1L && is.na(v[1]))) return(numeric(0))
  suppressWarnings(as.numeric(v))
}

.aml_at <- function(v, i) {
  if (is.null(v) || length(v) < i) return(NA_real_)
  suppressWarnings(as.numeric(v[i])[1])
}

.aml_ratio <- function(num, den) {
  if (!.aml_finite(num) || !.aml_finite(den) || den == 0) return(NA_real_)
  num / den
}

.aml_ctx <- function(d_is, d_bs, d_cf) {
  list(
    rev = .aml_pick(d_is, c("^Total Revenue$")),
    ni = .aml_pick(d_is, if (exists("NET_INCOME_PATTERNS")) {
      NET_INCOME_PATTERNS
    } else {
      c("Net Income Common Stockholders", "^Net Income$")
    }),
    ocf = .aml_pick(d_cf, c("^Operating Cash Flow$")),
    cff = .aml_pick(d_cf, c(
      "^Financing Cash Flow$",
      "^Cash Flow From Continuing Financing Activities$"
    )),
    cash = .aml_pick(d_bs, c(
      "^Cash And Cash Equivalents$",
      "^Cash Cash Equivalents And Short Term Investments$",
      "^Cash And Short Term Investments$"
    )),
    ar = .aml_pick(d_bs, c(
      "^Accounts Receivable$",
      "^Gross Accounts Receivable$",
      "Net Accounts Receivable"
    )),
    inv = .aml_pick(d_bs, c("^Inventory$", "^Inventories$")),
    other_recv = .aml_pick(d_bs, c(
      "^Other Receivables$",
      "^Other Current Receivables$",
      "^Non Current Other Receivables$"
    )),
    assets = .aml_pick(d_bs, c("^Total Assets$"))
  )
}

.aml_empty <- function(ok = FALSE, message = "") {
  list(
    ok = isTRUE(ok),
    message = as.character(message %||% ""),
    tier = "na",
    score = NA_real_,
    n_alert = 0L,
    n_watch = 0L,
    flags = data.frame(
      code = character(0),
      severity = character(0),
      value = character(0),
      note_zh = character(0),
      note_en = character(0),
      stringsAsFactors = FALSE
    ),
    evidence = character(0),
    disclaimer_key = "aml_disclaimer"
  )
}

.aml_flag_row <- function(code, severity, value, note_zh, note_en) {
  data.frame(
    code = as.character(code)[1],
    severity = as.character(severity)[1],
    value = as.character(value)[1],
    note_zh = as.character(note_zh)[1],
    note_en = as.character(note_en)[1],
    stringsAsFactors = FALSE
  )
}

.aml_sev_rank <- function(sev) {
  switch(as.character(sev)[1],
    alert = 3L,
    watch = 2L,
    clear = 1L,
    na = 0L,
    0L
  )
}

.aml_tier_from_flags <- function(flags) {
  if (is.null(flags) || !nrow(flags)) return("na")
  sev <- as.character(flags$severity)
  if (any(sev == "alert")) return("alert")
  if (any(sev == "watch")) return("watch")
  if (any(sev == "clear")) return("clear")
  "na"
}

.aml_score_from_flags <- function(flags) {
  if (is.null(flags) || !nrow(flags)) return(NA_real_)
  usable <- flags[flags$severity %in% c("alert", "watch", "clear"), , drop = FALSE]
  if (!nrow(usable)) return(NA_real_)
  pts <- vapply(usable$severity, function(s) {
    switch(as.character(s)[1], alert = 0, watch = 50, clear = 100, NA_real_)
  }, numeric(1))
  pts <- pts[is.finite(pts)]
  if (!length(pts)) return(NA_real_)
  mean(pts)
}

.aml_jurisdiction_tier <- function(meta) {
  # Elevated-risk HQ codes (engineering heuristic; missing → na).
  elevated <- c(
    "KY", "VG", "VI", "BM", "BZ", "PA", "SC", "MU", "MH", "LR", "CW", "IM", "JE", "GG"
  )
  ctry <- toupper(trimws(as.character(
    meta$country %||% meta$hq_country %||% meta$jurisdiction %||% ""
  )[1]))
  if (!nzchar(ctry)) return(list(sev = "na", value = "NA", note_zh = "缺管轄地資料", note_en = "Jurisdiction unavailable"))
  if (ctry %in% elevated) {
    return(list(
      sev = "watch",
      value = ctry,
      note_zh = "管轄地屬較高離岸／結構風險名單（啟發式）",
      note_en = "HQ jurisdiction on elevated offshore / structure watch list (heuristic)"
    ))
  }
  list(
    sev = "clear",
    value = ctry,
    note_zh = "管轄地未落在較高風險名單",
    note_en = "HQ jurisdiction not on elevated watch list"
  )
}

#' Evaluate corporate AML / fraud-possibility proxies from statements.
#'
#' @param d_is,d_bs,d_cf statement data frames (Yahoo-normalized Metric rows)
#' @param meta optional list: country/hq_country, peer_cash_rev_median, auditor_flag,
#'   going_concern, n_schilit
#' @param eq_pack optional evaluate_earnings_quality() result for compose reuse
evaluate_corp_aml_signals <- function(d_is, d_bs, d_cf, meta = list(), eq_pack = NULL) {
  if (is.null(meta) || !is.list(meta)) meta <- list()
  if (is.null(d_is) || is.null(d_bs) || is.null(d_cf) ||
      !is.data.frame(d_is) || !is.data.frame(d_bs) || !is.data.frame(d_cf) ||
      nrow(d_is) < 1L || nrow(d_bs) < 1L || nrow(d_cf) < 1L) {
    return(.aml_empty(
      ok = FALSE,
      message = "Load income, balance sheet, and cash flow to score corporate AML proxies."
    ))
  }

  th <- .AML_THRESHOLDS
  ctx <- tryCatch(.aml_ctx(d_is, d_bs, d_cf), error = function(e) NULL)
  if (is.null(ctx)) {
    return(.aml_empty(ok = FALSE, message = "Failed to read statement rows for AML proxies."))
  }

  rows <- list()
  evidence <- character(0)

  # C1 cash intensity (cash / revenue); peer z when median supplied
  cash0 <- .aml_at(ctx$cash, 1)
  rev0 <- .aml_at(ctx$rev, 1)
  cash_rev <- .aml_ratio(cash0, rev0)
  peer_med <- suppressWarnings(as.numeric(meta$peer_cash_rev_median)[1])
  if (.aml_finite(cash_rev) && .aml_finite(peer_med) && peer_med > 0) {
    z <- (cash_rev - peer_med) / max(abs(peer_med), 1e-4)
    sev <- if (z >= th$peer_z_alert) "alert" else if (z >= th$peer_z_watch) "watch" else "clear"
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C1", sev, sprintf("cash/rev=%.2f z=%.2f", cash_rev, z),
      "現金／營收相對同業中位偏離",
      "Cash/revenue deviation vs peer median"
    )
    evidence <- c(evidence, sprintf("AML_C1 cash_rev=%.4f peer_med=%.4f z=%.3f", cash_rev, peer_med, z))
  } else if (.aml_finite(cash_rev)) {
    sev <- if (cash_rev >= th$cash_rev_alert) {
      "alert"
    } else if (cash_rev >= th$cash_rev_watch) {
      "watch"
    } else {
      "clear"
    }
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C1", sev, sprintf("%.2f", cash_rev),
      "現金／營收絕對門檻（無同業中位時）",
      "Cash/revenue absolute bands (no peer median)"
    )
    evidence <- c(evidence, sprintf("AML_C1 cash_rev=%.4f", cash_rev))
  } else {
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C1", "na", "NA", "現金或營收不足", "Cash or revenue unavailable"
    )
  }

  # C2 operating cash vs earnings (reuse EQ pack when present)
  eq_ok <- is.list(eq_pack) && isTRUE(eq_pack$ok)
  if (eq_ok) {
    qoe_flag <- as.character(eq_pack$qoe_flag %||% "")[1]
    acc <- suppressWarnings(as.numeric(eq_pack$accruals_ratio)[1])
    sev <- if (identical(qoe_flag, "警示") || (is.finite(acc) && acc > 0.10)) {
      "alert"
    } else if (identical(qoe_flag, "觀察") || (is.finite(acc) && acc > 0.05)) {
      "watch"
    } else if (nzchar(qoe_flag) || is.finite(acc)) {
      "clear"
    } else {
      "na"
    }
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C2", sev,
      if (is.finite(acc)) sprintf("accruals=%.3f", acc) else qoe_flag,
      "重用盈餘品質／應計項目（非獨立第二套分數）",
      "Reuses QoE / accruals (composed, not a second opaque score)"
    )
    evidence <- c(evidence, sprintf("AML_C2 qoe_flag=%s accruals=%s", qoe_flag, format(acc)))
  } else {
    ni0 <- .aml_at(ctx$ni, 1)
    ocf0 <- .aml_at(ctx$ocf, 1)
    ta0 <- .aml_at(ctx$assets, 1)
    ta1 <- .aml_at(ctx$assets, 2)
    avg_ta <- if (.aml_finite(ta0) && .aml_finite(ta1)) (ta0 + ta1) / 2 else ta0
    acc <- .aml_ratio(
      (if (.aml_finite(ni0)) ni0 else NA_real_) - (if (.aml_finite(ocf0)) ocf0 else NA_real_),
      avg_ta
    )
    sev <- if (!.aml_finite(acc)) {
      "na"
    } else if (acc > 0.10) {
      "alert"
    } else if (acc > 0.05) {
      "watch"
    } else {
      "clear"
    }
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C2", sev,
      if (.aml_finite(acc)) sprintf("%.3f", acc) else "NA",
      "應計項目 (NI−OCF)/平均資產",
      "Accruals (NI−OCF)/average assets"
    )
  }

  # C3 WC cycle: (ΔAR + ΔInv) / sales
  ar0 <- .aml_at(ctx$ar, 1); ar1 <- .aml_at(ctx$ar, 2)
  inv0 <- .aml_at(ctx$inv, 1); inv1 <- .aml_at(ctx$inv, 2)
  d_ar <- if (.aml_finite(ar0) && .aml_finite(ar1)) ar0 - ar1 else NA_real_
  d_inv <- if (.aml_finite(inv0) && .aml_finite(inv1)) inv0 - inv1 else NA_real_
  wc_sales <- if (.aml_finite(rev0) && rev0 != 0 && (.aml_finite(d_ar) || .aml_finite(d_inv))) {
    ((if (.aml_finite(d_ar)) d_ar else 0) + (if (.aml_finite(d_inv)) d_inv else 0)) / rev0
  } else {
    NA_real_
  }
  sev <- if (!.aml_finite(wc_sales)) {
    "na"
  } else if (wc_sales >= th$wc_sales_alert) {
    "alert"
  } else if (wc_sales >= th$wc_sales_watch) {
    "watch"
  } else {
    "clear"
  }
  rows[[length(rows) + 1L]] <- .aml_flag_row(
    "AML_C3", sev,
    if (.aml_finite(wc_sales)) sprintf("%.3f", wc_sales) else "NA",
    "（Δ應收＋Δ存貨）／營收",
    "(ΔAR + ΔInventory) / sales"
  )

  # C4 other receivables / assets
  orec0 <- .aml_at(ctx$other_recv, 1)
  ta0 <- .aml_at(ctx$assets, 1)
  orec_ta <- .aml_ratio(orec0, ta0)
  sev <- if (!.aml_finite(orec_ta)) {
    "na"
  } else if (orec_ta >= th$other_recv_alert) {
    "alert"
  } else if (orec_ta >= th$other_recv_watch) {
    "watch"
  } else {
    "clear"
  }
  rows[[length(rows) + 1L]] <- .aml_flag_row(
    "AML_C4", sev,
    if (.aml_finite(orec_ta)) sprintf("%.3f", orec_ta) else "NA",
    "其他應收／總資產（關係人代理；缺列則 na）",
    "Other receivables / total assets (related-party proxy; na if missing)"
  )

  # C5 jurisdiction
  j <- .aml_jurisdiction_tier(meta)
  rows[[length(rows) + 1L]] <- .aml_flag_row(
    "AML_C5", j$sev, j$value, j$note_zh, j$note_en
  )

  # C6 |CFF| / assets
  cff0 <- .aml_at(ctx$cff, 1)
  cff_ta <- .aml_ratio(if (.aml_finite(cff0)) abs(cff0) else NA_real_, ta0)
  sev <- if (!.aml_finite(cff_ta)) {
    "na"
  } else if (cff_ta >= th$cff_assets_alert) {
    "alert"
  } else if (cff_ta >= th$cff_assets_watch) {
    "watch"
  } else {
    "clear"
  }
  rows[[length(rows) + 1L]] <- .aml_flag_row(
    "AML_C6", sev,
    if (.aml_finite(cff_ta)) sprintf("%.3f", cff_ta) else "NA",
    "|融資現金流|／總資產",
    "|Financing cash flow| / total assets"
  )

  # C7 Beneish / Schilit composition
  n_schilit <- suppressWarnings(as.integer(meta$n_schilit)[1])
  if (!is.finite(n_schilit)) n_schilit <- NA_integer_
  if (eq_ok) {
    m_flag <- as.character(eq_pack$m_flag %||% "")[1]
    n_red <- length(eq_pack$red_flags %||% character(0))
    sev <- if (identical(m_flag, "警示") || n_red >= 2L ||
              (is.finite(n_schilit) && n_schilit >= 3L)) {
      "alert"
    } else if (identical(m_flag, "觀察") || n_red >= 1L ||
               (is.finite(n_schilit) && n_schilit >= 1L)) {
      "watch"
    } else {
      "clear"
    }
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C7", sev,
      sprintf("m=%s red=%s schilit=%s", m_flag, n_red, format(n_schilit)),
      "與 Beneish／Schilit／EQ 紅旗疊加（composed）",
      "Composed with Beneish / Schilit / EQ red flags"
    )
  } else if (is.finite(n_schilit)) {
    sev <- if (n_schilit >= 3L) "alert" else if (n_schilit >= 1L) "watch" else "clear"
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C7", sev, sprintf("schilit=%d", n_schilit),
      "僅 Schilit 筆數（EQ 包未提供）",
      "Schilit count only (EQ pack unavailable)"
    )
  } else {
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C7", "na", "NA", "缺 EQ／Schilit 輸入", "EQ / Schilit inputs unavailable"
    )
  }

  # C8 auditor / opinion
  aud <- tolower(trimws(as.character(meta$auditor_flag %||% "")[1]))
  gc <- isTRUE(meta$going_concern) || identical(tolower(as.character(meta$going_concern %||% "")[1]), "true")
  if (gc || aud %in% c("change", "qualified", "adverse", "disclaimer")) {
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C8", "alert",
      if (gc) "going_concern" else aud,
      "審計意見／更換／繼續經營疑慮旗標",
      "Auditor opinion / change / going-concern flag"
    )
  } else if (identical(aud, "clean") || identical(aud, "unqualified")) {
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C8", "clear", aud,
      "審計意見無異常旗標",
      "No auditor anomaly flag"
    )
  } else {
    rows[[length(rows) + 1L]] <- .aml_flag_row(
      "AML_C8", "na", "NA",
      "缺審計師／意見資料（不硬給分）",
      "Auditor / opinion unavailable (no forced score)"
    )
  }

  flags <- do.call(rbind, rows)
  rownames(flags) <- NULL
  tier <- .aml_tier_from_flags(flags)
  score <- .aml_score_from_flags(flags)
  n_alert <- sum(flags$severity == "alert")
  n_watch <- sum(flags$severity == "watch")

  list(
    ok = TRUE,
    message = "",
    tier = tier,
    score = score,
    n_alert = as.integer(n_alert),
    n_watch = as.integer(n_watch),
    flags = flags,
    evidence = evidence,
    disclaimer_key = "aml_disclaimer"
  )
}

#' Veto copy keys for YNOW compose (alert only). Never a buy cue.
corp_aml_veto_keys <- function(pack) {
  if (!is.list(pack) || !identical(as.character(pack$tier %||% "")[1], "alert")) {
    return(NULL)
  }
  list(title_key = "aml_veto_title", text_key = "aml_veto_text")
}
