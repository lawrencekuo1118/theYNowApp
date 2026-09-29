# Valuation guards for model selection, DCF claim/WACC, and P/B applicability.
# Universal checks only: no ticker, no industry-specific parameter overrides,
# and no use of market price as the valuation answer.

.VG_MIN_APP <- 50
.VG_MIN_OVERALL <- 55
.VG_CROSS_APP <- 35
.VG_TV_WARN <- 0.80
.VG_SPREAD_WARN <- 0.01
.VG_PB_GAP <- 0.50
.VG_PEER_MIN <- 3L

.vg_num <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) NA_real_ else x
}

.vg_clip <- function(x) {
  x <- .vg_num(x)
  if (!is.finite(x)) return(0)
  max(0, min(100, x))
}

valuation_provenance <- function(value,
                                 source,
                                 effective_date = NA,
                                 calculation_method = NA_character_,
                                 confidence = NA_character_,
                                 override_status = "none") {
  list(
    value = value,
    source = as.character(source %||% "")[1],
    effective_date = effective_date,
    calculation_method = as.character(calculation_method %||% "")[1],
    confidence = as.character(confidence %||% "")[1],
    override_status = as.character(override_status %||% "none")[1]
  )
}

valuation_override_record <- function(original_value,
                                      detected_issue,
                                      proposed_value,
                                      source,
                                      confidence = "low",
                                      override_type = "proposed",
                                      affected_models = character(0),
                                      timestamp = Sys.time()) {
  list(
    original_value = original_value,
    detected_issue = as.character(detected_issue)[1],
    proposed_value = proposed_value,
    source = as.character(source)[1],
    confidence = as.character(confidence)[1],
    override_type = as.character(override_type)[1],
    timestamp = timestamp,
    affected_models = as.character(affected_models)
  )
}

valuation_diagnostic <- function(level = c("fatal", "material", "info"),
                                 code,
                                 field,
                                 current_value = NA,
                                 expected_rule = "",
                                 impact = "",
                                 auto_corrected = FALSE,
                                 adopted_value = NA,
                                 user_can_override = TRUE) {
  level <- match.arg(level)
  list(
    level = level,
    code = as.character(code)[1],
    field = as.character(field)[1],
    current_value = current_value,
    expected_rule = as.character(expected_rule)[1],
    impact = as.character(impact)[1],
    auto_corrected = isTRUE(auto_corrected),
    adopted_value = adopted_value,
    user_can_override = isTRUE(user_can_override)
  )
}

#' Plain-text P/B source note. A missing blend still returns the reason and diagnostics.
format_pb_targets_note <- function(d, locale = "zh-TW", mode = "justified") {
  if (is.null(d)) return("")
  loc <- if (exists("normalize_ui_locale_safe", mode = "function")) {
    normalize_ui_locale_safe(locale)
  } else {
    locale
  }
  fmt <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x)) "—" else sprintf("%.2f", x)
  }
  diags <- d$diagnostics
  diag_txt <- if (is.null(diags) || !length(diags)) {
    ""
  } else {
    paste(vapply(diags, function(one) format_valuation_diagnostic(one, loc), character(1)), collapse = "\n")
  }
  say <- function(key) {
    if (exists("ui_str", mode = "function")) ui_str(key, loc) else key
  }
  mid <- suppressWarnings(as.numeric(d$mid)[1])
  if (!is.finite(mid)) {
    head <- say("pb_note_no_blend")
  } else if (identical(as.character(mode)[1], "multiples")) {
    head <- sprintf(
      say("pb_note_multiples"),
      fmt(d$industry_mid), fmt(d$history_mid), fmt(d$low), fmt(d$mid), fmt(d$high)
    )
  } else {
    head <- sprintf(
      say("pb_note_justified"),
      fmt(d$justified), fmt(d$industry_mid), fmt(d$history_mid), fmt(d$low), fmt(d$mid), fmt(d$high)
    )
  }
  extra <- c(d$source_note, diag_txt)
  extra <- extra[nzchar(extra %||% "")]
  paste(c(head, extra), collapse = "\n")
}

format_valuation_diagnostic <- function(d, locale = "zh-TW") {
  if (is.null(d)) return("")
  title <- d$code
  if (exists("ui_str", mode = "function")) {
    hit <- ui_str(d$code, locale)
    if (!identical(hit, d$code)) title <- hit
  }
  sprintf(
    "[%s] %s | field=%s | value=%s | rule=%s | impact=%s | auto=%s | adopted=%s | user_override=%s",
    toupper(d$level), title, d$field,
    paste(d$current_value, collapse = ","),
    d$expected_rule, d$impact,
    if (isTRUE(d$auto_corrected)) "yes" else "no",
    paste(d$adopted_value, collapse = ","),
    if (isTRUE(d$user_can_override)) "yes" else "no"
  )
}

# ---------------------------------------------------------------------------
# Industry key
# ---------------------------------------------------------------------------

validate_industry_key <- function(industry_key = "") {
  key <- trimws(as.character(industry_key %||% "")[1])
  if (!nzchar(key)) {
    return(list(ok = FALSE, key = "", confidence = "missing",
                code = "val_diag_industry_missing"))
  }
  if (!exists("industry_standards", mode = "list") &&
      !exists("industry_standards", inherits = TRUE)) {
    return(list(ok = FALSE, key = key, confidence = "taxonomy_unloaded",
                code = "val_diag_industry_missing"))
  }
  std <- get("industry_standards", inherits = TRUE)
  if (is.null(std) || !(key %in% names(std))) {
    return(list(ok = FALSE, key = key, confidence = "invalid",
                code = "val_diag_industry_invalid"))
  }
  list(ok = TRUE, key = key, confidence = "taxonomy", code = "val_diag_industry_ok")
}

# ---------------------------------------------------------------------------
# WACC / CAPM
# ---------------------------------------------------------------------------

#' @param re,rd,tax_ratio decimals. tax_ratio must be in [0, 1].
#' @param other optional data.frame(name, weight, cost, after_tax)
compute_wacc <- function(re, rd, tax_ratio, we, wd, other = NULL) {
  re <- .vg_num(re); rd <- .vg_num(rd); tax <- .vg_num(tax_ratio)
  we <- .vg_num(we); wd <- .vg_num(wd)
  diags <- list()
  parts <- list()
  if (!is.finite(re) || !is.finite(rd) || !is.finite(tax) || !is.finite(we) || !is.finite(wd)) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_wacc_na", "WACC",
      current_value = NA_real_,
      expected_rule = "Re, Rd, tax, We, Wd all finite",
      impact = "Calculated WACC is NA; valuation stops",
      auto_corrected = FALSE, adopted_value = NA_real_, user_can_override = TRUE
    )
    return(list(ok = FALSE, wacc = NA_real_, diagnostics = diags, parts = parts,
                provenance = valuation_provenance(NA_real_, "incomplete", calculation_method = "WACC",
                                                  confidence = "none", override_status = "rejected")))
  }
  if (tax < 0 || tax > 1) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_wacc_tax", "tax_ratio", current_value = tax,
      expected_rule = "tax ratio in [0, 1]",
      impact = "WACC not computed",
      auto_corrected = FALSE, adopted_value = NA_real_
    )
    return(list(ok = FALSE, wacc = NA_real_, diagnostics = diags,
                provenance = valuation_provenance(NA_real_, "tax", calculation_method = "WACC",
                                                  confidence = "none", override_status = "rejected")))
  }
  rd_at <- rd * (1 - tax)
  w_sum <- we + wd
  part_wacc <- we * re + wd * rd_at
  parts[[length(parts) + 1L]] <- list(name = "equity", weight = we, cost = re, after_tax_cost = re)
  parts[[length(parts) + 1L]] <- list(name = "debt", weight = wd, cost = rd, after_tax_cost = rd_at)
  if (!is.null(other) && is.data.frame(other) && nrow(other) > 0) {
    for (i in seq_len(nrow(other))) {
      wt <- .vg_num(other$weight[i])
      cost <- .vg_num(other$cost[i])
      aft <- if ("after_tax" %in% names(other)) isTRUE(other$after_tax[i]) else TRUE
      nm <- if ("name" %in% names(other)) as.character(other$name[i]) else paste0("other_", i)
      if (!is.finite(wt) || !is.finite(cost)) next
      net <- if (aft) cost else cost * (1 - tax)
      part_wacc <- part_wacc + wt * net
      w_sum <- w_sum + wt
      parts[[length(parts) + 1L]] <- list(name = nm, weight = wt, cost = cost, after_tax_cost = net)
    }
  }
  if (!is.finite(w_sum) || abs(w_sum - 1) > 0.02) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_wacc_weights", "E/(D+E)+D/(D+E)",
      current_value = w_sum,
      expected_rule = "capital weights sum to 1 (±0.02)",
      impact = "WACC stopped",
      auto_corrected = FALSE, adopted_value = NA_real_
    )
    return(list(ok = FALSE, wacc = NA_real_, weight_sum = w_sum, diagnostics = diags, parts = parts,
                provenance = valuation_provenance(NA_real_, "weights", calculation_method = "WACC",
                                                  confidence = "none", override_status = "rejected")))
  }
  only_two <- length(parts) == 2L
  if (only_two) {
    lo <- min(re, rd_at); hi <- max(re, rd_at)
    if (part_wacc < lo - 1e-8 || part_wacc > hi + 1e-8) {
      diags[[length(diags) + 1L]] <- valuation_diagnostic(
        "fatal", "val_diag_wacc_bounds", "WACC",
        current_value = part_wacc,
        expected_rule = "With only equity and debt, WACC lies between Re and after-tax Rd",
        impact = "Formula error; valuation stopped",
        auto_corrected = FALSE, adopted_value = NA_real_, user_can_override = FALSE
      )
      return(list(ok = FALSE, wacc = NA_real_, diagnostics = diags, parts = parts,
                  provenance = valuation_provenance(NA_real_, "bounds", calculation_method = "WACC",
                                                    confidence = "none", override_status = "rejected")))
    }
  } else {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "info", "val_diag_wacc_other_capital", "capital_structure",
      current_value = paste(vapply(parts, function(p) p$name, character(1)), collapse = "+"),
      expected_rule = "Preferred, lease, or other claims are listed separately",
      impact = "WACC need not lie strictly between Re and after-tax Rd",
      auto_corrected = FALSE, adopted_value = part_wacc
    )
  }
  list(
    ok = TRUE, wacc = part_wacc, weight_sum = w_sum, re = re, rd = rd,
    rd_after_tax = rd_at, tax_ratio = tax, we = we, wd = wd,
    diagnostics = diags, parts = parts,
    provenance = valuation_provenance(
      part_wacc, "formula", calculation_method = "We*Re + Wd*Rd*(1-T) [+ other]",
      confidence = "calculated", override_status = "system_calculated"
    )
  )
}

#' Refuse to keep a previous manual WACC when the calculated value is NA.
resolve_wacc_input <- function(calculated = NA_real_, manual = NA_real_,
                               mode = c("calculated", "manual")) {
  mode <- match.arg(mode)
  calculated <- .vg_num(calculated)
  manual <- .vg_num(manual)
  if (identical(mode, "calculated")) {
    if (!is.finite(calculated)) {
      return(list(
        ok = FALSE, value = NA_real_, override_status = "rejected",
        provenance = valuation_provenance(NA_real_, "calculated", calculation_method = "WACC",
                                          confidence = "none", override_status = "rejected"),
        diagnostic = valuation_diagnostic(
          "fatal", "val_diag_wacc_na", "WACC", current_value = NA_real_,
          expected_rule = "A calculated WACC that is NA must not reuse a prior manual or prior-ticker value",
          impact = "DCF stopped", auto_corrected = FALSE, adopted_value = NA_real_
        )
      ))
    }
    return(list(
      ok = TRUE, value = calculated, override_status = "system_calculated",
      provenance = valuation_provenance(calculated, "calculated", calculation_method = "WACC",
                                        confidence = "calculated", override_status = "system_calculated"),
      diagnostic = NULL
    ))
  }
  list(
    ok = is.finite(manual), value = manual, override_status = "manual",
    provenance = valuation_provenance(manual, "user", calculation_method = "manual WACC",
                                      confidence = "manual", override_status = "manual"),
    diagnostic = valuation_diagnostic(
      "info", "val_diag_manual_wacc", "WACC", current_value = manual,
      expected_rule = "Manual WACC is labeled manual override, not system calculated",
      impact = "Discount rate follows the user input",
      auto_corrected = FALSE, adopted_value = manual, user_can_override = TRUE
    )
  )
}

capm_cost_of_equity <- function(rf, beta, rm = NA_real_, erp = NA_real_, extra_premium = 0) {
  rf <- .vg_num(rf); beta <- .vg_num(beta); rm <- .vg_num(rm)
  erp <- .vg_num(erp); extra <- .vg_num(extra_premium)
  if (!is.finite(extra)) extra <- 0
  diags <- list()
  if (!is.finite(rf) || !is.finite(beta)) {
    return(list(ok = FALSE, ke = NA_real_, diagnostics = list(valuation_diagnostic(
      "fatal", "val_diag_capm_inputs", "CAPM",
      expected_rule = "Rf and levered beta are finite",
      impact = "Ke is NA", auto_corrected = FALSE, adopted_value = NA_real_
    ))))
  }
  erp_use <- NA_real_
  if (is.finite(rm)) {
    erp_imp <- rm - rf
    if (is.finite(erp) && abs(erp - erp_imp) > 1e-6) {
      diags[[length(diags) + 1L]] <- valuation_diagnostic(
        "fatal", "val_diag_erp_mismatch", "ERP",
        current_value = erp, expected_rule = "ERP = Rm - Rf when both are supplied",
        impact = "Ke stopped", auto_corrected = FALSE, adopted_value = NA_real_
      )
      return(list(ok = FALSE, ke = NA_real_, diagnostics = diags))
    }
    erp_use <- erp_imp
  } else if (is.finite(erp)) {
    erp_use <- erp
  } else {
    return(list(ok = FALSE, ke = NA_real_, diagnostics = list(valuation_diagnostic(
      "fatal", "val_diag_capm_inputs", "ERP",
      expected_rule = "Provide Rm or ERP",
      impact = "Ke is NA", auto_corrected = FALSE, adopted_value = NA_real_
    ))))
  }
  if (erp_use < 0 || erp_use > 0.12) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_erp_range", "ERP", current_value = erp_use,
      expected_rule = "ERP is disclosed; values outside 2%–12% are a soft warning, not a ticker band",
      impact = "Ke remains calculated; confidence falls",
      auto_corrected = FALSE, adopted_value = erp_use
    )
  }
  if (extra != 0) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "info", "val_diag_extra_premium", "company_specific_premium",
      current_value = extra,
      expected_rule = "Company-specific premium is shown separately from beta and WACC",
      impact = "Ke = Rf + beta * ERP + premium",
      auto_corrected = FALSE, adopted_value = extra
    )
  }
  ke <- rf + beta * erp_use + extra
  list(
    ok = TRUE, ke = ke, erp = erp_use, extra_premium = extra, rf = rf, beta = beta, rm = rm,
    diagnostics = diags,
    provenance = valuation_provenance(
      ke, "CAPM", calculation_method = "Rf + beta*(Rm-Rf) + extra_premium",
      confidence = if (length(diags) && any(vapply(diags, function(d) d$level == "material", logical(1)))) "medium" else "calculated",
      override_status = if (extra != 0) "premium_disclosed" else "system_calculated"
    )
  )
}

# ---------------------------------------------------------------------------
# DCF claim, stages, quality
# ---------------------------------------------------------------------------

validate_dcf_claim_rate <- function(claim = "fcff", discount_kind = NULL, wacc = NA, ke = NA) {
  is_fcfe <- isTRUE(dcf_claim_is_fcfe(claim))
  kind <- tolower(as.character(discount_kind %||% if (is_fcfe) "ke" else "wacc")[1])
  diags <- list()
  if (is_fcfe && kind %in% c("wacc", "fcff")) {
    diags[[1]] <- valuation_diagnostic(
      "fatal", "val_diag_claim_mismatch", "discount_rate",
      current_value = "FCFE+WACC",
      expected_rule = "FCFE discounts at Ke and is already equity value",
      impact = "FCFE valuation stopped", auto_corrected = FALSE, adopted_value = NA
    )
    return(list(ok = FALSE, claim = "fcfe", rate = NA_real_, bridge = "none", diagnostics = diags))
  }
  if (!is_fcfe && kind %in% c("ke", "cost_of_equity", "re")) {
    diags[[1]] <- valuation_diagnostic(
      "fatal", "val_diag_claim_mismatch", "discount_rate",
      current_value = "FCFF+Ke",
      expected_rule = "FCFF discounts at WACC, then EV bridge to equity",
      impact = "FCFF valuation stopped", auto_corrected = FALSE, adopted_value = NA
    )
    return(list(ok = FALSE, claim = "fcff", rate = NA_real_, bridge = "ev", diagnostics = diags))
  }
  if (is_fcfe) {
    rate <- .vg_num(ke)
    if (!is.finite(rate)) {
      return(list(ok = FALSE, claim = "fcfe", rate = NA_real_, bridge = "none",
                  diagnostics = list(valuation_diagnostic(
                    "fatal", "val_diag_wacc_na", "Ke", current_value = NA,
                    expected_rule = "FCFE needs a finite Ke", impact = "stopped",
                    auto_corrected = FALSE, adopted_value = NA
                  ))))
    }
    return(list(ok = TRUE, claim = "fcfe", rate = rate, bridge = "none", rate_name = "Ke", diagnostics = diags))
  }
  rate <- .vg_num(wacc)
  if (!is.finite(rate)) {
    return(list(ok = FALSE, claim = "fcff", rate = NA_real_, bridge = "ev",
                diagnostics = list(valuation_diagnostic(
                  "fatal", "val_diag_wacc_na", "WACC", current_value = NA,
                  expected_rule = "FCFF needs a finite calculated or explicitly manual WACC",
                  impact = "stopped", auto_corrected = FALSE, adopted_value = NA
                ))))
  }
  list(ok = TRUE, claim = "fcff", rate = rate, bridge = "ev", rate_name = "WACC", diagnostics = diags)
}

validate_stage_years <- function(n_years, yr_stage1 = NA, yr_stage2 = NA, g2 = NA, g_term = NA) {
  n <- suppressWarnings(as.integer(round(.vg_num(n_years))))
  y1 <- suppressWarnings(as.integer(round(.vg_num(yr_stage1))))
  y2 <- suppressWarnings(as.integer(round(.vg_num(yr_stage2))))
  diags <- list()
  ok <- TRUE
  if (!is.finite(n) || n < 1L) {
    ok <- FALSE
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_stage_years", "forecast_years", current_value = n_years,
      expected_rule = "Forecast years >= 1", impact = "Two-stage path stopped",
      auto_corrected = FALSE, adopted_value = NA
    )
  }
  if (is.finite(y1) && is.finite(n) && (y1 < 1L || y1 >= n)) {
    ok <- FALSE
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_stage_years", "yr_stage1", current_value = y1,
      expected_rule = "0 < Stage 1 years < forecast years",
      impact = "Stage length is not replaced with a default",
      auto_corrected = FALSE, adopted_value = NA
    )
  }
  if (is.finite(y1) && !is.finite(y2)) {
    ok <- FALSE
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_stage_years", "yr_stage2", current_value = NA,
      expected_rule = "Stage 2 years must be supplied; the remainder is not inferred",
      impact = "Two-stage path stopped", auto_corrected = FALSE, adopted_value = NA
    )
  }
  if (is.finite(y1) && is.finite(y2) && is.finite(n) && (y1 + y2) != n) {
    ok <- FALSE
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_stage_years", "yr_stage1+yr_stage2",
      current_value = y1 + y2,
      expected_rule = "Stage 1 years + Stage 2 years = explicit forecast years",
      impact = "Two-stage path stopped", auto_corrected = FALSE, adopted_value = NA
    )
  }
  g2n <- .vg_num(g2); gt <- .vg_num(g_term)
  if (is.finite(g2n) && is.finite(gt) && abs(g2n - gt) < 1e-8) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_g2_equals_terminal", "g2",
      current_value = g2n,
      expected_rule = "If g2 equals terminal g, Stage 2 does not change the growth path",
      impact = "Confidence falls; the path is still explicit",
      auto_corrected = FALSE, adopted_value = g2n
    )
  }
  list(ok = ok, n_years = n, yr_stage1 = if (ok) y1 else NA_integer_,
       yr_stage2 = if (ok) y2 else NA_integer_, diagnostics = diags)
}

resolve_near_term_growth <- function(method = c("fundamental", "custom"),
                                     fundamental_g = NA_real_,
                                     custom_g = NA_real_,
                                     fallback_g = NA_real_,
                                     fallback_source = "none") {
  method <- match.arg(method)
  fg <- .vg_num(fundamental_g)
  cg <- .vg_num(custom_g)
  fb <- .vg_num(fallback_g)
  if (identical(method, "custom")) {
    if (!is.finite(cg)) {
      return(list(
        ok = FALSE, g = NA_real_, source = "custom",
        provenance = valuation_provenance(NA_real_, "user", calculation_method = "custom",
                                          confidence = "none", override_status = "missing"),
        record = NULL,
        diagnostic = valuation_diagnostic(
          "fatal", "val_diag_growth_custom_missing", "custom_g", current_value = NA,
          expected_rule = "Custom growth uses the user input only",
          impact = "Near-term g is NA; industry and fundamental values are not substituted",
          auto_corrected = FALSE, adopted_value = NA
        )
      ))
    }
    return(list(
      ok = TRUE, g = cg, source = "custom",
      provenance = valuation_provenance(cg, "user", calculation_method = "custom",
                                        confidence = "manual", override_status = "manual"),
      record = NULL, diagnostic = NULL
    ))
  }
  if (is.finite(fg)) {
    return(list(
      ok = TRUE, g = fg, source = "fundamental",
      provenance = valuation_provenance(fg, "fundamental", calculation_method = "driver or SGR",
                                        confidence = "calculated", override_status = "none"),
      record = NULL,
      diagnostic = valuation_diagnostic(
        "info", "val_diag_growth_fundamental", "g1", current_value = fg,
        expected_rule = "Fundamental method ignores custom growth",
        impact = "Custom input is not applied", auto_corrected = FALSE, adopted_value = fg
      )
    ))
  }
  if (is.finite(fb)) {
    rec <- valuation_override_record(
      fg, "fundamental growth unavailable", fb, fallback_source,
      confidence = "low", override_type = "fallback", affected_models = "dcf"
    )
    return(list(
      ok = TRUE, g = fb, source = fallback_source,
      provenance = valuation_provenance(fb, fallback_source, calculation_method = "fallback",
                                        confidence = "low", override_status = "fallback"),
      record = rec,
      diagnostic = valuation_diagnostic(
        "material", "val_diag_growth_fallback", "g1", current_value = fg,
        expected_rule = "Show the fallback source when fundamental inputs are missing",
        impact = "Near-term g is the disclosed fallback, not a silent default and not the custom input",
        auto_corrected = TRUE, adopted_value = fb
      )
    ))
  }
  list(
    ok = FALSE, g = NA_real_, source = "fundamental",
    provenance = valuation_provenance(NA_real_, "fundamental", calculation_method = "fundamental",
                                      confidence = "none", override_status = "missing"),
    record = NULL,
    diagnostic = valuation_diagnostic(
      "fatal", "val_diag_growth_fundamental_missing", "g1", current_value = NA,
      expected_rule = "Do not keep an old default and do not borrow custom g",
      impact = "Near-term g is NA", auto_corrected = FALSE, adopted_value = NA
    )
  )
}

dcf_result_quality <- function(explicit_pv, pv_tv, ev, cash = 0, debt = 0,
                               equity = NA, last_fcf = NA, last_revenue = NA,
                               wacc, g_term, per_share = NA,
                               per_share_wacc_up = NA, per_share_g_up = NA,
                               per_share_fcf_up = NA,
                               terminal_reinvestment = NA, terminal_roic = NA) {
  ev <- .vg_num(ev); explicit_pv <- .vg_num(explicit_pv); pv_tv <- .vg_num(pv_tv)
  cash <- .vg_num(cash); debt <- .vg_num(debt); equity <- .vg_num(equity)
  wacc <- .vg_num(wacc); g <- .vg_num(g_term)
  diags <- list()
  tv_share <- if (is.finite(ev) && abs(ev) > 1e-8 && is.finite(pv_tv)) pv_tv / ev else NA_real_
  explicit_share <- if (is.finite(ev) && abs(ev) > 1e-8 && is.finite(explicit_pv)) explicit_pv / ev else NA_real_
  net_cash <- if (is.finite(cash) || is.finite(debt)) {
    (if (is.finite(cash)) cash else 0) - (if (is.finite(debt)) debt else 0)
  } else NA_real_
  net_cash_share <- if (is.finite(equity) && abs(equity) > 1e-8 && is.finite(net_cash)) net_cash / equity else NA_real_
  fcf_margin <- if (is.finite(last_fcf) && is.finite(last_revenue) && abs(last_revenue) > 1e-8) last_fcf / last_revenue else NA_real_
  spread <- if (is.finite(wacc) && is.finite(g)) wacc - g else NA_real_
  sens_w <- if (is.finite(per_share) && abs(per_share) > 1e-8 && is.finite(per_share_wacc_up)) {
    (per_share_wacc_up - per_share) / per_share
  } else NA_real_
  sens_g <- if (is.finite(per_share) && abs(per_share) > 1e-8 && is.finite(per_share_g_up)) {
    (per_share_g_up - per_share) / per_share
  } else NA_real_
  sens_f <- if (is.finite(per_share) && abs(per_share) > 1e-8 && is.finite(per_share_fcf_up)) {
    (per_share_fcf_up - per_share) / per_share
  } else NA_real_
  if (is.finite(tv_share) && tv_share > .VG_TV_WARN) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_tv_weight", "pv_terminal / EV", current_value = tv_share,
      expected_rule = paste0("Soft warning when terminal value exceeds ", .VG_TV_WARN, " of EV"),
      impact = "Confidence cannot be high", auto_corrected = FALSE, adopted_value = tv_share
    )
  }
  if (is.finite(spread) && spread <= 0) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_wacc_gt_g", "WACC-g", current_value = spread,
      expected_rule = "WACC > terminal g", impact = "Gordon TV stopped",
      auto_corrected = FALSE, adopted_value = NA
    )
  } else if (is.finite(spread) && spread < .VG_SPREAD_WARN) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_wacc_g_spread", "WACC-g", current_value = spread,
      expected_rule = "Soft warning when WACC - g is under 1 percentage point",
      impact = "High sensitivity; confidence cannot be high",
      auto_corrected = FALSE, adopted_value = spread
    )
  }
  ident <- terminal_g_identity(g, terminal_reinvestment, terminal_roic)
  confidence <- "medium"
  if (any(vapply(diags, function(d) identical(d$level, "fatal"), logical(1)))) confidence <- "none"
  else if (length(diags)) confidence <- "low"
  else confidence <- "medium"
  list(
    tv_to_ev = tv_share,
    net_cash_to_equity = net_cash_share,
    explicit_pv_to_ev = explicit_share,
    final_fcf_margin = fcf_margin,
    wacc_minus_g = spread,
    sensitivity_wacc_plus_1pp = sens_w,
    sensitivity_g_plus_50bp = sens_g,
    sensitivity_base_fcf_plus_10pct = sens_f,
    terminal_identity = ident,
    confidence = confidence,
    diagnostics = diags
  )
}

terminal_g_identity <- function(g, reinvestment = NA, roic = NA) {
  g <- .vg_num(g); rr <- .vg_num(reinvestment); roic <- .vg_num(roic)
  if (is.finite(rr) && is.finite(roic)) {
    implied <- rr * roic
    return(list(
      terminal_g = g, terminal_reinvestment = rr, terminal_return = roic,
      implied_g = implied, simplified_gordon = FALSE,
      consistent = is.finite(g) && abs(g - implied) <= 0.005
    ))
  }
  list(
    terminal_g = g, terminal_reinvestment = NA_real_, terminal_return = NA_real_,
    implied_g = NA_real_, simplified_gordon = TRUE, consistent = NA
  )
}

value_dcf_per_share <- function(cf, claim = "fcff", wacc = NA, ke = NA,
                                g_term, cash = 0, debt = 0, shares,
                                discount_kind = NULL) {
  cf <- suppressWarnings(as.numeric(cf))
  cf <- cf[is.finite(cf)]
  gate <- validate_dcf_claim_rate(claim, discount_kind, wacc = wacc, ke = ke)
  shares <- .vg_num(shares)
  g <- .vg_num(g_term)
  diags <- gate$diagnostics
  if (!isTRUE(gate$ok)) {
    return(list(ok = FALSE, equity = NA_real_, per_share = NA_real_, enterprise = NA_real_,
                diagnostics = diags))
  }
  if (!length(cf)) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_fcf_missing", "FCFF", current_value = NA,
      expected_rule = "Cash-flow path must be finite", impact = "No value returned",
      auto_corrected = FALSE, adopted_value = NA
    )
    return(list(ok = FALSE, equity = NA_real_, per_share = NA_real_, diagnostics = diags))
  }
  if (!is.finite(shares) || shares <= 0) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_shares", "shares", current_value = shares,
      expected_rule = "Diluted shares > 0", impact = "Per-share value stopped",
      auto_corrected = FALSE, adopted_value = NA
    )
    return(list(ok = FALSE, equity = NA_real_, per_share = NA_real_, diagnostics = diags))
  }
  if (!is.finite(g) || gate$rate <= g) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_wacc_gt_g", if (identical(gate$claim, "fcfe")) "Ke-g" else "WACC-g",
      current_value = if (is.finite(g)) gate$rate - g else NA,
      expected_rule = "Discount rate > terminal g", impact = "Gordon TV stopped",
      auto_corrected = FALSE, adopted_value = NA
    )
    return(list(ok = FALSE, equity = NA_real_, per_share = NA_real_, diagnostics = diags))
  }
  if (cf[length(cf)] < 0) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_negative_terminal_cf", "terminal_cf", current_value = cf[length(cf)],
      expected_rule = "Do not Gordon-perpetuate a negative final cash flow",
      impact = "Use a driver-based path whose final year is positive, or stop",
      auto_corrected = FALSE, adopted_value = NA
    )
    return(list(ok = FALSE, equity = NA_real_, per_share = NA_real_, diagnostics = diags))
  }
  ev_or_eq <- .dcf_formula_ev_from_fcff(cf, r1 = gate$rate, g_term = g)
  if (!is.finite(ev_or_eq)) {
    return(list(ok = FALSE, equity = NA_real_, per_share = NA_real_, diagnostics = diags))
  }
  if (identical(gate$bridge, "ev")) {
    equity <- dcf_ev_to_equity(ev_or_eq, cash = cash, debt = debt)
    enterprise <- ev_or_eq
  } else {
    equity <- ev_or_eq
    enterprise <- NA_real_
  }
  list(
    ok = is.finite(equity),
    claim = gate$claim,
    rate_name = gate$rate_name,
    rate = gate$rate,
    bridge = gate$bridge,
    enterprise = enterprise,
    equity = equity,
    per_share = if (is.finite(equity)) equity / shares else NA_real_,
    shares = shares,
    diagnostics = diags,
    provenance = valuation_provenance(
      if (is.finite(equity)) equity / shares else NA_real_,
      gate$claim,
      calculation_method = if (identical(gate$bridge, "ev")) "PV(FCFF)+PV(TV) + cash - debt / diluted shares" else "PV(FCFE)+PV(TV) / diluted shares",
      confidence = "calculated",
      override_status = "none"
    )
  )
}

detect_share_basis_shift <- function(shares_newest, shares_prior) {
  a <- .vg_num(shares_newest); b <- .vg_num(shares_prior)
  if (!is.finite(a) || !is.finite(b) || a <= 0 || b <= 0) {
    return(list(shift = FALSE, ratio = NA_real_, diagnostic = NULL))
  }
  ratio <- a / b
  near_int <- function(x) {
    r <- round(x)
    is.finite(x) && r >= 2 && abs(x - r) <= 0.02 * r
  }
  if (near_int(ratio) || near_int(1 / ratio)) {
    return(list(
      shift = TRUE, ratio = ratio,
      diagnostic = valuation_diagnostic(
        "material", "val_diag_share_split", "shares", current_value = ratio,
        expected_rule = "A near-integer share-count jump is checked as a split or reverse split",
        impact = "Do not treat the jump as economic dilution until the split factor is confirmed",
        auto_corrected = FALSE, adopted_value = NA
      )
    ))
  }
  list(shift = FALSE, ratio = ratio, diagnostic = NULL)
}

apply_share_split <- function(shares, equity_value, split_ratio) {
  shares <- .vg_num(shares); equity <- .vg_num(equity_value); k <- .vg_num(split_ratio)
  if (!is.finite(shares) || shares <= 0 || !is.finite(equity) || !is.finite(k) || k <= 0) {
    return(list(ok = FALSE, shares = NA_real_, per_share = NA_real_, equity = NA_real_))
  }
  sh2 <- shares * k
  list(ok = TRUE, shares = sh2, per_share = equity / sh2, equity = equity, split_ratio = k)
}

stale_ticker_diagnostic <- function(cache_ticker, active_ticker) {
  a <- toupper(trimws(as.character(cache_ticker %||% "")[1]))
  b <- toupper(trimws(as.character(active_ticker %||% "")[1]))
  if (!nzchar(a) || !nzchar(b) || identical(a, b)) return(NULL)
  valuation_diagnostic(
    "fatal", "val_diag_stale_ticker", "ticker",
    current_value = paste(a, "->", b),
    expected_rule = "Session parameters are keyed by ticker; a prior company WACC, beta, industry band, or shares must not carry over",
    impact = "Block valuation until parameters refresh",
    auto_corrected = FALSE, adopted_value = NA, user_can_override = FALSE
  )
}

# ---------------------------------------------------------------------------
# P/B
# ---------------------------------------------------------------------------

assess_pb_applicability <- function(equity = NA, goodwill = NA, intangibles = NA, assets = NA,
                                    rd_expense = NA, revenue = NA,
                                    industry_text = "", industry_key = "",
                                    industry_confidence = NULL, peer_n = NA,
                                    roe = NA, buyback_to_equity = NA) {
  eq <- .vg_num(equity)
  gw <- .vg_num(goodwill); intang <- .vg_num(intangibles); assets <- .vg_num(assets)
  rd <- .vg_num(rd_expense); rev <- .vg_num(revenue); roe <- .vg_num(roe)
  bb <- .vg_num(buyback_to_equity); peers <- .vg_num(peer_n)
  ind <- validate_industry_key(industry_key)
  conf <- as.character(industry_confidence %||% ind$confidence)[1]
  txt <- paste(industry_text %||% "", industry_key %||% "")
  intangible_biz <- grepl(
    "Software|Internet|Interactive Media|Application|Brand|Platform|Data Processing|Advertising",
    txt, ignore.case = TRUE
  )
  book_biz <- grepl("Bank|Insurance|REIT|Real Estate|Utility|Utilities|\\bFinancials?\\b", txt, ignore.case = TRUE) ||
    grepl("^fn\\.", as.character(industry_key %||% ""))
  diags <- list()
  score <- 0
  blocked <- FALSE
  if (!is.finite(eq)) {
    blocked <- TRUE
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_pb_equity_missing", "equity", current_value = NA,
      expected_rule = "Common equity must be finite before any P/B",
      impact = "Ordinary P/B not produced", auto_corrected = FALSE, adopted_value = NA
    )
  } else if (eq <= 0) {
    blocked <- TRUE
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "fatal", "val_diag_neg_equity", "equity", current_value = eq,
      expected_rule = "Negative common equity does not support an ordinary P/B",
      impact = "No P/B fair value", auto_corrected = FALSE, adopted_value = NA
    )
  } else {
    score <- 45
    if (is.finite(roe) && roe > 0) score <- score + 15
    if (isTRUE(book_biz)) score <- score + 20
    if (isTRUE(intangible_biz)) {
      score <- score - 25
      diags[[length(diags) + 1L]] <- valuation_diagnostic(
        "material", "val_diag_pb_intangible_biz", "industry", current_value = txt,
        expected_rule = "Book value is a weak primary anchor when value is mostly unrecognized intangibles",
        impact = "P/B may be only a low-weight cross-check",
        auto_corrected = FALSE, adopted_value = score
      )
    }
  }
  gw_ratio <- if (is.finite(gw) && is.finite(assets) && assets > 0) gw / assets else NA_real_
  intang_ratio <- if (is.finite(intang) && is.finite(assets) && assets > 0) intang / assets else NA_real_
  if (is.finite(gw_ratio) && gw_ratio > 0.30) {
    score <- score - 15
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_pb_goodwill", "goodwill/assets", current_value = gw_ratio,
      expected_rule = "High goodwill weakens accounting book value",
      impact = "Lower P/B confidence; consider TBVPS disclosure",
      auto_corrected = FALSE, adopted_value = score
    )
  }
  if (is.finite(intang_ratio) && intang_ratio > 0.30) score <- score - 10
  rd_ratio <- if (is.finite(rd) && is.finite(rev) && rev > 0) rd / rev else NA_real_
  if (is.finite(rd_ratio) && rd_ratio > 0.08) {
    score <- score - 10
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_pb_rd", "RD/Revenue", current_value = rd_ratio,
      expected_rule = "Expensed R&D can leave book equity below economic capital",
      impact = "P/B applicability falls", auto_corrected = FALSE, adopted_value = score
    )
  }
  if (is.finite(bb) && bb > 0.25) {
    score <- score - 10
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_pb_buyback", "buyback/equity", current_value = bb,
      expected_rule = "Large buybacks can shrink book equity without an equal fall in value",
      impact = "P/B applicability falls", auto_corrected = FALSE, adopted_value = score
    )
  }
  industry_pb_ok <- isTRUE(ind$ok) && !identical(conf, "missing") && !identical(conf, "invalid") && !identical(conf, "low")
  if (!industry_pb_ok) {
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_industry_pb_blocked", "industry_key",
      current_value = industry_key,
      expected_rule = "Industry P/B is not used when the taxonomy key is missing, invalid, or low confidence",
      impact = "Justified P/B may still be shown separately",
      auto_corrected = FALSE, adopted_value = NA
    )
  }
  thin <- is.finite(peers) && peers < .VG_PEER_MIN
  if (thin) {
    industry_pb_ok <- FALSE
    diags[[length(diags) + 1L]] <- valuation_diagnostic(
      "material", "val_diag_thin_peers", "peer_n", current_value = peers,
      expected_rule = paste0("Peer sample below ", .VG_PEER_MIN, " cannot support a high-confidence industry multiple"),
      impact = "Industry P/B excluded from the blend",
      auto_corrected = FALSE, adopted_value = NA
    )
  }
  score <- .vg_clip(score)
  list(
    score = score,
    blocked = blocked || score < .VG_CROSS_APP,
    ordinary_pb = !blocked && score >= .VG_CROSS_APP,
    industry_pb_ok = isTRUE(industry_pb_ok) && !blocked,
    intangible_biz = isTRUE(intangible_biz),
    book_biz = isTRUE(book_biz),
    industry = ind,
    diagnostics = diags
  )
}

# ---------------------------------------------------------------------------
# Model recommendation scores
# ---------------------------------------------------------------------------

.vg_lens <- function(model) {
  switch(model,
         dcf = "cash_flow", ddm = "dividend", pb = "book",
         ri = "residual_earnings", nav = "assets", "other")
}

.vg_score_row <- function(model, applicability, data_quality, stability, accounting,
                          forecast, industry, notes = character(0), gordon_blocked = FALSE) {
  applicability <- .vg_clip(applicability)
  data_quality <- .vg_clip(data_quality)
  stability <- .vg_clip(stability)
  accounting <- .vg_clip(accounting)
  forecast <- .vg_clip(forecast)
  industry <- .vg_clip(industry)
  overall <- .vg_clip(
    0.25 * applicability + 0.15 * data_quality + 0.15 * stability +
      0.10 * accounting + 0.20 * forecast + 0.15 * industry
  )
  data.frame(
    model = model,
    lens = .vg_lens(model),
    applicability = applicability,
    data_quality = data_quality,
    stability = stability,
    accounting = accounting,
    forecast = forecast,
    industry = industry,
    overall = overall,
    gordon_blocked = isTRUE(gordon_blocked),
    notes = paste(notes, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

assemble_model_recommendation <- function(data_missing = FALSE,
                                          conf_in = list(),
                                          is_fcf_pos = FALSE,
                                          is_fcf_stable = FALSE,
                                          fcf_cv = NA_real_,
                                          is_div_stable = FALSE,
                                          ind_txt = "",
                                          ind_key = "",
                                          rev_g = NA_real_,
                                          ni = NA_real_,
                                          equity = NA_real_,
                                          roe = NA_real_,
                                          last_fcff = NA_real_,
                                          fcff_source = "unknown",
                                          n_fcff = 0L,
                                          goodwill = NA_real_,
                                          intangibles = NA_real_,
                                          assets = NA_real_,
                                          rd_expense = NA_real_,
                                          revenue = NA_real_) {
  empty_scores <- data.frame(
    model = character(0), lens = character(0), applicability = numeric(0),
    data_quality = numeric(0), stability = numeric(0), accounting = numeric(0),
    forecast = numeric(0), industry = numeric(0), overall = numeric(0),
    gordon_blocked = logical(0), notes = character(0), stringsAsFactors = FALSE
  )
  pack <- function(company_type, primary, secondary, summary_method, reason,
                   suggest_two_stage = FALSE, scores = empty_scores,
                   secondary_role = "none", weight_primary = NA_real_,
                   weight_secondary = NA_real_, inapplicable = character(0),
                   reason_en = NULL) {
    flags <- list(ddm = FALSE, dcf = FALSE, pb = FALSE, ri = FALSE, nav = FALSE)
    if (!is.null(primary) && nzchar(as.character(primary))) flags[[as.character(primary)]] <- TRUE
    if (!is.null(secondary) && nzchar(as.character(secondary))) flags[[as.character(secondary)]] <- TRUE
    tags <- unique(c(
      if (!is.null(primary)) as.character(primary),
      if (!is.null(secondary)) as.character(secondary)
    ))
    tags <- tags[nzchar(tags)]
    list(
      company_type = company_type,
      primary = if (is.null(primary) || !nzchar(as.character(primary))) NULL else as.character(primary),
      secondary = if (is.null(secondary) || !nzchar(as.character(secondary))) NULL else as.character(secondary),
      secondary_role = secondary_role,
      secondary_label = if (is.null(secondary) || !nzchar(as.character(secondary %||% ""))) "無適用副模型" else as.character(secondary),
      weight_primary = weight_primary,
      weight_secondary = weight_secondary,
      ddm = isTRUE(flags$ddm), dcf = isTRUE(flags$dcf), pb = isTRUE(flags$pb),
      ri = isTRUE(flags$ri), nav = isTRUE(flags$nav),
      tags = tags,
      summary_method = summary_method,
      reason = reason,
      reason_en = reason_en,
      suggest_two_stage = isTRUE(suggest_two_stage),
      confidence_inputs = conf_in,
      model_scores = scores,
      inapplicable = inapplicable,
      dependency_note = "RI, DDM, and NAV formulas are unchanged; only selection scores are attached."
    )
  }

  if (isTRUE(data_missing)) {
    return(pack(
      "insufficient", NULL, NULL, "無適用模型",
      "資料不足，沒有通過適用門檻的主模型，也不套用 P/B 作為靜默預設。",
      inapplicable = c("dcf", "pb", "ri", "ddm", "nav")
    ))
  }

  ind_status <- validate_industry_key(ind_key)
  pb_view <- assess_pb_applicability(
    equity = equity, goodwill = goodwill, intangibles = intangibles, assets = assets,
    rd_expense = rd_expense, revenue = revenue,
    industry_text = ind_txt, industry_key = ind_key,
    industry_confidence = ind_status$confidence, roe = roe
  )
  txt <- paste(ind_txt %||% "", ind_key %||% "")
  is_holding <- grepl("Conglomerate|Holding|fn\\.Conglomerate", txt, ignore.case = TRUE)
  is_book <- isTRUE(pb_view$book_biz)
  is_intang <- isTRUE(pb_view$intangible_biz)
  rev_g <- .vg_num(rev_g); ni <- .vg_num(ni); equity <- .vg_num(equity)
  roe <- .vg_num(roe); last_fcff <- .vg_num(last_fcff); fcf_cv <- .vg_num(fcf_cv)
  gordon_blocked <- is.finite(last_fcff) && last_fcff < 0
  cfo_ok <- identical(fcff_source, "cfo_identity") || identical(fcff_source, "cfo_identity_with_yahoo_fallback")

  dcf_app <- 20
  if (n_fcff >= 2L) dcf_app <- dcf_app + 15
  if (cfo_ok) dcf_app <- dcf_app + 10
  if (is.finite(rev_g)) dcf_app <- dcf_app + 10
  if (isTRUE(is_fcf_pos)) dcf_app <- dcf_app + 15
  if (is.finite(last_fcff) && last_fcff > 0) dcf_app <- dcf_app + 10
  if (is.finite(equity) && equity > 0) dcf_app <- dcf_app + 10
  if (gordon_blocked) dcf_app <- dcf_app - 15
  if (is.finite(equity) && equity <= 0 && is.finite(ni) && ni < 0) dcf_app <- dcf_app - 25
  dcf_stab <- if (!is.finite(fcf_cv)) 40 else if (fcf_cv <= 0.5) 85 else if (fcf_cv <= 0.75) 65 else 30
  if (!isTRUE(is_fcf_pos)) dcf_stab <- min(dcf_stab, 25)
  dcf_fcst <- if (isTRUE(is_fcf_stable) && is.finite(rev_g)) 80 else if (isTRUE(is_fcf_pos)) 60 else if (is.finite(rev_g) && cfo_ok) 45 else 25
  # Interest is an operating item for banks, insurers, and utilities, so unlevered FCFF is a weaker primary claim.
  if (is_book) {
    dcf_app <- dcf_app - 45
    dcf_fcst <- dcf_fcst - 20
  }
  dcf_ind <- if (is_holding) 35 else if (is_book) 40 else if (is_intang) 75 else 70
  dcf_data <- 25 + if (!is.null(conf_in$has_roe)) 0 else 0
  dcf_data <- 30 + (if (cfo_ok) 25 else 10) + (if (is.finite(equity)) 20 else 0) + (if (n_fcff >= 3L) 15 else 0)

  rows <- list(
    .vg_score_row(
      "dcf", dcf_app, dcf_data, dcf_stab, if (cfo_ok) 75 else 45, dcf_fcst, dcf_ind,
      notes = c(if (gordon_blocked) "final FCFF negative; do not Gordon the base" else "FCFF path available"),
      gordon_blocked = gordon_blocked
    )
  )
  pb_app <- if (isTRUE(pb_view$blocked)) 0 else pb_view$score
  pb_ind <- if (!isTRUE(pb_view$industry_pb_ok)) 25 else if (is_book) 90 else if (is_intang) 30 else 55
  rows[[length(rows) + 1L]] <- .vg_score_row(
    "pb", pb_app,
    data_quality = if (is.finite(equity) && equity > 0) 70 else 10,
    stability = if (is.finite(roe)) 60 else 30,
    accounting = if (is_intang) 30 else if (is_book) 85 else 55,
    forecast = if (is.finite(roe) && roe > 0) 60 else 25,
    industry = pb_ind,
    notes = if (isTRUE(pb_view$blocked)) "P/B blocked" else "P/B applicability"
  )
  ri_app <- if (is.finite(ni) && ni > 0 && is.finite(equity) && equity > 0 && is.finite(roe) && roe > 0) 80 else 15
  if (gordon_blocked && ri_app >= 80) ri_app <- ri_app + 10
  rows[[length(rows) + 1L]] <- .vg_score_row(
    "ri", ri_app, if (is.finite(roe)) 70 else 20, if (is.finite(roe) && roe > 0) 65 else 20,
    70, if (is.finite(roe) && roe > 0) 60 else 20, if (is_book) 70 else 55,
    notes = "RI formula unchanged; score is selection only"
  )
  ddm_app <- if (isTRUE(is_div_stable)) 75 else 15
  rows[[length(rows) + 1L]] <- .vg_score_row(
    "ddm", ddm_app, if (isTRUE(is_div_stable)) 70 else 25, if (isTRUE(is_div_stable)) 80 else 20,
    50, if (isTRUE(is_div_stable)) 65 else 20, if (is_book) 60 else 50,
    notes = "DDM formula unchanged; score is selection only"
  )
  nav_app <- if (is_holding && is.finite(equity) && equity > 0) 85 else 10
  rows[[length(rows) + 1L]] <- .vg_score_row(
    "nav", nav_app, if (is.finite(equity)) 60 else 15, 50,
    if (is_holding) 80 else 30, 40, if (is_holding) 85 else 25,
    notes = "NAV formula unchanged; score is selection only"
  )
  scores <- do.call(rbind, rows)
  rownames(scores) <- NULL

  eligible <- scores[scores$applicability >= .VG_MIN_APP & scores$overall >= .VG_MIN_OVERALL, , drop = FALSE]
  inapplicable <- scores$model[scores$applicability < .VG_CROSS_APP]
  if (!nrow(eligible)) {
    return(pack(
      "insufficient", NULL, NULL, "無適用模型",
      "沒有模型同時通過適用性與信心門檻。不輸出假精確合理價值，也不把低品質模型平均成區間。",
      scores = scores, inapplicable = as.character(inapplicable)
    ))
  }
  eligible <- eligible[order(-eligible$overall, -eligible$applicability, eligible$model), , drop = FALSE]
  primary <- as.character(eligible$model[1])
  primary_row <- scores[scores$model == primary, , drop = FALSE]
  rest <- scores[scores$lens != primary_row$lens[1] & scores$applicability >= .VG_CROSS_APP, , drop = FALSE]
  secondary <- NULL
  role <- "none"
  if (nrow(rest)) {
    rest <- rest[order(-rest$overall, -rest$applicability), , drop = FALSE]
    secondary <- as.character(rest$model[1])
    role <- if (rest$applicability[1] >= .VG_MIN_APP && !isTRUE(rest$gordon_blocked[1])) "valuation" else "cross_check"
  }
  # When positive earnings and negative FCFF make RI primary, the cash-flow model
  # is the relevant cross-check. It is not an equal-weight valuation band.
  if (identical(primary, "ri") && isTRUE(gordon_blocked)) {
    dcf_row <- scores[scores$model == "dcf", , drop = FALSE]
    if (nrow(dcf_row) == 1L && dcf_row$applicability[1] >= .VG_CROSS_APP) {
      secondary <- "dcf"
      role <- "cross_check"
    }
  }
  sp <- primary_row$overall[1]
  ss <- if (!is.null(secondary)) scores$overall[scores$model == secondary][1] else NA_real_
  sec_blocked <- if (!is.null(secondary)) isTRUE(scores$gordon_blocked[scores$model == secondary][1]) else FALSE
  if (!is.finite(ss)) {
    wp <- 1; ws <- 0; role <- "none"
  } else if (identical(role, "cross_check") || sec_blocked) {
    role <- "cross_check"
    ws <- min(0.25, ss / (sp + ss))
    wp <- 1 - ws
  } else {
    wp <- sp / (sp + ss)
    ws <- 1 - wp
  }
  suggest_two <- identical(primary, "dcf") && (is.finite(rev_g) && rev_g > 8 || !isTRUE(is_fcf_stable))
  ctype <- if (is_holding && identical(primary, "nav")) "holding_asset"
  else if (is_book && identical(primary, "pb")) "financial"
  else if (identical(primary, "ri") && gordon_blocked && is.finite(ni) && ni > 0) "ni_pos_fcff_neg"
  else if (is.finite(rev_g) && rev_g > 12 && identical(primary, "dcf")) "growth"
  else if (identical(primary, "dcf")) "mature"
  else "mixed"
  reason <- paste0(
    "主模型 ", primary, "（綜合信心 ", round(sp, 1),
    "）。權重隨信心分數，不預設等權。",
    if (is.null(secondary)) " 無適用副模型。" else paste0(
      " 副模型 ", secondary, " 角色=", role, "，建議權重 ",
      round(100 * ws, 1), "% / ", round(100 * wp, 1), "%。"
    )
  )
  reason_en <- paste0(
    "Primary model ", primary, " (overall confidence ", round(sp, 1),
    "). Weights follow confidence scores; they are not preset to be equal. ",
    if (is.null(secondary)) "No applicable secondary model." else paste0(
      "Secondary model ", secondary, " role=", role, ", suggested weights ",
      round(100 * ws, 1), "% / ", round(100 * wp, 1), "%."
    )
  )
  pack(
    ctype, primary, secondary,
    summary_method = paste0(.model_label(primary), if (!is.null(secondary)) paste0(" / ", .model_label(secondary), " (", role, ")") else ""),
    reason = reason,
    reason_en = reason_en,
    suggest_two_stage = suggest_two,
    scores = scores,
    secondary_role = role,
    weight_primary = wp,
    weight_secondary = if (is.null(secondary)) NA_real_ else ws,
    inapplicable = as.character(inapplicable)
  )
}
