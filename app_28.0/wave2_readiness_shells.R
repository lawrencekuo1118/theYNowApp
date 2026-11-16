# Eager readiness shells for DCF / DDM / RI / P/B / NAV / SOTP.
# The shell inside each tabItem owns the input nodes. The lazy body omits those ids.
# Module servers stay registered at session start.

ynow_wave2_row <- function(tab, shell, body) {
  stopifnot(is.character(tab), length(tab) == 1L, nzchar(tab))
  stopifnot(is.function(shell), is.function(body))
  structure(
    list(tab = tab, shell = shell, body = body),
    class = "ynow_wave2_row"
  )
}

.ynow_wave2_registry <- function() {
  list(
    dcf_calculator = ynow_wave2_row(
      "dcf_calculator",
      shell = function() .ynow_dcf_shell_ui(),
      body = function() .ynow_dcf_body_ui()
    ),
    ddm_calculator = ynow_wave2_row(
      "ddm_calculator",
      shell = function() .ynow_ddm_shell_ui(),
      body = function() .ynow_page_ui_ddm()
    ),
    ri_calculator = ynow_wave2_row(
      "ri_calculator",
      shell = function() ri_module_shell_ui("mod_ri"),
      body = function() ri_module_body_ui("mod_ri")
    ),
    pb_calculator = ynow_wave2_row(
      "pb_calculator",
      shell = function() pb_asset_module_shell_ui("mod_pb"),
      body = function() pb_asset_module_body_ui("mod_pb")
    ),
    nav_calculator = ynow_wave2_row(
      "nav_calculator",
      shell = function() nav_module_shell_ui("mod_nav"),
      body = function() nav_module_body_ui("mod_nav")
    ),
    sotp_calculator = ynow_wave2_row(
      "sotp_calculator",
      shell = function() sotp_module_shell_ui("mod_sotp"),
      body = function() sotp_module_body_ui("mod_sotp")
    )
  )
}

.ynow_wave2_tab_item <- function(tab) {
  tab <- as.character(tab)[1]
  row <- .ynow_wave2_registry()[[tab]]
  if (is.null(row)) stop(sprintf("unknown wave2 tab: %s", tab), call. = FALSE)
  tabItem(
    tabName = tab,
    row$shell(),
    uiOutput(.ynow_lazy_host_id(tab))
  )
}

.ynow_wave2_builders <- function() {
  reg <- .ynow_wave2_registry()
  stats::setNames(
    lapply(reg, function(row) row$body),
    vapply(reg, function(row) row$tab, character(1))
  )
}

# Ids that must exist in the first HTML. holdco_discount is included because
# model integrity reads it before the tab opens. A missing input is NA, not 0.
ynow_wave2_shell_ids <- function(tab) {
  tab <- as.character(tab)[1]
  ids <- list(
    dcf_calculator = c(
      "wacc_gordon", "wacc_stage1", "wacc_stage2", "yr_stage1", "g_stage1",
      "wacc_re", "wacc_rd", "wacc_tax", "use_estimated_re",
      "capm_rf", "capm_rm", "capm_beta", "sync_gs_beta",
      "rd_interest_expense", "rd_interest_bearing_debt",
      "g_growth_method", "custom_g",
      "dcf_beta_u_apply_source"
    ),
    ddm_calculator = c(
      "mod_ddm-d0", "mod_ddm-g", "mod_ddm-ke", "mod_ddm-ddm_mode",
      "mod_ddm-sync_g", "mod_ddm-est_eps", "mod_ddm-g_stage1", "mod_ddm-yr_stage1",
      "ddm_capm_rf", "ddm_capm_rm", "ddm_capm_beta", "ddm_sync_gs_beta",
      "ddm_use_estimated_re", "ddm_beta_u_apply_source"
    ),
    ri_calculator = c(
      "mod_ri-b0", "mod_ri-ri_ke", "mod_ri-ri_g", "mod_ri-ri_years",
      "mod_ri-ri_payout", "mod_ri-ri_roe", "mod_ri-roe_method",
      "mod_ri-roe_terminal", "mod_ri-roe_industry", "mod_ri-roe_custom_txt",
      "ri_beta_u_apply_source"
    ),
    pb_calculator = c(
      "mod_pb-basis", "mod_pb-bvps", "mod_pb-tbvps", "mod_pb-navps",
      "mod_pb-pb_low", "mod_pb-pb_mid", "mod_pb-pb_high", "mod_pb-use_industry_pb",
      "mod_pb-holdco_discount"
    ),
    nav_calculator = c(
      "mod_nav-navps", "mod_nav-nav_low", "mod_nav-nav_mid", "mod_nav-nav_high",
      "mod_nav-holdco_discount"
    ),
    sotp_calculator = c(
      "mod_sotp-shares", "mod_sotp-cash", "mod_sotp-debt"
    )
  )
  out <- ids[[tab]]
  if (is.null(out)) stop(sprintf("unknown wave2 tab: %s", tab), call. = FALSE)
  out
}
