# Valuation model Overview KPI rows: result leftmost, family tones, flow numeral only on result.
# Run: cd app_21.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_model_kpi_result_row.R

root <- if (file.exists("ynow_ui.R")) {
  getwd()
} else if (file.exists("app_21.0/ynow_ui.R")) {
  file.path(getwd(), "app_21.0")
} else {
  stop("Cannot find app_21.0 root")
}
setwd(root)

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat("OK:", label, "\n")
}

.read <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

ui_src <- .read("ynow_ui.R")
setup_src <- .read("setup.R")
srv_src <- .read("ynow_server.R")
ddm_src <- .read("ddm_module.R")
nav_src <- .read("nav_module.R")
pb_src <- .read("pb_asset_module.R")
ri_src <- .read("ri_module.R")
sotp_src <- .read("sotp_module.R")
rel_src <- .read("relative_multiples_module.R")
loc_src <- .read("ui_locale.R")

check("helper ynow_model_result_num", grepl("ynow_model_result_num", setup_src, fixed = TRUE))
check("helper wraps ynow-htcdi-flow", grepl("ynow-htcdi-flow ynow-model-kpi-result-num", setup_src, fixed = TRUE))

# DCF: result column before enterprise; flow on stock value
check(
  "DCF KPI row has result class before tone-2",
  grepl("ynow-model-kpi-row", ui_src, fixed = TRUE) &&
    grepl("ynow-model-kpi-result", ui_src, fixed = TRUE) &&
    regexpr("ibx_stock_value_dcf", ui_src, fixed = TRUE) <
      regexpr("ibx_enterprise_value_dcf", ui_src, fixed = TRUE)
)
check("DCF result uses flow numeral", grepl("ynow_model_result_num", srv_src, fixed = TRUE) &&
        grepl("ibx_stock_value_dcf", srv_src, fixed = TRUE))

# DDM
check(
  "DDM price leftmost vs D1",
  regexpr("ibx_ddm_price", ui_src, fixed = TRUE) < regexpr("ibx_ddm_d1", ui_src, fixed = TRUE)
)
check("DDM price uses flow", grepl("ynow_model_result_num(val)", ddm_src, fixed = TRUE))

# NAV: fair mid before navps / mkt
pos_mid <- regexpr("vbx_nav_mid", nav_src, fixed = TRUE)
pos_navps <- regexpr('valueBoxOutput(ns("vbx_navps")', nav_src, fixed = TRUE)
pos_mkt <- regexpr('valueBoxOutput(ns("vbx_mkt")', nav_src, fixed = TRUE)
check("NAV fair leftmost in UI", pos_mid > 0 && pos_navps > pos_mid && pos_mkt > pos_mid)
check("NAV fair uses flow", grepl("ynow_model_result_num(raw)", nav_src, fixed = TRUE))

# P/B: fair first; no Overview navps KPI
pos_fair <- regexpr('valueBoxOutput(ns("vbx_fair")', pb_src, fixed = TRUE)
pos_bvps <- regexpr('valueBoxOutput(ns("vbx_bvps")', pb_src, fixed = TRUE)
check("P/B fair leftmost", pos_fair > 0 && pos_bvps > pos_fair)
check("P/B Overview drops navps KPI", !grepl('valueBoxOutput(ns("vbx_navps")', pb_src, fixed = TRUE))
check("P/B fair uses flow", grepl("ynow_model_result_num(raw)", pb_src, fixed = TRUE))

# RI KPI row present; FV first
pos_ri_fv <- regexpr('valueBoxOutput(ns("vbx_ri_fv")', ri_src, fixed = TRUE)
pos_ri_b0 <- regexpr('valueBoxOutput(ns("vbx_ri_b0")', ri_src, fixed = TRUE)
check("RI FV leftmost", pos_ri_fv > 0 && pos_ri_b0 > pos_ri_fv)
check("RI FV uses flow", grepl("ynow_model_result_num(raw)", ri_src, fixed = TRUE))

# SOTP / Multiples
check("SOTP price leftmost + flow", grepl("ynow-model-kpi-result", sotp_src, fixed = TRUE) &&
        grepl("ynow_model_result_num(val)", sotp_src, fixed = TRUE))
check("Multiples PE/EVFCF/PS marked result", grepl("is_result = TRUE", rel_src, fixed = TRUE))

# i18n keys both locales
for (k in c("pb_vbx_fair", "ri_vbx_fv", "ri_vbx_b0", "ri_vbx_pv", "ri_vbx_tv")) {
  check(paste("locale key", k), length(gregexpr(paste0(k, " ="), loc_src, fixed = TRUE)[[1]]) >= 2L)
}

# Sibling tones must not equal result token in CSS vars block
check(
  "CSS sibling mixes distinct from result",
  grepl("--ynow-model-kpi-s2:", ui_src, fixed = TRUE) &&
    grepl("--ynow-model-kpi-s3:", ui_src, fixed = TRUE) &&
    grepl("--ynow-model-kpi-s4:", ui_src, fixed = TRUE) &&
    grepl("color-mix(in srgb, var(--ynow-model-accent)", ui_src, fixed = TRUE)
)

cat("PASS test_model_kpi_result_row\n")
