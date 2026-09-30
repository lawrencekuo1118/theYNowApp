# test_param_restore.R — restore CSV export / parse (no Shiny session)
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

source(file.path("..", "param_audit.R"), encoding = "UTF-8")

reg <- ynow_tracked_param_registry()
stopifnot(nrow(reg) >= 120L)
stopifnot(length(unique(vapply(reg, length, integer(1)))) == 1L)
stopifnot(all(c("input_id", "section", "label", "tab", "input_type", "full_only", "note") %in% names(reg)))
stopifnot(!anyDuplicated(reg$input_id))
stopifnot(all(grepl("^mod_fcf-", reg$input_id[reg$section == "FCF"])))
stopifnot(all(c(
  "mod_fcf-apply_capex_spike_smooth", "mod_fcf-capex_spike_mult", "mod_fcf-capex_spike_avg_years",
  "mod_fcf-apply_g_ceiling", "bt_net_margin", "chk_hfv_veto", "lab_im_eq_only", "session_ccy_pick",
  "macro-hist_period", "macro-bubble_top_n", "macro-bubble_attr_period", "macro-theme_key",
  "macro-bubble_buffett_asof"
) %in% reg$input_id))
stopifnot(all(reg$full_only[reg$input_id %in% c(
  "macro-theme_key", "macro-hist_period", "macro-bubble_top_n",
  "macro-bubble_attr_period", "macro-bubble_buffett_asof"
)]))

inp <- setNames(as.list(rep(2.5, nrow(reg))), reg$input_id)
for (i in seq_len(nrow(reg))) {
  typ <- reg$input_type[[i]]
  id <- reg$input_id[[i]]
  if (identical(typ, "checkbox")) {
    inp[[i]] <- TRUE
  } else if (typ %in% c("checkboxGroup", "select_multi", "picker")) {
    inp[[i]] <- c("dcf", "ddm")
  } else if (identical(typ, "daterange")) {
    inp[[i]] <- as.Date(c("2020-01-01", "2024-12-31"))
  } else if (identical(typ, "text")) {
    inp[[i]] <- "keyword"
  } else if (typ %in% c("radio", "select", "slider")) {
    inp[[i]] <- if (grepl("mode|claim|method|source|agg|basis|stage|purpose|window|oos|freq|lb_mode|primary|pool|max_n|cluster_[xy]|form", id)) {
      if (grepl("dcf_claim", id)) "fcff"
      else if (grepl("ddm_mode|dcf_mode", id)) "gordon"
      else if (grepl("basis", id)) "bvps"
      else if (grepl("target_mode", id)) "multiples"
      else if (grepl("bottomup_agg", id)) "mean"
      else if (grepl("growth_method|perpetual", id)) "fundamental"
      else if (grepl("lifecycle", id)) "auto"
      else if (grepl("apply_source|bl_source", id)) "summary"
      else if (grepl("roe_method", id)) "constant"
      else if (grepl("purpose", id)) "valuation"
      else if (grepl("industry_choice", id)) "sc.Foundry"
      else if (grepl("session_ccy", id)) "USD"
      else if (grepl("bt_fv_replay|dc_user_primary", id)) "dcf"
      else if (grepl("bt_fv_conv_window|bt_nav_window", id)) "all"
      else if (grepl("bt_fv_oos", id)) "realized"
      else if (grepl("bt_fv_analysis_freq", id)) "quarterly"
      else if (grepl("lab_im_lb_mode", id)) "overall"
      else if (grepl("lab_im_max_n", id)) "25"
      else if (grepl("lab_cluster_x", id)) "ROE"
      else if (grepl("lab_cluster_y", id)) "Operating_Margin"
      else if (grepl("lab_im_pool_rank", id)) "market"
      else if (identical(typ, "slider")) 0.5
      else "gordon"
    } else if (identical(typ, "slider")) {
      0.5
    } else {
      "gordon"
    }
  }
}
inp[["sgr"]] <- 2.75
inp[["wacc_gordon"]] <- 9.06
inp[["years"]] <- 5

df <- ynow_param_restore_export_df(inp, ticker = "TSM", market_mode = "US")
stopifnot(nrow(df) == nrow(reg) + 4L)
stopifnot(identical(df$InputId[[1]], "_meta.format"))
stopifnot(any(df$InputId == "sgr" & df$Value == "2.75"))
stopifnot(any(df$InputId == "mod_fcf-capex_spike_mult"))

tmp <- tempfile(fileext = ".csv")
utils::write.csv(df, tmp, row.names = FALSE, fileEncoding = "UTF-8")
parsed <- ynow_param_restore_parse_file(tmp)
stopifnot(isTRUE(parsed$ok))
stopifnot(identical(parsed$meta$ticker, "TSM"))
stopifnot(identical(parsed$meta$market_mode, "US"))
stopifnot(nrow(parsed$rows) == nrow(reg))
stopifnot("sgr" %in% parsed$rows$input_id)
stopifnot(parsed$rows$value[parsed$rows$input_id == "sgr"] == "2.75")

# Legacy CapEx ids remap
legacy <- data.frame(
  InputId = c("apply_capex_spike_smooth", "capex_spike_mult"),
  Value = c("TRUE", "1.35"),
  Type = c("checkbox", "numeric"),
  stringsAsFactors = FALSE
)
tmp2 <- tempfile(fileext = ".csv")
utils::write.csv(legacy, tmp2, row.names = FALSE)
parsed2 <- ynow_param_restore_parse_file(tmp2)
stopifnot(isTRUE(parsed2$ok))
stopifnot("mod_fcf-apply_capex_spike_smooth" %in% parsed2$rows$input_id)
stopifnot("mod_fcf-capex_spike_mult" %in% parsed2$rows$input_id)

# Human snapshot fallback (Parameter + Current Value)
human <- data.frame(
  Section = c("Meta", "SGR"),
  Parameter = c("Ticker", "SGR / terminal g (%)"),
  `Current Value` = c("AAPL", "3.1"),
  Formula = c("", ""),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
tmp3 <- tempfile(fileext = ".csv")
utils::write.csv(human, tmp3, row.names = FALSE)
parsed3 <- ynow_param_restore_parse_file(tmp3)
stopifnot(isTRUE(parsed3$ok))
stopifnot(identical(parsed3$meta$ticker, "AAPL"))
stopifnot(parsed3$rows$value[parsed3$rows$input_id == "sgr"] == "3.1")

# Snapshot helper builds registry rows
fake_input <- as.environment(inp)
class(fake_input) <- c("reactivevalues", "environment")
# Use list-style [[ for non-shiny input mock
inp_obj <- as.list(inp)
class(inp_obj) <- "list"
snap <- ynow_snapshot_registry_rows(inp_obj, lite = FALSE, extras = list(
  c("Meta", "Downloaded At", "now", "ts")
))
stopifnot(nrow(snap) == nrow(reg) + 1L)
stopifnot("CapEx spike smooth" %in% snap$Parameter)
snap_lite <- ynow_snapshot_registry_rows(inp_obj, lite = TRUE)
stopifnot(nrow(snap_lite) == sum(!reg$full_only))
stopifnot(sum(!reg$full_only) == 13L)
stopifnot(!"Gate: HFV veto" %in% snap_lite$Parameter)
stopifnot(!"CapEx spike smooth" %in% snap_lite$Parameter)
stopifnot(!"SGR / terminal g (%)" %in% snap_lite$Parameter)
stopifnot("Earnings quality only" %in% snap_lite$Parameter)
stopifnot("Pool rank logic" %in% snap_lite$Parameter)
stopifnot("Session Currency" %in% snap_lite$Parameter)
stopifnot(!"Overlay window" %in% snap_lite$Parameter)
stopifnot(!"Bubble Top N" %in% snap_lite$Parameter)
stopifnot("Overlay window" %in% snap$Parameter)
stopifnot(all(reg$input_id[!reg$full_only] %in% c(
  "session_ccy_pick", "industry_choice",
  "lab_im_pool_rank", "lab_im_concepts", "lab_im_max_n", "lab_im_max_n_custom",
  "lab_im_eq_only", "lab_im_include_adr", "lab_im_lb_mode",
  "lab_cluster_k", "lab_cluster_x", "lab_cluster_y", "lab_cluster_focus"
)))

# APP_DEFAULTS includes Lite Blue Chip seeds used by defaults table
cfg_txt <- paste(readLines(file.path("..", "default_config.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(grepl("lab_im_pool_rank\\s*=\\s*\"mcap\"", cfg_txt))
stopifnot(grepl("lab_im_concepts\\s*=\\s*character\\(0\\)", cfg_txt))
stopifnot(grepl("macro_hist_period\\s*=\\s*\"1y\"", cfg_txt))
stopifnot(grepl("macro_bubble_top_n\\s*=\\s*\"5\"", cfg_txt))

cat("test_param_restore.R: OK\n")
