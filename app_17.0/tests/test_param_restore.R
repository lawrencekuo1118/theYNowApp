# test_param_restore.R — restore CSV export / parse (no Shiny session)
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

source(file.path("..", "param_audit.R"), encoding = "UTF-8")

reg <- ynow_tracked_param_registry()
stopifnot(nrow(reg) == 65L)
stopifnot(length(unique(vapply(reg, length, integer(1)))) == 1L)
stopifnot(all(c("input_id", "section", "label", "tab", "input_type") %in% names(reg)))
stopifnot(all(grepl("^mod_fcf-", reg$input_id[reg$section == "FCF"])))

inp <- setNames(as.list(rep(2.5, nrow(reg))), reg$input_id)
for (i in seq_len(nrow(reg))) {
  typ <- reg$input_type[[i]]
  if (identical(typ, "checkbox")) inp[[i]] <- TRUE
  else if (typ %in% c("radio", "select")) {
    # Use plausible choice values per field
    id <- reg$input_id[[i]]
    inp[[i]] <- if (grepl("mode|claim|method|source|agg|basis|stage|purpose", id)) {
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
      else if (grepl("industry", id)) "sc.Foundry"
      else "gordon"
    } else "gordon"
  }
}
inp[["sgr"]] <- 2.75
inp[["wacc_gordon"]] <- 9.06
inp[["years"]] <- 5

df <- ynow_param_restore_export_df(inp, ticker = "TSM", market_mode = "US")
stopifnot(nrow(df) == 69L)
stopifnot(identical(df$InputId[[1]], "_meta.format"))
stopifnot(any(df$InputId == "sgr" & df$Value == "2.75"))

tmp <- tempfile(fileext = ".csv")
utils::write.csv(df, tmp, row.names = FALSE, fileEncoding = "UTF-8")
parsed <- ynow_param_restore_parse_file(tmp)
stopifnot(isTRUE(parsed$ok))
stopifnot(identical(parsed$meta$ticker, "TSM"))
stopifnot(identical(parsed$meta$market_mode, "US"))
stopifnot(nrow(parsed$rows) == 65L)
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
  Section = c("Meta", "Perpetual Growth"),
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

cat("test_param_restore.R: OK\n")
