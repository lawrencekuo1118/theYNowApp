# Offline probe: TSM / SKHY industry_key mapping path (Blue Chip Lab)
# Usage (from repo root or app_17.0):
#   Rscript app_17.0/tests/debug_tsm_skhy_industry.R
# Writes NDJSON to /opt/cursor/logs/debug.log via instrumented lab_*.R

root <- getwd()
app_dir <- if (basename(root) == "tests") {
  dirname(root)
} else if (dir.exists(file.path(root, "app_17.0"))) {
  file.path(root, "app_17.0")
} else if (file.exists(file.path(root, "lab_us_universe.R"))) {
  root
} else {
  stop("Cannot locate app_17.0")
}

# lab_industry_method.R sources sibling files by relative path
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(app_dir)

`%||%` <- function(a, b) if (is.null(a)) b else a

source("industry_standards.R", local = FALSE, encoding = "UTF-8")
source("lab_sp500_universe.R", local = FALSE, encoding = "UTF-8")
source("lab_us_universe.R", local = FALSE, encoding = "UTF-8")
source("lab_tw_universe.R", local = FALSE, encoding = "UTF-8")
if (file.exists("lab_concept_groups.R")) {
  source("lab_concept_groups.R", local = FALSE, encoding = "UTF-8")
}
source("lab_industry_method.R", local = FALSE, encoding = "UTF-8")

probe <- c("TSM", "SKHY", "MU", "NVDA")
u <- lab_get_us_universe(FALSE)
sp <- lab_get_sp500_universe(FALSE)
cands <- lab_us_quality_candidates()

u_tk <- toupper(trimws(as.character(u$ticker)))
rows <- lapply(probe, function(tk) {
  i <- match(tk, u_tk)
  data.frame(
    ticker = tk,
    in_us = !is.na(i),
    name = if (is.na(i)) NA_character_ else as.character(u$name[[i]]),
    industry_key = if (is.na(i)) NA_character_ else as.character(u$industry_key[[i]]),
    industry_raw = if (is.na(i)) NA_character_ else as.character(u$industry_raw[[i]]),
    in_sp500 = tk %in% toupper(trimws(as.character(sp$ticker))),
    in_overrides = tk %in% names(lab_ticker_industry_overrides()),
    stringsAsFactors = FALSE
  )
})
out <- do.call(rbind, rows)
out$industry_label <- vapply(out$industry_key, function(k) {
  if (identical(k, LAB_UNMAPPED_KEY)) LAB_UNMAPPED_LABEL
  else if (k %in% names(industry_labels)) as.character(industry_labels[[k]])
  else as.character(k)
}, character(1))

# #region agent log
tryCatch({
  cat(sprintf(
    paste0(
      '{"hypothesisId":"E","runId":"post-fix","location":"debug_tsm_skhy_industry.R",',
      '"message":"offline probe summary","data":{"rows":[%s],"unmapped_bucket_n":%d,',
      '"sc_Foundry_n":%d,"sc_Memory_n":%d},"timestamp":%s}\n'
    ),
    paste(vapply(seq_len(nrow(out)), function(i) {
      sprintf(
        '{"ticker":"%s","industry_key":"%s","industry_label":"%s","in_sp500":%s,"in_overrides":%s}',
        out$ticker[[i]], out$industry_key[[i]],
        gsub('"', "", out$industry_label[[i]]),
        ifelse(out$in_sp500[[i]], "true", "false"),
        ifelse(out$in_overrides[[i]], "true", "false")
      )
    }, character(1)), collapse = ","),
    length(cands[[LAB_UNMAPPED_KEY]] %||% character(0)),
    length(cands[["sc.Foundry"]] %||% character(0)),
    length(cands[["sc.Memory"]] %||% character(0)),
    format(as.numeric(Sys.time()) * 1000, scientific = FALSE)
  ), file = "/opt/cursor/logs/debug.log", append = TRUE)
}, error = function(e) message("log write failed: ", conditionMessage(e)))
# #endregion

print(out)
cat("\nlab.Unmapped candidates:", length(cands[[LAB_UNMAPPED_KEY]] %||% character(0)), "\n")
cat("TSM in Unmapped bucket:", "TSM" %in% (cands[[LAB_UNMAPPED_KEY]] %||% character(0)), "\n")
cat("SKHY in Unmapped bucket:", "SKHY" %in% (cands[[LAB_UNMAPPED_KEY]] %||% character(0)), "\n")
cat("TSM in Foundry bucket:", "TSM" %in% (cands[["sc.Foundry"]] %||% character(0)), "\n")
cat("SKHY in Memory bucket:", "SKHY" %in% (cands[["sc.Memory"]] %||% character(0)), "\n")
cat("TW code 24 →", lab_map_tw_industry_to_key("24"), "\n")
cat("DONE\n")
