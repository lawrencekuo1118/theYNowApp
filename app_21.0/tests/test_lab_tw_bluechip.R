#!/usr/bin/env Rscript
# TW Blue Chip Top-10: board filter, gate, sort, no padding (offline)
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
if (!exists("get_market_mode", mode = "function")) get_market_mode <- function() "TW"

source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_tw_universe.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

u <- lab_get_tw_universe(FALSE)
check("tw universe loaded", is.data.frame(u) && nrow(u) > 500L)
cands <- lab_tw_quality_candidates()
all_tk <- unique(unlist(cands, use.names = FALSE))
check("quality candidates nonempty", length(all_tk) > 500L)
ex <- toupper(as.character(u$exchange))
esb <- as.character(u$ticker[ex %in% c("ESB", "EMERGING", "TPEX_ESB")])
check("ESB excluded from bluechip", length(intersect(all_tk, esb)) == 0L)
check("ETF/index excluded", !any(all_tk %in% c("0050.TW", "0056.TW", "^TWII")))

catlg <- lab_build_industry_method_catalog(market_mode = "TW")
tks <- unique(na.omit(as.character(catlg$ticker)))
check("catalog TW/TWO only", length(tks) > 0L && all(grepl("\\.(TW|TWO)$", tks, ignore.case = TRUE)))

.mk <- function(tk, cagr, fs = 8L) {
  data.frame(
    ticker = tk, ok = TRUE, f_score = fs, quality_flag = 1L,
    is_quality = TRUE, is_quality_upside = TRUE,
    market_cap = 1e10, price = 100, fv = 100,
    upside_total_pct = cagr * 5, upside_cagr_pct = cagr, n_years = 5L,
    fv_note = NA_character_, method_used = "dcf", primary_live = "dcf",
    company_type = "mixed", company_name = tk, error = NA_character_,
    stringsAsFactors = FALSE
  )
}
scores <- rbind(
  .mk("2317.TW", 40), .mk("3008.TW", 35), .mk("1301.TW", 30),
  .mk("2379.TW", 28), .mk("2330.TW", 25), .mk("2002.TW", 22),
  .mk("2454.TW", 18), .mk("2412.TW", 15), .mk("2308.TW", 12),
  .mk("6505.TW", 11), .mk("1216.TW", 9),
  .mk("2881.TW", 8, 6L), .mk("6669.TW", 50, 5L)
)
merged <- lab_merge_catalog_scores(catlg, scores = scores, evaluated_only = TRUE)
check("merge keeps industry_label", "industry_label" %in% names(merged))
lb <- lab_quality_leaderboard(
  merged, top_n = 10L, gate_only = TRUE, eq_only = FALSE, scope = "overall"
)
check("top10 size", nrow(lb) == 10L)
check("industry labels filled", !any(lb$產業 %in% c("—", "", NA_character_)))
check("F<7 excluded", !any(grepl("6669|2881", as.character(lb$代號))))
cagr <- as.numeric(gsub("[+%]", "", lb[["年化估值漲幅"]]))
check("CAGR descending", length(cagr) == 10L && all(diff(cagr) <= 0))
pool <- lab_leaderboard_pool(merged, gate_only = TRUE)
check("pool drops F<7", nrow(pool) == 11L)
strip <- function(x) sub("\\.(TW|TWO)$", "", x, ignore.case = TRUE)
check(
  "top10 order matches pool",
  identical(strip(utils::head(pool$ticker, 10)), strip(as.character(lb$代號)))
)
tiny <- merged[merged$ticker %in% c("2330.TW", "2454.TW"), , drop = FALSE]
lb2 <- lab_quality_leaderboard(tiny, top_n = 10L, gate_only = TRUE, scope = "overall")
check("no pad under 10", nrow(lb2) == 2L)

lb_f <- lab_quality_leaderboard(
  merged, top_n = 10L, gate_only = TRUE, scope = "by_industry",
  industry_filter = "sc.Foundry"
)
check("by_industry Foundry nonempty", nrow(lb_f) >= 1L)
check("by_industry only Foundry", all(grepl("半導體|晶圓", lb_f$產業)))

if (fail > 0L) {
  cat(fail, " TW bluechip check(s) failed.\n", sep = "")
  quit(status = 1)
}
cat("PASS test_lab_tw_bluechip\n")
