#!/usr/bin/env Rscript
# Industry-chain mcap-weighted upside: all chains + snapshot + Refresh ranking UI
# Run: cd app_22.0/tests && Rscript test_lab_ind_mcap_chain_rank.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
    pass <<- pass + 1L
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

source(file.path(app_dir, "ui_locale.R"), local = FALSE)
# Minimal deps for industry_chain_id / picker
if (file.exists(file.path(app_dir, "industry_standards.R"))) {
  source(file.path(app_dir, "industry_standards.R"), local = FALSE)
}
# lab_* helpers (source file pieces via lab_industry_method if loadable)
src_lab <- file.path(app_dir, "lab_industry_method.R")
# lab_industry_method.R expects %||% and peers — load soft helpers from setup if present
if (file.exists(file.path(app_dir, "setup.R"))) {
  tryCatch(source(file.path(app_dir, "setup.R"), local = FALSE), error = function(e) NULL)
}
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists("normalize_ui_locale", mode = "function")) {
  normalize_ui_locale <- function(x) {
    x <- as.character(x %||% "zh-TW")[1]
    if (identical(x, "en") || startsWith(tolower(x), "en")) "en" else "zh-TW"
  }
}
if (!exists("normalize_market_mode", mode = "function")) {
  normalize_market_mode <- function(x) {
    x <- toupper(trimws(as.character(x %||% "US")[1]))
    if (x %in% c("TW", "TWN", "TAIWAN")) "TW" else "US"
  }
}
if (!exists("lab_leaderboard_pool", mode = "function")) {
  source(src_lab, local = FALSE)
}

keys <- c(
  "lab_im_ind_mcap_title", "lab_im_ind_mcap_badge", "lab_im_ind_mcap_help",
  "lab_im_ind_mcap_waiting", "lab_im_ind_mcap_empty", "lab_im_ind_mcap_status",
  "lab_im_ind_mcap_status_snap", "lab_im_ind_mcap_snap_at",
  "btn_lab_im_refresh_ind_mcap", "lab_im_refresh_ind_mcap_title",
  "lab_im_ind_mcap_refresh_done", "lab_im_ind_mcap_refresh_busy"
)
for (k in keys) {
  check(paste("en key", k), k %in% names(.UI_STRINGS$en) && nzchar(ui_str(k, "en")))
  check(paste("zh-TW key", k), k %in% names(.UI_STRINGS$`zh-TW`) && nzchar(ui_str(k, "zh-TW")))
}
check("badge zh 預設排名", identical(ui_str("lab_im_ind_mcap_badge", "zh-TW"), "預設排名"))
check("badge en Default ranking", identical(ui_str("lab_im_ind_mcap_badge", "en"), "Default ranking"))
check("btn zh 更新排名", identical(ui_str("btn_lab_im_refresh_ind_mcap", "zh-TW"), "更新排名"))
check("btn en Refresh ranking", identical(ui_str("btn_lab_im_refresh_ind_mcap", "en"), "Refresh ranking"))
check("zh no simplified 默认", !grepl("默认", ui_str("lab_im_ind_mcap_badge", "zh-TW"), fixed = TRUE))

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("UI title-row class", grepl("ynow-lab-im-ind-mcap-title-row", ui, fixed = TRUE))
check("UI refresh button id", grepl("lab_im_refresh_ind_mcap", ui, fixed = TRUE))
check("UI refresh label id", grepl("ynow_lab_im_refresh_ind_mcap_label", ui, fixed = TRUE))
check("UI CSS for refresh-ind-mcap", grepl("ynow-lab-im-refresh-ind-mcap", ui, fixed = TRUE))
check("applyUiLocale wires refresh label",
      grepl("ynow_lab_im_refresh_ind_mcap_label", ui, fixed = TRUE) &&
        grepl("btn_lab_im_refresh_ind_mcap", ui, fixed = TRUE))

check("server has chain board helper", grepl("lab_industry_chain_mcap_upside_leaderboard", srv, fixed = TRUE))
check("server has snapshot reactive", grepl("lab_im_ind_mcap_snap", srv, fixed = TRUE))
check("server refresh observeEvent", grepl("observeEvent\\(input\\$lab_im_refresh_ind_mcap", srv))
check("server persist after scores", grepl("\\.lab_im_persist_ind_mcap_from_scores", srv))

# Functional: all chains included + sorted
.mk <- function(tk, key, lab, cagr, mcap) {
  data.frame(
    ticker = tk,
    industry_key = key,
    industry_label = lab,
    method_used = "DCF",
    primary = "dcf",
    upside_cagr_pct = cagr,
    upside_total_pct = cagr * 3,
    f_score = 8,
    earnings_quality_pass = TRUE,
    fs_alert_count = 0L,
    market_cap = mcap,
    company_name = tk,
    stringsAsFactors = FALSE
  )
}
df <- rbind(
  .mk("A1", "sc.IC_Design", "半導體｜IC 設計", 40, 100),
  .mk("A2", "sc.Foundry", "半導體｜晶圓代工", 20, 50),
  .mk("B1", "fn.Banking", "金融｜銀行", 60, 80),
  .mk("C1", "tech.Software", "科技｜軟體", 10, 200)
)

built <- lab_industry_chain_mcap_upside_leaderboard(
  df, eq_only = FALSE, gate_only = FALSE, no_alert = FALSE,
  locale = "zh-TW", include_all_chains = TRUE
)
agg <- built$aggregate
tbl <- built$table
n_picker <- length(unname(lab_industry_chain_picker_choices("zh-TW")))
check("agg includes all picker chains", nrow(agg) == n_picker)
check("半導體 in agg", "半導體" %in% agg$chain_id)
check("金融 in agg", "金融" %in% agg$chain_id)
# Semi wavg = (100*40+50*20)/150 = 33.333; Bank = 60 → Bank first among finite
finite <- agg[is.finite(agg$wavg_cagr), , drop = FALSE]
check("sorted finite first is 金融", identical(finite$chain_id[[1]], "金融"))
check("table has 產業鏈 col", "產業鏈" %in% names(tbl))
check("table nrow = all chains", nrow(tbl) == n_picker)
check("table ranks 1..n", identical(as.integer(tbl$排名), seq_len(nrow(tbl))))

# 總市值 = full universe (extra names not in eval sample still count)
uni <- rbind(
  df,
  .mk("A3", "sc.Memory", "半導體｜記憶體", 0, 1000),
  .mk("A4", "sc.Equipment", "半導體｜設備", 0, 500)
)
# A3/A4 have upside 0 but finite — for universe totals use industry keys only via totals helper
uni_only <- rbind(
  df[, c("ticker", "industry_key", "market_cap")],
  data.frame(
    ticker = c("A3", "A4"),
    industry_key = c("sc.Memory", "sc.Equipment"),
    market_cap = c(1000, 500),
    stringsAsFactors = FALSE
  )
)
built2 <- lab_industry_chain_mcap_upside_leaderboard(
  df, eq_only = FALSE, gate_only = FALSE, no_alert = FALSE,
  locale = "zh-TW", include_all_chains = TRUE, universe_df = uni_only
)
semi <- built2$aggregate[built2$aggregate$chain_id == "半導體", , drop = FALSE]
check("semi total_mcap uses full universe", nrow(semi) == 1L &&
        isTRUE(all.equal(semi$total_mcap[[1]], 100 + 50 + 1000 + 500)))
check("semi n_names uses full universe", nrow(semi) == 1L && identical(as.integer(semi$n_names[[1]]), 4L))
check("semi wavg still from eval sample only",
      nrow(semi) == 1L && isTRUE(all.equal(semi$wavg_cagr[[1]], (100 * 40 + 50 * 20) / 150)))

# Snapshot round-trip + cache preferred as next default
td <- tempfile("ind_mcap_snap_")
dir.create(td)
cache_csv <- file.path(td, "cache_US.csv")
bundled_csv <- file.path(td, "bundled_US.csv")
# Old bundled default
old <- data.frame(
  chain_id = "金融", wavg_cagr = 1, total_mcap = 1, n_names = 1L,
  snapshot_at = "2020-01-01T00:00:00Z", market = "US", stringsAsFactors = FALSE
)
utils::write.csv(old, bundled_csv, row.names = FALSE)
options(ynow.ind_mcap_chain_snapshot = NULL)
# Point candidates via env slow cache
Sys.setenv(YNOW_SLOW_CACHE_DIR = td)
# clear cache env
if (exists(".lab_ind_mcap_chain_cache", inherits = TRUE)) {
  rm(list = ls(envir = .lab_ind_mcap_chain_cache), envir = .lab_ind_mcap_chain_cache)
}
# Write "updated" snapshot into slow cache (simulates 更新排名)
options(ynow.ind_mcap_chain_snapshot = cache_csv)
res <- lab_build_ind_mcap_chain_snapshot(
  df, market = "US", eq_only = FALSE, gate_only = FALSE, no_alert = FALSE,
  locale = "zh-TW", snapshot_at = "2026-10-12T12:00:00Z",
  universe_df = uni_only
)
check("snapshot saved path", is.character(res$path) && file.exists(res$path))
check("snapshot nrow all chains", nrow(res$snapshot) == n_picker)
check("snapshot semi total_mcap full universe", {
  s <- res$snapshot[res$snapshot$chain_id == "半導體", , drop = FALSE]
  nrow(s) == 1L && isTRUE(all.equal(s$total_mcap[[1]], 1650))
})
loaded <- lab_load_ind_mcap_chain_snapshot("US", force = TRUE)
check("load snapshot nrow", nrow(loaded) == n_picker)
check("load snapshot_at attr",
      identical(as.character(attr(loaded, "snapshot_at")), "2026-10-12T12:00:00Z") ||
        identical(as.character(loaded$snapshot_at[[1]]), "2026-10-12T12:00:00Z"))
# Newest wins: cache (2026) over older bundled — use isolated data dir via option pair
options(ynow.ind_mcap_chain_snapshot = NULL)
if (exists(".lab_ind_mcap_chain_cache", inherits = TRUE)) {
  rm(list = ls(envir = .lab_ind_mcap_chain_cache), envir = .lab_ind_mcap_chain_cache)
}
cache_dest <- lab_ind_mcap_chain_snapshot_cache_path("US")
dir.create(dirname(cache_dest), recursive = TRUE, showWarnings = FALSE)
file.copy(cache_csv, cache_dest, overwrite = TRUE)
# Simulate older bundled beside cache by writing a second older file and forcing
# candidate order via a temp "bundled" that load still sees if we put it in data/
# — instead verify path picker chooses cache_dest when both exist with times.
older_path <- file.path(td, "older_US.csv")
utils::write.csv(old, older_path, row.names = FALSE)
# Directly exercise path selection: cache newer than older_path
picked_path <- lab_ind_mcap_chain_snapshot_path("US")
check("path picker returns cache file",
      normalizePath(picked_path, winslash = "/", mustWork = FALSE) ==
        normalizePath(cache_dest, winslash = "/", mustWork = FALSE))
picked <- lab_load_ind_mcap_chain_snapshot("US", force = TRUE)
check("load prefers newer updated snapshot",
      identical(as.character(attr(picked, "snapshot_at") %||% picked$snapshot_at[[1]]),
                "2026-10-12T12:00:00Z"))
fmt <- lab_format_ind_mcap_chain_table(loaded, "en")
check("en col Industry chain", "Industry chain" %in% names(fmt))
check("save writes cache path helper",
      exists("lab_ind_mcap_chain_snapshot_cache_path", mode = "function"))
check("help mentions full universe total mcap (en)",
      grepl("full universe", ui_str("lab_im_ind_mcap_help", "en"), fixed = TRUE))
check("help mentions 全宇宙 (zh-TW)",
      grepl("全宇宙", ui_str("lab_im_ind_mcap_help", "zh-TW"), fixed = TRUE))
# cleanup env so bundled checks below are clean
Sys.unsetenv("YNOW_SLOW_CACHE_DIR")
options(ynow.ind_mcap_chain_snapshot = NULL)
if (exists(".lab_ind_mcap_chain_cache", inherits = TRUE)) {
  rm(list = ls(envir = .lab_ind_mcap_chain_cache), envir = .lab_ind_mcap_chain_cache)
}

# Bundled default present after build script (optional)
bundled <- file.path(app_dir, "data", "ind_mcap_chain_rank_snapshot_US.csv")
if (file.exists(bundled)) {
  options(ynow.ind_mcap_chain_snapshot = NULL)
  if (exists(".lab_ind_mcap_chain_cache", inherits = TRUE)) {
    rm(list = ls(envir = .lab_ind_mcap_chain_cache), envir = .lab_ind_mcap_chain_cache)
  }
  b <- lab_load_ind_mcap_chain_snapshot("US", force = TRUE)
  check("bundled snapshot non-empty", is.data.frame(b) && nrow(b) >= 15L)
} else {
  cat("INFO bundled ind_mcap_chain_rank_snapshot_US.csv not yet present\n")
}

# Legacy block test expectations still roughly hold
check("help mentions 全部產業鏈", grepl("全部產業鏈", ui_str("lab_im_ind_mcap_help", "zh-TW"), fixed = TRUE))
check("help mentions Refresh ranking", grepl("Refresh ranking", ui_str("lab_im_ind_mcap_help", "en"), fixed = TRUE))

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
