# Signed-in recent search history: rolling 24h normalize + file/memory persist
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "user_prefs.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

Sys.setenv(YNOW_USER_PREFS_FORCE_FILE = "1")
tmpdir <- tempfile("ynow_prefs_recent_")
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
old_wd <- getwd()
on.exit({
  setwd(old_wd)
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  Sys.unsetenv("YNOW_USER_PREFS_FORCE_FILE")
}, add = TRUE)
setwd(tmpdir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("SKIP: jsonlite not installed\n")
  quit(status = 0L)
}

source(file.path(root, "user_prefs.R"), local = FALSE, encoding = "UTF-8")
ynow_prefs_reset_memory()

now <- as.POSIXct(Sys.time(), tz = "UTC")
old <- now - (25 * 3600)
mid <- now - (2 * 3600)

ents <- ynow_prefs_normalize_recent_entries(
  list(
    list(ticker = "aapl", at = format(mid, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    list(ticker = "MSFT", at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    list(ticker = "OLD", at = format(old, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    list(ticker = "aapl", at = format(now - 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  ),
  now = now
)
tickers <- ynow_prefs_recent_entries_tickers(ents)
check("drops older than 24h", !("OLD" %in% tickers))
check("newest first", identical(tickers[1], "MSFT"))
check("dedupes keeping newest AAPL", identical(tickers, c("MSFT", "AAPL")))

uid <- "test-user-recent-24h"
check("empty get", identical(ynow_prefs_get_recent_search_24h(uid), character(0)))

# Use past offsets so a later merge (Sys.time()) always wins over server stamps.
t0 <- as.POSIXct(Sys.time(), tz = "UTC") - 60
a <- ynow_prefs_record_recent_search(uid, "AAPL", at = t0)
check("record AAPL", identical(a, "AAPL"))

b <- ynow_prefs_record_recent_search(uid, "msft", at = t0 + 10)
check("record MSFT front", identical(b, c("MSFT", "AAPL")))

c <- ynow_prefs_record_recent_search(uid, "AAPL", at = t0 + 20)
check("re-search AAPL bumps front", identical(c, c("AAPL", "MSFT")))

# Survive logout: clear memory, reload from file.
ynow_prefs_reset_memory()
check(
  "file reload after memory clear",
  identical(ynow_prefs_get_recent_search_24h(uid), c("AAPL", "MSFT"))
)

# Merge guest session tickers on login (guest stamped at Sys.time() > server t0+20).
merged <- ynow_prefs_merge_recent_search_24h(uid, c("TSLA", "aapl"))
check("merge guest TSLA front", identical(merged[1], "TSLA"))
check("merge guest reorders AAPL", identical(merged[1:2], c("TSLA", "AAPL")))
check("merge keeps MSFT", identical(merged, c("TSLA", "AAPL", "MSFT")))

# Stale entries pruned on get (plant a 30h-old sibling in the file).
stale_uid <- "test-user-stale"
invisible(ynow_prefs_set_recent_search_24h(stale_uid, "KEEP"))
store <- jsonlite::fromJSON(
  file.path(tmpdir, "ynow_data", "user_prefs.json"),
  simplifyVector = FALSE
)
store[[stale_uid]][[YNOW_PREF_RECENT_SEARCH_24H]] <- list(
  list(ticker = "KEEP", at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  list(
    ticker = "GONE",
    at = format(Sys.time() - (30 * 3600), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
)
jsonlite::write_json(store, file.path(tmpdir, "ynow_data", "user_prefs.json"),
                     auto_unbox = TRUE, pretty = TRUE)
ynow_prefs_reset_memory()
got_stale <- ynow_prefs_get_recent_search_24h(stale_uid)
check("get prunes stale GONE", identical(got_stale, "KEEP"))

# Cap
many <- paste0("T", seq_len(30L))
invisible(ynow_prefs_set_recent_search_24h(uid, many))
got_cap <- ynow_prefs_get_recent_search_24h(uid)
check("max 20 history", length(got_cap) == 20L)

# Pref key must not collide with stock-chip pins or page/sidebar pin keys.
# Sibling owns Recent-search stock pin grid (4×3); we only own timed history.
check(
  "distinct pref key vs pinned recent",
  !identical(YNOW_PREF_RECENT_SEARCH_24H, YNOW_PREF_PINNED_RECENT) &&
    identical(YNOW_PREF_RECENT_SEARCH_24H, "recent_search_history_24h")
)
check(
  "distinct pref key vs sidebar tabs",
  exists("YNOW_PREF_PINNED_SIDEBAR") &&
    !identical(YNOW_PREF_RECENT_SEARCH_24H, YNOW_PREF_PINNED_SIDEBAR)
)
# Coexistence: writing 24h history must not wipe pinned recent tickers.
uid_co <- "test-user-coexist"
invisible(ynow_prefs_set_pinned_recent(uid_co, c("NVDA", "META")))
invisible(ynow_prefs_record_recent_search(uid_co, "AAPL", at = Sys.time() - 30))
ynow_prefs_reset_memory()
check(
  "24h write keeps pinned recent",
  identical(ynow_prefs_get_pinned_recent(uid_co), c("NVDA", "META"))
)
check(
  "pinned write does not block 24h read",
  identical(ynow_prefs_get_recent_search_24h(uid_co), "AAPL")
)

# Wiring smoke
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("server loads 24h on auth", grepl("ynow_prefs_get_recent_search_24h", srv, fixed = TRUE))
check("server records on search", grepl("ynow_prefs_record_recent_search", srv, fixed = TRUE))
check("server merges guest on login", grepl("ynow_prefs_merge_recent_search_24h", srv, fixed = TRUE))
check("JS label handler", grepl("ynowRecentSearchLabel", ui, fixed = TRUE))
check("en 24h hint", grepl("recent_search_24h_hint", loc, fixed = TRUE))
check("zh 24h hint", grepl("登入後保留近 24 小時的搜尋紀錄", loc, fixed = TRUE))
check("zh 24h empty", grepl("近 24 小時尚無搜尋紀錄", loc, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
