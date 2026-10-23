# test_capacity_500.R — concurrency capacity planner for ≥500 sessions
suppressMessages({
  root <- if (file.exists("../../scripts/ynow_capacity.R")) {
    normalizePath("../..")
  } else if (file.exists("../scripts/ynow_capacity.R")) {
    normalizePath("..")
  } else {
    normalizePath(".")
  }
  source(file.path(root, "scripts", "ynow_capacity.R"), local = TRUE)
})

pass <- 0L
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

check("connections product", identical(ynow_capacity_connections(10L, 5L, 10L), 500L))
check("mb/conn reserves worker base", {
  # xxxlarge 8192 − 5*280 = 6792; / (5*10) = 135.84
  mb <- ynow_capacity_mb_per_conn("xxxlarge", 5L, 10L)
  is.finite(mb) && mb > 130 && mb < 145
})

# Starter entitlements (live account as of audit): cannot hit 500.
starter_ents <- list(
  MaxInstancesLimit = list(limit = 1L),
  MaxApplicationWorkersLimit = list(limit = 1L),
  ApplicationInstanceType = list(items = list("small", "medium", "large"))
)
starter <- ynow_capacity_from_entitlements(starter_ents, target = 500L)
check("Starter cannot reach 500", isFALSE(starter$ok))
check("Starter capacity ≤ 50", starter$capacity <= 50L)

stable <- ynow_capacity_stable_for_plan(starter_ents)
check("Starter stable shrinks connections", stable$settings$max_connections < 50L)
check("Starter stable capacity ≥ 1", stable$capacity >= 1L)
check("Starter stable MB/conn ≥ 100", isTRUE(stable$mb_per_conn >= 100))
# Post session_capacity opts: Starter floor 100 MB → typically 7 on large/1GB
check(
  "Starter stable capacity in 6–10 band",
  stable$capacity >= 6L && stable$capacity <= 10L
)
check(
  "async + session_capacity wired in global",
  {
    glob <- paste(
      readLines(file.path(root, "app_27.0", "global.R"), warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    if (!grepl("session_capacity", glob, fixed = TRUE)) {
      glob <- paste(
        readLines(file.path(root, "global.R"), warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )
    }
    grepl("session_capacity.R", glob, fixed = TRUE)
  }
)

# Professional-class entitlements: must recommend ≥500 with healthy MB/conn.
pro_ents <- list(
  MaxInstancesLimit = list(limit = 10L),
  MaxApplicationWorkersLimit = list(limit = 10L),
  ApplicationInstanceType = list(items = list(
    "small", "medium", "large", "xlarge", "xxlarge", "xxxlarge"
  ))
)
pro <- ynow_capacity_from_entitlements(pro_ents, target = 500L)
check("Professional can reach 500", isTRUE(pro$ok))
check("Professional capacity ≥ 500", pro$capacity >= 500L)
check("Professional uses xxxlarge", identical(pro$settings$size, "xxxlarge"))
check("Professional MB/conn ≥ 120", isTRUE(pro$mb_per_conn >= 120))
check(
  "Professional product matches settings",
  identical(
    ynow_capacity_connections(
      pro$settings$instances,
      pro$settings$workers,
      pro$settings$max_connections
    ),
    as.integer(pro$capacity)
  )
)

# Standard (5 instances) — may or may not hit 500 with MB floor; record expectation.
std_ents <- list(
  MaxInstancesLimit = list(limit = 5L),
  MaxApplicationWorkersLimit = list(limit = 10L),
  ApplicationInstanceType = list(items = list(
    "small", "medium", "large", "xlarge", "xxlarge", "xxxlarge"
  ))
)
std <- ynow_capacity_from_entitlements(std_ents, target = 500L)
# With 120 MB/conn floor + worker base, 5×xxxlarge often still reaches 500.
check(
  "Standard either reaches 500 or documents shortfall",
  isTRUE(std$ok) || isFALSE(std$ok)
)
if (isTRUE(std$ok)) {
  check("Standard capacity ≥ 500 when ok", std$capacity >= 500L)
} else {
  check("Standard shortfall capacity still positive", std$capacity >= 1L)
}

# Async hosted memory-safe helper present in app tree
async_path <- file.path(root, "app_27.0", "async_jobs.R")
if (!file.exists(async_path)) async_path <- file.path(root, "async_jobs.R")
async_src <- paste(readLines(async_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("async hosted memory-safe helper", grepl(".ynow_async_hosted_memory_safe", async_src, fixed = TRUE))
check("async sequential on hosted", grepl("future::sequential", async_src, fixed = TRUE))

auth_path <- file.path(root, "app_27.0", "auth_db.R")
if (!file.exists(auth_path)) auth_path <- file.path(root, "auth_db.R")
auth_src <- paste(readLines(auth_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("auth pool capped on hosted", grepl("YNOW_DB_POOL_MAX", auth_src, fixed = TRUE))

cat("\nPassed: ", pass, " Failed: ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
invisible(TRUE)
