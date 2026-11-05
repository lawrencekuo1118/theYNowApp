# test_cold_start_boot.R — defer Postgres schema + capacity keep-warm for first-load
# reconnect flashes (see Project docs/first-load-disconnect.md).
suppressPackageStartupMessages({
  root <- if (file.exists("global.R")) {
    normalizePath(".")
  } else if (file.exists("../global.R")) {
    normalizePath("..")
  } else {
    normalizePath(file.path("..", "app_27.0"))
  }
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

glb <- paste(readLines(file.path(root, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app <- paste(readLines(file.path(root, "app.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "global defines deferred postgres boot helper",
  grepl(".ynow_boot_postgres_schema", glb, fixed = TRUE) &&
    grepl(".ynow_arm_postgres_boot", glb, fixed = TRUE) &&
    grepl(".ynow_postgres_boot_armed", glb, fixed = TRUE)
)
check(
  "global does not call ensure_schema synchronously at source",
  {
    # The old sync block called ynow_db_ensure_schema() at top level inside
    # `if (ynow_db_ready())`. New code only defines helpers; arming is in onStart.
    !grepl(
      "if \\(exists\\(\"ynow_db_ready\".*\\n.*ynow_db_ensure_schema\\(\\)",
      glb
    )
  }
)
check(
  "arm schedules via later::later(delay = 0)",
  grepl("later::later(run, delay = 0)", glb, fixed = TRUE)
)
check(
  "app.R onStart arms postgres boot after Listen",
  grepl("onStart", app, fixed = TRUE) &&
    grepl(".ynow_arm_postgres_boot", app, fixed = TRUE) &&
    grepl(".ynow_arm_python_install", app, fixed = TRUE)
)

ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "UI cloaks shinyapps reconnect dialog during first connect",
  grepl("ynow-boot-connecting", ui, fixed = TRUE) &&
    grepl("#ss-connect-dialog", ui, fixed = TRUE) &&
    grepl("shiny:connected", ui, fixed = TRUE) &&
    grepl("setTimeout(clearBootCloak, 20000)", ui, fixed = TRUE)
)

# Capacity keep-warm (Option A)
cap_root <- if (file.exists(file.path(root, "..", "scripts", "ynow_capacity.R"))) {
  normalizePath(file.path(root, ".."))
} else {
  normalizePath(file.path(root, "../.."))
}
cap_src <- paste(
  readLines(file.path(cap_root, "scripts", "ynow_capacity.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
cfg_src <- paste(
  readLines(file.path(cap_root, "scripts", "configure_shinyapps_capacity.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check(
  "capacity settings include timeout_instance_idle",
  grepl("timeout_instance_idle", cap_src, fixed = TRUE) &&
    grepl("timeout_instance_idle = 3600L", cap_src, fixed = TRUE)
)
check(
  "configure tries Instance Idle Timeout API keys",
  grepl("application.idle.timeout", cfg_src, fixed = TRUE) &&
    grepl("application.instances.idle.timeout", cfg_src, fixed = TRUE) &&
    grepl("Instance Idle Timeout", cfg_src, fixed = TRUE)
)
check(
  "configure keeps min.processes=0 (no always-on worker)",
  grepl('set_prop("application.shiny.scheduler.min.processes", 0L)', cfg_src, fixed = TRUE)
)

# Planner still returns the new field on Starter stable path
suppressMessages(source(file.path(cap_root, "scripts", "ynow_capacity.R"), local = TRUE))
starter_ents <- list(
  MaxInstancesLimit = list(limit = 1L),
  MaxApplicationWorkersLimit = list(limit = 1L),
  ApplicationInstanceType = list(items = list("small", "medium", "large"))
)
stable <- ynow_capacity_stable_for_plan(starter_ents)
check(
  "Starter stable recommends 3600s instance idle",
  identical(as.integer(stable$settings$timeout_instance_idle), 3600L)
)

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1)
