# Lock chrome on first HTML paint (no FOUC waiting for ynowAuthFeature).
# Run: cd app_27.0 && Rscript tests/test_lock_first_paint.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- if (file.exists("ynow_ui.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate ynow_ui.R")
}

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK  ", label)
  } else {
    bad <<- bad + 1L
    message("FAIL ", label)
  }
}

src_auth <- paste(
  readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
src_ui <- paste(
  readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

check("stamp helper defined in auth_account.R", {
  grepl("ynow_stamp_auth_feature_body", src_auth, fixed = TRUE) &&
    grepl("ynow-auth-feature", src_auth, fixed = TRUE)
})

check("ynow_ui.R applies stamp after dashboardPage", {
  grepl("ynow_stamp_auth_feature_body(ui)", src_ui, fixed = TRUE)
})

source(file.path(app_dir, "auth_account.R"), local = TRUE)

# Minimal shinydashboard body (same shape as dashboardPage return).
fake_body <- htmltools::tags$body(
  class = "skin-black sidebar-collapse",
  style = "min-height: 611px;",
  htmltools::div(class = "wrapper", "x")
)

old_env <- Sys.getenv("YNOW_ENABLE_ACCOUNT_LOGIN", unset = NA_character_)
on.exit({
  if (is.na(old_env)) {
    Sys.unsetenv("YNOW_ENABLE_ACCOUNT_LOGIN")
  } else {
    Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = old_env)
  }
}, add = TRUE)

Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
stamped <- ynow_stamp_auth_feature_body(fake_body)
check("auth on: body gains ynow-auth-feature", {
  grepl("(^|\\s)ynow-auth-feature(\\s|$)", stamped$attribs$class)
})
check("auth on: keeps skin-black", {
  grepl("(^|\\s)skin-black(\\s|$)", stamped$attribs$class)
})
check("auth on: stamp is idempotent", {
  again <- ynow_stamp_auth_feature_body(stamped)
  identical(again$attribs$class, stamped$attribs$class) &&
    length(gregexpr("ynow-auth-feature", again$attribs$class, fixed = TRUE)[[1]]) == 1L
})

Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
unstamped <- ynow_stamp_auth_feature_body(fake_body)
check("auth off: body not stamped", {
  !grepl("ynow-auth-feature", unstamped$attribs$class, fixed = TRUE)
})

# Non-body tags are left alone.
other <- htmltools::div(class = "skin-black")
Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
check("non-body tag unchanged", {
  identical(ynow_stamp_auth_feature_body(other), other)
})

# Guest lock CSS still requires the body class (fail-closed once stamped).
check("guest lock CSS still keyed on ynow-auth-feature", {
  grepl(
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a > .ynow-menu-lock",
    src_ui,
    fixed = TRUE
  ) && (
    grepl(
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-member-gate",
      src_auth,
      fixed = TRUE
    ) ||
      grepl(
        "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-member-body",
        src_auth,
        fixed = TRUE
      )
  )
})

check("post-login still-locked chrome kept (ynow-sidebar-locked)", {
  grepl("ynow-sidebar-locked", src_ui, fixed = TRUE) &&
    grepl("ynowSyncGuestSidebarLocked", src_ui, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
