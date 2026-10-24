# GateMode registry: guest_auth | spend_fyn | topup_promo.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_gate_mode_registry.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
Sys.setenv(YNOW_ACCESS_LOCKS_FORCE_FILE = "1")

ok <- 0L
bad <- 0L
check <- function(name, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK ", name)
  } else {
    bad <<- bad + 1L
    message("FAIL ", name)
  }
}

app_dir <- normalizePath("..", mustWork = TRUE)
if (!grepl("app_27.0$", app_dir)) {
  app_dir <- normalizePath(".", mustWork = TRUE)
}
source(file.path(app_dir, "access_locks.R"), local = FALSE)

lock_entry <- function(unlock_type = NA_character_, points_cost = NA_integer_, effect = "lock") {
  list(
    effect = effect,
    unlock_type = unlock_type,
    points_cost = points_cost
  )
}

check("registry has the three modes", {
  reg <- ynow_gate_mode_registry()
  identical(names(reg), c("guest_auth", "spend_fyn", "topup_promo")) &&
    identical(reg$guest_auth$action, "auth") &&
    identical(reg$spend_fyn$action, "spend") &&
    identical(reg$spend_fyn$chrome, "spend_fyn") &&
    identical(reg$spend_fyn$secondary_key, "gate_mode_topup_cta") &&
    identical(reg$topup_promo$action, "topup") &&
    is.null(reg$topup_promo$secondary_key)
})

check("payload is slot names, not prose", {
  payload <- ynow_gate_mode_registry_payload()
  blob <- paste(unlist(payload), collapse = " ")
  identical(payload$spend_fyn$cta_key, "gate_mode_spend_cta") &&
    identical(payload$spend_fyn$body_key, "gate_mode_spend_body") &&
    !grepl("Unlock with FYN", blob, fixed = TRUE) &&
    !grepl("花費", blob, fixed = TRUE)
})

guest_pts <- ynow_gate_mode_resolve(
  lock_entry("points", 5L), authed = FALSE, fyn_balance = 12L
)
check("guest + points cost stays guest_auth", {
  identical(guest_pts$gate_mode, "guest_auth") &&
    identical(guest_pts$reason, "guest") &&
    identical(guest_pts$points_cost, 5L) &&
    is.null(guest_pts$fyn_balance) &&
    is.null(guest_pts$afford)
})

spend <- ynow_gate_mode_resolve(
  lock_entry("points", 5L), authed = TRUE, fyn_balance = 12L
)
check("authed + points cost is spend_fyn and affordable", {
  identical(spend$gate_mode, "spend_fyn") &&
    identical(spend$reason, "points_cost") &&
    identical(spend$points_cost, 5L) &&
    identical(spend$fyn_balance, 12L) &&
    isTRUE(spend$afford)
})

short <- ynow_gate_mode_resolve(
  lock_entry("points", 5L), authed = TRUE, fyn_balance = 3L
)
check("shortfall stays spend_fyn with afford false", {
  identical(short$gate_mode, "spend_fyn") &&
    identical(short$reason, "points_cost") &&
    identical(short$afford, FALSE)
})

sticky <- ynow_gate_mode_resolve(
  lock_entry(NA_character_, NA_integer_), authed = TRUE, fyn_balance = 9L
)
check("authed sticky unlock is topup_promo", {
  identical(sticky$gate_mode, "topup_promo") &&
    identical(sticky$reason, "sticky") &&
    is.null(sticky$points_cost) &&
    is.null(sticky$afford) &&
    identical(sticky$fyn_balance, 9L)
})

cond <- ynow_gate_mode_resolve(
  lock_entry("conditional", NA_integer_), authed = TRUE, fyn_balance = 1L
)
check("authed conditional residual is topup_promo", {
  identical(cond$gate_mode, "topup_promo") &&
    identical(cond$reason, "conditional")
})

other <- ynow_gate_mode_resolve(
  lock_entry("login", NA_integer_), authed = TRUE, fyn_balance = 1L
)
check("authed login residual is topup_promo other", {
  identical(other$gate_mode, "topup_promo") &&
    identical(other$reason, "other")
})

zero <- ynow_gate_mode_resolve(
  lock_entry("points", 0L), authed = TRUE, fyn_balance = 20L
)
check("points cost below 1 is topup_promo", {
  identical(zero$gate_mode, "topup_promo") &&
    !identical(zero$gate_mode, "spend_fyn")
})

hide <- ynow_gate_mode_resolve(
  lock_entry("points", 5L, effect = "hide"), authed = FALSE, fyn_balance = 4L
)
check("hide has no gate mode", {
  is.null(hide$gate_mode) && is.null(hide$reason) && is.null(hide$afford)
})

modes <- vapply(list(
  lock_entry("points", 8L),
  lock_entry(NA_character_),
  lock_entry("conditional"),
  lock_entry("login"),
  lock_entry("points", 0L),
  list(effect = "hide", unlock_type = "points", points_cost = 4L)
), function(entry) {
  hit <- ynow_gate_mode_resolve(entry, authed = TRUE, fyn_balance = 100L)
  as.character(hit$gate_mode %||% "")
}, character(1))
check("authed never resolves guest_auth", {
  !any(modes == "guest_auth")
})

row <- ynow_gate_mode_client_row(
  "tab:decision_checklist", "lock",
  lock_entry("points", 5L),
  authed = TRUE, fyn_balance = 12L
)
check("client row keeps unlock_type beside gate_mode", {
  identical(row$id, "tab:decision_checklist") &&
    identical(row$effect, "lock") &&
    identical(row$unlock_type, "points") &&
    identical(row$gate_mode, "spend_fyn") &&
    identical(row$points_cost, 5L) &&
    identical(row$reason, "points_cost") &&
    isTRUE(row$afford)
})

hide_row <- ynow_gate_mode_client_row(
  "tab:lab_notes", "hide",
  lock_entry("points", 5L),
  authed = TRUE, fyn_balance = 12L
)
check("hide client row nulls the mode", {
  identical(hide_row$effect, "hide") &&
    is.null(hide_row$gate_mode)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
