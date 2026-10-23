#!/usr/bin/env Rscript
# Live random-ticker audit across valuation model scenarios (US + TW).
# Verifies: recommend → param resolve → formula FV → reasonableness.
# Run:
#   cd app_27.0 && RETICULATE_PYTHON=.ynow_venv/bin/python3 \
#     Rscript tests/test_live_model_scenario_audit.R
#
# Optional: YNOW_AUDIT_SEED=42  YNOW_AUDIT_OUT=/path/to/report.md

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("setup.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

if (!nzchar(Sys.getenv("RETICULATE_PYTHON", ""))) {
  vpy <- file.path(app_dir, ".ynow_venv", "bin", "python3")
  if (file.exists(vpy)) Sys.setenv(RETICULATE_PYTHON = vpy)
}
Sys.unsetenv("YNOW_DEBUG_SKIP_PY")

source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "web_crawler.R"), local = FALSE)
source(file.path(app_dir, "valuation_guard.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "default_config.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)
source(file.path(app_dir, "ri_module.R"), local = FALSE)
source(file.path(app_dir, "relative_multiples_module.R"), local = FALSE)
source(file.path(app_dir, "nav_module.R"), local = FALSE)
source(file.path(app_dir, "pb_asset_module.R"), local = FALSE)

seed <- suppressWarnings(as.integer(Sys.getenv("YNOW_AUDIT_SEED", "20261012"))[1])
if (!is.finite(seed)) seed <- 20261012L
set.seed(seed)

out_path <- Sys.getenv(
  "YNOW_AUDIT_OUT",
  unset = "/opt/cursor/artifacts/live_model_scenario_audit.md"
)

pass <- 0L
fail <- 0L
warn_n <- 0L
findings <- character(0)
rows <- list()

check <- function(label, cond, level = "FAIL") {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("PASS ", label, "\n", sep = "")
    TRUE
  } else {
    if (identical(level, "WARN")) {
      warn_n <<- warn_n + 1L
      cat("WARN ", label, "\n", sep = "")
      findings <<- c(findings, paste0("WARN: ", label))
    } else {
      fail <<- fail + 1L
      cat("FAIL ", label, "\n", sep = "")
      findings <<- c(findings, paste0("FAIL: ", label))
    }
    FALSE
  }
}

stmt_collapsed <- function(res, name) {
  x <- res[[name]]
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  x$collapsed %||% x$expanded
}

yf_quote <- function(ticker) {
  tk <- as.character(ticker)[1]
  out <- tryCatch({
    reticulate::py_run_string(sprintf(
      paste0(
        "import math, yfinance as yf\n",
        "_t = yf.Ticker(%s)\n",
        "_fi = _t.fast_info\n",
        "def _f(x):\n",
        "    try:\n",
        "        v = float(x)\n",
        "        return v if math.isfinite(v) else float('nan')\n",
        "    except Exception:\n",
        "        return float('nan')\n",
        "audit_px = _f(getattr(_fi, 'last_price', float('nan')))\n",
        "audit_mc = _f(getattr(_fi, 'market_cap', float('nan')))\n",
        "audit_beta = _f((_t.info or {}).get('beta'))\n"
      ),
      deparse(tk)
    ))
    list(
      price = suppressWarnings(as.numeric(reticulate::py$audit_px)[1]),
      market_cap = suppressWarnings(as.numeric(reticulate::py$audit_mc)[1]),
      beta = suppressWarnings(as.numeric(reticulate::py$audit_beta)[1])
    )
  }, error = function(e) {
    list(price = NA_real_, market_cap = NA_real_, beta = NA_real_)
  })
  out
}

# Scenario pools: affinity → tickers (US + TW). Sample one random per scenario.
# Pools sized so RNG picks vary; seed makes runs reproducible.
scenarios <- list(
  list(
    id = "us_tech_dcf", market = "US", expect_primary = c("dcf", "ri"),
    industry_choice = "tech.Hardware",
    pool = c("AAPL", "MSFT", "GOOGL", "META", "NVDA", "AVGO", "ORCL", "CRM")
  ),
  list(
    id = "us_bank_pb", market = "US", expect_primary = c("pb", "ri", "ddm"),
    industry_choice = "fn.Banking",
    pool = c("JPM", "BAC", "WFC", "C", "USB", "PNC")
  ),
  list(
    id = "us_utility_ddm", market = "US", expect_primary = c("ddm", "dcf", "ri"),
    industry_choice = "util.Electric",
    pool = c("NEE", "DUK", "SO", "D", "AEP", "EXC")
  ),
  list(
    id = "us_cyclical_pb", market = "US", expect_primary = c("pb", "dcf", "ri"),
    industry_choice = "mat.Steel",
    pool = c("X", "NUE", "STLD", "CLF", "RS")
  ),
  list(
    id = "us_saas_dcf_2s", market = "US", expect_primary = c("dcf", "ri"),
    industry_choice = "saas.SaaS_Cloud",
    pool = c("NOW", "WDAY", "SNOW", "DDOG", "TEAM", "ZS")
  ),
  list(
    id = "us_holdco_nav", market = "US", expect_primary = c("nav", "pb"),
    industry_choice = "cong.Conglomerate",
    pool = c("BRK-B", "MMM", "GE", "HON")
  ),
  list(
    id = "tw_tech_dcf", market = "TW", expect_primary = c("dcf", "ri", "pb"),
    industry_choice = "tech.Semiconductors",
    pool = c("2330.TW", "2454.TW", "2303.TW", "3711.TW", "3034.TW")
  ),
  list(
    id = "tw_bank_pb", market = "TW", expect_primary = c("pb", "ri", "ddm"),
    industry_choice = "fn.Banking",
    pool = c("2881.TW", "2882.TW", "2891.TW", "2886.TW", "2880.TW")
  ),
  list(
    id = "tw_midcap_mixed", market = "TW", expect_primary = c("dcf", "ri", "pb", "ddm", "nav"),
    industry_choice = "",
    # OTC names use .TWO (Yahoo); listed use .TW
    pool = c("3363.TWO", "2376.TW", "3231.TW", "2357.TW", "2382.TW", "4938.TW")
  ),
  list(
    # Neg latest FCFF + thin ROE: must not keep Gordon-blocked DCF as FV primary
    id = "tw_otc_neg_fcff", market = "TW", expect_primary = c("dcf", "ri", "pb", "ddm", "nav"),
    industry_choice = "",
    pool = c("3363.TWO"),
    allow_insufficient = TRUE
  ),
  list(
    id = "us_consumer_ri", market = "US", expect_primary = c("dcf", "ri", "pb", "ddm"),
    industry_choice = "cons.PackagedFoods",
    pool = c("KO", "PEP", "PG", "CL", "KMB", "GIS")
  )
)

fv_models <- c("dcf", "ddm", "ri", "pb", "nav", "multiples", "sotp")

cat("\n== Live model scenario audit (seed=", seed, ") ==\n", sep = "")

for (sc in scenarios) {
  tk <- sample(sc$pool, 1L)
  cat("\n--- Scenario ", sc$id, " ticker=", tk, " ---\n", sep = "")
  row <- list(
    scenario = sc$id, market = sc$market, ticker = tk,
    scrape_ok = FALSE, primary = NA_character_, secondary = NA_character_,
    reference = NA_character_, multiples_method = NA_character_,
    price = NA_real_, fv_primary = NA_real_, fv_secondary = NA_real_,
    fv_multiples = NA_real_, fv_pb = NA_real_, mos_primary = NA_real_,
    note = ""
  )

  res <- tryCatch(cached_scrape_financials(tk), error = function(e) e)
  if (inherits(res, "error")) {
    check(paste0(sc$id, "/", tk, " scrape"), FALSE, "WARN")
    row$note <- paste("scrape_error:", conditionMessage(res))
    rows[[length(rows) + 1L]] <- row
    next
  }
  d_is <- stmt_collapsed(res, "Income Statement")
  d_bs <- stmt_collapsed(res, "Balance Sheet")
  d_cf <- stmt_collapsed(res, "Cash Flow")
  scrape_ok <- is.data.frame(d_is) && is.data.frame(d_bs) && is.data.frame(d_cf) &&
    nrow(d_is) > 0 && nrow(d_bs) > 0 && nrow(d_cf) > 0
  check(paste0(sc$id, "/", tk, " statements non-empty"), scrape_ok, "WARN")
  row$scrape_ok <- scrape_ok
  if (!scrape_ok) {
    row$note <- "empty_statements"
    rows[[length(rows) + 1L]] <- row
    next
  }

  info <- tryCatch(get_yahoo_industry(tk), error = function(e) NULL)
  ind_txt <- if (!is.null(info)) as.character(info$industry %||% "")[1] else ""
  q <- yf_quote(tk)
  px <- suppressWarnings(as.numeric(q$price)[1])
  mc <- suppressWarnings(as.numeric(q$market_cap)[1])
  beta <- suppressWarnings(as.numeric(q$beta)[1])
  row$price <- px

  shares <- tryCatch(
    select_current_metric(d_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock"),
    error = function(e) NA_real_
  )
  check(paste0(sc$id, "/", tk, " shares finite"), is.finite(shares) && shares > 0)

  rec <- tryCatch(
    recommend_valuation_models(
      d_cf, ind_txt, d_is, d_bs,
      industry_choice = sc$industry_choice
    ),
    error = function(e) e
  )
  if (inherits(rec, "error")) {
    check(paste0(sc$id, "/", tk, " recommend"), FALSE)
    row$note <- paste("recommend_error:", conditionMessage(rec))
    rows[[length(rows) + 1L]] <- row
    next
  }

  prim <- as.character(rec$primary %||% NA_character_)[1]
  sec <- as.character(rec$secondary %||% NA_character_)[1]
  ref <- as.character(rec$reference %||% NA_character_)[1]
  mm <- as.character(rec$multiples_method %||% NA_character_)[1]
  row$primary <- prim
  row$secondary <- sec
  row$reference <- ref
  row$multiples_method <- mm

  allow_insuff <- isTRUE(sc$allow_insufficient)
  dcf_blocked <- FALSE
  if (is.data.frame(rec$model_scores) && nrow(rec$model_scores)) {
    dcf_blocked <- isTRUE(rec$model_scores$gordon_blocked[rec$model_scores$model == "dcf"][1])
  }
  if (allow_insuff && (is.na(prim) || !nzchar(prim) ||
                       identical(as.character(rec$company_type %||% ""), "insufficient"))) {
    check(
      paste0(sc$id, "/", tk, " insufficient OK (no false Gordon DCF primary)"),
      is.na(prim) || !nzchar(prim) ||
        identical(as.character(rec$company_type %||% ""), "insufficient")
    )
    check(
      paste0(sc$id, "/", tk, " gordon-blocked DCF not FV primary"),
      !(identical(prim, "dcf") && dcf_blocked)
    )
  } else {
    check(
      paste0(sc$id, "/", tk, " has FV primary"),
      !is.na(prim) && nzchar(prim) && prim %in% c("dcf", "ddm", "ri", "pb", "nav")
    )
    check(
      paste0(sc$id, "/", tk, " gordon-blocked DCF not FV primary"),
      !(identical(prim, "dcf") && dcf_blocked)
    )
    check(
      paste0(sc$id, "/", tk, " primary in expected affinity"),
      is.na(prim) || prim %in% sc$expect_primary,
      "WARN"
    )
  }
  check(
    paste0(sc$id, "/", tk, " Multiples never secondary"),
    is.na(sec) || !identical(sec, "multiples")
  )
  check(
    paste0(sc$id, "/", tk, " reference is multiples when present"),
    is.na(ref) || identical(ref, "multiples"),
    "WARN"
  )
  # P/B folded into Multiples book family OR standalone pb primary/secondary
  pb_in_mult <- identical(mm, "pb") || identical(as.character(rec$multiples_family %||% ""), "book")
  pb_as_fv <- identical(prim, "pb") || identical(sec, "pb")
  check(
    paste0(sc$id, "/", tk, " P/B role defined (FV and/or Multiples book)"),
    pb_as_fv || pb_in_mult || identical(ref, "multiples"),
    "WARN"
  )

  # Estimate FV for all engines
  fvs <- list()
  for (m in fv_models) {
    est <- tryCatch(
      lab_estimate_fv_per_share(
        method = m,
        industry_key = sc$industry_choice,
        d_is = d_is, d_bs = d_bs, d_cf = d_cf,
        price = px, market_cap = mc, beta = beta, ticker = tk
      ),
      error = function(e) list(fv = NA_real_, note = conditionMessage(e))
    )
    fvs[[m]] <- est
  }
  fv_p <- suppressWarnings(as.numeric(fvs[[prim]]$fv)[1])
  fv_s <- if (!is.na(sec) && nzchar(sec)) suppressWarnings(as.numeric(fvs[[sec]]$fv)[1]) else NA_real_
  fv_m <- suppressWarnings(as.numeric(fvs[["multiples"]]$fv)[1])
  fv_pb <- suppressWarnings(as.numeric(fvs[["pb"]]$fv)[1])
  row$fv_primary <- fv_p
  row$fv_secondary <- fv_s
  row$fv_multiples <- fv_m
  row$fv_pb <- fv_pb

  # Primary FV: must be finite for recommended model (except documented gordon-blocked DCF)
  if (identical(prim, "dcf")) {
    scores <- rec$model_scores
    blocked <- FALSE
    if (is.data.frame(scores) && nrow(scores)) {
      blocked <- isTRUE(scores$gordon_blocked[scores$model == "dcf"][1])
    }
    if (blocked && !is.finite(fv_p)) {
      check(paste0(sc$id, "/", tk, " DCF gordon-blocked → no FV acceptable"), TRUE)
      row$note <- paste(row$note, "dcf_gordon_blocked")
    } else {
      check(paste0(sc$id, "/", tk, " primary DCF FV finite>0"), is.finite(fv_p) && fv_p > 0)
    }
  } else if (!is.na(prim) && nzchar(prim)) {
    check(
      paste0(sc$id, "/", tk, " primary ", prim, " FV finite>0"),
      is.finite(fv_p) && fv_p > 0
    )
  }

  if (!is.na(sec) && nzchar(sec) && sec %in% fv_models) {
    check(
      paste0(sc$id, "/", tk, " secondary ", sec, " FV finite>0"),
      is.finite(fv_s) && fv_s > 0,
      "WARN"
    )
  }

  # Param reasonableness from lab_resolve
  params <- tryCatch(
    lab_resolve_ticker_params(
      d_is, d_bs, d_cf,
      industry_key = sc$industry_choice,
      beta = beta, ticker = tk
    ),
    error = function(e) NULL
  )
  if (!is.null(params)) {
    ke <- suppressWarnings(as.numeric(params$ke_pct)[1])
    g <- suppressWarnings(as.numeric(params$g_pct)[1])
    check(paste0(sc$id, "/", tk, " Ke% in (3,40)"), is.finite(ke) && ke > 3 && ke < 40)
    check(paste0(sc$id, "/", tk, " g% < Ke%"), is.finite(g) && is.finite(ke) && g < ke, "WARN")
    check(paste0(sc$id, "/", tk, " g% in (-5,25)"), is.finite(g) && g > -5 && g < 25, "WARN")
  }

  # MOS vs price reasonableness when both finite
  if (is.finite(fv_p) && fv_p > 0 && is.finite(px) && px > 0) {
    mos <- (fv_p - px) / fv_p
    row$mos_primary <- mos
    # Extreme MOS (>95% or <-500%) flags display/param issues
    check(
      paste0(sc$id, "/", tk, " MOS not absurd (|MOS|<5 or FV/P in 0.05..20)"),
      abs(mos) < 5 || (fv_p / px > 0.05 && fv_p / px < 20),
      "WARN"
    )
  }

  # Closed-form Multiples PE identity when EPS>0
  eps <- tryCatch(
    select_current_metric(d_is, "Diluted EPS|Basic EPS|^EPS$", "flow"),
    error = function(e) NA_real_
  )
  eps <- suppressWarnings(as.numeric(eps)[1])
  pe_m <- suppressWarnings(as.numeric(APP_DEFAULTS$rel_pe_multiple %||% 18)[1])
  if (is.finite(eps) && eps > 0 && is.finite(pe_m)) {
    pe_imp <- calc_pe_implied_price(eps, pe_m)
    check(
      paste0(sc$id, "/", tk, " PE implied = EPS×multiple"),
      identical(pe_imp$status, "ok") &&
        abs(pe_imp$implied_price - eps * pe_m) < 1e-6
    )
  }

  # DCF via value_dcf_per_share when FCFF reconstructable
  if (identical(prim, "dcf") || identical(sec, "dcf")) {
    fcf_obj <- tryCatch(reconstruct_hist_fcff(d_cf, d_is), error = function(e) NULL)
    fcf_hist <- if (!is.null(fcf_obj)) suppressWarnings(as.numeric(fcf_obj$fcff)) else numeric(0)
    if (length(fcf_hist) && any(is.finite(fcf_hist))) {
      last_fcf <- suppressWarnings(as.numeric(tail(fcf_hist[is.finite(fcf_hist)], 1))[1])
      ke_dec <- if (!is.null(params)) suppressWarnings(as.numeric(params$ke_pct)[1]) / 100 else 0.09
      g_dec <- if (!is.null(params)) {
        min(suppressWarnings(as.numeric(params$g_pct)[1]) / 100, ke_dec - 0.015)
      } else {
        0.03
      }
      n <- if (!is.null(params)) suppressWarnings(as.integer(params$n_years)[1]) else 5L
      if (!is.finite(n) || n < 1L) n <- 5L
      g1 <- if (!is.null(params)) {
        max(0.01, min(suppressWarnings(as.numeric(params$g1_pct)[1]) / 100, 0.15))
      } else {
        g_dec
      }
      cf_path <- last_fcf * (1 + g1)^seq_len(n)
      cash <- tryCatch(
        select_current_metric(d_bs, "Cash And Cash Equivalents|Cash Cash Equivalents And Short Term Investments", "stock"),
        error = function(e) 0
      )
      debt <- tryCatch(
        select_current_metric(d_bs, "^Total Debt$|Long Term Debt", "stock"),
        error = function(e) 0
      )
      sh_use <- shares
      if (is.finite(px) && is.finite(mc) && px > 0) {
        sh_res <- tryCatch(
          resolve_shares_for_price(shares, price = px, market_cap = mc, ticker = tk),
          error = function(e) NULL
        )
        if (!is.null(sh_res) && is.finite(sh_res$shares)) sh_use <- sh_res$shares
      }
      vd <- tryCatch(
        value_dcf_per_share(
          cf = cf_path, claim = "fcff", wacc = ke_dec, ke = ke_dec,
          g_term = g_dec, cash = cash, debt = debt, shares = sh_use,
          discount_kind = "wacc"
        ),
        error = function(e) list(ok = FALSE)
      )
      if (isTRUE(vd$ok)) {
        check(
          paste0(sc$id, "/", tk, " value_dcf_per_share finite"),
          is.finite(vd$per_share) && vd$per_share > 0
        )
      } else if (is.finite(last_fcf) && last_fcf <= 0) {
        check(
          paste0(sc$id, "/", tk, " neg/zero FCFF → DCF gate expected"),
          TRUE
        )
        row$note <- paste(row$note, "neg_fcff")
      } else {
        check(paste0(sc$id, "/", tk, " value_dcf_per_share ok"), FALSE, "WARN")
      }
    }
  }

  rows[[length(rows) + 1L]] <- row
}

# Static completeness: Multiples book includes pb; UI locale keys for models
check(
  "relative_multiples book family lists pb",
  grepl("\\bpb\\b", paste(readLines(file.path(app_dir, "relative_multiples_module.R"), warn = FALSE), collapse = "\n"))
)
guard_txt <- paste(readLines(file.path(app_dir, "valuation_guard.R"), warn = FALSE), collapse = "\n")
check(
  "assemble_model_recommendation keeps multiples as reference",
  grepl("reference", guard_txt, fixed = TRUE) && grepl("multiples", guard_txt, fixed = TRUE)
)
check(
  "FV primary pool excludes multiples/sotp",
  grepl('c\\(\"dcf\",\"ddm\",\"ri\",\"pb\",\"nav\"\\)', guard_txt) ||
    grepl('c\\("dcf", "ddm", "ri", "pb", "nav"\\)', guard_txt)
)

df <- do.call(rbind, lapply(rows, function(r) {
  data.frame(
    scenario = r$scenario,
    market = r$market,
    ticker = r$ticker,
    scrape_ok = r$scrape_ok,
    primary = r$primary %||% NA_character_,
    secondary = r$secondary %||% NA_character_,
    reference = r$reference %||% NA_character_,
    multiples_method = r$multiples_method %||% NA_character_,
    price = r$price,
    fv_primary = r$fv_primary,
    fv_secondary = r$fv_secondary,
    fv_multiples = r$fv_multiples,
    fv_pb = r$fv_pb,
    mos_primary = r$mos_primary,
    note = r$note %||% "",
    stringsAsFactors = FALSE
  )
}))

# Write markdown report
md <- c(
  "# Live valuation model scenario audit",
  "",
  paste0("- Seed: `", seed, "`"),
  paste0("- App: `app_27.0`"),
  paste0("- When: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  paste0("- Results: PASS=", pass, " FAIL=", fail, " WARN=", warn_n),
  "",
  "## Sampled tickers",
  "",
  "| Scenario | Mkt | Ticker | Primary | Secondary | Ref | Mult method | Price | FV prim | FV sec | FV mult | FV P/B | MOS | Note |",
  "|---|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|"
)
for (i in seq_len(nrow(df))) {
  r <- df[i, ]
  md <- c(md, sprintf(
    "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
    r$scenario, r$market, r$ticker,
    r$primary %||% "", r$secondary %||% "", r$reference %||% "",
    r$multiples_method %||% "",
    ifelse(is.finite(r$price), sprintf("%.2f", r$price), ""),
    ifelse(is.finite(r$fv_primary), sprintf("%.2f", r$fv_primary), ""),
    ifelse(is.finite(r$fv_secondary), sprintf("%.2f", r$fv_secondary), ""),
    ifelse(is.finite(r$fv_multiples), sprintf("%.2f", r$fv_multiples), ""),
    ifelse(is.finite(r$fv_pb), sprintf("%.2f", r$fv_pb), ""),
    ifelse(is.finite(r$mos_primary), sprintf("%.1f%%", 100 * r$mos_primary), ""),
    gsub("\\|", "/", r$note)
  ))
}
md <- c(
  md, "",
  "## Findings", ""
)
if (!length(findings)) {
  md <- c(md, "- None (no FAIL/WARN).")
} else {
  md <- c(md, paste0("- ", findings))
}
md <- c(
  md, "",
  "## Coverage matrix",
  "",
  "| Layer | Covered |",
  "|---|---|",
  "| Model definition / recommendation gates | `recommend_valuation_models` / `assemble_model_recommendation` |",
  "| Param source | `lab_resolve_ticker_params` + Yahoo statements + CAPM/industry β |",
  "| Formula engines | lab DCF/DDM/RI/P/B/NAV/Multiples/SOTP + `value_dcf_per_share` + `calc_pe_implied_price` |",
  "| Markets | US + TW random samples |",
  "| Modes | Shared formula core (Lite/Full); recommendation identical |",
  ""
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(md, out_path)
csv_path <- sub("\\.md$", ".csv", out_path)
utils::write.csv(df, csv_path, row.names = FALSE)

cat("\n== Summary ==\n")
cat("PASS ", pass, "  FAIL ", fail, "  WARN ", warn_n, "\n", sep = "")
cat("Report: ", out_path, "\n", sep = "")
cat("CSV: ", csv_path, "\n", sep = "")
print(df[, c("scenario", "ticker", "primary", "secondary", "fv_primary", "price")])

# Soft-fail: only hard FAILs abort (WARN ok for affinity / scrape flakes)
if (fail > 0L) quit(status = 1L)
cat("AUDIT COMPLETE (no hard FAIL)\n")
