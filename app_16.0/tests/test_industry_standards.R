# Targeted tests: industry catalog + TW/Yahoo mapping fallbacks
root <- getwd()
app_dir <- if (basename(root) == "tests") {
  dirname(root)
} else if (dir.exists(file.path(root, "app_16.0"))) {
  file.path(root, "app_16.0")
} else if (file.exists(file.path(root, "industry_standards.R"))) {
  root
} else {
  stop("Cannot locate app_16.0")
}
`%||%` <- function(a, b) if (is.null(a)) b else a

source(file.path(app_dir, "industry_standards.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "lab_sp500_universe.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "lab_tw_universe.R"), local = FALSE, encoding = "UTF-8")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(paste0("FAIL: ", label), call. = FALSE)
  cat("OK:", label, "\n")
}

check("catalog size >= 55", length(industry_standards) >= 55L)
check("labels match standards", identical(sort(names(industry_labels)), sort(names(industry_standards))))
check("en labels match standards", identical(sort(names(industry_labels_en)), sort(names(industry_standards))))

snap_mets <- c("rev_growth", "roe", "roa", "opex_ratio", "eqt_multiplier")
for (m in snap_mets) {
  n <- sum(vapply(industry_standards, function(x) !is.null(x[[m]]), logical(1)))
  check(paste0("metric ", m, " covered"), n == length(industry_standards))
}

check("unknown key color none", identical(get_box_color("no.such.industry", "roe", 10), "none"))
check("unknown key band dash", identical(annotation_format_band("no.such.industry", "roe"), "—"))
check("empty yahoo resolve", identical(resolve_industry_key_from_yahoo(""), ""))
check("yahoo semiconductors", identical(
  map_yahoo_industry_to_key("Technology", "Semiconductors"),
  "sc.IC_Design"
))
check("yahoo restaurants", identical(
  map_yahoo_industry_to_key("Consumer Cyclical", "Restaurants"),
  "cons.Restaurants"
))

check("TW code 24 semiconductor", identical(lab_map_tw_industry_to_key("24"), "sc.Foundry"))
check("TW code 36 cloud", identical(lab_map_tw_industry_to_key("36"), "saas.SaaS_Cloud"))
check("TW 綠能環保", identical(lab_map_tw_industry_to_key("綠能環保"), "en.Environmental"))
check("TW 紡織纖維", identical(lab_map_tw_industry_to_key("紡織纖維"), "mat.Textiles"))
check("TW unknown unmapped", identical(lab_map_tw_industry_to_key("91"), LAB_UNMAPPED_KEY))

# GICS map targets exist in catalog
smap <- lab_gics_subindustry_map()
missing_keys <- setdiff(unique(unname(smap)), names(industry_standards))
check(paste0("GICS targets in catalog (", length(missing_keys), ")"), length(missing_keys) == 0L)

# Remap cache keys if CSV present
csv <- file.path(app_dir, "data", "tw_universe.csv")
if (file.exists(csv)) {
  u <- lab_read_tw_cache()
  n_um <- sum(u$industry_key == LAB_UNMAPPED_KEY, na.rm = TRUE)
  check("TW cache remap reduces unmapped", n_um < 400L)
  cat("TW unmapped after remap:", n_um, "/", nrow(u), "\n")
}

cat("ALL PASS\n")
