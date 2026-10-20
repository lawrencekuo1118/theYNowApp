# test_mos_vs_fair_value.R — near-zero FV must not explode MOS
# Run: Rscript app_25.0/tests/test_mos_vs_fair_value.R

.app_dir <- tryCatch(
  normalizePath(file.path(dirname(sys.frame(1)$ofile), "..")),
  error = function(e) normalizePath("..")
)
if (!file.exists(file.path(.app_dir, "setup.R"))) {
  .app_dir <- normalizePath("app_25.0")
}
setwd(.app_dir)
source("setup.R", local = TRUE)

check <- function(lab, cond) {
  cat(if (isTRUE(cond)) "OK  " else "FAIL", lab, "\n")
  if (!isTRUE(cond)) quit(status = 1L)
}

m <- mos_vs_fair_value(100, 80)
check("normal MOS 20%", is.finite(m) && abs(m - 0.2) < 1e-12)
check("exact zero FV → NA", is.na(mos_vs_fair_value(0, 602)))
# 3363-like RI clean-surplus residue
check("|FV|=1e-15 → NA", is.na(mos_vs_fair_value(-1.7764e-15, 602)))
check("|FV|=8e-15 → NA", is.na(mos_vs_fair_value(8.8818e-15, 602)))
check("|FV|=0.02 kept", is.finite(mos_vs_fair_value(0.02, 602)))
check("negative FV |fv|>=0.01 kept", is.finite(mos_vs_fair_value(-2.12, 602)))

cat("ALL PASS\n")
