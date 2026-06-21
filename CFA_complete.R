library(lavaan)
library(haven)
library(dplyr)

# ============================================================
# LOAD DATA
# ============================================================
df_cfa <- read_sav("DatasetRMS_studenten7.sav") %>%
  mutate(across(everything(), as.numeric))

# ============================================================
# CFA MODELS
# ============================================================

# Model A: Usability vs. Effectiveness (all items)
model_A <- '
  usability     =~ Spaced1 + retrieval1 + retrieval3 + retrieval5 +
                   retrieval7 + retrieval9 + retrieval11
  effectiveness =~ Spaced2 + retrieval2 + retrieval4 + retrieval6 +
                   retrieval8 + retrieval10 + retrieval12
'

# Model A2: Usability vs. Effectiveness (without retrieval8)
model_A2 <- '
  usability     =~ Spaced1 + retrieval1 + retrieval3 + retrieval5 +
                   retrieval7 + retrieval9 + retrieval11
  effectiveness =~ Spaced2 + retrieval2 + retrieval4 + retrieval6 +
                   retrieval10 + retrieval12
'

# Model B: Retrieval vs. Spaced (all items)
model_B <- '
  retrieval =~ retrieval1 + retrieval2 + retrieval3 + retrieval4 + retrieval5 +
               retrieval6 + retrieval7 + retrieval8 + retrieval9 +
               retrieval10 + retrieval11 + retrieval12
  spaced    =~ Spaced1 + Spaced2
'

# Model B2: Retrieval vs. Spaced (without retrieval8)
model_B2 <- '
  retrieval =~ retrieval1 + retrieval2 + retrieval3 + retrieval4 + retrieval5 +
               retrieval6 + retrieval7 + retrieval9 +
               retrieval10 + retrieval11 + retrieval12
  spaced    =~ Spaced1 + Spaced2
'

# ============================================================
# RUN MODELS (WLSMV, ordered items)
# ============================================================
ordered_items <- c("Spaced1", "Spaced2",
                   "retrieval1", "retrieval2", "retrieval3", "retrieval4",
                   "retrieval5", "retrieval6", "retrieval7", "retrieval8",
                   "retrieval9", "retrieval10", "retrieval11", "retrieval12")

fit_A  <- cfa(model_A,  data = df_cfa, estimator = "WLSMV", ordered = ordered_items)
fit_A2 <- cfa(model_A2, data = df_cfa, estimator = "WLSMV", ordered = ordered_items)
fit_B  <- cfa(model_B,  data = df_cfa, estimator = "WLSMV", ordered = ordered_items)
fit_B2 <- cfa(model_B2, data = df_cfa, estimator = "WLSMV", ordered = ordered_items)

# ============================================================
# FIT INDICES COMPARISON
# ============================================================
get_fit <- function(fit) {
  fi <- fitMeasures(fit, c("chisq", "df", "cfi", "rmsea",
                           "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  round(fi, 3)
}

cat("\n===== FIT INDICES COMPARISON =====\n")
cat("Cutoffs: CFI > .95 | RMSEA < .06 | SRMR < .08 (Hu & Bentler, 1999)\n\n")

fits <- list(fit_A, fit_A2, fit_B, fit_B2)

get_fm <- function(f, index) round(fitMeasures(f, index), 3)

comparison <- data.frame(
  Model   = c("A  — Usability/Effectiveness (with r8)",
              "A2 — Usability/Effectiveness (no r8)",
              "B  — Retrieval/Spaced (with r8)",
              "B2 — Retrieval/Spaced (no r8)"),
  Chi2    = sapply(fits, get_fm, "chisq"),
  df      = sapply(fits, get_fm, "df"),
  p       = sapply(fits, function(f) {
    fi <- fitMeasures(f, c("chisq", "df"))
    round(pchisq(fi["chisq"], df = fi["df"], lower.tail = FALSE), 3)
  }),
  CFI          = sapply(fits, get_fm, "cfi"),
  RMSEA        = sapply(fits, get_fm, "rmsea"),
  RMSEA_CI_low = sapply(fits, get_fm, "rmsea.ci.lower"),
  RMSEA_CI_up  = sapply(fits, get_fm, "rmsea.ci.upper"),
  SRMR         = sapply(fits, get_fm, "srmr")
)
print(comparison, row.names = FALSE)

# ============================================================
# FACTOR LOADINGS (standardized)
# ============================================================
print_loadings <- function(fit, model_name) {
  cat("\n===== LOADINGS:", model_name, "=====\n")
  sl <- standardizedSolution(fit)
  print(sl[sl$op == "=~", c("lhs", "rhs", "est.std", "pvalue")])
}

print_loadings(fit_A,  "A  — Usability/Effectiveness (with r8)")
print_loadings(fit_A2, "A2 — Usability/Effectiveness (no r8)")
print_loadings(fit_B,  "B  — Retrieval/Spaced (with r8)")
print_loadings(fit_B2, "B2 — Retrieval/Spaced (no r8)")