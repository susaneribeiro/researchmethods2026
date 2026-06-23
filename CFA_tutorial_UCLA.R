# Based on this tutorial: 
# https://stats.oarc.ucla.edu/r/seminars/rcfa/
# Confirmatory Factor Analysis (CFA) in R with lavaan from UCLA 

library(lavaan)
library(haven)
library(dplyr)

# Load data
df_cfa <- read_sav("DatasetRMS_studenten7.sav") %>%
  mutate(across(everything(), as.numeric))

ordered_items <- c("Spaced1", "Spaced2",
                   "retrieval1", "retrieval2", "retrieval3", "retrieval4",
                   "retrieval5", "retrieval6", "retrieval7", "retrieval8",
                   "retrieval9", "retrieval10", "retrieval11", "retrieval12")

# ── Model A: Usability / Effectiveness (with retrieval8) ──────────────────────
# proceduralUsability_6, = retrieval 1 
# conceptualUsability_6 = retrieval 5 
# factualUsability_6 = retrieval9
# proceduralUsability_13 = retrieval3 
# conceptualUsability_13 = retrieval7 
# FactualUsability_13 = retrieval11

#spaced1 = usability 

model_A <- '
  usability     =~ Spaced1 + retrieval1 + retrieval3 + retrieval5 +
                   retrieval7 + retrieval9 + retrieval11
  effectiveness =~ Spaced2 + retrieval2 + retrieval4 + retrieval6 +
                   retrieval8 + retrieval10 + retrieval12
'
fit_A <- cfa(model_A, data = df_cfa, estimator = "WLSMV", ordered = ordered_items)
summary(fit_A, fit.measures = TRUE, standardized = TRUE)
lavInspect(fit_A, "cor.lv")


# ── Model A2: Usability / Effectiveness (without retrieval8) ──────────────────
model_A2 <- '
  usability     =~ Spaced1 + retrieval1 + retrieval3 + retrieval5 +
                   retrieval7 + retrieval9 + retrieval11
  effectiveness =~ Spaced2 + retrieval2 + retrieval4 + retrieval6 +
                   retrieval10 + retrieval12
'
fit_A2 <- cfa(model_A2, data = df_cfa, estimator = "WLSMV", ordered = ordered_items)
summary(fit_A2, fit.measures = TRUE, standardized = TRUE)

lavInspect(fit_A2, "cor.lv")


# ── Model B: Retrieval / Spaced (with retrieval8) ─────────────────────────────
model_B <- '
  retrieval =~ retrieval1 + retrieval2 + retrieval3 + retrieval4 + retrieval5 +
               retrieval6 + retrieval7 + retrieval8 + retrieval9 +
               retrieval10 + retrieval11 + retrieval12
  spaced    =~ Spaced1 + Spaced2
'
fit_B <- cfa(model_B, data = df_cfa, estimator = "WLSMV", ordered = ordered_items)
summary(fit_B, fit.measures = TRUE, standardized = TRUE)

lavInspect(fit_B, "cor.lv")

# ── Model B2: Retrieval / Spaced (without retrieval8) ────────────────────────
model_B2 <- '
  retrieval =~ retrieval1 + retrieval2 + retrieval3 + retrieval4 + retrieval5 +
               retrieval6 + retrieval7 + retrieval9 +
               retrieval10 + retrieval11 + retrieval12
  spaced    =~ Spaced1 + Spaced2
'
fit_B2 <- cfa(model_B2, data = df_cfa, estimator = "WLSMV", ordered = ordered_items)
summary(fit_B2, fit.measures = TRUE, standardized = TRUE)
lavInspect(fit_B2, "cor.lv")