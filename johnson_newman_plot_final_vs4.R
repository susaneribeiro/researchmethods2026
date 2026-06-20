# ============================================================
#  JOHNSON-NEYMAN PLOT — RETRIEVAL PRACTICE x PRIOR KNOWLEDGE
#  Standalone script (loads data and builds all variables)
#
#  Plots the simple slope of retrieval practice (X) across the
#  moderator prior knowledge (Z = centered pretest), colouring
#  the line by region of significance and marking the range of
#  observed data. The plot is cropped to the observed range of
#  the moderator, and a secondary x-axis shows prior knowledge
#  on the original percentage scale.
# ============================================================

# Run once if needed:
# install.packages(c("interactions", "ggplot2"))

library(interactions)
library(ggplot2)


# ------------------------------------------------------------
# HOW THE JOHNSON-NEYMAN BOUNDARIES ARE COMPUTED (explanation)
# ------------------------------------------------------------
# (This is documentation only; the actual computation is done by the
#  interactions package in Section 3 and verified manually in Section 3b.)
#
# The simple slope of retrieval at a given level Z of the moderator is
#   slope(Z) = b2 + b3 * Z
# where b2 is the retrieval coefficient and b3 the interaction term.
#
# This slope is significant while  |slope(Z)| > t_crit * SE(slope(Z)),
# with
#   SE(slope(Z)) = sqrt( Var(b2) + Z^2 * Var(b3) + 2*Z*Cov(b2,b3) ).
#
# The boundaries are the values of Z where the slope is exactly at the
# significance threshold, i.e. slope(Z)^2 = t_crit^2 * SE(slope(Z))^2.
# Expanding gives a quadratic in Z:
#
#   (b3^2 - t^2*Var(b3)) * Z^2
# + 2*(b2*b3 - t^2*Cov(b2,b3)) * Z
# + (b2^2 - t^2*Var(b2)) = 0
#
# Its two roots are the Johnson-Neyman boundaries. The interactions
# package solves this internally and returns them in jn$bounds
# (Johnson & Neyman, 1936; Bauer & Curran, 2005).
# ------------------------------------------------------------


# ============================================================
# 0. LOAD DATA
# ============================================================
# Adjust the path/filename if your cleaned dataset is elsewhere.

df <- read.csv("dataset_clean.csv")
cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n")


# ============================================================
# 1. CENTER PREDICTORS AND BUILD VARIABLES
# ============================================================
# Centering subtracts the mean so that 0 = the sample average.
# This is required before fitting the interaction term, so that
# the main effects are interpretable at the mean of the moderator.

pretest_mean <- mean(df$pretest_pct)   # used later to convert axis back
df$pretest_pct_c <- df$pretest_pct     - pretest_mean
df$retrieval_c   <- df$score_retrieval - mean(df$score_retrieval)

cat("Mean pretest (centering constant):", round(pretest_mean, 3), "\n")


# ============================================================
# 2. MODERATION MODEL
# ============================================================
# The "*" syntax expands to:
#   pretest_pct_c + retrieval_c + pretest_pct_c:retrieval_c
# Using "*" lets johnson_neyman() detect the predictor (pred),
# the moderator (modx), and their interaction automatically.

model_jn <- lm(posttest_pct ~ pretest_pct_c * retrieval_c, data = df)

cat("\n--- Moderation model summary ---\n")
print(summary(model_jn))


# ============================================================
# 3. JOHNSON-NEYMAN ANALYSIS
# ============================================================
# pred  = X -> the variable whose slope we are probing (retrieval)
# modx  = Z -> the moderator (centered prior knowledge)
# The analysis identifies the moderator values where the slope of
# retrieval practice crosses the threshold of statistical significance.

jn <- johnson_neyman(
  model       = model_jn,
  pred        = retrieval_c,    # X: focal predictor
  modx        = pretest_pct_c,  # Z: moderator
  alpha       = 0.05,
  control.fdr = FALSE,          # TRUE = correction for multiple comparisons
  title       = "Johnson-Neyman: slope of retrieval across prior knowledge"
)

# Console output: boundaries + textual interpretation
print(jn)

cat("\n--- Johnson-Neyman boundaries (from package) ---\n")
print(jn$bounds)
cat("Observed range of Z (centered pretest):",
    round(range(df$pretest_pct_c), 2), "\n")


# ============================================================
# 3b. MANUAL COMPUTATION OF JN BOUNDARIES (verification)
# ============================================================
# Reproduces jn$bounds by solving the quadratic in Z (see the
# explanation block at the top of this script). This must run AFTER
# the model is fitted (Section 2), because it needs model_jn.
# b2 = retrieval coefficient; b3 = interaction coefficient.

co <- coef(model_jn)
V  <- vcov(model_jn)

# NOTE: confirm the interaction name with names(coef(model_jn)).
# With the "*" formula it is "pretest_pct_c:retrieval_c".
b2 <- co["retrieval_c"]
b3 <- co["pretest_pct_c:retrieval_c"]
var_b2  <- V["retrieval_c", "retrieval_c"]
var_b3  <- V["pretest_pct_c:retrieval_c", "pretest_pct_c:retrieval_c"]
cov_b23 <- V["retrieval_c", "pretest_pct_c:retrieval_c"]

t_crit <- qt(0.975, df = df.residual(model_jn))

# Quadratic coefficients:  A*Z^2 + B*Z + C = 0
A <- b3^2  - t_crit^2 * var_b3
B <- 2 * (b2 * b3 - t_crit^2 * cov_b23)
C <- b2^2  - t_crit^2 * var_b2

disc  <- B^2 - 4 * A * C
roots <- c((-B - sqrt(disc)) / (2 * A),
           (-B + sqrt(disc)) / (2 * A))

cat("\nManual JN boundaries (should match jn$bounds):",
    round(sort(roots), 3), "\n")


# ============================================================
# 4. PREPARE BOUNDARY ANNOTATIONS
# ============================================================
# Build a data.frame with the JN boundaries (centered scale) and
# their equivalent on the original percentage scale. Keep only the
# boundaries that fall WITHIN the observed range of the moderator,
# so we do not annotate extrapolated regions.

obs_range <- range(df$pretest_pct_c)

bounds_df <- data.frame(x = as.numeric(jn$bounds))
bounds_df$x_orig <- bounds_df$x + pretest_mean
bounds_df <- bounds_df[bounds_df$x >= obs_range[1] &
                         bounds_df$x <= obs_range[2], , drop = FALSE]

cat("\n--- Boundaries within observed range ---\n")
print(bounds_df)


# ============================================================
# 5. PLOT
# ============================================================
# Single chain, always starting from jn$plot.
#  - cropped to the observed range of the moderator
#  - boundary value annotated in centered scale (bottom)
#    and in % correct (near the zero line)
#  - secondary x-axis showing the original percentage scale

p_jn <- jn$plot +
  coord_cartesian(xlim = obs_range) +
  geom_text(data = bounds_df,
            aes(x = x, y = -Inf, label = round(x, 2)),
            inherit.aes = FALSE,
            vjust = -0.6, hjust = 1.1, size = 3.2) +
  geom_text(data = bounds_df,
            aes(x = x, y = 0, label = paste0(round(x_orig, 1), "%")),
            inherit.aes = FALSE,
            vjust = -0.8, hjust = -0.1, size = 3.2, fontface = "italic") +
  scale_x_continuous(
    name     = "Prior Knowledge (centered)",
    sec.axis = sec_axis(~ . + pretest_mean, name = "Prior Knowledge (% correct)")
  ) +
  labs(y    = "Slope of Retrieval Practice",
       fill = "Significance") +
  theme_minimal(base_size = 12)

print(p_jn)

ggsave("johnson_neyman_retrieval.png", plot = p_jn,
       width = 7, height = 4.5, dpi = 300)
cat("\nSaved: johnson_neyman_retrieval.png\n")


# ============================================================
#  SUGGESTED FIGURE NOTE FOR THE PAPER
# ============================================================
# The figure shows the simple slope of retrieval practice on the
# posttest across centered prior knowledge (lower axis) and on the
# original percentage scale (upper axis). The shaded regions mark
# moderator values for which the slope differs significantly from
# zero (p < .05) versus not (n.s.); vertical dashed lines mark the
# Johnson-Neyman boundaries. The horizontal black bar indicates the
# range of prior knowledge actually observed in the sample (N = 18),
# and the plot is cropped to that range. Within the observed range,
# the slope of retrieval practice is significantly negative for
# pretest scores below approximately 74% (centered value 3.44), and
# non-significant above that point. The second Johnson-Neyman
# boundary (centered 23.36, ~94%), beyond which the slope would
# become significantly positive, lies outside the maximum observed
# pretest score and is therefore not supported by data. Given the
# small sample size, the confidence bands are wide and results
# should be interpreted with caution (see Limitations).
# ============================================================