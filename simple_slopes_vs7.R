library(ggplot2)

# --- Dados ---
df <- read.csv("dataset_clean.csv")

# Centralizar e criar interação
df$pretest_pct_c <- df$pretest_pct - mean(df$pretest_pct)
df$retrieval_c   <- df$score_retrieval - mean(df$score_retrieval)
df$pretest_pct_x_retrieval <- df$pretest_pct_c * df$retrieval_c

# --- Modelo ---
model_retrieval_B <- lm(posttest_pct ~ pretest_pct_c + retrieval_c + pretest_pct_x_retrieval, data = df)

b0 <- coef(model_retrieval_B)["(Intercept)"]
b1 <- coef(model_retrieval_B)["pretest_pct_c"]
b2 <- coef(model_retrieval_B)["retrieval_c"]
b3 <- coef(model_retrieval_B)["pretest_pct_x_retrieval"]

# --- Linhas +1SD / -1SD ---
pretest_high <-  sd(df$pretest_pct_c)
pretest_low  <- -sd(df$pretest_pct_c)
retrieval_vals <- seq(min(df$retrieval_c), max(df$retrieval_c), length.out = 100)

predicted_high <- b0 + b1*pretest_high + b2*retrieval_vals + b3*pretest_high*retrieval_vals
predicted_low  <- b0 + b1*pretest_low  + b2*retrieval_vals + b3*pretest_low *retrieval_vals

plot_data <- data.frame(
  retrieval = c(retrieval_vals, retrieval_vals),
  posttest  = c(predicted_high, predicted_low),
  group     = factor(c(rep("High (+1 SD)", 100), rep("Low (-1 SD)", 100)),
                     levels = c("High (+1 SD)", "Low (-1 SD)"))
)

# --- Gráfico ---
ggplot(plot_data, aes(x = retrieval, y = posttest, color = group, linetype = group)) +
  geom_jitter(data = df,
              aes(x = retrieval_c, y = posttest_pct, fill = pretest_pct_c),
              shape = 21, size = 3, alpha = 0.7, inherit.aes = FALSE,
              width = 0.01, height = 0.5) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_fill_gradientn(colors = c("red", "white", "blue"),
                       values = scales::rescale(c(min(df$pretest_pct_c), 0, max(df$pretest_pct_c))),
                       name = "Prior Knowledge (centered)") +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_color_manual(values = c("#08306b", "#cb181d")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x = "Retrieval Practice (centered)", y = "Posttest Score (%)",
       color = "Prior Knowledge", linetype = "Prior Knowledge",
       title = "Moderation of Prior Knowledge on Retrieval Practice") +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("simple_slopes_retrieval_LLO.png", width = 6, height = 4.5, dpi = 300)