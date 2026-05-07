# ==============================================================================
# 🎲 04_probabilistic_analysis.R — OPTIMIZED FOR N=49
# Title: Financial Literacy & Over-Indebtedness Risk (H2)
# Focus: Separation-robust visuals for small-N (N=49)
# ==============================================================================


library(tidyverse)
library(logistf)
library(marginaleffects)
library(pROC)

cat("\n🚀 Updating Script 04: Generating Full Probabilistic Visual Suite...\n")
project_root <- "C:/Documents/Education-Project2"
df <- read_csv(file.path(project_root, "data/processed/educafin_model_ready.csv"), show_col_types = FALSE) %>%
  mutate(over_indebted = as.numeric(over_indebted_flag))

fig_dir <- file.path(project_root, "outputs/figures")

# 1. ESTIMATION
logit_firth <- logistf(over_indebted ~ lit_centered + inc_centered, data = df)
# Extract probabilities manually for robust plotting
df$pred_prob <- as.numeric(predict(logit_firth, type = "response"))

# 2. 04_Probability_Distribution.png
cat("📊 Plotting Probability Distributions...\n")
png(file.path(fig_dir, "04_Probability_Distribution.png"), width = 900, height = 500, res = 150)
ggplot(df, aes(x = pred_prob, fill = factor(over_indebted))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#4CAF50", "#E94F37"), name = "Over-Indebted", labels = c("No", "Yes")) +
  labs(title = "Predicted Risk Distribution", x = "Predicted Probability", y = "Density") +
  theme_minimal()
dev.off()

# 3. 04_Marginal_Effects_Plot.png (Using marginaleffects package)
cat("📊 Plotting Marginal Effects...\n")
png(file.path(fig_dir, "04_Marginal_Effects_Plot.png"), width = 800, height = 600, res = 150)
plot_slopes(logit_firth, variables = "lit_centered", condition = "inc_centered") +
  theme_minimal() +
  labs(title = "Marginal Effect of Literacy across Income Levels",
       y = "Change in Probability of Debt", x = "Income (Centered)")
dev.off()

# 4. 04_Calibration_Plot.png
cat("📊 Plotting Calibration Curve...\n")
png(file.path(fig_dir, "04_Calibration_Plot.png"), width = 700, height = 700, res = 150)
# Create deciles of risk
df_calib <- df %>%
  mutate(bin = ntile(pred_prob, 5)) %>% # 5 bins for N=49
  group_by(bin) %>%
  summarize(obs = mean(over_indebted), pred = mean(pred_prob))

plot(df_calib$pred, df_calib$obs, type = "b", pch = 19, col = "#2E86AB",
     xlim = c(0,1), ylim = c(0,1), xlab = "Predicted", ylab = "Observed",
     main = "Model Calibration (Predicted vs. Actual)")
abline(0, 1, lty = 2, col = "gray")
dev.off()

# 5. 04_Sensitivity_Specificity.png
cat("📊 Plotting Sensitivity/Specificity Trade-off...\n")
roc_obj <- roc(df$over_indebted, df$pred_prob, quiet = TRUE)
coords_df <- coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity")) %>%
  as_tibble() %>%
  pivot_longer(cols = c(sensitivity, specificity), names_to = "Metric", values_to = "Value")

png(file.path(fig_dir, "04_Sensitivity_Specificity.png"), width = 800, height = 600, res = 150)
ggplot(coords_df, aes(x = threshold, y = Value, color = Metric)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#2E86AB", "#E94F37")) +
  theme_minimal() +
  labs(title = "Threshold Optimization", x = "Probability Cut-off", y = "Rate")
dev.off()

cat("✅ Script 04 Visuals Updated & Completed.\n")