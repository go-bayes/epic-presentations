# ============================================================================
# PSYC 434 — Lab 6: Conditional Average Treatment Effects
# self-standing script — run from top to bottom
# ============================================================================

# --- packages ---------------------------------------------------------------

required_packages <- c("grf", "tidyverse")
missing_packages <- required_packages[
  !vapply(required_packages, \(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

required_causalworkshop_exports <- c(
  "compare_ate_methods",
  "simulate_nonlinear_data",
  "simulate_nzavs_data"
)

if (!requireNamespace("causalworkshop", quietly = TRUE) ||
  !all(required_causalworkshop_exports %in% getNamespaceExports("causalworkshop"))) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("go-bayes/causalworkshop", upgrade = "never")
}

library(causalworkshop)
library(grf)
library(tidyverse)

# --- why functional form matters --------------------------------------------

# generate 2000 observations with randomised treatment (no confounding).
# the true individual effect is non-linear:
#   tau(x) = 0.3 + 0.8*sin(2*x1) + 0.5*max(x2, 0)^2 - 0.4*x1*x2*x3
# because treatment is randomised, any estimation error comes purely from
# misspecifying the shape of the effect surface, not from confounding.
d_nl <- simulate_nonlinear_data(n = 2000, seed = 2026)

# fit four models to the same data and predict individual effects tau(x_i):
#   1. OLS — assumes effects change linearly with each covariate
#   2. polynomial (degree 3) — more flexible, but still parametric
#   3. GAM — learns smooth non-linear effects from the data
#   4. causal forest — non-parametric; targets the CATE directly
# all four recover the overall ATE reasonably well, but they differ in
# how accurately they predict *individual* effects.
result <- compare_ate_methods(d_nl)

# RMSE = root mean squared error of individual-level predictions.
# lower RMSE = the method captures heterogeneity more accurately.
# OLS typically has the highest RMSE because it cannot represent
# the sinusoidal and squared terms in the true effect formula.
print(result$summary)

# --- individual treatment effects from causal forest ------------------------

# simulate 5000 people observed across three waves, modelled on the NZAVS.
# unlike the non-linear simulation above, treatment assignment here depends
# on baseline confounders (e.g., extraverts are more likely to join community
# groups), so we need adjustment — not just a flexible functional form.
d <- causalworkshop::simulate_nzavs_data(n = 5000, seed = 2026)

# split into waves. the causal forest uses:
#   wave 0 (baseline) — pre-treatment covariates X
#   wave 1 (exposure)  — treatment indicator W (community group participation)
#   wave 2 (follow-up) — outcome Y (purpose)
d0 <- d |> filter(wave == 0)
d1 <- d |> filter(wave == 1)
d2 <- d |> filter(wave == 2)

# build the covariate matrix from baseline (wave 0) variables.
# we include demographics, personality, and baseline levels of the
# exposure and outcome so the forest can adjust for confounding.
covariate_cols <- c(
  "age", "male", "nz_european", "education", "partner", "employed",
  "log_income", "nz_dep", "agreeableness", "conscientiousness",
  "extraversion", "neuroticism", "openness",
  "community_group", "purpose"
)

X <- as.matrix(d0[, covariate_cols])  # covariates (baseline)
Y <- d2$purpose                      # outcome (wave 2)
W <- d1$community_group                # treatment (wave 1)

# fit causal forest.
#   honesty = TRUE: splits the sample in two — one half chooses where to
#     split, the other half estimates effects. this prevents overfitting.
#   tune.parameters = "all": cross-validates to pick the best tuning
#     parameters (e.g., minimum node size, fraction used for splitting).
cf <- causal_forest(
  X, Y, W,
  num.trees = 1000,
  honesty = TRUE,
  tune.parameters = "all",
  seed = 2026
)

# extract the forest's estimate of each person's treatment effect.
# tau_hat[i] = predicted change in purpose if person i joins a
# community group versus does not, given their baseline covariates.
tau_hat <- predict(cf)$predictions

cat("Mean tau_hat:  ", round(mean(tau_hat), 3), "\n")
cat("SD tau_hat:    ", round(sd(tau_hat), 3), "\n")
cat("Range tau_hat: ", round(range(tau_hat), 3), "\n")

# because this is simulated data, we know the true individual effect:
#   tau = 0.20 + 0.10*extraversion + 0.05*partner - 0.03*neuroticism^2
# we can check how well the forest recovers it.
tau_true <- d0$tau_community_purpose

cat("Correlation(tau_hat, tau_true):", round(cor(tau_hat, tau_true), 3), "\n")
cat("RMSE:", round(sqrt(mean((tau_hat - tau_true)^2)), 3), "\n")

# histogram of predicted treatment effects
ggplot(data.frame(tau_hat = tau_hat), aes(x = tau_hat)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = mean(tau_hat), colour = "red", linetype = "dashed") +
  labs(
    title = "Distribution of predicted treatment effects",
    x = expression(hat(tau)(x)),
    y = "Count"
  ) +
  theme_minimal()

# --- test for heterogeneity -------------------------------------------------

# the histogram may look spread out, but is that real or noise?
# test_calibration() answers this formally. look at the row
# "differential.forest.prediction": if its coefficient is significantly
# greater than zero, the variation in tau_hat reflects genuine
# heterogeneity, not just estimation noise.
cal_test <- test_calibration(cf)
print(cal_test)

# --- variable importance ----------------------------------------------------

# which covariates drive the heterogeneity? variable_importance() measures
# how often each variable is used for splitting across all trees.
# extraversion should dominate (largest coefficient in the true formula).
# neuroticism should also rank high. partner may rank lower than expected
# because it is binary — the forest has fewer ways to split on it.
var_imp <- variable_importance(cf)
importance_df <- data.frame(
  variable = colnames(X),
  importance = as.numeric(var_imp)
) |>
  arrange(desc(importance))

print(importance_df)

# --- subgroup analysis ------------------------------------------------------

# the true effect adds +0.10 per unit of extraversion, so people above
# the median should show larger predicted effects. let's check.
# compare effects by extraversion
high_extra <- tau_hat[d0$extraversion > 0]
low_extra <- tau_hat[d0$extraversion <= 0]

cat("Mean tau_hat (high extraversion):", round(mean(high_extra), 3), "\n")
cat("Mean tau_hat (low extraversion): ", round(mean(low_extra), 3), "\n")
cat("Difference:                      ", round(mean(high_extra) - mean(low_extra), 3), "\n")

# the true effect adds +0.05 for partnered individuals
partnered <- tau_hat[d0$partner == 1]
unpartnered <- tau_hat[d0$partner == 0]

cat("\nMean tau_hat (partnered):  ", round(mean(partnered), 3), "\n")
cat("Mean tau_hat (unpartnered):", round(mean(unpartnered), 3), "\n")
cat("Difference:                ", round(mean(partnered) - mean(unpartnered), 3), "\n")

# --- scatter plot: predicted vs true effects --------------------------------

# if the forest perfectly recovered every individual effect, all points
# would lie on the red 45-degree line. shrinkage toward the mean is
# normal — the forest trades individual accuracy for stability.
ggplot(
  data.frame(true = tau_true, predicted = tau_hat),
  aes(x = true, y = predicted)
) +
  geom_point(alpha = 0.1, colour = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  labs(
    title = "Predicted vs true individual treatment effects",
    x = expression(tau(x)),
    y = expression(hat(tau)(x))
  ) +
  theme_minimal()
