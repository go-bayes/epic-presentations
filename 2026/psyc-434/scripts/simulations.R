# ==============================================================================
# simulation script: population estimates and causal inference
# psyc 434 — causal inference for psychological science (2026)
# adapted from the 2025 sasp causal inference workshop
# ==============================================================================

# load required libraries --------------------------------------------------
library(tidyverse)
library(stdReg)
library(gtsummary)
library(clarify)
library(grf)

# ==============================================================================
# helper: simulate ate data with weights
# (replaces margot::simulate_ate_data_with_weights)
# ==============================================================================

simulate_ate_data_with_weights <- function(n_sample = 10000,
                                           n_population = 100000,
                                           p_z_sample = 0.1,
                                           p_z_population = 0.5,
                                           beta_a = 1,
                                           beta_z = 2.5,
                                           noise_sd = 0.5) {
  # sample data
  z_sample <- rbinom(n_sample, 1, p_z_sample)
  a_sample <- rbinom(n_sample, 1, 0.5)
  y_sample <- beta_a * a_sample + beta_z * a_sample * z_sample +
    rnorm(n_sample, 0, noise_sd)

  # population data
  z_population <- rbinom(n_population, 1, p_z_population)
  a_population <- rbinom(n_population, 1, 0.5)
  y_population <- beta_a * a_population + beta_z * a_population * z_population +
    rnorm(n_population, 0, noise_sd)

  # compute weights (inverse probability of being in the sample given z)
  p_sample_z1 <- (n_sample * p_z_sample) /
    (n_sample * p_z_sample + n_population * p_z_population)
  p_sample_z0 <- (n_sample * (1 - p_z_sample)) /
    (n_sample * (1 - p_z_sample) + n_population * (1 - p_z_population))

  weights <- ifelse(z_sample == 1, 1 / p_sample_z1, 1 / p_sample_z0)
  weights <- weights / mean(weights) # normalise

  sample_data <- data.frame(
    y_sample = y_sample,
    a_sample = a_sample,
    z_sample = z_sample,
    weights = weights
  )

  population_data <- data.frame(
    y_population = y_population,
    a_population = a_population,
    z_population = z_population
  )

  list(sample_data = sample_data, population_data = population_data)
}

# ==============================================================================
# s2/s3: generalisability and transportability simulation
# ==============================================================================

# simulate data with different distributions of effect modifiers
# between sample and population
set.seed(123)

data <- simulate_ate_data_with_weights(
  n_sample = 10000,
  n_population = 100000,
  p_z_sample = 0.1,
  p_z_population = 0.5,
  beta_a = 1,
  beta_z = 2.5,
  noise_sd = 0.5
)

# extract sample and population data
sample_data <- data$sample_data
population_data <- data$population_data

# check imbalance in effect modifier distribution
cat("\nsample distribution of effect modifier:\n")
print(table(sample_data$z_sample))

cat("\npopulation distribution of effect modifier:\n")
print(table(population_data$z_population))

# model coefficients: sample
model_sample <- glm(y_sample ~ a_sample * z_sample,
  data = sample_data
)

cat("\n=== model coefficients: sample ===\n")
print(summary(model_sample)$coefficients)

# model coefficients: weighted sample
model_weighted_sample <- glm(y_sample ~ a_sample * z_sample,
  data = sample_data, weights = weights
)

cat("\n=== model coefficients: weighted sample ===\n")
print(summary(model_weighted_sample)$coefficients)

# model coefficients: population
model_population <- glm(y_population ~ a_population * z_population,
  data = population_data
)

cat("\n=== model coefficients: population ===\n")
print(summary(model_population)$coefficients)

# marginal effect estimates using stdreg
# sample ate
fit_std_sample <- stdReg::stdGlm(model_sample,
  data = sample_data, X = "a_sample"
)

cat("\n=== sample ate ===\n")
print(summary(fit_std_sample, contrast = "difference", reference = 0))

# population ate (oracle)
fit_std_population <- stdReg::stdGlm(model_population,
  data = population_data, X = "a_population"
)

cat("\n=== population ate (oracle) ===\n")
print(summary(fit_std_population, contrast = "difference", reference = 0))

# weighted sample ate
fit_std_weighted_sample_weights <- stdReg::stdGlm(model_weighted_sample,
  data = sample_data, X = "a_sample"
)

cat("\n=== weighted sample ate ===\n")
print(summary(fit_std_weighted_sample_weights, contrast = "difference", reference = 0))

# manual ate calculation using predict function ----------------------------
cat("\n=== manual ate calculation using predict function ===\n")
cat("this section explicitly shows what stdReg is doing under the hood\n\n")

# create counterfactual datasets where everyone gets a = 0 and a = 1
sample_data_a0 <- sample_data |> mutate(a_sample = 0)
sample_data_a1 <- sample_data |> mutate(a_sample = 1)

# predict outcomes under each treatment level
y_pred_a0_sample <- predict(model_sample, newdata = sample_data_a0)
y_pred_a1_sample <- predict(model_sample, newdata = sample_data_a1)

# calculate ate as mean difference in predicted outcomes
ate_sample_manual <- mean(y_pred_a1_sample - y_pred_a0_sample)

cat("sample ate (manual calculation):", round(ate_sample_manual, 3), "\n")

# population ate using predict
population_data_a0 <- population_data |> mutate(a_population = 0)
population_data_a1 <- population_data |> mutate(a_population = 1)

y_pred_a0_population <- predict(model_population, newdata = population_data_a0)
y_pred_a1_population <- predict(model_population, newdata = population_data_a1)

ate_population_manual <- mean(y_pred_a1_population - y_pred_a0_population)

cat("population ate (manual calculation):", round(ate_population_manual, 3), "\n")

# weighted sample ate using predict
y_pred_a0_weighted <- predict(model_weighted_sample, newdata = sample_data_a0)
y_pred_a1_weighted <- predict(model_weighted_sample, newdata = sample_data_a1)

ate_weighted_sample_manual <- mean(y_pred_a1_weighted - y_pred_a0_weighted)

cat("weighted sample ate (manual calculation):", round(ate_weighted_sample_manual, 3), "\n")

# key insight
cat("\n=== key insight ===\n")
cat("1. model coefficients are nearly identical across sample, weighted sample, and population\n")
cat(
  "2. but the marginal ates differ between sample (", round(ate_sample_manual, 3),
  ") and population (", round(ate_population_manual, 3), ")\n"
)
cat("3. weighting corrects this, giving ate = ", round(ate_weighted_sample_manual, 3), "\n")
cat("4. this happens because the distribution of the effect modifier differs\n")
cat("   sample: z=1 is rare; population: z=1 is common\n\n")

# ==============================================================================
# s4: cross-sectional data ambiguity simulation
# ==============================================================================

# simulate data where l is a mediator between a and y
set.seed(123)

n <- 1000
p <- 0.5
alpha <- 0
beta <- 2
gamma <- 1
delta <- 1.5
sigma_L <- 1
sigma_Y <- 1.5

# simulate the data: fully mediated effect by l
A <- rbinom(n, 1, p)
L <- alpha + beta * A + rnorm(n, 0, sigma_L)
Y <- gamma + delta * L + rnorm(n, 0, sigma_Y)

# create data frame
data_cross <- data.frame(A = A, L = L, Y = Y)

# fit regression models
# model 1: control for l (assuming l is confounder)
fit_1 <- lm(Y ~ A + L, data = data_cross)

# model 2: omit l (assuming l is mediator)
fit_2 <- lm(Y ~ A, data = data_cross)

# create comparison table
table1 <- gtsummary::tbl_regression(fit_1)
table2 <- gtsummary::tbl_regression(fit_2)

table_comparison <- gtsummary::tbl_merge(
  list(table1, table2),
  tab_spanner = c(
    "Model: L assumed confounder",
    "Model: L assumed mediator"
  )
)

cat("\n=== cross-sectional model comparison ===\n")
print(table_comparison)

# calculate ate using clarify
set.seed(2025)
sim_coefs_fit_1 <- sim(fit_1)
sim_coefs_fit_2 <- sim(fit_2)

sim_est_fit_1 <- sim_ame(
  sim_coefs_fit_1,
  var = "A",
  subset = A == 1,
  contrast = "RD",
  verbose = FALSE
)

sim_est_fit_2 <- sim_ame(
  sim_coefs_fit_2,
  var = "A",
  subset = A == 1,
  contrast = "RD",
  verbose = FALSE
)

summary_sim_est_fit_1 <- summary(sim_est_fit_1, null = c(`RD` = 0))
summary_sim_est_fit_2 <- summary(sim_est_fit_2, null = c(`RD` = 0))

cat("\n=== ate estimates ===\n")
cat(
  "model 1 (l as confounder): ate =",
  round(summary_sim_est_fit_1[3, 1], 2), "\n"
)
cat(
  "model 2 (l as mediator): ate =",
  round(summary_sim_est_fit_2[3, 1], 2), "\n"
)

# ==============================================================================
# appendix d: confounding control strategies simulation
# ==============================================================================

set.seed(123)
n <- 10000

# baseline covariates
U <- rnorm(n)
A_0 <- rbinom(n, 1, prob = plogis(U))
Y_0 <- rnorm(n, mean = U, sd = 1)
L_0 <- rnorm(n, mean = U, sd = 1)

# coefficients for treatment assignment
beta_A0 <- 0.25
beta_Y0 <- 0.3
beta_L0 <- 0.2
beta_U <- 0.1

# simulate treatment assignment
A_1 <- rbinom(n, 1, prob = plogis(
  -0.5 +
    beta_A0 * A_0 +
    beta_Y0 * Y_0 +
    beta_L0 * L_0 +
    beta_U * U
))

# coefficients for continuous outcome
delta_A1 <- 0.3
delta_Y0 <- 0.9
delta_A0 <- 0.1
delta_L0 <- 0.3
theta_A0Y0L0 <- 0.5
delta_U <- 0.05

# simulate continuous outcome
Y_2 <- rnorm(n,
  mean = 0 +
    delta_A1 * A_1 +
    delta_Y0 * Y_0 +
    delta_A0 * A_0 +
    delta_L0 * L_0 +
    theta_A0Y0L0 * Y_0 * A_0 * L_0 +
    delta_U * U,
  sd = .5
)

# create data frame
data_confound <- data.frame(Y_2, A_0, A_1, L_0, Y_0, U)

# fit models
fit_no_control <- lm(Y_2 ~ A_1, data = data_confound)
fit_standard <- lm(Y_2 ~ A_1 + L_0, data = data_confound)
fit_interaction <- lm(Y_2 ~ A_1 * (L_0 + A_0 + Y_0), data = data_confound)

# create gtsummary tables
tbl_fit_no_control <- tbl_regression(fit_no_control)
tbl_fit_standard <- tbl_regression(fit_standard)
tbl_fit_interaction <- tbl_regression(fit_interaction)

# filter to show only treatment variable
tbl_list_modified <- lapply(
  list(tbl_fit_no_control, tbl_fit_standard, tbl_fit_interaction),
  function(tbl) {
    tbl |>
      modify_table_body(~ .x |> dplyr::filter(variable == "A_1"))
  }
)

# merge tables
table_comparison_confound <- tbl_merge(
  tbls = tbl_list_modified,
  tab_spanner = c("No Control", "Standard", "Interaction")
)

cat("\n=== confounding control strategy comparison ===\n")
print(table_comparison_confound)

# calculate ate using clarify
set.seed(123)
sim_coefs_fit_no_control <- sim(fit_no_control)
sim_coefs_fit_std <- sim(fit_standard)
sim_coefs_fit_int <- sim(fit_interaction)

sim_est_fit_no_control <- sim_ame(
  sim_coefs_fit_no_control,
  var = "A_1",
  subset = A_1 == 1,
  contrast = "RD",
  verbose = FALSE
)

sim_est_fit_std <- sim_ame(
  sim_coefs_fit_std,
  var = "A_1",
  subset = A_1 == 1,
  contrast = "RD",
  verbose = FALSE
)

sim_est_fit_int <- sim_ame(
  sim_coefs_fit_int,
  var = "A_1",
  subset = A_1 == 1,
  contrast = "RD",
  verbose = FALSE
)

summary_sim_coefs_fit_no_control <- summary(sim_est_fit_no_control, null = c(`RD` = 0))
summary_sim_est_fit_std <- summary(sim_est_fit_std, null = c(`RD` = 0))
summary_sim_est_fit_int <- summary(sim_est_fit_int, null = c(`RD` = 0))

cat("\n=== ate estimates for confounding control strategies ===\n")
cat(
  "no control: ate =",
  round(summary_sim_coefs_fit_no_control[3, 1], 2), "\n"
)
cat(
  "standard control: ate =",
  round(summary_sim_est_fit_std[3, 1], 2), "\n"
)
cat(
  "interaction model: ate =",
  round(summary_sim_est_fit_int[3, 1], 2), "\n"
)

# ==============================================================================
# appendix e: causal forest estimation
# ==============================================================================

# prepare data for causal forest
W <- as.matrix(data_confound$A_1)
Y <- as.matrix(data_confound$Y_2)
X <- as.matrix(data_confound[, c("L_0", "A_0", "Y_0")])

# fit causal forest model
fit_causal_forest <- causal_forest(X, Y, W)

# estimate average treatment effect
ate_cf <- average_treatment_effect(fit_causal_forest)
ate_cf_df <- data.frame(ate_cf)

cat("\n=== causal forest ate estimate ===\n")
cat(
  "ate =", round(ate_cf_df[1, 1], 2),
  ", se =", round(ate_cf_df[2, 1], 2), "\n"
)

# ==============================================================================
# end of script
# ==============================================================================

cat("\n=== all simulations completed successfully ===\n")
