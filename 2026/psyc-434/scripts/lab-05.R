# ============================================================================
# PSYC 434 — Lab 5: Average Treatment Effects
# self-standing script — run from top to bottom
# ============================================================================

# --- packages ---------------------------------------------------------------

# install only the packages that are missing
required_packages <- c("grf", "tidyverse")
missing_packages <- required_packages[
  !vapply(required_packages, \(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

required_causalworkshop_exports <- c("simulate_nzavs_data")

# install or refresh causalworkshop only if the required export is missing
if (!requireNamespace("causalworkshop", quietly = TRUE) ||
  !all(required_causalworkshop_exports %in% getNamespaceExports("causalworkshop"))) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  if (dir.exists("/Users/joseph/GIT/causalworkshop")) {
    remotes::install_local("/Users/joseph/GIT/causalworkshop", force = TRUE)
  } else {
    remotes::install_github("go-bayes/causalworkshop", force = TRUE)
  }
}

library(causalworkshop)
library(grf)
library(tidyverse)


# --- simulate data ----------------------------------------------------------

# generate a three-wave panel with known treatment effects
# wave 0 = baseline confounders, wave 1 = exposure, wave 2 = outcome
d <- causalworkshop::simulate_nzavs_data(n = 5000, seed = 2026)
dim(d)
names(d)

# split into waves
d0 <- d |> dplyr::filter(wave == 0) # baseline confounders (pre-treatment)
d1 <- d |> dplyr::filter(wave == 1) # exposure assignment
d2 <- d |> dplyr::filter(wave == 2) # outcomes measured after exposure

# check that the same people stay aligned across waves
stopifnot(all(d0$id == d1$id), all(d0$id == d2$id))

# the causal contrast: community_group = 1 (participates in a community
# group at wave 1) versus community_group = 0 (does not participate).
# the outcome is purpose at wave 2.
# tau_community_purpose stores the TRUE individual-level causal effect
# for each person, so the population mean of tau is the true ATE.
true_ate <- mean(d0$tau_community_purpose)
cat("True ATE:", round(true_ate, 3), "\n")

# --- naive ATE (biased) ----------------------------------------------------

# compare treated and untreated without adjusting for confounders.
# this estimate is biased because people who join community groups differ
# systematically from those who do not: they score higher in extraversion
# and agreeableness, lower in neuroticism, are more likely to have partners,
# and live in less deprived neighbourhoods. all of these traits independently
# boost purpose, so the naive comparison conflates the causal effect of
# community participation with pre-existing differences between groups.
# the confounding runs upward: the naive estimate overstates the true effect.
fit_naive <- lm(d2$purpose ~ d1$community_group)
naive_ate <- coef(fit_naive)[2]
cat("Naive ATE:", round(naive_ate, 3), "\n")
cat("True ATE: ", round(true_ate, 3), "\n")
cat("Bias:     ", round(naive_ate - true_ate, 3), "\n")

# --- adjusted ATE (regression) ---------------------------------------------

# adjusting for baseline confounders blocks the backdoor paths that created
# the upward bias in the naive estimate.
# collect baseline confounders and pre-treatment outcome values
df <- data.frame(
  y = d2$purpose,
  a = d1$community_group,
  age = d0$age,
  male = d0$male,
  nz_european = d0$nz_european,
  education = d0$education,
  partner = d0$partner,
  employed = d0$employed,
  log_income = d0$log_income,
  nz_dep = d0$nz_dep,
  agreeableness = d0$agreeableness,
  conscientiousness = d0$conscientiousness,
  extraversion = d0$extraversion,
  neuroticism = d0$neuroticism,
  openness = d0$openness,
  community_t0 = d0$community_group,
  purpose_t0 = d0$purpose
)

# regress the outcome on treatment plus observed confounders
fit_adj <- lm(y ~ a + age + male + nz_european + education + partner +
  employed + log_income + nz_dep + agreeableness +
  conscientiousness + extraversion + neuroticism + openness +
  community_t0 + purpose_t0, data = df)

adj_ate <- coef(fit_adj)["a"]
cat("Adjusted ATE:", round(adj_ate, 3), "\n")
cat("True ATE:    ", round(true_ate, 3), "\n")
cat("Bias:        ", round(adj_ate - true_ate, 3), "\n")

# --- g-computation by hand -------------------------------------------------

# g-computation makes the causal contrast explicit: predict each person's
# outcome under treatment (a = 1) and under no treatment (a = 0), then
# average the difference. this is the "two states" logic of causal inference.

# predict outcomes if everyone were treated
df_treated <- df
df_treated$a <- 1

# predict outcomes if no one were treated
df_control <- df
df_control$a <- 0

y_hat_treated <- predict(fit_adj, newdata = df_treated)
y_hat_control <- predict(fit_adj, newdata = df_control)

# average the individual-level contrasts to get the ATE
gcomp_ate <- mean(y_hat_treated - y_hat_control)
cat("G-computation ATE:", round(gcomp_ate, 3), "\n")
cat("True ATE:         ", round(true_ate, 3), "\n")

# --- causal forest ATE ------------------------------------------------------

# a causal forest is a non-parametric estimator that allows the treatment
# effect to vary flexibly across people. unlike linear regression, it makes
# no assumptions about the shape of the outcome surface.
#
# in this simulation the true outcome model is nearly linear and the treatment
# effect heterogeneity is mild (small personality modifiers). that means OLS
# is close to correctly specified and pays no price for its parametric
# assumptions. the causal forest, by contrast, uses honesty (sample splitting)
# and estimates a flexible function it does not need, paying a variance cost
# for flexibility that buys nothing here.
#
# this is the bias-variance tradeoff: when the true model is smooth and
# additive, parametric estimators are more efficient. causal forests earn
# their keep when the outcome surface is non-linear or when we care about
# heterogeneous treatment effects (week 8).
covariate_cols <- c(
  "age", "male", "nz_european", "education", "partner", "employed",
  "log_income", "nz_dep", "agreeableness", "conscientiousness",
  "extraversion", "neuroticism", "openness",
  "community_t0", "purpose_t0"
)

X <- as.matrix(df[, covariate_cols])
Y <- df$y
W <- df$a

cf <- grf::causal_forest(
  X, Y, W,
  num.trees = 1000,
  honesty = TRUE,
  tune.parameters = "all",
  seed = 2026
)

ate_cf <- grf::average_treatment_effect(cf)
cat(
  "Causal forest ATE:", round(unname(ate_cf["estimate"]), 3),
  "(SE:", round(unname(ate_cf["std.err"]), 3), ")\n"
)
cat("True ATE:         ", round(true_ate, 3), "\n")

# --- compare all estimates --------------------------------------------------

# the adjusted regression and g-computation estimates are close to the true ATE
# because the true outcome surface is nearly linear and OLS is well-specified.
# the causal forest is unbiased but slightly noisier: it pays a variance cost
# for flexibility the data do not require. all three adjusted estimators remove
# the upward bias visible in the naive estimate.
results <- data.frame(
  method = c("Naive", "Adjusted regression", "G-computation", "Causal forest"),
  estimate = c(
    unname(naive_ate), unname(adj_ate), unname(gcomp_ate), unname(ate_cf["estimate"])
  ),
  bias = c(
    unname(naive_ate - true_ate), unname(adj_ate - true_ate),
    unname(gcomp_ate - true_ate), unname(ate_cf["estimate"] - true_ate)
  )
)
results$estimate <- round(results$estimate, 3)
results$bias <- round(results$bias, 3)
print(results)
cat("\nTrue ATE:", round(true_ate, 3), "\n")

# --- optional extension: iptw in one short example --------------------------

# this is the same estimand by a different route
# g-computation models the outcome given treatment and confounders
# iptw models treatment given confounders, then reweights the sample

# estimate the probability of treatment from baseline confounders
ps_model <- glm(
  a ~ age + male + nz_european + education + partner +
    employed + log_income + nz_dep + agreeableness +
    conscientiousness + extraversion + neuroticism + openness +
    community_t0 + purpose_t0,
  data = df,
  family = binomial()
)

ps_hat <- predict(ps_model, type = "response")

# use stabilised weights to reduce variability
p_treated <- mean(df$a)
iptw <- ifelse(
  df$a == 1,
  p_treated / ps_hat,
  (1 - p_treated) / (1 - ps_hat)
)

# a very small weight check
weight_summary <- tibble(
  statistic = c("min", "median", "max"),
  value = c(min(iptw), median(iptw), max(iptw))
) |>
  mutate(value = round(value, 3))

print(weight_summary)

# fit a weighted outcome model with treatment only
fit_iptw <- lm(y ~ a, data = df, weights = iptw)
iptw_ate <- coef(fit_iptw)[["a"]]

iptw_results <- tibble(
  method = c("G-computation", "IPTW"),
  estimate = c(gcomp_ate, iptw_ate),
  bias = c(gcomp_ate - true_ate, iptw_ate - true_ate)
) |>
  mutate(
    estimate = round(estimate, 3),
    bias = round(bias, 3)
  )

print(iptw_results)
