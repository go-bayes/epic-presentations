# ============================================================================
# PSYC 434 — Lab 8: RATE and QINI Curves
# self-standing script — run from top to bottom
# ============================================================================

# learning aims for this lab:
#   1. translate a causal forest's individual effect estimates into a
#      *targeting rule*: a rule that picks who should receive the treatment.
#   2. read a Rank-Average Treatment Effect (RATE) curve as a diagnostic
#      of how concentrated benefit is at the top of the ranking.
#   3. read a Qini curve as a cumulative summary of targeting gain.
#   4. compute area-under-curve summaries (AUQC) and lift statistics that
#      let investigators compare candidate targeting rules.
#   5. profile who the forest treats as the high-benefit subgroup, so the
#      ranking can be interpreted, contested, and audited.
#
# context from prior weeks:
#   - week 5 introduced the average treatment effect (ATE).
#   - week 6 introduced the conditional average treatment effect (CATE),
#     written tau(x), the average effect for people with covariates x.
#   - this lab assumes we already have credible CATE estimates and asks:
#     what should we *do* with them?
#
# why this matters for psychology:
#   most interventions in our field are scarce. school programmes, clinical
#   trials, community placements, and policy pilots cannot serve everyone.
#   investigators must therefore decide who to enrol. RATE and Qini curves
#   give a transparent, auditable way to evaluate any proposed rule for
#   that decision.

# --- packages ---------------------------------------------------------------
required_packages <- c("grf", "tidyverse")
missing_packages <- required_packages[
  !vapply(required_packages, \(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

required_causalworkshop_exports <- c("simulate_nzavs_data")

if (!requireNamespace("causalworkshop", quietly = TRUE) ||
  !all(required_causalworkshop_exports %in% getNamespaceExports("causalworkshop"))) {
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }
  pak::pak("go-bayes/causalworkshop")
}



library(causalworkshop)
library(grf)
library(tidyverse)

# --- step 1: refit the causal forest from labs 5-6 --------------------------

# we re-run the simulation and forest from the previous labs so this script
# stands alone. the data come from a synthetic three-wave panel modelled on
# the New Zealand Attitudes and Values Study (NZAVS):
#   wave 0 — baseline covariates and pre-treatment outcomes
#   wave 1 — exposure assignment (community-group participation)
#   wave 2 — realised outcomes (purpose) measured after treatment
# the tau_* columns store the *true* individual treatment effects. in real
# data we never see these; the simulation gives us a benchmark to check
# whether the targeting rule is doing what we hope.
d <- causalworkshop::simulate_nzavs_data(n = 5000, seed = 2026)
d0 <- d |> filter(wave == 0)
d1 <- d |> filter(wave == 1)
d2 <- d |> filter(wave == 2)


head(d)

# build the covariate matrix from baseline (wave 0) variables. demographics,
# personality, and baseline levels of exposure and outcome go in so the
# forest can adjust for confounding while learning effect heterogeneity.
covariate_cols <- c(
  "age", "male", "nz_european", "education", "partner", "employed",
  "log_income", "nz_dep", "agreeableness", "conscientiousness",
  "extraversion", "neuroticism", "openness",
  "community_group", "purpose"
)

X <- as.matrix(d0[, covariate_cols]) # baseline covariates
Y <- d2$purpose # outcome at wave 2
W <- d1$community_group # treatment at wave 1

# fit a causal forest. honesty splits the data so different observations
# choose splits and estimate effects, which protects against overfitting.
# tune.parameters cross-validates the tuning constants.
cf <- causal_forest(
  X, Y, W,
  num.trees = 1000,
  honesty = TRUE,
  tune.parameters = "all",
  seed = 2026
)

# tau_hat[i] is the forest's estimate of the conditional average treatment
# effect for person i: how much purpose changes if person i joins a
# community group versus does not, given their baseline covariates.
tau_hat <- predict(cf)$predictions


# --- step 2: rank individuals by predicted benefit --------------------------

# the targeting question is: if we can treat only k people, who should we
# pick? the simplest answer is the top-k by tau_hat. that requires a single
# ranking of the population, from highest predicted benefit to lowest.

n <- length(tau_hat)
tau_order <- order(tau_hat, decreasing = TRUE) # indices sorted top to bottom
tau_sorted <- tau_hat[tau_order] # sorted CATE estimates

# look at the extremes to get a feel for the spread. if the top and bottom
# look almost identical, the forest sees little useful heterogeneity, and
# any targeting rule based on tau_hat will offer little gain over random
# assignment. wide spread is necessary, though not sufficient, for useful
# targeting.
cat("Top 5 predicted effects:   ", round(head(tau_sorted, 5), 3), "\n")
cat("Bottom 5 predicted effects:", round(tail(tau_sorted, 5), 3), "\n")
cat("Overall mean:              ", round(mean(tau_hat), 3), "\n")

# --- step 3: RATE curve -----------------------------------------------------

# RATE = Rank-Average Treatment Effect. for each targeting rate r in (0, 1],
# we compute the average tau_hat among the top r proportion of the ranking,
# then ask how much that exceeds the population mean (which is what random
# assignment would deliver in expectation).
#
# at r = 1.0 every rule reaches the population ATE, so the curve always
# returns to zero gain at the right edge. interest lies in the left side:
# a steeply rising curve means the forest concentrates benefit near the
# top of the ranking. a flat curve means the ranking carries no information.
#
# pedagogical note: this hand-rolled version uses tau_hat as if it were the
# truth. the formal estimator in grf::rank_average_treatment_effect() uses
# augmented inverse-propensity-weighted (AIPW) scores so the standard
# errors are valid. we compute the simpler version here to make the
# *idea* concrete; week 9 returns to inference.

rates <- seq(0.05, 1.00, by = 0.05)
rate_results <- tibble(
  rate = numeric(),
  avg_tau_targeted = numeric(),
  gain_over_random = numeric()
)

for (r in rates) {
  # keep only the top r proportion of predicted beneficiaries
  n_targeted <- floor(r * n)
  targeted_idx <- tau_order[seq_len(n_targeted)]
  # mean predicted benefit inside the targeted group
  avg_targeted <- mean(tau_hat[targeted_idx])
  # gain over random: how much more we deliver per treated person if we
  # pick the top r% versus picking r% uniformly at random.
  gain <- avg_targeted - mean(tau_hat)
  rate_results <- bind_rows(
    rate_results,
    tibble(rate = r, avg_tau_targeted = avg_targeted, gain_over_random = gain)
  )
}

# read this table column by column. avg_tau_targeted should fall as the
# rate climbs, because each step adds people with smaller predicted
# effects. gain_over_random should fall toward zero by r = 1.
print(rate_results |> mutate(across(where(is.numeric), \(x) round(x, 3))))

# plot the RATE curve. the x-axis is the targeting rate, the y-axis is
# average benefit per treated person above the population mean. interpret
# the steepness on the left as a measure of how informative the ranking
# is at small treatment budgets.
ggplot(rate_results, aes(x = rate, y = gain_over_random)) +
  geom_line(colour = "#E69F00", linewidth = 1) +
  geom_point(colour = "#E69F00", size = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "RATE curve: gain from targeting",
    x = "Targeting rate (proportion treated)",
    y = "Gain over random assignment"
  ) +
  theme_minimal()

# --- step 4: QINI curve -----------------------------------------------------

# Qini originated in marketing's uplift literature and answers a closely
# related question in cumulative form. for each share p, we compute:
#   cum_gain(p) = sum of tau_hat over the top p share
#                 minus p times the total tau_hat across the population.
# the second term is what random assignment would deliver to a fraction p
# of the population in expectation. the difference is the *extra* benefit
# unlocked by targeting.
#
# a Qini curve that bows above the diagonal indicates that the ranking
# concentrates benefit early. a curve that hugs the diagonal indicates
# that the ranking does no better than random. a curve below the diagonal
# is a warning sign: the rule actively assigns treatment away from those
# who benefit most.

qini_results <- tibble(
  percentile = numeric(),
  cumulative_gain = numeric()
)

for (p in rates) {
  # take the top p share of people according to predicted benefit
  n_top <- floor(p * n)
  top_idx <- tau_order[seq_len(n_top)]
  # cumulative extra benefit relative to giving p% of the total tau to
  # everyone: the targeting rule delivers more total benefit per unit of
  # treatment budget if this number is above zero.
  cum_gain <- sum(tau_hat[top_idx]) - p * sum(tau_hat)
  qini_results <- bind_rows(
    qini_results,
    tibble(percentile = p, cumulative_gain = cum_gain)
  )
}

# the cumulative_gain column rises, peaks somewhere in the middle of the
# population, then returns to zero at p = 1. the peak location signals
# the most informative slice of the ranking.
print(qini_results |> mutate(across(where(is.numeric), \(x) round(x, 3))))

# plot the Qini curve. interpret the height of the curve as cumulative
# extra benefit and the location of the peak as the natural cut-off
# beyond which extra targeting buys little additional gain.
ggplot(qini_results, aes(x = percentile, y = cumulative_gain)) +
  geom_line(colour = "#56B4E9", linewidth = 1) +
  geom_point(colour = "#56B4E9", size = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "QINI curve: cumulative targeting gain",
    x = "Population percentile",
    y = "Cumulative gain over random"
  ) +
  theme_minimal()

# AUQC = Area Under the Qini Curve. trapezoidal rule integrates the curve
# between p = 0 (where the gain is zero by construction) and p = 1.
# AUQC is a single-number summary of the targeting rule's quality,
# useful for comparing competing rules built from different forests,
# different covariates, or different policy constraints.
qini_for_area <- bind_rows(
  tibble(percentile = 0, cumulative_gain = 0),
  qini_results
)

auqc <- sum(
  diff(qini_for_area$percentile) *
    (head(qini_for_area$cumulative_gain, -1) +
      tail(qini_for_area$cumulative_gain, -1)) / 2
)
cat("Area Under QINI Curve (AUQC):", round(auqc, 3), "\n")

# --- step 5: targeting efficiency ------------------------------------------

# the curves above are continuous in the targeting rate. for stakeholders
# it often helps to summarise them at familiar thresholds. here we report
# the average predicted effect inside the top 10%, top 20%, and top 50%,
# alongside the population mean.

top_10_idx <- tau_order[seq_len(floor(0.10 * n))]
top_20_idx <- tau_order[seq_len(floor(0.20 * n))]
top_50_idx <- tau_order[seq_len(floor(0.50 * n))]
overall_mean <- mean(tau_hat)

# three derived quantities sit inside the table:
#   gain_vs_random    — additional benefit per treated person.
#   lift_vs_random    — multiplicative version: 2.0 means twice the average
#                       benefit, 0.5 means half. easier for non-technical
#                       audiences than additive gains.
#   efficiency_gain_pct — lift expressed as a percentage above the mean.
# the if-else guards protect against division by zero when the ATE is
# essentially zero, where lift is undefined and reporting it would mislead.
efficiency <- tibble(
  group = c("Top 10%", "Top 20%", "Top 50%", "Everyone"),
  avg_effect = c(
    mean(tau_hat[top_10_idx]),
    mean(tau_hat[top_20_idx]),
    mean(tau_hat[top_50_idx]),
    mean(tau_hat)
  )
) |>
  mutate(
    gain_vs_random = avg_effect - overall_mean,
    lift_vs_random = if (abs(overall_mean) > 1e-8) {
      avg_effect / overall_mean
    } else {
      rep(NA_real_, length(avg_effect))
    },
    efficiency_gain_pct = if (abs(overall_mean) > 1e-8) {
      round((lift_vs_random - 1) * 100, 1)
    } else {
      rep(NA_real_, length(avg_effect))
    }
  )

# read across the rows. you should see avg_effect drop monotonically from
# top 10% to everyone, with the "Everyone" row matching overall_mean.
# lift_vs_random above 1 means concentrating treatment in this slice
# delivers more benefit per person than treating everyone.
print(efficiency |> mutate(across(where(is.numeric), \(x) round(x, 3))))

# --- step 6: covariate profile of high-benefit individuals ------------------

# a ranking is only useful if it is interpretable. interpretability lets
# investigators explain the rule, audit it for fairness, and compare it
# against substantive theory. so we ask: who sits at the top?
#
# in this simulation the true individual effect is
#   tau = 0.20 + 0.10*extraversion + 0.05*partner - 0.03*neuroticism^2
# the forest never sees that formula. if the top-10% profile lines up
# with it (higher extraversion, more partnered, lower neuroticism) the
# ranking has recovered substantively meaningful structure.
top_10_data <- d0[tau_order[seq_len(floor(0.10 * n))], ]
everyone <- d0

# print the comparison. each line shows top-10% mean versus full-sample
# mean for one covariate. differences should track the true tau formula.
cat("Top 10% vs everyone:\n")
cat(
  "  Extraversion:    ", round(mean(top_10_data$extraversion), 2),
  "vs", round(mean(everyone$extraversion), 2), "\n"
)
cat(
  "  Neuroticism:     ", round(mean(top_10_data$neuroticism), 2),
  "vs", round(mean(everyone$neuroticism), 2), "\n"
)
cat(
  "  Partner (prop):  ", round(mean(top_10_data$partner), 2),
  "vs", round(mean(everyone$partner), 2), "\n"
)
cat(
  "  Agreeableness:   ", round(mean(top_10_data$agreeableness), 2),
  "vs", round(mean(everyone$agreeableness), 2), "\n"
)

# --- where this leads -------------------------------------------------------
#
# RATE and Qini curves describe how *concentrated* the benefit of a
# treatment is across a ranked population. they answer "how good is this
# ranking?" but they do not yet say *who* should receive treatment under
# real-world constraints (budgets, equity, eligibility rules).
#
# lab 9 takes the next step. policy trees turn the CATE surface into a
# short, interpretable decision rule that respects constraints and can
# be read off by a clinician, teacher, or community organiser. the
# ranking we built here is the input; the policy tree is the output.
