# ============================================================================
# psyc 434 -- lab 4: instructor notes and extensions
# self-standing script -- run from top to bottom
# ============================================================================

# --- packages ---------------------------------------------------------------
# install only the packages that are missing
required_packages <- c("tidyverse", "parameters")

missing_packages <- required_packages[
  !vapply(required_packages, \(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(parameters)

# students should start with `scripts/lab-04-student.R`
# use this script after the student version
# this fuller script adds:
# - more annotation around the simulation code
# - extra exercises with a factor predictor and a curved relationship
# - cleaner comparison tables
# - an optional extension on selection into the sample and
#   sample versus population estimates
# that final section reconnects the lab to the week 4 theme
# it is useful, but it is not the place to start if you are still getting
# comfortable with r syntax

# small helper to compare the fitted coefficient with the true one
show_term <- function(model, term, true_value) {
  if (!term %in% names(coef(model))) {
    cat(term, "is not in the current model\n")
    return(invisible(NULL))
  }

  estimate <- unname(coef(model)[[term]])

  cat(
    term, "estimate =", round(estimate, 2),
    "| true value =", round(true_value, 2), "\n"
  )
}

# --- exercise 1: simple linear regression -----------------------------------
# `set.seed()` makes the random numbers reproducible
# that means everyone should get the same simulated data
set.seed(434)

# `n` is the number of students we simulate
n <- 200

# `rnorm(n, mean = 10, sd = 2)` draws 200 values from a normal distribution
# centred on 10 with a standard deviation of 2
study_hours <- rnorm(n, mean = 10, sd = 2)

# this line builds exam scores from a linear model
# start at 40, add 4 points for each extra study hour,
# then add random error with `rnorm(n, mean = 0, sd = 5)`
exam_score <- 40 + 4 * study_hours + rnorm(n, mean = 0, sd = 5)

# `tibble()` makes a data frame
# this stores the two variables together in one rectangular dataset
df_simple <- tibble(
  study_hours = study_hours,
  exam_score = exam_score
)

# `ggplot(data, aes(...))` starts a graph
# `aes(x = ..., y = ...)` maps variables to the x-axis and y-axis
# `geom_point()` draws one point for each student
# `geom_smooth(method = "lm")` adds the fitted regression line
# `se = FALSE` hides the uncertainty band for now
# `labs()` adds the title and subtitle
ggplot(df_simple, aes(x = study_hours, y = exam_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "exercise 1",
    subtitle = "one predictor, one outcome"
  )

# coding practice:
# 1. run the model below as written
# 2. change the formula to `exam_score ~ 1`
# 3. rerun the next few lines and see what changes

# `lm()` fits a linear model
# the outcome goes on the left of `~`
# the predictor goes on the right of `~`
# `data = df_simple` tells r where to find the variables
fit_1 <- lm(
  exam_score ~ study_hours,
  data = df_simple
)

# this is the standard r output for a fitted regression
summary(fit_1)

# this prints a more readable summary of the same model
parameters::model_parameters(fit_1)
show_term(fit_1, "study_hours", true_value = 4)

# `predict(fit_1)` gives the fitted value for each student from the model
pred_1 <- df_simple |>
  mutate(fitted_value = predict(fit_1)) |>
  arrange(study_hours)

# this plot overlays the fitted values on the raw data
ggplot(pred_1, aes(x = study_hours, y = exam_score)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = fitted_value), colour = "steelblue", linewidth = 1) +
  labs(
    title = "exercise 1 fitted values",
    subtitle = "change the formula above and rerun this plot"
  )


# --- exercise 2: two predictors ---------------------------------------------
set.seed(435)
n <- 300

# `rnorm(n, mean = 0, sd = 1)` creates a motivation variable
# centred on 0, where positive values mean above-average motivation
motivation <- rnorm(n, mean = 0, sd = 1)

# study hours now depend partly on motivation
# the last `rnorm()` term adds person-to-person variation not explained by motivation
study_hours <- 8 + 1.5 * motivation + rnorm(n, mean = 0, sd = 1.5)

# exam score depends on both study hours and motivation
# the final `rnorm()` term is random error around that linear relationship
exam_score <- 55 + 2.5 * study_hours + 6 * motivation + rnorm(n, mean = 0, sd = 6)

df_multiple <- tibble(
  study_hours = study_hours,
  motivation = motivation,
  exam_score = exam_score
)

ggplot(df_multiple, aes(x = motivation, y = study_hours)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "exercise 2",
    subtitle = "motivation is related to study hours"
  )

# coding practice:
# 1. run the two models below as written
# 2. change `fit_2_adjusted` to `exam_score ~ study_hours`
# 3. rerun the tables and compare the study-hours coefficient
# 4. change it back to `exam_score ~ study_hours + motivation`

fit_2_naive <- lm(
  exam_score ~ study_hours,
  data = df_multiple
)

fit_2_adjusted <- lm(
  exam_score ~ study_hours + motivation,
  data = df_multiple
)

parameters::model_parameters(fit_2_naive)
parameters::model_parameters(fit_2_adjusted)

comparison_2 <- tibble(
  model = c("naive", "adjusted"),
  study_hours_estimate = c(
    coef(fit_2_naive)[["study_hours"]],
    coef(fit_2_adjusted)[["study_hours"]]
  ),
  true_value = 2.5
) |>
  mutate(
    study_hours_estimate = round(study_hours_estimate, 2),
    true_value = round(true_value, 2)
  )

print(comparison_2)

# the adjusted model gets closer to the data-generating slope for study hours


# --- exercise 3: a categorical predictor ------------------------------------
set.seed(436)
n <- 360

# `runif(n, min = 0, max = 12)` draws values evenly between 0 and 12
practice_hours <- runif(n, min = 0, max = 12)

# `sample(..., replace = TRUE)` randomly assigns each student to one method
teaching_method <- sample(
  c("standard", "practice", "feedback"),
  size = n,
  replace = TRUE
)

# `case_when()` converts each teaching method into its score effect
method_effect <- case_when(
  teaching_method == "standard" ~ 0,
  teaching_method == "practice" ~ 4,
  teaching_method == "feedback" ~ 7
)

# quiz score starts at 50, increases with practice hours,
# shifts up or down by teaching method, and then adds random error
quiz_score <- 50 + 1.8 * practice_hours + method_effect + rnorm(n, mean = 0, sd = 4)

df_factor <- tibble(
  practice_hours = practice_hours,
  teaching_method = factor(
    teaching_method,
    levels = c("standard", "practice", "feedback")
  ),
  quiz_score = quiz_score
)

ggplot(df_factor, aes(x = teaching_method, y = quiz_score, fill = teaching_method)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  labs(
    title = "exercise 3",
    subtitle = "a factor predictor changes the intercept"
  )

# coding practice:
# 1. run the model below as written
# 2. change it to `quiz_score ~ practice_hours`
# 3. rerun the coefficient table
# 4. then add `teaching_method` back in and compare

fit_3 <- lm(
  quiz_score ~ practice_hours + teaching_method,
  data = df_factor
)

# r automatically turns the factor into indicator variables
parameters::model_parameters(fit_3)

# `standard` is the reference group because it is listed first in the factor
show_term(fit_3, "practice_hours", true_value = 1.8)


# --- exercise 4: an interaction ---------------------------------------------
set.seed(437)
n <- 320

# `runif()` draws study hours evenly between 0 and 10
study_hours <- runif(n, min = 0, max = 10)

# `rbinom(n, 1, 0.5)` draws 0 or 1
# here, 1 means attended the workshop and 0 means did not
workshop <- rbinom(n, 1, 0.5)

# this outcome has an interaction
# study hours matter for everyone, workshop adds a boost,
# and the `1.5 * study_hours * workshop` term makes the slope steeper
# for students who attended the workshop
exam_score <- 45 + 2 * study_hours + 5 * workshop +
  1.5 * study_hours * workshop + rnorm(n, mean = 0, sd = 4)

df_interaction <- tibble(
  study_hours = study_hours,
  workshop = workshop,
  exam_score = exam_score
)

ggplot(
  df_interaction,
  aes(x = study_hours, y = exam_score, colour = factor(workshop))
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "exercise 4",
    subtitle = "the slope for study hours depends on workshop attendance",
    colour = "workshop"
  )

# coding practice:
# 1. run the model below as written
# 2. change the formula to `exam_score ~ study_hours + workshop`
# 3. rerun the coefficient table and the fitted-value plot
# 4. then add the interaction back with `study_hours * workshop`

fit_4 <- lm(
  exam_score ~ study_hours * workshop,
  data = df_interaction
)

parameters::model_parameters(fit_4)
show_term(fit_4, "study_hours", true_value = 2)
show_term(fit_4, "workshop", true_value = 5)
show_term(fit_4, "study_hours:workshop", true_value = 1.5)

pred_4 <- df_interaction |>
  mutate(fitted_value = predict(fit_4)) |>
  arrange(workshop, study_hours)

ggplot(
  pred_4,
  aes(x = study_hours, y = exam_score, colour = factor(workshop))
) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = fitted_value), linewidth = 1) +
  labs(
    title = "exercise 4 fitted values",
    subtitle = "change the formula above and rerun this plot",
    colour = "workshop"
  )


# --- exercise 5: a curved relationship --------------------------------------
set.seed(438)
n <- 250

# `runif()` draws stress values evenly between 0 and 10
stress <- runif(n, min = 0, max = 10)

# this outcome follows a quadratic relationship
# the `+ 6 * stress` part makes scores rise at first
# the `- 0.5 * stress^2` part bends the curve downward
# the final `rnorm()` term adds random error
task_score <- 35 + 6 * stress - 0.5 * stress^2 + rnorm(n, mean = 0, sd = 3)

df_curve <- tibble(
  stress = stress,
  task_score = task_score
)

ggplot(df_curve, aes(x = stress, y = task_score)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "exercise 5",
    subtitle = "the relationship is curved, not straight"
  )

# coding practice:
# 1. run the two models below as written
# 2. change `fit_5_quadratic` so it matches `fit_5_linear`
# 3. rerun the table and the plot
# 4. then add the squared stress term back in

fit_5_linear <- lm(
  task_score ~ stress,
  data = df_curve
)

fit_5_quadratic <- lm(
  task_score ~ stress + I(stress^2),
  data = df_curve
)

parameters::model_parameters(fit_5_linear)
parameters::model_parameters(fit_5_quadratic)

comparison_5 <- tibble(
  model = c("linear", "quadratic"),
  r_squared = c(
    summary(fit_5_linear)$r.squared,
    summary(fit_5_quadratic)$r.squared
  )
) |>
  mutate(r_squared = round(r_squared, 2))

print(comparison_5)

pred_5 <- df_curve |>
  mutate(
    fitted_linear = predict(fit_5_linear),
    fitted_quadratic = predict(fit_5_quadratic)
  ) |>
  arrange(stress)

ggplot(pred_5, aes(x = stress, y = task_score)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = fitted_linear), colour = "grey40", linewidth = 1) +
  geom_line(aes(y = fitted_quadratic), colour = "firebrick", linewidth = 1) +
  labs(
    title = "exercise 5 fitted values",
    subtitle = "grey = linear, red = quadratic"
  )

# the quadratic model matches the data-generating process more closely


# --- helpers for the optional section ---------------------------------------
# you can ignore this part until you reach the final section
g_compute <- function(model, data, treatment_var, weights = NULL) {
  data_a0 <- data
  data_a1 <- data

  data_a0[[treatment_var]] <- 0
  data_a1[[treatment_var]] <- 1

  contrast <- predict(model, newdata = data_a1) - predict(model, newdata = data_a0)

  if (is.null(weights)) {
    return(mean(contrast))
  }

  weighted.mean(contrast, w = weights)
}

simulate_transport_data <- function(
  n_population = 5000,
  p_z_population = 0.6,
  p_selected_if_z0 = 0.8,
  p_selected_if_z1 = 0.35,
  beta_a = 1,
  beta_az = 2,
  noise_sd = 1,
  seed = 123
) {
  set.seed(seed)

  # `rbinom()` draws binary variables, so z, a, and selected take values 0 or 1
  z_population <- rbinom(n_population, 1, p_z_population)
  a_population <- rbinom(n_population, 1, 0.5)

  # the treatment effect is `beta_a` when z = 0 and `beta_a + beta_az` when z = 1
  # the final `rnorm()` term adds random error
  y_population <- 50 + beta_a * a_population + beta_az * a_population * z_population +
    rnorm(n_population, 0, noise_sd)

  # people with z = 1 are less likely to be selected into the analytic sample
  selection_probability <- ifelse(
    z_population == 1,
    p_selected_if_z1,
    p_selected_if_z0
  )

  selected <- rbinom(n_population, 1, selection_probability)

  population_data <- tibble(
    y = y_population,
    a = a_population,
    z = z_population,
    selected = selected
  )

  sample_data <- population_data |>
    filter(selected == 1) |>
    select(y, a, z)

  weights <- ifelse(sample_data$z == 1, 1 / p_selected_if_z1, 1 / p_selected_if_z0)
  weights <- weights / mean(weights)

  list(
    sample_data = tibble(
      y = sample_data$y,
      a = sample_data$a,
      z = sample_data$z,
      weights = weights
    ),
    population_data = population_data |> select(y, a, z),
    beta_a = beta_a,
    beta_az = beta_az,
    p_selected_if_z0 = p_selected_if_z0,
    p_selected_if_z1 = p_selected_if_z1
  )
}


# --- optional demonstration: selection bias with effect modification ---------
# this last section returns to a week 4 idea.
# the treatment effect depends on z, and selection into the sample changes how
# common z is. plain regression in the selected sample does not automatically
# recover the population average treatment effect.
data_transport <- simulate_transport_data(
  n_population = 5000,
  p_z_population = 0.6,
  p_selected_if_z0 = 0.8,
  p_selected_if_z1 = 0.35,
  beta_a = 1,
  beta_az = 2,
  noise_sd = 1,
  seed = 439
)

sample_data <- data_transport$sample_data
population_data <- data_transport$population_data

# first check how much the selected sample and target population differ on z
cat("selected sample z distribution:\n")
print(prop.table(table(sample_data$z)))

cat("\npopulation z distribution:\n")
print(prop.table(table(population_data$z)))

# fit three versions of the model
# 1. no interaction in the selected sample
# 2. interaction in the selected sample
# 3. interaction plus weights to target the population distribution of z
fit_sample_no_interaction <- lm(
  y ~ a + z,
  data = sample_data
)

fit_sample <- lm(
  y ~ a * z,
  data = sample_data
)

fit_weighted <- lm(
  y ~ a * z,
  data = sample_data,
  weights = sample_data$weights
)

fit_population <- lm(
  y ~ a * z,
  data = population_data
)

# compare the selected-sample coefficients
coefficient_comparison <- tibble(
  model = c(
    "selected sample: no interaction",
    "selected sample: interaction",
    "weighted selected sample: interaction",
    "population: interaction"
  ),
  a = c(
    coef(fit_sample_no_interaction)[["a"]],
    coef(fit_sample)[["a"]],
    coef(fit_weighted)[["a"]],
    coef(fit_population)[["a"]]
  ),
  `a:z` = c(
    NA_real_,
    coef(fit_sample)[["a:z"]],
    coef(fit_weighted)[["a:z"]],
    coef(fit_population)[["a:z"]]
  )
) |>
  mutate(
    a = round(a, 2),
    `a:z` = round(`a:z`, 2)
  )

print(coefficient_comparison)

# because the treatment effect depends on z, the average effect depends on
# how common z is in the group we average over
true_ate_sample <- with(
  sample_data,
  mean(data_transport$beta_a + data_transport$beta_az * z)
)

true_ate_population <- with(
  population_data,
  mean(data_transport$beta_a + data_transport$beta_az * z)
)

# manually standardise predicted outcomes under treatment and control
ate_sample_no_interaction <- g_compute(
  fit_sample_no_interaction,
  sample_data,
  treatment_var = "a"
)

ate_sample <- g_compute(fit_sample, sample_data, treatment_var = "a")

ate_weighted <- g_compute(
  fit_weighted,
  sample_data,
  treatment_var = "a",
  weights = sample_data$weights
)

ate_population <- g_compute(fit_population, population_data, treatment_var = "a")

# collect the true and estimated average treatment effects in one table
results_transport <- tibble(
  estimand = c(
    "true selected-sample ate",
    "true population ate",
    "selected sample: no interaction",
    "selected sample: interaction",
    "weighted selected sample: interaction",
    "population g-computation"
  ),
  estimate = c(
    true_ate_sample,
    true_ate_population,
    ate_sample_no_interaction,
    ate_sample,
    ate_weighted,
    ate_population
  )
) |>
  mutate(estimate = round(estimate, 2))

print(results_transport)

# read this table from top to bottom:
# 1. the selected sample has the wrong mix of z values
# 2. omitting the interaction hides that treatment effects differ by z
# 3. even with the interaction, the unweighted estimate still targets the
#    selected sample rather than the population
# 4. weights recover the population average treatment effect more closely
