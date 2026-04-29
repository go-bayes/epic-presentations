# ============================================================================
# PSYC 434 -- Lab 3: Regression and Bias Mechanisms
# self-standing script -- run from top to bottom
# ============================================================================

# --- packages ---------------------------------------------------------------

# install only the packages that are missing
required_packages <- c("tidyverse", "parameters", "report", "ggeffects", "ggdag")
missing_packages <- required_packages[
  !vapply(required_packages, \(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)
library(parameters)
library(report)
library(ggeffects)
library(ggdag)

# --- regression refresher ---------------------------------------------------

# start with a simple linear relationship
set.seed(123)
n <- 200
study_hours <- rnorm(n, mean = 10, sd = 2)
exam_score <- 50 + 3 * study_hours + rnorm(n, mean = 0, sd = 6)

df_regression <- tibble(
  study_hours = study_hours,
  exam_score = exam_score
)

fit_regression <- lm(exam_score ~ study_hours, data = df_regression)
summary(fit_regression)
report::report(fit_regression)

# plot the fitted line and raw data
predicted_values <- ggeffects::ggpredict(fit_regression, terms = "study_hours")
plot(predicted_values, dot_alpha = 0.25, show_data = TRUE, jitter = 0.05)

# --- omitted variable bias --------------------------------------------------

# a common cause opens a backdoor path between treatment and outcome
dag_fork <- dagify(
  y ~ l,
  a ~ l,
  exposure = "a",
  outcome = "y"
) |>
  tidy_dagitty(layout = "tree")

ggdag(dag_fork) +
  theme_dag_blank() +
  labs(title = "omitted variable bias", subtitle = "l is a common cause of a and y")

ggdag_adjustment_set(dag_fork) +
  theme_dag_blank() +
  labs(title = "adjustment set", subtitle = "conditioning on l closes the backdoor path")

# here a has no causal effect on y
set.seed(434)
n <- 2000
l <- rnorm(n)
a <- 0.9 * l + rnorm(n)
y <- 1.2 * l + rnorm(n)

df_fork <- tibble(y = y, a = a, l = l)

fit_fork_naive <- lm(y ~ a, data = df_fork)
fit_fork_adjusted <- lm(y ~ a + l, data = df_fork)

parameters::model_parameters(fit_fork_naive)
parameters::model_parameters(fit_fork_adjusted)

fork_results <- tibble(
  model = c("naive", "adjusted for l"),
  estimate = c(
    coef(fit_fork_naive)[["a"]],
    coef(fit_fork_adjusted)[["a"]]
  )
) |>
  mutate(estimate = round(estimate, 3))

print(fork_results)

# --- mediator bias ----------------------------------------------------------

# if we want the total effect, controlling for the mediator blocks part of it
dag_pipe <- dagify(
  m ~ a,
  y ~ m,
  exposure = "a",
  outcome = "y"
) |>
  tidy_dagitty(layout = "tree")

ggdag(dag_pipe) +
  theme_dag_blank() +
  labs(title = "mediator bias", subtitle = "conditioning on m blocks the path from a to y")

# this graph has no adjustment set for the total effect
ggdag_adjustment_set(dag_pipe) +
  theme_dag_blank() +
  labs(title = "adjustment set", subtitle = "do not control for the mediator when estimating the total effect")

set.seed(435)
n <- 2000
a <- rbinom(n, 1, 0.5)
m <- 1.5 * a + rnorm(n)
y <- 2 * m + rnorm(n)

df_pipe <- tibble(y = y, a = a, m = m)

fit_pipe_total <- lm(y ~ a, data = df_pipe)
fit_pipe_overcontrolled <- lm(y ~ a + m, data = df_pipe)

parameters::model_parameters(fit_pipe_total)
parameters::model_parameters(fit_pipe_overcontrolled)

pipe_results <- tibble(
  model = c("total effect", "overcontrolled"),
  estimate = c(
    coef(fit_pipe_total)[["a"]],
    coef(fit_pipe_overcontrolled)[["a"]]
  )
) |>
  mutate(estimate = round(estimate, 3))

print(pipe_results)

# --- collider bias ----------------------------------------------------------

# conditioning on a collider opens a path that was closed before
dag_collider <- dagify(
  c_var ~ a + y,
  exposure = "a",
  outcome = "y"
) |>
  tidy_dagitty(layout = "tree")

ggdag(dag_collider) +
  theme_dag_blank() +
  labs(title = "collider bias", subtitle = "conditioning on c_var creates a spurious association")

ggdag_dseparated(
  dag_collider,
  from = "a",
  to = "y",
  controlling_for = "c_var"
) +
  theme_dag_blank() +
  labs(title = "conditioning on the collider", subtitle = "the path opens once we adjust for c_var")

# here a and y are independent before conditioning
set.seed(436)
n <- 2000
a <- rnorm(n)
y <- rnorm(n)
c_var <- a + y + rnorm(n)

df_collider <- tibble(y = y, a = a, c_var = c_var)

fit_collider_unadjusted <- lm(y ~ a, data = df_collider)
fit_collider_adjusted <- lm(y ~ a + c_var, data = df_collider)

parameters::model_parameters(fit_collider_unadjusted)
parameters::model_parameters(fit_collider_adjusted)

collider_results <- tibble(
  model = c("do not condition", "condition on collider"),
  estimate = c(
    coef(fit_collider_unadjusted)[["a"]],
    coef(fit_collider_adjusted)[["a"]]
  )
) |>
  mutate(estimate = round(estimate, 3))

print(collider_results)

# --- comparison -------------------------------------------------------------

# compare the treatment coefficient across the three bias mechanisms
results <- tibble(
  scenario = c(
    "omitted variable bias",
    "omitted variable bias",
    "mediator bias",
    "mediator bias",
    "collider bias",
    "collider bias"
  ),
  model = c(
    "naive",
    "adjusted for l",
    "total effect",
    "overcontrolled",
    "do not condition",
    "condition on collider"
  ),
  estimate = c(
    coef(fit_fork_naive)[["a"]],
    coef(fit_fork_adjusted)[["a"]],
    coef(fit_pipe_total)[["a"]],
    coef(fit_pipe_overcontrolled)[["a"]],
    coef(fit_collider_unadjusted)[["a"]],
    coef(fit_collider_adjusted)[["a"]]
  )
) |>
  mutate(estimate = round(estimate, 3))

print(results)

# the main lesson is simple:
# control for common causes
# do not control for mediators if you want the total effect
# do not control for colliders
