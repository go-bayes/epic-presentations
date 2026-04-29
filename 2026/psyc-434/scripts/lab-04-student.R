# ============================================================================
# psyc 434 -- lab 4: student practice script
# self-standing script -- run from top to bottom
# ============================================================================

# --- packages ---------------------------------------------------------------
# install only the packages that are missing
required_packages <- c("tidyverse")

missing_packages <- required_packages[
  !vapply(required_packages, \(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(tidyverse)

# start here.
# work one exercise at a time.
# change only the model formula after `~`.
# then rerun the model and the lines just below it.
#
# this script has three core exercises.
# 1. one predictor:
#    fit `exam_score ~ study_hours`, then change it to `exam_score ~ 1`
#    and see what the fitted line becomes.
# 2. add a second predictor:
#    start with `exam_score ~ study_hours`, then add `motivation`
#    and see how the coefficient for `study_hours` changes.
# 3. add an interaction:
#    start with `exam_score ~ study_hours + workshop`, then change it to
#    `exam_score ~ study_hours * workshop` and compare the fitted lines.
#
# the aim is not to memorise syntax.
# the aim is to notice what each change in the formula does.
#
# questions to answer as you work:
# - in exercise 1, what happens to the fitted line when you change
#   `exam_score ~ study_hours` to `exam_score ~ 1`?
# - in exercise 2, does the coefficient for `study_hours` get larger or
#   smaller after adding `motivation`?
# - in exercise 3, what changes when you replace `+` with `*` in the formula?
# - which formula felt easiest to interpret, and which felt hardest?


# --- exercise 1: one predictor ----------------------------------------------
# `set.seed()` makes the random numbers reproducible
set.seed(434)

# `n` is the number of students we simulate
n <- 200

# `rnorm()` draws random values from a normal distribution
study_hours <- rnorm(n, mean = 10, sd = 2)

# exam score follows a linear relationship with study hours
# the final `rnorm()` term adds random error
exam_score <- 40 + 4 * study_hours + rnorm(n, mean = 0, sd = 5)

# `tibble()` makes a data frame
df_simple <- tibble(
  study_hours = study_hours,
  exam_score = exam_score
)

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
# 3. rerun the summary and the fitted-value plot
# 4. question: what happens to the fitted line?

# edit here
fit_1 <- lm(
  exam_score ~ study_hours,
  data = df_simple
)

summary(fit_1)

pred_1 <- df_simple |>
  mutate(fitted_value = predict(fit_1)) |>
  arrange(study_hours)

ggplot(pred_1, aes(x = study_hours, y = exam_score)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = fitted_value), colour = "steelblue", linewidth = 1) +
  labs(
    title = "exercise 1 fitted values",
    subtitle = "rerun this after you change the formula"
  )


# --- exercise 2: add a second predictor -------------------------------------
set.seed(435)
n <- 300

# motivation is another normally distributed variable
motivation <- rnorm(n, mean = 0, sd = 1)

# study hours depend partly on motivation
study_hours <- 8 + 1.5 * motivation + rnorm(n, mean = 0, sd = 1.5)

# exam score depends on both study hours and motivation
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
# 1. run the model below as written
# 2. look at the coefficient for `study_hours`
# 3. change the formula to `exam_score ~ study_hours + motivation`
# 4. rerun the summary and see how the coefficient changes
# 5. question: does the `study_hours` coefficient get larger or smaller?

# edit here
fit_2 <- lm(
  exam_score ~ study_hours,
  data = df_multiple
)

summary(fit_2)


# --- exercise 3: interaction ------------------------------------------------
set.seed(437)
n <- 320

# `runif()` draws values evenly between 0 and 10
study_hours <- runif(n, min = 0, max = 10)

# `rbinom(n, 1, 0.5)` draws 0 or 1
# here, 1 means attended the workshop
workshop <- rbinom(n, 1, 0.5)

# this outcome has an interaction
# the slope for study hours is steeper when workshop = 1
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
    title = "exercise 3",
    subtitle = "the lines are not parallel",
    colour = "workshop"
  )

# coding practice:
# 1. run the model below as written
# 2. rerun the summary and fitted-value plot
# 3. change the formula to `exam_score ~ study_hours * workshop`
# 4. rerun the same lines and compare the fitted lines
# 5. question: when you replace `+` with `*`, do the fitted lines stay parallel?

# edit here
fit_3 <- lm(
  exam_score ~ study_hours + workshop,
  data = df_interaction
)

summary(fit_3)

pred_3 <- df_interaction |>
  mutate(fitted_value = predict(fit_3)) |>
  arrange(workshop, study_hours)

ggplot(
  pred_3,
  aes(x = study_hours, y = exam_score, colour = factor(workshop))
) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = fitted_value), linewidth = 1) +
  labs(
    title = "exercise 3 fitted values",
    subtitle = "rerun this after you change the formula",
    colour = "workshop"
  )


# if you finish early, open `scripts/lab-04.R`
# that file has extra annotation, more exercises, and an optional extension
