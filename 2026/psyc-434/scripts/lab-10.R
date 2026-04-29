# ============================================================================
# PSYC 434 — Lab 10: Measurement Invariance
# self-standing script — run from top to bottom
# ============================================================================

# --- packages ---------------------------------------------------------------

required_packages <- c("psych", "lavaan", "tidyverse")
missing_packages <- required_packages[
  !vapply(required_packages, \(pkg) requireNamespace(pkg, quietly = TRUE), logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

required_causalworkshop_exports <- c("simulate_measurement_items")

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
library(psych)
library(lavaan)
library(tidyverse)

# work around environments where detectCores() returns NA and lavaan errors
if (is.na(suppressWarnings(parallel::detectCores()))) {
  parallel_ns <- asNamespace("parallel")
  unlockBinding("detectCores", parallel_ns)
  assign(
    "detectCores",
    function(all.tests = FALSE, logical = TRUE) 1L,
    envir = parallel_ns
  )
  lockBinding("detectCores", parallel_ns)
}

# --- generate data ----------------------------------------------------------

d <- simulate_measurement_items(n = 2000, seed = 2026)
dim(d)
names(d)

# check true factor loadings and intercepts
attr(d, "true_loadings")
attr(d, "true_intercepts_group0")
attr(d, "true_intercepts_group1")

# --- exploratory factor analysis (EFA) --------------------------------------

# select items
items <- d |> select(item_1:item_6)

# factorability
psych::KMO(items)
psych::cortest.bartlett(cor(items), n = nrow(items))

# one-factor solution
fa_1 <- psych::fa(items, nfactors = 1, fm = "ml", rotate = "none")
print(fa_1$loadings, cutoff = 0.3)

# two-factor solution (for comparison)
fa_2 <- psych::fa(items, nfactors = 2, fm = "ml", rotate = "oblimin")
print(fa_2$loadings, cutoff = 0.3)

# --- confirmatory factor analysis (CFA) ------------------------------------

# specify one-factor model
model <- "
  distress =~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6
"

# fit CFA on full sample
fit_cfa <- cfa(model, data = d)
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

# extract key fit indices
fit_indices <- fitmeasures(fit_cfa, c("cfi", "rmsea", "srmr"))
print(round(fit_indices, 3))

# --- multigroup CFA: invariance testing -------------------------------------

# step 1: configural invariance
fit_configural <- cfa(model, data = d, group = "group")
summary(fit_configural, fit.measures = TRUE)

# step 2: metric invariance (equal loadings)
fit_metric <- cfa(model, data = d, group = "group",
                  group.equal = "loadings")
summary(fit_metric, fit.measures = TRUE)

# compare configural vs metric
lavTestLRT(fit_configural, fit_metric)

# step 3: scalar invariance (equal loadings + intercepts)
fit_scalar <- cfa(model, data = d, group = "group",
                  group.equal = c("loadings", "intercepts"))
summary(fit_scalar, fit.measures = TRUE)

# compare metric vs scalar
lavTestLRT(fit_metric, fit_scalar)

# --- partial scalar invariance ----------------------------------------------

# free intercepts for items 3 and 5
model_partial <- "
  distress =~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6
  item_3 ~ c(i3a, i3b) * 1
  item_5 ~ c(i5a, i5b) * 1
"

fit_partial <- cfa(model_partial, data = d, group = "group",
                   group.equal = c("loadings", "intercepts"))
summary(fit_partial, fit.measures = TRUE)

# compare partial scalar vs metric
lavTestLRT(fit_metric, fit_partial)

# changes in fit indices across nested models
delta_fit <- tibble(
  comparison = c("Metric - Configural", "Scalar - Metric", "Partial Scalar - Metric"),
  delta_cfi = c(
    fitmeasures(fit_metric, "cfi") - fitmeasures(fit_configural, "cfi"),
    fitmeasures(fit_scalar, "cfi") - fitmeasures(fit_metric, "cfi"),
    fitmeasures(fit_partial, "cfi") - fitmeasures(fit_metric, "cfi")
  ),
  delta_rmsea = c(
    fitmeasures(fit_metric, "rmsea") - fitmeasures(fit_configural, "rmsea"),
    fitmeasures(fit_scalar, "rmsea") - fitmeasures(fit_metric, "rmsea"),
    fitmeasures(fit_partial, "rmsea") - fitmeasures(fit_metric, "rmsea")
  ),
  delta_srmr = c(
    fitmeasures(fit_metric, "srmr") - fitmeasures(fit_configural, "srmr"),
    fitmeasures(fit_scalar, "srmr") - fitmeasures(fit_metric, "srmr"),
    fitmeasures(fit_partial, "srmr") - fitmeasures(fit_metric, "srmr")
  )
) |>
  mutate(across(starts_with("delta_"), \(x) round(x, 3)))

print(delta_fit)

# --- compare all models -----------------------------------------------------

models <- list(
  Configural = fit_configural,
  Metric = fit_metric,
  Scalar = fit_scalar,
  "Partial Scalar" = fit_partial
)

fit_table <- map_dfr(names(models), function(name) {
  fm <- fitmeasures(models[[name]], c("cfi", "rmsea", "srmr", "chisq", "df"))
  tibble(
    model = name,
    cfi = round(fm["cfi"], 3),
    rmsea = round(fm["rmsea"], 3),
    srmr = round(fm["srmr"], 3),
    chisq = round(fm["chisq"], 1),
    df = fm["df"]
  )
})

print(fit_table)

# ============================================================================
# Part B: Quarto research-report walkthrough
# ----------------------------------------------------------------------------
# the rest of this script demonstrates the manuscript scaffold students
# copy for the research report (Option A). it mirrors the lab's actual
# workflow: a setup.R script that holds every constant, helper, and
# label, plus a manuscript.qmd that sources setup.R and renders end-to-
# end. change the exposure or outcome in setup.R and every value in the
# manuscript updates on re-render.
# ============================================================================

# the scaffold lives at the repo root, not inside src/. find it from any
# working directory by walking up to the 26-434 root.
template_dir <- "/Users/joseph/GIT/26-434/research-report-template"

# 1. orient: list the scaffold files
list.files(template_dir)

# 2. read setup.R end to end so students see the six categories:
#    packages -> study constants -> labels -> palette -> data wrapper -> helpers
file.show(file.path(template_dir, "setup.R"))

# 3. source setup.R and exercise the helpers without rendering. this lets
#    students see the same objects the manuscript chunks consume.
source(file.path(template_dir, "setup.R"))
panel <- simulate_panel()
print(descriptives_table(panel))
cat(describe_exposure_text(), "\n")

# 4. render the manuscript once. students should run this from the
#    research-report-template directory in their own copy.
if (requireNamespace("quarto", quietly = TRUE)) {
  quarto::quarto_render(
    file.path(template_dir, "manuscript.qmd"),
    output_format = "html"
  )
} else {
  message(
    "install the quarto R package, or run from the shell:\n",
    "  cd ", template_dir, " && quarto render manuscript.qmd"
  )
}

# 5. swap exercise: edit setup.R so name_exposure becomes "volunteer_work"
#    and re-render. every inline value, table, and figure updates because
#    nothing in manuscript.qmd hard-codes the exposure name.
