# ============================================================================
# PSYC 434 -- Lab 10: End-to-End Research Report
# self-standing script -- run from top to bottom inside the unzipped
# research-report template directory
# ============================================================================

# learning aims for this lab:
#   1. assemble the course workflow into one report.
#   2. estimate four ATEs in one batch with margot::margot_causal_forest.
#   3. apply Bonferroni correction and report E-values for an outcome-wide
#      design.
#   4. fit policy trees and choose depth via a stated parsimony threshold.
#   5. apply margot_select_grf_policy_trees() as a transparent graphing
#      rule.
#   6. render manuscript.qmd to PDF.
#
# how this lab differs from earlier labs:
#   labs 5/6/8/9 taught the building blocks. lab 10 puts them together.
#   the assignment is outcome-wide on one of two exposures
#   (religious_service or volunteer_work). community_group is reserved
#   for the lab 9 worked example and must not be used here.
# ============================================================================

# --- packages ---------------------------------------------------------------

required_packages <- c(
  "ggplot2", "dplyr", "tibble", "tidyr", "ggdag", "grf",
  "knitr", "kableExtra", "rmarkdown",
  "margot", "causalworkshop"
)
missing <- required_packages[
  !vapply(required_packages, \(p) requireNamespace(p, quietly = TRUE), logical(1))
]

if (length(missing) > 0) {
  stop(
    "Missing package(s): ", paste(missing, collapse = ", "), "\n\n",
    "Run the setup block in Lab 10 first, restart R, then run this script ",
    "again. If GitHub package installation fails, use the course lab ",
    "machine. Errors mentioning make, gcc, g++, clang, or compilation ",
    "usually mean your system build tools are missing.",
    call. = FALSE
  )
}

if (packageVersion("margot") < "1.0.322") {
  stop("margot >= 1.0.322 is required. Run the setup block, restart R, then re-run.",
       call. = FALSE)
}
if (packageVersion("causalworkshop") < "0.6.0") {
  stop("causalworkshop >= 0.6.0 is required. Run the setup block, restart R, then re-run.",
       call. = FALSE)
}

suppressPackageStartupMessages({
  library(causalworkshop)
  library(margot)
  library(grf)
  library(ggdag)
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(knitr)
  library(kableExtra)
})

# --- step 1: load the report template's setup -------------------------------

# this script expects to be run from the unzipped research-report folder.
# setup.R is the single source of truth for study decisions and helpers.
if (!file.exists("setup.R")) {
  stop(
    "setup.R not found in the current working directory.\n",
    "Download and unzip the research-report template, then open the\n",
    "resulting folder as an RStudio project before running this script.",
    call. = FALSE
  )
}
source("setup.R")

cat("\n=== study decisions ===\n")
cat("exposure:                   ", name_exposure, "\n")
cat("outcomes (fixed):           ", paste(outcome_short_names, collapse = ", "), "\n")
cat("study_n:                    ", study_n, "\n")
cat("num_trees:                  ", num_trees, "\n")
cat("n_iterations_stability:     ", n_iterations_stability, "\n")
cat("alpha_family_wise:          ", alpha_family_wise, "\n")
cat("min_gain_for_depth_switch:  ", min_gain_for_depth_switch, "\n")
cat("policy_value_lower_threshold: ", policy_value_lower_threshold, "\n")
cat("treated_uplift_lower_threshold:", treated_uplift_lower_threshold, "\n")

# --- step 2: simulate the panel ---------------------------------------------

cat("\n=== simulating panel ===\n")
panel <- simulate_panel()
cat("rows: ", nrow(panel), "\n")
cat("exposure prevalence:", round(mean(panel$exposure_t1), 2), "\n")

# --- step 3: fit the four causal forests + policy-tree pipeline ------------

cat("\n=== fitting margot pipeline (cached re-runs reuse _cache/) ===\n")
t0 <- Sys.time()
fit <- run_fit_pipeline(panel)
cat("pipeline ready in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min\n")

# --- step 4: outcome-wide ATE table + plot + interpretation ----------------

cat("\n=== margot_plot: Bonferroni-adjusted ATEs with E-values ===\n")
ate <- ate_plot_objects(fit$models_binary)
print(ate$transformed_table)

cat("\n=== forest plot ===\n")
print(ate$plot)

cat("\n=== auto-generated interpretation ===\n")
cat(ate$interpretation, "\n")

# --- step 5: policy-tree summary --------------------------------------------

cat("\n=== policy-tree brief ===\n")
print(fit$wf$policy_brief_df)

cat("\n=== depth comparison ===\n")
print(fit$wf$best$depth_summary_df |>
        select(outcome_label, depth_selected, pv_depth1, pv_depth2,
               pv_gain, decision))

# --- step 6: graphing-rule selection ---------------------------------------

cat("\n=== graphing-rule selection ===\n")
print(fit$selection)

# --- step 7: print the policy trees that pass the rule ---------------------

cat("\n=== printing selected policy trees ===\n")
label_to_model <- setNames(
  fit$wf$best$depth_summary_df$model,
  fit$wf$best$depth_summary_df$outcome_label
)
graphed_labels <- fit$selection$Outcome[fit$selection$graph_policy_tree]
graphed <- unname(label_to_model[graphed_labels])
graphed <- intersect(graphed, names(fit$wf$plots))

if (length(graphed) == 0) {
  cat("no outcome passed the graphing rule.\n")
} else {
  for (mn in graphed) {
    cat("\n--- ", label_mapping[[mn]], " ---\n", sep = "")
    print(fit$wf$plots[[mn]]$combined_plot)
  }
}

# --- step 8: ground-truth audit (teaching scaffold) ------------------------

cat("\n=== ground-truth audit ===\n")
truth <- ground_truth_audit()
combined <- as.data.frame(fit$models_binary$combined_table, check.names = FALSE)
audit <- tibble(
  outcome_label = vapply(
    paste0("model_", rownames(combined)),
    function(id) {
      v <- label_mapping[[id]]
      if (is.null(v)) id else v
    },
    character(1)
  ),
  estimated_ate = combined[["E[Y(1)]-E[Y(0)]"]]
) |>
  left_join(truth, by = "outcome_label") |>
  mutate(diff = estimated_ate - true_mean_tau)
print(audit)

# --- step 9: render the manuscript ------------------------------------------

# the manuscript sources setup.R and re-fits the pipeline.
# uncomment the line below to render in-script, or run from the terminal:
#   quarto render manuscript.qmd
#
# quarto::quarto_render("manuscript.qmd")

cat("\nlab 10 script complete.\n")
cat("next: render manuscript.qmd from the terminal:\n")
cat("  quarto render manuscript.qmd\n")
