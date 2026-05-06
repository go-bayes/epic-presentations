# ============================================================================
# PSYC 434 — Lab 9: Policy Trees (multi-outcome workflow with margot)
# self-standing script — run from top to bottom
# ============================================================================

# learning aims for this lab:
#   1. move from a CATE ranking to an explicit policy rule.
#   2. read depth-1 and depth-2 policy trees.
#   3. compare tree depth using policy value and interpretability.
#   4. check policy coverage: what share of people would the rule treat?
#   5. translate one tree into plain language and state its limitations.
#
# how this lab differs from earlier labs:
#   earlier labs taught the pieces separately: average effects,
#   conditional average effects, causal forests, RATE, and QINI. this
#   lab uses a cached `margot` teaching workflow to put those pieces
#   together and focus on decision-tree allocation rules. use the course
#   workflow as normative. manuscript workflows are more complex because
#   they answer different questions, use real data, and add extra
#   diagnostics. this lab loads pre-fitted models from a cache (~80 MB)
#   so the workflow fits inside one session.
# ============================================================================

# --- packages ---------------------------------------------------------------

# This lab uses pre-fitted models. Do not build GitHub packages during
# class: that is slow and fragile on student laptops. Run the setup block
# in the lab notes before class, then restart R and run this script.
required_packages <- c(
  "ggplot2", "dplyr", "tibble", "arrow", "qs2", "googledrive",
  "margot", "causalworkshop"
)
missing <- required_packages[
  !vapply(required_packages, \(p) requireNamespace(p, quietly = TRUE), logical(1))
]

if (length(missing) > 0) {
  stop(
    "Missing package(s): ", paste(missing, collapse = ", "), "\n\n",
    "Run the setup block in Lab 9 first, restart R, then run this script again.\n",
    "If GitHub package installation fails, use the course lab machine or ask for ",
    "the pre-installed lab environment. Errors mentioning make, gcc, g++, clang, ",
    "or compilation usually mean your system build tools are missing.",
    call. = FALSE
  )
}

if (packageVersion("causalworkshop") < "0.6.0") {
  stop(
    "causalworkshop >= 0.6.0 is required. Run the setup block in Lab 9, ",
    "restart R, then run this script again.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(causalworkshop)
  library(margot)
  library(ggplot2)
  library(dplyr)
})

# --- step 1: load the cached fits ------------------------------------------

# the cache holds three pre-built artefacts:
#   models_binary         — the batch causal forest (one per outcome) with
#                           AIPW scores, OOB predictions, and the combined
#                           ATE table.
#   policy_tree_stability — bootstrap-based stability output for each
#                           outcome's depth-1 and depth-2 policy trees.
#   policy_workflow       — interpretive layer: depth recommendations,
#                           plots, and auto-generated prose summaries.
#
# what was run to make the cache (shown for transparency; do not run in lab):
# this is a teaching cache, not a reproduction of the employer-gratitude
# manuscript workflow.
#
# d <- causalworkshop::simulate_nzavs_data(n = 5000, seed = 2026)
# d0 <- d |> dplyr::filter(wave == 0)
# d1 <- d |> dplyr::filter(wave == 1)
# d2 <- d |> dplyr::filter(wave == 2)
#
# covariate_cols <- c(
#   "age", "male", "nz_european", "education", "partner", "employed",
#   "log_income", "nz_dep", "agreeableness", "conscientiousness",
#   "extraversion", "neuroticism", "openness",
#   "community_group", "purpose"
# )
#
# df_grf <- dplyr::bind_cols(
#   d0 |> dplyr::select(dplyr::all_of(covariate_cols)),
#   tibble::tibble(
#     community_group_t1 = d1$community_group,
#     t2_purpose = d2$purpose,
#     t2_belonging = d2$belonging,
#     t2_self_esteem = d2$self_esteem,
#     t2_life_satisfaction = d2$life_satisfaction
#   )
# )
#
# X <- as.matrix(df_grf[, covariate_cols])
# W <- df_grf$community_group_t1
# weights <- rep(1, nrow(df_grf))
# outcome_vars <- c(
#   "t2_purpose", "t2_belonging", "t2_self_esteem", "t2_life_satisfaction"
# )
# label_mapping <- list(
#   model_t2_purpose = "Sense of Purpose",
#   model_t2_belonging = "Belonging",
#   model_t2_self_esteem = "Self-esteem",
#   model_t2_life_satisfaction = "Life satisfaction"
# )
# grf_defaults <- list(num.trees = 1000, honesty = TRUE, tune.parameters = "all")
#
# models_binary <- margot::margot_causal_forest(
#   data = df_grf,
#   outcome_vars = outcome_vars,
#   covariates = X,
#   W = W,
#   weights = weights,
#   grf_defaults = grf_defaults,
#   top_n_vars = 12,
#   save_models = TRUE,
#   save_data = TRUE,
#   compute_conditional_means = TRUE,
#   train_proportion = 0.5,
#   use_train_test_split = TRUE,
#   seed = 2026
# )
#
# policy_tree_stability <- margot::margot_policy_tree_stability(
#   model_results = models_binary,
#   depth = 2,
#   n_iterations = 100,
#   vary_type = "split_only",
#   parallel = FALSE,
#   label_mapping = label_mapping,
#   seed = 2026
# )
#
# wf <- margot::margot_policy_workflow(
#   stability = policy_tree_stability,
#   original_df = df_grf,
#   label_mapping = label_mapping,
#   audience = "policy",
#   interpret_models = "recommended",
#   plot_models = "recommended"
# )
#
# load the pre-fitted results. the first run may take a moment while the
# cache downloads; later runs should be faster.
cache <- causalworkshop::load_lab_09_cache()

models_binary <- cache$models_binary
policy_tree_stability <- cache$policy_tree_stability
wf <- cache$policy_workflow

# the four outcomes are purpose, belonging, self-esteem, and life
# satisfaction at wave 2. the exposure is community-group participation
# at wave 1. labels are passed to plot/table calls below so figures are
# legible.
label_mapping <- list(
  model_t2_purpose = "Sense of Purpose",
  model_t2_belonging = "Belonging",
  model_t2_self_esteem = "Self-esteem",
  model_t2_life_satisfaction = "Life satisfaction"
)

# --- step 2: quick evidence check -------------------------------------------

# margot's combined_table holds, in one row per outcome:
#   the ATE on the risk-difference scale (E[Y(1)] - E[Y(0)]),
#   a 95% confidence interval,
#   the E-value (point) and E-value (bound) measuring how strong an
#     unmeasured confounder would need to be, on the relative-risk
#     scale, to explain the result away.
print(models_binary$combined_table)

# This is a quick check only. Lab 8 already covered ranking, RATE, and
# QINI. Today we use the fitted models to read policy trees.
ate_plot <- margot_plot(
  models_binary$combined_table,
  type = "RD",
  order = "magnitude_desc",
  e_val_bound_threshold = 1.2,
  label_mapping = label_mapping,
  save_path = tempdir()
)
print(ate_plot$plot)

# --- step 3: quick heterogeneity check ---------------------------------------

# the ATE asks "does it work on average?". the next question is "does
# it work the same for everyone?". margot_omnibus_hetero_test() wraps
# grf::test_calibration. the row labelled "Differential prediction" is
# what matters: a positive coefficient with a small p-value means the
# forest sees genuine heterogeneity, not just noise.
omnibus <- margot_omnibus_hetero_test(
  models_binary,
  label_mapping = label_mapping
)
print(omnibus)

# --- step 4: policy tree summary tables --------------------------------------

# This table is the first policy-tree result to read. Coverage is the
# share of participants the learned rule recommends for treatment. Notice
# that coverage is an output of the tree, not a fixed budget set by the
# analyst.
cat("\n=== policy-tree one-page summary ===\n")
print(wf$policy_brief_df)

# Compare depth-1 and depth-2. A deeper tree is only useful if the gain
# is worth the extra complexity.
cat("\n=== depth comparison ===\n")
print(wf$best$depth_summary_df)

# --- step 5: choose the parsimony threshold ----------------------------------

# Depth selection is not automatic. Investigators must state how much
# added policy value is needed before a more complex depth-2 tree is
# worth using. The cached workflow used a permissive default threshold.
# Try stricter thresholds and watch the selected depths change.
depth_thresholds <- c(0.005, 0.01, 0.03)
depth_sensitivity <- dplyr::bind_rows(lapply(depth_thresholds, \(threshold) {
  best_at_threshold <- suppressMessages(suppressWarnings(
    margot::margot_policy_summary_compare_depths(
      policy_tree_stability,
      label_mapping = label_mapping,
      min_gain_for_depth_switch = threshold,
      verbose = FALSE
    )
  ))
  best_at_threshold$depth_summary_df |>
    dplyr::transmute(
      threshold = threshold,
      outcome = outcome_label,
      selected_depth = depth_selected,
      pv_depth1 = pv_depth1,
      pv_depth2 = pv_depth2,
      gain_depth2_minus_depth1 = pv_depth2 - pv_depth1
    )
}))

cat("\n=== depth selection sensitivity ===\n")
print(depth_sensitivity)

# --- step 6: render and save policy trees ------------------------------------

# a policy tree converts the personalised CATE into a transparent
# allocation rule: "treat people in this leaf, do not treat in this
# leaf". the lab caps depth at 2 (at most three yes/no questions per
# rule).
#
# margot_plot_decision_tree() returns the tree diagram alone;
# margot_plot_policy_tree() returns the prediction-points scatter.
# render both per outcome and save them so you can inspect them outside
# the RStudio plot pane.
model_ids <- names(label_mapping)

plot_dir <- file.path(getwd(), "lab-09-policy-tree-plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

policy_tree_plots <- list()
for (m in model_ids) {
  cat("\n=== ", label_mapping[[m]], " ===\n", sep = "")
  depth1_tree <- margot_plot_decision_tree(
    policy_tree_stability,
    model_name = m,
    max_depth = 1,
    label_mapping = label_mapping
  )
  depth2_tree <- margot_plot_decision_tree(
    policy_tree_stability,
    model_name = m,
    max_depth = 2,
    label_mapping = label_mapping
  )
  depth2_scatter <- margot_plot_policy_tree(
    policy_tree_stability,
    model_name = m,
    max_depth = 2,
    label_mapping = label_mapping
  )

  suppressWarnings(print(depth1_tree))
  suppressWarnings(print(depth2_tree))
  suppressWarnings(print(depth2_scatter))

  suppressWarnings(ggplot2::ggsave(
    filename = file.path(plot_dir, paste0(m, "-depth1-tree.png")),
    plot = depth1_tree,
    width = 8,
    height = 5,
    dpi = 150
  ))
  suppressWarnings(ggplot2::ggsave(
    filename = file.path(plot_dir, paste0(m, "-depth2-tree.png")),
    plot = depth2_tree,
    width = 8,
    height = 5,
    dpi = 150
  ))
  suppressWarnings(ggplot2::ggsave(
    filename = file.path(plot_dir, paste0(m, "-depth2-scatter.png")),
    plot = depth2_scatter,
    width = 8,
    height = 5,
    dpi = 150
  ))

  policy_tree_plots[[m]] <- list(
    depth1_tree = depth1_tree,
    depth2_tree = depth2_tree,
    depth2_scatter = depth2_scatter
  )
}

cat("\nPolicy-tree plots saved to:\n  ", plot_dir, "\n", sep = "")
saved_policy_plots <- list.files(plot_dir, pattern = "\\.png$", full.names = TRUE)
cat("\nOpen and inspect every saved graph:\n")
print(saved_policy_plots)

# --- step 7: translate one policy tree ---------------------------------------

# Use the Purpose tree for the worked example. Open the saved depth-2
# tree and write the rule as ordinary language. Then compare the rule's
# coverage with the policy brief above.
worked_model <- "model_t2_purpose"
cat("\n=== worked tree for plain-language translation ===\n")
cat("Outcome: ", label_mapping[[worked_model]], "\n", sep = "")
cat("Open this file:\n  ",
  file.path(plot_dir, paste0(worked_model, "-depth2-tree.png")),
  "\n",
  sep = ""
)
cat(
  "Write the rule as: if [condition], recommend treatment; otherwise ...\n",
  "Then state one limitation of using this rule for real allocation.\n",
  sep = ""
)

# --- step 8: read the auto-generated prose ----------------------------------

# margot synthesises a draft narrative from the policy-workflow object.
# read it critically. the prose is generated from the same numbers you
# saw above; it does not bring new information. its value is that it
# forces a structure (assumption -> finding -> caveat) that students
# can imitate when they write their own results.
cat("\n=========== auto-generated prose ===========\n\n")
cat(wf$report_prose)

# --- step 9: ground truth from the simulator -------------------------------

# the analyses above never see the truth. the simulator stores the
# *true* individual treatment effects in tau_community_<outcome>
# columns. ranking outcomes by their true population mean tau lets us
# audit margot's recommendations without circularity.
d <- causalworkshop::simulate_nzavs_data(n = 5000, seed = 2026)
d0 <- d |> filter(wave == 0)

true_tau_table <- tibble::tibble(
  outcome = c("Sense of Purpose", "Belonging", "Self-esteem", "Life satisfaction"),
  true_mean_tau = c(
    mean(d0$tau_community_purpose),
    mean(d0$tau_community_belonging),
    mean(d0$tau_community_self_esteem),
    mean(d0$tau_community_life_satisfaction)
  ),
  true_sd_tau = c(
    sd(d0$tau_community_purpose),
    sd(d0$tau_community_belonging),
    sd(d0$tau_community_self_esteem),
    sd(d0$tau_community_life_satisfaction)
  )
) |>
  arrange(desc(true_mean_tau))

cat("\n=== ground truth, ranked by true population ATE ===\n")
print(true_tau_table)

# the spread (true_sd_tau) is the headroom for targeting: it tells you
# whether even a perfect ranking would deliver appreciable extra
# benefit. compare the ranking from this table with the order in
# margot_plot()'s ATE forest above. discrepancies between estimated and
# true rank are an honest measure of how much information the forest
# extracted from a finite sample.

# --- where this leads -------------------------------------------------------
#
# you have just walked the full lab pipeline, end to end:
# data -> batch causal forest
#      -> quick ATE and heterogeneity checks
#      -> policy-value and coverage summary
#      -> depth selection under a stated parsimony threshold
#      -> depth-1 and depth-2 policy trees
#      -> plain-language rule translation
#      -> ground-truth audit.
#
# this is a teaching analogue of the lab workflow. a manuscript adds
# more outcomes, stronger diagnostics, sensitivity checks, and more
# careful prose.
#
# week 10 returns to a different question entirely: are the things we
# *measured* the same things across the groups we are comparing? if
# they are not, all of the work above can be undermined by measurement
# non-invariance. lab 10 introduces classical and DAG-based measurement
# theory.
