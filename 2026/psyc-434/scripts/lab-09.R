# ============================================================================
# PSYC 434 — Lab 9: Policy Trees (multi-outcome workflow with margot)
# self-standing script — run from top to bottom
# ============================================================================

# learning aims for this lab:
#   1. read an outcome-wide ATE forest plot with E-values, the lab's
#      day-to-day way of asking "what works, on average, across several
#      outcomes at once?"
#   2. test whether each outcome shows real heterogeneity, using the
#      grf::test_calibration omnibus test wrapped by margot.
#   3. read a depth-2 policy tree, the kind of allocation rule you can
#      defend in front of a non-technical audience.
#   4. compare per-outcome QINI curves and RATE-AUTOC tables, the
#      diagnostics that decide whether targeting is worth it at all.
#   5. read margot's auto-generated prose summary and learn to treat it
#      as a draft, not a final answer.
#
# how this lab differs from earlier labs:
#   labs 5-8 fit one causal forest at a time, by hand, on a single
#   outcome. real lab-bench work uses the lab's `margot` package to fit
#   a *batch* of forests at once, attach E-values for sensitivity
#   analysis, and produce decision-tree allocation rules with auto-
#   generated prose. this lab loads pre-fitted models from a cache
#   (~80 MB) so the workflow fits inside one session.
# ============================================================================

# --- packages ---------------------------------------------------------------

required_packages <- c("ggplot2", "dplyr", "tibble", "arrow", "qs2", "googledrive")
missing <- required_packages[
  !vapply(required_packages, \(p) requireNamespace(p, quietly = TRUE), logical(1))
]
if (length(missing) > 0) install.packages(missing)

if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")

if (!requireNamespace("margot", quietly = TRUE)) {
  pak::pak("go-bayes/margot")
}

# require causalworkshop >= 0.6.0 (provides load_lab_09_cache and the
# arrow-based artefact reader). a stale install will be auto-upgraded.
if (!requireNamespace("causalworkshop", quietly = TRUE) ||
  packageVersion("causalworkshop") < "0.6.0") {
  pak::pak("go-bayes/causalworkshop")
  if ("causalworkshop" %in% loadedNamespaces()) {
    stop(
      "causalworkshop was upgraded; please restart R and re-run this lab.",
      call. = FALSE
    )
  }
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
# the cache is a public Google Drive file. the loader (in causalworkshop)
# downloads it once into a per-user cache directory, then reads from
# disk on every subsequent run. no `scripts/` folder is needed; this
# works from any working directory.
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

# --- step 2: outcome-wide ATE forest plot -----------------------------------

# margot's combined_table holds, in one row per outcome:
#   the ATE on the risk-difference scale (E[Y(1)] - E[Y(0)]),
#   a 95% confidence interval,
#   the E-value (point) and E-value (bound) measuring how strong an
#     unmeasured confounder would need to be, on the relative-risk
#     scale, to explain the result away.
print(models_binary$combined_table)

# margot_plot() turns that table into a forest-style figure. the dashed
# line at zero is the null; estimates whose E-value bound is below 1.2
# are flagged as easily explained away by mild unmeasured confounding.
ate_plot <- margot_plot(
  models_binary$combined_table,
  type = "RD",
  order = "magnitude_desc",
  e_val_bound_threshold = 1.2,
  label_mapping = label_mapping,
  save_path = tempdir()
)
print(ate_plot$plot)

# the plot's $interpretation slot is a draft sentence that names the
# outcomes most worth treating as causal. read it as a draft, not a
# verdict.
cat("\nautomatic interpretation:\n", ate_plot$interpretation, "\n", sep = "")

# --- step 3: omnibus heterogeneity test -------------------------------------

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

# --- step 4: per-outcome policy trees ---------------------------------------

# a policy tree converts the personalised CATE into a transparent
# allocation rule: "treat people in this leaf, do not treat in this
# leaf". the lab caps depth at 2 (at most three yes/no questions per
# rule).
#
# margot_plot_decision_tree() returns the tree diagram alone;
# margot_plot_policy_tree() returns the prediction-points scatter.
# render both per outcome.
model_ids <- names(label_mapping)
for (m in model_ids) {
  cat("\n=== ", label_mapping[[m]], " ===\n", sep = "")
  print(margot_plot_decision_tree(
    policy_tree_stability,
    model_name = m,
    max_depth = 2,
    label_mapping = label_mapping
  ))
  print(margot_plot_policy_tree(
    policy_tree_stability,
    model_name = m,
    max_depth = 2,
    label_mapping = label_mapping
  ))
}

# --- step 5: QINI curves ---------------------------------------------------

# QINI curves answer: at a moderate, realistic budget (say 10-40% of
# the population treated), does targeting beat random allocation? the
# vertical lines mark the spend levels where standard policy questions
# are usually asked.
qini_plots <- margot_plot_qini_batch(
  models_binary,
  label_mapping = label_mapping,
  spend_levels = c(0.1, 0.4)
)
for (p in qini_plots) print(p)

# --- step 6: RATE table -----------------------------------------------------

# RATE summarises targeting value as a single number per outcome.
# AUTOC weights all budgets equally; QINI weights mid-range budgets
# more. negative or near-zero RATE means the ranking has little
# practical content. margot_plot_rate_batch() returns a list of
# per-outcome plots; the numbers themselves come from margot_rate().
rate_plots <- margot_plot_rate_batch(
  models_binary,
  target = "AUTOC",
  label_mapping = label_mapping
)
for (p in rate_plots) print(p)

rate_table <- margot::margot_rate(
  models_binary,
  target = "AUTOC",
  label_mapping = label_mapping
)
print(rate_table)

# --- step 7: read the auto-generated prose ----------------------------------

# margot synthesises a draft narrative from the policy-workflow object.
# read it critically. the prose is generated from the same numbers you
# saw above; it does not bring new information. its value is that it
# forces a structure (assumption -> finding -> caveat) that students
# can imitate when they write their own results.
cat("\n=========== auto-generated prose ===========\n\n")
cat(wf$report_prose)

# the policy_brief_df is a tabular one-pager: one row per outcome with
# the recommended depth, the wins/losses against random, and a short
# label.
print(wf$policy_brief_df)

# --- step 8: ground truth from the simulator -------------------------------

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
# data -> batch causal forest -> ATE forest plot with E-values
#      -> omnibus heterogeneity tests
#      -> depth-2 policy trees with auto-generated prose
#      -> QINI and RATE diagnostics
#      -> ground-truth audit.
#
# this is the same workflow used to write papers in the lab. the
# difference between today's lab and a manuscript is mostly volume: a
# real study runs the same calls on twenty outcomes, then writes the
# paper around the resulting figures and tables.
#
# week 10 returns to a different question entirely: are the things we
# *measured* the same things across the groups we are comparing? if
# they are not, all of the work above can be undermined by measurement
# non-invariance. lab 10 introduces classical and DAG-based measurement
# theory.
