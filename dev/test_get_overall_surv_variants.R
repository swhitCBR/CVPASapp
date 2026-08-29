#' Test script: structural consistency of the three get_overall_surv() dev variants
#'
#' Runs get_overall_surv_OLD(), get_overall_surv_logit(), and
#' get_overall_surv_logitnorm() side-by-side on the same input and asserts
#' that their `deriv_ests` and `bias_correction_df` outputs have consistent
#' structure. Run from the project root (working directory = package root).

library(testthat)

source("dev/get_overall_surv_OLD.R")
source("dev/get_overall_surv_logit.R")
source("dev/get_overall_surv_logitnorm.R")

# --- Shared test input: 4 scenarios spanning low/high variance ---
MU_mat <- matrix(c(
  0.8,  0.5,  0.3, -0.2,  0.1,  0.4,
  0.8,  0.5,  0.3, -0.2,  0.1,  0.4,
  1.5,  2.0, -1.0,  0.0, -0.5,  1.0,
  1.5,  2.0, -1.0,  0.0, -0.5,  1.0
), nrow = 4, byrow = TRUE)

SIGMA2_mat <- matrix(c(
  0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
  0.5,  0.5,  0.5,  0.5,  0.5,  0.5,
  0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
  0.5,  0.5,  0.5,  0.5,  0.5,  0.5
), nrow = 4, byrow = TRUE)

n_scenarios <- nrow(MU_mat)
n_vars      <- ncol(MU_mat)

variants <- list(
  OLD       = get_overall_surv_OLD(MU_mat, SIGMA2_mat),
  logit     = get_overall_surv_logit(MU_mat, SIGMA2_mat),
  logitnorm = get_overall_surv_logitnorm(MU_mat, SIGMA2_mat)
)

deriv_ests_expected_cols <- c(
  "S_TCJ_CHP_mean", "S_TCJ_CHP_variance", "S_TCJ_CHP_sd",
  "S_TCJ_CHP_LCL", "S_TCJ_CHP_UCL",
  "S_HOR_CHP_mean", "S_HOR_CHP_variance", "S_HOR_CHP_sd",
  "S_HOR_CHP_LCL", "S_HOR_CHP_UCL"
)

# Columns common to all three bias_correction_df variants. get_overall_surv_OLD
# and get_overall_surv_logit additionally report f_mu/f_sec (Taylor-series
# intermediates), which get_overall_surv_logitnorm doesn't have since it uses
# exact logitnorm moments instead of a Taylor approximation.
bias_correction_common_cols <- c(
  "scenario", "variable", "mu", "sigma2",
  "E_prop_uncorrected", "correction_term"
)

test_that("each variant returns a list with deriv_ests and bias_correction_df", {
  for (nm in names(variants)) {
    out <- variants[[nm]]
    expect_type(out, "list")
    expect_named(out, c("deriv_ests", "bias_correction_df"), info = nm)
  }
})

test_that("deriv_ests has identical dimensions and column names across variants", {
  for (nm in names(variants)) {
    de <- variants[[nm]]$deriv_ests
    expect_s3_class(de, "data.frame")
    expect_equal(dim(de), c(n_scenarios, length(deriv_ests_expected_cols)), info = nm)
    expect_equal(names(de), deriv_ests_expected_cols, info = nm)
    expect_true(all(vapply(de, is.numeric, logical(1))), info = nm)
  }
})

test_that("bias_correction_df has consistent core structure across variants", {
  for (nm in names(variants)) {
    bc <- variants[[nm]]$bias_correction_df
    expect_s3_class(bc, "data.frame")
    expect_equal(nrow(bc), n_scenarios * n_vars, info = nm)
    expect_true(all(bias_correction_common_cols %in% names(bc)), info = nm)
  }
})

test_that("OLD and logit variants agree exactly on the Taylor-series bias correction", {
  # Both use the identical Component 1 formula, so their bias_correction_df
  # values (not just structure) should match exactly.
  bc_OLD   <- variants$OLD$bias_correction_df
  bc_logit <- variants$logit$bias_correction_df
  expect_equal(bc_OLD[bias_correction_common_cols], bc_logit[bias_correction_common_cols])
  expect_equal(bc_OLD$f_mu,  bc_logit$f_mu)
  expect_equal(bc_OLD$f_sec, bc_logit$f_sec)
})

test_that("scenario/variable labeling is consistent across all variants", {
  expected_scenarios <- paste0("Scenario_", seq_len(n_scenarios))
  for (nm in names(variants)) {
    bc <- variants[[nm]]$bias_correction_df
    expect_setequal(unique(bc$scenario), expected_scenarios)
    expect_equal(length(unique(bc$variable)), n_vars, info = nm)
  }
})

cat("All get_overall_surv variant consistency checks passed.\n")
