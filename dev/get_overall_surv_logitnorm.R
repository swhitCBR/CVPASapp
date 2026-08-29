#' Analytical Logit-Normal Network Approximation for Matrices (logitnorm Package Moments)
#'
#' This is a variant of \code{get_overall_surv_logit()} that is identical in every
#' respect except for Component 1: instead of approximating the proportion-scale
#' mean and variance of each logit-normal input via a second-order Taylor
#' (delta-method) expansion, it computes the exact moments using
#' \code{logitnorm::momentsLogitnorm()}, which evaluates them by numerical
#' integration over the true logit-normal density (Frederic & Lad 2008;
#' Wutzler 2012).
#'
#' Components 2-4 (sub-metric allocation, top-level propagation, and the
#' logit-scale delta-method confidence interval) are unchanged from
#' \code{get_overall_surv_logit()}.
#'
#' Requires the \code{logitnorm} package.
#'
#' @param MU_mat A matrix where each row is a scenario and the 6 columns are logit-scale means in order:
#'        [,1] Psi_SLJ, [,2] theta_HOR-TCJ|SLJ, [,3] Psi_MAC, [,4] theta_TCJ-CHP|MAC, [,5] theta_TCJ-CHP|TRN, [,6] theta_HOR-CHP|ORE
#' @param SIGMA2_mat A matrix of the same dimensions containing logit-scale variances.
#' @param conf_level A numeric value between 0 and 1 indicating the target confidence interval width (default = 0.95).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{deriv_ests}{A data frame containing multi-layer analytical results for each scenario row.}
#'     \item{bias_correction_df}{A long-format data frame (one row per scenario x input variable)
#'       reporting the implied mean-bias correction: the naive plug-in value \code{plogis(mu)},
#'       the exact logit-normal mean from \code{logitnorm::momentsLogitnorm()}, and the difference
#'       between them (analogous to the \code{(sigma2/2) * f''(mu)} correction term used in
#'       \code{get_overall_surv_logit()}'s Taylor-series approximation, but computed exactly
#'       rather than approximated).}
#'   }
get_overall_surv_logitnorm <- function(MU_mat, SIGMA2_mat, conf_level = 0.95) {

  if (!requireNamespace("logitnorm", quietly = TRUE)) {
    stop("The 'logitnorm' package is required for get_overall_surv_logitnorm(). Install it with install.packages('logitnorm').")
  }

  # Ensure inputs are treated as matrices
  MU_mat     <- as.matrix(MU_mat)
  SIGMA2_mat <- as.matrix(SIGMA2_mat)

  # Structural validation checks
  if (ncol(MU_mat) != 6 || ncol(SIGMA2_mat) != 6) {
    stop("Both MU_mat and SIGMA2_mat must have exactly 6 columns.")
  }
  if (nrow(MU_mat) != nrow(SIGMA2_mat)) {
    stop("MU_mat and SIGMA2_mat must have the exact same number of rows.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("The conf_level argument must be a numeric value strictly between 0 and 1.")
  }

  # --- COMPONENT 1: EXACT LOGIT-NORMAL MOMENTS (via {logitnorm}) ---
  # momentsLogitnorm() is not vectorized over mu/sigma, so apply it elementwise
  # and reshape back into matrices matching MU_mat's dimensions.
  sigma_mat <- sqrt(SIGMA2_mat)
  moments   <- mapply(
    function(m, s) logitnorm::momentsLogitnorm(mu = m, sigma = s),
    m = as.vector(MU_mat), s = as.vector(sigma_mat)
  )
  E_prop <- matrix(moments["mean", ], nrow = nrow(MU_mat), ncol = ncol(MU_mat))
  V_prop <- matrix(moments["var", ],  nrow = nrow(MU_mat), ncol = ncol(MU_mat))

  # Report the implied mean-bias correction for each input variable/scenario,
  # in long format for easy inspection: naive plogis(mu) plug-in vs. the exact
  # logitnorm mean, and their difference (the "correction" that using the
  # exact moment implicitly applies relative to the naive plug-in).
  f_mu_naive <- 1 / (1 + exp(-MU_mat))
  var_names  <- c("Psi_SLJ", "theta_HOR_TCJ_SLJ", "Psi_MAC",
                  "theta_TCJ_CHP_MAC", "theta_TCJ_CHP_TRN", "theta_HOR_CHP_ORE")
  bias_correction_df <- do.call(rbind, lapply(seq_len(ncol(MU_mat)), function(j) {
    data.frame(
      scenario           = paste0("Scenario_", seq_len(nrow(MU_mat))),
      variable           = var_names[j],
      mu                 = MU_mat[, j],
      sigma2             = SIGMA2_mat[, j],
      E_prop_uncorrected = f_mu_naive[, j],
      E_prop_exact       = E_prop[, j],
      correction_term    = E_prop[, j] - f_mu_naive[, j]
    )
  }))
  rownames(bias_correction_df) <- NULL

  # Isolate individual variable columns from the proportion matrices
  E_Psi_SLJ <- E_prop[, 1]; V_Psi_SLJ <- V_prop[, 1]
  E_t_SLJ   <- E_prop[, 2]; V_t_SLJ   <- V_prop[, 2]
  E_Psi_MAC <- E_prop[, 3]; V_Psi_MAC <- V_prop[, 3]
  E_t_MAC   <- E_prop[, 4]; V_t_MAC   <- V_prop[, 4]
  E_t_TRN   <- E_prop[, 5]; V_t_TRN   <- V_prop[, 5]
  E_t_ORE   <- E_prop[, 6]; V_t_ORE   <- V_prop[, 6]

  # --- COMPONENT 2: SUB-METRIC ALLOCATION LAYER (S_TCJ-CHP) ---
  E_S_TCJ <- (E_Psi_MAC * E_t_MAC) + ((1 - E_Psi_MAC) * E_t_TRN)

  V_S_TCJ <- (V_Psi_MAC + E_Psi_MAC^2) * V_t_MAC +
    (V_Psi_MAC + (1 - E_Psi_MAC)^2) * V_t_TRN +
    V_Psi_MAC * (E_t_MAC - E_t_TRN)^2

  # --- COMPONENT 3: TOP-LEVEL METRIC PROPAGATION (S_HOR-CHP) ---
  # Solve the independent product block columns: W = theta_SLJ * S_TCJ
  E_prod_block <- E_t_SLJ * E_S_TCJ
  V_prod_block <- (V_t_SLJ + E_t_SLJ^2) * (V_S_TCJ + E_S_TCJ^2) - (E_prod_block^2)

  # Propagate components into final top-level systemic column vectors
  E_final <- (E_Psi_SLJ * E_prod_block) + ((1 - E_Psi_SLJ) * E_t_ORE)

  V_final <- (V_Psi_SLJ + E_Psi_SLJ^2) * V_prod_block +
    (V_Psi_SLJ + (1 - E_Psi_SLJ)^2) * V_t_ORE +
    V_Psi_SLJ * (E_prod_block - E_t_ORE)^2

  # --- COMPONENT 4: LOGIT-SCALE DELTA-METHOD CI LAYER ---
  # Dynamically calculate the critical z-value based on user input
  alpha_tail <- (1 - conf_level) / 2
  z_crit     <- qnorm(1 - alpha_tail)

  # Helper: given proportion-scale mean/variance, return a logit-scale CI
  # back-transformed to the proportion scale. Using the delta method in
  # reverse: Var(logit(p)) ~= Var(p) / (E(1-E))^2, since
  # d/dp[logit(p)] = 1 / (p * (1 - p)).
  logit_ci <- function(E, V) {
    logit_mean <- log(E / (1 - E))
    logit_var  <- V / (E * (1 - E))^2
    logit_se   <- sqrt(logit_var)

    logit_lower <- logit_mean - z_crit * logit_se
    logit_upper <- logit_mean + z_crit * logit_se

    list(
      lower = 1 / (1 + exp(-logit_lower)),
      upper = 1 / (1 + exp(-logit_upper))
    )
  }

  # Layer A: sub-metric S_TCJ_CHP
  ci_S_TCJ <- logit_ci(E_S_TCJ, V_S_TCJ)

  # Layer B: top-level metric S_HOR_CHP
  ci_final <- logit_ci(E_final, V_final)

  # Combine hierarchical results into a structured data frame matching original rows
  scenario_results <- data.frame(
    # Sub-Metric Layer Outputs (S_TCJ_CHP)
    S_TCJ_CHP_mean     = E_S_TCJ,
    S_TCJ_CHP_variance = V_S_TCJ,
    S_TCJ_CHP_sd       = sqrt(V_S_TCJ),
    S_TCJ_CHP_LCL      = ci_S_TCJ$lower,
    S_TCJ_CHP_UCL      = ci_S_TCJ$upper,

    # Top-Level Propagation Outputs (S_HOR_CHP)
    S_HOR_CHP_mean     = E_final,
    S_HOR_CHP_variance = V_final,
    S_HOR_CHP_sd       = sqrt(V_final),
    S_HOR_CHP_LCL      = ci_final$lower,
    S_HOR_CHP_UCL      = ci_final$upper
  )

  # Assign structured scenario numbers to the rows
  rownames(scenario_results) <- paste0("Scenario_", 1:nrow(MU_mat))

  return(list(
    deriv_ests         = scenario_results,
    bias_correction_df = bias_correction_df
  ))
}
