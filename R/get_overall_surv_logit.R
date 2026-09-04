#' Derive overall survial
#'  
#' Deriving overall survival estimates by application of the plug-in detla method
#'
#' @description Derive estimates of overall survival HOR-CHP and TCH-CHP based on logit-normal scale predictions from component models 
#'
#' @param lo_comp_estmat A linear predictors on the log-odds scale (etas)
#'        component models, where each row is a scenario and the 6 columns are in order:
#'        \code{[,1]} Psi_SLJ, \code{[,2]} theta_HOR-TCJ|SLJ, \code{[,3]} Psi_MAC, \code{[,4]} theta_TCJ-CHP|MAC, \code{[,5]} theta_TCJ-CHP|TRN, \code{[,6]} theta_HOR-CHP|ORE
#' @param lo_comp_varmat A matrix of the same dimensions containing the logit-scale variances of the component model estimates.
#' @param conf_level A numeric value between 0 and 1 indicating the target confidence interval width (default = 0.95).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{deriv_ests}{A data frame containing multi-layer analytical results for each scenario row,
#'       plus one \code{bias_correction_<variable>} column per input variable reporting the
#'       Component 1 second-order mean-bias correction term \code{(sigma2/2) * f''(mu)} applied
#'       when mapping that variable's logit-scale input to the proportion scale.}
#'     \item{bias_correction_df}{A long-format data frame (one row per scenario x input variable)
#'       reporting the same Component 1 second-order mean-bias correction in more detail: the
#'       uncorrected plug-in value \code{f(mu)}, the second derivative \code{f''(mu)}, the
#'       correction term \code{(sigma2/2) * f''(mu)}, and the corrected proportion-scale mean
#'       actually used downstream.}
#'   }
#' @export
get_overall_surv_logit <- function(lo_comp_estmat, lo_comp_varmat, conf_level = 0.95) {

  # Ensure inputs are treated as matrices
  lo_comp_estmat <- as.matrix(lo_comp_estmat)
  lo_comp_varmat <- as.matrix(lo_comp_varmat)

  # Structural validation checks
  if (ncol(lo_comp_estmat) != 6 || ncol(lo_comp_varmat) != 6) {
    stop("Both lo_comp_estmat and lo_comp_varmat must have exactly 6 columns.")
  }
  if (nrow(lo_comp_estmat) != nrow(lo_comp_varmat)) {
    stop("lo_comp_estmat and lo_comp_varmat must have the exact same number of rows.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("The conf_level argument must be a numeric value strictly between 0 and 1.")
  }

  # --- COMPONENT 1: MATRIX DERIVATIVES & LAYER MAPPING ---
  f_mu  <- 1 / (1 + exp(-lo_comp_estmat))
  f_pr  <- f_mu * (1 - f_mu)
  f_sec <- f_mu * (1 - f_mu) * (1 - 2 * f_mu)

  # Map parameters into proportion-domain moment matrices
  bias_correction_term <- (lo_comp_varmat / 2) * f_sec
  E_prop <- f_mu + bias_correction_term
  V_prop <- (f_pr^2) * lo_comp_varmat

  # Report the second-order mean-bias correction applied to each input
  # variable/scenario, in long format for easy inspection.
  var_names <- c("Psi_SLJ", "theta_HOR_TCJ_SLJ", "Psi_MAC",
                 "theta_TCJ_CHP_MAC", "theta_TCJ_CHP_TRN", "theta_HOR_CHP_ORE")
  bias_correction_df <- do.call(rbind, lapply(seq_len(ncol(lo_comp_estmat)), function(j) {
    data.frame(
      scenario           = paste0("Scenario_", seq_len(nrow(lo_comp_estmat))),
      variable           = var_names[j],
      mu                 = lo_comp_estmat[, j],
      sigma2             = lo_comp_varmat[, j],
      f_mu               = f_mu[, j],
      f_sec              = f_sec[, j],
      correction_term    = bias_correction_term[, j],
      E_prop_uncorrected = f_mu[, j],
      E_prop_corrected   = E_prop[, j]
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



  # Layer A: sub-metric S_TCJ_CHP
  ci_S_TCJ <- logit_ci(E_S_TCJ, V_S_TCJ, z_crit)

  # Layer B: top-level metric S_HOR_CHP
  ci_final <- logit_ci(E_final, V_final, z_crit)

  # Combine hierarchical results into a structured data frame matching original rows
  pred_overallDF <- data.frame(
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

  # Append the Component 1 second-order mean-bias correction term for each
  # of the 6 input variables as its own column (wide format), matching
  # bias_correction_term computed above -- one column per variable, one row
  # per scenario, matching pred_overallDF's row order.
  bias_correction_wide <- as.data.frame(bias_correction_term)
  names(bias_correction_wide) <- paste0("bias_correction_", var_names)
  pred_overallDF <- cbind(pred_overallDF, bias_correction_wide)

  # Assign structured scenario numbers to the rows
  # rownames(pred_overallDF) <- paste0("Day_", 1:nrow(lo_comp_estmat))

  return(list(
    deriv_ests         = pred_overallDF,
    bias_correction_df = bias_correction_df
  ))
}


#' Logit-scale delta-method confidence interval
#'
#' Given a proportion-scale mean/variance, returns a confidence interval
#' computed on the logit scale and back-transformed to the proportion scale.
#' Uses the delta method in reverse: \code{Var(logit(p)) ~= Var(p) / (E(1-E))^2},
#' since \code{d/dp[logit(p)] = 1 / (p * (1 - p))}.
#'
#' @param E Proportion-scale mean.
#' @param V Proportion-scale variance.
#' @param z_crit Critical z-value (e.g. from \code{qnorm()}) determining the width of the interval.
#'
#' @returns A list with \code{lower} and \code{upper} bounds on the proportion scale.
#' @export
logit_ci <- function(E, V, z_crit) {
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
