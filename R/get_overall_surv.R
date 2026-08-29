
#' Analytical Logit-Normal Network Approximation for Matrices (Simulation-Free)
#'
#' @param E_prop A matrix where each row is a scenario and the 6 columns are proportion-scale means (already converted from the logit scale) in order:
#'        \code{[,1]} Psi_SLJ, \code{[,2]} theta_HOR-TCJ|SLJ, \code{[,3]} Psi_MAC, \code{[,4]} theta_TCJ-CHP|MAC, \code{[,5]} theta_TCJ-CHP|TRN, \code{[,6]} theta_HOR-CHP|ORE
#' @param V_prop A matrix of the same dimensions containing proportion-scale variances (already converted from the logit scale).
#' @param conf_level A numeric value between 0 and 1 indicating the target confidence interval width (default = 0.95).
#' @param MU_mat Optional. A matrix of the same dimensions as \code{E_prop} containing the original
#'   logit-scale means that \code{E_prop} was derived from (if available to the caller). Since
#'   \code{get_overall_surv()} takes already-converted proportion-scale moments and has no Component 1
#'   step of its own, it cannot compute a mean-bias correction internally. When \code{MU_mat} is supplied,
#'   this argument is used purely for *diagnostic* reporting: it lets \code{bias_correction_df} show how
#'   far the supplied \code{E_prop} differs from the naive, uncorrected \code{plogis(MU_mat)} plug-in,
#'   for comparison with \code{get_overall_surv_logit()}'s Component 1 correction. It has no effect on
#'   the returned \code{deriv_ests} calculation. Defaults to \code{NULL} (no diagnostic reporting).
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{deriv_ests}{A data frame containing multi-layer analytical results for each scenario row.}
#'     \item{beta_parm_df}{Beta-distribution shape parameters computed from the sub-metric and
#'       top-level moments. Retained for backwards compatibility; not used to derive the confidence
#'       intervals in \code{deriv_ests}, which instead use the logit-scale delta-method CI.}
#'     \item{bias_correction_df}{\code{NULL} if \code{MU_mat} is not supplied. Otherwise, a long-format
#'       data frame (one row per scenario x input variable) reporting the naive \code{plogis(mu)}
#'       plug-in, the supplied (possibly bias-corrected) \code{E_prop}, and their difference —
#'       for parity with the diagnostic reported by \code{get_overall_surv_logit()}.}
#'   }
get_overall_surv <- function(E_prop, V_prop, conf_level = 0.95, MU_mat = NULL) {
  
  SE_marg = qnorm(conf_level + (1 - conf_level) / 2)

  # Optional diagnostic: compare the supplied (possibly bias-corrected)
  # E_prop against the naive plogis(MU_mat) plug-in, if the caller happens
  # to have the original logit-scale means available. get_overall_surv()
  # itself has no Component 1 step, so this is reporting-only and does not
  # feed back into the deriv_ests calculation below.
  bias_correction_df <- NULL
  if (!is.null(MU_mat)) {
    MU_mat_chk <- as.matrix(MU_mat)
    E_prop_chk <- as.matrix(E_prop)
    if (!identical(dim(MU_mat_chk), dim(E_prop_chk))) {
      stop("MU_mat must have the same dimensions as E_prop.")
    }
    f_mu_naive <- 1 / (1 + exp(-MU_mat_chk))
    var_names  <- c("Psi_SLJ", "theta_HOR_TCJ_SLJ", "Psi_MAC",
                    "theta_TCJ_CHP_MAC", "theta_TCJ_CHP_TRN", "theta_HOR_CHP_ORE")
    bias_correction_df <- do.call(rbind, lapply(seq_len(ncol(MU_mat_chk)), function(j) {
      data.frame(
        scenario           = paste0("Scenario_", seq_len(nrow(MU_mat_chk))),
        variable           = var_names[j],
        mu                 = MU_mat_chk[, j],
        E_prop_uncorrected = f_mu_naive[, j],
        E_prop_supplied    = E_prop_chk[, j],
        correction_term    = E_prop_chk[, j] - f_mu_naive[, j]
      )
    }))
    rownames(bias_correction_df) <- NULL
  }

  E_Psi_SLJ <- E_prop[, 1]; V_Psi_SLJ <- V_prop[, 1]
  E_t_SLJ   <- E_prop[, 2]; V_t_SLJ   <- V_prop[, 2]
  E_Psi_MAC <- E_prop[, 3]; V_Psi_MAC <- V_prop[, 3] 
  E_t_MAC   <- E_prop[, 4]; V_t_MAC   <- V_prop[, 4]
  E_t_TRN   <- E_prop[, 5]; V_t_TRN   <- V_prop[, 5]
  E_t_ORE   <- E_prop[, 6]; V_t_ORE   <- V_prop[, 6]
  
  # --- COMPONENT 2: SUB-METRIC ALLOCATION LAYER (S_TCJ-CHP) ---
  E_S_TCJ <- (E_Psi_MAC * E_t_MAC) + ((1 - E_Psi_MAC) * E_t_TRN)
  # print(E_Psi_SLJ)
  
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


  
  # --- COMPONENT 4: PARAMETRIC DISTRIBUTION FIT LAYER ---
  # Dynamically calculate tail probabilities based on user input
  alpha_tail <- (1 - conf_level) / 2
  prob_lower <- alpha_tail
  prob_upper <- 1 - alpha_tail
  
  # Layer A: Fit and solve boundaries for the sub-metric S_TCJ_CHP
  match_S_TCJ <- (E_S_TCJ * (1 - E_S_TCJ) / V_S_TCJ) - 1
  alpha_S_TCJ <- E_S_TCJ * match_S_TCJ
  beta_S_TCJ  <- (1 - E_S_TCJ) * match_S_TCJ
  
  
  # Layer B: Fit and solve boundaries for the top-level metric S_HOR_CHP
  match_final <- (E_final * (1 - E_final) / V_final) - 1
  alpha_final <- E_final * match_final
  beta_final  <- (1 - E_final) * match_final
  
  
  
  beta_parm_df=data.frame(
    alpha_S_TCJ=alpha_S_TCJ,
    beta_S_TCJ=beta_S_TCJ,
    alpha_HOR=alpha_final,
    beta_HOR=beta_final)
  names(beta_parm_df) <- c("a_S_TCJ","b_S_TCJ","a_S_HOR","b_S_HOR")
  
  # beta CIs
  # beta_parm_df$S_TCJ_bLCL <- ifelse(beta_parm_df$a_S_TCJ>0 & beta_parm_df$b_S_TCJ>0,qbeta(prob_lower, beta_parm_df$a_S_TCJ, beta_parm_df$b_S_TCJ),NA)
  # beta_parm_df$S_TCJ_bUCL <- ifelse(beta_parm_df$a_S_TCJ>0 & beta_parm_df$b_S_TCJ>0,qbeta(prob_upper, beta_parm_df$a_S_TCJ, beta_parm_df$b_S_TCJ),NA) 
  # beta_parm_df$S_HOR_bLCL <- ifelse(beta_parm_df$a_S_HOR>0 & beta_parm_df$b_S_HOR>0,qbeta(prob_lower, beta_parm_df$a_S_HOR, beta_parm_df$b_S_HOR),NA)
  # beta_parm_df$S_HOR_bUCL <- ifelse(beta_parm_df$a_S_HOR>0 & beta_parm_df$b_S_HOR>0,qbeta(prob_upper, beta_parm_df$a_S_HOR, beta_parm_df$b_S_HOR),NA) 
  
  
  # 
  # beta_parm_df$S_HOR_bLCL <- ifelse(alpha_final>0 & beta_final>0,qbeta(prob_lower, alpha_final, beta_final),NA)
  # beta_parm_df$S_HOR_bUCL <- ifelse(alpha_final>0 & beta_final>0,qbeta(prob_upper, alpha_final, beta_final),NA)
  # 
  # lower_bound_final <- qbeta(prob_lower, alpha_final, beta_final)
  # upper_bound_final <- qbeta(prob_upper, alpha_final, beta_final)
  
  
    # return(beta_parm_df)
  
  # Combine hierarchical results into a structured data frame matching original rows
  scenario_results <- data.frame(
    # Sub-Metric Layer Outputs (S_TCJ_CHP)
    S_TCJ_CHP_mean     = E_S_TCJ,
    S_TCJ_CHP_variance = V_S_TCJ,
    S_TCJ_CHP_sd       = sqrt(V_S_TCJ),
    lo_S_TCJ_CHP=log((E_S_TCJ)/(1-E_S_TCJ)),

    # S_TCJ_CHP_LCL      = lower_bound_S_TCJ,
    # S_TCJ_CHP_UCL      = upper_bound_S_TCJ,
    
    # Top-Level Propagation Outputs (S_HOR_CHP)
    S_HOR_CHP_mean     = E_final,
    S_HOR_CHP_variance = V_final,
    S_HOR_CHP_sd       = sqrt(V_final),
    lo_S_HOR_CHP_mean=log((E_final)/(1-E_final))
    

    # S_HOR_CHP_LCL      = lower_bound_final,
    # S_HOR_CHP_UCL      = upper_bound_final
  )

  names(scenario_results) <- c("S_TCJ_CHP_mean","S_TCJ_CHP_var","S_TCJ_CHP_sd","lo_S_TCJ_CHP","S_HOR_CHP_mean","S_HOR_CHP_var","S_HOR_CHP_sd","lo_S_HOR_CHP_mean")
  
  # Delta-method transform of the proportion-scale variance to the logit
  # scale: Var(logit(p)) ~= Var(p) / (E(1-E))^2, since
  # d/dp[logit(p)] = 1 / (p * (1 - p)).
  lo_se_TCJ <- sqrt(scenario_results$S_TCJ_CHP_var / (scenario_results$S_TCJ_CHP_mean * (1 - scenario_results$S_TCJ_CHP_mean))^2)
  lo_se_HOR <- sqrt(scenario_results$S_HOR_CHP_var / (scenario_results$S_HOR_CHP_mean * (1 - scenario_results$S_HOR_CHP_mean))^2)
  
  
  scenario_results$S_TCJ_CHP_LCL <- plogis(scenario_results$lo_S_TCJ_CHP-lo_se_TCJ*SE_marg)
  scenario_results$S_TCJ_CHP_UCL <- plogis(scenario_results$lo_S_TCJ_CHP+lo_se_TCJ*SE_marg)
  
  scenario_results$S_HOR_CHP_LCL <- plogis(scenario_results$lo_S_HOR_CHP-lo_se_HOR*SE_marg)
  scenario_results$S_HOR_CHP_UCL <- plogis(scenario_results$lo_S_HOR_CHP+lo_se_HOR*SE_marg)
  

  # Assign structured scenario numbers to the rows
  # rownames(scenario_results) <- paste0("Scenario_", 1:nrow(MU_mat))
  
  out=list(
    "deriv_ests"=scenario_results,
    "beta_parm_df"=beta_parm_df,
    "bias_correction_df"=bias_correction_df
  )
  
  return(out)
  
  # return(scenario_results)
}

get_overall_surv_calc <- function(E_prop) {
  
  # SE_marg = qnorm(conf_level + (1 - conf_level) / 2)
  
  E_Psi_SLJ <- E_prop[, 1]; #V_Psi_SLJ <- V_prop[, 1]
  E_t_SLJ   <- E_prop[, 2]; #V_t_SLJ   <- V_prop[, 2]
  E_Psi_MAC <- E_prop[, 3]; #V_Psi_MAC <- V_prop[, 3] 
  E_t_MAC   <- E_prop[, 4]; #V_t_MAC   <- V_prop[, 4]
  E_t_TRN   <- E_prop[, 5]; #V_t_TRN   <- V_prop[, 5]
  E_t_ORE   <- E_prop[, 6]; #V_t_ORE   <- V_prop[, 6]
  
  # --- COMPONENT 2: SUB-METRIC ALLOCATION LAYER (S_TCJ-CHP) ---
  E_S_TCJ <- (E_Psi_MAC * E_t_MAC) + ((1 - E_Psi_MAC) * E_t_TRN)
  # print(E_Psi_SLJ)
  
  # V_S_TCJ <- (V_Psi_MAC + E_Psi_MAC^2) * V_t_MAC + 
  #   (V_Psi_MAC + (1 - E_Psi_MAC)^2) * V_t_TRN + 
  #   V_Psi_MAC * (E_t_MAC - E_t_TRN)^2
  
  # --- COMPONENT 3: TOP-LEVEL METRIC PROPAGATION (S_HOR-CHP) ---
  # Solve the independent product block columns: W = theta_SLJ * S_TCJ
  E_prod_block <- E_t_SLJ * E_S_TCJ
  # V_prod_block <- (V_t_SLJ + E_t_SLJ^2) * (V_S_TCJ + E_S_TCJ^2) - (E_prod_block^2)
  
  # Propagate components into final top-level systemic column vectors
  E_final <- (E_Psi_SLJ * E_prod_block) + ((1 - E_Psi_SLJ) * E_t_ORE)
  
  c(E_S_TCJ,E_final)
  
  }




