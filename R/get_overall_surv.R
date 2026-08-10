#' Analytical Logit-Normal Network Approximation for Matrices (Simulation-Free)
#'
#' @param MU_mat A matrix where each row is a scenario and the 6 columns are logit-scale means in order:
#'        [,1] Psi_SLJ, [,2] theta_HOR-TCJ|SLJ, [,3] Psi_MAC, [,4] theta_TCJ-CHP|MAC, [,5] theta_TCJ-CHP|TRN, [,6] theta_HOR-CHP|ORE
#' @param SIGMA2_mat A matrix of the same dimensions containing logit-scale variances.
#' @param conf_level A numeric value between 0 and 1 indicating the target confidence interval width (default = 0.95).
#'
#' @return A data frame containing multi-layer analytical results for each scenario row.
get_overall_surv <- function(MU_mat, SIGMA2_mat, conf_level = 0.95) {
  
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
  
  # --- COMPONENT 1: MATRIX DERIVATIVES & LAYER MAPPING ---
  f_mu  <- 1 / (1 + exp(-MU_mat))                                     
  f_pr  <- f_mu * (1 - f_mu)                                      
  f_sec <- f_mu * (1 - f_mu) * (1 - 2 * f_mu)                    
  
  # Map parameters into proportion-domain moment matrices
  E_prop <- f_mu + (SIGMA2_mat / 2) * f_sec                           
  V_prop <- (f_pr^2) * SIGMA2_mat                                     
  
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
  
  # --- COMPONENT 4: PARAMETRIC DISTRIBUTION FIT LAYER ---
  # Dynamically calculate tail probabilities based on user input
  alpha_tail <- (1 - conf_level) / 2
  prob_lower <- alpha_tail
  prob_upper <- 1 - alpha_tail
  
  # Layer A: Fit and solve boundaries for the sub-metric S_TCJ_CHP
  match_S_TCJ <- (E_S_TCJ * (1 - E_S_TCJ) / V_S_TCJ) - 1
  alpha_S_TCJ <- E_S_TCJ * match_S_TCJ
  beta_S_TCJ  <- (1 - E_S_TCJ) * match_S_TCJ
  
  lower_bound_S_TCJ <- qbeta(prob_lower, alpha_S_TCJ, beta_S_TCJ)
  upper_bound_S_TCJ <- qbeta(prob_upper, alpha_S_TCJ, beta_S_TCJ)
  
  # Layer B: Fit and solve boundaries for the top-level metric S_HOR_CHP
  match_final <- (E_final * (1 - E_final) / V_final) - 1
  alpha_final <- E_final * match_final
  beta_final  <- (1 - E_final) * match_final
  
  lower_bound_final <- qbeta(prob_lower, alpha_final, beta_final)
  upper_bound_final <- qbeta(prob_upper, alpha_final, beta_final)
  
  # Combine hierarchical results into a structured data frame matching original rows
  scenario_results <- data.frame(
    # Sub-Metric Layer Outputs (S_TCJ_CHP)
    S_TCJ_CHP_mean     = E_S_TCJ,
    S_TCJ_CHP_variance = V_S_TCJ,
    S_TCJ_CHP_sd       = sqrt(V_S_TCJ),
    S_TCJ_CHP_LCL      = lower_bound_S_TCJ,
    S_TCJ_CHP_UCL      = upper_bound_S_TCJ,
    
    # Top-Level Propagation Outputs (S_HOR_CHP)
    S_HOR_CHP_mean     = E_final,
    S_HOR_CHP_variance = V_final,
    S_HOR_CHP_sd       = sqrt(V_final),
    S_HOR_CHP_LCL      = lower_bound_final,
    S_HOR_CHP_UCL      = upper_bound_final
  )
  
  # Assign structured scenario numbers to the rows
  rownames(scenario_results) <- paste0("Scenario_", 1:nrow(MU_mat))
  
  return(scenario_results)
}


#' Logit-Normal Network Approximation for Matrices Via Monte Carlo Simulation
#'
#' @param MU_mat A matrix where each row is a scenario and the 6 columns are logit-scale means in order:
#'        [,1] Psi_SLJ, [,2] theta_HOR-TCJ|SLJ, [,3] Psi_MAC, [,4] theta_TCJ-CHP|MAC, [,5] theta_TCJ-CHP|TRN, [,6] theta_HOR-CHP|ORE
#' @param SIGMA2_mat A matrix of the same dimensions containing logit-scale variances.
#' @param conf_level A numeric value between 0 and 1 indicating the target confidence interval width (default = 0.95).
#' @param n_samples Number of Monte Carlo simulation iterations per scenario row (default = 100,000).
#'
#' @return A data frame containing multi-layer simulated results for each scenario row.
get_overall_surv_sim <- function(MU_mat, SIGMA2_mat, conf_level = 0.95, n_samples = 1000) {
  
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
  
  n_scenarios <- nrow(MU_mat)
  
  # Dynamically calculate tail percentiles based on user input
  alpha_tail <- (1 - conf_level) / 2
  prob_lower <- alpha_tail
  prob_upper <- 1 - alpha_tail
  
  # Pre-allocate vectors to build the final results data frame
  S_TCJ_CHP_mean     <- numeric(n_scenarios)
  S_TCJ_CHP_variance <- numeric(n_scenarios)
  S_TCJ_CHP_sd       <- numeric(n_scenarios)
  S_TCJ_CHP_LCL      <- numeric(n_scenarios)
  S_TCJ_CHP_UCL      <- numeric(n_scenarios)
  
  S_HOR_CHP_mean     <- numeric(n_scenarios)
  S_HOR_CHP_variance <- numeric(n_scenarios)
  S_HOR_CHP_sd       <- numeric(n_scenarios)
  S_HOR_CHP_LCL      <- numeric(n_scenarios)
  S_HOR_CHP_UCL      <- numeric(n_scenarios)
  
  # Loop through each scenario row to execute independent simulation blocks
  for (i in 1:n_scenarios) {
    
    # 1. DRAW SAMPLES DIRECTLY IN THE LATENT NORMAL DOMAIN
    # Row vectors provide the mean and standard deviation parameters for rnorm
    s_Psi_SLJ           <- rnorm(n_samples, mean = MU_mat[i, 1], sd = sqrt(SIGMA2_mat[i, 1]))
    s_theta_HOR_TCJ_SLJ <- rnorm(n_samples, mean = MU_mat[i, 2], sd = sqrt(SIGMA2_mat[i, 2]))
    s_Psi_MAC           <- rnorm(n_samples, mean = MU_mat[i, 3], sd = sqrt(SIGMA2_mat[i, 3]))
    s_theta_TCJ_CHP_MAC <- rnorm(n_samples, mean = MU_mat[i, 4], sd = sqrt(SIGMA2_mat[i, 4]))
    s_theta_TCJ_CHP_TRN <- rnorm(n_samples, mean = MU_mat[i, 5], sd = sqrt(SIGMA2_mat[i, 5]))
    s_theta_HOR_CHP_ORE <- rnorm(n_samples, mean = MU_mat[i, 6], sd = sqrt(SIGMA2_mat[i, 6]))
    
    # 2. TRANSFORM SAMPLES TO PROPORTIONS VIA THE LOGISTIC LINK
    Psi_SLJ           <- 1 / (1 + exp(-s_Psi_SLJ))
    theta_HOR_TCJ_SLJ <- 1 / (1 + exp(-s_theta_HOR_TCJ_SLJ))
    Psi_MAC           <- 1 / (1 + exp(-s_Psi_MAC))
    theta_TCJ_CHP_MAC <- 1 / (1 + exp(-s_theta_TCJ_CHP_MAC))
    theta_TCJ_CHP_TRN <- 1 / (1 + exp(-s_theta_TCJ_CHP_TRN))
    theta_HOR_CHP_ORE <- 1 / (1 + exp(-s_theta_HOR_CHP_ORE))
    
    # 3. APPLY ZERO-SUM STRUCTURAL WEIGHT CONSTRAINTS
    Psi_TRN <- 1.0 - Psi_MAC
    Psi_ORE <- 1.0 - Psi_SLJ
    
    # 4. EVALUATE THE HIERARCHICAL EQUATION VECTORS
    sim_S_TCJ_CHP <- (Psi_MAC * theta_TCJ_CHP_MAC) + (Psi_TRN * theta_TCJ_CHP_TRN)
    sim_S_HOR_CHP <- (Psi_SLJ * theta_HOR_TCJ_SLJ * sim_S_TCJ_CHP) + (Psi_ORE * theta_HOR_CHP_ORE)
    
    # 5. EXTRACT EMPIRICAL STATISTICAL PROFILES
    # Sub-Metric profile allocation
    S_TCJ_CHP_mean[i]     <- mean(sim_S_TCJ_CHP)
    S_TCJ_CHP_variance[i] <- var(sim_S_TCJ_CHP)
    S_TCJ_CHP_sd[i]       <- sd(sim_S_TCJ_CHP)
    S_TCJ_CHP_LCL[i]      <- quantile(sim_S_TCJ_CHP, prob_lower)
    S_TCJ_CHP_UCL[i]      <- quantile(sim_S_TCJ_CHP, prob_upper)
    
    # Top-Level profile allocation
    S_HOR_CHP_mean[i]     <- mean(sim_S_HOR_CHP)
    S_HOR_CHP_variance[i] <- var(sim_S_HOR_CHP)
    S_HOR_CHP_sd[i]       <- sd(sim_S_HOR_CHP)
    S_HOR_CHP_LCL[i]      <- quantile(sim_S_HOR_CHP, prob_lower)
    S_HOR_CHP_UCL[i]      <- quantile(sim_S_HOR_CHP, prob_upper)
  }
  
  # Combine results into the final scenario dataframe matrix
  scenario_results <- data.frame(
    S_TCJ_CHP_mean     = S_TCJ_CHP_mean,
    S_TCJ_CHP_variance = S_TCJ_CHP_variance,
    S_TCJ_CHP_sd       = S_TCJ_CHP_sd,
    S_TCJ_CHP_LCL      = S_TCJ_CHP_LCL,
    S_TCJ_CHP_UCL      = S_TCJ_CHP_UCL,
    
    S_HOR_CHP_mean     = S_HOR_CHP_mean,
    S_HOR_CHP_variance = S_HOR_CHP_variance,
    S_HOR_CHP_sd       = S_HOR_CHP_sd,
    S_HOR_CHP_LCL      = S_HOR_CHP_LCL,
    S_HOR_CHP_UCL      = S_HOR_CHP_UCL
  )
  
  rownames(scenario_results) <- paste0("Scenario_", 1:n_scenarios)
  return(scenario_results)
}


