


# load the confidence sets parameter estimates and joint covariance matrix

HOR_CHP_pred_comp_ls <- readRDS("HOR_CHP_pred_comp_ls.rds")

devtools::load_all()

devtools::load_all("../CVhelp")
CVhelp_dat_w_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide",DOY_rng = 1:250)
HOR_CHP_comp_ls_scl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = TRUE)

# consider changing HOR_CHP_mod_wrap to "HOR_CHP_xpred_prep"
xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
                              sel_rows_tmp1=CVhelp_dat_w_alt,
                              flength_in=240)



HOR_CHP_pred_get_pred_first_model <- function(HOR_CHP_pred_comp_ls_in=HOR_CHP_pred_comp_ls,
                                  xpred_tmp_in=xpred_tmp){
  
  AIC_DF_d2=HOR_CHP_pred_comp_ls_in$"AIC_DF_d2"
  mods_w_covmat_ls=HOR_CHP_pred_comp_ls_in$"mods_w_covmat_ls"
  pt_estsDF_ls=HOR_CHP_pred_comp_ls_in$"pt_estsDF_l"
  logSD_RELGRP_v <- HOR_CHP_pred_comp_ls_in$"logSD_RELGRP_v"
  xpred_tmp <- xpred_tmp_in
  
  
  jj=1 
  pred_DF_analytic_ls <- list()
  # for(jj in 1:nrow(AIC_DF_d2)){
  
  # row index for survival paramters
  ind_sconf <- which(rownames(mods_w_covmat_ls[[1]])=="S_pars")
  
  Spred_varnames <- pt_estsDF_ls[[jj]]$par_nm[ind_sconf]
  mtch_ind <- match(Spred_varnames,colnames(xpred_tmp))
  new_x <- as.matrix(xpred_tmp[,mtch_ind])
  
  # estimated parameters
  Spred_par_vals <- pt_estsDF_ls[[jj]]$coeff_param[ind_sconf] 
  Sigma_mat_S <- mods_w_covmat_ls[[jj]][ind_sconf,ind_sconf]
  
  
  # linear predictor
  EST <- as.vector(new_x  %*% Spred_par_vals)
  analytical_SE <- sqrt(rowSums((new_x %*% Sigma_mat_S) * new_x))
  
  # SD for random intercept
  SD_RELGRP <- exp(logSD_RELGRP_v[jj])
  
  pred_DF_analytic <- data.frame(
    rw=1:nrow(xpred_tmp),
    id=jj,
    xpred_tmp,
    EST,
    SE=analytical_SE,
    LCL=EST-analytical_SE*1.96,
    UCL=EST+analytical_SE*1.96,
    SEadj=sqrt(analytical_SE^2 + exp(logSD_RELGRP_v[jj])^2)
  )
  
  pred_DF_analytic$LCLadj=pred_DF_analytic$EST-pred_DF_analytic$SEadj*1.96
  pred_DF_analytic$UCLadj=pred_DF_analytic$EST+pred_DF_analytic$SEadj*1.96
  # pred_DF_analytic_ls[[jj]] <- pred_DF_analytic
  # }
  
  return(pred_DF_analytic)
  
  list(Spred_par_vals,
       SD_RELGRP,
       EST,
       analytical_SE)
}

HOR_CHP_pred_get_pred_ls <- function(HOR_CHP_pred_comp_ls_in=HOR_CHP_pred_comp_ls,
                                  xpred_tmp_in=xpred_tmp){
  
  AIC_DF_d2=HOR_CHP_pred_comp_ls_in$"AIC_DF_d2"
  mods_w_covmat_ls=HOR_CHP_pred_comp_ls_in$"mods_w_covmat_ls"
  pt_estsDF_ls=HOR_CHP_pred_comp_ls_in$"pt_estsDF_l"
  logSD_RELGRP_v <- HOR_CHP_pred_comp_ls_in$"logSD_RELGRP_v"
  xpred_tmp <- xpred_tmp_in
  
  
  jj=1 
  pred_DF_analytic_ls <- list()
  for(jj in 1:nrow(AIC_DF_d2)){
    
    # row index for survival paramters
    ind_sconf <- which(rownames(mods_w_covmat_ls[[jj]])=="S_pars")
    
    Spred_varnames <- pt_estsDF_ls[[jj]]$par_nm[ind_sconf]
    mtch_ind <- match(Spred_varnames,colnames(xpred_tmp))
    new_x <- as.matrix(xpred_tmp[,mtch_ind])
    
    # estimated parameters
    Spred_par_vals <- pt_estsDF_ls[[jj]]$coeff_param[ind_sconf] 
    Sigma_mat_S <- mods_w_covmat_ls[[jj]][ind_sconf,ind_sconf]
    
    
    # linear predictor
    EST <- as.vector(new_x  %*% Spred_par_vals)
    analytical_SE <- sqrt(rowSums((new_x %*% Sigma_mat_S) * new_x))
    
    # SD for random intercept
    SD_RELGRP <- exp(logSD_RELGRP_v[jj])
    
  pred_DF_analytic <- data.frame(
    rw=1:nrow(xpred_tmp),
    id=jj,
    xpred_tmp,
    EST,
    SE=analytical_SE,
    LCL=EST-analytical_SE*1.96,
    UCL=EST+analytical_SE*1.96,
    SEadj=sqrt(analytical_SE^2 + exp(logSD_RELGRP_v[jj])^2)
  )

  pred_DF_analytic$LCLadj=pred_DF_analytic$EST-pred_DF_analytic$SEadj*1.96
  pred_DF_analytic$UCLadj=pred_DF_analytic$EST+pred_DF_analytic$SEadj*1.96
  pred_DF_analytic_ls[[jj]] <- pred_DF_analytic
  }
    
  return(pred_DF_analytic_ls)
  
  # list(Spred_par_vals,
  #      SD_RELGRP,
  #      EST,
  #      analytical_SE)
}

HOR_CHP_pred_get_pred_all <- function(HOR_CHP_pred_comp_ls_in=HOR_CHP_pred_comp_ls,
                                     xpred_tmp_in=xpred_tmp){
  
  AIC_DF_d2=HOR_CHP_pred_comp_ls_in$"AIC_DF_d2"
  mods_w_covmat_ls=HOR_CHP_pred_comp_ls_in$"mods_w_covmat_ls"
  pt_estsDF_ls=HOR_CHP_pred_comp_ls_in$"pt_estsDF_l"
  logSD_RELGRP_v <- HOR_CHP_pred_comp_ls_in$"logSD_RELGRP_v"
  xpred_tmp <- xpred_tmp_in
  
  
  jj=1 
  pred_DF_analytic_ls <- list()
  for(jj in 1:nrow(AIC_DF_d2)){
    
    # row index for survival paramters
    ind_sconf <- which(rownames(mods_w_covmat_ls[[jj]])=="S_pars")
    
    Spred_varnames <- pt_estsDF_ls[[jj]]$par_nm[ind_sconf]
    mtch_ind <- match(Spred_varnames,colnames(xpred_tmp))
    new_x <- as.matrix(xpred_tmp[,mtch_ind])
    
    # estimated parameters
    Spred_par_vals <- pt_estsDF_ls[[jj]]$coeff_param[ind_sconf] 
    Sigma_mat_S <- mods_w_covmat_ls[[jj]][ind_sconf,ind_sconf]
    
    
    # linear predictor
    EST <- as.vector(new_x  %*% Spred_par_vals)
    analytical_SE <- sqrt(rowSums((new_x %*% Sigma_mat_S) * new_x))
    
    # SD for random intercept
    SD_RELGRP <- exp(logSD_RELGRP_v[jj])
    
    pred_DF_analytic <- data.frame(
      rw=1:nrow(xpred_tmp),
      id=jj,
      xpred_tmp,
      EST,
      SE=analytical_SE,
      LCL=EST-analytical_SE*1.96,
      UCL=EST+analytical_SE*1.96,
      SEadj=sqrt(analytical_SE^2 + exp(logSD_RELGRP_v[jj])^2)
    )
    
    pred_DF_analytic$LCLadj=pred_DF_analytic$EST-pred_DF_analytic$SEadj*1.96
    pred_DF_analytic$UCLadj=pred_DF_analytic$EST+pred_DF_analytic$SEadj*1.96
    pred_DF_analytic_ls[[jj]] <- pred_DF_analytic
  }
  # return(pred_DF_analytic_ls)
  
  pred_DF_analytic_comb <- do.call(rbind,pred_DF_analytic_ls)

  # wt averaging
  pred_DF_analytic_comb$wt=AIC_DF_d2$AICwt[pred_DF_analytic_comb$id]
  
  in_avg_tb <-  pred_DF_analytic_comb |>
    dplyr::group_by(rw) |>
    dplyr::summarize(
      wt_lp_EST=sum(EST*wt))
  
  # adding the model selection uncertainty to the estimate
  predDFcomb2 <- pred_DF_analytic_comb |> left_join(in_avg_tb,by = "rw") |> 
    mutate(
      diff_modavg=(EST-wt_lp_EST),
      SS_modavg=(EST-wt_lp_EST)^2,
      lo_SE_modavg=sqrt(SS_modavg+(SEadj)^2))
  
  full_var_predDF <- predDFcomb2 |>
    select(id,rw,EST,wt,wt_lp_EST,lo_SE_modavg) |>
    dplyr::group_by(rw) |>
    dplyr::summarize(
      var_modavg=sum(wt*lo_SE_modavg),
      se_moderr=sqrt(var_modavg))
  
  avg_est_tmp <- in_avg_tb |> left_join(full_var_predDF,by = "rw")
  xpred_tmp$rw=1:nrow(xpred_tmp)
  avg_est_tmp_wVars <- xpred_tmp |> left_join(avg_est_tmp,by = "rw")# |> left_join(predDFcomb2 |> select(rw,id,lo_pred,lo_SE,lo_SEadj))
  
  return(avg_est_tmp_wVars)
  
  predDFcomb3 <- predDFcomb2  |> left_join(avg_est_tmp_wVars)
  
  
  list("avg_est_tmp_wVars"=avg_est_tmp_wVars,
       "predDFcomb3"=predDFcomb3)
  # return(pred_DF_analytic_ls)
  
  # list(Spred_par_vals,
  #      SD_RELGRP,
  #      EST,
  #      analytical_SE)
}

HOR_CHP_pred_get_pred_all()
# HOR_CHP_pred_get_pred()

HOR_CHP_full_wrap <- function(flength_in=240,DOY_in=50:100,years_in=NULL){
  if(is.null(years_in)){
    years_in=unique(CVhelp_dat_w$Year)
  }
  
  CVhelp_dat_w_sub <-  subset(CVhelp_dat_w,DOY %in% DOY_in & Year %in% years_in)
  xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,sel_rows_tmp1=CVhelp_dat_w_sub,flength_in=flength_in,SJL_route_in = FALSE)
  # HOR_CHP_pred_get_pred(HOR_CHP_pred_comp_ls_in=HOR_CHP_pred_comp_ls,xpred_tmp_in=xpred_tmp)
  pred_DF <- HOR_CHP_pred_get_pred_all(HOR_CHP_pred_comp_ls_in=HOR_CHP_pred_comp_ls,xpred_tmp_in=xpred_tmp)[,c("Year","DOY","wt_lp_EST","se_moderr")]
  
  pred_DF_w_length <- data.frame(flength=flength_in,pred_DF)
  return(pred_DF_w_length)
}

pred_DF_mult_flength <- do.call(rbind,sapply(c(200,300,400),HOR_CHP_full_wrap,simplify = F))
pred_DF_mult_flength <- do.call(rbind,sapply(c(200,300,400),HOR_CHP_full_wrap,simplify = F,years_in=2011))


library(ggplot2)
ggplot(data=pred_DF_mult_flength |> filter(flength!=300),
       aes(y=plogis(wt_lp_EST),
           x=DOY,ymin=plogis(wt_lp_EST-(1.96*se_moderr)),
           ymax=plogis(wt_lp_EST+(1.96*se_moderr)),
           color=factor(flength),
           fill=factor(flength))) + 
  geom_ribbon(alpha=0.25,color=NA) + 
  geom_line(linewidth=.75) +
  geom_point(shape=21) +
  facet_wrap(~Year) + theme_minimal()




# HOR_CHP_full_wrap()

