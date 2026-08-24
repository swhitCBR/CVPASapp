
TCJ_CHP_get_pred <- function(
    TCJ_CHP_pred_comp_ls_in,#=HOR_CHP_pred_comp_ls,
    xpred_tmp_in=xpred_tmp,
    avg_onlyTF=TRUE#,
    # cov_mat_ls_in
    ){
  
  AIC_DF_d2=TCJ_CHP_pred_comp_ls_in$"AIC_DF_d2"
  
  
  AIC_DF_d2$wt <- get_aic_wts(df = AIC_DF_d2,aic_column = "AIC")
  
  
  # mods_w_covmat_ls=TCJ_CHP_pred_comp_ls_in$"mods_w_covmat_ls"
  # mods_w_covmat_ls=TCJ_CHP_pred_comp_ls_in$"mods_w_covmat_ls"
  # mods_w_covmat_ls <- cov_mat_ls_in
  
  mods_w_covmat_ls <- TCJ_CHP_pred_comp_ls_in$"cov_mat_ls"
  
  pt_estsDF_ls=TCJ_CHP_pred_comp_ls_in$"pt_estsDF_l"
  # pt_estsDF_ls=TCJ_CHP_pred_comp_ls_in$"TCJ_CHP_TMB_all_mods"$"pt_estsDF_l"
  # logSD_RELGRP_v <- TCJ_CHP_pred_comp_ls_in$"logSD_RELGRP_v"
  logSD_RELGRP_v <- TCJ_CHP_pred_comp_ls_in$"logSD_RELGRP_v"
  xpred_tmp <- xpred_tmp_in
  
  
  jj=1 
  pred_DF_analytic_ls <- list()
  for(jj in 1:nrow(AIC_DF_d2)){
    
    # row index for survival paramters
    ind_sconf <- which(rownames(mods_w_covmat_ls[[jj]])=="S_pars")
    # print(ind_sconf)
    
    Spred_varnames <- pt_estsDF_ls[[jj]]$par_nm[ind_sconf]
    mtch_ind <- match(Spred_varnames,colnames(xpred_tmp))
    new_x <- as.matrix(xpred_tmp[,mtch_ind])
    
    # estimated parameters
    # Spred_par_vals <- pt_estsDF_ls[[jj]]$coeff_param[ind_sconf] 
    
    Spred_par_vals <- pt_estsDF_ls[[jj]]$"S_coef"[ind_sconf] 
    
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
  # pred_DF_analytic_comb$wt=AIC_DF_d2$AICwt[pred_DF_analytic_comb$id]
  pred_DF_analytic_comb$wt=AIC_DF_d2$wt[pred_DF_analytic_comb$id]
  
  in_avg_tb <-  pred_DF_analytic_comb |>
    dplyr::group_by(rw) |>
    dplyr::summarize(
      wt_lp_EST=sum(EST*wt))
  
  # adding the model selection uncertainty to the estimate
  predDFcomb2 <- pred_DF_analytic_comb |> dplyr::left_join(in_avg_tb,by = "rw") |> 
    dplyr::mutate(
      diff_modavg=(EST-wt_lp_EST),
      SS_modavg=(EST-wt_lp_EST)^2,
      lo_SE_modavg=sqrt(SS_modavg+(SEadj)^2))
  
  full_var_predDF <- predDFcomb2 |>
    dplyr::select(id,rw,EST,wt,wt_lp_EST,lo_SE_modavg) |>
    dplyr::group_by(rw) |>
    dplyr::summarize(
      var_modavg=sum(wt*lo_SE_modavg),
      se_moderr=sqrt(var_modavg))
  
  avg_est_tmp <- in_avg_tb |> dplyr::left_join(full_var_predDF,by = "rw")
  xpred_tmp$rw=1:nrow(xpred_tmp)
  avg_est_tmp_wVars <- xpred_tmp |> dplyr::left_join(avg_est_tmp,by = "rw")# |> dplyr::left_join(predDFcomb2 |> select(rw,id,lo_pred,lo_SE,lo_SEadj))
  
  
  
  predDFcomb3 <- predDFcomb2  |> dplyr::left_join(avg_est_tmp_wVars |> dplyr::select(rw,wt_lp_EST,se_moderr),by = "rw")
  
  if(avg_onlyTF){return(avg_est_tmp_wVars)}
  
  out <- list("avg_est_tmp_wVars"=avg_est_tmp_wVars,
              "comb_est_tmp-wVars"=predDFcomb3,
              "AIC_tab"=AIC_DF_d2)
  
  return(out)

}
