devtools::load_all()

HOR_CHP_TMB_all_mods <- readRDS("../CVPAS_STH_app/output/HOR_CHP_TMB_all_mods.rds")
HOR_CHP_mod_dat_sep_ls <- readRDS("../CVPAS_STH_app/data/HOR_CHP_mod_dat_sep_ls.rds")
# HOR_CHP_TMB_mod_fits_ls_b <- readRDS("../CVPAS_beta/src/HOR_CHP_TMB_mod_fits_ls.rds")
HOR_CHP_TMB_mod_fits_ls <- readRDS("../CVPAS_STH_app/output/HOR_CHP_TMB_mod_fits_ls.rds")

length(HOR_CHP_TMB_mod_fits_ls)

# HOR_CHP_TMB_mod_fits_ls_b[[1]]$OPT$par
HOR_CHP_TMB_mod_fits_ls[[1]]$OPT$par



AIC_DF_d2 <- HOR_CHP_TMB_all_mods$"AIC_DF_full" |> dplyr::filter(dAIC<=2)

# HOR_CHP_TMB_all_mods$TMB_data_baseline

TMB:::getUserDLL()
# unload glmmTMB if currently loaded
pth2glmmTMB <- file.path(system.file("libs", package = "glmmTMB"),"x64")
if(TMB:::getUserDLL()=="glmmTMB") {dyn.unload(TMB::dynlib(file.path(pth2glmmTMB,"glmmTMB")))}
# dyn.load(TMB::dynlib("../CVPAS_beta/src/TMB/CVPASbeta_TMBExports"))


dyn.load(TMB::dynlib("C:/repos/CVPAS_STH_app/TMB/HOR_CHP_global"))


ii=1
predDF_ls <- list()
sigma_beta_ls <- list()
pt_estsDF_ls <- list()
mods_w_covmat_ls <- list()
pt_ests_SE_DF_ls <- list()
logSD_RELGRP_v <- c()
bt=proc.time()
for(ii in 1:nrow(AIC_DF_d2)){
  
  dm_str_val <- AIC_DF_d2$dm[ii]
  inclu_IND_pred <- which(as.numeric(strsplit(dm_str_val,"")[[1]])!=0)
  
  # TMB_data_tmp <- HOR_CHP_comp_ls_scl$"TMB_data_baseline"
  TMB_data_tmp <- HOR_CHP_TMB_all_mods$TMB_data_baseline
  TMB_data_tmp$"XX_s" <- TMB_data_tmp$"XX_s"[,inclu_IND_pred]
  TMB_data_tmp$"XX_pred_mat" <- TMB_data_tmp$"XX_s"[1:10,]

  # HOR_CHP_TMB_all_mods
  # starting values
  par_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val)$coeff_param
  # survival parameters
  Spar_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & !is.na(S_parID))$coeff_param
  # detection and lambda parameters
  Ppar_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & par_nm=="P_pars")$coeff_param
  # random error term
  logSD_RELGRP_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & par_nm=="logSD_RELGRP")$coeff_param
  
  parm_in_ls <- list(
    "S_pars"=c(Spar_tmp),
    "RELGRP_err"=HOR_CHP_TMB_mod_fits_ls[[ii]]$RELGRP_err_v,
    "logSD_RELGRP"=logSD_RELGRP_tmp,
    "P_pars"=matrix(
      Ppar_tmp,nrow=length(HOR_CHP_TMB_all_mods$TMB_data_baseline$col_P11),
      ncol=HOR_CHP_TMB_all_mods$TMB_data_baseline$P_lc_n))
  
  # OBJ_pred = TMB::MakeADFun(data=c(model="HOR_CHP_global_pred",TMB_data_tmp),
  #                           parameters=parm_in_ls,
  #                           DLL = "CVPASbeta_TMBExports",
  #                           silent=TRUE,random='RELGRP_err',
  #                           inner.control = list(maxit=20000))
  
  OBJ_pred = TMB::MakeADFun(data=TMB_data_tmp,
                            #data=c(model="HOR_CHP_global_pred",TMB_data_tmp),
                            parameters=parm_in_ls,
                            DLL = "HOR_CHP_global",
                            # DLL = "C:/repos/CVPAS_STH_app/TMB/HOR_CHP_global",
                            silent=TRUE,random='RELGRP_err',
                            inner.control = list(maxit=20000))
  
  # HOR_CHP_frst3_mod_ls_unscl <- fit_nested_tmb_mods(
  #   S_design_matrix_in = allint_DMs[c(1:3)],
  #   dir_in ="C:/repos/CVPAS_STH_app/TMB",
  #   TMB_data_baseline_in=HOR_CHP_comp_ls_unscl$"TMB_data_baseline",
  #   calc_SEs=FALSE,cpp_name_in = "HOR_CHP_global")
  
  SD_obj <- TMB::sdreport(obj = OBJ_pred,
                          getReportCovariance = T,
                          bias.correct = F,
                          skip.delta.method = F,
                          ignore.parm.uncertainty = F,
                          getJointPrecision = TRUE)
  
  cov_matrix <- solve(SD_obj$jointPrecision)
  
  mods_w_covmat_ls[[ii]] <- cov_matrix
  pt_estsDF_ls[[ii]] <- subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val)
  logSD_RELGRP_v[ii] <- logSD_RELGRP_tmp
  
  pt_ests_SE_DF_ls[[ii]] <- data.frame(ii=ii,subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val),SE=  sqrt(diag(SD_obj$cov.fixed)))
}

AIC_DF_d2$AICwt <- get_aic_wts(AIC_DF_d2,"AIC")

HOR_CHP_pred_comp_ls <- list(
   "AIC_DF_d2"=AIC_DF_d2,
   "mods_w_covmat_ls"=mods_w_covmat_ls,
   "pt_estsDF_ls"=pt_estsDF_ls,
   "logSD_RELGRP_v"=logSD_RELGRP_v,
   "TMB_data_baseline"=HOR_CHP_TMB_all_mods$"TMB_data_baseline",
   "data_inputs_ls"=HOR_CHP_mod_dat_sep_ls,
   "pt_ests_SE_DF_ls"=pt_ests_SE_DF_ls
   )

# usethis::use_data_raw("HOR_CHP_pred_comp_ls")
usethis::use_data(HOR_CHP_pred_comp_ls)
# 
# 
# # Getting new data 
# devtools::load_all("../CVhelp")
# CVhelp_dat_w_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide",DOY_rng = 1:250)
# HOR_CHP_comp_ls_scl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = TRUE)
# 
# # THIS SCRIPT IS PROBABLY OUT OF DATE
# 
# #  OLD
# # xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
# #                               sel_rows_tmp1=CVhelp_dat_w_alt,
# #                               flength_in=240)
# # REPLACED HOR_CHP_mod_wrap() with HOR_CHP_DM_scl()
# xpred_tmp <- HOR_CHP_DM_scl(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
#                               sel_rows_tmp1=CVhelp_dat_w_alt,
#                               flength_in=240)
# 
# 
# jj=1 
# pred_DF_analytic_ls <- list()
# for(jj in 1:nrow(AIC_DF_d2)){
#   
#   # row index for survival paramters
#   ind_sconf <- which(rownames(mods_w_covmat_ls[[1]])=="S_pars")
#   
#   Spred_varnames <- pt_estsDF_ls[[jj]]$par_nm[ind_sconf]
#   mtch_ind <- match(Spred_varnames,colnames(xpred_tmp))
#   new_x <- as.matrix(xpred_tmp[,mtch_ind])
#   
#   # estimated parameters
#   Spred_par_vals <- pt_estsDF_ls[[jj]]$coeff_param[ind_sconf] 
#   Sigma_mat_S <- mods_w_covmat_ls[[jj]][ind_sconf,ind_sconf]
#   
#   # linear predictor
#   EST <- as.vector(new_x  %*% Spred_par_vals)
#   analytical_SE <- sqrt(rowSums((new_x %*% Sigma_mat_S) * new_x))
# 
#   # SD for random intercept
#   SD_RELGRP <- exp(logSD_RELGRP_v[jj])
#   
#   pred_DF_analytic <- data.frame(
#     rw=1:nrow(xpred_tmp),
#     id=jj,
#     xpred_tmp,
#     EST,
#     SE=analytical_SE,
#     LCL=EST-analytical_SE*1.96,
#     UCL=EST+analytical_SE*1.96,
#     SEadj=sqrt(analytical_SE^2 + exp(logSD_RELGRP_v[jj])^2)
#     )
#   
#   pred_DF_analytic$LCLadj=pred_DF_analytic$EST-pred_DF_analytic$SEadj*1.96
#   pred_DF_analytic$UCLadj=pred_DF_analytic$EST+pred_DF_analytic$SEadj*1.96
#   pred_DF_analytic_ls[[jj]] <- pred_DF_analytic
# }
# 
# 
# pred_DF_analytic_comb <- do.call(rbind,pred_DF_analytic_ls)
# 
# library(ggplot2)
# ggplot() +
#   geom_line(data=pred_DF_analytic_comb, 
#             aes(y=plogis(EST),x=DOY,color=factor(id))) + facet_wrap(~Year)
# 
# pred_DF_analytic_comb
# pred_DF_analytic_comb$wt=AIC_DF_d2$AICwt[pred_DF_analytic_comb$id]
# 
# in_avg_tb <-  pred_DF_analytic_comb |>
#   dplyr::group_by(rw) |>
#   # filter(!(id %in% c(3,5,6))) |>
#   dplyr::summarize(
#     wt_lp_EST=sum(EST*wt))
# 
# # adding the model selection uncertainty to the estimate
# predDFcomb2 <- pred_DF_analytic_comb |> left_join(in_avg_tb) |> 
#   # filter(!(id %in% c(3,5,6))) |>
#   mutate(
#     diff_modavg=(EST-wt_lp_EST),
#     SS_modavg=(EST-wt_lp_EST)^2,
#     lo_SE_modavg=sqrt(SS_modavg+(SEadj)^2))
# 
# full_var_predDF <- predDFcomb2 |>
#   # filter(!(id %in% c(3,5,6))) |> # dumb models
#   select(id,rw,EST,wt,wt_lp_EST,lo_SE_modavg) |>
#   dplyr::group_by(rw) |>
#   dplyr::summarize(
#     var_modavg=sum(wt*lo_SE_modavg),
#     se_moderr=sqrt(var_modavg))
# 
# avg_est_tmp <- in_avg_tb |> left_join(full_var_predDF)
# xpred_tmp$rw=1:nrow(xpred_tmp)
# avg_est_tmp_wVars <- xpred_tmp |> left_join(avg_est_tmp)# |> left_join(predDFcomb2 |> select(rw,id,lo_pred,lo_SE,lo_SEadj))
# predDFcomb3 <- predDFcomb2  |> left_join(avg_est_tmp_wVars)
# 
# 
# ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
#        ,aes(y=plogis(EST),x=DOY,color=factor(id))) +#,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))
#   geom_line() + ggtitle("prob_scale") +
#   facet_wrap(~Year) + scale_y_continuous(limits=c(0,1))
# 
# ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
#        ,aes(y=EST,x=DOY,color=factor(id))) +#,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))
#   geom_line() + ggtitle("logit_scale") +
#   facet_wrap(~Year) #+ scale_y_continuous(limits=c(0,1))
# 
# 
# 
# ggplot(data=avg_est_tmp_wVars,
#        aes(y=plogis(wt_lp_EST),
#            x=DOY,ymin=plogis(wt_lp_EST-(1.96*se_moderr)),
#            ymax=plogis(wt_lp_EST+(1.96*se_moderr)))) + 
#   geom_ribbon(fill="gray50") + geom_point() + facet_wrap(~Year)
# 
# 
# # Combined estimates
# ggplot() +
#   geom_ribbon(data=avg_est_tmp_wVars,
#               aes(y=plogis(wt_lp_EST),
#                   x=DOY,ymin=plogis(wt_lp_EST-(1.96*se_moderr)),
#                   ymax=plogis(wt_lp_EST+(1.96*se_moderr))))+
#   geom_ribbon(data=pred_DF_analytic_comb #%>% filter(!(id %in% c(3,5,6)))
#               ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id)),alpha=0.25,color=NA) +# geom_point() +
#   geom_line(data=pred_DF_analytic_comb #%>% filter(!(id %in% c(3,5,6)))
#             ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
#   geom_line(data=avg_est_tmp_wVars,
#             aes(y=plogis(wt_lp_EST),
#                 x=DOY,ymin=plogis(wt_lp_EST-(1.96*se_moderr)),
#                 ymax=plogis(wt_lp_EST+(1.96*se_moderr))),linewidth=2) +
#   facet_wrap(~Year) + scale_y_continuous(limits=c(0,1))
