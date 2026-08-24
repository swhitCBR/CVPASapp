## code to prepare `TCJ_CHP_pred_comp_ls` dataset goes here

devtools::load_all("../TMBhelp")
# devtools::load_all("../CVhelp")
TCJ_CHP_TMB_all_mods <- readRDS("../CVPAS_STH_app/output/TCJ_CHP_TMB_all_mods.rds")

# loading functions that def work
source("C:/repos/CVPAS_STH_app/TCJ_CHP_cust_fxns.R")

load("../CVPAS_STH_app/data/TCJ_CHP_mod_dat_ls.RData")

# rlang::env_get_list()
# rlang::env_get(nm = ".dll")
# TMB:::getUserDLL()
# dyn.load(dyn.unload("TMB/TCJ_CHP_global"))
# 20 to 30
# TCJ_CHP_TMB_all_mods$AIC_DF_full[which(TCJ_CHP_TMB_all_mods$AIC_DF_full$dm=="111111111110000000000"),]
AIC_DF_full <- TCJ_CHP_TMB_all_mods$"AIC_DF_full"

AIC_DF_d2 <- TCJ_CHP_TMB_all_mods$"AIC_DF_full" |> dplyr::filter(dAIC<=2)
AIC_DF_d2 <- AIC_DF_d2 |> dplyr::mutate(candmodID=match(dm,TCJ_CHP_TMB_all_mods$allint_DMs))
# consider get_aic_wts()
AIC_DF_full$AICwt <- exp(-0.5*AIC_DF_full$dAIC)/sum(exp(-0.5*AIC_DF_full$dAIC))

# devtools::load_all("../TMBhelp")
TCJ_CHP_mod_fits_d2_ls <- fit_nested_tmb_mods_MOD(
  S_design_matrix_in = AIC_DF_d2$dm, 
  dir_in ="C:/repos/CVPAS_STH_app/TMB",
  TMB_data_baseline_in=TCJ_CHP_TMB_all_mods$"TMB_data_baseline",
  calc_SEs=TRUE,
  # cpp_name_in = "TCJ_CHP_global"
  cpp_name_in = "HOR_CHP_global"
)

TCJ_CHP_mod_fits_d2_ls$`1111110010101`$COV_mat
SD_obj <- TCJ_CHP_mod_fits_d2_ls$`1111110010101`$COV_mat
cov_matrix <- solve(SD_obj$jointPrecision)

# saveRDS(TCJ_CHP_mod_fits_d2_ls,"TCJ_CHP_mod_fits_d2_ls.rds")


# usethis::use_data_raw("TCJ_CHP_pred_comp_ls")


# TCJ_CHP_mod_fits_d2_ls[[1]]$est_tab$Estimate
S_coef_ls_confset <- lapply(1:length(TCJ_CHP_mod_fits_d2_ls),function(ii){
  aa=TCJ_CHP_mod_fits_d2_ls[[ii]]$"est_tab"
  S_coef=aa$"Estimate_SE"[aa$"Parameter"=="S_pars"]
  est_tmp=aa$"Estimate"[aa$"Parameter"=="S_pars"]
  se_tmp=aa$"Std..Error"[aa$"Parameter"=="S_pars"]
  c=data.frame(AICrank=ii,
               candmodID=AIC_DF_d2$candmodID[ii],
               dm=AIC_DF_d2$dm[ii],
               par_nm=names(TCJ_CHP_TMB_all_mods$"par_nm_ind_ls"[[AIC_DF_d2$candmodID[ii]]]),
               S_parID=TCJ_CHP_TMB_all_mods$"par_nm_ind_ls"[[AIC_DF_d2$candmodID[ii]]],
               S_coef,
               estimate=est_tmp,
               SE=se_tmp)
  rownames(c)=NULL
  return(c)})

S_coef_confset_DF <- do.call(rbind,S_coef_ls_confset)
S_conf_ls <- list(
  "AIC_DF_d2"=AIC_DF_d2,
  "S_coef_confset_DF"=S_coef_confset_DF)

# TCJ_CHP_TMB_all_mods

TCJ_CHP_comp_ls_unscl <- TCJ_CHP_comp(z_scale_vars = F,
                                      lvec.tcj_ls_in = lvec.tcj_ls,
                                      sdat.det.in=sdat.det.common2,
                                      x.df = x.df)



# TCJ_CHP_TMB_all_mods$pt_estsDF$dm

logSD_RELGRP_v <- as.numeric(sapply(TCJ_CHP_mod_fits_d2_ls,function(xx) xx$OPT$par["logSD_RELGRP"]))

pt_estsDF_l <- list()
for(ii in 1:nrow(AIC_DF_d2)){
  ind <- TCJ_CHP_TMB_all_mods$pt_estsDF$dm==AIC_DF_d2$dm[ii]
  # print(TCJ_CHP_TMB_all_mods$pt_estsDF[ind,])
  pt_estsDF_l[[ii]] <- TCJ_CHP_TMB_all_mods$pt_estsDF[ind,]
}




cov_mat_ls <- lapply(TCJ_CHP_mod_fits_d2_ls,function(xx) solve(xx$COV_mat$jointPrecision))

# pt_estsDF_ls[[ii]] <- subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val)
# dyn.load(dyn.unload("TMB/TCJ_CHP_global"))
# saveRDS(S_conf_ls,"output/TCJ_CHP_S_conf_ls.rds")
TCJ_CHP_pred_comp_ls <- list(
  "AIC_DF_d2"=AIC_DF_d2,
  "logSD_RELGRP_v"=logSD_RELGRP_v,
  "TCJ_CHP_mod_fits_d2_ls"=TCJ_CHP_mod_fits_d2_ls,
  "TCJ_CHP_TMB_all_mods"=TCJ_CHP_TMB_all_mods,
  "TCJ_CHP_comp_ls_unscl"=TCJ_CHP_comp_ls_unscl,
  "pt_estsDF_l"=pt_estsDF_l,
  "cov_mat_ls" = cov_mat_ls,
  "lvec.tcj_ls"=lvec.tcj_ls,
  "sdat.det.common2"=sdat.det.common2,
  "x.df"=x.df)
names(TCJ_CHP_pred_comp_ls)


TCJ_CHP_pred_comp_ls$TCJ_CHP_TMB_all_mods$pt_estsDF
TCJ_CHP_pred_comp_ls$TCJ_CHP_TMB_all_mods



# problems here
# saveRDS(TCJ_CHP_pred_comp_ls,"C:/repos/CVPAS_STH_app/OUTPUT/TCJ_CHP_pred_comp_ls.rds")
# TCJ_CHP_pred_comp_ls <- readRDS("C:/repos/CVPAS_STH_app/OUTPUT/TCJ_CHP_pred_comp_ls.rds")

# setwd("../CVPASapp")
usethis::use_data(TCJ_CHP_pred_comp_ls, overwrite = TRUE)
devtools::document()
