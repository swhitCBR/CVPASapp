## code to prepare `HOR_CHP_pred_comp_ls` dataset goes here
devtools::load_all()

HOR_CHP_TMB_all_mods <- readRDS("../CVPAS_STH_app/output/HOR_CHP_TMB_all_mods.rds")
HOR_CHP_mod_dat_sep_ls <- readRDS("../CVPAS_STH_app/data/HOR_CHP_mod_dat_sep_ls.rds")
HOR_CHP_TMB_mod_fits_ls <- readRDS("../CVPAS_STH_app/output/HOR_CHP_TMB_mod_fits_ls.rds")

length(HOR_CHP_TMB_mod_fits_ls)

HOR_CHP_TMB_mod_fits_ls[[1]]$OPT$par



AIC_DF_d2 <- HOR_CHP_TMB_all_mods$"AIC_DF_full" |> dplyr::filter(dAIC<=2)

TMB:::getUserDLL()
# unload glmmTMB if currently loaded
pth2glmmTMB <- file.path(system.file("libs", package = "glmmTMB"),"x64")
if(TMB:::getUserDLL()=="glmmTMB") {dyn.unload(TMB::dynlib(file.path(pth2glmmTMB,"glmmTMB")))}


dyn.load(TMB::dynlib("C:/repos/CVPAS_STH_app/TMB/HOR_CHP_global"))
TMB:::getUserDLL()

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

  OBJ_pred = TMB::MakeADFun(data=TMB_data_tmp,
                            parameters=parm_in_ls,
                            DLL = "HOR_CHP_global",
                            silent=TRUE,random='RELGRP_err',
                            inner.control = list(maxit=20000))

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
usethis::use_data(HOR_CHP_pred_comp_ls, overwrite = TRUE)
