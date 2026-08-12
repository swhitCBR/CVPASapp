# based on code in 'CVPAS_beta/dev/02_dev.R'

devtools::load_all()

# bad credentials
# devtools::install_github("https://github.com/swhitCBR/CVhelp",auth_token = "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")
# devtools::install_github("https://github.com/swhitCBR/TMBhelp",auth_token = "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")

devtools::load_all("../CVhelp")
devtools::load_all("../TMBhelp")

# devtools::install_github("https://github.com/swhitCBR/CVhelp")
# devtools::install_github("https://github.com/swhitCBR/TMBhelp")

# HOR_CHP_comp_ls_scl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = TRUE)

HOR_CHP_comp_ls_unscl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = FALSE)
# HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat <- HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_s[1:10,]
# 
# str(HOR_CHP_comp_ls_scl)
# names(HOR_CHP_comp_ls_scl)
# HOR_CHP_comp_ls_unscl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = TRUE)

# head(HOR_CHP_comp_ls_scl$XX_in)
head(HOR_CHP_comp_ls_unscl$XX_in)

# HOR_CHP_d2_unscl_supp_ls <- readRDS("../CVPAS_STH_app/output/HOR_CHP_d2_unscl_supp_ls.rds")
# str(HOR_CHP_d2_unscl_supp_ls)

devtools::load_all()
# source("dev/tmp_glmm_fxns.R")
head(CVhelp_dat_w)

# create design matrix based on rows in wide format data environmental and operational data
xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_unscl,
                              sel_rows_tmp1=CVhelp_dat_w[1:5,],
                              flength_in=240)
# View(CVhelp_dat_w)
# View(xpred_tmp)

attributes(HOR_CHP_comp_ls_unscl$XX_in)[[c("scaled_vars")]]
attributes(HOR_CHP_comp_ls_unscl$XX_in)

# source("R/get_var_center_scale.R")

#extract center and scale for relevent parameters
unscl_var_by_tmpDF <- get_var_center_scale(TMB_mod_ls=HOR_CHP_comp_ls_unscl)

xpred_tmp_nms <- names(xpred_tmp)

# only main effects
# head(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat[,1:11])
# tmb_bs_nm <- colnames(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat[,1:11])

# full set of predictors
tmb_bs_nm <- colnames(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat)

xpred_tmp_wcols <- xpred_tmp[,match(tmb_bs_nm,xpred_tmp_nms)]

# scl_var_by_tmpDF
# xpred_tmp_wcols
# xpred_tmp

TMB:::getUserDLL()
# unload glmmTMB if currently loaded
pth2glmmTMB <- file.path(system.file("libs", package = "glmmTMB"),"x64")
if(TMB:::getUserDLL()=="glmmTMB") {dyn.unload(TMB::dynlib(file.path(pth2glmmTMB,"glmmTMB")))}
dyn.load(TMB::dynlib("../CVPAS_beta/src/TMB/CVPASbeta_TMBExports"))
TMB:::getUserDLL()

# paste0(c("c('",paste0(colnames(xpred_tmp),collapse="','"),"')"),collapse="")
# paste0(c("c('",paste0(colnames(HOR_CHP_comp_ls_scl$"TMB_data_baseline"$XX_pred_mat),collapse="','"),"')"),collapse="")
# mtch_col_v <-   c('(Intercept)','date','Year','DOY','WYT','ORB','MID','Qomt.hor.1net','barrier.facTRUE','OUT','EXPORTS','X2','CLC','log.VNS.hor.5','SWP.hor.5','CVP.hor.5','Tmsd.hor.7dadm','WYT_drought','WYT_wet','flength','route.facB','YrRel','B_x_Temp','B_x_VNS','B_x_SWP','B_x_CVP','B_x_OMT','R_x_Temp','R_x_VNS','R_x_SWP','R_x_CVP','R_x_OMT')
# match(colnames(HOR_CHP_comp_ls_scl$"TMB_data_baseline"$XX_pred_mat),colnames(xpred_tmp))

mtch_col_v <- c('(Intercept)','WYT_wet','WYT_drought','route.facB','barrier.facTRUE','flength','Tmsd.hor.7dadm','log.VNS.hor.5','SWP.hor.5','CVP.hor.5','Qomt.hor.1net','B_x_Temp','B_x_VNS','B_x_SWP','B_x_CVP','B_x_OMT','R_x_Temp','R_x_VNS','R_x_SWP','R_x_CVP','R_x_OMT')

dim(xpred_tmp[,match(mtch_col_v,colnames(xpred_tmp))])

# HOR_CHP_comp_ls_unscl
# dim(HOR_CHP_comp_ls_scl$"TMB_data_baseline"$XX_pred_mat)

str(xpred_tmp[,match(mtch_col_v,colnames(xpred_tmp))])

# HOR_CHP_comp_ls_unscl$TMB_data_baseline
# str(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat)
# str(xpred_tmp[1:6,match(mtch_col_v,colnames(xpred_tmp))])

HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat <- as.matrix(xpred_tmp[1:5,match(mtch_col_v,colnames(xpred_tmp))])

str(as.matrix(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat))
# str(as.matrix(HOR_CHP_comp_ls_scl$"TMB_data_baseline"$XX_pred_mat))
# HOR_CHP_comp_ls_scl$"TMB_data_baseline"$XX_pred_mat <- xpred_tmp[1:6,match(mtch_col_v,colnames(xpred_tmp))]

# HOR_CHP_comp_ls_unscl$
# HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat <- as.matrix(xpred_tmp[1:6,match(mtch_col_v,colnames(xpred_tmp))])

HOR_CHP_TMB_all_mods_unscl <- readRDS("../CVPAS_STH_app/output/HOR_CHP_TMB_all_mods_unscl.rds")

AIC_DF_d2 <- HOR_CHP_TMB_all_mods_unscl$AIC_DF_d2_full


HOR_CHP_TMB_all_mods <- HOR_CHP_TMB_all_mods_unscl

# getting unscaled predictions
ii=1

HOR_CHP_TMB_mod_fits_ls <- readRDS("../CVPAS_beta/src/HOR_CHP_TMB_mod_fits_ls.rds")
# length(HOR_CHP_TMB_mod_fits_ls)
# HOR_CHP_TMB_all_mods$allint_DMs
# which(HOR_CHP_TMB_all_mods$allint_DMs=="111111111110000000000")
predDF_ls <- list()
bt=proc.time()
for(ii in 1:nrow(AIC_DF_d2)){
  # extract design matrix index vector
  dm_str_val <- AIC_DF_d2$dm[ii]
  # split binary index vector
  inclu_IND_pred <- which(as.numeric(strsplit(dm_str_val,"")[[1]])!=0)
  
  TMB_data_tmp <- HOR_CHP_comp_ls_unscl$"TMB_data_baseline"
  # TMB_data_tmp <- HOR_CHP_comp_ls_unscl$"TMB_data_baseline"
  TMB_data_tmp$"XX_s" <- TMB_data_tmp$"XX_s"[,inclu_IND_pred]
  TMB_data_tmp$"XX_pred_mat" <- TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred]
  
  head(TMB_data_tmp$"XX_pred_mat")
  # head(xpred_tmp_wcols[,inclu_IND_pred])
  
  # HOR_CHP_TMB_all_mods
  # starting values
  par_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val)$coeff_param
  # survival parameters
  Spar_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & !is.na(S_parID))$coeff_param
  # detection and lambda parameters
  Ppar_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & par_nm=="P_pars")$coeff_param
  # random error term
  logSD_RELGRP_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & par_nm=="logSD_RELGRP")$coeff_param
  # TMB_data_baseline_pred_tmp
  parm_in_ls <- list(
    "S_pars"=c(Spar_tmp),
    "RELGRP_err"=HOR_CHP_TMB_all_mods$HOR_CHP_comp_ls[[ii]]$RELGRP_err_v,
    "logSD_RELGRP"=logSD_RELGRP_tmp,
    "P_pars"=matrix(Ppar_tmp,nrow=length(HOR_CHP_TMB_all_mods$TMB_data_baseline$col_P11),
                    ncol=HOR_CHP_TMB_all_mods$TMB_data_baseline$P_lc_n))
  
  OBJ_pred = TMB::MakeADFun(data=c(model="HOR_CHP_global_pred",TMB_data_tmp),
                            parameters=parm_in_ls,
                            DLL = "CVPASbeta_TMBExports",
                            silent=TRUE,random='RELGRP_err',
                            inner.control = list(maxit=20000))
  # OBJ_pred$gr()
  # OBJ_pred$fn()
  summ_out_for_pred <- summary(TMB::sdreport(obj = OBJ_pred,
                                             getReportCovariance = T,
                                             bias.correct = F,
                                             skip.delta.method = F,
                                             ignore.parm.uncertainty = F))
  predDF <- data.frame(id=ii,rw=1:nrow(HOR_CHP_comp_ls_unscl$TMB_data_baseline$XX_pred_mat),
                       summ_out_for_pred[rownames(summ_out_for_pred)=="lp_Spred_uncond",])
  
  # predDF <- data.frame(id=ii,rw=1:nrow(HOR_CHP_comp_ls_unscl$TMB_data_baseline$XX_pred_mat),
  #                      summ_out_for_pred[rownames(summ_out_for_pred)=="lp_Spred_uncond",])
  names(predDF) <- c("id","rw","EST_lp","SE_lp"); rownames(predDF) <- NULL
  # head(predDF)
  predDF$SigmaREL_lp=exp(OBJ_pred$par["logSD_RELGRP"])
  predDF$comb_var=sqrt((predDF$SE_lp^2)+(predDF$SigmaREL_lp^2)) # seemingly mislabeled
  predDF$lo_pred=predDF$EST_lp
  predDF$lo_SE=predDF$SE_lp
  predDF$lo_SEadj=sqrt((predDF$SE_lp^2)+(predDF$SigmaREL_lp^2))
  predDF$LCL=predDF$lo_pred-1.96*predDF$lo_SE
  predDF$UCL=predDF$lo_pred+1.96*predDF$lo_SE
  predDF$LCLadj=predDF$lo_pred-1.96*predDF$lo_SEadj
  predDF$UCLadj=predDF$lo_pred+1.96*predDF$lo_SEadj
  predDF$EST=predDF$EST_lp
  predDF_ls[[ii]] <- predDF
} # 30 seconds to compute all preds
proc.time()-bt # 16 seconds 

predDF_ls



predDFcomb <- do.call(rbind,predDF_ls)
predDFcomb$wt=AIC_DF_d2$AICwt[predDFcomb$id]
predDFwtavgDF <-  predDFcomb %>% 
  group_by(rw) %>%
  summarize(
    EST_lp=sum(EST_lp*wt),
    EST=sum(EST*wt),
    LCL=min(LCL),
    UCL=max(UCL),
    lo_SEadj=mean(lo_SEadj), # fix later
    # pLCL=min(pLCL),
    # pUCL=max(pUCL),
    wtsum=sum(wt)) %>% mutate(
      pEST=plogis(EST_lp),
      pSE=pEST*(1-pEST)*lo_SEadj,
      pLCL=plogis(LCL),
      pUCL=plogis(UCL)
    )

nrow(predDFcomb)

predDFwtavgDF_comb <- data.frame(xpred_tmp[1:6,],predDFwtavgDF)
