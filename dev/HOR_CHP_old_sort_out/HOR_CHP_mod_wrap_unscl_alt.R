# based on code in 'CVPAS_beta/dev/02_dev.R'

devtools::load_all()
devtools::load_all("../CVhelp")
devtools::load_all("../TMBhelp")

HOR_CHP_comp_ls_unscl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = FALSE)

# latest run
HOR_CHP_TMB_all_mods_unscl <- readRDS("../CVPAS_STH_app/output/HOR_CHP_TMB_all_mods_unscl.rds")
HOR_CHP_TMB_all_mods_unscl$TMB_data_baseline$XX_pred_mat

devtools::load_all()
# source("dev/tmp_glmm_fxns.R")
head(CVhelp_dat_w)

# create design matrix based on rows in wide format data environmental and operational data
xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_unscl,
                              sel_rows_tmp1=CVhelp_dat_w,
                              # sel_rows_tmp1=CVhelp_dat_w[1:500,],
                              flength_in=240)

attributes(HOR_CHP_comp_ls_unscl$XX_in)[[c("scaled_vars")]]
attributes(HOR_CHP_comp_ls_unscl$XX_in)

#extract center and scale for relevent parameters
unscl_var_by_tmpDF <- get_var_center_scale(TMB_mod_ls=HOR_CHP_comp_ls_unscl)
xpred_tmp_nms <- names(xpred_tmp)

# only main effects
head(HOR_CHP_TMB_all_mods_unscl$"TMB_data_baseline"$XX_pred_mat[,1:11])

# full set of predictors
tmb_bs_nm <- colnames(HOR_CHP_TMB_all_mods_unscl$"TMB_data_baseline"$XX_pred_mat)
xpred_tmp_wcols <- xpred_tmp[,match(tmb_bs_nm,xpred_tmp_nms)]

TMB:::getUserDLL()
pth2glmmTMB <- file.path(system.file("libs", package = "glmmTMB"),"x64")
if(TMB:::getUserDLL()=="glmmTMB") {dyn.unload(TMB::dynlib(file.path(pth2glmmTMB,"glmmTMB")))}
dyn.load(TMB::dynlib("C:/repos/CVPAS_STH_app/TMB/HOR_CHP_global_pred"))

TMB:::getUserDLL()

mtch_col_v <- c('(Intercept)','WYT_wet','WYT_drought','route.facB','barrier.facTRUE','flength','Tmsd.hor.7dadm','log.VNS.hor.5','SWP.hor.5','CVP.hor.5','Qomt.hor.1net','B_x_Temp','B_x_VNS','B_x_SWP','B_x_CVP','B_x_OMT','R_x_Temp','R_x_VNS','R_x_SWP','R_x_CVP','R_x_OMT')

dim(xpred_tmp[,match(mtch_col_v,colnames(xpred_tmp))])
dim(HOR_CHP_TMB_all_mods_unscl$"TMB_data_baseline"$XX_pred_mat)

str(xpred_tmp[,match(mtch_col_v,colnames(xpred_tmp))])

str(HOR_CHP_TMB_all_mods_unscl$"TMB_data_baseline"$XX_pred_mat)
str(xpred_tmp[1:6,match(mtch_col_v,colnames(xpred_tmp))])

str(as.matrix(HOR_CHP_TMB_all_mods_unscl$"TMB_data_baseline"$XX_pred_mat))
# HOR_CHP_comp_ls_scl$"TMB_data_baseline"$XX_pred_mat <- xpred_tmp[1:6,match(mtch_col_v,colnames(xpred_tmp))]

# HOR_CHP_comp_ls_scl$"TMB_data_baseline"$XX_pred_mat <- as.matrix(xpred_tmp[1:6,match(mtch_col_v,colnames(xpred_tmp))])
HOR_CHP_TMB_all_mods_unscl$"TMB_data_baseline"$XX_pred_mat <- as.matrix(xpred_tmp[,match(mtch_col_v,colnames(xpred_tmp))])

AIC_DF_d2 <-HOR_CHP_TMB_all_mods_unscl$AIC_DF_d2_full
tmb_bs_nm <- colnames(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_s)
xpred_tmp_wcols <- xpred_tmp[,match(tmb_bs_nm,xpred_tmp_nms)]

# getting unscaled predictions
ii=1

HOR_CHP_TMB_all_mods_unscl$TMB_data_baseline
HOR_CHP_TMB_all_mods  <- HOR_CHP_TMB_all_mods_unscl
HOR_CHP_TMB_mod_fits_ls <- HOR_CHP_TMB_all_mods_unscl$HOR_CHP_comp_ls

AIC_DF_d2 <- AIC_DF_d2[1:3,]

predDF_ls <- list()
bt=proc.time()
ii=1
for(ii in 1:nrow(AIC_DF_d2)){
  # extract design matrix index vector
  dm_str_val <- AIC_DF_d2$dm[ii]
  # split binary index vector
  inclu_IND_pred <- which(as.numeric(strsplit(dm_str_val,"")[[1]])!=0)
  
  # TMB_data_tmp <- HOR_CHP_comp_ls_unscl$"TMB_data_baseline"
  TMB_data_tmp <- HOR_CHP_TMB_all_mods_unscl$"TMB_data_baseline"
  # TMB_data_tmp <- HOR_CHP_comp_ls_unscl$"TMB_data_baseline"
  TMB_data_tmp$"XX_s" <- TMB_data_tmp$"XX_s"[,inclu_IND_pred]
  TMB_data_tmp$"XX_pred_mat" <- TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred]
  
  head(TMB_data_tmp$"XX_pred_mat")
  head(xpred_tmp_wcols[,inclu_IND_pred])
  
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
    "RELGRP_err"=HOR_CHP_TMB_mod_fits_ls[[ii]]$RELGRP_err_v,
    "logSD_RELGRP"=logSD_RELGRP_tmp,
    "P_pars"=matrix(Ppar_tmp,nrow=length(HOR_CHP_TMB_all_mods$TMB_data_baseline$col_P11),
                    ncol=HOR_CHP_TMB_all_mods$TMB_data_baseline$P_lc_n))
  OBJ_pred = TMB::MakeADFun(#data=c(model="HOR_CHP_global_pred",TMB_data_tmp),
                            data=TMB_data_tmp,
                            parameters=parm_in_ls,
                            DLL = "HOR_CHP_global_pred",
                            silent=TRUE,random='RELGRP_err',
                            inner.control = list(maxit=20000))
  # OBJ_pred$gr()
  # OBJ_pred$fn()
  summ_out_for_pred <- summary(TMB::sdreport(obj = OBJ_pred,
                                             getReportCovariance = T,
                                             bias.correct = F,
                                             skip.delta.method = F,
                                             ignore.parm.uncertainty = F))
  predDF <- data.frame(id=ii,rw=1:nrow(TMB_data_tmp$"XX_pred_mat"),
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

library(dplyr)

get_aic_wts <- function(df, aic_column) {
  delta_aic <- df[[aic_column]] - min(df[[aic_column]], na.rm = TRUE)
  rel_likelihood <- exp(-0.5 * delta_aic)
  weights <- rel_likelihood / sum(rel_likelihood, na.rm = TRUE)
  return(weights)
}
AIC_DF_d2$AICwt <- get_aic_wts(AIC_DF_d2,"AIC")


predDFcomb <- do.call(rbind,predDF_ls)
predDFcomb$wt=AIC_DF_d2$AICwt[predDFcomb$id]
predDFwtavgDF <-  predDFcomb |> 
  dplyr::group_by(rw) |>
  dplyr::summarize(
    EST_lp=sum(EST_lp*wt),
    EST=sum(EST*wt),
    LCL=min(LCL),
    UCL=max(UCL),
    lo_SEadj=mean(lo_SEadj), # fix later
    # pLCL=min(pLCL),
    # pUCL=max(pUCL),
    wtsum=sum(wt)) |> 
  dplyr::mutate(
      pEST=plogis(EST_lp),
      pSE=pEST*(1-pEST)*lo_SEadj,
      pLCL=plogis(LCL),
      pUCL=plogis(UCL)
    )

nrow(predDFcomb)

library(ggplot2)



predDFcomb_wVars <- data.frame(predDFcomb,CVhelp_dat_w)



ggplot(data=predDFcomb_wVars,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
  # ,color=id,fill=id)) +
  geom_ribbon(alpha=0.25) + geom_point() + facet_wrap(~Year)





ggplot(data=predDFcomb,aes(y=plogis(EST),x=rw,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
                           # ,color=id,fill=id)) +
  geom_ribbon(alpha=0.25) + geom_point()


ggplot(data=predDFwtavgDF,aes(y=pEST,x=rw,ymin=pLCL,ymax=pUCL)) + geom_ribbon() + geom_point()

plot(pEST~rw,predDFwtavgDF)


# predDFwtavgDF_comb <- data.frame(xpred_tmp[1:6,],predDFwtavgDF)
