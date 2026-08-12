# based on code in 'CVPAS_beta/dev/02_dev.R'

devtools::load_all()

# bad credentials
# devtools::install_github("https://github.com/swhitCBR/CVhelp",auth_token = "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")
# devtools::install_github("https://github.com/swhitCBR/TMBhelp",auth_token = "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")

devtools::load_all("../CVhelp")
devtools::load_all("../TMBhelp")



# devtools::install_github("https://github.com/swhitCBR/CVhelp")
# devtools::install_github("https://github.com/swhitCBR/TMBhelp")

HOR_CHP_comp_ls_scl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = TRUE)
# HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat <- HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_s[1:10,]

HOR_CHP_comp_ls_unscl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = FALSE)
# HOR_CHP_comp_ls_unscl$TMB_data_baseline$XX_pred_mat <- HOR_CHP_comp_ls_unscl$TMB_data_baseline$XX_s[1:10,]


dim(HOR_CHP_comp_ls_scl$XX_in_w_int_WYT)
colnames(HOR_CHP_comp_ls_scl$XX_in_w_int_WYT)





HOR_CHP_TMB_all_mods <- readRDS("../CVPAS_beta/src/HOR_CHP_TMB_all_mods.rds")
str(HOR_CHP_TMB_all_mods)
names(HOR_CHP_TMB_all_mods)
HOR_CHP_TMB_mod_fits_ls <- readRDS("../CVPAS_beta/src/HOR_CHP_TMB_mod_fits_ls.rds")
names(HOR_CHP_TMB_mod_fits_ls)

AIC_DF_d2 <- HOR_CHP_TMB_all_mods$"AIC_DF_full" %>% dplyr::filter(dAIC<=2)
AIC_DF_d2 <- AIC_DF_d2 %>% dplyr::mutate(candmodID=match(dm,HOR_CHP_TMB_all_mods$allint_DMs))
AIC_DF_d2$AICwt <- exp(-0.5*AIC_DF_d2$dAIC)/sum(exp(-0.5*AIC_DF_d2$dAIC))
AIC_DF_d2 <- AIC_DF_d2 %>% dplyr::select(-iterations,-message)
head(AIC_DF_d2)


# CVhelp_dat_w <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide")

source("dev/tmp_glmm_fxns.R")
# 
# source("R/HOR_CHP_mod_wrap.R")
head(CVhelp_dat_w)

devtools::load_all()

# create design matrix based on rows in wide format data environmental and operational data
# xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_unscl,
#                  sel_rows_tmp1=CVhelp_dat_w[1:5,],
#                  flength_in=240)
# 
# xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
#                  sel_rows_tmp1=CVhelp_dat_w[1:250,],
#                  flength_in=240)

attributes(HOR_CHP_comp_ls_scl$XX_in)$center

xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
                              sel_rows_tmp1=CVhelp_dat_w,
                              flength_in=240)

xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
                 sel_rows_tmp1=CVhelp_dat_w[1:250,],
                 flength_in=240)


attributes(HOR_CHP_comp_ls_scl$XX_in)[[c("scaled_vars")]]

# source("R/get_var_center_scale.R")

#extract center and scale for relevent parameters
scl_var_by_tmpDF <- get_var_center_scale(TMB_mod_ls=HOR_CHP_comp_ls_scl)

# xpred_tmp
# xpred_tmp_wcols

# xpred_tmp

# HOR_CHP_comp_ls_scl$TMB_data_baseline



# column names for design matrix
xpred_tmp_nms <- names(xpred_tmp)

# only main effects
# head(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat[,1:11])
# tmb_bs_nm <- colnames(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat[,1:11])

# full set of predictors
# tmb_bs_nm <- colnames(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat)
tmb_bs_nm <- colnames(HOR_CHP_comp_ls_scl$XX_in_w_int_WYT)

setdiff(tmb_bs_nm,xpred_tmp_nms)
xpred_tmp_wcols <- xpred_tmp[,match(tmb_bs_nm,xpred_tmp_nms)]


# this doesn't work
# HOR_CHP_comp_ls_scl$"TMB_data_baseline"$"xpred_tmp" <- xpred_tmp

# HOR_CHP_comp_ls_scl$"TMB_data_baseline"$"xpred_tmp" <- xpred_tmp_wcols

HOR_CHP_comp_ls_scl$"TMB_data_baseline"$"XX_pred_mat" <- as.matrix(xpred_tmp_wcols)


# scl_var_by_tmpDF
# xpred_tmp_wcols
# xpred_tmp

TMB:::getUserDLL()
# unload glmmTMB if currently loaded
pth2glmmTMB <- file.path(system.file("libs", package = "glmmTMB"),"x64")
if(TMB:::getUserDLL()=="glmmTMB") {dyn.unload(TMB::dynlib(file.path(pth2glmmTMB,"glmmTMB")))}
dyn.load(TMB::dynlib("../CVPAS_beta/src/TMB/CVPASbeta_TMBExports"))


# getting unscaled predictions
ii=1
# HOR_CHP_TMB_all_mods$allint_DMs
# which(HOR_CHP_TMB_all_mods$allint_DMs=="111111111110000000000")
predDF_ls <- list()
bt=proc.time()
for(ii in 1:nrow(AIC_DF_d2)){
  # extract design matrix index vector
  dm_str_val <- AIC_DF_d2$dm[ii]
  # split binary index vector
  inclu_IND_pred <- which(as.numeric(strsplit(dm_str_val,"")[[1]])!=0)

  TMB_data_tmp <- HOR_CHP_comp_ls_scl$"TMB_data_baseline"
  TMB_data_tmp$"XX_s" <- TMB_data_tmp$"XX_s"[,inclu_IND_pred]
  # TMB_data_tmp$"XX_pred_mat" <- TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred]
  TMB_data_tmp$"XX_pred_mat" <- TMB_data_tmp$"XX_pred_mat"
  
  # attributes(HOR_CHP_comp_ls_scl$XX_in)
  # HOR_CHP_comp_ls_scl
  # substitution
  # TMB_data_tmp$"XX_pred_mat" <-xpred_tmp_wcols[,inclu_IND_pred]

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
  predDF <- data.frame(id=ii,rw=1:nrow(HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat),
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
AIC_DF_d2$AICwt <- get_aic_wts(AIC_DF_d2,"AIC")
predDFcomb <- do.call(rbind,predDF_ls)
predDFcomb$wt=AIC_DF_d2$AICwt[predDFcomb$id]

library(ggplot2)

predDFcomb_wVars <- data.frame(predDFcomb,CVhelp_dat_w)

ggplot(data=predDFcomb_wVars #%>% filter(!(id %in% c(3,5,6)))
       ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
  geom_ribbon(alpha=0.25,color=NA) +# geom_point() +
  geom_line() +
  facet_wrap(~Year)

in_avg_tb <-  predDFcomb |>
  dplyr::group_by(rw) |>
  # filter(!(id %in% c(3,5,6))) |>
  dplyr::summarize(
  wt_lp_EST=sum(EST_lp*wt))

# adding the model selection uncertainty to the estimate
predDFcomb2 <- predDFcomb |> left_join(in_avg_tb) |> 
  # filter(!(id %in% c(3,5,6))) |>
  mutate(
    diff_modavg=(lo_pred-wt_lp_EST),
    SS_modavg=(lo_pred-wt_lp_EST)^2,
    lo_SE_modavg=sqrt(SS_modavg+(lo_SEadj)^2))

AIC_DF_d2$AICwt
# hist(predDFcomb2$diff_modavg)
# hist(sqrt(predDFcomb2$SS_modavg))

full_var_predDF <- predDFcomb2 |>
  # filter(!(id %in% c(3,5,6))) |> # dumb models
  select(id,rw,lo_pred,wt,wt_lp_EST,lo_SE_modavg) |>
  dplyr::group_by(rw) |>
  dplyr::summarize(
    var_modavg=sum(wt*lo_SE_modavg),
    se_moderr=sqrt(var_modavg))

avg_est_tmp <- in_avg_tb |> left_join(full_var_predDF)
xpred_tmp$rw=1:nrow(xpred_tmp)
avg_est_tmp_wVars <- xpred_tmp |> left_join(avg_est_tmp)# |> left_join(predDFcomb2 |> select(rw,id,lo_pred,lo_SE,lo_SEadj))


predDFcomb3 <- predDFcomb2  |> left_join(avg_est_tmp_wVars)

ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
       ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
  geom_ribbon(alpha=0.15,color=NA) +# geom_point() +
  geom_line() +
  facet_wrap(~Year)



ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
       ,aes(y=plogis(EST),x=DOY,color=factor(id))) +#,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))
  geom_line() +
  facet_wrap(~Year) + scale_y_continuous(limits=c(0,1))

ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
       ,aes(y=lo_pred,x=DOY,color=factor(id))) +#,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))
  geom_line() +
  facet_wrap(~Year) #+ scale_y_continuous(limits=c(0,1))


ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
       ,aes(y=diff_modavg,x=DOY,color=factor(id))) +#,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))
  geom_line() +
  facet_wrap(~Year) #+ scale_y_continuous(limits=c(0,1))


# predDFcomb2
# avg_est_tmp_wVars
# predDFwtavgDF_wVars <- data.frame(predDFwtavgDF,CVhelp_dat_w)


# avg_est_tmp_wVars$var_modavg
# avg_est_tmp_wVars$
# avg_est_tmp_wVars$wt_lp_EST-avg_est_tmp_wVars$wt_lp_EST

ggplot(data=avg_est_tmp_wVars,
       aes(y=plogis(wt_lp_EST),
           x=DOY,ymin=plogis(wt_lp_EST-(1.96*se_moderr)),
           ymax=plogis(wt_lp_EST+(1.96*se_moderr)))) + 
  geom_ribbon() + geom_point() + facet_wrap(~Year)


# Combined estimates
ggplot() +
  geom_ribbon(data=avg_est_tmp_wVars,
              aes(y=plogis(wt_lp_EST),
                  x=DOY,ymin=plogis(wt_lp_EST-(1.96*se_moderr)),
                  ymax=plogis(wt_lp_EST+(1.96*se_moderr))))+
  geom_ribbon(data=predDFcomb_wVars #%>% filter(!(id %in% c(3,5,6)))
              ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id)),alpha=0.25,color=NA) +# geom_point() +
  geom_line(data=predDFcomb_wVars #%>% filter(!(id %in% c(3,5,6)))
          ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
  geom_line(data=avg_est_tmp_wVars,
            aes(y=plogis(wt_lp_EST),
                x=DOY,ymin=plogis(wt_lp_EST-(1.96*se_moderr)),
                ymax=plogis(wt_lp_EST+(1.96*se_moderr))),linewidth=2) +
  facet_wrap(~Year)


# 
# predDFwtavgDF <-  predDFcomb |> 
#   dplyr::group_by(rw) |>
#   dplyr::summarize(
#     EST_lp=sum(EST_lp*wt),
#     EST=sum(EST*wt),
#     LCL=min(LCL),
#     UCL=max(UCL),
#     lo_SEadj=mean(lo_SEadj), # fix later
#     # pLCL=min(pLCL),
#     # pUCL=max(pUCL),
#     wtsum=sum(wt)) |> 
#   dplyr::mutate(
#     pEST=plogis(EST_lp),
#     pSE=pEST*(1-pEST)*lo_SEadj,
#     pLCL=plogis(LCL),
#     pUCL=plogis(UCL)
#   )
# 
# nrow(predDFcomb)
# 
# library(ggplot2)
# 
# # predDFcomb_wVars <- data.frame(predDFcomb,CVhelp_dat_w)
# 
# ggplot(data=predDFcomb_wVars %>% filter(!(id %in% c(3,5,6)))
#        ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
#   geom_ribbon(alpha=0.25,color=NA) +# geom_point() +
#   geom_line() +
#   facet_wrap(~Year)
# 
# ggplot(data=predDFcomb_wVars %>% filter(!(id %in% c(3,5,6)))
#        ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
#   geom_ribbon(alpha=0.25,color=NA) +# geom_point() +
#   geom_line() +
#   facet_wrap(~Year)
# 
# predDFwtavgDF_wVars <- data.frame(predDFwtavgDF,CVhelp_dat_w)
# ggplot(data=predDFwtavgDF_wVars,aes(y=pEST,x=DOY,ymin=pLCL,ymax=pUCL)) + geom_ribbon() + geom_point() + facet_wrap(~Year)
# 
# 
# 
# 
# #unscaled version older
# bt=proc.time()
# for(ii in 1:nrow(AIC_DF_d2)){
#   dm_str_val <- AIC_DF_d2$dm[ii]
#   inclu_IND_pred <- which(as.numeric(strsplit(dm_str_val,"")[[1]])!=0)
# 
#   head(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat)
# 
#   TMB_data_tmp <- HOR_CHP_comp_ls_unscl$"TMB_data_baseline"
#   TMB_data_tmp$"XX_s" <- TMB_data_tmp$"XX_s"[,inclu_IND_pred]
#   TMB_data_tmp$"XX_pred_mat" <- TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred]
#   
#   attributes(HOR_CHP_comp_ls_scl$XX_in)
#   # HOR_CHP_comp_ls_scl
# 
#   # substitution
#   # TMB_data_tmp$"XX_pred_mat" <-xpred_tmp_wcols[,inclu_IND_pred]
# 
#   head(TMB_data_tmp$"XX_pred_mat")
#   head(xpred_tmp_wcols[,inclu_IND_pred])
# 
#   # HOR_CHP_TMB_all_mods$
#   par_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val)$coeff_param
#   Spar_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & !is.na(S_parID))$coeff_param
#   Ppar_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & par_nm=="P_pars")$coeff_param
#   logSD_RELGRP_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & par_nm=="logSD_RELGRP")$coeff_param
#   # TMB_data_baseline_pred_tmp
#   parm_in_ls <- list(
#     "S_pars"=c(Spar_tmp),
#     "RELGRP_err"=HOR_CHP_TMB_mod_fits_ls[[ii]]$RELGRP_err_v,
#     "logSD_RELGRP"=logSD_RELGRP_tmp,
#     "P_pars"=matrix(Ppar_tmp,nrow=length(HOR_CHP_TMB_all_mods$TMB_data_baseline$col_P11),
#                     ncol=HOR_CHP_TMB_all_mods$TMB_data_baseline$P_lc_n))
#   OBJ_pred = TMB::MakeADFun(data=c(model="HOR_CHP_global_pred",TMB_data_tmp),
#                        parameters=parm_in_ls,
#                        DLL = "CVPASbeta_TMBExports",
#                        silent=TRUE,random='RELGRP_err',
#                        inner.control = list(maxit=20000))
#   summ_out_for_pred <- summary(TMB::sdreport(obj = OBJ_pred,
#                                              getReportCovariance = T,
#                                              bias.correct = F,
#                                              skip.delta.method = F,
#                                              ignore.parm.uncertainty = F))
#   predDF <- data.frame(id=ii,rw=1:nrow(HOR_CHP_comp_ls_unscl$TMB_data_baseline$XX_pred_mat),
#                        summ_out_for_pred[rownames(summ_out_for_pred)=="lp_Spred_uncond",])
#   names(predDF) <- c("id","rw","EST_lp","SE_lp"); rownames(predDF) <- NULL
#   # head(predDF)
#   predDF$SigmaREL_lp=exp(OBJ_pred$par["logSD_RELGRP"])
#   predDF$comb_var=sqrt((predDF$SE_lp^2)+(predDF$SigmaREL_lp^2))
#   predDF$EST=plogis(predDF$EST_lp)
#   predDF$LCL=plogis(predDF$EST_lp-1.96*predDF$SE_lp)
#   predDF$UCL=plogis(predDF$EST_lp+1.96*predDF$SE_lp)
#   predDF$SEcomb_lp=sqrt(predDF$comb_var)
#   predDF$EST=plogis(predDF$EST_lp)
#   predDF$pLCL=plogis(predDF$EST_lp-1.96*predDF$SEcomb_lp)
#   predDF$pUCL=plogis(predDF$EST_lp+1.96*predDF$SEcomb_lp)
#   
#   predDF_ls[[ii]] <- predDF
# } # 30 seconds to compute all preds
# proc.time()-bt # 22 seconds remotely
# 
# 
# 
# # HOR_CHP_comp_ls_unscl
# # head(HOR_CHP_comp_ls_unscl$TMB_data_baseline$XX_pred_mat)
# # 
# head(HOR_CHP_comp_ls_unscl$XX_in)
# HOR_CHP_TMB_mod_fits_ls <- readRDS("../CVPAS_beta/src/HOR_CHP_TMB_mod_fits_ls.rds")
# # load("..CVPASbeta/src/HOR_CHP_mod_dat_ls.RData")
# # load("..CVPASbeta/src/HOR_CHP_mod_dat_ls.RData")
# 
# HOR_CHP_TMB_all_mods <- readRDS("../CVPAS_beta/src/HOR_CHP_TMB_all_mods.rds")
# # head(HOR_CHP_comp_ls_unscl$XX_in)
# 
# # HOR_CHP_TMB_all_mods$
# 
# AIC_DF_d2 <- HOR_CHP_TMB_all_mods$"AIC_DF_full" %>% dplyr::filter(dAIC<=2)
# AIC_DF_d2 <- AIC_DF_d2 %>% dplyr::mutate(candmodID=match(dm,HOR_CHP_TMB_all_mods$allint_DMs))
# AIC_DF_d2$AICwt <- exp(-0.5*AIC_DF_d2$dAIC)/sum(exp(-0.5*AIC_DF_d2$dAIC))
# AIC_DF_d2 <- AIC_DF_d2 %>% dplyr::select(-iterations,-message)
# head(AIC_DF_d2)
# 
# 
# # load("../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData")
# TMB:::getUserDLL()
# 
# 
# 
# env_tmp <- CVhelp_dat_w
# log_vns_tmp <- (env_tmp$VNS-attributes(HOR_CHP_comp_ls_scl$XX_in)$center["log.VNS.hor.5"])/attributes(HOR_CHP_comp_ls_scl$XX_in)$scale["log.VNS.hor.5"]
# new_predDF <- HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_s[rep(1,length(log_vns_tmp)),]
# new_predDF[,"log.VNS.hor.5"] <- log_vns_tmp
# 
# library(TMBhelp)
# 
# 
# 
# HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat <- new_predDF
# predDF_ls <- list()
# 
# nrow(AIC_DF_d2)
# 
# TMB:::getUserDLL()
# pth2glmmTMB <- file.path(system.file("libs", package = "glmmTMB"),"x64")
# dyn.unload(TMB::dynlib(file.path(pth2glmmTMB,"glmmTMB")))
# dyn.load(TMB::dynlib("../CVPAS_beta/src/TMB/CVPASbeta_TMBExports"))
# 
# ii=1
# bt=proc.time()
# # for(ii in 1:nrow(AIC_DF_d2)){
#   dm_str_val <- AIC_DF_d2$dm[ii]
#   inclu_IND_pred <- which(as.numeric(strsplit(dm_str_val,"")[[1]])!=0)
#   TMB_data_tmp <- HOR_CHP_comp_ls_scl$"TMB_data_baseline"
#   TMB_data_tmp$"XX_s" <- TMB_data_tmp$"XX_s"[,inclu_IND_pred]
#   TMB_data_tmp$"XX_pred_mat" <- TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred]
#   
#   # HOR_CHP_TMB_all_mods$
#   par_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val)$coeff_param
#   Spar_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & !is.na(S_parID))$coeff_param
#   Ppar_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & par_nm=="P_pars")$coeff_param
#   logSD_RELGRP_tmp=subset(HOR_CHP_TMB_all_mods$pt_estsDF,dm==dm_str_val & par_nm=="logSD_RELGRP")$coeff_param
#   # TMB_data_baseline_pred_tmp
#   parm_in_ls <- list(
#     "S_pars"=c(Spar_tmp),
#     "RELGRP_err"=HOR_CHP_TMB_mod_fits_ls[[ii]]$RELGRP_err_v,
#     "logSD_RELGRP"=logSD_RELGRP_tmp,
#     "P_pars"=matrix(Ppar_tmp,nrow=length(HOR_CHP_TMB_all_mods$TMB_data_baseline$col_P11),
#                     ncol=HOR_CHP_TMB_all_mods$TMB_data_baseline$P_lc_n))
#   OBJ_pred = TMB::MakeADFun(data=c(model="HOR_CHP_global_pred",TMB_data_tmp),
#                        parameters=parm_in_ls,
#                        DLL = "CVPASbeta_TMBExports",
#                        silent=TRUE,random='RELGRP_err',
#                        inner.control = list(maxit=20000))
#   summ_out_for_pred <- summary(TMB::sdreport(obj = OBJ_pred,
#                                              getReportCovariance = T,
#                                              bias.correct = F,
#                                              skip.delta.method = F,
#                                              ignore.parm.uncertainty = F))
#   predDF <- data.frame(id=ii,rw=1:nrow(HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat),
#                        summ_out_for_pred[rownames(summ_out_for_pred)=="lp_Spred_uncond",])
#   names(predDF) <- c("id","rw","EST_lp","SE_lp"); rownames(predDF) <- NULL
#   # head(predDF)
#   predDF$SigmaREL_lp=exp(OBJ_pred$par["logSD_RELGRP"])
#   predDF$comb_var=sqrt((predDF$SE_lp^2)+(predDF$SigmaREL_lp^2))
#   predDF$EST=plogis(predDF$EST_lp)
#   predDF$LCL=plogis(predDF$EST_lp-1.96*predDF$SE_lp)
#   predDF$UCL=plogis(predDF$EST_lp+1.96*predDF$SE_lp)
#   predDF$SEcomb_lp=sqrt(predDF$comb_var)
#   predDF$EST=plogis(predDF$EST_lp)
#   predDF$pLCL=plogis(predDF$EST_lp-1.96*predDF$SEcomb_lp)
#   predDF$pUCL=plogis(predDF$EST_lp+1.96*predDF$SEcomb_lp)
#   
#   predDF_ls[[ii]] <- predDF
# # } # 30 seconds to compute all preds
# proc.time()-bt # 22 seconds remotely
