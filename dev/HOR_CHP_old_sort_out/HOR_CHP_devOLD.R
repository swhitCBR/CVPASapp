# based on code in 'CVPAS_beta/dev/02_dev.R'

devtools::load_all()

devtools::load_all("../CVhelp")
devtools::load_all("../TMBhelp")


HOR_CHP_comp_ls_scl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = TRUE)


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
attributes(HOR_CHP_comp_ls_scl$XX_in)$center

CVhelp_dat_w_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide",DOY_rng = 1:365)
CVhelp_dat_w_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide",DOY_rng = 1:250)

nrow(CVhelp_dat_w_alt)
CVhelp_dat_w_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide",DOY_rng = 1:300)
nrow(CVhelp_dat_w_alt)==300*13


# # View(CVhelp_dat_w_alt)
# # table(is.na(CVhelp_dat_w_alt$date))
# nrow(CVhelp_dat_w_alt)
# CVhelp_dat_w_alt[which(is.na(predDFcomb_wVars$EST_lp)),]
# # table(is.na(CVhelp_dat_w_alt$OMT))
# # length(is.na(CVhelp_dat_w_alt$OMT))
# 
# CVhelp_dat_l_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "long",DOY_rng = 1:365)
# CVhelp_dat_l_alt
# CVhelp::get_env_plot(CVhelp_dat_l_alt)
# 

xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
                              sel_rows_tmp1=CVhelp_dat_w_alt,
                              flength_in=240)


# CVhelp_dat_w_alt[5200:5206,]

# CVhelp_dat_w_alt[which(is.na(predDFcomb_wVars$EST_lp)),]
# xpred_tmp[which(is.na(predDFcomb_wVars$EST_lp)),]


# View(CVhelp_dat_w)
# View(xpred_tmp)
# 
# xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
#                  sel_rows_tmp1=CVhelp_dat_w,#[1:250,],
#                  flength_in=240)#,SJL_route_in = F)

# xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
#                               sel_rows_tmp1=CVhelp_dat_w,
#                               flength_in=240)

attributes(HOR_CHP_comp_ls_scl$XX_in)[[c("scaled_vars")]]

# source("R/get_var_center_scale.R")

#extract center and scale for relevent parameters
scl_var_by_tmpDF <- get_var_center_scale(TMB_mod_ls=HOR_CHP_comp_ls_scl)


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
sigma_beta_ls <- list()
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
  head(xpred_tmp_wcols)
  # full set
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
                                             ignore.parm.uncertainty = F)
                               )
  predDF <- data.frame(id=ii,rw=1:nrow(HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat),
                       summ_out_for_pred[rownames(summ_out_for_pred)=="lp_Spred_uncond",])
  
  SD_obj <- TMB::sdreport(obj = OBJ_pred,
                          getReportCovariance = T,
                          bias.correct = F,
                          skip.delta.method = F,
                          ignore.parm.uncertainty = F,
                          getJointPrecision = TRUE)
  
  
  betas <- OBJ_pred$env$par
  new_X <- HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat[1:4,inclu_IND_pred]
  head(TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% Spar_tmp)
  head(TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% Spar_tmp)
  
  # (new_X %*% Spar_tmp) + 
    
  # parametric bootstrap
  cov_matrix <- solve(SD_obj$jointPrecision)
  fixed_ind <- which(rownames(cov_matrix)=="S_pars")
  sigma_beta <- cov_matrix[fixed_ind,fixed_ind]

  sigma_beta_ls[[ii]] <- sigma_beta
  sqrt(rowSums((new_X %*% sigma_beta) * new_X))
  head(predDF)
  
  # mu_joint <-  OBJ_pred$env$par
  # L <- Matrix::Cholesky(SD_obj$jointPrecision, LDL = FALSE)
  # n_boot <- 1000
  # p <- length(mu_joint)
  # library(Matrix)
  # Z <- matrix(rnorm(p * n_boot), nrow = p, ncol = n_boot)
  # boot_perturbations <- as.matrix(Matrix::solve(L, Z, system = "Lt"))
  # boot_samples_joint <- t(mu_joint + boot_perturbations)
  # 
  # S_pars_boot <- boot_samples_joint[,which(dimnames(boot_samples_joint)[[2]]=="S_pars")]
  # S_pars_boot
  # 
  # tmp_mat<- matrix(NA,nrow=nrow(S_pars_boot),ncol=nrow(TMB_data_tmp$"XX_pred_mat"))
  # for(ii in 1:1000){
  #   tmp_mat[ii,] <- as.vector(TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% S_pars_boot[ii,])}
  # matplot(tmp_mat)
  # matplot(plogis(tmp_mat))
  # 
  # # tmpDF <- data.frame(S_pars_boot[1,],Spar_tmp)
  # 
  # TMB_data_tmp$"XX_pred_mat"[1:4,inclu_IND_pred]  %*% tmpDF[,2]
  # 
  # as.numeric(S_pars_boot[1,])
  # TMB_data_tmp$"XX_pred_mat"[1:4,inclu_IND_pred] %*% Spar_tmp
  # 
  # betas
  # # apply(S_pars_boot,1,function(x) {x} ,simplify = F)
  # # S_pars_boot %*% new_X
  # 
  # sum(S_pars_boot[1,]*new_X[1,])
  # sum(S_pars_boot[1,]*new_X[1,])
  # 
  #   head(predDF)
  
  # vcov(SD_obj)
  # checking convergence
  # all(eigen(cov_matrix)$values > 0)
  # OBJ_pred$env$par
  # str(SD_obj$jointPrecision)
  # SD_obj$par.random
  # SD_obj$diag.cov.random
  # dim(SD_obj$cov)
  # SD_obj$cov.fixed

  
  
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

# predDF_ls

library(dplyr)
AIC_DF_d2$AICwt <- get_aic_wts(AIC_DF_d2,"AIC")
predDFcomb <- do.call(rbind,predDF_ls)
predDFcomb$wt=AIC_DF_d2$AICwt[predDFcomb$id]

library(ggplot2)

xpred_tmp$rw=xpred_tmp$DOY

# predDFcomb_wVars <- predDFcomb  |> left_join(xpred_tmp)

# predDFcomb_wVars <- data.frame(predDFcomb,CVhelp_dat_w)

predDFcomb_wVars <- data.frame(predDFcomb,xpred_tmp)

head(CVhelp_dat_w_alt)
View(CVhelp_dat_w_alt)
predDFcomb_wVars[which(is.na(predDFcomb_wVars$EST_lp)),]

xpred_tmp[which(is.na(predDFcomb_wVars$EST_lp)),]

ggplot(data=predDFcomb_wVars #%>% filter(!(id %in% c(3,5,6)))
       ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
  geom_ribbon(alpha=0.25,color=NA) +# geom_point() +
  geom_line() +
  facet_wrap(~Year)



table(is.na(predDFcomb$EST_lp))
head(predDFcomb)
table(table(predDFcomb$rw))
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

# ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
#        ,aes(y=plogis(EST),x=DOY,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))) +
#   geom_ribbon(alpha=0.15,color=NA) +# geom_point() +
#   geom_line() +
#   facet_wrap(~Year)

ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
       ,aes(y=plogis(EST),x=DOY,color=factor(id))) +#,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))
  geom_line() + ggtitle("prob_scale") +
  facet_wrap(~Year) + scale_y_continuous(limits=c(0,1))

ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
       ,aes(y=lo_pred,x=DOY,color=factor(id))) +#,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))
  geom_line() + ggtitle("logit_scale") +
  facet_wrap(~Year) #+ scale_y_continuous(limits=c(0,1))


# ggplot(data=predDFcomb3 #%>% filter(!(id %in% c(3,5,6)))
#        ,aes(y=diff_modavg,x=DOY,color=factor(id))) +#,ymin=plogis(LCL),ymax=plogis(UCL),color=factor(id),fill=factor(id))
#   geom_line() +
#   facet_wrap(~Year) #+ scale_y_continuous(limits=c(0,1))


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
