# based on code in 'CVPAS_beta/dev/02_dev.R'

devtools::load_all()

devtools::load_all("../CVhelp")
devtools::load_all("../TMBhelp")

HOR_CHP_comp_ls_scl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = TRUE)
HOR_CHP_comp_ls_scl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = TRUE)
HOR_CHP_comp_ls_unscl <- CVhelp::HOR_CHP_comp(RData_pth_in = "../CVPAS_beta/src/HOR_CHP_mod_dat_ls.RData",z_scale_vars = FALSE)

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

source("dev/tmp_glmm_fxns.R")
head(CVhelp_dat_w)

devtools::load_all()
attributes(HOR_CHP_comp_ls_scl$XX_in)$center

# CVhelp_dat_w_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide",DOY_rng = 1:365)
CVhelp_dat_w_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide",DOY_rng = 1:250)

# nrow(CVhelp_dat_w_alt)
# CVhelp_dat_w_alt <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide",DOY_rng = 1:300)
# nrow(CVhelp_dat_w_alt)==300*13

xpred_tmp <- HOR_CHP_mod_wrap(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
                              sel_rows_tmp1=CVhelp_dat_w_alt,
                              flength_in=240)


attributes(HOR_CHP_comp_ls_scl$XX_in)[[c("scaled_vars")]]

scl_var_by_tmpDF <- get_var_center_scale(TMB_mod_ls=HOR_CHP_comp_ls_scl)
xpred_tmp_nms <- names(xpred_tmp)

# full set of predictors
# tmb_bs_nm <- colnames(HOR_CHP_comp_ls_unscl$"TMB_data_baseline"$XX_pred_mat)
tmb_bs_nm <- colnames(HOR_CHP_comp_ls_scl$XX_in_w_int_WYT)

setdiff(tmb_bs_nm,xpred_tmp_nms)
xpred_tmp_wcols <- xpred_tmp[,match(tmb_bs_nm,xpred_tmp_nms)]

HOR_CHP_comp_ls_scl$"TMB_data_baseline"$"XX_pred_mat" <- as.matrix(xpred_tmp_wcols)

TMB:::getUserDLL()
# unload glmmTMB if currently loaded
pth2glmmTMB <- file.path(system.file("libs", package = "glmmTMB"),"x64")
if(TMB:::getUserDLL()=="glmmTMB") {dyn.unload(TMB::dynlib(file.path(pth2glmmTMB,"glmmTMB")))}
dyn.load(TMB::dynlib("../CVPAS_beta/src/TMB/CVPASbeta_TMBExports"))

ii=1
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
  
  #swapping in the XX_pred_mat
  TMB_data_tmp$"XX_pred_mat" <- TMB_data_tmp$"XX_pred_mat"
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
  OBJ_pred$gr()
  OBJ_pred$fn()
  summ_out_for_pred <- summary(TMB::sdreport(obj = OBJ_pred,
                                             getReportCovariance = T,
                                             bias.correct = F,
                                             skip.delta.method = F,
                                             ignore.parm.uncertainty = F)
  )
  predDF <- data.frame(id=ii,rw=1:nrow(HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat),
                       summ_out_for_pred[rownames(summ_out_for_pred)=="lp_Spred_uncond",])
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
  # predDF_ls[[ii]] <- predDF
  
  SD_obj <- TMB::sdreport(obj = OBJ_pred,
                          getReportCovariance = T,
                          bias.correct = T,
                          skip.delta.method = F,
                          ignore.parm.uncertainty = F,
                          getJointPrecision = TRUE)
  
  SD_OBJ_jp_mat <- SD_obj$jointPrecision
  cov_matrix <- solve(SD_obj$jointPrecision)
  
    # getting index of survival parameters
  fixed_ind <- which(rownames(cov_matrix)=="S_pars")
  sigma_beta <- cov_matrix[fixed_ind,fixed_ind]

  # library(corrplot)
  # corrplot(sigma_beta, is.corr = FALSE, method = "color", addcoef.col = "black")
  
  # cor_matrix <- cov2cor(cov_matrix)
  # cor_sigma_beta <- cov2cor(sigma_beta)
  # corrplot(cor_sigma_beta, method = "ellipse",)
  # corrplot(cor_matrix, method = "ellipse",)
  # 
  # vcov(SD_obj)
  # checking convergence
  all(eigen(cov_matrix)$values > 0)
  # OBJ_pred$env$par
  # # str(SD_obj$jointPrecision)
  # SD_obj$par.random
  # SD_obj$diag.cov.random
  # dim(SD_obj$cov)
  # SD_obj$cov.fixed
  
  # betas <- OBJ_pred$env$par
  # new_X <- HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat[1:4,inclu_IND_pred]
  new_X <- HOR_CHP_comp_ls_scl$TMB_data_baseline$XX_pred_mat[,inclu_IND_pred]
  
  TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% Spar_tmp
  sigma_beta_ls[[ii]] <- sigma_beta
  analytical_SE <- sqrt(rowSums((new_X %*% sigma_beta) * new_X))
  

  str(SD_OBJ_jp_mat)
  # full covariance matrix bootstrap
  # for the love of god, set perm=F for the cholesky
  L <- Matrix::Cholesky(SD_OBJ_jp_mat, LDL = FALSE,perm=F)
  n_boot <- 5000
  mu_joint =OBJ_pred$env$par
  p <- length(OBJ_pred$env$par)
  # library(Matrix)
  Z <- matrix(rnorm(p * n_boot), nrow = p, ncol = n_boot)
  boot_perturbations <- as.matrix(Matrix::solve(L, Z, system = "Lt"))
  boot_samples_joint <- t(mu_joint + boot_perturbations)

  tmp_v_ls<- list()
  for(jj in 1:nrow(boot_samples_joint)){
    S_pars_boot <- boot_samples_joint[jj,fixed_ind]
    tmp_v_ls[[jj]] <- as.vector(TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% S_pars_boot)
  }
  pred_DF_boot_raw<- do.call(cbind,tmp_v_ls)
  pred_DF_boot_summ <- data.frame(t(apply(pred_DF_boot_raw,1,function(x){c(mean(x),sd(x),quantile(x,c(0.025,0.5,0.975)))})))
  names(pred_DF_boot_summ2) <- c("boot_mean","boot_sd","boot_lcl","boot_median","boot_ucl")
  
    
  # library(mvtnorm) # For multivariate normal simulation
  simulated_draws <- mvtnorm::rmvnorm(n_boot, mean = mu_joint, sigma = as.matrix(cov_matrix))

  par(mfrow=c(2,1))
  hist(boot_samples_joint[,1],xlim=c(-2,2))
  hist(simulated_draws[,1],xlim=c(-2,2))
  
  hist(boot_samples_joint[,15],xlim=c(-5,1))
  hist(simulated_draws[,15],xlim=c(-5,1))
  
  par(mfcol=c(2,3),mar=c(3,3,1,1))
  for(kk in 1:ncol(boot_samples_joint)){
    hist(boot_samples_joint[,kk],main=kk,xlim=c(-3,3));abline(v=c(-2,2),col=2,lwd=3)
    hist(simulated_draws[,kk],main="",xlim=c(-3,3));abline(v=c(-2,2),col=2,lwd=3)}
  
  matplot(boot_samples_joint,type="l")
  matplot(simulated_draws,type="l")
  
  tmp_v_ls2<- list()
  for(jj in 1:nrow(simulated_draws)){
    S_pars_boot2 <- simulated_draws[jj,fixed_ind]
    tmp_v_ls2[[jj]] <- as.vector(TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% S_pars_boot2)
  }
  pred_DF_boot_raw2<- do.call(cbind,tmp_v_ls2)
  pred_DF_boot_summ2 <- data.frame(t(apply(pred_DF_boot_raw2,1,function(x){c(mean(x),sd(x),quantile(x,c(0.025,0.5,0.975)))})))
  names(pred_DF_boot_summ2) <- c("boot_mean","boot_sd","boot_lcl","boot_median","boot_ucl")
  
  hist(pred_DF_boot_raw2[1,])
  hist(pred_DF_boot_raw[1,])
  
  head(predDF)
  head(pred_DF_boot_summ2)
  head(pred_DF_boot_summ)
  
  # hist(simulated_draws[,1])
  # hist(boot_samples_joint[,1])
  
  apply(simulated_draws,2,summary)
  apply(simulated_draws,2,summary)
  
  # predDF <- data.frame(predDF,pred_DF_boot_summ2)
  head(predDF)
  
  # partial covariance matrix bootstrap
  
  L <- Matrix::Cholesky(SD_OBJ_jp_mat, LDL = FALSE)
  n_boot <- 5000
  mu_joint =OBJ_pred$env$par
  p <- length(OBJ_pred$env$par)
  library(Matrix)
  Z <- matrix(rnorm(p * n_boot), nrow = p, ncol = n_boot)
  boot_perturbations <- as.matrix(Matrix::solve(L, Z, system = "Lt"))
  boot_samples_joint <- t(mu_joint + boot_perturbations)
  
  tmp_v_ls<- list()
  for(jj in 1:nrow(boot_samples_joint)){
    S_pars_boot <- boot_samples_joint[jj,fixed_ind]
    tmp_v_ls[[jj]] <- as.vector(TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% S_pars_boot)
  }
  
  
  # predDF
  
  
  pred_DF_boot_raw<- do.call(cbind,tmp_v_ls)
  pred_DF_boot_summ <- data.frame(t(apply(pred_DF_boot_raw,1,function(x){c(mean(x),sd(x),quantile(x,c(0.025,0.5,0.975)))})))
  names(pred_DF_boot_summ) <- c("boot_mean","boot_sd","boot_lcl","boot_median","boot_ucl")
  hist(pred_DF_boot_raw[1,])
  
  head(predDF)
  head(pred_DF_boot_summ)
  
  predDF <- data.frame(predDF,pred_DF_boot_summ)
  
  # head(pred_DF_boot_summ)
  # dim(pred_DF_boot_raw)
  
  # S_pars_boot <- boot_samples_joint[,which(dimnames(boot_samples_joint)[[2]]=="S_pars")]
  # S_pars_boot
  
  
  
  
  # head(TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% Spar_tmp)
  # # (new_X %*% Spar_tmp) + 
  # # parametric bootstrap
  # cov_matrix <- solve(SD_obj$jointPrecision)
  # fixed_ind <- which(rownames(cov_matrix)=="S_pars")
  # sigma_beta <- cov_matrix[fixed_ind,fixed_ind]
  # 
  # sigma_beta_ls[[ii]] <- sigma_beta
  # sqrt(rowSums((new_X %*% sigma_beta) * new_X))
  # head(predDF)
  # 
  # # mu_joint <-  OBJ_pred$env$par
  # # L <- Matrix::Cholesky(SD_obj$jointPrecision, LDL = FALSE)
  # # n_boot <- 1000
  # # p <- length(mu_joint)
  # # library(Matrix)
  # # Z <- matrix(rnorm(p * n_boot), nrow = p, ncol = n_boot)
  # # boot_perturbations <- as.matrix(Matrix::solve(L, Z, system = "Lt"))
  # # boot_samples_joint <- t(mu_joint + boot_perturbations)
  # # 
  # # S_pars_boot <- boot_samples_joint[,which(dimnames(boot_samples_joint)[[2]]=="S_pars")]
  # # S_pars_boot
  # # 
  # # tmp_mat<- matrix(NA,nrow=nrow(S_pars_boot),ncol=nrow(TMB_data_tmp$"XX_pred_mat"))
  # # for(ii in 1:1000){
  # #   tmp_mat[ii,] <- as.vector(TMB_data_tmp$"XX_pred_mat"[,inclu_IND_pred] %*% S_pars_boot[ii,])}
  # # matplot(tmp_mat)
  # # matplot(plogis(tmp_mat))
  # # 
  # # # tmpDF <- data.frame(S_pars_boot[1,],Spar_tmp)
  # # 
  # # TMB_data_tmp$"XX_pred_mat"[1:4,inclu_IND_pred]  %*% tmpDF[,2]
  # # 
  # # as.numeric(S_pars_boot[1,])
  # # TMB_data_tmp$"XX_pred_mat"[1:4,inclu_IND_pred] %*% Spar_tmp
  # # 
  # # betas
  # # # apply(S_pars_boot,1,function(x) {x} ,simplify = F)
  # # # S_pars_boot %*% new_X
  # # 
  # # sum(S_pars_boot[1,]*new_X[1,])
  # # sum(S_pars_boot[1,]*new_X[1,])
  # # 
  # #   head(predDF)
  # 
  # # vcov(SD_obj)
  # # checking convergence
  # # all(eigen(cov_matrix)$values > 0)
  # # OBJ_pred$env$par
  # # str(SD_obj$jointPrecision)
  # # SD_obj$par.random
  # # SD_obj$diag.cov.random
  # # dim(SD_obj$cov)
  # # SD_obj$cov.fixed
  # 
  # 
  # 
  # # predDF <- data.frame(id=ii,rw=1:nrow(HOR_CHP_comp_ls_unscl$TMB_data_baseline$XX_pred_mat),
  # #                      summ_out_for_pred[rownames(summ_out_for_pred)=="lp_Spred_uncond",])
  # names(predDF) <- c("id","rw","EST_lp","SE_lp"); rownames(predDF) <- NULL
  # # head(predDF)
  # predDF$SigmaREL_lp=exp(OBJ_pred$par["logSD_RELGRP"])
  # predDF$comb_var=sqrt((predDF$SE_lp^2)+(predDF$SigmaREL_lp^2)) # seemingly mislabeled
  # predDF$lo_pred=predDF$EST_lp
  # predDF$lo_SE=predDF$SE_lp
  # predDF$lo_SEadj=sqrt((predDF$SE_lp^2)+(predDF$SigmaREL_lp^2))
  # predDF$LCL=predDF$lo_pred-1.96*predDF$lo_SE
  # predDF$UCL=predDF$lo_pred+1.96*predDF$lo_SE
  # predDF$LCLadj=predDF$lo_pred-1.96*predDF$lo_SEadj
  # predDF$UCLadj=predDF$lo_pred+1.96*predDF$lo_SEadj
  # predDF$EST=predDF$EST_lp
  # predDF_ls[[ii]] <- predDF
} # 30 seconds to compute all preds
proc.time()-bt # 16 seconds 
