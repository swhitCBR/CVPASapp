# generate predictions and plot from HOR_TCJ glmmTMB models

library(devtools)
load_all()

pred_tab_ls <- get_comp_model_preds(DOY_in = 1:250,years_in = NULL,flength_in = 200)

# options(digits = 3,scipen = 99)
pred_tab_ls <- get_comp_model_preds(DOY_in = 1:250,years_in = NULL,flength_in = 200)
pred_tab_ls2 <- get_overall_surv_preds(pred_tab_ls_in = pred_tab_ls,predict_int = F,conf_level = 0.9)
# pred_tab_ls2$"beta_parm_df"
get_pred_plts_dev(pred_pDF_comb_in = pred_tab_ls2$"pred_pDF_comb")







# my_sim_funORIG <- function(x,y){
#   rnorm(n = 1,mean = x,sd = y)}

expit <- function(x) {1/(1+exp(-1*x))}
my_sim_fun <- function(x,y){
  expit(rnorm(n = 1,mean = x,sd = y))}
# my_sim_fun((tmpDF[["lo_pred_mat"]])[1:3,],)

int_fun <- function(mat1,mat2){
  result_vec <- mapply(my_sim_fun,mat1,mat2)
  result_mat <- matrix(result_vec, nrow = nrow(mat1))
  result_mat}

rep_sim_est2  <- function(nsim,emat1,emat2){
  stopifnot(nrow(emat1)==nrow(emat2))
  replicate(n=nsim,expr = matrix(get_overall_surv_calc(int_fun(mat1 = emat1,mat2 = emat2)),nrow=nrow(emat1)))
}



tmpDF <- get_all_mod_pred_mats(pred_tab_ls_in = pred_tab_ls,predict_int = F,conf_level = 0.95)
tmpDF$pSE_mat
tmpDF$lo_pred_mat
tmpDF$loSEadj_mat
# tmpDF[["p_mat"]][1,]
# tmpDF[["pSE_mat"]][1,]


pred_tab_ls2$pred_pDF_comb |> dplyr::filter(Year==2011 & DOY==1 & param %in% c("S_TCJ_CHP","S_HOR_CHP")) |> dplyr::select(param,pr_pred,p_SEadj )
# pred_tab_ls2$pred_pDF_comb_w[,which(names(pred_tab_ls2$pred_pDF_comb_w) %in% c("S_TCJ_CHP","S_HOR_CHP"))][1,]
# get_overall_surv_logit(MU_mat = as.matrix(tmpDF[["lo_pred_mat"]][1,]),as.matrix(tmpDF[["loSEadj_mat"]][1,]^2))[,c("S_TCJ_CHP_mean","S_HOR_CHP_mean")]
source("dev/get_overall_surv_logit.R")
get_overall_surv_logit(MU_mat = as.matrix(tmpDF[["lo_pred_mat"]][1,]),
                       as.matrix(tmpDF[["loSEadj_mat"]][1,]^2))[,c("S_TCJ_CHP_mean","S_TCJ_CHP_sd","S_HOR_CHP_mean","S_HOR_CHP_sd")]

source("dev/get_overall_surv_logit.R")
source("dev/get_overall_surv_OLD.R")

get_overall_surv()
get_overall_surv_OLD()

# tmpDF$pSE_mat

get_overall_surv(E_prop = as.matrix(tmpDF[["p_mat"]][1,]),
                 V_prop = as.matrix(tmpDF[["pSE_mat"]][1,]^2))$"deriv_ests"[,c("S_TCJ_CHP_mean","S_TCJ_CHP_sd","S_HOR_CHP_mean","S_HOR_CHP_sd")]

get_overall_surv_logit(MU_mat = as.matrix(tmpDF[["lo_pred_mat"]][1,]),
                       as.matrix(tmpDF[["loSEadj_mat"]][1,]^2))[,c("S_TCJ_CHP_mean","S_TCJ_CHP_sd","S_TCJ_CHP_LCL","S_TCJ_CHP_UCL","S_HOR_CHP_mean","S_HOR_CHP_sd","S_HOR_CHP_LCL","S_HOR_CHP_UCL")]
get_overall_surv(E_prop = as.matrix(tmpDF[["p_mat"]][1,]),
                 V_prop = as.matrix(tmpDF[["pSE_mat"]][1,]^2))$"deriv_ests"[,c("S_TCJ_CHP_mean","S_TCJ_CHP_sd","S_TCJ_CHP_LCL","S_TCJ_CHP_UCL","S_HOR_CHP_mean","S_HOR_CHP_sd","S_HOR_CHP_LCL","S_HOR_CHP_UCL")]




# logit inputs

MU_vec <- matrix(c(0.08353233, 1.898172, 0.2437021, -0.2331759, -0.7107783, -0.4517569), nrow = 1)
SIGMA2_vec <- matrix(c(0.6304774, 0.7393159, 0.4519971, 0.4947115, 0.519552, 0.3127887), nrow = 1)

# Component 1 conversion (logit -> proportion scale) needed for get_overall_surv(),
# which expects already-converted E_prop/V_prop
f_mu  <- 1 / (1 + exp(-MU_vec))
f_pr  <- f_mu * (1 - f_mu) # the constants are omitted and the negative sign makes the second term 1-mu
f_sec <- f_mu * (1 - f_mu) * (1 - 2 * f_mu)
E_prop_new <- f_mu + (SIGMA2_vec / 2) * f_sec
V_prop_new <- (f_pr^2) * SIGMA2_vec

E_prop_new <- f_mu + (SIGMA2_vec / 2) * (f_mu * (1 - f_mu) * (1 - 2 * f_mu))

res_logit_new <- get_overall_surv_logit(MU_vec, SIGMA2_vec)
res_surv_new  <- get_overall_surv(E_prop_new, V_prop_new)$deriv_ests

res_logit_new
res_surv_new













# pred_pSE_DF_w1[,which(!names(pred_pSE_DF_w1) %in% c("Year","DOY"))]
# get_overall_surv(MU_mat = as.matrix(tmpDF[["p_mat"]][1,]),
#                      as.matrix(tmpDF[["pSE_mat"]][1,]^2))#[,c("S_TCJ_CHP_mean","S_TCJ_CHP_sd","S_HOR_CHP_mean","S_HOR_CHP_sd")]
# get_overall_surv_logit(MU_mat = as.matrix(tmpDF[["lo_pred_mat"]][1,]),
#                        as.matrix(tmpDF[["loSEadj_mat"]][1,]^2))[,c("S_TCJ_CHP_mean","S_TCJ_CHP_sd","S_HOR_CHP_mean","S_HOR_CHP_sd")]

# as.matrix(tmpDF[["p_mat"]][1,])
# pred_tab_ls

# tmpDF[["lo_pred_mat"]]
# tmpDF[["lo_pred_mat"]][1:30,]

# single draw
# Component model proportions 
int_fun(mat1 = as.matrix(tmpDF[["lo_pred_mat"]])[1:3,],mat2 = as.matrix(tmpDF[["loSEadj_mat"]][1:3,]))

# int_fun(mat1 = matrix(tmpDF[["lo_pred_mat"]])[1,],mat2 = matrix(tmpDF[["loSEadj_mat"]][1,]))

# doupling up
int_fun(mat1 = as.matrix(tmpDF[["lo_pred_mat"]])[c(1,1),],mat2 = as.matrix(tmpDF[["loSEadj_mat"]][c(1,1),]))
# Detrived estimates 
rep_sim_est2(nsim=1,emat1=as.matrix(tmpDF[["lo_pred_mat"]])[1:30,],
             emat2=as.matrix(tmpDF[["loSEadj_mat"]][1:30,]))[,,1]

rep_sim_out_deriv <- rep_sim_est2(nsim=10000,emat1=as.matrix(tmpDF[["lo_pred_mat"]])[1:30,],
                                  emat2=as.matrix(tmpDF[["loSEadj_mat"]][1:30,]))

# apply(rep_sim_out_deriv,1:2,length)
apply(rep_sim_out_deriv,1:2,mean)
apply(rep_sim_out_deriv,1:2,sd)
apply(rep_sim_out_deriv,1:2,function(x) quantile(x,c(0.025)))
apply(rep_sim_out_deriv,1:2,function(x) quantile(x,c(0.975)))

mean(rep_sim_out_deriv[1,1,])
mean(rep_sim_out_deriv[1,1,])
hist(rep_sim_out_deriv[1,1,])


data.frame(S_TCJ_CHP_mean=mean(rep_sim_out_deriv[1,1,]),S_TCJ_CHP_sd=sd(rep_sim_out_deriv[1,1,]),S_TCJ_CHP_LCL=quantile(rep_sim_out_deriv[1,1,],c(0.025)),S_TCJ_CHP_UCL=quantile(rep_sim_out_deriv[1,1,],c(0.975)),
           S_HOR_CHP_mean=mean(rep_sim_out_deriv[1,2,]),S_HOR_CHP_sd=sd(rep_sim_out_deriv[1,2,]),S_HOR_CHP_LCL=quantile(rep_sim_out_deriv[1,2,],c(0.025)),S_HOR_CHP_UCL=quantile(rep_sim_out_deriv[1,2,],c(0.975)))


get_overall_surv_logit(MU_mat = as.matrix(tmpDF[["lo_pred_mat"]][1,]),
                       as.matrix(tmpDF[["loSEadj_mat"]][1,]^2))[,c("S_TCJ_CHP_mean","S_TCJ_CHP_sd","S_TCJ_CHP_LCL","S_TCJ_CHP_UCL","S_HOR_CHP_mean","S_HOR_CHP_sd","S_HOR_CHP_LCL","S_HOR_CHP_UCL")]
get_overall_surv(E_prop = as.matrix(tmpDF[["p_mat"]][1,]),
                 V_prop = as.matrix(tmpDF[["pSE_mat"]][1,]^2))$"deriv_ests"[,c("S_TCJ_CHP_mean","S_TCJ_CHP_sd","S_TCJ_CHP_LCL","S_TCJ_CHP_UCL","S_HOR_CHP_mean","S_HOR_CHP_sd","S_HOR_CHP_LCL","S_HOR_CHP_UCL")]

load_all()


rep_sim_out_deriv_ALL <- rep_sim_est2(nsim=300,emat1=as.matrix(tmpDF[["lo_pred_mat"]]),
                                      emat2=as.matrix(tmpDF[["loSEadj_mat"]]))
rep_sim_out_deriv_ALL
apply(rep_sim_out_deriv_ALL,1:2,mean)

DF_sim_repALL <- data.frame(
  pred_tab_ls2$pred_pDF_comb |> dplyr::filter(param=="S_TCJ_CHP") |> dplyr::select(-pLCL,-pUCL,-p_SEadj,-pr_pred,-param),
  param="S_TCJ_CHP_sim",
  method="simulated",
  pr_pred=apply(rep_sim_out_deriv_ALL,1:2,mean)[,1],
  p_SEadj=apply(rep_sim_out_deriv_ALL,1:2,sd)[,1],
  pLCL=apply(rep_sim_out_deriv_ALL,1:2,function(x) quantile(x,c(0.025)))[,1],
  pUCL=apply(rep_sim_out_deriv_ALL,1:2,function(x) quantile(x,c(0.975)))[,1])

comp_simDF_TCJ <- dplyr::bind_rows(DF_sim_repALL,pred_tab_ls2$pred_pDF_comb |> dplyr::filter(param=="S_TCJ_CHP")|> dplyr::mutate(method="analytic"))


DF_sim_repALL <- data.frame(
  pred_tab_ls2$pred_pDF_comb |> dplyr::filter(param=="S_HOR_CHP") |> dplyr::select(-pLCL,-pUCL,-p_SEadj,-pr_pred,-param),
  param="S_HOR_CHP_sim",
  method="simulated",
  pr_pred=apply(rep_sim_out_deriv_ALL,1:2,mean)[,2],
  p_SEadj=apply(rep_sim_out_deriv_ALL,1:2,sd)[,2],
  pLCL=apply(rep_sim_out_deriv_ALL,1:2,function(x) quantile(x,c(0.025)))[,2],
  pUCL=apply(rep_sim_out_deriv_ALL,1:2,function(x) quantile(x,c(0.975)))[,2])

comp_simDF_HOR <- dplyr::bind_rows(DF_sim_repALL,pred_tab_ls2$pred_pDF_comb |> dplyr::filter(param=="S_HOR_CHP") |> dplyr::mutate(method="analytic"))
comp_simDF <- dplyr::bind_rows(
  comp_simDF_HOR,
  comp_simDF_TCJ)


# library(ggplot2)

ggplot2::ggplot(data=comp_simDF, #|> #dplyr::filter(Year==2011),
                ggplot2::aes(y=pr_pred,x=DOY,ymin=pLCL,ymax=pUCL,color=method,fill=method)) +
  ggplot2::facet_grid(Year~param) + 
  ggplot2::geom_ribbon(fill="gray",alpha=0.5,color=NA) +
  ggplot2::geom_line() + 
  ggplot2::geom_hline(yintercept = 0.5,linetype="dotted")

ggplot2::ggplot(data=comp_simDF, #|> #dplyr::filter(Year==2011),
                ggplot2::aes(y=pr_pred,x=DOY,ymin=pLCL,ymax=pUCL,color=method,fill=method)) +
  ggplot2::facet_grid(Year~param) + 
  ggplot2::geom_ribbon(fill="gray",alpha=0.5,color=NA) +
  ggplot2::geom_line() + 
  ggplot2::geom_hline(yintercept = 0.5,linetype="dotted")


