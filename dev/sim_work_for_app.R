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
my_sim_fun((tmpDF[["lo_pred_mat"]])[1:3,],)

int_fun <- function(mat1,mat2){
  result_vec <- mapply(my_sim_fun,mat1,mat2)
  result_mat <- matrix(result_vec, nrow = nrow(mat1))
  result_mat}

rep_sim_est2  <- function(nsim,emat1,emat2){
  stopifnot(nrow(emat1)==nrow(emat2))
  replicate(n=nsim,expr = matrix(get_overall_surv_calc(int_fun(mat1 = emat1,mat2 = emat2)),nrow=nrow(emat1)))
}


tmpDF <- get_all_mod_pred_mats(pred_tab_ls_in = pred_tab_ls,predict_int = F,conf_level = 0.9)

# single draw
# Component model proportions 
int_fun(mat1 = as.matrix(tmpDF[["lo_pred_mat"]])[1:3,],mat2 = as.matrix(tmpDF[["loSEadj_mat"]][1:3,]))
# Detrived estimates 
rep_sim_est2(nsim=1,emat1=as.matrix(tmpDF[["lo_pred_mat"]])[1:30,],
             emat2=as.matrix(tmpDF[["loSEadj_mat"]][1:30,]))[,,1]


# 
rep_sim_out_deriv <- rep_sim_est2(nsim=3000,emat1=as.matrix(tmpDF[["lo_pred_mat"]])[1:30,],
                                  emat2=as.matrix(tmpDF[["loSEadj_mat"]][1:30,]))

# apply(rep_sim_out_deriv,1:2,length)
apply(rep_sim_out_deriv,1:2,mean)
apply(rep_sim_out_deriv,1:2,sd)
apply(rep_sim_out_deriv,1:2,function(x) quantile(x,c(0.025)))
apply(rep_sim_out_deriv,1:2,function(x) quantile(x,c(0.975)))


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


