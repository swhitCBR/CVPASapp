# generate predictions aand plot from HOR_TCJ glmmTMB models

library(devtools)
load_all()


data("CVhelp_dat_w")
# data("CVhelp_dat_l")

str(HOR_TCJ_mod_d2_ls)
names(HOR_TCJ_mod_d2_ls)
HOR_TCJ_mod_d2_ls$HOR_TCJ_aictab
HOR_TCJ_mod_d2_ls$HOR_TCJ_d2_mods

HOR_TCJ_mod_d2_ls <- readRDS("../CVPAS_beta/src/HOR_TCJ_d2_mods.rds")
HOR_mod_d2_ls <- readRDS("../CVPAS_STH_app/output/HOR_mod_d2_ls.rds")

names(HOR_TCJ_mod_d2_ls)
glmmTMB_mod_ls <- list("HOR_TCJ"= HOR_TCJ_mod_d2_ls,
                       "HOR"= HOR_mod_d2_ls)
names(glmmTMB_mod_ls)
# usethis::use_data(glmmTMB_mod_ls,overwrite = TRUE)
# pred_prev_yrs_ls
getwd()
# write.csv(ann_HORbar_WYT_data,"CVPAS_annual_ref_tab.csv")

devtools::load_all()
glmmTMB_mod_ls

TMB:::getUserDLL()
dyn.unload(TMB::dynlib("../CVPAS_beta/src/TMB/CVPASbeta_TMBExports"))
pth2glmmTMB <- file.path(system.file("libs", package = "glmmTMB"),"x64")
dyn.load(TMB::dynlib(file.path(pth2glmmTMB,"glmmTMB")))

names(glmmTMB_mod_ls)
glmmTMB_mod_ls[["HOR_TCJ"]]
extract_glmmTMB_frame(glmmTMB_res_ls_in=glmmTMB_mod_ls[["HOR_TCJ"]])
# tmp <- CVhelp_dat_w[600:700,]

file.exists("dev/tmp_glmm_fxns.R")
source("dev/tmp_glmm_fxns.R")

HOR_CHP_mod_wrap


devtools::load_all("../CVhelp")


# relabels wide format variables so that they match up with HOR_TCJ model
HOR_TCJ_tmp <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w[600:700,],
                                HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]])

# draw_inputs_ann_summ_dt()
glmmTMB_mod_ls[["HOR_TCJ"]]$HOR_TCJ_aictab
glmmTMB_mod_ls[["HOR_TCJ"]]


 tmp_HOR_TCJ_preds1 <- get_glmmTMB_ests(sel_data_in = sel_rows_tmp4,
                                             aic_avg_tb_wts_in=aic_avg_tb_wts,
                                             glmmTMB_res_ls_in=HOR_TCJ_mod_ls)


aic_avg_tb <- HOR_TCJ_mod_ls$HOR_TCJ_aictab[names(HOR_TCJ_mod_ls$HOR_TCJ_d2_mods),]
  # print(aic_avg_tb)
  aic_avg_tb_wts <- aic_avg_tb |> dplyr::mutate(AICwt=exp(-0.5*dAIC)/sum(exp(-0.5*aic_avg_tb$dAIC)))
  
  # tmp_ls <- get_glmmTMB_ests()
  
  tmp_HOR_TCJ_preds1 <- get_glmmTMB_ests(sel_data_in = sel_rows_tmp4,
                                             aic_avg_tb_wts_in=aic_avg_tb_wts,
                                             glmmTMB_res_ls_in=HOR_TCJ_mod_ls)
  
  tmp_HOR_TCJ_preds2 <- dplyr::bind_rows(tmp_HOR_TCJ_preds1) |> 
    dplyr::group_by(sub_estimate,tmp_rw_ind) |> 
    dplyr::summarize(lo_pred=sum(fit*AICwt),
                     lo_SE=sum(AICwt*sqrt((se.fit^2)+(fit-lo_pred)^2)),
                     lo_SEadj=sum(AICwt*sqrt((SEadj^2)+(fit-lo_pred)^2))) |>
    dplyr::mutate(LCL=lo_pred-1.96*lo_SE,
                  UCL=lo_pred+1.96*lo_SE,
                  LCLadj=lo_pred-1.96*lo_SEadj,
                  UCLadj=lo_pred+1.96*lo_SEadj)
  
  dplyr::bind_cols(sel_rows_tmp4,tmp_HOR_TCJ_preds2)





HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                                     HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
                                     flength_in=244) 
pred_prev_yrs_ls <- list(  "HOR_TCJ_pred_tab" = HOR_TCJ_pred_tab)

# strange warning
# Error in .Call("FreeADFunObject", ptr, PACKAGE = DLL) : 
#   "FreeADFunObject" not available for .Call() for package "CVPASbeta_TMBExports"
# Error in .Call("FreeADFunObject", ptr, PACKAGE = DLL) : 
#   "FreeADFunObject" not available for .Call() for package "CVPASbeta_TMBExports"

usethis::use_data(pred_prev_yrs_ls,overwrite=TRUE)


glmmTMB_mod_ls[["HOR_TCJ"]]

# HOR_TCJ_mod_ls


HOR_TCJ_tmp2 <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w[600:700,],flength_in = 100,HOR_TCJ_mod_ls = HOR_TCJ_mod_d2_ls)
HOR_TCJ_tmp3 <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w[600:700,],flength_in = 400,HOR_TCJ_mod_ls = HOR_TCJ_mod_d2_ls)

ggplot2::ggplot() +
  ggplot2::geom_ribbon(data=HOR_TCJ_tmp,ggplot2::aes(x=DOY,ymin=plogis(LCL),ymax=plogis(UCL)),fill="gray40") +
  ggplot2::geom_line(data=HOR_TCJ_tmp,ggplot2::aes(x=DOY,y=plogis(lo_pred))) +
  ggplot2::facet_wrap(~Year)

ggplot2::ggplot() +
  ggplot2::geom_ribbon(data=HOR_TCJ_tmp,ggplot2::aes(x=DOY,ymin=plogis(LCL),ymax=plogis(UCL)),alpha=0.2,fill="gray40") +
  ggplot2::geom_ribbon(data=HOR_TCJ_tmp2,ggplot2::aes(x=DOY,ymin=plogis(LCL),ymax=plogis(UCL)),alpha=0.2,,fill="gray40") +
  ggplot2::geom_ribbon(data=HOR_TCJ_tmp3,ggplot2::aes(x=DOY,ymin=plogis(LCL),ymax=plogis(UCL)),alpha=0.2,,fill="gray40") +
  ggplot2::geom_line(data=HOR_TCJ_tmp,ggplot2::aes(x=DOY,y=plogis(lo_pred))) +
  ggplot2::geom_line(data=HOR_TCJ_tmp2,ggplot2::aes(x=DOY,y=plogis(lo_pred))) +
  ggplot2::geom_line(data=HOR_TCJ_tmp3,ggplot2::aes(x=DOY,y=plogis(lo_pred))) +
  ggplot2::facet_wrap(~Year)

ggplot2::ggplot() +
  # ggplot2::geom_ribbon(data=HOR_TCJ_tmp,ggplot2::aes(x=DOY,ymin=plogis(LCL),ymax=plogis(UCL)),fill="gray40") +
  ggplot2::geom_line(data=HOR_TCJ_tmp,ggplot2::aes(x=DOY,y=plogis(lo_pred))) +
  ggplot2::facet_wrap(~Year)


# HOR_TCJ_pred_tab <-
# HOR_TCJ_pred_tab <-
# ggplot2::ggplot() +
#   ggplot2::geom_ribbon(data=HOR_TCJ_pred_tab,ggplot2::aes(x=DOY,ymin=plogis(LCL),ymax=plogis(UCL))) +
#   ggplot2::geom_line(data=HOR_TCJ_pred_tab,ggplot2::aes(x=DOY,y=plogis(lo_pred))) +
#   ggplot2::facet_wrap(~Year) 

# history snippet
# HOR_TCJ_pred_tab <- dplyr::bind_cols(sel_rows_tmp4,tmp_HOR_TCJ_preds2)


pred_prev_yrs_ls <- list(  "HOR_TCJ_pred_tab" = HOR_TCJ_pred_tab)
usethis::use_data(pred_prev_yrs_ls)
# 
# # glmmTMB:::predict.glmmTMB(HOR_TCJ_mod_d2_ls$HOR_TCJ_d2_mods[[1]],newdata = tmp_ls[[2]][1:10,],se.fit = T)
# # # HOR_TCJ_mod_d2_ls$HOR_TCJ_d2_mods$
# # str(HOR_TCJ_mod_d2_ls$HOR_TCJ_d2_mods$`drought+flength+barrier*(VNS+flength+temp+SWP`)
# # HOR_TCJ_mod_d2_ls$HOR_TCJ_d2_mods$`drought+flength+barrier*(VNS+flength+temp+SWP`
# 
# # estimates and SEs
# lapply(glmmTMB:::predict.glmmTMB(HOR_TCJ_mod_d2_ls$HOR_TCJ_d2_mods[[ii]],newdata = sel_rows_tmp4,se.fit = T))
# 
# # extracting fixed model matrix
# tmp$modelInfo$terms$cond$fixed
# # RE sigma
# tmp$obj$report()$sd[[1]]
# # intercepts
# tmp$obj$report()$b 
# tmp$obj$report()
# # WYT_year_ref <- data.frame(
# #   year=c(2011,2012,2013,2014,2015,2016),
# #   WYT=c("wet","dry","drought","drought","drought","dry"),
# #   SJ_CDEC_WYT=c("Wet","Dry","Critical","Critical","Critical","Dry"))
# 
# 
# glmmTMB:::predict.glmmTMB(HOR_TCJ_mod_d2_ls$HOR_TCJ_d2_mods[[1]],newdata = sel_rows_tmp2,se.fit = F)
# 
# 

# footer

# not needed 
WYT_to_altCAT <- function(WYT_in){
  official_WYTs <- c("Wet","Dry","Critical","Below Normal","Above Normal")
  stopifnot(all(WYT_in %in% official_WYTs))
  c("wet","dry","drought","dry","wet")[match(WYT_in,official_WYTs)]
}

