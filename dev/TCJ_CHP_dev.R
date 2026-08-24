

# last working code

xpred_tmp_init <- TCJ_CHP_DM_scl(TCJ_CHP_comp_ls_unscl_in=TCJ_CHP_pred_comp_ls$"TCJ_CHP_comp_ls_unscl",
                                 flength_in = 244,
                                 sel_rows_tmp1=CVhelp_dat_w[1:5,])

TCJ_CHP_get_pred(xpred_tmp_in = xpred_tmp_init,
                 TCJ_CHP_pred_comp_ls_in = TCJ_CHP_pred_comp_ls)



# devtools::load_all("../TMBhelp")
# # devtools::load_all("../CVhelp")
# TCJ_CHP_TMB_all_mods <- readRDS("../CVPAS_STH_app/output/TCJ_CHP_TMB_all_mods.rds")
# 
# # loading functions that def work
# source("C:/repos/CVPAS_STH_app/TCJ_CHP_cust_fxns.R")
# 
# # rlang::env_get_list()
# # rlang::env_get(nm = ".dll")
# # TMB:::getUserDLL()
# # dyn.load(dyn.unload("TMB/TCJ_CHP_global"))
# # 20 to 30
# # TCJ_CHP_TMB_all_mods$AIC_DF_full[which(TCJ_CHP_TMB_all_mods$AIC_DF_full$dm=="111111111110000000000"),]
# AIC_DF_full <- TCJ_CHP_TMB_all_mods$"AIC_DF_full"
# 
# AIC_DF_d2 <- TCJ_CHP_TMB_all_mods$"AIC_DF_full" |> dplyr::filter(dAIC<=2)
# AIC_DF_d2 <- AIC_DF_d2 |> dplyr::mutate(candmodID=match(dm,TCJ_CHP_TMB_all_mods$allint_DMs))
# # consider get_aic_wts()
# AIC_DF_full$AICwt <- exp(-0.5*AIC_DF_full$dAIC)/sum(exp(-0.5*AIC_DF_full$dAIC))
# 
# # devtools::load_all("../TMBhelp")
# TCJ_CHP_mod_fits_d2_ls <- fit_nested_tmb_mods_MOD(
#   S_design_matrix_in = AIC_DF_d2$dm, 
#   dir_in ="C:/repos/CVPAS_STH_app/TMB",
#   TMB_data_baseline_in=TCJ_CHP_TMB_all_mods$"TMB_data_baseline",
#   calc_SEs=TRUE,
#   # cpp_name_in = "TCJ_CHP_global"
#   cpp_name_in = "HOR_CHP_global"
# )
# 
# TCJ_CHP_mod_fits_d2_ls$`1111110010101`$COV_mat
# SD_obj <- TCJ_CHP_mod_fits_d2_ls$`1111110010101`$COV_mat
# cov_matrix <- solve(SD_obj$jointPrecision)
# 
# saveRDS(TCJ_CHP_mod_fits_d2_ls,"TCJ_CHP_mod_fits_d2_ls.rds")
# 
# 
# usethis::use_data_raw("TCJ_CHP_pred_comp_ls")
# 
# 
# TCJ_CHP_mod_fits_d2_ls[[1]]$est_tab$Estimate
# S_coef_ls_confset <- lapply(1:length(TCJ_CHP_mod_fits_d2_ls),function(ii){
#   aa=TCJ_CHP_mod_fits_d2_ls[[ii]]$"est_tab"
#   S_coef=aa$"Estimate_SE"[aa$"Parameter"=="S_pars"]
#   est_tmp=aa$"Estimate"[aa$"Parameter"=="S_pars"]
#   se_tmp=aa$"Std..Error"[aa$"Parameter"=="S_pars"]
#   c=data.frame(AICrank=ii,
#                candmodID=AIC_DF_d2$candmodID[ii],
#                dm=AIC_DF_d2$dm[ii],
#                par_nm=names(TCJ_CHP_TMB_all_mods$"par_nm_ind_ls"[[AIC_DF_d2$candmodID[ii]]]),
#                S_parID=TCJ_CHP_TMB_all_mods$"par_nm_ind_ls"[[AIC_DF_d2$candmodID[ii]]],
#                S_coef,
#                estimate=est_tmp,
#                SE=se_tmp)
#   rownames(c)=NULL
#   return(c)})
# 
# S_coef_confset_DF <- do.call(rbind,S_coef_ls_confset)
# S_conf_ls <- list(
#   "AIC_DF_d2"=AIC_DF_d2,
#   "S_coef_confset_DF"=S_coef_confset_DF)
# 
# # saveRDS(S_conf_ls,"output/TCJ_CHP_S_conf_ls.rds")



# TCJ_CHP_mod_fits_d2_ls <- fit_nested_tmb_mods(
#   S_design_matrix_in = AIC_DF_d2$dm, 
#   dir_in ="C:/repos/CVPAS_STH_app/TMB",
#   TMB_data_baseline_in=TCJ_CHP_TMB_all_mods$"TMB_data_baseline",
#   calc_SEs=TRUE,cpp_name_in = "TCJ_CHP_global")

if(getwd()!="C:/repos/CVPAS_STH_app"){setwd("C:/repos/CVPAS_STH_app")}

# setwd("..")
# devtools::load_all("../TMBhelp")
# TCJ_CHP_mod_fits_d2_alt_sets_ls <- fit_nested_tmb_mods_MOD(
#   S_design_matrix_in = AIC_DF_d2$dm[1:2], 
#   dir_in ="C:/repos/CVPAS_STH_app/TMB",
#   TMB_data_baseline_in=TCJ_CHP_TMB_all_mods$"TMB_data_baseline",
#   calc_SEs=TRUE,
#   cpp_name_in = "HOR_CHP_global"#,
# )

load("data/TCJ_CHP_mod_dat_ls.RData")


TCJ_CHP_comp_ls_unscl <- TCJ_CHP_comp(z_scale_vars = F,
                                      lvec.tcj_ls_in = lvec.tcj_ls,sdat.det.in=sdat.det.common2,x.df = x.df)

TCJ_CHP_comp_ls_scl <- TCJ_CHP_comp(z_scale_vars = T,lvec.tcj_ls_in = lvec.tcj_ls,sdat.det.in=sdat.det.common2,x.df = x.df)

# sclabl_var_nms <- c("flength","Qomt.tcj.1net","log.VNS.tcj","CVP.tcj.4","SWP.tcj.4","Tmsd.tcj.7dadm")
# tmp_scl <-attributes(scale(TCJ_CHP_comp_ls_unscl$XX_in_w_int_WYT))
# tmp_scl[["scaled:center"]][sclabl_var_nms]#c("flength","Qomt.tcj.1net","log.VNS.tcj","CVP.tcj.4","SWP.tcj.4","Tmsd.tcj.7dadm")]
# tmp_scl[["scaled:scale"]][sclabl_var_nms]


TCJ_CHP_pred_comp_ls <- TCJ_CHP_mod_fits_d2_ls

head(TCJ_CHP_TMB_all_mods$"TMB_data_baseline")




head(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s)


# table(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s[,"TCJroute.facF"])

# new start

devtools::load_all()

# TCJ_CHP_TMB_all_mods <- TCJ_CHP_pred_comp_ls$TCJ_CHP_TMB_all_mods
# TCJ_CHP_mod_fits_d2_ls <- TCJ_CHP_pred_comp_ls$TCJ_CHP_mod_fits_d2_ls
lvec.tcj_ls <- TCJ_CHP_pred_comp_ls$lvec.tcj_ls
sdat.det.common2 <- TCJ_CHP_pred_comp_ls$sdat.det.common2
x.df <- TCJ_CHP_pred_comp_ls$x.df


source("C:/repos/CVPAS_STH_app/TCJ_CHP_cust_fxns.R")

# TCJ_CHP_pred_comp_ls$TCJ_CHP_comp_ls_unscl

TCJ_CHP_comp_ls_unscl <- TCJ_CHP_comp(z_scale_vars = F,
                                      lvec.tcj_ls_in = lvec.tcj_ls,
                                      sdat.det.in=sdat.det.common2,
                                      x.df = x.df)



# TCJ_CHP_comp_ls_scl <- TCJ_CHP_comp(z_scale_vars = T,lvec.tcj_ls_in = lvec.tcj_ls,sdat.det.in=sdat.det.common2,x.df = x.df)

xpred_tmp_init <- TCJ_CHP_DM_scl(
  TCJ_CHP_comp_ls_unscl_in=TCJ_CHP_comp_ls_unscl)

names(xpred_tmp_init)
xpred_tmp_init





setwd("C:/repos/CVPASapp")
source("C:/repos/CVPASapp/R/TCJ_CHP_DM_scl.R")

attributes(scale(TCJ_CHP_comp_ls_unscl$XX_in))

get_var_center_scale_MOD <- function(TMB_mod_ls){
  
  # scale(TMB_mod_ls$"XX_in")
  # return(TMB_mod_ls$"XX_in")
  # element_nms <- c("scaled_vars","center","scale")
  
  df <- data.frame(sapply(element_nms,function(x) attributes(TMB_mod_ls$"XX_in")[[x]]))
  df$"center" <- as.numeric(df$"center")
  df$"scale" <- as.numeric(df$"scale")
  return(df)
  
  # old implementation
  df_tmp <- data.frame(dplyr::bind_cols(
    lapply(element_nms,function(x) attributes(TMB_mod_ls$"XX_in")[[x]])))
  names(df_tmp) <- element_nms
  df_tmp
}


get_var_center_scale_MOD(TCJ_CHP_TMB_all_mods$"TCJ_CHP_comp_ls")
get_var_center_scale(TCJ_CHP_TMB_all_mods$"TCJ_CHP_comp_ls")

# get_var_center_scale(TCJ_CHP_comp_ls_scl)

attributes(scale(get_var_center_scale_MOD(TCJ_CHP_TMB_all_mods$"TCJ_CHP_comp_ls")))

# get_var_center_scale(TCJ_CHP_comp_ls_scl$)



devtools::load_all()
CVhelp_dat_w





