

# AIC tables

# kinda cryptic
TCJ_CHP_pred_comp_ls$AIC_DF_d2
HOR_CHP_pred_comp_ls$AIC_DF_d2

# formula_too_long
glmmTMB_mod_ls[["TCJ"]]$TCJ_d2_aictabZ

#very cryptic
glmmTMB_mod_ls[["HOR_TCJ"]]$HOR_TCJ_aictab
glmmTMB_mod_ls[["HOR"]]$HOR_d2_aictabZ


# very nice
TCJ_CHP_pred_comp_ls$S_conf_ls$S_coef_confset_DF

# HOR_CHP_pred_comp_ls lacks S_conf_ls

# kinda cool!
TCJ_CHP_pred_comp_ls$TCJ_CHP_TMB_all_mods$pt_estsDF |> 
  dplyr::filter(dm %in% c(TCJ_CHP_pred_comp_ls$AIC_DF_d2$dm)) |> 
  dplyr::select(-dmID,-S_parID) |>
  tidyr::pivot_wider(names_from = par_nm,values_from = S_coef)

TCJ_CHP_pred_comp_ls$"S_conf_ls"$"S_coef_confset_DF" |> 
  # dplyr::filter(dm %in% c(TCJ_CHP_pred_comp_ls$AIC_DF_d2$dm)) |> 
  dplyr::select(-S_parID,-AICrank,-candmodID,-estimate,-SE) |>
  tidyr::pivot_wider(names_from = par_nm,values_from = S_coef)

# glmmTMB based models
# glmmTMB
# HOR_CHP_pred_comp_ls$pt_ests_SE_DF_ls
data.frame(formula=names(glmmTMB_mod_ls[["HOR_TCJ"]]$HOR_TCJ_d2_mods))

# glmmTMB_mod_ls[["HOR_TCJ"]]$HOR_TCJ_aictab |> dplyr::filter(dAIC<2)
# formula column differs from othere
tmp_tb <- glmmTMB_mod_ls[["HOR_TCJ"]]$HOR_TCJ_aictab |> dplyr::filter(dAIC<2) #|>#
tmp_tb$wt=get_aic_wts(tmp_tb,aic_column = "AIC")  
tmp_tb

# glmmTMB_mod_ls[["HOR"]]$HOR_d2_aictabZ |> dplyr::filter(dAIC<2)
tmp_tb <- glmmTMB_mod_ls[["HOR"]]$HOR_d2_aictabZ |> dplyr::filter(dAIC<2) #|>#
tmp_tb$wt=get_aic_wts(tmp_tb,aic_column = "AIC")  
tmp_tb

# glmmTMB_mod_ls[["TCJ"]]$TCJ_d2_aictabZ |> dplyr::filter(dAIC<2)
tmp_tb <- glmmTMB_mod_ls[["TCJ"]]$TCJ_d2_aictabZ |> dplyr::filter(dAIC<2) #|>#
tmp_tb$wt=get_aic_wts(tmp_tb,aic_column = "AIC")  
tmp_tb

