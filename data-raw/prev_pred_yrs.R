## code to prepare `prev_pred_yrs` dataset goes here

# usethis::use_data(prev_pred_yrs, overwrite = TRUE)
# generate predictions and plot from HOR_TCJ glmmTMB models

library(devtools)
load_all()

pred_tab_ls <- all_mod_preds(DOY_in = 1:250,years_in = NULL,flength_in = 244)

# options(digits = 3,scipen = 99)
pred_tab_ls <- all_mod_preds(DOY_in = 1:250,years_in = NULL,flength_in = 244)
pred_tab_ls2 <- all_mod_preds_fun2(pred_tab_ls_in = pred_tab_ls,predict_int = F,conf_level = 0.95)
# pred_tab_ls2$"beta_parm_df"
# get_pred_plts_dev(pred_pDF_comb_in = pred_tab_ls2$"pred_pDF_comb")

prev_pred_yrs <- pred_tab_ls2$"pred_pDF_comb"

usethis::use_data(prev_pred_yrs, overwrite = TRUE)

usethis::use_data(pred_tab_ls, overwrite = TRUE)
# generate predictions and plot from HOR_TCJ glmmTMB models

head(prev_pred_yrs)


