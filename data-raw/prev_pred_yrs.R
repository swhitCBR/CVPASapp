## code to prepare `prev_pred_yrs` dataset goes here

# usethis::use_data(prev_pred_yrs, overwrite = TRUE)
# generate predictions and plot from HOR_TCJ glmmTMB models

library(devtools)
load_all()

pred_tab_ls <- get_comp_model_preds(DOY_in = 1:250,years_in = NULL,flength_in = 244)
pred_tab_ls2 <- get_overall_surv_preds(pred_tab_ls_in = pred_tab_ls,predict_int = F,conf_level = 0.95) 

head(pred_tab_ls$TCJ)



# options(digits = 3,scipen = 99)
# pred_tab_ls <- get_comp_model_preds(DOY_in = 1:250,years_in = NULL,flength_in = 244)


pred_tab_ls2$pred_pDF_comb

# pred_tab_ls2$"beta_parm_df"
get_pred_plts_dev(pred_pDF_comb_in = pred_tab_ls2$"pred_pDF_comb")

prev_pred_yrs <- pred_tab_ls2$"pred_pDF_comb"

usethis::use_data(prev_pred_yrs, overwrite = TRUE)

usethis::use_data(pred_tab_ls, overwrite = TRUE)
# generate predictions and plot from HOR_TCJ glmmTMB models

head(prev_pred_yrs)


head(pred_tab_ls$TCJ)
head(pred_tab_ls[["TCJ"]])


ggplot_doy_pred_plt

# other
tmp1 <- get_comp_model_preds(DOY_in = 1:250,years_in = NULL,flength_in = 244)
tmp2 <- get_overall_surv_preds(pred_tab_ls_in = tmp1,predict_int = F,conf_level = 0.95) 

head(tmp2)
head(tmp2$pred_pDF_comb)
head(tmp2$pred_pDF_comb_w)



ggplot_doy_pred_plt_single(
  data_in =  pred_pDF_comb,
  param_in="S_TCJ_CHP",
  doy_rng_in = c(1,250),
  pst_year_in = 2011)



