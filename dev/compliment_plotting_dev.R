## code to prepare `pred_tab_ls` dataset goes here

library(devtools)
load_all()

pred_tab_ls <- get_comp_model_preds(DOY_in = 1:250,years_in = NULL,flength_in = 244)
# options(digits = 3,scipen = 99)
# pred_tab_ls <- get_comp_model_preds(DOY_in = 1:250,years_in = NULL,flength_in = 244)
pred_tab_ls2 <- get_overall_surv_preds(pred_tab_ls_in = pred_tab_ls,predict_int = F,conf_level = 0.95) 
# usethis::use_data_raw("pred_tab_ls")

pred_pDF_comb <- pred_tab_ls2$pred_pDF_comb



ggplot_doy_pred_plt_compliment(
  data_in =  pred_pDF_comb,
  param_in="TCJ",
  doy_rng_in = c(1,250),
  pst_year_in = 2011)


ggplot_doy_pred_plt_compliment(
  data_in =  pred_pDF_comb,
  param_in="HOR",
  doy_rng_in = c(1,250),
  pst_year_in = 2011)
