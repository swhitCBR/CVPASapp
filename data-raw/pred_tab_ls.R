## code to prepare `pred_tab_ls` dataset goes here



library(devtools)
load_all()

pred_tab_ls <- all_mod_preds(DOY_in = 1:250,years_in = NULL,flength_in = 244)
# usethis::use_data_raw("pred_tab_ls")

# pred_tab_ls_OLD <- pred_tab_ls
# usethis::use_data(pred_tab_ls_OLD, overwrite = TRUE)



usethis::use_data(pred_tab_ls, overwrite = TRUE)

names(pred_tab_ls)
pred_tab_ls
pred_tab_ls_OLD
# names(pred_tab_ls_OLD
# )


# pred_prev_yrs_ls$

head(pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]])
pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]

head(pred_tab_ls$HOR_TCJviaSJL)
head(pred_tab_ls$HOR_TCJviaSJL)
pred_tab_ls$HOR_TCJviaSJL$WYT


ggplot_doy_rte_plt(
  HOR_TCJ_pred_tab_plt = pred_tab_ls[["TCJ"]],
  doy_rng_in = c(
    45,
    90
  ),
  # pst_year_in = 2013 + 2
  pst_year_in = 2013
)

load_all()
ggplot_doy_rte_plt(
  HOR_TCJ_pred_tab_plt = pred_tab_ls[["TCJ"]],
  doy_rng_in = c(
    45,
    90
  ),
  # pst_year_in = 2013 + 2
  pst_year_in = 2013
)


pred_tab_ls[["TCJ_CHPviaTRN"]]$LCL= pred_tab_ls[["TCJ_CHPviaTRN"]]$lo_pred-1.96* pred_tab_ls[["TCJ_CHPviaTRN"]]$lo_SEadj
pred_tab_ls[["TCJ_CHPviaTRN"]]$UCL= pred_tab_ls[["TCJ_CHPviaTRN"]]$lo_pred+1.96* pred_tab_ls[["TCJ_CHPviaTRN"]]$lo_SEadj

head(pred_tab_ls[["TCJ_CHPviaTRN"]])
# TCJ_CHPviaTRN_pred_tab
ggplot_doy_pred_plt(
  HOR_TCJ_pred_tab_plt =  pred_tab_ls[["TCJ_CHPviaTRN"]],
  doy_rng_in = c(
    45,
    90
  ),
  pst_year_in = 2013
)
