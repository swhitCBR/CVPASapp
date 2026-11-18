
devtools::load_all()
ggplot_doy_rte_plt(
  HOR_TCJ_pred_tab_plt = pred_tab_ls[["HOR"]],
  doy_rng_in = c(
    45,135
    # in_selected_RV$start_day,
    # in_selected_RV$end_day
  ),
  label_route_1 = "SJL",label_route_2 = "ORE",
  pst_year_in = "2013"#,param_in = "HOR"
)

ggplot_doy_rte_plt(
  HOR_TCJ_pred_tab_plt = pred_tab_ls[["HOR"]],
  doy_rng_in = c(
    45,135
    # in_selected_RV$start_day,
    # in_selected_RV$end_day
  ),
  label_route_1 = "MAC",label_route_2 = "TRN",
  pst_year_in = "2013"#,param_in = "HOR"
)


ggplot_doy_rte_plt(
  HOR_TCJ_pred_tab_plt = pred_tab_ls[["TCJ"]],
  doy_rng_in = c(
    45,135
    # in_selected_RV$start_day,
    # in_selected_RV$end_day
  ),
  pst_year_in = "2013",param_in = "HOR"
)



ggplot_doy_pred_plt_compliment()

ggplot_doy_pred_plt_compliment(
  data_in=pred_tab_ls[["HOR"]],
  doy_rng_in=c(1,250),
  pst_year_in="2012",
  param_in="HOR"#=#"S_HOR_CHP"
)
