
TCJ_CHP_mod_wrap <- function(
     TCJ_CHP_pred_comp_ls_in=TCJ_CHP_pred_comp_ls
     ,
     sel_rows_tmp1=CVhelp_dat_w
     ,
     flength_in=240
     # ,
     # SJL_route_in=TRUE
) {
  
  subb <- sel_rows_tmp1
  
  xpred_tmp_init <- TCJ_CHP_DM_scl(TCJ_CHP_comp_ls_unscl_in=TCJ_CHP_pred_comp_ls_in$"TCJ_CHP_comp_ls_unscl",
                                   flength_in = flength_in,
                                   sel_rows_tmp1=subb
                                   )
  
  TCJ_CHP_get_pred(xpred_tmp_in = xpred_tmp_init,
                   TCJ_CHP_pred_comp_ls_in = TCJ_CHP_pred_comp_ls_in)
  
  
}
