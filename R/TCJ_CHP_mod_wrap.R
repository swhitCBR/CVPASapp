
#' Title
#'
#' @param TCJ_CHP_pred_comp_ls_in 
#' @param sel_rows_tmp1 
#' @param flength_in 
#' @param DOY_in 
#' @param years_in 
#' @param SJL_route_in 
#'
#' @returns
#' @export
#'
#' @examples
TCJ_CHP_mod_wrap <- function(
     TCJ_CHP_pred_comp_ls_in=TCJ_CHP_pred_comp_ls
     ,
     sel_rows_tmp1=CVhelp_dat_w
     ,
     flength_in=240,
     DOY_in=1:250,
     years_in=NULL,
     SJL_route_in=TRUE
) {
  
  
  if(is.null(years_in)){
    years_in=unique(sel_rows_tmp1$Year)
  }
  
  sel_rows_tmp1 <- sel_rows_tmp1 |> dplyr::filter(Year %in% years_in & DOY %in% DOY_in)
  
  subb <- sel_rows_tmp1
  
  xpred_tmp_init <- TCJ_CHP_DM_scl(TCJ_CHP_comp_ls_unscl_in=TCJ_CHP_pred_comp_ls_in$"TCJ_CHP_comp_ls_unscl",
                                   flength_in = flength_in,
                                   sel_rows_tmp1=subb,
                                   SJL_route_in=SJL_route_in
                                   )
  
  outDF <- TCJ_CHP_get_pred(xpred_tmp_in = xpred_tmp_init,
                   TCJ_CHP_pred_comp_ls_in = TCJ_CHP_pred_comp_ls_in)
  
  outDF$lo_pred <-outDF$wt_lp_EST
  outDF$lo_SEadj <-outDF$se_moderr
  outDF$lo_SE <-outDF$se_moderr_noadj
  
  return(outDF)
  
  
}
