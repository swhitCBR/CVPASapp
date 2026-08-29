
#' Title
#'
#' @param flength_in 
#' @param DOY_in 
#' @param years_in 
#' @param CVhelp_dat_w_in 
#' @param SJL_route_in logical, SJL route as opposed to ORE
#' @param conf_level confidence level
#'
#' @returns
#' @export
#'
HOR_CHP_mod_wrap <- function(flength_in=240,
                             DOY_in=1:250,
                             years_in=NULL,
                             CVhelp_dat_w_in=CVhelp_dat_w,
                             SJL_route_in=TRUE,
                             conf_level=0.95){
  if(is.null(years_in)){
    years_in=unique(CVhelp_dat_w$Year)
  }
  HOR_CHP_comp_ls_scl <- HOR_CHP_comp_alt(
    HOR_CHP_data_inputs_ls_in=HOR_CHP_pred_comp_ls$"data_inputs_ls",
    z_scale_vars=T)
  
  CVhelp_dat_w_sub <-  subset(CVhelp_dat_w_in,DOY %in% DOY_in & Year %in% years_in)
  
  # return(CVhelp_dat_w_sub)
  
  xpred_tmp <- HOR_CHP_DM_scl(HOR_CHP_mod_ls=HOR_CHP_comp_ls_scl,
                              sel_rows_tmp1=CVhelp_dat_w_sub,
                              flength_in=flength_in,
                              SJL_route_in = SJL_route_in)
  
  pred_DF <- HOR_CHP_get_pred(HOR_CHP_pred_comp_ls_in=HOR_CHP_pred_comp_ls,
                              xpred_tmp_in=xpred_tmp)

  
  # HOR_CHP_pred_get_pred(HOR_CHP_pred_comp_ls_in=HOR_CHP_pred_comp_ls,xpred_tmp_in=xpred_tmp)
  # [,c("Year","DOY","wt_lp_EST","se_moderr")]
  
  pred_DF_w_length <- data.frame(flength=flength_in,pred_DF)
  
  outDF <- pred_DF_w_length
  
  outDF$lo_pred <-outDF$wt_lp_EST
  outDF$lo_SEadj <-outDF$se_moderr
  outDF$lo_SE <-outDF$se_moderr_noadj
  
  # Dynamically calculate the critical z-value based on user input
  alpha_tail <- (1 - conf_level) / 2
  z_crit     <- qnorm(1 - alpha_tail)
  
  outDF$LCLnoadj <- outDF$lo_pred - outDF$lo_SE * z_crit
  outDF$UCLnoadj <- outDF$lo_pred + outDF$lo_SE * z_crit
  
  outDF$LCL <- outDF$lo_pred - outDF$lo_SEadj * z_crit
  outDF$UCL <- outDF$lo_pred + outDF$lo_SEadj * z_crit
    
  
  # pred_tab_ls[["TCJ_CHPviaTRN"]]$LCL= pred_tab_ls[["TCJ_CHPviaTRN"]]$lo_pred-1.96* pred_tab_ls[["TCJ_CHPviaTRN"]]$lo_SEadj
  # pred_tab_ls[["TCJ_CHPviaTRN"]]$UCL= pred_tab_ls[["TCJ_CHPviaTRN"]]$lo_pred+1.96* pred_tab_ls[["TCJ_CHPviaTRN"]]$lo_SEadj

  return(outDF)
  
  
  # return(pred_DF_w_length)
}
