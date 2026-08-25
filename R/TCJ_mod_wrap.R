#' Title
#'
#' @details wrapper for generating HOR-TCJ survival predictions
#'
#' @param TCJ_mod_ls named list with aic table and list of models
#' @param sel_rows_tmp1 rows of 'CVhelp_dat_w' to use
#' @param flength_in fork length input scalar
#' @param DOY_in day of year subset
#' @param years_in years to subset
#' @param route 
#'
#' @returns dataframe with logit-scale predictions for survival model
#' 
#' @export
#' 
TCJ_mod_wrap <- function(TCJ_mod_ls,
                         sel_rows_tmp1=CVhelp_dat_w,
                         flength_in=240,
                         DOY_in=1:250,
                         years_in=NULL,
                         route="TRN"
) {
  
  if(is.null(years_in)){
    years_in=unique(sel_rows_tmp1$Year)
  }
  
  sel_rows_tmp1 <- sel_rows_tmp1 |> dplyr::filter(Year %in% years_in & DOY %in% DOY_in)
  
  stopifnot(flength_in >= 100 & flength_in <= 400)
  # renaming wide-format daily measures to match glmmTMB() model design matrices
  sel_rows_tmp2 <- sel_rows_tmp1 |> dplyr::rename(log.VNS.ent.tcj=VNS,
                                                  barrier.exit.tcj.fac=barrierTF,
                                                  CVP.exit.tcj=CVP,
                                                  SWP.exit.tcj=SWP)
  
  # return(sel_rows_tmp2)
  # Dealing with variable scaling
  # if(!all(attributes(HOR_CHP_mod_ls$XX_in)$center==0)){
  #   message("using scaled version")
  
  #   scl_var_tb <- get_var_center_scale(TMB_mod_ls=HOR_CHP_mod_ls)
  
  #   sel_rows_tmp2 <- sel_rows_tmp2 |> dplyr::mutate(
  #     log.VNS.hor.5=(log.VNS.hor.5-scl_var_tb$center[scl_var_tb$scaled_vars=="log.VNS.hor.5"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="log.VNS.hor.5"],
  #     SWP.hor.5=(SWP.hor.5-scl_var_tb$center[scl_var_tb$scaled_vars=="SWP.hor.5"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="SWP.hor.5"],
  #     Tmsd.hor.7dadm=(Tmsd.hor.7dadm-scl_var_tb$center[scl_var_tb$scaled_vars=="Tmsd.hor.7dadm"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="Tmsd.hor.7dadm"],
  #     CVP.hor.5=(CVP.hor.5-scl_var_tb$center[scl_var_tb$scaled_vars=="CVP.hor.5"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="CVP.hor.5"],
  #     Qomt.hor.1net=(Qomt.hor.1net-scl_var_tb$center[scl_var_tb$scaled_vars=="Qomt.hor.1net"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="Qomt.hor.1net"]
  #   )
  # }
  sel_rows_tmp3 <- sel_rows_tmp2 
  
  sel_rows_tmp4 <- sel_rows_tmp3 |> 
    dplyr::mutate(
      flength=flength_in,
      YrRel=NA
    )
  aic_avg_tb <- TCJ_mod_ls$TCJ_d2_aictabZ
  aic_avg_tb_wts <- aic_avg_tb |> dplyr::mutate(AICwt=exp(-0.5*dAIC)/sum(exp(-0.5*aic_avg_tb$dAIC)))
  
  tmp_HOR_preds1 <- get_glmmTMB_ests(sel_data_in = sel_rows_tmp4,
                                     mods_obj_nm='TCJ_d2_mods',
                                     aic_avg_tb_wts_in=aic_avg_tb_wts,
                                     glmmTMB_res_ls_in=TCJ_mod_ls,
                                     sub_estimate_in="TRN")
  
  
  tmp_HOR_preds2 <- dplyr::bind_rows(tmp_HOR_preds1) |> 
    dplyr::group_by(sub_estimate,tmp_rw_ind) |> 
    dplyr::summarize(lo_pred=sum(fit*AICwt),
                     lo_SE=sum(AICwt*sqrt((se.fit^2)+(fit-lo_pred)^2)),
                     lo_SEadj=sum(AICwt*sqrt((SEadj^2)+(fit-lo_pred)^2))) |>
    dplyr::mutate(LCL=lo_pred-1.96*lo_SE,
                  UCL=lo_pred+1.96*lo_SE,
                  LCLadj=lo_pred-1.96*lo_SEadj,
                  UCLadj=lo_pred+1.96*lo_SEadj)
  
  dplyr::bind_cols(sel_rows_tmp4,tmp_HOR_preds2)
  
  
}