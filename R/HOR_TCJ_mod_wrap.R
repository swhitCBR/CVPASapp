#' Title
#'
#' @details wrapper for generating HOR-TCJ survival predictions
#'
#' @param HOR_TCJ_mod_ls named list with aic table and list of models
#' @param sel_rows_tmp1 rows of 'CVhelp_dat_w' to use
#' @param flength_in fork length input scalar
#'
#' @returns dataframe with logit-scale predictions for survival model
#' @export
HOR_TCJ_mod_wrap <- function(HOR_TCJ_mod_ls,
                             sel_rows_tmp1=CVhelp_dat_w,
                             flength_in=240){
  stopifnot(flength_in >= 100 & flength_in <= 400)
  
  tmp_ls=extract_glmmTMB_frame(glmmTMB_res_ls_in=HOR_TCJ_mod_ls)
  
  # # renaming wide-format daily measures to match glmmTMB() model design matrices
  sel_rows_tmp2 <- sel_rows_tmp1 |> dplyr::rename(log.VNS.hor.5=VNS,
                                                  SWP.hor.5=SWP,
                                                  Tmsd.hor.7dadm=MSD,
                                                  barrier=barrierTF#,
                                                  # CVP=CVP.hor.5
  )
  sel_rows_tmp3 <- sel_rows_tmp2 |> dplyr::mutate(drought=WYT=="Critical")# special drought category
  sel_rows_tmp4 <- sel_rows_tmp3 |> dplyr::mutate(flength=flength_in ,YrRel=NA) # flength

  # sel_rows_tmp4 <- HOR_TCJ_redef_fun(sel_rows_tmp1_in=sel_rows_tmp1)
  
  aic_avg_tb <- HOR_TCJ_mod_ls$HOR_TCJ_aictab[names(HOR_TCJ_mod_ls$HOR_TCJ_d2_mods),]
  # print(aic_avg_tb)
  aic_avg_tb_wts <- aic_avg_tb |> dplyr::mutate(AICwt=exp(-0.5*dAIC)/sum(exp(-0.5*aic_avg_tb$dAIC)))
  
  # tmp_ls <- get_glmmTMB_ests()
  
  tmp_HOR_TCJ_preds1 <- get_glmmTMB_ests(sel_data_in = sel_rows_tmp4,
                                             aic_avg_tb_wts_in=aic_avg_tb_wts,
                                             glmmTMB_res_ls_in=HOR_TCJ_mod_ls)
  
  tmp_HOR_TCJ_preds2 <- dplyr::bind_rows(tmp_HOR_TCJ_preds1) |> 
    dplyr::group_by(sub_estimate,tmp_rw_ind) |> 
    dplyr::summarize(lo_pred=sum(fit*AICwt),
                     lo_SE=sum(AICwt*sqrt((se.fit^2)+(fit-lo_pred)^2)),
                     lo_SEadj=sum(AICwt*sqrt((SEadj^2)+(fit-lo_pred)^2))) |>
    dplyr::mutate(LCL=lo_pred-1.96*lo_SE,
                  UCL=lo_pred+1.96*lo_SE,
                  LCLadj=lo_pred-1.96*lo_SEadj,
                  UCLadj=lo_pred+1.96*lo_SEadj)
  
  dplyr::bind_cols(sel_rows_tmp4,tmp_HOR_TCJ_preds2)
}
