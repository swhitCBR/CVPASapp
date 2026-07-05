#' Title
#'
#' @param glmmTMB_res_ls_in named list containing glmmTMB model list and AIC table
#' @param mods_obj_nm name of the level-one element containing models
#'
#' @returns list of dataframes used to fit glmmTMB models
#' @export
#'
extract_glmmTMB_frame <- function(glmmTMB_res_ls_in,
                            mods_obj_nm='HOR_TCJ_d2_mods'){
  frm_ls <- lapply(1:length(glmmTMB_res_ls_in),function(ii){
    data.frame(
      mod_form=names(glmmTMB_res_ls_in[[mods_obj_nm]])[ii],
      glmmTMB_res_ls_in[[mods_obj_nm]][[ii]]$"frame")})
  frm_ls
}




#' Title
#'
#' @details wrapper for generating HOR-TCJ survival predictions
#'
#' @param HOR_CHP_mod_ls named list with aic table and list of models
#' @param sel_rows_tmp1 rows of 'CVhelp_dat_w' to use
#' @param flength_in fork length input scalar
#'
#' @returns dataframe with logit-scale predictions for survival model
#' 
#' @export
#' 
HOR_CHP_mod_wrap <- function(HOR_CHP_mod_ls,
                             sel_rows_tmp1=CVhelp_dat_w,
                             flength_in=240,
                             SJL_route_in=TRUE) {
  
  stopifnot(flength_in >= 100 & flength_in <= 400)

  # tmp_ls=extract_glmmTMB_frame(glmmTMB_res_ls_in=HOR_TCJ_mod_ls)
  # HOR_CHP_mod_ls$TMB_data_baseline$XX_pred_mat <- HOR_CHP_mod_ls$TMB_data_baseline$XX_s[1:10,]
  # tmp_ls <- HOR_CHP_mod_ls$TMB_data_baseline$XX_pred_mat
  # return(tmp_ls)

  # # renaming wide-format daily measures to match glmmTMB() model design matrices
  sel_rows_tmp2 <- sel_rows_tmp1 |> dplyr::rename(log.VNS.hor.5=VNS,
                                                  SWP.hor.5=SWP,
                                                  Tmsd.hor.7dadm=MSD,
                                                  barrier.facTRUE=barrierTF,
                                                  CVP.hor.5=CVP,
                                                  Qomt.hor.1net=OMT)
  
   if(!all(attributes(HOR_CHP_mod_ls$XX_in)$center==0)){
    message("using scaled version")
     
     scl_var_tb <- get_var_center_scale(TMB_mod_ls=HOR_CHP_mod_ls)

     sel_rows_tmp2 <- sel_rows_tmp2 |> dplyr::mutate(
        log.VNS.hor.5=(log.VNS.hor.5-scl_var_tb$center[scl_var_tb$scaled_vars=="log.VNS.hor.5"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="log.VNS.hor.5"],
        SWP.hor.5=(SWP.hor.5-scl_var_tb$center[scl_var_tb$scaled_vars=="SWP.hor.5"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="SWP.hor.5"],
        Tmsd.hor.7dadm=(Tmsd.hor.7dadm-scl_var_tb$center[scl_var_tb$scaled_vars=="Tmsd.hor.7dadm"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="Tmsd.hor.7dadm"],
        CVP.hor.5=(CVP.hor.5-scl_var_tb$center[scl_var_tb$scaled_vars=="CVP.hor.5"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="CVP.hor.5"],
        Qomt.hor.1net=(Qomt.hor.1net-scl_var_tb$center[scl_var_tb$scaled_vars=="Qomt.hor.1net"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="Qomt.hor.1net"]
      )
     
   }
  
 sel_rows_tmp3 <- sel_rows_tmp2 |> dplyr::mutate(
    Intercept=1,
    WYT_drought=as.numeric(WYT=="Critical"), # similar to plain drought category
    WYT_wet=as.numeric(WYT=="Wet")  )# special drought category

  sel_rows_tmp4 <- sel_rows_tmp3 |> 
    dplyr::mutate(
      flength=flength_in,
      route.facB=ifelse(SJL_route_in,0,1),
      YrRel=NA) |>
    dplyr::relocate(Intercept) |>
    dplyr::mutate(
      B_x_Temp=barrier.facTRUE*Tmsd.hor.7dadm,
      B_x_VNS=barrier.facTRUE*log.VNS.hor.5,
      B_x_SWP=barrier.facTRUE*SWP.hor.5,
      B_x_CVP=barrier.facTRUE*CVP.hor.5,
      B_x_OMT=barrier.facTRUE*Qomt.hor.1net,
      R_x_Temp=route.facB*Tmsd.hor.7dadm,
      R_x_VNS=route.facB*log.VNS.hor.5,
      R_x_SWP=route.facB*SWP.hor.5,
      R_x_CVP=route.facB*CVP.hor.5,
      R_x_OMT=route.facB*Qomt.hor.1net
      )
  
  names(sel_rows_tmp4)[1] <- "(Intercept)"
  
  return(sel_rows_tmp4)

  # aic_avg_tb <- HOR_TCJ_mod_ls$HOR_TCJ_aictab[names(HOR_TCJ_mod_ls$HOR_TCJ_d2_mods),]
  # # print(aic_avg_tb)
  # aic_avg_tb_wts <- aic_avg_tb |> dplyr::mutate(AICwt=exp(-0.5*dAIC)/sum(exp(-0.5*aic_avg_tb$dAIC)))
  
  # # tmp_ls <- get_glmmTMB_ests()
  
  # tmp_HOR_TCJ_preds1 <- get_glmmTMB_ests(sel_data_in = sel_rows_tmp4,
  #                                            aic_avg_tb_wts_in=aic_avg_tb_wts,
  #                                            glmmTMB_res_ls_in=HOR_TCJ_mod_ls)
  
  # tmp_HOR_TCJ_preds2 <- dplyr::bind_rows(tmp_HOR_TCJ_preds1) |> 
  #   dplyr::group_by(sub_estimate,tmp_rw_ind) |> 
  #   dplyr::summarize(lo_pred=sum(fit*AICwt),
  #                    lo_SE=sum(AICwt*sqrt((se.fit^2)+(fit-lo_pred)^2)),
  #                    lo_SEadj=sum(AICwt*sqrt((SEadj^2)+(fit-lo_pred)^2))) |>
  #   dplyr::mutate(LCL=lo_pred-1.96*lo_SE,
  #                 UCL=lo_pred+1.96*lo_SE,
  #                 LCLadj=lo_pred-1.96*lo_SEadj,
  #                 UCLadj=lo_pred+1.96*lo_SEadj)
  
  # dplyr::bind_cols(sel_rows_tmp4,tmp_HOR_TCJ_preds2)
}


#' Title
#'
#' @param sel_rows_tmp1_in rows of 'CVhelp_dat_w' to use
#' @param flength_in fork length input scalar
#' @param YrRel_in NA as default. Year and release group factor
#'
#' @returns CVhelp_dat_w file with defined column names that match glmmTMB model.matrix so that predict.glmmTMB() can be used
#' @export
#' 
HOR_TCJ_redef_fun <- function(sel_rows_tmp1_in=sel_rows_tmp1,
                              flength_in,
                              YrRel_in=NA){
  # renaming wide-format daily measures to match glmmTMB() model design matrices
  sel_rows_tmp2 <- sel_rows_tmp1_in |> dplyr::rename(log.VNS.hor.5=VNS,
                                                     SWP.hor.5=SWP,
                                                     Tmsd.hor.7dadm=MSD,
                                                     barrier=barrierTF#,
                                                     # CVP=CVP.hor.5
  )
  sel_rows_tmp3 <- sel_rows_tmp2 |> dplyr::mutate(drought=WYT=="Critical")# special drought category
  sel_rows_tmp4 <- sel_rows_tmp3 |> dplyr::mutate(flength=flength_in ,YrRel_in=NA) # flength 
  sel_rows_tmp4
}


