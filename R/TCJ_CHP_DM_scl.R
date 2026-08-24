
TCJ_CHP_DM_scl <- function(TCJ_CHP_comp_ls_unscl_in
                           ,
                           sel_rows_tmp1=CVhelp_dat_w
                           ,
                           flength_in=240
                           ,
                           SJL_route_in=TRUE
                           ) {
  
  
  sclabl_var_nms <- c("flength","Qomt.tcj.1net","log.VNS.tcj","CVP.tcj.4","SWP.tcj.4","Tmsd.tcj.7dadm")
  
  tmp_scl <-attributes(scale(TCJ_CHP_comp_ls_unscl_in$XX_in_w_int_WYT))
  tmp_df <- data.frame(
    scaled_vars=sclabl_var_nms,
    center=tmp_scl[["scaled:center"]][sclabl_var_nms],
    scale=tmp_scl[["scaled:scale"]][sclabl_var_nms])
  
  scl_var_tb <- tmp_df
  
  
  # return(tmp_df)
  stopifnot(flength_in >= 100 & flength_in <= 400)
  # unscaledTF <- !all(attributes(HOR_CHP_mod_ls$XX_in)$center==0)
  unscaledTF <- TRUE
  
  # tmp_ls=extract_glmmTMB_frame(glmmTMB_res_ls_in=HOR_TCJ_mod_ls)
  # HOR_CHP_mod_ls$TMB_data_baseline$XX_pred_mat <- HOR_CHP_mod_ls$TMB_data_baseline$XX_s[1:10,]
  # tmp_ls <- HOR_CHP_mod_ls$TMB_data_baseline$XX_pred_mat
  # return(tmp_ls)
  
  # # renaming wide-format daily measures to match glmmTMB() model design matrices
  sel_rows_tmp2 <- sel_rows_tmp1 |> dplyr::rename(Qomt.tcj.1net=OMT,
                                                  log.VNS.tcj=VNS,
                                                  CVP.tcj.4=CVP,
                                                  SWP.tcj.4=SWP,
                                                  Tmsd.tcj.7dadm=MSD,
                                                  )
  
  if(unscaledTF){
    message("using scaled version")
    
    # scl_var_tb <- get_var_center_scale(TMB_mod_ls=HOR_CHP_mod_ls)
    
    sel_rows_tmp2 <- sel_rows_tmp2 |> dplyr::mutate(
      
      flength=(flength_in-scl_var_tb$center[scl_var_tb$scaled_vars=="flength"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="flength"],
      
      Qomt.tcj.1net=(Qomt.tcj.1net-scl_var_tb$center[scl_var_tb$scaled_vars=="Qomt.tcj.1net"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="Qomt.tcj.1net"],
      
      log.VNS.tcj=(log.VNS.tcj-scl_var_tb$center[scl_var_tb$scaled_vars=="log.VNS.tcj"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="log.VNS.tcj"],
      
      CVP.tcj.4=(CVP.tcj.4-scl_var_tb$center[scl_var_tb$scaled_vars=="CVP.tcj.4"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="CVP.tcj.4"],
      
      SWP.tcj.4=(SWP.tcj.4-scl_var_tb$center[scl_var_tb$scaled_vars=="SWP.tcj.4"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="SWP.tcj.4"],
      
      Tmsd.tcj.7dadm=(Tmsd.tcj.7dadm-scl_var_tb$center[scl_var_tb$scaled_vars=="Tmsd.tcj.7dadm"])/scl_var_tb$scale[scl_var_tb$scaled_vars=="Tmsd.tcj.7dadm"]
    )
    # print("ha")
    # return(sel_rows_tmp2)
  }
  
  
  
  sel_rows_tmp3 <- sel_rows_tmp2 |> dplyr::mutate(
    Intercept=1,
    WYT_drought=as.numeric(WYT=="Critical"), # similar to plain drought category
    WYT_wet=as.numeric(WYT=="Wet")  )# special drought category
  
  sel_rows_tmp4 <- sel_rows_tmp3 |> 
    dplyr::mutate(
      # flength=flength_in,
      flength=ifelse(unscaledTF,flength,flength_in),
      # alternate TCJ route usage
      TCJroute.facF=ifelse(SJL_route_in,0,1),
      YrRel=NA) |>
    dplyr::relocate(Intercept) |>
    dplyr::mutate(
      R_x_SWP=TCJroute.facF*SWP.tcj.4,
      R_x_CVP=TCJroute.facF*CVP.tcj.4,
      R_x_OMT=TCJroute.facF*Qomt.tcj.1net
    )
  
  names(sel_rows_tmp4)[1] <- "(Intercept)"
  return(sel_rows_tmp4)
  
  plot_in=FALSE
  if(plot_in==TRUE){
  par(mfrow=c(2,1))
  hist(sel_rows_tmp4$Qomt.tcj.1net,main="OMT")
  hist(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s[,"Qomt.tcj.1net"],col=4,,xlim=c(-7,7),add=T)
  
  hist(sel_rows_tmp4$log.VNS.tcj,main="VNS")
  hist(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s[,"log.VNS.tcj"],col=4,,xlim=c(-7,7),add=T)
  
  hist(sel_rows_tmp4$SWP.tcj.4,main="SWP")
  hist(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s[,"SWP.tcj.4"],col=4,,xlim=c(-7,7),add=T)
  
  hist(sel_rows_tmp4$CVP.tcj.4,main="CVP")
  hist(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s[,"CVP.tcj.4"],col=4,,xlim=c(-7,7),add=T)
  
  hist(sel_rows_tmp4$Tmsd.tcj.7dadm,xlim=c(-7,7),main="MSD")
  hist(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s[,"Tmsd.tcj.7dadm"],col=4,,xlim=c(-7,7),add=T)
  }
  # head(sel_rows_tmp4)
  # head(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s)
  
  mtch_col_inds <- match(colnames(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s),names(sel_rows_tmp4))
  # head(sel_rows_tmp4[,mtch_col_inds])
  # str(TCJ_CHP_TMB_all_mods$"TMB_data_baseline"$XX_s)
  # str(as.matrix(sel_rows_tmp4[,mtch_col_inds]))
  
  pred_mat_XX_s <- as.matrix(sel_rows_tmp4[,mtch_col_inds])
  
  return(pred_mat_XX_s)
  
  # extra output
  list(
    "scl_var_tb"=scl_var_tb,
    "pred_mat_XX_s"=pred_mat_XX_s)
  
  
  
  
}
