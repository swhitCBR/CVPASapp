

#' Title
#'
#' @param flength_in 
#' @param DOY_in 
#' @param years_in 
#' @param TCJ_CHP_pred_comp_ls_in 
#' @param predict_int 
#'
#' @returns
#' @export
#'
#' @examples
all_mod_preds <- function(flength_in = 244,DOY_in=50:53,years_in=2012,#TCJ_CHP_pred_comp_ls_in,
                          predict_int=TRUE){
  ############ #
  ### HOR
  ############ #
  HOR_pred_tab <- HOR_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                               HOR_mod_ls=glmmTMB_mod_ls[["HOR"]],
                               flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  
  ############ #
  ### TCJ
  ############ #
  TCJ_pred_tab <- TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                               TCJ_mod_ls=glmmTMB_mod_ls[["TCJ"]],
                               flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  ########## #
  # HOR-TCJ
  ########## #
  HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                                       HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
                                       flength_in=flength_in,DOY_in=DOY_in,years_in=years_in) 
  ########## #
  # HOR-CHP
  ########## #
  HOR_CHP_viaSJL_pred_tab <- HOR_CHP_mod_wrap(SJL_route_in = TRUE,
                                              flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  
  HOR_CHP_viaORE_pred_tab <- HOR_CHP_mod_wrap(SJL_route_in = FALSE,
                                              flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  ########## #
  # TCJ-CHP
  ########## #
  TCJ_CHP_viaMAC_pred_tab <- TCJ_CHP_mod_wrap(#TCJ_CHP_pred_comp_ls_in=TCJ_CHP_pred_comp_ls_in,
                                              SJL_route_in = TRUE,
                                              flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  
  TCJ_CHP_viaTRN_pred_tab <- TCJ_CHP_mod_wrap(#TCJ_CHP_pred_comp_ls_in=TCJ_CHP_pred_comp_ls_in,
                                              SJL_route_in = FALSE,
                                              flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  ########################## #
  # Combination
  ########################## #
  pred_tab_ls<- list(
    "TCJ"=TCJ_pred_tab,
    "HOR"=HOR_pred_tab,
    "HOR_TCJviaSJL"=HOR_TCJ_pred_tab,
    "HOR_CHPviaSJL"=HOR_CHP_viaSJL_pred_tab,
    "HOR_CHPviaORE"=HOR_CHP_viaORE_pred_tab,
    "TCJ_CHPviaMAC"=TCJ_CHP_viaMAC_pred_tab,
    "TCJ_CHPviaTRN"=TCJ_CHP_viaTRN_pred_tab
  )

  return(pred_tab_ls)

}


all_mod_preds_fun2 <- function(pred_tab_ls_in,predict_int=F,conf_level=0.8){
  
  # SE_marg <- qnorm(1 - conf_level / 2)
  # SE_marg <- qnorm((1-conf_level)/2)
  SE_marg = qnorm(conf_level + (1 - conf_level) / 2)
  print(SE_marg)
  
  pred_tab_ls <- pred_tab_ls_in
  
  pred_lp <- do.call(rbind,
                     lapply(1:length(pred_tab_ls),function(ii){
                       pred_lp <- data.frame(param=names(pred_tab_ls)[ii],pred_tab_ls[[ii]][c("Year","DOY","lo_pred","lo_SEadj","lo_SE"
                                                                                              )])
                     }))
  
  pred_pDF <- pred_lp |> 
    dplyr::mutate(
      type=ifelse(param %in% c("TCJ","HOR"),"route","survival"),
      loLCL=lo_pred-SE_marg*lo_SEadj,
      loUCL=lo_pred+SE_marg*lo_SEadj,
      pr_pred=plogis(lo_pred),
      pLCL=plogis(loLCL),
      pUCL=plogis(loUCL),
      p_SEadj=plogis(lo_pred)*(1-plogis(lo_pred))*lo_SEadj, # SE_p = p x (1 - p) x SE_lo (adjusted upward based on added error)
      p_SE=plogis(lo_pred)*(1-plogis(lo_pred))*lo_SE # SE_p = p x (1 - p) x SE_lo
    )
  
  pred_pDF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
    dplyr::select(Year,DOY,param,pr_pred) |> 
    tidyr::pivot_wider(names_from=param,values_from = pr_pred) |> 
    dplyr::rename(Psi_ORE=HOR,
                  Psi_TRN=TCJ) |> 
    dplyr::mutate(Psi_SJL=1-Psi_ORE,
                  Psi_MAC=1-Psi_TRN) |>
    dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
    dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 
  
  # return(pred_pDF)
  if(predict_int){
    pred_pSE_DF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
      dplyr::select(Year,DOY,param,p_SEadj) |> 
      tidyr::pivot_wider(names_from=param,values_from = p_SEadj) |> 
      dplyr::rename(Psi_ORE=HOR,
                    Psi_TRN=TCJ) |> 
      dplyr::mutate(Psi_SJL=1-Psi_ORE,
                    Psi_MAC=1-Psi_TRN) |>
      dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
      dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 
  } else{
    pred_pSE_DF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |>
      dplyr::select(Year,DOY,param,p_SE) |>
      tidyr::pivot_wider(names_from=param,values_from = p_SE) |>
      dplyr::rename(Psi_ORE=HOR,
                    Psi_TRN=TCJ) |>
      dplyr::mutate(Psi_SJL=1-Psi_ORE,
                    Psi_MAC=1-Psi_TRN) |>
      dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
      dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE)
  }

  p_mat <- pred_pDF_w1[,which(!names(pred_pDF_w1) %in% c("Year","DOY"))]
  pSE_mat <- pred_pSE_DF_w1[,which(!names(pred_pSE_DF_w1) %in% c("Year","DOY"))]
  
  # comb_out <- get_overall_surv(E_prop = p_mat,V_prop = pSE_mat^2,conf_level = 0.95)

  deriv_comp_out <- get_overall_surv(E_prop = p_mat,V_prop = pSE_mat^2,conf_level = conf_level)
  
  comb_out <- deriv_comp_out$"deriv_ests"
  
  beta_parm_df <-deriv_comp_out$"beta_parm_df"
  
  
  
  # nonegative beta dist parameters

  
  overall_DF_full <- data.frame(pred_pDF_w1[,which(names(pred_pDF_w1) %in% c("Year","DOY"))],comb_out)
  overall_DF <- overall_DF_full #|> dplyr::select(Year,DOY,S_TCJ_CHP_mean,S_TCJ_CHP_LCL,S_TCJ_CHP_UCL,S_HOR_CHP_mean,S_HOR_CHP_LCL,S_HOR_CHP_UCL)
  
  
  
  TCH_CHP_overall_DF <- overall_DF |> dplyr::select(Year,DOY,S_TCJ_CHP_mean,S_TCJ_CHP_LCL,S_TCJ_CHP_UCL,S_TCJ_CHP_sd) |> 
    dplyr::rename(pr_pred=S_TCJ_CHP_mean,pLCL=S_TCJ_CHP_LCL,pUCL=S_TCJ_CHP_UCL,p_SEadj=S_TCJ_CHP_sd) |>
    dplyr::mutate(param="S_TCJ_CHP",type="overall_survival")
  
  HOR_CHP_overall_DF <- overall_DF |> dplyr::select(Year,DOY,S_HOR_CHP_mean,S_HOR_CHP_LCL,S_HOR_CHP_UCL,S_HOR_CHP_sd) |> 
    dplyr::rename(pr_pred=S_HOR_CHP_mean,pLCL=S_HOR_CHP_LCL,pUCL=S_HOR_CHP_UCL,p_SEadj=S_HOR_CHP_sd) |>
    dplyr::mutate(param="S_HOR_CHP",type="overall_survival")
  
  
  # ROW BINDING
  pred_pDF_comb <- dplyr::bind_rows(pred_pDF |> dplyr::select(-loLCL,-loUCL),
                                    TCH_CHP_overall_DF,
                                    HOR_CHP_overall_DF)
  
  pred_pDF_comb_w <- pred_pDF_comb |> 
    dplyr::select(Year,DOY,param,pr_pred) |> 
    tidyr::pivot_wider(names_from=param,values_from = pr_pred) |>
    dplyr::mutate(
      Psi_SJL=1-HOR,Psi_MAC=1-TCJ#,
      # S_TCJ_CHP_calc=TCJ*TCJ_CHPviaTRN+TCJ_CHPviaMAC*(1-TCJ)
      )
  # return(overall_DF)
  # return(pred_pDF_comb_w)
  
  # ind_beta_pos
  
  out_ls <- list(
    "pred_pDF_comb"=pred_pDF_comb,
    "overall_DF"=overall_DF,
    "p_mat"=p_mat,
    "pSE_mat"=pSE_mat,
    "pred_pDF_comb_w"=pred_pDF_comb_w,
    "beta_parm_df"=beta_parm_df
  )
  
  return(out_ls)
  
}


#' Title
#'
#' @param flength_in 
#' @param DOY_in 
#' @param years_in 
#' @param TCJ_CHP_pred_comp_ls_in 
#' @param predict_int 
#'
#' @returns
#' @export
#'
#' @examples
all_mod_preds <- function(flength_in = 244,DOY_in=50:53,years_in=2012,#TCJ_CHP_pred_comp_ls_in,
                          predict_int=TRUE){
  ############ #
  ### HOR
  ############ #
  HOR_pred_tab <- HOR_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                               HOR_mod_ls=glmmTMB_mod_ls[["HOR"]],
                               flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  
  ############ #
  ### TCJ
  ############ #
  TCJ_pred_tab <- TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                               TCJ_mod_ls=glmmTMB_mod_ls[["TCJ"]],
                               flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  ########## #
  # HOR-TCJ
  ########## #
  HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                                       HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
                                       flength_in=flength_in,DOY_in=DOY_in,years_in=years_in) 
  ########## #
  # HOR-CHP
  ########## #
  HOR_CHP_viaSJL_pred_tab <- HOR_CHP_mod_wrap(SJL_route_in = TRUE,
                                              flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  
  HOR_CHP_viaORE_pred_tab <- HOR_CHP_mod_wrap(SJL_route_in = FALSE,
                                              flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  ########## #
  # TCJ-CHP
  ########## #
  TCJ_CHP_viaMAC_pred_tab <- TCJ_CHP_mod_wrap(#TCJ_CHP_pred_comp_ls_in=TCJ_CHP_pred_comp_ls_in,
    SJL_route_in = TRUE,
    flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  
  TCJ_CHP_viaTRN_pred_tab <- TCJ_CHP_mod_wrap(#TCJ_CHP_pred_comp_ls_in=TCJ_CHP_pred_comp_ls_in,
    SJL_route_in = FALSE,
    flength_in=flength_in,DOY_in=DOY_in,years_in=years_in)
  ########################## #
  # Combination
  ########################## #
  pred_tab_ls<- list(
    "TCJ"=TCJ_pred_tab,
    "HOR"=HOR_pred_tab,
    "HOR_TCJviaSJL"=HOR_TCJ_pred_tab,
    "HOR_CHPviaSJL"=HOR_CHP_viaSJL_pred_tab,
    "HOR_CHPviaORE"=HOR_CHP_viaORE_pred_tab,
    "TCJ_CHPviaMAC"=TCJ_CHP_viaMAC_pred_tab,
    "TCJ_CHPviaTRN"=TCJ_CHP_viaTRN_pred_tab
  )
  
  return(pred_tab_ls)
  
}


get_all_mod_pred_mats <- function(pred_tab_ls_in,predict_int=F,conf_level=0.8){
  
  # SE_marg <- qnorm(1 - conf_level / 2)
  # SE_marg <- qnorm((1-conf_level)/2)
  SE_marg = qnorm(conf_level + (1 - conf_level) / 2)
  # print(SE_marg)
  
  pred_tab_ls <- pred_tab_ls_in
  
  pred_lp <- do.call(rbind,
                     lapply(1:length(pred_tab_ls),function(ii){
                       pred_lp <- data.frame(param=names(pred_tab_ls)[ii],pred_tab_ls[[ii]][c("Year","DOY","lo_pred","lo_SEadj","lo_SE"
                       )])
                     }))
  
  pred_pDF <- pred_lp |> 
    dplyr::mutate(
      type=ifelse(param %in% c("TCJ","HOR"),"route","survival"),
      loLCL=lo_pred-SE_marg*lo_SEadj,
      loUCL=lo_pred+SE_marg*lo_SEadj,
      pr_pred=plogis(lo_pred),
      pLCL=plogis(loLCL),
      pUCL=plogis(loUCL),
      p_SEadj=plogis(lo_pred)*(1-plogis(lo_pred))*lo_SEadj, # SE_p = p x (1 - p) x SE_lo (adjusted upward based on added error)
      p_SE=plogis(lo_pred)*(1-plogis(lo_pred))*lo_SE # SE_p = p x (1 - p) x SE_lo
    )
  
  # return(pred_pDF)
  lo_pred_pDF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
    dplyr::select(Year,DOY,param,lo_pred) |> 
    tidyr::pivot_wider(names_from=param,values_from = lo_pred) |> 
    dplyr::rename(Psi_ORE=HOR,
                  Psi_TRN=TCJ) |> 
    dplyr::mutate(Psi_SJL=1-plogis(Psi_ORE),
                  Psi_MAC=1-plogis(Psi_TRN),
                  Psi_SJL=log(Psi_SJL/(1-Psi_SJL)),
                  Psi_MAC=log(Psi_MAC/(1-Psi_MAC))
                  ) |>
    dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
    dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 
  # return(lo_pred_pDF_w1)
  
  lo_SEpred_pDF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
    dplyr::select(Year,DOY,param,lo_SE) |> 
    tidyr::pivot_wider(names_from=param,values_from = lo_SE) |> 
    dplyr::rename(Psi_ORE=HOR,
                  Psi_TRN=TCJ) |> 
    dplyr::mutate(Psi_SJL=1-Psi_ORE,
                  Psi_MAC=1-Psi_TRN) |>
    dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
    dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 
  # return(lo_SEpred_pDF_w1)
  
  loSEadj_pred_pDF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
    dplyr::select(Year,DOY,param,lo_SEadj) |> 
    tidyr::pivot_wider(names_from=param,values_from = lo_SEadj) |> 
    dplyr::rename(Psi_ORE=HOR,
                  Psi_TRN=TCJ) |> 
    dplyr::mutate(Psi_SJL=1-Psi_ORE,
                  Psi_MAC=1-Psi_TRN) |>
    dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
    dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 
  

  
  pred_pDF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
    dplyr::select(Year,DOY,param,pr_pred) |> 
    tidyr::pivot_wider(names_from=param,values_from = pr_pred) |> 
    dplyr::rename(Psi_ORE=HOR,
                  Psi_TRN=TCJ) |> 
    dplyr::mutate(Psi_SJL=1-Psi_ORE,
                  Psi_MAC=1-Psi_TRN) |>
    dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
    dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 
  
  # return(pred_pDF)
  if(predict_int){
    pred_pSE_DF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
      dplyr::select(Year,DOY,param,p_SEadj) |> 
      tidyr::pivot_wider(names_from=param,values_from = p_SEadj) |> 
      dplyr::rename(Psi_ORE=HOR,
                    Psi_TRN=TCJ) |> 
      dplyr::mutate(Psi_SJL=1-Psi_ORE,
                    Psi_MAC=1-Psi_TRN) |>
      dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
      dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 
  } else{
    pred_pSE_DF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |>
      dplyr::select(Year,DOY,param,p_SE) |>
      tidyr::pivot_wider(names_from=param,values_from = p_SE) |>
      dplyr::rename(Psi_ORE=HOR,
                    Psi_TRN=TCJ) |>
      dplyr::mutate(Psi_SJL=1-Psi_ORE,
                    Psi_MAC=1-Psi_TRN) |>
      dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
      dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE)
  }
  
  p_mat <- pred_pDF_w1[,which(!names(pred_pDF_w1) %in% c("Year","DOY"))]
  pSE_mat <- pred_pSE_DF_w1[,which(!names(pred_pSE_DF_w1) %in% c("Year","DOY"))]
  lo_pred_mat <- lo_pred_pDF_w1[,which(!names(lo_pred_pDF_w1) %in% c("Year","DOY"))]
  loSEadj_mat <- loSEadj_pred_pDF_w1[,which(!names(loSEadj_pred_pDF_w1) %in% c("Year","DOY"))]
  loSE_mat <- lo_SEpred_pDF_w1[,which(!names(lo_SEpred_pDF_w1) %in% c("Year","DOY"))]
  
  list("pred_pDF"=pred_pDF,
       "p_mat"=p_mat,
       "pSE_mat"=pSE_mat,
       "lo_pred_mat"=lo_pred_mat,
       "loSEadj_mat"=loSEadj_mat,
       "loSE_mat"=loSE_mat
       )
  
  }