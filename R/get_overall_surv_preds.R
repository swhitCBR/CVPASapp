#' Title
#'
#' @param pred_tab_ls_in table of predictions from component model sets `XXXX_mod_wrap()` functions
#' @param predict_int whether to include release group level erroradjSE
#' @param conf_level interval confidence level, based on Z-stat. Defaults to 0.95 (i.e., 1.96)
#'
#' @returns named list
#' @export
#'
#' @examples
get_overall_surv_preds <- function(pred_tab_ls_in,predict_int=F,conf_level=0.95){
  
  SE_marg = qnorm(conf_level + (1 - conf_level) / 2)
  
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
  
  # logit(1-p) = -logit(p), so the logit-scale complements of HOR/TCJ
  # (Psi_SJL = 1-Psi_ORE, Psi_MAC = 1-Psi_TRN on the proportion scale) are
  # simply their negatives on the logit scale -- no plogis()/log() r
  lo_pred_pDF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
    dplyr::select(Year,DOY,param,lo_pred) |> 
    tidyr::pivot_wider(names_from=param,values_from = lo_pred) |> 
    dplyr::mutate(lo_Psi_SJL=-HOR,
                  lo_Psi_MAC=-TCJ) |>
    dplyr::select(-HOR_CHPviaSJL,-HOR,-TCJ) |>
    dplyr::relocate(Year,DOY,lo_Psi_SJL,HOR_TCJviaSJL,lo_Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 
  
  # Logit-scale SE matrix, built the same way as lo_pred_pDF_w1 above,
  # respecting the predict_int switch. SE is unaffected by the sign flip
  # used for lo_Psi_SJL/lo_Psi_MAC (SE(-x) = SE(x)).
  if(predict_int){
    lo_pred_SE_pDF_w1 <- pred_pDF |>
      dplyr::select(Year,DOY,param,lo_SEadj) |>
      tidyr::pivot_wider(names_from=param,values_from = lo_SEadj) |>
      dplyr::mutate(lo_Psi_SJL=HOR,
                    lo_Psi_MAC=TCJ) |>
      dplyr::select(-HOR_CHPviaSJL,-HOR,-TCJ) |>
      dplyr::relocate(Year,DOY,lo_Psi_SJL,HOR_TCJviaSJL,lo_Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE)
  } else {
    lo_pred_SE_pDF_w1 <- pred_pDF |>
      dplyr::select(Year,DOY,param,lo_SE) |>
      tidyr::pivot_wider(names_from=param,values_from = lo_SE) |>
      dplyr::mutate(lo_Psi_SJL=HOR,
                    lo_Psi_MAC=TCJ) |>
      dplyr::select(-HOR_CHPviaSJL,-HOR,-TCJ) |>
      dplyr::relocate(Year,DOY,lo_Psi_SJL,HOR_TCJviaSJL,lo_Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE)
  }
  
  lo_pred_mat <- lo_pred_pDF_w1[,which(!names(lo_pred_pDF_w1) %in% c("Year","DOY"))]
  lo_pred_SE_mat <- lo_pred_SE_pDF_w1[,which(!names(lo_pred_SE_pDF_w1) %in% c("Year","DOY"))]
  
  deriv_comp_out <- get_overall_surv_logit(lo_comp_estmat = lo_pred_mat,lo_comp_varmat = lo_pred_SE_mat^2,conf_level = conf_level)
  
  comb_out <- deriv_comp_out$"deriv_ests"
  
  overall_DF_full <- data.frame(lo_pred_pDF_w1[,which(names(lo_pred_pDF_w1) %in% c("Year","DOY"))],comb_out)
  overall_DF <- overall_DF_full
  
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
  
  out_ls <- list(
    "pred_pDF_comb"=pred_pDF_comb,
    "overall_DF"=overall_DF,
    "pred_pDF_comb_w"=pred_pDF_comb_w,
    "deriv_ests"=comb_out,
    "bias_correction_df"=deriv_comp_out$"bias_correction_df"
  )
  
  return(out_ls)
  
}
