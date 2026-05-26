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