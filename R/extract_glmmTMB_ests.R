#' Title
#'
#' @param glmmTMB_res_ls_in 
#' @param mods_obj_nm 
#' @param sel_data_in 
#' @param aic_avg_tb_wts_in 
#'
#' @returns
#' @export
#'
#' @examples
extract_glmmTMB_ests <- function(glmmTMB_res_ls_in,
                                 mods_obj_nm='HOR_TCJ_d2_mods',
                                 sel_data_in=sel_rows_tmp4,
                                 aic_avg_tb_wts_in=aic_avg_tb_wts){
  require(glmmTMB)
  frm_ls <- lapply(1:length(glmmTMB_res_ls_in),
                   function(ii){
                     data.frame(
                       sub_estimate="HOR_TCJ",
                       tmp_rw_ind=1:nrow(sel_data_in),
                       mod_form=names(glmmTMB_res_ls_in[[mods_obj_nm]])[ii],
                       AICwt=aic_avg_tb_wts_in[names(glmmTMB_res_ls_in[[mods_obj_nm]])[ii],]$"AICwt",
                       RE_sigma=glmmTMB_res_ls_in[[mods_obj_nm]][[ii]]$obj$report()$sd[[1]],
                       glmmTMB:::predict.glmmTMB(glmmTMB_res_ls_in[[mods_obj_nm]][[ii]],
                                                 newdata = sel_data_in,se.fit = T)) |>
                       dplyr::mutate(
                         SEadj=sqrt((se.fit^2)+(RE_sigma^2)),
                         LCL=fit-1.96*se.fit,
                         UCL=fit+1.96*se.fit,
                         LCLadj=fit-1.96*SEadj,
                         UCLadj=fit+1.96*SEadj)
                   })
  frm_ls
}
