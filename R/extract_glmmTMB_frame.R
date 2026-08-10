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
