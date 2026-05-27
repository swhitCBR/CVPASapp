
#' Title
#'
#' @param TMB_mod_ls
#'
#' @returns
#'
#' @export
#' @examples
get_var_center_scale <- function(TMB_mod_ls){
  element_nms <- c("scaled_vars","center","scale")
  df_tmp <- data.frame(dplyr::bind_cols(lapply(element_nms,
  function(x) attributes(TMB_mod_ls$"XX_in")[[x]])))
  names(df_tmp) <- element_nms
  df_tmp}

