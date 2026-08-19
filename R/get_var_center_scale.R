#' Title
#'
#' @details Extract and scaled variable information
#'
#' @param TMB_mod_ls named list containing pre-existing TMB models
#'
#' @returns dataframe with columns correponding to the scaled variable name, its center (mean), and its scale (sd)
#'
#' @export
get_var_center_scale <- function(TMB_mod_ls){
  element_nms <- c("scaled_vars","center","scale")
  
  df <- data.frame(sapply(element_nms,function(x) attributes(TMB_mod_ls$"XX_in")[[x]]))
  df$"center" <- as.numeric(df$"center")
  df$"scale" <- as.numeric(df$"scale")
  return(df)
  
  # old implementation
  df_tmp <- data.frame(dplyr::bind_cols(
  lapply(element_nms,function(x) attributes(TMB_mod_ls$"XX_in")[[x]])))
  names(df_tmp) <- element_nms
  df_tmp
  }

