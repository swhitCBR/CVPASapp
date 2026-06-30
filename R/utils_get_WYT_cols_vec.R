#' Title
#'
#' @returns of Water Year Type colors with name attributes
#'
#' @export
#' 
#' @examples
#' utils_get_WYT_cols_vec()
#' 
utils_get_WYT_cols_vec <- function(){
  WYT_cols <- c("#3399FF",  "#99EEFF", "#FFFFCC","#FFCC66", "#FF5500")
  names(WYT_cols) <- c(
      "Wet",
      "Above Normal",
      "Below Normal",
      "Dry",
      "Critical") # new addition
  return(WYT_cols)
}

#' Title
#'
#' @param intab dataframe with WYT column for matching
#'
#' @returns matching color associated with name attribut for color vector
#'
#' @export
#' @examples
#' WYT_cols <- utils_get_WYT_cols_vec()
#' WYT_cols[match(ann_HORbar_WYT_data$WYT,names(WYT_cols))]
#'
mtch_WYT <- function(intab=ann_HORbar_WYT_data){
  WYT_cols_tmp <- utils_get_WYT_cols_vec()
  WYT_cols_tmp[match(intab$"WYT",names(WYT_cols_tmp))]
}
