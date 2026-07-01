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

