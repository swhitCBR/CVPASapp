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
get_mtch_WYT_txt <- function(intab=ann_HORbar_WYT_data){
  WYT_cols_tmp <- utils_get_WYT_cols_vec()
  WYT_cols_tmp[match(intab$"WYT",names(WYT_cols_tmp))]
}
