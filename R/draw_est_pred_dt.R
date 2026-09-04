#' Create a data table for estimated predictions
#'
#' @param pred_data Data frame containing prediction results (OUT_tmp$pred_pDF_comb)
#'
#' @returns A DT datatable object
#' @export
#'
#' @examples
#' draw_est_pred_dt(pred_data)
#'
draw_est_pred_dt <- function(pred_data) {
  
  # Remove specified columns
  cols_to_remove <- c("lo_pred", "lo_SEadj", "lo_SE", "p_SE")
  pred_data_filtered <- pred_data |> 
    dplyr::select(-dplyr::any_of(cols_to_remove)) |>
    dplyr::rename(
      Parameter = "param",
      Type = "type",
      Estimate = "pr_pred",
      LCL = "pLCL",
      UCL = "pUCL",
      SE = "p_SEadj"
    ) |>
    dplyr::relocate(Year, DOY, Parameter, Type, Estimate, SE, LCL, UCL)
  
  # Create datatable
  dt <- DT::datatable(
    pred_data_filtered,
    selection = "none",
    rownames = FALSE,
    options = list(
      info = FALSE,
      dom = '<"<"bottom"ip>',
      pageLength = 6,
      stripeClasses = list(),
      pagingType = "simple",
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'font-size': '100%'});",
        "$(this.api().table().body()).css({'font-size': '80%'});",
        "$(this.api().table().footer()).css({'font-size': '80%'});",
        "}"
      )
    )
  )
  
  # Round numeric columns to 3 decimal places
  numeric_cols <- which(sapply(pred_data_filtered, is.numeric))
  if (length(numeric_cols) > 0) {
    dt <- dt |> DT::formatRound(columns = numeric_cols, digits = 3)
  }
  
  # Format Year column as integer without comma separator
  if ("Year" %in% names(pred_data_filtered)) {
    dt <- dt |> DT::formatString(columns = which(names(pred_data_filtered) == "Year"), 
                                   prefix = "", suffix = "")
  }
  
  # Round DOY column to 0 decimal places (integers)
  if ("DOY" %in% names(pred_data_filtered)) {
    dt <- dt |> DT::formatRound(columns = which(names(pred_data_filtered) == "DOY"), digits = 0)
  }
  
  return(dt)
}
