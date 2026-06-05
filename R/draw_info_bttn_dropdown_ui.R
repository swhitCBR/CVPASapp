#' Title
#'
#' @param inputId_in 
#'
#' @returns
#' @export
#'
#' @examples
draw_ibutt_dropdown_ui <- function(
  inputId_in = "daily_var_def_ibutt2",
  ibox_content_label="daily_values_box"
){

  # info_box_contents = get_ibox_contents(ibox_content_label="daily_values_box")

  shinyWidgets::dropMenu(
    shinyWidgets::circleButton(
      inputId = inputId_in,
      icon = icon("info"),
      status = "primary",
      size = "xs",
    ),
    get_ibox_contents(ibox_content_label),
    # info_box_contents,
    placement = "left-start"
  )
}

