#' Title
#'
#' @param inputId_in 
#'
#' @returns `shiny.tag` object inside a [shinyWidgets::dropMenu]
#' 
#' @export
#'
#' 
draw_ibutt_dropdown_ui <- function(
  inputId_in,
  ibox_content_label="daily_values_box",
  width_in="100%"
){

  # info_box_contents = draw_ibox_ui(ibox_content_label="daily_values_box")

  shinyWidgets::dropMenu(
    style="width=600px;",
    shinyWidgets::circleButton(
      inputId = inputId_in,
      icon = icon("info"),
      status = "primary",
      size = "xs",
    ),
    width=width_in,
    draw_ibox_ui(ibox_content_label),
    # info_box_contents,
    placement = "left-start"
  )
}

