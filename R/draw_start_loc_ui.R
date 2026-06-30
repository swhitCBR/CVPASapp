#' Title
#'
#' @param loc_selected_in
#'
#' @returns R shiny UI elements
#' 
#' @family {ui builders }
#' @family {front-end functions }
#' 
#' @export
#' 
#' @examples
#' draw_start_loc_ui()
#' 
draw_start_loc_ui <- function(){
    div(
      style = "display: inline-flex; align-items: left;margin-top: 10px;",
      div(
        style = "height=25px; align-self:center",
        shinyWidgets::pickerInput(
          label = "Starting Location:",
          inline = T,
          width = "fit",
          'start_loc_in',
          choices = c("HOR","TCJ"),
          selected = "HOR",
          ,
          choicesOpt = list(
            style = paste0(
              "background-color:",
              c("#67AB9F", "#FF3399"),
              ";"
            )
          )
        )
      )
    )
  }