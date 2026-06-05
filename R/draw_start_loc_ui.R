#' Title
#'
#' @param loc_selected_in
#'
#' @returns
#'
#' @export
#' @examples
draw_start_loc_ui <- function(){
# draw_start_loc_ui <- function(loc_selected_in=in_selected_RV$LOC){
  #  output$start_loc_ui <- renderUI({
    div(
      style = "display: inline-flex; align-items: left;margin-top: 10px;",
      div(
        style = "height=25px; align-self:center",
        shinyWidgets::pickerInput(
          # label="Junction:",
          label = "Starting Location:",
          inline = T,
          width = "fit",
          'start_loc_in',
          choices = loc_opt_nms,
          # selected = loc_selected_in,
          selected = "HOR",
          ,
          choicesOpt = list(
            style = paste0(
              "background-color:",
              loc_opt_cols,
              ";"
            )
          )
        )
      )
    )
  }