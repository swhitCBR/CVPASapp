 draw_start_loc_ui <- function(){
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
          selected = in_selected_RV$LOC,
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