#' Title
#'
#' @param init_data_source_in 
#'
#' @returns
#' @export
#'
#' @examples
#' draw_inputs_star_loc_datsrc_col_ui(init_data_source_in="Previous year")
#' 
draw_inputs_star_loc_datsrc_col_ui <- function(init_data_source_in){
  column(
    width = 7,
    shiny::HTML("<h4> <b> Overview: </b>  </h4>"),
    p(
      "This tool provides predictions of survival and route usage for hypothetical releases of juvenile Steelhead 
      based on their expected location,conditions in the environment, and individual size."
    ),
    tags$ul(
      tags$li(
        "Select a junction to serve as a starting location for the migration",
        tags$ul(
          tags$li(
            style = "list-style-type: none;",
            draw_start_loc_ui()
          )
        )
      ),
      tags$li(
        "Select the source to use for daily hydrologic data and export operations;",
        br(),
        "either: (1) a previous year or (2) a user-provided data set",
        tags$ul(
          tags$li(
            style = "list-style-type: none;",
            div(
              style = "display: inline-flex; align-items: ;margin-top: 10px;",
              div(
                style = "margin-top: 0px; margin-right: 10px;font-weight:bold",
                shiny::HTML("<h5> <b> Source: </b>  </h4>")
              )
              ,
              div(
                style = "height=15px",
                shinyWidgets::pickerInput(
                  inputId = "data_source_picker",
                  choices = c(
                    "Previous year",
                    "Uploaded file (.csv)",
                    "None"
                  ),
                  width = "180px",
                  selected = init_data_source_in
                )
              )
            )
          )
        )
      )
    )
  )
}