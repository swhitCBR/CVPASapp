draw_indiv_attribs_details_ui <- function(){
      conditionalPanel(
         condition = "input.data_source_picker != 'None'",
      tagList(
        tags$details(
          id = "details_indiv",
          style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px;", # ; background-color:white",
          tags$summary(
            title = "Click to open or close",
            "Select Individual Attributes",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px; padding-bottom: 2px;background-color:#ddd;"
          ),
          fluidRow(
            style = "padding-inline-start: 15px;",
            column(
              width = 6,
              h5(strong("Fork Length")),
              tags$ul(
                style = "padding-inline-start: 20px;",
                tags$li(
                  "Uses average fork lengths of juvenile Steelhead used in modeling by default."
                )
              )
            ),
            column(width = 6, shiny::uiOutput("flength_sel_ui"))
          )
        )
      )
    )
}
