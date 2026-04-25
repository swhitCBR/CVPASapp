#' HydroSurv main page UI Function
#'
#' @description module outlining the UI elements of the hydro surv main page
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(

      h4("main_pg_neglected")
      # tags$head(
      #   # tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
      #   tags$style("
      #   /* This is a single-line comment for a .css */
      #   /* You can put css overide code in here */
      #   "
      #   ),
      #   tags$link(
      #     rel = "stylesheet",
      #     href = "www/scripts/jquery-ui/dialog/jquery-ui.css"
      #   )
      #   ,
      #   tags$script(src = "www/scripts/jquery-ui/dialog/jquery-ui.min.js"),
      #   tags$script(src = "www/scripts/panzoom/panzoom.js")
      # )
      # ,
      # shinydashboard::box(
      #   title = HTML("Main Page Title"),
      #   width = 12,
      #   solidHeader = TRUE,
      #   status = "primary",
      #   shiny::includeMarkdown(system.file("app/www/mds/main_text.md", package = "CVPASalpha"))
      # )
      )
  )
}

#' main_page Server Functions
#'
#' @noRd
mod_main_page_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_main_page_ui("main_page_1")

## To be copied in the server
# mod_main_page_server("main_page_1")


