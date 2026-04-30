#' HydroSurv main page UI Function
#'
#' @description module outlining the UI elements of the hydro surv main page
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_met_ref_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #     shinydashboard::box(
    #   title = HTML("Methods and References"),
    #   width = 12,
    #   solidHeader = TRUE,
    #   status = "primary",
    #   shiny::withMathJax(shiny::includeMarkdown(system.file("app/www/main/met_and_ref/overview_pt1.md", package = "CVPASapp")))
    #   ,
    #   tags$img(
    #   src = "www/simple_route_image.png",
    #   style = "width: 400px; height: auto; border: 2px solid #024c63;",
    #   name = "placeholder text",
    #   alt = "placeholder text"
    #   )
    #   ,
    #   shiny::withMathJax(shiny::includeMarkdown(system.file("app/www/main/met_and_ref/how_calc_pt2.md", package = "CVPASapp")))
    #   ,
    #   bscui::bscuiOutput(outputId = "surv_route_diagram_wtt", width = "50%", height = "100%") # not the absence of ns() function here bc render occurs on server
    #   ,
    #   bscui::bscuiOutput(outputId = "my_red_svg", width = "100%", height = "100%")
    #   ,
    #   shiny::withMathJax(shiny::includeMarkdown(system.file("app/www/main/met_and_ref/left_col_text.md", package = "CVPASapp")))
    #   ,
    #   shiny::includeMarkdown(system.file("app/www/biblio_doc.md", package = "CVPASapp"))
    # )
  )
    # fluidRow(

    #   h4("met_ref_pg_neglected")
    #   # tags$head(
    #   #   # tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
    #   #   tags$style("
    #   #   /* This is a single-line comment for a .css */
    #   #   /* You can put css overide code in here */
    #   #   "
    #   #   ),
    #   #   tags$link(
    #   #     rel = "stylesheet",
    #   #     href = "www/scripts/jquery-ui/dialog/jquery-ui.css"
    #   #   )
    #   #   ,
    #   #   tags$script(src = "www/scripts/jquery-ui/dialog/jquery-ui.min.js"),
    #   #   tags$script(src = "www/scripts/panzoom/panzoom.js")
    #   # )
    #   # ,
    #   # shinydashboard::box(
    #   #   title = HTML("Main Page Title"),
    #   #   width = 12,
    #   #   solidHeader = TRUE,
    #   #   status = "primary",
    #   #   shiny::includeMarkdown(system.file("app/www/mds/main_text.md", package = "CVPASalpha"))
    #   # )
    #   )
  # )
}

#' met_ref_page Server Functions
#'
#' @noRd
mod_met_ref_page_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_met_ref_page_ui("met_ref_page_1")

## To be copied in the server
# mod_met_ref_page_server("met_ref_page_1")


