#' Dashboard UI Function
#'
#' @description module for about page of app
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      shinydashboard::box(
        title = HTML("About Title"),
        width = 12,
        solidHeader = TRUE,
        status = "primary",
          shiny::includeMarkdown(system.file("app/www/mds/about_text.md", package = "CVPASalpha"))
      )
    )
  )
}

#' about Server Functions
#'
#' @noRd
mod_about_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
  })
}

