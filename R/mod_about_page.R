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
        title = HTML("About"),
        # title = HTML("Purpose <small style ='font-size:0.6em; color: white;'>BetaVersion.Nov05</small>"),
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        column(
          width = 5
          ,
          shiny::includeMarkdown(system.file("app/www/about_left_col_text.md", package = "CVPASapp"))
          ,
            div(
            class = "thumbnail-section",
            actionButton("goto_inputs_butt", "Select Inputs",
                         class = "btn btn-primary",
                         style = "font-size: 14px; color: white;
                  padding: 12px; text-align: center;"
            ) 
          )

        )
        ,
        column(
          width = 7,
          shinydashboard::box(
            tags$img(
            src = "www/simple_route_image.png",
            style = "width: 100%; height: auto;",# border: 2px solid #024c63;",
            name = "plasceholder text",
            alt = "plasceholder text"
            )
            ),
          shiny::withMathJax(shiny::includeMarkdown(system.file("app/www/about_fig_cap.md",
           package = "CVPASapp"))
          ),
                    div(
            class = "thumbnail-section",
            actionButton("show_modal_1", "Show Welcome Pop-up",
                         class = "btn btn-primary",
                         style = "font-size: 14px; color: white;
                  padding: 8px; text-align: center;"
            ) 
          )
        )
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

