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
          shiny::includeMarkdown(system.file("app/www/about/about_left_col_text.md", package = "CVPASapp"))
          ,
            div(
              style="display:inline-flex; margin-right: 10px",
              div(
            class = "thumbnail-section",
            actionButton("goto_inputs_butt", "Select Inputs",
                         class = "btn btn-primary",
                         style = "font-size: 13px; color: white; padding: 12px; text-align: center;"
            ) 
          )
          ,
          div(
            class = "thumbnail-section",
            actionButton("goto_met_ref_butt", "Methods and References",
                         class = "btn btn-primary",
                         style = "font-size: 13px; color: white;margin-left: 10px;
                          padding: 12px; text-align: center;"
            ) 
          )
          
        )

        )
        ,
        column(
          width = 7,
          # shinydashboard::box(
            tags$img(
            src = "www/about/simple_route_image.png",
            style = "width: 80%; height: 80%;",# border: 2px solid #024c63;",
            name = "Schematic view of junctions and routes through the south Delta ",
            alt = "Routes bifurcate at the Head of Old River and with the path along the San Joaquin River splitting again at Turner Cut junction; all paths converge prior to reaching Chipps Island"
            )
            ,
            tags$caption(
                        shiny::withMathJax(shiny::includeMarkdown(system.file("app/www/about_fig_cap.md",
           package = "CVPASapp"))
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

