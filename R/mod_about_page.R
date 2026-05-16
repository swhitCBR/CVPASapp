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
            src = "www/simple_route_image.png",
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
        ,
        div(
          id = "thumbnailModal",
          title = "Diagram of Covariates",
          tabindex = "-1",
          style = "display: none;",  # Hide the modal by default using inline CSS
          div(
            class = "modal-thumbnail-dialog",
            div(
              class = "modal-thumbnail-content",
              div(
                id = "modal-thumbnail-body",
                img(
                  id = "modalThumbnailImage",
                  src = "www/simple_route_image.png",
                  alt = "Diagram of the Columbia River System (Snake and Columbia rivers, Pacific Northwest, USA),
                  the Columbia River Estuary and Pacific Ocean, including the detection sites (capitalized text), reaches (thick arrows),
                  and covariates (thin arrows) associated with survival that were used in the hierarchical Bayesian CJS model."
                ),
                div(
                  class = "modal-thumbnail-caption",
                  HTML(
                    "
                        <em>Figure 1. Diagram of the Columbia River System (Snake and Columbia rivers, Pacific Northwest, USA),
                        the Columbia River Estuary and Pacific Ocean, including the detection sites (capitalized text), reaches (thick arrows),
                        and covariates (thin arrows) associated with survival that were used in the hierarchical Bayesian CJS model.
                        The juvenile detection sites are at Lower Granite Dam (LGR), Bonneville Dam (BON), and the estuary trawl towed array (TWX);
                        and the adult detection sites are at Bonneville Dam (BOA) and Lower Granite Dam (LGA).
                        Blue represents the downstream river covariates and survival reach. Purple represents the ocean covariates and survival reach,
                        and green represents the upstream river covariates and survival reach.</em>
                    "
                  )
                )
              )
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

