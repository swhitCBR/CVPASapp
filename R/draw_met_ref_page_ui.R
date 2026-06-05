#' Title
#'
#' @returns
#' @export
#'
draw_met_ref_page_ui <- function(){
  shiny::tagList(
  fluidRow(
    tagList(
      shinydashboard::box(
        title = HTML("Methods and References"),
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shiny::withMathJax(shiny::includeMarkdown(system.file(
          "app/www/met_and_ref/overview_pt1.md",
          package = "CVPASapp"
        ))),
        # SVG plot with embedded tooltips
        bscui::bscuiOutput(
          outputId = "surv_route_diagram_wtt",
          width = "70%",
          height = "100%"
        ),
        shiny::withMathJax(shiny::includeMarkdown(system.file(
          "app/www/met_and_ref/how_calc_pt2.md",
          package = "CVPASapp"
        ))),
        shiny::withMathJax(shiny::includeMarkdown(system.file(
          "app/www/met_and_ref/how_env_comp.md",
          package = "CVPASapp"
        ))),
        # plots from manuscripts
        tags$img(
          src = "https://onlinelibrary.wiley.com/cms/asset/25dec1bb-e0e1-40ea-95e2-f7762044473f/nafm11005-fig-0001-m.jpg",
          style = "width: 30%; height: 30%;"
        ),
        tags$img(
          src = "https://cdnsciencepub.com/cms/10.1139/cjfas-2020-0467/asset/images/large/cjfas-2020-0467f1.jpeg",
          style = "width: 30%; height: 30%;"
        ),
        shiny::includeMarkdown(system.file(
          "app/www/biblio_doc.md",
          package = "CVPASapp"
        ))
      )
    )
  )
)
}