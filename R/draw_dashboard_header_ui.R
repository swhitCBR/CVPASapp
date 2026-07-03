#' Title
#'
#' @returns shinydashboard header panel shinyUI code (i.e., within `shiny::tagList()`)
#' 
#' @export
#'
#' @examples
#' draw_dashboard_header_ui()
#' 
draw_dashboard_header_ui <- function(){
  
# converted from raw html using: charpente::html_2_R(draw_dashboard_header_ui())
tags$header(
  class = "main-header",
      tags$a(
      title="click to open or close left sidebar menu",
      style = "color: #F8F8F8; background-color: #024c63;",
      href = "#",
      class = "sidebar-toggle",
      `data-toggle` = "offcanvas",
      role = "button",
      tags$span(
        class = "sr-only",
        "Toggle navigation"
      )
  ),
  tags$span(
    # absolute and height style arguments ensure that bottom aligns with top bar
    style= "height:100%; display:inline-flex; position: absolute;",
    class = "logo",
    "CVPAS - Steelhead"
  ),
  tags$nav(
    class = "navbar navbar-static-top",
    role = "navigation",
    tags$span(
      style = "display:none;",
      tags$i(
        class = "fas fa-bars",
        style="transform: rotate(90deg);",
        role = "presentation",
        `aria-label` = "bars icon"
      )
    )
    ,
    tags$div(
      class = "navbar-custom-menu",
      style = "float: left; margin-left: 10px;",
      tags$ul(
        class = "nav navbar-nav",
        tags$li(
          class = "dropdown",
          style = "margin-top: 7.5px; margin-left: 5px; margin-right: 5px;",
          tags$button(
            class = "btn btn-default action-button btn-primary btn-sm",
            id = "top_about_butt",
            style = "color: #fff",
            type = "button",
            tags$i(
              class = "fas fa-house",
              role = "presentation",
              `aria-label` = "house icon",
              style = "color: white;"
            ),
            "About"
          )
        ),
        tags$li(
          class = "dropdown",
          style = "margin-top: 7.5px; margin-left: 5px; margin-right: 5px;",
          tags$button(
            class = "btn btn-default action-button btn-primary btn-sm",
            id = "top_met_ref_butt",
            style = "color: #fff",
            type = "button",
            tags$i(
              class = "fas fa-book",
              role = "presentation",
              `aria-label` = "book icon",
              style = "color: white;"
            ),
            "Methods and References"
          )
        ),
        tags$li(
          class = "dropdown",
          style = "margin-top: 7.5px; margin-left: 5px; margin-right: 5px;",
          tags$a(
            href = "https://www.cbr.washington.edu/",
            target = "_blank",
            style = "padding-top: 0px; padding-bottom: 0px;",
            tags$img(
              src = "www/cbr_logo_horiz.png",
              height = "40px",
              title = "Image of Columbia Basin Research logo"
            )
          )
        )
      )
    ),
    tags$div(
      class = "navbar-custom-menu",
      tags$ul(
        class = "nav navbar-nav",
        tags$li(tags$a(
          href = "#",
          `data-toggle` = "control-sidebar",
          tags$i(
            class = "fas fa-gears",
            role = "presentation",
            `aria-label` = "gears icon"
          )
        ))
      )
    )
  )
)
}
