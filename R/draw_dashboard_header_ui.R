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
  # dashboardHeader_MOD(
#   shinydashboardPlus::dashboardHeader(
#       title = "CVPAS - Steelhead",
#       # the leftUi container hacked so it can s
#       leftUi =  tagList(
#     actionButton(
#       inputId = "top_about_butt",
#       label = "About",
#       icon("house", style = "color: white;"),
#       class = "btn-primary btn-sm",
#       style="color: #fff")
#     ,
#     actionButton(
#       # tags$style("color:white"),
#       inputId = "top_met_ref_butt",
#       label = "Methods and References",
#       icon("book", style = "color: white;"),
#       class = "btn-primary btn-sm",
#       style="color: #fff"
#     )
#     ,
#     tags$a(
#       href = "https://www.cbr.washington.edu/", 
#       target="_blank",
#       style = "padding-top: 0px; padding-bottom: 0px;",
#       tags$img(
#         src = "www/cbr_logo_horiz.png",
#         height = "40px",
#         title = "Image of Columbia Basin Research logo"
#       )
#     )
#   )
# )
# 
# shiny::tagList(
#   shiny::HTML('
#   <header class="main-header">
#   <span class="logo">CVPAS - Steelhead</span>
#   <nav class="navbar navbar-static-top" role="navigation">
#     <span style="display:none;">
#       <i class="fas fa-bars" role="presentation" aria-label="bars icon"></i>
#     </span>
#     <a href="#" class="sidebar-toggle" data-toggle="offcanvas" role="button">
#       <span class="sr-only">Toggle navigation</span>
#     </a>
#     <div class="navbar-custom-menu" style="float: left; margin-left: 10px;">
#       <ul class="nav navbar-nav">
#         <li class="dropdown" style="margin-top: 7.5px; margin-left: 5px; margin-right: 5px;">
#           <button class="btn btn-default action-button btn-primary btn-sm" id="top_about_butt" style="color: #fff" type="button">
#             <i class="fas fa-house" role="presentation" aria-label="house icon" style="color: white;"></i>
#             About
#           </button>
#         </li>
#         <li class="dropdown" style="margin-top: 7.5px; margin-left: 5px; margin-right: 5px;">
#           <button class="btn btn-default action-button btn-primary btn-sm" id="top_met_ref_butt" style="color: #fff" type="button">
#             <i class="fas fa-book" role="presentation" aria-label="book icon" style="color: white;"></i>
#             Methods and References
#           </button>
#         </li>
#         <li class="dropdown" style="margin-top: 7.5px; margin-left: 5px; margin-right: 5px;">
#           <a href="https://www.cbr.washington.edu/" target="_blank" style="padding-top: 0px; padding-bottom: 0px;">
#             <img src="www/cbr_logo_horiz.png" height="40px" title="Image of Columbia Basin Research logo"/>
#           </a>
#         </li>
#       </ul>
#     </div>
#     <div class="navbar-custom-menu">
#       <ul class="nav navbar-nav">
#         <li>
#           <a href="#" data-toggle="control-sidebar">
#             <i class="fas fa-gears" role="presentation" aria-label="gears icon"></i>
#           </a>
#         </li>
#       </ul>
#     </div>
#   </nav>
# </header>
#   ')
# )
  
# converted from raw html using: charpente::html_2_R(draw_dashboard_header_ui())
tags$header(
  class = "main-header",
      tags$a(
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
    style= "height:100%; display:inline-flex; position: absolute;",
    class = "logo",
    # style="height:50.98px",
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
    ),
    # tags$a(
    #   href = "#",
    #   class = "sidebar-toggle",
    #   `data-toggle` = "offcanvas",
    #   role = "button",
    #   tags$span(
    #     class = "sr-only",
    #     "Toggle navigation"
    #   )
    # ),
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

