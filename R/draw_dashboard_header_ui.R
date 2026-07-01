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
shinydashboardPlus::dashboardHeader(
      title = "CVPAS - Steelhead",
      # the leftUi container hacked so it can s
      leftUi =  tagList(
    actionButton(
      inputId = "top_about_butt",
      label = "About",
      icon("house", style = "color: white;"),
      class = "btn-primary btn-sm",
      style="color: #fff")
    ,
    actionButton(
      # tags$style("color:white"),
      inputId = "top_met_ref_butt",
      label = "Methods and References",
      icon("book", style = "color: white;"),
      class = "btn-primary btn-sm",
      style="color: #fff"
    )
    ,
    tags$a(
      href = "https://www.cbr.washington.edu/", 
      target="_blank",
      style = "padding-top: 0px; padding-bottom: 0px;",
      tags$img(
        src = "www/cbr_logo_horiz.png",
        height = "40px",
        title = "Image of Columbia Basin Research logo"
      )
    )
  )
)
}



