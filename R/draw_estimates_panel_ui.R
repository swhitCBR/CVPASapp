#' create ui for estimate panels
#'
#' @returns a `shiny.tag` object inside a `shinydashboardPlus::box()``
#'
#' @export
#'
#' @examples
#' draw_est_box_ui()
#'
draw_est_box_ui <- function() {
  shinydashboardPlus::box(
    id = "est_box_ui",
    title = tags$a(span(h2(
      "Estimates",
      style = "margin-top: 0px; margin-bottom: 0px;"
    ))),
    solidHeader = TRUE,
    status = "primary",
    collapsible = T,
    collapsed = FALSE,
    width = 12,
    fluidRow(
      # Overall Survival
      draw_est_ovr_surv_div_ui()
      ,
      hr()
      ,
      # Reach Survival 
      draw_est_rch_surv_div_ui()
      ,
      hr()
      ,
      # Route-Specific Survival
      draw_est_rte_spec_surv_div_ui()
      ,
      hr()
      ,
      # Route Use
      draw_est_rte_div_ui()
    )
    ,
    footer = 
      draw_est_more_info_div_ui()
  )
}
