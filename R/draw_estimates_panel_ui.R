#' create ui for estimate panels
#'
#' @returns a `shiny.tag` object inside a `shinydashboardPlus::box()``
#'
#' @export
#'
#' @examples
#' draw_estimates_panel_ui()
#'
draw_estimates_panel_ui <- function() {
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
      draw_ovr_surv_est_sec_ui()
      ,
      hr()
      ,
      # Reach Survival 
      draw_reach_surv_est_sec_ui()
      ,
      hr()
      ,
      # Route-Specific Survival
      draw_rte_spec_surv_est_sec_ui()
      ,
      hr()
      ,
      # Route Use
      draw_rte_use_est_sec_ui()
    ),
    footer = div(
      style = "display: none;",
      div(
        id = "more_info_panel",
        style = "padding-left: 10px;",
        h3(
          paste0("More Information"),
          style = "color:black;text-decoration: underline;"
        )
      ),
      div(
        style = "padding-left: 10px;",
        tags$details(
          id = "hor_tcj_surv_deet",
          open = NULL,
          style = "margin-top:15px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
          tags$summary(
            title = "Click to open or close",
            "Route-Specific Survival",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
          ),
          h4(
            "Head of Old River to Turner Cut via the San Joaquin River",
            style = "margin-left:20px;"
          ),
          div(
            style = "display:flex;",
            plotOutput("doy_ins_ggpplt", height = "400px") #,
            # plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
          ),
          h4(
            "Turner Cut Chipps Island via the San Joaquin River",
            style = "margin-left:20px;"
          ),
          div(
            style = "display:flex;",
            plotOutput("doy_ins_ggpplt_dup1", height = "400px"),
            plotOutput("HOR_TCJ_pred_ggpplt_dup3", height = "400px")
          ),
          h4(
            "Head of Old River to Chipps Island via the Old and Middle rivers",
            style = "margin-left:20px;"
          ),
          div(
            style = "display:flex;",
            plotOutput("doy_ins_ggpplt_dup2", height = "400px"),
            plotOutput("HOR_TCJ_pred_ggpplt_dup1", height = "400px")
          )
        ),
        tags$details(
          id = "hor_chp_ore_surv_deets",
          open = NULL,
          style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
          tags$summary(
            title = "Click to open or close",
            "Model Details",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
          ),
          h4("HOR-TCJ Survival", style = "margin-left:20px;"),
          div(
            style = "display:flex;",
            p("placeholder")
          )
        )
      )
    )
  )
}
