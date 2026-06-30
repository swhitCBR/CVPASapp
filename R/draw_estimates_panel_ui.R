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
      draw_ovr_surv_est_sec_ui()
      ,
      # section break
      hr(),
      draw_reach_surv_est_sec_ui()
    ,
      hr(),
      draw_reach_spec_surv_est_sec_ui()
      ,
      # div(
      #   id = "route_spec_surv_panel",
      #   ##ALT## wide vs. long display of overall survival plots
      #   # style = "padding-left: 20px;padding-right:10px;"
      #   # equal width alternative
      #   style = "display:grid; grid-template-columns: repeat(3, 1fr); padding-left: 10px;padding-right:10px;",
      #   # style = "display:grid; grid-template-columns: repeat(2, 1fr); padding-left: 10px;padding-right:10px;",
      #   # flexible width alternative
      #   # style = "display:flex; justify-content: space-evenly;padding-left: 10px;padding-right:10px;",

      #   # div(
      #   #   HTML(
      #   #     '<h4 style="font-weight: normal; padding-left: 10px;">
      #   #       <strong>Route-Specific Survival: </strong>
      #   #       Head of Old River to CHP via SJR route</h4>'
      #   #   ),
      #   #   div(
      #   #     plotOutput("HOR_TCJ_pred_ggpplt_dup1b", height = "400px")
      #   #   )
      #   # ),
      #   # div(
      #   #   HTML(
      #   #     '<h4 style="font-weight: normal; padding-left: 10px;">
      #   #       <strong>Route-Specific Survival: </strong>
      #   #       Head of Old River to CHP via ORE route</h4>'
      #   #   ),
      #   #   div(
      #   #     plotOutput("HOR_TCJ_pred_ggpplt_dup1a", height = "400px")
      #   #   )
      #   # ),
      #   div(
      #     HTML(
      #       '<h4 style="font-weight: normal; padding-left: 10px;">
      #         <strong>Route-Specific Survival: </strong>
      #         Turner Cut Junction to CHP via SJR route</h4>'
      #     ),
      #     div(
      #       plotOutput("HOR_TCJ_pred_ggpplt_dup1e", height = "400px")
      #     )
      #   ),
      #   div(
      #     HTML(
      #       '<h4 style="font-weight: normal; padding-left: 10px;">
      #         <strong>Route-Specific Survival: </strong>
      #         Turner Cut Junction to CHP via TRN route</h4>'
      #     ),
      #     div(
      #       plotOutput("HOR_TCJ_pred_ggpplt_dup1c", height = "400px")
      #     )
      #   )
      # )
      # ,
      hr(),
      div(
        style = "padding-left: 10px;",
        id = "route_usage_panel",
        h3(
          paste0("Route Usage"),
          style = "color:#006400;text-decoration: underline;"
        ),
        column(
          width = 6,
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>At Head of Old River</strong>
               </h4>'
          ),
          plotOutput("HOR_TCJ_pred_ggpplt_dup1d", height = "400px")
        ),
        column(
          width = 6,
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>At Turner Cut Junction </strong>
               </h4>'
          ),
          plotOutput("HOR_TCJ_pred_ggpplt_dup1d2", height = "400px")
        )
        # Blue: #4E79A7 (Steel Blue)Orange: #F28E2B (Burnt Orange)
      )
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
