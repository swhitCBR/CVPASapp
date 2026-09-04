#' create ui for estimate panels
#'
#' @returns a `shiny.tag` object inside a `shinydashboardPlus::box()` footer
#'
#' @export
#'
#' @examples
#' draw_est_more_info_div_ui()
#'
draw_est_more_info_div_ui <- function(){
  div(
      # style = "display: none;",
            div(
        id = "bottom_download_panel",
        style = "padding-left: 10px;",
        h3(
          paste0("Download"),
          style = "color:black;text-decoration: underline;"
        ),
        div(
          style = "padding-top: 15px;",
          DT::dataTableOutput("table_est_pred")
        ),
        div(
          style = "padding-top: 15px; padding-bottom: 15px;",
          downloadButton(
            outputId = "download_est_pred",
            label = "Download Estimates (.csv)",
            icon = shiny::icon("download")
          )
        )
      )
      ,
      div(
        id = "more_info_panel",
        style = "padding-left: 10px;",
        h3(
          paste0("More Information"),
          style = "color:black;text-decoration: underline;"
        )
      )
      ,
      div(
        style = "padding-left: 10px;",
        tags$details(
          id = "hor_tcj_surv_deet",
          open = NULL,
          style = "margin-top:15px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
          tags$summary(
            title = "Click to open or close",
            "Additional Input Data Plots",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
          ),
          h4(
            "Lattice plots showing input valves in broader context",
            style = "margin-left:20px;"
          ),
          div(
            style = "display:flex;",
            plotOutput("doy_ins_ggpplt", height = "800px") 
          )
        )
        # ,
        # tags$details(
        #   id = "hor_chp_ore_surv_deets",
        #   open = NULL,
        #   style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
        #   tags$summary(
        #     title = "Click to open or close",
        #     "Model Details",
        #     style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
        #           padding-bottom: 2px;background-color:#ddd;"
        #   ),
        #   h4("HOR-TCJ Survival", style = "margin-left:20px;"),
        #   div(
        #     style = "display:flex;",
        #     p("placeholder")
        #   )
        # )
      )
    )
  }
