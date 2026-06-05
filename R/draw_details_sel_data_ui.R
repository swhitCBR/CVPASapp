draw_details_sel_data_ui <- function(){
  tags$details(
      id = "details_prev_yr",
      open = TRUE, #ifelse(input$data_source_picker == "Previous year", TRUE, NULL),
      style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white; ",
      tags$summary(
        title = "Click to open or close",
        "Select Daily Environmental Data",
        style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px; padding-bottom: 2px; background-color:#ddd"
      ),
        fluidPage(
      fluidRow(
        style = "padding-inline-start: 15px;",
        column(
          width = 5,

          div(
            tags$ul(
              style = "padding-inline-start: 10px;",
              tags$li(
                h5(
                  "Select a previous year:"
                ),
                div(
                  tags$ul(
                    tags$li(
                      style = "list-style-type: none;",
                      div(
                        style = "display: inline-flex; align-items: ;margin-top: 10px;",
                        div(
                          style = "margin-top: 0px; margin-right: 5px;font-weight:bold",
                          shiny::HTML("<h5> <b> Year: </b>  </h4>")
                        ),
                        div(
                          style = "height=15px",
                          shinyWidgets::pickerInput(
                            "year_picker",
                            label = NULL,
                            multiple = FALSE,

                            choices = c(as.character(2011:2024), "None"),
                            selected = in_selected_RV[["past_water_year"]],

                            choicesOpt = list(
                              style = paste0(
                                "background-color:",
                                WYT_cols[match(
                                  ann_HORbar_WYT_data$WYT,
                                  names(WYT_cols)
                                )],
                                ";"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                )
              ),
              tags$li(
                HTML(paste(
                  "<h5> Select a date range for arrival at  <strong>",
                  # (input$start_loc_in), #replaced selector with reactive value
                  # (in_selected_RV$LOC),
                  "</strong> junction by entering dates or adjusting Day of Year slider </h5> "
                ))
              )
            )
          ),
          tagList(
            div(
              shiny::uiOutput("start_date_entry_sep_ui"),
              div(
                # style = "display: flex; gap:20px;",
                shinyWidgets::dropMenu(
                  hideOnClick = FALSE,
                  placement = "bottom",
                  tag = actionButton(
                    inputId = "doy_slider_dropdown",

                    label = HTML(
                      '<i class="fas fa-sliders" role="presentation" aria-label="sliders icon"></i> Day of Year Slider'
                    )
                  ),
                  shiny::uiOutput("time_of_year_entry_ui")
                )
              )
            )
          )
        ),
        column(
          width = 7,
          div(
            # for resizing table height
            style = "border: solid 1px black; margin:10px;",
            # style = "border: solid 2px black; margin:10px;height:525px;",
            span(
              style = "display:flex; justify-content: space-between;padding-left: 10px;padding-right: 10px; align-items:center; border-bottom: solid 1px black;",
              title = "summary table of characteristics across years (2011-2024)",
              h5(em("Annual Summary Table")),
              draw_ibutt_dropdown_ui(
                inputId_in = "ann_summ_tab_ibutt",
                ibox_content_label = "ann_summ_tab_ibutt_content"
              )
              ,
              placement = "left-start"
            ),
             DT::dataTableOutput("table_in_WY")
          )
        )
      ),
      column(
        width = 12,
        div(
          style = "border: solid 1px black;margin-bottom:10px;", # margin:10px;",
          span(
            style = "display:flex; justify-content: space-between;padding-left: 10px;padding-right: 10px;    border-bottom: solid 1px gray; align-items:center",
            title = "Plots of selected or uploaded data in the context of observations from 2011-2024",
            h5(em("View Daily Values")),
            draw_ibutt_dropdown_ui(inputId_in = "daily_var_def_ibutt1",ibox_content_label="daily_values_box")
          ),
          div(
            style = "margin-left:20px;margin-top:20px",
            shinyWidgets::pickerInput(
              'radio_metric_view',
              label = "Variable",
              choices = c(
                "log(VNS)" = "VNS",
                "OUT" = "OUT",
                "MID" = "MID",
                "ORB" = "ORB",
                "OMT" = "OMT",
                "CVP" = "CVP",
                "SWP" = "SWP",
                "EXPORTS" = "EXPORTS",
                "CLC" = "CLC",
                "MSD" = "MSD"
              ),
              width = "200px",
              choicesOpt = list(
                subtext = c(
                  "Inflow",
                  "Outlflow",
                  "Interior flow",
                  "Interior flow",
                  "Interior flow",
                  "Exports",
                  "Exports",
                  "Exports",
                  "Temperature",
                  "Temperature"
                )
              )
            )
          ),
          plotOutput("doy_var_ggpplt", height = "400px")
        )
      )
    )
      # shiny::uiOutput("prev_yr_ui")
    )
}