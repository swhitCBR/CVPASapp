  #' Title
  #'
  #' @param input_tab_in input$tabs
  #' @param init_data_source_in data source object
  #'
  #' @returns a `shiny.tag` object inside a `shinydashboardPlus::box()``
  #'
  #' @export
  #' 
  #' @examples
  #' draw_inputs_panel_ui(init_data_source="Previous years")
  #' 
draw_inputs_panel_ui <- function(inputs_panel_collapse=inputs_panel_collapse,init_data_source_in){
    shinydashboardPlus::box(
      id = "input_box2",
      title = span(h2("Inputs",style="margin-top: 0px; margin-bottom: 0px;"),id="input_box2_title"),
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      collapsed = FALSE,
        width = 12,
      column(
        width = 7,
        shiny::HTML("<h4> <b> Overview: </b>  </h4>"),
        p(
          "This tool provides predictions of survival and route usage for hypothetical releases of juvenile Steelhead 
          based on their expected location,conditions in the environment, and individual size."
        ),
        tags$ul(
          tags$li(
            "Select a junction to serve as a starting location for the migration",
            tags$ul(
              tags$li(
                style = "list-style-type: none;",
                draw_start_loc_ui()
              )
            )
          ),
          tags$li(
            "Select the source to use for daily hydrologic data and export operations;",
            br(),
            "either: (1) a previous year or (2) a user-provided data set",
            tags$ul(
              tags$li(
                style = "list-style-type: none;",
                div(
                  style = "display: inline-flex; align-items: ;margin-top: 10px;",
                  div(
                    style = "margin-top: 0px; margin-right: 10px;font-weight:bold",
                    shiny::HTML("<h5> <b> Source: </b>  </h4>")
                  )
                  ,
                  div(
                    style = "height=15px",
                    shinyWidgets::pickerInput(
                      inputId = "data_source_picker",
                      choices = c(
                        "Previous year",
                        "Uploaded file (.csv)",
                        "None"
                      ),
                      width = "180px",
                      selected = init_data_source_in
                    )
                  )
                )
              )
            )
          )
        )
      ),
      column(
        width = 5,
        tagList(
          div(
            style = "margin-left: 10px; margin-right: 10px;;margin-top: 10px;;margin-bottom: 10px;",
            div(
              title = "click for more information",
              id = "schem_info_drop_div",
              style = "float:right !important;
                      position: relative;
                      z-index: 2"
              ,
              draw_ibutt_dropdown_ui(
                inputId_in = "daily_var_def_ibutt2",
                ibox_content_label = "basic_route_schematic_ibox_content"
              )
            ),
            tagList(
            conditionalPanel(condition = "input.start_loc_in == 'TCJ' && output.BarStatus == 'In'",
              draw_basic_route_schematic_svg(
              LOC_in = "TCJ",
              BAR_in = "In"
            ))
            ,
            conditionalPanel(condition = "input.start_loc_in == 'TCJ' && output.BarStatus == 'Out'",
              draw_basic_route_schematic_svg(
              LOC_in = "TCJ",
              BAR_in = "Out"
            ))
            ,
            conditionalPanel(condition = "input.start_loc_in == 'HOR' && output.BarStatus == 'In'",
              draw_basic_route_schematic_svg(
              LOC_in = "HOR",
              BAR_in = "In"
            ))
            ,
            conditionalPanel(condition = "input.start_loc_in == 'HOR' && output.BarStatus == 'Out'",
              draw_basic_route_schematic_svg(
              LOC_in = "HOR",
              BAR_in = "Out"
            ))
          )
          ,
          )
        )
      ),
      column(
        width = 12,
        style = "margin:10px;",
        tagList(
          # shiny::uiOutput("data_source_inputs_dynui")
        tagList(
          # switch(
            # data_source_selected(),
            # "None" = NULL,
            # "Previous year" = 
            conditionalPanel(
              condition = "input.data_source_picker == 'Previous year'",
              # shiny::uiOutput("details_sel_data_bar")
              draw_details_sel_data_ui()
            ),
            conditionalPanel(
              condition = "input.data_source_picker == 'Uploaded file (.csv)'",

            # "Uploaded file (.csv)" = 
              draw_upload_details_ui() #shiny::uiOutput("upload_deet_ui")
          )
        )
          ,

          # shiny::uiOutput("details_indiv_attrib_ui")
          draw_details_indiv_attrib_ui()
        )
      ),
      # footer = uiOutput("chk_input_ui")
      footer = div(
        style = "display: none;",
        id="checkTop",
        draw_chk_input_ui()
      )
    )
}
