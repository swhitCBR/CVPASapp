draw_inputs_panel_ui <- function(inputs_panel_collapse=inputs_panel_collapse){
    shinydashboardPlus::box(
      id = "input_box2",
      title = span(h2("Inputs",style="margin-top: 0px; margin-bottom: 0px;"),id="input_box2_title"),
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      collapsed = FALSE,
      # tags$head(tags$style(HTML(get_custom_css_txt(css_txt_content="sidebar")))),
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
                # shiny::uiOutput("start_loc_ui")
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
                      #      choicesOpt = list(
                      #       subtext = c(
                      #       "Inflow",
                      #       "Outlflow",
                      #       "Interior flow"
                      #   )
                      # )
                      ,
                      selected = init_data_source
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
          # ,
            # draw_basic_route_schematic_svg(
            #   # LOC_in = in_selected_RV$LOC,
            #   # BAR_in = in_selected_RV$BAR
            # )
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
              draw_upload_deet_ui() #shiny::uiOutput("upload_deet_ui")
          )
        )
          ,

          # shiny::uiOutput("details_indiv_attrib_ui")
          draw_details_indiv_attrib_ui()
        )
      ),
      # footer = uiOutput("chk_input_ui")
      footer = div(
        id="checkTop",
        draw_chk_input_ui()
      )
    )
}

# output$inputs_panel_UI <- renderUI({
#     shinydashboardPlus::box(
#       id = "input_box2",
#       title = span(h2("Inputs",style="margin-top: 0px; margin-bottom: 0px;")),
#       solidHeader = TRUE,
#       status = "primary",
#       collapsible = T,
#       # collapsed = TRUE,
#       collapsed = inputs_panel_collapse,
#       # collapsed = golem::get_golem_options("inputs_panel_collapse"),
#       width = 12,
#       tags$style(HTML(
#         "
#       .box {
#         border-top: 1px solid #ddd !important;
#         border-left: 1px solid #ddd;
#         border-right: 1px solid #ddd;
#         border-bottom: 1px solid #ddd;
#       }
#       .box-header {
#         border-bottom: 2px solid #ddd !important;
#       }
        
#     "
#       )),
#       column(
#         width = 7,
#         shiny::HTML("<h4> <b> Overview: </b>  </h4>"),
#         p(
#           "This tool provides predictions of survival and route usage for hypothetical releases of juvenile Steelhead 
#           based on their expected location,conditions in the environment, and individual size."
#         ),
#         tags$ul(
#           tags$li(
#             "Select a junction to serve as a starting location for the migration",
#             tags$ul(
#               tags$li(
#                 style = "list-style-type: none;",
#                 shiny::uiOutput("start_loc_ui")
#               )
#             )
#           ),
#           tags$li(
#             "Select the source to use for daily hydrologic data and export operations;",
#             br(),
#             "either: (1) a previous year or (2) a user-provided data set",
#             tags$ul(
#               tags$li(
#                 style = "list-style-type: none;",
#                 div(
#                   style = "display: inline-flex; align-items: ;margin-top: 10px;",
#                   div(
#                     style = "margin-top: 0px; margin-right: 10px;font-weight:bold",
#                     shiny::HTML("<h5> <b> Source: </b>  </h4>")
#                   )
#                   # ,
#                   # div(
#                   #   style = "height=15px",
#                   #   shinyWidgets::pickerInput(
#                   #     inputId = "data_source_picker",
#                   #     choices = c(
#                   #       "Previous year",
#                   #       "Uploaded file (.csv)",
#                   #       "None"
#                   #     ),
#                   #     width = "180px",
#                   #     #      choicesOpt = list(
#                   #     #       subtext = c(
#                   #     #       "Inflow",
#                   #     #       "Outlflow",
#                   #     #       "Interior flow"
#                   #     #   )
#                   #     # )
#                   #     ,
#                   #     selected = init_data_source
#                   #   )
#                   # )
#                 )
#               )
#             )
#           )
#         )
#       ),
#       column(
#         width = 5,
#         tagList(
#           div(
#             style = "margin-left: 10px; margin-right: 10px;;margin-top: 10px;;margin-bottom: 10px;",
#             div(
#               title = "click for more information",
#               id = "schem_info_drop_div",
#               style = "float:right !important;
#                       position: relative;
#                       z-index: 2;",
#               draw_ibutt_dropdown_ui(
#                 inputId_in = "daily_var_def_ibutt2",
#                 info_box_contents = get_ibox_contents(
#                   "basic_route_schematic_ibox_content"
#                 )
#               )
#               # shinyWidgets::dropdownButton(
#               #   right = TRUE,
#               #   up = FALSE,
#               #   circle = TRUE,
#               #   size = "xs",
#               #   status = "primary",
#               #   icon = icon("info", style = "color: white;"),
#               #   width = "300px",
#               #   p("Major routes and key junctions in the Delta") #,
#               # )
#             ),
#             draw_basic_route_schematic_svg(
#               LOC_in = in_selected_RV$LOC,
#               BAR_in = in_selected_RV$BAR
#             ),
#           )
#         )
#       ),
#       column(
#         width = 12,
#         style = "margin:10px;",
#         tagList(
#           # shiny::uiOutput("data_source_inputs_dynui")
#         tagList(
#           # switch(
#             # data_source_selected(),
#             # "None" = NULL,
#             # "Previous year" = 
#             conditionalPanel(
#               condition = "input.data_source_picker == 'Previous year'",
#               # shiny::uiOutput("details_sel_data_bar")
#               draw_details_sel_data_ui()
#             ),
#             conditionalPanel(
#               condition = "input.data_source_picker == 'Uploaded file (.csv)'",

#             # "Uploaded file (.csv)" = 
#               draw_upload_deet_ui() #shiny::uiOutput("upload_deet_ui")
#           )
#         )
#           ,

#           # shiny::uiOutput("details_indiv_attrib_ui")
#           draw_details_indiv_attrib_ui()
#         )
#       ),
#       footer = uiOutput("chk_input_ui")
#     )
#   })