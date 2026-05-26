#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # autoselect specify starting tab
  # session$onSessionEnded(stopApp)

  # Retrieve specific argument
  start_tab_passed <- golem::get_golem_options("start_tab")
  inputs_panel_collapse <- golem::get_golem_options("inputs_panel_collapse")

  # at startup ----
  # inputs_selected by default
  tab_selected <- reactiveVal(start_tab_passed)
  shinydashboard::updateTabItems("tabs",session = session, selected = start_tab_passed)
  # probably better as a reactiveValues (list format)
  data_source_selected <- reactiveVal("prev_pan")

  ### debugging print  ----
  observeEvent(input$tabs, {tab_selected(input$tabs); print(input$tabs)})
  observeEvent(input$data_source_picker, {data_source_selected(input$data_source_picker)})
  
  observeEvent(input$goto_inputs_butt, {
    tab_selected("inputs") # this is a reactiveVal
    shinydashboard::updateTabItems(
      "tabs",
      session = session,
      selected = "inputs"
    )
  })

  observeEvent(input$goto_met_ref_butt, {
    tab_selected("met_ref")
    shinydashboard::updateTabItems(
      "tabs",
      session = session,
      selected = "met_ref"
    )
  })

  observeEvent(input$top_met_ref_butt, {
    tab_selected("met_ref")
    shinydashboard::updateTabItems(
      "tabs",
      session = session,
      selected = "met_ref"
    )
  })

  observeEvent(input$top_about_butt, {
    tab_selected("about")
    shinydashboard::updateTabItems(
      "tabs",
      session = session,
      selected = "about"
    )
  })

  # debugging and printing
  output$sel_in_ls_text = renderText({
    RV_text_fun(
      heading = "Selected",
      RVls_in = shiny::reactiveValuesToList(in_selected_RV)
    )
  })

  # print global (i.e., locked-in values)
  output$glob_in_ls_text = renderText({
    RV_text_fun(heading = "none",
    RVls_in = shiny::reactiveValuesToList(global))
  })
  

  observeEvent(input$start_loc_in, {
    in_selected_RV$LOC <- input$start_loc_in
    # if (!SILENT) {
    #   cat("Starting location is:", global$LOC, "\n")
    # }
  })
  
  output$start_loc_heading = renderText({
    paste(in_selected_RV$start_loc_in)
  })
  

  ### change display based on reactive values ----
  # for selecting among schematic plots
  # swaps out .svg images depending on the values of two reactiveValues
  output$schem_start_loc_plt_ui <- renderUI({
    tst_val <- paste(in_selected_RV$LOC, in_selected_RV$BAR, sep = "_")
    tags$img(src = paste0("www/images/svg/basic route schematic/", tst_val, ".svg"),width = "100%")
  })

  # could just be in ui
  output$data_source_ui <- renderUI({
    shinyWidgets::pickerInput(inputId="data_source_picker",choices = c("Previous year", "Uploaded file (.csv)", "None"),selected = init_data_source)
  })


  ### change major display change based on reactive values ----
  # for selecting among schematic plots
  output$data_source_selected_txt <- renderText({
    data_source_selected()
  })

  output$deet_panel_prev_upload_ui <- renderUI({
    tagList(
      # shiny::verbatimTextOutput("data_source_selected_txt"),
      switch(
        data_source_selected(),
        "None" = NULL,
        "Previous year" = shiny::uiOutput("details_sel_data_bar"),
        "Uploaded file (.csv)" = shiny::uiOutput("details_up_data_bar")
      )
    )
  })

  output$details_sel_data_bar <- renderUI({
    tags$details(
      id = "details_prev_yr",
      open = TRUE, #ifelse(input$data_source_picker == "Previous year", TRUE, NULL),
      style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white; ",
      tags$summary(
        title = "Click to open or close",
        "Select Daily Environmental Data", 
        style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px; padding-bottom: 2px; background-color:#ddd"
      ),
      shiny::uiOutput("prev_yr_ui")
    )
  })

  output$details_up_data_bar <- renderUI({ draw_details_up() })

  output$details_indiv_attrib_ui <- renderUI({
    # tagList(
    ifelse(
      data_source_selected() == "None",
      tagList(NULL),
      tagList(
        tags$details(
          id = "details_indiv",
          style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px;",# ; background-color:white",
          tags$summary(
            title = "Click to open or close",
            "Select Individual Attributes",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px; padding-bottom: 2px;background-color:#ddd;"
          ),
          fluidRow(
            style = "padding-inline-start: 15px;",
            column(
              width = 6,
              h5(strong("Fork Length")),
              tags$ul(
                style = "padding-inline-start: 20px;",
                tags$li(
                  "By default, all predictions are based on the average fork length of juvenile Steelhead used in modeling."
                )
              )
            ),
            column(width = 6, shiny::uiOutput("flength_sel_ui"))
          )
        )
      )
    )
    # )
  })

  # })

  output$inputs_panel_UI <- renderUI({
    shinydashboardPlus::box(
      id = "input_box2",
      title = "Inputs",
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      # collapsed = TRUE,
      collapsed = inputs_panel_collapse,
      # collapsed = golem::get_golem_options("inputs_panel_collapse"),
      width = 12,
      tags$style(HTML(
        "
      .box {
        border-top: 1px solid #ddd !important;
        border-left: 1px solid #ddd;
        border-right: 1px solid #ddd;
        border-bottom: 1px solid #ddd;
      }
      .box-header {
        border-bottom: 2px solid #ddd !important;
      }
        
    "
      )),
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
                shiny::uiOutput("start_loc_ui")
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
                  ),
                  div(
                    style = "height=15px",
                    shiny::uiOutput("data_source_ui")
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
                      z-index: 2;",
              shinyWidgets::dropdownButton(
                right = TRUE,
                up = FALSE,
                circle = TRUE,
                size = "xs",
                status = "primary",
                icon = icon("info", style = "color: white;"),
                width = "300px",
                p("Major routes and key junctions in the Delta") #,
              )
            ),
            shiny::uiOutput("schem_start_loc_plt_ui")
          )
        )
      ),
      column(
        width = 12,
        style = "margin:10px;",
        tagList(
          shiny::uiOutput("deet_panel_prev_upload_ui"),
          shiny::uiOutput("details_indiv_attrib_ui")
        )
      )
      ,
      footer=uiOutput("chk_input_ui")
    )
  })



  output$estimates_panel_ui <- renderUI({
    shinydashboardPlus::box(
      id = "est_box_ui",
      title = shiny::HTML("Estimates"),
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      collapsed = FALSE,
      width = 12,
      column(
        width = 12,
        h4(strong(paste0("Overall Survival (HOR-CHP)"))),
        div(
          style = "display:flex; justify-content: space-between;padding-left: 10px;padding-right:10px;",#border-bottom: solid 1px gray; align-items:center",
          # style = "display:flex; justify-content: space-between;padding-left: 10px;padding-right: 10px;border-bottom: solid 1px gray; align-items:center",
          # title = "attributes of years 2011-2024",
          # h4(strong(paste0("Overall Survival (",in_selected_RV$start_loc_in,")"))),
         
          # textOutput("start_loc_heading"),
          # shinyWidgets::dropMenu(
          #   shinyWidgets::circleButton(
          #     inputId = "btn4",
          #     icon = icon("info"),
          #     status = "primary",
          #     size = "xs"
          #   ),
          #   p("How well user input data is assessed by comparing selections to the range of observed value in the Six-Year Study")#,
          #   # placement = "left-start"
          # )
        
        # ,
        column(width=7,
               p("more text here")
        ,
               plotOutput("ovrl_mods_pred_ggpplt2", height = "400px")
               ),
        column(width=5,
               div(
                 # for resizing table height
                 style = "border: solid 1px black; margin:10px;",
                 # style = "border: solid 2px black; margin:10px;height:525px;",
                 span(
                   style = "display:flex; justify-content: space-between;padding-left: 10px;padding-right: 10px; align-items:center; border-bottom: solid 1px black;",
                   title = "attributes of years 2011-2024",
                   h5(em("Route-Specific Survival")),
                   shinyWidgets::dropMenu(
                     shinyWidgets::circleButton(
                       inputId = "btn1",
                       icon = icon("info"),
                       status = "primary",
                       size = "xs"
                     ),
                     tags$dl(
                       # definition list
                       div(
                         style = "margin-left: 20px;",
                         HTML('<strong>Year:</strong> Calendar Year</p>'),
                         HTML('<strong>Category:</strong> Water Year Type (SJ)</p>'),
                         HTML(
                           '<strong>HOR Barrier:</strong> Barrier at Head of Old River</p>'
                         ),
                         HTML(
                           '<strong>Model:</strong> Used to fit statistical models</p>'
                         )
                       )
                     )
                   ),
                   placement = "left-start"
                 ),
                 # ),
                 plotOutput("submods_pred_ggpplt2", height = "400px")
               )
               # div(style="display:flex;",
                   
                   # ,
                   # plotOutput("doy_ins_ggpplt2", height = "400px")
               # )
        )
        # tags$details(
        #   id = "overall_surv_deet",
        #   open = TRUE,
        #   style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
        #   tags$summary(
        #     title = "Click to open or close",
        #     "Route Usage",
        #     # "Overall Survival", # "Upload/Review Daily Environmental Data",
        #     style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
        #           padding-bottom: 2px;background-color:#ddd;"
        #   )
        #   ,
        #   div(style="display:flex;",
        #       plotOutput("submods_pred_ggpplt2", height = "400px")
        #       # ,
        #       # plotOutput("doy_ins_ggpplt2", height = "400px")
        #   )
        #   # ,
        #   # plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
        #   # ,
        #   # plotOutput("doy_surv_ggpplt", height = "400px")
        #   
        #   # ,
        #   # p("empty_panel")
        # )
        )
        ,
        # column(width=7,
        tags$details(
          id = "hor_chp_surv_deet",
          open = TRUE,
          style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
          tags$summary(
            title = "Click to open or close",
            "Sub-Model Estimates", # "Upload/Review Daily Environmental Data",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
          )
          ,
          h4("HOR-TCJ Survival",style="margin-left:20px;"),
          # span(style="padding-left:20px;",
          #      h4(em("hh")),),
          div(style="display:flex;",
              plotOutput("doy_ins_ggpplt", height = "400px")
              ,
              plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
          )
          # ,
          # plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
          # ,
          # plotOutput("doy_surv_ggpplt", height = "400px")
          
          # ,
          # p("empty_panel")
        )
      # )
        # ,
        # column(width=5,
        # tags$details(
        #   id = "route_usage_deet",
        #   open = TRUE,
        #   style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
        #   tags$summary(
        #     title = "Click to open or close",
        #     "Reach-Specific Survival", # "Upload/Review Daily Environmental Data",
        #     style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
        #           padding-bottom: 2px;background-color:#ddd;"
        #   )
        #   ,
        #   div(style="display:flex;",
        #       p("fill-in")
        #       # plotOutput("doy_ins_ggpplt", height = "400px")
        #       # ,
        #       # plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
        #   )
        # )
        # )
      )
    )
  })

  # shinyWidgets::radioGroupButtons(
  #   'submodule_env_data_view_1-radio_metric_view',
  #   label = "Metric",
  #   choices = c(
  #     "log(VNS)" = "VNS",
  #     "T_msd" = "MSD",
  #     "T_clc" = "CLC",
  #     "CVP" = "CVP",
  #     "SWP" = "SWP"
  #   )
  # )
  # ,                  plotOutput("submodule_env_data_view_1-doy_ovr_plt2")

  output$chk_input_ui <- renderUI({
    div(
      column(
        width = 12,
        style = "margin-bottom: 20px",
        br(),
        # div(
        #   # class = "thumbnail-section",
        #   # style = "margin-left: 40px;",
        #   h4(strong("Check Inputs"))
        # ),
        span(
              style = "display:flex; justify-content: space-between;padding-left: 10px;padding-right: 10px;    border-bottom: solid 1px gray; align-items:center",
              # title = "attributes of years 2011-2024",
              h4(strong("Check Inputs")),
              shinyWidgets::dropMenu(
                shinyWidgets::circleButton(
                  inputId = "btn3",
                  icon = icon("info"),
                  status = "primary",
                  size = "xs"
                ),
                p("How well user input data is assessed by comparing selections to the range of observed value in the Six-Year Study"),
                placement = "left-start"
              )
            )
            ,
        column(
          style="margin-top:10px;",
          width = 6,
          # actionButton("check_inputs_butt", "Check Inputs")
          div(
            p(
            "Verify that specified values conform with data used to fit statistical sub-models"
          )),
          div(
            style = "display: inline-flex",

            actionButton(
              "check_inputs_butt",
              "Check Inputs",
              style = "color: white; background: #024c63; padding: 10px"
            ), ##3c8dbc ; #024c63
            div(
              style = "margin-left: 20px; align-content: flex-end;", # 2nd one for vert align
              shinyjs::disabled(
                shinyWidgets::prettyCheckbox(
                  inputId = "inputs_check", #"inputs_check_pretty",
                  value = FALSE,
                  label = "Valid",
                  icon = icon("check")
                )
              )
            )
          ),
          div(
            style = "margin-top: 20px",
            shinyjs::disabled(
              actionButton(
                "generate_ests_butt",
                "Generate Estimates",
                style = "color: white; background: #024c63; padding: 10px"
              )
            )
          )
        ),

        column(
                    style="margin-top:10px;",
          width = 6,
          actionButton("load_butt", "Load"),
          actionButton("reset_butt", "Reset"),
          div(
            style = "display:block;margin-top:10px;",
            wellPanel(
              textOutput("glob_in_ls_text"),
              tags$style(
                type = "text/css",
                "#glob_in_ls_text {white-space: pre-wrap;}"
              )
            )
          )
        )
      )
    )
  })

  observeEvent(
    input$check_inputs_butt,
    {
      shinyWidgets::updatePrettyCheckbox(
        session = session,
        inputId = "inputs_check",
        value = TRUE
      )
      # activates button
      shinyjs::enable("generate_ests_butt")
    }
  )

  observeEvent(input$reset_butt, {
    shinyWidgets::updatePrettyCheckbox(
      session = session,
      inputId = "inputs_check",
      value = FALSE
    )
    shinyjs::disable("generate_ests_butt")
  })

  output$time_of_year_ui <- renderUI({
    tagList(
      div(
        style = "display: left;margin-top: 10px;width: 600px;",
        sliderInput(
          "DOYslider_rng",
          label = NULL,
          min = 0,
          max = 250,
          value = c(in_selected_RV$start_day, in_selected_RV$end_day),
          width = '99%'
        ),
        plotOutput("doy_ref_strt_loc", height = "100px"),
        div(
          style = "display: flex;",
          HTML(paste(
            "<p> Day of arrival at <strong>",
            (input$start_loc_in),
            "</strong> junction </p> "
          )),
          div(
            style = "margin-left: 10px; z-index: 2",
            shinyWidgets::dropdownButton(
              right = FALSE,
              up = TRUE,
              circle = TRUE,
              size = "xs",
              status = "primary",
              icon = icon("info", style = "color: white;"),
              width = "300px",
              p(
                "Histogram of day of year when acoustic-tagged juvenile Steelhead were detected at each location."
              )
            )
          )
        )
      )
    )
  })

  # needs to be dynamic
  output$prev_yr_ui <- renderUI({
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
                            selected = in_selected_RV$past_water_year,

                            choicesOpt = list(
                              style = paste0(
                                "background-color:",
                                WYT_cols[match(
                                  ann_HORbar_WYT_data$WYT,
                                  names(WYT_cols)
                                )],
                                ";"
                              )
                              # ,
                              # subtext = (ann_HORbar_WYT_data$WYT)
                              # ,
                              #  content = paste("<div style=color:black>",c(as.character(2011:2024), "None"),c(ann_HORbar_WYT_data$WYT,"None"),"</div>")
                            )
                          )
                          # ,
                          # shiny::uiOutput("year_picker_ui")
                        )
                      )
                    )
                  ),
                )
              ),
              tags$li(
                HTML(paste(
                  "<h5> Select a date range for arrival at  <strong>",
                  (input$start_loc_in),
                  "</strong> junction by entering dates or adjusting Day of Year slider </h5> "
                ))
                # h5(
                #   "Select a date range for fish arriving at the selected junction by entering date(s) or adjusting Day of Year slider"
                # )
              )
            )
          ),
          tagList(
              div(
                # style = "display: flex; gap:20px; align-items:center",
                shiny::uiOutput("start_date_entry_sep_ui"),
              div(
                # style = "display: flex; gap:20px;",
                shinyWidgets::dropMenu(
                  placement = "bottom",
                  tag = actionButton(
                    inputId = "doy_slider_dropdown",
                    label = HTML(
                      '<i class="fas fa-sliders" role="presentation" aria-label="sliders icon"></i> Day of Year Slider'
                    )
                  ),
                  shiny::uiOutput("time_of_year_ui")
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
              title = "attributes of years 2011-2024",
              h5(em("Annual Summary Table")),
              shinyWidgets::dropMenu(
                shinyWidgets::circleButton(
                  inputId = "ann_summ_tab_ibttn",
                  icon = icon("info"),
                  status = "primary",
                  size = "xs"
                ),
                tags$dl(
                  # definition list
                  div(
                    style = "margin-left: 20px;",
                    HTML('<strong>Year:</strong> Calendar Year</p>'),
                    HTML('<strong>Category:</strong> Water Year Type (SJ)</p>'),
                    HTML(
                      '<strong>HOR Barrier:</strong> Barrier at Head of Old River</p>'
                    ),
                    HTML(
                      '<strong>Model:</strong> Used to fit statistical models</p>'
                    )
                  )
                  )
                ),
                placement = "left-start"
              ),
            # ),
            uiOutput("table_in_WY_UI")
          )
        )
      )
      ,
      column(
        width = 12,
            div(
            style = "border: solid 1px black;margin-bottom:10px;",# margin:10px;",
                    span(
              style = "display:flex; justify-content: space-between;padding-left: 10px;padding-right: 10px;    border-bottom: solid 1px gray; align-items:center",
              title = "Plots of selected or uploaded data in the context of observations from 2011-2024",
              h5(em("View Daily Values")),
              info_drop_fun(inputId_in="daily_var_def_info_bttn1")
            )
        ,
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
                "MSD" = "MSD"),
                width="200px",
                choicesOpt = list(
                subtext=c("Inflow","Outlflow","Interior flow","Interior flow","Interior flow","Exports","Exports","Exports","Temperature","Temperature")
              )
            )
          )
        ,
        plotOutput("doy_var_ggpplt", height = "400px")

      )
      )
    )
  })

  # Interactive lattice plot
  #  output$doy_latt_pltly <-  plotly::renderPlotly({
  output$doy_latt_ggpplt <- renderPlot({
    ggplot_doy_env_lattice_plt(
      CVhelp_dat_l_plt = CVhelp_dat_l,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )
  })

  output$doy_var_ggpplt <- renderPlot({
    ggplot_doy_env__plt(
      CVhelp_dat_l_plt = CVhelp_dat_l,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year,
      sub_var_in = input$radio_metric_view
      # log_trans
    )
  })
  
  
  output$doy_ins_ggpplt <- renderPlot({
    ggplot_doy_ins_plt(
      CVhelp_dat_l_plt = CVhelp_dat_l,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year,
      sub_var_in = input$radio_metric_view
      # log_trans
    )
  })
  # duplicated
  output$doy_ins_ggpplt2 <- renderPlot({
    ggplot_doy_ins_plt(
      CVhelp_dat_l_plt = CVhelp_dat_l,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year,
      sub_var_in = input$radio_metric_view
      # log_trans
    )
  })

  output$yr_var_ggpplt <- renderPlot({
    ggplot_yr_env_lattice_plt(
      CVhelp_dat_l_plt = CVhelp_dat_l,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year,
      sub_var_in = input$radio_metric_view
    )
  })


  output$table_in_WY <-  DT::renderDataTable(draw_ann_summ_tab(input_year=in_selected_RV$past_water_year))

  observeEvent(input$year_picker, {
    in_selected_RV$BAR <- ann_HORbar_WYT_data[
      which(ann_HORbar_WYT_data_TAB()$Year == input$year_picker),
      "barrier"
    ]
  })

  # update the selected row in the table
  observeEvent( input$year_picker,{in_selected_RV$past_water_year <- input$year_picker})

  # update selected reactive values
  observeEvent(
    input$table_in_WY_rows_selected,{      # year_picker
      in_selected_RV$past_water_year <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"Year"]
      in_selected_RV$WYT <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"WYT"]
      in_selected_RV$BAR <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"barrier"]
      # initializing global day of year variables
      in_selected_RV$start_date <- as.Date(
        paste(
          in_selected_RV$start_day,
          in_selected_RV$past_water_year,
          sep = "-"),format = "%j-%Y")
      in_selected_RV$end_date <- as.Date(
        paste(
          in_selected_RV$end_day,
          in_selected_RV$past_water_year,
          sep = "-"),format = "%j-%Y")
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "year_picker",
        selected = in_selected_RV$past_water_year
      )
    }
  )

  # data table output UI
  output$table_in_WY_UI <- renderUI({
    DT::dataTableOutput("table_in_WY")
  })
  
  # update values when water year is selected
  observeEvent(
    input$load_butt,
    {
      global$past_water_year <- in_selected_RV$past_water_year
      global$WYT <- in_selected_RV$WYT
      global$WYT <- in_selected_RV$WYT
      global$BAR <- in_selected_RV$BAR
      global$start_day <- in_selected_RV$start_day
      global$end_day <- in_selected_RV$end_day
      global$start_date <- in_selected_RV$start_date
      global$end_date <- in_selected_RV$end_date
      global$flength <- in_selected_RV$flength
    }
  )


  # because this is within a renderUI, changing the input$tab value rewrites the sidebar content
  output$sidebar_text <- renderUI({
    shiny::HTML(
      md_txt_extract(
        md_addr = "inst/app/www/sidebar/sidebar.md",
        header_ref = paste0("# ", tab_selected()),
        asHTML_frag = TRUE
      )
    )
  })

  output$top_of_body_text <- renderUI({
    # because this is within a renderUI, changing the input$tab value rewrites the sidebar content
    switch(
      tab_selected(),
      "about" = shiny::tagList(
        mod_about_page_ui("mod_about_page-about_page_ui_1")),
      "met_ref" =shiny::tagList(
        uiOutput("met_ref_page_ui")) ,
      "inputs" = shiny::tagList(
        uiOutput("input_page_UI"))
    )
  })

  # if remaining static move to ui.R script
  output$met_ref_page_ui<- renderUI({ draw_met_ref_page_ui()})

  output$start_date_entry_sep_ui <- renderUI({
    div(
      style = "display: flex; gap:20px;",
      shinyWidgets::airDatepickerInput(
        inputId = "date_start_sep",
        label = "Start Date :",
        addon = "none",
        width = "100px",
        value = c(
          as.Date(
            paste(in_selected_RV$past_water_year, in_selected_RV$start_day),
            "%Y %j"
          )
        ),
        minDate = as.Date("2011-01-01"),
        maxDate = as.Date("2024-12-31"),
        range = FALSE,
        disabledDaysOfWeek = TRUE
      ),
      shinyWidgets::airDatepickerInput(
        inputId = "date_end_sep",
        label = "End Date :",
        addon = "none",
        width = "100px",
        value = c(
          as.Date(
            paste(in_selected_RV$past_water_year, in_selected_RV$end_day),
            "%Y %j"
          )
        )
      )
    )
  })

  # output$DOY_slider_ui <- renderUI({
  #   tagList(
  #     sliderInput(
  #       "DOYslider_rng",
  #       label = NULL,
  #       min = 0,
  #       max = 250,
  #       value = c(in_selected_RV$start_day, in_selected_RV$end_day),
  #       width = '99%'
  #     )
  #   )
  # })

  observeEvent(input$date_start_sep, {
    in_selected_RV$start_day <- as.numeric(format(input$date_start_sep, "%j"))
  })

  observeEvent(input$date_end_sep, {
    in_selected_RV$end_day <- as.numeric(format(input$date_end_sep, "%j"))
  })

  observeEvent(input$DOYslider_rng, {
    in_selected_RV$start_day <- input$DOYslider_rng[1]
    in_selected_RV$end_day <- input$DOYslider_rng[2]

    if (!is.na(in_selected_RV$past_water_year)) {
      in_selected_RV$start_date <- as.Date(
        paste(
          in_selected_RV$start_day,
          in_selected_RV$past_water_year,
          sep = "-"
        ),
        format = "%j-%Y"
      )
      in_selected_RV$end_date <- as.Date(
        paste(
          in_selected_RV$end_day,
          in_selected_RV$past_water_year,
          sep = "-"
        ),
        format = "%j-%Y"
      )
    }
    # if (!SILENT) {
    #   cat("Doy range is:", input$DOYslider_rng, "\n")
    # }
  })

  # Fork length reference selection
  output$flength_sel_ui <- renderUI({
    shiny::tagList(
      div(
        style = "display: inline-flex; align-items: center;gap:20px;",
          textOutput("flength_inval_txt"),
          numericInput(
          "flength_num_in_dash",
          # label = NULL,
          label = "Fork Length (mm)",
          step = 10,
          value = in_selected_RV$flength, #init_flength
          width = "80px",
          min = 100,
          max = 400
        ),
        div(
          style = "margin-left: 20px; display:flex",
          # shiny::uiOutput("flength_dash_ui"),
          plotOutput("flength_ref_hist", height = "100px"),
          div(
            style = "display: left;margin-top: 10px;margin-right: 10px;",
            div(
              # style = "display: flex;",
              div(
                HTML(paste("<p> Fork length of juvenile Steelhead from Six-Year Study </p> "))),
              div(
                style = "margin-left: 10px; z-index: 2",
                class = "pull-right",
                shinyWidgets::dropMenu(
                  shinyWidgets::circleButton(
                    inputId = "flength_ibutt",
                    icon = icon("info"),
                    status = "primary",
                    size = "xs"
                  ),
                  p(
                    "Lengths of fish relased during acoustic telemetry studies (2011-2016). Overall mean fork length selected by default."
                  ),
                  placement = "left-start"
                )
              )
            )
          )
        )
      )
    )
  })

  # textOutput("flength_inval_txt")
  # observeEvent(input$flength_num_in_dash >400{
  #     shiny::showNotification(paste("Only fork lengths between 100 and 400 mm are permitted")
  #     ,closeButtonF, duration = 5)
  # })

  # output$flength_dash_ui <- renderUI({
  #   shiny::tagList(
  #     plotOutput("flength_ref_hist", height = "100px")
  #   )
  # })

  # Reference forklength histogram
  output$flength_ref_hist <- renderPlot(
    {
      input$flength_num_in_dash
      ggplot_flength_ref_hist(
        flength_in = in_selected_RV$flength,
        flength_hst_xlims_in = c(100, 400)
      )
    },
    alt = "histogram depicting an approximately normal distribution of fork lengths centered at 244 millimeters with most tags between 160 and 330 millimeters"
  )

  output$doy_ref_strt_loc <- renderPlot({
    ggplt_doy_ref_hist(
      DOY_arvDF_l_in = DOY_arvDF_l,
      LOC_in = in_selected_RV$LOC,
      start_day_in = in_selected_RV$start_day,
      end_day_in = in_selected_RV$end_day
    )
  })

  output$input_page_UI <- renderUI({
    tagList(
      uiOutput("inputs_panel_UI"),
      uiOutput("estimates_panel_ui")
    )
  })

  # message for invalid flength entries
  output$flength_inval_txt <- renderText({
    validate(
      need(input$flength_num_in_dash <= 400 & input$flength_num_in_dash >= 100,
         "Only values between 100 and 400 mm are permitted")
    )
    NULL
  })
  
  # observe flength numeric input with debounce
  flength_mod <- reactive({
       validate(
        need(input$flength_num_in_dash < 400, "Only values between 100 and 400 mm are permitted"),
        need(input$flength_num_in_dash > 100, "Only values between 100 and 400 mm are permitted"))
    input$flength_num_in_dash  }) |> debounce(1000)


  observeEvent(flength_mod(), {
    in_selected_RV$flength <- flength_mod()
  })

  # version with tooltips
  output$surv_route_diagram_wtt <- bscui::renderBscui({
    bscui::bscui(surv_route_diagram_wtt_xml) |>
      bscui::set_bscui_options(
        clip = TRUE,
        show_menu = FALSE,
        zoom_min = 1.0,
        zoom_max = 1.0
      )
  })

  output$start_loc_ui <- renderUI({
    div(
      style = "display: inline-flex; align-items: left;margin-top: 10px;",
      div(
        style = "height=25px; align-self:center",
        shinyWidgets::pickerInput(
          # label="Junction:",
          label = "Starting Location:",
          inline = T,
          width = "fit",
          'start_loc_in',
          choices = loc_opt,
          ,
          choicesOpt = list(
            style = paste0(
              "background-color:",
              c("#67AB9F", "#FF3399"),
              ";"
            )
          )
        )
      )
    )
  })

  # observeEvent(input$inputs_done, {

  observeEvent(input$generate_ests_butt, {
    if (!input$input_box2$collapsed) {
      shinydashboardPlus::updateBox("input_box2", action = "toggle")
    }
    if (input$est_box_ui$collapsed) {
      shinydashboardPlus::updateBox("est_box_ui", action = "toggle")
    }
    print(input$env_dat_tabpan)
  })

  output$table_env_inputs <- DT::renderDataTable(
    DT::datatable(
      CVhelp_dat_w |>
        dplyr::filter(
          Year == in_selected_RV$past_water_year &
            DOY >= in_selected_RV$start_day &
            DOY < in_selected_RV$end_day
        ) |>
        dplyr::select(date, WYT, VNS, OMT, CVP, SWP, CLC, MSD) |>
        dplyr::mutate(dplyr::across(where(is.numeric), round, 1)),
      options = list(
        # dom = "pl",
        dom = '<"<"bottom"ip>',
        initComplete = DT::JS(
          "
        function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '75%'});",
          "$(this.api().table().body()).css({'font-size': '75%'});",
          "$(this.api().table().caption()).css({'font-size': '75%'});",
          "}
        "
        )
      ),
      selection = list(
        mode = 'none',
        target = "cell",
        selectable = NULL,
        selected = NULL
      ),
      colnames = c(
        "Date",
        "Water Year Type",
        "log(VNS)",
        "OMT",
        "CVP",
        "SWP",
        "CLC",
        "MSD"
      ),
      rownames = FALSE
      #, caption = 'Table 1: This is a simple caption for the table.'
    )
  )


  output$doy_surv_ggpplt <- renderPlot({
      ggplot_doy_surv_plt(
          CVhelp_dat_w_plt=CVhelp_dat_w,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )


    
  })

  
  
  
  

  
  
  output$HOR_TCJ_pred_ggpplt <- renderPlot({

    # reading from previously ran version of the data
    HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    
    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix 
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)

     ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt=HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year
    )
    
  })
  

  output$submods_pred_ggpplt2 <- renderPlot({
    
    # reading from previously ran version of the data
    HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    
    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix 
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)
    
    ggplot_doy_pred_plt_mult(
      HOR_TCJ_pred_tab_plt=HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year+1
    )
    
  })
  
  
  output$ovrl_mods_pred_ggpplt2 <- renderPlot({
    
    # reading from previously ran version of the data
    HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    
    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix 
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)
    
    ggplot_doy_pred_plt_ovrl(
      HOR_TCJ_pred_tab_plt=HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year+1
    )
    
  })
  
  
  
  
  
}



