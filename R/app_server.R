#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # autoselect specify starting tab

  tab_selected <- reactiveVal("inputs")
  data_source_selected <- reactiveVal("prev_pan")
  shinydashboard::updateTabItems(
    "tabs",
    session = session,
    selected = "inputs"
  )

  # {DEBUGGING}
  observeEvent(input$tabs, {
    print(input$tabs)
    tab_selected(input$tabs)
  })

  observeEvent(input$goto_inputs_butt, {
    tab_selected("inputs")
    shinydashboard::updateTabItems(
      "tabs",
      session = session,
      selected = "inputs"
    )
  })

  observeEvent(input$goto_met_ref_butt, {
    # print(input$tabs)
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

  # Switching based on data source
  observeEvent(input$data_source_picker, {
    print(input$env_dat_tabpan)
    if (input$data_source_picker == "None") {
      data_source_selected("none")
    }

    if (input$data_source_picker == "Previous year") {
      data_source_selected("prev_pan")
      updateTabsetPanel(
        session = session,
        inputId = "env_dat_tabpan",
        selected = "prev_pan"
      )
    }

    if (input$data_source_picker == "Uploaded file (.csv)") {
      data_source_selected("up_pan")
      updateTabsetPanel(
        session = session,
        inputId = "env_dat_tabpan",
        selected = "up_pan"
      )
    }
  })

  output$sel_in_ls_text = renderText({
    RV_text_fun(
      heading = "Selected",
      RVls_in = shiny::reactiveValuesToList(in_selected_RV)
    )
  })

  output$glob_in_ls_text = renderText({
    RV_text_fun(
      heading = "Global",
      RVls_in = shiny::reactiveValuesToList(global)
    )
  })

  # top of page
  output$glob_in_ls_text_top = renderText({
    RV_text_fun(
      heading = "Selections:",
      RVls_in = shiny::reactiveValuesToList(global)
    )
  })

  observeEvent(input$start_loc_in, {
    in_selected_RV$LOC <- input$start_loc_in
    # if (!SILENT) {
    #   cat("Starting location is:", global$LOC, "\n")
    # }
  })

  # rendering schematic vector plots (svgs)
  output$schm_plt_HOR_CHP <- bscui::renderBscui({
    bscui::bscui(HOR_CHP_xml) |>
      bscui::set_bscui_options(
        clip = TRUE,
        show_menu = FALSE,
        zoom_min = 1.0,
        zoom_max = 1.0
      )
  })
  output$schm_plt_HOR_CHP_bar_in <- bscui::renderBscui({
    bscui::bscui(HOR_CHP_bar_in_xml) |>
      bscui::set_bscui_options(
        clip = TRUE,
        show_menu = FALSE,
        zoom_min = 1.0,
        zoom_max = 1.0
      )
  })
  output$schm_plt_TCJ_CHP <- bscui::renderBscui({
    bscui::bscui(TCJ_CHP_xml) |>
      bscui::set_bscui_options(
        clip = TRUE,
        show_menu = FALSE,
        zoom_min = 1.0,
        zoom_max = 1.0
      )
  })

  output$schm_plt_TCJ_CHP_bar_in <- bscui::renderBscui({
    bscui::bscui(TCJ_CHP_bar_in_xml) |>
      bscui::set_bscui_options(
        clip = TRUE,
        show_menu = FALSE,
        zoom_min = 1.0,
        zoom_max = 1.0
      )
  })

  # for selecting among schematic plots
  output$schem_start_loc_plt_ui <- renderUI({
    tst_val <- paste(in_selected_RV$LOC, in_selected_RV$BAR)
    div(
      title = "Major routes and key junctions in the Delta",
      shiny::tagList(
        switch(
          tst_val,
          "HOR Out" = bscui::bscuiOutput(
            outputId = "schm_plt_HOR_CHP",
            width = "100%",
            height = "300px"
          ),
          "TCJ Out" = bscui::bscuiOutput(
            outputId = "schm_plt_TCJ_CHP",
            width = "100%",
            height = "300px"
          ),
          "HOR In" = bscui::bscuiOutput(
            outputId = "schm_plt_HOR_CHP_bar_in",
            width = "100%",
            height = "300px"
          ),
          "TCJ In" = bscui::bscuiOutput(
            outputId = "schm_plt_TCJ_CHP_bar_in",
            width = "100%",
            height = "300px"
          )
        )
      )
    )
  })

  output$year_picker_ui <- renderUI({
    # div(
    # style = "display: inline-flex; align-items: left;", #margin-top: 10px;
    # div(
    #   style = "margin-top: 0px; margin-right: 5px;font-weight:bold",
    #   shiny::HTML("<h5> <b> Year: </b>  </h4>")
    #   # shiny::HTML("<h5> <b> Use data from: </b>  </h4>")
    # ),
    shinyWidgets::pickerInput(
      "year_picker",
      # label="Pick year",
      label = NULL,
      multiple = FALSE,
      # choices = c("None", as.character(2011:2024)),
      choices = c(as.character(2011:2024), "None"),
      # selected = NULL,
      # selected = "None",

      selected = init_water_year,
      choicesOpt = list(
        style = paste0(
          "background-color:",
          WYT_cols[match(ann_HORbar_WYT_data$WYT, names(WYT_cols))],
          ";"
        ),
        subtext = (ann_HORbar_WYT_data$WYT)
        # ,
        #  content = paste("<div style=color:black>",c(as.character(2011:2024), "None"),c(ann_HORbar_WYT_data$WYT,"None"),"</div>")
      )
    )
  })

  output$data_source_ui <- renderUI({
    shinyWidgets::pickerInput(
      "data_source_picker",
      choices = c("Previous year", "Uploaded file (.csv)", "None"),
      # selected="None"
      selected = init_data_source
    )
  })

  output$deet_panel_prev_upload_ui <- renderUI({
    tagList(
      switch(
        data_source_selected(),
        "None" = NULL,
        "prev_pan" = shiny::uiOutput("details_sel_data_bar"),
        #   tags$details(
        #     title="Click to open or close",
        #     id="my_deet1",
        #     style="margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white; ",
        #     tags$summary(
        #       "Select Daily Environmental Data",#"Select/Review Daily Environmental Data",
        #   style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px; padding-bottom: 2px; background-color:#ddd"),
        #   shiny::uiOutput("prev_yr_ui")
        # ),
        # "up_pan" =shiny::uiOutput("details_up_data_bar")
        "up_pan" = shiny::uiOutput("details_up_data_bar_redo")
      )
    )
  })

  output$details_sel_data_bar <- renderUI({
    tags$details(
      title = "Click to open or close",
      id = "my_deet1",
      style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white; ",
      tags$summary(
        "Select Daily Environmental Data", #"Select/Review Daily Environmental Data",
        style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px; padding-bottom: 2px; background-color:#ddd"
      ),
      shiny::uiOutput("prev_yr_ui")
    )
  })

  output$details_up_data_bar_redo <- renderUI({
    tags$details(
      title = "Click to open or close",
      id = "my_deet1",
      style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white; ",
      tags$summary(
        "Load Daily Environmental Data", # "Upload/Review Daily Environmental Data",
        style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
      ),
      tagList(
        fluidRow(
          # style = "padding-inline-start: 15px;",
          style = "padding: 15px;",
          column(
            width = 5,

            # div(
            # tags$ul(
            #   style = "padding-inline-start: 10px;",
            #   tags$li(
            # h5(
            # "Select a previous year:"
            # )
            # ,
            fileInput(
              inputId = "file1",
              label = "Choose CSV File",
              # buttonLabel="aaa",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),
            shiny::fluidRow(
              style = "margin-top:20px",
              column(
                width = 4,
                div(
                  role = "menuitem",
                  actionButton(
                    inputId = "run_button",
                    label = "Load"
                  )
                )
              ),
              column(
                width = 4,
                div(
                  role = "menuitem",
                  actionButton(
                    inputId = "reset_button",
                    label = "Reset"
                  )
                )
              ),
              column(
                width = 4,
                downloadButton(
                  outputId = "download_sample",
                  label = "Sample CSV",
                  icon = shiny::icon("download")
                )
              )
            ),
            # )
            #   )
            # )
            # )
            div(
              style = "padding-top: 20px;",
              role = "menu",
              shinyWidgets::dropdown(
                # id = "summ_man_in1",
                # style="simple",
                label = "Manual Input",
                icon = icon("edit"),
                title = "Click to open or close",
                size = "sm",
                width = "600px",
                # style="margin-top:20px; padding: 0px; color:#337ab7 ; background-color:white",
                # tags$summary(
                #   id="summ_man_in1",
                "Manual Input",
                # style = "font-size: 14px; font-weight: bold; background-color:#ddd; padding: 5px"
                # ),
                # div(
                #   style = "padding: 10px;",
                #   role = "menu",
                #   div(
                #     title = "aaa",
                #     role = "menuitem",
                #     # p("textAreaInput")
                textAreaInput(
                  inputId = "manual_input",
                  label = NULL,
                  # label = "Manual Input",
                  placeholder = 'year,date,WYT,barrier,VNS,OMT,T_MSD,T_CLC,CVP,SWP\n2013,"2013-04-29","Out",4130,-623,18.2,20.6,816,2421',
                  rows = 3
                )
              )
              # )
            )
            # tags$details(
            #   id = "summ_man_in1",
            #   title = "Click to open or close",
            #   style="margin-top:20px; padding: 0px; color:#337ab7 ; background-color:white",
            #   tags$summary(
            #     id="summ_man_in1",
            #     "Manual Input",
            #     style = "font-size: 14px; font-weight: bold; background-color:#ddd; padding: 5px"
            #   ),
            #   div(
            #     style = "padding: 10px;",
            #     role = "menu",
            #     div(
            #       title = "aaa",
            #       role = "menuitem",
            #       # p("textAreaInput")
            #       textAreaInput(
            #         inputId = "manual_input",
            #         label = NULL,
            #         # label = "Manual Input",
            #         placeholder = 'year,date,WYT,barrier,VNS,OMT,T_MSD,T_CLC,CVP,SWP\n2013,"2013-04-29","Out",4130,-623,18.2,20.6,816,2421',
            #         rows = 3
            #       )
            #     )
            #   )
            # )
          ),
          column(
            width = 7,
            div(
              tags$ul(
                style = "padding-inline-start: 10px;",
                tags$li(
                  h5(
                    "[[Datatable placeholder]]"
                  )
                )
              )
            )
          )
        )
      )
    )
  })

  output$details_up_data_bar <- renderUI({
    tags$details(
      title = "Click to open or close",
      id = "my_deet1",
      style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
      tags$summary(
        "Load Daily Environmental Data", # "Upload/Review Daily Environmental Data",
        style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
      ),
      wellPanel(
        # fluidRow(
        # column(width=6,
        div(
          role = "menuitem",
          title = "aaa",
          # p("fileInput")
          fileInput(
            inputId = "file1",
            label = "Choose CSV File",
            # buttonLabel="aaa",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),
        shiny::fluidRow(
          style = "margin-top:20px",
          column(
            width = 4,
            div(
              role = "menuitem",
              actionButton(
                inputId = "run_button",
                label = "Load"
              )
            )
          ),
          column(
            width = 4,
            div(
              role = "menuitem",
              actionButton(
                inputId = "reset_button",
                label = "Reset"
              )
            )
          ),
          column(
            width = 4,
            downloadButton(
              outputId = "download_sample",
              label = "Sample CSV",
              icon = shiny::icon("download")
            )
          )
        )
      )
      # ),
      # column(width=6,p("hhh"))
    )

    #               ,
    #  tags$details(
    #     id="my_deet2",
    #     title="Click to open or close",
    #     # style="margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
    #     tags$summary(
    #       # "Select Fork Length",
    #       strong("Manual Input"),
    #    style = "font-size: 18px; font-size: 18px; padding-left: 14px; padding-top: 2px; padding-bottom: 2px;background-color:#ddd;"),
    #    div(
    #               style="padding: 10px;",
    #               role = "menu",
    #               div(
    #                 title="aaa",
    #                 role = "menuitem",
    #                 # p("textAreaInput")
    #                 textAreaInput(
    #                   inputId = "manual_input",
    #                   label = NULL,
    #                   # label = "Manual Input",
    #                   placeholder = 'year,date,WYT,barrier,VNS,OMT,T_MSD,T_CLC,CVP,SWP\n2013,"2013-04-29","Out",4130,-623,18.2,20.6,816,2421',
    #                   rows = 3
    #                 )
    #               )
    #             )

    #     )

    #   ,
    #   shiny::tags$script(shiny::HTML(
    #     "document.addEventListener('DOMContentLoaded', function() {
    #   const dropdown = document.querySelector('#dropdown-menu-drop' + Object.keys(window.shinyWidgets.dropdownStates)[0]);
    #   if (dropdown) {
    #     dropdown.removeAttribute('role');
    #     dropdown.querySelectorAll('[role=\"menuitem\"]').forEach(el => el.removeAttribute('role'));
    #   }
    # });"
    #   ))
    # )
    # )
    # )
    # )
  })

  output$details_indiv_attrib_ui <- renderUI({
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
  })

  # output$accord_panel_indiv_attrib_ui <- renderUI({
  #   shinydashboardPlus::accordion(
  #     id = "accordion2",

  #     shinydashboardPlus::accordionItem(
  #       title = "Set Individual Attributes",
  #       collapsed = TRUE,
  #       p("temp")
  #     #   column(
  #     #     width = 6,
  #     #     h5(strong("Fork Length")),
  #     #     tags$ul(
  #     #       style = "padding-inline-start: 20px;",
  #     #       tags$li(
  #     #         "By default, all predictions are based on the average fork length of juvenile Steelhead used in modeling."
  #     #       )
  #     #     )
  #     #   ),
  #     #   column(width = 6, shiny::uiOutput("flength_sel_ui"))
  #     )
  #   )
  # })

  output$input_panel_UI <- renderUI({
    shinydashboardPlus::box(
      id = "input_box2",
      title = "Inputs",
      # title = shiny::HTML("Inputs"),
      solidHeader = TRUE,
      # status = "warning",
      status = "primary",
      collapsible = T,
      collapsed = FALSE,
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
        p(
          "Environmental conditions are represented using 
          daily summaries of field measurements obtained from monitoring stations throughout the region. The conditions on
          the day of arrival at key junctions serve as inputs for statistical sub-models that are used to generate route usage
          and survival probability predictions."
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
            # p("test",style="text-align: right;"),
            # div(class = "pull-right",

            # shinydashboardPlus::box            ,
            div(
              title = "click for more information",
              id = "schem_info_drop_div",
              # class = "pull-right",
              style = "float:right !important;
                      position: relative;
                      z-index: 2;",
              # position:fixed; !important",
              shinyWidgets::dropdownButton(
                # id="schem_info_drop",
                right = TRUE,
                up = FALSE,
                circle = TRUE,
                # tooltip=TRUE,
                size = "xs",
                status = "primary",
                icon = icon("info", style = "color: white;"),
                width = "300px",
                # tooltipOptions=shinyWidgets::tooltipOptions( title = "Params", html = FALSE)
                # tooltipOptions=shinyWidgets::tooltipOptions( title = "Params", html = TRUE)
                # ,
                # label=
                p("Major routes and key junctions in the Delta") #,
              )
            ),
            shiny::uiOutput("schem_start_loc_plt_ui")
            # ,
            #    div(class = "pull-right",
            #     shinyWidgets::dropdownButton(
            #       right = TRUE,
            #       up = TRUE,
            #       circle = TRUE,
            #       size = "xs",
            #       status = "primary",
            #       icon = icon("info", style = "color: white;"),
            #       width = "300px",
            #       p("Major routes and key junctions in the Delta ")
            #   )
            # )
          )
        )
      ),
      column(
        width = 12, #background-color:gray;
        #   tags$details(
        #     id="my_deet1",
        #     style="margin-top:5px; padding: 5px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:#ddd; ",
        #     tags$summary(
        #       "Select/Review Daily Environmental Data",
        #   style = "font-size: 18px;"),
        #   shiny::uiOutput("prev_yr_ui")
        # )
        # ,
        # tags$details(
        #   id="my_deet1",
        #   style="margin-top:5px; padding: 5px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:#ddd",
        #   tags$summary(
        #     "Upload/Review Daily Environmental Data",
        # style = "font-size: 18px; "),
        # shiny::uiOutput("details_up_data_bar")
        # deet_panel_prev_upload_ui
        shiny::uiOutput("deet_panel_prev_upload_ui"),
        # )

        tags$details(
          id = "my_deet2",
          title = "Click to open or close",
          style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
          tags$summary(
            # "Select Fork Length",
            "Select Individual Attributes",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px; padding-bottom: 2px;background-color:#ddd;"
          ),
          shiny::uiOutput("details_indiv_attrib_ui")
          #  "This content is inside"
        )
      ),
      hr(),
      # uiOutput("input_panel_UI_3"),
      # uiOutput("deet_panel_prev_upload_ui"),

      # uiOutput("accord_panel_indiv_attrib_ui"),
      uiOutput("accord_panel_view_in_ui")
      #     ,
      # footer =

      # uiOutput("chk_input_ui")
      #  fluidRow(
    )
  })

  output$deets_overall_ests <- renderUI({
    # fluidRow(
    tags$details(
      id = "my_deet2",
      title = "Click to open or close",
      style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
      tags$summary(
        "Overall Survival", # "Upload/Review Daily Environmental Data",
        style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
      ),
      p("empty_panel")
    )
    # )

    #   fluidRow(
    #   tags$details(
    #     title = "Click to open or close",
    #     id = "my_deet1",
    #     style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
    #     tags$summary(
    #       "Overall Survival", # "Upload/Review Daily Environmental Data",
    #       style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
    #                 padding-bottom: 2px;background-color:#ddd;"
    #     ),
    #     p("empty_panel")
    #   )
    # )
  })

  output$input_panel_UI_3 <- renderUI({
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
        # fluidRow(
        uiOutput("chk_input_ui")
      ),
      column(
        width = 12,
        uiOutput("deets_overall_ests")
      )
      # ,
      # shinydashboardPlus::accordion(
      #   id = "accordion4",

      #   shinydashboardPlus::accordionItem(
      #     title = "Overall Survival",
      #     collapsed = FALSE,
      #     plotOutput("doy_surv_ggplt")
      #     # p("overall survival")
      #   )
      # )
      # h2("stuff here")
    )
  }) # output$input_panel_UI_2 <- renderUI({
  #   shinydashboardPlus::box(
  #     id = "input_box3",
  #     title = shiny::HTML("More Inputs"),
  #     solidHeader = TRUE,
  #     status = "primary",
  #     collapsible = T,
  #     collapsed = TRUE,
  #     width = 12,
  #     h2("stuff here")
  #   )
  #   })

  output$chk_input_ui <- renderUI({
    div(
      column(
        width = 12,
        style = "margin-bottom: 20px",
        br(),
        div(
          # class = "thumbnail-section",
          # style = "margin-left: 40px;",
          h4(strong("Check Inputs")),
          p(
            "Verify that selected/uploaded values conform with data used to fit statistical sub-models"
          )
        ),
        column(
          width = 6,
          # actionButton("check_inputs_butt", "Check Inputs")
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
          ),
          hr()
        ),
        # ,
        # actionButton("generate_ests_butt", "Generate Estimates"

        column(
          width = 3,
          # actionButton("load_butt", "Load"),
          actionButton("reset_butt", "Reset"),

          # checkboxInput("load_check", label = "load"),
          # actionButton("inputs_done", "Done")
        )
      )
    )
  })

  observeEvent(
    input$check_inputs_butt,
    # input$load_butt
    {
      # shinyWidgets::updatePrettyCheckbox(
      shinyWidgets::updatePrettyCheckbox(
        session = session,
        inputId = "inputs_check",
        value = TRUE
      )

      shinyjs::enable("generate_ests_butt")
      # updateinputs_check
    }
  )

  observeEvent(input$reset_butt, {
    # shinyWidgets::updatePrettyCheckbox(
    shinyWidgets::updatePrettyCheckbox(
      session = session,
      inputId = "inputs_check",
      value = FALSE
    )
    # updateinputs_check

    shinyjs::disable("generate_ests_butt")
  })

  output$time_of_year_ui <- renderUI({
    tagList(
      # div(
      #   style = "display: inline-flex; align-items: left;margin-top: 10px;margin-left: 20px;",
      #   div(
      #     style = "margin-right: 20px;",
      #     shiny::uiOutput("start_date_entry_sep_ui")
      #     # ,
      #     # shiny::uiOutput("start_date_entry_ui")
      #   )
      # ),

      div(
        style = "display: left;margin-top: 10px;width: 600px;",
        shiny::uiOutput("DOY_slider_ui"),
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
              # tooltip=TRUE,
              # tooltipOptions=shinyWidgets::tooltipOptions(title = "..."),
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
      # )
    )
  })

  # needs to be dynamic
  output$prev_yr_ui <- renderUI({
    tagList(
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
                # h5(
                #   "Select a previous year from the dropdown menu or by clicking on a row in the 'Annual Summary Table' clicking on a row"
                # ),

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
                          shiny::uiOutput("year_picker_ui")
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
          # div(
          #   style = "display: inline-flex; align-items: left;margin-top: 10px;margin-left: 20px;",
          #   div(
          #     style = "margin-right: 20px;",

    tagList(
      validate(
        need(
          in_selected_RV$past_water_year != "None",
          label = "past_water_year",
          message = "Select a previous year"
        )
      ),
      div(
        style="display: flex; gap:20px; align-items:center",        
        # style="display: flex; gap:20px; align-items:center;",

          shiny::uiOutput("start_date_entry_sep_ui"),
          div(
        style="display: flex; gap:20px;",
                      shinyWidgets::dropMenu(
              placement="bottom",              
              tag=actionButton(
                inputId = "doy_slider_dropdown",
                label = HTML('<i class="fas fa-sliders" role="presentation" aria-label="sliders icon"></i> Day of Year Slider'
                )
              ),
              shiny::uiOutput("time_of_year_ui")
            )
          )
          # shiny::uiOutput("start_date_slider_ui")
      )
    )
    
          # ,
          # shiny::uiOutput("start_date_entry_ui")
          #   )
          # )


        ), #,
        column(
          width = 7,
          # style="border: solid 2px black; margin:5px;",
          div(
            style = "border: solid 2px black; margin:10px;height:525px;",
            span(
              style = "display:flex; justify-content: space-between; margin-left:10px; margin-right:10px; align-items:center",
              title = "attributes of years 2011-2025",
              h4("Annual Summary Table"),
              shinyWidgets::dropMenu(
               shinyWidgets::circleButton(inputId = "btn1", icon = icon("info"),status="primary",size="xs"),
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
                ,
                placement="left-start"
              )
            ),
            uiOutput("table_in_WY_UI")
          )
        )
        # shinydashboardPlus::box(
        #   width = 7,
        #   title = "Annual Summary Table",
        #   dropdownMenu = shinydashboardPlus::boxDropdown(
        #     circle = TRUE,
        #     # size = "xs",
        #     size = "md",
        #     status = "primary",
        #     icon = icon("circle-info", style = "color: #024c63;"),
        #     id = "ann_summ_drop",
        #     shinydashboardPlus::boxDropdownItem(
        #       tags$dl(
        #         # definition list
        #         div(
        #           style = "margin-left: 20px;",
        #           HTML('<strong>Year:</strong> Calendar Year</p>'),
        #           HTML('<strong>Category:</strong> Water Year Type (SJ)</p>'),
        #           HTML(
        #             '<strong>HOR Barrier:</strong> Barrier at Head of Old River</p>'
        #           ),
        #           HTML(
        #             '<strong>Model:</strong> Data in year to fit statistical sub-models</p>'
        #           )
        #         )
        #       )
        #     ),
        #     shinydashboardPlus::boxDropdownItem(
        #       tags$dl(
        #         # definition list
        #         div(
        #           style = "margin-left: 20px;",
        #           HTML('<strong>Year:</strong> Calendar Year</p>'),
        #           HTML('<strong>Category:</strong> Water Year Type (SJ)</p>'),
        #           HTML(
        #             '<strong>HOR Barrier:</strong> Barrier at Head of Old River</p>'
        #           ),
        #           HTML(
        #             '<strong>Model:</strong> Data in year to fit statistical sub-models</p>'
        #           )
        #         )
        #       )
        #     )
        #   ),
        #   uiOutput("table_in_WY_UI")
        # )
      )
    )
  })

          #   output$start_date_slider_ui <- renderUI({
          #     # tagList(
          # # div(
          # #   style="display:inline-flex;align:center;",

          # #   style = "padding-top: 20px;",
          # #   role = "menu",
          #   # shinyWidgets::dropdown(
          #   #   label = "Day of Year Slider",
          #   #   icon = icon("sliders"),
          #   #   title = "Click to open or close",
          #   #   size = "md",
          #   #   width = "600px",
          #   #   shiny::uiOutput("time_of_year_ui")
          #   # )
          #   shinyWidgets::dropMenu(
          #     placement="bottom",              
          #     tag=actionButton(
          #       inputId = "doy_slider_dropdown",
          #       label = HTML('<i class="fas fa-sliders" role="presentation" aria-label="sliders icon"></i> Day of Year Slider'
          #       )
          #     ),
          #     shiny::uiOutput("time_of_year_ui")
          #   )
          # # )
          # })


  output$env_panel_ui <- renderUI({
    tagList(
      switch(
        data_source_selected(),
        # shinydashboardPlus::box(
        # title = shiny::HTML("Environmental and Operational Variables (Previous years)"),
        # solidHeader = TRUE,
        # status="primary",
        # collapsible = F,
        # collapsible = T,

        "prev_pan" = tagList(
          p("old")
        ),
        "up_pan" = p("old")
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

  # rendering annual data table
  output$table_in_WY <- DT::renderDataTable(
    DT::datatable(
      ann_HORbar_WYT_data_TAB(),
      # selection="none",
      selection = list(
        mode = 'none',
        target = "row"
        # selected = which(ann_HORbar_WYT_data$Year == init_water_year)
        # selected = NULL
      ),
      colnames = c("Year", "Category", "HOR Barrier", "Model"),
      rownames = FALSE,
      options = list(
        info = FALSE,
        dom = "t",
        # dom = '<"<"bottom"ip>', # only the bottom
        stripeClasses = list(),
        # pageLength = 9,
        pageLength = 14,

        pagingType = "simple"
        ,
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '70%'});",
          "$(this.api().table().body()).css({'font-size': '70%'});",
          "$(this.api().table().caption()).css({'font-size': '70%'});",
          "$(this.api().table().footer()).css({'font-size': '70%'});",
          "}"
        )
      )
    ) |>
      DT::formatStyle(
        'WYT',
        backgroundColor = DT::styleEqual(wyt_type_opt, WYT_cols)
      ) |>
      DT::formatStyle(
        'Model',
        color = DT::styleEqual(c("Yes", "No"), c("#035a00", "#690202"))
        # ,
        # backgroundColor = DT::styleEqual(c("Yes", "No"), c("white", "#B3B3B3"))
      ) |>
      # only works for background color
      DT::formatStyle(
        columns = c('Year', 'WYT', 'barrier', 'Model'),
        target = "row",
        backgroundColor = DT::styleEqual(
          input$year_picker,
          c("#61bce6")
        )
      ) |>
      DT::formatStyle(
        'Year',
        # border = rep("4px solid #024c63",14)
        borderTop = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        ),
        borderBottom = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        ),
        borderLeft = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        )
      ) |>
      DT::formatStyle(
        'barrier',
        'Year',
        borderTop = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        ),
        borderBottom = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        )
      ) |>
      DT::formatStyle(
        'WYT',
        'Year',
        borderTop = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        ),
        borderBottom = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        )
      ) |>
      DT::formatStyle(
        'Model',
        'Year',
        borderTop = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        ),
        borderBottom = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        ),
        borderRight = DT::styleEqual(
          input$year_picker,
          c("4px solid #024c63")
        )
      )
    #         |>
    #   DT::formatStyle(
    #   'barrier',
    #   border = ifelse(ann_HORbar_WYT_data$Year==in_selected_RV$past_water_year,"1px solid black",NA)
    # )
  )

  test_proxy <- DT::dataTableProxy("table_in_WY")

  # update rows
  observeEvent(input$year_picker, {
    in_selected_RV$past_water_year <- as.numeric(input$year_picker)
    # if(in_selected_RV$past_water_year!=input$table_in_WY_rows_selected){
    DT::selectRows(
      test_proxy,
      match(as.numeric(input$year_picker), ann_HORbar_WYT_data$Year)
    )
    # }
  })

  observeEvent(
    input$table_in_WY_rows_selected,
    {
      # year_picker
      in_selected_RV$past_water_year <- ann_HORbar_WYT_data[
        input$table_in_WY_rows_selected,
        "Year"
      ]
      in_selected_RV$WYT <- ann_HORbar_WYT_data[
        input$table_in_WY_rows_selected,
        "WYT"
      ]
      in_selected_RV$BAR <- ann_HORbar_WYT_data[
        input$table_in_WY_rows_selected,
        "barrier"
      ]
      # initializing global day of year variables
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

  # observeEvent(in_selected_RV$past_water_year!=input$year_picker, {
  # observeEvent(input$start_loc_in, {
  #     shinyWidgets::updatepickerinput(inputs$year_picker,selected=in_selected_RV$past_water_year,session=session)
  #  })

  # update values when water year is selected
  observeEvent(
    input$load_butt,
    # input$load_check == TRUE
    {
      global$past_water_year <- ann_HORbar_WYT_data[
        input$table_in_WY_rows_selected,
        "Year"
      ]
      global$WYT <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected, "WYT"]
      global$BAR <- ann_HORbar_WYT_data[
        input$table_in_WY_rows_selected,
        "barrier"
      ]
      # initializing global day of year variables
      global$start_date <- as.Date(
        paste(global$start_day, global$past_water_year, sep = "-"),
        format = "%j-%Y"
      )
      global$end_date <- as.Date(
        paste(global$end_day, global$past_water_year, sep = "-"),
        format = "%j-%Y"
      )
      global$flength <- in_selected_RV$flength
    }
  )

  # Your application server logic
  # SIDE BAR UI
  output$cbr_dyn_sidebar_ui = shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(
        "About",
        tabName = "about",
        icon = icon("house"),
        selected = T
      ),
      #   shinydashboard::menuItem(
      #   "Prediction",
      #    startExpanded = TRUE,

      #   shinydashboard::menuSubItem(
      #                   text = "aaa",
      #                   icon = icon("sliders")
      #               )

      # ),
      shinydashboard::menuItem(
        "Inputs",
        tabName = "inputs",
        icon = icon("sliders"),
        startExpanded = T,
        selected = F
      ),
      # ,
      # shinydashboard::menuItem(
      #   "Methods and References",
      #   tabName = "met_ref",
      #   icon = icon("book")
      # )
      shinydashboard::menuItem(
        text = "Estimates",
        tabName = "estimates",
        icon = icon("chart-line"),
        startExpanded = T #,
      ),
      br(),
      tags$div(
        id = "newsidebox",
        wellPanel(
          uiOutput("sidebar_text") # Placeholder for dynamic text
        )
      )
    )
  })

  # because this is within a renderUI, changing the input$tab value rewrites the sidebar content
  output$sidebar_text <- renderUI({
    shiny::HTML(
      # shiny::HTML("sidebar")
      md_txt_extract(
        md_addr = "inst/app/www/mds/sidebar.md",
        header_ref = paste0("# ", tab_selected()),
        asHTML_frag = TRUE
      )
    )
  })

  #  output$about_page_UI <-  renderUI({})

  # because this is within a renderUI, changing the input$tab value rewrites the sidebar content
  output$top_of_body_text <- renderUI({
    switch(
      tab_selected(),

      # "about"=shiny::tagList(fluidRow( h2("new_render_pg_about")  )),
      # "main"=shiny::tagList(fluidRow( h2("new_render_pg_main")  )),
      # "main_input"=shiny::tagList(fluidRow( h2("new_render_pg_main_input")  ))

      "about" = shiny::tagList(
        # p("temp_about")
        # ,
        mod_about_page_ui("mod_about_page-about_page_ui_1")

        # uiOutput("about_page_UI")
        # fluidRow(
        # h2("new_render_pg_about"),
        #shiny::HTML(
        # wellPanel(
        # shiny::HTML(md_txt_extract(
        #   md_addr = "inst/app/www/mds/page.md",
        #   # header_ref = paste0("# ", tab_selected()),
        #   header_ref = paste0("# ", "about"),
        #   asHTML_frag = TRUE
        # )
        # )
      ),
      "met_ref" = shiny::tagList(
        fluidRow(
          tagList(
            shinydashboard::box(
              title = HTML("Methods and References"),
              width = 12,
              solidHeader = TRUE,
              status = "primary",
            # div(
            
              shiny::withMathJax(shiny::includeMarkdown(system.file(
                "app/www/main/met_and_ref/overview_pt1.md",
                package = "CVPASapp"
              )))
            ,
              # SVG plot with embedded tooltips
              bscui::bscuiOutput(
                outputId = "surv_route_diagram_wtt",
                width = "70%",
                height = "100%"
              ),

              shiny::withMathJax(shiny::includeMarkdown(system.file(
                "app/www/main/met_and_ref/how_calc_pt2.md",
                package = "CVPASapp"
              ))),
              # app/www
              bscui::bscuiOutput(
                outputId = "my_red_svg",
                width = "100%",
                height = "100%"
              ),
              shiny::withMathJax(shiny::includeMarkdown(system.file(
                "app/www/main/met_and_ref/left_col_text.md",
                package = "CVPASapp"
              )))
            ,
            tags$img(
            # src = "www/simple_route_image.png",
            # style=width="500px",
            src = "https://onlinelibrary.wiley.com/cms/asset/25dec1bb-e0e1-40ea-95e2-f7762044473f/nafm11005-fig-0001-m.jpg",
            style = "width: 30%; height: 30%;"
            # style = "width: 100%; height: 100%;"#,# border: 2px solid #024c63;",
            # name = "Schematic view of junctions and routes through the south Delta ",
            # alt = "Routes bifurcate at the Head of Old River and with the path along the San Joaquin River splitting again at Turner Cut junction; all paths converge prior to reaching Chipps Island"
            )
            ,
            tags$img(
            # src = "www/simple_route_image.png",
            # style=width="500px",
            src = "https://cdnsciencepub.com/cms/10.1139/cjfas-2020-0467/asset/images/large/cjfas-2020-0467f1.jpeg",
            style = "width: 30%; height: 30%;"
            # style = "width: 100%; height: 100%;"#,# border: 2px solid #024c63;",
            # name = "Schematic view of junctions and routes through the south Delta ",
            # alt = "Routes bifurcate at the Head of Old River and with the path along the San Joaquin River splitting again at Turner Cut junction; all paths converge prior to reaching Chipps Island"
            )
            ,
            shiny::includeMarkdown(system.file(
              "app/www/biblio_doc.md",
              package = "CVPASapp"
            ))
            )
          )
        )
      ),
      "inputs" = shiny::tagList(
        uiOutput("input_page_UI")
      )
    )

    # )
    # )
  })

  output$start_date_entry_sep_ui <- renderUI({
    # tagList(
      # validate(
      #   need(
      #     in_selected_RV$past_water_year != "None",
      #     label = "past_water_year",
      #     message = "Select a previous year"
      #   )
      # ),
      div(
        style="display: flex; gap:20px;",
        # column(
        #   width = 6,
          shinyWidgets::airDatepickerInput(
            inputId = "date_start_sep",
            label = "Start Date :",
            addon = "none",
            width="100px",
            # style="width:200px;",
            # style="font-size:5px",
            value = c(
              as.Date(
                paste(in_selected_RV$past_water_year, in_selected_RV$start_day),
                "%Y %j"
              )
              # ,
              # as.Date(
              #   paste(in_selected_RV$past_water_year, in_selected_RV$end_day),
              #   "%Y %j"
              # )
            ),
            minDate = as.Date("2011-01-01"),
            maxDate = as.Date("2024-12-31"),
            range = FALSE,
            # inline=TRUE,
            disabledDaysOfWeek = TRUE
            # ,
            # width = "80%"
          )
        # )
        ,
        # column(
        #   width = 6,
          shinyWidgets::airDatepickerInput(
            inputId = "date_end_sep",
            label = "End Date :",
            addon = "none",
            width="100px",
            value = c(
              # as.Date(
              #   paste(in_selected_RV$past_water_year, in_selected_RV$start_day),
              #   "%Y %j"
              # )
              # # ,
              as.Date(
                paste(in_selected_RV$past_water_year, in_selected_RV$end_day),
                "%Y %j"
              )
            )
          )
          # ,
          # minDate = as.Date("2011-01-01"),
          # maxDate = as.Date("2024-12-31"),
          # range = FALSE,
          # # inline=TRUE,
          # disabledDaysOfWeek = TRUE
          # ,
          # width = "80%"
        # )
    #   )
    )
  })

  # output$start_date_entry_ui <- renderUI({
  #   tagList(
  #     shinyWidgets::airDatepickerInput(
  #       inputId = "Id111",
  #       label = "Start and End Dates :",
  #       # value = c(as.Date("2011-01-01"),as.Date("2024-12-31")),
  #       value = c(
  #         as.Date(
  #           paste(in_selected_RV$past_water_year, in_selected_RV$start_day),
  #           "%Y %j"
  #         ),
  #         as.Date(
  #           paste(in_selected_RV$past_water_year, in_selected_RV$end_day),
  #           "%Y %j"
  #         )
  #       ),
  #       minDate = as.Date("2011-01-01"),
  #       maxDate = as.Date("2024-12-31"),
  #       range = TRUE,
  #       disabledDaysOfWeek = TRUE,
  #       width = "80%"
  #     )
  #     #  ,
  #     # daterangepicker::daterangepicker(
  #     #   inputId = "daterange_sel",
  #     #   # label = "Date:",
  #     #   label = NULL,
  #     #   icon=icon("calendar"),
  #     #   start = as.Date(
  #     #     paste(in_selected_RV$past_water_year, in_selected_RV$start_day),
  #     #     "%Y %j"),
  #     #   end = as.Date(
  #     #     paste(in_selected_RV$past_water_year, in_selected_RV$end_day),
  #     #     "%Y %j"),
  #     #   min = "2011-01-01",
  #     #   max = "2024-12-31",
  #     #   language = "en",
  #     #   style = "width:50%; border-radius:4px",
  #     #   options = list(
  #     #     "minYear" = in_selected_RV$past_water_year,
  #     #     "maxYear" = in_selected_RV$past_water_year,
  #     #     "cancelIsClear" = TRUE,
  #     #     "autoApply" = TRUE))
  #   )
  # })

  output$DOY_slider_ui <- renderUI({
    tagList(
      sliderInput(
        "DOYslider_rng",
        label = NULL,
        min = 0,
        max = 250,
        value = c(in_selected_RV$start_day, in_selected_RV$end_day),
        width = '99%'
      )
    )
  })

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
        style = "display: inline-flex; align-items: center;",
        shiny::uiOutput("flength_num_in"),
        div(
          style = "margin-left: 20px;",
          shiny::uiOutput("flength_dash_ui"),
          div(
            style = "display: left;margin-top: 10px;",
            div(
              style = "display: flex;",

              HTML(paste("<p> Fork length of juvenile Steelhead </p> ")),
              # HTML(paste("<p> Fork lenght of <strong>",(input$start_loc_in),"</strong> junction </p> "))
              # shiny::HTML(
              #   "<p style ='font-size:80%'> Histogram of day of year when acoustic-tagged juvenile Steelhead were detected at each location. </p>"
              # )
              div(
                style = "margin-left: 10px; z-index: 2",
                # class = "pull-right",
                # style="float:right !important;
                #         position: relative;
                #         z-index: 2;",
                shinyWidgets::dropdownButton(
                  right = FALSE,
                  up = TRUE,
                  circle = TRUE,
                  size = "xs",
                  status = "primary",
                  icon = icon("info", style = "color: white;"),
                  width = "300px",
                  p(
                    "Lengths of fish relased during acoustic telemetry studies (2011-2016). Overall mean fork length selected by default."
                  )
                )
              )
            )
          )
          # HTML(
          #   "<p style ='font-size:80%'>Fork length of juvenile Steelhead used in acoustic telemetry studies (2011-2016). </p>"
          #   # "<p style ='font-size:80%'>Fork length (mm). </p>"
          # )
        )
      )
    )
    # )
  })

  output$flength_dash_ui <- renderUI({
    shiny::tagList(
      plotOutput("flength_ref_hist", height = "100px")
    )
  })

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
    # triggers re-rendering
    # input$start_loc_in
    # input$radio_doy_type
    # ggplt_doy_ref_hist(
    #   DOY_arvDF_l_in = DOY_arvDF_l,
    #   LOC_in = in_selected_RV$LOC,
    #   start_day_in = in_selected_RV$start_day,
    #   end_day_in = in_selected_RV$end_day
    # )

    ggplt_doy_ref_hist(
      DOY_arvDF_l_in = DOY_arvDF_l,
      LOC_in = in_selected_RV$LOC,
      start_day_in = in_selected_RV$start_day,
      end_day_in = in_selected_RV$end_day
    )
  })

  output$input_page_UI <- renderUI({
    tagList(
      uiOutput("input_panel_UI"),
      #  ,
      #  uiOutput("input_panel_UI_2")
      uiOutput("input_panel_UI_3")
    )
  })

  # observe flength numeric input with debounce
  flength_mod <- reactive(input$flength_num_in_dash)
  flength_d <- debounce(flength_mod, 500) # prevents endless looping
  observeEvent(flength_d(), {
    in_selected_RV$flength <- flength_d()
  })

  output$flength_num_in <- renderUI({
    numericInput(
      "flength_num_in_dash",
      # label = NULL,
      label = "Fork Length (mm)",
      step = 10,
      value = in_selected_RV$flength, #init_flength
      width = "80px",
      min = 100,
      max = 400
    )
  })

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
      ), #,"HOR Barrier"), #"Year","Day of Year",
      rownames = FALSE,
      # caption = 'Table 1: This is a simple caption for the table.'
    )
  )

  # Interactive lattice plot
  #  output$doy_latt_pltly <-  plotly::renderPlotly({
  output$doy_surv_ggpplt <- renderPlot({
    CVhelp_dat_l$site <- CVhelp_dat_l$variable
    CVhelp_dat_l$var <- CVhelp_dat_l$variable
    CVhelp_dat_l$year <- CVhelp_dat_l$Year

    doy_rng_in <- c(
      global$start_day,
      global$end_day
    )

    doy_int1 <- doy_rng_in[1]
    doy_int2 <- doy_rng_in[2]
    CVhelp_dat_l$doy_int1 <- doy_int1
    CVhelp_dat_l$doy_int2 <- doy_int2
    CVhelp_dat_l$SELECTED <- CVhelp_dat_l$DOY >= doy_int1 &
      CVhelp_dat_l$DOY <= doy_int2
    # dat_w_subb <- subset(CVhelp_dat_w,DOY >= doy_int1 & CVhelp_dat_w$DOY <= doy_int2 )

    dat_w_subb <- CVhelp_dat_w

    dat_w_subb$est_day <- 0.02 +
      ((dat_w_subb$VNS - 8.010345) / 1.5) +
      0.6 * ((dat_w_subb$CVP - 1657) / 1500) +
      +((dat_w_subb$OMT - 1200) / 5000) #+  #fix in a bit
    # dat_w_subb$est_day[dat_w_subb$WYT=="Wet"]=dat_w_subb$est_day+ 0.04
    dat_w_subb$est_S <- plogis(dat_w_subb$est_day)
    # dat_w_subb <- dat_w_subb[which(CVhelp_dat_w$DOY >= doy_int1 & CVhelp_dat_w$DOY <= doy_int2),]
    dat_w_subb$SELECTED <- CVhelp_dat_w$DOY >= doy_int1 &
      CVhelp_dat_w$DOY <= doy_int2

    # ifelse(dat_w_subb$WYT[dat_w_subb$WYT=="Wet"],0.2,0) +
    # selected values fill
    surv_plt_wrap <- ggplot2::ggplot(
      data = dat_w_subb,
      ggplot2::aes(
        y = est_S,
        x = DOY,
        fill = SELECTED,
        color = SELECTED,
        group = Year,
        xmin = doy_int1,
        xmax = doy_int1
      )
    ) +
      ggplot2::geom_line(linewidth = 0.25) +
      ggplot2::geom_point(shape = 21, size = 1) +
      ggplot2::scale_fill_manual(values = c("darkgray", "#428BCA")) +
      ggplot2::scale_color_manual(values = c("gray40", "#28547A")) +
      ggplot2::geom_vline(xintercept = doy_int1) +
      ggplot2::geom_vline(xintercept = doy_int2) +
      ggplot2::labs(x = "Day of Year") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::facet_wrap(~Year)

    surv_plt_wrap
  })

  output$doy_pred_ggpplt <- renderPlot({
    # print(head(CVhelp_dat_l,3))
    CVhelp_dat_l$site <- CVhelp_dat_l$variable
    CVhelp_dat_l$var <- CVhelp_dat_l$variable
    CVhelp_dat_l$year <- CVhelp_dat_l$Year
    # print(head(CVhelp_dat_l,3))

    doy_rng_in <- c(
      global$start_day,
      global$end_day
    )

    doy_int1 <- doy_rng_in[1]
    doy_int2 <- doy_rng_in[2]
    CVhelp_dat_l$doy_int1 <- doy_int1
    CVhelp_dat_l$doy_int2 <- doy_int2
    CVhelp_dat_l$SELECTED <- CVhelp_dat_l$DOY >= doy_int1 &
      CVhelp_dat_l$DOY <= doy_int2

    dat_w_subb <- CVhelp_dat_w
    dat_w_subb$est_day <- 0.02 +
      ((dat_w_subb$VNS - 8.010345) / 1.5) +
      0.6 * ((dat_w_subb$CVP - 1657) / 1500) +
      ((dat_w_subb$OMT - 1200) / 5000) +
      global$flength #0.5*(global$flength-244)/30  #fix in a bit
    dat_w_subb$est_S <- plogis(dat_w_subb$est_day)
    dat_w_subb$SELECTED <- CVhelp_dat_w$DOY >= doy_int1 &
      CVhelp_dat_w$DOY <= doy_int2
    new_predDFests$EST_lp <- new_predDFests$EST_lp +
      0.05 * (global$flength - 244) / 30
    new_predDFests$EST <- plogis(new_predDFests$EST_lp)

    # ifelse(dat_w_subb$WYT[dat_w_subb$WYT=="Wet"],0.2,0) +
    # selected values fill
    ggplot2::ggplot() +
      # ggplot2::geom_ribbon(data=new_predDFests,ggplot2::aes(y=EST,x=date,ymin=pLCL,ymax=pUCL),fill="gray60") +
      ggplot2::geom_ribbon(
        data = new_predDFests %>%
          dplyr::filter(DOY > doy_int1 & DOY < doy_int2),
        ggplot2::aes(y = EST, x = DOY, ymin = LCL, ymax = UCL),
        fill = "gray30"
      ) +
      # ggplot2::geom_line(data=predDFcomb_modsub,ggplot2::aes(y=EST,x=date,color=factor(id))) +
      # ggplot2::geom_ribbon(data=predDFcomb_modsub,ggplot2::aes(y=EST,x=date,color=factor(id),ymin=LCL,ymax=UCL),fill=NA) +
      ggplot2::geom_point(
        data = new_predDFests,
        ggplot2::aes(y = EST, x = DOY, ymin = LCL, ymax = UCL)
      ) +
      ggplot2::facet_wrap(~Year, scale = "free_x") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_vline(xintercept = doy_int1) +
      ggplot2::geom_vline(xintercept = doy_int2) +
      ggplot2::scale_fill_manual(values = c("darkgray", "#428BCA")) +
      ggplot2::scale_color_manual(values = c("gray40", "#28547A")) #+
    # labs("S_TCJ-CHP")
  })
}
