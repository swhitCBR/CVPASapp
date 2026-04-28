#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # autoselect specify starting tab

  tab_selected <- reactiveVal("main_input")
  data_source_selected <- reactiveVal("prev_pan")
  shinydashboard::updateTabItems("tabs", session = session, selected = "main_input")

  # {DEBUGGING}
  observeEvent(input$tabs, {  
    print(input$tabs)  
    tab_selected(input$tabs)
  })

  # Switching based on data source
  observeEvent(input$data_source_picker, {  
    print(input$env_dat_tabpan)
    if(input$data_source_picker=="Previous year"){
      data_source_selected("prev_pan")
      updateTabsetPanel(session=session, inputId="env_dat_tabpan", selected = "prev_pan")
    } else{ 
      data_source_selected("up_pan")
      updateTabsetPanel(session=session, inputId="env_dat_tabpan", selected = "up_pan")}
  })

  # observeEvent(input$data_source_picker, { 



  # })



  output$sel_in_ls_text = renderText({ RV_text_fun(heading = "Selected",RVls_in =shiny::reactiveValuesToList(in_selected_RV))})
  output$glob_in_ls_text = renderText({ RV_text_fun(heading = "Global",RVls_in =shiny::reactiveValuesToList(global))})
  
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
  
    output$schm_plt_TCJ_CHP_bar_in <- bscui::renderBscui({bscui::bscui(TCJ_CHP_bar_in_xml) |>
        bscui::set_bscui_options(clip = TRUE,show_menu = FALSE, zoom_min = 1.0,zoom_max = 1.0)})

      # for selecting among schematic plots
    output$schem_start_loc_plt_ui <- renderUI({
      tst_val <- paste(in_selected_RV$LOC, in_selected_RV$BAR)
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
    })

  output$year_picker_ui <- renderUI({
  tagList(
    div(
      style = "display: inline-flex; align-items: left;margin-top: 10px;",
      div(
        style = "margin-top: 0px; margin-right: 5px;font-weight:bold",
        shiny::HTML("<h5> <b> Year: </b>  </h4>")
      ),
      div(
        style = "height=25px"
        ,
    shinyWidgets::pickerInput(
     "year_pickerOLD",
      # label="Pick year",
      label=NULL,
      choices = as.character(2011:2024),
      # selected = NULL,
      selected = init_water_year,
      choicesOpt = list(
        style = paste0("background-color:",WYT_cols[match(ann_HORbar_WYT_data$WYT,names(WYT_cols))],";")
      )
            )
      )
    )
    )
    })
  
  
 output$data_source_ui <- renderUI({
  # # tagList(
  # #   div(
  # #     style = "display: inline-flex; align-items: left;margin-top: 10px;",
  # #     div(
  # #       style = "margin-top: 0px; margin-right: 5px;font-weight:bold",
  # #       shiny::HTML("<h5> <b> Source: </b>  </h4>")
  # #       # shiny::HTML("<h5> <b> Use data from: </b>  </h4>")

  # #     ),
  # #     div(
  # #       style = "height=25px"
  # #       ,
  #   shinyWidgets::pickerInput(
  #    "data_source_picker",
  #     label=NULL,
  #     choices = c("Previous year","Uploaded file (.csv)")
  #           )
  #   #   )
  #   # )
  #   # )
   
   
   div(
    style="text-align: end; margin-left: 10px; margin-top: 10px;",
    div(
      style = "display: inline-flex; align-items: ;margin-top: 10px;",
      div(
        style = "margin-top: 0px; margin-right: 5px;font-weight:bold",
        shiny::HTML("<h5> <b> Source: </b>  </h4>")
        # shiny::HTML("<h5> <b> Use data from: </b>  </h4>")

      ),
      div(
        style = "height=15px"
        ,
    shinyWidgets::pickerInput(
     "data_source_picker",
      # label="Source",
      choices = c("Previous year","Uploaded file (.csv)")
            )
      )
    )
    # ,
      #   div(
      # style = "display: inline-flex; align-items: left;", #margin-top: 10px;
      # div(
      #   style = "margin-top: 0px; margin-right: 5px;font-weight:bold",
      #   shiny::HTML("<h5> <b> Year: </b>  </h4>")
      #   # shiny::HTML("<h5> <b> Use data from: </b>  </h4>")

      # )
    #   ,
    #   div(
    #     style = "height=15px"
    #     ,
    # # shinyWidgets::pickerInput(
    # #  "year_picker",
    # #   # label="Pick year",
    # #   label=NULL,
    # #   choices = as.character(2011:2024),
    # #   # selected = NULL,
    # #   selected = init_water_year,
    # #   choicesOpt = list(
    # #     style = paste0("background-color:",WYT_cols[match(ann_HORbar_WYT_data$WYT,names(WYT_cols))],";")
    # #   )
    # #         )
    #   )
    # )
    )
 })


  
  output$input_panel_UI <- renderUI({
    shinydashboardPlus::box(
      id = "input_box2",
      title = shiny::HTML("Inputs"),
      solidHeader = TRUE,
      # status = "warning",
      collapsible = T,
      collapsed = FALSE,
      width = 12,
      tags$style(HTML("
      .box {
        border-top: 1px solid #ddd !important;
        border-left: 1px solid #ddd;
        border-right: 1px solid #ddd;
        border-bottom: 1px solid #ddd;
      }
      .box-header {
        border-bottom: 2px solid #ddd !important;
      }
        
    ")),
      column(
  width = 5,
  shiny::HTML("<h4> <b> Overview: </b>  </h4>"),
          div(
          # style="",
          p("This tool provides predictions of survival and route usage for hypothetical releases of juvenile Steelhead 
          based on conditions in the evironment and individual size and location. Environmental conditions are represented using 
          daily summaries of field measurements obtained from monitoring stations spread throughout the region. The conditions on
          the day that fish arrive at key junction serve as inputs for statistical sub-models that are used to generate route usage
          and survival probability predictions.")
          
          )
          ,
          hr(),
          shiny::HTML("<h4> <b> Instructions: </b>  </h4>"),
          tags$ol(type="I",
           div(
                style = "display: inline-flex; align-items: left;",#margin-top: 10px;",
            tags$li(
              
            tags$u(
                style="font-size: 16px;","Select a starting location for a hypothetical release")
                # ,
                #  p(style="font-size: 14px;","Indicate whether hydrologic data from previous years (2011-2024) or user-provided data should be used")
                        
                        ,
            tags$ul(
            style="list-style-type: none;",
           
            tags$li(
              # shiny::uiOutput("start_loc_ui")
                              style = "display: inline-flex; align-items: left;margin-left: 20px;  10px;margin-right: 20px;",
                              div(
                                style = "align-content: center;",
    

                        shiny::uiOutput("start_loc_ui")
                      )
                        ,
                    shiny::uiOutput("schem_start_loc_plt_ui")
          ))
              ),
              )
              ,
          # shiny::splitLayout(
              div(
                style = "display: inline-flex; align-items: left;margin-top: 10px;",
            tags$li(
              tags$u(
                style="font-size: 16px;","Provide source of daily environmental data")
              ,
            p(style="font-size: 14px;","Indicate whether hydrologic data from previous years (2011-2024) or user-provided data should be used"),

               )
               ,
               shiny::uiOutput("data_source_ui") # both data source and year selection UI


              )
                             ,
              shiny::uiOutput("time_of_year_ui") 
              

            )
              

            ,
          br()
          ,
  column(width=6,
    p("flength_temp")
    )
      ,
   column(width=6,
      shiny::uiOutput("flength_sel_ui"))
  # p("Estimate surival from which junction to downstream to Chipp's Island (CHP)"),
  

),
      # 
      # column(width=12,
      
column(
  width=7,
  uiOutput("env_panel_ui")
  #   ,
  # shinydashboardPlus::box(
  #     id = "input_box3",
  #     title = shiny::HTML("Individual Fish Attributes"),
  #     solidHeader = TRUE,
  #     status = "primary",
  #     collapsible = T,
  #     collapsed = TRUE,
  #     width = 12,
  #     h3("temp")
  #   # shiny::uiOutput("flength_sel_ui") 
  #   )
        )
    ,
    footer=div(
      actionButton("inputs_done", "Done"),
      style="display:inline-block; float:right")
  )
  })
  
output$time_of_year_ui <- renderUI({
  tagList(
       div(
                style = "display: inline-flex; align-items: left;margin-top: 10px;margin-left: 20px;",
  div(
                style = "margin-right: 20px;",
  # h5(strong("Start and End Dates")),
  # h5(strong("Time of Year")),
      div(
      style = "display: inline-flex; align-items: left;", #margin-top: 10px;
      div(
        style = "margin-top: 0px; margin-right: 5px;font-weight:bold",
        shiny::HTML("<h5> <b> Year: </b>  </h4>")
        # shiny::HTML("<h5> <b> Use data from: </b>  </h4>")

      ),
      shinyWidgets::pickerInput(
     "year_picker",
      # label="Pick year",
      label=NULL,
      choices = as.character(2011:2024),
      # selected = NULL,
      selected = init_water_year,
      choicesOpt = list(
        style = paste0("background-color:",WYT_cols[match(ann_HORbar_WYT_data$WYT,names(WYT_cols))],";")
      )
            )
          )
            
,

    p("Provide a date or range of dates representing arrival at the selected junction by entering date(s) or adjusting Day of Year slider")
  ,
  shiny::uiOutput("start_date_entry_ui")
  # ,
  # hr()
)
  ,


  div(
                style = "display: left;margin-top: 10px;",
                  shiny::uiOutput("DOY_slider_ui"),
  plotOutput("doy_ref_strt_loc", height = "100px"),
    shiny::HTML("<p style ='font-size:80%'> Histogram depicts day of year when acoustic-tagged juvenile Steelhead were detected at each location, median day of year is selected by default. </p>")
  )
  ))
})


  output$env_panel_ui <- renderUI({
    tagList(
      switch(
      data_source_selected(),
      "prev_pan"=
      shinydashboardPlus::box(
      title = shiny::HTML("Environmental and Operational Variables (Previous years)"),
      solidHeader = TRUE,
      width=12,
      status="primary",
      collapsible = F,
      # collapsible = T,

        shiny::tabsetPanel(
          id="env_dat_tabpan",
          shiny::tabPanel(
            value="prev_pan",
            title = "Past water year",
            collapsed = F,
            # column(
            #   width = 6,
              # uiOutput("year_picker_ui")
              # ,
              h5("Select a water year by clicking on a row")
              ,
              uiOutput("table_in_WY_UI")
            # ),

            # column(width=6,
            #  h5(strong("Time of Year")),

            #     p("Provide a date or range of dates representing arrival at the selected junction (HOR or TCJ) by entering date(s) or adjusting Day of Year slider")
            #   ,
            #   shiny::uiOutput("start_date_entry_ui"),
            #   hr(),
            #   shiny::uiOutput("DOY_slider_ui"),
            
            #   plotOutput("doy_ref_strt_loc", height = "100px"),
            #    shiny::HTML("<p style ='font-size:80%'> Histogram depicts day of year when acoustic-tagged juvenile Steelhead were detected at each location, median day of year is selected by default. </p>")
               
            # )
          ),
          # shiny::tabPanel(
          #         title = "Single Variable",
          #         collapsed = F,
          #         shinyWidgets::radioGroupButtons(
          #           'radio_metric_view',
          #           label = "Metric",
          #           choices = c(
          #             "log(VNS)" = "VNS",
          #             "T_msd" = "MSD",
          #             "T_clc" = "CLC",
          #             "CVP" = "CVP",
          #             "SWP" = "SWP"
          #           )
          #         ),
          #         plotOutput("doy_ovr_plt2")
          #       ),
          shiny::tabPanel(
            # value="_pan",
            title = "Lattice",
              # p("placeholder")
              plotOutput('doy_latt_ggpplt')
        )
        ,
        shiny::tabPanel(
            # value="_pan",
            title = "Customize Inputs",
              # p("placeholder")
              # plotOutput('doy_latt_ggpplt')
        )
          # shiny::tabPanel(
          #   value="cust_pan",
          #   title = "Custom",
          #   collapsed = F,
          #   h5("Customize input"),
          #   h6("put your selection here!")
          # ),
      )
    )
    ,
    "up_pan"=
      shinydashboardPlus::box(
      title = shiny::HTML("Environmental and Operational Variables (Upload)"),
      solidHeader = TRUE,
      # width=8,
      width=12,
      # title=h4(strong("Environmental and Operational Variables")),
      status="primary",
              shiny::tabsetPanel(
          id="up_dat_tabpan",
          shiny::tabPanel(
            value="up_pan",
            title = "Upload Data",
            wellPanel(
              div(
                role = "menu",
                div(
                  role = "menuitem",
                  # p("textAreaInput")
                  textAreaInput(
                    inputId = "manual_input",
                    label = "Manual Input",
                    placeholder = 'year,date,WYT,barrier,VNS,OMT,T_MSD,T_CLC,CVP,SWP\n2013,"2013-04-29","Out",4130,-623,18.2,20.6,816,2421',
                    rows = 3
                  )
                ),
                div(
                  role = "menuitem",
                  # p("fileInput")
                  fileInput(
                    inputId = "file1",
                    label = "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ))
                )
              ),
            ),
            circle = TRUE,
            status = "primary",
            # icon = icon("gear"),
            width = "250px"
            # ,
            # tooltip = shinyWidgets::tooltipOptions(
            #   title = "Change Data Inputs"
            # )
            ,
            # br(),
            shiny::fluidRow(
              column(
                width = 4,
                div(
                  role = "menuitem",
                  p("run_button")
                  # actionButton(
                  #   inputId = ns("run_button"),
                  #   label = "Load"
                  # )
                  # actionButton(inputId = "run_button", label = "Load")
                )
              ),
              column(
                width = 4,
                div(
                  role = "menuitem",
                  p("reset_button")
                  # actionButton(
                  #   inputId = ns("reset_button"),
                  #   label = "Reset"
                  # )
                )
              ),
              column(
                width = 4,
                p("downloadButton")
                # downloadButton(
                #   outputId = ns("download_sample"),
                #   label = "Sample CSV",
                #   icon = shiny::icon("download")
                # )
              )
            )
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
        )
      )

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

  # rendering annual data table
  output$table_in_WY <- DT::renderDataTable(
    DT::datatable(
      ann_HORbar_WYT_data_TAB(),
      selection = list(
        mode = 'single',
        extentions="KeyTable",
        target = "row",
        selected = which(ann_HORbar_WYT_data$Year == init_water_year)),
      # server=FALSE,
      colnames = c("Year", "Category", "HOR Barrier", "Model"),
      rownames = FALSE, 
 
      # server=F,
      options = list(
        keys=TRUE, #related to KeyTable extension
        info=FALSE,
        dom = "t",
        # dom = '<"<"bottom"ip>',
        stripeClasses = list(),
             pageLength=17,
        # lengthMenu = c(13),
        # lengthMenu = c(17),
        pagingType = "simple",
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '75%'});",
          "$(this.api().table().body()).css({'font-size': '75%'});",
          "$(this.api().table().caption()).css({'font-size': '75%'});",
          "$(this.api().table().footer()).css({'font-size': '75%'});",
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
        backgroundColor = DT::styleEqual(c("Yes", "No"), c("white", "#B3B3B3"))
      )
  )

  test_proxy <- DT::dataTableProxy("table_in_WY")

  # update rows
  observeEvent(input$year_picker, {
      in_selected_RV$past_water_year <- as.numeric(input$year_picker)
      # if(in_selected_RV$past_water_year!=input$table_in_WY_rows_selected){
        DT::selectRows(test_proxy, match(as.numeric(input$year_picker),ann_HORbar_WYT_data$Year))
      # }
    })


  observeEvent(
    input$table_in_WY_rows_selected,{

      # year_picker
      in_selected_RV$past_water_year <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"Year"]
      in_selected_RV$WYT <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"WYT"]
      in_selected_RV$BAR <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"barrier"]
      # initializing global day of year variables
      in_selected_RV$start_date <- as.Date(
        paste(in_selected_RV$start_day,
          in_selected_RV$past_water_year,
          sep = "-"),format = "%j-%Y")
      in_selected_RV$end_date <- as.Date(
        paste(in_selected_RV$end_day,
          in_selected_RV$past_water_year,
          sep = "-"),format = "%j-%Y")
  
    shinyWidgets::updatePickerInput(session=session,inputId="year_picker",selected=in_selected_RV$past_water_year)
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
    input$load_check == TRUE,
     {global$past_water_year <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"Year"]
      global$WYT <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected, "WYT"]
      global$BAR <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"barrier"]
      # initializing global day of year variables
      global$start_date <- as.Date(
        paste(global$start_day, global$past_water_year, sep = "-"),
        format = "%j-%Y")
      global$end_date <- as.Date(
        paste(global$end_day, global$past_water_year, sep = "-"),
        format = "%j-%Y")
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
      shinydashboard::menuItem(
        "Main",
        tabName = "main",
        icon = icon("book")
      ),
      shinydashboard::menuItem(
        "Inputs",
        tabName = "main_input",
        icon = icon("sliders"),
        startExpanded = T,
        selected = F
      ),
      # shinydashboard::menuItem(
      #   text = "Estimates",
      #   tabName = "estimates",
      #   icon = icon("chart-line"),
      #   startExpanded = T #,
      # ),
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
        asHTML_frag = TRUE))
      })

  # because this is within a renderUI, changing the input$tab value rewrites the sidebar content
  output$top_of_body_text <- renderUI({
    switch(tab_selected(),
    
          # "about"=shiny::tagList(fluidRow( h2("new_render_pg_about")  )),
          # "main"=shiny::tagList(fluidRow( h2("new_render_pg_main")  )),
          # "main_input"=shiny::tagList(fluidRow( h2("new_render_pg_main_input")  ))

          "about"=shiny::tagList(
            # fluidRow( 
            # h2("new_render_pg_about"),
            #shiny::HTML(
            # wellPanel(
           shiny::HTML( md_txt_extract(
                md_addr = "inst/app/www/mds/page.md",
                # header_ref = paste0("# ", tab_selected()),
                header_ref = paste0("# ", "about"),
                asHTML_frag = TRUE
            )
          )
        #  )
        )
          ,
          "main"=shiny::tagList(fluidRow( 
            h2("new_render_pg_main")  

          ))
          ,
          "main_input"=shiny::tagList(
            uiOutput("input_page_UI")
            ))

    # )
  # )
      })

  output$start_date_entry_ui <- renderUI({
   tagList(
      shinyWidgets::airDatepickerInput(
      inputId = "Id111",
      label = "Start and End Dates :", 
      # value = c(as.Date("2011-01-01"),as.Date("2024-12-31")),
      value=c(as.Date(
        paste(in_selected_RV$past_water_year, in_selected_RV$start_day),
        "%Y %j"),
        as.Date(
        paste(in_selected_RV$past_water_year, in_selected_RV$end_day),
        "%Y %j"))
        ,
      minDate = as.Date("2011-01-01"),
      maxDate = as.Date("2024-12-31"),
      range=TRUE,
      disabledDaysOfWeek=TRUE,
     width = "80%")
    #  ,
    # daterangepicker::daterangepicker(
    #   inputId = "daterange_sel",
    #   # label = "Date:",
    #   label = NULL,
    #   icon=icon("calendar"),
    #   start = as.Date(
    #     paste(in_selected_RV$past_water_year, in_selected_RV$start_day),
    #     "%Y %j"),
    #   end = as.Date(
    #     paste(in_selected_RV$past_water_year, in_selected_RV$end_day),
    #     "%Y %j"),
    #   min = "2011-01-01",
    #   max = "2024-12-31",
    #   language = "en",
    #   style = "width:50%; border-radius:4px",
    #   options = list(
    #     "minYear" = in_selected_RV$past_water_year,
    #     "maxYear" = in_selected_RV$past_water_year,
    #     "cancelIsClear" = TRUE,
    #     "autoApply" = TRUE))
      )
  })

  output$DOY_slider_ui <- renderUI({
    tagList(
      sliderInput(
        "DOYslider_rng",
        label = NULL,
        min = 0,
        max = 250,
        value = c(in_selected_RV$start_day, in_selected_RV$end_day),
        width = '99%'))
  })

  observeEvent(input$DOYslider_rng, {
      in_selected_RV$start_day <- input$DOYslider_rng[1]
      in_selected_RV$end_day <- input$DOYslider_rng[2]

      if (!is.na(in_selected_RV$past_water_year)) {
        in_selected_RV$start_date <- as.Date(
          paste(in_selected_RV$start_day, in_selected_RV$past_water_year, sep = "-"),
          format = "%j-%Y"
        )
        in_selected_RV$end_date <- as.Date(
          paste(in_selected_RV$end_day, in_selected_RV$past_water_year, sep = "-"),
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
    class = "box-title",
    div(
      class = "thumbnail-container",
      style = "display: inline-flex; align-items: left;",
      div(
        class = "thumbnail-section",
        style = "margin-right: 10px;",
        shiny::HTML("<h4> <b> Fork Length: </b>  </h4>")
      ),
      div(
        class = "thumbnail-section",
        style = "margin-right: 5px;",
        shiny::uiOutput("flength_num_in")
      ),
      div(
        class = "thumbnail-section",
        shiny::HTML("<h4 style ='font-size:10px;'> (100 - 400 mm) </h4>")
      )
    )
  ),
    #   column(
    #     width = 3,
    #     HTML("<p style ='font-size:80%'>Fork length of juvenile Steelhead used in acoustic telemetry studies (2011-2016). </p>")
    #   ),
    #   column(width = 9, 
    #     shiny::uiOutput("flength_dash_ui"))
    # )
        shiny::uiOutput("flength_dash_ui")
        ,
        HTML("<p style ='font-size:80%'>Fork length of juvenile Steelhead used in acoustic telemetry studies (2011-2016). </p>")
)
# )
  })
  
  output$flength_dash_ui <- renderUI({
    shiny::tagList(
      plotOutput("flength_ref_hist",height = "100px")
      )
  })
  
  # Reference forklength histogram
  output$flength_ref_hist <- renderPlot({
    input$flength_num_in_dash
    ggplot_flength_ref_hist(flength_in=in_selected_RV$flength,flength_hst_xlims_in=c(100,400))
    }
    ,
  alt = "histogram depicting an approximately normal distribution of fork lengths centered at 244 millimeters with most tags between 160 and 330 millimeters"
  )
  

  output$doy_ref_strt_loc <- renderPlot({
    # triggers re-rendering
    # input$start_loc_in
    # input$radio_doy_type
    ggplt_doy_ref_hist(DOY_arvDF_l_in=DOY_arvDF_l,
                       LOC_in=in_selected_RV$LOC,start_day_in=in_selected_RV$start_day,
                       end_day_in=in_selected_RV$end_day)
  })
  
   output$input_page_UI <- renderUI({
    tagList(
     uiOutput("input_panel_UI")
    #  ,
    #  uiOutput("input_panel_UI_2")
    ,
    uiOutput("input_panel_UI_3")
    )
   })

   
    # observe flength numeric input with debounce
    flength_mod <- reactive(input$flength_num_in_dash)
    flength_d <- debounce(flength_mod,500) # prevents endless looping
     observeEvent(flength_d(),{
      in_selected_RV$flength <-flength_d()
    })

  output$flength_num_in <- renderUI({
        numericInput(
        "flength_num_in_dash",
        label = NULL,
        step = 10,
        value = in_selected_RV$flength,#init_flength
        width = "80px",
        min = 100,
        max = 400)
    })

  # output$input_panel_UI_2 <- renderUI({
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

      output$input_panel_UI_3 <- renderUI({
    shinydashboardPlus::box(
      id = "input_box4",
      title = shiny::HTML("Estimates"),
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      collapsed = TRUE,
      width = 12,
      h2("stuff here")
    )
    })


output$start_loc_ui <- renderUI({
    div(
      style = "display: inline-flex; align-items: left;margin-top: 10px;",
    # div(
    #   style = "display: inline-flex; align-items: left;margin-top: 10px;",
      # div(
      #   style = "margin-top: 0px; margin-right: 5px;font-weight:bold",
      #   shiny::HTML("<h5> <b> Starting Location: </b>  </h4>")
      # ),
      div(
        style = "height=25px; align-self:center"
        ,
      shinyWidgets::pickerInput(
          # label="Junction:",
        label="Starting Location:",
        'start_loc_in',
        choices = loc_opt
        # ,
        #     options = list(style = "btn-med")
      )
      )
      # ,
      # shiny::uiOutput("schem_start_loc_plt_ui")
    # )
    )
    })

   observeEvent(input$inputs_done, {
     shinydashboardPlus::updateBox("input_box2",action = "toggle")
     shinydashboardPlus::updateBox("input_box3",action = "toggle")

    #  input$env_dat_tabpan=="prev_pan"
     print(input$env_dat_tabpan)
     
  })

  # button at the top
  observeEvent(input$inputs_done2, {
    if(input$input_box3$collapsed){
      #  shinydashboardPlus::updateBox("input_box3",action = "update",options=list(status="primary"))
      shinydashboardPlus::updateBox("input_box3",action = "toggle")#options=list(status="danger"))
    }
    if(!input$input_box2$collapsed){
      shinydashboardPlus::updateBox("input_box2",action = "toggle")#options=list(status="danger"))
    }



  })

}
