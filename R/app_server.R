#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # autoselect specify starting tab

  tab_selected <- reactiveVal("about")
  shinydashboard::updateTabItems("tabs", session = session, selected = "about")

  # {DEBUGGING}
  observeEvent(input$tabs, {  
    print(input$tabs)  
    tab_selected(input$tabs)
  })

  output$sel_in_ls_text = renderText({ RV_text_fun(heading = "Selected",RVls_in =shiny::reactiveValuesToList(in_selected_RV))})
  output$glob_in_ls_text = renderText({ RV_text_fun(heading = "Global",RVls_in =shiny::reactiveValuesToList(global))})
  
  # proxy <- DT::dataTableProxy("table_in_WY")

  # rendering annual data table
  output$table_in_WY <- DT::renderDataTable(
    DT::datatable(
      ann_HORbar_WYT_data_TAB(),
      selection = list(
        mode = 'single',
        target = "row",
        selected = which(ann_HORbar_WYT_data$Year == init_water_year)),
      colnames = c("Year", "Category", "HOR Barrier", "Model"),
      rownames = FALSE,
      options = list(
        info=FALSE,
        dom = '<"<"bottom"ip>',
        stripeClasses = list(),
        lengthMenu = c(13),
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
    # )
  )

  observeEvent(
    input$table_in_WY_rows_selected,{
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
    }
  )

  # data table output UI
  output$table_in_WY_UI <- renderUI({   DT::dataTableOutput("table_in_WY") })

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
    daterangepicker::daterangepicker(
      inputId = "daterange_sel",
      label = "Date:",
      # label = NULL,
      start = as.Date(
        paste(global$past_water_year, global$start_day),
        "%Y %j"),
      end = as.Date(
        paste(global$past_water_year, global$end_day),
        "%Y %j"),
      min = "2011-01-01",max = "2024-12-31",
      language = "en",
      style = "width:50%; border-radius:4px",
      options = list(
        "minYear" = global$past_water_year,
        "maxYear" = global$past_water_year,
        "cancelIsClear" = TRUE,
        "autoApply" = TRUE))
  })

  output$DOY_slider_ui <- renderUI({
    tagList(
      sliderInput(
        "DOYslider_rng",
        label = NULL,
        min = 0,
        max = 250,
        value = c(global$start_day, global$end_day),
        width = '99%'))
  })

   output$input_page_UI <- renderUI({
    tagList(
     uiOutput("input_panel_UI"),
     uiOutput("input_panel_UI_2")
    )
   })


  output$input_panel_UI_2 <- renderUI({
    shinydashboardPlus::box(
      id = "input_box3",
      title = shiny::HTML("More Inputs"),
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      collapsed = TRUE,
      width = 12,
      h2("stuff here")
    )
    })


  output$input_panel_UI <- renderUI({
    shinydashboardPlus::box(
      id = "input_box2",
      title = shiny::HTML("Inputs"),
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      collapsed = FALSE,
      width = 12,
      column(width=12,
        h4(strong("Environmental and Operational Variables")),
        p(
          "Daily values for environmental and operational predictors are supplied to the tool by either:"
        ),
        shiny::HTML(
          "<ol>  
            <li> Looking up observations from a past water year, </li> 
            <li> Customize values from a past water year, or </li> 
            <li> Uploading a data set containing all the required values </li> 
          </ol>"
        ),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Previous water year",
            collapsed = F,
            column(
              width = 6,
              h5("Select a water year by clicking on a row"),
              uiOutput("table_in_WY_UI")
            ),
            column(
              width = 6,
              br(),
              shinydashboardPlus::box(
                id = "date_pick_box",
                width = 12,
                headerBorder = F,
                h5(strong("Time of Year")),
                p(
                  "Provide a date or range of dates representing arrival at the selected junction (HOR or TCJ) by entering date(s) or adjusting Day of Year slider"
                )
              ),
              shiny::uiOutput("start_date_entry_ui"),
              hr(),
              shiny::uiOutput("DOY_slider_ui")
            )
          ),
          shiny::tabPanel(
            title = "Custom",
            collapsed = F,
            h5("Customize input"),
            h6("put your selection here!")
          ),
          shiny::tabPanel(
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
            ),
            shiny::tags$script(shiny::HTML(
              "
          document.addEventListener('DOMContentLoaded', function() {
            const dropdown = document.querySelector('#dropdown-menu-drop' + Object.keys(window.shinyWidgets.dropdownStates)[0]);
            if (dropdown) {
              dropdown.removeAttribute('role');
              dropdown.querySelectorAll('[role=\"menuitem\"]').forEach(el => el.removeAttribute('role'));
            }
          });
              "
            ))
        )
      )
    )
    ,
    footer=div(
      actionButton("inputs_done", "Done"),
      style="display:inline-block; float:right")
  )
  })

   observeEvent(input$inputs_done, {
     shinydashboardPlus::updateBox("input_box2",action = "toggle")
     shinydashboardPlus::updateBox("input_box3",action = "toggle")
     
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
