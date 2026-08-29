#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  output$dyn_sidebar_txt <- renderText({paste0(get_sidebar_txt(input$tabs))})

  # autoselect specify starting tab
  # session$onSessionEnded(stopApp)

  # at startup ----
  tab_selected <- reactiveVal("inputs")

  data_source_selected <- reactiveVal("prev_pan")

  ### Tab switching behavior  ----
  observeEvent(input$tabs, {
    tab_selected(input$tabs) # changes tab
    print(input$tabs)
    # scrollIntoView options
    if(input$tabs=="check"){shinyjs::runjs("window.scrollTo({top: document.getElementById('details_indiv').getBoundingClientRect().top + window.scrollY - 75, behavior: 'smooth'});")}
    if(input$tabs=="inputs"){shinyjs::runjs("window.scrollTo({top: document.getElementById('inputTop').getBoundingClientRect().top + window.scrollY - 75, behavior: 'smooth'});")}
    if(input$tabs=="overall_surv"){shinyjs::runjs("window.scrollTo({top: document.getElementById('overall_surv_panel').getBoundingClientRect().top + window.scrollY - 75, behavior: 'smooth'});")}
    if(input$tabs=="reach_surv"){shinyjs::runjs("window.scrollTo({top: document.getElementById('reach_surv_panel').getBoundingClientRect().top + window.scrollY - 75, behavior: 'smooth'});")}
    if(input$tabs=="route_spec_surv"){shinyjs::runjs("window.scrollTo({top: document.getElementById('route_spec_surv_panel').getBoundingClientRect().top + window.scrollY - 75, behavior: 'smooth'});")}
    if(input$tabs=="route_usage"){shinyjs::runjs("window.scrollTo({top: document.getElementById('route_usage_panel').getBoundingClientRect().top + window.scrollY - 75, behavior: 'smooth'});")}
    # if(input$tabs=="more_info"){shinyjs::runjs("window.scrollTo({top: document.getElementById('more_info').getBoundingClientRect().top + window.scrollY - 75, behavior: 'smooth'});")}
  })

  output$sel_in_ls_text = renderText({
    get_rv_ls_txt(
      heading = "Selected",
      RVls_in = shiny::reactiveValuesToList(in_selected_RV))
  })

  # major dynamic ui elements ----

  output$main_page_content_dynui <- renderUI({
    draw_main_page_content_dynui(
      # input_tab_in= input$tabs,init_data_source_in=init_data_source_in
      input_tab_in= input$tabs,init_data_source_in="Previous year"
      )
  })

  # Auto-switching to top option when sidebar menu items are expanded
  
  observeEvent(input$sidebarItemExpanded, {
    print(paste("expand",input$sidebarItemExpanded))
    if(input$sidebarItemExpanded == "inputs_title"){
    shinydashboard::updateTabItems(
      inputId = "tabs",
      session = session,
      selected = "inputs")
    }
  })
  
  observeEvent(input$sidebarItemExpanded, {
    print(paste("expand",input$sidebarItemExpanded))
    if(input$sidebarItemExpanded == "estimates_title"){
    shinydashboard::updateTabItems(
      inputId = "tabs",
      session = session,
      selected = "overall_surv")
    }
  })

  ### tab and navigation  ----
  observeEvent(input$data_source_picker, {
    data_source_selected(input$data_source_picker)
  })

  observeEvent(input$goto_inputs_butt, {
    tab_selected("inputs")
    shinydashboard::updateTabItems(
      inputId = "tabs",
      session = session,
      selected = "inputs"
    )
  })
  observeEvent(input$goto_met_ref_butt, {
    tab_selected("met_ref")
    shinydashboard::updateTabItems(
      inputId = "tabs",
      session = session,
      selected = "met_ref"
    )
  })

  observeEvent(input$top_met_ref_butt, {
    tab_selected("met_ref")
    shinydashboard::updateTabItems(
      inputId = "tabs",
      session = session,
      selected = "met_ref"
    )
  })
  observeEvent(input$top_about_butt, {
    tab_selected("about")
    shinydashboard::updateTabItems(
      inputId = "tabs",
      session = session,
      selected = "about"
    )
  })

  ### reactive value definitions  ----

  # print global (i.e., locked-in values)
  output$glob_in_ls_text = renderText({
    get_rv_ls_txt(
      heading = "none",
      RVls_in = shiny::reactiveValuesToList(in_global_RV)
    )
  })

  observeEvent(input$start_loc_in, {
    in_selected_RV$LOC <- input$start_loc_in
  })

  # Starting location name to be inserted into UI downstream
  output$start_loc_heading = renderText({
    paste(in_selected_RV$start_loc_in)
  })


  ### change major display change based on reactive values ----
  # for selecting among schematic plots
  output$data_source_selected_txt <- renderText({
    data_source_selected()
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

  output$time_of_year_entry_ui <- renderUI({
    tagList(
      div(
        style = "display: left;margin-top: 10px;width: 600px;",
        sliderInput(
          "DOYslider_rng",
          label = NULL,
          min = 1,
          max = 250,
          # value = c(100,200),
          # value = isolate(c(in_selected_RV$start_day, in_selected_RV$end_day)),
          value = c(in_selected_RV$start_day, in_selected_RV$end_day),
          width = '99%'
        ),
        plotOutput("doy_ref_strt_loc", height = "100px"),
        div(
          # style = "display: inline-flex;    align-items: baseline;",
          style = "display: flex; justify-content: space-between;",

          div(
            style = "display: inline-flex;    align-items: baseline;",
            HTML(paste(
              "<p> Day of arrival at <strong>",
              (input$start_loc_in),
              # (in_selected_RV$LOC),
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
          ),
          div(
            actionButton("doy_slider_set_butt", "Done")
          )
        )
      )
    )
  })


  output$doy_var_ggpplt <- renderPlot({
    ggplot_doy_env_plt(
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

  output$doy_ins_ggpplt2_dup1 <- renderPlot({
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

  output$doy_ins_ggpplt2_dup2 <- renderPlot({
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

  output$doy_ins_ggpplt2_dup3 <- renderPlot({
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



  proxy <- DT::dataTableProxy("table_in_WY")

  output$table_in_WY <- DT::renderDataTable(
    draw_inputs_ann_summ_dt(input_year = in_selected_RV$past_water_year
  ))

  
  observeEvent(input$year_picker, {
    in_selected_RV$BAR <- ann_HORbar_WYT_data[
      which(ann_HORbar_WYT_data_TAB()$Year == input$year_picker),
      "barrier"
    ]
  })

  # passing barrier status (BAR to conditional panel
  output$BarStatus <- reactive({  in_selected_RV$BAR})
  outputOptions(output, "BarStatus", suspendWhenHidden = FALSE)

  

  # update the selected row in the table
  observeEvent(input$year_picker, {
    in_selected_RV$past_water_year <- input$year_picker
  })


  # data table output UI
  # output$table_in_WY_UI <- renderUI({
  #   DT::dataTableOutput("table_in_WY")
  # })

  # update values when water year is selected
  observeEvent(
    input$load_butt,
    {
      in_global_RV$past_water_year <- in_selected_RV$past_water_year
      in_global_RV$WYT <- in_selected_RV$WYT
      in_global_RV$BAR <- in_selected_RV$BAR
      in_global_RV$LOC <- in_selected_RV$LOC
      in_global_RV$start_day <- in_selected_RV$start_day
      in_global_RV$end_day <- in_selected_RV$end_day
      in_global_RV$start_date <- in_selected_RV$start_date
      in_global_RV$end_date <- in_selected_RV$end_date
      in_global_RV$flength <- in_selected_RV$flength


    

    }
  )



  # if remaining static move to ui.R script
  output$met_ref_page_ui <- renderUI({
    draw_met_ref_page_ui()
  })

  # user inputs ----

  ### date range entry  ----

  #
  output$start_date_entry_sep_ui <- renderUI({
    # tagList()
    div(
      style = "display: flex; gap:20px;",
      shinyWidgets::airDatepickerInput(
        inputId = "date_start_sep",
        label = "Start Date :",
        addon = "none",
        width = "100px",
        value = c(
          as.Date(
            isolate(paste(
              in_selected_RV$past_water_year,
              in_selected_RV$start_day
            )),
            # paste(in_selected_RV$past_water_year, 5),
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
            isolate(paste(
              in_selected_RV$past_water_year,
              in_selected_RV$end_day
            )),
            # paste(in_selected_RV$past_water_year, 55),
            "%Y %j"
          )
        )
      )
    )
  })

  # observeEvent(input$date_start_sep, {
  #   in_selected_RV$start_day <- as.numeric(format(input$date_start_sep, "%j"))
  # })

  # observeEvent(input$date_end_sep, {
  #   in_selected_RV$end_day <- as.numeric(format(input$date_end_sep, "%j"))
  # })

  observeEvent(input$DOYslider_rng, {
    if (in_selected_RV$start_day != input$DOYslider_rng[1]) {
      # in_selected_RV$start_day <- input$DOYslider_rng[1]
      shinyWidgets::updateAirDateInput(
        inputId = "date_start_sep",
        value = as.Date(
          paste(in_selected_RV$past_water_year, input$DOYslider_rng[1]),
          "%Y %j"
        )
      )
    }

    if (in_selected_RV$end_day != input$DOYslider_rng[2]) {
      # in_selected_RV$end_day <- input$DOYslider_rng[2]
      shinyWidgets::updateAirDateInput(
        inputId = "date_end_sep",
        value = as.Date(
          paste(in_selected_RV$past_water_year, input$DOYslider_rng[2]),
          "%Y %j"
        )
      )
    }

    # in_selected_RV$start_day <- input$DOYslider_rng[1]
    # in_selected_RV$end_day <- input$DOYslider_rng[2]

    # if (!is.na(in_selected_RV$past_water_year)) {
    #   in_selected_RV$start_date <- as.Date(
    #     paste(
    #       in_selected_RV$start_day,
    #       in_selected_RV$past_water_year,
    #       sep = "-"
    #     ),
    #     format = "%j-%Y"
    #   )
    #   in_selected_RV$end_date <- as.Date(
    #     paste(
    #       in_selected_RV$end_day,
    #       in_selected_RV$past_water_year,
    #       sep = "-"
    #     ),
    #     format = "%j-%Y"
    #   )
    # }
  })

  # When it closes
  # observeEvent(input$doy_slider_dropdown_dropmenu, {
  #     if(!input$doy_slider_dropdown_dropmenu){
  #       print("update!")
  #     }
  #     # ignoreInit = TRUE,
  #     # print(input$doy_slider_dropdown_dropmenu)
  #   })

  # close on click
  observeEvent(input$doy_slider_set_butt, {
    shinyWidgets::hideDropMenu("doy_slider_dropdown_dropmenu")

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

    if (in_selected_RV$start_day != input$DOYslider_rng[1]) {
      in_selected_RV$start_day <- input$DOYslider_rng[1]
      # shinyWidgets::updateAirDateInput(inputId="date_start_sep",
      #     value=as.Date(paste(in_selected_RV$past_water_year,input$DOYslider_rng[1]),"%Y %j"))
    }

    if (in_selected_RV$end_day != input$DOYslider_rng[2]) {
      in_selected_RV$end_day <- input$DOYslider_rng[2]
      #  shinyWidgets::updateAirDateInput(inputId="date_end_sep",
      #  value=as.Date(paste(in_selected_RV$past_water_year,input$DOYslider_rng[2]),"%Y %j"))
    }
  })

  # Fork length reference selection
  output$flength_sel_ui <- renderUI({
    shiny::tagList(
      fluidRow(
      # div(
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
                HTML(paste(
                  "<p> Fork length of juvenile Steelhead from Six-Year Study </p> "
                ))
              ),
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
      ggplot_input_flength_ref_hist(
        flength_in = in_selected_RV$flength,
        flength_hst_xlims_in = c(100, 400)
      )
    },
    alt = "histogram depicting an approximately normal distribution of fork lengths centered at 244 millimeters with most tags between 160 and 330 millimeters"
  )

  output$doy_ref_strt_loc <- renderPlot({
    get_inputs_day_input_ref_hist_ggplot(
      DOY_arvDF_l_in = DOY_arvDF_l,
      LOC_in = in_selected_RV$LOC,
      start_day_in = input$DOYslider_rng[1],
      end_day_in = input$DOYslider_rng[2]
    )
  })

  # message for invalid flength entries
  output$flength_inval_txt <- renderText({
    validate(
      need(
        input$flength_num_in_dash <= 400 & input$flength_num_in_dash >= 100,
        "Only values between 100 and 400 mm are permitted"
      )
    )
    NULL
  })

  # observe flength numeric input with debounce
  flength_mod <- reactive({
    validate(
      need(
        input$flength_num_in_dash < 400,
        "Only values between 100 and 400 mm are permitted"
      ),
      need(
        input$flength_num_in_dash > 100,
        "Only values between 100 and 400 mm are permitted"
      )
    )
    input$flength_num_in_dash
  }) |>
    debounce(1000)

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

  observeEvent(input$generate_ests_butt, {
    # if (!input$input_box2$collapsed) {
    #   shinydashboardPlus::updateBox("input_box2", action = "toggle")
    # }

    # if (input$est_box_ui$collapsed) {
    #   shinydashboardPlus::updateBox("est_box_ui", action = "toggle")
    # }

    # if(input$tabs=="inputs"){
    #   shinyjs::runjs("
    #   document.getElementById('inputTop').scrollIntoView({ behavior: 'smooth', block: 'start' });
    #  ")

    shinydashboard::updateTabItems(
      inputId = "tabs",
      session = session,
      selected = "estimates"
    )


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
        # displayStart = 2, supposed to be starting page index
        initComplete = DT::JS(
          "
        function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '75%'});",
          "$(this.api().table().body()).css({'font-size': '75%'});",
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
  

  # estimates ----

  ### checking inputs  ----

  ### calculate estimates  ----

  OUT_tmp <- reactiveValues()

  # previous year data is pre-loaded
  # OUT_tmp$HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
  OUT_tmp$HOR_TCJ_pred_tab <- pred_tab_ls[["HOR_TCJviaSJL"]]
  OUT_tmp$TCJ_pred_tab <- pred_tab_ls[["TCJ"]]
  OUT_tmp$HOR_pred_tab <- pred_tab_ls[["HOR"]]
  OUT_tmp$TCJ_CHPviaTRN_pred_tab <- pred_tab_ls[["TCJ_CHPviaTRN"]]
  
  OUT_tmp$HOR_TCJviaSJL_pred_tab <- pred_tab_ls[["HOR_TCJviaSJL"]]
  OUT_tmp$HOR_CHPviaSJL_pred_tab <- pred_tab_ls[["HOR_CHPviaSJL"]]
  OUT_tmp$HOR_CHPviaORE_pred_tab <- pred_tab_ls[["HOR_CHPviaORE"]]
  OUT_tmp$TCJ_CHPviaMAC_pred_tab <- pred_tab_ls[["TCJ_CHPviaMAC"]]

  # long-form combined predictors
  OUT_tmp$pred_pDF_comb <- pred_pDF_comb

  # observeEvent(input$flength_in, {  
  # observeEvent(flength_mod()!=244, {
  # observeEvent(input$load_butt, {
  observeEvent(input$generate_ests_butt, {
    pred_tab_ls_TEMP <- get_comp_model_preds(
      DOY_in = in_selected_RV$start_day:in_selected_RV$end_day,
      # DOY_in = c(
      #   in_selected_RV$start_day,
      #   in_selected_RV$end_day),
        years_in = in_selected_RV$past_water_year,
        flength_in = in_selected_RV$flength)

    OUT_tmp$pred_pDF_comb <-  get_overall_surv_preds(
      pred_tab_ls_in = pred_tab_ls_TEMP,
      predict_int = F,conf_level = 0.95)[["pred_pDF_comb"]]


    OUT_tmp$HOR_TCJ_pred_tab <- pred_tab_ls_TEMP[["HOR_TCJviaSJL"]]
    OUT_tmp$TCJ_pred_tab <- pred_tab_ls_TEMP[["TCJ"]]
    OUT_tmp$HOR_pred_tab <- pred_tab_ls_TEMP[["HOR"]]
    OUT_tmp$TCJ_CHPviaTRN_pred_tab <- pred_tab_ls_TEMP[["TCJ_CHPviaTRN"]]
    
    OUT_tmp$HOR_TCJviaSJL_pred_tab <- pred_tab_ls_TEMP[["HOR_TCJviaSJL"]]
    OUT_tmp$HOR_CHPviaSJL_pred_tab <- pred_tab_ls_TEMP[["HOR_CHPviaSJL"]]
    OUT_tmp$HOR_CHPviaORE_pred_tab <- pred_tab_ls_TEMP[["HOR_CHPviaORE"]]
    OUT_tmp$TCJ_CHPviaMAC_pred_tab <- pred_tab_ls_TEMP[["TCJ_CHPviaMAC"]]


    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # uses the full previous years data data set for everything except flength
    # setting to 'in_selected_RV' vs 'in_global_RV'
    # # in_selected_RV
    # OUT_tmp$HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(
    #   sel_rows_tmp1 = CVhelp_dat_w, # uses the full previous years data data set for everything except flength
    #   HOR_TCJ_mod_ls = glmmTMB_mod_ls[["HOR_TCJ"]],
    #   flength_in = in_selected_RV$flength
    # )

    # OUT_tmp$TCJ_pred_tab <- TCJ_mod_wrap(
    #   sel_rows_tmp1 = CVhelp_dat_w, # uses the full previous years data data set for everything except flength
    #   HOR_TCJ_mod_ls = glmmTMB_mod_ls[["TCJ"]],
    #   flength_in = in_selected_RV$flength
    # )

    # OUT_tmp$TCJ_pred_tab <- HOR_mod_wrap(
    #   sel_rows_tmp1 = CVhelp_dat_w, # uses the full previous years data data set for everything except flength
    #   HOR_TCJ_mod_ls = glmmTMB_mod_ls[["HOR"]],
    #   flength_in = in_selected_RV$flength
    # )


  })



  # new revision
#  output$HOR_TCJ_pred_plot <- renderPlot({
#     HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"
#     ggplot_doy_pred_plt(
#       HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
#       doy_rng_in = c(
#         in_selected_RV$start_day,
#         in_selected_RV$end_day
#       ),
#       pst_year_in = in_selected_RV$past_water_year
#     )
#   })

  output$HOR_TCJ_pred_ggpplt_s_tot <- renderPlot({

  # pred_pDF_comb_inarg <- pred_pDF_comb_TEMP

    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb
    ggplot_doy_pred_plt_single(
      data_in =  pred_pDF_comb_inarg,
      param_in="S_HOR_CHP",
      doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)
  
  })

  output$HOR_TCJviaSJL_pred_ggpplt_rs1 <- renderPlot({

    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb
    ggplot_doy_pred_plt_single(
      data_in =  pred_pDF_comb_inarg,
      param_in="HOR_TCJviaSJL",
      doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)
  # output$HOR_TCJ_pred_ggpplt_rs1 <- renderPlot({
    # HOR_TCJ_pred_tab <- OUT_tmp$HOR_TCJ_pred_tab
    # replacement
    # HOR_TCJ_pred_tab <- pred_tab_ls[["HOR_TCJviaSJL"]]
    # ggplot_doy_pred_plt(
    #   HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
    #   doy_rng_in = c(
    #     in_selected_RV$start_day,
    #     in_selected_RV$end_day
    #   ),
    #   pst_year_in = in_selected_RV$past_water_year
    # )
  })


  output$TCJ_CHP_all_pred_ggpplt_rs_2 <- renderPlot({
    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb
    ggplot_doy_pred_plt_single(
      data_in =  pred_pDF_comb_inarg,
      param_in="S_TCJ_CHP",
      doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)
  })


# HOR_TCJ_pred_ggpplt_dup1b
#   HOR_CHPviaSJL_pred_ggplt_rss_1
  output$HOR_CHPviaSJL_pred_ggplt_rss_1 <- renderPlot({
    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb
    ggplot_doy_pred_plt_single(
      data_in =  pred_pDF_comb_inarg,
      param_in="HOR_CHPviaSJL",
      doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)
  })
  
  output$HOR_CHPviaORE_pred_ggplt_rss_2 <- renderPlot({
    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb
    ggplot_doy_pred_plt_single(
      data_in =  pred_pDF_comb_inarg,
      param_in="HOR_CHPviaORE",
      doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)
  })
  

  output$TCJ_CHPviaMAC_pred_ggplt_rss_3 <- renderPlot({
    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb
    ggplot_doy_pred_plt_single(
      data_in =  pred_pDF_comb_inarg,
      param_in="TCJ_CHPviaMAC",
      doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)
  })
  
    output$TCJ_CHPviaTRN_pred_ggplt_rss_4 <- renderPlot({
    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb
    ggplot_doy_pred_plt_single(
      data_in =  pred_pDF_comb_inarg,
      param_in="TCJ_CHPviaTRN",
      doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)
  })


  
output$HOR_pred_ggpplt_ALT <- renderPlot({
    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb

    ggplot_doy_pred_plt_compliment(
      data_in =  pred_pDF_comb_inarg,
      param_in="HOR",
          doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)

  })


  output$TCJ_pred_ggpplt_ALT <- renderPlot({
    pred_pDF_comb_inarg <- OUT_tmp$pred_pDF_comb

    ggplot_doy_pred_plt_compliment(
      data_in =  pred_pDF_comb_inarg,
      param_in="TCJ",
          doy_rng_in = c(
          in_selected_RV$start_day,
          in_selected_RV$end_day),
      pst_year_in = in_selected_RV$past_water_year)

    # ggplot_doy_pred_plt_single(
    #   data_in =  pred_pDF_comb_inarg,
    #   param_in="TCJ",
    #   doy_rng_in = c(
    #       in_selected_RV$start_day,
    #       in_selected_RV$end_day),
    #   pst_year_in = in_selected_RV$past_water_year)
    # TCJ_pred_tab <-  pred_tab_ls[["TCJ"]]

    # TCJ_pred_tab <- OUT_tmp$TCJ_pred_tab 

    # ggplot_doy_rte_plt(
    #   HOR_TCJ_pred_tab_plt = TCJ_pred_tab,
    #   doy_rng_in = c(
    #     in_selected_RV$start_day,
    #     in_selected_RV$end_day
    #   ),
    #   pst_year_in = in_selected_RV$past_water_year
    # )
  })






  output$HOR_TCJ_pred_ggpplt_dup1 <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"

    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)

    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )
  })

  output$HOR_TCJ_pred_ggpplt_dup1a <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"

    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)

    HOR_TCJ_pred_tab$lo_pred <- log(
      (plogis(HOR_TCJ_pred_tab$lo_pred) * 0.45) /
        (1 - (plogis(HOR_TCJ_pred_tab$lo_pred) * 0.45))
    )
    HOR_TCJ_pred_tab$LCL <- log(
      (plogis(HOR_TCJ_pred_tab$LCL) * 0.45) /
        (1 - (plogis(HOR_TCJ_pred_tab$LCL) * 0.45))
    )
    HOR_TCJ_pred_tab$UCL <- log(
      (plogis(HOR_TCJ_pred_tab$UCL) * 0.45) /
        (1 - (plogis(HOR_TCJ_pred_tab$UCL) * 0.45))
    )

    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = 2013 + 9
      # pst_year_in = in_selected_RV$past_water_year
    )
  })



   # TCJ to CHP via TRN
  # HOR_TCJ_pred_ggpplt_dup1c
   output$TCJ_CHPviaTRN_ggpplt <- renderPlot({
     
    #  pred_tab_ls[["TCJ"]]
    TCJ_CHPviaTRN_pred_tab <- OUT_tmp$"TCJ_CHPviaTRN_pred_tab"
    TCJ_CHPviaTRN_pred_tab$LCL= TCJ_CHPviaTRN_pred_tab$lo_pred-1.96* TCJ_CHPviaTRN_pred_tab$lo_SEadj
    TCJ_CHPviaTRN_pred_tab$UCL= TCJ_CHPviaTRN_pred_tab$lo_pred+1.96* TCJ_CHPviaTRN_pred_tab$lo_SEadj
     
    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = TCJ_CHPviaTRN_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )
   })

    #   OUT_tmp$HOR_TCJviaSJL_pred_tab <- pred_tab_ls_TEMP[["HOR_TCJviaSJL"]]
    # OUT_tmp$HOR_CHPviaSJL_pred_tab <- pred_tab_ls_TEMP[["HOR_CHPviaSJL"]]
    # OUT_tmp$HOR_CHPviaORE_pred_tab <- pred_tab_ls_TEMP[["HOR_CHPviaORE"]]
    # OUT_tmp$TCJ_CHPviaMAC_pred_tab <- pred_tab_ls_TEMP[["TCJ_CHPviaMAC"]]
  
  
  # output$HOR_TCJ_pred_ggpplt_dup1d <- renderPlot({
  #   # reading from previously ran version of the data
  #   # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
  #   HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"

  #   # from scratch version
  #   # this function does a few things:
  #   # renames variables so that column names match the glmmTMB model.matrix
  #   # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
  #   #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
  #   #                                      flength_in=in_selected_RV$flength)

  #   # HOR_TCJ_pred_tab$lo_pred <- log((plogis(HOR_TCJ_pred_tab$lo_pred)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$lo_pred)*1.2)))
  #   # HOR_TCJ_pred_tab$LCL <- log((plogis(HOR_TCJ_pred_tab$LCL)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$LCL)*1.2)))
  #   # HOR_TCJ_pred_tab$UCL <- log((plogis(HOR_TCJ_pred_tab$UCL)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$UCL)*1.2)))

  #   ggplot_doy_rte_plt(
  #     HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
  #     doy_rng_in = c(
  #       in_selected_RV$start_day,
  #       in_selected_RV$end_day
  #     ),
  #     pst_year_in = 2013 + 9
  #     # pst_year_in = in_selected_RV$past_water_year
  #   )
  # })


# output$HOR_TCJ_pred_ggpplt_dup1d <- renderPlot({
#     # reading from previously ran version of the data
#     # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
#     HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"
#     ggplot_doy_rte_plt(
#       HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
#       doy_rng_in = c(
#         in_selected_RV$start_day,
#         in_selected_RV$end_day
#       ),
#       pst_year_in = 2013 + 9
#       # pst_year_in = in_selected_RV$past_water_year
#     )
#   })

  
  output$HOR_pred_ggpplt <- renderPlot({
    # baseline
    # HOR_pred_tab <-  pred_tab_ls[["HOR"]]

    HOR_pred_tab <- OUT_tmp$HOR_pred_tab 

    ggplot_doy_rte_plt(
      HOR_TCJ_pred_tab_plt = HOR_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )
  })


  # output$HOR_TCJ_pred_ggpplt_dup1d2 <- renderPlot({
  #   # reading from previously ran version of the data
  #   # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
  #   TCJ_pred_tab <- OUT_tmp$"TCJ_pred_tab"

  #   # TCJ_pred_tab <- pred_tab_ls$TCJ

  #   # from scratch version
  #   # this function does a few things:
  #   # renames variables so that column names match the glmmTMB model.matrix
  #   # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
  #   #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
  #   #                                      flength_in=in_selected_RV$flength)

  #   # HOR_TCJ_pred_tab$lo_pred <- log((plogis(HOR_TCJ_pred_tab$lo_pred)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$lo_pred)*1.2)))
  #   # HOR_TCJ_pred_tab$LCL <- log((plogis(HOR_TCJ_pred_tab$LCL)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$LCL)*1.2)))
  #   # HOR_TCJ_pred_tab$UCL <- log((plogis(HOR_TCJ_pred_tab$UCL)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$UCL)*1.2)))

  #   ggplot_doy_rte_plt(
  #     HOR_TCJ_pred_tab_plt = TCJ_pred_tab,
  #     doy_rng_in = c(
  #       in_selected_RV$start_day,
  #       in_selected_RV$end_day
  #     ),
  #     # pst_year_in = 2013 + 2
  #     pst_year_in = in_selected_RV$past_water_year
  #   )
  # })

  output$TCJ_pred_ggpplt <- renderPlot({
    # TCJ_pred_tab <-  pred_tab_ls[["TCJ"]]

    TCJ_pred_tab <- OUT_tmp$TCJ_pred_tab 

    ggplot_doy_rte_plt(
      HOR_TCJ_pred_tab_plt = TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )
  })


  output$HOR_TCJ_pred_ggpplt_dup1e <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"

    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)

    HOR_TCJ_pred_tab$lo_pred <- log(
      (plogis(HOR_TCJ_pred_tab$lo_pred) * 0.45) /
        (1 - (plogis(HOR_TCJ_pred_tab$lo_pred) * 0.45))
    )
    HOR_TCJ_pred_tab$LCL <- log(
      (plogis(HOR_TCJ_pred_tab$LCL) * 0.45) /
        (1 - (plogis(HOR_TCJ_pred_tab$LCL) * 0.45))
    )
    HOR_TCJ_pred_tab$UCL <- log(
      (plogis(HOR_TCJ_pred_tab$UCL) * 0.45) /
        (1 - (plogis(HOR_TCJ_pred_tab$UCL) * 0.45))
    )

    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = 2013 + 2
      # pst_year_in = in_selected_RV$past_water_year
    )
  })

  output$HOR_TCJ_pred_ggpplt_dup2 <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]

    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)

    # Loads the reactive values
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"

    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )
  })

  output$HOR_TCJ_pred_ggpplt_dup3 <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"
    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)

    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )
  })



}
