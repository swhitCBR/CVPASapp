#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # output$dyn_sidebar_txt <- renderText({paste0("You are viewing tab \"", get_dyn_sidebar_txt(input$tabs), "\"")})
  output$dyn_sidebar_txt <- renderText({paste0(get_sidebar_txt(input$tabs))})

  # autoselect specify starting tab
  # session$onSessionEnded(stopApp)

  # at startup ----
  tab_selected <- reactiveVal("inputs")

  data_source_selected <- reactiveVal("prev_pan")

  ### Tab switching behavior  ----
  observeEvent(input$tabs, {
    tab_selected(input$tabs)
    print(input$tabs)
    # scrollIntoView options
    if(input$tabs=="check"){shinyjs::runjs("document.getElementById('details_indiv').scrollIntoView({ behavior: 'smooth', block: 'start' });")}
    if(input$tabs=="inputs"){shinyjs::runjs("document.getElementById('inputTop').scrollIntoView({ behavior: 'smooth', block: 'start' });")}
    if(input$tabs=="overall_surv"){shinyjs::runjs("document.getElementById('est_box_ui').scrollIntoView({ behavior: 'smooth' });")}
    if(input$tabs=="route_spec_surv"){shinyjs::runjs("document.getElementById('route_spec_surv_panel').scrollIntoView({ behavior: 'smooth' });")}
    if(input$tabs=="route_usage"){shinyjs::runjs("document.getElementById('route_usage_panel').scrollIntoView({ behavior: 'smooth' });")}
    if(input$tabs=="more_info"){shinyjs::runjs("document.getElementById('more_info_panel').scrollIntoView({ behavior: 'smooth' });")}
  })

  output$sel_in_ls_text = renderText({
    RV_text_fun(
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


  observeEvent(input$sidebarItemExpanded, {
    print(paste("expand",input$sidebarItemExpanded))
    if(input$sidebarItemExpanded == "Inputs"){
    shinydashboard::updateTabItems(
      inputId = "tabs",
      session = session,
      selected = "inputs")
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
    RV_text_fun(
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

  output$yr_var_ggpplt <- renderPlot({
    ggplot_yr_env_lattice_plt(
      CVhelp_dat_l_plt = CVhelp_dat_l,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year,
      sub_var_in = input$radio_metric_view
    )
  })

  proxy <- DT::dataTableProxy("table_in_WY")

  output$table_in_WY <- DT::renderDataTable(
    draw_ann_summ_tab(input_year = in_selected_RV$past_water_year
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

  # because this is within a renderUI, changing the input$tab value rewrites the sidebar content
  output$sidebar_text <- renderUI({
    shiny::HTML(
      md_txt_extract(
        md_addr = "inst/app/www/sidebar/sidebar.md",
        header_ref = paste0("# ", tab_selected()),
        asHTML_frag = TRUE
      )
    )
    # draw_ibox_ui(box_content_in = "daily_values_box")
    # box_content_in
  })

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
                HTML(paste(
                  "<p> Fork length of juvenile Steelhead from Six-Year Study </p> "
                ))
              ),
              #                 p(
              #   "This is a paragraph of text. To trigger an event, please ",
              #   # actionLink("link_id", "click this link"),
              #                     shinyWidgets::dropMenu(
              #                 shiny::actionLink(
              #                   inputId = "six_year_ibutt",
              #                   # icon = icon("info"),
              #                   label="Six-Year Study",
              #                   status = "primary",
              #                   size = "xs"
              #                 ),
              #                 p("Lengths of fish relased during acoustic telemetry studies (2011-2016). Overall mean fork length selected by default."),
              #                 placement = "left-start"
              #               ),
              #   " to continue reading."
              # ),

              # ,
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
    ggplot_day_input_ref_hist(
      DOY_arvDF_l_in = DOY_arvDF_l,
      LOC_in = in_selected_RV$LOC,
      # start_day_in = in_selected_RV$start_day,
      # end_day_in = in_selected_RV$end_day
      start_day_in = input$DOYslider_rng[1],
      end_day_in = input$DOYslider_rng[2]
    )
  })

  output$input_page_UI <- renderUI({
    tagList(
      # draw_inputs_panel_UI()
      # ,
      # uiOutput("inputs_panel_UI"),
      # uiOutput("estimates_panel_ui")
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

  # output$start_loc_ui <- renderUI({
  #   div(
  #     style = "display: inline-flex; align-items: left;margin-top: 10px;",
  #     div(
  #       style = "height=25px; align-self:center",
  #       shinyWidgets::pickerInput(
  #         # label="Junction:",
  #         label = "Starting Location:",
  #         inline = T,
  #         width = "fit",
  #         'start_loc_in',
  #         choices = loc_opt_nms,
  #         selected = in_selected_RV$LOC,
  #         ,
  #         choicesOpt = list(
  #           style = paste0(
  #             "background-color:",
  #             loc_opt_cols,
  #             ";"
  #           )
  #         )
  #       )
  #     )
  #   )
  # })

  # observeEvent(input$inputs_done, {

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
  OUT_tmp$HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]

  observeEvent(input$load_butt, {
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # uses the full previous years data data set for everything except flength
    # setting to 'in_selected_RV' vs 'in_global_RV'
    # in_selected_RV
    OUT_tmp$HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(
      sel_rows_tmp1 = CVhelp_dat_w, # uses the full previous years data data set for everything except flength
      HOR_TCJ_mod_ls = glmmTMB_mod_ls[["HOR_TCJ"]],
      flength_in = in_selected_RV$flength
    )


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


  output$HOR_TCJ_pred_ggpplt_rs1 <- renderPlot({
    HOR_TCJ_pred_tab <- OUT_tmp$HOR_TCJ_pred_tab

    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = in_selected_RV$past_water_year
    )
  })



  output$HOR_TCJ_pred_ggpplt_rs_2 <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"
    HOR_TCJ_pred_tab_ORIG <- HOR_TCJ_pred_tab

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

  # HOR_TCJ_pred_tab_mod <- HOR_TCJ_pred_tab_ORIG
  # HOR_TCJ_pred_tab_mod$lo_pred <- HOR_TCJ_pred_tab$lo_pred /HOR_TCJ_pred_tab_ORIG$lo_pred 


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


  
  output$HOR_TCJ_pred_ggpplt_s_tot <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"
    HOR_TCJ_pred_tab_ORIG <- HOR_TCJ_pred_tab

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

  HOR_TCJ_pred_tab_mod <- HOR_TCJ_pred_tab_ORIG
  HOR_TCJ_pred_tab_mod$lo_pred <- HOR_TCJ_pred_tab$lo_pred*HOR_TCJ_pred_tab_ORIG$lo_pred 
  HOR_TCJ_pred_tab_mod$LCL <- HOR_TCJ_pred_tab$LCL*HOR_TCJ_pred_tab_ORIG$LCL 
  HOR_TCJ_pred_tab_mod$UCL <- HOR_TCJ_pred_tab$UCL*HOR_TCJ_pred_tab_ORIG$UCL 

    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab_mod,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = 2013 + 9
      # pst_year_in = in_selected_RV$past_water_year
    )
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

  output$HOR_TCJ_pred_ggpplt_dup1b <- renderPlot({
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"
    ggplot_doy_pred_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      # pst_year_in = in_selected_RV$past_water_year
      pst_year_in = 2013 + 8
    )
  })


  output$HOR_TCJ_pred_ggpplt_dup1c <- renderPlot({
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

  output$HOR_TCJ_pred_ggpplt_dup1d <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"

    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)

    # HOR_TCJ_pred_tab$lo_pred <- log((plogis(HOR_TCJ_pred_tab$lo_pred)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$lo_pred)*1.2)))
    # HOR_TCJ_pred_tab$LCL <- log((plogis(HOR_TCJ_pred_tab$LCL)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$LCL)*1.2)))
    # HOR_TCJ_pred_tab$UCL <- log((plogis(HOR_TCJ_pred_tab$UCL)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$UCL)*1.2)))

    ggplot_doy_rte_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = 2013 + 9
      # pst_year_in = in_selected_RV$past_water_year
    )
  })

  output$HOR_TCJ_pred_ggpplt_dup1d2 <- renderPlot({
    # reading from previously ran version of the data
    # HOR_TCJ_pred_tab <- pred_prev_yrs_ls[["HOR_TCJ_pred_tab"]]
    HOR_TCJ_pred_tab <- OUT_tmp$"HOR_TCJ_pred_tab"

    # from scratch version
    # this function does a few things:
    # renames variables so that column names match the glmmTMB model.matrix
    # HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
    #                                      HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
    #                                      flength_in=in_selected_RV$flength)

    # HOR_TCJ_pred_tab$lo_pred <- log((plogis(HOR_TCJ_pred_tab$lo_pred)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$lo_pred)*1.2)))
    # HOR_TCJ_pred_tab$LCL <- log((plogis(HOR_TCJ_pred_tab$LCL)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$LCL)*1.2)))
    # HOR_TCJ_pred_tab$UCL <- log((plogis(HOR_TCJ_pred_tab$UCL)*1.2 ) / (1-(plogis(HOR_TCJ_pred_tab$UCL)*1.2)))

    ggplot_doy_rte_plt(
      HOR_TCJ_pred_tab_plt = HOR_TCJ_pred_tab,
      doy_rng_in = c(
        in_selected_RV$start_day,
        in_selected_RV$end_day
      ),
      pst_year_in = 2013 + 2
      # pst_year_in = in_selected_RV$past_water_year
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
