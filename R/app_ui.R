#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    shinyjs::useShinyjs(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # # Your application UI logic
    fluidPage(
      shinydashboardPlus::dashboardPage(
        header = shinydashboard::dashboardHeader(
          title = HTML(
            "CVPAS - Steelhead"
          ),
          # Add a logo to the header
          tags$li(
            class = "dropdown header-img",
            tags$style(HTML(
              "
              .header-img {
                float: right;
                padding-right: 10px;
              }
            "
            )),
            tags$img(
              src = "www/cbr_logo_horiz.png",
              height = "50px",
              alt = "Image of Columbia Basin Research logo"
            )
          )
        ),
        ## Sidebar content - used as a navigation menu to each tab
        sidebar = shinydashboard::dashboardSidebar(
          collapsed = FALSE, # Set the sidebar to be collapsed by default
          # verbatimTextOutput("glob_code"),
          uiOutput("cbr_dyn_sidebar_ui")
          #, # CREATED IN "app_server.r"
          # h2("temp")
          # uiOutput("initiation_text_ui"),
          # verbatimTextOutput("loc_result_temp"),
          # verbatimTextOutput("load_in_ls_text"),
          # actionButton(inputId = "load_button_sub_env", label = "Load"),
          # shinyWidgets::materialSwitch(
          #   inputId = "loaded_switch",
          #   label = HTML(
          #     "<label style ='font-size:small;'>Custom Inputs </label>"
          #   ), #"Additional options",
          #   value = FALSE,
          #   status = "primary",
          #   inline = T),
          # actionButton(inputId = "run_button", label = "Run"),
          # actionButton(inputId = "SE_button", label = "get SEs"),
          # actionButton(inputId = "reset_button", label = "Reset")
        ),
        body = shinydashboard::dashboardBody(
          #add CSS CBR global theme
          fresh::use_theme(CBRtheme),
          tagList(
            # h2("temp2")
            # div(
            #   class = "thumbnail-section",
            #   style = "margin-top: 10px;",
            #   actionButton(
            #     inputId = "submodule_env_data_view_1-view_sel_row", # references a button within a submodule
            #     label = "View Selection"
            #   )
            # ),
            # mod_submodule_env_data_view_ui("submodule_env_data_view_1"),
            # uiOutput("mod_estimates_uiALT"),
            # shiny::wellPanel(
            #   shiny::tabsetPanel(
            #     shiny::tabPanel(
            #       title = "Data Table",
            #       value = "env_view_t2_modal",
            #       br(),
            #       DT::dataTableOutput("table_env_inputs")
            #     ),
            #     shiny::tabPanel(
            #       title = "Refresh Table",
            #       value = "refresh_tab",
            #       br(),
            #       actionButton(
            #         inputId = "refresh_butt_tab",
            #         label = "Refresh"
            #       ),
            #       br(),
            #       DT::dataTableOutput("refresh_tab")
            #     ),
            #     shiny::tabPanel(
            #       title = "Single Variable",
            #       collapsed = F,
            #       shinyWidgets::radioGroupButtons(
            #         'submodule_env_data_view_1-radio_metric_view',
            #         label = "Metric",
            #         choices = c(
            #           "log(VNS)" = "VNS",
            #           "T_msd" = "MSD",
            #           "T_clc" = "CLC",
            #           "CVP" = "CVP",
            #           "SWP" = "SWP"
            #         )
            #       ),
            #       plotOutput("submodule_env_data_view_1-doy_ovr_plt2")
            #     ),
            #     shiny::tabPanel(
            #       title = "Lattice",
            #       collapsed = F,
            #       actionButton(
            #         inputId = "refresh_butt_latt",
            #         label = "Refresh"
            #       ),
            #       plotOutput('doy_latt_ggpplt')
            #     )
            #   )
            # )
            # ,
            shinydashboard::tabItems(
              shinydashboard::tabItem(
                "about",
                mod_about_page_ui("about_page_ui_1")
              )
              ,
              shinydashboard::tabItem(
                "main",
                mod_main_page_ui("main_page_ui_1")
              )
              # shinydashboard::tabItem(
              #   "met_ref",
              #   mod_supplementary_page_ui("supplementary_page_ui_1")
              # ),
              # shinydashboard::tabItem(
              #   "input_page",
              #   tagList(
              #     h1("previously contained 'mod_submodule_env_data_view_ui'")
              #   )
              # ),
              # shinydashboard::tabItem(
              #   "estimates",
              #   tagList(
              #   )
              # )
            )
          )
          # ,
          # shiny::uiOutput("submodule_env_data_view_1-ui_DOY_sel_box")
        ),
        ## Footer content
        footer = uiOutput("CBR_footer_ui") # CREATED IN "app_server.r"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    # favicon(resources_path = "app/www"),
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CVPASalpha"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    #added external JS and CSS files beyond files within www/ folder
    shinyjs::useShinyjs(),
    
    # # External library  for js cookies (added by CO)
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/3.0.1/js.cookie.min.js"
    )
  )
}
