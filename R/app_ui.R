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
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        title = "CVPAS - Steelhead",
        #{{alt_config 1}--sub argument}
        # title=NULL,
        tags$li(
          class = "dropdown header-img",
          tags$style(HTML(".header-img {float: right;padding-right: 10px;")),
          #{{alt_config 1}--uncomment}
          # span("CVPAS - Steelhead", style = 'background-color: #01394a; color: white; font-weight: bold;transition: width .3s ease-in-out;display: block;float: left;height: 50px;font-size: 20px;line-height: 50px;text-align: left;width: 300px;font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;padding: 0 15px;font-weight: 300;overflow: hidden;'),
          tags$img(
            src = "www/cbr_logo_horiz.png",
            height = "50px",
            alt = "Image of Columbia Basin Research logo"
          )
        )
      ),
      ## Sidebar content - used as a navigation menu to each tab
      sidebar = shinydashboard::dashboardSidebar(
        #{{alt_config 1}--sub argument}
        # sidebar = shinydashboardPlus::dashboardSidebar(
        tagList(
          collapsed = FALSE, # Set the sidebar to be collapsed by default
          uiOutput("cbr_dyn_sidebar_ui")
        )
      ),
      body = shinydashboard::dashboardBody(
        # add CSS CBR global theme
        fresh::use_theme(CBRtheme),
        tagList(
          h2("CVPAS - Central Valley Steelhead Survival and Routing Predictions"),
          hr(),
          uiOutput("top_of_body_text"),
          shinydashboard::tabItems(
            shinydashboard::tabItem(
              "about",
              mod_about_page_ui("about_page_ui_1")
            ),
            shinydashboard::tabItem(
              "main",
              mod_main_page_ui("main_page_ui_1")
              # ,
              # ,

              # uiOutput("input_panel_UI")
              # ,
              # uiOutput("input_page_UI")

              # )
            )
          )
          ,
          fluidPage(
            # h1("page_top")
            column(
              actionButton("load_butt", "load"),
              checkboxInput("load_check", label = "load"),
              actionButton("inputs_done2", "Done"),
              width = 2
            ),
            column(verbatimTextOutput("sel_in_ls_text"), width = 5),
            column(verbatimTextOutput("glob_in_ls_text"), width = 5)
          )
          # table_in_WY2
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
        # ,
        # shinyWidgets::setShadow(class = "dropdown-menu")
      )
      # ,
      # shiny::uiOutput("submodule_env_data_view_1-ui_DOY_sel_box")
    ),
    ## Footer content
    footer = uiOutput("CBR_footer_ui") # CREATED IN "app_server.r"
  )
  # )
  # )
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
      app_title = "CVPAS_v0.9"
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
