#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML("
    .dropdown-menu span {width: 100%;} 
    .text-muted {color: black !important; float: right;}
    .bootstrap-select .dropdown-menu {
    min-width: 150;
    #schem_info_drop_div .dropdown:hover .dropdown-menu {
        display: block;
        margin-top: 0;
      }
        
    #date_end_sep { background-color: #f4f4f4 !important;}

    "))
    # ,
    # tags$style(HTML("
    #   /* Make the dropdown open on hover */
    #   .dropdown:hover .dropdown-menu {
    #     display: block;
    #     margin-top: 0;
    #   }
    # "))
    ),
    # # tags$style(HTML("
    # #   /* Change font size of the input text box */
    # #   #date_start_sep {font-size: 75%;height: auto;}
    # #   #date_start_sep-label {font-size: 75%;height: auto;}
    # #   #date_end_sep {font-size: 75%;height: auto;}
    # #   #date_end_sep-label {font-size: 75%;height: auto;}"
    # #    ))
    #   ),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydashboardPlus::dashboardPage(

      header = shinydashboardPlus::dashboardHeader(
        # tags$style="height: 60px;",
        leftUi=
         tagList(
        # shinyWidgets::dropdownButton(
        #       circle = TRUE,
        #       size = "xs",
        #       status = "primary",
        #       icon = icon("info", style = "color: white;"),
        #       width = "300px",
        #       HTML("<div style='color: black; margin: 10px;'>
        #         <p>This interactive online tool (Shiny app) provides reach survival estimates for wild spring/summer Chinook Salmon, steelhead, and fall Chinook Salmon
        #         originating upstream of Lower Granite Dam (Snake River, Pacific Northwest, USA) using a hierarchical Bayesian CJS mark-recapture model based on the method from Gosselin et al. (2021).</p>
        #         <p>The probability of survival includes direct and carryover effects on wild Snake River Chinook Salmon and Steelhead from juvenile to adult life stages in the hydrosystem.</p>
        #         <br>
        #         <p>To assist with navigation, descriptions of the menu items, tabs, and acronyms used in the tool are provided below.</p>
        #       </div>
        #       ")
        #   )
    #       ,
        
    #       shinyWidgets::dropdown(
    #             icon=icon("info"),
    #             tags$h3("List of Input"),

    # shinyWidgets::pickerInput(inputId = 'xcol2',
    #             label = 'X Variable',
    #             choices = names(iris),
    #             options = list(`style` = "btn-info"))
    #       )
          # ,
          # shinydashboard::dropdownMenu(
          # icon=icon("info"),
          # tags$li(
          #   p("TEMP")
          # )
          # # ,
          # # shinydashboardPlus::messageItem(from = "Support Team", message = "This is the content of a message.", time = "5 mins")#,
          # # shinydashboardPlus::messageItem(from = "Support Team", message = "This is the content of another message.", time = "2 hours"),
          # # shinydashboardPlus::messageItem(from = "New User", message = "Can I get some help?", time = "Today")
        # )
        # ,
        # shinyWidgets::dropdown(
        #   label = "Controls",
        #   icon = icon("sliders-h"),
        #   status = "primary",
        #   circle = FALSE,
        #   sliderInput(
        #     inputId = "dropdown_header",
        #     label = "Number of observations",
        #     min = 10, max = 100, value = 30
        #   )
        # )
          # ,
        actionButton(
          # tags$style("color:white"),
          inputId = "top_about_butt",
          label = "About",
          icon("house", style = "color: white;"),
          class = "btn-primary btn-sm",
          style="color: #fff"
          # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
            ,
        actionButton(
          # tags$style("color:white"),
          inputId = "top_met_ref_butt",
          label = "Methods and References",
          icon("book", style = "color: white;"),
          class = "btn-primary btn-sm",
          style="color: #fff"
          # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
        #     ,
        # shinyWidgets::dropdownButton(
        #   label = "Methods and References",
        #   status = "primary",
        #   circle = FALSE,
        #   icon = icon("book", style = "color: white;"),
        #   width = "300px",
        #       HTML("<div style='color: black; margin: 10px;'>
        #         <p>This interactive online tool (Shiny app) provides reach survival estimates for wild spring/summer Chinook Salmon, steelhead, and fall Chinook Salmon
        #         originating upstream of Lower Granite Dam (Snake River, Pacific Northwest, USA) using a hierarchical Bayesian CJS mark-recapture model based on the method from Gosselin et al. (2021).</p>
        #         <p>The probability of survival includes direct and carryover effects on wild Snake River Chinook Salmon and Steelhead from juvenile to adult life stages in the hydrosystem.</p>
        #         <br>
        #         <p>To assist with navigation, descriptions of the menu items, tabs, and acronyms used in the tool are provided below.</p>
        #       </div>
        #       ")
        # )
        ,
        tags$a(
          href = "https://www.cbr.washington.edu/", 
          target="_blank",
          style = "padding-top: 0px; padding-bottom: 0px;",
          # tags$style="padding-top: 0px; padding-bottom: 0px;"
        tags$img(
            src = "www/cbr_logo_horiz.png",
            height = "40px",
            title = "Image of Columbia Basin Research logo"
          )
        )
        )
          ,
        # HTML('<span class="logo">CVPAS - Steelhead</span>'),
        # tags$li("CVPAS - Steelhead"),
        title = "CVPAS - Steelhead"
        # title = HTML("<h4 style=height: 60px> CVPAS - Steelhead </h4>")
        # tags$style="height: 60px;"
        # ,
        # #{{alt_config 1}--sub argument}
        # # title=NULL,
        # tags$li(
        #   class = "dropdown header-img",
        #   style="display: inline-flex;",
        #   # tags$style(HTML(".header-img {float: right;padding-right: 10px;")),
        #   tags$style(HTML(".header-img {float: right;padding-right: 10px;")),
        #   #{{alt_config 1}--uncomment}
        #   # span("CVPAS - Steelhead", style = 'background-color: #01394a; color: white; font-weight: bold;transition: width .3s ease-in-out;display: block;float: left;height: 50px;font-size: 20px;line-height: 50px;text-align: left;width: 300px;font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;padding: 0 15px;font-weight: 300;overflow: hidden;'),
        #   # shinydashboardPlus::userOutput("user"),
        #   tags$img(
        #     src = "www/cbr_logo_horiz.png",
        #     height = "50px",
        #     alt = "Image of Columbia Basin Research logo"
        #   )
        # )
      ),
      ## Sidebar content - used as a navigation menu to each tab
      sidebar = shinydashboard::dashboardSidebar(
        #{{alt_config 1}--sub argument}
        # sidebar = shinydashboardPlus::dashboardSidebar(
        tagList(
          collapsed = FALSE, # Set the sidebar to be collapsed by default
          uiOutput("cbr_dyn_sidebar_ui")
          # ,
          # verbatimTextOutput("sel_in_ls_text")
          # ,
          # verbatimTextOutput("glob_in_ls_text")
        )
      ),
      body = shinydashboard::dashboardBody(
        # add CSS CBR global theme
        fresh::use_theme(CBRtheme),
        tagList(
          h2("CVPAS - South Delta Central Valley Steelhead Survival and Routing Predictions"),
          hr(),
          # TOP ROW 
          # p("This tool generates survival and routing predictions for juvenile Steelhead orginating from the San Joaquin basin based environmental and operational conditions.")
          # ,
          uiOutput("top_of_body_text"),
          shinydashboard::tabItems(
            shinydashboard::tabItem(
              "about"#,
            ),
            shinydashboard::tabItem(
              "met_ref",
              mod_met_ref_page_ui("met_ref_page_ui_1")
              # ,
              # ,

              # uiOutput("input_panel_UI")
              # ,
              # uiOutput("input_page_UI")

              # )
            )
          )
          # ,
 
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
