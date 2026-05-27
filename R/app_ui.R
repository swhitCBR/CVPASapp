#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    shinyjs::useShinyjs(),
    # adding 'subtext' on pickerInput
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
    ),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        leftUi=
         tagList(
        actionButton(
          inputId = "top_about_butt",
          label = "About",
          icon("house", style = "color: white;"),
          class = "btn-primary btn-sm",
          style="color: #fff")
          ,
        actionButton(
          # tags$style("color:white"),
          inputId = "top_met_ref_butt",
          label = "Methods and References",
          icon("book", style = "color: white;"),
          class = "btn-primary btn-sm",
          style="color: #fff"
            )
        ,
        tags$a(
          href = "https://www.cbr.washington.edu/", 
          target="_blank",
          style = "padding-top: 0px; padding-bottom: 0px;",
        tags$img(
            src = "www/cbr_logo_horiz.png",
            height = "40px",
            title = "Image of Columbia Basin Research logo"
          )
        )
        )
        ,
        # reactive values ----
        ## Title above sidebar ----
        title = "CVPAS - Steelhead"
      )
      ,
      ## Sidebar content - used as a navigation menu to each tab
      sidebar = shinydashboard::dashboardSidebar(
        #{{alt_config 1}--sub argument}
        # sidebar = shinydashboardPlus::dashboardSidebar(
        tagList(
          collapsed = FALSE, # Set the sidebar to be collapsed by default
          shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(
        "About",
        tabName = "about",
        icon = icon("house"),
        selected = T
      ),
      shinydashboard::menuItem(
        "Inputs",
        tabName = "inputs",
        icon = icon("sliders"),
        startExpanded = T,
        selected = F
      ),
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
          ,
          verbatimTextOutput("sel_in_ls_text")
                  ,
        actionButton("load_butt", "Load")
          # ,
          # verbatimTextOutput("glob_in_ls_text")
        )
      ),
      body = shinydashboard::dashboardBody(
         tags$script(HTML("$('body').addClass('fixed');")),
        # add CSS CBR global theme
        fresh::use_theme(CBRtheme),
        tagList(
          h2("CVPAS - South Delta Central Valley Steelhead Survival and Routing Predictions"),
          hr(),
          uiOutput("main_page_content_dynui")
        )
      )
    ),
    ## Footer content
    footer =    shinydashboardPlus::dashboardFooter(
      left = HTML(
        '<div style="color: #024c63;">
       <a href="https://cbr.washington.edu/" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">Columbia Basin Research</a> •
       <a href="https://fish.uw.edu/" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">School of Aquatic and Fishery Sciences</a> •
       <a href="https://environment.uw.edu/" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">College of the Environment</a> •
       <a href="https://www.washington.edu/" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">University of Washington</a>
     </div>'
      ),
      right = HTML(
        '<span class="footer-contact" style="color: #024c63;">
       <a href="mailto:web@cbr.washington.edu" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">
         <i class="fa fa-envelope" style="color: #024c63;"></i> web@cbr.washington.edu
       </a>
     </span>'
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
      app_title = "CVPAS_v0.9"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()

    #added external JS and CSS files beyond files within www/ folder
    shinyjs::useShinyjs(),
    # # External library  for js cookies (added by CO)
    # tags$script(
    #   src = "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/3.0.1/js.cookie.min.js"
    # )
  )
}
