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
    tags$head(
          tags$script("
      Shiny.addCustomMessageHandler('scroll-to', function(id) {
        document.getElementById(id).scrollIntoView({behavior: 'smooth'});
      });
    "),
    # tags$style(HTML("
    #     .scroll-box {
    #       scroll-margin-top: 600px; /* 50px header + 20px padding */
    #     }
    #   ")),
    tags$style(HTML("
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
      options = list(sidebarSlimScroll = FALSE),
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
        selected = F,
        shinydashboard::menuSubItem(
          "Starting location",
          tabName = "inputs",
          # icon = icon("A"),#<i class="fa-solid fa-a"></i>
          icon = HTML('<i class="fa-solid fa-location-dot"></i>'),
          selected = F)
          ,
          shinydashboard::menuSubItem(
          "Check Inputs",
          tabName = "check",
          icon = icon("check"),
          selected = F)
      ),
      # shinyjs::hide(
      shinydashboard::menuItem(
        text = "Estimates",
        tabName = "estimates",
        icon = icon("chart-line"),
        startExpanded = T #,
      # )
      ),
      # This is an invisible side panel
      # could not get the sidebar menu to hide using shinyjs
      # shinyjs::disabled(
      div(id="met_ref_side",
      shinydashboard::menuItem(
        text = NULL,#"Help",
        tabName = "met_ref",
        icon =NULL,# icon("book"),
        startExpanded = T #,
      )
      )
      # )
      ,
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
        #             shinyWidgets::pickerInput(
        #               inputId = "data_source_picker_dup",
        #               choices = c(
        #                 "Previous year",
        #                 "Uploaded file (.csv)",
        #                 "None"
        #               ),
        #               width = "180px",
        #               selected = init_data_source
        #             )
          # ,
          # verbatimTextOutput("glob_in_ls_text")
        )
      ),
      body = shinydashboard::dashboardBody(

        ##ALT## {prevents the sidebar from scrollling with rest of page}
        ## {causes a seemingly minor error in the javascript that has to do with 'slim-slider'}
         tags$script(HTML("$('body').addClass('fixed');")),
        #######

        # adds CSS CBR global theme
        fresh::use_theme(CBRtheme),
        tagList(
          h2("CVPAS - South Delta Central Valley Steelhead Survival and Routing Predictions",id="inputTop")
        ,
          hr()#id="inputTop")
          ,
        conditionalPanel(
                  #  condition = "input.tabs == 'inputs'",
         condition = "input.tabs == 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'",
        #  condition = "(typeof input.tabs !== 'undefined') && (input.tabs == 'inputs' || input.tabs == 'estimates')",
          draw_inputs_panel_ui(),
          draw_estimates_panel_ui(),
          # hr(id="estimatesTop")
         )
         ,
        conditionalPanel(
         condition = "input.tabs == 'about'",
         mod_about_page_ui("mod_about_page-about_page_ui_1")
         )
        ,
        conditionalPanel(
         condition = "input.tabs == 'met_ref'",
         uiOutput("met_ref_page_ui")
         )
          # uiOutput("main_page_content_dynui")
        )
        ,
  # actionButton("btn", "Go to Bottom"),
  # div(style = "height: 1000px;"), # Spacer
  # div(id = "bottom_element", h3("You've arrived!"))
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
