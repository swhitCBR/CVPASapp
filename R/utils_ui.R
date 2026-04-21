#' @title Combine shinydashboard elements
#' 
#' @description convenience function for combining subcomponents of the shinydashboard
#'
#' @param SD_header_in output from shinydashboard::dashboardHeader
#' @param SD_sidebar_in output from shinydashboard::dashboardSidebar()
#' @param SD_body_in output from shinydashboard::dashboardBody()
#' @param SD_footer_in output from shinydashboard::dashboardFooter()
#'
#' @returns shinydashboard object to include in fluid_row
#' 
assemble_dashboard <- function(SD_header_in=get_dashboard_header(),
                               SD_sidebar_in=get_dashboard_sidebar(),
                               SD_body_in=get_dashboard_body(),
                               SD_footer_in=get_dashboard_footer()){
  shinydashboardPlus::dashboardPage(
    header=SD_header_in,
    sidebar=SD_sidebar_in,
    body=SD_body_in,
    footer=SD_footer_in)
}

#' @title Produce shinydashboard header
#'
#' @param app_name_in name for the app
#' @param vers_name_in version name for the app
#'
#' @returns dashboardHeader object
#' 
get_dashboard_header <- function(app_name_in=golem::pkg_name(),# was just "CVPAS - Steelhead",
                                 vers_name_in="0.0.9"){

  vers_name_in <- paste0(" v",vers_name_in)
  title_ele_in <- HTML(paste0(app_name_in,"<small style ='font-size:0.4em;'>",vers_name_in,"</small>"))
  
  shinydashboard::dashboardHeader(
    title = title_ele_in,
    # Add a logo to the header
    tags$li(
      class = "dropdown header-img",
      tags$style(HTML("
              .header-img {
                float: right;
                padding-right: 10px;
              }
            ")),
      tags$img(src = "www/cbr_logo_horiz.png", height = "50px", alt = "Image of Columbia Basin Research logo")
    )
  )
  
}

# icons that can be used for modules
# https://icons.getbootstrap.com/

get_dashboard_sidebar <- function(){
  
  shinydashboard::dashboardSidebar(
    collapsed = FALSE,  # Set the sidebar to be collapsed by default
    shinydashboard::sidebarMenu(
      id = "tabs",
      
      shinydashboard::menuItem("About", tabName = "about", icon = icon("house"), selected = TRUE),
      shinydashboard::menuItem("CVPAS - Steelhead", tabName = "figs", icon = icon("chart-line"), selected = TRUE),
      # shinydashboard::menuItem("CVPAS - Steelhead", tabName = "figs", icon = icon("chart-line"), selected = TRUE),
      shinydashboard::menuItem("Methods & References", tabName = "supp", icon = icon("book")),
      shinydashboard::menuItem("User Guide & Release Notes", tabName = "info", icon = icon("info-circle"))
    ),
    br(),
    tags$div(
      style = "color: black; font-size: 14px; padding-left: 5px; padding-right: 5px",
      "Users can close this sidebar panel to see larger graphs by clicking on the",
      icon("navicon",
           style = "background-color:#024c63; color:white; padding:3px; border-radius:4px;"),
      "icon."
    ),
    
    br(),
    tags$div(
      id = "dynamic_sidebar_text",
      style = "color: #024c63; font-size: 14px; padding-left: 5px; padding-right: 5px",
      uiOutput("sidebar_text")  # Placeholder for dynamic text
    )
  )
  
}

get_dashboard_body <- function(){
  
  shinydashboard::dashboardBody(
    #add CSS CBR global theme
    fresh::use_theme(CBRtheme),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "about",
        h1("blah blah")),
      shinydashboard::tabItem(
        # tabName = "CVPAS - Steelhead",
        tabName = "figs",
        h1("blah blah","bleh"))
    )
  )
}

get_dashboard_footer <- function(){
  
  SD_footer <- shinydashboardPlus::dashboardFooter(
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
  
}
