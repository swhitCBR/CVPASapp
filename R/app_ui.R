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
        leftUi=draw_dashboard_header_ui(),
        title = "CVPAS - Steelhead"
      )
      ,
      ## Sidebar content - used as a navigation menu to each tab
      sidebar = shinydashboard::dashboardSidebar(
        draw_sidebar_ui()
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
          hr()
          ,
          conditionalPanel(
            condition = "input.tabs == 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'
             || input.tabs == 'overall_surv' || input.tabs == 'route_spec_surv' || input.tabs == 'route_usage' || input.tabs == 'more_info'",
            draw_inputs_panel_ui(),
            draw_estimates_panel_ui(),
          )
          ,
          conditionalPanel(
            condition = "input.tabs == 'about'",
            # mod_about_page_ui("mod_about_page-about_page_ui_1")
            draw_about_page_ui()
          )
          ,
          conditionalPanel(
            condition = "input.tabs == 'met_ref'",
            uiOutput("met_ref_page_ui")
          )
          # uiOutput("main_page_content_dynui")
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

