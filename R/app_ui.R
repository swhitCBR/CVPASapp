#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # adding package that allows selectors to be disabled/enabled (i.e., grayed-out and non reactive)
    shinyjs::useShinyjs(),
    # adding 'subtext' on pickerInput
    tags$head(
    #   tags$script("
    #   Shiny.addCustomMessageHandler('scroll-to', function(id) {
    #     document.getElementById(id).scrollIntoView({behavior: 'smooth'});
    #   });
    # "),
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
      header = draw_dashboard_header_ui()
      ,
      ## Sidebar content - used as a navigation menu to each tab
      sidebar = draw_sidebar_ui()
      ,
      # sidebar = shinydashboard::dashboardSidebar(
      #   draw_sidebar_ui()
      # ),
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
             || input.tabs == 'overall_surv' || input.tabs == 'reach_surv'|| input.tabs == 'route_spec_surv' || input.tabs == 'route_usage' || input.tabs == 'more_info'",
            draw_inputs_panel_ui(init_data_source="Previous years"),
            draw_estimates_panel_ui(),
          )
          ,
          conditionalPanel(
            condition = "input.tabs == 'about'",
            draw_about_page_ui()
          )
          ,
          conditionalPanel(
            condition = "input.tabs == 'met_ref'",
            uiOutput("met_ref_page_ui")
          )
        )
      )
    ),
    ## Footer content
    footer =    draw_CBR_footer_ui()
  )
}

