#' Title
#'
#' @returns shinydashboard sidebar panel shinyUI code (i.e., within `shiny::tagList()`)
#' 
#' @details 
#' calls [get_custom_css_txt()] for special formatting
#'
#' @export 
#' 
#' @examples
#' draw_sidebar_ui()
#' 
#' 
draw_sidebar_ui <- function() {
  shinydashboard::dashboardSidebar(
        tagList(
          tags$head(
            tags$style(HTML(get_custom_css_txt(css_txt_content="sidebar"))
          )
        ),
          # collapsed = FALSE, # Set the sidebar to be collapsed by default
          shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(
        "About",
        tabName = "about",
        icon = icon("house"),
        selected = FALSE
      ),
      shinydashboard::menuItem(
        "Inputs",
        # tabName = "inputs",
        expandedName ="inputs_title",
        icon = icon("sliders"),
        startExpanded = T,
        shinydashboard::menuSubItem(
          "Starting location",
          tabName = "inputs",
          icon = HTML('<i class="fa-solid fa-location-dot"></i>'),
          selected = TRUE
        )
          ,
          shinydashboard::menuSubItem(
          "Check inputs",
          tabName = "check",
          icon = icon("check"),
          selected = F)
      ),
      shinydashboard::menuItem(
        text = "Estimates",
        # tabName = "estimates",
        expandedName ="estimates_title",
        icon = icon("chart-line"),
        startExpanded = T ,
        shinydashboard::menuSubItem(
          "Overall survival",
          tabName = "overall_surv",
          icon=HTML('<i class="fa-solid fa-solidLarge fa-angle-right">‌</i>'),
          selected = F),
        shinydashboard::menuSubItem(
          "Reach survival",
          tabName = "reach_surv",
          icon=HTML('<i class="fa-solid fa-solidLarge fa-angle-right">‌</i>'),
                    selected = F),
        shinydashboard::menuSubItem(
          "Route-specific survival",
          tabName = "route_spec_surv",
          icon=HTML('<i class="fa-solid fa-solidLarge fa-angle-right">‌</i>'),
                    selected = F),
        shinydashboard::menuSubItem(
          "Route usage",
          tabName = "route_usage",
          icon=HTML('<i class="fa-solid fa-solidLarge fa-angle-right">‌</i>'),selected = F)
        # ,
        # shinydashboard::menuSubItem(
        #   "More information",
          
        #   tabName = "more_info",
        #   icon=HTML('<i class="fa-solid fa-solidLarge fa-angle-right">‌</i>'),
        #   selected = F)
      ),

      # This is an invisible side panel
      # could not get the sidebar menu to hide using shinyjs
      # shinyjs::disabled(
      div(id="met_ref_side",
        style="display: none !important;",
      shinydashboard::menuItem(
        # id="invis_met_ref",
        # style="display: none !important;",
        text = "Help",
        tabName = "met_ref",
        icon = icon("book"),
        startExpanded = T #,
      )
      )
    )
          ,
      br(),
      tags$div(
        id = "newsidebox", #needed for applying specific formatting
        wellPanel(
          textOutput("dyn_sidebar_txt")
        )
        ### FOR DEBUGGING ###
        ,
        verbatimTextOutput("sel_in_ls_text")
        # ,
        # actionButton("load_butt", "Load")
        )
      )
    )
      }