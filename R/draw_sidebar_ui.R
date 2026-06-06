#' Title
#'
#' @returns shinydashboard sidebar panel shinyUI code (i.e., within `shiny::tagList()`)
#' 
#' @details 
#' calls [get_custom_css_txt()] for special formatting
#'
#' @family {ui builders }
#' @family {front-end functions }
#' 
#' @export 
#' 
#' @examples
#' draw_sidebar_ui()
#' 
#' 
draw_sidebar_ui <- function() {
         #{{alt_config 1}--sub argument}
        # sidebar = shinydashboardPlus::dashboardSidebar(
        tagList(
                tags$head(tags$style(HTML(get_custom_css_txt(css_txt_content="sidebar")))),
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
      # div(id="met_ref_side",
      shinydashboard::menuItem(
        text = NULL,#"Help",
        tabName = "met_ref",
        icon =NULL,# icon("book"),
        startExpanded = T #,
      )
      # )
      # )

    )
          ,
      br(),
      tags$div(
        id = "newsidebox",
          # conditionalPanel(
          #   # condition = "input.tabs == 'inputs'",
          #   condition = paste0("input.tabs == '",input$tabs,"'"),
          #   get_sidebar_txt(input$tabs)
          # )
          # get_sidebar_txt("inputs")
          # get_sidebar_txt(input$tabs)
        wellPanel(
          textOutput("dyn_sidebar_txt"),
          get_sidebar_txt_content("ha")
        )
        ,
        verbatimTextOutput("sel_in_ls_text")
        ,
        actionButton("load_butt", "Load")
        )
      )
      }