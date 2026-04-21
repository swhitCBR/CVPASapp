#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # specify starting tab
  shinydashboard::updateTabItems("tabs",session = session,selected = "about")
  observeEvent(input$tabs, { print(input$tabs)})
  
  # Your application server logic
  # SIDE BAR UI
  output$cbr_dyn_sidebar_ui = shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(
        "About",
        tabName = "about",
        icon = icon("house"),
        selected = T
      )
      ,
      shinydashboard::menuItem(
        "Main",
        tabName = "main",
        icon = icon("book")
      )
      ,
      # shinydashboard::menuItem(
      #   "Inputs",
      #   tabName = "input_page",
      #   icon = icon("sliders"),
      #   startExpanded = T,
      #   selected = F
      # ),
      # shinydashboard::menuItem(
      #   text = "Estimates",
      #   tabName = "estimates",
      #   icon = icon("chart-line"),
      #   startExpanded = T #,
      # ),
      br(),
      tags$div(
        id = "newsidebox",
        wellPanel(
          uiOutput("sidebar_text") # Placeholder for dynamic text
        )
      )
    )
  })
  
  output$sidebar_text <- renderUI({
    tagList(
      switch(
        input$tabs,
        "about" = p("hello world")
        # "about" = shiny::includeMarkdown(system.file(
        #   "app/www/sidetxt/about.md",
        #   package = "CVPASbeta"
        # )
        # )
      ,
        "main" = p("hello main")
          # "about" = shiny::includeMarkdown(system.file(
          #   "app/www/sidetxt/about.md",
          #   package = "CVPASbeta"
          )
        # )
        # ,
        # "input_page" = shiny::includeMarkdown(system.file(
        #   "app/www/sidetxt/input.md",
        #   package = "CVPASbeta"
        # )),
        # "met_ref" = shiny::includeMarkdown(system.file(
        #   "app/www/sidetxt/met_ref.md",
        #   package = "CVPASbeta"
        # )),
        # "estimates" = shiny::includeMarkdown(system.file(
        #   "app/www/sidetxt/ests.md",
        #   package = "CVPASbeta"
        # )),
        # shiny::includeMarkdown(system.file(
        #   "app/www/sidetxt/about.md",
        #   package = "CVPASbeta"
        # ))
      # )
    )
  })
}
