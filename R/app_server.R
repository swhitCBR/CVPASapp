#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


  WYT_cols <- c("#3399FF", "#99EEFF", "#FFFFCC", "#FFCC66", "#FF5500")

  names(WYT_cols) <- c(
      "Wet",
      "Below Normal",
      "Above Normal",
      "Dry",
      "Critical"
    ) # new addition

  wyt_type_opt <- c(
      "Wet" = "Wet",
      "Above Normal" = "Above Normal",
      "Below Normal" = "Below Normal",
      "Dry" = "Dry",
      "Critical" = "Critical"
  )
  
  # specify starting tab
  shinydashboard::updateTabItems("tabs",session = session,selected = "main")
  observeEvent(input$tabs, { print(input$tabs)})

  init_water_year <- 2013
  init_WYT_type <- "Wet"
  init_start_loc <- "HOR"
  init_bar <- "Out"

  global <- reactiveValues(
      # past water year should be NA to start otherwise app flickers
      past_water_year = init_water_year,
      WYT = init_WYT_type,
      LOC = init_start_loc,
      BAR = init_bar
  )

  # rendering annual data table
   output$table_in_WY <- DT::renderDataTable(
     DT::datatable(
        ann_HORbar_WYT_data |> 
          dplyr::mutate(Model=ifelse(Year %in% c("2011","2012","2013","2014","2015","2016"),"Yes","No")) |>
          dplyr::select(Year,WYT,barrier,Model)
        ,
        selection = list(
          mode = 'single',
          target = "row",
          selected = which(ann_HORbar_WYT_data$Year == global$past_water_year)
        ),
        colnames = c("Year", "Category", "HOR Barrier","Model"),
        rownames = FALSE,
        options=list(
          dom='<"<"bottom"ip>',
          stripeClasses = list(),
        lengthMenu = c(13),
        pagingType = "simple",
        initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'font-size': '75%'});",
        "$(this.api().table().body()).css({'font-size': '75%'});",
        "$(this.api().table().caption()).css({'font-size': '75%'});",
        "}")
      ))  |>
        DT::formatStyle('WYT',backgroundColor = DT::styleEqual(wyt_type_opt, WYT_cols)) |>
        DT::formatStyle('Model',backgroundColor = DT::styleEqual(c("Yes","No"), c("white","#B3B3B3")))
        # )
   )
  
    # update values when water year is selected
    observeEvent(
      eventExpr = {
        input$table_in_WY_rows_selected
      },
      {
        global$past_water_year <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected,"Year"]
        global$WYT <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected, "WYT"]
        global$BAR <- ann_HORbar_WYT_data[input$table_in_WY_rows_selected, "barrier"]
        print(reactiveValuesToList(global))
      })


  # data table output UI
  output$table_in_WY_UI <- renderUI({
    DT::dataTableOutput("table_in_WY")
  })


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
