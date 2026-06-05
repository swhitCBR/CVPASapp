  #' Title
  #'
  #' @param input_tab_in
  #'
  #' @returns
  #'
  #' @export
  #' @examples
  # draw_main_page_content_dynui <- function(input_tab_in= input$tabs){
  #   switch(
  #     input_tab_in,
  #     "about" = shiny::tagList(
  #       mod_about_page_ui("mod_about_page-about_page_ui_1")
  #     ),
  #     "met_ref" = shiny::tagList(
  #       uiOutput("met_ref_page_ui")
  #     ),
  #     "inputs" = shiny::tagList(
  #        draw_inputs_panel_UI(),
  #        draw_estimates_panel_ui()
  #       #  uiOutput("estimates_panel_ui")      
  #     ),
  #     "estimates" = shiny::tagList(
  #       #  draw_inputs_panel_UI(),
  #       #  draw_estimates_panel_ui()
  #       #  uiOutput("estimates_panel_ui")      
  #       )
  #   )
  # }


    #' @examples
  draw_main_page_content_dynui <- function(input_tab_in= input$tabs){


      tagList(
        conditionalPanel(
                  #  condition = "input.tabs == 'inputs'",
         condition = "input.tabs == 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'",
        #  condition = "(typeof input.tabs !== 'undefined') && (input.tabs == 'inputs' || input.tabs == 'estimates')",
          draw_inputs_panel_ui(),
          draw_estimates_panel_ui(),
          # hr(id="estimatesTop")
         )
         ,
        # conditionalPanel(
        #  condition = "input.tabs == 'estimates'",
        # draw_estimates_panel_ui()
        #  )
        #  ,
      switch(
        input_tab_in,
        # "inputs" = shiny::tagList(
        #  draw_inputs_panel_ui(inputs_panel_collapse_in=inputs_panel_collapse_in)
        #    ),
        # "estimates" = shiny::tagList(
        #   # draw_inputs_panel_ui(),
        #  draw_estimates_panel_ui()
        #    ),
        "about" = shiny::tagList(
        mod_about_page_ui("mod_about_page-about_page_ui_1")
           ),
        "met_ref" = shiny::tagList(
        uiOutput("met_ref_page_ui")
      ))
      # )
    # } 
    # else{
    #   tagList(
    #      draw_inputs_panel_UI(),
    #      draw_estimates_panel_ui()
    #     )
    # }
    )

  }