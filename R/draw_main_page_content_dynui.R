  #' Title
  #'
  #' @param input_tab_in
  #'
  #' @returns ui output based on `uiOutput` [htmltools::tagList]
  #'
  #' @export
  #' 
  #' @examples
  #' draw_main_page_content_dynui()
  #' 
  draw_main_page_content_dynui <- function(input_tab_in= "about",init_data_source_in){
      tagList(
        conditionalPanel(
         condition = "input.tabs == 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'",
          draw_inputs_box_ui(init_data_source_in=init_data_source_in),
          draw_est_box_ui(),
         )
         ,
      switch(
        input_tab_in,
        "about" = shiny::tagList(
        draw_about_page_ui()
           ),
        "met_ref" = shiny::tagList(
        uiOutput("met_ref_page_ui")
      ))
    )
  }