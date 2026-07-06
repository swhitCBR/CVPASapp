  #' Title
  #'
  #' @param input_tab_in input$tabs
  #' @param init_data_source_in data source object
  #'
  #' @returns a `shiny.tag` object inside a `shinydashboardPlus::box()``
  #'
  #' @export
  #' 
  #' @examples
  #' draw_inputs_box_ui(init_data_source="Previous years")
  #' 
draw_inputs_box_ui <- function(
  init_data_source_in,
  collapsed_in){
 
  shinydashboardPlus::box(
      id = "input_box2",
      title = span(h2("Inputs",style="margin-top: 0px; margin-bottom: 0px;"),id="input_box2_title"),
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      collapsed = collapsed_in,
        width = 12,
        draw_inputs_star_loc_datsrc_col_ui(init_data_source_in=init_data_source_in)
        ,
        draw_inputs_map_schematic_col_ui()
      ,
      # div(
      column(
        width = 12,
        style = "margin:10px;",
        tagList(
        tagList(
            conditionalPanel(
              condition = "input.data_source_picker == 'Previous year'",
              draw_details_sel_data_ui()
            ),
            conditionalPanel(
              condition = "input.data_source_picker == 'Uploaded file (.csv)'",
              draw_upload_details_ui()
          )
        )
          ,
          draw_indiv_attribs_details_ui()
        )
      )
      ,
      footer = 
        draw_chk_input_div_ui()
    )
}
