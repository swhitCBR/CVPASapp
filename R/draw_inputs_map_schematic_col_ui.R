#' Title
#'
#'
#' @returns
#' @export
#'
#' @examples
#' draw_inputs_map_schematic_col_ui()
#' 
draw_inputs_map_schematic_col_ui <- function(){
  column(
    width = 5,
    tagList(
      div(
        style = "margin-left: 10px; margin-right: 10px;;margin-top: 10px;;margin-bottom: 10px;",
        div(
          title = "click for more information",
          id = "schem_info_drop_div",
          style = "float:right !important;
                  position: relative;
                  z-index: 2"
          ,
          draw_ibutt_dropdown_ui(
            inputId_in = "daily_var_def_ibutt2",
            ibox_content_label = "basic_route_schematic_ibox_content"
          )
        ),
        tagList(
        conditionalPanel(condition = "input.start_loc_in == 'TCJ' && output.BarStatus == 'In'",
          draw_basic_route_schematic_svg(
          LOC_in = "TCJ",
          BAR_in = "In"
        ))
        ,
        conditionalPanel(condition = "input.start_loc_in == 'TCJ' && output.BarStatus == 'Out'",
          draw_basic_route_schematic_svg(
          LOC_in = "TCJ",
          BAR_in = "Out"
        ))
        ,
        conditionalPanel(condition = "input.start_loc_in == 'HOR' && output.BarStatus == 'In'",
          draw_basic_route_schematic_svg(
          LOC_in = "HOR",
          BAR_in = "In"
        ))
        ,
        conditionalPanel(condition = "input.start_loc_in == 'HOR' && output.BarStatus == 'Out'",
          draw_basic_route_schematic_svg(
          LOC_in = "HOR",
          BAR_in = "Out"
        ))
      )
      ,
      )
    )
  )
}