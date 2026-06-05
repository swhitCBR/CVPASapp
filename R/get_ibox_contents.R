#' 
#' 
#' @details lookup content to place in box or well panel. If name is not recognized than an HTML paragraph `<p> placeholder </p>` is used
#' 
#' @param box_content_in specific text referring to a predefined HTML or shiny-renderable objected in a named list
#'
#' @returns HTML object displayed to display when information button 'ibutt' is clicked or in the help
#' 
#'
#' @export
#' @examples
#' 
#' 
get_ibox_contents <- function(box_content_in="daily_values_box"){
  if(box_content_in=="daily_values_box"){
   return(
   tags$dl(
      div(
        style = "margin-left: 20px;",
        HTML('<strong>log(VNS):</strong> Discharge at Vernalis gauging station(natural log scale)</p>'),
        HTML('<strong>OUT:</strong> Daily index of Delta outflow from Dayflow data series</p>'),
        HTML('<strong>MID:</strong> Discharge of Middle River measured at Bacon Island </p>'),
        HTML('<strong>ORB:</strong> Discharge of Old River measured at Bacon Island </p>'),
        HTML('<strong>OMT:</strong> Index of interior Delta flow north of export facilities (ORB + MID) </p>'),
        HTML('<strong>CVP:</strong> Water exports from federal Central Valley Project</p>'),
        HTML('<strong>SWP:</strong> Water exports from state water project</p>'),
        HTML('<strong>EXPORTS:</strong> Water exports combined (SWP + CVP)</p>'),
        HTML('<strong>CLC:</strong> Temperature measured at Mossdale Bridge gauging station</p>'),
        HTML('<strong>MSD:</strong> Temperature measured at Cliffton Court Forebay </p>')
      )
     )
    )
  }

    if(box_content_in=="ann_summ_tab_ibutt_content"){
    return(
      # definition list
              tags$dl(
                  div(
                    style = "margin-left: 20px;",
                    HTML('<strong>Year:</strong> Calendar Year</p>'),
                    HTML('<strong>Category:</strong> Water Year Type (SJ)</p>'),
                    HTML(
                      '<strong>HOR Barrier:</strong> Barrier at Head of Old River</p>'),
                    HTML(
                      '<strong>Model:</strong> Used to fit statistical models</p>'
                    )
                  )
                )
              )
            }

  if(box_content_in=="basic_route_schematic_ibox_content"){
   return(
    p("Major routes and key junctions in the Delta")
   )
  }
  else{
    p("placeholder")
  }
  }