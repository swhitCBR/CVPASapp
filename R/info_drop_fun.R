info_drop_fun <- function(inputId_in = "daily_var_def_info_bttn2"){
  shinyWidgets::dropMenu(
    shinyWidgets::circleButton(
      inputId = inputId_in,
      icon = icon("info"),
      status = "primary",
      size = "xs"
    ),
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
    ),
    placement = "left-start"
  )
}