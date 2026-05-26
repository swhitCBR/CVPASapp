#' Title
#'
#' @returns
#' @export
#'
#' @examples
draw_upload_deet_ui <- function(){
  tags$details(
    id = "details_up",
    # open=ifelse(
    #   input$data_source_picker=="Uploaded file (.csv)",TRUE,NULL),
    style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white; ",
    tags$summary(
      title = "Click to open or close",
      "Load Daily Environmental Data", # "Upload/Review Daily Environmental Data",
      style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
    ),
    tagList(
      fluidRow(
        style = "padding: 15px;",
        column(
          width = 5,
          fileInput(
            inputId = "file1",
            label = "Choose CSV File",
            # buttonLabel="aaa",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          shiny::fluidRow(
            style = "margin-top:20px",
            column(
              width = 4,
              div(
                role = "menuitem",
                actionButton(
                  inputId = "run_button",
                  label = "Load"
                )
              )
            ),
            column(
              width = 4,
              div(
                role = "menuitem",
                actionButton(
                  inputId = "reset_button",
                  label = "Reset"
                )
              )
            ),
            column(
              width = 4,
              downloadButton(
                outputId = "download_sample",
                label = "Sample CSV",
                icon = shiny::icon("download")
              )
            )
          ),
          div(
            style = "padding-top: 20px;",
            role = "menu",
            div(
              # style = "display: flex; gap:20px;",
              shinyWidgets::dropMenu(
                placement = "bottom",
                tag = actionButton(
                  inputId = "man_input_dropdown",
                  label = tags$div(
                    HTML('<i class="fas fa-edit" role="presentation" aria-label="sliders icon"></i> Manual Input')),
                ),
                textAreaInput(
                  inputId = "manual_input",
                  label = "Manual Input",
                  placeholder = 'year,date,WYT,barrier,VNS,OMT,T_MSD,T_CLC,CVP,SWP\n2013,"2013-04-29","Out",4130,-623,18.2,20.6,816,2421',
                  rows = 3,width="500px"
                )
              )
            )
          )
        ),
        column(
          width = 7,
          div(
            tags$ul(
              style = "padding-inline-start: 10px;",
              tags$li(
                h5(
                  "[[Datatable placeholder]]"
                )
              )
            )
            # ,
            # draw_info_bttn_dropdown_ui(inputID_in="daily_var_def_info_bttn2")
          )
        )
      )
    )
  )
}