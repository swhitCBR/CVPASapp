
draw_chk_input_div_ui <- function(){
  div(
        id="checkTop",
  conditionalPanel(
         condition = "input.data_source_picker != 'None'",
    div(
      id="checkTop",
      column(
        width = 12,
        style = "margin-bottom: 20px",
        span(
          style = "display:flex; justify-content: space-between;padding-left: 10px;padding-right: 10px;    border-bottom: solid 1px gray; align-items:center",
          h4(strong("Check Inputs")),
          shinyWidgets::dropMenu(
            shinyWidgets::circleButton(
              inputId = "btn3",
              icon = icon("info"),
              status = "primary",
              size = "xs"
            ),
            p(
              "Examines user-provided input data to the range of observed value in the Six-Year Study"
            ,
            width="300px")
            ,
            placement = "left-start"
          )
        ),
        column(
          style = "margin-top:10px;",
          width = 6,
          div(
            p(
              "Verify that specified values conform with data used to fit statistical sub-models"
            )
          ),
          div(
            style = "display: inline-flex",

            actionButton(
              "check_inputs_butt",
              "Check Inputs",
              style = "color: white; background: #024c63; padding: 10px"
            ), 
            div(
              style = "margin-left: 20px; align-content: flex-end;", # 2nd one for vert align
              shinyjs::disabled(
                shinyWidgets::prettyCheckbox(
                  inputId = "inputs_check", #"inputs_check_pretty",
                  value = FALSE,
                  label = "Valid",
                  icon = icon("check")
                )
              )
            )
          ),
          div(
            style = "margin-top: 20px",
            shinyjs::disabled(
              actionButton(
                "generate_ests_butt",
                "Generate Estimates",
                style = "color: white; background: #024c63; padding: 10px"
              )
            )
          )
        ),

        column(
          style = "margin-top:10px;",
          width = 6,
          # actionButton("load_butt", "Load"),
          actionButton("reset_butt", "Reset"),
          div(
            style = "display:block;margin-top:10px;",
            wellPanel(
              # textOutput("glob_in_ls_text"),
              tags$style(
                type = "text/css",
                "#glob_in_ls_text {white-space: pre-wrap;}"
              )
            )
          )
        )
      )
    )
  )
)
}