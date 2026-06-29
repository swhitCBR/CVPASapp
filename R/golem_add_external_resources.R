#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    # favicon(resources_path = "app/www"),
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CVPAS_v0.9"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    #added external JS and CSS files beyond files within www/ folder
    shinyjs::useShinyjs(),
    # # External library  for js cookies (added by CO)
    # tags$script(
    #   src = "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/3.0.1/js.cookie.min.js"
    # )
  )
}
