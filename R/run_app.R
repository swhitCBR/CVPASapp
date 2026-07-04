#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  # start_tab = "about",
  start_tab = "inputs",
  inputs_box_collapsed = FALSE,
  ...
) {
  
  # sourcing static objects used by the app
  source(system.file("global.R",package = "CVPASapp"))
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      start_tab=start_tab,
      inputs_box_collapsed=inputs_box_collapsed,
      ...)
  )

  
}
