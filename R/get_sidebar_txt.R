#' Title
#'
#' @param tab_name_in
#'
#' @returns  a `shiny.tag` with text
#'
#' @export
#' @examples
#' get_sidebar_txt("about")
#' 
get_sidebar_txt <- function(tab_name_in){
  tagList(
    switch(
    tab_name_in,
    "about" = "Click tabs on the sidebar to navigate the inputs options and reference material.",
    "inputs" = "Select input data based on previous years or upload your own data set",
        "check" = "Verify that selected values are comparable to data used to fit models",
    "estimates" = "Click tabs on the sidebar to navigate the inputs options and reference material.",
        "overall_surv" = "Survival probability estimates from starting location to Chipps Island accross all routes",
        "reach_surv" = "Survival probability for between key junctions",
        "route_spec_surv" = "Survival probability estimates for along particular routes",
        "route_usage" = "Route usage probability at key junctions",
        "more_info" = "Additional details on estimates and statistical models"
    )
  )
}

