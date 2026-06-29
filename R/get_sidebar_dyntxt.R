#' Title
#'
#' @param tab_name_in
#'
#' @returns
#'
#' @export
#' @examples
#' 
get_sidebar_dyntxt <- function(tab_name_in){
  tagList(
    switch(
    tab_name_in,
    "about" = "Click tabs on the sidebar to navigate the inputs options and reference material.",
    "inputs" = "Select input data based on previous years or upload your own data set",
        "check" = "Verify that selected values are comparable to data used to fit models",
    "estimates" = "Click tabs on the sidebar to navigate the inputs options and reference material.",
        "overall" = "Survival probability estimates from starting location to Chipps Island accross all routes",
        "route_spec_surv" = "Survival probability estimates for along particular routes",
        "route_usage" = "Route usage probability at key junctions",
        "more_info" = "Additional details on estimates and statistical models"
    )
  )
}


# #' Title
# #'
# #' @param tab_name_in
# #'
# #' @returns
# #'
# #' @export
# #' @examples
# get_sidebar_txt_content <- function(tab_name_in){
#   p(paste0("I am '",tab_name_in,"' text"))
# }




# #' Title
# #'
# #' @returns
# #'
# #' @export
# #' @examples
# #' 
# #' get_sidebar_txt
# #' 
# get_sidebar_txt <- function(){
#   tagList(
#     conditionalPanel(
#       condition = "input.tabs == 'inputs'",
#       get_sidebar_txt_content("inputs")
#       # p("I am 'inputs' text")
#     # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
#     )
#     ,
#     conditionalPanel(
#       condition = "input.tabs == 'check'",
#       p("I am 'check' text")
#     # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
#     )
#     ,
#     conditionalPanel(
#       condition = "input.tabs == 'estimates'",
#       p("I am 'estimates' text")
#     # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
#     )
#     ,
#     conditionalPanel(
#       condition = "input.tabs == 'met_ref'",
#       p("I am 'met_ref' text")
#     # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
#     )
#     ,
#     conditionalPanel(
#       condition = "input.tabs == 'about'",
#       p("I am 'about' text")
#     # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
#     )
# )
# }
