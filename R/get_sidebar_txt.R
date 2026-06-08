#' Title
#'
#' @param tab_name_in
#'
#' @returns
#'
#' @export
#' @examples
#' 
get_dyn_sidebar_txt <- function(tab_name_in){
  tagList(
    switch(
    tab_name_in,
    "inputs" = "hello inputs",
    "about" = "hello about"
    )
  )
}

#' Title
#'
#' @param tab_name_in
#'
#' @returns
#'
#' @export
#' @examples
get_sidebar_txt_content <- function(tab_name_in){
  p(paste0("I am '",tab_name_in,"' text"))
}




#' Title
#'
#' @returns
#'
#' @export
#' @examples
#' 
#' get_sidebar_txt
#' 
get_sidebar_txt <- function(){
  tagList(
    conditionalPanel(
      condition = "input.tabs == 'inputs'",
      get_sidebar_txt_content("inputs")
      # p("I am 'inputs' text")
    # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
    )
    ,
    conditionalPanel(
      condition = "input.tabs == 'check'",
      p("I am 'check' text")
    # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
    )
    ,
    conditionalPanel(
      condition = "input.tabs == 'estimates'",
      p("I am 'estimates' text")
    # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
    )
    ,
    conditionalPanel(
      condition = "input.tabs == 'met_ref'",
      p("I am 'met_ref' text")
    # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
    )
    ,
    conditionalPanel(
      condition = "input.tabs == 'about'",
      p("I am 'about' text")
    # 'inputs' || input.tabs == 'check' || input.tabs == 'estimates'"'met_ref' 'about'
    )
)
}
