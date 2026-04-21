golem_init_wrap <- function(
    pkg_nm_in,
    pkg_title_in,
    repo_url_in,
    authors_in=default_AUTHORS){
  
  golem::fill_desc(
    pkg_name = pkg_nm_in, # The name of the golem package containing the app (typically lowercase, no underscore or periods)
    pkg_title = pkg_title_in, # What the Package Does (One Line, Title Case, No Period)
    pkg_description = "PKG_DESC.", # What the package does (one paragraph).
    authors = authors_in,
    repo_url = NULL, # The URL of the GitHub repo (optional),
    pkg_version = "0.0.0.9000", # The version of the package containing the app
    set_options = TRUE # Set the global golem options
  )
  
  # golem::use_favicon("inst/app/www/favicon.png",ext="png")  
  
  add_pkg_v <- c("fresh","shinydashboard","shinydashboardPlus","bslib","markdown")
  sapply(1:length(add_pkg_v), function(ii)   usethis::use_package(add_pkg_v[ii]) )
  message("adding packages:\n ",paste(add_pkg_v,collapse = ","))
  # message("specifying all golem defaults")
  
}

default_AUTHORS <- c(
  person(
    given = "Steven", # Your First Name
    family = "Whitlock", # Your Last Name
    email = "swhit@uw.edu", # Your email
    role = c("aut", "cre") # Your role (here author/creator)
  ),
  person(
    given = "Rebecca", # Your First Name
    family = "Buchannan", # Your Last Name
    email = "rabuchan@uw.edu", # Your email
    role = c("aut") # Your role (here author/creator)
  ))



