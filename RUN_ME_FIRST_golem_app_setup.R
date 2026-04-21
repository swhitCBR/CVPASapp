####################################################################################### #
# IMPORTANT! This script should be opened in the repository created in step 01  
####################################################################################### #

# Load functions for customizing the initialization of the golem shiny package
source("R/utils_golem_init_wrap.R")

# Name and title the shiny application
app_title <- "CVPASapp"

golem_init_wrap(
  pkg_nm_in=golem::pkg_name(),
  pkg_title_in=app_title,
  repo_url_in="https://github.com/swhitCBR/CVPASapp")

source("dev/01_start.R")

devtools::load_all()
run_app()

########################################################### #
# redefining the default golem starting script, which
# applies authors, licenses, and such and is only intended to be run once
########################################################### #


# alter dashboard theme
# golem::add_utils("CBRtheme",open = FALSE)
add_CBRtheme()

########################################################### #
# modify appUI so that a pretty-looking dashboard appears
########################################################### #
# adding the dashboad utility functions
# golem::add_utils("ui")
# rewrite_utils_ui()
# # used to apply template defined by CO
# usethis::use_package("fresh")
# usethis::use_package("shinydashboard")
# usethis::use_package("shinydashboardPlus")
# usethis::use_package("bslib") # upgrades shiny to bootstrap 4 design objects

# runApp(display.mode = "showcase")
devtools::document()
devtools::load_all()
run_app()

########################################################## #
# Deploying as a Posit-hosted app
# assuming that a shiny.io account is up and running
########################################################## #

golem::add_positconnect_file()
# details on hoiw packrat works
# https://docs.posit.co/connect/admin/r/package-management/
options(rsconnect.packrat = TRUE)
rsconnect::deployApp()

