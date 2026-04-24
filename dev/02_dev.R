# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################


golem::add_positconnect_file()
# details on hoiw packrat works: https://docs.posit.co/connect/admin/r/package-management/
options(rsconnect.packrat = TRUE)
rsconnect::deployApp()
# trying to gt this to run
devtools::build()

usethis::use_package("covrpage") # didn'twork before install_github()
devtools::install_github("https://github.com/yonicd/covrpage")
# devtools::install_github("rhub")
install.packages("rhub")
# updated rsconnect based on recommendation here in golem issues page:https://github.com/ThinkR-open/golem/issues/1136
devtools::document()
# Engineering
# ran this which is part of the dev02 default
attachment::att_amend_desc()
# got a "error. check logs" message
#on shiny.io log said no package called "markdown" so ran
usethis::use_package("markdown")
devtools::document()
usethis::use_package("dplyr")

################################################################################ #

options(rsconnect.packrat = TRUE)
rsconnect::writeManifest()

################################################################################ #

# adding CVhelp code to package
devtools::install_github("https://github.com/swhitCBR/CVhelp",auth_token = "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")
# creating static data sets for CVhelp


devtools::install_github("https://github.com/swhitCBR/CVhelp",auth_token = "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")


################################################################################ #

devtools::install_github("https://github.com/swhitCBR/CVhelp",auth_token = "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")

# get_ann_HORbar_WYT_data <- function(){
#     HOR_bar_ann_data <- data.frame(CVhelp::get_HOR_barrier_data(dt_rng = c("2011-01-01", "2024-12-31")) %>% 
#                 dplyr::mutate(year=format(date,"%Y"),
#                             Year=as.numeric(year)) %>%
#                 dplyr::group_by(Year) %>% 
#                 dplyr::summarize(barrier_days=sum(barrierTF),
#                                 barrier=ifelse(sum(barrierTF)>0,"In","Out"))) #%>%
    
#     WYT_data <- CVhelp:::get_WYT_data(dt_rng=c("2011-01-01","2024-12-31")) #%>% rename(year=Year)# local access
#     ann_HORbar_WYT_data <- HOR_bar_ann_data %>% dplyr::left_join(WYT_data,by="Year")
#     return(ann_HORbar_WYT_data)}

# shiny::actionButton("refresh_ann_data_butt","refresh")
# shiny::observeEvent(input$refresh_ann_data_butt,{
#   
#   })

# devtools::load_all("../CVhelp")
ann_HORbar_WYT_data <- CVhelp::get_ann_HORbar_WYT_data()
CVhelp_dat_l <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "long")
CVhelp_dat_w <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide")

usethis::use_data(ann_HORbar_WYT_data,overwrite = T)
usethis::use_data(CVhelp_dat_l,overwrite = T)
usethis::use_data(CVhelp_dat_w,overwrite = T)

devtools::document()


# usethis::use_package("DT")
# library(DT)
################################################################################ #

# relocacted "C:\repos\CVPAS_meta\utils_ui_SEEMINGLY UNUSED.R"
usethis::use_package("daterangepicker")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file("script")
# golem::add_js_handler("handlers")
# golem::add_css_file("custom") # already done
# golem::add_sass_file("custom")
# golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("CVPASapp")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
# usethis::use_coverage()
# 
# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
