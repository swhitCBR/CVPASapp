golem::install_dev_deps()
# usethis::use_mit_license("Golem User") # You can set another license here
golem::use_readme_rmd(open = FALSE,overwrite = TRUE)
golem::use_favicon("inst/app/www/favicon.ico")
devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)
devtools::build_readme()
