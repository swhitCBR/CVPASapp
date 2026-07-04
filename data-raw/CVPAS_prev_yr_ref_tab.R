## code to prepare `CVPAS_prev_yr_ref_tab` dataset goes here

# reading in .csv table with previous year tables
CVPAS_prev_yr_ref_tab <- read.csv("inst/app/www/CVPAS_prev_yr_ref_tab.csv")

usethis::use_data(CVPAS_prev_yr_ref_tab, overwrite = TRUE)
