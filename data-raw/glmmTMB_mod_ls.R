## code to prepare `glmmTMB_mod_ls` dataset goes here


########################## #
# Saving glmmTMB models
########################## #

HOR_mod_d2_ls <- readRDS("../CVPAS_STH_app/output/HOR_d2_mods_ls.rds")
TCJ_mod_d2_ls <- readRDS("../CVPAS_STH_app/output/TCJ_d2_mods.rds")
HOR_TCJ_mod_d2_ls <- readRDS("../CVPAS_STH_app/output/HOR_TCJ_d2_mods.rds")


glmmTMB_mod_ls <- list("HOR_TCJ"= HOR_TCJ_mod_d2_ls,
                       "TCJ"= TCJ_mod_d2_ls,
                       "HOR"= HOR_mod_d2_ls)
names(glmmTMB_mod_ls)


usethis::use_data(glmmTMB_mod_ls, overwrite = TRUE)
