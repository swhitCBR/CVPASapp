# generate predictions and plot from HOR_TCJ glmmTMB models

library(devtools)
load_all()

############################# #
# Survival probability
############################# #

########## #
# HOR-CHP
########## #
HOR_CHP_pred_tab <- HOR_CHP_mod_wrap(flength_in = 244,years_in = 2012,DOY_in = 1:250)

########## #
# HOR-TCJ
########## #
HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                                     HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
                                     flength_in=244) 
########## #
# TCJ-CHP
########## #
TCJ_CHP_pred_tab <- TCJ_CHP_mod_wrap(TCJ_CHP_pred_comp_ls_in=TCJ_CHP_pred_comp_ls,
                                     flength_in=244)

head(TCJ_CHP_pred_tab)

############################# #
# Route-Use probability
############################# #


############ #
### HOR
############ #

HOR_pred_tab <- HOR_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                             HOR_mod_ls=glmmTMB_mod_ls[["HOR"]])

############ #
### TCJ
############ #


TCJ_mod_d2_ls <- readRDS("../CVPAS_STH_app/output/TCJ_d2_mods.rds")
# str(TCJ_mod_d2_ls)
# TCJ_mod_d2_ls$TCJ_d2_mods[[1]]$frame
# TCJ_mod_d2_ls$TCJ_d2_modsZ[[1]]$frame


############################# #
# Compilation
############################# #

pred_tab_ls<- list(
  # "TCJ"=HOR_pred_tab,
  "HOR"=HOR_pred_tab,
  "HOR_TCJ_pred_tab"=HOR_TCJ_pred_tab,
  "HOR_CHP"=HOR_CHP_pred_tab,
  "TCJ_CHP"=TCJ_CHP_pred_tab
)

