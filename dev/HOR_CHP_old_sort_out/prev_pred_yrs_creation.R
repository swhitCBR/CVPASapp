# generate predictions and plot from HOR_TCJ glmmTMB models

library(devtools)
load_all()

############################# #
# Survival probability
############################# #

########## #
# HOR-CHP
########## #
HOR_CHP_pred_tab <- HOR_CHP_mod_wrap(flength_in = 244)#,years_in = 2012,DOY_in = 1:250)

########## #
# HOR-TCJ
########## #
HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                                     HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
                                     flength_in=244
                                     ) 

########## #
# TCJ-CHP
########## #
TCJ_CHP_pred_tab <- TCJ_CHP_mod_wrap(TCJ_CHP_pred_comp_ls_in=TCJ_CHP_pred_comp_ls,
                                     flength_in=244)

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

TCJ_pred_tab <- TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
 # flength_in = 200,
 # DOY_in = 10:12,
 #  years_in=2018,
  TCJ_mod_ls=glmmTMB_mod_ls[["TCJ"]])


############################# #
# Compilation
############################# #

pred_tab_ls<- list(
  "TCJ"=TCJ_pred_tab,
  "HOR"=HOR_pred_tab,
  "HOR_TCJ_pred_tab"=HOR_TCJ_pred_tab,
  "HOR_CHP"=HOR_CHP_pred_tab,
  "TCJ_CHP"=TCJ_CHP_pred_tab
)

sapply(pred_tab_ls,nrow)
