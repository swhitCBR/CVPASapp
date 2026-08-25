# generate predictions and plot from HOR_TCJ glmmTMB models

library(devtools)
load_all()

# options(digits = 3,scipen = 99)
pred_tab_ls <- all_mod_preds(,DOY_in = 1:250,years_in = NULL,flength_in = 200)
pred_tab_ls2 <- all_mod_preds_fun2(pred_tab_ls_in = pred_tab_ls,predict_int = T,conf_level = 0.95)
# pred_tab_ls2$"beta_parm_df"
get_pred_plts_dev(pred_pDF_comb_in = pred_tab_ls2$"pred_pDF_comb")

TCJ_CHP_pred_comp_ls$AIC_DF_d2
TCJ_CHP_pred_comp_ls$TCJ_CHP_mod_fits_d2_ls
# TCJ_CHP_pred_comp_ls$TCJ_CHP_TMB_all_mods$pt_estsDF |> 
#   dplyr::filter(dm %in% c(TCJ_CHP_pred_comp_ls$AIC_DF_d2$dm)) |>
#   tidyr::pivot_wider(names_from = par_nm,values_from = S_coef)

TCJ_CHP_pred_comp_ls$S_conf_ls$S_coef_confset_DF
# kinda cool!
TCJ_CHP_pred_comp_ls$TCJ_CHP_TMB_all_mods$pt_estsDF |> 
  dplyr::filter(dm %in% c(TCJ_CHP_pred_comp_ls$AIC_DF_d2$dm)) |> 
  dplyr::select(-dmID,-S_parID) |>
  tidyr::pivot_wider(names_from = par_nm,values_from = S_coef)

TCJ_CHP_pred_comp_ls$S_conf_ls$S_coef_confset_DF |> 
  # dplyr::filter(dm %in% c(TCJ_CHP_pred_comp_ls$AIC_DF_d2$dm)) |> 
  dplyr::select(-S_parID,-AICrank,-candmodID,-estimate,-SE) |>
  tidyr::pivot_wider(names_from = par_nm,values_from = S_coef)

TCJ_CHP_pred_comp_ls$TCJ_CHP_TMB_all_mods$pt_estsDF |> 
  dplyr::filter(dm %in% c(TCJ_CHP_pred_comp_ls$AIC_DF_d2$dm)) |> 
  dplyr::select(-dmID,-S_parID) |>
  tidyr::pivot_wider(names_from = par_nm,values_from = S_coef) |>



# TCJ_CHP_pred_comp_ls$TCJ_CHP_mod_fits_d2_ls$`1111110010101`

############################################################# #
# Recreating periods in figures 6 and 9 of Buchanan (2024)
############################################################# #
tmpDOY <- as.numeric(format(as.Date("2011-05-01"),"%j")):as.numeric(format(as.Date("2011-05-05"),"%j"))
pred_tab_ls <- all_mod_preds(DOY_in = tmpDOY,years_in = 2011)
pred_tab_ls2 <- all_mod_preds_fun2(pred_tab_ls_in = pred_tab_ls,predict_int = T,conf_level = 0.95)
# pred_tab_ls2$"beta_parm_df"
get_pred_plts_dev(pred_pDF_comb_in = pred_tab_ls2$"pred_pDF_comb")

tmpDOY <- as.numeric(format(as.Date("2015-04-01"),"%j")):as.numeric(format(as.Date("2015-04-05"),"%j"))
pred_tab_ls <- all_mod_preds(DOY_in = tmpDOY,years_in = 2015)
pred_tab_ls2 <- all_mod_preds_fun2(pred_tab_ls_in = pred_tab_ls,predict_int = T,conf_level = 0.95)
# pred_tab_ls2$"beta_parm_df"
get_pred_plts_dev(pred_pDF_comb_in = pred_tab_ls2$"pred_pDF_comb")




# pred_tab_ls2$
# plot.new()
# matplot(pred_tab_ls2$beta_parm_df,type="l",add=T)
# abline(h=0)
table(apply(pred_tab_ls2$beta_parm_df,1,function(x) any(x<0)))


nrow(pred_tab_ls2$pred_pDF_comb_w)
# looking at beta dists
ind_pos <- which(apply(pred_tab_ls2$beta_parm_df,1,function(x) all(x>0)))
pred_tab_ls2$beta_parm_df[ind_pos,]

pred_tab_ls2$beta_parm_df[-ind_pos,]


pred_tab_ls2$pred_pDF_comb_w[ind_pos,]
pred_tab_ls2$pred_pDF_comb_w[-ind_pos,]



get_pred_plts_dev(pred_pDF_comb_in = pred_tab_ls2$"pred_pDF_comb")

pred_tab_ls2$overall_DF

head(pred_tab_ls2$overall_DF)
0.8*0.2
0.5*0.5

0.95*0.05
# ((1-0.2491214)*0.2491214)
# 0.4991207/((1-0.2491214)*0.2491214)



# head(pred_tab_ls2$overall_DF)

pred_tab_ls2$pred_pDF_comb

# pred_tab_ls2$


pred_tab_ls2 |> tidyr::pivot_longer(names(pred_tab_ls2)[!names(pred_tab_ls2) %in% c("Year","DOY")])

############ #
### HOR
############ #

HOR_pred_tab <- HOR_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                             HOR_mod_ls=glmmTMB_mod_ls[["HOR"]])

############ #
### TCJ
############ #

# TCJ_mod_wrap()

TCJ_pred_tab <- TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                             TCJ_mod_ls=glmmTMB_mod_ls[["TCJ"]])
########## #
# HOR-TCJ
########## #

HOR_TCJ_pred_tab <- HOR_TCJ_mod_wrap(sel_rows_tmp1 = CVhelp_dat_w,
                                     HOR_TCJ_mod_ls=glmmTMB_mod_ls[["HOR_TCJ"]],
                                     flength_in=244) 
########## #
# HOR-CHP
########## #

HOR_CHP_viaSJL_pred_tab <- HOR_CHP_mod_wrap(flength_in = 244,SJL_route_in = TRUE
                                     # years_in = 2012,
                                     # DOY_in = 1:250
                                     )

HOR_CHP_viaORE_pred_tab <- HOR_CHP_mod_wrap(flength_in = 244,SJL_route_in = FALSE
                                     # years_in = 2012,
                                     # DOY_in = 1:250
                                    )

########## #
# TCJ-CHP
########## #

TCJ_CHP_viaMAC_pred_tab <- TCJ_CHP_mod_wrap(#
                                     flength_in=244,SJL_route_in = TRUE)

TCJ_CHP_viaTRN_pred_tab <- TCJ_CHP_mod_wrap(
                                            flength_in=244,SJL_route_in = FALSE)

########################## #
# Combination
########################## #


pred_tab_ls<- list(
  "TCJ"=TCJ_pred_tab,
  "HOR"=HOR_pred_tab,
  "HOR_TCJviaSJL"=HOR_TCJ_pred_tab,
  "HOR_CHPviaSJL"=HOR_CHP_viaSJL_pred_tab,
  "HOR_CHPviaORE"=HOR_CHP_viaORE_pred_tab,
  "TCJ_CHPviaMAC"=TCJ_CHP_viaMAC_pred_tab,
  "TCJ_CHPviaTRN"=TCJ_CHP_viaTRN_pred_tab
  )



# sapply(pred_tab_ls,function(xx){"lo_pred" %in% names(xx)})
# sapply(pred_tab_ls,function(xx){"lo_SEadj" %in% names(xx)})
# sapply(pred_tab_ls,nrow)


pred_lp <- do.call(rbind,
  lapply(1:length(pred_tab_ls),function(ii){
    pred_lp <- data.frame(param=names(pred_tab_ls)[ii],pred_tab_ls[[ii]][c("Year","DOY","lo_pred","lo_SEadj")])
    }))

pred_pDF <- pred_lp |> 
  dplyr::mutate(
    type=ifelse(param %in% c("TCJ","HOR"),"route","survival"),
    loLCL=lo_pred-1.96*lo_SEadj,
    loUCL=lo_pred+1.96*lo_SEadj,
    pr_pred=plogis(lo_pred),
    pLCL=plogis(loLCL),
    pUCL=plogis(loUCL),
    p_SEadj=plogis(lo_pred)*lo_SEadj
    )

library(ggplot2)
# ggplot(data=pred_pDF,
#        aes(y=pr_pred,x=DOY,ymin=pLCL,ymax=pUCL,color=type)) +
#   facet_grid(Year~param) + 
#   geom_ribbon(fill="gray",color=NA) +
#   geom_line()
# 
# ggplot(data=pred_pDF |> dplyr::filter(Year==2011),
#        aes(y=pr_pred,x=DOY,ymin=pLCL,ymax=pUCL,color=type)) +
#   facet_grid(Year~param) + 
#   geom_ribbon(fill="gray",color=NA) +
#   geom_line()


# pred_lo_DF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
#   dplyr::select(Year,DOY,param,lo_pred) |> 
#   tidyr::pivot_wider(names_from=param,values_from = lo_pred) |> 
#   dplyr::rename(Psi_ORE=HOR,
#                 Psi_TRN=TCJ) |> 
#   dplyr::mutate(Psi_SJL=1-Psi_ORE,
#                 Psi_MAC=1-Psi_TRN) |>
#   dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
#   dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 


# pred_lo_SE_DF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
#   dplyr::select(Year,DOY,param,lo_SEadj) |> 
#   tidyr::pivot_wider(names_from=param,values_from = lo_SEadj) |> 
#   dplyr::rename(Psi_ORE=HOR,
#                 Psi_TRN=TCJ) |> 
#   dplyr::mutate(Psi_SJL=1-Psi_ORE,
#                 Psi_MAC=1-Psi_TRN) |>
#   dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
#   dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 



pred_pDF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
  dplyr::select(Year,DOY,param,pr_pred) |> 
  tidyr::pivot_wider(names_from=param,values_from = pr_pred) |> 
  dplyr::rename(Psi_ORE=HOR,
                Psi_TRN=TCJ) |> 
  dplyr::mutate(Psi_SJL=1-Psi_ORE,
                Psi_MAC=1-Psi_TRN) |>
  dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
  dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 


pred_pSE_DF_w1 <- pred_pDF |> #dplyr::filter(Year==2011 & DOY %in% 20:24) |> 
  dplyr::select(Year,DOY,param,p_SEadj) |> 
  tidyr::pivot_wider(names_from=param,values_from = p_SEadj) |> 
  dplyr::rename(Psi_ORE=HOR,
                Psi_TRN=TCJ) |> 
  dplyr::mutate(Psi_SJL=1-Psi_ORE,
                Psi_MAC=1-Psi_TRN) |>
  dplyr::select(-HOR_CHPviaSJL,-Psi_TRN,-Psi_ORE) |>
  dplyr::relocate(Year,DOY,Psi_SJL,HOR_TCJviaSJL,Psi_MAC,TCJ_CHPviaMAC,TCJ_CHPviaTRN,HOR_CHPviaORE) 


p_mat <- pred_pDF_w1[,which(!names(pred_pDF_w1) %in% c("Year","DOY"))]
pSE_mat <- pred_pSE_DF_w1[,which(!names(pred_pSE_DF_w1) %in% c("Year","DOY"))]

comb_out <- get_overall_surv(E_prop = p_mat,V_prop = pSE_mat^2,conf_level = 0.8)
overall_DF_full <- data.frame(pred_pDF_w1[,which(names(pred_pDF_w1) %in% c("Year","DOY"))],comb_out)
overall_DF <- overall_DF_full |> dplyr::select(Year,DOY,S_TCJ_CHP_mean,S_TCJ_CHP_LCL,S_TCJ_CHP_UCL,S_HOR_CHP_mean,S_HOR_CHP_LCL,S_HOR_CHP_UCL)

TCH_CHP_overall_DF <- overall_DF |> dplyr::select(Year,DOY,S_TCJ_CHP_mean,S_TCJ_CHP_LCL,S_TCJ_CHP_UCL) |> 
  dplyr::rename(pr_pred=S_TCJ_CHP_mean,pLCL=S_TCJ_CHP_LCL,pUCL=S_TCJ_CHP_UCL) |>
  dplyr::mutate(param="S_TCJ_CHP",type="overall_survival")

HOR_CHP_overall_DF <- overall_DF |> dplyr::select(Year,DOY,S_HOR_CHP_mean,S_HOR_CHP_LCL,S_HOR_CHP_UCL) |> 
  dplyr::rename(pr_pred=S_HOR_CHP_mean,pLCL=S_HOR_CHP_LCL,pUCL=S_HOR_CHP_UCL) |>
  dplyr::mutate(param="S_HOR_CHP",type="overall_survival")

pred_pDF_comb <- dplyr::bind_rows(pred_pDF,TCH_CHP_overall_DF,HOR_CHP_overall_DF)

pred_pDF_comb_w <- pred_pDF_comb |> 
  dplyr::select(Year,DOY,param,pr_pred) |> 
  tidyr::pivot_wider(names_from=param,values_from = pr_pred) |>
  dplyr::mutate(
    Psi_SJL=1-HOR,Psi_MAC=1-TCJ,S_TCJ_CHP_calc=TCJ*TCJ_CHPviaTRN+TCJ_CHPviaMAC*(1-TCJ))


ggplot(data=pred_pDF_comb |> dplyr::filter(Year==2011),
       aes(y=pr_pred,x=DOY,ymin=pLCL,ymax=pUCL,color=type)) +
  facet_grid(Year~param) + 
  geom_ribbon(fill="gray",color=NA) +
  geom_line()

ggplot(data=pred_pDF_comb, #|> #dplyr::filter(Year==2011),
       aes(y=pr_pred,x=DOY,ymin=pLCL,ymax=pUCL,color=type)) +
  facet_grid(Year~param) + 
  geom_ribbon(fill="gray",color=NA) +
  geom_line()

