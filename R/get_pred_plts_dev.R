

get_pred_plts_dev <- function(pred_pDF_comb_in){
  
  pred_pDF_comb_in <- pred_pDF_comb_in |>
    dplyr::mutate(param=factor(param,
                               levels=c("S_HOR_CHP","HOR_TCJviaSJL","S_TCJ_CHP","HOR_CHPviaSJL","HOR_CHPviaORE","TCJ_CHPviaMAC","TCJ_CHPviaTRN","HOR","TCJ")))
  
  pred_pDF_comb_tmp <- pred_pDF_comb_in
  ggplot2::ggplot(data=pred_pDF_comb_tmp, #|> #dplyr::filter(Year==2011),
                  ggplot2::aes(y=pr_pred,x=DOY,ymin=pLCL,ymax=pUCL,color=type)) +
    ggplot2::facet_grid(Year~param) + 
    ggplot2::geom_ribbon(fill="gray",color=NA) +
    ggplot2::geom_line() + 
    ggplot2::geom_hline(yintercept = 0.5,linetype="dotted") #+
  # ggplot2::geom_hline(yintercept = c(0.25,0.75),linetype="dotted")
  
}
