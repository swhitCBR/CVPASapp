#' Title
#'
#' @param HOR_TCJ_pred_tab_plt CVhelp_dat_w data wide-format with estimates and possibly standard errors
#' @param doy_rng_in range of days of year to include
#' @param pst_year_in previous water year by which to subset the data
#'
#' @returns
#' @export
#'
#' @examples
ggplot_doy_pred_plt <- function(
    HOR_TCJ_pred_tab_plt,
    doy_rng_in=c(10,100),
    pst_year_in="2012"#,
    # disp_SE=TRUE
){
  doy_int1 <- doy_rng_in[1]
  doy_int2 <- doy_rng_in[2]
  
  print(str(HOR_TCJ_pred_tab_plt))
  HOR_TCJ_pred_tab_plt <- subset(HOR_TCJ_pred_tab_plt,Year==pst_year_in)
  HOR_TCJ_pred_tab_plt$SELECTED  <- HOR_TCJ_pred_tab_plt$DOY >= doy_int1 & HOR_TCJ_pred_tab_plt$DOY <= doy_int2
  
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,ymin=plogis(LCL),ymax=plogis(UCL)),fill="gray70") +
    ggplot2::geom_line(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,y=plogis(lo_pred))) +
    ggplot2::geom_point(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,y=plogis(lo_pred),fill=SELECTED),shape=21) +
    #ggplot2::facet_wrap(~Year) + 
    ggplot2::geom_vline(xintercept = doy_int1) +
    ggplot2::geom_vline(xintercept = doy_int2) +
    ggplot2::scale_fill_manual(values = c("darkgray", "#428BCA")) +
    ggplot2::scale_color_manual(values = c("gray40", "#28547A")) + 
    ggplot2::theme(legend.position="none")+
    # ggplot2::labs(x="",y="") + 
    ggplot2::scale_y_continuous(breaks=c(0,0.2,0.5,0.8)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01))
}