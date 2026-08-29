ggplot_doy_pred_plt_single <- function(
    data_in,
    doy_rng_in=c(1,250),
    pst_year_in="2012",
    param_in#=#"S_HOR_CHP"
){
  
  stopifnot(is.character(param_in) & length(param_in)==1)
  stopifnot(param_in %in% unique(data_in$param))
  
  data_in <- data_in |> dplyr::filter(param==param_in)
  
  doy_int1 <- doy_rng_in[1]
  doy_int2 <- doy_rng_in[2]
  data_in <- subset(data_in,Year==pst_year_in)
  data_in$SELECTED  <- data_in$DOY >= doy_int1 & data_in$DOY <= doy_int2
  
  # DEBUG MODE
  # omitting this line can be thought of as activating a debug mode of sorts
  data_in <- data_in[data_in$DOY >= doy_int1 & data_in$DOY <= doy_int2,]

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=data_in,ggplot2::aes(x=DOY,ymin=pLCL,ymax=pUCL),fill="gray70") +
    ggplot2::geom_line(data=data_in,ggplot2::aes(x=DOY,y=pr_pred)) +
    ggplot2::geom_point(data=data_in,ggplot2::aes(x=DOY,y=pr_pred,fill=SELECTED),shape=21)+#,color="gray40") +
    ggplot2::scale_fill_manual(values = c("darkgray", "#428BCA")) +
    ggplot2::scale_color_manual(values = c("gray40", "#28547A")) +
    ggplot2::theme(legend.position="none")+
    ggplot2::scale_y_continuous(breaks=c(0,0.2,0.5,0.8,1),limits=c(0,1)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::labs(y=param_in,x="Day of Year") + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(size=14),axis.text.y = ggplot2::element_text(size=14),
                   axis.title.x = ggplot2::element_text(size=16),axis.title.y = ggplot2::element_text(size=16))
  
  
}