ggplot_doy_ins_plt <- function(
    CVhelp_dat_l_plt=CVhelp_dat_l,
    doy_rng_in=c(10,100),
    pst_year_in="2012",
    # ,
    sub_var_in=c("VNS","OUT","EXPORTS")
)
{
  
  # CVhelp_dat_l_plt <- subset(CVhelp_dat_l_plt,variable %in% c(sub_var_in))
  
  CVhelp_dat_l_plt$site <- CVhelp_dat_l_plt$variable
  CVhelp_dat_l_plt$var <- CVhelp_dat_l_plt$variable
  CVhelp_dat_l_plt$year <- CVhelp_dat_l_plt$Year
  
  doy_int1 <- doy_rng_in[1]
  doy_int2 <- doy_rng_in[2]
  CVhelp_dat_l_plt$doy_int1 <- doy_int1
  CVhelp_dat_l_plt$doy_int2 <- doy_int2
  CVhelp_dat_l_plt$SELECTED <- (CVhelp_dat_l_plt$DOY >= doy_rng_in[1] & 
                                  CVhelp_dat_l_plt$DOY <= doy_rng_in[2] & 
                                  CVhelp_dat_l_plt$Year==pst_year_in
  )
  
  # sub_vars <- c("VNS","SWP","MSD","barrierTF")
  sub_vars <- c("VNS","SWP","MSD")#,"drought")
  # plt_tmp <- 
  plt_tmp <- ggplot2::ggplot() +
    ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,Year==pst_year_in & site %in% sub_vars & !SELECTED & DOY<=doy_int1),
                       ggplot2::aes(y=value,x=DOY,color=site),alpha = 0.4) +
    ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,Year==pst_year_in & site %in% sub_vars & !SELECTED & DOY>=doy_int2),
                       ggplot2::aes(y=value,x=DOY,color=site),alpha = 0.4) +
    ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,Year==pst_year_in & site %in% sub_vars & SELECTED),
                        ggplot2::aes(y=value,x=DOY,color=site),alpha=1) +
    ggplot2::facet_wrap(~site,scales = "free_y",ncol=1) + ggplot2::theme(strip.position="right") +
    
    ggplot2::geom_vline(xintercept = doy_int1) +
    ggplot2::geom_vline(xintercept = doy_int2) +

    ggplot2::scale_alpha_manual(values = c(0.2, 0.9)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::labs(x = "Day of Year") +
    ggplot2::guides(alpha = "none")# +
    # ggplot2::theme(legend.position = c(0.9,0.8))# +
    # ggplot2::scale_fill_manual(values=WYT_cols[c(5,1,2,4,3)]) #reordering
  
  print(plt_tmp)
  
}