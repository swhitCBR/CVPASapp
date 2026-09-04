ggplot_doy_env_plt_MOD <- function(
    CVhelp_dat_l_plt=CVhelp_dat_l,
    doy_rng_in=c(10,100),
    pst_year_in="2012",
    sub_var_in="VNS",
    show_bg_lines=TRUE,
    WYT_cols_in=utils_get_WYT_cols_vec()
)
{
  
  CVhelp_dat_l_plt <- subset(CVhelp_dat_l_plt,variable==sub_var_in)
  
  CVhelp_dat_l_plt$site <- CVhelp_dat_l_plt$variable
  CVhelp_dat_l_plt$var <- CVhelp_dat_l_plt$variable
  CVhelp_dat_l_plt$year <- CVhelp_dat_l_plt$Year
  
  doy_int1 <- doy_rng_in[1]
  doy_int2 <- doy_rng_in[2]
  CVhelp_dat_l_plt$doy_int1 <- doy_int1
  CVhelp_dat_l_plt$doy_int2 <- doy_int2
  CVhelp_dat_l_plt$SELECTED <- (
    CVhelp_dat_l_plt$DOY >= doy_rng_in[1] & 
      CVhelp_dat_l_plt$DOY <= doy_rng_in[2] & 
      CVhelp_dat_l_plt$Year==pst_year_in
  )
  CVhelp_dat_l_plt$in_year <- CVhelp_dat_l_plt$Year==pst_year_in
  
  plt_tmp <- ggplot2::ggplot() +
    ggplot2::facet_wrap(~site) +
    ggplot2::geom_line(
      data = if(show_bg_lines) subset(CVhelp_dat_l_plt,!Year %in% c("2011","2012","2013","2014","2015","2016")) else NULL,
      ggplot2::aes(y=value,x=DOY,group=year),
      # color="black",
      linewidth=0.25
    ) +
    # ggplot2::geom_line(
    #   data = if(!pst_year_in %in% c("2011","2012","2013","2014","2015","2016")) subset(CVhelp_dat_l_plt,year==pst_year_in) else NULL,
    #   # data = if(!pst_year_in %in% c("2011","2012","2013","2014","2015","2016")) subset(CVhelp_dat_l_plt,year==pst_year_in) else NULL,
    #   ggplot2::aes(y=value,x=DOY,group=year),
    #   color="black",
    #   linewidth=1
    # ) +
    ggplot2::geom_line(
      data = CVhelp_dat_l_plt |> dplyr::filter(Year %in% c("2011","2012","2013","2014","2015","2016")),
      ggplot2::aes(
        x = DOY,
        y = value,
        alpha = SELECTED,
        group = year)) +
    ggplot2::geom_vline(xintercept = doy_int1) +
    ggplot2::geom_vline(xintercept = doy_int2) +
    ggplot2::geom_point(
      data = CVhelp_dat_l_plt,# |> dplyr::filter(Year %in% c("2011","2012","2013","2014","2015","2016")),
      # data = CVhelp_dat_l_plt |> dplyr::filter(Year %in% c("2011","2012","2013","2014","2015","2016")),
      ggplot2::aes(
        x = DOY,
        y = value,
        fill = WYT,
        alpha = SELECTED,
        group = year),
      shape = 21) +
    ggplot2::scale_alpha_manual(values = c(0.2, 0.9)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::labs(x = "Day of Year") + 
    ggplot2::guides(alpha = "none") +
    ggplot2::theme(legend.position = c(0.9,0.8)) +
    ggplot2::scale_fill_manual(values=WYT_cols_in) +
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size=16),
                   axis.text.x = ggplot2::element_text(size=14),
                   axis.text.y = ggplot2::element_text(size=14))
  # axis.title.x = ggplot2::element_text(size=16)
  
  #reordering+
  
  
  # +      ggplot2::scale_fill_manual(values=WYT_cols[c(5,1,2,4,3)]) #reordering
  
  print(plt_tmp)
  
}