ggplot_doy_surv_plt <- function(
  CVhelp_dat_w_plt=CVhelp_dat_w,
  doy_rng_in=c(10,100),
  pst_year_in="2012"
){
  # CVhelp_dat_l_plt$site <- CVhelp_dat_l_plt$variable
    # CVhelp_dat_l_plt$var <- CVhelp_dat_l_plt$variable
    CVhelp_dat_w_plt$year <- CVhelp_dat_w_plt$Year
    CVhelp_dat_w_plt <- subset(CVhelp_dat_w_plt,Year==pst_year_in)

    doy_int1 <- doy_rng_in[1]
    doy_int2 <- doy_rng_in[2]
    CVhelp_dat_w_plt$doy_int1 <- doy_int1
    CVhelp_dat_w_plt$doy_int2 <- doy_int2
    CVhelp_dat_w_plt$SELECTED <- CVhelp_dat_w_plt$DOY >= doy_int1 & CVhelp_dat_w_plt$DOY <= doy_int2


    # dat_w_subb <- subset(CVhelp_dat_w,DOY >= doy_int1 & CVhelp_dat_w$DOY <= doy_int2 )

    dat_w_subb <- CVhelp_dat_w_plt

    dat_w_subb$est_day <- 0.02 +
      ((dat_w_subb$VNS - 8.010345) / 1.5) +
      0.6 * ((dat_w_subb$CVP - 1657) / 1500) +
      +((dat_w_subb$OMT - 1200) / 5000) #+  #fix in a bit
    dat_w_subb$est_S <- plogis(dat_w_subb$est_day)
    dat_w_subb$SELECTED <- CVhelp_dat_w_plt$DOY >= doy_int1 &
      CVhelp_dat_w_plt$DOY <= doy_int2

    surv_plt_wrap <- ggplot2::ggplot(
      data = dat_w_subb,
      ggplot2::aes(
        y = est_S,
        x = DOY,
        fill = SELECTED,
        color = SELECTED,
        group = Year,
        xmin = doy_int1,
        xmax = doy_int1
      )
    ) +
      ggplot2::geom_line(linewidth = 0.25) +
      ggplot2::geom_point(shape = 21, size = 1) +
      ggplot2::scale_fill_manual(values = c("darkgray", "#428BCA")) +
      ggplot2::scale_color_manual(values = c("gray40", "#28547A")) +
      ggplot2::geom_vline(xintercept = doy_int1) +
      ggplot2::geom_vline(xintercept = doy_int2) +
      ggplot2::labs(x = "Day of Year",y="HOR-CHP") +
      ggplot2::ggtitle("HOR-CHP")+
      ggplot2::theme(legend.position = "none") #+
      #ggplot2::facet_wrap(~Year)

    surv_plt_wrap
}

 ggplot_doy_env_lattice_plt <- function(
  CVhelp_dat_l_plt=CVhelp_dat_l,
  doy_rng_in=c(10,100),
  pst_year_in="2012"
  )
  {

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
 
   flows_plt_wrap <- ggplot2::ggplot() +
      # lines of only model years
      ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,!Year %in% c("2011","2012","2013","2014","2015","2016")),
      ggplot2::aes(y=value,x=DOY,group=year),color="black",linewidth=0.25) +
      # points of only model years
      ggplot2::geom_point(data=subset(CVhelp_dat_l_plt,Year %in% c("2011","2012","2013","2014","2015","2016")),
      ggplot2::aes(y=value,
                      x=DOY,
                      fill=WYT,
                      alpha=SELECTED,
                      group=year),
                      shape=21,size=1) +
      ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,
                Year==pst_year_in),
      ggplot2::aes(y=value,
                      x=DOY,
                      group=year),color="black",linewidth=1) +
      # ggplot2::scale_fill_manual(values=WYT_cols) +
      ggplot2::scale_fill_manual(values=c("#FF5500","#FFEE99","#3399FF")) +
      ggplot2::facet_wrap(~variable,scale="free_y")+
      ggplot2::geom_vline(xintercept=doy_int1) +
      ggplot2::geom_vline(xintercept=doy_int2) +
      ggplot2::labs(x="Day of Year") +
      ggplot2::theme(legend.position="none") +
      ggplot2::scale_alpha_manual(values=c(0.5,1))

    # browser()

    print(flows_plt_wrap)
      }







 ggplot_yr_env_lattice_plt <- function(
  CVhelp_dat_l_plt=CVhelp_dat_l,
  doy_rng_in=c(10,100),
  pst_year_in="2012",
  sub_var_in = "VNS"
  )
  {

  # manually excluding some variables
  CVhelp_dat_l <- subset(CVhelp_dat_l, !variable %in% c("barrierTF","X2",""))

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
 
   flows_plt_wrap <- ggplot2::ggplot() +
      # lines of only model years
      ggplot2::geom_line(data=subset(CVhelp_dat_l_plt),#!Year %in% c("2011","2012","2013","2014","2015","2016")),
      ggplot2::aes(y=value,x=DOY,group=year),color="black",linewidth=0.25) +
      # points of only model years
      ggplot2::geom_point(data=subset(CVhelp_dat_l_plt,Year %in% c("2011","2012","2013","2014","2015","2016")),
      ggplot2::aes(y=value,
                      x=DOY,
                      fill=WYT,
                      alpha=SELECTED,
                      group=year),
                      shape=21,size=1) +
      ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,Year==pst_year_in),
      ggplot2::aes(y=value,
                      x=DOY,
                      group=year),color="black",linewidth=1) +
      # ggplot2::scale_fill_manual(values=WYT_cols) +
      ggplot2::scale_fill_manual(values=c("#FF5500","#FFEE99","#3399FF")) +
      ggplot2::facet_wrap(~variable,scale="free_y")+
      ggplot2::geom_vline(xintercept=doy_int1) +
      ggplot2::geom_vline(xintercept=doy_int2) +
      ggplot2::labs(x="Day of Year") +
      ggplot2::theme(legend.position="none") +
      ggplot2::scale_alpha_manual(values=c(0.5,1))


    flows_plt_wrap <- ggplot2::ggplot() +
      # lines of only model years
      ggplot2::geom_density(data=subset(CVhelp_dat_l_plt),#,!Year %in% c("2011","2012","2013","2014","2015","2016")),
      # ggplot2::geom_histogram(data=subset(CVhelp_dat_l_plt),#,!Year %in% c("2011","2012","2013","2014","2015","2016")),
      ggplot2::aes(x=value,group=year)) +#,color="black",linewidth=0.25) +

      # ggplot2::geom_density(data=subset(CVhelp_dat_l_plt),#,!Year %in% c("2011","2012","2013","2014","2015","2016")),
      # # ggplot2::geom_histogram(data=subset(CVhelp_dat_l_plt),#,!Year %in% c("2011","2012","2013","2014","2015","2016")),
      # ggplot2::aes(x=value,group=year)) 
      # points of only model years
      # ggplot2::geom_point(data=subset(CVhelp_dat_l_plt,Year %in% c("2011","2012","2013","2014","2015","2016")),
      # ggplot2::aes(y=value,
      #                 x=DOY,
      #                 fill=WYT,
      #                 alpha=SELECTED,
      #                 group=year),
      #                 shape=21,size=1) +
      # ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,Year==pst_year_in),
      # ggplot2::aes(y=value,
      #                 x=DOY,
      #                 group=year),color="black",linewidth=1) +
      # # ggplot2::scale_fill_manual(values=WYT_cols) +
      # ggplot2::scale_fill_manual(values=c("#FF5500","#FFEE99","#3399FF")) +
      ggplot2::facet_wrap(~variable,scale="free_x")+
      # ggplot2::geom_vline(xintercept=doy_int1) +
      # ggplot2::geom_vline(xintercept=doy_int2) +
      # ggplot2::labs(x="Day of Year") +
      ggplot2::theme(legend.position="none") +
      ggplot2::scale_alpha_manual(values=c(0.5,1))
    # browser()

    print(flows_plt_wrap)
      }



