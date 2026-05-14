#' utils_plotting
#'
#' @param DOY_arvDF_l_in day of year arrivals from 2011-2016 studies
#' @param LOC_in TCJ or HOR
#' @param start_day_in starting day of year
#' @param end_day_in ending day of year
#' @param doy_hst_xlims ggplot limits
#'
#' @description prepare annual WYT data in package for Datatable presentation in app
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
ggplt_doy_ref_hist <- function(
    DOY_arvDF_l_in,
    LOC_in,
    start_day_in,
    end_day_in,
    doy_hst_xlims=c(0,250)){
  
  tmp_sub <- subset(DOY_arvDF_l_in, name == paste0(LOC_in, "_DOY") & !is.na(value))
  plt_out <- ggplot2::ggplot(data = tmp_sub, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      breaks = seq(doy_hst_xlims[1],doy_hst_xlims[2],10),
      # bins = 25,
      color = "gray20",
      fill = c("#00BFBF", "#D52B80")[match(LOC_in, c("HOR", "TCJ"))]) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),axis.title.y = ggplot2::element_blank(),axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_vline(
      xintercept = c(start_day_in,end_day_in),
      color = "#024c63",
      linewidth = 1.5) +
    ggplot2::scale_y_continuous(expand=c(0,200))
  
  print(plt_out)
}



#' ggplot_flength_ref_hist 
#'
#' @param flength_ref_dat_in dataframe with 'flength' column, defaults to package data set
#' @param flength_in fork length reference value
#' @param flength_hst_xlims_in histogram limits
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
ggplot_flength_ref_hist <- function(
    flength_ref_dat_in=flength_ref_dat,
    flength_in,
    flength_hst_xlims_in=flength_hst_xlims
    ){
  
  
  tmp_sub <- flength_ref_dat_in |>
    dplyr::filter(
      flength > flength_hst_xlims_in[1] & flength < flength_hst_xlims_in[2]
    )
  
  plt_out <- ggplot2::ggplot(data = tmp_sub, ggplot2::aes(x = flength)) +
    ggplot2::geom_histogram(
      # bins = 25
      breaks = seq(flength_hst_xlims_in[1],flength_hst_xlims_in[2],10),
    ) +
    ggplot2::scale_y_continuous(limits = c(0,800)) +
    # ggplot2::scale_x_continuous(limits = flength_hst_xlims_in) + #expand=c(0,0),
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),axis.title.y = ggplot2::element_blank(),axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),axis.title.x = ggplot2::element_blank()) +
    ggplot2::geom_vline(
      xintercept = flength_in,
      color = "#024c63",
      linewidth = 1.5)
  
  print(plt_out)
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
  
  # doy_rng_in <- c(global_reactive$global$start_day,global_reactive$global$end_day)

  doy_int1 <- doy_rng_in[1]
  doy_int2 <- doy_rng_in[2]
  CVhelp_dat_l_plt$doy_int1 <- doy_int1
  CVhelp_dat_l_plt$doy_int2 <- doy_int2
  CVhelp_dat_l_plt$SELECTED <- (CVhelp_dat_l_plt$DOY >= doy_rng_in[1] & 
    CVhelp_dat_l_plt$DOY <= doy_rng_in[2] & 
    CVhelp_dat_l_plt$Year==pst_year_in
    # CVhelp_dat_l_plt$Year==global_reactive$global$past_water_year
)
 
   flows_plt_wrap <- ggplot2::ggplot() +
      ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,
            !Year %in% c("2011","2012","2013","2014","2015","2016")),
        ggplot2::aes(y=value,x=DOY,group=year),color="black",linewidth=0.25) +
      ggplot2::geom_point(data=subset(CVhelp_dat_l_plt,
                                        Year %in% c("2011","2012","2013","2014","2015","2016")),
      ggplot2::aes(y=value,
                      x=DOY,
                      fill=WYT,
                      alpha=SELECTED,
                      group=year),
                      shape=21,size=1) +
      ggplot2::geom_line(data=subset(CVhelp_dat_l_plt,
                Year==pst_year_in
              ),
      ggplot2::aes(y=value,
                      x=DOY,
                      group=year),color="black",linewidth=1) +
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