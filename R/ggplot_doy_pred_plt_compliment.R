ggplot_doy_pred_plt_compliment <- function(
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

  # Calculate complement columns
  data_in <- data_in |>
    dplyr::mutate(
      pr_comp = 1 - pr_pred,
      pLCL_comp = 1 - pUCL,
      pUCL_comp = 1 - pLCL
    )
  
  # Prepare long format data for plotting to facilitate legend and common aesthetics
  
  # if(param_in=="HOR"){

  # }
  
  if(param_in=="TCJ"){
    
    df_prop <- data_in |>
      dplyr::mutate(type = "TRN") |>
      dplyr::select(DOY, y = pr_pred, ymin = pLCL, ymax = pUCL, SELECTED, type)
    
    df_comp <- data_in |>
      dplyr::mutate(type = "MAC") |>
      dplyr::select(DOY, y = pr_comp, ymin = pLCL_comp, ymax = pUCL_comp, SELECTED, type)
    data_long <- dplyr::bind_rows(df_prop, df_comp)
    
    plt_tmp <- ggplot2::ggplot(data_long, ggplot2::aes(x=DOY, fill=type)) + #, linetype=type , color=type
      ggplot2::geom_ribbon(ggplot2::aes(ymin=ymin, ymax=ymax), alpha=0.2, color=NA) +
      ggplot2::geom_line(ggplot2::aes(y=y), size=1,linetype="solid") +
      ggplot2::geom_point(ggplot2::aes(y=y, fill=type),shape=21,color="gray30",# size=3,
                          stroke=1,color="black") +
      ggplot2::scale_color_manual(values = c("gray40", "#28547A")) +
      ggplot2::scale_fill_manual(name = NULL, values = c("MAC" = "#4E79A7", "TRN" = "#F28E2B")) +
      ggplot2::scale_y_continuous(breaks=c(0,0.2,0.5,0.8,1),limits=c(0,1)) +
      ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
      ggplot2::labs(y=param_in, x="Day of Year",fill="Route") + 
      ggplot2::theme(legend.position="top",
                     axis.text.x = ggplot2::element_text(size=14),axis.text.y = ggplot2::element_text(size=14),
                     axis.title.x = ggplot2::element_text(size=16),axis.title.y = ggplot2::element_text(size=16))
    return(plt_tmp)

  }
  
  df_prop <- data_in |>
    dplyr::mutate(type = "ORE") |>
    dplyr::select(DOY, y = pr_pred, ymin = pLCL, ymax = pUCL, SELECTED, type)
  
  df_comp <- data_in |>
    dplyr::mutate(type = "SJL") |>
    dplyr::select(DOY, y = pr_comp, ymin = pLCL_comp, ymax = pUCL_comp, SELECTED, type)
  data_long <- dplyr::bind_rows(df_prop, df_comp)
  
  ggplot2::ggplot(data_long, ggplot2::aes(x=DOY, fill=type)) + #, linetype=type , color=type
    ggplot2::geom_ribbon(ggplot2::aes(ymin=ymin, ymax=ymax), alpha=0.2, color=NA) +
    ggplot2::geom_line(ggplot2::aes(y=y), size=1,linetype="solid") +
    ggplot2::geom_point(ggplot2::aes(y=y, fill=type),shape=21,color="gray30", #size=3,
                        stroke=1,color="black") +
    # ggplot2::scale_fill_manual(values = c("darkgray", "#428BCA")) +
    ggplot2::scale_color_manual(values = c("gray40", "#28547A")) +
    ggplot2::scale_fill_manual(name = NULL, values = c("SJL" = "#4E79A7", "ORE" = "#F28E2B")) +
    ggplot2::scale_y_continuous(breaks=c(0,0.2,0.5,0.8,1),limits=c(0,1)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::labs(y=param_in, x="Day of Year",fill="Route") + 
    ggplot2::theme(legend.position="top",
                   axis.text.x = ggplot2::element_text(size=14),axis.text.y = ggplot2::element_text(size=14),
                   axis.title.x = ggplot2::element_text(size=16),axis.title.y = ggplot2::element_text(size=16),legend.title = ggplot2::element_text(color="black",size=14))
  
  
  
}