# Builds a plotmath expression (as a string, for use with parse=TRUE) from a
# param_in code. Params with "via" become conditional theta-hat notation,
# e.g. "HOR_CHPviaSJL" -> hat(theta)[HOR-CHP|SJL]. Others become survival
# S-hat notation with a leading "S_" stripped, e.g. "S_HOR_CHP" -> hat(S)[HOR-CHP].
param_label_plotmath <- function(param_in) {

  if (grepl("via", param_in)) {
    parts <- strsplit(param_in, "via")[[1]]
    base_tokens <- strsplit(parts[1], "_")[[1]]
    base_str <- paste(base_tokens, collapse = '*"-"*')
    sprintf('italic(hat(theta)[%s*"|"*%s])', base_str, parts[2])
  } else {
    tokens <- strsplit(param_in, "_")[[1]]
    if (tokens[1] == "S") tokens <- tokens[-1]
    base_str <- paste(tokens, collapse = '*"-"*')
    sprintf('italic(hat(S)[%s])', base_str)
  }
}

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

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=data_in,ggplot2::aes(x=DOY,ymin=pLCL,ymax=pUCL),fill="gray70") +
    ggplot2::geom_line(data=data_in,ggplot2::aes(x=DOY,y=pr_pred)) +
    ggplot2::geom_point(data=data_in,ggplot2::aes(x=DOY,y=pr_pred,fill=SELECTED),shape=21)+#,color="gray40") +
    ggplot2::scale_fill_manual(values = c("darkgray", "#428BCA")) +
    ggplot2::scale_color_manual(values = c("gray40", "#28547A")) +
    ggplot2::theme(legend.position="none")+
    ggplot2::scale_y_continuous(breaks=c(0,0.2,0.5,0.8,1),limits=c(0,1)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::annotate("text",x=Inf,y=Inf,label=param_label_plotmath(param_in),parse=TRUE,hjust=1.1,vjust=1.5,size=5.625) +
    ggplot2::labs(y=NULL,x="Day of Year") + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(size=14),axis.text.y = ggplot2::element_text(size=14),
                   axis.title.x = ggplot2::element_text(size=16),axis.title.y = ggplot2::element_text(size=16))
  
  
}