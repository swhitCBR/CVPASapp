
ggplot_doy_rte_plt <- function(
    HOR_TCJ_pred_tab_plt,
    doy_rng_in=c(10,100),
    pst_year_in="2012"#,
    # disp_SE=TRUE
){
  doy_int1 <- doy_rng_in[1]
  doy_int2 <- doy_rng_in[2]
  
  # print(str(HOR_TCJ_pred_tab_plt))
  HOR_TCJ_pred_tab_plt <- subset(HOR_TCJ_pred_tab_plt,Year==pst_year_in)
  HOR_TCJ_pred_tab_plt$SELECTED  <- HOR_TCJ_pred_tab_plt$DOY >= doy_int1 & HOR_TCJ_pred_tab_plt$DOY <= doy_int2

  # manipulations to change height of line and ribbon
  HOR_TCJ_pred_tab_plt$lo_pred <- HOR_TCJ_pred_tab_plt$lo_pred+2.5
  HOR_TCJ_pred_tab_plt$UCL <-HOR_TCJ_pred_tab_plt$UCL+2.5
  HOR_TCJ_pred_tab_plt$LCL <- HOR_TCJ_pred_tab_plt$LCL+2.5

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,ymin=plogis(LCL),ymax=plogis(UCL)),fill="gray70") +
    ggplot2::geom_line(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,y=plogis(lo_pred))) +
    ggplot2::geom_point(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,y=plogis(lo_pred),fill=SELECTED),shape=21) +
    #ggplot2::facet_wrap(~Year) + 
    ggplot2::geom_vline(xintercept = doy_int1) +
    ggplot2::geom_vline(xintercept = doy_int2) +
    ggplot2::scale_fill_manual(values = c("darkgray", "darkgreen")) +
    # ggplot2::scale_color_manual(values = c("gray40", "green")) + 
    ggplot2::theme(legend.position="none")+
    # ggplot2::scale_color_manual() + 
    # ggplot2::labs(x="",y="") + 
    # ggplot2::scale_y_continuous(breaks=c(0,0.2,0.5,0.8)) +
    # ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) + 
    ggplot2::scale_y_continuous(breaks=c(0,0.2,0.5,0.8),limits=c(0,1)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::labs(y="Survival Probability",x="Day of Year") + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(size=14),axis.text.y = ggplot2::element_text(size=14),
                   axis.title.x = ggplot2::element_text(size=16),axis.title.y = ggplot2::element_text(size=16))


print(head(HOR_TCJ_pred_tab_plt))
print(str(HOR_TCJ_pred_tab_plt))
  
HOR_TCJ_pred_tab_plt <- HOR_TCJ_pred_tab_plt |> 
  dplyr::mutate(SJR_prob=plogis(.data$lo_pred),
                nonSJR_prob=1-SJR_prob,
                SJR_prob_LCL=plogis(.data$LCL),
                SJR_prob_UCL=plogis(.data$UCL),
                nonSJR_prob_LCL=1-.data$SJR_prob_UCL,
                nonSJR_prob_UCL=1-.data$SJR_prob_LCL
              )

# HOR_TCJ_pred_tab_plt <- HOR_TCJ_pred_tab_plt |> dplyr::mutate(SJR_prob=plogis(.data$lo_pred))
# Blue: #4E79A7 (Steel Blue)Orange: #F28E2B (Burnt Orange)
 HOR_TCJ_pred_tab_plt <- HOR_TCJ_pred_tab_plt[HOR_TCJ_pred_tab_plt$DOY >= doy_int1 & HOR_TCJ_pred_tab_plt$DOY <= doy_int2,]
  
ggplot2::ggplot() +
    ggplot2::geom_ribbon(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,ymin=SJR_prob_LCL,ymax=SJR_prob_UCL),fill="#4E79A7",alpha=0.4) +
    ggplot2::geom_ribbon(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,ymin=nonSJR_prob_LCL,ymax=nonSJR_prob_UCL),fill="#F28E2B",alpha=0.4) +
    ggplot2::geom_line(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,y=nonSJR_prob)) +
    ggplot2::geom_point(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,y=nonSJR_prob),fill="#F28E2B",shape=21) +
    ggplot2::geom_line(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,y=SJR_prob)) +
    ggplot2::geom_point(data=HOR_TCJ_pred_tab_plt,ggplot2::aes(x=DOY,y=SJR_prob),fill="#4E79A7",shape=21) +
    # ggplot2::geom_vline(xintercept = doy_int1) +
    # ggplot2::geom_vline(xintercept = doy_int2) +
    # ggplot2::theme(legend.position="bottom")+
    ggplot2::labs(y="Route Use Probability",x="Day of Year") +
    ggplot2::scale_y_continuous(breaks=c(0,0.2,0.5,0.8),limits=c(0,1)) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size=14),axis.text.y = ggplot2::element_text(size=14),
                   axis.title.x = ggplot2::element_text(size=16),axis.title.y = ggplot2::element_text(size=16))#+
  # scale_fill_manual(
  #   name = "Manual Aesthetic Legend",
  #   values = c(
  #     "First Manual Label"  = "#4E79A7",
  #     "Second Manual Label" = "#F28E2B"))





}