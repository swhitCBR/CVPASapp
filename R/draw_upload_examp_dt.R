#' Title
#'
#' @param columns_used
#'
#' @returns
#' @export
#'
#' @examples
draw_upload_examp_dt <- function(columns_used=c("date","Year","WYT","OMT","barrierTF","CLC","VNS","SWP","CVP","MSD")) {
  
  
  df_to_plot <- get_upload_dat_example(columns_used=columns_used,CVhelp_dat_w_in=CVhelp_dat_w)
  # # Use provided columns or all columns
  # df_to_plot <- if (!is.null(columns_used)) {
  #   CVhelp_dat_w[, columns_used, drop = FALSE]
  # } else {
  #   CVhelp_dat_w
  # }
  # 
  # df_to_plot$OMT <- signif(df_to_plot$OMT,digits=5)
  # df_to_plot$VNS <- signif(df_to_plot$VNS,digits=3)
  # df_to_plot$SWP <- signif(df_to_plot$SWP,digits=5)
  # df_to_plot$CVP <- signif(df_to_plot$CVP,digits=5)
  # df_to_plot$CLC <- signif(df_to_plot$CLC,digits=3)
  # df_to_plot$MSD <- signif(df_to_plot$MSD,digits=3)
  # 
  # df_to_plot <- df_to_plot |> dplyr::relocate(date,Year,WYT,barrierTF,VNS,OMT,CVP,SWP,CLC,MSD)
  # # colnames = c("Date",'Year', 'WYT', 'barrier','log(VNS)','OMT','CVP','SWP','CLC','MSD'),
  # 
  # df_to_plot  <- df_to_plot|> dplyr::rename(Date=date,
  #                                           "HOR Barrier"=barrierTF,
  #                                           "log(VNS)"=VNS
  #                                           )
  # 
  # # We take a subset for the preview
  # dt_head <- head(df_to_plot, 6)

  # Base datatable
  dt <- DT::datatable(caption ="Daily Input Variable Template",
    # dt_head,
    df_to_plot,
    selection = "none",
    rownames = FALSE,
    options = list(
      info = FALSE,
      dom = '<"<"bottom"ip>',
      pageLength = 6,
      stripeClasses = list(),
      pagingType = "simple",
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'font-size': '100%'});",
        "$(this.api().table().body()).css({'font-size': '80%'});",
        "$(this.api().table().footer()).css({'font-size': '80%'});",
        "}"
      )
    )
  )
  # # Round numeric columns to 2 decimal places
  # numeric_cols <- names(dt_head)[sapply(dt_head, is.numeric)]
  # if (length(numeric_cols) > 0) {
  #   dt <- dt |> DT::formatRound(columns = numeric_cols, digits = 2)
  # }

  return(dt)

}

get_upload_dat_example <- function(
    CVhelp_dat_w_in,
    columns_used=c("date","Year","WYT","OMT","barrierTF","CLC","VNS","SWP","CVP","MSD")){
  
  # Use provided columns or all columns
  df_to_plot <- if (!is.null(columns_used)) {
    CVhelp_dat_w_in[, columns_used, drop = FALSE]
  } else {
    CVhelp_dat_w_in
  }
  
  df_to_plot$OMT <- signif(df_to_plot$OMT,digits=5)
  df_to_plot$VNS <- signif(df_to_plot$VNS,digits=3)
  df_to_plot$SWP <- signif(df_to_plot$SWP,digits=5)
  df_to_plot$CVP <- signif(df_to_plot$CVP,digits=5)
  df_to_plot$CLC <- signif(df_to_plot$CLC,digits=3)
  df_to_plot$MSD <- signif(df_to_plot$MSD,digits=3)
  
  df_to_plot <- df_to_plot |> dplyr::relocate(date,Year,WYT,barrierTF,VNS,OMT,CVP,SWP,CLC,MSD)
  # colnames = c("Date",'Year', 'WYT', 'barrier','log(VNS)','OMT','CVP','SWP','CLC','MSD'),
  
  df_to_plot  <- df_to_plot|> dplyr::rename(Date=date,
                                            "HOR Barrier"=barrierTF,
                                            "log(VNS)"=VNS
  )
  return(df_to_plot)
}
