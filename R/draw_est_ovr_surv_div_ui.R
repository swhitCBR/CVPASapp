#' Title
#'
#' @returns 
#'
#' @export
#' 
#' @examples
#' draw_est_ovr_surv_div_ui()
#' 
draw_est_ovr_surv_div_ui <-  function(){
 tagList( 
      div(
      # column(width=12,
      id="overall_surv_panel",
      h3(
        paste0("Overall Survival"),
        style = "color:#28547A;text-decoration: underline;padding-left: 10px;"
      ),
      div(
        style = "display:grid; grid-template-columns: repeat(1, 1fr); padding-left: 10px;padding-right:10px;",
        # style = "display:grid; grid-template-columns: repeat(3, 1fr); padding-left: 10px;padding-right:10px;",
        div(
          draw_title_w_paren_hover_text_html(
                title_in="Head of Old River to Chipps Island",
                paren_htext_in="all routes",
                modal_text_in="Estimate accounts for all possible routes from Head of Old River to
                 Chipps Island, including the mainstem San Joaquin River, Old and Middle rivers,
                 salvage via CVP and SWP, and other interior Delta routes."
          )
          ,
          plotOutput("HOR_TCJ_pred_ggpplt_s_tot")
        )
      )
      )
    )
}

