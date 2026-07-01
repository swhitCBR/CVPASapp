#' Title
#'
#' @returns 
#'
#' @export
#' 
#' @examples
#' draw_ovr_surv_est_sec_ui()
#' 
draw_ovr_surv_est_sec_ui <-  function(){
 tagList( 
      div(
      id="overall_surv_panel",
      h3(
        paste0("Overall Survival"),
        style = "color:#28547A;text-decoration: underline;padding-left: 10px;"
      ),
      div(
        style = "display:grid; grid-template-columns: repeat(3, 1fr); padding-left: 10px;padding-right:10px;",
        div(
          draw_title_w_paren_hover_text_html(
                title_in="Head of Old River to Chipps Island",
                paren_htext_in="all routes",
                modal_text_in="Estimate accounts for the possibility that smolts are diverted from the main
               San Joaquin River along the route the Old/Middle River route (ORE), which diverges
                at the Head of Old River junction (HOR), or into Turner Cut at
                Turner Cut junction (TCJ) further downstream."
          )
          ,
          plotOutput("HOR_TCJ_pred_ggpplt_s_tot")
        )
      )
      )
    )
}

