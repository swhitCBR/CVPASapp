
#' Title
#'
#' @param plots_in
#'
#' @returns
#'
#' @export
#' @examples
#' draw_rte_spec_surv_est_sec_ui()
#' 
draw_rte_spec_surv_est_sec_ui <- function(
  plots_in=list(
    "HOR_TCJ_pred_ggpplt_dup1b",
    "HOR_TCJ_pred_ggpplt_dup1a",
    "HOR_TCJ_pred_ggpplt_dup1e",
    "HOR_TCJ_pred_ggpplt_dup1c")){
  tagList(
      div(
      id='route_spec_surv_panel',
      h3(paste0("Route-Specific Survival"),style = "color:#28547A;text-decoration: underline;padding-left: 10px;"),
      div(
        style = "display:grid; grid-template-columns: repeat(2, 1fr); padding-left: 10px;padding-right:10px;",
        div(
           span(
            h4(
              style = "font-weight: normal; padding-left: 10px; display:inline-flex",
              draw_title_w_paren_hover_text_html(
                title_in="Head of Old River to Chipps Island",
                paren_htext_in="via SJR",
                modal_text_in="Estimate applies to smolts remaining 
                within the San Joaquin River."
              )
            )
          )
          ,
          div(
            plotOutput(plots_in[1], height = "400px")
          )
        ),
        div(
           span(
            h4(
              style = "font-weight: normal; padding-left: 10px; display:inline-flex",
              # draw_title_w_paren_hover_text_html()
              draw_title_w_paren_hover_text_html(
                title_in="Head of Old River to Chipps Island",
                paren_htext_in="via ORE",
                modal_text_in="Estimate applies to smolts traveling along the Old and Middle
                River routes."
              )
            )
          )
          ,
          div(
            plotOutput(plots_in[2], height = "400px")
          )
        )
        ,
                div(
           span(
            h4(
              style = "font-weight: normal; padding-left: 10px; display:inline-flex",
              # draw_title_w_paren_hover_text_html()
              draw_title_w_paren_hover_text_html(
                title_in="Turner Cut Junction to Chipps Island",
                paren_htext_in="via SJR",
                modal_text_in="Estimate applies to smolts traveling along the Old and Middle
                River routes."
              )
            )
          )
          ,
          div(
            plotOutput(plots_in[3], height = "400px")
          )
        ),
                div(
           span(
            h4(
              style = "font-weight: normal; padding-left: 10px; display:inline-flex",
              # draw_title_w_paren_hover_text_html()
              draw_title_w_paren_hover_text_html(
                title_in="Turner Cut Junction to Chipps Island",
                paren_htext_in="via TRN",
                modal_text_in="Estimate applies to smolts traveling along the Old and Middle
                River routes."
              )
            )
          )
          ,
          div(
            plotOutput(plots_in[4], height = "400px")
          )
        )
      )
    )
  )
 }
