#' Title
#'
#' @returns
#' @export
#'
#' @examples
draw_est_rte_div_ui <- function(){
  tagList(
  div(
        style = "padding-left: 10px;",
        id = "route_usage_panel",
        h3(
          paste0("Route Usage"),
          style = "color:#006400;text-decoration: underline;"
        ),
        column(
          width = 6,
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>At Head of Old River</strong>
               </h4>'
          ),
          plotOutput("HOR_TCJ_pred_ggpplt_dup1d", height = "400px")
        ),
        column(
          width = 6,
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>At Turner Cut Junction </strong>
               </h4>'
          ),
          plotOutput("HOR_TCJ_pred_ggpplt_dup1d2", height = "400px")
        )
        # Blue: #4E79A7 (Steel Blue)Orange: #F28E2B (Burnt Orange)
      )
    )
    }