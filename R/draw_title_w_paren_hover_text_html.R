
#' Title
#'
#' @param title_in bolded title
#' @param paren_htext_in parenthetical plain text to the right of the title w/ same format
#' @param modal_text_in text that appears in the model
#'
#' @returns HTML code with title and hover text over a parenthetical
#'
#' @export
#' 
#' @examples
#' draw_title_w_paren_hover_text_html(
#' title_in="Head of Old River to Turner Cut Junction",
#' paren_htext_in="via SJR",
#' modal_text_in="Hello World")
#' 
draw_title_w_paren_hover_text_html <- function(
  title_in="Head of Old River to Turner Cut Junction",
  paren_htext_in="via SJR",
  modal_text_in="Hello World"){
    h4(
      style = "font-weight: normal; padding-left: 10px; display:inline-flex",
      span(
        style="margin-right:5px",
        strong(title_in),
        p(
          style="display:inline-flex",
          "(",
        div(
          class="hover-container",
          a(paren_htext_in
            ,.noWS = "outside"
       
        ),
          div(
          class="hover-modal",
            p(modal_text_in)
                        ,.noWS = "outside"

        )
      ,
    ")"
  )
    )
    )
      )
}

