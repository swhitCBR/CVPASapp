#' md_txt_extract 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
md_txt_extract <- function(
  md_addr="inst/app/www/mds/welcome_modal_top.md",
  header_ref,
  silent=TRUE,
  asHTML_frag=FALSE){
  md_lines_v <- base::readLines(md_addr)
  header_line_ids <- grep("^# ", md_lines_v)
  start_node <- grep(paste0("^",header_ref), md_lines_v)
  strt_ind <- which(header_line_ids==start_node)
  ref_line_ids <- (header_line_ids[strt_ind]+1):(header_line_ids[strt_ind+1]-1)
if(!silent) print(md_lines_v[ref_line_ids])
  
raw_txt <- paste(md_lines_v[ref_line_ids],collapse = "")  

if(!asHTML_frag){ return(raw_txt)  }

# in_p
 html_frag_out <-  markdown::markdownToHTML(
    shiny::HTML(
      raw_txt),
      fragment.only = T)
  
  return(html_frag_out)
}

