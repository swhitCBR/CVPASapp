# Custom CSS that does not work if it's in the 'custom.css' file alone
#' Title
#'
#' @param css_txt_content
#'
#' @returns .css definitions for specific ui elements (i.e., sidebar)
#'
#' @export
#' @examples
#' get_custom_css_txt()
#' 
get_custom_css_txt <- function(css_txt_content="sidebar"){
    if(css_txt_content=="sidebar"){
    tmp_txt <- "
       /* TOP-LEVEL MENU  */
       /* gray highlight to blue-gray and blue to black text  */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { 
        background-color: rgba(2, 76, 99, 0.5); 
        color: #0f0f0f; 
      }
    
      /* sidebar menu formatting  */
      /* selected value  */
      .skin-blue .main-sidebar .sidebar-menu .active a { 
        background-color: rgba(2, 76, 99, 0.5); 
         /* color: #0f0f0f; */
      }
   
      /* SUB-MENU  */
      /* dark gray-blue on hover  */
      .skin-blue .sidebar-menu .treeview-menu>li>a:hover  { 
        /* color: #0f0f0f; */
        background-color: rgba(2, 76, 99, 0.8); 
      }

      /* darker blue outline for selections within submenu  */
      .skin-blue .sidebar-menu .treeview-menu>li>a:hover  { 
        color: #0f0f0f;  
        background-color: rgba(2, 76, 99, 0.8); 
      }

      .box {
        border-top: 1px solid #ddd !important;
        border-left: 1px solid #ddd;
        border-right: 1px solid #ddd;
        border-bottom: 1px solid #ddd;
      }
      .box-header {
        border-bottom: 2px solid #ddd !important;
      }
        
    "
      
    return(tmp_txt)
      
    }
    
    warning("css content not loaded")

    }
