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
      /*   background-color: rgba(2, 76, 99, 0.5); */
        color: #0f0f0f; 
      }

    /* Right justify logo and dropdowns in the leftUI() dashboard object */
    .navbar-custom-menu{
      float: right !important; 
    }
    
    /* sidebar menu formatting  */
    .treeview-menu {
        padding: 5px 5px 5px 25px !important;
        display: block;
        font-size: 10px !important
    }

    /* SUB-MENU  */
    /* medium gray-blue on hover  */
    .skin-blue .sidebar-menu .treeview-menu>li>a:hover  { 
      color: #0f0f0f; 
      background-color: rgba(2, 76, 99, 0.5); 
    }

  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
      /*color: #0f0f0f; */
        color: #024c63;
      background-color: rgba(2, 76, 99, 0.5); /* Change hover background color here */
  }

   /* dynamic sidebar text color #024c63*/
    #newsidebox{
      color: #024c63;
      font-size: 14px;
      padding-left: 5px;
      padding-right: 5px;
      margin-left: 10px;
      margin-right: 10px;
      text-wrap: auto;
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

    /* DATA TABLE  */
    /* background of datatable header bar color -- for better visibility */
    table.dataTable thead {
    background-color: #B6B6B6 !important;
    }

    table.dataTable {
    border-collapse: collapse !important;
    }

    /* SLIDER AESTHETICS */
    /* slider bar filled with CBR dark blue color; affects DOYslider_rng*/
    .irs--shiny .irs-bar {
        background: #024c63 !important;
    }

    .irs--shiny .irs-bar--single {
        border-top: 1px solid transparent !important;
        border-bottom: 1px solid transparent !important;
        background: transparent !important;
    }

    .irs--shiny .irs-from,
    .irs--shiny .irs-to,
    .irs--shiny .irs-single {
      color: #fff;
      text-shadow: none;
      padding: 1px 3px;
      background-color: #024c63 !important;
    }

    /* AIR DATE PICKER AESTHETICS -  MATCH GREY BACKGROUND OF OTHER SELECTORS */
      #date_start_sep {
      background-color: #f4f4f4 !important;
      margin-bottom: 0px;
    }

    #date_end_sep {
      background-color: #f4f4f4 !important;
    }

    /* Color for the start and end dates */
    .air-datepicker-cell.-selected-, 
    .air-datepicker-cell.-selected-.-focus- {
      background-color: #024c63 !important; 
      color: #fff !important;
    }

    /* DROPDOWN SHADING THAT MATCHES CBR COLORS */

    .dropdown-item {
      /* color: #fff !important; */
      color: #444 !important;
      text-shadow: none;
      /* background-color: #024c63 !important; */
    }

    /* dropdown-menu  hover color*/
    .dropdown-item:hover {
      /* background-color: #024c63 !important; Your preferred color */
      color: white !important;
    }

    .dropdown-menu .active {
      color: #fff !important;
      text-shadow: none;
      /* background-color: #024c63 !important;  */
    }
  
    /* DETAILS BOX FORMATTING */
  
    /* Plus and minus symbols */
    summary::before {
      content: '+ ';
    }

    /* 3. Change to minus when open */
    details[open] summary::before {
      content: '− ';
    }

    summary {
      list-style: none;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    /* Add a horizontal line between title and content */
    details[open] > summary {
      border-bottom: 2px solid #fff9f1;}

    summary {
      color: #30353b;
      border-radius: 0px;
    }

    details {
      color: #30353b !important;
    }




}
        
    "
      
    return(tmp_txt)
      
    }
    
    warning("css content not loaded")

    }
