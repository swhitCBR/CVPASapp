# Initial values
  init_water_year <- 2013
  init_DOY <- c(45, 135)
  init_WYT_type <- "Wet"
  init_start_loc <- "HOR"
  init_bar <- "Out"
  init_flength <- 244
  
  
# Water Year Type Labels and selections 
  WYT_cols <- c("#3399FF",  "#99EEFF", "#FFFFCC","#FFCC66", "#FF5500")
  names(WYT_cols) <- c(
      "Wet",
      "Above Normal",
      "Below Normal",
      "Dry",
      "Critical") # new addition

  wyt_type_opt <- c(
      "Wet" = "Wet",
      "Above Normal" = "Above Normal",
      "Below Normal" = "Below Normal",
      "Dry" = "Dry",
      "Critical" = "Critical")

  RV_text_fun <- function(heading="replace_me",RVls_in,sep_in="\n   "){
    if(heading!="none"){
    paste(
      heading,
      paste0(
        "start_day - end_day : ",
        RVls_in[["start_day"]], #global_reactive$start_day,
        " - ",
        RVls_in[["end_day"]] #global_reactive$end_day
      ),
      paste0("flength : ", RVls_in[["flength"]]),
      paste0(
        "past_water_year : ",
        RVls_in[["past_water_year"]]
      ),
      paste0("WYT : ", RVls_in[["WYT"]]),
      paste0("BAR : ", RVls_in[["BAR"]]),
      paste0("LOC : ", RVls_in[["LOC"]]),
      sep = sep_in
    )
  } else{
     paste(
      paste0(
        "start_day - end_day : ",
        RVls_in[["start_day"]], #global_reactive$start_day,
        " - ",
        RVls_in[["end_day"]] #global_reactive$end_day
      ),
      paste0("flength : ", RVls_in[["flength"]]),
      paste0(
        "past_water_year : ",
        RVls_in[["past_water_year"]]
      ),
      paste0("WYT : ", RVls_in[["WYT"]]),
      paste0("BAR : ", RVls_in[["BAR"]]),
      paste0("LOC : ", RVls_in[["LOC"]]),
      sep = "\n"
    )
  }

}

  in_selected_RV <- reactiveValues(
    # past water year should be NA to start otherwise app flickers
    past_water_year = init_water_year,
    start_day = init_DOY[1],
    end_day = init_DOY[2],
    start_date = as.Date(
      paste(init_DOY[1], init_water_year, sep = "-"),
      format = "%j-%Y"
    ),
    end_date = as.Date(
      paste(init_DOY[2], init_water_year, sep = "-"),
      format = "%j-%Y"
    ),
    flength = init_flength,
    WYT = init_WYT_type,
    LOC = init_start_loc,
    BAR = init_bar
  )

  global <- reactiveValues(
    # past water year should be NA to start otherwise app flickers
    past_water_year = init_water_year,
    start_day = init_DOY[1],
    end_day = init_DOY[2],
    start_date = as.Date(
      paste(init_DOY[1], init_water_year, sep = "-"),
      format = "%j-%Y"
    ),
    end_date = as.Date(
      paste(init_DOY[2], init_water_year, sep = "-"),
      format = "%j-%Y"
    ),
    flength = init_flength,
    WYT = init_WYT_type,
    LOC = init_start_loc,
    BAR = init_bar
  )

# prepare data stored in package for creation of Datatable in app
ann_HORbar_WYT_data_TAB <- reactiveVal(CVPASapp:::past_year_tab_prep(ann_data_in = ann_HORbar_WYT_data))


    # .svg plot read_in
    HOR_CHP_xml <- xml2::read_xml(
      "inst/app/www/images/svg/basic route schematic/HOR_CHP.svg"
    )
    HOR_CHP_bar_in_xml <- xml2::read_xml(
      "inst/app/www/images/svg/basic route schematic/HOR_CHP_bar_in.svg"
    )
    TCJ_CHP_xml <- xml2::read_xml(
      "inst/app/www/images/svg/basic route schematic/TCJ_CHP.svg"
    )
    TCJ_CHP_bar_in_xml <- xml2::read_xml(
      "inst/app/www/images/svg/basic route schematic/TCJ_CHP_bar_in.svg"
    )

        barrier_opt <- c("In" = "In", "Out" = "Out")
    barrier_label <- "HOR Barrier:"
    loc_opt <- c(
      'Head of Old River (HOR)' = "HOR",
      'Turner Cut Junction (TCJ)' = "TCJ"
    )

    init_data_source <- "Previous year"

     surv_route_diagram_wtt_xml <- xml2::read_xml(
    "inst/app/www/surv_route_diagram_wtt.svg"
  )
