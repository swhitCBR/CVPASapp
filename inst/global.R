# Initial values
  init_data_source <- "Previous year"
  init_water_year <- 2013
  init_DOY <- c(45, 135)
  init_WYT_type <- "Wet"
  init_start_loc <- "HOR"
  init_bar <- "Out"
  init_flength <- 244

  WYT_cols <- utils_get_WYT_cols_vec()

  wyt_type_opt <- c(
      "Wet" = "Wet",
      "Above Normal" = "Above Normal",
      "Below Normal" = "Below Normal",
      "Dry" = "Dry",
      "Critical" = "Critical")

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


  past_year_tab_prep <- function(
  ann_data_in=ann_HORbar_WYT_data,
  model_yes_years=c("2011", "2012", "2013", "2014", "2015", "2016")
){ann_data_in |>
      dplyr::mutate(
        Model = ifelse(
          Year %in% model_yes_years,"Yes","No")) |>
      dplyr::select(Year, WYT, barrier, Model)
}

# prepare data stored in package for creation of Datatable in app
ann_HORbar_WYT_data_TAB <- reactiveVal(CVPASapp:::past_year_tab_prep(ann_data_in = ann_HORbar_WYT_data))

# .svg plot read_in
# HOR_CHP_xml <- xml2::read_xml("inst/app/www/images/svg/basic_route_schematic/HOR_CHP.svg")
# HOR_CHP_bar_in_xml <- xml2::read_xml("inst/app/www/images/svg/basic_route_schematic/HOR_CHP_bar_in.svg")
# TCJ_CHP_xml <- xml2::read_xml("inst/app/www/images/svg/basic_route_schematic/TCJ_CHP.svg")
# TCJ_CHP_bar_in_xml <- xml2::read_xml("inst/app/www/images/svg/basic_route_schematic/TCJ_CHP_bar_in.svg")
surv_route_diagram_wtt_xml <- xml2::read_xml("inst/app/www/met_and_ref/surv_route_diagram_wtt.svg")

barrier_opt <- c("In" = "In", "Out" = "Out")
barrier_label <- "HOR Barrier:"
loc_opt <- c('Head of Old River (HOR)' = "HOR",'Turner Cut Junction (TCJ)' = "TCJ")



