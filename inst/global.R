# Initial values
  init_water_year <- 2013
  init_DOY <- c(45, 135)
  init_WYT_type <- "Wet"
  init_start_loc <- "HOR"
  init_bar <- "Out"
  init_flength <- 244
  
  
# Water Year Type Labels and selections 
  WYT_cols <- c("#3399FF", "#99EEFF", "#FFFFCC", "#FFCC66", "#FF5500")
  names(WYT_cols) <- c(
      "Wet",
      "Below Normal",
      "Above Normal",
      "Dry",
      "Critical") # new addition

  wyt_type_opt <- c(
      "Wet" = "Wet",
      "Above Normal" = "Above Normal",
      "Below Normal" = "Below Normal",
      "Dry" = "Dry",
      "Critical" = "Critical")

  RV_text_fun <- function(heading="replace_me",RVls_in){
    paste(
      heading,
      paste0(
        "start_day - end_day : ",
        RVls_in[["start_day"]], #global_reactive$start_day,
        " - ",
        RVls_in[["end_day"]] #global_reactive$end_day
      ),
      # paste0("start_date: ", RVls_in[["start_date"]]),
      # paste0("end_date: ", RVls_in[["end_date"]]),
      # paste0("data_type : ", RVls_in[["data_type"]]),
      # paste0("date_entry : ", RVls_in[["date_entry"]]),
      paste0("flength : ", RVls_in[["flength"]]),
      paste0(
        "past_water_year : ",
        RVls_in[["past_water_year"]]
      ),
      paste0("WYT : ", RVls_in[["WYT"]]),
      paste0("BAR : ", RVls_in[["BAR"]]),
      paste0("LOC : ", RVls_in[["LOC"]]),
      sep = "\n\t"
    )
  }

# examp_RVls <- list("WYT"="Wet","BAR"="Out","LOC"="TCJ","flength"=244,"start_day"=40,"end_day"=99)
# cat(RV_text_fun(RVls_in=examp_RVls))
# parse(RV_text_fun(RVls_in=examp_RVls))

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

# ann_HORbar_WYT_data_TAB <- reactiveVal(
#  ann_HORbar_WYT_data |>
#         dplyr::mutate(
#           Model = ifelse(
#             Year %in%
#               c("2011", "2012", "2013", "2014", "2015", "2016"),
#             "Yes",
#             "No"
#           )
#         ) |>
#         dplyr::select(Year, WYT, barrier, Model))

# prepare data stored in package for creation of Datatable in app
ann_HORbar_WYT_data_TAB <- reactiveVal(CVPASapp:::past_year_tab_prep(ann_data_in = ann_HORbar_WYT_data))