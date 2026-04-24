#' past_year_tab_prep
#'
#' @description prepare annual WYT data in package for Datatable presentation in app
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
  past_year_tab_prep <- function(
    ann_data_in=ann_HORbar_WYT_data,
    model_yes_years=c("2011", "2012", "2013", "2014", "2015", "2016")
  ){ann_data_in |>
        dplyr::mutate(
          Model = ifelse(
            Year %in% model_yes_years,"Yes","No")) |>
        dplyr::select(Year, WYT, barrier, Model)
  }