


CVPAS_prev_yr_ref_tab



# ann_HORbar_WYT_data
# past_year_tab_prep()

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


# Getting rid of unnecessary functions
ann_HORbar_WYT_data_TAB <- past_year_tab_prep(ann_HORbar_WYT_data)


  past_year_tab_prep_MOD <- function(
    ann_data_in=ann_HORbar_WYT_data,
    model_yes_years=c("2011", "2012", "2013", "2014", "2015", "2016")
  ){ann_data_in |>
        dplyr::mutate(
          Model = ifelse(
            Year %in% model_yes_years,"Yes","No")) #|>
        # dplyr::select(Year, WYT, barrier, Model)
  }

ann_HORbar_WYT_data_TAB <- past_year_tab_prep_MOD(ann_HORbar_WYT_data)

# writing everything
write.csv(ann_HORbar_WYT_data_TAB,"inst/app/www/CVPAS_prev_yr_ref_tab.csv",row.names=F)


CVPAS_prev_yr_ref_tab <- read.csv("inst/app/www/CVPAS_prev_yr_ref_tab.csv")
str(CVPAS_prev_yr_ref_tab)


## adding data
usethis::use_data_raw("CVPAS_prev_yr_ref_tab")
