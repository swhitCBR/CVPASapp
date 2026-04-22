#' Central Valley Daily and Annual Environmental Data
#'
#' A subset of data from multiple agencies with filtering and imputation carried about by the 'CVhelp" package
#'
#' @format ## `ann_HORbar_WYT_data`
#' A data frame with 14 rows and 6 columns:
#' \describe{
#'   \item{Year}{Calendar year}
#'   \item{barrier_days}{days with barrier}
#'   \item{barrier}{annual barrier status (i.e., any days with obstruction at all)}
#'   \item{ind}{index of 'wetness'}
#'   \item{wyt}{Water Year Type, abbreviation name}
#'   \item{WYT}{Water Year Type, full name}
#'   ...
#' }
#' @source <https://github.com/swhitCBR/CVhelp>
"ann_HORbar_WYT_data"

#' Central Valley Daily and Daily Environmental Data
#'
#' A subset of data from multiple agencies with filtering and imputation carried about by the 'CVhelp" package
#'
#' @format ## `CVhelp_dat_l`
#' A data frame with 42000 rows and 6 columns:
#' \describe{
#'   \item{date}{days with barrier}
#'   \item{Year}{Calendar year}
#'   \item{DOY}{annual barrier status (i.e., any days with obstruction at all)}
#'   \item{WYT}{Water Year Type, full name}
#'   \item{variable}{
#'     \describe{
#'        \item{CLC}{Temperature Clifton Court Forebay(C)}
#'        \item{MSD}{Temperature Mossdale Bridge (C)}
#'        \item{CVP}{Exports Federal Facility (kcfs)}
#'        \item{SWP}{Exports State Facility (kcfs)}
#'        \item{EXPORTS}{Combined Exports (kcfs)}
#'        \item{MID}{Middle River Flow (kcfs)}
#'        \item{OMT}{Water Movement in Interior Delta(C)}
#'        \item{ORB}{Old River Flow (kcfs)}
#'        \item{OUT}{Delta Outflow}
#'        \item{X2}{Export to flow index}
#'        \item{VNS}{Discharge at Vernalis}
#'        \item{barrierTF}{logical, barrier status} 
#'     }}
#'   \item{value}{daily numeric value}
#'   ...
#' }
#' @source <https://github.com/swhitCBR/CVhelp>
"CVhelp_dat_l"