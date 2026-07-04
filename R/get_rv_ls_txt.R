get_rv_ls_txt <- function(heading="replace_me",RVls_in,sep_in="\n   "){
    if(heading!="none"){
    paste(
      heading,
      paste0(
        "start_day - end_day : \n\t",
        RVls_in[["start_day"]], 
        " - ",
        RVls_in[["end_day"]] 
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
        RVls_in[["start_day"]], 
        " - ",
        RVls_in[["end_day"]] 
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