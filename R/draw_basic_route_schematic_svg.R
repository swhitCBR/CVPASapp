
draw_basic_route_schematic_svg <- function(LOC_in=in_selected_RV$LOC,BAR_in=in_selected_RV$BAR){
    # tst_val <- paste(in_selected_RV$LOC, in_selected_RV$BAR, sep = "_")
    tst_val <- paste(LOC_in, BAR_in, sep = "_")
    tags$img(src = paste0("www/images/svg/basic route schematic/", tst_val, ".svg"),width = "100%")
}
