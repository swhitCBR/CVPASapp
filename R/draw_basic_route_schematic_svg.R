
draw_basic_route_schematic_svg <- function(
    title_in="schematic showing routes two branching routes through the South Delta, with junctions at the Head of Old River and Turner Cut",
    LOC_in="HOR",
    BAR_in="Out",
    # LOC_in=in_selected_RV$LOC,
    # BAR_in=in_selected_RV$BAR,
    # width_in="100%",
    # height_in="100%",
    height_in="250px",
    img_pth="www/images/svg"
){
        concat_name <- paste(LOC_in, BAR_in, sep = "_")
        tags$img(
            src =  file.path(
                img_pth,
                "basic_route_schematic",
                paste0(concat_name, ".svg")
            ),
        title=title_in,
        # width = width_in,
        height = height_in
    )
}

