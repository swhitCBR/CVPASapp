library(webshot2)
library(magick)

golem_app_object <- CVPASapp::run_app()


#' Slice a Shiny App into High-Resolution Print Strips
#'
#' @param app_path Path to the Shiny application directory or URL.
#' @param output_dir Directory where the image strips should be saved.
#' @param vwidth The base browser layout viewport width (default 1440).
#' @param vheight The total browser height to render and slice (default 9000).
#' @param target_width_in Physical print width in inches (default 10.7).
#' @param target_height_in Physical print height in inches (default 7.5).
#' @param dpi Target print resolution DPI (default 600).
#' @param prefix File name prefix for output strips.
#' @param delay_secs seconds to wait until screenshot is taken
#'
slice_app_for_print <- function(app_path, 
                                    output_dir = ".", 
                                    vwidth = 1440, 
                                    vheight = 9000, 
                                    target_width_in = 10.7, 
                                    target_height_in = 7.5, 
                                    dpi = 600,
                                    prefix = "print_strip",
                                    delay_secs=3) {
  
  # 1. Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 2. Calculate pixel requirements
  pixel_width  <- target_width_in * dpi   # e.g., 10.7 * 600 = 6420
  pixel_height <- target_height_in * dpi  # e.g., 7.5 * 600 = 4500
  
  # 3. Calculate fractional zoom needed to hit target width perfectly
  calculated_zoom <- pixel_width / vwidth
  
  # 4. Define temporary file path for the raw full-page shot
  temp_full_shot <- file.path(output_dir, paste0("TEMP_full_page_", prefix, ".png"))
  
  message(paste0("Capturing app at ", vwidth, "vw with zoom factor ", round(calculated_zoom, 4), "..."))
  
  # 5. Capture the canvas
  webshot2::appshot(
    app = app_path, 
    file = temp_full_shot, 
    vwidth = vwidth, 
    vheight = vheight, 
    zoom = calculated_zoom,
    delay = delay_secs             # Wait 3 seconds for charts to render
  )
  
  # 6. Read image into memory and get actual dimensions
  img <- magick::image_read(temp_full_shot)
  info <- magick::image_info(img)
  
  total_height <- info$height
  current_top  <- 0
  strip_number <- 1
  
  message(paste0("Slicing into ", dpi, " DPI strips (", pixel_width, "x", pixel_height, "px)..."))
  
  # 7. Loop and slice
  while (current_top < total_height) {
    # Define current bounding crop box
    geometry_string <- paste0(pixel_width, "x", pixel_height, "+0+", current_top)
    
    # Crop the segment
    strip <- magick::image_crop(img, geometry_string)
    
    # Define output file path
    out_file <- file.path(output_dir, paste0(prefix, "_", strip_number, ".png"))
    
    # Stamp DPI metadata and save
    magick::image_write(
      strip, 
      path = out_file,
      format = "png",
      density = paste0(dpi, "x", dpi)
    )
    
    # Advance parameters
    current_top <- current_top + pixel_height
    strip_number <- strip_number + 1
  }
  
  # 8. Clean up the massive temporary full-page file
  if (file.exists(temp_full_shot)) {
    file.remove(temp_full_shot)
  }
  
  message(paste0("Success! Generated ", strip_number - 1, " print-ready strips in: ", output_dir))
  return(invisible(TRUE))
}


slice_app_for_print(
  app_path = golem_app_object,#"path/to/shiny_app",
  output_dir = "output/high_res_strips",
  vwidth = 1440,
  vheight = 12000,        # Make this large enough to fit your app contents
  target_width_in = 10.7,
  target_height_in = 7.5,
  dpi = 600,
  prefix = "project_alpha"
)







# OLD CODE


# # Screenshot the app object
# # appshot(app = my_app, file = "shiny_app_object.png")


# # 2. Extract the Shiny app object from your golem package function
# golem_app_object <- CVPASapp::run_app()

# webshot2::appshot(
#   app = golem_app_object,
#   file = "custom_size.png",
#   vwidth = 1920,        # Width of viewport
#   vheight = 1080,       # Height of viewport
#   delay = 10             # Wait 3 seconds for charts to render
# )


# webshot2::appshot(
#   app = golem_app_object,
#   file = "custom_size2.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1009,       # Height of viewport
#   delay = 10,             # Wait 3 seconds for charts to render
#   zoom = 3
# )

# webshot2::appshot(
#   app = golem_app_object,
#   file = "inputs_shot.png",
#   vwidth = 1920,        # Width of viewport
#   vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   selector = "#input_box2",
#   zoom = 3
# )

# webshot2::appshot(
#   app = golem_app_object,
#   file = "inputs_shot2.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   selector = "#input_box2",
#   zoom = 1
# )

# webshot2::appshot(
#   app = golem_app_object,
#   file = "inputs_shot2.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   selector = "#input_box2",
#   zoom = 1
# )


# webshot2::appshot(
#   app = golem_app_object,
#   file = "horiz_slice2.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   # selector = "#input_box2",
#   zoom = 1,
#   cliprect = c(000, 9, 1440, 500) 
# )
# # 2475

# webshot2::appshot(
#   app = golem_app_object,
#   file = "horiz_slice_appropht.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   zoom = 3,
#   cliprect = c(000, 10, 1440, 1009) 
# )

# webshot2::appshot(
#   app = golem_app_object,
#   file = "horiz_slice_appropht.png",
#   vwidth = 1440,        # Width of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   zoom = 3,
#   cliprect = c(000, 10, 1440, 1009) 
# )

# webshot2::appshot(
#   app = golem_app_object,
#   file = "horiz_slice_appropht.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   zoom = 3,
#   cliprect = c(000, 10, 1440, 1009) 
# )



# webshot2::appshot(
#   app = golem_app_object,
#   file = "horiz_slice_appropht_a.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   zoom = 3,
#   cliprect = c(000, 10, 1440, 1009) 
# )




# webshot2::appshot(
#   app = golem_app_object,
#   file = "horiz_slice_appropht_b.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   zoom = 3,
#   cliprect = c(000, 10+999, 1440, 1009+999) 
# )

# webshot2::appshot(
#   app = golem_app_object,
#   file = "horiz_slice_approphtb_.png",
#   vwidth = 1440,        # Width of viewport
#   # vheight = 1080,       # Height of viewport
#   delay = 5,             # Wait 3 seconds for charts to render
#   zoom = 3,
#   cliprect = c(000, 1010, 1440, 2009) 
# )

# # seq(10,1009,999)
# # seq(1010,1998,999)
# # 1009-10
# # webshot2::appshot(
# #   app = golem_app_object,
# #   file = "shot2.png",
# #   vwidth = 1920,        # Width of viewport
# #   vheight = 1080,       # Height of viewport
# #   delay = 5,             # Wait 3 seconds for charts to render
# #   selector = "#doy_var_ggpplt"
# # )


