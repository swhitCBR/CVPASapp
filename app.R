# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = FALSE) # production mode vs development mode
# CVPASapp::run_app(start_tab = "inputs",inputs_panel_collapse=F) #display.mode = "showcase") # add parameters here (if any)
CVPASapp::run_app(start_tab = "about",inputs_panel_collapse=F) #display.mode = "showcase") # add parameters here (if any)

