draw_about_page_ui <- function(){
  tagList(
    tags$head(
      tags$style("
        .hover-container {
          position: relative;
          /* FIX: Changed from inline-block to inline so text wraps seamlessly without jumping to a new line */
          display: inline;
        }
        .hover-modal {
          display: none;
          position: absolute;
          top: 125%; 
          left: 0;
          width: 600px; 
          max-width: 90vw;
          background-color: #ffffff;
          color: #333333;
          border: 1px solid #dddddd;
          box-shadow: 0px 8px 16px rgba(0,0,0,0.15);
          padding: 20px;
          border-radius: 8px;
          z-index: 1050; 
          text-align: left; 
          /* FIX: Explicitly set font-weight and white-space to normalize parent inline constraints */
          font-weight: normal;
          white-space: normal;
        }
        /* Targets the child modal display when hovering over the wrapper container */
        .hover-container:hover .hover-modal {
          display: block;
        }
        .modal-img {
          width: 100%;
          height: 100%;
          border-radius: 4px;
          margin-top: 10px;
          border: 1px solid #eee;
        }
      ")
    ),
    fluidRow(
      shinydashboard::box(
        title = HTML("About"),
        # title = HTML("Purpose <small style ='font-size:0.6em; color: white;'>BetaVersion.Nov05</small>"),
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        column(
          width = 7, 
          style="display:inline"
          # ,
          # shiny::includeMarkdown(system.file("app/www/about/about_left_col_text.md", package = "CVPASapp"))
          ,
          HTML('
          <p style="display:inline"> 
          The CVPAS tool produces survival predictions for hypothetical
          groups of Juvenile Steelhead based on the timing of their arrival at key river junctions
          and a data set defining environmental and operational conditions. The main quantities of
          interest that the CVPAS tool produces are predicted probabilities of survival to Chipps 
          Island (CHP) from either Head of Old River (HOR) or Turner Cut Junction (TCJ) denoted
          as \\(S_{HOR-CHP} \\) and \\(S_{TCJ-CHP} \\) 
					There are multiple paths that juvenile Steelhead may take as they migrate through the Delta
					and route usage and reach-specific survival rates vary based on environmental conditions and
					estimates are generated from an ensemble of survival and routing models were obtained by
					analyzing acoustic telemetry detection data gathered between 2011 and 2016 
            <div class="hover-container">
            <a href="https://cdnsciencepub.com/doi/10.1139/cjfas-2020-0467" target="_blank" rel="noopener noreferrer">(Buchanan et al. 2021; </a>
              <div class="hover-modal">
                <a href="https://cdnsciencepub.com/doi/10.1139/cjfas-2020-0467" target="_blank">
                  <img src="www/about/Buchanan_etal_2021_topofpage.png" class="modal-img">
                </a>
              </div>
            </div>
            <div class="hover-container">
            <a href="" target="_blank" rel="noopener noreferrer">Buchanan et al. 2024) </a>.
              <div class="hover-modal">
                <a href="https://onlinelibrary.wiley.com/doi/full/10.1002/nafm.11005" target="_blank">
                  <img src="www/about/Buchanan2024_topofpage.png" class="modal-img">
                </a>
              </div>
            </div>
						 </p>
        ')
          ,
          div(
            style="display:inline-flex; margin-right: 10px",
            div(
              class = "thumbnail-section",
              actionButton("goto_inputs_butt", "Select Inputs",
                           class = "btn btn-primary",
                           style = "font-size: 13px; color: white; padding: 12px; text-align: center;"
              ) 
            )
            ,
            div(
              class = "thumbnail-section",
              actionButton("goto_met_ref_butt", "Methods and References",
                           class = "btn btn-primary",
                           style = "font-size: 13px; color: white;margin-left: 10px;
                          padding: 12px; text-align: center;"
              ) 
            )
          )
        )
        ,
        column(
          width = 5,
          # ORIGINAL SCHEMATIC
          # tags$img(
          #   src = "www/about/simple_route_image.png",
          #   style = "width: 80%; height: 80%;",# border: 2px solid #024c63;",
          #   name = "Schematic view of junctions and routes through the south Delta ",
          #   alt = "Routes bifurcate at the Head of Old River and with the path along the San Joaquin River splitting again at Turner Cut junction; all paths converge prior to reaching Chipps Island"
          # ),
          tags$img(
            src = "www/images/svg/overall_survival_schematic_abbrev.svg",
            style = "width: 80%; height: 80%;",# border: 2px solid #024c63;",
            name = "Schematic view of junctions and routes through the south Delta ",
            alt = "Routes bifurcate at the Head of Old River and with the path along the San Joaquin River splitting again at Turner Cut junction; all paths converge prior to reaching Chipps Island"
          )
          ,
          # tags$img(
          #   src = "www/images/svg/overall_survival_schematic_full_nms.svg",
          #   style = "width: 80%; height: 80%;",# border: 2px solid #024c63;",
          #   name = "Schematic view of junctions and routes through the south Delta ",
          #   alt = "Routes bifurcate at the Head of Old River and with the path along the San Joaquin River splitting again at Turner Cut junction; all paths converge prior to reaching Chipps Island"
          # )
          # ,
          tags$caption(
            shiny::withMathJax(shiny::includeMarkdown(system.file("app/www/about_fig_cap.md",
                                                                  package = "CVPASapp"))
            )
          )
        )
      )
    )
  )
}
