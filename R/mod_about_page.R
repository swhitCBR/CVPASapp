#' Dashboard UI Function
#'
#' @description module for about page of app
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = HTML("About"),
        # title = HTML("Purpose <small style ='font-size:0.6em; color: white;'>BetaVersion.Nov05</small>"),
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        column(
          width = 5, 
          style="display:inline"
          # ,
          # shiny::includeMarkdown(system.file("app/www/about/about_left_col_text.md", package = "CVPASapp"))
          ,
          HTML('
          <p style="display:inline"> The CVPAS tool produces survival predictions for hypothetical groups of Juvenile Steelhead based on the timing of their arrival at key river junctions and a data set defining environmental and operational conditions. The main quantities of interest that the CVPAS tool produces are predicted probabilities of survival to Chipps Island (CHP) from either the head of Old River (HOR) or Turner Cut Junction (TCJ) denoted as <span class="MathJax_Preview" style="color: inherit;"></span><span class="MathJax" id="MathJax-Element-1-Frame" tabindex="0" style="position: relative;" data-mathml="&lt;math xmlns=&quot;http://www.w3.org/1998/Math/MathML&quot;&gt;&lt;msub&gt;&lt;mi&gt;S&lt;/mi&gt;&lt;mrow class=&quot;MJX-TeXAtom-ORD&quot;&gt;&lt;mi&gt;H&lt;/mi&gt;&lt;mi&gt;O&lt;/mi&gt;&lt;mi&gt;R&lt;/mi&gt;&lt;mo&gt;&amp;#x2212;&lt;/mo&gt;&lt;mi&gt;C&lt;/mi&gt;&lt;mi&gt;H&lt;/mi&gt;&lt;mi&gt;P&lt;/mi&gt;&lt;/mrow&gt;&lt;/msub&gt;&lt;/math&gt;" role="presentation"><nobr aria-hidden="true"><span class="math" id="MathJax-Span-1" style="width: 5.598em; display: inline-block;"><span style="display: inline-block; position: relative; width: 4.646em; height: 0px; font-size: 120%;"><span style="position: absolute; clip: rect(1.313em, 1004.65em, 2.622em, -999.997em); top: -2.199em; left: 0em;"><span class="mrow" id="MathJax-Span-2"><span class="msubsup" id="MathJax-Span-3"><span style="display: inline-block; position: relative; width: 4.646em; height: 0px;"><span style="position: absolute; clip: rect(3.098em, 1000.66em, 4.17em, -999.997em); top: -3.985em; left: 0em;"><span class="mi" id="MathJax-Span-4" style="font-family: MathJax_Math-italic;">S<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span style="display: inline-block; width: 0px; height: 3.991em;"></span></span><span style="position: absolute; top: -3.807em; left: 0.598em;"><span class="texatom" id="MathJax-Span-5"><span class="mrow" id="MathJax-Span-6"><span class="mi" id="MathJax-Span-7" style="font-size: 70.7%; font-family: MathJax_Math-italic;">H<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span class="mi" id="MathJax-Span-8" style="font-size: 70.7%; font-family: MathJax_Math-italic;">O</span><span class="mi" id="MathJax-Span-9" style="font-size: 70.7%; font-family: MathJax_Math-italic;">R</span><span class="mo" id="MathJax-Span-10" style="font-size: 70.7%; font-family: MathJax_Main;">−</span><span class="mi" id="MathJax-Span-11" style="font-size: 70.7%; font-family: MathJax_Math-italic;">C<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span class="mi" id="MathJax-Span-12" style="font-size: 70.7%; font-family: MathJax_Math-italic;">H<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span class="mi" id="MathJax-Span-13" style="font-size: 70.7%; font-family: MathJax_Math-italic;">P<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span></span></span><span style="display: inline-block; width: 0px; height: 3.991em;"></span></span></span></span></span><span style="display: inline-block; width: 0px; height: 2.205em;"></span></span></span><span style="display: inline-block; overflow: hidden; vertical-align: -0.354em; border-left: 0px solid; width: 0px; height: 1.218em;"></span></span></nobr><span class="MJX_Assistive_MathML" role="presentation"><math xmlns="http://www.w3.org/1998/Math/MathML"><msub><mi>S</mi><mrow class="MJX-TeXAtom-ORD"><mi>H</mi><mi>O</mi><mi>R</mi><mo>−</mo><mi>C</mi><mi>H</mi><mi>P</mi></mrow></msub></math></span></span><script type="math/tex" id="MathJax-Element-1">S_{HOR-CHP}</script> and <span class="MathJax_Preview" style="color: inherit;"></span><span class="MathJax" id="MathJax-Element-2-Frame" tabindex="0" style="position: relative;" data-mathml="&lt;math xmlns=&quot;http://www.w3.org/1998/Math/MathML&quot;&gt;&lt;msub&gt;&lt;mi&gt;S&lt;/mi&gt;&lt;mrow class=&quot;MJX-TeXAtom-ORD&quot;&gt;&lt;mi&gt;T&lt;/mi&gt;&lt;mi&gt;C&lt;/mi&gt;&lt;mi&gt;J&lt;/mi&gt;&lt;mo&gt;&amp;#x2212;&lt;/mo&gt;&lt;mi&gt;C&lt;/mi&gt;&lt;mi&gt;H&lt;/mi&gt;&lt;mi&gt;P&lt;/mi&gt;&lt;/mrow&gt;&lt;/msub&gt;&lt;/math&gt;" role="presentation"><nobr aria-hidden="true"><span class="math" id="MathJax-Span-14" style="width: 5.301em; display: inline-block;"><span style="display: inline-block; position: relative; width: 4.408em; height: 0px; font-size: 120%;"><span style="position: absolute; clip: rect(1.313em, 1004.41em, 2.622em, -999.997em); top: -2.199em; left: 0em;"><span class="mrow" id="MathJax-Span-15"><span class="msubsup" id="MathJax-Span-16"><span style="display: inline-block; position: relative; width: 4.408em; height: 0px;"><span style="position: absolute; clip: rect(3.098em, 1000.66em, 4.17em, -999.997em); top: -3.985em; left: 0em;"><span class="mi" id="MathJax-Span-17" style="font-family: MathJax_Math-italic;">S<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span style="display: inline-block; width: 0px; height: 3.991em;"></span></span><span style="position: absolute; top: -3.807em; left: 0.598em;"><span class="texatom" id="MathJax-Span-18"><span class="mrow" id="MathJax-Span-19"><span class="mi" id="MathJax-Span-20" style="font-size: 70.7%; font-family: MathJax_Math-italic;">T<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span class="mi" id="MathJax-Span-21" style="font-size: 70.7%; font-family: MathJax_Math-italic;">C<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span class="mi" id="MathJax-Span-22" style="font-size: 70.7%; font-family: MathJax_Math-italic;">J<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span class="mo" id="MathJax-Span-23" style="font-size: 70.7%; font-family: MathJax_Main;">−</span><span class="mi" id="MathJax-Span-24" style="font-size: 70.7%; font-family: MathJax_Math-italic;">C<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span class="mi" id="MathJax-Span-25" style="font-size: 70.7%; font-family: MathJax_Math-italic;">H<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span><span class="mi" id="MathJax-Span-26" style="font-size: 70.7%; font-family: MathJax_Math-italic;">P<span style="display: inline-block; overflow: hidden; height: 1px; width: 0.063em;"></span></span></span></span><span style="display: inline-block; width: 0px; height: 3.991em;"></span></span></span></span></span><span style="display: inline-block; width: 0px; height: 2.205em;"></span></span></span><span style="display: inline-block; overflow: hidden; vertical-align: -0.354em; border-left: 0px solid; width: 0px; height: 1.218em;"></span></span></nobr><span class="MJX_Assistive_MathML" role="presentation"><math xmlns="http://www.w3.org/1998/Math/MathML"><msub><mi>S</mi><mrow class="MJX-TeXAtom-ORD"><mi>T</mi><mi>C</mi><mi>J</mi><mo>−</mo><mi>C</mi><mi>H</mi><mi>P</mi></mrow></msub></math></span></span><script type="math/tex" id="MathJax-Element-2">S_{TCJ-CHP}</script>. 
					  There are multiple paths that juvenile Steelhead may take as they migrate through the Delta and route usage and reach-specific survival rates vary based on environmental conditions and estimates are generated from an ensemble of survival and routing models were obtained by analyzing acoustic telemetry detection data gathered between 2011 and 2016 
            <div class="hover-container">
            <a href="https://cdnsciencepub.com/doi/10.1139/cjfas-2020-0467" target="_blank" rel="noopener noreferrer">(Buchanan et al. 2021; </a>
              <div class="hover-modal">
                <a href="https://cdnsciencepub.com/doi/10.1139/cjfas-2020-0467" target="_blank">
                  <img src="https://onlinelibrary.wiley.com/cms/asset/dc48586b-dbfd-4240-9705-8c08ee6cf914/nafm.v44.3.cover.jpg" class="modal-img">
                </a>
              </div>
            </div>
            <div class="hover-container">
            <a href="" target="_blank" rel="noopener noreferrer">Buchanan et al. 2024) </a>.
              <div class="hover-modal">
                <a href="https://onlinelibrary.wiley.com/doi/full/10.1002/nafm.11005" target="_blank">
                  <img src="https://onlinelibrary.wiley.com/cms/asset/dc48586b-dbfd-4240-9705-8c08ee6cf914/nafm.v44.3.cover.jpg" class="modal-img">
                </a>
              </div>
            </div>
						 </p>
        ')
          #PLACEHOLDER
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
          width = 7,
          # shinydashboard::box(
            tags$img(
            src = "www/about/simple_route_image.png",
            style = "width: 80%; height: 80%;",# border: 2px solid #024c63;",
            name = "Schematic view of junctions and routes through the south Delta ",
            alt = "Routes bifurcate at the Head of Old River and with the path along the San Joaquin River splitting again at Turner Cut junction; all paths converge prior to reaching Chipps Island"
            )
            ,
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

#' about Server Functions
#'
#' @noRd
mod_about_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
  })
}

