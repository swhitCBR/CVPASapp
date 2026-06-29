draw_estimates_panel_ui <- function() {
  shinydashboardPlus::box(
    id = "est_box_ui",
    title = tags$a(span(h2(
      "Estimates",
      style = "margin-top: 0px; margin-bottom: 0px;"
    ))),
    # title = shiny::HTML("Estimates"),
    solidHeader = TRUE,
    status = "primary",
    collapsible = T,
    collapsed = FALSE,
    width = 12,
    fluidRow(
      h3(
        paste0("Overall Survival"),
        style = "color:#28547A;text-decoration: underline;padding-left: 10px;"
      ),
      div(
        ##ALT## wide vs. long display of overall survival plots
        # style = "padding-left: 20px;padding-right:10px;"
        # equal width alternative
        style = "display:grid; grid-template-columns: repeat(3, 1fr); padding-left: 10px;padding-right:10px;",
        # flexible width alternative
        # style = "display:flex; justify-content: space-evenly;padding-left: 10px;padding-right:10px;",
        div(
          # HTML(
          #   '<h4 style="font-weight: normal; padding-left: 10px;">
          #     <strong>Total Survival: </strong>
          #      HOR to CHP (all routes)</h4>'
          # ),

          span(h4(
            style="font-weight: normal; padding-left: 10px; display:inline-flex",
              HTML('
               <span style="margin-right:5px">
                <strong>Total Survival: </strong>HOR to CHP
            <div class="hover-container">
            <a>(all routes) </a>
              <div class="hover-modal">
                <p> Estimate accounts for the possibility that smolts are diverted from the main San Joaquin River along the route
                the Old/Middle River route (ORE), which diverges at the Head of Old River junction (HOR), or into Turner Cut at the 
                Turner Cut junction (TCJ) further downstream. </p>
              </div>
            </div>

              </span>'
            ),
              draw_ibutt_dropdown_ui(
                inputId_in = "total_surv_ibutt",
                ibox_content_label="total_surv_ibox"))

          ),

          
          div(
            div(
              tags$ul(
                style = "padding-left:15px;",
                tags$li(
                  "Survival from Head of Old River to Chipps Island across (all routes).",
                  style = "margin-left:25px;"
                )
              )
            ),
            # plotOutput("HOR_TCJ_pred_ggpplt_dup1a", height = "400px"),
            # plotOutput("HOR_TCJ_pred_ggpplt_dup1a"),
            plotOutput("HOR_TCJ_pred_ggpplt_s_tot")
            

          )
        )
      ),
      hr(),
      h3(
        paste0("Reach Survival"),
        style = "color:#28547A;text-decoration: underline;padding-left: 10px;"
      ),
      div(
        ##ALT## wide vs. long display of overall survival plots
        # style = "padding-left: 20px;padding-right:10px;"
        # equal width alternative
        style = "display:grid; grid-template-columns: repeat(3, 1fr); padding-left: 10px;padding-right:10px;",
        # flexible width alternative
        # style = "display:flex; justify-content: space-evenly;padding-left: 10px;padding-right:10px;",
        div(
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>Reach Survival: </strong>
               HOR to TCJ (via SJR)</h4>'
          ),
          div(
            # div(
            #   tags$ul(
            #     style = "padding-left:15px;",
            #     tags$li(
            #       "Survival from Head of Old River to Chipps Island across (all routes).",
            #       style = "margin-left:25px;"
            #     )
            #   )
            # ),
            plotOutput("HOR_TCJ_pred_ggpplt_rs1", height = "400px")
            # p("placeholder")
          )
        ),
        div(
          # HTML('
          #      <span style="margin-right:5px">
          #       <strong>Reach Survival: </strong>
          #      TCJ to CHP
          #   <div class="hover-container">
          #   <a>(all routes) </a>
          #     <div class="hover-modal">
          #       <p> Estimate accounts for the possibility that smolts are diverted from the main San Joaquin River along the route
          #       the Old/Middle River route (ORE), which diverges at the Head of Old River junction (HOR), or into Turner Cut at the 
          #       Turner Cut junction (TCJ) further downstream. </p>
          #     </div>
          #   </div>

          #     </span>'
          #   ),
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>Reach Survival: </strong>
               TCJ to CHP (all routes)</h4>'
          ),
          div(
            # div(
            #   tags$ul(
            #     style = "padding-left:15px;",
            #     tags$li(
            #       "Survival from Head of Old River to Chipps Island across (all routes).",
            #       style = "margin-left:25px;"
            #     )
            #   )
            # ),
            # p("placeholder"),
            plotOutput("HOR_TCJ_pred_ggpplt_rs_2", height = "400px")
          )
        )
      ),
      hr(),
      h3(
        paste0("Route-Specific Survival"),
        style = "color:#28547A;text-decoration: underline;padding-left: 10px;"
      ),
      div(
        id = "route_spec_surv_panel",
        ##ALT## wide vs. long display of overall survival plots
        # style = "padding-left: 20px;padding-right:10px;"
        # equal width alternative
        style = "display:grid; grid-template-columns: repeat(2, 1fr); padding-left: 10px;padding-right:10px;",
        # flexible width alternative
        # style = "display:flex; justify-content: space-evenly;padding-left: 10px;padding-right:10px;",

        div(
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>Route-Specific Survival: </strong>
               HOR to CHP via SJR route</h4>'
          ),
          div(
            # div(
            #   tags$ul(
            #     style = "padding-left:15px;",
            #     tags$li(
            #       "Survival from Head of Old River to Turner Cut Junction via San Joaquin R.",
            #       style = "margin-left:25px;"
            #     )
            #   )
            # ),
            plotOutput("HOR_TCJ_pred_ggpplt_dup1b", height = "400px")
          )
        ),
        div(
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>Route-Specific Survival: </strong>
               HOR to CHP via ORE route</h4>'
          ),
          div(
            # div(
            #   tags$ul(
            #     style = "padding-left:15px;",
            #     tags$li(
            #       "Survival from Head of Old River to Chipps Island via Old/Middle River Route",
            #       style = "margin-left:25px;"
            #     )
            #   )
            # ),
            # p("placeholder")
            plotOutput("HOR_TCJ_pred_ggpplt_dup1a", height = "400px")
          )
        ),
        div(
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>Route-Specific Survival: </strong>
               TCJ to CHP via SJR route</h4>'
          ),
          div(
            # div(
            #   tags$ul(
            #     style = "padding-left:15px;",
            #     tags$li(
            #       "Survival from Head of Old River to Turner Cut Junction via San Joaquin R.",
            #       style = "margin-left:25px;"
            #     )
            #   )
            # ),
            plotOutput("HOR_TCJ_pred_ggpplt_dup1e", height = "400px")
          )
        ),
        div(
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>Route-Specific Survival: </strong>
               TCJ to CHP via TRN route</h4>'
          ),
          div(
            # div(
            #   tags$ul(
            #     style = "padding-left:15px;",
            #     tags$li(
            #       "Survival from Head of Old River to Turner Cut Junction via San Joaquin R.",
            #       style = "margin-left:25px;"
            #     )
            #   )
            # ),
            plotOutput("HOR_TCJ_pred_ggpplt_dup1c", height = "400px")
          )
        )
      ),
      hr(),
      div(
        style = "padding-left: 10px;",
        id = "route_usage_panel",
        h3(
          paste0("Route Usage"),
          style = "color:#006400;text-decoration: underline;"
        ),
        column(
          width = 6,
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>Route Usage: </strong>
               at HOR</h4>'
          ),
          # h4(
          #   strong(paste0("San Joaquin River vs. Old River  (all routes)")),
          #   style = "padding-left:10px;"
          # ),
          # plotOutput("HOR_TCJ_pred_ggpplt_dup1a", height = "400px")
          plotOutput("HOR_TCJ_pred_ggpplt_dup1d", height = "400px")
        ),
        column(
          width = 6,
          HTML(
            '<h4 style="font-weight: normal; padding-left: 10px;">
              <strong>Route Usage: </strong>
               at TCJ</h4>'
          ),
          plotOutput("HOR_TCJ_pred_ggpplt_dup1d2", height = "400px")
        )
        # Blue: #4E79A7 (Steel Blue)Orange: #F28E2B (Burnt Orange)
      )
    ),
    footer = div(
      style = "display: none;",
      #p("hdfsl")
      # footer(
      div(
        id = "more_info_panel",
        style = "padding-left: 10px;",
        h3(
          paste0("More Information"),
          style = "color:black;text-decoration: underline;"
        )
      ),
      div(
        style = "padding-left: 10px;",
        tags$details(
          id = "hor_tcj_surv_deet",
          open = NULL,
          style = "margin-top:15px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
          tags$summary(
            title = "Click to open or close",
            "Route-Specific Survival",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
          ),
          h4(
            "Head of Old River to Turner Cut via the San Joaquin River",
            style = "margin-left:20px;"
          ),
          div(
            style = "display:flex;",
            plotOutput("doy_ins_ggpplt", height = "400px")#,
            # plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
          ),
          h4(
            "Turner Cut Chipps Island via the San Joaquin River",
            style = "margin-left:20px;"
          ),
          div(
            style = "display:flex;",
            plotOutput("doy_ins_ggpplt_dup1", height = "400px"),
            plotOutput("HOR_TCJ_pred_ggpplt_dup3", height = "400px")
          ),
          h4(
            "Head of Old River to Chipps Island via the Old and Middle rivers",
            style = "margin-left:20px;"
          ),
          # p("Head of Old River to Turner Cut",style="margin-left:25px;"),
          div(
            style = "display:flex;",
            plotOutput("doy_ins_ggpplt_dup2", height = "400px"),
            plotOutput("HOR_TCJ_pred_ggpplt_dup1", height = "400px")
          )
        ),
        tags$details(
          id = "hor_chp_ore_surv_deets",
          open = NULL,
          style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
          tags$summary(
            title = "Click to open or close",
            "Model Details",
            style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
                  padding-bottom: 2px;background-color:#ddd;"
          ),
          h4("HOR-TCJ Survival", style = "margin-left:20px;"),
          div(
            style = "display:flex;",
            p("placeholder")
            # plotOutput("doy_ins_ggpplt_dup3", height = "400px")
            # ,
            # plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
          )
        )
      )
    )
    # )
  )
  # )
}
# })

# output$estimates_panel_ui <- renderUI({
#     shinydashboardPlus::box(
#       id = "est_box_ui",
#       title = tags$a(span(h2("Estimates",style="margin-top: 0px; margin-bottom: 0px;"))),
#       # title = shiny::HTML("Estimates"),
#       solidHeader = TRUE,
#       status = "primary",
#       collapsible = T,
#       collapsed = FALSE,
#       width = 12,
#       fluidRow(
#         h3(
#         # strong(
#           # paste0("Overall Survival Probability"),
#           paste0("Survival Probability (all routes)"),
#           style = "color:#28547A;text-decoration: underline;padding-left: 10px;"
#         # )

#       )
#         ,
#         div(
#           ##ALT## wide vs. long display of overall survival plots
#           # style = "padding-left: 20px;padding-right:10px;"
#           # equal width alternative
#           style = "display:grid; grid-template-columns: repeat(3, 1fr); padding-left: 10px;padding-right:10px;",
#           # flexible width alternative
#           # style = "display:flex; justify-content: space-evenly;padding-left: 10px;padding-right:10px;",

#           div(
#             h4(
#               strong(paste0(
#                 "Head of Old River to Chipps Island (HOR-CHP)"
#               )),
#               style = "padding-left:10px;"
#             ),
#             div(
#               div(
#                 tags$ul(
#                   style = "padding-left:15px;",
#                   tags$li(
#                     "Survival from Head of Old River to Chipps Island across (all routes).",
#                     #ALT# longer version
#                     style = "margin-left:25px;"
#                   )
#                 )
#               ),
#               plotOutput("HOR_TCJ_pred_ggpplt_dup1a", height = "400px")
#             )
#           ),
#           div(
#             h4(
#               strong(paste0(
#                 "Head of Old River to Turner Cut Junction (HOR-TRN)"
#               )),
#               style = "padding-left:10px;"
#             ),
#             div(
#               div(
#                 tags$ul(
#                   style = "padding-left:15px;",
#                   tags$li(
#                     "Survival from Head of Old River to Turner Cut Junction via San Joaquin R.",
#                     style = "margin-left:25px;"
#                   )
#                 )
#               ),
#               plotOutput("HOR_TCJ_pred_ggpplt_dup1b", height = "400px")
#             )
#           ),
#           div(
#             h4(
#               strong(paste0(
#                 "Turner Cut Junction to Chipps Island (TRN-CHP)"
#               )),
#               style = "padding-left:10px;"
#             ),
#             div(
#               div(
#                 tags$ul(
#                   style = "padding-left:15px;",
#                   tags$li(
#                     "Survival from Turner Cut Junction to Chipps Island (all routes)",
#                     style = "margin-left:25px;"
#                   )
#                 )
#               ),
#               plotOutput("HOR_TCJ_pred_ggpplt_dup1c", height = "400px")
#             )
#           )
#         ),
#         div(
#           style = "padding-left: 10px;",
#           h3(
#             paste0("Route Usage"),
#             style = "color:#006400;text-decoration: underline;"
#         ),
#           column(
#             width = 6,
#             h4(
#               strong(paste0("San Joaquin River vs. Old River  (all routes)")),
#               style = "padding-left:10px;"
#             ),
#             # plotOutput("HOR_TCJ_pred_ggpplt_dup1a", height = "400px")
#             plotOutput("HOR_TCJ_pred_ggpplt_dup1d", height = "400px")
#           ),
#           column(
#             width = 6,
#             h4(
#               strong(paste0("San Joaquin River vs. Turner Cut")),
#               style = "padding-left:10px;"
#             ),
#             plotOutput("HOR_TCJ_pred_ggpplt_dup1d2", height = "400px")
#           )
#           # Blue: #4E79A7 (Steel Blue)Orange: #F28E2B (Burnt Orange)
#         )
#       ),
#       footer = div(
#         #p("hdfsl")
#         # footer(
#         div(
#           style = "padding-left: 10px;",
#                     h3(
#             paste0("More Information"),
#             style = "color:black;text-decoration: underline;"
#         ),
#           # h4(strong(
#           #   paste0("More Information"),
#           #   style = "color:black;text-decoration: underline;"
#           # )),
#         ),
#         div(
#           style = "padding-left: 10px;",
#           tags$details(
#             id = "hor_tcj_surv_deet",
#             open = NULL,
#             style = "margin-top:15px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
#             tags$summary(
#               title = "Click to open or close",
#               "Route-Specific Survival",
#               style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
#                   padding-bottom: 2px;background-color:#ddd;"
#             ),
#             h4(
#               "Head of Old River to Turner Cut via the San Joaquin River",
#               style = "margin-left:20px;"
#             ),
#             div(
#               style = "display:flex;",
#               plotOutput("doy_ins_ggpplt", height = "400px"),
#               plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
#             ),
#             h4(
#               "Turner Cut Chipps Island via the San Joaquin River",
#               style = "margin-left:20px;"
#             ),
#             div(
#               style = "display:flex;",
#               plotOutput("doy_ins_ggpplt_dup1", height = "400px"),
#               plotOutput("HOR_TCJ_pred_ggpplt_dup3", height = "400px")
#             ),
#             h4(
#               "Head of Old River to Chipps Island via the Old and Middle rivers",
#               style = "margin-left:20px;"
#             ),
#             # p("Head of Old River to Turner Cut",style="margin-left:25px;"),
#             div(
#               style = "display:flex;",
#               plotOutput("doy_ins_ggpplt_dup2", height = "400px"),
#               plotOutput("HOR_TCJ_pred_ggpplt_dup1", height = "400px")
#             )
#           ),
#           tags$details(
#             id = "hor_chp_ore_surv_deets",
#             open = NULL,
#             style = "margin-top:10px; padding: 0px; color:#337ab7 ; border:solid; border-width: 1px ; background-color:white",
#             tags$summary(
#               title = "Click to open or close",
#               "Model Details",
#               style = "font-size: 18px; font-size: 18px; padding-left: 4px; padding-top: 2px;
#                   padding-bottom: 2px;background-color:#ddd;"
#             ),
#             h4("HOR-TCJ Survival", style = "margin-left:20px;"),
#             div(
#               style = "display:flex;",
#               p("placeholder")
#               # plotOutput("doy_ins_ggpplt_dup3", height = "400px")
#               # ,
#               # plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
#             )
#           )
#         )
#       )
#       # )
#     )
#   })
