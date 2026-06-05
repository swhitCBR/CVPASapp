draw_estimates_panel_ui <- function(){
    shinydashboardPlus::box(
      id = "est_box_ui",
      title = tags$a(span(h2("Estimates",style="margin-top: 0px; margin-bottom: 0px;"))),
      # title = shiny::HTML("Estimates"),
      solidHeader = TRUE,
      status = "primary",
      collapsible = T,
      collapsed = FALSE,
      width = 12,
      fluidRow(
        h3(
        # strong(
          # paste0("Overall Survival Probability"),
          paste0("Survival Probability (all routes)"),
          style = "color:#28547A;text-decoration: underline;padding-left: 10px;"
        # )
        
      )
        ,
        div(
          ##ALT## wide vs. long display of overall survival plots
          # style = "padding-left: 20px;padding-right:10px;"
          # equal width alternative
          style = "display:grid; grid-template-columns: repeat(3, 1fr); padding-left: 10px;padding-right:10px;",
          # flexible width alternative
          # style = "display:flex; justify-content: space-evenly;padding-left: 10px;padding-right:10px;",

          div(
            h4(
              strong(paste0(
                "Head of Old River to Chipps Island (HOR-CHP)"
              )),
              style = "padding-left:10px;"
            ),
            div(
              div(
                tags$ul(
                  style = "padding-left:15px;",
                  tags$li(
                    "Survival from Head of Old River to Chipps Island across (all routes).",
                    #ALT# longer version
                    style = "margin-left:25px;"
                  )
                )
              ),
              plotOutput("HOR_TCJ_pred_ggpplt_dup1a", height = "400px")
            )
          ),
          div(
            h4(
              strong(paste0(
                "Head of Old River to Turner Cut Junction (HOR-TRN)"
              )),
              style = "padding-left:10px;"
            ),
            div(
              div(
                tags$ul(
                  style = "padding-left:15px;",
                  tags$li(
                    "Survival from Head of Old River to Turner Cut Junction via San Joaquin R.",
                    style = "margin-left:25px;"
                  )
                )
              ),
              plotOutput("HOR_TCJ_pred_ggpplt_dup1b", height = "400px")
            )
          ),
          div(
            h4(
              strong(paste0(
                "Turner Cut Junction to Chipps Island (TRN-CHP)"
              )),
              style = "padding-left:10px;"
            ),
            div(
              div(
                tags$ul(
                  style = "padding-left:15px;",
                  tags$li(
                    "Survival from Turner Cut Junction to Chipps Island (all routes)",
                    style = "margin-left:25px;"
                  )
                )
              ),
              plotOutput("HOR_TCJ_pred_ggpplt_dup1c", height = "400px")
            )
          )
        ),
        div(
          style = "padding-left: 10px;",
          h3(
            paste0("Route Usage"),
            style = "color:#006400;text-decoration: underline;"
        ),
          column(
            width = 6,
            h4(
              strong(paste0("San Joaquin River vs. Old River  (all routes)")),
              style = "padding-left:10px;"
            ),
            # plotOutput("HOR_TCJ_pred_ggpplt_dup1a", height = "400px")
            plotOutput("HOR_TCJ_pred_ggpplt_dup1d", height = "400px")
          ),
          column(
            width = 6,
            h4(
              strong(paste0("San Joaquin River vs. Turner Cut")),
              style = "padding-left:10px;"
            ),
            plotOutput("HOR_TCJ_pred_ggpplt_dup1d2", height = "400px")
          )
          # Blue: #4E79A7 (Steel Blue)Orange: #F28E2B (Burnt Orange)
        )
      ),
      footer = div(
        #p("hdfsl")
        # footer(
        div(
          style = "padding-left: 10px;",
                    h3(
            paste0("More Information"),
            style = "color:black;text-decoration: underline;"
        ),
          # h4(strong(
          #   paste0("More Information"),
          #   style = "color:black;text-decoration: underline;"
          # )),
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
              plotOutput("doy_ins_ggpplt", height = "400px"),
              plotOutput("HOR_TCJ_pred_ggpplt", height = "400px")
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