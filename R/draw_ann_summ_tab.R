draw_ann_summ_tab <- function(input_year=input$year_picker){
  # DT::renderDataTable(
    DT::datatable(
      ann_HORbar_WYT_data_TAB(),
      selection = "none",
      colnames = c("Year", "Water Year Type", "HOR Barrier", "Used in Models"),
      rownames = FALSE,
      options = list(
        info = FALSE,
        # dom = "t",
        # pageLength = 14,
        dom = '<"<"bottom"ip>', # only the bottom
        pageLength = 6,
        stripeClasses = list(),
        pagingType = "simple",
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '100%'});",
          "$(this.api().table().body()).css({'font-size': '80%'});",
          "$(this.api().table().footer()).css({'font-size': '80%'});",
          "}"
        )
      )
    ) |>
      DT::formatStyle(
        'WYT',
        backgroundColor = DT::styleEqual(wyt_type_opt, utils_get_WYT_cols_vec())
      ) |>
      DT::formatStyle(
        'Model',
        color = DT::styleEqual(c("Yes", "No"), c("#035a00", "#690202"))
      ) |>
      # only works for background color and not border
      DT::formatStyle(
        columns = c('Year', 'WYT', 'barrier', 'Model'),
        target = "row",
        backgroundColor = DT::styleEqual(
          input_year,
          c("#61bce6")
        )
      ) |>
      DT::formatStyle(
        'Year',
        borderTop = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        ),
        borderBottom = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        ),
        borderLeft = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        )
      ) |>
      DT::formatStyle(
        'barrier',
        'Year',
        borderTop = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        ),
        borderBottom = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        )
      ) |>
      DT::formatStyle(
        'WYT',
        'Year',
        borderTop = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        ),
        borderBottom = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        )
      ) |>
      DT::formatStyle(
        'Model',
        'Year',
        borderTop = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        ),
        borderBottom = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        ),
        borderRight = DT::styleEqual(
          input_year,
          c("4px solid #024c63")
        )
      )
  # )
  
}