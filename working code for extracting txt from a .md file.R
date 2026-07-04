# because this is within a renderUI, changing the input$tab value rewrites the sidebar content
output$sidebar_text <- renderUI({
  shiny::HTML(
    md_txt_extract(
      md_addr = "inst/app/www/sidebar/sidebar.md",
      header_ref = paste0("# ", tab_selected()),
      asHTML_frag = TRUE
    )
  )
  # draw_ibox_ui(box_content_in = "daily_values_box")
  # box_content_in
})