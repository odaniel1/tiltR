# Example from https://shiny.rstudio.com/gallery/faithful.html
function(input, output) {

  data<- reactive({
    data <- get_tilt_data(input$url)

    data <- data %>% dplyr::mutate(
      SG = dplyr::case_when(
        Color == "ORANGE" ~ SG + input$cal_orange,
        Color == "GREEN"  ~ SG + input$cal_green
      )
    )
    return(data)
  })

  output$sg_plot <- renderPlot({
    p <- ggplot(data(), aes(Timepoint, SG)) +
      geom_point(color= "grey") +
      geom_smooth() +
      scale_y_continuous(limits = c(1,1.070)) +
      theme_minimal()
    return(p)
  })


  output$temp_plot <- renderPlot({
    p <- ggplot(data(), aes(Timepoint, Temp)) +
      geom_line() +
      theme_minimal()
    return(p)
  })
}
