# Example from https://shiny.rstudio.com/gallery/faithful.html
function(input, output) {

  # print(input$beerName)
  app_reactive_vals <- reactiveValues(
    outlier_rows = FALSE, # for excluding in stan model
    sg_post = NULL # posterior SG plot layer
    )

  # get / update data
  data <- reactive({
    data <- get_tilt_data(input$url)

    data <- data %>% dplyr::mutate(
      SG = dplyr::case_when(
        Color == "ORANGE" ~ SG + input$cal_orange,
        Color == "GREEN"  ~ SG + input$cal_green
      )
    )

    return(data)
  })

  observeEvent(input$sg_click, {
    res <- nearPoints(data(), input$sg_click, allRows = TRUE)
    app_reactive_vals$outlier_rows <- xor(app_reactive_vals$outlier_rows, res$selected_)
  })

  # Toggle points that are brushed, when button is clicked
  observeEvent(input$outlier_toggle, {
    res <- brushedPoints(data(), input$sg_brush, allRows = TRUE)
    app_reactive_vals$outlier_rows <- xor(app_reactive_vals$outlier_rows, res$selected_)
  })

  # Reset all points
  observeEvent(input$outlier_reset, {
    app_reactive_vals$outlier_rows <- FALSE
  })

  # on button click, fit model
  observeEvent( input$run_stan,{

    # remove outliers
    data_stan <- data()[!app_reactive_vals$outlier_rows, ]

    # forecast days + days already elapsed
    forecast_days <- input$forecast_days + ceiling( max(data()$day) )

    # fit model
    stan_fit <- logistic_model_stan(
      data = data_stan, pars = c("day", "SG"), fg_ant = input$fg_ant, fg_sd = 0.0005, days = forecast_days,
      chains = 4, iter = 2000, cores = 2)

    # posterior sg quantiles
    data_post <- sg_posterior(stan_fit)

    # create posterior interval plot layer
    app_reactive_vals$sg_post <-  geom_line(data = data_post, aes(t,  sg_post, group = quantile, linetype = range))
  })

  # sg plot
  output$sg_plot <- renderPlot({

    plot_df <- data()
    plot_df$outlier <- app_reactive_vals$outlier_rows

    p <- ggplot() +
      geom_point(data = plot_df, aes(day, SG, color = outlier)) +
      scale_y_continuous(limits = c(1,1.100)) +
      unit_x_scale() +
      tiltR_theme()

    p <- p + app_reactive_vals$sg_post
    return(p)
  })

  # temperature plot
  output$temp_plot <- renderPlot({
    p <- ggplot(data(), aes(day, Temp)) +
      geom_line() +
      unit_x_scale() +
      tiltR_theme()
    return(p)
  })
}
