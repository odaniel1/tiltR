# Example from https://shiny.rstudio.com/gallery/faithful.html
function(input, output) {

  # print(input$beerName)
  app_reactive_vals <- reactiveValues(
    outlier_rows = FALSE, # for excluding in stan model
    sg_post = NULL, # posterior SG plot layer
    fg_line = NULL,
    fg_annotate = NULL
    )

  # get / update data
  data <- reactive({
    data <- get_tilt_data(input$url)

    data <- data %>% dplyr::mutate(
      sg_points = dplyr::case_when(
        Color == "ORANGE" ~ sg_points + input$cal_orange,
        Color == "GREEN"  ~ sg_points + input$cal_green
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

    data_stan <- data_stan %>%
      mutate(
        hr = floor( day * 24 )
      ) %>%
      group_by(hr) %>%
      slice(1) %>%
      ungroup()

    # forecast days + days already elapsed
    forecast_days <- input$forecast_days + ceiling( max(data()$day) )

    # fit model
    stan_fit <- logistic_model_stan(
      data = data_stan, pars = c("day", "sg_points"), fg_ant = input$fg_ant, fg_sd = 0.01, days = forecast_days,
      chains = 4, iter = 1500, cores = 4)

    print(rstan::summary(stan_fit, pars = c("b", "M", "fg","nu", "sigma")))

    # posterior sg quantiles
    data_post <- sg_posterior(stan_fit)

    # create posterior interval plot layer
    app_reactive_vals$sg_post <-  geom_line(data = data_post, aes(t,  sg_post, group = quantile, linetype = range))

    # create target fg layer
    ann_label <- glue::glue("Target final gravity points: {input$fg_ant}")
    app_reactive_vals$fg_line <- geom_hline(yintercept = input$fg_ant,  color = "darkgrey")
    app_reactive_vals$fg_annotate <- annotate("text", x = 0.5, y = input$fg_ant +1, label =ann_label, size = 5.5)
  })

  # sg plot
  output$sg_plot <- renderPlot({

    plot_df <- data()
    plot_df$outlier <- app_reactive_vals$outlier_rows

    p <- ggplot() +
      geom_point(data = plot_df, aes(day, sg_points, color = outlier)) +
      unit_x_scale() +
      dec_y_scale() +
      labs(x = "Fermentation Time (days)", y = "SG (points)") +
      tiltR_theme()

    p <- p + app_reactive_vals$sg_post
    p <- p + app_reactive_vals$fg_line + app_reactive_vals$fg_annotate
    return(p)
  })

  # temperature plot
  output$temp_plot <- renderPlot({
    p <- ggplot(data(), aes(day, Temp)) +
      geom_line() +
      unit_x_scale() +
      labs(x = "Fermentation Time (days)", y = "Temp (C)") +
      tiltR_theme()
    return(p)
  })
}
