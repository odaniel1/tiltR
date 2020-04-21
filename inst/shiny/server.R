# Example from https://shiny.rstudio.com/gallery/faithful.html
function(input, output,session) {

  # ---- SET REACTIVE VALUES ---------------------------------------------------

  react_vals <- reactiveValues(
    brew_df = NULL,
    calibration_n = 0,
    calibration_df = tibble(day = numeric(0), sg_points = numeric(0)),
    calibration_plot_layer = NULL,
    outlier_rows = FALSE, # for excluding in stan model
    sg_post = NULL, # posterior SG plot layer
    fg_line = NULL,
    fg_annotate = NULL
    )

  # ---- GET DATA --------------------------------------------------------------

  observeEvent(input$url,
    react_vals$brew_df <- get_tilt_data(input$url)
  )

  # ---- CALIBRATION POINTS ----------------------------------------------------

  # Set first calibration point to be sg at day = 0.
  observe({
    updateNumericInput(session=session, inputId = "calDT_0",
                    value= react_vals$brew_df$day[1] %>% as.character())
    updateNumericInput(session=session,inputId = "calVal_0",
                       value = round(react_vals$brew_df$sg_points[1]))
  })

  # Add calibration points
  observeEvent(input$addCal, ignoreNULL = FALSE, ignoreInit = TRUE,{

    react_vals$calibration_n <- react_vals$calibration_n + 1
    cal_n <- react_vals$calibration_n

    insertUI( immediate = TRUE,
              selector = "#calibrationPoints", where = "beforeEnd",

              splitLayout(
                  numericInput(paste0("calDT_",cal_n), "Day:", value = react_vals$brew_df$day[1]),
                  numericInput(paste0("calVal_",cal_n), label="SG (points)", value = round(react_vals$brew_df$sg_points[1]))
              )
    )
  })

  # Apply calibration
  observeEvent(input$applyCal,{

    # define calibration point data frame
    react_vals$calibration_df <- tibble::tibble(
      day = purrr::map(0:react_vals$calibration_n, ~input[[paste0("calDT_",.)]]) %>% unlist,
      sg_points = purrr::map(0:react_vals$calibration_n, ~input[[paste0("calVal_",.)]]) %>% unlist
    )

    # adjust brew_df for calibration
    react_vals$brew_df <- purrr::reduce(
      .init = react_vals$brew_df,
      .x = purrr::transpose(react_vals$calibration_df),
      .f = function(data,cal){calibrate_data(data, cal$day, cal$sg_points)}
    )

    # create calibration point plot layer
    react_vals$calibration_plot_layer <-
      geom_point(data= react_vals$calibration_df, aes(day,sg_points), color = "black", size = 2)
  }
  )

  # ---- OUTLIER TOGGLE --------------------------------------------------------

  observeEvent(input$sg_click, {
    res <- nearPoints(react_vals$brew_df, input$sg_click, allRows = TRUE)
    react_vals$outlier_rows <- xor(react_vals$outlier_rows, res$selected_)
  })

  # Toggle points that are brushed, when button is clicked
  observeEvent(input$outlier_toggle, {
    res <- brushedPoints(react_vals$brew_df, input$sg_brush, allRows = TRUE)
    react_vals$outlier_rows <- xor(react_vals$outlier_rows, res$selected_)
  })

  # Reset all points
  observeEvent(input$outlier_reset, {
    react_vals$outlier_rows <- FALSE
  })

  # ---- STAN MODEL ------------------------------------------------------------

  # on button click, fit model
  observeEvent( input$run_stan,{

    # remove outliers
    data_stan <- react_vals$brew_df[!react_vals$outlier_rows, ]

    data_stan <- data_stan %>%
      filter(sg_points > 0) %>%
      mutate(
        hr = floor( day * 24 )
      ) %>%
      group_by(hr) %>%
      slice(1) %>%
      ungroup()

    # forecast days + days already elapsed
    forecast_days <- input$forecast_days + ceiling( max(react_vals$brew_df$day) )

    # fit model
    stan_fit <- logistic_model_stan(
      data = data_stan, pars = c("day", "sg_points"), fg_ant = input$fg_ant, fg_sd = 0.01, days = forecast_days,
      chains = 4, iter = 1500, cores = 4)

    print(rstan::summary(stan_fit, pars = c("b", "M", "fg","nu", "sigma")))

    # posterior sg quantiles
    data_post <- sg_posterior(stan_fit)

    # create posterior interval plot layer
    react_vals$sg_post <-  geom_line(data = data_post, aes(t,  sg_post, group = quantile, linetype = range))

    # create target fg layer
    ann_label <- glue::glue("Target final gravity points: {input$fg_ant}")
    react_vals$fg_line <- geom_hline(yintercept = input$fg_ant,  color = "darkgrey")
    react_vals$fg_annotate <- annotate("text", x = 0.5, y = input$fg_ant +1, label =ann_label, size = 5.5)
  })

  # ---- OUTPUT PLOTS ----------------------------------------------------------

  # sg plot
  output$sg_plot <- renderPlot({

    plot_df <- react_vals$brew_df
    plot_df$outlier <- react_vals$outlier_rows

    p <- ggplot() +
      geom_point(data = plot_df, aes(day, sg_points, color = outlier)) +
      unit_x_scale() +
      dec_y_scale() +
      labs(x = "Fermentation Time (days)", y = "SG (points)") +
      tiltR_theme()

    p <- p + react_vals$sg_post
    p <- p + react_vals$fg_line + react_vals$fg_annotate
    p <- p + react_vals$calibration_plot_layer
    return(p)
  })

  # temperature plot
  output$temp_plot <- renderPlot({
    p <- ggplot(react_vals$brew_df, aes(day, Temp)) +
      geom_line() +
      unit_x_scale() +
      labs(x = "Fermentation Time (days)", y = "Temp (C)") +
      tiltR_theme()
    return(p)
  })
}
