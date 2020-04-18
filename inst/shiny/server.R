# Example from https://shiny.rstudio.com/gallery/faithful.html
function(input, output) {

  # print(input$beerName)
  app_reactive_vals <- reactiveValues(sg_post = NULL)

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

  # on button click, fit model
  observeEvent( input$run_stan,{

    print("preparing data")

    data_stan <- data() %>% mutate(
      t = (difftime(Timepoint, min(Timepoint)))/(24 * 60*60),
      t = as.character(t) %>% as.numeric()
    )

    stan_fit <- logistic_model_stan(
      data = data_stan, pars = c("t", "SG"), fg_ant = input$fg_ant, fg_sd = 0.0005, days = input$forecast_days,
      chains = 2, iter = 2000, cores = 2)

    data_post <- sg_posterior(stan_fit) %>%
      mutate(
        Timepoint = min(data()$Timepoint) + (24 * 60^2) *t
      )

    app_reactive_vals$sg_post <-  geom_line(data = data_post, aes(Timepoint,  sg_post, group = quantile, linetype = range))
  })

  # sg plot
  output$sg_plot <- renderPlot({
    p <- ggplot(data(), aes(Timepoint, SG)) +
      geom_point(color= "grey") +
      scale_y_continuous(limits = c(1,1.070)) +
      theme_minimal()

    p <- p + app_reactive_vals$sg_post
    return(p)
  })

  # temperature plot
  output$temp_plot <- renderPlot({
    p <- ggplot(data(), aes(Timepoint, Temp)) +
      geom_line() +
      theme_minimal()
    return(p)
  })
}
