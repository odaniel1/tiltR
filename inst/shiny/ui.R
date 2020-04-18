# Example from https://shiny.rstudio.com/gallery/faithful.html
bootstrapPage(theme = shinythemes::shinytheme("flatly"),

  titlePanel("TiltR"),

  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "url", label = "Googlesheets URL:",
                value = "https://docs.google.com/spreadsheets/d/1KO_JR32M-2LGv0C39hpWJBHAAGU9wvqXPrXecxLCQC4/edit?ts=5e97754e#gid=734290882"),

      actionButton(inputId = "run_stan", label = "Run Forecast"),

      numericInput(inputId = "cal_orange", label = "Orange Calibration Offset:", value = 0),

      numericInput(inputId = "cal_green", label = "Green Calibration Offset:", value = 0),

    ),

    mainPanel(
      plotOutput(outputId = "sg_plot", height = "300px"),

      plotOutput(outputId = "temp_plot", height = "300px"),
    )
  )
)
