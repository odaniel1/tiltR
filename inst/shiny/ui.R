# Example from https://shiny.rstudio.com/gallery/faithful.html
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),

  dashboardBody(
    bootstrapPage(
      theme = shinythemes::shinytheme("flatly"),
      useShinyjs(),

      titlePanel("TiltR"),

      sidebarPanel(width = 3,

                   selectInput(inputId = "url", label = h4("Select Fermentation"), beers),

                   p("This app has been developed to..."),
                   div(style = "margin-top:-1.5em",hr()),

                   # Calibration adjustments
                   box(id = "calibrationBox", width = '800px', title = "Calibration",
                       collapsible = TRUE, collapsed = TRUE,
                       numericInput(inputId = "cal_orange", label = "Orange Calibration Offset (gravity points):", value = 0),
                       numericInput(inputId = "cal_green", label = "Green Calibration Offset (gravity points):", value = 0)
                   ),

                   # Forecasting
                   box(id = "forecastBox", width = '800px', title = "Forecast",
                       collapsible = TRUE, collapsed = TRUE,
                       numericInput(inputId = "fg_ant", "Target Final Gravity (points):", value = 16, min = 0),
                       numericInput(inputId = "forecast_days", "Forecast length (days)", value = 0, min = 0),
                       actionButton(inputId = "run_stan", label = "Run Forecast")
                   ),

                   collapseInput(inputId = "forecast_collapsed", boxId = "forecastBox")
      ),

      mainPanel(width = 9,
                tabsetPanel(
                  tabPanel("Specific Gravity",
                           plotOutput(outputId = "sg_plot", height = "600px",
                                      click = "sg_click",
                                      brush = brushOpts(id = "sg_brush")),
                           actionButton("outlier_toggle", "Toggle Outliers"),
                           actionButton("outlier_reset", "Reset Outliers")
                           ),
                  tabPanel("Temperature", plotOutput(outputId = "temp_plot", height = "600px"))
                )
      )
    )
  )
)

