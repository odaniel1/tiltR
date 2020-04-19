library(ggplot2)
library(dplyr)
library(tibble)
library(googlesheets4)
library(rstan)
library(shinydashboard)
library(shinyjs)

# Prepare beer name lookup for selectInput
beers <- data(tiltURLs)
beers <- split(tiltURLs, tiltURLs$status)
beers <- purrr::map(beers, ~setNames(.$url, .$name))


# set color palette
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = wesanderson::wes_palette("Royal1")[1:2])
}

# set unit scale; https://stackoverflow.com/a/39041905/829967
unit_x_scale <- function(...) scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x))))

# tiltR plot theme
tiltR_theme <- function(){
  theme_minimal() +
  theme(legend.pos = "none")
}
