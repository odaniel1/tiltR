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
unit_x_scale <- function(...) scale_x_continuous(
  expand = c(0, 0),
  limits = function(x) c(0, ceiling(max(x))),
  breaks = function(x) seq(floor(min(x)), ceiling(max(x)))
)

dec_y_scale <- function(...) scale_y_continuous(
  expand = c(0, 0),
  limits = function(y) c(0, 10 * ceiling(max(y)/10)),
  breaks = function(y) seq(0, 10 * ceiling(max(y)/10), by = 10)
)

# tiltR plot theme
tiltR_theme <- function(){
  theme_minimal() +
  theme(
    text = element_text(size=16),
    legend.pos = "none"
    )
}
