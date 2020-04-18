library(ggplot2)
library(dplyr)
library(tibble)
library(googlesheets4)
library(rstan)

# Prepare beer name lookup for selectInput
beers <- data(tiltURLs)
beers <- split(tiltURLs, tiltURLs$status)
beers <- purrr::map(beers, ~setNames(.$url, .$name))


# set color palette
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = c("grey", "hotpink"))
}

# tiltR plot theme
tiltR_theme <- function(){
  theme_minimal() +
  theme(legend.pos = "none")
}
