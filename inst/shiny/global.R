library(ggplot2)
library(dplyr)
library(tibble)
library(googlesheets4)
library(rstan)

# Prepare beer name lookup for selectInput
beers <- data(tiltURLs)
beers <- split(tiltURLs, tiltURLs$status)
beers <- purrr::map(beers, ~setNames(.$url, .$name))
