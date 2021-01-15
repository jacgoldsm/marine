library(shiny)
library(shiny.semantic)
library(dplyr)
library(geodist)
library(leaflet)
library(glue)
library(shinycssloaders)

ships <- vroom::vroom("ships.csv")

choices <- sort(unique(ships$ship_type))
