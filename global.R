library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(countrycode)
library(dplyr)
library(RColorBrewer)
library(shinyURL)
library(rclipboard)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(geojsonsf)

source('api.R')
#####################
# Global variables
#####################

commodity_groups <- c("crude_oil"="Crude oil",
                      "oil_products"="Oil products",
                      "oil_or_chemical"="Oil products",
                      "lng"="LNG",
                      "coal"="Coal",
                      "bulk"="Bulk (ex. coal)")
