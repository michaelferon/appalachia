rm( list = ls() )

# Make sure you're in the 'Code' directory.

load('../Data/data-full/dataFullMask.Rdata')
source('myFunctions.R')

library(config)    # Configuration details.
library(lubridate) # Handling date-times.
library(ggplot2)   # Plotting.
library(reshape2)  # Reshaping data.
library(fields)    # Plotting spatial data.
library(ggmap)     # Spatial plotting using maps.
library(zoo)       # For rollmean().

# Need a Google API key for some of the stuff below.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)
basemap.hybrid <- get.basemap('google', 'hybrid', lonBounds, latBounds)




## Dividing data spatially into RESO^2 quadrants.
RESO <- 10000
latTicks <- seq(latBounds[1], latBounds[2], length = RESO + 1)
lonTicks <- seq(lonBounds[1], lonBounds[2], length = RESO + 1)
X <- assign.quadrants(X, latTicks, lonTicks, RESO)

# Here, lat and lon are the midpoints of each quadrant.
lat <- rev(rollmean(latTicks, 2))
lon <- rollmean(lonTicks, 2)
