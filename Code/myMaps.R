rm( list = ls() )

# Make sure you're in the "Code" directory.

load('../Data/data-full/dataFull.Rdata')

library(lubridate)
library(ggplot2)
library(ggmap)
library(config)
library(sp)
library(rgdal)

# Get Google API key.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

# Get shapefile data, https://www.eia.gov/maps/maps.htm
shp.data <- readOGR(dsn = '../Data/shapefile/shale-plays/ShalePlays_US_EIA_Sep2019.shp')
h <- which(shp.data$Shale_play == 'Haynesville-Bossier')
shp.data <- shp.data[h, ]
df <- fortify(shp.data)

# Map boundaries.
latBounds <- c(min(X$latitude), max(X$latitude))
lonBounds <- c(min(X$longitude), max(X$longitude))


# Map of the Haynesville-Bossier Shale
map <- ggmap(get_map(location = c(mean(lonBounds), mean(latBounds)), source = 'google',
                     maptype = 'hybrid', zoom = 6))
map <- map + scale_x_continuous(limits = lonBounds, expand = c(0, 0))
map <- map + scale_y_continuous(limits = latBounds, expand = c(0, 0))
map <- map + geom_polygon(data = df, aes(long, lat), size = 0.3, alpha = 0.3,
                          color = 'white', fill = 'orangered4')
map <- map + ggtitle('The Haynesville-Bossier Shale')
map <- map + xlab('Longitude') + ylab('Latitude')
map


# Sattelite view.
map <- ggmap(get_map(location = c(mean(lonBounds), mean(latBounds)), source = 'google',
                     maptype = 'satellite', zoom = 6))
map <- map + scale_x_continuous(limits = lonBounds, expand = c(0, 0))
map <- map + scale_y_continuous(limits = latBounds, expand = c(0, 0))
map <- map + ggtitle('Satellite View')
map <- map + xlab('Longitude') + ylab('Latitude')
map


