rm( list = ls() )

# Make sure you're in the "Code" directory.

load('../Data/data-full/dataFull.Rdata')
source('myFunctions.R')

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
latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)


# Map of the Haynesville-Bossier Shale
shale.map <- get.basemap('google', 'hybrid', lonBounds, latBounds)
shale.map <- shale.map + geom_polygon(data = df, aes(long, lat), size = 0.3, alpha = 0.3,
                                      color = 'white', fill = 'orangered4')
shale.map <- shale.map + ggtitle('The Haynesville-Bossier Shale')
shale.map <- shale.map + xlab('Longitude') + ylab('Latitude')
pdf(file = '../Figures/Haynesville-Bossier.pdf', height = 6.5, width = 8)
print(shale.map)
dev.off()


# Sattelite view.
sat.map <- get.basemap('google', 'satellite', lonBounds, latBounds)
sat.map <- sat.map + ggtitle('Satellite View')
sat.map <- sat.map + xlab('Longitude') + ylab('Latitude')
pdf(file = '../Figures/satellite.pdf', height = 6.5, width = 8)
print(sat.map)
dev.off()


