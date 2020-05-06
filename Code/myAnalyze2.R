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
library(dplyr)
library(viridis)
library(sp)
library(rgdal)

# Need a Google API key for some of the stuff below.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)
basemap.hybrid <- get.basemap('google', 'hybrid', lonBounds, latBounds)



quilt.plot(X$longitude, X$latitude, X$qa_value == 1.0,
           col = viridis(64))#, zlim = c(0, 1))
US(add = TRUE, lwd = 3.0)



RESO <- 100
latTicks <- seq(latBounds[1], latBounds[2], length = RESO + 1)
lonTicks <- seq(lonBounds[1], lonBounds[2], length = RESO + 1)
X <- assign.quadrants(X, latTicks, lonTicks, RESO)

lat <- rev(rollmean(latTicks, 2))
lon <- rollmean(lonTicks, 2)


## Plotting proportion of high qa-values.
df.qa <- as.data.frame(matrix(NA, nrow = RESO^2, ncol = 3))
value <- tapply(X$qa_value, X$quadrant, function(x) mean(x == 1.0))
ind <- as.numeric(names(value))

colnames(df.qa) <- c('longitude', 'latitude', 'value')
df.qa$longitude <- rep(lon, times = RESO)
df.qa$latitude <- rep(lat, each = RESO)
df.qa$value[ind] <- value
df.qa <- na.omit(df.qa)

width <- abs(mean(diff(lon)))
height <- abs(mean(diff(lat)))

pdf(file = '../Figures/qa.pdf', height = 3.0, width = 4.5)
basemap.hybrid +
  geom_tile(data = df.qa, height = height, width = width, alpha = 0.75,
            mapping = aes(x = longitude, y = latitude, fill = value)) +
  scale_fill_gradientn(colors = viridis(100), limits = c(0, 0.8)) +
  xlab('Longitude') + ylab('Latitude') +
  theme(legend.key.height = unit(1.0, 'cm')) +
  labs(fill = '')
dev.off()
rm(df.qa, value, ind, width, height)






## HEATMAPS with high-qa only.
Z <- X[X$qa_value == 1.0, ]
## Dividing data spatially into RESO^2 quadrants.
RESO <- 100
latTicks <- seq(latBounds[1], latBounds[2], length = RESO + 1)
lonTicks <- seq(lonBounds[1], lonBounds[2], length = RESO + 1)
Z <- assign.quadrants(Z, latTicks, lonTicks, RESO)

# Here, lat and lon are the midpoints of each quadrant.
lat <- rev(rollmean(latTicks, 2))
lon <- rollmean(lonTicks, 2)

## DATA AGGREGATION
dateTimes.days <- my.yday(Z$time_utc)
uniqueDays <- unique(dateTimes.days)
data.day <- data.aggregate(Z, dateTimes.days, uniqueDays, 'Day')

full <- prop.matrix(uniqueDays, data.day, RESO)
image(x = lon, y = rev(lat), z = t(apply(full$mat, 2, rev)),
      main = 'Daily Elevated Methane Levels', xlab = 'Longitude', ylab = 'Latitude')
US(add = TRUE, lwd = 2)

g <- ggmap.prop.matrix(full$mat, lat, lon, basemap.hybrid, 'Daily')
pdf(file = '../Figures/proportions/high-qa/propmat-high-qa.pdf', height = 3.0, width = 4.5)
print(g)
dev.off()




## HAYNESVILLE
## All qa.
shp.data <- readOGR(dsn = '../Data/shapefile/shale-plays/ShalePlays_US_EIA_Sep2019.shp')
h <- which(shp.data$Shale_play == 'Haynesville-Bossier')
shp.data <- shp.data[h, ]
haynesville <- fortify(shp.data)
in.haynesville <- as.logical(point.in.polygon(X$longitude, X$latitude,
                                              haynesville$long, haynesville$lat))

df.h <- X[in.haynesville, ]
df.out <- X[!in.haynesville, ]
quilt.plot(df.h$longitude, df.h$latitude, df.h$methane_mixing_ratio_bias_corrected,
           xlim = range(X$longitude), ylim = range(X$latitude))
US(add = TRUE, lwd = 2)

dateTimes.days <- my.yday(df.h$time_utc)
uniqueDays <- unique(dateTimes.days)
data.day <- data.aggregate(df.h, dateTimes.days, uniqueDays, 'Day')

full <- prop.matrix(uniqueDays, data.day, RESO)
image(x = lon, y = rev(lat), z = t(apply(full$mat, 2, rev)),
      main = 'Daily Elevated Methane Levels', xlab = 'Longitude', ylab = 'Latitude')
US(add = TRUE, lwd = 2)

g <- ggmap.prop.matrix(full$mat, lat, lon, basemap.hybrid, 'Daily')
print(g)

## High qa only.
in.haynesville <- as.logical(point.in.polygon(Z$longitude, Z$latitude,
                                              haynesville$long, haynesville$lat))
df.h <- Z[in.haynesville, ]
df.out <- Z[!in.haynesville, ]
quilt.plot(df.h$longitude, df.h$latitude, df.h$methane_mixing_ratio_bias_corrected,
           xlim = range(X$longitude), ylim = range(X$latitude))
US(add = TRUE, lwd = 2)

dateTimes.days <- my.yday(df.h$time_utc)
uniqueDays <- unique(dateTimes.days)
data.day <- data.aggregate(df.h, dateTimes.days, uniqueDays, 'Day')

full <- prop.matrix(uniqueDays, data.day, RESO)
image(x = lon, y = rev(lat), z = t(apply(full$mat, 2, rev)),
      main = 'Daily Elevated Methane Levels', xlab = 'Longitude', ylab = 'Latitude')
US(add = TRUE, lwd = 2)









