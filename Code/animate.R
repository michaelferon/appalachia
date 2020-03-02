rm( list = ls() )

# Make sure you're in the 'Code' directory.
# Expect very long run-times for this script.

load('../Data/data-full/dataFull.Rdata')
source('myFunctions.R')

library(config)    # Configuration details.
library(lubridate) # Handling date-times.
library(ggplot2)   # Plotting.
library(reshape2)  # Reshaping data.
library(fields)
library(ggmap)     # Spatial plotting using maps.
library(zoo)       # For rollmean().
library(gganimate) # Animations!
library(gifski)    # Rendering animations.

# Need a Google API key for some of the stuff below.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

# ggmap stuff.
latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)
basemap.hybrid <- get.basemap('google', 'hybrid', lonBounds, latBounds)




## Dividing data spatially into RESO^2 quadrants.
RESO <- 64
latTicks <- seq(latBounds[1], latBounds[2], length = RESO + 1)
lonTicks <- seq(lonBounds[1], lonBounds[2], length = RESO + 1)
X <- assign.quadrants(X, latTicks, lonTicks, RESO)

# Here, lat and lon are the midpoints of each quadrant.
lat <- rev(rollmean(latTicks, 2))
lon <- rollmean(lonTicks, 2)





## DATA AGGREGATION
dateTimes.months <- my.month(dateTimes)
uniqueMonths <- unique(dateTimes.months)
data.month <- data.aggregate(X, dateTimes.months, uniqueMonths, 'Month')
monthlyMeans <- means.matrix(X, data.month, dateTimes.months, uniqueMonths, RESO)

zLim <- c(1800, 1900)
test <- melt(monthlyMeans, id.vars = 'Date')
colnames(test) <- c('date', 'quadrant', 'methane')
test$longitude <- rep(rep(lon, each = 21), times = RESO)
test$latitude <- rep(rep(lat, each = 21), each = RESO)
temp <- month(test$date)
temp[temp < 10] <- paste('0', temp[temp < 10], sep='')
test$date <- paste(year(test$date), temp, sep='-')
test <- na.omit(test)
test$methane[test$methane <= zLim[1]] <- zLim[1]
test$methane[test$methane >= zLim[2]] <- zLim[2]

## Test with smaller file.
# test <- subset(test, date == '2018-05' | date == '2018-12')

## Avoid blurring.
test$group <- seq_len(nrow(test))

width <- abs(mean(diff(lon)))
height <- abs(mean(diff(lat)))
lTitle <- expression(paste('CH'[4], ' Column Average Mixing Ratio [ppbv]'))

g <- basemap.hybrid
g <- g + geom_tile(data = test, height = height, width = width, alpha = 0.65,
                   mapping = aes(x = longitude, y = latitude, fill = methane, group = group))
g <- g + scale_fill_gradientn(colors = tim.colors(64), limits = zLim)
g <- g + xlab('Longitude') + ylab('Latitude')
g <- g + theme(legend.key.width = unit(2.25, 'cm'))
g <- g + theme(legend.position = 'bottom')
g <- g + guides(fill = guide_colorbar(title = lTitle, title.position = 'top'))
g <- g + ggtitle('{closest_state}')


anim <- g + transition_states(date, transition_length = 2, state_length = 1)
anim <- anim + enter_fade() + exit_fade()


animate(anim, duration = 20, nframes = 200, res = 200, width = 1500, height = 1050)
anim_save(filename = '../Figures/animations/out.gif', animation = last_animation())

# animate(anim, nframes = 5)
# anim_save(filename = './test.gif', animation = last_animation())






