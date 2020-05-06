rm( list = ls() )

# Make sure you're in the 'Code' directory.

load('../Data/data-full/dataFull.Rdata')
source('myFunctions.R')

library(config)    # Configuration details.
library(lubridate) # Handling date-times.
library(ggplot2)   # Plotting.
library(reshape2)  # Reshaping data.
library(fields)    # Plotting spatial data.
library(ggmap)     # Spatial plotting using maps.
library(googleway) # Elevation data.
library(zoo)       # For rollmean().

# Need a Google API key for some of the stuff below.
 info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
googleway::set_key(info$google_api_key)
rm(info)

STATS <- TRUE # Print summary statistics?
MAKE_GG_PLOTS <- FALSE # Wise to keep this FALSE -- see ~/Figures/gg3DayPlots.
MAKE_QUILT_PLOTS <- FALSE # This as well -- see ~/Figures/3DayPlots.
MAKE_TIME_SERIES <- TRUE # Leave this FALSE.

# ggmap stuff.
latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)
basemap.hybrid <- get.basemap('google', 'hybrid', lonBounds, latBounds)




## Dividing data spatially into RESO^2 quadrants.
RESO <- 2
latTicks <- seq(latBounds[1], latBounds[2], length = RESO + 1)
lonTicks <- seq(lonBounds[1], lonBounds[2], length = RESO + 1)
X <- assign.quadrants(X, latTicks, lonTicks, RESO)

# Here, lat and lon are the midpoints of each quadrant.
lat <- rev(rollmean(latTicks, 2))
lon <- rollmean(lonTicks, 2)




## DATA AGGREGATION
dateTimes.days <- my.yday(dateTimes)
dateTimes.weeks <- my.week(dateTimes)
dateTimes.months <- my.month(dateTimes)

dateTimes.weeks[dateTimes.weeks == '2018-53'] <- '2018-52'
dateTimes.weeks[dateTimes.weeks == '2019-53'] <- '2019-52'

uniqueDays <- unique(dateTimes.days)
uniqueWeeks <- unique(dateTimes.weeks)
uniqueMonths <- unique(dateTimes.months)

data.day <- data.aggregate(X, dateTimes.days, uniqueDays, 'Day')
data.week <- data.aggregate(X, dateTimes.weeks, uniqueWeeks, 'Week')
data.month <- data.aggregate(X, dateTimes.months, uniqueMonths, 'Month')




## MEANS
dailyMeans <- means.matrix(X, data.day, dateTimes.days, uniqueDays, RESO)
weeklyMeans <- means.matrix(X, data.week, dateTimes.weeks, uniqueWeeks, RESO)
monthlyMeans <- means.matrix(X, data.month, dateTimes.months, uniqueMonths, RESO)




## Time-series Plots.
# Plotting weekly means.
if (MAKE_TIME_SERIES) {
  start <- as.POSIXct('2018-05-01', tz = 'UTC')
  end <- as.POSIXct('2020-04-01', tz = 'UTC')
  weekDF <- melt(weeklyMeans, id.vars = 'Date')
  g.week <- ggplot(data = weekDF, aes(x = Date, y = value, col = variable)) + geom_line()
  g.week <- g.week + xlab('Time') + ylab('Methane Mixing Ratio') + labs(col = 'Quadrant')
  g.week <- g.week + theme_bw()
  g.week <- g.week + ylim(1550.667, 1905.837)
  g.week <- g.week + xlim(start, end)
  
  # Plotting daily means.
  dayDF <- melt(dailyMeans, id.vars = 'Date')
  g.day <- ggplot(data = dayDF, aes(x = Date, y = value, col = variable)) + geom_line()
  g.day <- g.day + xlab('Time') + ylab('Methane Mixing Ratio') + labs(col = 'Quadrant')
  g.day <- g.day + theme_bw()
  g.day <- g.day + ylim(1550.667, 1905.837)
  g.day <- g.day + xlim(start, end)
  
  if (RESO == 2) {
    pdf(file = '../Figures/time-series/ts-weekly.pdf', height = 3.5, width = 10.0)
    print(g.week)
    dev.off()
    
    pdf(file = '../Figures/time-series/ts-daily.pdf', height = 3.5, width = 10.0)
    print(g.day)
    dev.off()
  }
  
  rm(weekDF, dayDF, g.week, g.day, start, end)
}




## Some Summary Statistics.
if (STATS) {
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, length))
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, mean))
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, sd))
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, IQR))
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, mad))
}




## QUILT-PLOTS
xLim <- lonBounds + c(-0.1, 0.1)
yLim <- latBounds + c(-0.065, 0.065)
zLim <- c(1750, 1950)
zLim <- c(1710, 1910)


## Quilt plots.
# if (MAKE_QUILT_PLOTS) {
#   for (month in 1:length(data.month)) {
#     quilt.plot.month(data.month[[month]], xLim, yLim, zLim)
#   }
#   for (month in 1:length(data.month)) {
#     df <- data.month[[month]]
#     df <- df[df$qa_value == 1.0, ]
#     quilt.plot.month(df, xLim, yLim, zLim, outdir = '../Figures/3DayPlots/high-qa/')
#   }
#   
#   rm(month, df)
# }
# 
# if (MAKE_GG_PLOTS & RESO == 64) {
#   for (month in 1:length(data.month)) {
#     ggmap.plot.month(data.month[[month]], basemap.hybrid, lat, lon, RESO, zLim)
#   }
#   for (month in 18:length(data.month)) {
#     df <- data.month[[month]]
#     df <- df[df$qa_value == 1.0, ]
#     ggmap.plot.month(df, basemap.hybrid, lat, lon, RESO, zLim, outdir = '../Figures/gg3DayPlots/high-qa/')
#   }
#   
#   rm(month, df)
# }

rm(xLim, yLim, zLim)




## Seasonal distributions (for four quadrants).
if (RESO == 2) {
  seasons <- c(rep('Winter', 2), rep('Spring', 3), rep('Summer', 3), rep('Fall', 3), 'Winter')
  c1 <- rgb(30/255, 178/255, 243/255)
  c2 <- rgb(64/255, 180/255, 31/255)
  c3 <- rgb(246/255, 119/255, 112/255)
  c4 <- rgb(183/255, 158/255, 33/255)
  my.colors <- c(c1, c2, c3, c4)

  start <- as.POSIXlt('2018-12-01 00:00:00', tz = 'UTC')
  end <- as.POSIXlt('2019-12-01 00:00:00', tz = 'UTC')
  df <- subset(X, start <= time_utc & time_utc < end)
  df$season <- factor(seasons[month(df$time_utc)], levels = c('Winter', 'Spring', 'Summer', 'Fall'))
  df$quadrant <- factor(df$quadrant, levels = 1:4, labels = paste('Q', 1:4, sep=''))

  g <- ggplot(data = df, mapping = aes(x = methane_mixing_ratio_bias_corrected)) +
    geom_density(mapping = aes(color = season)) +
    facet_wrap(~quadrant, ncol = 2) + theme_bw() +
    xlim(1750, 1950) + ylab('Density Estimate') + xlab('Methane Mixing Ratio') +
    labs(color = 'Season', title = 'Seasonal Distributions by Quadrant',
         subtitle = 'December 2018 - November 2019') +
    scale_color_manual(values = my.colors)
  pdf(file = '../Figures/other/dist1.pdf', height = 5.25, width = 9)
  print(g)
  dev.off()

  rm(df)

  df <- X
  df$season <- factor(seasons[month(df$time_utc)], levels = c('Winter', 'Spring', 'Summer', 'Fall'))
  df$quadrant <- factor(df$quadrant, levels = 1:4, labels = paste('Q', 1:4, sep=''))
  q <- ggplot(data = df, mapping = aes(x = methane_mixing_ratio_bias_corrected)) +
    geom_density(mapping = aes(color = season)) +
    facet_wrap(~quadrant, ncol = 2) + theme_bw() +
    xlim(1750, 1950) + ylab('Density Estimate') + xlab('Methane Mixing Ratio') +
    labs(color = 'Season', title = 'Seasonal Distributions by Quadrant',
         subtitle = 'May 2018 - January 2020') +
    scale_color_manual(values = my.colors)
  pdf(file = '../Figures/other/dist2.pdf', height = 5.25, width = 9)
  print(q)
  dev.off()


  rm(seasons, start, end, df, c1, c2, c3, c4, my.colors, g, q)
}










