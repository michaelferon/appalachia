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

# Need a Google API key for some of the stuff below.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
googleway::set_key(info$google_api_key)
rm(info)

STATS <- FALSE # Print summary statistics?
MAKE_GG_PLOTS <- TRUE # Wise to keep this FALSE -- see ~/Figures/gg3DayPlots.
MAKE_QUILT_PLOTS <- FALSE # This as well -- see ~/Figures/3DayPlots.

# ggmap stuff.
latBounds <- c(min(X$latitude), max(X$latitude))
lonBounds <- c(min(X$longitude), max(X$longitude))
basemap.hybrid <- get.basemap('google', 'hybrid')




## Dividing data spatially into RESO^2 quadrants.
RESO <- 64
latTicks <- rev(seq(min(X$latitude), max(X$latitude), length = RESO + 1))[-1]
lonTicks <- seq(min(X$longitude), max(X$longitude), length = RESO + 1)[-1]
X <- assign.quadrants(X, latTicks, lonTicks, RESO)

# Here, lat and lon are the midpoints of each quadrant.
lat <- rev(seq((latTicks[RESO] + latTicks[RESO - 1])/2, (latTicks[1] + max(X$latitude))/2, length = RESO))
lon <- seq((min(X$longitude) + lonTicks[1])/2, (lonTicks[RESO - 1] + lonTicks[RESO])/2, length = RESO)




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
weekDF <- melt(weeklyMeans, id.vars = 'Date')
g.week <- ggplot(data = weekDF, aes(x = Date, y = value, col = variable)) + geom_line()
g.week <- g.week + ggtitle('Mean Weekly Methane Mixing Ratio\nby Quadrant')
g.week <- g.week + xlab('Time') + ylab('Methane Mixing Ratio') + labs(col = 'Quadrant')

# Plotting daily means.
dayDF <- melt(dailyMeans, id.vars = 'Date')
g.day <- ggplot(data = dayDF, aes(x = Date, y = value, col = variable)) + geom_line()
g.day <- g.day + ggtitle('Mean Daily Methane Mixing Ratio\nby Quadrant')
g.day <- g.day + xlab('Time') + ylab('Methane Mixing Ratio') + labs(col = 'Quadrant')

rm(weekDF, dayDF)




## Some Summary Statistics.
if (STATS) {
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, length))
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, mean))
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, sd))
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, IQR))
  print(tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, mad))
  
  if (RESO == 2) {
    xbar <- tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, mean)
    s <- tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, sd)
    n <- tapply(X$methane_mixing_ratio_bias_corrected, X$quadrant, length)
    z <- 2.57583
    lower <- xbar - z*s/sqrt(n)
    upper <- xbar + z*s/sqrt(n)
    df <- as.data.frame(cbind(lower, upper), row.names = paste('Q', 1:4, sep=''))
    g <- ggplot(data = df, mapping = aes(x = row.names(df), ymin = lower, ymax = upper, color = row.names(df)))
    g <- g + geom_errorbar(width = 0.2) + ggtitle('99% Confidence Intervals for each Quadrant')
    g <- g + labs(col = 'Quadrant') + ylab('Methane Mixing Ratio') + xlab('Quadrant')
    g <- g + theme_bw()
    print(g)
    
    rm(xbar, s, n, z, lower, upper, df, g)
  }
}




## QUILT-PLOTS
xLim <- lonBounds + c(-0.1, 0.1)
yLim <- latBounds + c(-0.065, 0.065)
zLim <- c(1650, 1900)

if (MAKE_QUILT_PLOTS) {
  for (month in 1:length(data.month)) {
    quilt.plot.month(data.month[[month]], xLim, yLim, zLim)
  }
  
  ## BROKEN for now
  # for (month in 1:length(data.month)) {
  #   df <- data.month[[month]]
  #   df <- df[df$qa_value == 1.0, ]
  #   quilt.plot.month(df, xLim, yLim, zLim, outdir = '../Figures/3DayPlots/high-qa/')
  # }
  
  rm(month)
}

if (MAKE_GG_PLOTS & RESO == 64) {
  for (month in 1:length(data.month)) {
    ggmap.plot.month(data.month[[month]], basemap.hybrid, lat, lon, RESO, zLim)
  }
  
  ## BROKEN for now
  # for (month in 1:length(data.month)) {
  #   df <- data.month[[month]]
  #   df <- df[df$qa_value == 1.0, ]
  #   ggmap.plot.month(df, basemap.hybrid, lat, lon, RESO, zLim, outdir = '../Figures/gg3DayPlots/high-qa/')
  # }
  
  rm(month)
}

rm(xLim, yLim, zLim)




library(googleway)
test <- google_elevation(df_locations = X[1:10, 3:4], location_type = 'individual')





