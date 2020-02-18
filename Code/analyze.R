rm( list = ls() )

# Make sure you're in the "Code" directory.

load('../Data/data-full/dataFull.Rdata') # Use this now.
source('functions.R')


library(lubridate)
library(ggplot2)
library(reshape2)
library(fields)

# Do you want to see the time-series plots?
# WARNING: If RESO is set too high, these plots will be chaotic.
PLOTS <- TRUE
STATS <- FALSE # Print summary statistics?


## Dividing data spatially into RESO^2 quadrants.
RESO <- 2
latTicks <- rev(seq(min(X$latitude), max(X$latitude), length = RESO + 1))[-1]
lonTicks <- seq(min(X$longitude), max(X$longitude), length = RESO + 1)[-1]
for (i in 1:RESO) {
  for (j in 1:RESO) {
    X$quadrant[ X$latitude >= latTicks[i] & X$longitude <= lonTicks[j] & X$quadrant == 0 ] <- RESO*(i - 1) + j
  }
}

# WARNING: THIS PLOT IS TIME-INTENSIVE. It just verifies that quadrants were divided correctly.
# ggplot(data = X) + geom_point(mapping = aes(x = longitude, y = latitude, col = quadrant))

dateTimes.days <- my.yday(dateTimes)
dateTimes.weeks <- my.week(dateTimes)
dateTimes.months <- my.month(dateTimes)


# Aggregate by day into list: data.day
uniqueDays <- unique(dateTimes.days)
data.day <- vector(mode = 'list', length = length(uniqueDays))
names(data.day) <- paste('Day', uniqueDays, sep = '')
for (i in 1:length(data.day)) {
  data.day[[i]] <- X[dateTimes.days == uniqueDays[i], ]
}

# Aggregate by week into list: data.week
uniqueWeeks <- unique(dateTimes.weeks)
data.week <- vector(mode = 'list', length = length(uniqueWeeks))
names(data.week) <- paste('Week', uniqueWeeks, sep = '')
for (i in 1:length(data.week)) {
  data.week[[i]] <- X[dateTimes.weeks == uniqueWeeks[i], ]
}

# Aggregate by month into list: data.month
uniqueMonths <- unique(dateTimes.months)
data.month <- vector(mode = 'list', length = length(uniqueMonths))
names(data.month) <- paste('Month', uniqueMonths, sep = '')
for (i in 1:length(data.month)) {
  data.month[[i]] <- X[dateTimes.months == uniqueMonths[i], ]
}


# Compute DAILY methane averages, aggregated per quadrant.
dailyMeans <- matrix(data = NA, nrow = length(uniqueDays), ncol = RESO^2)
for (i in 1:length(uniqueDays)) {
  x <- tapply(data.day[[i]]$methane_mixing_ratio_bias_corrected, data.day[[i]]$quadrant, mean)
  temp <- as.numeric(names(x))
  dailyMeans[i, temp] <- x
}
dailyMeans <- as.data.frame(dailyMeans, row.names = names(data.day))
dailyMeans <- cbind(as.POSIXlt(sort(tapply(X$time, dateTimes.days, mean)), origin = '2010-01-01 00:00:00', tz = 'UTC'), dailyMeans)
colnames(dailyMeans) <- c('Date', paste('Q', 1:(RESO^2), sep=''))

# Compute WEEKLY methane averages, aggregated per quadrant.
weeklyMeans <- matrix(data = NA, nrow = length(uniqueWeeks), ncol = RESO^2)
for (i in 1:length(uniqueWeeks)) {
  x <- tapply(data.week[[i]]$methane_mixing_ratio_bias_corrected, data.week[[i]]$quadrant, mean)
  temp <- as.numeric(names(x))
  weeklyMeans[i, temp] <- x
}
weeklyMeans <- as.data.frame(weeklyMeans, row.names = names(data.week))
weeklyMeans <- cbind(as.POSIXlt(sort(tapply(X$time, dateTimes.weeks, mean)), origin = '2010-01-01 00:00:00', tz = 'UTC'), weeklyMeans)
colnames(weeklyMeans) <- c('Date', paste('Q', 1:(RESO^2), sep=''))

# Compute MONTHLY methane averages, aggregated per quadrant.
monthlyMeans <- matrix(data = NA, nrow = length(uniqueMonths), ncol = RESO^2)
for (i in 1:length(uniqueMonths)) {
  x <- tapply(data.month[[i]]$methane_mixing_ratio_bias_corrected, data.month[[i]]$quadrant, mean)
  temp <- as.numeric(names(x))
  monthlyMeans[i, temp] <- x
}
monthlyMeans <- as.data.frame(monthlyMeans, row.names = names(data.month))
monthlyMeans <- cbind(as.POSIXlt(sort(tapply(X$time, dateTimes.months, mean)), origin = '2010-01-01 00:00:00', tz = 'UTC'), monthlyMeans)
colnames(monthlyMeans) <- c('Date', paste('Q', 1:(RESO^2), sep=''))


# Here, lat and lon are the midpoints of each quadrant.
lat <- rev(seq((latTicks[RESO] + latTicks[RESO - 1])/2, (latTicks[1] + max(X$latitude))/2, length = RESO))
# lat <- rep(lat, each = RESO)
lon <- seq((min(X$longitude) + lonTicks[1])/2, (lonTicks[RESO - 1] + lonTicks[RESO])/2, length = RESO)
# lon <- rep(lon, times = RESO)


# A bit of cleanup.
rm(i, j, temp, x)


# Time-series Plots.
if (PLOTS) {
  # Plotting weekly means.
  weekDF <- melt(weeklyMeans, id.vars = 'Date')
  g.week <- ggplot(data = weekDF, aes(x = Date, y = value, col = variable)) + geom_line()
  g.week <- g.week + ggtitle('Mean Weekly Methane Mixing Ratios\nby Quadrant')
  g.week <- g.week + xlab('Time') + ylab('Methane Mixing Ratio, Bias Corrected') #+ labs(col = 'Quadrant')
  
  # Plotting daily means.
  dayDF <- melt(dailyMeans, id.vars = 'Date')
  g.day <- ggplot(data = dayDF, aes(x = Date, y = value, col = variable)) + geom_line()
  g.day <- g.day + ggtitle('Mean Daily Methane Mixing Ratios\nby Quadrant')
  g.day <- g.day + xlab('Time') + ylab('Methane Mixing Ratio, Bias Corrected') #+ labs(col = 'Quadrant')
  
  rm(weekDF, dayDF)
}


# Some Summary Statistics.
if (STATS) {
  tapply(X$methane_mixing_ratio, X$quadrant, mean)
  tapply(X$methane_mixing_ratio, X$quadrant, sd)
  tapply(X$methane_mixing_ratio, X$quadrant, length)
  tapply(X$methane_mixing_ratio, X$quadrant, IQR)
  tapply(X$methane_mixing_ratio, X$quadrant, mad)
}



## ANOMALY DETECTION
# Monthly Anomalies
anomMonth <- anomaly.matrix(uniqueMonths, data.month, RESO)

# Weekly Anomalies
anomWeek <- anomaly.matrix(uniqueWeeks, data.week, RESO)

# Daily Anomalies
anomDay <- anomaly.matrix(uniqueDays, data.day, RESO)


# Heatmaps!
image(x = lon, y = rev(lat), z = t(apply(anomMonth, 2, rev)),
      main = 'Monthly Global Anomaly Heatmap', xlab = 'Longitude', ylab = 'Latitude')
image(x = lon, y = rev(lat), z = t(apply(anomWeek, 2, rev)),
      main = 'Weekly Global Anomaly Heatmap', xlab = 'Longitude', ylab = 'Latitude')
image(x = lon, y = rev(lat), z = t(apply(anomDay, 2, rev)),
      main = 'Daily Global Anomaly Heatmap', xlab = 'Longitude', ylab = 'Latitude')



## PLOTS
outdir <- '../Figures/3DayPlots/'
xMin <- min(X$longitude) - 0.1
xMax <- max(X$longitude) + 0.1
yMin <- min(X$latitude) - 0.1
yMax <- max(X$latitude) + 0.1

xLim <- c(xMin, xMax)
yLim <- c(yMin, yMax)

for (month in 1:length(data.month)) {
  plot.month(data.month[[month]], xLim, yLim, outdir = outdir)
}


rm(outdir, xMin, xMax, yMin, yMax, xLim, yLim)












