rm( list = ls() )

# Make sure you're in the 'Code' directory.

load('../Data/data-full/X100.Rdata')
source('myFunctions.R')

library(config)    # Configuration details.
library(lubridate) # Handling date-times.
library(ggplot2)   # Plotting.
library(ggmap)     # Spatial plotting using maps.
library(fields)

# Need a Google API key for some of the stuff below.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

# ggmap stuff.
latBounds <- c(min(X$latitude), max(X$latitude))
lonBounds <- c(min(X$longitude), max(X$longitude))
basemap.hybrid <- get.basemap('google', 'hybrid')




## DATA AGGREGATION
dateTimes.days <- my.yday(X$time_utc)
dateTimes.weeks <- my.week(X$time_utc)
dateTimes.months <- my.month(X$time_utc)

dateTimes.weeks[dateTimes.weeks == '2018-53'] <- '2018-52'
dateTimes.weeks[dateTimes.weeks == '2019-53'] <- '2019-52'

uniqueDays <- unique(dateTimes.days)
uniqueWeeks <- unique(dateTimes.weeks)
uniqueMonths <- unique(dateTimes.months)

data.day <- data.aggregate(X, dateTimes.days, uniqueDays, 'Day')
data.week <- data.aggregate(X, dateTimes.weeks, uniqueWeeks, 'Week')
data.month <- data.aggregate(X, dateTimes.months, uniqueMonths, 'Month')




## HEATMAPS
# Daily proportion matrix.
full <- prop.matrix(uniqueDays, data.day, RESO)
image(x = lon, y = rev(lat), z = t(apply(full$mat, 2, rev)),
      main = 'Daily Elevated Methane Levels', xlab = 'Longitude', ylab = 'Latitude')
US(add = TRUE, lwd = 2)

# Heatmap with geo-map underlaid.
g <- ggmap.prop.matrix(full$mat, lat, lon, basemap.hybrid, 'Daily')
pdf(file = '../Figures/proportions/propmat.pdf', height = 5.25, width = 9)
print(g)
dev.off()

rm(g)




## HEATMAPS BY MONTH
prop.list <- vector(mode = 'list', length = length(data.month) + 1)
prop.list[[1]] <- full
names(prop.list) <- c('full', names(data.month))
rm(full)

outdir <- '../Figures/proportions/monthly-pdf/'
for (i in 1:length(data.month)) {
  month <- data.month[[i]]
  monthNum <- month(month$time_utc[1])
  monthName <- months(month$time_utc[1])
  yearName <- year(month$time_utc[1])
  pTitle <- paste(monthName, yearName)
  
  month.days <- day(month$time_utc)
  month.uniqueDays <- unique(month.days)
  month.data.day <- data.aggregate(month, month.days, month.uniqueDays, 'Day')
  
  struct <- prop.matrix(month.uniqueDays, month.data.day, RESO)
  prop.list[[i + 1]] <- struct
  
  if (monthNum < 10) { monthNum <- paste('0', monthNum, sep='') }
  title <- paste(yearName, monthNum, sep='-')

  # png(filename = paste(outdir, title, '.png', sep=''), height = 480, width = 825)
  pdf(file = paste(outdir, title, '.pdf', sep=''), height = 5.25, width = 9)
  g <- ggmap.prop.matrix(struct$mat, lat, lon, basemap.hybrid, 'Daily', pTitle = pTitle)
  print(g)
  dev.off()
}

rm(i, month, monthNum, monthName, yearName, pTitle, month.days, month.uniqueDays,
   month.data.day, struct, title, g)

save(prop.list, file = '../Data/data-full/propList.Rdata')

