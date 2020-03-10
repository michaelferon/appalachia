rm( list = ls() )

# Make sure you're in the 'Code' directory.

load('../Data/mask/mask.Rdata')
load('../Data/data-full/dataFull.Rdata')
source('myFunctions.R')
library(config)    # Configuration details.
library(lubridate) # Handling date-times.
library(ggplot2)   # Plotting.
library(ggmap)     # Spatial plotting using maps.
library(zoo)       # For rollmean().
library(fields)
library(ncdf4)


info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)
basemap.hybrid <- get.basemap('google', 'hybrid', lonBounds, latBounds)



## Map with water overlaid.
mask.water <- subset(mask, water == 1)
water <- get_map(location = c(mean(lonBounds), mean(latBounds)), color = 'bw',
               source = 'stamen', maptype = 'terrain-background', zoom = 6)
water <- ggmap(water)
water <- water + scale_x_continuous(limits = lonBounds, expand = c(0, 0))
water <- water + scale_y_continuous(limits = latBounds, expand = c(0, 0))
water <- water +
  geom_point(data = mask.water, mapping = aes(x = longitude, y = latitude),
             color = 'blue', size = 0.1, stroke = 0, shape = 16) +
  ggtitle('Land Water Mask') + xlab('Longitude') + ylab('Latitude')
pdf(file = '../Figures/mask.pdf', height = 6.5, width = 8.0)
print(water)
dev.off()




RESO <- 2875
latTicks <- seq(latBounds[1], latBounds[2], length = RESO + 1)
lonTicks <- seq(lonBounds[1], lonBounds[2], length = RESO + 1)
lat <- rev(rollmean(latTicks, 2))
lon <- rollmean(lonTicks, 2)

X <- assign.quadrants(X, latTicks, lonTicks, RESO)




mask <- assign.quadrants(mask, latTicks, lonTicks, RESO)
length(unique(mask$quadrant)) / RESO^2
noData <- (1:(RESO^2))[!(1:(RESO^2) %in% unique(mask$quadrant))]

# TEST
test <- tapply(mask$quadrant, mask$quadrant, length)
tapply(test, test, function(x) length(x) / RESO^2)
rm(test)
# END TEST

prop.water <- tapply(mask$water, mask$quadrant, mean)
prop.water <- prop.water[!(prop.water > 0)]
goodQuadrants <- as.numeric(names(prop.water))

X <- subset(X, quadrant %in% goodQuadrants | quadrant %in% noData)
rm(mask, noData, prop.water, goodQuadrants, dateTimes)

X$quadrant <- 0
dateTimes <- X$time_utc
save(X, dateTimes, file = '../Data/data-full/dataFullMask.Rdata')




## 
RESO <- 100
latTicks <- seq(latBounds[1], latBounds[2], length = RESO + 1)
lonTicks <- seq(lonBounds[1], lonBounds[2], length = RESO + 1)
lat <- rev(rollmean(latTicks, 2))
lon <- rollmean(lonTicks, 2)

X <- assign.quadrants(X, latTicks, lonTicks, RESO)


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
g <- ggmap.prop.matrix(full$mat, lat, lon, basemap.hybrid, 'Daily',
                       pTitle = 'Mask Adjusted Daily Elevated Methane Levels')
pdf(file = '../Figures/proportions/mask-adjusted/propmat-mask.pdf', height = 6.5, width = 7.5)
print(g)
dev.off()

rm(g)




## HEATMAPS BY MONTH
prop.list.adj <- vector(mode = 'list', length = length(data.month) + 1)
prop.list.adj[[1]] <- full
names(prop.list.adj) <- c('full', names(data.month))
rm(full)

outdir <- '../Figures/proportions/mask-adjusted/monthly-pdf/'
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
  prop.list.adj[[i + 1]] <- struct
  
  if (monthNum < 10) { monthNum <- paste('0', monthNum, sep='') }
  title <- paste(yearName, monthNum, sep='-')
  
  pdf(file = paste(outdir, title, '.pdf', sep=''), height = 6.5, width = 7.5)
  g <- ggmap.prop.matrix(struct$mat, lat, lon, basemap.hybrid, 'Daily', pTitle = pTitle)
  print(g)
  dev.off()
}

rm(i, month, monthNum, monthName, yearName, pTitle, month.days, month.uniqueDays,
   month.data.day, struct, title, g)

save(prop.list.adj, file = '../Data/data-full/propListAdj.Rdata')




