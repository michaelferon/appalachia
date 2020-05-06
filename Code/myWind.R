#Code to perform a basic shifting of data points based on their wind vector at the time of measurement
#Pseudo-equation : X_wind = lat/long point + (wind vector)(amount of time in hours)
rm(list = ls())
library(ggplot2)
library(ggmap)
library(dplyr)
library(lubridate)
library(zoo)
library(viridis)
library(readr)
library(readxl)

source('myFunctions.R')

info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

load('../data/data-full/dataFullMask.Rdata')
X <- X %>% filter(time_utc >= as.POSIXct('2019-03-20', tz = 'UTC'))
rm(dateTimes)

#Number of hours forward to shift data
time_shift = 24



#Initialize X_wind
X_wind = X

#69 (roughly) represents number of miles in 1 degree latitude.
#Difficult to calculate exactly like for longitude, but 69 should be an accurate enough measurement for our purposes
X_wind[,'latitude'] = X[,'latitude'] + (1/69)*(2.237) *time_shift * X[,'eastward_wind']

#Length of 1 degree of Longitude in miles = cosine (latitude in decimal degrees) * length of degree (miles) at equator.
#Could change this to use an integral to exactly calculate time shift and remove some bias
X_wind[,'longitude'] = X[,'longitude'] + (1/69.172)*(2.237) * cos(X[,'latitude']) * time_shift *X[,'northward_wind']

#Times must be shifted forward by time_shift hours
#Date data type allows for addition of seconds
X_wind[,'time'] = X[,'time'] + 60*60*time_shift
X_wind[,'time_utc'] = X[,'time_utc'] + 60*60*time_shift

#Remove any data that occured before april 1
X_wind <- X_wind %>% filter(time_utc < as.POSIXct('2020-04-01', tz = 'UTC'))


## Setup the boundries for the quilt plot
latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)
basemap.hybrid <- get.basemap('google', 'hybrid', lonBounds, latBounds)

#Set the resolution of the quilt plot, create the subregions
RESO <- 50
latTicks <- seq(latBounds[1], latBounds[2], length = RESO + 1)
lonTicks <- seq(lonBounds[1], lonBounds[2], length = RESO + 1)
X <- assign.quadrants(X, latTicks, lonTicks, RESO)
X_wind <- assign.quadrants(X_wind, latTicks, lonTicks, RESO)

# Here, lat and lon are the midpoints of each quadrant.
lat <- rev(rollmean(latTicks, 2))
lon <- rollmean(lonTicks, 2)

#Code to figure out the days, weeks, and months we have data for, then split the data into
#lists based on the time periods
days <- my.yday(X$time_utc)
windDays <- my.yday(X_wind$time_utc)
months <- my.month(X$time_utc)
windMonths <- my.month(X_wind$time_utc)

uniqueDays <- unique(days)
uniqueWindDays <- unique(windDays)
uniqueMonths <- unique(months)
uniqueWindMonths <- unique(windMonths)

data.day <- data.aggregate(X, days, uniqueDays, 'Day')
wind.day <- data.aggregate(X_wind, windDays, uniqueWindDays, 'Day')
data.month <- data.aggregate(X, months, uniqueMonths, 'Month')
wind.month <- data.aggregate(X_wind, windMonths, uniqueWindMonths, 'Month')

#calculate the means of each subregion/time period combination
dailyMeans <- means.matrix(X, data.day, days, uniqueDays, RESO)
dailyWindMeans <- means.matrix(X_wind, wind.day, windDays, uniqueWindDays, RESO)
dailyMeans$Date <- as.Date(dailyMeans$Date)
dailyWindMeans$Date <- as.Date(dailyWindMeans$Date)

monthlyMeans <- means.matrix(X, data.month, months, uniqueMonths, RESO)
monthlyWindMeans <- means.matrix(X_wind, wind.month, windMonths, uniqueWindMonths, RESO)
monthlyMeans$Date <- as.Date(monthlyMeans$Date)
monthlyWindMeans$Date <- as.Date(monthlyWindMeans$Date)

dailyMeans <- dailyMeans %>% filter(Date %in% dailyWindMeans$Date)
dailyWindMeans <- dailyWindMeans %>% filter(Date %in% dailyMeans$Date)

#Perform the wind adjustment
windAdjustedMeans <- dailyMeans
windAdjustedMeans[, -1] <- dailyMeans[, -1] - dailyWindMeans[, -1]


test <- windAdjustedMeans[, -1]
test2 <- dailyMeans[, -1]
test <- apply(test, 2, mean, na.rm = TRUE)
test2 <- apply(test2, 2, mean, na.rm = TRUE)

#Create plots
df <- tibble(
  methane = test,
  longitude = rep(lon, times = RESO),
  latitude = rep(lat, each = RESO)
) %>% na.omit
height = abs(mean(diff(lat)))
width = abs(mean(diff(lon)))

df2 <- tibble(
  methane2 = test2 - mean(test2, na.rm = TRUE),
  longitude = rep(lon, times = RESO),
  latitude = rep(lat, each = RESO)
) %>% na.omit
height = abs(mean(diff(lat)))
width = abs(mean(diff(lon)))

pdf(file = '../figures/wind/wind.pdf', height = 3.0, width = 4.5)
basemap.hybrid +
  geom_tile(
    data = df,
    mapping = aes(x = longitude, y = latitude, fill = methane),
    height = height, width = width, alpha = 0.65
  ) +
  scale_fill_gradientn(colors = magma(100), limits = c(-50, 50)) +
  xlab('Longitude') + ylab('Latitude') +
  theme(legend.key.height = unit(1.0, 'cm')) +
  labs(fill = '')
dev.off()

pdf(file = '../figures/wind/Methane.pdf', height = 3.0, width = 4.5)
basemap.hybrid +
  geom_tile(
    data = df2,
    mapping = aes(x = longitude, y = latitude, fill = methane2),
    height = height, width = width, alpha = 0.65
  ) +
  scale_fill_gradientn(colors = magma(100), limits = c(-50, 50)) +
  xlab('Longitude') + ylab('Latitude') +
  theme(legend.key.height = unit(1.0, 'cm')) +
  labs(fill = '')
dev.off()


## NEW STUFF.
windAdjustedMeans.month <- monthlyMeans
windAdjustedMeans.month[, -1] <- monthlyMeans[, -1] - monthlyWindMeans[, -1]

shale.data <- read_excel('../data/shale/shale_gas_202002.xlsx') %>%
  select('Date', 'Haynesville (LA & TX)') %>%
  filter(Date >= as.Date('2019-03-01'), Date < as.Date('2020-02-01')) %>%
  rename(shale = `Haynesville (LA & TX)`, date = Date)
shale.data$methane <- apply(windAdjustedMeans.month[, -1], 1, mean, na.rm = TRUE)

shale.months <- c('March', 'April', 'May', 'June', 'July', 'August', 'September',
                  'October', 'November', 'December', 'January')
shale.data$month <- shale.months
shale.data$month <- as.factor(shale.data$month)
shale.data <- shale.data %>% slice(-11)


model <- lm(methane ~ shale, data = shale.data)
summary(model)
r.squared <- summary(model)$adj.r.squared

pdf(file = '../figures/wind/methane-shale2.pdf', height = 4.68, width = 6.32)
plot(shale.data$shale,
     shale.data$methane,
     xlab = 'Shale Gas',
     ylab = 'Methane Mixing Ratio'
)
abline(model)
legend('topright', bty = 'n',
       legend = paste('Adj. R2 = ', format(r.squared, digits = 4)))
dev.off()

