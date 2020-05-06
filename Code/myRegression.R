rm(list = ls())

# Make sure you're in the 'Code' directory.

load('../Data/data-full/dataFullMask.Rdata')
rm(dateTimes)
source('myFunctions.R')

library(config)
library(lubridate)
library(ggplot2)
library(ggmap)
library(zoo)
library(dplyr)
library(viridis)
library(sp)
library(rgdal)
library(fields)
library(readxl)

# Need a Google API key for some of the stuff below.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)
basemap.hybrid <- get.basemap('google', 'hybrid', lonBounds, latBounds)

## High-qa only.
X <- X %>%
  as_tibble %>%
  filter(qa_value == 1.0) %>%
  rename(methane = methane_mixing_ratio_bias_corrected)




## Get Haynesville shapefile data.
shp.data <- readOGR(dsn = '../Data/shapefile/shale-plays/ShalePlays_US_EIA_Sep2019.shp')
h <- which(shp.data$Shale_play == 'Haynesville-Bossier')
shp.data <- shp.data[h, ]
haynesville <- fortify(shp.data)
in.haynesville <- as.logical(
  point.in.polygon(X$longitude, X$latitude, haynesville$long, haynesville$lat)
)

# Construct dataframes.
df.haynesville <- X[in.haynesville, ]
df.haynesville$methane_adj <- as.numeric(NA)
df.haynesville <- df.haynesville %>% select(1:5, 11, 6:10)
df.out <- X[!in.haynesville, ]
# Sanity check
quilt.plot(df.haynesville$longitude, df.haynesville$latitude,
           df.haynesville$methane, xlim = lonBounds, ylim = latBounds)
quilt.plot(df.out$longitude, df.out$latitude,
           df.out$methane, xlim = lonBounds, ylim = latBounds)

# make the background correction
days <- my.yday(df.haynesville$time_utc)
outDays <- my.yday(df.out$time_utc)
uniqueDays <- unique(days)

for (day in uniqueDays) {
  outMean <- mean(df.out$methane[outDays == day])
  df.haynesville$methane_adj[days == day] <- df.haynesville$methane[days == day] - outMean
}

# plot data
df.haynesville <- df.haynesville %>%
  mutate(methane = methane - mean(methane) + mean(methane_adj))

pdf(file = '../figures/regression/bg-combo.pdf', width = 9, height = 4)
par(mfrow=c(1,2),oma=c(1.2,1.2,0,1),mar=c(3, 4, 4, 2) + 0.1)
quilt.plot(df.haynesville$longitude, df.haynesville$latitude,
           df.haynesville$methane, xlim = c(-96, -92), ylim = c(30.51, 33.49),
           zlim = c(-40, 40), xlab = '', ylab = '', add.legend = TRUE,
           main = 'Original data (centered)')
US(add = TRUE)
par(mar = c(3, 4, 4, 2) + 0.1)
quilt.plot(df.haynesville$longitude, df.haynesville$latitude,
           df.haynesville$methane_adj, xlim = c(-96, -92), ylim = c(30.51, 33.49),
           zlim = c(-40, 40), xlab = '', ylab = '', main = 'Background-corrected data')
US(add = TRUE)
mtext("Longitude",side=1,line=0,outer=TRUE,cex=1.3)
mtext("Latitude",side=2,line=0,outer=TRUE,cex=1.3,las=0)
dev.off()

months <- seq(as.Date('2018-05-15'), as.Date('2020-02-15'), length = 22)
tempdf <- tibble(
  time = months,
  methane = as.numeric(NA)
)
for (i in 1:nrow(tempdf)) {
  tempdf$methane[i] <- mean(df.haynesville$methane_adj[my.month(df.haynesville$time_utc) == my.month(tempdf$time[i])])
}
tempdf <- tempdf %>% na.omit

methaneMean <- mean(tempdf$methane)
x <- seq(1, nrow(tempdf), by = 1)
model <- tibble(
  time = tempdf$time,
  methane = lm(tempdf$methane ~ x)$fitted.values
)

pdf(file = '../figures/regression/haynesville-methane.pdf', height = 4.0, width = 10)
ggplot(tempdf, aes(time, methane)) +
  geom_line(data = model, color = 'OrangeRed3',
            alpha = 0.75, linetype = 'dashed') +
  geom_line(color = 'SteelBlue') +
  theme_minimal() +
  xlab('Time') + ylab('Methane Mixing Ratio') +
  ylim(-8, 13)
dev.off()





## Shale gas data
shale.data <- read_excel('../data/shale/shale_gas_202002.xlsx') %>%
  select('Date', 'Haynesville (LA & TX)') %>%
  filter(Date >= as.Date('2018-05-01'), Date < as.Date('2020-03-01')) %>%
  rename(shale = `Haynesville (LA & TX)`, date = Date) %>%
  mutate(date = as.Date(date)) %>%
  filter(date != as.Date('2018-09-01'))


vec <- tapply(df.haynesville$methane_adj, my.month(df.haynesville$time_utc), mean)
vec <- vec[-length(vec)]
shale.data$methane <- vec

pdf(file = "../Figures/Regression/Shale_Gas_Prod_Line.pdf", height = 5, width = 7.0)
plot(shale.data$date, shale.data$shale, type = 'l', xlab = 'Time', ylab = 'Shale Gas Production')
dev.off()

# create the model
model <- lm(methane ~ shale, data = shale.data)
model.sum <- summary(model)
r.squared <- model.sum$r.squared

pdf(file = "../Figures/Regression/lm.pdf", height = 5, width = 7)
plot(shale.data$shale, shale.data$methane, xlab = 'Shale Gas Production (billions of cubic feet)', ylab = 'Methane Mixing Ratio')
abline(model)
# legend('topleft', bty = 'n', legend = paste('R2 = ', format(r.squared, digits = 2)))
dev.off()
