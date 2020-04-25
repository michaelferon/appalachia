rm(list = ls())

# Make sure you're in the 'Code' directory.

load('../Data/data-full/dataFullMask.Rdata')
source('myFunctions.R')

library(lubridate)
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(fields)

rm(dateTimes)
X <- X %>%
  as_tibble



bounds <- c(1800, 1907.525)
## Marginal QA.
f.marginal <- fourier.df(X)

pdf(file = '../figures/fourier/ts.pdf', height = 3.0, width = 10.0)
f.marginal$ts
dev.off()

# pdf(file = '../figures/fourier/ts-trend.pdf', height = 3.0, width = 10.0)
# f.marginal$ts.trend
# dev.off()

pdf(file = '../figures/fourier/frequency.pdf', height = 3.0, width = 10.0)
f.marginal$g
dev.off()

pdf(file = '../figures/fourier/period.pdf', height = 3.0, width = 10.0)
f.marginal$p
dev.off()




## High QA.
Z <- X %>% filter(qa_value == 1.0)

f.high <- fourier.df(Z, tbounds = bounds, mbounds = c(0, 2830.564))

pdf(file = '../figures/fourier/ts-high-qa.pdf', height = 3.0, width = 10.0)
f.high$ts
dev.off()

# pdf(file = '../figures/fourier/ts-trend-high-qa.pdf', height = 3.0, width = 10.0)
# f.high$ts.trend
# dev.off()

pdf(file = '../figures/fourier/frequency-high-qa.pdf', height = 3.0, width = 10.0)
f.high$g
dev.off()

pdf(file = '../figures/fourier/period-high-qa.pdf', height = 3.0, width = 10.0)
f.high$p
dev.off()


### Region-by-region.
## Get shapefile data.
dfw = read.csv('../Data/ARIMA/DFW_AUS.csv')
names(dfw) = c("L", "B", "R", "T")

wetlands = read.csv('../Data/ARIMA/Wetlands.csv')
names(wetlands) = c("L", "B", "R", "T")

shp.data <- readOGR(dsn = '../Data/shapefile/shale-plays/ShalePlays_US_EIA_Sep2019.shp')
h <- which(shp.data$Shale_play == 'Haynesville-Bossier')
shp.data <- shp.data[h, ] %>% fortify
in.haynesville <- as.logical(point.in.polygon(X$longitude, X$latitude,
                                              shp.data$long, shp.data$lat))

## Build data-frames.
# Haynesville.
df.haynesville <- X[in.haynesville, ]

# Wetlands.
df.wetlands <- X[X$longitude >= wetlands$L[1] & X$longitude <= wetlands$R[1] &
                   X$latitude >= wetlands$B[1] & X$latitude <= wetlands$T[1], ]
for (i in 2:nrow(wetlands)) {
  temp <- X[X$longitude >= wetlands$L[i] & X$longitude <= wetlands$R[i] &
              X$latitude >= wetlands$B[i] & X$latitude <= wetlands$T[i], ]
  df.wetlands <- rbind(df.wetlands, temp)
}

# DFW.
df.dfw <- X[X$longitude >= dfw$L[1] & X$longitude <= dfw$R[1] &
                  X$latitude >= dfw$B[1] & X$latitude <= dfw$T[1], ]
for (i in 2:nrow(dfw)) {
  temp <- X[X$longitude >= dfw$L[i] & X$longitude <= dfw$R[i] &
              X$latitude >= dfw$B[i] & X$latitude <= dfw$T[i], ]
  df.dfw <- rbind(df.dfw, temp)
}

rm(dfw, wetlands, shp.data, in.haynesville, temp, i, h)

## Fourier transform.
f.haynesville <- fourier.df(df.haynesville, type = 'weekly', mbounds = c(0, 797.9622), k = 2)
f.wetlands <- fourier.df(df.wetlands, type = 'weekly', mbounds = c(0, 797.9622), k = 2)
f.dfw <- fourier.df(df.dfw, type = 'weekly', mbounds = c(0, 797.9622), k = 2)

## Save figures.
# Haynesville.
pdf(file = '../figures/fourier/haynesville/haynesville-ts.pdf', height = 3.0, width = 10.0)
f.haynesville$ts
dev.off()

# pdf(file = '../figures/fourier/haynesville/haynesville-ts-trend.pdf', height = 3.0, width = 10.0)
# f.haynesville$ts.trend
# dev.off()

pdf(file = '../figures/fourier/haynesville/haynesville-frequency.pdf', height = 3.0, width = 10.0)
f.haynesville$g
dev.off()

pdf(file = '../figures/fourier/haynesville/haynesville-period.pdf', height = 3.0, width = 10.0)
f.haynesville$p
dev.off()

# Wetlands.
pdf(file = '../figures/fourier/wetlands/wetlands-ts.pdf', height = 3.0, width = 10.0)
f.wetlands$ts
dev.off()

# pdf(file = '../figures/fourier/wetlands/wetlands-ts-trend.pdf', height = 3.0, width = 10.0)
# f.wetlands$ts.trend
# dev.off()

pdf(file = '../figures/fourier/wetlands/wetlands-frequency.pdf', height = 3.0, width = 10.0)
f.wetlands$g
dev.off()

pdf(file = '../figures/fourier/wetlands/wetlands-period.pdf', height = 3.0, width = 10.0)
f.wetlands$p
dev.off()

# DFW.
pdf(file = '../figures/fourier/dfw/dfw-ts.pdf', height = 3.0, width = 10.0)
f.dfw$ts
dev.off()

# pdf(file = '../figures/fourier/dfw/dfw-ts-trend.pdf', height = 3.0, width = 10.0)
# f.dfw$ts.trend
# dev.off()

pdf(file = '../figures/fourier/dfw/dfw-frequency.pdf', height = 3.0, width = 10.0)
f.dfw$g
dev.off()

pdf(file = '../figures/fourier/dfw/dfw-period.pdf', height = 3.0, width = 10.0)
f.dfw$p
dev.off()





### High QA.
df.haynesville <- df.haynesville %>% filter(qa_value == 1.0)
df.wetlands <- df.wetlands %>% filter(qa_value == 1.0)
df.dfw <- df.dfw %>% filter(qa_value == 1.0)

## Fourier transform.
f.haynesville <- fourier.df(df.haynesville, tbounds = bounds, type = 'weekly', mbounds = c(0, 676.6103), k = 2)
f.wetlands <- fourier.df(df.wetlands, tbounds = bounds, type = 'weekly', mbounds = c(0, 676.6103), k = 2)
f.dfw <- fourier.df(df.dfw, tbounds = bounds, type = 'weekly', mbounds = c(0, 676.6103), k = 2)

## Save figures.
# Haynesville.
pdf(file = '../figures/fourier/haynesville/haynesville-ts-high-qa.pdf', height = 3.0, width = 10.0)
f.haynesville$ts
dev.off()

# pdf(file = '../figures/fourier/haynesville/haynesville-ts-trend-high-qa.pdf', height = 3.0, width = 10.0)
# f.haynesville$ts.trend
# dev.off()

pdf(file = '../figures/fourier/haynesville/haynesville-frequency-high-qa.pdf', height = 3.0, width = 10.0)
f.haynesville$g
dev.off()

pdf(file = '../figures/fourier/haynesville/haynesville-period-high-qa.pdf', height = 3.0, width = 10.0)
f.haynesville$p
dev.off()

# Wetlands.
pdf(file = '../figures/fourier/wetlands/wetlands-ts-high-qa.pdf', height = 3.0, width = 10.0)
f.wetlands$ts
dev.off()

# pdf(file = '../figures/fourier/wetlands/wetlands-ts-trend-high-qa.pdf', height = 3.0, width = 10.0)
# f.wetlands$ts.trend
# dev.off()

pdf(file = '../figures/fourier/wetlands/wetlands-frequency-high-qa.pdf', height = 3.0, width = 10.0)
f.wetlands$g
dev.off()

pdf(file = '../figures/fourier/wetlands/wetlands-period-high-qa.pdf', height = 3.0, width = 10.0)
f.wetlands$p
dev.off()

# DFW.
pdf(file = '../figures/fourier/dfw/dfw-ts-high-qa.pdf', height = 3.0, width = 10.0)
f.dfw$ts
dev.off()

# pdf(file = '../figures/fourier/dfw/dfw-ts-trend-high-qa.pdf', height = 3.0, width = 10.0)
# f.dfw$ts.trend
# dev.off()

pdf(file = '../figures/fourier/dfw/dfw-frequency-high-qa.pdf', height = 3.0, width = 10.0)
f.dfw$g
dev.off()

pdf(file = '../figures/fourier/dfw/dfw-period-high-qa.pdf', height = 3.0, width = 10.0)
f.dfw$p
dev.off()







  
