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
  as_tibble %>%
  filter(qa_value == 1.0)



bounds <- c(1800, 1907.252)
## High QA.
f <- fourier.df(X, tsbounds = bounds, mbounds = c(0, 2830.564))

## Save figures.
pdf(file = '../figures/fourier/ts.pdf', height = 3.0, width = 10.0)
f$ts
dev.off()
pdf(file = '../figures/fourier/frequency.pdf', height = 3.0, width = 10.0)
f$g
dev.off()
pdf(file = '../figures/fourier/period.pdf', height = 3.0, width = 10.0)
f$p
dev.off()

pdf(file = '../figures/fourier/adjusted/ts-trend.pdf', height = 3.0, width = 10.0)
f$ts.trend
dev.off()
pdf(file = '../figures/fourier/adjusted/ts-adj.pdf', height = 3.0, width = 10.0)
f$ts.adj
dev.off()
pdf(file = '../figures/fourier/adjusted/frequency-adj.pdf', height = 3.0, width = 10.0)
f$g.adj
dev.off()
pdf(file = '../figures/fourier/adjusted/period-adj.pdf', height = 3.0, width = 10.0)
f$p.adj
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
mbounds <- c(0, 683.1120)
f.haynesville <- fourier.df(df.haynesville, tsbounds = bounds, type = 'weekly', mbounds = mbounds, k = 2)
f.wetlands <- fourier.df(df.wetlands, tsbounds = bounds, type = 'weekly', mbounds = mbounds, k = 2)
f.dfw <- fourier.df(df.dfw, tsbounds = bounds, type = 'weekly', mbounds = mbounds, k = 2)


## Save figures.
# Haynesville.
pdf(file = '../figures/fourier/haynesville/haynesville-ts.pdf', height = 3.0, width = 10.0)
f.haynesville$ts
dev.off()
pdf(file = '../figures/fourier/haynesville/haynesville-frequency.pdf', height = 3.0, width = 10.0)
f.haynesville$g
dev.off()
pdf(file = '../figures/fourier/haynesville/haynesville-period.pdf', height = 3.0, width = 10.0)
f.haynesville$p
dev.off()

pdf(file = '../figures/fourier/adjusted/haynesville/haynesville-ts-trend.pdf', height = 3.0, width = 10.0)
f.haynesville$ts.trend
dev.off()
pdf(file = '../figures/fourier/adjusted/haynesville/haynesville-ts-adj.pdf', height = 3.0, width = 10.0)
f.haynesville$ts.adj
dev.off()
pdf(file = '../figures/fourier/adjusted/haynesville/haynesville-frequency-adj.pdf', height = 3.0, width = 10.0)
f.haynesville$g.adj
dev.off()
pdf(file = '../figures/fourier/adjusted/haynesville/haynesville-period-adj.pdf', height = 3.0, width = 10.0)
f.haynesville$p.adj
dev.off()


# Wetlands.
pdf(file = '../figures/fourier/wetlands/wetlands-ts.pdf', height = 3.0, width = 10.0)
f.wetlands$ts
dev.off()
pdf(file = '../figures/fourier/wetlands/wetlands-frequency.pdf', height = 3.0, width = 10.0)
f.wetlands$g
dev.off()
pdf(file = '../figures/fourier/wetlands/wetlands-period.pdf', height = 3.0, width = 10.0)
f.wetlands$p
dev.off()

pdf(file = '../figures/fourier/adjusted/wetlands/wetlands-ts-trend.pdf', height = 3.0, width = 10.0)
f.wetlands$ts.trend
dev.off()
pdf(file = '../figures/fourier/adjusted/wetlands/wetlands-ts-adj.pdf', height = 3.0, width = 10.0)
f.wetlands$ts.adj
dev.off()
pdf(file = '../figures/fourier/adjusted/wetlands/wetlands-frequency-adj.pdf', height = 3.0, width = 10.0)
f.wetlands$g.adj
dev.off()
pdf(file = '../figures/fourier/adjusted/wetlands/wetlands-period-adj.pdf', height = 3.0, width = 10.0)
f.wetlands$p.adj
dev.off()


# DFW.
pdf(file = '../figures/fourier/dfw/dfw-ts.pdf', height = 3.0, width = 10.0)
f.dfw$ts
dev.off()
pdf(file = '../figures/fourier/dfw/dfw-frequency.pdf', height = 3.0, width = 10.0)
f.dfw$g
dev.off()
pdf(file = '../figures/fourier/dfw/dfw-period.pdf', height = 3.0, width = 10.0)
f.dfw$p
dev.off()

pdf(file = '../figures/fourier/adjusted/dfw/dfw-ts-trend.pdf', height = 3.0, width = 10.0)
f.dfw$ts.trend
dev.off()
pdf(file = '../figures/fourier/adjusted/dfw/dfw-ts-adj.pdf', height = 3.0, width = 10.0)
f.dfw$ts.adj
dev.off()
pdf(file = '../figures/fourier/adjusted/dfw/dfw-frequency-adj.pdf', height = 3.0, width = 10.0)
f.dfw$g.adj
dev.off()
pdf(file = '../figures/fourier/adjusted/dfw/dfw-period.pdf', height = 3.0, width = 10.0)
f.dfw$p
dev.off()







  
