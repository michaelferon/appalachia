# # ARIMA DF Init # #
# This file initializes the 3 dataframes that will be used in building the ARIMA Time Series Models
rm(list = ls())

library(fields)
library(dplyr)
library(lubridate)
library(forecast)
library(stats)
library(sp) ## Michael's shapefile stuff.
library(rgdal) ## This too.
library(ggplot2)

source('myFunctions.R')

# load the adhoc region masks
DFW.AUS = read.csv('../Data/ARIMA/DFW_AUS.csv')
names(DFW.AUS) = c("L", "B", "R", "T")

Haynesville = read.csv('../Data/ARIMA/Haynesville.csv')
names(Haynesville) = c("L", "B", "R", "T")

Wetlands = read.csv('../Data/ARIMA/Wetlands.csv')
names(Wetlands) = c("L", "B", "R", "T")

load('../Data/data-full/dataFullMask.Rdata')
X$time_utc <- as.POSIXct(X$time_utc)


## build the dataframes - THIS IS WHERE MICHAEL MAKES THE SHAPE FILE ADJUSTMENTS
# Haynesville
shp.data <- readOGR(dsn = '../Data/shapefile/shale-plays/ShalePlays_US_EIA_Sep2019.shp')
h <- which(shp.data$Shale_play == 'Haynesville-Bossier')
shp.data <- shp.data[h, ] %>% fortify
in.haynesville <- as.logical(point.in.polygon(X$longitude, X$latitude,
                                              shp.data$long, shp.data$lat))
df.Haynesville <- X[in.haynesville, ]

# Wetlands
df.Wetlands <- X[X$longitude >= Wetlands$L[1] & X$longitude <= Wetlands$R[1] &
                   X$latitude >= Wetlands$B[1] & X$latitude <= Wetlands$T[1], ]
for (i in 2:nrow(Wetlands)) {
  temp <- X[X$longitude >= Wetlands$L[i] & X$longitude <= Wetlands$R[i] &
              X$latitude >= Wetlands$B[i] & X$latitude <= Wetlands$T[i], ]
  df.Wetlands <- rbind(df.Wetlands, temp)
}

# DFW-AUS
df.DFW.AUS <- X[X$longitude >= DFW.AUS$L[1] & X$longitude <= DFW.AUS$R[1] &
                  X$latitude >= DFW.AUS$B[1] & X$latitude <= DFW.AUS$T[1], ]
for (i in 2:nrow(DFW.AUS)) {
  temp <- X[X$longitude >= DFW.AUS$L[i] & X$longitude <= DFW.AUS$R[i] &
              X$latitude >= DFW.AUS$B[i] & X$latitude <= DFW.AUS$T[i], ]
  df.DFW.AUS <- rbind(df.DFW.AUS, temp)
}

## PLOTS
pdf(file = "../Figures/ARIMA Model Plots/Haynesville.pdf", height = 4.25, width = 10.0)
quilt.plot(df.Haynesville$longitude, df.Haynesville$latitude,
           df.Haynesville$methane_mixing_ratio_bias_corrected,
           xlim = range(X$longitude), ylim = range(X$latitude),
           xlab = "Longitude", ylab = "Lattitude",
           zlim = c(1575,1950))
title("Haynesville Methane Mixing Ratios")
US(add = TRUE)
dev.off()

pdf(file = "../Figures/ARIMA Model Plots/Wetlands.pdf", height = 4.25, width = 10.0)
quilt.plot(df.Wetlands$longitude, df.Wetlands$latitude,
           df.Wetlands$methane_mixing_ratio_bias_corrected,
           xlim = range(X$longitude), ylim = range(X$latitude),
           xlab = "Longitude", ylab = "Lattitude",
           zlim = c(1575,1950))
title("Wetlands Methane Mixing Ratios")
US(add = TRUE)
dev.off()

pdf(file = "../Figures/ARIMA Model Plots/DFWAUS.pdf", height = 4.25, width = 10.0)
quilt.plot(df.DFW.AUS$longitude, df.DFW.AUS$latitude,
           df.DFW.AUS$methane_mixing_ratio_bias_corrected,
           xlim = range(X$longitude), ylim = range(X$latitude),
           xlab = "Longitude", ylab = "Lattitude",
           zlim = c(1575,1950))
title("Dallas-Fort Worth to Austin Methane Mixing Ratios")
US(add = TRUE)
dev.off()


# trim the dataframes
keep = c('time', "time_utc", "methane_mixing_ratio_bias_corrected", "qa_value")

df.DFW.AUS = df.DFW.AUS[, names(df.DFW.AUS) %in% keep]
df.Haynesville = df.Haynesville[, names(df.Haynesville) %in% keep]
df.Wetlands = df.Wetlands[, names(df.Wetlands) %in% keep]



## ARIMA Model Analysis
## create the ARIMA dataframes

weekSeq <- my.week(X$time_utc)
weekSeq[weekSeq == '2018-53'] <- '2018-52'
weekSeq[weekSeq == '2019-53'] <- '2019-52'
weekSeq <- unique(weekSeq)

# Haynesville
arima.Haynesville <- arima.df(df.Haynesville, weekSeq)

# DFW-AUS
arima.DFW.AUS <- arima.df(df.DFW.AUS, weekSeq)

# Wetlands
arima.Wetlands <- arima.df(df.Wetlands, weekSeq)


# Create the ARIMA models
arima.model.Haynesville = auto.arima(as.numeric(arima.Haynesville$methane))
arima.model.DFW.AUS = auto.arima(as.numeric(arima.DFW.AUS$methane))
arima.model.Wetlands = auto.arima(as.numeric(arima.Wetlands$methane))


Haynesville.ARIMA = stats::arima(as.numeric(arima.Haynesville$methane), order=c(2,1,2))
Haynesville.Forecast = forecast(Haynesville.ARIMA, h=4)
x <- arima.Haynesville$time
for (i in 1:4) { x <- c(x, x[length(x)] + 604800) }
y <- c(arima.Haynesville$methane, Haynesville.Forecast$mean)
pdf(file = "../Figures/ARIMA Model Plots/Haynesville-ARIMA.pdf", height = 4.25, width = 10.0)
par(mar = c(5, 4, 1, 2) + 0.1)
plot(arima.Haynesville$time, arima.Haynesville$methane, type = 'l',
     xlab = "Time", ylab = "Methane Mixing Ratio", 
     # main = "1 Month Haynesville Forecast",
     xlim = range(x),
     ylim = c(1700, 1950))
lines(tail(x, 5), tail(y, 5), col = 'red')
points(tail(x, 4), tail(y, 4), col = 'red')
dev.off()
# GGPLOT version.
pdf(file = '../Figures/ARIMA Model Plots/gg/Haynesville-ARIMA.pdf', height = 4.25, width = 10.0)
gg.arima(arima.Haynesville, Haynesville.Forecast,
         title = '1 Month Haynesville Forecast')
dev.off()

DFW.AUS.ARIMA = stats::arima(as.numeric(arima.DFW.AUS$methane), order=c(1,1,1))
DFW.AUS.Forecast = forecast(DFW.AUS.ARIMA, h=4)
x <- arima.DFW.AUS$time
for (i in 1:4) { x <- c(x, x[length(x)] + 604800) }
y <- c(arima.DFW.AUS$methane, DFW.AUS.Forecast$mean)
pdf(file = "../Figures/ARIMA Model Plots/DFWAUS-ARIMA.pdf", height = 4.25, width = 10.0)
par(mar = c(5, 4, 1, 2) + 0.1)
plot(arima.DFW.AUS$time, arima.DFW.AUS$methane, type = 'l',
     xlab = "Time", ylab = "Methane Mixing Ratio", 
     # main = "1 Month Dallas-Fort Worth to Austin Forecast",
     xlim = range(x),
     ylim = c(1700, 1950))
lines(tail(x, 5), tail(y, 5), col = 'red')
points(tail(x, 4), tail(y, 4), col = 'red')
dev.off()
# GGPLOT version.
pdf(file = '../Figures/ARIMA Model Plots/gg/DFWAUS-ARIMA.pdf', height = 4.25, width = 10.0)
gg.arima(arima.DFW.AUS, DFW.AUS.Forecast,
         title = '1 Month Dallas-Fort Worth to Austin Forecast')
dev.off()


Wetlands.ARIMA = stats::arima(as.numeric(arima.Wetlands$methane), order=c(1,1,1))
Wetlands.Forecast = forecast(Wetlands.ARIMA, h=4)
x <- arima.Wetlands$time
for (i in 1:4) { x <- c(x, x[length(x)] + 604800) }
y <- c(arima.Wetlands$methane, Wetlands.Forecast$mean)
pdf(file = "../Figures/ARIMA Model Plots/Wetlands-ARIMA.pdf", height = 4.25, width = 10.0)
par(mar = c(5, 4, 1, 2) + 0.1)
plot(arima.Wetlands$time, arima.Wetlands$methane, type = 'l',
     xlab = "Time", ylab = "Methane Mixing Ratio", 
     # main = "1 Month Wetlands Forecast",
     xlim = range(x),
     ylim = c(1700, 1950))
lines(tail(x, 5), tail(y, 5), col = 'red')
points(tail(x, 4), tail(y, 4), col = 'red')
dev.off()
# GGPLOT version.
pdf(file = '../Figures/ARIMA Model Plots/gg/Wetlands-ARIMA.pdf', height = 4.25, width = 10.0)
gg.arima(arima.Wetlands, Wetlands.Forecast,
         title = '1 Month Wetlands Forecast')
dev.off()







# ARIMA Models with 1.0 qa Values

df.Haynesville = df.Haynesville[df.Haynesville$qa_value == 1.0, ]
df.DFW.AUS = df.DFW.AUS[df.DFW.AUS$qa_value == 1.0, ]
df.Wetlands = df.Wetlands[df.Wetlands$qa_value == 1.0, ]

# Haynesville
arima.Haynesville <- arima.df(df.Haynesville, weekSeq)

# DFW-AUS
arima.DFW.AUS <- arima.df(df.DFW.AUS, weekSeq)

# Wetlands
arima.Wetlands <- arima.df(df.Wetlands, weekSeq)

# Create the ARIMA models
arima.model.Haynesville = auto.arima(as.numeric(arima.Haynesville$methane))
arima.model.DFW.AUS = auto.arima(as.numeric(arima.DFW.AUS$methane))
arima.model.Wetlands = auto.arima(as.numeric(arima.Wetlands$methane))


Haynesville.ARIMA = stats::arima(as.numeric(arima.Haynesville$methane), order=c(1,1,1))
Haynesville.Forecast = forecast(Haynesville.ARIMA, h=4)
x <- arima.Haynesville$time
for (i in 1:4) { x <- c(x, x[length(x)] + 604800) }
y <- c(arima.Haynesville$methane, Haynesville.Forecast$mean)
pdf(file = "../Figures/ARIMA Model Plots/Haynesville-ARIMA-qa1.pdf", height = 4.25, width = 10.0)
par(mar = c(5, 4, 1, 2) + 0.1)
plot(arima.Haynesville$time, arima.Haynesville$methane, type = 'l',
     xlab = "Time", ylab = "Methane Mixing Ratio", 
     # main = "1 Month Haynesville Forecast",
     xlim = range(x),
     ylim = c(1800, 1950))
lines(tail(x, 5), tail(y, 5), col = 'red')
points(tail(x, 4), tail(y, 4), col = 'red')
dev.off()
# GGPLOT version.
pdf(file = '../Figures/ARIMA Model Plots/gg/Haynesville-ARIMA-qa1.pdf', height = 4.25, width = 10.0)
gg.arima(arima.Haynesville, Haynesville.Forecast,
         title = 'High QA - 1 Month Haynesville Forecast')
dev.off()


DFW.AUS.ARIMA = stats::arima(as.numeric(arima.DFW.AUS$methane), order=c(2,1,1))
DFW.AUS.Forecast = forecast(DFW.AUS.ARIMA, h=4)
x <- arima.DFW.AUS$time
for (i in 1:4) { x <- c(x, x[length(x)] + 604800) }
y <- c(arima.DFW.AUS$methane, DFW.AUS.Forecast$mean)
pdf(file = "../Figures/ARIMA Model Plots/DFWAUS-ARIMA-qa1.pdf", height = 4.25, width = 10.0)
par(mar = c(5, 4, 1, 2) + 0.1)
plot(arima.DFW.AUS$time, arima.DFW.AUS$methane, type = 'l',
     xlab = "Time", ylab = "Methane Mixing Ratio", 
     # main = "1 Month Dallas-Fort Worth to Austin Forecast",
     xlim = range(x),
     ylim = c(1800, 1950))
lines(tail(x, 5), tail(y, 5), col = 'red')
points(tail(x, 4), tail(y, 4), col = 'red')
dev.off()
# GGPLOT version.
pdf(file = '../Figures/ARIMA Model Plots/gg/DFWAUS-ARIMA-qa1.pdf', height = 4.25, width = 10.0)
gg.arima(arima.DFW.AUS, DFW.AUS.Forecast,
         title = 'High QA - 1 Month Dallas-Fort Worth to Austin Forecast')
dev.off()


Wetlands.ARIMA = stats::arima(as.numeric(arima.Wetlands$methane), order=c(1,1,1))
Wetlands.Forecast = forecast(Wetlands.ARIMA, h=4)
x <- arima.Wetlands$time
for (i in 1:4) { x <- c(x, x[length(x)] + 604800) }
y <- c(arima.Wetlands$methane, Wetlands.Forecast$mean)
pdf(file = "../Figures/ARIMA Model Plots/Wetlands-ARIMA-qa1.pdf", height = 4.25, width = 10.0)
par(mar = c(5, 4, 1, 2) + 0.1)
plot(arima.Wetlands$time, arima.Wetlands$methane, type = 'l',
     xlab = "Time", ylab = "Methane Mixing Ratio", 
     # main = "1 Month Wetlands Forecast",
     xlim = range(x),
     ylim = c(1800, 1950))
lines(tail(x, 5), tail(y, 5), col = 'red')
points(tail(x, 4), tail(y, 4), col = 'red')
dev.off()
# GGPLOT version.
pdf(file = '../Figures/ARIMA Model Plots/gg/Wetlands-ARIMA-qa1.pdf', height = 4.25, width = 10.0)
gg.arima(arima.Wetlands, Wetlands.Forecast,
         title = 'High QA - 1 Month Wetlands Forecast')
dev.off()





