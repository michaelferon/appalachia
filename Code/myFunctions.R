## FUNCTIONS


get.basemap <- function(source, type) {
  map <- get_map(location = c(mean(lonBounds), mean(latBounds)),
                 source = source, maptype = type, zoom = 6)
  map <- ggmap(map)
  map <- map + scale_x_continuous(limits = lonBounds, expand = c(0, 0))
  map <- map + scale_y_continuous(limits = latBounds, expand = c(0, 0))
  
  return(map)
}


my.yday <- function(data) {
  return( paste(year(data), '-', yday(data), sep='') )
}


my.week <- function(data) {
  return( paste(year(data), '-', week(data), sep='') )
}


my.month <- function(data) {
  return( paste(year(data), '-', month(data), sep='') )
}


assign.quadrants <- function(X, latTicks, lonTicks, RESO) {
  for (i in 1:RESO) {
    for (j in 1:RESO) {
      X$quadrant[ X$latitude >= latTicks[i] & X$longitude <= lonTicks[j] & X$quadrant == 0 ] <- RESO*(i - 1) + j
    }
  }
  
  return(X)
}


data.aggregate <- function(X, dateTimes.period, uniqueTimes, type) {
  data.period <- vector(mode = 'list', length = length(uniqueTimes))
  names(data.period) <- paste(type, uniqueTimes, sep = '')
  for (i in 1:length(data.period)) {
    data.period[[i]] <- X[dateTimes.period == uniqueTimes[i], ]
  }
  
  return(data.period)
}


means.matrix <- function(X, data, dateTimes.period, uniqueTimes, RESO) {
  periodMeans <- matrix(data = NA, nrow = length(uniqueTimes), ncol = RESO^2)
  for (i in 1:length(uniqueTimes)) {
     value <- tapply(data[[i]]$methane_mixing_ratio_bias_corrected, data[[i]]$quadrant, mean)
     temp <- as.numeric(names(value))
     periodMeans[i, temp] <- value
  }
  
  periodMeans <- as.data.frame(periodMeans, row.names = names(data))
  periodMeans <- cbind(as.POSIXlt(sort(tapply(X$time, dateTimes.period, mean)), origin = '2010-01-01 00:00:00', tz = 'UTC'), periodMeans)
  colnames(periodMeans) <- c('Date', paste('Q', 1:(RESO^2), sep = ''))
  
  return(periodMeans)
}


prop.matrix <- function(uniqueTimes, data, RESO, local = FALSE) {
  
  mat <- matrix(0, RESO, RESO)
  nMat <- matrix(0, RESO, RESO)
  
  for (i in 1:length(uniqueTimes)) {
    df <- data[[i]]
    if (!local) {
      baseMean <- mean(df$methane_mixing_ratio_bias_corrected, na.rm = TRUE)
    }
    for (quadrant in 1:(RESO^2)) {
      j <- ceiling(quadrant / RESO)
      k <- (quadrant - 1) %% RESO + 1
      if (local) {
        neighbors <- ( (df$quadrant == quadrant - 1) | (df$quadrant == quadrant + 1) ) |
          ( (df$quadrant <= quadrant - RESO + 1) & (df$quadrant >= quadrant - RESO - 1) ) |
          ( (df$quadrant <= quadrant + RESO + 1) & (df$quadrant >= quadrant + RESO - 1) )
        baseMean <- mean(df$methane_mixing_ratio_bias_corrected[neighbors], na.rm = TRUE)
      }
      qMean <- mean(df$methane_mixing_ratio_bias_corrected[df$quadrant == quadrant], na.rm = TRUE)
      plusOne <- as.numeric(baseMean < qMean)
      if (!is.na(plusOne)) {
        mat[j, k] <- mat[j, k] + plusOne
        nMat[j, k] <- nMat[j, k] + 1
      }
    }
  }
  
  return(list(mat = mat/nMat, nMat = nMat))
}


ggmap.prop.matrix <- function(prop.mat, lat, lon, basemap, type, pTitle = NULL, opacity = 0.65) {
  df <- as.data.frame(matrix(NA, nrow = RESO^2, ncol = 3))
  colnames(df) <- c('longitude', 'latitude', 'value')
  df$longitude <- rep(lon, each = RESO)
  df$latitude <- rep(lat, times = RESO)
  df$value <- c(prop.mat)
  df <- na.omit(df)
  
  if (type == 'Daily') {
    lTitle <- expression(paste('Estimated proportion of days with above-average ', 'CH'[4], ' levels'))
  } else if (type == 'Weekly') {
    lTitle <- expression(paste('Estimated proportion of weeks with above-average ', 'CH'[4], ' levels'))
  } else if (type == 'Monthly') {
    lTitle <- expression(paste('Estimated proportion of months with above-average ', 'CH'[4], ' levels'))
  }
  
  if (is.null(pTitle)) {
    pTitle <- paste(type, 'Elevated Methane Levels')
  }
  
  width <- abs(mean(diff(lon)))
  height <- abs(mean(diff(lat)))

  g <- basemap
  g <- g + geom_tile(data = df, height = height, width = width, alpha = opacity,
                     mapping = aes(x = longitude, y = latitude, fill = value))
  g <- g + scale_fill_gradientn(colors = hcl.colors(16, "YlOrRd", rev = TRUE))
  g <- g + ggtitle(pTitle) + xlab('Longitude') + ylab('Latitude')
  g <- g + theme(legend.key.width = unit(2.25, 'cm'))
  g <- g + theme(legend.position = 'bottom')
  g <- g + guides(fill = guide_colorbar(title = lTitle, title.position = 'top'))
  
  return(g)
}


setup.3day.a <- function(data) {
  daysInMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  yearName <- year(data$time_utc[1])
  monthName <- months(data$time_utc[1])
  monthNum <- month(data$time_utc[1])
  maxDay <- daysInMonth[monthNum]
  numGroups <- floor(maxDay / 3)
  group <- floor((mday(data$time_utc) - 1) / 3) + 1
  group[group > numGroups] <- numGroups
  
  return(list(daysInMonth = daysInMonth, yearName = yearName, monthName = monthName,
              monthNum = monthNum, maxDay = maxDay, numGroups = numGroups, group = group))
}


setup.3day.b <- function(i, numGroups, monthNum, monthName, yearName, maxDay) {
  firstDay <- 3*(i - 1) + 1
  lastDay <- 3*(i - 1) + 3
  if (maxDay == 31 | maxDay == 28) {
    if (i == numGroups) {
      lastDay <- lastDay + 1
    }
  }
  
  pasteDay <- firstDay
  if (pasteDay < 10) { pasteDay <- paste(0, pasteDay, sep='') }
  pasteMonth <- monthNum
  if (pasteMonth < 10) { pasteMonth <- paste(0, pasteMonth, sep='') }
  t <- paste(monthName, ' ', firstDay, ' - ', monthName, ' ', lastDay, ', ', yearName, sep='')
  
  return(list(pasteDay = pasteDay, pasteMonth = pasteMonth, t = t))
}


quilt.plot.month <- function(data, xLim, yLim, zLim, outdir = '../Figures/3DayPlots/marginal-qa/') {
  a <- setup.3day.a(data)
  
  for (i in 1:(a$numGroups)) {
    df <- data[a$group == i, ]
    if (dim(df)[1] == 0) { next }
    
    b <- setup.3day.b(i, a$numGroups, a$monthNum, a$monthName, a$yearName, a$maxDay)
    
    pdf(file = paste(outdir, a$yearName, '-', b$pasteMonth, '-', b$pasteDay, '.pdf', sep=''),
        height = 6, width = 9)
    quilt.plot(df$longitude, df$latitude, df$methane_mixing_ratio_bias_corrected, na.rm = TRUE,
               main = b$t, xlim = xLim, ylim = yLim, zlim = zLim,
               xlab = 'Longitude', ylab = 'Latitude')
    US(add = TRUE, lwd = 2)
    dev.off()
  }
}


ggmap.plot.month <- function(data, basemap, lat, lon, RESO, zLim, outdir = '../Figures/gg3DayPlots/marginal-qa/') {
  a <- setup.3day.a(data)
  
  for (i in 1:(a$numGroups)) {
    df <- data[a$group == i, ]
    df.plot <- as.data.frame(matrix(NA, nrow = RESO^2, ncol = 3))
    colnames(df.plot) <- c('longitude', 'latitude', 'methane_mixing_ratio_bias_corrected')
    df.plot$longitude <- rep(lon, times = RESO)
    df.plot$latitude <- rep(lat, each = RESO)
    value <- tapply(df$methane_mixing_ratio_bias_corrected, df$quadrant, mean)
    value[value < zLim[1]] <- zLim[1]
    value[value > zLim[2]] <- zLim[2]
    temp <- as.numeric(names(value))
    df.plot$methane_mixing_ratio_bias_corrected[temp] <- value
    df.plot <- na.omit(df.plot)
    
    width <- abs(mean(diff(lon)))
    height <- abs(mean(diff(lat)))
    
    b <- setup.3day.b(i, a$numGroups, a$monthNum, a$monthName, a$yearName, a$maxDay)
    
    pdf(file = paste(outdir, a$yearName, '-', b$pasteMonth, '-', b$pasteDay, '.pdf', sep=''),
        height = 5.25, width = 9)
    my.plot <- ggmap.geom_tile(df.plot, basemap, height, width, b$t, zLim)
    print(my.plot)
    dev.off()
  }
}


ggmap.geom_tile <- function(df, basemap, height, width, t, zLim) {
  
  lTitle <- expression(paste('CH'[4], ' Column Average Mixing Ratio [ppbv]'))
  
  g <- basemap
  g <- g + geom_tile(data = df, height = height, width = width, alpha = 0.65,
                     mapping = aes(x = longitude, y = latitude,
                                   fill = methane_mixing_ratio_bias_corrected))
  g <- g + scale_fill_gradientn(colors = tim.colors(64), limits = zLim)
  g <- g + ggtitle(t) + xlab('Longitude') + ylab('Latitude')
  g <- g + theme(legend.key.width = unit(2.25, 'cm'))
  g <- g + theme(legend.position = 'bottom')
  g <- g + guides(fill = guide_colorbar(title = lTitle, title.position = 'top'))
  return(g)
}





