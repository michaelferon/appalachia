## FUNCTIONS


get.basemap <- function(source, type, lonBounds, latBounds) {
  map <- get_map(location = c(mean(lonBounds), mean(latBounds)),
                 source = source, maptype = type, zoom = 6)
  map <- ggmap(map)
  map <- map + scale_x_continuous(limits = lonBounds, expand = c(0, 0))
  map <- map + scale_y_continuous(limits = latBounds, expand = c(0, 0))
  
  return(map)
}


my.yday <- function(data) {
  temp <- yday(data)
  lt100 <- temp < 100
  lt10 <- temp < 10
  temp[lt100] <- paste('0', temp[lt100], sep='')
  temp[lt10] <- paste('0', temp[lt10], sep='')
  return( paste(year(data), '-', temp, sep='') )
}


my.week <- function(data) {
  temp <- week(data)
  temp[temp < 10] <- paste('0', temp[temp < 10], sep='')
  return( paste(year(data), '-', temp, sep='') )
}


my.month <- function(data) {
  temp <- month(data)
  temp[temp < 10] <- paste('0', temp[temp < 10], sep='')
  return( paste(year(data), '-', temp, sep='') )
}


assign.quadrants <- function(X, latTicks, lonTicks, RESO) {
  templat <- as.numeric(factor(cut(X$latitude, latTicks, labels = 1:RESO, include.lowest = TRUE),
                               levels = RESO:1))
  templon <- as.numeric(cut(X$longitude, lonTicks, labels = 1:RESO, include.lowest = TRUE))
  X$quadrant <- RESO*(templat - 1) + templon
  
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
    value <- tapply(data[[i]]$methane_mixing_ratio_bias_corrected, data[[i]]$quadrant, mean, na.rm = TRUE)
    temp <- as.numeric(names(value))
    periodMeans[i, temp] <- value
  }
  
  periodMeans <- as.data.frame(periodMeans, row.names = names(data))
  periodMeans <- cbind(as.POSIXlt(sort(tapply(X$time, dateTimes.period, mean)), origin = '2010-01-01 00:00:00', tz = 'UTC'), periodMeans)
  colnames(periodMeans) <- c('Date', paste('Q', 1:(RESO^2), sep = ''))
  
  return(periodMeans)
}


prop.matrix <- function(uniqueTimes, data, RESO) {
  
  mat <- matrix(0, RESO, RESO)
  nMat <- matrix(0, RESO, RESO)
  
  for (i in 1:length(uniqueTimes)) {
    df <- data[[i]]
    baseMean <- mean(df$methane_mixing_ratio_bias_corrected, na.rm = TRUE)
    meanVec <- rep(NA, RESO^2)
    qMeans <- tapply(df$methane_mixing_ratio_bias_corrected, df$quadrant, mean, na.rm = TRUE)
    meanVec[as.numeric(names(qMeans))] <- qMeans
    compVec <- as.numeric(meanVec > baseMean)
    
    nMat <- nMat + matrix(as.numeric(!is.na(compVec)), nrow = RESO, ncol = RESO, byrow = TRUE)
    compVec[is.na(compVec)] <- 0
    
    mat <- mat + matrix(compVec, nrow = RESO, ncol = RESO, byrow = TRUE)
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
  g <- g + xlab('Longitude') + ylab('Latitude')
  g <- g + theme(legend.key.height = unit(1.0, 'cm'))
  g <- g + labs(fill = '')
  
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
    if (dim(df)[1] <= 1) { next }
    
    b <- setup.3day.b(i, a$numGroups, a$monthNum, a$monthName, a$yearName, a$maxDay)
    
    pdf(file = paste(outdir, a$yearName, '-', b$pasteMonth, '-', b$pasteDay, '.pdf', sep=''),
        height = 4.25, width = 9)
    par(mar = c(2, 4, 2, 2) + 0.1)
    quilt.plot(df$longitude, df$latitude, df$methane_mixing_ratio_bias_corrected, na.rm = TRUE,
               xlim = xLim, ylim = yLim, zlim = zLim)
    title(b$t, line = 0.30, cex.main = 1.5)
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
        height = 6.5, width = 7.5)
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
  g <- g + xlab('Longitude') + ylab('Latitude')
  g <- g + theme(legend.key.height = unit(1.0, 'cm'))
  g <- g + labs(fill = '')
  return(g)
}


wind.shift <- function(X,time_shift){
  X_wind = X
  #69 (roughly) represents number of miles in 1 degree latitude.
  #Difficult to calculate exactly like for longitude, but 69 should be an accurate enough measurement for our purposes
  X_wind[,'latitude'] = X[,'latitude'] + (1/69) *time_shift * X[,'eastward_wind']
  
  #Length of 1 degree of Longitude in miles = cosine (latitude in decimal degrees) * length of degree (miles) at equator.
  #Could change this to use an integral to exactly calculate time shift and remove some bias
  X_wind[,'longitude'] = X[,'longitude'] + (1/69.172) * cos(X[,'latitude']) * time_shift *X[,'northward_wind']
  
  #Times must be shifted forward by time_shift hours
  #Date data type allows for addition of seconds
  X_wind[,'time'] = X[,'time'] + 60*60*time_shift
  X_wind[,'time_utc'] = X[,'time_utc'] + 60*60*time_shift
  return(X_wind)
}


arima.df <- function(data, weekSeq) {
  weeks <- my.week(data$time_utc)
  weeks[weeks == '2018-53'] <- '2018-52'
  weeks[weeks == '2019-53'] <- '2019-52'
  vec <- tapply(data$methane_mixing_ratio_bias_corrected,
                weeks,
                mean)
  times <- tapply(data$time, weeks, mean) %>%
    as.POSIXct(tz = 'UTC', origin = '2010-01-01 00:00:00')
  df <- data.frame(week = weekSeq, time = NA, methane = NA)
  for (i in 1:length(vec)) {
    ind <- which(my.week(times[i]) == weekSeq)
    df[ind, 'time'] <- times[i]
    df[ind, 'methane'] <- vec[i]
  }
  df$time <- as.POSIXct(df$time, tz = 'UTC', origin = '1970-01-01')
  inds <- which(is.na(df$methane))
  for (ind in inds) {
    df[ind, 'time'] <- df[ind - 1, 'time'] + 604800
    df[ind, 'methane'] <- df[ind - 1, 'methane']
  }
  
  return(df)
}


gg.arima <- function(data, forecast, title, h = 4) {
  x <- data$time
  for (i in 1:h) { x <- c(x, x[length(x)] + 604800) }
  y <- c(data$methane, forecast$mean)
  
  df <- tibble(
    time = x,
    methane = y,
    lower = as.numeric(NA),
    upper = as.numeric(NA)
  )
  df$lower[(nrow(df) - h + 1):nrow(df)] <- forecast$lower %>%
    as.matrix %>% .[, 1]
  df$upper[(nrow(df) - h + 1):nrow(df)] <- forecast$upper %>%
    as.matrix %>% .[, 1]
  g <- ggplot(df, aes(time, methane)) +
    geom_line() +
    geom_errorbar(data = (df %>% top_n(-h)),
                  aes(ymin = lower, ymax = upper), size = 2.5,
                  alpha = 0.25, color = 'orangered4') +
    geom_point(data = (df %>% top_n(-h)),
               mapping = aes(time, methane),
               color = 'red') +
    theme_minimal() +
    ggtitle(title) +
    xlab('Time') + ylab('Methane Mixing Ratio') +
    ylim(1701.5, 1907.5) +
    xlim(as.POSIXct('2018-05-03 17:02:17', tz = 'UTC'),
         as.POSIXct('2020-02-03 17:32:33', tz = 'UTC') + 604800*h + 1)
  return(g)
}

fourier.df <- function(data, tsbounds = c(1800, 1907.252), mbounds = c(0, 683.1120), type = 'daily', k = 6) {
  if (type == 'daily') {
    mid <- 351
    time <- seq(as.Date('2018-05-01'), as.Date('2020-03-31'), by = 1)
    fact <- my.yday(data$time_utc)
  } else if (type == 'weekly') {
    mid <- 51
    time = seq(as.Date('2018-05-02'), as.Date('2020-03-31'), length = 100)
    fact <- my.week(data$time_utc)
    fact[fact == '2018-53'] <- '2018-52'
    fact[fact == '2019-53'] <- '2019-52'
  }
  df <- tibble(
    time = time,
    methane = as.numeric(NA)
  )
  vec <- tapply(
    data$methane_mixing_ratio_bias_corrected,
    fact,
    mean
  )
  
  for (i in 1:length(vec)) {
    if (type == 'daily') {
      df$methane[my.yday(df$time) == names(vec)[i]] <- vec[i]
    } else if (type == 'weekly') {
      df$methane[my.week(df$time) == names(vec)[i]] <- vec[i]
    }
  }
  
  for (i in 1:nrow(df)) {
    a <- i - k
    b <- i + k
    if (a < 1) {
      b <- b + abs(a) + 1
      a <- a + abs(a) + 1
    }
    if (b > nrow(df)) {
      a <- a + nrow(df) - b
      b <- nrow(df)
    }
    if (is.na(df$methane[i])) {
      df$methane[i] <- mean(df$methane[a:b], na.rm = TRUE)
    }
  }
  
  methaneMean <- mean(df$methane)
  x <- seq(1, nrow(df), by = 1)
  model <- tibble(
    time = df$time,
    methane = lm(df$methane ~ x)$fitted.values
  )
  
  ts <- ggplot(df, aes(time, methane)) +
    geom_line(color = 'SteelBlue') +
    theme_minimal() +
    xlab('Time') + ylab('Methane Mixing Ratio') +
    ylim(tsbounds)
  
  ts.trend <- ggplot(df, aes(time, methane)) +
    geom_line(data = model, color = 'OrangeRed3',
              alpha = 0.75, linetype = 'dashed') +
    geom_line(color = 'SteelBlue') +
    theme_minimal() +
    xlab('Time') + ylab('Methane Mixing Ratio') +
    ylim(tsbounds)
  
  df.adj <- df %>%
    mutate(methane = methane - model$methane + methaneMean)
  
  ts.adj <- ggplot(df.adj, aes(time, methane)) +
    geom_line(color = 'SteelBlue') +
    theme_minimal() +
    xlab('Time') + ylab('Methane Mixing Ratio') +
    ylim(tsbounds)
  
  hz <- seq(0, 1, length = nrow(df))
  if (type == 'daily') {
    hzMonth  <- hz * 365/12
  } else if (type == 'weekly') {
    hzMonth <- hz * 365/(7 * 12)
  }
  
  df <- df %>%
    mutate_at('methane', function(x) x - mean(x)) %>%
    mutate(
      fft = fft(methane),
      mod = Mod(fft),
      arg = Arg(fft),
      hz = hz,
      hzMonth = hzMonth,
      period = 1/hz,
      periodMonth = 1/hzMonth,
      group = cut(mod, breaks = quantile(mod, probs = c(0, 0.95, 1)),
                  labels = c('Low', 'High'), include.lowest = TRUE)
      # group = factor(mod > 200, labels = c('Low', 'High'))
    )
  df.adj <- df.adj %>%
    mutate_at('methane', function(x) x - mean(x)) %>%
    mutate(
      fft = fft(methane),
      mod = Mod(fft),
      arg = Arg(fft),
      hz = hz,
      hzMonth = hzMonth,
      period = 1/hz,
      periodMonth = 1/hzMonth,
      group = cut(mod, breaks = quantile(mod, probs = c(0, 0.95, 1)),
                  labels = c('Low', 'High'), include.lowest = TRUE)
      # group = factor(mod > 200, labels = c('Low', 'High'))
    )
  
  
  if (type == 'daily') {
    breaks = seq(0, 16, by = 1)
  } else if (type == 'weekly') {
    breaks = seq(0, 2.2, by = 0.2)
  }
  
  g <- df %>% slice(1:mid) %>%
    ggplot(aes(x = hzMonth, y = 0, xend = hzMonth, yend = mod, color = group)) +
    scale_color_manual(values = c('SteelBlue', 'OrangeRed3')) +
    geom_segment() +
    # ggtitle('Frequency Spectrum') +
    xlab(expression(paste('Frequency (months'^-1, ')'))) +
    ylab('Magnitude, |z|') +
    labs(color = 'Magnitude') +
    scale_x_continuous(breaks = breaks) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    ylim(mbounds)
  g.adj <- df.adj %>% slice(1:mid) %>%
    ggplot(aes(x = hzMonth, y = 0, xend = hzMonth, yend = mod, color = group)) +
    scale_color_manual(values = c('SteelBlue', 'OrangeRed3')) +
    geom_segment() +
    # ggtitle('Frequency Spectrum') +
    xlab(expression(paste('Frequency (months'^-1, ')'))) +
    ylab('Magnitude, |z|') +
    labs(color = 'Magnitude') +
    scale_x_continuous(breaks = breaks) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    ylim(mbounds)
  
  p <- df %>% slice(1:mid) %>%
    ggplot(aes(x = periodMonth, y = 0, xend = periodMonth,
               yend = mod, color = group)) +
    scale_color_manual(values = c('SteelBlue', 'OrangeRed3')) +
    geom_segment() +
    # ggtitle('Period Spectrum') +
    xlab(expression(paste('Period, in days, on a log'[10], ' scale'))) +
    ylab('Magnitude, |z|') +
    labs(color = 'Magnitude') +
    scale_x_log10(n.breaks = 12) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    ylim(mbounds)
  p.adj <- df.adj %>% slice(1:mid) %>%
    ggplot(aes(x = periodMonth, y = 0, xend = periodMonth,
               yend = mod, color = group)) +
    scale_color_manual(values = c('SteelBlue', 'OrangeRed3')) +
    geom_segment() +
    # ggtitle('Period Spectrum') +
    xlab(expression(paste('Period, in days, on a log'[10], ' scale'))) +
    ylab('Magnitude, |z|') +
    labs(color = 'Magnitude') +
    scale_x_log10(n.breaks = 12) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    ylim(mbounds)
  
  return(list(df = df, g = g, p = p, ts = ts, ts.trend = ts.trend,
              df.adj = df.adj, g.adj = g.adj, p.adj = p.adj, ts.adj = ts.adj))
}
