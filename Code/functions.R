## FUNCTIONS

# Returns anomaly matrix, where element ij represents the proportion of times that the mean of quadrant ij
# was above the 'baseline' mean. The baseline is defined globally by default, but can be set to only consider
# neighboring quadrants using the parameter 'local'. With 'adj' set to TRUE, a PROPORTION of times is returned.
# Otherwise, the NUMBER of times is returned, but this is sensitive to sparsity in certain quadrants.
anomaly.matrix <- function(uniqueTimes, data, RESO, local = FALSE, adj = TRUE) {
  
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
  
  if (adj) {
    return(mat / nMat)
  } else {
    return(mat)
  }
}


my.yday <- function(data) {
  return( paste(year(data), '-', yday(data), sep='') )
}

my.week <- function(data) {
  return( paste(year(data), '-', week(data), sep='') )
}

my.month <- function(data) {
  return( paste(year(data), '-', month(data), sep=''))
}


plot.month <- function(data, xLim, yLim, outdir = NULL) {
  
  daysInMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  yearName <- unique(year(data$time_utc))
  monthName <- unique(months(data$time_utc))
  monthNum <- unique(month(data$time_utc))
  maxDay <- daysInMonth[monthNum]
  numPlots <- floor(maxDay / 3)
  group <- floor((mday(data$time_utc) - 1) / 3) + 1
  group[group > numPlots] <- numPlots
  
  for (i in 1:numPlots) {
    df <- data[group == i, ]
    firstDay <- 3*(i - 1) + 1
    lastDay <- 3*(i - 1) + 3
    if (maxDay == 31 | maxDay == 28) {
      if (i == numPlots) {
        lastDay <- lastDay + 1
      }
    }
    
    if (!is.null(outdir)) {
      pasteDay <- firstDay
      if (pasteDay < 10) { pasteDay <- paste(0, pasteDay, sep='') }
      pdf(file = paste(outdir, yearName, '-', monthNum, '-', pasteDay, '.pdf', sep=''),
          height = 7, width = 10) 
    }
    
    t <- paste(monthName, ' ', firstDay, ' - ', monthName, ' ', lastDay, ', ', yearName, sep='')
    quilt.plot(df$longitude, df$latitude, df$methane_mixing_ratio_bias_corrected,
               main = t, xlim = c(xMin, xMax), ylim = c(yMin, yMax),
               xlab = 'Longitude', ylab = 'Latitude')
    US(add = TRUE, col = 'grey', lwd = 2)
    
    if (!is.null(outdir)) {
      dev.off() 
    }
  }
}



