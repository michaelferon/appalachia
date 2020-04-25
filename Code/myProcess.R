rm( list = ls() )

# Make sure you're in the 'Code' directory.

load('../Data/data-full/varDictFull.Rdata')
load('../Data/data-full/varDictNew.Rdata')
library(lubridate)


# Variables of interest (for now).
vars <- c('time_utc',
          'time',
          'longitude',
          'latitude',
          'methane_mixing_ratio_bias_corrected',
          'qa_value',
          'eastward_wind',
          'northward_wind',
          'surface_altitude',
          'quadrant')

# Matrix dimensions.
mx <- length(var_dict$methane_mixing_ratio_bias_corrected)
mz <- length(var_dict_new$methane_mixing_ratio_bias_corrected)
n <- length(vars)

# Construct data matrices, X and Z.
X <- matrix(data = 0, nrow = mx, ncol = n, dimnames = list(NULL, vars))
Z <- matrix(data = 0, nrow = mz, ncol = n, dimnames = list(NULL, vars))
for (i in 2:(n-1)) {
  X[, i] <- var_dict[[vars[i]]]
  Z[, i] <- var_dict_new[[vars[i]]]
}
X <- as.data.frame(na.omit(X)) # Convert to data-frame.
Z <- as.data.frame(na.omit(Z))

X <- rbind(X, Z)

X <- X[X$qa_value > 0.2, ]



X$eastward_wind[X$eastward_wind == 999] <- NA
X$northward_wind[X$northward_wind == 999] <- NA

X$qa_value[X$qa_value < 0.5] <- 0.4
X$qa_value[X$qa_value > 0.5] <- 1.0
X$qa_value <- as.factor(X$qa_value)

# Get datetimes.
X$time_utc <- as.POSIXct(X$time, origin = '2010-01-01 00:00:00', tz = 'UTC')
X <- X[order(X$time, X$longitude), ] # Sort by time and longitude.
dateTimes <- X$time_utc

row.names(X) <- NULL

rm(i, mx, mz, n, vars, var_dict, var_dict_new, Z)
save(list = ls(), file = '../Data/data-full/dataFull.Rdata')


