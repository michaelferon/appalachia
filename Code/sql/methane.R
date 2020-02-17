rm( list = ls() )

# Make sure you're in the "Code" directory.

load("../../Data/data-full/varDictFull.Rdata")
library(lubridate)


# Variables of interest (for now).
vars <- c("time_utc",
          "time",
          "latitude",
          "longitude",
          "methane_mixing_ratio",
          "methane_mixing_ratio_bias_corrected",
          "qa_value",
          "eastward_wind",
          "northward_wind",
          "solar_zenith_angle",
          "viewing_zenith_angle",
          "water_total_column",
          "surface_albedo_SWIR",
          "surface_albedo_NIR",
          "aerosol_optical_thickness_SWIR",
          "surface_altitude",
          "surface_altitude_precision",
          "surface_classification",
          "surface_pressure",
          "apparent_scene_pressure")

# Matrix dimensions.
m <- length(var_dict$methane_mixing_ratio)
n <- length(vars)

# Construct data matrix, X.
X <- matrix(data = 0, nrow = m, ncol = n, dimnames = list(NULL, vars))
for (i in 2:n) {
  X[, i] <- var_dict[[vars[i]]]
}
X <- as.data.frame(na.omit(X)) # Convert to data-frame.

# Get datetimes.
X$time_utc <- as.POSIXlt(X$time, origin = "2010-01-01 00:00:00", tz = "UTC")
X <- X[order(X$time, X$latitude), ] # Sort by time.

rm(i, m, n, vars, var_dict)

write.csv(X, file = "methane.csv", row.names = FALSE)

