rm( list = ls() )

# Make sure you're in the 'Code' directory.
## DO NOT RUN THIS CODE.
## WARNING: The conversion from hdf4 to nc takes a LOT of space.
## The necessary data is saved to mask.Rdata.

load('../Data/data-full/dataFull.Rdata')
source('myFunctions.R')
library(ncdf4)

# Directory where `.nc` files are stored, having been converted from `.hdf`.
# These data are NOT STORED IN DROPBOX. THEY ONLY EXIST ON MICHAEL'S MACHINE.
data.dir <- '../Data/mask/nc/'
file_names <- paste(data.dir, list.files(data.dir), sep='')
".DS_Store" %in% file_names
any(is.na(file_names))
length(file_names)


file <- nc_open(file_names[1], write = FALSE)

# Read data from `.nc` files.
lat_vals <- ncvar_get(file, 'latitude')
lon_vals <- ncvar_get(file, 'longitude')
water <- ncvar_get(file, 'water_mask')

# mask: dataframe with all the land-water mask data.
mask <- data.frame(longitude = c(lon_vals), latitude = c(lat_vals), water = c(water))

# Get data from each `.nc` file.
for (i in 2:4) {
  file <- nc_open(file_names[i], write = FALSE)
  lat_vals <- ncvar_get(file, 'latitude')
  lon_vals <- ncvar_get(file, 'longitude')
  water <- ncvar_get(file, 'water_mask')
  
  temp <- data.frame(longitude = c(lon_vals), latitude = c(lat_vals), water = c(water))
  mask <- rbind(mask, temp)
}



latBounds <- range(X$latitude)
lonBounds <- range(X$longitude)

# Restrict data to only those points which lie within our region of interest.
mask <- subset(mask, lonBounds[1] <= longitude & longitude <= lonBounds[2])
mask <- subset(mask, latBounds[1] <= latitude & latitude <= latBounds[2])
rownames(mask) <- NULL
mask$quadrant <- 0

# Mask data saved to mask.Rdata, to be processed by `mask.R`.
rm(dateTimes, file, lat_vals, lon_vals, temp, water, data.dir, file_names, i, latBounds, lonBounds)
save(mask, file = '../Data/mask/mask.Rdata')
