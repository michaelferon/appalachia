rm( list = ls() )

# Make sure you're in the "Code" directory.

load('../Data/data-full/dataFullMask.Rdata')
source('myFunctions.R')

library(lubridate)
library(ggplot2)
library(ggmap)
library(config)
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(zoo)
library(viridis)

# Need a Google API key for some of the stuff below.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

X <- X %>% as_tibble
file <- paste(
  '../Data/population/gpw-v4-population-density-rev11_2020_30_',
  'sec_tif/gpw_v4_population_density_rev11_2020_30_sec.tif',
   sep=''
)

lonBounds <- range(X$longitude)
latBounds <- range(X$latitude)
basemap.hybrid <- get.basemap('google', 'hybrid', lonBounds, latBounds)

lonTicks <- seq(-180, 180, length = 43201)
latTicks <- rev(seq(-90, 90, length = 21601))
lon <- rollmean(lonTicks, 2)
lat <- rollmean(latTicks, 2)

width <- abs(mean(diff(lon)))
height <- abs(mean(diff(lat)))

goodLat <- which(latBounds[1] <= lat & lat <= latBounds[2])
goodLon <- which(lonBounds[1] <= lon & lon <= lonBounds[2])



data <- raster(file)

pop <- data[goodLat, goodLon, ] %>%
  matrix(nrow = length(goodLat), ncol = length(goodLon), byrow = TRUE)
df <- tibble(
  pop = c(pop),
  lat = rep(lat[goodLat], times = length(goodLon)),
  lon = rep(lon[goodLon], each = length(goodLat))
) %>% na.omit

df <- df %>%
  mutate(log10pop = log10(pop))
df$log10pop[is.infinite(df$log10pop)] <- min(df$log10pop[is.finite(df$log10pop)])


# basemap.hybrid +
#   geom_raster(
#     data = df,
#     mapping = aes(x = lon, y = lat, fill = log10pop)
#   ) +
#   scale_fill_gradientn(colors = viridis(100))

pdf(file = '../Figures/popTile.pdf', height = 3.0, width = 4.5)
g <- basemap.hybrid +
  geom_tile(data = df, height = height, width = width, alpha = 0.45,
            mapping = aes(x = lon, y = lat, fill = log10pop)) +
  scale_fill_gradientn(colors = viridis(1000)) +
  xlab('Longitude') + ylab('Latitude') +
  theme(legend.key.height = unit(1.0, 'cm')) +
  labs(color = '')
print(g)
dev.off()

# pdf(file = '../Figures/pop.pdf', height = 3.0, width = 4.5)
# basemap.hybrid +
#   geom_point(
#     data = df,
#     mapping = aes(x = lon, y = lat, color = log10pop),
#     size = 0.01,
#     alpha = 0.45
#   ) +
#   scale_color_gradientn(colors = viridis(1000)) +
#   xlab('Longitude') + ylab('Latitude') +
#   theme(legend.key.height = unit(1.0, 'cm')) +
#   labs(color = '')
# dev.off()



