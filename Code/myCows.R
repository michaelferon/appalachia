rm(list = ls())

library(lubridate)
library(ggplot2)
library(ggmap)
library(config)
library(sp)
library(rgdal)
library(readr)
library(viridis)
library(dplyr)
library(maptools)

info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)



# Data: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# read in shapefile for Texas
state.shp <- readOGR('../data/shapefile/cb_2018_us_state_500k/cb_2018_us_state_500k.shp')
states <- state.shp$STATEFP[state.shp$NAME %in% c('Texas')]

# read in shapefile for Texas counties
cnty.shp <- readOGR('../data/shapefile/cb_2018_us_county_500k/cb_2018_us_county_500k.shp')
shp.data <- cnty.shp[cnty.shp$STATEFP %in% states, ]
df <- fortify(shp.data, region = 'NAME') %>%
  rename(County = id)

# read in cow population numbers
cows <- read_csv('../data/cows/Texas_Cow_Data.csv') %>%
  select(-1, -3, -4)
colnames(cows) <- c('County', 'District', 'Cattle')

# join the data together
df <- df %>% left_join(cows, by = 'County') %>% as_tibble %>%
  mutate(Cattle = log10(Cattle))


# create the plots
t <- expression(paste('on a log'[10], ' scale', sep = ''))
# pdf(file = '../Figures/TexasCattle.pdf', height = 6.31, width = 7.51)
pdf(file = '../Figures/TexasCattle.pdf', height = 5.0, width = 6.66)
ggplot(df, aes(long, lat, group = group, fill = Cattle)) +
  geom_polygon() +
  theme(panel.grid = element_blank()) +
  theme_minimal() + ggtitle('Cattle Population') +
  labs(subtitle = t, fill = '') + xlab('') + ylab('') +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(legend.key.height = unit(1.5, 'cm')) +
  scale_fill_gradientn(colors = plasma(100))
dev.off()

