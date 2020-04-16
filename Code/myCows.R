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

info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)



# Data: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
state.shp <- readOGR('../data/shapefile/cb_2018_us_state_500k/cb_2018_us_state_500k.shp')
states <- state.shp$STATEFP[state.shp$NAME %in% c('Texas')]

cnty.shp <- readOGR('../data/shapefile/cb_2018_us_county_500k/cb_2018_us_county_500k.shp')
shp.data <- cnty.shp[cnty.shp$STATEFP %in% states, ]
df <- fortify(shp.data, region = 'NAME') %>%
  rename(County = id)

cows <- read_csv('../data/cows/Texas_Cow_Data.csv') %>%
  select(-1, -3, -4)
colnames(cows) <- c('County', 'District', 'Cattle')

df <- df %>% left_join(cows, by = 'County') %>% as_tibble %>%
  mutate(Cattle = Cattle/1000)

ggmap(get_map(location = c(-100, 31.25), zoom = 6, maptype = 'satellite')) +
  geom_polygon(data = df, mapping = aes(long, lat, group = County, fill = Cattle),
               alpha = 0.75) +
  theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_gradientn(colors = magma(100))

# pdf(file = '../Figures/TexasCattle.pdf', height = 6.31, width = 7.51)
pdf(file = '../Figures/TexasCattle.pdf', height = 5.64, width = 7.51)
ggplot(df, aes(long, lat, group = group, fill = Cattle)) +
  geom_polygon() +
  theme(panel.grid = element_blank()) +
  theme_minimal() + ggtitle('Cattle Population') +
  labs(subtitle = 'in thousands', fill = '') + xlab('') + ylab('') +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  scale_fill_gradientn(colors = cividis(100))
dev.off()

