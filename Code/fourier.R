rm(list = ls())

# Make sure you're in the 'Code' directory.

load('../Data/data-full/dataFullMask.Rdata')
source('myFunctions.R')

library(config)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggmap)

# Need a Google API key for some of the stuff below.
info <- config::get(file = './config/config.yml')
ggmap::register_google(info$google_api_key)
rm(info)

df <- tibble(
  time = seq(as.Date('2018-05-01'), as.Date('2020-01-31'), by = 1),
  methane = as.numeric(NA)
)
vec <- tapply(
  X$methane_mixing_ratio_bias_corrected,
  my.yday(X$time_utc),
  mean
)

for (i in 1:length(vec)) {
  df$methane[my.yday(df$time) == names(vec)[i]] <- vec[i]
}
for (i in 1:nrow(df)) {
  if (is.na(df$methane[i])) {
    df$methane[i] <- mean(df$methane[(i-6):(i+6)], na.rm = TRUE)
  }
}

ggplot(df, aes(time, methane)) +
  geom_line(color = 'SteelBlue') +
  theme_bw() +
  ggtitle('Mean Daily Methane Mixing Ratio') +
  xlab('Time') + ylab('Methane Mixing Ratio')

mid <- 321
df <- df %>%
  mutate(
    fft = fft(methane),
    mod = Mod(fft),
    arg = Arg(fft),
    hz = seq(0, nrow(.) - 1, by = 1),
    group = cut(mod, breaks = quantile(mod, probs = c(0, 0.895, 1)),
                labels = c('Low', 'High'), include.lowest = TRUE),
    group = factor(mod > 1000, labels = c('Low', 'High'))
  )
plot(
  df$hz[2:(nrow(df) - 1)],
  df$mod[2:(nrow(df) - 1)],
  type = 'h',
  main = 'Spectral Analysis',
  xlab = expression(paste('Frequency (days'^-1, ')')),
  ylab = 'Magnitude, |z|'
)
pdf(file = '../figures/spectrum.pdf', height = 4.93, width = 10.52)
df %>% slice(2:mid) %>%
  ggplot(aes(x = hz, y = 0, xend = hz, yend = mod, color = group)) +
  scale_color_manual(values = c('SteelBlue', 'OrangeRed3')) +
  geom_segment() +
  ggtitle('Frequency Spectrum') +
  xlab(expression(paste('Frequency (days'^-1, ')'))) +
  ylab('Magnitude, |z|') +
  labs(color = 'Magnitude') +
  scale_x_continuous(breaks = seq(0, 320, by = 20)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
dev.off()
  
