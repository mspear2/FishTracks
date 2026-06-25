library(tidyverse)
library(suntools)
library(here)
library(mgcv)
StationIDs <- c(
  '05538010',
  '05538020',
  '412341088161001',
  '05541498',
  '411955088280601'
)


tracks <- map_dfr(StationIDs, download_fishtracks)
coords <-matrix(c(-88.10265102118505, 41.50349736382981),nrow = 1)
dates <- as.POSIXct(unique(as.Date(tracks$TimeStamp)))
solar_noon <- suntools::solarnoon(
  crds = coords,
  dateTime = dates,
  POSIXct.out = TRUE
)  %>%
  mutate(time = with_tz(time, tzone = 'America/Chicago')) %>%
  pull(time)


tracks %>%
  filter(StationID == '05538010') %>%
  ggplot(aes(x = TimeStamp, y = VRUniqTagCount)) +
  geom_line(alpha = .7, linewidth = .1, color = 'royalblue') +
  geom_vline(
    lwd = 1,
    color = 'yellow2',
    alpha = .6,
    xintercept = solar_noon,
  ) +
  geom_vline(
    lwd = 1,
    color = 'midnightblue',
    alpha = .6,
    xintercept = c(
      paste0(tracks$TimeStamp %>% as.Date %>% unique,' 24:00:00') %>% ymd_hms() %>% force_tz('America/Chicago')
    )
  ) +
  #facet_wrap(~StationID, scales = 'free_y') +
  theme_bw() +
  scale_y_continuous(limits = c(0,NA), expand = c(0,0))


modeling_data <- tracks %>%
  mutate(
    tod_numeric = (as.numeric(hms::as_hms(TimeStamp)) / 3600),
    date_numeric = ((as.numeric(format(TimeStamp, "%j"))))
  )

saveRDS(modeling_data,here('output', 'modeling_data.rds'))


model <- gam(data = modeling_data, family = poisson(),
             VRUniqTagCount ~ s(tod_numeric, bs = 'cc', k = 8) + s(date_numeric, k = 4))
saveRDS(model,here('output', 'model'))

gam.check(model)

plot(model, rug = TRUE, shade = TRUE, shift = coef(model)[1])
marginaleffects::plot_predictions(model, condition = 'tod_numeric') +
  scale_y_continuous(limits = c(0,NA), expand = c(0,NA)) +
  scale_x_continuous(limits = c(0,24), expand = c(0,0)) +
  labs(
    x = 'Hour of day',
    y = 'Predicted # of unique tag detections',
    title = 'Diel pattern in invasive carp FishTracks data above Brandon Road L&D'
  )
ggsave(here('output', 'partial diel effect.png'))
marginaleffects::plot_predictions(model, condition = 'date_numeric')
ggsave(here('output', 'day effect.png'))
