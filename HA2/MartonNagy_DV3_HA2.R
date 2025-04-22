## DATA LOADING
library(tidytuesdayR)
raw_data <- tidytuesdayR::tt_load('2023-06-20')

library(data.table)
ufo_sightings <- data.table(raw_data$`ufo_sightings`)
places <- data.table(raw_data$`places`)

rm(raw_data)

## Having worked with this dataset before, I will now concentrate only on US sightings

ufo_sightings <- ufo_sightings[country_code == 'US']
places <- places[country_code == 'US']

## First, let's create my custom theme

theme_custom <- function() {
  theme(
    text = element_text(family = 'serif'),
    axis.text = element_text(color = 'darkgrey', size = 8),
    axis.title = element_text(color = 'darkgoldenrod4', size = 10, face = 'bold'),
    panel.background = element_rect(fill = 'beige'),
    panel.border = element_blank(),
    plot.title = element_text(color = 'darkgoldenrod4', size = 13, face = 'bold', hjust = 0.5),
    plot.subtitle = element_text(color = 'darkgoldenrod4', size = 11, hjust = 0.5),
    legend.position = 'top',
    legend.title = element_text(color = 'darkgoldenrod4', size = 10),
    legend.text = element_text(color = 'darkgoldenrod4', size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = 'bisque2', linewidth = 0.25),
    axis.ticks = element_blank()
  )
}

## Plot 1: How did the number of sightings per US states evolve since 2000?

p1_data <- ufo_sightings[year(reported_date_time) >= 2000, .N, by = .(year(reported_date_time), state)][order(year, -N)]
p1_data <- p1_data[, rank := order(-N), by = year][rank <= 25, ]

library(ggplot2)
library(gganimate)

p1 <- ggplot(p1_data, aes(N, reorder(factor(rank), -rank), fill = state, group = state)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = state), hjust = 1.2, vjust = 0.5, size = 5, color = 'white') +
  labs(title = 'US UFO sightings in {closest_state} per state (top 25)',
       subtitle = 'Total sigthings in this year: {ufo_sightings[year(reported_date_time) == closest_state, .N]}',
       x = 'Number of sightings',
       y = 'Rank') +
  theme_custom() +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank()) +
  scale_fill_discrete() +
  transition_states(year, transition_length = 3) +
  ease_aes('linear')

animate(p1, duration = 24, fps = 30)

## Plot 2: Scatterpie map visualization of day parts' share of total number of sightings per state

download.file('https://raw.githubusercontent.com/PublicaMundi/MappingAPI/refs/heads/master/data/geojson/us-states.json',
              'us.geojson', mode = 'wb')

library(sf)

us_map <- st_read('us.geojson')

library(maptiles)

map_tiles <- get_tiles(us_map, apikey = '7a6ecbf0-ba66-4558-9fe2-63e7662bdef4', zoom = 4,
                       provider = 'CartoDB.PositronNoLabels', crop = TRUE)

library(tidyterra)
library(scatterpie)

ufo_sightings[, after12 := as.character(day_part %in% c('night', 'nautical dusk', 'afternoon',
                                           'astronomical dusk', 'civil dusk'))]
ufo_sightings[after12 == 'TRUE', after12 := 'After_12AM']
ufo_sightings[after12 == 'FALSE', after12 := 'Before_12AM']

p2_data <- merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))[, .(.N, lat = mean(latitude), lon = mean(longitude)), by = .(state, after12)]
p2_data_wide <- dcast(p2_data, state + lat + lon ~ after12, value.var = "N")
p2_data_wide <- p2_data_wide[, .(lat = mean(lat), lon = mean(lon), After_12AM = sum(After_12AM, na.rm = TRUE), Before_12AM = sum(Before_12AM, na.rm = TRUE)), by = state]


ggplot() +
  geom_spatraster_rgb(data = map_tiles) +
  geom_sf(data = us_map, fill = NA, color = 'black') +
  geom_scatterpie(
    aes(x = lon, y = lat, group = state),
    data = p2_data_wide,
    cols = c("Before_12AM", "After_12AM"),
    pie_scale = 0.75
  ) +
  labs(title = 'Share of sightings before and after noon in each US state') +
  scale_fill_excel_new(theme ='Paper', name = 'Time of day') +
  theme_custom() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
  )

## Plot 3: Average duration of sightings in the US

p3_data <- merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))[, .(avg_duration_min = mean(duration_seconds) / 60), by = .(latitude, longitude)]

ggplot() +
  geom_spatraster_rgb(data = map_tiles) +
  geom_sf(data = us_map, fill = NA, color = 'black') +
  stat_summary_hex(
    data = p3_data,
    aes(x = longitude, y = latitude, z = avg_duration_min),
    fun = mean,
    bins = 25,
    alpha = 0.8
  ) +
  scale_fill_viridis_c(name = "Avg. duration (min)", option = "mako") +
  labs(title = 'Spatial distribution of average sighting duration in the US') +
  theme_custom() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
  )

## Plot 4: UFO sightings tracker
  
dark_map_tiles <- get_tiles(us_map, apikey = '7a6ecbf0-ba66-4558-9fe2-63e7662bdef4', zoom = 4,
                       provider = 'CartoDB.DarkMatterNoLabels', crop = TRUE)
p4_data <- merge(ufo_sightings, places, by = c('city', 'state', 'country_code'))[year(reported_date_time) == 2014, ]

p4 <- ggplot(p4_data) +
  geom_spatraster_rgb(data = dark_map_tiles) +
  geom_sf(data = us_map, fill = NA, color = 'grey') +
  geom_point(aes(longitude, latitude), color = 'yellow') +
  transition_states(as.Date(reported_date_time)) +
  labs(title = 'US UFO sightings on {closest_state}',
       subtitle = 'Total no. of sightings on this day {p4_data[as.Date(reported_date_time) == as.Date(closest_state), .N]}') +
  theme_custom() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
  )

  
animate(p4, duration = 30, fps = 15)

## Plot 5: Interactive boxplot of duration distribution by shape

library(ggiraph)

summary_stats <- ufo_sightings %>%
  group_by(shape) %>%
  summarise(
    median_duration = median(duration_seconds),
    mean_duration = mean(duration_seconds),
    min_duration = min(duration_seconds),
    max_duration = max(duration_seconds)
  )

ufo_sightings <- merge(ufo_sightings, summary_stats, by = "shape")

p5 <- ggplot(ufo_sightings, aes(x = factor(shape), y = log(duration_seconds / 60 + 1/60))) +
  geom_boxplot_interactive(aes(
    fill = shape,
    tooltip = paste(
      "Shape:", shape,
      "<br>Median:", round(median_duration/60, 1),
      "<br>Mean:", round(mean_duration/60, 1),
      "<br>Min:", round(min_duration/60, 1),
      "<br>Max:", round(max_duration/60, 1)
    ),
    data_id = shape
  )) +
  scale_fill_viridis_d(name = "UFO Shape") +
  labs(title = "US UFO sighting duration distribution by shape",
       subtitle = "Hover over the boxplots to see detailed summary metrics!",
       x = "Shape", y = "Duration in minutes (log)") +
  theme_custom() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90))

girafe(ggobj = p5)

## Plot 6: Explorative interactive map with Plotly

library(plotly)

p6_data <- merge(ufo_sightings, places, by = c('city', 'state', 'country_code')) %>%
  filter(year(reported_date_time) == 2023) %>%
  mutate(
    text = paste("City:", city,
                 "<br>Duration (min):", round(duration_seconds / 60, 2),
                 "<br>Shape:", shape,
                 "<br>Date & time:", reported_date_time
                 )
  )

p6 <- ggplot(p6_data) +
  geom_sf(data = us_map, fill = 'dodgerblue4', color = 'lightgrey', linewidth = 0.5) +
  geom_point(aes(longitude, latitude, text = text), color = 'yellow', size = 0.25) +
  labs(title = 'US UFO sightings in 2023<br>Hover over the points to reveal details!') +
  theme_custom() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
  )

ggplotly(p6, tooltip = 'text')

names(grDevices::windowsFonts())

