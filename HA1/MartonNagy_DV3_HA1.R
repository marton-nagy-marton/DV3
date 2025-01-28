library(jsonlite)
library(data.table)
regions <- data.table(fromJSON('https://keeper.sparecores.net/table/region'))
zones <- data.table(fromJSON('https://keeper.sparecores.net/table/zone'))

str(regions)

str(zones)

regions[, .N, by = country_id][order(-N)]

pander::pander(regions[, .N, by = country_id][order(-N)])

library(ggplot2)

ggplot(regions[!is.na(founding_year), .N, by = founding_year], aes(x = founding_year, y = N)) +
  geom_col() +
  theme_bw() +
  scale_x_continuous(breaks = seq(min(regions$founding_year, na.rm = TRUE),
                                  max(regions$founding_year, na.rm = TRUE), 1)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.75)) +
  labs(title = 'Number of datacenters by founding year',
       subtitle = 'Note that regions with unknown founding year were excluded from the plot.')
  

ggplot(regions[!is.na(founding_year), .N, by = founding_year], aes(x = founding_year, y = N)) +
  geom_col() +
  geom_vline(xintercept = mean(regions$founding_year, na.rm = TRUE), linewidth = 1.5, color = 'red') +
  theme_bw() +
  scale_x_continuous(breaks = seq(min(regions$founding_year, na.rm = TRUE),
                                  max(regions$founding_year, na.rm = TRUE), 1)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.75)) +
  labs(title = 'Number of datacenters by founding year',
       subtitle = 'Note that regions with unknown founding year were excluded from the plot.')

library(countrycode)

regions[, continent := countrycode(country_id, 'iso2c', 'continent')]

regions[continent == 'Europe', .N]

eu_regions <- regions[continent == 'Europe']

eu_regions_zones = merge(eu_regions, zones[, .(zones = .N), by = region_id], all.x = TRUE)

eu_regions_zones[zones == 3, .N]

eu_regions_zones[zones == 1, .N]

ggplot(eu_regions_zones, aes(x = vendor_id, fill = factor(zones))) +
  geom_bar() +
  theme_bw() +
  labs(y = 'Number of regions') +
  theme(axis.title.x = element_blank(),
        legend.position = 'top') +
  guides(fill=guide_legend(title='Number of availability zones in the region'))

download.file('https://raw.githubusercontent.com/leakyMirror/map-of-europe/refs/heads/master/GeoJSON/europe.geojson',
              'europe.geojson', mode = 'wb')

library(sf)

map <- st_read('europe.geojson')

ggplot() + geom_sf(data = map) + theme_void()

library(maptiles)

map_tiles <- get_tiles(map, apikey = 'c8880324-95c7-4b2c-be2f-c7faf72b49db', zoom = 4,
                       provider = 'Stadia.StamenTerrainBackground', crop = TRUE)

library(tidyterra)

ggplot() +
  geom_spatraster_rgb(data = map_tiles, , maxcell = 10e+05, interpolate = TRUE) +
  theme_void()

ggplot() +
  geom_spatraster_rgb(data = map_tiles, , maxcell = 10e+05, interpolate = TRUE) +
  geom_sf(data = map, fill = NA, color = 'black') +
  geom_point(data = eu_regions_zones, aes(x = lon, y = lat,
                                          color = green_energy,
                                          size = zones,
                                          shape = vendor_id
                                          ),
             alpha = 0.6) +
  scale_color_manual(values = c('TRUE' = 'darkgreen', 'FALSE' = 'brown')) +
  theme_void()
