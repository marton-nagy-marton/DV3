# install.packages can only install from CRAN
# to install a package from GitHub we have to use the remotes package
# we do not have to load the whole package, we can call only one function with package name and double colon
remotes::install_github('daroczig/students')

library(students)
?students
str(students)
summary(lm(math ~ shoe, students))

plot(students$shoe, students$math)
abline(lm(math ~ shoe, students), col = 'red')

# shoe: size of shoe
# math: math test results
# there is a common cause behind the pattern: age

# TODO: ggplot  on the above
library(ggplot2)
ggplot(students, aes(x = shoe, y = math)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'red') +
  theme_bw()

# TODO: general EDA

summary(students)

library(GGally)
ggpairs(students) + theme_bw()

library(gtExtras)
gt_plt_summary(students)

summary(lm(math ~ shoe + x, students))

.secret
# Be careful when installing from GitHub - there can be malicious code inside!

download.file("https://bit.ly/de-cities-distance", "cities.xls", mode = 'wb')
library(readxl)
cities <- read_excel("cities.xls")

cities <- cities[1:(nrow(cities)-3), ]
cities
cities <- cities[, -1]
cities

# how do we visualize 15 dimensions? -> multidimensional scaling

?cmdscale

mds <- cmdscale(as.dist(cities))
mds

plot(mds)
text(mds[,1], mds[,2], names(cities))
mds <- -mds
plot(mds)
text(mds[,1], mds[,2], names(cities))
mds[2, ] <- -mds[2, ]
plot(mds)
text(mds[,1], mds[,2], names(cities))

# redo plot in ggplot2
mds <- cmdscale(as.dist(cities))
mds <- as.data.frame(mds)
mds$city <- rownames(mds)

ggplot(mds, aes(V1, V2*-1)) +
  #geom_point() +
  geom_text(aes(label = city)) +
  theme_bw() +
  labs(x = 'x', y = 'y')

?eurodist
eurodist

euro_mds <- cmdscale(eurodist)
euro_mds <- as.data.frame(euro_mds)
euro_mds$city <- rownames(euro_mds)

ggplot(euro_mds, aes(V1, -V2)) +
  geom_text(aes(label = city)) +
  theme_void()

library(ggmap)
library(tidygeocoder)

?geocode

library(data.table)
euro_mds <- data.table(geocode(euro_mds, 'city'))

ggplot(euro_mds, aes(long, lat)) +
  geom_text(aes(label = city)) +
  theme_void()

world <- map_data('world')
str(world)
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), fill = 'white', color = 'black') +
  theme_void() +
  coord_fixed(1.3) +
  geom_point(data = euro_mds, aes(long, lat), color = 'orange')

world$a <- grepl("A",world$region)
world

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region, fill = a), color = 'black') +
  theme_void() +
  coord_fixed(1.3) +
  geom_point(data = euro_mds, aes(long, lat), color = 'orange')

library(countrycode)
# look up region continents
?countrycode

world$continent <- countrycode(world$region, origin = 'country.name', destination = 'continent')

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region, fill = continent)) +
  theme_void() +
  coord_fixed(1.3) +
  geom_point(data = euro_mds, aes(long, lat), color = 'black') +
  theme(legend.position = 'none')

world <- data.table(world)

register_stadiamaps('c8880324-95c7-4b2c-be2f-c7faf72b49db')

?get_stadiamap

map <- get_stadiamap(
  c(left = min(euro_mds$long) * 0.995, 
    right = max(euro_mds$long) * 1.005,
    bottom = min(euro_mds$lat) * 0.995,
    top = max(euro_mds$lat) * 1.005),
  zoom = 4,
  maptype = 'stamen_toner'
)

ggmap(map) +
  theme_void() +
  geom_point(data = euro_mds, aes(long, lat), color = 'orange')

download.file(
  'https://stacks.stanford.edu/object/rc343vz5889',
  'Austria_boundary.zip', mode = 'wb')
download.file(
  'https://stacks.stanford.edu/object/yv617vc9132',
  'Austria_divisions.zip', mode = 'wb')
unzip('Austria_boundary.zip')
unzip('Austria_divisions.zip')

library(sf)

st_layers('.')

adm0 <- st_read('.', 'AUT_adm0')
plot(adm0)
st_geometry(adm0)

ggplot() + geom_sf(data = adm0)

adm2 <- st_read('.', 'AUT_adm2')
ggplot() +
  geom_sf(data = adm0, color = 'black', size = 5) +
  geom_sf(data = adm2, size = 0.1, color = 'gray') + 
  theme_void()

cities <- fread('https://simplemaps.com/static/data/country-cities/at/at.csv')

ggplot() +
  geom_sf(data = adm2, linewidth = 0.1, color = 'gray', fill = NA) +
  geom_sf(data = adm0, color = 'black', linewidth = 1, fill = NA) +
  geom_point(data = cities, aes(x = lng, y = lat, size = population, color = capital)) +
  theme_void()

download.file('https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2021/simplified-95/bezirke_95_topo.json',
              'austria.geojson')
map <- st_read('austria.geojson')

library(leaflet)
?leaflet

leaflet(map)

# getting back to MDS
mtcars

mds <- data.frame(cmdscale(dist(scale(mtcars))))
mds$car <- rownames(mtcars)

ggplot(mds, aes(X1, -X2, label = car)) + geom_text() + theme_void()

library(ggrepel)

ggplot(mds, aes(X1, -X2, label = car)) +
  geom_text_repel(max.overlaps = 18) +
  geom_point() +
  theme_void()

?mtcars

# UC Berkeley dataset
?UCBAdmissions

berkely <- as.data.frame(UCBAdmissions)

ggplot(berkely, aes(x = interaction(Gender, Dept), y = Freq, fill = Admit)) +
  geom_col(position = "stack") +
  labs(x = "Department and Gender", y = "Frequency", title = "Admissions by Department and Gender") +
  theme_minimal()

ggplot(berkely, aes(x = Gender, y = Freq, fill = Admit)) +
  geom_col(position = "fill") +
  labs(x = "Department and Gender", y = "Frequency", title = "Admissions by Department and Gender") +
  facet_wrap(~Dept) +
  scale_fill_manual(values = c('Admitted' = 'darkgreen', 'Rejected' = 'brown2')) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_percent())

?iris

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  geom_smooth(aes(color = Species), method = 'lm', se = FALSE) +
  theme_minimal()

summary(anscombe)

library(tidyr)
library(dplyr)

# Convert the anscombe data frame into long format
anscombe_long <- anscombe %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "category"),
    names_pattern = "(.)(\\d)"
  )

# Show the result
anscombe_long

ggplot(anscombe_long, aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~category) +
  theme_minimal()

anscombe_viz = rbindlist(lapply(1:4,
                                function(i) data.frame(x = anscombe[, i],
                                                       y = anscombe[, i + 4],
                                                       dataset_id = i)))

ggplot(anscombe_viz, aes(x, y)) + 
  geom_point(color = 'orange', size = 5) + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~dataset_id) +
  theme_minimal()

library(datasauRus)

df <- copy(datasaurus_dozen_wide)

dt = rbindlist(lapply(seq(1,26,2), 
                      function(i) data.frame(x = df[, i, drop = TRUE],
                                             y = df[, i + 1, drop = TRUE],
                                             dataset_id = substr(colnames(df[, i]), 1, nchar(colnames(df[, i])) - 2))))

ggplot(dt, aes(x, y)) + 
  geom_point(color = 'orange') + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~dataset_id) +
  theme_minimal()
