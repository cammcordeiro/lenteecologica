# HEAT MAP

library(tidyverse)

# Reproducible data ----
obs <- data.frame(x = 35 + (rnorm(100)/5), y = -2.5 + (rnorm(100)/5),
                  count = rbinom(100, 10, 0.4))
obs <- rbind(obs, 
             data.frame(x = 34 + (rnorm(100)/5), y = -3 + (rnorm(100)/5),
                        count = rbinom(100, 10, 0.4))
)

# Plot ----
ggplot(obs, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette = 'OrRd', direction = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = 'none') +
  geom_point(cex = 0.5)


###
library(ggmap)

# load data
occ_NF <- read.csv("data/GBIF_occ_NF_03may2022_ameaca.csv", header = T) %>% 
  mutate(ameacada = ifelse(is.na(Categoria.de.AmeaÃ.a), "não", "sim")) %>% 
  filter(!is.na(decimalLatitude))

NORFLU <- rgdal::readOGR("data/shapefile/map.shp")
UCs_NF <- rgdal::readOGR("data/shapefile/UCs_NF.shp")


osm <- get_stamenmap(bbox = c(min(occ_NF$decimalLongitude), min(occ_NF$decimalLatitude), max(occ_NF$decimalLongitude), max(occ_NF$decimalLatitude)))
g <- ggmap(osm)
g +   
  stat_density_2d(data = occ_NF, aes(fill = ..density.., x = decimalLongitude, y = decimalLatitude), geom = "raster", contour = FALSE, , alpha = 0.5) + coord_cartesian() +
  #scale_fill_distiller(palette = 'BuPu', direction = 1) +
  theme(legend.position = 'none')


#####
library(leaflet.extras)
library(tidyverse)
occ_rj <- read.csv("~/git/lenteecologica/data/occ_rio-de-janeiro.csv")

occ_rj %>% 
  select(Longitude, Latitude) %>% 
  # distinct() %>% 
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stamen.Watercolor") %>%  # Stamen.TonerLite
  # addCircleMarkers(lng = ~Longitude, lat = ~Latitude, fillOpacity = 0.1,
  #                  stroke = F) %>% 
  addPolygons(data=NORFLU, weight=1, col = 'red') %>% 
  #setView( 178, -20, 5 ) %>%
  addHeatmap(
    lng = ~Longitude, lat = ~Latitude,
    blur = 90, max = 0.05, radius = 5,
    cellSize = 1
  )

## for more examples see
# browseURL(system.file("examples/heatmaps.R", package = "leaflet.extras"))
kml <- readr::read_file(
  system.file("examples/data/kml/crimes.kml.zip", package = "leaflet.extras")
)

leaflet() %>%
  setView(-77.0369, 38.9072, 12) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addKMLHeatmap(kml, radius = 7) %>%
  addKML(
    kml,
    markerType = "circleMarker",
    stroke = FALSE, fillColor = "black", fillOpacity = 1,
    markerOptions = markerOptions(radius = 1))

## for more examples see
# browseURL(system.file("examples/KML.R", package = "leaflet.extras"))



occ_NF %>% 
  mutate(ameacada = ifelse(is.na(Categoria.de.AmeaÃ.a), "não", "sim")) %>% 
  group_by(NM_MUNICIP, ameacada) %>%
  summarise(n_distinct(scientificName), length(scientificName))

