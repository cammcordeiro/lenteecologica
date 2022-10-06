# Required packages
library(tidyverse)
library(rgdal)
library(sp)


# load data
# occ_NF <- read.csv("GBIF_occ_NF_03may2022.csv", header = T) %>% 
#   #mutate(municipio = rep(LETTERS[1:10], 924)) %>% 
#   select(scientificName, decimalLongitude, decimalLatitude)


norte_RJ <- read.delim("~/Downloads/0263225-210914110416597.csv") %>% 
  mutate(decimalLongitude = as.numeric(decimalLongitude), 
         decimalLatitude = as.numeric(decimalLatitude)) %>% 
  filter(!is.na(decimalLongitude))


occs <- norte_RJ %>%
  dplyr::select(occurrenceID, decimalLongitude, decimalLatitude)


# Shapefile import
NORFLU <- rgdal::readOGR("map.shp")
NORFLU_wsg84 <- spTransform(NORFLU, CRS("+proj=longlat +datum=WGS84"))

UCs <- rgdal::readOGR("ucstodas.shp")
proj4string(UCs) <- CRS("+proj=longlat +datum=WGS84")

# Filter UCs within cities
UCs_NF <- UCs %>% subset(NOME_UC1 %in% c(occ_NF %>% 
                                   select(NOME_UC1, NM_MUNICIP) %>% 
                                   filter(!is.na(NOME_UC1)) %>% 
                                   distinct(NOME_UC1) %>% pull()))

# UCs_NF@data <- left_join(UCs_NF@data, occ_NF %>% 
#                            select(NOME_UC1, NM_MUNICIP) %>% 
#                            filter(!is.na(NOME_UC1)) %>% 
#                            distinct())
# 
# UCs_NF$NM_MUNICIP <- occ_NF %>% 
#   select(NOME_UC1, NM_MUNICIP) %>% 
#   filter(!is.na(NOME_UC1)) %>%
#   distinct() %>% 
#   select(NM_MUNICIP)



writeOGR(UCs_NF, dsn = "/Users/cesarcordeiro/git/CCBD_atividade4", layer = "UCs_NF",
         driver = "ESRI Shapefile")




# Spatial data
firstPoints <- SpatialPoints(coords = cbind(occ$decimalLongitude, occs$decimalLatitude),
                             proj4string=CRS("+proj=longlat +datum=WGS84"))
# firstPoints <- SpatialPointsDataFrame(coords = cbind(occ_NF$decimalLongitude, occ_NF$decimalLatitude), data = occ_NF,
#                                proj4string = CRS("+proj=longlat +datum=WGS84"))


# # Occurrences within cities
cities <- sp::over(firstPoints, NORFLU_wsg84)
protc <- sp::over(firstPoints, UCs)

# merge
# bind_cols(protc, cities, norte_RJ) %>% 
#   filter(!is.na(NM_MUNICIP)) %>% 
#   write.csv("GBIF_occ_NF_03may2022.csv", row.names = FALSE)

occs <- bind_cols(protc, cities, occ_NF) %>%
  filter(!is.na(NM_MUNICIP))

occs %>% 
  group_by(NM_MUNICIP, CATEGORI3) %>% 
  summarise(occ = n_distinct(occurrenceID)) %>% data.frame()

occ_NF %>% 
  mutate(CATEGORI3 = plyr::mapvalues(CATEGORI3, from = c(NA, "\xc1rea de Prote\xe7\xe3o Ambiental",
                                                         "Esta\xe7\xe3o Ecol\xf3gica", "Ref\xfagio de Vida Silvestre",
                                                         "Reserva Biol\xf3gica", "Reserva Particular do Patrim\xf4nio Natural"), 
                                     to = c("Fora de UC", "Àrea de Proteção Ambiental",
                                     "Estação Ecológica", "Refúgio de Vida Silvestre",
                                     "Reserva Biológica", "Reserva Particular do Patrimônio Natural"))) %>% 
  mutate(CATEGORIA = ifelse(CATEGORI3 == "Fora de UC", "Fora de UC", "Dentro de UC")) %>% 
  ggplot(aes(y = NM_MUNICIP, fill = CATEGORI3)) +
    geom_bar(position = 'stack') +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "")


#############
# http://dados.mma.gov.br
# https://dados.gov.br
ameaca <- read.csv("lista-de-especies-ameacas-2020.csv", header = T, sep = ";")

intersect(
  norte_RJ %>% 
    distinct(species) %>% 
    filter(species != "") %>% 
    pull(),
  ameaca %>% 
    distinct(Espécie..Simplificado.) %>%
    filter(Espécie..Simplificado. != "") %>% 
    pull()
  )

tudo <- left_join(read.csv("GBIF_occ_NF_03may2022.csv", header = T), 
          ameaca %>% 
            select(Espécie..Simplificado., Categoria.de.Ameaça) %>% 
            distinct() %>%
            filter(Espécie..Simplificado. != ""), by = c("species" = "Espécie..Simplificado.")) # %>% 
  # write.csv("GBIF_occ_NF_03may2022_ameaca.csv", row.names = F)


tudo %>% 
  mutate(ameacada = ifelse(is.na(Categoria.de.Ameaça), "não", "sim")) %>% 
  group_by(NM_MUNICIP, ameacada) %>% 
  summarise(spp = n_distinct(species)) %>% 
  ggplot(aes(y = NM_MUNICIP, x = spp, fill = ameacada)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(y = "", x = "Espécies (n)")

tudo %>% 
  filter(!is.na(Categoria.de.Ameaça)) %>% 
  ggplot(aes(y = NM_MUNICIP, fill = Categoria.de.Ameaça)) +
    geom_bar(position = "fill") +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "Contribuição relativa", y = "")


tudo %>% 
  mutate(ameacada = ifelse(is.na(Categoria.de.Ameaça), "não", "sim")) %>% 
  filter(NM_MUNICIP == "CARAPEBUS") %>% 
  group_by(NM_MUNICIP, ameacada) %>% 
  summarise(spp = n_distinct(species)) %>% 
  mutate(relativa = spp/sum(spp)) %>% 
  ggplot(aes(y = ameacada, x = spp)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "")


tudo %>% 
  mutate(ameacada = ifelse(is.na(Categoria.de.Ameaça), "não", "sim")) %>% 
  filter(NM_MUNICIP == "CARAPEBUS") %>% 
  group_by(NM_MUNICIP, ameacada) %>% 
  summarise(spp = n_distinct(species)) %>% 
  ggplot(aes(x = ameacada, y = spp)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "x", start = -pi/2)
   

data.frame(x = 1, percent = 54.2) %>% 
  mutate(title = paste0(percent, "%")) %>% 
  ggplot(aes(xmin = 1, xmax = 1.5, ymin = 0, ymax = percent)) +
    geom_rect(aes(xmin = 1, xmax = 1.5, ymin = 0, ymax = 100), fill = "#ece8bd") +
    geom_rect(fill = "#47d22d") + 
    xlim(c(0, 1.5)) + 
    ylim(c(0,200)) +
    geom_text(aes(x = 0, y = 0, label = title), 
              colour = "black", size = 10) +
    coord_polar(theta = "y", start = -pi/2) + 
    theme_void() + 
    # theme(plot.margin = unit(rep(0, 4), "cm")) +
    labs(title = "Espécies não ameaçadas")

###############################

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = subset(NORFLU, NM_MUNICIP == "MACAÉ"),
              col = "",
              fillColor = "blue",
              highlight = highlightOptions(weight = 0.1,
                                           color = "darkblue",
                                           fillOpacity = 0.7,
                                           bringToFront = F)) %>% 
  addPolygons(data = subset(UCs_NF, NM_MUNICIP == "MACAÉ"),
              fillColor = "red") %>% 
  addCircleMarkers(data = occ_NF %>% 
                     dplyr::filter(!is.na(decimalLatitude)) %>% 
                     distinct(),
                   ~decimalLongitude, 
                   ~decimalLatitude,
                   radius = 5,
                   #label = ~as.character(datasetName),
                   stroke = FALSE, 
                   fillOpacity = 0.5) 

