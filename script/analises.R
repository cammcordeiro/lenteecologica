## https://learning.nceas.ucsb.edu/2019-11-RRCourse/spatial-vector-analysis-using-sf.html

library(tidyverse)

# download via SibBr spatial portal using Norte Fluminense shapefile
gbif_RJ <- read.csv("data/records-2023-12-19.csv") 

norte_RJ <- gbif_RJ %>%
  dplyr::select(decimalLongitude, decimalLatitude,
                eventDate, decimalLatitude, decimalLongitude, kingdom:genus, species) %>% 
  # norte_RJ <- read.delim("~/Downloads/0263225-210914110416597.csv") %>% 
  mutate(decimalLongitude = as.numeric(decimalLongitude), 
         decimalLatitude = as.numeric(decimalLatitude),
         eventDate = as.POSIXct(eventDate, format = "%Y-%m-%d")) %>% 
  filter(!is.na(decimalLongitude)) %>% 
  distinct()

norte_RJ %>% distinct(eventDate)

## Read in shapefile using sf
library(sf)

NORFLU <- read_sf("data/shapefile/map.shp") %>% 
  filter(NM_MUNICIP != "BOM JESUS DO ITABAPOANA") 
  

plot(NORFLU)  

# ak_regions_3338 <- ak_regions %>%
#   st_transform(crs = 3338)
# 
# st_crs(ak_regions_3338)
# plot(ak_regions_3338)  


NORFLU %>%
  select(NM_MUNICIP) %>% 
  plot()


#
str(norte_RJ)

occ <- st_as_sf(norte_RJ, 
                     coords = c('decimalLongitude', 'decimalLatitude'),
                     crs = 4674,
                     remove = F)
head(occ)

# occ_joined <- st_join(occ, NORFLU, join = st_within)
occ_joined <- st_intersection(occ, NORFLU)

occ_joined %>% 
  select(NM_MUNICIP) %>% 
  plot()

pop_region <- occ_joined %>% 
  as.data.frame() %>% 
  group_by(NM_MUNICIP) %>% 
  summarise(especies = length(unique(species)),
            ocorrencias = length(species)) %>% 
  bind_cols(tibble(pop = c(483551, 13847, 12958, 21104, 246391, 22393, 38961, 45059, 36573)))

pop_regionOCC <- left_join(NORFLU, pop_region) 

# plot(pop_regionOCC["ocorrencias"])

pop_regionOCC %>% 
  ggplot() +
    geom_sf(aes(fill = especies)) +
    geom_point(data = occ_joined %>% filter(is.na(NM_MUNICIP)), aes(x = decimalLongitude,  y = decimalLatitude), color = "darkblue") +
    theme_bw() +
    labs(fill = "número de táxons") +
    # scale_fill_continuous(low = "khaki", high = "firebrick")
    scale_fill_continuous(low = "lightgrey", high = "firebrick")


############
# reg e spp / pop

pop_regionOCC %>% 
  mutate(sp_pop = especies / pop,
         occ_pop = ocorrencias / pop) %>% 
  select(NM_MUNICIP, sp_pop, occ_pop) %>% 
  arrange(-sp_pop)


# spp e  occ
occ_joined %>% 
  as.data.frame() %>% 
  group_by(NM_MUNICIP) %>% 
  summarise(especies = length(unique(species)),
            genus = length(unique(genus)),
            family = length(unique(family)),
            order = length(unique(order)),
            ocorrencias = length(species))

#### 
# acumulado occ por ano
occ_joined %>% 
  as.data.frame() %>%
  # filter(eventDate < "2022-05-01") %>% 
  mutate(ano = year(eventDate)) %>% 
  group_by(ano) %>% 
  summarise(ocorrencias = length(species)) %>% 
  mutate(acc = cumsum(ocorrencias)) %>% 
  ggplot(aes(x = ano, y = acc)) +
  geom_col() +
  theme_classic() +
  labs(y = "número acumulado de registros (n)", x = "") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000)) +
  scale_x_continuous(breaks = seq(1950, 2023, by = 15), limits = c(1950, 2023))


## spp e  occ / ultimo ano

# total 2022
occ_joined %>% 
  as.data.frame() %>%
  filter(eventDate > "2022-01-01",
         eventDate > "2023-01-01") %>% 
  summarise(ocorrencias = length(species),
            especies = length(unique(species))) 


### por ano ###
library(mgcv)

occ_joined %>% 
  as.data.frame() %>%
  filter(eventDate > "2019-01-01") %>% 
  mutate(ano = year(eventDate)) %>% 
  group_by(ano) %>% 
  summarise(ocorrencias = length(species)) %>% 
  mutate(pct_change = (ocorrencias/lag(ocorrencias) - 1) * 100) %>% 
  summarise(median(pct_change, na.rm = T),
            quantile(pct_change, na.rm = T, probs = c(0.25, 0.75)))


# # %>% 
#   # gam(ocorrencias ~ s(ano, bs = "cr"), data = .) %>% summary()
#   ggplot(aes(x = ano, y = ocorrencias)) +
#     geom_point() +
#     geom_smooth(method = "gam")


occ_joined %>% 
  as.data.frame() %>%
  filter(eventDate > "2013-01-01") %>% 
  mutate(ano = year(eventDate)) %>% 
  group_by(ano) %>% 
  summarise(ocorrencias = length(species),
            taxons = length(unique(species))) %>% 
  summarise(median(ocorrencias),
            sd(ocorrencias),
            median(taxons),
            sd(taxons))

# medias
occ_joined %>% 
  as.data.frame() %>%
  filter(eventDate > "2013-01-01") %>% 
  mutate(ano = year(eventDate)) %>% 
  group_by(ano) %>% 
  summarise(ocorrencias = length(species),
            táxons = length(unique(species))) %>% 
  ggplot(aes(x = ano, y = ocorrencias, fill = táxons)) +
    geom_col() +  scale_fill_continuous(low = "lightblue", high = "firebrick") +
    theme_classic() +
    labs(y = "número de registros (n)", x = "") +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 14)) +
    scale_x_continuous(breaks = seq(2013, 2023, by = 1))

#            
occ_joined %>% 
  as.data.frame() %>%
  group_by(NM_MUNICIP) %>% 
  summarise(especies = length(unique(species)),
            genus = length(unique(genus)),
            family = length(unique(family)),
            order = length(unique(order)),
            ocorrencias = length(species))

# reino
occ_joined %>%
  as.data.frame() %>%
  group_by(NM_MUNICIP, kingdom, phylum) %>%
  summarise(especies = length(unique(species)),
            ocorrencias = length(species)) %>%
  filter(kingdom != "",
         phylum != "") # %>%
  write.csv("taxon_reino-filo.csv", row.names = F)
  

# reino
occ_joined %>% 
  as.data.frame() %>% 
  group_by(NM_MUNICIP, kingdom, phylum) %>% 
  summarise(especies = length(unique(species)),
            ocorrencias = length(species)) %>% 
  filter(kingdom != "") %>%
  rename(Reino = kingdom) %>% 
  ggplot(aes(x = NM_MUNICIP, y = especies, fill = Reino)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "", y = "número de táxons (n)") +
  scale_fill_brewer(palette="Spectral")

# plantae
occ_joined %>% 
  as.data.frame() %>% 
  group_by(NM_MUNICIP, kingdom, phylum) %>% 
  summarise(especies = length(unique(species)),
            ocorrencias = length(species)) %>% 
  filter(kingdom == "Plantae",
         phylum != "") %>%
  rename(Filo = phylum) %>%
  ggplot(aes(x = NM_MUNICIP, y = especies, fill = Filo)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "", y = "número de táxons (n)") +
  scale_fill_brewer(palette="Spectral")

# animalia
occ_joined %>% 
  as.data.frame() %>% 
  group_by(NM_MUNICIP, kingdom, phylum) %>% 
  summarise(especies = length(unique(species)),
            ocorrencias = length(species)) %>% 
  filter(kingdom == "Animalia",
         phylum != "") %>%
  rename(Filo = phylum) %>%
  ggplot(aes(x = NM_MUNICIP, y = especies, fill = Filo)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "", y = "número de táxons (n)") +
  scale_fill_brewer(palette="Spectral")

##########################################################################

midia <- read.csv("data/seguidores1.csv", sep = ";") %>% 
  mutate(entrada = as.POSIXct(entrada, format = "%d/%m/%Y"),
         primeiro_post = as.POSIXct(primeiro_post, format = "%d/%m/%Y"),
         ultimo_post = as.POSIXct(ultimo_post, format = "%d/%m/%Y"),
         curso = as.POSIXct(curso, format = "%d/%m/%Y"),
         tempo_no_IN = as.numeric(tempo_no_IN),
         tempo_primeiro_post = as.numeric(tempo_primeiro_post)) %>% 
  mutate(vida_iN = difftime(ymd("2023-12-02"), primeiro_post, units = "days") %>% as.numeric,
         lapso = difftime(entrada, primeiro_post, units = "days") %>% abs() %>% as.numeric,
         inatividade = difftime(ultimo_post, primeiro_post, units = "days") %>% abs() %>% as.numeric,
         efetividade_pos = difftime(primeiro_post, curso, units = "days") %>% as.numeric,
         efetividade = difftime(ultimo_post, curso, units = "days") %>% as.numeric)

#
midia %>%
  filter(!is.na(efetividade_pos)) %>% 
  summary()


midia %>% 
  mutate(curso = ifelse(is.na(curso), "nao", "sim")) %>% 
  group_by(curso) %>% 
  summarise(registros = sum(registros, na.rm = T),
            especies = sum(especies, na.rm = T))


midia %>% 
  as.data.frame() %>%
  filter(!is.na(entrada),
         entrada > "2013-01-01") %>% 
  mutate(ano = year(entrada)) %>% 
  group_by(ano) %>% 
  summarise(ocorrencias = length(registros),
            taxons = length(unique(especies))) %>%
  ggplot(aes(x = ano, y = ocorrencias, fill = taxons)) +
    geom_col() +  scale_fill_continuous(low = "lightblue", high = "firebrick") +
    theme_classic() +
    labs(y = "número de registros (n)", x = "") +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 14)) +
    scale_x_continuous(breaks = seq(2013, 2023, by = 1))


midia %>% 
  as.data.frame() %>%
  filter(!is.na(entrada),
         entrada > "2013-01-01") %>% 
  mutate(ano = year(entrada)) %>% 
  group_by(ano) %>% 
  summarise(ocorrencias = length(registros),
            taxons = length(unique(especies))) %>%
  mutate(pct_change = (ocorrencias/lag(ocorrencias) - 1) * 100) %>% 
  select(ano, ocorrencias) %>%  rstatix::chisq_test()
  summarise(mean(pct_change, na.rm = T))

#### teste
midia %>% 
  filter(entrada > "2023-01-01") %>% 
  mutate(curso = ifelse(is.na(curso), "nao", "sim")) %>% 
  group_by(curso) %>% 
  summarise(registros = sum(registros, na.rm = T),
            usuarios = length(iNaturalist),
            especies = sum(especies, na.rm = T)) %>% 
  mutate(registros_pessoa = registros / usuarios) %>% 
  select(registros_pessoa) %>% 
  chisq.test()

midia %>% 
  filter(entrada > "2023-01-01") %>% 
  mutate(curso = ifelse(is.na(curso), "nao", "sim")) %>% 
  group_by(curso) %>% 
  summarise(registros = sum(registros, na.rm = T),
            usuarios = length(iNaturalist),
            especies = sum(especies, na.rm = T)) %>% 
  mutate(taxon_pessoa = especies / usuarios) %>%
  select(taxon_pessoa) %>% 
  chisq.test()

#####
# ver composicao de especies

midia %>% 
  filter(entrada < "2023-05-13") %>% 
  group_by(curso) %>% 
  summarise(registros = sum(registros, na.rm = T))

# spp e  occ / after Lente
occ_joined %>% 
  as.data.frame() %>%
  filter(eventDate > "2023-01-01") %>% 
  summarise(ocorrencias = length(species))

occ_joined %>% 
  as.data.frame() %>% 
  filter(eventDate > "2023-01-01") %>% 
  # group_by(NM_MUNICIP) %>% 
  summarise(especies = length(unique(species)),
            genus = length(unique(genus)),
            family = length(unique(family)),
            order = length(unique(order)),
            ocorrencias = length(species))
