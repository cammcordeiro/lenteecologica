
library(tidyverse)
library(leaflet)
library(vegan)
library(ggrepel)

## Check species names and classification
# https://www.gbif.org/tools/species-lookup 
taxon <- read.csv("~/git/lenteecologica/data/taxonomia.csv")

monit <- read.csv('data/monitoramento_UENF.csv') %>% 
  filter(!taxon_corrigido %in% c("", "#N/A"),
         local != "") %>% 
  select(data:coletor, taxon_corrigido) %>% 
  mutate(n_coletor = str_count(coletor, "/")+1,
         local = trimws(local) %>% 
           recode(., "Pontp 9" = "Ponto 9", "Ponto1" = "Ponto 1", "Ponto  10" = "Ponto 10"),
         ano_mes = as.Date(data) %>% zoo::as.yearmon(),
         mes = as.Date(data) %>% month() %>% as.factor())
  
monit %>% 
  select(taxon_corrigido) %>% 
  distinct() %>% 
  arrange(taxon_corrigido)


monit_taxon <- monit %>% 
  left_join(taxon %>% 
              select(canonicalName, kingdom:species) %>% 
              filter(canonicalName != "",
                     genus != "Formicidae") %>% 
              distinct() %>% 
              rename(taxon_corrigido = canonicalName), by = "taxon_corrigido") %>% 
  filter(!is.na(kingdom)) 


# taxons mais frequentes
monit_1 <- monit %>% 
  select(taxon_corrigido) %>% 
  arrange(taxon_corrigido) %>% 
  count(taxon_corrigido) %>% 
  arrange(-n) %>% 
  mutate(frel = (n/sum(n)*100)) %>% 
  filter(frel > 0.9) %>% 
  pull(taxon_corrigido)

# riqueza de taxons
riqueza <- monit %>% 
  filter(!is.na(data)) %>% 
  mutate(mes = as.Date(data) %>% month() %>% as.factor(),
         n_coletor = str_count(coletor, "/")+1) %>%
  mutate(mes = recode(mes, "1" = "janeiro", "2" = "fevereiro", "3" = "março", "4" = "abril", 
                      "5" = "maio", "6"= "junho", "7" = "julho", "8" = "agosto", "9" = "setembro", 
                      "10" = "outubro", "11" = "novembro", "12" = "dezembro")) %>% 
  filter(!is.na(mes)) %>% 
  group_by(mes, local, coletor) %>% 
  reframe(riqueza = n_distinct(taxon_corrigido)) %>% 
  mutate(n_coletor = str_count(coletor, "/")+1) %>% 
  mutate(riq_std = riqueza/n_coletor)


# similaridade
wide <- monit %>% 
  # mutate(taxon = ifelse(is.na(taxon_corrigido), nome_comum, taxon)) %>% 
  select(local, data, mes, taxon_corrigido) %>%
  filter(!is.na(mes)) %>%
  distinct() %>%
  mutate(ab = 1) %>%
  group_by(local, mes, taxon_corrigido) %>% 
  summarise(freq = sum(ab)) %>%
  select(local, mes, taxon_corrigido, freq) %>%
  pivot_wider(names_from = 'taxon_corrigido', values_from = 'freq') %>% 
  replace(is.na(.), 0)

wide_pto <- monit %>% 
  select(local, data, mes, taxon_corrigido) %>%
  filter(!is.na(mes)) %>%
  distinct() %>%
  mutate(ab = 1) %>%
  group_by(local, taxon_corrigido) %>% 
  summarise(freq = sum(ab)) %>%
  select(local, taxon_corrigido, freq) %>%
  pivot_wider(names_from = 'taxon_corrigido', values_from = 'freq') %>% 
  replace(is.na(.), 0)

wide_mes <- monit %>% 
  select(local, data, mes, taxon_corrigido) %>%
  filter(!is.na(mes)) %>%
  distinct() %>%
  mutate(ab = 1) %>%
  group_by(mes, taxon_corrigido) %>% 
  summarise(freq = sum(ab)) %>%
  select(mes, taxon_corrigido, freq) %>%
  pivot_wider(names_from = 'taxon_corrigido', values_from = 'freq') %>% 
  replace(is.na(.), 0)

wide_freq <- monit %>% 
  # mutate(taxon = ifelse(is.na(taxon_corrigido), nome_comum, taxon)) %>% 
  select(local, data, taxon_corrigido) %>%
  filter(taxon_corrigido %in% monit_1) %>%
  distinct() %>%
  mutate(ab = 1) %>%
  pivot_wider(names_from = 'taxon_corrigido', values_from = 'ab') %>% 
  replace(is.na(.), 0)

ordem <- monit %>% 
  select(local, data, taxon_corrigido) %>%
  filter(taxon_corrigido %in% monit_1) %>%
  distinct() %>%
  mutate(ab = 1)  %>% 
  mutate(mes = as.Date(data) %>% month() %>% as.factor()) %>% 
  group_by(taxon_corrigido) %>% 
  reframe(freq = sum(ab)/length(unique(wide_freq$data))) %>% 
  arrange(-freq) %>% 
  pull(taxon_corrigido)


########################################################
# geral
# riqueza filo
monit_taxon %>% 
  group_by(kingdom, phylum) %>%  
  reframe(riqueza = n_distinct(taxon_corrigido)) %>% 
  mutate(phylum = factor(phylum, levels = c("Tracheophyta", "Bryophyta",
                                            "Chordata", "Arthropoda", "Mollusca",
                                            "Ascomycota", "Basidiomycota",
                                            "Mycetozoa"))) %>%
  ggplot(aes(y = phylum, x = riqueza, fill = kingdom)) +
  geom_bar(stat = 'identity') +
  theme_classic() +
  labs(x = "Registros (n)", y = "") +
  theme(legend.title = element_blank())

# riqueza classe
monit_taxon %>% 
  group_by(kingdom, phylum, class) %>%  
  reframe(riqueza = n_distinct(taxon_corrigido)) %>% 
  filter(class != "") %>% 
  ggplot(aes(y = reorder(class, -riqueza), x = riqueza, fill = kingdom)) +
  geom_bar(stat = 'identity') +
  theme_classic() +
  labs(x = "Táxons (n)", y = "") +
  theme(legend.title = element_blank())


# riqueza Plantae
monit_taxon %>% 
  filter(kingdom == "Plantae") %>% 
  group_by(phylum, class, taxon_corrigido) %>%  
  reframe(freq = length(taxon_corrigido)) %>% 
  arrange(-freq) %>% 
  filter(freq > 50) %>%
  ggplot(aes(y = reorder(taxon_corrigido, freq), x = freq, fill = class)) +
  geom_bar(stat = 'identity') +
  theme_classic() +
  labs(x = "Registros (n)", y = "") +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(face = 'italic'))

monit_taxon %>% 
  filter(kingdom == "Plantae") %>% 
  group_by(phylum, class, taxon_corrigido, mes) %>%  
  reframe(freq = length(taxon_corrigido)) %>% 
  arrange(-freq) %>% 
  filter(freq > 10) %>%
  ggplot(aes(y = reorder(taxon_corrigido, freq), x = freq, fill = class)) +
    geom_bar(stat = 'identity') +
    theme_classic() +
    labs(x = "Registros (n)", y = "") +
    theme(legend.title = element_blank(),
          axis.text.y = element_text(face = 'italic')) +
    facet_grid(~mes)


# riqueza Animalia
monit_taxon %>% 
  filter(kingdom == "Animalia") %>% 
  group_by(phylum, class, taxon_corrigido) %>%  
  reframe(freq = length(taxon_corrigido)) %>% 
  arrange(-freq) %>% 
  filter(freq > 50) %>%
  ggplot(aes(y = reorder(taxon_corrigido, freq), x = freq, fill = class)) +
  geom_bar(stat = 'identity') +
  theme_classic() +
  labs(x = "Registros (n)", y = "") +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(face = 'italic'))


########################################################
# temporal
monit %>% 
  filter(!is.na(data)) %>% 
  mutate(mes = as.Date(data) %>% month() %>% as.factor()) %>% # taxon_corrigido = factor(taxon_corrigido, levels = ordem)
  mutate(mes = recode(mes, "1" = "janeiro", "2" = "fevereiro", "3" = "março", "4" = "abril", 
                      "5" = "maio", "6"= "junho", "7" = "julho", "8" = "agosto", "9" = "setembro", 
                      "10" = "outubro", "11" = "novembro", "12" = "dezembro")) %>% 
  filter(!is.na(mes)) %>% 
  group_by(mes, local, coletor) %>% 
  reframe(riqueza = n_distinct(taxon_corrigido)) %>% 
  ggplot(aes(x = mes, y = riqueza)) +
    geom_boxplot() + 
    geom_jitter(alpha = 0.4, width = 0.1) + 
    theme_classic() +
    labs(y = "Riqueza (S)", x = "") +
    theme(legend.position = "") 

riqueza %>% 
  # filter(!is.na(data)) %>% 
  # mutate(mes = as.Date(data) %>% month() %>% as.factor()) %>% # taxon_corrigido = factor(taxon_corrigido, levels = ordem)
  mutate(mes = recode(mes, "1" = "janeiro", "2" = "fevereiro", "3" = "março", "4" = "abril", 
                      "5" = "maio", "6"= "junho", "7" = "julho", "8" = "agosto", "9" = "setembro", 
                      "10" = "outubro", "11" = "novembro", "12" = "dezembro")) %>% 
  # filter(!is.na(mes)) %>% 
  # group_by(mes, local, coletor) %>% 
  # reframe(riqueza = n_distinct(taxon_corrigido)) %>% 
  ggplot(aes(x = mes, y = riq_std)) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.4, width = 0.1) + 
  theme_classic() +
  labs(y = "Riqueza (S)", x = "") +
  theme(legend.position = "") 

# teste
lm(riq_std ~ mes * local, riqueza) %>% anova()
aov(riq_std ~ mes * local, riqueza) %>% TukeyHSD(which = "mes") #%>% plot(las=1)
aov(riq_std ~ mes * local, riqueza) %>% TukeyHSD(which = "local") #%>% plot(las=1)


########################################################
# espacial
monit %>%
  group_by(data, local, coletor) %>% 
  reframe(riqueza = n_distinct(taxon_corrigido)) %>% 
  mutate(local = factor(local, levels = c("Ponto 1", "Ponto 2", "Ponto 3", "Ponto 4", "Ponto 5", "Ponto 6", 
                                          "Ponto 7", "Ponto 8", "Ponto 9", "Ponto 10"))) %>% 
  ggplot(aes(x = local, y = riqueza)) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.4, width = 0.1) + 
  theme_classic() +
  labs(y = "Riqueza (S)", x = "") +
  theme(legend.position = "") 


# geral
monit %>% 
  select(local, data, taxon_corrigido) %>%
  filter(taxon_corrigido %in% monit_1) %>%
  distinct() %>%
  mutate(ab = 1)  %>% 
  mutate(mes = as.Date(data) %>% month() %>% as.factor()) %>% 
  group_by(taxon_corrigido) %>% 
  reframe(freq = sum(ab)/length(unique(wide_freq$data))) %>% 
  arrange(-freq) %>% 
  ggplot(aes(x = reorder(taxon_corrigido, -freq), y = freq)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic")) +
    labs(x = "", y = "Frequência relativa (%)")

# mes  
wide_freq %>% 
  mutate(mes = as.Date(data) %>% month() %>% as.factor(),
         taxon_corrigido = factor(taxon_corrigido, levels = ordem)) %>%
  mutate(mes = recode(mes, "1" = "janeiro", "2" = "fevereiro", "3" = "março", "4" = "abril", 
                      "5" = "maio", "6"= "junho", "7" = "julho", "8" = "agosto", "9" = "setembro", 
                      "10" = "outubro", "11" = "novembro", "12" = "dezembro")) %>% 
  group_by(mes, taxon_corrigido) %>% 
  reframe(freq = sum(ab)/length(unique(wide_freq$data))) %>% 
  arrange(-freq) %>% 
  ggplot(aes(y = taxon_corrigido, x = freq)) +
    geom_bar(stat = "identity") +
    facet_grid(~mes) + 
    theme_classic() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, face = "italic"),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(y = "", x = "Frequência relativa (%)")

# local
wide_freq %>% 
  mutate(taxon_corrigido = factor(taxon_corrigido, levels = ordem),
         local = factor(local, levels = c("Ponto 1", "Ponto 2", "Ponto 3", "Ponto 4", "Ponto 5", 
                                          "Ponto 6", "Ponto 7", "Ponto 8", "Ponto 9", "Ponto 10"))) %>% 
  group_by(local, taxon_corrigido) %>% 
  reframe(freq = sum(ab)/length(unique(wide_freq$data))) %>% 
  arrange(-freq) %>% 
  ggplot(aes(y = taxon_corrigido, x = freq)) +
  geom_bar(stat = "identity") +
  facet_grid(~local) + 
  theme_classic() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, face = "italic"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "", x = "Frequência relativa (%)")





# data.spp <- wide_freq[ rowSums(wide_freq[3:ncol(wide_freq)])!=0, ]

# CURVAS PARA 10, 20 E 30 M
### rarefaction curves (curvas de rarefacao)
sac1 <- specaccum(wide[3:ncol(wide)])

plot(sac1, ci.type="polygon", ci.col="#41AB5D", 
     las=1) #  ylim=c(0,40), xlim=c(0,60), xaxt="n", yaxt="n",  ylab="", xlab=""
# par(new=T) 


#nMDS#
#criar tabela#
# wide_freq <- wide %>% select(monit_1) 
# dados1 = wide_freq[,3:ncol(wide_freq)]
# nmds <- metaMDS(decostand(dados1, method = "pa"), distance = "bray", k = 2, trymax = 50, autotransform=TRUE)
# 
# #plot(nmds)
# #nmds = vegan::metaMDS(dist)
# plot(nmds, type = "text")
# stressplot(nmds)
# 
# 
# # site scores
# MDS_xy <- data.frame(nmds$points)
# MDS_xy$local <- wide$local
# 
# site_scores <- MDS_xy %>%
#   group_by(local) %>%
#   dplyr::slice(chull(MDS1, MDS2)) %>%
#   data.frame() # %>%
# # filter(site == "Arraial do Cabo")
# 
# para.scores <- as.data.frame(scores(nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
# para.scores$species <- rownames(para.scores)  # create a column of species, from the rownames of species.scores
# 
# #
# site_scores %>%
#   # data.frame() %>%
#   ggplot(., aes(x=MDS1, y=MDS2)) +
#     geom_polygon(alpha = 0.3, aes(fill = local)) +
#     geom_point(data = MDS_xy, aes(x=MDS1, y=MDS2), color = "black", shape = 19, size = 1.2, alpha = 0.6) +
#     geom_point(data = para.scores, aes(x=NMDS1, y=NMDS2), color = "red", shape = 3) +
#     scale_color_gradient(low = "blue", high = "red") +
#     theme_classic() +
#     theme(legend.title = element_blank()) +
#     # facet_grid(~ mpa) +
#     geom_text_repel(data=para.scores, aes(x=NMDS1, y=NMDS2, label=species), alpha=0.7, segment.color = 'grey50',
#                     segment.size = 0.5, box.padding = unit(0.9, 'lines'), show.legend = FALSE)# , size=4)
#     # scale_fill_manual(values = c("t10" = "#440154FF", "t20" = "#1F968BFF", "t30" = "#FDE725FF"))

#####################################################

# # GBIF x iNaturalist
# 
# dados <- read.delim("~/Downloads/0028192-241107131044228.csv") 
# 
# gbif <- dados %>% 
#   dplyr::select(kingdom:species, decimalLatitude, decimalLongitude, institutionCode, eventDate) %>% # "EBIRD_BRA", "EBIRD" 
#   distinct() %>% 
#   mutate(institutionCode = ifelse(institutionCode == "iNaturalist", "iNaturalist", "GBIF"),
#          eventDate = as.Date(eventDate, format = "%Y-%m-%d")) %>% 
#   # filter(eventDate > "2022-01-01") %>% 
#   filter(!is.na(eventDate))
# 
# 
# #
# gbif %>% 
#   mutate(eventDate = year(eventDate)) %>% 
#   filter(eventDate > 2008, eventDate < 2024) %>% 
#   group_by(eventDate, institutionCode) %>% 
#   reframe(S = n_distinct(species)) %>% 
#   ggplot(aes(x = eventDate, y = S, color = institutionCode)) +
#     geom_line() +
#     theme_classic() +
#     # facet_grid(institutionCode ~.) +
#     labs(x = "", y = "Riqueza de táxons (n)") +
#     geom_smooth(method = "gam") +
#   theme(legend.title=element_blank(),
#         legend.position = 'bottom')
# 
# 
# ## juntando GBIF com monitoramento
# spp <- gbif %>% 
#   select(species, institutionCode, eventDate) %>% 
#   # filter(species %in% gbif_1) %>% 
#   distinct() %>% 
#   bind_rows(
#     monit %>% 
#       mutate(taxon = ifelse(is.na(taxon), nome_comum, taxon)) %>% 
#       select(taxon, DATA) %>% 
#       filter(taxon != "",
#              taxon %in% monit_1) %>% 
#       distinct() %>% 
#       rename(species = "taxon",
#              eventDate = "DATA") %>% 
#       mutate(institutionCode = "UENF",
#              eventDate = as.Date(eventDate, format = "%Y-%m-%d"))) %>% #filter(eventDate > "2021-01-01")
#   mutate(eventDate = year(eventDate))
# 
# #
# 
# # similaridade
# wide_all <- spp %>% 
#   filter(species != "",
#          eventDate > 2008,
#          eventDate < 2024) %>% #eventDate > 2021)
#   distinct() %>% 
#   mutate(ab = 1) %>% 
#   pivot_wider(names_from = 'species', values_from = 'ab') %>% 
#   replace(is.na(.), 0)
# 
# spp_all <- wide_all[ rowSums(wide_all[3:ncol(wide_all)])!=0, ]
# 
# dados_all = spp_all[, 3:ncol(spp_all)]
# nmds <- metaMDS(decostand(dados_all, method = "pa"), distance = "jaccard", k = 2, trymax = 100, autotransform=TRUE)
# 
# nmds <- metaMDS(decostand(dados_all, method = "pa"), distfun = betadiver, distance = "z", k = 2, trymax = 100, autotransform=TRUE)
# 
# #plot(nmds)
# #nmds = vegan::metaMDS(dist)
# plot(nmds, type = "text")
# 
# stressplot(nmds)
# nmds$stress
# 
# # site scores
# MDS_xy <- data.frame(nmds$points)
# MDS_xy$institutionCode <- wide_all$institutionCode
# MDS_xy$eventDate <- wide_all$eventDate
# 
# site_scores <- MDS_xy %>%
#   group_by(institutionCode, eventDate) %>%
#   dplyr::slice(chull(MDS1, MDS2)) %>%
#   data.frame() # %>%
# # filter(site == "Arraial do Cabo")
# 
# para.scores <- as.data.frame(scores(nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
# para.scores$species <- rownames(para.scores)  # create a column of species, from the rownames of species.scores
# 
# # 
# site_scores %>% 
#   # data.frame() %>%
#   ggplot(., aes(x=MDS1, y=MDS2)) +
#     geom_polygon(alpha = 0.3, aes(fill = institutionCode)) +
#     geom_point(data = MDS_xy, aes(x=MDS1, y=MDS2, colour = institutionCode), shape = 19, size = 5, alpha = 0.6) +
#     geom_point(data = para.scores, aes(x=NMDS1, y=NMDS2), color = "red", shape = 3) +
#     theme_classic() +
#     theme(legend.title = element_blank()) +
#     # facet_grid(~ mpa) +
#     geom_text_repel(data=site_scores, aes(x=MDS1, y=MDS2, label=institutionCode), alpha=0.7, segment.color = 'grey50',
#                     segment.size = 0.5, box.padding = unit(0.9, 'lines'), show.legend = FALSE)+# , size=4)
#     scale_color_manual(values = c("GBIF" = "#440154FF", "iNaturalist" = "#1F968BFF", "UENF" = "#FDE725FF"))


# https://rfunctions.blogspot.com/2016/08/measuring-and-comparing-beta-diversity.html

# presabs <- ifelse(wide[,3:ncol(wide)] > 0,1,0)
# # presabs <- ifelse(wide_mes_f[,3:ncol(wide_mes_f)] > 0,1,0)
# 
# dist <- betapart::beta.pair(presabs[, 3:ncol(presabs)], index.family="jaccard")
# # groups <- factor(c("GBIF", "GBIF", "UENF", "iNaturalist", "iNaturalist", "iNaturalist"))
# bd <- betadisper(dist[[3]], wide$mes)
# plot(bd)
# boxplot(bd, las = 2)
# anova(bd)
# 
# # Calculate pairwise dissimilarities
# pairwise_dissim <- betapart::beta.pair(presabs, index.family = "sor")
# 
# # Extract turnover and nestedness components
# turnover <- pairwise_dissim$beta.sim
# nestedness <- pairwise_dissim$beta.sne
# 
# # Load the package
# library(betapart)
# 
# # Calculate pairwise dissimilarities
# pairwise_dissim <- beta.pair(presabs, index.family = "sor")
# 
# # Extract turnover and nestedness components
# turnover <- pairwise_dissim$beta.sim
# nestedness <- pairwise_dissim$beta.sne
# 
# nesttry <- nestedtemp(wide_freq[,3:ncol(wide_freq)])
# nestedbetajac(wide_freq[,3:ncol(wide_freq)])
# nestedbetasor(wide_freq[,3:ncol(wide_freq)])
# 
# d3 <- designdist(vegdist(wide_freq[,3:ncol(wide_freq)], 'jaccard', binary = T), "gamma/alpha - 1", alphagamma = TRUE)
# dis <- designdist(vegdist(wide_freq[,3:ncol(wide_freq)], 'jaccard', binary = T), "(log(A+B-J)-log(A+B)+log(2))/log(2)")
# ordiplot(cmdscale(d3), las = 1, type = 'text')
# summary(dis)

###
# Clusterização k-means
# set.seed(123)
# 
# wide_mes_f <- wide_mes %>% select(monit_1) 
# wide_pto_f <- wide_pto %>% select(monit_1)
# 
# km.res <- kmeans(ifelse(wide_mes_f[,2:ncol(wide_mes_f)] > 0,1,0), 4, nstart=25)
# km.res <- kmeans(ifelse(wide_pto_f[,2:ncol(wide_pto_f)] > 0,1,0), 4, nstart=25)
# print(km.res)
# 
# aggregate(wide_mes, by=list(cluster = km.res$cluster), mean)
# 
# mtcars2 <- cbind(cluster = km.res$cluster, wide_freq[,3:ncol(wide_freq)])
# cbind(cluster = km.res$cluster, wide_freq[,1:2]) %>% 
#   arrange(cluster) %>% 
#   select(-local) %>% distinct() %>% 
#   data.frame()
#   
# head(mtcars2)
# 
# library(factoextra)
# 
# fviz_cluster(km.res, data=mtcars2,
#              palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
#              ellipse.type="euclid",
#              star.plot=TRUE,
#              repel=TRUE,
#              ggtheme=theme_classic()
# )


## temporal
wide_mes_f <- wide_mes_f %>% data.frame()
rownames(wide_mes_f) <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
df <- scale(wide_mes_f[, 2:ncol(wide_mes_f)])
dista <- dist(df, method="euclidean")
dista.hc <- hclust(d = dista, method="ward.D")
fviz_dend(dista.hc, cex=0.8, main = "", ylab = 'distância de dissimilaridade')


## espacial
wide_pto_f <- wide_pto_f %>% data.frame()
rownames(wide_pto_f) <- wide_pto_f$local
df1 <- scale(wide_pto_f[, 2:ncol(wide_pto_f)])
dista1 <- dist(df1, method="euclidean")
dista.hc1 <- hclust(d = dista1, method="ward.D")
fviz_dend(dista.hc1, cex=0.8, main = "", ylab = 'distância de dissimilaridade')


