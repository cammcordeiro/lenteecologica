
library(tidyverse)

met <- bind_rows(
  read.csv2("~/Downloads/meteoro_mar2025-set2025.csv"),
  read.csv2("~/Downloads/meteoro_set2024-mar2025.csv")  
) %>% 
  mutate(Data = gsub("/", "-", Data) %>% 
           as.Date(., format = "%d-%m-%Y")) %>% 
  select(Data, Temp..Ins...C., Umi..Ins.....) %>% 
  rename("temperatura" = "Temp..Ins...C.",
         "umidade" = "Umi..Ins.....") %>% 
  group_by(Data) %>% 
  reframe(med_temp_dia = mean(temperatura),
          ampl_temp_dia = max(temperatura)-min(temperatura),
          med_umid_dia = mean(umidade)) 

datas <- monit %>% 
  rename("Data" = "data") %>% 
  filter(Data != "") %>%
  distinct(Data, ano_mes) %>% 
  mutate(Data = as.Date(Data)) 

# Calcular médias da semana anterior
resultados <- datas %>%
  rowwise() %>%
  mutate(
    temp_7dias_anteriores = mean(
      met$med_temp_dia[met$Data >= (Data - 7) & met$Data <= (Data - 1)],
      na.rm = TRUE
    ),
    temp_amp_7dias_anteriores = mean(
      met$ampl_temp_dia[met$Data >= (Data - 7) & met$Data <= (Data - 1)],
      na.rm = TRUE
    ),
    umid_7dias_anteriores = mean(
      met$med_umid_dia[met$Data >= (Data - 7) & met$Data <= (Data - 1)],
      na.rm = TRUE
    )
  ) %>%
  ungroup()






  