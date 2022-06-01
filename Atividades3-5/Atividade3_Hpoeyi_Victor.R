########## ATIVIDADE 3 ##########
##Victor Lupinacci - PPGE/UFRJ##


library(rgbif)
library(tidyverse)
library(dplyr)

###Baixando ocorrências GBIF

##Espécie escolhida: Halichoeres poeyi

hp_gbif <- occ_data(scientificName = "Halichoeres poeyi", 
                    hasCoordinate = TRUE,
                    hasGeospatialIssue=FALSE)

##Checando número de linhas e colunas

dim(hp_gbif$data) 


##Checando nome das colunas 

hp_gbif$data %>% names

##Chegando erros

issues_gbif <- hp_gbif$data$issues %>%
  unique()
gbif_issues() %>%
  data.frame() %>% 
  filter(code %in% issues_gbif) 

##Selecionando variáveis de interesse

hp_gbif1 <- hp_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()
lapply(hp_gbif1, unique)

hp_gbif_ok <- hp_gbif1

##Pacotes para criação do mapa

install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")

library(ggmap)
library(maps)
library(mapdata)


world <- map_data('world')


##Obervando os pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = hp_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Halichoeres poeyi")))

###Baixando ocorrências OBIS

##Pacotes necessários

library(robis)
library(dplyr)

hp_obis <- occurrence("Halichoeres poeyi")
names(hp_obis)

##Selecionando variáveis de interesse

hp_obis1 <- hp_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

##Checando problemas reportados (flags)

hp_obis1 %>% 
  distinct(flags)

hp_obis1 %>% 
  filter(!flags %in% c("no_depth", "NA", "on_land","depth_exceeds_bath", "depth_exceeds_bath,on_land", "no_depth,on_land", "on_land,depth_exceeds_bath"),
         !is.na(datasetName) ) %>% 
  lapply(., unique)

hp_obis_ok <- hp_obis1 %>% 
  dplyr::filter(!flags %in% c("no_depth", "NA", "on_land","depth_exceeds_bath", "depth_exceeds_bath,on_land", "no_depth,on_land", "on_land,depth_exceeds_bath"),
                !is.na(datasetName) )

##Criando o mapa

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = hp_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Halichoeres poeyi")))

##Unindo GBIF e OBIS

all_data <- bind_rows(hp_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      hp_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Halichoeres poeyi") %>% 
  dplyr::select(-rn)

##Criando mapa com todas as ocorrências

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Halichoeres poeyi")))

##Exportando
write.csv(all_data, "occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)
