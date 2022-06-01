########## ATIVIDADE 2 ##########
##Victor Lupinacci - PPGE/UFRJ##
library(tidyverse)
library(vegan)
library(dplyr)

getwd()

###Carregando csv
iris <- read.csv("iris_mod.csv", header = T)
lapply(iris, unique)

###Verificando características dos dados e fazendo controle de qualidade
iris %>% 
  select(Species, Sepal.Length:Petal.Width) %>% 
  pivot_longer(cols = -Species, names_to = "variavel", values_to = "valores") %>% 
  ggplot(aes(y = valores, fill = Species)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_y') +
  theme_classic()

###Verificar observações que violam limites esperados
install.packages("validate")
library(validate)

rules <- validator(in_range(lat, min = -90, max = 90),
                   in_range(lat, min = -180, max = 180),
                   is.character(site),
                   is.numeric(date),
                   all_complete(iris))

out   <- confront(iris, rules)
summary(out)
plot(out)

###Verificando os nomes dos táxons
install.packages("taxize")
library(taxize)

species <- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(iris %>% 
              distinct(Species))

###Manipulando os dados

##Planilha Base

?iris
iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, uri)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = lon, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date,
                scientificName = Species,
                scientificNameID = uri) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")

##Planilhas do eMOF

#Create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 

#Create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct()

#Create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))

##Controle de qualidade

#Check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)

setdiff(eventCore$eventID, eMOF$eventID)

setdiff(occurrences$eventID, eMOF$eventID)

#Check NA values
eMOF %>%
  filter(is.na(eventID))

occurrences %>%
  filter(is.na(eventID))

##Escrevendo as matrizes como arquivos de texto
rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF"))) #remover o que não será publicado

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}

