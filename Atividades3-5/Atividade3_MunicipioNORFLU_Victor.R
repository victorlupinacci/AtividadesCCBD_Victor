########## ATIVIDADE 3 ##########
##Victor Lupinacci - PPGE/UFRJ##

library(sp)

library(rgdal)

library(dplyr)


getwd()

##Carregando CSV de ocorrencias em Sao Francisco de Itabapoana (Fonte: SiBBr)

SFI <- read.csv("./occ_sfi.csv", header=T,  sep = ",")

head(SFI)


##Selecionando as colunas

SFI <-  dplyr::select (SFI, Species, Latitude, Longitude)

dim(SFI)

##Tirando registos com NA
SFI_na <- SFI %>% tidyr::drop_na(Species, Longitude, Latitude)

## Limpando registros com outros problemas referente as coordenadas 
## Marcando os registros potencialmente problematicos 

flags_spatial_SFI <- CoordinateCleaner::clean_coordinates (
  x = SFI_na,
  species = "Species",
  lon = "Longitude", 
  lat = "Latitude",
  tests = c("duplicates", # duplicatas
            "equal", # coordenadas iguais
            "gbif", # raio ao redor da sede da GBIF
            "seas", # pontos no mar
            "validity", # ponto de fora do sistema de coordenadas
            "zeros" # zeros e pontos onde lat = lon
  )
)


head(flags_spatial_SFI)
summary(flags_spatial_SFI)
#Flagged 111 of 272 records 
#.val (0)     .equ (0)    .zer  (0)   .sea  (93)   .gbf  (0)   .dpl (86) 


## Excluindo os pontos considerados problematicos 
SFI_f <- SFI_na %>% 
  dplyr::filter(flags_spatial_SFI$.summary == TRUE)
nrow(SFI_f)
colnames(SFI_f)
#161 registros de ocorrência

##Transformando em dados espaciais para plotar
coordinates(SFI_f) <- ~ Longitude+Latitude
#Shp Norte Fluminense RJ
NORFLU <- readOGR("./NORFLU/map.shp")

plot(SFI_f)
plot(NORFLU, add=TRUE)


##Baixando dados marinhos (Fonte:OBIS)

install.packages("robis")
library("robis")
library("dplyr")

##Baixando ocorrência no OBIS para polígono de área marinha de São Francisco de Itabapoana (exportado do MyMaps)
occMar <- occurrence(geometry = "POLYGON ((-41.024442 -21.6196172, -40.1117911 -21.5980284, -40.2947355 -21.3081399, -40.9607816 -21.3030221, -40.9649015 -21.3612259, -40.9944273 -21.3944748, -41.0204787 -21.435002, -41.0630507 -21.475902, -41.074037 -21.5180681, -41.0601769 -21.5648622, -41.024442 -21.6196172))
")

head(occMar)


##Selecionando colunas de interesse

occMarFilt <- dplyr::select (occMar, species, decimalLatitude, decimalLongitude)



##Tirando registos com NA
occMarFilt_na <- occMarFilt %>% tidyr::drop_na(species, decimalLatitude, decimalLongitude)

## Limpando registros com outros problemas referente as coordenadas 
## Marcando os registros potencialmente problematicos 

flags_spatial_occMarFilt <- CoordinateCleaner::clean_coordinates (
  x = occMarFilt_na,
  species = "species",
  lon = "decimalLongitude", 
  lat = "decimalLatitude",
  tests = c("duplicates", # duplicatas
            "equal", # coordenadas iguais
            "gbif", # raio ao redor da sede da GBIF
            "validity", # ponto de fora do sistema de coordenadas
            "zeros" # zeros e pontos onde lat = lon
  )
)


head(flags_spatial_occMarFilt)
summary(flags_spatial_occMarFilt)
#Flagged 62 of 670 records 
#.val (0)     .equ (0)    .zer  (0)   .gbf  (0)   .dpl (62) 


## Excluindo os pontos considerados problematicos 
occMarFilt_f <- occMarFilt_na %>% 
  dplyr::filter(flags_spatial_occMarFilt$.summary == TRUE)

nrow(occMarFilt_f)
colnames(occMarFilt_f)
#608 registros de ocorrência

##Transformando em dados espaciais para plotar
coordinates(occMarFilt_f) <- ~ decimalLongitude+decimalLatitude

plot(occMarFilt_f)
plot(NORFLU, add=TRUE)
plot(SFI_f, add=TRUE)


##Juntando ocorrências terrestres e marinhas
#Total de ocorrências: 161 + 608 = 769

occTotalSFI <- bind_rows(SFI_f, occMarFilt_f)
write.csv(occTotalSFI, "OccSaoFrancItab.csv", row.names = FALSE)
