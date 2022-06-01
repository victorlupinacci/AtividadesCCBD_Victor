########## ATIVIDADE 1 ##########
##Victor Lupinacci - PPGE/UFRJ##

getwd()
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

###Carregando csv da turma

victor <- read.csv("./atividade1_VICTORLUPINACCI.csv", header=T,  sep = ";")
natalia <- read.csv("./atividade1_NATALIAMELO.csv", header=T,  sep = ";")
marina <- read.csv("./atividade1_MARINAMEGA.csv", header=T,  sep = ";")
mariana <- read.csv("./atividade1_MARIANAFAITANIN.csv", header=T,  sep = ";")
luiza <- read.csv("./atividade1_LUIZAVIEIRA.csv", header=T,  sep = ";")
leticia <- read.csv("./atividade1_LETICIAEVANGELISTA.csv", header=T,  sep = ";")
anaclara <- read.csv("./atividade1_ANACLARA.csv", header=T,  sep = ",")


###Renomeando as colunas do meu data frame para seguir de base para os outros dataframes
head(victor)

victor <-  rename(victor, Sepal_length_cm = SepLength, Sepal_width_cm = SepWidth, Petal_length_cm = PetLength, Petal_width_cm = PetWidth, decimalLatitude = Latitude, decimalLongitude= Longitude)

#Unindo colunas de data
victor$Data <- apply(victor[, 7:9], 1, paste, collapse = "-")

#Alterando formato
victor$Data <- factor(victor$Data) %>% 
  as.Date(format = "%d-%m-%Y") %>% 
  format("%Y-%m-%d")

#Excluindo colunas "Dia" "Mes" e "Ano"
victor$Dia <- NULL
victor$Mes <- NULL
victor$Ano <- NULL

#Padronizando classe da coluna "Site"
class(victor$Site)
victor$Site <- as.numeric(victor$Site)

###Editando dataframe "natalia"


#Renomeando colunas em comum
head(natalia)

natalia  <-  rename (natalia, Especie = specie, AmostraArq = sample, AmostraFicha = id_ficha, decimalLatitude = latitude, decimalLongitude = longitude, Sepal_length_cm = sepal_lenght, Sepal_width_cm = sepal_lenght, Site = site, Petal_length_cm = petal_length, Petal_width_cm = petal_width, Data = date)

#Padronizando coluna "Data"
natalia$Data <- factor(natalia$Data) %>% 
  as.Date(format = "%Y_%m_%d") %>% 
  format("%Y-%m-%d")

#Removendo "site_" das células da coluna "Site"
natalia$Site <- gsub("site_", "", as.character(natalia$Site))

#Colocando gênero em letra maiúscula
natalia$Especie <- gsub("iris", "Iris", as.character(natalia$Especie))

#Adicionando casas decimais nas colunas "decimalLatitude" e "decimalLongitude"
natalia$decimalLatitude <- format(natalia$decimalLatitude, digits = 5, big.mark=".", big.interval=6)
natalia$decimalLongitude <- format(natalia$decimalLongitude, digits = 5, big.mark=".", big.interval=6)

#Padronizando classe das colunas
natalia$Site <- as.numeric(natalia$Site)
natalia$decimalLatitude <- as.numeric(natalia$decimalLatitude)
natalia$decimalLongitude <- as.numeric(natalia$decimalLongitude)

###Editando dataframe "marina"


#Renomeando colunas em comum
head(marina)

marina = rename (marina, decimalLatitude = latitude, decimalLongitude = longitude, Especie = specie, AmostraArq = sample_pdf, AmostraFicha = sample_card, Sepal_length_cm = sepal_lenght_cm, Sepal_width_cm = sepal_widht_cm, Petal_length_cm = petal_lenght_cm, Petal_width_cm = petal_width_cm, Site = site, Data=date)

#Padronizando coluna "Data"
marina$Data <- factor(marina$Data) %>% 
  as.Date(format = "%d/%m/%Y") %>% 
  format("%Y-%m-%d")

#Colocando "A" em letra maiúscula nas colunas "de "AmostraFicha" e "AmostraArq"
marina$AmostraFicha <- gsub("a", "A", as.character(marina$AmostraFicha))
marina$AmostraArq <- gsub("a", "A", as.character(marina$AmostraArq))

#Substituindo espaço por "_" na coluna "Especie"

marina$Especie <- gsub(" ", "_", as.character(marina$Especie))

#Consertando casas decimais nas colunas "decimalLatitude" e "decimalLongitude"
marina <- marina %>% mutate(decimalLatitude = gsub("\\.", "", decimalLatitude), 
                            decimalLongitude = gsub("\\.", "", decimalLongitude)) %>%  
  mutate(decimalLatitude = format(as.numeric(decimalLatitude) / 10^6, nsmall = 6), 
         decimalLongitude = format(as.numeric(decimalLongitude) / 10^6, nsmall = 6)) 

#Padronizando classe das colunas
marina$Site <- as.numeric(marina$Site)
marina$decimalLatitude <- as.numeric(marina$decimalLatitude)
marina$decimalLongitude <- as.numeric(marina$decimalLongitude)

###Editando dataframe "mariana"


#Renomeando colunas em comum
head(mariana)
mariana = rename (mariana, Especie = Espécie, AmostraArq = Nome.do.arquivo, AmostraFicha = Amostra, decimalLatitude = Latitude, decimalLongitude = Longitude, Sepal_length_cm = Sepal.length..cm., Sepal_width_cm = Sepal.width..cm., Petal_length_cm =  Petal.length..cm., Petal_width_cm =  Petal.width..cm., Site = Área, Mes = Mês)

#Unindo colunas de data
mariana$Data <- apply(mariana[, 3:5], 1, paste, collapse = "-")

#Apagando colunas "Família", "Gênero", "Ano", "Mes" e "Dia"
mariana$Famíia <- NULL
mariana$Gênero <- NULL
mariana$Dia <- NULL
mariana$Mes <- NULL
mariana$Ano <- NULL

#Removendo "Site" das células da coluna "Site"
mariana$Site <- gsub("Site", "", as.character(mariana$Site))

#Substituindo espaço por "_" na coluna "Especie"
mariana$Especie <- gsub(" ", "_", as.character(mariana$Especie))

#Padronizando classe das colunas
mariana$Site <- as.numeric(mariana$Site)
mariana$decimalLatitude <- as.numeric(mariana$decimalLatitude)
mariana$decimalLongitude <- as.numeric(mariana$decimalLongitude)

  
###Editando dataframe "luiza"


#Renomeando colunas em comum
head(luiza)
luiza = rename (luiza, decimalLatitude = Latitude, decimalLongitude = Longitude, AmostraArq = ï..Amostra, AmostraFicha = Amostra2)

#Padronizando coluna "Data"
luiza$Data <- factor(luiza$Data) %>% 
  as.Date(format = "%d/%m/%Y") %>% 
  format("%Y-%m-%d")

#Removendo "Site" das células da coluna "Site"
luiza$Site <- gsub("Site", "", as.character(luiza$Site))

#Padronizando classe das colunas
luiza$Site <- as.numeric(luiza$Site)
luiza$decimalLatitude <- as.numeric(luiza$decimalLatitude)
luiza$decimalLongitude <- as.numeric(luiza$decimalLongitude)


###Editando dataframe "leticia"


#Renomeando colunas em comum
head(leticia)
leticia = rename (leticia, Especie = Espécies, AmostraArq = Amostra.arq., AmostraFicha = Amostra, decimalLatitude = Latitude, decimalLongitude = Longitude, Sepal_length_cm = Sepal.length..cm., Sepal_width_cm = Sepal.width..cm., Petal_length_cm =  Petal.length..cm., Petal_width_cm =  Petal.width..cm.)

#Removendo "Site" das células da coluna "Site"
leticia$Site <- gsub("Site", "", as.character(leticia$Site))

#Substituindo espaço por "_" na coluna "Especie"
leticia$Especie <- gsub(" ", "_", as.character(leticia$Especie))

#Consertando casas decimais nas colunas "decimalLatitude" e "decimalLongitude"
leticia <- leticia %>% 
  mutate(decimalLatitude = gsub("\\.", "", decimalLatitude), 
         decimalLongitude = gsub("\\.", "", decimalLongitude)) %>%  
  mutate(decimalLatitude = format(as.numeric(decimalLatitude) / 10^6, nsmall = 6), 
         decimalLongitude = format(as.numeric(decimalLongitude) / 10^6, nsmall = 6)) 

#Padronizando classe das colunas
leticia$Site <- as.numeric(leticia$Site)
leticia$decimalLatitude <- as.numeric(leticia$decimalLatitude)
leticia$decimalLongitude <- as.numeric(leticia$decimalLongitude)


###Editando dataframe "anaclara"


#Renomeando colunas em comum
head(anaclara)
anaclara = rename (anaclara, Especie = espÃ.cie, AmostraArq = amostra_cod, AmostraFicha = amostra, decimalLatitude = latitude, decimalLongitude = longitude, Sepal_length_cm = sepal.lenght..cm., Sepal_width_cm = sepal.width..cm., Petal_length_cm =  petal.length..cm., Petal_width_cm =  petal.width..cm., Site = site, Data=data)

#Removendo "Site" das células da coluna "Site"
anaclara$Site <- gsub("Site", "", as.character(anaclara$Site))

#Substituindo espaço por "_" na coluna "Especie"
anaclara$Especie <- gsub(" ", "_", as.character(anaclara$Especie))

#Consertando casas decimais nas colunas "decimalLatitude" e "decimalLongitude"
anaclara <- anaclara %>% 
  mutate(decimalLatitude = gsub("\\.", "", decimalLatitude), 
         decimalLongitude = gsub("\\.", "", decimalLongitude)) %>%  
  mutate(decimalLatitude = format(as.numeric(decimalLatitude) / 10^6, nsmall = 6), 
         decimalLongitude = format(as.numeric(decimalLongitude) / 10^6, nsmall = 6)) 

#Padronizando classe das colunas
anaclara$Site <- as.numeric(anaclara$Site)
anaclara$decimalLatitude <- as.numeric(anaclara$decimalLatitude)
anaclara$decimalLongitude <- as.numeric(anaclara$decimalLongitude)


###Conferindo todos os dataframes


head(victor)
head(natalia)
head(marina)
head(mariana)
head(luiza)
head(leticia)
head(anaclara)

class(victor)
class(natalia)
class(marina)
class(mariana)
class(luiza)
class(leticia)
class(anaclara)


###Unindo os dataframes


turma <- bind_rows(victor, natalia, marina, mariana, luiza, leticia, anaclara)


###Salvando csv


write.csv(turma, "TurmaPadronizado.csv", row.names = FALSE)
