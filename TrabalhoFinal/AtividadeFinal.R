---
title: "Atividade Final"
author: "Victor Lupinacci"
date: "01/06/2022"
output: html_document
---

# Informações sobre a atividade {.unnumbered}

A atividade objetiva relacionar os pontos de ocorrência coletados e filtrados da espécie Halichoeres poeyi (atividade 3) e relacionar com variáveis ambientais obtidas no Bio-ORACLE a partir de uma análise de componentes principais (PCA).

## Carregando pacotes necessários

```{r, echo = T, results = 'hide',warning=FALSE, message=FALSE}
library(biomod2)
library(raster)
library(rgdal)
library(usdm)
library(spThin)
library(dplyr)
library(maptools)
library(ggbiplot)
```

## Carregando camadas ambientais

#### Temperatura Media
```{r, echo = T, results = 'hide',warning=FALSE, message=FALSE}
temp_mean <- raster("./CamadasAmbientais/Present.Surface.Temperature.Mean.tif")
```
