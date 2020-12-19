library(tidyverse)
library(vegan)

soil <- read.csv("Data/soil_nutrients.csv")
colnames(soil)








pairs(soil,
      lower.panel = NULL, 
      col = as.numeric(soil$Species))