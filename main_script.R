## Making a map 

#LIBRARIES ----
library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(RColorBrewer) #for some more colors
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot

#LOADING THE DATASETS ----

flood <- read.csv("Datasets/Geospatial_flooding.csv") #loading in the flood dataset
str(flood)

world <- map_data("world") #downloading coordinates for the world,we'll select countries by ourselves
head(world) 
str(world)

# DATA WRANGLING----

flood <- flood[-c(1), ] %>% 
  dplyr::select(Major.Country, Began, Ended, 
                Death, Affected.population, 
                Homeless, Damage..USD.if.no.units., Adjusted.Damage.to.2002)

colnames(flood) <- c("Country", "Start", "End", "Deaths", 
                     "Affected_population", "Homeless",
                       "Damage_usd", "Adjusted_damage_usd")

str(flood)

## Turning all variables into integers/numbers----
flood$Start <- as.integer(flood$Start)
flood$End <- as.integer(flood$End)
flood$Deaths <- as.integer(gsub(",", "", flood$Deaths))
flood$Affected_population <- as.integer(gsub(",", "", flood$Affected_population))
flood$Homeless <- as.integer(gsub(",", "", flood$Homeless))
flood$Damage_usd <- as.numeric(gsub(",", "", flood$Damage_usd))
flood$Adjusted_damage_usd <- as.numeric(gsub(",", "", flood$Adjusted_damage_usd))

str(flood)

## Filtering out from 1980s----

flood <- flood %>% 
  group_by(Country) %>%
  filter(Start >= 1980) %>%
  ungroup()


## Calculating the mean over the years----

flood <- flood %>% 
  mutate(mean = ()/4) %>% 
  



  
