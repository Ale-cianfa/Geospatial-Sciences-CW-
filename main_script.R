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
flood$Country <- as.factor(flood$Country)
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

unique(flood$Start)

## Countries I selected, South East Asia----
  #Indonesia, Thailand, Vietnam, Malaysia, Philippines, Cambodia
#TITLE: 35 years of flooding in South-East Asia, the economic and human cost 
  
SEA <- c("Indonesia","Thailand", "Southern Thailand", "Thailand -  Typhoons Ira and Lola",
         "Thailand - Typhoon Gay", "Thailand - Typhoon Harry", 
         "Thailand - Typhoons Gloria and Frankie", "Vietnam", "Vietnam - Cyclone Cecil", 
         "Vietnam - Tropical Storm Chip", "Vietnam - Typhoon Fritz", "Vietnam - Typhoon Linda",
         "Vietnam - Typhoons Angela and Zack", "Vietnam -Tropical Storm Elvis", 
         "Vietnam -Typhoon Dawn", "Malaysia", "Malaysia - Tropical Storm Greg", "Philippines",
         "Philippines - Storm Iliang", "Philippines - Tropical Depression Karing", 
         "Philippines - Tropical Storm Flo / Kadiang", "Philippines - Tropical Storm Rubing",
         "Philippines - Tropical Storm Sybil", "Philippines - Typhoon Angela", 
         "Philippines - Typhoon Angela, Tropical Storm Zach", "Philippines - Typhoon Garding",
         "Philippines - Typhoon Gordon", "Philippines - Typhoon Kent and Tropical Storm Nina", 
         "Philippines - Typhoon Mameng", "Philippines - Typhoon Maring, Tropical Storm Ted", 
         "Philippines - Typhoon Mike", "Philippines - Typhoon Sara (Openg)", 
         "Philippines - Typhoon Teresa (Katring)", "Philippines - Typhoon Uring",
         "Philippines - Typhoons Gloring (Gloria) and Huaning (Herb)", "Philippines -Typhoon Nell", 
         "Cambodia")

#NB: this one doesn't work "Philippines - Tropical Storm Flo / Kadiang"

flood_sea <- flood %>%
  filter(Country %in% SEA) %>%
  mutate(Country = str_replace(Country, "Southern Thailand", "Thailand")) %>% 
  mutate(Country = str_replace(Country, "Thailand -  Typhoons Ira and Lola", "Thailand")) %>% 
  mutate(Country = str_replace(Country, "Thailand - Typhoon Gay", "Thailand")) %>% 
  mutate(Country = str_replace(Country, "Thailand - Typhoon Harry", "Thailand")) %>% 
  mutate(Country = str_replace(Country, "Thailand - Typhoons Gloria and Frankie", "Thailand")) %>% 
  mutate(Country = str_replace(Country, "Vietnam - Cyclone Cecil", "Vietnam")) %>% 
  mutate(Country = str_replace(Country, "Vietnam - Tropical Storm Chip", "Vietnam")) %>% 
  mutate(Country = str_replace(Country, "Vietnam - Typhoon Fritz", "Vietnam")) %>% 
  mutate(Country = str_replace(Country, "Vietnam - Typhoon Linda", "Vietnam")) %>% 
  mutate(Country = str_replace(Country, "Vietnam - Typhoons Angela and Zack", "Vietnam")) %>% 
  mutate(Country = str_replace(Country, "Vietnam -Tropical Storm Elvis", "Vietnam")) %>% 
  mutate(Country = str_replace(Country, "Vietnam -Typhoon Dawn", "Vietnam")) %>% 
  mutate(Country = str_replace(Country, "Malaysia - Tropical Storm Greg", "Malaysia")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Storm Iliang", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Tropical Depression Karing", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Tropical Storm Rubing", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Tropical Storm Sybil", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Angela", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Angela, Tropical Storm Zach", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Garding", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Gordon", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Kent and Tropical Storm Nina", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Mameng", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Maring, Tropical Storm Ted", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Mike", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Teresa (Katring)", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoon Uring", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines - Typhoons Gloring (Gloria) and Huaning (Herb)", "Philippines")) %>% 
  mutate(Country = str_replace(Country, "Philippines -Typhoon Nell", "Philippines"))


## Calculating the mean over the years----

flood_sea2 <-flood_sea %>% 
  group_by(Country) %>%
  summarize(Mean_affected_pop = mean(Affected_population, na.rm=TRUE)) %>% 
  ungroup()

flood_sea3 <-flood_sea %>% 
  group_by(Country) %>% 
  summarize(Mean_damage = mean(Adjusted_damage_usd, na.rm=TRUE)) %>% 
  ungroup()

# Staring to map it----

  
  



  



  
