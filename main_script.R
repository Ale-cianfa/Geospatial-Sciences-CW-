## Making a map 

#LIBRARIES ----
library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(RColorBrewer) #for some more colors
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
library("wesanderson")


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
flood$Country <- as.character(flood$Country)
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
  
SEA1 <- c("Indonesia","Thailand", "Southern Thailand", "Thailand -  Typhoons Ira and Lola",
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
  filter(Country %in% SEA1) %>%
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
flood_sea <- flood_sea %>%
  mutate(Country = str_replace(Country, "Philippines - Typhoons Gloring (Gloria) and Huaning (Herb)", "Philippines"))
  #this doesn't work and idk why 

sea_pop <-flood_sea %>% 
  group_by(Country) %>%
  summarize(Mean_affected_pop = mean(Affected_population, na.rm=TRUE)) %>% 
  ungroup()

sea_pop <- sea_pop[-c(5, 6, 7, 8), ]

sea_pop$Mean_affected_pop <- sea_pop$Mean_affected_pop / 100000 #in 100.000 thousands


sea_damage <-flood_sea %>% 
  group_by(Country) %>% 
  summarize(Mean_damage = mean(Adjusted_damage_usd, na.rm=TRUE)) %>%
  ungroup()

sea_damage <- sea_damage[-c(5, 6, 7, 8), ]

sea_damage$Mean_damage <- sea_damage$Mean_damage / 1000000 #making the numbers in millions 

sea_damage$Country

# Staring to map it----

Sea_2 <- c("Cambodia", "Indonesia", "Malaysia",
           "Philippines", "Thailand", "Vietnam")

sea_map <- world %>% 
  filter(region %in% Sea_2) %>% 
  dplyr::select(-subregion) 

Sea_flood_p <- left_join(sea_map, sea_pop, by = c("region" = "Country")) 

str(Sea_flood_p)

scales::show_col(c("#9DBF9E", "#A84268", "#FCB97D", "#C0BCB5", "#4A6C6F", "#FF5E5B"))

(Sea_flood_pmap <- ggplot() +
    geom_polygon(data = Sea_flood_p, aes(x = long, y = lat, group = group, fill = Mean_affected_pop) 
                 , color="black", size = 0.1) + #plot the data points on the map
    theme_void() + #choosing what type of background we want to display 
    scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"), na.value = "grey80")+
    theme(text = element_text(family = "Futura-Bold"),
          legend.position = c(0.87, 0.75),
          legend.title = element_text(family = "Futura-Bold", size = 10),
          legend.text = element_text(family = "Futura-Medium", size = 10)) +
    labs(y = "Latitude", x = "Longitude", #labs can be used to rename the axis and titles of your plots
         fill = "Affected population",
         title = "Population affected by flooding in South East Asia from 1980 to 2014"))

# LINK INSERT MAP: https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/



Sea_flood_d <- left_join(sea_map, sea_damage, by = c("region" = "Country")) 

str(Sea_flood_d)

(Sea_flood_dmap <- ggplot() +
    geom_polygon(data = Sea_flood_d, aes(x = long, y = lat, group = group, fill = Mean_damage) 
                 , color="black", size = 0.3) + #plot the data points on the map
    theme_minimal() + #choosing what type of background we want to display 
    scale_fill_viridis(direction = -1) +
    coord_map())  #making the projection nice

  



  
