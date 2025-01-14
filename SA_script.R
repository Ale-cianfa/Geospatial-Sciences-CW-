## Making a map for South Asia

#LIBRARIES ----
library(tidyverse) #for basic data wrangling functions and ggplot2
library(ggthemes) #for extra map themes
library(viridis) #for the colors
library(RColorBrewer) #for some more colors
library(maps) #for the base map data
library(hrbrthemes) #for the fonts in ggplot
#library("wesanderson")
library(ggsn)
library(png)
library(ggrepel)    # for annotations


#LOADING THE DATASETS ----

flood <- read.csv("Datasets/Geospatial_flooding.csv") #loading in the flood dataset
str(flood)

world <- map_data("world") #downloading coordinates for the world,we'll select countries by ourselves
str(world)

#DATA WRANGLING----

flood <- flood[-c(1), ] %>% #removing the subcategories for each column 
  dplyr::select(Major.Country, Began, Ended, 
                Death, Affected.population, 
                Homeless, Damage..USD.if.no.units., Adjusted.Damage.to.2002, Reference)

#Renaming the column so they are more consistent 
colnames(flood) <- c("Country", "Start", "End", "Deaths", 
                     "Affected_population", "Homeless",
                     "Damage_usd", "Adjusted_damage_usd", "Reference")

str(flood) #new datasets, they are all character variables which is not what we want 

## Turning all variables into appropriate types----
flood$Country <- as.character(flood$Country)
flood$Start <- as.integer(flood$Start)
flood$End <- as.integer(flood$End)
flood$Deaths <- as.integer(gsub(",", "", flood$Deaths)) #putting nothing instead of the comma in some numbers
#so r can recognize them as numerics/integers
flood$Affected_population <- as.numeric(gsub(",", "", flood$Affected_population))
flood$Homeless <- as.integer(gsub(",", "", flood$Homeless))
flood$Damage_usd <- as.numeric(gsub(",", "", flood$Damage_usd))
flood$Adjusted_damage_usd <- as.numeric(gsub(",", "", flood$Adjusted_damage_usd))

str(flood)

##Filtering out from 1980s----

flood <- flood %>% 
  group_by(Country) %>%
  filter(Start >= 1980) %>% #this makes sure to remove everything below 1980 
  ungroup()

unique(flood$Start)

##Filterning For South Asia (SA)----

SA <- c("Southeastern India", "India, Bangladesh", "India", "Bangladesh", "Nepal", "Sri Lanka", "Pakistan")

flood_sa <- flood %>%
  filter(Country %in% SA) %>%
  mutate(Country = str_replace(Country, "Southeastern India", "India")) %>% 
  mutate(Country = str_replace(Country, "India, Bangladesh", "India")) 
unique(flood_sa$Reference)
#CREATING NEW SUMMARY DF FOR DAMAGE AND POP----

##Population----
sa_pop <- aggregate(x= flood_sa$Affected_population,
                     by= list(flood_sa$Country),
                     FUN=sum, na.rm = TRUE)  
colnames(sa_pop) <- c("Country","Affected_population")
sa_pop$Affected_population <- sa_pop$Affected_population / 1000000 #in millions

##Damage----
sa_damage <- aggregate(x= flood_sa$Adjusted_damage_usd,
                        by= list(flood_sa$Country),
                        FUN=sum, na.rm = TRUE)

colnames(sa_damage) <- c("Country","Adjusted_damage_usd")

sa_damage$Adjusted_damage_usd <- sa_damage$Adjusted_damage_usd / 1000000000 #making the numbers in billions 

#MAPPING----
## Filtering world map for SA----
Sa_2 <- c("India", "Bangladesh", "Nepal", "Sri Lanka", "Pakistan")

sa_map <- world %>% 
  filter(region %in% Sa_2) %>% 
  dplyr::select(-subregion) 

Sa_flood_p <- left_join(sa_map, sa_pop, by = c("region" = "Country")) 

str(Sa_flood_p)

##Country Centroids----
centroids <- read.csv("Datasets/centroids.csv") #reading centroid data
head(centroids)
str(centroids) 

centroids_sa <- centroids %>% 
  filter(name %in% Sa_2) %>% 
  dplyr::select(name, Longitude, Latitude, iso_a3) 

## Making the values lables to add to the maps----
Sa_flood_d <- left_join(sa_map, sa_damage, by = c("region" = "Country")) 
str(Sa_flood_d)

lables <- left_join(centroids_sa, sa_damage, by = c("name" = "Country"))      

lables <- left_join(lables, sa_pop, by = c("name" = "Country"))      
str(lables)

#Fixing Bangladesh centroid
lables[1, 2] = 92
lables[1, 3] = 24.25

#Fixing India centroid
lables[2, 2] = 79
lables[2, 3] = 23.5

#Fixing Sri Lanka centroid
lables[3, 2] = 82
lables[3, 3] = 8

#Fixing Nepal centroid
lables[4, 2] = 84
lables[4, 3] = 29

#Fixing Pakistan centroid
lables[5, 2] = 68
lables[5, 3] = 30.3

## Making the two maps----

### Population----

(sa_pop_map <- ggplot() +
   geom_polygon(data = Sa_flood_p, aes(x = long, y = lat, group = group, fill = Affected_population) 
                , color="black", size = 0.2) + #plot the data points on the map
   theme_void() + #choosing what type of background we want to display 
   coord_map() +
   geom_text(data = centroids_sa, aes(Longitude, Latitude, label = name), size = 5) +
   #scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"), na.value = "grey80")+
   scale_fill_gradientn(colors = c("#94d2bd", "#0a9396", "#005f73"), na.value = "grey80")+
   theme(plot.title = element_text(family = "Futura-Bold", size = 20),
         legend.position = c(0.1, 0.30),
         legend.title = element_text(family = "Futura-Bold", size = 14),
         legend.text = element_text(family = "Futura-Medium", size = 12), 
         plot.background = element_rect(fill = "#f5f5f2", color = NA)) +
   theme(plot.title = element_text(hjust= 0, size = 20)) +
   labs(y = "Latitude", x = "Longitude", #labs can be used to rename the axis and titles of your plots
        fill = "Affected \npopulation \n(millions)",
        title = "\nPopulation affected by flooding in South Asia") +
   geom_label_repel(data = lables,
                    aes(x = Longitude, y = Latitude, label = Affected_population),
                    box.padding = 1, size = 4, nudge_x = 1, 
                    nudge_y = 2, alpha = 0.8, arrow = arrow(length = unit(0.01, "npc"))))


#ggsave(plot = sa_pop_map, filename = "img/affected_populatio_sa.png", width = 12, height = 8)

### Damage----

(sa_dam_map <- ggplot() +
    geom_polygon(data = Sa_flood_d, aes(x = long, y = lat, group = group, fill = Adjusted_damage_usd) 
                 , color="black", size = 0.2) + #plot the data points on the map
    theme_void() + #choosing what type of background we want to display
    coord_map() +
    scale_fill_gradientn(colors = c("#ee9b00", "#bb3e03", "#ae2012"), na.value = "grey80") +
    geom_text(data = centroids_sa, aes(Longitude, Latitude, label = name), size = 5) +
    theme(plot.title = element_text(family = "Futura-Bold", size = 20),
          legend.position = c(0.1, 0.30),
          legend.title = element_text(family = "Futura-Bold", size = 14),
          legend.text = element_text(family = "Futura-Medium", size = 12), 
          plot.background = element_rect(fill = "#f5f5f2", color = NA)) +
    theme(plot.title = element_text(hjust= 0, size = 20)) +
    labs(y = "Latitude", x = "Longitude", #labs can be used to rename the axis and titles of your plots
         fill = "Damage \n(billion USD)",
         title = "\nDamage caused by flooding in South Asia") +
    geom_label_repel(data = lables,
                     aes(x = Longitude, y = Latitude, label = Adjusted_damage_usd),
                     box.padding = 1, size = 4, nudge_x = 1, 
                     nudge_y = 2, alpha = 0.8, arrow = arrow(length = unit(0.01, "npc"))))

#ggsave(plot = sa_dam_map, filename = "img/damage_$_sa.png", width = 12, height = 8)

## Adding scale----

### Population----
(final_sa_pop <- sa_pop_map +
   ggsn::scalebar(data = sa_map,
                  transform = TRUE, dist = 500, dist_unit = "km", model='WGS84',
                  height = 0.009, location = "bottomleft", st.dist = 0.06))
#ggsave(plot = final_sa_pop, filename = "img/affected_populatio_sa.png", width = 12, height = 8)

### Damage----
(final_sa_dam <- sa_dam_map +
   ggsn::scalebar(data = sa_map,
                  transform = TRUE, dist = 500, dist_unit = "km", model='WGS84',
                  height = 0.009, location = "bottomleft", st.dist = 0.06))

#ggsave(plot = final_sa_dam, filename = "img/damage_$_sa.png", width = 12, height = 8)

## Add a north arrow----
library(magick)

img <- magick::image_read('/path/image.png'


pic <- readPNG("/Misc/nort_arrow.png")

grid::grid.raster(pic)


