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
        title = "\nPopulation affected by flooding in South Asia"))

#ggsave(plot = sa_pop_map, filename = "img/affected_populatio_sa.png", width = 12, height = 8)

### Damage----

Sa_flood_d <- left_join(sa_map, sa_damage, by = c("region" = "Country")) 
str(Sa_flood_d)

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
         title = "\nDamage caused by flooding in South Asia"))

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

## Stacked Plot----
Sa_flood_d$region <- as.factor(Sa_flood_d$region)  # Change to factor
levels(Sa_flood_d$region)  # Show factor levels in their default order

### 
f_1980 <- sum(ifelse(Sa_flood_d$year == "1980", forest$f_area_mln, 0))  # Total cover in 1990
f_2016 <- sum(ifelse(forest$year == "2016", forest$f_area_mln, 0))  # Total cover in 2016
f_tot_diff <- round(f_1990 - f_2016, digits = 2)  # Total difference in forest cover
f_perc_diff <- round((f_1990 - f_2016) * 100 / f_1990, digits = 2)  # Percentage difference in forest cover

(stacked <- ggplot(forest, aes(x = year , y = f_area_mln, fill = Country.Name)) + 
    geom_area(alpha=0.8 , size=.2, color = "black") +  # This is the color of the lines between countries
    theme_minimal() +
    bbc_style() +  # Add the bbc style
    coord_cartesian(xlim = c(1990, 2017), 
                    ylim = c(0, 10)) +  # Determine axis limits
    scale_fill_manual(values = c("slategray", "#ABB4C4", "#ABBCDB", "#AAC9E0", 
                                 "#548FB3", "#4682B5", "#34648F", "#366281", 
                                 "#384B87", "#0B2475", "#5D6166", "#071721")) +  # Set the colour palette
    scale_x_continuous(labels = c(1990,2000,2010,2020)) +  # Determine x-axis labels
    geom_vline(xintercept = 2016, colour = "black", linetype = "dotted", size = 1) +  # Add vertical orientation line
    geom_hline(yintercept = f_1990, colour = "black", linetype = "dotted", size = 1) +  # Add horizontal orientation line
    geom_segment(aes(x = 2016, y = f_2016, xend = 2016, yend = f_1990),
                 colour = "red", linetype = "solid", size = 2) +  # Highlight the changes of total forest cover
    theme(plot.title = element_text(size = 12, hjust = 0.5),
          legend.position = "right",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm")) +
    annotate("text", x = 2006, y = 9.6, colour = "black",
             label = paste("In 25 years South America lost \n", f_tot_diff,
                           "million sq km of forests (", f_perc_diff, "%)"),
             size = 3.5, fontface = "italic") +  # Add annotation
    geom_curve(aes(x = 2009, y = 9.27, xend = 2015.5, yend = 8.7),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.5,
               color = "black", curvature = 0.3) +  # Add an arrow 
    labs(title = "Brazil's forests at risk",
         fill = "Countries",
         y = expression(paste("Forest cover (million ", km^2, ")")),
         x = "Year")  # Add the lab titles
  
  

