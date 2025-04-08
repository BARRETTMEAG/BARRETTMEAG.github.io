getwd("/Users/meaghanbarrett/Desktop/Classes/UVU/BIOL3100/Data_Course_BARRETT")
library(tidyverse)
library(leaflet)
library(ggplot2)

sharks <-read.csv('Assignments/Assignment_4/Sharks/list_coor_australia.csv')
colnames(sharks) <- c("longitude", "latitude")
sharks$longitude <- as.numeric(sharks$longitude)
sharks$latitude <- as.numeric(sharks$latitude)
sharks <- sharks[!is.na(sharks$longitude) & !is.na(sharks$latitude), ]
View(sharks)

map <- leaflet() %>% 
  addTiles() 
  #addMarkers(lng = -9.14, lat = 38.7)
  for (i in 1:nrow(sharks)) {
    map <- map %>% addMarkers(lng = sharks$longitude[i], lat = sharks$latitude[i], popup = paste("Marker ", i))
  }

map

leaflet() %>% 
  addTiles() %>%   
  addMarkers(lng = -9.14, lat = 38.7)

