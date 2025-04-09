#Final_Project
library(tidyverse)
library(janitor)
library(leaflet)
library(dplyr)
library(ggplot2)


gator <- read.csv("fatal_alligator_attacks_US.csv")
View(gator)

names(gator)

gator <-clean_names(gator)

gator <-gator %>% 
  mutate(location = str_extract(details, "(Miami|Florida|Georgia|Texas|Louisiana|South Carolina)"),
         location = ifelse(location == "Miami", "Florida", location))

gator %>%
  mutate(date = as.Date(date, format = "%B %d, %Y")) %>%  
  filter(age != "?") %>%
  mutate(age = as.numeric(age)) %>%
  filter(age >= 2 & age <= 81) %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%  
  arrange(age) %>%
  group_by(location) %>%
  ggplot(aes(x = factor(year), y = age, color = location)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y = "Age")

deaths <-gator %>%
  filter(location != "?") %>%  
  filter(sex != "?") %>%       
  group_by(location, sex) %>%
  tally()

deaths %>% 
  ggplot(aes(x = location, y = n, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Alligator Attacks by State",
       x = "State", y = "Number of Victims", fill = "Sex")


g_wolves <- read.csv("global_wolves.csv")
g_wolves <-clean_names(g_wolves)
View(g_wolves)

g_wolves$type_of_attack[is.na(g_wolves$type_of_attack) | g_wolves$type_of_attack == ""] <- "Unknown"

g_wolves %>% 
  group_by(type_of_attack) %>% 
  ggplot(aes(x = type_of_attack))  +
  geom_bar(aes(y = ..count..)) + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Types of Attacks", y = "Victim Count")

shark_1 <- read.csv("Shark_attacks/attacks.csv")
View(shark_1)

shark_1 <- clean_names(shark_1)

shark_1$type[is.na(shark_1$type) | shark_1$type == "Invalid"] <- "Unknown"
shark_1_subset <- shark_1[1:6302, ]

shark_1 %>% 
  group_by(country) %>% 
  ggplot(aes(x = type)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shark_2 <- read.csv("shark_attacks.csv")
View(shark_2)
shark_2 <-clean_names(shark_2)

shark_2$type[is.na(shark_2$type) | shark_2$type == "Invalid"] <- "Unknown"

shark_2 %>% 
  group_by(area) %>% 
  ggplot(aes(x = type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shark_3 <- read.csv("Shark_attacks/list_coor_australia.csv")
str(shark_3)
View(shark_3)

colnames(shark_3) <- c("latitude", "longitude")

center_lat <- -25.2744
center_lon <- 133.7751
zoom_level <- 5  

map <- leaflet() %>%
  addTiles() %>%  
  setView(center_lon, center_lat, zoom = zoom_level)

for (i in 1:nrow(shark_3)) {
  map <- map %>% addMarkers(lng = shark_3$longitude[i], lat = shark_3$latitude[i])
}

map
