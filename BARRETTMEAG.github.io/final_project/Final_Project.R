#Final_Project
library(tidyverse)
library(janitor)
library(leaflet)
library(dplyr)
library(ggplot2)

gator <- read.csv("Assignments/Assignment_4/fatal_alligator_attacks_US.csv")
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
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(title = "Alligator Attacks by State",
       x = "State", y = "Number of Victims", fill = "Sex")


g_wolves <- read.csv("Assignments/Assignment_4/global_wolves.csv")
View(g_wolves)

g_wolves %>% 
  group_by(Type.of.attack) %>% 
  ggplot(aes(x = Type.of.attack)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shark_2 <- read.csv("Assignments/Assignment_4/shark_attacks.csv")
View(shark_2)

shark_2 %>% 
  group_by(Area) %>% 
  ggplot(aes(x = Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shark_1 <- read.csv("Assignments/Assignment_4/Sharks/attacks.csv")
View(shark_1)

shark_1 %>% 
  group_by(Country) %>% 
  ggplot(aes(x = Type)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#shark_1.5 <- read.csv("Assignments/Assignment_4/Sharks/list_coor_australia.csv")
#View(shark_1.5)

leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = -9.14, lat = 38.7) #use for Australia coords