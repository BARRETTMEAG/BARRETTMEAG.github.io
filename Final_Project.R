#Final_Project

# load libraries
library(tidyverse)
library(janitor)
library(leaflet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tibble)
library(readr)
library(forcats)


# GATORS ####
# read csv files
gator <- read.csv("predators/fatal_alligator_attacks_US.csv")

# clean names
gator <-clean_names(gator)

# converts data structure to more appropriate data types
gator <-gator %>% 
  mutate(location = str_extract(details, "(Miami|Florida|Georgia|Texas|Louisiana|South Carolina)"),
         location = ifelse(location == "Miami", "Florida", location))

#graph gator attacks over the years and color per state
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

# graph deaths of gators per state
deaths %>% 
  ggplot(aes(x = location, y = n, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Alligator Attacks by State",
       x = "State", y = "Number of Victims", fill = "Sex")


# WOLVES ####
# Try different ways to clean the data
g_wolves <- read.csv("predators/global_wolves.csv")
g_wolves <-clean_names(g_wolves)
View(g_wolves)

df_split <- g_wolves %>%
  mutate(victims = str_replace_all(victims, "(?i)(?<=\\d)(?=male|female)", ", ")) %>%  
  mutate(victims = str_replace_all(victims, "(?<!e)male", " male")) %>%  
  mutate(victims = str_replace_all(victims, "(?<! )female", " female")) %>%
  mutate(victims = str_replace_all(victims, "\\s+and\\s+", ", ")) %>%
  separate_rows(victims, sep = ",(?=(?:[^\\\"]*\\\"[^\\\"]*\\\")*[^\\\"]*$)")  %>%
  mutate(victims = str_trim(victims)) %>%
  mutate(Group = rep(1:(n()/3), each = 3)[1:n()]) %>%
  group_by(Group) %>%
  summarise(
    Name = first(victims),
    Age = suppressWarnings(as.numeric(victims[2])),
    Sex = str_to_lower(str_trim(victims[3]))
  ) %>%
  ungroup()

View(df_wolves)

g_wolves$type_of_attack[is.na(g_wolves$type_of_attack) | g_wolves$type_of_attack == ""] <- "Unknown"

# Graph Victim Count and Type of Attacks for wolves
g_wolves %>% 
  group_by(type_of_attack) %>% 
  ggplot(aes(x = type_of_attack))  +
  geom_bar(aes(y = ..count..)) + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Types of Attacks", y = "Victim Count")


# Read the CSV file 
data <- read.csv("predators/global_wolves.csv", stringsAsFactors = FALSE)

data <- clean_names(data)

# Use a regular expression to extract the country from the 'Location' column
data$country <- sub(".*,\\s*(.*)$", "\\1", data$location)

# Extract the country assuming it's the last word in the Location string
data$country <- str_extract(data$location, "[^,]+$")

# Trim any leading/trailing whitespace
data$country <- trimws(data$country)

data <- data %>%
  mutate(type_of_attack = ifelse(type_of_attack == "", NA, type_of_attack))

# Graph wolf attacks
data %>% 
  ggplot(aes(x = country, fill = type_of_attack)) +
  geom_bar(position = "dodge") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(title = "Attacks and Country",
       x = "Country",
       y = "Count of Attacks") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8))  


### Attempt 2
# Load the CSV
df <- read_csv("predators/global_wolves.csv") %>% 
  clean_names()

# Function to split or duplicate victims
expand_victims <- function(victim_string, row_data) {
  row_data <- as_tibble(row_data)  
  
  # Insert separator where gender is jammed with next name, like "maleAlex"
  victim_string <- str_replace_all(victim_string, "(male|female)(?=[A-Z])", "\\1; ")
  
  # Case 1: multiple named people
  if (str_detect(victim_string, "[A-Za-z]+, \\d{1,2}, (male|female)")) {
    people <- str_extract_all(victim_string, "[^,]+, \\d{1,2}, (male|female)")[[1]]
    return(map_df(people, ~ mutate(row_data, victims = .x)))
  }
  
  # Case 2: vague with number
  number_match <- str_match(victim_string, "(\\d+)|([Tt]wo|[Tt]hree|[Ff]our|[Ff]ive|[Ss]ix|[Ss]even|[Ee]ight)")
  if (!is.na(number_match[1])) {
    num <- suppressWarnings(as.numeric(number_match[1]))
    if (is.na(num)) {
      num <- case_when(
        str_detect(number_match[1], regex("two", ignore_case = TRUE)) ~ 2,
        str_detect(number_match[1], regex("three", ignore_case = TRUE)) ~ 3,
        str_detect(number_match[1], regex("four", ignore_case = TRUE)) ~ 4,
        str_detect(number_match[1], regex("five", ignore_case = TRUE)) ~ 5,
        str_detect(number_match[1], regex("six", ignore_case = TRUE)) ~ 6,
        str_detect(number_match[1], regex("seven", ignore_case = TRUE)) ~ 7,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 8,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 9,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 10,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 11,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 12,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 13,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 14,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 15,
        TRUE ~ 1
      )
    }
    return(map_df(1:num, ~ mutate(row_data, victims = "Unknown")))
  }
  
  # Default: return original row
  return(row_data)
}

# Apply to each row
cleaned_df <- df %>%
  rowwise() %>%
  do(expand_victims(.$victims, .)) %>%
  ungroup()

cleaned_df <-cleaned_df <- cleaned_df %>%
  mutate(
    country = str_remove(country, "\\.$")  
  )

cleaned_df <- cleaned_df %>%
  mutate(country = str_extract(location, "[^,]+$") %>% str_trim())


View(cleaned_df)

cleaned_df %>% 
  ggplot(aes(x = country, fill = type_of_attack)) +
  geom_bar(position = "dodge") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(title = "Attacks and Country",
       x = "Country",
       y = "Count of Attacks") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8))  


# Attempt 3 at cleaning wolf data
# Load and clean the CSV
df <- read_csv("predators/global_wolves.csv") %>%
  clean_names()

# Function to expand and clean victim entries
expand_victims <- function(victim_string, row_data) {
  row_data <- as_tibble(row_data)
  victim_string <- str_trim(victim_string)
  
  # Fix gender attached to names like "maleAlex"
  victim_string <- str_replace_all(victim_string, "(male|female)(?=[A-Z])", "\\1, ")
  
  # Replace "and" with commas for consistency
  victim_string <- str_replace_all(victim_string, "\\band\\b", ",")
  
  # Normalize separators
  victim_string <- str_replace_all(victim_string, ";", ",")
  victim_string <- str_replace_all(victim_string, "\\s*,\\s*", ", ")
  victim_string <- str_squish(victim_string)
  
  # Extract complete victim phrases: "Name, age gender"
  people <- str_extract_all(
    victim_string,
    "(?:[A-Z][a-z'\\-]+\\s*){1,3},?\\s*(?:adult|child)?\\s*(male|female)?"
  )[[1]] %>% str_trim()
  
  # Filter out empty results
  people <- people[people != ""]
  
  # Return a row for each victim
  map_df(people, function(person) {
    mutate(row_data, victims = person)
  })
}

# Apply function row-by-row
cleaned_df <- df %>%
  rowwise() %>%
  do(expand_victims(.$victims, .)) %>%
  ungroup()

# Export cleaned data
#write_csv(cleaned_df, "cleaned_global_wolves.csv")

View(cleaned_df)


# Cleaner Wolves Data ####
# Load and clean the dataset
df <- read_csv("predators/global_wolves.csv") %>% 
  clean_names()

# Function to expand victims and extract age & sex
expand_victims <- function(victim_string, row_data) {
  row_data <- as_tibble(row_data)  
  
  victim_string <- str_replace_all(victim_string, "(male|female)(?=[A-Z])", "\\1; ")
  
  if (str_detect(victim_string, "[A-Za-z]+,? ?\\d{1,2}, (male|female)")) {
    people <- str_extract_all(victim_string, "[^,;]+,? ?\\d{1,2}, (male|female)")[[1]]
    
    return(map_df(people, function(person) {
      age <- str_extract(person, "\\d{1,2}") %>% as.numeric()
      sex <- str_extract(person, "male|female")
      mutate(row_data, victims = person, age = age, sex = sex)
    }))
  }
  
  number_match <- str_match(victim_string, "(\\d+)|([Tt]wo|[Tt]hree|[Ff]our|[Ff]ive|[Ss]ix|[Ss]even|[Ee]ight|[Nn]ine|[Tt]en|[Ee]leven|[Tt]welve|[Tt]hirteen|[Ff]ourteen|[Ff]ifteen)")
  
  if (!is.na(number_match[1])) {
    num <- suppressWarnings(as.numeric(number_match[1]))
    if (is.na(num)) {
      num <- case_when(
        str_detect(number_match[1], regex("two", ignore_case = TRUE)) ~ 2,
        str_detect(number_match[1], regex("three", ignore_case = TRUE)) ~ 3,
        str_detect(number_match[1], regex("four", ignore_case = TRUE)) ~ 4,
        str_detect(number_match[1], regex("five", ignore_case = TRUE)) ~ 5,
        str_detect(number_match[1], regex("six", ignore_case = TRUE)) ~ 6,
        str_detect(number_match[1], regex("seven", ignore_case = TRUE)) ~ 7,
        str_detect(number_match[1], regex("eight", ignore_case = TRUE)) ~ 8,
        str_detect(number_match[1], regex("nine", ignore_case = TRUE)) ~ 9,
        str_detect(number_match[1], regex("ten", ignore_case = TRUE)) ~ 10,
        str_detect(number_match[1], regex("eleven", ignore_case = TRUE)) ~ 11,
        str_detect(number_match[1], regex("twelve", ignore_case = TRUE)) ~ 12,
        str_detect(number_match[1], regex("thirteen", ignore_case = TRUE)) ~ 13,
        str_detect(number_match[1], regex("fourteen", ignore_case = TRUE)) ~ 14,
        str_detect(number_match[1], regex("fifteen", ignore_case = TRUE)) ~ 15,
        TRUE ~ 1
      )
    }
    
    return(map_df(1:num, ~ mutate(row_data, victims = "Unknown", age = NA, sex = NA)))
  }
  
  sex <- str_extract(victim_string, "male|female")
  age <- str_extract(victim_string, "\\d{1,2}") %>% as.numeric()
  
  return(mutate(row_data, victims = victim_string, age = age, sex = sex))
}

# Expand victims into individual rows
cleaned_df <- df %>%
  rowwise() %>%
  do(expand_victims(.$victims, .)) %>%
  ungroup()

# Clean country info from 'location' column
cleaned_df <- cleaned_df %>%
  mutate(
    country = str_extract(location, "[^,]+$") %>% str_trim(),
    country = str_remove(country, "\\.")
  ) %>%
  filter(!is.na(country) & country != "")

# Get top 15 countries by number of attacks
top_countries <- cleaned_df %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(country)

# Filter to just those countries
filtered_df <- cleaned_df %>%
  filter(country %in% top_countries)

filtered_df <- filtered_df %>%
  filter(!is.na(type_of_attack) & type_of_attack != "")

# Plot
filtered_df %>%
  ggplot(aes(x = fct_infreq(country), fill = type_of_attack)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Countries with Wolf Attacks",
    x = "Country",
    y = "Number of Attacks",
    fill = "Attack Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


# SHARKS ####
# Shark 1####
shark_1 <- read.csv("predators/Shark_attacks/attacks.csv")
View(shark_1)

shark_1 <- clean_names(shark_1)

shark_1$type[is.na(shark_1$type) | shark_1$type == ""] <- "Unknown"
shark_1$type[is.na(shark_1$type) | shark_1$type == "Invalid"] <- "Unknown"
shark_1_subset <- shark_1[1:6302, ]

# Graphing bar plot for shark 1
shark_1 %>% 
  group_by(country) %>% 
  ggplot(aes(x = type)) +
  geom_bar(aes(y = ..count..)) + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Shark 2 ####
shark_2 <- read.csv("predators/shark_attacks.csv")
View(shark_2)
shark_2 <-clean_names(shark_2)

shark_2$type[is.na(shark_2$type) | shark_2$type == "Invalid"] <- "Unknown"

# Graphing Attacks for shark 2
shark_2 %>% 
  group_by(area) %>% 
  ggplot(aes(x = type)) +
  geom_bar(aes(y = ..count..)) + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


top_countries <- shark_2 %>%
  count(country, sort = TRUE) %>%
  top_n(15, n) %>%
  pull(country)


filter(!is.na(month)) %>%
# Shark 3: Australia Coordination####
shark_3 <- read.csv("predators/Shark_attacks/list_coor_australia.csv")
str(shark_3)
View(shark_3)

colnames(shark_3) <- c("latitude", "longitude")

center_lat <- -25.2744
center_lon <- 133.7751
zoom_level <- 5  

# mapping longitude and latitude
map <- leaflet() %>%
  addTiles() %>%  
  setView(center_lon, center_lat, zoom = zoom_level)

for (i in 1:nrow(shark_3)) {
  map <- map %>% addMarkers(lng = shark_3$longitude[i], lat = shark_3$latitude[i])
}

map


# Who is King?
# 1. Summarize the number of attacks from each cleaned dataset

# Alligator
gator_attacks <- gator %>%
  filter(!is.na(date)) %>%
  nrow()

# Shark - Using shark_1 since it has most rows and was pre-cleaned
shark_attacks <- shark_1 %>%
  filter(!is.na(type), type != "Unknown") %>%
  nrow()

# Wolves - Using cleaned_df from wolf data
wolf_attacks <- cleaned_df %>%
  filter(!is.na(type_of_attack), type_of_attack != "") %>%
  nrow()

# 2. Create a comparison data frame
attack_king_df <- tibble(
  predator = c("Shark", "Alligator", "Wolf"),
  total_attacks = c(shark_attacks, gator_attacks, wolf_attacks)
)

# 3. Plot it
attack_king_df %>%
  ggplot(aes(x = reorder(predator, total_attacks), y = total_attacks, fill = predator)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = total_attacks), vjust = -0.5, size = 4) +
  labs(
    title = "The King of Attacks",
    x = "Predator",
    y = "Documented Attacks"
  ) +
  scale_fill_manual(values = c("Shark" = "#FF3C38", "Alligator" = "#0C9463", "Wolf" = "#4C4C6D")) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    legend.position = "none"
  )
