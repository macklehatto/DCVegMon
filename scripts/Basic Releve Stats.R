library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(here)
library(ggplot2)
library(readr)
library(purrr)
library(ggthemes)

#get all the files in this directory

file_list <- list.files(path = here::here("data", "Processed Releve Data"), full.names = TRUE)

#merge them all together into one dataframe

all_data <- read_csv(file_list)

#how many species were recorded per year (not cumulative)?

species_by_year <- all_data %>% 
  distinct(releveyear, scientific_name) %>% 
  count(releveyear, name = "n_species")

species_by_year_counts <- all_data %>% 
  count(releveyear, scientific_name)

ggplot(species_by_year, aes(x = releveyear, y = n_species)) +
  geom_col(alpha = 0.7, fill = "#8bca84") +
  labs(
    x = "year", 
    y = "plant species"
  ) +
  ggtitle("Number of Species Observations Per Year")+
  theme_solarized()

#how many species were recorded per year (cumulative)?

year_x_sp <- all_data %>% 
  distinct(releveyear, scientific_name) %>% 
  arrange(releveyear) %>% 
  group_by(releveyear) %>% 
  summarise(scientific_name = list(scientific_name), .groups = "drop") %>% 
  mutate(
    cumulative_species = accumulate(scientific_name, union), 
    cumulative_richness = map_int(cumulative_species, length)
  )

ggplot(year_x_sp, aes(x = releveyear, y = cumulative_richness)) +
  geom_line(color = "#8bca84") +
  geom_point(color = "#8bca84") +
  labs(
    x = "Year", 
    y = "Plant species"
  ) +
  ggtitle("Cumulative Number of Plant Species Observed") +
  theme_solarized()
  
#how many releves were completed?

releves_by_year <- all_data %>% 
  distinct(releveyear, releveSite) %>% 
  count(releveyear, name = "n_releves")

ggplot(releves_by_year, aes(x = releveyear, y = n_releves)) +
  geom_col(alpha = 0.7, fill = "#8bca84") +
  labs(
    x = "year", 
    y = "releves completed"
  ) +
  ggtitle("Number of Releves Completed Per Year")+
  theme_solarized()


