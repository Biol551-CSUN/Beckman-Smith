#### Group practice wrangling data using dplyr ####
#### Created by: Natalie Beckman-Smith ####
#### Created on: 2021-02-15 ####
#### Updated on: 2021-02-15 ####
################################################################################

#### Load Libraries ####
library(palmerpenguins)
library(tidyverse)
library(here)

#### Load data ####
# The data is part of the package and is called penguins
glimpse(penguins)

#### Data Transformation ####
# Prompt 1 ####
penguins_summary <- penguins %>% #use penguin dataframe, assign to new dataframe
  drop_na(species) %>% #leave out any rows with "NA" in species column 
  drop_na(island) %>% #leave out any rows with "NA" in island column
  drop_na(sex) %>% #leave out any rows with "NA" in sex column
  group_by(species, island, sex) %>% #select data columns you want to group summary data by
  summarise(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE), #give mean of body mass for selected groups with new name, without 'NA's 
            varience_body_mass_g = var(body_mass_g, na.rm = TRUE)) #give variance of body mass for selected groups with new name, without 'NA's 
view(penguins_summary) #open new data sheet with summary data 

# Prompt 2 ####
penguins_plot <- penguins %>% #use penguin dataframe, assign to new dataframe
  filter(sex != "male") %>% #exclude males 
  mutate(log_body_mass = log(body_mass_g)) %>% #create new column of log of the body mass
  select(species, island, sex, log_body_mass) %>% #select columns to remain in dataframe, can use to rename and order them as well
  ggplot(mapping = aes(x = species, #set up plot of species vs. log(body mass)
                       y = log_body_mass,
                       color = island, #separate and plot islands by color (colors outline of boxplots)
                       fill = island)) + #fill in boxplots with color as well
  geom_boxplot(alpha = 0.7) + #create boxplot, make fill slightly transparent so can see mean line
  labs(x = "Species", #give labels 
       y = "Log (Body Mass)",
       color = "Island", 
       fill = "Island" , 
       title = "Log Body Mass of Penguins" 
       ) +
  theme_bw() + #change aesthetic theme
  ggsave(here("Week_4","output","WranglingLabPenguinPlot.png"), #export plot as an image 
         width = 7, height = 5)

penguins_plot #view plot in R