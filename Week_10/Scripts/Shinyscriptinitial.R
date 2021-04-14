#### Create a Shiny App ####
#### By: Natalie Beckman-Smith
#### Created: April 7, 2021

#### Load Libraries ####
library(tidyverse)
library(here)
library(lubridate)

#### Load Data ####
HatchBabyExport <- read_csv(here("Week_10","Data","HatchBabyExport.csv"))

#### Data Analysis ####
babyweight <- HatchBabyExport %>% #creates a new dataframe
  rename(babyname = "Baby Name", starttime = "Start Time", activity = "Activity", weight = "Amount") %>% #renames columns 
  select(babyname, starttime, activity, weight) %>% #selects columns
  filter(activity == "Weight") %>% #filters only weightings
  mutate(date = mdy_hm(starttime), #makes dates into characters
         weightround = as.numeric(weight)) #rounds numbers 


ggplot(data = babyweight, #plots weight over time
       aes(x = date, 
           y = weightround,
           group = 1)) + #makes it possible to havea line 
  geom_point() +
  geom_line() +
  xlab("Date") +
  ylab("Weight (lbs)") +
  ggtitle(paste("Change in Baby's Weight over Time"))







micah <- babyweight %>% 
  filter(babyname == "Micah")

blakely <- babyweight %>% 
  filter(babyname == "Blakely")


ggplot(data = micah, 
       aes(x = date, 
           y = weightround,
           group = 1)) +
  geom_point() +
  geom_line()



