### This is my first script. I am learning how to import data.
### Created by: Natalie Beckman-Smith
### Created on: 2021-02-03
##############################################################


### Load Libraries ####
library(tidyverse)
library(here)


### Read in data ####
WeightData<-read_csv(here("Week_2","Data","weightdata.csv"))


### Data Analysis ####

head(WeightData)
tail(WeightData)
view(WeightData)
