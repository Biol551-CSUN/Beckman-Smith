#### I am making a plot with my group using penguin data ####
#### Created by: Natalie Beckman-Smith ####
#### Created on: 2021-02-10 ####
#### Updated on: 2021-02-10 ####

#######################################################
#### Load Libraries ####
library(palmerpenguins)
library(tidyverse)
library(here)
library(devtools)
library(ggthemes)
library(PNWColors)

#### Load Data ####
# The data is part of the package and is called penguins
glimpse(penguins)

#### Data Analysis ####
penguins<-penguins%>% #this is to get rid of the "na"s in the data before we plot
  drop_na(sex)

pal <- pnw_palette("Winter",2) #used to assign the custom colors 

ggplot(data=penguins, #picks which data set to plot
  mapping = aes(x = species, #mapping is where to manipulate plot aspects for any actual variables from the data
                y = flipper_length_mm,
                color = sex, fill=sex)) + #separated the color and fill of the box plots
  geom_boxplot(alpha=0.5) + #made the box plot fill more transparent
  geom_jitter(alpha=0.5, position = position_jitterdodge( #used to make the scattered points align properly over the plots
      jitter.width = NULL,
      jitter.height = 0,
      dodge.width = 0.75
      )) +
  facet_wrap(~island) + #made into three separate plots by island 
  labs(x = "Species", #used to give labels 
       y = "Flipper Length (mm)",
       color = "sex", #this one changed the legend title 
       title = "Flipper length of male and female penguins",
       subtitle = "Penguin distribution by island") +
  theme_bw() + #changed aesthetic theme
  scale_color_manual(values = pal) + #changed color of the outlines
  scale_fill_manual(values = pal) + #changed color of the fill
ggsave(here("Week_3","output","LabPenguinPlot.png"), #exported plot as an image 
       width = 7, height = 5)

