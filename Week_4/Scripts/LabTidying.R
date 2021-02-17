#### Today we are going to practice tidy with biogeochemistry data from Hawaii ####
#### Created by: Natalie Beckman-Smith ####
#### Created on: 2021-02-17 ####
#### Updated on: 2021-02-17 ####

#### Load Libraries ####
library(tidyverse)
library(here)
library(PNWColors)

#### Load Data ####
ChemData<-read_csv(here("Week_4","data", "chemicaldata_maunalua.csv"))
View(ChemData) #shows datasheet in new tab
glimpse(ChemData) #shows variables of data in console

#### Data Analysis Part 1 ####
ChemData_tidy<-ChemData %>% #creates new dataframe for summary stats
  filter(complete.cases(.)) %>% #filters out everything that is not a complete row (removes all NAs)
  separate(col = Tide_time, #chooses the tide time col
           into = c("Tide","Time"), #separates it into two columns Tide and Time
           sep = "_", #separates columns by _
           remove = FALSE) %>% #leaves the original Tide_time column
  filter(Season == "SPRING") %>% #keeps only data from Spring
  pivot_longer(cols = Temp_in:percent_sgd, #selects the cols you want to pivot  
               names_to = "Variables", #the names of the new cols with all the column names 
               values_to = "Values") %>% #the names of the new col with all the values 
  group_by(Variables, Site, Tide) %>% #creates groups to by summarized by
  summarise(Value_mean = mean(Values, na.rm = TRUE), #calculates mean
            Value_variance = var(Values, na.rm = TRUE), #calculates variance
            Value_sd = sd(Values, na.rm = TRUE)) %>% #calculates standard deviation 
  write_csv(here("Week_4","output","LabTidyingSummary.csv"))
  
#### Data Analysis Part 2 ####
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% #filters out everything that is not a complete row (removes all NAs)
  separate(col = Tide_time, #chooses the tide time col
           into = c("Tide","Time"), #separates it into two columns Tide and Time
           sep = "_", #separate by _
           remove = FALSE) %>% #leaves the original Tide_time column
  rename('Nitrate+Nitrite (umol/L)' = "NN", #renames the variables for plot  
         'Phosphate (umol/L)' = "Phosphate",
         'Silicate (umol/L' = "Silicate",
         'TA (umol/kg)' = "TA",
         'Percent SGD' = "percent_sgd",
         'Temperature (C)' = "Temp_in") %>% 
  pivot_longer(cols = c('Temperature (C)', 'Phosphate (umol/L)':'Percent SGD'), #selects the cols you want to pivot  
               names_to = "Variables", # the names of the new cols with all the column names 
               values_to = "Values") %>% # the names of the new col with all the values 
  mutate(Site = ifelse(Site == "BP", "Black Point", "Wailupe")) #renames sites
  
#### Data Visualization ####
pal<-pnw_palette("Starfish",2) #assigns color palette

ggplot(data = ChemData_clean, #makes plot of salinity vs. chem values
      aes(x = Salinity,
          y = Values,
          color = Site)) + #plots both sites by color
  geom_line() + #makes line graph
  labs(y = "Parameters") + #renames y-axis
  theme_bw() + #changes theme
  theme(legend.position = "right") + #moves legend to the right side
  facet_wrap(~Variables, scales = "free") + #makes a separate plot for every chem variable
  scale_color_manual(values = pal)+ #changes color palette 
  ggsave(here("Week_4","output","LabTidyingHawaiiPlot.png"), #exports plot as an image 
         width = 7, height = 5)