#### Group practice using lubridate ####
#### Created by: Natalie Beckman-Smith ####
#### Created on: 2021-02-24 ####
#### Updated on: 2021-02-24 ####
################################################################################

#### Load Libraries ####
library(tidyverse)
library(here)
library(lubridate)

#### Load data ####
ConData<-read_csv(here("Week_5","data", "CondData.csv"))
DepthData<-read_csv(here("Week_5","data", "DepthData.csv"))

#### Data Transformation ####
ConDataDate <- ConData %>% #makes new dataframe for transforming date 
  mutate(DateTime = ymd_hms(date)) %>% #changes date from character
  mutate(DateTime = round_date(DateTime, "10 seconds")) %>%  #rounds to nearest 10 seconds
  select (-date) #removes the original old date column 
view(ConDataDate) #views the data with the newly transformed date 

DepthDataDate <- DepthData %>% #same as above for second dataset
  mutate(DateTime = ymd_hms(date)) %>% 
  select (-date)
view(DepthDataDate)

CondDepthData <- inner_join(ConDataDate, DepthDataDate) %>% #joins both transformed datasets by date column, removing NAs
  mutate(Hour = hour(DateTime)) %>% #makes new column of just the hours
  mutate(Minute = minute(DateTime)) %>% #makes new column of just the minutes
  group_by(Hour, Minute) %>%  #groups by hour and minute for summary statistics
  summarise(mean_date = mean(DateTime, na.rm = TRUE), #finds means of data by minute
            mean_depth = mean(Depth, na.rm = TRUE),
            mean_temperature = mean(TempInSitu, na.rm = TRUE),
            mean_salinity = mean(SalinityInSitu_1pCal, na.rm = TRUE)) %>% 
  unite(col = "hour_minute", c(Hour,Minute), sep = ":", remove = FALSE) #adds new column combining hours and minutes for easier plotting
view(CondDepthData) #views summary table 

#### Data Visualization ####
ggplot(data = CondDepthData, #pulls from transformed and combined summary dataset to make plot
       aes(x = mean_date, #assigns data to axes
           y = mean_depth, 
           colour = mean_temperature)) +
  geom_point() + #scatterplot
  scale_y_reverse() + #reverses the y-axis so it mimics depth 
  labs (x = "Time", #relabels axes and adds title
        y = "Mean Depth(m)",
        color = "Mean Temperature(C)",
        title = "Temperature Over Depth and Time") +
ggsave(here("Week_5","output","LabLubridatePlot.png"), #exports plot as an image 
       width = 7, height = 5)
  
ggplot(data = CondDepthData, #pulls from transformed and combined summary dataset to make plot
       aes(x = mean_date, #assigns data to axes
           y = mean_depth, 
           colour = mean_salinity)) +
  geom_point() + #scatterplot
  scale_y_reverse() + #reverses the y-axis so it mimics depth 
  labs (x = "Time", #relabels axes and adds title
        y = "Mean Depth(m)",
        color = "Mean Salinity",
        title = "Salinity Over Depth and Time") +
  ggsave(here("Week_5","output","LabLubridatePlot2.png"), #exports plot as an image 
         width = 7, height = 5)